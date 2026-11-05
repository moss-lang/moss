(* ------------------------------------------------------------------------ *)
(* XTyping.v — a typing judgment for MossX's elaborated programs (Track B).  *)
(*                                                                           *)
(* Core Moss's `has_ty` (CoreMoss.v §8) extended to the constructs the       *)
(* elaborator emits beyond the core: machine numbers, structs (a tag over a *)
(* record), `var` and assignment, `if`, `while`, `loop`, `break`, `return`, *)
(* pattern matches with field patterns, and primitives bound as providers,  *)
(* typed by their signatures (Audit.v Part IV, `prim_sig`).  Like `has_ty`  *)
(* it is a proof device: Audit.v does not mention it.  XSafety.v proves it  *)
(* sound for `xinterp` at every fuel.                                        *)
(*                                                                           *)
(* Scope.  Everything of the core, abstract functors and Rule A's captures *)
(* and pins included: a functor with a body has its signature in a table  *)
(* Fb beside Σ and its body in P.  The bootstrap IR's                       *)
(* XApplyBr, XBindFunSym and XBindFunCopy have no rule: FastPath.v (Track   *)
(* D) proves the program that uses them runs as the one that does not.     *)
(* The GC primitives (`Heap`, `Arrays`, `Mutable`) have                     *)
(* no signature in Part IV and so no rule; nor do the memory primitives at  *)
(* an `Addr` receiver (MossX.mem_args), which `prim_law` does not cover.    *)
(*                                                                           *)
(* Design.                                                                   *)
(*  - Types are Core Moss's `ty`.  A side table `Rp` gives some nominal      *)
(*    symbols a machine representation (I32, I64, F32, F64) or a struct     *)
(*    layout (fields and their types over the tag's telescope); every other *)
(*    nominal is a tag as in the core.  So the core's statics (formation,   *)
(*    satisfaction, merging, subtyping) are reused unchanged.               *)
(*  - The signature Σ carries no bodies: `g_fn Sg f` is (telescope,        *)
(*    signature, None) for every fn, and the program P holds the bodies     *)
(*    (`xp_fn`).  A fn is *defined* when P has a body for it.  Then         *)
(*    `wf_gsig Sg` is exactly declaration formation, and every lemma of     *)
(*    CoreMoss.v about the statics applies to Σ as it is.                   *)
(*  - The θ of `XTag` and `XCallD` is not read by the dynamics, and the     *)
(*    elaborator writes [] there (Audit.v, DRAFT NOTE 3): the rules take θ  *)
(*    from the derivation, not from the term (Curry style).  For the same  *)
(*    reason a type bind has an implicit form (`XtBindTyI`) beside          *)
(*    `XBindTy`: the elaborator emits no `XBindTy`.  The θ of a method key *)
(*    (`IMth`) and of an application stays syntactic: the dynamics reads it.*)
(*  - A judgment carries the enclosing fn's result type R (None where       *)
(*    `return` is not allowed) and whether `break` is allowed (L).          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit CoreMoss HTrie.
Import ListNotations.

Set Implicit Arguments.

(* ===================== 1. Representations ================================ *)

(* What a nominal symbol is at run time, if not a tag over a payload. *)
Inductive rep : Type :=
| RI32 | RI64 | RF32 | RF64
(* a GC reference (`Ref`) or GC array (`Array`): a tag over a heap address;
   the contents' type is the tag's θ at this symbol (`contents`) *)
| RRef : tsym -> rep
| RArr : tsym -> rep.

(* A store typing: an address's kind (true: an array) and its contents'
   type, keyed by the address's bits (HTrie.key, as XHeap.hty is). *)
Definition sty : Type := list bool -> option (bool * ty).

(* H' extends H. *)
Definition sext (H H' : sty) : Prop := forall kk c, H kk = Some c -> H' kk = Some c.

(* A reference or array type's contents: its θ at the symbol. *)
Definition contents (th : subst) (x : tsym) : ty :=
  match lookup_subst x th with Some t => t | None => TUnit end.

Fixpoint assoc_ty (f : fld) (fts : list (fld * ty)) : option ty :=
  match fts with
  | [] => None
  | (f', t) :: r => if Nat.eqb f f' then Some t else assoc_ty f r
  end.

Section XStatics.

Variable Sg : gsig.
Variable Rp : nsym -> option rep.
Variable P : xprog.
(* Functors with a body (Rule A, D84): for each, the arguments it is linked
   to, the inputs it captures where it is bound, and its pins.  Σ declares
   only abstract functors; a functor with a body has its signature here and
   its body in P (`xp_fun`), as a fn has its body in P. *)
Variable Fb : usym -> option (tele * tele * subst).

(* ===================== 2. Value typing =================================== *)

(* Values against *closed* types (a grounding has been applied), at a store
   typing H for the GC heap's references. *)
Inductive xvt (H : sty) : xvalue -> ty -> Prop :=
| XvUnit : xvt H XVUnit TUnit
| XvTag : forall n D taup th w,
    Rp n = None ->
    g_tag Sg n = Some (D, taup) ->
    xvt H w (app_subst th taup) ->
    xvt H (XVTag n w) (TNom n th)
| XvInj : forall w tau' ms,
    xvt H w tau' -> In tau' ms -> xvt H w (TUnion ms)
| XvI32 : forall n th i, Rp n = Some RI32 -> xvt H (XVI32 i) (TNom n th)
| XvI64 : forall n th z, Rp n = Some RI64 -> xvt H (XVI64 z) (TNom n th)
| XvF32 : forall n th x, Rp n = Some RF32 -> xvt H (XVF32 x) (TNom n th)
| XvF64 : forall n th x, Rp n = Some RF64 -> xvt H (XVF64 x) (TNom n th)
(* a record (CoreMoss.WfRec): its fields, in its type's order; a struct is
   a tag over one (XvTag) *)
| XvRec : forall fts fs,
    Rp rec_nom = None -> g_tag Sg rec_nom = None ->
    map fst fs = map fst fts ->
    Forall2 (fun ft fw => xvt H (snd fw) (snd ft)) fts fs ->
    xvt H (XVRec fs) (TNom rec_nom fts)
| XvRef : forall n th x a,
    Rp n = Some (RRef x) -> H (key a) = Some (false, contents th x) ->
    xvt H (XVTag n (XVRef a)) (TNom n th)
| XvArr : forall n th x a,
    Rp n = Some (RArr x) -> H (key a) = Some (true, contents th x) ->
    xvt H (XVTag n (XVRef a)) (TNom n th).

(* Bool, the type `if` and `while` test: the tag P names, over the union of
   its two units. *)
Definition bool_ty : ty := TNom (xp_bool P) [].

(* A Part IV signature type, read as a Core Moss type (closed or not: each
   case is stable under substitution). *)
Definition pty_ok (tau : ty) (t : xty) : Prop :=
  match t with
  | XTI32 => exists n th, tau = TNom n th /\ Rp n = Some RI32
  | XTI64 => exists n th, tau = TNom n th /\ Rp n = Some RI64
  | XTF32 => exists n th, tau = TNom n th /\ Rp n = Some RF32
  | XTF64 => exists n th, tau = TNom n th /\ Rp n = Some RF64
  | XTUnit => tau = TUnit
  | XTBool => tau = bool_ty
  (* an `Addr`: a tag over an I32 *)
  | XTAddr => exists n th D m thm, tau = TNom n th /\ Rp n = None
              /\ g_tag Sg n = Some (D, TNom m thm) /\ Rp m = Some RI32
  (* any tag *)
  | XTTag => exists n th, tau = TNom n th /\ Rp n = None /\ g_tag Sg n <> None
  end.

(* A primitive's signature: as a fn (Part IV, prim_sig), or as a method of
   `Memories` (prim_msig). *)
Definition prim_sigs (p : prim) (ps : list xty) (r : xty) : Prop :=
  prim_sig p = Some (ps, r) \/ prim_msig p = Some (ps, r).

(* t is a reference (k = false) or array (k = true) type with contents c. *)
Definition is_ref (k : bool) (t c : ty) : Prop :=
  exists n th x, t = TNom n th /\ Rp n = Some (if k then RArr x else RRef x)
  /\ contents th x = c.

(* The GC primitives' signatures (XHeap.gc_pre / gc_post read as types). *)
Definition gc_shape (p : prim) (taus : list ty) (ret : ty) : Prop :=
  match p, taus with
  | PBox rf, [c] => exists th x, ret = TNom rf th /\ Rp rf = Some (RRef x) /\ contents th x = c
  | PDeref, [r] => is_ref false r ret
  | PRefSet, [r; c] => is_ref false r c /\ ret = TUnit
  | PArrNew ar, [c; i] => pty_ok i XTI32
      /\ exists th x, ret = TNom ar th /\ Rp ar = Some (RArr x) /\ contents th x = c
  | PArrLen, [r] => (exists c, is_ref true r c) /\ pty_ok ret XTI32
  | PArrGet, [r; i] => pty_ok i XTI32 /\ is_ref true r ret
  | PArrSet, [r; i; c] => pty_ok i XTI32 /\ is_ref true r c /\ ret = TUnit
  | PArrFill, [r; i; c; n] => pty_ok i XTI32 /\ pty_ok n XTI32 /\ is_ref true r c /\ ret = TUnit
  | PArrCopy, [r; i; r2; j; n] => pty_ok i XTI32 /\ pty_ok j XTI32 /\ pty_ok n XTI32
      /\ (exists c, is_ref true r c /\ is_ref true r2 c) /\ ret = TUnit
  | _, _ => False
  end.

(* A primitive provides a fn or method of these parameter and result types:
   an unmodelled one any (it stops with XUns), one with a Part IV signature
   that signature, a GC primitive its shape. *)
Definition prim_ok (p : prim) (taus : list ty) (ret : ty) : Prop :=
  (exists c, p = PUnsupported c)
  \/ (exists ps r, prim_sigs p ps r /\ Forall2 pty_ok taus ps /\ pty_ok ret r)
  \/ gc_shape p taus ret.

(* ===================== 3. Patterns ======================================= *)

(* The variables a pattern binds, in the order `match_pat` binds them, for
   a scrutinee whose σ-image is tau0s (and whose type as written is tau0). *)
Inductive pat_ok (tau0 tau0s : ty) : xpat -> tenv -> Prop :=
| PoAny : pat_ok tau0 tau0s (XPAny None) []
| PoBind : forall x, pat_ok tau0 tau0s (XPAny (Some x)) [(x, tau0)]
| PoTag : forall n thn ms D taup ob,
    members tau0s = Some ms ->
    In (TNom n thn) ms ->
    Rp n = None ->
    g_tag Sg n = Some (D, taup) ->
    pat_ok tau0 tau0s (XPHead n ob [])
      (match ob with Some x => [(x, app_subst thn taup)] | None => [] end)
(* a struct's fields, each bound to a variable *)
| PoStruct : forall n thn ms D fts fs tys,
    members tau0s = Some ms ->
    In (TNom n thn) ms ->
    Rp n = None ->
    g_tag Sg n = Some (D, TNom rec_nom fts) ->
    Forall2 (fun fx t => assoc_ty (fst fx) fts = Some t) fs tys ->
    pat_ok tau0 tau0s (XPHead n None fs)
      (combine (map snd fs) (map (app_subst thn) tys)).

Definition pat_head (p : xpat) : option nsym :=
  match p with XPHead n _ _ => Some n | XPAny _ => None end.

(* Every value of the scrutinee's type is caught by some arm: one arm binds
   anything, or every member is a tag (a struct among them) whose head has an arm.  (A
   member at a machine representation is not a tag: no head pattern catches
   its values.) *)
Definition exhaustive (tau0s : ty) (arms : list (xpat * xexpr)) : Prop :=
  (exists ob eb, In (XPAny ob, eb) arms)
  \/ (exists ms, members tau0s = Some ms /\
        Forall (fun mem => exists n th, mem = TNom n th
                   /\ Rp n = None /\ g_tag Sg n <> None
                   /\ exists ob fs eb, In (XPHead n ob fs, eb) arms) ms).

(* ===================== 4. The judgment =================================== *)

(* Capture at `bind φ = h;` (CoreMoss.cap_ok, reading h's captured inputs Dc
   and pins from Fb): h's captured inputs are available here and answered
   from here (smc), its pins hold here, and a captured symbol that is also
   an input or output of φ is one of φ's fixed inputs. *)
Definition xcap_ok (Phi : ctx) (Du Dr : tele) (u : usym) (Dc : tele) (pins : subst)
    (smc : smap) : Prop :=
  sat0 Sg Phi Dc
  /\ smap_ok Phi (id_app Dc) smc Dc
  /\ Forall (fun p => app_subst (cS Phi) (TAbs (fst p)) = app_subst (cS Phi) (snd p)) pins
  /\ (forall x, In x (tyreqs Dc) -> In x (tyreqs Du ++ tyreqs Dr) -> In x (g_fixed Sg u)).

Definition ret_wf (Phi : ctx) (R : option ty) : Prop :=
  match R with Some r => wf_ty Sg Phi r | None => True end.

Definition ret_fresh (ts : list tsym) (R : option ty) : Prop :=
  match R with Some r => Forall (fun t => ~ In t (ty_frees r)) ts | None => True end.

Inductive xhas (R : option ty) : bool -> ctx -> tenv -> xexpr -> ty -> Prop :=
(* ---- the core's rules (CoreMoss.has_ty), but abstract functors ---- *)
| XtVar : forall L Phi G x tau,
    lookup_tenv x G = Some tau ->
    xhas R L Phi G (XVar x) tau
| XtUnit : forall L Phi G,
    xhas R L Phi G XUnit TUnit
| XtSub : forall L Phi G e tau' tau,
    xhas R L Phi G e tau' ->
    subty Phi tau' tau ->
    wf_ty Sg Phi tau ->
    xhas R L Phi G e tau
| XtVal : forall L Phi G v Dv tauv,
    g_val Sg v = Some (Dv, tauv) ->
    avail Phi (IVal v) ->
    xhas R L Phi G (XVl v) (app_subst (cS Phi) tauv)
| XtTag : forall L Phi G n D tau0 th th0 e,
    Rp n = None ->
    g_tag Sg n = Some (D, tau0) ->
    sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    xhas R L Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
    xhas R L Phi G (XTag n th0 e) (app_subst (cS Phi) (TNom n th))
| XtNeed : forall L Phi G f Df S args,
    g_fn Sg f = Some (Df, S, None) ->
    avail Phi (IFn f) ->
    Forall2 (fun e tau => xhas R L Phi G e (app_subst (cS Phi) tau))
            args (fs_params S) ->
    xhas R L Phi G (XCallA f args) (app_subst (cS Phi) (fs_ret S))
| XtCall : forall L Phi G f Df S body th th0 sm args,
    g_fn Sg f = Some (Df, S, None) ->
    xp_fn P f = Some body ->
    sat Sg Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Df ->
    Forall2 (fun e tau =>
               xhas R L Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params S) ->
    xhas R L Phi G (XCallD f th0 sm args)
         (app_subst (cS Phi) (app_subst th (fs_ret S)))
| XtMeth : forall L Phi G e0 tau0 m Dm S rt0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    xhas R L Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                app_subst_item (cS Phi) i1
                = app_subst_item (cS Phi) (IMth rt0 m th0)) ->
    Forall2 (fun e tau =>
               xhas R L Phi G e
                    (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    xhas R L Phi G (XMeth e0 m (IMth rt0 m th0) args)
         (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S))
| XtLet : forall L Phi G x e1 e2 tau1 tau,
    xhas R L Phi G e1 tau1 ->
    (* regularity of what the variable stores, as a premise *)
    incl (ty_frees tau1) (tyreqs (cA Phi)) ->
    xhas R L Phi ((x, tau1) :: G) e2 tau ->
    xhas R L Phi G (XLet x e1 e2) tau
| XtBindTy : forall L Phi G t tau' e tau,
    g_ty Sg t = true ->
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    xhas R L (bind_ty t tau' Phi) G e tau ->
    wf_ty Sg Phi tau ->
    ret_wf Phi R ->
    xhas R L Phi G (XBindTy t tau' e) tau
(* the same, with the bind erased from the term (the elaborator's output) *)
| XtBindTyI : forall L Phi G t tau' e tau,
    g_ty Sg t = true ->
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    xhas R L (bind_ty t tau' Phi) G e tau ->
    wf_ty Sg Phi tau ->
    ret_wf Phi R ->
    xhas R L Phi G e tau
| XtBindVal : forall L Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    xhas R L Phi G e1 (app_subst (cS Phi) tauv) ->
    xhas R L (add_item (IVal v) Phi) G e tau ->
    xhas R L Phi G (XBindVal v e1 e) tau
| XtBindFn : forall L Phi G f Df S f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', None) ->
    xp_fn P f' = Some body ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    sig_eq (cS Phi) S' S ->
    xhas R L (add_item (IFn f) Phi) G e tau ->
    xhas R L Phi G (XBindFn f f' sm e) tau
| XtBindMth : forall L Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    msat th Dm S ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', None) ->
    xp_fn P f' = Some body ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    xhas R L (add_item (IMth tauk m th) Phi) G e tau ->
    xhas R L Phi G (XBindMth tauk m th f' sm e) tau
(* ---- primitives, typed by their signatures ---- *)
| XtBindPrim : forall L Phi G f Df S p e tau,
    g_fn Sg f = Some (Df, S, None) ->
    sat0 Sg Phi Df ->
    prim_ok p (map (app_subst (cS Phi)) (fs_params S)) (app_subst (cS Phi) (fs_ret S)) ->
    xhas R L (add_item (IFn f) Phi) G e tau ->
    xhas R L Phi G (XBindPrim (IFn f) p e) tau
| XtBindPrimM : forall L Phi G tauk m Dm S th p e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    msat th Dm S ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    prim_ok p (app_subst (cS Phi) tauk
               :: map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S))
            (interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S)) ->
    xhas R L (add_item (IMth tauk m th) Phi) G e tau ->
    xhas R L Phi G (XBindPrim (IMth tauk m th) p e) tau
(* ---- abstract functors [D78], with Rule A's captures and pins (D84) ---- *)
| XtBindFun : forall L Phi G u Du Dr h Dh Dc pins sm smc e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    Fb h = Some (Dh, Dc, pins) ->
    (exists bs, xp_fun P h = Some (Dr, bs)) ->
    fun_link Sg Dh sm Du ->
    xcap_ok Phi Du Dr u Dc pins smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    xhas R L (add_item (IFun u) Phi) G e tau ->
    xhas R L Phi G (XBindFun u h sm smc e) tau
| XtBindFunA : forall L Phi G u Du Dr w Dw sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    xhas R L (add_item (IFun u) Phi) G e tau ->
    xhas R L Phi G (XBindFunA u w sm e) tau
| XtApply : forall L Phi G u Du Dr th sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    fixed_ok Sg Phi u th ->
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    xhas R L (open_res (app_tele th Dr) Phi) G e tau ->
    Forall (fun t => ~ In t (ty_frees tau)) (tyreqs Dr) ->
    ret_fresh (tyreqs Dr) R ->
    xhas R L Phi G (XApply u th sm e) tau
(* ---- MossX's extensions ---- *)
(* a record, field by field in its type's order (a struct is XtTag over
   one) *)
| XtRec : forall L Phi G fs fts,
    map fst fs = map fst fts ->
    Forall2 (fun fe ft => xhas R L Phi G (snd fe) (snd ft)) fs fts ->
    xhas R L Phi G (XRec fs) (TNom rec_nom fts)
(* a field of a record *)
| XtFieldR : forall L Phi G e tau fts f tf,
    xhas R L Phi G e tau ->
    wf_ty Sg Phi (app_subst (cS Phi) tau) ->
    app_subst (cS Phi) tau = TNom rec_nom fts ->
    assoc_ty f fts = Some tf ->
    xhas R L Phi G (XField e f) tf
(* a field of a struct: of the record its tag is over *)
| XtField : forall L Phi G e tau n th D fts f tf,
    xhas R L Phi G e tau ->
    wf_ty Sg Phi (app_subst (cS Phi) tau) ->
    app_subst (cS Phi) tau = TNom n th ->
    Rp n = None ->
    g_tag Sg n = Some (D, TNom rec_nom fts) ->
    assoc_ty f fts = Some tf ->
    xhas R L Phi G (XField e f) (app_subst th tf)
| XtSeq : forall L Phi G e1 e2 tau1 tau,
    xhas R L Phi G e1 tau1 ->
    xhas R L Phi G e2 tau ->
    xhas R L Phi G (XSeq e1 e2) tau
| XtAssign : forall L Phi G x e tx,
    lookup_tenv x G = Some tx ->
    xhas R L Phi G e tx ->
    xhas R L Phi G (XAssign x e) TUnit
| XtIf : forall L Phi G c e1 e2 tau,
    xhas R L Phi G c bool_ty ->
    xhas R L Phi G e1 tau ->
    xhas R L Phi G e2 tau ->
    xhas R L Phi G (XIf c e1 e2) tau
| XtWhile : forall L Phi G c b tb,
    xhas R L Phi G c bool_ty ->
    xhas R true Phi G b tb ->
    xhas R L Phi G (XWhile c b) TUnit
| XtLoop : forall L Phi G b tb,
    xhas R true Phi G b tb ->
    xhas R L Phi G (XLoop b) TUnit
| XtReturn : forall L Phi G e rt tau,
    R = Some rt ->
    xhas R L Phi G e rt ->
    xhas R L Phi G (XReturn e) tau
| XtBreak : forall Phi G tau,
    xhas R true Phi G XBreak tau
| XtMatchP : forall L Phi G e0 tau0 arms Gbs tau,
    xhas R L Phi G e0 tau0 ->
    wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
    (forall ms, members (app_subst (cS Phi) tau0) = Some ms ->
                NoDup (map member_head ms)) ->
    exhaustive (app_subst (cS Phi) tau0) arms ->
    Forall2 (fun a Gb => pat_ok tau0 (app_subst (cS Phi) tau0) (fst a) Gb) arms Gbs ->
    Forall (fun Gb => Forall (fun p => incl (ty_frees (snd p)) (tyreqs (cA Phi))) Gb) Gbs ->
    Forall2 (fun a Gb => xhas R L Phi (Gb ++ G) (snd a) tau) arms Gbs ->
    xhas R L Phi G (XMatchP e0 arms) tau.

(* A functor body's binds (CoreMoss.binds_ty): each checked with the earlier
   ones in force, a val's expression under no variables, no `return` and no
   `break`. *)
Inductive xbinds_ty : ctx -> list xbind -> ctx -> Prop :=
| XbNil : forall Phi, xbinds_ty Phi [] Phi
| XbTy : forall Phi t tau' r Phi',
    g_ty Sg t = true ->
    t <> this_sym ->
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    xbinds_ty (bind_ty t tau' Phi) r Phi' ->
    xbinds_ty Phi (XBTy t tau' :: r) Phi'
| XbVal : forall Phi v Dv tauv e1 r Phi',
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    xhas None false Phi [] e1 (app_subst (cS Phi) tauv) ->
    xbinds_ty (add_item (IVal v) Phi) r Phi' ->
    xbinds_ty Phi (XBVal v e1 :: r) Phi'
| XbFn : forall Phi f Df S f' D' S' body sm r Phi',
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', None) ->
    xp_fn P f' = Some body ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    sig_eq (cS Phi) S' S ->
    xbinds_ty (add_item (IFn f) Phi) r Phi' ->
    xbinds_ty Phi (XBFn f f' sm :: r) Phi'
| XbMth : forall Phi tauk m Dm S th f' D' S' body sm tau0' rest r Phi',
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    msat th Dm S ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', None) ->
    xp_fn P f' = Some body ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    xbinds_ty (add_item (IMth tauk m th) Phi) r Phi' ->
    xbinds_ty Phi (XBMth tauk m th f' sm :: r) Phi'
| XbPrim : forall Phi f Df S p r Phi',
    g_fn Sg f = Some (Df, S, None) ->
    sat0 Sg Phi Df ->
    prim_ok p (map (app_subst (cS Phi)) (fs_params S)) (app_subst (cS Phi) (fs_ret S)) ->
    xbinds_ty (add_item (IFn f) Phi) r Phi' ->
    xbinds_ty Phi (XBPrim (IFn f) p :: r) Phi'
| XbPrimM : forall Phi tauk m Dm S th p r Phi',
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    msat th Dm S ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    prim_ok p (app_subst (cS Phi) tauk
               :: map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S))
            (interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S)) ->
    xbinds_ty (add_item (IMth tauk m th) Phi) r Phi' ->
    xbinds_ty Phi (XBPrim (IMth tauk m th) p :: r) Phi'
| XbFun : forall Phi u Du Dr h Dh Dc pins sm smc r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    Fb h = Some (Dh, Dc, pins) ->
    (exists bs, xp_fun P h = Some (Dr, bs)) ->
    fun_link Sg Dh sm Du ->
    xcap_ok Phi Du Dr u Dc pins smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    xbinds_ty (add_item (IFun u) Phi) r Phi' ->
    xbinds_ty Phi (XBFun u h sm smc :: r) Phi'
| XbFunA : forall Phi u Du Dr w Dw sm r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) ->
    fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    xbinds_ty (add_item (IFun u) Phi) r Phi' ->
    xbinds_ty Phi (XBFunA u w sm :: r) Phi'
| XbApply : forall Phi u Du Dr th sm r Phi',
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    fixed_ok Sg Phi u th ->
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    xbinds_ty (open_res (app_tele th Dr) Phi) r Phi' ->
    xbinds_ty Phi (XBApply u th sm :: r) Phi'.

(* ===================== 5. Programs ======================================= *)

(* Bool as `truthy` and Part IV's xvty read it. *)
Definition bool_ok : Prop :=
  Rp (xp_bool P) = None /\ Rp (xp_true P) = None /\ Rp (xp_false P) = None
  /\ (exists Db, g_tag Sg (xp_bool P)
                 = Some (Db, TUnion [TNom (xp_true P) []; TNom (xp_false P) []]))
  /\ (exists Dt, g_tag Sg (xp_true P) = Some (Dt, TUnit))
  /\ (exists Df, g_tag Sg (xp_false P) = Some (Df, TUnit)).

(* A well-formed MossX program: Σ's declarations are well-formed (the core's
   wf_gsig, with no bodies in Σ), every body in P is typed at its
   declaration, and the representation table is coherent with Σ. *)
Record xwf : Prop := {
  xwf_decls : wf_gsig Sg;
  xwf_no_core_bodies : forall f D S b, g_fn Sg f = Some (D, S, b) -> b = None;
  (* Σ declares only abstract functors *)
  xwf_fun_abstract : forall u Du Dr b, g_fun Sg u = Some (Du, Dr, b) -> b = None;
  (* a functor with a body (CoreMoss.wf_fun_decl's body clause, Rule A):
     its linked arguments, captured inputs and pins form; its result is
     formed over them; and its binds, checked once there, leave every
     result item in force *)
  xwf_fb : forall h Dh Dc pins Dr bs,
      Fb h = Some (Dh, Dc, pins) -> xp_fun P h = Some (Dr, bs) ->
      exists PhiD PhiC, wf_tele Sg empty_ctx Dh PhiD
      /\ wf_tele Sg empty_ctx Dc PhiC
      /\ pins_ok pins PhiC
      /\ wf_tele Sg (fun_ctx PhiD PhiC pins) Dr
           (mkCtx (cA (fun_ctx PhiD PhiC pins) ++ Dr) (cS (fun_ctx PhiD PhiC pins)))
      /\ exists PhiB, xbinds_ty (fun_ctx PhiD PhiC pins) bs PhiB
         /\ Forall (fun i => In i (cA PhiB)) Dr;
  xwf_bodies : forall f D S body,
      g_fn Sg f = Some (D, S, None) -> xp_fn P f = Some body ->
      exists PhiD, wf_tele Sg empty_ctx D PhiD
      /\ xhas (Some (fs_ret S)) false PhiD (body_env S) body (fs_ret S);
  xwf_bool : bool_ok;
  (* the record head is no symbol's: records are only records *)
  xwf_rec : Rp rec_nom = None /\ g_tag Sg rec_nom = None
}.

End XStatics.
