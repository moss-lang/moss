(* ------------------------------------------------------------------------ *)
(* CoreMoss.v — mechanization of the calculus in core-moss.tex               *)
(*                                                                           *)
(* This file defines the syntax, static semantics, and dynamic semantics of  *)
(* Core Moss, and states the metatheory of the paper's §4.  Everything is    *)
(* definitional except where marked: Proposition 4.1 is proved, the paper's  *)
(* Figure 1 example is encoded and *computed* to a value by the executable   *)
(* interpreter (twice — once erased — as evidence for Proposition 4.3), and  *)
(* the remaining theorems are stated precisely and Admitted.                 *)
(*                                                                           *)
(* Deviations from the paper, all mechanization-level:                       *)
(*                                                                           *)
(* 1. Elaborated normal form.  The paper lets a reference omit bindings for  *)
(*    requirements already in force, reading it through the completion θ̂     *)
(*    (rules S-Pass/S-Inst).  Here every nominal reference and application   *)
(*    is total as written — elaboration has materialized θ̂, with pass-       *)
(*    through requirements appearing as identity entries (t, TAbs t).  The   *)
(*    satisfaction judgment therefore checks coverage plus availability      *)
(*    rather than building a completion.                                     *)
(* 2. Satisfaction and availability are recursive definitions (Forall over   *)
(*    the telescope) rather than inference rules; they are equivalent to     *)
(*    the paper's S-rules read under (1).                                    *)
(* 3. Match arms are ordered to align with the scrutinee's members, rather   *)
(*    than being an unordered bijection; unions are not quotiented by        *)
(*    member order.                                                          *)
(* 4. As in the paper's §3, every method provider takes the receiver as its  *)
(*    first argument; `this` does not appear.                                *)
(* 5. The dynamics is over elaborated terms: satisfaction maps ς and chosen  *)
(*    provisions ι₀ are fields of the syntax, and the typing rules check     *)
(*    them (the paper's "elaboration records these choices").                *)
(* 6. Provisions are keyed by the receiver's full type, not merely its       *)
(*    head.  The paper's §1 states that the dispatch key is the pair         *)
(*    (receiver *type*, method symbol); its typing figure keys on the head   *)
(*    as a shortcut, adequate under Moss's unique-nominal-per-instantiation  *)
(*    idiom [D51] but unsound in the raw calculus — a provision bound at     *)
(*    Slot[Int] would be found for a Slot[Char] receiver, and the This-      *)
(*    substitution would lie.  Discovered while stating preservation; the    *)
(*    correction belongs in the paper's figure too.                          *)
(* 7. Type binds are fresh: bind t = τ requires t not already in force.      *)
(*    Surface Moss allows lexical shadowing [D25]; under substitution-based  *)
(*    statics, shadowing an in-use abstract type symbol would conflate its   *)
(*    two generations (types already stored in Γ would be re-read under the  *)
(*    new binding).  Val/fn/method binds shadow harmlessly and remain        *)
(*    unrestricted.  Found while designing the preservation invariant.       *)
(* 8. Satisfaction carries a coherence premise: a val or fn requirement      *)
(*    names no types, so nothing else stops f[T=Bool] from being satisfied   *)
(*    by an ambient v : T that was provided under bind T=Int.  Where a       *)
(*    non-type requirement is answered from the ambient context, the         *)
(*    application must agree with the context on the type symbols that       *)
(*    requirement's own telescope mentions.  (Method items already carry     *)
(*    their receiver and application, so they were immune — see (6).)  Also  *)
(*    found while designing the preservation invariant.                      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith.
Import ListNotations.

Set Implicit Arguments.

(* ===================== 1. Symbols ======================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)

(* The distinguished receiver type of method signatures. *)
Definition this_sym : tsym := 0.

(* ===================== 2. Types and substitutions ======================== *)

Inductive ty : Type :=
| TUnit  : ty
| TAbs   : tsym -> ty                       (* abstract type symbol (incl. This) *)
| TNom   : nsym -> list (tsym * ty) -> ty   (* total applied nominal reference *)
| TUnion : list ty -> ty.                   (* members must be nominal refs *)

Definition subst := list (tsym * ty).

(* A custom induction principle: the auto-generated one is useless for the
   nested lists inside TNom/TUnion. *)
Fixpoint ty_ind' (P : ty -> Prop)
    (HU : P TUnit)
    (HA : forall t, P (TAbs t))
    (HN : forall n th, Forall (fun p => P (snd p)) th -> P (TNom n th))
    (HUn : forall ms, Forall P ms -> P (TUnion ms))
    (t : ty) {struct t} : P t :=
  match t with
  | TUnit => HU
  | TAbs a => HA a
  | TNom n th =>
      HN n th
        ((fix go (l : list (tsym * ty)) : Forall (fun p => P (snd p)) l :=
            match l with
            | [] => Forall_nil (fun p => P (snd p))
            | p :: r => @Forall_cons _ (fun p => P (snd p)) p r
                          (ty_ind' HU HA HN HUn (snd p)) (go r)
            end) th)
  | TUnion ms =>
      HUn ms
        ((fix go (l : list ty) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | t' :: r => @Forall_cons _ P t' r (ty_ind' HU HA HN HUn t') (go r)
            end) ms)
  end.

Fixpoint lookup_subst (t : tsym) (s : subst) : option ty :=
  match s with
  | [] => None
  | (t', tau) :: r => if Nat.eqb t t' then Some tau else lookup_subst t r
  end.

(* Applying a substitution.  Substitutions accumulated by the statics are
   idempotent by construction (bind t = τ stores στ), so one pass suffices. *)
Fixpoint app_subst (s : subst) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TAbs t => match lookup_subst t s with Some tau' => tau' | None => TAbs t end
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => (fst p, app_subst s (snd p)) :: go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => app_subst s t' :: go r
                 end) ms)
  end.

Lemma app_subst_nil : forall tau, app_subst [] tau = tau.
Proof.
  induction tau using ty_ind'; simpl; try reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. destruct x; reflexivity.
  - f_equal. induction H; simpl; [reflexivity|].
    rewrite H, IHForall. reflexivity.
Qed.

(* Boolean equality on types. *)
Fixpoint ty_eqb (a b : ty) {struct a} : bool :=
  match a, b with
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => Nat.eqb t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

(* ===================== 3. Items and telescopes =========================== *)

(* A method item carries the *receiver type* it is provided at: the paper's
   §1 key, the pair (receiver type, method symbol), plus the application θ
   that interprets the method's own requirements (deviation 6). *)
Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : ty -> msym -> subst -> item.

Definition tele := list item.

Fixpoint subst_eqb (a b : subst) : bool :=
  match a, b with
  | [], [] => true
  | p1 :: r1, p2 :: r2 =>
      Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && subst_eqb r1 r2
  | _, _ => false
  end.

Definition item_eqb (a b : item) : bool :=
  match a, b with
  | ITy t1, ITy t2 => Nat.eqb t1 t2
  | IVal v1, IVal v2 => Nat.eqb v1 v2
  | IFn f1, IFn f2 => Nat.eqb f1 f2
  | IMth r1 m1 th1, IMth r2 m2 th2 =>
      ty_eqb r1 r2 && Nat.eqb m1 m2 && subst_eqb th1 th2
  | _, _ => false
  end.

(* The head a union member discriminates by. *)
Definition member_head (tau : ty) : option nsym :=
  match tau with
  | TNom n _ => Some n
  | _ => None
  end.

Definition map_snd (A B C : Type) (f : B -> C) (l : list (A * B)) : list (A * C) :=
  map (fun p => (fst p, f (snd p))) l.

(* A substitution acts on an item through the types it carries. *)
Definition app_subst_item (s : subst) (i : item) : item :=
  match i with
  | ITy t => ITy t
  | IVal v => IVal v
  | IFn f => IFn f
  | IMth rt m th => IMth (app_subst s rt) m (map_snd (app_subst s) th)
  end.

(* ===================== 4. Contexts in force ============================== *)

(* Φ = ⟨A; σ⟩: available items and the static substitution from type binds. *)
Record ctx : Type := mkCtx { cA : list item; cS : subst }.

(* Availability, ι ∈ Φ: membership in A modulo σ (the paper's residue of key
   canonicalization).  Defined as a boolean so that Proposition 4.1's
   decidability claim is discharged by construction. *)
Definition item_subst_eqb (s : subst) (i1 i2 : item) : bool :=
  item_eqb (app_subst_item s i1) (app_subst_item s i2).

Definition avail_b (Phi : ctx) (i : item) : bool :=
  existsb (fun i0 => item_subst_eqb (cS Phi) i0 i) (cA Phi).

Definition avail (Phi : ctx) (i : item) : Prop := avail_b Phi i = true.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

Definition tyreqs (D : tele) : list tsym :=
  flat_map (fun i => match i with ITy t => [t] | _ => [] end) D.

(* The identity application: the paper's ∅ ⊨ Δ, materialized. *)
Definition id_app (D : tele) : subst :=
  map (fun t => (t, TAbs t)) (tyreqs D).

Definition add_item (i : item) (Phi : ctx) : ctx :=
  mkCtx (i :: cA Phi) (cS Phi).

Definition bind_ty (t : tsym) (tau : ty) (Phi : ctx) : ctx :=
  mkCtx (ITy t :: cA Phi) ((t, app_subst (cS Phi) tau) :: cS Phi).

(* ===================== 5. Expressions and values ========================= *)

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

Inductive expr : Type :=
| EVar     : var -> expr
| EUnit    : expr
| EVl      : vsym -> expr
| ETag     : nsym -> subst -> expr -> expr
| ECallD   : fsym -> subst -> smap -> list expr -> expr   (* defined fn call *)
| ECallA   : fsym -> list expr -> expr                    (* abstract fn call *)
| EMeth    : expr -> msym -> item -> list expr -> expr    (* item: the provision *)
| EMatch   : expr -> list (nsym * var * expr) -> expr
| ELet     : var -> expr -> expr -> expr
| EBindTy  : tsym -> ty -> expr -> expr
| EBindVal : vsym -> expr -> expr -> expr
| EBindFn  : fsym -> fsym -> smap -> expr -> expr
| EBindMth : ty -> msym -> subst -> fsym -> smap -> expr -> expr.
(* In EBindMth the first field is the receiver type; its head is the key. *)

Inductive value : Type :=
| VUnit : value
| VTag  : nsym -> value -> value.

(* ===================== 6. Global signatures ============================== *)

Record fnsig : Type := mkSig { fs_params : list ty; fs_ret : ty }.

(* Σ: every symbol's kind, signature, and (concatenated) enclosing telescope.
   Method signatures may mention TAbs this_sym; the receiver is understood as
   an implicit first argument. *)
Record gsig : Type := mkGsig {
  g_ty  : tsym -> bool;
  g_tag : nsym -> option (tele * ty);
  g_val : vsym -> option (tele * ty);
  g_fn  : fsym -> option (tele * fnsig * option expr);
  g_mth : msym -> option (tele * fnsig)
}.

(* The type symbols a val/fn requirement's own telescope mentions — the
   dependencies whose instantiations coherence must pin down (deviation 8). *)
Definition dep_tys (Sg : gsig) (i : item) : list tsym :=
  match i with
  | IVal v => match g_val Sg v with Some (Dv, _) => tyreqs Dv | None => [] end
  | IFn f => match g_fn Sg f with Some (Df, _, _) => tyreqs Df | None => [] end
  | _ => []
  end.

(* Satisfaction Φ ⊢ θ ⊨ Δ, in elaborated normal form: θ is defined exactly
   on Δ's type items (identity entries playing the paper's S-Pass); every
   other requirement, read under θ, is available; and — coherence — where a
   non-type requirement is answered from the ambient context, θ must agree
   with the context on the type symbols that requirement depends on
   (deviation 8). *)
Definition sat (Sg : gsig) (Phi : ctx) (th : subst) (D : tele) : Prop :=
  map fst th = tyreqs D /\
  Forall (fun i =>
            is_ty_item i = true \/
            (avail Phi (app_subst_item th i) /\
             Forall (fun t =>
                       app_subst (cS Phi) (app_subst th (TAbs t))
                       = app_subst (cS Phi) (TAbs t))
                    (dep_tys Sg i))) D.

Definition sat0 (Sg : gsig) (Phi : ctx) (D : tele) : Prop :=
  sat Sg Phi (id_app D) D.

(* ===================== 7. Type formation and subtyping =================== *)

Definition is_nom (tau : ty) : Prop := exists n th, tau = TNom n th.

Definition heads_of (s : subst) (ms : list ty) : list (option nsym) :=
  map (fun tau => member_head (app_subst s tau)) ms.

Inductive wf_ty (Sg : gsig) (Phi : ctx) : ty -> Prop :=
| WfUnit : wf_ty Sg Phi TUnit
| WfNeed : forall t, avail Phi (ITy t) -> wf_ty Sg Phi (TAbs t)
| WfTag : forall n D tau0 th,
    g_tag Sg n = Some (D, tau0) ->
    sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    wf_ty Sg Phi (TNom n th)
| WfUnion : forall ms,
    Forall (wf_ty Sg Phi) ms ->
    Forall is_nom ms ->
    NoDup (heads_of (cS Phi) ms) ->
    wf_ty Sg Phi (TUnion ms).

(* Type equality up to the substitution in force (the applicative identity
   rule [D18]); unions are compared in order (deviation 3). *)
Definition ty_eq (s : subst) (t1 t2 : ty) : Prop :=
  app_subst s t1 = app_subst s t2.

(* Subtyping: injection only [D17]. *)
Inductive subty (Phi : ctx) : ty -> ty -> Prop :=
| SubEq : forall t1 t2, ty_eq (cS Phi) t1 t2 -> subty Phi t1 t2
| SubInj : forall n th ms tau',
    In tau' ms ->
    ty_eq (cS Phi) (TNom n th) tau' ->
    subty Phi (TNom n th) (TUnion ms).

(* The members a match must cover: a nominal ref is its singleton union. *)
Definition members (tau : ty) : option (list ty) :=
  match tau with
  | TNom n th => Some [TNom n th]
  | TUnion ms => Some ms
  | _ => None
  end.

(* ===================== 8. Typing ========================================= *)

Definition tenv := list (var * ty).

Fixpoint lookup_tenv (x : var) (G : tenv) : option ty :=
  match G with
  | [] => None
  | (x', tau) :: r => if Nat.eqb x x' then Some tau else lookup_tenv x r
  end.

Fixpoint lookup_smap (i : item) (sm : smap) : option item :=
  match sm with
  | [] => None
  | (a, b) :: r => if item_eqb i a then Some b else lookup_smap i r
  end.

(* ς correctness: each non-type requirement, read under the application θ, is
   sent to an in-force item with the same σ-image. *)
Definition item_matches (s th : subst) (i i' : item) : Prop :=
  app_subst_item s (app_subst_item th i) = app_subst_item s i'.

Definition smap_ok (Phi : ctx) (th : subst) (sm : smap) (D : tele) : Prop :=
  map fst sm = filter (fun i => negb (is_ty_item i)) D /\
  Forall (fun p => In (snd p) (cA Phi)
                   /\ item_matches (cS Phi) th (fst p) (snd p)) sm.

(* Interpreting a method signature at a receiver: ρ = σ ∘ θ ∘ [This ↦ τ₀]. *)
Definition this_subst (tau0 : ty) : subst := [(this_sym, tau0)].

Definition interp_m (s th : subst) (tau0 tau : ty) : ty :=
  app_subst s (app_subst th (app_subst (this_subst tau0) tau)).

(* "ι is a provision of m at receiver type τ₀", judged after σ. *)
Definition is_prov_at (s : subst) (tau0 : ty) (m : msym) (i : item) : Prop :=
  exists th', app_subst_item s i = IMth (app_subst s tau0) m th'.

Definition sig_eq (s : subst) (S1 S2 : fnsig) : Prop :=
  map (app_subst s) (fs_params S1) = map (app_subst s) (fs_params S2)
  /\ app_subst s (fs_ret S1) = app_subst s (fs_ret S2).

(* Match arms.  An arm (N, x, e) handles member N, binding x to the payload.
   These helpers keep the recursive premise of T-Match a direct application
   (Rocq's strict-positivity checker cannot see through matches or exists). *)
Definition arm_name (arm : nsym * var * expr) : nsym := fst (fst arm).
Definition arm_var (arm : nsym * var * expr) : var := snd (fst arm).
Definition arm_body (arm : nsym * var * expr) : expr := snd arm.

(* The payload type an arm's binder receives from a member N[θ]: the tag's
   declared payload under θ.  (Total for the recursion's sake; the shape
   premise `arm_matches` rules the junk cases out.) *)
Definition member_payload (Sg : gsig) (mem : ty) : ty :=
  match mem with
  | TNom n thn =>
      match g_tag Sg n with
      | Some (_, taup) => app_subst thn taup
      | None => TUnit
      end
  | _ => TUnit
  end.

Definition arm_matches (Sg : gsig) (mem : ty) (arm : nsym * var * expr) : Prop :=
  match mem with
  | TNom n _ => arm_name arm = n /\ exists D taup, g_tag Sg n = Some (D, taup)
  | _ => False
  end.

Inductive has_ty (Sg : gsig) : ctx -> tenv -> expr -> ty -> Prop :=
| TVarR : forall Phi G x tau,
    lookup_tenv x G = Some tau ->
    has_ty Sg Phi G (EVar x) tau
| TUnitR : forall Phi G,
    has_ty Sg Phi G EUnit TUnit
| TSubR : forall Phi G e tau' tau,
    has_ty Sg Phi G e tau' ->
    subty Phi tau' tau ->
    has_ty Sg Phi G e tau
| TValR : forall Phi G v Dv tauv,
    g_val Sg v = Some (Dv, tauv) ->
    avail Phi (IVal v) ->
    has_ty Sg Phi G (EVl v) (app_subst (cS Phi) tauv)
| TTagR : forall Phi G n D tau0 th e,
    g_tag Sg n = Some (D, tau0) ->
    sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
    has_ty Sg Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th))
| TNeedR : forall Phi G f Df S args,
    g_fn Sg f = Some (Df, S, None) ->
    avail Phi (IFn f) ->
    Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
            args (fs_params S) ->
    has_ty Sg Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S))
| TCallR : forall Phi G f Df S body th sm args,
    g_fn Sg f = Some (Df, S, Some body) ->
    sat Sg Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Df ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params S) ->
    has_ty Sg Phi G (ECallD f th sm args)
           (app_subst (cS Phi) (app_subst th (fs_ret S)))
| TMethR : forall Phi G e0 tau0 m Dm S rt0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    has_ty Sg Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    (* the provision's receiver type is the receiver's type, after σ *)
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    (* dispatch demands exactly one hit [D1] *)
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                i1 = IMth rt0 m th0) ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e
                      (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    has_ty Sg Phi G (EMeth e0 m (IMth rt0 m th0) args)
           (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S))
| TMatchR : forall Phi G e0 tau0 ms arms tau,
    has_ty Sg Phi G e0 tau0 ->
    members (app_subst (cS Phi) tau0) = Some ms ->
    (* regularity, made local: a wf scrutinee type has distinct heads, and
       find_arm needs that to pick the arm the injection was typed at *)
    NoDup (map member_head ms) ->
    Forall2 (arm_matches Sg) ms arms ->
    Forall2 (fun mem arm =>
               has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                      (arm_body arm) tau) ms arms ->
    has_ty Sg Phi G (EMatch e0 arms) tau
| TLetR : forall Phi G x e1 e2 tau1 tau,
    has_ty Sg Phi G e1 tau1 ->
    has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
    has_ty Sg Phi G (ELet x e1 e2) tau
| TBindTyR : forall Phi G t tau' e tau,
    g_ty Sg t = true ->
    (* freshness: no shadowing of an in-force type symbol (deviation 7) *)
    ~ In (ITy t) (cA Phi) ->
    wf_ty Sg Phi tau' ->
    has_ty Sg (bind_ty t tau' Phi) G e tau ->
    has_ty Sg Phi G (EBindTy t tau' e) tau
| TBindValR : forall Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Sg Phi Dv ->
    has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
    has_ty Sg (add_item (IVal v) Phi) G e tau ->
    has_ty Sg Phi G (EBindVal v e1 e) tau
| TBindFnR : forall Phi G f Df S f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi Df ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* signature match after the substitutions in force; never inferred [D27] *)
    sig_eq (cS Phi) S' S ->
    has_ty Sg (add_item (IFn f) Phi) G e tau ->
    has_ty Sg Phi G (EBindFn f f' sm e) tau
| TBindMthR : forall Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    sat Sg Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* the provider's first parameter is the receiver *)
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
    has_ty Sg Phi G (EBindMth tauk m th f' sm e) tau.

(* ===================== 9. Telescope, declaration, program formation ====== *)

Definition mk_ctx (D : tele) : ctx := mkCtx D [].

Inductive item_ok (Sg : gsig) (Phi : ctx) : item -> Prop :=
| OkTy : forall t, g_ty Sg t = true -> item_ok Sg Phi (ITy t)
| OkVal : forall v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> sat0 Sg Phi Dv -> item_ok Sg Phi (IVal v)
| OkFn : forall f Df S b,
    g_fn Sg f = Some (Df, S, b) -> sat0 Sg Phi Df -> item_ok Sg Phi (IFn f)
| OkMth : forall rt m Dm S th,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi rt ->
    sat Sg Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    item_ok Sg Phi (IMth rt m th).

(* The telescoping rule [D20]: each item is checked with only the items to
   its left (and the enclosing telescope) in force. *)
Inductive wf_tele (Sg : gsig) : tele -> tele -> Prop :=
| WNil : forall D0, wf_tele Sg D0 []
| WCons : forall D0 i D,
    item_ok Sg (mk_ctx D0) i ->
    wf_tele Sg (D0 ++ [i]) D ->
    wf_tele Sg D0 (i :: D).

(* A defined body's parameters become variables 0, 1, ... *)
Definition body_env (S : fnsig) : tenv :=
  combine (seq 0 (length (fs_params S))) (fs_params S).

Record wf_gsig (Sg : gsig) : Prop := {
  wf_tag_decl : forall n D taup,
      g_tag Sg n = Some (D, taup) ->
      wf_tele Sg [] D /\ wf_ty Sg (mk_ctx D) taup;
  wf_val_decl : forall v D tauv,
      g_val Sg v = Some (D, tauv) ->
      wf_tele Sg [] D /\ wf_ty Sg (mk_ctx D) tauv;
  wf_fn_decl : forall f D S b,
      g_fn Sg f = Some (D, S, b) ->
      wf_tele Sg [] D
      /\ Forall (wf_ty Sg (mk_ctx D)) (fs_params S)
      /\ wf_ty Sg (mk_ctx D) (fs_ret S)
      /\ (forall body, b = Some body ->
             has_ty Sg (mk_ctx D) (body_env S) body (fs_ret S));
  wf_mth_decl : forall m D S,
      g_mth Sg m = Some (D, S) ->
      wf_tele Sg [] D
      /\ Forall (wf_ty Sg (mk_ctx (ITy this_sym :: D))) (fs_params S)
      /\ wf_ty Sg (mk_ctx (ITy this_sym :: D)) (fs_ret S)
}.

(* ⊢ P ok: a well-formed signature whose main is defined, closed, unit-valued. *)
Definition wf_prog (Sg : gsig) (main : fsym) : Prop :=
  wf_gsig Sg /\
  exists body, g_fn Sg main = Some ([], mkSig [] TUnit, Some body).

(* ===================== 10. Dynamics ====================================== *)

(* Runtime contexts: val items carry values; fn and method items carry
   (code, captured context) pairs — the dynamic residue of [D2]. *)
Inductive dentry : Type :=
| DVal : value -> dentry
| DClo : fsym -> list (item * dentry) -> dentry.

Definition denv := list (item * dentry).
Definition venv := list (var * value).

Fixpoint lookup_denv (i : item) (d : denv) : option dentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_denv i r
  end.

Fixpoint lookup_venv (x : var) (g : venv) : option value :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_venv x r
  end.

(* δ ∘ ς: the caller's provisions, restricted and re-keyed to the callee's
   own requirement names. *)
Fixpoint compose_smap (d : denv) (sm : smap) : option denv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_denv s d, compose_smap d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint find_arm (n : nsym) (arms : list (nsym * var * expr))
  : option (var * expr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_arm n r
  end.

Definition mk_venv (ws : list value) : venv :=
  combine (seq 0 (length ws)) ws.

(* Big-step evaluation (Figure 7 of the paper). *)
Inductive eval (Sg : gsig) : denv -> venv -> expr -> value -> Prop :=
| EvUnit : forall d g,
    eval Sg d g EUnit VUnit
| EvVar : forall d g x w,
    lookup_venv x g = Some w ->
    eval Sg d g (EVar x) w
| EvVal : forall d g v w,
    lookup_denv (IVal v) d = Some (DVal w) ->
    eval Sg d g (EVl v) w
| EvTag : forall d g n th e w,
    eval Sg d g e w ->
    eval Sg d g (ETag n th e) (VTag n w)
| EvLet : forall d g x e1 e2 w1 w,
    eval Sg d g e1 w1 ->
    eval Sg d ((x, w1) :: g) e2 w ->
    eval Sg d g (ELet x e1 e2) w
| EvMatch : forall d g e0 arms n w' x eb w,
    eval Sg d g e0 (VTag n w') ->
    find_arm n arms = Some (x, eb) ->
    eval Sg d ((x, w') :: g) eb w ->
    eval Sg d g (EMatch e0 arms) w
| EvCallD : forall d g f th sm args D S body ws d' w,
    g_fn Sg f = Some (D, S, Some body) ->
    Forall2 (eval Sg d g) args ws ->
    compose_smap d sm = Some d' ->
    eval Sg d' (mk_venv ws) body w ->
    eval Sg d g (ECallD f th sm args) w
| EvCallA : forall d g f args f' df D' S' body ws w,
    lookup_denv (IFn f) d = Some (DClo f' df) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg df (mk_venv ws) body w ->
    eval Sg d g (ECallA f args) w
| EvMeth : forall d g e0 m prov args f' d' D' S' body w0 ws w,
    lookup_denv prov d = Some (DClo f' d') ->
    g_fn Sg f' = Some (D', S', Some body) ->
    eval Sg d g e0 w0 ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg d' (mk_venv (w0 :: ws)) body w ->
    eval Sg d g (EMeth e0 m prov args) w
| EvBindTy : forall d g t tau e w,
    eval Sg d g e w ->
    eval Sg d g (EBindTy t tau e) w
| EvBindVal : forall d g v e1 e w1 w,
    eval Sg d g e1 w1 ->
    eval Sg ((IVal v, DVal w1) :: d) g e w ->
    eval Sg d g (EBindVal v e1 e) w
| EvBindFn : forall d g f f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IFn f, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindFn f f' sm e) w
| EvBindMth : forall d g tauk m th f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindMth tauk m th f' sm e) w.

(* An executable twin: a fueled definitional interpreter.  It is the
   deterministic implementation of `eval`, and its Err result is the
   instrumented error of Theorem 4.2 (any missing lookup or shape mismatch). *)
Inductive res : Type :=
| Ok        : value -> res
| Err       : res
| OutOfFuel : res.

Fixpoint interp (fuel : nat) (Sg : gsig) (d : denv) (g : venv) (e : expr)
  {struct fuel} : res :=
  match fuel with
  | 0 => OutOfFuel
  | S k =>
      let interp_args := interp_list k Sg d g in
      match e with
      | EVar x => match lookup_venv x g with
                  | Some w => Ok w
                  | None => Err
                  end
      | EUnit => Ok VUnit
      | EVl v => match lookup_denv (IVal v) d with
                 | Some (DVal w) => Ok w
                 | _ => Err
                 end
      | ETag n th e1 =>
          match interp k Sg d g e1 with
          | Ok w => Ok (VTag n w)
          | rr => rr
          end
      | ECallD f th sm args =>
          match g_fn Sg f with
          | Some (_, _, Some body) =>
              match interp_args args with
              | inl ws =>
                  match compose_smap d sm with
                  | Some d' => interp k Sg d' (mk_venv ws) body
                  | None => Err
                  end
              | inr rr => rr
              end
          | _ => Err
          end
      | ECallA f args =>
          match lookup_denv (IFn f) d with
          | Some (DClo f' df) =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp_args args with
                  | inl ws => interp k Sg df (mk_venv ws) body
                  | inr rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMeth e0 m prov args =>
          match lookup_denv prov d with
          | Some (DClo f' d') =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp k Sg d g e0 with
                  | Ok w0 =>
                      match interp_args args with
                      | inl ws => interp k Sg d' (mk_venv (w0 :: ws)) body
                      | inr rr => rr
                      end
                  | rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMatch e0 arms =>
          match interp k Sg d g e0 with
          | Ok (VTag n w') =>
              match find_arm n arms with
              | Some (x, eb) => interp k Sg d ((x, w') :: g) eb
              | None => Err
              end
          | Ok VUnit => Err
          | rr => rr
          end
      | ELet x e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg d ((x, w1) :: g) e2
          | rr => rr
          end
      | EBindTy _ _ e1 => interp k Sg d g e1
      | EBindVal v e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg ((IVal v, DVal w1) :: d) g e2
          | rr => rr
          end
      | EBindFn f f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IFn f, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindMth tauk m th f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IMth tauk m th, DClo f' dc) :: d) g e1
          | None => Err
          end
      end
  end
with interp_list (fuel : nat) (Sg : gsig) (d : denv) (g : venv)
    (l : list expr) {struct fuel} : sum (list value) res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match l with
      | [] => inl []
      | e1 :: r =>
          match interp k Sg d g e1 with
          | Ok w =>
              match interp_list k Sg d g r with
              | inl ws => inl (w :: ws)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end.

(* Type erasure (Proposition 4.3): drop type binds and applications.  Items
   are runtime *keys*, not consulted types, so the applications inside IMth
   items and EBindMth stay: erasing a name would change which drawer the
   context is filed in, not what a type means. *)
Fixpoint erase (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVl v => EVl v
  | ETag n _ e1 => ETag n [] (erase e1)
  | ECallD f _ sm args =>
      ECallD f [] sm ((fix go (l : list expr) : list expr :=
                         match l with
                         | [] => []
                         | e1 :: r => erase e1 :: go r
                         end) args)
  | ECallA f args =>
      ECallA f ((fix go (l : list expr) : list expr :=
                   match l with
                   | [] => []
                   | e1 :: r => erase e1 :: go r
                   end) args)
  | EMeth e0 m prov args =>
      EMeth (erase e0) m prov
            ((fix go (l : list expr) : list expr :=
                match l with
                | [] => []
                | e1 :: r => erase e1 :: go r
                end) args)
  | EMatch e0 arms =>
      EMatch (erase e0)
             ((fix go (l : list (nsym * var * expr)) :=
                 match l with
                 | [] => []
                 | (n, x, eb) :: r => (n, x, erase eb) :: go r
                 end) arms)
  | ELet x e1 e2 => ELet x (erase e1) (erase e2)
  | EBindTy _ _ e1 => erase e1
  | EBindVal v e1 e2 => EBindVal v (erase e1) (erase e2)
  | EBindFn f f' sm e1 => EBindFn f f' sm (erase e1)
  | EBindMth tauk m th f' sm e1 => EBindMth tauk m th f' sm (erase e1)
  end.

(* ===================== 10b. Induction principles ========================= *)

(* The auto-generated induction principles are useless at the nested lists
   (arguments of calls, arms of matches) and at the Forall2 premises of the
   evaluation rules.  These two replacements are the load-bearing
   infrastructure of every proof below. *)

Fixpoint expr_ind' (P : expr -> Prop)
    (HVar : forall x, P (EVar x))
    (HUnit : P EUnit)
    (HVl : forall v, P (EVl v))
    (HTag : forall n th e, P e -> P (ETag n th e))
    (HCallD : forall f th sm args, Forall P args -> P (ECallD f th sm args))
    (HCallA : forall f args, Forall P args -> P (ECallA f args))
    (HMeth : forall e0 m prov args,
        P e0 -> Forall P args -> P (EMeth e0 m prov args))
    (HMatch : forall e0 arms,
        P e0 -> Forall (fun a => P (snd a)) arms -> P (EMatch e0 arms))
    (HLet : forall x e1 e2, P e1 -> P e2 -> P (ELet x e1 e2))
    (HBindTy : forall t tau e, P e -> P (EBindTy t tau e))
    (HBindVal : forall v e1 e2, P e1 -> P e2 -> P (EBindVal v e1 e2))
    (HBindFn : forall f f' sm e, P e -> P (EBindFn f f' sm e))
    (HBindMth : forall tauk m th f' sm e, P e -> P (EBindMth tauk m th f' sm e))
    (e : expr) {struct e} : P e :=
  let REC := expr_ind' HVar HUnit HVl HTag HCallD HCallA HMeth HMatch HLet
                       HBindTy HBindVal HBindFn HBindMth in
  match e with
  | EVar x => HVar x
  | EUnit => HUnit
  | EVl v => HVl v
  | ETag n th e1 => HTag n th e1 (REC e1)
  | ECallD f th sm args =>
      HCallD f th sm args
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | ECallA f args =>
      HCallA f args
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | EMeth e0 m prov args =>
      HMeth e0 m prov args (REC e0)
        ((fix go (l : list expr) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | e1 :: r => @Forall_cons _ P e1 r (REC e1) (go r)
            end) args)
  | EMatch e0 arms =>
      HMatch e0 arms (REC e0)
        ((fix go (l : list (nsym * var * expr)) : Forall (fun a => P (snd a)) l :=
            match l with
            | [] => Forall_nil (fun a => P (snd a))
            | a :: r => @Forall_cons _ (fun a => P (snd a)) a r (REC (snd a)) (go r)
            end) arms)
  | ELet x e1 e2 => HLet x e1 e2 (REC e1) (REC e2)
  | EBindTy t tau e1 => HBindTy t tau e1 (REC e1)
  | EBindVal v e1 e2 => HBindVal v e1 e2 (REC e1) (REC e2)
  | EBindFn f f' sm e1 => HBindFn f f' sm e1 (REC e1)
  | EBindMth tauk m th f' sm e1 => HBindMth tauk m th f' sm e1 (REC e1)
  end.

Section EvalInd.
  Variable Sg : gsig.
  Variable P : denv -> venv -> expr -> value -> Prop.

  Hypothesis HUnit : forall d g, P d g EUnit VUnit.
  Hypothesis HVar : forall d g x w,
      lookup_venv x g = Some w -> P d g (EVar x) w.
  Hypothesis HVal : forall d g v w,
      lookup_denv (IVal v) d = Some (DVal w) -> P d g (EVl v) w.
  Hypothesis HTag : forall d g n th e w,
      eval Sg d g e w -> P d g e w -> P d g (ETag n th e) (VTag n w).
  Hypothesis HLet : forall d g x e1 e2 w1 w,
      eval Sg d g e1 w1 -> P d g e1 w1 ->
      eval Sg d ((x, w1) :: g) e2 w -> P d ((x, w1) :: g) e2 w ->
      P d g (ELet x e1 e2) w.
  Hypothesis HMatch : forall d g e0 arms n w' x eb w,
      eval Sg d g e0 (VTag n w') -> P d g e0 (VTag n w') ->
      find_arm n arms = Some (x, eb) ->
      eval Sg d ((x, w') :: g) eb w -> P d ((x, w') :: g) eb w ->
      P d g (EMatch e0 arms) w.
  Hypothesis HCallD : forall d g f th sm args D S body ws d' w,
      g_fn Sg f = Some (D, S, Some body) ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      compose_smap d sm = Some d' ->
      eval Sg d' (mk_venv ws) body w -> P d' (mk_venv ws) body w ->
      P d g (ECallD f th sm args) w.
  Hypothesis HCallA : forall d g f args f' df D' S' body ws w,
      lookup_denv (IFn f) d = Some (DClo f' df) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      eval Sg df (mk_venv ws) body w -> P df (mk_venv ws) body w ->
      P d g (ECallA f args) w.
  Hypothesis HMeth : forall d g e0 m prov args f' d' D' S' body w0 ws w,
      lookup_denv prov d = Some (DClo f' d') ->
      g_fn Sg f' = Some (D', S', Some body) ->
      eval Sg d g e0 w0 -> P d g e0 w0 ->
      Forall2 (eval Sg d g) args ws -> Forall2 (P d g) args ws ->
      eval Sg d' (mk_venv (w0 :: ws)) body w -> P d' (mk_venv (w0 :: ws)) body w ->
      P d g (EMeth e0 m prov args) w.
  Hypothesis HBindTy : forall d g t tau e w,
      eval Sg d g e w -> P d g e w -> P d g (EBindTy t tau e) w.
  Hypothesis HBindVal : forall d g v e1 e w1 w,
      eval Sg d g e1 w1 -> P d g e1 w1 ->
      eval Sg ((IVal v, DVal w1) :: d) g e w ->
      P ((IVal v, DVal w1) :: d) g e w ->
      P d g (EBindVal v e1 e) w.
  Hypothesis HBindFn : forall d g f f' sm dc e w,
      compose_smap d sm = Some dc ->
      eval Sg ((IFn f, DClo f' dc) :: d) g e w ->
      P ((IFn f, DClo f' dc) :: d) g e w ->
      P d g (EBindFn f f' sm e) w.
  Hypothesis HBindMth : forall d g tauk m th f' sm dc e w,
      compose_smap d sm = Some dc ->
      eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w ->
      P ((IMth tauk m th, DClo f' dc) :: d) g e w ->
      P d g (EBindMth tauk m th f' sm e) w.

  Lemma eval_ind' : forall d g e w, eval Sg d g e w -> P d g e w.
  Proof.
    fix IH 5.
    intros d g e w D.
    destruct D.
    - apply HUnit.
    - apply HVar; assumption.
    - apply HVal; assumption.
    - apply HTag; [assumption | apply IH; assumption].
    - eapply HLet; try eassumption; apply IH; assumption.
    - eapply HMatch; try eassumption; apply IH; assumption.
    - eapply HCallD; try eassumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - eapply HCallA; try eassumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - eapply HMeth; try eassumption.
      + apply IH; assumption.
      + match goal with
        | F : Forall2 (eval _ _ _) _ _ |- Forall2 _ _ _ =>
            clear - IH F; induction F; constructor;
            [apply IH; assumption | assumption]
        end.
      + apply IH; assumption.
    - apply HBindTy; [assumption | apply IH; assumption].
    - eapply HBindVal; try eassumption; apply IH; assumption.
    - eapply HBindFn; try eassumption; apply IH; assumption.
    - eapply HBindMth; try eassumption; apply IH; assumption.
  Qed.
End EvalInd.

(* ===================== 11. Metatheory ==================================== *)

(* ---- Proposition 4.1 (resolution is search-free). ----------------------
   The context premises of the statics are the single membership test
   `avail`, a boolean over the finite set A — decidable by construction, and
   no judgment quantifies over Σ.  The uniqueness premise of T-Meth makes the
   provision annotation a function of the derivation. *)

Theorem availability_decidable :
  forall Phi i, {avail Phi i} + {~ avail Phi i}.
Proof.
  intros Phi i. unfold avail.
  destruct (avail_b Phi i).
  - left; reflexivity.
  - right; discriminate.
Defined.

Theorem method_resolution_functional :
  forall (Phi : ctx) k m i1 i2,
    In i1 (cA Phi) -> is_prov_at (cS Phi) k m i1 ->
    In i2 (cA Phi) -> is_prov_at (cS Phi) k m i2 ->
    (forall i, In i (cA Phi) -> is_prov_at (cS Phi) k m i -> i = i1) ->
    i2 = i1.
Proof. intros Phi k m i1 i2 H1 P1 H2 P2 Uniq. apply Uniq; assumption. Qed.

(* ---- Leaf lemmas for the proof effort -----------------------------------
   Every open proof below ends in `Admitted.` inside a unique
   (* BEGIN:name *) ... (* END:name *) block.  The statements are frozen:
   the integration harness rejects a fill that changes anything outside
   its block or that contains Axiom/Admitted/admit.  The comment above
   each lemma records the intended proof. *)

(* erase pushes through argument and arm lists as `map`; these unfoldings
   mean later proofs never fight an anonymous inner fix. *)

Lemma erase_ECallD : forall f th sm args,
  erase (ECallD f th sm args) = ECallD f [] sm (map erase args).
Proof. reflexivity. Qed.

Lemma erase_ECallA : forall f args,
  erase (ECallA f args) = ECallA f (map erase args).
Proof. reflexivity. Qed.

Lemma erase_EMeth : forall e0 m prov args,
  erase (EMeth e0 m prov args) = EMeth (erase e0) m prov (map erase args).
Proof. reflexivity. Qed.

Definition erase_arm (a : nsym * var * expr) : nsym * var * expr :=
  (fst (fst a), snd (fst a), erase (snd a)).

Lemma erase_EMatch : forall e0 arms,
  erase (EMatch e0 arms) = EMatch (erase e0) (map erase_arm arms).
Proof.
  intros; simpl; f_equal.
  induction arms as [| a r IHr]; simpl; [reflexivity |].
  destruct a as [[n x] eb]; simpl; unfold erase_arm; simpl; now rewrite IHr.
Qed.

(* find_arm commutes with arm erasure.
   Hint: induction on arms; destruct the head arm and the Nat.eqb test. *)
(* BEGIN:find_arm_erase *)
Lemma find_arm_erase : forall n arms,
  find_arm n (map erase_arm arms)
  = match find_arm n arms with
    | Some (x, eb) => Some (x, erase eb)
    | None => None
    end.
Proof.
  induction arms as [| [[n' x] eb] r IH]; intros; simpl.
  - reflexivity.
  - unfold erase_arm at 1; simpl.
    destruct (Nat.eqb n n'); [reflexivity | apply IH].
Qed.
(* END:find_arm_erase *)

(* A found arm is one of the arms.
   Hint: induction on arms; destruct the head and the Nat.eqb test;
   injection on the Some. *)
(* BEGIN:find_arm_in *)
Lemma find_arm_in : forall n arms x eb,
  find_arm n arms = Some (x, eb) -> In (n, x, eb) arms.
Proof.
  induction arms as [| [[n' x'] eb'] r IH]; intros x eb H.
  - simpl in H. discriminate.
  - simpl in H. destruct (Nat.eqb n n') eqn:Heq.
    + injection H as Hx Heb; subst.
      apply Nat.eqb_eq in Heq; subst.
      left. reflexivity.
    + right. apply IH. exact H.
Qed.
(* END:find_arm_in *)

(* The inr payload of interp_list is never Ok: the only inr injections wrap
   an Err or OutOfFuel that some element produced.
   Hint: induction on fuel; destruct l, then the head's interp result, then
   the tail's interp_list result; congruence closes every branch. *)
(* BEGIN:interp_list_never_ok *)
Lemma interp_list_never_ok :
  forall fuel Sg d g l w, interp_list fuel Sg d g l <> inr (Ok w).
Proof.
  induction fuel as [| k IH]; intros Sg d g l w Heq.
  - simpl in Heq. discriminate Heq.
  - simpl in Heq. destruct l as [| e1 r].
    + discriminate Heq.
    + destruct (interp k Sg d g e1) eqn:E1.
      * destruct (interp_list k Sg d g r) eqn:E2.
        -- discriminate Heq.
        -- injection Heq as Heq'. subst r0.
           apply (IH Sg d g r w E2).
      * discriminate Heq.
      * discriminate Heq.
Qed.
(* END:interp_list_never_ok *)

(* One extra unit of fuel preserves success, for both interpreters at once.
   Hint: induction on fuel (the two conjuncts need each other).  In the S
   case destruct e (resp. l); simpl in the hypothesis AND the goal;
   destruct every scrutinee with eqn:; rewrite with the induction
   hypotheses on the Ok/inl branches; on `inr rr` branches where the
   hypothesis says rr = Ok w, contradict with interp_list_never_ok. *)
(* BEGIN:interp_S_all *)
Lemma interp_S_all :
  forall fuel,
    (forall Sg d g e w, interp fuel Sg d g e = Ok w ->
                        interp (S fuel) Sg d g e = Ok w)
    /\ (forall Sg d g l ws, interp_list fuel Sg d g l = inl ws ->
                            interp_list (S fuel) Sg d g l = inl ws).
Proof.
  induction fuel as [|k [IH1 IH2]].
  - split; intros; simpl in *; discriminate.
  - split.
    + intros Sg d g e w H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 mm prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk mm th f' sm e1 ];
        simpl; subst q; simpl in H.
      * assumption.
      * assumption.
      * assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). assumption.
      * destruct (g_fn Sg f) as [[[D0 S0] ob]|] eqn:E0; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) eqn:E2.
        -- rewrite (IH2 _ _ _ _ _ E2).
           destruct (compose_smap d sm) eqn:E3; try discriminate.
           apply IH1. assumption.
        -- exfalso. rewrite H in E2.
           eapply interp_list_never_ok; exact E2.
      * destruct (lookup_denv (IFn f) d) as [[wv|fc dc]|] eqn:E0;
          try discriminate.
        destruct (g_fn Sg fc) as [[[D0 S0] ob]|] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) eqn:E2.
        -- rewrite (IH2 _ _ _ _ _ E2). apply IH1. assumption.
        -- exfalso. rewrite H in E2.
           eapply interp_list_never_ok; exact E2.
      * destruct (lookup_denv prov d) as [[wv|fc dc]|] eqn:E0;
          try discriminate.
        destruct (g_fn Sg fc) as [[[D0 S0] ob]|] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp k Sg d g e0) eqn:E2; try discriminate.
        rewrite (IH1 _ _ _ _ _ E2).
        destruct (interp_list k Sg d g args) eqn:E3.
        -- rewrite (IH2 _ _ _ _ _ E3). apply IH1. assumption.
        -- exfalso. rewrite H in E3.
           eapply interp_list_never_ok; exact E3.
      * destruct (interp k Sg d g e0) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1).
        destruct v as [|n w']; try discriminate.
        destruct (find_arm n arms) as [[x eb]|] eqn:E2; try discriminate.
        apply IH1. assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). apply IH1. assumption.
      * apply IH1. assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1). apply IH1. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH1. assumption.
      * destruct (compose_smap d sm) eqn:E1; try discriminate.
        apply IH1. assumption.
    + intros Sg d g l ws H. simpl in H.
      remember (S k) as q eqn:Eq.
      destruct l as [|e1 r]; simpl; subst q; simpl in H.
      * assumption.
      * destruct (interp k Sg d g e1) eqn:E1; try discriminate.
        rewrite (IH1 _ _ _ _ _ E1).
        destruct (interp_list k Sg d g r) eqn:E2; try discriminate.
        rewrite (IH2 _ _ _ _ _ E2). assumption.
Qed.
(* END:interp_S_all *)

(* Fuel monotonicity, from interp_S_all by induction on the ≤ derivation. *)
(* BEGIN:interp_mono *)
Lemma interp_mono :
  forall fuel fuel' Sg d g e w,
    fuel <= fuel' -> interp fuel Sg d g e = Ok w ->
    interp fuel' Sg d g e = Ok w.
Proof.
  intros fuel fuel' Sg d g e w Hle.
  induction Hle as [| fuel' Hle IH].
  - intros H; exact H.
  - intros H. apply interp_S_all. apply IH. exact H.
Qed.

Lemma interp_list_mono :
  forall fuel fuel' Sg d g l ws,
    fuel <= fuel' -> interp_list fuel Sg d g l = inl ws ->
    interp_list fuel' Sg d g l = inl ws.
Proof.
  intros fuel fuel' Sg d g l ws Hle.
  induction Hle as [| fuel' Hle IH].
  - intros H; exact H.
  - intros H. apply interp_S_all. apply IH. exact H.
Qed.
(* END:interp_mono *)

(* ---- Determinism of the big-step relation (the interpreter is its
   deterministic implementation).
   Hint: apply eval_ind' with
     P d g e w1 := forall w2, eval Sg d g e w2 -> w1 = w2;
   in each case invert the second derivation (the constructors are
   syntax-directed) and chain the induction hypotheses; equalities between
   lookups/compose_smap results close by congruence; for the argument
   lists prove an auxiliary zip: Forall2 (fun e w => forall w', eval e w'
   -> w = w') l ws1 -> Forall2 (eval) l ws2 -> ws1 = ws2. *)
(* BEGIN:eval_deterministic *)
(* Zip: pointwise determinism along an argument list determines the list. *)
Lemma eval_deterministic_aux1 :
  forall Sg d g l ws1 ws2,
    Forall2 (fun e w => forall w', eval Sg d g e w' -> w = w') l ws1 ->
    Forall2 (eval Sg d g) l ws2 ->
    ws1 = ws2.
Proof.
  intros Sg d g l ws1 ws2 H1. revert ws2.
  induction H1 as [| e w l' ws' Hh Ht IHt]; intros ws2 H2;
    inversion H2; subst.
  - reflexivity.
  - f_equal.
    + apply Hh; assumption.
    + apply IHt; assumption.
Qed.

(* Syntax-directed inversion principles for [eval], stated with explicit
   existentials so that the determinism proof never depends on names
   invented by [inversion]. *)
Lemma eval_deterministic_aux2 :
  forall Sg d g n th e w2,
    eval Sg d g (ETag n th e) w2 ->
    exists w, w2 = VTag n w /\ eval Sg d g e w.
Proof.
  intros Sg d g n th e w2 H. inversion H; subst.
  eexists. split; [ reflexivity | eassumption ].
Qed.

Lemma eval_deterministic_aux3 :
  forall Sg d g x e1 e2 w2,
    eval Sg d g (ELet x e1 e2) w2 ->
    exists w1, eval Sg d g e1 w1 /\ eval Sg d ((x, w1) :: g) e2 w2.
Proof.
  intros Sg d g x e1 e2 w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux4 :
  forall Sg d g e0 arms w2,
    eval Sg d g (EMatch e0 arms) w2 ->
    exists n w' x eb,
      eval Sg d g e0 (VTag n w') /\
      find_arm n arms = Some (x, eb) /\
      eval Sg d ((x, w') :: g) eb w2.
Proof.
  intros Sg d g e0 arms w2 H. inversion H; subst.
  do 4 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux5 :
  forall Sg d g f th sm args w2,
    eval Sg d g (ECallD f th sm args) w2 ->
    exists D S body ws d',
      g_fn Sg f = Some (D, S, Some body) /\
      Forall2 (eval Sg d g) args ws /\
      compose_smap d sm = Some d' /\
      eval Sg d' (mk_venv ws) body w2.
Proof.
  intros Sg d g f th sm args w2 H. inversion H; subst.
  do 5 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux6 :
  forall Sg d g f args w2,
    eval Sg d g (ECallA f args) w2 ->
    exists f' df D' S' body ws,
      lookup_denv (IFn f) d = Some (DClo f' df) /\
      g_fn Sg f' = Some (D', S', Some body) /\
      Forall2 (eval Sg d g) args ws /\
      eval Sg df (mk_venv ws) body w2.
Proof.
  intros Sg d g f args w2 H. inversion H; subst.
  do 6 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux7 :
  forall Sg d g e0 m prov args w2,
    eval Sg d g (EMeth e0 m prov args) w2 ->
    exists f' d' D' S' body w0 ws,
      lookup_denv prov d = Some (DClo f' d') /\
      g_fn Sg f' = Some (D', S', Some body) /\
      eval Sg d g e0 w0 /\
      Forall2 (eval Sg d g) args ws /\
      eval Sg d' (mk_venv (w0 :: ws)) body w2.
Proof.
  intros Sg d g e0 m prov args w2 H. inversion H; subst.
  do 7 eexists. repeat split; eassumption.
Qed.

Lemma eval_deterministic_aux8 :
  forall Sg d g t tau e w2,
    eval Sg d g (EBindTy t tau e) w2 -> eval Sg d g e w2.
Proof.
  intros Sg d g t tau e w2 H. inversion H; subst. assumption.
Qed.

Lemma eval_deterministic_aux9 :
  forall Sg d g v e1 e w2,
    eval Sg d g (EBindVal v e1 e) w2 ->
    exists w1, eval Sg d g e1 w1 /\ eval Sg ((IVal v, DVal w1) :: d) g e w2.
Proof.
  intros Sg d g v e1 e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux10 :
  forall Sg d g f f' sm e w2,
    eval Sg d g (EBindFn f f' sm e) w2 ->
    exists dc, compose_smap d sm = Some dc /\
               eval Sg ((IFn f, DClo f' dc) :: d) g e w2.
Proof.
  intros Sg d g f f' sm e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Lemma eval_deterministic_aux11 :
  forall Sg d g tauk m th f' sm e w2,
    eval Sg d g (EBindMth tauk m th f' sm e) w2 ->
    exists dc, compose_smap d sm = Some dc /\
               eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w2.
Proof.
  intros Sg d g tauk m th f' sm e w2 H. inversion H; subst.
  eexists. split; eassumption.
Qed.

Theorem eval_deterministic :
  forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
Proof.
  intros Sg.
  assert (Hmain : forall d g e w1,
             eval Sg d g e w1 -> forall w2, eval Sg d g e w2 -> w1 = w2).
  { apply (@eval_ind' Sg
             (fun d g e w1 => forall w2, eval Sg d g e w2 -> w1 = w2)).
    - (* EUnit *)
      intros d g w2 H2. inversion H2; subst. reflexivity.
    - (* EVar *)
      intros d g x w Hl w2 H2. inversion H2; subst. congruence.
    - (* EVl *)
      intros d g v w Hl w2 H2. inversion H2; subst. congruence.
    - (* ETag *)
      intros d g n th e w Hev IH w2 H2.
      apply eval_deterministic_aux2 in H2.
      destruct H2 as [u [Hu Hev2]]. subst w2.
      f_equal. apply IH. exact Hev2.
    - (* ELet *)
      intros d g x e1 e2 w1 w Hev1 IH1 Hev2 IH2 w2 H2.
      apply eval_deterministic_aux3 in H2.
      destruct H2 as [u [Ha Hb]].
      specialize (IH1 _ Ha). subst u.
      apply IH2. exact Hb.
    - (* EMatch *)
      intros d g e0 arms n w' x eb w Hev0 IH0 Hfa Hevb IHb w2 H2.
      apply eval_deterministic_aux4 in H2.
      destruct H2 as [n0 [u [x0 [eb0 [Ha [Hb Hc]]]]]].
      specialize (IH0 _ Ha).
      assert (En : n0 = n) by congruence.
      assert (Eu : u = w') by congruence.
      subst n0 u.
      assert (Ex : x0 = x) by congruence.
      assert (Eb : eb0 = eb) by congruence.
      subst x0 eb0.
      apply IHb. exact Hc.
    - (* ECallD *)
      intros d g f th sm args D S body ws d' w Hfn HF HFP Hcs Hev IH w2 H2.
      apply eval_deterministic_aux5 in H2.
      destruct H2 as [D0 [S0 [body0 [ws0 [d0 [Ha [Hb [Hc Hd]]]]]]]].
      assert (Ebody : body0 = body) by congruence.
      assert (Ed : d0 = d') by congruence.
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 d0 ws0.
      apply IH. exact Hd.
    - (* ECallA *)
      intros d g f args f' df D' S' body ws w Hld Hfn HF HFP Hev IH w2 H2.
      apply eval_deterministic_aux6 in H2.
      destruct H2 as [f0 [df0 [D0 [S0 [body0 [ws0 [Ha [Hb [Hc Hd]]]]]]]]].
      assert (Ef : f0 = f') by congruence. subst f0.
      assert (Edf : df0 = df) by congruence. subst df0.
      assert (Ebody : body0 = body) by congruence.
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 ws0.
      apply IH. exact Hd.
    - (* EMeth *)
      intros d g e0 m prov args f' d' D' S' body w0 ws w
             Hld Hfn Hev0 IH0 HF HFP Hev IH w2 H2.
      apply eval_deterministic_aux7 in H2.
      destruct H2 as [f0 [d0 [D0 [S0 [body0 [u0 [ws0 [Ha [Hb [Hc [Hd He]]]]]]]]]]].
      assert (Ef : f0 = f') by congruence. subst f0.
      assert (Ed : d0 = d') by congruence. subst d0.
      assert (Ebody : body0 = body) by congruence.
      specialize (IH0 _ Hc).
      assert (Ews : ws0 = ws) by
        (symmetry; eapply eval_deterministic_aux1; eassumption).
      subst body0 ws0 u0.
      apply IH. exact He.
    - (* EBindTy *)
      intros d g t tau e w Hev IH w2 H2.
      apply eval_deterministic_aux8 in H2.
      apply IH. exact H2.
    - (* EBindVal *)
      intros d g v e1 e w1 w Hev1 IH1 Hev IH w2 H2.
      apply eval_deterministic_aux9 in H2.
      destruct H2 as [u [Ha Hb]].
      specialize (IH1 _ Ha). subst u.
      apply IH. exact Hb.
    - (* EBindFn *)
      intros d g f f' sm dc e w Hcs Hev IH w2 H2.
      apply eval_deterministic_aux10 in H2.
      destruct H2 as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb.
    - (* EBindMth *)
      intros d g tauk m th f' sm dc e w Hcs Hev IH w2 H2.
      apply eval_deterministic_aux11 in H2.
      destruct H2 as [dc0 [Ha Hb]].
      assert (E : dc0 = dc) by congruence. subst dc0.
      apply IH. exact Hb. }
  intros d g e w1 w2 H1 H2. exact (Hmain d g e w1 H1 w2 H2).
Qed.
(* END:eval_deterministic *)

(* ---- Adequacy of the interpreter for the relation. ----------------------
   Hint (soundness): induction on fuel, both conjuncts at once; fuel 0
   discriminates; in the S case destruct e (resp. l), destruct every
   scrutinee with eqn:, feed the equations to the induction hypotheses,
   and finish with the evaluation constructors (econstructor; eauto). *)
(* BEGIN:interp_sound_all *)
Lemma interp_sound_all :
  forall fuel,
    (forall Sg d g e w, interp fuel Sg d g e = Ok w -> eval Sg d g e w)
    /\ (forall Sg d g l ws, interp_list fuel Sg d g l = inl ws ->
                            Forall2 (eval Sg d g) l ws).
Proof.
  induction fuel as [| k IHk].
  - split; intros Sg d g e w H; simpl in H; discriminate.
  - destruct IHk as [IH1 IH2]. split.
    + intros Sg d g e w H.
      destruct e as [ x | | v | n th e1 | f th sm args | f args
                    | e0 m prov args | e0 arms | x e1 e2 | t tau e1
                    | v e1 e2 | f f' sm e1 | tauk m th f' sm e1 ];
        simpl in H.
      * destruct (lookup_venv x g) as [w'|] eqn:E1; try discriminate.
        injection H as H; subst. constructor; assumption.
      * injection H as H; subst. constructor.
      * destruct (lookup_denv (IVal v) d) as [en|] eqn:E1; try discriminate.
        destruct en as [w' | fc dc]; try discriminate.
        injection H as H; subst. constructor; assumption.
      * destruct (interp k Sg d g e1) as [w' | | ] eqn:E1; try discriminate.
        injection H as H; subst. apply IH1 in E1. constructor; assumption.
      * destruct (g_fn Sg f) as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E2.
        -- destruct (compose_smap d sm) as [d'|] eqn:E3; try discriminate.
           apply IH2 in E2. apply IH1 in H.
           eapply EvCallD; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E2.
      * destruct (lookup_denv (IFn f) d) as [en|] eqn:E0; try discriminate.
        destruct en as [w' | f' df]; try discriminate.
        destruct (g_fn Sg f') as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E2.
        -- apply IH2 in E2. apply IH1 in H.
           eapply EvCallA; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E2.
      * destruct (lookup_denv prov d) as [en|] eqn:E0; try discriminate.
        destruct en as [w' | f' d']; try discriminate.
        destruct (g_fn Sg f') as [[[D S] ob] | ] eqn:E1; try discriminate.
        destruct ob as [body|]; try discriminate.
        destruct (interp k Sg d g e0) as [w0 | | ] eqn:E2; try discriminate.
        destruct (interp_list k Sg d g args) as [ws | rr] eqn:E3.
        -- apply IH1 in E2. apply IH2 in E3. apply IH1 in H.
           eapply EvMeth; eassumption.
        -- subst rr. exfalso. eapply interp_list_never_ok. exact E3.
      * destruct (interp k Sg d g e0) as [w' | | ] eqn:E1; try discriminate.
        destruct w' as [ | n w'']; try discriminate.
        destruct (find_arm n arms) as [[xa eb] | ] eqn:E2; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvMatch; eassumption.
      * destruct (interp k Sg d g e1) as [w1 | | ] eqn:E1; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvLet; eassumption.
      * apply IH1 in H. constructor; assumption.
      * destruct (interp k Sg d g e1) as [w1 | | ] eqn:E1; try discriminate.
        apply IH1 in E1. apply IH1 in H.
        eapply EvBindVal; eassumption.
      * destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
        apply IH1 in H. eapply EvBindFn; eassumption.
      * destruct (compose_smap d sm) as [dc|] eqn:E1; try discriminate.
        apply IH1 in H. eapply EvBindMth; eassumption.
    + intros Sg d g l ws H.
      destruct l as [| e1 r]; simpl in H.
      * injection H as H; subst. constructor.
      * destruct (interp k Sg d g e1) as [w | | ] eqn:E1; try discriminate.
        destruct (interp_list k Sg d g r) as [ws' | rr] eqn:E2;
          try discriminate.
        injection H as H; subst. apply IH1 in E1. apply IH2 in E2.
        constructor; assumption.
Qed.
(* END:interp_sound_all *)

Theorem interp_sound :
  forall fuel Sg d g e w,
    interp fuel Sg d g e = Ok w -> eval Sg d g e w.
Proof. intro fuel. apply (proj1 (interp_sound_all fuel)). Qed.

(* Pointwise interpreter success lifts to the list interpreter.
   Hint: induction on the Forall2; base case: fuel 1; cons case: take
   S (max f1 f2) and settle both sides with interp_mono/interp_list_mono
   (Nat.le_max_l / Nat.le_max_r). *)
(* BEGIN:interp_list_complete *)
Lemma interp_list_complete :
  forall Sg d g l ws,
    Forall2 (fun e w => exists fuel, interp fuel Sg d g e = Ok w) l ws ->
    exists fuel, interp_list fuel Sg d g l = inl ws.
Proof.
  intros Sg d g l ws H.
  induction H as [| e1 w1 r rws He Hr IH].
  - exists 1. simpl. reflexivity.
  - destruct He as [f1 Hf1]. destruct IH as [f2 Hf2].
    assert (Ha : interp (Nat.max f1 f2) Sg d g e1 = Ok w1).
    { eapply interp_mono; [ apply Nat.le_max_l | exact Hf1 ]. }
    assert (Hb : interp_list (Nat.max f1 f2) Sg d g r = inl rws).
    { eapply interp_list_mono; [ apply Nat.le_max_r | exact Hf2 ]. }
    exists (S (Nat.max f1 f2)). simpl.
    rewrite Ha, Hb. reflexivity.
Qed.
(* END:interp_list_complete *)

(* Hint (completeness): apply eval_ind' with
     P d g e w := exists fuel, interp fuel Sg d g e = Ok w;
   each case combines the sub-derivations' fuels with max and interp_mono,
   uses interp_list_complete for argument lists, and picks S (that fuel);
   the interpreter's scrutinees are then all determined by the case's
   premises, so the goal computes. *)
(* BEGIN:interp_complete *)
Lemma interp_complete_aux1 : forall Sg d g e w a b,
  interp a Sg d g e = Ok w -> interp (Nat.max a b) Sg d g e = Ok w.
Proof.
  intros Sg d g e w a b H.
  eapply interp_mono; [apply Nat.le_max_l | exact H].
Qed.

Lemma interp_complete_aux2 : forall Sg d g e w a b,
  interp b Sg d g e = Ok w -> interp (Nat.max a b) Sg d g e = Ok w.
Proof.
  intros Sg d g e w a b H.
  eapply interp_mono; [apply Nat.le_max_r | exact H].
Qed.

Lemma interp_complete_aux3 : forall Sg d g l ws a b,
  interp_list a Sg d g l = inl ws -> interp_list (Nat.max a b) Sg d g l = inl ws.
Proof.
  intros Sg d g l ws a b H.
  eapply interp_list_mono; [apply Nat.le_max_l | exact H].
Qed.

Lemma interp_complete_aux4 : forall Sg d g l ws a b,
  interp_list b Sg d g l = inl ws -> interp_list (Nat.max a b) Sg d g l = inl ws.
Proof.
  intros Sg d g l ws a b H.
  eapply interp_list_mono; [apply Nat.le_max_r | exact H].
Qed.

Theorem interp_complete :
  forall Sg d g e w,
    eval Sg d g e w -> exists fuel, interp fuel Sg d g e = Ok w.
Proof.
  intros Sg.
  apply (@eval_ind' Sg
           (fun d g e w => exists fuel, interp fuel Sg d g e = Ok w)).
  - (* EvUnit *) intros d g. exists 1. reflexivity.
  - (* EvVar *) intros d g x w Hl. exists 1. simpl. rewrite Hl. reflexivity.
  - (* EvVal *) intros d g v w Hl. exists 1. simpl. rewrite Hl. reflexivity.
  - (* EvTag *) intros d g n th e w _ [a Ha]. exists (S a). simpl.
    rewrite Ha. reflexivity.
  - (* EvLet *) intros d g x e1 e2 w1 w _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e1 = Ok w1)
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d ((x, w1) :: g) e2 = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. exact Hb'.
  - (* EvMatch *) intros d g e0 arms n w' x eb w _ [a Ha] Hfind _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e0 = Ok (VTag n w'))
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d ((x, w') :: g) eb = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. rewrite Hfind. exact Hb'.
  - (* EvCallD *)
    intros d g f th sm args D Sf body ws d' w Hfn _ HF Hcs _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_list (Nat.max a b) Sg d g args = inl ws)
      by (apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg d' (mk_venv ws) body = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hfn. rewrite Ha'. rewrite Hcs. exact Hb'.
  - (* EvCallA *)
    intros d g f args f' df D' S' body ws w Hlk Hfn _ HF _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max a b)).
    assert (Ha' : interp_list (Nat.max a b) Sg d g args = inl ws)
      by (apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg df (mk_venv ws) body = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hlk. rewrite Hfn. rewrite Ha'. exact Hb'.
  - (* EvMeth *)
    intros d g e0 m prov args f' d' D' S' body w0 ws w Hlk Hfn _ [c Hc]
           _ HF _ [b Hb].
    apply interp_list_complete in HF. destruct HF as [a Ha].
    exists (S (Nat.max c (Nat.max a b))).
    assert (Hc' : interp (Nat.max c (Nat.max a b)) Sg d g e0 = Ok w0)
      by (apply interp_complete_aux1; exact Hc).
    assert (Ha' : interp_list (Nat.max c (Nat.max a b)) Sg d g args = inl ws)
      by (apply interp_complete_aux4; apply interp_complete_aux3; exact Ha).
    assert (Hb' : interp (Nat.max c (Nat.max a b)) Sg d'
                         (mk_venv (w0 :: ws)) body = Ok w)
      by (apply interp_complete_aux2; apply interp_complete_aux2; exact Hb).
    simpl. rewrite Hlk. rewrite Hfn. rewrite Hc'. rewrite Ha'. exact Hb'.
  - (* EvBindTy *) intros d g t tau e w _ [a Ha]. exists (S a). simpl. exact Ha.
  - (* EvBindVal *) intros d g v e1 e w1 w _ [a Ha] _ [b Hb].
    exists (S (Nat.max a b)).
    assert (Ha' : interp (Nat.max a b) Sg d g e1 = Ok w1)
      by (apply interp_complete_aux1; exact Ha).
    assert (Hb' : interp (Nat.max a b) Sg ((IVal v, DVal w1) :: d) g e = Ok w)
      by (apply interp_complete_aux2; exact Hb).
    simpl. rewrite Ha'. exact Hb'.
  - (* EvBindFn *) intros d g f f' sm dc e w Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
  - (* EvBindMth *) intros d g tauk m th f' sm dc e w Hcs _ [a Ha].
    exists (S a). simpl. rewrite Hcs. exact Ha.
Qed.
(* END:interp_complete *)

(* ===================== 11b. The semantic soundness development =========== *)

(* --- Free symbols and closed types. -------------------------------------- *)

Fixpoint ty_frees (tau : ty) : list tsym :=
  match tau with
  | TUnit => []
  | TAbs t => [t]
  | TNom _ th =>
      (fix go (l : list (tsym * ty)) : list tsym :=
         match l with [] => [] | p :: r => ty_frees (snd p) ++ go r end) th
  | TUnion ms =>
      (fix go (l : list ty) : list tsym :=
         match l with [] => [] | t' :: r => ty_frees t' ++ go r end) ms
  end.

Definition closed_ty (tau : ty) : Prop := ty_frees tau = [].

(* app_subst unfolds to map on the nested lists (definitionally). *)
Lemma app_subst_nom : forall s n th,
  app_subst s (TNom n th) = TNom n (map_snd (app_subst s) th).
Proof. reflexivity. Qed.

Lemma app_subst_union : forall s ms,
  app_subst s (TUnion ms) = TUnion (map (app_subst s) ms).
Proof. reflexivity. Qed.

(* --- Value typing. --------------------------------------------------------- *)

Inductive vty (Sg : gsig) : value -> ty -> Prop :=
| VtyUnit : vty Sg VUnit TUnit
| VtyTag : forall n D taup th w,
    g_tag Sg n = Some (D, taup) ->
    vty Sg w (app_subst th taup) ->
    vty Sg (VTag n w) (TNom n th)
| VtyInj : forall w tau' ms,
    vty Sg w tau' -> In tau' ms -> vty Sg w (TUnion ms).

(* --- Groundings. ---------------------------------------------------------- *)

(* A grounding maps abstract type symbols to closed types; it is the runtime
   instantiation of the abstract symbols a region was checked under. *)
Definition grounding (g : subst) : Prop :=
  Forall (fun p => closed_ty (snd p)) g.

(* g absorbs s: reading through s and then g is just reading through g. *)
Definition absorbs (g s : subst) : Prop :=
  forall t tau, lookup_subst t s = Some tau ->
                lookup_subst t g = Some (app_subst g tau).

Definition covers (g : subst) (ts : list tsym) : Prop :=
  forall t, In t ts -> exists tau, lookup_subst t g = Some tau.

Definition ctx_grounded (g : subst) (Phi : ctx) : Prop :=
  grounding g /\ absorbs g (cS Phi) /\ covers g (tyreqs (cA Phi)).

(* --- Environment agreement. ----------------------------------------------- *)

Definition tele_covered (d : denv) (D : tele) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_denv i d = Some en) D.

(* Entry-level agreement, by structural recursion on the entry.  The
   existential g' is the closure's own grounding: the ground instantiation
   of the provider's telescope, captured at its bind site. *)
Fixpoint dentry_agree (Sg : gsig) (g : subst) (i : item) (en : dentry)
    {struct en} : Prop :=
  match i, en with
  | IVal v, DVal w =>
      match g_val Sg v with
      | Some (_, tauv) => vty Sg w (app_subst g tauv)
      | None => False
      end
  | IFn f, DClo f' d' =>
      match g_fn Sg f, g_fn Sg f' with
      | Some (_, Sf, None), Some (D', S', Some _) =>
          exists g',
            grounding g' /\ covers g' (tyreqs D') /\
            map (app_subst g') (fs_params S') = map (app_subst g) (fs_params Sf) /\
            app_subst g' (fs_ret S') = app_subst g (fs_ret Sf) /\
            tele_covered d' D' /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) d'
      | _, _ => False
      end
  | IMth rt m th, DClo f' d' =>
      match g_mth Sg m, g_fn Sg f' with
      | Some (_, Sm), Some (D', S', Some _) =>
          exists g',
            grounding g' /\ covers g' (tyreqs D') /\
            map (app_subst g') (fs_params S')
              = app_subst g rt
                :: map (fun tau =>
                          app_subst g (app_subst th (app_subst (this_subst rt) tau)))
                       (fs_params Sm) /\
            app_subst g' (fs_ret S')
              = app_subst g (app_subst th (app_subst (this_subst rt) (fs_ret Sm))) /\
            tele_covered d' D' /\
            (fix all (dd : denv) : Prop :=
               match dd with
               | [] => True
               | p :: r => dentry_agree Sg g' (fst p) (snd p) /\ all r
               end) d'
      | _, _ => False
      end
  | _, _ => False
  end.

Definition denv_agree (Sg : gsig) (g : subst) (A : list item) (d : denv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_denv i d = Some en) A
  /\ Forall (fun p => dentry_agree Sg g (fst p) (snd p)) d.

Definition venv_agree (Sg : gsig) (g : subst) (G : tenv) (ge : venv) : Prop :=
  forall x tau, lookup_tenv x G = Some tau ->
    exists w, lookup_venv x ge = Some w /\ vty Sg w (app_subst g tau).

(* --- The statement. -------------------------------------------------------- *)

Definition safe_res (Sg : gsig) (gt : ty) (r : res) : Prop :=
  match r with
  | Ok w => vty Sg w gt
  | Err => False
  | OutOfFuel => True
  end.

Definition SAFE (Sg : gsig) (fuel : nat) : Prop :=
  forall Phi G e tau d ge g,
    has_ty Sg Phi G e tau ->
    ctx_grounded g Phi ->
    denv_agree Sg g (cA Phi) d ->
    venv_agree Sg g G ge ->
    safe_res Sg (app_subst g (app_subst (cS Phi) tau)) (interp fuel Sg d ge e).

(* --- Substitution algebra (leaves). ---------------------------------------- *)

(* Boolean equalities reflect equality.
   Hint: ty_eqb_eq by ty_ind' with inner list inductions and Nat.eqb_eq /
   andb_true_iff; then subst_eqb_eq and item_eqb_eq are inductions/case
   analyses over it.  Both directions of the iff are needed. *)
(* BEGIN:eqb_eq *)
Lemma ty_eqb_eq : forall a b, ty_eqb a b = true <-> a = b.
Proof.
Admitted. (* FILL:eqb_eq *)

Lemma subst_eqb_eq : forall a b, subst_eqb a b = true <-> a = b.
Proof.
Admitted. (* FILL:eqb_eq *)

Lemma item_eqb_eq : forall a b, item_eqb a b = true <-> a = b.
Proof.
Admitted. (* FILL:eqb_eq *)
(* END:eqb_eq *)

(* A successful denv lookup returns one of the pairs, with the key equal to
   the query.  Hint: induction on d; item_eqb_eq. *)
(* BEGIN:lookup_denv_in *)
Lemma lookup_denv_in : forall i d en,
  lookup_denv i d = Some en -> In (i, en) d.
Proof.
Admitted. (* FILL:lookup_denv_in *)
(* END:lookup_denv_in *)

(* Looking a symbol up under a mapped substitution.
   Hint: induction on th. *)
(* BEGIN:subst_chain *)
Lemma lookup_map_snd : forall (f : ty -> ty) t th,
  lookup_subst t (map_snd f th)
  = match lookup_subst t th with
    | Some tau => Some (f tau)
    | None => None
    end.
Proof.
Admitted. (* FILL:subst_chain *)

(* Substituting after a total application is applying the mapped
   application: the composition law monomorphization rests on.
   Hint: ty_ind'; the TAbs case is lookup_map_snd plus the coverage
   hypothesis; In/incl over ty_frees with in_or_app for the nested cases. *)
Lemma app_subst_chain : forall s th tau,
  incl (ty_frees tau) (map fst th) ->
  app_subst s (app_subst th tau) = app_subst (map_snd (app_subst s) th) tau.
Proof.
Admitted. (* FILL:subst_chain *)
(* END:subst_chain *)

(* Reading through an absorbed substitution changes nothing.
   Hint: ty_ind'; the TAbs case splits on lookup_subst t s. *)
(* BEGIN:absorbs_all *)
Lemma absorbs_all : forall g s,
  absorbs g s ->
  forall tau, app_subst g (app_subst s tau) = app_subst g tau.
Proof.
Admitted. (* FILL:absorbs_all *)
(* END:absorbs_all *)

(* Closed types are fixed by substitution; grounding a covered type closes
   it.  Hint: ty_ind'; app_eq_nil facts about ty_frees of the nested lists;
   for the second lemma also Forall_forall over the grounding. *)
(* BEGIN:closed_subst *)
Lemma app_subst_closed : forall s tau,
  closed_ty tau -> app_subst s tau = tau.
Proof.
Admitted. (* FILL:closed_subst *)

Lemma closed_app_subst_ground : forall g tau,
  grounding g ->
  incl (ty_frees tau) (map fst g) ->
  closed_ty (app_subst g tau).
Proof.
Admitted. (* FILL:closed_subst *)
(* END:closed_subst *)

(* Substitutions that agree on a type's free symbols read it equally.
   Hint: ty_ind'; in_or_app in the nested cases. *)
(* BEGIN:frees_agree *)
Lemma app_subst_frees_agree : forall g1 g2 tau,
  (forall t, In t (ty_frees tau) ->
             lookup_subst t g1 = lookup_subst t g2) ->
  app_subst g1 tau = app_subst g2 tau.
Proof.
Admitted. (* FILL:frees_agree *)
(* END:frees_agree *)

(* --- Value-typing inversions and subtyping. -------------------------------- *)

(* Hint: inversion; VtyInj cannot produce TUnit or TNom, and a union member
   reached by VtyInj is in the list. *)
(* BEGIN:vty_inv *)
Lemma vty_unit_inv : forall Sg w, vty Sg w TUnit -> w = VUnit.
Proof.
Admitted. (* FILL:vty_inv *)

Lemma vty_union_inv : forall Sg w ms,
  vty Sg w (TUnion ms) -> exists tau', In tau' ms /\ vty Sg w tau'.
Proof.
Admitted. (* FILL:vty_inv *)

Lemma vty_nom_inv : forall Sg w n th,
  vty Sg w (TNom n th) ->
  exists D taup w', w = VTag n w' /\ g_tag Sg n = Some (D, taup)
                    /\ vty Sg w' (app_subst th taup).
Proof.
Admitted. (* FILL:vty_inv *)
(* END:vty_inv *)

(* Subtyping preserves value typing under any grounding of the context.
   Hint: case on the subtyping derivation; SubEq rewrites (ty_eq is an
   equation between σ images, apply f_equal with app_subst g); SubInj maps
   the member equality through app_subst_union/in_map and re-injects. *)
(* BEGIN:vty_subty *)
Lemma vty_subty : forall Sg Phi g tau' tau w,
  subty Phi tau' tau ->
  vty Sg w (app_subst g (app_subst (cS Phi) tau')) ->
  vty Sg w (app_subst g (app_subst (cS Phi) tau)).
Proof.
Admitted. (* FILL:vty_subty *)
(* END:vty_subty *)

(* --- Availability gives syntactic membership for symbol-only items. -------
   Hint: avail is an existsb; app_subst_item preserves the constructor, so
   the witness must be the same symbol-only item; item_eqb_eq. *)
(* BEGIN:avail_in *)
Lemma avail_ty_in : forall Phi t, avail Phi (ITy t) -> In (ITy t) (cA Phi).
Proof.
Admitted. (* FILL:avail_in *)

Lemma avail_val_in : forall Phi v, avail Phi (IVal v) -> In (IVal v) (cA Phi).
Proof.
Admitted. (* FILL:avail_in *)

Lemma avail_fn_in : forall Phi f, avail Phi (IFn f) -> In (IFn f) (cA Phi).
Proof.
Admitted. (* FILL:avail_in *)
(* END:avail_in *)

(* --- Variable environments. ------------------------------------------------ *)

(* Hint: venv_agree_cons is a case split on Nat.eqb; venv_agree_body goes by
   induction on the Forall2 after generalizing the seq start index. *)
(* BEGIN:venv_agree_lemmas *)
Lemma venv_agree_cons : forall Sg g G ge x tau w,
  venv_agree Sg g G ge ->
  vty Sg w (app_subst g tau) ->
  venv_agree Sg g ((x, tau) :: G) ((x, w) :: ge).
Proof.
Admitted. (* FILL:venv_agree_lemmas *)

Lemma venv_agree_body : forall Sg g ws taus,
  Forall2 (fun w tau => vty Sg w (app_subst g tau)) ws taus ->
  venv_agree Sg g (combine (seq 0 (length taus)) taus) (mk_venv ws).
Proof.
Admitted. (* FILL:venv_agree_lemmas *)
(* END:venv_agree_lemmas *)

(* --- Regularity: well-formed types only mention in-force symbols. ---------- *)

(* Hint: mirror eval_ind': a fix on the wf_ty derivation whose K-Tag/K-Union
   cases traverse the Forall premises with an inner induction. *)
(* BEGIN:wf_ty_ind *)
Lemma wf_ty_ind' : forall (Sg : gsig) (Phi : ctx) (P : ty -> Prop),
  P TUnit ->
  (forall t, avail Phi (ITy t) -> P (TAbs t)) ->
  (forall n D tau0 th,
      g_tag Sg n = Some (D, tau0) ->
      sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      Forall (fun p => P (snd p)) th ->
      P (TNom n th)) ->
  (forall ms,
      Forall (wf_ty Sg Phi) ms -> Forall P ms ->
      Forall is_nom ms ->
      NoDup (heads_of (cS Phi) ms) ->
      P (TUnion ms)) ->
  forall tau, wf_ty Sg Phi tau -> P tau.
Proof.
Admitted. (* FILL:wf_ty_ind *)
(* END:wf_ty_ind *)

(* Hint: wf_ty_ind'; the TAbs case is avail_ty_in; tyreqs of a tele is a
   flat_map, so In facts transfer by in_flat_map. *)
(* BEGIN:wf_ty_frees *)
Lemma wf_ty_frees : forall Sg Phi tau,
  wf_ty Sg Phi tau -> incl (ty_frees tau) (tyreqs (cA Phi)).
Proof.
Admitted. (* FILL:wf_ty_frees *)
(* END:wf_ty_frees *)

(* Telescopes are dependency-closed: a non-type item's own type requirements
   appear among the telescope's type items (the paper's [D20]).
   Hint: induction on the wf_tele derivation, generalizing the prefix; the
   item_ok premise's sat0 makes each dependency available, hence
   (avail_ty_in) in the prefix-plus-current telescope; the id_app domain
   equation of sat0 places it. *)
(* BEGIN:tele_deps_closed *)
Lemma tele_deps_closed : forall Sg D0 D,
  wf_tele Sg D0 D ->
  forall i, In i D -> is_ty_item i = false ->
  incl (dep_tys Sg i) (tyreqs (D0 ++ D)).
Proof.
Admitted. (* FILL:tele_deps_closed *)
(* END:tele_deps_closed *)

(* --- compose_smap: success and lookups. ------------------------------------ *)

(* Hint: inductions on sm; for compose_smap_first also item_eqb_eq to align
   lookup_smap's first match with the constructed environment's. *)
(* BEGIN:compose_smap_lemmas *)
Lemma compose_smap_defined : forall d sm,
  Forall (fun p => exists en, lookup_denv (snd p) d = Some en) sm ->
  exists d', compose_smap d sm = Some d'.
Proof.
Admitted. (* FILL:compose_smap_lemmas *)

Lemma compose_smap_keys : forall d sm d',
  compose_smap d sm = Some d' -> map fst d' = map fst sm.
Proof.
Admitted. (* FILL:compose_smap_lemmas *)

Lemma compose_smap_entry : forall d sm d' p,
  compose_smap d sm = Some d' -> In p d' ->
  exists sat', In (fst p, sat') sm /\ lookup_denv sat' d = Some (snd p).
Proof.
Admitted. (* FILL:compose_smap_lemmas *)

Lemma compose_smap_first : forall d sm d' req sat',
  compose_smap d sm = Some d' ->
  lookup_smap req sm = Some sat' ->
  exists en, lookup_denv sat' d = Some en /\ lookup_denv req d' = Some en.
Proof.
Admitted. (* FILL:compose_smap_lemmas *)
(* END:compose_smap_lemmas *)

(* --- Transferring an entry to the callee's point of view. ------------------ *)

(* An entry that agrees at item i1 under g1 also agrees at item i2 under g2,
   provided the two groundings read the two items identically: same
   symbol-only skeleton, equal method receiver/application images, and equal
   readings of the type symbols a val/fn item depends on.
   Hint: case analysis on i1 and en (dentry_agree unfolds one level only;
   the nested closure conjuncts mention the inner grounding g' and carry
   over untouched); the item-image equality forces i2's shape (injectivity
   of the constructors); for IVal/IFn rewrite the signature readings with
   app_subst_frees_agree via the dep_tys premise (dep_tys bounds exactly
   the frees of the stored signature — take that bound as a given from the
   telescoped well-formedness, or note the readings only ever apply g to
   tauv/S whose frees are within dep_tys by wf_gsig; if a side condition
   is genuinely missing here, report it in notes rather than forcing it). *)
(* BEGIN:dentry_agree_retype *)
Lemma dentry_agree_retype : forall Sg g1 i1 g2 i2 en,
  dentry_agree Sg g1 i1 en ->
  app_subst_item g1 i1 = app_subst_item g2 i2 ->
  (forall t, In t (dep_tys Sg i1) ->
             app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  dentry_agree Sg g2 i2 en.
Proof.
Admitted. (* FILL:dentry_agree_retype *)
(* END:dentry_agree_retype *)

(* --- The callee's grounding. ------------------------------------------------ *)

(* Each callee type requirement, read through the application, the context,
   and the caller's grounding. *)
Definition callee_grounding (g : subst) (Phi : ctx) (th : subst) (D : tele)
    : subst :=
  map (fun t => (t, app_subst g (app_subst (cS Phi) (app_subst th (TAbs t)))))
      (tyreqs D).

(* Reading a D-covered type through the callee grounding is reading its
   applied image through the caller's.
   Hint: the lookup lemma is an induction on tyreqs D; the reads lemma is
   ty_ind' with the TAbs case closed by the lookup lemma. *)
(* BEGIN:callee_grounding_reads *)
Lemma callee_grounding_lookup : forall g Phi th D t,
  In t (tyreqs D) ->
  lookup_subst t (callee_grounding g Phi th D)
  = Some (app_subst g (app_subst (cS Phi) (app_subst th (TAbs t)))).
Proof.
Admitted. (* FILL:callee_grounding_reads *)

Lemma callee_grounding_reads : forall g Phi th D tau,
  incl (ty_frees tau) (tyreqs D) ->
  app_subst (callee_grounding g Phi th D) tau
  = app_subst g (app_subst (cS Phi) (app_subst th tau)).
Proof.
Admitted. (* FILL:callee_grounding_reads *)
(* END:callee_grounding_reads *)

(* --- The crux: satisfying a telescope hands the callee an agreeing world. --
   Hint: this is where everything above meets.  compose_smap_defined fires
   because smap_ok's satisfiers are available items whose entries the
   caller's coverage provides (satisfiers of non-type requirements are
   non-type items: app_subst_item preserves constructors and item_matches
   equates images).  Coverage of D under d' comes from smap_ok's domain
   equation plus compose_smap_keys/first.  Entry agreement at the callee's
   items: compose_smap_entry fetches the caller entry; the caller's
   denv_agree gives dentry_agree at the satisfier via lookup_denv_in;
   dentry_agree_retype moves it to the requirement item under the callee
   grounding — its item-image premise is item_matches pushed under g
   (absorbs_all + callee_grounding_reads with frees bounded by wf
   regularity), and its dep premise is sat's coherence conjunct pushed
   under g the same way (tele_deps_closed bounds the dependencies within
   tyreqs D).  ctx_grounded of the callee: grounding by
   closed_app_subst_ground (θ's ranges are wf, so their σ-then-g readings
   are closed via absorbs_all + wf_ty_frees + the caller's coverage);
   absorbs is vacuous (mk_ctx has empty σ); covers is
   callee_grounding_lookup.  If a premise is genuinely missing, report it
   precisely in notes instead of forcing the proof. *)
(* BEGIN:callee_env_agree *)
Lemma callee_env_agree : forall Sg g Phi th sm D d,
  wf_gsig Sg ->
  wf_tele Sg [] D ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  ctx_grounded g Phi ->
  denv_agree Sg g (cA Phi) d ->
  exists d', compose_smap d sm = Some d'
    /\ ctx_grounded (callee_grounding g Phi th D) (mk_ctx D)
    /\ denv_agree Sg (callee_grounding g Phi th D) D d'.
Proof.
Admitted. (* FILL:callee_env_agree *)
(* END:callee_env_agree *)

(* --- A lean induction principle for typing: only T-Sub re-types the same
   term, so only it needs an induction hypothesis. ------------------------- *)

Section HasTyIndSub.
  Variable Sg : gsig.
  Variable P : ctx -> tenv -> expr -> ty -> Prop.

  Hypothesis HVar : forall Phi G x tau,
      lookup_tenv x G = Some tau -> P Phi G (EVar x) tau.
  Hypothesis HUnit : forall Phi G, P Phi G EUnit TUnit.
  Hypothesis HSub : forall Phi G e tau' tau,
      has_ty Sg Phi G e tau' -> P Phi G e tau' -> subty Phi tau' tau ->
      P Phi G e tau.
  Hypothesis HVal : forall Phi G v Dv tauv,
      g_val Sg v = Some (Dv, tauv) -> avail Phi (IVal v) ->
      P Phi G (EVl v) (app_subst (cS Phi) tauv).
  Hypothesis HTag : forall Phi G n D tau0 th e,
      g_tag Sg n = Some (D, tau0) -> sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
      P Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th)).
  Hypothesis HNeed : forall Phi G f Df S args,
      g_fn Sg f = Some (Df, S, None) -> avail Phi (IFn f) ->
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
              args (fs_params S) ->
      P Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S)).
  Hypothesis HCall : forall Phi G f Df S body th sm args,
      g_fn Sg f = Some (Df, S, Some body) ->
      sat Sg Phi th Df ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Df ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
              args (fs_params S) ->
      P Phi G (ECallD f th sm args)
        (app_subst (cS Phi) (app_subst th (fs_ret S))).
  Hypothesis HMeth : forall Phi G e0 tau0 m Dm S rt0 th0 args,
      g_mth Sg m = Some (Dm, S) ->
      has_ty Sg Phi G e0 tau0 ->
      In (IMth rt0 m th0) (cA Phi) ->
      app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
      (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                  i1 = IMth rt0 m th0) ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
              args (fs_params S) ->
      P Phi G (EMeth e0 m (IMth rt0 m th0) args)
        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S)).
  Hypothesis HMatch : forall Phi G e0 tau0 ms arms tau,
      has_ty Sg Phi G e0 tau0 ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      NoDup (map member_head ms) ->
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 (fun mem arm =>
                 has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                        (arm_body arm) tau) ms arms ->
      P Phi G (EMatch e0 arms) tau.
  Hypothesis HLet : forall Phi G x e1 e2 tau1 tau,
      has_ty Sg Phi G e1 tau1 ->
      has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
      P Phi G (ELet x e1 e2) tau.
  Hypothesis HBindTy : forall Phi G t tau' e tau,
      g_ty Sg t = true ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      has_ty Sg (bind_ty t tau' Phi) G e tau ->
      P Phi G (EBindTy t tau' e) tau.
  Hypothesis HBindVal : forall Phi G v Dv tauv e1 e tau,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
      has_ty Sg (add_item (IVal v) Phi) G e tau ->
      P Phi G (EBindVal v e1 e) tau.
  Hypothesis HBindFn : forall Phi G f Df S f' D' S' body sm e tau,
      g_fn Sg f = Some (Df, S, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' S ->
      has_ty Sg (add_item (IFn f) Phi) G e tau ->
      P Phi G (EBindFn f f' sm e) tau.
  Hypothesis HBindMth : forall Phi G tauk m Dm S th f' D' S' body sm
                               tau0' rest e tau,
      g_mth Sg m = Some (Dm, S) ->
      wf_ty Sg Phi tauk ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
      has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
      P Phi G (EBindMth tauk m th f' sm e) tau.

  Lemma has_ty_ind_sub : forall Phi G e tau,
      has_ty Sg Phi G e tau -> P Phi G e tau.
  Proof.
    fix IH 5.
    intros Phi G e tau D.
    destruct D.
    - apply HVar; assumption.
    - apply HUnit.
    - eapply HSub; [eassumption | apply IH; assumption | assumption].
    - eapply HVal; eassumption.
    - eapply HTag; eassumption.
    - eapply HNeed; eassumption.
    - eapply HCall; eassumption.
    - eapply HMeth; eassumption.
    - eapply HMatch; eassumption.
    - eapply HLet; eassumption.
    - eapply HBindTy; eassumption.
    - eapply HBindVal; eassumption.
    - eapply HBindFn; eassumption.
    - eapply HBindMth; eassumption.
  Qed.
End HasTyIndSub.

(* --- The case lemmas: one per typing rule, at fuel S k, under the strong
   fuel induction hypothesis. ----------------------------------------------- *)

Section SafetyCases.
  Variable Sg : gsig.
  Hypothesis WF : wf_gsig Sg.
  Variable k : nat.
  Hypothesis IHfuel : forall j, j <= k -> SAFE Sg j.

  (* Argument lists evaluate safely: interp_list at any fuel ≤ k either
     runs dry, or yields values at the grounded types — never Err.
     Hint: induction on the Forall2 with the fuel generalized; the head
     uses IHfuel, the tail the inner induction at smaller fuel (destruct j
     first: interp_list 0 is inr OutOfFuel). *)
  (* BEGIN:safety_args *)
  Lemma safety_args : forall j, j <= k ->
    forall Phi G d ge g args taus,
      Forall2 (fun e tau => has_ty Sg Phi G e tau) args taus ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      match interp_list j Sg d ge args with
      | inl ws => Forall2 (fun w tau =>
                    vty Sg w (app_subst g (app_subst (cS Phi) tau))) ws taus
      | inr Err => False
      | inr _ => True
      end.
  Proof.
  Admitted. (* FILL:safety_args *)
  (* END:safety_args *)

  (* Hint: interp (S k) on EVar reduces to the venv lookup; venv_agree
     provides the value and its typing at g tau; the conclusion's type is
     g (σ tau), so rewrite with absorbs_all (ctx_grounded's second
     component)... wait: venv_agree stores g tau where tau is the stored
     type, and the goal reads g (σ tau) — absorbs_all equates them. *)
  (* BEGIN:safety_case_var *)
  Lemma safety_case_var : forall Phi G x tau d ge g,
      lookup_tenv x G = Some tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EVar x)).
  Proof.
  Admitted. (* FILL:safety_case_var *)
  (* END:safety_case_var *)

  (* BEGIN:safety_case_unit *)
  Lemma safety_case_unit : forall Phi G d ge g,
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) TUnit))
               (interp (S k) Sg d ge EUnit).
  Proof.
  Admitted. (* FILL:safety_case_unit *)
  (* END:safety_case_unit *)

  (* Hint: the second hypothesis is the structural induction hypothesis —
     the safety of the same term at the smaller type; vty_subty converts
     its verdict on the Ok branch (destruct the interp result). *)
  (* BEGIN:safety_case_sub *)
  Lemma safety_case_sub : forall Phi G e tau' tau d ge g,
      has_ty Sg Phi G e tau' ->
      (forall d0 ge0 g0,
          ctx_grounded g0 Phi ->
          denv_agree Sg g0 (cA Phi) d0 ->
          venv_agree Sg g0 G ge0 ->
          safe_res Sg (app_subst g0 (app_subst (cS Phi) tau'))
                   (interp (S k) Sg d0 ge0 e)) ->
      subty Phi tau' tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge e).
  Proof.
  Admitted. (* FILL:safety_case_sub *)
  (* END:safety_case_sub *)

  (* Hint: avail_val_in + denv coverage finds the entry; Forall over the
     environment (via lookup_denv_in) gives its dentry_agree, which forces
     the DVal shape and types the value at g tauv; absorbs_all collapses
     the two σ applications in the conclusion. *)
  (* BEGIN:safety_case_val *)
  Lemma safety_case_val : forall Phi G v Dv tauv d ge g,
      g_val Sg v = Some (Dv, tauv) ->
      avail Phi (IVal v) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) tauv)))
               (interp (S k) Sg d ge (EVl v)).
  Proof.
  Admitted. (* FILL:safety_case_val *)
  (* END:safety_case_val *)

  (* Hint: IHfuel on the payload typing; the conclusion's tag type unfolds
     by app_subst_nom; VtyTag wants the payload at
     app_subst (map_snd (g∘σ) th) tau0, which app_subst_chain provides —
     its coverage side condition is wf_gsig's payload regularity
     (wf_ty_frees at mk_ctx D, whose tyreqs equal sat's domain equation) —
     plus absorbs_all for the σσ collapse. *)
  (* BEGIN:safety_case_tag *)
  Lemma safety_case_tag : forall Phi G n D tau0 th e d ge g,
      g_tag Sg n = Some (D, tau0) ->
      sat Sg Phi th D ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) (TNom n th))))
               (interp (S k) Sg d ge (ETag n th e)).
  Proof.
  Admitted. (* FILL:safety_case_tag *)
  (* END:safety_case_tag *)

  (* Hint: avail_fn_in + coverage + dentry_agree give the closure and its
     signature equations at the closure's own grounding g'; safety_args
     evaluates the arguments at the caller's g; the equations transport
     the argument values to g' readings of fs_params S' (rewrite along the
     map equation, Forall2 + map juggling), venv_agree_body builds the
     body's variable environment, wf_gsig types the provider's body under
     its telescope, so IHfuel at fuel k finishes; the result comes back at
     g' (fs_ret S') = g (fs_ret S), and absorbs_all collapses the σs. *)
  (* BEGIN:safety_case_need *)
  Lemma safety_case_need : forall Phi G f Df Sf args d ge g,
      g_fn Sg f = Some (Df, Sf, None) ->
      avail Phi (IFn f) ->
      Forall2 (fun e tau => has_ty Sg Phi G e (app_subst (cS Phi) tau))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi) (fs_ret Sf))))
               (interp (S k) Sg d ge (ECallA f args)).
  Proof.
  Admitted. (* FILL:safety_case_need *)
  (* END:safety_case_need *)

  (* Hint: callee_env_agree (wf_gsig provides wf_tele for Df) builds the
     callee world and makes compose_smap succeed; safety_args evaluates
     the arguments; callee_grounding_reads aligns argument and result
     types (frees bounded via wf_ty_frees from wf_gsig's signature
     regularity at mk_ctx Df, whose tyreqs match sat's domain equation);
     wf_gsig types the body under mk_ctx Df; venv_agree_body; IHfuel at k
     lands the result, and callee_grounding_reads plus absorbs_all
     translate it back to the caller's reading. *)
  (* BEGIN:safety_case_call *)
  Lemma safety_case_call : forall Phi G f Df Sf body th sm args d ge g,
      g_fn Sg f = Some (Df, Sf, Some body) ->
      sat Sg Phi th Df ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      smap_ok Phi th sm Df ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                                  (app_subst (cS Phi)
                                     (app_subst th (fs_ret Sf)))))
               (interp (S k) Sg d ge (ECallD f th sm args)).
  Proof.
  Admitted. (* FILL:safety_case_call *)
  (* END:safety_case_call *)

  (* Hint: the provision item is in A, so coverage yields its closure and
     (via lookup_denv_in + the environment Forall) its dentry_agree, whose
     signature equations mention g rt0 and g∘th0∘[This↦rt0] readings; the
     receiver equality σ rt0 = σ tau0 with absorbs_all rewrites those into
     the σ tau0 forms of the goal and argument typings (push g inside the
     This-substitution with app_subst_chain over th0 extended at this_sym,
     or pointwise via app_subst_frees_agree); the structural hypothesis
     evaluates the receiver at fuel k? no — the receiver is evaluated at
     fuel k by the interpreter, so use IHfuel on its typing; then
     safety_args, venv_agree_body with the receiver value consed on, and
     IHfuel at k on the provider's body. *)
  (* BEGIN:safety_case_meth *)
  Lemma safety_case_meth : forall Phi G e0 tau0 m Dm Sf rt0 th0 args d ge g,
      g_mth Sg m = Some (Dm, Sf) ->
      has_ty Sg Phi G e0 tau0 ->
      In (IMth rt0 m th0) (cA Phi) ->
      app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
      (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
                  i1 = IMth rt0 m th0) ->
      Forall2 (fun e tau =>
                 has_ty Sg Phi G e
                        (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
              args (fs_params Sf) ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi)
                     (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0)
                               (fs_ret Sf))))
               (interp (S k) Sg d ge (EMeth e0 m (IMth rt0 m th0) args)).
  Proof.
  Admitted. (* FILL:safety_case_meth *)
  (* END:safety_case_meth *)

  (* Hint: IHfuel on the scrutinee's typing (it runs at fuel k) yields a
     value at g (σ tau0); members of that grounded type are the g-images
     of ms (destruct σ tau0 via the members equation, app_subst_union /
     app_subst_nom); vty_union_inv / vty_nom_inv pick the member the value
     was injected at; member_head is stable under app_subst, so NoDup of
     the heads makes find_arm select the arm aligned (Forall2) with that
     member; the binder's payload type transports by app_subst_chain (tag
     payload regularity from wf_gsig + the member's sat domain equation —
     note the member's application th_j is already a σ image, absorbed by
     g); venv_agree_cons extends the environment; IHfuel at k on the arm
     body finishes.  This is the most intricate case; report precisely in
     notes if a fact is missing. *)
  (* BEGIN:safety_case_match *)
  Lemma safety_case_match : forall Phi G e0 tau0 ms arms tau d ge g,
      has_ty Sg Phi G e0 tau0 ->
      members (app_subst (cS Phi) tau0) = Some ms ->
      NoDup (map member_head ms) ->
      Forall2 (arm_matches Sg) ms arms ->
      Forall2 (fun mem arm =>
                 has_ty Sg Phi ((arm_var arm, member_payload Sg mem) :: G)
                        (arm_body arm) tau) ms arms ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EMatch e0 arms)).
  Proof.
  Admitted. (* FILL:safety_case_match *)
  (* END:safety_case_match *)

  (* BEGIN:safety_case_let *)
  Lemma safety_case_let : forall Phi G x e1 e2 tau1 tau d ge g,
      has_ty Sg Phi G e1 tau1 ->
      has_ty Sg Phi ((x, tau1) :: G) e2 tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (ELet x e1 e2)).
  Proof.
  Admitted. (* FILL:safety_case_let *)
  (* END:safety_case_let *)

  (* Hint: ground the extended context by g2 := (t, g (σ τ')) :: g.
     Closedness of the new range: wf_ty_frees bounds τ''s frees within
     A's type items, the caller's coverage grounds them (absorbs_all
     collapses the σ), closed_app_subst_ground closes.  absorbs for the
     extended σ: the new entry is definitional; old entries avoid t —
     freshness means no A-item mentions t... careful: absorbs is about
     lookups, and the extension changes lookups of t only, so old
     obligations transfer whenever their types do not mention t — their
     frees sit in A's type items by regularity of what σ stores; if that
     bound is not derivable from ctx_grounded alone, strengthen your
     in-block helper to thread it (the caller can also just pick g with
     dom g = A's type items so that t is genuinely fresh for g;
     app_subst_frees_agree then transfers every reading).  denv/venv
     agreement transports by the same frees argument.  IHfuel at k on the
     continuation under the extended context. *)
  (* BEGIN:safety_case_bindty *)
  Lemma safety_case_bindty : forall Phi G t tau' e tau d ge g,
      g_ty Sg t = true ->
      ~ In (ITy t) (cA Phi) ->
      wf_ty Sg Phi tau' ->
      has_ty Sg (bind_ty t tau' Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindTy t tau' e)).
  Proof.
  Admitted. (* FILL:safety_case_bindty *)
  (* END:safety_case_bindty *)

  (* Hint: IHfuel evaluates e1 to a value at g (σ tauv); the extended denv
     ((IVal v, DVal w) :: d) agrees with the extended A — the new entry's
     dentry_agree is that vty with absorbs_all collapsing σ; coverage of
     IVal v is the head lookup (item_eqb reflexivity via item_eqb_eq);
     IHfuel at k on the continuation. *)
  (* BEGIN:safety_case_bindval *)
  Lemma safety_case_bindval : forall Phi G v Dv tauv e1 e tau d ge g,
      g_val Sg v = Some (Dv, tauv) ->
      sat0 Sg Phi Dv ->
      has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
      has_ty Sg (add_item (IVal v) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindVal v e1 e)).
  Proof.
  Admitted. (* FILL:safety_case_bindval *)
  (* END:safety_case_bindval *)

  (* Hint: callee_env_agree at D' (sat0 is sat at id_app) builds the
     captured environment dc and the provider's grounding; the new entry
     (IFn f, DClo f' dc) agrees because sig_eq pushed under g via
     absorbs_all and callee_grounding_reads (wf_gsig regularity bounds the
     signatures' frees) yields exactly dentry_agree's equations; extend A
     and d, IHfuel at k on the continuation. *)
  (* BEGIN:safety_case_bindfn *)
  Lemma safety_case_bindfn : forall Phi G f Df Sf f' D' S' body sm e tau d ge g,
      g_fn Sg f = Some (Df, Sf, None) ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi Df ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      sig_eq (cS Phi) S' Sf ->
      has_ty Sg (add_item (IFn f) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindFn f f' sm e)).
  Proof.
  Admitted. (* FILL:safety_case_bindfn *)
  (* END:safety_case_bindfn *)

  (* Hint: as bindfn, but the new entry is a method provision: its
     dentry_agree equations are the rule's receiver/argument/result
     equations pushed under g (the first parameter via ty_eq and
     absorbs_all, the rest via the map equation and app_subst_chain /
     app_subst_frees_agree over the This-extended application). *)
  (* BEGIN:safety_case_bindmth *)
  Lemma safety_case_bindmth : forall Phi G tauk m Dm Sf th f' D' S' body sm
                                     tau0' rest e tau d ge g,
      g_mth Sg m = Some (Dm, Sf) ->
      wf_ty Sg Phi tauk ->
      sat Sg Phi th Dm ->
      Forall (fun p => wf_ty Sg Phi (snd p)) th ->
      g_fn Sg f' = Some (D', S', Some body) ->
      sat0 Sg Phi D' ->
      smap_ok Phi (id_app D') sm D' ->
      fs_params S' = tau0' :: rest ->
      ty_eq (cS Phi) tau0' tauk ->
      map (app_subst (cS Phi)) rest
        = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params Sf) ->
      app_subst (cS Phi) (fs_ret S')
        = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret Sf) ->
      has_ty Sg (add_item (IMth tauk m th) Phi) G e tau ->
      ctx_grounded g Phi ->
      denv_agree Sg g (cA Phi) d ->
      venv_agree Sg g G ge ->
      safe_res Sg (app_subst g (app_subst (cS Phi) tau))
               (interp (S k) Sg d ge (EBindMth tauk m th f' sm e)).
  Proof.
  Admitted. (* FILL:safety_case_bindmth *)
  (* END:safety_case_bindmth *)

  (* Assembly of one fuel step: the inner induction on the typing
     derivation (has_ty_ind_sub) dispatches each constructor to its case
     lemma.  Hint: unfold SAFE; intros; revert the environments; apply
     has_ty_ind_sub with the motive
       P Phi G e tau := forall d ge g, ctx_grounded g Phi ->
         denv_agree Sg g (cA Phi) d -> venv_agree Sg g G ge ->
         safe_res Sg (app_subst g (app_subst (cS Phi) tau))
                  (interp (S k) Sg d ge e);
     every case is exactly a safety_case_* lemma. *)
  (* BEGIN:safety_step *)
  Lemma safety_step : SAFE Sg (S k).
  Proof.
  Admitted. (* FILL:safety_step *)
  (* END:safety_step *)

End SafetyCases.

(* Hint: strong induction on fuel — e.g. `induction fuel using lt_wf_ind`
   or a helper `forall n, (forall m, m < n -> SAFE Sg m) -> SAFE Sg n`;
   fuel 0 is OutOfFuel, hence trivially safe (interp 0 computes); the
   successor case is safety_step with IHfuel j Hj := the strong hypothesis
   at j (mind the off-by-one: j <= k is j < S k). *)
(* BEGIN:safety *)
Theorem safety : forall Sg, wf_gsig Sg -> forall fuel, SAFE Sg fuel.
Proof.
Admitted. (* FILL:safety *)
(* END:safety *)

(* ---- Theorem 4.2 (soundness), at the program level. ---------------------
   Stated for main, where the context in force and both environments are
   empty, so no grounding of abstract type symbols is needed.  The
   generalized invariant of the paper (δ ⊨ Φ) requires each closure to carry
   its own ground instantiation of the abstract types it was captured under;
   spelling that out is the first real step of the proof effort, and we
   deliberately do not bake a weaker (false) approximation in here. *)

Definition value_has_unit_ty (w : value) : Prop := w = VUnit.

Theorem soundness_main :
  forall Sg main body fuel,
    wf_prog Sg main ->
    g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err
    /\ (forall w, interp fuel Sg [] [] body = Ok w -> value_has_unit_ty w).
Proof.
  intros Sg main body fuel [WF _] Hmain.
  pose proof WF as WF0. destruct WF0 as [Wtag Wval Wfn Wmth].
  destruct (Wfn _ _ _ _ Hmain) as (_ & _ & _ & Hb).
  specialize (Hb body eq_refl). simpl in Hb.
  pose proof (safety WF fuel) as HS. unfold SAFE in HS.
  specialize (HS (mk_ctx []) [] body TUnit [] [] [] Hb).
  simpl in HS.
  assert (Hg : ctx_grounded [] (mk_ctx [])).
  { split; [constructor | split].
    - intros t tau Hl; discriminate Hl.
    - intros t Hin; simpl in Hin; contradiction. }
  assert (Hd : denv_agree Sg [] [] []) by (split; constructor).
  assert (Hv : venv_agree Sg [] [] []).
  { intros x tau Hl; discriminate Hl. }
  specialize (HS Hg Hd Hv).
  split.
  - intros HE. rewrite HE in HS. exact HS.
  - intros w Hw. rewrite Hw in HS.
    apply vty_unit_inv in HS. exact HS.
Qed.

(* ---- Proposition 4.3 (phase separation). --------------------------------
   Evaluation never inspects a type: it is invariant under erasing every
   application θ and every type bind.

   Both directions go by structural induction on e (expr_ind'), with
   inversion on the evaluation derivation: bodies fetched from Σ are not
   erased, so no induction on evaluation is needed.
   Hints: rewrite with erase_ECallD/ECallA/EMeth/EMatch before inverting
   (backward) or before rebuilding (forward); find_arm_erase aligns the
   selected arms and find_arm_in + Forall_forall fetches the structural
   induction hypothesis for the selected arm's body; for argument lists,
   zip the Forall IH with the Forall2 evaluation premise by induction on
   the Forall2. *)
(* BEGIN:phase_separation_fwd *)
(* Zipping the structural induction hypothesis for an argument list with the
   Forall2 evaluation premise of a call rule. *)
Lemma phase_separation_fwd_aux1 :
  forall Sg d g args ws,
    Forall (fun e0 => forall d0 g0 w0,
                eval Sg d0 g0 e0 w0 -> eval Sg d0 g0 (erase e0) w0) args ->
    Forall2 (eval Sg d g) args ws ->
    Forall2 (eval Sg d g) (map erase args) ws.
Proof.
  intros Sg d g args ws HF H2.
  induction H2 as [| e1 w1 r ws' He H2' IH]; simpl.
  - constructor.
  - constructor.
    + apply (Forall_inv HF). exact He.
    + apply IH. exact (Forall_inv_tail HF).
Qed.

Lemma phase_separation_fwd :
  forall Sg e d g w, eval Sg d g e w -> eval Sg d g (erase e) w.
Proof.
  intros Sg e.
  apply (@expr_ind'
           (fun e0 => forall d g w, eval Sg d g e0 w -> eval Sg d g (erase e0) w)).
  - (* EVar *) intros x d g w Hev. exact Hev.
  - (* EUnit *) intros d g w Hev. exact Hev.
  - (* EVl *) intros v d g w Hev. exact Hev.
  - (* ETag *) intros n th e1 IH d g w Hev.
    inversion Hev; subst. simpl. apply EvTag. apply IH. assumption.
  - (* ECallD *) intros f th sm args HF d g w Hev.
    rewrite erase_ECallD. inversion Hev; subst.
    eapply EvCallD; try eassumption.
    apply phase_separation_fwd_aux1; assumption.
  - (* ECallA *) intros f args HF d g w Hev.
    rewrite erase_ECallA. inversion Hev; subst.
    eapply EvCallA; try eassumption.
    apply phase_separation_fwd_aux1; assumption.
  - (* EMeth *) intros e0 m prov args IH0 HF d g w Hev.
    rewrite erase_EMeth. inversion Hev; subst.
    eapply EvMeth; try eassumption.
    + apply IH0. eassumption.
    + apply phase_separation_fwd_aux1; assumption.
  - (* EMatch *) intros e0 arms IH0 HF d g w Hev.
    rewrite erase_EMatch. inversion Hev; subst.
    eapply EvMatch.
    + apply IH0. eassumption.
    + rewrite find_arm_erase.
      match goal with
      | H : find_arm _ arms = Some _ |- _ => rewrite H
      end.
      reflexivity.
    + match goal with
      | H : find_arm ?n arms = Some (?x, ?eb) |- _ =>
          assert (Hin : In (n, x, eb) arms) by (apply find_arm_in; exact H)
      end.
      rewrite Forall_forall in HF. specialize (HF _ Hin). simpl in HF.
      apply HF. assumption.
  - (* ELet *) intros x e1 e2 IH1 IH2 d g w Hev.
    inversion Hev; subst. simpl. eapply EvLet.
    + apply IH1. eassumption.
    + apply IH2. eassumption.
  - (* EBindTy *) intros t tau e1 IH d g w Hev.
    inversion Hev; subst. apply IH. assumption.
  - (* EBindVal *) intros v e1 e2 IH1 IH2 d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindVal.
    + apply IH1. eassumption.
    + apply IH2. eassumption.
  - (* EBindFn *) intros f f' sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindFn.
    + eassumption.
    + apply IH. eassumption.
  - (* EBindMth *) intros tauk m th f' sm e1 IH d g w Hev.
    inversion Hev; subst. simpl. eapply EvBindMth.
    + eassumption.
    + apply IH. eassumption.
Qed.
(* END:phase_separation_fwd *)

(* BEGIN:phase_separation_bwd *)
Lemma phase_separation_bwd_aux1 :
  forall Sg args,
    Forall (fun e => forall d g w, eval Sg d g (erase e) w -> eval Sg d g e w)
           args ->
    forall d g ws,
      Forall2 (eval Sg d g) (map erase args) ws ->
      Forall2 (eval Sg d g) args ws.
Proof.
  intros Sg args HF.
  induction HF as [| e r He Hr IH]; intros d g ws H2; simpl in H2.
  - inversion H2. constructor.
  - inversion H2; subst. constructor.
    + apply He; assumption.
    + apply IH; assumption.
Qed.

Lemma phase_separation_bwd_aux2 :
  forall Sg (arms : list (nsym * var * expr)),
    Forall (fun a => forall d g w,
                eval Sg d g (erase (snd a)) w -> eval Sg d g (snd a) w) arms ->
    forall n x eb, In (n, x, eb) arms ->
      forall d g w, eval Sg d g (erase eb) w -> eval Sg d g eb w.
Proof.
  intros Sg arms HF.
  induction HF as [| a r Ha Hr IH]; intros n x eb Hin d g w Hev.
  - inversion Hin.
  - destruct Hin as [Heq | Hin].
    + subst a. simpl in Ha. apply Ha; assumption.
    + eapply IH; eassumption.
Qed.

Lemma phase_separation_bwd :
  forall Sg e d g w, eval Sg d g (erase e) w -> eval Sg d g e w.
Proof.
  intros Sg e. revert e.
  apply (@expr_ind'
           (fun e => forall d g w, eval Sg d g (erase e) w -> eval Sg d g e w)).
  - intros x d g w H. exact H.
  - intros d g w H. exact H.
  - intros v d g w H. exact H.
  - intros n th e1 IH d g w H. simpl in H. inversion H; subst.
    constructor. apply IH; assumption.
  - intros f th sm args IHargs d g w H.
    rewrite erase_ECallD in H. inversion H; subst.
    econstructor; try eassumption.
    eapply phase_separation_bwd_aux1; eassumption.
  - intros f args IHargs d g w H.
    rewrite erase_ECallA in H. inversion H; subst.
    econstructor; try eassumption.
    eapply phase_separation_bwd_aux1; eassumption.
  - intros e0 m prov args IH0 IHargs d g w H.
    rewrite erase_EMeth in H. inversion H; subst.
    econstructor; try eassumption.
    + apply IH0; eassumption.
    + eapply phase_separation_bwd_aux1; eassumption.
  - intros e0 arms IH0 IHarms d g w H.
    rewrite erase_EMatch in H. inversion H; subst.
    match goal with
    | Hfa : find_arm ?nn (map erase_arm arms) = Some (?xx, ?ee) |- _ =>
        rewrite find_arm_erase in Hfa;
        destruct (find_arm nn arms) as [[x0 eb0] | ] eqn:E;
        [ injection Hfa as Hx Heb; subst | discriminate ]
    end.
    eapply EvMatch.
    + apply IH0; eassumption.
    + eassumption.
    + eapply phase_separation_bwd_aux2;
        [ eassumption | eapply find_arm_in; eassumption | eassumption ].
  - intros x e1 e2 IH1 IH2 d g w H. simpl in H. inversion H; subst.
    econstructor.
    + apply IH1; eassumption.
    + apply IH2; eassumption.
  - intros t tau e1 IH d g w H. simpl in H.
    constructor. apply IH; assumption.
  - intros v e1 e2 IH1 IH2 d g w H. simpl in H. inversion H; subst.
    econstructor.
    + apply IH1; eassumption.
    + apply IH2; eassumption.
  - intros f f' sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
  - intros tauk m th f' sm e1 IH d g w H. simpl in H. inversion H; subst.
    econstructor; [ eassumption | apply IH; eassumption ].
Qed.
(* END:phase_separation_bwd *)

Theorem phase_separation :
  forall Sg d g e w,
    eval Sg d g e w <-> eval Sg d g (erase e) w.
Proof.
  intros; split; [apply phase_separation_fwd | apply phase_separation_bwd].
Qed.

(* ===================== 12. The paper's example, executed ================= *)

(* The program of Figure 1, elaborated into the core by hand.  Running it —
   and running its erasure — validates the dynamic semantics by computation,
   and is the mechanized counterpart of the example-level reading of
   Proposition 4.3. *)

Module Example.

  (* Symbols.  (this_sym = 0 is reserved.) *)
  Definition sElem : tsym := 1.
  Definition nEmpty : nsym := 1.
  Definition nFull : nsym := 2.
  Definition nSlot : nsym := 3.
  Definition nO : nsym := 4.
  Definition vZero : vsym := 1.
  Definition fCoalesce : fsym := 1.
  Definition fDemo : fsym := 2.
  Definition fMain : fsym := 3.
  Definition mOrZero : msym := 1.

  (* [Elem = Elem]: the identity application on Δ = Elem. *)
  Definition th_id : subst := [(sElem, TAbs sElem)].

  Definition slot_payload : ty :=
    TUnion [TNom nEmpty []; TNom nFull th_id].
  Definition slot_ty : ty := TNom nSlot th_id.

  (* fn coalesce(this: Slot): Elem { match this { Slot u =>
       match u { Empty => zero, Full e => e } } }  — requires {Elem, zero} *)
  Definition coalesce_body : expr :=
    EMatch (EVar 0)
      [(nSlot, 1,
         EMatch (EVar 1)
           [(nEmpty, 2, EVl vZero);
            (nFull, 3, EVar 3)])].

  (* The provision item Slot.or_zero[Elem = Elem], keyed at the receiver
     type Slot[Elem = Elem]. *)
  Definition prov : item := IMth slot_ty mOrZero th_id.

  (* fn demo(s: Slot): Elem { s.or_zero() } — requires {Elem, zero, prov} *)
  Definition demo_body : expr := EMeth (EVar 0) mOrZero prov [].
  Definition demo_tele : tele := [ITy sElem; IVal vZero; prov].

  (* fn main() { bind Elem = O; bind zero = O;
       bind Slot.or_zero[Elem=Elem] = coalesce;
       let _ = demo(Slot (Full (O))); () } *)
  Definition main_body : expr :=
    EBindTy sElem (TNom nO [])
    (EBindVal vZero (ETag nO [] EUnit)
    (EBindMth slot_ty mOrZero th_id fCoalesce
        [(IVal vZero, IVal vZero)]
    (ELet 5
       (ECallD fDemo th_id
          [(IVal vZero, IVal vZero); (prov, prov)]
          [ETag nSlot th_id (ETag nFull th_id (ETag nO [] EUnit))])
       EUnit))).

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sElem || Nat.eqb t this_sym)
    (fun n =>
       if Nat.eqb n nEmpty then Some ([], TUnit)
       else if Nat.eqb n nFull then Some ([ITy sElem], TAbs sElem)
       else if Nat.eqb n nSlot then Some ([ITy sElem], slot_payload)
       else if Nat.eqb n nO then Some ([], TUnit)
       else None)
    (fun v =>
       if Nat.eqb v vZero then Some ([ITy sElem], TAbs sElem) else None)
    (fun f =>
       if Nat.eqb f fCoalesce then
         Some ([ITy sElem; IVal vZero],
               mkSig [slot_ty] (TAbs sElem), Some coalesce_body)
       else if Nat.eqb f fDemo then
         Some (demo_tele, mkSig [slot_ty] (TAbs sElem), Some demo_body)
       else if Nat.eqb f fMain then
         Some ([], mkSig [] TUnit, Some main_body)
       else None)
    (fun m =>
       if Nat.eqb m mOrZero then Some ([ITy sElem], mkSig [] (TAbs sElem))
       else None).

  (* The program runs, and yields (). *)
  Example runs : interp 1000 Sg [] [] main_body = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

  (* Its type-erasure runs to the same value (Proposition 4.3, by
     computation on this instance). *)
  Example erased_runs : interp 1000 Sg [] [] (erase main_body) = Ok VUnit.
  Proof. vm_compute. reflexivity. Qed.

End Example.
