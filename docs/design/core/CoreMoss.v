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

(* ===================== 3. Keys, items, telescopes ======================== *)

Inductive key : Type :=
| KNom : nsym -> key
| KAbs : tsym -> key.

Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : key -> msym -> subst -> item.   (* provision of m at a receiver key *)

Definition tele := list item.

Definition key_eqb (a b : key) : bool :=
  match a, b with
  | KNom n1, KNom n2 => Nat.eqb n1 n2
  | KAbs t1, KAbs t2 => Nat.eqb t1 t2
  | _, _ => false
  end.

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
  | IMth k1 m1 th1, IMth k2 m2 th2 =>
      key_eqb k1 k2 && Nat.eqb m1 m2 && subst_eqb th1 th2
  | _, _ => false
  end.

(* The head of a type: the receiver key it dispatches under. *)
Definition ty_head (tau : ty) : option key :=
  match tau with
  | TNom n _ => Some (KNom n)
  | TAbs t => Some (KAbs t)
  | _ => None
  end.

Definition map_snd (A B C : Type) (f : B -> C) (l : list (A * B)) : list (A * C) :=
  map (fun p => (fst p, f (snd p))) l.

(* σ acting on keys and items (partial: a key must stay a key). *)
Definition app_subst_key (s : subst) (k : key) : option key :=
  match k with
  | KNom n => Some (KNom n)
  | KAbs t =>
      match lookup_subst t s with
      | Some tau => ty_head tau
      | None => Some (KAbs t)
      end
  end.

Definition app_subst_item (s : subst) (i : item) : option item :=
  match i with
  | ITy t => Some (ITy t)
  | IVal v => Some (IVal v)
  | IFn f => Some (IFn f)
  | IMth k m th =>
      match app_subst_key s k with
      | Some k' => Some (IMth k' m (map_snd (app_subst s) th))
      | None => None
      end
  end.

(* ===================== 4. Contexts in force ============================== *)

(* Φ = ⟨A; σ⟩: available items and the static substitution from type binds. *)
Record ctx : Type := mkCtx { cA : list item; cS : subst }.

(* Availability, ι ∈ Φ: membership in A modulo σ (the paper's residue of key
   canonicalization).  Defined as a boolean so that Proposition 4.1's
   decidability claim is discharged by construction. *)
Definition item_subst_eqb (s : subst) (i1 i2 : item) : bool :=
  match app_subst_item s i1, app_subst_item s i2 with
  | Some a, Some b => item_eqb a b
  | _, _ => false
  end.

Definition avail_b (Phi : ctx) (i : item) : bool :=
  existsb (fun i0 => item_subst_eqb (cS Phi) i0 i) (cA Phi).

Definition avail (Phi : ctx) (i : item) : Prop := avail_b Phi i = true.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

Definition tyreqs (D : tele) : list tsym :=
  flat_map (fun i => match i with ITy t => [t] | _ => [] end) D.

(* Satisfaction Φ ⊢ θ ⊨ Δ, in elaborated normal form: θ is defined exactly on
   Δ's type items (identity entries playing the paper's S-Pass), and every
   other requirement, read under θ, is available. *)
Definition sat (Phi : ctx) (th : subst) (D : tele) : Prop :=
  map fst th = tyreqs D /\
  Forall (fun i =>
            is_ty_item i = true \/
            match app_subst_item th i with
            | Some i1 => avail Phi i1
            | None => False
            end) D.

(* The identity application: the paper's ∅ ⊨ Δ, materialized. *)
Definition id_app (D : tele) : subst :=
  map (fun t => (t, TAbs t)) (tyreqs D).

Definition sat0 (Phi : ctx) (D : tele) : Prop := sat Phi (id_app D) D.

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

(* ===================== 7. Type formation and subtyping =================== *)

Definition is_nom (tau : ty) : Prop := exists n th, tau = TNom n th.

Definition heads_of (s : subst) (ms : list ty) : list (option key) :=
  map (fun tau => ty_head (app_subst s tau)) ms.

Inductive wf_ty (Sg : gsig) (Phi : ctx) : ty -> Prop :=
| WfUnit : wf_ty Sg Phi TUnit
| WfNeed : forall t, avail Phi (ITy t) -> wf_ty Sg Phi (TAbs t)
| WfTag : forall n D tau0 th,
    g_tag Sg n = Some (D, tau0) ->
    sat Phi th D ->
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
  match app_subst_item th i with
  | Some i1 =>
      match app_subst_item s i1, app_subst_item s i' with
      | Some a, Some b => a = b
      | _, _ => False
      end
  | None => False
  end.

Definition smap_ok (Phi : ctx) (th : subst) (sm : smap) (D : tele) : Prop :=
  Forall (fun i =>
            is_ty_item i = true \/
            exists i', lookup_smap i sm = Some i'
                       /\ In i' (cA Phi)
                       /\ item_matches (cS Phi) th i i') D.

(* Interpreting a method signature at a receiver: ρ = σ ∘ θ ∘ [This ↦ τ₀]. *)
Definition this_subst (tau0 : ty) : subst := [(this_sym, tau0)].

Definition interp_m (s th : subst) (tau0 tau : ty) : ty :=
  app_subst s (app_subst th (app_subst (this_subst tau0) tau)).

(* "ι is a provision of m at key k", judged after σ. *)
Definition is_prov_at (s : subst) (k : key) (m : msym) (i : item) : Prop :=
  exists th', app_subst_item s i = Some (IMth k m th').

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
    sat Phi th D ->
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
    sat Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Df ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e (app_subst (cS Phi) (app_subst th tau)))
            args (fs_params S) ->
    has_ty Sg Phi G (ECallD f th sm args)
           (app_subst (cS Phi) (app_subst th (fs_ret S)))
| TMethR : forall Phi G e0 tau0 m Dm S k k0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    has_ty Sg Phi G e0 tau0 ->
    ty_head (app_subst (cS Phi) tau0) = Some k ->
    In (IMth k0 m th0) (cA Phi) ->
    is_prov_at (cS Phi) k m (IMth k0 m th0) ->
    (* dispatch demands exactly one hit [D1] *)
    (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) k m i1 ->
                i1 = IMth k0 m th0) ->
    Forall2 (fun e tau =>
               has_ty Sg Phi G e
                      (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    has_ty Sg Phi G (EMeth e0 m (IMth k0 m th0) args)
           (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S))
| TMatchR : forall Phi G e0 tau0 ms arms tau,
    has_ty Sg Phi G e0 tau0 ->
    members (app_subst (cS Phi) tau0) = Some ms ->
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
    wf_ty Sg Phi tau' ->
    has_ty Sg (bind_ty t tau' Phi) G e tau ->
    has_ty Sg Phi G (EBindTy t tau' e) tau
| TBindValR : forall Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) ->
    sat0 Phi Dv ->
    has_ty Sg Phi G e1 (app_subst (cS Phi) tauv) ->
    has_ty Sg (add_item (IVal v) Phi) G e tau ->
    has_ty Sg Phi G (EBindVal v e1 e) tau
| TBindFnR : forall Phi G f Df S f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, S, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Phi Df ->
    sat0 Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* signature match after the substitutions in force; never inferred [D27] *)
    sig_eq (cS Phi) S' S ->
    has_ty Sg (add_item (IFn f) Phi) G e tau ->
    has_ty Sg Phi G (EBindFn f f' sm e) tau
| TBindMthR : forall Phi G tauk k m Dm S th f' D' S' body sm tau0' rest e tau,
    g_mth Sg m = Some (Dm, S) ->
    wf_ty Sg Phi tauk ->
    ty_head tauk = Some k ->
    sat Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Phi D' ->
    smap_ok Phi (id_app D') sm D' ->
    (* the provider's first parameter is the receiver *)
    fs_params S' = tau0' :: rest ->
    ty_eq (cS Phi) tau0' tauk ->
    map (app_subst (cS Phi)) rest
      = map (interp_m (cS Phi) th (app_subst (cS Phi) tauk)) (fs_params S) ->
    app_subst (cS Phi) (fs_ret S')
      = interp_m (cS Phi) th (app_subst (cS Phi) tauk) (fs_ret S) ->
    has_ty Sg (add_item (IMth k m th) Phi) G e tau ->
    has_ty Sg Phi G (EBindMth tauk m th f' sm e) tau.

(* ===================== 9. Telescope, declaration, program formation ====== *)

Definition mk_ctx (D : tele) : ctx := mkCtx D [].

Inductive item_ok (Sg : gsig) (Phi : ctx) : item -> Prop :=
| OkTy : forall t, g_ty Sg t = true -> item_ok Sg Phi (ITy t)
| OkVal : forall v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> sat0 Phi Dv -> item_ok Sg Phi (IVal v)
| OkFn : forall f Df S b,
    g_fn Sg f = Some (Df, S, b) -> sat0 Phi Df -> item_ok Sg Phi (IFn f)
| OkMth : forall k m Dm S th,
    g_mth Sg m = Some (Dm, S) ->
    sat Phi th Dm ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    item_ok Sg Phi (IMth k m th).

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
| EvBindMth : forall d g tauk k m th f' sm dc e w,
    ty_head tauk = Some k ->
    compose_smap d sm = Some dc ->
    eval Sg ((IMth k m th, DClo f' dc) :: d) g e w ->
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
      let interp_args :=
        fix go (l : list expr) : sum (list value) res :=
          match l with
          | [] => inl []
          | e1 :: r =>
              match interp k Sg d g e1 with
              | Ok w => match go r with
                        | inl ws => inl (w :: ws)
                        | inr rr => inr rr
                        end
              | rr => inr rr
              end
          end in
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
          match ty_head tauk, compose_smap d sm with
          | Some kk, Some dc =>
              interp k Sg ((IMth kk m th, DClo f' dc) :: d) g e1
          | _, _ => Err
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

(* ---- Determinism of the big-step relation (the interpreter is its
   deterministic implementation). ------------------------------------------ *)

Theorem eval_deterministic :
  forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
Admitted. (* routine induction; needs a nested induction principle for the
             Forall2 argument-list premises *)

(* ---- Adequacy of the interpreter for the relation. ---------------------- *)

Theorem interp_sound :
  forall fuel Sg d g e w,
    interp fuel Sg d g e = Ok w -> eval Sg d g e w.
Admitted. (* induction on fuel *)

Theorem interp_complete :
  forall Sg d g e w,
    eval Sg d g e w -> exists fuel, interp fuel Sg d g e = Ok w.
Admitted. (* induction on the evaluation derivation; fuel is its height *)

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
Admitted. (* preservation + context safety; routine but long induction, to be
             carried out over a strengthened invariant relating δ to Φ *)

(* ---- Proposition 4.3 (phase separation). --------------------------------
   Evaluation never inspects a type: it is invariant under erasing every
   application θ and every type bind. *)

Theorem phase_separation :
  forall Sg d g e w,
    eval Sg d g e w <-> eval Sg d g (erase e) w.
Admitted. (* induction on each derivation; no rule consults the erased fields *)

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

  (* The provision item Slot.or_zero[Elem = Elem]. *)
  Definition prov : item := IMth (KNom nSlot) mOrZero th_id.

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
