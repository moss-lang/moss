(* ------------------------------------------------------------------------ *)
(* Specialization.v — the specialization theorem for Core Moss [D16].        *)
(*                                                                           *)
(* D16: a program that type-checks generically never fails a check after    *)
(* specialization.  This file defines specialization in the calculus and    *)
(* proves that statement for it.                                             *)
(*                                                                           *)
(*  1. A *binding* (g, M) of a generic context Φ is what a specialization   *)
(*     is keyed by in both compilers [D80]: g says what each type symbol in  *)
(*     force is (a ground substitution: `ctx_grounded`, the grounding the   *)
(*     soundness proof already uses), and M — the specialized environment  *)
(*     — holds, under each requirement's *generic* key, its concrete        *)
(*     signature and the provider bound to it (the code symbol of a fn or   *)
(*     method provider, the functor and chain of a functor binding).  An    *)
(*     entry's provider has exactly the requirement's concrete signature    *)
(*     (`prov_sig`, as dentry_agree has it: the audit's BadProvider is      *)
(*     refused), and a functor binding's entry carries its capture         *)
(*     (`cap_static`: a captured grounding agreeing with g on X(φ), D84    *)
(*     rule 6), so an application is checked fixed at the specialized types *)
(*     too (MTApply).                                                       *)
(*  2. `spec g e` specializes code: every type annotation is read through g *)
(*     (a type bind's right side included, which then extends g), so no    *)
(*     abstract symbol is left.  Provision keys are *not* rewritten: they   *)
(*     are opaque labels resolved through M, as D80 resolves a method once  *)
(*     per call site against the generic type and codegen only looks the    *)
(*     target up.  (Rewriting the keys would be wrong: two keys a generic   *)
(*     body tells apart may collapse at a specialization; see §7, Collapse.)*)
(*  3. `mty` is the monomorphic checker: every type is compared by syntactic *)
(*     equality, every annotation must be ground, every val, fn, method and  *)
(*     functor requirement must resolve through M to an entry of the right  *)
(*     concrete signature, union heads must be distinct, and each call site *)
(*     and provider bind must hand its callee a valid binding of its own.   *)
(*     An application of an abstract functor [D78] is checked at *every*    *)
(*     ground instantiation of its result (the existential's parametricity), *)
(*     which in particular covers whatever the bound functor makes it.      *)
(*  4. Theorems: specialization preserves typing (`spec_typed`), every unit *)
(*     reachable from main through call sites and provider binds is a valid *)
(*     binding whose specialization is monomorphically well-typed            *)
(*     (`specialization`), evaluation is unchanged (`spec_eval`), the       *)
(*     runtime environment of the soundness proof is such a binding         *)
(*     (`runtime_binding`), and — the finding — the set of reachable units  *)
(*     need not be finite (`polyrec_infinite`): D30's depth backstop is a   *)
(*     real exception, not a formality.                                      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith.
Import ListNotations.
From NM Require Import CoreMoss.
From Stdlib Require Import Lia.

Set Implicit Arguments.

(* ===================== 1. Specialization of code ========================= *)

(* `spec` (specialization of code) is audit-level: Audit.v, Part III.1. *)

(* Plain substitution of the annotations of (already specialized) code: what
   instantiating an application's result types does to the code after it. *)
Fixpoint tsub (r : subst) (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVl v => EVl v
  | ETag n th e1 => ETag n (map_snd (app_subst r) th) (tsub r e1)
  | ECallD f th sm args =>
      ECallD f (map_snd (app_subst r) th) sm
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: q => tsub r a :: go q end) args)
  | ECallA f args =>
      ECallA f ((fix go (l : list expr) : list expr :=
                   match l with [] => [] | a :: q => tsub r a :: go q end) args)
  | EMeth e0 m prov args =>
      EMeth (tsub r e0) m prov
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: q => tsub r a :: go q end) args)
  | EMatch e0 arms =>
      EMatch (tsub r e0)
        ((fix go (l : list (nsym * var * expr)) :=
            match l with
            | [] => []
            | (n, x, eb) :: q => (n, x, tsub r eb) :: go q
            end) arms)
  | ELet x e1 e2 => ELet x (tsub r e1) (tsub r e2)
  | EBindTy t tau e1 => EBindTy t (app_subst r tau) (tsub r e1)
  | EBindVal v e1 e2 => EBindVal v (tsub r e1) (tsub r e2)
  | EBindFn f f' sm e1 => EBindFn f f' sm (tsub r e1)
  | EBindMth tk m th f' sm e1 => EBindMth tk m th f' sm (tsub r e1)
  | EBindFun u h sm smc e1 => EBindFun u h sm smc (tsub r e1)
  | EBindFunA u w sm e1 => EBindFunA u w sm (tsub r e1)
  | EApply u th sm e1 => EApply u th sm (tsub r e1)
  end.

Lemma spec_ECallD : forall g f th sm args,
  spec g (ECallD f th sm args) = ECallD f (map_snd (app_subst g) th) sm (map (spec g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Lemma spec_ECallA : forall g f args, spec g (ECallA f args) = ECallA f (map (spec g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Lemma spec_EMeth : forall g e0 m prov args,
  spec g (EMeth e0 m prov args) = EMeth (spec g e0) m prov (map (spec g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Definition spec_arm (g : subst) (a : nsym * var * expr) : nsym * var * expr :=
  (fst (fst a), snd (fst a), spec g (snd a)).
Lemma spec_EMatch : forall g e0 arms,
  spec g (EMatch e0 arms) = EMatch (spec g e0) (map (spec_arm g) arms).
Proof.
  intros. unfold spec_arm. simpl. f_equal; try (induction arms as [|[[n x] eb] r IH]; simpl; congruence).
Qed.

Lemma tsub_ECallD : forall g f th sm args,
  tsub g (ECallD f th sm args) = ECallD f (map_snd (app_subst g) th) sm (map (tsub g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Lemma tsub_ECallA : forall g f args, tsub g (ECallA f args) = ECallA f (map (tsub g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Lemma tsub_EMeth : forall g e0 m prov args,
  tsub g (EMeth e0 m prov args) = EMeth (tsub g e0) m prov (map (tsub g) args).
Proof. intros. simpl. f_equal; try (induction args; simpl; congruence). Qed.
Definition tsub_arm (g : subst) (a : nsym * var * expr) : nsym * var * expr :=
  (fst (fst a), snd (fst a), tsub g (snd a)).
Lemma tsub_EMatch : forall g e0 arms,
  tsub g (EMatch e0 arms) = EMatch (tsub g e0) (map (tsub_arm g) arms).
Proof.
  intros. unfold tsub_arm. simpl. f_equal; try (induction arms as [|[[n x] eb] r IH]; simpl; congruence).
Qed.

(* --- Substitution composition.  (One-pass app_subst composes exactly.) --- *)

Lemma app_subst_compose : forall r g tau,
  app_subst r (app_subst g tau) = app_subst (map_snd (app_subst r) g ++ r) tau.
Proof.
  intros r g tau. induction tau using ty_ind'.
  - reflexivity.
  - simpl. rewrite callee_env_agree_aux8, lookup_map_snd.
    destruct (lookup_subst t g); reflexivity.
  - rewrite !app_subst_nom. f_equal. unfold map_snd. rewrite map_map.
    apply map_ext_in. intros p Hp. rewrite Forall_forall in H.
    simpl. f_equal. apply H. exact Hp.
  - rewrite !app_subst_union. f_equal. rewrite map_map.
    apply map_ext_in. intros p Hp. rewrite Forall_forall in H. apply H. exact Hp.
Qed.

Lemma map_snd_compose : forall (r g th : subst),
  map_snd (app_subst r) (map_snd (app_subst g) th)
  = map_snd (app_subst (map_snd (app_subst r) g ++ r)) th.
Proof.
  intros. unfold map_snd. rewrite map_map. apply map_ext. intros [a b].
  simpl. rewrite app_subst_compose. reflexivity.
Qed.

Lemma tsub_spec : forall e r g,
  tsub r (spec g e) = spec (map_snd (app_subst r) g ++ r) e.
Proof.
  induction e using expr_ind'; intros r g; try reflexivity.
  - simpl. rewrite map_snd_compose, IHe. reflexivity.
  - rewrite spec_ECallD, tsub_ECallD, spec_ECallD, map_snd_compose, map_map.
    f_equal. apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_ECallA, tsub_ECallA, spec_ECallA, map_map.
    f_equal. apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_EMeth, tsub_EMeth, spec_EMeth, map_map, IHe.
    f_equal. apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_EMatch, tsub_EMatch, spec_EMatch, map_map, IHe.
    f_equal. apply map_ext_in. intros [[n x] eb] Ha. rewrite Forall_forall in H.
    unfold tsub_arm, spec_arm. simpl. f_equal. apply (H _ Ha).
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. rewrite IHe.
    change (map_snd (app_subst r) ((t, app_subst g tau) :: g))
      with ((t, app_subst r (app_subst g tau)) :: map_snd (app_subst r) g).
    rewrite app_subst_compose. reflexivity.
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
Qed.

(* --- Specialization changes nothing the dynamics looks at. --------------- *)

Lemma erase_spec : forall e g, erase (spec g e) = erase e.
Proof.
  induction e using expr_ind'; intros g; try reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - rewrite spec_ECallD, !erase_ECallD, map_map. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_ECallA, !erase_ECallA, map_map. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_EMeth, !erase_EMeth, map_map, IHe. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite spec_EMatch, !erase_EMatch, map_map, IHe. f_equal.
    apply map_ext_in. intros [[n x] eb] Ha. rewrite Forall_forall in H.
    unfold erase_arm, spec_arm. simpl. f_equal. apply (H _ Ha).
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. apply IHe.
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
Qed.

Lemma erase_tsub : forall e r, erase (tsub r e) = erase e.
Proof.
  induction e using expr_ind'; intros r; try reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - rewrite tsub_ECallD, !erase_ECallD, map_map. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite tsub_ECallA, !erase_ECallA, map_map. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite tsub_EMeth, !erase_EMeth, map_map, IHe. f_equal.
    apply map_ext_in. intros a Ha. rewrite Forall_forall in H. apply H. exact Ha.
  - rewrite tsub_EMatch, !erase_EMatch, map_map, IHe. f_equal.
    apply map_ext_in. intros [[n x] eb] Ha. rewrite Forall_forall in H.
    unfold erase_arm, tsub_arm. simpl. f_equal. apply (H _ Ha).
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe1, IHe2. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
  - simpl. rewrite IHe. reflexivity.
Qed.

(* Evaluation agreement: a specialization runs exactly as the generic code
   (Proposition 4.3 twice — specialization only rewrites what erasure
   drops). *)
Theorem spec_eval : forall Sg d ge g e w,
  eval Sg d ge (spec g e) w <-> eval Sg d ge e w.
Proof.
  intros. rewrite phase_separation, erase_spec, <- phase_separation. reflexivity.
Qed.

(* ===================== 2. Bindings: the specialized environment ========== *)

(* An entry of the specialized environment, under a requirement's generic
   key: a val's concrete type; a fn provider's code and the concrete
   signature it is called at; a method provider's code and the provision's
   concrete key (receiver and application); a functor binding [D78]. *)
Inductive ment : Type :=
| MVal : ty -> ment
| MFn  : fsym -> list ty -> ty -> ment
| MMth : fsym -> item -> ment
| MFun : usym -> list smap -> ment.

Definition menv := list (item * ment).

Fixpoint lookup_menv (i : item) (M : menv) : option ment :=
  match M with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_menv i r
  end.

(* M ∘ ς: a callee's specialized environment, keyed by its own requirements
   (the static twin of compose_smap; "a callee's [environment] is built by
   finding each need under the key the caller's code means by it and storing
   it under the callee's own" [D80]). *)
Fixpoint scompose (M : menv) (sm : smap) : option menv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_menv s M, scompose M r with
      | Some en, Some M' => Some ((req, en) :: M')
      | _, _ => None
      end
  end.

(* A binding's types, restricted to a telescope's own symbols. *)
Definition restrict (g : subst) (ts : list tsym) : subst :=
  map (fun t => (t, app_subst g (TAbs t))) ts.

Definition has_body (Sg : gsig) (f : fsym) : Prop :=
  exists D S b, g_fn Sg f = Some (D, S, Some b).

(* f' is a provider of the concrete signature ps -> r: it has a body, and
   its own signature, at some ground binding of its telescope, is ps -> r
   (as dentry_agree asks of a runtime closure).  The theorem audit's
   BadProvider labelled `f : () -> A` with a provider returning a B; this
   is the clause that refuses it (`bad_provider_refused`). *)
Definition prov_sig (Sg : gsig) (f' : fsym) (ps : list ty) (r : ty) : Prop :=
  exists D' S' b' g', g_fn Sg f' = Some (D', S', Some b') /\ grounding g'
  /\ map (app_subst g') (fs_params S') = ps /\ app_subst g' (fs_ret S') = r.

Lemma prov_sig_body : forall Sg f' ps r, prov_sig Sg f' ps r -> has_body Sg f'.
Proof. intros Sg f' ps r [D' [S' [b' [g' [H _]]]]]. exists D', S', b'. exact H. Qed.

(* D84 rule 6 / RECONCILE row 29: a functor binding is a closure, so its
   specialized entry carries the capture — the static twin of dentry_agree's
   DFun clause: there is a grounding gc of h's captured inputs (where it was
   bound), validating h's merge equations and pins, that reads every captured
   symbol h's linked arguments or its result also read as g does, each such
   symbol being a fixed input of φ.  So h's body is specialized at gc for
   what it captured and at the application for the rest, and the entry
   depends on g only through X(φ). *)
Definition cap_static (Sg : gsig) (g : subst) (u h : usym) (Dh Dr : tele) : Prop :=
  exists PhiC gc, tele_ctx (fst (g_cap Sg h)) = Some PhiC
  /\ grounding gc /\ covers gc (tyreqs (cA PhiC))
  /\ absorbs gc (cS PhiC) /\ absorbs gc (snd (g_cap Sg h))
  /\ (forall x, In x (tyreqs (cA PhiC)) -> In x (tyreqs Dh ++ res_reads Sg Dr) ->
        In x (g_fixed Sg u) /\ app_subst gc (TAbs x) = app_subst g (TAbs x)).

(* An entry is correct for a requirement at the types g: it is the
   requirement's signature read through g, and names a provider of exactly
   that signature (a fn with a body: for a method provider, the receiver
   then the method's parameters read at the key, as dentry_agree has it; a
   functor chained to the requirement, as dentry_agree asks of a runtime
   functor binding). *)
Definition mok (Sg : gsig) (g : subst) (i : item) (en : ment) : Prop :=
  match i, en with
  | IVal v, MVal t =>
      exists Dv tv, g_val Sg v = Some (Dv, tv) /\ t = app_subst g tv
  | IFn f, MFn f' ps r =>
      (exists Df Sf, g_fn Sg f = Some (Df, Sf, None)
                     /\ ps = map (app_subst g) (fs_params Sf)
                     /\ r = app_subst g (fs_ret Sf))
      /\ prov_sig Sg f' ps r
  | IMth rt m th, MMth f' ci =>
      ci = app_subst_item g (IMth rt m th)
      /\ (exists Dm Sm, g_mth Sg m = Some (Dm, Sm)
          /\ prov_sig Sg f'
               (app_subst g rt
                :: map (fun tau => app_subst g (app_subst (this_subst rt) (app_subst th tau)))
                       (fs_params Sm))
               (app_subst g (app_subst (this_subst rt) (app_subst th (fs_ret Sm)))))
  | IFun u, MFun h sms =>
      exists Du Dr Dh bs, g_fun Sg u = Some (Du, Dr, None)
      /\ g_fun Sg h = Some (Dh, Dr, Some bs) /\ chain_ok Sg h sms u
      /\ cap_static Sg g u h Dh Dr
  | _, _ => False
  end.

Definition mreal (Sg : gsig) (g : subst) (A : list item) (M : menv) : Prop :=
  Forall (fun i => is_ty_item i = true \/
                   exists en, lookup_menv i M = Some en /\ mok Sg g i en) A.

Definition item_reg (Sg : gsig) (A : list item) : Prop :=
  Forall (fun i => incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
                        (tyreqs A)) A.

Definition mth_shapes (Sg : gsig) (A : list item) : Prop :=
  forall rt m th Dm Sm, In (IMth rt m th) A -> g_mth Sg m = Some (Dm, Sm) ->
    exists PhiDm, tele_ctx Dm = Some PhiDm /\ map fst th = tyreqs (cA PhiDm).

(* (g, M) is a binding of the generic context Φ: g grounds every type symbol
   in force (and only those), absorbing the substitution in force; M resolves
   every non-type item in force; plus the two regularities the soundness
   proof also carries. *)
Definition binding_ok (Sg : gsig) (Phi : ctx) (g : subst) (M : menv) : Prop :=
  ctx_grounded g Phi
  /\ incl (map fst g) (tyreqs (cA Phi))
  /\ mreal Sg g (cA Phi) M
  /\ item_reg Sg (cA Phi)
  /\ mth_shapes Sg (cA Phi).

(* ===================== 3. The monomorphic checker ========================= *)

Definition closed_subst (th : subst) : Prop := Forall (fun p => closed_ty (snd p)) th.

(* Subtyping between concrete types: identity or injection [D17]. *)
Inductive msub : ty -> ty -> Prop :=
| MSubRefl : forall t, msub t t
| MSubInj : forall n th ms, In (TNom n th) ms -> msub (TNom n th) (TUnion ms).

(* A method's concrete parameter and result types at a concrete provision
   key IMth rt m th. *)
Definition mparam (rt : ty) (th : subst) (tau : ty) : ty := interp_m [] th rt tau.

Inductive mty (Sg : gsig) : subst -> menv -> tenv -> expr -> ty -> Prop :=
| MTVar : forall g M G x tau,
    lookup_tenv x G = Some tau -> mty Sg g M G (EVar x) tau
| MTUnit : forall g M G, mty Sg g M G EUnit TUnit
| MTSub : forall g M G e tau' tau,
    mty Sg g M G e tau' -> msub tau' tau -> mty Sg g M G e tau
| MTVal : forall g M G v tau,
    lookup_menv (IVal v) M = Some (MVal tau) -> mty Sg g M G (EVl v) tau
| MTTag : forall g M G n D tau0 th e,
    g_tag Sg n = Some (D, tau0) ->
    closed_subst th ->
    mty Sg g M G e (app_subst th tau0) ->
    mty Sg g M G (ETag n th e) (TNom n th)
(* An abstract fn call is a direct call of the provider bound to it. *)
| MTNeed : forall g M G f f' ps r args,
    lookup_menv (IFn f) M = Some (MFn f' ps r) ->
    Forall2 (mty Sg g M G) args ps ->
    mty Sg g M G (ECallA f args) r
(* A defined call: concrete application, and the callee's own binding —
   its types and its environment re-keyed through ς — is a valid binding of
   its telescope (the callee's specialization is a unit of its own). *)
| MTCall : forall g M G f Df S body th sm args PhiD Mc,
    g_fn Sg f = Some (Df, S, Some body) ->
    tele_ctx Df = Some PhiD ->
    closed_subst th ->
    scompose M sm = Some Mc ->
    binding_ok Sg PhiD (restrict th (tyreqs (cA PhiD))) Mc ->
    Forall2 (mty Sg g M G) args (map (app_subst th) (fs_params S)) ->
    mty Sg g M G (ECallD f th sm args) (app_subst th (fs_ret S))
(* A method call: the key resolves to one provision, whose concrete
   receiver is the receiver's type. *)
| MTMeth : forall g M G e0 m Dm S prov f' rt th args,
    g_mth Sg m = Some (Dm, S) ->
    lookup_menv prov M = Some (MMth f' (IMth rt m th)) ->
    mty Sg g M G e0 rt ->
    Forall2 (mty Sg g M G) args (map (mparam rt th) (fs_params S)) ->
    mty Sg g M G (EMeth e0 m prov args) (mparam rt th (fs_ret S))
| MTMatch : forall g M G e0 tau0 ms arms tau,
    mty Sg g M G e0 tau0 ->
    members tau0 = Some ms ->
    Forall is_nom ms ->
    NoDup (map member_head ms) ->
    Forall2 (arm_matches Sg) ms arms ->
    Forall2 (fun mem arm =>
               mty Sg g M ((arm_var arm, member_payload Sg mem) :: G)
                   (arm_body arm) tau) ms arms ->
    mty Sg g M G (EMatch e0 arms) tau
| MTLet : forall g M G x e1 e2 tau1 tau,
    mty Sg g M G e1 tau1 ->
    mty Sg g M ((x, tau1) :: G) e2 tau ->
    mty Sg g M G (ELet x e1 e2) tau
(* A type bind is a concrete alias, read into what is in scope. *)
| MTBindTy : forall g M G t c e tau,
    closed_ty c ->
    mty Sg ((t, c) :: g) M (map_snd (app_subst [(t, c)]) G) e tau ->
    mty Sg g M G (EBindTy t c e) tau
| MTBindVal : forall g M G v Dv tv e1 e tau,
    g_val Sg v = Some (Dv, tv) ->
    mty Sg g M G e1 (app_subst g tv) ->
    mty Sg g ((IVal v, MVal (app_subst g tv)) :: M) G e tau ->
    mty Sg g M G (EBindVal v e1 e) tau
(* A fn bind: the provider, specialized at the types here and its captured
   environment, is a valid unit with exactly the bound signature. *)
| MTBindFn : forall g M G f Df Sf f' D' S' body sm PhiD' Mc e tau,
    g_fn Sg f = Some (Df, Sf, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    tele_ctx D' = Some PhiD' ->
    scompose M sm = Some Mc ->
    binding_ok Sg PhiD' (restrict g (tyreqs (cA PhiD'))) Mc ->
    map (app_subst (restrict g (tyreqs (cA PhiD')))) (fs_params S')
      = map (app_subst g) (fs_params Sf) ->
    app_subst (restrict g (tyreqs (cA PhiD'))) (fs_ret S') = app_subst g (fs_ret Sf) ->
    mty Sg g ((IFn f, MFn f' (map (app_subst g) (fs_params Sf))
                              (app_subst g (fs_ret Sf))) :: M) G e tau ->
    mty Sg g M G (EBindFn f f' sm e) tau
| MTBindMth : forall g M G tk m th Dm Sm f' D' S' body sm PhiD' Mc rt' th'
                     t0 rest e tau,
    g_mth Sg m = Some (Dm, Sm) ->
    app_subst_item g (IMth tk m th) = IMth rt' m th' ->
    g_fn Sg f' = Some (D', S', Some body) ->
    tele_ctx D' = Some PhiD' ->
    scompose M sm = Some Mc ->
    binding_ok Sg PhiD' (restrict g (tyreqs (cA PhiD'))) Mc ->
    fs_params S' = t0 :: rest ->
    app_subst (restrict g (tyreqs (cA PhiD'))) t0 = rt' ->
    map (app_subst (restrict g (tyreqs (cA PhiD')))) rest
      = map (mparam rt' th') (fs_params Sm) ->
    app_subst (restrict g (tyreqs (cA PhiD'))) (fs_ret S') = mparam rt' th' (fs_ret Sm) ->
    mty Sg g ((IMth tk m th, MMth f' (IMth rt' m th')) :: M) G e tau ->
    mty Sg g M G (EBindMth tk m th f' sm e) tau
| MTBindFun : forall g M G u Du Dr h Dh bs sm smc e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    fun_link Sg Dh sm Du ->
    mty Sg g ((IFun u, MFun h [sm]) :: M) G e tau ->
    mty Sg g M G (EBindFun u h sm smc e) tau
| MTBindFunA : forall g M G u Du Dr w Dw h sms sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    g_fun Sg w = Some (Dw, Dr, None) ->
    lookup_menv (IFun w) M = Some (MFun h sms) ->
    fun_link Sg Dw sm Du ->
    mty Sg g ((IFun u, MFun h (sms ++ [sm])) :: M) G e tau ->
    mty Sg g M G (EBindFunA u w sm e) tau
(* An application [D78]: φ is bound here, its inputs as they are here form a
   valid binding of its argument telescope, and the code after it checks at
   every ground instantiation of the result — types and providers — that
   the signature allows: the bound functor's actual result is one of them. *)
(* Rule A: the application is at θ, read through g; its result is keyed
   by its items read at θ. *)
| MTApply : forall g M G u Du Dr th h sms sm PhiU Mu e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    lookup_menv (IFun u) M = Some (MFun h sms) ->
    tele_ctx Du = Some PhiU ->
    scompose M sm = Some Mu ->
    binding_ok Sg PhiU (restrict (map_snd (app_subst g) th) (tyreqs (cA PhiU))) Mu ->
    (* Rule A at the specialized types: θ, read through g, is the identity on
       X(φ) — the binding's captured fixed inputs are what this application
       reads (cap_static), so nothing is stale after specialization *)
    Forall (fun x => app_subst g (app_subst th (TAbs x)) = app_subst g (TAbs x))
           (g_fixed Sg u) ->
    (forall res Mr,
       grounding res -> map fst res = tyreqs Dr ->
       mreal Sg (g ++ res) (app_tele th Dr) Mr ->
       Forall (fun p => In (fst p) (app_tele th Dr)) Mr ->
       mty Sg (g ++ res) (Mr ++ M) (map_snd (app_subst res) G)
           (tsub res e) (app_subst res tau)) ->
    mty Sg g M G (EApply u th sm e) tau.

(* ===================== 4. Lemmas about bindings ========================== *)

Lemma scompose_defined : forall M sm,
  Forall (fun p => exists en, lookup_menv (snd p) M = Some en) sm ->
  exists M', scompose M sm = Some M'.
Proof.
  intros M sm H. induction sm as [|[req s] r IH]; simpl.
  - exists []. reflexivity.
  - inversion H as [|p0 r0 Hhd Htl]; subst.
    destruct Hhd as [en He]. simpl in He.
    destruct (IH Htl) as [M' HM']. rewrite He, HM'. eexists. reflexivity.
Qed.

Lemma scompose_first : forall M sm M' req sat',
  scompose M sm = Some M' -> lookup_smap req sm = Some sat' ->
  exists en, lookup_menv sat' M = Some en /\ lookup_menv req M' = Some en.
Proof.
  intros M sm. induction sm as [|[a b] r IH]; simpl; intros M' req sat' H Hl.
  - discriminate.
  - destruct (lookup_menv b M) as [en|] eqn:He; try discriminate.
    destruct (scompose M r) as [Mr|] eqn:Hr; try discriminate.
    injection H as H; subst M'.
    destruct (item_eqb req a) eqn:Ha.
    + injection Hl as Hl; subst sat'.
      exists en. split; [exact He|]. simpl. rewrite Ha. reflexivity.
    + destruct (IH Mr req sat' eq_refl Hl) as [en' [H1 H2]].
      exists en'. split; [exact H1|]. simpl. rewrite Ha. exact H2.
Qed.

Lemma lookup_smap_in : forall i sm j, lookup_smap i sm = Some j -> In (i, j) sm.
Proof.
  intros i sm. induction sm as [|[a b] r IH]; simpl; intros j H; [discriminate|].
  destruct (item_eqb i a) eqn:E.
  - injection H as <-. apply item_eqb_eq in E. subst. left. reflexivity.
  - right. apply IH. exact H.
Qed.

Lemma lookup_menv_app : forall i M1 M2,
  lookup_menv i (M1 ++ M2) =
  match lookup_menv i M1 with Some en => Some en | None => lookup_menv i M2 end.
Proof.
  intros i M1. induction M1 as [|[a en] r IH]; intros M2; simpl; [reflexivity|].
  destruct (item_eqb i a); [reflexivity | apply IH].
Qed.

Lemma lookup_menv_in_keys : forall i M en,
  lookup_menv i M = Some en -> In (i, en) M.
Proof.
  intros i M. induction M as [|[a b] r IH]; simpl; intros en H; [discriminate|].
  destruct (item_eqb i a) eqn:E.
  - injection H as <-. apply item_eqb_eq in E. subst. left. reflexivity.
  - right. apply IH. exact H.
Qed.

Lemma lookup_tenv_map : forall (f : ty -> ty) x G tau,
  lookup_tenv x G = Some tau -> lookup_tenv x (map_snd f G) = Some (f tau).
Proof.
  intros f x G. induction G as [|[y a] r IH]; simpl; intros tau H; [discriminate|].
  destruct (Nat.eqb x y); [injection H as <-; reflexivity | apply IH; exact H].
Qed.

(* Reading a type through a substitution only consults its free symbols. *)
Lemma app_subst_agree : forall g1 g2 tau,
  (forall t, In t (ty_frees tau) -> lookup_subst t g1 = lookup_subst t g2) ->
  app_subst g1 tau = app_subst g2 tau.
Proof. exact app_subst_frees_agree. Qed.

Lemma app_subst_item_agree : forall g1 g2 i,
  (forall t, In t (item_ty_frees i) -> lookup_subst t g1 = lookup_subst t g2) ->
  app_subst_item g1 i = app_subst_item g2 i.
Proof.
  intros g1 g2 [u|v|f|rt m th|uf] H; simpl; try reflexivity. f_equal.
  - apply app_subst_agree. intros t Ht. apply H. simpl. apply in_or_app. left. exact Ht.
  - unfold map_snd. apply map_ext_in. intros [z a] Hin. simpl. f_equal.
    apply app_subst_agree. intros q Hq. apply H. simpl. apply in_or_app. right.
    apply in_flat_map. exists (z, a). split; assumption.
Qed.

Lemma cap_static_reground : forall Sg g1 g2 u h Dh Dr,
  cap_static Sg g1 u h Dh Dr ->
  (forall x, In x (g_fixed Sg u) -> app_subst g1 (TAbs x) = app_subst g2 (TAbs x)) ->
  cap_static Sg g2 u h Dh Dr.
Proof.
  intros Sg g1 g2 u h Dh Dr [PhiC [gc [H1 [H2 [H3 [H4 [H5 H6]]]]]]] Hx.
  exists PhiC, gc. repeat (split; [assumption |]).
  intros x Hc Hr. destruct (H6 x Hc Hr) as [HX Heq]. split; [exact HX |].
  rewrite Heq. exact (Hx x HX).
Qed.

(* An entry stays correct under any binding that reads the requirement's own
   type material the same way. *)
Lemma mok_reground : forall Sg g1 g2 i en,
  wf_gsig Sg ->
  mok Sg g1 i en ->
  (forall t, In t (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) ->
             lookup_subst t g1 = lookup_subst t g2) ->
  mok Sg g2 i en.
Proof.
  intros Sg g1 g2 i en WF Hm Hag.
  destruct i as [t|v|f|rt m th|u]; destruct en as [tv0|f' ps r|f' ci|h sms];
    simpl in Hm |- *; try contradiction.
  - destruct Hm as [Dv [tv [Hv ->]]]. exists Dv, tv. split; [exact Hv|].
    apply app_subst_agree. intros x Hx. apply Hag. apply in_or_app. left.
    simpl. rewrite Hv.
    destruct (wf_val_decl WF _ Hv) as [PhiDv [Wtv Hwf]].
    apply wf_ty_frees in Hwf.
    destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtv x) (Hwf x Hx)) as [[] | Hin].
    exact Hin.
  - destruct Hm as [[Df [Sf [Hf [-> ->]]]] Hb]. split; [|exact Hb].
    exists Df, Sf. split; [exact Hf|].
    destruct (wf_fn_decl WF _ Hf) as [PhiDf [Wtf [Hps [Hrt _]]]].
    assert (Hdep : forall tau, wf_ty Sg PhiDf tau ->
              app_subst g1 tau = app_subst g2 tau).
    { intros tau Hw. apply app_subst_agree. intros x Hx. apply Hag.
      apply in_or_app. left. simpl. rewrite Hf. apply wf_ty_frees in Hw.
      destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtf x) (Hw x Hx)) as [[] | Hin].
      exact Hin. }
    split.
    + apply map_ext_in. intros tau Htau. apply Hdep.
      rewrite Forall_forall in Hps. apply Hps. exact Htau.
    + apply Hdep. exact Hrt.
  - destruct Hm as [-> [Dm [Sm [Hm Hp]]]]. split.
    + apply (app_subst_item_agree g1 g2 (IMth rt m th)). intros x Hx. apply Hag.
      apply in_or_app. right. apply in_or_app. left. exact Hx.
    + exists Dm, Sm. split; [exact Hm |].
      assert (HM : forall tau, In tau (fs_params Sm ++ [fs_ret Sm]) ->
                app_subst g1 (app_subst (this_subst rt) (app_subst th tau))
                = app_subst g2 (app_subst (this_subst rt) (app_subst th tau))).
      { intros tau Htau. apply app_subst_agree. intros x Hx. apply Hag.
        apply in_or_app. right. apply in_or_app. right. simpl. rewrite Hm.
        apply in_flat_map. exists (app_subst (this_subst rt) (app_subst th tau)).
        split; [apply in_map_iff; exists tau; split; [reflexivity | exact Htau] | exact Hx]. }
      assert (Hr : app_subst g1 rt = app_subst g2 rt).
      { apply app_subst_agree. intros x Hx. apply Hag.
        apply in_or_app. right. apply in_or_app. left. simpl. apply in_or_app. left. exact Hx. }
      rewrite <- Hr, <- (HM (fs_ret Sm)) by (apply in_or_app; right; left; reflexivity).
      replace (map (fun tau => app_subst g2 (app_subst (this_subst rt) (app_subst th tau)))
                   (fs_params Sm))
        with (map (fun tau => app_subst g1 (app_subst (this_subst rt) (app_subst th tau)))
                  (fs_params Sm)); [exact Hp |].
      apply map_ext_in. intros tau Htau. apply HM. apply in_or_app. left. exact Htau.
  - destruct Hm as [Du [Dr [Dh [bs [Hu [Hh [Hch Hcap]]]]]]].
    exists Du, Dr, Dh, bs. split; [exact Hu |]. split; [exact Hh |]. split; [exact Hch |].
    apply (@cap_static_reground Sg g1 g2 u h Dh Dr Hcap). intros x Hx. simpl.
    rewrite (Hag x); [reflexivity |]. simpl. rewrite app_nil_r. exact Hx.
Qed.

Lemma mok_retype : forall Sg g1 i1 g2 i2 en,
  wf_gsig Sg ->
  mok Sg g1 i1 en ->
  app_subst_item g1 i1 = app_subst_item g2 i2 ->
  (forall t, In t (dep_tys Sg i1) -> app_subst g1 (TAbs t) = app_subst g2 (TAbs t)) ->
  (* for a method item, the composed readings agree (dentry_agree_retype's (b)) *)
  (forall rt m th rt' th' Dm Sm tau,
      i1 = IMth rt m th -> i2 = IMth rt' m th' ->
      g_mth Sg m = Some (Dm, Sm) ->
      In tau (fs_params Sm) \/ tau = fs_ret Sm ->
      interp_m g1 th rt tau = interp_m g2 th' rt' tau) ->
  mok Sg g2 i2 en.
Proof.
  intros Sg g1 i1 g2 i2 en WF Hm Hit Hdep HM.
  destruct i1 as [t1|v1|f1|rt1 m1 th1|u1]; destruct i2 as [t2|v2|f2|rt2 m2 th2|u2];
    simpl in Hit; try discriminate;
    destruct en as [tv0|f' ps r|f' ci|h sms]; simpl in Hm |- *; try contradiction.
  - injection Hit as <-. destruct Hm as [Dv [tv [Hv ->]]]. exists Dv, tv.
    split; [exact Hv|].
    apply dentry_agree_retype_aux1. intros x Hx. apply Hdep. simpl. rewrite Hv.
    destruct (wf_val_decl WF _ Hv) as [PhiDv [Wtv Hwf]].
    apply wf_ty_frees in Hwf.
    destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtv x) (Hwf x Hx)) as [[] | Hin].
    exact Hin.
  - injection Hit as <-. destruct Hm as [[Df [Sf [Hf [-> ->]]]] Hb]. split; [|exact Hb].
    exists Df, Sf. split; [exact Hf|].
    destruct (wf_fn_decl WF _ Hf) as [PhiDf [Wtf [Hps [Hrt _]]]].
    assert (HD : forall tau, wf_ty Sg PhiDf tau -> app_subst g1 tau = app_subst g2 tau).
    { intros tau Hw. apply dentry_agree_retype_aux1. intros x Hx. apply Hdep.
      simpl. rewrite Hf. apply wf_ty_frees in Hw.
      destruct (proj1 (@wf_tele_tyreqs _ _ _ _ Wtf x) (Hw x Hx)) as [[] | Hin].
      exact Hin. }
    split.
    + apply map_ext_in. intros tau Htau. apply HD.
      rewrite Forall_forall in Hps. apply Hps. exact Htau.
    + apply HD. exact Hrt.
  - injection Hit as Hrt Hmm Hth. subst m2.
    destruct Hm as [-> [Dm [Sm [Hm Hp]]]]. split; [simpl; rewrite Hrt, Hth; reflexivity |].
    exists Dm, Sm. split; [exact Hm |].
    assert (E : forall tau, In tau (fs_params Sm) \/ tau = fs_ret Sm ->
              app_subst g1 (app_subst (this_subst rt1) (app_subst th1 tau))
              = app_subst g2 (app_subst (this_subst rt2) (app_subst th2 tau)))
      by (intros tau Ht; exact (HM rt1 m1 th1 rt2 th2 Dm Sm tau eq_refl eq_refl Hm Ht)).
    rewrite <- Hrt, <- (E (fs_ret Sm) (or_intror eq_refl)).
    replace (map (fun tau => app_subst g2 (app_subst (this_subst rt2) (app_subst th2 tau)))
                 (fs_params Sm))
      with (map (fun tau => app_subst g1 (app_subst (this_subst rt1) (app_subst th1 tau)))
                (fs_params Sm)); [exact Hp |].
    apply map_ext_in. intros tau Htau. apply E. left. exact Htau.
  - injection Hit as <-.
    destruct Hm as [Du [Dr [Dh [bs [Hu [Hh [Hch Hcap]]]]]]].
    exists Du, Dr, Dh, bs. split; [exact Hu |]. split; [exact Hh |]. split; [exact Hch |].
    apply (@cap_static_reground Sg g1 g2 u1 h Dh Dr Hcap). intros x Hx. apply Hdep.
    simpl. exact Hx.
Qed.

Lemma callee_grounding_dom : forall g Phi th A,
  map fst (callee_grounding g Phi th A) = tyreqs A.
Proof. intros. unfold callee_grounding. rewrite map_map. simpl. apply map_id. Qed.

(* The static twin of callee_env_agree: satisfying a telescope hands the
   callee a valid binding.  The ctx_grounded half is callee_env_agree's,
   verbatim; the environment half is coherence (deviation 8) read at
   concrete types. *)
Lemma scallee : forall Sg g Phi th sm D M PhiD,
  wf_gsig Sg ->
  wf_tele Sg empty_ctx D PhiD ->
  sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  smap_ok Phi th sm D ->
  binding_ok Sg Phi g M ->
  exists Mc, scompose M sm = Some Mc
    /\ binding_ok Sg PhiD (callee_grounding g Phi th (cA PhiD)) Mc.
Proof.
  intros Sg g Phi th sm D M PhiD WF Wtel Hsat Hth Hsm Hb.
  pose proof (wf_tele_ctx Wtel) as Htc.
  destruct Hsat as [PhiD1 [Htc1 [Hthdom [Hsat Hmerge]]]].
  assert (E1 : PhiD1 = PhiD) by congruence. subst PhiD1.
  destruct Hsm as [PhiD2 [Htc2 [Hsmdom Hsm]]].
  assert (E2 : PhiD2 = PhiD) by congruence. subst PhiD2.
  destruct Hb as [Hcg [Hdomg [Hmr [HAreg HAmth]]]].
  pose proof Hcg as Hcg0.
  destruct Hcg as [Hground [Habs [Hcovers [Hsigma Hreserved]]]].
  assert (Hacc0 : forall i, In i (cA empty_ctx) ->
      incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i)
           (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hdep0 : forall i, In i (cA empty_ctx) -> is_ty_item i = false ->
      incl (dep_tys Sg i) (tyreqs (cA empty_ctx)))
    by (intros i Hi; destruct Hi).
  assert (Hmth0 : forall rt0 m0 ap0 Dm0 Sm0,
      In (IMth rt0 m0 ap0) (cA empty_ctx) -> g_mth Sg m0 = Some (Dm0, Sm0) ->
      exists PhiDm0, tele_ctx Dm0 = Some PhiDm0
                     /\ map fst ap0 = tyreqs (cA PhiDm0))
    by (intros rt0 m0 ap0 Dm0 Sm0 Hi; destruct Hi).
  (* every satisfier has an entry *)
  assert (Hsmlook : Forall (fun p => exists en, lookup_menv (snd p) M = Some en) sm).
  { rewrite Forall_forall. intros [req sat'] Hin.
    rewrite Forall_forall in Hsm. destruct (Hsm (req, sat') Hin) as [Hina Hmatch].
    unfold mreal in Hmr. rewrite Forall_forall in Hmr.
    destruct (Hmr sat' Hina) as [Hty|[en [Hen _]]].
    - assert (Hreqin : In req (filter (fun i => negb (is_ty_item i)) (cA PhiD))).
      { rewrite <- Hsmdom. apply in_map_iff. exists (req, sat'). split; [reflexivity|exact Hin]. }
      apply filter_In in Hreqin. destruct Hreqin as [_ Hreqty].
      apply Bool.negb_true_iff in Hreqty.
      pose proof (@callee_env_agree_aux4 (cS Phi) th req sat' Hreqty Hmatch) as Hsatty.
      rewrite Hsatty in Hty. discriminate.
    - exists en. exact Hen. }
  destruct (@scompose_defined M sm Hsmlook) as [Mc Hcompose].
  exists Mc. split; [exact Hcompose|].
  (* the ctx_grounded half, as in callee_env_agree *)
  assert (Hcgground : grounding (callee_grounding g Phi th (cA PhiD))).
  { unfold grounding, callee_grounding.
    apply Forall_map. rewrite Forall_forall. intros t Htin.
    assert (Htth : In t (map fst th)) by (rewrite Hthdom; exact Htin).
    destruct (subst_chain_aux1 t th Htth) as [a Ha].
    assert (Hpa : In (t, a) th) by (apply callee_env_agree_aux1; exact Ha).
    rewrite Forall_forall in Hth.
    assert (Hwa := Hth (t, a) Hpa). simpl in Hwa.
    simpl. rewrite Ha. rewrite (absorbs_all Habs a).
    apply closed_app_subst_ground; [exact Hground|].
    intros u Hu. apply wf_ty_frees in Hwa. simpl in Hwa.
    destruct (Hcovers u (Hwa u Hu)) as [b Hb].
    pose proof (@callee_env_agree_aux1 u g b Hb) as Hpair.
    apply (in_map (@fst tsym ty)) in Hpair. simpl in Hpair. exact Hpair. }
  assert (HDreserved : ~ In this_sym (tyreqs (cA PhiD))).
  { apply (@callee_env_agree_aux13 Sg empty_ctx D PhiD Wtel). intro Hbad. exact Hbad. }
  assert (HsigmaD : sigma_regular (cS PhiD) (cA PhiD)).
  { apply (@tele_sigma_regular Sg empty_ctx D PhiD Wtel).
    split; [intros x Hx; destruct Hx | constructor]. }
  assert (HabsD : absorbs (callee_grounding g Phi th (cA PhiD)) (cS PhiD)).
  { intros t tau Hlk.
    assert (Hpair : In (t, tau) (cS PhiD)) by (apply callee_env_agree_aux1; exact Hlk).
    assert (Htin : In t (tyreqs (cA PhiD))).
    { apply (proj1 HsigmaD). apply in_map_iff. exists (t, tau). split; [reflexivity|exact Hpair]. }
    assert (Htaufree : incl (ty_frees tau) (tyreqs (cA PhiD))).
    { destruct HsigmaD as [_ Hrng]. rewrite Forall_forall in Hrng. exact (Hrng (t, tau) Hpair). }
    rewrite (@callee_grounding_lookup g Phi th (cA PhiD) t Htin).
    rewrite (@callee_grounding_reads g Phi th (cA PhiD) tau Htaufree).
    f_equal. unfold merge_respects in Hmerge. rewrite Forall_forall in Hmerge.
    exact (f_equal (app_subst g) (Hmerge (t, tau) Hpair)). }
  assert (Hctx : ctx_grounded (callee_grounding g Phi th (cA PhiD)) PhiD).
  { split; [exact Hcgground|]. split; [exact HabsD|].
    split.
    - intros t Hin. eexists. apply callee_grounding_lookup. exact Hin.
    - split; [exact HsigmaD|exact HDreserved]. }
  split; [exact Hctx|].
  split; [rewrite callee_grounding_dom; intros x Hx; exact Hx|].
  split; [| split].
  - (* the environment half *)
    unfold mreal. rewrite Forall_forall. intros i Hin.
    destruct (is_ty_item i) eqn:Hty; [left; reflexivity | right].
    assert (Hif : In i (filter (fun j => negb (is_ty_item j)) (cA PhiD))).
    { apply filter_In. split; [exact Hin|]. apply Bool.negb_true_iff. exact Hty. }
    assert (Himap : In i (map fst sm)) by (rewrite Hsmdom; exact Hif).
    destruct (callee_env_agree_aux3 i sm Himap) as [j Hlookup].
    destruct (@scompose_first M sm Mc i j Hcompose Hlookup) as [en [Hj Hi]].
    exists en. split; [exact Hi|].
    pose proof (@lookup_smap_in i sm j Hlookup) as Hpij.
    rewrite Forall_forall in Hsm. destruct (Hsm (i, j) Hpij) as [Hja Hmatch].
    simpl in Hja, Hmatch.
    pose proof (callee_env_agree_aux4 Hty Hmatch) as Hjty.
    unfold mreal in Hmr. rewrite Forall_forall in Hmr.
    destruct (Hmr j Hja) as [Hbad | [en' [Hj' Hok]]]; [rewrite Hjty in Hbad; discriminate|].
    rewrite Hj in Hj'. injection Hj' as <-.
    eapply (@mok_retype Sg g j); [exact WF | exact Hok | | | ].
    + symmetry. eapply callee_env_agree_aux6; [exact Habs| |exact Hmatch].
      intros u Hu.
      exact (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0 i Hin u
               (in_or_app _ _ _ (or_intror (in_or_app _ _ _ (or_introl Hu))))).
    + intros t Hdep.
      assert (Hdep' : In t (dep_tys Sg i)).
      { pose proof (@callee_env_agree_aux7 Sg (cS Phi) th i j Hmatch) as Heqdep.
        rewrite Heqdep. exact Hdep. }
      rewrite callee_grounding_reads.
      * rewrite Forall_forall in Hsat. destruct (Hsat i Hin) as [Hty'|[_ Hcoh]].
        -- rewrite Hty in Hty'. discriminate.
        -- rewrite Forall_forall in Hcoh. rewrite (Hcoh t Hdep').
           symmetry. apply absorbs_all. exact Habs.
      * intros u Hu. simpl in Hu. destruct Hu as [Heq|Hu]; [subst u|contradiction].
        exact (@tele_deps_closed Sg empty_ctx D PhiD Wtel Hdep0 i Hin Hty t Hdep').
    + intros rt m ap rt' ap' Dm Sm tau HiE HjE Hm Htau.
      simpl in HiE, HjE.
      subst j. subst i.
      simpl in Hmatch.
      injection Hmatch as Hrt Hap.
      destruct (@callee_env_agree_aux11 Sg empty_ctx D PhiD rt' m ap' Dm Sm
                  Wtel Hmth0 Hin Hm) as [PhiDm [HtcDm Hapdom]].
      assert (Hapdom' : map fst ap = tyreqs (cA PhiDm)).
      { pose proof (f_equal (map (@fst tsym ty)) Hap) as E.
        unfold map_snd in E. rewrite !map_map in E. simpl in E.
        rewrite <- Hapdom. symmetry. exact E. }
      assert (Hwf_tau :
        wf_ty Sg (add_item (ITy this_sym) PhiDm) tau).
      { destruct (wf_mth_decl WF _ Hm) as [PhiDm0 [WtDm [Hps Hret]]].
        assert (EDm : PhiDm0 = PhiDm).
        { pose proof (wf_tele_ctx WtDm) as HtcDm0. congruence. }
        subst PhiDm0.
        destruct Htau as [HtauIn|Heq].
        - rewrite Forall_forall in Hps. exact (Hps tau HtauIn).
        - subst tau. exact Hret. }
      assert (Hfree_tau :
        incl (ty_frees tau) (this_sym :: tyreqs (cA PhiDm))).
      { apply wf_ty_frees in Hwf_tau. simpl in Hwf_tau. exact Hwf_tau. }
      apply callee_env_agree_aux5.
      intros t Ht.
      specialize (Hfree_tau t Ht).
      unfold interp_m. simpl.
      destruct (lookup_subst t ap) as [b|] eqn:Hb;
      destruct (lookup_subst t ap') as [a|] eqn:Ha.
      -- assert (Hreqreg :=
             @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
               (IMth rt' m ap') Hin).
         assert (Hafree : incl (ty_frees a) (tyreqs (cA PhiD))).
         { intros u Hu. apply Hreqreg. simpl.
           apply in_or_app. left. apply in_or_app. right.
           apply in_flat_map. exists (t, a). split.
           - apply callee_env_agree_aux1. exact Ha.
           - exact Hu. }
         assert (Hbfree : incl (ty_frees b) (tyreqs (cA Phi))).
         { unfold item_reg in HAreg. rewrite Forall_forall in HAreg.
           specialize (HAreg (IMth rt m ap) Hja).
           intros u Hu. apply HAreg. simpl.
           apply in_or_app. left. apply in_or_app. right.
           apply in_flat_map. exists (t, b). split.
           - apply callee_env_agree_aux1. exact Hb.
           - exact Hu. }
         rewrite (callee_env_agree_aux14 rt b).
         2: { intro Hbad. exact (Hreserved (Hbfree _ Hbad)). }
         rewrite (callee_env_agree_aux14 rt' a).
         2: { intro Hbad. exact (HDreserved (Hafree _ Hbad)). }
         rewrite callee_grounding_reads by exact Hafree.
         assert (Hapeq :
           map_snd
             (fun x => app_subst (cS Phi) (app_subst th x)) ap' =
           map_snd (app_subst (cS Phi)) ap).
         { unfold map_snd in Hap |- *. rewrite map_map in Hap. exact Hap. }
         destruct (@callee_env_agree_aux12
           (fun x => app_subst (cS Phi) (app_subst th x))
           (app_subst (cS Phi))
           ap' ap t a Hapeq Ha) as [b' [Hb' Hab]].
         rewrite Hb in Hb'. injection Hb' as Heq. subst b'.
         rewrite <- (absorbs_all Habs b).
         exact (f_equal (app_subst g) (eq_sym Hab)).
      -- exfalso.
         assert (Hinap : In t (map fst ap)).
         { pose proof (@callee_env_agree_aux1 t ap b Hb) as Hp.
           apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
         rewrite Hapdom' in Hinap. rewrite <- Hapdom in Hinap.
         destruct (subst_chain_aux1 t ap' Hinap) as [x Hx].
         rewrite Ha in Hx. discriminate.
      -- exfalso.
         assert (Hinap : In t (map fst ap')).
         { pose proof (@callee_env_agree_aux1 t ap' a Ha) as Hp.
           apply (in_map (@fst tsym ty)) in Hp. simpl in Hp. exact Hp. }
         rewrite Hapdom in Hinap. rewrite <- Hapdom' in Hinap.
         destruct (subst_chain_aux1 t ap Hinap) as [x Hx].
         rewrite Hb in Hx. discriminate.
      -- assert (Htthis : t = this_sym).
         { destruct Hfree_tau as [Heq|HinD]; [symmetry; exact Heq|].
           exfalso. rewrite <- Hapdom' in HinD.
           destruct (subst_chain_aux1 t ap HinD) as [x Hx].
           rewrite Hb in Hx. discriminate. }
         subst t. simpl.
         assert (Hreqreg :=
           @tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0
             (IMth rt' m ap') Hin).
         assert (Hrtfree : incl (ty_frees rt') (tyreqs (cA PhiD))).
         { intros u Hu. apply Hreqreg. simpl.
           apply in_or_app. left. apply in_or_app. left.
           exact Hu. }
         rewrite callee_grounding_reads by exact Hrtfree.
         rewrite Hrt. symmetry. apply absorbs_all. exact Habs.
  - unfold item_reg. rewrite Forall_forall. intros i Hin.
    exact (@tele_items_regular Sg empty_ctx D PhiD WF Wtel Hacc0 i Hin).
  - intros rt m ap Dm Sm Hin Hm.
    exact (@callee_env_agree_aux11 Sg empty_ctx D PhiD rt m ap Dm Sm Wtel Hmth0 Hin Hm).
Qed.

Lemma callee_grounding_restrict : forall g Phi th A,
  absorbs g (cS Phi) ->
  incl (tyreqs A) (map fst th) ->
  callee_grounding g Phi th A = restrict (map_snd (app_subst g) th) (tyreqs A).
Proof.
  intros g Phi th A Habs Hinc. unfold callee_grounding, restrict.
  apply map_ext_in. intros t Ht. f_equal. simpl.
  rewrite lookup_map_snd.
  destruct (subst_chain_aux1 t th (Hinc t Ht)) as [a Ha]. rewrite Ha.
  apply absorbs_all. exact Habs.
Qed.

Lemma callee_grounding_id : forall g Phi A,
  absorbs g (cS Phi) ->
  callee_grounding g Phi (id_app A) A = restrict g (tyreqs A).
Proof.
  intros g Phi A Habs. unfold callee_grounding, restrict.
  apply map_ext. intros t. f_equal. rewrite safety_case_bindfn_aux3.
  apply absorbs_all. exact Habs.
Qed.

(* --- What each bind does to a binding. ---------------------------------- *)

Lemma mreal_reground : forall Sg g1 g2 A M,
  wf_gsig Sg -> mreal Sg g1 A M -> item_reg Sg A ->
  (forall u, In u (tyreqs A) -> lookup_subst u g1 = lookup_subst u g2) ->
  mreal Sg g2 A M.
Proof.
  intros Sg g1 g2 A M WF Hm Hreg Hl. unfold mreal in *.
  rewrite Forall_forall in Hm |- *. unfold item_reg in Hreg. rewrite Forall_forall in Hreg.
  intros i Hi. destruct (Hm i Hi) as [H|[en [He Hok]]]; [left; exact H|right].
  exists en. split; [exact He|]. eapply mok_reground; [exact WF|exact Hok|].
  intros t Ht. apply Hl. apply (Hreg i Hi). exact Ht.
Qed.

Lemma binding_bindty : forall Sg Phi t tau' g M,
  wf_gsig Sg ->
  t <> this_sym ->
  ~ In (ITy t) (cA Phi) ->
  wf_ty Sg Phi tau' ->
  binding_ok Sg Phi g M ->
  binding_ok Sg (bind_ty t tau' Phi) ((t, app_subst g tau') :: g) M.
Proof.
  intros Sg Phi t tau' g M WF Hneq Hfresh Htau' Hb.
  destruct Hb as [Hg [Hdomg [Hmr [Hreg Hmth]]]].
  pose proof Hg as Hg0.
  destruct Hg as [Hground [Habs [Hcover Hsig]]].
  destruct Hsig as [Hsig Hreserved].
  destruct Hsig as [Hdom Hran].
  set (tau2 := app_subst g tau').
  set (g2 := (t, tau2) :: g).
  assert (Hlook : forall u, In u (tyreqs (cA Phi)) -> lookup_subst u g2 = lookup_subst u g).
  { intros u Hu. unfold g2. apply safety_case_bindty_aux2 with (A := cA Phi); assumption. }
  assert (Hfree' : incl (ty_frees tau') (tyreqs (cA Phi)))
    by (apply wf_ty_frees in Htau'; exact Htau').
  assert (Hclosed2 : closed_ty tau2).
  { unfold tau2. apply closed_app_subst_ground; [exact Hground|].
    intros u Hu. destruct (Hcover u (Hfree' u Hu)) as [a Ha].
    apply safety_case_bindty_aux4 in Ha. exact Ha. }
  assert (Hg2abs : absorbs g2 ((t, app_subst (cS Phi) tau') :: cS Phi)).
  { intros u a Hl. simpl in Hl.
    destruct (Nat.eqb u t) eqn:E.
    - injection Hl as Hl. subst a. apply Nat.eqb_eq in E. subst u.
      simpl. rewrite Nat.eqb_refl. unfold tau2. f_equal.
      rewrite <- (absorbs_all Habs). symmetry.
      apply app_subst_frees_agree. intros z Hz. apply Hlook.
      eapply safety_case_bindty_aux3; eauto.
    - assert (Hu : In u (map fst (cS Phi))) by (apply safety_case_bindty_aux4 in Hl; exact Hl).
      unfold g2. simpl. rewrite E.
      rewrite (app_subst_frees_agree ((t, tau2) :: g) g a).
      + exact (Habs u a Hl).
      + intros z Hz. change (lookup_subst z g2 = lookup_subst z g).
        apply Hlook. rewrite Forall_forall in Hran.
        apply (Hran (u, a)); [|exact Hz].
        apply safety_case_bindty_aux5. exact Hl. }
  assert (Hg2cover : covers g2 (tyreqs (cA (bind_ty t tau' Phi)))).
  { intros u Hu. simpl in Hu. destruct Hu as [E | Hu].
    - subst u. exists tau2. simpl. rewrite Nat.eqb_refl. reflexivity.
    - destruct (Hcover u Hu) as [a Ha]. exists a. rewrite Hlook; assumption. }
  assert (Hg2sig : sigma_regular (cS (bind_ty t tau' Phi)) (cA (bind_ty t tau' Phi))).
  { split.
    - simpl. intros u [E | Hu]; [subst; left; reflexivity | right; apply Hdom; exact Hu].
    - simpl. constructor.
      + intros u Hu. right. eapply safety_case_bindty_aux3; eauto.
      + eapply Forall_impl; [|exact Hran]. intros [u a] Ha z Hz. right. exact (Ha z Hz). }
  split; [| split; [| split; [| split]]].
  - split; [constructor; [exact Hclosed2|exact Hground]|].
    split; [exact Hg2abs|]. split; [exact Hg2cover|]. split; [exact Hg2sig|].
    simpl. intros [Heq|Hin]; [apply Hneq; exact Heq | exact (Hreserved Hin)].
  - simpl. intros u [E|Hu]; [left; exact E | right; apply Hdomg; exact Hu].
  - unfold mreal in *. simpl. constructor; [left; reflexivity|].
    eapply mreal_reground in Hmr; [exact Hmr | exact WF | exact Hreg |].
    intros u Hu. symmetry. apply Hlook. exact Hu.
  - unfold item_reg in *. simpl. constructor.
    + intros u Hu. simpl in Hu. contradiction.
    + eapply Forall_impl; [|exact Hreg]. intros i Hi z Hz. right. exact (Hi z Hz).
  - intros rt m th Dm Sm Hin Hm. simpl in Hin. destruct Hin as [E|Hin]; [discriminate|].
    exact (Hmth rt m th Dm Sm Hin Hm).
Qed.

Lemma binding_push : forall Sg Phi g M i en,
  binding_ok Sg Phi g M ->
  mok Sg g i en ->
  incl (dep_tys Sg i ++ item_ty_frees i ++ method_item_frees Sg i) (tyreqs (cA Phi)) ->
  is_ty_item i = false ->
  (forall rt m th Dm Sm, i = IMth rt m th -> g_mth Sg m = Some (Dm, Sm) ->
     exists PhiDm, tele_ctx Dm = Some PhiDm /\ map fst th = tyreqs (cA PhiDm)) ->
  binding_ok Sg (add_item i Phi) g ((i, en) :: M).
Proof.
  intros Sg Phi g M i en [Hg [Hdomg [Hmr [Hreg Hmth]]]] Hok Hi Hty Hsh.
  assert (Htyeq : tyreqs (cA (add_item i Phi)) = tyreqs (cA Phi)).
  { simpl. unfold tyreqs. simpl. destruct i; try reflexivity. discriminate. }
  split; [| split; [| split; [| split]]].
  - destruct Hg as [Hgr [Hab [Hcv [Hsr Hth]]]].
    split; [exact Hgr|]. split; [exact Hab|].
    unfold covers, sigma_regular in *. rewrite Htyeq. simpl.
    split; [exact Hcv|]. split; [exact Hsr|exact Hth].
  - rewrite Htyeq. exact Hdomg.
  - unfold mreal in *. simpl. constructor.
    + right. exists en. simpl. rewrite (proj2 (item_eqb_eq i i) eq_refl).
      split; [reflexivity|exact Hok].
    + eapply Forall_impl; [|exact Hmr]. intros j [Hj|[en' [He Hj]]]; [left; exact Hj|right].
      simpl. destruct (item_eqb j i) eqn:E.
      * apply item_eqb_eq in E. subst j. exists en. split; [reflexivity|exact Hok].
      * exists en'. split; [exact He|exact Hj].
  - unfold item_reg in *. rewrite Htyeq. simpl. constructor; [exact Hi|exact Hreg].
  - intros rt m th Dm Sm Hin Hm. simpl in Hin. destruct Hin as [E|Hin].
    + eapply Hsh; [exact E | exact Hm].
    + exact (Hmth rt m th Dm Sm Hin Hm).
Qed.

Lemma lookup_app_left : forall t (a b : subst) x,
  lookup_subst t a = Some x -> lookup_subst t (a ++ b) = Some x.
Proof. intros. rewrite callee_env_agree_aux8, H. reflexivity. Qed.

Lemma lookup_notin_dom : forall t (a : subst), ~ In t (map fst a) -> lookup_subst t a = None.
Proof.
  intros t a H. destruct (lookup_subst t a) eqn:E; [|reflexivity].
  exfalso. apply H. eapply safety_case_bindty_aux4. exact E.
Qed.

(* After an application [D78]: the binding extended at the result by any
   ground instantiation of its types and any resolution of its items. *)
Lemma binding_open_res : forall Sg Phi u Du Dr g M res Mr,
  wf_gsig Sg ->
  g_fun Sg u = Some (Du, Dr, None) ->
  sat0 Sg Phi Du ->
  Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
  binding_ok Sg Phi g M ->
  grounding res -> map fst res = tyreqs Dr ->
  mreal Sg (g ++ res) Dr Mr -> Forall (fun p => In (fst p) Dr) Mr ->
  binding_ok Sg (open_res Dr Phi) (g ++ res) (Mr ++ M).
Proof.
  intros Sg Phi u Du Dr g M res Mr WF Hu Hs0 Hfresh Hb Hres Hresdom HMr HMrk.
  destruct Hb as [Hg [Hdomg [Hmr [Hreg Hmth]]]].
  pose proof Hg as Hg0.
  destruct Hg as [Hground [Habs [Hcover [[Hdom Hran] Hreserved]]]].
  assert (Hlk : forall x, In x (tyreqs (cA Phi)) -> lookup_subst x (g ++ res) = lookup_subst x g).
  { intros x Hx. destruct (Hcover x Hx) as [a Ha]. rewrite Ha. apply lookup_app_left. exact Ha. }
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiD [HtcD [WtD [WtR _]]]].
  assert (HthisD : ~ In this_sym (tyreqs (cA PhiD))).
  { apply (@callee_env_agree_aux13 Sg empty_ctx Du PhiD WtD). intro H; exact H. }
  assert (HthisR : ~ In this_sym (tyreqs Dr)).
  { intro H. apply (@callee_env_agree_aux13 Sg PhiD Dr _ WtR HthisD). simpl.
    rewrite tyreqs_app. apply in_or_app. right. exact H. }
  assert (Hnotold : forall x, In x (tyreqs Dr) -> ~ In x (tyreqs (cA Phi))).
  { intros x Hx Hin. rewrite Forall_forall in Hfresh. apply (Hfresh x Hx).
    apply safety_case_bindty_aux1. exact Hin. }
  split; [| split; [| split; [| split]]].
  - split; [apply Forall_app; split; assumption|].
    split.
    + intros t tau Hl. pose proof (Habs t tau Hl) as Ht.
      assert (Htin : In t (tyreqs (cA Phi))).
      { apply Hdom. eapply safety_case_bindty_aux4. exact Hl. }
      rewrite (Hlk t Htin), Ht. f_equal. apply app_subst_frees_agree.
      intros z Hz. symmetry. apply Hlk. rewrite Forall_forall in Hran.
      apply (Hran (t, tau)); [apply safety_case_bindty_aux5; exact Hl | exact Hz].
    + split; [| split].
      * intros x Hx. simpl in Hx. rewrite tyreqs_app in Hx. apply in_app_or in Hx.
        destruct Hx as [Hx|Hx].
        -- assert (Hng : lookup_subst x g = None).
           { apply lookup_notin_dom. intro H. apply (Hnotold x Hx). apply Hdomg. exact H. }
           rewrite callee_env_agree_aux8, Hng.
           apply subst_chain_aux1. rewrite Hresdom. exact Hx.
        -- rewrite (Hlk x Hx). exact (Hcover x Hx).
      * apply sigma_regular_weaken with (A := cA Phi); [split; assumption|].
        apply open_res_tyreqs.
      * simpl. rewrite tyreqs_app. intro H. apply in_app_or in H.
        destruct H as [H|H]; [exact (HthisR H) | exact (Hreserved H)].
  - simpl. rewrite tyreqs_app, map_app. intros x Hx. apply in_app_or in Hx.
    apply in_or_app. destruct Hx as [Hx|Hx]; [right; apply Hdomg; exact Hx|].
    left. rewrite <- Hresdom. exact Hx.
  - unfold mreal in *. simpl. apply Forall_app. split.
    + eapply Forall_impl; [|exact HMr]. intros i [Hi|[en [He Hok]]]; [left; exact Hi|right].
      exists en. rewrite lookup_menv_app, He. split; [reflexivity|exact Hok].
    + rewrite Forall_forall in Hmr |- *. intros i Hi.
      destruct (is_ty_item i) eqn:Hit; [left; reflexivity|right].
      destruct (Hmr i Hi) as [Hty|[en [He Hok]]]; [rewrite Hit in Hty; discriminate|].
      rewrite lookup_menv_app.
      destruct (lookup_menv i Mr) as [en'|] eqn:E.
      * pose proof (@lookup_menv_in_keys i Mr en' E) as Ein.
        rewrite Forall_forall in HMrk.
        pose proof (HMrk _ Ein) as HiDr. simpl in HiDr.
        rewrite Forall_forall in HMr. destruct (HMr i HiDr) as [Hty'|[en'' [He'' Hok'']]].
        -- rewrite Hit in Hty'; discriminate.
        -- rewrite E in He''. injection He'' as <-. exists en'.
           split; [reflexivity|exact Hok''].
      * exists en. split; [exact He|]. eapply mok_reground; [exact WF|exact Hok|].
        intros t Ht. symmetry. apply Hlk. unfold item_reg in Hreg. rewrite Forall_forall in Hreg.
        apply (Hreg i Hi). exact Ht.
  - unfold item_reg in *. simpl. apply Forall_app. split.
    + rewrite Forall_forall. intros i Hi. eapply open_res_regular; [exact WF|exact Hu|exact Hs0|exact Hi].
    + eapply Forall_impl; [|exact Hreg]. intros i Hi z Hz. apply open_res_tyreqs. exact (Hi z Hz).
  - assert (Hmth0 : forall rt0 m0 ap0 Dm0 Sm0,
      In (IMth rt0 m0 ap0) (cA empty_ctx) -> g_mth Sg m0 = Some (Dm0, Sm0) ->
      exists PhiDm0, tele_ctx Dm0 = Some PhiDm0 /\ map fst ap0 = tyreqs (cA PhiDm0))
      by (intros rt0 m0 ap0 Dm0 Sm0 Hi; destruct Hi).
    intros rt m th Dm Sm Hin Hm. simpl in Hin. apply in_app_or in Hin.
    destruct Hin as [Hin|Hin]; [|exact (Hmth rt m th Dm Sm Hin Hm)].
    eapply (@callee_env_agree_aux11 Sg PhiD Dr _ rt m th Dm Sm WtR); [| |exact Hm].
    + intros rt' m' ap' Dm' Sm' Hin' Hm'.
      exact (@callee_env_agree_aux11 Sg empty_ctx Du PhiD rt' m' ap' Dm' Sm' WtD Hmth0 Hin' Hm').
    + simpl. apply in_or_app. right. exact Hin.
Qed.

(* Rule A: the same after an application at θ, whose result is keyed by its
   items read at θ. *)
Lemma binding_open_res_th : forall Sg Phi u Du Dr th g M res Mr,
  wf_gsig Sg ->
  g_fun Sg u = Some (Du, Dr, None) ->
  sat Sg Phi th Du ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  res_coherent Sg Phi th Dr ->
  Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
  binding_ok Sg Phi g M ->
  grounding res -> map fst res = tyreqs Dr ->
  mreal Sg (g ++ res) (app_tele th Dr) Mr ->
  Forall (fun p => In (fst p) (app_tele th Dr)) Mr ->
  binding_ok Sg (open_res (app_tele th Dr) Phi) (g ++ res) (Mr ++ M).
Proof.
  intros Sg Phi u Du Dr th g M res Mr WF Hu Hs0 Hthw Hcoh Hfresh Hb Hres Hresdom HMr HMrk.
  destruct Hb as [Hg [Hdomg [Hmr [Hreg Hmth]]]].
  pose proof Hg as Hg0.
  destruct Hg as [Hground [Habs [Hcover [[Hdom Hran] Hreserved]]]].
  assert (Hlk : forall x, In x (tyreqs (cA Phi)) -> lookup_subst x (g ++ res) = lookup_subst x g).
  { intros x Hx. destruct (Hcover x Hx) as [a Ha]. rewrite Ha. apply lookup_app_left. exact Ha. }
  destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiD [HtcD [WtD [WtR _]]]].
  assert (HthisD : ~ In this_sym (tyreqs (cA PhiD))).
  { apply (@callee_env_agree_aux13 Sg empty_ctx Du PhiD WtD). intro H; exact H. }
  assert (HthisR : ~ In this_sym (tyreqs Dr)).
  { intro H. apply (@callee_env_agree_aux13 Sg PhiD Dr _ WtR HthisD). simpl.
    rewrite tyreqs_app. apply in_or_app. right. exact H. }
  assert (Hnotold : forall x, In x (tyreqs Dr) -> ~ In x (tyreqs (cA Phi))).
  { intros x Hx Hin. rewrite Forall_forall in Hfresh. apply (Hfresh x Hx).
    apply safety_case_bindty_aux1. exact Hin. }
  split; [| split; [| split; [| split]]].
  - split; [apply Forall_app; split; assumption|].
    split.
    + intros t tau Hl. pose proof (Habs t tau Hl) as Ht.
      assert (Htin : In t (tyreqs (cA Phi))).
      { apply Hdom. eapply safety_case_bindty_aux4. exact Hl. }
      rewrite (Hlk t Htin), Ht. f_equal. apply app_subst_frees_agree.
      intros z Hz. symmetry. apply Hlk. rewrite Forall_forall in Hran.
      apply (Hran (t, tau)); [apply safety_case_bindty_aux5; exact Hl | exact Hz].
    + split; [| split].
      * intros x Hx. simpl in Hx. rewrite tyreqs_app, tyreqs_app_tele in Hx. apply in_app_or in Hx.
        destruct Hx as [Hx|Hx].
        -- assert (Hng : lookup_subst x g = None).
           { apply lookup_notin_dom. intro H. apply (Hnotold x Hx). apply Hdomg. exact H. }
           rewrite callee_env_agree_aux8, Hng.
           apply subst_chain_aux1. rewrite Hresdom. exact Hx.
        -- rewrite (Hlk x Hx). exact (Hcover x Hx).
      * apply sigma_regular_weaken with (A := cA Phi); [split; assumption|].
        apply open_res_tyreqs.
      * simpl. rewrite tyreqs_app, tyreqs_app_tele. intro H. apply in_app_or in H.
        destruct H as [H|H]; [exact (HthisR H) | exact (Hreserved H)].
  - simpl. rewrite tyreqs_app, tyreqs_app_tele, map_app. intros x Hx. apply in_app_or in Hx.
    apply in_or_app. destruct Hx as [Hx|Hx]; [right; apply Hdomg; exact Hx|].
    left. rewrite <- Hresdom. exact Hx.
  - unfold mreal in *. simpl. apply Forall_app. split.
    + eapply Forall_impl; [|exact HMr]. intros i [Hi|[en [He Hok]]]; [left; exact Hi|right].
      exists en. rewrite lookup_menv_app, He. split; [reflexivity|exact Hok].
    + rewrite Forall_forall in Hmr |- *. intros i Hi.
      destruct (is_ty_item i) eqn:Hit; [left; reflexivity|right].
      destruct (Hmr i Hi) as [Hty|[en [He Hok]]]; [rewrite Hit in Hty; discriminate|].
      rewrite lookup_menv_app.
      destruct (lookup_menv i Mr) as [en'|] eqn:E.
      * pose proof (@lookup_menv_in_keys i Mr en' E) as Ein.
        rewrite Forall_forall in HMrk.
        pose proof (HMrk _ Ein) as HiDr. simpl in HiDr.
        rewrite Forall_forall in HMr. destruct (HMr i HiDr) as [Hty'|[en'' [He'' Hok'']]].
        -- rewrite Hit in Hty'; discriminate.
        -- rewrite E in He''. injection He'' as <-. exists en'.
           split; [reflexivity|exact Hok''].
      * exists en. split; [exact He|]. eapply mok_reground; [exact WF|exact Hok|].
        intros t Ht. symmetry. apply Hlk. unfold item_reg in Hreg. rewrite Forall_forall in Hreg.
        apply (Hreg i Hi). exact Ht.
  - unfold item_reg in *. simpl. apply Forall_app. split.
    + rewrite Forall_forall. intros i Hi.
      exact (@open_res_th_regular Sg Phi u Du Dr th WF Hu Hs0 Hthw Hcoh (conj Hdom Hran) i Hi).
    + eapply Forall_impl; [|exact Hreg]. intros i Hi z Hz. apply open_res_tyreqs. exact (Hi z Hz).
  - assert (Hmth0 : forall rt0 m0 ap0 Dm0 Sm0,
      In (IMth rt0 m0 ap0) (cA empty_ctx) -> g_mth Sg m0 = Some (Dm0, Sm0) ->
      exists PhiDm0, tele_ctx Dm0 = Some PhiDm0 /\ map fst ap0 = tyreqs (cA PhiDm0))
      by (intros rt0 m0 ap0 Dm0 Sm0 Hi; destruct Hi).
    intros rt m th0 Dm Sm Hin Hm. simpl in Hin. apply in_app_or in Hin.
    destruct Hin as [Hin|Hin]; [|exact (Hmth rt m th0 Dm Sm Hin Hm)].
    unfold app_tele in Hin. apply in_map_iff in Hin. destruct Hin as [i [Ei Hin]].
    destruct i as [?|?|?|rt1 m1 ap1|?]; try discriminate Ei.
    simpl in Ei. injection Ei as <- <- <-.
    destruct (@callee_env_agree_aux11 Sg PhiD Dr _ rt1 m1 ap1 Dm Sm WtR) as [PhiDm [HtcDm Hfst]];
      [| |exact Hm|].
    3: { exists PhiDm. split; [exact HtcDm |]. unfold map_snd. rewrite map_map. exact Hfst. }
    + intros rt' m' ap' Dm' Sm' Hin' Hm'.
      exact (@callee_env_agree_aux11 Sg empty_ctx Du PhiD rt' m' ap' Dm' Sm' WtD Hmth0 Hin' Hm').
    + simpl. apply in_or_app. right. exact Hin.
Qed.


(* ===================== 5. Specialization preserves typing ================ *)

Lemma ground_closed : forall g Phi tau,
  ctx_grounded g Phi -> incl (ty_frees tau) (tyreqs (cA Phi)) -> closed_ty (app_subst g tau).
Proof.
  intros g Phi tau [Hgr [_ [Hcv _]]] Hf. apply closed_app_subst_ground; [exact Hgr|].
  intros u Hu. destruct (Hcv u (Hf u Hu)) as [a Ha]. eapply safety_case_bindty_aux4. exact Ha.
Qed.

Lemma map_snd_closed : forall r g, grounding g -> map_snd (app_subst r) g = g.
Proof.
  intros r g H. unfold grounding in H. induction H as [|[t a] q Ha Hq IH]; [reflexivity|].
  change (map_snd (app_subst r) ((t, a) :: q)) with ((t, app_subst r a) :: map_snd (app_subst r) q).
  rewrite IH. simpl in Ha. rewrite (app_subst_closed r Ha). reflexivity.
Qed.

Lemma restrict_reads : forall g ts tau,
  incl (ty_frees tau) ts -> app_subst (restrict g ts) tau = app_subst g tau.
Proof.
  intros g ts tau H. apply dentry_agree_retype_aux1. intros t Ht. simpl.
  unfold restrict.
  pose proof (lookup_prefix_in (fun t0 => app_subst g (TAbs t0)) ts t [] (H t Ht)) as E.
  rewrite app_nil_r in E. rewrite E. reflexivity.
Qed.

Lemma sat_dom : forall Sg Phi th D PhiD,
  sat Sg Phi th D -> tele_ctx D = Some PhiD -> map fst th = tyreqs (cA PhiD).
Proof.
  intros Sg Phi th D PhiD [PhiD1 [H1 [Hd _]]] H2. rewrite H1 in H2. injection H2 as <-. exact Hd.
Qed.

Lemma wf_th_closed : forall Sg g Phi th,
  ctx_grounded g Phi -> Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  closed_subst (map_snd (app_subst g) th).
Proof.
  intros Sg g Phi th Hg H. unfold closed_subst, map_snd. rewrite Forall_map.
  eapply Forall_impl; [|exact H]. intros p Hp. simpl.
  eapply ground_closed; [exact Hg|]. apply wf_ty_frees in Hp. exact Hp.
Qed.

(* A method's signature read at a provision, then grounded, is the method's
   concrete signature at the provision's ground key. *)
Lemma mparam_ground : forall g s th0 rt tau,
  grounding g -> absorbs g s ->
  ~ In this_sym (map fst g) ->
  ~ In this_sym (map fst th0) ->
  Forall (fun p => incl (ty_frees (snd p)) (map fst g)) th0 ->
  incl (ty_frees tau) (this_sym :: map fst th0) ->
  app_subst g (interp_m s th0 (app_subst s rt) tau)
  = mparam (app_subst g rt) (map_snd (app_subst g) th0) tau.
Proof.
  intros g s th0 rt tau Hgr Habs Hng Hnth Hran Hfr.
  unfold interp_m at 1. rewrite (absorbs_all Habs).
  change (app_subst g (app_subst (this_subst (app_subst s rt)) (app_subst th0 tau)))
    with (interp_m g th0 (app_subst s rt) tau).
  unfold mparam. apply callee_env_agree_aux5. intros t Ht.
  unfold interp_m. rewrite app_subst_nil. simpl.
  rewrite lookup_map_snd.
  destruct (Hfr t Ht) as [<- | Hin].
  - rewrite (lookup_notin_dom this_sym th0 Hnth). simpl.
    replace (this_sym =? this_sym) with true by (symmetry; apply Nat.eqb_refl).
    apply absorbs_all. exact Habs.
  - destruct (subst_chain_aux1 t th0 Hin) as [a Ha]. rewrite Ha.
    assert (Hfa : incl (ty_frees a) (map fst g)).
    { rewrite Forall_forall in Hran. apply (Hran (t, a)). apply callee_env_agree_aux1. exact Ha. }
    rewrite callee_env_agree_aux14.
    + rewrite callee_env_agree_aux14; [reflexivity|].
      rewrite (@closed_app_subst_ground g a Hgr Hfa). intros [].
    + intro Hb. apply Hng. apply Hfa. exact Hb.
Qed.

Lemma msub_of_subty : forall g Phi tau' tau,
  ctx_grounded g Phi -> subty Phi tau' tau ->
  msub (app_subst g tau') (app_subst g tau).
Proof.
  intros g Phi tau' tau [_ [Habs _]] H. destruct H as [t1 t2 Heq | n th ms tau'' Hin Heq].
  - unfold ty_eq in Heq. rewrite <- (absorbs_all Habs t1), Heq, (absorbs_all Habs). constructor.
  - unfold ty_eq in Heq. rewrite app_subst_nom, app_subst_union. constructor.
    rewrite <- app_subst_nom. rewrite <- (absorbs_all Habs), Heq, (absorbs_all Habs).
    apply in_map. exact Hin.
Qed.

Lemma members_subst : forall g X ms,
  members X = Some ms -> members (app_subst g X) = Some (map (app_subst g) ms).
Proof.
  intros g X ms H. destruct X; simpl in H; try discriminate; injection H as <-.
  - rewrite app_subst_nom. reflexivity.
  - rewrite app_subst_union. reflexivity.
Qed.

Lemma wf_members : forall Sg Phi X ms,
  wf_ty Sg Phi X -> members X = Some ms ->
  Forall (wf_ty Sg Phi) ms /\ Forall is_nom ms.
Proof.
  intros Sg Phi X ms Hw H. destruct X; simpl in H; try discriminate; injection H as <-.
  - split; constructor; [exact Hw|constructor| |constructor]. eexists; eexists; reflexivity.
  - inversion Hw; subst. split; assumption.
Qed.

Lemma head_subst : forall g m, is_nom m -> member_head (app_subst g m) = member_head m.
Proof. intros g m [n [th ->]]. reflexivity. Qed.

Lemma nom_subst : forall g m, is_nom m -> is_nom (app_subst g m).
Proof. intros g m [n [th ->]]. rewrite app_subst_nom. eexists; eexists; reflexivity. Qed.

Lemma arm_matches_subst : forall Sg g m a,
  is_nom m -> arm_matches Sg m a -> arm_matches Sg (app_subst g m) a.
Proof. intros Sg g m a [n [th ->]] H. rewrite app_subst_nom. exact H. Qed.

Lemma payload_subst : forall Sg g Phi m,
  wf_gsig Sg -> wf_ty Sg Phi m -> is_nom m ->
  app_subst g (member_payload Sg m) = member_payload Sg (app_subst g m).
Proof.
  intros Sg g Phi m WF Hw [n [th ->]]. rewrite app_subst_nom. simpl.
  inversion Hw as [| |n' D tau0 th' Hn Hsat Hth|]; subst.
  rewrite Hn. apply app_subst_chain.
  destruct (wf_tag_decl WF _ Hn) as [PhiD [Wt Hwt]].
  rewrite (sat_dom Hsat (@wf_tele_ctx Sg _ _ Wt)). eapply wf_ty_frees. exact Hwt.
Qed.

Lemma Forall2_map_r : forall (A B : Type) (R : A -> B -> Prop) (f : B -> B) l1 l2,
  Forall2 (fun a b => R a (f b)) l1 l2 -> Forall2 R l1 (map f l2).
Proof. intros. induction H; simpl; constructor; assumption. Qed.

Lemma Forall2_map_l : forall (A B : Type) (R : A -> B -> Prop) (f : A -> A) l1 l2,
  Forall2 (fun a b => R (f a) b) l1 l2 -> Forall2 R (map f l1) l2.
Proof. intros. induction H; simpl; constructor; assumption. Qed.

Lemma Forall2_map_both : forall (A B : Type) (R : A -> B -> Prop) (h : A -> A) (f : B -> B) l1 l2,
  Forall2 (fun a b => R (h a) (f b)) l1 l2 -> Forall2 R (map h l1) (map f l2).
Proof. intros. induction H; simpl; constructor; assumption. Qed.

Lemma Forall2_impl_in : forall (A B : Type) (R1 R2 : A -> B -> Prop) l1 l2,
  (forall a b, In a l1 -> In b l2 -> R1 a b -> R2 a b) ->
  Forall2 R1 l1 l2 -> Forall2 R2 l1 l2.
Proof.
  intros A B R1 R2 l1 l2 H F. induction F; constructor.
  - apply H; [left; reflexivity|left; reflexivity|assumption].
  - apply IHF. intros a b Ha Hb. apply H; right; assumption.
Qed.

Section SpecCases.
  Variable Sg : gsig.
  Variable WF : wf_gsig Sg.

  Definition SP (Phi : ctx) (G : tenv) (e : expr) (tau : ty) : Prop :=
    forall g M, binding_ok Sg Phi g M ->
      mty Sg g M (map_snd (app_subst g) G) (spec g e) (app_subst g tau).

  Lemma bo_abs : forall Phi g M, binding_ok Sg Phi g M ->
    forall t, app_subst g (app_subst (cS Phi) t) = app_subst g t.
  Proof. intros Phi g M [[_ [Habs _]] _]. apply absorbs_all. exact Habs. Qed.

  Lemma bo_entry : forall Phi g M i, binding_ok Sg Phi g M -> In i (cA Phi) ->
    is_ty_item i = false -> exists en, lookup_menv i M = Some en /\ mok Sg g i en.
  Proof.
    intros Phi g M i [_ [_ [Hmr _]]] Hi Hty. unfold mreal in Hmr. rewrite Forall_forall in Hmr.
    destruct (Hmr i Hi) as [H|H]; [rewrite Hty in H; discriminate | exact H].
  Qed.

  Lemma bo_dom_frees : forall Phi g M tau, binding_ok Sg Phi g M ->
    incl (ty_frees tau) (tyreqs (cA Phi)) -> incl (ty_frees tau) (map fst g).
  Proof.
    intros Phi g M tau [[_ [_ [Hcv _]]] _] H u Hu. destruct (Hcv u (H u Hu)) as [a Ha].
    eapply safety_case_bindty_aux4. exact Ha.
  Qed.

  Lemma case_sub : forall Phi G e tau' tau,
    SP Phi G e tau' -> subty Phi tau' tau -> SP Phi G e tau.
  Proof.
    intros Phi G e tau' tau IH Hs g M Hb. eapply MTSub; [apply IH; exact Hb|].
    eapply msub_of_subty; [exact (proj1 Hb) | exact Hs].
  Qed.

  Lemma case_val : forall Phi G v Dv tauv,
    g_val Sg v = Some (Dv, tauv) -> avail Phi (IVal v) ->
    SP Phi G (EVl v) (app_subst (cS Phi) tauv).
  Proof.
    intros Phi G v Dv tauv Hv Hav g M Hb. simpl. rewrite (bo_abs Hb).
    destruct (@bo_entry Phi g M (IVal v) Hb (avail_val_in Hav) eq_refl) as [en [He Hok]].
    destruct en; simpl in Hok; try contradiction.
    destruct Hok as [Dv' [tv [Hv' ->]]]. rewrite Hv in Hv'. injection Hv' as <- <-.
    apply MTVal. exact He.
  Qed.

  Lemma case_tag : forall Phi G n D tau0 th e,
    g_tag Sg n = Some (D, tau0) -> sat Sg Phi th D ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    SP Phi G e (app_subst (cS Phi) (app_subst th tau0)) ->
    SP Phi G (ETag n th e) (app_subst (cS Phi) (TNom n th)).
  Proof.
    intros Phi G n D tau0 th e Hn Hsat Hth IH g M Hb. simpl spec. rewrite (bo_abs Hb).
    rewrite app_subst_nom. eapply MTTag; [exact Hn | eapply wf_th_closed; [exact (proj1 Hb)|exact Hth] |].
    pose proof (IH g M Hb) as H. rewrite (bo_abs Hb) in H.
    rewrite app_subst_chain in H; [exact H|].
    destruct (wf_tag_decl WF _ Hn) as [PhiD [Wt Hwt]].
    rewrite (sat_dom Hsat (@wf_tele_ctx Sg _ _ Wt)). eapply wf_ty_frees. exact Hwt.
  Qed.

  Lemma case_need : forall Phi G f Df S args,
    g_fn Sg f = Some (Df, S, None) -> avail Phi (IFn f) ->
    Forall2 (fun e tau => SP Phi G e (app_subst (cS Phi) tau)) args (fs_params S) ->
    SP Phi G (ECallA f args) (app_subst (cS Phi) (fs_ret S)).
  Proof.
    intros Phi G f Df S args Hf Hav IH g M Hb. rewrite spec_ECallA, (bo_abs Hb).
    destruct (@bo_entry Phi g M (IFn f) Hb (avail_fn_in Hav) eq_refl) as [en [He Hok]].
    destruct en; simpl in Hok; try contradiction.
    destruct Hok as [[Df' [Sf' [Hf' [-> ->]]]] _]. rewrite Hf in Hf'. injection Hf' as <- <-.
    eapply MTNeed; [exact He|]. apply Forall2_map_both.
    eapply Forall2_impl; [|exact IH]. intros a b Hab. simpl.
    pose proof (Hab g M Hb) as H. rewrite (bo_abs Hb) in H. exact H.
  Qed.

  Lemma case_call : forall Phi G f Df S body th sm args,
    g_fn Sg f = Some (Df, S, Some body) -> sat Sg Phi th Df ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th -> smap_ok Phi th sm Df ->
    Forall2 (fun e tau => SP Phi G e (app_subst (cS Phi) (app_subst th tau))) args (fs_params S) ->
    SP Phi G (ECallD f th sm args) (app_subst (cS Phi) (app_subst th (fs_ret S))).
  Proof.
    intros Phi G f Df S body th sm args Hf Hsat Hth Hsm IH g M Hb.
    rewrite spec_ECallD, (bo_abs Hb).
    destruct (wf_fn_decl WF _ Hf) as [PhiD [Wt [Hps [Hrt _]]]].
    pose proof (@wf_tele_ctx Sg _ _ Wt) as Htc.
    pose proof (sat_dom Hsat Htc) as Hdom.
    destruct (@scallee Sg g Phi th sm Df M PhiD WF Wt Hsat Hth Hsm Hb) as [Mc [Hc Hbc]].
    rewrite callee_grounding_restrict in Hbc;
      [| exact (proj1 (proj2 (proj1 Hb))) | rewrite Hdom; intros x Hx; exact Hx].
    assert (Hrd : forall tau, wf_ty Sg PhiD tau ->
              app_subst g (app_subst th tau) = app_subst (map_snd (app_subst g) th) tau).
    { intros tau Hw. apply app_subst_chain. rewrite Hdom. eapply wf_ty_frees. exact Hw. }
    rewrite (Hrd _ Hrt).
    eapply MTCall; [exact Hf | exact Htc | eapply wf_th_closed; [exact (proj1 Hb)|exact Hth]
                   | exact Hc | exact Hbc |].
    apply Forall2_map_both. eapply Forall2_impl_in; [|exact IH].
    intros a b _ Hbin Hab. pose proof (Hab g M Hb) as H. rewrite (bo_abs Hb) in H.
    rewrite Hrd in H; [exact H|]. rewrite Forall_forall in Hps. apply Hps. exact Hbin.
  Qed.

  Lemma bo_this : forall Phi g M, binding_ok Sg Phi g M -> ~ In this_sym (map fst g).
  Proof.
    intros Phi g M [[_ [_ [_ [_ Hr]]]] [Hd _]] H. apply Hr. apply Hd. exact H.
  Qed.

  Lemma case_meth : forall Phi G e0 tau0 m Dm S rt0 th0 args,
    g_mth Sg m = Some (Dm, S) ->
    SP Phi G e0 tau0 ->
    In (IMth rt0 m th0) (cA Phi) ->
    app_subst (cS Phi) rt0 = app_subst (cS Phi) tau0 ->
    Forall2 (fun e tau => SP Phi G e (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau))
            args (fs_params S) ->
    SP Phi G (EMeth e0 m (IMth rt0 m th0) args)
       (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) (fs_ret S)).
  Proof.
    intros Phi G e0 tau0 m Dm S rt0 th0 args Hm IH0 Hin Hrt IH g M Hb.
    rewrite spec_EMeth.
    destruct (@bo_entry Phi g M (IMth rt0 m th0) Hb Hin eq_refl) as [en [He Hok]].
    destruct en as [|f' ps r|f' ci|]; simpl in Hok; try contradiction.
    destruct Hok as [-> _].
    pose proof Hb as [[Hgr [Habs _]] [_ [_ [Hreg Hmth]]]].
    destruct (Hmth _ _ _ _ _ Hin Hm) as [PhiDm [HtcDm Hthdom]].
    destruct (wf_mth_decl WF _ Hm) as [PhiDm' [WtDm [Hps Hret]]].
    rewrite (@wf_tele_ctx Sg _ _ WtDm) in HtcDm. injection HtcDm as <-.
    assert (Hgrt : app_subst g tau0 = app_subst g rt0).
    { rewrite <- (bo_abs Hb tau0), <- (bo_abs Hb rt0), Hrt. reflexivity. }
    assert (Hnth : ~ In this_sym (map fst th0)).
    { rewrite Hthdom. apply (@callee_env_agree_aux13 Sg empty_ctx Dm PhiDm' WtDm). intro H; exact H. }
    assert (Hran : Forall (fun p => incl (ty_frees (snd p)) (map fst g)) th0).
    { rewrite Forall_forall. intros [x a] Hp z Hz. unfold item_reg in Hreg.
      rewrite Forall_forall in Hreg. eapply bo_dom_frees; [exact Hb| |exact Hz].
      intros q Hq. apply (Hreg _ Hin). apply in_or_app. right. apply in_or_app. left.
      simpl. apply in_or_app. right. apply in_flat_map. exists (x, a). split; assumption. }
    assert (Hconv : forall tau, wf_ty Sg (add_item (ITy this_sym) PhiDm') tau ->
              app_subst g (interp_m (cS Phi) th0 (app_subst (cS Phi) tau0) tau)
              = mparam (app_subst g rt0) (map_snd (app_subst g) th0) tau).
    { intros tau Hw. rewrite <- Hgrt. apply mparam_ground; try assumption.
      - exact (bo_this Hb).
      - apply wf_ty_frees in Hw. simpl in Hw. rewrite Hthdom. exact Hw. }
    rewrite (Hconv _ Hret).
    eapply MTMeth; [exact Hm | exact He | rewrite <- Hgrt; apply IH0; exact Hb |].
    apply Forall2_map_both. eapply Forall2_impl_in; [|exact IH].
    intros a b _ Hbin Hab. pose proof (Hab g M Hb) as H.
    rewrite Hconv in H; [exact H|]. rewrite Forall_forall in Hps. apply Hps. exact Hbin.
  Qed.

  Lemma case_match : forall Phi G e0 tau0 ms arms tau,
    SP Phi G e0 tau0 ->
    wf_ty Sg Phi (app_subst (cS Phi) tau0) ->
    members (app_subst (cS Phi) tau0) = Some ms ->
    NoDup (map member_head ms) ->
    Forall2 (arm_matches Sg) ms arms ->
    Forall2 (fun mem arm => SP Phi ((arm_var arm, member_payload Sg mem) :: G)
                               (arm_body arm) tau) ms arms ->
    SP Phi G (EMatch e0 arms) tau.
  Proof.
    intros Phi G e0 tau0 ms arms tau IH0 Hw Hms Hnd Ham IH g M Hb.
    rewrite spec_EMatch.
    destruct (wf_members Hw Hms) as [Hwm Hnom].
    rewrite Forall_forall in Hwm, Hnom.
    eapply MTMatch with (tau0 := app_subst g tau0) (ms := map (app_subst g) ms).
    - apply IH0. exact Hb.
    - rewrite <- (bo_abs Hb). apply members_subst. exact Hms.
    - rewrite Forall_forall. intros x Hx. apply in_map_iff in Hx. destruct Hx as [m0 [<- Hm0]].
      apply nom_subst. apply Hnom. exact Hm0.
    - rewrite map_map. erewrite map_ext_in; [exact Hnd|].
      intros m0 Hm0. apply head_subst. apply Hnom. exact Hm0.
    - apply Forall2_map_both. eapply Forall2_impl_in; [|exact Ham].
      intros a b Ha _ Hab. apply arm_matches_subst; [apply Hnom; exact Ha|]. exact Hab.
    - apply Forall2_map_both. eapply Forall2_impl_in; [|exact IH].
      intros a b Ha _ Hab. pose proof (Hab g M Hb) as H. simpl in H.
      rewrite (payload_subst g WF (Hwm a Ha) (Hnom a Ha)) in H. exact H.
  Qed.

  Lemma case_let : forall Phi G x e1 e2 tau1 tau,
    SP Phi G e1 tau1 -> SP Phi ((x, tau1) :: G) e2 tau -> SP Phi G (ELet x e1 e2) tau.
  Proof.
    intros Phi G x e1 e2 tau1 tau IH1 IH2 g M Hb. simpl.
    eapply MTLet; [apply IH1; exact Hb | apply (IH2 g M Hb)].
  Qed.

  Lemma bind_read : forall g t c x,
    grounding g -> ~ In t (map fst g) ->
    app_subst [(t, c)] (app_subst g x) = app_subst ((t, c) :: g) x.
  Proof.
    intros g t c x Hgr Hnt. rewrite app_subst_compose, map_snd_closed by exact Hgr.
    apply app_subst_frees_agree. intros z _. rewrite callee_env_agree_aux8. simpl.
    destruct (Nat.eqb z t) eqn:E.
    - apply Nat.eqb_eq in E. subst z. rewrite (lookup_notin_dom t g Hnt). simpl.
      replace (t =? t) with true by (symmetry; apply Nat.eqb_refl). reflexivity.
    - destruct (lookup_subst z g); [reflexivity|]. simpl.
      replace (z =? t) with false by (symmetry; exact E). reflexivity.
  Qed.

  Lemma case_bindty : forall Phi G t tau' e tau,
    t <> this_sym -> ~ In (ITy t) (cA Phi) -> wf_ty Sg Phi tau' ->
    SP (bind_ty t tau' Phi) G e tau -> wf_ty Sg Phi tau ->
    SP Phi G (EBindTy t tau' e) tau.
  Proof.
    intros Phi G t tau' e tau Hne Hfr Hw IH Hwt g M Hb. simpl.
    pose proof (binding_bindty WF Hne Hfr Hw Hb) as Hb'.
    pose proof (IH _ _ Hb') as H.
    assert (Hnt : ~ In t (map fst g)).
    { intro Hin. apply Hfr. apply safety_case_bindty_aux1. apply (proj1 (proj2 Hb)). exact Hin. }
    assert (Hgr : grounding g) by exact (proj1 (proj1 Hb)).
    apply MTBindTy.
    - eapply ground_closed; [exact (proj1 Hb)|]. eapply wf_ty_frees. exact Hw.
    - replace (app_subst g tau) with (app_subst ((t, app_subst g tau') :: g) tau).
      + replace (map_snd (app_subst [(t, app_subst g tau')]) (map_snd (app_subst g) G))
          with (map_snd (app_subst ((t, app_subst g tau') :: g)) G); [exact H|].
        unfold map_snd. rewrite map_map. apply map_ext. intros [y a]. simpl.
        rewrite bind_read by assumption. reflexivity.
      + apply app_subst_frees_agree. intros z Hz.
        apply safety_case_bindty_aux2 with (A := cA Phi); [exact Hfr|].
        eapply wf_ty_frees; [exact Hwt|exact Hz].
  Qed.

  Lemma sat0_incl : forall Phi D, sat0 Sg Phi D -> incl (tyreqs D) (tyreqs (cA Phi)).
  Proof.
    intros Phi D Hs x Hx. pose proof Hs as [PhiD [Htc _]].
    apply (sat0_tyreqs_incl Hs Htc). apply (proj2 (tele_ctx_tyreqs D Htc x)). exact Hx.
  Qed.

  Lemma case_bindval : forall Phi G v Dv tauv e1 e tau,
    g_val Sg v = Some (Dv, tauv) -> sat0 Sg Phi Dv ->
    SP Phi G e1 (app_subst (cS Phi) tauv) ->
    SP (add_item (IVal v) Phi) G e tau ->
    SP Phi G (EBindVal v e1 e) tau.
  Proof.
    intros Phi G v Dv tauv e1 e tau Hv Hs IH1 IH g M Hb. simpl.
    eapply MTBindVal; [exact Hv | rewrite <- (bo_abs Hb); apply IH1; exact Hb |].
    apply IH. apply binding_push; [exact Hb | | | reflexivity | intros; discriminate].
    - simpl. exists Dv, tauv. split; [exact Hv|reflexivity].
    - simpl. rewrite Hv. rewrite !app_nil_r. apply sat0_incl. exact Hs.
  Qed.

  (* A provider bound at the identity application of its telescope: its
     specialization is a valid unit at the types here. *)
  Lemma provider_unit : forall Phi g M f' D' S' body sm,
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi D' -> smap_ok Phi (id_app D') sm D' ->
    binding_ok Sg Phi g M ->
    exists PhiD' Mc, tele_ctx D' = Some PhiD' /\ wf_tele Sg empty_ctx D' PhiD'
      /\ scompose M sm = Some Mc
      /\ binding_ok Sg PhiD' (restrict g (tyreqs (cA PhiD'))) Mc.
  Proof.
    intros Phi g M f' D' S' body sm Hf' Hs' Hsm Hb.
    destruct (wf_fn_decl WF _ Hf') as [PhiD [Wt' _]].
    pose proof (@wf_tele_ctx Sg _ _ Wt') as Htc.
    destruct Hs' as [PhiD1 [Htc1 [Hsat' Hav']]].
    rewrite Htc in Htc1. injection Htc1 as <-.
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiD))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    assert (Hsat : sat Sg Phi (id_app (cA PhiD)) D') by (exists PhiD; split; [exact Htc | exact Hsat']).
    pose proof (@safety_case_bindmth_aux8 Phi D' (cA PhiD) sm D' Hsm) as Hsm2.
    destruct (@scallee Sg g Phi (id_app (cA PhiD)) sm D' M PhiD WF Wt' Hsat Hwfth Hsm2 Hb)
      as [Mc [Hc Hbc]].
    rewrite callee_grounding_id in Hbc by exact (proj1 (proj2 (proj1 Hb))).
    exists PhiD, Mc. split; [exact Htc|]. split; [exact Wt'|]. split; [exact Hc|exact Hbc].
  Qed.

  Lemma restrict_wf : forall g PhiD tau, wf_ty Sg PhiD tau ->
    app_subst (restrict g (tyreqs (cA PhiD))) tau = app_subst g tau.
  Proof. intros g PhiD tau Hw. apply restrict_reads. eapply wf_ty_frees. exact Hw. Qed.

  Lemma case_bindfn : forall Phi G f Df Sf f' D' S' body sm e tau,
    g_fn Sg f = Some (Df, Sf, None) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    sat0 Sg Phi Df -> sat0 Sg Phi D' -> smap_ok Phi (id_app D') sm D' ->
    sig_eq (cS Phi) S' Sf ->
    SP (add_item (IFn f) Phi) G e tau ->
    SP Phi G (EBindFn f f' sm e) tau.
  Proof.
    intros Phi G f Df Sf f' D' S' body sm e tau Hf Hf' Hsf Hs' Hsm [Hsp Hsr] IH g M Hb. simpl.
    destruct (@provider_unit Phi g M f' D' S' body sm Hf' Hs' Hsm Hb) as [PhiD [Mc [Htc [Wt [Hc Hbc]]]]].
    destruct (wf_fn_decl WF _ Hf') as [PhiD2 [Wt2 [Hps [Hrt _]]]].
    rewrite (@wf_tele_ctx Sg _ _ Wt2) in Htc. injection Htc as <-.
    assert (E1 : map (app_subst (restrict g (tyreqs (cA PhiD2)))) (fs_params S')
                 = map (app_subst g) (fs_params Sf)).
    { transitivity (map (app_subst g) (map (app_subst (cS Phi)) (fs_params S'))).
      + rewrite map_map. apply map_ext_in. intros a Ha. rewrite (bo_abs Hb).
        apply restrict_wf. rewrite Forall_forall in Hps. apply Hps. exact Ha.
      + rewrite Hsp, map_map. apply map_ext. intros a. apply (bo_abs Hb). }
    assert (E2 : app_subst (restrict g (tyreqs (cA PhiD2))) (fs_ret S') = app_subst g (fs_ret Sf)).
    { rewrite (restrict_wf _ Hrt), <- (bo_abs Hb), Hsr. apply (bo_abs Hb). }
    eapply MTBindFn; [exact Hf | exact Hf' | exact (@wf_tele_ctx Sg _ _ Wt2) | exact Hc | exact Hbc
                     | exact E1 | exact E2 |].
    - apply IH. apply binding_push; [exact Hb | | | reflexivity | intros; discriminate].
      + simpl. split; [exists Df, Sf; split; [exact Hf | split; reflexivity] |].
        exists D', S', body, (restrict g (tyreqs (cA PhiD2))).
        split; [exact Hf' |]. split; [exact (proj1 (proj1 Hbc)) |]. split; [exact E1 | exact E2].
      + simpl. rewrite Hf. rewrite !app_nil_r. apply sat0_incl. exact Hsf.
  Qed.

  Lemma case_bindmth : forall Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau,
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
    SP (add_item (IMth tauk m th) Phi) G e tau ->
    SP Phi G (EBindMth tauk m th f' sm e) tau.
  Proof.
    intros Phi G tauk m Dm S th f' D' S' body sm tau0' rest e tau
           Hm Hwk Hsat Hth Hf' Hs' Hsm Hps' Heq0 Hrest Hret IH g M Hb. simpl.
    destruct (@provider_unit Phi g M f' D' S' body sm Hf' Hs' Hsm Hb) as [PhiD [Mc [Htc [Wt [Hc Hbc]]]]].
    destruct (wf_fn_decl WF _ Hf') as [PhiD2 [Wt2 [Hps [Hrt _]]]].
    rewrite (@wf_tele_ctx Sg _ _ Wt2) in Htc. injection Htc as <-.
    destruct (wf_mth_decl WF _ Hm) as [PhiDm [WtDm [HpsM HretM]]].
    pose proof (sat_dom Hsat (@wf_tele_ctx Sg _ _ WtDm)) as Hthdom.
    pose proof Hb as [[Hgr [Habs _]] _].
    assert (Hnth : ~ In this_sym (map fst th)).
    { rewrite Hthdom. apply (@callee_env_agree_aux13 Sg empty_ctx Dm PhiDm WtDm). intro H; exact H. }
    assert (Hran : Forall (fun p => incl (ty_frees (snd p)) (map fst g)) th).
    { eapply Forall_impl; [|exact Hth]. intros p Hp. eapply bo_dom_frees; [exact Hb|].
      eapply wf_ty_frees. exact Hp. }
    assert (Hconv : forall tau1, wf_ty Sg (add_item (ITy this_sym) PhiDm) tau1 ->
              app_subst g (interp_m (cS Phi) th (app_subst (cS Phi) tauk) tau1)
              = mparam (app_subst g tauk) (map_snd (app_subst g) th) tau1).
    { intros tau1 Hw. apply mparam_ground; try assumption.
      - exact (bo_this Hb).
      - apply wf_ty_frees in Hw. simpl in Hw. rewrite Hthdom. exact Hw. }
    assert (Hpin : forall a, In a rest -> wf_ty Sg PhiD2 a).
    { intros a Ha. rewrite Forall_forall in Hps. apply Hps. rewrite Hps'. right. exact Ha. }
    assert (P1 : app_subst (restrict g (tyreqs (cA PhiD2))) tau0' = app_subst g tauk).
    { rewrite restrict_wf.
      + unfold ty_eq in Heq0. rewrite <- (bo_abs Hb), Heq0. apply (bo_abs Hb).
      + rewrite Forall_forall in Hps. apply Hps. rewrite Hps'. left. reflexivity. }
    assert (P2 : map (app_subst (restrict g (tyreqs (cA PhiD2)))) rest
                 = map (mparam (app_subst g tauk) (map_snd (app_subst g) th)) (fs_params S)).
    { transitivity (map (app_subst g) (map (app_subst (cS Phi)) rest)).
      + rewrite map_map. apply map_ext_in. intros a Ha. rewrite (bo_abs Hb).
        apply restrict_wf. apply Hpin. exact Ha.
      + rewrite Hrest, map_map. apply map_ext_in. intros a Ha. apply Hconv.
        rewrite Forall_forall in HpsM. apply HpsM. exact Ha. }
    assert (P3 : app_subst (restrict g (tyreqs (cA PhiD2))) (fs_ret S')
                 = mparam (app_subst g tauk) (map_snd (app_subst g) th) (fs_ret S)).
    { rewrite (restrict_wf _ Hrt), <- (bo_abs Hb), Hret. apply Hconv. exact HretM. }
    (* the provider's signature in dentry_agree's form, for mok *)
    assert (Hconv2 : forall tau1, wf_ty Sg (add_item (ITy this_sym) PhiDm) tau1 ->
              app_subst g (app_subst (this_subst tauk) (app_subst th tau1))
              = mparam (app_subst g tauk) (map_snd (app_subst g) th) tau1).
    { intros tau1 Hw. rewrite <- (Hconv tau1 Hw). unfold interp_m.
      rewrite (absorbs_all Habs).
      change (app_subst g (app_subst (this_subst tauk) (app_subst th tau1)))
        with (interp_m g th tauk tau1).
      change (app_subst g (app_subst (this_subst (app_subst (cS Phi) tauk)) (app_subst th tau1)))
        with (interp_m g th (app_subst (cS Phi) tauk) tau1).
      apply callee_env_agree_aux5. intros z Hz. unfold interp_m. simpl.
      destruct (lookup_subst z th) as [a|] eqn:Ha.
      - assert (Hna : ~ In this_sym (ty_frees a)).
        { intros Hin. destruct Hb as [[_ [_ [_ [_ Hres]]]] _]. apply Hres.
          rewrite Forall_forall in Hth.
          exact (wf_ty_frees (Hth (z, a) (callee_env_agree_aux1 _ _ Ha)) _ Hin). }
        rewrite !(@callee_env_agree_aux14 _ a Hna). reflexivity.
      - simpl. destruct (Nat.eqb z this_sym); [| reflexivity].
        symmetry. apply absorbs_all. exact Habs. }
    eapply MTBindMth with (rt' := app_subst g tauk) (th' := map_snd (app_subst g) th);
      [exact Hm | reflexivity | exact Hf' | exact (@wf_tele_ctx Sg _ _ Wt2) | exact Hc | exact Hbc
      | exact Hps' | exact P1 | exact P2 | exact P3 |].
    - apply IH. apply binding_push; [exact Hb | | | reflexivity |].
      + simpl. split; [reflexivity|]. exists Dm, S. split; [exact Hm |].
        exists D', S', body, (restrict g (tyreqs (cA PhiD2))).
        split; [exact Hf' |]. split; [exact (proj1 (proj1 Hbc)) |].
        split.
        * rewrite Hps'. simpl. rewrite P1, P2. f_equal.
          apply map_ext_in. intros a Ha. symmetry. apply Hconv2.
          rewrite Forall_forall in HpsM. apply HpsM. exact Ha.
        * rewrite P3. symmetry. apply Hconv2. exact HretM.
      + intros u Hu. apply in_app_or in Hu. destruct Hu as [Hu|Hu]; [simpl in Hu; contradiction|].
        apply in_app_or in Hu. destruct Hu as [Hu|Hu].
        * simpl in Hu. apply in_app_or in Hu. destruct Hu as [Hu|Hu].
          -- eapply wf_ty_frees; [exact Hwk|exact Hu].
          -- apply in_flat_map in Hu. destruct Hu as [p [Hp Hu]].
             rewrite Forall_forall in Hth. eapply wf_ty_frees; [exact (Hth p Hp)|exact Hu].
        * eapply safety_case_bindmth_aux_regular; [exact WF|exact Hm|exact Hwk|exact Hsat|exact Hth|exact Hu].
      + intros rt m0 th0 Dm0 Sm0 E Hm0. injection E as <- <- <-.
        rewrite Hm in Hm0. injection Hm0 as <- <-.
        exists PhiDm. split; [exact (@wf_tele_ctx Sg _ _ WtDm) | exact Hthdom].
  Qed.

  Lemma fixed_reg : forall Phi u, Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    incl (dep_tys Sg (IFun u) ++ item_ty_frees (IFun u) ++ method_item_frees Sg (IFun u))
         (tyreqs (cA Phi)).
  Proof.
    intros Phi u Hfx x Hx. simpl in Hx. rewrite app_nil_r in Hx.
    rewrite Forall_forall in Hfx. apply wf_ty_frees_aux3. apply avail_ty_in. exact (Hfx x Hx).
  Qed.

  Lemma case_bindfun : forall Phi G u Du Dr h Dh bs sm smc e tau,
    g_fun Sg u = Some (Du, Dr, None) -> g_fun Sg h = Some (Dh, Dr, Some bs) ->
    fun_link Sg Dh sm Du -> cap_ok Sg Phi Du Dr u h smc ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    SP (add_item (IFun u) Phi) G e tau ->
    SP Phi G (EBindFun u h sm smc e) tau.
  Proof.
    intros Phi G u Du Dr h Dh bs sm smc e tau Hu Hh Hl Hcap Hfx IH g M Hb. simpl.
    eapply MTBindFun; [exact Hu | exact Hh | exact Hl |].
    apply IH. apply binding_push; [exact Hb | | exact (@fixed_reg Phi u Hfx)
                                  | reflexivity | intros; discriminate].
    simpl. exists Du, Dr, Dh, bs. split; [exact Hu|]. split; [exact Hh|].
    split; [eapply ChCons; [exact Hh | exact Hu | exact Hl | apply ChNil] |].
    (* the capture: h's captured inputs as they are here *)
    destruct Hcap as [Hs0 [Hsmc [Hpins Hstat]]].
    destruct (@fun_body_regular Sg h Dh Dr bs WF Hh)
      as [PhiH [PhiC [_ [_ [Wc [[Hpd Hpr] _]]]]]].
    pose proof (@wf_tele_ctx Sg _ _ Wc) as Htc.
    destruct Hs0 as [PhiC1 [Htc1 [Hsat' Hav']]].
    rewrite Htc in Htc1. injection Htc1 as <-.
    assert (Hwfth : Forall (fun p => wf_ty Sg Phi (snd p)) (id_app (cA PhiC))).
    { apply Forall_forall. intros p Hp. unfold id_app in Hp.
      apply in_map_iff in Hp. destruct Hp as [t0 [Heq Hin]]. subst p. simpl.
      apply WfNeed. rewrite Forall_forall in Hav'. apply Hav'. exact Hin. }
    assert (Hsat : sat Sg Phi (id_app (cA PhiC)) (fst (g_cap Sg h)))
      by (exists PhiC; split; [exact Htc | exact Hsat']).
    pose proof (@safety_case_bindmth_aux8 Phi _ (cA PhiC) smc _ Hsmc) as Hsm2.
    destruct (@scallee Sg g Phi (id_app (cA PhiC)) smc _ M PhiC WF Wc Hsat Hwfth Hsm2 Hb)
      as [Mc [_ Hbc]].
    rewrite callee_grounding_id in Hbc by exact (proj1 (proj2 (proj1 Hb))).
    destruct Hbc as [[Hgr [Habs [Hcov _]]] _].
    assert (Hlk : forall x, In x (tyreqs (cA PhiC)) ->
              app_subst (restrict g (tyreqs (cA PhiC))) (TAbs x) = app_subst g (TAbs x)).
    { intros x Hx. apply restrict_reads. intros z [<- | []]. exact Hx. }
    pose proof (bo_abs Hb) as HabsG.
    exists PhiC, (restrict g (tyreqs (cA PhiC))).
    split; [exact Htc |]. split; [exact Hgr |]. split; [exact Hcov |]. split; [exact Habs |].
    split.
    - (* the pins hold where h is bound *)
      intros t0 tau0 Hl0.
      pose proof (callee_env_agree_aux1 _ _ Hl0) as Hin.
      assert (Ht : In t0 (tyreqs (cA PhiC)))
        by (apply Hpd; apply in_map_iff; exists (t0, tau0); split; [reflexivity | exact Hin]).
      assert (Ea : lookup_subst t0 (restrict g (tyreqs (cA PhiC)))
                   = Some (app_subst g (TAbs t0))).
      { unfold restrict. rewrite <- (app_nil_r (map _ _)).
        exact (lookup_prefix_in (fun t1 => app_subst g (TAbs t1)) _ t0 [] Ht). }
      rewrite Ea. f_equal. rewrite restrict_reads.
      + rewrite Forall_forall in Hpins. pose proof (Hpins (t0, tau0) Hin) as Hp.
        cbn [fst snd] in Hp.
        rewrite <- (HabsG (TAbs t0)), Hp. apply HabsG.
      + rewrite Forall_forall in Hpr. exact (Hpr (t0, tau0) Hin).
    - intros x Hx Hxr. split; [| exact (Hlk x Hx)].
      apply Hstat; [apply (proj1 (tele_ctx_tyreqs _ Htc x)); exact Hx |].
      exact (@link_reads_bound Sg WF u Du Dr h Dh bs sm x Hu Hh Hl Hxr).
  Qed.

  Lemma case_bindfuna : forall Phi G u Du Dr w Dw sm e tau,
    g_fun Sg u = Some (Du, Dr, None) -> g_fun Sg w = Some (Dw, Dr, None) ->
    avail Phi (IFun w) -> fun_link Sg Dw sm Du ->
    incl (g_fixed Sg w) (g_fixed Sg u) ->
    Forall (fun x => avail Phi (ITy x)) (g_fixed Sg u) ->
    SP (add_item (IFun u) Phi) G e tau ->
    SP Phi G (EBindFunA u w sm e) tau.
  Proof.
    intros Phi G u Du Dr w Dw sm e tau Hu Hw Hav Hl Hxx Hfx IH g M Hb. simpl.
    destruct (@bo_entry Phi g M (IFun w) Hb (avail_fun_in Hav) eq_refl) as [en [He Hok]].
    destruct en as [| | |h sms]; simpl in Hok; try contradiction.
    destruct Hok as [Dw' [Dr' [Dh [bs [Hw' [Hh [Hch Hcs]]]]]]].
    rewrite Hw in Hw'. injection Hw' as <- <-.
    eapply MTBindFunA; [exact Hu | exact Hw | exact He | exact Hl |].
    apply IH. apply binding_push; [exact Hb | | exact (@fixed_reg Phi u Hfx)
                                  | reflexivity | intros; discriminate].
    simpl. exists Du, Dr, Dh, bs. split; [exact Hu|]. split; [exact Hh|].
    split; [eapply chain_ok_snoc; [exact Hch | exact Hw | exact Hu | exact Hl] |].
    (* the copy keeps the capture; what w holds fixed, u does too *)
    destruct Hcs as [PhiC [gc [H1 [H2 [H3 [H4 [H5 H6]]]]]]].
    exists PhiC, gc. repeat (split; [assumption |]).
    intros x Hx Hxr. destruct (H6 x Hx Hxr) as [HX Heq]. split; [apply Hxx; exact HX | exact Heq].
  Qed.

  Lemma case_apply : forall Phi G u Du Dr th sm e tau,
    g_fun Sg u = Some (Du, Dr, None) ->
    avail Phi (IFun u) ->
    sat Sg Phi th Du ->
    Forall (fun p => wf_ty Sg Phi (snd p)) th ->
    smap_ok Phi th sm Du ->
    fixed_ok Sg Phi u th ->
    res_coherent Sg Phi th Dr ->
    Forall (fun t => ~ In (ITy t) (cA Phi)) (tyreqs Dr) ->
    SP (open_res (app_tele th Dr) Phi) G e tau ->
    SP Phi G (EApply u th sm e) tau.
  Proof.
    intros Phi G u Du Dr th sm e tau Hu Hav Hsat Hthw Hsm Hfx Hcoh Hfr IH g M Hb. simpl.
    destruct (@bo_entry Phi g M (IFun u) Hb (avail_fun_in Hav) eq_refl) as [en [He Hok]].
    destruct en as [| | |h sms]; simpl in Hok; try contradiction.
    destruct (@fun_decl_regular Sg u Du Dr WF Hu) as [PhiU [HtcU [WtU _]]].
    pose proof (sat_dom Hsat HtcU) as Hdom.
    destruct (@scallee Sg g Phi th sm Du M PhiU WF WtU Hsat Hthw Hsm Hb) as [Mu [Hc Hbc]].
    rewrite callee_grounding_restrict in Hbc;
      [| exact (proj1 (proj2 (proj1 Hb))) | rewrite Hdom; intros x Hx; exact Hx].
    eapply MTApply; [exact Hu | exact He | exact HtcU | exact Hc | exact Hbc | |].
    { unfold fixed_ok in Hfx. rewrite Forall_forall in Hfx |- *. intros x Hx.
      rewrite <- (bo_abs Hb (app_subst th (TAbs x))), (Hfx x Hx). apply (bo_abs Hb). }
    intros res Mr Hres Hdomr HMr HMk.
    pose proof (@binding_open_res_th Sg Phi u Du Dr th g M res Mr WF Hu Hsat Hthw Hcoh Hfr Hb
                  Hres Hdomr HMr HMk) as Hb'.
    pose proof (IH _ _ Hb') as H.
    assert (Hgr : grounding g) by exact (proj1 (proj1 Hb)).
    rewrite tsub_spec, (@map_snd_closed res g Hgr).
    replace (app_subst res (app_subst g tau)) with (app_subst (g ++ res) tau)
      by (rewrite app_subst_compose, (@map_snd_closed res g Hgr); reflexivity).
    replace (map_snd (app_subst res) (map_snd (app_subst g) G))
      with (map_snd (app_subst (g ++ res)) G); [exact H|].
    rewrite map_snd_compose, (@map_snd_closed res g Hgr). reflexivity.
  Qed.
End SpecCases.

(* Specialization preserves typing: a generically well-typed expression,
   specialized at any binding of its context, is monomorphically well-typed
   at the bound types. *)
Theorem spec_typed : forall Sg, wf_gsig Sg ->
  forall Phi G e tau, has_ty Sg Phi G e tau ->
  forall g M, binding_ok Sg Phi g M ->
  mty Sg g M (map_snd (app_subst g) G) (spec g e) (app_subst g tau).
Proof.
  intros Sg WF. fix IH 5. intros Phi G e tau H.
  destruct H.
  - intros g M Hb. simpl. apply MTVar. apply lookup_tenv_map. assumption.
  - intros g M Hb. simpl. apply MTUnit.
  - eapply case_sub; [unfold SP; apply IH; eassumption | eassumption].
  - eapply case_val; eassumption.
  - eapply case_tag; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_need; try eassumption.
    match goal with F : Forall2 _ _ _ |- _ =>
      clear - IH F; induction F; constructor; [unfold SP; apply IH; assumption | assumption] end.
  - eapply case_call; try eassumption.
    match goal with F : Forall2 _ _ _ |- _ =>
      clear - IH F; induction F; constructor; [unfold SP; apply IH; assumption | assumption] end.
  - eapply case_meth; try eassumption; [unfold SP; apply IH; assumption|].
    match goal with F : Forall2 _ _ _ |- _ =>
      clear - IH F; induction F; constructor; [unfold SP; apply IH; assumption | assumption] end.
  - eapply case_match; try eassumption; [unfold SP; apply IH; assumption|].
    match goal with F : Forall2 (fun mem arm => has_ty _ _ _ _ _) _ _ |- _ =>
      clear - IH F; induction F; constructor; [unfold SP; apply IH; assumption | assumption] end.
  - eapply case_let; unfold SP; apply IH; eassumption.
  - eapply case_bindty; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_bindval; try eassumption; unfold SP; apply IH; assumption.
  - eapply case_bindfn; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_bindmth; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_bindfun; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_bindfuna; try eassumption. unfold SP; apply IH; assumption.
  - eapply case_apply; try eassumption. unfold SP; apply IH; assumption.
Qed.

(* ===================== 6. Units, reachability, the theorem =============== *)

(* A specialization unit: a function with a body, at a binding — the
   compilers' specialization key (declaration, concrete types, providers). *)
Inductive sunit : Type := UFn : fsym -> subst -> menv -> sunit.

Definition unit_ok (Sg : gsig) (u : sunit) : Prop :=
  match u with
  | UFn f g M => exists Df S body PhiD, g_fn Sg f = Some (Df, S, Some body)
                 /\ wf_tele Sg empty_ctx Df PhiD /\ binding_ok Sg PhiD g M
  end.

(* The unit's specialized body checks monomorphically, at its signature read
   through its binding. *)
Definition unit_mono (Sg : gsig) (u : sunit) : Prop :=
  match u with
  | UFn f g M => exists Df S body, g_fn Sg f = Some (Df, S, Some body)
                 /\ mty Sg g M (map_snd (app_subst g) (body_env S)) (spec g body)
                        (app_subst g (fs_ret S))
  end.

Theorem unit_spec : forall Sg u, wf_gsig Sg -> unit_ok Sg u -> unit_mono Sg u.
Proof.
  intros Sg [f g M] WF [Df [S [body [PhiD [Hf [Wt Hb]]]]]].
  exists Df, S, body. split; [exact Hf|].
  destruct (wf_fn_decl WF _ Hf) as [PhiD' [Wt' [_ [_ Hbody]]]].
  assert (E : PhiD' = PhiD).
  { pose proof (@wf_tele_ctx Sg _ _ Wt) as H1. pose proof (@wf_tele_ctx Sg _ _ Wt') as H2. congruence. }
  subst PhiD'. eapply spec_typed; [exact WF | apply Hbody; reflexivity | exact Hb].
Qed.

(* The units a specialized body invokes: a defined call (the callee at its
   application and re-keyed environment), and a provider bound by a fn or
   method bind (the provider at the types in force and its captured
   environment) — threading binds as the checker does, and under an
   application at every instantiation of its result. *)
Inductive calls (Sg : gsig) : subst -> menv -> expr -> sunit -> Prop :=
| CCall : forall g M f th sm args Df S b PhiD Mc,
    g_fn Sg f = Some (Df, S, Some b) -> tele_ctx Df = Some PhiD ->
    scompose M sm = Some Mc ->
    calls Sg g M (ECallD f th sm args) (UFn f (restrict th (tyreqs (cA PhiD))) Mc)
| CCallArg : forall g M f th sm args a u,
    In a args -> calls Sg g M a u -> calls Sg g M (ECallD f th sm args) u
| CNeedArg : forall g M f args a u,
    In a args -> calls Sg g M a u -> calls Sg g M (ECallA f args) u
| CTag : forall g M n th e u, calls Sg g M e u -> calls Sg g M (ETag n th e) u
| CMeth0 : forall g M e0 m prov args u,
    calls Sg g M e0 u -> calls Sg g M (EMeth e0 m prov args) u
| CMethArg : forall g M e0 m prov args a u,
    In a args -> calls Sg g M a u -> calls Sg g M (EMeth e0 m prov args) u
| CMatch0 : forall g M e0 arms u, calls Sg g M e0 u -> calls Sg g M (EMatch e0 arms) u
| CMatchArm : forall g M e0 arms a u,
    In a arms -> calls Sg g M (arm_body a) u -> calls Sg g M (EMatch e0 arms) u
| CLet1 : forall g M x e1 e2 u, calls Sg g M e1 u -> calls Sg g M (ELet x e1 e2) u
| CLet2 : forall g M x e1 e2 u, calls Sg g M e2 u -> calls Sg g M (ELet x e1 e2) u
| CBindTy : forall g M t c e u,
    calls Sg ((t, c) :: g) M e u -> calls Sg g M (EBindTy t c e) u
| CBindVal1 : forall g M v e1 e u, calls Sg g M e1 u -> calls Sg g M (EBindVal v e1 e) u
| CBindVal2 : forall g M v Dv tv e1 e u,
    g_val Sg v = Some (Dv, tv) ->
    calls Sg g ((IVal v, MVal (app_subst g tv)) :: M) e u ->
    calls Sg g M (EBindVal v e1 e) u
| CBindFnP : forall g M f f' sm e D' S' b PhiD' Mc,
    g_fn Sg f' = Some (D', S', Some b) -> tele_ctx D' = Some PhiD' ->
    scompose M sm = Some Mc ->
    calls Sg g M (EBindFn f f' sm e) (UFn f' (restrict g (tyreqs (cA PhiD'))) Mc)
| CBindFnS : forall g M f f' sm e Df Sf u,
    g_fn Sg f = Some (Df, Sf, None) ->
    calls Sg g ((IFn f, MFn f' (map (app_subst g) (fs_params Sf))
                               (app_subst g (fs_ret Sf))) :: M) e u ->
    calls Sg g M (EBindFn f f' sm e) u
| CBindMthP : forall g M tk m th f' sm e D' S' b PhiD' Mc,
    g_fn Sg f' = Some (D', S', Some b) -> tele_ctx D' = Some PhiD' ->
    scompose M sm = Some Mc ->
    calls Sg g M (EBindMth tk m th f' sm e) (UFn f' (restrict g (tyreqs (cA PhiD'))) Mc)
| CBindMthS : forall g M tk m th f' sm e u,
    calls Sg g ((IMth tk m th, MMth f' (app_subst_item g (IMth tk m th))) :: M) e u ->
    calls Sg g M (EBindMth tk m th f' sm e) u
| CBindFun : forall g M u0 h sm smc e u,
    calls Sg g ((IFun u0, MFun h [sm]) :: M) e u -> calls Sg g M (EBindFun u0 h sm smc e) u
| CBindFunA : forall g M u0 w sm e h sms u,
    lookup_menv (IFun w) M = Some (MFun h sms) ->
    calls Sg g ((IFun u0, MFun h (sms ++ [sm])) :: M) e u ->
    calls Sg g M (EBindFunA u0 w sm e) u
| CApply : forall g M u0 th sm e Du Dr res Mr u,
    g_fun Sg u0 = Some (Du, Dr, None) ->
    grounding res -> map fst res = tyreqs Dr ->
    mreal Sg (g ++ res) (app_tele th Dr) Mr ->
    Forall (fun p => In (fst p) (app_tele th Dr)) Mr ->
    calls Sg (g ++ res) (Mr ++ M) (tsub res e) u ->
    calls Sg g M (EApply u0 th sm e) u.

(* What a monomorphic derivation guarantees of each form, whatever its
   type (subsumption peeled off). *)
Definition shape (Sg : gsig) (g : subst) (M : menv) (G : tenv) (e : expr) : Prop :=
  match e with
  | ETag _ _ e1 => exists t, mty Sg g M G e1 t
  | ECallD f th sm args =>
      (exists ts, Forall2 (mty Sg g M G) args ts)
      /\ (forall Df S b PhiD Mc, g_fn Sg f = Some (Df, S, Some b) ->
            tele_ctx Df = Some PhiD -> scompose M sm = Some Mc ->
            binding_ok Sg PhiD (restrict th (tyreqs (cA PhiD))) Mc)
  | ECallA _ args => exists ts, Forall2 (mty Sg g M G) args ts
  | EMeth e0 _ _ args =>
      (exists t, mty Sg g M G e0 t) /\ (exists ts, Forall2 (mty Sg g M G) args ts)
  | EMatch e0 arms =>
      (exists t, mty Sg g M G e0 t)
      /\ (forall a, In a arms -> exists G' t, mty Sg g M G' (arm_body a) t)
  | ELet _ e1 e2 =>
      (exists t, mty Sg g M G e1 t) /\ (exists G' t, mty Sg g M G' e2 t)
  | EBindTy t c e1 => exists G' t', mty Sg ((t, c) :: g) M G' e1 t'
  | EBindVal v e1 e2 =>
      (exists t, mty Sg g M G e1 t)
      /\ (forall Dv tv, g_val Sg v = Some (Dv, tv) ->
            exists t, mty Sg g ((IVal v, MVal (app_subst g tv)) :: M) G e2 t)
  | EBindFn f f' sm e1 =>
      (forall D' S' b PhiD' Mc, g_fn Sg f' = Some (D', S', Some b) ->
         tele_ctx D' = Some PhiD' -> scompose M sm = Some Mc ->
         binding_ok Sg PhiD' (restrict g (tyreqs (cA PhiD'))) Mc)
      /\ (forall Df Sf, g_fn Sg f = Some (Df, Sf, None) ->
            exists t, mty Sg g ((IFn f, MFn f' (map (app_subst g) (fs_params Sf))
                                               (app_subst g (fs_ret Sf))) :: M) G e1 t)
  | EBindMth tk m th f' sm e1 =>
      (forall D' S' b PhiD' Mc, g_fn Sg f' = Some (D', S', Some b) ->
         tele_ctx D' = Some PhiD' -> scompose M sm = Some Mc ->
         binding_ok Sg PhiD' (restrict g (tyreqs (cA PhiD'))) Mc)
      /\ (exists t, mty Sg g ((IMth tk m th, MMth f' (app_subst_item g (IMth tk m th))) :: M)
                        G e1 t)
  | EBindFun u h sm smc e1 => exists t, mty Sg g ((IFun u, MFun h [sm]) :: M) G e1 t
  | EBindFunA u w sm e1 =>
      forall h sms, lookup_menv (IFun w) M = Some (MFun h sms) ->
        exists t, mty Sg g ((IFun u, MFun h (sms ++ [sm])) :: M) G e1 t
  | EApply u th sm e1 =>
      forall Du Dr res Mr, g_fun Sg u = Some (Du, Dr, None) ->
        grounding res -> map fst res = tyreqs Dr ->
        mreal Sg (g ++ res) (app_tele th Dr) Mr ->
        Forall (fun p => In (fst p) (app_tele th Dr)) Mr ->
        exists G' t, mty Sg (g ++ res) (Mr ++ M) G' (tsub res e1) t
  | _ => True
  end.

Lemma Forall2_in_r : forall (A B : Type) (R : A -> B -> Prop) l1 l2 b,
  Forall2 R l1 l2 -> In b l2 -> exists a, In a l1 /\ R a b.
Proof.
  intros A B R l1 l2 b H. induction H; intros Hb; [destruct Hb|].
  destruct Hb as [<-|Hb]; [exists x; split; [left; reflexivity|assumption]|].
  destruct (IHForall2 Hb) as [a [Ha Hr]]. exists a. split; [right; exact Ha|exact Hr].
Qed.

Lemma mty_shape : forall Sg g M G e tau, mty Sg g M G e tau -> shape Sg g M G e.
Proof.
  intros Sg g M G e tau H. induction H; simpl; try exact I; try assumption.
  - eexists; eassumption.
  - eexists; eassumption.
  - split; [eexists; eassumption|].
    intros Df0 S0 b0 PhiD0 Mc0 Hf Htc Hc. congruence.
  - split; eexists; eassumption.
  - split; [eexists; eassumption|]. intros a Ha.
    destruct (@Forall2_in_r _ _ _ ms arms a H4 Ha) as [mem [_ Hm]]. do 2 eexists. exact Hm.
  - split; [eexists; eassumption | do 2 eexists; eassumption].
  - do 2 eexists; eassumption.
  - split; [eexists; eassumption|]. intros Dv' tv' Hv'. rewrite H in Hv'.
    injection Hv' as <- <-. eexists; eassumption.
  - split.
    + intros D0 S0 b0 PhiD0 Mc0 Hf Htc Hc. congruence.
    + intros Df0 Sf0 Hf. rewrite H in Hf. injection Hf as <- <-. eexists; eassumption.
  - split.
    + intros D0 S0 b0 PhiD0 Mc0 Hf Htc Hc. congruence.
    + simpl in H0 |- *. rewrite H0. eexists; eassumption.
  - eexists; eassumption.
  - intros h0 sms0 Hl. rewrite H1 in Hl. injection Hl as <- <-. eexists; eassumption.
  - intros Du0 Dr0 res Mr Hu Hres Hdom HMr HMk. rewrite H in Hu. injection Hu as <- <-.
    do 2 eexists.
    match goal with
    | Hh : forall res Mr, grounding res -> _ -> _ -> _ -> mty _ _ _ _ _ _ |- _ =>
        apply Hh; assumption
    end.
Qed.

Lemma Forall2_in_l : forall (A B : Type) (R : A -> B -> Prop) l1 l2 a,
  Forall2 R l1 l2 -> In a l1 -> exists b, In b l2 /\ R a b.
Proof.
  intros A B R l1 l2 a H. induction H; intros Ha; [destruct Ha|].
  destruct Ha as [<-|Ha]; [exists y; split; [left; reflexivity|assumption]|].
  destruct (IHForall2 Ha) as [b [Hb Hr]]. exists b. split; [right; exact Hb|exact Hr].
Qed.

Lemma unit_ok_of : forall Sg f g M Df S b PhiD,
  wf_gsig Sg -> g_fn Sg f = Some (Df, S, Some b) -> tele_ctx Df = Some PhiD ->
  binding_ok Sg PhiD g M -> unit_ok Sg (UFn f g M).
Proof.
  intros Sg f g M Df S b PhiD WF Hf Htc Hb.
  destruct (wf_fn_decl WF _ Hf) as [PhiD' [Wt _]].
  rewrite (@wf_tele_ctx Sg _ _ Wt) in Htc. injection Htc as <-.
  exists Df, S, b, PhiD'. split; [exact Hf|]. split; [exact Wt|exact Hb].
Qed.

(* Every unit a monomorphically checked body invokes is a valid binding. *)
Lemma calls_ok : forall Sg, wf_gsig Sg ->
  forall g M e u, calls Sg g M e u ->
  forall G tau, mty Sg g M G e tau -> unit_ok Sg u.
Proof.
  intros Sg WF g M e u Hc.
  induction Hc; intros G0 tau0 Hm; apply mty_shape in Hm; simpl in Hm.
  - destruct Hm as [_ Hb]. eapply unit_ok_of; [exact WF|exact H|exact H0|].
    eapply Hb; eassumption.
  - destruct Hm as [[ts Hts] _]. destruct (@Forall2_in_l _ _ _ _ _ a Hts H) as [t [_ Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [ts Hts]. destruct (@Forall2_in_l _ _ _ _ _ a Hts H) as [t [_ Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [t Ht]. eapply IHHc. exact Ht.
  - destruct Hm as [[t Ht] _]. eapply IHHc. exact Ht.
  - destruct Hm as [_ [ts Hts]]. destruct (@Forall2_in_l _ _ _ _ _ a Hts H) as [t [_ Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [[t Ht] _]. eapply IHHc. exact Ht.
  - destruct Hm as [_ Ha]. destruct (Ha a H) as [G' [t Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [[t Ht] _]. eapply IHHc. exact Ht.
  - destruct Hm as [_ [G' [t Ht]]]. eapply IHHc. exact Ht.
  - destruct Hm as [G' [t0 Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [[t Ht] _]. eapply IHHc. exact Ht.
  - destruct Hm as [_ Hv]. destruct (Hv _ _ H) as [t Ht]. eapply IHHc. exact Ht.
  - destruct Hm as [Hp _]. eapply unit_ok_of; [exact WF|exact H|exact H0|]. eapply Hp; eassumption.
  - destruct Hm as [_ Hs]. destruct (Hs _ _ H) as [t Ht]. eapply IHHc. exact Ht.
  - destruct Hm as [Hp _]. eapply unit_ok_of; [exact WF|exact H|exact H0|]. eapply Hp; eassumption.
  - destruct Hm as [_ [t Ht]]. eapply IHHc. exact Ht.
  - destruct Hm as [t Ht]. eapply IHHc. exact Ht.
  - destruct (Hm _ _ H) as [t Ht]. eapply IHHc. exact Ht.
  - destruct (Hm _ _ _ _ H H0 H1 H2 H3) as [G' [t Ht]]. eapply IHHc. exact Ht.
Qed.

(* Reachability from main: main at the empty binding, and every unit a
   reachable unit's specialized body invokes. *)
Inductive reach (Sg : gsig) (main : fsym) : sunit -> Prop :=
| RMain : reach Sg main (UFn main [] [])
| RStep : forall f g M Df S body u,
    reach Sg main (UFn f g M) ->
    g_fn Sg f = Some (Df, S, Some body) ->
    calls Sg g M (spec g body) u ->
    reach Sg main u.

Lemma binding_empty : forall Sg, binding_ok Sg empty_ctx [] [].
Proof.
  intros Sg. split; [| split; [| split; [| split]]].
  - split; [constructor|]. split; [intros t tau Hl; discriminate Hl|].
    split; [intros t Hin; destruct Hin|].
    split; [split; [intros t Hin; destruct Hin | constructor] | intros Hin; destruct Hin].
  - intros x Hx; destruct Hx.
  - constructor.
  - constructor.
  - intros rt m th Dm Sm Hin. destruct Hin.
Qed.

(* ---- The specialization theorem [D16]. ----------------------------------
   In a well-formed program, every unit reachable from main — every function
   at every binding some chain of calls and provider binds reaches, through
   applications of abstract functors at every instantiation of their
   results — is a valid binding of its function's telescope, and its
   specialized body is monomorphically well-typed: ground annotations,
   types as bound, every requirement resolved through the specialized
   environment to one provider of the right concrete signature, distinct
   union heads, and each unit it invokes valid in turn.  No check made
   after specialization can fail. *)
Theorem specialization : forall Sg main u,
  wf_prog Sg main -> reach Sg main u -> unit_ok Sg u /\ unit_mono Sg u.
Proof.
  intros Sg main u [WF [body Hmain]] Hr.
  assert (Hok : unit_ok Sg u).
  { induction Hr as [|f g M Df S b u Hr IH Hf Hc].
    - exists [], (mkSig [] TUnit), body, empty_ctx. split; [exact Hmain|].
      split; [constructor | apply binding_empty].
    - destruct (@unit_spec Sg (UFn f g M) WF IH) as [Df' [S' [b' [Hf' Hm]]]].
      rewrite Hf in Hf'. injection Hf' as <- <- <-.
      eapply calls_ok; [exact WF | exact Hc | exact Hm]. }
  split; [exact Hok | exact (@unit_spec Sg u WF Hok)].
Qed.

(* ===================== 7. Examples ======================================= *)

(* ---- One generic function, specialized at two bindings. -----------------

       tag A; tag B;  type E;  assume E { tag Box(E); }
       fn .dflt(): This;                       # a detached method
       fn a_dflt(x: A): A { x }   fn b_dflt(x: B): B { x }
       type T;
       assume T, T.dflt {
         fn wrap(x: T): Box[E=T] { Box(x.dflt()) }
       }
       fn main() {
         bind A.dflt = a_dflt;  bind B.dflt = b_dflt;
         let _ = wrap[T=A](A);  let _ = wrap[T=B](B);
       }

   `wrap` is checked once; main reaches it at T=A (its provision resolved
   to a_dflt) and at T=B (to b_dflt), and both specializations are computed
   below and covered by the theorem. *)
Module SpecExample.
  Definition sT : tsym := 1.
  Definition sE : tsym := 2.
  Definition nA : nsym := 1.
  Definition nB : nsym := 2.
  Definition nBox : nsym := 3.
  Definition mD : msym := 1.
  Definition fA : fsym := 1.
  Definition fB : fsym := 2.
  Definition fWrap : fsym := 3.
  Definition fMain : fsym := 4.

  Definition tA : ty := TNom nA [].
  Definition tB : ty := TNom nB [].
  Definition prov : item := IMth (TAbs sT) mD [].
  Definition wrap_tele : tele := [ITy sT; prov].
  Definition wrap_body : expr :=
    ETag nBox [(sE, TAbs sT)] (EMeth (EVar 0) mD prov []).
  Definition call_at (t : ty) (n : nsym) : expr :=
    ECallD fWrap [(sT, t)] [(prov, IMth t mD [])] [ETag n [] EUnit].
  Definition main_body : expr :=
    EBindMth tA mD [] fA []
    (EBindMth tB mD [] fB []
    (ELet 10 (call_at tA nA)
    (ELet 11 (call_at tB nB) EUnit))).

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sT || Nat.eqb t sE || Nat.eqb t this_sym)
    (fun n => if Nat.eqb n nA then Some ([], TUnit)
              else if Nat.eqb n nB then Some ([], TUnit)
              else if Nat.eqb n nBox then Some ([ITy sE], TAbs sE)
              else None)
    (fun _ => None)
    (fun f => if Nat.eqb f fA then Some ([], mkSig [tA] tA, Some (EVar 0))
              else if Nat.eqb f fB then Some ([], mkSig [tB] tB, Some (EVar 0))
              else if Nat.eqb f fWrap then
                Some (wrap_tele, mkSig [TAbs sT] (TNom nBox [(sE, TAbs sT)]), Some wrap_body)
              else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some main_body)
              else None)
    (fun m => if Nat.eqb m mD then Some ([], mkSig [] (TAbs this_sym)) else None)
    (fun _ => None)
     (fun _ => []) (fun _ => []) (fun _ => ([], [])).

  (* The two specializations of `wrap` main reaches, computed: the
     annotation is concrete, the provision key is the generic label, and the
     specialized environment resolves it to each binding's provider. *)
  Definition PhiW : ctx := mkCtx wrap_tele [].
  Definition MA : menv := [(prov, MMth fA (IMth tA mD []))].
  Definition MB : menv := [(prov, MMth fB (IMth tB mD []))].

  Example wrap_at_A :
    spec [(sT, tA)] wrap_body = ETag nBox [(sE, tA)] (EMeth (EVar 0) mD prov []).
  Proof. reflexivity. Qed.
  Example wrap_at_B :
    spec [(sT, tB)] wrap_body = ETag nBox [(sE, tB)] (EMeth (EVar 0) mD prov []).
  Proof. reflexivity. Qed.
  Example resolved_A : lookup_menv prov MA = Some (MMth fA (IMth tA mD [])).
  Proof. reflexivity. Qed.
  Example resolved_B : lookup_menv prov MB = Some (MMth fB (IMth tB mD [])).
  Proof. reflexivity. Qed.

  (* ... and they are what main's own specialization invokes. *)
  Lemma reach_A : reach Sg fMain (UFn fWrap [(sT, tA)] MA).
  Proof.
    eapply RStep; [apply RMain | reflexivity |]. simpl.
    apply CBindMthS. apply CBindMthS. apply CLet1.
    change (UFn fWrap [(sT, tA)] MA) with (UFn fWrap (restrict [(sT, tA)] (tyreqs (cA PhiW))) MA).
    eapply CCall; reflexivity.
  Qed.
  Lemma reach_B : reach Sg fMain (UFn fWrap [(sT, tB)] MB).
  Proof.
    eapply RStep; [apply RMain | reflexivity |]. simpl.
    apply CBindMthS. apply CBindMthS. apply CLet2. apply CLet1.
    change (UFn fWrap [(sT, tB)] MB) with (UFn fWrap (restrict [(sT, tB)] (tyreqs (cA PhiW))) MB).
    eapply CCall; reflexivity.
  Qed.
  Lemma reach_provider : reach Sg fMain (UFn fA [] []).
  Proof.
    eapply RStep; [apply RMain | reflexivity |]. simpl.
    change (UFn fA [] []) with (UFn fA (restrict [] (tyreqs (cA empty_ctx))) []).
    eapply CBindMthP; reflexivity.
  Qed.

  (* ---- The program is well-formed. ---- *)

  Lemma conv : forall Phi G e t t', has_ty Sg Phi G e t -> t = t' -> has_ty Sg Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Lemma sat0_nil : forall Phi, sat0 Sg Phi [].
  Proof.
    intros Phi. exists empty_ctx. split; [reflexivity|].
    split; [split; [reflexivity | split; constructor] | constructor].
  Qed.

  Lemma wf_unit_tag : forall Phi n, n = nA \/ n = nB -> wf_ty Sg Phi (TNom n []).
  Proof.
    intros Phi n Hn. eapply WfTag with (D := []) (tau0 := TUnit);
      [destruct Hn as [-> | ->]; reflexivity | | constructor].
    exists empty_ctx. split; [reflexivity|]. split; [reflexivity | split; constructor].
  Qed.

  Lemma tag_unit : forall Phi G n, n = nA \/ n = nB ->
    has_ty Sg Phi G (ETag n [] EUnit) (TNom n []).
  Proof.
    intros Phi G n Hn. eapply conv.
    - eapply TTagR with (D := []) (tau0 := TUnit); [destruct Hn as [-> | ->]; reflexivity | | constructor |].
      + exists empty_ctx. split; [reflexivity|]. split; [reflexivity | split; constructor].
      + apply conv with (t := TUnit); [constructor | reflexivity].
    - destruct Phi. reflexivity.
  Qed.


  Lemma wrap_typed :
    has_ty Sg PhiW (body_env (mkSig [TAbs sT] (TNom nBox [(sE, TAbs sT)]))) wrap_body
           (TNom nBox [(sE, TAbs sT)]).
  Proof.
    eapply conv.
    - eapply TTagR with (D := [ITy sE]) (tau0 := TAbs sE); [reflexivity | | |].
      + eexists. split; [reflexivity|]. split; [reflexivity|].
        split; [repeat constructor; left; reflexivity | constructor].
      + constructor; [apply WfNeed; reflexivity | constructor].
      + eapply conv.
        * eapply TMethR with (Dm := []) (S := mkSig [] (TAbs this_sym)) (tau0 := TAbs sT);
            [reflexivity | constructor; reflexivity | right; left; reflexivity | reflexivity | | constructor].
          intros i1 Hin [th' Hp]. simpl in Hin.
          destruct Hin as [<- | [<- | []]]; [discriminate | reflexivity].
        * reflexivity.
    - reflexivity.
  Qed.

  Definition Phi1 : ctx := add_item (IMth tA mD []) empty_ctx.
  Definition Phi2 : ctx := add_item (IMth tB mD []) Phi1.

  Lemma call_typed : forall G t n, (n = nA /\ t = tA) \/ (n = nB /\ t = tB) ->
    has_ty Sg Phi2 G (call_at t n) (TNom nBox [(sE, t)]).
  Proof.
    intros G t n Hnt.
    assert (Hin : In (IMth t mD []) (cA Phi2))
      by (destruct Hnt as [[-> ->] | [-> ->]]; simpl; tauto).
    assert (Hwt : wf_ty Sg Phi2 t)
      by (destruct Hnt as [[-> ->] | [-> ->]]; apply wf_unit_tag; tauto).
    assert (Hn : n = nA \/ n = nB) by (destruct Hnt as [[-> _] | [-> _]]; tauto).
    assert (Ht : app_subst [] t = t) by apply app_subst_nil.
    assert (Htn : t = TNom n []) by (destruct Hnt as [[-> ->] | [-> ->]]; reflexivity).
    eapply conv.
    - eapply TCallR with (Df := wrap_tele) (S := mkSig [TAbs sT] (TNom nBox [(sE, TAbs sT)]))
                         (body := wrap_body); [reflexivity | | | |].
      + exists PhiW. split; [reflexivity|]. split; [reflexivity|].
        split; [| constructor].
        constructor; [left; reflexivity|]. constructor; [|constructor].
        right. split; [| constructor].
        unfold avail, avail_b. apply existsb_exists. exists (IMth t mD []).
        split; [exact Hin|]. unfold item_subst_eqb. apply item_eqb_eq. simpl. rewrite Ht. reflexivity.
      + constructor; [exact Hwt | constructor].
      + exists PhiW. split; [reflexivity|]. split; [reflexivity|].
        constructor; [|constructor]. split; [exact Hin|].
        unfold item_matches. simpl. rewrite Ht. reflexivity.
      + constructor; [|constructor]. eapply conv.
        * apply tag_unit. exact Hn.
        * simpl. rewrite Ht. symmetry. exact Htn.
    - simpl. rewrite Ht. reflexivity.
  Qed.

  Lemma main_typed : has_ty Sg empty_ctx (body_env (mkSig [] TUnit)) main_body TUnit.
  Proof.
    assert (Hmth : forall t n Phi, n = nA /\ t = tA \/ n = nB /\ t = tB ->
               forall f e, g_fn Sg f = Some ([], mkSig [t] t, Some (EVar 0)) ->
               has_ty Sg (add_item (IMth t mD []) Phi) [] e TUnit ->
               has_ty Sg Phi [] (EBindMth t mD [] f [] e) TUnit).
    { intros t n Phi Hnt f e Hf He.
      eapply TBindMthR with (Dm := []) (S := mkSig [] (TAbs this_sym)) (D' := [])
                            (S' := mkSig [t] t) (tau0' := t) (rest := []);
        [reflexivity | | | constructor | exact Hf | apply sat0_nil | | reflexivity
        | reflexivity | reflexivity | | exact He].
      - destruct Hnt as [[-> ->] | [-> ->]]; apply wf_unit_tag; tauto.
      - exists empty_ctx. split; [reflexivity|]. split; [reflexivity|]. split; constructor.
      - exists empty_ctx. split; [reflexivity|]. split; [reflexivity|constructor].
      - destruct Hnt as [[-> ->] | [-> ->]]; reflexivity. }
    eapply Hmth with (n := nA); [left; split; reflexivity | reflexivity |].
    eapply Hmth with (n := nB); [right; split; reflexivity | reflexivity |].
    eapply TLetR; [apply call_typed; left; split; reflexivity|].
    eapply TLetR; [| constructor].
    eapply conv; [apply call_typed; right; split; reflexivity | reflexivity].
  Qed.

  Lemma wf_sig : wf_gsig Sg.
  Proof.
    constructor.
    - intros n D taup H.
      destruct n as [|[|[|[|n]]]]; simpl in H; try discriminate; injection H as <- <-.
      + exists empty_ctx. split; constructor.
      + exists empty_ctx. split; constructor.
      + exists (mkCtx [ITy sE] []). split.
        * eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
        * apply WfNeed. reflexivity.
    - intros v D tauv H. discriminate.
    - intros f D S b H.
      destruct f as [|[|[|[|[|f]]]]]; simpl in H; try discriminate; injection H as <- <- <-.
      + exists empty_ctx. split; [constructor|].
        split; [constructor; [apply wf_unit_tag; tauto | constructor]|].
        split; [apply wf_unit_tag; tauto|].
        intros body Hb. injection Hb as <-. constructor. reflexivity.
      + exists empty_ctx. split; [constructor|].
        split; [constructor; [apply wf_unit_tag; tauto | constructor]|].
        split; [apply wf_unit_tag; tauto|].
        intros body Hb. injection Hb as <-. constructor. reflexivity.
      + exists PhiW. split.
        * eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity |].
          eapply WCons; [| reflexivity | constructor].
          eapply OkMth with (Dm := []) (S := mkSig [] (TAbs this_sym)) (th := []);
            [reflexivity | apply WfNeed; reflexivity | | constructor].
          exists empty_ctx. split; [reflexivity|]. split; [reflexivity|]. split; constructor.
        * split; [constructor; [apply WfNeed; reflexivity | constructor]|].
          split.
          -- eapply WfTag with (D := [ITy sE]) (tau0 := TAbs sE); [reflexivity | |].
             ++ eexists. split; [reflexivity|]. split; [reflexivity|].
                split; [repeat constructor; left; reflexivity | constructor].
             ++ constructor; [apply WfNeed; reflexivity | constructor].
          -- intros body Hb. injection Hb as <-. exact wrap_typed.
      + exists empty_ctx. split; [constructor|]. split; [constructor|]. split; [constructor|].
        intros body Hb. injection Hb as <-. exact main_typed.
    - intros m D S H. destruct m as [|[|m]]; simpl in H; try discriminate; injection H as <- <-.
      exists empty_ctx. split; [constructor|]. split; [constructor | apply WfNeed; reflexivity].
    - intros u Du Dr ob H. discriminate.
  Qed.

  Lemma wf : wf_prog Sg fMain.
  Proof. split; [exact wf_sig | eexists; reflexivity]. Qed.
End SpecExample.

(* Both specializations of `wrap`, and the provider's, are valid units whose
   specialized bodies check monomorphically. *)
Corollary spec_example_two_bindings :
  unit_mono SpecExample.Sg (UFn SpecExample.fWrap [(SpecExample.sT, SpecExample.tA)] SpecExample.MA)
  /\ unit_mono SpecExample.Sg (UFn SpecExample.fWrap [(SpecExample.sT, SpecExample.tB)] SpecExample.MB)
  /\ unit_mono SpecExample.Sg (UFn SpecExample.fA [] []).
Proof.
  split; [|split].
  - exact (proj2 (specialization SpecExample.wf SpecExample.reach_A)).
  - exact (proj2 (specialization SpecExample.wf SpecExample.reach_B)).
  - exact (proj2 (specialization SpecExample.wf SpecExample.reach_provider)).
Qed.

(* ---- One generic function, two functor bindings [D78]. ------------------
   §12c's `roundtrip` (generic over Boxed and an abstract allocator Alloc,
   applied inside it) is reached from main at Boxed=O twice: once with Alloc
   bound to Wrap, once to Bare copied through Alloc2.  Same types, different
   specialized environments; and at each binding's actual result the code
   after `bind Alloc;` checks with Handle and the providers made concrete —
   Box[O] with box_it/unbox, or O with id/id. *)
Module FunctorSpec.
  Import FunctorExample.
  Definition gO : subst := [(sBoxed, TNom nO [])].
  Definition MWrap : menv := [(IFun uAlloc, MFun uWrap [[]])].
  Definition MBare : menv := [(IFun uAlloc, MFun uBare [[]; []])].
  Definition PhiR' : ctx := mkCtx [ITy sBoxed; IFun uAlloc] [].

  Lemma reach_wrap : reach Sg fMain (UFn fRound gO MWrap).
  Proof.
    eapply RStep; [apply RMain | reflexivity |]. simpl.
    apply CLet1. apply CBindTy. apply CBindFun. apply CLet1.
    change (UFn fRound gO MWrap)
      with (UFn fRound (restrict [(sBoxed, TNom nO [])] (tyreqs (cA PhiR'))) MWrap).
    eapply CCall; reflexivity.
  Qed.

  Lemma reach_bare : reach Sg fMain (UFn fRound gO MBare).
  Proof.
    eapply RStep; [apply RMain | reflexivity |]. simpl.
    apply CLet1. apply CBindTy. apply CBindFun. apply CLet2. apply CBindFun.
    eapply CBindFunA; [reflexivity|].
    change (UFn fRound gO MBare)
      with (UFn fRound (restrict [(sBoxed, TNom nO [])] (tyreqs (cA PhiR'))) MBare).
    eapply CCall; reflexivity.
  Qed.

  (* The result instantiations each binding actually makes (Wrap binds
     Handle = Box[Elem=Boxed]; Bare binds Handle = Boxed), with the
     providers its body binds. *)
  Definition boxO : ty := TNom nBox [(sElem, TNom nO [])].
  Definition res_wrap : subst := [(sHandle, boxO)].
  Definition Mr_wrap : menv :=
    [(IFn fAlloc, MFn fBoxIt [TNom nO []] boxO);
     (load_item, MMth fUnbox (IMth boxO mLoad [(sBoxed, TNom nO [])]))].
  Definition res_bare : subst := [(sHandle, TNom nO [])].
  Definition Mr_bare : menv :=
    [(IFn fAlloc, MFn fId [TNom nO []] (TNom nO []));
     (load_item, MMth fId (IMth (TNom nO []) mLoad [(sBoxed, TNom nO [])]))].

  Lemma scope_checks : forall res Mr,
    (res = res_wrap /\ Mr = Mr_wrap) \/ (res = res_bare /\ Mr = Mr_bare) ->
    forall M, (M = MWrap \/ M = MBare) ->
    exists G' t, mty Sg (gO ++ res) (Mr ++ M) G'
                     (tsub res (EMeth (ECallA fAlloc [EVar 0]) mLoad load_item [])) t.
  Proof.
    intros res Mr Hrm M HM.
    assert (Hu : unit_mono Sg (UFn fRound gO M)).
    { destruct HM as [-> | ->];
        [exact (proj2 (specialization functor_example_ok reach_wrap))
        |exact (proj2 (specialization functor_example_ok reach_bare))]. }
    destruct Hu as [Df [S [body [Hf Hm]]]]. simpl in Hf. injection Hf as <- <- <-.
    apply mty_shape in Hm. simpl in Hm.
    eapply (Hm [ITy sBoxed] alloc_res res Mr eq_refl).
    - destruct Hrm as [[-> ->] | [-> ->]]; (constructor; [reflexivity | constructor]).
    - destruct Hrm as [[-> ->] | [-> ->]]; reflexivity.
    - destruct Hrm as [[-> ->] | [-> ->]]; unfold mreal;
        (constructor; [left; reflexivity|]);
        (constructor;
           [right; eexists; split; [reflexivity|]; simpl; split;
            [do 2 eexists; split; [reflexivity | split; reflexivity]
            | eexists _, _, _, [(sBoxed, TNom nO [])];
              split; [reflexivity | split; [repeat constructor | split; reflexivity]]]
           |]);
        (constructor;
           [right; eexists; split; [reflexivity|]; simpl; split; [reflexivity|];
            do 2 eexists; split; [reflexivity |];
            eexists _, _, _, [(sBoxed, TNom nO [])];
            split; [reflexivity | split; [repeat constructor | split; reflexivity]]
           |]);
        constructor.
    - destruct Hrm as [[-> ->] | [-> ->]];
        (constructor; [simpl; tauto|]); (constructor; [simpl; tauto|]); constructor.
  Qed.
End FunctorSpec.

Corollary functor_example_two_bindings :
  unit_mono FunctorExample.Sg (UFn FunctorExample.fRound FunctorSpec.gO FunctorSpec.MWrap)
  /\ unit_mono FunctorExample.Sg (UFn FunctorExample.fRound FunctorSpec.gO FunctorSpec.MBare)
  /\ FunctorSpec.MWrap <> FunctorSpec.MBare.
Proof.
  split; [|split].
  - exact (proj2 (specialization functor_example_ok FunctorSpec.reach_wrap)).
  - exact (proj2 (specialization functor_example_ok FunctorSpec.reach_bare)).
  - discriminate.
Qed.

(* ---- What the generic checker rejects, so no specialization must. --------
   A match on an abstract type may not name a member [D16, §8 of the log]:
   `fn f(x: T) { match x { A => () } }` is ill-typed generically, so whether
   T is bound to a union holding A never has to be checked per
   specialization. *)
Module Rejected.
  Import SpecExample.
  Definition G0 : tenv := [(0, TAbs sT)].

  Lemma var_type : forall tau, has_ty Sg PhiW G0 (EVar 0) tau -> tau = TAbs sT.
  Proof.
    intros tau H. remember (EVar 0) as e eqn:Ee. remember PhiW as Phi eqn:EPhi.
    remember G0 as G eqn:EG.
    induction H; try discriminate.
    - injection Ee as ->. subst. simpl in H. injection H as <-. reflexivity.
    - subst. specialize (IHhas_ty eq_refl eq_refl eq_refl). subst tau'.
      inversion H0 as [t1 t2 Heq E1 E2|]; subst.
      unfold ty_eq in Heq. simpl in Heq. rewrite !app_subst_nil in Heq. exact (eq_sym Heq).
  Qed.

  Lemma match_on_abstract_rejected : forall tau,
    ~ has_ty Sg PhiW G0 (EMatch (EVar 0) [(nA, 1, EUnit)]) tau.
  Proof.
    intros tau H. remember (EMatch (EVar 0) [(nA, 1, EUnit)]) as e eqn:Ee.
    remember PhiW as Phi eqn:EPhi. remember G0 as G eqn:EG.
    induction H; try discriminate.
    - exact (IHhas_ty Ee EPhi EG).
    - injection Ee as -> ->. subst. apply var_type in H. subst tau0. discriminate H1.
  Qed.
End Rejected.

Corollary match_on_abstract_rejected : forall tau,
  ~ has_ty SpecExample.Sg SpecExample.PhiW Rejected.G0
        (EMatch (EVar 0) [(SpecExample.nA, 1, EUnit)]) tau.
Proof. exact Rejected.match_on_abstract_rejected. Qed.

(* ---- Finding: re-running the generic checker at a specialization is not
   D16-safe.  Two provisions a generic body tells apart by their receivers
   (T.m[P=T] and U.m[P=V]) collapse onto one concrete receiver at T=U=A,
   V=B; T-Meth's uniqueness, re-judged on the substituted context, then
   rejects a call that was resolved — uniquely and correctly — generically.
   The monomorphic checker keeps provision keys generic (D80: resolve once
   per call site, codegen only looks the target up), and accepts it. *)
Module Collapse.
  Definition sT : tsym := 1.
  Definition sU : tsym := 2.
  Definition sV : tsym := 3.
  Definition sP : tsym := 4.
  Definition mM : msym := 1.
  Definition fP : fsym := 1.
  Definition tA : ty := TNom 1 [].
  Definition tB : ty := TNom 2 [].
  Definition provT : item := IMth (TAbs sT) mM [(sP, TAbs sT)].
  Definition provU : item := IMth (TAbs sU) mM [(sP, TAbs sV)].
  Definition Sg : gsig := mkGsig (fun _ => true) (fun _ => None) (fun _ => None)
    (fun _ => None)
    (fun m => if Nat.eqb m mM then Some ([ITy sP], mkSig [] (TAbs sP)) else None)
    (fun _ => None)
     (fun _ => []) (fun _ => []) (fun _ => ([], [])).
  Definition call : expr := EMeth (EVar 0) mM provT [].

  (* Generically: one provision at receiver T. *)
  Definition PhiG : ctx := mkCtx [ITy sT; ITy sU; ITy sV; provT; provU] [].
  Lemma generic_ok : has_ty Sg PhiG [(0, TAbs sT)] call (TAbs sT).
  Proof.
    unfold call.
    cut (has_ty Sg PhiG [(0, TAbs sT)] (EMeth (EVar 0) mM provT [])
           (interp_m (cS PhiG) [(sP, TAbs sT)] (app_subst (cS PhiG) (TAbs sT))
                     (fs_ret (mkSig [] (TAbs sP))))); [intro H; exact H|].
    eapply TMethR with (Dm := [ITy sP]); [reflexivity | constructor; reflexivity
      | simpl; tauto | reflexivity | | constructor].
    intros i1 Hin [th' Hp]. simpl in Hin.
    destruct Hin as [<- | [<- | [<- | [<- | [<- | []]]]]]; try discriminate; reflexivity.
  Qed.

  (* The naive post-monomorphization check: the same checker on the
     substituted context and code, at T=U=A, V=B. *)
  Definition PhiN : ctx :=
    mkCtx [IMth tA mM [(sP, tA)]; IMth tA mM [(sP, tB)]] [].
  Lemma naive_recheck_rejects : forall tau,
    ~ has_ty Sg PhiN [(0, tA)] (EMeth (EVar 0) mM (IMth tA mM [(sP, tA)]) []) tau.
  Proof.
    intros tau H. remember (EMeth (EVar 0) mM (IMth tA mM [(sP, tA)]) []) as e eqn:Ee.
    remember PhiN as Phi eqn:EPhi.
    induction H; try discriminate.
    - exact (IHhas_ty Ee EPhi).
    - injection Ee as E0 E1 E2 E3 E4. subst. simpl in H2. rewrite !app_subst_nil in H2.
      assert (Hp : is_prov_at (cS PhiN) tau0 mM (IMth tA mM [(sP, tB)])).
      { exists [(sP, tB)]. simpl. rewrite app_subst_nil, <- H2. reflexivity. }
      specialize (H3 _ (or_intror (or_introl eq_refl)) Hp). discriminate H3.
  Qed.

  (* The monomorphic checker, at the binding: keys stay generic, so the
     call resolves to provT's provider at its concrete key. *)
  Definition Mspec : menv :=
    [(provT, MMth fP (IMth tA mM [(sP, tA)])); (provU, MMth fP (IMth tA mM [(sP, tB)]))].
  Lemma mono_ok : mty Sg [(sT, tA); (sU, tA); (sV, tB)] Mspec [(0, tA)]
                      (spec [(sT, tA); (sU, tA); (sV, tB)] call) tA.
  Proof.
    cut (mty Sg [(sT, tA); (sU, tA); (sV, tB)] Mspec [(0, tA)]
           (spec [(sT, tA); (sU, tA); (sV, tB)] call) (mparam tA [(sP, tA)] (fs_ret (mkSig [] (TAbs sP)))));
      [intro H; exact H|].
    eapply MTMeth with (Dm := [ITy sP]); [reflexivity | reflexivity | constructor; reflexivity | constructor].
  Qed.
End Collapse.

Corollary collapse_finding :
  has_ty Collapse.Sg Collapse.PhiG [(0, TAbs Collapse.sT)] Collapse.call (TAbs Collapse.sT)
  /\ (forall tau, ~ has_ty Collapse.Sg Collapse.PhiN [(0, Collapse.tA)]
                     (EMeth (EVar 0) Collapse.mM (IMth Collapse.tA Collapse.mM [(Collapse.sP, Collapse.tA)]) []) tau)
  /\ mty Collapse.Sg [(Collapse.sT, Collapse.tA); (Collapse.sU, Collapse.tA); (Collapse.sV, Collapse.tB)]
         Collapse.Mspec [(0, Collapse.tA)]
         (spec [(Collapse.sT, Collapse.tA); (Collapse.sU, Collapse.tA); (Collapse.sV, Collapse.tB)] Collapse.call)
         Collapse.tA.
Proof. split; [exact Collapse.generic_ok | split; [exact Collapse.naive_recheck_rejects | exact Collapse.mono_ok]]. Qed.

(* ---- Termination: the set of needed specializations is NOT finite. ------
   Polymorphic recursion through a type bind (D72 generations; no functor
   needed):

       type T;  type E;  assume E { tag Wrap(E); }  tag A;
       assume T {
         fn nest() { bind T2 = Wrap[E=T]; nest[T=T2](); }
       }
       fn main() { nest[T=A](); }

   is well-formed, and main reaches nest at T = A, Wrap[A], Wrap[Wrap[A]], …
   — every one a valid unit with a well-typed specialization (the theorem
   applies to each), and no finite list contains them all.  So finiteness
   cannot be part of the specialization theorem: D30's depth backstop is a
   genuine exception to D16, and the calculus (like the surface language)
   does nothing to rule this out.  (scratch/specialization/polyrec*.moss:
   the same program with a run-time counter makes the self-hosted compiler
   trap — stack exhaustion, or a memory fault when each level doubles the
   layout — and makes the bootstrap either compile it, when every level
   has one layout, or refuse it after specialization.) *)
Module PolyRec.
  Definition sT : tsym := 1.
  Definition sT2 : tsym := 2.
  Definition sE : tsym := 3.
  Definition nWrap : nsym := 1.
  Definition nA : nsym := 2.
  Definition fNest : fsym := 1.
  Definition fMain : fsym := 2.
  Definition tA : ty := TNom nA [].

  Definition nest_body : expr :=
    EBindTy sT2 (TNom nWrap [(sE, TAbs sT)]) (ECallD fNest [(sT, TAbs sT2)] [] []).
  Definition main_body : expr := ECallD fNest [(sT, tA)] [] [].

  Definition Sg : gsig := mkGsig
    (fun t => Nat.eqb t sT || Nat.eqb t sT2 || Nat.eqb t sE)
    (fun n => if Nat.eqb n nWrap then Some ([ITy sE], TAbs sE)
              else if Nat.eqb n nA then Some ([], TUnit) else None)
    (fun _ => None)
    (fun f => if Nat.eqb f fNest then Some ([ITy sT], mkSig [] TUnit, Some nest_body)
              else if Nat.eqb f fMain then Some ([], mkSig [] TUnit, Some main_body)
              else None)
    (fun _ => None) (fun _ => None)
     (fun _ => []) (fun _ => []) (fun _ => ([], [])).

  Definition PhiN : ctx := mkCtx [ITy sT] [].

  Lemma conv : forall Phi G e t t', has_ty Sg Phi G e t -> t = t' -> has_ty Sg Phi G e t'.
  Proof. intros; subst; assumption. Qed.

  Lemma call_nest : forall Phi t, wf_ty Sg Phi t ->
    has_ty Sg Phi [] (ECallD fNest [(sT, t)] [] []) TUnit.
  Proof.
    intros Phi t Hw. eapply conv.
    - eapply TCallR with (Df := [ITy sT]) (S := mkSig [] TUnit) (body := nest_body);
        [reflexivity | | constructor; [exact Hw | constructor] | | constructor].
      + exists PhiN. split; [reflexivity|]. split; [reflexivity|].
        split; [constructor; [left; reflexivity | constructor] | constructor].
      + exists PhiN. split; [reflexivity|]. split; [reflexivity | constructor].
    - reflexivity.
  Qed.

  Lemma wf_wrap : forall Phi, avail Phi (ITy sT) -> wf_ty Sg Phi (TNom nWrap [(sE, TAbs sT)]).
  Proof.
    intros Phi H. eapply WfTag; [reflexivity | |].
    - eexists. split; [reflexivity|]. split; [reflexivity|].
      split; [repeat constructor; left; reflexivity | constructor].
    - constructor; [apply WfNeed; exact H | constructor].
  Qed.

  Lemma wf_A : forall Phi, wf_ty Sg Phi tA.
  Proof.
    intros Phi. eapply WfTag with (D := []) (tau0 := TUnit); [reflexivity | | constructor].
    exists empty_ctx. split; [reflexivity|]. split; [reflexivity | split; constructor].
  Qed.

  Lemma wf_sig : wf_gsig Sg.
  Proof.
    constructor.
    - intros n D taup H. destruct n as [|[|[|n]]]; simpl in H; try discriminate;
        injection H as <- <-.
      + exists (mkCtx [ITy sE] []). split.
        * eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
        * apply WfNeed. reflexivity.
      + exists empty_ctx. split; constructor.
    - intros v D tauv H. discriminate.
    - intros f D S b H. destruct f as [|[|[|f]]]; simpl in H; try discriminate;
        injection H as <- <- <-.
      + exists PhiN. split.
        * eapply WCons; [constructor; [reflexivity | discriminate] | reflexivity | constructor].
        * split; [constructor|]. split; [constructor|].
          intros body Hb. injection Hb as <-.
          eapply TBindTyR; [reflexivity | discriminate | simpl; intros [H|[]]; discriminate
                           | apply wf_wrap; reflexivity | | constructor].
          apply call_nest. apply WfNeed. reflexivity.
      + exists empty_ctx. split; [constructor|]. split; [constructor|]. split; [constructor|].
        intros body Hb. injection Hb as <-. apply call_nest. apply wf_A.
    - intros m D S H. discriminate.
    - intros u Du Dr ob H. discriminate.
  Qed.

  Lemma wf : wf_prog Sg fMain.
  Proof. split; [exact wf_sig | eexists; reflexivity]. Qed.

  Fixpoint wrapk (k : nat) : ty :=
    match k with O => tA | S k' => TNom nWrap [(sE, wrapk k')] end.

  Definition unit_at (k : nat) : sunit := UFn fNest [(sT, wrapk k)] [].

  Lemma reach_all : forall k, reach Sg fMain (unit_at k).
  Proof.
    induction k as [|k IH].
    - eapply RStep; [apply RMain | reflexivity |]. simpl.
      change (unit_at 0) with (UFn fNest (restrict [(sT, tA)] (tyreqs (cA PhiN))) []).
      eapply CCall; reflexivity.
    - eapply RStep; [exact IH | reflexivity |]. simpl. apply CBindTy.
      change (unit_at (S k))
        with (UFn fNest (restrict [(sT, wrapk (S k))] (tyreqs (cA PhiN))) []).
      eapply CCall; reflexivity.
  Qed.

  Definition usize (u : sunit) : nat := match u with UFn _ g _ => subst_size g end.

  Lemma wrapk_size : forall k, k <= ty_size (wrapk k).
  Proof. induction k as [|k IH]; simpl; lia. Qed.

  Lemma unit_size : forall k, k <= usize (unit_at k).
  Proof. intros k. simpl. pose proof (wrapk_size k). lia. Qed.

  Lemma in_le_sum : forall u l, In u l -> usize u <= list_sum (map usize l).
  Proof.
    intros u l. induction l as [|a l IH]; simpl; [intros []|].
    intros [<-|H]; [lia | specialize (IH H); lia].
  Qed.
End PolyRec.

(* Every T = Wrap^k[A] is a reachable, valid, monomorphically well-typed
   specialization of `nest` ... *)
Corollary polyrec_all_specialized : forall k,
  unit_ok PolyRec.Sg (PolyRec.unit_at k) /\ unit_mono PolyRec.Sg (PolyRec.unit_at k).
Proof. intros k. exact (specialization PolyRec.wf (PolyRec.reach_all k)). Qed.

(* ... and no finite set of specializations covers what main reaches. *)
Theorem polyrec_infinite :
  wf_prog PolyRec.Sg PolyRec.fMain
  /\ ~ exists l, forall u, reach PolyRec.Sg PolyRec.fMain u -> In u l.
Proof.
  split; [exact PolyRec.wf|]. intros [l Hl].
  set (N := list_sum (map PolyRec.usize l)).
  pose proof (@PolyRec.in_le_sum _ l (Hl _ (PolyRec.reach_all (S N)))) as H1.
  pose proof (PolyRec.unit_size (S N)) as H2. fold N in H1. lia.
Qed.

(* ---- The runtime environment is a binding. ------------------------------
   The static part of any runtime environment the soundness proof considers
   (denv_agree at a grounding g) is a specialized environment resolving
   every requirement in force at g: the provider code a closure carries is
   the label, the val's and fn's types are read through g, a functor
   binding is copied.  So bindings are exactly the static shadow of the
   runtime states that type soundness covers — the premise of
   `specialization`'s units is not vacuous, and its provider labels are the
   providers that actually run. *)
Definition stat_entry (Sg : gsig) (g : subst) (p : item * dentry) : item * ment :=
  match p with
  | (IVal v, _) =>
      (IVal v, MVal (match g_val Sg v with Some (_, tv) => app_subst g tv | None => TUnit end))
  | (IFn f, DClo f' _) =>
      (IFn f, match g_fn Sg f with
              | Some (_, Sf, _) => MFn f' (map (app_subst g) (fs_params Sf)) (app_subst g (fs_ret Sf))
              | None => MFn f' [] TUnit
              end)
  | (IMth rt m th, DClo f' _) => (IMth rt m th, MMth f' (app_subst_item g (IMth rt m th)))
  | (IFun u, DFun h sms _) => (IFun u, MFun h sms)
  | (i, _) => (i, MVal TUnit)
  end.

Definition static_of (Sg : gsig) (g : subst) (d : denv) : menv := map (stat_entry Sg g) d.

Lemma stat_entry_key : forall Sg g p, fst (stat_entry Sg g p) = fst p.
Proof. intros Sg g [[t|v|f|rt m th|u] [w|f' d'|h sms]]; reflexivity. Qed.

Lemma lookup_static : forall Sg g i d,
  lookup_menv i (static_of Sg g d) =
  match lookup_denv i d with Some en => Some (snd (stat_entry Sg g (i, en))) | None => None end.
Proof.
  intros Sg g i d. induction d as [|[k en] r IH]; [reflexivity|].
  change (static_of Sg g ((k, en) :: r)) with (stat_entry Sg g (k, en) :: static_of Sg g r).
  pose proof (stat_entry_key Sg g (k, en)) as Hk.
  destruct (stat_entry Sg g (k, en)) as [k' en'] eqn:E. simpl in Hk. subst k'.
  cbn [lookup_menv lookup_denv].
  destruct (item_eqb i k) eqn:Ei; [|exact IH].
  apply item_eqb_eq in Ei. subst. rewrite E. reflexivity.
Qed.

Theorem runtime_binding : forall Sg g A d,
  denv_agree Sg g A d -> mreal Sg g A (static_of Sg g d).
Proof.
  intros Sg g A d [Hcov [Hent _]]. unfold mreal. rewrite Forall_forall in Hcov |- *.
  rewrite Forall_forall in Hent. intros i Hi.
  destruct (Hcov i Hi) as [Hty|[en Hen]]; [left; exact Hty|right].
  rewrite lookup_static, Hen. eexists. split; [reflexivity|].
  pose proof (Hent _ (lookup_denv_in _ _ Hen)) as Hag. simpl in Hag.
  destruct i as [t|v|f|rt m th|u]; destruct en as [w|f' d'|h sms]; simpl in Hag |- *;
    try contradiction.
  - destruct (g_val Sg v) as [[Dv tv]|] eqn:Ev; [|contradiction].
    exists Dv, tv. split; reflexivity.
  - destruct (g_fn Sg f) as [[[Df Sf] [b|]]|] eqn:Ef; try contradiction.
    destruct (g_fn Sg f') as [[[D' S'] [b'|]]|] eqn:Ef'; try contradiction.
    destruct Hag as [PhiD' [g' [_ [Hgr [_ [_ [Hps [Hr _]]]]]]]].
    split; [exists Df, Sf; split; [reflexivity | split; reflexivity] |].
    exists D', S', b', g'. split; [exact Ef' |]. split; [exact Hgr |]. split; assumption.
  - destruct (g_mth Sg m) as [[Dm Sm]|] eqn:Em; [|contradiction].
    destruct (g_fn Sg f') as [[[D' S'] [b'|]]|] eqn:Ef'; try contradiction.
    destruct Hag as [PhiD' [g' [_ [Hgr [_ [_ [Hps [Hr _]]]]]]]].
    split; [reflexivity|]. exists Dm, Sm. split; [reflexivity |].
    exists D', S', b', g'. split; [exact Ef' |]. split; [exact Hgr |]. split; assumption.
  - destruct (g_fun Sg u) as [[[Du Dr] [bu|]]|] eqn:Eu; try contradiction.
    destruct (g_fun Sg h) as [[[Dh Drh] [bh|]]|] eqn:Eh; try contradiction.
    destruct Hag as [-> [Hch [PhiC [g' [H1 [H2 [H3 [H4 [H5 [_ [_ [HK _]]]]]]]]]]]].
    exists Du, Dr, Dh, bh. split; [reflexivity|]. split; [first [reflexivity | exact Eh]|].
    split; [exact Hch |].
    exists PhiC, g'. repeat (split; [assumption |]). exact HK.
Qed.

(* ---- The theorem audit's BadProvider, now refused. -----------------------
   A unit whose `IFn f : () -> A` is labelled with a provider whose own
   signature returns a B (and whose body runs to a B) was a valid binding
   under the old mok, which asked only that the provider have a body.  The
   provider clause (`prov_sig`) refuses it: no grounding reads the closed
   return type B as A. *)
Module BadProvider.
  Definition nA : nsym := 1.  Definition nB : nsym := 2.
  Definition tA : ty := TNom nA [].  Definition tB : ty := TNom nB [].
  Definition f : fsym := 1.  Definition bad : fsym := 2.  Definition user : fsym := 3.
  Definition Sg : gsig := mkGsig (fun _ => false)
    (fun n => if Nat.eqb n nA then Some ([], TUnit)
              else if Nat.eqb n nB then Some ([], TUnit) else None)
    (fun _ => None)
    (fun g => if Nat.eqb g f then Some ([], mkSig [] tA, None)
              else if Nat.eqb g bad then Some ([], mkSig [] tB, Some (ETag nB [] EUnit))
              else if Nat.eqb g user then Some ([IFn f], mkSig [] tA, Some (ECallA f []))
              else None)
    (fun _ => None) (fun _ => None)
    (fun _ => []) (fun _ => []) (fun _ => ([], [])).
  Definition M : menv := [(IFn f, MFn bad [] tA)].

  (* ... what would run is a B. *)
  Example runs_B : interp 10 Sg [(IFn f, DClo bad [])] [] (ECallA f []) = Ok (VTag nB VUnit).
  Proof. reflexivity. Qed.

  Lemma refused : ~ unit_ok Sg (UFn user [] M).
  Proof.
    intros [Df [S [body [PhiD [Hf [Wt [_ [_ [Hmr _]]]]]]]]].
    simpl in Hf. injection Hf as <- <- <-.
    inversion Wt as [| Phi0 i D Phi' Phi'' Hok Hm Wt' E1 E2]; subst.
    inversion Wt' as [Phi1 E3 E4 |]; subst. simpl in Hm. injection Hm as <-.
    unfold mreal in Hmr. simpl in Hmr. inversion Hmr as [| i l Hi _]; subst.
    destruct Hi as [Hi | [en [He Hok']]]; [discriminate Hi |].
    simpl in He. injection He as <-. simpl in Hok'.
    destruct Hok' as [_ [D' [S' [b' [g' [Hb [_ [_ Hr]]]]]]]].
    simpl in Hb. injection Hb as <- <- <-. simpl in Hr. discriminate Hr.
  Qed.
End BadProvider.

Theorem bad_provider_refused :
  ~ unit_ok BadProvider.Sg (UFn BadProvider.user [] BadProvider.M)
  /\ interp 10 BadProvider.Sg [(IFn BadProvider.f, DClo BadProvider.bad [])] []
       (ECallA BadProvider.f []) = Ok (VTag BadProvider.nB VUnit).
Proof. split; [exact BadProvider.refused | exact BadProvider.runs_B]. Qed.
