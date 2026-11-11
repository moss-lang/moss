(* What a run of bind items adds to a context mentions only its scope
   (linknewpf). PublicScoping's [binditems_scoped] allows a provision to
   mention a declared type symbol out of scope as well (a family demand
   may read an unbound requirement as itself), but bind items never do:
   every provision and key a bind item adds is read through the type
   table (or the brackets), so it mentions only [scope_gens]. Together
   with [binditems_scoped]'s growth by `made` this bounds a body's new
   provisions by the context it starts from. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicFormation PublicScoping.
From NM Require PublicContextFacts PublicMerge PublicOrientation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Scope.
Variable W : A.world.
Variable Prog : program.
Import A.
Hypothesis units_bare : UnitsBare W.
Hypothesis family_sig_ok : FamilySigReadOffOk W.

Definition ProvScope (C C' : cx) : Prop := forall k p, In (k, p) (c_provisions C') ->
  In (k, p) (c_provisions C) \/ (incl (provision_gens p) (scope_gens C') /\ incl (PublicCallRealize.key_gens k) (scope_gens C')).


(* Binds may read old meanings through the merge, but keep each meaning's
   class. This is independent of its stored representative. *)
Definition TypesKept (C C' : cx) : Prop := PublicContextFacts.CxSection C ->
  forall x t, In (x, t) (c_ty C) -> exists u,
    In (x, u) (c_ty C') /\ MergeEq (c_merge C) t u.

Lemma types_kept_incl : forall C C', incl (c_ty C) (c_ty C') -> TypesKept C C'.
Proof.
  intros C C' Included _ x t It. exists t. split; [exact (Included _ It)|apply MERefl].
Qed.

Definition StepScope (C C' : cx) (m : list nat) : Prop :=
  incl (scope_gens C) (scope_gens C') /\ (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ In y m) /\
  ProvScope C C' /\ (c_sub C' = c_sub C /\ c_merge C' = c_merge C) /\ TypesKept C C'.

(* The temporary row identity is used only to transport the section.
   Subsequent occurrence reasoning compares sections of the same merge. *)
Lemma step_scope_section : forall C C' made,
  StepScope C C' made -> PublicContextFacts.CxSection C -> PublicContextFacts.CxSection C'.
Proof.
  intros C C' made [_ [_ [_ [[Rows Merge] _]]]] Section.
  exact (PublicContextFacts.section_same_fields C C' Rows Merge Section).
Qed.


Lemma step_class_scope_grows : forall C C' made,
  StepScope C C' made -> PublicContextFacts.CxSection C ->
  incl (class_scope_gens C) (class_scope_gens C').
Proof.
  intros C C' made [_ [_ [_ [[_ Merge] Kept]]]] Section y Iy.
  unfold class_scope_gens in *. rewrite Merge. apply in_app_or in Iy as [Inside|Iy].
  - apply in_or_app. left. exact Inside.
  - apply in_or_app. destruct (in_dec Nat.eq_dec y (merge_symbols (c_merge C))) as [Inside|Outside];
      [left; exact Inside|right].
    destruct (tm_gens_in _ y Iy) as [x [t [It IY]]].
    destruct (Kept Section x t It) as [u [Iu Equal]].
    apply (in_tm_gens _ y x u Iu). apply (proj1 (PublicMerge.merge_eq_gens_out Equal y Outside)). exact IY.
Qed.

Lemma step_scope_class_view : forall C C' made,
  StepScope C C' made -> PublicContextFacts.CxSection C ->
  (forall g v, In (g, v) (c_sub C) -> lookup_subst g (c_sub C) = Some v) ->
  ClassScopeView C -> ClassScopeView C'.
Proof.
  intros C C' made Step Section Function View.
  pose proof (step_scope_section C C' made Step Section) as Section'.
  pose proof (step_class_scope_grows C C' made Step Section) as Grow.
  destruct Step as [Scope [_ [_ [[Rows Merge] _]]]].
  apply class_scope_view; [exact Section'| |].
  - intros g v Iq. rewrite Rows in Iq |- *. split; [exact (Function g v Iq)|].
    apply Grow, (proj1 (View g)). exact (scope_sub_key C g v Iq).
  - intros y Iy. apply Scope, (proj2 (View y)). unfold class_scope_gens.
    apply in_or_app. left. rewrite <- Merge. exact Iy.
Qed.

Definition ClassStepScope (C C' : cx) (made : list nat) : Prop :=
  (forall y, In y (class_scope_gens C') -> In y (class_scope_gens C) \/ In y made) /\
  (forall k p, In (k, p) (c_provisions C') -> In (k, p) (c_provisions C) \/
    (incl (provision_gens p) (class_scope_gens C') /\ incl (PublicCallRealize.key_gens k) (class_scope_gens C'))).

Lemma step_scope_classes : forall C C' made,
  StepScope C C' made -> ClassScopeView C -> ClassScopeView C' -> ClassStepScope C C' made.
Proof.
  intros C C' made [_ [Back [Prov _]]] View View'. split.
  - intros y Iy. destruct (Back y (proj2 (View' y) Iy)) as [Before|Made]; [left|right; exact Made].
    exact (proj1 (View y) Before).
  - intros k p Iq. destruct (Prov k p Iq) as [Old|[Types Keys]]; [left; exact Old|right].
    split; intros y Iy; apply (proj1 (View' y)); [exact (Types y Iy)|exact (Keys y Iy)].
Qed.

(* The live linking interface speaks only about class scopes. The stored
   row function is needed to establish the temporary view, not its clients. *)
Lemma functx_step_scope : forall u C ns C' made,
  FunCtx W u C ns -> StepScope C C' made ->
  incl (class_scope_gens C) (map fst (c_ty C)) /\ ClassStepScope C C' made /\
  PublicContextFacts.CxSection C' /\ c_merge C' = c_merge C.
Proof.
  intros u C ns C' made FC Step.
  destruct (functx_scope_view W u C ns FC) as [View Bound].
  pose proof (proj2 (PublicContextFacts.functor_context_merge W u C ns FC)) as Section.
  pose proof (step_scope_class_view C C' made Step Section
    (PublicOrientation.functx_sub_function W u C ns FC) View) as View'.
  split; [exact Bound|split; [exact (step_scope_classes C C' made Step View View')|split]].
  - exact (step_scope_section C C' made Step Section).
  - exact (proj2 (proj1 (proj2 (proj2 (proj2 Step))))).
Qed.

(* A section cannot erase or introduce a generation outside its merge. *)
Lemma section_gens_out : forall C D,
  PublicContextFacts.CxSection C -> incl (merge_symbols (c_merge C)) D ->
  forall t y, ~ In y D -> (In y (gens (canon C t)) <-> In y (gens t)).
Proof.
  intros C D Section Support t y Outside. symmetry.
  eapply PublicMerge.merge_eq_gens_out.
  - apply PublicMerge.merge_eq_read. exact (proj1 Section).
  - intro InMerge. exact (Outside (Support y InMerge)).
Qed.

Lemma sections_gens_out : forall C C' D,
  PublicContextFacts.CxSection C -> PublicContextFacts.CxSection C' -> c_merge C' = c_merge C ->
  (forall g v, In (g, v) (c_sub C) -> In g D /\ incl (gens v) D) ->
  forall t y, ~ In y D -> (In y (gens (canon C' t)) <-> In y (gens t)).
Proof.
  intros C C' D Section Section' Merge Scope.
  assert (Source : forall t y, ~ In y D -> (In y (gens (canon C t)) <-> In y (gens t))).
  { intros t y Outside. split.
    - intro InCanon. destruct (gens_subst t (c_sub C) y InCanon) as [z [Iz [[_ ->]|[v [Read InV]]]]]; [exact Iz|].
      exfalso. exact (Outside (proj2 (Scope z v (lookup_subst_in_pair _ _ _ Read)) y InV)).
    - intro InTy. apply (gens_keep t (c_sub C) y InTy).
      destruct (lookup_subst y (c_sub C)) as [v|] eqn:Read; [|reflexivity].
      exfalso. exact (Outside (proj1 (Scope y v (lookup_subst_in_pair _ _ _ Read)))). }
  assert (Same : forall t, canon C (canon C' t) = canon C t).
  { intro t. symmetry. apply (proj2 Section). rewrite <- Merge.
    apply PublicMerge.merge_eq_read. exact (proj1 Section'). }
  intros t y Outside. rewrite <- (Source (canon C' t) y Outside), Same.
  exact (Source t y Outside).
Qed.

Lemma section_sig_gens_out : forall C D,
  (forall t y, ~ In y D -> (In y (gens (canon C t)) <-> In y (gens t))) ->
  forall sg y, ~ In y D -> (In y (sig_gens (sig_subst (c_sub C) sg)) <-> In y (sig_gens sg)).
Proof.
  intros C D Gens [ps rt] y Outside. unfold sig_gens, sig_subst. cbn.
  rewrite !in_app_iff. split; intros [InRet|InParam].
  - left. apply (proj1 (Gens rt y Outside)). exact InRet.
  - right. apply in_flat_map in InParam as [t [InT InY]].
    apply in_map_iff in InT as [t' [<- InT]]. apply in_flat_map.
    exists t'. split; [exact InT|exact (proj1 (Gens t' y Outside) InY)].
  - left. exact (proj2 (Gens rt y Outside) InRet).
  - right. apply in_flat_map in InParam as [t [InT InY]].
    apply in_flat_map. exists (canon C t). split; [apply in_map; exact InT|exact (proj2 (Gens t y Outside) InY)].
Qed.

Lemma set_provision_in : forall C k0 p0 k p, In (k, p) (c_provisions (set_provision C k0 p0)) ->
  (k, p) = (k0, p0) \/ In (k, p) (c_provisions C).
Proof.
  intros C k0 p0 k p I. cbn in I. destruct I as [E|I]; [left; symmetry; exact E|right].
  apply filter_In in I as [I _]. exact I.
Qed.

Lemma provide_set_in : forall C k0 p0 a k p, In (k, p) (c_provisions (provide C k0 a (set_provision C k0 p0))) ->
  In (k, p) (c_provisions C) \/ (k = k0 /\ incl (provision_gens p) (provision_gens p0 ++ inst_gens a)).
Proof.
  intros C k0 p0 a k p I. unfold provide in I.
  assert (L : ilook k0 (c_provisions (set_provision C k0 p0)) = Some p0) by (cbn; rewrite item_eqb_refl'; reflexivity).
  rewrite L in I.
  assert (Old : forall q, In (k, p) (c_provisions (set_provision (set_provision C k0 p0) k0 q)) ->
    (k, p) = (k0, q) \/ In (k, p) (c_provisions C) \/ (k, p) = (k0, p0)).
  { intros q Iq. destruct (set_provision_in _ _ _ _ _ Iq) as [E|Iq']; [left; exact E|right].
    destruct (set_provision_in _ _ _ _ _ Iq') as [E|Iq'']; [right; exact E|left; exact Iq'']. }
  assert (Fin : forall q, incl (provision_gens q) (provision_gens p0 ++ inst_gens a) ->
    In (k, p) (c_provisions (set_provision (set_provision C k0 p0) k0 q)) ->
    In (k, p) (c_provisions C) \/ (k = k0 /\ incl (provision_gens p) (provision_gens p0 ++ inst_gens a))).
  { intros q Sq Iq. destruct (Old q Iq) as [E|[Ic|E]].
    - injection E as -> ->. right. split; [reflexivity|exact Sq].
    - left. exact Ic.
    - injection E as -> ->. right. split; [reflexivity|intros y Iy; apply in_or_app; left; exact Iy]. }
  destruct p0 as [t a0|g a0|g a0|fx].
  - apply (Fin (ProvidedVal t a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - apply (Fin (ProvidedFn g a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - apply (Fin (ProvidedMethod g a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - destruct (set_provision_in _ _ _ _ _ I) as [E|Ic]; [injection E as -> ->; right; split; [reflexivity|intros y Iy; apply in_or_app; left; exact Iy]|left; exact Ic].
Qed.

Lemma scope_provide_set : forall C k0 p0 a, scope_gens (provide C k0 a (set_provision C k0 p0)) = scope_gens C.
Proof. intros C k0 p0 a. rewrite provide_scope. reflexivity. Qed.

Lemma merge_provide_set : forall C k0 p0 a, c_merge (provide C k0 a (set_provision C k0 p0)) = c_merge C.
Proof. intros C k0 p0 a. rewrite PublicContextFacts.provide_merge. reflexivity. Qed.

Lemma sub_provide_set : forall C k0 p0 a, c_sub (provide C k0 a (set_provision C k0 p0)) = c_sub C.
Proof. intros C k0 p0 a. destruct (provide_tables C k0 a (set_provision C k0 p0)) as [_ [E _]]. exact E. Qed.

(* A provision made at C, scoped: then the step is scoped. *)
Lemma step_provide : forall C k0 p0 a,
  incl (provision_gens p0 ++ inst_gens a) (scope_gens C) -> incl (PublicCallRealize.key_gens k0) (scope_gens C) ->
  StepScope C (provide C k0 a (set_provision C k0 p0)) [].
Proof.
  intros C k0 p0 a Sp Sk.
  assert (Keep : TypesKept C (provide C k0 a (set_provision C k0 p0))).
  { apply types_kept_incl. rewrite (proj1 (provide_tables C k0 a (set_provision C k0 p0))).
    intros q Iq. exact Iq. }
  unfold StepScope. rewrite scope_provide_set, sub_provide_set, merge_provide_set.
  split; [intros y I; exact I|split; [intros y I; left; exact I|split; [|split; [split; reflexivity|exact Keep]]]].
  intros k p I. rewrite scope_provide_set. destruct (provide_set_in _ _ _ _ _ _ I) as [Ic|[-> Sg]]; [left; exact Ic|right].
  split; [intros y Iy; exact (Sp y (Sg y Iy))|exact Sk].
Qed.

Lemma step_set_fun : forall C u fx, (forall x tx, In (x, tx) fx -> nlook x (c_ty C) = Some tx) ->
  StepScope C (set_fun C u fx) [].
Proof.
  intros C u fx Fx.
  assert (Keep : TypesKept C (set_fun C u fx)) by (apply types_kept_incl; intros q Iq; exact Iq).
  split; [intros y I; exact I|split; [intros y I; left; exact I|split; [|split; [split; reflexivity|exact Keep]]]].
  intros k p I. destruct (set_provision_in _ _ _ _ _ I) as [E|Ic]; [|left; exact Ic].
  injection E as -> ->. right. split; [|intros y []].
  intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  apply in_flat_map in Iy as [[x o] [Iq Iy]]. apply in_map_iff in Iq as [[x' tx] [E Iq]]. cbn in E. injection E as -> <-.
  change (In y (scope_gens C)). apply ty_scope. exact (nlook_gens _ _ _ _ (Fx _ _ Iq) Iy).
Qed.

Lemma step_trans : forall C C1 C2 m1 m2, StepScope C C1 m1 -> StepScope C1 C2 m2 -> StepScope C C2 (m1 ++ m2).
Proof.
  intros C C1 C2 m1 m2 [I1 [B1 [P1 [[S1 M1] K1]]]] [I2 [B2 [P2 [[S2 M2] K2]]]].
  split; [intros y I; exact (I2 y (I1 y I))|split; [|split; [|split; [split; [rewrite S2; exact S1|rewrite M2; exact M1]|]]]].
  - intros y I. destruct (B2 y I) as [I'|I']; [destruct (B1 y I') as [I''|I'']; [left; exact I''|]|];
      right; apply in_or_app; [left|right]; assumption.
  - intros k p I. destruct (P2 k p I) as [I'|S]; [|right; exact S].
    destruct (P1 k p I') as [I''|[Sa Sb]]; [left; exact I''|right].
    split; intros y Iy; [exact (I2 y (Sa y Iy))|exact (I2 y (Sb y Iy))].
  - intros Section x t It. destruct (K1 Section x t It) as [u [Iu E1]].
    pose proof (PublicContextFacts.section_same_fields C C1 S1 M1 Section) as Section1.
    destruct (K2 Section1 x u Iu) as [v [Iv E2]]. exists v. split; [exact Iv|].
    eapply METrans; [exact E1|rewrite <- M1; exact E2].
Qed.

Lemma pg_split : forall p y, In y (provision_gens p) ->
  (exists g, provision_signature p = Some g /\ In y (sig_gens g)) \/ In y (inst_gens (provision_inst p)).
Proof.
  intros p y I. unfold provision_gens in I. apply in_app_or in I as [I|I]; [|right; exact I].
  destruct (provision_signature p) as [g|]; [left; exists g; split; [reflexivity|exact I]|contradiction].
Qed.

Lemma tm_in_scope : forall C y, In y (tm_gens (c_ty C)) -> In y (scope_gens C).
Proof. intros C y I. exact (ty_scope C y I). Qed.

Fixpoint item_scope F C it w C' F' m (D : BindItem W Prog F C it w C' F' m) {struct D} :
  FormedCx W C -> FrameFormed W F -> StepScope C C' m
with items_scope F C its w C' F' m (D : BindItems W Prog F C its w C' F' m) {struct D} :
  FormedCx W C -> FrameFormed W F -> StepScope C C' m.
Proof.
  - destruct D as [F C pa t rp ra tau R K Np T
                  |F C pa v d n t0 i declared yv x xt a R D' Ast T Ey Fi App
                  |F C pa f g sg pg sm a types R K Sig FP App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a yp r m bs rt sg target pre sm ap types R MN B T Sig MP App NoFam
                  |F C pa u d n ps a r bodies wrap C' F' made R D' Ast Na Bi
                  |F C pa a Typed
                  |F C pa u pg h Cu nsu Ch nsh target sm fx
                  |F C pa u pg gg Cu nsu Cg nsg h pins sm smc fx]; intros Fc Ff.
    + (* bind T = tau *)
      assert (Sv : forall y, In y (gens (canon C tau)) -> In y (scope_gens C)).
      { intros y Iy. destruct (gens_canon C tau y Iy) as [I|I]; [exact (ty_scope C y (tyof_none_gens W _ _ _ _ T y I))|exact I]. }
      assert (Keep : TypesKept C (set_ty C t (canon C tau)))
        by (apply types_kept_incl; intros q Iq; right; exact Iq).
      split; [exact (scope_set_ty C t _)|split; [intros y I; left; exact (set_ty_back C t _ Sv y I)|split; [|split; [split; reflexivity|exact Keep]]]].
      intros k p I. left. exact I.
    + (* bind v = e *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. cbn in Iy'. rewrite app_nil_r in Iy'. apply tm_in_scope. exact (tyof_none_gens W _ _ _ _ T y Iy').
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind f = g *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E _]]]; [exact (tm_in_scope C y I)|discriminate].
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind f = lambda *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E _]]]; [exact (tm_in_scope C y I)|discriminate].
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind R.m = provider *)
      assert (Full : forall y, In y (tm_gens (bs ++ c_ty C)) -> In y (scope_gens C)).
      { intros y Iy. apply tm_in_scope. apply tm_gens_app in Iy as [Iy|Iy]; [exact (br_gens W _ _ _ _ _ B y Iy)|exact Iy]. }
      assert (Rt : forall y, In y (gens rt) -> In y (scope_gens C)) by (intros y Iy; exact (Full y (tyof_none_gens W _ _ _ _ T y Iy))).
      apply step_provide.
      * intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
        -- injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E Ix]]]; [exact (Full y I)|].
           injection E as <-. exact (Rt y Ix).
        -- cbn in Iy'. contradiction.
        -- exact (Full y (appof_scoped W _ _ _ App y Iy)).
      * intros y Iy. cbn in Iy. destruct (gens_canon C rt y Iy) as [I|I]; [exact (Rt y I)|exact I].
    + (* bind F; F with a body *)
      exact (items_scope _ _ _ _ _ _ _ Bi Fc Ff).
    + (* bind F; F abstract *)
      destruct a as [u Cu ns th outs gensof newgens C' ks].
      destruct Typed as (_ & _ & _ & _ & _ & _ & FF & _ & Outs & _ & OutsI & PR & _).
      change (ProvideResult W u C (fun t => TAbs (gensof t)) C' ks) in PR.
      cbv beta iota zeta delta [OutputIdentityAt app_functor app_identity app_allocated] in Outs.
      destruct (apply_abstract_scope W _ _ _ _ _ PR) as [Inc Back].
      destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs' [OutsI' Ety]]]].
      pose proof Fc as [Cf [Ks Ts]].
      assert (InTy : forall x v, In (x, v) (output_type_map C outs' (fun t => TAbs (gensof t))) ->
        In (x, app_subst (c_sub C') v) (c_ty C')).
      { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
      assert (OldIn : forall x v, In (x, v) (c_ty C) -> In (x, canon C v) (output_type_map C outs' (fun t => TAbs (gensof t)))).
      { intros x v Iq. unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iq]. }
      assert (OutIn : forall t, In t outs' -> In (t, canon C (TAbs (gensof t))) (output_type_map C outs' (fun t => TAbs (gensof t)))).
      { intros t It. unfold output_type_map. apply in_map_iff. exists (t, TAbs (gensof t)). split; [reflexivity|].
        apply in_or_app; left. apply in_map_iff. exists t. split; [reflexivity|exact It]. }
      assert (OutAbs : forall t, FunOutputs W u t -> Abstract W (gensof t)).
      { intros t Ft. destruct (Outs t Ft) as [[_ [E _]]|[_ Ig]];
          [rewrite E; exact (typed_abstract W _ (outputs_typed W _ _ Ft))|].
        apply (fresh_abstract W). match goal with FF : A.FreshFor _ _ _ ?gs |- _ => exact (freshfor_fresh W _ _ _ _ FF Ig) end. }
      set (K := fun y => In y (scope_gens C')).
      assert (ReadK : ReadWithin K (c_sub C)).
      { rewrite <- Esub. apply canon_within. intros y Iy. exact Iy. }
      assert (TyK : forall x v, In (x, v) (c_ty C) -> TyOk W (canon C v) /\ GensIn K (gens (canon C v))).
      { intros x v Iq. split; [exact (canon_ok W C v Cf (cf_ty _ _ Cf _ _ Iq))|].
        exact (table_scope C' x (canon C v) (InTy _ _ (OldIn _ _ Iq))). }
      assert (OutK : forall t, FunOutputs W u t ->
         TyOk W (canon C (TAbs (gensof t))) /\ GensIn K (gens (canon C (TAbs (gensof t))))).
      { intros t Ft. split; [exact (canon_ok W C _ Cf (tyok_abs W _ (OutAbs t Ft)))|].
        exact (table_scope C' t _ (InTy _ _ (OutIn t (proj2 (OutsI' t) Ft)))). }
      destruct (provide_result_ok W _ _ _ _ _ K PR (cf_read _ _ Cf) ReadK TyK OutK) as [_ [_ [_ Pr]]].
      assert (Keep : TypesKept C C').
      { intros Section x v Iq. exists (app_subst (c_sub C') (canon C v)).
        split; [exact (InTy _ _ (OldIn _ _ Iq))|]. rewrite Esub.
        eapply METrans.
        - exact (@PublicMerge.merge_eq_read _ _ (proj1 Section) v).
        - exact (@PublicMerge.merge_eq_read _ _ (proj1 Section) (canon C v)). }
      split; [exact Inc|split; [|split; [|split; [split; [exact Esub|exact (PublicContextFacts.provided_result_merge W u C _ C' _ PR)]|exact Keep]]]].
      * intros y I. destruct (Back y I) as [I'|[t [Ft ->]]]; [left; exact I'|right].
        apply in_map. exact (proj2 (OutsI t) Ft).
      * intros k p Iq. destruct (Pr k p Iq) as [Iq'|[_ [_ Kr]]]; [left; exact Iq'|].
        destruct (provide_result_gens W _ _ _ _ _ K PR ReadK (fun x v I => proj2 (TyK x v I)) (fun t F => proj2 (OutK t F)) k p Iq)
          as [Iq'|G]; [left; exact Iq'|right].
        split; [exact G|]. destruct k as [| | |r0 m0 th0|]; cbn [PublicCallRealize.key_gens]; try (intros y []).
        exact (Kr r0 m0 th0 eq_refl).
    + (* bind F = H, a copy *)
      match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        apply step_set_fun; intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq)) end.
    + (* bind F = G, a closure *)
      match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        apply step_set_fun; intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq)) end.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2]; intros Fc Ff.
    + split; [intros y I; exact I|split; [intros y I; left; exact I|split; [intros k p I; left; exact I|split; [split; reflexivity|]]]].
      apply types_kept_incl. intros q Iq. exact Iq.
    + destruct (binditem_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ D1 Fc Ff) as [Fc1 Ff1].
      exact (step_trans _ _ _ _ _ (item_scope _ _ _ _ _ _ _ D1 Fc Ff) (items_scope _ _ _ _ _ _ _ D2 Fc1 Ff1)).
Qed.

End Scope.
