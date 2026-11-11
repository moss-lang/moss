(* The executable Rule B check decides the class-based judgment when the
   callee has a solved merge and the table uses a section of that merge. *)
From Stdlib Require Import List Bool.
From NM Require Import Audit.
From NM Require EaCall EaBase PublicContextFacts PublicMerge PublicFormation
  PublicTypeFacts PublicTypeRead PublicSubstFacts PublicCallRealize.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Lemma bound_reading_spec : forall C Cs r, PublicContextFacts.CxSection Cs ->
  EaCall.has_bound_reading C Cs (canon Cs (TAbs r)) = true <-> InForceClass C (c_merge Cs) r.
Proof.
  intros C Cs r Section. unfold EaCall.has_bound_reading, InForceClass. rewrite existsb_exists. split.
  - intros [[y t] [Iq B]]. exists y. split.
    + apply (PublicContextFacts.context_canon_iff_mergeeq Cs Section).
      apply PublicTypeFacts.ty_eqb_spec. exact B.
    + rewrite <- PublicCallRealize.lookup_subst_nlook'. apply PublicSubstFacts.lookup_subst_in.
      change y with (fst (y,t)). apply in_map. exact Iq.
  - intros [y [Eq Found]]. destruct (nlook y (c_ty C)) as [t|] eqn:L; [|contradiction].
    exists (y,t). split; [exact (PublicTypeRead.nlook_in _ _ _ _ L)|].
    apply PublicTypeFacts.ty_eqb_spec, (PublicContextFacts.context_canon_iff_mergeeq Cs Section). exact Eq.
Qed.

Lemma bound_leaf_spec : forall C Cs y, PublicContextFacts.CxSection Cs ->
  (EaCall.bound_in C y || EaCall.has_bound_reading C Cs (canon Cs (TAbs y))) = true <->
  InForceClass C (c_merge Cs) y.
Proof.
  intros C Cs y Section. rewrite orb_true_iff, (bound_reading_spec C Cs y Section). split.
  - intros [Bound|Bound]; [|exact Bound]. exists y. split; [apply MERefl|].
    unfold EaCall.bound_in in Bound. destruct (nlook y (c_ty C)); cbn [EaBase.is_some] in Bound; congruence.
  - intro Bound. right. exact Bound.
Qed.

Local Lemma nonabs_test : forall t,
  (match t with TAbs _ => false | _ => true end) = true <-> forall z, t <> TAbs z.
Proof.
  intros []; split; try (intros _ y E; discriminate E); try reflexivity.
  - discriminate.
  - intro H. exfalso. exact (H _ eq_refl).
Qed.

Theorem in_force_through_spec : forall C Cs r,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  EaCall.in_force_through C Cs r = true <-> InForceThrough C Cs r.
Proof.
  intros C Cs r S Section. unfold EaCall.in_force_through, InForceThrough.
  rewrite orb_true_iff, andb_true_iff, nonabs_test, forallb_forall, (bound_reading_spec C Cs r Section).
  setoid_rewrite (bound_leaf_spec C Cs _ Section).
  pose proof (@PublicMerge.merge_eq_read _ _ (proj1 Section) (TAbs r)) as Read.
  pose proof (PublicFormation.section_output_expanded _ _ S Section (TAbs r)) as Expanded.
  change (expand (c_merge Cs) (canon Cs (TAbs r)) = canon Cs (TAbs r)) in Expanded.
  pose proof (PublicMerge.expand_outer S Read) as Outer.
  pose proof (@PublicMerge.expanded_in_force C _ _ _ S Read) as Leaves.
  change (type_outer (expand (c_merge Cs) (TAbs r)) =
    type_outer (expand (c_merge Cs) (canon Cs (TAbs r)))) in Outer.
  change ((forall x, In x (gens (expand (c_merge Cs) (TAbs r))) -> InForceClass C (c_merge Cs) x) <->
    (forall x, In x (gens (expand (c_merge Cs) (canon Cs (TAbs r)))) -> InForceClass C (c_merge Cs) x)) in Leaves.
  rewrite Expanded in Outer, Leaves.
  rewrite (@PublicMerge.type_outer_nonabs _ _ Outer), Leaves. reflexivity.
Qed.

Theorem requirements_in_force_spec : forall C Cs,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  EaCall.requirements_in_force C Cs = true <-> RequirementsInForce C Cs.
Proof.
  intros C Cs S Section. unfold EaCall.requirements_in_force, RequirementsInForce. rewrite forallb_forall. split.
  - intros Test r Ir. apply in_map_iff in Ir as [q [<- Iq]].
    apply (in_force_through_spec C Cs _ S Section). exact (Test q Iq).
  - intros Test q Iq. apply (in_force_through_spec C Cs _ S Section), Test, in_map. exact Iq.
Qed.

Theorem rule_b_sound : forall W who C Cs,
  Solved (c_merge Cs) -> PublicContextFacts.CxSection Cs ->
  EaCall.rule_b W who C Cs = EaBase.Ok tt -> RequirementsInForce C Cs.
Proof.
  intros W who C Cs S Section Check. apply (requirements_in_force_spec C Cs S Section).
  unfold EaCall.rule_b in Check.
  destruct (find (fun q => negb (EaCall.in_force_through C Cs (fst q))) (c_ty Cs)) as [[r t]|] eqn:Found; [discriminate Check|].
  unfold EaCall.requirements_in_force. apply forallb_forall. intros q Iq.
  apply negb_false_iff.
  exact (@find_none _ (fun p => negb (EaCall.in_force_through C Cs (fst p)))
    (c_ty Cs) Found q Iq).
Qed.
