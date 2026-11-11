(* The executable Rule B check decides the class-based judgment when the
   callee has a solved merge and the table uses a section of that merge. *)
From Stdlib Require Import List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require EaCall EaBase PublicContextFacts PublicMerge PublicFormation
  PublicTypeFacts PublicTypeRead PublicSubstFacts PublicCallRealize.
Import AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.

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

Lemma min_first_none : forall A (l : list (nat * A)),
  EaCall.min_first l = None -> l = [].
Proof.
  intros A l. assert (SomeKept : forall r (p : nat * A),
    EaCall.min_first (p :: r) <> None).
  { unfold EaCall.min_first. cbn [fold_left].
    induction r as [|q rest IH]; intros p; [discriminate|].
    cbn [fold_left]. destruct (fst q <? fst p); apply IH. }
  destruct l as [|p rest]; [reflexivity|].
  intro Empty. contradiction (SomeKept rest p).
Qed.

Theorem call_theta_with_sound : forall W who C Cs read_class s th,
  PublicContextFacts.CxSection C -> PublicContextFacts.CxSection Cs ->
  EaCall.call_theta_with W who C Cs read_class = EaBase.Ok th ->
  th = map (fun q => (fst q, EaCall.call_read C Cs read_class (fst q))) (c_ty Cs) /\
  CallTheta W C Cs s th.
Proof.
  intros W who C Cs read_class s th Caller Callee Check.
  unfold EaCall.call_theta_with in Check.
  set (candidate := map (fun q => (fst q, EaCall.call_read C Cs read_class (fst q))) (c_ty Cs)) in *.
  set (fails := fun q : tsym * ty =>
    negb (ty_eqb (canon C (app_subst candidate (TAbs (fst q))))
      (canon C (app_subst candidate (snd q))))) in Check.
  destruct (EaCall.min_first (filter fails (c_sub Cs))) as [[g t]|] eqn:Bad;
    [discriminate Check|].
  injection Check as <-. apply min_first_none in Bad. split; [reflexivity|]. split.
  - unfold candidate. now rewrite map_map.
  - split.
    + intros r Ir Bound. unfold candidate.
      rewrite (PublicSubstFacts.lookup_on_keys _ _ _ Ir).
      unfold EaCall.call_read, EaCall.bound_in.
      destruct (nlook r (c_ty C)); [reflexivity|contradiction].
    + eapply PublicMerge.respects_section; [exact Callee|].
      intros g t Row. apply (PublicContextFacts.context_canon_iff_mergeeq C Caller).
      apply PublicTypeFacts.ty_eqb_spec.
      assert (Test : fails (g,t) = false).
      { destruct (fails (g,t)) eqn:E; [|reflexivity].
        assert (In (g,t) (filter fails (c_sub Cs))) by (apply filter_In; auto).
        rewrite Bad in H. contradiction. }
      apply negb_false_iff in Test. exact Test.
Qed.

Theorem call_theta_sound : forall W who C Cs s th,
  PublicContextFacts.CxSection C -> PublicContextFacts.CxSection Cs ->
  EaCall.call_theta W who C Cs = EaBase.Ok th -> CallTheta W C Cs s th.
Proof.
  intros W who C Cs s th Caller Callee Check.
  exact (proj2 (call_theta_with_sound W who C Cs (EaCall.class_value C Cs) s th Caller Callee Check)).
Qed.
