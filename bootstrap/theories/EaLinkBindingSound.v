(* The checks of a closure's source binding establish its typing evidence. *)
From Stdlib Require Import List Bool.
From NM Require Import Audit EaBase EaCall EaFunctor EaMergeSound EaCopyNeedSound
  EaResultBudget EaTypesSound.
From NM Require EaSatisfiesSound PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Local Lemma bound_in_spec : forall C t,
  bound_in C t = true <-> nlook t (c_ty C) <> None.
Proof.
  intros C t. unfold bound_in. destruct (nlook t (c_ty C)); simpl; split; congruence.
Qed.

Local Lemma oprovision_eqb_spec : forall a b,
  oprovision_eqb a b = true <-> a = b.
Proof.
  intros [a|] [b|]; cbn [oprovision_eqb]; try (split; congruence).
  rewrite provision_eqb_spec. split; congruence.
Qed.

Theorem check_link_binding_sound : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  forall u gg Cu nsu Cg nsg s after target,
  check_link_binding E u gg Cu nsu Cg nsg s = Ok (target, after) ->
  after = s /\ LinkBinding (e_W E) u Cu nsu Cg nsg target.
Proof.
  intros E bound Bounded Budget u gg Cu nsu Cg nsg s after target Checked.
  unfold check_link_binding, mbind in Checked.
  destruct (find (unsafe_capture Cu nsu Cg (EaTypes.fixed (e_W E) (e_metadata_fuel E) u)) nsg)
    as [offender|] eqn:Capture; [discriminate|].
  destruct (forallb (linked_provision_agrees Cu nsu Cg) nsg) eqn:Provision; [|discriminate].
  destruct (forallb (fun q => linked_type_agrees Cu Cg (fst q)) (c_ty Cg)) eqn:Types; [|discriminate].
  injection Checked as <- <-. split; [reflexivity|]. constructor; [reflexivity| | |].
  - intros k p t InNeed NotLinked Lookup Mentions Bound.
    pose proof (find_none _ _ Capture k InNeed) as Safe.
    unfold unsafe_capture, linked_need in Safe.
    destruct (imem (canon_item Cu k) nsu) eqn:Linked.
    { exfalso. apply NotLinked, imem_spec, Linked. }
    rewrite Lookup in Safe. apply negb_false_iff in Safe.
    pose proof (proj1 (forallb_forall _ _) Safe t Mentions) as TypeSafe.
    cbn beta in TypeSafe.
    rewrite (proj2 (bound_in_spec Cu t) Bound) in TypeSafe.
    apply (fixed_exact _ _ Bounded _ _ Budget _), nmem_spec, TypeSafe.
  - intros k InNeed Linked. pose proof (proj1 (forallb_forall _ _) Provision k InNeed) as Agrees.
    unfold linked_provision_agrees, linked_need in Agrees.
    rewrite (proj2 (imem_spec _ _) Linked) in Agrees. now apply oprovision_eqb_spec.
  - intros x tx Lookup Bound.
    pose proof (PublicTypeRead.nlook_in _ _ _ _ Lookup) as Member.
    pose proof (proj1 (forallb_forall _ _) Types (x, tx) Member) as Agrees.
    cbn [fst] in Agrees. unfold linked_type_agrees in Agrees.
    rewrite Lookup, (proj2 (bound_in_spec Cu x) Bound) in Agrees.
    apply andb_true_iff in Agrees as [Scoped Equal]. split.
    + intros y InType. apply bound_in_spec. exact (proj1 (forallb_forall _ _) Scoped y InType).
    + now apply PublicTypeFacts.ty_eqb_spec.
Qed.

Theorem translate_link_targets_sound : forall Cu Cg target needs,
  translate_link_targets Cu Cg target needs = link_map Cu Cg target needs.
Proof.
  intros Cu Cg target needs. unfold translate_link_targets, link_map.
  apply map_ext. intro k. now rewrite !EaSatisfiesSound.rkof_exact.
Qed.
