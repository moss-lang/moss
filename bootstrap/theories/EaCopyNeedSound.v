(* The executable chooses a provision only after checking the site
   interpretation and resolving any remaining choice in the declared merges. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit EaBase EaMergeSound.
From NM Require EaFunctor EaCtx EaUnifySound PublicCallRealize PublicMerge PublicTypeFacts.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Local Lemma oty_eqb_spec : forall a b, oty_eqb a b = true <-> a = b.
Proof.
  intros [a|] [b|]; cbn [oty_eqb]; try (split; congruence).
  rewrite PublicTypeFacts.ty_eqb_spec. split; congruence.
Qed.

Local Lemma inst_eqb_spec : forall a b, inst_eqb a b = true <-> a = b.
Proof.
  induction a as [|[x t] a IH]; intros [|[y u] b]; cbn [inst_eqb]; try (split; congruence).
  rewrite !andb_true_iff, Nat.eqb_eq, oty_eqb_spec, IH.
  split; intros; intuition congruence.
Qed.

Lemma provision_eqb_spec : forall a b, provision_eqb a b = true <-> a = b.
Proof.
  intros [] []; cbn [provision_eqb]; try (split; congruence);
    rewrite ?andb_true_iff, ?PublicTypeFacts.ty_eqb_spec, ?fsig_eqb_spec,
      ?inst_eqb_spec, ?PublicCallRealize.subst_eqb_spec;
    split; intros; intuition congruence.
Qed.

Lemma copy_need_candidates_spec : forall C s needs k p other,
  In other (EaFunctor.copy_need_candidates C s needs k p) <->
  In other needs /\ CopyNeedAt C s k p other.
Proof.
  intros C s needs k p other. unfold EaFunctor.copy_need_candidates, CopyNeedAt.
  rewrite filter_In, andb_true_iff, PublicCallRealize.item_eqb_spec.
  destruct (ilook (provision_id other) (c_provisions C)) as [p'|].
  - rewrite provision_eqb_spec. split.
    + intros [Member [Key Provision]]. split; [exact Member|]. split; [exact Key|].
      exists p'. split; [reflexivity|exact Provision].
    + intros [Member [Key [q [Lookup Provision]]]]. injection Lookup as <-. auto.
  - split; [intros [_ [_ Bad]]; discriminate Bad|intros [_ [_ [q [Bad _]]]]; discriminate Bad].
Qed.

Theorem choose_copy_need_sound : forall E source name Cu Ch s needs k p target,
  EaFunctor.choose_copy_need E source name Cu Ch s needs k p = Ok target ->
  CopyNeedChoice Cu Ch s needs k p target.
Proof.
  intros E source name Cu Ch s needs k p target Checked.
  unfold EaFunctor.choose_copy_need in Checked.
  destruct (EaFunctor.copy_need_candidates Cu s needs k p) as [|a [|b rest]] eqn:Candidates.
  - destruct (existsb _ needs); discriminate.
  - injection Checked as <-.
    assert (Candidate : forall other, In other [a] <-> In other needs /\ CopyNeedAt Cu s k p other).
    { intro other. rewrite <- Candidates. apply copy_need_candidates_spec. }
    destruct (proj1 (Candidate a) (or_introl eq_refl)) as [Member At].
    split; [exact Member|]. split; [exact At|]. left.
    intros other InNeeds AtSite. pose proof (proj2 (Candidate other) (conj InNeeds AtSite)) as InChoice.
    cbn in InChoice. intuition congruence.
  - destruct (EaCtx.merge E (merge_equations (c_merge Cu) ++ merge_equations (c_merge Ch)))
      as [[section M]|] eqn:Merged; [|discriminate].
    destruct (filter (item_eqbM M k) (a :: b :: rest)) as [|chosen [|extra choices]] eqn:Chosen;
      try discriminate.
    injection Checked as <-.
    destruct (EaUnifySound.merge_sound _ _ _ _ Merged) as [_ [Merge _]].
    assert (Candidate : forall other,
      In other [chosen] <-> In other needs /\ CopyNeedAt Cu s k p other /\ KeyEq M k other).
    { intro other. rewrite <- Chosen, filter_In, (PublicMerge.item_eqbM_spec (proj1 Merge)).
      rewrite <- Candidates, copy_need_candidates_spec. tauto. }
    destruct (proj1 (Candidate chosen) (or_introl eq_refl)) as [Member [At Equal]].
    split; [exact Member|]. split; [exact At|]. right.
    exists M. split; [exact Merge|]. split; [exact Equal|].
    intros other InNeeds AtSite Eq. pose proof (proj2 (Candidate other) (conj InNeeds (conj AtSite Eq))) as InChoice.
    cbn in InChoice. intuition congruence.
Qed.

(* The generated equations preserve every declared class, independently
   of which member supplies the rows of a particular section. *)
Lemma merge_equations_solve : forall M s,
  Unifies s (merge_equations M) -> PublicMerge.SolvesClasses M s.
Proof.
  intros M s [_ Read].
  assert (Member : forall c h rest x,
    In c M -> mc_members c = h :: rest -> In x (h :: rest) ->
    app_subst s (TAbs h) = app_subst s (TAbs x)).
  { intros c h rest x InClass Members [<-|InRest]; [reflexivity|].
    apply Read. unfold merge_equations. apply in_flat_map.
    exists c. split; [exact InClass|]. rewrite Members.
    destruct (mc_shape c) as [t|]; [right|]; apply in_map_iff; exists x; auto. }
  split.
  - intros c x y InClass InX InY.
    destruct (mc_members c) as [|h rest] eqn:Members; [contradiction|].
    rewrite <- (Member c h rest x InClass Members InX).
    exact (Member c h rest y InClass Members InY).
  - intros c x t InClass InX Shape.
    destruct (mc_members c) as [|h rest] eqn:Members; [contradiction|].
    rewrite <- (Member c h rest x InClass Members InX).
    apply Read. unfold merge_equations. apply in_flat_map.
    exists c. split; [exact InClass|]. rewrite Members, Shape. now left.
Qed.

Theorem combined_merge_extends : forall M N joint,
  MergeOf (merge_equations M ++ merge_equations N) joint ->
  (forall t u, MergeEq M t u -> MergeEq joint t u) /\
  (forall t u, MergeEq N t u -> MergeEq joint t u).
Proof.
  intros M N joint Merge.
  assert (Restrict : forall es, incl es (merge_equations M ++ merge_equations N) ->
    forall s, Unifies s (merge_equations M ++ merge_equations N) -> Unifies s es).
  { intros es Incl s [Items Equations]. split; eauto. }
  split.
  - intros t u Eq. apply (PublicMerge.merge_semantic_reading Merge).
    intros s Unifies. apply (PublicMerge.merge_eq_sound (M := M)); [|exact Eq].
    apply merge_equations_solve, Restrict; [|exact Unifies].
    intros x Member. apply in_or_app. now left.
  - intros t u Eq. apply (PublicMerge.merge_semantic_reading Merge).
    intros s Unifies. apply (PublicMerge.merge_eq_sound (M := N)); [|exact Eq].
    apply merge_equations_solve, Restrict; [|exact Unifies].
    intros x Member. apply in_or_app. now right.
Qed.

Local Lemma combined_key_same : forall entries M N k k',
  MergeOf entries M -> MergeOf entries N ->
  KeyEq M k k' <-> KeyEq N k k'.
Proof.
  intros entries M N [] [] First Second; cbn [KeyEq]; try reflexivity.
  now rewrite (PublicMerge.merge_presentation_independent First Second).
Qed.

Theorem copy_need_choice_unique : forall Cu Ch s needs k p a b,
  CopyNeedChoice Cu Ch s needs k p a ->
  CopyNeedChoice Cu Ch s needs k p b -> a = b.
Proof.
  intros Cu Ch s needs k p a b [InA [AtA [OnlyA|(M & MergeM & EqA & OnlyA)]]]
    [InB [AtB [OnlyB|(N & MergeN & EqB & OnlyB)]]].
  - symmetry. exact (OnlyA b InB AtB).
  - symmetry. exact (OnlyA b InB AtB).
  - exact (OnlyB a InA AtA).
  - apply (OnlyB a InA AtA). exact (proj1 (combined_key_same _ _ _ _ _ MergeM MergeN) EqA).
Qed.
