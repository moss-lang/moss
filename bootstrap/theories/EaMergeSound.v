(* Merge validation proves unification and generation scope. Needs
   enumerate exactly the entry keys. Principality, idempotence and the
   functional substitution invariant remain separate. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaCtx EaTypesSound PublicTypeFacts PublicCallRealize.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma nlist_eqb_spec : forall a b, nlist_eqb a b = true <-> a = b.
Proof.
  induction a as [|x a IH]; intros [|y b]; cbn; try (split; congruence).
  rewrite andb_true_iff, Nat.eqb_eq, IH. split; [intros [-> ->]; reflexivity|intro Equal; inversion Equal; auto].
Qed.

Lemma tys_eqb_spec : forall a b, tys_eqb a b = true <-> a = b.
Proof.
  induction a as [|x a IH]; intros [|y b]; cbn; try (split; congruence).
  rewrite andb_true_iff, ty_eqb_spec, IH. split; [intros [-> ->]; reflexivity|intro Equal; inversion Equal; auto].
Qed.

Lemma fsig_eqb_spec : forall a b, fsig_eqb a b = true <-> a = b.
Proof.
  intros [ps rt] [ps' rt']. unfold fsig_eqb. cbn [sg_params sg_ret].
  rewrite andb_true_iff, tys_eqb_spec, ty_eqb_spec.
  split; [intros [-> ->]; reflexivity|intro Equal; inversion Equal; auto].
Qed.

Lemma ofsig_eqb_spec : forall a b, ofsig_eqb a b = true <-> a = b.
Proof.
  intros [a|] [b|]; cbn [ofsig_eqb]; try (split; congruence).
  rewrite fsig_eqb_spec. split; congruence.
Qed.

Lemma pair_lists_equal : forall a b : list (nat * ty),
  nlist_eqb (map fst a) (map fst b) = true ->
  forallb (fun q => ty_eqb (fst q) (snd q)) (combine (map snd a) (map snd b)) = true -> a = b.
Proof.
  induction a as [|[x t] a IH]; intros [|[y u] b] Keys Types;
    cbn in Keys, Types; try discriminate; [reflexivity|].
  apply andb_true_iff in Keys as [Key Keys]. apply Nat.eqb_eq in Key.
  apply andb_true_iff in Types as [TypeEqual Types]. apply ty_eqb_spec in TypeEqual.
  subst. f_equal. exact (IH _ Keys Types).
Qed.

Lemma oinst_eqb_spec : forall a b, oinst_eqb a b = true <-> a = b.
Proof.
  intros [a|] [b|]; cbn [oinst_eqb]; try (split; congruence).
  split.
  - rewrite andb_true_iff. intros [Keys Types]. f_equal. apply pair_lists_equal; assumption.
  - intro Equal. injection Equal as <-. apply andb_true_iff. split.
    + apply nlist_eqb_spec. reflexivity.
    + induction a as [|[x t] a IH]; cbn; [reflexivity|].
      rewrite (proj2 (ty_eqb_spec t t) eq_refl), IH. reflexivity.
Qed.

Theorem unifiesb_spec : forall substitution entries,
  unifiesb substitution entries = true <-> Unifies substitution entries.
Proof.
  intros substitution entries. unfold unifiesb, Unifies. rewrite andb_true_iff. split.
  - intros [Pairs Equations]. split.
    + intros e1 e2 k First Second Key1 Key2.
      apply forallb_forall with (x:=e1) in Pairs; [|exact First].
      apply forallb_forall with (x:=e2) in Pairs; [|exact Second].
      rewrite Key1, Key2, (proj2 (item_eqb_spec k k) eq_refl) in Pairs.
      cbn in Pairs. apply andb_true_iff in Pairs as [Signature Instantiation].
      split; [apply ofsig_eqb_spec; exact Signature|apply oinst_eqb_spec; exact Instantiation].
    + intros left right Member. apply forallb_forall with (x:=NEq left right) in Equations;
        [|exact Member]. apply ty_eqb_spec. exact Equations.
  - intros [Pairs Equations]. split.
    + apply forallb_forall. intros e1 First. apply forallb_forall. intros e2 Second.
      destruct (entry_key substitution e1) as [k1|] eqn:Key1; [|reflexivity].
      destruct (entry_key substitution e2) as [k2|] eqn:Key2; [|reflexivity].
      destruct (item_eqb k1 k2) eqn:Equal; [|reflexivity].
      apply item_eqb_spec in Equal. subst k2. cbn.
      destruct (Pairs _ _ _ First Second Key1 Key2) as [Signature Instantiation].
      apply andb_true_iff. split; [apply ofsig_eqb_spec|apply oinst_eqb_spec]; assumption.
    + apply forallb_forall. intros e Member. destruct e; try reflexivity.
      apply ty_eqb_spec, Equations. exact Member.
Qed.

Theorem merge_scopeb_spec : forall generations substitution,
  merge_scopeb generations substitution = true <->
  forall g t, In (g, t) substitution -> In g generations /\ incl (gens t) generations.
Proof.
  intros generations substitution. unfold merge_scopeb. rewrite forallb_forall. split.
  - intros Checked g t Member. specialize (Checked (g,t) Member). cbn in Checked.
    apply andb_true_iff in Checked as [Domain Image]. split.
    + apply nmem_spec. exact Domain.
    + intros x Present. apply nmem_spec. exact (proj1 (forallb_forall _ _) Image x Present).
  - intros Scoped [g t] Member. cbn. destruct (Scoped _ _ Member) as [Domain Image].
    apply andb_true_iff. split; [apply nmem_spec; exact Domain|].
    apply forallb_forall. intros x Present. apply nmem_spec, Image. exact Present.
Qed.

Theorem merge_checked : forall E entries substitution classes,
  EaCtx.merge E entries = Ok (substitution, classes) ->
  Unifies substitution entries /\
  (forall g t, In (g, t) substitution ->
    In g (flat_map entry_gens entries) /\ incl (gens t) (flat_map entry_gens entries)).
Proof.
  intros E entries substitution classes Run. unfold EaCtx.merge, rbind in Run.
  destruct (merge_rounds E _ _ _ _) as [computed|message]; [|discriminate].
  destruct (unifiesb computed entries) eqn:Checked; [|discriminate].
  destruct (merge_scopeb (flat_map entry_gens entries) computed) eqn:Scoped; [|discriminate].
  injection Run as <- <-. split.
  - apply unifiesb_spec. exact Checked.
  - apply merge_scopeb_spec. exact Scoped.
Qed.

Corollary merge_unifies : forall E entries substitution classes,
  EaCtx.merge E entries = Ok (substitution, classes) -> Unifies substitution entries.
Proof. intros E entries substitution classes Run. exact (proj1 (merge_checked _ _ _ _ Run)). Qed.

Lemma imem_spec : forall key entries, imem key entries = true <-> In key entries.
Proof.
  intros key entries. induction entries as [|entry entries IH]; cbn; [split; [discriminate|contradiction]|].
  rewrite orb_true_iff, item_eqb_spec, IH. intuition congruence.
Qed.

Lemma iuniq_members : forall key entries, In key (iuniq entries) <-> In key entries.
Proof.
  intros key entries. unfold iuniq. rewrite <- in_rev.
  assert (Fold : forall xs acc,
    In key (fold_left (fun acc entry => if imem entry acc then acc else entry :: acc) xs acc)
    <-> In key xs \/ In key acc).
  { intro xs. induction xs as [|entry xs IH]; intro acc; cbn; [tauto|].
    rewrite IH. destruct (imem entry acc) eqn:Member; cbn.
    - apply imem_spec in Member. intuition congruence.
    - tauto. }
  rewrite Fold. cbn. tauto.
Qed.

Lemma iuniq_nodup : forall entries, NoDup (iuniq entries).
Proof.
  intro entries. unfold iuniq. apply NoDup_rev.
  assert (Fold : forall xs acc, NoDup acc -> NoDup
    (fold_left (fun acc entry => if imem entry acc then acc else entry :: acc) xs acc)).
  { intro xs. induction xs as [|entry xs IH]; intros acc Distinct; cbn; [exact Distinct|].
    apply IH. destruct (imem entry acc) eqn:Member; [exact Distinct|]. constructor; [|exact Distinct].
    intro Present. apply imem_spec in Present. congruence. }
  apply Fold. constructor.
Qed.

Theorem needs_exact : forall substitution entries,
  Needs substitution entries (needs substitution entries).
Proof.
  intros substitution entries. unfold Needs, needs. split; [apply iuniq_nodup|].
  intro key. rewrite iuniq_members, in_flat_map. split.
  - intros (e & Member & Read). exists e. split; [exact Member|].
    destruct (entry_key substitution e) as [found|] eqn:Found; [|contradiction].
    destruct Read as [<-|[]]. reflexivity.
  - intros (e & Member & Read). exists e. split; [exact Member|]. rewrite Read. left. reflexivity.
Qed.
