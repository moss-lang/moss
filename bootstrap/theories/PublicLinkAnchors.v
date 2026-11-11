(* A linked class uses a shared source input as the run-time reading of
   its private atomic representative. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit.
From NM Require PublicSubstFacts PublicTypeFacts PublicTypeRead.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Lemma input_anchor_read : forall Cu Cg y t,
  lookup_subst y (link_input_anchors Cu Cg) = Some t ->
  exists x original input,
    t = TAbs x /\ In (y, original) (c_ty Cg) /\ In (x, TAbs y) (c_ty Cg) /\
    nlook y (c_ty Cu) = None /\ nlook x (c_ty Cu) = Some input.
Proof.
  intros Cu Cg y t Read. apply PublicSubstFacts.lookup_subst_in_pair in Read.
  unfold link_input_anchors in Read. apply in_flat_map in Read as [[private original] [Private Read]].
  cbn [fst snd] in Read. destruct (nlook private (c_ty Cu)) eqn:NotShared; [contradiction|].
  destruct (find (fun shared => match nlook (fst shared) (c_ty Cu) with
    | Some _ => ty_eqb (snd shared) (TAbs private) | None => false end) (c_ty Cg))
    as [[x canonical]|] eqn:Anchor; [|contradiction].
  destruct Read as [Same|[]]. injection Same as <- <-.
  apply find_some in Anchor as [Source Shared]. cbn [fst snd] in Shared.
  destruct (nlook x (c_ty Cu)) as [input|] eqn:Input; [|discriminate].
  apply PublicTypeFacts.ty_eqb_spec in Shared. subst canonical.
  exists x, original, input. repeat split; assumption.
Qed.

Lemma input_anchor_keeps_shared : forall Cu Cg x,
  nlook x (c_ty Cu) <> None -> lookup_subst x (link_input_anchors Cu Cg) = None.
Proof.
  intros Cu Cg x Shared. destruct (lookup_subst x (link_input_anchors Cu Cg)) eqn:Read; [|reflexivity].
  destruct (input_anchor_read _ _ _ _ Read) as (y & original & input & _ & _ & _ & Missing & _).
  contradiction.
Qed.

Local Lemma filter_subst_read : forall select source x t,
  lookup_subst x (filter (fun q => select (fst q)) source) = Some t ->
  select x = true /\ lookup_subst x source = Some t.
Proof.
  intros select source. induction source as [|[y value] rest IH]; intros x t Read; [discriminate|].
  cbn [filter fst] in Read. cbn [lookup_subst].
  destruct (Nat.eqb x y) eqn:Equal.
  - apply Nat.eqb_eq in Equal. subst y. destruct (select x) eqn:Selected.
    + cbn in Read. rewrite Nat.eqb_refl in Read. auto.
    + apply IH in Read as [Contradiction _]. congruence.
  - destruct (select y) eqn:Selected; [cbn in Read; rewrite Equal in Read|]; now apply IH.
Qed.

Lemma adopted_input_read : forall C Cu Cg y t,
  lookup_subst y (link_adopted_inputs C Cu Cg) = Some t ->
  ~ In y (map fst (c_ty C) ++ context_gens C) /\ lookup_subst y (link_input_anchors Cu Cg) = Some t.
Proof.
  intros C Cu Cg y t Read. unfold link_adopted_inputs in Read.
  destruct (filter_subst_read (fun x => negb (existsb (Nat.eqb x)
    (map fst (c_ty C) ++ context_gens C))) _ _ _ Read) as [Absent Anchor].
  split; [|exact Anchor].
  apply negb_true_iff in Absent. intros Member.
  assert (Present : existsb (Nat.eqb y) (map fst (c_ty C) ++ context_gens C) = true).
  { apply existsb_exists. exists y. split; [exact Member|apply Nat.eqb_refl]. }
  congruence.
Qed.

Lemma anchored_input_table_read : forall Cu Cg x t,
  lookup_subst x (c_ty Cg) = Some t ->
  lookup_subst x (link_input_table Cu Cg) = Some (app_subst (link_input_anchors Cu Cg) t).
Proof.
  intros Cu Cg x t. unfold link_input_table. induction (c_ty Cg) as [|[y u] rest IH]; intro Read; [discriminate|].
  cbn [map lookup_subst fst snd] in *. destruct (Nat.eqb x y); [congruence|now apply IH].
Qed.

Lemma lexical_capture_has_no_adopted_input : forall C Cu Cg caller captured y t,
  lookup_subst y (link_adopted_inputs C Cu Cg) = Some t ->
  output_meaning (typed_capture caller (lexical_capture C) captured) y = None.
Proof.
  intros C Cu Cg caller captured y t Adopted.
  destruct (adopted_input_read _ _ _ _ _ Adopted) as [Absent _]. unfold output_meaning.
  destruct (lookup_subst y (type_aliases (typed_capture caller (lexical_capture C) captured))) as [alias|] eqn:Alias.
  - exfalso. apply Absent, in_or_app. left.
    apply PublicSubstFacts.lookup_subst_in_pair in Alias. cbn in Alias. rewrite map_map in Alias.
    apply in_map_iff in Alias as [[x v] [Same Member]]. cbn in Same. injection Same as KeyEqual _. subst y.
    exact (in_map fst _ (x,v) Member).
  - destruct (lookup_subst y (type_meanings (typed_capture caller (lexical_capture C) captured))) as [meaning|] eqn:Meaning; [|reflexivity].
    exfalso. apply Absent, in_or_app. right.
    apply PublicSubstFacts.lookup_subst_in_pair in Meaning. cbn in Meaning. rewrite map_map in Meaning.
    apply in_map_iff in Meaning as [x [Same Member]]. cbn in Same. injection Same as KeyEqual _. subst y. exact Member.
Qed.

Lemma adoption_keeps_source_scope : forall C Cu Cg t,
  incl (gens t) (map fst (c_ty Cg)) ->
  incl (gens (app_subst (link_adopted_inputs C Cu Cg) t)) (map fst (c_ty Cg)).
Proof.
  intros C Cu Cg t Scope y InType. apply PublicSubstFacts.gens_subst in InType as [x [Input [Absent|Substituted]]].
  - destruct Absent as [_ ->]. exact (Scope x Input).
  - destruct Substituted as [v [Read Member]]. apply adopted_input_read in Read as [_ Read].
    destruct (input_anchor_read _ _ _ _ Read) as (shared & original & fixed & -> & _ & Shared & _ & _).
    destruct Member as [<-|[]]. exact (in_map fst _ (shared, TAbs x) Shared).
Qed.
