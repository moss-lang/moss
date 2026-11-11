(* Saturation of the finite set of (symbol, strictness) readings. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import EaExpand.
Import ListNotations.

Definition read_key := (nat * bool)%type.
Definition read_step := (nat * read_key)%type.

Definition ReadingStep (steps : list read_step) (a b : read_key) : Prop :=
  exists strict, In (fst a, (fst b, strict)) steps /\ snd b = snd a || strict.

Local Lemma seen_read_spec : forall acc read, seen_read acc read = true <-> In read acc.
Proof.
  intros acc [symbol strict]. unfold seen_read. rewrite existsb_exists. split.
  - intros ([other flag] & Present & Test). cbn [fst snd] in Test.
    apply andb_true_iff in Test as [Symbol Strict].
    apply Nat.eqb_eq in Symbol. apply (proj1 (Bool.eqb_true_iff _ _)) in Strict.
    subst other flag. exact Present.
  - intro Present. exists (symbol, strict). split; [exact Present|].
    cbn [fst snd]. now rewrite Nat.eqb_refl, Bool.eqb_reflx.
Qed.

Local Lemma unseen_read_spec : forall acc read,
  negb (seen_read acc read) = true <-> ~ In read acc.
Proof. intros. rewrite negb_true_iff, <- seen_read_spec. destruct (seen_read acc read); intuition discriminate. Qed.

Local Lemma next_reads_spec : forall steps frontier b,
  In b (next_reads steps frontier) <-> exists a, In a frontier /\ ReadingStep steps a b.
Proof.
  intros steps frontier [target flag]. unfold next_reads. split.
  - intros Present. apply in_flat_map in Present as ([source prior] & Head & Present).
    apply in_flat_map in Present as ([from [to strict]] & Step & Present).
    cbn [fst snd] in Present. destruct (Nat.eqb_spec from source) as [->|Different];
      [|contradiction].
    destruct Present as [Equal|[]]. inversion Equal; subst.
    exists (source, prior). split; [exact Head|exists strict; auto].
  - intros ([source prior] & Head & strict & Step & Flag).
    cbn [fst snd] in Step, Flag. subst flag.
    apply in_flat_map. exists (source, prior). split; [exact Head|].
    apply in_flat_map. exists (source, (target, strict)). split; [exact Step|].
    cbn [fst snd]. rewrite Nat.eqb_refl. now left.
Qed.

Local Definition step_targets (steps : list read_step) : list read_key :=
  flat_map (fun step => [(fst (snd step), false); (fst (snd step), true)]) steps.

Local Lemma step_targets_cover : forall steps a b,
  ReadingStep steps a b -> In b (step_targets steps).
Proof.
  intros steps [source prior] [target flag] [strict [Present Equal]].
  unfold step_targets. apply in_flat_map.
  exists (source, (target, strict)). split; [exact Present|].
  destruct flag; cbn; auto.
Qed.

Local Lemma step_targets_length : forall steps,
  List.length (step_targets steps) = 2 * List.length steps.
Proof. induction steps; [reflexivity|]. unfold step_targets in *. cbn. lia. Qed.

Local Lemma filter_length_monotone : forall A (p q : A -> bool) xs,
  (forall x, In x xs -> p x = true -> q x = true) ->
  List.length (filter p xs) <= List.length (filter q xs).
Proof.
  intros A p q xs. induction xs as [|x xs IH]; intro Implies; [reflexivity|].
  pose proof (IH (fun a Present => Implies a (or_intror Present))) as Less.
  cbn [filter]. destruct (p x) eqn:P, (q x) eqn:Q; cbn [List.length]; try lia.
  specialize (Implies x (or_introl eq_refl) P). congruence.
Qed.

Local Lemma filter_length_decreases : forall A (p q : A -> bool) xs changed,
  (forall x, In x xs -> p x = true -> q x = true) ->
  In changed xs -> p changed = false -> q changed = true ->
  List.length (filter p xs) < List.length (filter q xs).
Proof.
  intros A p q xs. induction xs as [|x xs IH]; intros changed Implies Present P Q; [contradiction|].
  cbn [filter]. destruct Present as [<-|Present].
  - rewrite P, Q. cbn [List.length].
    pose proof (filter_length_monotone A p q xs (fun a Member => Implies a (or_intror Member))).
    lia.
  - pose proof (IH _ (fun a Member => Implies a (or_intror Member)) Present P Q) as Less.
    destruct (p x) eqn:PX, (q x) eqn:QX; cbn [List.length]; try lia.
    specialize (Implies x (or_introl eq_refl) PX). congruence.
Qed.

Local Definition unseen_count steps acc :=
  List.length (filter (fun r => negb (seen_read acc r)) (step_targets steps)).

Local Lemma next_reduces_unseen : forall steps frontier acc new,
  new = filter (fun r => negb (seen_read acc r)) (next_reads steps frontier) ->
  new <> [] -> unseen_count steps (acc ++ new) < unseen_count steps acc.
Proof.
  intros steps frontier acc new New Nonempty.
  destruct new as [|changed others]; [contradiction|].
  unfold unseen_count.
  assert (Added : In changed (changed :: others)) by now left.
  rewrite New in Added. apply filter_In in Added as [Next Unseen].
  destruct (proj1 (next_reads_spec _ _ _) Next) as (source & Present & Step).
  apply filter_length_decreases with (changed := changed).
  - intros r Member Missing. apply unseen_read_spec in Missing.
    apply unseen_read_spec. intro Found. apply Missing. now apply in_or_app; left.
  - eapply step_targets_cover. exact Step.
  - destruct (negb (seen_read (acc ++ changed :: others) changed)) eqn:Missing; [|reflexivity].
    apply unseen_read_spec in Missing. exfalso. apply Missing. apply in_or_app. right. now left.
  - exact Unseen.
Qed.

Definition ReadsClosed (steps : list read_step) (acc : list read_key) : Prop :=
  forall a b, In a acc -> ReadingStep steps a b -> In b acc.

Local Definition FrontierCovers (steps : list read_step) (frontier acc : list read_key) : Prop :=
  forall a b, In a acc -> ReadingStep steps a b -> In b acc \/ In a frontier.

Local Lemma reads_closure_grows_closed : forall fuel steps frontier acc,
  unseen_count steps acc < fuel -> FrontierCovers steps frontier acc ->
  let closed := reads_closure fuel steps frontier acc in
  incl acc closed /\ ReadsClosed steps closed.
Proof.
  induction fuel as [|fuel IH]; intros steps frontier acc Enough Cover; [lia|].
  cbn [reads_closure].
  remember (filter (fun read => negb (seen_read acc read)) (next_reads steps frontier))
    as new eqn:New.
  assert (Collected : forall a b, In a acc -> ReadingStep steps a b -> In b acc \/ In b new).
  {
    intros a b Present Step. destruct (Cover a b Present Step) as [Visited|Frontier];
      [now left|].
    destruct (seen_read acc b) eqn:Seen.
    - left. now apply seen_read_spec.
    - right. rewrite New. apply filter_In. split; [|now rewrite Seen].
      apply next_reads_spec. exists a. auto.
  }
  destruct new as [|head tail].
  - split; [apply incl_refl|].
    intros a b Present Step. destruct (Collected a b Present Step) as [Seen|[]]. exact Seen.
  - assert (Fewer : unseen_count steps (acc ++ head :: tail) < unseen_count steps acc).
    { eapply next_reduces_unseen; [exact New|discriminate]. }
    destruct (IH steps (head :: tail) (acc ++ head :: tail)) as [Growth Closed]; [lia| |].
    + intros a b Present Step. apply in_app_or in Present as [Old|Newly].
      * left. now apply in_or_app, (Collected a b Old Step).
      * now right.
    + split; [|exact Closed]. intros a Present. apply Growth, in_or_app. now left.
Qed.

Theorem reads_closure_covers : forall steps root,
  let reached := reads_closure (S (2 * List.length steps)) steps [(root, false)] [(root, false)] in
  In (root, false) reached /\ ReadsClosed steps reached.
Proof.
  intros steps root.
  assert (Enough : unseen_count steps [(root, false)] < S (2 * List.length steps)).
  {
    unfold unseen_count.
    pose proof (filter_length (fun r => negb (seen_read [(root, false)] r)) (step_targets steps))
      as Count.
    rewrite step_targets_length in Count.
    apply (proj2 (Nat.lt_succ_r _ _)).
    exact (Nat.le_trans _ _ _ (Nat.le_add_r _ _) (Nat.eq_le_incl _ _ Count)).
  }
  assert (Cover : FrontierCovers steps [(root, false)] [(root, false)]) by
    (intros a b Present Step; now right).
  destruct (reads_closure_grows_closed _ _ _ _ Enough Cover) as [Growth Closed].
  split; [apply Growth; now left|exact Closed].
Qed.
