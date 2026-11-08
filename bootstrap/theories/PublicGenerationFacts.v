(* Facts about Audit's live generation/output helpers, independent of
   executable elaboration, the generation prototype, and source refinement. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit PublicCaptureFacts.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Theorem bind_type_preserves_generations : forall d source rhs,
  type_meanings (bind_type d source rhs) = type_meanings d.
Proof. reflexivity. Qed.

Theorem bind_type_preserves_ground : forall d source rhs tau,
  ground (bind_type d source rhs) tau = ground d tau.
Proof. reflexivity. Qed.

Theorem bind_type_records_alias : forall d source rhs,
  lookup_subst source (type_aliases (bind_type d source rhs)) = Some (ground d rhs).
Proof. intros; cbn [bind_type type_aliases lookup_subst]; rewrite Nat.eqb_refl; reflexivity. Qed.

Lemma lookup_subst_append : forall symbol prefix suffix,
  lookup_subst symbol (prefix ++ suffix) =
    match lookup_subst symbol prefix with
    | Some meaning => Some meaning
    | None => lookup_subst symbol suffix
    end.
Proof.
  intros symbol prefix. induction prefix as [|[key value] rest IH]; intro suffix; cbn.
  - reflexivity.
  - destruct (Nat.eqb symbol key); [reflexivity|exact (IH suffix)].
Qed.

Lemma fresh_result_types_disjoint : forall writes occupied,
  fresh_result_types writes occupied = true ->
  forall symbol meaning, lookup_subst symbol occupied = Some meaning ->
    lookup_subst symbol writes = None.
Proof.
  induction writes as [|[destination value] rest IH]; intros occupied Fresh symbol meaning Found.
  - reflexivity.
  - cbn in Fresh. destruct (lookup_subst destination occupied) eqn:Vacant; [discriminate|].
    assert (Different : Nat.eqb symbol destination = false).
    { destruct (Nat.eqb symbol destination) eqn:Same; [|reflexivity].
      apply Nat.eqb_eq in Same; subst symbol. rewrite Vacant in Found; discriminate. }
    cbn [lookup_subst]. rewrite Different.
    eapply IH; [exact Fresh|]. cbn [lookup_subst]; rewrite Different; exact Found.
Qed.

Theorem fresh_result_types_preserve_occupied : forall writes occupied,
  fresh_result_types writes occupied = true ->
  forall symbol meaning, lookup_subst symbol occupied = Some meaning ->
    lookup_subst symbol (writes ++ occupied) = Some meaning.
Proof.
  intros writes occupied Fresh symbol meaning Found.
  rewrite lookup_subst_append, (fresh_result_types_disjoint writes occupied Fresh symbol meaning Found).
  exact Found.
Qed.

Theorem output_meaning_prefers_alias : forall body source meaning,
  lookup_subst source (type_aliases body) = Some meaning ->
  output_meaning body source = Some meaning.
Proof. intros; unfold output_meaning; rewrite H; reflexivity. Qed.

Theorem output_meaning_uses_unchanged_formal : forall body source,
  lookup_subst source (type_aliases body) = None ->
  output_meaning body source = lookup_subst source (type_meanings body).
Proof. intros; unfold output_meaning; rewrite H; reflexivity. Qed.

(* Both destination writes and source aliases contain exactly the same
   sequence of meanings read from the one completed body environment. *)
Theorem read_result_types_single_snapshot : forall body declared outputs writes aliases,
  read_result_types body declared outputs = Some (writes, aliases) ->
  exists meanings,
    Forall2 (fun output meaning =>
      existsb (item_eqb (ITy (fst output))) declared = true /\
      output_meaning body (fst output) = Some meaning) outputs meanings /\
    writes = combine (map snd outputs) meanings /\
    aliases = combine (map fst outputs) meanings.
Proof.
  intros body declared outputs. induction outputs as [|[source destination] rest IH];
    intros writes aliases Read.
  - cbn in Read. inversion Read; subst. exists []; repeat split; constructor.
  - cbn in Read. destruct (existsb (item_eqb (ITy source)) declared) eqn:Declared; [|discriminate].
    destruct (output_meaning body source) as [meaning|] eqn:Meaning;
      destruct (read_result_types body declared rest) as [[tail_writes tail_aliases]|] eqn:Tail;
      try discriminate.
    inversion Read; subst writes aliases.
    destruct (IH tail_writes tail_aliases eq_refl) as [meanings [Reads [Writes Aliases]]].
    exists (meaning :: meanings). split.
    + constructor; [cbn; split; assumption|exact Reads].
    + cbn; rewrite Writes, Aliases; split; reflexivity.
Qed.

Theorem result_environment_snapshot : forall caller outputs body declared provisions delta,
  result_environment caller outputs body declared provisions = Some delta ->
  exists writes aliases,
    read_result_types body declared outputs = Some (writes, aliases) /\
    fresh_result_types writes (type_meanings caller) = true /\
    type_meanings delta = writes /\ type_aliases delta = aliases /\
    provision_values delta = provision_values provisions.
Proof.
  intros caller outputs body declared provisions delta Export.
  unfold result_environment in Export.
  destruct (read_result_types body declared outputs) as [[writes aliases]|] eqn:Read; [|discriminate].
  destruct (fresh_result_types writes (type_meanings caller)) eqn:Fresh; [|discriminate].
  inversion Export; subst delta. exists writes, aliases; cbn; repeat split; assumption.
Qed.

Theorem result_environment_preserves_generation : forall caller outputs body declared provisions delta,
  result_environment caller outputs body declared provisions = Some delta ->
  forall symbol meaning, lookup_subst symbol (type_meanings caller) = Some meaning ->
    lookup_subst symbol (type_meanings (append_environment delta caller)) = Some meaning.
Proof.
  intros caller outputs body declared provisions delta Export symbol meaning Found.
  destruct (result_environment_snapshot caller outputs body declared provisions delta Export)
    as [writes [aliases [Read [Fresh [Writes [Aliases Provisions]]]]]].
  cbn [append_environment type_meanings]. rewrite Writes.
  eapply fresh_result_types_preserve_occupied; eassumption.
Qed.

(* Presence of all mentioned old generations is essential: extending the
   map can change grounding of a previously absent generation. *)
Theorem result_environment_preserves_supported_ground :
  forall caller outputs body declared provisions delta tau,
  result_environment caller outputs body declared provisions = Some delta ->
  (forall symbol, In symbol (A.gens tau) ->
    exists meaning, lookup_subst symbol (type_meanings caller) = Some meaning) ->
  ground (append_environment delta caller) tau = ground caller tau.
Proof.
  intros caller outputs body declared provisions delta tau Export Complete.
  unfold ground. apply substitution_agrees_on_generations. intros symbol Member.
  destruct (Complete symbol Member) as [meaning Found].
  rewrite (result_environment_preserves_generation caller outputs body declared provisions delta
    Export symbol meaning Found), Found. reflexivity.
Qed.

Theorem result_environment_exports_one_snapshot :
  forall caller outputs body declared provisions delta,
  result_environment caller outputs body declared provisions = Some delta ->
  exists meanings,
    Forall2 (fun output meaning =>
      existsb (item_eqb (ITy (fst output))) declared = true /\
      output_meaning body (fst output) = Some meaning) outputs meanings /\
    type_meanings delta = combine (map snd outputs) meanings /\
    type_aliases delta = combine (map fst outputs) meanings.
Proof.
  intros caller outputs body declared provisions delta Export.
  destruct (result_environment_snapshot caller outputs body declared provisions delta Export)
    as [writes [aliases [Read [Fresh [Writes [Aliases Provisions]]]]]].
  destruct (read_result_types_single_snapshot body declared outputs writes aliases Read)
    as [meanings [Reads [ReadWrites ReadAliases]]].
  exists meanings; split; [exact Reads|]. rewrite Writes, Aliases; split; assumption.
Qed.

Theorem result_environment_rejects_unfresh_snapshot :
  forall caller outputs body declared provisions writes aliases,
  read_result_types body declared outputs = Some (writes, aliases) ->
  fresh_result_types writes (type_meanings caller) = false ->
  result_environment caller outputs body declared provisions = None.
Proof. intros; unfold result_environment; rewrite H, H0; reflexivity. Qed.

Theorem result_environment_rejects_missing_snapshot :
  forall caller outputs body declared provisions,
  read_result_types body declared outputs = None ->
  result_environment caller outputs body declared provisions = None.
Proof. intros; unfold result_environment; rewrite H; reflexivity. Qed.

Example missing_output_is_rejected :
  result_environment empty_environment [(2,7)] empty_environment [ITy 2] empty_environment = None.
Proof. reflexivity. Qed.

Example undeclared_output_is_rejected :
  result_environment empty_environment [(2,7)]
    (bind_type empty_environment 2 (TMachine MI32)) [] empty_environment = None.
Proof. reflexivity. Qed.

Example occupied_destination_is_rejected :
  result_environment (Environment [(7,TMachine MF64)] []) [(2,7)]
    (bind_type empty_environment 2 (TMachine MI32)) [ITy 2] empty_environment = None.
Proof. reflexivity. Qed.

Example duplicate_destination_is_rejected :
  result_environment empty_environment [(2,7);(2,7)]
    (bind_type empty_environment 2 (TMachine MI32)) [ITy 2] empty_environment = None.
Proof. reflexivity. Qed.
