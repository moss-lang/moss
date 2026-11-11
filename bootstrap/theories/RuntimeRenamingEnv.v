(* The interpreter's environments, captures and functor results commute
   with the renaming of their symbolic keys and type meanings. *)
From Stdlib Require Import List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit RuntimeRenaming.
Import AuditBase.Semantics ListNotations.

Section Environments.
Variable rename : renaming.
Hypothesis injective : forall left right, rename left = rename right -> left = right.

Lemma rename_meanings : forall d,
  type_meanings (rename_environment rename d) = rename_subst rename (type_meanings d).
Proof. destruct d; reflexivity. Qed.

Lemma rename_aliases : forall d,
  type_aliases (rename_environment rename d) = rename_subst rename (type_aliases d).
Proof. destruct d; reflexivity. Qed.

Lemma rename_append_environment : forall a b,
  append_environment (rename_environment rename a) (rename_environment rename b) =
  rename_environment rename (append_environment a b).
Proof.
  intros [ta aa pa] [tb ab pb].
  cbn [rename_environment append_environment type_meanings type_aliases provision_values].
  unfold rename_subst. now rewrite !map_app.
Qed.

Lemma rename_install_provision : forall key entry d,
  install_provision (rename_item rename key) (rename_provision rename entry) (rename_environment rename d) =
  rename_environment rename (install_provision key entry d).
Proof. intros key entry [types aliases entries]. reflexivity. Qed.

Lemma rename_lookup_val : forall v d,
  lookup_provision (IVal (rename v)) (rename_environment rename d) =
  option_map (rename_provision rename) (lookup_provision (IVal v) d).
Proof. intros. exact (rename_lookup_provision rename injective (IVal v) d). Qed.

Lemma rename_lookup_fn : forall f d,
  lookup_provision (IFn (rename f)) (rename_environment rename d) =
  option_map (rename_provision rename) (lookup_provision (IFn f) d).
Proof. intros. exact (rename_lookup_provision rename injective (IFn f) d). Qed.

Lemma rename_lookup_fun : forall u d,
  lookup_provision (IFun (rename u)) (rename_environment rename d) =
  option_map (rename_provision rename) (lookup_provision (IFun u) d).
Proof. intros. exact (rename_lookup_provision rename injective (IFun u) d). Qed.

Lemma rename_assign_local : forall variable value g,
  assign_local variable (rename_value rename value) (rename_locals rename g) =
  option_map (rename_locals rename) (assign_local variable value g).
Proof.
  intros variable value g. induction g as [|[x old] rest IH]; [reflexivity|].
  cbn [rename_locals assign_local map].
  destruct (Nat.eqb variable x); [reflexivity|].
  unfold rename_locals in IH.
  rewrite IH. destruct (assign_local variable value rest); reflexivity.
Qed.

Lemma rename_select_provisions : forall d selection,
  select_provisions (rename_environment rename d) (rename_smap rename selection) =
  option_map (rename_environment rename) (select_provisions d selection).
Proof.
  intros d selection. induction selection as [|[required supplied] rest IH]; [reflexivity|].
  unfold rename_smap in IH.
  cbn [rename_smap select_provisions map]. rewrite rename_lookup_provision, IH by exact injective.
  destruct (lookup_provision supplied d), (select_provisions d rest); try reflexivity.
  cbn [option_map]. now rewrite rename_install_provision.
Qed.

Lemma rename_adapt_provisions : forall d adaptation,
  adapt_provisions (rename_environment rename d) (option_map (rename_smap rename) adaptation) =
  option_map (rename_environment rename) (adapt_provisions d adaptation).
Proof. intros d [selection|]; [apply rename_select_provisions|reflexivity]. Qed.

Lemma rename_smap_source : forall required selection,
  smap_source (rename_item rename required) (rename_smap rename selection) =
  option_map (rename_item rename) (smap_source required selection).
Proof.
  intros required selection. induction selection as [|[key supplied] rest IH]; [reflexivity|].
  unfold rename_smap in IH.
  cbn [rename_smap smap_source map]. rewrite rename_item_eqb by exact injective.
  destruct (item_eqb required key); [reflexivity|exact IH].
Qed.

Lemma rename_compose_adaptation : forall adaptation selection,
  compose_adaptation (option_map (rename_smap rename) adaptation) (rename_smap rename selection) =
  option_map (rename_smap rename) (compose_adaptation adaptation selection).
Proof.
  intros [captured|] selection; [|reflexivity].
  unfold compose_adaptation. cbn [option_map].
  f_equal. induction captured as [|[required supplied] rest IH]; [reflexivity|].
  cbn [rename_smap map fst snd].
  rewrite (rename_smap_source supplied selection).
  destruct (smap_source supplied selection); cbn [option_map]; f_equal; exact IH.
Qed.

Lemma rename_parameter_values : forall values,
  parameter_values (map (rename_value rename) values) = rename_locals rename (parameter_values values).
Proof.
  intros values. unfold parameter_values. rewrite length_map. generalize (seq 0 (length values)).
  induction values as [|value rest IH]; intros [|position positions]; cbn [combine map rename_locals];
    try reflexivity. now rewrite IH.
Qed.

Lemma rename_bind_type : forall d name tau,
  bind_type (rename_environment rename d) (rename name) (rename_ty rename tau) =
  rename_environment rename (bind_type d name tau).
Proof.
  intros [types aliases entries] name tau.
  unfold bind_type. rewrite rename_ground by exact injective. reflexivity.
Qed.

Lemma rename_ground_rows : forall d rows,
  map (fun row => (fst row, ground (rename_environment rename d) (snd row))) (rename_subst rename rows) =
  rename_subst rename (map (fun row => (fst row, ground d (snd row))) rows).
Proof.
  intros d rows. induction rows as [|[name tau] rest IH]; [reflexivity|].
  cbn [rename_subst map fst snd]. rewrite rename_ground by exact injective.
  f_equal. exact IH.
Qed.

Lemma rename_instantiate : forall d th,
  instantiate (rename_environment rename d) (rename_subst rename th) =
  rename_environment rename (instantiate d th).
Proof. intros. unfold instantiate. rewrite rename_ground_rows. reflexivity. Qed.

Lemma rename_typed_capture : forall d types captured,
  typed_capture (rename_environment rename d) (rename_capture rename types) (rename_environment rename captured) =
  rename_environment rename (typed_capture d types captured).
Proof.
  intros d [generations aliases] [types views entries].
  unfold typed_capture. cbn [rename_capture capture_generations capture_aliases].
  rewrite !rename_ground_rows. reflexivity.
Qed.

Lemma rename_call_environment : forall d th captured,
  call_environment (rename_environment rename d) (rename_subst rename th) (rename_environment rename captured) =
  rename_environment rename (call_environment d th captured).
Proof. intros. unfold call_environment. now rewrite rename_instantiate, rename_append_environment. Qed.

Lemma rename_primitive_closure : forall d key operation,
  primitive_closure (rename_environment rename d) (rename_item rename key) (rename_operation rename operation) =
  rename_provision rename (primitive_closure d key operation).
Proof.
  intros d key operation. destruct key; try reflexivity.
  unfold primitive_closure. cbn [rename_item rename_provision rename_site option_map].
  now rewrite rename_ground by exact injective.
Qed.

Lemma rename_output_meaning : forall d source,
  output_meaning (rename_environment rename d) (rename source) =
  option_map (rename_ty rename) (output_meaning d source).
Proof.
  intros d source. unfold output_meaning. rewrite rename_aliases, rename_meanings.
  rewrite !rename_lookup_subst by exact injective.
  destruct (lookup_subst source (type_aliases d)); reflexivity.
Qed.

Lemma rename_symbol_member : forall symbol symbols,
  existsb (Nat.eqb (rename symbol)) (map rename symbols) = existsb (Nat.eqb symbol) symbols.
Proof.
  intros symbol symbols. induction symbols as [|first rest IH]; [reflexivity|].
  cbn [existsb map]. now rewrite rename_eqb, IH by exact injective.
Qed.

Local Lemma rename_generated_rows : forall sources (read read' : nat -> ty),
  (forall source, read' (rename source) = rename_ty rename (read source)) ->
  map (fun source => (source, read' source)) (map rename sources) =
  rename_subst rename (map (fun source => (source, read source)) sources).
Proof.
  intros sources read read' Related. unfold rename_subst. rewrite !map_map.
  apply map_ext. intro source. cbn. now rewrite Related.
Qed.

Lemma rename_application_environment : forall P P' d provider arguments captured,
  views_related rename P P' ->
  application_environment P' (rename_environment rename d) (rename provider)
    (rename_subst rename arguments) (rename_environment rename captured) =
  rename_environment rename (application_environment P d provider arguments captured).
Proof.
  intros P P' d provider arguments captured Views. unfold application_environment.
  rewrite (related_functors _ _ _ Views).
  destruct (view_functor P provider) as [code|]; [|destruct captured; reflexivity].
  cbn [option_map rename_functor_code fc_inputs fc_linked fc_table].
  set (left_base := fun source =>
    match (if existsb (Nat.eqb source) (fc_linked code) then lookup_subst source arguments else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning captured source with Some meaning => meaning | None => TAbs source end
    end).
  set (right_base := fun source =>
    match (if existsb (Nat.eqb source) (map rename (fc_linked code))
           then lookup_subst source (rename_subst rename arguments) else None) with
    | Some meaning => ground (rename_environment rename d) meaning
    | None =>
        match output_meaning (rename_environment rename captured) source with
        | Some meaning => meaning | None => TAbs source end
    end).
  assert (BasesRead : forall source, right_base (rename source) = rename_ty rename (left_base source)).
  { intro source. unfold right_base, left_base.
    rewrite rename_symbol_member, rename_lookup_subst by exact injective.
    destruct (existsb (Nat.eqb source) (fc_linked code)), (lookup_subst source arguments); cbn [option_map].
    all: first [apply rename_ground; exact injective
               | rewrite rename_output_meaning; destruct (output_meaning captured source); reflexivity]. }
  set (left_bases := map (fun source => (source, left_base source)) (fc_inputs code)).
  set (right_bases := map (fun source => (source, right_base source)) (map rename (fc_inputs code))).
  assert (Bases : right_bases = rename_subst rename left_bases) by now apply rename_generated_rows.
  set (left_read := fun source =>
    match lookup_subst source (fc_table code) with Some tau => app_subst left_bases tau | None => left_base source end).
  set (right_read := fun source =>
    match lookup_subst source (rename_subst rename (fc_table code)) with
    | Some tau => app_subst right_bases tau | None => right_base source end).
  set (left_rows := map (fun source => (source, left_read source)) (fc_inputs code)).
  set (right_rows := map (fun source => (source, right_read source)) (map rename (fc_inputs code))).
  assert (Rows : right_rows = rename_subst rename left_rows).
  { apply rename_generated_rows. intros source. unfold left_read, right_read.
    rewrite rename_lookup_subst by exact injective.
    destruct (lookup_subst source (fc_table code)); cbn [option_map]; [|apply BasesRead].
    rewrite Bases. now apply rename_app_subst. }
  change (EnvironmentWithAliases right_rows right_rows (provision_values (rename_environment rename captured)) =
    rename_environment rename (EnvironmentWithAliases left_rows left_rows (provision_values captured))).
  rewrite Rows. destruct captured. reflexivity.
Qed.

Lemma rename_type_member : forall source outputs,
  existsb (item_eqb (ITy (rename source))) (map (rename_item rename) outputs) =
  existsb (item_eqb (ITy source)) outputs.
Proof.
  intros source outputs. induction outputs as [|first rest IH]; [reflexivity|].
  cbn [existsb map]. change (ITy (rename source)) with (rename_item rename (ITy source)).
  rewrite rename_item_eqb by exact injective. f_equal. exact IH.
Qed.

Lemma rename_read_result_types : forall d outputs roles,
  read_result_types (rename_environment rename d) (map (rename_item rename) outputs)
    (rename_outputs rename roles) =
  option_map (fun '(writes, aliases) => (rename_subst rename writes, rename_subst rename aliases))
    (read_result_types d outputs roles).
Proof.
  intros d outputs roles. induction roles as [|[source destination] rest IH]; [reflexivity|].
  unfold rename_outputs in IH.
  cbn [rename_outputs read_result_types map]. rewrite rename_type_member.
  destruct (existsb (item_eqb (ITy source)) outputs); [|reflexivity].
  rewrite rename_output_meaning, IH.
  destruct (output_meaning d source), (read_result_types d outputs rest) as [[writes aliases]|]; reflexivity.
Qed.

Lemma rename_result_environment : forall roles d outputs selected,
  result_environment (rename_outputs rename roles) (rename_environment rename d)
    (map (rename_item rename) outputs) (rename_environment rename selected) =
  option_map (rename_environment rename) (result_environment roles d outputs selected).
Proof.
  intros roles d outputs [types aliases entries]. unfold result_environment.
  rewrite rename_read_result_types. destruct (read_result_types d outputs roles) as [[writes aliases']|]; reflexivity.
Qed.

(* Missing copy positions use a fixed type-key sentinel. *)
Hypothesis fixed_sentinel : rename 0 = 0.

Lemma rename_closure_sources : forall positions sources,
  closure_sources positions (map (rename_item rename) sources) =
  map (rename_item rename) (closure_sources positions sources).
Proof.
  intros [positions|] sources; [|reflexivity].
  cbn [closure_sources]. rewrite map_map. apply map_ext.
  intros [position|]; [|cbn [rename_item]; now rewrite fixed_sentinel].
  replace (ITy 0) with (rename_item rename (ITy 0)) at 1 by
    (cbn [rename_item]; now rewrite fixed_sentinel).
  apply map_nth.
Qed.

Lemma rename_result_selection : forall keys positions sources,
  result_selection (map (rename_item rename) keys) positions (map (rename_item rename) sources) =
  rename_smap rename (result_selection keys positions sources).
Proof.
  intros keys positions sources. unfold result_selection, rename_smap.
  rewrite rename_closure_sources, map_rev. f_equal.
  generalize (closure_sources positions sources). clear positions sources.
  induction keys as [|key keys IH]; intros [|source sources]; cbn [map combine]; try reflexivity.
  now rewrite IH.
Qed.
End Environments.
