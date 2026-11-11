(* Equivariance of the actual bootstrap host and its public execution. *)
From Stdlib Require Import List Bool PrimInt63 Uint63.
From NM Require Import Audit AuditBase RuntimeRenaming BaseRenaming BaseHostRenaming BaseHostMemoryRenaming.
From NM Require BaseHost BaseHostScalars MossX ExecutionModel PublicOperationFacts PublicBoolTyping InterpreterRenaming.
Import AuditBase.Semantics MossX ListNotations.
Import ExecutionModel (xvalue(..), xout(..), xprog(..), xp_bool, xp_true, xp_false, xp_argv, prim, base_primitive).
Local Open Scope uint63_scope.
Local Opaque loop_fuel.

Section Concrete.
Variable rename : nat -> nat.
Local Notation move := (rename_state rename).
Local Notation stored := (rename_stored rename).
Local Notation step := (rename_step rename).
Local Notation action := (base_renaming rename).

Lemma unaddr_renamed : forall value, unaddr (stored value) = stored (unaddr value).
Proof. intro w. destruct w; try reflexivity. destruct w; reflexivity. Qed.

Lemma mem_args_renamed : forall primitive values,
  mem_args primitive (map stored values) = map stored (mem_args primitive values).
Proof.
  intros primitive values. destruct primitive; try reflexivity;
    try (cbn [mem_args]; rewrite !map_map; apply map_ext, unaddr_renamed).
  all: destruct values as [|a [|b [|c rest]]]; try reflexivity;
    destruct a; reflexivity.
Qed.

Lemma keep_heap_renamed : forall original state,
  keep_heap (move original) (move state) = move (keep_heap original state).
Proof. reflexivity. Qed.

Lemma rename_plain_primitive : forall primitive,
  Heap.is_gc primitive = false -> BaseCall.cast_operation primitive = None ->
  rename_base_primitive rename primitive = primitive.
Proof. destruct primitive; cbn [Heap.is_gc BaseCall.cast_operation]; intros; try reflexivity; discriminate. Qed.

Lemma rename_unsupported : forall operation,
  decode operation = None -> rename_base_operation rename operation = operation.
Proof. intros operation Decode. unfold rename_base_operation. now rewrite Decode. Qed.

Lemma xbool_renamed : forall P condition,
  rename (xp_bool P) = xp_bool P ->
  rename (xp_true P) = xp_true P ->
  rename (xp_false P) = xp_false P ->
  stored (xbool P condition) = xbool P condition.
Proof.
  intros P [] Bool True False; cbn [xbool rename_stored]; now rewrite Bool, ?True, ?False.
Qed.

Lemma conv_op_renamed : forall kind value,
  conv_op kind (stored value) = conv_op kind value.
Proof. intros kind w. destruct kind, w; reflexivity. Qed.

Lemma conv64_op_renamed : forall kind value,
  conv64_op kind (stored value) = conv64_op kind value.
Proof. intros kind w. destruct kind, w; reflexivity. Qed.

Ltac split_input_value name :=
  try (let input := constr:(name) in
    match goal with
    | |- context [rename_stored _ input] =>
        destruct input; cbn [rename_stored]; try reflexivity
    end).

Theorem plain_step_renamed : forall P primitive values state,
  rename (xp_bool P) = xp_bool P ->
  rename (xp_true P) = xp_true P ->
  rename (xp_false P) = xp_false P ->
  is_gc_prim primitive = false ->
  run_prim0 P primitive (map stored values) (move state) =
  step (run_prim0 P primitive values state).
Proof.
  intros P primitive values state Bool True False NotGC.
  destruct primitive; cbn [is_gc_prim] in NotGC; try discriminate NotGC;
    destruct values as [|arg1 [|arg2 [|arg3 [|arg4 [|arg5 [|arg6 [|arg7 [|arg8 [|arg9 [|extra rest]]]]]]]]]];
    cbn [map run_prim0 ext_prim gc_prim]; try reflexivity.
  all: rewrite ?conv_op_renamed, ?conv64_op_renamed.
  (* Only split the arguments still tested by the selected primitive. *)
  all: split_input_value arg1; split_input_value arg2; split_input_value arg3;
    split_input_value arg4; split_input_value arg5; split_input_value arg6;
    split_input_value arg7; split_input_value arg8; split_input_value arg9.
  all: try rewrite ?bounds_renamed, ?load_le_renamed, ?store_le_renamed,
    ?load_op_renamed, ?store_op_renamed, ?i64_load_renamed, ?i64_store_renamed,
    ?mem_grow_renamed, ?copy_bytes_renamed, ?fill_bytes_renamed,
    ?fd_write_renamed, ?fd_read_renamed, ?fd_close_renamed, ?path_open_renamed,
    ?args_sizes_get_renamed, ?args_get_renamed.
  all: try reflexivity.
  all: try (unfold okv, rename_step; cbn [fst snd rename_machine_out]; now rewrite xbool_renamed).
  all: try match goal with
    | |- context [conv_op ?kind ?input] =>
        destruct kind; try (is_var input; destruct input); cbn [conv_op]
    | |- context [conv64_op ?kind ?input] =>
        destruct kind; try (is_var input; destruct input); cbn [conv64_op]
    | |- context [i64_un ?kind _] => destruct kind
    end; try reflexivity.
  (* Compare success and trap results once both sides have the same guard. *)
  all: repeat match goal with
    | |- context [if ?condition then _ else _] => destruct condition; try reflexivity
    | |- context [option_map _ ?result] =>
        lazymatch type of result with
        | option (int * mstate) => destruct result as [[count next]|]
        | option mstate => destruct result as [next|]
        end; cbn [option_map rename_count_state]; try reflexivity
    | |- context [rename_count_state _ ?result] =>
        destruct result as [count next]; cbn [rename_count_state]; try reflexivity
    | |- context [i32_bin ?kind ?left ?right] => destruct (i32_bin kind left right); reflexivity
    | |- context [i64_bin ?kind ?left ?right] => destruct (i64_bin kind left right); reflexivity
    | |- context [f64_bits ?bits] => destruct (f64_bits bits); try reflexivity
    | |- context [load_op ?kind ?state ?address] => destruct (load_op kind state address); reflexivity
    | |- context [i64_load ?kind ?state ?address] => destruct (i64_load kind state address); reflexivity
    end.
Qed.
Lemma split_memory_arguments_renamed : forall primitive values,
  BaseCall.split_memory_arguments primitive (map (rename_value action) values) =
  let '(site, arguments) := BaseCall.split_memory_arguments primitive values in
  (rename_site action site, map (rename_value action) arguments).
Proof.
  intros primitive values. unfold BaseCall.split_memory_arguments.
  destruct (BaseCall.memory_operation primitive); [|reflexivity].
  destruct values as [|[|tag payload|fields|[sort bits|address [[kind tau]|]]] rest]; reflexivity.
Qed.

Hypothesis injective : forall left right, rename left = rename right -> left = right.

Lemma scratch_memory_key_renamed : forall state site,
  scratch_memory_key (move state) (rename_site action site) =
  rename_site action (scratch_memory_key state site).
Proof.
  intros state [tau|]; [destruct tau|]; try reflexivity.
  unfold scratch_memory_key. cbn [rename_site option_map rename_ty rename_state ms_memory_parameter].
  destruct (ms_memory_parameter state) as [parameter|]; [|reflexivity].
  cbn [option_map]. fold (rename_subst action l).
  pose proof (rename_lookup_subst action injective parameter l) as Lookup.
  cbn [base_renaming rename_symbol] in Lookup.
  rewrite Lookup.
  destruct (lookup_subst parameter l) as [key|]; [|reflexivity].
  cbn [option_map]. change (TAbs (rename parameter)) with (rename_ty action (TAbs parameter)).
  rewrite rename_ty_eqb by exact injective.
  destruct (ty_eqb key (TAbs parameter)); reflexivity.
Qed.

Lemma find_scratch_memory_renamed : forall key memories,
  find_scratch_memory (rename_ty action key) (map (rename_memory_row rename) memories) =
  find_scratch_memory key memories.
Proof.
  intros key memories. induction memories as [|[other memory] rest IH]; [reflexivity|].
  cbn [map rename_memory_row fst snd find_scratch_memory]. rewrite rename_ty_eqb by exact injective.
  now destruct (ty_eqb key other).
Qed.

Lemma set_scratch_memory_renamed : forall key memory memories,
  set_scratch_memory (rename_ty action key) memory (map (rename_memory_row rename) memories) =
  map (rename_memory_row rename) (set_scratch_memory key memory memories).
Proof.
  intros key memory memories. induction memories as [|[other old] rest IH]; [reflexivity|].
  cbn [map rename_memory_row fst snd set_scratch_memory]. rewrite rename_ty_eqb by exact injective.
  now destruct (ty_eqb key other); cbn; try rewrite IH.
Qed.

Lemma in_memory_renamed : forall site left right state,
  (forall state,
    right (move state) = let '(out, next) := left state in (rename_signal action out, move next)) ->
  in_memory (scratch_memory_key (move state) (rename_site action site)) right (move state) =
  let '(out, next) := in_memory (scratch_memory_key state site) left state in (rename_signal action out, move next).
Proof.
  intros site left right state Steps.
  rewrite scratch_memory_key_renamed. unfold in_memory.
  destruct (scratch_memory_key state site) as [key|]; [|apply Steps].
  cbn [rename_site option_map]. change (ms_scratch_memories (move state)) with
    (map (rename_memory_row rename) (ms_scratch_memories state)).
  rewrite find_scratch_memory_renamed.
  change (with_linear_memory (move state) _ _) with
    (move (with_linear_memory state (find_scratch_memory key (ms_scratch_memories state))
      (ms_scratch_memories state))).
  rewrite Steps. destruct (left _) as [out next].
  change (linear_memory_of (move next)) with (linear_memory_of next).
  rewrite set_scratch_memory_renamed. reflexivity.
Qed.

Theorem raw_primitive_renamed : forall P P' operation values state,
  views_related action P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  BaseHost.primitive P' (rename_base_operation rename operation)
    (map (rename_value action) values) (move state) =
  let '(out, next) := BaseHost.primitive P operation values state in
  (rename_signal action out, move next).
Proof.
  intros P P' operation values state Views Bool True False.
  rewrite (base_host_ignores_renamed_code action P P' Views Bool True False).
  destruct (decode operation) as [primitive|] eqn:Decode.
  - apply PublicOperationFacts.decode_canonical in Decode. subst operation.
    destruct (Heap.is_gc primitive) eqn:IsGC; [now apply raw_gc_renamed|].
    destruct (BaseCall.cast_operation primitive) as [[array tag]|] eqn:Cast.
    { destruct primitive; try discriminate. unfold BaseHost.primitive, BaseHost.primitive_in.
      rewrite rename_base_encoding, !BaseHost.decode_host_encode.
      reflexivity. }
    pose proof (rename_plain_primitive primitive IsGC Cast) as Fixed.
    unfold BaseHost.primitive, BaseHost.primitive_in, BaseHost.primitive_decoded, BaseHost.with_memory_arguments.
    rewrite rename_base_encoding, Fixed, !BaseHost.decode_host_encode, split_memory_arguments_renamed.
    destruct (BaseCall.split_memory_arguments primitive values) as [site arguments].
    apply in_memory_renamed. intros current.
    pose proof (gc_arguments_renamed rename primitive arguments) as Arguments.
    rewrite Fixed in Arguments.
    unfold BaseHost.primitive_step. rewrite !run_prim_fast_spec.
    unfold run_prim. rewrite BaseHost.concrete_is_gc, IsGC.
    rewrite Arguments, mem_args_renamed.
    rewrite plain_step_renamed; try assumption.
    + destruct (run_prim0 _ _ _ _) as [out next].
      cbn [rename_step fst snd]. now rewrite from_signal_renamed, keep_heap_renamed.
    + now rewrite BaseHost.concrete_is_gc.
  - rewrite (rename_unsupported operation Decode).
    unfold BaseHost.primitive, BaseHost.primitive_in, BaseHost.decode_host, BaseHost.decode_host_operation.
    now rewrite Decode.
Qed.

Definition host_states_related (left right : mstate) : Prop := right = move left.

Lemma concrete_raw_calls_related : forall P P',
  views_related action P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  raw_calls_related rename host_states_related BaseHost.primitive P P'.
Proof.
  intros P P' Views Bool True False operation values left right Related.
  unfold host_states_related in Related. subst right.
  rewrite (raw_primitive_renamed P P' operation values left Views Bool True False).
  destruct (BaseHost.primitive P operation values left); split; reflexivity.
Qed.
End Concrete.

Module Public := BaseHost.Public.
Module Execution := InterpreterRenaming.PublicInterpreterRenaming BaseHost.Concrete Public.

Definition default_interfaces (left right : Public.interface) : Prop :=
  left = Public.default_interface /\ right = Public.default_interface.

Lemma default_raw_calls_related : forall rename P P',
  (forall left right, rename left = rename right -> left = right) ->
  views_related (base_renaming rename) P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  raw_calls_related rename
    (Execution.R.machine_states_related default_interfaces (host_states_related rename))
    Public.ReferenceHost.raw_primitive P P'.
Proof.
  intros rename P P' Injective Views Bool True False operation values
    [interface [argv read state]] [interface' [argv' read' state']] Related.
  destruct Related as [[Left Right] [Args [Read External]]].
  cbn [Public.arguments Public.read_file Public.external fst snd] in Left, Right, Args, Read, External.
  unfold host_states_related in External. subst interface interface' argv' read' state'.
  cbn [Public.ReferenceHost.raw_primitive Public.default_interface Public.supports
    Public.primitive Public.external Public.arguments Public.read_file].
  unfold BaseHost.Concrete.primitive.
  rewrite (raw_primitive_renamed rename Injective P P' operation values state Views Bool True False).
  destruct (BaseHost.primitive P operation values state); repeat split; reflexivity.
Qed.

Theorem default_primitive_calls_related : forall rename P P',
  (forall left right, rename left = rename right -> left = right) ->
  views_related (base_renaming rename) P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  Execution.N.E.primitives_related (base_renaming rename)
    (Execution.R.machine_states_related default_interfaces (host_states_related rename)) P P'.
Proof.
  intros rename P P' Injective Views Bool True False.
  unfold Execution.N.E.primitives_related, Public.ReferenceHost.run_prim, Base.call.
  apply base_call_equivariant; [assumption|apply default_raw_calls_related; assumption].
Qed.

Lemma init_state_renamed : forall rename pages,
  init_state pages = rename_state rename (init_state pages).
Proof.
  intros rename pages. unfold init_state. rewrite <- store_le_renamed.
  match goal with |- context [mem_grow ?state pages] => set (empty := state) end.
  assert (Empty : rename_state rename empty = empty) by reflexivity.
  pose proof (@mem_grow_renamed rename empty pages) as Grow. rewrite Empty in Grow.
  destruct (mem_grow empty pages) as [count state].
  cbn [rename_count_state] in Grow. injection Grow as State.
  cbn [snd]. now rewrite <- State.
Qed.

Lemma initial_worlds_related : forall rename argv read pages parameter,
  Execution.R.worlds_related (host_states_related rename)
    (Public.World argv read (init_memories pages parameter))
    (Public.World argv read (init_memories pages (option_map rename parameter))).
Proof.
  intros rename argv read pages parameter. split; [reflexivity|split; [reflexivity|]].
  unfold host_states_related. cbn [Public.external]. unfold init_memories.
  pose proof (f_equal ms_heap (init_state_renamed rename pages)) as Heap.
  cbn [rename_state ms_heap] in Heap.
  unfold rename_state. cbn [ms_mem ms_pages ms_out ms_err ms_heap ms_hnext
    ms_fds ms_nextfd ms_memory_parameter ms_scratch_memories map].
  now rewrite <- Heap.
Qed.

Lemma related_final_streams : forall rename left right,
  Execution.R.executions_related (base_renaming rename) (host_states_related rename) left right ->
  let left_state := Public.external (Public.final_state left) in
  let right_state := Public.external (Public.final_state right) in
  ms_out right_state = ms_out left_state /\ ms_err right_state = ms_err left_state.
Proof.
  intros rename left right [_ [_ [_ Related]]].
  unfold host_states_related in Related. rewrite Related. split; reflexivity.
Qed.

Lemma declared_bool_labels_fixed : forall rename bundle W program initial,
  fixes_declarations rename W ->
  rename 0%nat = 0%nat ->
  AuditBase.Semantics.Elaboration.ElaboratesIn bundle W program ->
  rename (view_bool (Public.runtime program initial)) = view_bool (Public.runtime program initial) /\
  rename (view_true (Public.runtime program initial)) = view_true (Public.runtime program initial) /\
  rename (view_false (Public.runtime program initial)) = view_false (Public.runtime program initial).
Proof.
  intros rename bundle W program initial Fixed Zero Elaborated.
  apply PublicBoolTyping.elaborates_bool_symbols in Elaborated.
  destruct Elaborated as [_ [[Bool [True False]]|[_ [Bool [True False]]]]].
  - destruct Bool as (bool_decl & _ & Bool & _).
    destruct True as (true_decl & _ & True & _).
    destruct False as (false_decl & _ & False & _).
    exact (conj (Fixed _ _ Bool) (conj (Fixed _ _ True) (Fixed _ _ False))).
  - change (rename (program_bool program) = program_bool program /\
      rename (program_true program) = program_true program /\
      rename (program_false program) = program_false program).
    now rewrite Bool, True, False.
Qed.

Theorem default_runs_equivariant : forall rename program initial initial',
  (forall left right, rename left = rename right -> left = right) ->
  rename 0%nat = 0%nat ->
  rename (view_bool (Public.runtime program initial)) = view_bool (Public.runtime program initial) ->
  rename (view_true (Public.runtime program initial)) = view_true (Public.runtime program initial) ->
  rename (view_false (Public.runtime program initial)) = view_false (Public.runtime program initial) ->
  Execution.R.worlds_related (host_states_related rename) initial initial' ->
  (forall result, Public.Runs program Public.default_interface initial result ->
    exists result',
      Public.Runs (rename_program (base_renaming rename) program) Public.default_interface initial' result' /\
      Execution.R.executions_related (base_renaming rename) (host_states_related rename) result result') /\
  (forall result', Public.Runs (rename_program (base_renaming rename) program) Public.default_interface initial' result' ->
    exists result,
      Public.Runs program Public.default_interface initial result /\
      Execution.R.executions_related (base_renaming rename) (host_states_related rename) result result').
Proof.
  intros rename program initial initial' Injective Sentinel Bool True False World.
  apply (Execution.runs_equivariant (base_renaming rename) Injective Sentinel default_interfaces
    (host_states_related rename)); [exact (conj (conj eq_refl eq_refl) World)|].
  apply default_primitive_calls_related; try assumption.
  destruct World as [Args [Read _]]. unfold Public.runtime.
  rewrite Args, Read. now apply program_view_equivariant.
Qed.
