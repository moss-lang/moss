(* Equivariance of the actual bootstrap host and its public execution. *)
From Stdlib Require Import List Bool PrimInt63 Uint63.
From NM Require Import Audit AuditBase RuntimeRenaming BaseRenaming BaseHostRenaming BaseHostMemoryRenaming.
From NM Require BaseHost BaseHostScalars MossX ExecutionModel PublicOperationFacts InterpreterRenaming.
Import Audit.Semantics MossX ListNotations.
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
  Heap.is_gc primitive = false -> rename_base_primitive rename primitive = primitive.
Proof. destruct primitive; cbn [Heap.is_gc]; intros; try reflexivity; discriminate. Qed.

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
    pose proof (rename_plain_primitive primitive IsGC) as Fixed.
    pose proof (gc_arguments_renamed rename primitive values) as Arguments.
    rewrite Fixed in Arguments.
    unfold BaseHost.primitive, BaseHost.primitive_in.
    rewrite rename_base_encoding, Fixed, !BaseHost.decode_host_encode, !run_prim_fast_spec.
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
  views_related (base_renaming rename) P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  raw_calls_related rename
    (Execution.R.machine_states_related default_interfaces (host_states_related rename))
    Public.ReferenceHost.raw_primitive P P'.
Proof.
  intros rename P P' Views Bool True False operation values
    [interface [argv read state]] [interface' [argv' read' state']] Related.
  destruct Related as [[Left Right] [Args [Read External]]].
  cbn [Public.arguments Public.read_file Public.external fst snd] in Left, Right, Args, Read, External.
  unfold host_states_related in External. subst interface interface' argv' read' state'.
  cbn [Public.ReferenceHost.raw_primitive Public.default_interface Public.supports
    Public.primitive Public.external Public.arguments Public.read_file].
  unfold BaseHost.Concrete.primitive.
  rewrite (raw_primitive_renamed rename P P' operation values state Views Bool True False).
  destruct (BaseHost.primitive P operation values state); repeat split; reflexivity.
Qed.

Theorem default_primitive_calls_related : forall rename P P',
  views_related (base_renaming rename) P P' ->
  rename (view_bool P) = view_bool P ->
  rename (view_true P) = view_true P ->
  rename (view_false P) = view_false P ->
  Execution.N.E.primitives_related (base_renaming rename)
    (Execution.R.machine_states_related default_interfaces (host_states_related rename)) P P'.
Proof.
  intros rename P P' Views Bool True False.
  unfold Execution.N.E.primitives_related, Public.ReferenceHost.run_prim, Base.call.
  apply base_call_equivariant, default_raw_calls_related; assumption.
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
