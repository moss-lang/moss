(* The evaluator commutes with symbolic renaming when views and primitive
   calls agree under that renaming. *)
From Stdlib Require Import List Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit BaseFacts PublicValueTyping RuntimeRenaming RuntimeRenamingEnv RuntimeRenamingControl.
Import AuditBase.Semantics ListNotations.

Module InterpreterRenaming (Prims : PRIMITIVES) (I : INTERP Prims).
Module E := ExecutionRenaming Prims I.
Module C := ControlRenaming Prims I.

Section Execution.
Variable rename : renaming.
Hypothesis injective : forall left right, rename left = rename right -> left = right.
Hypothesis fixed_sentinel : rename 0 = 0.
Variable state_related : Prims.mstate -> Prims.mstate -> Prop.
Variables P P' : program_view.
Hypothesis Views : views_related rename P P'.
Hypothesis Calls : E.primitives_related rename state_related P P'.

Definition bindings_related
    (left right : (environment * locals * Prims.mstate) + I.evaluation) : Prop :=
  match left, right with
  | inl (d, g, st), inl (d', g', st') =>
      d' = rename_environment rename d /\ g' = rename_locals rename g /\ state_related st st'
  | inr result, inr result' => E.evaluations_related rename state_related result result'
  | _, _ => False
  end.

Definition applications_related
    (left right : (environment * Prims.mstate) + I.evaluation) : Prop :=
  match left, right with
  | inl (d, st), inl (d', st') => d' = rename_environment rename d /\ state_related st st'
  | inr result, inr result' => E.evaluations_related rename state_related result result'
  | _, _ => False
  end.

Definition execution_related (fuel : nat) : Prop :=
  (forall d g left right e, state_related left right ->
    E.evaluations_related rename state_related
      (I.eval fuel P d g left e)
      (I.eval fuel P' (rename_environment rename d) (rename_locals rename g) right (rename_expr rename e))) /\
  (forall d g left right args, state_related left right ->
    E.arguments_related rename state_related
      (I.eval_arguments fuel P d g left args)
      (I.eval_arguments fuel P' (rename_environment rename d) (rename_locals rename g) right
        (map (rename_expr rename) args))) /\
  (forall d g left right bindings, state_related left right ->
    bindings_related
      (I.eval_bindings fuel P d g left bindings)
      (I.eval_bindings fuel P' (rename_environment rename d) (rename_locals rename g) right
        (map (rename_binding rename) bindings))) /\
  (forall d u th outputs needs results left right, state_related left right ->
    applications_related
      (I.apply_functor fuel P d u th outputs needs results left)
      (I.apply_functor fuel P' (rename_environment rename d) (rename u)
        (rename_subst rename th) (rename_outputs rename outputs) (rename_smap rename needs)
        (map (rename_item rename) results) right)).

Local Lemma rename_record_arguments : forall (fields : list (fld * expr)),
  map snd (map (fun '(name, field) => (name, rename_expr rename field)) fields) =
  map (rename_expr rename) (map snd fields).
Proof. induction fields as [|[name field] rest IH]; cbn; congruence. Qed.

Local Lemma rename_record_names : forall (fields : list (fld * expr)),
  map fst (map (fun '(name, field) => (name, rename_expr rename field)) fields) = map fst fields.
Proof. induction fields as [|[name field] rest IH]; cbn; congruence. Qed.

Local Lemma rename_record_values : forall fields values,
  VRecord (combine fields (map (rename_value rename) values)) =
  rename_value rename (VRecord (combine fields values)).
Proof.
  intros fields values. cbn [rename_value]. f_equal. revert fields.
  induction values as [|value rest IH]; intros [|field fields]; cbn; try reflexivity.
  now rewrite IH.
Qed.

Local Lemma rename_method_parameters : forall value values,
  parameter_values (rename_value rename value :: map (rename_value rename) values) =
  rename_locals rename (parameter_values (value :: values)).
Proof. intros receiver args. exact (rename_parameter_values rename (receiver :: args)). Qed.

Local Lemma rename_locals_app : forall left right,
  rename_locals rename left ++ rename_locals rename right = rename_locals rename (left ++ right).
Proof. intros. unfold rename_locals. now rewrite map_app. Qed.

Local Lemma rename_locals_length : forall g, length (rename_locals rename g) = length g.
Proof. intros. unfold rename_locals. apply length_map. Qed.

Local Lemma rename_locals_skipn : forall n g,
  skipn n (rename_locals rename g) = rename_locals rename (skipn n g).
Proof.
  induction n as [|n IH]; intros [|[x w] rest]; cbn [rename_locals skipn map]; try reflexivity.
  exact (IH rest).
Qed.

Local Lemma rename_locals_tail : forall g,
  tl (rename_locals rename g) = rename_locals rename (tl g).
Proof. intros [|[position value] rest]; reflexivity. Qed.

Local Ltac fold_install_provision :=
  match goal with
  | |- context [install_provision ?key ?entry ?d] =>
      let right := constr:(install_provision (rename_item rename key)
        (rename_provision rename entry) (rename_environment rename d)) in
      match goal with
      | |- context [install_provision ?key' ?entry' ?d'] =>
          let actual := constr:(install_provision key' entry' d') in
          unify actual right; change actual with right; rewrite rename_install_provision
      end
  end.

Local Ltac unpack_evaluation Related :=
  let Signal := fresh "Signal" in let Locals := fresh "Locals" in let State := fresh "State" in
  destruct Related as [Signal [Locals State]];
  cbn [I.evaluation_signal I.evaluation_locals I.evaluation_state] in Signal, Locals, State;
  subst.

Local Ltac unpack_arguments Related :=
  first [unpack_evaluation Related
        | let Values := fresh "Values" in let Locals := fresh "Locals" in let State := fresh "State" in
          destruct Related as [Values [Locals State]]; subst].

Local Ltac unpack_application Related :=
  first [unpack_evaluation Related
        | let Environment := fresh "Environment" in let State := fresh "State" in
          destruct Related as [Environment State]; subst].

Local Ltac compare_eval IHe :=
  match goal with
  | |- context [I.eval ?fuel P ?d ?g ?st ?e] =>
      match goal with
      | State : state_related st ?st' |- _ =>
          let right := constr:(I.eval fuel P' (rename_environment rename d)
            (rename_locals rename g) st' (rename_expr rename e)) in
          match goal with
          | |- context [I.eval fuel P' ?d' ?g' st' ?e'] =>
              let actual := constr:(I.eval fuel P' d' g' st' e') in
              unify actual right; change actual with right;
              let Related := fresh "Related" in let Signal := fresh "signal" in
              pose proof (IHe d g st st' e State) as Related;
              destruct (I.eval fuel P d g st e) as [Signal ? ?];
              destruct right as [? ? ?];
              unpack_evaluation Related; destruct Signal
          end
      end
  end.

Local Ltac compare_args IHargs :=
  match goal with
  | |- context [I.eval_arguments ?fuel P ?d ?g ?st ?args] =>
      match goal with
      | State : state_related st ?st' |- _ =>
          let right := constr:(I.eval_arguments fuel P' (rename_environment rename d)
            (rename_locals rename g) st' (map (rename_expr rename) args)) in
          match goal with
          | |- context [I.eval_arguments fuel P' ?d' ?g' st' ?args'] =>
              let actual := constr:(I.eval_arguments fuel P' d' g' st' args') in
              unify actual right; change actual with right;
              let Related := fresh "Related" in let Signal := fresh "signal" in
              pose proof (IHargs d g st st' args State) as Related;
              destruct (I.eval_arguments fuel P d g st args) as [[[? ?] ?]|[Signal ? ?]];
              destruct right as [[[? ?] ?]|[? ? ?]];
              cbn [E.arguments_related] in Related; try contradiction;
              unpack_arguments Related; try destruct Signal
          end
      end
  end.

Local Ltac compare_bindings IHbind :=
  match goal with
  | |- context [I.eval_bindings ?fuel P ?d ?g ?st ?bs] =>
      match goal with
      | State : state_related st ?st' |- _ =>
          let right := constr:(I.eval_bindings fuel P' (rename_environment rename d)
            (rename_locals rename g) st' (map (rename_binding rename) bs)) in
          match goal with
          | |- context [I.eval_bindings fuel P' ?d' ?g' st' ?bs'] =>
              let actual := constr:(I.eval_bindings fuel P' d' g' st' bs') in
              unify actual right; change actual with right;
              let Related := fresh "Related" in let Signal := fresh "signal" in
              pose proof (IHbind d g st st' bs State) as Related;
              destruct (I.eval_bindings fuel P d g st bs) as [[[? ?] ?]|[Signal ? ?]];
              destruct right as [[[? ?] ?]|[? ? ?]];
              cbn [bindings_related] in Related; try contradiction;
              unpack_arguments Related; try destruct Signal
          end
      end
  end.

Local Ltac compare_applications IHapp :=
  match goal with
  | |- context [I.apply_functor ?fuel P ?d ?u ?th ?outputs ?needs ?results ?st] =>
      match goal with
      | State : state_related st ?st' |- _ =>
          let right := constr:(I.apply_functor fuel P' (rename_environment rename d) (rename u)
            (rename_subst rename th) (rename_outputs rename outputs) (rename_smap rename needs)
            (map (rename_item rename) results) st') in
          match goal with
          | |- context [I.apply_functor fuel P' ?d' ?u' ?th' ?outputs' ?needs' ?results' st'] =>
              let actual := constr:(I.apply_functor fuel P' d' u' th' outputs' needs' results' st') in
              unify actual right; change actual with right;
              let Related := fresh "Related" in let Signal := fresh "signal" in
              pose proof (IHapp d u th outputs needs results st st' State) as Related;
              destruct (I.apply_functor fuel P d u th outputs needs results st) as [[? ?]|[Signal ? ?]];
              destruct right as [[? ?]|[? ? ?]];
              cbn [applications_related] in Related; try contradiction;
              unpack_application Related; try destruct Signal
          end
      end
  end.

Local Ltac compare_primitive :=
  match goal with
  | |- context [I.run_prim_at P ?operation ?site ?values ?st] =>
      match goal with
      | State : state_related st ?st' |- _ =>
          let right := constr:(I.run_prim_at P' (rename_operation rename operation) (rename_site rename site)
            (map (rename_value rename) values) st') in
          match goal with
          | |- context [I.run_prim_at P' ?operation' ?site' ?values' st'] =>
              let actual := constr:(I.run_prim_at P' operation' site' values' st') in
              unify actual right; change actual with right;
              let Related := fresh "Related" in let Signal := fresh "Signal" in let Next := fresh "Next" in
              pose proof (Calls operation site values st st' State) as Related;
              unfold I.run_prim_at;
              destruct (Prims.run_prim P operation site values st);
              destruct (Prims.run_prim P' (rename_operation rename operation) (rename_site rename site)
                (map (rename_value rename) values) st');
              destruct Related as [Signal Next]; subst
          end
      end
  end.

Theorem execution_equivariant : forall fuel, execution_related fuel.
Proof.
  induction fuel as [|fuel [IHe [IHargs [IHbind IHapp]]]].
  { unfold execution_related. repeat split; intros; assumption || reflexivity. }
  unfold execution_related. split; [|split; [|split]];
    [ intros d g left right e State; destruct e; cbn [I.eval rename_expr]
    | intros d g left right args State; destruct args; cbn [I.eval_arguments map]
    | intros d g left right bs State; destruct bs as [|b rest]; [|destruct b];
      cbn [I.eval_bindings map rename_binding]
    | intros d u th outputs needs results left right State; cbn [I.apply_functor] ].
  all: repeat first
    [ solve [repeat split; assumption || reflexivity]
    | compare_eval IHe
    | compare_args IHargs
    | compare_bindings IHbind
    | compare_applications IHapp
    | compare_primitive
    | progress rewrite rename_lookup_local
    | progress rewrite rename_lookup_val by exact injective
    | progress rewrite rename_lookup_fn by exact injective
    | progress rewrite rename_lookup_fun by exact injective
    | progress rewrite rename_lookup_provision by exact injective
    | progress rewrite (related_functions _ _ _ Views)
    | progress rewrite (related_functors _ _ _ Views)
    | progress rewrite (E.check_tag_equivariant rename injective P P') by exact (related_casts _ _ _ Views)
    | progress rewrite (C.rename_truthy rename injective P P') by exact Views
    | progress rewrite C.rename_get_field
    | progress rewrite C.rename_select_arm by exact injective
    | progress rewrite rename_record_arguments
    | progress rewrite rename_record_names
    | progress rewrite rename_record_values
    | progress rewrite rename_select_provisions by exact injective
    | progress rewrite rename_adapt_provisions by exact injective
    | progress rewrite rename_parameter_values
    | progress rewrite rename_method_parameters
    | progress rewrite rename_locals_app
    | progress rewrite rename_locals_length
    | progress rewrite rename_locals_skipn
    | progress rewrite rename_locals_tail
    | progress rewrite rename_call_environment by exact injective
    | progress rewrite rename_assign_local
    | progress rewrite rename_bind_type by exact injective
    | progress rewrite rename_typed_capture by exact injective
    | progress rewrite rename_install_provision
    | fold_install_provision
    | progress rewrite rename_compose_adaptation by exact injective
    | progress rewrite rename_primitive_closure by exact injective
    | progress rewrite rename_append_environment
    | progress rewrite rename_result_environment by exact injective
    | progress rewrite rename_result_selection by exact fixed_sentinel
    | progress rewrite (rename_application_environment rename injective P P') by exact Views
    | progress cbn [option_map rename_provision rename_functor_code rename_signal
        fc_bindings fc_outputs fc_sources I.ret_call I.pop1 I.popn I.with_g]
    | match goal with
      | |- context [match ?x with _ => _ end] =>
          lazymatch x with
          | option_map _ _ => fail
          | _ =>
              lazymatch type of x with
              | option provision_value => destruct x as [[]|]
              | option bool => destruct x as [[]|]
              | option (_ * _) => destruct x as [[? ?]|]
              | option _ => destruct x
              | _ => fail
              end
          end
      end ].
Qed.

Theorem run_main_equivariant : forall fuel main d left right,
  state_related left right ->
  E.evaluations_related rename state_related
    (I.run_main fuel P main d left)
    (I.run_main fuel P' (rename main) (rename_environment rename d) right).
Proof.
  intros fuel main d left right State.
  unfold I.run_main. rewrite (related_functions _ _ _ Views).
  destruct (view_function P main) as [body|]; [|repeat split; auto].
  cbn [option_map].
  pose proof (proj1 (execution_equivariant fuel) d [] left right body State) as Related.
  cbn [rename_locals map] in Related.
  destruct (I.eval fuel P d [] left body) as [out g next].
  destruct (I.eval fuel P' (rename_environment rename d) [] right (rename_expr rename body)) as [out' g' next'].
  unpack_evaluation Related. destruct out; repeat split; auto.
Qed.
End Execution.
End InterpreterRenaming.

Module PublicInterpreterRenaming (Host : HOST) (M : PUBLIC Host).
Module N := InterpreterRenaming M.ReferenceHost M.Reference.
Module R := PublicRenaming Host M.

Theorem runs_equivariant : forall (rename : renaming),
  (forall left right, rename left = rename right -> left = right) ->
  rename 0 = 0 ->
  forall interfaces_related external_related program interface interface' initial initial',
    R.machine_states_related interfaces_related external_related (interface, initial) (interface', initial') ->
    N.E.primitives_related rename (R.machine_states_related interfaces_related external_related)
      (M.runtime program initial) (M.runtime (rename_program rename program) initial') ->
    (forall result, M.Runs program interface initial result ->
      exists result', M.Runs (rename_program rename program) interface' initial' result' /\
        R.executions_related rename external_related result result') /\
    (forall result', M.Runs (rename_program rename program) interface' initial' result' ->
      exists result, M.Runs program interface initial result /\
        R.executions_related rename external_related result result').
Proof.
  intros rename Injective Sentinel Interfaces External program interface interface' initial initial' State Calls.
  destruct State as [Related [Arguments [Read World]]].
  assert (Views : views_related rename
    (M.runtime program initial) (M.runtime (rename_program rename program) initial')).
  { unfold M.runtime. cbn in Arguments, Read. rewrite Arguments, Read.
    now apply program_view_equivariant. }
  assert (Runs : forall fuel,
    N.E.evaluations_related rename (R.machine_states_related Interfaces External)
      (M.Reference.run_main fuel (M.runtime program initial) (program_main program)
        (program_environment program) (interface, initial))
      (M.Reference.run_main fuel (M.runtime (rename_program rename program) initial')
        (program_main (rename_program rename program)) (program_environment (rename_program rename program))
        (interface', initial'))).
  { intro fuel. apply (N.run_main_equivariant rename Injective Sentinel
      (R.machine_states_related Interfaces External)); try assumption. repeat split; assumption. }
  split.
  - apply (R.runs_transport rename Interfaces External). exact Runs.
  - intros result' [fuel Seen']. pose proof (R.observe_equivariant rename Interfaces External _ _ (Runs fuel)) as Seen.
    rewrite Seen' in Seen.
    destruct (M.observe (M.Reference.run_main fuel (M.runtime program initial) (program_main program)
      (program_environment program) (interface, initial))) as [result|] eqn:Observed; [|contradiction].
    exists result. split; [now exists fuel|exact Seen].
Qed.
End PublicInterpreterRenaming.
