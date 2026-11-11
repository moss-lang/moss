(* A change of state representation commutes with execution when it
   commutes with every primitive call. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit PublicValueTyping.
Import Audit.Semantics ListNotations.

Module StateSimulation (Raw Public : PRIMITIVES) (R : INTERP Raw) (S : INTERP Public).
Section StateMap.
Variable lift : Raw.mstate -> Public.mstate.
Variable P : program_view.
Hypothesis primitive_commutes : forall operation site values state,
  Public.run_prim P operation site values (lift state) =
    let '(signal, next) := Raw.run_prim P operation site values state in
    (signal, lift next).

Definition lift_eval (r : R.evaluation) : S.evaluation :=
  S.Evaluation (R.evaluation_signal r) (R.evaluation_locals r) (lift (R.evaluation_state r)).

Definition lift_bind_result (r : (environment * locals * Raw.mstate) + R.evaluation)
    : (environment * locals * Public.mstate) + S.evaluation :=
  match r with
  | inl (d, g, st) => inl (d, g, lift st)
  | inr r => inr (lift_eval r)
  end.

Definition lift_args_result (r : (list value * locals * Raw.mstate) + R.evaluation)
    : (list value * locals * Public.mstate) + S.evaluation :=
  match r with
  | inl (ws, g, st) => inl (ws, g, lift st)
  | inr r => inr (lift_eval r)
  end.

Definition lift_apply_result (r : (environment * Raw.mstate) + R.evaluation)
    : (environment * Public.mstate) + S.evaluation :=
  match r with
  | inl (d, st) => inl (d, lift st)
  | inr r => inr (lift_eval r)
  end.

Lemma select_arm_exact : forall arms value, S.select_arm arms value = R.select_arm arms value.
Proof.
  induction arms as [|[pat body] arms IH]; intros value; [reflexivity|].
  cbn [S.select_arm R.select_arm].
  destruct pat as [[binder|]|tag binder fields]; try reflexivity.
  destruct value as [|n payload|record|sort bits|address header]; try exact (IH _).
  cbn [S.match_pat R.match_pat].
  destruct (Nat.eqb tag n); [|exact (IH _)].
  destruct fields as [|field fields]; [reflexivity|].
  destruct payload as [|payload_tag inner|record|sort bits|address header]; try reflexivity.
  change (R.bind_fields (field :: fields) record) with (S.bind_fields (field :: fields) record).
  destruct (S.bind_fields (field :: fields) record); reflexivity.
Qed.

Definition commutes fuel :=
  (forall d g st e,
    S.eval fuel P d g (lift st) e = lift_eval (R.eval fuel P d g st e)) /\
  (forall d g st args,
    S.eval_arguments fuel P d g (lift st) args = lift_args_result (R.eval_arguments fuel P d g st args)) /\
  (forall d g st bindings,
    S.eval_bindings fuel P d g (lift st) bindings = lift_bind_result (R.eval_bindings fuel P d g st bindings)) /\
  (forall d u th outs sm rs st,
    S.apply_functor fuel P d u th outs sm rs (lift st) = lift_apply_result (R.apply_functor fuel P d u th outs sm rs st)).

Theorem execution_commutes : forall fuel, commutes fuel.
Proof.
  induction fuel as [|fuel [IHe [IHargs [IHbind IHapp]]]].
  { repeat split; reflexivity. }
  unfold commutes. repeat split.
  all: intros.
  all: try (destruct e; cbn [S.eval R.eval]).
  all: try (destruct args; cbn [S.eval_arguments R.eval_arguments]).
  all: try (destruct bindings as [|binding rest];
      [cbn [S.eval_bindings R.eval_bindings] | destruct binding; cbn [S.eval_bindings R.eval_bindings]]).
  all: try cbn [S.apply_functor R.apply_functor].
  all: repeat first
    [ progress rewrite IHe
    | progress rewrite IHargs
    | progress rewrite IHbind
    | progress rewrite IHapp
    | progress rewrite primitive_commutes
    | progress rewrite select_arm_exact
    | progress unfold R.run_prim_at, S.run_prim_at
    | progress cbn [lift_eval lift_args_result lift_bind_result lift_apply_result
        R.evaluation_signal R.evaluation_locals R.evaluation_state
        S.ret_call R.ret_call S.pop1 R.pop1 S.popn R.popn S.with_g R.with_g]
    | solve [reflexivity]
    | match goal with
      | |- context [S.truthy ?p ?w] => change (S.truthy p w) with (R.truthy p w)
      | |- context [S.get_field ?f ?w] => change (S.get_field f w) with (R.get_field f w)
      | |- context [R.eval ?k ?p ?d ?g ?st ?e] =>
          let signal := fresh "signal" in
          destruct (R.eval k p d g st e) as [signal ? ?]; destruct signal
      | |- context [R.eval_arguments ?k ?p ?d ?g ?st ?args] =>
          destruct (R.eval_arguments k p d g st args) as [[[? ?] ?]|[? ? ?]]
      | |- context [R.eval_bindings ?k ?p ?d ?g ?st ?bs] =>
          destruct (R.eval_bindings k p d g st bs) as [[[? ?] ?]|[? ? ?]]
      | |- context [R.apply_functor ?k ?p ?d ?u ?th ?outs ?sm ?rs ?st] =>
          destruct (R.apply_functor k p d u th outs sm rs st) as [[? ?]|[? ? ?]]
      | |- context [Raw.run_prim ?p ?operation ?site ?values ?st] =>
          destruct (Raw.run_prim p operation site values st) as [? ?]
      | |- context [match ?scrutinee with _ => _ end] =>
          lazymatch type of scrutinee with
          | option provision_value => destruct scrutinee as [[]|]
          | option bool => destruct scrutinee as [[]|]
          | option (_ * _) => destruct scrutinee as [[? ?]|]
          | option _ => destruct scrutinee
          | _ => fail
          end
      end ].
Qed.

Theorem run_main_commutes : forall fuel main env initial,
  S.run_main fuel P main env (lift initial) =
    lift_eval (R.run_main fuel P main env initial).
Proof.
  intros fuel main env initial.
  unfold S.run_main, R.run_main.
  destruct (view_function P main); [|reflexivity].
  rewrite (proj1 (execution_commutes fuel)).
  destruct (R.eval fuel P env [] initial e). reflexivity.
Qed.
End StateMap.
End StateSimulation.
