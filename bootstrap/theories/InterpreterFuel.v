(* Fuel bounds the observation. Every finished execution is unchanged at
   larger bounds, including the final state and explicit failure signals. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit PublicValueTyping.
From NM Require Import BaseFacts.
Import Audit.Semantics ListNotations.

Module Fuel (Prims : PRIMITIVES) (I : INTERP Prims).
Import I.

Definition finished (result : evaluation) : Prop :=
  evaluation_signal result <> OutOfFuel.

Definition answered {T} (result : T + evaluation) : Prop :=
  match result with inl _ => True | inr stop => finished stop end.

Definition stable_at (fuel : nat) : Prop :=
  (forall more P d g st e,
    fuel <= more -> finished (eval fuel P d g st e) ->
    eval more P d g st e = eval fuel P d g st e) /\
  (forall more P d g st args,
    fuel <= more -> answered (eval_arguments fuel P d g st args) ->
    eval_arguments more P d g st args = eval_arguments fuel P d g st args) /\
  (forall more P d g st bindings,
    fuel <= more -> answered (eval_bindings fuel P d g st bindings) ->
    eval_bindings more P d g st bindings = eval_bindings fuel P d g st bindings) /\
  (forall more P d u th outputs sm rs st,
    fuel <= more -> answered (apply_functor fuel P d u th outputs sm rs st) ->
    apply_functor more P d u th outputs sm rs st = apply_functor fuel P d u th outputs sm rs st).

Local Ltac finished_case More Whole :=
  first [ solve [exfalso; apply Whole; reflexivity]
        | rewrite (More ltac:(cbn [finished answered evaluation_signal]; first [exact I | discriminate]))
        ].

Theorem execution_stable : forall fuel, stable_at fuel.
Proof.
  induction fuel as [|fuel [IHe [IHargs [IHbind IHapp]]]].
  { unfold stable_at. repeat split; intros; exfalso; apply H0; reflexivity. }
  unfold stable_at. repeat split;
    [ intros [|more] P d g st e Bound Whole; [inversion Bound|];
      destruct e; cbn [eval] in Whole |- *
    | intros [|more] P d g st args Bound Whole; [inversion Bound|];
      destruct args; cbn [eval_arguments] in Whole |- *
    | intros [|more] P d g st bindings Bound Whole; [inversion Bound|];
      destruct bindings as [|binding rest]; [|destruct binding]; cbn [eval_bindings] in Whole |- *
    | intros [|more] P d u th outs sm rs st Bound Whole; [inversion Bound|];
      cbn [apply_functor] in Whole |- * ].
  all: apply le_S_n in Bound.
  all: lazymatch type of Bound with ?low <= ?high =>
  repeat first
    [ solve [reflexivity]
    | solve [exfalso; apply Whole; reflexivity]
    | match goal with
      | |- context [eval high ?P ?d ?g ?st ?e] =>
          let Stable := fresh "Stable" in
          let result := fresh "outcome" in
          pose proof (IHe high P d g st e Bound) as Stable;
          destruct (eval low P d g st e) as [result ? ?]; destruct result;
          finished_case Stable Whole
      | |- context [eval_arguments high ?P ?d ?g ?st ?args] =>
          let Stable := fresh "Stable" in
          let result := fresh "outcome" in
          pose proof (IHargs high P d g st args Bound) as Stable;
          destruct (eval_arguments low P d g st args) as [[[? ?] ?]|[result ? ?]];
          [|destruct result]; finished_case Stable Whole
      | |- context [eval_bindings high ?P ?d ?g ?st ?bs] =>
          let Stable := fresh "Stable" in
          let result := fresh "outcome" in
          pose proof (IHbind high P d g st bs Bound) as Stable;
          destruct (eval_bindings low P d g st bs) as [[[? ?] ?]|[result ? ?]];
          [|destruct result]; finished_case Stable Whole
      | |- context [apply_functor high ?P ?d ?u ?th ?outs ?sm ?rs ?st] =>
          let Stable := fresh "Stable" in
          let result := fresh "outcome" in
          pose proof (IHapp high P d u th outs sm rs st Bound) as Stable;
          destruct (apply_functor low P d u th outs sm rs st) as [[? ?]|[result ? ?]];
          [|destruct result]; finished_case Stable Whole
      | |- context [match ?scrutinee with _ => _ end] =>
          lazymatch type of scrutinee with
          | option provision_value => destruct scrutinee as [[]|]
          | option bool => destruct scrutinee as [[]|]
          | option (_ * _) => destruct scrutinee as [[? ?]|]
          | option _ => destruct scrutinee
          | _ => fail
          end
      end ] end.
Qed.

Theorem run_main_stable : forall fuel more P main d st,
  fuel <= more -> finished (run_main fuel P main d st) ->
  run_main more P main d st = run_main fuel P main d st.
Proof.
  intros fuel more P main d st Bound Whole.
  unfold run_main in Whole |- *. destruct (view_function P main); [|reflexivity].
  rewrite (proj1 (execution_stable fuel) more P d [] st e Bound); [reflexivity|].
  unfold finished in *. destruct (eval fuel P d [] st e) as [out g state].
  destruct out; try discriminate. exact Whole.
Qed.
End Fuel.

Module PublicFuel (Host : HOST) (M : PUBLIC Host).
Module F := Fuel M.ReferenceHost M.Reference.

Lemma observed_run_stable : forall program interface initial fuel more result,
  fuel <= more ->
  M.observe (M.Reference.run_main fuel (M.runtime program initial)
    (program_main program) (program_environment program) (interface, initial)) = Some result ->
  M.observe (M.Reference.run_main more (M.runtime program initial)
    (program_main program) (program_environment program) (interface, initial)) = Some result.
Proof.
  intros program interface initial fuel more result Bound Seen.
  erewrite (F.run_main_stable fuel more); [exact Seen|exact Bound|].
  unfold F.finished. unfold M.observe in Seen.
  destruct (M.Reference.evaluation_signal _) in *; try discriminate.
Qed.

Theorem runs_deterministic : forall program interface initial left right,
  M.Runs program interface initial left -> M.Runs program interface initial right -> left = right.
Proof.
  intros program interface initial left right (fuel & Left) (more & Right).
  destruct (Nat.le_ge_cases fuel more) as [Bound|Bound].
  - pose proof (observed_run_stable program interface initial fuel more left Bound Left).
    congruence.
  - pose proof (observed_run_stable program interface initial more fuel right Bound Right).
    congruence.
Qed.
End PublicFuel.
