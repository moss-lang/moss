(* The top of the public semantic-safety proof (G05 from T20).
   A Wrong outcome is exactly a Stuck entry signal: run_main's ret_call
   turns an escaping Breaking into Stuck, and observe maps Stuck to Wrong.
   The remaining obligation, that an admissible program run under a
   compatible interface never reaches a Stuck entry signal, is stated here
   as entry_never_stuck and passed as an explicit premise. *)
From Stdlib Require Import List.
From NM Require AuditBase.
From NM Require Import BaseFacts.
From NM Require Import Audit PublicValueTyping.
Import AuditBase.Semantics ListNotations.

Module SafetyReduction (Host : HOST) (M0 : PUBLIC Host).
Module M := M0.
Import M.

Definition stuck_signal (signal : signal) : Prop :=
  match signal with Stuck _ => True | _ => False end.

(* T20's finish condition, restricted to what can reach observe. *)
Definition entry_never_stuck (prelude : file_path) : Prop :=
  forall program interface initial fuel,
    Admissible prelude program -> Compatible program interface ->
    ~ stuck_signal (Reference.evaluation_signal
        (Reference.run_main fuel (runtime program initial) (program_main program)
          (program_environment program) (interface, initial))).

Lemma ret_call_not_breaking : forall g r,
  Reference.evaluation_signal (Reference.ret_call g r) <> Breaking.
Proof. intros g [[] ? ?]; discriminate. Qed.

Lemma run_main_not_breaking : forall fuel P main d0 st0,
  Reference.evaluation_signal (Reference.run_main fuel P main d0 st0) <> Breaking.
Proof.
  intros fuel P main d0 st0. unfold Reference.run_main.
  destruct (view_function P main); [apply ret_call_not_breaking|discriminate].
Qed.

(* observe reports Wrong exactly for Stuck and Breaking signals. *)
Lemma observe_wrong : forall result code final,
  observe result = Some (Result (Wrong code) final) ->
  stuck_signal (Reference.evaluation_signal result) \/
  Reference.evaluation_signal result = Breaking.
Proof.
  intros [signal locals [interface state]] code final Observed.
  unfold observe in Observed; simpl in *.
  destruct signal; try discriminate; simpl; auto.
Qed.

Theorem safety_from_entry : forall prelude,
  entry_never_stuck prelude ->
  forall program interface initial,
    Admissible prelude program -> Compatible program interface ->
    forall code final, ~ Runs program interface initial (Result (Wrong code) final).
Proof.
  intros prelude Entry program interface initial Admissible Compatible code final [fuel Observed].
  destruct (observe_wrong _ _ _ Observed) as [Stuck|Breaks].
  - exact (Entry program interface initial fuel Admissible Compatible Stuck).
  - exact (run_main_not_breaking _ _ _ _ _ Breaks).
Qed.

(* Conversely, a Stuck entry signal is a Wrong run: the reduction loses nothing. *)
Theorem stuck_entry_is_wrong : forall program interface initial fuel,
  stuck_signal (Reference.evaluation_signal
    (Reference.run_main fuel (runtime program initial) (program_main program)
      (program_environment program) (interface, initial))) ->
  exists code final, Runs program interface initial (Result (Wrong code) final).
Proof.
  intros program interface initial fuel Stuck.
  destruct (Reference.run_main fuel (runtime program initial) (program_main program)
    (program_environment program) (interface, initial)) as [signal locals [interface' state]] eqn:Run.
  simpl in Stuck. destruct signal; try contradiction.
  exists n, state, fuel. rewrite Run. reflexivity.
Qed.
End SafetyReduction.
