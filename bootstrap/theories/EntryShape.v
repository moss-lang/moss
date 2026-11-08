(* Accepted entry signatures supply the empty argument environment used by
   run_main. The return classifier also preserves native zero-lane ABI. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel Elab.
Import ListNotations ExecutionModel.Input.

Lemma check_main_signature_empty_parameters : forall g info before after,
    check_main_signature g info before = Good (tt,after) -> fn_params g = [].
Proof.
  intros g info before after H. unfold check_main_signature in H.
  unfold mbind at 1 in H.
  destruct (check_main_arity g before) as [[[] middle]|message] eqn:E; [|discriminate].
  unfold check_main_arity in E. destruct (fn_params g) eqn:Params; [reflexivity|].
  discriminate.
Qed.
