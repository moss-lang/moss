(* Returned dictionaries keep their captured fixed meanings even when the
   enclosing provider creates a new output generation with the same name. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel Elab.
Import ListNotations.

Example returned_functor_old_capture_refused :
  let body := set_app (env_with_edges [(1,YNom 2 [])] [])
                     (KyFun 3) [(1,Some (YAbs 1))] 9 in
  let promised := set_app empty_env (KyFun 3) [(1,Some (YAbs 1))] 10 in
  result_functor_fixed_ok body promised [(1,YNom 2 [])] (KyFun 3) (KyFun 3) = false.
Proof. reflexivity. Qed.

Example returned_functor_new_capture_accepted :
  let body := set_app (env_with_edges [(1,YNom 2 [])] [])
                     (KyFun 3) [(1,Some (YNom 2 []))] 9 in
  let promised := set_app empty_env (KyFun 3) [(1,Some (YAbs 1))] 10 in
  result_functor_fixed_ok body promised [(1,YNom 2 [])] (KyFun 3) (KyFun 3) = true.
Proof. reflexivity. Qed.

Lemma returned_functor_fixed_reading : forall body promised made actual_key promised_key wanted stamp,
    get_app promised promised_key = Some (wanted,stamp) ->
    result_functor_fixed_ok body promised made actual_key promised_key = true ->
    forall t y, In (t,Some y) wanted ->
    exists got,
      opt_default None (assoc Nat.eqb t
        (match get_app body actual_key with Some (a,_) => a | None => [] end)) = Some got /\
      ety_eqb (canon body got) (promised_result_type body made y) = true.
Proof.
  intros body promised made actual_key promised_key wanted stamp Hwanted H t y Hin.
  unfold result_functor_fixed_ok in H. rewrite Hwanted in H.
  apply forallb_forall with (x:=(t,Some y)) in H; [|exact Hin].
  change (match opt_default None (assoc Nat.eqb t
    (match get_app body actual_key with Some (a,_) => a | None => [] end)) with
    | Some got => ety_eqb (canon body got) (promised_result_type body made y)
    | None => false end = true) in H.
  destruct (opt_default None (assoc Nat.eqb t
    (match get_app body actual_key with Some (a,_) => a | None => [] end)))
    as [got|] eqn:E; [|discriminate].
  exists got. split; [reflexivity|exact H].
Qed.
