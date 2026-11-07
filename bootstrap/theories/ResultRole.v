(* Output-role substitution precedes canonicalization of old input UFs. *)
From Stdlib Require Import List.
From NM Require Import Elab.
Import ListNotations.

Example result_role_survives_old_input_equation :
  let inputs := env_with_edges [(1,YAbs 1);(2,YAbs 2)] [(1,YAbs 2)] in
  let body := set_tn inputs 1 (YNom 3 []) 99 in
  canon inputs (YAbs 1) = YAbs 2 /\
  promised_result_type body [(1,YNom 3 [])] (YAbs 1) = YNom 3 [].
Proof. split; reflexivity. Qed.

Example other_input_role_stays_old :
  let inputs := env_with_edges [(1,YAbs 1);(2,YAbs 2)] [(1,YAbs 2)] in
  let body := set_tn inputs 1 (YNom 3 []) 99 in
  promised_result_type body [(1,YNom 3 [])] (YAbs 2) = YAbs 2.
Proof. reflexivity. Qed.
