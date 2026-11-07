(* Operational frames used by traces and semantic body proofs. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame FnBodyTrace.
Lemma ok_body_receiver:forall g e,ok(body_receiver g e).
Proof. intros;unfold body_receiver,nominal_args;okt. Qed.
Lemma ok_fn_sig_body:forall g tm e this,ok(fn_sig g tm e this).
Proof. intros;unfold fn_sig;okt. Qed.
Lemma ok_register_body:forall owner e needs sg,ok(register_reify_code owner e needs sg).
Proof. intros;unfold register_reify_code;okt. Qed.
