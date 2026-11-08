(* The four emitting operations instantiate the shared traversal for frames. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL reqs fn_info check_assumed_requirements check_assumes].
Lemma ok_wrapper_atom : forall fuel pv, ok (wrapper fuel pv).
Proof. intros [|fuel] pv; cbn [wrapper]; okt. Qed.
Lemma ok_forwarder_atom : forall fuel,
  (forall g, ok (lower_body fuel g)) -> forall tg, ok (forwarder (S fuel) tg).
Proof. intros fuel IH tg; cbn [forwarder]; okt. Qed.
Lemma ok_lower_body_atom : forall fuel,
  (forall c e b exp, ok (synth_block fuel c e b exp)) ->
  forall g, ok (lower_body (S fuel) g).
Proof. intros fuel IH g; cbn [lower_body]; okt. Qed.
Lemma ok_lambda_provider_atom : forall fuel,
  (forall c e b exp, ok (synth_block fuel c e b exp)) ->
  forall c e ps rt body bound sg this what,
    ok (lambda_provider (S fuel) c e ps rt body bound sg this what).
Proof. intros fuel IH c e ps rt body bound sg this what; cbn [lambda_provider]; okt. Qed.

Lemma ok_match_fn_sig_operation : forall fuel c e pv wanted kind,
  ok (match_fn_sig fuel c e pv wanted kind).
Proof. intros [|fuel] c e pv wanted kind; cbn [match_fn_sig]; okt. Qed.
