(* Fuel extension preserves the entire generated body and metadata receipt. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabMono LambdaTrace.
Import ListNotations.
Lemma lambda_reify_mono:forall X owner(m1 m2:M X),le_m m1 m2->
 le_m(in_reify_code owner m1)(in_reify_code owner m2).
Proof.
 intros X owner m1 m2 Mono;unfold in_reify_code.
 apply le_bind;[apply le_refl|intro saved].
 apply le_bind;[apply le_refl|intro u].
 apply le_bind;[exact Mono|intro value];apply le_refl.
Qed.
Theorem lambda_generated_mono:forall fuel c en ps body sg this pid lets,
 le_m(lambda_generated_body fuel c en ps body sg this pid lets)
     (lambda_generated_body(S fuel)c en ps body sg this pid lets).
Proof.
 intros;unfold lambda_generated_body;apply lambda_reify_mono,le_with_vars.
 apply le_bind;[apply le_refl|intro declarations].
 apply le_bind.
 - exact((proj1(proj2(proj2(mono_group fuel))))(lambda_own_context c ps sg this)en(ABlock[](Some body))(Some(sg_ret sg))).
 - intros[[block out]bt];apply le_refl.
Qed.
