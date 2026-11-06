(* Final program bodies for every historical row of a generated lambda.
   Source recursion placeholders have other origins and are not included. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabFrame.
Import ListNotations.
Definition FinalLambdas(P:xprogram)(s:st):Prop:=forall key body owner,
 In(key,body)(st_fns s)->nget(st_syms s)key=Some owner->gs_origin owner=OLambda->
 Audit.Elaboration.body_of P key=Some body.
Lemma final_lambdas_keep:forall P s final,
 (forall row,In row(st_fns s)->In row(st_fns final))->
 (forall key owner,nget(st_syms s)key=Some owner->nget(st_syms final)key=Some owner)->
 FinalLambdas P final->FinalLambdas P s.
Proof. intros P s final Rows Symbols Fact key body owner Row Lookup Origin.
 eapply Fact;[apply Rows;exact Row|apply Symbols;exact Lookup|exact Origin]. Qed.
Lemma final_lambdas_back:forall P s final,Fr s final->FinalLambdas P final->FinalLambdas P s.
Proof.
 intros P s final Frame Fact;eapply final_lambdas_keep;[|exact(fr_keep _ _ Frame)|exact Fact].
 intros row Member;destruct(fx_fns _ _ (fr_x _ _ Frame))as[prefix Eq];rewrite Eq;apply in_or_app;right;exact Member.
Qed.
