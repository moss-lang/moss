(* Every lambda allocated during a completed helper has a completed body.
   Initially pending owners are allowed; they are preserved separately. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame FnPresentStable ForwarderHistory.
Import ListNotations.
Set Implicit Arguments.
Definition NewLambdas(initial final:st):Prop:=forall key owner,
 nget(st_syms final)key=Some owner->gs_origin owner=OLambda->
 nget(st_syms initial)key=None->exists body,nassoc key(st_fns final)=Some body.
Definition LambdaComplete(s:st):Prop:=forall key owner,
 nget(st_syms s)key=Some owner->gs_origin owner=OLambda->exists body,nassoc key(st_fns s)=Some body.
Lemma new_lambdas_same_symbols:forall s final,st_syms final=st_syms s->NewLambdas s final.
Proof. intros s final Same key owner Lookup Origin Missing;rewrite Same,Missing in Lookup;discriminate. Qed.
Lemma new_lambdas_trans:forall initial mid final,Inv mid->Fr initial mid->Fr mid final->
 fn_present_stable mid final->NewLambdas initial mid->NewLambdas mid final->NewLambdas initial final.
Proof.
 intros initial mid final I F1 F2 Stable H1 H2 key owner Lookup Origin Missing.
 destruct(nget(st_syms mid)key)as[g|]eqn:Mid.
 - pose proof(fr_keep _ _ F2 _ _ Mid)as Last;rewrite Lookup in Last;injection Last as <-.
   destruct(H1 _ _ Mid Origin Missing)as[body Present];exists body.
   exact(Stable _ _ (@symbol_below_next mid key owner I Mid)Present).
 - exact(H2 _ _ Lookup Origin Mid).
Qed.
Lemma new_lambdas_trans_except:forall initial mid final owner,
 Inv mid->Fr initial mid->Fr mid final->owner<st_next initial->
 (forall key body,key<>owner->key<st_next mid->nassoc key(st_fns mid)=Some body->nassoc key(st_fns final)=Some body)->
 NewLambdas initial mid->NewLambdas mid final->NewLambdas initial final.
Proof.
 intros initial mid final owner I F1 F2 Below Stable H1 H2 key g Lookup Origin Missing.
 destruct(nget(st_syms mid)key)as[h|]eqn:Mid.
 - pose proof(fr_keep _ _ F2 _ _ Mid)as Last;rewrite Lookup in Last;injection Last as <-.
   destruct(H1 _ _ Mid Origin Missing)as[body Present];exists body;apply(Stable key body).
   + pose proof(fr_new_above _ _ F1 _ _ Missing Mid);lia.
   + exact(@symbol_below_next mid key g I Mid).
   + exact Present.
 - exact(H2 _ _ Lookup Origin Mid).
Qed.
Lemma lambda_complete_preserved:forall initial final,
 Inv initial->Fr initial final->fn_present_stable initial final->NewLambdas initial final->
 LambdaComplete initial->LambdaComplete final.
Proof.
 intros initial final I Frame Stable New Complete key owner Lookup Origin.
 destruct(nget(st_syms initial)key)as[g|]eqn:Old.
 - pose proof(fr_keep _ _ Frame _ _ Old)as Last;rewrite Lookup in Last;injection Last as <-.
   destruct(Complete _ _ Old Origin)as[body Present];exists body.
   exact(Stable _ _ (@symbol_below_next initial key owner I Old)Present).
 - exact(New _ _ Lookup Origin Old).
Qed.
