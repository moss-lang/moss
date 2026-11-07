(* Completed generated code is tied to one real compiler invocation. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame
 ReifyHeader ReifyHeaderStable FnPresentStable ForwarderHistory.
Import ListNotations.
Set Implicit Arguments.
Definition LambdaOrigin(f:nat)(body:xexpr) header (origin:origin):Prop:=
 (exists fuel c e ps rt expression bound sg this what initial pre sm completed,
 origin=OLambda/\Inv initial/\lambda_provider fuel c e ps rt expression bound sg this what initial=Good((f,pre,sm),completed)/\
 nassoc f(st_fns completed)=Some body/\function_header completed f=header)\/
 (exists fuel provider initial completed,
 origin=OLambda/\Inv initial/\nget(st_wrappers initial)(gs_id provider)=None/\
 wrapper fuel provider initial=Good(f,completed)/\
 nassoc f(st_fns completed)=Some body/\function_header completed f=header)\/
 (exists fuel g block initial completed,
  origin=gs_origin g/\Inv initial/\gs_id g=f/\nget(st_syms initial)f=Some g/\fn_body g=Some block/\
  nassoc f(st_fns initial)=None/\f<st_next initial/\lower_body fuel g initial=Good(tt,completed)/\
  nassoc f(st_fns completed)=Some body/\function_header completed f=header).
Definition GeneratedCode(g:gsym):Prop:=gs_origin g=OLambda\/exists target,gs_origin g=OForwarder target.
Definition NewLambdaOrigins(initial final:st):Prop:=forall key owner,
 nget(st_syms final)key=Some owner->GeneratedCode owner->
 nget(st_syms initial)key=None->exists body,
 nassoc key(st_fns final)=Some body/\LambdaOrigin key body(function_header final key)(gs_origin owner).
Lemma new_lambda_origins_same_symbols:forall initial final,
 st_syms final=st_syms initial->NewLambdaOrigins initial final.
Proof. intros initial final Same key owner At Origin Missing;rewrite Same,Missing in At;discriminate. Qed.
Lemma new_lambda_origins_trans:forall initial mid final,
 Inv mid->Fr initial mid->Fr mid final->fn_present_stable mid final->
 (forall key body,key<st_next mid->nassoc key(st_fns mid)=Some body->
 function_header final key=function_header mid key)->
 NewLambdaOrigins initial mid->NewLambdaOrigins mid final->NewLambdaOrigins initial final.
Proof.
 intros initial mid final I F1 F2 Stable Headers First Last key owner At Origin Missing.
 destruct(nget(st_syms mid)key)as[old|]eqn:Mid.
 - pose proof(fr_keep _ _ F2 _ _ Mid)as Kept;rewrite At in Kept;injection Kept as <-.
   destruct(First _ _ Mid Origin Missing)as[body[Present Provenance]].
   exists body;split;[exact(Stable _ _ (@symbol_below_next mid key owner I Mid)Present)|].
   rewrite(Headers _ _ (@symbol_below_next mid key owner I Mid)Present);exact Provenance.
 - exact(Last _ _ At Origin Mid).
Qed.
Lemma new_lambda_origins_trans_except:forall initial mid final excluded,
 Inv mid->Fr initial mid->Fr mid final->excluded<st_next initial->
 (forall key body,key<>excluded->key<st_next mid->nassoc key(st_fns mid)=Some body->nassoc key(st_fns final)=Some body)->
 (forall key body,key<>excluded->key<st_next mid->nassoc key(st_fns mid)=Some body->
 function_header final key=function_header mid key)->
 NewLambdaOrigins initial mid->NewLambdaOrigins mid final->NewLambdaOrigins initial final.
Proof.
 intros initial mid final excluded I F1 F2 Bound Stable Headers First Last key owner At Origin Missing.
 destruct(nget(st_syms mid)key)as[old|]eqn:Mid.
 - pose proof(fr_keep _ _ F2 _ _ Mid)as Kept;rewrite At in Kept;injection Kept as <-.
   assert(Other:key<>excluded)by(pose proof(fr_new_above _ _ F1 _ _ Missing Mid);lia).
   destruct(First _ _ Mid Origin Missing)as[body[Present Provenance]].
   exists body;split;[exact(Stable _ _ Other(@symbol_below_next mid key owner I Mid)Present)|].
   rewrite(Headers _ _ Other(@symbol_below_next mid key owner I Mid)Present);exact Provenance.
 - exact(Last _ _ At Origin Mid).
Qed.
Lemma new_lambda_origins_trans_present:forall initial mid final,
 Inv mid->Fr initial mid->Fr mid final->fn_present_stable mid final->
 (forall key body,key<st_next mid->nassoc key(st_fns mid)=Some body->
 function_header final key=function_header mid key)->
 NewLambdaOrigins initial mid->NewLambdaOrigins mid final->NewLambdaOrigins initial final.
Proof.
 intros initial mid final I F1 F2 Stable Headers First Last key owner At Origin Missing.
 destruct(nget(st_syms mid)key)as[old|]eqn:Mid.
 - pose proof(fr_keep _ _ F2 _ _ Mid)as Kept;rewrite At in Kept;injection Kept as <-.
   destruct(First _ _ Mid Origin Missing)as[body[Present Provenance]].
   exists body;split;[exact(Stable _ _ (@symbol_below_next mid key owner I Mid)Present)|].
   rewrite(Headers _ _ (@symbol_below_next mid key owner I Mid)Present);exact Provenance.
 - exact(Last _ _ At Origin Mid).
Qed.
