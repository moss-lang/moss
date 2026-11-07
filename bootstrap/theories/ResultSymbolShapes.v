(* Loaded kind validity fixes the declaration forms read by result visitors. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound.
Lemma result_val_shape:forall g,KindOK g->gs_kind g=KVal->
 exists n t body,gs_decl g=ADVal n t body.
Proof.
 intros g H K;unfold KindOK in H;rewrite K in H.
 destruct(gs_decl g);cbn[Audit.Elaboration.kind_of kconv]in H;
 try match type of H with context[match ?r with _=>_ end]=>destruct r end;try discriminate H.
 repeat eexists;reflexivity.
Qed.
Lemma result_fn_shape:forall g,KindOK g->gs_kind g=KFn->
 exists r n ps rt body,gs_decl g=ADFn r n ps rt body.
Proof.
 intros g H K;unfold KindOK in H;rewrite K in H.
 destruct(gs_decl g);cbn[Audit.Elaboration.kind_of kconv]in H;
 try match type of H with context[match ?r with _=>_ end]=>destruct r end;try discriminate H.
 repeat eexists;reflexivity.
Qed.
Lemma result_ctx_shape:forall g,KindOK g->gs_kind g=KCtx->
 exists n its,gs_decl g=ADCtx n its.
Proof.
 intros g H K;unfold KindOK in H;rewrite K in H.
 destruct(gs_decl g);cbn[Audit.Elaboration.kind_of kconv]in H;
 try match type of H with context[match ?r with _=>_ end]=>destruct r end;try discriminate H.
 repeat eexists;reflexivity.
Qed.
Lemma result_functor_shape:forall g,KindOK g->gs_kind g=KFunctor->
 exists n names args res body,gs_decl g=ADFunctor n names args res body.
Proof.
 intros g H K;unfold KindOK in H;rewrite K in H.
 destruct(gs_decl g);cbn[Audit.Elaboration.kind_of kconv]in H;
 try match type of H with context[match ?r with _=>_ end]=>destruct r end;try discriminate H.
 repeat eexists;reflexivity.
Qed.
