(* The abstract application rule, once its actual result reader has formed. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab ElabFrame BodySound BindSequence ApplyFreshExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Binding.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma cxrel_with_fresh:forall c e F C more,
 CxRel w R Fl c e F C->
 CxRel w R Fl(cx_with_fresh c(cx_fresh c++more))e(A.with_fresh F more)C.
Proof.
 intros c e F C more HC;pose proof(cr_fresh _ _ _ _ _ _ _ HC)as Fresh.
 destruct HC;constructor;cbn[cx_with_fresh A.with_fresh]in *;auto.
 change(A.fr_fresh F++more=cx_fresh c++more);now rewrite Fresh.
Qed.
Theorem abstract_bind_from_result:forall c e e' F C pa u Cu ns fx gens th sm Cout keys,
 A.resolve_sym W(A.fr_mod F)pa=Some u->A.nlook u(A.c_funs C)=Some fx->A.FunCtx W u Cu ns->
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty C)=Some here/\In(t,A.canon C here)th)->
 (forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx)->
 A.Satisfies C Cu ns sm->
 (forall t,A.FunOutputs W u t->
 (A.nlook t(A.c_ty C)=None/\generation_of gens t=t)\/
 (A.nlook t(A.c_ty C)<>None/\A.Fresh W(generation_of gens t)/\
 In(generation_of gens t)(map snd gens)/\In(t,TAbs(generation_of gens t))th))->
 A.ProvideResult W u C(fun t=>TAbs(generation_of gens t))Cout keys->CxRel w R Fl c e' F Cout->
 BindResult w P R Fl c e F C(Spec(Some pa)None None,None)[SApply u th sm]e'
 (cx_with_fresh c(cx_fresh c++map snd gens)).
Proof.
 intros c e e' F C pa u Cu ns fx gens th sm Cout keys Resolve Have FC Reads Fixed Sat Outputs Result HC.
 exists(A.with_fresh F(map snd gens)),Cout,(map snd gens),(fun x=>XApply u th sm x).
 split;[apply cxrel_with_fresh;exact HC|];split;[reflexivity|];split;[reflexivity|];split.
 - eapply A.BApplyAbstract; eassumption.
 - intro tail;reflexivity.
Qed.
End Binding.
