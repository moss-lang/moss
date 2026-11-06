(* Sequential result installation preserves occurrence order and last-wins tables. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 Merge CanonTypes NominalSupport CtxWalk CtxMerge FamilyShapeExec FamilyMethodExec FamilyMethodCtx FamilyResultFold TySupport
 FamilyOutputSubst ProvideResultExec ProvideResultFold.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Section Walk.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySound.W w R Fl).
Definition ResultItemOK(self fuel:nat):Prop:=forall c e F C fg mi sp outer ob out s e' keys s',
 provide_spec fuel e fg mi sp outer out s=Good((e',keys),s')->
 (forall t,A.ResultMentions W self(gm_path(get_mod w mi))sp t->A.nlook t(ob++A.c_ty C)<>None)->
 (forall t,A.Mentions W self t->A.nlook t(A.c_ty C)<>None)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)sp es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Definition ResultItemsOK(self fuel:nat):Prop:=forall c e F C fg mi specs outer ob out s e' keys s',
 mfold(fun acc sp=>provide_spec fuel(fst acc)fg mi sp outer(snd acc))(e,out)specs s=Good((e',keys),s')->
 (forall sp,In sp specs->forall t,A.ResultMentions W self(gm_path(get_mod w mi))sp t->A.nlook t(ob++A.c_ty C)<>None)->
 (forall t,A.Mentions W self t->A.nlook t(A.c_ty C)<>None)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,A.OutputItemsEntries W(gm_path(get_mod w mi))ob(A.c_ty C)specs es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Lemma result_items_fold:forall self fuel,ResultItemOK self fuel->ResultItemsOK self fuel.
Proof.
 intros self fuel Item;unfold ResultItemsOK.
 intros c e F C fg mi specs;revert e C.
 induction specs as[|sp rest IH];intros e C outer ob out s e' keys s' E Dom Self Hi Hw Hmi L HC Stable Wo So O.
 - cbn[mfold]in E;injection E as <- <- <-.
   exists[],[];split;[constructor|];split;[rewrite app_nil_r;reflexivity|].
   split;[reflexivity|rewrite family_empty_self by exact Stable;exact HC].
 - cbn[mfold]in E;apply bind_inv in E as([e1 keys1]&s1&First&Tail).
   destruct(ok_provide_spec fuel e fg mi sp outer out _ _ _ Hi First)as[I1 Fr1].
   assert(Otail:ok(mfold(fun acc sp=>provide_spec fuel(fst acc)fg mi sp outer(snd acc))(e1,keys1)rest))by okt.
   destruct(Otail _ _ _ I1 Tail)as[_ Ft].
   destruct(Item c e F C fg mi sp outer ob out s e1 keys1 s1 First (Dom sp(or_introl eq_refl)) Self Hi Hw Hmi
    (later_back w P R Fl _ _ Ft L)HC Stable Wo So O)as(es1&fresh1&Sem1&Keys1&RK1&HC1).
   pose proof(provide_spec_frame fuel e fg mi sp outer out s e1 keys1 s1 First)as[T1[U1 L1]].
   assert(O1:map(fun q=>(fst q,tr Fl(canon e1(snd q))))outer=ob).
   {unfold canon;rewrite U1;exact O. }
   destruct(IH e1(A.output_cx C(A.c_ty C)es1)outer ob keys1 s1 e' keys s' Tail ltac:(rewrite family_output_types by exact Stable;intros q Hq;exact(Dom q(or_intror Hq))) ltac:(rewrite family_output_types by exact Stable;exact Self) I1
    (fr_w' w _ _ Fr1 Hw)Hmi L HC1(family_output_stable C es1 Stable)Wo So O1)
    as(es2&fresh2&Sem2&Keys2&RK2&HC2).
   rewrite family_output_types in Sem2,HC2 by exact Stable.
   exists(es1++es2),(fresh1++fresh2);split.
   + eapply A.OIsCons;[exact Sem1|exact Sem2].
   + split.
     * rewrite Keys2,Keys1,<-app_assoc;reflexivity.
     * split.
       -- rewrite map_app;eapply result_keys_app;[exact RK1|exact RK2].
       -- rewrite output_cx_append;exact HC2.
Qed.
End Walk.
