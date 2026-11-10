(* Sequential actual result walks for method-only families. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 Merge CanonTypes NominalSupport CtxWalk KeyTranslation FamilyShapeExec FamilyMethodExec FamilyMethodCtx FamilyResultFold TySupport
 FamilyOutputSubst ProvideResultExec ProvideResultFold.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Section Walk.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Definition FamilyItemOK(fuel:nat):Prop:=forall c e F C fg mi sp outer ob out s e' keys s',
 provide_spec fuel e fg mi sp outer out s=Good((e',keys),s')->
 A.FamilyResult W(gm_path(get_mod w mi))sp->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)sp es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Definition FamilyItemsOK(fuel:nat):Prop:=forall c e F C fg mi specs outer ob out s e' keys s',
 mfold(fun acc sp=>provide_spec fuel(fst acc)fg mi sp outer(snd acc))(e,out)specs s=Good((e',keys),s')->
 Forall(A.FamilyResult W(gm_path(get_mod w mi)))specs->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,A.OutputItemsEntries W(gm_path(get_mod w mi))ob(A.c_ty C)specs es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Lemma family_items_fold:forall fuel,FamilyItemOK fuel->FamilyItemsOK fuel.
Proof.
 intros fuel Item;unfold FamilyItemsOK.
 intros c e F C fg mi specs;revert e C.
 induction specs as[|sp rest IH];intros e C outer ob out s e' keys s' E Family Hi Hw Hmi L HC Stable Wo So O.
 - cbn[mfold]in E;injection E as <- <- <-.
   exists[],[];split;[constructor|];split;[rewrite app_nil_r;reflexivity|].
   split;[reflexivity|];split;[constructor|rewrite family_empty_self by exact Stable;exact HC].
 - apply Forall_cons_iff in Family as[Hsp Hrest].
   cbn[mfold]in E;apply bind_inv in E as([e1 keys1]&s1&First&Tail).
   destruct(ok_provide_spec fuel e fg mi sp outer out _ _ _ Hi First)as[I1 Fr1].
   assert(Otail:ok(mfold(fun acc sp=>provide_spec fuel(fst acc)fg mi sp outer(snd acc))(e1,keys1)rest))by okt.
   destruct(Otail _ _ _ I1 Tail)as[_ Ft].
   destruct(Item c e F C fg mi sp outer ob out s e1 keys1 s1 First Hsp Hi Hw Hmi
    (later_back w P R Fl _ _ Ft L)HC Stable Wo So O)as(es1&fresh1&Sem1&Keys1&RK1&Support1&HC1).
   pose proof(provide_spec_frame fuel e fg mi sp outer out s e1 keys1 s1 First)as[T1[U1 L1]].
   assert(O1:map(fun q=>(fst q,tr Fl(canon e1(snd q))))outer=ob).
   {unfold canon;rewrite U1;exact O. }
   destruct(IH e1(A.output_cx C(A.c_ty C)es1)outer ob keys1 s1 e' keys s' Tail Hrest I1
    (fr_w' w _ _ Fr1 Hw)Hmi L HC1(family_output_stable C es1 Stable)Wo So O1)
    as(es2&fresh2&Sem2&Keys2&RK2&Support2&HC2).
   rewrite family_output_types in Sem2,HC2 by exact Stable.
   exists(es1++es2),(fresh1++fresh2);split.
   + eapply A.OIsCons;[exact Sem1|exact Sem2].
   + split.
     * rewrite Keys2,Keys1,<-app_assoc;reflexivity.
     * split.
       -- rewrite map_app;eapply result_keys_app;[exact RK1|exact RK2].
       -- split;[apply Forall_app;split;assumption|rewrite output_cx_append;exact HC2].
Qed.
Lemma family_method_walk:forall f c e F C fg mi pa dn a outer ob out s e' keys s',
 provide_spec(S f)e fg mi(Spec(Some pa)(Some dn)a)outer out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)(Some dn)a)es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Proof.
 intros f c e F C fg mi pa dn a outer ob out s e' keys s' E Hi Hw Hmi L HC Stable Wo So O.
 destruct(family_method_step w R Fl b Hload Finj c e F C f fg mi pa dn a outer ob out s e' keys s'
 E Hi Hw Hmi L HC Wo So O)as(r&mid&sg&app&Sem&Cr&Csg&Keys&ReceiverSupport&Ctx).
 exists[A.NMth(tr Fl r)mid sg app],[KyMth r mid];split;[exact Sem|];split;[exact Keys|].
 split.
 - unfold A.ResultKeys;cbn[flat_map A.entry_key map key_tr].
   rewrite app_nil_r.
   change([A.mkey(tr Fl r)mid]=[A.mkey(A.canon C(tr Fl r))mid]);rewrite Cr;reflexivity.
 - split;[constructor;[exact ReceiverSupport|constructor]|rewrite family_method_output by assumption;exact Ctx].
Qed.
Lemma family_item_successor:forall f,FamilyItemsOK f->FamilyItemOK(S f).
Proof.
 intros f Items;unfold FamilyItemOK.
 intros c e F C fg mi [pa dn a] outer ob out s e' keys s' E Family Hi Hw Hmi L HC Stable Wo So O.
 destruct pa as[pa|];destruct dn as[dn|].
 - eapply family_method_walk;eassumption.
 - inversion Family as[|mi0 pa0 br0 u d n its sps Res Decl Ast Esps Leafs];try subst mi0;try subst pa0;try subst br0.
   cbn[provide_spec]in E;apply bind_inv in E as(sub&sx&Sub&E).
   assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
   destruct(Os _ _ _ Hi Sub)as[Ix Fx].
   assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
   apply bind_inv in E as(tg&sr&Resolve&E).
   destruct(esym_rel w R Fl b Hload mi pa sx tg sr Ix Wx Hmi Resolve)as[-> Rel].
   destruct tg as[g|];[|exfalso;eapply err_not_good;exact E].
   destruct Rel as[Rg Hg].
   assert(Eu:gs_id g=u)by congruence.
   rewrite <-Eu in Decl.
   change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
   rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
   cbn[A.d_ast]in Ast;cbn[A.d_mod]in Leafs.
   assert(Kg:gs_kind g=KCtx).
   {pose proof(load_world_kind b w Hload _ _ Hg)as K;unfold KindOK in K;rewrite Ast in K.
    destruct(gs_kind g);cbn[A.kind_of kconv]in K;try discriminate K;reflexivity. }
   rewrite Kg,Ast in E;cbn beta iota zeta in E.
   subst its. rewrite ctx_specs_map,length_map,Nat.eqb_refl in E. cbn[negb] in E.
   assert(Otail:ok(mfold(fun acc it=>provide_spec f(fst acc)fg(gs_mod g)it sub(snd acc))(e,out)sps))by okt.
   destruct(Otail _ _ _ Ix E)as[_ Ft].
   destruct(family_result_subst w R Fl b Hload c e F C mi a outer ob s sub sx Sub Hi Hw Hmi
    (later_back w P R Fl _ _ Ft L)HC Wo So O)as(bs&Br&Bmap&Wsub&Ssub).
   destruct(Items c e F C fg(gs_mod g)sps sub(bs++ob)out sx e' keys s' E Leafs Ix Wx
    (loader_mod_in w R Fl b Hload _ _ Hg)L HC Stable Wsub Ssub Bmap)
    as(es&fresh&Sem&Keys&RK&Support&Ctx).
   exists es,fresh;split.
   + eapply A.OIECtx with(c:=gs_id g)(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))(n:=n)(its:=map CISpec sps)(sps:=sps)(bs:=bs).
     * exact Rg.
     * exact(w_decl_read w R Fl _ _ Hg).
     * exact Ast.
     * reflexivity.
     * exact Br.
     * exact Sem.
   + split;[exact Keys|split;[exact RK|split;assumption]].
 - inversion Family.
 - inversion Family.
Qed.
Theorem family_result_walk:forall fuel,FamilyItemOK fuel/\FamilyItemsOK fuel.
Proof.
 induction fuel as[|f[Item Items]].
 - assert(Item:FamilyItemOK 0).
   {unfold FamilyItemOK;intros c e F C fg mi sp outer ob out s e' keys s' E;
    exfalso;eapply err_not_good;exact E. }
   split;[exact Item|apply family_items_fold;exact Item].
 - pose proof(family_item_successor f Items)as Next.
   split;[exact Next|apply family_items_fold;exact Next].
Qed.
End Walk.
