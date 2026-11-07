(* The actual general result visitor, with recursive contexts, sequential
   occurrences, and fixed readings derived from real mention availability. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 CtxWalk CtxMerge NominalSupport FamilyResultFold FamilyResultWalk
 OutputScopeExec ResultItemsFold ResultTypedLeaves ResultTrivialLeaves ResultSymbolShapes
 ProvideResultDomain ProvideResultReading ProvideResultFunction ProvideResultMethod ProvideFunctorContext.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[app_subst ety_of fn_sig functor_fixed FUEL reqs type_requirements].
Section Walk.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Lemma result_item_successor:forall self f,
 ResultItemsOK w (P:=P) R Fl self f->ResultItemOK w (P:=P) R Fl self(S f).
Proof.
 intros self f Items;unfold ResultItemOK.
 intros c e F C fg mi[pa dn a]outer ob out s e' keys sf Run Dom Self Hi Hw Hmi L HC Stable Wo So O.
 destruct pa as[pa|];destruct dn as[dn|].
 - destruct(family_method_walk w R Fl b Hload Finj f c e F C fg mi pa dn a outer ob out s e' keys sf
   Run Hi Hw Hmi L HC Stable Wo So O)as(es&fresh&Sem&Keys&RK&Support&Ctx).
   exists es,fresh;split;[exact Sem|];split;[exact Keys|];split;assumption.
 - cbn[provide_spec]in Run;apply bind_inv in Run as(sub&sx&Sub&Run).
   assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
   destruct(Os _ _ _ Hi Sub)as[Ix Fx].
   assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
   apply bind_inv in Run as(tg&sy&Resolve&Run).
   destruct(esym_rel w R Fl b Hload mi pa sx tg sy Ix Wx Hmi Resolve)as[-> Rel].
   destruct tg as[g|];[|exfalso;eapply err_not_good;exact Run].
   destruct Rel as[Res Hg].
   pose proof(load_world_kind b w Hload _ _ Hg)as Kind.
   destruct(gs_kind g)eqn:K.
   + rewrite ?K in Run;cbn beta iota zeta in Run;injection Run as <- <- <-.
     eapply(result_type_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|exact K|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + unfold already_provided in Run;rewrite ?K in Run;cbn beta iota zeta in Run;injection Run as <- <- <-.
     eapply(result_defined_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|unfold already_provided;rewrite K;reflexivity|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + unfold already_provided in Run;rewrite ?K in Run;cbn beta iota zeta in Run;injection Run as <- <- <-.
     eapply(result_defined_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|unfold already_provided;rewrite K;reflexivity|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + unfold already_provided in Run;rewrite ?K in Run;cbn beta iota zeta in Run;injection Run as <- <- <-.
     eapply(result_defined_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|unfold already_provided;rewrite K;reflexivity|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + destruct(result_val_shape g Kind K)as(n&t&body&D).
     unfold already_provided in Run;rewrite ?K,?D in Run;cbn beta iota zeta in Run.
     destruct body as[body|].
     * cbn beta iota in Run;injection Run as <- <- <-.
       eapply(result_defined_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|unfold already_provided;rewrite K,D;reflexivity|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
     * cbn beta iota zeta in Run.
       eapply(result_value_leaf w R Fl b Hload);[exact Sub|exact Run|exact Res|exact Hg|exact D|exact K|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + destruct(result_fn_shape g Kind K)as(rcv&n&ps&rt&body&D).
     unfold already_provided,fn_body in Run;rewrite ?K,?D in Run;cbn beta iota zeta in Run.
     destruct body as[body|].
     * cbn[is_some]in Run;injection Run as <- <- <-.
       eapply(result_defined_leaf w R Fl b Hload);[exact Sub|exact Res|exact Hg|unfold already_provided,fn_body;rewrite K,D;reflexivity|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
     * cbn[is_some]in Run.
       eapply(result_function_leaf w R Fl b Hload);[exact Sub|exact Run|exact Res|exact Hg|exact D|exact K|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
   + destruct(result_ctx_shape g Kind K)as(n&its&D).
     rewrite ?K,?D in Run;cbn beta iota zeta in Run.
     assert(Otail:ok(mfold(fun acc it=>provide_spec f(fst acc)fg(gs_mod g)it sub(snd acc))(e,out)its))by okt.
     destruct(Otail _ _ _ Ix Run)as[_ Ft].
     destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
      Sub Hi Hw Hmi(later_back w P R Fl _ _ Ft L)HC Wo So O)
      as(bs&Br&Bmap&Wsub&Ssub&TR&St&M).
     assert(Child:forall sp,In sp its->forall t,A.ResultMentions W self(gm_path(get_mod w(gs_mod g)))sp t->A.nlook t((bs++ob)++A.c_ty C)<>None).
     {intros sp Insp t Mention;rewrite <-app_assoc.
      eapply(output_context_domain W self(gm_path(get_mod w mi))pa a(gs_id g)
       (A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))n its(ob++A.c_ty C)bs);
      [exact Res|exact(w_decl_rd w R Fl _ _ Hg)|exact D|exact Br|exact Dom|exact Insp|exact Mention]. }
     destruct(Items c e F C fg(gs_mod g)its sub(bs++ob)out sx e' keys sf Run Child Self Ix Wx
      (loader_mod_in w R Fl b Hload _ _ Hg)L HC Stable Wsub Ssub Bmap)as(es&fresh&Sem&Keys&RK&Ctx).
     exists es,fresh;split.
     * eapply A.OIECtx with(c:=gs_id g)(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))(n:=n)(its:=its)(bs:=bs);
       [exact Res|exact(w_decl_rd w R Fl _ _ Hg)|exact D|exact Br|exact Sem].
     * split;[exact Keys|];split;assumption.
   + unfold already_provided in Run;rewrite ?K in Run;cbn beta iota zeta in Run.
     exfalso;eapply err_not_good;exact Run.
   + destruct(result_functor_shape g Kind K)as(n&names&args&res&body&D).
     unfold already_provided in Run;rewrite ?K,?D in Run;cbn beta iota zeta in Run.
     destruct body as[body|];[exfalso;eapply err_not_good;exact Run|].
     cbn beta iota zeta in Run.
     eapply(result_functor_leaf w R Fl b Hload);[exact Sub|exact Run|exact Res|exact Hg|exact D|exact K|exact Dom|exact Self|exact Hi|exact Hw|exact Hmi|exact L|exact HC|exact Stable|exact Wo|exact So|exact O].
 - cbn[provide_spec]in Run;apply bind_inv in Run as(sub&sx&Sub&Run);exfalso;eapply err_not_good;exact Run.
 - cbn[provide_spec]in Run;apply bind_inv in Run as(sub&sx&Sub&Run);exfalso;eapply err_not_good;exact Run.
Qed.
Theorem result_walk:forall self fuel,ResultItemOK w (P:=P) R Fl self fuel/\ResultItemsOK w (P:=P) R Fl self fuel.
Proof.
 intros self fuel;induction fuel as[|f[Item Items]].
 - assert(Item:ResultItemOK w (P:=P) R Fl self 0).
   {unfold ResultItemOK;intros;exfalso;eapply err_not_good;exact H. }
   split;[exact Item|apply result_items_fold;exact Item].
 - pose proof(result_item_successor self f Items)as Next.
   split;[exact Next|apply result_items_fold;exact Next].
Qed.
End Walk.
