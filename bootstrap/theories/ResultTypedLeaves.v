(* Complete value/function output leaves after the actual symbol resolution.
   Their bracket scopes and snapshots come from actual source execution. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 CtxWalk CtxMerge NominalSupport ProvideResultMethod ProvideResultReading ProvideResultFunction ProvideResultContext ProvideResultFold
 ProvideFunctorContext ProvideResultDomain ProvideValueContext ProvideFunctionContext OutputScopeExec FamilyResultFold ResultLeafTables.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[app_subst ety_of fn_sig FUEL reqs type_requirements].
Section Leaves.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem result_value_leaf:forall c e F C mi pa a outer ob out s sub sx g n t e' keys sf,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 provide_value_code e g t(sub++tymap e)out sx=Good((e',keys),sf)->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id g)->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADVal n t None->gs_kind g=KVal->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)None a)es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Proof.
 intros c e F C mi pa a outer ob out s sub sx g n t e' keys sf Sub Code Res Hg D K Hi Hw Hmi L HC Stable Wo So O.
 assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
 destruct(Os _ _ _ Hi Sub)as[Ix Fx].
 assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
 assert(Oc:ok(provide_value_code e g t(sub++tymap e)out)).
 {unfold provide_value_code;apply ok_bind;[apply ok_ety_of|intro];apply ok_bind;[apply output_provided_ok|intro;apply ok_ret]. }
 destruct(Oc _ _ _ Ix Code)as[_ Fc].
 pose proof(later_back w P R Fl _ _ Fc L)as Lx.
 destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
 Sub Hi Hw Hmi Lx HC Wo So O)as(bs&Br&Bmap&Wsub&Ssub&TR&St&M).
 assert(NotM:A.kind_sym W(gs_id g)<>Some A.DKMeth).
 {unfold A.kind_sym;rewrite(w_decl_rd w R Fl _ _ Hg);cbn[A.d_ast];rewrite D;discriminate. }
 destruct(provide_value_context_read w R Fl b Hload c e F C g t(sub++tymap e)
 (bs++ob++A.c_ty C)out sx e' keys sf Code Ix Wx(loader_mod_in w R Fl b Hload _ _ Hg)L HC TR St M NotM)
 as(y&app&stamp&Env&Keys&Ty&App&Ctx).
 exists[A.NVal(gs_id g)(tr Fl(canon e y))(output_inst Fl(freeze_app e app))],[KyVal(gs_id g)].
 split.
 - eapply A.OIEVal with(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))(bs:=bs).
   + exact Res.
   + exact(w_decl_rd w R Fl _ _ Hg).
   + exact D.
   + exact Br.
   + exact Ty.
   + exact App.
 - split;[exact Keys|].
   split;[reflexivity|].
   rewrite result_value_output by (try exact Stable;apply(family_canonical_receiver w R Fl _ _ _ _ HC)).
   exact Ctx.
Qed.
Theorem result_function_leaf:forall c e F C mi pa a outer ob out s sub sx g rcv n ps rt e' keys sf,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 provide_function_code e g(sub++tymap e)out sx=Good((e',keys),sf)->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id g)->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn rcv n ps rt None->gs_kind g=KFn->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)None a)es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Proof.
 intros c e F C mi pa a outer ob out s sub sx g rcv n ps rt e' keys sf Sub Code Res Hg D K Hi Hw Hmi L HC Stable Wo So O.
 assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
 destruct(Os _ _ _ Hi Sub)as[Ix Fx].
 assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
 assert(Oc:ok(provide_function_code e g(sub++tymap e)out)).
 {unfold provide_function_code;apply ok_bind;[apply output_signature_ok|intro];apply ok_bind;[apply output_provided_ok|intro;apply ok_ret]. }
 destruct(Oc _ _ _ Ix Code)as[_ Fc].
 pose proof(later_back w P R Fl _ _ Fc L)as Lx.
 destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
 Sub Hi Hw Hmi Lx HC Wo So O)as(bs&Br&Bmap&Wsub&Ssub&TR&St&M).
 assert(NotM:A.kind_sym W(gs_id g)<>Some A.DKMeth).
 {unfold A.kind_sym;rewrite(w_decl_rd w R Fl _ _ Hg);cbn[A.d_ast].
  pose proof(load_world_kind b w Hload _ _ Hg)as Kind;unfold KindOK in Kind;rewrite K in Kind;rewrite Kind;discriminate. }
 destruct(provide_function_context_read w R Fl b Hload c e F C g(sub++tymap e)
 (bs++ob++A.c_ty C)out sx e' keys sf rcv n ps rt None Code Ix Wx Hg D L HC TR St M NotM)
 as(sg&app&stamp&Env&Keys&Sig&App&Ctx).
 exists[A.NFn(gs_id g)(output_sig Fl e sg)(output_inst Fl(freeze_app e app))],[KyFn(gs_id g)].
 split.
 - eapply A.OIEFn with(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))(bs:=bs).
   + exact Res.
   + unfold A.kind_sym;rewrite(w_decl_rd w R Fl _ _ Hg);cbn[A.d_ast].
     pose proof(load_world_kind b w Hload _ _ Hg)as Kind;unfold KindOK in Kind;rewrite K in Kind;exact Kind.
   + exact(w_decl_rd w R Fl _ _ Hg).
   + exact D.
   + exact Br.
   + exact Sig.
   + exact App.
 - split;[exact Keys|].
   split;[reflexivity|].
   rewrite result_function_output by (try exact Stable;apply(family_canonical_signature w R Fl _ _ _ _ HC)).
   exact Ctx.
Qed.
Theorem result_functor_leaf:forall self c e F C mi pa a outer ob out s sub sx g n names args res e' keys sf,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)->
 provide_functor_code e g(sub++tymap e)out sx=Good((e',keys),sf)->
 A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id g)->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->gs_kind g=KFunctor->
 (forall t,A.ResultMentions W self(gm_path(get_mod w mi))(Spec(Some pa)None a)t->A.nlook t(ob++A.c_ty C)<>None)->
 (forall t,A.Mentions W self t->A.nlook t(A.c_ty C)<>None)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->CxRel w R Fl c e F C->
 family_types_stable C->Forall(fun q=>WFty(snd q))outer->Forall(fun q=>Support W(snd q))outer->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists es fresh,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)None a)es(A.c_ty C)/\
 keys=out++fresh/\A.ResultKeys(A.c_sub C)es(map(key_tr Fl)fresh)/\
 CxRel w R Fl c e' F(A.output_cx C(A.c_ty C)es).
Proof.
 intros self c e F C mi pa a outer ob out s sub sx g n names args res e' keys sf Sub Code Res Hg D K Dom Self Hi Hw Hmi L HC Stable Wo So O.
 assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
 destruct(Os _ _ _ Hi Sub)as[Ix Fx].
 assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
 assert(Oc:ok(provide_functor_code e g(sub++tymap e)out)).
 {unfold provide_functor_code,functor_fixed;okt. }
 destruct(Oc _ _ _ Ix Code)as[_ Fc].
 pose proof(later_back w P R Fl _ _ Fc L)as Lx.
 destruct(output_scope_execution w R Fl b Hload c e F C mi a outer ob s sub sx
 Sub Hi Hw Hmi Lx HC Wo So O)as(bs&Br&Bmap&Wsub&Ssub&TR&St&M).
 assert(Av:forall x,A.Fixed W(gs_id g)x->A.nlook x(bs++ob++A.c_ty C)<>None).
 {eapply(output_functor_domain W self(gm_path(get_mod w mi))pa a(gs_id g)
   (A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)))with(n:=n)(ps:=names)(args:=args)(res:=res)(tm:=ob++A.c_ty C)(bs:=bs).
  - exact Res.
  - exact(w_decl_rd w R Fl _ _ Hg).
  - exact D.
  - exact Br.
  - exact Dom.
  - intros x Mention;rewrite nlook_app;destruct(A.nlook x ob);[discriminate|exact(Self x Mention)]. }
 destruct(provide_functor_context_read w R Fl b Hload c e F C g(sub++tymap e)
 (bs++ob++A.c_ty C)out sx e' keys sf n names args res Code Ix Wx Hg D L HC M Av)
 as(fx&Read&Keys&Ctx).
 exists[A.NFun(gs_id g)fx],[KyFun(gs_id g)];split.
 - eapply A.OIEFun with(d:=A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))(bs:=bs).
   + exact Res.
   + exact(w_decl_rd w R Fl _ _ Hg).
   + exact D.
   + exact Br.
   + exact Av.
   + exact Read.
 - split;[exact Keys|];split;[reflexivity|].
   assert(Canon:A.fixed_subst(A.c_sub C)fx=fx).
   {eapply(result_fixed_readings_canonical w R Fl);[exact HC|exact M|exact Read]. }
   rewrite result_functor_output by assumption;exact Ctx.
Qed.
End Leaves.
