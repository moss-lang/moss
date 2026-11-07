(* The actual platform-root context visitor, including its implicit types. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy BodyTr
 Merge CanonTypes CtxWalk CtxMerge CtxMethodExec LibraryFacts ResultMentionsExact ResultMentionsNoDup MethodCallCore XBridge.
Import ListNotations.
Local Open Scope string_scope.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements functor_fixed].
Section Root.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma sym_declared_read:forall x k s g s',
 A.kind_sym WW x=Some k->sym x s=Good(g,s')->Inv s->st_w s=w->
 s'=s/\gs_id g=x/\nget(w_syms w)(gs_id g)=Some g.
Proof.
 intros x k s g s' K E Hi Hw.
 unfold A.kind_sym,BodySoundCore.W,WorldSound.W_read in K;cbn[A.w_decl A.d_ast]in K.
 destruct(nget(w_syms w)x)as[g0|]eqn:Hg;[|discriminate K].
 assert(G:nget(st_syms s)x=Some g0)by exact(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)x g0 ltac:(rewrite Hw;exact Hg)).
 unfold sym in E;rewrite G in E;injection E as <- <-.
 pose proof(load_world_ids b w Hload x g0 Hg)as Id.
 split;[reflexivity|split;[exact Id|rewrite Id;exact Hg]].
Qed.
Lemma isym_type_exact:forall x tm es tm',
 A.kind_sym WW x=Some A.DKType->ISym w R Fl x[]tm es tm'->
 es=[A.NTy x]/\tm'=match A.nlook x tm with Some _=>tm|None=>(x,TAbs x)::tm end.
Proof.
 intros x tm es tm' K S.
 destruct S as[(Kt&->&->)|[(k&Kt&Hk&->&->)|[(d&n&t&ty0&ap&D&Da&Ty&Ap&->&->)|
 [(g&ap&Kt&Sg&Ap&->&->)|[(d&n&ps&args&res&fx&D&Da&Guard&Read&->&->)|(d&n&its&D&Da&Is)]]]]].
 - split;reflexivity.
 - rewrite K in Kt;injection Kt as <-;destruct Hk as[H|[H|[H|[H _]]]];discriminate H.
 - unfold A.kind_sym in K;rewrite D in K;cbn[A.d_ast]in K;rewrite Da in K;discriminate K.
 - rewrite K in Kt;discriminate Kt.
 - unfold A.kind_sym in K;rewrite D in K;cbn[A.d_ast]in K;rewrite Da in K;discriminate K.
 - unfold A.kind_sym in K;rewrite D in K;cbn[A.d_ast]in K;rewrite Da in K;discriminate K.
Qed.
Lemma isym_fun_exact:forall x tm es tm',
 A.kind_sym WW x=Some A.DKFunctor->ISym w R Fl x[]tm es tm'->
 (exists d n names args res,A.w_decl WW x=Some d/\A.d_ast d=ADFunctor n names args res None)/\
 exists fx,es=[A.NFun x fx]/\tm'=tm/\A.FixedReadings WW x tm fx.
Proof.
 intros x tm es tm' K S.
 destruct S as[(Kt&->&->)|[(k&Kt&Hk&->&->)|[(d&n&t&ty0&ap&D&Da&Ty&Ap&->&->)|
 [(g&ap&Kt&Sg&Ap&->&->)|[(d&n&ps&args&res&fx&D&Da&Guard&Read&->&->)|(d&n&its&D&Da&Is)]]]]].
 - rewrite K in Kt;discriminate Kt.
 - rewrite K in Kt;injection Kt as <-;destruct Hk as[H|[H|[H|[H _]]]];discriminate H.
 - unfold A.kind_sym in K;rewrite D in K;cbn[A.d_ast]in K;rewrite Da in K;discriminate K.
 - rewrite K in Kt;discriminate Kt.
 - split;[exists d,n,ps,args,res;split;assumption|exists fx;split;[reflexivity|split;[reflexivity|exact Read]]].
 - unfold A.kind_sym in K;rewrite D in K;cbn[A.d_ast]in K;rewrite Da in K;discriminate K.
Qed.
Lemma root_type_walk:forall cf mi xs e needs s e' needs' s' U tm les,
 mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e,needs)xs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 Forall(fun t=>A.kind_sym WW t=Some A.DKType)xs->
 exists les',map(tr_ent Fl)les'=map A.NTy xs/\WS Fl U e'(A.add_tys xs tm)(les++les')needs'.
Proof.
 intros cf mi xs;induction xs as[|x rest IH];intros e needs s e' needs' s' U tm les H Hi Hw Hmi L W Types.
 - injection H as <- <- <-;exists[];split;[reflexivity|rewrite app_nil_r;exact W].
 - inversion Types as[|x0 xs0 K Rest];subst x0 xs0.
   cbn[mfold]in H;apply bind_inv in H as([e1 ns1]&s1&H1&H).
   apply bind_inv in H1 as(tg&s0&Es&Eadd).
   destruct(sym_declared_read x A.DKType s tg s0 K Es Hi Hw)as[->[Id Gt]].
   destruct(ok_add_symbol_item cf e mi tg[]needs _ _ _ Hi Eadd)as[I1 F1].
   assert(Otail:ok(mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e1,ns1)rest))by okt.
   destruct(Otail _ _ _ I1 H)as[_ Ft].
   destruct(proj1(walk_all w(P:=P)R Fl b Hload(meth_step_holds w R Fl b Hload)cf)
    e mi tg[]needs s e1 ns1 s1 U tm les[] Eadd Hi Hw Hmi Gt(later_back w P R Fl _ _ Ft L)W eq_refl(Forall_nil _))
    as(l1&es1&tm1&S1&M1&W1).
   rewrite Id in S1.
   destruct(isym_type_exact x tm es1 tm1 K S1)as[-> ->].
   destruct(IH e1 ns1 s1 e' needs' s' U _ (les++l1)H I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)Hmi L W1 Rest)
    as(l2&M2&W2).
   exists(l1++l2);split.
   + rewrite map_app,M1,M2;reflexivity.
   + rewrite app_assoc;exact W2.
Qed.
Definition root_item_step(mi gi:nat)(acc:env*list key)(sp:spec):M(env*list key):=
 let '(e,needs):=acc in
 match sp with
 |Spec(Some p)None _=>
   let! fo:=Elab.resolve_sym gi p in
   match fo with
   |Some fg=>if negb(kind_eqb(gs_kind fg)KFunctor)||is_some(nget(en_funs e)(gs_id fg))then ret acc else
     let! xs:=functor_fixed fg in
     let! cf:=ctx_fuel in
     let! (e1,needs1):=mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e,needs)xs in
     add_symbol_item cf e1 mi fg[]needs1
   |None=>ret acc end
 |_=>ret acc end.
#[local] Hint Resolve ok_functor_fixed ok_ctx_fuel : okdb.
Lemma ok_root_item_step:forall mi gi acc sp,ok(root_item_step mi gi acc sp).
Proof. intros mi gi[e needs][pa d a];unfold root_item_step;destruct pa,d;okt. Qed.
Inductive RootItemView(q:string)(es0:list A.entry)(tm:list(nat*ty)):
 spec->list A.entry->list(nat*ty)->Prop:=
 | RootFunView:forall p a u du n names args res xs fx,
   A.resolve_sym WW q p=Some u->A.w_decl WW u=Some du->A.d_ast du=ADFunctor n names args res None->
   ~(exists fx0,In(A.NFun u fx0)es0)->NoDup xs->(forall x,In x xs<->A.Fixed WW u x)->
   A.FixedReadings WW u(A.add_tys xs tm)fx->
   RootItemView q es0 tm(Spec(Some p)None a)(map A.NTy xs++[A.NFun u fx])(A.add_tys xs tm)
 | RootSkipView:forall sp,
   (match sp with Spec(Some p)None _=>forall u,A.resolve_sym WW q p=Some u->
     A.kind_sym WW u=Some A.DKFunctor->exists fx,In(A.NFun u fx)es0|_=>True end)->
   RootItemView q es0 tm sp[]tm.
Lemma fun_in_entries:forall u les,
 (exists fx,In(A.NFun u fx)(map(tr_ent Fl)les))<->exists fx,In(LFun u fx)les.
Proof.
 intros u les;split.
 - intros[fx H];apply in_map_iff in H as(l&E&Hin);destruct l;cbn[tr_ent]in E;try discriminate E.
   injection E as <- _;eauto.
 - intros[fx H];exists(tr_fixed Fl fx);exact(in_map(tr_ent Fl)les(LFun u fx)H).
Qed.

Lemma assumed_functor_abstract:forall cf e mi fg needs s e' needs' s',
 gs_kind fg=KFunctor->add_symbol_item cf e mi fg[]needs s=Good((e',needs'),s')->
 exists n names args res,gs_decl fg=ADFunctor n names args res None.
Proof.
 intros[|cf]e mi fg needs s e' needs' s' K H;[method_noerr H|].
 cbn[add_symbol_item]in H;apply bind_inv in H as(u&s1&Et&H);rewrite K in H.
 destruct(gs_decl fg)as[asm ds|tn|an aty0|tag params pay|un|vn vt init|recv fnn ps rt body|cn its|fnm names args res body]eqn:D;
 try solve[method_noerr H].
 destruct body as[body|];[method_noerr H|].
 exists fnm,names,args,res;reflexivity.
Qed.
Theorem root_item_exec:forall mi gi e needs sp s e' needs' s' U tm les,
 root_item_step mi gi(e,needs)sp s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->In gi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 exists l1 es tm1,RootItemView(gm_path(get_mod w gi))(map(tr_ent Fl)les)tm sp es tm1/\
 map(tr_ent Fl)l1=es/\WS Fl U e' tm1(les++l1)needs'.
Proof.
 intros mi gi e needs[pa d a]s e' needs' s' U tm les H Hi Hw Hmi Hgi L W.
 unfold root_item_step in H;cbn beta iota zeta in H.
 destruct pa as[p|],d as[dn|];try solve[
  injection H as <- <- <-;exists[],[],tm;split;[constructor;exact I|split;[reflexivity|rewrite app_nil_r;exact W]]].
 apply bind_inv in H as(fo&s0&Er&H).
 destruct(esym_rel w R Fl b Hload gi p s fo s0 Hi Hw Hgi Er)as[-> Res].
 destruct fo as[fg|].
 2:{injection H as <- <- <-;exists[],[],tm;split;[constructor;intros u Ru Ku;rewrite Res in Ru;discriminate Ru|split;[reflexivity|rewrite app_nil_r;exact W]]. }
 destruct Res as[Res Hg].
 destruct(negb(kind_eqb(gs_kind fg)KFunctor)||is_some(nget(en_funs e)(gs_id fg)))eqn:Choice.
 - injection H as <- <- <-;exists[],[],tm;split;[|split;[reflexivity|rewrite app_nil_r;exact W]].
   constructor;intros u Ru Ku;rewrite Res in Ru;injection Ru as <-.
   rewrite(kind_sym_rd w R Fl b Hload _ _ Hg)in Ku;injection Ku as Ku.
   change(kconv(gs_kind fg)=kconv KFunctor)in Ku;apply kconv_inj in Ku.
   rewrite Ku in Choice;cbn[kind_eqb negb orb]in Choice.
   apply fun_in_entries;apply(proj1(ws_funs _ _ _ _ _ _ W(gs_id fg))).
   destruct(nget(en_funs e)(gs_id fg));[discriminate|discriminate Choice].
 - apply orb_false_iff in Choice as[Kbad Missing];apply negb_false_iff in Kbad;apply kind_eqb_spec in Kbad.
   assert(NonF:nget(en_funs e)(gs_id fg)=None).
   {destruct(nget(en_funs e)(gs_id fg));[discriminate Missing|reflexivity]. }
   apply bind_inv in H as(xs&s1&Hx&H);apply bind_inv in H as(cf&s2&Hcf&H).
   apply bind_inv in H as([e1 ns1]&s3&Htypes&Hfun).
   destruct(assumed_functor_abstract cf e1 mi fg ns1 s3 e' needs' s' Kbad Hfun)as(n&names&args&res&D).
   destruct(ok_functor_fixed fg s xs s1 Hi Hx)as[I1 F1].
   destruct(ok_ctx_fuel s1 cf s2 I1 Hcf)as[I2 F2].
   assert(Otypes:ok(mfold(fun acc t=>let! tg:=sym t in add_symbol_item cf(fst acc)mi tg[](snd acc))(e,needs)xs))by okt.
   destruct(Otypes _ _ _ I2 Htypes)as[I3 F3].
   destruct(ok_add_symbol_item cf e1 mi fg[]ns1 _ _ _ I3 Hfun)as[I4 F4].
   pose proof(later_back w P R Fl _ _(fr_trans _ _ _ F2(fr_trans _ _ _ F3 F4))L)as L1.
   pose proof(functor_fixed_exact w(P:=P)R Fl b Hload fg n names args res s xs s1 Hg D Hx Hi Hw L1)as Exact.
   assert(Types:Forall(fun x=>A.kind_sym WW x=Some A.DKType)xs).
   {apply Forall_forall;intros x Hin;exact(fixed_type b WW Hworld(gs_id fg)x(proj1(Exact x)Hin)). }
   destruct(root_type_walk cf mi xs e needs s2 e1 ns1 s3 U tm les Htypes I2
    ltac:(rewrite(fr_w _ _ F2),(fr_w _ _ F1);exact Hw)Hmi(later_back w P R Fl _ _ F4 L)W Types)
    as(lt&Mt&Wt).
   destruct(proj1(walk_all w(P:=P)R Fl b Hload(meth_step_holds w R Fl b Hload)cf)
    e1 mi fg[]ns1 s3 e' needs' s' U(A.add_tys xs tm)(les++lt)[] Hfun I3
    ltac:(rewrite(fr_w _ _ F3),(fr_w _ _ F2),(fr_w _ _ F1);exact Hw)Hmi Hg L Wt eq_refl(Forall_nil _))
    as(lf&esf&tmf&Sf&Mf&Wf).
   assert(Kf:A.kind_sym WW(gs_id fg)=Some A.DKFunctor).
   {rewrite(kind_sym_rd w R Fl b Hload _ _ Hg),Kbad;reflexivity. }
   destruct(isym_fun_exact(gs_id fg)(A.add_tys xs tm)esf tmf Kf Sf)as[_[fx[-> [-> Read]]]].
   exists(lt++lf),(map A.NTy xs++[A.NFun(gs_id fg)fx]),(A.add_tys xs tm);split.
   + eapply RootFunView with(du:=A.Decl(gm_path(get_mod w(gs_mod fg)))(gs_decl fg)(gs_assumes fg)).
     * exact Res.
     * exact(w_decl_rd w R Fl _ _ Hg).
     * cbn[A.d_ast];exact D.
     * intro Hin;apply fun_in_entries in Hin;pose proof(proj2(ws_funs _ _ _ _ _ _ W(gs_id fg))Hin)as Present.
       rewrite NonF in Present;apply Present;reflexivity.
     * exact(functor_fixed_nodup _ _ _ _ Hx).
     * exact Exact.
     * exact Read.
   + split;[rewrite map_app,Mt,Mf;reflexivity|rewrite app_assoc;exact Wf].
Qed.
Lemma root_view_cons:forall q es0 tm sp es1 tm1 rest es2 tm2,
 RootItemView q es0 tm sp es1 tm1->A.RootItems WW q(es0++es1)tm1 rest es2 tm2->
 A.RootItems WW q es0 tm(sp::rest)(es1++es2)tm2.
Proof.
 intros q es0 tm sp es1 tm1 rest es2 tm2 V Tail;destruct V.
 - rewrite<-app_assoc; eapply A.RIFun;eassumption.
 - cbn in *;rewrite app_nil_r in Tail;eapply A.RISkip;eassumption.
Qed.
Theorem root_items_exec:forall mi gi specs e needs s e' needs' s' U tm les,
 mfold(root_item_step mi gi)(e,needs)specs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->In gi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 exists lr er tmr,A.RootItems WW(gm_path(get_mod w gi))(map(tr_ent Fl)les)tm specs er tmr/\
 map(tr_ent Fl)lr=er/\WS Fl U e' tmr(les++lr)needs'.
Proof.
 intros mi gi specs;induction specs as[|sp rest IH];intros e needs s e' needs' s' U tm les H Hi Hw Hmi Hgi L W.
 - injection H as <- <- <-;exists[],[],tm;split;[constructor|split;[reflexivity|rewrite app_nil_r;exact W]].
 - cbn[mfold]in H;apply bind_inv in H as([e1 ns1]&s1&H1&H).
   destruct(ok_root_item_step mi gi(e,needs)sp s(e1,ns1)s1 Hi H1)as[I1 F1].
   destruct(ok_mfold _ _ _ rest(e1,ns1)(fun acc sp=>ok_root_item_step mi gi acc sp)s1(e',needs')s' I1 H)as[_ Ft].
   destruct(root_item_exec mi gi e needs sp s e1 ns1 s1 U tm les H1 Hi Hw Hmi Hgi(later_back w P R Fl _ _ Ft L)W)
    as(l1&es1&tm1&V&M1&W1).
   destruct(IH e1 ns1 s1 e' needs' s' U tm1(les++l1)H I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)Hmi Hgi L W1)
    as(l2&es2&tm2&Tail&M2&W2).
   rewrite map_app,M1 in Tail.
   exists(l1++l2),(es1++es2),tm2;split;[eapply root_view_cons;eassumption|].
   split;[rewrite map_app,M1,M2;reflexivity|rewrite app_assoc;exact W2].
Qed.
Lemma lang_gc_source:forall gc,A.LangSym WW "wasm" A.NSName "Gc" gc->
 exists wm,sassoc "wasm"(lang_modules w)=Some wm/\sassoc "Gc"(gm_names(get_mod w wm))=Some gc.
Proof.
 intros gc(d&p&Hd&Lib&Scope).
 destruct(selected_scope w R Fl p A.NSName "Gc" gc Lib Scope)as(wm&Ew&Ep&Es).
 exists wm;split;[exact Ew|exact Es].
Qed.
Lemma sym_world_decl_read:forall x d s g s',
 A.w_decl WW x=Some d->sym x s=Good(g,s')->Inv s->st_w s=w->
 s'=s/\gs_id g=x/\nget(w_syms w)(gs_id g)=Some g.
Proof.
 intros x d s g s' Hd E Hi Hw.
 unfold BodySoundCore.W,WorldSound.W_read in Hd;cbn[A.w_decl]in Hd.
 destruct(nget(w_syms w)x)as[g0|]eqn:Hg;[|discriminate Hd].
 assert(G:nget(st_syms s)x=Some g0)by exact(iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)x g0 ltac:(rewrite Hw;exact Hg)).
 unfold sym in E;rewrite G in E;injection E as <- <-.
 pose proof(load_world_ids b w Hload x g0 Hg)as Id.
 split;[reflexivity|split;[exact Id|rewrite Id;exact Hg]].
Qed.
Lemma root_gc_not_context:forall wm x gc,
 sassoc "wasm"(lang_modules w)=Some wm->sassoc "Gc"(gm_names(get_mod w wm))=Some x->
 gs_id gc=x->nget(w_syms w)(gs_id gc)=Some gc->
 (forall n its,gs_decl gc<>ADCtx n its)->
 forall gc' d n its,A.LangSym WW "wasm" A.NSName "Gc" gc'->A.w_decl WW gc'=Some d->A.d_ast d<>ADCtx n its.
Proof.
 intros wm x gc Ew Ex Id Hg N gc' d n its Lib Hd D.
 destruct(lang_gc_source gc' Lib)as(wm'&Ew'&Ex').
 rewrite Ew in Ew';injection Ew' as <-;rewrite Ex in Ex';injection Ex' as <-.
 rewrite<-Id,(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-;cbn[A.d_ast]in D.
 exact(N n its D).
Qed.
Theorem add_root_context_exec:forall main e mi needs s e' needs' s' U tm les,
 A.IsMainFn WW main->add_root_context e mi needs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 exists lr er tmr,A.RootCtx WW main(map(tr_ent Fl)les)tm er tmr/\
 map(tr_ent Fl)lr=er/\WS Fl U e' tmr(les++lr)needs'.
Proof.
 intros main e mi needs s e' needs' s' U tm les Main H Hi Hw Hmi L W.
 assert(O:ok(add_root_context e mi needs))by(unfold add_root_context;okt).
 destruct(O _ _ _ Hi H)as[_ Froot].
 pose proof(later_back w P R Fl _ _ Froot L)as L0.
 unfold add_root_context in H;apply bind_inv in H as(wm0&s0&Elm&H).
 unfold lang in Elm;apply bind_inv in Elm as(st0&s2&Eg&Elm);unfold get in Eg;injection Eg as <- <-;injection Elm as <- <-.
 rewrite(lt_lang _ _ _ _ _ L0)in H.
 destruct(sassoc "wasm"(lang_modules w))as[wm|]eqn:Ewm.
 2:{injection H as <- <- <-;exists[],[],tm;split;[|split;[reflexivity|rewrite app_nil_r;exact W]].
    apply A.RootNone;right;intros gc d n its Lib Hd D.
    destruct(lang_gc_source gc Lib)as(wm'&Ew&Ex);rewrite Ewm in Ew;discriminate Ew. }
 apply bind_inv in H as(gm&s0&Em&H);unfold the_mod in Em;injection Em as <- <-;rewrite Hw in H.
 destruct(sassoc "Gc"(gm_names(get_mod w wm)))as[x|]eqn:Egc.
 2:{injection H as <- <- <-;exists[],[],tm;split;[|split;[reflexivity|rewrite app_nil_r;exact W]].
    apply A.RootNone;right;intros gc d n its Lib Hd D.
    destruct(lang_gc_source gc Lib)as(wm'&Ew&Ex);rewrite Ewm in Ew;injection Ew as <-;rewrite Egc in Ex;discriminate Ex. }
 apply bind_inv in H as(gc&s0&Es&H).
 pose proof(lang_symbol_scope b w R Fl Hload "wasm" false "Gc" wm x Ewm Egc)as Lib.
 destruct Lib as(d&p&Hd&Library&Scope).
 destruct(sym_world_decl_read x d s gc s0 Hd Es Hi Hw)as[->[Id Hg]].
 assert(Lib:A.LangSym WW "wasm" A.NSName "Gc"(gs_id gc)).
 {rewrite Id;exists d,p;repeat split;assumption. }
 destruct(gs_decl gc)as[asm ds|tn|an aty0|tag params pay|un|vn vt init|recv fnn ps rt body|cn its|fnm names args res body]eqn:D;
 try solve[destruct(gs_kind gc);(injection H as <- <- <-;exists[],[],tm;split;
   [apply A.RootNone;right;eapply root_gc_not_context;[exact Ewm|exact Egc|exact Id|exact Hg|];intros n0 its0 Contra;try rewrite D in Contra;discriminate Contra
   |split;[reflexivity|rewrite app_nil_r;exact W]])].
 assert(K:gs_kind gc=KCtx).
 {pose proof(load_world_kind b w Hload _ _ Hg)as K;unfold KindOK in K;rewrite D in K.
  destruct(gs_kind gc);cbn[kconv A.kind_of]in K;try discriminate K;reflexivity. }
 rewrite K in H.
 change(mfold(root_item_step mi(gs_mod gc))(e,needs)its s=Good((e',needs'),s'))in H.
 destruct(root_items_exec mi(gs_mod gc)its e needs s e' needs' s' U tm les H Hi Hw Hmi
  (loader_mod_in w R Fl b Hload _ _ Hg)L W)as(lr&er&tmr&Root&Maps&Wr).
 exists lr,er,tmr;split;[|split;assumption].
 eapply A.RootGc with(gc:=gs_id gc)(d:=A.Decl(gm_path(get_mod w(gs_mod gc)))(gs_decl gc)(gs_assumes gc)).
 - exact Main.
 - exact Lib.
 - exact(w_decl_rd w R Fl _ _ Hg).
 - cbn[A.d_ast];exact D.
 - exact Root.
Qed.
End Root.
