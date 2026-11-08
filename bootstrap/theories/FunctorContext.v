(* Formation of a functor's own executable input context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy Merge CanonTypes
 SourceClean ContextClean CtxWalk CtxMerge CtxMethodExec FunctorMerge ImplicitTypeWalk FunctorScopeTypes ResultMentionsExact NestingTypesExec NeedKeys.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Section Context.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Let MS:@MethStep w P R Fl:=meth_step_holds w R Fl b Hload.
Lemma specs_walk:forall f mi specs e needs s e' needs' s' U tm les,
 mfold(fun acc sp=>add_spec f(fst acc)mi sp[](snd acc))(e,needs)specs s=Good((e',needs'),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->WS Fl U e tm les needs->
 exists added es tm',A.ItemsEntries WW(gm_path(get_mod w mi))[]tm specs es tm'/\
 map(tr_ent Fl)added=es/\WS Fl U e' tm'(les++added)needs'.
Proof.
 intros f mi specs;induction specs as[|sp rest IH];intros e needs s e' needs' s' U tm les H Hi Hw Hmi L WS0.
 - cbn[mfold]in H;injection H as <- <- <-;exists[],[],tm;split;[constructor|]. split;[reflexivity|].
   rewrite app_nil_r;exact WS0.
 - cbn[mfold]in H;apply bind_inv in H as([e1 n1]&s1&E1&H).
   destruct(ok_add_spec f e mi sp[]needs _ _ _ Hi E1)as[I1 F1].
   destruct(ok_mfold _ _ _ rest(e1,n1)(fun acc q=>ok_add_spec f(fst acc)mi q[](snd acc))_ _ _ I1 H)as[_ Ft].
   destruct(proj2(walk_all w (P:=P) R Fl b Hload MS f)e mi sp[]needs s e1 n1 s1 U tm les[]
    E1 Hi Hw Hmi(later_back w P R Fl _ _ Ft L)WS0 eq_refl(Forall_nil _))as(a1&es1&tm1&IE1&M1&WS1).
   destruct(IH e1 n1 s1 e' needs' s' U tm1(les++a1)H I1 ltac:(rewrite(fr_w _ _ F1);exact Hw)Hmi L WS1)
    as(a2&es2&tm2&IE2&M2&WS2).
   exists(a1++a2),(es1++es2),tm2;split;[econstructor;eassumption|]. split.
   + rewrite map_app,M1,M2;reflexivity.
   + rewrite app_assoc;exact WS2.
Qed.


Definition own_walk(f mi:nat)(args:list spec)(outs ms:list nat)(e:env):M(env*list key):=
 let! (e1,n1):=mfold(fun acc sp=>add_spec f(fst acc)mi sp[](snd acc))(e,[])args in
 mfold(implicit_type_step f mi outs)(e1,n1)ms.
Lemma ok_own_walk:forall f mi args outs ms e,ok(own_walk f mi args outs ms e).
Proof.
 intros;unfold own_walk;apply ok_bind.
 - apply ok_mfold;intros;apply ok_add_spec.
 - intros[e0 n0];apply ok_mfold;intros;apply ok_implicit_type_step.
Qed.
Definition OwnRd(mi:nat)(args:list spec)(outs ms:list nat)(es:list A.entry)(tm:list(nat*ty)):Prop:=
 exists aes atm added,A.ItemsEntries WW(gm_path(get_mod w mi))[][]args aes atm/\
 (forall t,In t added<->In t ms/\~In t outs/\A.nlook t atm=None)/\
 es=aes++map A.NTy added/\tm=map(fun t=>(t,TAbs t))(rev added)++atm.
Lemma own_rd_unifies:forall mi args outs ms es tm es' tm' sb,
 OwnRd mi args outs ms es tm->OwnRd mi args outs ms es' tm'->
 A.Unifies sb es<->A.Unifies sb es'.
Proof.
 intros mi args outs ms es tm es' tm' sb(aes&atm&ad&I&_&->&_)(bes&btm&bd&J&_&->&_).
 rewrite!unifies_append_types;eapply ies_unifies;eauto.
Qed.
Lemma own_round:forall f mi args outs ms U s e needs s',
 In mi(w_order w)->(forall t,In t ms->A.kind_sym WW t=Some A.DKType)->
 Tri U->UfOK U->Inv s->st_w s=w->Later w P R Fl s'->
 own_walk f mi args outs ms(env_with_edges[]U)s=Good((e,needs),s')->
 exists les tm,OwnRd mi args outs ms(map(tr_ent Fl)les)tm/\WS Fl U e tm les needs.
Proof.
 intros f mi args outs ms U s e needs s' Hmi Types T O Hi Hw L H.
 unfold own_walk in H;apply bind_inv in H as([e1 n1]&s1&Hargs&H).
 assert(Oa:ok(mfold(fun acc sp=>add_spec f(fst acc)mi sp[](snd acc))(env_with_edges[]U,[])args)).
 { apply ok_mfold;intros;apply ok_add_spec. }
 destruct(Oa _ _ _ Hi Hargs)as[I1 F1].
 assert(Oi:ok(mfold(implicit_type_step f mi outs)(e1,n1)ms)).
 { apply ok_mfold;intros;apply ok_implicit_type_step. }
 destruct(Oi _ _ _ I1 H)as[_ Fi].
 destruct(specs_walk f mi args _ []s e1 n1 s1 U[][]Hargs Hi Hw Hmi
  (later_back w P R Fl _ _ Fi L)(ws_start Fl U T O))as(al&aes&atm&IA&M&WS1).
 cbn[app]in WS1.
 destruct(implicit_types_walk b w R Fl f mi outs ms e1 n1 s1 e needs s' U atm al Hload Types H I1
  ltac:(rewrite(fr_w _ _ F1);exact Hw)WS1)as(added&->&Uf&WS2&Member&Missing).
 exists(al++map LTy added),(map(fun t=>(t,TAbs t))(rev added)++atm);split;[|exact WS2].
 exists aes,atm,added;split;[exact IA|]. split.
 - intro t;rewrite Member,(ws_tn _ _ _ _ _ _ WS1 t).
   destruct(tn_get e1 t);cbn;intuition discriminate.
 - split;[rewrite map_app,M,map_map;reflexivity|reflexivity].
Qed.
Theorem own_merged:forall(Finj:forall a c,Fl a=Fl c->a=c)f mi args outs ms s e needs s',
 In mi(w_order w)->(forall t,In t ms->A.kind_sym WW t=Some A.DKType)->
 merged_walk mi(own_walk f mi args outs ms)s=Good((e,needs),s')->
 Inv s->st_w s=w->Later w P R Fl s'->
 exists les tm,OwnRd mi args outs ms(map(tr_ent Fl)les)tm/\WS Fl(en_uf e)e tm les needs/\
 A.MostGeneralMerge(suf Fl(en_uf e))(map(tr_ent Fl)les)/\
 A.Needs(suf Fl(en_uf e))(map(tr_ent Fl)les)(map(key_tr Fl)needs).
Proof.
 intros Finj f mi args outs ms s e needs s' Hmi Types H Hi Hw L.
 apply(merged_walk_equiv_spec Fl(own_walk f mi args outs ms)Finj(ok_own_walk f mi args outs ms)
  (OwnRd mi args outs ms)(own_rd_unifies mi args outs ms)
  (fun s=>st_w s=w)(Later w P R Fl)
  (fun s0 s1 F E=>fr_w' w s0 s1 F E)(fun s0 s1 F L1=>later_back w P R Fl s0 s1 F L1))
  with(mi:=mi)(s:=s)(s':=s');[|exact H|exact Hi|exact Hw|exact L].
 intros U s0 e0 n0 s0' T O I W L0 E.
 exact(own_round f mi args outs ms U s0 e0 n0 s0' Hmi Types T O I W L0 E).
Qed.
Lemma mentions_from_lists:forall g n names args res ms outs,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 (forall t,In t ms<->exists sp,In sp res/\A.ResultMentions WW(gs_id g)(gm_path(get_mod w(gs_mod g)))sp t)->
 (forall t,In t outs<->A.FunOutputs WW(gs_id g)t)->
 forall t,In t ms/\~In t outs<->A.Mentions WW(gs_id g)t.
Proof.
 intros g n names args res ms outs Hg D M O t;split.
 - intros[Ht No];destruct(proj1(M t)Ht)as(sp&Hin&HM).
   exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,sp.
   split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split.
   + cbn[A.d_ast A.functor_parts];rewrite D;reflexivity.
   + split;[exact Hin|]. split;[exact HM|]. intro Hout;apply No,(proj2(O t));exact Hout.
 - intros(d&ps&a&r&body&sp&Hd&Hp&Hin&HM&No).
   rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
   cbn[A.d_ast A.functor_parts]in Hp;rewrite D in Hp;injection Hp as <- <- <- <-.
   cbn[A.d_mod]in HM;split.
   + apply(M t);exists sp;split;assumption.
   + intro HinOut;apply No,(proj1(O t));exact HinOut.
Qed.
Record FunEnvTables(e:env)(C:A.cx):Prop:={
 fet_sub:A.c_sub C=suf Fl(en_uf e);
 fet_ty:forall k,A.nlook k(A.c_ty C)=option_map(fun y=>tr Fl(canon e y))(tn_get e k);
 fet_vals:forall v,A.nlook v(A.c_vals C)=option_map(fun y=>tr Fl(canon e y))(nget(en_vals e)v);
 fet_fns:forall f,A.nlook f(A.c_fns C)=option_map(csig Fl e)(nget(en_fns e)f);
 fet_mths:forall k m,ety_ok k=true->canon e k=k->A.mlook(tr Fl k)m(A.c_mths C)=option_map(csig Fl e)(get_mth e k m);
 fet_funs:forall u,A.nlook u(A.c_funs C)=option_map(fun p=>A.known(tr_inst Fl(fst p)))(get_app e(KyFun u))/\
   is_some(A.nlook u(A.c_funs C))=is_some(nget(en_funs e)u);
 fet_apps:forall k,nofun k=true->key_ok k=true->A.ilook(key_tr Fl k)(A.c_apps C)=
   option_map(fun p=>tr_inst Fl(fst p))(get_app e k)
}.
Theorem fun_env_context_valid:forall g n names args res s e needs s',
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists C,A.FunCtx WW(gs_id g)C(map(key_tr Fl)needs)/\
   FunEnvTables e C /\ Tri(en_uf e) /\ Forall(fun k=>key_ok k=true)needs /\ merge_edges e=en_uf e.
Proof.
 intros g n names args res s e needs s' Wo Hg D H Hi Hw L.
 unfold fun_env in H;unfold functor_parts in H;rewrite D in H;cbn beta iota zeta in H.
 apply bind_inv in H as(outs&s1&Ho&H).
 destruct(result_types_rel w R Fl b Hload g n names args res None s outs s1 Hg D Ho Hi Hw)as[->[_ Outs]].
 apply bind_inv in H as(ms&s2&Hms&H).
 assert(Om:ok(result_mentions g))by(unfold result_mentions;okt).
 destruct(Om _ _ _ Hi Hms)as[I2 F2].
 assert(W2:st_w s2=w)by(rewrite(fr_w _ _ F2);exact Hw).
 apply bind_inv in H as(cf&s3&Hcf&H).
 destruct(ok_ctx_fuel _ _ _ I2 Hcf)as[I3 F3].
 apply bind_inv in H as([e0 ns]&s4&Hwalk&H);injection H as <- <- <-.
 change(merged_walk(gs_mod g)(own_walk cf(gs_mod g)args outs ms)s3=Good((e0,ns),s4))in Hwalk.
 assert(Owalk:ok(merged_walk(gs_mod g)(own_walk cf(gs_mod g)args outs ms))).
 { apply ok_merged_walk_frame;intro;apply ok_own_walk. }
 destruct(Owalk _ _ _ I3 Hwalk)as[_ Fw].
 assert(L2:Later w P R Fl s2)by exact(later_back w P R Fl _ _ F3(later_back w P R Fl _ _ Fw L)).
 pose proof(result_mentions_exact w (P:=P) R Fl b Hload g n names args res None s ms s2 Hg D Hms Hi Hw L2)as Ms.
 pose proof(mentions_from_lists g n names args res ms outs Hg D Ms Outs)as Mentions.
 assert(Types:forall t,In t ms->A.kind_sym WW t=Some A.DKType).
 { exact(actual_result_mentions_types b w R Fl P g n names args res None s ms s2 Hload Wo Hg D Hms Hi Hw L2). }
 assert(Finj:forall a c,Fl a=Fl c->a=c)by exact(A.wo_fld Wo).
 destruct(own_merged Finj cf(gs_mod g)args outs ms s3 e0 ns s4(loader_mod_in w R Fl b Hload _ _ Hg)Types Hwalk I3
  ltac:(rewrite(fr_w _ _ F3);exact W2)L)as(les&tm&Rd&WS0&Mg&Nd).
 destruct Rd as(aes&atm&added&IA&Member&Es&Tm).
 set(sigma:=suf Fl(en_uf e0)).
 exists(A.entries_cx sigma tm(map(tr_ent Fl)les)). split.
 {
 exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,aes,atm,tm,sigma,(rev added).
 split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split.
 - cbn[A.d_ast A.functor_parts];rewrite D;reflexivity.
 - split;[exact IA|]. split.
   + intro t;rewrite<-in_rev,Member;rewrite<-Mentions;tauto.
   + split;[exact Tm|]. split.
     * apply(proj1(mgm_append_types sigma aes added));rewrite<-Es;exact Mg.
     * split.
       -- rewrite Es,entries_cx_append_types;reflexivity.
       -- apply(proj1(needs_append_types sigma aes added(map(key_tr Fl)ns)));rewrite<-Es;exact Nd.
 }
 split.
 { constructor.
 - reflexivity.
 - intro k;exact(ctx_ty Fl(en_uf e0)e0 tm les ns WS0 eq_refl k).
 - intro v;exact(ctx_vals Fl(en_uf e0)e0 tm les ns WS0 eq_refl v).
 - intro f;exact(ctx_fns Fl(en_uf e0)e0 tm les ns WS0 eq_refl f).
 - intros k m Ok Ck;exact(ctx_mths Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl k m Ok Ck).
 - intro u;exact(ctx_funpack Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl u).
 - intros k Nk Ok;exact(ctx_apps Fl Finj(en_uf e0)e0 tm les ns WS0 eq_refl k Nk Ok).

 }
 split;[exact(ws_tri _ _ _ _ _ _ WS0)|].
 split;[exact(ws_needs_ok Fl (en_uf e0) e0 tm les ns WS0 eq_refl)|].
 change(merge_edges e0=en_uf e0). unfold merge_edges;rewrite(tne_nil _ _ _ _ _ _ WS0);reflexivity.

Qed.
Theorem fun_env_context:forall g n names args res s e needs s',
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists C,A.FunCtx WW(gs_id g)C(map(key_tr Fl)needs)/\FunEnvTables e C.
Proof.
 intros g n names args res s e needs s' Wo Hg D H Hi Hw L.
 destruct(fun_env_context_valid g n names args res s e needs s' Wo Hg D H Hi Hw L)as(C&FC&Tables&Ti&Keys&Edges).
 exists C;auto.
Qed.
Corollary fun_env_forms:forall g n names args res s e needs s',
 A.WorldOf b WW->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 fun_env g s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists C,A.FunCtx WW(gs_id g)C(map(key_tr Fl)needs).
Proof.
 intros g n names args res s e needs s' Wo Hg D H Hi Hw L.
 destruct(fun_env_context g n names args res s e needs s' Wo Hg D H Hi Hw L)as(C&FC&Tables).
 exists C;exact FC.
Qed.
End Context.
