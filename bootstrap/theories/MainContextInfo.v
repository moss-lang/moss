(* The actual fn_info walk of main, including the platform root context. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve BodySound BodyTy BodyTr
 Merge CanonTypes CtxWalk CtxMerge CtxMethodExec RootContextExec FunctorMerge InfoCache MainRoot LoaderOrigin NeedKeys.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Open Scope list_scope.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements merged_walk].
Definition root_entry(e:A.entry):Prop:=match e with A.NTy _|A.NFun _ _=>True|_=>False end.
Lemma root_items_neutral:forall W q es0 tm its er tmr,
 A.RootItems W q es0 tm its er tmr->Forall root_entry er.
Proof.
 intros W q es0 tm its er tmr H;induction H.
 - constructor.
 - apply Forall_app;split.
   + apply Forall_forall;intros e Hin;apply in_map_iff in Hin as(x&<-&_);exact I.
   + constructor;[exact I|exact IHRootItems].
 - exact IHRootItems.
Qed.
Lemma root_ctx_neutral:forall W main es0 tm er tmr,
 A.RootCtx W main es0 tm er tmr->Forall root_entry er.
Proof. intros W main es0 tm er tmr H;destruct H;[constructor|eapply root_items_neutral;eassumption]. Qed.
Lemma unifies_append_root:forall sb es er,Forall root_entry er->
 A.Unifies sb(es++er)<->A.Unifies sb es.
Proof.
 intros sb es er Neutral;split.
 - intros H e1 e2 k H1 H2;apply H;apply in_or_app;left;assumption.
 - intros H e1 e2 k H1 H2 K1 K2.
   apply in_app_or in H1 as[H1|H1];apply in_app_or in H2 as[H2|H2].
   + exact(H e1 e2 k H1 H2 K1 K2).
   + pose proof(proj1(Forall_forall _ _)Neutral e2 H2)as R2.
     destruct e2;cbn[root_entry]in R2;try contradiction;[discriminate K2|].
     cbn[A.entry_key]in K2;injection K2 as <-.
     destruct e1;cbn[A.entry_key]in K1;try discriminate K1;reflexivity.
   + pose proof(proj1(Forall_forall _ _)Neutral e1 H1)as R1.
     destruct e1;cbn[root_entry]in R1;try contradiction;[discriminate K1|].
     cbn[A.entry_key]in K1;injection K1 as <-.
     destruct e2;cbn[A.entry_key]in K2;try discriminate K2;reflexivity.
   + pose proof(proj1(Forall_forall _ _)Neutral e1 H1)as R1.
     pose proof(proj1(Forall_forall _ _)Neutral e2 H2)as R2.
     destruct e1;cbn[root_entry]in R1;try contradiction;[discriminate K1|].
     destruct e2;cbn[root_entry]in R2;try contradiction;[discriminate K2|reflexivity].
Qed.
Section Main.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Notation WW:=(BodySound.W w R Fl).
Let MS:@MethStep w P R Fl:=meth_step_holds w R Fl b Hload.
Definition MainRd(g:gsym)(es:list A.entry)(tmr:list(nat*ty)):Prop:=
 exists aes atm er,A.ItemsEntries WW(gm_path(get_mod w(gs_mod g)))[][](map A.assume_spec(gs_assumes g))aes atm/\
 A.RootCtx WW(gs_id g)aes atm er tmr/\es=aes++er.
Lemma main_rd_unifies:forall g es tm es' tm' sb,
 MainRd g es tm->MainRd g es' tm'->A.Unifies sb es<->A.Unifies sb es'.
Proof.
 intros g es tm es' tm' sb(aes&atm&er&I&Rctx&->)(bes&btm&br&J&S&->).
 destruct(ies_shape_det WW _ _ _ _ _ _ _ _ I J)as[Shapes ->].
 rewrite(unifies_append_root sb aes er(root_ctx_neutral _ _ _ _ _ _ Rctx)),
  (unifies_append_root sb bes br(root_ctx_neutral _ _ _ _ _ _ S));eapply ies_unifies;eauto.
Qed.
Theorem main_round:forall g U s e needs s',
 nget(w_syms w)(gs_id g)=Some g->A.IsMainFn WW(gs_id g)->
 Tri U->UfOK U->Inv s->st_w s=w->Later w P R Fl s'->
 fi_walk g(env_with_edges[]U)s=Good((e,needs),s')->
 exists les tm,MainRd g(map(tr_ent Fl)les)tm/\WS Fl U e tm les needs.
Proof.
 intros g U s e needs s' Hg Main T O Hi Hw L H.
 assert(Nf:forall t,gs_origin g<>OForwarder t)by exact(loaded_no_forwarder b w g Hload Hg).
 pose proof(load_world_source b w Hload _ _ Hg)as Origin.
 unfold fi_walk in H;rewrite Origin in H.
 all: apply bind_inv in H as([e1 n1]&s1&Hass&H).
 all: assert(Oass:ok(mfold(fun acc it=>add_assume_item(fst acc)(gs_mod g)it(snd acc))(env_with_edges[]U,[])(gs_assumes g)))
  by(apply ok_mfold;intros;apply ok_add_assume_item).
 all: destruct(Oass _ _ _ Hi Hass)as[I1 F1].
 all: apply bind_inv in H as(main&s2&Emain&H).
 all: destruct(ok_is_main g _ _ _ I1 Emain)as[I2 F2].
 all: assert(W1:st_w s1=w)by(rewrite(fr_w _ _ F1);exact Hw).
 all: assert(TrueMain:main=true)by(apply(proj2(is_main_spec w R Fl b Hload g s1 main s2 Hg W1 Emain));exact Main).
 all: subst main.
 all: assert(Oroot:ok(add_root_context e1(gs_mod g)n1))by(unfold add_root_context;okt).
 all: destruct(Oroot _ _ _ I2 H)as[_ Froot].
 all: pose proof(later_back w P R Fl _ _(fr_trans _ _ _ F2 Froot)L)as L1.
 all: destruct(assumes_walk w(P:=P)R Fl b Hload MS(gs_mod g)(gs_assumes g)(env_with_edges[]U)[]s e1 n1 s1 U[][]
   Hass Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)L1(ws_start Fl U T O))as(al&aes&atm&IA&M&Wa).
 all: cbn[app]in Wa.
 all: destruct(add_root_context_exec w(P:=P)R Fl b Hload Hworld(gs_id g)e1(gs_mod g)n1 s2 e needs s' U atm al
   Main H I2 ltac:(rewrite(fr_w _ _ F2);exact W1)(loader_mod_in w R Fl b Hload _ _ Hg)L Wa)
   as(lr&er&tmr&Root&Mr&Wr).
 all: exists(al++lr),tmr;split.
 - exists aes,atm,er;split;[exact IA|split].
   + rewrite<-M;exact Root.
   + rewrite map_app,M,Mr;reflexivity.
 - exact Wr.
Qed.
Theorem main_merged:forall(Finj:forall a c,Fl a=Fl c->a=c)g s e needs s',
 nget(w_syms w)(gs_id g)=Some g->A.IsMainFn WW(gs_id g)->
 merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),s')->Inv s->st_w s=w->Later w P R Fl s'->
 exists les tm,MainRd g(map(tr_ent Fl)les)tm/\WS Fl(en_uf e)e tm les needs/\
 A.MostGeneralMerge(suf Fl(en_uf e))(map(tr_ent Fl)les)/\
 A.Needs(suf Fl(en_uf e))(map(tr_ent Fl)les)(map(key_tr Fl)needs).
Proof.
 intros Finj g s e needs s' Hg Main H Hi Hw L.
 apply(merged_walk_equiv_spec Fl(fi_walk g)Finj(ok_fi_walk g)(MainRd g)(main_rd_unifies g)
  (fun s=>st_w s=w)(Later w P R Fl)
  (fun s0 s1 F E=>fr_w' w s0 s1 F E)(fun s0 s1 F L1=>later_back w P R Fl s0 s1 F L1))
  with(mi:=gs_mod g)(s:=s)(s':=s');[|exact H|exact Hi|exact Hw|exact L].
 intros U s0 e0 n0 s0' T O I W L0 E.
 exact(main_round g U s0 e0 n0 s0' Hg Main T O I W L0 E).
Qed.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Definition MainInfo(g:gsym)(i:fninfo):Prop:=FnCtx w R Fl g i/\Tri(en_uf(fi_env i))/\Forall(fun k=>key_ok k=true)(fi_needs i).
Lemma main_walk_info:forall g s e needs s1,
 nget(w_syms w)(gs_id g)=Some g->A.IsMainFn WW(gs_id g)->
 merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),s1)->Inv s->st_w s=w->Later w P R Fl s1->
 MainInfo g(FInfo(freeze_all_apps e)needs
  (map(fun k=>(k,match get_app(freeze_all_apps e)k with Some(a,_)=>a|None=>[]end))needs)(merge_edges e)).
Proof.
 intros g s e needs s1 Hg Main H Hi Hw L.
 destruct(main_merged Finj g s e needs s1 Hg Main H Hi Hw L)as(les&tm&Rd&W&Mg&Nd).
 destruct Rd as(aes&atm&er&IA&Root&Es).
 set(U:=en_uf e)in *.
 split.
 - unfold FnCtx;exists(A.entries_cx(suf Fl U)tm(map(tr_ent Fl)les));cbn[fi_env fi_needs fi_merges].
   split;[|split;[reflexivity|split]].
   + exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),aes,atm,er,tm,(suf Fl U).
     split;[exact(w_decl_rd w R Fl _ _ Hg)|split;[exact IA|split;[exact Root|]]].
     rewrite<-Es;split;[exact Mg|split;[reflexivity|exact Nd]].
   + unfold merge_edges;rewrite(tne_nil _ _ _ _ _ _ W);reflexivity.
   + split;[|split;[|split;[|split;[|split]]]].
     * intro k;exact(ctx_ty Fl U e tm les needs W eq_refl k).
     * intro v;exact(ctx_vals Fl U e tm les needs W eq_refl v).
     * intro f;exact(ctx_fns Fl U e tm les needs W eq_refl f).
     * intros k m Ok Ck;exact(ctx_mths Fl Finj U e tm les needs W eq_refl k m Ok Ck).
     * intro u;exact(ctx_funpack Fl Finj U e tm les needs W eq_refl u).
     * split;[intros k Nk Ok;exact(ctx_apps Fl Finj U e tm les needs W eq_refl k Nk Ok)|].
       split;[exact(ws_method_unique _ _ _ _ _ _ W)|].
       split;[exact(ws_method_canonical _ _ _ _ _ _ W eq_refl)|].
       intros r m sg;exact(ctx_mths_domain Fl U e tm les needs W eq_refl r m sg).
 - split;[exact(ws_tri _ _ _ _ _ _ W)|exact(ws_needs_ok Fl U e tm les needs W eq_refl)].
Qed.
Theorem fn_info_main:forall g s i s',
 nget(w_syms w)(gs_id g)=Some g->A.IsMainFn WW(gs_id g)->
 fn_info g s=Good(i,s')->Inv s->InfoInv s->st_w s=w->Later w P R Fl s'->MainInfo g i.
Proof.
 intros g s i s' Hg Main H Hi I Hw L.
 assert(Hb:load_world b=Good(st_w s))by(rewrite Hw;exact Hload).
 assert(Gs:nget(st_syms s)(gs_id g)=Some g)by(apply(iv_loader _ Hi b Hb);rewrite Hw;exact Hg).
 destruct(nget(st_info s)(gs_id g))as[j|]eqn:Ej.
 - pose proof H as H0;unfold fn_info in H0;apply bind_inv in H0 as(x&s2&E&H0);injection E as <- <-.
   rewrite Ej in H0;injection H0 as <- <-.
   destruct(I _ _ Ej)as[(g'&s0&e&needs&s1&G0&Hi0&H1&->&F)|N].
   + destruct(ok_merged_fi g' s0 _ s1 Hi0 H1)as[_ F1].
     assert(W0:st_w s0=w)by(rewrite<-Hw,(fl_w _ _ F),(fr_w _ _ F1);reflexivity).
     assert(Eg:g'=g).
     {assert(Hb0:load_world b=Good(st_w s0))by(rewrite W0;exact Hload).
      pose proof(iv_loader _ Hi0 b Hb0(gs_id g)g ltac:(rewrite W0;exact Hg))as E0;congruence. }
     subst g';exact(main_walk_info g s0 e needs s1 Hg Main H1 Hi0 W0(later_fromL w R Fl _ _ F L)).
   + exfalso;pose proof(proj1 N b Hb)as N0;rewrite Hw in N0;congruence.
 - destruct(fn_info_miss g s i s' H Ej)as(g'&e&needs&s1&Eg&H1&->&->).
   rewrite Gs in Eg;injection Eg as <-.
   exact(main_walk_info g s e needs s1 Hg Main H1 Hi Hw(later_info w R Fl s1 _ L)).
Qed.
End Main.
