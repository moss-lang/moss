(* Every loaded declaration's input context is valid, including main's
   implicit platform-root entries. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 Merge CanonTypes CtxWalk CtxMerge CtxMethodExec ContextClean ContextSupport ContextWalkSupport
 ContextMergeSupport ContextFormationSupport NominalSupport InfoCache InfoPreserve
 LoaderOrigin MainRoot MainContextInfo RootContextSupport.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Loaded.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma loaded_context_round_valid:forall g U s e needs final,
 nget(w_syms w)(gs_id g)=Some g->
 Tri U->UfOK U->UfSupport WW U->Inv s->st_w s=w->Later w P R Fl final->
 fi_walk g(env_with_edges[]U)s=Good((e,needs),final)->
 ContextClean e/\Tri(en_uf e)/\EnvSupport WW e.
Proof.
 intros g U s e needs final Loaded TriU CleanU SupportU Inv Hw Late Whole.
 pose proof Whole as Run.
 pose proof(load_world_source b w Hload _ _ Loaded)as Origin.
 unfold fi_walk in Run;rewrite Origin in Run.
 apply bind_inv in Run as[[e1 needs1][s1[Assumes Run]]].
 assert(Oass:ok(mfold(fun acc it=>add_assume_item(fst acc)(gs_mod g)it(snd acc))
   (env_with_edges[]U,[])(gs_assumes g)))by(apply ok_mfold;intros;apply ok_add_assume_item).
 destruct(Oass _ _ _ Inv Assumes)as[Inv1 Frame1].
 apply bind_inv in Run as[main[s2[Main Run]]].
 destruct(ok_is_main g s1 main s2 Inv1 Main)as[Inv2 Frame2].
 assert(Orest:ok(if main then add_root_context e1(gs_mod g)needs1 else ret(e1,needs1)))
   by(destruct main;[unfold add_root_context;okt|apply ok_ret]).
 destruct(Orest _ _ _ Inv2 Run)as[_ Frame3].
 pose proof(later_back w P R Fl _ _(fr_trans _ _ _ Frame2 Frame3)Late)as Late1.
 assert(Mi:In(gs_mod g)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Loaded).
 destruct(assumes_walk w(P:=P)R Fl b Hload(meth_step_holds w R Fl b Hload)
   (gs_mod g)(gs_assumes g)(env_with_edges[]U)[]s e1 needs1 s1 U[][] Assumes Inv Hw Mi Late1
   (ws_start Fl U TriU CleanU))as(added&entries&tm&Items&Map&W1).
 cbn[List.app]in W1.
 destruct(support_edges_start WW U CleanU TriU SupportU)as[InitialClean[InitialTri InitialSupport]].
 pose proof(assume_items_context_support w(P:=P)R Fl b Hload(gs_mod g)(gs_assumes g)
   (env_with_edges[]U)[]s e1 needs1 s1 Assumes Inv Hw Mi InitialClean InitialTri InitialSupport Late1)as Support1.
 destruct main.
 - assert(TrueMain:A.IsMainFn WW(gs_id g)).
   {apply(proj1(is_main_spec w R Fl b Hload g s1 true s2 Loaded
     ltac:(rewrite(fr_w _ _ Frame1);exact Hw)Main));reflexivity. }
   destruct(main_round w(P:=P)R Fl b Hload Hworld g U s e needs final Loaded TrueMain
     TriU CleanU Inv Hw Late Whole)as(les&tm'&Rd&WF).
   split;[exact(ws_clean _ _ _ _ _ _ WF)|]. split;[exact(ws_tri _ _ _ _ _ _ WF)|].
   eapply add_root_context_support;[exact Hload|exact Hworld|exact Run|exact Inv2|
     rewrite(fr_w _ _ Frame2),(fr_w _ _ Frame1);exact Hw|exact Mi|exact Late|exact W1|exact(proj2(proj2 Support1))].
 - injection Run as <- <- <-;split;[exact(ws_clean _ _ _ _ _ _ W1)|].
   split;[exact(ws_tri _ _ _ _ _ _ W1)|exact(proj2(proj2 Support1))].
Qed.

Theorem loaded_merged_context_valid:forall g s e needs final,
 nget(w_syms w)(gs_id g)=Some g->
 merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),final)->
 Inv s->st_w s=w->Later w P R Fl final->
 ContextClean e/\Tri(en_uf e)/\EnvSupport WW e.
Proof.
 intros g s e needs final Loaded Run Inv Hw Late.
 apply(merged_walk_support w(P:=P)R Fl(fi_walk g))with(mi:=gs_mod g)(s:=s)(needs:=needs)(s':=final);
   try assumption.
 - intro input;unfold fi_walk;rewrite(load_world_source b w Hload _ _ Loaded).
   apply ok_bind;[apply ok_mfold;intros;apply ok_add_assume_item|].
   intros[e0 needs0];apply ok_bind;[apply ok_is_main|intro main;destruct main;
     [unfold add_root_context;okt|apply ok_ret]].
 - intros U s0 e0 needs0 sf TriU CleanU SupportU I W L E.
   exact(loaded_context_round_valid g U s0 e0 needs0 sf Loaded TriU CleanU SupportU I W L E).
Qed.

Theorem fn_info_valid_loaded:forall g s info final,
 nget(w_syms w)(gs_id g)=Some g->fn_info g s=Good(info,final)->
 Inv s->InfoInv s->st_w s=w->Later w P R Fl final->
 ContextClean(fi_env info)/\Tri(en_uf(fi_env info))/\EnvSupport WW(fi_env info).
Proof.
 intros g s info final Loaded Run Inv Cache Hw Late.
 assert(LoadHere:load_world b=Good(st_w s))by(rewrite Hw;exact Hload).
 assert(Present:nget(st_syms s)(gs_id g)=Some g)by(apply(iv_loader _ Inv b LoadHere);rewrite Hw;exact Loaded).
 destruct(nget(st_info s)(gs_id g))as[old|]eqn:Lookup.
 - pose proof Run as E;unfold fn_info in E;apply bind_inv in E as[snapshot[mid[Read E]]].
   injection Read as <- <-;rewrite Lookup in E;injection E as <- <-.
   destruct(Cache _ _ Lookup)as[(g0&s0&e&needs&s1&G0&I0&Walk&->&Frame)|[Missing _]].
   + destruct(ok_merged_fi g0 s0 _ s1 I0 Walk)as[_ Prefix].
     assert(W0:st_w s0=w)by(rewrite<-Hw,(fl_w _ _ Frame),(fr_w _ _ Prefix);reflexivity).
     assert(Same:g0=g).
     {assert(L0:load_world b=Good(st_w s0))by(rewrite W0;exact Hload).
      pose proof(iv_loader _ I0 b L0(gs_id g)g ltac:(rewrite W0;exact Loaded))as At;congruence. }
     subst g0;destruct(loaded_merged_context_valid g s0 e needs s1 Loaded Walk I0 W0
       (later_fromL w R Fl _ _ Frame Late))as[C[T S]].
     exact(freeze_all_apps_valid WW e C T S).
   + exfalso;specialize(Missing b LoadHere);rewrite Hw,Loaded in Missing;discriminate.
 - destruct(fn_info_miss g s info final Run Lookup)as[g0[e[needs[s1[Sym[Walk[-> ->]]]]]]].
   rewrite Present in Sym;injection Sym as <-.
   destruct(loaded_merged_context_valid g s e needs s1 Loaded Walk Inv Hw(later_info w R Fl _ _ Late))as[C[T S]].
   exact(freeze_all_apps_valid WW e C T S).
Qed.
End Loaded.
