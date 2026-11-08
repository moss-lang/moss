(* Cached and newly computed forwarder contexts retain the actual identity
   application and a declaration signature read at requirement names. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr CtxWalk CtxMerge
 InfoInvariant InfoPreserve InfoCache FnInfoCore ForwarderPrototypeSeed ForwarderPrototypeWalk FnSnapshotAlign.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Definition ForwarderPrototype(W:A.world)(t:nat)(info:fninfo):Prop:=
 (exists sg,A.SigOf W t(requirement_snapshot(A.w_reqs W t))None sg)/\
 fi_needs info=[KyFn t]/\
 fi_apps info=[(KyFn t,map(fun r=>(r,Some(YAbs r)))(A.w_reqs W t))]/\fi_merges info=[].
Lemma freeze_requirement_application:forall e rs,en_uf e=[]->
 fzE e(map(fun r=>(r,Some(YAbs r)))rs)=map(fun r=>(r,Some(YAbs r)))rs.
Proof.
 intros e rs Uf;unfold fzE,freeze_app;rewrite !map_map;apply map_ext;intro r.
 cbn[fst snd option_map];unfold canon;rewrite Uf;reflexivity.
Qed.
Section Prototype.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma forwarder_walk_info:forall g t target s e needs final n ps rt body,
 gs_origin g=OForwarder t->nget(w_syms w)t=Some target->gs_kind target=KFn->
 gs_decl target=ADFn RPlain n ps rt body->ReqTypes w R t->Inv s->st_w s=w->Later w P R Fl final->
 merged_walk(gs_mod g)(fi_walk g)s=Good((e,needs),final)->
 ForwarderPrototype WW t(FInfo(freeze_all_apps e)needs
 (map(fun k=>(k,match get_app(freeze_all_apps e)k with Some(a,_)=>a|None=>[]end))needs)(merge_edges e)).
Proof.
 intros g t target s e needs final n ps rt body Origin Loaded Kind Decl Types Hi Hw L Run.
 destruct(forwarder_merged_walk w (P:=P) R Fl b Hload g t target s e needs final n ps rt body
 Origin Loaded Kind Decl Types Hi Hw L Run)as[seed[sg[stamp[Seed[-> [Sig ->]]]]]].
 unfold ForwarderPrototype;cbn[A.w_reqs BodySoundCore.W W_read fi_needs fi_apps fi_merges].
 split;[exists(tr_sig Fl sg);exact Sig|]. split;[reflexivity|]. split.
 - cbn[map];rewrite get_app_freeze.
   unfold get_app;cbn[set_app set_fn en_apps];rewrite(seed_apps _ _ Seed).
   cbn[aset assoc fst snd];rewrite key_eqb_refl;cbn[option_map fst snd].
   rewrite freeze_requirement_application;[reflexivity|exact(seed_uf _ _ Seed)].
 - change(filter(fun p=>negb(ety_eqb(snd p)(YAbs(fst p))))(tymap seed)++en_uf seed=[]).
   rewrite(seed_no_type_edges _ _ Seed),(seed_uf _ _ Seed);reflexivity.
Qed.
Theorem fn_info_forwarder_prototype:forall g0 g t target s info final n ps rt body,
 nget(st_syms s)(gs_id g0)=Some g->gs_origin g=OForwarder t->
 nget(w_syms w)t=Some target->gs_kind target=KFn->
 gs_decl target=ADFn RPlain n ps rt body->ReqTypes w R t->
 fn_info g0 s=Good(info,final)->Inv s->InfoInv s->st_w s=w->Later w P R Fl final->
 ForwarderPrototype WW t info.
Proof.
 intros g0 g t target s info final n ps rt body Gs Origin Loaded Kind Decl Types Run Hi Ii Hw L.
 destruct(nget(st_info s)(gs_id g0))as[cached|]eqn:Cached.
 - pose proof Run as Read;unfold fn_info in Read.
   apply bind_inv in Read as[x[s2[Get Read]]];injection Get as <- <-.
   rewrite Cached in Read;injection Read as <- <-.
   destruct(Ii _ _ Cached)as[(g'&s0&e&needs&s1&Lookup&I0&Walk&->&Frame)|[_[g'[Lookup NotForward]]]].
   + destruct(ok_merged_fi g' _ _ _ I0 Walk)as[I1 F1].
     assert(Same:g'=g)by(pose proof(fl_keep _ _ Frame _ _ (fr_keep _ _ F1 _ _ Lookup));congruence).
     subst g'. eapply forwarder_walk_info;[exact Origin|exact Loaded|exact Kind|exact Decl|exact Types|exact I0| | |exact Walk].
     * rewrite <-(fr_w _ _ F1),<-(fl_w _ _ Frame);exact Hw.
     * exact(later_fromL w R Fl _ _ Frame L).
   + rewrite Gs in Lookup;injection Lookup as <-;exfalso;exact(NotForward t Origin).
 - destruct(fn_info_miss g0 s info final Run Cached)as[g'[e[needs[s1[Lookup[Walk[-> ->]]]]]]].
   assert(Same:g'=g)by congruence;subst g'.
   eapply forwarder_walk_info;[exact Origin|exact Loaded|exact Kind|exact Decl|exact Types|exact Hi|exact Hw| |exact Walk].
   exact(later_info w R Fl s1 _ L).
Qed.
End Prototype.
