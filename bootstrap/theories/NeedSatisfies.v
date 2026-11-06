(* The actual need checker and satisfaction-map emitter establish Satisfies. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound BodyTr Merge CtxWalk CtxMerge
 CanonTypes InstantiateTr NeedExec NeedMapExec SmapExec InfoShape.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Execution.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Lemma need_satisfies_tables : forall mi e callee info s ps sn sm s' c F C Cs,
 require_needs mi e callee info s=Good(ps,sn)->
 smap_at(fi_env info)e ps sn=Good(sm,s')->Inv s->Later w P R Fl s'->
 CxRel w R Fl c e F C ->
 A.c_sub Cs=suf Fl(en_uf(fi_env info))->fi_merges info=en_uf(fi_env info)->
 Tri(en_uf(fi_env info))->AppsShape info->Forall(fun k=>key_ok k=true)(fi_needs info)->
 (forall k,nofun k=true->key_ok k=true->
 A.ilook(key_tr Fl k)(A.c_apps Cs)=option_map(fun p=>tr_inst Fl(fst p))(get_app(fi_env info)k))->
 A.Satisfies C Cs(map(key_tr Fl)(fi_needs info))sm.
Proof.
 intros mi e callee info s ps sn sm s' c F C Cs En Es Hi L Hc Sub Merges Ti Shape Keys Apps.
 destruct(require_needs_all _ _ _ _ _ _ _ Hi En)as[Pairs[Present Coherent]].
 assert(On:ok(require_needs mi e callee info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Hi En)as[In Fn].
 assert(MapView:Forall(fun p=>key_apps Fl(fi_env info)Cs(fst p)/\key_apps Fl e C(snd p))ps).
 { rewrite Pairs. apply Forall_forall;intros p Hp. apply in_map_iff in Hp as[k[<- Hk]].
   split;cbn[fst snd].
   - destruct k;try exact I. exact(Apps(KyMth e0 n)eq_refl(proj1(Forall_forall _ _)Keys _ Hk)).
   - exact(caller_key_apps w R Fl b Hload c e F C k Hc(proj1(Forall_forall _ _)Keys _ Hk)). }
 pose proof(smap_at_context w R Fl _ _ _ _ _ _ _ _ Es In L Sub Ti
   (cr_sub _ _ _ _ _ _ _ Hc)(proj1(cr_uf _ _ _ _ _ _ _ Hc))MapView)as MapEq.
 unfold A.Satisfies. split.
 - rewrite MapEq,Pairs,!map_map. apply map_ext. intro k;cbn[fst snd].
   rewrite(key_tr_site w R Fl c e F C k Hc);reflexivity.
 - split.
   + intros k Hk. apply in_map_iff in Hk as[need[<- Hneed]].
     exact(key_present_site_provides w R Fl b Hload c e F C need Hc
       (proj1(Forall_forall _ _)Keys _ Hneed)(proj1(Forall_forall _ _)Present _ Hneed)).
   + split.
     * intros k a Hk Ha t want Hwant. apply in_map_iff in Hk as[need[<- Hneed]].
       pose proof(proj1(Forall_forall _ _)Keys _ Hneed)as Ok.
       destruct need as[v|f|r m|u].

       all:match type of Ok with key_ok ?k=true=>rewrite(Apps k eq_refl Ok)in Ha end.
       all:match type of Ha with context[get_app _ ?k]=>destruct(get_app(fi_env info)k)as[[raw stamp]|]eqn:Raw;[|discriminate Ha]end.
       all:injection Ha as <-.
       all:rewrite <-(key_tr_site w R Fl c e F C _ Hc).
       all:pose proof(caller_key_apps w R Fl b Hload c e F C _ Hc Ok)as Caller.
       all:cbn[site_key key_tr]in Caller|-*.
       all:try rewrite(cr_vapps _ _ _ _ _ _ _ Hc).
       all:try rewrite(cr_fapps _ _ _ _ _ _ _ Hc).
       all:try rewrite(cr_funapps _ _ _ _ _ _ _ Hc).
       all:try(unfold A.mkey in Caller;rewrite Caller).
       all:match goal with |-context[get_app ?en ?k]=>destruct(get_app en k)as[[have hs]|]eqn:Have;[|exact I]end.
       all:cbn[option_map fst].
       all:eapply incoherent_semantic;[exact Hc| |exact Have|].
       all:try exact(proj1(Forall_forall _ _)Coherent _ Hneed).
       all:rewrite(fn_info_app_lookup _ _ Shape Hneed),Raw;exact Hwant.
     * unfold require_needs in En. apply bind_inv in En as[u[su[Eu _]]].
       eapply require_merges_semantic;[exact Eu|exact Hc|]. rewrite Merges;exact Sub.
Qed.
Lemma need_satisfies : forall mi e callee info s ps sn sm s' c F C Cs,
 require_needs mi e callee info s=Good(ps,sn)->
 smap_at(fi_env info)e ps sn=Good(sm,s')->Inv s->Later w P R Fl s'->
 CxRel w R Fl c e F C ->
 CtxOfIE(BodySound.W w R Fl)(gs_id callee)Cs(map(key_tr Fl)(fi_needs info))->
 A.c_sub Cs=suf Fl(en_uf(fi_env info))->fi_merges info=en_uf(fi_env info)->
 Tri(en_uf(fi_env info))->AppsShape info->Forall(fun k=>key_ok k=true)(fi_needs info)->
 (forall k,nofun k=true->key_ok k=true->
 A.ilook(key_tr Fl k)(A.c_apps Cs)=option_map(fun p=>tr_inst Fl(fst p))(get_app(fi_env info)k))->
 A.Satisfies C Cs(map(key_tr Fl)(fi_needs info))sm.
Proof.
 intros mi e callee info s ps sn sm s' c F C Cs En Es Hi L Hc Ctx Sub Merges Ti Shape Keys Apps.
 eapply need_satisfies_tables; eauto.
Qed.
End Execution.
