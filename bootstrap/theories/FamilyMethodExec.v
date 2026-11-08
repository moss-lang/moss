(* Canonical semantic readings of the actual family method output setter. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 Merge CanonTypes CanonBodyTy CtxWalk TyOfSubst SigReadingSubst ProvideResultMethod ProvideResultFunction ProvideResultReading ProvideResultCanonical ProvideResultSubst ProvideResultContext.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[fn_sig provided_at symbol_type app_subst ety_of FUEL].
Lemma provide_spec_method_phases:forall f e fg mi pa dn a outer out s e' keys s',
 provide_spec(S f)e fg mi(Spec(Some pa)(Some dn)a)outer out s=Good((e',keys),s')->
 exists sub sx recv sr,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sx)/\
 resolve_type_symbol mi pa sx=Good(recv,sr)/\
 provide_method_code e mi recv dn(sub++tymap e)out sr=Good((e',keys),s').
Proof.
 intros f e fg mi pa dn a outer out s e' keys s' E;cbn[provide_spec]in E.
 apply bind_inv in E as(sub&sx&Sub&E).
 apply bind_inv in E as(recv&sr&Resolve&E).
 exists sub,sx,recv,sr;split;[exact Sub|];split;[exact Resolve|].
 apply bind_inv in E as(mo&sm&Find&E);destruct mo as[m|];[|exfalso;eapply err_not_good;exact E].
 unfold provide_method_code,mbind;rewrite Find;exact E.
Qed.
#[local]Hint Resolve output_provided_ok output_signature_ok:okdb.
Section Method.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma output_receiver_resolve:forall mi pa s recv sf,
 resolve_type_symbol mi pa s=Good(recv,sf)->Inv s->st_w s=w->In mi(w_order w)->
 sf=s/\A.resolve_sym W(gm_path(get_mod w mi))pa=Some(gs_id recv)/\
 nget(w_syms w)(gs_id recv)=Some recv.
Proof.
 intros mi pa s recv sf H Hi Hw Hmi.
 unfold resolve_type_symbol in H;apply bind_inv in H as(r&s1&Er&H).
 destruct(rpath_rel w R Fl b Hload mi pa s r s1 Hw Hmi Er)as[-> Res].
 destruct r as[[x|j]|];try(exfalso;eapply err_not_good;exact H).
 apply bind_inv in H as(g&s2&Eg&H).
 destruct(sym_loader w R Fl b Hload _ _ x s g s2 Hi Hw Res Eg)as[-> Hg].
 assert(Hg':nget(w_syms w)(gs_id g)=Some g).
 {rewrite(load_world_ids b w Hload x g Hg);exact Hg. }
 destruct(gs_kind g);try(exfalso;eapply err_not_good;exact H).
 all:injection H as <- <-.
 all:split;[reflexivity|].
 all:split.
 all:try exact Hg'.
 all:unfold A.resolve_sym;change(A.resolve(W_read w R Fl))with(A.resolve W);rewrite Res;
 cbn[option_map ent_of];rewrite(load_world_ids b w Hload x g Hg);reflexivity.
Qed.
Lemma family_output_brackets:forall e mi a outer ob tm s sub sf,
 (match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end)s=Good(sub,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->Tri(en_uf e)->
 TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e->
 map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 Forall(fun q=>WFty(snd q))outer->
 (forall k,A.nlook k(ob++tm)=option_map(fun y=>tr Fl(canon e y))(nassoc k(outer++tymap e)))->
 exists bs,map(fun q=>(fst q,tr Fl(canon e(snd q))))sub=bs++ob/\
 Forall(fun q=>WFty(snd q))sub/\A.OutputBrackets W(gm_path(get_mod w mi))(ob++tm)a bs.
Proof.
 intros e mi a outer ob tm s sub sf E Hi Hw Hmi L T TR Map Wo M.
 destruct a as[bs|].
 - apply bind_inv in E as(xs&sx&Read&E);injection E as <- <-.
   destruct(output_brackets_canonical w R Fl b Hload e mi bs outer s xs sx(ob++tm)
    Read Hi Hw Hmi L T TR M)as[B Wxs].
   exists(map(fun q=>(fst q,tr Fl(canon e(snd q))))xs);split.
   + rewrite map_app,Map;reflexivity.
   + split;[apply Forall_app;auto|exact B].
 - injection E as <- <-;exists[];split;[exact Map|split;[exact Wo|constructor]].
Qed.
Theorem provide_method_canonical:forall e mi recv dn tm tmr out s e' keys s',
 provide_method_code e mi recv dn tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->nget(w_syms w)(gs_id recv)=Some recv->
 Later w P R Fl s'->Tri(en_uf e)->TmRel Fl tm(map(trp Fl)tm)e->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 exists m this sg a stamp,
 A.MethodNamed W(gm_path(get_mod w mi))(gs_id recv)dn(gs_id m)/\
 A.RecvTy W(gs_id recv)tmr(tr Fl(canon e this))/\WFty this/\
 A.SigOf W(gs_id m)tmr(Some(tr Fl(canon e this)))(output_sig Fl e sg)/\
 A.AppOf W(gs_id m)tmr(output_inst Fl(freeze_app e a))/\
 e'=set_app(set_mth e(canon e this)(gs_id m)sg)
 (KyMth(canon e this)(gs_id m))(freeze_app(set_mth e(canon e this)(gs_id m)sg)a)stamp/\
 keys=out++[KyMth(canon e this)(gs_id m)].
Proof.
 intros e mi recv dn tm tmr out s e' keys s' E Hi Hw Hmi Hr L T TR M.
 destruct(provide_method_read w R Fl b Hload e mi recv dn tm(map(trp Fl)tm)out s e' keys s'
 E Hi Hw Hmi Hr L TR)as(m&this&sg&a&stamp&Named&Recv&Wthis&Sig&App&Env&Keys).
 pose proof(output_subrel Fl e tm tmr T M)as SR.
 pose proof(output_receiver_subst W(suf Fl(en_uf e))_ _ _ Recv tmr SR)as Rc.
 rewrite tr_canon in Rc by exact T.
 pose proof(sigof_subst W(suf Fl(en_uf e))_ _ _ _ Sig tmr SR)as Sg.
 cbn[option_map]in Sg;rewrite tr_canon in Sg by exact T.
 assert(SigEq:A.FSig(map(ExecutionModel.app_subst(suf Fl(en_uf e)))(A.sg_params(tr_sig Fl sg)))
 (ExecutionModel.app_subst(suf Fl(en_uf e))(A.sg_ret(tr_sig Fl sg)))=output_sig Fl e sg).
 {unfold output_sig,tr_sig;cbn[A.sg_params A.sg_ret];f_equal.
  - rewrite map_map;apply map_ext;intro p;apply tr_canon;exact T.
  - apply tr_canon;exact T. }
 rewrite SigEq in Sg.
 pose proof(output_appof_subst W(suf Fl(en_uf e))_ _ _ App tmr SR)as Ap.
 rewrite output_freeze_translation in Ap by exact T.
 exists m,this,sg,a,stamp;repeat split;assumption.
Qed.
Lemma canonical_method_reading:forall e tm tmr recv m this sg app,
 Tri(en_uf e)->(forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.RecvTy W recv(map(trp Fl)tm)(tr Fl this)->
 A.SigOf W m(map(trp Fl)tm)(Some(tr Fl this))(tr_sig Fl sg)->
 A.AppOf W m(map(trp Fl)tm)(output_inst Fl app)->
 A.RecvTy W recv tmr(tr Fl(canon e this))/\
 A.SigOf W m tmr(Some(tr Fl(canon e this)))(output_sig Fl e sg)/\
 A.AppOf W m tmr(output_inst Fl(freeze_app e app)).
Proof.
 intros e tm tmr recv m this sg app T M Recv Sig App.
 assert(SR:SubRel(suf Fl(en_uf e))(map(trp Fl)tm)tmr).
 {intro k;rewrite nlook_trp,M;destruct(nassoc k tm);cbn[option_map];[rewrite tr_canon by exact T|];reflexivity. }
 pose proof(output_receiver_subst W(suf Fl(en_uf e))_ _ _ Recv tmr SR)as Rc.
 rewrite tr_canon in Rc by exact T.
 pose proof(sigof_subst W(suf Fl(en_uf e))_ _ _ _ Sig tmr SR)as Sg.
 cbn[option_map]in Sg;rewrite tr_canon in Sg by exact T.
 assert(SigEq:A.FSig(map(ExecutionModel.app_subst(suf Fl(en_uf e)))(A.sg_params(tr_sig Fl sg)))
 (ExecutionModel.app_subst(suf Fl(en_uf e))(A.sg_ret(tr_sig Fl sg)))=output_sig Fl e sg).
 {unfold output_sig,tr_sig;cbn[A.sg_params A.sg_ret];f_equal.
  - rewrite map_map;apply map_ext;intro p;apply tr_canon;exact T.
  - apply tr_canon;exact T. }
 rewrite SigEq in Sg.
 pose proof(output_appof_subst W(suf Fl(en_uf e))_ _ _ App tmr SR)as Ap.
 rewrite output_freeze_translation in Ap by exact T;repeat split;assumption.
Qed.
Lemma family_canon_lookup:forall e xs k,
 A.nlook k(map(fun q=>(fst q,tr Fl(canon e(snd q))))xs)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k xs).
Proof.
 intros e xs;induction xs as[|[j y]xs IH];intro k;[reflexivity|].
 cbn[A.nlook map fst snd nassoc assoc];destruct(Nat.eqb k j);[reflexivity|apply IH].
Qed.
Theorem provide_family_method:forall c e F C f fg mi pa dn a outer ob out s e' keys s',
 provide_spec(S f)e fg mi(Spec(Some pa)(Some dn)a)outer out s=Good((e',keys),s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))outer->map(fun q=>(fst q,tr Fl(canon e(snd q))))outer=ob->
 exists m this sg app stamp,
 A.OutputItemEntries W(gm_path(get_mod w mi))ob(A.c_ty C)(Spec(Some pa)(Some dn)a)
 [A.NMth(tr Fl(canon e this))(gs_id m)(output_sig Fl e sg)(output_inst Fl(freeze_app e app))](A.c_ty C)/\
 e'=set_app(set_mth e(canon e this)(gs_id m)sg)
 (KyMth(canon e this)(gs_id m))(freeze_app(set_mth e(canon e this)(gs_id m)sg)app)stamp/\
 keys=out++[KyMth(canon e this)(gs_id m)].
Proof.
 intros c e F C f fg mi pa dn a outer ob out s e' keys s' E Hi Hw Hmi L HC Wo O.
 destruct(provide_spec_method_phases _ _ _ _ _ _ _ _ _ _ _ _ _ E)
 as(sub&sx&recv&sr&Sub&Resolve&Method).
 assert(Os:ok(match a with Some bs=>let!xs:=app_subst 200 e mi bs outer in ret(xs++outer)|None=>ret outer end))by okt.
 destruct(Os _ _ _ Hi Sub)as[Ix Fx].
 assert(Wx:st_w sx=w)by(rewrite(fr_w _ _ Fx);exact Hw).
 destruct(output_receiver_resolve _ _ _ _ _ Resolve Ix Wx Hmi)as[->[Res Hr]].
 assert(Om:ok(provide_method_code e mi recv dn(sub++tymap e)out)).
 {unfold provide_method_code;okt. }
 destruct(Om _ _ _ Ix Method)as[_ Fm].
 pose proof(later_back w P R Fl _ _ Fm L)as Lx.
 assert(TR0:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 {apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ HC)|intro k;rewrite nlook_trp,nassoc_tymap;reflexivity]. }
 assert(TR:TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wo]. }
 assert(M:forall k,A.nlook k(ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(outer++tymap e))).
 {intro k;rewrite <-O,nlook_app,family_canon_lookup,(cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
  destruct(nassoc k outer);reflexivity. }
 destruct(family_output_brackets e mi a outer ob(A.c_ty C)s sub sx Sub Hi Hw Hmi Lx
  (proj1(cr_uf _ _ _ _ _ _ _ HC))TR O Wo M)as(bs&Bmap&Wsub&Br).
 assert(TRs:TmRel Fl(sub++tymap e)(map(trp Fl)(sub++tymap e))e).
 {rewrite map_app;apply tmrel_app;[exact TR0|reflexivity|exact Wsub]. }
 assert(Ms:forall k,A.nlook k(bs++ob++A.c_ty C)=
 option_map(fun y=>tr Fl(canon e y))(nassoc k(sub++tymap e))).
 {intro k;rewrite app_assoc,<-Bmap,nlook_app,family_canon_lookup,
 (cr_ty _ _ _ _ _ _ _ HC),nassoc_app,nassoc_tymap.
 destruct(nassoc k sub);reflexivity. }
 destruct(provide_method_canonical e mi recv dn(sub++tymap e)(bs++ob++A.c_ty C)out sx e' keys s'
 Method Ix Wx Hmi Hr L(proj1(cr_uf _ _ _ _ _ _ _ HC))TRs Ms)
 as(m&this&sg&app&stamp&Named&Recv&Wthis&Sig&App&Env&Keys).
 exists m,this,sg,app,stamp;split;[|split;assumption].
 eapply A.OIEMeth;[exact Res|exact Named|exact Br|exact Recv|exact Sig|exact App].
Qed.
End Method.
