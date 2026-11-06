(* Output readers use the completed, canonical meaning table.  In particular,
   the model records the same frozen instantiation as actual provided_at. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound
 BodyTy Merge CanonTypes TyOfSubst ProvideResultReading ProvideResultSubst.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma output_subrel:forall Fl e tm tmr,
 Tri(en_uf e)->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 SubRel(suf Fl(en_uf e))(map(trp Fl)tm)tmr.
Proof.
 intros Fl e tm tmr T R k;rewrite R,nlook_trp.
 destruct(nassoc k tm);cbn[option_map];[rewrite tr_canon by exact T|];reflexivity.
Qed.
Lemma output_freeze_translation:forall Fl e a,
 Tri(en_uf e)->
 output_app_subst(suf Fl(en_uf e))(output_inst Fl a)=output_inst Fl(freeze_app e a).
Proof.
 intros Fl e a T;unfold output_app_subst,output_inst,freeze_app;rewrite !map_map.
 apply map_ext;intros[x[y|]];cbn[fst snd option_map];[rewrite tr_canon by exact T|];reflexivity.
Qed.
Section Read.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Local Opaque reqs type_requirements ety_of provision_app.
Theorem output_type_canonical:forall t mi tm e this s d s' tmr,
 ety_of t mi tm e this s=Good(d,s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 Tri(en_uf e)->TmRel Fl tm(map(trp Fl)tm)e->opt_wf this->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.TyOf WW(gm_path(get_mod w mi))tmr(option_map(fun y=>tr Fl(canon e y))this)t(tr Fl(canon e d)) /\WFty d.
Proof.
 intros t mi tm e this s d s' tmr E Hi Hw Hmi L T TR Wth M.
 rewrite ety_of_eq in E.
 destruct(proj1(ty_all w R Fl b Hload 200)t mi tm e this s d s'(map(trp Fl)tm)
   E Hi Hw Hmi TR Wth L)as[Read Wd];split;[|exact Wd].
 pose proof(tyof_subst WW(suf Fl(en_uf e))_ _ _ _ _ Read tmr(output_subrel Fl e tm tmr T M))as H.
 rewrite tr_canon in H by exact T.
 assert(Th:option_map(Audit.app_subst(suf Fl(en_uf e)))(option_map(tr Fl)this)=
   option_map(fun y=>tr Fl(canon e y))this).
 {destruct this;cbn[option_map];[rewrite tr_canon by exact T|];reflexivity. }
 rewrite Th in H;exact H.
Qed.
Theorem output_provision_canonical:forall g tm tmr e s a s',
 provision_app g tm false s=Good(a,s')->Inv s->Later w P R Fl s'->
 Tri(en_uf e)->TmRel Fl tm(map(trp Fl)tm)e->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.kind_sym WW(gs_id g)<>Some A.DKMeth ->
 A.AppOf WW(gs_id g)tmr(output_inst Fl(freeze_app e a)).
Proof.
 intros g tm tmr e s a s' E Hi L T TR M K.
 pose proof(ordinary_provision_rel w R Fl g tm(map(trp Fl)tm)e s a s' E Hi L TR K)as Raw.
 pose proof(output_appof_subst WW(suf Fl(en_uf e))_ _ _ Raw tmr(output_subrel Fl e tm tmr T M))as H.
 rewrite output_freeze_translation in H by exact T;exact H.
Qed.
End Read.

Lemma output_meaning_translation:forall Fl e xs,
 Tri(en_uf e)->
 smap(suf Fl(en_uf e))(map(trp Fl)xs)=
 map(fun q=>(fst q,tr Fl(canon e(snd q))))xs.
Proof.
 intros Fl e xs T;unfold smap,trp;rewrite map_map;apply map_ext.
 intros[x y];cbn[fst snd];rewrite tr_canon by exact T;reflexivity.
Qed.
Section Brackets.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Theorem output_brackets_canonical:forall e mi bs outer s xs s' tmr,
 app_subst 200 e mi bs outer s=Good(xs,s')->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl s'->
 Tri(en_uf e)->TmRel Fl(outer++tymap e)(map(trp Fl)(outer++tymap e))e->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k(outer++tymap e)))->
 A.OutputBrackets WW(gm_path(get_mod w mi))tmr(Some bs)
 (map(fun q=>(fst q,tr Fl(canon e(snd q))))xs) /\
 Forall(fun q=>WFty(snd q))xs.
Proof.
 intros e mi bs outer s xs s' tmr E Hi Hw Hmi L T TR M.
 destruct(proj1(proj2(ty_all w R Fl b Hload 200))e mi bs outer s xs s'
   (map(trp Fl)(outer++tymap e))E Hi Hw Hmi TR L)as(lr&Rel&Eq&Wx).
 split;[|exact Wx].
 assert(B:A.OutputBrackets WW(gm_path(get_mod w mi))
   (map(trp Fl)(outer++tymap e))(Some bs)(rev lr)).
 {apply A.OBrSome;exact Rel. }
 pose proof(output_brackets_subst WW(suf Fl(en_uf e))_ _ _ _ B tmr
   (output_subrel Fl e _ _ T M))as Norm.
 rewrite <-Eq,output_meaning_translation in Norm by exact T;exact Norm.
Qed.
End Brackets.

From NM Require Import CtxWalk SigSound SigReadingSubst.
Section Signature.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Theorem output_signature_canonical:forall g tm tmr e this s sg s' rcv n ps rt body,
 fn_sig g tm e this s=Good(sg,s')->Inv s->st_w s=w->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn rcv n ps rt body->
 Later w P R Fl s'->Tri(en_uf e)->TmRel Fl tm(map(trp Fl)tm)e->opt_wf this->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 A.SigOf WW(gs_id g)tmr(option_map(fun y=>tr Fl(canon e y))this)(A.FSig(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))(tr Fl(canon e(sg_ret sg)))).
Proof.
 intros g tm tmr e this s sg s' rcv n ps rt body E Hi Hw Hg D L T TR Wth M.
 destruct(fn_sig_rel_at w R Fl b Hload g tm(map(trp Fl)tm)e this s sg s' rcv n ps rt body
   E Hi Hw Hg D TR Wth L)as[Raw This].
 pose proof(sigof_subst WW(suf Fl(en_uf e))_ _ _ _ Raw tmr(output_subrel Fl e tm tmr T M))as Norm.
 assert(Sig:A.FSig(map(Audit.app_subst(suf Fl(en_uf e)))(A.sg_params(tr_sig Fl sg)))
   (Audit.app_subst(suf Fl(en_uf e))(A.sg_ret(tr_sig Fl sg)))=A.FSig(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))(tr Fl(canon e(sg_ret sg)))).
 {unfold tr_sig;cbn[A.sg_params A.sg_ret];f_equal.
  - rewrite map_map;apply map_ext;intro p;apply tr_canon;exact T.
  - apply tr_canon;exact T. }
 rewrite Sig in Norm.
 assert(Th:option_map(Audit.app_subst(suf Fl(en_uf e)))(option_map(tr Fl)this)=
   option_map(fun y=>tr Fl(canon e y))this).
 {destruct this;cbn[option_map];[rewrite tr_canon by exact T|];reflexivity. }
 rewrite Th in Norm;exact Norm.
Qed.
End Signature.
