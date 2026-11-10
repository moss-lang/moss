(* Checked provider signature matching uses the actual canonical readings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore
 BodyTy SigSound TyOfSubst CanonTypes CanonBodyTy Merge RuleBKeep.
From NM Require Export SigReadingSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Definition canonical_sig(Fl:string->nat)(e:env)(sg:Elab.fsig):A.fsig:=
 A.FSig(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))(tr Fl(canon e(sg_ret sg))).
Lemma sig_eqb_in_canonical:forall Fl e1 a e2 b,
 sig_eqb_in e1 a e2 b=true ->canonical_sig Fl e1 a=canonical_sig Fl e2 b.
Proof.
 intros Fl e1[pa ra ta]e2[pb rb tb]E. unfold sig_eqb_in in E. cbn[sg_params sg_ret]in E.
 apply andb_true_iff in E as[Eab Er]. apply andb_true_iff in Eab as[Len Pars].
 apply Nat.eqb_eq in Len. apply ety_eqb_true in Er.
 unfold canonical_sig. cbn[sg_params sg_ret]. f_equal;[|rewrite Er;reflexivity].
 clear -Len Pars. revert pb Len Pars. induction pa as[|[na a]pa IH];intros[|[nb b]pb]Len Pars;
 cbn[length map combine]in *;try discriminate;[reflexivity|].
 apply andb_true_iff in Pars as[Hab Rest]. apply ety_eqb_true in Hab. cbn[fst snd]in Hab.
 cbn[fst snd]. f_equal;[rewrite Hab;reflexivity|]. apply IH;[injection Len;auto|exact Rest].
Qed.


Section Canonical.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Lemma fn_sig_canonical:forall c e F C g this s sg s' recv n ps rt body,
 fn_sig g(tymap e)e this s=Good(sg,s') ->Inv s->st_w s=w ->
 CxRel w R Fl c e F C ->nget(w_syms w)(gs_id g)=Some g ->
 gs_decl g=ADFn recv n ps rt body ->opt_wf this ->Later w P R Fl s' ->
 A.SigOf WW(gs_id g)(A.c_ty C)(option_map(fun y=>tr Fl(canon e y))this)(canonical_sig Fl e sg).
Proof.
 intros c e F C g this s sg s' recv n ps rt body E Hi Hw Hc Hg D Wthis L.
 assert(TR:TmRel Fl(tymap e)(map(trp Fl)(tymap e))e).
 { apply tmrel_tymap;[exact(cr_ty_wf _ _ _ _ _ _ _ Hc)|]. intro k;rewrite nlook_trp,nassoc_tymap;reflexivity. }
 destruct(fn_sig_rel_at w R Fl b Hload _ _ _ _ _ _ _ _ _ _ _ _ _ E Hi Hw Hg D TR Wthis L)as[Sig _].
 assert(SR:SubRel(suf Fl(en_uf e))(map(trp Fl)(tymap e))(A.c_ty C)).
 { intro k. rewrite(cr_ty _ _ _ _ _ _ _ Hc),nlook_trp,nassoc_tymap.
   destruct(tn_get e k);cbn[option_map];[rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))|];reflexivity. }
 pose proof(sigof_subst WW(suf Fl(en_uf e))_ _ _ _ Sig _ SR)as S.
 assert(Ethis:option_map(ExecutionModel.app_subst(suf Fl(en_uf e)))(option_map(tr Fl)this)=option_map(fun y=>tr Fl(canon e y))this).
 { destruct this;cbn[option_map];[rewrite tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))|];reflexivity. }
 rewrite Ethis in S. unfold CtxWalk.tr_sig in S. cbn[A.sg_params A.sg_ret]in S.
 assert(Eps:map(ExecutionModel.app_subst(suf Fl(en_uf e)))(map(fun p=>tr Fl(snd p))(sg_params sg))=
   map(fun p=>tr Fl(canon e(snd p)))(sg_params sg)).
 { rewrite map_map. apply map_ext. intro p. apply tr_canon;exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)). }
 rewrite Eps,tr_canon in S by exact(proj1(cr_uf _ _ _ _ _ _ _ Hc)). exact S.
Qed.

Ltac noerr_provider H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(? &? &? &H)])].
Theorem match_fn_sig_signature:forall fuel c e F C pv wanted kind s sm s' recv n ps rt body,
 match_fn_sig fuel c e pv wanted kind s=Good(sm,s') ->Inv s->st_w s=w ->
 CxRel w R Fl c e F C ->nget(w_syms w)(gs_id pv)=Some pv ->
 gs_decl pv=ADFn recv n ps rt body ->Later w P R Fl s' ->
 A.SigOf WW(gs_id pv)(A.c_ty C)None(canonical_sig Fl e wanted).
Proof.
 intros fuel c e F C pv wanted kind s sm s' recv n ps rt body E Hi Hw Hc Hg D L.
 destruct fuel as[|fuel];[discriminate E|]. cbn[match_fn_sig]in E.
 apply bind_inv in E as(info &s1 &Einfo &E).
 apply bind_inv in E as(rbu&rbs&RBk&E). pose proof(requirements_in_force_keep _ _ _ _ _ _ _ _ RBk)as ->.
 destruct(ok_fn_info_frame _ _ _ _ Hi Einfo)as[I1 F1].
 apply bind_inv in E as(pairs &s2 &Epairs &E).
 assert(On:ok(require_needs(cx_mod c)e pv info))by(unfold require_needs;okt).
 destruct(On _ _ _ I1 Epairs)as[I2 F2].
 apply bind_inv in E as(got &s3 &Esig &E).
 assert(Os:ok(fn_sig pv(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Os _ _ _ I2 Esig)as[I3 F3].
 destruct(sig_eqb_in e got e wanted)eqn:Eq;[|noerr_provider E].
 apply bind_inv in E as(umeta &se &Eevent &Emap).
 assert(Oevent:ok(record_reify_event(REProvider ety key(gs_id pv)(fi_needs info)(reify_sig e got)(reify_arguments e))))by(unfold record_reify_event;apply ok_reify_update).
 destruct(Oevent _ _ _ I3 Eevent)as[Ie Fevent].
 assert(Om:ok(smap_at(fi_env info)e pairs))by(unfold smap_at;okt).
 destruct(Om _ _ _ Ie Emap)as[I4 F4].
 pose proof(fn_sig_canonical c e F C pv None s2 got s3 recv n ps rt body Esig I2
   ltac:(rewrite(fr_w _ _ F2),(fr_w _ _ F1);exact Hw)Hc Hg D I(later_back w P R Fl _ _ (fr_trans _ _ _ Fevent F4) L))as Sig.
 rewrite(sig_eqb_in_canonical Fl e got e wanted Eq)in Sig. exact Sig.
Qed.
End Canonical.
