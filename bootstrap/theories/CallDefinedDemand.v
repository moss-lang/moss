From NM Require Import WrapperKeyReadonly.
(* Actual defined calls with arbitrary automatic family demands. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabMono WorldSound ElabFrame InfoPreserve BodySound BodyTy BodyTr
 CtxMerge MethodKeyTr CanonTypes CanonBodyTy NominalSupport EtyTyExec CallArgs ProviderSig SourceSatisfies
 LoaderOrigin MainRoot CallAbstract CallCacheExec NeedMapExec CallDefinedCore CallThetaExec FnSigExec InstantiateTr
 CallDefined CallDefinedAny SourceSatisfiesAny NeedReceiverFormation FamilyDemandExec FamilyDemandFrame CallFwdReadonly.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Section Defined.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Variable checked:st.
Hypothesis Hcontexts:check_contexts checked=Good(tt,checked).
Hypothesis Hchecked:st_w checked=w.
Notation WW:=(BodySound.W w R Fl).
Theorem defined_demand_ex:forall f,OK w P R Fl f->
 forall c e g pa args exp s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id g)->A.kind_sym WW(gs_id g)=Some A.DKFn->
 A.Ex WW P F C(AECall pa None args)exp x(tr Fl(canon e t))/\WFty t/\Support WW t.
Proof.
 intros f IH c e g pa args exp s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Resolve Kind.
 destruct(defined_call_code_exec _ _ _ _ _ _ _ _ _ _ E)
 as(info&si&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Ed&En&Es&Eg&Ea&El&Et&->&->).
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Isi Fsi].
 pose proof(proj2(okI_fn_info g)_ _ _ Hi Ii Ei)as Iisi.
 destruct(ok_demand_needs f c e info _ _ _ Isi Ed)as[Isd Fd].
 pose proof(proj2(okI_demand_needs f c e info)_ _ _ Isi Iisi Ed)as Iisd.
 assert(On:ok(require_needs(cx_mod c)ed g info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Isd En)as[Isn Fn].
 pose proof(info_step _ _ Iisd(frl_of_fr _ _ Fn)(ni_require_needs _ _ _ _ _ _ _ En) ltac:(wrapper_cache_same))as Iisn.
 assert(Os:ok(smap_at(fi_env info)ed pairs)).
 { unfold smap_at. apply ok_mmap. intros[a z]. apply ok_bind;[apply ok_key_item_at_frame|intro].
   apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Os _ _ _ Isn Es)as[Iss Fs].
 pose proof(info_step _ _ Iisn(frl_of_fr _ _ Fs)(ni_smap_at _ _ _ _ _ _ Es) ltac:(wrapper_cache_same))as Iiss.
 assert(Og:ok(fn_sig g(tymap ed)ed None))by(unfold fn_sig;okt).
 destruct(Og _ _ _ Iss Eg)as[Isg Fg].
 pose proof(info_step _ _ Iiss(frl_of_fr _ _ Fg)(ni_fn_sig _ _ _ _ _ _ _ Eg) ltac:(wrapper_cache_same))as Iisg.
 destruct(ok_check_args f c ed(gs_name g)sg(map AA args)_ _ _ Isg Ea)as[Isa Fa].
 destruct(ok_layout_ok ed(sg_ret sg)_ _ _ Isa El)as[Isl Flay].
 assert(Ot:ok(call_theta ed g))by apply ok_call_theta.
 destruct(Ot _ _ _ Isl Et)as[Ifin Ft].
 pose proof(post_back w P R Fl _ _ F(fr_trans _ _ _ Flay Ft)Hp)as Hpa.
 pose proof(later_back w P R Fl _ _ Fa(proj1 Hpa))as Lg.
 pose proof(later_back w P R Fl _ _ Fg Lg)as Ls.
 assert(Wss:st_w ss=w)by(rewrite <-(fr_w _ _ Fg);exact(lt_w _ _ _ _ _ Lg)).
 assert(Hw:st_w s=w).
 { rewrite <-(fr_w _ _ (fr_trans _ _ _ Fsi(fr_trans _ _ _ Fd(fr_trans _ _ _ Fn Fs))));exact Wss. }
 pose proof(later_back w P R Fl _ _ Fs Ls)as Ln.
 pose proof(later_back w P R Fl _ _ Fn Ln)as Ld.
 pose proof(later_back w P R Fl _ _ Fd Ld)as Li.
 pose proof(fn_info_need_receivers w R Fl b Hload Hworld g s info si Hg Ei Hi Ii Hw Li)as NeedFormation.
 destruct(demand_needs_exec w R Fl b Hload Finj f c e F C info si pre ed sd checked
 Hworld Hcontexts Hchecked Ed Isi(fr_w' w _ _ Fsi Hw)Ld Hc NeedFormation)as(Cd&Demand&HCd).
 destruct(@fn_info_satisfaction_any_after w P R Fl b Hload Hworld Finj g s info si sd(cx_mod c)ed pairs sn sm ss c F Cd
   Hg Ei Fd En Es Hi Ii Hw Isd Ls HCd)as[Cf[Ctx Sat]].
 pose proof(fn_sig_canonical w R Fl b Hload c ed F Cd g None ss sg sgstate RPlain n ps rt(Some body)
   Eg Iss Wss HCd Hg Decl I Lg)as Sig.
 destruct(fn_sig_body_properties w R Fl b Hload c ed F Cd g ss sg sgstate Eg Iss Wss Hg HCd Lg)
   as[Clean[Sp[Sr Sthis]]]. destruct Clean as[Wps[Wr Wthis]].
 assert(Both:Forall(fun p=>WFty(snd p)/\Support WW(snd p))(sg_params sg)).
 { apply Forall_forall;intros p Hin;split.
   - apply ety_ok_wf;exact(proj1(Forall_forall _ _)Wps(snd p)(in_map snd _ _ Hin)).
   - exact(proj1(Forall_forall _ _)Sp p Hin). }
 pose proof(@check_args_aa_ok w P R Fl f IH c ed(gs_name g)sg args sgstate xs sa F Cd Both Ea Isg
   Iisg ltac:(fwd_inv) HCd Hpa)as Args.
 pose proof(call_theta_tr w R Fl c ed F Cd g sl th s' Et Isl(proj1 Hp)HCd)as Theta.
 assert(Types:forall raw,tr Fl(canon ed raw)=tr Fl(canon e raw)).
 {intro raw;rewrite <-!tr_canon by(first[exact(proj1(cr_uf _ _ _ _ _ _ _ HCd))|exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))]).
  rewrite <-(cr_sub _ _ _ _ _ _ _ HCd),<-(cr_sub _ _ _ _ _ _ _ Hc).
  rewrite(proj2(demand_types _ _ _ _ _ _ _ Demand));reflexivity. }
 split;[|split;[exact(ety_ok_wf _ Wr)|exact Sr]].
 rewrite <-Types;rewrite Theta.
 eapply A.ExCallDDemand with(Cf:=Cf)(ns:=map(key_tr Fl)(fi_needs info))(g:=canonical_sig Fl ed sg)(C':=Cd).
 - exact Resolve.
 - exact Kind.
 - exists n,ps,rt,body;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite Decl;reflexivity.
 - exact Ctx.
 - exact Demand.
 - exact Sat.
 - exact Sig.
 - exact Args.
Qed.
End Defined.
Section Final.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySound.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Theorem defined_demand_step:forall f,OK w P R Fl f->@DefinedDemandStep w P R Fl f.
Proof.
 intros f IH c e g pa args exp s x t s' F C n ps rt body Run Hi Ii Fi HC Post Hg D Resolve Kind Nonempty.
 destruct(lt_contexts _ _ _ _ _(proj1 Post))as(sc&Checked&World).
 exact(defined_demand_ex w R Fl b Hload Hworld Finj sc Checked World f IH c e g pa args exp s x t s' F C n ps rt body
 Run Hi Ii Fi HC Post Hg D Resolve Kind).
Qed.
End Final.
