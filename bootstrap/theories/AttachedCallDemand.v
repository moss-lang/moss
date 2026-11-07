From NM Require Import BodyProofTools.
From NM Require Import WrapperKeyReadonly.
From NM Require Import CallFwdReadonly.
(* Actual defined attached calls when every need is already in force. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve BodySoundCore BodyTr BodyTy
 CanonTypes EtyTyExec Merge InstantiateTr NominalSupport SourceSupport NeedMapExec MethodKeyTr CallArgs CallCacheExec
 CallDefinedCore CallThetaExec DemandEmptyExec ProviderSig ReceiverCallEnv ReceiverFnSig
 CtxWalk CtxMerge SourceSatisfiesAfter MethodSelectExec MethodCallCore MethodCallRoutes AttachedCallExec AttachedCallNoDemand NeedReceiverFormation FamilyDemandExec FamilyDemandFrame FamilyDemandGroup.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Lemma receiver_types_sub:forall Fl xs C,
 A.c_sub(fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))xs C)=A.c_sub C.
Proof. intros Fl xs;induction xs as[|q rest IH];intro C;[reflexivity|cbn[fold_left];rewrite IH;reflexivity]. Qed.
Section Attached.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Variable checked:st.
Hypothesis Hcontexts:check_contexts checked=Good(tt,checked).
Hypothesis Hchecked:st_w checked=w.
Hypothesis ReqNd:forall n,NoDup(A.w_reqs(BodySoundCore.W w R Fl)n).
Notation WW:=(BodySoundCore.W w R Fl).
Notation tc:=(fun e y=>tr Fl(canon e y)).
Theorem attached_demand_sound:forall f,OK w P R Fl f->
 forall c e F C ox r n h att body args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=Some body->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E.
 destruct(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s(Some att)s(cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw Lookup)as[_[Ha Hg]].
 destruct(attached_decl_loaded w R Fl b Hload h n att body Ha Hg Hb)as(rn&ps&rt&D).
 pose proof(attached_nonmain w R Fl att rn n ps rt body Hg D)as Nm.
 destruct(attached_code_exec f c e ox r att args s x t s' body Hb E)
 as(info&sf&e2&st&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Et&Ed&En&Es&Eg&Ea&El&Ec&->&->).
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Ifi Ffi].
 pose proof(proj2(okI_fn_info att)_ _ _ Hi Ii Ei)as Iifi.
 destruct(ok_receiver_types(receiver_args e r)e sf e2 st Ifi Et)as[Ist Fst].
 pose proof(info_step _ _ Iifi(frl_of_fr _ _ Fst)(ni_receiver_types _ _ _ _ _ Et) ltac:(wrapper_cache_same))as Iist.
 destruct(ok_demand_needs f c e2 info _ _ _ Ist Ed)as[Isd Fd].
 pose proof(proj2(okI_demand_needs f c e2 info)_ _ _ Ist Iist Ed)as Iisd.
 destruct(receiver_types_context w R Fl(receiver_args e r)c e F C sf e2 st Et Hc
  (receiver_args_properties w R Fl c e F C r Hc Wr Sr))as[Ue Hc2].
 set(C2:=fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))(receiver_args e r)C)in *.
 assert(On:ok(require_needs(cx_mod c)ed att info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Isd En)as[Isn Fn].
 pose proof(info_step _ _ Iisd(frl_of_fr _ _ Fn)(ni_require_needs _ _ _ _ _ _ _ En) ltac:(wrapper_cache_same))as Iisn.
 assert(Os:ok(smap_at(fi_env info)ed pairs)).
 {unfold smap_at;apply ok_mmap;intros[a z];apply ok_bind;[apply ok_key_item_at_frame|intro].
  apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Os _ _ _ Isn Es)as[Iss Fs].
 pose proof(info_step _ _ Iisn(frl_of_fr _ _ Fs)(ni_smap_at _ _ _ _ _ _ Es) ltac:(wrapper_cache_same))as Iiss.
 assert(Og:ok(fn_sig att(tymap ed)ed(Some r)))by(unfold fn_sig;okt).
 destruct(Og _ _ _ Iss Eg)as[Isg Fg].
 pose proof(info_step _ _ Iiss(frl_of_fr _ _ Fg)(ni_fn_sig _ _ _ _ _ _ _ Eg) ltac:(wrapper_cache_same))as Iisg.
 destruct(ok_check_args f c e(gs_name att)sg(map AA args)_ _ _ Isg Ea)as[Isa Fa].
 destruct(ok_layout_ok e(sg_ret sg)_ _ _ Isa El)as[Isl Flay].
 destruct(ok_call_theta ed att _ _ _ Isl Ec)as[Ifin Ft].
 pose proof(post_back w P R Fl _ _ F(fr_trans _ _ _ Flay Ft)Hp)as Hpa.
 pose proof(later_back w P R Fl _ _ Fa(proj1 Hpa))as Lg.
 pose proof(later_back w P R Fl _ _ Fg Lg)as Ls.
 assert(Wss:st_w ss=w)by(rewrite<-(fr_w _ _ Fg);exact(lt_w _ _ _ _ _ Lg)).
 pose proof(later_back w P R Fl _ _ Fs Ls)as Ln.
 pose proof(later_back w P R Fl _ _ Fn Ln)as Ld.
 pose proof(later_back w P R Fl _ _ Fd Ld)as Lt.
 pose proof(later_back w P R Fl _ _ Fst Lt)as Li.
 pose proof(fn_info_need_receivers w R Fl b Hload Hworld att s info sf Hg Ei Hi Ii Hw Li)as NeedFormation.
 destruct(demand_needs_exec w R Fl b Hload Finj f c e2 F C2 info st pre ed sd checked
 Hworld Hcontexts Hchecked Ed Ist(fr_w' w _ _ Fst(fr_w' w _ _ Ffi Hw))Ld Hc2 NeedFormation)as(Cd&Demand&HCd).
 destruct(fn_info_satisfaction_after w R Fl b Hload Finj att s info sf sd(cx_mod c)ed pairs sn sm ss c F Cd
   Hg Nm Ei(fr_trans _ _ _ Fst Fd)En Es Hi Ii Hw Isd Ls HCd)as[Cf[Ctx Sat]].
 assert(Types:forall raw,tr Fl(canon ed raw)=tr Fl(canon e raw)).
 {intro raw;rewrite <-!tr_canon by(first[exact(proj1(cr_uf _ _ _ _ _ _ _ HCd))|exact(proj1(cr_uf _ _ _ _ _ _ _ Hc))]).
 rewrite <-(cr_sub _ _ _ _ _ _ _ HCd),<-(cr_sub _ _ _ _ _ _ _ Hc).
 rewrite(proj2(demand_types _ _ _ _ _ _ _ Demand)).
 unfold C2;rewrite receiver_types_sub;reflexivity. }
 assert(Sigs:canonical_sig Fl ed sg=canonical_sig Fl e sg).
 {unfold canonical_sig;f_equal;[|apply Types].
 apply map_ext;intros[nm raw];cbn[fst snd];apply Types. }
 pose proof(fn_sig_canonical w R Fl b Hload c ed F Cd att(Some r)ss sg sgstate(RAttached rn)n ps rt(Some body)
   Eg Iss Wss HCd Hg D Wr Lg)as Sig.
 rewrite Sigs in Sig.
 change(A.SigOf WW(gs_id att)(A.c_ty Cd)(Some(tc ed r))(canonical_sig Fl e sg))in Sig.
 cbn beta in Sig;rewrite Types in Sig.
 destruct(fn_sig_receiver_properties w R Fl b Hload c ed F Cd att r ss sg sgstate Eg Iss Wss Hg HCd Wr Sr Lg)
 as[Clean[Sp[Sret Sthis]]];destruct Clean as[Wps[Wret Wthis]].
 assert(Both:Forall(fun p=>WFty(snd p)/\Support WW(snd p))(sg_params sg)).
 {apply Forall_forall;intros p Hin;split.
  - apply ety_ok_wf;exact(proj1(Forall_forall _ _)Wps(snd p)(in_map snd _ _ Hin)).
  - exact(proj1(Forall_forall _ _)Sp p Hin). }
 pose proof(@check_args_aa_ok w P R Fl f IH c e(gs_name att)sg args sgstate xs sa F C Both Ea Isg Iisg ltac:(fwd_inv) Hc Hpa)as Args.
 pose proof(call_theta_tr w R Fl c ed F Cd att sl th s' Ec Isl(proj1 Hp)HCd)as Theta.
 rewrite Theta.
 split;[|split;[exact(ety_ok_wf _ Wret)|exact Sret]].
 assert(Ectx:fold_left(fun C p=>A.set_ty C(fst p)(snd p))(recv_sem Fl(receiver_args e r))C=C2)
 by(apply fold_receiver_sem).
 rewrite<-Ectx in Demand.
 eapply A.MCAttachedDefinedDemand with(h:=h)(att:=gs_id att)(Ca:=Cf)(ns:=map(key_tr Fl)(fi_needs info))
  (g:=canonical_sig Fl e sg)(rargs:=recv_sem Fl(receiver_args e r))(Cd:=Cd).
 - exact(head_tr_some Fl(canon e r)h Head).
 - exact Ha.
 - exact(detached_provision_absent w R Fl b Hload c e F C r n Hc(method_receiver_clean w R Fl b Hload c e F C r Hc Wr Sr)Absent).
 - exists rn,ps,rt,body;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite D;reflexivity.
 - exact(receiver_args_head Fl e r h Head).
 - exact Ctx.
 - exact Demand.
 - exact Sat.
 - exact Sig.
 - exact Args.
Qed.
End Attached.
Section Final.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Theorem attached_demand_step:forall f,OK w P R Fl f->@AttachedDemandStep w P R Fl f.
Proof.
 intros f IH c e F C ox r n h att body args s x t sf HC Wr Sr Hi Ii Fi Post Head Lookup World Body Absent Run Nonempty.
 destruct(lt_contexts _ _ _ _ _(proj1 Post))as(sc&Checked&Wsc).
 exact(attached_demand_sound w R Fl b Hload Finj Hworld sc Checked Wsc f IH
 c e F C ox r n h att body args s x t sf HC Wr Sr Hi Ii Fi Post Head Lookup World Body Absent Run).
Qed.
End Final.
