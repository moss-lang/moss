From NM Require Import WrapperKeyReadonly.
From NM Require Import CallFwdReadonly.
(* Actual defined attached calls when every need is already in force. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame InfoPreserve BodySound BodyTr BodyTy
 CanonTypes EtyTyExec Merge InstantiateTr NominalSupport SourceSupport NeedMapExec MethodKeyTr CallArgs CallCacheExec
 CallDefinedCore CallThetaExec DemandEmptyExec ProviderSig ReceiverCallEnv ReceiverFnSig
 CtxWalk CtxMerge SourceSatisfiesAfter MethodSelectExec MethodCallCore MethodCallRoutes AttachedCallExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Section Attached.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Hypothesis ReqNd:forall n,NoDup(A.w_reqs(BodySound.W w R Fl)n).
Notation WW:=(BodySound.W w R Fl).
Notation tc:=(fun e y=>tr Fl(canon e y)).
Lemma attached_decl_loaded:forall h n att body,
 A.Attached WW h n(gs_id att)->nget(w_syms w)(gs_id att)=Some att->fn_body att=Some body->
 exists rn ps rt,gs_decl att=ADFn(RAttached rn)n ps rt(Some body).
Proof.
 intros h n att body Ha Hg Hb.
 apply(proj1(lr_attached _ _(load_world_reads b w R Fl Hload)h n(gs_id att)))in Ha.
 destruct Ha as(d&rn&ps&rt&body0&Hd&D&Rest).
 change(A.w_decl WW(gs_id att)=Some d)in Hd.
 rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-;cbn[A.d_ast]in D.
 unfold fn_body in Hb;rewrite D in Hb;cbn in Hb;subst body0.
 exists rn,ps,rt;exact D.
Qed.
Lemma attached_nonmain:forall att rn n ps rt body,
 nget(w_syms w)(gs_id att)=Some att->gs_decl att=ADFn(RAttached rn)n ps rt(Some body)->
 ~A.IsMainFn WW(gs_id att).
Proof.
 intros att rn n ps rt body Hg D(d&nm&ps0&rt0&body0&p&Hd&D0&Scope).
 rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-;cbn[A.d_ast]in D0;rewrite D in D0;discriminate D0.
Qed.
Theorem attached_ready_sound:forall f,OK w P R Fl f->
 forall c e F C ox r n h att body args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=Some body->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 (forall info sf e2 st,fn_info att s=Good(info,sf)->receiver_types(receiver_args e r)e sf=Good(e2,st)->
  forall k,In k(fi_needs info)->key_present e2(site_key e2 k)=true)->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E Ready.
 destruct(attached_call_lookup_any w R Fl b Hload(cx_mod c)h n s(Some att)s(cr_mod_in _ _ _ _ _ _ _ Hc)Hi Hw Lookup)as[_[Ha Hg]].
 destruct(attached_decl_loaded h n att body Ha Hg Hb)as(rn&ps&rt&D).
 pose proof(attached_nonmain att rn n ps rt body Hg D)as Nm.
 destruct(attached_code_exec f c e ox r att args s x t s' body Hb E)
 as(info&sf&e2&st&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Et&Ed&En&Es&Eg&Ea&El&Ec&->&->).
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Ifi Ffi].
 pose proof(proj2(okI_fn_info att)_ _ _ Hi Ii Ei)as Iifi.
 destruct(ok_receiver_types(receiver_args e r)e sf e2 st Ifi Et)as[Ist Fst].
 pose proof(info_step _ _ Iifi(frl_of_fr _ _ Fst)(ni_receiver_types _ _ _ _ _ Et) ltac:(wrapper_cache_same))as Iist.
 destruct(demand_needs_present _ _ _ _ _ _ _ _ Ist(Ready _ _ _ _ Ei Et)Ed)as[-> [-> ->]].
 destruct(receiver_types_context w R Fl(receiver_args e r)c e F C sf e2 st Et Hc
  (receiver_args_properties w R Fl c e F C r Hc Wr Sr))as[Ue Hc2].
 set(C2:=fold_left(fun C p=>A.set_ty C(fst p)(tr Fl(snd p)))(receiver_args e r)C)in *.
 assert(On:ok(require_needs(cx_mod c)e2 att info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Ist En)as[Isn Fn].
 pose proof(info_step _ _ Iist(frl_of_fr _ _ Fn)(ni_require_needs _ _ _ _ _ _ _ En) ltac:(wrapper_cache_same))as Iisn.
 assert(Os:ok(smap_at(fi_env info)e2 pairs)).
 {unfold smap_at;apply ok_mmap;intros[a z];apply ok_bind;[apply ok_key_item_at_frame|intro].
  apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Os _ _ _ Isn Es)as[Iss Fs].
 pose proof(info_step _ _ Iisn(frl_of_fr _ _ Fs)(ni_smap_at _ _ _ _ _ _ Es) ltac:(wrapper_cache_same))as Iiss.
 assert(Og:ok(fn_sig att(tymap e2)e2(Some r)))by(unfold fn_sig;okt).
 destruct(Og _ _ _ Iss Eg)as[Isg Fg].
 pose proof(info_step _ _ Iiss(frl_of_fr _ _ Fg)(ni_fn_sig _ _ _ _ _ _ _ Eg) ltac:(wrapper_cache_same))as Iisg.
 destruct(ok_check_args f c e(gs_name att)sg(map AA args)_ _ _ Isg Ea)as[Isa Fa].
 destruct(ok_layout_ok e(sg_ret sg)_ _ _ Isa El)as[Isl Flay].
 destruct(ok_call_theta e2 att _ _ _ Isl Ec)as[Ifin Ft].
 pose proof(post_back w P R Fl _ _ F(fr_trans _ _ _ Flay Ft)Hp)as Hpa.
 pose proof(later_back w P R Fl _ _ Fa(proj1 Hpa))as Lg.
 pose proof(later_back w P R Fl _ _ Fg Lg)as Ls.
 assert(Wss:st_w ss=w)by(rewrite<-(fr_w _ _ Fg);exact(lt_w _ _ _ _ _ Lg)).
 destruct(fn_info_satisfaction_after w R Fl b Hload Finj att s info sf st(cx_mod c)e2 pairs sn sm ss c F C2
   Hg Nm Ei Fst En Es Hi Ii Hw Ist Ls Hc2)as[Cf[Ctx Sat]].
 pose proof(fn_sig_canonical w R Fl b Hload c e2 F C2 att(Some r)ss sg sgstate(RAttached rn)n ps rt(Some body)
   Eg Iss Wss Hc2 Hg D Wr Lg)as Sig.
 rewrite(@canonical_sig_uf Fl e e2 sg Ue)in Sig.
 change(A.SigOf WW(gs_id att)(A.c_ty C2)(Some(tc e2 r))(canonical_sig Fl e sg))in Sig.
 cbn beta in Sig;rewrite(canon_uf e e2 Ue)in Sig.
 pose proof(receiver_args_nodup w R Fl c e F C r Hc Sr ReqNd)as Nd.
 assert(Sig':A.SigOf WW(gs_id att)(recv_sem Fl(receiver_args e r)++A.c_ty C)(Some(tc e r))(canonical_sig Fl e sg)).
 {eapply(sigof_lookup_ext WW(lt_reqclosed _ _ _ _ _ Lg));[|exact Sig].
  intro k;symmetry;exact(receiver_tymap_lookup Fl(receiver_args e r)C Nd k). }
 destruct(fn_sig_receiver_properties w R Fl b Hload c e2 F C2 att r ss sg sgstate Eg Iss Wss Hg Hc2 Wr Sr Lg)
 as[Clean[Sp[Sret Sthis]]];destruct Clean as[Wps[Wret Wthis]].
 assert(Both:Forall(fun p=>WFty(snd p)/\Support WW(snd p))(sg_params sg)).
 {apply Forall_forall;intros p Hin;split.
  - apply ety_ok_wf;exact(proj1(Forall_forall _ _)Wps(snd p)(in_map snd _ _ Hin)).
  - exact(proj1(Forall_forall _ _)Sp p Hin). }
 pose proof(@check_args_aa_ok w P R Fl f IH c e(gs_name att)sg args sgstate xs sa F C Both Ea Isg Iisg ltac:(fwd_inv) Hc Hpa)as Args.
 pose proof(call_theta_tr w R Fl c e2 F C2 att sl th s' Ec Isl(proj1 Hp)Hc2)as Theta.
 rewrite Theta;cbn[fold_stmts].
 split;[|split;[exact(ety_ok_wf _ Wret)|exact Sret]].
 assert(Ectx:fold_left(fun C p=>A.set_ty C(fst p)(snd p))(recv_sem Fl(receiver_args e r))C=C2)
 by(apply fold_receiver_sem).
 rewrite<-Ectx.
 eapply A.MCAttachedDefined with(h:=h)(att:=gs_id att)(Ca:=Cf)(ns:=map(key_tr Fl)(fi_needs info))
  (g:=canonical_sig Fl e sg)(rargs:=recv_sem Fl(receiver_args e r)).
 - exact(head_tr_some Fl(canon e r)h Head).
 - exact Ha.
 - exact(detached_provision_absent w R Fl b Hload c e F C r n Hc(method_receiver_clean w R Fl b Hload c e F C r Hc Wr Sr)Absent).
 - exists rn,ps,rt,body;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite D;reflexivity.
 - exact(receiver_args_head Fl e r h Head).
 - exact Ctx.
 - rewrite Ectx;exact Sat.
 - exact Sig'.
 - exact Args.
Qed.
Theorem attached_empty_sound:forall f,OK w P R Fl f->
 forall c e F C ox r n h att body args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=Some body->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 (exists info sf e2 st ed sd,fn_info att s=Good(info,sf)/\receiver_types(receiver_args e r)e sf=Good(e2,st)/\
 demand_needs f c e2 info st=Good(([],ed),sd))->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Proof.
 intros f IH c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E Empty.
 destruct(attached_code_exec f c e ox r att args s x t s' body Hb E)
 as(info&sf&e2&st&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Et&Ed&En&Es&Eg&Ea&El&Ec&Ex&Er).
 destruct Empty as(info0&sf0&e20&st0&ed0&sd0&Ei0&Et0&Ed0).
 rewrite Ei in Ei0;injection Ei0 as <- <-.
 rewrite Et in Et0;injection Et0 as <- <-.
 pose proof(demand_needs_empty_env _ _ _ _ _ _ _ Ed0)as Env.
 assert(EE:ed=e2)by(congruence);subst ed.
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Ifi Ffi].
 destruct(ok_receiver_types(receiver_args e r)e sf e2 st Ifi Et)as[Ist Fst].
 destruct(ok_demand_needs f c e2 info _ _ _ Ist Ed)as[Isd Fd].
 destruct(require_needs_exact _ _ _ _ _ _ _ Isd En)as[_[Present _]].
 assert(Ready:forall i is ee ts,fn_info att s=Good(i,is)->receiver_types(receiver_args e r)e is=Good(ee,ts)->
  forall k,In k(fi_needs i)->key_present ee(site_key ee k)=true).
 {intros i is ee ts Eii Ett k Hin;rewrite Ei in Eii;injection Eii as <- <-.
  rewrite Et in Ett;injection Ett as <- <-;exact(proj1(Forall_forall _ _)Present _ Hin). }
 exact(attached_ready_sound f IH c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E Ready).
Qed.
Definition AttachedDemandStep(f:nat):Prop:=
 forall c e F C ox r n h att body args s x t s',
 CxRel w R Fl c e F C->WFty r->Support WW r->Inv s->InfoInv s->FwdInv s->Post w P R Fl s' F->
 head(canon e r)=Some h->attached_call_lookup(cx_mod c)h n s=Good(Some att,s)->st_w s=w->fn_body att=Some body->
 (forall d,sassoc n(gm_detached(get_mod w(cx_mod c)))=Some d->get_mth e(canon e r)d=None)->
 method_attached_code f c e ox r att args s=Good((x,t),s')->
 (exists info sf e2 st pre ed sd,fn_info att s=Good(info,sf)/\receiver_types(receiver_args e r)e sf=Good(e2,st)/\
  demand_needs f c e2 info st=Good((pre,ed),sd)/\pre<>[])->
 A.MethCall WW P F C ox(tc e r)[n]args x(tc e t)/\WFty t/\Support WW t.
Theorem attached_defined_step:forall f,OK w P R Fl f->AttachedDemandStep f->AttachedDefinedStep w P R Fl f.
Proof.
 intros f IH HD c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E.
 destruct(attached_code_exec f c e ox r att args s x t s' body Hb E)
 as(info&sf&e2&st&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Et&Ed&En&Es&Eg&Ea&El&Ec&Ex&Er).
 destruct pre as[|first rest].
 - eapply attached_empty_sound;[exact IH|exact Hc|exact Wr|exact Sr|exact Hi|exact Ii|exact Hfi|exact Hp|exact Head|exact Lookup|exact Hw|exact Hb|exact Absent|exact E|].
   exists info,sf,e2,st,ed,sd;repeat split;assumption.
 - apply(HD c e F C ox r n h att body args s x t s' Hc Wr Sr Hi Ii Hfi Hp Head Lookup Hw Hb Absent E).
   exists info,sf,e2,st,(first::rest),ed,sd;repeat split;try assumption;discriminate.
Qed.
End Attached.
