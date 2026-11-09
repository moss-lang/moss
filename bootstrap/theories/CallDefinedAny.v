From NM Require Import BodyProofTools.
From NM Require Import WrapperKeyReadonly.
From NM Require Import CallFwdReadonly.
(* Actual defined calls, including main; only real nonempty demand remains. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabMono WorldSound ElabFrame InfoPreserve BodySoundCore BodyTy BodyTr
 CtxMerge MethodKeyTr CanonTypes CanonBodyTy NominalSupport EtyTyExec CallArgs ProviderSig SourceSatisfies
 LoaderOrigin MainRoot CallAbstract CallCacheExec DemandEmptyExec NeedMapExec CallDefinedCore CallThetaExec FnSigExec InstantiateTr CallDefined SourceSatisfiesAny.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Section Defined.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Hworld:A.WorldOf b(BodySoundCore.W w R Fl).
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem defined_ready_any : forall f,OK w P R Fl f->
 forall c e g args s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 (forall info si,fn_info g s=Good(info,si)->forall k,In k(fi_needs info)->key_present e(site_key e k)=true)->
 exists info si Cf sm sg xs,
 fn_info g s=Good(info,si)/\
 A.CtxOf WW(gs_id g)Cf(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cf(map(key_tr Fl)(fi_needs info))sm/\
 A.SigOf WW(gs_id g)(A.c_ty C)None(canonical_sig Fl e sg)/\
 A.Args WW P F C args(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))xs/\
 x=XCallD(gs_id g)(A.theta_at WW C(gs_id g))sm xs/\t=sg_ret sg/\WFty t/\Support WW t.
Proof.
 intros f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Ready.
 destruct(defined_call_code_exec _ _ _ _ _ _ _ _ _ _ E)
 as(info&si&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Ed&En&Es&Eg&Ea&El&Et&->&->).
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Isi Fsi].
 pose proof(proj2(okI_fn_info g)_ _ _ Hi Ii Ei)as Iisi.
 destruct(demand_needs_present _ _ _ _ _ _ _ _ Isi(Ready _ _ Ei)Ed)as[-> [-> ->]].
 assert(On:ok(require_needs(cx_mod c)e g info))by(unfold require_needs,require_merges;okt).
 destruct(On _ _ _ Isi En)as[Isn Fn].
 pose proof(info_step _ _ Iisi(frl_of_fr _ _ Fn)(ni_require_needs _ _ _ _ _ _ _ En) ltac:(wrapper_cache_same))as Iisn.
 assert(Os:ok(smap_at(fi_env info)e pairs)).
 { unfold smap_at. apply ok_mmap. intros[a z]. apply ok_bind;[apply ok_key_item_at_frame|intro].
   apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Os _ _ _ Isn Es)as[Iss Fs].
 pose proof(info_step _ _ Iisn(frl_of_fr _ _ Fs)(ni_smap_at _ _ _ _ _ _ Es) ltac:(wrapper_cache_same))as Iiss.
 assert(Og:ok(fn_sig g(tymap e)e None))by(unfold fn_sig;okt).
 destruct(Og _ _ _ Iss Eg)as[Isg Fg].
 pose proof(info_step _ _ Iiss(frl_of_fr _ _ Fg)(ni_fn_sig _ _ _ _ _ _ _ Eg) ltac:(wrapper_cache_same))as Iisg.
 destruct(ok_check_args f c e(gs_name g)sg(map AA args)_ _ _ Isg Ea)as[Isa Fa].
 destruct(ok_layout_ok e(sg_ret sg)_ _ _ Isa El)as[Isl Flay].
 assert(Ot:ok(call_theta e g))by apply ok_call_theta.
 destruct(Ot _ _ _ Isl Et)as[Ifin Ft].
 pose proof(post_back w P R Fl _ _ F(fr_trans _ _ _ Flay Ft)Hp)as Hpa.
 pose proof(later_back w P R Fl _ _ Fa(proj1 Hpa))as Lg.
 pose proof(later_back w P R Fl _ _ Fg Lg)as Ls.
 assert(Wss:st_w ss=w)by(rewrite <-(fr_w _ _ Fg);exact(lt_w _ _ _ _ _ Lg)).
 assert(Hw:st_w s=w).
 { rewrite <-(fr_w _ _ (fr_trans _ _ _ Fsi(fr_trans _ _ _ Fn Fs)));exact Wss. }
 destruct(@fn_info_satisfaction_any w P R Fl b Hload Hworld Finj g s info si(cx_mod c)e pairs sn sm ss c F C
   Hg Ei En Es Hi Ii Hw Ls Hc)as[Cf[Ctx Sat]].
 pose proof(fn_sig_canonical w R Fl b Hload c e F C g None ss sg sgstate RPlain n ps rt(Some body)
   Eg Iss Wss Hc Hg Decl I Lg)as Sig.
 destruct(fn_sig_body_properties w R Fl b Hload c e F C g ss sg sgstate Eg Iss Wss Hg Hc Lg)
   as[Clean[Sp[Sr Sthis]]]. destruct Clean as[Wps[Wr Wthis]].
 assert(Both:Forall(fun p=>WFty(snd p)/\Support WW(snd p))(sg_params sg)).
 { apply Forall_forall;intros p Hin;split.
   - apply ety_ok_wf;exact(proj1(Forall_forall _ _)Wps(snd p)(in_map snd _ _ Hin)).
   - exact(proj1(Forall_forall _ _)Sp p Hin). }
 pose proof(@check_args_aa_ok w P R Fl f IH c e(gs_name g)sg args sgstate xs sa F C Both Ea Isg
   Iisg ltac:(fwd_inv) Hc Hpa)as Args.
 pose proof(call_theta_tr w R Fl c e F C g sl th s' Et Isl(proj1 Hp)Hc)as Theta.
 exists info,si,Cf,sm,sg,xs. split;[exact Ei|]. split;[exact Ctx|]. split;[exact Sat|].
 split;[exact Sig|]. split;[exact Args|]. split;[cbn[fold_stmts];now rewrite Theta|].
 split;[reflexivity|]. split;[exact(ety_ok_wf _ Wr)|exact Sr].
Qed.
Theorem defined_empty_any : forall f,OK w P R Fl f->
 forall c e g args s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 (exists info si ed sd,fn_info g s=Good(info,si)/\demand_needs f c e info si=Good(([],ed),sd))->
 exists info si Cf sm sg xs,
 fn_info g s=Good(info,si)/\
 A.CtxOf WW(gs_id g)Cf(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cf(map(key_tr Fl)(fi_needs info))sm/\
 A.SigOf WW(gs_id g)(A.c_ty C)None(canonical_sig Fl e sg)/\
 A.Args WW P F C args(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))xs/\
 x=XCallD(gs_id g)(A.theta_at WW C(gs_id g))sm xs/\t=sg_ret sg/\WFty t/\Support WW t.
Proof.
 intros f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Empty.
 destruct(defined_call_code_exec _ _ _ _ _ _ _ _ _ _ E)
 as(info&si&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Ed&En&Es&Eg&Ea&El&Et&Ex&Er).
 destruct Empty as(info0&si0&ed0&sd0&Ei0&Ed0).
 rewrite Ei in Ei0;injection Ei0 as <- <-.
 pose proof(demand_needs_empty_env _ _ _ _ _ _ _ Ed0)as Env.
 assert(EE:ed=e)by(congruence). subst ed.
 destruct(ok_fn_info_frame _ _ _ _ Hi Ei)as[Isi Fsi].
 destruct(ok_demand_needs f c e info _ _ _ Isi Ed)as[Isd Fd].
 destruct(require_needs_exact _ _ _ _ _ _ _ Isd En)as[_[Present _]].
 assert(Ready:forall i is,fn_info g s=Good(i,is)->forall k,In k(fi_needs i)->key_present e(site_key e k)=true).
 { intros i is Eii k Hin. rewrite Ei in Eii;injection Eii as <- <-.
   exact(proj1(Forall_forall _ _)Present _ Hin). }
 exact(defined_ready_any f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Ready).
Qed.
Theorem defined_empty_any_ex:forall f,OK w P R Fl f->
 forall c e g pa args exp s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id g)->A.kind_sym WW(gs_id g)=Some A.DKFn->
 (exists info si ed sd,fn_info g s=Good(info,si)/\demand_needs f c e info si=Good(([],ed),sd))->
 A.Ex WW P F C(AECall pa None args)exp x(tr Fl(canon e t))/\WFty t/\Support WW t.
Proof.
 intros f IH c e g pa args exp s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg D Hr Hk Empty.
 destruct(defined_empty_any f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg D Empty)
 as(info&si&Cf&sm&sg&xs&Ei&Ctx&Sat&Sig&Args&->&->&Wt&St).
 split;[|split;assumption].
 eapply A.ExCallD with(Cf:=Cf)(ns:=map(key_tr Fl)(fi_needs info))(g:=canonical_sig Fl e sg).
 - exact Hr.
 - exact Hk.
 - exists n,ps,rt,body;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite D;reflexivity.
 - exact Ctx.
 - exact Sat.
 - exact Sig.
 - exact Args.
Qed.
Definition DefinedDemandStep(f:nat):Prop:=
 forall c e g pa args exp s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id g)->A.kind_sym WW(gs_id g)=Some A.DKFn->
 (exists info si pre ed sd,fn_info g s=Good(info,si)/\demand_needs f c e info si=Good((pre,ed),sd)/\pre<>[])->
 A.Ex WW P F C(AECall pa None args)exp x(tr Fl(canon e t))/\WFty t/\Support WW t.
Theorem defined_fn_step:forall f,OK w P R Fl f->DefinedDemandStep f->DefinedFnStep w P R Fl f.
Proof.
 intros f IH HD c e pa a args exp s x t s' Ha E F C Hi Ii Hfi Hc Wexp Sexp Hp
  (fid&fnm&ps&rt&body&Hr&Hk&Decl).
 destruct(synth_defined_exec w R Fl b Hload f c e pa a args fid fnm ps rt body exp s x t s' F C
  Ha E Hi Hc Hp Hr Hk Decl)as[-> [g[sc[Hg[Id[D[Code[Fr Pc]]]]]]]].
 subst fid.
 destruct(defined_call_code_exec _ _ _ _ _ _ _ _ _ _ Code)
 as(info&si&pre&ed&sd&pairs&sn&sm&ss&sg&sgstate&xs&sa&u&sl&th&Ei&Ed&En&Es&Eg&Ea&El&Et&Ex&Er).
 destruct pre as[|first rest].
 - apply(defined_empty_any_ex f IH c e g pa args(option_map(fun t=>tr Fl(canon e t))exp)s x t sc F C fnm ps rt body
   Code Hi Ii Hfi Hc Pc Hg D Hr Hk).
   exists info,si,ed,sd;split;assumption.
 - apply(HD c e g pa args(option_map(fun t=>tr Fl(canon e t))exp)s x t sc F C fnm ps rt body
   Code Hi Ii Hfi Hc Pc Hg D Hr Hk).
   exists info,si,(first::rest),ed,sd;repeat split;try assumption;discriminate.
Qed.
End Defined.
