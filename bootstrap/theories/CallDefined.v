From NM Require Import BodyProofTools.
From NM Require Import WrapperKeyReadonly.
From NM Require Import CallFwdReadonly.
(* The ordinary no-demand branch of a defined call, from its actual run. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabMono WorldSound ElabFrame InfoPreserve BodySoundCore BodyTy BodyTr
 CtxMerge MethodKeyTr CanonTypes CanonBodyTy NominalSupport EtyTyExec CallArgs ProviderSig SourceSatisfies
 LoaderOrigin MainRoot CallAbstract CallCacheExec DemandEmptyExec NeedMapExec CallDefinedCore CallThetaExec FnSigExec InstantiateTr.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL canon instantiate ety_ty reqs type_requirements].
Section Defined.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a c,Fl a=Fl c->a=c.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem synth_call_defined_exec : forall f c e pa a args fid fnm ps rt body s x t s' F C, a = None ->
 synth_call(S f)c e pa a args s=Good((x,t),s')->Inv s->CxRel w R Fl c e F C->st_w s=w->
 A.resolve_sym WW(A.fr_mod F)pa=Some fid->A.kind_sym WW fid=Some A.DKFn->
 A.ast_of WW fid=Some(ADFn RPlain fnm ps rt(Some body))->
 a=None/\exists g,nget(w_syms w)fid=Some g/\gs_id g=fid/\
 gs_decl g=ADFn RPlain fnm ps rt(Some body)/\defined_call_code f c e fid g args s=Good((x,t),s').
Proof.
 intros f c e pa a args fid fnm ps rt body s x t s' F C Ha E Hi Hc Hw Resolve Kind Decl.
 cbn[synth_call]in E. apply bind_inv in E as[tg[sp[Ep E]]].
 destruct(rpath_rel w R Fl b Hload _ _ _ _ _ Hw(cr_mod_in _ _ _ _ _ _ _ Hc)Ep)as[-> Path].
 rewrite <-(cr_mod _ _ _ _ _ _ _ Hc)in Path.
 unfold A.resolve_sym in Resolve;rewrite Path in Resolve.
 destruct tg as[[id|mi]|];cbn[option_map ent_of]in Resolve;try discriminate Resolve.
 injection Resolve as ->. apply bind_inv in E as[g[ss[Es E]]].
 cbn[option_map ent_of]in Path.
 destruct(sym_loader w R Fl b Hload _ _ _ _ _ _ Hi Hw Path Es)as[-> Hg].
 pose proof(load_world_ids b w Hload fid g Hg)as Id.
 pose proof(kind_sym_read b w R Fl Hload fid g Hg)as Gkind.
 change(A.kind_sym WW fid=Some(kconv(gs_kind g)))in Gkind.
 rewrite Kind in Gkind;destruct(gs_kind g)eqn:K;try discriminate Gkind.
 change(A.ast_of(W_read w R Fl)fid=Some(ADFn RPlain fnm ps rt(Some body)))in Decl.
 unfold A.ast_of in Decl;rewrite(w_decl_read w R Fl fid g Hg)in Decl.
 cbn[option_map A.d_ast]in Decl;injection Decl as D.
 cbn beta iota in E. destruct a as[bs|];[discriminate Ha|].
 apply bind_inv in E as[mem[sm[Em E]]]. unfold is_memory_instruction in Em;injection Em as <- <-.
 apply bind_inv in E as[ms[sm[Em E]]]. pose proof(@memory_sym_state s ms sm Em)as ->.
 apply bind_inv in E as[u[su[Eu E]]]. injection Eu as <- <-.
 unfold is_defined_fn,fn_body in E;rewrite K,D in E;cbn in E.
 split;[reflexivity|]. exists g;split;[exact Hg|]. split;[exact Id|]. split;[exact D|].
 unfold defined_call_code;exact E.
Qed.

Theorem synth_defined_exec : forall f c e pa a args fid fnm ps rt body exp s x t s' F C, a = None ->
 synth(S f)c e(AECall pa a args)exp s=Good((x,t),s')->
 Inv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 A.resolve_sym WW(A.fr_mod F)pa=Some fid->A.kind_sym WW fid=Some A.DKFn->
 A.ast_of WW fid=Some(ADFn RPlain fnm ps rt(Some body))->
 a=None/\exists g sc,nget(w_syms w)fid=Some g/\gs_id g=fid/\
 gs_decl g=ADFn RPlain fnm ps rt(Some body)/\
 defined_call_code f c e fid g args s=Good((x,t),sc)/\Fr sc s'/\Post w P R Fl sc F.
Proof.
 intros f c e pa a args fid fnm ps rt body exp s x t s' F C Ha E Hi Hc Hp Resolve Kind Decl.
 cbn[synth]in E. apply bind_inv in E as[[v vt][sc[Ec E]]].
 apply bind_inv in E as[u[sr[Ei E]]]. injection E as <- <- <-.
 destruct(ok_synth_call f c e pa a args _ _ _ Hi Ec)as[Ic Fc].
 destruct(ok_inject_check e vt exp _ _ _ Ic Ei)as[Ir Fi].
 assert(Hw:st_w s=w).
 { rewrite <-(fr_w _ _ (fr_trans _ _ _ Fc Fi));exact(lt_w _ _ _ _ _(proj1 Hp)). }
 pose proof(mono_group f)as Mon;do 11(apply proj2 in Mon);apply proj1 in Mon.
 destruct(synth_call_defined_exec f c e pa a args fid fnm ps rt body s v vt sc F C Ha
   (Mon c e pa a args s((v,vt),sc)Ec)Hi Hc Hw Resolve Kind Decl)as[-> [g[Hg[Id[D Code]]]]].
 split;[reflexivity|]. exists g,sc;split;[exact Hg|]. split;[exact Id|]. split;[exact D|].
 split;[exact Code|]. split;[exact Fi|]. exact(post_back w P R Fl _ _ F Fi Hp).
Qed.

Theorem defined_ready_evidence : forall f,OK w P R Fl f->
 forall c e g args s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 (forall k,gs_origin g<>OForwarder k)->~A.IsMainFn WW(gs_id g)->
 (forall s0 m s1,st_w s0=w->is_main g s0=Good(m,s1)->m=false)->
 (forall info si,fn_info g s=Good(info,si)->forall k,In k(fi_needs info)->key_present e(site_key e k)=true)->
 exists info si Cf sm sg xs,
 fn_info g s=Good(info,si)/\
 CtxOfIE WW(gs_id g)Cf(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cf(map(key_tr Fl)(fi_needs info))sm/\
 A.SigOf WW(gs_id g)(A.c_ty C)None(canonical_sig Fl e sg)/\
 A.Args WW P F C args(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))xs/\
 x=XCallD(gs_id g)(A.theta_at WW C(gs_id g))sm xs/\t=sg_ret sg/\WFty t/\Support WW t.
Proof.
 intros f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Nf Nm Nm' Ready.
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
 destruct(fn_info_satisfaction_nonroot w R Fl b Hload Finj _ _ _ _ _ _ _ _ _ _ _ _ _ Hg Nf Nm Nm'
   Ei En Es Hi Ii Hw Ls Hc)as[Cf[Ctx Sat]].
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
Theorem defined_ready_loaded : forall f,OK w P R Fl f->
 forall c e g args s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 ~A.IsMainFn WW(gs_id g)->
 (forall info si,fn_info g s=Good(info,si)->forall k,In k(fi_needs info)->key_present e(site_key e k)=true)->
 exists info si Cf sm sg xs,
 fn_info g s=Good(info,si)/\
 CtxOfIE WW(gs_id g)Cf(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cf(map(key_tr Fl)(fi_needs info))sm/\
 A.SigOf WW(gs_id g)(A.c_ty C)None(canonical_sig Fl e sg)/\
 A.Args WW P F C args(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))xs/\
 x=XCallD(gs_id g)(A.theta_at WW C(gs_id g))sm xs/\t=sg_ret sg/\WFty t/\Support WW t.
Proof.
 intros f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Nm Ready.
 exact(defined_ready_evidence f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl
   (loaded_no_forwarder b w g Hload Hg)Nm(not_main w R Fl b Hload g Hg Nm)Ready).
Qed.
Theorem defined_empty_loaded : forall f,OK w P R Fl f->
 forall c e g args s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 ~A.IsMainFn WW(gs_id g)->
 (exists info si ed sd,fn_info g s=Good(info,si)/\demand_needs f c e info si=Good(([],ed),sd))->
 exists info si Cf sm sg xs,
 fn_info g s=Good(info,si)/\
 CtxOfIE WW(gs_id g)Cf(map(key_tr Fl)(fi_needs info))/\
 A.Satisfies C Cf(map(key_tr Fl)(fi_needs info))sm/\
 A.SigOf WW(gs_id g)(A.c_ty C)None(canonical_sig Fl e sg)/\
 A.Args WW P F C args(map(fun p=>tr Fl(canon e(snd p)))(sg_params sg))xs/\
 x=XCallD(gs_id g)(A.theta_at WW C(gs_id g))sm xs/\t=sg_ret sg/\WFty t/\Support WW t.
Proof.
 intros f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Nm Empty.
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
 exact(defined_ready_loaded f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg Decl Nm Ready).
Qed.
Theorem defined_empty_ex:forall f,OK w P R Fl f->
 forall c e g pa args exp s x t s' F C n ps rt body,
 defined_call_code f c e(gs_id g)g args s=Good((x,t),s')->
 Inv s->InfoInv s->FwdInv s->CxRel w R Fl c e F C->Post w P R Fl s' F->
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFn RPlain n ps rt(Some body)->
 A.resolve_sym WW(A.fr_mod F)pa=Some(gs_id g)->A.kind_sym WW(gs_id g)=Some A.DKFn->
 ~A.IsMainFn WW(gs_id g)->
 (exists info si ed sd,fn_info g s=Good(info,si)/\demand_needs f c e info si=Good(([],ed),sd))->
 A.Ex WW P F C(AECall pa None args)exp x(tr Fl(canon e t))/\WFty t/\Support WW t.
Proof.
 intros f IH c e g pa args exp s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg D Hr Hk Nm Empty.
 destruct(defined_empty_loaded f IH c e g args s x t s' F C n ps rt body E Hi Ii Hfi Hc Hp Hg D Nm Empty)
 as(info&si&Cf&sm&sg&xs&Ei&Ctx&Sat&Sig&Args&->&->&Wt&St).
 split;[|split;assumption].
 eapply A.ExCallD with(Cf:=Cf)(ns:=map(key_tr Fl)(fi_needs info))(g:=canonical_sig Fl e sg).
 - exact Hr.
 - exact Hk.
 - exists n,ps,rt,body;unfold A.ast_of;rewrite(w_decl_rd w R Fl _ _ Hg);cbn;rewrite D;reflexivity.
 - apply ctx_of_ie_complete;exact Ctx.
 - exact Sat.
 - exact Sig.
 - exact Args.
Qed.
End Defined.
