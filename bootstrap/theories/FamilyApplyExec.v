(* Actual abstract family application: inputs, fixed captures and results. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReifyData ElabFrameCore BodySoundCore BodyTr BodyTy
 Merge CtxWalk CtxMerge FunctorContext FunctorSatisfies NeedSatisfiesGap FunctorFixedExec ApplyAbstractExec
 ApplyThetaExec MethodKeyTr FamilyShapeExec FamilyResultExec FamilyProvideResult FamilyResultFold TySupport.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[FUEL reqs type_requirements ety_ty ety_ty_go].
Section Apply.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_application_entries:forall fuel c e F C g n names args res s pre e' c' s',
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_abstract fuel c e g s=Good((pre,e',c'),s')->
 Inv s->st_w s=w->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists es fx Cu ns th sm C' ks,
 pre=[SApply(gs_id g)th sm]/\c'=c/\A.nlook(gs_id g)(A.c_funs C)=Some fx/\
 A.FunCtx W(gs_id g)Cu ns/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty C)=Some here/\In(t,A.canon C here)th)/\
 (forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx)/\
 A.FixedAgreement W C(gs_id g)/\A.Satisfies C Cu ns sm/\
 A.ProvideResult W(gs_id g)C(fun t=>TAbs t)C' ks/\CxRel w R Fl c' e' F C'/\
 A.OutputItemsEntries W(gm_path(get_mod w(gs_mod g)))[](family_result_tys C)res es(family_result_tys C)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 A.ResultKeys(A.c_sub C)es ks/\C'=A.output_cx C(family_result_tys C)es.
Proof.
 intros fuel c e F C g n names args res s pre e' c' s' Wo Hg D Fam E Hi Hw L HC.
 destruct(apply_abstract_phases _ _ _ _ _ _ _ _ _ E)
 as(ef&needs&sf&xf&sx&sy&sz&pairs&sp&Av&Env&Fix&Guard&Input&Req&Tail).
 assert(Oe:ok(fun_env g))by(unfold fun_env;okt).
 destruct(Oe _ _ _ Hi Env)as[If Fenv].
 assert(Wf:st_w sf=w)by(rewrite(fr_w _ _ Fenv);exact Hw).
 assert(Of:ok(functor_fixed g))by(unfold functor_fixed;okt).
 destruct(Of _ _ _ If Fix)as[Ix Ffix].
 destruct(ok_miter _ _ _ (ok_fixed_guard(cx_mod c)e g _) _ _ _ Ix Guard)as[Iy Fguard].
 destruct(ok_miter _ _ _ (ok_input_guard(cx_mod c)e g) _ _ _ Iy Input)as[Iz Finput].
 assert(Or:ok(require_needs(cx_mod c)e g(abstract_info ef needs)))by(unfold require_needs;okt).
 destruct(Or _ _ _ Iz Req)as[Ip Freq].
 assert(Wp:st_w sp=w)by(rewrite(fr_w _ _ Freq),(fr_w _ _ Finput),(fr_w _ _ Fguard),(fr_w _ _ Ffix);exact Wf).
 destruct(abstract_tail_phases _ _ _ _ _ _ _ _ _ _ _ Tail)
 as(rts&sr&gens&sg&th&st0&extra&st1&sm&ss&keys&so&Types&Gen&Theta&Extra&Sm&Provide&Meta&Pre&Ctx).
 destruct(family_result_types_empty w R Fl b Hload g n names args res sp rts sr Hg D Fam Types Ip Wp)as[-> ->].
 cbn[mmap]in Gen;injection Gen as <- <-.
 unfold output_arguments in Extra;cbn[filter mmap]in Extra;injection Extra as <- <-.
 rewrite app_nil_r in Pre;cbn[map]in Ctx;rewrite app_nil_r in Ctx.
 assert(Csame:c'=c)by(rewrite Ctx;destruct c;reflexivity).
 assert(Oth:ok(mmap(input_argument e)(map fst(en_tn ef))))by(apply ok_mmap;apply ok_input_argument).
 destruct(Oth _ _ _ Ip Theta)as[It Ftheta].
 assert(Osm:ok(smap_at ef e pairs)).
 {unfold smap_at;apply ok_mmap;intros[a b0];apply ok_bind;[apply ok_key_item_at_frame|intro].
 apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Osm _ _ _ It Sm)as[Is Fsm].
 assert(Op:ok(provide_result e g []))by(unfold provide_result;okt).
 destruct(Op _ _ _ Is Provide)as[Io Fprovide].
 assert(Om:ok(record_reify_event(REApply ety key(gs_id g)needs keys
 (map(fun t=>(t,canon e(opt_default YUnit(tn_get e t))))(map fst(en_tn ef)))[](reify_scope_of e'))))by okt.
 destruct(Om _ _ _ Io Meta)as[_ Fmeta].
 pose proof(later_back w P R Fl _ _ Fmeta L)as Lo.
 pose proof(later_back w P R Fl _ _ Fprovide Lo)as Ls.
 pose proof(later_back w P R Fl _ _ Fsm Ls)as Lt.
 pose proof(later_back w P R Fl _ _ Ftheta Lt)as Lp.
 pose proof(later_back w P R Fl _ _ Freq Lp)as Lz.
 pose proof(later_back w P R Fl _ _ Finput Lz)as Ly.
 pose proof(later_back w P R Fl _ _ Fguard Ly)as Lx.
 pose proof(later_back w P R Fl _ _ Ffix Lx)as Lf.
 destruct(fun_env_context_valid w R Fl b Hload g n names args res s ef needs sf Wo Hg D Env Hi Hw Lf)
 as(Cu&FC&Tables&Ti&Keys&Edges).
 assert(Sat:A.Satisfies C Cu(map(key_tr Fl)needs)sm).
 {eapply(need_satisfies_tables_smap w R Fl b Hload)with(info:=abstract_info ef needs);
 [exact Req|exact Sm|exact Iz|exact It|exact Ls|exact HC|exact(fet_sub _ _ _ Tables)|exact Edges|
 exact Ti|reflexivity|exact Keys|exact(fet_apps _ _ _ Tables)]. }
 assert(Have:is_some(A.nlook(gs_id g)(A.c_funs C))=true).
 {rewrite(cr_funspresent _ _ _ _ _ _ _ HC);exact Av. }
 destruct(A.nlook(gs_id g)(A.c_funs C))as[fx|]eqn:Fu;[|discriminate Have].
 assert(Fixed:forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx).
 {eapply(actual_application_fixed w R Fl b Hload);[exact Wo|exact Hg|exact D|exact Env|exact Fix|exact Guard|exact Input|exact Hi|exact Hw|exact Lz|exact HC|exact Fu]. }
 assert(Agree:A.FixedAgreement W C(gs_id g)).
 {eapply(actual_recorded_fixed_agreement w R Fl b Hload);[exact Hg|exact D|exact Fix|exact Guard|exact If|exact Wf|exact Ly|exact HC]. }
 assert(Wss:st_w ss=w)by(rewrite(fr_w _ _ Fsm),(fr_w _ _ Ftheta);exact Wp).
 destruct(family_provide_result_entries w R Fl b Hload Finj c e F C g n names args res [](fun t=>TAbs t)ss e' keys so
 Hg D Fam Provide Is Wss Lo HC)as(es&Sem&Supported&RK&HC').
 set(C':=A.output_cx C(family_result_tys C)es).
 assert(Result:A.ProvideResult W(gs_id g)C(fun t=>TAbs t)C'(map(key_tr Fl)keys)).
 {exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,[],es,(family_result_tys C).
 split;[exact(w_decl_read w R Fl _ _ Hg)|].
 split;[cbn[A.d_ast A.functor_parts];rewrite D;reflexivity|].
 split;[intro t;split;[intros[]|intro H;exfalso;exact(family_no_outputs W(gs_id g)Fam t H)]|].
 split;[exact Sem|];split;[exact RK|reflexivity]. }
 exists es,fx,Cu,(map(key_tr Fl)needs),th,sm,C',(map(key_tr Fl)keys).
 split;[exact Pre|];split;[exact Csame|];split;[reflexivity|];split;[exact FC|].
 split.
 - eapply(application_theta_reads w R Fl);[exact HC|exact Tables|exact Input|exact Theta|exact Ip|exact Wp|exact Lt].
 - split;[exact Fixed|];split;[exact Agree|];split;[exact Sat|];split;[exact Result|];split;[rewrite Csame;exact HC'|].
   split;[exact Sem|];split;[exact Supported|];split;[exact RK|reflexivity].
Qed.
Theorem family_application:forall fuel c e F C g n names args res s pre e' c' s',
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_abstract fuel c e g s=Good((pre,e',c'),s')->
 Inv s->st_w s=w->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists fx Cu ns th sm C' ks,
 pre=[SApply(gs_id g)th sm]/\c'=c/\A.nlook(gs_id g)(A.c_funs C)=Some fx/\
 A.FunCtx W(gs_id g)Cu ns/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty C)=Some here/\In(t,A.canon C here)th)/\
 (forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx)/\
 A.FixedAgreement W C(gs_id g)/\A.Satisfies C Cu ns sm/\
 A.ProvideResult W(gs_id g)C(fun t=>TAbs t)C' ks/\CxRel w R Fl c' e' F C'.
Proof.
 intros fuel c e F C g n names args res s pre e' c' s' Wo Hg D Fam E Hi Hw L HC.
 destruct(family_application_entries fuel c e F C g n names args res s pre e' c' s' Wo Hg D Fam E Hi Hw L HC)
 as(es&fx&Cu&ns&th&sm&C'&ks&Pre&Ctx&Fu&FC&Reads&Fixed&Agree&Sat&Result&Ctx'&Sem&Supp&RK&Out).
 exists fx,Cu,ns,th,sm,C',ks.
 split;[exact Pre|];split;[exact Ctx|];split;[exact Fu|];split;[exact FC|].
 split;[exact Reads|];split;[exact Fixed|];split;[exact Agree|];split;[exact Sat|];split;assumption.
Qed.
Theorem family_bind_item:forall fuel c e F C pa g n names args res s pre e' c' s',
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->A.resolve_sym W(A.fr_mod F)pa=Some(gs_id g)->
 apply_abstract fuel c e g s=Good((pre,e',c'),s')->
 Inv s->st_w s=w->Later w P R Fl s'->CxRel w R Fl c e F C->
 exists th sm C',pre=[SApply(gs_id g)th sm]/\
 A.BindItem W P F C(Spec(Some pa)None None,None)(XApply(gs_id g)th sm)C' F[]/\
 CxRel w R Fl c' e' F C'.
Proof.
 intros fuel c e F C pa g n names args res s pre e' c' s' Wo Hg D Fam Resolve E Hi Hw L HC.
 destruct(family_application fuel c e F C g n names args res s pre e' c' s' Wo Hg D Fam E Hi Hw L HC)
 as(fx&Cu&ns&th&sm&C'&ks&Pre&Ctx&Fu&FC&Reads&Fixed&Agree&Sat&Result&HC').
 exists th,sm,C';split;[exact Pre|];split;[|exact HC'].
 assert(Frame:A.with_fresh F[]=F).
 {unfold A.with_fresh;rewrite app_nil_r;destruct F;reflexivity. }
 rewrite <-Frame at 2.
 eapply A.BApplyAbstract with(fx:=fx)(Cu:=Cu)(ns:=ns)(gensof:=fun t=>t)(ks:=ks);
 [exact Resolve|exact Fu|exact FC|exact Reads|exact Fixed|exact Sat| |exact Result].
 intros t Hout;exfalso;exact(family_no_outputs W(gs_id g)Fam t Hout).
Qed.
End Apply.
