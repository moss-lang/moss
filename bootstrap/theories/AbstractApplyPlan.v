(* All real abstract-application obligations before the result reader. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx ReifyData Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 Merge CtxWalk CtxMerge FunctorContext FunctorSatisfies NeedSatisfiesGap FunctorFixedExec
 ApplyAbstractExec MethodKeyTr ApplyThetaExec ApplyFreshExec NestingTypesExec FunctorInputDomain.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Plan.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Theorem abstract_application_plan:forall fuel c e F C g n names args res s pre e' c' sf,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 apply_abstract fuel c e g s=Good((pre,e',c'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 exists ef needs Cu fx rts gens th extra sm ss keys so,
 A.nlook(gs_id g)(A.c_funs C)=Some fx/\A.FunCtx W(gs_id g)Cu(map(key_tr Fl)needs)/\
 FunEnvTables Fl ef Cu/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty C)=Some here/\In(t,A.canon C here)(th++extra))/\
 (forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx)/\
 A.FixedAgreement W C(gs_id g)/\A.Satisfies C Cu(map(key_tr Fl)needs)sm/\
 NoDup rts/\(forall t,In t rts<->A.FunOutputs W(gs_id g)t)/\
 Forall2(GenRel w R Fl e)rts gens/\
 (forall t,A.FunOutputs W(gs_id g)t->
 (A.nlook t(A.c_ty C)=None/\generation_of gens t=t)\/
 (A.nlook t(A.c_ty C)<>None/\A.Fresh W(generation_of gens t)/\
 In(generation_of gens t)(map snd gens)/\In(t,TAbs(generation_of gens t))(th++extra)))/\
 (forall t,A.Mentions W(gs_id g)t->tn_get e t<>None)/\
 provide_result e g gens ss=Good((e',keys),so)/\Inv ss/\st_w ss=w/\Later w P R Fl so/\
 pre=[SApply(gs_id g)(th++extra)sm]/\c'=cx_with_fresh c(cx_fresh c++map snd gens).
Proof.
 intros fuel c e F C g n names args res s pre e' c' sf Wo Hg D E Hi Hw L HC.
 destruct(apply_abstract_phases _ _ _ _ _ _ _ _ _ E)
 as(ef&needs&sf0&xf&sx&sy&sz&pairs&sp&Av&Env&Fix&Guard&Input&Req&Tail).
 assert(Oe:ok(fun_env g))by(apply ElabFrameCore.ok_fun_env);destruct(Oe _ _ _ Hi Env)as[If Fenv].
 assert(Wf:st_w sf0=w)by(rewrite(fr_w _ _ Fenv);exact Hw).
 assert(Of:ok(functor_fixed g))by(unfold functor_fixed;okt);destruct(Of _ _ _ If Fix)as[Ix Ffix].
 destruct(ok_miter _ _ _ (ok_fixed_guard(cx_mod c)e g _) _ _ _ Ix Guard)as[Iy Fguard].
 destruct(ok_miter _ _ _ (ok_input_guard(cx_mod c)e g) _ _ _ Iy Input)as[Iz Finput].
 assert(Or:ok(require_needs(cx_mod c)e g(abstract_info ef needs)))by(unfold require_needs;okt).
 destruct(Or _ _ _ Iz Req)as[Ip Freq].
 assert(Wp:st_w sp=w)by(rewrite(fr_w _ _ Freq),(fr_w _ _ Finput),(fr_w _ _ Fguard),(fr_w _ _ Ffix);exact Wf).
 destruct(abstract_tail_phases _ _ _ _ _ _ _ _ _ _ _ Tail)
 as(rts&sr&gens&sg&th&st0&extra&st1&sm&ss&keys&so&Types&Gen&Theta&Extra&Sm&Provide&Meta&Pre&Ctx).
 destruct(result_types_rel w R Fl b Hload g n names args res None sp rts sr Hg D Types Ip Wp)as[-> [Nd Domain]].
 destruct(ok_mmap _ _ (output_generation e g)rts (ok_output_generation e g) _ _ _ Ip Gen)as[Ig Fgen].
 assert(Wg:st_w sg=w)by(rewrite(fr_w _ _ Fgen);exact Wp).
 assert(Oth:ok(mmap(input_argument e)(map fst(en_tn ef))))by(apply ok_mmap;apply ok_input_argument).
 destruct(Oth _ _ _ Ig Theta)as[It Ftheta].
 assert(Oextra:ok(output_arguments gens))by(unfold output_arguments;apply ok_mmap;intro;apply ok_ret).
 destruct(Oextra _ _ _ It Extra)as[Iex Fextra].
 assert(Osm:ok(smap_at ef e pairs)).
 {unfold smap_at;apply ok_mmap;intros[a z];apply ok_bind;[apply ok_key_item_at_frame|intro].
 apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Osm _ _ _ Iex Sm)as[Is Fsm].
 assert(Op:ok(provide_result e g gens))by(unfold provide_result;okt);destruct(Op _ _ _ Is Provide)as[Io Fprovide].
 assert(Om:ok(record_reify_event(REApply ety key(gs_id g)needs keys
 (map(fun t=>(t,canon e(opt_default YUnit(tn_get e t))))(map fst(en_tn ef)))
 (map(fun p=>(fst p,YAbs(snd p)))gens)(reify_scope_of e'))))by okt.
 destruct(Om _ _ _ Io Meta)as[_ Fmeta].
 pose proof(later_back w P R Fl _ _ Fmeta L)as Lo.
 pose proof(later_back w P R Fl _ _ Fprovide Lo)as Lss.
 pose proof(later_back w P R Fl _ _ Fsm Lss)as Lex.
 pose proof(later_back w P R Fl _ _ Fextra Lex)as Lt.
 pose proof(later_back w P R Fl _ _ Ftheta Lt)as Lg.
 pose proof(later_back w P R Fl _ _ Fgen Lg)as Lp.
 pose proof(later_back w P R Fl _ _ Freq Lp)as Lz.
 pose proof(later_back w P R Fl _ _ Finput Lz)as Ly.
 pose proof(later_back w P R Fl _ _ Fguard Ly)as Lx.
 pose proof(later_back w P R Fl _ _ Ffix Lx)as Lf.
 destruct(fun_env_context_valid w R Fl b Hload g n names args res s ef needs sf0 Wo Hg D Env Hi Hw Lf)
 as(Cu&FC&Tables&Ti&Keys&Edges).
 assert(Sat:A.Satisfies C Cu(map(key_tr Fl)needs)sm).
 {eapply(need_satisfies_tables_smap w R Fl b Hload)with(info:=abstract_info ef needs);
 [exact Req|exact Sm|exact Iz|exact Iex|exact Lss|exact HC|exact(fet_sub _ _ _ Tables)|exact Edges|
 exact Ti|reflexivity|exact Keys|exact(fet_apps _ _ _ Tables)]. }
 assert(Have:is_some(A.nlook(gs_id g)(A.c_funs C))=true)by(rewrite(cr_funspresent _ _ _ _ _ _ _ HC);exact Av).
 destruct(A.nlook(gs_id g)(A.c_funs C))as[fx|]eqn:Fu;[|discriminate Have].
 assert(Fixed:forall x tx,In(x,tx)fx->exists here,A.nlook x(A.c_ty C)=Some here/\A.canon C here=A.canon C tx).
 {eapply(actual_application_fixed w R Fl b Hload);[exact Wo|exact Hg|exact D|exact Env|exact Fix|exact Guard|exact Input|exact Hi|exact Hw|exact Lz|exact HC|exact Fu]. }
 assert(Agree:A.FixedAgreement W C(gs_id g)).
 {eapply(actual_recorded_fixed_agreement w R Fl b Hload);[exact Hg|exact D|exact Fix|exact Guard|exact If|exact Wf|exact Ly|exact HC]. }
 pose proof(application_theta_reads w R Fl c e F C ef Cu(cx_mod c)g sy sz sg th st0 HC Tables Input Theta Ig Wg Lt)as Reads.
 pose proof(output_generations_rel b w R Fl Hload e g rts sp gens sg Hg Gen Ip Wp Lg)as Generated.
 assert(Mentioned:forall t,A.Mentions W(gs_id g)t->tn_get e t<>None).
 {intros t Mention;pose proof(fun_ctx_mentions_available W(gs_id g)Cu(map(key_tr Fl)needs)
 (A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g))n names args res t
 (w_decl_read w R Fl _ _ Hg)ltac:(cbn[A.d_ast];exact D)FC Mention)as Present.
 destruct(A.nlook t(A.c_ty Cu))as[tt|]eqn:Tt;[|contradiction].
 destruct(Reads t tt Tt)as(here&Here&Hit);rewrite(cr_ty _ _ _ _ _ _ _ HC)in Here.
 destruct(tn_get e t);[discriminate|discriminate Here]. }
 exists ef,needs,Cu,fx,rts,gens,th,extra,sm,ss,keys,so.
 split;[reflexivity|];split;[exact FC|];split;[exact Tables|];split.
 - intros t tt Ht;destruct(Reads t tt Ht)as(here&Here&Hit);exists here;split;[exact Here|apply in_or_app;left;exact Hit].
 - split;[exact Fixed|];split;[exact Agree|];split;[exact Sat|];split;[exact Nd|];split;[exact Domain|].
   split;[exact Generated|];split.
   + intros t Ht;eapply generation_output_premise;[exact HC|exact Generated|exact Extra|apply Domain;exact Ht].
   + split;[exact Mentioned|];split;[exact Provide|];split;[exact Is|];split.
     * rewrite(fr_w _ _ Fsm),(fr_w _ _ Fextra),(fr_w _ _ Ftheta);exact Wg.
     * split;[exact Lo|];split;assumption.
Qed.
End Plan.
