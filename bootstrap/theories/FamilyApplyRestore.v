(* Actual automatic family application, including temporary type restoration. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr
 BodyTy NominalSupport CanonTypes FamilyMatchingCtx FamilyApplyExec ApplyAbstractExec
 ApplyThetaExec MethodKeyTr FunctorFixedExec FamilyResultExec ProvideResultExec FamilyResultFold TySupport.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[FUEL reqs type_requirements].
Section FamilyRestore.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_abstract_frame:forall fuel c e g n names args res s pre e' c' sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_abstract fuel c e g s=Good((pre,e',c'),sf)->
 Inv s->st_w s=w->provision_frame e e'.
Proof.
 intros fuel c e g n names args res s pre e' c' sf Hg D Fam E Hi Hw.
 destruct(apply_abstract_phases _ _ _ _ _ _ _ _ _ E)
 as(ef&needs&ss&xf&sx&sy&sz&pairs&sp&Av&Env&Fix&Guard&Input&Req&Tail).
 assert(Oe:ok(fun_env g))by(apply ElabFrameCore.ok_fun_env).
 destruct(Oe _ _ _ Hi Env)as[If Fenv].
 assert(Of:ok(functor_fixed g))by(unfold functor_fixed;okt).
 destruct(Of _ _ _ If Fix)as[Ix Ffix].
 destruct(ok_miter _ _ _ (ok_fixed_guard(cx_mod c)e g _) _ _ _ Ix Guard)as[Iy Fguard].
 destruct(ok_miter _ _ _ (ok_input_guard(cx_mod c)e g) _ _ _ Iy Input)as[Iz Finput].
 assert(Or:ok(require_needs(cx_mod c)e g(abstract_info ef needs)))by(unfold require_needs;okt).
 destruct(Or _ _ _ Iz Req)as[Ip Freq].
 assert(Wp:st_w sp=w).
 {rewrite(fr_w _ _ Freq),(fr_w _ _ Finput),(fr_w _ _ Fguard),(fr_w _ _ Ffix),(fr_w _ _ Fenv);exact Hw. }
 destruct(abstract_tail_phases _ _ _ _ _ _ _ _ _ _ _ Tail)
 as(rts&sr&gens&sg&th&st0&extra&st1&sm&ssm&keys&so&Types&Gen&Theta&Extra&Sm&Provide&Meta&Pre&Ctx).
 destruct(family_result_types_empty w R Fl b Hload g n names args res sp rts sr Hg D Fam Types Ip Wp)as[-> ->].
 cbn[mmap]in Gen;injection Gen as <- <-.
 unfold output_arguments in Extra;cbn[filter mmap]in Extra;injection Extra as <- <-.
 assert(Oth:ok(mmap(input_argument e)(map fst(en_tn ef))))by(apply ok_mmap;apply ok_input_argument).
 destruct(Oth _ _ _ Ip Theta)as[It Ftheta].
 assert(Osm:ok(smap_at ef e pairs)).
 {unfold smap_at;apply ok_mmap;intros[a bb];apply ok_bind;[apply ok_key_item_at_frame|intro].
 apply ok_bind;[apply ok_key_item_at_frame|intro;apply ok_ret]. }
 destruct(Osm _ _ _ It Sm)as[Is Fsm].
 assert(Wss:st_w ssm=w)by(rewrite(fr_w _ _ Fsm),(fr_w _ _ Ftheta);exact Wp).
 destruct(family_provide_result_phases w R Fl b Hload g n names args res e [] ssm e' keys so
 Hg D Fam Provide Is Wss)as(f&ss0&Fuel&Walk).
 eapply provide_fold_frame;[|exact Walk].
 intros ea out sp0 sa eb outb sb Ex;cbn[fst snd]in Ex.
 eapply provide_spec_frame;exact Ex.
Qed.
Lemma apply_family_uf:forall fuel c e g n names args res matching wanted s pre e' sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_family fuel c e g matching wanted s=Good((pre,e'),sf)->
 Inv s->st_w s=w->en_uf e'=en_uf e.
Proof.
 intros[|fuel]c e g n names args res matching wanted s pre e' sf Hg D Fam E Hi Hw;[discriminate E|].
 cbn[apply_family]in E;apply bind_inv in E as(ei&si&Install&E).
 change(mfold family_install_step e matching s=Good(ei,si))in Install.
 apply bind_inv in E as([[xs er]cr]&sr&App&E).
 apply bind_inv in E as(mem&ss&Mem&E);apply bind_inv in E as([]&sf0&Flag&E);injection E as <- <- <-.
 assert(Oinstall:ok(mfold family_install_step e matching)).
 {apply ok_mfold;intro q;unfold family_install_step;okt. }
 destruct(Oinstall _ _ _ Hi Install)as[Ii Fi].
 destruct(family_abstract_frame fuel c ei g n names args res si xs er cr sr Hg D Fam App Ii
   ltac:(rewrite(fr_w _ _ Fi);exact Hw))as[Tn[Uf Lc]].
 change(en_uf er=en_uf e);rewrite Uf;eapply family_install_uf;exact Install.
Qed.
Theorem apply_family_context_entries:forall fuel c e F C g n names args res matching wanted s pre e' sf,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_family fuel c e g matching wanted s=Good((pre,e'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))matching->Forall(fun q=>Support W(snd q))matching->
 exists es ei si er sr emitted sm fx Cu ns Cr ks,
 let Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C in
 mfold family_install_step e matching s=Good(ei,si)/\
 apply_abstract (pred fuel)c ei g si=Good(([SApply(gs_id g)emitted sm],er,c),sr)/\
 pre=[SApply(gs_id g)emitted sm]/\
 CxRel w R Fl c ei F Ci/\
 A.nlook(gs_id g)(A.c_funs Ci)=Some fx/\A.FunCtx W(gs_id g)Cu ns/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty Ci)=Some here/\In(t,A.canon Ci here)emitted)/\
 A.FixedAgreement W Ci(gs_id g)/\A.Satisfies Ci Cu ns sm/\
 A.ProvideResult W(gs_id g)Ci(fun t=>TAbs t)Cr ks/\
 CxRel w R Fl c e' F(A.family_restore C Cr)/\
 A.OutputItemsEntries W(gm_path(get_mod w(gs_mod g)))[](family_result_tys Ci)res es(family_result_tys Ci)/\
 Forall(fun ent=>match ent with A.NMth rt _ _ _=>TySupport W rt|_=>True end)es/\
 A.ResultKeys(A.c_sub Ci)es ks/\Cr=A.output_cx Ci(family_result_tys Ci)es.
Proof.
 intros[|fuel]c e F C g n names args res matching wanted s pre e' sf Wo Hg D Fam E Hi Hw L HC Wm Sm;
 [discriminate E|].
 cbn[apply_family]in E;apply bind_inv in E as(ei&si&Install&E).
 change(mfold family_install_step e matching s=Good(ei,si))in Install.
 apply bind_inv in E as([[xs er]cr]&sr&App&E).
 apply bind_inv in E as(mem&ss&Mem&E).
 apply bind_inv in E as([]&sf0&Flag&E);injection E as <- <- <-.
 assert(Oinstall:ok(mfold family_install_step e matching)).
 {apply ok_mfold;intro q;unfold family_install_step;okt. }
 destruct(Oinstall _ _ _ Hi Install)as[Ii Fi].
 destruct(ok_apply_abstract fuel c ei g _ _ _ Ii App)as[Ir Fr].
 assert(Om:ok(is_memories g))by(unfold is_memories;okt).
 destruct(Om _ _ _ Ir Mem)as[Im Fm].
 assert(Of:ok(if mem then upd(s_memory false true)else ret tt))by(destruct mem;okt).
 destruct(Of _ _ _ Im Flag)as[If Ff].
 pose proof(later_back w P R Fl _ _ Ff L)as Lm.
 pose proof(later_back w P R Fl _ _ Fm Lm)as Lr.
 destruct(family_matching_install w R Fl matching c e F C s ei si Install HC Wm Sm)as[Ui HCi].
 set(Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C)in *.
 assert(Wi:st_w si=w)by(rewrite(fr_w _ _ Fi);exact Hw).
 destruct(family_application_entries w R Fl b Hload Finj fuel c ei F Ci g n names args res si xs er cr sr
 Wo Hg D Fam App Ii Wi Lr HCi)
 as(es&fx&Cu&ns&emitted&smap&Cr&ks&Pre&Ctx&Fu&FC&Reads&Fixed&Agree&Sat&Result&HR&Sem&Supported&RK&Final).
 rewrite Ctx in App,HR.
 assert(Frame:provision_frame ei er)by(eapply family_abstract_frame;eauto).
 destruct Frame as[Tn[Ur Loc]].
 assert(Out:forall x,~In x(map fst matching)->nassoc x(en_tn er)=nassoc x(en_tn e)).
 {intros x Hx;rewrite Tn;eapply family_install_outside;[exact Install|exact Hx]. }
 assert(Rest:CxRel w R Fl c(restore_tn er e(map fst matching))F(A.family_restore C Cr)).
 {eapply family_restore_cxrel;[exact HC|exact HR|rewrite Ur;exact Ui|exact Out]. }
 exists es,ei,si,er,sr,emitted,smap,fx,Cu,ns,Cr,ks;cbn zeta.
 split;[exact Install|];split;[rewrite <-Pre;exact App|];split;[exact Pre|].
 split;[exact HCi|];split;[exact Fu|];split;[exact FC|];split;[exact Reads|].
 split;[exact Agree|];split;[exact Sat|];split;[exact Result|];split;[exact Rest|].
 split;[exact Sem|];split;[exact Supported|];split;assumption.
Qed.
Theorem apply_family_context:forall fuel c e F C g n names args res matching wanted s pre e' sf,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_family fuel c e g matching wanted s=Good((pre,e'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))matching->Forall(fun q=>Support W(snd q))matching->
 exists ei si er sr emitted sm fx Cu ns Cr ks,
 let Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C in
 mfold family_install_step e matching s=Good(ei,si)/\
 apply_abstract (pred fuel)c ei g si=Good(([SApply(gs_id g)emitted sm],er,c),sr)/\
 pre=[SApply(gs_id g)emitted sm]/\
 CxRel w R Fl c ei F Ci/\
 A.nlook(gs_id g)(A.c_funs Ci)=Some fx/\A.FunCtx W(gs_id g)Cu ns/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty Ci)=Some here/\In(t,A.canon Ci here)emitted)/\
 A.FixedAgreement W Ci(gs_id g)/\A.Satisfies Ci Cu ns sm/\
 A.ProvideResult W(gs_id g)Ci(fun t=>TAbs t)Cr ks/\
 CxRel w R Fl c e' F(A.family_restore C Cr).
Proof.
 intros fuel c e F C g n names args res matching wanted s pre e' sf Wo Hg D Fam E Hi Hw L HC Wm Sm.
 destruct(apply_family_context_entries fuel c e F C g n names args res matching wanted s pre e' sf Wo Hg D Fam E Hi Hw L HC Wm Sm)
 as(es&ei&si&er&sr&emitted&sm&fx&Cu&ns&Cr&ks&Install&App&Pre&HCi&Fu&FC&Reads&Agree&Sat&Result&Rest&Sem&Supported&RK&Final).
 exists ei,si,er,sr,emitted,sm,fx,Cu,ns,Cr,ks;cbn zeta in *.
 split;[exact Install|];split;[exact App|];split;[exact Pre|];split;[exact HCi|].
 split;[exact Fu|];split;[exact FC|];split;[exact Reads|];split;[exact Agree|].
 split;[exact Sat|];split;assumption.
Qed.
End FamilyRestore.
