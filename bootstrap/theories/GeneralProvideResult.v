(* General actual result installation: only real application input availability
   and the generated output domain are required. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTy
 CtxMerge NominalSupport NestingTypesExec ApplyFreshExec OutputInstallContext
 ProvideResultExec ProvideResultContext ProvideResultDomain ProvideResultFold
 FamilyResultFold ResultWalkExec ResultItemsFold.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Lemma result_output_map_old:forall C ts out t,
 A.nlook t(A.c_ty C)<>None->A.nlook t(A.output_type_map C ts out)<>None.
Proof.
 intros C ts out t Present;unfold A.output_type_map.
 rewrite family_normal_lookup,nlook_app.
 destruct(A.nlook t(map(fun r=>(r,out r))ts));[discriminate|].
 destruct(A.nlook t(A.c_ty C));[discriminate|contradiction].
Qed.
Lemma result_output_map_new:forall C ts out t,
 In t ts->A.nlook t(A.output_type_map C ts out)<>None.
Proof.
 intros C ts out t Hit;apply(output_nlook_member _ t(A.canon C(out t))).
 unfold A.output_type_map;apply in_map_iff;exists(t,out t);split;[reflexivity|].
 apply in_or_app;left;apply in_map_iff;exists t;split;[reflexivity|exact Hit].
Qed.
Section Result.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySoundCore.W w R Fl).
Theorem general_provide_result:forall c e F C g n names args res gens s e' keys sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 provide_result e g gens s=Good((e',keys),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 (forall t,A.Mentions W(gs_id g)t->tn_get e t<>None)->
 (forall t,A.FunOutputs W(gs_id g)t->nassoc t gens<>None)->
 exists C',A.ProvideResult W(gs_id g)C(fun t=>TAbs(generation_of gens t))C'(map(key_tr Fl)keys)/\
 CxRel w R Fl c e' F C'.
Proof.
 intros c e F C g n names args res gens s e' keys sf Hg D Run Hi Hw L HC Mentioned Generated.
 destruct(provide_result_phases e g gens s e' keys sf Run)as(ts&st&et&se&fuel&sn&Types&Install&Fuel&Walk).
 destruct(result_types_rel w R Fl b Hload g n names args res None s ts st Hg D Types Hi Hw)as[-> [Nd Domain]].
 assert(Oi:ok(mfold(install_output_type gens)e ts)).
 {apply ok_mfold;intros en t;unfold install_output_type;destruct(nassoc t gens);okt. }
 destruct(Oi _ _ _ Hi Install)as[Ie Fe].
 assert(We:st_w se=w)by(rewrite(fr_w _ _ Fe);exact Hw).
 assert(Of:ok walk_fuel)by okt.
 destruct(Of _ _ _ Ie Fuel)as[Inum Fn].
 assert(Wn:st_w sn=w)by(rewrite(fr_w _ _ Fn);exact We).
 assert(Total:forall t,In t ts->nassoc t gens<>None)by(intros t Hit;apply Generated;apply Domain;exact Hit).
 set(tm:=A.output_type_map C ts(fun t=>TAbs(generation_of gens t))).
 set(C0:=output_with_types C tm).
 destruct(output_install_context w R Fl ts gens c e F C s et se Install HC Total)as[HC0 Uf].
 change(CxRel w R Fl c et F C0)in HC0.
 assert(Stable:family_types_stable C0).
 {unfold family_types_stable;change(map(fun q=>(fst q,A.canon C(snd q)))tm=tm).
  unfold tm,A.output_type_map;rewrite map_map;apply map_ext;intros[k ty];cbn[fst snd].
  rewrite(output_context_canon_idem w R Fl _ _ _ _ HC);reflexivity. }
 assert(Init:A.output_cx C tm[]=C0).
 {unfold C0,A.output_cx;cbn[rev flat_map List.app].
  change(map(fun q=>(fst q,A.canon C(snd q)))tm=tm)in Stable.
  rewrite Stable;destruct C;reflexivity. }
 assert(Self:forall t,A.Mentions W(gs_id g)t->A.nlook t(A.c_ty C0)<>None).
 {intros t Ment;change(A.nlook t tm<>None);apply result_output_map_old.
  rewrite(cr_ty _ _ _ _ _ _ _ HC).
  destruct(tn_get e t)eqn:Nt;[discriminate|exfalso;exact(Mentioned t Ment Nt)]. }
 assert(Dom:forall sp,In sp res->forall t,A.ResultMentions W(gs_id g)(gm_path(get_mod w(gs_mod g)))sp t->A.nlook t([]++A.c_ty C0)<>None).
 {intros sp Hit t Ment;cbn[List.app];destruct(in_dec Nat.eq_dec t ts)as[Out|NotOut].
  - apply result_output_map_new;exact Out.
  - apply Self;exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,sp.
    split;[exact(w_decl_rd w R Fl _ _ Hg)|].
  split.
    + cbn[A.d_ast A.functor_parts];rewrite D;reflexivity.
    + split;[exact Hit|];split;[exact Ment|]. intro Out;apply NotOut;apply Domain;exact Out. }
 assert(Walk':mfold(fun acc sp=>provide_spec fuel(fst acc)g(gs_mod g)sp[](snd acc))(et,[])res sn=Good((e',keys),sf)).
 {unfold Elab.functor_parts in Walk;rewrite D in Walk;exact Walk. }
 destruct(proj2(result_walk w (P:=P)R Fl b Hload Finj(gs_id g)fuel)c et F C0 g(gs_mod g)res[][][]sn e' keys sf
  Walk' Dom Self Inum Wn(loader_mod_in w R Fl b Hload _ _ Hg)L HC0 Stable(Forall_nil _)(Forall_nil _)eq_refl)
 as(es&fresh&Sem&Keys&RK&Ctx).
 cbn[List.app]in Keys;subst fresh.
 assert(Final:A.output_cx C0(A.c_ty C0)es=A.output_cx C tm es).
 {change(A.output_cx C0 tm es=A.output_cx C tm es).
  rewrite <-Init,<-output_cx_append;reflexivity. }
 rewrite Final in Ctx.
 exists(A.output_cx C tm es);split;[|exact Ctx].
 exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),names,args,res,None,ts,es,tm.
 split;[exact(w_decl_rd w R Fl _ _ Hg)|].
  split.
 - cbn[A.d_ast A.functor_parts];rewrite D;reflexivity.
 - split;[exact Domain|].
  split;[exact Sem|].
  split;[exact RK|reflexivity].
Qed.
End Result.
