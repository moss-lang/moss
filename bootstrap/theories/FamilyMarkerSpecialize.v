(* The actual family parameters and markers supply the full semantic specialization. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore BodySoundCore BodyTr BodyTy
 TyOfSubst SourceReadingExtend FamilyKeysExec FamilyMarkersExec FamilyMarkerContext FamilyMarkedSubst ProvideResultContext FunctorScopeTypes FunctorContext NestingTypesExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_semantic_target(C:A.cx)(matching:subst):list(nat*ty):=
 smap(A.c_sub C)(A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)).
Section Specialize.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Lemma family_params_types:forall g n names args res s ps sf,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 family_params g s=Good(ps,sf)->Inv s->st_w s=w->Later w P R Fl sf->
 forall t,In t ps->A.kind_sym W t=Some A.DKType.
Proof.
 intros g n names args res s ps sf Wo Hg D E Hi Hw L.
 unfold family_params in E;apply bind_inv in E as(opt&sp&Params&E).
 destruct names as[names|].
 - destruct opt as[keys|].
   2:{unfold functor_params,functor_brackets in Params;rewrite D in Params;
     apply bind_inv in Params as(ks&ss&Map&Bad);discriminate Bad. }
   destruct(functor_params_names w R Fl b Hload g n names args res None s keys sp Hg D Params Hi Hw)as[-> Names].
   injection E as <- <-.
   clear -Names;induction Names as[|name key names keys[Res Kind]Names IH];intros t Hin;[contradiction|].
   destruct Hin as[<-|Hin];[exact Kind|exact(IH t Hin)].
 - unfold functor_params,functor_brackets in Params;rewrite D in Params;injection Params as <- <-.
   eapply actual_result_mentions_types;[exact Hload|exact Wo|exact Hg|exact D|exact E|exact Hi|exact Hw|exact L].
Qed.
Theorem family_marker_completion_types:forall c e receiver F C g n names args res si ps sp marks sm target,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 family_params g si=Good(ps,sp)->
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps sp=Good(marks,sm)->
 Inv si->st_w si=w->Later w P R Fl sm->CxRel w R Fl c e F C->
 @TypeExtension W target(overlay_missing(family_marker_types marks)target).
Proof.
 intros c e receiver F C g n names args res si ps sp marks sm target Wo Hg D Params Alloc Hi Hw L HC.
 assert(Op:ok(family_params g))by(unfold family_params,functor_params;okt).
 destruct(Op _ _ _ Hi Params)as[Ip Fp].
 assert(Oalloc:ok(mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps)).
 {apply ok_mmap;intro p;apply ok_bind;[apply ok_fresh_marker_for|intro;apply ok_ret]. }
 destruct(Oalloc _ _ _ Ip Alloc)as[Im Falloc].
 pose proof(family_params_types g n names args res si ps sp Wo Hg D Params Hi Hw(later_back w P R Fl _ _ Falloc L))as Types.
 pose proof(FamilyMarkersExec.family_marker_keys e receiver ps sp marks sm Alloc)as Keys.
 apply family_marker_types_domain;rewrite Forall_forall;intros[p marker]Hin;apply Types.
 rewrite <-Keys;apply in_map_iff;exists(p,marker);split;[reflexivity|exact Hin].
Qed.
Theorem family_marker_specializes:forall c e receiver F C g n names args res matching si ps sp marks sm,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->family_params g si=Good(ps,sp)->
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps sp=Good(marks,sm)->
 Inv si->st_w si=w->Later w P R Fl sm->CxRel w R Fl c e F C->
 Forall(fun q=>A.Poly W(gs_id g)(fst q))matching->
 let target:=family_semantic_target C matching in
 SubRel(family_marker_subst marks target)
  (A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))marks C))
  (overlay_missing(family_marker_types marks)target).
Proof.
 intros c e receiver F C g n names args res matching si ps sp marks sm Hg D Fam Params Alloc Hi Hw L HC All;cbn zeta.
 assert(Op:ok(family_params g))by(unfold family_params,functor_params;okt).
 destruct(Op _ _ _ Hi Params)as[Ip Fp].
 assert(Oalloc:ok(mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps)).
 {apply ok_mmap;intro p;apply ok_bind;[apply ok_fresh_marker_for|intro;apply ok_ret]. }
 destruct(Oalloc _ _ _ Ip Alloc)as[Im Falloc].
 pose proof(family_params_poly w (P:=P)R Fl b Hload g n names args res si ps sp Hg D(proj1 Fam)
 Params Hi Hw(later_back w P R Fl _ _ Falloc L))as Poly.
 pose proof(FamilyMarkersExec.family_marker_keys e receiver ps sp marks sm Alloc)as Keys.
 rewrite family_marker_cx_types;apply family_marked_subrel.
 - exact(proj1(family_marker_allocation e receiver ps sp marks sm Alloc)).
 - intros k y Read;eapply family_marker_subst_fixes_caller;[exact Alloc|exact HC|exact Read].
 - unfold family_semantic_target;apply family_matching_target_outside.
   + rewrite Forall_forall in All|-*;intros q Hq;rewrite Keys;apply Poly;exact(All q Hq).
   + intros k y Read;exact(output_context_type_stable w R Fl c e F C k y HC Read).
Qed.
End Specialize.
