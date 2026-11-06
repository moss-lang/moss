(* A collected selection remains a valid family step after earlier groups. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr BodyTy
 NominalSupport SourceSupport Merge CanonTypes RelFacts FamilyOnly FamilySelectTrace FamilyMatchingCtx
 FamilyApplyRestore FamilySelectedOccurrence FamilyProducedKey FamilyResultFold FamilyResultFrame
 FamilyMarkerSpecialize TySupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_set_types_sub:forall xs C,
 A.c_sub(fold_left(fun C q=>A.set_ty C(fst q)(snd q))xs C)=A.c_sub C.
Proof. intro xs;induction xs as[|q xs IH];intro C;[reflexivity|cbn[fold_left];rewrite IH;reflexivity]. Qed.
Lemma family_set_types_ext:forall xs C D,A.c_ty C=A.c_ty D->
 A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))xs C)=
 A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))xs D).
Proof. intro xs;induction xs as[|q xs IH];intros C D H;[exact H|cbn[fold_left];apply IH;cbn[A.set_ty A.c_ty];rewrite H;reflexivity]. Qed.
Lemma family_matching_uf:forall Fl e e0 matching,en_uf e=en_uf e0->
 family_matching Fl e matching=family_matching Fl e0 matching.
Proof. intros;unfold family_matching;apply map_ext;intros[k y];cbn[fst snd];rewrite(canon_uf e0 e H);reflexivity. Qed.
Lemma family_result_matching_target:forall Fl e matching C,
 family_result_tys(fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C)=
 family_semantic_target C(family_matching Fl e matching).
Proof. intros;unfold family_result_tys,family_semantic_target,smap;apply map_ext;intro q;unfold A.canon;rewrite family_set_types_sub;reflexivity. Qed.
Section Group.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Notation W:=(BodySound.W w R Fl).
Theorem family_demand_group:forall f c e0 F C0 e C mi r mid s g matching ss sa pre e' sf rawWanted wanted sc,
 A.WorldOf b W->check_contexts sc=Good(tt,sc)->st_w sc=w->
 family_member mi e0 r mid s=Good(Some(g,matching),ss)->
 Inv s->st_w s=w->Later w P R Fl ss->CxRel w R Fl c e0 F C0->
 WFty r->Support W r->canon e0 r=r->get_mth e0 r mid=None->
 In(A.mkey(tr Fl r)mid)wanted->
 apply_family f c e g matching rawWanted sa=Good((pre,e'),sf)->
 Inv sa->st_w sa=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 A.c_ty C=A.c_ty C0->A.c_sub C=A.c_sub C0->A.c_funs C=A.c_funs C0->en_uf e=en_uf e0->
 exists full sm C',pre=[SApply(gs_id g)full sm]/\
 A.FamilyStep W C0 wanted C(gs_id g)(family_matching Fl e0 matching)full sm C'/\
 CxRel w R Fl c e' F C'/\A.c_ty C'=A.c_ty C0/\A.c_sub C'=A.c_sub C0/\A.c_funs C'=A.c_funs C0/\en_uf e'=en_uf e0.
Proof.
 intros f c e0 F C0 e C mi r mid s g matching ss sa pre e' sf rawWanted wanted sc
 Wo Checked Wsc Select Hi Hw Ls HC0 Wr Sr Can Miss Wanted App Ia Wa L HC Ty Sub Funs Uf.
 destruct(family_member_substitution w R Fl b Hload c e0 F C0 mi r mid s g matching ss HC0 Select Hi Hw Ls Wr Sr)
 as[[Hg Fam][Wm[Sm[Nd Poly]]]].
 pose proof(family_member_only w R Fl b Hload c e0 F C0 mi r mid s g matching ss sc
 Finj Wo Checked Wsc HC0 Select Hi Hw Ls Wr Sr Can)as Only.
 destruct(proj1 Fam)as(d&n&names&args&res&Decl&D&Shape).
 change(A.w_decl(WorldSound.W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl g=ADFunctor n names args res None)in D.
 destruct(apply_family_context_entries w R Fl b Hload Finj f c e F C g n names args res matching rawWanted
 sa pre e' sf Wo Hg D Fam App Ia Wa L HC Wm Sm)
 as(es&ei&si&er&sr&full&sm&fx&Cu&ns&Cr&ks&Install&Inner&Pre&HCi&Fu&FC&Reads&Agree&Sat&Result&HR&Sem&Supported&RK&Final).
 set(Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C)in *.
 assert(Target:family_result_tys Ci=family_semantic_target C0(family_matching Fl e0 matching)).
 {unfold Ci;rewrite family_result_matching_target,(family_matching_uf Fl e e0 matching Uf).
  unfold family_semantic_target;rewrite Sub,(family_set_types_ext _ C C0 Ty);reflexivity. }
 assert(Occurrence:exists sg ap,In(A.NMth(tr Fl r)mid sg ap)es).
 {eapply(family_selected_occurrence w R Fl b Hload);[exact Wo|exact Select|exact Hi|exact Hw|exact Ls|exact HC0|exact Wr|exact Sr|exact Can| |exact Supported].
  rewrite <-Target;unfold functor_parts;rewrite D;exact Sem. }
 destruct Occurrence as(sg&ap&Entry).
 assert(CanKey:A.canon Ci(tr Fl r)=tr Fl r).
 {unfold A.canon;unfold Ci;rewrite family_set_types_sub,Sub,(cr_sub _ _ _ _ _ _ _ HC0),tr_canon by exact(proj1(cr_uf _ _ _ _ _ _ _ HC0));rewrite Can;reflexivity. }
 assert(Key:In(A.mkey(tr Fl r)mid)ks).
 {unfold A.ResultKeys in RK;rewrite RK;apply in_flat_map;exists(A.NMth(tr Fl r)mid sg ap);split;[exact Entry|].
  cbn[A.entry_key];change(In(A.mkey(tr Fl r)mid)[A.mkey(A.canon Ci(tr Fl r))mid]);rewrite CanKey;left;reflexivity. }
 assert(FuC:A.nlook(gs_id g)(A.c_funs C)=Some fx).
 {unfold Ci in Fu;rewrite fold_set_ty_funs in Fu;exact Fu. }
 assert(Missing:A.mlook(tr Fl r)mid(A.c_mths C0)=None).
 {assert(Clean:ety_ok r=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Sr Wr).
  pose proof(cr_mths _ _ _ _ _ _ _ HC0 r mid ltac:(rewrite Can;exact Clean))as M;cbn beta in M;rewrite Can,Miss in M;exact M. }
 exists full,sm,(A.family_restore C Cr);split;[exact Pre|];split.
 - eapply A.FamilyStepApply with(ot:=tr Fl r)(m:=mid)(fx:=fx)(Cu:=Cu)(nsu:=ns)(Cr:=Cr)(ks:=ks).
   + exact Wanted.
   + exact Missing.
   + rewrite <-Funs;exact FuC.
   + exact FuC.
   + exact Fam.
   + exact Only.
   + apply family_matching_poly;exact Poly.
   + exact FC.
   + intros t Pt Present;destruct(A.nlook t(A.c_ty Cu))as[tt|]eqn:E;[|contradiction].
     destruct(Reads t tt E)as(here&Have&Hit);exists(A.canon Ci here);exact Hit.
   + rewrite <-(family_matching_uf Fl e e0 matching Uf);exact Reads.
   + rewrite <-(family_matching_uf Fl e e0 matching Uf);exact Agree.
   + rewrite <-(family_matching_uf Fl e e0 matching Uf);exact Sat.
   + rewrite <-(family_matching_uf Fl e e0 matching Uf);exact Result.
   + exact Key.
 - split;[exact HR|];split;[exact Ty|];split.
   + change(A.c_sub Cr=A.c_sub C0).
     rewrite(proj1(family_result_frame W(gs_id g)Ci(fun t=>TAbs t)Cr ks Fam Result)).
     unfold Ci;rewrite family_set_types_sub;exact Sub.
   + split.
     * destruct(family_restore_frame W(gs_id g)C Ci Cr(fun t=>TAbs t)ks Fam Result)as[FuR _].
   rewrite FuR;unfold Ci;rewrite fold_set_ty_funs;exact Funs.
     * rewrite <-(Uf);eapply(apply_family_uf w R Fl b Hload);eassumption.
Qed.
End Group.
