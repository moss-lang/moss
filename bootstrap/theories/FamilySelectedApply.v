(* Actual selection followed by application supplies every family-call premise. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy Merge CanonTypes
 NominalSupport TySupport SourceSupport RelFacts ProvideResultContext FamilySelectTrace FamilyMatchingCtx
 FamilyApplyRestore FamilyProducedKey FamilyResultFrame FamilyOnly.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Selected.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall x y,Fl x=Fl y->x=y.
Notation W:=(BodySoundCore.W w R Fl).
Theorem family_selected_application:forall f c e F C mi r mid s g matching si sa wanted pre e' sf sg sc,
 A.WorldOf b W->check_contexts sc=Good(tt,sc)->st_w sc=w->
 family_member mi e r mid s=Good(Some(g,matching),si)->Fr si sa->
 apply_family f c e g matching wanted sa=Good((pre,e'),sf)->
 Inv s->st_w s=w->Inv sa->Later w P R Fl sf->CxRel w R Fl c e F C->
 WFty r->Support W r->canon e r=r->get_mth e r mid=None->get_mth e' r mid=Some sg->
 exists fx emitted sm Cu ns Cr ks,
 let matchtypes:=family_matching Fl e matching in
 let Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))matchtypes C in
 pre=[SApply(gs_id g)emitted sm]/\A.nlook(gs_id g)(A.c_funs C)=Some fx/\A.Family W(gs_id g)/\
 Forall(fun q=>A.Poly W(gs_id g)(fst q))matchtypes/\A.FunCtx W(gs_id g)Cu ns/\
 (forall t,A.Poly W(gs_id g)t->A.nlook t(A.c_ty Cu)<>None->exists ty,In(t,ty)emitted)/\
 (forall t tt,A.nlook t(A.c_ty Cu)=Some tt->exists here,A.nlook t(A.c_ty Ci)=Some here/\In(t,A.canon Ci here)emitted)/\
 A.FixedAgreement W Ci(gs_id g)/\A.Satisfies Ci Cu ns sm/\A.ProvideResult W(gs_id g)Ci(fun t=>TAbs t)Cr ks/\
 In(A.mkey(tr Fl r)mid)ks/\A.OnlyFamily W C(gs_id g)(A.mkey(tr Fl r)mid)/\
 CxRel w R Fl c e' F(A.family_restore C Cr).
Proof.
 intros f c e F C mi r mid s g matching si sa wanted pre e' sf sg sc Wo Checked Wsc Select Gap App Hi Hw Ia L HC Wr Sr Can Miss Found.
 assert(Os:ok(family_member mi e r mid))by(unfold family_member;okt).
 destruct(Os _ _ _ Hi Select)as[Ii Fi].
 destruct(ok_apply_family f c e g matching wanted _ _ _ Ia App)as[If Fa].
 pose proof(later_back w P R Fl _ _ Fa L)as La.
 pose proof(later_back w P R Fl _ _ Gap La)as Li.
 assert(Wa:st_w sa=w)by(exact(fr_w' w _ _ Gap(fr_w' w _ _ Fi Hw))).
 destruct(family_member_substitution w R Fl b Hload c e F C mi r mid s g matching si HC Select Hi Hw Li Wr Sr)
 as[[Hg Fam][Wm[Sm[Nd Poly]]]].
 pose proof(family_member_only w R Fl b Hload c e F C mi r mid s g matching si sc
 Finj Wo Checked Wsc HC Select Hi Hw Li Wr Sr Can)as Only.
 destruct(proj1 Fam)as(d&n&names&args&res&Decl&D&Shape).
 change(A.w_decl(WorldSound.W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl g=ADFunctor n names args res None)in D.
 destruct(apply_family_context w R Fl b Hload Finj f c e F C g n names args res matching wanted
 sa pre e' sf Wo Hg D Fam App Ia Wa L HC Wm Sm)
 as(ei&sit&er&sr&emitted&sm&fx&Cu&ns&Cr&ks&Install&Inner&Pre&HCi&Fu&Ctx&Reads&Agree&Sat&Result&HR).
 set(Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C)in *.
 assert(Uf:en_uf e'=en_uf e)by(eapply apply_family_uf;eauto).
 assert(Can':canon e' r=r)by(rewrite(canon_uf e e' Uf);exact Can).
 assert(Clean:ety_ok r=true)by exact(support_clean(loaded_record_unowned w R Fl b Hload)Sr Wr).
 assert(MissR:A.mlook(tr Fl r)mid(A.c_mths Ci)=None).
 {unfold Ci;rewrite family_matching_method_table.
  pose proof(cr_mths _ _ _ _ _ _ _ HC r mid ltac:(rewrite Can;exact Clean))as Read.
  cbn beta in Read;rewrite Can,Miss in Read;exact Read. }
 assert(FoundR:A.mlook(tr Fl r)mid(A.c_mths Cr)=Some(output_sig Fl e' sg)).
 {pose proof(cr_mths _ _ _ _ _ _ _ HR r mid ltac:(rewrite Can';exact Clean))as Read.
  cbn beta in Read;rewrite Can',Found in Read;exact Read. }
 destruct(family_result_method_entries W(gs_id g)Ci(fun t=>TAbs t)Cr ks Fam Result)
 as(tm&es&Entries&Keys&Final).
 assert(Key:In(A.mkey(tr Fl r)mid)ks).
 {eapply result_method_key_new;[exact Keys|exact MissR|rewrite <-Final;exact FoundR]. }
 exists fx,emitted,sm,Cu,ns,Cr,ks;cbn zeta.
 split;[exact Pre|];split;[unfold Ci in Fu;rewrite fold_set_ty_funs in Fu;exact Fu|].
 split;[exact Fam|];split;[apply family_matching_poly;exact Poly|];split;[exact Ctx|].
 split.
 - intros t Pt Present;destruct(A.nlook t(A.c_ty Cu))as[ty|]eqn:Read;[|contradiction].
   destruct(Reads t ty Read)as(here&Have&Hit);eexists;exact Hit.
 - split;[exact Reads|];split;[exact Agree|];split;[exact Sat|];split;[exact Result|].
   split;[exact Key|];split;assumption.
Qed.
End Selected.
