(* Successful automatic family selection and application really produce the key. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTr
 BodyTy NominalSupport CanonTypes FamilyMatchingCtx FamilyApplyRestore FamilySelectTrace
 FamilyResultFrame RelFacts ProvideResultContext.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Lemma family_matching_method_table:forall Fl e th C,
 A.c_mths(fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e th)C)=A.c_mths C.
Proof.
 intros Fl e th;unfold family_matching;induction th as[|[t ty]rest IH];intro C;
 [reflexivity|cbn[map fst snd fold_left];rewrite IH;reflexivity].
Qed.
Lemma family_matching_poly:forall W Fl e u th,
 Forall(fun q=>A.Poly W u(fst q))th->
 Forall(fun q=>A.Poly W u(fst q))(family_matching Fl e th).
Proof.
 intros W Fl e u th H;unfold family_matching;rewrite Forall_forall in *.
 intros q Hq;apply in_map_iff in Hq as([t ty]&<-&Hin);exact(H(t,ty)Hin).
Qed.
Section Key.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Hypothesis Finj:forall a b,Fl a=Fl b->a=b.
Notation W:=(BodySound.W w R Fl).
Theorem family_application_produces:forall fuel c e F C g n names args res matching wanted s pre e' sf r mid sg,
 A.WorldOf b W->nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Family W(gs_id g)->apply_family fuel c e g matching wanted s=Good((pre,e'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 Forall(fun q=>WFty(snd q))matching->Forall(fun q=>Support W(snd q))matching->
 Forall(fun q=>A.Poly W(gs_id g)(fst q))matching->
 canon e r=r->ety_ok r=true->get_mth e r mid=None->get_mth e' r mid=Some sg->
 A.FamilyProduces W C(gs_id g)(A.mkey(tr Fl r)mid)/\
 exists C',CxRel w R Fl c e' F C'.
Proof.
 intros fuel c e F C g n names args res matching wanted s pre e' sf r mid sg
 Wo Hg D Fam App Hi Hw L HC Wm Sm Poly Can Clean Miss Found.
 destruct(apply_family_context w R Fl b Hload Finj fuel c e F C g n names args res matching wanted
 s pre e' sf Wo Hg D Fam App Hi Hw L HC Wm Sm)
 as(ei&si&er&sr&emitted&smap&fx&Cu&ns&Cr&ks&Install&Inner&Pre&HCi&Fu&FC&Reads&Agree&Sat&Result&HR).
 set(Ci:=fold_left(fun C q=>A.set_ty C(fst q)(snd q))(family_matching Fl e matching)C)in *.
 assert(Uf:en_uf e'=en_uf e)by(eapply apply_family_uf;eauto).
 assert(Can':canon e' r=r)by(rewrite(canon_uf e e' Uf);exact Can).
 assert(MissR:A.mlook(tr Fl r)mid(A.c_mths Ci)=None).
 {unfold Ci;rewrite family_matching_method_table.
  pose proof(cr_mths _ _ _ _ _ _ _ HC r mid ltac:(rewrite Can;exact Clean))as M.
  cbn beta in M;rewrite Can,Miss in M;exact M. }
 assert(FoundR:A.mlook(tr Fl r)mid(A.c_mths Cr)=Some(output_sig Fl e' sg)).
 {pose proof(cr_mths _ _ _ _ _ _ _ HR r mid ltac:(rewrite Can';exact Clean))as M.
  cbn beta in M;rewrite Can',Found in M;exact M. }
 destruct(family_result_method_entries W(gs_id g)Ci(fun t=>TAbs t)Cr ks Fam Result)
 as(tm&es&Entries&Keys&Final).
 assert(Produced:In(A.mkey(tr Fl r)mid)ks).
 {eapply result_method_key_new;[exact Keys|exact MissR|rewrite <-Final;exact FoundR]. }
 split.
 - exists fx,(family_matching Fl e matching),Cr,ks.
   split;[unfold Ci in Fu;rewrite fold_set_ty_funs in Fu;exact Fu|].
   split;[exact Fam|];split;[apply family_matching_poly;exact Poly|];split;assumption.
 - exists(A.family_restore C Cr);exact HR.
Qed.
Theorem family_selected_produces:forall fuel c e F C mi r mid s g matching si wanted pre e' sf sg,
 A.WorldOf b W->family_member mi e r mid s=Good(Some(g,matching),si)->
 apply_family fuel c e g matching wanted si=Good((pre,e'),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->CxRel w R Fl c e F C->
 WFty r->Support W r->canon e r=r->ety_ok r=true->
 get_mth e r mid=None->get_mth e' r mid=Some sg->
 A.FamilyProduces W C(gs_id g)(A.mkey(tr Fl r)mid)/\exists C',CxRel w R Fl c e' F C'.
Proof.
 intros fuel c e F C mi r mid s g matching si wanted pre e' sf sg
 Wo Select App Hi Hw L HC Wr Sr Can Clean Miss Found.
 assert(Oselect:ok(family_member mi e r mid))by(unfold family_member;okt).
 destruct(Oselect _ _ _ Hi Select)as[Ii Fi].
 destruct(ok_apply_family fuel c e g matching wanted _ _ _ Ii App)as[If Fapp].
 destruct(family_member_substitution w R Fl b Hload c e F C mi r mid s g matching si HC Select Hi Hw
 (later_back w P R Fl _ _ Fapp L)Wr Sr)as[[Hg Fam][Wm[Sm[Nd Poly]]]].
 destruct(proj1 Fam)as(d&n&names&args&res&Decl&D&Shape).
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl g=ADFunctor n names args res None)in D.
 eapply family_application_produces;[exact Wo|exact Hg|exact D|exact Fam|exact App|exact Ii|
  rewrite(fr_w _ _ Fi);exact Hw|exact L|exact HC|exact Wm|exact Sm|exact Poly|exact Can|exact Clean|exact Miss|exact Found].
Qed.
End Key.
