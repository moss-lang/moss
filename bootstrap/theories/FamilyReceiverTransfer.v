(* Specialization commutes with context brackets and the selected receiver. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel Sx CtxMerge TyOfSubst TySupport SourceReadingExtend ProvideResultSubst.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma family_forall2_det:forall X Y(R:X->Y->Prop),
 (forall x y z,R x y->R x z->y=z)->forall xs ys zs,
 Forall2 R xs ys->Forall2 R xs zs->ys=zs.
Proof.
 intros X Y R Det xs ys zs F;revert zs;induction F as[|x y xs ys H F IH];intros zs G;inversion G;subst.
 - reflexivity.
 - f_equal;[eapply Det;eassumption|eapply IH;eassumption].
Qed.
Theorem family_output_brackets_det:forall W p tm a bs1 bs2,
 A.OutputBrackets W p tm a bs1->A.OutputBrackets W p tm a bs2->bs1=bs2.
Proof.
 intros W p tm a bs1 bs2 B1 B2;destruct B1 as[|bs l1 F1];inversion B2;subst;[reflexivity|].
 f_equal;eapply family_forall2_det;[|exact F1|eassumption].
 intros b0[k1 t1][k2 t2](sp1&sa1&Eq1&R1&K1&T1)(sp2&sa2&Eq2&R2&K2&T2).
 cbn[fst snd]in *.
 assert(sp2=sp1)by congruence;assert(sa2=sa1)by congruence;subst sp2 sa2.
 assert(k2=k1)by congruence;subst k2;f_equal;eapply tyof_det;eassumption.
Qed.
Lemma family_type_extension_app:forall W bs tm tm',
 @TypeExtension W tm tm'->@TypeExtension W(bs++tm)(bs++tm').
Proof.
 intros W bs tm tm'[Ext Same];split;[apply lookup_extends_app;exact Ext|].
 intros r Other;rewrite !nlook_app';destruct(A.nlook r bs);[reflexivity|apply Same;exact Other].
Qed.
Theorem family_brackets_transfer:forall W sigma p marked semantic completed a bm bs,
 SubRel sigma marked completed->@TypeExtension W semantic completed->
 A.OutputBrackets W p marked a bm->A.OutputBrackets W p semantic a bs->
 smap sigma bm=bs/\SubRel sigma(bm++marked)(bs++completed)/\
 @TypeExtension W(bs++semantic)(bs++completed).
Proof.
 intros W sigma p marked semantic completed a bm bs SR TE Bm Bs.
 pose proof(output_brackets_subst W sigma p marked a bm Bm completed SR)as Bm'.
 pose proof(output_brackets_extends W p semantic a bs Bs completed(proj1 TE))as Bs'.
 assert(Eq:smap sigma bm=bs)by(eapply family_output_brackets_det;eassumption).
 split;[exact Eq|];split.
 - rewrite <-Eq;apply subrel_app;exact SR.
 - apply family_type_extension_app;exact TE.
Qed.
Theorem family_receiver_transfer:forall b W sigma r marked semantic completed pat target,
 A.WorldOf b W->TySupport W target->
 SubRel sigma marked completed->@TypeExtension W semantic completed->
 A.RecvTy W r marked pat->A.RecvTy W r semantic target->
 ExecutionModel.app_subst sigma pat=target.
Proof.
 intros b W sigma r marked semantic completed pat target Wo Supported SR TE Rm Rs.
 pose proof(output_receiver_subst W sigma r marked pat Rm completed SR)as Rm'.
 pose proof(receiver_extends_supported W b r semantic target Wo Supported Rs completed TE)as Rs'.
 eapply recvty_det;eassumption.
Qed.
