(* Family result receivers are already canonical under the caller's resolved merges. *)
From Stdlib Require Import List String.
From NM Require Import Audit NMap Sx Elab BodySound BodyTr CtxMerge Merge CanonTypes
 TyOfSubst ProvideResultSubst FamilyOutputLeaf FamilyReceiverTransfer FamilyMarkerSpecialize FamilyShapeExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma family_output_leaf_stable:forall W p ob tm sp rt mid,
 FamilyOutputLeaf W p ob tm sp rt mid->forall sigma,
 SubRel sigma(ob++tm)(ob++tm)->Audit.app_subst sigma rt=rt.
Proof.
 intros W p ob tm sp rt mid Leaf;induction Leaf;intros sigma SR.
 - pose proof(output_brackets_subst W sigma p(ob++tm)a bs H1(ob++tm)SR)as Br.
   assert(Eq:smap sigma bs=bs)by(eapply family_output_brackets_det;[exact Br|exact H1]).
   pose proof(subrel_app sigma bs _ _ SR)as SR';rewrite Eq in SR'.
   pose proof(output_receiver_subst W sigma r(bs++ob++tm)rt H2(bs++ob++tm)SR')as Rr.
   eapply recvty_det;[exact Rr|exact H2].
 - pose proof(output_brackets_subst W sigma p(ob++tm)a bs H2(ob++tm)SR)as Br.
   assert(Eq:smap sigma bs=bs)by(eapply family_output_brackets_det;[exact Br|exact H2]).
   pose proof(subrel_app sigma bs _ _ SR)as SR';rewrite Eq in SR'.
   apply IHLeaf;rewrite <-app_assoc;exact SR'.
Qed.
Lemma family_matching_sub:forall matching C,
 A.c_sub(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)=A.c_sub C.
Proof.
 intro matching;induction matching as[|[p t]rest IH];intro C;[reflexivity|].
 cbn[fold_left fst snd];rewrite IH;reflexivity.
Qed.
Lemma family_output_type_target:forall C matching,
 A.output_type_map(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)[](fun t=>TAbs t)=
 family_semantic_target C matching.
Proof.
 intros;unfold A.output_type_map,family_semantic_target,smap,A.canon;cbn[map List.app].
 rewrite family_matching_sub;reflexivity.
Qed.
Section Canonical.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma family_target_self_subrel:forall c e F C matching,
 CxRel w R Fl c e F C->
 SubRel(A.c_sub C)(family_semantic_target C matching)(family_semantic_target C matching).
Proof.
 intros c e F C matching HC k;unfold family_semantic_target;rewrite nlook_smap.
 destruct(A.nlook k(A.c_ty(fold_left(fun C q=>A.set_ty C(fst q)(snd q))matching C)))as[t|];
 cbn[option_map];[|reflexivity];f_equal;symmetry.
 rewrite(cr_sub _ _ _ _ _ _ _ HC);apply suf_idem,tri_rk;exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
Qed.
Theorem family_produces_semantic_leaf:forall c e F C u rt mid,
 CxRel w R Fl c e F C->A.FamilyProduces W C u(A.mkey rt mid)->
 exists fx matching d n names args results sp,
 A.nlook u(A.c_funs C)=Some fx/\A.Family W u/\
 Forall(fun q=>A.Poly W u(fst q))matching/\
 A.w_decl W u=Some d/\A.d_ast d=ADFunctor n names args results None/\In sp results/\
 FamilyOutputLeaf W(A.d_mod d)[](family_semantic_target C matching)sp rt mid.
Proof.
 intros c e F C u rt mid HC[fx[matching[Cr[keys[Have[Fam[Poly[Provide Key]]]]]]]].
 destruct(provide_result_method_leaf W u _ _ Cr keys rt mid Provide Key)
 as(d&names&args&results&body&outs&sp&raw&Decl&Parts&Outputs&Hin&Leaf&Receiver).
 assert(Empty:outs=[]).
 {destruct outs as[|t rest];[reflexivity|].
  exfalso;apply(family_no_outputs W u Fam t);apply Outputs;left;reflexivity. }
 subst outs;rewrite family_output_type_target in Leaf.
 pose proof(family_output_leaf_stable W _ _ _ _ _ _ Leaf(A.c_sub C)
  (family_target_self_subrel c e F C matching HC))as Stable.
 unfold A.canon in Receiver;rewrite family_matching_sub in Receiver;rewrite Stable in Receiver;subst raw.
 destruct(proj1 Fam)as(d0&n&names0&args0&results0&D0&Ast&Shape).
 assert(d0=d)by congruence;subst d0;rewrite Ast in Parts;cbn[A.functor_parts]in Parts.
 injection Parts as <- <- <- <-.
 exists fx,matching,d,n,names0,args0,results0,sp.
 split;[exact Have|];split;[exact Fam|];split;[exact Poly|];split;[exact Decl|];
 split;[exact Ast|];split;[exact Hin|exact Leaf].
Qed.
End Canonical.
