(* Real generated output-type installation forms the result reader's type table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame BodySound BodyTr BodyTy Merge CanonTypes
 NominalSupport BindType ProvideResultExec ProvideResultContext ApplyFreshExec FamilyResultFold.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition output_types_fold(f:nat->ty)(ts:list nat)(C:A.cx):A.cx:=
 fold_left(fun C t=>A.set_ty C t(f t))ts C.
Lemma output_types_fold_lookup:forall ts f C k,
 A.nlook k(A.c_ty(output_types_fold f ts C))=
 if nmem k ts then Some(f k)else A.nlook k(A.c_ty C).
Proof.
 intro ts;induction ts as[|t rest IH];intros f C k;[reflexivity|].
 unfold output_types_fold at 1;cbn[fold_left];change(A.nlook k(A.c_ty(output_types_fold f rest(A.set_ty C t(f t))))=if nmem k(t::rest)then Some(f k)else A.nlook k(A.c_ty C)).
 rewrite IH;cbn[nmem existsb A.set_ty A.c_ty A.nlook fst snd].
 destruct(Nat.eqb_spec k t)as[Eq|Ne];[subst t|];destruct(nmem k rest);reflexivity.
Qed.
Lemma output_types_fold_shape:forall ts f C,
 output_types_fold f ts C=output_with_types C(A.c_ty(output_types_fold f ts C)).
Proof.
 intro ts;induction ts as[|t rest IH];intros f C.
 - destruct C;reflexivity.
 - unfold output_types_fold at 1;cbn[fold_left].
   change(output_types_fold f rest(A.set_ty C t(f t))=output_with_types C(A.c_ty(output_types_fold f rest(A.set_ty C t(f t))))).
   rewrite(IH f(A.set_ty C t(f t)))at 1;destruct C;reflexivity.
Qed.
Lemma output_type_values_lookup:forall ts(f:nat->ty)k,
 A.nlook k(map(fun t=>(t,f t))ts)=if nmem k ts then Some(f k)else None.
Proof.
 intro ts;induction ts as[|t rest IH];intros f k;[reflexivity|].
 cbn[map A.nlook nmem existsb fst snd];destruct(Nat.eqb_spec k t)as[->|Ne];[reflexivity|exact(IH f k)].
Qed.
Section Install.
Variable w:Elab.world. Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma output_install_fold:forall ts gens c e0 e F C s et sf,
 mfold(install_output_type gens)e ts s=Good(et,sf)->CxRel w R Fl c e F C->en_uf e=en_uf e0->
 (forall t,In t ts->nassoc t gens<>None)->
 CxRel w R Fl c et F(output_types_fold(fun t=>tr Fl(canon e0(YAbs(generation_of gens t))))ts C)/\
 en_uf et=en_uf e0.
Proof.
 intro ts;induction ts as[|t rest IH];intros gens c e0 e F C s et sf Run HC Uf Total.
 - injection Run as <- <-;split;assumption.
 - cbn[mfold]in Run;apply bind_inv in Run as(en&sn&Step&Tail).
   destruct(nassoc t gens)as[g|]eqn:Get;[|exfalso;apply(Total t(or_introl eq_refl));exact Get].
   unfold install_output_type in Step;rewrite Get in Step;apply bind_inv in Step as(stamp&ss&Fresh&Ret);injection Ret as <- <-.
   assert(Hnew:CxRel w R Fl c(set_tn e t(YAbs g)stamp)F(A.set_ty C t(tr Fl(canon e0(YAbs g))))).
   {rewrite <-(canon_uf e0 e Uf).
    apply cxrel_type;[exact HC|reflexivity|constructor]. }
   assert(Unew:en_uf(set_tn e t(YAbs g)stamp)=en_uf e0)by exact Uf.
   destruct(IH gens c e0(set_tn e t(YAbs g)stamp)F(A.set_ty C t(tr Fl(canon e0(YAbs g))))ss et sf
   Tail Hnew Unew ltac:(intros x Hx;apply Total;right;exact Hx))as[Out Uout].
   split;[|exact Uout].
   unfold output_types_fold;cbn[fold_left].
   unfold generation_of at 2;rewrite Get;exact Out.
Qed.
Theorem output_install_context:forall ts gens c e F C s et sf,
 mfold(install_output_type gens)e ts s=Good(et,sf)->CxRel w R Fl c e F C->
 (forall t,In t ts->nassoc t gens<>None)->
 CxRel w R Fl c et F(output_with_types C(A.output_type_map C ts(fun t=>TAbs(generation_of gens t))))/\en_uf et=en_uf e.
Proof.
 intros ts gens c e F C s et sf Run HC Total.
 destruct(output_install_fold ts gens c e e F C s et sf Run HC eq_refl Total)as[Out Uf].
 rewrite output_types_fold_shape in Out.
 split;[|exact Uf].
 apply(cxrel_output_types w R Fl c et F(output_with_types C(A.c_ty(output_types_fold(fun t=>tr Fl(canon e(YAbs(generation_of gens t))))ts C))))with(tm:=A.output_type_map C ts(fun t=>TAbs(generation_of gens t)))in Out.
 - exact Out.
 - intro k;cbn[output_with_types A.c_ty];rewrite output_types_fold_lookup.
   unfold A.output_type_map;rewrite map_app,map_map,nlook_app.
   rewrite output_type_values_lookup.
   destruct(nmem k ts);cbn.
   + f_equal;unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ HC);change(Audit.app_subst(suf Fl(en_uf e))(tr Fl(YAbs(generation_of gens k)))=tr Fl(canon e(YAbs(generation_of gens k)))).
     apply tr_canon;exact(proj1(cr_uf _ _ _ _ _ _ _ HC)).
   + rewrite family_normal_lookup;destruct(A.nlook k(A.c_ty C))as[y|]eqn:Read;[|reflexivity].
     cbn[option_map];f_equal;eapply output_context_type_stable;eassumption.
Qed.
End Install.
