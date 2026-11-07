(* Normalizing result type tables is justified by the actual context invariant. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySoundCore BodyTr BodyTy Merge CanonTypes
 ProvideResultContext ProvideResultFold.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_result_tys(C:A.cx):list(nat*ty):=
 map(fun q=>(fst q,A.canon C(snd q)))(A.c_ty C).
Definition family_types_stable(C:A.cx):Prop:=
 map(fun q=>(fst q,A.canon C(snd q)))(A.c_ty C)=A.c_ty C.
Lemma family_normal_lookup:forall(C:A.cx)xs k,
 A.nlook k(map(fun q=>(fst q,A.canon C(snd q)))xs)=option_map(A.canon C)(A.nlook k xs).
Proof.
 intros C xs;induction xs as[|[j y]xs IH];intro k;[reflexivity|].
 cbn[A.nlook map fst snd];destruct(Nat.eqb k j);[reflexivity|apply IH].
Qed.
Lemma family_empty_output:forall C,
 A.output_type_map C[](fun t=>TAbs t)=family_result_tys C.
Proof. reflexivity. Qed.
Lemma family_empty_self:forall C,family_types_stable C->A.output_cx C(A.c_ty C)[]=C.
Proof.
 intros C Stable;unfold A.output_cx;cbn[rev flat_map List.app];rewrite Stable;destruct C;reflexivity.
Qed.
Lemma family_method_output:forall C r m sg app,
 family_types_stable C->A.canon C r=r->A.sig_subst(A.c_sub C)sg=sg->
 A.output_cx C(A.c_ty C)[A.NMth r m sg app]=
 A.provide C(A.mkey r m)app(A.set_mth C r m sg).
Proof. intros;rewrite output_cx_method,family_empty_self by assumption;rewrite H0,H1;reflexivity. Qed.
Lemma family_method_types_stable:forall C r m sg app,
 family_types_stable C->family_types_stable(A.provide C(A.mkey r m)app(A.set_mth C r m sg)).
Proof. intros;exact H. Qed.
Lemma family_output_types:forall C es,
 family_types_stable C->A.c_ty(A.output_cx C(A.c_ty C)es)=A.c_ty C.
Proof. intros C es H;exact H. Qed.
Lemma family_output_sub:forall C tm es,A.c_sub(A.output_cx C tm es)=A.c_sub C.
Proof. reflexivity. Qed.
Lemma family_output_stable:forall C es,
 family_types_stable C->family_types_stable(A.output_cx C(A.c_ty C)es).
Proof.
 intros C es H;unfold family_types_stable;rewrite family_output_types by exact H;exact H.
Qed.
Section Actual.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat).
Lemma family_result_tys_lookup:forall c e F C,
 CxRel w R Fl c e F C->forall k,A.nlook k(family_result_tys C)=A.nlook k(A.c_ty C).
Proof.
 intros c e F C H k;unfold family_result_tys;rewrite family_normal_lookup.
 destruct(A.nlook k(A.c_ty C))as[y|]eqn:Y;cbn[option_map];[|reflexivity].
 rewrite(output_context_type_stable w R Fl _ _ _ _ _ _ H Y);reflexivity.
Qed.
Lemma family_result_tys_idem:forall c e F C,
 CxRel w R Fl c e F C->
 map(fun q=>(fst q,A.canon C(snd q)))(family_result_tys C)=family_result_tys C.
Proof.
 intros c e F C H;unfold family_result_tys;rewrite map_map;apply map_ext;intro q;cbn[fst snd].
 rewrite(output_context_canon_idem w R Fl _ _ _ _ H);reflexivity.
Qed.
Lemma family_initial_context:forall c e F C,
 CxRel w R Fl c e F C->
 CxRel w R Fl c e F(output_with_types C(family_result_tys C))/\
 family_types_stable(output_with_types C(family_result_tys C))/\
 A.output_cx C(family_result_tys C)[]=output_with_types C(family_result_tys C).
Proof.
 intros c e F C H;split.
 - apply cxrel_output_types;[exact H|exact(family_result_tys_lookup c e F C H)].
 - split.
   + unfold family_types_stable;change(map(fun q=>(fst q,A.canon C(snd q)))(family_result_tys C)=family_result_tys C).
     exact(family_result_tys_idem _ _ _ _ H).
   + unfold A.output_cx;cbn[rev flat_map List.app].
     rewrite(family_result_tys_idem _ _ _ _ H);destruct C;reflexivity.
Qed.
Lemma family_canonical_receiver:forall c e F C,
 CxRel w R Fl c e F C->forall r,A.canon C(tr Fl(canon e r))=tr Fl(canon e r).
Proof.
 intros c e F C H r;unfold A.canon;rewrite(cr_sub _ _ _ _ _ _ _ H).
 rewrite tr_canon,canon_idem by exact(proj1(cr_uf _ _ _ _ _ _ _ H));reflexivity.
Qed.
Lemma family_canonical_signature:forall c e F C,
 CxRel w R Fl c e F C->forall sg,A.sig_subst(A.c_sub C)(output_sig Fl e sg)=output_sig Fl e sg.
Proof.
 intros c e F C H sg;unfold A.sig_subst,output_sig;cbn[A.sg_params A.sg_ret].
 f_equal.
 - rewrite map_map;apply map_ext;intro q;apply(family_canonical_receiver _ _ _ _ H).
 - apply(family_canonical_receiver _ _ _ _ H).
Qed.
End Actual.
