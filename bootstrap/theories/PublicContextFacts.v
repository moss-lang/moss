(* Canonicalization facts supplied by actual public context formation. *)
From Stdlib Require Import List.
From NM Require Import Audit PublicAliasContext.
From NM Require PublicMerge.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition canonical_context (C : A.cx) : Prop :=
  forall tau, A.canon C (A.canon C tau) = A.canon C tau.

Definition CxSection (C : A.cx) : Prop := A.SectionOf (A.c_merge C) (A.c_sub C).

Theorem entries_canonical : forall substitution classes meanings entries,
  A.MostGeneralMerge substitution entries ->
  canonical_context (A.entries_cx substitution classes meanings entries).
Proof. intros substitution classes meanings entries [_ [Idempotent _]]; exact Idempotent. Qed.

Lemma section_canonical : forall C, CxSection C -> canonical_context C.
Proof.
  intros C [Reads Respects] t. unfold A.canon. symmetry.
  apply Respects, PublicMerge.merge_eq_read. exact Reads.
Qed.

Theorem function_context_merge : forall W f C needs,
  A.CtxOf W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & es & tm & er & tmr & s & M & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem functor_context_merge : forall W f C needs,
  A.FunCtx W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & ps & a & r & b & es & tm & tm' & s & M & implicit &
    _ & _ & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem assumed_context_merge : forall W f C needs,
  A.AssumeCtx W f C needs -> A.Solved (A.c_merge C) /\ CxSection C.
Proof.
  intros W f C needs (d & es & tm & er & tmr & s & M & _ & _ & _ & _ & Formed & Section & -> & _).
  exact (conj (proj1 Formed) Section).
Qed.

Theorem function_context_canonical : forall W function C needs,
  A.CtxOf W function C needs -> canonical_context C.
Proof.
  intros W f C needs Context. apply section_canonical.
  exact (proj2 (function_context_merge W f C needs Context)).
Qed.

Theorem functor_context_canonical : forall W functor C needs,
  A.FunCtx W functor C needs -> canonical_context C.
Proof.
  intros W f C needs Context. apply section_canonical.
  exact (proj2 (functor_context_merge W f C needs Context)).
Qed.

Corollary context_canon_iff_mergeeq : forall C, CxSection C ->
  forall t u, A.canon C t = A.canon C u <-> A.MergeEq (A.c_merge C) t u.
Proof.
  intros C Section t u. symmetry. apply PublicMerge.merge_section_kernel. exact Section.
Qed.

Lemma section_same_fields : forall C C',
  A.c_sub C' = A.c_sub C -> A.c_merge C' = A.c_merge C ->
  CxSection C -> CxSection C'.
Proof. intros C C' Sub Merge Section. unfold CxSection in *. now rewrite Sub, Merge. Qed.

Lemma provide_merge : forall C k a C',
  A.c_merge (A.provide C k a C') = A.c_merge C'.
Proof.
  intros C k a C'. unfold A.provide.
  destruct (A.ilook k (A.c_provisions C')) as [[]|]; reflexivity.
Qed.

Lemma provided_result_merge : forall W u C out C' ks,
  A.ProvideResult W u C out C' ks -> A.c_merge C' = A.c_merge C.
Proof.
  intros W u C out C' ks (d & ps & a & r & b & outs & es & tm & _ & _ & _ & _ & _ & ->).
  reflexivity.
Qed.

Lemma family_context_merge : forall matching C,
  A.c_merge (A.family_context C matching) = A.c_merge C.
Proof.
  induction matching as [|[x t] matching IH]; intros C; [reflexivity |].
  change (A.c_merge (A.family_context (A.set_ty C x t) matching) = A.c_merge C).
  rewrite IH. reflexivity.
Qed.

Theorem type_binding_preserves_canonical : forall C source meaning,
  canonical_context C -> canonical_context (A.set_ty C source meaning).
Proof. intros C source meaning Canonical; exact Canonical. Qed.

Theorem output_context_preserves_canonical : forall C meanings entries,
  canonical_context C -> canonical_context (A.output_cx C meanings entries).
Proof. intros C meanings entries Canonical; exact Canonical. Qed.

Theorem formed_type_binding_preserves_alias_context : forall C d source tau,
  canonical_context C -> alias_context_agrees C d ->
  alias_context_agrees (A.set_ty C source (A.canon C tau))
    (bind_type d source (A.canon C tau)).
Proof. intros; apply type_bind_preserves_alias_context; auto. Qed.

(* The world fixes each symbol's context, so every rule reading it agrees. *)
Lemma function_context_record : forall W s C ns,
  A.CtxOf W s C ns -> A.w_context W s = Some (C, ns).
Proof.
  intros W s C ns (d & es & tm & er & tmr & σ & M & _ & _ & _ & _ & _ & _ & _ & _ & Recorded).
  exact Recorded.
Qed.

Lemma functor_context_record : forall W u C ns,
  A.FunCtx W u C ns -> A.w_context W u = Some (C, ns).
Proof.
  intros W u C ns
    (d & ps & a & r & b & es & tm & tm' & s & M & implicit & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & Recorded).
  exact Recorded.
Qed.

Theorem function_context_unique : forall W s C ns C' ns',
  A.CtxOf W s C ns -> A.CtxOf W s C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W s C ns C' ns' Context Context'.
  pose proof (function_context_record W s C ns Context) as Recorded.
  pose proof (function_context_record W s C' ns' Context') as Recorded'.
  rewrite Recorded in Recorded'. injection Recorded' as <- <-. split; reflexivity.
Qed.

Theorem functor_context_unique : forall W u C ns C' ns',
  A.FunCtx W u C ns -> A.FunCtx W u C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W u C ns C' ns' Context Context'.
  pose proof (functor_context_record W u C ns Context) as Recorded.
  pose proof (functor_context_record W u C' ns' Context') as Recorded'.
  rewrite Recorded in Recorded'. injection Recorded' as <- <-. split; reflexivity.
Qed.

(* A call's instantiation cannot bind two members of one merged class
   differently: if the callee merges g1 and g2 into z, their meanings at a
   caller that binds both agree. *)
Theorem call_theta_merged : forall W C Cs s th g1 g2 z,
  canonical_context C -> A.CallTheta W C Cs s th ->
  In (g1, TAbs z) (A.c_sub Cs) -> In (g2, TAbs z) (A.c_sub Cs) ->
  In g1 (map fst (A.c_ty Cs)) -> In g2 (map fst (A.c_ty Cs)) ->
  A.nlook g1 (A.c_ty C) <> None -> A.nlook g2 (A.c_ty C) <> None ->
  A.canon C (app_subst (A.c_ty C) (TAbs g1)) = A.canon C (app_subst (A.c_ty C) (TAbs g2)).
Proof.
  intros W C Cs s th g1 g2 z Canonical [_ [Bound Merges]] M1 M2 R1 R2 F1 F2.
  pose proof (Merges g1 (TAbs z) M1) as E1. pose proof (Merges g2 (TAbs z) M2) as E2.
  cbn [app_subst] in E1, E2. rewrite (Bound g1 R1 F1) in E1. rewrite (Bound g2 R2 F2) in E2.
  rewrite Canonical in E1, E2. congruence.
Qed.
