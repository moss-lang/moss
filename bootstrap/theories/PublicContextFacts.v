(* Canonicalization facts supplied by actual public context formation. *)
From Stdlib Require Import List.
From NM Require Import Audit PublicAliasContext.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition canonical_context (C : A.cx) : Prop :=
  forall tau, A.canon C (A.canon C tau) = A.canon C tau.

Theorem entries_canonical : forall substitution meanings entries,
  A.MostGeneralMerge substitution entries ->
  canonical_context (A.entries_cx substitution meanings entries).
Proof. intros substitution meanings entries [_ [Idempotent _]]; exact Idempotent. Qed.

Theorem function_context_canonical : forall W function C needs,
  A.CtxOf W function C needs -> canonical_context C.
Proof.
  intros W function C needs
    [declaration [entries [meanings [root_entries [root_meanings [substitution
      [_ [_ [_ [Merge [-> _]]]]]]]]]]].
  apply entries_canonical; exact Merge.
Qed.

Theorem functor_context_canonical : forall W functor C needs,
  A.FunCtx W functor C needs -> canonical_context C.
Proof.
  intros W functor C needs
    [declaration [parameters [inputs [outputs [body [entries [meanings [all_meanings
      [substitution [implicit [_ [_ [_ [_ [_ [Merge [-> _]]]]]]]]]]]]]]]]].
  apply entries_canonical; exact Merge.
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
Theorem function_context_unique : forall W s C ns C' ns',
  A.CtxOf W s C ns -> A.CtxOf W s C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W s C ns C' ns'
    [d [es [tm [er [tmr [σ [_ [_ [_ [_ [_ [_ Recorded]]]]]]]]]]]]
    [d' [es' [tm' [er' [tmr' [σ' [_ [_ [_ [_ [_ [_ Recorded']]]]]]]]]]]].
  rewrite Recorded in Recorded'. injection Recorded' as <- <-. split; reflexivity.
Qed.

Theorem functor_context_unique : forall W u C ns C' ns',
  A.FunCtx W u C ns -> A.FunCtx W u C' ns' -> C = C' /\ ns = ns'.
Proof.
  intros W u C ns C' ns' Ctx Ctx'.
  destruct Ctx as [d [ps [a [r [b [es [tm [tm1 [σ [implicit [_ [_ [_ [_ [_ [_ [_ [_ Recorded]]]]]]]]]]]]]]]]]].
  destruct Ctx' as [d' [ps' [a' [r' [b' [es' [tm' [tm1' [σ' [implicit' [_ [_ [_ [_ [_ [_ [_ [_ Recorded']]]]]]]]]]]]]]]]]].
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
  intros W C Cs s th g1 g2 z Canonical [_ [Bound [Merges _]]] M1 M2 R1 R2 F1 F2.
  pose proof (Merges g1 (TAbs z) M1) as E1. pose proof (Merges g2 (TAbs z) M2) as E2.
  cbn [app_subst] in E1, E2. rewrite (Bound g1 R1 F1) in E1. rewrite (Bound g2 R2 F2) in E2.
  rewrite Canonical in E1, E2. congruence.
Qed.
