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
