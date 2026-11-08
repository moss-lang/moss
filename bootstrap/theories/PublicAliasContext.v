(* A local static/runtime alias-reader invariant. Formation of the initial
   relation and canonicalization idempotence remain separate obligations. *)
From Stdlib Require Import List PeanoNat.
From NM Require Import Audit PublicGenerationFacts.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition alias_context_agrees (C : A.cx) (d : environment) : Prop :=
  forall source tau, A.nlook source (A.c_ty C) = Some tau ->
    output_meaning d source = Some (ground d (A.canon C tau)).

Lemma canon_set_ty : forall C source meaning tau,
  A.canon (A.set_ty C source meaning) tau = A.canon C tau.
Proof. reflexivity. Qed.

Lemma output_meaning_bind_type_other : forall d target rhs source,
  Nat.eqb source target = false ->
  output_meaning (bind_type d target rhs) source = output_meaning d source.
Proof.
  intros d target rhs source Different. unfold output_meaning.
  cbn [bind_type type_aliases type_meanings lookup_subst]. rewrite Different; reflexivity.
Qed.

(* Exactly the static and runtime updates emitted by BTypeBind. Only
   idempotence on the assigned meaning is required, not a global axiom
   asserting idempotence for arbitrary contexts or substitutions. *)
Theorem type_bind_preserves_alias_context : forall C d target tau,
  alias_context_agrees C d ->
  A.canon C (A.canon C tau) = A.canon C tau ->
  alias_context_agrees (A.set_ty C target (A.canon C tau))
    (bind_type d target (A.canon C tau)).
Proof.
  intros C d target tau Agrees Idempotent source meaning Found.
  cbn [A.set_ty A.c_ty A.nlook] in Found.
  rewrite canon_set_ty, bind_type_preserves_ground.
  destruct (Nat.eqb source target) eqn:Same.
  - injection Found as <-. rewrite Idempotent.
    unfold output_meaning. cbn [bind_type type_aliases lookup_subst].
    rewrite Same; reflexivity.
  - rewrite (output_meaning_bind_type_other d target (A.canon C tau) source Same).
    apply Agrees; exact Found.
Qed.

(* Include the actual rule premises to record the link with the public
   judgment. No initial-context, type-reading, or merge-formation theorem
   is being assumed implicitly by the alias invariant. *)
Theorem public_type_bind_has_alias_context : forall W P F C d pa target rp ra tau,
  A.resolve_sym W (A.fr_mod F) pa = Some target ->
  A.kind_sym W target = Some A.DKType ->
  ~ In target (A.fr_pins F) ->
  A.TyOf W (A.fr_mod F) (A.c_ty C) None (Input.ATRef rp ra) tau ->
  alias_context_agrees C d ->
  A.canon C (A.canon C tau) = A.canon C tau ->
  A.BindItem W P F C
    (Input.Spec (Some pa) None None, Some (Input.AEPath rp ra))
    (bind_type_expression target (A.canon C tau))
    (A.set_ty C target (A.canon C tau)) F [] /\
  alias_context_agrees (A.set_ty C target (A.canon C tau))
    (bind_type d target (A.canon C tau)).
Proof.
  intros W P F C d pa target rp ra tau Resolve Kind Unpinned Typed Agrees Idempotent.
  split.
  - eapply A.BTypeBind; eassumption.
  - apply type_bind_preserves_alias_context; assumption.
Qed.
