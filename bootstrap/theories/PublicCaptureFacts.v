(* Public lexical-capture facts, independent of executable elaboration and
   closure compilation. Grounding stability needs a complete captured type
   scope; it is not asserted for names absent from the captured snapshot. *)
From Stdlib Require Import List.
From NM Require AuditBase.
From NM Require Import Audit.
Import AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Lemma lookup_subst_prefix : forall prefix suffix symbol meaning,
  lookup_subst symbol prefix = Some meaning ->
  lookup_subst symbol (prefix ++ suffix) = Some meaning.
Proof.
  induction prefix as [|[key value] rest IH]; intros suffix symbol meaning Found;
    cbn in *; [discriminate|].
  destruct (PeanoNat.Nat.eqb symbol key); [exact Found|apply IH; exact Found].
Qed.

Theorem substitution_agrees_on_generations : forall tau left right,
  (forall symbol, In symbol (A.gens tau) -> lookup_subst symbol left = lookup_subst symbol right) ->
  app_subst left tau = app_subst right tau.
Proof.
  fix IH 1. intros tau left right Agree.
  destruct tau as [machine|fields| |symbol|head arguments|members];
    cbn [app_subst]; try reflexivity.
  - f_equal. revert Agree.
    induction fields as [|[field child] rest Tail]; intro Agree; cbn; [reflexivity|].
    f_equal.
    + f_equal. apply IH. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; left; exact Member.
    + apply Tail. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; right; exact Member.
  - rewrite (Agree symbol (or_introl eq_refl)); reflexivity.
  - f_equal. revert Agree.
    induction arguments as [|[parameter child] rest Tail]; intro Agree; cbn; [reflexivity|].
    f_equal.
    + f_equal. apply IH. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; left; exact Member.
    + apply Tail. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; right; exact Member.
  - f_equal. revert Agree.
    induction members as [|child rest Tail]; intro Agree; cbn; [reflexivity|].
    f_equal.
    + apply IH. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; left; exact Member.
    + apply Tail. intros symbol Member. apply Agree.
      cbn [A.gens]. apply in_or_app; right; exact Member.
Qed.

Lemma capture_lookup_stable : forall caller captured symbol meaning,
  lookup_subst symbol (type_meanings captured) = Some meaning ->
  lookup_subst symbol (type_meanings (capture_environment caller captured)) = Some meaning.
Proof. intros; apply lookup_subst_prefix; assumption. Qed.

Theorem capture_ground_stable : forall caller captured tau,
  (forall symbol, In symbol (A.gens tau) ->
    exists meaning, lookup_subst symbol (type_meanings captured) = Some meaning) ->
  ground (capture_environment caller captured) tau = ground captured tau.
Proof.
  intros caller captured tau Complete. unfold ground.
  apply substitution_agrees_on_generations. intros symbol Member.
  destruct (Complete symbol Member) as [meaning Found].
  rewrite (capture_lookup_stable caller captured symbol meaning Found), Found. reflexivity.
Qed.

Theorem capture_provision_stable : forall caller captured key,
  lookup_provision key (capture_environment caller captured) = lookup_provision key captured.
Proof. reflexivity. Qed.
