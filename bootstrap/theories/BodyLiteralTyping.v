(* A literal plan records contextual arithmetic choices without emitted code. *)
From Stdlib Require Import List.
From NM Require Import Audit BodyMethodTyping.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Theorem literal_translation : forall W C tree plan t,
  TypesLiteral W C tree plan t -> Literal W C tree (emit_literal C plan) t.
Proof.
  intros W C tree plan t Typed. induction Typed; cbn [emit_literal].
  - eapply LiteralZero; eassumption.
  - eapply LiteralOne; eassumption.
  - destruct operation; cbn [arithmetic_tree arithmetic_item] in *.
    + eapply LiteralAdd; try eassumption. eapply literal_method_translation; eassumption.
    + eapply LiteralMul; try eassumption. eapply literal_method_translation; eassumption.
    + eapply LiteralDiv; try eassumption. eapply literal_method_translation; eassumption.
  - eapply LiteralFloat; eassumption.
Qed.

Theorem literal_readback : forall W C tree code t,
  Literal W C tree code t ->
  exists plan, TypesLiteral W C tree plan t /\ code = emit_literal C plan.
Proof.
  intros W C tree code t Typed.
  induction Typed as [z t Lang Read|o t Lang Read|
    a b x y result t m Left [x' [Left' ->]] Right [y' [Right' ->]] Lang Operation|
    a b x y result t m Left [x' [Left' ->]] Right [y' [Right' ->]] Lang Operation|
    a b x y result t m Left [x' [Left' ->]] Right [y' [Right' ->]] Lang Operation|
    a x ti tp t f Operand [plan [Typed ->]] Lang Read Fits].
  - exists (PlanLiteralValue z). split; [eapply TypesLiteralZero; eassumption|reflexivity].
  - exists (PlanLiteralValue o). split; [eapply TypesLiteralOne; eassumption|reflexivity].
  - destruct (literal_method_readback _ _ _ _ _ _ _ Operation) as (family & Selected & ->).
    exists (PlanLiteralArithmetic t m family x' y'). split; [|reflexivity].
    exact (@TypesLiteralArithmetic W C LiteralAddition _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - destruct (literal_method_readback _ _ _ _ _ _ _ Operation) as (family & Selected & ->).
    exists (PlanLiteralArithmetic t m family x' y'). split; [|reflexivity].
    exact (@TypesLiteralArithmetic W C LiteralMultiplication _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - destruct (literal_method_readback _ _ _ _ _ _ _ Operation) as (family & Selected & ->).
    exists (PlanLiteralArithmetic t m family x' y'). split; [|reflexivity].
    exact (@TypesLiteralArithmetic W C LiteralDivision _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - exists (PlanLiteralFloat f plan). split; [eapply TypesLiteralFloat; eassumption|reflexivity].
Qed.
