(* A literal plan records contextual arithmetic choices without emitted code. *)
From Stdlib Require Import List.
From NM Require Import Audit BodyMethodTyping.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Inductive literal_plan : Type :=
| PlanLiteralValue (value : nat)
| PlanLiteralArithmetic (operand_type : ty) (method : nat) (family : option family_use)
    (left right : literal_plan)
| PlanLiteralFloat (function : nat) (operand : literal_plan).

Inductive literal_arithmetic : Type := LiteralAddition | LiteralMultiplication | LiteralDivision.

Definition arithmetic_tree (operation : literal_arithmetic) : literal_tree -> literal_tree -> literal_tree :=
  match operation with
  | LiteralAddition => LAdd
  | LiteralMultiplication => LMul
  | LiteralDivision => LDiv
  end.

Definition arithmetic_item (operation : literal_arithmetic) : lang_item :=
  match operation with
  | LiteralAddition => LIAdd
  | LiteralMultiplication => LIMul
  | LiteralDivision => LIDiv
  end.

Inductive TypesLiteral (W : world) (C : cx) : literal_tree -> literal_plan -> ty -> Prop :=
| TypesLiteralZero : forall z t,
    LangItem W LIZero z -> nlook z (c_vals C) = Some t ->
    TypesLiteral W C LZero (PlanLiteralValue z) t
| TypesLiteralOne : forall o t,
    LangItem W LIOne o -> nlook o (c_vals C) = Some t ->
    TypesLiteral W C LOne (PlanLiteralValue o) t
| TypesLiteralArithmetic : forall operation left right x y t method family,
    TypesLiteral W C left x t -> TypesLiteral W C right y t ->
    LangItem W (arithmetic_item operation) method ->
    TypesMethodProvision W C t method family (FSig [t] t) ->
    TypesLiteral W C (arithmetic_tree operation left right) (PlanLiteralArithmetic t method family x y) t
| TypesLiteralFloat : forall tree operand actual expected t f,
    TypesLiteral W C tree operand actual -> LangItem W LIFloat f ->
    nlook f (c_fns C) = Some (FSig [expected] t) -> fits actual expected ->
    TypesLiteral W C (LFloat tree) (PlanLiteralFloat f operand) t.

Fixpoint emit_literal (C : cx) (plan : literal_plan) : expr :=
  match plan with
  | PlanLiteralValue value => EVal value
  | PlanLiteralArithmetic t method family lhs rhs =>
      emit_provided_method C t method family (emit_literal C lhs) [emit_literal C rhs]
  | PlanLiteralFloat f operand => EInvoke f [emit_literal C operand]
  end.

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
    exact (TypesLiteralArithmetic W C LiteralAddition _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - destruct (literal_method_readback _ _ _ _ _ _ _ Operation) as (family & Selected & ->).
    exists (PlanLiteralArithmetic t m family x' y'). split; [|reflexivity].
    exact (TypesLiteralArithmetic W C LiteralMultiplication _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - destruct (literal_method_readback _ _ _ _ _ _ _ Operation) as (family & Selected & ->).
    exists (PlanLiteralArithmetic t m family x' y'). split; [|reflexivity].
    exact (TypesLiteralArithmetic W C LiteralDivision _ _ _ _ _ _ _ Left' Right' Lang Selected).
  - exists (PlanLiteralFloat f plan). split; [eapply TypesLiteralFloat; eassumption|reflexivity].
Qed.
