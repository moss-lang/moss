(* The expression recursion preserves earlier code slots. Its action
   contract also applies to the knot that calls expression and binding
   elaboration at lower fuel. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit EaBase EaCodeEffects.
From NM Require EaExpr EaDecide EaMain EaBindCodeEffects.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Lemma code_effect_get_const : forall n, PreservesOldCode (get_const n).
Proof. intros n s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_put_const : forall n value, PreservesOldCode (put_const n value).
Proof.
  intros n value s x s' Run. unfold put_const in Run. injection Run as _ <-.
  apply unchanged_code_transition; [apply Nat.le_refl| | |reflexivity]; intros [] i; reflexivity.
Qed.

Lemma code_effect_need_named : forall F n, PreservesOldCode (EaExpr.need_named F n).
Proof. intros F n. unfold EaExpr.need_named. apply code_effect_check. Qed.

Lemma code_effect_join : forall E a b expected, PreservesOldCode (EaExpr.join_m E a b expected).
Proof.
  intros E a b expected. unfold EaExpr.join_m. destruct (EaDecide.join a b expected);
    [apply code_effect_return|apply code_effect_fail].
Qed.

Record ExpressionCodeEffects (E : env) (R : rec) (fuel : nat) : Prop := {
  ex_code_effect : forall F C e expected, PreservesOldCode (EaExpr.ex E R fuel F C e expected);
  fields_code_effect : forall F C fields types, PreservesOldCode (EaExpr.mmap_fields E R fuel F C fields types);
  args_code_effect : forall F C who names args types, PreservesOldCode (EaExpr.args E R fuel F C who names args types);
  cond_code_effect : forall F C cond, PreservesOldCode (EaExpr.cond E R fuel F C cond);
  branch_code_effect : forall F C branch expected, PreservesOldCode (EaExpr.branch E R fuel F C branch expected);
  arms_code_effect : forall F C type arms expected, PreservesOldCode (EaExpr.arms E R fuel F C type arms expected);
  arms_list_code_effect : forall F C type arms expected, PreservesOldCode (EaExpr.arms_list E R fuel F C type arms expected);
  method_code_effect : forall F C expr type method args, PreservesOldCode (EaExpr.meth_call E R fuel F C expr type method args);
  provided_code_effect : forall F C expr type method args, PreservesOldCode (EaExpr.provided E R fuel F C expr type method args);
  block_code_effect : forall F C block expected, PreservesOldCode (EaExpr.blke E R fuel F C block expected);
  statements_code_effect : forall F C statements tail expected, PreservesOldCode (EaExpr.blk E R fuel F C statements tail expected)
}.

Local Ltac expression_effect :=
  first
    [ solve [eauto 1 using code_effect_return, code_effect_fail, code_effect_pure,
        code_effect_check, code_effect_lift, code_effect_get_const, code_effect_put_const,
        code_effect_need_named, code_effect_join]
    | lazymatch goal with
      | |- PreservesOldCode (EaExpr.read_global_value _ _ _ _ _) =>
        unfold EaExpr.read_global_value; expression_effect
      | |- PreservesOldCode (EaExpr.read_constant _ _ _ _ _ _) =>
        unfold EaExpr.read_constant; expression_effect
      | |- PreservesOldCode (mbind _ _) =>
        apply code_effect_bind; [expression_effect|intro; cbn beta iota zeta; expression_effect]
      | |- PreservesOldCode (miter _ _) =>
        apply code_effect_miter; intro; expression_effect
      | |- PreservesOldCode ?action =>
        lazymatch action with
        | context [match ?decision with _ => _ end] =>
          destruct decision; cbn beta iota zeta; expression_effect
        end
      end ].

Theorem expression_code_effects : forall E R fuel,
  (forall F C e expected, PreservesOldCode (r_ex R F C e expected)) ->
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  ExpressionCodeEffects E R fuel.
Proof.
  intros E R fuel Expr Bind. induction fuel as [|fuel IH].
  - constructor; intros; apply code_effect_fail.
  - destruct IH.
    constructor; intros;
      cbn [EaExpr.ex EaExpr.read_global_value EaExpr.read_constant EaExpr.mmap_fields EaExpr.args EaExpr.cond EaExpr.branch EaExpr.arms
        EaExpr.arms_list EaExpr.meth_call EaExpr.provided EaExpr.blke EaExpr.blk];
      expression_effect.
Qed.


Lemma mkrec_code_effect : forall call,
  (forall query, PreservesOldCode (call query)) ->
  let R := EaMain.mkrec call in
  (forall F C e expected, PreservesOldCode (r_ex R F C e expected)) /\
  (forall F C block expected, PreservesOldCode (r_blke R F C block expected)) /\
  (forall F C items, PreservesOldCode (r_bind_items R F C items)).
Proof.
  intros call Every. cbn [EaMain.mkrec r_ex r_blke r_bind_items]. split; [|split].
  all: intros; apply code_effect_bind; [apply Every|].
  all: intros []; first [apply code_effect_return|apply code_effect_fail].
Qed.

Theorem run_code_effect : forall E fuel query, PreservesOldCode (EaMain.run E fuel query).
Proof.
  intros E fuel. induction fuel as [|fuel IH]; intro query; cbn [EaMain.run].
  - apply code_effect_fail.
  - destruct (mkrec_code_effect _ IH) as (Expr & Block & Bind).
    pose proof (expression_code_effects E (EaMain.mkrec (EaMain.run E fuel))
      (e_fuel E) Expr Bind) as Effects.
    destruct query.
    all: apply code_effect_bind; [|intro result; apply code_effect_return].
    + apply (ex_code_effect _ _ _ Effects).
    + apply (block_code_effect _ _ _ Effects).
    + apply EaBindCodeEffects.bind_items_preserve_code; assumption.
Qed.
