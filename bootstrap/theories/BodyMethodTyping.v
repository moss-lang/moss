(* Method selection records the source provision and its type. Translation
   turns that provision into a runtime key, with any family application. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Theorem provided_method_translation : forall W P F C receiver method choice signature syntax object arguments,
  TypesMethodProvision W C receiver method choice signature ->
  Args W P F C syntax (sg_params signature) arguments ->
  Provided W P F C object receiver method syntax
    (emit_provided_method C receiver method choice object arguments) (sg_ret signature).
Proof.
  intros W P F C receiver method choice signature syntax object arguments Typed Checked.
  destruct Typed; cbn [emit_provided_method demand_result]; [eapply PrIn|eapply PrFamily]; eassumption.
Qed.

Theorem provided_method_readback : forall W P F C object receiver method syntax code t,
  Provided W P F C object receiver method syntax code t ->
  exists choice signature arguments,
    TypesMethodProvision W C receiver method choice signature /\
    Args W P F C syntax (sg_params signature) arguments /\
    t = sg_ret signature /\ code = emit_provided_method C receiver method choice object arguments.
Proof.
  intros W P F C object receiver method syntax code t Typed.
  destruct Typed.
  - exists None, g, xs. split; [now constructor|auto].
  - exists (Some (FamilyUse u matching th formal needs C' ks)), g, xs.
    split; [now constructor|auto].
Qed.

Theorem literal_method_translation : forall W C t m choice x y,
  TypesMethodProvision W C t m choice (FSig [t] t) ->
  LitOp W C t m x y (emit_provided_method C t m choice x [y]).
Proof.
  intros W C t m choice x y Typed. inversion Typed; subst; cbn [emit_provided_method demand_result].
  - now apply LitOpIn.
  - eapply LitOpFamily; eassumption.
Qed.

Theorem literal_method_readback : forall W C t m x y code,
  LitOp W C t m x y code ->
  exists choice, TypesMethodProvision W C t m choice (FSig [t] t) /\
    code = emit_provided_method C t m choice x [y].
Proof.
  intros W C t m x y code Typed. destruct Typed.
  - exists None. split; [now constructor|reflexivity].
  - exists (Some (FamilyUse u matching th formal needs C' ks)).
    split; [now constructor|reflexivity].
Qed.

Theorem method_call_translation : forall W P F C receiver path selected signature syntax object arguments,
  TypesMethodSelection W F C receiver path selected signature ->
  Args W P F C syntax (sg_params signature) arguments ->
  MethCall W P F C object receiver path syntax (emit_method_call C receiver selected object arguments)
    (sg_ret signature).
Proof.
  intros W P F C receiver path selected signature syntax object arguments Typed Checked.
  destruct Typed; cbn [emit_method_call].
  - eapply MCQualified; [eassumption|eassumption|eapply provided_method_translation; eassumption].
  - eapply MCProvisionWins; try eassumption. eapply provided_method_translation; eassumption.
  - eapply MCAttachedDefined; try eassumption. reflexivity.
  - eapply MCAttachedAbstract; try eassumption. eapply provided_method_translation; eassumption.
  - eapply MCDetached; try eassumption. eapply provided_method_translation; eassumption.
Qed.

Theorem method_call_readback : forall W P F C object receiver path syntax code t,
  MethCall W P F C object receiver path syntax code t ->
  exists selected signature arguments,
    TypesMethodSelection W F C receiver path selected signature /\
    Args W P F C syntax (sg_params signature) arguments /\
    t = sg_ret signature /\ code = emit_method_call C receiver selected object arguments.
Proof.
  intros W P F C object receiver path syntax code t Typed. destruct Typed.
  all: try match goal with
  | Read : Provided _ _ _ _ _ _ ?m _ _ _ |- _ =>
      destruct (provided_method_readback _ _ _ _ _ _ _ _ _ _ Read)
        as (choice & signature & values & Provision & Checked & -> & ->);
      exists (SelectedProvision m choice), signature, values;
      split; [econstructor; eassumption|auto]
  end.
  subst rargs. exists (SelectedDefinition att Ca ns th demands Cd), g, xs.
  split; [econstructor; eassumption|auto].
Qed.
