(* Function and constant typing choose source body plans. Translation
   supplies the numbering and relates a function plan to its emitted slot. *)
From Stdlib Require Import List.
From NM Require AuditBase.
From NM Require Import Audit BodyTyping.
From NM Require BodyReadback.
From NM Require Import LegacyBodyJudgments.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

Theorem function_translation_agrees : forall W P resolve f,
  FunctionPlanAt W P resolve f ->
  FnElab W P f.
Proof.
  intros W P resolve f Plans d receiver n parameters annotation body Declared Ast.
  destruct (Plans _ _ _ _ _ _ Declared Ast) as [[C needs this signature F code result] [Typed Emitted]].
  cbn [TypesFunction TranslatesFunction function_context function_needs function_receiver function_signature
    function_layout function_body function_result function_parameters emit_function] in *.
  destruct Typed as (Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins & Body).
  destruct Emitted as (Numbered & Placed & Emitted).
  exists C, needs, this, signature, F, (emitted_code (block_emission resolve (without_pins F)
    (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
      (combine (map fst parameters) (sg_params signature)) C) code)), result.
  pose proof (proj1 (proj2 (body_translation_agrees W P resolve)) _ _ _ _ _ _ Body Placed) as TypedBody.
  repeat first [assumption|split].
Qed.

Theorem constant_translation_agrees : forall W P resolve d annotation initial plan,
  TypesConstantInitializer W d annotation initial plan -> TranslatesConstant W P resolve plan ->
  ConstElab W P d annotation initial.
Proof.
  intros W P resolve d annotation initial [F code declared actual] Typed [Numbered Placed].
  cbn [TypesConstantInitializer emit_constant constant_layout constant_body constant_declared constant_actual] in *.
  destruct Typed as (Assumes & TypeRead & Body & Fits & Module & Return & This & Loop & Fresh & Applying & Pins).
  split; [exact Assumes|].
  exists declared. split; [exact TypeRead|].
  exists F, (emitted_code (expression_emission resolve F (Cx [] [] [] [] []) code)), actual.
  pose proof (proj1 (body_translation_agrees W P resolve) _ _ _ _ _ _ Body Placed) as TypedBody.
  repeat first [assumption|split].
Qed.

Theorem function_plan_readback : forall W P,
  forall f d receiver n parameters annotation body,
  FnElab W P f -> w_decl W f = Some d -> d_ast d = ADFn receiver n parameters annotation (Some body) ->
  exists plan, TypesFunction W f d receiver n (map fst parameters) body plan /\
    TranslatesFunction W P identity_slots f (map fst parameters) plan.
Proof.
  intros W P f d receiver n parameters annotation body Typed Declared Ast.
  destruct (Typed _ _ _ _ _ _ Declared Ast) as (C & needs & this & g & F & code & t &
    Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins & Numbered & Body & Emitted).
  destruct (BodyReadback.block_has_plan W P _ _ _ _ _ _ Body) as (plan & Checked & Emit & Placed).
  exists (FunctionPlan C needs this g F plan t). split.
  - unfold TypesFunction, function_parameters. cbn. repeat first [assumption|split].
  - unfold TranslatesFunction, emit_function, function_parameters. cbn.
    unfold translate_block in Emit. rewrite Emit. repeat first [assumption|split].
Qed.

Theorem constant_plan_readback : forall W P,
  forall d annotation initial,
  ConstElab W P d annotation initial ->
  exists plan, TypesConstantInitializer W d annotation initial plan /\ TranslatesConstant W P identity_slots plan.
Proof.
  intros W P d annotation initial
    (Assumes & declared & TypeRead & F & code & actual & Body & Fits & Module & Return & This & Loop &
      Fresh & Applying & Pins & Numbered).
  destruct (BodyReadback.expression_has_plan W P _ _ _ _ _ _ Body) as (plan & Checked & _ & Placed).
  exists (ConstantPlan F plan declared actual). unfold TypesConstantInitializer, TranslatesConstant.
  cbn. repeat first [assumption|split].
Qed.

Lemma map_declaration_forms : forall W first second,
  (forall d annotation initial, first d annotation initial -> second d annotation initial) ->
  forall symbol, DeclFormsWith W first symbol -> DeclFormsWith W second symbol.
Proof.
  intros W first second Convert symbol Typed d Declared.
  specialize (Typed d Declared). destruct (d_ast d); try exact Typed.
  destruct Typed as [TypeRead Body]. split; [exact TypeRead|]. auto.
Qed.

Lemma map_elaboration_bodies : forall bundle W P first_fn second_fn first_const second_const,
  (forall f, first_fn f -> second_fn f) ->
  (forall d annotation initial, first_const d annotation initial -> second_const d annotation initial) ->
  ElaboratesWith bundle W P first_fn first_const ->
  ElaboratesWith bundle W P second_fn second_const.
Proof.
  intros bundle W P first_fn second_fn first_const second_const Function Constant
    (World & Functions & Entry & Main & Environment & Bool & Lang & Assumes & Decls).
  assert (Functions' : forall f d rcv n ps rt body,
    w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) -> second_fn f).
  { intros. apply Function. eapply Functions; eassumption. }
  assert (Decls' : forall s, DeclFormsWith W second_const s).
  { intro s. exact (map_declaration_forms W first_const second_const Constant s (Decls s)). }
  unfold ElaboratesWith. repeat first [assumption|split].
Qed.

Theorem separated_elaboration_sound : forall bundle W P,
  ElaboratesIn bundle W P -> LegacyElaboratesIn bundle W P.
Proof.
  intros bundle W P [resolve Typed]. eapply map_elaboration_bodies; [| |exact Typed].
  - apply function_translation_agrees.
  - intros d annotation initial [plan [TypedBody Numbered]].
    exact (constant_translation_agrees W P resolve d annotation initial plan TypedBody Numbered).
Qed.

Theorem separated_elaboration_readback : forall bundle W P,
  LegacyElaboratesIn bundle W P -> ElaboratesIn bundle W P.
Proof.
  intros bundle W P Typed. exists identity_slots.
  eapply map_elaboration_bodies; [| |exact Typed].
  - intros f Function d receiver n parameters annotation body Declared Ast.
    exact (function_plan_readback W P f d receiver n parameters annotation body Function Declared Ast).
  - apply constant_plan_readback.
Qed.

Theorem function_typing_renumber : forall W f d receiver name parameters body
    C needs this signature F plan result slots,
  TypesFunction W f d receiver name parameters body (FunctionPlan C needs this signature F plan result) ->
  SameLocalNames F slots ->
  TypesFunction W f d receiver name parameters body
    (FunctionPlan C needs this signature (renumber_body_frame F slots) plan result).
Proof.
  intros W f d receiver name parameters body C needs this signature F plan result slots Typed Names.
  unfold TypesFunction in *. cbn [function_context function_needs function_receiver function_signature
    function_layout function_body function_result function_parameters renumber_body_frame] in *.
  destruct Typed as (Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins & Body).
  pose proof (proj1 (proj2 (body_typing_renumber W)) _ _ _ _ _ _ Body slots Names) as Body'.
  repeat first [assumption|split].
Qed.

Theorem constant_typing_renumber : forall W d annotation initial F plan declared actual slots,
  TypesConstantInitializer W d annotation initial (ConstantPlan F plan declared actual) ->
  SameLocalNames F slots ->
  TypesConstantInitializer W d annotation initial (ConstantPlan (renumber_body_frame F slots) plan declared actual).
Proof.
  intros W d annotation initial F plan declared actual slots Typed Names.
  unfold TypesConstantInitializer in *. cbn [constant_layout constant_body constant_declared constant_actual renumber_body_frame] in *.
  destruct Typed as (Assumes & TypeRead & Body & Fits & Module & Return & This & Loop & Fresh & Applying & Pins).
  pose proof (proj1 (body_typing_renumber W) _ _ _ _ _ _ Body slots Names) as Body'.
  repeat first [assumption|split].
Qed.
