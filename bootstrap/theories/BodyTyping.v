(* Relate source typing and structural translation to the combined
   judgments used by the safety proof. *)
From Stdlib Require Import List String Bool.
From NM Require AuditBase.
From NM Require Import Audit BodyLiteralTyping BodyMethodTyping.
From NM Require Import LegacyBodyJudgments.
From NM Require BodyFunctionProviderTyping BodyProviderTyping.
From NM Require BodyLinkTyping BodyLambdaTyping.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition translate_field_patterns (F : frame) :=
  emit_field_patterns (fr_var F).

Definition translate_pattern (F : frame) := emit_pattern (fr_var F).

Definition argument_emission (resolve : nat -> nat) (F : frame) (C : cx) :=
  emit_each (expression_emission resolve F C).
Definition record_fields_emission (resolve : nat -> nat) (F : frame) (C : cx)
    (fields : list (fld * expression_plan)) :=
  emit_each (fun '(n, child) => emit_map (pair n) (expression_emission resolve F C child)) fields.
Definition arms_emission (resolve : nat -> nat) (F : frame) (C : cx) :=
  emit_each (fun '(pattern, body) => emit_map (pair (translate_pattern F pattern))
    (expression_emission resolve F (pattern_scope C pattern) body)).
Definition bind_item_emission (resolve : nat -> nat) (F : frame) := emit_bind_item resolve (fr_var F).
Definition bind_items_emission (resolve : nat -> nat) (F : frame) :=
  emit_flat (bind_item_emission resolve F).

Theorem field_pattern_translation : forall W F fields source plan,
  TypesFieldPatterns W F fields source plan ->
  FieldPats W F fields source (field_pattern_bindings plan) (translate_field_patterns F plan).
Proof. intros W F fields source plan Typed; induction Typed; cbn; eauto using FPNil, FPPun, FPWild, FPBind. Qed.

Theorem field_pattern_has_plan : forall W F fields source bindings code,
  FieldPats W F fields source bindings code ->
  exists plan, TypesFieldPatterns W F fields source plan /\
    field_pattern_bindings plan = bindings /\ translate_field_patterns F plan = code.
Proof.
  intros W F fields source bindings code Typed.
  induction Typed as [|n t rest bindings code Read Named Rest IH
    |n t rest bindings code Read Rest IH|n binder t rest bindings code Read Resolve Named Rest IH].
  - exists []. repeat split. constructor.
  - destruct IH as (plan & Types & <- & <-). exists (PatternPun n t :: plan).
    repeat split. now constructor.
  - destruct IH as (plan & Types & <- & <-). exists (PatternIgnore n :: plan).
    repeat split. eapply TypesFieldsIgnore; eassumption.
  - destruct IH as (plan & Types & <- & <-). exists (PatternRename n binder t :: plan).
    repeat split. now constructor.
Qed.

Theorem pattern_translation : forall W F scrutinee source plan,
  TypesPattern W F scrutinee source plan ->
  PatOf W F scrutinee source (translate_pattern F plan)
    (pattern_bindings plan) (pattern_heads plan) (pattern_catches plan).
Proof.
  intros W F scrutinee source plan Typed. destruct Typed; cbn;
    eauto using PWild, PBinder, PNominal, PTagBind, PRecord, field_pattern_translation.
Qed.

Theorem pattern_has_plan : forall W F scrutinee source code bindings heads catch,
  PatOf W F scrutinee source code bindings heads catch ->
  exists plan, TypesPattern W F scrutinee source plan /\ translate_pattern F plan = code /\
    pattern_bindings plan = bindings /\ pattern_heads plan = heads /\ pattern_catches plan = catch.
Proof.
  intros W F scrutinee source code bindings heads catch Typed.
  destruct Typed as [|n Resolved Named|path tag arguments Resolved Kind Member
    |path tag arguments binder payload Resolved Kind Member Payload Read Named
    |path tag arguments fields field_types bindings code Resolved Kind Member Payload Fields Distinct].
  - exists PatternWildcard. repeat split. constructor.
  - exists (PatternBinder n scrutinee). repeat split. now constructor.
  - exists (PatternNominal tag). repeat split. eapply TypesPatternNominal; eassumption.
  - exists (PatternPayload tag binder payload). repeat split. eapply TypesPatternPayload; eassumption.
  - destruct (field_pattern_has_plan _ _ _ _ _ _ Fields) as (plan & Typed & <- & <-).
    exists (PatternRecord tag plan). repeat split. eapply TypesPatternRecord; eassumption.
Qed.

Lemma binding_plan_numbering : forall W F C item plan result F' made,
  TypesBindItem W F C item plan result F' made -> fr_var F' = fr_var F.
Proof. intros W F C item plan result F' made Typed. destruct Typed; reflexivity. Qed.

Lemma bind_plans_numbering : forall W F C items plans result F' made,
  TypesBindItems W F C items plans result F' made -> fr_var F' = fr_var F.
Proof.
  intros W F C items plans result F' made Typed. induction Typed; [reflexivity|].
  rewrite IHTyped. eapply binding_plan_numbering; eassumption.
Qed.

Lemma splice_bindings_append : forall left right,
  splice_bindings (left ++ right) = fun body => splice_bindings left (splice_bindings right body).
Proof.
  induction left as [|b left IH]; intro right; [reflexivity|].
  change ((fun body => EBind b (splice_bindings (left ++ right) body)) =
    fun body => EBind b (splice_bindings left (splice_bindings right body))).
  now rewrite IH.
Qed.

Lemma function_placements_exact : forall W P resolve signature provider,
  placements_in W P (function_placements resolve signature provider) <->
  BodyFunctionProviderTyping.FunctionProviderPlacement W P resolve signature provider.
Proof.
  intros W P resolve signature [named|occurrence handle]; cbn [function_placements BodyFunctionProviderTyping.FunctionProviderPlacement];
    unfold placements_in; [split; [tauto|intros; constructor]|].
  rewrite Forall_cons_iff. cbn [placement_in]. unfold TranslatesForwarder.
  split; [tauto|]. intros [Fresh Body]. split; [tauto|constructor].
Qed.

Lemma lambda_prefix_bindings : forall F provider,
  splice_bindings (lambda_capture_bindings (fr_var F) provider) = BodyLambdaTyping.lambda_descriptor_prefix F provider.
Proof.
  intros F provider. unfold lambda_capture_bindings, BodyLambdaTyping.lambda_descriptor_prefix, translate_lambda_captures.
  induction (lambda_descriptor_captures provider) as [|[n v] rest IH]; [reflexivity|].
  change ((fun body => EBind (BValue v (EVar (local_number (fr_var F) n)))
    (splice_bindings (map (fun '(n, symbol) => BValue symbol (EVar (local_number (fr_var F) n))) rest) body)) =
    fun body => bind_value_expression v (EVar (var_of F n))
      (fold_right (fun q rest => bind_value_expression (snd q) (EVar (var_of F (fst q))) rest) body rest)).
  now rewrite IH.
Qed.

Definition identity_slots (token : nat) := token.

Definition binding_activity_of (instruction : binding) : binding_activity :=
  match instruction with
  | BValue _ _ => BindValueActivity
  | BApply family _ _ _ _ => BindApplyActivity family
  | _ => BindOtherActivity
  end.

Lemma bind_item_activity_emitted : forall resolve slots plan,
  map binding_activity_of (emitted_code (emit_bind_item resolve slots plan)) = binding_plan_activities plan.
Proof.
  intros resolve slots. fix Visit 1. intros plan. destruct plan;
    cbn [emit_bind_item emit_only emit_map emit_flat emit_lambda_binding emitted_code binding_plan_activities];
    try reflexivity.
  - revert items. fix walk 1. intros [|step rest]; [reflexivity|].
    cbn [flat_map]. now rewrite map_app, (Visit step), (walk rest).
  - unfold lambda_capture_bindings. rewrite map_app, map_map.
    f_equal. apply map_ext. now intros [n captured].
  - unfold lambda_capture_bindings. rewrite map_app, map_map.
    f_equal. apply map_ext. now intros [n captured].
Qed.

Theorem family_bind_emission_exact : forall W resolve F plans,
  FamilyBindPlans W plans <-> FamilyBindings W (emitted_code (bind_items_emission resolve F plans)).
Proof.
  intros W resolve F plans.
  assert (Activities : map binding_activity_of (emitted_code (bind_items_emission resolve F plans)) =
    flat_map binding_plan_activities plans).
  { induction plans as [|plan plans IH]; [reflexivity|].
    change (map binding_activity_of
      (emitted_code (emit_bind_item resolve (fr_var F) plan) ++
       emitted_code (bind_items_emission resolve F plans)) =
      binding_plan_activities plan ++ flat_map binding_plan_activities plans).
    now rewrite map_app, bind_item_activity_emitted, IH. }
  unfold FamilyBindPlans, FamilyBindings. rewrite <- Activities, Forall_map.
  split; apply Forall_impl; now intros [].
Qed.

Definition translate_expr (F : frame) (C : cx) (plan : expression_plan) :=
  emitted_code (expression_emission identity_slots F C plan).
Definition translate_block (F : frame) (C : cx) (plan : block_plan) :=
  emitted_code (block_emission identity_slots F C plan).
Definition translate_bind_item (F : frame) (plan : binding_plan) :=
  splice_bindings (emitted_code (bind_item_emission identity_slots F plan)).
Definition translate_bind_items (F : frame) (plans : list binding_plan) :=
  splice_bindings (emitted_code (bind_items_emission identity_slots F plans)).

Lemma translate_block_numbering : forall F F' C plan,
  fr_var F = fr_var F' -> translate_block F C plan = translate_block F' C plan.
Proof. intros F F' C plan Same. unfold translate_block, block_emission. now rewrite Same. Qed.

Definition translate_record_fields (F : frame) (C : cx) (fields : list (fld * expression_plan)) :=
  emitted_code (record_fields_emission identity_slots F C fields).

Definition translate_arms (F : frame) (C : cx) (arms : list (pattern_plan * expression_plan)) :=
  emitted_code (arms_emission identity_slots F C arms).

Scheme types_expr_ind := Induction for TypesExpr Sort Prop
with types_blke_ind := Induction for TypesBlkE Sort Prop
with types_blk_ind := Induction for TypesBlk Sort Prop
with types_args_ind := Induction for TypesArgs Sort Prop
with types_cond_ind := Induction for TypesCond Sort Prop
with types_branch_ind := Induction for TypesBranch Sort Prop
with types_fields_ind := Induction for TypesRecordFields Sort Prop
with types_arms_ind := Induction for TypesArms Sort Prop
with types_arm_list_ind := Induction for TypesArmList Sort Prop
with types_bind_items_ind := Induction for TypesBindItems Sort Prop
with types_bind_item_ind := Induction for TypesBindItem Sort Prop.
Combined Scheme body_typing_ind from types_expr_ind, types_blke_ind, types_blk_ind, types_args_ind,
  types_cond_ind, types_branch_ind, types_fields_ind, types_arms_ind, types_arm_list_ind,
  types_bind_items_ind, types_bind_item_ind.

Definition CheckedEmission {A} (W : world) (P : program)
    (output : emission A) (judgment : A -> Prop) : Prop :=
  placements_in W P (emitted_placements output) -> judgment (emitted_code output).

Local Ltac use_placements :=
  try unfold lambda_binding_placements in *;
  try unfold linked_binding_placements in *;
  unfold placements_in in *;
  repeat match goal with
  | Placed : Forall (placement_in _ _) (_ ++ _) |- _ => apply Forall_app in Placed as [? ?]
  | Placed : Forall (placement_in _ _) (_ :: _) |- _ => apply Forall_cons_iff in Placed as [? ?]
  end;
  cbn [placement_in] in *;
  repeat match goal with
  | Step : Forall (placement_in ?w ?p) ?slots -> _ |- _ =>
      let Checked := fresh "Checked" in
      assert (Checked : Forall (placement_in w p) slots) by assumption;
      specialize (Step Checked); clear Checked
  end.

Local Lemma method_binding_from_header : forall W P F C path label brackets method receiver signature
    arguments source target types prefix provisions,
  TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
  MethProvider W P F C source signature receiver target types prefix provisions ->
  BindItem W P F C (Spec (Some path) (Some label) brackets, Some source)
    (fun body => prefix (bind_method_expression (canon C receiver) method (theta_of C arguments)
      target types provisions body))
    (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F [].
Proof.
  intros W P F C path label brackets method receiver signature arguments source target types prefix provisions
    (receiver_symbol & readings & Resolve & Method & Brackets & Receiver & Signature & Application & Family & Abstract)
    Provider.
  eapply BMethBind; eassumption.
Qed.

Local Lemma lambda_from_header : forall W P F C ps ann body signature this layout
    (provider : lambda_body_descriptor block_plan) result code resolve,
  TypesLambdaHeader W F C ps ann body signature this layout provider result ->
  BlkE W P layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
    (Some (sg_ret signature)) code result ->
  Numbering layout (fr_this layout) (map fst ps) ->
  BodyLambdaTyping.LambdaBodyPlacement W P C layout resolve provider code ->
  LambdaProvider W P F C ps ann body signature this (resolve (lambda_occurrence provider))
    (BodyLambdaTyping.lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider).
Proof.
  intros W P F C ps ann body signature this layout provider result code resolve Typed Body Numbered
    [Fresh [Code Origin]].
  destruct Typed as [caps capvals Cl Length Distinct Annotations Return Capture Context
    Module Ret This Loop Keep NamedBody NamedOuter Fits Needs Unique].
  rewrite This in Numbered.
  eapply (@LP W P F C ps ann body signature this _ caps capvals Cl layout code result
    (lambda_provision_needs provider) (lambda_descriptor_needs C provider)); try eassumption; reflexivity.
Qed.

Local Lemma linked_from_header : forall W P resolve layout descriptor items result nested made,
  TypesLinkedHeader W layout descriptor items result made ->
  (Family W (linked_target_symbol (linked_handle descriptor)) ->
    FamilyBindPlans W (linked_bind_plan descriptor)) ->
  BindItems W P layout (linked_provider_context descriptor) items
    (splice_bindings (emitted_code (bind_items_emission resolve layout (linked_bind_plan descriptor))))
    result nested made ->
  Numbering layout None [] ->
  BodyLinkTyping.LinkedBodyPlacement W P resolve descriptor
    (emitted_code (bind_items_emission resolve layout (linked_bind_plan descriptor))) ->
  LinkBody W P (linked_target_symbol (linked_handle descriptor)) (linked_provider_symbol (linked_handle descriptor))
    (resolve (linked_occurrence (linked_handle descriptor))) (link_plan_pins (linked_result_reading descriptor)).
Proof.
  intros W P resolve layout descriptor items result nested made Typed FamilyAllowed Body Numbered Placed.
  destruct Typed as [dg name parameters inputs outputs bodies target_needs provider_needs Cb created gen promised
    target provider Cu Cg outs reading Declared Ast Target Provider Module Ret This Loop Protected
    NoInput Outputs Checked].
  set (plan := linked_bind_plan descriptor) in *.
  destruct Checked as [_ Site PinsProvision ResultTypes PinsExact PinsScoped].
  set (bindings := emitted_code (bind_items_emission resolve layout plan)) in *.
  assert (FamilyCode : Family W target -> FamilyBindings W bindings).
  { intro Family. apply (proj1 (family_bind_emission_exact W resolve layout plan)). now apply FamilyAllowed. }
  assert (Translated : TranslatesLinkedBody W P target provider (resolve (linked_occurrence (linked_handle descriptor)))
    Cu Cg (link_plan_site reading) outs bindings (link_plan_keys reading)) by exact Placed.
  rewrite Site in PinsProvision, ResultTypes, Translated.
  eapply (@LB W P target provider _ dg name parameters inputs outputs bodies Cu target_needs Cg provider_needs
    layout (splice_bindings bindings) Cb nested created outs gen promised (link_plan_keys reading) bindings
    (link_plan_pins reading)); try eassumption.
  apply BodyLinkTyping.spliced_link_bindings.
Qed.

Theorem body_translation_agrees : forall W P resolve,
  (forall F C e expected plan t, TypesExpr W F C e expected plan t ->
    CheckedEmission W P (expression_emission resolve F C plan)
      (fun code => Ex W P F C e expected code t)) /\
  (forall F C block expected plan t, TypesBlkE W F C block expected plan t ->
    CheckedEmission W P (block_emission resolve (without_pins F) C plan)
      (fun code => BlkE W P F C block expected code t)) /\
  (forall F C statements tail expected plan t made, TypesBlk W F C statements tail expected plan t made ->
    CheckedEmission W P (block_emission resolve F C plan)
      (fun code => Blk W P F C statements tail expected code t made)) /\
  (forall F C args types plans, TypesArgs W F C args types plans ->
    CheckedEmission W P (argument_emission resolve F C plans) (Args W P F C args types)) /\
  (forall F C condition plan, TypesCond W F C condition plan ->
    CheckedEmission W P (expression_emission resolve F C plan) (Cond W P F C condition)) /\
  (forall F C branch expected plan t, TypesBranch W F C branch expected plan t ->
    CheckedEmission W P (expression_emission resolve F C plan)
      (fun code => Branch W P F C branch expected code t)) /\
  (forall F C given fields plans, TypesRecordFields W F C given fields plans ->
    CheckedEmission W P (record_fields_emission resolve F C plans)
      (Forall2 (fun ft xf => fst xf = fst ft /\
        exists n source actual, n = fst ft /\ In (n, source) given /\
          Ex W P F C (match source with Some e => e | None => AEPath [n] None end)
            (Some (snd ft)) (snd xf) actual /\ fits actual (snd ft)) fields)) /\
  (forall F C st arms exp plans t, TypesArms W F C st arms exp plans t ->
    CheckedEmission W P (arms_emission resolve F C plans)
      (fun code => Arms W P F C st arms exp code t)) /\
  (forall F C st arms exp plans covered catch t, TypesArmList W F C st arms exp plans covered catch t ->
    CheckedEmission W P (arms_emission resolve F C plans)
      (fun code => ArmsList W P F C st arms exp code covered catch t)) /\
  (forall F C items plans result F' made, TypesBindItems W F C items plans result F' made ->
    CheckedEmission W P (bind_items_emission resolve F plans)
      (fun code => BindItems W P F C items (splice_bindings code) result F' made)) /\
  (forall F C item plan result F' made, TypesBindItem W F C item plan result F' made ->
    CheckedEmission W P (bind_item_emission resolve F plan)
      (fun code => BindItem W P F C item (splice_bindings code) result F' made)).
Proof.
  intros W P resolve. apply body_typing_ind; intros; unfold CheckedEmission in *;
    cbn [expression_emission block_emission argument_emission record_fields_emission
      arms_emission bind_items_emission bind_item_emission emit_expr emit_block emit_bind_item
      emit_only emit_map emit_pair emit_each emit_flat require_placement emitted_code emitted_placements
      emit_lambda_binding lambda_binding_placements linked_binding_placements
      splice_bindings fold_right map flat_map app] in *;
    intros; use_placements;
    eauto 8 using ExUnit, ExThis, ExLocal, ExBlock, ExIfThen, ExIfElse, ExReturnUnit, ExReturn,
    ExBreak, ExCallD, ExCallA, ExBareTag, ExTag, ExRecord, ExField, ExMatch, ExMeth, ExUnary, ExBinary,
    ExLit, ExVal, ExCallBr, provided_method_translation, method_call_translation, literal_translation,
    BlkECheck, BlkTail, BlkEmpty, BlkLet, BlkAssign, BlkExprLast, BlkExpr, BlkWhile, BlkLoop,
    BlkLoopLast, ArgsNil, ArgsCons, CondBool, BranchBlock, BranchExpr, Forall2_nil, ArmsEmptyNever,
    ArmsSome, ArmsOne, ArmsMore, pattern_translation, BINil, BTypeBind, BValBind, BApplyBody,
    BApplyAbstract, BFunCopy.
  - eapply ExConst; try eassumption. exists layout, actual. repeat first [assumption | split].
  - (* The outgoing scope changes typing, while the local numbers persist. *)
    lazymatch goal with
    | Typed : TypesBindItems W F C items plans result F' made,
      Tail : Forall _ _ -> Blk W P F' result rest tail exp _ t created |- _ =>
      let Same := fresh "Same" in
      pose proof (bind_plans_numbering _ _ _ _ _ _ _ _ Typed) as Same;
      unfold block_emission in Tail;
      rewrite Same in Tail;
      eapply (@BlkBind W P F C items rest tail exp
        (splice_bindings (emitted_code (bind_items_emission resolve F plans))) result F'
        (emitted_code (block_emission resolve F result continuation)) t made created);
        [eassumption|eapply Tail; eassumption]
    end.
  - constructor; [|eauto]. cbn [fst snd]. split; [reflexivity|].
    exists n, syntax, actual. repeat split; eauto.
  - lazymatch goal with
    | Typed : TypesBindItem W F C ?source_item plan middle F1 made1,
      Tail : Forall _ _ -> BindItems W P F1 middle rest _ result F2 made2 |- _ =>
      let Same := fresh "Same" in
      pose proof (binding_plan_numbering _ _ _ _ _ _ _ _ Typed) as Same;
      unfold bind_item_emission in Tail;
      rewrite Same in Tail;
      rewrite splice_bindings_append;
      eapply BICons; [eassumption|eapply Tail; eassumption]
    end.
  - eapply BFnBind; try eassumption.
    eapply BodyFunctionProviderTyping.function_provider_plan_translates;
      [eassumption|now apply function_placements_exact].
  - eapply (@method_binding_from_header W P F C path0 label brackets method receiver signature arguments
      (AEField (AEPath provider None) name0) (provider_symbol plan) (named_provider_capture plan)
      (fun body => body) (named_provider_provisions site plan)); [eassumption|].
    eapply BodyProviderTyping.attached_method_plan_translates; eassumption.
  - eapply (@method_binding_from_header W P F C path0 label brackets method receiver signature arguments
      (AEPath provider None) (resolve (wrapped_method_occurrence plan))
      (named_provider_capture (wrapped_method_provider plan)) (fun body => body)
      (named_provider_provisions C (wrapped_method_provider plan))); [eassumption|].
    eapply BodyFunctionProviderTyping.wrapped_method_plan_translates; [eassumption|].
    unfold BodyFunctionProviderTyping.WrappedMethodPlacement. tauto.
  - rewrite splice_bindings_append, lambda_prefix_bindings.
    eapply (@BFnLambda W P F C path0 f signature ps ann body (resolve (lambda_occurrence provider))
      (BodyLambdaTyping.lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider) arguments); try eassumption.
    eapply (@lambda_from_header W P F C ps ann body signature None layout provider result
      (emitted_code (block_emission resolve layout (lambda_descriptor_site signature ps provider)
        (lambda_source_body provider))) resolve); eassumption.
  - rewrite splice_bindings_append, lambda_prefix_bindings.
    eapply (@method_binding_from_header W P F C path0 label brackets method receiver signature arguments
      (AELam ps ann body) (resolve (lambda_occurrence provider)) (lexical_capture C)
      (BodyLambdaTyping.lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider)); [eassumption|].
    apply MPLambda.
    eapply (@lambda_from_header W P F C ps ann body signature (Some receiver) layout provider result
      (emitted_code (block_emission resolve layout (lambda_descriptor_site signature ps provider)
        (lambda_source_body provider))) resolve); eassumption.
  - eapply (@BFunClosure W P F C path0 (linked_target_symbol (linked_handle descriptor)) provider
      (linked_provider_symbol (linked_handle descriptor)) (linked_target_context descriptor) target_needs
      (linked_provider_context descriptor) provider_needs0 (resolve (linked_occurrence (linked_handle descriptor)))
      (link_plan_pins (linked_result_reading descriptor)) selected
      (link_map (linked_target_context descriptor) (linked_provider_context descriptor) selected provider_needs0)
      (satisfaction_map C (linked_provider_context descriptor) (closure_arguments C (linked_provider_context descriptor))
        (captured_needs selected provider_needs0)) fixed); try eassumption; try reflexivity.
    + eapply linked_from_header; eassumption.
    + split; [reflexivity|eassumption].
Qed.

Definition HasExpressionPlan (W : world) (P : program) (F : frame) (C : cx) (source : aexpr) : Prop :=
  forall expected code t, Ex W P F C source expected code t ->
    exists plan, TypesExpr W F C source expected plan t /\ translate_expr F C plan = code.

Definition HasBranchPlan (W : world) (P : program) (F : frame) (C : cx) (source : aexpr) : Prop :=
  forall expected code t, Branch W P F C source expected code t ->
    exists plan, TypesBranch W F C source expected plan t /\ translate_expr F C plan = code.

Theorem bare_tag_source_has_plan : forall W P F C path tag,
  (forall n, path = [n] -> slook n (c_locals C) = None) ->
  resolve_sym W (fr_mod F) path = Some tag -> BareTag W tag ->
  HasExpressionPlan W P F C (AEPath path None).
Proof.
  intros W P F C path tag NotLocal Resolve Kind expected code t Typed. inversion Typed; subst.
  all: try solve [exfalso; match goal with
    | Read : slook ?name (c_locals ?context) = Some _ |- _ =>
      pose proof (NotLocal name eq_refl); congruence
    end].
  all: try solve [eexists; split; [eapply TypesBareTag; eassumption|reflexivity]].
  all: exfalso; destruct Kind as [bare_name Bare]; unfold ast_of in Bare.
  all: match goal with
  | Read : option_map d_ast (w_decl ?world ?bare) = Some (ADBareTag _),
    Declared : w_decl ?world ?symbol = Some ?d, Syntax : d_ast ?d = ADVal _ _ _ |- _ =>
    assert (Found : w_decl world bare = Some d) by congruence;
    rewrite Found in Read; cbn in Read; rewrite Syntax in Read; discriminate
  end.
Qed.

Theorem tag_source_has_plan : forall W P F C path brackets args tag,
  resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
  (forall child, In child args -> HasExpressionPlan W P F C child) ->
  HasExpressionPlan W P F C (AECall path brackets args).
Proof.
  intros W P F C path brackets args tag Resolve Kind Children expected code t Typed.
  inversion Typed; subst.
  all: try solve [
    match goal with
    | Function : kind_sym ?world ?symbol = Some DKFn,
      TagKind : kind_sym ?world ?target = Some DKTag |- _ => congruence
    end].
  lazymatch goal with
  | Child : Ex W P F C ?source (Some ?payload) ?value ?actual,
    Tag : PayloadOf W ?symbol ?arguments ?payload |- _ =>
    destruct (Children source (or_introl eq_refl) (Some payload) value actual Child) as (plan & Types & <-);
    exists (PlanTag symbol arguments plan); split; [eapply TypesTag; eassumption|reflexivity]
  end.
Qed.

Theorem record_fields_have_plan : forall W P F C given fields code,
  (forall n source, In (n, source) given ->
    HasExpressionPlan W P F C (match source with Some e => e | None => AEPath [n] None end)) ->
  Forall2 (fun ft xf => fst xf = fst ft /\
    exists n source actual, n = fst ft /\ In (n, source) given /\
      Ex W P F C (match source with Some e => e | None => AEPath [n] None end)
        (Some (snd ft)) (snd xf) actual /\ fits actual (snd ft)) fields code ->
  exists plan, TypesRecordFields W F C given fields plan /\ translate_record_fields F C plan = code.
Proof.
  intros W P F C given fields code Children Typed. induction Typed as
    [|[n t] [n' expression] fields code [Same (binder & source & actual & Field & Present & Child & Fits)] Tail IH].
  - exists []. split; [constructor|reflexivity].
  - cbn [fst snd] in *. subst binder n'.
    destruct (Children _ _ Present _ _ _ Child) as (plan & TypedPlan & <-).
    destruct IH as (plans & TypedFields & <-).
    exists ((n, plan) :: plans). split; [econstructor; eassumption|reflexivity].
Qed.

Theorem record_source_has_plan : forall W P F C path brackets given,
  (forall n source, In (n, source) given ->
    HasExpressionPlan W P F C (match source with Some e => e | None => AEPath [n] None end)) ->
  HasExpressionPlan W P F C (AERec path brackets given).
Proof.
  intros W P F C path brackets given Children expected code t Typed. inversion Typed; subst.
  lazymatch goal with
  | Payload : PayloadOf W ?tag ?arguments (TRecord ?field_types),
    Fields : Forall2 _ ?field_types ?field_code |- _ =>
    destruct (record_fields_have_plan W P F C given field_types field_code Children Fields)
      as (plans & TypedFields & <-);
    exists (PlanRecord tag arguments plans); split; [eapply TypesRecord; eassumption|reflexivity]
  end.
Qed.

Theorem field_source_has_plan : forall W P F C source field,
  HasExpressionPlan W P F C source -> HasExpressionPlan W P F C (AEField source field).
Proof.
  intros W P F C source field Object expected code t Typed. inversion Typed; subst.
  match goal with
  | Child : Ex W P F C source None ?value ?actual |- _ =>
    destruct (Object _ _ _ Child) as (plan & TypedPlan & <-);
    exists (PlanField plan field); split; [eapply TypesField; eassumption|reflexivity]
  end.
Qed.

Theorem arm_list_has_plan : forall W P F C st arms exp code covered catch t,
  ArmsList W P F C st arms exp code covered catch t ->
  (forall pat body pattern, In (pat, body) arms -> TypesPattern W F st pat pattern ->
    HasBranchPlan W P F (pattern_scope C pattern) body) ->
  exists plans, TypesArmList W F C st arms exp plans covered catch t /\ translate_arms F C plans = code.
Proof.
  intros W P F C st arms exp code covered catch t Typed.
  induction Typed as
    [F C st pat body exp pat_code binds covered catch body_code t Pat Body Fits
    |F C st pat body rest exp pat_code binds covered catch body_code bt rest_code rest_heads
      rest_catch rt t Nonempty Pat Body Fits Tail IH Join]; intros Children.
  - destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & TypedPat & <- & <- & <- & <-).
    destruct (Children _ _ _ (or_introl eq_refl) TypedPat _ _ _ Body) as (plan & TypedBody & <-).
    exists [(pattern, plan)]. split; [eapply TypesArmOne; eassumption|reflexivity].
  - destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & TypedPat & <- & <- & <- & <-).
    destruct (Children _ _ _ (or_introl eq_refl) TypedPat _ _ _ Body) as (plan & TypedBody & <-).
    destruct IH as (plans & TypedRest & <-).
    { intros p b choice Present. apply Children. now right. }
    exists ((pattern, plan) :: plans). split; [eapply TypesArmMore; eassumption|reflexivity].
Qed.

Theorem arms_have_plan : forall W P F C st arms exp code t,
  Arms W P F C st arms exp code t ->
  (forall pat body pattern, In (pat, body) arms -> TypesPattern W F st pat pattern ->
    HasBranchPlan W P F (pattern_scope C pattern) body) ->
  exists plans, TypesArms W F C st arms exp plans t /\ translate_arms F C plans = code.
Proof.
  intros W P F C st arms exp code t Typed Children.
  destruct Typed as [F C exp|F C st arms exp code covered catch t NotNever Typed Exhaustive].
  - exists []. split; [constructor|reflexivity].
  - destruct (arm_list_has_plan _ _ _ _ _ _ _ _ _ _ _ Typed Children) as (plans & TypedArms & <-).
    exists plans. split; [eapply TypesArmsSome; eassumption|reflexivity].
Qed.

Theorem match_source_has_plan : forall W P F C scrutinee arms,
  HasExpressionPlan W P F C scrutinee ->
  (forall st pat body pattern, In (pat, body) arms -> TypesPattern W F (canon C st) pat pattern ->
    HasBranchPlan W P F (pattern_scope C pattern) body) ->
  HasExpressionPlan W P F C (AEMatch scrutinee arms).
Proof.
  intros W P F C scrutinee arms Scrutinee Children exp code t Typed. inversion Typed; subst.
  match goal with
  | Object : Ex W P F C scrutinee None ?value ?st, ArmReads : Arms W P F C (canon C ?st) arms exp ?xs t |- _ =>
    destruct (Scrutinee None value st Object) as (plan & TypedObject & <-);
    destruct (arms_have_plan W P F C (canon C st) arms exp xs t ArmReads (Children st))
      as (plans & TypedArms & <-);
    exists (PlanMatch plan plans); split; [eapply TypesMatch; eassumption|reflexivity]
  end.
Qed.

(* Slot numbers are absent from the plan. Only the set of source names
   is checked, so translation can choose any valid numbering. *)
Definition renumber_body_frame (F : frame) (slots : list (name * nat)) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) slots (fr_fresh F) (fr_applying F) (fr_pins F).

Definition SameLocalNames (F : frame) (slots : list (name * nat)) : Prop :=
  forall n, Named F n <-> slook n slots <> None.

Local Lemma renumbered_named : forall F slots n,
  SameLocalNames F slots -> Named F n -> Named (renumber_body_frame F slots) n.
Proof. intros F slots n Names NamedBefore. exact (proj1 (Names n) NamedBefore). Qed.

Lemma field_pattern_renumber : forall W F fields source plan slots,
  TypesFieldPatterns W F fields source plan -> SameLocalNames F slots ->
  TypesFieldPatterns W (renumber_body_frame F slots) fields source plan.
Proof.
  intros W F fields source plan slots Typed Names. induction Typed;
    eauto using TypesFieldsEmpty, TypesFieldsPun, TypesFieldsIgnore, TypesFieldsRename, renumbered_named.
Qed.

Lemma pattern_renumber : forall W F st source plan slots,
  TypesPattern W F st source plan -> SameLocalNames F slots ->
  TypesPattern W (renumber_body_frame F slots) st source plan.
Proof.
  intros W F st source plan slots Typed Names. destruct Typed;
    eauto using TypesPatternWildcard, TypesPatternBinder, TypesPatternNominal, TypesPatternPayload,
      TypesPatternRecord, field_pattern_renumber, renumbered_named.
Qed.

Local Lemma method_selection_renumber : forall W F C receiver path plan signature slots,
  TypesMethodSelection W F C receiver path plan signature ->
  TypesMethodSelection W (renumber_body_frame F slots) C receiver path plan signature.
Proof.
  intros W F C receiver path plan signature slots Typed. destruct Typed; cbn zeta in *;
    eauto using TypesMethodQualified, TypesMethodProvisionWins, TypesMethodAttachedDefined,
      TypesMethodAttachedAbstract, TypesMethodDetached.
Qed.

Local Lemma application_renumber : forall W F C path plan slots,
  TypesApplication W F C path plan -> TypesApplication W (renumber_body_frame F slots) C path plan.
Proof. exact (fun _ _ _ _ _ _ Typed => Typed). Qed.

Local Lemma function_provider_renumber : forall W F C path signature plan slots,
  TypesFunctionProvider W F C path signature plan ->
  TypesFunctionProvider W (renumber_body_frame F slots) C path signature plan.
Proof. intros W F C path signature [] slots Typed; exact Typed. Qed.

Local Lemma method_bind_header_renumber : forall W F C path label brackets method receiver signature arguments slots,
  TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
  TypesMethodBindHeader W (renumber_body_frame F slots) C path label brackets method receiver signature arguments.
Proof. exact (fun _ _ _ _ _ _ _ _ _ _ _ Typed => Typed). Qed.

Local Lemma attached_plan_renumber : forall W F C path name receiver signature site plan slots,
  TypesAttachedMethodPlan W F C path name receiver signature site plan ->
  TypesAttachedMethodPlan W (renumber_body_frame F slots) C path name receiver signature site plan.
Proof. exact (fun _ _ _ _ _ _ _ _ _ _ Typed => Typed). Qed.

Local Lemma wrapped_plan_renumber : forall W F C path signature plan slots,
  TypesWrappedMethodPlan W F C path signature plan ->
  TypesWrappedMethodPlan W (renumber_body_frame F slots) C path signature plan.
Proof. exact (fun _ _ _ _ _ _ _ Typed => Typed). Qed.

Local Lemma lambda_header_renumber : forall W F C ps ann body signature this layout
    (provider : lambda_body_descriptor block_plan) result slots,
  TypesLambdaHeader W F C ps ann body signature this layout provider result ->
  SameLocalNames F slots ->
  TypesLambdaHeader W (renumber_body_frame F slots) C ps ann body signature this layout provider result.
Proof.
  intros W F C ps ann body signature this layout provider result slots Typed Names.
  destruct Typed as [caps capvals Cl Length Distinct Annotations Return Capture Context
    Module Ret This Loop Keep NamedBody NamedOuter Fits Needs Unique].
  eapply TLambdaHeader; try eassumption.
  rewrite Forall_forall in NamedOuter |- *. intros n Present.
  apply renumbered_named; [exact Names|now apply NamedOuter].
Qed.

Local Lemma binding_plan_keeps_names : forall W F C item plan result F' made slots,
  TypesBindItem W F C item plan result F' made -> SameLocalNames F slots -> SameLocalNames F' slots.
Proof.
  intros W F C item plan result F' made slots Typed Names.
  unfold SameLocalNames, Named in *. now rewrite (binding_plan_numbering _ _ _ _ _ _ _ _ Typed).
Qed.

Local Lemma bind_plans_keep_names : forall W F C items plans result F' made slots,
  TypesBindItems W F C items plans result F' made -> SameLocalNames F slots -> SameLocalNames F' slots.
Proof.
  intros W F C items plans result F' made slots Typed Names.
  unfold SameLocalNames, Named in *. now rewrite (bind_plans_numbering _ _ _ _ _ _ _ _ Typed).
Qed.

Theorem body_typing_renumber : forall W,
  (forall F C e expected plan t, TypesExpr W F C e expected plan t ->
    forall slots, SameLocalNames F slots -> TypesExpr W (renumber_body_frame F slots) C e expected plan t) /\
  (forall F C block expected plan t, TypesBlkE W F C block expected plan t ->
    forall slots, SameLocalNames F slots -> TypesBlkE W (renumber_body_frame F slots) C block expected plan t) /\
  (forall F C statements tail expected plan t made, TypesBlk W F C statements tail expected plan t made ->
    forall slots, SameLocalNames F slots ->
    TypesBlk W (renumber_body_frame F slots) C statements tail expected plan t made) /\
  (forall F C args types plans, TypesArgs W F C args types plans ->
    forall slots, SameLocalNames F slots -> TypesArgs W (renumber_body_frame F slots) C args types plans) /\
  (forall F C condition plan, TypesCond W F C condition plan ->
    forall slots, SameLocalNames F slots -> TypesCond W (renumber_body_frame F slots) C condition plan) /\
  (forall F C branch expected plan t, TypesBranch W F C branch expected plan t ->
    forall slots, SameLocalNames F slots -> TypesBranch W (renumber_body_frame F slots) C branch expected plan t) /\
  (forall F C given fields plans, TypesRecordFields W F C given fields plans ->
    forall slots, SameLocalNames F slots -> TypesRecordFields W (renumber_body_frame F slots) C given fields plans) /\
  (forall F C st arms exp plans t, TypesArms W F C st arms exp plans t ->
    forall slots, SameLocalNames F slots -> TypesArms W (renumber_body_frame F slots) C st arms exp plans t) /\
  (forall F C st arms exp plans covered catch t, TypesArmList W F C st arms exp plans covered catch t ->
    forall slots, SameLocalNames F slots ->
      TypesArmList W (renumber_body_frame F slots) C st arms exp plans covered catch t) /\
  (forall F C items plans result F' made, TypesBindItems W F C items plans result F' made ->
    forall slots, SameLocalNames F slots ->
      TypesBindItems W (renumber_body_frame F slots) C items plans result (renumber_body_frame F' slots) made) /\
  (forall F C item plan result F' made, TypesBindItem W F C item plan result F' made ->
    forall slots, SameLocalNames F slots ->
      TypesBindItem W (renumber_body_frame F slots) C item plan result (renumber_body_frame F' slots) made).
Proof.
  intros W. apply body_typing_ind; intros;
    eauto 9 using TypesUnit, TypesThis, TypesLocal, TypesBlock, TypesIfThen, TypesIfElse, TypesReturnUnit,
    TypesReturn, TypesBreak, TypesDefinedCall, TypesAbstractCall, TypesBareTag, TypesTag, TypesRecord,
    TypesField, TypesMatch, TypesMethod, TypesUnary, TypesBinary, TypesNumeric, TypesGlobalValue,
    TypesConstant, TypesBracketedCall, method_selection_renumber, TypesBlkCheck, TypesTail, TypesEmpty, TypesLet, TypesAssign, TypesExprLast,
    TypesExprThen, TypesWhile, TypesLoop, TypesLoopLast, TypesArgsNil, TypesArgsCons, TypesBool,
    TypesBranchBlock, TypesBranchExpr, TypesRecordFieldsEmpty, TypesRecordFieldsCons, TypesArmsEmpty,
    TypesArmsSome, TypesArmOne, TypesArmMore, pattern_renumber, renumbered_named,
    TypesBindEmpty, TypesBindType, TypesBindValue, TypesBindAbstractApply, TypesBindFunctorCopy,
    application_renumber, TypesBindFunction, TypesBindAttached, TypesBindWrapped,
    function_provider_renumber, method_bind_header_renumber, attached_plan_renumber, wrapped_plan_renumber,
    TypesBindFunctionLambda, TypesBindMethodLambda, lambda_header_renumber, TypesBindClosure.
  - eapply TypesBindBlock with (F' := renumber_body_frame F' slots).
    + exact (H slots H1).
    + apply H0. eapply bind_plans_keep_names; eassumption.
  - eapply (@TypesBindCons W (renumber_body_frame F slots) C item0 rest plan plans middle
      (renumber_body_frame F1 slots) made1 result (renumber_body_frame F2 slots) made2).
    + exact (H slots H1).
    + apply H0. eapply binding_plan_keeps_names; eassumption.
  - exact (@TypesBindInlineApply W (renumber_body_frame F slots) C path0 symbol d name0 parameters
      inputs outputs bodies plans result (renumber_body_frame nested slots) made e e0 e1 n (H slots H0)).
Qed.

(* A source-only fragment for checking the reverse bridge. Names listed
   in scope are local at that occurrence, including after let/var. *)
Inductive LocalExpr : list name -> aexpr -> Prop :=
| LocalUnit : forall scope, LocalExpr scope AEUnit
| LocalPath : forall scope n, In n scope -> LocalExpr scope (AEPath [n] None)
| LocalReturnUnit : forall scope, LocalExpr scope (AEReturn None)
| LocalReturn : forall scope result, LocalExpr scope result -> LocalExpr scope (AEReturn (Some result))
| LocalBlockExpr : forall scope statements tail,
    LocalBlock scope statements tail -> LocalExpr scope (AEBlock (ABlock statements tail))
with LocalBlock : list name -> list astmt -> option aexpr -> Prop :=
| LocalEmpty : forall scope, LocalBlock scope [] None
| LocalTail : forall scope result, LocalExpr scope result -> LocalBlock scope [] (Some result)
| LocalLet : forall scope statement n ann initializer mode rest tail,
    LetStmt statement n ann initializer mode -> LocalExpr scope initializer ->
    LocalBlock (n :: scope) rest tail -> LocalBlock scope (statement :: rest) tail.

Scheme local_expr_ind := Induction for LocalExpr Sort Prop
with local_block_ind := Induction for LocalBlock Sort Prop.
Combined Scheme local_body_ind from local_expr_ind, local_block_ind.

Definition LocalsInScope (scope : list name) (C : cx) : Prop :=
  forall n, In n scope -> exists t mode, slook n (c_locals C) = Some (t, mode).

Local Lemma local_scope_extends : forall scope C n t mode,
  LocalsInScope scope C -> LocalsInScope (n :: scope) (add_local C n t mode).
Proof.
  intros scope C n t mode Known other Member. cbn [add_local c_locals slook].
  destruct (String.eqb_spec other n) as [->|Different]; [eauto|].
  destruct Member as [Same|Member]; [congruence|now apply Known].
Qed.

Theorem local_source_has_plan : forall W P,
  (forall scope e, LocalExpr scope e -> forall F C expected code t,
    LocalsInScope scope C -> Ex W P F C e expected code t ->
    exists plan, TypesExpr W F C e expected plan t /\ translate_expr F C plan = code) /\
  (forall scope statements tail, LocalBlock scope statements tail -> forall F C expected code t made,
    LocalsInScope scope C -> Blk W P F C statements tail expected code t made ->
    exists plan, TypesBlk W F C statements tail expected plan t made /\ translate_block F C plan = code).
Proof.
  intros W P. apply local_body_ind.
  - intros scope F C expected code t Known Typed. inversion Typed; subst.
    exists PlanUnit. split; [constructor|reflexivity].
  - intros scope n Member F C expected code t Known Typed.
    destruct (Known n Member) as (local_type & mode & Read).
    inversion Typed; subst; try solve [
      eexists (PlanLocal _); split; [eapply TypesLocal; eassumption|reflexivity]].
    all: exfalso; match goal with
      | NotLocal : forall n', [?local] = [n'] -> slook n' (c_locals ?context) = None |- _ =>
        specialize (NotLocal local eq_refl); congruence
      end.
  - intros scope F C expected code t Known Typed. inversion Typed; subst.
    exists PlanReturnUnit. split; [now constructor|reflexivity].
  - intros scope result Local IH F C expected code t Known Typed.
    inversion Typed; subst.
    match goal with
    | Child : Ex W P F C result (Some ?want) ?value ?actual |- _ =>
      destruct (IH F C (Some want) value actual Known Child) as (plan & TypedPlan & <-)
    end.
    exists (PlanReturn plan). split; [eapply TypesReturn; eassumption|reflexivity].
  - intros scope statements tail Local IH F C expected code t Known Typed.
    inversion Typed; subst.
    match goal with
    | Body : BlkE W P F C (ABlock statements tail) expected _ _ |- _ => inversion Body; subst
    end.
    match goal with
    | Raw : Blk W P (without_pins F) C statements tail expected ?code ?ty ?made |- _ =>
      destruct (IH (without_pins F) C expected code ty made Known Raw) as (plan & TypedPlan & <-)
    end.
    exists (PlanBlock plan). split; [eapply TypesBlock, TypesBlkCheck; eassumption|reflexivity].
  - intros scope F C expected code t made Known Typed. inversion Typed; subst.
    exists PlanEmpty. split; [constructor|reflexivity].
  - intros scope result Local IH F C expected code t made Known Typed. inversion Typed; subst.
    match goal with
    | Child : Ex W P F C result expected ?code ?ty |- _ =>
      destruct (IH F C expected code ty Known Child) as (plan & TypedPlan & <-)
    end.
    exists (PlanTail plan). split; [now constructor|reflexivity].
  - intros scope statement n ann initializer mode rest tail Form Init IHinit Rest IHrest
      F C expected code t made Known Typed.
    inversion Form; subst; clear Form; inversion Typed; subst.
    all: match goal with
    | Form : LetStmt _ _ _ _ _ |- _ => inversion Form; subst; clear Form
    end.
    all: match goal with
    | InitTyped : Ex ?world ?program ?frame ?context ?source ?want ?value ?actual,
      RestTyped : Blk ?world ?program ?frame (add_local ?context ?local ?local_type ?local_mode)
        ?remaining ?last ?expect ?continued ?result_type ?new_names |- _ =>
      destruct (IHinit frame context want value actual Known InitTyped) as (init_plan & InitPlan & <-);
      destruct (IHrest frame (add_local context local local_type local_mode) expect continued result_type new_names
        (local_scope_extends scope context local local_type local_mode Known) RestTyped) as (rest_plan & RestPlan & <-);
      exists (PlanLet local local_type local_mode init_plan rest_plan); split;
      [eapply TypesLet; [constructor|eassumption|eassumption|eassumption|eassumption|eassumption]|reflexivity]
    end.
Qed.

Definition local_example_frame : frame :=
  Frame "/main.moss" (Some TUnit) None false [("saved", 1)] [] [] [].

Definition local_example_source : aexpr :=
  AEBlock (ABlock [ASLet "saved" None AEUnit]
    (Some (AEReturn (Some (AEPath ["saved"] None))))).

Definition local_example_plan : expression_plan :=
  PlanBlock (PlanLet "saved" TUnit LLet PlanUnit
    (PlanTail (PlanReturn (PlanLocal "saved")))).

Example local_example_types : forall W,
  TypesExpr W local_example_frame (Cx [] [] [] [] []) local_example_source None local_example_plan never.
Proof.
  intro W. unfold local_example_source, local_example_plan.
  apply (TypesBlock W _ _ None _ _ never). eapply TypesBlkCheck with (made := []).
  - eapply TypesLet with (declared := None) (vt := TUnit).
    + constructor.
    + reflexivity.
    + constructor.
    + reflexivity.
    + discriminate.
    + apply TypesTail. eapply TypesReturn with (expected := TUnit) (actual := TUnit).
      * reflexivity.
      * eapply TypesLocal; [reflexivity|discriminate].
      * now left.
  - intros g [].
  - exact I.
Qed.

Example local_example_translation :
  translate_expr local_example_frame (Cx [] [] [] [] []) local_example_plan =
  ELet 1 EUnit (EReturn (EVar 1)).
Proof. reflexivity. Qed.
