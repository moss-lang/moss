(* Read the source plans from the joint elaboration derivation. Recursion
   follows derivations, including the elaboration of a bracket expansion. *)
From Stdlib Require Import List String.
From NM Require Import Audit BodyTyping BodyMethodTyping BodyLiteralTyping.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section Readback.
Variables (W : world) (P : program).

Local Definition placements {A} (output : emission A) : Prop :=
  placements_in W P (emitted_placements output).

Local Ltac finish_placements :=
  cbn [placements expression_emission block_emission argument_emission record_fields_emission
    arms_emission bind_item_emission bind_items_emission emit_expr emit_block emit_bind_item
    emit_only emit_map emit_pair emit_each emit_flat require_placement emitted_placements
    flat_map app] in *;
  unfold placements_in in *;
  repeat first [assumption | apply Forall_app; split | apply Forall_cons_iff; split; cbn [placement_in]
    | apply Forall_nil | split | reflexivity].

Hypothesis read_provider_binding : forall F C item wrap C' F' made,
  BindItem W P F C item wrap C' F' made ->
  exists plan, TypesBindItem W F C item plan C' F' made /\ translate_bind_item F plan = wrap /\
    placements (bind_item_emission identity_slots F plan).

Fixpoint read_expression F C source expected code t (D : Ex W P F C source expected code t) {struct D} :
  exists plan, TypesExpr W F C source expected plan t /\ translate_expr F C plan = code /\
    placements (expression_emission identity_slots F C plan)
with read_arguments F C args types code (D : Args W P F C args types code) {struct D} :
  exists plans, TypesArgs W F C args types plans /\ map (translate_expr F C) plans = code /\
    placements (argument_emission identity_slots F C plans)
with read_condition F C condition code (D : Cond W P F C condition code) {struct D} :
  exists plan, TypesCond W F C condition plan /\ translate_expr F C plan = code /\
    placements (expression_emission identity_slots F C plan)
with read_branch F C branch expected code t (D : Branch W P F C branch expected code t) {struct D} :
  exists plan, TypesBranch W F C branch expected plan t /\ translate_expr F C plan = code /\
    placements (expression_emission identity_slots F C plan)
with read_arms F C st arms expected code t (D : Arms W P F C st arms expected code t) {struct D} :
  exists plans, TypesArms W F C st arms expected plans t /\ translate_arms F C plans = code /\
    placements (arms_emission identity_slots F C plans)
with read_arm_list F C st arms expected code heads catch t
    (D : ArmsList W P F C st arms expected code heads catch t) {struct D} :
  exists plans, TypesArmList W F C st arms expected plans heads catch t /\ translate_arms F C plans = code /\
    placements (arms_emission identity_slots F C plans)
with read_method F C object receiver path args code t (D : MethCall W P F C object receiver path args code t) {struct D} :
  exists selected signature plans, TypesMethodSelection W F C receiver path selected signature /\
    TypesArgs W F C args (sg_params signature) plans /\ t = sg_ret signature /\
    emit_method_call C receiver selected object (map (translate_expr F C) plans) = code /\
    placements (argument_emission identity_slots F C plans)
with read_provided F C object receiver method args code t (D : Provided W P F C object receiver method args code t) {struct D} :
  exists family signature plans, TypesMethodProvision W C receiver method family signature /\
    TypesArgs W F C args (sg_params signature) plans /\ t = sg_ret signature /\
    emit_provided_method C receiver method family object (map (translate_expr F C) plans) = code /\
    placements (argument_emission identity_slots F C plans)
with read_block_expression F C body expected code t (D : BlkE W P F C body expected code t) {struct D} :
  exists plan, TypesBlkE W F C body expected plan t /\ translate_block (without_pins F) C plan = code /\
    placements (block_emission identity_slots (without_pins F) C plan)
with read_block F C statements tail expected code t made (D : Blk W P F C statements tail expected code t made) {struct D} :
  exists plan, TypesBlk W F C statements tail expected plan t made /\ translate_block F C plan = code /\
    placements (block_emission identity_slots F C plan)
with read_bind_items F C items wrap result F' made (D : BindItems W P F C items wrap result F' made) {struct D} :
  exists plans, TypesBindItems W F C items plans result F' made /\ translate_bind_items F plans = wrap /\
    placements (bind_items_emission identity_slots F plans)
with read_bind_item F C item wrap result F' made (D : BindItem W P F C item wrap result F' made) {struct D} :
  exists plan, TypesBindItem W F C item plan result F' made /\ translate_bind_item F plan = wrap /\
    placements (bind_item_emission identity_slots F plan).
Proof.
  - destruct D as
      [F C exp
      |F C exp t This
      |F C exp n t mu Local Named
      |F C exp path s NotLocal Resolve Kind
      |F C exp path s d n ann t NotLocal Resolve Decl Ast Present
      |F C exp path s d n ann source t code NotLocal Resolve Decl Ast Assumes AnnotationTyped Initializer
      |F C exp path brackets args s readings arguments payload_type source value actual
         Resolve Kind Brackets Readings Payload ArgList Child Fits
      |F C exp path args f formal needs arguments signature values demands result
         Resolve Kind SourceBody Context Demand Needs Theta Signature Args InForce
      |F C exp path args f signature values Resolve Kind Abstract Provision Args
      |F C exp path brackets args f code t Resolve Kind Brackets Expansion
      |F C exp object path args ox ot code t Object Method
      |F C exp op object lib name method ox ot code t Operator Language Object Head Provided
      |F C exp op left right lib name method ox ot code t Operator Language Object Head Provided
      |F C exp text code t Numeric Literal
      |F C exp condition then_body cx bx bt Condition Then Fits BranchType
      |F C exp condition then_body other cx bx bt ox ot t Condition Then Else Fits Join
      |F C exp object arms value st results t Object Arms
      |F C exp body code t Block
      |F C exp Ret
      |F C exp source expected code actual Ret Value Fits
      |F C exp Loop
      |F C exp path brackets given tag readings arguments payload field_types field_code
         Resolve Kind Brackets Readings Payload Shape Distinct Count Fields
      |F C exp object field value actual payload t Object Payload Field].
    + exists PlanUnit. split; [constructor|split; [reflexivity|finish_placements]].
    + exists PlanThis. split; [now constructor|split; [reflexivity|finish_placements]].
    + exists (PlanLocal n). split; [eapply TypesLocal; eassumption|split; [reflexivity|finish_placements]].
    + exists (PlanUnitTag s). split; [eapply TypesUnitTag; eassumption|split; [reflexivity|finish_placements]].
    + exists (PlanGlobalValue s). split; [eapply TypesGlobalValue; eassumption|split; [reflexivity|finish_placements]].
    + destruct Initializer as (layout & actual & Init & Fits & Module & Ret & This & Loop & Fresh & Applying & Pins & Layout).
      destruct (read_expression _ _ _ _ _ _ Init) as (plan & Typed & <- & ?Placed).
      exists (PlanConstant layout plan). split; [eapply TypesConstant; eassumption|split; [reflexivity|finish_placements]].
    + subst args. destruct (read_expression _ _ _ _ _ _ Child) as (plan & Typed & <- & ?Placed).
      exists (PlanTag s arguments plan). split; [eapply TypesTag; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <- & ?Placed).
      exists (PlanDefinedCall f formal needs arguments demands result plans).
      split; [eapply TypesDefinedCall; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <- & ?Placed).
      exists (PlanAbstractCall f plans). split; [eapply TypesAbstractCall; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Expansion) as (plan & Typed & <- & ?Placed).
      exists (PlanBracketedCall plan). split; [eapply TypesBracketedCall; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <- & ?Placed).
      destruct (read_method _ _ _ _ _ _ _ _ Method) as (selected & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (PlanMethod (canon C ot) selected object_plan plans).
      split; [eapply TypesMethod; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <- & ?Placed).
      destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (PlanOperator (canon C ot) method family object_plan plans).
      split; [eapply TypesUnary; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <- & ?Placed).
      destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (PlanOperator (canon C ot) method family object_plan plans).
      split; [eapply TypesBinary; eassumption|split; [reflexivity|finish_placements]].
    + destruct (literal_readback _ _ _ _ _ Literal) as (plan & Typed & ->).
      exists (PlanNumeric plan). split; [eapply TypesNumeric; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <- & ?Placed).
      destruct (read_block_expression _ _ _ _ _ _ Then) as (then_plan & ThenTyped & <- & ?Placed).
      exists (PlanIf condition_plan then_plan None). split; [eapply TypesIfThen; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <- & ?Placed).
      destruct (read_block_expression _ _ _ _ _ _ Then) as (then_plan & ThenTyped & <- & ?Placed).
      destruct (read_branch _ _ _ _ _ _ Else) as (else_plan & ElseTyped & <- & ?Placed).
      exists (PlanIf condition_plan then_plan (Some else_plan)).
      split; [eapply TypesIfElse; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <- & ?Placed).
      destruct (read_arms _ _ _ _ _ _ _ Arms) as (arms_plan & ArmsTyped & <- & ?Placed).
      exists (PlanMatch object_plan arms_plan). split; [eapply TypesMatch; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_block_expression _ _ _ _ _ _ Block) as (plan & Typed & <- & ?Placed).
      exists (PlanBlock plan). split; [eapply TypesBlock; eassumption|split; [reflexivity|finish_placements]].
    + exists PlanReturnUnit. split; [now constructor|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <- & ?Placed).
      exists (PlanReturn plan). split; [eapply TypesReturn; eassumption|split; [reflexivity|finish_placements]].
    + exists PlanBreak. split; [now constructor|split; [reflexivity|finish_placements]].
    + subst payload.
      assert (ReadFields : forall types values,
        Forall2 (fun field value => fst value = fst field /\
          exists n source actual, n = fst field /\ In (n, source) given /\
            Ex W P F C (match source with Some e => e | None => AEPath [n] None end)
              (Some (snd field)) (snd value) actual /\ fits actual (snd field)) types values ->
        exists plans, TypesRecordFields W F C given types plans /\ translate_record_fields F C plans = values /\
          placements (record_fields_emission identity_slots F C plans)).
      { fix read_fields 3. intros types values Evidence.
        destruct Evidence as [|[n expected] [other value] types values
          [Same (name & source & actual & Field & Present & Typed & Fits)] Rest].
        - exists []. split; [constructor|split; [reflexivity|finish_placements]].
        - cbn [fst snd] in *. subst other name.
          destruct (read_expression _ _ _ _ _ _ Typed) as (plan & Value & <- & ?Placed).
          destruct (read_fields _ _ Rest) as (plans & RestTyped & <- & ?Placed).
          exists ((n, plan) :: plans). split; [econstructor; eassumption|split; [reflexivity|finish_placements]]. }
      destruct (ReadFields _ _ Fields) as (plans & FieldsTyped & <- & ?Placed).
      exists (PlanRecord tag arguments plans). split; [eapply TypesRecord; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Object) as (plan & Typed & <- & ?Placed).
      exists (PlanField plan field). split; [eapply TypesField; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as [F C|F C first rest ty tys code codes actual First Fits Rest].
    + exists []. split; [constructor|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ First) as (plan & Typed & <- & ?Placed).
      destruct (read_arguments _ _ _ _ _ Rest) as (plans & RestTyped & <- & ?Placed).
      exists (plan :: plans). split; [econstructor; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as [F C condition code t bool Bool Expr Fits].
    destruct (read_expression _ _ _ _ _ _ Expr) as (plan & Typed & <- & ?Placed).
    exists plan. split; [econstructor; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as [F C body exp code t Block|F C source exp code t NotBlock Expr].
    + destruct (read_block_expression _ _ _ _ _ _ Block) as (plan & Typed & <- & ?Placed).
      exists (PlanBlock plan). split; [econstructor; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Expr) as (plan & Typed & <- & ?Placed).
      exists plan. split; [eapply TypesBranchExpr; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as [F C exp|F C st arms exp code heads catch t NotNever Arms Exhaustive].
    + exists []. split; [constructor|split; [reflexivity|finish_placements]].
    + destruct (read_arm_list _ _ _ _ _ _ _ _ _ Arms) as (plans & Typed & <- & ?Placed).
      exists plans. split; [eapply TypesArmsSome; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as
      [F C st pat body exp pattern_code bindings heads catch body_code t Pat Body Fits
      |F C st pat body rest exp pattern_code bindings heads catch body_code bt rest_code
        rest_heads rest_catch rt t Nonempty Pat Body Fits Rest Join].
    + destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & Pattern & <- & <- & <- & <-).
      destruct (read_branch _ _ _ _ _ _ Body) as (plan & Typed & <- & ?Placed).
      exists [(pattern, plan)]. split; [eapply TypesArmOne; eassumption|split; [reflexivity|finish_placements]].
    + destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & Pattern & <- & <- & <- & <-).
      destruct (read_branch _ _ _ _ _ _ Body) as (plan & Typed & <- & ?Placed).
      destruct (read_arm_list _ _ _ _ _ _ _ _ _ Rest) as (plans & RestTyped & <- & ?Placed).
      exists ((pattern, plan) :: plans). split; [eapply TypesArmMore; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as
      [F C object receiver path args method code t Qualified Resolve Provided
      |F C object receiver name args head attached method code t Head Attached Scope Present Provided
      |F C object receiver name args head attached formal needs types signature values receiver_types demands result
         Head Attached Absent SourceBody Receiver Context Demand Needs Theta Signature Args InForce
      |F C object receiver name args head attached code t Head Attached Absent Abstract Provided
      |F C object receiver name args method code t NotAttached Scope Provided].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodQualified; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodProvisionWins; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
    + subst receiver_types. destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <- & ?Placed).
      exists (SelectedDefinition attached formal needs types demands result), signature, plans.
      split; [eapply TypesMethodAttachedDefined; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (SelectedProvision attached family), signature, plans.
      split; [eapply TypesMethodAttachedAbstract; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <- & ?Placed).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodDetached; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
  - destruct D as
      [F C object receiver method args signature values Present Args
      |F C object receiver method args u matching types formal needs result keys signature values
         Absent Family ReadOff Produces Unique Present Args InForce].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <- & ?Placed).
      exists None, signature, plans. split; [now apply TypesMethodIn|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <- & ?Placed).
      exists (Some (FamilyUse u matching types formal needs result keys)), signature, plans.
      split; [eapply TypesMethodFamily; eassumption|repeat first [assumption | split | reflexivity | solve [finish_placements]]].
  - destruct D as [F C statements tail exp code t made Block Stays Fits].
    destruct (read_block _ _ _ _ _ _ _ _ Block) as (plan & Typed & <- & ?Placed).
    exists plan. split; [eapply TypesBlkCheck; eassumption|split; [reflexivity|finish_placements]].
  - destruct D as
      [F C source exp code t Tail
      |F C exp
      |F C statement n ann initializer rest tail exp declared value actual local_type code t made mode
         Let Annotation Value Fits Named Rest
      |F C n source rest tail exp local_type value actual code t made Local Value Fits Rest Named
      |F C source exp value actual Value
      |F C source rest tail exp value actual code t made Continued Value Rest
      |F C condition body rest tail exp cx bx bt code t made Condition Body Rest
      |F C body rest tail exp bx bt code t made Continued Body Rest
      |F C body exp code t Body NoBreak
      |F C items rest tail exp wrap C' F' code t made created Items Rest].
    + destruct (read_expression _ _ _ _ _ _ Tail) as (plan & Typed & <- & ?Placed).
      exists (PlanTail plan). split; [now constructor|split; [reflexivity|finish_placements]].
    + exists PlanEmpty. split; [constructor|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (initializer_plan & Initializer & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <- & ?Placed).
      exists (PlanLet n local_type mode initializer_plan rest_plan).
      split; [eapply TypesLet; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (value_plan & Typed & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <- & ?Placed).
      exists (PlanAssign n value_plan rest_plan). split; [eapply TypesAssign; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <- & ?Placed).
      exists (PlanExprLast plan). split; [eapply TypesExprLast; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (value_plan & Typed & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <- & ?Placed).
      exists (PlanExprThen value_plan rest_plan). split; [eapply TypesExprThen; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <- & ?Placed).
      destruct (read_block_expression _ _ _ _ _ _ Body) as (body_plan & BodyTyped & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <- & ?Placed).
      exists (PlanWhile condition_plan body_plan rest_plan). split; [eapply TypesWhile; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_block_expression _ _ _ _ _ _ Body) as (body_plan & BodyTyped & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <- & ?Placed).
      exists (PlanLoop body_plan rest_plan). split; [eapply TypesLoop; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_block_expression _ _ _ _ _ _ Body) as (plan & Typed & <- & ?Placed).
      exists (PlanLoopLast plan). split; [eapply TypesLoopLast; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_bind_items _ _ _ _ _ _ _ Items) as (plans & ItemsTyped & <- & ?Placed).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (continuation & Typed & <- & ?Placed).
      exists (PlanBind plans C' continuation). split; [eapply TypesBindBlock; eassumption|]. split.
      * unfold translate_block, block_emission. cbn [emit_block emit_pair emit_flat emitted_code].
        now rewrite (bind_plans_numbering _ _ _ _ _ _ _ _ ItemsTyped).
      * unfold placements, block_emission in *.
        rewrite (bind_plans_numbering _ _ _ _ _ _ _ _ ItemsTyped) in *.
        finish_placements.
  - destruct D as
      [F C|F C item rest first middle F1 made1 more result F2 made2 Item Rest].
    + exists []. split; [constructor|split; [reflexivity|finish_placements]].
    + destruct (read_bind_item _ _ _ _ _ _ _ Item) as (plan & Typed & <- & ?Placed).
      destruct (read_bind_items _ _ _ _ _ _ _ Rest) as (plans & RestTyped & <- & ?Placed).
      exists (plan :: plans). split; [eapply TypesBindCons; eassumption|]. split.
      * unfold translate_bind_items, translate_bind_item, bind_items_emission, bind_item_emission, emit_flat.
        cbn [emitted_code flat_map].
        rewrite splice_bindings_append.
        now rewrite (binding_plan_numbering _ _ _ _ _ _ _ _ Typed).
      * unfold placements, bind_items_emission, bind_item_emission in *.
        rewrite (binding_plan_numbering _ _ _ _ _ _ _ _ Typed) in *.
        finish_placements.
  - pose proof D as Original. destruct D as
      [F C path symbol right arguments meaning Resolve Kind Unpinned Read
      |F C path symbol d name annotation initializer declared source code actual arguments
         Resolve Decl Ast Read Value Fits Application
      | | |
      |F C path symbol d name params inputs outputs bodies wrap result nested made
         Resolve Decl Ast Nonrecursive Body
      |F C path plan Typed
      |F C path u provider h Cu nsu Ch nsh target map fixed positions
         Resolve ResolveProvider Abstract Available Target Provider FixedHere FixedInTarget Agreement
         Same Map FixedRead Inputs Copy
      | ].
    + exists (PlanTypeBind symbol (canon C meaning)).
      split; [eapply TypesBindType; eassumption|split; [reflexivity|finish_placements]].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <- & ?Placed).
      exists (PlanValueBind C symbol plan). split; [eapply TypesBindValue; eassumption|split; [reflexivity|finish_placements]].
    + eapply read_provider_binding; exact Original.
    + eapply read_provider_binding; exact Original.
    + eapply read_provider_binding; exact Original.
    + destruct (read_bind_items _ _ _ _ _ _ _ Body) as (plans & Typed & <- & ?Placed).
      exists (PlanInlineApply plans). split; [eapply TypesBindInlineApply; eassumption|split; [reflexivity|finish_placements]].
    + exists (PlanAbstractApply C plan). split; [now apply TypesBindAbstractApply|split; [reflexivity|finish_placements]].
    + subst map. exists (PlanFunctorCopy u h Cu Ch target nsh positions).
      split; [eapply TypesBindFunctorCopy; eassumption|split; [reflexivity|finish_placements]].
    + eapply read_provider_binding; exact Original.
Qed.

End Readback.
