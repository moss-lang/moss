(* Read the source plans from the joint elaboration derivation. Recursion
   follows derivations, including the elaboration of a bracket expansion. *)
From Stdlib Require Import List String.
From NM Require Import Audit BodyTyping BodyMethodTyping BodyLiteralTyping.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section Readback.
Variables (W : world) (P : program).

Hypothesis read_provider_binding : forall F C item wrap C' F' made,
  BindItem W P F C item wrap C' F' made ->
  exists plan, TypesBindItem W F C item plan C' F' made /\ translate_bind_item F plan = wrap.

Fixpoint read_expression F C source expected code t (D : Ex W P F C source expected code t) {struct D} :
  exists plan, TypesExpr W F C source expected plan t /\ translate_expr F C plan = code
with read_arguments F C args types code (D : Args W P F C args types code) {struct D} :
  exists plans, TypesArgs W F C args types plans /\ map (translate_expr F C) plans = code
with read_condition F C condition code (D : Cond W P F C condition code) {struct D} :
  exists plan, TypesCond W F C condition plan /\ translate_expr F C plan = code
with read_branch F C branch expected code t (D : Branch W P F C branch expected code t) {struct D} :
  exists plan, TypesBranch W F C branch expected plan t /\ translate_expr F C plan = code
with read_arms F C st arms expected code t (D : Arms W P F C st arms expected code t) {struct D} :
  exists plans, TypesArms W F C st arms expected plans t /\ translate_arms F C plans = code
with read_arm_list F C st arms expected code heads catch t
    (D : ArmsList W P F C st arms expected code heads catch t) {struct D} :
  exists plans, TypesArmList W F C st arms expected plans heads catch t /\ translate_arms F C plans = code
with read_method F C object receiver path args code t (D : MethCall W P F C object receiver path args code t) {struct D} :
  exists selected signature plans, TypesMethodSelection W F C receiver path selected signature /\
    TypesArgs W F C args (sg_params signature) plans /\ t = sg_ret signature /\
    emit_method_call C receiver selected object (map (translate_expr F C) plans) = code
with read_provided F C object receiver method args code t (D : Provided W P F C object receiver method args code t) {struct D} :
  exists family signature plans, TypesMethodProvision W C receiver method family signature /\
    TypesArgs W F C args (sg_params signature) plans /\ t = sg_ret signature /\
    emit_provided_method C receiver method family object (map (translate_expr F C) plans) = code
with read_block_expression F C body expected code t (D : BlkE W P F C body expected code t) {struct D} :
  exists plan, TypesBlkE W F C body expected plan t /\ translate_block (without_pins F) C plan = code
with read_block F C statements tail expected code t made (D : Blk W P F C statements tail expected code t made) {struct D} :
  exists plan, TypesBlk W F C statements tail expected plan t made /\ translate_block F C plan = code
with read_bind_items F C items wrap result F' made (D : BindItems W P F C items wrap result F' made) {struct D} :
  exists plans, TypesBindItems W F C items plans result F' made /\ translate_bind_items F plans = wrap
with read_bind_item F C item wrap result F' made (D : BindItem W P F C item wrap result F' made) {struct D} :
  exists plan, TypesBindItem W F C item plan result F' made /\ translate_bind_item F plan = wrap.
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
    + exists PlanUnit. split; [constructor|reflexivity].
    + exists PlanThis. split; [now constructor|reflexivity].
    + exists (PlanLocal n). split; [eapply TypesLocal; eassumption|reflexivity].
    + exists (PlanUnitTag s). split; [eapply TypesUnitTag; eassumption|reflexivity].
    + exists (PlanGlobalValue s). split; [eapply TypesGlobalValue; eassumption|reflexivity].
    + destruct Initializer as (layout & actual & Init & Fits & Module & Ret & This & Loop & Fresh & Applying & Pins & Layout).
      destruct (read_expression _ _ _ _ _ _ Init) as (plan & Typed & <-).
      exists (PlanConstant layout plan). split; [eapply TypesConstant; eassumption|reflexivity].
    + subst args. destruct (read_expression _ _ _ _ _ _ Child) as (plan & Typed & <-).
      exists (PlanTag s arguments plan). split; [eapply TypesTag; eassumption|reflexivity].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <-).
      exists (PlanDefinedCall f formal needs arguments demands result plans).
      split; [eapply TypesDefinedCall; eassumption|reflexivity].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <-).
      exists (PlanAbstractCall f plans). split; [eapply TypesAbstractCall; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Expansion) as (plan & Typed & <-).
      exists (PlanBracketedCall plan). split; [eapply TypesBracketedCall; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <-).
      destruct (read_method _ _ _ _ _ _ _ _ Method) as (selected & signature & plans & Choice & Args & -> & <-).
      exists (PlanMethod (canon C ot) selected object_plan plans).
      split; [eapply TypesMethod; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <-).
      destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (PlanOperator (canon C ot) method family object_plan plans).
      split; [eapply TypesUnary; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <-).
      destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (PlanOperator (canon C ot) method family object_plan plans).
      split; [eapply TypesBinary; eassumption|reflexivity].
    + destruct (literal_readback _ _ _ _ _ Literal) as (plan & Typed & ->).
      exists (PlanNumeric plan). split; [eapply TypesNumeric; eassumption|reflexivity].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <-).
      destruct (read_block_expression _ _ _ _ _ _ Then) as (then_plan & ThenTyped & <-).
      exists (PlanIf condition_plan then_plan None). split; [eapply TypesIfThen; eassumption|reflexivity].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <-).
      destruct (read_block_expression _ _ _ _ _ _ Then) as (then_plan & ThenTyped & <-).
      destruct (read_branch _ _ _ _ _ _ Else) as (else_plan & ElseTyped & <-).
      exists (PlanIf condition_plan then_plan (Some else_plan)).
      split; [eapply TypesIfElse; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Object) as (object_plan & ObjectTyped & <-).
      destruct (read_arms _ _ _ _ _ _ _ Arms) as (arms_plan & ArmsTyped & <-).
      exists (PlanMatch object_plan arms_plan). split; [eapply TypesMatch; eassumption|reflexivity].
    + destruct (read_block_expression _ _ _ _ _ _ Block) as (plan & Typed & <-).
      exists (PlanBlock plan). split; [eapply TypesBlock; eassumption|reflexivity].
    + exists PlanReturnUnit. split; [now constructor|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <-).
      exists (PlanReturn plan). split; [eapply TypesReturn; eassumption|reflexivity].
    + exists PlanBreak. split; [now constructor|reflexivity].
    + subst payload.
      assert (ReadFields : forall types values,
        Forall2 (fun field value => fst value = fst field /\
          exists n source actual, n = fst field /\ In (n, source) given /\
            Ex W P F C (match source with Some e => e | None => AEPath [n] None end)
              (Some (snd field)) (snd value) actual /\ fits actual (snd field)) types values ->
        exists plans, TypesRecordFields W F C given types plans /\ translate_record_fields F C plans = values).
      { fix read_fields 3. intros types values Evidence.
        destruct Evidence as [|[n expected] [other value] types values
          [Same (name & source & actual & Field & Present & Typed & Fits)] Rest].
        - exists []. split; [constructor|reflexivity].
        - cbn [fst snd] in *. subst other name.
          destruct (read_expression _ _ _ _ _ _ Typed) as (plan & Value & <-).
          destruct (read_fields _ _ Rest) as (plans & RestTyped & <-).
          exists ((n, plan) :: plans). split; [econstructor; eassumption|reflexivity]. }
      destruct (ReadFields _ _ Fields) as (plans & FieldsTyped & <-).
      exists (PlanRecord tag arguments plans). split; [eapply TypesRecord; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Object) as (plan & Typed & <-).
      exists (PlanField plan field). split; [eapply TypesField; eassumption|reflexivity].
  - destruct D as [F C|F C first rest ty tys code codes actual First Fits Rest].
    + exists []. split; [constructor|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ First) as (plan & Typed & <-).
      destruct (read_arguments _ _ _ _ _ Rest) as (plans & RestTyped & <-).
      exists (plan :: plans). split; [econstructor; eassumption|reflexivity].
  - destruct D as [F C condition code t bool Bool Expr Fits].
    destruct (read_expression _ _ _ _ _ _ Expr) as (plan & Typed & <-).
    exists plan. split; [econstructor; eassumption|reflexivity].
  - destruct D as [F C body exp code t Block|F C source exp code t NotBlock Expr].
    + destruct (read_block_expression _ _ _ _ _ _ Block) as (plan & Typed & <-).
      exists (PlanBlock plan). split; [econstructor; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Expr) as (plan & Typed & <-).
      exists plan. split; [eapply TypesBranchExpr; eassumption|reflexivity].
  - destruct D as [F C exp|F C st arms exp code heads catch t NotNever Arms Exhaustive].
    + exists []. split; [constructor|reflexivity].
    + destruct (read_arm_list _ _ _ _ _ _ _ _ _ Arms) as (plans & Typed & <-).
      exists plans. split; [eapply TypesArmsSome; eassumption|reflexivity].
  - destruct D as
      [F C st pat body exp pattern_code bindings heads catch body_code t Pat Body Fits
      |F C st pat body rest exp pattern_code bindings heads catch body_code bt rest_code
        rest_heads rest_catch rt t Nonempty Pat Body Fits Rest Join].
    + destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & Pattern & <- & <- & <- & <-).
      destruct (read_branch _ _ _ _ _ _ Body) as (plan & Typed & <-).
      exists [(pattern, plan)]. split; [eapply TypesArmOne; eassumption|reflexivity].
    + destruct (pattern_has_plan _ _ _ _ _ _ _ _ Pat) as (pattern & Pattern & <- & <- & <- & <-).
      destruct (read_branch _ _ _ _ _ _ Body) as (plan & Typed & <-).
      destruct (read_arm_list _ _ _ _ _ _ _ _ _ Rest) as (plans & RestTyped & <-).
      exists ((pattern, plan) :: plans). split; [eapply TypesArmMore; eassumption|reflexivity].
  - destruct D as
      [F C object receiver path args method code t Qualified Resolve Provided
      |F C object receiver name args head attached method code t Head Attached Scope Present Provided
      |F C object receiver name args head attached formal needs types signature values receiver_types demands result
         Head Attached Absent SourceBody Receiver Context Demand Needs Theta Signature Args InForce
      |F C object receiver name args head attached code t Head Attached Absent Abstract Provided
      |F C object receiver name args method code t NotAttached Scope Provided].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodQualified; eassumption|auto].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodProvisionWins; eassumption|auto].
    + subst receiver_types. destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <-).
      exists (SelectedDefinition attached formal needs types demands result), signature, plans.
      split; [eapply TypesMethodAttachedDefined; eassumption|auto].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (SelectedProvision attached family), signature, plans.
      split; [eapply TypesMethodAttachedAbstract; eassumption|auto].
    + destruct (read_provided _ _ _ _ _ _ _ _ Provided) as (family & signature & plans & Choice & Args & -> & <-).
      exists (SelectedProvision method family), signature, plans.
      split; [eapply TypesMethodDetached; eassumption|auto].
  - destruct D as
      [F C object receiver method args signature values Present Args
      |F C object receiver method args u matching types formal needs result keys signature values
         Absent Family ReadOff Produces Unique Present Args InForce].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <-).
      exists None, signature, plans. split; [now apply TypesMethodIn|auto].
    + destruct (read_arguments _ _ _ _ _ Args) as (plans & Typed & <-).
      exists (Some (FamilyUse u matching types formal needs result keys)), signature, plans.
      split; [eapply TypesMethodFamily; eassumption|auto].
  - destruct D as [F C statements tail exp code t made Block Stays Fits].
    destruct (read_block _ _ _ _ _ _ _ _ Block) as (plan & Typed & <-).
    exists plan. split; [eapply TypesBlkCheck; eassumption|reflexivity].
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
    + destruct (read_expression _ _ _ _ _ _ Tail) as (plan & Typed & <-).
      exists (PlanTail plan). split; [now constructor|reflexivity].
    + exists PlanEmpty. split; [constructor|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (initializer_plan & Initializer & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <-).
      exists (PlanLet n local_type mode initializer_plan rest_plan).
      split; [eapply TypesLet; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (value_plan & Typed & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <-).
      exists (PlanAssign n value_plan rest_plan). split; [eapply TypesAssign; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <-).
      exists (PlanExprLast plan). split; [eapply TypesExprLast; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (value_plan & Typed & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <-).
      exists (PlanExprThen value_plan rest_plan). split; [eapply TypesExprThen; eassumption|reflexivity].
    + destruct (read_condition _ _ _ _ Condition) as (condition_plan & CondTyped & <-).
      destruct (read_block_expression _ _ _ _ _ _ Body) as (body_plan & BodyTyped & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <-).
      exists (PlanWhile condition_plan body_plan rest_plan). split; [eapply TypesWhile; eassumption|reflexivity].
    + destruct (read_block_expression _ _ _ _ _ _ Body) as (body_plan & BodyTyped & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (rest_plan & RestTyped & <-).
      exists (PlanLoop body_plan rest_plan). split; [eapply TypesLoop; eassumption|reflexivity].
    + destruct (read_block_expression _ _ _ _ _ _ Body) as (plan & Typed & <-).
      exists (PlanLoopLast plan). split; [eapply TypesLoopLast; eassumption|reflexivity].
    + destruct (read_bind_items _ _ _ _ _ _ _ Items) as (plans & ItemsTyped & <-).
      destruct (read_block _ _ _ _ _ _ _ _ Rest) as (continuation & Typed & <-).
      exists (PlanBind plans C' continuation). split; [eapply TypesBindBlock; eassumption|].
      unfold translate_block. cbn [emit_block].
      now rewrite (bind_plans_numbering _ _ _ _ _ _ _ _ ItemsTyped).
  - destruct D as
      [F C|F C item rest first middle F1 made1 more result F2 made2 Item Rest].
    + exists []. split; [constructor|reflexivity].
    + destruct (read_bind_item _ _ _ _ _ _ _ Item) as (plan & Typed & <-).
      destruct (read_bind_items _ _ _ _ _ _ _ Rest) as (plans & RestTyped & <-).
      exists (plan :: plans). split; [eapply TypesBindCons; eassumption|].
      unfold translate_bind_items, translate_bind_item. cbn [flat_map].
      rewrite splice_bindings_append.
      now rewrite (binding_plan_numbering _ _ _ _ _ _ _ _ Typed).
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
      split; [eapply TypesBindType; eassumption|reflexivity].
    + destruct (read_expression _ _ _ _ _ _ Value) as (plan & Typed & <-).
      exists (PlanValueBind C symbol plan). split; [eapply TypesBindValue; eassumption|reflexivity].
    + eapply read_provider_binding; exact Original.
    + eapply read_provider_binding; exact Original.
    + eapply read_provider_binding; exact Original.
    + destruct (read_bind_items _ _ _ _ _ _ _ Body) as (plans & Typed & <-).
      exists (PlanInlineApply plans). split; [eapply TypesBindInlineApply; eassumption|reflexivity].
    + exists (PlanAbstractApply C plan). split; [now apply TypesBindAbstractApply|reflexivity].
    + subst map. exists (PlanFunctorCopy u h Cu Ch target nsh positions).
      split; [eapply TypesBindFunctorCopy; eassumption|reflexivity].
    + eapply read_provider_binding; exact Original.
Qed.

End Readback.
