(* Separate expression and control-flow typing from translation.
   Typing chooses a plan over source names. Translation uses the frame's
   numbering and encodes the checked context and demand choices. *)
From Stdlib Require Import List String Bool.
From NM Require Import Audit BodyLiteralTyping BodyMethodTyping.
From NM Require BodyFunctionProviderTyping BodyProviderTyping.
From NM Require BodyLinkTyping.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Inductive field_pattern_plan : Type :=
| PatternPun (field : fld) (field_type : ty)
| PatternIgnore (field : fld)
| PatternRename (field : fld) (binder : name) (field_type : ty).

Definition field_pattern_bindings (fields : list field_pattern_plan) : list (name * ty) :=
  flat_map (fun field => match field with
    | PatternPun n t | PatternRename _ n t => [(n, t)]
    | PatternIgnore _ => []
    end) fields.

Definition local_number (slots : list (name * nat)) (n : name) : nat :=
  match slook n slots with Some slot => slot | None => 0 end.

Definition emit_field_patterns (slots : list (name * nat)) (fields : list field_pattern_plan) : list (fld * var) :=
  flat_map (fun field => match field with
    | PatternPun n _ => [(n, local_number slots n)]
    | PatternRename n binder _ => [(n, local_number slots binder)]
    | PatternIgnore _ => []
    end) fields.

Definition translate_field_patterns (F : frame) :=
  emit_field_patterns (fr_var F).

Inductive TypesFieldPatterns (W : world) (F : frame) (fields : list (fld * ty))
    : list (name * option apat) -> list field_pattern_plan -> Prop :=
| TypesFieldsEmpty : TypesFieldPatterns W F fields [] []
| TypesFieldsPun : forall n t rest plan,
    slook n fields = Some t -> Named F n -> TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, None) :: rest) (PatternPun n t :: plan)
| TypesFieldsIgnore : forall n t rest plan,
    slook n fields = Some t -> TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, Some APWild) :: rest) (PatternIgnore n :: plan)
| TypesFieldsRename : forall n binder t rest plan,
    slook n fields = Some t -> resolve W (fr_mod F) [binder] = None -> Named F binder ->
    TypesFieldPatterns W F fields rest plan ->
    TypesFieldPatterns W F fields ((n, Some (APPath [binder])) :: rest) (PatternRename n binder t :: plan).

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

Inductive pattern_plan : Type :=
| PatternWildcard
| PatternBinder (binder : name) (binder_type : ty)
| PatternNominal (tag : nat)
| PatternPayload (tag : nat) (binder : name) (payload : ty)
| PatternRecord (tag : nat) (fields : list field_pattern_plan).

Definition pattern_bindings (plan : pattern_plan) : list (name * ty) :=
  match plan with
  | PatternWildcard | PatternNominal _ => []
  | PatternBinder n t | PatternPayload _ n t => [(n, t)]
  | PatternRecord _ fields => field_pattern_bindings fields
  end.

Definition pattern_heads (plan : pattern_plan) : list nat :=
  match plan with
  | PatternNominal tag | PatternPayload tag _ _ | PatternRecord tag _ => [tag]
  | PatternWildcard | PatternBinder _ _ => []
  end.

Definition pattern_catches (plan : pattern_plan) : bool :=
  match plan with PatternWildcard | PatternBinder _ _ => true | _ => false end.

Definition pattern_scope (C : cx) (plan : pattern_plan) : cx :=
  fold_left (fun C q => add_local C (fst q) (snd q) LLet) (pattern_bindings plan) C.

Definition emit_pattern (slots : list (name * nat)) (plan : pattern_plan) : pattern :=
  match plan with
  | PatternWildcard => PAny None
  | PatternBinder n _ => PAny (Some (local_number slots n))
  | PatternNominal tag => PHead tag None []
  | PatternPayload tag n _ => PHead tag (Some (local_number slots n)) []
  | PatternRecord tag fields => PHead tag None (emit_field_patterns slots fields)
  end.

Definition translate_pattern (F : frame) := emit_pattern (fr_var F).

Inductive TypesPattern (W : world) (F : frame) (scrutinee : ty) : apat -> pattern_plan -> Prop :=
| TypesPatternWildcard : TypesPattern W F scrutinee APWild PatternWildcard
| TypesPatternBinder : forall n,
    resolve W (fr_mod F) [n] = None -> Named F n ->
    TypesPattern W F scrutinee (APPath [n]) (PatternBinder n scrutinee)
| TypesPatternNominal : forall path tag kind arguments,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some kind ->
    (kind = DKUnit \/ kind = DKTag) -> In (TNom tag arguments) (members scrutinee) ->
    TypesPattern W F scrutinee (APPath path) (PatternNominal tag)
| TypesPatternPayload : forall path tag arguments binder payload,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    In (TNom tag arguments) (members scrutinee) -> PayloadOf W tag arguments payload ->
    resolve W (fr_mod F) [binder] = None -> Named F binder ->
    TypesPattern W F scrutinee (APTag path (APPath [binder])) (PatternPayload tag binder payload)
| TypesPatternRecord : forall path tag arguments fields field_types plan,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    In (TNom tag arguments) (members scrutinee) -> PayloadOf W tag arguments (TRecord field_types) ->
    TypesFieldPatterns W F field_types fields plan -> NoDup (map fst (field_pattern_bindings plan)) ->
    TypesPattern W F scrutinee (APRec (Some path) fields) (PatternRecord tag plan).

Theorem pattern_translation : forall W F scrutinee source plan,
  TypesPattern W F scrutinee source plan ->
  PatOf W F scrutinee source (translate_pattern F plan)
    (pattern_bindings plan) (pattern_heads plan) (pattern_catches plan).
Proof.
  intros W F scrutinee source plan Typed. destruct Typed; cbn;
    eauto using PWild, PBinder, PUnit, PTagBind, PRecord, field_pattern_translation.
Qed.

Theorem pattern_has_plan : forall W F scrutinee source code bindings heads catch,
  PatOf W F scrutinee source code bindings heads catch ->
  exists plan, TypesPattern W F scrutinee source plan /\ translate_pattern F plan = code /\
    pattern_bindings plan = bindings /\ pattern_heads plan = heads /\ pattern_catches plan = catch.
Proof.
  intros W F scrutinee source code bindings heads catch Typed.
  destruct Typed as [|n Resolved Named|path tag kind arguments Resolved Kind Nominal Member
    |path tag arguments binder payload Resolved Kind Member Payload Read Named
    |path tag arguments fields field_types bindings code Resolved Kind Member Payload Fields Distinct].
  - exists PatternWildcard. repeat split. constructor.
  - exists (PatternBinder n scrutinee). repeat split. now constructor.
  - exists (PatternNominal tag). repeat split. eapply TypesPatternNominal; eassumption.
  - exists (PatternPayload tag binder payload). repeat split. eapply TypesPatternPayload; eassumption.
  - destruct (field_pattern_has_plan _ _ _ _ _ _ Fields) as (plan & Typed & <- & <-).
    exists (PatternRecord tag plan). repeat split. eapply TypesPatternRecord; eassumption.
Qed.

Inductive expression_plan : Type :=
| PlanUnit
| PlanThis
| PlanLocal (n : name)
| PlanBlock (body : block_plan)
| PlanIf (condition : expression_plan) (then_body : block_plan) (otherwise : option expression_plan)
| PlanReturnUnit
| PlanReturn (result : expression_plan)
| PlanBreak
| PlanDefinedCall (f : nat) (formal : cx) (needs : list item)
    (arguments : subst) (demands : list family_use) (call_site : cx) (args : list expression_plan)
| PlanAbstractCall (f : nat) (args : list expression_plan)
| PlanUnitTag (tag : nat)
| PlanTag (tag : nat) (arguments : subst) (payload : expression_plan)
| PlanRecord (tag : nat) (arguments : subst) (fields : list (fld * expression_plan))
| PlanField (object : expression_plan) (field : fld)
| PlanMatch (scrutinee : expression_plan) (arms : list (pattern_plan * expression_plan))
| PlanMethod (receiver : ty) (selected : method_call_plan) (object : expression_plan)
    (args : list expression_plan)
| PlanOperator (receiver : ty) (method : nat) (family : option family_use) (object : expression_plan)
    (args : list expression_plan)
| PlanNumeric (literal : literal_plan)
| PlanGlobalValue (symbol : nat)
(* A constant's initializer has its own numbering, recorded as a layout
   witness so the bridge can reproduce the exact code at each occurrence. *)
| PlanConstant (initializer_layout : frame) (initializer : expression_plan)
| PlanBracketedCall (expansion : expression_plan)
with block_plan : Type :=
| PlanEmpty
| PlanTail (result : expression_plan)
| PlanLet (n : name) (declared : ty) (mode : lmut) (initializer : expression_plan) (rest : block_plan)
| PlanAssign (n : name) (value : expression_plan) (rest : block_plan)
| PlanExprLast (value : expression_plan)
| PlanExprThen (value : expression_plan) (rest : block_plan)
| PlanWhile (condition : expression_plan) (body rest : block_plan)
| PlanLoop (body rest : block_plan)
| PlanLoopLast (body : block_plan)
| PlanBind (items : list binding_plan) (result : cx) (rest : block_plan)
with binding_plan : Type :=
| PlanTypeBind (symbol : nat) (meaning : ty)
| PlanValueBind (site : cx) (symbol : nat) (initializer : expression_plan)
| PlanInlineApply (items : list binding_plan)
| PlanAbstractApply (site : cx) (application : application)
| PlanFunctorCopy (target provider : nat) (target_context provider_context : cx)
    (selected : item -> item) (needs : list item) (positions : list nat)
| PlanFunctionBind (site : cx) (symbol : nat) (signature : fsig) (provider : function_provider_plan)
| PlanAttachedBind (receiver : ty) (method : nat) (arguments : subst) (site : cx)
    (provider : named_provider_plan)
| PlanWrappedBind (receiver : ty) (method : nat) (arguments : subst) (site : cx)
    (signature : fsig) (provider : wrapped_method_plan)
| PlanFunctionLambda (site : cx) (symbol : nat) (signature : fsig) (parameters : list name)
    (layout : frame) (provider : lambda_body_descriptor block_plan)
| PlanMethodLambda (receiver : ty) (method : nat) (arguments : subst) (site : cx) (signature : fsig)
    (parameters : list name) (layout : frame) (provider : lambda_body_descriptor block_plan)
| PlanFunctorClosure (site : cx) (layout : frame) (body : linked_body_descriptor (list binding_plan))
    (selected : item -> option item) (provider_needs : list item).

Definition TypesMethodBindHeader (W : world) (F : frame) (C : cx)
    (path : path) (label : name) (brackets : sapp) (method : nat) (receiver : ty)
    (signature : fsig) (arguments : inst) : Prop :=
  exists receiver_symbol readings,
    resolve_sym W (fr_mod F) path = Some receiver_symbol /\
    MethodNamed W (fr_mod F) receiver_symbol label method /\
    Brackets W (fr_mod F) (c_ty C) method brackets readings /\
    TyOf W (fr_mod F) (readings ++ c_ty C) None (ATRef path None) receiver /\
    SigOf W method (readings ++ c_ty C) (Some receiver) signature /\
    AppOf W method (readings ++ c_ty C) arguments /\
    (forall u, ~ FamilyProduces W C u (mkey (canon C receiver) method)) /\
    ~ (exists rc n ps rt b, ast_of W method = Some (ADFn rc n ps rt (Some b))).

Definition TypesLambdaHeader (W : world) (F : frame) (C : cx)
    (ps : list (name * option aty)) (ann : option aty) (body : aexpr) (signature : fsig)
    (this : option ty) (layout : frame) (provider : lambda_body_descriptor block_plan) (result : ty) : Prop :=
  @TypesLambdaProvider block_plan (fun _ _ _ _ _ _ actual => actual = result)
    W F C ps ann body signature this layout provider.

Inductive binding_activity : Type :=
| BindValueActivity
| BindApplyActivity (family : nat)
| BindOtherActivity.

Fixpoint binding_plan_activities (plan : binding_plan) : list binding_activity :=
  match plan with
  | PlanValueBind _ _ _ => [BindValueActivity]
  | PlanAbstractApply _ application => [BindApplyActivity (app_functor application)]
  | PlanInlineApply items => flat_map binding_plan_activities items
  | PlanFunctionLambda _ _ _ _ _ provider | PlanMethodLambda _ _ _ _ _ _ _ provider =>
      map (fun _ => BindValueActivity) (lambda_descriptor_captures provider) ++ [BindOtherActivity]
  | _ => [BindOtherActivity]
  end.

Definition allowed_family_activity (W : world) (activity : binding_activity) : Prop :=
  match activity with
  | BindValueActivity => False
  | BindApplyActivity family => Family W family
  | BindOtherActivity => True
  end.

Definition FamilyBindPlans (W : world) (plans : list binding_plan) : Prop :=
  Forall (allowed_family_activity W) (flat_map binding_plan_activities plans).

Definition TypesLinkedHeader (W : world) (layout : frame) (body : linked_body_descriptor (list binding_plan))
    (items : list (spec * option aexpr)) (result : cx) (after : frame) (made : list nat) : Prop :=
  @TypesLinkedBody (list binding_plan)
    (fun _ _ _ source _ C F created => source = items /\ C = result /\ F = after /\ created = made)
    FamilyBindPlans W layout body.

Definition closure_arguments (C formal : cx) : subst :=
  map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty formal).

Inductive TypesExpr (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesUnit : forall F C exp, TypesExpr W F C AEUnit exp PlanUnit TUnit
| TypesThis : forall F C exp t, fr_this F = Some t -> TypesExpr W F C AEThis exp PlanThis t
| TypesLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    TypesExpr W F C (AEPath [n] None) exp (PlanLocal n) t
| TypesBlock : forall F C exp b plan t,
    TypesBlkE W F C b exp plan t ->
    TypesExpr W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => t end)
| TypesIfThen : forall F C exp c b cp bp bt,
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    TypesExpr W F C (AEIf c b None) exp (PlanIf cp bp None) TUnit
| TypesIfElse : forall F C exp c b e cp bp bt ep et t,
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    TypesBranch W F C e exp ep et -> opt_fits et exp -> Join (branch_ty b bt) et exp t ->
    TypesExpr W F C (AEIf c b (Some e)) exp (PlanIf cp bp (Some ep)) t
| TypesReturnUnit : forall F C exp, fr_ret F = Some TUnit ->
    TypesExpr W F C (AEReturn None) exp PlanReturnUnit never
| TypesReturn : forall F C exp e expected plan actual,
    fr_ret F = Some expected ->
    TypesExpr W F C e (Some expected) plan actual -> fits actual expected ->
    TypesExpr W F C (AEReturn (Some e)) exp (PlanReturn plan) never
| TypesBreak : forall F C exp, fr_loop F = true -> TypesExpr W F C AEBreak exp PlanBreak never
| TypesDefinedCall : forall F C exp pa args f formal needs arguments g plans demands call_site,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f formal needs ->
    Demand W C (map (at_th C arguments) needs) [] C demands call_site ->
    NeedsSatisfied call_site formal arguments needs -> CallTheta W call_site formal f arguments ->
    SigOf W f (c_ty call_site) None g ->
    TypesArgs W F C args (sg_params g) plans ->
    RequirementsInForce C formal ->
    TypesExpr W F C (AECall pa None args) exp
      (PlanDefinedCall f formal needs arguments demands call_site plans) (sg_ret g)
| TypesAbstractCall : forall F C exp pa args f g plans,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g -> TypesArgs W F C args (sg_params g) plans ->
    TypesExpr W F C (AECall pa None args) exp (PlanAbstractCall f plans) (sg_ret g)
| TypesUnitTag : forall F C exp path tag,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKUnit ->
    TypesExpr W F C (AEPath path None) exp (PlanUnitTag tag) (TNom tag [])
| TypesTag : forall F C exp path brackets payload tag readings arguments payload_type plan actual,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) tag brackets readings ->
    Forall2 (fun r q => fst q = r /\ nlook r (readings ++ c_ty C) = Some (snd q)) (w_reqs W tag) arguments ->
    PayloadOf W tag arguments payload_type ->
    TypesExpr W F C payload (Some payload_type) plan actual -> fits actual payload_type ->
    TypesExpr W F C (AECall path brackets [payload]) exp (PlanTag tag arguments plan) (TNom tag arguments)
| TypesRecord : forall F C exp path brackets given tag readings arguments fields plans,
    resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) tag brackets readings ->
    Forall2 (fun r q => fst q = r /\ nlook r (readings ++ c_ty C) = Some (snd q)) (w_reqs W tag) arguments ->
    PayloadOf W tag arguments (TRecord fields) ->
    NoDup (map fst given) -> List.length given = List.length fields ->
    TypesRecordFields W F C given fields plans ->
    TypesExpr W F C (AERec path brackets given) exp (PlanRecord tag arguments plans) (TNom tag arguments)
| TypesField : forall F C exp object field plan actual payload t,
    TypesExpr W F C object None plan actual ->
    ((exists tag arguments, actual = TNom tag arguments /\ kind_sym W tag = Some DKTag /\
      PayloadOf W tag arguments payload) \/ payload = actual) ->
    (exists fields, payload = TRecord fields /\ slook field fields = Some t) ->
    TypesExpr W F C (AEField object field) exp (PlanField plan field) t
| TypesMatch : forall F C exp scrutinee arms plan st plans t,
    TypesExpr W F C scrutinee None plan st -> TypesArms W F C (canon C st) arms exp plans t ->
    TypesExpr W F C (AEMatch scrutinee arms) exp (PlanMatch plan plans) t
| TypesMethod : forall F C exp object path args receiver object_type selected signature plans,
    TypesExpr W F C object None receiver object_type ->
    TypesMethodSelection W F C (canon C object_type) path selected signature ->
    TypesArgs W F C args (sg_params signature) plans ->
    TypesExpr W F C (AEMeth object path args) exp
      (PlanMethod (canon C object_type) selected receiver plans) (sg_ret signature)
| TypesUnary : forall F C exp op operand lib name method receiver object_type family signature plans,
    operator_method op true = Some (lib, name) -> LangSym W lib NSDetached name method ->
    TypesExpr W F C operand None receiver object_type -> head (canon C object_type) <> None ->
    TypesMethodProvision W C (canon C object_type) method family signature ->
    TypesArgs W F C [] (sg_params signature) plans ->
    TypesExpr W F C (AEUnary op operand) exp
      (PlanOperator (canon C object_type) method family receiver plans) (sg_ret signature)
| TypesBinary : forall F C exp op left right lib name method receiver object_type family signature plans,
    operator_method op false = Some (lib, name) -> LangSym W lib NSDetached name method ->
    TypesExpr W F C left None receiver object_type -> head (canon C object_type) <> None ->
    TypesMethodProvision W C (canon C object_type) method family signature ->
    TypesArgs W F C [right] (sg_params signature) plans ->
    TypesExpr W F C (AEBinary op left right) exp
      (PlanOperator (canon C object_type) method family receiver plans) (sg_ret signature)
| TypesNumeric : forall F C exp text plan t,
    NumericText text -> TypesLiteral W C (literal_tree_of text) plan t ->
    TypesExpr W F C (AELit text) exp (PlanNumeric plan) t
| TypesGlobalValue : forall F C exp path symbol d name annotation t,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation None -> nlook symbol (c_vals C) = Some t ->
    TypesExpr W F C (AEPath path None) exp (PlanGlobalValue symbol) t
| TypesConstant : forall F C exp path symbol d name annotation source t layout plan actual,
    (forall n, path = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation (Some source) -> d_assumes d = [] ->
    TyOf W (d_mod d) [] None annotation t ->
    TypesExpr W layout (Cx [] [] [] [] []) source (Some t) plan actual -> fits actual t ->
    fr_mod layout = d_mod d -> fr_ret layout = None -> fr_this layout = None -> fr_loop layout = false ->
    fr_fresh layout = [] -> fr_applying layout = [] -> fr_pins layout = [] ->
    TypesExpr W F C (AEPath path None) exp (PlanConstant layout plan) t
| TypesBracketedCall : forall F C exp path brackets args f expansion t,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    CallBrackets W (fr_mod F) f brackets ->
    TypesExpr W F C (Audit.Syntax.call_block path brackets args) None expansion t ->
    TypesExpr W F C (AECall path (Some brackets) args) exp (PlanBracketedCall expansion) t
with TypesBlkE (W : world) : frame -> cx -> ablock -> option ty -> block_plan -> ty -> Prop :=
| TypesBlkCheck : forall F C ss tl exp plan t made,
    TypesBlk W (without_pins F) C ss tl exp plan t made ->
    (forall g, In g made -> ~ In g (gens t)) -> opt_fits t exp ->
    TypesBlkE W F C (ABlock ss tl) exp plan t
with TypesBlk (W : world) : frame -> cx -> list astmt -> option aexpr -> option ty -> block_plan -> ty -> list nat -> Prop :=
| TypesTail : forall F C y exp plan t,
    TypesExpr W F C y exp plan t -> TypesBlk W F C [] (Some y) exp (PlanTail plan) t []
| TypesEmpty : forall F C exp, TypesBlk W F C [] None exp PlanEmpty TUnit []
| TypesLet : forall F C st n ann y rest tl exp declared v vt t0 cont t made mu,
    LetStmt st n ann y mu ->
    (match ann with
    | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
    | None => declared = None end) ->
    TypesExpr W F C y declared v vt ->
    (match declared with Some d => fits vt d /\ t0 = d | None => t0 = vt end) ->
    Named F n ->
    TypesBlk W F (add_local C n t0 mu) rest tl exp cont t made ->
    TypesBlk W F C (st :: rest) tl exp (PlanLet n t0 mu v cont) t made
| TypesAssign : forall F C n y rest tl exp tv v vt cont t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    TypesExpr W F C y (Some tv) v vt -> fits vt tv ->
    TypesBlk W F C rest tl exp cont t made -> Named F n ->
    TypesBlk W F C (ASAssign n y :: rest) tl exp (PlanAssign n v cont) t made
| TypesExprLast : forall F C y exp v vt,
    TypesExpr W F C y None v vt ->
    TypesBlk W F C [ASExpr y] None exp (PlanExprLast v) (if ty_eqb vt never then never else TUnit) []
| TypesExprThen : forall F C y rest tl exp v vt cont t made,
    (rest <> [] \/ tl <> None) ->
    TypesExpr W F C y None v vt -> TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASExpr y :: rest) tl exp (PlanExprThen v cont) t made
| TypesWhile : forall F C c b rest tl exp cp bp bt cont t made,
    TypesCond W F C c cp -> TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASWhile c b :: rest) tl exp (PlanWhile cp bp cont) t made
| TypesLoop : forall F C b rest tl exp bp bt cont t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks W (fr_mod F) b) ->
    TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASLoop b :: rest) tl exp (PlanLoop bp cont) t made
| TypesLoopLast : forall F C b exp bp bt,
    TypesBlkE W (with_loop F) C b None bp bt -> ~ BlockBreaks W (fr_mod F) b ->
    TypesBlk W F C [ASLoop b] None exp (PlanLoopLast bp) never []
| TypesBindBlock : forall F C items rest tail exp plans result F' continuation t made created,
    TypesBindItems W F C items plans result F' made ->
    TypesBlk W F' result rest tail exp continuation t created ->
    TypesBlk W F C (ASBind items :: rest) tail exp (PlanBind plans result continuation) t (made ++ created)
with TypesArgs (W : world) : frame -> cx -> list aexpr -> list ty -> list expression_plan -> Prop :=
| TypesArgsNil : forall F C, TypesArgs W F C [] [] []
| TypesArgsCons : forall F C a rest t ts plan plans actual,
    TypesExpr W F C a (Some t) plan actual -> fits (canon C actual) (canon C t) ->
    TypesArgs W F C rest ts plans -> TypesArgs W F C (a :: rest) (t :: ts) (plan :: plans)
with TypesCond (W : world) : frame -> cx -> aexpr -> expression_plan -> Prop :=
| TypesBool : forall F C c plan t b,
    LangItem W LIBool b -> TypesExpr W F C c (Some (TNom b [])) plan t ->
    fits t (TNom b []) -> TypesCond W F C c plan
with TypesBranch (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesBranchBlock : forall F C b exp plan t,
    TypesBlkE W F C b exp plan t ->
    TypesBranch W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => branch_ty b t end)
| TypesBranchExpr : forall F C e exp plan t,
    (forall b, e <> AEBlock b) -> TypesExpr W F C e exp plan t ->
    TypesBranch W F C e exp plan t
with TypesRecordFields (W : world) : frame -> cx -> list (name * option aexpr) ->
    list (fld * ty) -> list (fld * expression_plan) -> Prop :=
| TypesRecordFieldsEmpty : forall F C given, TypesRecordFields W F C given [] []
| TypesRecordFieldsCons : forall F C given n t syntax plan actual rest plans,
    In (n, syntax) given ->
    TypesExpr W F C (match syntax with Some e => e | None => AEPath [n] None end) (Some t) plan actual ->
    fits actual t -> TypesRecordFields W F C given rest plans ->
    TypesRecordFields W F C given ((n, t) :: rest) ((n, plan) :: plans)
with TypesArms (W : world) : frame -> cx -> ty -> list (apat * aexpr) ->
    option ty -> list (pattern_plan * expression_plan) -> ty -> Prop :=
| TypesArmsEmpty : forall F C exp,
    TypesArms W F C never [] exp [] (match exp with Some e => e | None => never end)
| TypesArmsSome : forall F C st arms exp plans covered catch t,
    st <> never -> TypesArmList W F C st arms exp plans covered catch t ->
    (catch = true \/ (members st <> [] /\ forall m h, In m (members st) -> head m = Some h -> In h covered)) ->
    TypesArms W F C st arms exp plans t
with TypesArmList (W : world) : frame -> cx -> ty -> list (apat * aexpr) ->
    option ty -> list (pattern_plan * expression_plan) -> list nat -> bool -> ty -> Prop :=
| TypesArmOne : forall F C st pat body exp pattern plan t,
    TypesPattern W F st pat pattern -> TypesBranch W F (pattern_scope C pattern) body exp plan t ->
    opt_fits t exp ->
    TypesArmList W F C st [(pat, body)] exp [(pattern, plan)] (pattern_heads pattern) (pattern_catches pattern) t
| TypesArmMore : forall F C st pat body rest exp pattern plan bt plans covered catch rt t,
    rest <> [] ->
    TypesPattern W F st pat pattern -> TypesBranch W F (pattern_scope C pattern) body exp plan bt ->
    opt_fits bt exp -> TypesArmList W F C st rest exp plans covered catch rt -> Join bt rt exp t ->
    TypesArmList W F C st ((pat, body) :: rest) exp ((pattern, plan) :: plans)
      (pattern_heads pattern ++ covered) (pattern_catches pattern || catch) t
with TypesBindItems (W : world) : frame -> cx -> list (spec * option aexpr) ->
    list binding_plan -> cx -> frame -> list nat -> Prop :=
| TypesBindEmpty : forall F C, TypesBindItems W F C [] [] C F []
| TypesBindCons : forall F C item rest plan plans middle F1 made1 result F2 made2,
    TypesBindItem W F C item plan middle F1 made1 ->
    TypesBindItems W F1 middle rest plans result F2 made2 ->
    TypesBindItems W F C (item :: rest) (plan :: plans) result F2 (made1 ++ made2)
with TypesBindItem (W : world) : frame -> cx -> (spec * option aexpr) ->
    binding_plan -> cx -> frame -> list nat -> Prop :=
| TypesBindType : forall F C path symbol right arguments meaning,
    resolve_sym W (fr_mod F) path = Some symbol -> kind_sym W symbol = Some DKType ->
    ~ In symbol (fr_pins F) ->
    TyOf W (fr_mod F) (c_ty C) None (ATRef right arguments) meaning ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath right arguments))
      (PlanTypeBind symbol (canon C meaning)) (set_ty C symbol (canon C meaning)) F []
| TypesBindValue : forall F C path symbol d name annotation initializer declared source plan actual arguments,
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADVal name annotation initializer ->
    TyOf W (d_mod d) (c_ty C) None annotation declared ->
    TypesExpr W F C source (Some declared) plan actual -> fits actual declared ->
    AppOf W symbol (c_ty C) arguments ->
    TypesBindItem W F C (Spec (Some path) None None, Some source) (PlanValueBind C symbol plan)
      (provide C (IVal symbol) arguments (set_val C symbol declared)) F []
| TypesBindInlineApply : forall F C path symbol d name parameters inputs outputs bodies plans result nested made,
    resolve_sym W (fr_mod F) path = Some symbol -> w_decl W symbol = Some d ->
    d_ast d = ADFunctor name parameters inputs outputs (Some bodies) -> ~ In symbol (fr_applying F) ->
    TypesBindItems W (with_mod F (d_mod d) symbol) C (List.concat bodies) plans result nested made ->
    TypesBindItem W F C (Spec (Some path) None None, None) (PlanInlineApply plans)
      result (carry_frame_allocations F nested) made
| TypesBindAbstractApply : forall F C path plan,
    TypesApplication W F C path plan ->
    TypesBindItem W F C (Spec (Some path) None None, None) (PlanAbstractApply C plan) (app_result plan)
      (frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) (app_allocated plan)))
      (map (app_identity plan) (app_outputs plan))
| TypesBindFunctorCopy : forall F C path u provider h Cu nsu Ch nsh target fixed positions,
    resolve_sym W (fr_mod F) path = Some u -> resolve_sym W (fr_mod F) provider = Some h ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) -> nlook h (c_funs C) <> None ->
    FunCtx W u Cu nsu -> FunCtx W h Ch nsh ->
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    (forall x, Fixed W h x -> Fixed W u x) -> FixedAgreement W C h -> SameResult W u h ->
    (forall x t, In (x, t) fixed <-> Fixed W u x /\ nlook x (c_ty C) = Some t) ->
    (forall x t, nlook x (c_ty Ch) = Some t -> nlook x (c_ty Cu) <> None) ->
    CopyInterpretation W (map (fun q => (fst q, canon C (snd q))) fixed) u h Cu nsu nsh Ch positions target ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctorCopy u h Cu Ch target nsh positions) (set_fun C u fixed) F []
| TypesBindFunction : forall F C path f signature provider plan arguments,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None signature -> TypesFunctionProvider W F C provider signature plan ->
    AppOf W f (c_ty C) arguments ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctionBind C f signature plan) (provide C (IFn f) arguments (set_fn C f signature)) F []
| TypesBindAttached : forall F C path label brackets method receiver signature arguments provider name site plan,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesAttachedMethodPlan W F C provider name receiver signature site plan ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AEField (AEPath provider None) name))
      (PlanAttachedBind (canon C receiver) method (theta_of C arguments) site plan)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindWrapped : forall F C path label brackets method receiver signature arguments provider plan,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesWrappedMethodPlan W F C provider signature plan ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AEPath provider None))
      (PlanWrappedBind (canon C receiver) method (theta_of C arguments) C signature plan)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindFunctionLambda : forall F C path f signature ps ann body arguments layout provider result,
    resolve_sym W (fr_mod F) path = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None signature -> AppOf W f (c_ty C) arguments ->
    TypesLambdaHeader W F C ps ann body signature None layout provider result ->
    TypesBlkE W layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
      (Some (sg_ret signature)) (lambda_source_body provider) result ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AELam ps ann body))
      (PlanFunctionLambda C f signature (map fst ps) layout provider)
      (provide C (IFn f) arguments (set_fn C f signature)) F []
| TypesBindMethodLambda : forall F C path label brackets method receiver signature arguments ps ann body layout provider result,
    TypesMethodBindHeader W F C path label brackets method receiver signature arguments ->
    TypesLambdaHeader W F C ps ann body signature (Some receiver) layout provider result ->
    TypesBlkE W layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
      (Some (sg_ret signature)) (lambda_source_body provider) result ->
    TypesBindItem W F C (Spec (Some path) (Some label) brackets, Some (AELam ps ann body))
      (PlanMethodLambda (canon C receiver) method (theta_of C arguments) C signature (map fst ps) layout provider)
      (provide C (mkey (canon C receiver) method) arguments (set_mth C (canon C receiver) method signature)) F []
| TypesBindClosure : forall F C path provider layout descriptor target_needs provider_needs
    selected fixed items result nested made,
    let handle := linked_handle descriptor in
    let u := linked_target_symbol handle in
    let g := linked_provider_symbol handle in
    let Cu := linked_target_context descriptor in
    let Cg := linked_provider_context descriptor in
    let pins := link_plan_pins (linked_result_reading descriptor) in
    resolve_sym W (fr_mod F) path = Some u -> resolve_sym W (fr_mod F) provider = Some g ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    w_context W u = Some (Cu, target_needs) -> w_context W g = Some (Cg, provider_needs) ->
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u g ->
    LinkBinding W u Cu target_needs Cg provider_needs
      (map (fun q => (fst q, canon C (snd q))) fixed) (link_private_readings C Cu Cg)
      (link_adopted_inputs C Cu Cg) selected ->
    TypesLinkedHeader W layout descriptor items result nested made ->
    TypesBindItems W layout Cg items (linked_bind_plan descriptor) result nested made ->
    NeedsSatisfied C Cg (closure_arguments C Cg) (captured_needs selected provider_needs) ->
    (forall x t, In (x, t) fixed <-> Fixed W u x /\ nlook x (c_ty C) = Some t) ->
    (forall x, In x (map fst (c_ty Cg)) -> ~ In x (map fst (c_ty Cu)) -> InForceThrough C Cg x) ->
    (forall x t, In (x, t) pins -> exists here, nlook x (c_ty C) = Some here /\
      (forall y, In y (gens t) -> nlook y (c_ty C) <> None) /\
      canon C here = canon C (app_subst (closure_arguments C Cg) t)) ->
    TypesBindItem W F C (Spec (Some path) None None, Some (AEPath provider None))
      (PlanFunctorClosure C layout descriptor selected provider_needs) (set_fun C u fixed) F [].

Lemma binding_plan_numbering : forall W F C item plan result F' made,
  TypesBindItem W F C item plan result F' made -> fr_var F' = fr_var F.
Proof. intros W F C item plan result F' made Typed. destruct Typed; reflexivity. Qed.

Lemma bind_plans_numbering : forall W F C items plans result F' made,
  TypesBindItems W F C items plans result F' made -> fr_var F' = fr_var F.
Proof.
  intros W F C items plans result F' made Typed. induction Typed; [reflexivity|].
  rewrite IHTyped. eapply binding_plan_numbering; eassumption.
Qed.

Definition splice_bindings (bindings : list binding) (body : expr) : expr := fold_right EBind body bindings.

Lemma splice_bindings_append : forall left right,
  splice_bindings (left ++ right) = fun body => splice_bindings left (splice_bindings right body).
Proof.
  induction left as [|b left IH]; intro right; [reflexivity|].
  change ((fun body => EBind b (splice_bindings (left ++ right) body)) =
    fun body => EBind b (splice_bindings left (splice_bindings right body))).
  now rewrite IH.
Qed.

Definition application_binding (C : cx) (a : application) : binding :=
  BApply (app_functor a) (app_arguments a) (map (fun t => (t, app_identity a t)) (app_outputs a))
    (satisfaction_map C (app_input a) (app_arguments a) (app_needs a))
    (map (rkey_of (app_result a)) (app_provisions a)).

Inductive placement_requirement : Type :=
| PlaceFunction (slot : nat) (body : expr) (section : option subst)
| PlaceFunctor (slot : nat) (body : functor_code) (source : nat * nat)
| PlaceLayout (layout : frame) (receiver : option ty) (parameters : list name).

Definition placement_in (W : world) (P : program) (requirement : placement_requirement) : Prop :=
  match requirement with
  | PlaceFunction slot body section =>
      Fresh W slot /\ body_of P slot = Some body /\
      match section with
      | Some reads => nlook slot (lambda_sections (program_origins P)) = Some reads
      | None => True
      end
  | PlaceFunctor slot body source =>
      Fresh W slot /\ funbody_of P slot = Some body /\
      nlook slot (linked_origins (program_origins P)) = Some source
  | PlaceLayout layout receiver parameters => Numbering layout receiver parameters
  end.

Definition placements_in (W : world) (P : program) :=
  Forall (placement_in W P).

Definition function_placements (resolve : nat -> nat) (signature : fsig)
    (provider : function_provider_plan) : list placement_requirement :=
  match provider with
  | NamedFunctionProvider _ => []
  | AbstractFunctionForwarder occurrence handle =>
      [PlaceFunction (resolve occurrence) (forwarder_body handle signature) None]
  end.

Lemma function_placements_exact : forall W P resolve signature provider,
  placements_in W P (function_placements resolve signature provider) <->
  FunctionProviderPlacement W P resolve signature provider.
Proof.
  intros W P resolve signature [named|occurrence handle]; cbn [function_placements FunctionProviderPlacement];
    unfold placements_in; [split; [tauto|intros; constructor]|].
  rewrite Forall_cons_iff. cbn [placement_in]. unfold TranslatesForwarder.
  split; [tauto|]. intros [Fresh Body]. split; [tauto|constructor].
Qed.

Record emission (A : Type) := Emission {
  emitted_code : A;
  emitted_placements : list placement_requirement
}.
Arguments Emission {A}.
Arguments emitted_code {A}.
Arguments emitted_placements {A}.

Definition emit_only {A} (code : A) : emission A := Emission code [].

Definition emit_map {A B} (make : A -> B) (source : emission A) : emission B :=
  Emission (make (emitted_code source)) (emitted_placements source).

Definition emit_pair {A B C} (make : A -> B -> C) (first : emission A) (second : emission B) : emission C :=
  Emission (make (emitted_code first) (emitted_code second))
    (emitted_placements first ++ emitted_placements second).

Definition emit_each {A B} (emit : A -> emission B) (sources : list A) : emission (list B) :=
  Emission (map (fun source => emitted_code (emit source)) sources)
    (flat_map (fun source => emitted_placements (emit source)) sources).

Definition emit_flat {A B} (emit : A -> emission (list B)) (sources : list A) : emission (list B) :=
  Emission (flat_map (fun source => emitted_code (emit source)) sources)
    (flat_map (fun source => emitted_placements (emit source)) sources).

Definition require_placement {A} (requirement : placement_requirement) (source : emission A) : emission A :=
  Emission (emitted_code source) (requirement :: emitted_placements source).

Definition lambda_capture_bindings (slots : list (name * nat)) (provider : lambda_body_descriptor block_plan) :=
  map (fun '(n, symbol) => BValue symbol (EVar (local_number slots n)))
    (lambda_descriptor_captures provider).

Lemma lambda_prefix_bindings : forall F provider,
  splice_bindings (lambda_capture_bindings (fr_var F) provider) = lambda_descriptor_prefix F provider.
Proof.
  intros F provider. unfold lambda_capture_bindings, lambda_descriptor_prefix, translate_lambda_captures.
  induction (lambda_descriptor_captures provider) as [|[n v] rest IH]; [reflexivity|].
  change ((fun body => EBind (BValue v (EVar (local_number (fr_var F) n)))
    (splice_bindings (map (fun '(n, symbol) => BValue symbol (EVar (local_number (fr_var F) n))) rest) body)) =
    fun body => bind_value_expression v (EVar (var_of F n))
      (fold_right (fun q rest => bind_value_expression (snd q) (EVar (var_of F (fst q))) rest) body rest)).
  now rewrite IH.
Qed.

Definition lambda_binding_placements (resolve : nat -> nat) (site : cx) (parameters : list name)
    (layout : frame) (provider : lambda_body_descriptor block_plan) (body : emission expr) :=
  PlaceLayout layout (fr_this layout) parameters ::
  PlaceFunction (resolve (lambda_occurrence provider))
    (lambda_descriptor_code layout provider (emitted_code body)) (Some (c_sub site)) ::
  emitted_placements body.

Definition emit_lambda_binding (resolve : nat -> nat) (slots : list (name * nat)) (site : cx)
    (parameters : list name) (layout : frame) (provider : lambda_body_descriptor block_plan)
    (body : emission expr) (make : nat -> smap -> binding) : emission (list binding) :=
  Emission (lambda_capture_bindings slots provider ++
    [make (resolve (lambda_occurrence provider)) (lambda_descriptor_needs site provider)])
    (lambda_binding_placements resolve site parameters layout provider body).

Definition linked_binding_placements (resolve : nat -> nat) (layout : frame)
    (descriptor : linked_body_descriptor (list binding_plan)) (body : emission (list binding)) :=
  let handle := linked_handle descriptor in
  PlaceLayout layout None [] ::
  PlaceFunctor (resolve (linked_occurrence handle))
    (linked_descriptor_code descriptor (emitted_code body))
    (linked_target_symbol handle, linked_provider_symbol handle) ::
  emitted_placements body.

Fixpoint emit_expr (resolve : nat -> nat) (slots : list (name * nat)) (C : cx)
    (plan : expression_plan) : emission expr :=
  match plan with
  | PlanUnit => emit_only EUnit
  | PlanThis => emit_only (EVar 0)
  | PlanLocal n => emit_only (EVar (local_number slots n))
  | PlanBlock body => emit_block resolve slots C body
  | PlanIf condition body other =>
      let otherwise := match other with
        | Some plan => emit_expr resolve slots C plan
        | None => emit_only EUnit
        end in
      emit_pair (fun '(test, yes) no => EIf test yes no)
        (emit_pair pair (emit_expr resolve slots C condition) (emit_block resolve slots C body))
        otherwise
  | PlanReturnUnit => emit_only (EReturn EUnit)
  | PlanReturn result => emit_map EReturn (emit_expr resolve slots C result)
  | PlanBreak => emit_only EBreak
  | PlanDefinedCall f formal needs arguments demands call_site args =>
      emit_map (fun values => translate_demands C demands (translate_call call_site formal f arguments needs values))
        (emit_each (emit_expr resolve slots C) args)
  | PlanAbstractCall f args => emit_map (EInvoke f) (emit_each (emit_expr resolve slots C) args)
  | PlanUnitTag tag => emit_only (ETag tag [] EUnit)
  | PlanTag tag arguments payload => emit_map (ETag tag arguments) (emit_expr resolve slots C payload)
  | PlanRecord tag arguments fields =>
      emit_map (fun values => ETag tag arguments (ERecord values))
        (emit_each (fun '(n, child) => emit_map (pair n) (emit_expr resolve slots C child)) fields)
  | PlanField object field => emit_map (fun value => EField value field) (emit_expr resolve slots C object)
  | PlanMatch scrutinee arms =>
      emit_pair EMatch (emit_expr resolve slots C scrutinee)
        (emit_each (fun '(pattern, body) => emit_map (pair (emit_pattern slots pattern))
          (emit_expr resolve slots (pattern_scope C pattern) body)) arms)
  | PlanMethod receiver selected object args =>
      emit_pair (emit_method_call C receiver selected) (emit_expr resolve slots C object)
        (emit_each (emit_expr resolve slots C) args)
  | PlanOperator receiver method family object args =>
      emit_pair (emit_provided_method C receiver method family) (emit_expr resolve slots C object)
        (emit_each (emit_expr resolve slots C) args)
  | PlanNumeric literal => emit_only (emit_literal C literal)
  | PlanGlobalValue symbol => emit_only (EVal symbol)
  | PlanConstant layout initializer =>
      require_placement (PlaceLayout layout None [])
        (emit_expr resolve (fr_var layout) (Cx [] [] [] [] []) initializer)
  | PlanBracketedCall expansion => emit_expr resolve slots C expansion
  end
with emit_block (resolve : nat -> nat) (slots : list (name * nat)) (C : cx)
    (plan : block_plan) : emission expr :=
  match plan with
  | PlanEmpty => emit_only EUnit
  | PlanTail result => emit_expr resolve slots C result
  | PlanLet n t mode initializer rest =>
      emit_pair (ELet (local_number slots n)) (emit_expr resolve slots C initializer)
        (emit_block resolve slots (add_local C n t mode) rest)
  | PlanAssign n value rest =>
      emit_pair (fun value => ESeq (EAssign (local_number slots n) value))
        (emit_expr resolve slots C value) (emit_block resolve slots C rest)
  | PlanExprLast value => emit_map (fun value => ESeq value EUnit) (emit_expr resolve slots C value)
  | PlanExprThen value rest =>
      emit_pair ESeq (emit_expr resolve slots C value) (emit_block resolve slots C rest)
  | PlanWhile condition body rest =>
      emit_pair ESeq (emit_pair EWhile (emit_expr resolve slots C condition) (emit_block resolve slots C body))
        (emit_block resolve slots C rest)
  | PlanLoop body rest =>
      emit_pair (fun body => ESeq (ELoop body)) (emit_block resolve slots C body) (emit_block resolve slots C rest)
  | PlanLoopLast body => emit_map (fun body => ESeq (ELoop body) EUnit) (emit_block resolve slots C body)
  | PlanBind items result rest =>
      emit_pair splice_bindings (emit_flat (emit_bind_item resolve slots) items) (emit_block resolve slots result rest)
  end
with emit_bind_item (resolve : nat -> nat) (slots : list (name * nat)) (plan : binding_plan)
    : emission (list binding) :=
  match plan with
  | PlanTypeBind symbol meaning => emit_only [BType symbol meaning]
  | PlanValueBind site symbol initializer =>
      emit_map (fun value => [BValue symbol value]) (emit_expr resolve slots site initializer)
  | PlanInlineApply items => emit_flat (emit_bind_item resolve slots) items
  | PlanAbstractApply site plan => emit_only [application_binding site plan]
  | PlanFunctorCopy target provider Cu Ch selected needs positions =>
      emit_only [BCopyFunctor target provider (map (fun k => (rkey_of Ch k, rkey_of Cu (selected k))) needs) positions]
  | PlanFunctionBind site symbol signature provider =>
      Emission [BFunction symbol (function_plan_target resolve provider)
        (function_plan_capture provider) (function_plan_provisions site provider)]
        (function_placements resolve signature provider)
  | PlanAttachedBind receiver method arguments site provider =>
      emit_only [BMethod receiver method arguments (provider_symbol provider)
        (named_provider_capture provider) (named_provider_provisions site provider)]
  | PlanWrappedBind receiver method arguments site signature provider =>
      let named := wrapped_method_provider provider in
      let slot := resolve (wrapped_method_occurrence provider) in
      Emission [BMethod receiver method arguments slot (named_provider_capture named)
        (named_provider_provisions site named)] [PlaceFunction slot (wrapped_method_code signature provider) None]
  | PlanFunctionLambda site symbol signature parameters layout provider =>
      let body_site := lambda_parameter_context (lambda_plan_capture_context provider) parameters (sg_params signature) in
      emit_lambda_binding resolve slots site parameters layout provider
        (emit_block resolve (fr_var layout) body_site (lambda_source_body provider))
        (fun target needs => BFunction symbol target (lexical_capture site) needs)
  | PlanMethodLambda receiver method arguments site signature parameters layout provider =>
      let body_site := lambda_parameter_context (lambda_plan_capture_context provider) parameters (sg_params signature) in
      emit_lambda_binding resolve slots site parameters layout provider
        (emit_block resolve (fr_var layout) body_site (lambda_source_body provider))
        (fun target needs => BMethod receiver method arguments target (lexical_capture site) needs)
  | PlanFunctorClosure site layout descriptor selected needs =>
      let handle := linked_handle descriptor in
      let Cu := linked_target_context descriptor in
      let Cg := linked_provider_context descriptor in
      let body := emit_flat (emit_bind_item resolve (fr_var layout)) (linked_bind_plan descriptor) in
      Emission [BFunctor (linked_target_symbol handle) (resolve (linked_occurrence handle)) (lexical_capture site)
        (link_map Cu Cg selected needs)
        (satisfaction_map site Cg (closure_arguments site Cg) (captured_needs selected needs))]
        (linked_binding_placements resolve layout descriptor body)
  end.

Definition identity_slots (token : nat) := token.

Definition expression_emission (resolve : nat -> nat) (F : frame) := emit_expr resolve (fr_var F).
Definition block_emission (resolve : nat -> nat) (F : frame) := emit_block resolve (fr_var F).
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

Local Lemma lambda_from_header : forall W P F C ps ann body signature this layout provider result code resolve,
  TypesLambdaHeader W F C ps ann body signature this layout provider result ->
  BlkE W P layout (lambda_descriptor_site signature ps provider) (ABlock [] (Some body))
    (Some (sg_ret signature)) code result ->
  Numbering layout (fr_this layout) (map fst ps) ->
  LambdaBodyPlacement W P C layout resolve provider code ->
  LambdaProvider W P F C ps ann body signature this (resolve (lambda_occurrence provider))
    (lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider).
Proof.
  intros W P F C ps ann body signature this layout provider result code resolve Typed Body Numbered
    [Fresh [Code Origin]].
  destruct Typed as [body_type caps capvals Cl Length Distinct Annotations Return Capture Context
    Module Ret This Loop Keep NamedBody NamedOuter Result Fits Needs Unique].
  cbn beta in Result. subst body_type. rewrite This in Numbered.
  eapply (@LP W P F C ps ann body signature this _ caps capvals Cl layout code result
    (lambda_provision_needs provider) (lambda_descriptor_needs C provider)); try eassumption; reflexivity.
Qed.

Local Lemma linked_from_header : forall W P resolve layout descriptor items result nested made,
  TypesLinkedHeader W layout descriptor items result nested made ->
  BindItems W P layout (linked_provider_context descriptor) items
    (splice_bindings (emitted_code (bind_items_emission resolve layout (linked_bind_plan descriptor))))
    result nested made ->
  Numbering layout None [] ->
  LinkedBodyPlacement W P resolve descriptor
    (emitted_code (bind_items_emission resolve layout (linked_bind_plan descriptor))) ->
  LinkBody W P (linked_target_symbol (linked_handle descriptor)) (linked_provider_symbol (linked_handle descriptor))
    (resolve (linked_occurrence (linked_handle descriptor))) (link_plan_pins (linked_result_reading descriptor)).
Proof.
  intros W P resolve layout descriptor items result nested made Typed Body Numbered Placed.
  destruct Typed as [dg name parameters inputs outputs bodies target_needs provider_needs Cb Fb created gen promised
    target provider Cu Cg outs reading plan Declared Ast Target Provider Module Ret This Loop Protected
    [Source [Result [Nested Made]]] NoInput Outputs Checked FamilyAllowed].
  subst items result nested made.
  destruct Checked as [_ Site PinsProvision ResultTypes PinsExact PinsScoped].
  set (bindings := emitted_code (bind_items_emission resolve layout plan)) in *.
  assert (FamilyCode : Family W target -> FamilyBindings W bindings).
  { intro Family. apply (proj1 (family_bind_emission_exact W resolve layout plan)). now apply FamilyAllowed. }
  assert (Translated : TranslatesLinkedBody W P target provider (resolve (linked_occurrence (linked_handle descriptor)))
    Cu Cg (link_plan_site reading) outs bindings (link_plan_keys reading)) by exact Placed.
  rewrite Site in PinsProvision, ResultTypes, Translated.
  eapply (@LB W P target provider _ dg name parameters inputs outputs bodies Cu target_needs Cg provider_needs
    layout (splice_bindings bindings) Cb Fb created outs gen promised (link_plan_keys reading) bindings
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
    ExBreak, ExCallD, ExCallA, ExUnitTag, ExTag, ExRecord, ExField, ExMatch, ExMeth, ExUnary, ExBinary,
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
    unfold WrappedMethodPlacement. tauto.
  - rewrite splice_bindings_append, lambda_prefix_bindings.
    eapply (@BFnLambda W P F C path0 f signature ps ann body (resolve (lambda_occurrence provider))
      (lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider) arguments); try eassumption.
    eapply (@lambda_from_header W P F C ps ann body signature None layout provider result
      (emitted_code (block_emission resolve layout (lambda_descriptor_site signature ps provider)
        (lambda_source_body provider))) resolve); eassumption.
  - rewrite splice_bindings_append, lambda_prefix_bindings.
    eapply (@method_binding_from_header W P F C path0 label brackets method receiver signature arguments
      (AELam ps ann body) (resolve (lambda_occurrence provider)) (lexical_capture C)
      (lambda_descriptor_prefix F provider) (lambda_descriptor_needs C provider)); [eassumption|].
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

Theorem unit_tag_source_has_plan : forall W P F C path tag,
  (forall n, path = [n] -> slook n (c_locals C) = None) ->
  resolve_sym W (fr_mod F) path = Some tag -> kind_sym W tag = Some DKUnit ->
  HasExpressionPlan W P F C (AEPath path None).
Proof.
  intros W P F C path tag NotLocal Resolve Kind expected code t Typed. inversion Typed; subst.
  all: try solve [exfalso; match goal with
    | Read : slook ?name (c_locals ?context) = Some _ |- _ =>
      pose proof (NotLocal name eq_refl); congruence
    end].
  all: try solve [eexists (PlanUnitTag _); split; [eapply TypesUnitTag; eassumption|reflexivity]].
  all: match goal with
  | Read : resolve_sym ?world (fr_mod ?frame) ?path = Some ?symbol,
    Declared : w_decl ?world ?symbol = Some ?d,
    UnitKind : kind_sym ?world ?target = Some DKUnit,
    Syntax : d_ast ?d = ADVal _ _ _ |- _ =>
    assert (symbol = target) by congruence; subst symbol;
    unfold kind_sym in UnitKind; rewrite Declared, Syntax in UnitKind; discriminate
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

Local Lemma lambda_header_renumber : forall W F C ps ann body signature this layout provider result slots,
  TypesLambdaHeader W F C ps ann body signature this layout provider result ->
  SameLocalNames F slots ->
  TypesLambdaHeader W (renumber_body_frame F slots) C ps ann body signature this layout provider result.
Proof.
  intros W F C ps ann body signature this layout provider result slots Typed Names.
  destruct Typed as [body_type caps capvals Cl Length Distinct Annotations Return Capture Context
    Module Ret This Loop Keep NamedBody NamedOuter Result Fits Needs Unique].
  eapply TLP; try eassumption.
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
    TypesReturn, TypesBreak, TypesDefinedCall, TypesAbstractCall, TypesUnitTag, TypesTag, TypesRecord,
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
