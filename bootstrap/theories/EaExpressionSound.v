(* Assemble the expression cases with the same scope and final-code
   contracts used by the executable block and binding checks. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaExpr EaControlSound EaChecked EaRecSound.
From NM Require Import LegacyBodyJudgments.
From NM Require EaConstructSound EaCallExpressionSound EaDecideSound EaMatchSound
  EaMethodSound EaLiteralSound EaValuePathSound EaContextTableSound EaFamilyUseSound EaTypes EaTypesSound
  EaDefinedCallSound PublicContextFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Expressions.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Local Notation Expr := (ExPostAt E P valid post R).
Local Notation Block := (BlkEPostAt E P valid post R).
Local Notation Action := (ExActionSound E P valid post).

Hypothesis Callbacks : RecSound E P valid post R.
Hypothesis CallerSection : forall F C state, valid F C state -> PublicContextFacts.CxSection C.
Hypothesis LocalScope : forall F C bindings state,
  valid F (add_binds C bindings) state <-> valid F C state.
Hypothesis Contexts : EaContextTableSound.ContextTableSound W.
Hypothesis Demand : EaDefinedCallSound.DemandSound E.
Hypothesis Families : EaFamilyUseSound.FamilyUseSound E.
Hypothesis World : exists bundle, WorldOf bundle W.
Hypothesis Constants : EaValuePathSound.ConstantsChecked E R P valid post.

Local Lemma call_sound : forall n,
  (forall depth, depth < n -> Expr depth) -> Expr n ->
  forall F C path brackets arguments expected,
  Action F C (AECall path brackets arguments) expected
    (ex E R (S n) F C (AECall path brackets arguments) expected).
Proof.
  intros n Below ExprN F C path brackets arguments expected before [code t] after Run.
  pose proof Run as Call. cbn [ex] in Run. fold W in Run.
  destruct (resolve_sym W (fr_mod F) path) as [symbol|] eqn:Resolved; [|discriminate Run].
  cbn [mbind ret] in Run.
  destruct (w_decl W symbol) as [d|] eqn:Declared; [|discriminate Run].
  cbn [mbind ret] in Run.
  destruct (d_ast d) as [| | |name params annotation| | |receiver name params result body| |] eqn:Ast;
    try discriminate Run.
  - exact (EaConstructSound.tag_call_sound E R P valid post n ExprN F C path brackets arguments
      expected symbol d name params annotation Resolved Declared Ast before (code, t) after Call).
  - destruct receiver; try discriminate Run.
    destruct brackets as [brackets|].
    + exact (EaCallExpressionSound.bracketed_plain_call_sound E R P valid post n (proj1 Callbacks)
        F C path brackets arguments expected symbol d name params result body Resolved Declared Ast
        before (code, t) after Call).
    + exact (EaCallExpressionSound.plain_call_sound E R P valid post Contexts CallerSection Demand
        n Below F C path arguments expected symbol d name params result body Resolved Declared Ast
        before (code, t) after Call).
Qed.

Local Lemma method_expression_sound : forall n,
  (forall depth, depth < n -> Expr depth) -> Expr n ->
  forall F C receiver path arguments expected,
  Action F C (AEMeth receiver path arguments) expected
    (ex E R (S n) F C (AEMeth receiver path arguments) expected).
Proof.
  intros n Below ExprN F C receiver path arguments expected before [code t] after Run.
  cbn [ex] in Run.
  apply mbind_ok in Run as [[ox ot] [middle [Object Method]]].
  destruct (ExprN _ _ _ _ _ _ _ Object) as [Earlier TypedObject].
  destruct (EaMethodSound.method_call_post_sound E R P valid post CallerSection Contexts
    Demand Families World n Below _ _ _ _ _ _ _ _ _ Method) as [Backward TypedCall].
  split; [intros Post; now apply Earlier, Backward|]. intros Valid Post.
  destruct (TypedObject Valid (Backward Post)) as [Middle ObjectTyped].
  destruct (TypedCall Middle Post) as [Final CallTyped].
  split; [exact Final|eapply ExMeth; eassumption].
Qed.

Definition operator_expression (op : string) (left : aexpr) (right : option aexpr) : aexpr :=
  match right with None => AEUnary op left | Some argument => AEBinary op left argument end.

Local Lemma operator_expression_sound : forall n,
  (forall depth, depth < n -> Expr depth) -> Expr n ->
  forall F C op left right expected,
  Action F C (operator_expression op left right) expected
    (ex E R (S n) F C (operator_expression op left right) expected).
Proof.
  intros n Below ExprN F C op left right expected before [code t] after Run.
  set (unary := match right with None => true | Some _ => false end).
  set (arguments := match right with None => [] | Some argument => [argument] end).
  assert (Read : (let* (lib, name) := lift (operator_method op unary) "unknown operator" in
    let* method := lift (EaTypes.lang_sym W lib NSDetached name)
      ("operator " ++ bq name ++ " needs lib/" ++ lib ++ ".moss to be loaded") in
    let* (receiver, receiver_type) := ex E R n F C left None in
    let* _ := check (is_some (head (canon C receiver_type)))
      (qty W (canon C receiver_type) ++ " has no methods (it has no nominal head)") in
    provided E R n F C receiver (canon C receiver_type) method arguments) before =
      Ok ((code, t), after)) by (destruct right; exact Run).
  clear Run. rename Read into Run.
  apply mbind_ok in Run as ([lib name] & start & Op & Run).
  destruct (operator_method op unary) as [[library method_name]|] eqn:Operator; [|discriminate Op].
  injection Op as <- <- <-.
  apply mbind_ok in Run as (method & start & Read & Run).
  destruct (EaTypes.lang_sym W library NSDetached method_name) as [symbol|] eqn:Resolved; [|discriminate Read].
  injection Read as <- <-.
  apply mbind_ok in Run as [[receiver receiver_type] [middle [Object Run]]].
  apply mbind_ok in Run as ([] & start & Check & Run).
  destruct (head (canon C receiver_type)) as [tag|] eqn:Head; [|discriminate Check].
  injection Check as <-.
  destruct (ExprN _ _ _ _ _ _ _ Object) as [Earlier TypedObject].
  destruct (EaMethodSound.provided_post_sound E R P valid post CallerSection Families n
    Below _ _ _ _ _ _ _ _ _ Run) as [Backward TypedCall].
  split; [intros Post; now apply Earlier, Backward|]. intros Valid Post.
  destruct (TypedObject Valid (Backward Post)) as [Middle ObjectTyped].
  destruct (TypedCall Middle Post) as [Final CallTyped].
  split; [exact Final|].
  assert (Nominal : head (canon C receiver_type) <> None) by congruence.
  destruct right as [right|]; cbn [operator_expression] in *.
  - eapply ExBinary; [exact Operator|eapply EaTypesSound.lang_sym_sound; exact Resolved|exact ObjectTyped|exact Nominal|exact CallTyped].
  - eapply ExUnary; [exact Operator|eapply EaTypesSound.lang_sym_sound; exact Resolved|exact ObjectTyped|exact Nominal|exact CallTyped].
Qed.

Theorem expression_step_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  (forall depth, depth < n -> Block depth) -> Expr n.
Proof.
  intros [|n] Expressions Blocks F C syntax expected.
  { intros before result after Run. discriminate. }
  assert (Below : forall depth, depth < n -> Expr depth) by (intros; apply Expressions; lia).
  assert (ExprN : Expr n) by (apply Expressions; lia).
  destruct syntax as [| |path brackets|path brackets arguments|receiver field|receiver path arguments|
    path brackets fields|condition yes no|scrutinee arms|returned| |block|op operand|op left right|text|
    params result body|fields].
  - intros before [code t] after Run. injection Run as <- <- <-.
    split; [tauto|intros Valid _; split; [exact Valid|constructor]].
  - intros before [code t] after Run. cbn [ex] in Run.
    destruct (fr_this F) as [receiver|] eqn:This; [|discriminate Run].
    injection Run as <- <- <-. split; [tauto|intros Valid _; split; [exact Valid|now apply ExThis]].
  - exact (EaValuePathSound.value_path_sound E R P valid post Constants n F C path brackets expected).
  - exact (call_sound n Below ExprN F C path brackets arguments expected).
  - now apply EaConstructSound.field_expression_sound.
  - exact (method_expression_sound n Below ExprN F C receiver path arguments expected).
  - now apply EaConstructSound.record_expression_sound.
  - now apply EaDecideSound.if_post_sound.
  - now apply EaMatchSound.match_post_sound.
  - now apply return_post_sound.
  - apply break_post_sound.
  - intros before [code t] after Run. cbn [ex] in Run.
    apply mbind_ok in Run as [[value actual] [middle [Body Run]]].
    injection Run as <- <- <-.
    destruct (Blocks n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Body) as [Backward Typed].
    split; [exact Backward|]. intros Valid Post.
    destruct (Typed Valid Post) as [Final BlockTyped]. split; [exact Final|now apply ExBlock].
  - exact (operator_expression_sound n Below ExprN F C op operand None expected).
  - exact (operator_expression_sound n Below ExprN F C op left (Some right) expected).
  - eapply EaLiteralSound.literal_expression_sound; [apply CallerSection|exact Families].
  - intros before result' after Run. discriminate Run.
  - now apply EaConstructSound.bare_record_expression_sound.
Qed.
End Expressions.
