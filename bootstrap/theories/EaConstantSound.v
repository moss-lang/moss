(* A cached constant keeps its declaration's typed initializer and code. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaControlSound.
From NM Require Import LegacyBodyJudgments.
From NM Require EaExpr EaDecide EaNumbering NMap WorldSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Definition ConstantReading (W : world) (P : program) (d : decl) (syntax : aty)
    (initial : aexpr) (code : expr) (declared : ty) : Prop :=
  d_assumes d = [] /\ TyOf W (d_mod d) [] None syntax declared /\
  exists F actual, Ex W P F (Cx [] [] [] [] []) initial (Some declared) code actual /\
    fits actual declared /\ fr_mod F = d_mod d /\ fr_ret F = None /\ fr_this F = None /\
    fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F None [].

Definition TypedConstantCache (W : world) (P : program) (state : est) : Prop :=
  forall symbol d n syntax initial code declared,
    w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
    NMap.nget (es_const state) symbol = Some (code, declared) ->
    ConstantReading W P d syntax initial code declared.

Lemma initial_constant_cache_typed : forall W P counter, TypedConstantCache W P (init_state counter).
Proof. intros W P counter symbol d n syntax initial code declared _ _ Read. discriminate Read. Qed.

Lemma cache_constant_sound : forall W P state after symbol d n syntax initial code declared,
  TypedConstantCache W P state ->
  w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
  ConstantReading W P d syntax initial code declared ->
  put_const symbol (code, declared) state = Ok (tt, after) ->
  TypedConstantCache W P after.
Proof.
  intros W P state after symbol d n syntax initial code declared Cache Found Ast Reading Write.
  injection Write as <-. intros other d' n' syntax' initial' code' declared' Found' Ast' Read.
  cbn [es_const] in Read.
  destruct (Nat.eq_dec other symbol) as [->|Different].
  - rewrite WorldSound.nget_nset_same in Read. inversion Read; subst code' declared'.
    rewrite Found in Found'. injection Found' as <-.
    rewrite Ast in Ast'. inversion Ast'; subst n' syntax' initial'. exact Reading.
  - rewrite WorldSound.nget_nset_other in Read by exact Different. eapply Cache; eassumption.
Qed.

Lemma constant_reading_ex : forall W P F C path expected symbol d n syntax initial code declared,
  (forall local, path = [local] -> slook local (c_locals C) = None) ->
  resolve_sym W (fr_mod F) path = Some symbol ->
  w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
  ConstantReading W P d syntax initial code declared ->
  Ex W P F C (AEPath path None) expected code declared.
Proof.
  intros W P F C path expected symbol d n syntax initial code declared Local Resolve Found Ast
    (NoAssumes & TypeRead & Body).
  eapply ExConst; eassumption.
Qed.

Theorem constant_initializer_sound : forall E R P d syntax initial declared code actual before after,
  d_assumes d = [] ->
  ty_of (e_W E) (e_fuel E) (d_mod d) [] None syntax = Ok declared ->
  let F := Frame (d_mod d) None None false
    (mk_frame_vars false [] (EaDecide.ex_names (e_W E) (e_fuel E) (d_mod d) initial)) [] [] [] in
  (r_ex R F (Cx [] [] [] [] []) initial (Some declared) before = Ok ((code, actual), after) ->
    Ex (e_W E) P F (Cx [] [] [] [] []) initial (Some declared) code actual) ->
  r_ex R F (Cx [] [] [] [] []) initial (Some declared) before = Ok ((code, actual), after) ->
  fitsb actual declared = true -> ConstantReading (e_W E) P d syntax initial code declared.
Proof.
  intros E R P d syntax initial declared code actual before after Assumes TypeRead F Checked Run Fits.
  split; [exact Assumes|split; [eapply ty_of_sound; exact TypeRead|]].
  exists F, actual. split; [now apply Checked|split; [now apply fitsb_exact|]].
  repeat (split; [reflexivity|]).
  eapply EaNumbering.mk_frame_vars_numbering; [reflexivity|constructor].
Qed.

Theorem constant_expression_implies_initializer : forall W P F C name expected symbol d syntax initial code t,
  c_locals C = [] ->
  resolve_sym W (fr_mod F) [name] = Some symbol ->
  w_decl W symbol = Some d -> d_ast d = ADVal name syntax (Some initial) ->
  Ex W P F C (AEPath [name] None) expected code t ->
  ConstElab W P d syntax initial.
Proof.
  intros W P F C name expected symbol d syntax initial code t Locals Resolve Declared Ast Typed.
  inversion Typed; subst;
    repeat match goal with
    | R1 : resolve_sym ?world ?p ?pa = Some ?s1, R2 : resolve_sym ?world ?p ?pa = Some ?s2 |- _ =>
        assert_fails (constr_eq s1 s2); assert (s1 = s2) by congruence; subst s1
    | D1 : w_decl ?world ?s = Some ?d1, D2 : w_decl ?world ?s = Some ?d2 |- _ =>
        assert_fails (constr_eq d1 d2); assert (d1 = d2) by congruence; subst d1
    end.
  - match goal with
    | Local : slook _ (c_locals C) = Some _ |- _ => rewrite Locals in Local; discriminate
    end.
  - match goal with
    | Bare : BareTag ?world ?s, D : w_decl ?world ?s = Some ?d,
      A : d_ast ?d = ADVal _ _ (Some _) |- _ =>
        destruct Bare as [? Bare]; unfold ast_of in Bare; rewrite D in Bare; cbn in Bare; rewrite A in Bare; discriminate
    end.
  - congruence.
  - match goal with
    | A : d_ast ?d = ADVal _ ?t0 (Some ?i), B : d_ast ?d = ADVal _ ?t1 (Some ?j) |- _ =>
        assert_fails (constr_eq i j);
        assert (t0 = t1 /\ i = j) as [-> ->] by (rewrite A in B; now inversion B)
    end.
    unfold ConstElab. split; [assumption|]. exists t. split; [assumption|].
    match goal with
    | Body : exists F t', Ex _ _ F _ _ _ ?x t' /\ _ |- _ =>
        destruct Body as (F0 & t' & Body); exists F0, x, t'; exact Body
    end.
Qed.

From NM Require Import EaChecked EaCodeEffects.
From NM Require EaExpressionCodeEffects.

Section ReadConstant.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Let empty := Cx [] [] [] [] [].

Hypothesis PostBack : forall before after, CodeTransition before after -> post after -> post before.
Hypothesis ReadExpr : forall F C syntax expected,
  ExActionSound E P valid post F C syntax expected (r_ex R F C syntax expected).
Hypothesis ExprEffect : forall F C syntax expected,
  PreservesOldCode (r_ex R F C syntax expected).
Hypothesis Cache : forall F C state, valid F C state -> TypedConstantCache W P state.
Hypothesis Enter : forall F C state d initial,
  valid F C state -> valid (EaExpr.constant_frame E d initial) empty state.
Hypothesis Resume : forall F C before after d initial,
  valid F C before -> CodeTransition before after ->
  valid (EaExpr.constant_frame E d initial) empty after -> valid F C after.
Hypothesis Install : forall F C before after symbol d n syntax initial code declared,
  valid F C before -> w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
  ConstantReading W P d syntax initial code declared ->
  put_const symbol (code, declared) before = Ok (tt, after) ->
  valid F C after.

Theorem read_constant_sound : forall F C symbol d n syntax initial,
  w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
  Checked post (valid F C) (fun _ => valid F C) (EaExpr.read_constant E R symbol d syntax initial)
    (fun '(code, declared) _ => ConstantReading W P d syntax initial code declared).
Proof.
  intros F C symbol d n syntax initial Declared Ast before [code declared] after Run.
  unfold EaExpr.read_constant in Run.
  apply mbind_ok in Run as (cached & checked & Get & Run).
  unfold get_const in Get. injection Get as <- <-.
  destruct (NMap.nget (es_const before) symbol) as [[cached_type cached_value]|] eqn:Found.
  - injection Run as <- <- <-. split; [tauto|].
    intros Valid _. split; [exact Valid|eapply Cache; eassumption].
  - apply mbind_ok in Run as ([] & checked & Assumes & Run).
    destruct (d_assumes d) as [|head assumptions] eqn:NoAssumes; [|discriminate Assumes].
    injection Assumes as <-.
    apply mbind_ok in Run as (meaning & started & Read & Run).
    unfold pure in Read.
    destruct (ty_of (e_W E) (e_fuel E) (d_mod d) [] None syntax) as [declared_type|message]
      eqn:ReadType; [|discriminate Read].
    injection Read as <- <-.
    apply mbind_ok in Run as ([body actual] & emitted & Body & Run).
    apply mbind_ok in Run as ([] & checked & Check & Run).
    destruct (fitsb actual declared_type) eqn:Fits; [|discriminate Check].
    injection Check as <-.
    apply mbind_ok in Run as ([] & installed & Write & Run).
    injection Run as <- <- <-.
    destruct (ReadExpr _ _ _ _ _ _ _ Body) as [Back Typed].
    assert (PutBack : post installed -> post emitted).
    { apply PostBack. eapply EaExpressionCodeEffects.code_effect_put_const. exact Write. }
    split; [now intros Post; apply Back, PutBack|].
    intros Valid Post. destruct (Typed (Enter _ _ _ _ _ Valid) (PutBack Post)) as [ValidBody BodyTyped].
    assert (Reading : ConstantReading W P d syntax initial body declared_type).
    { eapply constant_initializer_sound; [exact NoAssumes|exact ReadType| |exact Body|exact Fits].
      intros _. exact BodyTyped. }
    split; [|exact Reading].
    eapply Install; [|exact Declared|exact Ast|exact Reading|exact Write].
    eapply Resume; [exact Valid|eapply ExprEffect; exact Body|exact ValidBody].
Qed.
End ReadConstant.
