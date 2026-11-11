(* Function and constant typing choose source body plans. Translation
   supplies the numbering and relates a function plan to its emitted slot. *)
From Stdlib Require Import List.
From NM Require Import Audit BodyTyping.
From NM Require EaFunctionSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Record function_plan := FunctionPlan {
  function_context : cx;
  function_needs : list item;
  function_receiver : option ty;
  function_signature : fsig;
  function_layout : frame;
  function_body : block_plan;
  function_result : ty
}.

Definition function_parameters (parameters : list name) (plan : function_plan) : cx :=
  fold_left (fun C q => add_local C (fst q) (snd q) LLet)
    (combine parameters (sg_params (function_signature plan))) (function_context plan).

Definition TypesFunction (W : world) (f : nat) (d : decl) (receiver : fnrecv) (n : name)
    (parameters : list name) (body : ablock) (plan : function_plan) : Prop :=
  let F := function_layout plan in
  let C := function_context plan in
  let g := function_signature plan in
  CtxOf W f C (function_needs plan) /\
  EaFunctionSound.FunctionReceiver W f receiver n C (function_receiver plan) /\
  SigOf W f (c_ty C) (function_receiver plan) g /\
  fr_mod F = d_mod d /\ fr_ret F = Some (sg_ret g) /\ fr_this F = function_receiver plan /\
  fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
  TypesBlkE W F (function_parameters parameters plan) body (Some (sg_ret g))
    (function_body plan) (function_result plan).

Definition emit_function (parameters : list name) (plan : function_plan) : expr :=
  translate_block (without_pins (function_layout plan)) (function_parameters parameters plan)
    (function_body plan).

Definition TranslatesFunction (P : program) (f : nat)
    (parameters : list name) (plan : function_plan) : Prop :=
  Numbering (function_layout plan) (function_receiver plan) parameters /\
  body_of P f = Some (emit_function parameters plan).

Record constant_plan := ConstantPlan {
  constant_layout : frame;
  constant_body : expression_plan;
  constant_declared : ty;
  constant_actual : ty
}.

Definition TypesConstant (W : world) (d : decl) (annotation : aty) (initial : aexpr)
    (plan : constant_plan) : Prop :=
  let F := constant_layout plan in
  let C := Cx [] [] [] [] [] in
  d_assumes d = [] /\ TyOf W (d_mod d) [] None annotation (constant_declared plan) /\
  TypesExpr W F C initial (Some (constant_declared plan)) (constant_body plan) (constant_actual plan) /\
  fits (constant_actual plan) (constant_declared plan) /\
  fr_mod F = d_mod d /\ fr_ret F = None /\ fr_this F = None /\
  fr_loop F = false /\ fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [].

Definition emit_constant (plan : constant_plan) : expr :=
  translate_expr (constant_layout plan) (Cx [] [] [] [] []) (constant_body plan).

Definition TranslatesConstant (plan : constant_plan) : Prop :=
  Numbering (constant_layout plan) None [].

Theorem function_translation_agrees : forall W P f,
  (forall d receiver n parameters annotation body,
    w_decl W f = Some d -> d_ast d = ADFn receiver n parameters annotation (Some body) ->
    exists plan, TypesFunction W f d receiver n (map fst parameters) body plan /\
      TranslatesFunction P f (map fst parameters) plan) ->
  FnElab W P f.
Proof.
  intros W P f Plans d receiver n parameters annotation body Declared Ast.
  destruct (Plans _ _ _ _ _ _ Declared Ast) as [[C needs this signature F code result] [Typed Emitted]].
  cbn [TypesFunction TranslatesFunction function_context function_needs function_receiver function_signature
    function_layout function_body function_result function_parameters emit_function] in *.
  destruct Typed as (Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins & Body).
  destruct Emitted as [Numbered Emitted].
  exists C, needs, this, signature, F, (translate_block (without_pins F)
    (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
      (combine (map fst parameters) (sg_params signature)) C) code), result.
  pose proof (proj1 (proj2 (body_translation_agrees W P)) _ _ _ _ _ _ Body) as TypedBody.
  repeat first [assumption|split].
Qed.

Theorem constant_translation_agrees : forall W P d annotation initial plan,
  TypesConstant W d annotation initial plan -> TranslatesConstant plan ->
  ConstElab W P d annotation initial.
Proof.
  intros W P d annotation initial [F code declared actual] Typed Numbered.
  cbn [TypesConstant TranslatesConstant constant_layout constant_body constant_declared constant_actual] in *.
  destruct Typed as (Assumes & TypeRead & Body & Fits & Module & Return & This & Loop & Fresh & Applying & Pins).
  split; [exact Assumes|].
  exists declared. split; [exact TypeRead|].
  exists F, (translate_expr F (Cx [] [] [] [] []) code), actual.
  pose proof (proj1 (body_translation_agrees W P) _ _ _ _ _ _ Body) as TypedBody.
  repeat first [assumption|split].
Qed.

Theorem function_plan_readback : forall W P,
  (forall F C body expected code t,
    BlkE W P F C body expected code t ->
    exists plan, TypesBlkE W F C body expected plan t /\ translate_block (without_pins F) C plan = code) ->
  forall f d receiver n parameters annotation body,
  FnElab W P f -> w_decl W f = Some d -> d_ast d = ADFn receiver n parameters annotation (Some body) ->
  exists plan, TypesFunction W f d receiver n (map fst parameters) body plan /\
    TranslatesFunction P f (map fst parameters) plan.
Proof.
  intros W P Readback f d receiver n parameters annotation body Typed Declared Ast.
  destruct (Typed _ _ _ _ _ _ Declared Ast) as (C & needs & this & g & F & code & t &
    Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins & Numbered & Body & Emitted).
  destruct (Readback _ _ _ _ _ _ Body) as (plan & Checked & Emit).
  exists (FunctionPlan C needs this g F plan t). split.
  - unfold TypesFunction, function_parameters. cbn. repeat first [assumption|split].
  - unfold TranslatesFunction, emit_function, function_parameters. cbn. now rewrite Emit.
Qed.

Theorem constant_plan_readback : forall W P,
  (forall F C syntax expected code t,
    Ex W P F C syntax expected code t ->
    exists plan, TypesExpr W F C syntax expected plan t /\ translate_expr F C plan = code) ->
  forall d annotation initial,
  ConstElab W P d annotation initial ->
  exists plan, TypesConstant W d annotation initial plan /\ TranslatesConstant plan.
Proof.
  intros W P Readback d annotation initial
    (Assumes & declared & TypeRead & F & code & actual & Body & Fits & Module & Return & This & Loop &
      Fresh & Applying & Pins & Numbered).
  destruct (Readback _ _ _ _ _ _ Body) as (plan & Checked & _).
  exists (ConstantPlan F plan declared actual). unfold TypesConstant, TranslatesConstant.
  cbn. repeat first [assumption|split].
Qed.
