(* The function pass writes each declared body once. Later stages preserve
   it, including when they emit more generated code. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase EaCodeAllocationSound EaCodeEffects
  EaAssemblyCodeEffects EaControlSound EaRecSound.
From NM Require EaMain EaFunctionSound EaContextTableSound EaExpressionCodeEffects WorldSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Section FunctionPass.
Variable E : env.
Variable bound : nat.
Hypothesis Contexts : EaContextTableSound.ContextTableSound (e_W E).

Local Lemma declared_slot_write : forall f x before after,
  set_fn_body f x before = Ok (tt, after) ->
  read_code after FunctionTable f = Some x.
Proof.
  intros f x before after Write. injection Write as <-.
  cbn [read_code es_fns]. apply WorldSound.nget_nset_same.
Qed.

Local Lemma function_action_run : forall f d rcv n ps rt body before after,
  w_decl (e_W E) f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
  EaMain.elaborate_function E f before = Ok (tt, after) ->
  EaMain.elab_fn E f before = Ok (tt, after).
Proof.
  intros f d rcv n ps rt body before after Declared Ast Run.
  unfold EaMain.elaborate_function, EaMain.defined_fn in Run. rewrite Declared, Ast in Run.
  unfold with_msg in Run.
  destruct (EaMain.elab_fn E f before) as [[value state]|message]; [exact Run|discriminate].
Qed.

Local Lemma function_step_keeps_other : forall f target before after,
  f <> target -> target < es_next before ->
  EaMain.elaborate_function E f before = Ok (tt, after) ->
  read_code after FunctionTable target = read_code before FunctionTable target.
Proof.
  intros f target before after Different Earlier Run.
  unfold EaMain.elaborate_function, EaMain.defined_fn in Run.
  destruct (w_decl (e_W E) f) as [d|] eqn:Declared; [|now injection Run as <-].
  destruct (d_ast d) as [| | | | | |rcv n ps rt body| |] eqn:Ast; try (now injection Run as <-).
  destruct body as [body|]; [|now injection Run as <-].
  unfold with_msg in Run.
  destruct (EaMain.elab_fn E f before) as [[value state]|message] eqn:Checked; [|discriminate].
  inversion Run; subst value state.
  destruct (EaFunctionSound.elab_fn_read _ _ _ _ _ _ _ _ _ _ Contexts Declared Ast Checked)
    as (C & needs & this & signature & F & code & ty & start & emitted &
      _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & -> & Body & Write).
  destruct (EaExpressionCodeEffects.mkrec_code_effect _
    (EaExpressionCodeEffects.run_code_effect E (e_fuel E))) as (_ & Block & _).
  pose proof (Block _ _ _ _ _ _ _ Body) as Effect.
  injection Write as <-. cbn [read_code es_fns].
  rewrite WorldSound.nget_nset_other by congruence.
  exact (old_slots_preserved _ _ Effect FunctionTable target Earlier).
Qed.

Local Lemma function_pass_keeps_unchecked : forall symbols target before after,
  (forall f, In f symbols -> f < bound) ->
  ~ In target symbols -> target < bound -> bound <= es_next before -> CodeTablesBound before ->
  miter (EaMain.elaborate_function E) symbols before = Ok (tt, after) ->
  read_code after FunctionTable target = read_code before FunctionTable target.
Proof.
  induction symbols as [|f symbols IH]; intros target before after Within NotWritten Source Counter Bound Run.
  - now injection Run as <-.
  - cbn [miter] in Run. apply mbind_ok in Run as ([] & middle & Head & Tail).
    assert (Bounds : PreservesCodeBound bound (EaMain.elaborate_function E f)).
    { unfold EaMain.elaborate_function. destruct (EaMain.defined_fn E f).
      - apply code_bound_with_msg, source_function_preserves_bound, Within. now left.
      - apply code_effect_preserves_bound, code_effect_return. }
    destruct (Bounds _ _ _ Counter Bound Head) as (Middle & BoundMiddle & _).
    rewrite (IH target middle after); try exact Tail; try lia; try exact BoundMiddle.
    + apply (function_step_keeps_other f target before middle); [intro Equal; apply NotWritten; now left|lia|exact Head].
    + intros k Present. apply Within. now right.
    + intro Present. apply NotWritten. now right.
Qed.

Theorem function_pass_selects : forall symbols f before after,
  NoDup symbols -> (forall k, In k symbols -> k < bound) ->
  In f symbols -> bound <= es_next before -> CodeTablesBound before -> LinkRecords before ->
  LambdaOriginsBound before ->
  miter (EaMain.elaborate_function E) symbols before = Ok (tt, after) ->
  exists started finished remaining,
    EaMain.elaborate_function E f started = Ok (tt, finished) /\
    bound <= es_next started /\ CodeTablesBound started /\ LinkRecords started /\
    LambdaOriginsBound started /\
    read_code after FunctionTable f = read_code finished FunctionTable f /\
    (forall k, In k remaining -> k < bound) /\
    miter (EaMain.elaborate_function E) remaining finished = Ok (tt, after).
Proof.
  induction symbols as [|k symbols IH]; intros f before after Distinct Within Present Counter Bound Links Lambda Run;
    [contradiction|].
  inversion Distinct as [|? ? NotTail TailDistinct]; subst.
  cbn [miter] in Run. apply mbind_ok in Run as ([] & middle & Head & Tail).
  assert (Bounds : PreservesCodeBound bound (EaMain.elaborate_function E k)).
  { unfold EaMain.elaborate_function. destruct (EaMain.defined_fn E k).
    - apply code_bound_with_msg, source_function_preserves_bound, Within. now left.
    - apply code_effect_preserves_bound, code_effect_return. }
  destruct (Bounds _ _ _ Counter Bound Head) as (Middle & BoundMiddle & _ & _ & LinksMiddle & LambdaMiddle).
  assert (TailWithin : forall x, In x symbols -> x < bound).
  { intros x Member. apply Within. now right. }
  destruct Present as [<-|Present].
  - exists before, middle, symbols. split; [exact Head|].
    split; [exact Counter|split; [exact Bound|split; [exact Links|split; [exact Lambda|]]]].
    split; [|exact (conj TailWithin Tail)].
    eapply function_pass_keeps_unchecked; try eassumption; [apply Within; now left|lia].
  - eapply (IH f middle after); [exact TailDistinct|exact TailWithin|exact Present|lia|
      exact BoundMiddle|now apply LinksMiddle|now apply LambdaMiddle|exact Tail].
Qed.

Theorem assemble_functions_sound : forall entry P final,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  (forall F C block expected code t started emitted,
    bound <= es_next started -> CodeTablesBound started -> LinkRecords started ->
    LambdaOriginsBound started ->
    r_blke (EaMain.R0 E) F C block expected started = Ok ((code, t), emitted) ->
    GeneratedPost bound final emitted ->
    BlkE (e_W E) P F C block expected code t) ->
  forall f, f < bound -> FnElab (e_W E) P f.
Proof.
  intros entry P final Assembled Blocks f DeclaredBelow d rcv n ps rt body Declared Ast.
  pose proof (assembled_code_lookup_exact _ _ _ _ _ Assembled) as [FinalCode _].
  unfold EaMain.assemble in Assembled.
  apply mbind_ok in Assembled as ([] & after_functions & Functions & Assembled).
  apply mbind_ok in Assembled as ([] & after_constants & Constants & Finish).
  assert (Within : forall k, In k (seq 0 bound) -> k < bound) by
    (intros k Present; apply in_seq in Present; lia).
  assert (Present : In f (seq 0 bound)) by (apply in_seq; lia).
  destruct (function_pass_selects (seq 0 bound) f (init_state bound) after_functions (seq_NoDup _ _) Within Present (Nat.le_refl bound)
    (code_tables_initial bound) (initial_link_records bound) (initial_lambda_origins_bound bound) Functions)
    as (started & finished & remaining & Checked & Counter & Bound & Links & Lambda & SourceCode & TailWithin & Tail).
  pose proof (function_action_run _ _ _ _ _ _ _ _ _ Declared Ast Checked) as Function.
  destruct (EaFunctionSound.elab_fn_read _ _ _ _ _ _ _ _ _ _ Contexts Declared Ast Function)
    as (C & ns & this & g & F & code & t & start & emitted &
      Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins &
      Numbering & -> & Body & Write).
  assert (Written : read_code finished FunctionTable f = Some code) by
    (eapply declared_slot_write; exact Write).
  destruct (EaExpressionCodeEffects.mkrec_code_effect _
    (EaExpressionCodeEffects.run_code_effect E (e_fuel E))) as (_ & BlockEffect & _).
  destruct (BlockEffect _ _ _ _ _ _ _ Body) as [BodyCounter _ BodyBound _ _ _].
  specialize (BodyBound Bound).
  destruct (source_write_preserves_bound bound f code DeclaredBelow emitted tt finished
    ltac:(lia) BodyBound Write) as (WriteCounter & WriteBound & WriteCode & WriteOrigins & _).
  destruct (function_pass_preserves_code_bound E bound remaining TailWithin finished tt after_functions
    ltac:(lia) WriteBound Tail) as (TailCounter & _ & TailCode & TailOrigins & _).
  pose proof (code_effect_miter _ (EaMain.elaborate_constant E) (seq 0 bound) (constant_code_effect E))
    as ConstantEffects.
  destruct (ConstantEffects _ _ _ Constants) as [ConstantCounter ConstantCode _ ConstantOrigins _ _].
  destruct (finish_assembly_code_effect E bound entry _ _ _ Finish)
    as [FinalCounter FinishCode _ FinishOrigins _ _].
  assert (Retained : GeneratedPost bound final emitted).
  {
    constructor; [lia| |].
    - intros table id data [Generated Older] Read.
      rewrite FinishCode, ConstantCode, TailCode, WriteCode; try exact Read; lia.
    - intros table id origin [Generated Older] Read.
      rewrite FinishOrigins, ConstantOrigins, TailOrigins, WriteOrigins; try exact Read; lia.
  }
  assert (Code : body_of P f = Some code).
  { rewrite FinalCode, FinishCode, ConstantCode, SourceCode, Written; try reflexivity; lia. }
  pose proof (Blocks _ _ _ _ _ _ _ _ Counter Bound Links Lambda Body Retained) as Typed.
  exists C, ns, this, g, F, code, t.
  repeat first [assumption | split].
Qed.
End FunctionPass.
