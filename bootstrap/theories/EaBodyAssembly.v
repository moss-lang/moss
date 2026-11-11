(* The actual two source-body passes establish typed bodies and constants.
   Generated code from each pass is read in the same final program. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import Audit EaBase EaExpr EaChecked EaRecSound EaControlSound EaCodeEffects
  EaCodeAllocationSound EaAssemblyCodeEffects EaBodyContext EaBodyState EaBodyCallbacks.
From NM Require EaConstantSound EaContextTableSound EaExpressionCodeEffects EaFunctionSound
  EaFunctionAssemblySound EaMain PublicWorldFormation WorldSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Section BodyPasses.
Variables (E : env) (P : program) (bound : nat) (final : est).
Let W := e_W E.
Let R := EaMain.R0 E.
Let valid := BodyValid W P bound.
Let state_valid := BodyState W P bound.
Let post := GeneratedPost bound final.
Hypothesis Contexts : EaContextTableSound.ContextTableSound W.
Hypothesis World : exists bundle, WorldOf bundle W.
Hypothesis Callbacks : RecSound E P valid post R.

Local Lemma block_effect : forall F C block expected, PreservesOldCode (r_blke R F C block expected).
Proof.
  unfold R, EaMain.R0.
  exact (proj1 (proj2 (EaExpressionCodeEffects.mkrec_code_effect _
    (EaExpressionCodeEffects.run_code_effect E (e_fuel E))))).
Qed.

Local Lemma declared_function_write : forall f code before after,
  set_fn_body f code before = Ok (tt, after) ->
  read_code after FunctionTable f = Some code.
Proof.
  intros f code before after Write. injection Write as <-.
  cbn [read_code es_fns]. apply WorldSound.nget_nset_same.
Qed.

Local Lemma plain_source_context : forall C ps ts,
  BodyContext W C ->
  BodyContext W (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
    (combine ps ts) C).
Proof.
  intros C ps ts Context. change (BodyContext W (add_binds C (combine ps ts))).
  now apply body_context_add_binds.
Qed.

Theorem typed_function_step : forall symbol before after,
  symbol < bound -> state_valid before -> post after ->
  EaMain.elaborate_function E symbol before = Ok (tt, after) ->
  state_valid after /\
  (body_of P symbol = read_code after FunctionTable symbol -> FnElab W P symbol).
Proof.
  intros symbol before after Below State Post Run.
  unfold EaMain.elaborate_function in Run.
  destruct (EaMain.defined_fn E symbol) eqn:Defined.
  2: {
    injection Run as <-. split; [exact State|].
    intros _ d rcv n ps rt body Declared Ast.
    unfold EaMain.defined_fn in Defined. fold W in Defined.
    rewrite Declared, Ast in Defined. discriminate Defined.
  }
  unfold EaMain.defined_fn in Defined. fold W in Defined.
  destruct (w_decl W symbol) as [d|] eqn:Declared; [|discriminate Defined].
  destruct (d_ast d) as [| | | | | |rcv name params result body| |] eqn:Ast;
    try discriminate Defined.
  destruct body as [body|]; [|discriminate Defined].
  unfold with_msg in Run.
  destruct (EaMain.elab_fn E symbol before) as [[ignored finished]|message] eqn:Computed; [|discriminate].
  injection Run as -> ->.
  destruct (EaFunctionSound.elab_fn_read _ _ _ _ _ _ _ _ _ _ Contexts Declared Ast Computed)
    as (C & needs & this & signature & F & code & t & start & emitted &
      Formal & Receiver & Signature & Module & Return & This & Loop & Fresh & Applying & Pins &
      Numbering & -> & Body & Write).
  pose proof (block_effect _ _ _ _ _ _ _ Body) as Change.
  destruct State as [Caches TypedLinks].
  assert (BeforeWrite : post emitted).
  {
    assert (Floor : bound <= es_next emitted).
    { pose proof (body_counter _ _ _ _ Caches). pose proof (counter_grows _ _ Change). lia. }
    eapply generated_post_compose; [|exact Post].
    eapply (source_action_establishes_post unit bound (set_fn_body symbol code));
      [exact (source_write_preserves_bound bound symbol code Below)|exact Floor| |exact Write].
    exact (code_bound_preserved _ _ Change (body_code_tables _ _ _ _ Caches)).
  }
  destruct (proj1 (proj2 Callbacks) _ _ _ _ _ _ _ Body) as [_ Checked].
  assert (Context : BodyContext W C).
  { destruct World as [bundle Loaded]. eapply function_body_context; [|exact Formal].
    exact (PublicWorldFormation.mentions_typed_of bundle W Loaded). }
  destruct (Checked (conj (plain_source_context _ _ _ Context)
    (conj Caches TypedLinks)) BeforeWrite) as [[_ NewState] Typed].
  split; [eapply source_write_keeps_state; eassumption|].
  intros Code d' rcv' name' params' result' body' Found Shape.
  rewrite Declared in Found. injection Found as <-.
  rewrite Ast in Shape. injection Shape as <- <- <- <- <-.
  rewrite (declared_function_write _ _ _ _ Write) in Code.
  exists C, needs, this, signature, F, code, t. repeat first [assumption | split].
Qed.

Local Lemma constant_name : forall symbol d name annotation initial,
  w_decl W symbol = Some d -> d_ast d = ADVal name annotation (Some initial) ->
  resolve_sym W (d_mod d) [name] = Some symbol.
Proof.
  intros symbol d name annotation initial Declared Ast.
  destruct World as [bundle LoadedWorld].
  destruct (@wo_decl_sound bundle W LoadedWorld _ _ Declared)
    as [[Loaded (module & Module & _)] _].
  assert (Scoped : w_scope W (d_mod d) NSName name = Some (ESym symbol)).
  { apply (proj2 (@wo_scope bundle W LoadedWorld _ _ _ _ Loaded)).
    apply ScPrimary, ScExported. eapply ExOwn; [exact Declared|reflexivity|].
    now rewrite Ast. }
  unfold resolve_sym, resolve. fold W. now rewrite Scoped.
Qed.

Theorem typed_constant_step : forall symbol before after,
  state_valid before -> post after ->
  EaMain.elaborate_constant E symbol before = Ok (tt, after) ->
  state_valid after /\
  (forall d n syntax initial, w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
    ConstElab W P d syntax initial).
Proof.
  intros symbol before after State Post Run.
  unfold EaMain.elaborate_constant in Run. fold W in Run.
  destruct (w_decl W symbol) as [d|] eqn:Declared.
  2: {
    injection Run as <-. split; [exact State|].
    intros d n syntax initial Found. discriminate Found.
  }
  destruct (d_ast d) as [| | | | |name annotation initial| | |] eqn:Ast;
    try solve [injection Run as <-; split; [exact State|];
      intros claimed claimed_name claimed_type claimed_value Found Shape;
      assert (d = claimed) by congruence; subst claimed; rewrite Ast in Shape; discriminate Shape].
  destruct initial as [initial|].
  2: {
    injection Run as <-. split; [exact State|].
    intros d' n syntax init Found Shape. injection Found as <-. congruence.
  }
  unfold with_msg in Run.
  lazymatch type of Run with context [mbind ?read ?rest before] =>
    destruct (mbind read rest before) as [[result finish]|message] eqn:Read
  end; [|discriminate].
  destruct result. injection Run as <-.
  apply mbind_ok in Read as ([code declared] & emitted & Body & Read).
  injection Read as <-.
  destruct (proj1 Callbacks _ _ _ _ _ _ _ Body) as [_ Checked].
  destruct (Checked (conj (empty_body_context W) State) Post) as [[_ NewState] Typed].
  split; [exact NewState|].
  intros d' n syntax init Found Shape.
  injection Found as <-.
  rewrite Ast in Shape. injection Shape as <- <- <-.
  lazymatch type of Typed with
  | Ex _ _ ?F ?C _ _ _ _ =>
      eapply (EaConstantSound.constant_expression_implies_initializer
        W P F C name None symbol d annotation initial code declared)
  end.
  - reflexivity.
  - eapply constant_name; eassumption.
  - exact Declared.
  - exact Ast.
  - exact Typed.
Qed.

Local Lemma function_step_bound : forall symbol,
  symbol < bound -> PreservesCodeBound bound (EaMain.elaborate_function E symbol).
Proof.
  intros symbol Within. unfold EaMain.elaborate_function. destruct (EaMain.defined_fn E symbol).
  - apply code_bound_with_msg, source_function_preserves_bound. exact Within.
  - apply code_effect_preserves_bound, code_effect_return.
Qed.

Theorem function_pass_typed : forall symbols before after,
  NoDup symbols -> (forall symbol, In symbol symbols -> symbol < bound) ->
  state_valid before -> post after ->
  miter (EaMain.elaborate_function E) symbols before = Ok (tt, after) ->
  state_valid after /\
  forall f, In f symbols -> body_of P f = read_code after FunctionTable f -> FnElab W P f.
Proof.
  induction symbols as [|symbol symbols IH]; intros before after Distinct Within State Post Run.
  - injection Run as <-. split; [exact State|intros f []].
  - inversion Distinct as [|? ? NotInTail TailDistinct]; subst.
    cbn [miter] in Run. apply mbind_ok in Run as ([] & middle & Head & Tail).
    assert (HeadWithin : symbol < bound) by (apply Within; now left).
    assert (TailWithin : forall k, In k symbols -> k < bound) by (intros; apply Within; now right).
    destruct (function_step_bound symbol HeadWithin _ _ _ (body_counter _ _ _ _ (proj1 State))
      (body_code_tables _ _ _ _ (proj1 State)) Head) as (Counter & Tables & _).
    assert (Floor : bound <= es_next middle).
    { pose proof (body_counter _ _ _ _ (proj1 State)). lia. }
    assert (MiddlePost : post middle).
    { eapply generated_post_compose; [|exact Post].
      eapply source_action_establishes_post; [apply function_pass_preserves_code_bound; exact TailWithin|
        exact Floor|exact Tables|exact Tail]. }
    destruct (typed_function_step _ _ _ HeadWithin State MiddlePost Head) as [Middle Typed].
    destruct (IH _ _ TailDistinct TailWithin Middle Post Tail) as [Finished TypedTail].
    split; [exact Finished|].
    intros f [<-|Member] Code; [|now apply TypedTail].
    apply Typed. rewrite Code.
    eapply (EaFunctionAssemblySound.function_pass_keeps_unchecked E bound Contexts);
      [exact TailWithin|exact NotInTail|exact HeadWithin|exact Floor|exact Tables|exact Tail].
Qed.

Theorem constant_pass_typed : forall symbols before after,
  state_valid before -> post after ->
  miter (EaMain.elaborate_constant E) symbols before = Ok (tt, after) ->
  state_valid after /\
  forall symbol, In symbol symbols ->
  forall d n syntax initial, w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
    ConstElab W P d syntax initial.
Proof.
  induction symbols as [|symbol symbols IH]; intros before after State Post Run.
  - injection Run as <-. split; [exact State|intros k []].
  - cbn [miter] in Run. apply mbind_ok in Run as ([] & middle & Head & Tail).
    pose proof (code_effect_miter _ (EaMain.elaborate_constant E) symbols (constant_code_effect E)
      _ _ _ Tail) as TailChange.
    assert (MiddlePost : post middle) by (eapply code_transition_preserves_post; eassumption).
    destruct (typed_constant_step _ _ _ State MiddlePost Head) as [Middle Typed].
    destruct (IH _ _ Middle Post Tail) as [Finished TypedTail].
    split; [exact Finished|]. intros k [<-|Member]; [exact Typed|now apply TypedTail].
Qed.

Theorem assembled_bodies_sound : forall entry,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  (forall f, f < bound -> FnElab W P f) /\
  (forall symbol d n syntax initial, symbol < bound -> w_decl W symbol = Some d ->
    d_ast d = ADVal n syntax (Some initial) -> ConstElab W P d syntax initial).
Proof.
  intros entry Assembled.
  pose proof (proj1 (assembled_code_lookup_exact _ _ _ _ _ Assembled)) as FinalCode.
  unfold EaMain.assemble in Assembled.
  apply mbind_ok in Assembled as ([] & after_functions & Functions & Rest).
  apply mbind_ok in Rest as ([] & after_constants & Constants & Finish).
  assert (Within : forall k, In k (seq 0 bound) -> k < bound) by
    (intros k Member; apply in_seq in Member; lia).
  pose proof (finish_assembly_code_effect E bound entry _ _ _ Finish) as FinishChange.
  pose proof (code_effect_miter _ (EaMain.elaborate_constant E) (seq 0 bound)
    (constant_code_effect E) _ _ _ Constants) as ConstantChange.
  assert (ConstantPost : post after_constants).
  { eapply code_transition_preserves_post; [exact FinishChange|apply generated_post_refl]. }
  assert (FunctionPost : post after_functions) by (eapply code_transition_preserves_post; eassumption).
  destruct (function_pass_typed _ _ _ (seq_NoDup _ _) Within (initial_body_state W P bound)
    FunctionPost Functions) as [AfterFunctions FunctionsTyped].
  destruct (constant_pass_typed _ _ _ AfterFunctions ConstantPost Constants) as [_ ConstantsTyped].
  split.
  - intros f Below. apply FunctionsTyped; [apply in_seq; lia|].
    rewrite FinalCode.
    assert (Older : f < es_next after_functions).
    { pose proof (body_counter _ _ _ _ (proj1 AfterFunctions)). lia. }
    rewrite (old_slots_preserved _ _ FinishChange) by (pose proof (counter_grows _ _ ConstantChange); lia).
    now rewrite (old_slots_preserved _ _ ConstantChange) by exact Older.
  - intros symbol d n syntax initial Below Declared Ast.
    assert (Member : In symbol (seq 0 bound)) by (apply in_seq; lia).
    exact (ConstantsTyped symbol Member d n syntax initial Declared Ast).
Qed.
End BodyPasses.

From NM Require BootstrapSound EaBudget EaCtx EaFamilyProducerSound EaMetadataSound EaWorldSound ElabWorld NMap.

Theorem elaborate_bodies_sound : BootstrapSound.BodiesSound.
Proof.
  intros bundle W program Elaborated.
  pose proof (EaWorldSound.elaborate_with_world_sound _ _ _ _ Elaborated) as World.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Elaborated) as Contexts.
  unfold EaMain.elaborate, EaMain.elaborate_with, EaMain.elaborate_with_metadata in Elaborated.
  destruct (EaMain.bundle_world_r bundle) as [[W0 bound]|message] eqn:Loaded; [|discriminate].
  cbn zeta in Elaborated.
  set (metadata := EaMain.metadata_fuel W0 bound) in *.
  set (modules := map am_path (b_mods bundle)) in *.
  set (mains := EaMain.main_symbols W0 modules) in *.
  set (initial := Env W0 default_strategy modules mains EaMain.elab_fuel metadata) in *.
  set (installed := ElabWorld.with_context W0 (NMap.nget (EaCtx.context_table initial bound))) in *.
  set (E := Env installed default_strategy modules mains EaMain.elab_fuel metadata) in *.
  destruct (EaMain.assumes_complete E bound) as [[]|message] eqn:Assumes; [|discriminate].
  destruct (EaMain.decls_form E bound) as [[]|message] eqn:Decls; [|discriminate].
  destruct (EaMain.assemble E bound (b_entry bundle) (init_state bound)) as [[P final]|message]
    eqn:Assembled; [|discriminate].
  injection Elaborated as <- <-.
  assert (Bounded : forall n d, w_decl (e_W E) n = Some d -> n < bound).
  { exact (EaWorldSound.bundle_world_r_bound _ _ _ Loaded). }
  assert (Budget : EaBudget.metadata_budget (e_W E) bound < e_metadata_fuel E).
  { exact (EaMetadataSound.metadata_fuel_sufficient W0 bound). }
  assert (WorldExists : exists input, WorldOf input (e_W E)).
  { now exists bundle. }
  pose proof (PublicWorldFormation.mentions_typed_of bundle (e_W E) World) as Mentions.
  pose proof (EaFamilyProducerSound.checked_family_use E bound Bounded Budget Decls Mentions Contexts) as Families.
  pose proof (checked_body_callbacks E bound (b_entry bundle) P final Bounded Budget WorldExists
    Decls Contexts Assembled (e_fuel E) Families
    (fun C receiver method u =>
      EaFamilyProducerSound.family_produces_covers E bound Bounded Budget Decls Mentions C u receiver method))
    as Checked.
  destruct (assembled_bodies_sound E P bound final Contexts WorldExists Checked
    (b_entry bundle) Assembled) as [Functions Constants].
  split.
  - intros f d rcv n ps rt body Declared Ast.
    exact (Functions f (Bounded f d Declared)).
  - intros f d n syntax initializer Declared Ast.
    exact (Constants f d n syntax initializer (Bounded f d Declared) Declared Ast).
Qed.
