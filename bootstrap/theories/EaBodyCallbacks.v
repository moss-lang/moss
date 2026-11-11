(* Instantiate the recursive body checker with the actual context and cache
   invariants and the code of the assembled program. *)
From Stdlib Require Import List String Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaExpr EaChecked EaControlSound EaCodeEffects EaRecSound
  EaBodyContext EaBodyState EaLinkBodySound.
From NM Require Import LegacyBodyJudgments.
From NM Require EaBind EaBindCodeEffects EaBindItemSound EaBlockSound EaBudget EaCacheEffects EaClosureSound EaConstantSound
  EaContextTableSound EaDefinedCallSound EaDependencyFuel EaExpressionCodeEffects EaExpressionSound
  EaFamily EaFamilyComplete EaFamilyUseSound EaTypes
  EaForwarderSound EaFunctor EaLambdaSound EaMain EaResultBudget EaValuePathSound EaWrappedProviderSound PublicContextFacts PublicFormation PublicWorldFormation.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Definition BodyValid (W : world) (P : program) (bound : nat) (_ : frame) (C : cx) (state : est) : Prop :=
  BodyContext W C /\ BodyState W P bound state.

Lemma valid_body_locals : forall W P bound F C name ty mode state,
  BodyValid W P bound F (add_local C name ty mode) state <->
  BodyValid W P bound F C state.
Proof. intros. unfold BodyValid. now rewrite body_context_add_local. Qed.

Lemma valid_body_binds : forall W P bound F C bindings state,
  BodyValid W P bound F (add_binds C bindings) state <->
  BodyValid W P bound F C state.
Proof. intros. unfold BodyValid. now rewrite body_context_add_binds. Qed.

Section ConcreteCallbacks.
Variables (E : env) (bound : nat) (entry : string) (P : program) (final : est).
Let W := e_W E.
Let valid := BodyValid W P bound.
Let post := GeneratedPost bound final.
Hypotheses
  (Bounded : forall n d, w_decl W n = Some d -> n < bound)
  (Budget : EaBudget.metadata_budget W bound < e_metadata_fuel E)
  (World : exists bundle, WorldOf bundle W)
  (Decls : EaMain.decls_form E bound = Ok tt)
  (Contexts : EaContextTableSound.ContextTableSound W)
  (Assembly : EaMain.assemble E bound entry (init_state bound) = Ok (P, final)).

Local Lemma world_mentions : PublicFormation.MentionsTyped W.
Proof. destruct World as [bundle Loaded]. exact (PublicWorldFormation.mentions_typed_of bundle W Loaded). Qed.

Local Lemma source_fresh : forall id, bound <= id -> Fresh W id.
Proof.
  intros id After. unfold Fresh. destruct (w_decl W id) as [d|] eqn:Found; [|reflexivity].
  specialize (Bounded id d Found). lia.
Qed.

Local Lemma caller_section : forall F C state, valid F C state -> PublicContextFacts.CxSection C.
Proof. intros F C state [Context _]. exact (proj1 Context). Qed.

Local Lemma counter_valid : forall F C state, valid F C state -> bound <= es_next state.
Proof. intros F C state [_ [Caches _]]. exact (body_counter _ _ _ _ Caches). Qed.

Local Lemma post_earlier : forall before after, CodeTransition before after -> post after -> post before.
Proof. intros before after Change. apply code_transition_preserves_post. exact Change. Qed.

Theorem body_function_provider : forall kind F C path signature,
  Checked post (valid F C) (fun _ => valid F C) (EaBind.fn_provider E kind F C path signature)
    (fun '(g, types, sm) _ => FnProvider W P F C (AEPath path None) signature None g types sm).
Proof.
  apply (EaForwarderSound.function_provider_post_sound E P bound entry final valid
    Assembly source_fresh Contexts caller_section).
  - intros F C state [_ [Caches _]]. exact (body_forwarders _ _ _ _ Caches).
  - exact counter_valid.
  - intros F C before after [Context State] Change Cache.
    split; [exact Context|eapply place_forwarder_keeps_state; eassumption].
Qed.

Theorem body_expression_step : forall R,
  RecSound E P valid post R ->
  (forall F C syntax expected, PreservesOldCode (r_ex R F C syntax expected)) ->
  EaFamilyUseSound.FamilyUseSound E ->
  forall n,
  (forall depth, depth < n -> ExPostAt E P valid post R depth) ->
  (forall depth, depth < n -> BlkEPostAt E P valid post R depth) ->
  ExPostAt E P valid post R n.
Proof.
  intros R Callbacks Effects Families.
  apply (EaExpressionSound.expression_step_sound E R P valid post Callbacks caller_section
    (valid_body_binds W P bound) Contexts (EaFamilyUseSound.demand_sound_from_family_reads E Families)
    Families World).
  intros F C symbol d n syntax initial.
  apply (EaConstantSound.read_constant_sound E R P valid post post_earlier
    (proj1 Callbacks) Effects).
  - intros F' C' state [_ [Caches _]]. exact (body_constants _ _ _ _ Caches).
  - intros F' C' state d' init [_ State]. exact (conj (empty_body_context W) State).
  - intros F' C' before after d' init [Context _] _ [_ State]. exact (conj Context State).
  - intros F' C' before after symbol' d' n' syntax' init code declared
      [Context State] Found Ast Read Write.
    split; [exact Context|eapply write_constant_keeps_state; eassumption].
Qed.

Theorem body_lambda_provider : forall R,
  RecSound E P valid post R ->
  (forall F C block expected, PreservesOldCode (r_blke R F C block expected)) ->
  forall what F C params annotation body signature this,
  Checked post (valid F C) (fun _ => valid F C)
    (EaBind.lambda_provider E R what F C params annotation body signature this)
    (fun '(target, prefix, sm) _ =>
      LambdaProvider W P F C params annotation body signature this target prefix sm).
Proof.
  intros R Callbacks Effects what F C params annotation body signature this
    before [[target prefix] sm] after Run.
  pose proof (lambda_provider_code_effect E R what F C params annotation body signature this Effects
    _ _ _ Run) as Change.
  split; [exact (post_earlier _ _ Change)|].
  intros [Context [Caches TypedLinks]] Post.
  destruct (EaLambdaSound.lambda_provider_sound E R bound entry P final (BodyCaches W P bound)
    Bounded Assembly Effects what F C params annotation body signature this before target prefix sm after
    (proj1 Context) (proj1 (proj2 Context)) (body_counter _ _ _ _ Caches)
    (body_code_tables _ _ _ _ Caches) (body_lambda_origins _ _ _ _ Caches)
    Caches TypedLinks Post) as (Typed & Links & Cache).
  - intros count values captured slot start Capture Reserve State.
    eapply lambda_reservation_keeps_caches; eassumption.
  - intros slot code emitted written Issued Vacant Write Note State.
    eapply fill_lambda_keeps_caches; eassumption.
  - intros caps values captured slot start code t finish _ _ _ State Links Post' Fl Cl Body.
    assert (Nested : BodyContext W Cl).
    { apply lambda_parameter_body_context, captured_body_context. exact Context. }
    destruct (proj1 (proj2 Callbacks) Fl Cl _ _ _ _ _ Body) as [_ CheckedBody].
    destruct (CheckedBody (conj Nested (conj State Links)) Post') as [[_ [NewCache NewLinks]] BodyTyped].
    now split.
  - exact Run.
  - exact (conj (conj Context (conj Cache Links)) Typed).
Qed.

Theorem body_method_provider : forall R,
  RecSound E P valid post R ->
  (forall F C block expected, PreservesOldCode (r_blke R F C block expected)) ->
  forall method F C syntax signature receiver,
  Checked post (valid F C) (fun _ => valid F C)
    (EaBind.meth_provider E R method F C syntax signature receiver)
    (fun '(target, types, prefix, sm) _ =>
      MethProvider W P F C syntax signature receiver target types prefix sm).
Proof.
  intros R Callbacks Effects. destruct World as [bundle Loaded].
  refine (EaWrappedProviderSound.method_provider_post_sound E P bound entry final valid
    Assembly source_fresh Contexts _ counter_valid _ bundle R Loaded caller_section
    (body_function_provider "method") _).
  - intros F C state [_ [Caches _]]. exact (body_wrappers _ _ _ _ Caches).
  - intros F C before after [Context State] Change Cache.
    split; [exact Context|eapply place_wrapper_keeps_state; eassumption].
  - intros. apply (body_lambda_provider R Callbacks Effects).
Qed.

Local Lemma linked_code_from_post : forall state, post state -> LinkedCodeInProgram P bound state.
Proof.
  intros state Post. split.
  - intros h code Range Read. exact (generated_code_in_assembly E bound entry P final state
      Assembly Post FunctorTable h code Range Read).
  - intros h target pins Range Read.
    exact (proj1 (generated_link_origin_in_assembly E bound entry P final state h target pins
      Assembly Post Range Read)).
Qed.

Theorem body_closure : forall R,
  RecSound E P valid post R ->
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  forall F C path target provider_path provider,
  resolve_sym W (fr_mod F) path = Some target ->
  resolve_sym W (fr_mod F) provider_path = Some provider ->
  kind_sym W provider = Some DKFunctor ->
  Checked post (valid F C)
    (fun '(wrap, C') => bind_scope_valid valid F C (wrap, C', F, []))
    (EaFunctor.fun_closure E R F C target provider)
    (fun '(wrap, C') _ => BindItem W P F C
      (Spec (Some path) None None, Some (AEPath provider_path None)) wrap C' F []).
Proof.
  intros R Callbacks Effects F C path target provider_path provider Resolved ResolveProvider Kind
    before [wrap result] after Run.
  pose proof (EaBindCodeEffects.fun_closure_code_effect E R Effects _ _ _ _ _ _ _ Run) as Change.
  split; [exact (post_earlier _ _ Change)|].
  intros [Context [Caches TypedLinks]] Post.
  destruct (EaClosureSound.fun_closure_sound E R P bound post (BodyCaches W P bound)
    Bounded Budget Decls Contexts F C path target provider_path provider before wrap result after
    Resolved ResolveProvider Kind (body_counter _ _ _ _ Caches) Post)
    as (Typed & LinkCache & Cache).
  - intros slot finish TargetKind Kept Link.
    refine (link_body_sound E P R bound post Bounded Budget Contexts
      (EaFamilyComplete.decls_form_family_classifier E bound Bounded Budget Decls)
      linked_code_from_post post_earlier Effects (BodyCaches W P bound) _ _
      target provider before slot finish _ _ _ _ _ _ _ _).
    + intros u Cu Cb label start value end_state State Roles.
      destruct (EaCacheEffects.cache_effect_output_roles _ _ _ _ _ _ _ _ Roles)
        as [Fwd Wrap Const _ _].
      eapply (carry_caches_under_transition W P bound start end_state);
        [eapply EaBindCodeEffects.link_output_roles_code_effect; exact Roles|exact State|
          exact Fwd|exact Wrap|exact Const].
    + intros. eapply linked_emit_keeps_caches; eassumption.
    + exact TargetKind.
    + exact (body_counter _ _ _ _ Caches).
    + exact (body_link_records _ _ _ _ Caches).
    + exact TypedLinks.
    + exact Caches.
    + exact Kept.
    + intros d bodies Cg needs prefix Cb Fb made finish' Formed Post' Fg Nested.
      destruct (proj2 (proj2 Callbacks) Fg Cg (List.concat bodies) _ _ _ Nested) as [_ CheckedBody].
      destruct (CheckedBody (conj (functor_body_context _ _ _ _ world_mentions Formed) (conj Caches TypedLinks)) Post')
        as [[[_ [NextCache NextLinks]] _] Body].
      now split.
    + exact Link.
  - exact Run.
  - assert (ContextResult : BodyContext W result).
    { eapply EaBindItemSound.body_context_path_bind; eassumption. }
    split; [|exact Typed].
    split; [exact (conj ContextResult (conj Cache LinkCache))|].
    intros state [_ State]. exact (conj Context State).
Qed.
Theorem checked_body_callbacks : forall fuel,
  EaFamilyUseSound.FamilyUseSound E ->
  (forall C receiver method u, PublicContextFacts.CxSection C ->
    FamilyProduces W C u (mkey receiver method) ->
    EaFamily.family_produces E C u (mkey receiver method) = true) ->
  RecSound E P valid post (EaMain.mkrec (EaMain.run E fuel)).
Proof.
  intros fuel Families Detected. apply run_elaboration_at_fuel.
  - intros inner R Callbacks n Expressions Blocks.
    destruct (EaExpressionCodeEffects.mkrec_code_effect _ (EaExpressionCodeEffects.run_code_effect E inner))
      as [ExprEffect _].
    split.
    + exact (body_expression_step R Callbacks ExprEffect Families n Expressions Blocks).
    + apply (EaBlockSound.blke_post_sound E R P valid post (valid_body_locals W P bound)
        ltac:(reflexivity) ltac:(reflexivity) (proj2 (proj2 Callbacks))); assumption.
  - intros inner R Callbacks.
    destruct (EaExpressionCodeEffects.mkrec_code_effect _ (EaExpressionCodeEffects.run_code_effect E inner))
      as (_ & BlockEffect & BindEffect).
    apply (EaBindItemSound.bind_item_post_sound E R P bound final (BodyState W P bound)
      Bounded Budget Contexts Decls).
    + intros state [Cache _]. exact (body_counter _ _ _ _ Cache).
    + intros F C u before result after State Run.
      eapply (carry_body_state W P bound before after);
        [eapply apply_abstract_code_effect|eapply EaCacheEffects.cache_effect_apply_abstract|exact State];
        exact Run.
    + intros F C u h before result after State Run.
      eapply (carry_body_state W P bound before after);
        [eapply EaBindCodeEffects.fun_copy_code_effect|eapply EaCacheEffects.cache_effect_copy|exact State];
        exact Run.
    + intros symbol meanings. apply EaDependencyFuel.app_of_sound.
      change (EaDependencyFuel.signature_depth W symbol <= e_metadata_fuel E).
      eapply Nat.le_trans; [exact (EaResultBudget.signatures_depth_bounded W bound Bounded symbol)|].
      apply Nat.le_trans with (m := EaBudget.metadata_budget W bound).
      * unfold EaBudget.metadata_budget. apply Nat.le_add_r.
      * exact (Nat.lt_le_incl _ _ Budget).
    + exact Callbacks.
    + exact (body_function_provider "fn").
    + intros. apply (body_lambda_provider R Callbacks BlockEffect).
    + exact (body_method_provider R Callbacks BlockEffect).
    + exact Detected.
    + exact (body_closure R Callbacks BindEffect).
Qed.
End ConcreteCallbacks.
