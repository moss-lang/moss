(* The wrapper cache preserves the exact body emitted for a callee and arity. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import Audit EaBase EaBind EaChecked EaCodeEffects EaCodeAllocationSound EaRecSound NMap.
From NM Require Import LegacyBodyJudgments.
From NM Require EaBindCodeEffects EaContextTableSound EaControlSound EaDefinedProviderSound
  EaMain EaSatisfiesSound PublicContextFacts WorldSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Definition WrappedSlot (W : world) (bound : nat) (state : est) (g n : nat)
    (slot : code_slot FunctionTable) : Prop :=
  bound <= slot_index slot < es_next state /\
  exists formal needs, w_context W g = Some (formal, needs) /\
    read_code state FunctionTable (slot_index slot) = Some (method_wrapper_body g n formal needs).

Definition WrappedRecords (W : world) (bound : nat) (state : est) : Prop :=
  forall g n slot, In ((g, n), slot) (es_wrap state) -> WrappedSlot W bound state g n slot.

Lemma initial_wrapped_records : forall W bound, WrappedRecords W bound (init_state bound).
Proof. intros W bound g n slot []. Qed.

Lemma wrapped_slot_preserved : forall W bound before after g n slot,
  CodeTransition before after -> WrappedSlot W bound before g n slot ->
  WrappedSlot W bound after g n slot.
Proof.
  intros W bound before after g n slot Change [[Lower Upper] (formal & needs & Context & Code)].
  split; [pose proof (counter_grows _ _ Change); lia|].
  exists formal, needs. split; [exact Context|].
  now rewrite (old_slots_preserved _ _ Change) by exact Upper.
Qed.

Lemma wrapped_records_preserved : forall W bound before after,
  CodeTransition before after -> es_wrap after = es_wrap before ->
  WrappedRecords W bound before -> WrappedRecords W bound after.
Proof.
  intros W bound before after Change Same Cache g n slot Member.
  rewrite Same in Member. eapply wrapped_slot_preserved; [exact Change|now apply Cache].
Qed.

(* The only additional change besides reserving and writing code is the
   wrapper row. In particular the forwarder, link and constant caches persist. *)
Record MethodWrapperEffects (before after : est) : Prop := {
  method_wrapper_code_effect : CodeTransition before after;
  method_wrapper_fwd : es_fwd after = es_fwd before;
  method_wrapper_link : es_link after = es_link before;
  method_wrapper_const : es_const after = es_const before;
  method_wrapper_lamcx : es_lamcx after = es_lamcx before;
  method_wrapper_pins : es_pins after = es_pins before;
  method_wrapper_names : es_names after = es_names before
}.

Theorem place_method_wrapper_sound : forall W bound before g n formal needs slot after,
  w_context W g = Some (formal, needs) -> bound <= es_next before ->
  WrappedRecords W bound before ->
  place_method_wrapper g n formal needs before = Ok (slot, after) ->
  MethodWrapperEffects before after /\ WrappedRecords W bound after /\
  WrappedSlot W bound after g n slot /\ pair_look (g, n) (es_wrap after) = Some slot.
Proof.
  intros W bound before g n formal needs slot after Context Lower Cache Run.
  pose proof (place_method_wrapper_code_effect _ _ _ _ _ _ _ Run) as Effect.
  unfold place_method_wrapper, get_wrap, mbind in Run.
  destruct (pair_look (g, n) (es_wrap before)) as [cached|] eqn:Hit.
  - unfold ret in Run. injection Run as <- <-.
    split; [constructor; try reflexivity; exact Effect|].
    split; [exact Cache|]. split; [|exact Hit].
    apply Cache. now apply EaBindCodeEffects.pair_look_found.
  - unfold emit_fn_code, reserve_code, fresh, mbind, ret, write_fn_code, set_fn_body, put_wrap in Run.
    injection Run as <- <-.
    match type of Effect with CodeTransition before ?after => set (written := after) in * end.
    split; [constructor; try reflexivity; exact Effect|].
    assert (Stored : WrappedSlot W bound written g n (CodeSlot (es_next before))).
    { unfold written. split; [cbn [slot_index es_next]; lia|].
      exists formal, needs. split; [exact Context|].
      cbn [read_code es_fns slot_index]. apply WorldSound.nget_nset_same. }
    split.
    + intros other count cached Member. cbn [es_wrap] in Member.
      destruct Member as [Same|Older].
      * now injection Same as <- <- <-.
      * eapply wrapped_slot_preserved; [exact Effect|now apply Cache].
    + split; [exact Stored|]. unfold written. cbn [es_wrap pair_look fst snd].
      now rewrite !Nat.eqb_refl.
Qed.

Lemma wrapped_slot_in_assembly : forall E bound entry P final state g n formal needs slot,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  (forall symbol, bound <= symbol -> Fresh (e_W E) symbol) ->
  GeneratedPost bound final state ->
  w_context (e_W E) g = Some (formal, needs) -> WrappedSlot (e_W E) bound state g n slot ->
  Fresh (e_W E) (slot_index slot) /\
  body_of P (slot_index slot) = Some
    (ECall g (theta_ctx formal) (map (fun k => (rkey_of formal k, rkey_of formal k)) needs)
      (map EVar (seq 1 n))).
Proof.
  intros E bound entry P final state g n formal needs slot Assemble WorldBound Post Context
    [Range (read_formal & read_needs & Read & Code)].
  rewrite Context in Read. injection Read as <- <-.
  split; [apply WorldBound; lia|].
  pose proof (generated_code_in_assembly E bound entry P final state Assemble Post
    FunctionTable _ _ Range Code) as Body.
  cbn in Body. unfold method_wrapper_body in Body.
  assert (Keys : map (fun k => (rkof formal k, rkof formal k)) needs =
      map (fun k => (rkey_of formal k, rkey_of formal k)) needs).
  { apply map_ext. intro key. now rewrite !EaSatisfiesSound.rkof_exact. }
  now rewrite Keys in Body.
Qed.

Section WrappedProvider.
Variables (E : env) (P : program) (bound : nat) (entry : string) (final : est).
Let W := e_W E.
Let post := GeneratedPost bound final.
Variable valid : frame -> cx -> est -> Prop.
Hypotheses
  (assembly : EaMain.assemble E bound entry (init_state bound) = Ok (P, final))
  (world_bound : forall symbol, bound <= symbol -> Fresh W symbol)
  (contexts : EaContextTableSound.ContextTableSound W)
  (cache_valid : forall F C state, valid F C state -> WrappedRecords W bound state)
  (counter_valid : forall F C state, valid F C state -> bound <= es_next state)
  (wrapper_valid : forall F C before after, valid F C before ->
    MethodWrapperEffects before after -> WrappedRecords W bound after -> valid F C after).

Local Lemma wrapped_provider_context : forall F C path sg g types sm formal needs,
  FnProvider W P F C (AEPath path None) sg None g types sm ->
  w_context W g = Some (formal, needs) -> CtxOf W g formal needs.
Proof.
  intros F C path sg g types sm formal needs Provider Read.
  pose proof (contexts _ _ _ Read) as Context.
  inversion Provider; subst.
  - match goal with Kind : kind_sym W g = Some DKFn |- _ => now rewrite Kind in Context end.
  - match goal with Translate : TranslatesForwarder W P _ _ g |- _ => destruct Translate as [FreshSlot _] end.
    unfold Fresh in FreshSlot. unfold kind_sym in Context. now rewrite FreshSlot in Context.
Qed.

Theorem wrapped_method_provider_post_sound : forall R method F C path sg receiver,
  Checked post (valid F C) (fun _ => valid F C) (fn_provider E "method" F C path sg)
    (fun '(g, types, sm) _ => FnProvider W P F C (AEPath path None) sg None g types sm) ->
  Checked post (valid F C) (fun _ => valid F C)
    (meth_provider E R method F C (AEPath path None) sg receiver)
    (fun '(target, types, prefix, sm) _ =>
      MethProvider W P F C (AEPath path None) sg receiver target types prefix sm).
Proof.
  intros R method F C path sg receiver Function before [[[target types] prefix] sm] after Run.
  cbn [meth_provider] in Run.
  apply EaControlSound.mbind_ok in Run as ([[g types'] sm'] & middle & FunctionRun & Run).
  fold W in Run.
  destruct (w_context W g) as [[formal needs]|] eqn:Context; cbn [mbind ret] in Run; [|discriminate].
  apply EaControlSound.mbind_ok in Run as (slot & written & Place & Run).
  unfold ret in Run. injection Run as <- <- <- <- <-.
  destruct (Function _ _ _ FunctionRun) as [Before FunctionSound].
  pose proof (place_method_wrapper_code_effect _ _ _ _ _ _ _ Place) as Change.
  assert (Middle : post written -> post middle) by (eapply code_transition_preserves_post; exact Change).
  split; [intros Final; now apply Before, Middle|]. intros Valid Final.
  destruct (FunctionSound Valid (Middle Final)) as [ValidMiddle Provider].
  destruct (place_method_wrapper_sound W bound _ _ _ _ _ _ _ Context
    (counter_valid _ _ _ ValidMiddle) (cache_valid _ _ _ ValidMiddle) Place)
    as (Effects & Cache & Stored & _).
  split; [eapply wrapper_valid; eassumption|].
  destruct (wrapped_slot_in_assembly E bound entry P final written g _ formal needs slot
    assembly world_bound Final Context Stored) as [FreshSlot Body].
  eapply MPWrapped; [exact Provider|eapply wrapped_provider_context; eassumption|reflexivity|
    exact FreshSlot|exact Body].
Qed.

Theorem method_provider_post_sound : forall bundle R,
  WorldOf bundle W ->
  (forall F C state, valid F C state -> PublicContextFacts.CxSection C) ->
  (forall F C path sg,
    Checked post (valid F C) (fun _ => valid F C) (fn_provider E "method" F C path sg)
      (fun '(target, types, sm) _ => FnProvider W P F C (AEPath path None) sg None target types sm)) ->
  (forall what F C params ann body sg receiver,
    Checked post (valid F C) (fun _ => valid F C)
      (lambda_provider E R what F C params ann body sg (Some receiver))
      (fun '(target, prefix, sm) _ =>
        LambdaProvider W P F C params ann body sg (Some receiver) target prefix sm)) ->
  forall method F C syntax sg receiver,
  Checked post (valid F C) (fun _ => valid F C) (meth_provider E R method F C syntax sg receiver)
    (fun '(target, types, prefix, sm) _ => MethProvider W P F C syntax sg receiver target types prefix sm).
Proof.
  intros bundle R World Caller Function Lambda method F C syntax sg receiver.
  destruct syntax as [| |path application| |syntax field| | | | | | | | | | |params ann body];
    try solve [intros before result after Run; discriminate].
  - destruct application; [intros before result after Run; discriminate|].
    apply wrapped_method_provider_post_sound, Function.
  - destruct syntax as [| |path application| | | | | | | | | | | | |];
      try solve [intros before result after Run; discriminate].
    destruct application; [intros before result after Run; discriminate|].
    exact (EaDefinedProviderSound.attached_method_provider_post_sound E P valid post contexts Caller
      bundle R method F C path field sg receiver World).
  - intros before [[[target types] prefix] sm] after Run.
    cbn [meth_provider] in Run. fold W in Run.
    apply EaControlSound.mbind_ok in Run as ([[provider pre] needs] & written & Provider & Run).
    unfold ret in Run. injection Run as <- <- <- <- <-.
    destruct (Lambda _ F C params ann body sg receiver _ _ _ Provider) as [Backward Typed].
    split; [exact Backward|]. intros Valid Final.
    destruct (Typed Valid Final) as [ValidAfter ProviderTyped].
    split; [exact ValidAfter|now apply MPLambda].
Qed.

End WrappedProvider.
