(* Typing an abstract provider and assigning its forwarder a runtime slot. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaBind EaTypes EaCodeAllocationSound EaCodeEffects EaChecked EaRecSound NMap.
From NM Require Import LegacyBodyJudgments.
From NM Require AllocationFacts EaTypesSound EaMergeSound WorldSound EaMain EaControlSound EaAssemblyCodeEffects.
From NM Require EaContextTableSound EaDefinedProviderSound PublicContextFacts.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Lemma cfn_exact : forall C g, cfn C g = nlook g (c_fns C).
Proof.
  intros C g. unfold cfn, c_fns.
  induction (c_provisions C) as [|[k p] rest IH]; [reflexivity|].
  destruct k as [t|v|f|r m a|u], p as [t' a'|sg a'|sg a'|fx];
    cbn [cfn_go flat_map app nlook]; try exact IH.
  now destruct (Nat.eqb g f).
Qed.

Theorem read_forwarder_sound : forall E kind F C pa g sg handle,
  resolve_sym (e_W E) (fr_mod F) pa = Some g ->
  kind_sym (e_W E) g = Some DKFn ->
  (exists nm ps rt, ast_of (e_W E) g = Some (ADFn RPlain nm ps rt None)) ->
  read_forwarder E kind C g sg = Ok handle ->
  handle = ForwarderOf g /\ TypesForwarder (e_W E) F C pa sg handle.
Proof.
  intros E kind F C pa g sg handle Resolved Kind Abstract Checked.
  unfold read_forwarder in Checked.
  destruct (cfn C g) as [provided|] eqn:Present; [|discriminate].
  cbn [rbind] in Checked.
  destruct (fsig_eqb provided sg) eqn:Equal; [|discriminate].
  apply EaMergeSound.fsig_eqb_spec in Equal. subst provided. injection Checked as <-.
  split; [reflexivity|]. unfold TypesForwarder. cbn [forwarder_source].
  rewrite cfn_exact in Present. repeat split; assumption.
Qed.

Definition ForwarderSlotSound (W : world) (s : est) (handle : forwarder_handle)
    (sg : fsig) (slot : code_slot FunctionTable) : Prop :=
  Fresh W (slot_index slot) /\
  read_code s FunctionTable (slot_index slot) = Some (forwarder_body handle sg).

Lemma put_forwarder_spec : forall s g arity slot,
  exists s',
    put_fwd g arity slot s = Ok (tt, s') /\
    forwarder_at s' g arity = Some slot /\ es_next s' = es_next s /\
    forall table number, read_code s' table number = read_code s table number.
Proof.
  intros s g arity slot.
  exists (Est (es_next s) (es_fns s) (es_funs s) (nset (es_fwd s) g (nset (forwarders_at s g) arity slot)) (es_wrap s)
    (es_link s) (es_const s) (es_lamcx s) (es_pins s) (es_names s)).
  split; [reflexivity|]. split.
  - unfold forwarder_at, forwarders_at. cbn [es_fwd]. now rewrite !WorldSound.nget_nset_same.
  - split; [reflexivity|intros [] number; reflexivity].
Qed.

Theorem place_forwarder_spec : forall W start handle sg s,
  start <= es_next s ->
  (forall n, start <= n -> Fresh W n) ->
  CodeTablesBound s ->
  (forall slot, forwarder_at s (forwarder_source handle) (List.length (sg_params sg)) = Some slot ->
    ForwarderSlotSound W s handle sg slot) ->
  exists slot s',
    place_forwarder handle sg s = Ok (slot, s') /\
    forwarder_at s' (forwarder_source handle) (List.length (sg_params sg)) = Some slot /\
    ForwarderSlotSound W s' handle sg slot /\
    CodeTablesBound s' /\ CodeTablesExtend s s' /\
    match forwarder_at s (forwarder_source handle) (List.length (sg_params sg)) with
    | Some old => slot = old /\ s' = s
    | None => Allocates (code_counter_scope W start s) [slot_index slot] (code_counter_scope W start s')
    end.
Proof.
  intros W start handle sg s Start WorldBound Tables Cache.
  destruct (forwarder_at s (forwarder_source handle) (List.length (sg_params sg))) as [cached|] eqn:Cached.
  - exists cached, s. split.
    + unfold place_forwarder, get_fwd, mbind. now rewrite Cached.
    + split; [exact Cached|]. split; [apply Cache; reflexivity|].
      split; [exact Tables|]. split; [intros table n code Present; exact Present|].
      split; reflexivity.
  - destruct (emit_code_spec W start FunctionTable (forwarder_body handle sg) s Start WorldBound Tables)
      as (slot & middle & Emit & Allocated & _ & Bound & Stored & Extended).
    destruct (put_forwarder_spec middle (forwarder_source handle) (List.length (sg_params sg)) slot)
      as (s' & Remember & InCache & Next & SameCode).
    pose proof (AllocationFacts.allocation_member nat _ _ _ _ Allocated (or_introl eq_refl))
      as [FreshSlot _].
    exists slot, s'. split.
    + unfold place_forwarder, get_fwd, mbind. cbn [emit_code] in Emit.
      now rewrite Cached, Emit, Remember.
    + split; [exact InCache|]. split.
      * split; [exact FreshSlot|]. now rewrite SameCode.
      * split.
        -- intros n Beyond table. rewrite SameCode. apply Bound. now rewrite <- Next.
        -- split.
           ++ intros table n code Present. rewrite SameCode. exact (Extended _ _ _ Present).
           ++ unfold code_counter_scope in *. now rewrite Next.
Qed.

Lemma abstract_provider_path : forall E kind F C pa g nm ps rt sg,
  resolve_sym (e_W E) (fr_mod F) pa = Some g ->
  kind_sym (e_W E) g = Some DKFn ->
  ast_of (e_W E) g = Some (ADFn RPlain nm ps rt None) ->
  fn_provider E kind F C pa sg =
    (let* handle := pure (read_forwarder E kind C g sg) in
     let* slot := place_forwarder handle sg in
     let input := forwarder_source handle in
     ret (slot_index slot, TypeCapture [] [], [(IFn input, IFn input)])).
Proof.
  intros E kind F C pa g nm ps rt sg Resolved Kind Abstract.
  apply EaTypesSound.kind_is_spec in Kind.
  unfold fn_provider. rewrite Resolved. unfold mbind at 1. cbn [ret].
  rewrite Kind. unfold mbind at 1. cbn [ret]. now rewrite Abstract.
Qed.

Theorem placed_forwarder_translates : forall E bound entry P after s handle sg slot,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, after) ->
  CodeTablesExtend s after ->
  ForwarderSlotSound (e_W E) s handle sg slot ->
  TranslatesForwarder (e_W E) P handle sg (slot_index slot).
Proof.
  intros E bound entry P after s handle sg slot Run Extended [FreshSlot Stored].
  split; [exact FreshSlot|].
  rewrite (proj1 (EaAssemblyCodeEffects.assembled_code_lookup_exact E bound entry P after Run)).
  exact (Extended FunctionTable _ _ Stored).
Qed.

Theorem abstract_provider_sound : forall E kind F C pa g nm ps rt sg start s w types sm s' entry P final,
  resolve_sym (e_W E) (fr_mod F) pa = Some g ->
  kind_sym (e_W E) g = Some DKFn ->
  ast_of (e_W E) g = Some (ADFn RPlain nm ps rt None) ->
  start <= es_next s ->
  (forall n, start <= n -> Fresh (e_W E) n) ->
  CodeTablesBound s ->
  (forall slot, forwarder_at s g (List.length (sg_params sg)) = Some slot ->
    ForwarderSlotSound (e_W E) s (ForwarderOf g) sg slot) ->
  fn_provider E kind F C pa sg s = Ok ((w, types, sm), s') ->
  EaMain.assemble E start entry (init_state start) = Ok (P, final) ->
  CodeTablesExtend s' final ->
  FnProvider (e_W E) P F C (AEPath pa None) sg None w types sm.
Proof.
  intros E kind F C pa g nm ps rt sg start s w types sm s' entry P final
    Resolved Kind Abstract Start WorldBound Tables Cache Run Assembled Extended.
  rewrite (abstract_provider_path E kind F C pa g nm ps rt sg Resolved Kind Abstract) in Run.
  apply EaControlSound.mbind_ok in Run as (handle & initial & Read & Run).
  unfold pure in Read.
  destruct (read_forwarder E kind C g sg) as [selected|message] eqn:Checked; [|discriminate].
  injection Read as <- <-.
  destruct (read_forwarder_sound E kind F C pa g sg selected Resolved Kind
    (ex_intro _ nm (ex_intro _ ps (ex_intro _ rt Abstract))) Checked) as [-> Typed].
  apply EaControlSound.mbind_ok in Run as (slot & returned & Place & Run).
  destruct (place_forwarder_spec (e_W E) start (ForwarderOf g) sg s Start WorldBound Tables Cache)
    as (placed & written & Placed & _ & SlotSound & _).
  rewrite Placed in Place. injection Place as <- <-.
  unfold ret in Run. inversion Run; subst.
  eapply FPForward with (handle := ForwarderOf g); [exact Typed|].
  eapply placed_forwarder_translates; eassumption.
Qed.

Definition ForwarderSlot (bound : nat) (state : est) (g arity : nat)
    (slot : code_slot FunctionTable) : Prop :=
  bound <= slot_index slot < es_next state /\
  read_code state FunctionTable (slot_index slot) =
    Some (EInvoke g (map EVar (seq 0 arity))).

Definition ForwarderRecords (bound : nat) (state : est) : Prop :=
  forall g arity slot, forwarder_at state g arity = Some slot ->
    ForwarderSlot bound state g arity slot.

Lemma initial_forwarder_records : forall bound, ForwarderRecords bound (init_state bound).
Proof.
  intros bound g arity slot. unfold forwarder_at, forwarders_at.
  cbn [init_state es_fwd]. now rewrite !WorldSound.nget_NE.
Qed.

Lemma forwarder_slot_preserved : forall bound before after g arity slot,
  CodeTransition before after -> ForwarderSlot bound before g arity slot ->
  ForwarderSlot bound after g arity slot.
Proof.
  intros bound before after g arity slot Change [[Lower Upper] Code].
  split; [pose proof (counter_grows _ _ Change); lia|].
  now rewrite (old_slots_preserved _ _ Change) by exact Upper.
Qed.

Lemma forwarder_records_preserved : forall bound before after,
  CodeTransition before after -> es_fwd after = es_fwd before ->
  ForwarderRecords bound before -> ForwarderRecords bound after.
Proof.
  intros bound before after Change Same Cache g arity slot Lookup.
  unfold forwarder_at, forwarders_at in Lookup. rewrite Same in Lookup.
  eapply forwarder_slot_preserved; [exact Change|now apply Cache].
Qed.

Record ForwarderEffects (before after : est) : Prop := {
  forwarder_code_effect : CodeTransition before after;
  forwarder_wrap : es_wrap after = es_wrap before;
  forwarder_link : es_link after = es_link before;
  forwarder_const : es_const after = es_const before;
  forwarder_lamcx : es_lamcx after = es_lamcx before;
  forwarder_pins : es_pins after = es_pins before;
  forwarder_names : es_names after = es_names before
}.

Theorem place_forwarder_records : forall bound before handle sg slot after,
  bound <= es_next before -> ForwarderRecords bound before ->
  place_forwarder handle sg before = Ok (slot, after) ->
  ForwarderEffects before after /\ ForwarderRecords bound after /\
  ForwarderSlot bound after (forwarder_source handle) (List.length (sg_params sg)) slot.
Proof.
  intros bound before handle sg slot after Lower Cache Run.
  pose proof (place_forwarder_code_effect _ _ _ _ _ Run) as Change.
  unfold place_forwarder, get_fwd, mbind in Run.
  set (g := forwarder_source handle) in *.
  set (arity := List.length (sg_params sg)) in *.
  destruct (forwarder_at before g arity) as [cached|] eqn:Hit.
  - unfold ret in Run. injection Run as <- <-.
    split; [constructor; try reflexivity; exact Change|].
    split; [exact Cache|now apply Cache].
  - unfold emit_fn_code, reserve_code, fresh, mbind, ret, write_fn_code, set_fn_body, put_fwd in Run.
    injection Run as <- <-.
    match type of Change with CodeTransition before ?after => set (written := after) in * end.
    split; [constructor; try reflexivity; exact Change|].
    assert (Stored : ForwarderSlot bound written g arity (CodeSlot (es_next before))).
    { unfold written. split; [cbn [slot_index es_next]; lia|].
      cbn [read_code es_fns slot_index]. apply WorldSound.nget_nset_same. }
    split; [|exact Stored].
    intros other n found Lookup.
    unfold forwarder_at, forwarders_at, written in Lookup. cbn [es_fwd] in Lookup.
    rewrite WorldSound.nget_nset in Lookup.
    destruct (Nat.eqb other g) eqn:Symbol.
    + apply Nat.eqb_eq in Symbol. subst other. rewrite WorldSound.nget_nset in Lookup.
      destruct (Nat.eqb n arity) eqn:Arity.
      * apply Nat.eqb_eq in Arity. subst n. now injection Lookup as <-.
      * eapply forwarder_slot_preserved; [exact Change|now apply Cache].
    + eapply forwarder_slot_preserved; [exact Change|now apply Cache].
Qed.

Section FunctionProviders.
Variables (E : env) (P : program) (bound : nat) (entry : string) (final : est).
Let W := e_W E.
Let post := GeneratedPost bound final.
Variable valid : frame -> cx -> est -> Prop.
Hypotheses
  (assembly : EaMain.assemble E bound entry (init_state bound) = Ok (P, final))
  (world_bound : forall symbol, bound <= symbol -> Fresh W symbol)
  (contexts : EaContextTableSound.ContextTableSound W)
  (caller_section : forall F C state, valid F C state -> PublicContextFacts.CxSection C)
  (cache_valid : forall F C state, valid F C state -> ForwarderRecords bound state)
  (counter_valid : forall F C state, valid F C state -> bound <= es_next state)
  (forwarder_valid : forall F C before after, valid F C before ->
    ForwarderEffects before after -> ForwarderRecords bound after -> valid F C after).

Theorem abstract_provider_post_sound : forall kind F C path g nm ps rt sg,
  resolve_sym W (fr_mod F) path = Some g ->
  ast_of W g = Some (ADFn RPlain nm ps rt None) ->
  Checked post (valid F C) (fun _ => valid F C) (fn_provider E kind F C path sg)
    (fun '(target, types, sm) _ => FnProvider W P F C (AEPath path None) sg None target types sm).
Proof.
  intros kind F C path g nm ps rt sg Resolved Abstract before [[target types] sm] after Run.
  assert (Kind : kind_sym W g = Some DKFn) by
    exact (EaDefinedProviderSound.kind_of_read_declaration W _ _ Abstract).
  rewrite (abstract_provider_path E kind F C path g nm ps rt sg Resolved Kind Abstract) in Run.
  apply EaControlSound.mbind_ok in Run as (handle & initial & Checked & Run).
  unfold pure in Checked.
  destruct (read_forwarder E kind C g sg) as [selected|] eqn:Read; [|discriminate].
  injection Checked as <- <-.
  apply EaControlSound.mbind_ok in Run as (slot & written & Place & Run).
  unfold ret in Run. injection Run as <- <- <- <-.
  pose proof (place_forwarder_code_effect _ _ _ _ _ Place) as Change.
  split; [eapply code_transition_preserves_post; exact Change|]. intros Valid Final.
  destruct (place_forwarder_records bound _ _ _ _ _ (counter_valid _ _ _ Valid)
    (cache_valid _ _ _ Valid) Place) as (Effects & Cache & Range & Code).
  split; [eapply forwarder_valid; eassumption|].
  destruct (read_forwarder_sound E kind F C path g sg selected Resolved Kind
    (ex_intro _ nm (ex_intro _ ps (ex_intro _ rt Abstract))) Read) as [-> Typed].
  eapply FPForward; [exact Typed|]. split; [apply world_bound; lia|].
  exact (generated_code_in_assembly E bound entry P final written assembly Final FunctionTable
    (slot_index slot) (forwarder_body (ForwarderOf g) sg) Range Code).
Qed.

Theorem function_provider_post_sound : forall kind F C path sg,
  Checked post (valid F C) (fun _ => valid F C) (fn_provider E kind F C path sg)
    (fun '(target, types, sm) _ => FnProvider W P F C (AEPath path None) sg None target types sm).
Proof.
  intros kind F C path sg before result after Run.
  pose proof Run as Original. unfold fn_provider in Run. fold W in Run.
  destruct (resolve_sym W (fr_mod F) path) as [symbol|] eqn:Resolve;
    cbn [mbind ret] in Run; [|discriminate].
  destruct (kind_is W symbol DKFn) eqn:Kind; cbn [mbind ret] in Run; [|discriminate].
  destruct (ast_of W symbol) as [syntax|] eqn:Ast; [|discriminate].
  destruct syntax as [| | | | | |receiver name parameters output body| |]; try discriminate.
  destruct receiver; try discriminate. destruct body as [body|].
  - exact (EaDefinedProviderSound.defined_function_provider_post_sound E P valid post
      contexts caller_section F C path symbol name parameters output body sg kind Resolve Ast
      before result after Original).
  - exact (abstract_provider_post_sound kind F C path symbol name parameters output sg Resolve Ast
      before result after Original).
Qed.

End FunctionProviders.
