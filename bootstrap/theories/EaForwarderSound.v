(* Typing an abstract provider and assigning its forwarder a runtime slot. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaBind EaCodeAllocationSound NMap.
From NM Require AllocationFacts EaTypesSound EaMergeSound WorldSound EaMain EaControlSound EaAssemblyCodeEffects.
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

Lemma put_forwarder_spec : forall s g slot,
  exists s',
    put_fwd g slot s = Ok (tt, s') /\
    nget (es_fwd s') g = Some slot /\ es_next s' = es_next s /\
    forall table number, read_code s' table number = read_code s table number.
Proof.
  intros s g slot.
  exists (Est (es_next s) (es_fns s) (es_funs s) (nset (es_fwd s) g slot) (es_wrap s)
    (es_link s) (es_const s) (es_unsupported s) (es_lamcx s) (es_pins s) (es_names s)).
  split; [reflexivity|]. split; [apply WorldSound.nget_nset_same|].
  split; [reflexivity|intros [] number; reflexivity].
Qed.

Theorem place_forwarder_spec : forall W start handle sg s,
  start <= es_next s ->
  (forall n, start <= n -> Fresh W n) ->
  CodeTablesBound s ->
  (forall slot, nget (es_fwd s) (forwarder_source handle) = Some slot ->
    ForwarderSlotSound W s handle sg slot) ->
  exists slot s',
    place_forwarder handle sg s = Ok (slot, s') /\
    nget (es_fwd s') (forwarder_source handle) = Some slot /\
    ForwarderSlotSound W s' handle sg slot /\
    CodeTablesBound s' /\ CodeTablesExtend s s' /\
    match nget (es_fwd s) (forwarder_source handle) with
    | Some old => slot = old /\ s' = s
    | None => Allocates (code_counter_scope W start s) [slot_index slot] (code_counter_scope W start s')
    end.
Proof.
  intros W start handle sg s Start WorldBound Tables Cache.
  destruct (nget (es_fwd s) (forwarder_source handle)) as [cached|] eqn:Cached.
  - exists cached, s. split.
    + unfold place_forwarder, get_fwd, mbind. now rewrite Cached.
    + split; [exact Cached|]. split; [apply Cache; reflexivity|].
      split; [exact Tables|]. split; [intros table n code Present; exact Present|].
      split; reflexivity.
  - destruct (emit_code_spec W start FunctionTable (forwarder_body handle sg) s Start WorldBound Tables)
      as (slot & middle & Emit & Allocated & _ & Bound & Stored & Extended).
    destruct (put_forwarder_spec middle (forwarder_source handle) slot)
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
  (forall slot, nget (es_fwd s) g = Some slot ->
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
