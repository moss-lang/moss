(* The body passes share one allocator and four code/constant caches.
   Cache typing can use final-program code via GeneratedPost. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase EaCodeAllocationSound EaCodeEffects EaRecSound EaCacheEffects.
From NM Require EaAssemblyCodeEffects EaBindCodeEffects EaConstantSound EaControlSound
  EaForwarderSound EaFunctor EaLinkBodySound EaWrappedProviderSound WorldSound.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Import EaForwarderSound EaWrappedProviderSound EaConstantSound EaLinkBodySound.

Record BodyCaches (W : world) (P : program) (bound : nat) (state : est) : Prop := {
  body_counter : bound <= es_next state;
  body_code_tables : CodeTablesBound state;
  body_link_records : LinkRecords state;
  body_lambda_origins : LambdaOriginsBound state;
  body_constants : TypedConstantCache W P state;
  body_forwarders : ForwarderRecords bound state;
  body_wrappers : WrappedRecords W bound state
}.

Definition BodyState W P bound state : Prop :=
  BodyCaches W P bound state /\ TypedLinkCache W P state.

Lemma initial_body_state : forall W P bound, BodyState W P bound (init_state bound).
Proof.
  intros W P bound. split.
  - constructor; [apply Nat.le_refl|apply code_tables_initial|apply initial_link_records|
      apply initial_lambda_origins_bound|apply initial_constant_cache_typed|
      apply initial_forwarder_records|apply initial_wrapped_records].
  - intros u provider slot Read. discriminate Read.
Qed.

Lemma link_cache_same : forall W P before after,
  es_link after = es_link before -> es_pins after = es_pins before ->
  TypedLinkCache W P before -> TypedLinkCache W P after.
Proof. intros W P before after Rows Pins. unfold TypedLinkCache, link_pins_at. now rewrite Rows, Pins. Qed.

Lemma constant_cache_same : forall W P before after,
  es_const after = es_const before ->
  TypedConstantCache W P before -> TypedConstantCache W P after.
Proof. intros W P before after Rows. unfold TypedConstantCache. now rewrite Rows. Qed.

Local Lemma forwarder_cache_kept : forall bound before after,
  GeneratedPost bound after before -> es_fwd after = es_fwd before ->
  ForwarderRecords bound before -> ForwarderRecords bound after.
Proof.
  intros bound before after Post Rows Cache symbol arity slot Read.
  unfold forwarder_at, forwarders_at in Read. rewrite Rows in Read.
  destruct (Cache _ _ _ Read) as [Range Stored].
  split; [pose proof (generated_counter _ _ _ Post); lia|].
  eapply generated_code; eassumption.
Qed.

Local Lemma wrapper_cache_kept : forall W bound before after,
  GeneratedPost bound after before -> es_wrap after = es_wrap before ->
  WrappedRecords W bound before -> WrappedRecords W bound after.
Proof.
  intros W bound before after Post Rows Cache callee arity slot Member.
  rewrite Rows in Member.
  destruct (Cache _ _ _ Member) as (Range & formal & needs & Context & Stored).
  split; [pose proof (generated_counter _ _ _ Post); lia|].
  exists formal, needs. split; [exact Context|].
  eapply generated_code; eassumption.
Qed.

Local Lemma carry_body_caches : forall W P bound before after,
  BodyCaches W P bound before ->
  GeneratedPost bound after before -> CodeTablesBound after -> LinkRecords after ->
  LambdaOriginsBound after ->
  es_fwd after = es_fwd before -> es_wrap after = es_wrap before ->
  es_const after = es_const before ->
  BodyCaches W P bound after.
Proof.
  intros W P bound before after [Floor Tables Links Origins Constants Forwarders Wrappers]
    Post Tables' Links' Origins' Fwd Wrap Const.
  constructor; try assumption.
  - pose proof (generated_counter _ _ _ Post). lia.
  - eapply constant_cache_same; eassumption.
  - eapply forwarder_cache_kept; eassumption.
  - eapply wrapper_cache_kept; eassumption.
Qed.

Lemma carry_caches_under_transition : forall W P bound before after,
  CodeTransition before after -> BodyCaches W P bound before ->
  es_fwd after = es_fwd before -> es_wrap after = es_wrap before ->
  es_const after = es_const before ->
  BodyCaches W P bound after.
Proof.
  intros W P bound before after Change State Fwd Wrap Const.
  eapply carry_body_caches; [exact State| | | | |exact Fwd|exact Wrap|exact Const].
  - eapply code_transition_preserves_post; [exact Change|apply generated_post_refl].
  - eapply code_bound_preserved; [exact Change|exact (body_code_tables _ _ _ _ State)].
  - eapply link_records_preserved; [exact Change|exact (body_link_records _ _ _ _ State)].
  - eapply lambda_origins_bound_preserved; [exact Change|exact (body_lambda_origins _ _ _ _ State)].
Qed.

Theorem carry_body_state : forall W P bound before after,
  CodeTransition before after -> CacheFields before after ->
  BodyState W P bound before -> BodyState W P bound after.
Proof.
  intros W P bound before after Change [Fwd Wrap Const Link Pins] [State TypedLinks].
  split.
  - eapply carry_caches_under_transition; eassumption.
  - eapply link_cache_same; eassumption.
Qed.

Theorem write_constant_keeps_state : forall W P bound before after symbol d n syntax initial code declared,
  BodyState W P bound before ->
  w_decl W symbol = Some d -> d_ast d = Input.ADVal n syntax (Some initial) ->
  ConstantReading W P d syntax initial code declared ->
  put_const symbol (code, declared) before = Ok (tt, after) ->
  BodyState W P bound after.
Proof.
  intros W P bound before after symbol d n syntax initial code declared [State TypedLinks]
    Found Ast Reading Write.
  assert (Typed : TypedConstantCache W P after).
  { eapply cache_constant_sound; [exact (body_constants _ _ _ _ State)|exact Found|exact Ast|
      exact Reading|exact Write]. }
  destruct State as [Floor Tables Links Origins Constants Forwarders Wrappers].
  injection Write as <-. split; [constructor; try assumption|exact TypedLinks].
Qed.

Theorem place_forwarder_keeps_state : forall W P bound before after,
  BodyState W P bound before ->
  ForwarderEffects before after -> ForwarderRecords bound after ->
  BodyState W P bound after.
Proof.
  intros W P bound before after [State LinkTyped] [Change Wrap Link Const Origin Pins Names]
    Forwarders. split; [|eapply link_cache_same; eassumption].
  destruct State as [Floor Tables Links Origins Constants _ Wrappers].
  constructor; try exact Forwarders.
  - pose proof (counter_grows _ _ Change). lia.
  - eapply code_bound_preserved; eassumption.
  - eapply link_records_preserved; eassumption.
  - eapply lambda_origins_bound_preserved; eassumption.
  - eapply constant_cache_same; eassumption.
  - eapply wrapped_records_preserved; eassumption.
Qed.

Theorem place_wrapper_keeps_state : forall W P bound before after,
  BodyState W P bound before ->
  MethodWrapperEffects before after -> WrappedRecords W bound after ->
  BodyState W P bound after.
Proof.
  intros W P bound before after [State LinkTyped] [Change Fwd Link Const Origin Pins Names]
    Wrappers. split; [|eapply link_cache_same; eassumption].
  destruct State as [Floor Tables Links Origins Constants Forwarders _].
  constructor; try exact Wrappers.
  - pose proof (counter_grows _ _ Change). lia.
  - eapply code_bound_preserved; eassumption.
  - eapply link_records_preserved; eassumption.
  - eapply lambda_origins_bound_preserved; eassumption.
  - eapply constant_cache_same; eassumption.
  - eapply forwarder_records_preserved; eassumption.
Qed.

Theorem lambda_reservation_keeps_caches : forall W P bound count before values middle slot after,
  BodyCaches W P bound before ->
  fresh_n count before = Ok (values, middle) ->
  reserve_code FunctionTable middle = Ok (slot, after) ->
  BodyCaches W P bound after.
Proof.
  intros W P bound count before values middle slot after State Capture Reserve.
  pose proof (code_effect_fresh_n _ _ _ _ Capture) as First.
  pose proof (code_effect_reserve _ _ _ _ Reserve) as Second.
  destruct (cache_effect_fresh_n _ _ _ _ Capture) as [Fwd Wrap Const Link Pins].
  destruct (cache_effect_reserve _ _ _ _ Reserve) as [Fwd' Wrap' Const' Link' Pins'].
  eapply (carry_caches_under_transition W P bound before after);
    [exact (code_transition_compose _ _ _ First Second)|exact State|congruence|congruence|congruence].
Qed.

Theorem fill_lambda_keeps_caches : forall W P bound before written after slot body C,
  BodyCaches W P bound before -> slot_index slot < es_next before ->
  read_code before FunctionTable (slot_index slot) = None ->
  write_fn_code slot body before = Ok (tt, written) ->
  put_lamcx slot C written = Ok (tt, after) ->
  BodyCaches W P bound after.
Proof.
  intros W P bound before written after [slot] body C State Issued Empty Write Note.
  destruct State as [Floor Tables Links Origins Constants Forwarders Wrappers].
  unfold write_fn_code, set_fn_body in Write. injection Write as <-.
  unfold put_lamcx in Note. injection Note as <-.
  cbn [slot_index] in Issued, Empty.
  constructor; cbn [es_next]; try assumption.
  - intros id Beyond [].
    all: change (es_next before <= id) in Beyond.
    + cbn [read_code es_fns]. rewrite WorldSound.nget_nset_other by lia.
      exact (Tables id Beyond FunctionTable).
    + exact (Tables id Beyond FunctorTable).
  - intros id Beyond. change (es_next before <= id) in Beyond. cbn [read_origin es_lamcx]. rewrite WorldSound.nget_nset_other by lia.
    exact (Origins id Beyond).
  - intros g arity cached Read.
    destruct (Forwarders _ _ _ Read) as [Range Stored]. split; [exact Range|].
    cbn [read_code es_fns]. rewrite WorldSound.nget_nset_other; [exact Stored|].
    intro Same. rewrite Same, Empty in Stored. discriminate.
  - intros g arity cached Present.
    destruct (Wrappers _ _ _ Present) as (Range & formal & needs & Context & Stored).
    split; [exact Range|]. exists formal, needs. split; [exact Context|].
    cbn [read_code es_fns]. rewrite WorldSound.nget_nset_other; [exact Stored|].
    intro Same. rewrite Same, Empty in Stored. discriminate.
Qed.

Theorem linked_emit_keeps_caches : forall W P bound u provider pins code before slot after,
  BodyCaches W P bound before ->
  EaFunctor.emit_linked_body u provider pins code before = Ok (slot, after) ->
  BodyCaches W P bound after.
Proof.
  intros W P bound u provider pins code before slot after State Run.
  pose proof (proj1 (EaBindCodeEffects.emit_linked_body_records_origin _ _ _ _ _ _ _ Run))
    as Change.
  eapply carry_caches_under_transition; [exact Change|exact State| | |].
  all: unfold EaFunctor.emit_linked_body, emit_fun_code, reserve_code, mbind, fresh, ret,
    write_fun_code, set_fun_body, put_pins, put_link in Run; injection Run as _ <-; reflexivity.
Qed.

Theorem source_write_keeps_state : forall W P bound symbol body before after,
  symbol < bound -> BodyState W P bound before ->
  set_fn_body symbol body before = Ok (tt, after) -> BodyState W P bound after.
Proof.
  intros W P bound symbol body before after Source [State Typed] Run.
  pose proof (EaAssemblyCodeEffects.source_write_preserves_bound bound symbol body Source) as Bounded.
  pose proof (body_counter _ _ _ _ State) as Floor.
  pose proof (body_code_tables _ _ _ _ State) as Tables.
  pose proof (source_action_establishes_post _ _ _ _ _ _ Bounded Floor Tables Run) as Post.
  destruct (Bounded _ _ _ Floor Tables Run) as (Next & Code & _ & _ & Links & Origins).
  destruct (cache_effect_write_fn _ _ _ _ _ Run) as [Fwd Wrap Const Link Pins].
  split; [|eapply link_cache_same; eassumption].
  eapply carry_body_caches; [exact State|exact Post|exact Code|apply Links|apply Origins|exact Fwd|
    exact Wrap|exact Const].
  - exact (body_link_records _ _ _ _ State).
  - exact (body_lambda_origins _ _ _ _ State).
Qed.
