(* Successful body elaboration preserves every earlier code slot.
   A lambda reserves its own slot before elaborating its body, and that
   recursive elaboration must preserve the vacancy until the lambda writes. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase EaBind EaCodeAllocationSound NMap.
From NM Require WorldSound EaControlSound EaFunctor.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Record CodeTransition (before after : est) : Prop := {
  counter_grows : es_next before <= es_next after;
  old_slots_preserved : forall table n, n < es_next before ->
    read_code after table n = read_code before table n;
  code_bound_preserved : CodeTablesBound before -> CodeTablesBound after;
  old_origins_preserved : forall table n, n < es_next before ->
    read_origin after table n = read_origin before table n;
  link_records_preserved : LinkRecords before -> LinkRecords after
}.

Definition PreservesOldCode {A} (action : M A) : Prop :=
  forall s x s', action s = Ok (x, s') -> CodeTransition s s'.

Lemma code_transition_refl : forall s, CodeTransition s s.
Proof. constructor; auto. Qed.

Lemma code_transition_compose : forall a b c,
  CodeTransition a b -> CodeTransition b c -> CodeTransition a c.
Proof.
  intros a b c [NextB SlotsB BoundB OriginsB LinksB] [NextC SlotsC BoundC OriginsC LinksC]. constructor.
  - lia.
  - intros table n Old. rewrite SlotsC by lia. exact (SlotsB table n Old).
  - auto.
  - intros table n Old. rewrite OriginsC by lia. exact (OriginsB table n Old).
  - auto.
Qed.

Lemma code_transition_extends : forall a b,
  CodeTablesBound a -> CodeTransition a b -> CodeTablesExtend a b.
Proof.
  intros a b Bound Transition table n code Found.
  assert (Older : n < es_next a).
  { destruct (lt_dec n (es_next a)) as [Older|Beyond]; [exact Older|].
    rewrite (Bound n ltac:(lia) table) in Found. discriminate. }
  rewrite (old_slots_preserved a b Transition table n Older). exact Found.
Qed.

Lemma code_transition_preserves_reservation : forall a b n,
  CodeTransition a b -> n < es_next a -> VacantCode a n -> VacantCode b n.
Proof.
  intros a b n Transition Older Reserved table.
  rewrite (old_slots_preserved a b Transition table n Older). apply Reserved.
Qed.

Lemma unchanged_code_transition : forall a b,
  es_next a <= es_next b ->
  (forall table n, read_code b table n = read_code a table n) ->
  (forall table n, read_origin b table n = read_origin a table n) ->
  es_link b = es_link a ->
  CodeTransition a b.
Proof.
  intros a b Next Same Origins Links. constructor; try assumption.
  - intros table n _. apply Same.
  - intros Bound n Beyond table. rewrite Same. apply Bound. lia.
  - intros table n _. apply Origins.
  - intros Registered target slot Member. rewrite Links in Member.
    destruct (Registered target slot Member) as (Before & Target & Code).
    split; [lia|]. split.
    + unfold read_link_target. now rewrite Links.
    + now rewrite Same.
Qed.

Lemma code_effect_return : forall A (x : A), PreservesOldCode (ret x).
Proof. intros A x s y s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_fail : forall A msg, PreservesOldCode (@fail A msg).
Proof. intros A msg s y s' Run. discriminate. Qed.

Lemma code_effect_pure : forall A (result : er A), PreservesOldCode (pure result).
Proof. intros A [x|msg] s y s' Run; [|discriminate]. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_bind : forall A B (action : M A) (next : A -> M B),
  PreservesOldCode action ->
  (forall x, PreservesOldCode (next x)) ->
  PreservesOldCode (mbind action next).
Proof.
  intros A B action next First Second s x s' Run.
  apply EaControlSound.mbind_ok in Run as (middle & st & Run1 & Run2).
  eapply code_transition_compose; [eapply First|eapply Second]; eassumption.
Qed.

Lemma code_effect_mmap : forall A B (f : A -> M B) values,
  (forall x, PreservesOldCode (f x)) -> PreservesOldCode (mmap f values).
Proof.
  intros A B f values Effects. induction values as [|x rest IH]; cbn [mmap].
  - apply code_effect_return.
  - apply code_effect_bind; [apply Effects|]. intro y.
    apply code_effect_bind; [exact IH|]. intro ys. apply code_effect_return.
Qed.

Lemma code_effect_fresh : PreservesOldCode fresh.
Proof.
  intros s x s' Run. unfold fresh in Run. injection Run as _ <-.
  apply unchanged_code_transition; [cbn [es_next]; lia| | |reflexivity]; intros [] n; reflexivity.
Qed.

Lemma code_effect_fresh_n : forall count, PreservesOldCode (fresh_n count).
Proof.
  induction count as [|count IH]; cbn [fresh_n]; [apply code_effect_return|].
  apply code_effect_bind; [apply code_effect_fresh|]. intro g.
  apply code_effect_bind; [exact IH|]. intro gs. apply code_effect_return.
Qed.

Lemma code_effect_reserve : forall table, PreservesOldCode (reserve_code table).
Proof.
  intro table. unfold reserve_code.
  apply code_effect_bind; [apply code_effect_fresh|intro g; apply code_effect_return].
Qed.

Lemma code_effect_put_name : forall n name, PreservesOldCode (put_name n name).
Proof.
  intros n name s x s' Run. unfold put_name in Run. injection Run as _ <-.
  apply unchanged_code_transition; [apply Nat.le_refl| | |reflexivity]; intros [] i; reflexivity.
Qed.

Lemma code_effect_reserved_lamcx : forall slot C a b c value,
  es_next a <= slot_index slot ->
  CodeTransition a b ->
  put_lamcx slot C b = Ok (value, c) ->
  CodeTransition a c.
Proof.
  intros slot C a b c value Fresh [Next Code Bound Origins Links] Run.
  unfold put_lamcx in Run. injection Run as _ <-.
  constructor; [exact Next|exact Code|exact Bound| |exact Links].
  intros [] n Old; cbn [read_origin es_lamcx es_link].
  - rewrite WorldSound.nget_nset_other by lia. apply (Origins FunctionTable n Old).
  - apply (Origins FunctorTable n Old).
Qed.

Lemma code_effect_reserved_write : forall table (slot : code_slot table) code a b c value,
  es_next a <= slot_index slot < es_next b ->
  CodeTransition a b ->
  write_code slot code b = Ok (value, c) ->
  CodeTransition a c.
Proof.
  intros [] [slot] code a b c value [Fresh Valid] Transition Run;
    unfold write_code, write_fn_code, write_fun_code, set_fn_body, set_fun_body in Run;
    injection Run as _ <-;
    destruct Transition as [Next Old Bound Origins Links]; cbn [slot_index] in Fresh, Valid;
    constructor; cbn [es_next].
  all: try exact Next.
  all: try solve [intros table n Present; destruct table; cbn [read_code es_fns es_funs];
    try rewrite WorldSound.nget_nset_other by lia;
    first [exact (Old FunctionTable n Present)|exact (Old FunctorTable n Present)]].
  all: try solve [intros BoundA n Future table; cbn [es_next] in Future; destruct table;
    cbn [read_code es_fns es_funs]; try rewrite WorldSound.nget_nset_other by lia;
    first [exact (Bound BoundA n Future FunctionTable)|exact (Bound BoundA n Future FunctorTable)]].
  all: try solve [intros [] n Earlier;
    [exact (Origins FunctionTable n Earlier)|exact (Origins FunctorTable n Earlier)]].
  all: intros Registered target linked Member;
    destruct (Links Registered target linked Member) as (Below & Target & Stored);
    split; [exact Below|split; [exact Target|]].
  - exact Stored.
  - destruct (Nat.eq_dec (slot_index linked) slot) as [Equal|Different].
    + exists code. cbn [read_code es_funs]. rewrite Equal. apply WorldSound.nget_nset_same.
    + cbn [read_code es_funs]. now rewrite WorldSound.nget_nset_other.
Qed.

Lemma code_effect_emit : forall table (code : code_in table), PreservesOldCode (emit_code table code).
Proof.
  intros table code s result s' Run.
  assert (Expand : emit_code table code =
    (let* slot := reserve_code table in let* _ := write_code slot code in ret slot)).
  { destruct table; reflexivity. }
  rewrite Expand in Run. clear Expand.
  apply EaControlSound.mbind_ok in Run as (slot & middle & Reserved & Run).
  apply EaControlSound.mbind_ok in Run as (unit_result & written & Written & Run).
  injection Run as _ <-.
  eapply code_effect_reserved_write; [|eapply code_effect_reserve; exact Reserved|exact Written].
  unfold reserve_code, mbind, fresh, ret in Reserved. injection Reserved as <- <-.
  cbn [slot_index es_next]. lia.
Qed.


Lemma code_effect_get_fwd : forall g arity, PreservesOldCode (get_fwd g arity).
Proof. intros g arity s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_put_fwd : forall g arity slot, PreservesOldCode (put_fwd g arity slot).
Proof.
  intros g arity slot s x s' Run. unfold put_fwd in Run. injection Run as _ <-.
  apply unchanged_code_transition; [apply Nat.le_refl| | |reflexivity]; intros [] i; reflexivity.
Qed.

Theorem place_forwarder_code_effect : forall handle signature,
  PreservesOldCode (place_forwarder handle signature).
Proof.
  intros handle signature. unfold place_forwarder.
  apply code_effect_bind; [apply code_effect_get_fwd|]. intros [slot|].
  - apply code_effect_return.
  - apply code_effect_bind; [exact (code_effect_emit FunctionTable _)|].
    intro slot. apply code_effect_bind; [apply code_effect_put_fwd|].
    intro ignored. apply code_effect_return.
Qed.

Lemma reserved_slot_number : forall table s slot after,
  reserve_code table s = Ok (slot, after) ->
  slot_index slot = es_next s /\ es_next after = S (es_next s).
Proof.
  intros table s slot after Run. unfold reserve_code, mbind, fresh, ret in Run.
  injection Run as <- <-. split; reflexivity.
Qed.

Theorem nested_code_preserves_reserved_slot : forall table A s slot after (prepare : M A) x written,
  CodeTablesBound s ->
  reserve_code table s = Ok (slot, after) ->
  PreservesOldCode prepare -> prepare after = Ok (x, written) ->
  VacantCode written (slot_index slot).
Proof.
  intros table A s slot after prepare x written Bound Reserve Preserves Run.
  pose proof (reserved_slot_number _ _ _ _ Reserve) as [Number Advance].
  apply (code_transition_preserves_reservation after written (slot_index slot)).
  - eapply Preserves. exact Run.
  - lia.
  - intro kind. unfold reserve_code, mbind, fresh, ret in Reserve.
    injection Reserve as <- <-. destruct kind.
    + exact (Bound _ (Nat.le_refl _) FunctionTable).
    + exact (Bound _ (Nat.le_refl _) FunctorTable).
Qed.

Lemma code_effect_check : forall (b : bool) msg, PreservesOldCode (check b msg).
Proof. intros [] msg; [apply code_effect_return|apply code_effect_fail]. Qed.

Lemma code_effect_miter : forall A (f : A -> M unit) values,
  (forall x, PreservesOldCode (f x)) -> PreservesOldCode (miter f values).
Proof.
  intros A f values Effects. induction values as [|x rest IH]; cbn [miter].
  - apply code_effect_return.
  - apply code_effect_bind; [apply Effects|intro ignored; exact IH].
Qed.

Lemma code_effect_map_err : forall A f (action : M A),
  PreservesOldCode action -> PreservesOldCode (map_err f action).
Proof.
  intros A f action Effect s x s' Run. unfold map_err in Run.
  destruct (action s) as [[value after]|msg] eqn:Checked; [|discriminate].
  injection Run as <- <-. eapply Effect. exact Checked.
Qed.

Theorem lambda_provider_code_effect : forall E R what F C ps rt body sg this,
  (forall F' C' block expected, PreservesOldCode (r_blke R F' C' block expected)) ->
  PreservesOldCode (lambda_provider E R what F C ps rt body sg this).
Proof.
  intros E R what F C ps rt body sg this Lower. unfold lambda_provider.
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind.
  { destruct (suniq_dup (map fst ps)); [apply code_effect_fail|apply code_effect_return]. }
  intros []. apply code_effect_bind.
  { apply code_effect_miter. intros [[name [annotation|]] argument]; cbn [check_lambda_annotation].
    - apply code_effect_bind; [apply code_effect_pure|intro ty; apply code_effect_check].
    - apply code_effect_return. }
  intros []. apply code_effect_bind.
  { destruct rt as [annotation|]; cbn [check_lambda_annotation].
    - apply code_effect_bind; [apply code_effect_pure|intro ty; apply code_effect_check].
    - apply code_effect_return. }
  intros []. cbn zeta. apply code_effect_bind; [apply code_effect_fresh_n|].
  intros capvals s result final Run.
  apply EaControlSound.mbind_ok in Run as (slot & reserved & Reserve & Run).
  apply EaControlSound.mbind_ok in Run as (numbered_result & numbered & Numbered & Run).
  apply EaControlSound.mbind_ok in Run as ([expression result_type] & nested & Body & Run).
  apply EaControlSound.mbind_ok in Run as (fit_result & validated & Fits & Run).
  apply EaControlSound.mbind_ok in Run as (write_result & written & Write & Run).
  apply EaControlSound.mbind_ok in Run as (note_result & noted & Note & Run).
  injection Run as _ <-.
  assert (Prepare : CodeTransition reserved validated).
  { eapply code_transition_compose; [eapply code_effect_check; exact Numbered|].
    eapply code_transition_compose; [eapply code_effect_map_err; [apply Lower|exact Body]|].
    eapply code_effect_check. exact Fits. }
  destruct (reserved_slot_number _ _ _ _ Reserve) as [Index Next].
  assert (Range : es_next s <= slot_index slot < es_next validated).
  { pose proof (counter_grows _ _ Prepare). lia. }
  eapply (code_effect_reserved_lamcx slot C); [lia| |exact Note].
  eapply (code_effect_reserved_write FunctionTable); [exact Range| |exact Write].
  eapply code_transition_compose; [eapply code_effect_reserve; exact Reserve|exact Prepare].
Qed.


Theorem lambda_provider_records_section : forall E R what F C ps rt body sg this
    before pid prefix needs after,
  lambda_provider E R what F C ps rt body sg this before = Ok ((pid, prefix, needs), after) ->
  read_origin after FunctionTable pid = Some (c_sub C) /\
  exists emitted, read_code after FunctionTable pid = Some emitted.
Proof.
  intros E R what F C ps rt body sg this before pid prefix needs after Run.
  unfold lambda_provider in Run.
  do 7 (apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run)).
  apply EaControlSound.mbind_ok in Run as ([body_code body_type] & ?state & ?step & Run).
  do 3 (apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run)).
  injection Run as <- _ _ <-.
  match goal with
  | Note : put_lamcx ?slot C ?written = Ok (_, _),
    Write : write_fn_code ?slot ?emitted _ = Ok (_, ?written) |- _ =>
      unfold put_lamcx in Note; injection Note as _ <-;
      split; [cbn [read_origin es_lamcx]; now rewrite WorldSound.nget_nset_same|];
      exists emitted; unfold write_fn_code, set_fn_body in Write; injection Write as _ <-;
      apply WorldSound.nget_nset_same
  end.
Qed.


Lemma code_effect_lift : forall A (value : option A) message,
  PreservesOldCode (lift value message).
Proof.
  intros A [x|] message; [apply code_effect_return|apply code_effect_fail].
Qed.

Theorem fresh_table_code_effect : forall E u roles, PreservesOldCode (EaFunctor.fresh_table E u roles).
Proof.
  intros E u roles. unfold EaFunctor.fresh_table.
  apply code_effect_mmap. intro role.
  apply code_effect_bind; [apply code_effect_fresh|]. intro generation.
  apply code_effect_bind; [apply code_effect_put_name|].
  intro named. apply code_effect_return.
Qed.

Theorem fn_provider_code_effect : forall E kind F C pa sg, PreservesOldCode (fn_provider E kind F C pa sg).
Proof.
  intros E kind F C pa sg. unfold fn_provider.
  apply code_effect_bind; [apply code_effect_lift|]. intro g.
  apply code_effect_bind; [apply code_effect_check|intros []].
  destruct (ast_of (e_W E) g) as [syntax|]; [|apply code_effect_fail].
  destruct syntax as [| | | | | |receiver name parameters result body| |]; try apply code_effect_fail.
  destruct receiver; try apply code_effect_fail.
  destruct body as [body|].
  - apply code_effect_bind; [apply code_effect_lift|intros [Cg needs]].
    repeat (apply code_effect_bind; [first [apply code_effect_pure | apply code_effect_check]|intro]).
    apply code_effect_return.
  - apply code_effect_bind; [apply code_effect_pure|]. intro handle.
    apply code_effect_bind; [apply place_forwarder_code_effect|].
    intro slot. apply code_effect_return.
Qed.

Lemma code_effect_require_allocation_floor : forall support,
  PreservesOldCode (EaFunctor.require_allocation_floor support).
Proof.
  intros support before result after Read. unfold EaFunctor.require_allocation_floor in Read.
  eapply code_effect_check. exact Read.
Qed.

Theorem application_outputs_code_effect : forall E F C u outs,
  PreservesOldCode (EaFunctor.allocate_application_outputs E F C u outs).
Proof.
  intros E F C u outs. unfold EaFunctor.allocate_application_outputs. cbn zeta.
  apply code_effect_bind.
  { destruct outs; [apply code_effect_return|apply code_effect_require_allocation_floor]. }
  intros []. apply code_effect_mmap. intro output. unfold EaFunctor.allocate_output.
  destruct (EaCall.bound_in C output || nmem output (context_gens C ++ key_receiver_gens C))%bool.
  - apply code_effect_bind; [apply code_effect_fresh|]. intro generation.
    apply code_effect_bind; [apply code_effect_put_name|intro named; apply code_effect_return].
  - match goal with
    | |- PreservesOldCode (if ?blocked then _ else _) =>
      destruct blocked; [apply code_effect_fail|apply code_effect_return]
    end.
Qed.

Theorem apply_abstract_code_effect : forall E F C u, PreservesOldCode (EaFunctor.apply_abstract E F C u).
Proof.
  intros E F C u. unfold EaFunctor.apply_abstract.
  apply code_effect_bind; [apply code_effect_lift|]. intro fixed.
  apply code_effect_bind; [apply code_effect_lift|]. intros [callee needs].
  apply code_effect_bind.
  { match goal with
    | |- PreservesOldCode (match ?missing with Some _ => _ | None => _ end) =>
      destruct missing as [[t ty]|]; [apply code_effect_fail|apply code_effect_return]
    end. }
  intros []. apply code_effect_bind.
  { match goal with
    | |- PreservesOldCode (match ?mismatch with Some _ => _ | None => _ end) =>
      destruct mismatch as [[t ty]|]; [apply code_effect_fail|apply code_effect_return]
    end. }
  intros []. apply code_effect_bind; [apply code_effect_pure|]. intro selected.
  apply code_effect_bind; [apply application_outputs_code_effect|]. intro outputs.
  apply code_effect_bind; [apply code_effect_pure|]. intros [result keys].
  apply code_effect_bind; [apply code_effect_pure|]. intros [formal formal_keys].
  apply code_effect_bind; [apply code_effect_check|]. intro accepted.
  apply code_effect_return.
Qed.

Lemma code_effect_get_wrap : forall key, PreservesOldCode (get_wrap key).
Proof. intros key s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_put_wrap : forall key slot, PreservesOldCode (put_wrap key slot).
Proof.
  intros key slot s x s' Run. unfold put_wrap in Run. injection Run as _ <-.
  apply unchanged_code_transition; [apply Nat.le_refl| | |reflexivity]; intros [] i; reflexivity.
Qed.

Theorem place_method_wrapper_code_effect : forall g n Cg needs,
  PreservesOldCode (place_method_wrapper g n Cg needs).
Proof.
  intros g n Cg needs. unfold place_method_wrapper.
  apply code_effect_bind; [apply code_effect_get_wrap|]. intros [slot|].
  - apply code_effect_return.
  - apply code_effect_bind; [exact (code_effect_emit FunctionTable _)|].
    intro slot. apply code_effect_bind; [apply code_effect_put_wrap|].
    intro stored. apply code_effect_return.
Qed.

Theorem meth_provider_code_effect : forall E R method F C provider sg receiver,
  (forall F' C' block expected, PreservesOldCode (r_blke R F' C' block expected)) ->
  PreservesOldCode (meth_provider E R method F C provider sg receiver).
Proof.
  intros E R method F C provider sg receiver Lower. unfold meth_provider.
  destruct provider as [| |path application| |syntax field| | | | | | | | | | |parameters ann body];
    try apply code_effect_fail.
  - destruct application; [apply code_effect_fail|].
    apply code_effect_bind; [apply fn_provider_code_effect|]. intros [[g types] sm].
    apply code_effect_bind; [apply code_effect_lift|]. intros [context needs].
    apply code_effect_bind; [apply place_method_wrapper_code_effect|].
    intro slot. apply code_effect_return.
  - destruct syntax as [| |path application| | | | | | | | | | | | |]; try apply code_effect_fail.
    destruct application; [apply code_effect_fail|].
    apply code_effect_bind; [apply code_effect_lift|]. intro nominal.
    apply code_effect_bind; [apply code_effect_lift|]. intro attached.
    destruct (ast_of (e_W E) attached) as [syntax|]; [|apply code_effect_fail].
    destruct syntax as [| | | | | |receiver_kind name ps result body| |]; try apply code_effect_fail.
    destruct receiver_kind; try apply code_effect_fail.
    destruct body; [|apply code_effect_fail].
    destruct (canon C receiver); try apply code_effect_fail.
    apply code_effect_bind; [apply code_effect_check|intros []].
    apply code_effect_bind; [apply code_effect_lift|]. intros [context needs].
    repeat (apply code_effect_bind; [first [apply code_effect_pure | apply code_effect_check]|intro]).
    apply code_effect_return.
  - apply code_effect_bind; [apply lambda_provider_code_effect; exact Lower|].
    intros [[slot prefix] needs]. apply code_effect_return.
Qed.
