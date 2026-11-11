(* The actual bind sequence preserves earlier code and pending slots. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaCodeEffects EaCodeAllocationSound.
From NM Require EaBind EaControlSound EaFunctor WorldSound.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Lemma code_effect_optional_failure : forall A (problem : option A) message,
  PreservesOldCode (match problem with Some value => fail (message value) | None => ret tt end).
Proof.
  intros A [value|] message; [apply code_effect_fail|apply code_effect_return].
Qed.

Lemma code_effect_pair_failure : forall A B (problem : option (A * B)) message,
  PreservesOldCode (match problem with Some (x, y) => fail (message x y) | None => ret tt end).
Proof.
  intros A B [[x y]|] message; [apply code_effect_fail|apply code_effect_return].
Qed.

Theorem copy_interpretation_code_effect : forall E sigma u h Cu nsu nsh Ch,
  PreservesOldCode (EaFunctor.copy_interpretation E sigma u h Cu nsu nsh Ch).
Proof.
  intros E sigma u h Cu nsu nsh Ch. unfold EaFunctor.copy_interpretation.
  apply code_effect_bind; [apply code_effect_require_allocation_floor|intros []].
  apply code_effect_bind; [apply fresh_table_code_effect|]. intro outputs.
  apply code_effect_bind; [apply fresh_table_code_effect|]. intro inputs.
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind.
  { apply code_effect_mmap. intro k.
    apply code_effect_bind; [apply code_effect_lift|]. intro provided.
    apply code_effect_bind; [apply code_effect_pure|]. intro selected.
    apply code_effect_return. }
  intro matched. apply code_effect_bind; [apply code_effect_pure|]. intros [result keys].
  apply code_effect_bind; [apply code_effect_pure|]. intros [source source_keys].
  apply code_effect_bind.
  { apply code_effect_mmap. intro key. unfold EaFunctor.copy_result_position.
    match goal with
    | |- PreservesOldCode (match ?positions with [] => _ | _ :: _ => _ end) =>
      destruct positions as [|[position selected] rest]; [apply code_effect_fail|]
    end.
    apply code_effect_bind; [apply code_effect_check|intros []].
    repeat match goal with
    | |- PreservesOldCode (match ?value with Some _ => _ | None => _ end) =>
      destruct value
    end; try apply code_effect_fail.
    apply code_effect_bind; [apply code_effect_check|]. intro accepted. apply code_effect_return. }
  intro positions. apply code_effect_return.
Qed.

Theorem fun_copy_code_effect : forall E F C u h,
  PreservesOldCode (EaFunctor.fun_copy E F C u h).
Proof.
  intros E F C u h. unfold EaFunctor.fun_copy.
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind; [apply code_effect_lift|]. intro available.
  apply code_effect_bind; [apply code_effect_lift|]. intros [Cu nsu].
  apply code_effect_bind; [apply code_effect_lift|]. intros [Ch nsh].
  apply code_effect_bind; [apply code_effect_optional_failure|intros []].
  apply code_effect_bind; [apply code_effect_optional_failure|intros []].
  do 3 (apply code_effect_bind; [apply code_effect_check|intros []]).
  apply code_effect_bind; [apply copy_interpretation_code_effect|]. intros [needs positions].
  apply code_effect_return.
Qed.

Theorem bind_items_code_effect : forall E R,
  (forall F C item, PreservesOldCode (EaBind.bind_item E R F C item)) ->
  forall F C items, PreservesOldCode (EaBind.bind_items E R F C items).
Proof.
  intros E R Step F C items. revert F C.
  induction items as [|item rest IH]; intros F C; cbn [EaBind.bind_items].
  - apply code_effect_return.
  - apply code_effect_bind; [apply Step|]. intros [[[wrap C'] F'] made].
    apply code_effect_bind; [apply IH|]. intros [[[wrap' C''] F''] made'].
    apply code_effect_return.
Qed.

Theorem bind_item_code_effect : forall E R,
  (forall F C e expected, PreservesOldCode (r_ex R F C e expected)) ->
  (forall F C block expected, PreservesOldCode (r_blke R F C block expected)) ->
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  (forall F C u g, PreservesOldCode (EaFunctor.fun_closure E R F C u g)) ->
  forall F C item, PreservesOldCode (EaBind.bind_item E R F C item).
Proof.
  intros E R Expr Block Binds Closure F C [[left label brackets] right].
  unfold EaBind.bind_item. cbn zeta.
  destruct left as [path|].
  2: destruct label, brackets; apply code_effect_fail.
  destruct label as [method|].
  - destruct right as [rhs|]; [|apply code_effect_fail].
    apply code_effect_bind; [apply code_effect_lift|]. intro receiver.
    apply code_effect_bind; [apply code_effect_lift|]. intro symbol.
    apply code_effect_bind; [apply code_effect_check|intros []].
    apply code_effect_bind; [apply code_effect_pure|]. intro bracket_types.
    apply code_effect_bind; [apply code_effect_pure|]. intro receiver_type.
    apply code_effect_bind; [apply code_effect_pure|]. intro signature.
    apply code_effect_bind; [apply meth_provider_code_effect; exact Block|].
    intros [[[target types] prefix] needs].
    apply code_effect_bind; [apply code_effect_optional_failure|intros []].
    apply code_effect_return.
  - destruct brackets; [apply code_effect_fail|].
    apply code_effect_bind; [apply code_effect_lift|]. intro symbol.
    apply code_effect_bind; [apply code_effect_lift|]. intro declaration.
    destruct (d_ast declaration) as [|type_name| | | |value_name expected initial|
      receiver fn_name parameters result body| |functor_name arguments inputs outputs body];
      try solve [destruct right; apply code_effect_fail].
    + destruct right as [rhs|]; [|apply code_effect_fail].
      destruct rhs as [| |target application| | | | | | | | | | | | |];
        try apply code_effect_fail.
      apply code_effect_bind; [apply code_effect_check|intros []].
      apply code_effect_bind; [apply code_effect_pure|]. intro meaning.
      apply code_effect_return.
    + destruct right as [rhs|]; [|apply code_effect_fail].
      apply code_effect_bind; [apply code_effect_pure|]. intro declared.
      apply code_effect_bind; [apply Expr|]. intros [expression actual].
      apply code_effect_bind; [apply code_effect_check|intros []].
      apply code_effect_return.
    + destruct receiver; try solve [destruct right; apply code_effect_fail].
      destruct right as [rhs|]; [|apply code_effect_fail].
      destruct rhs as [| |provider application| | | | | | | | | | | | |params ann syntax];
        try apply code_effect_fail.
      * destruct application; [apply code_effect_fail|].
        apply code_effect_bind; [apply code_effect_pure|]. intro signature.
        apply code_effect_bind; [apply fn_provider_code_effect|]. intros [[target types] needs].
        apply code_effect_return.
      * apply code_effect_bind; [apply code_effect_pure|]. intro signature.
        apply code_effect_bind; [apply lambda_provider_code_effect; exact Block|].
        intros [[target prefix] needs]. apply code_effect_return.
    + destruct body as [body|].
      * destruct right; [apply code_effect_fail|].
        apply code_effect_bind; [apply code_effect_check|intros []].
        apply code_effect_bind; [apply Binds|]. intros [[[wrap C'] F'] made].
        apply code_effect_return.
      * destruct right as [rhs|]; [|apply apply_abstract_code_effect].
        destruct rhs as [| |provider application| | | | | | | | | | | | |]; try apply code_effect_fail.
        destruct application; [apply code_effect_fail|].
        apply code_effect_bind; [apply code_effect_lift|]. intro g.
        destruct (ast_of (e_W E) g) as [syntax|]; [|apply code_effect_fail].
        destruct syntax as [| | | | | | | |name arguments' inputs' outputs' body]; try apply code_effect_fail.
        destruct body.
        -- apply code_effect_bind; [apply Closure|]. intros [wrap C'].
           apply code_effect_return.
        -- apply code_effect_bind; [apply fun_copy_code_effect|]. intros [wrap C'].
           apply code_effect_return.
Qed.

Lemma code_effect_get_link : forall key, PreservesOldCode (get_link key).
Proof. intros key s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma code_effect_prepare_link : forall slot pins a b c value,
  es_next a <= slot_index slot ->
  CodeTransition a b ->
  put_pins slot pins b = Ok (value, c) ->
  CodeTransition a c.
Proof.
  intros slot pins a b c value Fresh [Next Code Bound Origins Links Lambda] Run.
  unfold put_pins in Run. injection Run as _ <-.
  constructor; [exact Next|exact Code|exact Bound| |exact Links|exact Lambda].
  intros [] n Old.
  - apply (Origins FunctionTable n Old).
  - cbn [read_origin]. unfold link_pins_at. cbn [es_pins].
    rewrite WorldSound.nget_nset_other by lia.
    exact (Origins FunctorTable n Old).
Qed.

Lemma code_effect_register_link : forall key slot a b c value code,
  es_next a <= slot_index slot < es_next b ->
  es_link b = es_link a ->
  read_code b FunctorTable (slot_index slot) = Some code ->
  CodeTransition a b ->
  put_link key slot b = Ok (value, c) ->
  CodeTransition a c.
Proof.
  intros key slot a b c value code [Fresh Valid] Same Stored [Next Code Bound Origins Links Lambda] Run.
  unfold put_link in Run. injection Run as _ <-.
  constructor; [exact Next|exact Code|exact Bound| | |exact Lambda].
  - intros [] n Old.
    + apply (Origins FunctionTable n Old).
    + unfold read_origin, read_link_target, link_pins_at. cbn [es_link es_pins find fst snd].
      destruct (Nat.eqb_spec n (slot_index slot)); [lia|apply (Origins FunctorTable n Old)].
  - intros Registered target earlier Member. cbn [es_link] in Member.
    destruct Member as [Equal|Member].
    + injection Equal as <- <-. split; [exact Valid|]. split; [|exists code; exact Stored].
      unfold read_link_target. cbn [es_link find fst snd]. now rewrite Nat.eqb_refl.
    + destruct (Links Registered target earlier Member) as (Below & Target & Existing).
      split; [exact Below|split; [|exact Existing]].
      unfold read_link_target in *. cbn [es_link find fst snd].
      rewrite Same in Member.
      pose proof (proj1 (Registered target earlier Member)) as Earlier.
      destruct (Nat.eqb_spec (slot_index earlier) (slot_index slot)); [lia|exact Target].
Qed.

Lemma code_effect_get_pins : forall slot, PreservesOldCode (get_pins slot).
Proof. intros slot s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Theorem emit_linked_body_records_origin : forall u gg pins code before h after,
  EaFunctor.emit_linked_body u gg pins code before = Ok (h, after) ->
  CodeTransition before after /\ read_code after FunctorTable (slot_index h) = Some code /\
  read_origin after FunctorTable (slot_index h) = Some ((u, gg), pins) /\
  es_next before <= slot_index h < es_next after.
Proof.
  intros u gg pins code before h after Run. unfold EaFunctor.emit_linked_body in Run.
  apply EaControlSound.mbind_ok in Run as (slot & written & Emitted & Run).
  apply EaControlSound.mbind_ok in Run as (pinned & middle & Pins & Run).
  apply EaControlSound.mbind_ok in Run as (linked & stored & Link & Run).
  injection Run as <- <-.
  assert (Emits : slot_index slot = es_next before /\
      es_next middle = S (es_next before) /\ es_link middle = es_link before /\
      read_code middle FunctorTable (slot_index slot) = Some code).
  { unfold emit_fun_code, reserve_code, mbind, fresh, ret, write_fun_code, set_fun_body in Emitted.
    injection Emitted as <- <-.
    unfold put_pins in Pins. injection Pins as _ <-.
    repeat split; try reflexivity. apply WorldSound.nget_nset_same. }
  destruct Emits as (Index & Next & Same & Code).
  split.
  - eapply (code_effect_register_link (u, gg) slot before middle stored linked code);
      [lia|exact Same|exact Code| |exact Link].
    eapply (code_effect_prepare_link slot pins); [lia| |exact Pins].
    exact (code_effect_emit FunctorTable code before slot written Emitted).
  - unfold put_link in Link. injection Link as _ <-.
    split; [exact Code|]. split.
    + unfold put_pins in Pins. injection Pins as _ <-.
      unfold read_origin, read_link_target, link_pins_at. cbn [es_link es_pins find fst snd].
      now rewrite Nat.eqb_refl, WorldSound.nget_nset_same.
    + cbn [es_next]. lia.
Qed.

Lemma pair_look_found : forall A key (rows : list ((nat * nat) * A)) value,
  pair_look key rows = Some value -> In (key, value) rows.
Proof.
  intros A [u gg] rows value. induction rows as [|[[target provider] stored] rows IH];
    cbn [pair_look fst snd]; [discriminate|].
  destruct (Nat.eqb u target && Nat.eqb gg provider) eqn:Equal; [|intro Run; right; now apply IH].
  apply andb_true_iff in Equal as [U G]. apply Nat.eqb_eq in U, G. subst target provider.
  intros Run. injection Run as <-. now left.
Qed.

Theorem cached_link_records_origin : forall before u gg slot after,
  LinkRecords before ->
  get_link (u, gg) before = Ok (Some slot, after) ->
  after = before /\ slot_index slot < es_next after /\
  read_origin after FunctorTable (slot_index slot) = Some ((u, gg), link_pins_at after (slot_index slot)) /\
  exists code, read_code after FunctorTable (slot_index slot) = Some code.
Proof.
  intros before u gg slot after Registered Read. unfold get_link in Read.
  injection Read as Present <-. apply pair_look_found in Present.
  destruct (Registered _ _ Present) as (Earlier & Origin & Code).
  repeat split; try assumption. cbn [read_origin]. now rewrite Origin.
Qed.

Lemma link_output_roles_code_effect : forall E u Cu Cb who,
  PreservesOldCode (EaFunctor.link_output_roles E u Cu Cb who).
Proof.
  intros E u Cu Cb who. unfold EaFunctor.link_output_roles.
  apply code_effect_bind; [apply code_effect_optional_failure|intros []].
  apply code_effect_bind; [apply fresh_table_code_effect|]. intro roles.
  apply code_effect_bind; [apply code_effect_pure|]. intro promised.
  apply code_effect_return.
Qed.

Theorem link_body_code_effect : forall E R,
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  forall u g, PreservesOldCode (EaFunctor.link_body E R u g).
Proof.
  intros E R Binds u g. unfold EaFunctor.link_body.
  apply code_effect_bind; [apply code_effect_get_link|]. intros [cached|]; [apply code_effect_return|].
  apply code_effect_bind; [apply code_effect_lift|]. intro declaration.
  destruct (d_ast declaration) as [| | | | | | | |name parameters arguments results body]; try apply code_effect_fail.
  destruct body as [body|]; [|apply code_effect_fail].
  apply code_effect_bind; [apply code_effect_lift|]. intros [input input_needs].
  apply code_effect_bind; [apply code_effect_lift|]. intros [definition definition_needs].
  apply code_effect_bind; [apply Binds|]. intros [[[wrap returned] frame] made].
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind; [apply link_output_roles_code_effect|]. intros [[outputs generations] [result keys]].
  apply code_effect_bind; [apply code_effect_pure|]. intro plan.
  apply code_effect_bind; [apply code_effect_lift|]. intro bindings.
  apply code_effect_bind.
  { match goal with
    | |- PreservesOldCode (if ?family then _ else _) =>
      destruct family; [|apply code_effect_return]
    end.
    match goal with
    | |- PreservesOldCode (match ?observable with Some _ => _ | None => _ end) =>
      destruct observable as [binding|]; [destruct binding|];
        first [apply code_effect_return|apply code_effect_fail]
    end. }
  intros [] before h after Run.
  exact (proj1 (emit_linked_body_records_origin _ _ _ _ _ _ _ Run)).
Qed.

Lemma check_link_binding_code_effect : forall E u g Cu nsu Cg nsg recorded private adopted,
  PreservesOldCode (EaFunctor.check_link_binding E u g Cu nsu Cg nsg recorded private adopted).
Proof.
  intros E u g Cu nsu Cg nsg recorded private adopted. unfold EaFunctor.check_link_binding.
  apply code_effect_bind.
  { intros s result after Run. injection Run as _ <-. apply code_transition_refl. }
  intro state. apply code_effect_bind; [apply code_effect_pure|]. intro choices.
  apply code_effect_bind; [apply code_effect_optional_failure|intros []].
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_return.
Qed.

Theorem fun_closure_code_effect : forall E R,
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  forall F C u g, PreservesOldCode (EaFunctor.fun_closure E R F C u g).
Proof.
  intros E R Binds F C u g. unfold EaFunctor.fun_closure.
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind; [apply code_effect_lift|]. intros [input input_needs].
  apply code_effect_bind; [apply code_effect_lift|]. intros [definition definition_needs].
  apply code_effect_bind; [apply code_effect_optional_failure|intros []].
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind; [apply check_link_binding_code_effect|intro target].
  apply code_effect_bind; [apply link_body_code_effect; exact Binds|]. intro slot.
  apply code_effect_bind; [apply code_effect_pure|]. intro captured.
  apply code_effect_bind; [apply code_effect_pair_failure|intros []].
  apply code_effect_bind; [apply code_effect_get_pins|]. intro pins.
  apply code_effect_bind; [apply code_effect_pair_failure|intros []].
  apply code_effect_return.
Qed.

Theorem bind_items_preserve_code : forall E R,
  (forall F C e expected, PreservesOldCode (r_ex R F C e expected)) ->
  (forall F C block expected, PreservesOldCode (r_blke R F C block expected)) ->
  (forall F C items, PreservesOldCode (r_bind_items R F C items)) ->
  forall F C items, PreservesOldCode (EaBind.bind_items E R F C items).
Proof.
  intros E R Expr Block Binds.
  apply bind_items_code_effect.
  apply bind_item_code_effect; try assumption.
  apply fun_closure_code_effect. exact Binds.
Qed.
