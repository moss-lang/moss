(* State fields left alone by allocation, source body writes and checks. *)
From Stdlib Require Import List Bool.
From NM Require Import Audit EaBase EaCodeAllocationSound EaCodeEffects EaControlSound.
From NM Require EaCall EaFunctor.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Record CacheFields (before after : est) : Prop := {
  same_forwarders : es_fwd after = es_fwd before;
  same_wrappers : es_wrap after = es_wrap before;
  same_constants : es_const after = es_const before;
  same_links : es_link after = es_link before;
  same_link_pins : es_pins after = es_pins before
}.

Definition PreservesCacheFields {A} (action : M A) : Prop :=
  forall before value after, action before = Ok (value, after) -> CacheFields before after.

Lemma cache_fields_same : forall state, CacheFields state state.
Proof. constructor; reflexivity. Qed.

Lemma cache_fields_compose : forall before middle after,
  CacheFields before middle -> CacheFields middle after -> CacheFields before after.
Proof. intros ? ? ? [] []; constructor; congruence. Qed.

Lemma cache_effect_bind : forall A B (action : M A) (next : A -> M B),
  PreservesCacheFields action ->
  (forall value, PreservesCacheFields (next value)) ->
  PreservesCacheFields (mbind action next).
Proof.
  intros A B action next First Second before result after Run.
  apply mbind_ok in Run as (middle & state & Start & Rest).
  eapply cache_fields_compose; [eapply First|eapply Second]; eassumption.
Qed.

Lemma cache_effect_ret : forall A (value : A), PreservesCacheFields (ret value).
Proof. intros A value before result after Run. now injection Run as _ <-; apply cache_fields_same. Qed.

Lemma cache_effect_fail : forall A message, PreservesCacheFields (@fail A message).
Proof. intros A message before result after Run. discriminate Run. Qed.

Lemma cache_effect_pure : forall A (result : er A), PreservesCacheFields (pure result).
Proof. intros A [value|message]; [exact (cache_effect_ret A value)|exact (cache_effect_fail A message)]. Qed.

Lemma cache_effect_lift : forall A (value : option A) message, PreservesCacheFields (lift value message).
Proof. intros A [value|] message; [exact (cache_effect_ret A value)|exact (cache_effect_fail A message)]. Qed.

Lemma cache_effect_check : forall (condition : bool) message, PreservesCacheFields (check condition message).
Proof. intros [] message; [exact (cache_effect_ret unit tt)|exact (cache_effect_fail unit message)]. Qed.

Lemma cache_effect_floor : forall support, PreservesCacheFields (EaFunctor.require_allocation_floor support).
Proof. intros support before value after Run. eapply cache_effect_check; exact Run. Qed.

Lemma cache_effect_mmap : forall A B (action : A -> M B) values,
  (forall value, PreservesCacheFields (action value)) ->
  PreservesCacheFields (mmap action values).
Proof.
  intros A B action values Effect. induction values as [|value values IH]; cbn [mmap].
  - apply cache_effect_ret.
  - apply cache_effect_bind; [apply Effect|]. intro head.
    apply cache_effect_bind; [exact IH|]. intro tail. apply cache_effect_ret.
Qed.

Lemma cache_effect_fresh : PreservesCacheFields fresh.
Proof. intros before value after Run. injection Run as _ <-. constructor; reflexivity. Qed.

Lemma cache_effect_fresh_n : forall n, PreservesCacheFields (fresh_n n).
Proof.
  induction n as [|n IH]; cbn [fresh_n]; [apply cache_effect_ret|].
  apply cache_effect_bind; [apply cache_effect_fresh|]. intro g.
  apply cache_effect_bind; [exact IH|]. intro gs. apply cache_effect_ret.
Qed.

Lemma cache_effect_reserve : forall table, PreservesCacheFields (reserve_code table).
Proof.
  intro table. unfold reserve_code. apply cache_effect_bind; [apply cache_effect_fresh|].
  intro g. apply cache_effect_ret.
Qed.

Lemma cache_effect_write_fn : forall symbol body, PreservesCacheFields (set_fn_body symbol body).
Proof. intros symbol body before value after Run. injection Run as _ <-. constructor; reflexivity. Qed.

Lemma cache_effect_lamcx : forall slot context, PreservesCacheFields (put_lamcx slot context).
Proof. intros slot context before value after Run. injection Run as _ <-. constructor; reflexivity. Qed.

Local Lemma cache_effect_name : forall symbol name, PreservesCacheFields (put_name symbol name).
Proof. intros symbol name before value after Run. injection Run as _ <-. constructor; reflexivity. Qed.

Local Ltac cache_pure :=
  first [apply cache_effect_ret | apply cache_effect_fail | apply cache_effect_lift |
    apply cache_effect_check | apply cache_effect_pure | apply cache_effect_floor |
    apply cache_effect_fresh | apply cache_effect_name].

Local Ltac cache_check_match :=
  match goal with
  | |- PreservesCacheFields (match ?problem with Some _ => _ | None => _ end) =>
    first [destruct problem as [[left right]|]; cache_pure | destruct problem; cache_pure]
  end.

Lemma cache_effect_fresh_table : forall E u roles,
  PreservesCacheFields (EaFunctor.fresh_table E u roles).
Proof.
  intros E u roles. unfold EaFunctor.fresh_table.
  apply cache_effect_mmap. intro role.
  apply cache_effect_bind; [cache_pure|]. intro generation.
  apply cache_effect_bind; [cache_pure|]. intros []. cache_pure.
Qed.

Lemma cache_effect_output_roles : forall E u Cu Cb who,
  PreservesCacheFields (EaFunctor.link_output_roles E u Cu Cb who).
Proof.
  intros E u Cu Cb who. unfold EaFunctor.link_output_roles.
  apply cache_effect_bind; [cache_check_match|intros []].
  apply cache_effect_bind; [apply cache_effect_fresh_table|]. intro roles.
  apply cache_effect_bind; [cache_pure|intros [result keys]]. cache_pure.
Qed.

Lemma cache_effect_application_outputs : forall E F C u outs,
  PreservesCacheFields (EaFunctor.allocate_application_outputs E F C u outs).
Proof.
  intros E F C u outs. unfold EaFunctor.allocate_application_outputs. cbn zeta.
  apply cache_effect_bind; [destruct outs; cache_pure|intros []].
  apply cache_effect_mmap. intro output. unfold EaFunctor.allocate_output.
  destruct (EaCall.bound_in C output || nmem output (context_gens C ++ key_receiver_gens C))%bool.
  - apply cache_effect_bind; [cache_pure|]. intro generation.
    apply cache_effect_bind; [cache_pure|intros []]. cache_pure.
  - match goal with
    | |- PreservesCacheFields (if ?blocked then _ else _) => destruct blocked; cache_pure
    end.
Qed.

Theorem cache_effect_apply_abstract : forall E F C u,
  PreservesCacheFields (EaFunctor.apply_abstract E F C u).
Proof.
  intros E F C u. unfold EaFunctor.apply_abstract.
  apply cache_effect_bind; [cache_pure|]. intro fixed.
  apply cache_effect_bind; [cache_pure|]. intros [formal needs].
  do 2 (apply cache_effect_bind; [cache_check_match|intros []]).
  apply cache_effect_bind; [cache_pure|]. intro selected.
  apply cache_effect_bind; [apply cache_effect_application_outputs|]. intro outputs.
  do 2 (apply cache_effect_bind; [cache_pure|intros [? ?]]).
  apply cache_effect_bind; [cache_pure|intros []]. cache_pure.
Qed.

Local Lemma cache_effect_copy_interpretation : forall E sigma u h Cu nsu nsh Ch,
  PreservesCacheFields (EaFunctor.copy_interpretation E sigma u h Cu nsu nsh Ch).
Proof.
  intros E sigma u h Cu nsu nsh Ch. unfold EaFunctor.copy_interpretation.
  apply cache_effect_bind; [cache_pure|intros []].
  do 2 (apply cache_effect_bind; [apply cache_effect_fresh_table|intro]).
  apply cache_effect_bind; [cache_pure|intros []].
  apply cache_effect_bind.
  { apply cache_effect_mmap. intro key.
    apply cache_effect_bind; [cache_pure|]. intro provision.
    apply cache_effect_bind; [cache_pure|]. intro selected. cache_pure. }
  intro matched.
  do 2 (apply cache_effect_bind; [cache_pure|intros [? ?]]).
  apply cache_effect_bind.
  { apply cache_effect_mmap. intro key. unfold EaFunctor.copy_result_position.
    match goal with
    | |- PreservesCacheFields (match ?positions with [] => _ | _ :: _ => _ end) =>
        destruct positions as [|[position selected] rest]; [cache_pure|]
    end.
    apply cache_effect_bind; [cache_pure|intros []].
    repeat match goal with
    | |- PreservesCacheFields (match ?value with Some _ => _ | None => _ end) => destruct value
    end; try cache_pure.
    apply cache_effect_bind; [cache_pure|intros []]. cache_pure. }
  intro positions. cache_pure.
Qed.

Theorem cache_effect_copy : forall E F C u h,
  PreservesCacheFields (EaFunctor.fun_copy E F C u h).
Proof.
  intros E F C u h. unfold EaFunctor.fun_copy.
  apply cache_effect_bind; [cache_pure|intros []].
  apply cache_effect_bind; [cache_pure|]. intro available.
  do 2 (apply cache_effect_bind; [cache_pure|intros [? ?]]).
  do 2 (apply cache_effect_bind; [cache_check_match|intros []]).
  do 3 (apply cache_effect_bind; [cache_pure|intros []]).
  apply cache_effect_bind; [apply cache_effect_copy_interpretation|intros [? ?]]. cache_pure.
Qed.
