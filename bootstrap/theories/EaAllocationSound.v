(* The executable counter supplies a consecutive range of unused identities. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase.
From NM Require EaFunctor AllocationFacts EaBind EaTypesSound EaDecide.
Import Audit.Semantics.Elaboration ListNotations.

Theorem fresh_n_range : forall count s,
  exists s', fresh_n count s = Ok (seq (es_next s) count, s') /\
    es_next s' = es_next s + count.
Proof.
  induction count as [|count IH]; intros s.
  - exists s. split; [reflexivity|lia].
  - destruct s as [next fns funs fwd wrap link const unsupported lamcx pins names].
    cbn [fresh_n mbind fresh es_next es_fns es_funs es_fwd es_wrap es_link
      es_const es_unsupported es_lamcx es_pins es_names].
    destruct (IH (Est (S next) fns funs fwd wrap link const unsupported lamcx pins names))
      as (s' & Run & Next).
    cbn [es_next] in Run, Next. unfold mbind. rewrite Run.
    exists s'. split; [reflexivity|lia].
Qed.

Theorem fresh_table_range : forall E u roles s,
  exists s', EaFunctor.fresh_table E u roles s =
    Ok (combine roles (seq (es_next s) (length roles)), s') /\
    es_next s' = es_next s + length roles.
Proof.
  intros E u roles. induction roles as [|role roles IH]; intros s.
  - exists s. split; [reflexivity|cbn; lia].
  - destruct s as [next fns funs fwd wrap link const unsupported lamcx pins names].
    cbn [EaFunctor.fresh_table mmap mbind fresh put_name es_next es_fns es_funs
      es_fwd es_wrap es_link es_const es_unsupported es_lamcx es_pins es_names ret].
    unfold mbind.
    match goal with
    | |- context [mmap ?allocate roles ?after] =>
        change (mmap allocate roles after) with (EaFunctor.fresh_table E u roles after);
        destruct (IH after) as (s' & Run & Next)
    end.
    cbn [es_next] in Run, Next. rewrite Run.
    exists s'. split; [reflexivity|cbn; lia].
Qed.

Lemma read_fresh_table : forall roles start,
  NoDup roles ->
  map (EaFunctor.gen_of (combine roles (seq start (length roles)))) roles = seq start (length roles).
Proof.
  intros roles start Distinct. revert start.
  induction Distinct as [|role roles Absent Distinct IH]; intros start; [reflexivity|].
  cbn [seq combine length map]. unfold EaFunctor.gen_of at 1. cbn [nlook].
  rewrite Nat.eqb_refl. f_equal.
  transitivity (map (EaFunctor.gen_of (combine roles (seq (S start) (length roles)))) roles).
  - apply map_ext_in. intros r Member. unfold EaFunctor.gen_of. cbn [nlook].
    destruct (Nat.eqb_spec r role); [subst r; contradiction|reflexivity].
  - apply IH.
Qed.

(* A copy allocates its output readings and its unknown input readings
   consecutively. Both tables use the same counter and allocation scope. *)
Theorem fresh_tables_allocate : forall E u outs ys (before : allocation_scope nat) s,
  NoDup outs -> NoDup ys ->
  (forall g, es_next s <= g -> scope_permits before g) ->
  (forall g, In g (scope_allocated before ++ scope_support before) -> g < es_next s) ->
  exists gt s1 ct s2,
    EaFunctor.fresh_table E u outs s = Ok (gt, s1) /\
    EaFunctor.fresh_table E u ys s1 = Ok (ct, s2) /\
    let batch := map (EaFunctor.gen_of gt) outs ++ map (EaFunctor.gen_of ct) ys in
    Allocates before batch (extend_allocation_scope before batch).
Proof.
  intros E u outs ys before s Outs Ys Permitted Reserved.
  destruct (fresh_table_range E u outs s) as (s1 & First & Next).
  destruct (fresh_table_range E u ys s1) as (s2 & Second & _).
  exists (combine outs (seq (es_next s) (length outs))), s1.
  exists (combine ys (seq (es_next s1) (length ys))), s2.
  split; [exact First|]. split; [exact Second|]. cbn zeta.
  rewrite (read_fresh_table outs _ Outs), (read_fresh_table ys _ Ys), Next, <- seq_app.
  apply AllocationFacts.allocate_range; assumption.
Qed.

Theorem capture_counter_allocation : forall W C count s,
  (forall g, es_next s <= g -> Fresh W g) ->
  (forall v, In v (value_provision_keys (c_provisions C)) -> v < es_next s) ->
  exists batch s',
    fresh_n count s = Ok (batch, s') /\ es_next s' = es_next s + count /\ length batch = count /\
    Allocates (capture_allocation_scope W C) batch
      (extend_allocation_scope (capture_allocation_scope W C) batch).
Proof.
  intros W C count s Permitted Reserved.
  destruct (fresh_n_range count s) as (s' & Run & Next).
  exists (seq (es_next s) count), s'. split; [exact Run|]. split; [exact Next|].
  split; [apply length_seq|apply AllocationFacts.allocate_range; assumption].
Qed.

Lemma suniq_exact : forall names,
  NoDup (suniq names) /\ forall name, In name (suniq names) <-> In name names.
Proof.
  assert (Fold : forall names acc,
    NoDup acc ->
    let result := fold_left (fun acc x => if smem x acc then acc else x :: acc) names acc in
    NoDup result /\ forall name, In name result <-> In name names \/ In name acc).
  { induction names as [|x xs IH]; intros acc Distinct; cbn [fold_left].
    - split; [exact Distinct|]. intro n. cbn [In]. tauto.
    - destruct (smem x acc) eqn:Known.
      + apply EaTypesSound.smem_spec in Known.
        destruct (IH acc Distinct) as [D Members].
        split; [exact D|]. intro name. rewrite Members. cbn [In]. intuition congruence.
      + assert (D : NoDup (x :: acc)).
        { constructor; [|exact Distinct]. intro Present.
          apply EaTypesSound.smem_spec in Present. congruence. }
        destruct (IH (x :: acc) D) as [D' Members].
        split; [exact D'|]. intro name. rewrite Members. cbn [In]. intuition. }
  intro names. unfold suniq.
  destruct (Fold names [] (NoDup_nil _)) as [Distinct Members].
  split; [apply NoDup_rev; exact Distinct|].
  intro name. rewrite <- in_rev, Members. cbn [In]. tauto.
Qed.

Theorem lambda_capture_names_exact : forall E F C parameters body,
  let caps := EaBind.lambda_capture_names E F C parameters body in
  NoDup caps /\
  forall n, In n caps <-> (Mentions_name (e_W E) F body n /\ slook n (c_locals C) <> None /\
    ~ In n parameters /\ fr_applying F = []).
Proof.
  intros E F C parameters body. unfold EaBind.lambda_capture_names.
  destruct (fr_applying F) as [|u stack] eqn:Applying.
  - cbn zeta.
    set (capturable := filter (fun n => is_some (slook n (c_locals C)) && negb (smem n parameters))
      (EaDecide.mentioned_names (e_W E) F body)).
    destruct (suniq_exact capturable) as [Distinct Members]. split; [exact Distinct|].
    intro n. rewrite Members. unfold capturable.
    rewrite filter_In, Bool.andb_true_iff, Bool.negb_true_iff.
    unfold Mentions_name, EaDecide.mentioned_names, EaDecide.global_name, is_some.
    destruct (slook n (c_locals C)) as [local|] eqn:Local.
    + destruct (smem n parameters) eqn:ParameterFound.
      * apply EaTypesSound.smem_spec in ParameterFound. split; [intros [_ [_ Efalse]]; discriminate|].
        intros (_ & _ & Absent & _). contradiction.
      * assert (Absent : ~ In n parameters).
        { intro Present. apply EaTypesSound.smem_spec in Present. congruence. }
        intuition discriminate.
    + split; [intros [_ [Present _]]; discriminate|intros (_ & Present & _); contradiction].
  - split; [constructor|]. intros n. split; [intros []|intros (_ & _ & _ & Bad); discriminate].
Qed.

Theorem lambda_captures_from_counter : forall E F C parameters body s,
  (forall g, es_next s <= g -> Fresh (e_W E) g) ->
  (forall v, In v (value_provision_keys (c_provisions C)) -> v < es_next s) ->
  let caps := EaBind.lambda_capture_names E F C parameters body in
  exists values s',
    fresh_n (length caps) s = Ok (values, s') /\
    es_next s' = es_next s + length caps /\
    TypesLambdaCaptures (e_W E) F C parameters body caps values.
Proof.
  intros E F C parameters body s Permitted Reserved. cbn zeta.
  set (caps := EaBind.lambda_capture_names E F C parameters body).
  destruct (lambda_capture_names_exact E F C parameters body) as [Distinct Names].
  destruct (capture_counter_allocation (e_W E) C (length caps) s Permitted Reserved)
    as (values & s' & Run & Next & Length & Allocated).
  exists values, s'. split; [exact Run|]. split; [exact Next|].
  unfold TypesLambdaCaptures. split; [exact Names|].
  split; [exact Distinct|]. split; [exact Length|].
  eexists. exact Allocated.
Qed.
