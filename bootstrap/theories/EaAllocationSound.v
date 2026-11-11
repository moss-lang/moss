(* The executable counter supplies a consecutive range of unused identities. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase.
From NM Require EaFunctor AllocationFacts EaBind EaTypesSound EaDecide.
Import AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.

Theorem fresh_n_range : forall count s,
  exists s', fresh_n count s = Ok (seq (es_next s) count, s') /\
    es_next s' = es_next s + count.
Proof.
  induction count as [|count IH]; intros s.
  - exists s. split; [reflexivity|lia].
  - destruct s as [next fns funs fwd wrap link const lamcx pins names].
    cbn [fresh_n mbind fresh es_next es_fns es_funs es_fwd es_wrap es_link
      es_const es_lamcx es_pins es_names].
    destruct (IH (Est (S next) fns funs fwd wrap link const lamcx pins names))
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
  - destruct s as [next fns funs fwd wrap link const lamcx pins names].
    cbn [EaFunctor.fresh_table mmap mbind fresh put_name es_next es_fns es_funs
      es_fwd es_wrap es_link es_const es_lamcx es_pins es_names ret].
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

Theorem lambda_capture_scope_free_exact : forall C first count,
  EaBind.lambda_capture_scope_free C first count = true <->
  forall v, In v (value_provision_keys (c_provisions C)) -> ~ (first <= v < first + count).
Proof.
  intros C first [|count].
  - split; [intros _ v _; lia|reflexivity].
  - cbn [EaBind.lambda_capture_scope_free]. rewrite forallb_forall. split.
    + intros Checked v Selected. unfold value_provision_keys in Selected.
      apply in_flat_map in Selected as ([key provision] & Member & Selected).
      specialize (Checked (key, provision) Member).
      destruct key; cbn [fst In] in Selected; try contradiction.
      destruct Selected as [<-|[]]. cbn [fst] in Checked.
      apply Bool.orb_true_iff in Checked as [Before|After].
      * apply Nat.ltb_lt in Before. lia.
      * apply Nat.leb_le in After. lia.
    + intros Disjoint [key provision] Member.
      destruct key as [t|v|f|recv m types|u]; cbn [fst]; try reflexivity.
      assert (Selected : In v (value_provision_keys (c_provisions C))).
      { unfold value_provision_keys. apply in_flat_map.
        exists (IVal v, provision). split; [exact Member|now left]. }
      specialize (Disjoint v Selected).
      apply Bool.orb_true_iff. destruct (Nat.lt_ge_cases v first).
      * left. now apply Nat.ltb_lt.
      * right. apply Nat.leb_le. lia.
Qed.

Theorem capture_counter_allocation : forall W C count s batch s',
  (forall g, es_next s <= g -> Fresh W g) ->
  EaBind.allocate_lambda_captures C count s = Ok (batch, s') ->
  fresh_n count s = Ok (batch, s') /\ es_next s' = es_next s + count /\ length batch = count /\
  Allocates (capture_allocation_scope W C) batch
    (extend_allocation_scope (capture_allocation_scope W C) batch).
Proof.
  intros W C count s batch s' Permitted Run.
  unfold EaBind.allocate_lambda_captures in Run.
  destruct (EaBind.lambda_capture_scope_free C (es_next s) count) eqn:Checked; [|discriminate].
  pose proof (proj1 (lambda_capture_scope_free_exact C (es_next s) count) Checked) as Clear.
  destruct (fresh_n_range count s) as (after & Exact & Counter).
  rewrite Run in Exact. inversion Exact; subst batch s'.
  split; [exact Run|]. split; [exact Counter|]. split; [apply length_seq|].
  split; [|reflexivity]. split; [apply seq_NoDup|].
  apply Forall_forall. intros g Issued. apply in_seq in Issued.
  split; [apply Permitted; lia|].
  intro Collision. exact (Clear g Collision Issued).
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

Theorem lambda_captures_from_counter : forall E F C parameters body s values s',
  (forall g, es_next s <= g -> Fresh (e_W E) g) ->
  let caps := EaBind.lambda_capture_names E F C parameters body in
  EaBind.allocate_lambda_captures C (length caps) s = Ok (values, s') ->
  fresh_n (length caps) s = Ok (values, s') /\
  es_next s' = es_next s + length caps /\
  TypesLambdaCaptures (e_W E) F C parameters body caps values.
Proof.
  intros E F C parameters body s values s' Permitted. cbn zeta.
  set (caps := EaBind.lambda_capture_names E F C parameters body). intro Run.
  destruct (lambda_capture_names_exact E F C parameters body) as [Distinct Names].
  destruct (capture_counter_allocation (e_W E) C (length caps) s values s' Permitted Run)
    as (Fresh & Counter & Length & Allocated).
  split; [exact Fresh|]. split; [exact Counter|].
  unfold TypesLambdaCaptures. split; [exact Names|].
  split; [exact Distinct|]. split; [exact Length|].
  eexists. exact Allocated.
Qed.
