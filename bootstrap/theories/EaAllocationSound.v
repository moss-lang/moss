(* The executable counter supplies a consecutive range of unused identities. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase.
From NM Require EaFunctor AllocationFacts.
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
