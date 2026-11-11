(* Scoped registration of generated function and functor bodies. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase NMap.
From NM Require AllocationFacts CodeTable WorldSound EaMain EaControlSound EaPlatform.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Definition code_in (table : code_table) : Type :=
  match table with FunctionTable => expr | FunctorTable => functor_code end.

Definition write_code (table : code_table) : code_slot table -> code_in table -> M unit :=
  match table with FunctionTable => write_fn_code | FunctorTable => write_fun_code end.
Arguments write_code {table} _ _.

Definition emit_code (table : code_table) : code_in table -> M (code_slot table) :=
  match table with FunctionTable => emit_fn_code | FunctorTable => emit_fun_code end.

Definition read_code (s : est) (table : code_table) (n : nat) : option (code_in table) :=
  match table with
  | FunctionTable => nget (es_fns s) n
  | FunctorTable => nget (es_funs s) n
  end.

Definition origin_in (table : code_table) : Type :=
  match table with FunctionTable => subst | FunctorTable => nat * nat end.

Definition read_origin (s : est) (table : code_table) (n : nat) : option (origin_in table) :=
  match table with
  | FunctionTable => option_map c_sub (nget (es_lamcx s) n)
  | FunctorTable => option_map fst (find (fun q => Nat.eqb n (slot_index (snd q))) (es_link s))
  end.

Definition VacantCode (s : est) (n : nat) : Prop :=
  forall table, read_code s table n = None.

Definition CodeTablesBound (s : est) : Prop :=
  forall n, es_next s <= n -> VacantCode s n.

Definition CodeTablesExtend (before after : est) : Prop :=
  forall table n code, read_code before table n = Some code -> read_code after table n = Some code.

Definition code_counter_scope (W : world) (start : nat) (s : est) : allocation_scope nat :=
  AllocationScope (Fresh W) (seq start (es_next s - start)) [].

Lemma code_tables_initial : forall start, CodeTablesBound (init_state start).
Proof. intros start n _ []; apply WorldSound.nget_NE. Qed.

Lemma code_counter_increments : forall start next,
  start <= next ->
  seq start (S next - start) = seq start (next - start) ++ [next].
Proof.
  intros start next Bounded.
  replace (S next - start) with (next - start + 1) by lia.
  rewrite seq_app. cbn [seq]. replace (start + (next - start)) with next by lia.
  reflexivity.
Qed.

Theorem reserve_code_spec : forall W start table s,
  start <= es_next s ->
  (forall n, start <= n -> Fresh W n) ->
  CodeTablesBound s ->
  exists slot s',
    reserve_code table s = Ok (slot, s') /\
    slot_index slot = es_next s /\ es_next s' = S (es_next s) /\
    Allocates (code_counter_scope W start s) [slot_index slot] (code_counter_scope W start s') /\
    (forall table' n, read_code s' table' n = read_code s table' n) /\
    VacantCode s' (slot_index slot) /\ CodeTablesBound s'.
Proof.
  intros W start table [next fns funs fwd wrap link const unsupported lamcx pins names]
    Bounded WorldBound Tables.
  exists (CodeSlot next), (Est (S next) fns funs fwd wrap link const unsupported lamcx pins names).
  split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|]. split.
  - change (Allocates
      (AllocationScope (Fresh W) (seq start (next - start)) []) [next]
      (AllocationScope (Fresh W) (seq start (S next - start)) [])).
    rewrite code_counter_increments by exact Bounded.
    apply (AllocationFacts.allocate_range
      (AllocationScope (Fresh W) (seq start (next - start)) []) next 1).
    + intros n AtLeast. apply WorldBound. cbn [es_next] in Bounded. lia.
    + intros n Present. cbn [scope_allocated scope_support] in Present.
      rewrite app_nil_r in Present. apply in_seq in Present.
      cbn [es_next] in Bounded. lia.
  - assert (Same : forall table' n, read_code (Est (S next) fns funs fwd wrap link const unsupported lamcx pins names)
      table' n = read_code (Est next fns funs fwd wrap link const unsupported lamcx pins names) table' n)
      by (intros [] n; reflexivity).
    split; [exact Same|]. split.
    + intro k. rewrite Same. apply Tables. apply Nat.le_refl.
    + intros n AtLeast k. rewrite Same. apply Tables. cbn [es_next] in *. lia.
Qed.

Theorem write_code_spec : forall table (slot : code_slot table) code s,
  slot_index slot < es_next s ->
  CodeTablesBound s ->
  exists s',
    write_code slot code s = Ok (tt, s') /\
    es_next s' = es_next s /\ CodeTablesBound s' /\
    read_code s' table (slot_index slot) = Some code /\
    (forall table' n, table' <> table \/ n <> slot_index slot ->
      read_code s' table' n = read_code s table' n).
Proof.
  intros [] [slot] code [next fns funs fwd wrap link const unsupported lamcx pins names] Bounded Tables.
  - exists (Est next (nset fns slot code) funs fwd wrap link const unsupported lamcx pins names).
    split; [reflexivity|]. split; [reflexivity|].
    assert (Other : forall table' n, table' <> FunctionTable \/ n <> slot ->
      read_code (Est next (nset fns slot code) funs fwd wrap link const unsupported lamcx pins names) table' n =
      read_code (Est next fns funs fwd wrap link const unsupported lamcx pins names) table' n).
    { intros [] n [Kind|Different]; try contradiction; try reflexivity.
      cbn [read_code es_fns]. apply WorldSound.nget_nset_other. exact Different. }
    split.
    + intros n Beyond table'. rewrite Other; [apply Tables; exact Beyond|].
      right. cbn [es_next slot_index] in *. lia.
    + split; [apply WorldSound.nget_nset_same|exact Other].
  - exists (Est next fns (nset funs slot code) fwd wrap link const unsupported lamcx pins names).
    split; [reflexivity|]. split; [reflexivity|].
    assert (Other : forall table' n, table' <> FunctorTable \/ n <> slot ->
      read_code (Est next fns (nset funs slot code) fwd wrap link const unsupported lamcx pins names) table' n =
      read_code (Est next fns funs fwd wrap link const unsupported lamcx pins names) table' n).
    { intros [] n [Kind|Different]; try contradiction; try reflexivity.
      cbn [read_code es_funs]. apply WorldSound.nget_nset_other. exact Different. }
    split.
    + intros n Beyond table'. rewrite Other; [apply Tables; exact Beyond|].
      right. cbn [es_next slot_index] in *. lia.
    + split; [apply WorldSound.nget_nset_same|exact Other].
Qed.

Lemma vacant_slot_preserves_old_code : forall before after table slot,
  VacantCode before slot ->
  (forall table' n, table' <> table \/ n <> slot -> read_code after table' n = read_code before table' n) ->
  CodeTablesExtend before after.
Proof.
  intros before after table slot Vacant Same table' n code Present.
  destruct (Nat.eq_dec n slot) as [->|Different].
  - rewrite Vacant in Present. discriminate.
  - now rewrite (Same table' n (or_intror Different)).
Qed.

Theorem write_reserved_code_spec : forall table (slot : code_slot table) code s,
  slot_index slot < es_next s -> VacantCode s (slot_index slot) -> CodeTablesBound s ->
  exists s',
    write_code slot code s = Ok (tt, s') /\
    es_next s' = es_next s /\ CodeTablesBound s' /\
    read_code s' table (slot_index slot) = Some code /\ CodeTablesExtend s s'.
Proof.
  intros table slot code s Bounded Vacant Tables.
  destruct (write_code_spec table slot code s Bounded Tables)
    as (s' & Run & Next & Bound & Stored & Other).
  exists s'. repeat split; try assumption.
  eapply vacant_slot_preserves_old_code; eassumption.
Qed.

Theorem emit_code_spec : forall W start table (code : code_in table) s,
  start <= es_next s ->
  (forall n, start <= n -> Fresh W n) ->
  CodeTablesBound s ->
  exists slot s',
    emit_code table code s = Ok (slot, s') /\
    Allocates (code_counter_scope W start s) [slot_index slot] (code_counter_scope W start s') /\
    VacantCode s (slot_index slot) /\ CodeTablesBound s' /\
    read_code s' table (slot_index slot) = Some code /\ CodeTablesExtend s s'.
Proof.
  intros W start table code s Start WorldBound Tables.
  destruct (reserve_code_spec W start table s Start WorldBound Tables)
    as (slot & middle & Reserve & Index & Next & Allocation & Unchanged & Vacant & Bounded).
  assert (CanWrite : slot_index slot < es_next middle) by (rewrite Index, Next; lia).
  destruct (write_reserved_code_spec table slot code middle CanWrite Vacant Bounded)
    as (s' & Write & Next' & Bounded' & Stored & Extend).
  exists slot, s'. split.
  - destruct table; cbn [emit_code write_code] in *; unfold emit_fn_code, emit_fun_code, mbind;
      rewrite Reserve, Write; reflexivity.
  - split.
    + unfold code_counter_scope in *. now rewrite Next'.
    + split; [rewrite Index; apply Tables; lia|].
      split; [exact Bounded'|]. split; [exact Stored|].
      intros table' n old Present. apply Extend. now rewrite Unchanged.
Qed.

Lemma collect_above : forall A (table : nmap A) top number acc,
  top < number ->
  nlook number (EaMain.collect table top acc) = nlook number acc.
Proof.
  intros A table top. induction top as [|top IH]; intros number acc Above; cbn [EaMain.collect].
  - destruct (nget table 0); [|reflexivity].
    cbn [nlook]. destruct (Nat.eqb_spec number 0); [lia|reflexivity].
  - rewrite IH by lia. destruct (nget table (S top)); [|reflexivity].
    cbn [nlook]. destruct (Nat.eqb_spec number (S top)); [lia|reflexivity].
Qed.

Lemma collect_within : forall A (table : nmap A) top number acc,
  number <= top ->
  nlook number (EaMain.collect table top acc) =
  match nget table number with Some code => Some code | None => nlook number acc end.
Proof.
  intros A table top. induction top as [|top IH]; intros number acc Within.
  - assert (number = 0) by lia. subst number. cbn [EaMain.collect].
    now destruct (nget table 0).
  - cbn [EaMain.collect]. destruct (Nat.eq_dec number (S top)) as [->|Different].
    + rewrite collect_above by lia. destruct (nget table (S top)); [|reflexivity].
      cbn [nlook]. now rewrite Nat.eqb_refl.
    + rewrite IH by lia. destruct (nget table number); [reflexivity|].
      destruct (nget table (S top)); [|reflexivity].
      cbn [nlook]. destruct (Nat.eqb_spec number (S top)); [contradiction|reflexivity].
Qed.

Lemma collect_code_table : forall A (table : nmap A) top,
  (forall number, top < number -> nget table number = None) ->
  forall number, nlook number (EaMain.collect table top []) = nget table number.
Proof.
  intros A table top Bound number. destruct (le_lt_dec number top) as [Within|Above].
  - rewrite collect_within by exact Within. now destruct (nget table number).
  - rewrite collect_above by exact Above. symmetry. exact (Bound number Above).
Qed.

Lemma successful_check_state : forall (b : bool) message s value s',
  check b message s = Ok (value, s') -> s' = s.
Proof. intros [] message s value s' Run; [now injection Run|discriminate]. Qed.

Theorem assemble_collects_code : forall E bound entry before P after,
  EaMain.assemble E bound entry before = Ok (P, after) ->
  program_functions P = EaMain.collect (es_fns after) (es_next after) [] /\
  program_functors P = EaMain.collect (es_funs after) (es_next after) [] /\
  program_origins P = EaMain.assembled_origins after.
Proof.
  intros E bound entry before P after Run. unfold EaMain.assemble in Run.
  destruct (EaPlatform.bool_syms E) as [[boolean truth] falsity].
  do 6 (apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run)).
  apply EaControlSound.mbind_ok in Run as ([context needs] & ?state & ?step & Run).
  repeat apply EaControlSound.mbind_ok in Run as (?value & ?state & ?step & Run).
  injection Run as <- <-.
  match goal with
  | Snapshot : get ?state = Ok (?saved, ?after_get),
    Check : (if ?b then ret tt else fail ?message) ?after_get = Ok (?u, ?after_check) |- _ =>
      rewrite (successful_check_state b message after_get u after_check Check);
      unfold get in Snapshot; injection Snapshot as <- <-
  end.
  repeat split; reflexivity.
Qed.

Theorem assembled_code_lookup : forall E bound entry before P after,
  EaMain.assemble E bound entry before = Ok (P, after) -> CodeTablesBound after ->
  (forall n, body_of P n = read_code after FunctionTable n) /\
  (forall n, funbody_of P n = read_code after FunctorTable n).
Proof.
  intros E bound entry before P after Run Bound.
  destruct (assemble_collects_code E bound entry before P after Run) as (Functions & Functors & _).
  unfold body_of, funbody_of. rewrite Functions, Functors.
  split; apply collect_code_table; intros n Above; [apply (Bound n ltac:(lia) FunctionTable)|
    apply (Bound n ltac:(lia) FunctorTable)].
Qed.

Theorem assembled_lambda_section : forall E bound entry before P after pid,
  EaMain.assemble E bound entry before = Ok (P, after) ->
  pid < es_next after ->
  nlook pid (lambda_sections (program_origins P)) = option_map c_sub (nget (es_lamcx after) pid).
Proof.
  intros E bound entry before P after pid Run Within.
  destruct (assemble_collects_code E bound entry before P after Run) as (_ & _ & Origins).
  rewrite Origins. cbn [EaMain.assembled_origins lambda_sections].
  rewrite CodeTable.nlook_map_values, collect_within by lia.
  now destruct (nget (es_lamcx after) pid).
Qed.

Theorem assembled_link_origin : forall E bound entry before P after h,
  EaMain.assemble E bound entry before = Ok (P, after) ->
  nlook h (linked_origins (program_origins P)) =
    option_map fst (find (fun q => Nat.eqb h (slot_index (snd q))) (es_link after)).
Proof.
  intros E bound entry before P after h Run.
  destruct (assemble_collects_code E bound entry before P after Run) as (_ & _ & Origins).
  rewrite Origins. cbn [EaMain.assembled_origins linked_origins].
  induction (es_link after) as [|[origin slot] rest IH]; [reflexivity|].
  cbn [map nlook find fst snd].
  destruct (Nat.eqb h (slot_index slot)); [reflexivity|exact IH].
Qed.
