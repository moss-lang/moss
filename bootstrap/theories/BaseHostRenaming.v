(* The concrete Base heap stores symbolic types inside values. Renaming
   those values keeps their addresses and the external byte state. *)
From Stdlib Require Import List Bool Arith PrimInt63 Uint63.
From NM Require Import Audit AuditBase RuntimeRenaming BaseRenaming.
From NM Require BaseHost BaseHostScalars MossX ExecutionModel HTrie NatTrie.
Import Audit.Semantics ListNotations.
Import MossX.
Import ExecutionModel (xvalue(..), xout(..), prim, xprog, base_primitive).
Local Opaque loop_fuel.

Section HostState.
Variable rename : nat -> nat.
Local Notation action := (base_renaming rename).

Fixpoint rename_stored (w : xvalue) : xvalue :=
  match w with
  | XVTag tag payload => XVTag (rename tag) (rename_stored payload)
  | XVStored value => XVStored (rename_value action value)
  | other => other
  end.

Definition rename_machine_out (outcome : xout) : xout :=
  match outcome with
  | XOk value => XOk (rename_stored value)
  | XRet value => XRet (rename_stored value)
  | other => other
  end.

Fixpoint rename_array_elements (elements : HTrie.trie xvalue) : HTrie.trie xvalue :=
  match elements with
  | HTrie.TE => HTrie.TE
  | HTrie.TN lower value upper =>
      HTrie.TN (rename_array_elements lower) (option_map rename_stored value) (rename_array_elements upper)
  end.

Definition rename_array (array : garr) : garr :=
  (rename_stored (fst array), rename_array_elements (snd array)).

Definition rename_cell (cell : hobj) : hobj :=
  match cell with
  | HNone => HNone
  | HBox value => HBox (rename_stored value)
  | HArr array count => HArr (rename_array array) count
  end.

Fixpoint rename_heap (heap : NatTrie.tmap hobj) : NatTrie.tmap hobj :=
  match heap with
  | NatTrie.TE => NatTrie.TE
  | NatTrie.TL address cell => NatTrie.TL address (option_map rename_cell cell)
  | NatTrie.TN lower cell upper => NatTrie.TN (rename_heap lower) (option_map rename_cell cell) (rename_heap upper)
  end.

Definition rename_state (state : mstate) : mstate :=
  MS (ms_mem state) (ms_pages state) (ms_out state) (ms_err state)
    (rename_heap (ms_heap state)) (ms_hnext state) (ms_fds state) (ms_nextfd state).

Definition rename_step (result : xout * mstate) : xout * mstate :=
  (rename_machine_out (fst result), rename_state (snd result)).

Lemma to_scalar_renamed : forall w,
  BaseHostScalars.to_scalar (rename_value action w) = rename_stored (BaseHostScalars.to_scalar w).
Proof.
  induction w as [|tag payload IH|fields|sort bits|address header]; cbn [rename_value rename_stored BaseHostScalars.to_scalar];
    try reflexivity.
  - now rewrite IH.
  - destruct sort as [|[|[|[|sort]]]]; reflexivity.
Qed.

Lemma from_value_renamed : forall w,
  BaseHostScalars.from_value (rename_stored w) = rename_value action (BaseHostScalars.from_value w).
Proof.
  induction w; cbn [rename_stored BaseHostScalars.from_value rename_value]; try reflexivity.
  - now rewrite IHw.
  - destruct (f64_bits f); reflexivity.
Qed.

Lemma from_signal_renamed : forall outcome,
  BaseHostScalars.from_signal (rename_machine_out outcome) =
  rename_signal action (BaseHostScalars.from_signal outcome).
Proof. destruct outcome; cbn [rename_machine_out BaseHostScalars.from_signal rename_signal]; now try rewrite from_value_renamed. Qed.

Lemma reference_argument_renamed : forall w,
  BaseHost.reference_argument (rename_value action w) =
  rename_stored (BaseHost.reference_argument w).
Proof.
  intros [|tag payload|fields|sort bits|address header]; try reflexivity.
  destruct payload as [|inner value|fields|sort bits|address [[kind held]|]]; reflexivity.
Qed.

Lemma scalar_arguments_renamed : forall values,
  map BaseHostScalars.to_scalar (map (rename_value action) values) =
  map rename_stored (map BaseHostScalars.to_scalar values).
Proof. intro values. rewrite !map_map. apply map_ext. apply to_scalar_renamed. Qed.

Lemma gc_arguments_renamed : forall primitive values,
  BaseHost.gc_arguments (rename_base_primitive rename primitive) (map (rename_value action) values) =
  map rename_stored (BaseHost.gc_arguments primitive values).
Proof.
  intros primitive values. destruct primitive; try apply scalar_arguments_renamed;
    destruct values as [|a [|b [|c [|d [|e [|f rest]]]]]];
    cbn [BaseHost.gc_arguments rename_base_primitive map rename_stored];
    try rewrite !reference_argument_renamed, !to_scalar_renamed; try reflexivity;
    rewrite ?reference_argument_renamed, ?to_scalar_renamed, ?scalar_arguments_renamed; reflexivity.
Qed.

Lemma array_elements_get_renamed : forall elements key,
  HTrie.tget_l (rename_array_elements elements) key =
  option_map rename_stored (HTrie.tget_l elements key).
Proof.
  induction elements as [|left IHl value right IHr]; intros [|[] rest]; cbn; auto.
Qed.

Lemma array_elements_set_renamed : forall key elements value,
  HTrie.tset_l (rename_array_elements elements) key (rename_stored value) =
  rename_array_elements (HTrie.tset_l elements key value).
Proof.
  induction key as [|[] rest IH]; intros [|left old right] value; cbn [HTrie.tset_l rename_array_elements];
    now try rewrite <- IH.
Qed.

Lemma garr_get_renamed : forall array index,
  garr_get (rename_array array) index = rename_stored (garr_get array index).
Proof.
  intros [default elements] index. unfold garr_get, rename_array.
  cbn [fst snd]. rewrite !HTrie.tget_key, array_elements_get_renamed.
  destruct (HTrie.tget_l elements _); reflexivity.
Qed.

Lemma garr_set_renamed : forall array index value,
  garr_set (rename_array array) index (rename_stored value) = rename_array (garr_set array index value).
Proof.
  intros [default elements] index value. unfold garr_set, rename_array.
  cbn [fst snd]. rewrite !HTrie.tset_key, array_elements_set_renamed. reflexivity.
Qed.

Lemma heap_lookup_renamed : forall heap address,
  NatTrie.tget (rename_heap heap) address = option_map rename_cell (NatTrie.tget heap address).
Proof.
  induction heap as [|key cell|left IHl cell right IHr]; intro address; cbn [NatTrie.tget rename_heap].
  - reflexivity.
  - destruct (Nat.eqb address key); reflexivity.
  - destruct (Nat.eqb address 0), (NatTrie.tleft address); auto.
Qed.

Lemma split_heap_leaf_renamed : forall key cell,
  NatTrie.split_leaf key (option_map rename_cell cell) =
  let '(lower, value, upper) := NatTrie.split_leaf key cell in
  (rename_heap lower, option_map rename_cell value, rename_heap upper).
Proof.
  intros key cell. unfold NatTrie.split_leaf. destruct (Nat.eqb key 0), (NatTrie.tleft key); reflexivity.
Qed.

Lemma heap_set_go_renamed : forall fuel heap address cell,
  NatTrie.tset_go fuel (rename_heap heap) address (rename_cell cell) =
  rename_heap (NatTrie.tset_go fuel heap address cell).
Proof.
  induction fuel as [|fuel IH]; intros [|key old|left old right] address cell;
    cbn [NatTrie.tset_go rename_heap]; try reflexivity.
  - destruct (Nat.eqb address key); [reflexivity|].
    rewrite split_heap_leaf_renamed. destruct (NatTrie.split_leaf key old) as [[lower value] upper].
    destruct (Nat.eqb address 0), (NatTrie.tleft address); cbn [rename_heap]; now try rewrite IH.
  - destruct (Nat.eqb address 0), (NatTrie.tleft address); cbn [rename_heap]; now try rewrite IH.
Qed.

Lemma heap_get_renamed : forall state address,
  heap_get (rename_state state) address = rename_cell (heap_get state address).
Proof.
  intros state address. unfold heap_get. cbn [rename_state ms_heap].
  rewrite heap_lookup_renamed. destruct (NatTrie.tget _ _); reflexivity.
Qed.

Lemma heap_set_renamed : forall state address cell,
  heap_set (rename_state state) address (rename_cell cell) = rename_state (heap_set state address cell).
Proof.
  intros state address cell. unfold heap_set, rename_state.
  cbn [ms_mem ms_pages ms_out ms_err ms_heap ms_hnext ms_fds ms_nextfd].
  unfold NatTrie.tset. now rewrite heap_set_go_renamed.
Qed.

Lemma heap_alloc_renamed : forall state cell,
  heap_alloc (rename_state state) (rename_cell cell) =
  option_map (fun '(address, next) => (address, rename_state next)) (heap_alloc state cell).
Proof.
  intros state cell. unfold heap_alloc.
  cbn [rename_state ms_hnext ms_heap]. rewrite heap_lookup_renamed.
  destruct (Nat.leb heap_max (ms_hnext state)), (NatTrie.tget (ms_heap state) (ms_hnext state)); try reflexivity.
  cbn [option_map rename_state ms_mem ms_pages ms_out ms_err ms_heap ms_hnext ms_fds ms_nextfd].
  unfold NatTrie.tset. now rewrite heap_set_go_renamed.
Qed.

Lemma ref_of_renamed : forall w, ref_of (rename_stored w) = ref_of w.
Proof. destruct w; try reflexivity. destruct w; reflexivity. Qed.

Lemma arr_of_renamed : forall state w,
  arr_of (rename_state state) (rename_stored w) =
  option_map (fun '(address, elements, count) => (address, rename_array elements, count)) (arr_of state w).
Proof.
  intros state w. unfold arr_of. rewrite ref_of_renamed.
  destruct (ref_of w) as [address|]; [|reflexivity].
  rewrite heap_get_renamed. destruct (heap_get state address); reflexivity.
Qed.

Lemma loop_map : forall A B (map_state : A -> B) fuel start stop step step' initial,
  (forall i state, step' i (map_state state) = option_map map_state (step i state)) ->
  int_loop fuel start stop step' (map_state initial) =
  option_map map_state (int_loop fuel start stop step initial).
Proof.
  intros A B map_state fuel. induction fuel as [|fuel IH]; intros start stop step step' initial Step;
    cbn [int_loop]; [reflexivity|].
  destruct (start <? stop)%uint63; [|reflexivity].
  rewrite Step. destruct (step start initial); cbn [option_map]; [apply IH; exact Step|reflexivity].
Qed.
Lemma array_fill_renamed : forall start count value elements,
  int_loop loop_fuel 0 count
    (fun i xs => Some (garr_set xs (start + i)%uint63 (rename_stored value))) (rename_array elements) =
  option_map rename_array
    (int_loop loop_fuel 0 count (fun i xs => Some (garr_set xs (start + i)%uint63 value)) elements).
Proof. intros. apply loop_map. intros. cbn [option_map]. now rewrite garr_set_renamed. Qed.

Lemma array_copy_renamed : forall start offset count source elements,
  int_loop loop_fuel 0 count
    (fun i xs => Some (garr_set xs (start + i)%uint63 (garr_get (rename_array source) (offset + i)%uint63)))
    (rename_array elements) =
  option_map rename_array
    (int_loop loop_fuel 0 count
      (fun i xs => Some (garr_set xs (start + i)%uint63 (garr_get source (offset + i)%uint63))) elements).
Proof.
  intros. apply loop_map. intros. cbn [option_map].
  now rewrite garr_get_renamed, garr_set_renamed.
Qed.

Lemma set_step_renamed : forall state address cell,
  (XOk XVUnit, heap_set (rename_state state) address (rename_cell cell)) =
  rename_step (XOk XVUnit, heap_set state address cell).
Proof. intros. now rewrite heap_set_renamed. Qed.

Theorem gc_step_renamed : forall primitive values state,
  Heap.is_gc primitive = true ->
  gc_prim (base_primitive (rename_base_primitive rename primitive))
    (map rename_stored values) (rename_state state) =
  rename_step (gc_prim (base_primitive primitive) values state).
Proof.
  intros primitive values state IsGC. destruct primitive;
    cbn [Heap.is_gc] in IsGC; try discriminate IsGC;
    destruct values as [|a [|b [|c [|d [|e [|f rest]]]]]];
    cbn [rename_base_primitive base_primitive map gc_prim]; try reflexivity.
  all: try (destruct b; reflexivity).
  all: try (destruct b; try reflexivity; destruct d; reflexivity).
  all: try (destruct b; try reflexivity; destruct d; try reflexivity; destruct e; reflexivity).
  - change (HBox (rename_stored a)) with (rename_cell (HBox a)).
    rewrite heap_alloc_renamed.
    destruct (heap_alloc state (HBox a)) as [[address next]|]; reflexivity.
  - rewrite ref_of_renamed. destruct (ref_of a); [|reflexivity].
    rewrite heap_get_renamed. destruct (heap_get state n); reflexivity.
  - rewrite ref_of_renamed. destruct (ref_of a); [|reflexivity].
    rewrite heap_get_renamed. destruct (heap_get state n); try reflexivity.
    apply (set_step_renamed state n (HBox b)).
  - destruct b; try reflexivity. cbn [gc_prim rename_stored].
    destruct (array_max <? i)%uint63; [reflexivity|].
    change (HArr (rename_stored a, HTrie.TE) i) with (rename_cell (HArr (a, HTrie.TE) i)).
    rewrite heap_alloc_renamed.
    destruct (heap_alloc state (HArr (a, HTrie.TE) i)) as [[address next]|]; reflexivity.
  - rewrite arr_of_renamed. destruct (arr_of state a) as [[[address elements] count]|]; reflexivity.
  - destruct b; try reflexivity. cbn [gc_prim rename_stored].
    rewrite arr_of_renamed. destruct (arr_of state a) as [[[address elements] count]|]; [|reflexivity].
    cbn [option_map]. destruct (i <? count)%uint63; [|reflexivity].
    now rewrite garr_get_renamed.
  - destruct b; try reflexivity. cbn [gc_prim rename_stored].
    rewrite arr_of_renamed. destruct (arr_of state a) as [[[address elements] count]|]; [|reflexivity].
    cbn [option_map]. destruct (i <? count)%uint63; [|reflexivity].
    rewrite garr_set_renamed. apply (set_step_renamed state address (HArr (garr_set elements i c) count)).
  - destruct b; try reflexivity. destruct d; try reflexivity. cbn [gc_prim rename_stored].
    rewrite arr_of_renamed. destruct (arr_of state a) as [[[address elements] count]|]; [|reflexivity].
    cbn [option_map]. destruct (count <? i + i0)%uint63; [reflexivity|].
    rewrite array_fill_renamed. destruct (int_loop _ _ _ _ _) as [filled|]; [|reflexivity].
    apply (set_step_renamed state address (HArr filled count)).
  - destruct b; try reflexivity. destruct d; try reflexivity. destruct e; try reflexivity.
    cbn [gc_prim rename_stored]. rewrite !arr_of_renamed.
    destruct (arr_of state a) as [[[address elements] count]|]; [|reflexivity].
    destruct (arr_of state c) as [[[source_address source] source_count]|]; [|reflexivity].
    cbn [option_map]. destruct ((count <? i + i1)%uint63 || (source_count <? i0 + i1)%uint63); [reflexivity|].
    rewrite array_copy_renamed. destruct (int_loop _ _ _ _ _) as [copied|]; [|reflexivity].
    apply (set_step_renamed state address (HArr copied count)).
Qed.
Theorem raw_gc_renamed : forall P P' primitive values state,
  Heap.is_gc primitive = true ->
  BaseHost.primitive P' (rename_base_operation rename (encode primitive))
    (map (rename_value action) values) (rename_state state) =
  let '(out, next) := BaseHost.primitive P (encode primitive) values state in
  (rename_signal action out, rename_state next).
Proof.
  intros P P' primitive values state IsGC.
  assert (RenamedGC : Heap.is_gc (rename_base_primitive rename primitive) = true).
  { destruct primitive; exact IsGC. }
  unfold BaseHost.primitive, BaseHost.primitive_in, BaseHost.primitive_decoded.
  rewrite rename_base_encoding, !BaseHost.decode_host_encode, !BaseHost.gc_run by assumption.
  rewrite gc_arguments_renamed, gc_step_renamed by assumption.
  destruct (gc_prim _ _ _) as [out next].
  cbn [rename_step fst snd]. now rewrite from_signal_renamed.
Qed.
End HostState.
