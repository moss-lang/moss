(* Base's concrete primitive implementation. MossX provides the machine
   and heap operations. Stored Audit values cross the heap intact. *)
From Stdlib Require Import List Bool Arith PrimInt63 Uint63.
From NM Require Import Audit AuditBase BaseFacts BaseHostScalars MossX.
From NM Require ExecutionModel HTrie NatTrie PublicOperationFacts.
Import Audit.Semantics ListNotations.
Import ExecutionModel (xvalue(..), xprog(..), prim, base_primitive).

Local Opaque loop_fuel.

Definition reference_argument (v : value) : xvalue :=
  match v with
  | VTag tag (VRef address None) => XVTag tag (XVRef address)
  | _ => XVUnit
  end.

Definition gc_arguments (p : wasm_prim) (ws : list value) : list xvalue :=
  match p, ws with
  | PBox _, [v] => [XVStored v]
  | PArrNew _, [v; count] => [XVStored v; to_scalar count]
  | PDeref, [r] | PArrLen, [r] => [reference_argument r]
  | PRefSet, [r; v] => [reference_argument r; XVStored v]
  | PArrGet, [r; i] => [reference_argument r; to_scalar i]
  | PArrSet, [r; i; v] => [reference_argument r; to_scalar i; XVStored v]
  | PArrFill, [r; i; v; count] => [reference_argument r; to_scalar i; XVStored v; to_scalar count]
  | PArrCopy, [r; i; source; j; count] =>
      [reference_argument r; to_scalar i; reference_argument source; to_scalar j; to_scalar count]
  | _, _ => map to_scalar ws
  end.

Definition decode_host_operation (operation : op) : option (wasm_prim * prim) :=
  option_map (fun p => (p, base_primitive p)) (decode operation).
Definition decode_host := decode_host_operation.

Definition primitive_step (P : xprog) (p : wasm_prim) (concrete : prim)
    (values : list value) (state : mstate) : signal * mstate :=
  let '(out, next) := run_prim_fast P concrete (gc_arguments p values) state in
  (from_signal out, next).

Definition with_memory_arguments (p : wasm_prim) (values : list value)
    (step : list value -> mstate -> signal * mstate) (state : mstate) : signal * mstate :=
  let '(site, arguments) := BaseCall.split_memory_arguments p values in
  in_memory (scratch_memory_key state site) (step arguments) state.

Definition primitive_decoded (P : xprog) (p : wasm_prim) (concrete : prim)
    (values : list value) (state : mstate) : signal * mstate :=
  with_memory_arguments p values (primitive_step P p concrete) state.

Definition primitive_in (P : xprog) (operation : op) (values : list value)
    (state : mstate) : signal * mstate :=
  match decode_host operation with
  | Some (p, concrete) => primitive_decoded P p concrete values state
  | None => (UnsupportedOperation (unsupported_code operation), state)
  end.

Definition primitive (P : program_view) := primitive_in (legacy_view P).

Definition scalar_i32 (w : value) : option int :=
  match w with VScalar 0 bits => Some (w32 (Uint63.of_Z bits)) | _ => None end.

Definition address_i32 (w : value) : option int :=
  match w with VTag _ payload => scalar_i32 payload | _ => scalar_i32 w end.

Definition i32_result (bits : int) : signal := Normal (VScalar 0 (Uint63.to_Z bits)).

(* The common integer operations can read and return Audit scalars
   directly. The ordinary machine step handles all other operands. *)
Definition direct_i32 (p : wasm_prim) (values : list value) (state : mstate) : option (signal * mstate) :=
  match p, values with
  | PI32Bin kind, [a; b] =>
      match scalar_i32 a, scalar_i32 b with
      | Some x, Some y =>
          Some (match i32_bin kind x y with inl r => i32_result r | inr trap => Trap trap end, state)
      | _, _ => None
      end
  | PI32Cmp kind, [a; b] =>
      match scalar_i32 a, scalar_i32 b with
      | Some x, Some y => Some (i32_result (i32_cmp kind x y), state)
      | _, _ => None
      end
  | PI32Un kind, [a] =>
      match scalar_i32 a with Some x => Some (i32_result (i32_un kind x), state) | _ => None end
  | PLoad kind, [a] =>
      match address_i32 a with
      | Some x => Some (match load_op kind state x with
                        | Some r => i32_result r | None => Trap trap_oob end, state)
      | None => None
      end
  | PStore kind, [a; b] =>
      match address_i32 a, address_i32 b with
      | Some x, Some y =>
          Some (match store_op kind state x y with
                | Some next => (Normal VUnit, keep_heap state next)
                | None => (Trap trap_oob, state)
                end)
      | _, _ => None
      end
  | _, _ => None
  end.

Lemma scalar_i32_argument : forall w i, scalar_i32 w = Some i -> to_scalar w = XVI32 i.
Proof.
  intros [|tag payload|fields|[|sort] bits|address header] i Found; try discriminate.
  now injection Found as <-.
Qed.

Lemma address_i32_argument : forall w i, address_i32 w = Some i -> unaddr (to_scalar w) = XVI32 i.
Proof.
  intros w i Found. destruct w; try now rewrite (scalar_i32_argument _ _ Found).
  cbn [address_i32 to_scalar] in *. now rewrite (scalar_i32_argument _ _ Found).
Qed.

Lemma keep_heap_same : forall state, keep_heap state state = state.
Proof. destruct state; reflexivity. Qed.

Lemma direct_i32_agrees : forall P p values state result,
  direct_i32 p values state = Some result ->
  primitive_step P p (base_primitive p) values state = result.
Proof.
  intros P p values state result Direct.
  destruct p; destruct values as [|a [|b [|extra rest]]];
    cbn [direct_i32] in Direct; try discriminate Direct;
    unfold primitive_step; cbn [base_primitive gc_arguments map run_prim_fast numeric_prim].
  all: repeat match type of Direct with
    | context [scalar_i32 ?w] =>
        let bits := fresh "bits" in let read := fresh "Read" in
        destruct (scalar_i32 w) as [bits|] eqn:read; try discriminate Direct;
        rewrite (scalar_i32_argument _ _ read)
    end.
  all: try (injection Direct as <-; cbn [run_prim0]; try reflexivity;
    match goal with |- context [i32_bin ?kind ?left ?right] =>
      destruct (i32_bin kind left right); reflexivity end).
  all: unfold run_prim; cbn [is_gc_prim mem_args map].
  all: repeat match type of Direct with
    | context [address_i32 ?w] =>
        let bits := fresh "bits" in let read := fresh "Read" in
        destruct (address_i32 w) as [bits|] eqn:read; try discriminate Direct;
        rewrite (address_i32_argument _ _ read)
    end.
  all: injection Direct as <-; cbn [run_prim0].
  - destruct (load_op _ _ _); cbn [okv]; now rewrite keep_heap_same.
  - unfold unit_or_trap. destruct (store_op _ _ _ _); [reflexivity|now rewrite keep_heap_same].
Qed.

Definition direct_step (P : xprog) (p : wasm_prim) (concrete : prim)
    (values : list value) (state : mstate) : signal * mstate :=
  match direct_i32 p values state with
  | Some result => result
  | None => primitive_step P p concrete values state
  end.

Lemma direct_step_agrees : forall P p values state,
  direct_step P p (base_primitive p) values state =
  primitive_step P p (base_primitive p) values state.
Proof.
  intros. unfold direct_step. destruct (direct_i32 p values state) eqn:Direct; [|reflexivity].
  symmetry. now apply direct_i32_agrees.
Qed.

Definition call_in_deferred (P : xprog) (operation : op) (site : unit -> option ty)
    (values : list value) (state : mstate) : signal * mstate :=
  match decode_host operation with
  | Some (p, concrete) =>
      let arguments := BaseCall.primitive_arguments p values in
      let memory := if BaseCall.memory_operation p then scratch_memory_key state (site tt) else None in
      let '(out, next) := in_memory memory (direct_step P p concrete arguments) state in
      (DeferredBaseCall.result_at_site p site out, next)
  | None => (UnsupportedOperation (unsupported_code operation), state)
  end.

Theorem call_in_deferred_agrees : forall P operation site values state,
  call_in_deferred (legacy_view P) operation site values state =
  BaseCall.call primitive P operation (site tt) values state.
Proof.
  intros. unfold call_in_deferred, BaseCall.call, primitive, primitive_in, decode_host, decode_host_operation.
  destruct (decode operation) as [p|]; [|destruct (site tt); reflexivity].
  cbn [option_map]. unfold primitive_decoded, with_memory_arguments, BaseCall.arguments_at_site.
  rewrite split_memory_arguments_at_site.
  assert (Memory : scratch_memory_key state (if BaseCall.memory_operation p then site tt else None) =
      (if BaseCall.memory_operation p then scratch_memory_key state (site tt) else None)).
  { destruct (BaseCall.memory_operation p); reflexivity. }
  rewrite Memory. unfold in_memory.
  destruct (if BaseCall.memory_operation p then scratch_memory_key state (site tt) else None);
    rewrite direct_step_agrees;
    destruct (primitive_step _ _ _ _ _) as [out next];
    now rewrite DeferredBaseCall.result_at_site_agrees.
Qed.

Definition call_in (P : xprog) (operation : op) (site : option ty)
    (values : list value) (state : mstate) : signal * mstate :=
  call_in_deferred P operation (fun _ => site) values state.

Theorem call_in_agrees : forall P operation site values state,
  call_in (legacy_view P) operation site values state =
  BaseCall.call primitive P operation site values state.
Proof. intros. exact (call_in_deferred_agrees P operation (fun _ => site) values state). Qed.

Definition stored_ok (Q : value -> Prop) (w : xvalue) : Prop :=
  match w with XVStored v => Q v | _ => False end.

Definition elements_ok (Q : value -> Prop) (xs : garr) : Prop :=
  stored_ok Q (fst xs) /\ forall key w, HTrie.tget_l (snd xs) key = Some w -> stored_ok Q w.

Definition cell_ok (o : hobj) (kind : nat) (Q : value -> Prop) : Prop :=
  match o with
  | HBox w => kind = 0 /\ stored_ok Q w
  | HArr xs _ => kind = 1 /\ elements_ok Q xs
  | HNone => False
  end.

Definition heap_ok (typing : Heap.hty) (state : mstate) : Prop :=
  forall a kind Q, typing a = Some (kind, Q) -> cell_ok (heap_get state a) kind Q.

Lemma elements_get : forall Q xs i, elements_ok Q xs -> stored_ok Q (garr_get xs i).
Proof.
  intros Q [default entries] i [Default Entries].
  unfold garr_get. cbn [fst snd]. rewrite HTrie.tget_key.
  destruct (HTrie.tget_l entries _) eqn:Found; [eapply Entries; eassumption|exact Default].
Qed.

Lemma elements_set : forall Q xs i w,
  elements_ok Q xs -> stored_ok Q w -> elements_ok Q (garr_set xs i w).
Proof.
  intros Q [default entries] i w [Default Entries] Typed.
  split; [exact Default|]. intros key value.
  unfold garr_set. cbn [fst snd]. rewrite HTrie.tset_key.
  destruct (list_eq_dec bool_dec (ExecutionModel.HeapAddress.key i) key) as [<-|Different].
  - rewrite HTrie.tget_l_set_same. intros [= <-]. exact Typed.
  - rewrite HTrie.tget_l_set_other by exact Different. apply Entries.
Qed.

Lemma loop_preserves : forall T (I : T -> Prop) fuel start stop step init final,
  int_loop fuel start stop step init = Some final -> I init ->
  (forall i before after, I before -> step i before = Some after -> I after) -> I final.
Proof.
  intros T I fuel. induction fuel as [|fuel IH]; intros start stop step init final Run Init Step;
    cbn [int_loop] in Run; [discriminate|].
  destruct (start <? stop)%uint63.
  - destruct (step start init) eqn:Next; [|discriminate].
    eapply IH; [exact Run|eapply Step; eassumption|exact Step].
  - injection Run as <-. exact Init.
Qed.

Lemma heap_set_ok : forall typing state a o kind Q,
  heap_ok typing state -> typing a = Some (kind, Q) -> cell_ok o kind Q ->
  heap_ok typing (heap_set state a o).
Proof.
  intros typing state a o kind Q Valid Old Cell other kind' Q' Lookup.
  unfold heap_get, heap_set. cbn [ms_heap]. rewrite NatTrie.tget_tset.
  destruct (Nat.eqb other a) eqn:Same.
  - apply Nat.eqb_eq in Same. subst other. rewrite Old in Lookup. injection Lookup as <- <-. exact Cell.
  - exact (Valid _ _ _ Lookup).
Qed.

Definition add_cell (typing : Heap.hty) (address kind : nat) (Q : value -> Prop) : Heap.hty :=
  fun a => if Nat.eqb a address then Some (kind, Q) else typing a.

Lemma heap_alloc_ok : forall typing state object a next kind Q,
  heap_ok typing state -> heap_alloc state object = Some (a, next) -> cell_ok object kind Q ->
  typing a = None /\ heap_ok (add_cell typing a kind Q) next /\
  Heap.hext typing (add_cell typing a kind Q).
Proof.
  intros typing state object a next kind Q Valid Alloc Cell. unfold heap_alloc in Alloc.
  destruct (Nat.leb heap_max (ms_hnext state)); [discriminate|].
  destruct (NatTrie.tget (ms_heap state) (ms_hnext state)) eqn:Free; [discriminate|].
  injection Alloc as <- <-.
  assert (Fresh : typing (ms_hnext state) = None).
  { destruct (typing (ms_hnext state)) as [[old_kind old_Q]|] eqn:Lookup; [|reflexivity].
    pose proof (Valid _ _ _ Lookup) as Old. unfold heap_get in Old. rewrite Free in Old. contradiction. }
  split; [exact Fresh|]. split.
  - intros other kind' Q' Lookup. unfold heap_get. cbn [ms_heap]. rewrite NatTrie.tget_tset.
    unfold add_cell in Lookup. destruct (Nat.eqb other (ms_hnext state)).
    + injection Lookup as <- <-. exact Cell.
    + exact (Valid _ _ _ Lookup).
  - intros other kind' Q' Lookup. unfold add_cell.
    destruct (Nat.eqb other (ms_hnext state)) eqn:Same; [|exact Lookup].
    apply Nat.eqb_eq in Same. subst other. rewrite Fresh in Lookup. discriminate.
Qed.

Lemma reference_cell : forall typing state kind Q r,
  heap_ok typing state -> Heap.href typing kind r Q ->
  exists tag a, r = VTag tag (VRef a None) /\ typing a = Some (kind, Q)
    /\ cell_ok (heap_get state a) kind Q.
Proof. intros typing state kind Q r Valid (tag & a & -> & Lookup). exists tag, a. auto. Qed.

Lemma heap_weaken : forall typing weaker state,
  heap_ok typing state ->
  (forall a kind Q, weaker a = Some (kind, Q) ->
    exists Q0, typing a = Some (kind, Q0) /\ forall v, Q0 v -> Q v) ->
  heap_ok weaker state.
Proof.
  intros typing weaker state Valid Weak a kind Q Lookup.
  destruct (Weak _ _ _ Lookup) as (Q0 & Old & Implies).
  pose proof (Valid _ _ _ Old) as Typed.
  assert (Stored : forall w, stored_ok Q0 w -> stored_ok Q w).
  { destruct w; cbn; auto. }
  destruct (heap_get state a) as [|v|xs n]; cbn [cell_ok] in *; try contradiction.
  - destruct Typed as [-> V]. split; [reflexivity|auto].
  - destruct Typed as [-> [Default Entries]]. repeat split; eauto.
Qed.

Lemma empty_heap_ok : forall state, heap_ok (fun _ => None) state.
Proof. intros state a kind Q Impossible. discriminate. Qed.

Lemma concrete_is_gc : forall p, is_gc_prim (base_primitive p) = Heap.is_gc p.
Proof. destruct p; reflexivity. Qed.

Lemma decode_host_encode : forall p, decode_host (encode p) = Some (p, base_primitive p).
Proof. intros. unfold decode_host, decode_host_operation. now rewrite PublicOperationFacts.decode_encode. Qed.

Lemma primitive_step_keeps_heap : forall P p values state typing,
  Heap.is_gc p = false ->
  heap_ok typing state ->
  heap_ok typing (snd (primitive_step P p (base_primitive p) values state)).
Proof.
  intros P p values state typing NotGC Valid.
  unfold primitive_step. rewrite run_prim_fast_spec. unfold run_prim.
  rewrite concrete_is_gc, NotGC.
  destruct (run_prim0 _ _ _ _) as [out next]. exact Valid.
Qed.

Lemma primitive_keeps_heap : forall P operation ws state typing,
  (match decode operation with Some p => Heap.is_gc p = false | None => True end) ->
  heap_ok typing state ->
  heap_ok typing (snd (primitive P operation ws state)).
Proof.
  intros P operation ws state typing NotGC Valid.
  unfold primitive, primitive_in, primitive_decoded, decode_host, decode_host_operation, with_memory_arguments.
  destruct (decode operation) as [p|]; [|exact Valid].
  cbn [option_map]. destruct (BaseCall.split_memory_arguments p ws) as [site args].
  unfold in_memory. destruct (scratch_memory_key state site).
  - specialize (primitive_step_keeps_heap (legacy_view P) p args
        (with_linear_memory state (find_scratch_memory t (ms_scratch_memories state))
          (ms_scratch_memories state)) typing NotGC Valid) as Keep.
    destruct (primitive_step _ _ _ _ _) as [out next]. exact Keep.
  - now apply primitive_step_keeps_heap.
Qed.

Lemma gc_run : forall P p ws state, Heap.is_gc p = true ->
  run_prim_fast P (base_primitive p) ws state = gc_prim (base_primitive p) ws state.
Proof.
  intros. rewrite run_prim_fast_spec. unfold run_prim. rewrite concrete_is_gc, H.
  destruct p; try discriminate H; reflexivity.
Qed.

Lemma array_post : forall typing Q p w,
  (match p with PArrGet => Q w | PArrLen => exists bits, w = VScalar 0 bits | _ => w = VUnit end) ->
  (match p with PArrGet | PArrLen | PArrSet | PArrFill | PArrCopy => True | _ => False end) ->
  Heap.gc_post typing typing p Q w.
Proof. intros typing Q p w H Allowed. split; [intros ? ? ? E; exact E|]. destruct p; tauto. Qed.

Lemma primitive_gc : forall P p ws state typing Q,
  heap_ok typing state -> Heap.gc_pre typing p ws Q ->
  match primitive P (encode p) ws state with
  | (Normal value, next) =>
      exists extended, heap_ok extended next /\ Heap.gc_post typing extended p Q value
  | (Trap _, next) => heap_ok typing next
  | _ => False
  end.
Proof.
  intros P p ws state typing Q Valid Pre.
  assert (IsGC : Heap.is_gc p = true) by (destruct p; cbn in Pre; try contradiction; reflexivity).
  unfold primitive, primitive_in, primitive_decoded, with_memory_arguments, BaseCall.split_memory_arguments.
  rewrite decode_host_encode, (memory_operation_gc _ IsGC).
  cbn [scratch_memory_key in_memory]. unfold primitive_step. rewrite gc_run by exact IsGC.
  destruct p; cbn [Heap.gc_pre] in Pre; try contradiction.
  - destruct ws as [|v [|extra rest]]; try contradiction.
    cbn [gc_arguments base_primitive gc_prim].
    destruct (heap_alloc state (HBox (XVStored v))) as [[a next]|] eqn:Alloc; [|exact Valid].
    destruct (@heap_alloc_ok typing state _ a next 0 Q Valid Alloc (conj eq_refl Pre))
      as (Fresh & Typed & Extension).
    exists (add_cell typing a 0 Q). split; [exact Typed|]. split; [exact Extension|].
    exists a. cbn. unfold add_cell. now rewrite Nat.eqb_refl.
  - destruct ws as [|r [|extra rest]]; try contradiction.
    destruct (@reference_cell typing state 0 Q r Valid Pre) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument base_primitive gc_prim ref_of].
    destruct (heap_get state a) as [|w|xs count]; cbn [cell_ok] in Cell; try contradiction.
    + destruct w; cbn [stored_ok] in Cell; try tauto.
      exists typing. split; [exact Valid|]. split; [intros ? ? ? E; exact E|exact (proj2 Cell)].
    + discriminate (proj1 Cell).
  - destruct ws as [|r [|v [|extra rest]]]; try contradiction.
    destruct Pre as [Ref Typed].
    destruct (@reference_cell typing state 0 Q r Valid Ref) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument base_primitive gc_prim ref_of].
    destruct (heap_get state a) as [|w|xs count]; cbn [cell_ok] in Cell; try contradiction.
    + exists typing. split.
      * eapply heap_set_ok; [exact Valid|exact At|split; [reflexivity|exact Typed]].
      * split; [intros ? ? ? E; exact E|reflexivity].
    + discriminate (proj1 Cell).
  - destruct ws as [|v [|count [|extra rest]]]; try contradiction.
    all: try (destruct count as [|tag w|fields|[|sort] bits|a header]; try contradiction).
    cbn [gc_arguments to_scalar base_primitive gc_prim].
    destruct (array_max <? w32 (Uint63.of_Z bits))%uint63; [exact Valid|].
    destruct (heap_alloc state (HArr (XVStored v, HTrie.TE) (w32 (Uint63.of_Z bits))))
      as [[a next]|] eqn:Alloc; [|exact Valid].
    assert (Cell : cell_ok (HArr (XVStored v, HTrie.TE) (w32 (Uint63.of_Z bits))) 1 Q).
    { repeat split; [exact Pre|intros ? ? E; discriminate]. }
    destruct (@heap_alloc_ok typing state _ a next 1 Q Valid Alloc Cell) as (Fresh & Typed & Extension).
    exists (add_cell typing a 1 Q). split; [exact Typed|]. split; [exact Extension|].
    exists a. cbn. unfold add_cell. now rewrite Nat.eqb_refl.
  - destruct ws as [|r [|extra rest]]; try contradiction.
    destruct (@reference_cell typing state 1 Q r Valid Pre) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument base_primitive gc_prim arr_of ref_of].
    destruct (heap_get state a) as [|w|xs count]; cbn [cell_ok] in Cell; try contradiction.
    + discriminate (proj1 Cell).
    + exists typing. split; [exact Valid|]. apply array_post; [eexists; reflexivity|exact I].
  - destruct ws as [|r [|i [|extra rest]]]; try contradiction.
    all: try (destruct i as [|tag w|fields|[|sort] bits|a header]; try contradiction).
    destruct (@reference_cell typing state 1 Q r Valid Pre) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument to_scalar base_primitive gc_prim arr_of ref_of].
    destruct (heap_get state a) as [|w|xs count]; cbn [cell_ok] in Cell; try contradiction.
    + discriminate (proj1 Cell).
    + destruct (w32 (Uint63.of_Z bits) <? count)%uint63; [|exact Valid].
      pose proof (elements_get Q xs (w32 (Uint63.of_Z bits)) (proj2 Cell)) as Typed.
      destruct (garr_get xs _) as [|payload_tag w|i|f|fields|f|i|a'|array tau a'|w];
        cbn [stored_ok] in Typed; try contradiction.
      exists typing. split; [exact Valid|]. apply array_post; [exact Typed|exact I].
  - destruct ws as [|r [|i [|v [|extra rest]]]]; try contradiction.
    all: try (destruct i as [|tag w|fields|[|sort] bits|a header]; try contradiction).
    destruct Pre as [Ref Typed].
    destruct (@reference_cell typing state 1 Q r Valid Ref) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument to_scalar base_primitive gc_prim arr_of ref_of].
    destruct (heap_get state a) as [|w|xs count]; cbn [cell_ok] in Cell; try contradiction.
    + discriminate (proj1 Cell).
    + destruct (w32 (Uint63.of_Z bits) <? count)%uint63; [|exact Valid].
      exists typing. split.
      * eapply heap_set_ok; [exact Valid|exact At|]. split; [reflexivity|].
        apply elements_set; [exact (proj2 Cell)|exact Typed].
      * apply array_post; [reflexivity|exact I].
  - destruct ws as [|r [|i [|v [|count [|extra rest]]]]]; try contradiction.
    all: try (destruct i as [|tag w|fields|[|sort] bits|a header]; try contradiction).
    all: try (destruct count as [|tag w|fields|[|sort] len|a header]; try contradiction).
    destruct Pre as [Ref Typed].
    destruct (@reference_cell typing state 1 Q r Valid Ref) as (tag & a & -> & At & Cell).
    cbn [gc_arguments reference_argument to_scalar base_primitive gc_prim arr_of ref_of].
    destruct (heap_get state a) as [|w|xs size]; cbn [cell_ok] in Cell; try contradiction.
    + discriminate (proj1 Cell).
    + destruct (size <? w32 (Uint63.of_Z bits) + w32 (Uint63.of_Z len))%uint63; [exact Valid|].
      destruct (int_loop _ _ _ _ xs) as [ys|] eqn:Run; [|exact Valid].
      assert (Elements : elements_ok Q ys).
      { eapply loop_preserves; [exact Run|exact (proj2 Cell)|].
        intros j before after Before [= <-]. apply elements_set; [exact Before|exact Typed]. }
      exists typing. split.
      * eapply heap_set_ok; [exact Valid|exact At|split; [reflexivity|exact Elements]].
      * apply array_post; [reflexivity|exact I].
  - destruct ws as [|r [|i [|source [|j [|count [|extra rest]]]]]]; try contradiction.
    all: try (destruct i as [|tag w|fields|[|sort] at_bits|a header]; try contradiction).
    all: try (destruct j as [|tag w|fields|[|sort] start_bits|a header]; try contradiction).
    all: try (destruct count as [|tag w|fields|[|sort] len|a header]; try contradiction).
    destruct Pre as (Ref & Q2 & Source & Implies).
    destruct (@reference_cell typing state 1 Q r Valid Ref) as (tag & a & -> & At & Cell).
    destruct (@reference_cell typing state 1 Q2 source Valid Source) as (tag2 & a2 & -> & At2 & Cell2).
    cbn [gc_arguments reference_argument to_scalar base_primitive gc_prim arr_of ref_of].
    destruct (heap_get state a) as [|w|xs size]; cbn [cell_ok] in Cell; try contradiction;
      [discriminate (proj1 Cell)|].
    destruct (heap_get state a2) as [|w|ys size2]; cbn [cell_ok] in Cell2; try contradiction;
      [discriminate (proj1 Cell2)|].
    destruct ((size <? w32 (Uint63.of_Z at_bits) + w32 (Uint63.of_Z len))%uint63 ||
      (size2 <? w32 (Uint63.of_Z start_bits) + w32 (Uint63.of_Z len))%uint63); [exact Valid|].
    destruct (int_loop _ _ _ _ xs) as [zs|] eqn:Run; [|exact Valid].
    assert (Elements : elements_ok Q zs).
    { eapply loop_preserves; [exact Run|exact (proj2 Cell)|].
      intros offset before after Before [= <-]. apply elements_set; [exact Before|].
      set (index := (w32 (Uint63.of_Z start_bits) + offset)%uint63).
      pose proof (elements_get Q2 ys index (proj2 Cell2)) as Typed.
      destruct (garr_get ys index); cbn [stored_ok] in *; try contradiction. now apply Implies. }
    exists typing. split.
    + eapply heap_set_ok; [exact Valid|exact At|split; [reflexivity|exact Elements]].
    + apply array_post; [reflexivity|exact I].
Qed.

Lemma plain_arguments : forall p params result values,
  prim_sig p = Some (params, result) -> gc_arguments p values = map to_scalar values.
Proof. destruct p; intros; try reflexivity; discriminate. Qed.

Lemma method_arguments : forall p params result values,
  prim_msig p = Some (params, result) -> gc_arguments p values = map to_scalar values.
Proof. destruct p; intros; try reflexivity; discriminate. Qed.

Lemma memory_result : forall P p site values state shape,
  (forall st, AuditBase.BaseLaws.primitive_result P shape
    (fst (primitive_step (legacy_view P) p (base_primitive p) values st))) ->
  AuditBase.BaseLaws.primitive_result P shape
    (fst (primitive_decoded (legacy_view P) p (base_primitive p)
      (BaseCall.memory_arguments p site values) state)).
Proof.
  intros P p site values state shape Result.
  unfold primitive_decoded, with_memory_arguments.
  rewrite split_memory_arguments_at_site. unfold in_memory.
  destruct (scratch_memory_key _ _); [|apply Result].
  specialize (Result (with_linear_memory state
    (find_scratch_memory t (ms_scratch_memories state)) (ms_scratch_memories state))).
  destruct (primitive_step _ _ _ _ _); exact Result.
Qed.

Theorem primitive_laws : forall P,
  AuditBase.BaseLaws.laws mstate P (fun _ => true) primitive heap_ok.
Proof.
  intros P. repeat split.
  - intros p params result site values state _ Sig Args.
    unfold primitive, primitive_in. rewrite decode_host_encode.
    apply memory_result. intros st. unfold primitive_step. rewrite (plain_arguments _ _ _ _ Sig).
    exact (scalar_sound P _ _ _ _ _ Sig Args).
  - intros p params result site values state _ Sig Args.
    unfold primitive, primitive_in. rewrite decode_host_encode.
    apply memory_result. intros st. unfold primitive_step. rewrite (method_arguments _ _ _ _ Sig).
    exact (scalar_method_sound P _ _ _ _ _ Sig Args).
  - intros operation values state Decode _. unfold primitive, primitive_in, primitive_decoded, decode_host, decode_host_operation.
    now rewrite Decode.
  - apply heap_weaken.
  - apply empty_heap_ok.
  - intros operation values state typing _. apply primitive_keeps_heap.
  - intros p values state typing Q _. apply primitive_gc.
Qed.

Module Concrete <: AuditBase.HOST.
  Definition state := mstate.
  Definition primitive := primitive.
  Definition heap_ok := heap_ok.
End Concrete.

Module Public := AuditBase.Public Concrete.

Theorem default_compatible : forall program,
  Public.Link.Ready program -> Public.Compatible program Public.default_interface.
Proof.
  intros program Ready. split; [exact Ready|].
  split; [reflexivity|]. intros args read. apply primitive_laws.
Qed.

Theorem primitive_ignores_code : forall P P',
  view_bool P' = view_bool P ->
  view_true P' = view_true P ->
  view_false P' = view_false P ->
  view_arguments P' = view_arguments P ->
  view_read P' = view_read P ->
  forall operation values state, primitive P' operation values state = primitive P operation values state.
Proof.
  intros P P' Bool True False Args Read. unfold primitive, legacy_view.
  now rewrite Bool, True, False, Args, Read.
Qed.
