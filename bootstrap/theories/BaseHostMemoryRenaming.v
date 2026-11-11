(* Memory and WASI operations carry the symbolic heap through their byte
   and file effects. Their byte inputs and byte results are independent
   of its symbolic contents. *)
From Stdlib Require Import List Bool PrimInt63 Uint63.
From NM Require Import BaseHostRenaming.
From NM Require Import MossX.
From NM Require ExecutionModel.
Import ListNotations.
Import ExecutionModel (xp_read).
Local Open Scope uint63_scope.
Local Opaque loop_fuel.

Section Memory.
Variable rename : nat -> nat.
Local Notation move := (rename_state rename).

Definition rename_count_state (result : int * mstate) : int * mstate :=
  let '(count, state) := result in (count, move state).

Definition rename_state_count (result : mstate * int) : mstate * int :=
  let '(state, count) := result in (move state, count).

Definition rename_state_bytes_count (result : mstate * list int * int) : mstate * list int * int :=
  let '(state, bytes, count) := result in (move state, bytes, count).

Lemma bounds_renamed : forall state address count,
  in_bounds (move state) address count = in_bounds state address count.
Proof. reflexivity. Qed.

Lemma get_byte_renamed : forall state address,
  get_byte (move state) address = get_byte state address.
Proof. reflexivity. Qed.

Lemma set_byte_renamed : forall state address byte,
  set_byte (move state) address byte = move (set_byte state address byte).
Proof. reflexivity. Qed.

Lemma load_le_renamed : forall count state address,
  load_le (move state) address count = load_le state address count.
Proof.
  induction count as [|count IH]; intros; cbn [load_le]; [reflexivity|].
  now rewrite get_byte_renamed, IH.
Qed.

Lemma store_le_renamed : forall count state address value,
  store_le (move state) address value count = move (store_le state address value count).
Proof.
  induction count as [|count IH]; intros; cbn [store_le]; [reflexivity|].
  now rewrite set_byte_renamed, IH.
Qed.

Lemma load32_renamed : forall state address,
  load32 (move state) address = load32 state address.
Proof.
  intros. unfold load32. rewrite bounds_renamed.
  destruct (in_bounds state address 4); [now rewrite load_le_renamed|reflexivity].
Qed.

Lemma store32_renamed : forall state address value,
  store32 (move state) address value = option_map move (store32 state address value).
Proof.
  intros. unfold store32. rewrite bounds_renamed.
  destruct (in_bounds state address 4); [now rewrite store_le_renamed|reflexivity].
Qed.

Lemma mem_grow_renamed : forall state delta,
  mem_grow (move state) delta = rename_count_state (mem_grow state delta).
Proof.
  intros. unfold mem_grow. cbn [rename_state ms_pages ms_mem].
  destruct (max_pages <? ms_pages state + delta); [reflexivity|].
  destruct (int_loop _ _ _ _ _); reflexivity.
Qed.


Lemma copy_bytes_renamed : forall state dst src count,
  copy_bytes (move state) dst src count = move (copy_bytes state dst src count).
Proof.
  intros state dst src count.
  assert (Loop : forall offset,
    int_loop loop_fuel 0 count
      (fun i s => Some (set_byte s (dst + offset i) (get_byte s (src + offset i)))) (move state) =
    option_map move (int_loop loop_fuel 0 count
      (fun i s => Some (set_byte s (dst + offset i) (get_byte s (src + offset i)))) state)).
  { intros. apply loop_map. intros. now rewrite get_byte_renamed, set_byte_renamed. }
  unfold copy_bytes. destruct (dst <=? src).
  - rewrite (Loop (fun i => i)). destruct (int_loop _ _ _ _ _); reflexivity.
  - rewrite (Loop (fun i => count - 1 - i)). destruct (int_loop _ _ _ _ _); reflexivity.
Qed.

Lemma fill_bytes_renamed : forall state dst byte count,
  fill_bytes (move state) dst byte count = move (fill_bytes state dst byte count).
Proof.
  intros state dst byte count. unfold fill_bytes.
  rewrite (@loop_map _ _ move _ _ _ (fun i s => Some (set_byte s (dst + i) byte))); [destruct (int_loop _ _ _ _ _); reflexivity|].
  intros. now rewrite set_byte_renamed.
Qed.

Lemma emit_bytes_renamed : forall fd state address count,
  emit_bytes fd (move state) address count = option_map move (emit_bytes fd state address count).
Proof.
  intros fd state address count. unfold emit_bytes. rewrite bounds_renamed.
  destruct (in_bounds state address count); [|reflexivity].
  apply loop_map. intros. rewrite get_byte_renamed. destruct (fd =? 2); reflexivity.
Qed.

Lemma fd_write_renamed : forall state fd iovs count written,
  fd_write (move state) fd iovs count written = option_map rename_count_state (fd_write state fd iovs count written).
Proof.
  intros state fd iovs count written. unfold fd_write.
  destruct (negb ((fd =? 1) || (fd =? 2))); [reflexivity|].
  change (move state, 0) with (rename_state_count (state, 0)).
  match goal with
  | |- context [int_loop _ _ _ ?step (state, 0)] =>
      rewrite (@loop_map _ _ rename_state_count _ _ _ step)
  end.
  - destruct (int_loop _ _ _ _ _) as [[next total]|]; cbn [option_map rename_state_count]; [|reflexivity].
    rewrite store32_renamed. destruct (store32 next written (w32 total)); reflexivity.
  - intros i [current total]. cbn [rename_state_count]. rewrite !load32_renamed.
    destruct (load32 current (iovs + 8 * i)) as [base|],
      (load32 current (iovs + 8 * i + 4)) as [len|]; try reflexivity.
    rewrite emit_bytes_renamed. destruct (emit_bytes fd current base len); reflexivity.
Qed.

Lemma store_bytes_renamed : forall bytes state address,
  store_bytes (move state) address bytes = move (store_bytes state address bytes).
Proof.
  induction bytes as [|byte bytes IH]; intros; cbn [store_bytes]; [reflexivity|].
  now rewrite set_byte_renamed, IH.
Qed.

Lemma args_sizes_get_renamed : forall P state argc size,
  args_sizes_get P (move state) argc size = option_map move (args_sizes_get P state argc size).
Proof.
  intros. unfold args_sizes_get. rewrite store32_renamed.
  destruct (store32 state argc _); cbn [option_map]; [apply store32_renamed|reflexivity].
Qed.

Lemma args_get_renamed : forall args state pointers bytes,
  args_get_go args (move state) pointers bytes = option_map move (args_get_go args state pointers bytes).
Proof.
  induction args as [|arg args IH]; intros; cbn [args_get_go]; [reflexivity|].
  rewrite bounds_renamed.
  destruct (in_bounds state bytes _); [|reflexivity].
  rewrite store32_renamed. destruct (store32 state pointers bytes); cbn [option_map]; [|reflexivity].
  rewrite store_bytes_renamed. apply IH.
Qed.

Lemma read_bytes_renamed : forall state address count,
  read_bytes (move state) address count = read_bytes state address count.
Proof.
  reflexivity.
Qed.

Lemma set_fds_renamed : forall state fds next,
  set_fds (move state) fds next = move (set_fds state fds next).
Proof. reflexivity. Qed.

Lemma path_open_renamed : forall P state path count result,
  path_open P (move state) path count result =
  option_map rename_count_state (path_open P state path count result).
Proof.
  intros. unfold path_open. rewrite read_bytes_renamed.
  destruct (read_bytes state path count); [|reflexivity].
  destruct (xp_read P l); [|reflexivity].
  cbn [rename_state ms_nextfd ms_fds]. rewrite set_fds_renamed, store32_renamed.
  destruct (store32 _ _ _); reflexivity.
Qed.

Lemma fill_from_renamed : forall state address count bytes,
  fill_from (move state) address count bytes =
  option_map rename_state_bytes_count (fill_from state address count bytes).
Proof.
  intros. unfold fill_from. rewrite bounds_renamed.
  destruct (in_bounds state address count); [|reflexivity].
  change (move state, bytes, 0) with (rename_state_bytes_count (state, bytes, 0)).
  apply loop_map. intros i [[current [|byte rest]] copied]; cbn [rename_state_bytes_count];
    [reflexivity|now rewrite set_byte_renamed].
Qed.

Lemma fd_read_renamed : forall state fd iovs count read,
  fd_read (move state) fd iovs count read = option_map rename_count_state (fd_read state fd iovs count read).
Proof.
  intros state fd iovs count read. unfold fd_read. cbn [rename_state ms_fds].
  destruct (lookup_fd fd (ms_fds state)) as [bytes|]; [|reflexivity].
  change (move state, bytes, 0) with (rename_state_bytes_count (state, bytes, 0)).
  match goal with
  | |- context [int_loop _ _ _ ?step (state, bytes, 0)] =>
      rewrite (@loop_map _ _ rename_state_bytes_count _ _ _ step)
  end.
  - destruct (int_loop _ _ _ _ _) as [[[next rest] total]|]; cbn [option_map rename_state_bytes_count]; [|reflexivity].
    cbn [rename_state ms_fds ms_nextfd]. rewrite set_fds_renamed, store32_renamed.
    destruct (store32 _ _ _); reflexivity.
  - intros i [[current rest] total]. cbn [rename_state_bytes_count]. rewrite !load32_renamed.
    destruct (load32 current (iovs + 8 * i)) as [base|],
      (load32 current (iovs + 8 * i + 4)) as [len|]; try reflexivity.
    rewrite fill_from_renamed. destruct (fill_from current base len rest) as [[[next rest'] read']|]; reflexivity.
Qed.

Lemma fd_close_renamed : forall state fd,
  fd_close (move state) fd = rename_count_state (fd_close state fd).
Proof.
  intros. unfold fd_close. cbn [rename_state ms_fds ms_nextfd].
  destruct (lookup_fd fd (ms_fds state)); reflexivity.
Qed.

Lemma load_op_renamed : forall kind state address,
  load_op kind (move state) address = load_op kind state address.
Proof.
  intros. unfold load_op. rewrite bounds_renamed.
  destruct (in_bounds state address _); [now rewrite load_le_renamed|reflexivity].
Qed.

Lemma store_op_renamed : forall kind state address value,
  store_op kind (move state) address value = option_map move (store_op kind state address value).
Proof.
  intros. unfold store_op. rewrite bounds_renamed.
  destruct (in_bounds state address _); [now rewrite store_le_renamed|reflexivity].
Qed.
Lemma i64_load_renamed : forall kind state address,
  i64_load kind (move state) address = i64_load kind state address.
Proof.
  intros. unfold i64_load. rewrite bounds_renamed.
  destruct (in_bounds state address _); [|reflexivity].
  destruct kind; reflexivity.
Qed.

Lemma i64_store_renamed : forall kind state address value,
  i64_store kind (move state) address value = option_map move (i64_store kind state address value).
Proof.
  intros. unfold i64_store. rewrite bounds_renamed.
  destruct (in_bounds state address _); [|reflexivity].
  destruct kind; cbn [option_map]; now rewrite !store_le_renamed.
Qed.
End Memory.
