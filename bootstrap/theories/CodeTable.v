(* First-match lookup tables for code rows. Small symbol ranges use an
   array under extraction. Larger ranges use a sparse trie. *)
From Stdlib Require Import List Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit NatTrie.
Import AuditBase.Semantics ListNotations.

Definition index_rows {A : Type} (rows : list (nat * A)) : tmap A :=
  fold_right (fun '(key, value) index => tset index key value) TE rows.

Lemma index_rows_lookup : forall A (rows : list (nat * A)) key,
  tget (index_rows rows) key = Elaboration.nlook key rows.
Proof.
  intros A rows key. induction rows as [|[entry value] rows IH]; [reflexivity|].
  unfold index_rows in IH.
  cbn [index_rows fold_right Elaboration.nlook]. now rewrite tget_tset, IH.
Qed.

Inductive slots (A : Type) := Slots : list (option A) -> slots A.
Arguments Slots {A} _.

Definition dense_get {A} (cells : slots A) (key : nat) : option A :=
  let '(Slots values) := cells in
  match nth_error values key with Some value => value | None => None end.

Inductive table (A : Type) :=
| Dense : slots A -> table A
| Sparse : tmap A -> table A.
Arguments Dense {A} _.
Arguments Sparse {A} _.

Definition get {A} (index : table A) : nat -> option A :=
  match index with Dense cells => dense_get cells | Sparse rows => tget rows end.

Definition last_key {A} (rows : list (nat * A)) : nat :=
  fold_right (fun row next => Nat.max (fst row) next) 0 rows.

Definition build {A} (rows : list (nat * A)) : table A :=
  let index := index_rows rows in
  let last := last_key rows in
  if Nat.leb last 1048575
  then Dense (Slots (map (tget index) (seq 0 (S last))))
  else Sparse index.

Lemma above_last_key : forall A (rows : list (nat * A)) key,
  last_key rows < key -> Elaboration.nlook key rows = None.
Proof.
  intros A rows key. induction rows as [|[entry value] rows IH]; [reflexivity|].
  cbn [last_key fold_right fst Elaboration.nlook]. intros Above.
  change (Nat.max entry (last_key rows) < key) in Above.
  destruct (Nat.eqb key entry) eqn:Equal.
  - apply Nat.eqb_eq in Equal. subst entry. pose proof (Nat.le_max_l key (last_key rows)). lia.
  - apply IH. pose proof (Nat.le_max_r entry (last_key rows)). lia.
Qed.

Lemma nth_error_seq_lt : forall count first offset,
  offset < count -> nth_error (seq first count) offset = Some (first + offset).
Proof.
  induction count as [|count IH]; intros first [|offset] Bound; try lia.
  - cbn [seq nth_error]. now rewrite Nat.add_0_r.
  - cbn [seq nth_error]. rewrite IH by lia. f_equal. lia.
Qed.

Theorem get_build : forall A (rows : list (nat * A)) key,
  get (build rows) key = Elaboration.nlook key rows.
Proof.
  intros A rows key. unfold build.
  destruct (Nat.leb (last_key rows) 1048575); cbn [get dense_get].
  - rewrite nth_error_map.
    destruct (Nat.lt_ge_cases key (S (last_key rows))) as [Within|Above].
    + rewrite nth_error_seq_lt by exact Within. apply index_rows_lookup.
    + assert (Missing : nth_error (seq 0 (S (last_key rows))) key = None).
      { apply nth_error_None. now rewrite seq_length. }
      rewrite Missing, above_last_key by lia. reflexivity.
  - apply index_rows_lookup.
Qed.

Lemma nlook_map_values : forall A B (f : A -> B) rows key,
  Elaboration.nlook key (map (fun row => (fst row, f (snd row))) rows) =
  option_map f (Elaboration.nlook key rows).
Proof.
  intros A B f rows key. induction rows as [|[entry value] rows IH]; [reflexivity|].
  cbn [map Elaboration.nlook fst snd]. destruct (Nat.eqb key entry); [reflexivity|exact IH].
Qed.
