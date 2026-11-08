(* ------------------------------------------------------------------------ *)
(* HTrie.v — finite maps keyed by machine integers, as binary tries.         *)
(*                                                                           *)
(* MossX's GC heap (boxes and GC arrays) and a GC array's elements live in   *)
(* these, not in primitive arrays: a primitive array's laws (get after set, *)
(* ...) are axioms of the kernel's specification (Corelib's ArrayAxioms),   *)
(* which the certificate does not assume.  A key is walked by its bits,     *)
(* low first, until what is left of it is 0 (at most 63 levels); the walk  *)
(* is the list `key a`, and the laws below hold of keys as lists, so none  *)
(* of them needs a fact about machine arithmetic.                           *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool PrimInt63 Uint63.
From NM Require ExecutionModel.
Import ListNotations.

Inductive trie (A : Type) : Type :=
| TE : trie A
| TN : trie A -> option A -> trie A -> trie A.
Arguments TE {A}.
Arguments TN {A}.

Local Open Scope uint63_scope.

Notation key_go := ExecutionModel.HeapAddress.key_go.
Notation key := ExecutionModel.HeapAddress.key.

(* ---- the maps, by lists of bits ---- *)

Fixpoint tget_l {A} (m : trie A) (k : list bool) : option A :=
  match m with
  | TE => None
  | TN l v r =>
      match k with
      | [] => v
      | true :: k' => tget_l r k'
      | false :: k' => tget_l l k'
      end
  end.

Fixpoint tset_l {A} (m : trie A) (k : list bool) (x : A) : trie A :=
  let '(l, v, r) := match m with TE => (TE, None, TE) | TN l v r => (l, v, r) end in
  match k with
  | [] => TN l (Some x) r
  | true :: k' => TN l v (tset_l r k' x)
  | false :: k' => TN (tset_l l k' x) v r
  end.

Lemma tget_l_set_same : forall A (m : trie A) k x, tget_l (tset_l m k x) k = Some x.
Proof.
  intros A m k. revert m. induction k as [|[|] k IH]; intros [|l v r] x; simpl; auto.
Qed.

Lemma tget_l_TE : forall A k, @tget_l A TE k = None.
Proof. reflexivity. Qed.

Lemma tget_l_set_other : forall A (m : trie A) k k' x,
  k <> k' -> tget_l (tset_l m k x) k' = tget_l m k'.
Proof.
  intros A m k. revert m. induction k as [|b k IH]; intros m k' x Hne.
  - destruct k' as [|b' k']; [exfalso; apply Hne; reflexivity |].
    destruct m as [|l v r]; destruct b'; reflexivity.
  - destruct k' as [|b' k'].
    + destruct m as [|l v r]; destruct b; reflexivity.
    + assert (Hk : b <> b' \/ k <> k').
      { destruct (bool_dec b b') as [<- | Hb]; [right | left; exact Hb].
        intros E; apply Hne; subst; reflexivity. }
      destruct m as [|l v r]; destruct b; destruct b'; simpl;
        try reflexivity;
        try (destruct Hk as [Hk | Hk]; [exfalso; apply Hk; reflexivity |]; rewrite IH; auto).
Qed.

(* ---- the same maps, walked by the key itself (what runs) ---- *)

Fixpoint tget_go {A} (fuel : nat) (m : trie A) (a : int) : option A :=
  match m with
  | TE => None
  | TN l v r =>
      match fuel with
      | O => v
      | S f => if a =? 0 then v
               else if a land 1 =? 1 then tget_go f r (a >> 1) else tget_go f l (a >> 1)
      end
  end.

Fixpoint tset_go {A} (fuel : nat) (m : trie A) (a : int) (x : A) : trie A :=
  let '(l, v, r) := match m with TE => (TE, None, TE) | TN l v r => (l, v, r) end in
  match fuel with
  | O => TN l (Some x) r
  | S f => if a =? 0 then TN l (Some x) r
           else if a land 1 =? 1 then TN l v (tset_go f r (a >> 1) x)
           else TN (tset_go f l (a >> 1) x) v r
  end.

Definition tget {A} (m : trie A) (a : int) : option A := tget_go 63 m a.
Definition tset {A} (m : trie A) (a : int) (x : A) : trie A := tset_go 63 m a x.

Lemma tget_go_l : forall A f (m : trie A) a, tget_go f m a = tget_l m (key_go f a).
Proof.
  intros A f. induction f as [|f IH]; intros [|l v r] a; simpl; auto.
  destruct (a =? 0); [reflexivity |].
  rewrite !IH. destruct (a land 1 =? 1); reflexivity.
Qed.

Lemma tset_go_l : forall A f (m : trie A) a x, tset_go f m a x = tset_l m (key_go f a) x.
Proof.
  intros A f. induction f as [|f IH]; intros m a x; destruct m as [|l v r]; simpl;
    try reflexivity;
    destruct (a =? 0); try reflexivity;
    destruct (a land 1 =? 1); rewrite IH; reflexivity.
Qed.

Lemma tget_key : forall A (m : trie A) a, tget m a = tget_l m (key a).
Proof. intros. apply tget_go_l. Qed.

Lemma tset_key : forall A (m : trie A) a x, tset m a x = tset_l m (key a) x.
Proof. intros. apply tset_go_l. Qed.
