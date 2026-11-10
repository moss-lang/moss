(* ------------------------------------------------------------------------ *)
(* NatTrie.v — binary tries keyed by nat, with their get/set laws.          *)
(*                                                                           *)
(* The tables of the proved passes on Audit programs (ProgramPrune.v's read  *)
(* sets, ProgramCapTrim.v's).  run/Extract.v realizes the key                *)
(* steps `tleft` and `tdown` on OCaml ints.                                 *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* Key 0 is at a branch's root. Key k+1 goes left if k is even and
   right if k is odd, with key k/2 below. A singleton keeps its remaining
   key in a leaf, so insertion into an empty subtree needs no spine.
   The value is stored as an option so lookup can return it directly. *)
Inductive tmap (A : Type) : Type :=
| TE : tmap A
| TL : nat -> option A -> tmap A
| TN : tmap A -> option A -> tmap A -> tmap A.
Arguments TE {A}.
Arguments TL {A}.
Arguments TN {A}.

Definition evenb (k : nat) : bool := Nat.eqb (k - 2 * Nat.div2 k) 0.

(* The step from a nonzero key to its child: which side, and the child key.
   run/Extract.v realizes both on OCaml ints (k = 0 included). *)
Definition tleft (k : nat) : bool := evenb (k - 1).
Definition tdown (k : nat) : nat := Nat.div2 (k - 1).

Fixpoint tget {A} (m : tmap A) (k : nat) {struct m} : option A :=
  match m with
  | TE => None
  | TL j v => if Nat.eqb k j then v else None
  | TN l v r =>
      if Nat.eqb k 0 then v else if tleft k then tget l (tdown k) else tget r (tdown k)
  end.

(* Expose one branch when a singleton's key differs from the inserted key. *)
Definition split_leaf {A} (j : nat) (v : option A) : tmap A * option A * tmap A :=
  if Nat.eqb j 0 then (TE, v, TE)
  else if tleft j then (TL (tdown j) v, None, TE)
       else (TE, None, TL (tdown j) v).

Fixpoint tset_go {A} (fuel : nat) (m : tmap A) (k : nat) (x : A) : tmap A :=
  match fuel with
  | 0 => m
  | S f =>
      let insert l v r :=
        if Nat.eqb k 0 then TN l (Some x) r
        else if tleft k then TN (tset_go f l (tdown k) x) v r
             else TN l v (tset_go f r (tdown k) x) in
      match m with
      | TE => TL k (Some x)
      | TL j v =>
          if Nat.eqb k j then TL k (Some x)
          else let '(l, o, r) := split_leaf j v in insert l o r
      | TN l v r => insert l v r
      end
  end.

Lemma tleft_S : forall k, tleft (S k) = evenb k.
Proof. intro k. unfold tleft. simpl. rewrite Nat.sub_0_r. reflexivity. Qed.
Lemma tdown_S : forall k, tdown (S k) = Nat.div2 k.
Proof. intro k. unfold tdown. simpl. rewrite Nat.sub_0_r. reflexivity. Qed.

Definition tset {A} (m : tmap A) (k : nat) (x : A) : tmap A := tset_go (S k) m k x.

Lemma evenb_split : forall k,
  (evenb k = true /\ k = 2 * Nat.div2 k) \/ (evenb k = false /\ k = S (2 * Nat.div2 k)).
Proof.
  intro k. unfold evenb. pose proof (Nat.div2_odd k) as H.
  destruct (Nat.odd k); simpl in H.
  - right. split; [|lia]. apply Nat.eqb_neq. lia.
  - left. split; [|lia]. apply Nat.eqb_eq. lia.
Qed.

Lemma tget_empty : forall A k, @tget A TE k = None.
Proof. reflexivity. Qed.

Local Lemma div2_eqb_same_side : forall j k, evenb j = evenb k ->
  Nat.eqb (Nat.div2 j) (Nat.div2 k) = Nat.eqb j k.
Proof.
  intros j k E. destruct (evenb_split k) as [[Ek Hk]|[Ek Hk]];
    destruct (evenb_split j) as [[Ej Hj]|[Ej Hj]]; rewrite Ek, Ej in E; try discriminate;
    destruct (Nat.eqb (Nat.div2 j) (Nat.div2 k)) eqn:E1, (Nat.eqb j k) eqn:E2; auto;
    first [apply Nat.eqb_eq in E1 | apply Nat.eqb_neq in E1];
    first [apply Nat.eqb_eq in E2 | apply Nat.eqb_neq in E2]; try lia;
    exfalso; apply E1; subst; reflexivity.
Qed.

Local Lemma tget_split_leaf : forall A k (x : option A) j,
  let '(l, v, r) := split_leaf k x in tget (TN l v r) j = tget (TL k x) j.
Proof.
  intros A [|k] x [|j]; cbn [split_leaf tget Nat.eqb].
  - reflexivity.
  - destruct (tleft (S j)); reflexivity.
  - rewrite tleft_S. destruct (evenb k); reflexivity.
  - rewrite tleft_S, tdown_S.
    destruct (evenb k) eqn:Ek; cbn [tget Nat.eqb]; rewrite tleft_S, tdown_S;
      destruct (evenb j) eqn:Ej; cbn [tget].
    all: try (rewrite div2_eqb_same_side by congruence; reflexivity).
    all: assert (Nat.eqb j k = false) by (apply Nat.eqb_neq; congruence).
    all: rewrite H; reflexivity.
Qed.

(* A common statement for the two insertion cases. *)
Local Definition set_branch {A} (descend : tmap A -> nat -> tmap A) (k : nat) (x : A)
    (l : tmap A) (v : option A) (r : tmap A) : tmap A :=
  if Nat.eqb k 0 then TN l (Some x) r
  else if tleft k then TN (descend l (tdown k)) v r
       else TN l v (descend r (tdown k)).

Local Lemma tget_set_branch : forall A (descend : tmap A -> nat -> tmap A) k (x : A) l v r j,
  (k <> 0 -> forall sub q, tget (descend sub (tdown k)) q =
    if Nat.eqb q (tdown k) then Some x else tget sub q) ->
  tget (set_branch descend k x l v r) j =
    if Nat.eqb j k then Some x else tget (TN l v r) j.
Proof.
  intros A descend [|k] x l v r [|j] D; cbn [set_branch tget Nat.eqb].
  - reflexivity.
  - destruct (tleft (S j)); reflexivity.
  - destruct (tleft (S k)); reflexivity.
  - specialize (D ltac:(discriminate)).
    rewrite ?tleft_S, ?tdown_S in *.
    destruct (evenb k) eqn:Ek; cbn [tget Nat.eqb]; rewrite ?tleft_S, ?tdown_S;
      destruct (evenb j) eqn:Ej; rewrite ?D.
    all: try (rewrite div2_eqb_same_side by congruence; reflexivity).
    all: assert (Nat.eqb j k = false) by (apply Nat.eqb_neq; congruence).
    all: rewrite H; reflexivity.
Qed.

Lemma tget_tset_go : forall A f (m : tmap A) k x j, k < f ->
  tget (tset_go f m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof.
  induction f as [|f IH]; intros m k x j Hk; [lia|].
  assert (D : k <> 0 -> forall sub q, tget (tset_go f sub (tdown k) x) q =
    if Nat.eqb q (tdown k) then Some x else tget sub q).
  { intros K sub q. apply IH. destruct k as [|k]; [contradiction|].
    rewrite tdown_S. pose proof (Nat.div2_odd k). destruct (Nat.odd k); cbn in *; lia. }
  destruct m as [|u v|l v r]; cbn [tset_go tget].
  - destruct (Nat.eqb j k); reflexivity.
  - destruct (Nat.eqb k u) eqn:E.
    + apply Nat.eqb_eq in E. subst u. cbn [tget]. destruct (Nat.eqb j k); reflexivity.
    + pose proof (@tget_split_leaf A u v j) as Split.
      destruct (split_leaf u v) as [[l o] r].
      change (tget (set_branch (fun sub key => tset_go f sub key x) k x l o r) j =
        if Nat.eqb j k then Some x else tget (TL u v) j).
      rewrite tget_set_branch by exact D. rewrite Split. reflexivity.
  - change (tget (set_branch (fun sub key => tset_go f sub key x) k x l v r) j =
      if Nat.eqb j k then Some x else tget (TN l v r) j).
    apply tget_set_branch. exact D.
Qed.

Lemma tget_tset : forall A (m : tmap A) k x j,
  tget (tset m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof. intros. apply tget_tset_go. lia. Qed.
