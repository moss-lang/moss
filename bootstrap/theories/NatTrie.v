(* ------------------------------------------------------------------------ *)
(* NatTrie.v — binary tries keyed by nat, with their get/set laws.          *)
(*                                                                           *)
(* The tables of the proved passes on Audit programs (AuditPrune.v's read  *)
(* sets, AuditCapTrim.v's).  run/Extract.v realizes the key                *)
(* steps `tleft` and `tdown` on OCaml ints.                                 *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* key 0 is the root; key k+1 goes left if k is even, right if odd, with
   key k/2 below (NMap.v's layout; the insertion here is bounded by the key
   itself, so the laws hold for every key). *)
Inductive tmap (A : Type) : Type :=
| TE : tmap A
| TN : tmap A -> option A -> tmap A -> tmap A.
Arguments TE {A}.
Arguments TN {A}.

Definition evenb (k : nat) : bool := Nat.eqb (k - 2 * Nat.div2 k) 0.

(* The step from a nonzero key to its child: which side, and the child key.
   run/Extract.v realizes both on OCaml ints (k = 0 included). *)
Definition tleft (k : nat) : bool := evenb (k - 1).
Definition tdown (k : nat) : nat := Nat.div2 (k - 1).

Fixpoint tget {A} (m : tmap A) (k : nat) {struct m} : option A :=
  match m with
  | TE => None
  | TN l v r =>
      if Nat.eqb k 0 then v else if tleft k then tget l (tdown k) else tget r (tdown k)
  end.

Fixpoint tset_go {A} (fuel : nat) (m : tmap A) (k : nat) (x : A) : tmap A :=
  match fuel with
  | 0 => m
  | S f =>
      match m with
      | TE =>
          if Nat.eqb k 0 then TN TE (Some x) TE
          else if tleft k then TN (tset_go f TE (tdown k) x) None TE
          else TN TE None (tset_go f TE (tdown k) x)
      | TN l v r =>
          if Nat.eqb k 0 then TN l (Some x) r
          else if tleft k then TN (tset_go f l (tdown k) x) v r
          else TN l v (tset_go f r (tdown k) x)
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

Lemma tget_tset_go : forall A f (m : tmap A) k x j, k < f ->
  tget (tset_go f m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof.
  induction f as [|f IH]; intros m k x j Hk; [lia|].
  assert (Hdiv : forall k', S k' < S f -> Nat.div2 k' < f)
    by (intros k' H'; pose proof (Nat.div2_odd k'); destruct (Nat.odd k'); simpl in *; lia).
  assert (Hpar : forall j' k', evenb j' = evenb k' ->
             Nat.eqb (Nat.div2 j') (Nat.div2 k') = Nat.eqb j' k').
  { intros j' k' E. simpl.
    destruct (evenb_split k') as [[Ek Hk']|[Ek Hk']];
    destruct (evenb_split j') as [[Ej Hj']|[Ej Hj']]; rewrite Ek, Ej in E; try discriminate;
    destruct (Nat.eqb (Nat.div2 j') (Nat.div2 k')) eqn:E1, (Nat.eqb j' k') eqn:E2; auto;
    first [apply Nat.eqb_eq in E1 | apply Nat.eqb_neq in E1];
    first [apply Nat.eqb_eq in E2 | apply Nat.eqb_neq in E2]; try lia;
    exfalso; apply E1; subst; reflexivity. }
  assert (Hne : forall j' k', evenb j' <> evenb k' -> Nat.eqb j' k' = false).
  { intros j' k' E. apply Nat.eqb_neq. intros ->. auto. }
  destruct m as [|l v r]; destruct k as [|k']; destruct j as [|j']; simpl;
    rewrite ?tleft_S, ?tdown_S; simpl; rewrite ?tleft_S, ?tdown_S; auto.
  all: try (destruct (evenb j'); reflexivity).
  all: try (destruct (evenb k'); reflexivity).
  all: destruct (evenb k') eqn:Ek; simpl;
       rewrite ?IH by (apply Hdiv; exact Hk); simpl; rewrite ?tleft_S, ?tdown_S;
       destruct (evenb j') eqn:Ej;
       first [rewrite Hpar by congruence; reflexivity
             | rewrite Hne by congruence; reflexivity].
Qed.

Lemma tget_tset : forall A (m : tmap A) k x j,
  tget (tset m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof. intros. apply tget_tset_go. lia. Qed.
