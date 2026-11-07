From Stdlib Require Import List Arith.
From NM Require Import Elab.
Lemma nassoc_aset : forall V n k (v : V) l,
  nassoc n (aset Nat.eqb k v l) = if Nat.eqb n k then Some v else nassoc n l.
Proof.
  intros V n k v l. induction l as [|[k' v'] r IH]; cbn.
  - destruct (Nat.eqb n k); reflexivity.
  - destruct (Nat.eqb_spec k k') as [<-|Hne]; cbn.
    + destruct (Nat.eqb n k); reflexivity.
    + destruct (Nat.eqb_spec n k') as [->|]; [|exact IH].
      destruct (Nat.eqb_spec k' k); [congruence | reflexivity].
Qed.
