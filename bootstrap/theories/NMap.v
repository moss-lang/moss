(* ------------------------------------------------------------------------ *)
(* NMap.v — finite maps keyed by nat, as binary tries.                       *)
(*                                                                           *)
(* Keys are nats, which extract to OCaml ints (ExtrOcamlNatInt); the trie    *)
(* walks a key with div2 and minus only, both extracted natively, so a      *)
(* lookup is O(log k) where an association list is O(n).                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Arith Lia.
Import ListNotations.

Inductive nmap (A : Type) : Type :=
| NE : nmap A
| NN : nmap A -> option A -> nmap A -> nmap A.
Arguments NE {A}.
Arguments NN {A}.

(* key 0 is the root; key k+1 goes left if k is even, right if odd, with
   key k/2 below. *)
Definition is_even_nat (k : nat) : bool := Nat.eqb (k - 2 * Nat.div2 k) 0.

Fixpoint nget {A} (m : nmap A) (k : nat) {struct m} : option A :=
  match m with
  | NE => None
  | NN l v r =>
      match k with
      | 0 => v
      | S k' => if is_even_nat k' then nget l (Nat.div2 k') else nget r (Nat.div2 k')
      end
  end.

(* Insertion recurses on a bound (the key's bit length is at most the key). *)
Fixpoint nset_go {A} (fuel : nat) (m : nmap A) (k : nat) (x : A) : nmap A :=
  match fuel with
  | 0 => m
  | S f =>
      let '(l, v, r) := match m with NE => (NE, None, NE) | NN l v r => (l, v, r) end in
      match k with
      | 0 => NN l (Some x) r
      | S k' =>
          if is_even_nat k' then NN (nset_go f l (Nat.div2 k') x) v r
          else NN l v (nset_go f r (Nat.div2 k') x)
      end
  end.

(* A key k is at depth at most k + 1, so S k levels always suffice; every
   key that is an OCaml int is at depth at most 64, so this is what 64
   levels did on every key below 2^62 (Track A: with a fixed 64, a key of
   2^64 or more was silently dropped, which no soundness theorem about the
   elaborator's tables could rule out). *)
Definition nset {A} (m : nmap A) (k : nat) (x : A) : nmap A := nset_go (S k) m k x.
