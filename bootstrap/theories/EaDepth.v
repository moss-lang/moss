(* Syntactic depths used by executable dependency-reader fuel selection. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit EaTypes.
Import ListNotations Audit.Input.

Definition largest {A} (size : A -> nat) (xs : list A) : nat :=
  fold_right (fun x rest => Nat.max (size x) rest) 0 xs.

Fixpoint spec_depth (syntax : spec) : nat :=
  match syntax with
  | Spec _ _ application => S (match application with
      | Some bindings => largest (fun b => spec_depth (snd b)) bindings
      | None => 0
      end)
  end.

Fixpoint type_depth (syntax : aty) : nat :=
  S (match syntax with
     | ATRec fields => largest (fun field => type_depth (snd field)) fields
     | ATUnion alternatives => largest type_depth alternatives
     | ATRef _ application => largest (fun b => spec_depth (snd b)) (bracket_list application)
     | _ => 0
     end).
