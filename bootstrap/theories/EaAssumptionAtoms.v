(* Local identities for the absent atomic classes of an assumed item. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaCall NMap.
Import AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.

Definition scope_floor (bound : nat) (support : list nat) : nat :=
  fold_right (fun g next => Nat.max (S g) next) bound support.

Definition callee_leaves (Cs : cx) : list nat :=
  flat_map (fun q => gens (canon Cs (TAbs (fst q)))) (c_ty Cs).

Definition class_unread (C Cs : cx) (z : nat) : bool :=
  negb (has_bound_reading C Cs (TAbs z)).

Definition absent_classes (C Cs : cx) : list nat :=
  filter (class_unread C Cs) (sort_uniq (callee_leaves Cs)).

Definition assumption_atom_start (C Cs : cx) (bound : nat) : nat :=
  scope_floor bound (context_type_support C ++ context_type_support Cs).

Definition choose_assumption_atom (C Cs : cx) (bound : nat) : nat -> option nat :=
  match absent_classes C Cs with
  | [] => fun _ => None
  | classes =>
      let start := assumption_atom_start C Cs bound in
      fun r =>
        match canon Cs (TAbs r) with
        | TAbs z => if nmem z classes then Some (start + z) else None
        | _ => None
        end
  end.

Definition assumption_class_read (C Cs : cx) (atom : nat -> option nat) (z : nat) : ty :=
  match atom z with Some g => TAbs g | None => class_value C Cs z end.

Definition assumption_theta (W : world) (who : string) (C Cs : cx) (bound : nat) : er subst :=
  let atom := choose_assumption_atom C Cs bound in
  call_theta_with W who C Cs (assumption_class_read C Cs atom).

(* Errors name the earliest source member of a class. The spelling does
   not participate in comparing types. *)
Definition assumption_labels (W : world) (C Cs : cx) (bound : nat) : nmap string :=
  match absent_classes C Cs with
  | [] => NE
  | classes =>
      let start := assumption_atom_start C Cs bound in
      fold_left (fun labels z =>
        nset labels (start + z) (sym_name W (fold_right Nat.min z (class_of Cs (TAbs z))))) classes NE
  end.

Definition resolve_assumption_error {A} (W : world) (C Cs : cx) (bound : nat) (result : er A) : er A :=
  match result with
  | Ok x => Ok x
  | Err msg => Err (resolve_names (assumption_labels W C Cs bound) msg)
  end.
