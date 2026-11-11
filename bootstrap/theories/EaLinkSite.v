(* A closure checks shared inputs using the target's fixed readings and
   one local atom for each remaining atomic class. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit EaBase EaAssumptionAtoms.
Import Audit.Semantics Audit.Semantics.Elaboration.

Definition site_classes (C : cx) (fixed : subst) : list nat :=
  filter (fun y => negb (is_some (lookup_subst y fixed))) (sort_uniq (callee_leaves C)).

Definition site_atom (bound : nat) (recorded : subst) (y : nat) : nat :=
  scope_floor bound (flat_map (fun q => gens (snd q)) recorded) + y.

Definition site_theta (bound : nat) (C : cx) (recorded private : subst) : subst :=
  let fixed := copy_fixed_readings C recorded in
  copy_subst fixed (site_classes C fixed) (site_atom bound (recorded ++ private)).
