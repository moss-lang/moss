(* XTag's type arguments use the context's solved merge. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel NMap Sx Elab.
Import ListNotations.

Example tag_theta_merged_argument : forall s,
  tag_theta (set_uf empty_env 1 YUnit) [(7,YAbs 1)] s = Good ([(7,TUnit)],s).
Proof. reflexivity. Qed.

Example tag_theta_nested_merged_argument : forall s,
  tag_theta (set_uf empty_env 1 YUnit) [(7,YNom 3 [(4,YAbs 1)])] s =
    Good ([(7,TNom 3 [(4,TUnit)])],s).
Proof. reflexivity. Qed.
