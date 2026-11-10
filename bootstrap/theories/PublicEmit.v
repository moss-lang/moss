(* Direct emission of executable type metadata into the public type syntax.
   Structural field names stay strings; no record-head or field-number
   encoding is needed at this boundary. This is not whole elaboration
   soundness: contexts, code emission and machine initialization remain. *)
From Stdlib Require Import List String Arith Lia.
From NM Require Import Audit.
From NM Require Elab.
Import Audit.Semantics ListNotations.

Fixpoint emit_type (t : Elab.ety) : ty :=
  match t with
  | Elab.YUnit => TUnit
  | Elab.YNever => TUnion []
  | Elab.YAbs symbol => TAbs symbol
  | Elab.YNom head arguments =>
      TNom head (map (fun q => (fst q, emit_type (snd q))) arguments)
  | Elab.YUnion members => TUnion (map emit_type members)
  | Elab.YRec fields => TRecord (map (fun q => (fst q, emit_type (snd q))) fields)
  end.

Definition emit_subst (s : list (nat * Elab.ety)) : subst :=
  map (fun q => (fst q, emit_type (snd q))) s.

Lemma emit_lookup : forall s symbol,
  lookup_subst symbol (emit_subst s) = option_map emit_type (LoadWorld.nassoc symbol s).
Proof.
  induction s as [|[head meaning] rest IH]; intro symbol.
  - reflexivity.
  - change ((if Nat.eqb symbol head then Some (emit_type meaning)
      else lookup_subst symbol (emit_subst rest)) =
      option_map emit_type (if Nat.eqb symbol head then Some meaning else LoadWorld.nassoc symbol rest)).
    destruct (Nat.eqb symbol head); [reflexivity|exact (IH symbol)].
Qed.

Theorem emit_substitution : forall fuel t s,
  Elab.ety_depth t <= fuel ->
  emit_type (Elab.ety_subst fuel s t) = app_subst (emit_subst s) (emit_type t).
Proof.
  induction fuel as [|fuel IH]; intros t s Enough.
  - destruct t; cbn [Elab.ety_depth] in Enough; lia.
  - destruct t as [| |symbol|head arguments|members|fields];
      cbn [Elab.ety_subst emit_type app_subst]; try reflexivity.
    + rewrite emit_lookup. destruct (LoadWorld.nassoc symbol s); reflexivity.
    + f_equal. revert Enough.
      induction arguments as [|[name child] rest Tail]; intro Enough; cbn [map]; [reflexivity|].
      f_equal.
      * f_equal. apply IH. cbn [Elab.ety_depth map fold_right] in Enough.
        pose proof (Nat.le_max_l (Elab.ety_depth child)
          (fold_right Nat.max 0 (map (fun q => Elab.ety_depth (snd q)) rest))). lia.
      * apply Tail. cbn [Elab.ety_depth map fold_right] in *.
        pose proof (Nat.le_max_r (Elab.ety_depth child)
          (fold_right Nat.max 0 (map (fun q => Elab.ety_depth (snd q)) rest))). lia.
    + f_equal. revert Enough.
      induction members as [|child rest Tail]; intro Enough; cbn [map]; [reflexivity|].
      f_equal.
      * apply IH. cbn [Elab.ety_depth map fold_right] in Enough.
        pose proof (Nat.le_max_l (Elab.ety_depth child)
          (fold_right Nat.max 0 (map Elab.ety_depth rest))). lia.
      * apply Tail. cbn [Elab.ety_depth map fold_right] in *.
        pose proof (Nat.le_max_r (Elab.ety_depth child)
          (fold_right Nat.max 0 (map Elab.ety_depth rest))). lia.
    + f_equal. revert Enough.
      induction fields as [|[name child] rest Tail]; intro Enough; cbn [map]; [reflexivity|].
      f_equal.
      * f_equal. apply IH. cbn [Elab.ety_depth map fold_right] in Enough.
        pose proof (Nat.le_max_l (Elab.ety_depth child)
          (fold_right Nat.max 0 (map (fun q => Elab.ety_depth (snd q)) rest))). lia.
      * apply Tail. cbn [Elab.ety_depth map fold_right] in *.
        pose proof (Nat.le_max_r (Elab.ety_depth child)
          (fold_right Nat.max 0 (map (fun q => Elab.ety_depth (snd q)) rest))). lia.
Qed.

Corollary emit_full_substitution : forall s t,
  emit_type (Elab.ety_subst_full s t) = app_subst (emit_subst s) (emit_type t).
Proof. intros; apply emit_substitution; reflexivity. Qed.

Example record_fields_are_preserved :
  emit_type (Elab.YRec [("payload"%string, Elab.YAbs 7)]) = TRecord [("payload"%string, TAbs 7)].
Proof. reflexivity. Qed.
