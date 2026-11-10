(* Structural equality facts for the public type syntax. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit.
Import Audit.Semantics ListNotations.

Theorem ty_eqb_spec : forall left right,
  ty_eqb left right = true <-> left = right.
Proof.
  fix IH 1. intros left right; destruct left as [m|fields| |t|n th|ms]; destruct right;
    cbn [ty_eqb]; try (split; congruence).
  - rewrite Nat.eqb_eq. split; congruence.
  - assert (ListEqual : forall other,
      (fix go (fs gs : list (string * ty)) :=
        match fs, gs with
        | [], [] => true
        | (f,t)::fs, (g,u)::gs => String.eqb f g && ty_eqb t u && go fs gs
        | _, _ => false end) fields other = true <-> fields = other).
    { induction fields as [|[name child] rest Tail]; intros [|[name' child'] rest'];
        cbn; try (split; congruence).
      rewrite !andb_true_iff, String.eqb_eq, IH, Tail.
      split; [intros [[-> ->] ->]; reflexivity|intro Equal; inversion Equal; auto]. }
    rewrite ListEqual. split; congruence.
  - rewrite Nat.eqb_eq. split; congruence.
  - assert (ListEqual : forall other,
      (fix go (xs ys : list (tsym * ty)) :=
        match xs, ys with
        | [], [] => true
        | q::rest, q'::rest' =>
            Nat.eqb (fst q) (fst q') && ty_eqb (snd q) (snd q') && go rest rest'
        | _, _ => false end) th other = true <-> th = other).
    { induction th as [|[name child] rest Tail]; intros [|[name' child'] rest'];
        cbn; try (split; congruence).
      rewrite !andb_true_iff, Nat.eqb_eq, IH, Tail.
      split; [intros [[-> ->] ->]; reflexivity|intro Equal; inversion Equal; auto]. }
    rewrite andb_true_iff, Nat.eqb_eq, ListEqual.
    split; [intros [-> ->]; reflexivity|intro Equal; inversion Equal; auto].
  - assert (ListEqual : forall other,
      (fix go (xs ys : list ty) :=
        match xs, ys with
        | [], [] => true
        | child::rest, child'::rest' => ty_eqb child child' && go rest rest'
        | _, _ => false end) ms other = true <-> ms = other).
    { induction ms as [|child rest Tail]; intros [|child' rest'];
        cbn; try (split; congruence).
      rewrite andb_true_iff, IH, Tail.
      split; [intros [-> ->]; reflexivity|intro Equal; inversion Equal; auto]. }
    rewrite ListEqual. split; congruence.
Qed.

Corollary ty_eqb_refl : forall tau, ty_eqb tau tau = true.
Proof. intro tau; apply ty_eqb_spec; reflexivity. Qed.
