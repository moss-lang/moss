(* The supported Base operations have canonical names and symbol operands. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import AuditBase.
From NM Require Import Audit.
Import AuditBase.Semantics ListNotations.

Lemma operation_symbols_eqb_spec : forall xs ys,
  operation_symbols_eqb xs ys = true <-> xs = ys.
Proof.
  induction xs as [|x xs IH]; intros [|y ys]; cbn; try (split; congruence).
  rewrite andb_true_iff, Nat.eqb_eq, IH. split; [intros [-> ->]; reflexivity|intro E; inversion E; auto].
Qed.

Lemma operation_eqb_spec : forall a b, operation_eqb a b = true <-> a = b.
Proof.
  intros [a xs] [b ys]. unfold operation_eqb. cbn [op_name op_syms].
  rewrite andb_true_iff, String.eqb_eq, operation_symbols_eqb_spec.
  split; [intros [-> ->]; reflexivity|intro E; inversion E; auto].
Qed.

Lemma decode_candidate_encode : forall p,
  decode_candidate (encode p) = Some p.
Proof.
  intros p. destruct p;
    repeat match goal with
    | x : i32bin |- _ => destruct x
    | x : i32cmp |- _ => destruct x
    | x : i32un |- _ => destruct x
    | x : f64bin |- _ => destruct x
    | x : f64cmp |- _ => destruct x
    | x : f64un |- _ => destruct x
    | x : conv |- _ => destruct x
    | x : conv64 |- _ => destruct x
    | x : bool |- _ => destruct x
    | x : loadk |- _ => destruct x
    | x : storek |- _ => destruct x
    | x : load64k |- _ => destruct x
    | x : store64k |- _ => destruct x
    | x : i64un |- _ => destruct x
    end; reflexivity.
Qed.

Lemma decode_encode : forall p, decode (encode p) = Some p.
Proof.
  intros p. unfold decode, decode_operation. rewrite decode_candidate_encode.
  rewrite (proj2 (operation_eqb_spec _ _) eq_refl). reflexivity.
Qed.

Lemma decode_canonical : forall operation p, decode operation = Some p -> operation = encode p.
Proof.
  intros operation p. unfold decode, decode_operation.
  destruct (decode_candidate operation) as [found|]; [|discriminate].
  destruct (operation_eqb operation (encode found)) eqn:Equal; [|discriminate].
  intros Same. injection Same as <-. exact (proj1 (operation_eqb_spec _ _) Equal).
Qed.

(* Duplicate names in the diagnostic table still retain their own indices. *)
Lemma unsupported_operation_code : forall name index,
  decode (unsupported_operation name index) = None /\
  unsupported_code (unsupported_operation name index) = index.
Proof. intros; split; reflexivity. Qed.
