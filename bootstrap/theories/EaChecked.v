(* A successful action transports final evidence backwards. Its typing
   judgment and outgoing invariant can use evidence at the result state. *)
From Stdlib Require Import List.
From NM Require Import Audit EaBase EaExpr.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Definition Checked {A} (post valid_before : est -> Prop) (valid_after : A -> est -> Prop)
    (action : M A) (judgment : A -> est -> Prop) : Prop :=
  forall before result after,
    action before = Ok (result, after) ->
    (post after -> post before) /\
    (valid_before before -> post after -> valid_after result after /\ judgment result after).

Lemma checked_return : forall A post valid (result : A) judgment,
  (forall state, valid state -> post state -> judgment result state) ->
  Checked post valid (fun _ => valid) (ret result) judgment.
Proof.
  intros A post valid result judgment Prove before value after Run.
  unfold ret in Run. injection Run as <- <-. split; [tauto|intros; auto].
Qed.

Lemma checked_bind : forall A B post valid_before valid_middle valid_after
    (action : M A) (next : A -> M B) first second judgment,
  Checked post valid_before valid_middle action first ->
  (forall a, Checked post (valid_middle a) valid_after (next a) (second a)) ->
  (forall a middle result after, first a middle -> second a result after -> judgment result after) ->
  Checked post valid_before valid_after (mbind action next) judgment.
Proof.
  intros A B post valid_before valid_middle valid_after action next first second judgment
    Action Next Combine before result after Run.
  unfold mbind in Run.
  destruct (action before) as [[a middle]|] eqn:ActionRun; [|discriminate].
  rename Run into NextRun.
  destruct (Action _ _ _ ActionRun) as [Earlier First].
  destruct (Next a _ _ _ NextRun) as [Backward Second].
  split.
  - intros Post. now apply Earlier, Backward.
  - intros Valid Post.
    destruct (First Valid (Backward Post)) as [Middle FirstTyped].
    destruct (Second Middle Post) as [Final SecondTyped].
    split; [exact Final|eapply Combine; eassumption].
Qed.

Lemma checked_by_effect : forall A post valid_before valid_after
    (action : M A) judgment (extends : est -> est -> Prop),
  (forall before after, extends before after -> post after -> post before) ->
  (forall before result after, action before = Ok (result, after) -> extends before after) ->
  (forall before result after, valid_before before -> action before = Ok (result, after) ->
    post after -> valid_after result after /\ judgment result after) ->
  Checked post valid_before valid_after action judgment.
Proof.
  intros A post valid_before valid_after action judgment extends Back Effects Prove before result after Run.
  split.
  - apply Back. eapply Effects; exact Run.
  - intros Valid Post. exact (Prove _ _ _ Valid Run Post).
Qed.

Lemma checked_without_post : forall A valid_before valid_after (action : M A) judgment,
  (forall before result after, valid_before before -> action before = Ok (result, after) ->
    valid_after result after /\ judgment result after) ->
  Checked (fun _ => True) valid_before valid_after action judgment.
Proof. intros A pre next action judgment Prove before result after Run. split; [tauto|intros Valid _; exact (Prove _ _ _ Valid Run)]. Qed.

Section ExpressionContracts.
Variables (E : env) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Definition ExActionSound F C syntax expected (action : M (expr * ty)) : Prop :=
  Checked post (valid F C) (fun _ => valid F C) action
    (fun '(compiled, t) _ => Ex W P F C syntax expected compiled t).

Definition BlkEActionSound F C syntax expected (action : M (expr * ty)) : Prop :=
  Checked post (valid F C) (fun _ => valid F C) action
    (fun '(compiled, t) _ => BlkE W P F C syntax expected compiled t).

Definition ExPostAt R n : Prop :=
  forall F C syntax expected,
    ExActionSound F C syntax expected (ex E R n F C syntax expected).

Definition BlkEPostAt R n : Prop :=
  forall F C syntax expected,
    BlkEActionSound F C syntax expected (blke E R n F C syntax expected).

Definition BlkPostAt R n : Prop :=
  forall F C statements tail expected,
    Checked post (valid F C) (fun _ => valid F C) (blk E R n F C statements tail expected)
      (fun '(compiled, t, made) _ => Blk W P F C statements tail expected compiled t made).

End ExpressionContracts.
