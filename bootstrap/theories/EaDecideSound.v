(* The executable branch checks follow Audit's expected-type and join rules. *)
From Stdlib Require Import Bool Arith Lia.
From NM Require Import Audit EaBase EaDecide EaExpr EaControlSound EaChecked.
From NM Require PublicTypeFacts.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Theorem opt_fitsb_exact : forall actual expected,
  opt_fitsb actual expected = true <-> opt_fits actual expected.
Proof.
  intros actual [expected|]; cbn [opt_fitsb opt_fits]; [apply fitsb_exact|tauto].
Qed.

Theorem join_sound : forall left right expected result,
  join left right expected = Ok result -> Join left right expected result.
Proof.
  intros left right [expected|] result Run; unfold join in Run.
  - injection Run as <-. constructor.
  - destruct (ty_eqb left never) eqn:Left.
    + apply PublicTypeFacts.ty_eqb_spec in Left. subst left.
      injection Run as <-. constructor.
    + destruct (ty_eqb right never || ty_eqb left right) eqn:Right; [|discriminate].
      injection Run as <-. apply JoinSame.
      * apply orb_true_iff in Right as [Never|Equal];
          [left|right]; apply PublicTypeFacts.ty_eqb_spec; assumption.
      * intro Equal. apply PublicTypeFacts.ty_eqb_spec in Equal. congruence.
Qed.

Lemma join_m_sound : forall E left right expected before result after,
  join_m E left right expected before = Ok (result, after) ->
  Join left right expected result /\ before = after.
Proof.
  intros E left right expected before result after Run.
  unfold join_m in Run. destruct (join left right expected) eqn:Joined; [|discriminate].
  unfold ret in Run. injection Run as <- <-. split; [now apply join_sound|reflexivity].
Qed.

Section ConditionalPost.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Local Notation Expr := (ExPostAt E P valid post R).
Local Notation Block := (BlkEPostAt E P valid post R).

Theorem if_post_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  (forall depth, depth < n -> Block depth) ->
  forall F C condition yes no expected,
    ExActionSound E P valid post F C (AEIf condition yes no) expected
      (ex E R n F C (AEIf condition yes no) expected).
Proof.
  intros [|n] Expressions Blocks F C condition yes no expected before [compiled t] after Run;
    [discriminate|].
  destruct no as [no|]; cbn [ex] in Run; fold (cond E R) (blke E R) (branch E R) in Run.
  all: apply mbind_ok in Run as [cx' [middle [Condition Run]]].
  all: apply mbind_ok in Run as [[bx bt] [next [Then Run]]].
  all: destruct (cond_post_sound E R P valid post n ltac:(intros depth Less; apply Expressions; lia)
    _ _ _ _ _ _ Condition) as [Earlier TypedCondition].
  all: destruct (Blocks n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Then) as [BackwardThen TypedThen].
  - apply mbind_ok in Run as [[ex' et] [joined [Else Run]]].
    destruct (opt_fitsb et expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
    apply mbind_ok in Run as [ty [final [Joined Run]]].
    unfold ret in Run. injection Run as <- <- <-.
    destruct (join_m_sound E _ _ _ _ _ _ Joined) as [JoinTyped <-].
    destruct (branch_post_sound E R P valid post n ltac:(intros depth Less; apply Expressions; lia)
      ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ Else) as [Backward TypedElse].
    split; [intros Post; now apply Earlier, BackwardThen, Backward|]. intros Valid Post.
    destruct (TypedCondition Valid (BackwardThen (Backward Post))) as [Middle ConditionTyped].
    destruct (TypedThen Middle (Backward Post)) as [Next ThenTyped].
    destruct (TypedElse Next Post) as [Final ElseTyped].
    split; [exact Final|eapply ExIfElse; [exact ConditionTyped|exact ThenTyped|exact ElseTyped|
      now apply opt_fitsb_exact|exact JoinTyped]].
  - destruct (opt_fitsb TUnit expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
    destruct (ty_eqb bt TUnit || ty_eqb bt never) eqn:BranchType; cbn [mbind ret] in Run; [|discriminate].
    injection Run as <- <- <-.
    split; [intros Post; now apply Earlier, BackwardThen|]. intros Valid Post.
    destruct (TypedCondition Valid (BackwardThen Post)) as [Middle ConditionTyped].
    destruct (TypedThen Middle Post) as [Final ThenTyped].
    split; [exact Final|eapply ExIfThen; [exact ConditionTyped|exact ThenTyped|now apply opt_fitsb_exact|]].
    apply orb_true_iff in BranchType as [Unit|Never]; [left|right]; now apply PublicTypeFacts.ty_eqb_spec.
Qed.
End ConditionalPost.

Section Conditional.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.

Theorem if_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  (forall depth, depth < n -> BlkESoundAt E R P valid depth) ->
  forall F C condition yes no expected before after compiled t,
    valid F C before ->
    ex E R n F C (AEIf condition yes no) expected before = Ok ((compiled, t), after) ->
    Ex W P F C (AEIf condition yes no) expected compiled t /\ valid F C after.
Proof.
  intros n Expressions Blocks F C condition yes no expected before after compiled t Valid Run.
  destruct (if_post_sound E R P valid (fun _ => True) n
    (fun depth Less => ex_without_post _ _ _ _ _ (Expressions depth Less))
    (fun depth Less => blke_without_post _ _ _ _ _ (Blocks depth Less)) _ _ _ _ _ _ _ _ _ Run)
    as [_ Checked].
  destruct (Checked Valid I) as [Final Typed]. now split.
Qed.
End Conditional.
