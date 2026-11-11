(* The executable branch checks follow Audit's expected-type and join rules. *)
From Stdlib Require Import Bool Arith Lia.
From NM Require Import Audit EaBase EaDecide EaExpr EaControlSound.
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

Section Conditional.
Variables (E : env) (R : rec) (P : program).
Variable valid : frame -> cx -> est -> Prop.
Let W := e_W E.

Theorem if_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  (forall depth, depth < n -> BlkESoundAt E R P valid depth) ->
  forall F C condition yes no expected before after compiled t,
    valid F C before ->
    ex E R n F C (AEIf condition yes no) expected before = Ok ((compiled, t), after) ->
    Ex W P F C (AEIf condition yes no) expected compiled t /\ valid F C after.
Proof.
  intros [|n] Expressions Blocks F C condition yes no expected before after compiled t Valid Run;
    [discriminate|].
  destruct no as [no|]; cbn [ex] in Run; fold (cond E R) (blke E R) (branch E R) in Run.
  all: apply mbind_ok in Run as [cx' [middle [Condition Run]]].
  all: apply mbind_ok in Run as [[bx bt] [next [Then Run]]].
  all: destruct (cond_sound E R P valid n ltac:(intros depth Less; apply Expressions; lia)
    _ _ _ _ _ _ Valid Condition) as [ConditionTyped Middle].
  all: destruct (Blocks n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ _ Middle Then)
    as [ThenTyped ThenValid].
  - apply mbind_ok in Run as [[ex' et] [joined [Else Run]]].
    destruct (opt_fitsb et expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
    apply mbind_ok in Run as [ty [final [Joined Run]]].
    unfold ret in Run. injection Run as <- <- <-.
    destruct (branch_sound E R P valid n ltac:(intros depth Less; apply Expressions; lia)
      ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ _ ThenValid Else)
      as [ElseTyped ElseValid].
    destruct (join_m_sound E _ _ _ _ _ _ Joined) as [JoinTyped <-].
    split; [eapply ExIfElse; [exact ConditionTyped|exact ThenTyped|exact ElseTyped|
      now apply opt_fitsb_exact|exact JoinTyped]|exact ElseValid].
  - destruct (opt_fitsb TUnit expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
    destruct (ty_eqb bt TUnit || ty_eqb bt never) eqn:BranchType; cbn [mbind ret] in Run; [|discriminate].
    injection Run as <- <- <-.
    split; [eapply ExIfThen; [exact ConditionTyped|exact ThenTyped|now apply opt_fitsb_exact|]|exact ThenValid].
    apply orb_true_iff in BranchType as [Unit|Never]; [left|right]; now apply PublicTypeFacts.ty_eqb_spec.
Qed.
End Conditional.
