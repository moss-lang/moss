(* Each executable match checks its patterns, branch types, and coverage. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit EaBase EaExpr EaTypesSound EaPatternSound EaControlSound EaDecideSound.
From NM Require EaFamily PublicTypeFacts.
Import EaFamily (is_nil).
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Lemma match_coverage_sound : forall scrutinee covered catch,
  catch || (negb (is_nil (members scrutinee)) &&
    is_nil (filter (fun member => match head member with
      | Some tag => negb (nmem tag covered) | None => false end) (members scrutinee))) = true ->
  catch = true \/ (members scrutinee <> [] /\
    forall member tag, In member (members scrutinee) -> head member = Some tag -> In tag covered).
Proof.
  intros scrutinee covered catch Checked. apply orb_true_iff in Checked as [Catch|Complete]; [now left|right].
  apply andb_true_iff in Complete as [Nonempty Complete]. split.
  - intro Empty. now rewrite Empty in Nonempty.
  - intros member tag Present Head. apply nmem_spec.
    destruct (nmem tag covered) eqn:Covered; [reflexivity|].
    assert (Missing : In member (filter (fun member => match head member with
      | Some tag => negb (nmem tag covered) | None => false end) (members scrutinee))).
    { apply filter_In. split; [exact Present|]. now rewrite Head, Covered. }
    destruct (filter _ (members scrutinee)); [contradiction|discriminate].
Qed.

Section Match.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.

Variable valid : frame -> cx -> est -> Prop.
Hypothesis locals_preserve_invariant : forall F C bindings state,
  valid F (add_binds C bindings) state <-> valid F C state.

Definition ExprBelow (n : nat) : Prop :=
  forall depth, depth < n -> ExSoundAt E R P valid depth.

Definition BlockBelow (n : nat) : Prop :=
  forall depth, depth < n -> BlkESoundAt E R P valid depth.

Theorem arms_list_sound : forall n,
  ExprBelow n -> BlockBelow n ->
  forall F C scrutinee syntax expected before after compiled covered catch t,
    valid F C before ->
    arms_list E R n F C scrutinee syntax expected before = Ok ((compiled, covered, catch, t), after) ->
    ArmsList W P F C scrutinee syntax expected compiled covered catch t /\ valid F C after.
Proof.
  induction n as [|n IH]; intros Expressions Blocks F C scrutinee syntax expected
    before after compiled covered catch t Valid Run; [discriminate|].
  destruct syntax as [|[pattern body] rest]; [discriminate|]. cbn [arms_list] in Run.
  apply mbind_ok in Run as [[[[xp binds] heads] captures] [middle [Pattern Run]]].
  unfold pure in Pattern.
  destruct (pat_of E F scrutinee pattern) as [[[[pat locals] tags] all]|] eqn:PatternRead; [|discriminate].
  injection Pattern as <- <- <- <- <-.
  pose proof (pat_of_sound E F scrutinee pattern _ _ _ _ PatternRead) as PatternTyped.
  apply mbind_ok in Run as [[bx bt] [next [Branch Run]]].
  destruct (opt_fitsb bt expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
  assert (BranchTyped : Audit.Semantics.Elaboration.Branch W P F (add_binds C locals) body expected bx bt /\
      valid F (add_binds C locals) next).
  { eapply (branch_sound E R P valid n); [intros depth Less; apply Expressions; lia|
      intros depth Less; apply Blocks; lia|apply locals_preserve_invariant; exact Valid|exact Branch]. }
  destruct BranchTyped as [BranchTyped BranchValid]. apply locals_preserve_invariant in BranchValid.
  destruct rest as [|[next_pattern next_body] rest].
  - unfold ret in Run. injection Run as <- <- <- <- <-.
    split; [eapply ArmsOne; [exact PatternTyped|exact BranchTyped|now apply opt_fitsb_exact]|exact BranchValid].
  - apply mbind_ok in Run as [[[[tail tail_heads] tail_catch] tail_ty] [joined [Tail Run]]].
    apply mbind_ok in Run as [ty [final [Join Run]]].
    unfold ret in Run. injection Run as <- <- <- <- <-.
    destruct (join_m_sound E _ _ _ _ _ _ Join) as [JoinTyped <-].
    destruct (IH ltac:(intros depth Less; apply Expressions; lia)
      ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ _ _ _ _ BranchValid Tail)
      as [TailTyped Final].
    split; [eapply ArmsMore; [discriminate|exact PatternTyped|exact BranchTyped|
      now apply opt_fitsb_exact|exact TailTyped|exact JoinTyped]|exact Final].
Qed.

Theorem arms_sound : forall n,
  ExprBelow n -> BlockBelow n ->
  forall F C scrutinee syntax expected before after compiled t,
    valid F C before ->
    arms E R n F C scrutinee syntax expected before = Ok ((compiled, t), after) ->
    Arms W P F C scrutinee syntax expected compiled t /\ valid F C after.
Proof.
  intros [|n] Expressions Blocks F C scrutinee syntax expected before after compiled t Valid Run;
    [discriminate|]. cbn [arms] in Run.
  destruct (ty_eqb scrutinee never) eqn:Never.
  - apply PublicTypeFacts.ty_eqb_spec in Never. subst scrutinee.
    destruct syntax; [|discriminate]. unfold ret in Run. injection Run as <- <- <-.
    split; [constructor|exact Valid].
  - destruct (negb (is_nil syntax)); cbn [mbind ret] in Run; [|discriminate].
    apply mbind_ok in Run as [[[[rows covered] catch] result] [next [ListRead Run]]].
    destruct (catch || (negb (is_nil (members scrutinee)) &&
      is_nil (filter (fun member => match head member with
        | Some tag => negb (nmem tag covered) | None => false end) (members scrutinee)))) eqn:Coverage;
      cbn [mbind ret] in Run; [|discriminate].
    injection Run as <- <- <-.
    destruct (arms_list_sound n ltac:(intros depth Less; apply Expressions; lia)
      ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ _ _ _ _ Valid ListRead)
      as [ListTyped Final].
    split; [|exact Final]. eapply ArmsSome.
    + intro Equal. apply PublicTypeFacts.ty_eqb_spec in Equal. congruence.
    + exact ListTyped.
    + now apply match_coverage_sound.
Qed.

Theorem match_sound : forall n,
  ExprBelow n -> BlockBelow n ->
  forall F C scrutinee syntax expected before after compiled t,
    valid F C before ->
    ex E R n F C (AEMatch scrutinee syntax) expected before = Ok ((compiled, t), after) ->
    Ex W P F C (AEMatch scrutinee syntax) expected compiled t /\ valid F C after.
Proof.
  intros [|n] Expressions Blocks F C scrutinee syntax expected before after compiled t Valid Run;
    [discriminate|]. cbn [ex] in Run. fold (ex E R) (arms E R) in Run.
  apply mbind_ok in Run as [[sx st] [middle [Scrutinee Run]]].
  apply mbind_ok in Run as [[rows ty] [next [ArmRead Run]]].
  unfold ret in Run. injection Run as <- <- <-.
  destruct (Expressions n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ _ Valid Scrutinee)
    as [ScrutineeTyped Middle].
  destruct (arms_sound n ltac:(intros depth Less; apply Expressions; lia)
    ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ _ _ Middle ArmRead)
    as [ArmsTyped Final].
  split; [eapply ExMatch; eassumption|exact Final].
Qed.
End Match.
