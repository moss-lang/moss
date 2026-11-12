(* Expression/control subrules with explicit lower-fuel elaboration contracts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaExpr EaTypesSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require PublicTypeFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma mbind_ok : forall A B (m : M A) (next : A -> M B) before value after,
  mbind m next before = Ok (value, after) ->
  exists a middle, m before = Ok (a, middle) /\ next a middle = Ok (value, after).
Proof.
  intros A B m next before value after Run. unfold mbind in Run.
  destruct (m before) as [[a middle]|message]; [exists a, middle; split; [reflexivity|exact Run]|discriminate].
Qed.

Theorem fitsb_exact : forall t expected, fitsb t expected = true <-> fits t expected.
Proof.
  intros t expected. unfold fitsb, fits.
  assert (Union : (match expected with
    | TUnion ms => forallb (fun m => existsb (ty_eqb m) ms) (members t)
    | _ => false end) = true <->
    exists ms, expected = TUnion ms /\ forall m, In m (members t) -> In m ms).
  { destruct expected; try (split; [discriminate|intros [? [Impossible _]]; discriminate]).
    rewrite forallb_forall. split.
    - intro Checked. exists l. split; [reflexivity|]. intros member Present.
      specialize (Checked member Present). apply existsb_exists in Checked as [found [InUnion Equal]].
      apply PublicTypeFacts.ty_eqb_spec in Equal. now subst.
    - intros [ms [Equal Included]]. injection Equal as <-. intros member Present.
      apply existsb_exists. exists member. split; [apply Included, Present|apply PublicTypeFacts.ty_eqb_spec; reflexivity]. }
  rewrite !orb_true_iff, !PublicTypeFacts.ty_eqb_spec, Union. tauto.
Qed.

Lemma oty_eqb_exact : forall a b, oty_eqb a b = true <-> a = b.
Proof.
  intros [a|] [b|]; cbn [oty_eqb]; try (split; congruence).
  rewrite PublicTypeFacts.ty_eqb_spec. split; congruence.
Qed.

Section ControlContracts.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
(* The lower-fuel contracts thread the caller's invariant through each
   successful elaboration. In particular they need not hold for arbitrary
   contents of the generated-body and constant caches. *)
Variable valid : frame -> cx -> est -> Prop.
Definition ExSoundAt (n : nat) : Prop :=
  forall F C syntax expected before after value t,
  valid F C before ->
  ex E R n F C syntax expected before = Ok ((value, t), after) ->
  Ex W P F C syntax expected value t /\ valid F C after.

Definition BlkESoundAt (n : nat) : Prop :=
  forall F C syntax expected before after value t,
  valid F C before ->
  blke E R n F C syntax expected before = Ok ((value, t), after) ->
  BlkE W P F C syntax expected value t /\ valid F C after.

End ControlContracts.

Lemma ex_without_post : forall E R P valid n,
  ExSoundAt E R P valid n -> ExPostAt E P valid (fun _ => True) R n.
Proof.
  intros E R P valid n Sound F C syntax expected. apply checked_without_post.
  intros before [value t] after Valid Run.
  destruct (Sound _ _ _ _ _ _ _ _ Valid Run) as [Typed Final]. now split.
Qed.

Lemma blke_without_post : forall E R P valid n,
  BlkESoundAt E R P valid n -> BlkEPostAt E P valid (fun _ => True) R n.
Proof.
  intros E R P valid n Sound F C syntax expected. apply checked_without_post.
  intros before [value t] after Valid Run.
  destruct (Sound _ _ _ _ _ _ _ _ Valid Run) as [Typed Final]. now split.
Qed.

Section PureControl.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.

Theorem bare_return_sound : forall n F C expected before after value t,
  ex E R n F C (AEReturn None) expected before = Ok ((value, t), after) ->
  Ex W P F C (AEReturn None) expected value t /\ before = after.
Proof.
  intros [|n] F C expected before after value t Run; [discriminate|]. cbn [ex] in Run.
  destruct (oty_eqb (fr_ret F) (Some TUnit)) eqn:Return; [|discriminate].
  cbn [mbind ret] in Run. injection Run as <- <- <-.
  split; [apply ExReturnUnit, oty_eqb_exact; exact Return|reflexivity].
Qed.

Theorem break_sound : forall n F C expected before after value t,
  ex E R n F C AEBreak expected before = Ok ((value, t), after) ->
  Ex W P F C AEBreak expected value t /\ before = after.
Proof.
  intros [|n] F C expected before after value t Run; [discriminate|]. cbn [ex] in Run.
  destruct (fr_loop F) eqn:Loop; [|discriminate]. cbn [mbind ret] in Run. injection Run as <- <- <-.
  split; [apply ExBreak; exact Loop|reflexivity].
Qed.

End PureControl.

Section ControlPost.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Local Notation Expr := (ExPostAt E P valid post R).
Local Notation Block := (BlkEPostAt E P valid post R).

Theorem args_post_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C callee names syntax types,
    Checked post (valid F C) (fun _ => valid F C) (args E R n F C callee names syntax types)
      (fun values _ => Args W P F C syntax types values).
Proof.
  induction n as [|n IH]; intros Below F C callee names syntax types before values after Run;
    [discriminate|]. cbn [args] in Run.
  destruct (negb (Nat.eqb (List.length syntax) (List.length types))); [discriminate|].
  destruct syntax as [|argument syntax], types as [|expected types]; try discriminate.
  - unfold ret in Run. injection Run as <- <-.
    split; [tauto|intros Valid _; split; [exact Valid|constructor]].
  - apply mbind_ok in Run as [[value actual] [middle [First Run]]].
    destruct (fitsb (canon C actual) (canon C expected)) eqn:Fits; [|discriminate].
    change (mbind (args E R n F C callee (tl names) syntax types)
      (fun rest => ret (value :: rest)) middle = Ok (values, after)) in Run.
    apply mbind_ok in Run as [rest [final [Tail Run]]]. unfold ret in Run. injection Run as <- <-.
    destruct (Below n (Nat.lt_succ_diag_r n) F C argument (Some expected) before (value, actual) middle First)
      as [Earlier Arg].
    assert (Less : forall depth, depth < n -> Expr depth) by (intros; apply Below; lia).
    destruct (IH Less _ _ _ _ _ _ _ _ _ Tail) as [Backward Rest].
    split; [intros Post; now apply Earlier, Backward|]. intros Valid Post.
    destruct (Arg Valid (Backward Post)) as [Middle Typed].
    destruct (Rest Middle Post) as [Final TypedRest].
    split; [exact Final|eapply ArgsCons; [exact Typed|now apply fitsb_exact|exact TypedRest]].
Qed.

Theorem cond_post_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C syntax,
    Checked post (valid F C) (fun _ => valid F C) (cond E R n F C syntax)
      (fun value _ => Cond W P F C syntax value).
Proof.
  intros [|n] Below F C syntax before value after Run; [discriminate|].
  cbn [cond] in Run. fold (ex E R) in Run.
  destruct (lang_sym (e_W E) "bool" NSName "Bool") as [boolean|] eqn:Boolean; [|discriminate].
  cbn [mbind ret] in Run.
  apply mbind_ok in Run as [[x t] [middle [Read Run]]].
  destruct (fitsb t (TNom boolean [])) eqn:Fits; [|discriminate].
  cbn [mbind ret] in Run. injection Run as <- <-.
  destruct (Below n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Read) as [Backward Typed].
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final Expression].
  split; [exact Final|]. eapply CondBool with (t:=t) (b:=boolean).
  - change (LangSym W "bool" NSName "Bool" boolean). eapply lang_sym_sound; exact Boolean.
  - exact Expression.
  - now apply fitsb_exact.
Qed.

Theorem branch_post_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  (forall depth, depth < n -> Block depth) ->
  forall F C syntax expected,
    Checked post (valid F C) (fun _ => valid F C) (branch E R n F C syntax expected)
      (fun '(value, t) _ => Branch W P F C syntax expected value t).
Proof.
  intros [|n] ExprBelow BlockBelow F C syntax expected before [value t] after Run; [discriminate|].
  destruct syntax; cbn [branch] in Run;
    try solve [
      destruct (ExprBelow n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Run) as [Backward Typed];
      split; [exact Backward|intros Valid Post];
      destruct (Typed Valid Post) as [Final Expression];
      split; [exact Final|apply BranchExpr; [intros block0 Impossible; discriminate|exact Expression]]].
  apply mbind_ok in Run as [[x actual] [middle [Read Run]]]. unfold ret in Run. injection Run as <- <- <-.
  destruct (BlockBelow n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Read) as [Backward Typed].
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final BlockTyped]. split; [exact Final|now apply BranchBlock].
Qed.

Theorem return_post_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C syntax expected,
    ExActionSound E P valid post F C (AEReturn syntax) expected (ex E R n F C (AEReturn syntax) expected).
Proof.
  intros n Below F C [syntax|] expected before [value t] after Run.
  - destruct n as [|n]; [discriminate|]. cbn [ex] in Run.
    destruct (fr_ret F) as [ret_type|] eqn:Return; [|discriminate]. cbn [mbind ret] in Run.
    apply mbind_ok in Run as [[x actual] [middle [Read Run]]].
    destruct (fitsb actual ret_type) eqn:Fits; [|discriminate].
    cbn [mbind ret] in Run. injection Run as <- <- <-.
    destruct (Below n (Nat.lt_succ_diag_r n) _ _ _ _ _ _ _ Read) as [Backward Typed].
    split; [exact Backward|]. intros Valid Post.
    destruct (Typed Valid Post) as [Final Expression].
    split; [exact Final|eapply ExReturn; [exact Return|exact Expression|now apply fitsb_exact]].
  - destruct (bare_return_sound E R P n F C expected before after value t Run) as [Returned <-].
    split; [tauto|intros Valid _; now split].
Qed.

Lemma break_post_sound : forall n F C expected,
  ExActionSound E P valid post F C AEBreak expected (ex E R n F C AEBreak expected).
Proof.
  intros n F C expected before [value t] after Run.
  destruct (break_sound E R P n F C expected before after value t Run) as [Typed <-].
  split; [tauto|intros Valid _; now split].
Qed.
End ControlPost.

Section Control.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.

Theorem args_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  forall F C callee names syntax types before after values,
    valid F C before ->
    args E R n F C callee names syntax types before = Ok (values, after) ->
    Args W P F C syntax types values /\ valid F C after.
Proof.
  intros n Below F C callee names syntax types before after values Valid Run.
  destruct (args_post_sound E R P valid (fun _ => True) n
    (fun depth Less => ex_without_post _ _ _ _ _ (Below depth Less)) _ _ _ _ _ _ _ _ _ Run)
    as [_ Checked].
  destruct (Checked Valid I) as [Final Typed]. now split.
Qed.

Theorem cond_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  forall F C syntax before after value,
    valid F C before ->
    cond E R n F C syntax before = Ok (value, after) -> Cond W P F C syntax value /\ valid F C after.
Proof.
  intros n Below F C syntax before after value Valid Run.
  destruct (cond_post_sound E R P valid (fun _ => True) n
    (fun depth Less => ex_without_post _ _ _ _ _ (Below depth Less)) _ _ _ _ _ _ Run) as [_ Checked].
  destruct (Checked Valid I) as [Final Typed]. now split.
Qed.

Theorem branch_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  (forall depth, depth < n -> BlkESoundAt E R P valid depth) ->
  forall F C syntax expected before after value t,
  valid F C before ->
  branch E R n F C syntax expected before = Ok ((value, t), after) ->
  Branch W P F C syntax expected value t /\ valid F C after.
Proof.
  intros n ExprBelow BlockBelow F C syntax expected before after value t Valid Run.
  destruct (branch_post_sound E R P valid (fun _ => True) n
    (fun depth Less => ex_without_post _ _ _ _ _ (ExprBelow depth Less))
    (fun depth Less => blke_without_post _ _ _ _ _ (BlockBelow depth Less)) _ _ _ _ _ _ _ Run)
    as [_ Checked].
  destruct (Checked Valid I) as [Final Typed]. now split.
Qed.

Theorem return_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  forall F C syntax expected before after value t,
  valid F C before ->
  ex E R n F C (AEReturn syntax) expected before = Ok ((value, t), after) ->
  Ex W P F C (AEReturn syntax) expected value t /\ valid F C after.
Proof.
  intros n Below F C syntax expected before after value t Valid Run.
  destruct (return_post_sound E R P valid (fun _ => True) n
    (fun depth Less => ex_without_post _ _ _ _ _ (Below depth Less)) _ _ _ _ _ _ _ Run)
    as [_ Checked].
  destruct (Checked Valid I) as [Final Typed]. now split.
Qed.
End Control.
