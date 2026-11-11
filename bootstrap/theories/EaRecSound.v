(* The query driver threads validity forwards and final-program evidence
   backwards. A judgment can use the evidence at the action's end. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit EaBase EaExpr EaBind EaMain EaControlSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

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
  apply mbind_ok in Run as [a [middle [Run NextRun]]].
  destruct (Action _ _ _ Run) as [Earlier First].
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

Section Callbacks.
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

Definition bind_scope_valid F C (result : wrapper * cx * frame * list nat) (after : est) : Prop :=
  let '(_, C', F', _) := result in
  valid F' C' after /\ forall final, valid F' C' final -> valid F C final.

Definition BindItemSound (R : rec) : Prop :=
  forall F C item,
    Checked post (valid F C) (bind_scope_valid F C) (bind_item E R F C item)
      (fun '(wrap, C', F', made) _ => BindItem W P F C item wrap C' F' made).

Definition BindItemsSound
    (read : frame -> cx -> list (spec * option aexpr) -> M (wrapper * cx * frame * list nat)) : Prop :=
  forall F C items,
    Checked post (valid F C) (bind_scope_valid F C) (read F C items)
      (fun '(wrap, C', F', made) _ => BindItems W P F C items wrap C' F' made).

Theorem bind_items_sound : forall R,
  BindItemSound R -> BindItemsSound (bind_items E R).
Proof.
  intros R ItemSound F C items. revert F C.
  induction items as [|item items IH]; intros F C before [[[wrap C'] F'] made] after Run;
    cbn [bind_items] in Run.
  - unfold ret in Run. injection Run as <- <- <- <- <-.
    split; [tauto|]. intros Valid _. split; [exact (conj Valid (fun _ Valid => Valid))|constructor].
  - apply mbind_ok in Run as [[[[wrap1 C1] F1] made1] [middle [Item Run]]].
    apply mbind_ok in Run as [[[[wrap2 C2] F2] made2] [next [Tail Run]]].
    unfold ret in Run. injection Run as <- <- <- <- <-.
    destruct (ItemSound F C item _ _ _ Item) as [Earlier CheckedItem].
    destruct (IH F1 C1 _ _ _ Tail) as [Backward CheckedTail].
    split.
    + intros Post. now apply Earlier, Backward.
    + intros Valid Post.
      destruct (CheckedItem Valid (Backward Post)) as [[Middle ExitItem] TypedItem].
      destruct (CheckedTail Middle Post) as [[Final ExitTail] TypedTail].
      split.
      * split; [exact Final|]. intros final Scope. now apply ExitItem, ExitTail.
      * eapply BICons; eassumption.
Qed.

Definition RecSound (R : rec) : Prop :=
  (forall F C syntax expected, ExActionSound F C syntax expected (r_ex R F C syntax expected)) /\
  (forall F C syntax expected, BlkEActionSound F C syntax expected (r_blke R F C syntax expected)) /\
  BindItemsSound (r_bind_items R).

Theorem run_callbacks_sound : forall fuel,
  (forall R, RecSound R -> ExPostAt R (e_fuel E)) ->
  (forall R, RecSound R -> BlkEPostAt R (e_fuel E)) ->
  (forall R, RecSound R -> BindItemSound R) ->
  RecSound (mkrec (run E fuel)).
Proof.
  induction fuel as [|fuel IH]; intros Expressions Blocks Bindings.
  { repeat split; intros; discriminate. }
  specialize (IH Expressions Blocks Bindings).
  split; [|split].
  - intros F C syntax expected before [compiled t] after Read.
    cbn [mkrec r_ex] in Read.
    apply mbind_ok in Read as [answer [middle [Call Read]]].
    cbn [run] in Call. apply mbind_ok in Call as [[x ty] [next [Expr Call]]].
    unfold ret in Call. injection Call as <- <-.
    unfold ret in Read. injection Read as <- <- <-.
    exact (Expressions (mkrec (run E fuel)) IH F C syntax expected before (x, ty) next Expr).
  - intros F C syntax expected before [compiled t] after Read.
    cbn [mkrec r_blke] in Read.
    apply mbind_ok in Read as [answer [middle [Call Read]]].
    cbn [run] in Call. apply mbind_ok in Call as [[x ty] [next [Block Call]]].
    unfold ret in Call. injection Call as <- <-.
    unfold ret in Read. injection Read as <- <- <-.
    exact (Blocks (mkrec (run E fuel)) IH F C syntax expected before (x, ty) next Block).
  - intros F C items before [[[wrap C'] F'] made] after Read.
    cbn [mkrec r_bind_items] in Read.
    apply mbind_ok in Read as [answer [middle [Call Read]]].
    cbn [run] in Call. apply mbind_ok in Call as [[[[w D] G] created] [next [Bind Call]]].
    unfold ret in Call. injection Call as <- <-.
    unfold ret in Read. injection Read as <- <- <- <- <-.
    exact (bind_items_sound (mkrec (run E fuel)) (Bindings _ IH) F C items before (w, D, G, created) next Bind).
Qed.

Theorem run_elaboration_sound : forall fuel,
  (forall R, RecSound R -> forall n,
    (forall depth, depth < n -> ExPostAt R depth) ->
    (forall depth, depth < n -> BlkEPostAt R depth) ->
    ExPostAt R n /\ BlkEPostAt R n) ->
  (forall R, RecSound R -> BindItemSound R) ->
  RecSound (mkrec (run E fuel)).
Proof.
  intros fuel BodyStep Bindings.
  assert (Bodies : forall R, RecSound R -> forall n, ExPostAt R n /\ BlkEPostAt R n).
  { intros R Checked n. induction n using lt_wf_ind.
    apply (BodyStep R Checked n).
    - intros depth Less. exact (proj1 (H depth Less)).
    - intros depth Less. exact (proj2 (H depth Less)). }
  apply run_callbacks_sound.
  - intros R Checked. exact (proj1 (Bodies R Checked _)).
  - intros R Checked. exact (proj2 (Bodies R Checked _)).
  - exact Bindings.
Qed.
End Callbacks.
