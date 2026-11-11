(* Successful executable blocks thread the frame, context and generated
   body state described by the block rules. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaDecide EaExpr EaDecideSound EaControlSound EaBreaksSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.
Local Open Scope string_scope.

Lemma named_check_sound : forall F name before result after,
  need_named F name before = Ok (result, after) -> Named F name /\ before = after.
Proof.
  intros F name before result after Run. unfold need_named in Run.
  destruct (slook name (fr_var F)) eqn:Numbered; cbn [is_some ret] in Run; [|discriminate].
  injection Run as <- <-. split; [|reflexivity].
  unfold Named. rewrite Numbered. discriminate.
Qed.

Lemma read_local_annotation_sound : forall W fuel F C annotation before declared after,
  (match annotation with
   | Some a => let* d := pure (ty_of W fuel (fr_mod F) (c_ty C) (fr_this F) a) in ret (Some d)
   | None => ret None
   end) before = Ok (declared, after) ->
  before = after /\
  match annotation with
  | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
  | None => declared = None
  end.
Proof.
  intros W fuel F C [annotation|] before declared after Run.
  - apply mbind_ok in Run as [d [middle [Read Run]]]. unfold pure in Read.
    destruct (ty_of W fuel (fr_mod F) (c_ty C) (fr_this F) annotation) as [found|] eqn:Found;
      [|discriminate].
    injection Read as <- <-. unfold ret in Run. injection Run as <- <-.
    split; [reflexivity|]. exists found. split; [eapply ty_of_sound; exact Found|reflexivity].
  - unfold ret in Run. injection Run as <- <-. split; reflexivity.
Qed.

Lemma local_type_check_sound : forall actual expected (message : ty -> string) before chosen after,
  (match expected with
   | Some d => let* _ := check (fitsb actual d) (message d) in ret d
   | None => ret actual
   end) before = Ok (chosen, after) ->
  before = after /\
  match expected with Some d => fits actual d /\ chosen = d | None => chosen = actual end.
Proof.
  intros actual [d|] message before chosen after Run.
  - destruct (fitsb actual d) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
    injection Run as <- <-. split; [reflexivity|]. split; [now apply fitsb_exact|reflexivity].
  - unfold ret in Run. injection Run as <- <-. split; reflexivity.
Qed.

Lemma block_result_generations_disjoint : forall t made,
  find (fun g => nmem g made) (gens t) = None ->
  forall g, In g made -> ~ In g (gens t).
Proof.
  intros t made Check g Made Present.
  apply nmem_spec in Made. apply (find_none _ _ Check g) in Present. congruence.
Qed.

Lemma loop_follow_yes : forall W fuel md body (rest : list astmt) (tail : option aexpr),
  (match rest, tail with [], None => block_breaks W fuel md body | _, _ => Some true end) = Some true ->
  rest <> [] \/ tail <> None \/ BlockBreaks W md body.
Proof.
  intros W fuel md body [|next rest] [tail|] Run; try (left; discriminate).
  - right. left. discriminate.
  - right. right. eapply block_breaks_yes; exact Run.
Qed.

Lemma loop_follow_no : forall W fuel md body (rest : list astmt) (tail : option aexpr),
  (match rest, tail with [], None => block_breaks W fuel md body | _, _ => Some true end) = Some false ->
  rest = [] /\ tail = None /\ ~ BlockBreaks W md body.
Proof.
  intros W fuel md body [|next rest] [tail|] Run; try discriminate.
  split; [reflexivity|]. split; [reflexivity|eapply block_breaks_no; exact Run].
Qed.

Section Blocks.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Let fuel := e_fuel E.
Variable valid : frame -> cx -> est -> Prop.

Hypothesis local_scope : forall F C name t mu state,
  valid F (add_local C name t mu) state <-> valid F C state.
Hypothesis loop_scope : forall F C state,
  valid (with_loop F) C state <-> valid F C state.
Hypothesis unpinned_scope : forall F C state,
  valid (without_pins F) C state <-> valid F C state.
Hypothesis bind_items_sound : forall F C items wrap C' F' made before after,
  valid F C before ->
  r_bind_items R F C items before = Ok ((wrap, C', F', made), after) ->
  BindItems W P F C items wrap C' F' made /\ valid F' C' after /\
  forall final, valid F' C' final -> valid F C final.

Definition BlkSoundAt (n : nat) : Prop :=
  forall F C statements tail expected before after compiled t made,
    valid F C before ->
    blk E R n F C statements tail expected before = Ok ((compiled, t, made), after) ->
    Blk W P F C statements tail expected compiled t made /\ valid F C after.

Theorem blk_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  (forall depth, depth < n -> BlkESoundAt E R P valid depth) ->
  BlkSoundAt n.
Proof.
  induction n as [|n IH]; intros Expressions Blocks F C statements tail expected before after compiled t made Valid Run;
    [discriminate|].
  pose proof (Expressions n (Nat.lt_succ_diag_r n)) as ExprHere.
  pose proof (Blocks n (Nat.lt_succ_diag_r n)) as BlockHere.
  pose proof (IH ltac:(intros depth Less; apply Expressions; lia)
    ltac:(intros depth Less; apply Blocks; lia)) as RestHere.
  cbn [blk] in Run. fold (ex E R) (blk E R) (cond E R) (blke E R) in Run.
  destruct statements as [|statement rest].
  - destruct tail as [tail|].
    + apply mbind_ok in Run as [[value actual] [next [Tail Run]]].
      unfold ret in Run. injection Run as <- <- <- <-.
      destruct (ExprHere _ _ _ _ _ _ _ _ Valid Tail) as [Typed Final].
      split; [constructor; exact Typed|exact Final].
    + unfold ret in Run. injection Run as <- <- <- <-. split; [constructor|exact Valid].
  - destruct statement as [name annotation value|name annotation value|name value|items|condition body|body|value].
    1,2: apply mbind_ok in Run as [declared [reading [Read Run]]];
      apply read_local_annotation_sound in Read as [<- Annotation];
      apply mbind_ok in Run as [[v vt] [next [Value Run]]];
      destruct (ExprHere _ _ _ _ _ _ _ _ Valid Value) as [ValueTyped Next];
      apply mbind_ok in Run as [t0 [chosen [Choice Run]]];
      apply local_type_check_sound in Choice as [<- Chosen];
      apply mbind_ok in Run as [unit [named [Named Run]]];
      apply named_check_sound in Named as [Named <-];
      apply mbind_ok in Run as [[[xr rt] created] [final [Rest Run]]];
      unfold ret in Run; injection Run as <- <- <- <-;
      destruct (RestHere _ _ _ _ _ _ _ _ _ _ (proj2 (local_scope _ _ _ _ _ _) Next) Rest)
        as [RestTyped Final];
      apply (proj1 (local_scope F C name t0 _ _)) in Final;
      split; [eapply BlkLet; [constructor|exact Annotation|exact ValueTyped|exact Chosen|exact Named|exact RestTyped]|exact Final].
    + destruct (slook name (c_locals C)) as [[tv []]|] eqn:Local; try discriminate.
      apply mbind_ok in Run as [[v vt] [next [Value Run]]].
      destruct (fitsb vt tv) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
      apply mbind_ok in Run as [[[xr rt] created] [final [Rest Run]]].
      apply mbind_ok in Run as [unit [named [Named Run]]].
      apply named_check_sound in Named as [Named <-].
      unfold ret in Run. injection Run as <- <- <- <-.
      destruct (ExprHere _ _ _ _ _ _ _ _ Valid Value) as [ValueTyped Next].
      destruct (RestHere _ _ _ _ _ _ _ _ _ _ Next Rest) as [RestTyped Final].
      split; [eapply BlkAssign; [exact Local|exact ValueTyped|now apply fitsb_exact|exact RestTyped|exact Named]|exact Final].
    + apply mbind_ok in Run as [[[[wrap C'] F'] created] [next [Bindings Run]]].
      apply mbind_ok in Run as [[[xr rt] more] [final [Rest Run]]].
      unfold ret in Run. injection Run as <- <- <- <-.
      destruct (bind_items_sound _ _ _ _ _ _ _ _ _ Valid Bindings) as [BindingsTyped [Next Restore]].
      destruct (RestHere _ _ _ _ _ _ _ _ _ _ Next Rest) as [RestTyped Final].
      split; [eapply BlkBind; eassumption|now apply Restore].
    + apply mbind_ok in Run as [cx' [checked [Condition Run]]].
      apply mbind_ok in Run as [[bx bt] [next [Body Run]]].
      apply mbind_ok in Run as [[[xr rt] created] [final [Rest Run]]].
      unfold ret in Run. injection Run as <- <- <- <-.
      destruct (cond_sound E R P valid n ltac:(intros depth Less; apply Expressions; lia)
        _ _ _ _ _ _ Valid Condition) as [ConditionTyped Checked].
      destruct (BlockHere _ _ _ _ _ _ _ _ (proj2 (loop_scope _ _ _) Checked) Body)
        as [BodyTyped Next]. apply (proj1 (loop_scope F C next)) in Next.
      destruct (RestHere _ _ _ _ _ _ _ _ _ _ Next Rest) as [RestTyped Final].
      split; [eapply BlkWhile; eassumption|exact Final].
    + fold W fuel in Run.
      destruct (match rest, tail with
        | [], None => block_breaks W fuel (fr_mod F) body
        | _, _ => Some true end) as [[|]|] eqn:Follow; try discriminate.
      all: apply mbind_ok in Run as [[bx bt] [next [Body Run]]].
      all: destruct (BlockHere _ _ _ _ _ _ _ _ (proj2 (loop_scope _ _ _) Valid) Body)
        as [BodyTyped Next]; apply (proj1 (loop_scope F C next)) in Next.
      * apply mbind_ok in Run as [[[xr rt] created] [final [Rest Run]]].
        unfold ret in Run. injection Run as <- <- <- <-.
        destruct (RestHere _ _ _ _ _ _ _ _ _ _ Next Rest) as [RestTyped Final].
        split; [eapply BlkLoop; [eapply loop_follow_yes; exact Follow|exact BodyTyped|exact RestTyped]|exact Final].
      * unfold ret in Run. injection Run as <- <- <- <-.
        destruct (loop_follow_no _ _ _ _ _ _ Follow) as [-> [-> NoBreak]].
        split; [eapply BlkLoopLast; eassumption|exact Next].
    + destruct rest as [|next_statement rest], tail as [tail|].
      all: apply mbind_ok in Run as [[v vt] [next [Value Run]]].
      all: destruct (ExprHere _ _ _ _ _ _ _ _ Valid Value) as [ValueTyped Next].
      2: { unfold ret in Run. injection Run as <- <- <- <-.
           split; [eapply BlkExprLast; exact ValueTyped|exact Next]. }
      all: apply mbind_ok in Run as [[[xr rt] created] [final [Rest Run]]];
        unfold ret in Run; injection Run as <- <- <- <-;
        destruct (RestHere _ _ _ _ _ _ _ _ _ _ Next Rest) as [RestTyped Final];
        split; [eapply BlkExpr; [solve [left; discriminate|right; discriminate]|exact ValueTyped|exact RestTyped]|exact Final].
Qed.

Theorem blke_sound : forall n,
  (forall depth, depth < n -> ExSoundAt E R P valid depth) ->
  (forall depth, depth < n -> BlkESoundAt E R P valid depth) ->
  BlkESoundAt E R P valid n.
Proof.
  intros [|n] Expressions Blocks F C [statements tail] expected before after compiled t Valid Run;
    [discriminate|]. cbn [blke] in Run. fold (blk E R) in Run.
  apply mbind_ok in Run as [[[value actual] made] [next [Body Run]]].
  destruct (find (fun g => nmem g made) (gens actual)) eqn:Escapes; cbn [mbind ret] in Run; [discriminate|].
  destruct (opt_fitsb actual expected) eqn:Fits; cbn [mbind ret] in Run; [|discriminate].
  injection Run as <- <- <-.
  destruct (blk_sound n ltac:(intros depth Less; apply Expressions; lia)
    ltac:(intros depth Less; apply Blocks; lia) _ _ _ _ _ _ _ _ _ _
    (proj2 (unpinned_scope _ _ _) Valid) Body) as [Typed Final].
  apply (proj1 (unpinned_scope F C _)) in Final. split; [|exact Final].
  eapply BlkECheck; [exact Typed|now apply block_result_generations_disjoint|now apply opt_fitsb_exact].
Qed.
End Blocks.
