(* The query driver threads validity forwards and final-program evidence
   backwards. A judgment can use the evidence at the action's end. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import Audit EaBase EaExpr EaBind EaMain EaControlSound
  EaCodeAllocationSound EaCodeEffects EaAssemblyCodeEffects EaChecked.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

(* Only populated generated slots need to survive into the final state.
   An enclosing action can still fill a slot reserved around a child. *)
Record GeneratedPost (source_bound : nat) (final state : est) : Prop := {
  generated_counter : es_next state <= es_next final;
  generated_code : forall table number code,
    source_bound <= number < es_next state ->
    read_code state table number = Some code ->
    read_code final table number = Some code;
  generated_origin : forall table number origin,
    source_bound <= number < es_next state ->
    read_origin state table number = Some origin ->
    read_origin final table number = Some origin
}.

Lemma generated_post_refl : forall source_bound state,
  GeneratedPost source_bound state state.
Proof. constructor; auto. Qed.

Lemma generated_post_compose : forall source_bound before middle final,
  GeneratedPost source_bound middle before ->
  GeneratedPost source_bound final middle ->
  GeneratedPost source_bound final before.
Proof.
  intros source_bound before middle final [Counter Code Origin] [Counter' Code' Origin'].
  constructor; [lia| |].
  - intros table number value Issued Read. apply Code'; [lia|now apply Code].
  - intros table number value Issued Read. apply Origin'; [lia|now apply Origin].
Qed.

Theorem code_transition_preserves_post : forall source_bound before after final,
  CodeTransition before after ->
  GeneratedPost source_bound final after ->
  GeneratedPost source_bound final before.
Proof.
  intros source_bound before after final [Counter Code Bound Origin Links] Post.
  eapply generated_post_compose; [|exact Post]. constructor; [exact Counter| |].
  - intros table number value [_ Issued] Read. now rewrite Code by exact Issued.
  - intros table number value [_ Issued] Read. now rewrite Origin by exact Issued.
Qed.

Theorem source_action_establishes_post : forall A source_bound (action : M A) before result after,
  PreservesCodeBound source_bound action ->
  source_bound <= es_next before ->
  CodeTablesBound before ->
  action before = Ok (result, after) ->
  GeneratedPost source_bound after before.
Proof.
  intros A source_bound action before result after Preserves Lower Bound Run.
  destruct (Preserves _ _ _ Lower Bound Run) as (Counter & Bound' & Code & Origin & Links).
  constructor; [exact Counter| |].
  - intros table number value [Generated Issued] Read. now rewrite Code by assumption.
  - intros table number value [_ Issued] Read. now rewrite Origin by assumption.
Qed.

Theorem generated_code_in_assembly : forall E source_bound entry P final state,
  assemble E source_bound entry (init_state source_bound) = Ok (P, final) ->
  GeneratedPost source_bound final state ->
  forall table number code, source_bound <= number < es_next state ->
    read_code state table number = Some code ->
    (match table as t return option (code_in t) with
     | FunctionTable => body_of P number | FunctorTable => funbody_of P number end) = Some code.
Proof.
  intros E source_bound entry P final state Assembled Post table number code Issued Read.
  destruct (assembled_code_lookup_exact _ _ _ _ _ Assembled) as [Functions Functors].
  pose proof (generated_code _ _ _ Post table number code Issued Read) as Kept.
  destruct table; [now rewrite Functions|now rewrite Functors].
Qed.

Theorem generated_lambda_origin_in_assembly : forall E source_bound entry P final state number origin,
  assemble E source_bound entry (init_state source_bound) = Ok (P, final) ->
  GeneratedPost source_bound final state ->
  source_bound <= number < es_next state ->
  read_origin state FunctionTable number = Some origin ->
  nlook number (lambda_sections (program_origins P)) = Some origin.
Proof.
  intros E source_bound entry P final state number origin Assembled Post Issued Read.
  pose proof (generated_counter _ _ _ Post) as Counter.
  rewrite (assembled_lambda_section _ _ _ _ _ _ _ Assembled) by lia.
  exact (generated_origin _ _ _ Post FunctionTable _ _ Issued Read).
Qed.

Theorem generated_link_origin_in_assembly : forall E source_bound entry P final state number target pins,
  assemble E source_bound entry (init_state source_bound) = Ok (P, final) ->
  GeneratedPost source_bound final state ->
  source_bound <= number < es_next state ->
  read_origin state FunctorTable number = Some (target, pins) ->
  nlook number (linked_origins (program_origins P)) = Some target /\ link_pins_at final number = pins.
Proof.
  intros E source_bound entry P final state number target pins Assembled Post Issued Read.
  pose proof (generated_origin _ _ _ Post FunctorTable _ _ Issued Read) as Kept.
  cbn [read_origin] in Kept.
  rewrite (assembled_link_origin _ _ _ _ _ _ _ Assembled).
  fold (read_link_target final number).
  destruct (read_link_target final number) as [found|]; [|discriminate].
  injection Kept as <- <-. split; reflexivity.
Qed.

Section Callbacks.
Variables (E : env) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Definition bind_scope_valid F C (result : wrapper * cx * frame * list nat) (after : est) : Prop :=
  let '(_, C', F', _) := result in
  valid F' C' after /\ forall final, valid F' C' final -> valid F C final.

Definition BindItemAt (R : rec) F C item : Prop :=
  Checked post (valid F C) (bind_scope_valid F C) (bind_item E R F C item)
    (fun '(wrap, C', F', made) _ => BindItem W P F C item wrap C' F' made).

Definition BindItemSound (R : rec) : Prop :=
  forall F C item, BindItemAt R F C item.

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
  (forall F C syntax expected, ExActionSound E P valid post F C syntax expected (r_ex R F C syntax expected)) /\
  (forall F C syntax expected, BlkEActionSound E P valid post F C syntax expected (r_blke R F C syntax expected)) /\
  BindItemsSound (r_bind_items R).

Theorem run_callbacks_sound : forall fuel,
  (forall R, RecSound R -> ExPostAt E P valid post R (e_fuel E)) ->
  (forall R, RecSound R -> BlkEPostAt E P valid post R (e_fuel E)) ->
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
    (forall depth, depth < n -> ExPostAt E P valid post R depth) ->
    (forall depth, depth < n -> BlkEPostAt E P valid post R depth) ->
    ExPostAt E P valid post R n /\ BlkEPostAt E P valid post R n) ->
  (forall R, RecSound R -> BindItemSound R) ->
  RecSound (mkrec (run E fuel)).
Proof.
  intros fuel BodyStep Bindings.
  assert (Bodies : forall R, RecSound R -> forall n, ExPostAt E P valid post R n /\ BlkEPostAt E P valid post R n).
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
