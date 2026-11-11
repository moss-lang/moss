(* Assembly writes declared bodies below the initial counter. It
   preserves the bound on its code tables and keeps all previously
   allocated generated slots. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaCodeAllocationSound EaCodeEffects.
From NM Require EaBindCodeEffects EaControlSound EaMain EaPlatform WorldSound.
From NM Require Import EaExpressionCodeEffects.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Local Open Scope string_scope.

Definition PreservesCodeBound {A} (bound : nat) (action : M A) : Prop :=
  forall s x s', bound <= es_next s -> CodeTablesBound s ->
    action s = Ok (x, s') -> es_next s <= es_next s' /\ CodeTablesBound s' /\
    (forall table id, bound <= id -> id < es_next s -> read_code s' table id = read_code s table id) /\
    (forall table id, id < es_next s -> read_origin s' table id = read_origin s table id) /\
    (LinkRecords s -> LinkRecords s') /\
    (LambdaOriginsBound s -> LambdaOriginsBound s').

Lemma code_effect_preserves_bound : forall A bound (action : M A),
  PreservesOldCode action -> PreservesCodeBound bound action.
Proof.
  intros A bound action Effect s x s' Next Bound Run.
  destruct (Effect _ _ _ Run) as [Next' Old Bound' Origins Links Lambda]. split; [exact Next'|].
  split; [exact (Bound' Bound)|]. split; [|split; [exact Origins|split; assumption]].
  intros table id _ Earlier. apply Old. exact Earlier.
Qed.

Lemma code_bound_bind : forall A B bound (action : M A) (next : A -> M B),
  PreservesCodeBound bound action ->
  (forall x, PreservesCodeBound bound (next x)) ->
  PreservesCodeBound bound (mbind action next).
Proof.
  intros A B bound action next First Second s x s' Next Bound Run.
  apply EaControlSound.mbind_ok in Run as (middle & state & Run1 & Run2).
  destruct (First _ _ _ Next Bound Run1) as (Next' & Bound' & Earlier & Origins & Links & Lambda).
  destruct (Second middle state x s' ltac:(lia) Bound' Run2) as (Next'' & Bound'' & Earlier' & Origins' & Links' & Lambda').
  split; [lia|]. split; [exact Bound''|]. split.
  - intros table id Generated Old. rewrite Earlier' by lia. apply Earlier; assumption.
  - split.
    + intros table id Old. rewrite Origins' by lia. now apply Origins.
    + auto.
Qed.

Lemma code_bound_miter : forall A bound (f : A -> M unit) values,
  (forall x, In x values -> PreservesCodeBound bound (f x)) ->
  PreservesCodeBound bound (miter f values).
Proof.
  intros A bound f values. induction values as [|x rest IH]; intro Effects; cbn [miter].
  - apply code_effect_preserves_bound, code_effect_return.
  - apply code_bound_bind.
    + apply Effects. now left.
    + intro ignored. apply IH. intros x' Present. apply Effects. now right.
Qed.

Lemma source_write_preserves_bound : forall bound f body,
  f < bound -> PreservesCodeBound bound (set_fn_body f body).
Proof.
  intros bound f body Source s x s' Next Bound Run.
  assert (Origins : forall table id, read_origin s' table id = read_origin s table id).
  { unfold set_fn_body in Run. injection Run as _ <-. intros [] id; reflexivity. }
  destruct (write_code_spec FunctionTable (CodeSlot f) body s ltac:(cbn [slot_index]; lia) Bound)
    as (written & Write & Next' & Tables & _ & Other).
  change (set_fn_body f body s = Ok (tt, written)) in Write.
  rewrite Run in Write. injection Write as _ ->.
  split; [lia|]. split; [exact Tables|]. split.
  2: {
    split; [intros table id _; apply Origins|].
    unfold set_fn_body in Run. injection Run as _ <-. split; auto.
  }
  intros table id Generated Old.
  apply Other. right. cbn [slot_index]. lia.
Qed.

Lemma code_effect_with_msg : forall A message (action : M A),
  PreservesOldCode action -> PreservesOldCode (with_msg message action).
Proof.
  intros A message action Effect. apply (code_effect_map_err A (String.append message) action Effect).
Qed.

Lemma code_bound_with_msg : forall A bound message (action : M A),
  PreservesCodeBound bound action -> PreservesCodeBound bound (with_msg message action).
Proof.
  intros A bound message action Effect s x s' Next Bound Run.
  unfold with_msg in Run.
  destruct (action s) as [[value written]|message'] eqn:Computed; [|discriminate].
  injection Run as <- <-. exact (Effect _ _ _ Next Bound Computed).
Qed.

Lemma code_effect_get : PreservesOldCode get.
Proof. intros s x s' Run. injection Run as _ <-. apply code_transition_refl. Qed.

Lemma platform_entry_code_effect : forall E C k, PreservesOldCode (EaPlatform.platform_entry E C k).
Proof.
  intros E C [type|value|fn|receiver method args|functor].
  all: unfold EaPlatform.platform_entry; cbn zeta; try apply code_effect_fail.
  - apply code_effect_bind; [apply code_effect_lift|]. intro declaration.
    apply code_effect_bind; [apply code_effect_lift|]. intro value_type.
    apply code_effect_bind; [apply code_effect_check|intros []].
    destruct (filed_name (d_ast declaration)) as [[ns name]|]; [|apply code_effect_fail].
    destruct ns; try apply code_effect_fail.
    destruct (String.eqb name "i32_zero"), (String.eqb name "i32_one");
      first [apply code_effect_return|apply code_effect_fail].
  - apply code_effect_bind; [apply code_effect_lift|]. intro declaration.
    destruct (d_ast declaration) as [| | | | | |receiver name params result body| |]; try apply code_effect_fail.
    destruct receiver; try apply code_effect_fail.
    match goal with
    | |- PreservesOldCode (match ?primitive with Some _ => _ | None => _ end) =>
      destruct primitive
    end.
    + apply code_effect_bind; [apply code_effect_check|intros []]. apply code_effect_return.
    + apply code_effect_fail.
  - apply code_effect_bind; [apply code_effect_lift|]. intro declaration.
    destruct (d_ast declaration) as [| | | | | | | |name parameters arguments results body]; try apply code_effect_fail.
    destruct body; [apply code_effect_fail|].
    apply code_effect_bind; [apply code_effect_check|intros []].
    apply code_effect_bind; [apply code_effect_pure|]. intro body.
    apply code_effect_bind; [exact (code_effect_emit FunctorTable _)|].
    intro slot. apply code_effect_return.
Qed.

Lemma main_environment_code_effect : forall E main, PreservesOldCode (EaPlatform.main_environment E main).
Proof.
  intros E main. unfold EaPlatform.main_environment.
  apply code_effect_bind; [apply code_effect_lift|]. intros [context needs].
  apply code_effect_bind.
  { apply code_effect_mmap. intro key.
    apply code_effect_bind; [apply platform_entry_code_effect|]. intro provided.
    apply code_effect_return. }
  intro platform. apply code_effect_return.
Qed.

Theorem source_function_preserves_bound : forall E bound f,
  f < bound -> PreservesCodeBound bound (EaMain.elab_fn E f).
Proof.
  intros E bound f Declared. unfold EaMain.elab_fn.
  apply code_bound_bind; [apply code_effect_preserves_bound, code_effect_lift|]. intro declaration.
  destruct (d_ast declaration) as [| | | | | |receiver name parameters result body| |];
    try (apply code_effect_preserves_bound, code_effect_return).
  destruct body as [body|]; [|apply code_effect_preserves_bound, code_effect_return].
  apply code_bound_bind; [apply code_effect_preserves_bound, code_effect_lift|]. intros [context needs].
  apply code_bound_bind.
  { apply code_effect_preserves_bound. destruct receiver; try apply code_effect_return.
    apply code_effect_bind; [apply code_effect_lift|]. intro nominal.
    do 2 (apply code_effect_bind; [apply code_effect_check|intros []]).
    apply code_effect_bind.
    { match goal with
      | |- PreservesOldCode (match ?absent with Some _ => _ | None => _ end) =>
        destruct absent; [apply code_effect_fail|apply code_effect_return]
      end. }
    intro arguments. apply code_effect_return. }
  intro this.
  apply code_bound_bind; [apply code_effect_preserves_bound, code_effect_pure|]. intro signature.
  apply code_bound_bind; [apply code_effect_preserves_bound, EaBindCodeEffects.code_effect_optional_failure|intros []].
  apply code_bound_bind.
  { apply code_effect_preserves_bound.
    destruct (mkrec_code_effect _ (run_code_effect E (e_fuel E))) as [_ [Block _]].
    apply Block. }
  intros [body_code body_type]. apply source_write_preserves_bound. exact Declared.
Qed.

Theorem constant_code_effect : forall E symbol, PreservesOldCode (EaMain.elaborate_constant E symbol).
Proof.
  intros E symbol. unfold EaMain.elaborate_constant.
  destruct (w_decl (e_W E) symbol) as [declaration|]; [|apply code_effect_return].
  destruct (d_ast declaration) as [| | | | |name type initial| | |]; try apply code_effect_return.
  destruct initial as [expression|]; [|apply code_effect_return].
  apply code_effect_with_msg.
  apply code_effect_bind; [|intro ignored; apply code_effect_return].
  destruct (mkrec_code_effect _ (run_code_effect E (e_fuel E))) as [Expr _]. apply Expr.
Qed.

Theorem finish_assembly_code_effect : forall E bound entry,
  PreservesOldCode (EaMain.finish_assembly E bound entry).
Proof.
  intros E bound entry. unfold EaMain.finish_assembly.
  apply code_effect_bind; [apply code_effect_lift|]. intro main.
  do 2 (apply code_effect_bind; [apply code_effect_check|intros []]).
  apply code_effect_bind; [apply main_environment_code_effect|]. intro environment.
  apply code_effect_bind; [apply code_effect_lift|]. intros [context needs].
  apply code_effect_bind; [apply code_effect_pure|]. intro signature.
  apply code_effect_bind; [apply code_effect_check|intros []].
  do 2 (apply code_effect_bind; [apply code_effect_pure|intros []]).
  apply code_effect_bind; [apply code_effect_check|intros []].
  apply code_effect_bind; [apply code_effect_pure|intros []].
  apply code_effect_bind; [apply code_effect_get|]. intro state.
  destruct (EaPlatform.bool_syms E) as [[boolean true_symbol] false_symbol].
  do 2 (apply code_effect_bind; [apply code_effect_check|intros []]).
  apply code_effect_return.
Qed.

Theorem function_pass_preserves_code_bound : forall E bound symbols,
  (forall f, In f symbols -> f < bound) ->
  PreservesCodeBound bound (miter (EaMain.elaborate_function E) symbols).
Proof.
  intros E bound symbols Bounded. apply code_bound_miter. intros f Present.
  unfold EaMain.elaborate_function. destruct (EaMain.defined_fn E f).
  - apply code_bound_with_msg, source_function_preserves_bound. now apply Bounded.
  - apply code_effect_preserves_bound, code_effect_return.
Qed.

Theorem assemble_preserves_code_bound : forall E bound entry, PreservesCodeBound bound (EaMain.assemble E bound entry).
Proof.
  intros E bound entry. unfold EaMain.assemble.
  apply code_bound_bind.
  { apply function_pass_preserves_code_bound. intros f Present. apply in_seq in Present. lia. }
  intros []. apply code_effect_preserves_bound.
  apply code_effect_bind.
  - apply code_effect_miter. apply constant_code_effect.
  - intros []. apply finish_assembly_code_effect.
Qed.

Theorem assembly_code_tables_bound : forall E bound entry P final,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  bound <= es_next final /\ CodeTablesBound final.
Proof.
  intros E bound entry P final Run.
  destruct (assemble_preserves_code_bound E bound entry (init_state bound) P final
    (Nat.le_refl bound) (code_tables_initial bound) Run) as (Counter & Tables & _).
  split; assumption.
Qed.

Theorem assembled_code_lookup_exact : forall E bound entry P final,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  (forall n, body_of P n = read_code final FunctionTable n) /\
  (forall n, funbody_of P n = read_code final FunctorTable n).
Proof.
  intros E bound entry P final Run.
  exact (assembled_code_lookup E bound entry (init_state bound) P final Run
    (proj2 (assembly_code_tables_bound E bound entry P final Run))).
Qed.

Theorem assembly_link_records : forall E bound entry P final,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  LinkRecords final.
Proof.
  intros E bound entry P final Run.
  destruct (assemble_preserves_code_bound E bound entry (init_state bound) P final
    (Nat.le_refl bound) (code_tables_initial bound) Run) as (_ & _ & _ & _ & Links & _).
  apply Links, initial_link_records.
Qed.

Theorem assembly_lambda_origins_bound : forall E bound entry P final,
  EaMain.assemble E bound entry (init_state bound) = Ok (P, final) ->
  LambdaOriginsBound final.
Proof.
  intros E bound entry P final Run.
  destruct (assemble_preserves_code_bound E bound entry (init_state bound) P final
    (Nat.le_refl bound) (code_tables_initial bound) Run) as (_ & _ & _ & _ & _ & Lambda).
  apply Lambda, initial_lambda_origins_bound.
Qed.
