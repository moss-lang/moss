(* Global values read their source declaration. Local references are
   translated using the checked numbering. *)
From Stdlib Require Import List String Bool.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaExpr EaControlSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaConstantSound EaLiteralSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Section Paths.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.

Definition ConstantsChecked : Prop :=
  forall F C symbol d n syntax initial,
  w_decl W symbol = Some d -> d_ast d = ADVal n syntax (Some initial) ->
  Checked post (valid F C) (fun _ => valid F C) (read_constant E R symbol d syntax initial)
    (fun '(code, declared) _ => EaConstantSound.ConstantReading W P d syntax initial code declared).

Hypothesis Constants : ConstantsChecked.

Theorem global_value_sound : forall F C path expected,
  (forall n, path = [n] -> slook n (c_locals C) = None) ->
  ExActionSound E P valid post F C (AEPath path None) expected (read_global_value E R F C path).
Proof.
  intros F C path expected Local before [code t] after Run.
  unfold read_global_value in Run. fold W in Run.
  apply mbind_ok in Run as (s & middle & Resolve & Run).
  destruct (resolve_sym W (fr_mod F) path) as [symbol|] eqn:Resolved; [|discriminate Resolve].
  injection Resolve as <- <-.
  apply mbind_ok in Run as (declared & middle & Read & Run).
  destruct (w_decl W symbol) as [d|] eqn:Found; [|discriminate Read].
  injection Read as <- <-.
  destruct (d_ast d) as [assumes declarations|n|n body|n params arity syntax|n|n syntax initial|
    receiver n params result body|n items|n params inputs outputs body] eqn:Ast;
    try discriminate Run.
  - injection Run as <- <- <-. split; [tauto|]. intros Valid _.
    split; [exact Valid|]. eapply ExBareTag; [exact Local|exact Resolved|].
    exists n. unfold ast_of. fold W. rewrite Found. cbn. now rewrite Ast.
  - destruct initial as [init|].
    + destruct (Constants F C _ _ _ _ _ Found Ast _ _ _ Run) as [Back Checked].
      split; [exact Back|]. intros Valid Post.
      destruct (Checked Valid Post) as [ValidAfter Initializer].
      split; [exact ValidAfter|].
      eapply EaConstantSound.constant_reading_ex; eassumption.
    + apply mbind_ok in Run as (meaning & middle & Lookup & Run).
      destruct (cval C symbol) as [t'|] eqn:ReadValue; [|discriminate Lookup].
      injection Lookup as <- <-. injection Run as <- <- <-.
      split; [tauto|]. intros Valid _.
      split; [exact Valid|].
      eapply ExVal; [exact Local|exact Resolved|exact Found|exact Ast|].
      now rewrite <- EaLiteralSound.cval_exact.
Qed.

Theorem value_path_sound : forall n F C path application expected,
  ExActionSound E P valid post F C (AEPath path application) expected
    (ex E R (S n) F C (AEPath path application) expected).
Proof.
  intros n F C path [application|] expected before [code t] after Run; [discriminate|].
  cbn [ex] in Run.
  destruct path as [|name [|next rest]].
  - exact (global_value_sound F C [] expected ltac:(intros; discriminate) before (code, t) after Run).
  - destruct (slook name (c_locals C)) as [[t' mutability]|] eqn:Local.
    + apply mbind_ok in Run as ([] & middle & NamedCheck & Run).
      unfold need_named in NamedCheck.
      destruct (slook name (fr_var F)) as [slot|] eqn:Slot; [|discriminate NamedCheck].
      injection NamedCheck as <-. injection Run as <- <- <-.
      split; [tauto|]. intros Valid _. split; [exact Valid|].
      eapply ExLocal; [exact Local|]. unfold Named. congruence.
    + assert (Missing : forall other, [name] = [other] -> slook other (c_locals C) = None).
      { intros other Equal. now injection Equal as <-. }
      exact (global_value_sound F C [name] expected Missing before (code, t) after Run).
  - exact (global_value_sound F C (name :: next :: rest) expected ltac:(intros; discriminate)
      before (code, t) after Run).
Qed.
End Paths.
