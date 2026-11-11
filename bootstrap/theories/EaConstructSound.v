(* Typed nominal construction, records and field projection. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaExpr EaControlSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaNumbering PublicTypeFacts.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Local Lemma pure_result : forall A (read : er A) before value after,
  pure read before = Ok (value, after) -> read = Ok value /\ before = after.
Proof. intros A [read|message] before value after Run; [injection Run as -> ->; auto|discriminate]. Qed.

Local Lemma lift_result : forall A (read : option A) message before value after,
  lift read message before = Ok (value, after) -> read = Some value /\ before = after.
Proof. intros A [read|] message before value after Run; [injection Run as -> ->; auto|discriminate]. Qed.

Local Lemma check_result : forall (test : bool) message before value after,
  check test message before = Ok (value, after) -> test = true /\ value = tt /\ before = after.
Proof. intros [] message before value after Run; [injection Run as <- ->; auto|discriminate]. Qed.

Section Constructs.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Local Notation Expr := (ExPostAt E P valid post R).
Local Notation Action := (ExActionSound E P valid post).

Local Lemma tag_arguments_read : forall C s rows arguments,
  rmap (fun r => let! t := ropt (nlook r (rows ++ c_ty C))
    ("type " ++ bq (sym_name W r) ++ " (required by " ++ qs W s ++
      ") is not available in the context here") in Ok (r, t)) (w_reqs W s) = Ok arguments ->
  Forall2 (fun r row => fst row = r /\ nlook r (rows ++ c_ty C) = Some (snd row))
    (w_reqs W s) arguments.
Proof.
  intros C s rows arguments Read. eapply rmap_sound; [|exact Read].
  intros r row Run.
  cbn beta in Run.
  destruct (nlook r (rows ++ c_ty C)) as [t|] eqn:Found; cbn [rbind] in Run.
  - injection Run as <-. split; [reflexivity|first [reflexivity|exact Found]].
  - discriminate Run.
Qed.

Theorem tag_call_sound : forall n,
  Expr n ->
  forall F C path application args expected s d name ps syntax,
  resolve_sym W (fr_mod F) path = Some s -> w_decl W s = Some d ->
  d_ast d = ADTag name ps syntax ->
  Action F C (AECall path application args) expected
    (ex E R (S n) F C (AECall path application args) expected).
Proof.
  intros n Below F C path application args expected s d name ps syntax
    Resolve Declared Tag before [value t] after Run.
  cbn [ex] in Run. fold W in Run. rewrite Resolve in Run. cbn [mbind ret] in Run.
  rewrite Declared in Run. cbn [mbind ret] in Run. rewrite Tag in Run.
  apply mbind_ok in Run as (rows & mid & Read & Run).
  apply pure_result in Read as [Rows <-].
  apply mbind_ok in Run as (arguments & mid & Read & Run).
  apply pure_result in Read as [Arguments <-].
  apply mbind_ok in Run as (payload & mid & Read & Run).
  apply pure_result in Read as [Payload <-].
  destruct args as [|y [|extra rest]]; try discriminate.
  apply mbind_ok in Run as ([body actual] & finish & Child & Run).
  apply mbind_ok in Run as (checked & final & Check & Run).
  apply check_result in Check as [Fits [_ <-]].
  injection Run as <- <- <-.
  destruct (Below _ _ _ _ _ _ _ Child) as [Backward Typed].
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final Body]. split; [exact Final|].
  eapply ExTag; [exact Resolve|unfold kind_sym; fold W; now rewrite Declared, Tag|
    eapply brackets_sound; exact Rows|now apply tag_arguments_read|
    eapply payload_of_sound; exact Payload|reflexivity|exact Body|now apply fitsb_exact].
Qed.

Theorem record_fields_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C fs ftys,
  Checked post (valid F C) (fun _ => valid F C) (mmap_fields E R n F C fs ftys)
    (fun fields _ => Forall2 (fun ft xf => fst xf = fst ft /\
      exists n v vt, n = fst ft /\ In (n, v) fs /\
        Ex W P F C (match v with Some y => y | None => AEPath [n] None end)
          (Some (snd ft)) (snd xf) vt /\ fits vt (snd ft)) ftys fields).
Proof.
  induction n as [|n IH]; intros Below F C fs ftys before fields after Run; [discriminate|].
  cbn [mmap_fields] in Run.
  destruct ftys as [|[label field_type] rest].
  - injection Run as <- <-. split; [tauto|intros Valid _; split; [exact Valid|constructor]].
  - apply mbind_ok in Run as (syntax & started & Read & Run).
    destruct (find (fun q => String.eqb (fst q) label) fs)
      as [[found chosen]|] eqn:Found; [|discriminate Read].
    injection Read as <- <-.
    apply find_some in Found as [Member Equal]. cbn [fst] in Equal.
    apply String.eqb_eq in Equal. subst found.
    apply mbind_ok in Run as ([value actual] & emitted & Field & Run).
    destruct (Below n ltac:(lia) _ _ _ _ _ _ _ Field) as [Earlier TypedField].
    apply mbind_ok in Run as (checked & checked_state & Check & Run).
    apply check_result in Check as [Fits [_ <-]].
    apply mbind_ok in Run as (others & final & Tail & Run).
    injection Run as <- <-.
    destruct (IH (fun depth Lt => Below depth ltac:(lia)) _ _ _ _ _ _ _ Tail)
      as [Backward TypedFields].
    split; [intros Post; now apply Earlier, Backward|]. intros Valid Post.
    destruct (TypedField Valid (Backward Post)) as [ValidEmitted Typed].
    destruct (TypedFields ValidEmitted Post) as [ValidAfter TypedTail].
    split; [exact ValidAfter|]. constructor; [|exact TypedTail].
    split; [reflexivity|]. exists label, chosen, actual.
    split; [reflexivity|split; [exact Member|split; [exact Typed|now apply fitsb_exact]]].
Qed.

Theorem record_expression_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C path application fields expected,
  Action F C (AERec path application fields) expected
    (ex E R (S n) F C (AERec path application fields) expected).
Proof.
  intros n Below F C path application fields expected before [value t] after Run.
  cbn [ex] in Run.
  apply mbind_ok in Run as (s & middle & Read & Run).
  apply lift_result in Read as [Resolved <-].
  apply mbind_ok in Run as (checked & middle & Read & Run).
  apply check_result in Read as [Tag [_ <-]].
  apply mbind_ok in Run as (rows & middle & Read & Run).
  apply pure_result in Read as [Rows <-].
  apply mbind_ok in Run as (arguments & middle & Read & Run).
  apply pure_result in Read as [Arguments <-].
  apply mbind_ok in Run as (payload & middle & Read & Run).
  apply pure_result in Read as [Payload <-].
  destruct payload as [|ftys| | | |]; try discriminate.
  apply mbind_ok in Run as (known & middle & Known & Run).
  destruct (find (fun q => negb (is_some (slook (fst q) ftys))) fields)
    as [[label syntax]|] eqn:KnownFields; [discriminate Known|].
  injection Known as _ <-.
  apply mbind_ok in Run as (distinct & middle & Distinct & Run).
  destruct (suniq_dup (map fst fields)) as [duplicate|] eqn:DistinctFields;
    [discriminate Distinct|].
  injection Distinct as _ <-.
  apply mbind_ok in Run as (present & middle & Present & Run).
  destruct (filter (fun q => negb (existsb (fun q' => String.eqb (fst q') (fst q)) fields)) ftys)
    as [|missing rest] eqn:PresentFields; [|discriminate Present].
  injection Present as _ <-.
  apply mbind_ok in Run as (arity & middle & Check & Run).
  apply check_result in Check as [Arity [_ <-]]. apply Nat.eqb_eq in Arity.
  apply mbind_ok in Run as (translated & finish & Fields & Run).
  injection Run as <- <- <-.
  destruct (record_fields_sound n Below _ _ _ _ _ _ _ Fields) as [Backward Typed].
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final Proof]. split; [exact Final|].
  eapply ExRecord; [exact Resolved|now apply kind_is_spec|eapply brackets_sound; exact Rows|
    now apply tag_arguments_read|eapply payload_of_sound; exact Payload|reflexivity|
    now apply EaNumbering.checked_parameters_distinct|exact Arity|exact Proof].
Qed.

Theorem field_expression_sound : forall n,
  Expr n ->
  forall F C receiver field expected,
  Action F C (AEField receiver field) expected (ex E R (S n) F C (AEField receiver field) expected).
Proof.
  intros n Below F C receiver field expected before [value t] after Run.
  cbn [ex] in Run.
  apply mbind_ok in Run as ([object object_type] & middle & Object & Run).
  destruct (Below _ _ _ _ _ _ _ Object) as [Backward Typed].
  apply mbind_ok in Run as (record & start & Record & Run).
  assert (ReadRecord :
    ((exists tag args, object_type = TNom tag args /\ kind_sym W tag = Some DKTag /\
       PayloadOf W tag args record) \/ record = object_type) /\ start = middle).
  {
    destruct object_type as [| | | |tag args|];
      try (injection Record as <- <-; split; [now right|reflexivity]).
    destruct (kind_is (e_W E) tag DKTag) eqn:Tag.
    - apply pure_result in Record as [Read <-]. split; [left|reflexivity].
      exists tag, args. split; [reflexivity|split; [now apply kind_is_spec|]].
      eapply payload_of_sound. exact Read.
    - injection Record as <- <-. split; [now right|reflexivity].
  }
  destruct ReadRecord as [Shape ->].
  destruct record as [|fields| | | |]; try discriminate.
  apply mbind_ok in Run as (field_type & finish & Read & Run).
  apply lift_result in Read as [Field <-]. injection Run as <- <- <-.
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final Proof]. split; [exact Final|].
  eapply ExField; [exact Proof|exact Shape|]. exists fields. auto.
Qed.
End Constructs.
