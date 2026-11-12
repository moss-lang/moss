(* Typed nominal construction, records and field projection. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaExpr EaControlSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaNumbering PublicTypeFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
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
  forall F C path application args expected s d name ps arity syntax,
  resolve_sym W (fr_mod F) path = Some s -> w_decl W s = Some d ->
  d_ast d = ADTag name ps arity syntax ->
  Action F C (AECall path application args) expected
    (ex E R (S n) F C (AECall path application args) expected).
Proof.
  intros n Below F C path application args expected s d name ps arity syntax
    Resolve Declared Tag before [value t] after Run.
  cbn [ex] in Run. fold W in Run. rewrite Resolve in Run. cbn [mbind ret] in Run.
  rewrite Declared in Run. cbn [mbind ret] in Run. rewrite Tag in Run.
  apply mbind_ok in Run as (rows & mid & Read & Run).
  apply pure_result in Read as [Rows <-].
  apply mbind_ok in Run as (arguments & mid & Read & Run).
  apply pure_result in Read as [Arguments <-].
  apply mbind_ok in Run as (payload & mid & Read & Run).
  apply pure_result in Read as [Payload <-].
  apply mbind_ok in Run as (y & started & Read & Run).
  apply lift_result in Read as [ArgumentsSyntax <-].
  assert (PayloadSyntax : TagCallPayload W s args y).
  { exists name, ps, arity, syntax. split; [|exact ArgumentsSyntax].
    unfold ast_of. rewrite Declared. cbn [option_map]. now rewrite Tag. }
  apply mbind_ok in Run as ([body actual] & finish & Child & Run).
  apply mbind_ok in Run as (checked & final & Check & Run).
  apply check_result in Check as [Fits [_ <-]].
  injection Run as <- <- <-.
  destruct (Below _ _ _ _ _ _ _ Child) as [Backward Typed].
  split; [exact Backward|]. intros Valid Post.
  destruct (Typed Valid Post) as [Final Body]. split; [exact Final|].
  eapply ExTag; [exact Resolve| |eapply brackets_sound; exact Rows|now apply tag_arguments_read|
    eapply payload_of_sound; exact Payload|exact PayloadSyntax|exact Body|now apply fitsb_exact].
  intros [bare_name Bare]. unfold ast_of in Bare. fold W in Bare.
  rewrite Declared in Bare. cbn in Bare. rewrite Tag in Bare. discriminate.
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
  apply check_result in Read as [Tag [_ <-]]. apply andb_true_iff in Tag as [Tag _].
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

Theorem synth_fields_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C fs,
  Checked post (valid F C) (fun _ => valid F C) (synth_fields E R n F C fs)
    (fun '(types, values) _ =>
      map fst fs = map fst types /\
      Forall2 (fun ft xf => fst xf = fst ft /\
        exists n v vt, n = fst ft /\ In (n, v) fs /\
          Ex W P F C (record_field_source n v) None (snd xf) vt /\ vt = snd ft) types values).
Proof.
  induction n as [|n IH]; intros Below F C fs before [types values] after Run; [discriminate|].
  cbn [synth_fields] in Run. destruct fs as [|[name syntax] rest].
  - injection Run as <- <- <-. split; [tauto|intros Valid _; repeat split; [exact Valid|constructor]].
  - apply mbind_ok in Run as ([value actual] & middle & Head & Run).
    apply mbind_ok in Run as ([field_types field_values] & final & Tail & Run).
    injection Run as <- <- <-.
    destruct (Below n ltac:(lia) _ _ _ _ _ _ _ Head) as [Earlier TypedHead].
    destruct (IH (fun depth Lt => Below depth ltac:(lia)) _ _ _ _ _ _ Tail) as [Backward TypedTail].
    split; [intros Post; now apply Earlier, Backward|]. intros Valid Post.
    destruct (TypedHead Valid (Backward Post)) as [Middle Typed].
    destruct (TypedTail Middle Post) as [Final [Names Fields]].
    split; [exact Final|]. split; [cbn; now f_equal|]. constructor.
    + split; [reflexivity|]. exists name, syntax, actual. cbn [fst snd].
      split; [reflexivity|split; [now left|split; [exact Typed|reflexivity]]].
    + eapply Forall2_impl; [|exact Fields].
      intros ft xf [Same (n' & v & t & Label & Present & Child & SameType)].
      split; [exact Same|]. exists n', v, t.
      split; [exact Label|split; [now right|split; assumption]].
Qed.

Theorem bare_record_expression_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C fields expected,
  Action F C (AEBareRecord fields) expected
    (ex E R (S n) F C (AEBareRecord fields) expected).
Proof.
  intros n Below F C fields expected before [value t] after Run.
  cbn [ex] in Run.
  apply mbind_ok in Run as (distinct & middle & Distinct & Run).
  destruct (suniq_dup (map fst fields)) eqn:NoDuplicates; [discriminate Distinct|].
  injection Distinct as _ <-.
  assert (Unique : NoDup (map fst fields)) by now apply EaNumbering.checked_parameters_distinct.
  destruct expected as [expected|].
  - destruct expected as [|field_types| | | |]; try discriminate.
    apply mbind_ok in Run as (checked_types & middle & TypeCheck & Run).
    destruct (suniq_dup (map fst field_types)) eqn:DistinctTypes; [discriminate TypeCheck|].
    injection TypeCheck as _ <-.
    apply mbind_ok in Run as (arity & middle & Check & Run).
    apply check_result in Check as [Arity [_ <-]]. apply Nat.eqb_eq in Arity.
    apply mbind_ok in Run as (values & finish & Fields & Run).
    injection Run as <- <- <-.
    destruct (record_fields_sound n Below _ _ _ _ _ _ _ Fields) as [Backward Typed].
    split; [exact Backward|]. intros Valid Post.
    destruct (Typed Valid Post) as [Final Evidence]. split; [exact Final|].
    eapply ExBareRecord; [exact Unique|exact Arity| |exact Evidence].
    split; [reflexivity|now apply EaNumbering.checked_parameters_distinct].
  - apply mbind_ok in Run as ([field_types values] & finish & Fields & Run).
    injection Run as <- <- <-.
    destruct (synth_fields_sound n Below _ _ _ _ _ _ Fields) as [Backward Typed].
    split; [exact Backward|]. intros Valid Post.
    destruct (Typed Valid Post) as [Final [Names Evidence]]. split; [exact Final|].
    eapply ExBareRecord; [exact Unique| |exact Names|exact Evidence].
    apply (f_equal (@List.length fld)) in Names. now rewrite !map_length in Names.
Qed.

Theorem field_expression_sound : forall n,
  Expr n ->
  forall F C receiver field bare expected,
  Action F C (AEField receiver field bare) expected (ex E R (S n) F C (AEField receiver field bare) expected).
Proof.
  intros n Below F C receiver field bare expected before [value t] after Run.
  cbn [ex] in Run.
  apply mbind_ok in Run as ([object object_type] & middle & Object & Run).
  destruct (Below _ _ _ _ _ _ _ Object) as [Backward Typed].
  apply mbind_ok in Run as (record & start & Record & Run).
  assert (ReadRecord :
    ((bare = false /\ exists tag args, object_type = TNom tag args /\ kind_sym W tag = Some DKTag /\
       PayloadOf W tag args record) \/ record = object_type) /\ start = middle).
  {
    destruct object_type as [| | | |tag args|];
      try (injection Record as <- <-; split; [now right|reflexivity]).
    destruct bare; cbn [negb andb] in Record.
    - injection Record as <- <-. split; [now right|reflexivity].
    - destruct (kind_is (e_W E) tag DKTag) eqn:Tag.
      + apply pure_result in Record as [Read <-]. split; [left|reflexivity].
        split; [reflexivity|]. exists tag, args. split; [reflexivity|split; [now apply kind_is_spec|]].
        eapply payload_of_sound. exact Read.
      + injection Record as <- <-. split; [now right|reflexivity].
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
