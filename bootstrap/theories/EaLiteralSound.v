(* Numeric text has the stated lexical derivation, and literal trees check
   and translate through the same provisions as ordinary calls. *)
From Stdlib Require Import List String Bool Arith Lia.
From Stdlib Require Import Strings.Byte.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaDecide EaExpr EaFamily EaTypes EaTypesSound EaControlSound
  EaCallExpressionSound EaFamilyUseSound.
From NM Require Import LegacyBodyJudgments.
From NM Require FrontEnd Parse PublicTypeFacts PublicContextFacts EaMergeSound EaSatisfiesSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Local Lemma lex_first_raw : forall i input tokens,
  Syntax.Lexes i input tokens ->
  forall kind text offset rest, tokens = Syntax.Tok kind text offset :: rest ->
  kind <> Syntax.EOF ->
  exists skipped raw tail, input = skipped ++ raw ++ tail /\
    text = String.string_of_list_byte raw /\ Syntax.Lexeme raw kind tail.
Proof.
  intros i input tokens Lexed. induction Lexed as [i|i c r ts Space Read IH|
    i comment r ts Comment Ending Read IH|i raw k tail tokens First Read IH];
    intros kind text offset rest Tokens NotEnd.
  - exfalso. apply NotEnd. congruence.
  - destruct (IH _ _ _ _ Tokens NotEnd) as (skipped & raw & tail & -> & Text & ReadRaw).
    exists (c :: skipped), raw, tail. auto.
  - destruct (IH _ _ _ _ Tokens NotEnd) as (skipped & raw & tail & -> & Text & ReadRaw).
    exists ("#"%byte :: comment ++ skipped), raw, tail.
    split; [cbn [app]; now rewrite <- app_assoc|auto].
  - inversion Tokens; subst kind text offset rest.
    exists [], raw, tail. auto.
Qed.

Theorem numeric_text_sound : forall text, numeric_text text = true -> NumericText text.
Proof.
  intros text Checked. unfold numeric_text in Checked.
  destruct (Parse.lex (String.list_byte_of_string text)) as [tokens|message offset] eqn:Lexed;
    [|discriminate].
  destruct tokens as [|[kind raw offset] [|[end_kind end_raw end_offset] [|extra rest]]];
    try discriminate.
  destruct end_kind; try discriminate.
  apply andb_true_iff in Checked as [Checks Normal].
  apply andb_true_iff in Checks as [Kind Text].
  apply String.eqb_eq in Text. subst raw.
  apply String.eqb_eq in Normal.
  assert (Numeric : kind = Syntax.INTEGER \/ kind = Syntax.DECIMAL).
  {
    apply orb_true_iff in Kind as [Integer|Decimal].
    - left. destruct (FrontEnd.token_kind_eqb_spec kind Syntax.INTEGER); congruence.
    - right. destruct (FrontEnd.token_kind_eqb_spec kind Syntax.DECIMAL); congruence.
  }
  assert (NotEnd : kind <> Syntax.EOF).
  { destruct Numeric as [-> | ->]; discriminate. }
  apply FrontEnd.lex_sound in Lexed.
  destruct (lex_first_raw _ _ _ Lexed _ _ _ _ eq_refl NotEnd)
    as (skipped & raw & tail & Bytes & Text & Read).
  rewrite Text, String.list_byte_of_string_of_list_byte in Bytes.
  apply (f_equal (@List.length _)) in Bytes. rewrite !app_length in Bytes.
  assert (Empty : List.length skipped = 0 /\ List.length tail = 0) by lia.
  destruct Empty as [Skipped Tail]. apply length_zero_iff_nil in Tail. subst tail.
  exists text, kind. split.
  - rewrite Text, String.list_byte_of_string_of_list_byte. exact Read.
  - exact (conj Numeric Normal).
  - destruct end_kind; discriminate Checked.
Qed.

Lemma cval_exact : forall C v, cval C v = nlook v (c_vals C).
Proof.
  intros C v. unfold cval, c_vals.
  induction (c_provisions C) as [|[key value] rest IH]; [reflexivity|].
  destruct key as [type|val|fn|recv method inst|functor], value;
    cbn [cval_go flat_map app nlook]; try exact IH.
  destruct (Nat.eqb v val); [reflexivity|exact IH].
Qed.

Local Lemma option_read : forall A (read : option A) message value,
  ropt read message = Ok value -> read = Some value.
Proof. intros A [found|] message value Run; [now injection Run as <-|discriminate]. Qed.

Section Literals.
Variable E : env.
Let W := e_W E.
Variable C : cx.
Hypothesis Sectioned : PublicContextFacts.CxSection C.
Hypothesis Families : FamilyUseSound E.

Local Lemma lit_op_sound : forall t m left right code,
  lit_op E C t m left right = Ok code -> LitOp W C t m left right code.
Proof.
  intros t m left right code Run. unfold lit_op in Run.
  destruct (cmth C t m) as [signature|] eqn:InScope.
  - destruct (fsig_eqb signature (FSig [t] t)) eqn:Fits; [|discriminate].
    apply EaMergeSound.fsig_eqb_spec in Fits. subst signature. injection Run as <-.
    unfold rk. change (LitOp W C t m left right (EMethod left m (rkof C (mkey t m)) [right])).
    rewrite EaSatisfiesSound.rkof_exact. apply LitOpIn.
    now rewrite <- cmth_exact.
  - apply rbind_ok in Run as ([ [[[[u matching] arguments] sm] result] keys ] & Family & Run).
    destruct (cmth result t m) as [signature|] eqn:Found; [|discriminate].
    destruct (fsig_eqb signature (FSig [t] t)) eqn:Fits; [|discriminate].
    apply EaMergeSound.fsig_eqb_spec in Fits. subst signature. injection Run as <-.
    destruct (Families _ _ _ _ _ _ _ _ _ _ Sectioned Sectioned Family)
      as (formal & needs & App & Match & Present & Only & InForce & Map).
    change (rk result t m) with (rkof result (mkey t m)).
    rewrite Map, (map_ext _ _ (EaSatisfiesSound.rkof_exact result)), EaSatisfiesSound.rkof_exact.
    change (LitOp W C t m left right
      (translate_family_use C (FamilyUse u matching arguments formal needs result keys)
        (EMethod left m (rkey result t m) [right]))).
    eapply LitOpFamily; try eassumption; now rewrite <- cmth_exact.
Qed.

Theorem literal_sound : forall why tree code t,
  literal E why C tree = Ok (code, t) -> Literal W C tree code t.
Proof.
  intros why tree. induction tree; intros code t Run; cbn [literal] in Run.
  all: try solve [apply rbind_ok in Run as ([left lt] & Left & Run);
      apply rbind_ok in Run as ([right rt] & Right & Run);
      apply rbind_ok in Run as (fits & Fits & Run);
      destruct (ty_eqb lt rt) eqn:Equal; [|discriminate Fits];
      apply PublicTypeFacts.ty_eqb_spec in Equal; subst rt;
      apply rbind_ok in Run as (method & Read & Run);
      apply rbind_ok in Run as (body & Op & Run); injection Run as <- <-;
      first [eapply LiteralAdd|eapply LiteralMul|eapply LiteralDiv];
      [eapply IHtree1; exact Left|eapply IHtree2; exact Right|
       eapply lang_sym_sound, option_read; exact Read|eapply lit_op_sound; exact Op]].
  - apply rbind_ok in Run as (zero & Read & Run).
    apply rbind_ok in Run as (zt & Found & Run).
    injection Run as <- <-. apply LiteralZero.
    + eapply lang_sym_sound, option_read; exact Read.
    + rewrite <- cval_exact. eapply option_read; exact Found.
  - apply rbind_ok in Run as (one & Read & Run).
    apply rbind_ok in Run as (ot & Found & Run).
    injection Run as <- <-. apply LiteralOne.
    + eapply lang_sym_sound, option_read; exact Read.
    + rewrite <- cval_exact. eapply option_read; exact Found.

  - apply rbind_ok in Run as ([integer it] & Integer & Run).
    apply rbind_ok in Run as (float & Read & Run).
    destruct (cfn C float) as [[params result]|] eqn:Found; [|discriminate].
    destruct params as [|param [|extra rest]]; try discriminate.
    apply rbind_ok in Run as (checked & Check & Run).
    destruct (fitsb it param) eqn:Fits; [|discriminate Check].
    injection Run as <- <-. eapply LiteralFloat with (tp := param).
    + eapply IHtree. exact Integer.
    + eapply lang_sym_sound, option_read; exact Read.
    + now rewrite <- cfn_exact.
    + now apply fitsb_exact.
Qed.
End Literals.

From NM Require Import EaChecked.

Theorem literal_expression_sound : forall E R P (valid : frame -> cx -> est -> Prop) post n F C text expected,
  (forall state, valid F C state -> PublicContextFacts.CxSection C) ->
  FamilyUseSound E ->
  ExActionSound E P valid post F C (AELit text) expected (ex E R (S n) F C (AELit text) expected).
Proof.
  intros E R P valid post n F C text expected Sectioned Families before [code t] after Run.
  cbn [ex] in Run.
  destruct (numeric_text text) eqn:Numeric; [|discriminate].
  cbn [mbind ret pure] in Run.
  destruct (literal E ("the literal `" ++ text ++ "`") C (literal_tree_of text)) as [[compiled ty]|message]
    eqn:Literal; [|discriminate].
  injection Run as <- <- <-.
  split; [tauto|]. intros Valid _.
  split; [exact Valid|]. eapply ExLit; [now apply numeric_text_sound|].
  eapply literal_sound; [exact (Sectioned before Valid)|exact Families|exact Literal].
Qed.
