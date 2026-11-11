(* The executable call branches consume checked types and translate their
   arguments in the caller's own context. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaExpr EaTypes EaTypesSound EaControlSound EaDefinedCallSound EaChecked.
From NM Require Import LegacyBodyJudgments.
From NM Require EaFunctionSound EaContextTableSound EaSatisfiesSound PublicContextFacts.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma cfn_exact : forall C f, cfn C f = nlook f (c_fns C).
Proof.
  intros C f. unfold cfn, c_fns.
  induction (c_provisions C) as [|[key value] rest IH]; [reflexivity|].
  destruct key as [type|val|fn|recv method inst|functor], value;
    cbn [cfn_go flat_map app nlook]; try exact IH.
  destruct (Nat.eqb f fn); [reflexivity|exact IH].
Qed.

Lemma cmth_exact : forall C receiver method,
  cmth C receiver method = mlook receiver method (c_mths C).
Proof.
  intros C receiver method. unfold cmth, c_mths.
  induction (c_provisions C) as [|[key value] rest IH]; [reflexivity|].
  destruct key as [type|val|fn|recv symbol inst|functor], value;
    cbn [cmth_go flat_map app mlook fst snd]; try exact IH.
  destruct (Nat.eqb method symbol), (ty_eqb receiver recv); try reflexivity; exact IH.
Qed.

Section Calls.
Variables (E : env) (R : rec) (P : program).
Let W := e_W E.
Variable valid : frame -> cx -> est -> Prop.
Variable post : est -> Prop.
Local Notation Expr := (ExPostAt E P valid post R).
Local Notation Action := (ExActionSound E P valid post).

Hypothesis Contexts : EaContextTableSound.ContextTableSound W.
Hypothesis CallerSection : forall F C s, valid F C s -> PublicContextFacts.CxSection C.
Hypothesis DemandCheck : DemandSound E.

Local Lemma fn_context : forall f formal needs,
  kind_sym W f = Some DKFn -> w_context W f = Some (formal, needs) -> CtxOf W f formal needs.
Proof. intros f formal needs Kind Found. specialize (Contexts _ _ _ Found). now rewrite Kind in Contexts. Qed.

Theorem plain_call_sound : forall n,
  (forall depth, depth < n -> Expr depth) ->
  forall F C path args expected f d name params result body,
  resolve_sym W (fr_mod F) path = Some f -> w_decl W f = Some d ->
  d_ast d = ADFn RPlain name params result body ->
  Action F C (AECall path None args) expected
    (ex E R (S n) F C (AECall path None args) expected).
Proof.
  intros n Below F C path args expected f d name params result body
    Resolved Declared Ast before [value t] after Run.
  cbn [ex] in Run. fold W in Run. rewrite Resolved in Run. cbn [mbind ret] in Run.
  rewrite Declared in Run. cbn [mbind ret] in Run. rewrite Ast in Run.
  assert (Kind : kind_sym W f = Some DKFn) by (unfold kind_sym; now rewrite Declared, Ast).
  assert (ReadAst : ast_of W f = Some (ADFn RPlain name params result body)) by
    (unfold ast_of; rewrite Declared; cbn [option_map]; now rewrite Ast).
  destruct body as [body|].
  - apply mbind_ok in Run as (prepared & started & Prepare & Run).
    unfold pure in Prepare.
    destruct (check_defined_call E f C None) as [call|message] eqn:Prepared; [|discriminate Prepare].
    injection Prepare as <- <-.
    apply mbind_ok in Run as (values & finish & Arguments & Run).
    injection Run as <- <- <-.
    destruct (args_post_sound E R P valid post n Below _ _ _ _ _ _ _ _ _ Arguments)
      as [Backward Checked].
    split; [exact Backward|]. intros Valid Post.
    destruct (Checked Valid Post) as [Final Typed].
    destruct (check_defined_call_sound E f C None call DemandCheck
      (CallerSection _ _ _ Valid) (fun formal needs => fn_context f formal needs Kind) Prepared)
      as (formal & needs & steps & called & Callee & Force & Demands & Types & Satisfied & Signature & Emit).
    split; [exact Final|]. rewrite Emit.
    eapply ExCallD; [exact Resolved|exact Kind|exists name, params, result, body; exact ReadAst|
      exact Callee|exact Demands|exact Satisfied|exact Types|exact Signature|exact Typed|exact Force].
  - apply mbind_ok in Run as (signature & started & Read & Run).
    destruct (cfn C f) as [g|] eqn:Found; [|discriminate Read].
    injection Read as <- <-.
    apply mbind_ok in Run as (values & finish & Arguments & Run).
    injection Run as <- <- <-.
    destruct (args_post_sound E R P valid post n Below _ _ _ _ _ _ _ _ _ Arguments)
      as [Backward Checked].
    split; [exact Backward|]. intros Valid Post.
    destruct (Checked Valid Post) as [Final Typed]. split; [exact Final|].
    eapply ExCallA; [exact Resolved|exact Kind|exists name, params, result; exact ReadAst|
      now rewrite <- cfn_exact|exact Typed].
Qed.

Theorem bracketed_plain_call_sound : forall n,
  (forall F C syntax expected, Action F C syntax expected (r_ex R F C syntax expected)) ->
  forall F C path brackets args expected f d name params result body,
  resolve_sym W (fr_mod F) path = Some f -> w_decl W f = Some d ->
  d_ast d = ADFn RPlain name params result body ->
  Action F C (AECall path (Some brackets) args) expected
    (ex E R (S n) F C (AECall path (Some brackets) args) expected).
Proof.
  intros n Rec F C path brackets args expected f d name params result body
    Resolved Declared Ast before [value t] after Run.
  cbn [ex] in Run. fold W in Run. rewrite Resolved in Run. cbn [mbind ret] in Run.
  rewrite Declared in Run. cbn [mbind ret] in Run. rewrite Ast in Run.
  apply mbind_ok in Run as ([] & started & Checked & Run).
  assert (Unchanged : started = before).
  {
    clear Run. revert before started Checked.
    induction brackets as [|b rest IH]; intros before started Checked; cbn [miter] in Checked.
    - now injection Checked.
    - apply mbind_ok in Checked as ([] & intermediate & Check & Rest).
      destruct b as [left_path syntax]. cbn [fst snd] in Check.
      destruct (resolve_sym W (fr_mod F) left_path) as [x|]; [|discriminate Check].
      destruct (kind_is W x DKType && nmem x (w_reqs W f)); [|discriminate Check].
      destruct syntax as [[right|] [member|] application]; try discriminate Check.
      injection Check as <-. now apply (IH _ _ Rest).
  }
  subst started.
  assert (Brackets : CallBrackets W (fr_mod F) f brackets).
  {
    apply Forall_forall. intros [part spec] Present.
    all: destruct (EaFunctionSound.miter_member_run _ _ _ _ _ Checked _ Present)
      as (started & checked & Item).
    all: cbn [fst snd] in Item.
    all: destruct (resolve_sym W (fr_mod F) part) as [x|] eqn:ResolvedPart; [|discriminate Item].
    all: destruct (kind_is W x DKType && nmem x (w_reqs W f)) eqn:RequiredType; [|discriminate Item].
    all: destruct spec as [[right|] [member|] application]; try discriminate Item.
    all: apply andb_true_iff in RequiredType as [Kind Required].
    all: split; [exists x; split; [exact ResolvedPart|split; [now apply kind_is_spec|now apply nmem_spec]]|
      exists right, application; reflexivity].
  }
  destruct (Rec _ _ _ _ _ _ _ Run) as [Backward TypedCall].
  split; [exact Backward|]. intros Valid Post.
  destruct (TypedCall Valid Post) as [Final Typed]. split; [exact Final|].
  eapply ExCallBr; [exact Resolved| |exact Brackets|exact Typed].
  unfold kind_sym. fold W. now rewrite Declared, Ast.
Qed.
End Calls.
