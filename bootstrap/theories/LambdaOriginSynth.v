From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabFrame ElabInv FnReadonly SymReadonly
 ForwarderHistory FnBound FnBoundEmit FnBoundSynth FnPresentStable FnPresentEmit FnPresentSynth
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 LambdaOrigin LambdaOriginOps LambdaOriginEmit.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma lh_synth_group : forall fuel,
  (forall c e x exp, LH (synth fuel c e x exp)) /\
  (forall c e a exp, LH (synth_arg fuel c e a exp)) /\
  (forall c e b exp, LH (synth_block fuel c e b exp)) /\
  (forall c e ss, LH (check_stmts fuel c e ss)) /\
  (forall c e s, LH (check_stmt fuel c e s)) /\
  (forall c e y, LH (check_bool fuel c e y)) /\
  (forall c e x exp, LH (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, LH (synth_match fuel c e s arms exp)) /\
  (forall g, LH (const_val fuel g)) /\
  (forall c e p a, LH (synth_path fuel c e p a)) /\
  (forall c e nm sg args, LH (check_args fuel c e nm sg args)) /\
  (forall c e p a args, LH (synth_call fuel c e p a args)) /\
  (forall c e p a fs, LH (synth_record fuel c e p a fs)) /\
  (forall c e o n, LH (synth_field fuel c e o n)) /\
  (forall c e o p args, LH (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, LH (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, LH (apply_family fuel c e fg th want)) /\
  (forall c e info, LH (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, LH (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, LH (lang_call fuel c e o n args why)) /\
  (forall c e ds why, LH (int_literal fuel c e ds why)) /\
  (forall c e v why, LH (float_of fuel c e v why)) /\
  (forall c e text, LH (synth_literal fuel c e text)) /\
  (forall c e sp y, LH (check_bind fuel c e sp y)) /\
  (forall c e y, LH (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, LH (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, LH (wrapper fuel pv)) /\
  (forall tg, LH (forwarder fuel tg)) /\
  (forall g, LH_at(gs_id g)(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, LH (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, LH (apply_functor fuel c e sp)) /\
  (forall c e g, LH (apply_body fuel c e g)) /\
  (forall c e fg, LH (apply_abstract fuel c e fg)) /\
  (forall c e fg y, LH (bind_functor fuel c e fg y)) /\
  (forall fg bg, LH (link_body fuel fg bg)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; lht. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  all: solve [fintros; first [apply lh_wrapper|apply lh_forwarder_step;exact I28|apply lha_lower_body_step;exact I2|apply lh_lambda_provider_step;exact I2|cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; lht]].
Qed.

Lemma lh_synth : forall fuel c e x exp, LH (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (lh_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve lh_synth : lhdb.

Lemma lh_synth_arg : forall fuel c e a exp, LH (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (lh_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve lh_synth_arg : lhdb.

Lemma lh_synth_block : forall fuel c e b exp, LH (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (lh_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve lh_synth_block : lhdb.

Lemma lh_check_stmts : forall fuel c e ss, LH (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (lh_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve lh_check_stmts : lhdb.

Lemma lh_check_stmt : forall fuel c e s, LH (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve lh_check_stmt : lhdb.

Lemma lh_check_bool : forall fuel c e y, LH (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve lh_check_bool : lhdb.

Lemma lh_synth_if : forall fuel c e x exp, LH (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve lh_synth_if : lhdb.

Lemma lh_synth_match : forall fuel c e s arms exp, LH (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve lh_synth_match : lhdb.

Lemma lh_const_val : forall fuel g, LH (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve lh_const_val : lhdb.

Lemma lh_synth_path : forall fuel c e p a, LH (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve lh_synth_path : lhdb.

Lemma lh_check_args : forall fuel c e nm sg args, LH (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve lh_check_args : lhdb.

Lemma lh_synth_call : forall fuel c e p a args, LH (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve lh_synth_call : lhdb.

Lemma lh_synth_record : forall fuel c e p a fs, LH (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve lh_synth_record : lhdb.

Lemma lh_synth_field : forall fuel c e o n, LH (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve lh_synth_field : lhdb.

Lemma lh_synth_method : forall fuel c e o p args, LH (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve lh_synth_method : lhdb.

Lemma lh_provided_call : forall fuel c e ox oty m args, LH (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve lh_provided_call : lhdb.

Lemma lh_apply_family : forall fuel c e fg th want, LH (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve lh_apply_family : lhdb.

Lemma lh_demand_needs : forall fuel c e info, LH (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve lh_demand_needs : lhdb.

Lemma lh_synth_operator : forall fuel c e lm n op operand args, LH (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve lh_synth_operator : lhdb.

Lemma lh_lang_call : forall fuel c e o n args why, LH (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve lh_lang_call : lhdb.

Lemma lh_int_literal : forall fuel c e ds why, LH (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve lh_int_literal : lhdb.

Lemma lh_float_of : forall fuel c e v why, LH (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve lh_float_of : lhdb.

Lemma lh_synth_literal : forall fuel c e text, LH (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve lh_synth_literal : lhdb.

Lemma lh_check_bind : forall fuel c e sp y, LH (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve lh_check_bind : lhdb.

Lemma lh_expect_fn_provider : forall fuel c e y, LH (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve lh_expect_fn_provider : lhdb.

Lemma lh_match_fn_sig : forall fuel c e pv wanted kind, LH (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve lh_match_fn_sig : lhdb.

Lemma lh_wrapper : forall fuel pv, LH (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve lh_wrapper : lhdb.

Lemma lh_forwarder : forall fuel tg, LH (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve lh_forwarder : lhdb.

Lemma lh_lower_body : forall fuel g, LH_at(gs_id g)(lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve lh_lower_body : lhdb.

Lemma lh_lambda_provider : forall fuel c e ps rt body bound sg this what, LH (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve lh_lambda_provider : lhdb.

Lemma lh_apply_functor : forall fuel c e sp, LH (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve lh_apply_functor : lhdb.

Lemma lh_apply_body : forall fuel c e g, LH (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve lh_apply_body : lhdb.

Lemma lh_apply_abstract : forall fuel c e fg, LH (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve lh_apply_abstract : lhdb.

Lemma lh_bind_functor : forall fuel c e fg y, LH (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve lh_bind_functor : lhdb.

Lemma lh_link_body : forall fuel fg bg, LH (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lh_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve lh_link_body : lhdb.
