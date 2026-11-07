From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabFrame ElabInv FnReadonly FnPresentStable FnPresentEmit FnPresentSynth ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit.
From NM Require Import SynthTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma hs_synth_group : forall fuel,
  (forall c e x exp, HS (synth fuel c e x exp)) /\
  (forall c e a exp, HS (synth_arg fuel c e a exp)) /\
  (forall c e b exp, HS (synth_block fuel c e b exp)) /\
  (forall c e ss, HS (check_stmts fuel c e ss)) /\
  (forall c e s, HS (check_stmt fuel c e s)) /\
  (forall c e y, HS (check_bool fuel c e y)) /\
  (forall c e x exp, HS (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, HS (synth_match fuel c e s arms exp)) /\
  (forall g, HS (const_val fuel g)) /\
  (forall c e p a, HS (synth_path fuel c e p a)) /\
  (forall c e nm sg args, HS (check_args fuel c e nm sg args)) /\
  (forall c e p a args, HS (synth_call fuel c e p a args)) /\
  (forall c e p a fs, HS (synth_record fuel c e p a fs)) /\
  (forall c e o n, HS (synth_field fuel c e o n)) /\
  (forall c e o p args, HS (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, HS (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, HS (apply_family fuel c e fg th want)) /\
  (forall c e info, HS (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, HS (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, HS (lang_call fuel c e o n args why)) /\
  (forall c e ds why, HS (int_literal fuel c e ds why)) /\
  (forall c e v why, HS (float_of fuel c e v why)) /\
  (forall c e text, HS (synth_literal fuel c e text)) /\
  (forall c e sp y, HS (check_bind fuel c e sp y)) /\
  (forall c e y, HS (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, HS (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, HS (wrapper fuel pv)) /\
  (forall tg, HS (forwarder fuel tg)) /\
  (forall g, HSE(gs_id g)(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, HS (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, HS (apply_functor fuel c e sp)) /\
  (forall c e g, HS (apply_body fuel c e g)) /\
  (forall c e fg, HS (apply_abstract fuel c e fg)) /\
  (forall c e fg y, HS (bind_functor fuel c e fg y)) /\
  (forall fg bg, HS (link_body fuel fg bg)).
Proof.
 refine (@synth_preservation_group (@HS) (fun fuel g=>HSE(gs_id g)(lower_body fuel g)) _ (@hs_ret) (@hs_bind) (@hs_mmap) (@hs_mfold) (@hs_miter) (@hs_with_vars) (fun A m F W H O=>@hs_keeps_first A m F H O) (@hs_wrapper) (@hs_forwarder_step) (@hse_lower_body_step) (@hs_lambda_provider_step)).
 intro g;cbn[lower_body];hst.
Qed.

Lemma hs_synth : forall fuel c e x exp, HS (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (hs_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve hs_synth : hsdb.

Lemma hs_synth_arg : forall fuel c e a exp, HS (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (hs_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve hs_synth_arg : hsdb.

Lemma hs_synth_block : forall fuel c e b exp, HS (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (hs_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve hs_synth_block : hsdb.

Lemma hs_check_stmts : forall fuel c e ss, HS (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (hs_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve hs_check_stmts : hsdb.

Lemma hs_check_stmt : forall fuel c e s, HS (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve hs_check_stmt : hsdb.

Lemma hs_check_bool : forall fuel c e y, HS (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve hs_check_bool : hsdb.

Lemma hs_synth_if : forall fuel c e x exp, HS (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve hs_synth_if : hsdb.

Lemma hs_synth_match : forall fuel c e s arms exp, HS (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve hs_synth_match : hsdb.

Lemma hs_const_val : forall fuel g, HS (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve hs_const_val : hsdb.

Lemma hs_synth_path : forall fuel c e p a, HS (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve hs_synth_path : hsdb.

Lemma hs_check_args : forall fuel c e nm sg args, HS (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve hs_check_args : hsdb.

Lemma hs_synth_call : forall fuel c e p a args, HS (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve hs_synth_call : hsdb.

Lemma hs_synth_record : forall fuel c e p a fs, HS (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve hs_synth_record : hsdb.

Lemma hs_synth_field : forall fuel c e o n, HS (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve hs_synth_field : hsdb.

Lemma hs_synth_method : forall fuel c e o p args, HS (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve hs_synth_method : hsdb.

Lemma hs_provided_call : forall fuel c e ox oty m args, HS (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve hs_provided_call : hsdb.

Lemma hs_apply_family : forall fuel c e fg th want, HS (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve hs_apply_family : hsdb.

Lemma hs_demand_needs : forall fuel c e info, HS (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve hs_demand_needs : hsdb.

Lemma hs_synth_operator : forall fuel c e lm n op operand args, HS (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve hs_synth_operator : hsdb.

Lemma hs_lang_call : forall fuel c e o n args why, HS (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve hs_lang_call : hsdb.

Lemma hs_int_literal : forall fuel c e ds why, HS (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve hs_int_literal : hsdb.

Lemma hs_float_of : forall fuel c e v why, HS (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve hs_float_of : hsdb.

Lemma hs_synth_literal : forall fuel c e text, HS (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve hs_synth_literal : hsdb.

Lemma hs_check_bind : forall fuel c e sp y, HS (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve hs_check_bind : hsdb.

Lemma hs_expect_fn_provider : forall fuel c e y, HS (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve hs_expect_fn_provider : hsdb.

Lemma hs_match_fn_sig : forall fuel c e pv wanted kind, HS (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve hs_match_fn_sig : hsdb.

Lemma hs_wrapper : forall fuel pv, HS (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve hs_wrapper : hsdb.

Lemma hs_forwarder : forall fuel tg, HS (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve hs_forwarder : hsdb.

Lemma hse_lower_body : forall fuel g, HSE(gs_id g)(lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve hse_lower_body : hsdb.

Lemma hs_lambda_provider : forall fuel c e ps rt body bound sg this what, HS (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve hs_lambda_provider : hsdb.

Lemma hs_apply_functor : forall fuel c e sp, HS (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve hs_apply_functor : hsdb.

Lemma hs_apply_body : forall fuel c e g, HS (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve hs_apply_body : hsdb.

Lemma hs_apply_abstract : forall fuel c e fg, HS (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve hs_apply_abstract : hsdb.

Lemma hs_bind_functor : forall fuel c e fg y, HS (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve hs_bind_functor : hsdb.

Lemma hs_link_body : forall fuel fg bg, HS (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (hs_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve hs_link_body : hsdb.
