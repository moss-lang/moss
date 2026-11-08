From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrame ElabInv FwdReadonly FwdInvariant FwdPreserve.
From NM Require Import SynthTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma okF_synth_group : forall fuel,
  (forall c e x exp, okF (synth fuel c e x exp)) /\
  (forall c e a exp, okF (synth_arg fuel c e a exp)) /\
  (forall c e b exp, okF (synth_block fuel c e b exp)) /\
  (forall c e ss, okF (check_stmts fuel c e ss)) /\
  (forall c e s, okF (check_stmt fuel c e s)) /\
  (forall c e y, okF (check_bool fuel c e y)) /\
  (forall c e x exp, okF (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, okF (synth_match fuel c e s arms exp)) /\
  (forall g, okF (const_val fuel g)) /\
  (forall c e p a, okF (synth_path fuel c e p a)) /\
  (forall c e nm sg args, okF (check_args fuel c e nm sg args)) /\
  (forall c e p a args, okF (synth_call fuel c e p a args)) /\
  (forall c e p a fs, okF (synth_record fuel c e p a fs)) /\
  (forall c e o n, okF (synth_field fuel c e o n)) /\
  (forall c e o p args, okF (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, okF (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, okF (apply_family fuel c e fg th want)) /\
  (forall c e info, okF (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, okF (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, okF (lang_call fuel c e o n args why)) /\
  (forall c e ds why, okF (int_literal fuel c e ds why)) /\
  (forall c e v why, okF (float_of fuel c e v why)) /\
  (forall c e text, okF (synth_literal fuel c e text)) /\
  (forall c e sp y, okF (check_bind fuel c e sp y)) /\
  (forall c e y, okF (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, okF (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, okF (wrapper fuel pv)) /\
  (forall tg, okF (forwarder fuel tg)) /\
  (forall g, okF (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, okF (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, okF (apply_functor fuel c e sp)) /\
  (forall c e g, okF (apply_body fuel c e g)) /\
  (forall c e fg, okF (apply_abstract fuel c e fg)) /\
  (forall c e fg y, okF (bind_functor fuel c e fg y)) /\
  (forall fg bg, okF (link_body fuel fg bg)).
Proof.
  refine (@synth_preservation_group (@okF) (fun fuel g=>okF(lower_body fuel g)) _ (@okF_ret) (@okF_bind) (@okF_mmap) (@okF_mfold) (@okF_miter) (@okF_with_vars) (fun A m F W H O=>@okF_readonly A m O W) (@okF_wrapper) (@okF_forwarder_step) (@okF_lower_body_step) (@okF_lambda_provider_step)).
  intro g;cbn[lower_body];fwt.
Qed.

Lemma okF_synth : forall fuel c e x exp, okF (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (okF_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve okF_synth : fwdb.

Lemma okF_synth_arg : forall fuel c e a exp, okF (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (okF_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve okF_synth_arg : fwdb.

Lemma okF_synth_block : forall fuel c e b exp, okF (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (okF_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve okF_synth_block : fwdb.

Lemma okF_check_stmts : forall fuel c e ss, okF (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (okF_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve okF_check_stmts : fwdb.

Lemma okF_check_stmt : forall fuel c e s, okF (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve okF_check_stmt : fwdb.

Lemma okF_check_bool : forall fuel c e y, okF (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve okF_check_bool : fwdb.

Lemma okF_synth_if : forall fuel c e x exp, okF (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve okF_synth_if : fwdb.

Lemma okF_synth_match : forall fuel c e s arms exp, okF (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve okF_synth_match : fwdb.

Lemma okF_const_val : forall fuel g, okF (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve okF_const_val : fwdb.

Lemma okF_synth_path : forall fuel c e p a, okF (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve okF_synth_path : fwdb.

Lemma okF_check_args : forall fuel c e nm sg args, okF (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve okF_check_args : fwdb.

Lemma okF_synth_call : forall fuel c e p a args, okF (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve okF_synth_call : fwdb.

Lemma okF_synth_record : forall fuel c e p a fs, okF (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve okF_synth_record : fwdb.

Lemma okF_synth_field : forall fuel c e o n, okF (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve okF_synth_field : fwdb.

Lemma okF_synth_method : forall fuel c e o p args, okF (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve okF_synth_method : fwdb.

Lemma okF_provided_call : forall fuel c e ox oty m args, okF (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve okF_provided_call : fwdb.

Lemma okF_apply_family : forall fuel c e fg th want, okF (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve okF_apply_family : fwdb.

Lemma okF_demand_needs : forall fuel c e info, okF (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve okF_demand_needs : fwdb.

Lemma okF_synth_operator : forall fuel c e lm n op operand args, okF (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve okF_synth_operator : fwdb.

Lemma okF_lang_call : forall fuel c e o n args why, okF (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve okF_lang_call : fwdb.

Lemma okF_int_literal : forall fuel c e ds why, okF (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve okF_int_literal : fwdb.

Lemma okF_float_of : forall fuel c e v why, okF (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve okF_float_of : fwdb.

Lemma okF_synth_literal : forall fuel c e text, okF (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve okF_synth_literal : fwdb.

Lemma okF_check_bind : forall fuel c e sp y, okF (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve okF_check_bind : fwdb.

Lemma okF_expect_fn_provider : forall fuel c e y, okF (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve okF_expect_fn_provider : fwdb.

Lemma okF_match_fn_sig : forall fuel c e pv wanted kind, okF (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve okF_match_fn_sig : fwdb.

Lemma okF_wrapper : forall fuel pv, okF (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve okF_wrapper : fwdb.

Lemma okF_forwarder : forall fuel tg, okF (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve okF_forwarder : fwdb.

Lemma okF_lower_body : forall fuel g, okF (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve okF_lower_body : fwdb.

Lemma okF_lambda_provider : forall fuel c e ps rt body bound sg this what, okF (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve okF_lambda_provider : fwdb.

Lemma okF_apply_functor : forall fuel c e sp, okF (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve okF_apply_functor : fwdb.

Lemma okF_apply_body : forall fuel c e g, okF (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve okF_apply_body : fwdb.

Lemma okF_apply_abstract : forall fuel c e fg, okF (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve okF_apply_abstract : fwdb.

Lemma okF_bind_functor : forall fuel c e fg y, okF (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve okF_bind_functor : fwdb.

Lemma okF_link_body : forall fuel fg bg, okF (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (okF_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve okF_link_body : fwdb.
