From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabFrame ElabInv FnReadonly FnBound FnBoundEmit.
From NM Require Import SynthTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Local Lemma bf_pure_input : forall A (f : st -> A),
  BF (fun s => Good (f s, s)).
Proof.
  intros A f. apply bf_keeps.
  - apply ok_reader. intros s a final Run. injection Run as _ <-. reflexivity.
  - unfold keeps. intros s a final Run. injection Run as _ <-. reflexivity.
Qed.
#[local] Hint Resolve bf_pure_input : bfdb.

Lemma bf_synth_group : forall fuel,
  (forall c e x exp, BF (synth fuel c e x exp)) /\
  (forall c e a exp, BF (synth_arg fuel c e a exp)) /\
  (forall c e b exp, BF (synth_block fuel c e b exp)) /\
  (forall c e ss, BF (check_stmts fuel c e ss)) /\
  (forall c e s, BF (check_stmt fuel c e s)) /\
  (forall c e y, BF (check_bool fuel c e y)) /\
  (forall c e x exp, BF (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, BF (synth_match fuel c e s arms exp)) /\
  (forall g, BF (const_val fuel g)) /\
  (forall c e p a, BF (synth_path fuel c e p a)) /\
  (forall c e nm sg args, BF (check_args fuel c e nm sg args)) /\
  (forall c e p a args, BF (synth_call fuel c e p a args)) /\
  (forall c e p a fs, BF (synth_record fuel c e p a fs)) /\
  (forall c e o n, BF (synth_field fuel c e o n)) /\
  (forall c e o p args, BF (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, BF (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, BF (apply_family fuel c e fg th want)) /\
  (forall c e info, BF (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, BF (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, BF (lang_call fuel c e o n args why)) /\
  (forall c e ds why, BF (int_literal fuel c e ds why)) /\
  (forall c e v why, BF (float_of fuel c e v why)) /\
  (forall c e text, BF (synth_literal fuel c e text)) /\
  (forall c e sp y, BF (check_bind fuel c e sp y)) /\
  (forall c e y, BF (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, BF (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, BF (wrapper fuel pv)) /\
  (forall tg, BF (forwarder fuel tg)) /\
  (forall g, BF_at (gs_id g) (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, BF (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, BF (apply_functor fuel c e sp)) /\
  (forall c e g, BF (apply_body fuel c e g)) /\
  (forall c e fg, BF (apply_abstract fuel c e fg)) /\
  (forall c e fg y, BF (bind_functor fuel c e fg y)) /\
  (forall fg bg, BF (link_body fuel fg bg)).
Proof.
  refine (@synth_preservation_group (@BF) (fun fuel g=>BF_at(gs_id g)(lower_body fuel g)) _ (@bf_ret) (@bf_bind) (@bf_mmap) (@bf_mfold) (@bf_miter) (@bf_with_vars) (fun A m F W H O=>@bf_keeps A m O F) (@bf_wrapper) (@bf_forwarder_step) (@bf_lower_body_step) (@bf_lambda_provider_step)).
  intro g;cbn[lower_body];bft.
Qed.

Lemma bf_synth : forall fuel c e x exp, BF (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (bf_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve bf_synth : bfdb.

Lemma bf_synth_arg : forall fuel c e a exp, BF (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (bf_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve bf_synth_arg : bfdb.

Lemma bf_synth_block : forall fuel c e b exp, BF (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (bf_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve bf_synth_block : bfdb.

Lemma bf_check_stmts : forall fuel c e ss, BF (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (bf_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve bf_check_stmts : bfdb.

Lemma bf_check_stmt : forall fuel c e s, BF (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve bf_check_stmt : bfdb.

Lemma bf_check_bool : forall fuel c e y, BF (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve bf_check_bool : bfdb.

Lemma bf_synth_if : forall fuel c e x exp, BF (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve bf_synth_if : bfdb.

Lemma bf_synth_match : forall fuel c e s arms exp, BF (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve bf_synth_match : bfdb.

Lemma bf_const_val : forall fuel g, BF (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve bf_const_val : bfdb.

Lemma bf_synth_path : forall fuel c e p a, BF (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve bf_synth_path : bfdb.

Lemma bf_check_args : forall fuel c e nm sg args, BF (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve bf_check_args : bfdb.

Lemma bf_synth_call : forall fuel c e p a args, BF (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve bf_synth_call : bfdb.

Lemma bf_synth_record : forall fuel c e p a fs, BF (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve bf_synth_record : bfdb.

Lemma bf_synth_field : forall fuel c e o n, BF (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve bf_synth_field : bfdb.

Lemma bf_synth_method : forall fuel c e o p args, BF (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve bf_synth_method : bfdb.

Lemma bf_provided_call : forall fuel c e ox oty m args, BF (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve bf_provided_call : bfdb.

Lemma bf_apply_family : forall fuel c e fg th want, BF (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve bf_apply_family : bfdb.

Lemma bf_demand_needs : forall fuel c e info, BF (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve bf_demand_needs : bfdb.

Lemma bf_synth_operator : forall fuel c e lm n op operand args, BF (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve bf_synth_operator : bfdb.

Lemma bf_lang_call : forall fuel c e o n args why, BF (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve bf_lang_call : bfdb.

Lemma bf_int_literal : forall fuel c e ds why, BF (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve bf_int_literal : bfdb.

Lemma bf_float_of : forall fuel c e v why, BF (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve bf_float_of : bfdb.

Lemma bf_synth_literal : forall fuel c e text, BF (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve bf_synth_literal : bfdb.

Lemma bf_check_bind : forall fuel c e sp y, BF (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve bf_check_bind : bfdb.

Lemma bf_expect_fn_provider : forall fuel c e y, BF (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve bf_expect_fn_provider : bfdb.

Lemma bf_match_fn_sig : forall fuel c e pv wanted kind, BF (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve bf_match_fn_sig : bfdb.

Lemma bf_wrapper : forall fuel pv, BF (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve bf_wrapper : bfdb.

Lemma bf_forwarder : forall fuel tg, BF (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve bf_forwarder : bfdb.

Lemma bf_lower_body_at : forall fuel g, BF_at (gs_id g) (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve bf_lower_body_at : bfdb.

Lemma bf_lambda_provider : forall fuel c e ps rt body bound sg this what, BF (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve bf_lambda_provider : bfdb.

Lemma bf_apply_functor : forall fuel c e sp, BF (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve bf_apply_functor : bfdb.

Lemma bf_apply_body : forall fuel c e g, BF (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve bf_apply_body : bfdb.

Lemma bf_apply_abstract : forall fuel c e fg, BF (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve bf_apply_abstract : bfdb.

Lemma bf_bind_functor : forall fuel c e fg y, BF (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve bf_bind_functor : bfdb.

Lemma bf_link_body : forall fuel fg bg, BF (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (bf_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve bf_link_body : bfdb.
