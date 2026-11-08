From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrame ElabInv FnReadonly FnPresentStable FnPresentEmit.
From NM Require Import SynthTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma pf_synth_group : forall fuel,
  (forall c e x exp, PF (synth fuel c e x exp)) /\
  (forall c e a exp, PF (synth_arg fuel c e a exp)) /\
  (forall c e b exp, PF (synth_block fuel c e b exp)) /\
  (forall c e ss, PF (check_stmts fuel c e ss)) /\
  (forall c e s, PF (check_stmt fuel c e s)) /\
  (forall c e y, PF (check_bool fuel c e y)) /\
  (forall c e x exp, PF (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, PF (synth_match fuel c e s arms exp)) /\
  (forall g, PF (const_val fuel g)) /\
  (forall c e p a, PF (synth_path fuel c e p a)) /\
  (forall c e nm sg args, PF (check_args fuel c e nm sg args)) /\
  (forall c e p a args, PF (synth_call fuel c e p a args)) /\
  (forall c e p a fs, PF (synth_record fuel c e p a fs)) /\
  (forall c e o n, PF (synth_field fuel c e o n)) /\
  (forall c e o p args, PF (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, PF (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, PF (apply_family fuel c e fg th want)) /\
  (forall c e info, PF (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, PF (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, PF (lang_call fuel c e o n args why)) /\
  (forall c e ds why, PF (int_literal fuel c e ds why)) /\
  (forall c e v why, PF (float_of fuel c e v why)) /\
  (forall c e text, PF (synth_literal fuel c e text)) /\
  (forall c e sp y, PF (check_bind fuel c e sp y)) /\
  (forall c e y, PF (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, PF (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, PF (wrapper fuel pv)) /\
  (forall tg, PF (forwarder fuel tg)) /\
  (forall g, PF (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, PF (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, PF (apply_functor fuel c e sp)) /\
  (forall c e g, PF (apply_body fuel c e g)) /\
  (forall c e fg, PF (apply_abstract fuel c e fg)) /\
  (forall c e fg y, PF (bind_functor fuel c e fg y)) /\
  (forall fg bg, PF (link_body fuel fg bg)).
Proof.
  refine (@synth_preservation_group (@PF) (fun fuel g=>PF(lower_body fuel g)) _ (@pf_ret) (@pf_bind) (@pf_mmap) (@pf_mfold) (@pf_miter) (@pf_with_vars) (fun A m F W H O=>@pf_keeps A m O F) (@pf_wrapper) (@pf_forwarder_step) (@pf_lower_body_step) (@pf_lambda_provider_step)).
  intro g;cbn[lower_body];pft.
Qed.

Lemma pf_synth : forall fuel c e x exp, PF (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (pf_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve pf_synth : pfdb.

Lemma pf_synth_arg : forall fuel c e a exp, PF (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (pf_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve pf_synth_arg : pfdb.

Lemma pf_synth_block : forall fuel c e b exp, PF (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (pf_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve pf_synth_block : pfdb.

Lemma pf_check_stmts : forall fuel c e ss, PF (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (pf_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve pf_check_stmts : pfdb.

Lemma pf_check_stmt : forall fuel c e s, PF (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve pf_check_stmt : pfdb.

Lemma pf_check_bool : forall fuel c e y, PF (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve pf_check_bool : pfdb.

Lemma pf_synth_if : forall fuel c e x exp, PF (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve pf_synth_if : pfdb.

Lemma pf_synth_match : forall fuel c e s arms exp, PF (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve pf_synth_match : pfdb.

Lemma pf_const_val : forall fuel g, PF (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve pf_const_val : pfdb.

Lemma pf_synth_path : forall fuel c e p a, PF (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve pf_synth_path : pfdb.

Lemma pf_check_args : forall fuel c e nm sg args, PF (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve pf_check_args : pfdb.

Lemma pf_synth_call : forall fuel c e p a args, PF (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve pf_synth_call : pfdb.

Lemma pf_synth_record : forall fuel c e p a fs, PF (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve pf_synth_record : pfdb.

Lemma pf_synth_field : forall fuel c e o n, PF (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve pf_synth_field : pfdb.

Lemma pf_synth_method : forall fuel c e o p args, PF (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve pf_synth_method : pfdb.

Lemma pf_provided_call : forall fuel c e ox oty m args, PF (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve pf_provided_call : pfdb.

Lemma pf_apply_family : forall fuel c e fg th want, PF (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve pf_apply_family : pfdb.

Lemma pf_demand_needs : forall fuel c e info, PF (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve pf_demand_needs : pfdb.

Lemma pf_synth_operator : forall fuel c e lm n op operand args, PF (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve pf_synth_operator : pfdb.

Lemma pf_lang_call : forall fuel c e o n args why, PF (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve pf_lang_call : pfdb.

Lemma pf_int_literal : forall fuel c e ds why, PF (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve pf_int_literal : pfdb.

Lemma pf_float_of : forall fuel c e v why, PF (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve pf_float_of : pfdb.

Lemma pf_synth_literal : forall fuel c e text, PF (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve pf_synth_literal : pfdb.

Lemma pf_check_bind : forall fuel c e sp y, PF (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve pf_check_bind : pfdb.

Lemma pf_expect_fn_provider : forall fuel c e y, PF (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve pf_expect_fn_provider : pfdb.

Lemma pf_match_fn_sig : forall fuel c e pv wanted kind, PF (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve pf_match_fn_sig : pfdb.

Lemma pf_wrapper : forall fuel pv, PF (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve pf_wrapper : pfdb.

Lemma pf_forwarder : forall fuel tg, PF (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve pf_forwarder : pfdb.

Lemma pf_lower_body : forall fuel g, PF (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve pf_lower_body : pfdb.

Lemma pf_lambda_provider : forall fuel c e ps rt body bound sg this what, PF (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve pf_lambda_provider : pfdb.

Lemma pf_apply_functor : forall fuel c e sp, PF (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve pf_apply_functor : pfdb.

Lemma pf_apply_body : forall fuel c e g, PF (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve pf_apply_body : pfdb.

Lemma pf_apply_abstract : forall fuel c e fg, PF (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve pf_apply_abstract : pfdb.

Lemma pf_bind_functor : forall fuel c e fg y, PF (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve pf_bind_functor : pfdb.

Lemma pf_link_body : forall fuel fg bg, PF (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (pf_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve pf_link_body : pfdb.
