From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabFrame ElabInv FnReadonly LambdaReadonly LambdaReadonlyEmit.
From NM Require Import SynthTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma lr_synth_group : forall fuel,
  (forall c e x exp, LR (synth fuel c e x exp)) /\
  (forall c e a exp, LR (synth_arg fuel c e a exp)) /\
  (forall c e b exp, LR (synth_block fuel c e b exp)) /\
  (forall c e ss, LR (check_stmts fuel c e ss)) /\
  (forall c e s, LR (check_stmt fuel c e s)) /\
  (forall c e y, LR (check_bool fuel c e y)) /\
  (forall c e x exp, LR (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, LR (synth_match fuel c e s arms exp)) /\
  (forall g, LR (const_val fuel g)) /\
  (forall c e p a, LR (synth_path fuel c e p a)) /\
  (forall c e nm sg args, LR (check_args fuel c e nm sg args)) /\
  (forall c e p a args, LR (synth_call fuel c e p a args)) /\
  (forall c e p a fs, LR (synth_record fuel c e p a fs)) /\
  (forall c e o n, LR (synth_field fuel c e o n)) /\
  (forall c e o p args, LR (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, LR (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, LR (apply_family fuel c e fg th want)) /\
  (forall c e info, LR (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, LR (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, LR (lang_call fuel c e o n args why)) /\
  (forall c e ds why, LR (int_literal fuel c e ds why)) /\
  (forall c e v why, LR (float_of fuel c e v why)) /\
  (forall c e text, LR (synth_literal fuel c e text)) /\
  (forall c e sp y, LR (check_bind fuel c e sp y)) /\
  (forall c e y, LR (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, LR (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, LR (wrapper fuel pv)) /\
  (forall tg, LR (forwarder fuel tg)) /\
  (forall g, LR_except(gs_id g)(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, LR (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, LR (apply_functor fuel c e sp)) /\
  (forall c e g, LR (apply_body fuel c e g)) /\
  (forall c e fg, LR (apply_abstract fuel c e fg)) /\
  (forall c e fg y, LR (bind_functor fuel c e fg y)) /\
  (forall fg bg, LR (link_body fuel fg bg)).
Proof.
 refine (@synth_preservation_group (@LR) (fun fuel g=>LR_except(gs_id g)(lower_body fuel g)) _
  (@lr_ret) (@lr_bind) (@lr_mmap) (@lr_mfold) (@lr_miter) (@lr_with_vars)
  (fun A m F W H O=>@lr_keeps A m O F)
  (@lr_wrapper) (@lr_forwarder_step) (@lre_lower_body_step) (@lr_lambda_provider_step)).
 intro g;cbn[lower_body];lrt.
Qed.

Lemma lr_synth : forall fuel c e x exp, LR (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (lr_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve lr_synth : lrdb.

Lemma lr_synth_arg : forall fuel c e a exp, LR (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (lr_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve lr_synth_arg : lrdb.

Lemma lr_synth_block : forall fuel c e b exp, LR (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (lr_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve lr_synth_block : lrdb.

Lemma lr_check_stmts : forall fuel c e ss, LR (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (lr_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve lr_check_stmts : lrdb.

Lemma lr_check_stmt : forall fuel c e s, LR (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve lr_check_stmt : lrdb.

Lemma lr_check_bool : forall fuel c e y, LR (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve lr_check_bool : lrdb.

Lemma lr_synth_if : forall fuel c e x exp, LR (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve lr_synth_if : lrdb.

Lemma lr_synth_match : forall fuel c e s arms exp, LR (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve lr_synth_match : lrdb.

Lemma lr_const_val : forall fuel g, LR (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve lr_const_val : lrdb.

Lemma lr_synth_path : forall fuel c e p a, LR (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve lr_synth_path : lrdb.

Lemma lr_check_args : forall fuel c e nm sg args, LR (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve lr_check_args : lrdb.

Lemma lr_synth_call : forall fuel c e p a args, LR (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve lr_synth_call : lrdb.

Lemma lr_synth_record : forall fuel c e p a fs, LR (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve lr_synth_record : lrdb.

Lemma lr_synth_field : forall fuel c e o n, LR (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve lr_synth_field : lrdb.

Lemma lr_synth_method : forall fuel c e o p args, LR (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve lr_synth_method : lrdb.

Lemma lr_provided_call : forall fuel c e ox oty m args, LR (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve lr_provided_call : lrdb.

Lemma lr_apply_family : forall fuel c e fg th want, LR (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve lr_apply_family : lrdb.

Lemma lr_demand_needs : forall fuel c e info, LR (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve lr_demand_needs : lrdb.

Lemma lr_synth_operator : forall fuel c e lm n op operand args, LR (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve lr_synth_operator : lrdb.

Lemma lr_lang_call : forall fuel c e o n args why, LR (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve lr_lang_call : lrdb.

Lemma lr_int_literal : forall fuel c e ds why, LR (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve lr_int_literal : lrdb.

Lemma lr_float_of : forall fuel c e v why, LR (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve lr_float_of : lrdb.

Lemma lr_synth_literal : forall fuel c e text, LR (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve lr_synth_literal : lrdb.

Lemma lr_check_bind : forall fuel c e sp y, LR (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve lr_check_bind : lrdb.

Lemma lr_expect_fn_provider : forall fuel c e y, LR (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve lr_expect_fn_provider : lrdb.

Lemma lr_match_fn_sig : forall fuel c e pv wanted kind, LR (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve lr_match_fn_sig : lrdb.

Lemma lr_wrapper : forall fuel pv, LR (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve lr_wrapper : lrdb.

Lemma lr_forwarder : forall fuel tg, LR (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve lr_forwarder : lrdb.

Lemma lr_lower_body : forall fuel g, LR_except(gs_id g)(lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve lr_lower_body : lrdb.

Lemma lr_lambda_provider : forall fuel c e ps rt body bound sg this what, LR (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve lr_lambda_provider : lrdb.

Lemma lr_apply_functor : forall fuel c e sp, LR (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve lr_apply_functor : lrdb.

Lemma lr_apply_body : forall fuel c e g, LR (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve lr_apply_body : lrdb.

Lemma lr_apply_abstract : forall fuel c e fg, LR (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve lr_apply_abstract : lrdb.

Lemma lr_bind_functor : forall fuel c e fg y, LR (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve lr_bind_functor : lrdb.

Lemma lr_link_body : forall fuel fg bg, LR (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (lr_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve lr_link_body : lrdb.
