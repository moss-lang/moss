From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabFrame ElabInv FnReadonly SymReadonly LambdaCompletionOps LambdaCompletionEmit.
From NM Require Import AllocationTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Local Lemma shared_ch_allocate : forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource->kind<>KTag /\kind<>KUnit->orig<>OLambda->
 (forall target,orig<>OForwarder target)->(forall id,CH(k id))->
 CH(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Source Kind Lambda Forward K.
 apply ch_new_sym_bind;[exact Source|exact Kind|exact Lambda|].
 intro id;apply ch_at_of_ch;apply K.
Qed.

Local Lemma shared_ch_expect : forall fuel,
 (forall tg,CH(forwarder fuel tg))->forall c e y,CH(expect_fn_provider(S fuel)c e y).
Proof. intros fuel Forward c e y;cbn[expect_fn_provider];cht. Qed.

Lemma ch_synth_group : forall fuel,
  (forall c e x exp, CH (synth fuel c e x exp)) /\
  (forall c e a exp, CH (synth_arg fuel c e a exp)) /\
  (forall c e b exp, CH (synth_block fuel c e b exp)) /\
  (forall c e ss, CH (check_stmts fuel c e ss)) /\
  (forall c e s, CH (check_stmt fuel c e s)) /\
  (forall c e y, CH (check_bool fuel c e y)) /\
  (forall c e x exp, CH (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, CH (synth_match fuel c e s arms exp)) /\
  (forall g, CH (const_val fuel g)) /\
  (forall c e p a, CH (synth_path fuel c e p a)) /\
  (forall c e nm sg args, CH (check_args fuel c e nm sg args)) /\
  (forall c e p a args, CH (synth_call fuel c e p a args)) /\
  (forall c e p a fs, CH (synth_record fuel c e p a fs)) /\
  (forall c e o n, CH (synth_field fuel c e o n)) /\
  (forall c e o p args, CH (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, CH (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, CH (apply_family fuel c e fg th want)) /\
  (forall c e info, CH (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, CH (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, CH (lang_call fuel c e o n args why)) /\
  (forall c e ds why, CH (int_literal fuel c e ds why)) /\
  (forall c e v why, CH (float_of fuel c e v why)) /\
  (forall c e text, CH (synth_literal fuel c e text)) /\
  (forall c e sp y, CH (check_bind fuel c e sp y)) /\
  (forall c e y, CH (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, CH (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, CH (wrapper fuel pv)) /\
  (forall tg, CH (forwarder fuel tg)) /\
  (forall g, CH_at(gs_id g)(lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, CH (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, CH (apply_functor fuel c e sp)) /\
  (forall c e g, CH (apply_body fuel c e g)) /\
  (forall c e fg, CH (apply_abstract fuel c e fg)) /\
  (forall c e fg y, CH (bind_functor fuel c e fg y)) /\
  (forall fg bg, CH (link_body fuel fg bg)).
Proof.
 refine (@AllocationTraversal.synth_preservation_group (@CH) (fun fuel g=>CH_at(gs_id g)(lower_body fuel g)) _ (fun fuel tg=>CH(forwarder fuel tg)) _
  (@ch_ret) (@ch_bind) (@ch_mmap) (@ch_mfold) (@ch_miter) (@ch_with_vars)
  (fun A m F H S O=>@ch_keeps A m O F S) (@shared_ch_allocate) (@shared_ch_expect)
  (@ch_wrapper) (@ch_forwarder_step) (@cha_lower_body_step) (@ch_lambda_provider_step)).
 - intro g;cbn[lower_body];cht.
 - intro tg;cbn[forwarder];cht.
Qed.

Lemma ch_synth : forall fuel c e x exp, CH (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (ch_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve ch_synth : chdb.

Lemma ch_synth_arg : forall fuel c e a exp, CH (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (ch_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve ch_synth_arg : chdb.

Lemma ch_synth_block : forall fuel c e b exp, CH (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (ch_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve ch_synth_block : chdb.

Lemma ch_check_stmts : forall fuel c e ss, CH (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (ch_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve ch_check_stmts : chdb.

Lemma ch_check_stmt : forall fuel c e s, CH (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve ch_check_stmt : chdb.

Lemma ch_check_bool : forall fuel c e y, CH (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve ch_check_bool : chdb.

Lemma ch_synth_if : forall fuel c e x exp, CH (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve ch_synth_if : chdb.

Lemma ch_synth_match : forall fuel c e s arms exp, CH (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve ch_synth_match : chdb.

Lemma ch_const_val : forall fuel g, CH (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve ch_const_val : chdb.

Lemma ch_synth_path : forall fuel c e p a, CH (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve ch_synth_path : chdb.

Lemma ch_check_args : forall fuel c e nm sg args, CH (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve ch_check_args : chdb.

Lemma ch_synth_call : forall fuel c e p a args, CH (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve ch_synth_call : chdb.

Lemma ch_synth_record : forall fuel c e p a fs, CH (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve ch_synth_record : chdb.

Lemma ch_synth_field : forall fuel c e o n, CH (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve ch_synth_field : chdb.

Lemma ch_synth_method : forall fuel c e o p args, CH (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve ch_synth_method : chdb.

Lemma ch_provided_call : forall fuel c e ox oty m args, CH (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve ch_provided_call : chdb.

Lemma ch_apply_family : forall fuel c e fg th want, CH (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve ch_apply_family : chdb.

Lemma ch_demand_needs : forall fuel c e info, CH (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve ch_demand_needs : chdb.

Lemma ch_synth_operator : forall fuel c e lm n op operand args, CH (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve ch_synth_operator : chdb.

Lemma ch_lang_call : forall fuel c e o n args why, CH (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve ch_lang_call : chdb.

Lemma ch_int_literal : forall fuel c e ds why, CH (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve ch_int_literal : chdb.

Lemma ch_float_of : forall fuel c e v why, CH (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve ch_float_of : chdb.

Lemma ch_synth_literal : forall fuel c e text, CH (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve ch_synth_literal : chdb.

Lemma ch_check_bind : forall fuel c e sp y, CH (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve ch_check_bind : chdb.

Lemma ch_expect_fn_provider : forall fuel c e y, CH (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve ch_expect_fn_provider : chdb.

Lemma ch_match_fn_sig : forall fuel c e pv wanted kind, CH (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve ch_match_fn_sig : chdb.

Lemma ch_wrapper : forall fuel pv, CH (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve ch_wrapper : chdb.

Lemma ch_forwarder : forall fuel tg, CH (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve ch_forwarder : chdb.

Lemma ch_lower_body : forall fuel g, CH_at(gs_id g)(lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve ch_lower_body : chdb.

Lemma ch_lambda_provider : forall fuel c e ps rt body bound sg this what, CH (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve ch_lambda_provider : chdb.

Lemma ch_apply_functor : forall fuel c e sp, CH (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve ch_apply_functor : chdb.

Lemma ch_apply_body : forall fuel c e g, CH (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve ch_apply_body : chdb.

Lemma ch_apply_abstract : forall fuel c e fg, CH (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve ch_apply_abstract : chdb.

Lemma ch_bind_functor : forall fuel c e fg y, CH (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve ch_bind_functor : chdb.

Lemma ch_link_body : forall fuel fg bg, CH (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ch_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve ch_link_body : chdb.
