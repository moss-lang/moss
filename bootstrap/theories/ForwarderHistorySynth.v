From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabFrame ElabInv FnReadonly SymReadonly ForwarderHistory ForwarderHistoryOps ForwarderHistoryStep ForwarderExpectHistory ForwarderHistoryEmit.
From NM Require Import AllocationTraversal.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Local Lemma shared_gh_allocate : forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource->kind<>KTag /\kind<>KUnit->orig<>OLambda->
 (forall target,orig<>OForwarder target)->(forall id,GH(k id))->
 GH(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Source Kind Lambda Forward K.
 apply gh_new_sym_bind;[exact Source|exact Kind|exact Forward|].
 intro id;apply gh_at_of_gh;apply K.
Qed.

Lemma gh_synth_group : forall fuel,
  (forall c e x exp, GH (synth fuel c e x exp)) /\
  (forall c e a exp, GH (synth_arg fuel c e a exp)) /\
  (forall c e b exp, GH (synth_block fuel c e b exp)) /\
  (forall c e ss, GH (check_stmts fuel c e ss)) /\
  (forall c e s, GH (check_stmt fuel c e s)) /\
  (forall c e y, GH (check_bool fuel c e y)) /\
  (forall c e x exp, GH (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, GH (synth_match fuel c e s arms exp)) /\
  (forall g, GH (const_val fuel g)) /\
  (forall c e p a, GH (synth_path fuel c e p a)) /\
  (forall c e nm sg args, GH (check_args fuel c e nm sg args)) /\
  (forall c e p a args, GH (synth_call fuel c e p a args)) /\
  (forall c e p a fs, GH (synth_record fuel c e p a fs)) /\
  (forall c e o n, GH (synth_field fuel c e o n)) /\
  (forall c e o p args, GH (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, GH (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, GH (apply_family fuel c e fg th want)) /\
  (forall c e info, GH (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, GH (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, GH (lang_call fuel c e o n args why)) /\
  (forall c e ds why, GH (int_literal fuel c e ds why)) /\
  (forall c e v why, GH (float_of fuel c e v why)) /\
  (forall c e text, GH (synth_literal fuel c e text)) /\
  (forall c e sp y, GH (check_bind fuel c e sp y)) /\
  (forall c e y, GH (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, GH (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, GH (wrapper fuel pv)) /\
  (forall tg, GHF fuel tg) /\
  (forall g, GH_at (gs_id g) (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, GH (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, GH (apply_functor fuel c e sp)) /\
  (forall c e g, GH (apply_body fuel c e g)) /\
  (forall c e fg, GH (apply_abstract fuel c e fg)) /\
  (forall c e fg y, GH (bind_functor fuel c e fg y)) /\
  (forall fg bg, GH (link_body fuel fg bg)).
Proof.
 refine (@AllocationTraversal.synth_preservation_group (@GH)
  (fun fuel g=>GH_at(gs_id g)(lower_body fuel g)) _ (@GHF) (@gh_forwarder_zero)
  (@gh_ret) (@gh_bind) (@gh_mmap) (@gh_mfold) (@gh_miter) (@gh_with_vars)
  (fun A m F H S O=>@gh_keeps A m O F S) (@shared_gh_allocate) (@gh_expect_fn_provider_step)
  (@gh_wrapper) (@gh_forwarder_step) (@gh_lower_body_step) (@gh_lambda_provider_step)).
 intro g;cbn[lower_body];ght.
Qed.

Lemma gh_synth : forall fuel c e x exp, GH (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (gh_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve gh_synth : ghdb.

Lemma gh_synth_arg : forall fuel c e a exp, GH (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (gh_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve gh_synth_arg : ghdb.

Lemma gh_synth_block : forall fuel c e b exp, GH (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (gh_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve gh_synth_block : ghdb.

Lemma gh_check_stmts : forall fuel c e ss, GH (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (gh_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve gh_check_stmts : ghdb.

Lemma gh_check_stmt : forall fuel c e s, GH (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve gh_check_stmt : ghdb.

Lemma gh_check_bool : forall fuel c e y, GH (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve gh_check_bool : ghdb.

Lemma gh_synth_if : forall fuel c e x exp, GH (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve gh_synth_if : ghdb.

Lemma gh_synth_match : forall fuel c e s arms exp, GH (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve gh_synth_match : ghdb.

Lemma gh_const_val : forall fuel g, GH (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve gh_const_val : ghdb.

Lemma gh_synth_path : forall fuel c e p a, GH (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve gh_synth_path : ghdb.

Lemma gh_check_args : forall fuel c e nm sg args, GH (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve gh_check_args : ghdb.

Lemma gh_synth_call : forall fuel c e p a args, GH (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve gh_synth_call : ghdb.

Lemma gh_synth_record : forall fuel c e p a fs, GH (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve gh_synth_record : ghdb.

Lemma gh_synth_field : forall fuel c e o n, GH (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve gh_synth_field : ghdb.

Lemma gh_synth_method : forall fuel c e o p args, GH (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve gh_synth_method : ghdb.

Lemma gh_provided_call : forall fuel c e ox oty m args, GH (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve gh_provided_call : ghdb.

Lemma gh_apply_family : forall fuel c e fg th want, GH (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve gh_apply_family : ghdb.

Lemma gh_demand_needs : forall fuel c e info, GH (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve gh_demand_needs : ghdb.

Lemma gh_synth_operator : forall fuel c e lm n op operand args, GH (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve gh_synth_operator : ghdb.

Lemma gh_lang_call : forall fuel c e o n args why, GH (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve gh_lang_call : ghdb.

Lemma gh_int_literal : forall fuel c e ds why, GH (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve gh_int_literal : ghdb.

Lemma gh_float_of : forall fuel c e v why, GH (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve gh_float_of : ghdb.

Lemma gh_synth_literal : forall fuel c e text, GH (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve gh_synth_literal : ghdb.

Lemma gh_check_bind : forall fuel c e sp y, GH (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve gh_check_bind : ghdb.

Lemma gh_expect_fn_provider : forall fuel c e y, GH (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve gh_expect_fn_provider : ghdb.

Lemma gh_match_fn_sig : forall fuel c e pv wanted kind, GH (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve gh_match_fn_sig : ghdb.

Lemma gh_wrapper : forall fuel pv, GH (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve gh_wrapper : ghdb.

Lemma gh_forwarder : forall fuel tg, GHF fuel tg.
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve gh_forwarder : ghdb.

Lemma gh_lower_body_at : forall fuel g, GH_at (gs_id g) (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve gh_lower_body_at : ghdb.

Lemma gh_lambda_provider : forall fuel c e ps rt body bound sg this what, GH (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve gh_lambda_provider : ghdb.

Lemma gh_apply_functor : forall fuel c e sp, GH (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve gh_apply_functor : ghdb.

Lemma gh_apply_body : forall fuel c e g, GH (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve gh_apply_body : ghdb.

Lemma gh_apply_abstract : forall fuel c e fg, GH (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve gh_apply_abstract : ghdb.

Lemma gh_bind_functor : forall fuel c e fg y, GH (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve gh_bind_functor : ghdb.

Lemma gh_link_body : forall fuel fg bg, GH (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (gh_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve gh_link_body : ghdb.
