(* Every newly emitted function retains an actual completed invocation
   and a usable immutable frame, through the real synthesis visitor. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabFrame ElabInv FnReadonly SymReadonly
 FnBound FnBoundEmit FnBoundSynth FnPresentStable ReifyHeader ReifyHeaderReadonly
 ReifyHeaderStable ReifyHeaderSynth ReifyHeaderPresent
 FunctionFrameHistory FunctionFrameOps FunctionFrameLower FunctionFrameWrapper FunctionFrameForwarder FunctionFrameLambda.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ff_lower_body_zero:forall g,FF_bound g(lower_body 0 g).
Proof.
 intro;split;[apply hp_lower_body|];split;[apply bf_lower_body_at|].
 intros s value final I Bound Below Physical Run;discriminate.
Qed.

Lemma ff_synth_group : forall fuel,
  (forall c e x exp, FF (synth fuel c e x exp)) /\
  (forall c e a exp, FF (synth_arg fuel c e a exp)) /\
  (forall c e b exp, FF (synth_block fuel c e b exp)) /\
  (forall c e ss, FF (check_stmts fuel c e ss)) /\
  (forall c e s, FF (check_stmt fuel c e s)) /\
  (forall c e y, FF (check_bool fuel c e y)) /\
  (forall c e x exp, FF (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, FF (synth_match fuel c e s arms exp)) /\
  (forall g, FF (const_val fuel g)) /\
  (forall c e p a, FF (synth_path fuel c e p a)) /\
  (forall c e nm sg args, FF (check_args fuel c e nm sg args)) /\
  (forall c e p a args, FF (synth_call fuel c e p a args)) /\
  (forall c e p a fs, FF (synth_record fuel c e p a fs)) /\
  (forall c e o n, FF (synth_field fuel c e o n)) /\
  (forall c e o p args, FF (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, FF (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, FF (apply_family fuel c e fg th want)) /\
  (forall c e info, FF (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, FF (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, FF (lang_call fuel c e o n args why)) /\
  (forall c e ds why, FF (int_literal fuel c e ds why)) /\
  (forall c e v why, FF (float_of fuel c e v why)) /\
  (forall c e text, FF (synth_literal fuel c e text)) /\
  (forall c e sp y, FF (check_bind fuel c e sp y)) /\
  (forall c e y, FF (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, FF (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, FF (wrapper fuel pv)) /\
  (forall tg, FF (forwarder fuel tg)) /\
  (forall g, FF_bound g (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, FF (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, FF (apply_functor fuel c e sp)) /\
  (forall c e g, FF (apply_body fuel c e g)) /\
  (forall c e fg, FF (apply_abstract fuel c e fg)) /\
  (forall c e fg y, FF (bind_functor fuel c e fg y)) /\
  (forall fg bg, FF (link_body fuel fg bg)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; first[apply ff_lower_body_zero|fft]. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  all: solve [fintros; first [lazymatch goal with
 | |-FF(wrapper _ _)=>apply ff_wrapper
 | |-FF(forwarder _ _)=>apply ff_forwarder_step;exact I28
 | |-FF_bound _(lower_body _ _)=>apply ff_lower_body_step;exact I2
 | |-FF(lambda_provider _ _ _ _ _ _ _ _ _ _)=>apply ff_lambda_provider_step;exact I2
 end|cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; fft]].
Qed.

Lemma ff_synth : forall fuel c e x exp, FF (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (ff_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve ff_synth : ffdb.

Lemma ff_synth_arg : forall fuel c e a exp, FF (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (ff_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve ff_synth_arg : ffdb.

Lemma ff_synth_block : forall fuel c e b exp, FF (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (ff_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve ff_synth_block : ffdb.

Lemma ff_check_stmts : forall fuel c e ss, FF (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (ff_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve ff_check_stmts : ffdb.

Lemma ff_check_stmt : forall fuel c e s, FF (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve ff_check_stmt : ffdb.

Lemma ff_check_bool : forall fuel c e y, FF (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve ff_check_bool : ffdb.

Lemma ff_synth_if : forall fuel c e x exp, FF (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve ff_synth_if : ffdb.

Lemma ff_synth_match : forall fuel c e s arms exp, FF (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve ff_synth_match : ffdb.

Lemma ff_const_val : forall fuel g, FF (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve ff_const_val : ffdb.

Lemma ff_synth_path : forall fuel c e p a, FF (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve ff_synth_path : ffdb.

Lemma ff_check_args : forall fuel c e nm sg args, FF (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve ff_check_args : ffdb.

Lemma ff_synth_call : forall fuel c e p a args, FF (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve ff_synth_call : ffdb.

Lemma ff_synth_record : forall fuel c e p a fs, FF (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve ff_synth_record : ffdb.

Lemma ff_synth_field : forall fuel c e o n, FF (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve ff_synth_field : ffdb.

Lemma ff_synth_method : forall fuel c e o p args, FF (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve ff_synth_method : ffdb.

Lemma ff_provided_call : forall fuel c e ox oty m args, FF (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve ff_provided_call : ffdb.

Lemma ff_apply_family : forall fuel c e fg th want, FF (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve ff_apply_family : ffdb.

Lemma ff_demand_needs : forall fuel c e info, FF (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve ff_demand_needs : ffdb.

Lemma ff_synth_operator : forall fuel c e lm n op operand args, FF (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve ff_synth_operator : ffdb.

Lemma ff_lang_call : forall fuel c e o n args why, FF (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve ff_lang_call : ffdb.

Lemma ff_int_literal : forall fuel c e ds why, FF (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve ff_int_literal : ffdb.

Lemma ff_float_of : forall fuel c e v why, FF (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve ff_float_of : ffdb.

Lemma ff_synth_literal : forall fuel c e text, FF (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve ff_synth_literal : ffdb.

Lemma ff_check_bind : forall fuel c e sp y, FF (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve ff_check_bind : ffdb.

Lemma ff_expect_fn_provider : forall fuel c e y, FF (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve ff_expect_fn_provider : ffdb.

Lemma ff_match_fn_sig : forall fuel c e pv wanted kind, FF (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve ff_match_fn_sig : ffdb.

Lemma ff_wrapper : forall fuel pv, FF (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve ff_wrapper : ffdb.

Lemma ff_forwarder : forall fuel tg, FF (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve ff_forwarder : ffdb.

Lemma ff_lower_body_at : forall fuel g, FF_bound g (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve ff_lower_body_at : ffdb.

Lemma ff_lambda_provider : forall fuel c e ps rt body bound sg this what, FF (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve ff_lambda_provider : ffdb.

Lemma ff_apply_functor : forall fuel c e sp, FF (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve ff_apply_functor : ffdb.

Lemma ff_apply_body : forall fuel c e g, FF (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve ff_apply_body : ffdb.

Lemma ff_apply_abstract : forall fuel c e fg, FF (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve ff_apply_abstract : ffdb.

Lemma ff_bind_functor : forall fuel c e fg y, FF (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve ff_bind_functor : ffdb.

Lemma ff_link_body : forall fuel fg bg, FF (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ff_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve ff_link_body : ffdb.
