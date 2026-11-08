(* Every newly emitted function retains an actual completed invocation
   and a usable immutable frame, through the real synthesis visitor. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabFrame ElabInv FnReadonly SymReadonly
 FnBound FnBoundEmit FnBoundSynth FnPresentStable ReifyHeader ReifyHeaderReadonly
 ReifyHeaderStable ReifyHeaderSynth ReifyHeaderPresent
 FunctionFrameHistory FunctionFrameOps FunctionFrameLower FunctionFrameWrapper FunctionFrameForwarder FunctionFrameLambda.
From NM Require Import SynthTraversal ReifyHeaderEmit.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ff_lower_body_zero:forall g,FF_bound g(lower_body 0 g).
Proof.
 intro;split;[apply hp_lower_body|];split;[apply bf_lower_body_at|].
 intros s value final I Bound Below Physical Run;discriminate.
Qed.

(* Restoring local variables needs header stability as well as a frame
   receipt. Keep these two properties together in the shared algebra,
   then project the frame receipt for the existing public theorem. *)
Local Definition Framed {A}(m:M A):Prop:=HS m /\ FF m.
Local Definition FramedLower fuel g:Prop:=HSE(gs_id g)(lower_body fuel g) /\ FF_bound g(lower_body fuel g).
Local Lemma framed_ret:forall A(a:A),Framed(ret a).
Proof. intros;split;[apply hs_ret|apply ff_ret]. Qed.
Local Lemma framed_bind:forall A B(m:M A)(k:A->M B),Framed m->(forall a,Framed(k a))->Framed(mbind m k).
Proof. intros A B m k [Hm Fm] K;split;[apply hs_bind;[exact Hm|intro a;exact(proj1(K a))]|apply ff_bind;[exact Fm|intro a;exact(proj2(K a))]]. Qed.
Local Lemma framed_map:forall A B(f:A->M B)xs,(forall x,Framed(f x))->Framed(mmap f xs).
Proof. intros A B f xs K;split;[apply hs_mmap;intro x;exact(proj1(K x))|apply ff_mmap;intro x;exact(proj2(K x))]. Qed.
Local Lemma framed_fold:forall A B(f:A->B->M A)xs acc,(forall a x,Framed(f a x))->Framed(mfold f acc xs).
Proof. intros A B f xs acc K;split;[apply hs_mfold;intros a x;exact(proj1(K a x))|apply ff_mfold;intros a x;exact(proj2(K a x))]. Qed.
Local Lemma framed_iter:forall A(f:A->M unit)xs,(forall x,Framed(f x))->Framed(miter f xs).
Proof. intros A f xs K;split;[apply hs_miter;intro x;exact(proj1(K x))|apply ff_miter;intro x;exact(proj2(K x))]. Qed.
Local Lemma framed_vars:forall A init(m:M A),Framed m->Framed(with_vars init m).
Proof. intros A init m[H F];split;[apply hs_with_vars;exact H|apply ff_with_vars;assumption]. Qed.
Local Lemma framed_base:forall A(m:M A),keeps st_fns m->keeps st_fwd m->keeps function_headers m->ok m->Framed m.
Proof. intros;split;[apply hs_keeps_first;assumption|apply ff_keeps;assumption]. Qed.
Local Lemma framed_zero:forall g,FramedLower 0 g.
Proof. intro g;split;[cbn[lower_body];hst|apply ff_lower_body_zero]. Qed.
Local Lemma framed_wrapper:forall fuel pv,Framed(wrapper fuel pv).
Proof. intros;split;[apply hs_wrapper|apply ff_wrapper]. Qed.
Local Lemma framed_forwarder:forall fuel,(forall g,FramedLower fuel g)->forall tg,Framed(forwarder(S fuel)tg).
Proof. intros fuel K tg;split;[apply hs_forwarder_step;intro g;exact(proj1(K g))|apply ff_forwarder_step;intro g;exact(proj2(K g))]. Qed.
Local Lemma framed_lower:forall fuel,(forall c e b exp,Framed(synth_block fuel c e b exp))->forall g,FramedLower(S fuel)g.
Proof. intros fuel K g;split;[apply hse_lower_body_step;intros c e b exp;exact(proj1(K c e b exp))|apply ff_lower_body_step;intros c e b exp;exact(proj2(K c e b exp))]. Qed.
Local Lemma framed_lambda:forall fuel,(forall c e b exp,Framed(synth_block fuel c e b exp))->forall c e ps rt body bound sg this what,Framed(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel K;intros;split;[apply hs_lambda_provider_step;intros c0 e0 b0 exp;exact(proj1(K c0 e0 b0 exp))|apply ff_lambda_provider_step;intros c0 e0 b0 exp;exact(proj2(K c0 e0 b0 exp))]. Qed.
Local Lemma framed_group : forall fuel,
  (forall c e x exp, Framed (synth fuel c e x exp)) /\
  (forall c e a exp, Framed (synth_arg fuel c e a exp)) /\
  (forall c e b exp, Framed (synth_block fuel c e b exp)) /\
  (forall c e ss, Framed (check_stmts fuel c e ss)) /\
  (forall c e s, Framed (check_stmt fuel c e s)) /\
  (forall c e y, Framed (check_bool fuel c e y)) /\
  (forall c e x exp, Framed (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, Framed (synth_match fuel c e s arms exp)) /\
  (forall g, Framed (const_val fuel g)) /\
  (forall c e p a, Framed (synth_path fuel c e p a)) /\
  (forall c e nm sg args, Framed (check_args fuel c e nm sg args)) /\
  (forall c e p a args, Framed (synth_call fuel c e p a args)) /\
  (forall c e p a fs, Framed (synth_record fuel c e p a fs)) /\
  (forall c e o n, Framed (synth_field fuel c e o n)) /\
  (forall c e o p args, Framed (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, Framed (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, Framed (apply_family fuel c e fg th want)) /\
  (forall c e info, Framed (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, Framed (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, Framed (lang_call fuel c e o n args why)) /\
  (forall c e ds why, Framed (int_literal fuel c e ds why)) /\
  (forall c e v why, Framed (float_of fuel c e v why)) /\
  (forall c e text, Framed (synth_literal fuel c e text)) /\
  (forall c e sp y, Framed (check_bind fuel c e sp y)) /\
  (forall c e y, Framed (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, Framed (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, Framed (wrapper fuel pv)) /\
  (forall tg, Framed (forwarder fuel tg)) /\
  (forall g, FramedLower fuel g) /\
  (forall c e ps rt body bound sg this what, Framed (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, Framed (apply_functor fuel c e sp)) /\
  (forall c e g, Framed (apply_body fuel c e g)) /\
  (forall c e fg, Framed (apply_abstract fuel c e fg)) /\
  (forall c e fg y, Framed (bind_functor fuel c e fg y)) /\
  (forall fg bg, Framed (link_body fuel fg bg)).
Proof. exact (@synth_preservation_group (@Framed) FramedLower framed_zero framed_ret framed_bind framed_map framed_fold framed_iter framed_vars framed_base framed_wrapper framed_forwarder framed_lower framed_lambda). Qed.
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
Proof. intro fuel. destruct (framed_group fuel) as (H0 & H1 & H2 & H3 & H4 & H5 & H6 & H7 & H8 & H9 & H10 & H11 & H12 & H13 & H14 & H15 & H16 & H17 & H18 & H19 & H20 & H21 & H22 & H23 & H24 & H25 & H26 & H27 & H28 & H29 & H30 & H31 & H32 & H33 & H34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  - intros c e x exp; exact (proj2 (H0 c e x exp)).
  - intros c e a exp; exact (proj2 (H1 c e a exp)).
  - intros c e b exp; exact (proj2 (H2 c e b exp)).
  - intros c e ss; exact (proj2 (H3 c e ss)).
  - intros c e s; exact (proj2 (H4 c e s)).
  - intros c e y; exact (proj2 (H5 c e y)).
  - intros c e x exp; exact (proj2 (H6 c e x exp)).
  - intros c e s arms exp; exact (proj2 (H7 c e s arms exp)).
  - intros g; exact (proj2 (H8 g)).
  - intros c e p a; exact (proj2 (H9 c e p a)).
  - intros c e nm sg args; exact (proj2 (H10 c e nm sg args)).
  - intros c e p a args; exact (proj2 (H11 c e p a args)).
  - intros c e p a fs; exact (proj2 (H12 c e p a fs)).
  - intros c e o n; exact (proj2 (H13 c e o n)).
  - intros c e o p args; exact (proj2 (H14 c e o p args)).
  - intros c e ox oty m args; exact (proj2 (H15 c e ox oty m args)).
  - intros c e fg th want; exact (proj2 (H16 c e fg th want)).
  - intros c e info; exact (proj2 (H17 c e info)).
  - intros c e lm n op operand args; exact (proj2 (H18 c e lm n op operand args)).
  - intros c e o n args why; exact (proj2 (H19 c e o n args why)).
  - intros c e ds why; exact (proj2 (H20 c e ds why)).
  - intros c e v why; exact (proj2 (H21 c e v why)).
  - intros c e text; exact (proj2 (H22 c e text)).
  - intros c e sp y; exact (proj2 (H23 c e sp y)).
  - intros c e y; exact (proj2 (H24 c e y)).
  - intros c e pv wanted kind; exact (proj2 (H25 c e pv wanted kind)).
  - intros pv; exact (proj2 (H26 pv)).
  - intros tg; exact (proj2 (H27 tg)).
  - intros g; exact (proj2 (H28 g)).
  - intros c e ps rt body bound sg this what; exact (proj2 (H29 c e ps rt body bound sg this what)).
  - intros c e sp; exact (proj2 (H30 c e sp)).
  - intros c e g; exact (proj2 (H31 c e g)).
  - intros c e fg; exact (proj2 (H32 c e fg)).
  - intros c e fg y; exact (proj2 (H33 c e fg y)).
  - intros fg bg; exact (proj2 (H34 fg bg)).
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
