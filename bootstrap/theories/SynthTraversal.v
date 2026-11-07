(* Compatibility algebra for synthesis preservation.
   SemanticTraversal checks the common35-function induction once. This
   interface supplies its constant/path steps from the simpler quiet-base
   rule, so existing invariant instances keep the same statements and
   arguments without repeating the compiler traversal. Lower remains an
   independent owner-indexed property. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabFrameCore ElabKeeps ReadonlyHelpers ReifyHeader.
From NM Require Import SemanticTraversal InfoInputs.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Section Traversal.
Variable P : forall A:Type,M A->Prop.
Arguments P {A}.
Variable Lower : nat->gsym->Prop.
Hypothesis Lower_zero : forall g,Lower 0 g.
Hypothesis P_ret : forall A(a:A),P(ret a).
Hypothesis P_bind : forall A B(m:M A)(k:A->M B),P m->(forall a,P(k a))->P(mbind m k).
Hypothesis P_map : forall A B(f:A->M B)xs,(forall x,P(f x))->P(mmap f xs).
Hypothesis P_fold : forall A B(f:A->B->M A)xs acc,(forall a x,P(f a x))->P(mfold f acc xs).
Hypothesis P_iter : forall A(f:A->M unit)xs,(forall x,P(f x))->P(miter f xs).
Hypothesis P_vars : forall A init(m:M A),P m->P(with_vars init m).
Hypothesis P_base : forall A(m:M A),keeps st_fns m->keeps st_fwd m->keeps function_headers m->ok m->P m.
Hypothesis P_wrapper : forall fuel pv,P(wrapper fuel pv).
Hypothesis P_forwarder_step : forall fuel,(forall g,Lower fuel g)->forall tg,P(forwarder(S fuel)tg).
Hypothesis P_lower_body_step : forall fuel,(forall c e b exp,P(synth_block fuel c e b exp))->forall g,Lower(S fuel)g.
Hypothesis P_lambda_provider_step : forall fuel,(forall c e b exp,P(synth_block fuel c e b exp))->forall c e ps rt body bound sg this what,P(lambda_provider(S fuel)c e ps rt body bound sg this what).
Local Ltac readonly_fns := eapply keeps_projection with (p:=readonly_state)(q:=fun x=>fst(fst(fst(fst x))));solve[auto with keepsdb].
Local Ltac readonly_fwd := eapply keeps_projection with (p:=readonly_state)(q:=fun x=>snd(fst(fst(fst x))));solve[auto with keepsdb].
Local Ltac readonly_headers := eapply keeps_projection with (p:=readonly_state)(q:=snd);solve[auto with keepsdb].
Local Ltac generic_kept := apply P_base;[solve[readonly_fns]|solve[readonly_fwd]|solve[readonly_headers]|solve[okt]].
Local Ltac generic_leaf := apply P_base;[solve[keepst]|solve[keepst]|solve[keepst]|solve[okt]].
Local Ltac generic_tac := repeat(fintros;first[solve[auto]|lazymatch goal with
 | |-P(mbind _ _)=>apply P_bind
 | |-P(mmap _ _)=>apply P_map
 | |-P(mfold _ _ _)=>apply P_fold
 | |-P(miter _ _)=>apply P_iter
 | |-P(with_vars _ _)=>apply P_vars
 | |-P(in_reify_code _ _)=>unfold in_reify_code
 | |-P ?m=>first[solve[generic_kept]|lazymatch m with
  |upd _=>generic_leaf
  |(fun s=>_)=>generic_leaf
  |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|let h:=head_of m in progress unfold h]
  end]
 end]).

Local Lemma generic_const_zero:forall g,P(const_val 0 g).
Proof. intro;cbn[const_val];generic_tac. Qed.
Local Lemma generic_path_zero:forall c e p a,P(synth_path 0 c e p a).
Proof. intros;cbn[synth_path];generic_tac. Qed.
Local Lemma generic_const_step:forall fuel,(forall c e x exp,P(synth fuel c e x exp))->forall g,P(const_val(S fuel)g).
Proof. intros;cbn[const_val];generic_tac. Qed.
Local Lemma generic_path_step:forall fuel,(forall g,P(const_val fuel g))->forall c e p a,P(synth_path(S fuel)c e p a).
Proof. intros;cbn[synth_path];generic_tac. Qed.
Local Lemma generic_info:forall g,P(fn_info g).
Proof. intro;generic_kept. Qed.
Local Lemma generic_semantic_base:forall A(m:M A),keeps readonly_state m->InfoInputs.NI m->ok m->P m.
Proof. intros A m K N O;apply P_base.
 - eapply keeps_projection with(p:=readonly_state)(q:=fun x=>fst(fst(fst(fst x))));exact K.
 - eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst(fst x))));exact K.
 - eapply keeps_projection with(p:=readonly_state)(q:=snd);exact K.
 - exact O.
Qed.
Lemma synth_preservation_group : forall fuel,
  (forall c e x exp, P (synth fuel c e x exp)) /\
  (forall c e a exp, P (synth_arg fuel c e a exp)) /\
  (forall c e b exp, P (synth_block fuel c e b exp)) /\
  (forall c e ss, P (check_stmts fuel c e ss)) /\
  (forall c e s, P (check_stmt fuel c e s)) /\
  (forall c e y, P (check_bool fuel c e y)) /\
  (forall c e x exp, P (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, P (synth_match fuel c e s arms exp)) /\
  (forall g, P (const_val fuel g)) /\
  (forall c e p a, P (synth_path fuel c e p a)) /\
  (forall c e nm sg args, P (check_args fuel c e nm sg args)) /\
  (forall c e p a args, P (synth_call fuel c e p a args)) /\
  (forall c e p a fs, P (synth_record fuel c e p a fs)) /\
  (forall c e o n, P (synth_field fuel c e o n)) /\
  (forall c e o p args, P (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, P (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, P (apply_family fuel c e fg th want)) /\
  (forall c e info, P (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, P (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, P (lang_call fuel c e o n args why)) /\
  (forall c e ds why, P (int_literal fuel c e ds why)) /\
  (forall c e v why, P (float_of fuel c e v why)) /\
  (forall c e text, P (synth_literal fuel c e text)) /\
  (forall c e sp y, P (check_bind fuel c e sp y)) /\
  (forall c e y, P (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, P (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, P (wrapper fuel pv)) /\
  (forall tg, P (forwarder fuel tg)) /\
  (forall g, Lower fuel g) /\
  (forall c e ps rt body bound sg this what, P (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, P (apply_functor fuel c e sp)) /\
  (forall c e g, P (apply_body fuel c e g)) /\
  (forall c e fg, P (apply_abstract fuel c e fg)) /\
  (forall c e fg y, P (bind_functor fuel c e fg y)) /\
  (forall fg bg, P (link_body fuel fg bg)).
Proof.
 exact (@SemanticTraversal.synth_preservation_group (@P) Lower Lower_zero
 (fun fuel g=>P(const_val fuel g)) (fun fuel c e p a=>P(synth_path fuel c e p a))
 generic_const_zero generic_path_zero (fun fuel c e p a H=>H) generic_const_step generic_path_step generic_info
 P_ret P_bind P_map P_fold P_iter P_vars generic_semantic_base P_wrapper P_forwarder_step P_lower_body_step P_lambda_provider_step).
Qed.
End Traversal.
