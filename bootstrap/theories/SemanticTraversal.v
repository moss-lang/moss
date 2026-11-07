(* Shared traversal for information and constant-cache invariants.
   ConstSlot may carry a symbol-provenance premise; only PathSlot consumes
   it, through a separate checked step. fn_info is an atomic semantic
   contract because it genuinely writes the information cache.

   One fuel induction for the common elaborator traversal.
   P describes preservation by ordinary computations. Lower is independent:
   lower_body can satisfy an indexed property that depends on its owner.
   Each client supplies the monad laws, its quiet-step rule, and the four
   operations that may emit function bodies or update forwarder rows.
   Quiet base computations also preserve function headers; this lets one
   traversal serve both ordinary invariants and function-frame receipts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabFrameCore ElabKeeps ReadonlyHelpers ReifyHeader.
From NM Require Import InfoInputs.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Section Traversal.
Variable P : forall A:Type,M A->Prop.
Arguments P {A}.
Variable Lower : nat->gsym->Prop.
Hypothesis Lower_zero : forall g,Lower 0 g.
Variable ConstSlot : nat->gsym->Prop.
Variable PathSlot : nat->cx->env->path->sapp->Prop.
Hypothesis Const_zero : forall g,ConstSlot 0 g.
Hypothesis Path_zero : forall c e p a,PathSlot 0 c e p a.
Hypothesis Path_lift : forall fuel c e p a,PathSlot fuel c e p a->P(synth_path fuel c e p a).
Hypothesis Const_step : forall fuel,(forall c e x exp,P(synth fuel c e x exp))->forall g,ConstSlot(S fuel)g.
Hypothesis Path_step : forall fuel,(forall g,ConstSlot fuel g)->forall c e p a,PathSlot(S fuel)c e p a.
Hypothesis P_fn_info : forall g,P(fn_info g).

Hypothesis P_ret : forall A(a:A),P(ret a).
Hypothesis P_bind : forall A B(m:M A)(k:A->M B),P m->(forall a,P(k a))->P(mbind m k).
Hypothesis P_map : forall A B(f:A->M B)xs,(forall x,P(f x))->P(mmap f xs).
Hypothesis P_fold : forall A B(f:A->B->M A)xs acc,(forall a x,P(f a x))->P(mfold f acc xs).
Hypothesis P_iter : forall A(f:A->M unit)xs,(forall x,P(f x))->P(miter f xs).
Hypothesis P_vars : forall A init(m:M A),P m->P(with_vars init m).
Hypothesis P_base : forall A(m:M A),keeps readonly_state m->InfoInputs.NI m->ok m->P m.
Hypothesis P_wrapper : forall fuel pv,P(wrapper fuel pv).
Hypothesis P_forwarder_step : forall fuel,(forall g,Lower fuel g)->forall tg,P(forwarder(S fuel)tg).
Hypothesis P_lower_body_step : forall fuel,(forall c e b exp,P(synth_block fuel c e b exp))->forall g,Lower(S fuel)g.
Hypothesis P_lambda_provider_step : forall fuel,(forall c e b exp,P(synth_block fuel c e b exp))->forall c e ps rt body bound sg this what,P(lambda_provider(S fuel)c e ps rt body bound sg this what).
Local Ltac generic_kept := apply P_base;[solve[auto with keepsdb]|solve[nit]|solve[okt]].
Local Ltac generic_leaf := apply P_base;[solve[keepst]|solve[nit]|solve[okt]].
Local Ltac generic_tac := repeat(fintros;first[solve[auto]|lazymatch goal with
 | |-P(fn_info _)=>apply P_fn_info
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
Lemma synth_preservation_group : forall fuel,
  (forall c e x exp, P (synth fuel c e x exp)) /\
  (forall c e a exp, P (synth_arg fuel c e a exp)) /\
  (forall c e b exp, P (synth_block fuel c e b exp)) /\
  (forall c e ss, P (check_stmts fuel c e ss)) /\
  (forall c e s, P (check_stmt fuel c e s)) /\
  (forall c e y, P (check_bool fuel c e y)) /\
  (forall c e x exp, P (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, P (synth_match fuel c e s arms exp)) /\
  (forall g, ConstSlot fuel g) /\
  (forall c e p a, PathSlot fuel c e p a) /\
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
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; generic_tac. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  all: solve [fintros; first [apply Const_step;exact I0|apply Path_step;exact I8|apply P_wrapper|apply P_forwarder_step;exact I28|apply P_lower_body_step;exact I2|apply P_lambda_provider_step;exact I2|cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; generic_tac]].
Qed.

End Traversal.
