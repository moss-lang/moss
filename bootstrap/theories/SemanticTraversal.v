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
From NM Require Import Audit NMap Sx Elab ElabFrameCore ElabKeeps ReadonlyHelpers ReifyHeader ReifyData.
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
(* Certify each quiet helper once. The traversal dispatches by its head
   instead of rediscovering preservation through repeated tactic search. *)
Local Lemma shared_record_event : forall event, P (record_reify_event event).
Proof. intros;apply P_base;[apply kr_record_event|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_record_exports_at : forall owner types items inputs promised actual roles,
 P (record_reify_exports_at owner types items inputs promised actual roles).
Proof. intros;apply P_base;[apply kr_record_exports_at|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_register_platform : forall u e needs sg,
 P (register_reify_code (ROPlatform u) e needs sg).
Proof. intros;apply P_base;[apply kr_register_platform|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_register_link : forall u g e needs sg,
 P (register_reify_code (ROLink u g) e needs sg).
Proof. intros;apply P_base;[apply kr_register_link|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_walk_specs : forall fuel mi sp out, P (walk_specs fuel mi sp out).
Proof. intros;apply P_base;[apply kr_walk_specs|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_free_type_syms : forall fuel t mi out, P (free_type_syms fuel t mi out).
Proof. intros;apply P_base;[apply kr_free_type_syms|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_reqs_frame : forall x, P (reqs x).
Proof. intros;apply P_base;[apply kr_reqs_frame|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_show_go : forall fuel t, P (show_go fuel t).
Proof. intros;apply P_base;[apply kr_show_go|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_app_subst : forall fuel e mi bs outer, P (app_subst fuel e mi bs outer).
Proof. intros;apply P_base;[apply kr_app_subst|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_symbol_type : forall fuel mi g tm e, P (symbol_type fuel mi g tm e).
Proof. intros;apply P_base;[apply kr_symbol_type|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_result_mentions_go : forall fuel self mi specs out, P (result_mentions_go fuel self mi specs out).
Proof. intros;apply P_base;[apply kr_result_mentions_go|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_functor_fixed_go : forall fuel g, P (functor_fixed_go fuel g).
Proof. intros;apply P_base;[apply kr_functor_fixed_go|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_check_literal_sig : forall e mi m oty args sg,
  P (check_literal_sig e mi m oty args sg).
Proof. intros;apply P_base;[apply kr_check_literal_sig|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_fun_env_input : forall g,P(fun_env g).
Proof. intros;apply P_base;[apply kr_fun_env_input|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_ety_ty_go : forall fuel t, P (ety_ty_go fuel t).
Proof. intros;apply P_base;[apply kr_ety_ty_go|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_slots_go : forall fuel e t visiting, P (slots_go fuel e t visiting).
Proof. intros;apply P_base;[apply kr_slots_go|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_expand_spec : forall fuel mi sp out, P (expand_spec fuel mi sp out).
Proof. intros;apply P_base;[apply kr_expand_spec|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_functor_total : forall fuel g, P (functor_total fuel g).
Proof. intros;apply P_base;[apply kr_functor_total|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_provide_spec : forall fuel e fg mi sp outer out, P (provide_spec fuel e fg mi sp outer out).
Proof. intros;apply P_base;[apply kr_provide_spec|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_family_leaves_ok : forall fuel mi ps specs, P (family_leaves_ok fuel mi ps specs).
Proof. intros;apply P_base;[apply kr_family_leaves_ok|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_family_leaves : forall fuel e mi specs outer, P (family_leaves fuel e mi specs outer).
Proof. intros;apply P_base;[apply kr_family_leaves|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_compile_pattern : forall mi e p sty heads, P (compile_pattern mi e p sty heads).
Proof. intros;apply P_base;[apply kr_compile_pattern|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_payload_type_input : forall t e,P(payload_type t e).
Proof. intros;apply P_base;[apply kr_payload_type_input|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_check_relevant_input : forall mi g a,P(check_relevant mi g a).
Proof. intros;apply P_base;[apply kr_check_relevant_input|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_fn_sig_input : forall g tm e this,P(fn_sig g tm e this).
Proof. intros;apply P_base;[apply kr_fn_sig_input|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_ety_of_input : forall t mi tm e this,P(ety_of t mi tm e this).
Proof. intros;apply P_base;[apply kr_ety_of_input|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_check_captured_reading : forall mi site prototype formal actual,
 P(check_captured_reading mi site prototype formal actual).
Proof. intros;apply P_base;[apply kr_check_captured_reading|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_check_link_fixed_current : forall mi site fg bg fixed,
 P(check_link_fixed_current mi site fg bg fixed).
Proof. intros;apply P_base;[apply kr_check_link_fixed_current|solve[nit]|solve[okt]]. Qed.





Local Lemma shared_fresh_marker_for : forall e r,P(fresh_marker_for e r).
Proof. intros;apply P_base;[apply kr_fresh_marker_for|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_check_provider_effects : forall mi bs,P(check_provider_effects mi bs).
Proof. intros;apply P_base;[apply kr_check_provider_effects|solve[nit]|solve[okt]]. Qed.

Local Lemma shared_fresh : P fresh.
Proof. apply P_base;[solve[auto with keepsdb]|unfold fresh;nit|apply ok_fresh]. Qed.
Local Lemma shared_local_var : forall n,P(local_var n).
Proof. intros;apply P_base;[solve[auto with keepsdb]|unfold local_var;nit|apply ok_local_var]. Qed.
Local Lemma shared_fld_id : forall n,P(fld_id n).
Proof. intros;apply P_base;[solve[auto with keepsdb]|unfold fld_id;nit|apply ok_fld_id]. Qed.
Local Lemma shared_unsupported : forall n,P(unsupported n).
Proof. intros;apply P_base;[solve[auto with keepsdb]|unfold unsupported;nit|apply ok_unsupported]. Qed.
Local Lemma shared_new_sym : forall mi n k d o,
 o<>OSource->k<>KTag /\ k<>KUnit->P(new_sym mi n k d o).
Proof. intros;apply P_base;[solve[auto with keepsdb]|unfold new_sym;nit|apply ok_new_sym;assumption]. Qed.

Local Lemma shared_get : P get.
Proof. apply P_base;[apply keeps_get|unfold get;nit|apply ok_get]. Qed.
Local Lemma shared_lift : forall A(r:res A),P(lift r).
Proof. intros;apply P_base;[apply keeps_lift|unfold lift;nit|apply ok_lift]. Qed.
Local Lemma shared_fail : forall A msg,P(@fail A msg).
Proof. intros;apply P_base;[apply keeps_fail|apply ni_fail|apply ok_fail]. Qed.

Local Ltac generic_named :=
 lazymatch goal with |-P ?m=> let h:=head_of m in lazymatch h with
  | get=>apply shared_get
  | lift=>apply shared_lift
  | fail=>apply shared_fail
  | fresh=>apply shared_fresh
  | local_var=>apply shared_local_var
  | fld_id=>apply shared_fld_id
  | unsupported=>apply shared_unsupported
  | new_sym=>apply shared_new_sym;[discriminate|split;discriminate]
  | record_reify_event => apply shared_record_event
  | record_reify_exports_at => apply shared_record_exports_at
  | register_reify_code => first [apply shared_register_platform|apply shared_register_link]
  | walk_specs => apply shared_walk_specs
  | free_type_syms => apply shared_free_type_syms
  | reqs => apply shared_reqs_frame
  | show_go => apply shared_show_go
  | app_subst => apply shared_app_subst
  | symbol_type => apply shared_symbol_type
  | result_mentions_go => apply shared_result_mentions_go
  | functor_fixed_go => apply shared_functor_fixed_go
  | check_literal_sig => apply shared_check_literal_sig
  | fun_env => apply shared_fun_env_input
  | ety_ty_go => apply shared_ety_ty_go
  | slots_go => apply shared_slots_go
  | expand_spec => apply shared_expand_spec
  | functor_total => apply shared_functor_total
  | provide_spec => apply shared_provide_spec
  | family_leaves_ok => apply shared_family_leaves_ok
  | family_leaves => apply shared_family_leaves
  | compile_pattern => apply shared_compile_pattern
  | payload_type => apply shared_payload_type_input
  | check_relevant => apply shared_check_relevant_input
  | fn_sig => apply shared_fn_sig_input
  | ety_of => apply shared_ety_of_input
  | check_captured_reading => apply shared_check_captured_reading
  | check_link_fixed_current => apply shared_check_link_fixed_current
  | fresh_marker_for => apply shared_fresh_marker_for
  | check_provider_effects => apply shared_check_provider_effects
 end end.
Local Lemma shared_reader : forall A(m:M A),
 (forall s a final,m s=Good(a,final)->final=s)->P m.
Proof.
 intros A m R;apply P_base.
 - intros s a final Run;rewrite(R s a final Run);reflexivity.
 - intros s a final Run;rewrite(R s a final Run);reflexivity.
 - apply ok_reader;exact R.
Qed.
Local Ltac generic_reader :=
 apply shared_reader; let s:=fresh "s" in let a:=fresh "a" in
 let final:=fresh "final" in let Run:=fresh "Run" in
 intros s a final Run;
 repeat(match type of Run with context[match ?x with _=>_ end]=>destruct x end);
 try discriminate Run;injection Run as _ <-;reflexivity.

Local Ltac generic_leaf := first[solve[generic_reader]|apply P_base;[solve[keepst]|solve[nit]|solve[okt]]].
(* The shortcut uses the previous fuel hypotheses. The structural cases
   apply the construction methods. *)
Local Ltac generic_tac previous := repeat(fintros;first[solve[apply P_ret]|solve[previous ()]|lazymatch goal with
 | |-P(fn_info _)=>apply P_fn_info
 | |-P(mbind _ _)=>apply P_bind
 | |-P(mmap _ _)=>apply P_map
 | |-P(mfold _ _ _)=>apply P_fold
 | |-P(miter _ _)=>apply P_iter
 | |-P(with_vars _ _)=>apply P_vars
 | |-P(in_reify_code _ _)=>unfold in_reify_code
 | |-P ?m=>first[solve[generic_named]|lazymatch m with
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
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; generic_tac ltac:(fun _ => solve[clear P_base P_bind P_map P_fold P_iter P_vars P_fn_info P_wrapper P_forwarder_step P_lower_body_step P_lambda_provider_step Const_step Path_step;auto 2]). }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))).
  (* Select the previous-fuel fact by its function, without searching
     every induction hypothesis at every monadic step. *)
  all: let previous _ :=
    lazymatch goal with |- P ?m =>
      let h := head_of m in lazymatch h with
      | synth => apply I0
      | synth_arg => apply I1
      | synth_block => apply I2
      | check_stmts => apply I3
      | check_stmt => apply I4
      | check_bool => apply I5
      | synth_if => apply I6
      | synth_match => apply I7
      | check_args => apply I10
      | synth_call => apply I11
      | synth_record => apply I12
      | synth_field => apply I13
      | synth_method => apply I14
      | provided_call => apply I15
      | apply_family => apply I16
      | demand_needs => apply I17
      | synth_operator => apply I18
      | lang_call => apply I19
      | int_literal => apply I20
      | float_of => apply I21
      | synth_literal => apply I22
      | check_bind => apply I23
      | expect_fn_provider => apply I24
      | match_fn_sig => apply I25
      | synth_path => apply Path_lift; apply I9
      | wrapper => apply I26
      | forwarder => apply I27
      | lambda_provider => apply I29
      | apply_functor => apply I30
      | apply_body => apply I31
      | apply_abstract => apply I32
      | bind_functor => apply I33
      | link_body => apply I34
      end
    end in
    solve [fintros; first [apply Const_step;exact I0|apply Path_step;exact I8|apply P_wrapper|apply P_forwarder_step;exact I28|apply P_lower_body_step;exact I2|apply P_lambda_provider_step;exact I2|cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; generic_tac previous]].
Qed.

End Traversal.
