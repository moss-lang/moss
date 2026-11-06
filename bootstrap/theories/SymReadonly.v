(* Non-emitting type, context and signature helpers preserve the complete
   visible function table, including temporary recursion markers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ks_mentioned_nominals : forall fuel t mi, keeps st_syms (mentioned_nominals fuel t mi).
Proof. induction fuel; fintros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve ks_mentioned_nominals : keepsdb.

Lemma ks_walk_specs : forall fuel mi sp out, keeps st_syms (walk_specs fuel mi sp out).
Proof. induction fuel; fintros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve ks_walk_specs : keepsdb.

Lemma ks_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_syms (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_syms (type_requirements fuel s)) /\
  (forall root, keeps st_syms (solve_requirements fuel root)) /\
  (forall g, keeps st_syms (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma ks_free_type_syms : forall fuel t mi out, keeps st_syms (free_type_syms fuel t mi out).
Proof. intros. apply ks_free_type_syms_group. Qed.
Lemma ks_type_requirements : forall fuel s, keeps st_syms (type_requirements fuel s).
Proof. intros. apply ks_free_type_syms_group. Qed.
Lemma ks_solve_requirements : forall fuel root, keeps st_syms (solve_requirements fuel root).
Proof. intros. apply ks_free_type_syms_group. Qed.
Lemma ks_listed_params : forall fuel g, keeps st_syms (listed_params fuel g).
Proof. intros. apply ks_free_type_syms_group. Qed.
#[export] Hint Resolve ks_free_type_syms ks_type_requirements ks_solve_requirements ks_listed_params : keepsdb.

Lemma ks_reqs_frame : forall x, keeps st_syms (reqs x).
Proof. intro x. rewrite reqs_frame_eq. apply ks_type_requirements. Qed.
#[export] Hint Resolve ks_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma ks_show_go : forall fuel t, keeps st_syms (show_go fuel t).
Proof. induction fuel; fintros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve ks_show_go : keepsdb.

Lemma ks_unify : forall fuel e mi a b why, keeps st_syms (unify fuel e mi a b why).
Proof. induction fuel; fintros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve ks_unify : keepsdb.

Lemma ks_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_syms (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_syms (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_syms (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma ks_elab_type : forall fuel t mi tm e this, keeps st_syms (elab_type fuel t mi tm e this).
Proof. intros. apply ks_elab_type_group. Qed.
Lemma ks_app_subst : forall fuel e mi bs outer, keeps st_syms (app_subst fuel e mi bs outer).
Proof. intros. apply ks_elab_type_group. Qed.
Lemma ks_symbol_type : forall fuel mi g tm e, keeps st_syms (symbol_type fuel mi g tm e).
Proof. intros. apply ks_elab_type_group. Qed.
#[export] Hint Resolve ks_elab_type ks_app_subst ks_symbol_type : keepsdb.

Lemma ks_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_syms (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_syms (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma ks_result_mentions_go : forall fuel self mi specs out, keeps st_syms (result_mentions_go fuel self mi specs out).
Proof. intros. apply ks_result_mentions_go_group. Qed.
Lemma ks_functor_fixed_go : forall fuel g, keeps st_syms (functor_fixed_go fuel g).
Proof. intros. apply ks_result_mentions_go_group. Qed.
#[export] Hint Resolve ks_result_mentions_go ks_functor_fixed_go : keepsdb.

Lemma ks_check_literal_sig : forall e mi m oty args sg,
  keeps st_syms (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve ks_check_literal_sig : keepsdb.

Lemma ks_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_syms (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_syms (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma ks_add_symbol_item : forall fuel e mi g subst needs, keeps st_syms (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply ks_add_symbol_item_group. Qed.
Lemma ks_add_spec : forall fuel e mi sp outer needs, keeps st_syms (add_spec fuel e mi sp outer needs).
Proof. intros. apply ks_add_symbol_item_group. Qed.
#[export] Hint Resolve ks_add_symbol_item ks_add_spec : keepsdb.

Lemma ks_merged_walk_frame : forall mi walk,
 (forall e, keeps st_syms (walk e)) -> keeps st_syms (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve ks_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma ks_fun_env_input:forall g,keeps st_syms(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply ks_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve ks_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma ks_fn_info_frame : forall g, keeps st_syms (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply ks_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve ks_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma ks_ety_ty_go : forall fuel t, keeps st_syms (ety_ty_go fuel t).
Proof. induction fuel; fintros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve ks_ety_ty_go : keepsdb.

Lemma ks_slots_go : forall fuel e t visiting, keeps st_syms (slots_go fuel e t visiting).
Proof. induction fuel; fintros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve ks_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma ks_expand_spec : forall fuel mi sp out, keeps st_syms (expand_spec fuel mi sp out).
Proof. induction fuel; fintros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve ks_expand_spec : keepsdb.

Lemma ks_functor_total : forall fuel g, keeps st_syms (functor_total fuel g).
Proof. induction fuel; fintros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve ks_functor_total : keepsdb.

Lemma ks_provide_spec : forall fuel e fg mi sp outer out, keeps st_syms (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; fintros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve ks_provide_spec : keepsdb.

Lemma ks_family_leaves_ok : forall fuel mi ps specs, keeps st_syms (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; fintros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve ks_family_leaves_ok : keepsdb.

Lemma ks_family_leaves : forall fuel e mi specs outer, keeps st_syms (family_leaves fuel e mi specs outer).
Proof. induction fuel; fintros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve ks_family_leaves : keepsdb.

Lemma ks_compile_pattern : forall mi e p sty heads, keeps st_syms (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
#[export]Hint Resolve ks_compile_pattern:keepsdb.
Lemma ks_payload_type_input:forall t e,keeps st_syms(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[export]Hint Resolve ks_payload_type_input:keepsdb.
Lemma ks_check_relevant_input:forall mi g a,keeps st_syms(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[export]Hint Resolve ks_check_relevant_input:keepsdb.
Lemma ks_fn_sig_input:forall g tm e this,keeps st_syms(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[export]Hint Resolve ks_fn_sig_input:keepsdb.
Lemma ks_ety_of_input:forall t mi tm e this,keeps st_syms(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[export]Hint Resolve ks_ety_of_input:keepsdb.

Lemma ks_check_captured_reading : forall mi site prototype formal actual,
 keeps st_syms(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
#[export] Hint Resolve ks_check_captured_reading : keepsdb.
Lemma ks_fresh_marker : forall e,keeps st_syms(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
#[export] Hint Resolve ks_fresh_marker : keepsdb.

Lemma ks_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_syms(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
#[export] Hint Resolve ks_check_link_fixed_current : keepsdb.

Lemma ks_runtime_provider_inputs : forall h,keeps st_syms(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;keepst. Qed.
#[export] Hint Resolve ks_runtime_provider_inputs : keepsdb.
Lemma ks_check_platform_entry_view : forall e k entry,keeps st_syms(check_platform_entry_view e k entry).
Proof. intros;unfold check_platform_entry_view;keepst. Qed.
#[export] Hint Resolve ks_check_platform_entry_view : keepsdb.

Lemma ks_fresh_marker_for : forall e r,keeps st_syms(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[export] Hint Resolve ks_fresh_marker_for : keepsdb.
