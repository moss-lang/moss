(* Non-emitting type, context and signature helpers preserve the complete
   visible function table, including temporary recursion markers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma kf_mentioned_nominals : forall fuel t mi, keeps st_fns (mentioned_nominals fuel t mi).
Proof. induction fuel; fintros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve kf_mentioned_nominals : keepsdb.

Lemma kf_walk_specs : forall fuel mi sp out, keeps st_fns (walk_specs fuel mi sp out).
Proof. induction fuel; fintros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve kf_walk_specs : keepsdb.

Lemma kf_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_fns (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_fns (type_requirements fuel s)) /\
  (forall root, keeps st_fns (solve_requirements fuel root)) /\
  (forall g, keeps st_fns (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma kf_free_type_syms : forall fuel t mi out, keeps st_fns (free_type_syms fuel t mi out).
Proof. intros. apply kf_free_type_syms_group. Qed.
Lemma kf_type_requirements : forall fuel s, keeps st_fns (type_requirements fuel s).
Proof. intros. apply kf_free_type_syms_group. Qed.
Lemma kf_solve_requirements : forall fuel root, keeps st_fns (solve_requirements fuel root).
Proof. intros. apply kf_free_type_syms_group. Qed.
Lemma kf_listed_params : forall fuel g, keeps st_fns (listed_params fuel g).
Proof. intros. apply kf_free_type_syms_group. Qed.
#[export] Hint Resolve kf_free_type_syms kf_type_requirements kf_solve_requirements kf_listed_params : keepsdb.

Lemma kf_reqs_frame : forall x, keeps st_fns (reqs x).
Proof. intro x. rewrite reqs_frame_eq. apply kf_type_requirements. Qed.
#[export] Hint Resolve kf_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kf_show_go : forall fuel t, keeps st_fns (show_go fuel t).
Proof. induction fuel; fintros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve kf_show_go : keepsdb.

Lemma kf_unify : forall fuel e mi a b why, keeps st_fns (unify fuel e mi a b why).
Proof. induction fuel; fintros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve kf_unify : keepsdb.

Lemma kf_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_fns (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_fns (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_fns (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma kf_elab_type : forall fuel t mi tm e this, keeps st_fns (elab_type fuel t mi tm e this).
Proof. intros. apply kf_elab_type_group. Qed.
Lemma kf_app_subst : forall fuel e mi bs outer, keeps st_fns (app_subst fuel e mi bs outer).
Proof. intros. apply kf_elab_type_group. Qed.
Lemma kf_symbol_type : forall fuel mi g tm e, keeps st_fns (symbol_type fuel mi g tm e).
Proof. intros. apply kf_elab_type_group. Qed.
#[export] Hint Resolve kf_elab_type kf_app_subst kf_symbol_type : keepsdb.

Lemma kf_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_fns (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_fns (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma kf_result_mentions_go : forall fuel self mi specs out, keeps st_fns (result_mentions_go fuel self mi specs out).
Proof. intros. apply kf_result_mentions_go_group. Qed.
Lemma kf_functor_fixed_go : forall fuel g, keeps st_fns (functor_fixed_go fuel g).
Proof. intros. apply kf_result_mentions_go_group. Qed.
#[export] Hint Resolve kf_result_mentions_go kf_functor_fixed_go : keepsdb.

Lemma kf_check_literal_sig : forall e mi m oty args sg,
  keeps st_fns (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve kf_check_literal_sig : keepsdb.

Lemma kf_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_fns (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_fns (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma kf_add_symbol_item : forall fuel e mi g subst needs, keeps st_fns (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply kf_add_symbol_item_group. Qed.
Lemma kf_add_spec : forall fuel e mi sp outer needs, keeps st_fns (add_spec fuel e mi sp outer needs).
Proof. intros. apply kf_add_symbol_item_group. Qed.
#[export] Hint Resolve kf_add_symbol_item kf_add_spec : keepsdb.

Lemma kf_merged_walk_frame : forall mi walk,
 (forall e, keeps st_fns (walk e)) -> keeps st_fns (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kf_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kf_fun_env_input:forall g,keeps st_fns(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply kf_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve kf_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kf_fn_info_frame : forall g, keeps st_fns (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply kf_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve kf_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kf_ety_ty_go : forall fuel t, keeps st_fns (ety_ty_go fuel t).
Proof. induction fuel; fintros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kf_ety_ty_go : keepsdb.

Lemma kf_slots_go : forall fuel e t visiting, keeps st_fns (slots_go fuel e t visiting).
Proof. induction fuel; fintros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kf_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kf_expand_spec : forall fuel mi sp out, keeps st_fns (expand_spec fuel mi sp out).
Proof. induction fuel; fintros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve kf_expand_spec : keepsdb.

Lemma kf_functor_total : forall fuel g, keeps st_fns (functor_total fuel g).
Proof. induction fuel; fintros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve kf_functor_total : keepsdb.

Lemma kf_provide_spec : forall fuel e fg mi sp outer out, keeps st_fns (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; fintros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve kf_provide_spec : keepsdb.

Lemma kf_family_leaves_ok : forall fuel mi ps specs, keeps st_fns (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; fintros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve kf_family_leaves_ok : keepsdb.

Lemma kf_family_leaves : forall fuel e mi specs outer, keeps st_fns (family_leaves fuel e mi specs outer).
Proof. induction fuel; fintros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve kf_family_leaves : keepsdb.

Lemma kf_compile_pattern : forall mi e p sty heads, keeps st_fns (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
#[export]Hint Resolve kf_compile_pattern:keepsdb.
Lemma kf_payload_type_input:forall t e,keeps st_fns(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[export]Hint Resolve kf_payload_type_input:keepsdb.
Lemma kf_check_relevant_input:forall mi g a,keeps st_fns(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[export]Hint Resolve kf_check_relevant_input:keepsdb.
Lemma kf_fn_sig_input:forall g tm e this,keeps st_fns(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[export]Hint Resolve kf_fn_sig_input:keepsdb.
Lemma kf_ety_of_input:forall t mi tm e this,keeps st_fns(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[export]Hint Resolve kf_ety_of_input:keepsdb.

Lemma kf_check_captured_reading : forall mi site prototype formal actual,
 keeps st_fns(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
#[export] Hint Resolve kf_check_captured_reading : keepsdb.
Lemma kf_fresh_marker : forall e,keeps st_fns(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
#[export] Hint Resolve kf_fresh_marker : keepsdb.

Lemma kf_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_fns(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
#[export] Hint Resolve kf_check_link_fixed_current : keepsdb.

Lemma kf_platform_fun : forall g,keeps st_fns(platform_fun g).
Proof. intros;unfold platform_fun;keepst. Qed.
#[export] Hint Resolve kf_platform_fun : keepsdb.
Lemma kf_platform_entry : forall k prims,keeps st_fns(platform_entry k prims).
Proof. intros;unfold platform_entry;keepst. Qed.
#[export] Hint Resolve kf_platform_entry : keepsdb.
Lemma kf_runtime_provider_inputs : forall h,keeps st_fns(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;keepst. Qed.
#[export] Hint Resolve kf_runtime_provider_inputs : keepsdb.
Lemma kf_check_platform_entry_view : forall e k entry,keeps st_fns(check_platform_entry_view e k entry).
Proof. intros;unfold check_platform_entry_view;keepst. Qed.
#[export] Hint Resolve kf_check_platform_entry_view : keepsdb.

Lemma kf_fresh_marker_for : forall e r,keeps st_fns(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[export] Hint Resolve kf_fresh_marker_for : keepsdb.

Lemma kf_check_constant_effects:forall mi x,keeps st_fns(check_constant_effects mi x).
Proof. intros;unfold check_constant_effects;keepst. Qed.
Lemma kf_check_provider_effects:forall mi bs,keeps st_fns(check_provider_effects mi bs).
Proof. intros;unfold check_provider_effects;keepst. Qed.
#[export]Hint Resolve kf_check_constant_effects kf_check_provider_effects:keepsdb.
