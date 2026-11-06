(* Type, context and signature helpers preserve function scope metadata. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv ReifyHeader.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma kh_record_event:forall event,keeps function_headers(record_reify_event event).
Proof. unfold keeps;intros event s [] final Run;eapply record_event_header_rows;exact Run. Qed.
Lemma kh_record_exports_at:forall owner types items inputs promised actual roles,
 keeps function_headers(record_reify_exports_at owner types items inputs promised actual roles).
Proof. unfold keeps;intros owner types items inputs promised actual roles s [] final Run;eapply record_exports_header_rows;exact Run. Qed.
Lemma kh_register_platform:forall u e needs sg,
 keeps function_headers(register_reify_code(ROPlatform u)e needs sg).
Proof. unfold keeps;intros;unfold register_reify_code,upd in H;injection H as <- <-;reflexivity. Qed.
Lemma kh_register_link:forall u g e needs sg,
 keeps function_headers(register_reify_code(ROLink u g)e needs sg).
Proof. unfold keeps;intros;unfold register_reify_code,upd in H;injection H as <- <-;reflexivity. Qed.
#[export]Hint Resolve kh_record_event kh_record_exports_at kh_register_platform kh_register_link:keepsdb.

Lemma kh_mentioned_nominals : forall fuel t mi, keeps function_headers (mentioned_nominals fuel t mi).
Proof. induction fuel; fintros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve kh_mentioned_nominals : keepsdb.

Lemma kh_walk_specs : forall fuel mi sp out, keeps function_headers (walk_specs fuel mi sp out).
Proof. induction fuel; fintros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve kh_walk_specs : keepsdb.

Lemma kh_free_type_syms_group : forall fuel,
  (forall t mi out, keeps function_headers (free_type_syms fuel t mi out)) /\
  (forall s, keeps function_headers (type_requirements fuel s)) /\
  (forall root, keeps function_headers (solve_requirements fuel root)) /\
  (forall g, keeps function_headers (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma kh_free_type_syms : forall fuel t mi out, keeps function_headers (free_type_syms fuel t mi out).
Proof. intros. apply kh_free_type_syms_group. Qed.
Lemma kh_type_requirements : forall fuel s, keeps function_headers (type_requirements fuel s).
Proof. intros. apply kh_free_type_syms_group. Qed.
Lemma kh_solve_requirements : forall fuel root, keeps function_headers (solve_requirements fuel root).
Proof. intros. apply kh_free_type_syms_group. Qed.
Lemma kh_listed_params : forall fuel g, keeps function_headers (listed_params fuel g).
Proof. intros. apply kh_free_type_syms_group. Qed.
#[export] Hint Resolve kh_free_type_syms kh_type_requirements kh_solve_requirements kh_listed_params : keepsdb.

Lemma kh_reqs_frame : forall x, keeps function_headers (reqs x).
Proof. intro x. rewrite reqs_frame_eq. apply kh_type_requirements. Qed.
#[export] Hint Resolve kh_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kh_show_go : forall fuel t, keeps function_headers (show_go fuel t).
Proof. induction fuel; fintros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve kh_show_go : keepsdb.

Lemma kh_unify : forall fuel e mi a b why, keeps function_headers (unify fuel e mi a b why).
Proof. induction fuel; fintros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve kh_unify : keepsdb.

Lemma kh_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps function_headers (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps function_headers (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps function_headers (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma kh_elab_type : forall fuel t mi tm e this, keeps function_headers (elab_type fuel t mi tm e this).
Proof. intros. apply kh_elab_type_group. Qed.
Lemma kh_app_subst : forall fuel e mi bs outer, keeps function_headers (app_subst fuel e mi bs outer).
Proof. intros. apply kh_elab_type_group. Qed.
Lemma kh_symbol_type : forall fuel mi g tm e, keeps function_headers (symbol_type fuel mi g tm e).
Proof. intros. apply kh_elab_type_group. Qed.
#[export] Hint Resolve kh_elab_type kh_app_subst kh_symbol_type : keepsdb.

Lemma kh_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps function_headers (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps function_headers (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma kh_result_mentions_go : forall fuel self mi specs out, keeps function_headers (result_mentions_go fuel self mi specs out).
Proof. intros. apply kh_result_mentions_go_group. Qed.
Lemma kh_functor_fixed_go : forall fuel g, keeps function_headers (functor_fixed_go fuel g).
Proof. intros. apply kh_result_mentions_go_group. Qed.
#[export] Hint Resolve kh_result_mentions_go kh_functor_fixed_go : keepsdb.

Lemma kh_check_literal_sig : forall e mi m oty args sg,
  keeps function_headers (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve kh_check_literal_sig : keepsdb.

Lemma kh_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps function_headers (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps function_headers (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma kh_add_symbol_item : forall fuel e mi g subst needs, keeps function_headers (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply kh_add_symbol_item_group. Qed.
Lemma kh_add_spec : forall fuel e mi sp outer needs, keeps function_headers (add_spec fuel e mi sp outer needs).
Proof. intros. apply kh_add_symbol_item_group. Qed.
#[export] Hint Resolve kh_add_symbol_item kh_add_spec : keepsdb.

Lemma kh_merged_walk_frame : forall mi walk,
 (forall e, keeps function_headers (walk e)) -> keeps function_headers (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kh_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kh_fun_env_input:forall g,keeps function_headers(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply kh_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve kh_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kh_fn_info_frame : forall g, keeps function_headers (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply kh_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve kh_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kh_ety_ty_go : forall fuel t, keeps function_headers (ety_ty_go fuel t).
Proof. induction fuel; fintros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kh_ety_ty_go : keepsdb.

Lemma kh_slots_go : forall fuel e t visiting, keeps function_headers (slots_go fuel e t visiting).
Proof. induction fuel; fintros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kh_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kh_expand_spec : forall fuel mi sp out, keeps function_headers (expand_spec fuel mi sp out).
Proof. induction fuel; fintros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve kh_expand_spec : keepsdb.

Lemma kh_functor_total : forall fuel g, keeps function_headers (functor_total fuel g).
Proof. induction fuel; fintros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve kh_functor_total : keepsdb.

Lemma kh_provide_spec : forall fuel e fg mi sp outer out, keeps function_headers (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; fintros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve kh_provide_spec : keepsdb.

Lemma kh_family_leaves_ok : forall fuel mi ps specs, keeps function_headers (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; fintros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve kh_family_leaves_ok : keepsdb.

Lemma kh_family_leaves : forall fuel e mi specs outer, keeps function_headers (family_leaves fuel e mi specs outer).
Proof. induction fuel; fintros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve kh_family_leaves : keepsdb.

Lemma kh_compile_pattern : forall mi e p sty heads, keeps function_headers (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
#[export]Hint Resolve kh_compile_pattern:keepsdb.
Lemma kh_payload_type_input:forall t e,keeps function_headers(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[export]Hint Resolve kh_payload_type_input:keepsdb.
Lemma kh_check_relevant_input:forall mi g a,keeps function_headers(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[export]Hint Resolve kh_check_relevant_input:keepsdb.
Lemma kh_fn_sig_input:forall g tm e this,keeps function_headers(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[export]Hint Resolve kh_fn_sig_input:keepsdb.
Lemma kh_ety_of_input:forall t mi tm e this,keeps function_headers(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[export]Hint Resolve kh_ety_of_input:keepsdb.

Lemma kh_check_captured_reading : forall mi site prototype formal actual,
 keeps function_headers(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
#[export] Hint Resolve kh_check_captured_reading : keepsdb.
Lemma kh_fresh_marker : forall e,keeps function_headers(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
#[export] Hint Resolve kh_fresh_marker : keepsdb.

Lemma kh_check_link_fixed_current : forall mi site fg bg fixed,
 keeps function_headers(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
#[export] Hint Resolve kh_check_link_fixed_current : keepsdb.

Lemma kh_platform_fun : forall g,keeps function_headers(platform_fun g).
Proof. intros;unfold platform_fun;keepst. Qed.
#[export] Hint Resolve kh_platform_fun : keepsdb.
Lemma kh_platform_entry : forall k prims,keeps function_headers(platform_entry k prims).
Proof. intros;unfold platform_entry;keepst. Qed.
#[export] Hint Resolve kh_platform_entry : keepsdb.
Lemma kh_runtime_provider_inputs : forall h,keeps function_headers(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;keepst. Qed.
#[export] Hint Resolve kh_runtime_provider_inputs : keepsdb.
Lemma kh_check_platform_entry_view : forall e k entry,keeps function_headers(check_platform_entry_view e k entry).
Proof. intros;unfold check_platform_entry_view;keepst. Qed.
#[export] Hint Resolve kh_check_platform_entry_view : keepsdb.

Lemma kh_fresh_marker_for : forall e r,keeps function_headers(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[export] Hint Resolve kh_fresh_marker_for : keepsdb.

Lemma kh_check_constant_effects:forall mi x,keeps function_headers(check_constant_effects mi x).
Proof. intros;unfold check_constant_effects;keepst. Qed.
Lemma kh_check_provider_effects:forall mi bs,keeps function_headers(check_provider_effects mi bs).
Proof. intros;unfold check_provider_effects;keepst. Qed.
#[export]Hint Resolve kh_check_constant_effects kh_check_provider_effects:keepsdb.
