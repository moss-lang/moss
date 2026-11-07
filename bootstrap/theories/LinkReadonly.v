(* Quiet helpers leave linked-functor creation history unchanged. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabKeeps ReifyData.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma kl_record_event : forall event, keeps st_links (record_reify_event event).
Proof. intro event;unfold record_reify_event;keepst. Qed.
Lemma kl_record_exports_at : forall owner types items inputs promised actual roles,
 keeps st_links (record_reify_exports_at owner types items inputs promised actual roles).
Proof. intros;unfold record_reify_exports_at;keepst. Qed.
Lemma kl_register_platform : forall u e needs sg,
 keeps st_links (register_reify_code (ROPlatform u) e needs sg).
Proof. intros. unfold register_reify_code; keepst. Qed.
Lemma kl_register_link : forall u g e needs sg,
 keeps st_links (register_reify_code (ROLink u g) e needs sg).
Proof. intros. unfold register_reify_code; keepst. Qed.
#[export] Hint Resolve kl_record_event kl_record_exports_at kl_register_platform kl_register_link : keepsdb.

Lemma kl_mentioned_nominals : forall fuel t mi, keeps st_links (mentioned_nominals fuel t mi).
Proof. induction fuel; keeps_intros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve kl_mentioned_nominals : keepsdb.

Lemma kl_walk_specs : forall fuel mi sp out, keeps st_links (walk_specs fuel mi sp out).
Proof. induction fuel; keeps_intros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve kl_walk_specs : keepsdb.

Lemma kl_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_links (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_links (type_requirements fuel s)) /\
  (forall root, keeps st_links (solve_requirements fuel root)) /\
  (forall g, keeps st_links (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma kl_free_type_syms : forall fuel t mi out, keeps st_links (free_type_syms fuel t mi out).
Proof. intros. apply kl_free_type_syms_group. Qed.
Lemma kl_type_requirements : forall fuel s, keeps st_links (type_requirements fuel s).
Proof. intros. apply kl_free_type_syms_group. Qed.
Lemma kl_solve_requirements : forall fuel root, keeps st_links (solve_requirements fuel root).
Proof. intros. apply kl_free_type_syms_group. Qed.
Lemma kl_listed_params : forall fuel g, keeps st_links (listed_params fuel g).
Proof. intros. apply kl_free_type_syms_group. Qed.
#[export] Hint Resolve kl_free_type_syms kl_type_requirements kl_solve_requirements kl_listed_params : keepsdb.

Lemma kl_reqs_frame : forall x, keeps st_links (reqs x).
Proof. intro x. unfold reqs. apply kl_type_requirements. Qed.
#[export] Hint Resolve kl_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kl_show_go : forall fuel t, keeps st_links (show_go fuel t).
Proof. induction fuel; keeps_intros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve kl_show_go : keepsdb.

Lemma kl_unify : forall fuel e mi a b why, keeps st_links (unify fuel e mi a b why).
Proof. induction fuel; keeps_intros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve kl_unify : keepsdb.

Lemma kl_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_links (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_links (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_links (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma kl_elab_type : forall fuel t mi tm e this, keeps st_links (elab_type fuel t mi tm e this).
Proof. intros. apply kl_elab_type_group. Qed.
Lemma kl_app_subst : forall fuel e mi bs outer, keeps st_links (app_subst fuel e mi bs outer).
Proof. intros. apply kl_elab_type_group. Qed.
Lemma kl_symbol_type : forall fuel mi g tm e, keeps st_links (symbol_type fuel mi g tm e).
Proof. intros. apply kl_elab_type_group. Qed.
#[export] Hint Resolve kl_elab_type kl_app_subst kl_symbol_type : keepsdb.

Lemma kl_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_links (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_links (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma kl_result_mentions_go : forall fuel self mi specs out, keeps st_links (result_mentions_go fuel self mi specs out).
Proof. intros. apply kl_result_mentions_go_group. Qed.
Lemma kl_functor_fixed_go : forall fuel g, keeps st_links (functor_fixed_go fuel g).
Proof. intros. apply kl_result_mentions_go_group. Qed.
#[export] Hint Resolve kl_result_mentions_go kl_functor_fixed_go : keepsdb.

Lemma kl_check_literal_sig : forall e mi m oty args sg,
  keeps st_links (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve kl_check_literal_sig : keepsdb.

Lemma kl_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_links (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_links (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma kl_add_symbol_item : forall fuel e mi g subst needs, keeps st_links (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply kl_add_symbol_item_group. Qed.
Lemma kl_add_spec : forall fuel e mi sp outer needs, keeps st_links (add_spec fuel e mi sp outer needs).
Proof. intros. apply kl_add_symbol_item_group. Qed.
#[export] Hint Resolve kl_add_symbol_item kl_add_spec : keepsdb.

Lemma kl_merged_walk_frame : forall mi walk,
 (forall e, keeps st_links (walk e)) -> keeps st_links (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kl_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kl_fun_env_input:forall g,keeps st_links(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply kl_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve kl_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kl_fn_info_frame : forall g, keeps st_links (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply kl_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve kl_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kl_ety_ty_go : forall fuel t, keeps st_links (ety_ty_go fuel t).
Proof. induction fuel; keeps_intros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kl_ety_ty_go : keepsdb.

Lemma kl_slots_go : forall fuel e t visiting, keeps st_links (slots_go fuel e t visiting).
Proof. induction fuel; keeps_intros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kl_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kl_expand_spec : forall fuel mi sp out, keeps st_links (expand_spec fuel mi sp out).
Proof. induction fuel; keeps_intros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve kl_expand_spec : keepsdb.

Lemma kl_functor_total : forall fuel g, keeps st_links (functor_total fuel g).
Proof. induction fuel; keeps_intros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve kl_functor_total : keepsdb.

Lemma kl_provide_spec : forall fuel e fg mi sp outer out, keeps st_links (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; keeps_intros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve kl_provide_spec : keepsdb.

Lemma kl_family_leaves_ok : forall fuel mi ps specs, keeps st_links (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; keeps_intros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve kl_family_leaves_ok : keepsdb.

Lemma kl_family_leaves : forall fuel e mi specs outer, keeps st_links (family_leaves fuel e mi specs outer).
Proof. induction fuel; keeps_intros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve kl_family_leaves : keepsdb.

Lemma kl_compile_pattern : forall mi e p sty heads, keeps st_links (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
#[export]Hint Resolve kl_compile_pattern:keepsdb.
Lemma kl_payload_type_input:forall t e,keeps st_links(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[export]Hint Resolve kl_payload_type_input:keepsdb.
Lemma kl_check_relevant_input:forall mi g a,keeps st_links(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[export]Hint Resolve kl_check_relevant_input:keepsdb.
Lemma kl_fn_sig_input:forall g tm e this,keeps st_links(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[export]Hint Resolve kl_fn_sig_input:keepsdb.
Lemma kl_ety_of_input:forall t mi tm e this,keeps st_links(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[export]Hint Resolve kl_ety_of_input:keepsdb.

Lemma kl_check_captured_reading : forall mi site prototype formal actual,
 keeps st_links(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
#[export] Hint Resolve kl_check_captured_reading : keepsdb.
Lemma kl_fresh_marker : forall e,keeps st_links(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
#[export] Hint Resolve kl_fresh_marker : keepsdb.

Lemma kl_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_links(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
#[export] Hint Resolve kl_check_link_fixed_current : keepsdb.

Lemma kl_runtime_provider_inputs : forall h,keeps st_links(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;keepst. Qed.
#[export] Hint Resolve kl_runtime_provider_inputs : keepsdb.
Lemma kl_check_platform_entry_view : forall e k entry,keeps st_links(check_platform_entry_view e k entry).
Proof. intros;unfold check_platform_entry_view;keepst. Qed.
#[export] Hint Resolve kl_check_platform_entry_view : keepsdb.

Lemma kl_fresh_marker_for : forall e r,keeps st_links(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[export] Hint Resolve kl_fresh_marker_for : keepsdb.

Lemma kl_check_constant_effects:forall mi x,keeps st_links(check_constant_effects mi x).
Proof. intros;unfold check_constant_effects;keepst. Qed.
Lemma kl_check_provider_effects:forall mi bs,keeps st_links(check_provider_effects mi bs).
Proof. intros;unfold check_provider_effects;keepst. Qed.
#[export]Hint Resolve kl_check_constant_effects kl_check_provider_effects:keepsdb.

(* Whole-program assumption checks revisit the same requirement helper under
   several declaration kinds. Prove its complete readonly effect once. *)
Lemma kl_check_assumed_requirements:forall mi e k,
 keeps st_links(check_assumed_requirements mi e k).
Proof. intros;unfold check_assumed_requirements;keepst. Qed.
#[export]Hint Resolve kl_check_assumed_requirements:keepsdb.
Lemma kl_check_assumes:keeps st_links check_assumes.
Proof. unfold check_assumes;keepst. Qed.
#[export]Hint Resolve kl_check_assumes:keepsdb.
