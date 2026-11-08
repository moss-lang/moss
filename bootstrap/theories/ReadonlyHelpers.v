(* Shared preservation proofs for type, context and signature helpers.
   One traversal preserves function rows, forwarder rows, wrapper cache,
   constant cache and function headers; clients select their projection. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabKeeps ReifyData ReifyHeader.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Definition readonly_base (s : st) := (st_fns s, st_fwd s, st_wrappers s, st_consts s).
Definition readonly_state (s : st) := (readonly_base s, function_headers s).
Lemma keeps_projection : forall T U A (p : st -> T) (q : T -> U) (m : M A),
 keeps p m -> keeps (fun s => q (p s)) m.
Proof. intros T U A p q m H s a final Run. rewrite (H s a final Run). reflexivity. Qed.

Lemma readonly_headers : forall A (m : M A),
 keeps readonly_base m -> keeps function_headers m -> keeps readonly_state m.
Proof. intros A m Base Headers s a final Run. unfold readonly_state.
 rewrite (Base s a final Run), (Headers s a final Run). reflexivity. Qed.
Lemma kr_record_event : forall event, keeps readonly_state (record_reify_event event).
Proof. intro event. apply readonly_headers.
 - unfold record_reify_event; keepst.
 - unfold keeps; intros s [] final Run. eapply record_event_header_rows; exact Run.
Qed.
Lemma kr_record_exports_at : forall owner types items inputs promised actual roles,
 keeps readonly_state (record_reify_exports_at owner types items inputs promised actual roles).
Proof. intros. apply readonly_headers.
 - unfold record_reify_exports_at; keepst.
 - unfold keeps; intros s [] final Run. eapply record_exports_header_rows; exact Run.
Qed.
Lemma kr_register_platform : forall u e needs sg,
 keeps readonly_state (register_reify_code (ROPlatform u) e needs sg).
Proof. intros. unfold register_reify_code; keepst. Qed.
Lemma kr_register_link : forall u g e needs sg,
 keeps readonly_state (register_reify_code (ROLink u g) e needs sg).
Proof. intros. unfold register_reify_code; keepst. Qed.
#[export] Hint Resolve kr_record_event kr_record_exports_at kr_register_platform kr_register_link : keepsdb.

Lemma kr_mentioned_nominals : forall fuel t mi, keeps readonly_state (mentioned_nominals fuel t mi).
Proof. induction fuel; keeps_intros; cbn [mentioned_nominals]; keepst. Qed.
#[export] Hint Resolve kr_mentioned_nominals : keepsdb.

Lemma kr_walk_specs : forall fuel mi sp out, keeps readonly_state (walk_specs fuel mi sp out).
Proof. induction fuel; keeps_intros; cbn [walk_specs]; keepst. Qed.
#[export] Hint Resolve kr_walk_specs : keepsdb.

Lemma kr_free_type_syms_group : forall fuel,
  (forall t mi out, keeps readonly_state (free_type_syms fuel t mi out)) /\
  (forall s, keeps readonly_state (type_requirements fuel s)) /\
  (forall root, keeps readonly_state (solve_requirements fuel root)) /\
  (forall g, keeps readonly_state (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))); keeps_intros; cbn [free_type_syms type_requirements solve_requirements listed_params]; keepst.
Qed.

Lemma kr_free_type_syms : forall fuel t mi out, keeps readonly_state (free_type_syms fuel t mi out).
Proof. intros. apply kr_free_type_syms_group. Qed.
Lemma kr_type_requirements : forall fuel s, keeps readonly_state (type_requirements fuel s).
Proof. intros. apply kr_free_type_syms_group. Qed.
Lemma kr_solve_requirements : forall fuel root, keeps readonly_state (solve_requirements fuel root).
Proof. intros. apply kr_free_type_syms_group. Qed.
Lemma kr_listed_params : forall fuel g, keeps readonly_state (listed_params fuel g).
Proof. intros. apply kr_free_type_syms_group. Qed.
#[export] Hint Resolve kr_free_type_syms kr_type_requirements kr_solve_requirements kr_listed_params : keepsdb.

Lemma kr_reqs_frame : forall x, keeps readonly_state (reqs x).
Proof. intro x. unfold reqs. apply kr_type_requirements. Qed.
#[export] Hint Resolve kr_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kr_show_go : forall fuel t, keeps readonly_state (show_go fuel t).
Proof. induction fuel; keeps_intros; cbn [show_go]; keepst. Qed.
#[export] Hint Resolve kr_show_go : keepsdb.

Lemma kr_unify : forall fuel e mi a b why, keeps readonly_state (unify fuel e mi a b why).
Proof. induction fuel; keeps_intros; cbn [unify]; keepst. Qed.
#[export] Hint Resolve kr_unify : keepsdb.

Lemma kr_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps readonly_state (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps readonly_state (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps readonly_state (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; keepst. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); keeps_intros; cbn [elab_type app_subst symbol_type]; keepst.
Qed.

Lemma kr_elab_type : forall fuel t mi tm e this, keeps readonly_state (elab_type fuel t mi tm e this).
Proof. intros. apply kr_elab_type_group. Qed.
Lemma kr_app_subst : forall fuel e mi bs outer, keeps readonly_state (app_subst fuel e mi bs outer).
Proof. intros. apply kr_elab_type_group. Qed.
Lemma kr_symbol_type : forall fuel mi g tm e, keeps readonly_state (symbol_type fuel mi g tm e).
Proof. intros. apply kr_elab_type_group. Qed.
#[export] Hint Resolve kr_elab_type kr_app_subst kr_symbol_type : keepsdb.

Lemma kr_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps readonly_state (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps readonly_state (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [result_mentions_go functor_fixed_go]; keepst.
Qed.

Lemma kr_result_mentions_go : forall fuel self mi specs out, keeps readonly_state (result_mentions_go fuel self mi specs out).
Proof. intros. apply kr_result_mentions_go_group. Qed.
Lemma kr_functor_fixed_go : forall fuel g, keeps readonly_state (functor_fixed_go fuel g).
Proof. intros. apply kr_result_mentions_go_group. Qed.
#[export] Hint Resolve kr_result_mentions_go kr_functor_fixed_go : keepsdb.

Lemma kr_check_literal_sig : forall e mi m oty args sg,
  keeps readonly_state (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); keepst.
Qed.
#[export] Hint Resolve kr_check_literal_sig : keepsdb.

Lemma kr_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps readonly_state (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps readonly_state (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; keepst. }
  destruct IH as (I0 & I1).
  refine (conj _ _); keeps_intros; cbn [add_symbol_item add_spec]; keepst.
Qed.

Lemma kr_add_symbol_item : forall fuel e mi g subst needs, keeps readonly_state (add_symbol_item fuel e mi g subst needs).
Proof. intros. apply kr_add_symbol_item_group. Qed.
Lemma kr_add_spec : forall fuel e mi sp outer needs, keeps readonly_state (add_spec fuel e mi sp outer needs).
Proof. intros. apply kr_add_symbol_item_group. Qed.
#[export] Hint Resolve kr_add_symbol_item kr_add_spec : keepsdb.

Lemma kr_merged_walk_frame : forall mi walk,
 (forall e, keeps readonly_state (walk e)) -> keeps readonly_state (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kr_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kr_fun_env_input:forall g,keeps readonly_state(fun_env g).
Proof. intros;unfold fun_env;keepst. all:apply kr_merged_walk_frame;intros;keepst. Qed.
#[export]Hint Resolve kr_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kr_fn_info_frame : forall g, keeps readonly_state (fn_info g).
Proof. intro g. unfold fn_info. keepst. all: apply kr_merged_walk_frame; intros; keepst. Qed.
#[export] Hint Resolve kr_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kr_ety_ty_go : forall fuel t, keeps readonly_state (ety_ty_go fuel t).
Proof. induction fuel; keeps_intros; cbn [ety_ty_go]; keepst. Qed.
#[export] Hint Resolve kr_ety_ty_go : keepsdb.

Lemma kr_slots_go : forall fuel e t visiting, keeps readonly_state (slots_go fuel e t visiting).
Proof. induction fuel; keeps_intros; cbn [slots_go]; keepst. Qed.
#[export] Hint Resolve kr_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kr_expand_spec : forall fuel mi sp out, keeps readonly_state (expand_spec fuel mi sp out).
Proof. induction fuel; keeps_intros; cbn [expand_spec]; keepst. Qed.
#[export] Hint Resolve kr_expand_spec : keepsdb.

Lemma kr_functor_total : forall fuel g, keeps readonly_state (functor_total fuel g).
Proof. induction fuel; keeps_intros; cbn [functor_total]; keepst. Qed.
#[export] Hint Resolve kr_functor_total : keepsdb.

Lemma kr_provide_spec : forall fuel e fg mi sp outer out, keeps readonly_state (provide_spec fuel e fg mi sp outer out).
Proof. induction fuel; keeps_intros; cbn [provide_spec]; keepst. Qed.
#[export] Hint Resolve kr_provide_spec : keepsdb.

Lemma kr_family_leaves_ok : forall fuel mi ps specs, keeps readonly_state (family_leaves_ok fuel mi ps specs).
Proof. induction fuel; keeps_intros; cbn [family_leaves_ok]; keepst. Qed.
#[export] Hint Resolve kr_family_leaves_ok : keepsdb.

Lemma kr_family_leaves : forall fuel e mi specs outer, keeps readonly_state (family_leaves fuel e mi specs outer).
Proof. induction fuel; keeps_intros; cbn [family_leaves]; keepst. Qed.
#[export] Hint Resolve kr_family_leaves : keepsdb.

Lemma kr_compile_pattern : forall mi e p sty heads, keeps readonly_state (compile_pattern mi e p sty heads).
Proof. intros;unfold compile_pattern;keepst. Qed.
#[export]Hint Resolve kr_compile_pattern:keepsdb.
Lemma kr_payload_type_input:forall t e,keeps readonly_state(payload_type t e).
Proof. intros;unfold payload_type;keepst. Qed.
#[export]Hint Resolve kr_payload_type_input:keepsdb.
Lemma kr_check_relevant_input:forall mi g a,keeps readonly_state(check_relevant mi g a).
Proof. intros;unfold check_relevant;keepst. Qed.
#[export]Hint Resolve kr_check_relevant_input:keepsdb.
Lemma kr_fn_sig_input:forall g tm e this,keeps readonly_state(fn_sig g tm e this).
Proof. intros;unfold fn_sig;keepst. Qed.
#[export]Hint Resolve kr_fn_sig_input:keepsdb.
Lemma kr_ety_of_input:forall t mi tm e this,keeps readonly_state(ety_of t mi tm e this).
Proof. intros;unfold ety_of;keepst. Qed.
#[export]Hint Resolve kr_ety_of_input:keepsdb.

Lemma kr_check_captured_reading : forall mi site prototype formal actual,
 keeps readonly_state(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;keepst. Qed.
#[export] Hint Resolve kr_check_captured_reading : keepsdb.
Lemma kr_fresh_marker : forall e,keeps readonly_state(fresh_marker e).
Proof. intros;unfold fresh_marker;keepst. Qed.
#[export] Hint Resolve kr_fresh_marker : keepsdb.

Lemma kr_check_link_fixed_current : forall mi site fg bg fixed,
 keeps readonly_state(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;keepst. Qed.
#[export] Hint Resolve kr_check_link_fixed_current : keepsdb.

Lemma kr_platform_fun : forall g,keeps readonly_state(platform_fun g).
Proof. intros;unfold platform_fun;keepst. Qed.
#[export] Hint Resolve kr_platform_fun : keepsdb.
Lemma kr_platform_entry : forall k prims,keeps readonly_state(platform_entry k prims).
Proof. intros;unfold platform_entry;keepst. Qed.
#[export] Hint Resolve kr_platform_entry : keepsdb.
Lemma kr_runtime_provider_inputs : forall h,keeps readonly_state(runtime_provider_inputs h).
Proof. intros;unfold runtime_provider_inputs;keepst. Qed.
#[export] Hint Resolve kr_runtime_provider_inputs : keepsdb.
Lemma kr_check_platform_entry_view : forall e k entry,keeps readonly_state(check_platform_entry_view e k entry).
Proof. intros;unfold check_platform_entry_view;keepst. Qed.
#[export] Hint Resolve kr_check_platform_entry_view : keepsdb.

Lemma kr_fresh_marker_for : forall e r,keeps readonly_state(fresh_marker_for e r).
Proof. intros;unfold fresh_marker_for;keepst. Qed.
#[export] Hint Resolve kr_fresh_marker_for : keepsdb.

Lemma kr_check_constant_effects:forall mi x,keeps readonly_state(check_constant_effects mi x).
Proof. intros;unfold check_constant_effects;keepst. Qed.
Lemma kr_check_provider_effects:forall mi bs,keeps readonly_state(check_provider_effects mi bs).
Proof. intros;unfold check_provider_effects;keepst. Qed.
#[export]Hint Resolve kr_check_constant_effects kr_check_provider_effects:keepsdb.

(* Whole-program assumption checks revisit the same requirement helper under
   several declaration kinds. Prove its complete readonly effect once. *)
Lemma kr_check_assumed_requirements:forall mi e k,
 keeps readonly_state(check_assumed_requirements mi e k).
Proof. intros;unfold check_assumed_requirements;keepst. Qed.
#[export]Hint Resolve kr_check_assumed_requirements:keepsdb.
Lemma kr_check_assumes:keeps readonly_state check_assumes.
Proof. unfold check_assumes;keepst. Qed.
#[export]Hint Resolve kr_check_assumes:keepsdb.
