(* Type, context and signature helpers preserve the eta-wrapper cache. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabKeeps.
From NM Require Import ReadonlyHelpers.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma kwr_mentioned_nominals : forall fuel t mi, keeps st_wrappers (mentioned_nominals fuel t mi).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_mentioned_nominals : keepsdb.

Lemma kwr_walk_specs : forall fuel mi sp out, keeps st_wrappers (walk_specs fuel mi sp out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_walk_specs : keepsdb.

Lemma kwr_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_wrappers (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_wrappers (type_requirements fuel s)) /\
  (forall root, keeps st_wrappers (solve_requirements fuel root)) /\
  (forall g, keeps st_wrappers (listed_params fuel g)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.

Lemma kwr_free_type_syms : forall fuel t mi out, keeps st_wrappers (free_type_syms fuel t mi out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_type_requirements : forall fuel s, keeps st_wrappers (type_requirements fuel s).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_solve_requirements : forall fuel root, keeps st_wrappers (solve_requirements fuel root).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_listed_params : forall fuel g, keeps st_wrappers (listed_params fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_free_type_syms kwr_type_requirements kwr_solve_requirements kwr_listed_params : keepsdb.

Lemma kwr_reqs_frame : forall x, keeps st_wrappers (reqs x).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kwr_show_go : forall fuel t, keeps st_wrappers (show_go fuel t).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_show_go : keepsdb.

Lemma kwr_unify : forall fuel e mi a b why, keeps st_wrappers (unify fuel e mi a b why).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_unify : keepsdb.

Lemma kwr_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_wrappers (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_wrappers (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_wrappers (symbol_type fuel mi g tm e)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.

Lemma kwr_elab_type : forall fuel t mi tm e this, keeps st_wrappers (elab_type fuel t mi tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_app_subst : forall fuel e mi bs outer, keeps st_wrappers (app_subst fuel e mi bs outer).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_symbol_type : forall fuel mi g tm e, keeps st_wrappers (symbol_type fuel mi g tm e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_elab_type kwr_app_subst kwr_symbol_type : keepsdb.

Lemma kwr_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_wrappers (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_wrappers (functor_fixed_go fuel g)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.

Lemma kwr_result_mentions_go : forall fuel self mi specs out, keeps st_wrappers (result_mentions_go fuel self mi specs out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_functor_fixed_go : forall fuel g, keeps st_wrappers (functor_fixed_go fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_result_mentions_go kwr_functor_fixed_go : keepsdb.

Lemma kwr_check_literal_sig : forall e mi m oty args sg,
  keeps st_wrappers (check_literal_sig e mi m oty args sg).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_check_literal_sig : keepsdb.

Lemma kwr_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_wrappers (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_wrappers (add_spec fuel e mi sp outer needs)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.

Lemma kwr_add_symbol_item : forall fuel e mi g subst needs, keeps st_wrappers (add_symbol_item fuel e mi g subst needs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_add_spec : forall fuel e mi sp outer needs, keeps st_wrappers (add_spec fuel e mi sp outer needs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_add_symbol_item kwr_add_spec : keepsdb.

Lemma kwr_merged_walk_frame : forall mi walk,
 (forall e, keeps st_wrappers (walk e)) -> keeps st_wrappers (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kwr_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kwr_fun_env_input:forall g,keeps st_wrappers(fun_env g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kwr_fn_info_frame : forall g, keeps st_wrappers (fn_info g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kwr_ety_ty_go : forall fuel t, keeps st_wrappers (ety_ty_go fuel t).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_ety_ty_go : keepsdb.

Lemma kwr_slots_go : forall fuel e t visiting, keeps st_wrappers (slots_go fuel e t visiting).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kwr_expand_spec : forall fuel mi sp out, keeps st_wrappers (expand_spec fuel mi sp out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_expand_spec : keepsdb.

Lemma kwr_functor_total : forall fuel g, keeps st_wrappers (functor_total fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_functor_total : keepsdb.

Lemma kwr_provide_spec : forall fuel e fg mi sp outer out, keeps st_wrappers (provide_spec fuel e fg mi sp outer out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_provide_spec : keepsdb.

Lemma kwr_family_leaves_ok : forall fuel mi ps specs, keeps st_wrappers (family_leaves_ok fuel mi ps specs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_family_leaves_ok : keepsdb.

Lemma kwr_family_leaves : forall fuel e mi specs outer, keeps st_wrappers (family_leaves fuel e mi specs outer).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_family_leaves : keepsdb.

Lemma kwr_compile_pattern : forall mi e p sty heads, keeps st_wrappers (compile_pattern mi e p sty heads).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_compile_pattern:keepsdb.
Lemma kwr_payload_type_input:forall t e,keeps st_wrappers(payload_type t e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_payload_type_input:keepsdb.
Lemma kwr_check_relevant_input:forall mi g a,keeps st_wrappers(check_relevant mi g a).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_check_relevant_input:keepsdb.
Lemma kwr_fn_sig_input:forall g tm e this,keeps st_wrappers(fn_sig g tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_fn_sig_input:keepsdb.
Lemma kwr_ety_of_input:forall t mi tm e this,keeps st_wrappers(ety_of t mi tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_ety_of_input:keepsdb.

Lemma kwr_check_captured_reading : forall mi site prototype formal actual,
 keeps st_wrappers(check_captured_reading mi site prototype formal actual).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_check_captured_reading : keepsdb.
Lemma kwr_fresh_marker : forall e,keeps st_wrappers(fresh_marker e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_fresh_marker : keepsdb.

Lemma kwr_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_wrappers(check_link_fixed_current mi site fg bg fixed).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_check_link_fixed_current : keepsdb.

Lemma kwr_platform_fun : forall g,keeps st_wrappers(platform_fun g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_platform_fun : keepsdb.
Lemma kwr_platform_entry : forall k prims,keeps st_wrappers(platform_entry k prims).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_platform_entry : keepsdb.
Lemma kwr_runtime_provider_inputs : forall h,keeps st_wrappers(runtime_provider_inputs h).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_runtime_provider_inputs : keepsdb.
Lemma kwr_check_platform_entry_view : forall e k entry,keeps st_wrappers(check_platform_entry_view e k entry).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_check_platform_entry_view : keepsdb.

Lemma kwr_fresh_marker_for : forall e r,keeps st_wrappers(fresh_marker_for e r).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export] Hint Resolve kwr_fresh_marker_for : keepsdb.

Lemma kwr_check_constant_effects:forall mi x,keeps st_wrappers(check_constant_effects mi x).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
Lemma kwr_check_provider_effects:forall mi bs,keeps st_wrappers(check_provider_effects mi bs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst x))); auto with keepsdb. Qed.
#[export]Hint Resolve kwr_check_constant_effects kwr_check_provider_effects:keepsdb.
