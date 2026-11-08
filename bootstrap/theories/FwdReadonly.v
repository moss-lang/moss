(* Non-emitting type, context and signature helpers preserve the complete
   visible function table, including temporary recursion markers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabKeeps.
From NM Require Import ReadonlyHelpers.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL ELAB_FUEL].
Lemma kw_mentioned_nominals : forall fuel t mi, keeps st_fwd (mentioned_nominals fuel t mi).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_mentioned_nominals : keepsdb.

Lemma kw_walk_specs : forall fuel mi sp out, keeps st_fwd (walk_specs fuel mi sp out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_walk_specs : keepsdb.

Lemma kw_free_type_syms_group : forall fuel,
  (forall t mi out, keeps st_fwd (free_type_syms fuel t mi out)) /\
  (forall s, keeps st_fwd (type_requirements fuel s)) /\
  (forall root, keeps st_fwd (solve_requirements fuel root)) /\
  (forall g, keeps st_fwd (listed_params fuel g)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.

Lemma kw_free_type_syms : forall fuel t mi out, keeps st_fwd (free_type_syms fuel t mi out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_type_requirements : forall fuel s, keeps st_fwd (type_requirements fuel s).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_solve_requirements : forall fuel root, keeps st_fwd (solve_requirements fuel root).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_listed_params : forall fuel g, keeps st_fwd (listed_params fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_free_type_syms kw_type_requirements kw_solve_requirements kw_listed_params : keepsdb.

Lemma kw_reqs_frame : forall x, keeps st_fwd (reqs x).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_reqs_frame : keepsdb.
Local Strategy opaque [reqs type_requirements].

Lemma kw_show_go : forall fuel t, keeps st_fwd (show_go fuel t).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_show_go : keepsdb.

Lemma kw_unify : forall fuel e mi a b why, keeps st_fwd (unify fuel e mi a b why).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_unify : keepsdb.

Lemma kw_elab_type_group : forall fuel,
  (forall t mi tm e this, keeps st_fwd (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, keeps st_fwd (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, keeps st_fwd (symbol_type fuel mi g tm e)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.

Lemma kw_elab_type : forall fuel t mi tm e this, keeps st_fwd (elab_type fuel t mi tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_app_subst : forall fuel e mi bs outer, keeps st_fwd (app_subst fuel e mi bs outer).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_symbol_type : forall fuel mi g tm e, keeps st_fwd (symbol_type fuel mi g tm e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_elab_type kw_app_subst kw_symbol_type : keepsdb.

Lemma kw_result_mentions_go_group : forall fuel,
  (forall self mi specs out, keeps st_fwd (result_mentions_go fuel self mi specs out)) /\
  (forall g, keeps st_fwd (functor_fixed_go fuel g)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.

Lemma kw_result_mentions_go : forall fuel self mi specs out, keeps st_fwd (result_mentions_go fuel self mi specs out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_functor_fixed_go : forall fuel g, keeps st_fwd (functor_fixed_go fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_result_mentions_go kw_functor_fixed_go : keepsdb.

Lemma kw_check_literal_sig : forall e mi m oty args sg,
  keeps st_fwd (check_literal_sig e mi m oty args sg).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_check_literal_sig : keepsdb.

Lemma kw_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, keeps st_fwd (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, keeps st_fwd (add_spec fuel e mi sp outer needs)).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.

Lemma kw_add_symbol_item : forall fuel e mi g subst needs, keeps st_fwd (add_symbol_item fuel e mi g subst needs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_add_spec : forall fuel e mi sp outer needs, keeps st_fwd (add_spec fuel e mi sp outer needs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_add_symbol_item kw_add_spec : keepsdb.

Lemma kw_merged_walk_frame : forall mi walk,
 (forall e, keeps st_fwd (walk e)) -> keeps st_fwd (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. keepst. Qed.
#[export] Hint Resolve kw_merged_walk_frame : keepsdb.
Local Strategy opaque [merged_walk].
Lemma kw_fun_env_input:forall g,keeps st_fwd(fun_env g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_fun_env_input:keepsdb.
Local Strategy opaque [fun_env].
Lemma kw_fn_info_frame : forall g, keeps st_fwd (fn_info g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_fn_info_frame : keepsdb.
Local Strategy opaque [fn_info].

Lemma kw_ety_ty_go : forall fuel t, keeps st_fwd (ety_ty_go fuel t).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_ety_ty_go : keepsdb.

Lemma kw_slots_go : forall fuel e t visiting, keeps st_fwd (slots_go fuel e t visiting).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_slots_go : keepsdb.
Local Strategy opaque [slots_go].

Lemma kw_expand_spec : forall fuel mi sp out, keeps st_fwd (expand_spec fuel mi sp out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_expand_spec : keepsdb.

Lemma kw_functor_total : forall fuel g, keeps st_fwd (functor_total fuel g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_functor_total : keepsdb.

Lemma kw_provide_spec : forall fuel e fg mi sp outer out, keeps st_fwd (provide_spec fuel e fg mi sp outer out).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_provide_spec : keepsdb.

Lemma kw_family_leaves_ok : forall fuel mi ps specs, keeps st_fwd (family_leaves_ok fuel mi ps specs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_family_leaves_ok : keepsdb.

Lemma kw_family_leaves : forall fuel e mi specs outer, keeps st_fwd (family_leaves fuel e mi specs outer).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_family_leaves : keepsdb.

Lemma kw_compile_pattern : forall mi e p sty heads, keeps st_fwd (compile_pattern mi e p sty heads).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_compile_pattern:keepsdb.
Lemma kw_payload_type_input:forall t e,keeps st_fwd(payload_type t e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_payload_type_input:keepsdb.
Lemma kw_check_relevant_input:forall mi g a,keeps st_fwd(check_relevant mi g a).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_check_relevant_input:keepsdb.
Lemma kw_fn_sig_input:forall g tm e this,keeps st_fwd(fn_sig g tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_fn_sig_input:keepsdb.
Lemma kw_ety_of_input:forall t mi tm e this,keeps st_fwd(ety_of t mi tm e this).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_ety_of_input:keepsdb.

Lemma kw_check_captured_reading : forall mi site prototype formal actual,
 keeps st_fwd(check_captured_reading mi site prototype formal actual).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_check_captured_reading : keepsdb.
Lemma kw_fresh_marker : forall e,keeps st_fwd(fresh_marker e).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_fresh_marker : keepsdb.

Lemma kw_check_link_fixed_current : forall mi site fg bg fixed,
 keeps st_fwd(check_link_fixed_current mi site fg bg fixed).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_check_link_fixed_current : keepsdb.

Lemma kw_platform_fun : forall g,keeps st_fwd(platform_fun g).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_platform_fun : keepsdb.
Lemma kw_platform_entry : forall k prims,keeps st_fwd(platform_entry k prims).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_platform_entry : keepsdb.
Lemma kw_runtime_provider_inputs : forall h,keeps st_fwd(runtime_provider_inputs h).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_runtime_provider_inputs : keepsdb.
Lemma kw_check_platform_entry_view : forall e k entry,keeps st_fwd(check_platform_entry_view e k entry).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_check_platform_entry_view : keepsdb.

Lemma kw_fresh_marker_for : forall e r,keeps st_fwd(fresh_marker_for e r).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export] Hint Resolve kw_fresh_marker_for : keepsdb.

Lemma kw_check_constant_effects:forall mi x,keeps st_fwd(check_constant_effects mi x).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
Lemma kw_check_provider_effects:forall mi bs,keeps st_fwd(check_provider_effects mi bs).
Proof. intros; repeat split; intros; eapply keeps_projection with (p := readonly_state) (q := fun x => snd (fst (fst (fst x)))); auto with keepsdb. Qed.
#[export]Hint Resolve kw_check_constant_effects kw_check_provider_effects:keepsdb.
