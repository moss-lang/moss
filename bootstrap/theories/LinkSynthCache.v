(* Preservation proofs for the non-recursive queries LinkSynthPreserve's
   mutual traversal calls. Kept apart so they compile in parallel with the
   link-preservation files that traversal also needs (LinkPreserve,
   LinkBindPreserve, LinkHigherPreserve), off its critical path. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv ReifyData
 InfoInvariant InfoPreserve InfoInputs FwdInvariant FwdReadonly FwdPreserve FwdSynthInv
 LinkCacheInvariant LinkReadonly LinkConditional
 ProviderPhysical WrapperKeyReadonly ReadonlyHelpers.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Local Lemma kws_ety_ty : forall t,keeps st_fwd(ety_ty t).
Proof. intro t;unfold ety_ty;apply kw_ety_ty_go. Qed.
Local Lemma kls_ety_ty : forall t,keeps st_links(ety_ty t).
Proof. intro t;unfold ety_ty;apply kl_ety_ty_go. Qed.
#[export] Hint Resolve kws_ety_ty kls_ety_ty:keepsdb.
Local Lemma kws_key_theta : forall e k,keeps st_fwd(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
Local Lemma kls_key_theta : forall e k,keeps st_links(key_theta e k).
Proof. intros;unfold key_theta;keepst. Qed.
#[export] Hint Resolve kws_key_theta kls_key_theta:keepsdb.
Local Lemma kws_key_item : forall e k,keeps st_fwd(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
Local Lemma kls_key_item : forall e k,keeps st_links(key_item_at e k).
Proof. intros;unfold key_item_at;keepst. Qed.
#[export] Hint Resolve kws_key_item kls_key_item:keepsdb.
Local Lemma okL_readonly : forall A(m:M A),keeps readonly_state m->NI m->keeps st_links m->ok m->okL m.
Proof.
 intros A m Read Info Links Valid;apply okL_keeps.
 - apply okI_ni;[exact Valid|exact Info|].
   eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst x)));exact Read.
 - apply okF_readonly;[exact Valid|].
   eapply keeps_projection with(p:=readonly_state)(q:=fun x=>snd(fst(fst(fst x))));exact Read.
 - exact Links.
Qed.
Ltac synth_link_quiet := first[solve[apply okL_readonly;[auto with keepsdb|auto with nidb|auto with keepsdb|auto with okdb]]|apply okL_keeps;
 [first[solve[auto with okIdb]|apply okI_ni;[solve[okt]|solve[nit]|solve[keepst]]]|
  first[solve[auto with fwdb]|apply okF_readonly;[solve[okt]|solve[keepst]]]|solve[keepst]]].
Local Lemma cached_link_get : okL get.
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_show : forall t,okL(show t).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_show_key : forall k,okL(show_key k).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fresh : okL fresh.
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_local_var : forall name,okL(local_var name).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fld_id : forall name,okL(fld_id name).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_unsupported : forall why,okL(unsupported why).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fn_info : forall g,okL(fn_info g).
Proof. intros;synth_link_quiet. Qed.
(* Share preservation proofs for repeated diagnostic and provider queries.
   Re-running readonly traversals at every call site dominates this mutual
   proof; the cached lemmas share those proofs without changing obligations. *)
Local Lemma cached_link_err : forall A mi why,okL(@err A mi why).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_require_needs : forall mi e g info,okL(require_needs mi e g info).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_family_member : forall mi e r m,okL(family_member mi e r m).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_expect_provider : forall c e y this,okL(expect_provider c e y this).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_compile_pattern : forall x0 x1 x2 x3 x4,okL(compile_pattern x0 x1 x2 x3 x4).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_inject_check : forall x0 x1 x2,okL(inject_check x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_lang_symbol : forall x0 x1 x2 x3 x4,okL(lang_symbol x0 x1 x2 x3 x4).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_smap_at : forall x0 x1 x2,okL(smap_at x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_expect_defined_fn : forall x0 x1,okL(expect_defined_fn x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_join : forall x0 x1 x2 x3 x4,okL(join x0 x1 x2 x3 x4).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_provided_at : forall x0 x1 x2 x3 x4,okL(provided_at x0 x1 x2 x3 x4).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fun_env : forall x0,okL(fun_env x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_layout_ok : forall x0 x1,okL(layout_ok x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_nominal_args : forall x0 x1 x2,okL(nominal_args x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fail : forall x0 x1,okL(@fail x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_check_relevant : forall x0 x1 x2,okL(check_relevant x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_method_key : forall x0 x1 x2 x3,okL(method_key x0 x1 x2 x3).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_bool_type : forall x0,okL(bool_type x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_resolve_type_symbol : forall x0 x1,okL(resolve_type_symbol x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_fn_sig : forall x0 x1 x2 x3,okL(fn_sig x0 x1 x2 x3).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_resolve_sym : forall x0 x1,okL(resolve_sym x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_call_theta : forall x0 x1,okL(call_theta x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_memory_sym : okL(memory_sym ).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_payload_type : forall x0 x1,okL(payload_type x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_rpath : forall x0 x1,okL(rpath x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_ety_of : forall x0 x1 x2 x3 x4,okL(ety_of x0 x1 x2 x3 x4).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_record_reify_event : forall x0,okL(record_reify_event x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_key_item_at : forall x0 x1,okL(key_item_at x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_find_method_decl : forall x0 x1 x2,okL(find_method_decl x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_provide_result : forall x0 x1 x2,okL(provide_result x0 x1 x2).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_sym : forall x0,okL(sym x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_the_mod : forall x0,okL(the_mod x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_tag_theta : forall x0 x1,okL(tag_theta x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_replay_reify_constant : forall x0,okL(replay_reify_constant x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_key_item : forall x0,okL(key_item x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_is_memories : forall x0,okL(is_memories x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_result_types : forall x0,okL(result_types x0).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_rdetached : forall x0 x1,okL(rdetached x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_emit_fn : forall x0 x1,okL(emit_fn x0 x1).
Proof. intros;synth_link_quiet. Qed.
Local Lemma cached_link_is_memory_instruction : forall x0,okL(is_memory_instruction x0).
Proof. intros;synth_link_quiet. Qed.
Ltac cached_link_quiet := lazymatch goal with|-okL ?m=>let h:=head_of m in lazymatch h with
| get => apply cached_link_get
| show => apply cached_link_show
| show_key => apply cached_link_show_key
| fresh => apply cached_link_fresh
| local_var => apply cached_link_local_var
| fld_id => apply cached_link_fld_id
| unsupported => apply cached_link_unsupported
| fn_info => apply cached_link_fn_info
| @err => apply cached_link_err
| require_needs => apply cached_link_require_needs
| family_member => apply cached_link_family_member
| expect_provider => apply cached_link_expect_provider
| compile_pattern => apply cached_link_compile_pattern
| inject_check => apply cached_link_inject_check
| lang_symbol => apply cached_link_lang_symbol
| smap_at => apply cached_link_smap_at
| expect_defined_fn => apply cached_link_expect_defined_fn
| join => apply cached_link_join
| provided_at => apply cached_link_provided_at
| fun_env => apply cached_link_fun_env
| layout_ok => apply cached_link_layout_ok
| nominal_args => apply cached_link_nominal_args
| @fail => apply cached_link_fail
| check_relevant => apply cached_link_check_relevant
| method_key => apply cached_link_method_key
| bool_type => apply cached_link_bool_type
| resolve_type_symbol => apply cached_link_resolve_type_symbol
| fn_sig => apply cached_link_fn_sig
| resolve_sym => apply cached_link_resolve_sym
| call_theta => apply cached_link_call_theta
| memory_sym => apply cached_link_memory_sym
| payload_type => apply cached_link_payload_type
| rpath => apply cached_link_rpath
| ety_of => apply cached_link_ety_of
| record_reify_event => apply cached_link_record_reify_event
| key_item_at => apply cached_link_key_item_at
| find_method_decl => apply cached_link_find_method_decl
| provide_result => apply cached_link_provide_result
| sym => apply cached_link_sym
| the_mod => apply cached_link_the_mod
| tag_theta => apply cached_link_tag_theta
| replay_reify_constant => apply cached_link_replay_reify_constant
| key_item => apply cached_link_key_item
| is_memories => apply cached_link_is_memories
| result_types => apply cached_link_result_types
| rdetached => apply cached_link_rdetached
| emit_fn => apply cached_link_emit_fn
| is_memory_instruction => apply cached_link_is_memory_instruction
end end.
