(* The checked component contracts, supporting proof milestones, and
   the executable roots run/Extract.v extracts. Their
   assumptions are transitively everything the guarantee rests on, and
   check.sh requires them to be only the kernel's `Primitive` declarations
   (Rocq's primitive integers, floats and arrays): no Axiom, no Admitted. *)
From NM Require Parse EaCtx EaMain EaWorldSound MossX.

From NM Require AuditExamples AuditBuildExample
  SourceSnapshot PublicEmit PublicClosureExample PublicCaptureFacts
  SnapshotLoaderBound PublicWorld PublicTypeFacts PublicValueTyping PublicCastTyping
  PublicSavedTypeExample PublicHeapTyping PublicWorldScope SnapshotLoader
  PublicLocalsTyping PublicWorldRequirements SnapshotLoaderWitness
  PublicGenerationFacts PublicAliasContext PublicGenerationCallExample PublicOutputTransferExample
  PublicGenerationPrototype PublicCaptureScopeFacts PublicFunctorScopeExample PublicContextFacts PublicSafety
  PublicTypeRead PublicBoolTyping PublicBodySafety PublicCallable PublicBodySlice PublicArms PublicTagCheck PublicCallRealize
  PublicEntry PublicPrimitiveSafety PublicPrimitiveSites PublicFormation PublicLinkMerges PublicWorldFormation PublicFamilySig PublicCopyApply
  PublicPlatformApply PublicFamilyStep PublicLinkResults PublicOutputSubst PublicFamilyAgree PublicChainAgree
  PublicChainCore PublicChainNew PublicChainFwd PublicScoping PublicCallerKeys PublicFamilyCoherent PublicCopyFresh PublicLinkWalk PublicLinkScope PublicFamilyLink
  PublicLinkPins PublicLinkRun PublicChainSigs PublicChainRealize PublicFamilyCopy PublicConsol PublicOrientation PublicEquation PublicSameNeed
  PublicMergeClass PublicMerge PublicMergePresent PublicMergeOps EagerCopy ElabWorld ElabWorldExample ProgramPrune ProgramFastPath ProgramCapTrim InterpreterLazy InterpreterFast InterpreterCompiled.

Module PublicPublic := Audit.Semantics.Public AuditExamples.Host.
Module PublicHeaps := PublicHeapTyping.HeapSafety AuditExamples.Host PublicPublic.
Module PublicInterp := Audit.Semantics.Interpreter PublicClosureExample.Prims.
Module PublicLinkMergesI := PublicLinkMerges.LinkMerges PublicClosureExample.Prims PublicInterp.
(* public_safety's instance. Every functor of the body-safety chain is
   instantiated inside it exactly once (PublicFamilyCopy contains
   PublicChainRealize, which contains PublicChainSigs, and so on down to
   PublicBodySlice), so the roots below are taken from that one instance
   by full path: a separate instance of each would copy the chain again,
   and Print Assumptions would traverse every copy; and even a top-level
   `Module X := PublicEntryTop.FCp` is a copy of that submodule, not an
   alias. Paths: FCp PublicFamilyCopy, CRz PublicChainRealize, CSg
   PublicChainSigs, FL PublicFamilyLink (LRn PublicLinkRun), CF
   PublicChainFwd, CN PublicChainNew, CC PublicChainCore, LR
   PublicLinkResults, PA PublicPlatformApply, FS PublicFamilyStep, CopyA
   PublicCopyApply, S PublicBodySlice (AR PublicArms, B PublicBoolTyping,
   TC PublicTagCheck, TC.CT PublicCastTyping); SR PublicSafety, PS
   PublicPrimitiveSafety. *)
Module PublicEntryTop := PublicEntry.EntrySafety AuditExamples.Host PublicPublic.
Module PublicSiteErasure := PublicPrimitiveSites.ReferenceSites AuditExamples.Host PublicPublic.

(* public_safety is CORRECT.safety, the statement Audit.v gives, for the
   same Public instance: the statement below is read off CORRECT's
   `safety` (abstracted over its `prelude`), not written out again, so
   changing public_safety's statement alone, or Audit.v's alone, fails
   here. *)
Module SafetyIsCorrect (C : PublicPublic.CORRECT).
Definition statement : Prop.
Proof.
  let t := type of C.safety in
  lazymatch eval pattern C.prelude in t with ?f _ => exact (forall prelude, f prelude) end.
Defined.
Goal True.
  let s := eval cbv beta delta [statement] in statement in
  let t := type of PublicEntryTop.public_safety in
  constr_eq s t; exact I.
Qed.
Definition public_safety_is_safety : statement := PublicEntryTop.public_safety.
End SafetyIsCorrect.

(* Read-set pruning of Audit programs (ProgramPrune.v) keeps behaviour:
   with public_safety, an admissible program on a compatible interface
   whose primitives ignore the program's code has, at every fuel, the same
   observation as its pruned program, so the same Runs. The default
   interface of this instance's host ignores the view entirely. *)
Module PruneTop := ProgramPrune.PruneRuns AuditExamples.Host PublicPublic.
Definition pruned_observation_agrees : forall prelude program interface initial,
  PublicPublic.Admissible prelude program -> PublicPublic.Compatible program interface ->
  PruneTop.code_blind interface -> forall fuel,
  PublicPublic.observe (PublicPublic.Reference.run_main fuel
    (PublicPublic.runtime (ProgramPrune.prune program) initial)
    (Audit.Semantics.program_main (ProgramPrune.prune program))
    (Audit.Semantics.program_environment (ProgramPrune.prune program)) (interface, initial))
  = PublicPublic.observe (PublicPublic.Reference.run_main fuel
    (PublicPublic.runtime program initial) (Audit.Semantics.program_main program)
    (Audit.Semantics.program_environment program) (interface, initial)) :=
  fun prelude program interface initial Ha Hc Hb =>
    @PruneTop.observe_pruned program interface initial Hb
      (PublicEntryTop.public_safety prelude program interface initial Ha Hc).
Definition pruned_runs_agree : forall prelude program interface initial result,
  PublicPublic.Admissible prelude program -> PublicPublic.Compatible program interface ->
  PruneTop.code_blind interface ->
  PublicPublic.Runs (ProgramPrune.prune program) interface initial result
  <-> PublicPublic.Runs program interface initial result :=
  fun prelude program interface initial result Ha Hc Hb =>
    @PruneTop.runs_pruned program interface initial result Hb
      (PublicEntryTop.public_safety prelude program interface initial Ha Hc).
Definition default_interface_code_blind : PruneTop.code_blind PublicPublic.default_interface :=
  fun _ _ _ _ _ _ _ _ _ _ _ => eq_refl.

(* The platform-family fast path for Audit programs (ProgramFastPath.v):
   each application of a primitive-only platform family becomes the
   primitive bindings its continuation reads. The transformed run is
   eventually equal to the original at every fuel where the original is
   neither stuck nor out of fuel, and conversely; with public_safety (no
   Wrong), the same Runs, for interfaces whose primitives ignore the
   program's code. Composed with read-set pruning, as moss-ref runs them. *)
Module FastTop := ProgramFastPath.FastRuns AuditExamples.Host PublicPublic.
Definition fastpath_runs_agree : forall prelude program interface initial result,
  PublicPublic.Admissible prelude program -> PublicPublic.Compatible program interface ->
  PruneTop.code_blind interface ->
  PublicPublic.Runs (ProgramFastPath.fastpath program) interface initial result
  <-> PublicPublic.Runs program interface initial result :=
  fun prelude program interface initial result Ha Hc Hb =>
    @FastTop.runs_fastpath program interface initial result Hb
      (PublicEntryTop.public_safety prelude program interface initial Ha Hc).
Lemma fastpath_prune_runs_agree : forall prelude program interface initial result,
  PublicPublic.Admissible prelude program -> PublicPublic.Compatible program interface ->
  PruneTop.code_blind interface ->
  PublicPublic.Runs (ProgramPrune.prune (ProgramFastPath.fastpath program)) interface initial result
  <-> PublicPublic.Runs program interface initial result.
Proof.
  intros prelude program interface initial result Ha Hc Hb.
  pose proof (PublicEntryTop.public_safety prelude program interface initial Ha Hc) as Hs.
  rewrite (@PruneTop.runs_pruned (ProgramFastPath.fastpath program) interface initial result Hb).
  - exact (@FastTop.runs_fastpath program interface initial result Hb Hs).
  - intros code final Hr. apply (Hs code final).
    apply (proj1 (@FastTop.runs_fastpath program interface initial _ Hb Hs)). exact Hr.
Qed.

(* Capture trimming for Audit programs (ProgramCapTrim.v): each fn and
   method closure's type capture cut to what its code grounds. The trimmed
   run is the original run at every fuel (no safety premise), for
   interfaces whose primitives ignore the program's code. Composed after
   the fast path and pruning, as moss-ref runs them (run/driver.ml). *)
Module CapTrimTop := ProgramCapTrim.TrimRuns AuditExamples.Host PublicPublic.
Definition captrim_runs_agree : forall program interface initial result,
  PruneTop.code_blind interface ->
  PublicPublic.Runs (ProgramCapTrim.captrim program) interface initial result
  <-> PublicPublic.Runs program interface initial result :=
  fun program interface initial result Hb => @CapTrimTop.runs_trimmed program interface initial result Hb.
Lemma fastpath_prune_captrim_runs_agree : forall prelude program interface initial result,
  PublicPublic.Admissible prelude program -> PublicPublic.Compatible program interface ->
  PruneTop.code_blind interface ->
  PublicPublic.Runs (ProgramCapTrim.captrim (ProgramPrune.prune (ProgramFastPath.fastpath program)))
    interface initial result
  <-> PublicPublic.Runs program interface initial result.
Proof.
  intros prelude program interface initial result Ha Hc Hb.
  rewrite (captrim_runs_agree _ _ _ _ Hb).
  exact (fastpath_prune_runs_agree prelude program interface initial result Ha Hc Hb).
Qed.

(* Deferred type substitution (InterpreterLazy.v): moss-ref runs the reference
   interpreter as InterpreterLazy.LazyInterpreter, which returns exactly what the
   reference returns, at every fuel, for every primitive implementation
   (run/driver.ml), here at the public reference host. *)
Module LazyTop := InterpreterLazy.LazyInterpreter PublicPublic.ReferenceHost.
(* Trie-keyed provisions on top (InterpreterFast.v): equal to the lazy
   interpreter on related environments, so to the reference. *)
Module FastTopI := InterpreterFast.FastInterpreter PublicPublic.ReferenceHost.
(* Compiled to closures on top (InterpreterCompiled.v): under tbl_ok, equal to the
   fast interpreter, so to the reference. *)
Module CloTop := InterpreterCompiled.CloInterpreter PublicPublic.ReferenceHost.

(* One dependency traversal computes the union for every root below. The
   roots are a list, not a tuple: a nested pair repeats the types of all
   the roots before it, so a tuple of n roots has a term quadratic in n.
   Closed.v separately checks the roots that must not assume primitives. *)
Import Stdlib.Lists.List.ListNotations.
Definition root {T : Type} (x : T) : {T : Type & T} := existT (fun T => T) T x.
Definition certificate_roots : list {T : Type & T} := [
  root (@NM.PublicMerge.merge_eq_substitute);
  root (@NM.PublicMerge.respects_section);
  root (@NM.PublicMerge.merge_eq_sound);
  root (@NM.PublicMerge.merge_section_kernel);
  root (@NM.PublicMerge.merge_semantic_reading_with_section);
  root (@NM.PublicMerge.section_is_most_general);
  root (@NM.PublicMerge.merge_has_mgm);
  root (@NM.PublicMerge.expand_mergeeq);
  root (@NM.PublicMerge.solved_has_section);
  root (@NM.PublicMerge.merge_semantic_reading);
  root (@NM.PublicMerge.merge_presentation_independent);
  root (@NM.PublicMerge.merge_eqb_spec);
  root (@NM.PublicMerge.expand_outer);
  root (@NM.PublicMerge.expand_children);
  root (@NM.PublicMerge.item_eqbM_spec);
  root (@NM.PublicMerge.ilookM_key);
  root (@NM.PublicMergePresent.mgm_structured_image_reads_shape);
  root (@NM.PublicMergePresent.mgm_has_merge);
  root (@NM.PublicMergePresent.present_merge_of);
  root (@NM.EaCtx.merge_presentation);
  root (@NM.PublicMergePresent.canon_iff_mergeeq);
  root (@NM.PublicMergePresent.merge_same_swap);
  root (@NM.PublicMergeOps.merge_read_proper);
  root (@NM.PublicMergeOps.key_eq_proper);
  root (@NM.PublicMergeOps.sig_eq_proper);
  root (@NM.PublicMergeOps.ilookM_first);
  root (@NM.PublicMergeOps.ilookM_proper);
  root (@NM.PublicMergeOps.mlookM_proper);
  root (@NM.PublicMergeOps.providesM_proper);
  root (@NM.PublicMergeOps.fitsM_proper);
  root (@NM.PublicMergeOps.headM_proper);
  root (@NM.PublicMergeOps.joinM_deterministic);
  root (@NM.PublicMergeOps.tyof_merge_agree);
  root (@NM.PublicMergeOps.sigof_merge_agree);
 root (@NM.AuditExamples.bare_return_requires_unit);
  root (@NM.AuditExamples.one_does_not_insert_a_multiply);
  root (@NM.AuditExamples.an_arbitrary_cast_entry_is_rejected);
  root (@NM.AuditExamples.a_unit_platform_constant_is_rejected);
  root (@NM.AuditBuildExample.public_build_is_inhabited);
  root (@NM.AuditBuildExample.built_program_runs);
  root (@NM.SourceSnapshot.source_read_iff);
  root (@NM.SourceSnapshot.source_from_files_sound);
  root (@NM.SourceSnapshot.source_from_files_complete);
  root (@NM.SourceSnapshot.source_read_finite);
  root (@NM.SourceSnapshot.source_read_canonical);
  root (@NM.SnapshotLoaderBound.snapshot_fuel_stable);
  root (@NM.SnapshotLoaderBound.snapshot_success_at_bound);
  root (@NM.SnapshotLoaderBound.load_snapshot_success_adequate);
  root (@NM.SnapshotLoader.load_sound);
  root (@NM.SnapshotLoader.load_complete);
  root (@NM.SnapshotLoader.walk_stable);
  root (@NM.SnapshotLoaderWitness.bundled_reachable_agreement);
  root (@NM.SnapshotLoaderWitness.load_elaborates_complete);
  root (@NM.PublicWorld.load_world_declarations);
  root (@NM.PublicWorld.load_world_declaration_complete);
  root (@NM.PublicWorld.load_world_resolves);
  root (@NM.PublicWorldScope.load_world_world_of_from_requirements);
  root (@NM.ElabWorld.bundle_world_sound);
  root (@NM.EaWorldSound.elaborate_with_world_sound);
  root (@NM.ElabWorld.bundle_world_with_sound);
  root (@NM.ElabWorldExample.example_world);
  root (@NM.ElabWorldExample.unlisted_rejected);
  root (@NM.PublicTypeFacts.ty_eqb_spec);
  root (@NM.PublicValueTyping.typing_extends);
  root (@NM.PublicValueTyping.field_lookup_typed);
  root (@NM.PublicValueTyping.checked_header_typed);
  root (@NM.PublicValueTyping.tagged_payload_typed);
  root (@NM.PublicLocalsTyping.locals_lookup);
  root (@NM.PublicLocalsTyping.locals_assign);
  root (@NM.PublicLocalsTyping.locals_extends);
  root (@NM.PublicGenerationFacts.bind_type_preserves_ground);
  root (@NM.PublicGenerationFacts.result_environment_preserves_generation);
  root (@NM.PublicGenerationFacts.result_environment_preserves_supported_ground);
  root (@NM.PublicGenerationFacts.result_environment_exports_one_snapshot);
  root (@NM.PublicAliasContext.public_type_bind_has_alias_context);
  root (@NM.PublicContextFacts.function_context_merge);
  root (@NM.PublicContextFacts.functor_context_merge);
  root (@NM.PublicContextFacts.assumed_context_merge);
  root (@NM.PublicContextFacts.context_canon_iff_mergeeq);
  root (@NM.PublicContextFacts.reads_merge_canon_iff);
  root (@NM.PublicContextFacts.reads_empty_merge);
  root (@NM.PublicContextFacts.call_theta_respects);
  root (@NM.PublicContextFacts.function_context_canonical);
  root (@NM.PublicContextFacts.functor_context_canonical);
  root (@NM.PublicContextFacts.formed_type_binding_preserves_alias_context);
  root (@NM.PublicContextFacts.function_context_unique);
  root (@NM.PublicContextFacts.functor_context_unique);
  root (@NM.PublicContextFacts.call_theta_merged);
  root (@NM.PublicScoping.methread_merge_iff);
  root (@NM.PublicScoping.methsread_canon_iff);
  root (@NM.PublicScoping.methsread_mlook_canonical);
  root (@NM.PublicRuntimeKeys.normalized_rkey_proper);
  root (@NM.PublicRuntimeKeys.normalized_rkey_stored);
  root (@NM.PublicRuntimeKeys.normalized_rkey_eq_iff);
  root (@NM.PublicRuntimeKeys.runtime_lookup_proper);
  root (@NM.PublicRuntimeKeys.keys_match_with_normalized);
  root (@NM.PublicRuntimeKeys.keys_match_with_normalized_iff);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.envok_runtime_iff);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.envok_runtime_method);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_runtime_iff);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_with_method_keyeq);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.pre_method_provision);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.merge_link_site);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_with_link_site);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.envok_with_of_on);
  root (@NM.PublicCaptureScopeFacts.typed_capture_generation_lookup);
  root (@NM.PublicCaptureScopeFacts.typed_capture_alias_lookup);
  root (@NM.PublicGenerationCallExample.actual_application_exports_unchanged_formal_not_stale_capture);
  root (@NM.PublicGenerationCallExample.ambient_generation_does_not_block_callee_first_output);
  root (@NM.PublicGenerationCallExample.bound_function_ambient_generation_does_not_block_first_output);
  root (@NM.PublicFunctorScopeExample.no_input_provider_application_succeeds);
  root (@NM.PublicFunctorScopeExample.private_input_provider_application_exports_concrete_meaning);
  root (@NM.PublicOutputTransferExample.prior_caller_output_does_not_occupy_callee_destination);
  root (@NM.PublicGenerationPrototype.GenerationPrototype.fresh_export_preserves_supported_type);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.TC.CT.check_tag_preserves);
  root (@NM.PublicHeapTyping.allocation_predicates);
  root (@PublicHeaps.box_preserves_heap);
  root (@PublicHeaps.array_preserves_heap);
  root (@PublicEntryTop.SR.safety_from_entry);
  root (@NM.PublicTypeRead.tyof_det);
  root (@NM.PublicTypeRead.payload_det);
  root (@NM.PublicValueTyping.unit_tag_typed);
  root (@NM.PublicBoolTyping.bool_value_shape);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.B.truthy_typed);
  root (@NM.PublicBodySafety.ctx_locals_push_distinct);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.safe_all);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.family_step_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.copy_apply_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.link_results_of_chain);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.platform_apply_of_chain);
  root (@PublicEntryTop.entry_safe);
  root (@PublicEntryTop.safety_public);
  root (@PublicEntryTop.safety_obligations);
  root (@PublicEntryTop.public_safety);
  root (@PublicEntryTop.handles_formed);
  root (@NM.PublicWorldFormation.mentions_typed_of);
  root (@NM.PublicFamilySig.family_sig_ok_readoff);
  root (@NM.PublicScoping.ex_scoped);
  root (@NM.PublicScoping.demand_at_th_scoped);
  root (@PublicEntryTop.ex_scoped_ok);
  root (@PublicEntryTop.demand_scoped_ok);
  root (@PublicEntryTop.entry_primitive);
  root (@PublicEntryTop.platform_prims_public);
  root (@PublicEntryTop.platform_sigs_fixed);
  root (@PublicEntryTop.platform_handles_formed);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.chain_platform_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.family_platform_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.chain_prims_of_ground);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.chain_core_premises);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.family_core);
  root (@NM.PublicChainAgree.chain_cover);
  root (@NM.PublicChainAgree.copy_news_methods);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.family_result_agrees);
  root (@NM.PublicFamilyAgree.family_items_agree);
  root (@NM.PublicFamilyAgree.items_agree_gen);
  root (@NM.PublicFamilyAgree.items_agree_pos);
  root (@NM.PublicFamilyAgree.family_items_pos);
  root (@NM.PublicCallerKeys.same_kind_entries);
  root (@NM.PublicCallerKeys.provide_result_length);
  root (@NM.PublicCallerKeys.selection_last);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.chain_forward_sigs);
  root (@NM.PublicChainFwd.news_method_key);
  root (@NM.PublicFamilyAgree.family_entries_rel);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.platform_not_linked);
  root (@PublicEntryTop.entry_realizes);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.callee_realizes);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.chain_results_scoped);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.kept_outputs_unmentioned);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.chain_core_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.chain_results_of_new);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.family_link_results_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.body_provides);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.bindings_writes);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.chain_new_ok);
  root (@NM.PublicCopySubst.copy_outputs_at);
  root (@NM.PublicLinkWalk.family_binds_no_dests);
  root (@NM.PublicLinkScope.items_scope);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.family_link_new_ok);
  root (@PublicEntryTop.FCp.CRz.CSg.chain_forward_copy_sigs);
  root (@PublicEntryTop.FCp.CRz.CSg.chain_sigs);
  root (@PublicEntryTop.FCp.CRz.chain_real);
  root (@PublicEntryTop.FCp.CRz.chain_realize_ok);
  root (@PublicEntryTop.FCp.family_copy_realize_ok);
  root (@PublicEntryTop.FCp.family_top);
  root (@PublicEntryTop.FCp.platform_level);
  root (@PublicEntryTop.FCp.CRz.root_level);
  root (@PublicEntryTop.FCp.CRz.top_envok);
  root (@NM.PublicChainRealize.level_rel);
  root (@NM.PublicChainRealize.pos_last_all);
  root (@NM.PublicChainSigs.pos_last);
  root (@NM.PublicChainSigs.result_gens_vals);
  root (@NM.PublicChainSigs.entries_rel_kinds);
  root (@NM.PublicCopySubst.ground_app_subst);
  root (@NM.PublicCopySubst.provision_gens_subst);
  root (@NM.PublicCopyFresh.result_gens);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.copy_read_at);
  root (@NM.PublicLinkPins.items_dests_made);
  root (@NM.PublicConsol.link_body_has_body);
  root (@NM.PublicConsol.result_realizes_restated);
  root (@NM.PublicConsol.link_sources_restated);
  root (@NM.PublicOrientation.mgm_classes_iff);
  root (@NM.PublicOrientation.mgm_reorient);
  root (@NM.PublicOrientation.merges_by_pairs);
  root (@NM.PublicEquation.same_swap_classes);
  root (@NM.PublicEquation.eq_swap_classes);
  root (@NM.PublicMergeClass.functx_merges);
  root (@NM.PublicSameNeed.need_reading_orientation_free_F);
  root (@NM.PublicSameNeed.need_reading_orientation_free_G);
  root (@NM.PublicSameNeed.link_need_unambiguous);
  root (@NM.EagerCopy.sources_compose);
  root (@NM.EagerCopy.positions_chain);
  root (@NM.EagerCopy.selection_chain);
  root (@NM.EagerCopy.adapt_compose);
  root (@NM.EagerCopy.adapt_chain_sound);
  root (@NM.EagerCopy.adapt_chain_strict);
  root (@NM.PublicFormation.freshfor_fresh);
  root (@NM.PublicLinkRun.app_env_meaning);
  root (@NM.PublicChainSigs.table_reads_merge);
  root (@NM.PublicChainSigs.table_reads_merge_iff);
  root (@PublicEntryTop.FCp.CRz.chain_real);
  root (@PublicEntryTop.FCp.CRz.root_level);
  root (@NM.PublicChainRealize.entry_reading_of_rel);
  root (@NM.PublicChainRealize.entry_peq);
  root (@NM.PublicCallRealize.gens_subst_hit);
  root (@NM.PublicCallRealize.gens_subst_iff);
  root (@NM.PublicCallRealize.functx_merge_symbols);
  root (@NM.PublicMerge.merge_eq_gens_out);
  root (@NM.PublicLinkScope.section_gens_out);
  root (@NM.PublicLinkScope.step_scope_section);
  root (@NM.PublicLinkScope.sections_gens_out);
  root (@NM.PublicLinkScope.section_sig_gens_out);
  root (@NM.PublicChainRealize.entry_keys_merge_iff);
  root (@NM.PublicChainRealize.tr_all_merge);
  root (@NM.PublicChainRealize.tr_all_section);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.LRn.link_run_fact);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.LRn.body_meaning_input);
  root (@NM.PublicLinkPins.link_dests_out);
  root (@NM.PublicCallerKeys.provide_result_key_news);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.selection_news);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_sources_range);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.family_copy_apply_of_realize);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.apply_functor_keys);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.pre_junk_meaning);
  root (@NM.PublicGenerationFacts.result_environment_shadows_occupied);
  root (@NM.PublicOutputSubst.output_entries_rel);
  root (@NM.PublicOutputSubst.provide_result_rel);
  root (@PublicLinkMergesI.captured_in_force_ok);
  root (@NM.PublicLinkMerges.captured_mentioned);
  root (@NM.PublicFormation.family_keys_typed);
  root (@NM.PublicCallRealize.ctxof_formed);
  root (@NM.PublicCallRealize.call_agree);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.absorbs_canon_iff);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.static_ok_canonical_iff);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.call_absorbs_merge);
  root (@NM.PublicFormation.ex_ok);
  root (@NM.PublicFormation.ex_formed');
  root (@NM.PublicFormation.ex_nominal_formed');
  root (@NM.PublicFormation.keys_scoped);
  root (@NM.PublicFormation.table_keys_scoped);
  root (@NM.PublicFormation.subst_equal_gens);
  root (@NM.PublicFormation.retracted_formed);
  root (@NM.PublicFormation.merge_shapes_formed);
  root (@NM.PublicFormation.class_section_formed);
  root (@NM.PublicFormation.merge_section_formed);
  root (@NM.PublicFormation.merge_section_ok);
  root (@NM.PublicFormation.tmok_read);
  root (@NM.PublicCallRealize.sig_read_scoped);
  root (@NM.PublicSubstFacts.gens_subst_iff);
  root (@NM.PublicScoping.entry_provision_gens_read_f);
  root (@NM.PublicFormation.scope_gens_section_bound);
  root (@NM.PublicFormation.functx_scope_gens);
  root (@NM.PublicFormation.ctxof_class_formed);
  root (@NM.PublicFormation.functx_class_formed);
  root (@NM.PublicMerge.merge_eq_gens_bound);
  root (@NM.PublicMerge.section_gens_bound);
  root (@NM.PublicFormation.ctxof_cx_formed);
  root (@NM.PublicFormation.functx_cx_formed);
  root (@NM.PublicFormation.demand_formed_cx);
  root (@NM.PublicFormation.apply_abstract_formed);
  root (@NM.PublicFormation.lambda_cx_formed);
  root (@NM.PublicFormation.fn_body_formed);
  root (@NM.PublicFormation.units_bare_of);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.arms_select);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.TC.tag_check);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.PC.callable_lower);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.PC.callable_extends);
  root (@PublicEntryTop.FCp.CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.PC.callable_lower_extends);
  root (@NM.AuditFacts.platform_linked_fields);
  root (@PublicEntryTop.PS.primitive_safe);
  root (@PublicEntryTop.PS.state_ok_initial);
  root (@NM.PublicTypeRead.tyof_abs);
  root (@NM.PublicTypeRead.tyof_rel);
  root (@NM.PublicTypeRead.sig_rel);
  root (@PublicEntryTop.SR.stuck_entry_is_wrong);
  root (@NM.PublicSavedTypeExample.legacy_saved_and_emitted_reads_differ);
  root (@NM.PublicSavedTypeExample.alias_binding_preserves_old_generation);
  root (@NM.PublicSavedTypeExample.public_bind_boxed_from_saved);
  root (@NM.PublicSavedTypeExample.saved_f64_rejects_integer_handle);
  root (@NM.PublicSavedTypeExample.result_transfer_preserves_old_generation);
  root (@NM.PublicEmit.emit_full_substitution);
  root (@NM.PublicClosureExample.C.expression_correct);
  root (@NM.PublicClosureExample.compiled_bodies_correct);
  root (@NM.PublicClosureExample.all_fuel_agreement);
  root (@NM.PublicClosureExample.compiled_capture_output_cast);
  root (@NM.PublicCaptureFacts.substitution_agrees_on_generations);
  root (@NM.PublicCaptureFacts.capture_ground_stable);
  root (@NM.PublicCaptureFacts.capture_provision_stable);
(* What the reference implementation, moss-ref, runs on a source file
   (run/Extract.v): the Gallina front end (and the AST dump of --dump),
   the Audit-side elaborator, and the primitives. Audit's interpreter and
   the passes are covered by public_safety and the pass theorems. *)
  root (@NM.Parse.parse_moss);
  root (@NM.Sx.Dump.dump_file);
  root (@NM.Parse.print_sx);
  root (@NM.Parse.load);
  root (@NM.EaMain.elaborate);
  root (@NM.EaMain.elaborate_with);
  root (@NM.EaBase.default_strategy);
  root (@NM.MossX.run_prim_fast_spec);
  root (@NM.MossX.run_prim_fast);
  root (@NM.MossX.run_prim);
  root (@NM.MossX.init_state);

(* Read-set pruning of Audit programs. *)
  root (@PruneTop.PS.prune_sound);
  root (@PruneTop.runs_pruned);
  root (@PruneTop.observe_pruned);
  root (@pruned_observation_agrees);
  root (@pruned_runs_agree);
  root (@default_interface_code_blind);

(* The platform-family fast path for Audit programs. *)
  root (@FastTop.FS.run_main_forward);
  root (@FastTop.FS.run_main_backward);
  root (@FastTop.runs_fast_one);
  root (@FastTop.runs_fastpath);
  root (@fastpath_runs_agree);
  root (@fastpath_prune_runs_agree);

(* Capture trimming for Audit programs. *)
  root (@CapTrimTop.PS.captrim_sound);
  root (@CapTrimTop.observe_trimmed);
  root (@CapTrimTop.runs_trimmed);
  root (@captrim_runs_agree);
  root (@fastpath_prune_captrim_runs_agree);

(* Ignored nonallocating primitive sites preserve exact public execution. *)
  root (@PublicSiteErasure.reference_site_erasure);
  root (@PublicSiteErasure.Simulation.primitive_bindings_normalized);
  root (@PublicSiteErasure.Simulation.primitive_chain_normalized);
  root (@PublicSiteErasure.Simulation.primitive_prefix_normalized);
  root (@PublicSiteErasure.Simulation.primitive_functor_application_normalized);

(* Deferred type substitution. *)
  root (@LazyTop.leval_agrees);
  root (@LazyTop.lrun_main_agrees);
  root (@FastTopI.feval_agrees);
  root (@FastTopI.frun_main_agrees);
  root (@CloTop.cc_agrees);
  root (@CloTop.crun_main_agrees);
  root (@CloTop.crun_main_reference)].

Print Assumptions certificate_roots.

From NM Require InputGenerationAllocate InputGenerationRuntime.

(* Private input scopes retain generation meanings across source rebinding. *)
Print Assumptions NM.InputGenerationMerge.private_rebind_read.
Print Assumptions NM.InputGenerationPlan.link_input_canonical_read.
Print Assumptions NM.InputGenerationPlan.source_frame_name_readback.
Print Assumptions NM.InputGenerationAllocate.reserved_link_input_plan.
Print Assumptions NM.InputGenerationAllocate.reserved_input_symbols.
Print Assumptions NM.InputGenerationAllocate.reserved_private_scope.
Print Assumptions NM.InputGenerationRuntime.old_input_survives_output_shadowing.
Print Assumptions NM.InputGenerationRuntime.private_capture_grounding_survives_caller_rebinding.

From NM Require EaMetadataSound.
Print Assumptions NM.EaMetadataSound.elaborate_with_legacy_metadata.
