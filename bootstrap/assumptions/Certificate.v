(* The checked component contracts, supporting proof milestones, and
   the executable roots Extract2.v extracts. Their assumptions are
   transitively everything the guarantee rests on, and check.sh requires
   them to be only the kernel's `Primitive` declarations (Rocq's primitive
   integers, floats and arrays): no Axiom, no Admitted. *)
From NM Require Certified Parse FastPath Prune SlotX FoldX
  CachedClo CachedCloFacts LazyX LazyClo LazyInterpFacts LazyCloFacts LazyOracle XHeapC MainProgramBool BindMethodLambdaExec BindMethodDispatch BindMethodAllDispatch BindMethodExec BodyPending.

From NM Require AuditExamples AuditBuildExample AuditRepresentation
  SourceSnapshot PublicEmit PublicClosureExample PublicCaptureFacts
  SnapshotLoaderBound PublicWorld PublicTypeFacts PublicValueTyping PublicCastTyping
  PublicSavedTypeExample PublicHeapTyping PublicWorldScope SnapshotLoader
  PublicLocalsTyping PublicWorldRequirements SnapshotLoaderWitness
  PublicGenerationFacts PublicAliasContext PublicGenerationCallExample PublicOutputTransferExample
  PublicGenerationPrototype PublicCaptureScopeFacts PublicFunctorScopeExample PublicContextFacts PublicSafety
  PublicTypeRead PublicBoolTyping PublicBodySafety PublicBodySlice PublicArms PublicTagCheck PublicCallRealize
  PublicEntry PublicPrimitiveSafety PublicFormation PublicLinkMerges PublicWorldFormation PublicFamilySig PublicCopyApply
  PublicPlatformApply PublicFamilyStep PublicLinkResults PublicOutputSubst PublicFamilyAgree PublicChainAgree
  PublicChainCore PublicChainNew PublicChainFwd PublicScoping PublicCallerKeys PublicFamilyCoherent PublicCopyFresh.

Module PublicInterp := Audit.Semantics.Interpreter PublicClosureExample.Prims.
Module PublicCasts := PublicCastTyping.CastTyping PublicClosureExample.Prims PublicInterp.
Module PublicPublic := Audit.Semantics.Public AuditExamples.Host.
Module PublicHeaps := PublicHeapTyping.HeapSafety AuditExamples.Host PublicPublic.
Module PublicBool := PublicBoolTyping.BoolTruthy PublicClosureExample.Prims PublicInterp.
Module PublicArmsI := PublicArms.Arms PublicClosureExample.Prims PublicInterp.
Module PublicTags := PublicTagCheck.Checks PublicClosureExample.Prims PublicInterp.
Module PublicSlice := PublicBodySlice.BodySlice PublicClosureExample.Prims PublicInterp.
Module PublicPlatform := PublicPlatformApply.PlatformApply PublicClosureExample.Prims PublicInterp.
Module PublicCopy := PublicCopyApply.CopyApply PublicClosureExample.Prims PublicInterp.
Module PublicLinkRes := PublicLinkResults.LinkResults PublicClosureExample.Prims PublicInterp.
Module PublicChainCoreI := PublicChainCore.ChainCore PublicClosureExample.Prims PublicInterp.
Module PublicChainNewI := PublicChainNew.ChainNew PublicClosureExample.Prims PublicInterp.
Module PublicChainFwdI := PublicChainFwd.ChainFwd PublicClosureExample.Prims PublicInterp.
Module PublicSafetyTop := PublicSafety.SafetyReduction AuditExamples.Host PublicPublic.
Module PublicFamily := PublicFamilyStep.FamilyStep PublicClosureExample.Prims PublicInterp.
Module PublicEntryTop := PublicEntry.EntrySafety AuditExamples.Host PublicPublic.
Module PublicPrims := PublicPrimitiveSafety.PrimitiveSafety AuditExamples.Host PublicPublic.
Module PublicLinkMergesI := PublicLinkMerges.LinkMerges PublicClosureExample.Prims PublicInterp.

(* One dependency traversal computes the union for every field below.
   Closed.v separately checks the roots that must not assume primitives. *)
Definition certificate_roots :=
 (@NM.AuditExamples.bare_return_requires_unit,
  @NM.AuditExamples.one_does_not_insert_a_multiply,
  @NM.AuditExamples.an_arbitrary_cast_entry_is_rejected,
  @NM.AuditBuildExample.public_build_is_inhabited,
  @NM.AuditBuildExample.built_program_runs,
  @NM.SourceSnapshot.source_read_iff,
  @NM.SourceSnapshot.source_from_files_sound,
  @NM.SourceSnapshot.source_from_files_complete,
  @NM.SourceSnapshot.source_read_finite,
  @NM.SourceSnapshot.source_read_canonical,
  @NM.SnapshotLoaderBound.snapshot_fuel_stable,
  @NM.SnapshotLoaderBound.snapshot_success_at_bound,
  @NM.SnapshotLoaderBound.load_snapshot_success_adequate,
  @NM.SnapshotLoader.load_sound,
  @NM.SnapshotLoader.load_complete,
  @NM.SnapshotLoader.walk_stable,
  @NM.SnapshotLoaderWitness.bundled_reachable_agreement,
  @NM.SnapshotLoaderWitness.load_elaborates_complete,
  @NM.PublicWorld.load_world_declarations,
  @NM.PublicWorld.load_world_declaration_complete,
  @NM.PublicWorld.load_world_resolves,
  @NM.PublicWorldScope.load_world_world_of_from_requirements,
  @NM.PublicWorldRequirements.elaborate_public_world_of,
  @NM.PublicTypeFacts.ty_eqb_spec,
  @NM.PublicValueTyping.typing_extends,
  @NM.PublicValueTyping.field_lookup_typed,
  @NM.PublicValueTyping.checked_header_typed,
  @NM.PublicValueTyping.tagged_payload_typed,
  @NM.PublicLocalsTyping.locals_lookup,
  @NM.PublicLocalsTyping.locals_assign,
  @NM.PublicLocalsTyping.locals_extends,
  @NM.PublicGenerationFacts.bind_type_preserves_ground,
  @NM.PublicGenerationFacts.result_environment_preserves_generation,
  @NM.PublicGenerationFacts.result_environment_preserves_supported_ground,
  @NM.PublicGenerationFacts.result_environment_exports_one_snapshot,
  @NM.PublicAliasContext.public_type_bind_has_alias_context,
  @NM.PublicContextFacts.function_context_canonical,
  @NM.PublicContextFacts.functor_context_canonical,
  @NM.PublicContextFacts.formed_type_binding_preserves_alias_context,
  @NM.PublicContextFacts.function_context_unique,
  @NM.PublicContextFacts.functor_context_unique,
  @NM.PublicContextFacts.call_theta_merged,
  @NM.PublicCaptureScopeFacts.typed_capture_generation_lookup,
  @NM.PublicCaptureScopeFacts.typed_capture_alias_lookup,
  @NM.PublicGenerationCallExample.actual_application_exports_unchanged_formal_not_stale_capture,
  @NM.PublicGenerationCallExample.ambient_generation_does_not_block_callee_first_output,
  @NM.PublicGenerationCallExample.bound_function_ambient_generation_does_not_block_first_output,
  @NM.PublicFunctorScopeExample.no_input_provider_application_succeeds,
  @NM.PublicFunctorScopeExample.private_input_provider_application_exports_concrete_meaning,
  @NM.PublicOutputTransferExample.prior_caller_output_does_not_occupy_callee_destination,
  @NM.PublicGenerationPrototype.GenerationPrototype.fresh_export_preserves_supported_type,
  @PublicCasts.check_tag_preserves,
  @NM.PublicHeapTyping.allocation_predicates,
  @PublicHeaps.box_preserves_heap,
  @PublicHeaps.array_preserves_heap,
  @PublicSafetyTop.safety_from_entry,
  @NM.PublicTypeRead.tyof_det,
  @NM.PublicTypeRead.payload_det,
  @NM.PublicValueTyping.unit_tag_typed,
  @NM.PublicBoolTyping.bool_value_shape,
  @PublicBool.truthy_typed,
  @NM.PublicBodySafety.ctx_locals_push_distinct,
  @PublicSlice.safe_all,
  @PublicFamily.family_step_ok,
  @PublicCopy.copy_apply_ok,
  @PublicCopy.link_results_of_chain,
  @PublicCopy.platform_apply_of_chain,
  @PublicEntryTop.entry_safe,
  @PublicEntryTop.safety_public,
  @PublicEntryTop.safety_obligations,
  @PublicEntryTop.handles_formed,
  @NM.PublicWorldFormation.merge_formed_of,
  @NM.PublicWorldFormation.mentions_typed_of,
  @NM.PublicFamilySig.family_sig_ok_readoff,
  @NM.PublicScoping.ex_scoped,
  @NM.PublicScoping.demand_at_site_scoped,
  @PublicEntryTop.ex_scoped_ok,
  @PublicEntryTop.demand_scoped_ok,
  @PublicEntryTop.entry_primitive,
  @PublicEntryTop.platform_prims_public,
  @PublicEntryTop.platform_sigs_fixed,
  @PublicEntryTop.platform_handles_formed,
  @PublicPlatform.chain_platform_ok,
  @PublicPlatform.family_platform_ok,
  @PublicPlatform.chain_prims_of_ground,
  @PublicPlatform.chain_core_premises,
  @PublicPlatform.family_core,
  @NM.PublicChainAgree.chain_cover,
  @NM.PublicChainAgree.copy_news_methods,
  @PublicPlatform.family_result_agrees,
  @NM.PublicFamilyAgree.family_items_agree,
  @NM.PublicFamilyAgree.items_agree_gen,
  @NM.PublicFamilyAgree.items_agree_pos,
  @NM.PublicFamilyAgree.family_items_pos,
  @NM.PublicCallerKeys.same_kind_entries,
  @NM.PublicCallerKeys.provide_result_length,
  @NM.PublicCallerKeys.selection_last,
  @PublicChainFwdI.chain_forward_sigs,
  @NM.PublicChainFwd.news_method_key,
  @NM.PublicFamilyAgree.family_entries_rel,
  @PublicPlatform.platform_not_linked,
  @PublicEntryTop.entry_realizes,
  @PublicSlice.callee_realizes,
  @PublicLinkRes.chain_results_scoped,
  @PublicLinkRes.kept_outputs_unmentioned,
  @PublicChainCoreI.chain_core_ok,
  @PublicChainCoreI.chain_results_of_new,
  @PublicChainCoreI.family_link_results_ok,
  @PublicChainCoreI.body_provides,
  @PublicChainCoreI.bindings_writes,
  @PublicChainNewI.chain_new_ok,
  @NM.PublicCopyFresh.copy_outputs_at,
  @NM.PublicCopyFresh.ground_app_subst,
  @NM.PublicCallerKeys.provide_result_key_news,
  @PublicChainCoreI.selection_news,
  @PublicCopy.chain_sources_range,
  @PublicFamily.family_copy_apply_of_realize,
  @PublicFamily.apply_functor_keys,
  @PublicLinkRes.pre_junk_meaning,
  @NM.PublicGenerationFacts.result_environment_shadows_occupied,
  @NM.PublicOutputSubst.output_entries_rel,
  @NM.PublicOutputSubst.provide_result_rel,
  @PublicLinkMergesI.captured_in_force_ok,
  @NM.PublicLinkMerges.captured_mentioned,
  @NM.PublicFormation.family_keys_typed,
  @NM.PublicCallRealize.ctxof_formed,
  @NM.PublicCallRealize.call_agree,
  @NM.PublicCallRealize.call_absorbs,
  @NM.PublicFormation.ex_ok,
  @NM.PublicFormation.ex_formed',
  @NM.PublicFormation.ex_nominal_formed',
  @NM.PublicFormation.keys_scoped,
  @NM.PublicFormation.table_keys_scoped,
  @NM.PublicFormation.ctxof_cx_formed,
  @NM.PublicFormation.functx_cx_formed,
  @NM.PublicFormation.demand_formed_cx,
  @NM.PublicFormation.apply_abstract_formed,
  @NM.PublicFormation.lambda_cx_formed,
  @NM.PublicFormation.fn_body_formed,
  @NM.PublicFormation.units_bare_of,
  @PublicArmsI.arms_select,
  @PublicTags.tag_check,
  @PublicPrims.primitive_safe,
  @PublicPrims.state_ok_initial,
  @NM.PublicTypeRead.tyof_abs,
  @NM.PublicTypeRead.tyof_rel,
  @NM.PublicTypeRead.sig_rel,
  @PublicSafetyTop.stuck_entry_is_wrong,
  @NM.PublicSavedTypeExample.legacy_saved_and_emitted_reads_differ,
  @NM.PublicSavedTypeExample.alias_binding_preserves_old_generation,
  @NM.PublicSavedTypeExample.public_bind_boxed_from_saved,
  @NM.PublicSavedTypeExample.saved_f64_rejects_integer_handle,
  @NM.PublicSavedTypeExample.result_transfer_preserves_old_generation,
  @NM.PublicEmit.emit_full_substitution,
  @NM.PublicClosureExample.C.expression_correct,
  @NM.PublicClosureExample.compiled_bodies_correct,
  @NM.PublicClosureExample.all_fuel_agreement,
  @NM.PublicClosureExample.compiled_capture_output_cast,
  @NM.PublicCaptureFacts.substitution_agrees_on_generations,
  @NM.PublicCaptureFacts.capture_ground_stable,
  @NM.PublicCaptureFacts.capture_provision_stable,
  @NM.BindMethodExec.check_bind_method,
  @NM.BindMethodAllDispatch.residual_after_method_bindings,
  @NM.BindMethodLambdaExec.check_bind_method_lambda,
  @NM.BindMethodDispatch.residual_after_method_lambda,
  @NM.BodyPending.pending,
  @NM.MainProgramBool.main_program_bool,
  @NM.MainProgramBool.elaborate_bool,
  @NM.Certified.Certified.elaborate,
  @NM.Certified.elaborate_sx_reads,
  @NM.Certified.default_interface_laws,
  @NM.Certified.default_compatible,
  @NM.Certified.Certified.eval_deterministic,
  @NM.Certified.Certified.interp_sound,
  @NM.Certified.Certified.interp_complete,
  @NM.Certified.Certified.spec_eval,
  @NM.Certified.Certified.xinterp_conservative,
  @NM.Certified.Certified.prim_law,
  @NM.Certified.Certified.prim_mlaw,
  @NM.Certified.Certified.prim_ulaw,
  @NM.Certified.Certified.Typed,
  @NM.Certified.Certified.typed_safe,
  @NM.Certified.Certified.typed_inhabited,
  @NM.Certified.Certified.method_choice_unique,
  @NM.Certified.RuntimeCertified.expression_safe,
  @NM.Certified.RuntimeCertified.main_safe,
  @NM.Certified.RuntimeCertified.main_typing_inhabited,
  @NM.Certified.RuntimeCertified.heap_example,
  @NM.XHeapC.ConcreteHeap.heap_mono,
  @NM.XHeapC.ConcreteHeap.heap_empty,
  @NM.XHeapC.ConcreteHeap.heap_other,
  @NM.XHeapC.ConcreteHeap.heap_gc,
  @NM.Certified.FrontEndCertified.lexer_sound,
  @NM.Certified.FrontEndCertified.lexer_complete,
  @NM.Certified.FrontEndCertified.parse,
  @NM.Certified.FrontEndCertified.parser_sound,
  @NM.Certified.FrontEndCertified.parser_complete,
  @NM.Certified.LoaderCertified.load,
  @NM.Certified.LoaderCertified.load_sound,
  @NM.Certified.elaborate_sx,
  @NM.Certified.run_main,
  @NM.Certified.OracleCertified.interpreter_correct,
(* Supporting lemmas for the source soundness proof. *)
  @NM.Certified.world_reads,
  @NM.Certified.elaborate_world_reads,
  @NM.Certified.world_of_reading,
  @NM.Certified.world_resolves,
  @NM.Certified.context_method_named,
  @NM.Certified.context_type_meanings,
  @NM.Certified.dump_reads,
  @NM.Certified.elaboration_frame,
  @NM.Certified.assume_types_exact,

(* Partial verification milestone: actual function-frame coverage and
   runtime safety given an explicit typing certificate. These do not assert
   that every accepted source program supplies that certificate. *)
  @NM.Certified.SourceCertified.elaborate_metadata_projects,
  @NM.Certified.SourceCertified.loader_reads,
  @NM.Certified.SourceCertified.world_of_reading,
  @NM.Certified.SourceCertified.world_resolves,
  @NM.Certified.SourceCertified.elaborate_world_of,
  @NM.Certified.SourceCertified.context_type_meanings,
  @NM.Certified.SourceCertified.function_frames_total,
  @NM.Certified.RuntimeCertified.typed_runtime_never_err,
  @NM.Certified.RuntimeCertified.certified_reification_never_err,

(* The front end moss_m2.ml runs on a source file: the Gallina lexer,
   parser and bundler (the driver only reads files), whose output the
   elaborator above reads. *)
  @NM.Parse.load_bundle,
  @NM.Parse.bundle_sx,

(* The legacy slot/fold engine (MOSS_ENGINE=fold), as moss_m2.ml runs it: the family
   fast path, pruning, pre-resolution, folding and closure compilation,
   then the run; and the theorem that makes it ExecutionModel's semantics, given the tables the driver
   builds (README.md, What is trusted). *)
  @NM.FastPath.fast_program,
  @NM.Prune.oracle_program,
  @NM.SlotX.shapes,
  @NM.SlotX.compile_fns,
  @NM.SlotX.compile_funs,
  @NM.FoldX.fold_e,
  @NM.FoldX.fold_b,
  @NM.CachedClo.cc,
  @NM.CachedClo.ccbs,
  @NM.CloScalar.prepare_scalar,
  @NM.CloScalar.prepared_scalar_execution,
  @NM.CachedClo.crun_main,
  @NM.CachedCloFacts.engine_agrees,
  @NM.CachedCloFacts.fast_engine_agrees,

(* The default deferred-type engine (MOSS_ENGINE=lazy) and its AST reference,
   with exact environment/compiler agreement and
   the same fast-path and pruning exceptions as the original pipeline. *)
  @NM.LazyX.frun_main,
  @NM.LazyClo.cc,
  @NM.LazyClo.ccbs,
  @NM.LazyClo.crun_main,
  @NM.LazyInterpFacts.frun_main_eq,
  @NM.LazyCloFacts.crun_main_eq,
  @NM.LazyCloFacts.engine_agrees,
  @NM.LazyCloFacts.fast_engine_agrees,

(* Scalar folding preserves checked composition, runtime types and fuel. *)
  @NM.LazyFold.fold_call,
  @NM.LazyFoldFacts.fold_call_agrees,

(* The direct engine bound to its explicit pipeline/table contract. *)
  @NM.LazyOracle.Certified.mapped_engine_agrees,
  @NM.LazyOracle.Certified.compiled_engine_agrees,
  @NM.LazyOracle.Certified.fast_path_agrees,
  @NM.LazyOracle.Certified.engine_agrees,
  @NM.LazyOracle.Certified.fast_engine_agrees).

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
