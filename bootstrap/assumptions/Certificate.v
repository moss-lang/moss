(* The checked component contracts, supporting proof milestones, and
   the executable roots Extract2.v extracts. Their assumptions are
   transitively everything the guarantee rests on, and check.sh requires
   them to be only the kernel's `Primitive` declarations (Rocq's primitive
   integers, floats and arrays): no Axiom, no Admitted. *)
From NM Require Certified Parse FastPath Prune SlotX FoldX
  CachedClo CachedCloFacts LazyX LazyClo LazyInterpFacts LazyCloFacts LazyOracle XHeapC MainProgramBool BindMethodLambdaExec BindMethodDispatch BindMethodAllDispatch BindMethodExec BodyPending.

From NM Require AuditExamples AuditBuildExample AuditRepresentation
  SourceSnapshot PublicEmit PublicClosureExample PublicCaptureFacts.

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
