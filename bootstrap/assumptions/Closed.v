(* These proofs are closed under the global context.
   A single traversal shares dependencies across the proof roots. *)
From Stdlib Require Import List.
Import ListNotations.
From NM Require
  AllocationFacts AllocationRenaming BodyLiteralTyping BodyMethodTyping BodyProgramTyping
  BodyReadback BodyTyping EaAllocationSound EaApplicationSound EaAssemblyCodeEffects
  EaAssumesSound EaBindCodeEffects EaBindItemSound EaClosureSound EaCodeAllocationSound
  EaCodeEffects EaConstantSound EaContextFuel EaContextReadFacts EaContextTableSound
  EaControlSound EaCopySound EaDeclarationSound EaDefinedProviderSound EaDependencyFuel
  EaDirectBindSound EaEntriesSound EaExpressionCodeEffects EaExpressionSound EaFamilyMatching
  EaFamilyProducerSound EaFamilySound EaFixedSound EaForwarderSound EaLambdaKeys EaLambdaSound
  EaLinkBodySound EaLiteralSound EaMainSymbols EaMentionsSound EaMergeSound EaMetadataSound
  EaMethodSound EaOutputSound EaPatternSound EaProviderBindSound EaResultBudget
  EaResultCoverage EaResultSound EaRootSound EaSatisfiesSound EaTypesSound EaUnifySound
  EaValuePathSound EaWorldSound EaWrappedProviderSound FrontEnd FrontEndComplete.

Definition root {T : Type} (proof : T) : {T : Type & T} := existT (fun T => T) T proof.

Definition closed_roots : list {T : Type & T} := [
  root (@NM.FrontEnd.parse_moss_sound);
  root (@NM.FrontEndComplete.parse_moss_complete);
  root (@NM.EaTypesSound.ty_of_sound);
  root (@NM.EaTypesSound.brackets_sound);
  root (@NM.EaTypesSound.sig_of_sound);
  root (@NM.EaTypesSound.lang_sym_sound);
  root (@NM.EaTypesSound.lang_item_sym_sound);
  root (@NM.EaTypesSound.method_named_sound);
  root (@NM.EaTypesSound.payload_of_sound);
  root (@NM.EaTypesSound.recv_ty_sound);
  root (@NM.EaMentionsSound.free_in_sound);
  root (@NM.EaMentionsSound.sig_mentions_sound);
  root (@NM.EaMentionsSound.spec_types_go_sound);
  root (@NM.EaMentionsSound.spec_types_sound);
  root (@NM.EaMentionsSound.fun_outputs_sound);
  root (@NM.EaDependencyFuel.free_in_exact);
  root (@NM.EaDependencyFuel.sig_mentions_exact);
  root (@NM.EaDependencyFuel.app_of_sound);
  root (@NM.EaDependencyFuel.ty_of_depth);
  root (@NM.EaDependencyFuel.sig_of_depth);
  root (@NM.EaDependencyFuel.free_in_exact_after_type_read);
  root (@NM.EaDependencyFuel.app_of_sound_after_signature_read);
  root (@NM.EaContextFuel.spec_types_eventually_complete);
  root (@NM.EaContextFuel.spec_types_exact);
  root (@NM.EaContextFuel.fun_outputs_exact);
  root (@NM.EaWorldSound.bundle_world_r_sound);
  root (@NM.EaWorldSound.bundle_world_r_bound);
  root (@NM.EaWorldSound.bundle_world_r_context_readers_exact);
  root (@NM.EaResultSound.result_mentions_sound);
  root (@NM.EaResultSound.mentions_sound);
  root (@NM.EaResultSound.bundle_world_r_result_readers_sound);
  root (@NM.EaFixedSound.poly_explicit_exact);
  root (@NM.EaFixedSound.poly_sound);
  root (@NM.EaFixedSound.fixed_sound);
  root (@NM.EaFixedSound.fixed_readings_member_sound);
  root (@NM.EaFixedSound.bundle_world_r_fixed_readers_sound);
  root (@NM.EaFixedSound.fixed_readings_unbracketed);
  root (@NM.EaMainSymbols.load_world_scope_path);
  root (@NM.EaMainSymbols.bundle_world_r_main_symbols_exact);
  root (@NM.EaMainSymbols.elaborate_with_main_symbols_exact);
  root (@NM.EaResultCoverage.result_mentions_path);
  root (@NM.EaResultCoverage.result_mentions_eventually_complete);
  root (@NM.EaResultCoverage.mentions_eventually_complete);
  root (@NM.EaResultBudget.result_mentions_exact);
  root (@NM.EaResultBudget.mentions_exact);
  root (@NM.EaResultBudget.poly_exact);
  root (@NM.EaResultBudget.fixed_exact);
  root (@NM.EaResultBudget.fixed_readings_exact);
  root (@NM.EaResultBudget.bundle_world_r_fixed_readings);
  root (@NM.EaResultBudget.bundle_world_r_dependency_readers_exact);
  root (@NM.EaRootSound.lang_sym_complete);
  root (@NM.EaRootSound.lang_item_sym_complete);
  root (@NM.EaRootSound.root_items_sound);
  root (@NM.EaRootSound.root_ctx_sound);
  root (@NM.EaMetadataSound.metadata_fuel_sufficient);
  root (@NM.EaMetadataSound.metadata_fuel_legacy);
  root (@NM.EaMetadataSound.bundle_world_r_metadata_exact);
  root (@NM.EaEntriesSound.item_entries_sound);
  root (@NM.EaEntriesSound.items_entries_sound);
  root (@NM.EaEntriesSound.entries_sound_at_budget);
  root (@NM.EaEntriesSound.bundle_world_r_entries_sound);
  root (@NM.EaRootSound.bundle_world_r_root_ctx_sound);
  root (@NM.EaMergeSound.unifiesb_spec);
  root (@NM.EaMergeSound.merge_scopeb_spec);
  root (@NM.EaMergeSound.merge_checked);
  root (@NM.EaMergeSound.merge_unifies);
  root (@NM.EaMergeSound.needs_exact);
  root (@NM.EaContextTableSound.context_table_computed);
  root (@NM.EaContextTableSound.assume_ctx_sound);
  root (@NM.EaContextTableSound.fun_ctx_sound);
  root (@NM.EaOutputSound.ctx_spec_items_sound);
  root (@NM.EaOutputSound.output_brackets_sound);
  root (@NM.EaOutputSound.output_item_entries_sound);
  root (@NM.EaOutputSound.output_items_entries_sound);
  root (@NM.EaOutputSound.provide_result_sound);
  root (@NM.EaOutputSound.output_entries_sound_at_budget);
  root (@NM.EaOutputSound.provide_result_sound_at_budget);
  root (@NM.EaOutputSound.bundle_world_r_provide_result_sound);
  root (@NM.EaFamilySound.family_result_sound);
  root (@NM.EaFamilySound.is_family_shape_sound);
  root (@NM.EaFamilySound.family_bracket_free_sound);
  root (@NM.EaFamilySound.family_bracket_free_complete);
  root (@NM.EaFamilySound.family_keyed_sound);
  root (@NM.EaFamilySound.is_family_sound);
  root (@NM.EaFamilySound.bundle_world_r_is_family_sound);
  root (@NM.EaContextTableSound.installed_context_table_sound);
  root (@NM.EaContextReadFacts.type_readers_context);
  root (@NM.EaContextReadFacts.mentions_context);
  root (@NM.EaContextReadFacts.entries_context);
  root (@NM.EaContextReadFacts.root_ctx_context);
  root (@NM.EaContextReadFacts.merge_context);
  root (@NM.EaContextReadFacts.assume_ctx_context);
  root (@NM.EaContextReadFacts.fun_ctx_context);
  root (@NM.EaContextReadFacts.context_table_context);
  root (@NM.EaContextTableSound.bundle_world_r_context_table_sound);
  root (@NM.EaUnifySound.resolve_absorbed);
  root (@NM.EaUnifySound.unify_absorbed);
  root (@NM.EaUnifySound.merge_principal);
  root (@NM.EaUnifySound.merge_function);
  root (@NM.EaUnifySound.merge_sound);
  root (@NM.EaPatternSound.member_with_head_sound);
  root (@NM.EaPatternSound.field_pats_sound);
  root (@NM.EaPatternSound.pat_of_sound);
  root (@NM.EaControlSound.fitsb_exact);
  root (@NM.AllocationFacts.allocation_grows);
  root (@NM.AllocationFacts.allocations_disjoint);
  root (@NM.AllocationFacts.allocation_rename);
  root (@NM.AllocationFacts.allocate_range);
  root (@NM.AllocationFacts.frame_allocation_transition);
  root (@NM.AllocationFacts.fresh_outputs_disjoint);
  root (@NM.AllocationFacts.frame_allocation_compatible);
  root (@NM.AllocationFacts.copy_allocation_contract);
  root (@NM.AllocationFacts.capture_allocation_contract);
  root (@NM.EaAllocationSound.fresh_n_range);
  root (@NM.EaAllocationSound.fresh_table_range);
  root (@NM.EaAllocationSound.fresh_tables_allocate);
  root (@NM.EaAllocationSound.lambda_capture_scope_free_exact);
  root (@NM.EaAllocationSound.capture_counter_allocation);
  root (@NM.EaAllocationSound.lambda_captures_from_counter);
  root (@NM.EaSatisfiesSound.pfind_exact);
  root (@NM.EaSatisfiesSound.sat_map_exact);
  root (@NM.EaSatisfiesSound.satisfies_gen_sound);
  root (@NM.EaAssumesSound.choose_assumption_atom_sound);
  root (@NM.EaAssumesSound.assumption_theta_sound);
  root (@NM.EaAssumesSound.assumes_complete_sound);
  root (@NM.EaAssumesSound.elaborate_with_assumes_complete);
  root (@NM.EaCodeAllocationSound.reserve_code_spec);
  root (@NM.EaCodeAllocationSound.write_reserved_code_spec);
  root (@NM.EaCodeAllocationSound.emit_code_spec);
  root (@NM.EaCodeAllocationSound.assembled_code_lookup);
  root (@NM.EaCodeAllocationSound.assembled_lambda_section);
  root (@NM.EaCodeAllocationSound.assembled_link_origin);
  root (@NM.EaCodeEffects.lambda_provider_records_section);
  root (@NM.EaBindCodeEffects.emit_linked_body_records_origin);
  root (@NM.EaBindCodeEffects.cached_link_records_origin);
  root (@NM.EaAssemblyCodeEffects.assembly_link_records);
  root (@NM.EaAssemblyCodeEffects.assembly_lambda_origins_bound);
  root (@NM.EaLinkBodySound.link_body_at_checked_decls);
  root (@NM.EaLinkBodySound.emit_linked_body_preserves_typed_cache);
  root (@NM.EaLinkBodySound.linked_code_in_program_earlier);
  root (@NM.EaLinkBodySound.assembled_linked_code_in_program);
  root (@NM.EaApplicationSound.allocate_application_outputs_range);
  root (@NM.EaApplicationSound.collides_only_as_sound);
  root (@NM.EaApplicationSound.apply_abstract_sound);
  root (@NM.EaForwarderSound.read_forwarder_sound);
  root (@NM.EaForwarderSound.place_forwarder_spec);
  root (@NM.EaForwarderSound.abstract_provider_sound);
  root (@NM.EaFamilySound.family_application_sound);
  root (@NM.EaCodeEffects.code_transition_extends);
  root (@NM.EaCodeEffects.nested_code_preserves_reserved_slot);
  root (@NM.EaCodeEffects.fn_provider_code_effect);
  root (@NM.EaCodeEffects.lambda_provider_code_effect);
  root (@NM.EaCodeEffects.meth_provider_code_effect);
  root (@NM.EaCodeEffects.apply_abstract_code_effect);
  root (@NM.EaCodeEffects.fresh_table_code_effect);
  root (@NM.AllocationRenaming.fresh_batches_are_renamings);
  root (@NM.AllocationRenaming.allocated_batches_alpha);
  root (@NM.EaBindCodeEffects.copy_interpretation_code_effect);
  root (@NM.EaBindCodeEffects.link_body_code_effect);
  root (@NM.EaBindCodeEffects.bind_items_preserve_code);
  root (@NM.EaExpressionCodeEffects.expression_code_effects);
  root (@NM.EaExpressionCodeEffects.run_code_effect);
  root (@NM.EaAssemblyCodeEffects.assemble_preserves_code_bound);
  root (@NM.EaAssemblyCodeEffects.assembled_code_lookup_exact);
  root (@NM.EaClosureSound.fun_closure_sound);
  root (@NM.EaDeclarationSound.same_result_exact_at_declarations);
  root (@NM.EaCopySound.copy_result_position_sound);
  root (@NM.EaCopySound.copy_interpretation_sound);
  root (@NM.EaCopySound.fun_copy_sound);
  root (@NM.EaLambdaKeys.lambda_need_keys_exact);
  root (@NM.EaLambdaKeys.captured_cx_keys_match);
  root (@NM.EaFamilyMatching.read_off_one_domain);
  root (@NM.EaFamilyMatching.read_off_one_complete);
  root (@NM.EaFamilyMatching.family_read_off_sound);
  root (@NM.EaLambdaSound.lambda_provider_sound);
  root (@NM.EaLambdaSound.write_lambda_keeps_generated_post);
  root (@NM.EaConstantSound.read_constant_sound);
  root (@NM.EaLiteralSound.literal_expression_sound);
  root (@NM.EaMethodSound.method_call_post_sound);
  root (@NM.EaValuePathSound.value_path_sound);
  root (@NM.EaExpressionSound.expression_step_sound);
  root (@NM.EaDefinedProviderSound.defined_function_provider_post_sound);
  root (@NM.EaDefinedProviderSound.attached_method_provider_post_sound);
  root (@NM.EaDirectBindSound.type_bind_post_sound);
  root (@NM.EaDirectBindSound.value_bind_post_sound);
  root (@NM.EaDirectBindSound.inline_functor_post_sound);
  root (@NM.EaProviderBindSound.named_function_bind_post_sound);
  root (@NM.EaProviderBindSound.lambda_function_bind_post_sound);
  root (@NM.EaProviderBindSound.method_bind_post_sound);
  root (@NM.EaForwarderSound.function_provider_post_sound);
  root (@NM.EaWrappedProviderSound.method_provider_post_sound);
  root (@NM.BodyMethodTyping.method_call_translation);
  root (@NM.BodyMethodTyping.method_call_readback);
  root (@NM.BodyLiteralTyping.literal_translation);
  root (@NM.BodyLiteralTyping.literal_readback);
  root (@NM.EaFamilyProducerSound.family_produces_covers);
  root (@NM.EaFamilyProducerSound.checked_family_use);
  root (@NM.EaBindItemSound.bind_item_post_sound);
  root (@NM.BodyTyping.body_translation_agrees);
  root (@NM.BodyTyping.body_typing_renumber);
  root (@NM.BodyTyping.local_source_has_plan);
  root (@NM.BodyReadback.expression_has_plan);
  root (@NM.BodyReadback.block_has_plan);
  root (@NM.BodyProgramTyping.separated_elaboration_sound);
  root (@NM.BodyProgramTyping.separated_elaboration_readback)
].

Print Assumptions closed_roots.
