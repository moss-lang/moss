(* These executable type-reader proofs are closed under the global context.
   check.sh audits every assumption reported here. *)
From NM Require EaTypesSound.

Print Assumptions NM.EaTypesSound.ty_of_sound.
Print Assumptions NM.EaTypesSound.brackets_sound.
Print Assumptions NM.EaTypesSound.sig_of_sound.

Print Assumptions NM.EaTypesSound.lang_sym_sound.
Print Assumptions NM.EaTypesSound.lang_item_sym_sound.
Print Assumptions NM.EaTypesSound.method_named_sound.
Print Assumptions NM.EaTypesSound.payload_of_sound.
Print Assumptions NM.EaTypesSound.recv_ty_sound.

From NM Require EaMentionsSound.
Print Assumptions NM.EaMentionsSound.free_in_sound.
Print Assumptions NM.EaMentionsSound.sig_mentions_sound.
Print Assumptions NM.EaMentionsSound.spec_types_go_sound.
Print Assumptions NM.EaMentionsSound.spec_types_sound.
Print Assumptions NM.EaMentionsSound.fun_outputs_sound.

From NM Require EaDependencyFuel EaContextFuel.
Print Assumptions NM.EaDependencyFuel.free_in_exact.
Print Assumptions NM.EaDependencyFuel.sig_mentions_exact.
Print Assumptions NM.EaDependencyFuel.app_of_sound.
Print Assumptions NM.EaDependencyFuel.ty_of_depth.
Print Assumptions NM.EaDependencyFuel.sig_of_depth.
Print Assumptions NM.EaDependencyFuel.free_in_exact_after_type_read.
Print Assumptions NM.EaDependencyFuel.app_of_sound_after_signature_read.
Print Assumptions NM.EaContextFuel.spec_types_eventually_complete.
Print Assumptions NM.EaContextFuel.spec_types_exact.
Print Assumptions NM.EaContextFuel.fun_outputs_exact.

From NM Require EaWorldSound.
Print Assumptions NM.EaWorldSound.bundle_world_r_sound.
Print Assumptions NM.EaWorldSound.bundle_world_r_bound.
Print Assumptions NM.EaWorldSound.bundle_world_r_context_readers_exact.

From NM Require EaResultSound.
Print Assumptions NM.EaResultSound.result_mentions_sound.
Print Assumptions NM.EaResultSound.mentions_sound.
Print Assumptions NM.EaResultSound.bundle_world_r_result_readers_sound.

From NM Require EaFixedSound.
Print Assumptions NM.EaFixedSound.poly_explicit_exact.
Print Assumptions NM.EaFixedSound.poly_sound.
Print Assumptions NM.EaFixedSound.fixed_sound.
Print Assumptions NM.EaFixedSound.fixed_readings_member_sound.
Print Assumptions NM.EaFixedSound.bundle_world_r_fixed_readers_sound.

Print Assumptions NM.EaFixedSound.fixed_readings_unbracketed.

From NM Require EaMainSymbols.
Print Assumptions NM.EaMainSymbols.load_world_scope_path.
Print Assumptions NM.EaMainSymbols.bundle_world_r_main_symbols_exact.
Print Assumptions NM.EaMainSymbols.elaborate_with_main_symbols_exact.

From NM Require EaResultCoverage.
Print Assumptions NM.EaResultCoverage.result_mentions_path.
Print Assumptions NM.EaResultCoverage.result_mentions_eventually_complete.
Print Assumptions NM.EaResultCoverage.mentions_eventually_complete.

From NM Require EaResultBudget.
Print Assumptions NM.EaResultBudget.result_mentions_exact.
Print Assumptions NM.EaResultBudget.mentions_exact.
Print Assumptions NM.EaResultBudget.poly_exact.
Print Assumptions NM.EaResultBudget.fixed_exact.
Print Assumptions NM.EaResultBudget.fixed_readings_exact.
Print Assumptions NM.EaResultBudget.bundle_world_r_fixed_readings.
Print Assumptions NM.EaResultBudget.bundle_world_r_dependency_readers_exact.

From NM Require EaRootSound.
Print Assumptions NM.EaRootSound.lang_sym_complete.
Print Assumptions NM.EaRootSound.lang_item_sym_complete.
Print Assumptions NM.EaRootSound.root_items_sound.
Print Assumptions NM.EaRootSound.root_ctx_sound.

From NM Require EaMetadataSound.
Print Assumptions NM.EaMetadataSound.metadata_fuel_sufficient.
Print Assumptions NM.EaMetadataSound.metadata_fuel_legacy.
Print Assumptions NM.EaMetadataSound.bundle_world_r_metadata_exact.

From NM Require EaEntriesSound.
Print Assumptions NM.EaEntriesSound.item_entries_sound.
Print Assumptions NM.EaEntriesSound.items_entries_sound.
Print Assumptions NM.EaEntriesSound.entries_sound_at_budget.
Print Assumptions NM.EaEntriesSound.bundle_world_r_entries_sound.

Print Assumptions NM.EaRootSound.bundle_world_r_root_ctx_sound.
From NM Require EaMergeSound.
Print Assumptions NM.EaMergeSound.unifiesb_spec.
Print Assumptions NM.EaMergeSound.merge_scopeb_spec.
Print Assumptions NM.EaMergeSound.merge_checked.
Print Assumptions NM.EaMergeSound.merge_unifies.
Print Assumptions NM.EaMergeSound.needs_exact.

From NM Require EaContextTableSound.
Print Assumptions NM.EaContextTableSound.context_table_computed.
Print Assumptions NM.EaContextTableSound.assume_ctx_sound.
Print Assumptions NM.EaContextTableSound.fun_ctx_sound.

From NM Require EaOutputSound.
Print Assumptions NM.EaOutputSound.ctx_spec_items_sound.
Print Assumptions NM.EaOutputSound.output_brackets_sound.
Print Assumptions NM.EaOutputSound.output_item_entries_sound.
Print Assumptions NM.EaOutputSound.output_items_entries_sound.
Print Assumptions NM.EaOutputSound.provide_result_sound.
Print Assumptions NM.EaOutputSound.output_entries_sound_at_budget.
Print Assumptions NM.EaOutputSound.provide_result_sound_at_budget.
Print Assumptions NM.EaOutputSound.bundle_world_r_provide_result_sound.

From NM Require EaFamilySound.
Print Assumptions NM.EaFamilySound.family_result_sound.
Print Assumptions NM.EaFamilySound.is_family_shape_sound.
Print Assumptions NM.EaFamilySound.family_bracket_free_sound.
Print Assumptions NM.EaFamilySound.family_bracket_free_complete.
Print Assumptions NM.EaFamilySound.family_keyed_sound.
Print Assumptions NM.EaFamilySound.is_family_sound.
Print Assumptions NM.EaFamilySound.bundle_world_r_is_family_sound.
Print Assumptions NM.EaContextTableSound.installed_context_table_sound.


From NM Require EaContextReadFacts.
Print Assumptions NM.EaContextReadFacts.type_readers_context.
Print Assumptions NM.EaContextReadFacts.mentions_context.
Print Assumptions NM.EaContextReadFacts.entries_context.
Print Assumptions NM.EaContextReadFacts.root_ctx_context.
Print Assumptions NM.EaContextReadFacts.merge_context.
Print Assumptions NM.EaContextReadFacts.assume_ctx_context.
Print Assumptions NM.EaContextReadFacts.fun_ctx_context.
Print Assumptions NM.EaContextReadFacts.context_table_context.

Print Assumptions NM.EaContextTableSound.bundle_world_r_context_table_sound.

From NM Require EaUnifySound.
Print Assumptions NM.EaUnifySound.resolve_absorbed.
Print Assumptions NM.EaUnifySound.unify_absorbed.
Print Assumptions NM.EaUnifySound.merge_principal.
Print Assumptions NM.EaUnifySound.merge_function.
Print Assumptions NM.EaUnifySound.merge_sound.

From NM Require EaPatternSound.
Print Assumptions NM.EaPatternSound.member_with_head_sound.
Print Assumptions NM.EaPatternSound.field_pats_sound.
Print Assumptions NM.EaPatternSound.pat_of_sound.

From NM Require EaControlSound.
Print Assumptions NM.EaControlSound.fitsb_exact.

From NM Require AllocationFacts.
Print Assumptions NM.AllocationFacts.allocation_grows.
Print Assumptions NM.AllocationFacts.allocations_disjoint.
Print Assumptions NM.AllocationFacts.allocation_rename.
Print Assumptions NM.AllocationFacts.allocate_range.
Print Assumptions NM.AllocationFacts.frame_allocation_transition.
Print Assumptions NM.AllocationFacts.fresh_outputs_disjoint.
Print Assumptions NM.AllocationFacts.frame_allocation_compatible.
Print Assumptions NM.AllocationFacts.copy_allocation_contract.
Print Assumptions NM.AllocationFacts.capture_allocation_contract.

From NM Require EaAllocationSound.
Print Assumptions NM.EaAllocationSound.fresh_n_range.
Print Assumptions NM.EaAllocationSound.fresh_table_range.
Print Assumptions NM.EaAllocationSound.fresh_tables_allocate.
Print Assumptions NM.EaAllocationSound.capture_counter_allocation.
Print Assumptions NM.EaAllocationSound.lambda_captures_from_counter.

From NM Require EaSatisfiesSound.
Print Assumptions NM.EaSatisfiesSound.pfind_exact.
Print Assumptions NM.EaSatisfiesSound.sat_map_exact.
Print Assumptions NM.EaSatisfiesSound.satisfies_gen_sound.

From NM Require EaAssumesSound.
Print Assumptions NM.EaAssumesSound.choose_assumption_atom_sound.
Print Assumptions NM.EaAssumesSound.assumption_theta_sound.
Print Assumptions NM.EaAssumesSound.assumes_complete_sound.
Print Assumptions NM.EaAssumesSound.elaborate_with_assumes_complete.

From NM Require EaCodeAllocationSound.
Print Assumptions NM.EaCodeAllocationSound.reserve_code_spec.
Print Assumptions NM.EaCodeAllocationSound.write_reserved_code_spec.
Print Assumptions NM.EaCodeAllocationSound.emit_code_spec.
Print Assumptions NM.EaCodeAllocationSound.assembled_code_lookup.
Print Assumptions NM.EaCodeAllocationSound.assembled_lambda_section.
Print Assumptions NM.EaCodeAllocationSound.assembled_link_origin.
From NM Require EaCodeEffects EaBindCodeEffects.
Print Assumptions NM.EaCodeEffects.lambda_provider_records_section.
Print Assumptions NM.EaBindCodeEffects.emit_linked_body_records_origin.
From NM Require EaAssemblyCodeEffects.
Print Assumptions NM.EaBindCodeEffects.cached_link_records_origin.
Print Assumptions NM.EaAssemblyCodeEffects.assembly_link_records.

From NM Require EaApplicationSound.
Print Assumptions NM.EaApplicationSound.allocate_application_outputs_range.
Print Assumptions NM.EaApplicationSound.collides_only_as_sound.
Print Assumptions NM.EaApplicationSound.apply_abstract_sound.

From NM Require EaForwarderSound.
Print Assumptions NM.EaForwarderSound.read_forwarder_sound.
Print Assumptions NM.EaForwarderSound.place_forwarder_spec.
Print Assumptions NM.EaForwarderSound.abstract_provider_sound.

From NM Require EaFamilySound.
Print Assumptions NM.EaFamilySound.family_application_sound.

From NM Require EaCodeEffects.
Print Assumptions NM.EaCodeEffects.code_transition_extends.
Print Assumptions NM.EaCodeEffects.nested_code_preserves_reserved_slot.
Print Assumptions NM.EaCodeEffects.fn_provider_code_effect.
Print Assumptions NM.EaCodeEffects.lambda_provider_code_effect.
Print Assumptions NM.EaCodeEffects.meth_provider_code_effect.
Print Assumptions NM.EaCodeEffects.apply_abstract_code_effect.
Print Assumptions NM.EaCodeEffects.fresh_table_code_effect.

From NM Require AllocationRenaming.
Print Assumptions NM.AllocationRenaming.fresh_batches_are_renamings.
Print Assumptions NM.AllocationRenaming.allocated_batches_alpha.

From NM Require EaBindCodeEffects.
Print Assumptions NM.EaBindCodeEffects.copy_interpretation_code_effect.
Print Assumptions NM.EaBindCodeEffects.link_body_code_effect.
Print Assumptions NM.EaBindCodeEffects.bind_items_preserve_code.

From NM Require EaExpressionCodeEffects EaAssemblyCodeEffects.
Print Assumptions NM.EaExpressionCodeEffects.expression_code_effects.
Print Assumptions NM.EaExpressionCodeEffects.run_code_effect.
Print Assumptions NM.EaAssemblyCodeEffects.assemble_preserves_code_bound.
Print Assumptions NM.EaAssemblyCodeEffects.assembled_code_lookup_exact.
