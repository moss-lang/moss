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
