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
