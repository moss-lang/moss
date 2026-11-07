(* The theorems whose assumptions the design notes and the files' headers
   cite: check.sh requires each to be "Closed under the global context",
   not even assuming Rocq's primitives.
   - CoreMoss.v's header and semantics.md [D78] ("Mechanized"):
     determinism, interpreter adequacy, phase separation, resolution
     decidability and type soundness;
   - semantics.md [D84] ("Why it is sound": "every theorem closes under the
     global context"): the theorems it names, in CoreMoss.v, D84.v and
     Specialization.v;
   - semantics.md [D71] ("Why it is sound": "no Admitted, no axioms beyond
     the development's"): Narrowing.v's three. *)
From NM Require CoreMoss D84 Specialization Narrowing DumpRoundTrip.
From NM Require InputGenerationAllocate InputGenerationRuntime.

(* [D78], CoreMoss.v's header *)
Print Assumptions NM.CoreMoss.eval_deterministic.
Print Assumptions NM.CoreMoss.interp_sound.
Print Assumptions NM.CoreMoss.interp_complete.
Print Assumptions NM.CoreMoss.phase_separation.
Print Assumptions NM.CoreMoss.availability_decidable.
Print Assumptions NM.CoreMoss.method_resolution_functional.
Print Assumptions NM.CoreMoss.safety.
Print Assumptions NM.CoreMoss.soundness_main.

(* [D84] *)
Print Assumptions NM.D84.held_narrow_rejected.
Print Assumptions NM.CoreMoss.capture_at_bind.
Print Assumptions NM.CoreMoss.capture_through_copy.
Print Assumptions NM.CoreMoss.polymorphic_provider_fixed.
Print Assumptions NM.CoreMoss.fixed_inputs_never_stale.
Print Assumptions NM.CoreMoss.res_coherence_needed.
Print Assumptions NM.D84.on_demand_elaboration.
Print Assumptions NM.D84.on_demand_theta_determined.
Print Assumptions NM.D84.arrays_on_demand_ok.
Print Assumptions NM.D84.family_fn_result_incoherent.
Print Assumptions NM.D84.family_val_placement.
Print Assumptions NM.D84.family_apply_placement.
Print Assumptions NM.CoreMoss.fixed_ok_decidable.
Print Assumptions NM.CoreMoss.res_coherent_decidable.
Print Assumptions NM.CoreMoss.pins_hold_decidable.
Print Assumptions NM.CoreMoss.capture_fixed_decidable.
Print Assumptions NM.CoreMoss.identity_application_conservative.
Print Assumptions NM.CoreMoss.empty_capture_conservative.
Print Assumptions NM.CoreMoss.identity_keys_conservative.
Print Assumptions NM.Specialization.spec_typed.
Print Assumptions NM.Specialization.specialization.

(* [D71] *)
Print Assumptions NM.Narrowing.vty_narrow_invariant.
Print Assumptions NM.Narrowing.wf_ty_narrowed.
Print Assumptions NM.Narrowing.narrowed_soundness.

Print Assumptions NM.DumpRoundTrip.dump_reads.

(* Private input scopes retain generation meanings across source rebinding. *)
Print Assumptions NM.InputGenerationMerge.private_rebind_read.
Print Assumptions NM.InputGenerationPlan.link_input_canonical_read.
Print Assumptions NM.InputGenerationPlan.source_frame_name_readback.
Print Assumptions NM.InputGenerationAllocate.reserved_link_input_plan.
Print Assumptions NM.InputGenerationAllocate.reserved_input_symbols.
Print Assumptions NM.InputGenerationAllocate.reserved_private_scope.
Print Assumptions NM.InputGenerationRuntime.old_input_survives_output_shadowing.
Print Assumptions NM.InputGenerationRuntime.private_capture_grounding_survives_caller_rebinding.
