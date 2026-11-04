(* The certificate: everything Certified.v binds to Audit.v's PROVED_NOW,
   and the executable roots Extract2.v extracts.  Their assumptions are
   transitively everything the guarantee rests on, and check.sh requires
   them to be only the kernel's `Primitive` declarations (Rocq's primitive
   integers, floats and arrays): no Axiom, no Admitted. *)
From NM Require Certified.

Print Assumptions NM.Certified.Certified.elaborate.
Print Assumptions NM.Certified.Certified.elaborate_sx.
Print Assumptions NM.Certified.Certified.elaborate_sx_reads.
Print Assumptions NM.Certified.Certified.eval_deterministic.
Print Assumptions NM.Certified.Certified.interp_sound.
Print Assumptions NM.Certified.Certified.interp_complete.
Print Assumptions NM.Certified.Certified.spec_eval.
Print Assumptions NM.Certified.Certified.xinterp_conservative.
Print Assumptions NM.Certified.Certified.prim_law.
Print Assumptions NM.Certified.Certified.Typed.
Print Assumptions NM.Certified.Certified.typed_safe.
Print Assumptions NM.Certified.Certified.typed_inhabited.
Print Assumptions NM.Certified.Certified.method_choice_unique.
Print Assumptions NM.Certified.elaborate_sx.
Print Assumptions NM.Certified.run_main.
