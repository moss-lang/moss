(* The certificate: everything Certified.v binds to Audit.v's PROVED_NOW,
   and the executable roots Extract2.v extracts.  Their assumptions are
   transitively everything the guarantee rests on, and check.sh requires
   them to be only the kernel's `Primitive` declarations (Rocq's primitive
   integers, floats and arrays): no Axiom, no Admitted. *)
From NM Require Certified Parse FastPath Prune SlotX FoldX CloX.

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
Print Assumptions NM.Certified.fast_path.
Print Assumptions NM.Certified.fast_path_agrees.
Print Assumptions NM.Certified.fast_path_engine_agrees.
Print Assumptions NM.Certified.world_reads.
Print Assumptions NM.Certified.elaborate_world_reads.
Print Assumptions NM.Certified.world_of_reading.
Print Assumptions NM.Certified.world_resolves.

(* The front end moss_m2.ml runs on a source file: the Gallina lexer,
   parser and bundler (the driver only reads files), whose output the
   elaborator above reads. *)
Print Assumptions NM.Parse.load_bundle.
Print Assumptions NM.Parse.bundle_sx.

(* The default engine (MOSS_ENGINE=fold), as moss_m2.ml runs it: the family
   fast path, pruning, pre-resolution, folding and closure compilation,
   then the run; and the theorem that makes it Audit's semantics, given the tables the driver
   builds (README.md, What is trusted). *)
Print Assumptions NM.FastPath.fast_program.
Print Assumptions NM.Prune.oracle_program.
Print Assumptions NM.SlotX.shapes.
Print Assumptions NM.SlotX.compile_fns.
Print Assumptions NM.SlotX.compile_funs.
Print Assumptions NM.FoldX.fold_e.
Print Assumptions NM.FoldX.fold_b.
Print Assumptions NM.CloX.cc.
Print Assumptions NM.CloX.ccbs.
Print Assumptions NM.CloX.crun_main.
Print Assumptions NM.CloX.engine_agrees.
