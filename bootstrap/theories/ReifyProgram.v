(* Executable staged lowering. This function is deliberately not wired into
   the driver until its raw-success and typing theorems are complete. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel CoreMoss Sx Elab ReifyData Reify ReifyPlan ReifyFunctors
  ReifyProgramWalk ReifyPlatform ReifyJobs ReifyAssemble.
Import ListNotations.

Definition reify_program (source : st) (raw : xprogram) : option reify_result :=
  match prepare_function_stage source raw with
  | None=>None
  | Some prepared=>match walk_prepared_functions source raw prepared with
      | None=>None
      | Some bodies=>match prepare_initial_platform source raw prepared bodies with
          | None=>None
          | Some initial=>match compile_all_provider_jobs source(pfs_defaults prepared)(pfs_frames prepared)
              (ipr_global initial)(ipr_functors initial)with
              | None=>None
              | Some jobs=>match compile_adapter_tables(rf_declarations(cpj_global jobs))
                  (fw_adapters(cpj_work jobs))with
                  | None=>None
                  | Some(adapters,exports,inputs)=>
                      Some(assemble_reified_program raw(rff_code(pfs_main prepared))
                        (rfb_functions bodies++cpj_functions jobs)(cpj_functors jobs++adapters)
                        (ipr_environment initial)
                        (map(fun job=>(rfj_code job,tyreqs(rfd_inputs(rfv_schema(rfj_view job)))))
                          (fw_jobs(cpj_work jobs))++inputs)
                        (cpj_exports jobs++exports)(rf_declarations(cpj_global jobs)))
                  end
              end
          end
      end
  end.

Definition elaborate_runtime_candidate (bundle : bundle) : res reify_result :=
  match elaborate_with_metadata bundle with
  | Bad message=>Bad message
  | Good(raw,source)=>match reify_program source raw with
      | Some result=>Good result
      | None=>Bad "internal: immutable lowering failed"%string end
  end.

Lemma reify_program_assembly : forall source raw result,
  reify_program source raw=Some result->
  exists main functions functors environment inputs exports declarations,
    result=assemble_reified_program raw main functions functors environment inputs exports declarations.
Proof.
 intros source raw result H;unfold reify_program in H.
 destruct(prepare_function_stage source raw)as[prepared|];[|discriminate].
 destruct(walk_prepared_functions source raw prepared)as[bodies|];[|discriminate].
 destruct(prepare_initial_platform source raw prepared bodies)as[initial|];[|discriminate].
 destruct(compile_all_provider_jobs source(pfs_defaults prepared)(pfs_frames prepared)
   (ipr_global initial)(ipr_functors initial))as[jobs|];[|discriminate].
 destruct(compile_adapter_tables(rf_declarations(cpj_global jobs))(fw_adapters(cpj_work jobs)))
   as[[[adapters exports]inputs]|];[|discriminate].
 inversion H;subst;eexists;eexists;eexists;eexists;eexists;eexists;eexists;reflexivity.
Qed.

Lemma reify_program_runtime_metadata : forall source raw result,
  reify_program source raw=Some result->
  xo_immutable(rr_program result)=true /\
  xo_bool(rr_program result)=xo_bool raw /\
  xo_true(rr_program result)=xo_true raw /\
  xo_false(rr_program result)=xo_false raw /\
  xo_prims(rr_program result)=xo_prims raw /\
  xo_cast(rr_program result)=xo_cast raw.
Proof.
 intros source raw result H.
 destruct(reify_program_assembly _ _ _ H)as[main[functions[functors[environment[inputs[exports[declarations ->]]]]]]].
 repeat split;reflexivity.
Qed.
