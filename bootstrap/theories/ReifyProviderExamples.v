From Stdlib Require Import List String Uint63 PArray.
From NM Require Import ExecutionModel Parse Elab ReifyData ReifyProgramWalk ReifyFunctors ReifyJobs Reify ReifyAssemble ReifyInitial ReifyPlan MossX CoreMoss HTrie.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition provider_example_source : string :=
  "fn q();functor F[]:->q;functor G:->q{bind q=()=>();}fn main(){bind F=G;bind F;q();}".
Definition provider_example_stage : option reified_function_bodies :=
  match parse_moss provider_example_source with
  | Bad _=>None
  | Good file=>match elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])with
      | Bad _=>None
      | Good(raw,source)=>match prepare_function_stage source raw with
          | None=>None
          | Some prepared=>walk_prepared_functions source raw prepared
          end
      end
  end.
Example actual_defined_provider_binding_and_application_lower :
  provider_example_stage<>None.
Proof. vm_compute. discriminate. Qed.
Example actual_defined_provider_code_reserved_once :
  option_map(fun result=>List.length(fw_jobs(rfb_functors result)))provider_example_stage=Some 1.
Proof. vm_compute. reflexivity. Qed.

Definition provider_example_compiled : option compiled_provider_jobs :=
  match parse_moss provider_example_source with
  | Bad _=>None
  | Good file=>match elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])with
      | Bad _=>None
      | Good(raw,source)=>match prepare_function_stage source raw with
          | None=>None
          | Some prepared=>match walk_prepared_functions source raw prepared with
              | None=>None
              | Some bodies=>compile_all_provider_jobs source(pfs_defaults prepared)(pfs_frames prepared)
                  (rfb_global bodies)(rfb_functors bodies)
              end
          end
      end
  end.
Example actual_defined_provider_body_compiles : provider_example_compiled<>None.
Proof. vm_compute. discriminate. Qed.
Example actual_defined_provider_no_pending_jobs :
  option_map(fun compiled=>next_provider_job(cpj_work compiled))provider_example_compiled=Some None.
Proof. vm_compute. reflexivity. Qed.

Definition provider_example_runtime : option xprogram :=
  match parse_moss provider_example_source with
  | Bad _=>None
  | Good file=>match elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])with
      | Bad _=>None
      | Good(raw,source)=>match prepare_function_stage source raw with
          | None=>None
          | Some prepared=>match walk_prepared_functions source raw prepared with
              | None=>None
              | Some bodies=>match compile_all_provider_jobs source(pfs_defaults prepared)(pfs_frames prepared)
                  (rfb_global bodies)(rfb_functors bodies)with
                  | None=>None
                  | Some jobs=>match compile_adapter_tables(rf_declarations(cpj_global jobs))
                      (fw_adapters(cpj_work jobs))with
                      | None=>None
                      | Some(adapters,exports,inputs)=>Some(rr_program(assemble_reified_program raw(rff_code(pfs_main prepared))
                          (rfb_functions bodies++cpj_functions jobs)(cpj_functors jobs++adapters)
                          (rit_environment(pfs_initial prepared))
                          (map(fun job=>(rfj_code job,tyreqs(rfd_inputs(rfv_schema(rfj_view job)))))
                            (fw_jobs(cpj_work jobs))++inputs)
                          (cpj_exports jobs++exports)(rf_declarations(cpj_global jobs))))
                      end
                  end
              end
          end
      end
  end.
(* No primitive reads memory in this program; a compact empty machine
   avoids evaluating init_state's intentionally enormous loop_fuel in VM. *)
Definition provider_example_state : MossX.mstate :=
  MossX.MS(PArray.make 1(PArray.make 1 0%uint63))0%uint63[][]HTrie.TE 0%uint63[]4%uint63.
Example actual_assembled_defined_provider_runs :
  option_map(fun program=>MossX.XS.xr_out(MossX.XS.run_main 100(program_of program[](fun _=>None))
    (xo_main program)(xo_env program)provider_example_state))provider_example_runtime=
    Some(XOk XVUnit).
Proof.
  let compiled:=eval vm_compute in provider_example_runtime in
  change (option_map(fun program=>MossX.XS.xr_out(MossX.XS.run_main 100
    (program_of program[](fun _=>None))(xo_main program)(xo_env program)provider_example_state))
    compiled=Some(XOk XVUnit)).
  reflexivity.
Qed.

(* Concrete examples below deliberately have no initial platform provisions.
   Their globals consist only of the external immutable type descriptors. *)
Definition assemble_empty_platform_example (text : string) : option xprogram :=
  match parse_moss text with
  | Bad _=>None
  | Good file=>match elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])with
      | Bad _=>None
      | Good(raw,source)=>match xo_env raw,prepare_function_stage source raw with
          | [],Some prepared=>match walk_prepared_functions source raw prepared with
              | None=>None
              | Some bodies=>match compile_all_provider_jobs source(pfs_defaults prepared)(pfs_frames prepared)
                  (rfb_global bodies)(rfb_functors bodies)with
                  | None=>None
                  | Some jobs=>match compile_adapter_tables(rf_declarations(cpj_global jobs))
                      (fw_adapters(cpj_work jobs))with
                      | None=>None
                      | Some(adapters,exports,inputs)=>Some(rr_program(assemble_reified_program raw(rff_code(pfs_main prepared))
                          (rfb_functions bodies++cpj_functions jobs)(cpj_functors jobs++adapters)
                          (rit_environment(pfs_initial prepared))
                          (map(fun job=>(rfj_code job,tyreqs(rfd_inputs(rfv_schema(rfj_view job)))))
                            (fw_jobs(cpj_work jobs))++inputs)
                          (cpj_exports jobs++exports)(rf_declarations(cpj_global jobs))))
                      end
                  end
              end
          | _,_=>None end
      end
  end.
Definition manifest_provider_program : option xprogram := assemble_empty_platform_example
  "type T;tag Old;assume T{fn q():T;}functor F[]:T->q;functor G:T->q{bind T=Old;bind q=()=>Old;}fn main(){bind T=Old;bind F=G;bind F;match q(){Old=>()};}".
Definition captured_value_provider_program : option xprogram := assemble_empty_platform_example
  "type T;tag Old;assume T{val x:T;fn q():T;}functor F[]:T->q;functor G:T,x->q{bind q=()=>x;}fn main(){bind T=Old;bind x=Old;bind F=G;bind F;match q(){Old=>()};}".
Definition example_program_outcome (program : xprogram) : xout :=
  MossX.XS.xr_out(MossX.XS.run_main 100(program_of program[](fun _=>None))
    (xo_main program)(xo_env program)provider_example_state).
Example manifest_provider_preserves_fixed_type :
  option_map example_program_outcome manifest_provider_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in manifest_provider_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
Example captured_provider_preserves_value_type :
  option_map example_program_outcome captured_value_provider_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in captured_value_provider_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.

Definition copied_provider_program : option xprogram := assemble_empty_platform_example
  "fn q();functor F[]:->q;functor H[]:->q;functor G:->q{bind q=()=>();}fn main(){bind H=G;bind F=H;bind F;q();}".
Example copied_provider_uses_actual_adapter :
  option_map example_program_outcome copied_provider_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in copied_provider_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
Definition copied_fixed_provider_program : option xprogram := assemble_empty_platform_example
  "type T;tag Old;assume T{val x:T;fn q():T;}functor F[]:T->q;functor H[]:T->q;functor G:T,x->q{bind q=()=>x;}fn main(){bind T=Old;bind x=Old;bind H=G;bind F=H;bind F;match q(){Old=>()};}".
Example copied_fixed_provider_keeps_quoted_capture :
  option_map example_program_outcome copied_fixed_provider_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in copied_fixed_provider_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
Definition copied_polymorphic_provider_program : option xprogram := assemble_empty_platform_example
  "type T;tag Old;assume T{val x:T;fn q():T;}functor F[T]:T,x->q;functor H[T]:T,x->q;functor G:T,x->q{bind q=()=>x;}fn main(){bind H=G;bind F=H;bind T=Old;bind x=Old;bind F;match q(){Old=>()};}".
Example copied_polymorphic_provider_uses_future_input :
  option_map example_program_outcome copied_polymorphic_provider_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in copied_polymorphic_provider_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.

Definition dormant_generic_child_program : option xprogram := assemble_empty_platform_example
 "fn q();val x:();functor Child[]:->q;functor Outer[]:Child->x;functor Pass:Child->x{bind x=();}fn dormant(){bind Outer=Pass;}fn main(){}".
Example dormant_provider_does_not_demand_unprovided_child_schema :
 option_map example_program_outcome dormant_generic_child_program=Some(XOk XVUnit).
Proof.
 let compiled:=eval vm_compute in dormant_generic_child_program in
 change(option_map example_program_outcome compiled=Some(XOk XVUnit));reflexivity.
Qed.
