(* Initial platform entries are transferred into main's immutable formal
   slots. Platform functors use the same real provider job mechanism. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab ReifyData Reify ReifySource ReifySig
  ReifyFunctorProfiles ReifyTransfers ReifyRoles ReifyInitial ReifyPlan ReifyFunctors ReifyProviders ReifyProgramWalk.
Import ListNotations.

Definition initial_environment_grounding (environment : xdenv) : subst := xdtypes environment.
Definition close_initial_captures (ground : subst) (arguments : subst) : xdenv :=
  map(fun p=>(ITy(fst p),XDType(ExecutionModel.app_subst ground(snd p))))arguments.
Definition platform_linked_items (view : reify_functor_view) : smap :=
  map(fun p=>(fst p,fst p))(rfv_input_items view).

Record initial_platform_result : Type := InitialPlatformResult {
  ipr_environment : xdenv;
  ipr_global : reify_state;
  ipr_functors : functor_work
}.
Fixpoint lower_platform_entries (source : st) (defaults : subst)
    (scope : reify_scope ety key) (ground : subst) (items : smap)
    (entries : xdenv) (global : reify_state) (work : functor_work)
    : option initial_platform_result :=
  match entries with
  | []=>Some(InitialPlatformResult[]global work)
  | (ITy _,XDType _)::rest=>lower_platform_entries source defaults scope ground items rest global work
  | (original,entry)::rest=>
      match lookup_smap original items with
      | None=>None
      | Some target=>
          let continue mapped after after_work:=
            match lower_platform_entries source defaults scope ground items rest after after_work with
            | Some result=>Some(InitialPlatformResult((target,mapped)::ipr_environment result)
                (ipr_global result)(ipr_functors result))
            | None=>None end in
          match original,target,entry with
          | IVal _,IVal _,XDVal value=>continue(XDVal value)global work
          | IFn _,IFn _,XDPrim primitive=>continue(XDPrim primitive)global work
          | IFun original,IFun slot,XDFun raw_code [] []=>
              match lookup_functor_view slot(fw_views work),source_functor_fixed source original with
              | Some view,Some fixed_names=>
                  match reserve_provider_job source defaults raw_code view fixed_names
                      (platform_linked_items view)[]global work,
                    reify_code_for_symbol source raw_code with
                  | Some(job,reserved,reserved_work),Some code=>
                      let caller:=ReifyState(rf_next reserved)(rf_names global)(rf_generations global)
                        items(rf_declarations reserved) in
                      match provider_fixed_capture_arguments(rfd_fixed(rfv_schema(rfj_view job)))
                          (rfd_fixed_inst(rfv_schema view)),
                        provider_capture_arguments source(rc_scope ety key code)(rfj_type_captures job)
                          (rs_names ety key scope)caller,
                        resolve_item_transfers(rfj_item_captures job)[]caller with
                      | Some fixed_args,Some private_args,Some[]=>
                          continue(XDFun(rfj_code job)[]
                            (close_initial_captures ground(fixed_args++private_args)))reserved reserved_work
                      | _,_,_=>None end
                  | _,_=>None end
              | _,_=>None end
          | _,_,_=>None end
      end
  end.

Definition prepare_initial_platform (source : st) (raw : xprogram)
    (prepared : prepared_function_stage) (bodies : reified_function_bodies)
    : option initial_platform_result :=
  let frame:=pfs_main prepared in
  let defaults:=rit_environment(pfs_initial prepared) in
  match initial_frame_type_entries source raw frame(rit_descriptors(pfs_initial prepared))with
  | None=>None
  | Some main_types=>
      let environment:=main_types++defaults in
      match lower_platform_entries source(pfs_defaults prepared)(rff_source_scope frame)
        (initial_environment_grounding environment)(rff_item_roles frame)(xo_env raw)
        (rfb_global bodies)(rfb_functors bodies)with
      | Some result=>Some(InitialPlatformResult(environment++ipr_environment result)
          (ipr_global result)(ipr_functors result))
      | None=>None end
  end.
