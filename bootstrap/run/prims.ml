(* Keep the primitive context fixed for the run. The extracted host step
   needs only this context and the primitive-managed state. *)
let host : ExecutionModel.xprog option ref = ref None

let initialize view = host := Some (BaseHostScalars.legacy_view view)

module P = struct
  type mstate = MossX.mstate
  let raw_primitive _ operation values state =
    let context = match !host with Some context -> context | None -> assert false in
    BaseHost.primitive_in context operation values state
  let run_prim view operation site values state =
    AuditBase.BaseCall.call raw_primitive view operation site values state
end
