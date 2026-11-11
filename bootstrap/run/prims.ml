(* Keep the primitive context fixed for the run. The extracted host step
   needs only this context and the primitive-managed state. *)
let host : ExecutionModel.xprog option ref = ref None

let initialize view = host := Some (BaseHostScalars.legacy_view view)

let context () = match !host with Some context -> context | None -> assert false

module P = struct
  type mstate = MossX.mstate
  let run_prim _ operation site values state =
    BaseHost.call_in (context ()) operation site values state
end

module Deferred = struct
  let run_prim_deferred _ operation site values state =
    BaseHost.call_in_deferred (context ()) operation
      (fun () -> InterpreterFast.force_held site) values state
end
