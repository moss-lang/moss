(* Platform primitive bindings use the exact checked final source signature
   and frozen method key, rather than inferring a prototype from a name. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel CoreMoss Elab BodyTr ReifyData Reify ReifySource
  ReifyInterfaces ReifyBindings ReifyWalkData.
Import ListNotations.

Fixpoint primitive_method_signature (source : st) (scope : reify_scope ety key)
    (receiver : ty) (method : msym) (theta : subst)
    (methods : list ((ety * msym) * reify_signature ety))
    : option (reify_signature ety) :=
  match methods with
  | [] => None
  | ((raw_receiver,candidate),signature) :: rest =>
      if Nat.eqb method candidate &&
         ty_eqb receiver (tr (reify_field_number source) raw_receiver) &&
         subst_eqb theta (reify_snapshot_theta source scope (KyMth raw_receiver candidate))
      then Some signature
      else primitive_method_signature source scope receiver method theta rest
  end.
Definition prepare_primitive_binding (source : st) (code : reify_code ety key)
    (item : item) (primitive : prim) (before : reify_local)
    : option (xbind * reify_local) :=
  match rc_exports ety key code with
  | None => None
  | Some exports =>
      let scope := rx_actual ety key exports in
      match item with
      | IFn name =>
          match assoc Nat.eqb name (rs_functions ety key scope) with
          | None => None
          | Some signature =>
              let raw := annotation_raw_signature source signature in
              let deps := signature_type_inputs (lower_fn_signature (rl_runtime before) raw) in
              let '(slot,after) := lower_function_provision name deps raw (rl_runtime before) in
              Some (XBPrim (IFn slot) primitive,ReifyLocal after (rl_aliases before))
          end
      | IMth receiver method theta =>
          match primitive_method_signature source scope receiver method theta (rs_methods ety key scope) with
          | None => None
          | Some signature =>
              let raw := annotation_raw_signature source signature in
              let deps := signature_type_inputs (lower_fn_signature (rl_runtime before) raw) in
              let '(target,after) := lower_method_provision item receiver deps raw (rl_runtime before) in
              Some (XBPrim target primitive,ReifyLocal after (rl_aliases before))
          end
      | _ => None end
  end.
