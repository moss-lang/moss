(* Check first allocation and cache reuse for the same parsed source provider. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaBind EaMain EaCtx NMap ElabWorldExample.
From NM Require ElabWorld.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Local Open Scope string_scope.

Definition forwarder_source_text : string := "fn original();
assume original { fn reader() {} }
fn main() {}
".

Definition checks_reused_forwarder : bool :=
  match bundle_world_r (bundle_of (load_one forwarder_source_text)) with
  | Err _ => false
  | Ok (W0, bound) =>
    let p := "/lib/main.moss" in
    let modules := [p] in
    let mains := main_symbols W0 modules in
    let metadata := metadata_fuel W0 bound in
    let E0 := Env W0 default_strategy modules mains elab_fuel metadata in
    let W := ElabWorld.with_context W0 (nget (context_table E0 bound)) in
    let E := Env W default_strategy modules mains elab_fuel metadata in
    match resolve_sym W p ["original"], resolve_sym W p ["reader"] with
    | Some input, Some reader =>
      match w_context W reader with
      | Some (C, _) =>
        let F := Frame p None None false [] [] [] [] in
        let sig := FSig [] TUnit in
        let run := fn_provider E "fn" F C ["original"] sig in
        match read_forwarder E "fn" C input sig, run (init_state bound) with
        | Ok handle, Ok ((number, _, _), first) =>
          match run first, nget (es_fns first) number, nget (es_fwd first) input with
          | Ok ((reused, _, _), second), Some (EInvoke called []), Some slot =>
            (forwarder_source handle =? input)%nat && (called =? input)%nat &&
            (number =? bound)%nat && (reused =? number)%nat &&
            (slot_index slot =? number)%nat &&
            (es_next first =? S bound)%nat && (es_next second =? es_next first)%nat
          | _, _, _ => false
          end
        | _, _ => false
        end
      | None => false
      end
    | _, _ => false
    end
  end.

Example source_typed_once_allocates_then_reuses_forwarder : checks_reused_forwarder = true.
Proof. vm_compute. reflexivity. Qed.
