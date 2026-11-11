(* Exercise the assumption check through parsed Moss and the actual context table. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaAssumptionAtoms EaMain EaCtx NMap ElabWorldExample.
From NM Require ElabWorld.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration ListNotations.
Local Open Scope string_scope.

Definition method_source : string := "type X;
type Y;
type Receiver;
assume X = Y { fn .m(); }
assume Receiver, Receiver.m { fn unused() {} }
fn main() {}
".

Definition method_needs_source : string := "type X;
type Y;
type Receiver;
fn .dep();
assume X = Y, X.dep { fn .m(); }
assume Receiver, Receiver.m { fn unused() {} }
fn main() {}
".

Definition method_structured_source : string := "type X;
type Y;
type Box;
type Shape = {field: X};
type Receiver;
assume X = Y, Box = Shape { fn .m(); }
assume Receiver, Receiver.m { fn unused() {} }
fn main() {}
".

Definition assumption_observation (strat : strategy) (src : string) :=
  match bundle_world_r (bundle_of (load_one src)) with
  | Err _ => None
  | Ok (W0, bound) =>
    let mods := ["/lib/main.moss"] in
    let mains := main_symbols W0 mods in
    let fuel := metadata_fuel W0 bound in
    let E0 := Env W0 strat mods mains elab_fuel fuel in
    let W := ElabWorld.with_context W0 (nget (context_table E0 bound)) in
    match w_scope W "/lib/main.moss" NSDetached "m",
          resolve_sym W "/lib/main.moss" ["unused"],
          resolve_sym W "/lib/main.moss" ["Receiver"] with
    | Some (ESym m), Some u, Some recv =>
      match w_context W m, w_context W u with
      | Some (Cs, _), Some (C, ns) =>
        match capp C (mkey (TAbs recv) m) with
        | Some a =>
          let Cv := fold_left (fun C q => set_ty C (fst q) (snd q)) (known a) C in
          let E := Env W strat mods mains elab_fuel fuel in
          Some (bound, assumption_theta W "assuming .m" Cv Cs bound, assumed_complete E bound C ns)
        | None => None
        end
      | _, _ => None
      end
    | _, _, _ => None
    end
  end.

Definition reverse_strategy := Strategy (fun _ _ => false) (s_pick default_strategy).

Definition shared_fresh_atom (strat : strategy) : bool :=
  match assumption_observation strat method_source with
  | Some (bound, Ok [(_, TAbs x); (_, TAbs y)], Ok tt) =>
      (bound <=? x)%nat && (x =? y)%nat
  | _ => false
  end.

Example missing_class_uses_one_fresh_atom :
  shared_fresh_atom default_strategy = true /\ shared_fresh_atom reverse_strategy = true.
Proof. split; vm_compute; reflexivity. Qed.

Definition fresh_leaf_in_shape (strat : strategy) : bool :=
  match assumption_observation strat method_structured_source with
  | Some (bound, Ok [(_, TRecord [(_, TAbs field)]); (_, TAbs x); (_, TAbs y)], Ok tt) =>
      (bound <=? x)%nat && (x =? y)%nat && (x =? field)%nat
  | _ => false
  end.

Example structured_read_uses_the_same_fresh_atom :
  fresh_leaf_in_shape default_strategy = true /\ fresh_leaf_in_shape reverse_strategy = true.
Proof. split; vm_compute; reflexivity. Qed.

Definition missing_need_diagnostic (strat : strategy) :=
  option_map (fun '(_, _, result) => result) (assumption_observation strat method_needs_source).

Example missing_need_diagnostic_is_stable :
  let rejected := Some (Err "calling `.m` needs `X.dep`, which is not available in the context here") in
  missing_need_diagnostic default_strategy = rejected /\
  missing_need_diagnostic reverse_strategy = rejected.
Proof. split; vm_compute; reflexivity. Qed.
