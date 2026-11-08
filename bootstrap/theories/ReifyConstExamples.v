(* An actual successful source check, frame allocation, and immutable walk.
   The reader's extra type binding does not change cached initializer scope. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Parse Elab ReifyData Reify ReifySource ReifyInitial
  ReifyInterfaces ReifyPlan ReifyWalk ReifyFunctors ReifyWalkData ReifyProgramWalk.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition constant_scope_source : string :=
  "type T;tag A;fn q();val v:()={bind q=()=>();};fn main(){bind T=A;v;v;}".
Definition constant_scope_elaboration : res (xprogram * st) :=
  match parse_moss constant_scope_source with
  | Good file => elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])
  | Bad message => Bad message end.
Definition constant_scope_specifications (source : st) :=
  flat_map (fun pair => let code := snd pair in
    match rc_owner ety key code,rc_signature ety key code with
    | ROFunction f,Some signature => [(f,rc_scope ety key code,signature)]
    | _,_ => [] end) (rm_codes (st_reify source)).
Definition constant_scope_lowering : option (xexpr * walk_cursor) :=
  match constant_scope_elaboration with
  | Bad _ => None
  | Good (raw,source) =>
      let initial := lower_global_defaults source in
      let defaults := sti_roles (rit_interface initial) in
      let '(plain,global) := prepare_function_frames source
        (constant_scope_specifications source) (sti_state (rit_interface initial)) in
      let frames := map (attach_function_defaults defaults) plain in
      match lookup_function_frame (xo_main raw) frames,
        assoc reify_owner_eqb (ROFunction (xo_main raw)) (rm_codes (st_reify source)),
        assoc Nat.eqb (xo_main raw) (xo_fns raw) with
      | Some frame,Some code,Some body =>
          reify_expression (WalkConfig source defaults frames code) body
            (WalkCursor (ReifyLocal (enter_function_frame frame global) [])
              empty_functor_work [])
      | _,_,_ => None end
  end.
Example actual_cached_initializer_lowers : constant_scope_lowering <> None.
Proof. vm_compute. discriminate. Qed.

Definition constant_full_function_stage : option reified_function_bodies :=
  match constant_scope_elaboration with
  | Bad _=>None
  | Good(raw,source)=>match prepare_function_stage source raw with
      | Some prepared=>walk_prepared_functions source raw prepared
      | None=>None end
  end.
Example actual_cached_initializer_full_function_stage : constant_full_function_stage<>None.
Proof. vm_compute. discriminate. Qed.
