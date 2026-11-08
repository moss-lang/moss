(* A cached initializer's IR is reused at several source reads. Its binding
   annotations must occur at every corresponding position in the readers. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Parse Elab ReifyData.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.

Definition repeated_constant_source : string :=
  "fn q(); val c:()={bind q=()=>();()}; fn f(){c;c;} fn g(){c;} fn main(){f();g();}".
Definition repeated_constant_elaboration : res (xprogram * st) :=
  match parse_moss repeated_constant_source with
  | Good file => elaborate_with_metadata
      (Bundle "main.moss" "" [AModule "main.moss" false [] file])
  | Bad message => Bad message end.
Definition constant_function_annotations : nat :=
  match repeated_constant_elaboration with
  | Good (_,source) => fold_right (fun event count =>
      match event with REFunction _ _ _ _ _ _ => S count | _ => count end) 0
      (flat_map (fun p => rc_events ety key (snd p)) (rm_codes (st_reify source)))
  | Bad _ => 999 end.
Definition constant_annotation_cache_size : nat :=
  match repeated_constant_elaboration with
  | Good (_,source) => List.length (rm_constants (st_reify source))
  | Bad _ => 999 end.

Example constant_binding_trace_replayed_at_all_three_reads : constant_function_annotations = 3.
Proof. vm_compute. reflexivity. Qed.
Example constant_binding_trace_cached_once : constant_annotation_cache_size = 1.
Proof. vm_compute. reflexivity. Qed.
