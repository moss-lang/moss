(* Source checking order and final IR nesting order can differ: demand
   preludes wrap an already synthesized receiver. Events are annotations,
   not a linear token stream for a structural IR traversal. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Parse Elab ReifyData.
Import ListNotations ExecutionModel.Input.
Local Open Scope string_scope.
Definition reordered_source : string :=
  "tag T;fn q();fn T.m();functor F[]:->T.m;functor G:->T.m{bind T.m=()=>();}fn main(){bind F=G;({bind q=()=>();T}).m();}".
Definition reordered_elaboration : res (xprogram * st) :=
  match parse_moss reordered_source with
  | Good file => elaborate_with_metadata (Bundle "main.moss" "" [AModule "main.moss" false [] file])
  | Bad message => Bad message end.
Definition event_binding_kind (event : reify_event ety key) : list nat :=
  match event with
  | REFunction _ _ _ _ _ _ => [1]
  | REApply _ _ _ _ _ _ _ _ => [2]
  | REFunctor _ _ _ _ _ _ _ => [3]
  | _ => [] end.
Fixpoint raw_binding_order (e : xexpr) : list nat :=
  match e with
  | XBindFun _ _ _ _ b => 3 :: raw_binding_order b
  | XApply _ _ _ b => 2 :: raw_binding_order b
  | XBindFn _ _ _ b => 1 :: raw_binding_order b
  | XMeth r _ _ _ => raw_binding_order r
  | XSeq a b | XLet _ a b => raw_binding_order a ++ raw_binding_order b
  | _ => [] end.
Definition reordered_events : list nat :=
  match reordered_elaboration with
  | Good (p,s) => match assoc reify_owner_eqb (ROFunction (xo_main p)) (rm_codes (st_reify s)) with
    | Some code => flat_map event_binding_kind (rev (rc_events ety key code))
    | None => [] end
  | Bad _ => [] end.
Definition reordered_ir : list nat :=
  match reordered_elaboration with
  | Good (p,_) => match assoc Nat.eqb (xo_main p) (xo_fns p) with
    | Some body => raw_binding_order body | None => [] end
  | Bad _ => [] end.
Example checking_order_has_receiver_before_demand : reordered_events = [3;1;2].
Proof. vm_compute. reflexivity. Qed.
Example ir_nesting_has_demand_before_receiver : reordered_ir = [3;2;1].
Proof. vm_compute. reflexivity. Qed.
