(* The operational body-lowering iterator, independent of its histories. *)
From Stdlib Require Import Bool.
From NM Require Import ExecutionModel Elab.
Definition lower_one(fuel id:nat):M unit:=
 let! g:=sym id in
 if is_some(fn_body g)&&(kind_eqb(gs_kind g)KFn||kind_eqb(gs_kind g)KMethod)
 then lower_body fuel g else ret tt.
