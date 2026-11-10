(* Signature metadata retained for the legacy lowering records. *)
From Stdlib Require Import List.
From NM Require Export ExecutionModel.

Record fnsig : Type := mkSig { fs_params : list ty; fs_ret : ty }.
