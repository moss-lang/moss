(* Shared elaborator result for RelExamples. Keeping this computation in its
   own module avoids rerunning it when an individual relational proof changes.
   The public data definitions in RelExamples remain exact aliases. *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss MossX NMap Sx Elab RelSupport BxBridged.
Import ListNotations.
Import ExecutionModel.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ===================== tests/wasi/bridged.moss ========================== *)

Definition bridged := Eval vm_compute in run_elab bridged_sx.
Definition P_br : xprogram := match bridged with Good (p, _) => p | Bad _ => XProgram [] [] 0 0 0 0 [] [] [] [] [] false [] [] end.
Definition S_br : st := match bridged with Good (_, s) => s | Bad _ => dummy_st end.
Definition W_br : Elaboration.world := W_of S_br.
