(* The closure obligations of `bind F = G;` (BFunClosure, LinkBody).

   - `LinkMergesOk` and `BindMergesOk` (formerly of PublicBodySlice) were
     false for programs both implementations accept:
     examples/linked-merge-captured.moss (G merges the captured Y into the
     linked X) and examples/bind-merge-unbound.moss (G merges Y, not in
     force where G is bound, into Z). Audit's `application_environment`
     now reads G's inputs through G's table (`fc_table`), so G's merges
     hold by construction and both obligations are gone
     (scratch/orch/repair-AUDIT.md).
   - `CapturedInForceOk` (`captured_in_force_ok` below): each of G's inputs
     F does not take is in force where G is bound, by rule B for the
     closure (BFunClosure). Before rule B, a captured need could mention a
     declared symbol out of force (examples/functor-captured-unbound.moss,
     now rejected), and `captured_mentioned` showed the closure's
     `lexical_capture` kept its meaning (scratch/orch/family4-AUDIT.md). *)
From Stdlib Require Import List Arith Bool.
From NM Require Import Audit.
From NM Require PublicValueTyping PublicBodySlice.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.


(* ---- CapturedInForceOk ------------------------------------------------------ *)

Module LinkMerges (Prims : PRIMITIVES) (I0 : PublicValueTyping.INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.

(* Immediate from rule B for the closure (BFunClosure): G's inputs that F
   does not take are in force where G is bound. *)
Theorem captured_in_force_ok : forall W, S.CapturedInForceOk W.
Proof. intros W C Cu Cg x Cap Ig Ou. left. exact (Cap x Ig Ou). Qed.
End LinkMerges.
