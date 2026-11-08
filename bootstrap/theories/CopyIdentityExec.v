(* Identity copies keep their actual existing interface.  Absence of a
   recomputed export profile records exactly that checked identity case. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound.
Section Copy.
Theorem copy_profile_none:forall mi site ef eg fg bg fixed fneeds gneeds s sf,
 (if Nat.eqb(gs_id fg)(gs_id bg)then ret None
  else let!view:=copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds in ret(Some view))s=Good(None,sf)->
 gs_id fg=gs_id bg/\sf=s.
Proof.
 intros mi site ef eg fg bg fixed fneeds gneeds s sf Run.
 destruct(Nat.eqb(gs_id fg)(gs_id bg))eqn:Same.
 - injection Run as <-;split;[now apply Nat.eqb_eq|reflexivity].
 - apply bind_inv in Run as(view&ss&View&Run);discriminate Run.
Qed.
Theorem copy_profile_some:forall mi site ef eg fg bg fixed fneeds gneeds s profile sf,
 (if Nat.eqb(gs_id fg)(gs_id bg)then ret None
  else let!view:=copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds in ret(Some view))s=Good(Some profile,sf)->
 gs_id fg<>gs_id bg/\copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds s=Good(profile,sf).
Proof.
 intros mi site ef eg fg bg fixed fneeds gneeds s profile sf Run.
 destruct(Nat.eqb(gs_id fg)(gs_id bg))eqn:Same;[discriminate Run|].
 apply bind_inv in Run as(view&ss&View&Run);injection Run as <- <-.
 split;[now apply Nat.eqb_neq|exact View].
Qed.
End Copy.
