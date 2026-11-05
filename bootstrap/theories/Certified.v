(* ------------------------------------------------------------------------ *)
(* Certified.v — binds the implementation to the audit contract (Audit.v,   *)
(* Part VI).  No checker or elaborator logic here: every operation and      *)
(* proof is an alias of something compiled and checked elsewhere.           *)
(*                                                                           *)
(* Bound:   Audit.PROVED_NOW, by `Certified` below.                          *)
(* Pending: the rest of Audit.COMPLETE, i.e. exactly                         *)
(*   - elaborate_sound      (elaborate b = Good p -> Elaborates b p); its    *)
(*                           first step, the loader's reading, is bound     *)
(*                           below (world_reads, WorldSound.v)              *)
(*   - elaborate_complete   (b elaborable -> elaborate b = Good _)           *)
(*   - accepted_never_err   (accepted programs never get stuck, MossX)       *)
(*   - elaborated_typed     (core-fragment output is Typed; the bridge from  *)
(*                           the elaborator to typed_safe)                   *)
(* There is no Admitted, Axiom or Parameter in this file, and COMPLETE is    *)
(* not claimed: `Certified` is ascribed PROVED_NOW only.                     *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List.
From NM Require Import Audit CoreMoss MossX Sx Elab Specialization RelFacts.
From NM Require FastPath WorldSound ElabFrame.
Import ListNotations.

Module Certified <: PROVED_NOW.
  Definition elaborate := Elab.elaborate.
  Definition elaborate_sx := Elab.elaborate_sx.
  Lemma elaborate_sx_reads : forall x,
    elaborate_sx x = match Input.read_bundle x with
                     | Input.Good b => elaborate b
                     | Input.Bad m => Input.Bad m
                     end.
  Proof. intro x. reflexivity. Qed.

  Definition eval_deterministic := CoreMoss.eval_deterministic.
  Definition interp_sound := CoreMoss.interp_sound.
  Definition interp_complete := CoreMoss.interp_complete.
  Definition spec_eval := Specialization.spec_eval.

  Module Prims := MossX.ConcretePrims.
  Module X := MossX.XS.
  Definition xinterp_conservative := MossX.xinterp_conservative.
  Definition prim_law := MossX.prim_sound.

  Definition Typed := CoreMoss.wf_prog.
  Lemma typed_safe : forall Sg main body fuel,
    Typed Sg main -> g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err.
  Proof. intros Sg main body fuel H Hm. exact (proj1 (@soundness_main Sg main body fuel H Hm)). Qed.
  Lemma typed_inhabited : exists Sg main u, Typed Sg main /\ g_fun Sg u <> None.
  Proof.
    exists FunctorExample.Sg, FunctorExample.fMain, 1. split.
    - exact functor_example_ok.
    - intro H. vm_compute in H. discriminate H.
  Qed.

  Definition method_choice_unique := RelFacts.method_choice_unique.
End Certified.

(* Audit.v DRAFT NOTE 24, the family fast path: the oracle runs
   FastPath.fast_program of the elaborated program (each application of a
   platform family of primitives, `Memories`, keeping only the members
   used).  Proved: that program runs exactly as the elaborated one, at
   every fuel at which the elaborated one does not run out of fuel; and
   the default engine on it, pruned, computes the elaborated program's
   result unless that run runs out of fuel or gets stuck. *)
Definition fast_path := FastPath.fast_program.
Definition fast_path_agrees := FastPath.fast_run.
Definition fast_path_engine_agrees := FastPath.fast_engine_agrees.

(* Toward elaborate_sound (Audit.v, Part VI, COMPLETE), step 1: the static
   world the elaborator's loader reads a bundle as satisfies every condition
   of Part V's WorldOf about modules, imports, declarations, D44 exports and
   scopes and D61 attached methods (WorldSound.LoaderReading), whatever the
   requirements and field numbering; with those two (ReqsReading, an
   injective numbering) it is a WorldOf (world_of_reading).  The remaining
   conditions are WorldSound.v's section world_todo. *)
Definition world_reads := WorldSound.load_world_reads.
Definition elaborate_world_reads := WorldSound.elaborate_world_reads.
Definition world_of_reading := WorldSound.worldof_of_reading.
(* and every path the elaborator resolves in a module of that world, Part
   V's `resolve` resolves to the same entity *)
Definition world_resolves := WorldSound.load_world_resolves.
(* and what elaboration keeps of that world: its symbols, unchanged; only
   fresh, non-source symbols made; record fields numbered injectively *)
Definition elaboration_frame := ElabFrame.elaborate_frame.
(* the requirements the elaborator computes for a val, fn, method, context
   or functor are Part V's AssumeTypes (toward ReqsReading's rr_plain) *)
Definition assume_types_exact := WorldSound.load_world_assume_types.

(* The executable roots (Extract2.v extracts these). *)
Definition elaborate_sx := Certified.elaborate_sx.
Definition run_main := MossX.XS.run_main.
