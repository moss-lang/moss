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
From NM Require FastPath WorldSound ElabFrame ReqsTotal.
From NM Require XTyping XSafety XExample XHeapC.
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
  Definition prim_mlaw := MossX.prim_msound.
  Definition prim_ulaw := MossX.prim_usound.

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
   fresh, non-source symbols made; record fields numbered injectively; and
   the requirement lists it caches for tags and aliases are closed and least
   (toward ReqsReading's rr_closed, rr_least and rr_listed) *)
Definition elaboration_frame := ElabFrame.elaborate_frame.
(* the requirements the elaborator computes for a val, fn, method, context
   or functor are Part V's AssumeTypes (toward ReqsReading's rr_plain) *)
Definition assume_types_exact := WorldSound.load_world_assume_types.
(* and, steps 1-3 together: whenever the elaborator accepts a bundle, the
   loader's world with the requirement lists the elaborator cached (the
   least relevance-closed ones, and AssumeTypes, for the symbols it never
   asked about) and any injective field numbering is a WorldOf of it *)
Definition elaborate_world_of := ReqsTotal.elaborate_world_of.

(* The executable roots (Extract2.v extracts these). *)
Definition elaborate_sx := Certified.elaborate_sx.
Definition run_main := MossX.XS.run_main.

(* Track B: type safety of MossX itself (XTyping.v's judgment,
   XSafety.v's proof), for the interpreter above over the concrete
   primitives.  Not an obligation of Audit.v yet: it is the step towards
   accepted_never_err that does not depend on the elaboration relation.
   - x_typed_safe: a typed MossX expression, run in an environment agreeing
     with its context at a store typing the machine's GC heap agrees with,
     never produces XErr at any fuel; a value it produces has its type at
     an extension of that store typing, a `return` the fn's result type,
     and `break` only happens inside a loop.
   - x_run_main_safe: in a well-formed program (XTyping.xwf), `run_main`
     never produces XErr, given an initial environment agreeing with what
     main assumes. *)
Definition x_typed_safe := XSafety.mossx_typed_safe.
Definition x_run_main_safe := XSafety.mossx_run_main_safe.
(* Non-vacuity, as typed_inhabited for the core: a program that binds
   primitives, builds and reads a struct, loops, assigns and matches is
   well-formed (XExample.v). *)
Lemma x_typed_inhabited : exists Sg Rp P Fb, XTyping.xwf Sg Rp P Fb
  /\ exists main body, xp_fn P main = Some body
  /\ g_fn Sg main = Some ([], mkSig [] TUnit, None).
Proof.
  exists XExample.XExample.Sg, XExample.XExample.Rp, XExample.XExample.P, XExample.XExample.Fb.
  split; [exact XExample.XExample.example_wf |].
  exists XExample.XExample.fMain, XExample.XExample.main_body. split; reflexivity.
Qed.

(* And on the GC heap: a well-formed program whose main binds the `Ref`
   primitives at Ref[I32] (typed by their shapes, XTyping.gc_shape) and,
   run, allocates a box and writes it (XExample.XExampleRef). *)
Lemma x_heap_inhabited : exists Sg Rp P Fb, XTyping.xwf Sg Rp P Fb
  /\ exists main body st0, xp_fn P main = Some body
  /\ g_fn Sg main = Some ([], mkSig [] TUnit, None)
  /\ MossX.XS.xr_out (MossX.XS.run_main 30 P main [] st0) = XOk XVUnit
  /\ MossX.ms_heap (MossX.XS.xr_st (MossX.XS.run_main 30 P main [] st0)) <> HTrie.TE.
Proof.
  exists XExample.XExampleRef.Sg, XExample.XExampleRef.Rp, XExample.XExampleRef.P,
    XExample.XExampleRef.Fb.
  split; [exact XExample.XExampleRef.ref_wf |].
  exists XExample.XExampleRef.fMain, XExample.XExampleRef.main_body, XExample.XExampleRef.st0.
  destruct XExample.XExampleRef.ref_runs as [Ho Hh].
  split; [reflexivity|]. split; [reflexivity|]. split; [exact Ho|].
  rewrite Hh. discriminate.
Qed.

(* Track B: MossX's concrete GC heap (tries, HTrie.v) satisfies the heap law
   XHeap.HEAP_LAWS, without the kernel's array axioms. *)
Definition x_heap_mono := XHeapC.ConcreteHeap.heap_mono.
Definition x_heap_empty := XHeapC.ConcreteHeap.heap_empty.
Definition x_heap_other := XHeapC.ConcreteHeap.heap_other.
Definition x_heap_gc := XHeapC.ConcreteHeap.heap_gc.
