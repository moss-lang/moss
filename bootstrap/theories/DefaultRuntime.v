(* The fixed public interface used by the executable's primitive runtime.
   Arguments and the file snapshot belong to Model.state, not the interface. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel MossX XHeapC.

Module Host <: HOST.
  Definition state := MossX.mstate.
  Definition primitive := MossX.run_prim.
  Definition heap_ok := XHeapC.ConcreteHeap.heap_ok.
End Host.
Module Model := Public Host.
Definition default_interface := Model.default_interface.

Theorem default_interface_laws : forall P,
  Model.interface_laws P default_interface.
Proof.
  intro P. unfold Model.interface_laws, default_interface, Model.default_interface.
  cbn [Model.primitive Model.supports Model.heap_ok].
  split.
  - intros operation inputs result values state _ Signature Typed.
    exact (@MossX.prim_sound P operation inputs result values state Signature Typed).
  - split.
    + intros operation inputs result values state _ Signature Typed.
      exact (@MossX.prim_msound P operation inputs result values state Signature Typed).
    + split.
      * intros code values state _. exact (@MossX.prim_usound P code values state).
      * split; [exact XHeapC.ConcreteHeap.heap_mono|].
        split; [exact XHeapC.ConcreteHeap.heap_empty|].
        split.
        -- intros operation values state typing _.
           exact (@XHeapC.ConcreteHeap.heap_other P operation values state typing).
        -- intros operation values state typing predicate _.
           exact (@XHeapC.ConcreteHeap.heap_gc P operation values state typing predicate).
Qed.

Theorem default_compatible : forall program,
  Platform.Ready program -> Model.Compatible program default_interface.
Proof.
  intros program Ready. split; [exact Ready|].
  split; [intros; reflexivity|]. intros; apply default_interface_laws.
Qed.
