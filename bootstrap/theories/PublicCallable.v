(* Generic contracts for values callable by the language interpreter.
   This module depends on the interpreter and value typing, not the body
   safety induction or a platform's primitive signatures. *)
From Stdlib Require Import List Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit PublicValueTyping PublicLocalsTyping.
Import AuditBase.Semantics ListNotations.

Module Callable (Prims : PRIMITIVES) (I0 : INTERP Prims).
Section Contracts.
Variables (W : Elaboration.world) (P : program_view).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

(* What a call of a closure may produce, read at the caller's types. *)
Definition CallOutcome (H : heap_types) (t : ty) (r : I0.evaluation) : Prop :=
  match r with
  | I0.Evaluation signal _ st =>
      match signal with
      | Normal w | Returning w => exists H', heap_extends H H' /\ state_ok H' st /\ value_typed W P H' w t
      | Stuck _ | Breaking => False
      | _ => True
      end
  end.
Definition PrimOutcome (H : heap_types) (t : ty) (r : signal * Prims.mstate) : Prop :=
  match r with
  | (signal, st) =>
      match signal with
      | Normal w => exists H', heap_extends H H' /\ state_ok H' st /\ value_typed W P H' w t
      | Trap _ | Exit _ | UnsupportedOperation _ | OutOfFuel => True
      | _ => False
      end
  end.

(* A closure is callable below fuel n: fuel only decreases, so a closure
   available at fuel n is only ever invoked at a smaller fuel. The ghost
   heap only grows, so it is invoked at an extension of the heap H0 it was
   available at, where its captured values are typed. *)
Definition CallableOk (n : nat) (H0 : heap_types) (d : environment) (params : list ty) (ret : ty) (pv : provision_value) : Prop :=
  match pv with
  | FunctionClosure f df => exists body, view_function P f = Some body /\
      forall j, j < n -> forall H st ws, heap_extends H0 H -> state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        CallOutcome H (ground d ret) (I0.eval j P df (parameter_values ws) st body)
  | PrimitiveClosure p held => forall H st ws, heap_extends H0 H -> state_ok H st ->
        Forall2 (fun w t => value_typed W P H w (ground d t)) ws params ->
        PrimOutcome H (ground d ret) (I0.run_prim_at P p held ws st)
  | _ => False
  end.

Lemma extends_trans : forall H1 H2 H3,
  heap_extends H1 H2 -> heap_extends H2 H3 -> heap_extends H1 H3.
Proof. unfold heap_extends; auto. Qed.

Lemma callable_lower : forall n m H0 d params ret pv, m <= n ->
  CallableOk n H0 d params ret pv -> CallableOk m H0 d params ret pv.
Proof.
  intros n m H0 d params ret [| f df | |] Le Ok; cbn in *; auto.
  destruct Ok as [body [View Run]]. exists body; split; [exact View|].
  intros j Lt; apply Run; lia.
Qed.


Lemma callable_extends : forall n H H' d params ret pv, heap_extends H H' ->
  CallableOk n H d params ret pv -> CallableOk n H' d params ret pv.
Proof.
  intros n H H' d params ret [| f df | |] Ext Ok; cbn in *; auto.
  - destruct Ok as [body [View Run]]. exists body; split; [exact View|].
    intros j Lt Hc st ws E; apply Run; [exact Lt|exact (extends_trans _ _ _ Ext E)].
  - intros Hc st ws E; apply Ok; exact (extends_trans _ _ _ Ext E).
Qed.

Lemma callable_lower_extends : forall n m H0 H1 d params ret pv, m <= n -> heap_extends H0 H1 ->
  CallableOk n H0 d params ret pv -> CallableOk m H1 d params ret pv.
Proof.
  intros n m H0 H1 d params ret pv Le Ext Ok.
  exact (callable_extends _ _ _ _ _ _ _ Ext (callable_lower _ _ _ _ _ _ _ Le Ok)).
Qed.

End Contracts.
End Callable.
