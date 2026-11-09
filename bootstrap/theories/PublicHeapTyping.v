(* Ghost heap types supply the predicates required by the public host laws.
   Allocation grows immutable type assignments and weakens old predicates
   using value-typing monotonicity. No concrete heap representation is used. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit PublicValueTyping.
Import Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Definition heap_add (H : heap_types) (fresh : address) (kind : bool) (held : ty) : heap_types :=
  fun address => if Nat.eqb address fresh then Some (kind, held) else H address.

Lemma heap_add_extends : forall H fresh kind held,
  H fresh = None -> heap_extends H (heap_add H fresh kind held).
Proof.
  intros H fresh kind held Absent address cell Found. unfold heap_add.
  destruct (Nat.eqb address fresh) eqn:Equal; [apply Nat.eqb_eq in Equal; subst; congruence|exact Found].
Qed.

Lemma heap_add_here : forall H fresh kind held,
  heap_add H fresh kind held fresh = Some (kind, held).
Proof. intros; unfold heap_add; rewrite Nat.eqb_refl; reflexivity. Qed.

Section HeapTyping.
Variables (W : A.world) (P : program_view).

Definition heap_predicates (H : heap_types) : Heap.hty :=
  fun address => match H address with
    | Some (kind, held) => Some (kind, fun value => value_typed W P H value held)
    | None => None end.

(* Exactly the predicate-weakening relation in Public.interface_laws. *)
Definition heap_weakens (strong weak : Heap.hty) : Prop :=
  forall address kind predicate, weak address = Some (kind, predicate) ->
    exists original, strong address = Some (kind, original) /\
      forall value, original value -> predicate value.

Theorem allocation_predicates : forall H fresh kind held extended,
  H fresh = None ->
  Heap.hext (heap_predicates H) extended ->
  extended fresh = Some (kind, fun value => value_typed W P H value held) ->
  heap_weakens extended (heap_predicates (heap_add H fresh kind held)).
Proof.
  intros H fresh kind held extended Absent Extends Fresh address cell predicate Found.
  unfold heap_predicates, heap_add in Found.
  destruct (Nat.eqb address fresh) eqn:Equal.
  - apply Nat.eqb_eq in Equal; subst address. injection Found as <- <-.
    exists (fun value => value_typed W P H value held). split; [exact Fresh|].
    intros value Typed. eapply value_typed_extends; [apply heap_add_extends; exact Absent|exact Typed].
  - destruct (H address) as [[oldkind oldheld]|] eqn:Old; [|discriminate].
    injection Found as <- <-.
    exists (fun value => value_typed W P H value oldheld). split.
    + apply Extends. unfold heap_predicates; rewrite Old; reflexivity.
    + intros value Typed. eapply value_typed_extends; [apply heap_add_extends; exact Absent|exact Typed].
Qed.

Theorem allocation_header : forall H fresh kind held,
  handle_typed (heap_add H fresh kind held) kind held (VCheckedHandle kind held fresh).
Proof. intros; constructor; apply heap_add_here. Qed.

End HeapTyping.

Module HeapSafety (Host : HOST) (M0 : PUBLIC Host).
Module M := M0.

Theorem box_preserves_heap : forall W P interface H state tag value held raw next,
  M.interface_laws P interface -> M.supports interface (PBox tag) = true ->
  M.heap_ok interface (heap_predicates W P H) state ->
  value_typed W P H value held ->
  M.primitive interface P (PBox tag) [value] state = (Normal raw, next) ->
  exists H' address,
    heap_extends H H' /\ M.heap_ok interface (heap_predicates W P H') next /\
    raw = VTag tag (VHandle address) /\
    handle_typed H' false held (VCheckedHandle false held address).
Proof.
  intros W P interface H state tag value held raw next Laws Supported Valid Typed Run.
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC (PBox tag) [value] state (heap_predicates W P H)
    (fun value => value_typed W P H value held) Supported Valid Typed).
  rewrite Run in GC.
  destruct GC as [extended [ValidNext [Extends [fresh [Raw [Absent Fresh]]]]]].
  assert (FreshH : H fresh = None).
  { unfold heap_predicates in Absent.
    destruct (H fresh) as [[kind tau]|]; [discriminate|reflexivity]. }
  exists (heap_add H fresh false held), fresh. split.
  - apply heap_add_extends; exact FreshH.
  - split.
    + eapply Weaken; [exact ValidNext|].
      eapply allocation_predicates; eassumption.
    + split; [exact Raw|apply allocation_header].
Qed.

Theorem array_preserves_heap : forall W P interface H state tag value held count raw next,
  M.interface_laws P interface -> M.supports interface (PArrNew tag) = true ->
  M.heap_ok interface (heap_predicates W P H) state ->
  value_typed W P H value held ->
  M.primitive interface P (PArrNew tag) [value; VI32 count] state = (Normal raw, next) ->
  exists H' address,
    heap_extends H H' /\ M.heap_ok interface (heap_predicates W P H') next /\
    raw = VTag tag (VHandle address) /\
    handle_typed H' true held (VCheckedHandle true held address).
Proof.
  intros W P interface H state tag value held count raw next Laws Supported Valid Typed Run.
  destruct Laws as [_ [_ [_ [Weaken [_ [_ GC]]]]]].
  specialize (GC (PArrNew tag) [value; VI32 count] state (heap_predicates W P H)
    (fun value => value_typed W P H value held) Supported Valid Typed).
  rewrite Run in GC.
  destruct GC as [extended [ValidNext [Extends [fresh [Raw [Absent Fresh]]]]]].
  assert (FreshH : H fresh = None).
  { unfold heap_predicates in Absent.
    destruct (H fresh) as [[kind tau]|]; [discriminate|reflexivity]. }
  exists (heap_add H fresh true held), fresh. split.
  - apply heap_add_extends; exact FreshH.
  - split.
    + eapply Weaken; [exact ValidNext|].
      eapply allocation_predicates; eassumption.
    + split; [exact Raw|apply allocation_header].
Qed.

End HeapSafety.
