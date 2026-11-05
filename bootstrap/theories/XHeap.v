(* ------------------------------------------------------------------------ *)
(* XHeap.v — the GC primitives' law (Track B).                              *)
(*                                                                           *)
(* What the GC primitives of `Heap`, `Mutable` and `Arrays` (PBox, PDeref,   *)
(* PRefSet, PArrNew, PArrLen, PArrGet, PArrSet, PArrFill, PArrCopy) must    *)
(* satisfy for XSafety to cover programs that use them: a store typing Ψ    *)
(* (an address's kind, box or array, and what its contents satisfy), an    *)
(* invariant `heap_ok Ψ st` the primitive semantics chooses, and laws:     *)
(* every other primitive keeps it; an allocation extends Ψ at a fresh       *)
(* address; a read gives a value its address's predicate holds of; a write *)
(* needs one.  A module type, not part of Audit.v: XHeapC.v proves MossX's  *)
(* concrete primitives satisfy it, and XSafety.v assumes it.               *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List PrimInt63.
From NM Require Import Audit HTrie.
Import ListNotations.

(* A store typing: an address's kind (true: an array) and what its
   contents (a box's value, an array's elements) satisfy.  It is keyed by
   an address's bits (HTrie.key), so that no law needs a fact about
   machine-integer equality. *)
Definition hty : Type := list bool -> option (bool * (xvalue -> Prop)).

(* Ψ' extends Ψ: every address Ψ types, Ψ' types the same way. *)
Definition hext (Psi Psi' : hty) : Prop :=
  forall a k Q, Psi a = Some (k, Q) -> Psi' a = Some (k, Q).

(* w is a reference (a tag over a heap address) of kind k whose contents
   satisfy Q. *)
Definition href (Psi : hty) (k : bool) (w : xvalue) (Q : xvalue -> Prop) : Prop :=
  exists n a, w = XVTag n (XVRef a) /\ Psi (key a) = Some (k, Q).

(* What a GC primitive needs of its arguments, for contents satisfying Q. *)
Definition gc_pre (Psi : hty) (p : prim) (ws : list xvalue) (Q : xvalue -> Prop) : Prop :=
  match p, ws with
  | PBox _, [v] => Q v
  | PDeref, [r] => href Psi false r Q
  | PRefSet, [r; v] => href Psi false r Q /\ Q v
  | PArrNew _, [v; XVI32 _] => Q v
  | PArrLen, [r] => href Psi true r Q
  | PArrGet, [r; XVI32 _] => href Psi true r Q
  | PArrSet, [r; XVI32 _; v] => href Psi true r Q /\ Q v
  | PArrFill, [r; XVI32 _; v; XVI32 _] => href Psi true r Q /\ Q v
  | PArrCopy, [r; XVI32 _; r2; XVI32 _; XVI32 _] =>
      href Psi true r Q /\ exists Q2, href Psi true r2 Q2 /\ forall x, Q2 x -> Q x
  | _, _ => False
  end.

(* What it gives back, when it gives a value w, with Ψ' its store typing after. *)
Definition gc_post (Psi Psi' : hty) (p : prim) (Q : xvalue -> Prop) (w : xvalue) : Prop :=
  hext Psi Psi' /\
  match p with
  | PBox rf => exists a, w = XVTag rf (XVRef a) /\ Psi (key a) = None /\ Psi' (key a) = Some (false, Q)
  | PArrNew ar => exists a, w = XVTag ar (XVRef a) /\ Psi (key a) = None /\ Psi' (key a) = Some (true, Q)
  | PDeref | PArrGet => Q w
  | PArrLen => exists n, w = XVI32 n
  | _ => w = XVUnit
  end.

Definition is_gc (p : prim) : bool :=
  match p with
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => true
  | _ => false
  end.

Module Type HEAP_LAWS (Import Prims : PRIM_SEMANTICS).
  Parameter heap_ok : hty -> mstate -> Prop.

  (* the invariant holds of fewer addresses, and of weaker predicates *)
  Axiom heap_mono : forall Psi Psi' st,
    heap_ok Psi st ->
    (forall a k Q', Psi' a = Some (k, Q') ->
       exists Q, Psi a = Some (k, Q) /\ forall v, Q v -> Q' v) ->
    heap_ok Psi' st.

  (* of no address at all, it holds of any machine *)
  Axiom heap_empty : forall st, heap_ok (fun _ => None) st.

  (* every other primitive keeps it *)
  Axiom heap_other : forall P p ws st Psi,
    is_gc p = false -> heap_ok Psi st -> heap_ok Psi (snd (run_prim P p ws st)).

  (* the GC primitives: a value as gc_post says, or a trap *)
  Axiom heap_gc : forall P p ws st Psi Q,
    heap_ok Psi st -> gc_pre Psi p ws Q ->
    match run_prim P p ws st with
    | (XOk w, st') => exists Psi', heap_ok Psi' st' /\ gc_post Psi Psi' p Q w
    | (XTrap _, st') => heap_ok Psi st'
    | _ => False
    end.
End HEAP_LAWS.

(* Used by XSafety.v: value typing carries a store typing (XTyping.sty) of
   the contents' types, XSafety.PsiH reads it as an hty, and every run
   ends at an extension of the store typing it started with, which the
   final machine agrees with.  XHeapC.ConcreteHeap is MossX's instance. *)
