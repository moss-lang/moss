(* Projection preservation for the elaborator state. This layer depends only
   on the executable monad, so readonly proofs can precede frame soundness. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel Elab.
Import ExecutionModel.Input.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint].

Ltac keeps_intros := repeat (lazymatch goal with |- forall _, _ => intro end).
Ltac keeps_head t := lazymatch t with ?f _ => keeps_head f | _ => t end.

Definition keeps {T A} (proj : st -> T) (m : M A) : Prop :=
  forall s a s', m s = Good (a, s') -> proj s' = proj s.

Arguments keeps : simpl never.

Section Keeps.
Variables (T : Type) (proj : st -> T).

Lemma keeps_ret : forall A (a : A), keeps proj (ret a).
Proof. intros A a s x s' H. injection H as _ <-. reflexivity. Qed.

Lemma keeps_fail : forall A msg, keeps proj (@fail A msg).
Proof. intros A msg s x s' H. discriminate H. Qed.

Lemma keeps_get : keeps proj get.
Proof. intros s x s' H. injection H as _ <-. reflexivity. Qed.

Lemma keeps_lift : forall A (r : res A), keeps proj (lift r).
Proof. intros A r s x s' H. unfold lift in H. destruct r; [injection H as _ <-; reflexivity | discriminate H]. Qed.

Lemma keeps_bind : forall A B (m : M A) (k : A -> M B), keeps proj m -> (forall a, keeps proj (k a)) ->
  keeps proj (mbind m k).
Proof.
  intros A B m k Hm Hk s b final Run. unfold mbind in Run.
  destruct (m s) as [[a mid]|message] eqn:Step; [|discriminate Run].
  rewrite (Hk a mid b final Run). exact (Hm s a mid Step).
Qed.

Lemma keeps_mmap : forall A B (f : A -> M B) l, (forall x, keeps proj (f x)) -> keeps proj (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply keeps_ret|].
  apply keeps_bind; [apply Hf | intro y; apply keeps_bind; [exact IH | intro; apply keeps_ret]].
Qed.

Lemma keeps_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, keeps proj (f a x)) -> keeps proj (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply keeps_ret|].
  apply keeps_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma keeps_miter : forall A (f : A -> M unit) l, (forall x, keeps proj (f x)) -> keeps proj (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply keeps_ret|].
  apply keeps_bind; [apply Hf | intro; exact IH].
Qed.

End Keeps.

Create HintDb keepsdb.
#[export] Hint Resolve keeps_ret keeps_fail keeps_get keeps_lift : keepsdb.

(* a primitive step: `Good (a, s')` for an s' that the projection reads as s
   (by computation, for a concrete projection) *)
Ltac keeps_leaf :=
  let s := fresh "s" in let a := fresh "a" in let s' := fresh "s'" in let H := fresh "H" in
  intros s a s' H; cbv beta iota in H;
  repeat (match type of H with context [match ?x with _ => _ end] => destruct x end);
  first [ discriminate H | injection H as _ <-; reflexivity ].

Ltac kfixind1 :=
  lazymatch goal with |- keeps _ (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac kfixind3 :=
  lazymatch goal with |- keeps _ (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

(* `keeps proj t` for a concrete projection, down to primitives *)
Ltac keepst :=
  repeat (keeps_intros; lazymatch goal with
  | |- keeps _ (mbind _ _) => apply keeps_bind
  | |- keeps _ (mmap _ _) => apply keeps_mmap
  | |- keeps _ (mfold _ _ _) => apply keeps_mfold
  | |- keeps _ (miter _ _) => apply keeps_miter
  | |- keeps _ (upd _) => unfold upd; keeps_leaf
  | |- keeps _ (fun s => _) => keeps_leaf
  | |- keeps _ ?t =>
      first [ solve [auto with keepsdb]
            | (lazymatch t with ?F _ => is_fix F end; kfixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; kfixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := keeps_head t in progress unfold h ]
  end).

(* ElabFrame's primitives (okdb), for a projection they do not write *)
#[export] Hint Extern 2 (keeps _ fresh) => (unfold fresh; keeps_leaf) : keepsdb.
#[export] Hint Extern 2 (keeps _ (fld_id _)) => (unfold fld_id; keepst) : keepsdb.
#[export] Hint Extern 2 (keeps _ (local_var _)) => (unfold local_var; keepst) : keepsdb.
#[export] Hint Extern 2 (keeps _ (emit_fn _ _)) => (unfold emit_fn; keepst) : keepsdb.
#[export] Hint Extern 2 (keeps _ (unsupported _)) => (unfold unsupported; keeps_leaf) : keepsdb.
#[export] Hint Extern 2 (keeps _ (new_sym _ _ _ _ _)) => (unfold new_sym; keepst) : keepsdb.

