(* ------------------------------------------------------------------------ *)
(* ElabInv.v — an invariant of a cache, over a whole run of the elaborator *)
(* (Track A).  A cache of the state (const_val's st_consts, fn_info's      *)
(* st_info) holds entries that only a few steps write; the relation reads  *)
(* every entry of the run's last state.  That they all hold is an          *)
(* induction over the run: each step keeps the cache valid.                *)
(*                                                                           *)
(* What an entry asks may read the run's last state (the field numbers it  *)
(* ends with, that nothing was reported unsupported, …), so the step is    *)
(* conditional on a property L of the states after it, one that holds of   *)
(* a state when it holds of a later one (along ElabFrame's Fr).            *)
(*                                                                           *)
(*   keeps proj m   m leaves `proj` of the state unchanged.                 *)
(*   okq C m        m is ElabFrame's `ok`, and from a state whose cache     *)
(*                  satisfies Q to one satisfying L, it ends with Q.        *)
(*                                                                           *)
(* A cache is a `Cache` record (its projection, L, Q, and how they meet    *)
(* ElabFrame's frame); `keepst` proves `keeps` of a function that never    *)
(* writes the projection, and `okqt` decomposes `okq` down to the steps    *)
(* that write it, which are the caller's.                                   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore.
Import ListNotations.

Local Strategy opaque [Nat.of_num_uint].

(* ======================================================================== *)
(* 1. keeps                                                                  *)
(* ======================================================================== *)

From NM Require Export ElabKeeps.

(* ======================================================================== *)
(* 2. A cache, and okq                                                       *)
(* ======================================================================== *)

Record Cache : Type := {
  ca_T : Type;
  ca_proj : st -> ca_T;
  (* what the run's last state gives back to the states before it *)
  ca_L : st -> Prop;
  (* the cache's invariant *)
  ca_Q : st -> Prop;
  ca_L_back : forall s s', Fr s s' -> ca_L s' -> ca_L s;
  ca_L_vars : forall vs n s, ca_L (s_vars vs n s) -> ca_L s;
  ca_Q_vars : forall vs n s, ca_Q s -> ca_Q (s_vars vs n s);
  (* a step that leaves the projection alone keeps Q *)
  ca_Q_keep : forall s s', Inv s -> Fr s s' -> ca_proj s' = ca_proj s -> ca_L s' -> ca_Q s -> ca_Q s'
}.

Definition okq (C : Cache) {A} (m : M A) : Prop :=
  ok m /\ forall s a s', Inv s -> m s = Good (a, s') -> ca_L C s' -> ca_Q C s -> ca_Q C s'.

Arguments okq : simpl never.

Section Okq.
Variable C : Cache.

Lemma okq_keeps : forall A (m : M A), ok m -> keeps (ca_proj C) m -> okq C m.
Proof.
  intros A m Ho Hk. split; [exact Ho|]. intros s a s' Hi H L Q.
  destruct (Ho s a s' Hi H) as [_ F]. exact (ca_Q_keep C s s' Hi F (Hk s a s' H) L Q).
Qed.

Lemma okq_ret : forall A (a : A), okq C (ret a).
Proof. intros. apply okq_keeps; [apply ok_ret | apply keeps_ret]. Qed.

Lemma okq_bind : forall A B (m : M A) (k : A -> M B), okq C m -> (forall a, okq C (k a)) -> okq C (mbind m k).
Proof.
  intros A B m k [Om Qm] Hk. split; [apply ok_bind; [exact Om | intro a; exact (proj1 (Hk a))]|].
  intros s b s'' Hi H L Q. apply bind_inv in H as (a & s' & H1 & H2).
  destruct (Om s a s' Hi H1) as [Hi' _]. destruct (Hk a) as [Ok Qk].
  destruct (Ok s' b s'' Hi' H2) as [_ F2].
  exact (Qk s' b s'' Hi' H2 L (Qm s a s' Hi H1 (ca_L_back C s' s'' F2 L) Q)).
Qed.

Lemma okq_mmap : forall A B (f : A -> M B) l, (forall x, okq C (f x)) -> okq C (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply okq_ret|].
  apply okq_bind; [apply Hf | intro y; apply okq_bind; [exact IH | intro; apply okq_ret]].
Qed.

Lemma okq_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, okq C (f a x)) -> okq C (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply okq_ret|].
  apply okq_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma okq_miter : forall A (f : A -> M unit) l, (forall x, okq C (f x)) -> okq C (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply okq_ret|].
  apply okq_bind; [apply Hf | intro; exact IH].
Qed.

(* with_vars: the body runs from a renumbered state, which Q and L do not
   read *)
Lemma okq_with_vars_inv : forall A init (m : M A) s a s', okq C m -> Inv s -> with_vars init m s = Good (a, s') ->
  exists s3, m (s_vars init (List.length init) s) = Good (a, s3) /\ s' = s_vars (st_vars s) (st_nvar s) s3 /\
    Inv (s_vars init (List.length init) s).
Proof.
  intros A init m s a s' _ Hi H. unfold with_vars in H.
  apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-.
  apply bind_inv in H as (r & s3 & H3 & H).
  apply bind_inv in H as (u' & s4 & E2 & H). unfold upd in E2. injection E2 as <- <-.
  injection H as <- <-. exists s3. split; [exact H3 | split; [reflexivity|]].
  destruct Hi; constructor; assumption.
Qed.

Lemma okq_with_vars : forall A init (m : M A), okq C m -> okq C (with_vars init m).
Proof.
  intros A init m Hm. split; [apply ok_with_vars; exact (proj1 Hm)|].
  intros s a s' Hi H L Q.
  destruct (okq_with_vars_inv A init m s a s' Hm Hi H) as (s3 & H3 & -> & Hi1).
  apply ca_Q_vars. apply ca_L_vars in L.
  exact (proj2 Hm _ _ _ Hi1 H3 L (ca_Q_vars C _ _ _ Q)).
Qed.

End Okq.

Create HintDb okqdb.
#[export] Hint Resolve okq_ret : okqdb.

(* the projection of a concrete cache, read off its definition *)
Ltac cache_proj :=
  lazymatch goal with |- keeps (ca_proj ?C) _ =>
    let h := head_of C in unfold h; cbn [ca_proj] end.

(* okq of a step that never writes the cache: its ok and keeps *)
Ltac okq_keeps_leaf :=
  apply okq_keeps; [solve [okt] | cache_proj; solve [keepst]].
Ltac okq_keeps_lemma :=
  apply okq_keeps; [solve [auto with okdb] | cache_proj; solve [auto with keepsdb]].

Ltac qfixind1 :=
  lazymatch goal with |- okq _ (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac qfixind3 :=
  lazymatch goal with |- okq _ (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

(* Decompose `okq C t` down to the steps that write the cache, which stay
   goals (or are hints of okqdb, or hypotheses: a group's induction
   hypothesis).  `ext` is tried first at each step (a caller's rule for
   what okq alone does not see); `okqt`
   is `okqt_with fail`. *)
Ltac okqt_with ext :=
  repeat (fintros; first [ ext | lazymatch goal with
  | |- okq _ (mbind _ _) => apply okq_bind
  | |- okq _ (mmap _ _) => apply okq_mmap
  | |- okq _ (mfold _ _ _) => apply okq_mfold
  | |- okq _ (miter _ _) => apply okq_miter
  | |- okq _ (with_vars _ _) => apply okq_with_vars
  | |- okq _ (upd _) => okq_keeps_leaf
  | |- okq _ (fun s => _) => okq_keeps_leaf
  | |- okq _ ?t =>
      first [ solve [auto with okqdb]
            | solve [okq_keeps_lemma]
            | (lazymatch t with ?F _ => is_fix F end; qfixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; qfixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end ]).

Ltac okqt := okqt_with fail.
