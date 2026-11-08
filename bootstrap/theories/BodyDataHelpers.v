(* Static execution helpers, independent of record/match soundness. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore ElabMono BodySoundCore Merge CanonTypes.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Section Down.
Variables (w : Elab.world) (P : xprogram) (R : nat -> list nat) (Fl : string -> nat).

(* the judgments at fuel f+1 give them at f *)
Lemma ok_down : forall f, OK w P R Fl (S f) -> OK w P R Fl f.
Proof.
  intros f [S1 B1 Ss1 St1 C1 I1 P1 K1].
  destruct (mono_group f) as (M0 & M1 & M2 & M3 & M4 & M5 & M6 & M7 & M8 & M9 & _).
  constructor.
  - intros c e x exp s v t s' H. exact (S1 c e x exp s v t s' (M0 _ _ _ _ _ _ H)).
  - intros c e bl exp s v t bt s' H. exact (B1 c e bl exp s v t bt s' (M2 _ _ _ _ _ _ H)).
  - intros c e ss s xs e1 c1 d s' H. exact (Ss1 c e ss s xs e1 c1 d s' (M3 _ _ _ _ _ H)).
  - intros c e st0 s xs e1 c1 d s' H. exact (St1 c e st0 s xs e1 c1 d s' (M4 _ _ _ _ _ H)).
  - intros c e y s v s' H. exact (C1 c e y s v s' (M5 _ _ _ _ _ H)).
  - intros c e x exp s v t s' H. exact (I1 c e x exp s v t s' (M6 _ _ _ _ _ _ H)).
  - intros c e p a s v t s' H. exact (P1 c e p a s v t s' (M9 _ _ _ _ _ _ H)).
  - intros g s v t s' H. exact (K1 g s v t s' (M8 _ _ _ H)).
Qed.

End Down.
Section Static.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Variable b:bundle.
Hypothesis Hload:load_world b=Good w.
Notation trF:=(tr Fl).
Lemma sym_nominal : forall s x g s', Inv s -> st_w s = w -> sym x s = Good (g, s') ->
  (gs_kind g = KTag \/ gs_kind g = KUnit) -> s' = s /\ nget (w_syms w) x = Some g.
Proof.
  intros s x g s' Hi Hw H Hk. unfold sym in H. destruct (nget (st_syms s) x) as [g0|] eqn:Ex; [|discriminate].
  injection H as <- <-. split; [reflexivity|].
  destruct (nget (w_syms w) x) as [g1|] eqn:Ew.
  - pose proof (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g1 ltac:(rewrite Hw; exact Ew)) as E.
    rewrite E in Ex. exact Ex.
  - destruct (iv_kind _ Hi b ltac:(rewrite Hw; exact Hload) x g0 ltac:(rewrite Hw; exact Ew) Ex) as [K1 K2].
    destruct Hk; contradiction.
Qed.
Lemma synth_wrap : forall e exp s v t s' (m : M (xexpr * ety)),
  (let! (v, t) := m in inject_check e t exp ;; ret (v, t)) s = Good ((v, t), s') ->
  ok m -> Inv s ->
  exists s1, m s = Good ((v, t), s1) /\ Inv s1 /\ Fr s s1 /\ Fr s1 s'.
Proof.
  intros e exp s v t s' m H Om Hi. apply bind_inv in H as ([v0 t0] & s1 & H1 & H).
  apply bind_inv in H as (u & s2 & Hinj & H). injection H as <- <- <-.
  destruct (Om _ _ _ Hi H1) as [Hi1 Fs1]. destruct (ok_inject_check e t0 exp _ _ _ Hi1 Hinj) as [_ Fs2].
  exists s1. auto.
Qed.
Lemma context_canon_tr : forall e C t,
  A.c_sub C = suf Fl (en_uf e) -> Tri (en_uf e) -> A.canon C (trF t) = trF (canon e t).
Proof. intros e C t H T. unfold A.canon. rewrite H. apply tr_canon. exact T. Qed.
End Static.
