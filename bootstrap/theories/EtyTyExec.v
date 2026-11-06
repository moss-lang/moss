(* Shared executable type translation and field-number frame facts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound.
Import ListNotations.
Local Open Scope list_scope.
Local Strategy opaque [Nat.of_num_uint FUEL].
Tactic Notation "bi" hyp(H) "as" ident(a) ident(s1) ident(H1) :=
 apply bind_inv in H as (a & s1 & H1 & H).
Ltac okstep' H L Hi Hi' Fs' := destruct(L _ _ _ Hi H)as[Hi' Fs'].
Lemma mmap_pure : forall A B (g : A -> M B) (h : A -> B) (Q : st -> Prop) l s ys s',
  mmap g l s = Good (ys, s') -> Inv s -> Q s' ->
  (forall x, ok (g x)) ->
  (forall x s1 y s2, In x l -> Inv s1 -> g x s1 = Good (y, s2) -> Q s2 -> y = h x) ->
  (forall s1 s2, Fr s1 s2 -> Q s2 -> Q s1) ->
  ys = map h l.
Proof.
  intros A B g h Q l. induction l as [|x r IH]; intros s ys s' H Hi Q' Ok Hx Hq.
  - cbn in H. injection H as <- _. reflexivity.
  - cbn [mmap] in H. apply bind_inv in H as (y & s1 & H1 & H). apply bind_inv in H as (ys0 & s2 & H2 & H).
    injection H as <- <-. destruct (Ok x _ _ _ Hi H1) as [Hi1 F1].
    destruct (ok_mmap _ _ g r Ok _ _ _ Hi1 H2) as [_ F2].
    cbn [map]. f_equal.
    + exact (Hx x s y s1 (or_introl eq_refl) Hi H1 (Hq _ _ F2 Q')).
    + exact (IH s1 ys0 s2 H2 Hi1 Q' Ok (fun x0 s0 y0 s3 Hin => Hx x0 s0 y0 s3 (or_intror Hin)) Hq).
Qed.

Lemma ok_layout_ok : forall e t, ok (layout_ok e t).
Proof. intros. unfold layout_ok. okt. Qed.

Section Reading.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Notation trF:=(tr Fl).
Lemma fld_id_final : forall n s i s' s'', Inv s -> fld_id n s = Good (i, s') -> Fr s' s'' -> Later w P R Fl s'' -> Fl n = i.
Proof.
  intros n s i s' s'' Hi H Fs L. destruct (fld_id_numbered n s i s' H (iv_flds _ Hi)) as [_ E].
  exact (lt_flds _ _ _ _ _ (later_back w P R Fl s' s'' Fs L) n i E).
Qed.

(* a record's field, as the relation looks it up *)
Lemma ety_ty_go_tr : forall f t s y s', ety_ty_go f t s = Good (y, s') -> Inv s -> Later w P R Fl s' -> y = trF t.
Proof.
  induction f as [|f IH]; intros t s y s' H Hi L; [discriminate H|].
  cbn [ety_ty_go] in H. destruct t as [| |g|n xs|ms|fs].
  - injection H as <- _. reflexivity.
  - injection H as <- _. reflexivity.
  - injection H as <- _. reflexivity.
  - bi H as xs' s1 H1. injection H as <- ->. cbn [tr]. f_equal.
    refine (mmap_pure _ _ _ (fun p => (fst p, trF (snd p))) (Later w P R Fl) xs s xs' s' H1 Hi L _ _ _).
    + intro x. okt.
    + intros x s0 y0 s2 _ Hi0 Hx L2. bi Hx as y1 s3 Hy. injection Hx as <- <-.
      rewrite (IH _ _ _ _ Hy Hi0 L2). reflexivity.
    + intros s0 s2 Fs L2. exact (later_back w P R Fl _ _ Fs L2).
  - bi H as ms' s1 H1. injection H as <- ->. cbn [tr]. f_equal.
    refine (mmap_pure _ _ _ trF (Later w P R Fl) ms s ms' s' H1 Hi L _ _ _).
    + intro x. apply ok_ety_ty_go.
    + intros x s0 y0 s2 _ Hi0 Hx L2. exact (IH _ _ _ _ Hx Hi0 L2).
    + intros s0 s2 Fs L2. exact (later_back w P R Fl _ _ Fs L2).
  - bi H as fs' s1 H1. injection H as <- ->. cbn [tr]. f_equal.
    refine (mmap_pure _ _ _ (fun p => (Fl (fst p), trF (snd p))) (Later w P R Fl) fs s fs' s' H1 Hi L _ _ _).
    + intro x. okt.
    + intros x s0 y0 s2 _ Hi0 Hx L2. bi Hx as i s3 Hf. okstep' Hf (ok_fld_id (fst x)) Hi0 Hi3 Fs3.
      bi Hx as y1 s4 Hy. injection Hx as <- ->. okstep' Hy (ok_ety_ty_go f (snd x)) Hi3 Hi4 Fs4.
      rewrite (IH _ _ _ _ Hy Hi3 L2), (fld_id_final (fst x) s0 i s3 s2 Hi0 Hf Fs4 L2). reflexivity.
    + intros s0 s2 Fs L2. exact (later_back w P R Fl _ _ Fs L2).
Qed.

(* (unfolding ety_ty in a hypothesis applied to a state sends the
   conversion checker into the fuel 200; an equation does not) *)
Lemma ety_ty_eq : forall t, ety_ty t = ety_ty_go 200 t.
Proof. reflexivity. Qed.

Lemma ety_ty_tr : forall t s y s', ety_ty t s = Good (y, s') -> Inv s -> Later w P R Fl s' -> y = trF t.
Proof. intros t s y s' H Hi L. rewrite ety_ty_eq in H. exact (ety_ty_go_tr _ t s y s' H Hi L). Qed.

Lemma ok_ety_ty : forall t, ok (ety_ty t).
Proof. intro t. rewrite ety_ty_eq. apply ok_ety_ty_go. Qed.

End Reading.
