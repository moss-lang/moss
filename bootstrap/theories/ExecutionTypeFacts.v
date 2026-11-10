(* Type facts used by the legacy metadata and lowering proofs. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel.
Import ListNotations.
Set Implicit Arguments.

Fixpoint ty_ind' (P : ty -> Prop)
    (HU : P TUnit)
    (HA : forall t, P (TAbs t))
    (HN : forall n th, Forall (fun p => P (snd p)) th -> P (TNom n th))
    (HUn : forall ms, Forall P ms -> P (TUnion ms))
    (t : ty) {struct t} : P t :=
  match t with
  | TUnit => HU
  | TAbs a => HA a
  | TNom n th =>
      HN n th
        ((fix go (l : list (tsym * ty)) : Forall (fun p => P (snd p)) l :=
            match l with
            | [] => Forall_nil (fun p => P (snd p))
            | p :: r => @Forall_cons _ (fun p => P (snd p)) p r
                          (ty_ind' HU HA HN HUn (snd p)) (go r)
            end) th)
  | TUnion ms =>
      HUn ms
        ((fix go (l : list ty) : Forall P l :=
            match l with
            | [] => Forall_nil P
            | t' :: r => @Forall_cons _ P t' r (ty_ind' HU HA HN HUn t') (go r)
            end) ms)
  end.

Lemma eqb_eq_aux1 : forall th1 th2,
  Forall (fun p => forall b, ty_eqb (snd p) b = true <-> snd p = b) th1 ->
  ((fix go (l1 l2 : list (tsym * ty)) : bool :=
      match l1, l2 with
      | [], [] => true
      | p1 :: r1, p2 :: r2 =>
          Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
      | _, _ => false
      end) th1 th2 = true) <-> th1 = th2.
Proof.
  induction th1 as [| [t1 ty1] r1 IHr]; intros th2 HF; destruct th2 as [| [t2 ty2] r2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply andb_true_iff in H as [Hp Hr].
    apply andb_true_iff in Hp as [Ht Hty].
    apply Nat.eqb_eq in Ht; subst t2.
    inversion HF as [| p r Hhd Htl Heq]; subst; simpl in Hhd.
    apply Hhd in Hty; subst ty2.
    f_equal. apply IHr; assumption.
  - inversion HF as [| p r Hhd Htl Heq]; subst; simpl in Hhd.
    apply andb_true_iff; split.
    + apply andb_true_iff; split.
      * apply Nat.eqb_eq; congruence.
      * apply Hhd; congruence.
    + apply IHr; congruence.
Qed.

(* Private helper: element-wise boolean equality on the plain [ty] lists
   that occur inside TUnion. *)
Lemma eqb_eq_aux2 : forall ms1 ms2,
  Forall (fun t => forall b, ty_eqb t b = true <-> t = b) ms1 ->
  ((fix go (l1 l2 : list ty) : bool :=
      match l1, l2 with
      | [], [] => true
      | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
      | _, _ => false
      end) ms1 ms2 = true) <-> ms1 = ms2.
Proof.
  induction ms1 as [| t1 r1 IHr]; intros ms2 HF; destruct ms2 as [| t2 r2];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply andb_true_iff in H as [Ht Hr].
    inversion HF as [| t r Hhd Htl Heq]; subst.
    apply Hhd in Ht; subst t2.
    f_equal. apply IHr; assumption.
  - inversion HF as [| t r Hhd Htl Heq]; subst.
    apply andb_true_iff; split.
    + apply Hhd; congruence.
    + apply IHr; congruence.
Qed.

Lemma ty_eqb_eq : forall a b, ty_eqb a b = true <-> a = b.
Proof.
  induction a as [ | t1 | n1 th1 IH | ms1 IH] using ty_ind'; intros b; destruct b as [ | t2 | n2 th2 | ms2 ];
    simpl; split; intro H;
    try reflexivity; try discriminate.
  - apply Nat.eqb_eq in H; subst; reflexivity.
  - injection H as ->; apply Nat.eqb_eq; reflexivity.
  - apply andb_true_iff in H as [Hn Hth].
    apply Nat.eqb_eq in Hn; subst n2.
    destruct (@eqb_eq_aux1 th1 th2 IH) as [Hfwd _].
    apply Hfwd in Hth; subst th2; reflexivity.
  - injection H as Hn Hth; subst n2; subst th2.
    apply andb_true_iff; split.
    + apply Nat.eqb_eq; reflexivity.
    + destruct (@eqb_eq_aux1 th1 th1 IH) as [_ Hbwd].
      apply Hbwd; reflexivity.
  - destruct (@eqb_eq_aux2 ms1 ms2 IH) as [Hfwd _].
    apply Hfwd in H; subst ms2; reflexivity.
  - injection H as Hms; subst ms2.
    destruct (@eqb_eq_aux2 ms1 ms1 IH) as [_ Hbwd].
    apply Hbwd; reflexivity.
Qed.
