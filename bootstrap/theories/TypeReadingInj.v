(* Injectivity of the source type reading, independent of context formation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab BodyTr CanonTypes.
Import ListNotations.
Local Open Scope list_scope.

Section Inj.
Variable Fl : string -> nat.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Notation trF := (tr Fl).

Lemma map_inj_forall : forall X Y (f : X -> Y) (P : X -> Prop) xs ys,
  Forall (fun x => forall y, P y -> f x = f y -> x = y) xs -> Forall P ys -> map f xs = map f ys -> xs = ys.
Proof.
  intros X Y f P xs. induction xs as [|x r IH]; intros [|y r'] Fx Fy E; cbn in E; try discriminate; [reflexivity|].
  injection E as E1 E2. inversion Fx; subst. inversion Fy; subst. f_equal; [auto | eauto].
Qed.

Lemma forallb_Forall : forall X (f : X -> bool) l, forallb f l = true -> Forall (fun x => f x = true) l.
Proof. intros X f l H. apply Forall_forall. intros x Hx. rewrite forallb_forall in H. exact (H x Hx). Qed.

Lemma tr_inj : forall a b, ety_ok a = true -> ety_ok b = true -> trF a = trF b -> a = b.
Proof.
  induction a as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intros b Oa Ob E;
    destruct b as [| |g'|n' ys|ms'|fs']; cbn [tr] in E; try discriminate; try reflexivity.
  - destruct ms' as [|m r]; [discriminate Ob | discriminate E].
  - injection E as ->. reflexivity.
  - cbn [ety_ok] in Oa, Ob. apply andb_true_iff in Oa as [Na Oa]. apply andb_true_iff in Ob as [Nb Ob].
    injection E as <- E. f_equal.
    eapply (map_inj_forall _ _ _ (fun p => ety_ok (snd p) = true)); [| exact (forallb_Forall _ _ _ Ob) | exact E].
    pose proof (forallb_Forall _ _ _ Oa) as Fa. rewrite Forall_forall in IH, Fa |- *.
    intros [k x] Hx [k' y] Hy Exy. cbn in Exy |- *. injection Exy as <- Exy. f_equal.
    exact (IH (k, x) Hx y (Fa _ Hx) Hy Exy).
  - cbn [ety_ok] in Oa. apply andb_true_iff in Oa as [Na _]. injection E as En _. subst n.
    discriminate Na.
  - destruct ms as [|m r]; [discriminate Oa | discriminate E].
  - pose proof (union_members_ok ms Oa) as Fa. pose proof (union_members_ok ms' Ob) as Fb.
    destruct ms as [|m r]; [discriminate Oa|]. destruct ms' as [|m' r']; [discriminate Ob|].
    assert (E' : map trF (m :: r) = map trF (m' :: r')) by (injection E as E1 E2; cbn; rewrite E1, E2; reflexivity).
    clear E. rename E' into E. f_equal.
    eapply (map_inj_forall _ _ _ (fun x => ety_ok x = true)); [| exact Fb | exact E].
    rewrite Forall_forall in IH, Fa |- *.
    intros x Hx y Hy Exy. exact (IH x Hx y (Fa _ Hx) Hy Exy).
  - cbn [ety_ok] in Ob. apply andb_true_iff in Ob as [Nb _]. injection E as En _. subst n'.
    discriminate Nb.
  - cbn [ety_ok] in Oa, Ob. injection E as E. f_equal.
    eapply (map_inj_forall _ _ _ (fun p => ety_ok (snd p) = true)); [| exact (forallb_Forall _ _ _ Ob) | exact E].
    pose proof (forallb_Forall _ _ _ Oa) as Fa. rewrite Forall_forall in IH, Fa |- *.
    intros [k x] Hx [k' y] Hy Exy. cbn in Exy |- *. injection Exy as Ek Exy. rewrite (Finj _ _ Ek). f_equal.
    exact (IH (k, x) Hx y (Fa _ Hx) Hy Exy).
Qed.

End Inj.
