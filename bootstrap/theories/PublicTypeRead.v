(* Type readings are functional for the public relation: TyOf, Brackets,
   SigOf and PayloadOf each determine their result. Ported from CtxMerge.v,
   which proves the same for ExecutionModel's copy of the relation. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Det.
Variable W : A.world.
Import A.

Lemma pair_eta : forall X Y (p q : X * Y), fst p = fst q -> snd p = snd q -> p = q.
Proof. intros X Y [a b] [c d]; cbn; intros -> ->; reflexivity. Qed.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Fixpoint tyof_det p tm th t a (H : TyOf W p tm th t a) {struct H} : forall b, TyOf W p tm th t b -> a = b
with br_det p tm g x l (H : Brackets W p tm g x l) {struct H} : forall l', Brackets W p tm g x l' -> l = l'.
Proof.
  - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros b' H'; inversion H'; subst; clear H'.
    + reflexivity.
    + reflexivity.
    + reflexivity.
    + f_equal. match goal with H0 : Forall2 _ fs ?ts0 |- _ => assert_fails (constr_eq H0 F); revert ts0 H0 end. repeat match goal with H : NoDup _ |- _ => clear H end.
      induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; intros ts0 F0; inversion F0; subst; [reflexivity|].
      match goal with H0 : fst ?y = _ /\ _ |- _ => destruct H0 as [E2 Hy] end.
      f_equal; [apply pair_eta; [congruence | exact (tyof_det _ _ _ _ _ Hft _ Hy)] | apply IH; assumption].
    + match goal with H0 : Forall2 _ ms ?ts0 |- _ => assert_fails (constr_eq H0 F); rename ts0 into ts1 end.
      assert (E : ts = ts1).
      { match goal with H0 : Forall2 _ ms ts1 |- _ => clear -F H0 tyof_det; revert ts1 H0 end.
        induction F as [|m t ms' ts' Ht Fr IH]; intros ts0 F0; inversion F0; subst; [reflexivity|].
        f_equal; [exact (tyof_det _ _ _ _ _ Ht _ ltac:(eassumption)) | apply IH; assumption]. }
      subst. reflexivity.
    + congruence.
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => assert_fails (constr_eq H0 B); pose proof (br_det _ _ _ _ _ B _ H0) as <- end.
      f_equal. match goal with H0 : Forall2 _ (w_reqs W g) ?a0 |- _ => assert_fails (constr_eq H0 F); revert a0 H0 end.
      clear -F. induction F as [|r ra rs ras [E1 L1] Fr IH]; intros a0 F0; inversion F0; subst; [reflexivity|].
      match goal with H0 : fst ?y = _ /\ _ |- _ => destruct H0 as [E2 L2] end.
      f_equal; [apply pair_eta; congruence | apply IH; assumption].
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0. assert (d0 = d) by congruence. subst d0.
      rewrite A in *. match goal with H0 : ADAlias _ _ = ADAlias _ _ |- _ => injection H0 as <- <- end.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => assert_fails (constr_eq H0 B); pose proof (br_det _ _ _ _ _ B _ H0) as <- end.
      exact (tyof_det _ _ _ _ _ T _ ltac:(eassumption)).
  - destruct H as [p tm g|p tm g bs l F]; intros l' H'; inversion H'; subst; clear H'; [reflexivity|].
    f_equal. match goal with H0 : Forall2 _ bs ?l0 |- _ => assert_fails (constr_eq H0 F); revert l0 H0 end.
    induction F as [|b0 tb bs' l1 (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; intros l0 F0; inversion F0; subst; [reflexivity|].
    match goal with H0 : exists _ _, _ |- _ => destruct H0 as (sp' & sa' & E2 & R2 & K2 & Q2 & T2) end.
    rewrite E1 in E2. injection E2 as <- <-.
    f_equal; [apply pair_eta; [congruence|] | apply IH; assumption].
    exact (tyof_det _ _ _ _ _ T1 _ T2).
Qed.

Lemma forall2_tyof_det : forall p tm th l ts ts', Forall2 (fun (pt : name * aty) t => TyOf W p tm th (snd pt) t) l ts ->
  Forall2 (fun (pt : name * aty) t => TyOf W p tm th (snd pt) t) l ts' -> ts = ts'.
Proof.
  intros p tm th l ts ts' F. revert ts'. induction F as [|x y l1 l2 H Fr IH]; intros ts' F'; inversion F'; subst; [reflexivity|].
  f_equal; [exact (tyof_det _ _ _ _ _ H _ ltac:(eassumption)) | apply IH; assumption].
Qed.

Lemma sigof_det : forall s tm th g1 g2, SigOf W s tm th g1 -> SigOf W s tm th g2 -> g1 = g2.
Proof.
  intros s tm th [ps1 r1] [ps2 r2] (d & rcv & n & ps & rt & body & D & A & P1 & R1)
    (d' & rcv' & n' & ps' & rt' & body' & D' & A' & P2 & R2).
  rewrite D in D'. injection D' as <-. rewrite A in A'. injection A' as <- <- <- <- <-. cbn in *.
  f_equal; [exact (forall2_tyof_det _ _ _ _ _ _ P1 P2)|].
  destruct rt as [r|]; [exact (tyof_det _ _ _ _ _ R1 _ R2) | congruence].
Qed.

Lemma payload_det : forall s xs p1 p2, PayloadOf W s xs p1 -> PayloadOf W s xs p2 -> p1 = p2.
Proof.
  intros s xs p1 p2 [? ? d n ps t ? D A T] Second.
  inversion Second as [? ? d' n' ps' t' ? D' A' T']; subst.
  rewrite D in D'. injection D' as <-. rewrite A in A'. injection A' as <- <- <-.
  exact (tyof_det _ _ _ _ _ T _ T').
Qed.

Lemma payload_tag : forall s xs payload, PayloadOf W s xs payload -> kind_sym W s = Some DKTag.
Proof. intros s xs payload [? ? d n ps t ? D A _]; unfold kind_sym; rewrite D, A; reflexivity. Qed.
End Det.
