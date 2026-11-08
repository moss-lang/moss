(* Type readings are functional for the public relation: TyOf, Brackets,
   SigOf and PayloadOf each determine their result. Ported from CtxMerge.v,
   which proves the same for ExecutionModel's copy of the relation. *)
From Stdlib Require Import List String Arith.
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

(* A type read with meanings tm and then substituted by s is the same
   expression read with substituted meanings. Ported from TyOfSubst.v. *)
Section Subst.
Variable W : A.world.
Variable s : subst.
Import A.

Definition smap (l : list (nat * ty)) : list (nat * ty) := map (fun q => (fst q, app_subst s (snd q))) l.

Lemma subst_nom : forall n xs, app_subst s (TNom n xs) = TNom n (smap xs).
Proof.
  intros n xs. cbn. f_equal; try (induction xs as [|q r IH]; [reflexivity|]; cbn; rewrite IH; reflexivity).
Qed.

Lemma subst_union : forall ms, app_subst s (TUnion ms) = TUnion (map (app_subst s) ms).
Proof.
  intros ms. cbn. f_equal; try (induction ms as [|q r IH]; [reflexivity|]; cbn; rewrite IH; reflexivity).
Qed.

Lemma subst_record : forall fs, app_subst s (TRecord fs) = TRecord (map (fun q => (fst q, app_subst s (snd q))) fs).
Proof.
  intros fs. cbn. f_equal. induction fs as [|[f t] r IH]; [reflexivity|]. cbn. f_equal. exact IH.
Qed.

Lemma nlook_app' : forall X k (l m : list (nat * X)), nlook k (l ++ m) = match nlook k l with Some v => Some v | None => nlook k m end.
Proof. intros X k l m. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Lemma nlook_smap : forall k l, nlook k (smap l) = option_map (app_subst s) (nlook k l).
Proof. intros k l. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Definition SubRel (tm tm' : list (nat * ty)) : Prop := forall k, nlook k tm' = option_map (app_subst s) (nlook k tm).

Lemma subrel_app : forall l tm tm', SubRel tm tm' -> SubRel (l ++ tm) (smap l ++ tm').
Proof.
  intros l tm tm' H k. rewrite !nlook_app', nlook_smap. destruct (nlook k l); [reflexivity | apply H].
Qed.

Lemma members_sub : forall t, (forall m, In m (members t) -> exists n xs, m = TNom n xs) ->
  members (app_subst s t) = map (app_subst s) (members t).
Proof.
  intros [m|fs| |g|n xs|ms] H; cbn [members].
  - destruct (H (TMachine m) (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - destruct (H (TRecord fs) (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - destruct (H TUnit (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - destruct (H (TAbs g) (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - rewrite subst_nom. reflexivity.
  - rewrite subst_union. reflexivity.
Qed.

Lemma flat_sub : forall ts, Forall (fun m => exists n xs, m = TNom n xs) (flat_map members ts) ->
  flat_map members (map (app_subst s) ts) = map (app_subst s) (flat_map members ts).
Proof.
  intros ts H. induction ts as [|t r IH]; [reflexivity|]. cbn [map flat_map] in *.
  apply Forall_app in H as [H1 H2]. rewrite map_app, (IH H2). f_equal.
  apply members_sub. intros m Hm. rewrite Forall_forall in H1. exact (H1 m Hm).
Qed.

Fixpoint tyof_subst p tm th t a (H : TyOf W p tm th t a) {struct H} :
  forall tm', SubRel tm tm' -> TyOf W p tm' (option_map (app_subst s) th) t (app_subst s a)
with br_subst p tm g x l (H : Brackets W p tm g x l) {struct H} :
  forall tm', SubRel tm tm' -> Brackets W p tm' g x (smap l).
Proof.
  - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros tm' Hs.
    + apply TyNever.
    + apply TyUnit.
    + apply TyThis.
    + rewrite subst_record. apply TyRec.
      * exact Nd.
      * clear Nd. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [constructor|]. constructor; [|exact IH].
        split; [exact E1 | exact (tyof_subst _ _ _ _ _ Hft tm' Hs)].
    + subst flat. rewrite subst_union, <- (flat_sub ts Fm). apply TyUnion with (ts := map (app_subst s) ts).
      * clear Fm Nd. induction F as [|m t ms' ts' Ht Fr IH]; [constructor|].
        constructor; [exact (tyof_subst _ _ _ _ _ Ht tm' Hs) | exact IH].
      * reflexivity.
      * rewrite (flat_sub ts Fm). rewrite Forall_forall in Fm |- *. intros m Hm.
        apply in_map_iff in Hm as (m0 & <- & Hm0). destruct (Fm m0 Hm0) as (n & xs & ->).
        rewrite subst_nom. eauto.
      * rewrite (flat_sub ts Fm), map_map.
        replace (map (fun x => head (app_subst s x)) (flat_map members ts)) with (map head (flat_map members ts)); [exact Nd|].
        apply map_ext_in. intros m Hm. rewrite Forall_forall in Fm. destruct (Fm m Hm) as (n & xs & ->).
        rewrite subst_nom. reflexivity.
    + eapply TyType; [exact R | exact K | rewrite Hs, L; reflexivity].
    + rewrite subst_nom. eapply TyNom; [exact R | exact K | exact Kk | exact (br_subst _ _ _ _ _ B tm' Hs)|].
      pose proof (subrel_app bs tm tm' Hs) as Hs'.
      clear B. induction F as [|r ra rs ras [E1 E2] Fr IH]; [constructor|]. constructor; [|exact IH].
      split; [exact E1|]. cbn [snd]. rewrite Hs', E2. reflexivity.
    + eapply TyAlias; [exact R | exact D | exact A | exact (br_subst _ _ _ _ _ B tm' Hs)|].
      exact (tyof_subst _ _ _ _ _ T _ (subrel_app bs tm tm' Hs)).
  - destruct H as [p tm g|p tm g bs l F]; intros tm' Hs.
    + apply BrNone.
    + unfold smap. rewrite map_rev. apply BrSome.
      induction F as [|b0 tb bs' l' (sp & sa & E1 & E2 & E3 & E4 & Ht) Fr IH]; [constructor|]. constructor; [|exact IH].
      exists sp, sa. split; [exact E1|]. split; [exact E2|]. split; [exact E3|]. split; [exact E4|].
      exact (tyof_subst _ _ _ _ _ Ht tm' Hs).
Qed.

Lemma payload_subst : forall n xs payload, PayloadOf W n xs payload ->
  PayloadOf W n (smap xs) (app_subst s payload).
Proof.
  intros n xs payload Payload. destruct Payload as [? ? d nm ps t ? D A T].
  econstructor; [exact D|exact A|].
  exact (tyof_subst _ _ _ _ _ T _ (fun k => nlook_smap k _)).
Qed.
End Subst.

Lemma app_subst_nil : forall t, app_subst [] t = t.
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms]; cbn; try reflexivity.
  - f_equal. induction fields as [|[f t] rest Tail]; cbn; [reflexivity|]. rewrite IH, Tail. reflexivity.
  - f_equal. induction th as [|[x t] rest Tail]; cbn; [reflexivity|]. rewrite IH, Tail. reflexivity.
  - f_equal. induction ms as [|t rest Tail]; cbn; [reflexivity|]. rewrite IH, Tail. reflexivity.
Qed.

(* An abstract type read by TyOf is the receiver or a meaning in force. *)
Section Abstract.
Variable W : A.world.
Import A.

Lemma nlook_in : forall X k (l : list (nat * X)) v, nlook k l = Some v -> In (k, v) l.
Proof.
  intros X k l. induction l as [|[a x] r IH]; intros v Found; [discriminate|].
  cbn in Found. destruct (Nat.eqb_spec k a) as [->|_]; [injection Found as <-; left; reflexivity|right; auto].
Qed.

Fixpoint tyof_abs p tm th a t (D : TyOf W p tm th a t) {struct D} :
  forall g, t = TAbs g -> th = Some (TAbs g) \/ exists k, nlook k tm = Some (TAbs g)
with br_abs p tm h x l (D : Brackets W p tm h x l) {struct D} :
  forall k g, nlook k l = Some (TAbs g) -> exists k', nlook k' tm = Some (TAbs g).
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g0 k bs args R K Kk B F|p tm th pa a g0 d n body bs tau R D A B T];
      intros g E; try discriminate.
    + left; congruence.
    + right; subst; eauto.
    + destruct (tyof_abs _ _ _ _ _ T g E) as [Th|[k Found]]; [discriminate|].
      rewrite nlook_app' in Found. destruct (nlook k bs) eqn:Inner.
      * injection Found as ->. right. exact (br_abs _ _ _ _ _ B k g Inner).
      * right; eauto.
  - destruct D as [p tm h|p tm h bs l F]; intros k g Found; [discriminate|].
    apply nlook_in in Found. apply (proj2 (in_rev _ _)) in Found.
    clear br_abs. induction F as [|b0 tb bs' l' (sp & sa & E1 & E2 & E3 & E4 & Ht) Fr IH]; [contradiction|].
    destruct Found as [E|Found]; [|exact (IH Found)].
    subst tb. cbn in Ht.
    destruct (tyof_abs _ _ _ _ _ Ht g eq_refl) as [Th|Some']; [discriminate|exact Some'].
Qed.
End Abstract.
