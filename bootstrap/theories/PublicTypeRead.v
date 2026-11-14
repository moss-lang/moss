(* Type readings are functional for the public relation: TyOf, Brackets,
   SigOf and PayloadOf each determine their result. Ported from CtxMerge.v,
   which proves the same for ExecutionModel's copy of the relation. *)
From Stdlib Require Import List String Arith Sorting.Permutation.
From NM Require AuditBase.
From NM Require Import Audit.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Lemma insert_ordered_perm : forall X (before : X -> X -> bool) x xs,
  Permutation (insert_ordered before x xs) (x :: xs).
Proof.
  intros X before x xs. induction xs as [|y ys IH]; cbn; [reflexivity|].
  destruct (before x y); [reflexivity|].
  eapply Permutation_trans with (l' := y :: x :: ys); [now constructor|apply perm_swap].
Qed.

Lemma order_perm : forall X (before : X -> X -> bool) xs,
  Permutation (fold_right (insert_ordered before) [] xs) xs.
Proof.
  intros X before xs. induction xs as [|x xs IH]; cbn; [reflexivity|].
  eapply Permutation_trans with (l' := x :: fold_right (insert_ordered before) [] xs);
    [apply insert_ordered_perm|now constructor].
Qed.

Lemma order_in : forall X (before : X -> X -> bool) (xs : list X) x,
  In x (fold_right (insert_ordered before) [] xs) -> In x xs.
Proof. intros; eapply Permutation_in; [apply order_perm|eassumption]. Qed.

Lemma order_map : forall X Y (before : X -> X -> bool) (after : Y -> Y -> bool)
    (f : X -> Y) xs,
  (forall x y, In x xs -> In y xs -> after (f x) (f y) = before x y) ->
  map f (fold_right (insert_ordered before) [] xs) =
    fold_right (insert_ordered after) [] (map f xs).
Proof.
  intros X Y before after f xs. induction xs as [|x xs IH]; intros Orders; cbn; [reflexivity|].
  rewrite <- IH by (intros; apply Orders; now right).
  remember (fold_right (insert_ordered before) [] xs) as ys eqn:Sorted.
  assert (Members : forall y, In y ys -> In y xs) by (intros; subst ys; eapply order_in; eassumption).
  clear Sorted IH. induction ys as [|y ys IH]; cbn; [reflexivity|].
  assert (Order : after (f x) (f y) = before x y).
  { apply Orders; [now left|right; apply Members; now left]. }
  rewrite Order.
  destruct (before x y); cbn; [reflexivity|].
  f_equal. apply IH. intros; apply Members; now right.
Qed.

Lemma order_fields_map : forall X Y (f : X -> Y) (fields : list (string * X)),
  map (fun q => (fst q, f (snd q))) (order_fields fields) =
  order_fields (map (fun q => (fst q, f (snd q))) fields).
Proof. intros; apply order_map; reflexivity. Qed.

Lemma order_members_map : forall (f : ty -> ty) members,
  (forall t, In t members -> union_key (f t) = union_key t) ->
  map f (order_members members) = order_members (map f members).
Proof. intros; apply order_map; intros; now rewrite !H. Qed.

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
                  |p tm th pa t tau R K L|p tm th pa a g bs args R K B F|p tm th pa a g d n body bs tau R D A B T];
      intros b' H'; inversion H'; subst; clear H'.
    + reflexivity.
    + reflexivity.
    + reflexivity.
    + f_equal. apply f_equal. match goal with H0 : Forall2 _ fs ?ts0 |- _ => assert_fails (constr_eq H0 F); revert ts0 H0 end. repeat match goal with H : NoDup _ |- _ => clear H end.
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
Proof. intros s xs payload [? ? d n ps t ? D A _]; unfold kind_sym; rewrite D.
  destruct (d_ast d) as [| | | | | | | |]; cbn in A |- *; try discriminate; reflexivity. Qed.
Lemma bare_tag_payload : forall tag args, BareTag W tag -> PayloadOf W tag args TUnit.
Proof.
  intros tag args [name Bare]. unfold ast_of in Bare.
  destruct (w_decl W tag) as [d|] eqn:Declared; [|discriminate].
  injection Bare as Ast. eapply PayTag; [exact Declared|rewrite Ast; reflexivity|constructor].
Qed.
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
  - destruct (H (TRigid m) (or_introl eq_refl)) as (? & ? & E). discriminate E.
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
                  |p tm th pa t tau R K L|p tm th pa a g bs args R K B F|p tm th pa a g d n body bs tau R D A B T];
      intros tm' Hs.
    + apply TyNever.
    + apply TyUnit.
    + apply TyThis.
    + rewrite subst_record, order_fields_map. apply TyRec.
      * exact Nd.
      * clear Nd. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [constructor|]. constructor; [|exact IH].
        split; [exact E1 | exact (tyof_subst _ _ _ _ _ Hft tm' Hs)].
    + subst flat. rewrite subst_union, order_members_map, <- (flat_sub ts Fm).
      2: { intros m Present. rewrite Forall_forall in Fm. destruct (Fm m Present) as (n & xs & ->). now rewrite subst_nom. }
      apply TyUnion with (ts := map (app_subst s) ts).
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
    + rewrite subst_nom. eapply TyNom; [exact R | exact K | exact (br_subst _ _ _ _ _ B tm' Hs)|].
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
                  |p tm th pa t tau R K L|p tm th pa a g0 bs args R K B F|p tm th pa a g0 d n body bs tau R D A B T];
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

(* Two readings of one type expression agree after substitution when their
   meaning tables agree wherever both define a symbol. *)
Section Relate.
Variable W : A.world.
Variables s1 s2 : subst.
Import A.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Definition Agree (tm1 tm2 : list (nat * ty)) : Prop :=
  forall k v1 v2, nlook k tm1 = Some v1 -> nlook k tm2 = Some v2 -> app_subst s1 v1 = app_subst s2 v2.

Definition Rel (q1 q2 : nat * ty) : Prop := fst q1 = fst q2 /\ app_subst s1 (snd q1) = app_subst s2 (snd q2).

Lemma rel_nlook : forall l1 l2, Forall2 Rel l1 l2 -> forall k,
  (nlook k l1 = None <-> nlook k l2 = None) /\ Agree l1 l2.
Proof.
  intros l1 l2 F k. split.
  - revert k. induction F as [|[a v] [b u] r1 r2 [E _] _ IH]; intros k; [split; reflexivity|].
    cbn in E |- *. subst b. destruct (Nat.eqb k a); [split; discriminate|apply IH].
  - clear k. induction F as [|[a v] [b u] r1 r2 [E V] _ IH]; intros k v1 v2 L1 L2; [discriminate|].
    cbn in E, V, L1, L2. subst b. destruct (Nat.eqb k a).
    + injection L1 as <-. injection L2 as <-. exact V.
    + exact (IH k v1 v2 L1 L2).
Qed.

Lemma agree_app : forall l1 l2 tm1 tm2, Forall2 Rel l1 l2 -> Agree tm1 tm2 -> Agree (l1 ++ tm1) (l2 ++ tm2).
Proof.
  intros l1 l2 tm1 tm2 F A k v1 v2 L1 L2. rewrite nlook_app' in L1, L2.
  destruct (rel_nlook l1 l2 F k) as [Same Ag].
  destruct (nlook k l1) as [x1|] eqn:K1; destruct (nlook k l2) as [x2|] eqn:K2.
  - injection L1 as <-. injection L2 as <-. exact (Ag k x1 x2 K1 K2).
  - exfalso. destruct Same as [_ S2]. discriminate (S2 eq_refl).
  - exfalso. destruct Same as [S1 _]. discriminate (S1 eq_refl).
  - exact (A k v1 v2 L1 L2).
Qed.

Lemma smap_rel : forall (l1 l2 : list (nat * ty)), Forall2 Rel l1 l2 -> smap s1 l1 = smap s2 l2.
Proof.
  intros l1 l2 F. unfold smap. induction F as [|[a v] [b u] r1 r2 [E V] _ IH]; [reflexivity|].
  cbn in E, V |- *. rewrite E, V, IH. reflexivity.
Qed.

Lemma forall2_rev : forall X Y (R : X -> Y -> Prop) l1 l2, Forall2 R l1 l2 -> Forall2 R (rev l1) (rev l2).
Proof.
  intros X Y R l1 l2 F. induction F; cbn; [constructor|].
  apply Forall2_app; [exact IHF|repeat constructor; assumption].
Qed.

Fixpoint tyof_rel p tm1 th1 a t1 (D : TyOf W p tm1 th1 a t1) {struct D} :
  forall tm2 th2 t2, TyOf W p tm2 th2 a t2 -> Agree tm1 tm2 ->
  (forall x1 x2, th1 = Some x1 -> th2 = Some x2 -> app_subst s1 x1 = app_subst s2 x2) ->
  app_subst s1 t1 = app_subst s2 t2
with br_rel p tm1 g x l1 (D : Brackets W p tm1 g x l1) {struct D} :
  forall tm2 l2, Brackets W p tm2 g x l2 -> Agree tm1 tm2 -> Forall2 Rel l1 l2.
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g bs args R K B F|p tm th pa a g d n body bs tau R D A B T];
      intros tm2 th2 t2 D2 Ag This; inversion D2; subst; clear D2.
    + reflexivity.
    + reflexivity.
    + apply This; reflexivity.
    + rewrite !subst_record, !order_fields_map. f_equal. apply f_equal.
      match goal with H0 : Forall2 _ fs ?ts0 |- _ => assert_fails (constr_eq H0 F); rename H0 into F2; rename ts0 into ts2 end.
      repeat match goal with H : NoDup _ |- _ => clear H end.
      revert ts2 F2. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; intros ts2 F2;
        inversion F2 as [|? ft2 ? ts2' [E2 Hft2] Fr2]; subst; [reflexivity|].
      cbn. f_equal; [|apply IH; assumption].
      destruct ft, ft2; cbn in *. f_equal; [congruence|].
      exact (tyof_rel _ _ _ _ _ Hft _ _ _ Hft2 Ag This).
    + match goal with H0 : Forall2 _ ms ?ts0 |- _ => rename H0 into F2; rename ts0 into ts2 end.
      match goal with H0 : Forall _ (flat_map members ts2) |- _ => rename H0 into Fm2 end.
      rewrite !subst_union, !order_members_map.
      2,3: intros m Present; match goal with
           | Formed : Forall _ ?all, Present : In m ?all |- _ =>
               rewrite Forall_forall in Formed; destruct (Formed m Present) as (n & xs & ->); now rewrite subst_nom
           end.
      f_equal. apply f_equal.
      rewrite <- (flat_sub s1 ts Fm), <- (flat_sub s2 ts2 Fm2). f_equal.
      clear Fm Fm2. repeat match goal with H : NoDup _ |- _ => clear H end. revert ts2 F2. induction F as [|m t ms' ts' Ht Fr IH]; intros ts2 F2;
        inversion F2; subst; [reflexivity|].
      cbn. f_equal; [|apply IH; assumption].
      exact (tyof_rel _ _ _ _ _ Ht _ _ _ ltac:(eassumption) Ag This).
    + match goal with H0 : resolve_sym W p pa = Some ?t' |- _ => assert (t' = t) by congruence; subst t' end.
      eapply Ag; eassumption.
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => rename H0 into B2; rename bs0 into bs2 end.
      pose proof (br_rel _ _ _ _ _ B _ _ B2 Ag) as BR.
      pose proof (agree_app _ _ _ _ BR Ag) as Ag'.
      rewrite !subst_nom. f_equal. apply smap_rel.
      match goal with H0 : Forall2 _ (w_reqs W g) ?a0 |- _ => rename H0 into F2; rename a0 into args2 end.
      clear B B2 BR. revert args2 F2.
      induction F as [|r ra rs ras [E1 L1] Fr IH]; intros args2 F2; inversion F2 as [|? ra2 ? ras2 [E2 L2] Fr2]; subst;
        constructor; [|apply IH; assumption].
      split; [congruence|]. exact (Ag' _ _ _ L1 L2).
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0. assert (d0 = d) by congruence. subst d0.
      rewrite A in *. match goal with H0 : ADAlias _ _ = ADAlias _ _ |- _ => injection H0 as <- <- end.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => rename H0 into B2; rename bs0 into bs2 end.
      pose proof (br_rel _ _ _ _ _ B _ _ B2 Ag) as BR.
      exact (tyof_rel _ _ _ _ _ T _ _ _ ltac:(eassumption) (agree_app _ _ _ _ BR Ag)
        (fun _ _ E _ => ltac:(discriminate E))).
  - destruct D as [p tm g|p tm g bs l F]; intros tm2 l2 D2 Ag; inversion D2; subst; clear D2; [constructor|].
    apply forall2_rev. rename l into l1.
    match goal with H0 : Forall2 _ bs ?l0 |- _ => rename H0 into F2; rename l0 into l2' end.
    revert l2' F2. induction F as [|b0 tb bs' l1' (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; intros l2' F2;
      inversion F2 as [|? tb2 ? l2'' (sp' & sa' & E2 & R2 & K2 & Q2 & T2) Fr2]; subst; constructor; [|apply IH; assumption].
    rewrite E1 in E2. injection E2 as <- <-.
    split; [congruence|].
    exact (tyof_rel _ _ _ _ _ T1 _ _ _ T2 Ag (fun _ _ E _ => ltac:(discriminate E))).
Qed.

Lemma sig_rel : forall f tm1 tm2 th1 th2 g1 g2,
  SigOf W f tm1 th1 g1 -> SigOf W f tm2 th2 g2 -> Agree tm1 tm2 ->
  (forall x1 x2, th1 = Some x1 -> th2 = Some x2 -> app_subst s1 x1 = app_subst s2 x2) ->
  map (app_subst s1) (sg_params g1) = map (app_subst s2) (sg_params g2) /\
  app_subst s1 (sg_ret g1) = app_subst s2 (sg_ret g2).
Proof.
  intros f tm1 tm2 th1 th2 [ps1 r1] [ps2 r2] (d & rcv & n & ps & rt & body & D & Ast & P1 & R1)
    (d' & rcv' & n' & ps' & rt' & body' & D' & Ast' & P2 & R2) Ag This.
  rewrite D in D'. injection D' as <-. rewrite Ast in Ast'. injection Ast' as <- <- <- <- <-. cbn in *.
  split.
  - clear R1 R2 D Ast. revert ps2 P2. induction P1 as [|pt t ps0 ts Ht Fr IH]; intros ps2 P2;
      inversion P2; subst; [reflexivity|]. cbn. f_equal; [|apply IH; assumption].
    exact (tyof_rel _ _ _ _ _ Ht _ _ _ ltac:(eassumption) Ag This).
  - destruct rt as [r|]; [exact (tyof_rel _ _ _ _ _ R1 _ _ _ R2 Ag This)|rewrite R1, R2; reflexivity].
Qed.
End Relate.

(* Two composed substitutions agree on a type when they agree on each
   generation it mentions. *)
Theorem subst2_agree : forall t s1 s2 s3 s4,
  (forall x, In x (A.gens t) -> app_subst s1 (app_subst s2 (TAbs x)) = app_subst s3 (app_subst s4 (TAbs x))) ->
  app_subst s1 (app_subst s2 t) = app_subst s3 (app_subst s4 t).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s1 s2 s3 s4 Agree; cbn [A.gens] in Agree.
  - reflexivity.
  - rewrite !subst_record. rewrite !map_map. f_equal.
    induction fields as [|[f t] rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + f_equal. apply IH. intros x Ix. apply Agree. apply in_or_app. left. exact Ix.
    + apply Tail. intros x Ix. apply Agree. apply in_or_app. right. exact Ix.
  - reflexivity.
  - apply Agree. left. reflexivity.
  - rewrite !subst_nom. unfold smap. rewrite !map_map. f_equal.
    induction th as [|[x t] rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + f_equal. apply IH. intros y Iy. apply Agree. apply in_or_app. left. exact Iy.
    + apply Tail. intros y Iy. apply Agree. apply in_or_app. right. exact Iy.
  - rewrite !subst_union. rewrite !map_map. f_equal.
    induction ms as [|t rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + apply IH. intros y Iy. apply Agree. apply in_or_app. left. exact Iy.
    + apply Tail. intros y Iy. apply Agree. apply in_or_app. right. exact Iy.
Qed.

Corollary subst_agree_on : forall t s1 s2,
  (forall x, In x (A.gens t) -> app_subst s1 (TAbs x) = app_subst s2 (TAbs x)) ->
  app_subst s1 t = app_subst s2 t.
Proof.
  intros t s1 s2 Agree. rewrite <- (app_subst_nil t) at 1 2.
  apply subst2_agree. intros x Ix. cbn [app_subst lookup_subst]. exact (Agree x Ix).
Qed.

(* Three substitutions in a row agree with two when they agree on each
   generation the type mentions. *)
Theorem subst3_agree : forall t s1 s2 s3 s4 s5,
  (forall x, In x (A.gens t) ->
     app_subst s1 (app_subst s2 (app_subst s3 (TAbs x))) = app_subst s4 (app_subst s5 (TAbs x))) ->
  app_subst s1 (app_subst s2 (app_subst s3 t)) = app_subst s4 (app_subst s5 t).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s1 s2 s3 s4 s5 Agree; cbn [A.gens] in Agree.
  - reflexivity.
  - rewrite !subst_record. rewrite !map_map. f_equal.
    induction fields as [|[f t] rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + f_equal. apply IH. intros x Ix. apply Agree. apply in_or_app. left. exact Ix.
    + apply Tail. intros x Ix. apply Agree. apply in_or_app. right. exact Ix.
  - reflexivity.
  - apply Agree. left. reflexivity.
  - rewrite !subst_nom. unfold smap. rewrite !map_map. f_equal.
    induction th as [|[x t] rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + f_equal. apply IH. intros y Iy. apply Agree. apply in_or_app. left. exact Iy.
    + apply Tail. intros y Iy. apply Agree. apply in_or_app. right. exact Iy.
  - rewrite !subst_union. rewrite !map_map. f_equal.
    induction ms as [|t rest Tail]; [reflexivity|]. cbn in Agree |- *. f_equal.
    + apply IH. intros y Iy. apply Agree. apply in_or_app. left. exact Iy.
    + apply Tail. intros y Iy. apply Agree. apply in_or_app. right. exact Iy.
Qed.

(* Grounding through an instantiation map composes. *)
Corollary subst_through : forall t s th,
  (forall x, In x (A.gens t) -> lookup_subst x th <> None) ->
  app_subst (map (fun q => (fst q, app_subst s (snd q))) th) t = app_subst s (app_subst th t).
Proof.
  intros t s th Dom. rewrite <- (app_subst_nil t) at 1.
  apply subst2_agree. intros x Ix. cbn [app_subst lookup_subst].
  specialize (Dom x Ix). clear Ix. induction th as [|[y u] rest IH]; cbn in *; [congruence|].
  destruct (Nat.eqb x y); [reflexivity|]. apply IH. exact Dom.
Qed.

(* The generations a reading produces come from its meanings or receiver. *)
Section Support.
Variable W : A.world.
Import A.

Definition tm_gens (tm : list (nat * ty)) : list nat := flat_map (fun q => gens (snd q)) tm.

Lemma tm_gens_app : forall l m g, In g (tm_gens (l ++ m)) <-> In g (tm_gens l) \/ In g (tm_gens m).
Proof. intros l m g. unfold tm_gens. rewrite flat_map_app. apply in_app_iff. Qed.

Lemma nlook_gens : forall tm k v g, nlook k tm = Some v -> In g (gens v) -> In g (tm_gens tm).
Proof.
  intros tm k v g Found Ig. apply nlook_in in Found. unfold tm_gens. apply in_flat_map.
  exists (k, v). split; assumption.
Qed.

Lemma member_gens : forall t m g, In m (members t) -> In g (gens m) -> In g (gens t).
Proof.
  intros [m0|fs| |h|n xs|ms] m g Mem Ig; cbn in Mem; try (destruct Mem as [<-|[]]; exact Ig).
  cbn. induction ms as [|m1 rest IH]; [contradiction|]. apply in_or_app.
  destruct Mem as [<-|Mem]; [left; exact Ig|right; exact (IH Mem)].
Qed.

Fixpoint tyof_gens p tm th a t (D : TyOf W p tm th a t) {struct D} :
  forall g, In g (gens t) -> In g (tm_gens tm) \/ (exists x, th = Some x /\ In g (gens x))
with br_gens p tm h x l (D : Brackets W p tm h x l) {struct D} :
  forall g, In g (tm_gens l) -> In g (tm_gens tm).
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g0 bs args R K B F|p tm th pa a g0 d n body bs tau R D A B T];
      intros g Ig; cbn [gens] in Ig.
    + contradiction.
    + contradiction.
    + right. eauto.
    + assert (Source : forall fs : list (string * ty),
          In g (gens (TRecord fs)) <-> exists q, In q fs /\ In g (gens (snd q))).
      { intro fields. induction fields as [|[label t] rest IH]; cbn [gens fst snd In].
        - split; [intros []|intros [q [[] _]]].
        - change (In g (List.app (gens t) (gens (TRecord rest))) <->
            exists q, ((label, t) = q \/ In q rest) /\ In g (gens (snd q))).
          rewrite in_app_iff, IH. split.
          + intros [Here|[q [Present Gen]]]; [exists (label, t); now auto|exists q; now auto].
          + intros [q [[<-|Present] Gen]]; [now left|right; exists q; now auto]. }
      assert (Raw : In g (gens (TRecord ts))).
      { apply Source in Ig as [q [Present Gen]]. apply Source.
        exists q. split; [eapply order_in; exact Present|exact Gen]. }
      clear Ig Nd. rename Raw into Ig. cbn [gens] in Ig.
      induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [contradiction|].
      destruct ft as [f t]. cbn in Ig. apply in_app_or in Ig as [Here|There];
        [exact (tyof_gens _ _ _ _ _ Hft g Here)|exact (IH There)].
    + subst flat. clear Fm Nd.
      assert (Flat : exists m, In m (flat_map members ts) /\ In g (gens m)).
      { clear F. assert (Ordered : exists m, In m (order_members (flat_map members ts)) /\ In g (gens m)).
        { induction (order_members (flat_map members ts)) as [|m rest IH]; [contradiction|].
        cbn in Ig. apply in_app_or in Ig as [Here|There]; [exists m; split; [left; reflexivity|exact Here]|].
          destruct (IH There) as [m' [Im Ig']]. exists m'; split; [right; exact Im|exact Ig']. }
        destruct Ordered as [m [Present Gen]]. exists m. split; [eapply order_in; exact Present|exact Gen]. }
      destruct Flat as [m [Im Ig']]. apply in_flat_map in Im as [t [It Mem]].
      pose proof (member_gens t m g Mem Ig') as Igt. clear Ig Ig' Mem.
      induction F as [|a t' ms' ts' Ht Fr IH]; [contradiction|].
      destruct It as [<-|It]; [exact (tyof_gens _ _ _ _ _ Ht g Igt)|exact (IH It)].
    + left. exact (nlook_gens _ _ _ _ L Ig).
    + left. assert (Args : In g (tm_gens args)).
      { clear -Ig. induction args as [|[r v] rest IH]; [contradiction|]. cbn in Ig |- *.
        apply in_app_or in Ig as [Here|There]; apply in_or_app; [left; exact Here|right; exact (IH There)]. }
      clear Ig. induction F as [|r ra rs ras [E1 L1] Fr IH]; [contradiction|].
      destruct ra as [r' v]. cbn in Args. apply in_app_or in Args as [Here|There]; [|exact (IH There)].
      cbn in L1. pose proof (nlook_gens _ _ _ _ L1 Here) as Ibt. apply tm_gens_app in Ibt as [InB|InT];
        [exact (br_gens _ _ _ _ _ B g InB)|exact InT].
    + left. destruct (tyof_gens _ _ _ _ _ T g Ig) as [Ibt|[x [E _]]]; [|discriminate].
      apply tm_gens_app in Ibt as [InB|InT]; [exact (br_gens _ _ _ _ _ B g InB)|exact InT].
  - destruct D as [p tm h|p tm h bs l F]; intros g Ig; [contradiction|].
    apply in_flat_map in Ig as [q [Iq Ig]]. apply (proj2 (in_rev _ _)) in Iq.
    clear br_gens. induction F as [|b0 tb bs' l' (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; [contradiction|].
    destruct Iq as [<-|Iq]; [|exact (IH Iq)].
    destruct (tyof_gens _ _ _ _ _ T1 g Ig) as [In'|[x [E _]]]; [exact In'|discriminate].
Qed.
End Support.
