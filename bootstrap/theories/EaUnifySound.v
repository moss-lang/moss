(* Successful unification preserves every solution of its input equations.
   This direction needs neither fuel adequacy nor idempotence. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaCtx EaTypesSound EaMergeSound PublicTypeRead.
From NM Require PublicTypeFacts PublicOrientation PublicCallRealize.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Definition Absorbs (outer inner : subst) : Prop :=
  forall g t, In (g, t) inner -> app_subst outer (TAbs g) = app_subst outer t.

Lemma absorbs_cons : forall outer inner g t,
  Absorbs outer inner -> app_subst outer (TAbs g) = app_subst outer t ->
  Absorbs outer ((g, t) :: inner).
Proof.
  intros outer inner g t Old New x u [Equal|Member].
  - injection Equal as <- <-. exact New.
  - exact (Old _ _ Member).
Qed.

Lemma absorbs_read : forall outer inner,
  Absorbs outer inner -> forall t, app_subst outer (app_subst inner t) = app_subst outer t.
Proof.
  intros outer inner Rows t. rewrite <- (app_subst_nil t) at 2.
  apply subst2_agree. intros g Member. cbn [app_subst lookup_subst].
  destruct (lookup_subst g inner) as [u|] eqn:Read; [|reflexivity].
  symmetry. apply Rows. exact (PublicOrientation.lookup_subst_pair _ _ _ Read).
Qed.

Lemma walk_absorbed : forall outer inner fuel t, Absorbs outer inner ->
  app_subst outer (walk fuel inner t) = app_subst outer t.
Proof.
  intros outer inner fuel. induction fuel as [|fuel IH]; intros t Rows; [reflexivity|].
  destruct t; cbn [walk]; try reflexivity.
  destruct (lookup_subst _ inner) as [u|] eqn:Read; [|reflexivity].
  rewrite IH by exact Rows. symmetry. apply Rows.
  exact (PublicOrientation.lookup_subst_pair _ _ _ Read).
Qed.

Theorem resolve_absorbed : forall outer inner fuel t, Absorbs outer inner ->
  app_subst outer (EaCtx.resolve fuel inner t) = app_subst outer t.
Proof.
  intros outer inner fuel. induction fuel as [|fuel IH]; intros t Rows; [reflexivity|].
  transitivity (app_subst outer (walk (S fuel) inner t)); [|apply walk_absorbed; exact Rows].
  cbn [EaCtx.resolve]. destruct (walk (S fuel) inner t) as [m|fields| |g|n args|members]; try reflexivity.
  - rewrite !subst_record, map_map. f_equal. apply map_ext.
    intros [field child]. cbn [fst snd]. f_equal. apply IH. exact Rows.
  - rewrite !subst_nom. unfold PublicTypeRead.smap. rewrite map_map. f_equal. apply map_ext.
    intros [parameter child]. cbn [fst snd]. f_equal. apply IH. exact Rows.
  - rewrite !subst_union, map_map. f_equal. apply map_ext. intro child. apply IH. exact Rows.
Qed.

Lemma closure_absorbed : forall outer inner fuel, Absorbs outer inner ->
  Absorbs outer (closure fuel inner).
Proof.
  intros outer inner fuel Rows g t Member. unfold closure in Member.
  apply in_map_iff in Member as [[key value] [Equal Member]].
  injection Equal as <- <-. symmetry. apply resolve_absorbed. exact Rows.
Qed.

Definition read_equal (outer : subst) (pair : ty * ty) : Prop :=
  app_subst outer (fst pair) = app_subst outer (snd pair).

Lemma combine_reads_equal : forall outer left right,
  map (app_subst outer) left = map (app_subst outer) right ->
  Forall (read_equal outer) (combine left right).
Proof.
  intros outer left. induction left as [|t left IH]; intros [|u right] Equal;
    cbn in Equal; try discriminate; [constructor|].
  injection Equal as Head Tail. constructor; [exact Head|apply IH; exact Tail].
Qed.

Lemma paired_types_read_equal : forall A outer (left right : list (A * ty)),
  map (fun q => (fst q, app_subst outer (snd q))) left =
  map (fun q => (fst q, app_subst outer (snd q))) right ->
  Forall (read_equal outer) (combine (map snd left) (map snd right)).
Proof.
  intros A outer left right Equal. apply combine_reads_equal.
  apply (f_equal (map snd)) in Equal. rewrite !map_map in Equal |- *. exact Equal.
Qed.

Section Fold.
Variable step : subst -> ty -> ty -> er subst.
Let advance (acc : er subst) (pair : ty * ty) : er subst :=
  rbind acc (fun rows => step rows (fst pair) (snd pair)).

Lemma unify_fold_error : forall pairs message,
  fold_left advance pairs (Err message) = Err message.
Proof. intro pairs. induction pairs; intro message; cbn [fold_left advance rbind]; auto. Qed.

Lemma unify_fold_absorbed : forall outer pairs,
  (forall s a b s', Absorbs outer s -> app_subst outer a = app_subst outer b ->
    step s a b = Ok s' -> Absorbs outer s') ->
  Forall (read_equal outer) pairs -> forall s s', Absorbs outer s ->
  fold_left advance pairs (Ok s) = Ok s' -> Absorbs outer s'.
Proof.
  intros outer pairs Step Equal. induction Equal as [|[a b] pairs Head Tail IH];
    intros s s' Rows Run.
  - injection Run as <-. exact Rows.
  - cbn [fold_left advance rbind fst snd] in Run.
    destruct (step s a b) as [updated|message] eqn:Read.
    + eapply IH; [exact (Step _ _ _ _ Rows Head Read)|exact Run].
    + rewrite unify_fold_error in Run. discriminate.
Qed.
End Fold.

Theorem unify_absorbed : forall E fuel bound why outer s a b s',
  Absorbs outer s -> app_subst outer a = app_subst outer b ->
  unify E fuel bound why s a b = Ok s' -> Absorbs outer s'.
Proof.
  intros E fuel. induction fuel as [|fuel IH]; intros bound why outer s a b s' Rows Equal Run;
    [discriminate|].
  pose proof (walk_absorbed outer s bound a Rows) as ARead.
  pose proof (walk_absorbed outer s bound b Rows) as BRead.
  rewrite <- ARead, <- BRead in Equal. cbn [unify] in Run.
  destruct (ty_eqb (walk bound s a) (walk bound s b)) eqn:Checked.
  { injection Run as <-. exact Rows. }
  destruct (walk bound s a) as [m|fields| |x|n args|members],
    (walk bound s b) as [m'|fields'| |y|n' args'|members']; try discriminate.
  all: try solve [
    match type of Run with
    | context [if nmem ?g (gens (EaCtx.resolve ?budget ?rows ?t)) then _ else _] =>
        destruct (nmem g (gens (EaCtx.resolve budget rows t))) eqn:Occurs
    end;
    [discriminate|injection Run as <-; apply absorbs_cons; [exact Rows|first [exact Equal|symmetry; exact Equal]]]].
  - destruct (slist_eqb (map fst fields) (map fst fields')) eqn:Labels; [|discriminate].
    rewrite !subst_record in Equal. injection Equal as Types.
    eapply (unify_fold_absorbed (unify E fuel bound why)); [intros; eapply IH; eassumption|apply paired_types_read_equal; exact Types|exact Rows|exact Run].
  - destruct (s_orient (e_strat E) x y); injection Run as <-;
      apply absorbs_cons; [exact Rows|exact Equal|exact Rows|symmetry; exact Equal].
  - destruct (Nat.eqb n n' && nlist_eqb (map fst args) (map fst args')) eqn:Labels; [|discriminate].
    rewrite !subst_nom in Equal. injection Equal as _ Types. unfold PublicTypeRead.smap in Types.
    eapply (unify_fold_absorbed (unify E fuel bound why)); [intros; eapply IH; eassumption|apply paired_types_read_equal; exact Types|exact Rows|exact Run].
  - destruct (Nat.eqb (List.length members) (List.length members')) eqn:Arity; [|discriminate].
    rewrite !subst_union in Equal. injection Equal as Types.
    eapply (unify_fold_absorbed (unify E fuel bound why)); [intros; eapply IH; eassumption|apply combine_reads_equal; exact Types|exact Rows|exact Run].
Qed.
