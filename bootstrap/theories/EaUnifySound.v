(* Successful unification preserves every solution of its input equations.
   This direction needs neither fuel adequacy nor idempotence. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaCtx EaTypesSound EaMergeSound PublicTypeRead.
From NM Require PublicTypeFacts PublicOrientation PublicCallRealize PublicSubstFacts.
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

Lemma signature_read_absorbed : forall outer inner g, Absorbs outer inner ->
  sig_subst outer (sig_subst inner g) = sig_subst outer g.
Proof.
  intros outer inner [parameters result] Rows. unfold sig_subst. cbn [sg_params sg_ret].
  rewrite map_map. f_equal.
  - apply map_ext. intro t. apply absorbs_read. exact Rows.
  - apply absorbs_read. exact Rows.
Qed.

Definition read_key (outer : subst) (key : item) : item :=
  match key with IMth t m args => IMth (app_subst outer t) m args | _ => key end.
Definition read_inst (outer : subst) (instance : subst) : subst :=
  map (fun p => (fst p, app_subst outer (snd p))) instance.

Lemma entry_key_absorbed : forall outer inner e, Absorbs outer inner ->
  option_map (read_key outer) (entry_key inner e) = entry_key outer e.
Proof.
  intros outer inner [] Rows; cbn [entry_key read_key option_map mkey]; try reflexivity.
  rewrite (absorbs_read _ _ Rows). reflexivity.
Qed.

Lemma entry_sig_absorbed : forall outer inner e, Absorbs outer inner ->
  option_map (sig_subst outer) (entry_sig inner e) = entry_sig outer e.
Proof.
  intros outer inner [] Rows; cbn [entry_sig option_map]; try reflexivity;
    try now rewrite (signature_read_absorbed _ _ _ Rows).
  change (Some (FSig [] (app_subst outer (app_subst inner t))) = Some (FSig [] (app_subst outer t))).
  rewrite (absorbs_read _ _ Rows). reflexivity.
Qed.

Lemma entry_inst_absorbed : forall outer inner e, Absorbs outer inner ->
  option_map (read_inst outer) (entry_inst inner e) = entry_inst outer e.
Proof.
  intros outer inner [] Rows; cbn [entry_inst option_map]; try reflexivity;
    unfold read_inst, inst_read; rewrite map_map; f_equal;
    apply map_ext; intros [source binding]; cbn [fst snd]; f_equal; apply absorbs_read; exact Rows.
Qed.

Lemma matched_entries_read_equal : forall outer inner entries first second key,
  Absorbs outer inner -> Unifies outer entries -> In first entries -> In second entries ->
  entry_key inner first = Some key -> entry_key inner second = Some key ->
  option_map (sig_subst outer) (entry_sig inner first) = option_map (sig_subst outer) (entry_sig inner second) /\
  option_map (read_inst outer) (entry_inst inner first) = option_map (read_inst outer) (entry_inst inner second).
Proof.
  intros outer inner entries first second key Rows [Unifier _] First Second K1 K2.
  rewrite !(entry_sig_absorbed _ _ _ Rows), !(entry_inst_absorbed _ _ _ Rows).
  apply (Unifier _ _ (read_key outer key) First Second);
    rewrite <- (entry_key_absorbed outer inner _ Rows); now rewrite ?K1, ?K2.
Qed.

Lemma unify_signature_absorbed : forall E bound why outer s a b s',
  Absorbs outer s -> sig_subst outer a = sig_subst outer b ->
  unify_sig E bound why s a b = Ok s' -> Absorbs outer s'.
Proof.
  intros E bound why outer s [ps rt] [ps' rt'] s' Rows Equal Run.
  unfold unify_sig in Run. cbn [sg_params sg_ret] in Run.
  destruct (Nat.eqb (List.length ps) (List.length ps')) eqn:Arity; [|discriminate].
  unfold sig_subst in Equal. cbn [sg_params sg_ret] in Equal.
  injection Equal as Parameters Result.
  eapply (unify_fold_absorbed (unify E bound bound why));
    [intros; eapply unify_absorbed; eassumption| |exact Rows|exact Run].
  apply combine_reads_equal. cbn [map]. now rewrite Parameters, Result.
Qed.

Lemma unify_instantiation_absorbed : forall E bound why outer s a b s',
  Absorbs outer s -> read_inst outer a = read_inst outer b ->
  unify_inst E bound why s a b = Ok s' -> Absorbs outer s'.
Proof.
  intros E bound why outer s a b s' Rows Equal Run. unfold unify_inst in Run.
  lazymatch type of Run with
  | context [if ?test then _ else _] => destruct test eqn:Names
  end; [|discriminate Run].
  eapply (unify_fold_absorbed (unify E bound bound why));
    [intros; eapply unify_absorbed; eassumption|apply paired_types_read_equal; exact Equal|exact Rows|exact Run].
Qed.

Lemma earlier_key : forall inner e key,
  okey_eqb (entry_key inner e) (Some key) = true -> entry_key inner e = Some key.
Proof.
  intros inner e key. unfold okey_eqb. destruct (entry_key inner e) as [found|]; [|discriminate].
  intro Equal. apply PublicCallRealize.item_eqb_spec in Equal. now subst found.
Qed.

Lemma merge_round_absorbed : forall E bound entries outer inner,
  Unifies outer entries -> Absorbs outer inner ->
  forall rest seen s s', incl rest entries -> incl seen entries -> Absorbs outer s ->
  merge_round E bound inner seen rest s = Ok s' -> Absorbs outer s'.
Proof.
  intros E bound entries outer inner Unifier Inner rest.
  induction rest as [|e rest IH]; intros seen s s' Remaining Seen Rows Run.
  - injection Run as <-. exact Rows.
  - assert (Member : In e entries) by (apply Remaining; left; reflexivity).
    assert (Tail : forall rows, Absorbs outer rows ->
      merge_round E bound inner (seen ++ [e]) rest rows = Ok s' -> Absorbs outer s').
    { intros rows R Step. eapply IH; [| |exact R|exact Step].
      - intros k Present. apply Remaining. right. exact Present.
      - intros k Present. apply in_app_or in Present as [Earlier|[<-|[]]];
          [apply Seen; exact Earlier|exact Member]. }
    cbn [merge_round] in Run. destruct (entry_key inner e) as [key|] eqn:Key.
    + destruct (find (fun earlier => okey_eqb (entry_key inner earlier) (Some key)) seen)
        as [previous|] eqn:Found; [|exact (Tail _ Rows Run)].
      apply find_some in Found as [Previous Match]. apply earlier_key in Match.
      destruct (matched_entries_read_equal outer inner entries previous e key Inner Unifier
        (Seen _ Previous) Member Match Key) as [Signatures Instances].
      destruct (entry_sig inner previous) as [a|], (entry_sig inner e) as [b|]; try discriminate.
      2: { exact (Tail _ Rows Run). }
      cbn [option_map] in Signatures.
      assert (SignatureEqual : sig_subst outer a = sig_subst outer b) by congruence.
      apply rbind_ok in Run as [s1 [SignatureRun Run]].
      apply rbind_ok in Run as [s2 [InstanceRun Run]].
      assert (SignatureRows : Absorbs outer s1).
      { eapply unify_signature_absorbed; [exact Rows|exact SignatureEqual|exact SignatureRun]. }
      apply (Tail s2); [|exact Run].
      destruct (entry_inst inner previous) as [ia|], (entry_inst inner e) as [ib|]; try discriminate.
      * cbn [option_map] in Instances. injection Instances as InstanceEqual.
        eapply unify_instantiation_absorbed; [exact SignatureRows|exact InstanceEqual|exact InstanceRun].
      * injection InstanceRun as <-. exact SignatureRows.
    + destruct e; try exact (Tail _ Rows Run).
      apply rbind_ok in Run as [updated [Equation Run]].
      apply (Tail updated); [|exact Run].
      eapply unify_absorbed; [exact Rows|apply (proj2 Unifier); exact Member|exact Equation].
Qed.

Lemma merge_rounds_absorbed : forall E rounds bound entries outer s s',
  Unifies outer entries -> Absorbs outer s ->
  merge_rounds E rounds bound entries s = Ok s' -> Absorbs outer s'.
Proof.
  intros E rounds. induction rounds as [|rounds IH]; intros bound entries outer s s' Unifier Rows Run;
    [discriminate|].
  cbn [merge_rounds] in Run. apply rbind_ok in Run as [updated [Round Run]].
  assert (Updated : Absorbs outer updated).
  { eapply (merge_round_absorbed E bound entries outer (closure bound s) Unifier
      (closure_absorbed _ _ _ Rows) entries [] s updated);
      [apply incl_refl|intros k []|exact Rows|exact Round]. }
  destruct (Nat.eqb (List.length updated) (List.length s)) eqn:Same.
  - injection Run as <-. apply closure_absorbed. exact Updated.
  - eapply IH; [exact Unifier|exact Updated|exact Run].
Qed.

Theorem merge_principal : forall E entries sigma classes,
  EaCtx.merge E entries = Ok (sigma, classes) ->
  forall other, Unifies other entries -> forall t,
    app_subst other (app_subst sigma t) = app_subst other t.
Proof.
  intros E entries sigma classes Run other Unifier.
  unfold EaCtx.merge, rbind in Run.
  destruct (merge_rounds E _ _ _ _) as [computed|message] eqn:Rounds; [|discriminate].
  destruct (unifiesb computed entries), (merge_scopeb (flat_map entry_gens entries) computed);
    try discriminate.
  injection Run as <- <-. apply absorbs_read.
  eapply merge_rounds_absorbed; [exact Unifier| |exact Rounds]. intros g t [].
Qed.

Lemma closure_row : forall fuel s g t,
  In (g, t) (closure fuel s) -> t = EaCtx.resolve fuel s (TAbs g).
Proof.
  intros fuel s g t Member. unfold closure in Member.
  apply in_map_iff in Member as [[x u] [Equal _]]. injection Equal as <- <-. reflexivity.
Qed.

Lemma closure_function : forall fuel s g t,
  In (g, t) (closure fuel s) -> lookup_subst g (closure fuel s) = Some t.
Proof.
  intros fuel s g t Member.
  destruct (lookup_subst g (closure fuel s)) as [found|] eqn:Read.
  - f_equal. rewrite (closure_row _ _ _ _ Member).
    apply closure_row. exact (PublicOrientation.lookup_subst_pair _ _ _ Read).
  - assert (Key : In g (map fst (closure fuel s))).
    { apply in_map_iff. exists (g, t). auto. }
    exfalso. exact (PublicSubstFacts.lookup_subst_in _ _ Key Read).
Qed.

Lemma merge_rounds_returns_closure : forall E rounds bound entries s s',
  merge_rounds E rounds bound entries s = Ok s' -> exists raw, s' = closure bound raw.
Proof.
  intros E rounds. induction rounds as [|rounds IH]; intros bound entries s s' Run; [discriminate|].
  cbn [merge_rounds] in Run. apply rbind_ok in Run as [updated [Round Run]].
  destruct (Nat.eqb (List.length updated) (List.length s)) eqn:Same.
  - injection Run as <-. exists updated. reflexivity.
  - eapply IH. exact Run.
Qed.

Theorem merge_function : forall E entries sigma classes,
  EaCtx.merge E entries = Ok (sigma, classes) ->
  forall g t, In (g, t) sigma -> lookup_subst g sigma = Some t.
Proof.
  intros E entries sigma classes Run. unfold EaCtx.merge, rbind in Run.
  destruct (merge_rounds E _ _ _ _) as [computed|message] eqn:Rounds; [|discriminate].
  destruct (unifiesb computed entries), (merge_scopeb (flat_map entry_gens entries) computed);
    try discriminate.
  injection Run as <- <-.
  destruct (merge_rounds_returns_closure _ _ _ _ _ _ Rounds) as [raw ->].
  apply closure_function.
Qed.

Theorem merge_sound : forall E entries sigma classes,
  EaCtx.merge E entries = Ok (sigma, classes) ->
  MostGeneralMerge sigma entries /\ MergeOf entries classes /\ SectionOf classes sigma.
Proof.
  intros E entries sigma classes Run.
  destruct (merge_checked _ _ _ _ Run) as [Unifier Scoped].
  pose proof (merge_principal _ _ _ _ Run) as Principal.
  pose proof (Principal sigma Unifier) as Idempotent.
  pose proof (merge_function _ _ _ _ Run) as Function.
  assert (MostGeneral : MostGeneralMerge sigma entries) by (repeat first [assumption | split]).
  split; [exact MostGeneral|exact (merge_presentation _ _ _ _ Run MostGeneral)].
Qed.
