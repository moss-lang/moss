From Stdlib Require Import List Bool Arith.
From NM Require Import Audit EaBase EaCall EaTypesSound NMap.
From NM Require EaCallSound EaControlSound EaMergeSound ProgramPrune
  PublicContextFacts PublicFamilyCoherent
  PublicOrientation PublicTypeFacts WorldSound.
Import ListNotations Audit.Semantics Audit.Semantics.Elaboration.

Definition slot_entries {A} (slot : nat) (entries : list (item * A)) :=
  filter (fun q => Nat.eqb slot (item_slot (fst q))) entries.

Lemma pindex_bucket : forall A (entries : list (item * A)) slot,
  (match nget (pindex entries) slot with Some bucket => bucket | None => [] end) =
  slot_entries slot entries.
Proof.
  intros A entries slot. induction entries as [|[key value] entries IH]; [reflexivity|].
  cbn [pindex fold_right fst slot_entries filter].
  fold (pindex entries). rewrite WorldSound.nget_nset.
  destruct (Nat.eqb_spec slot (item_slot key)) as [->|Different].
  - now rewrite IH.
  - exact IH.
Qed.

Lemma ilook_slot_entries : forall A (entries : list (item * A)) key,
  ilook key (slot_entries (item_slot key) entries) = ilook key entries.
Proof.
  intros A entries key. induction entries as [|[head value] entries IH]; [reflexivity|].
  unfold slot_entries in *. cbn [filter fst ilook].
  destruct (Nat.eqb (item_slot key) (item_slot head)) eqn:Slot; cbn [ilook].
  - rewrite IH. reflexivity.
  - rewrite IH. destruct (item_eqb key head) eqn:Equal; [|reflexivity].
    apply ProgramPrune.item_eqb_true in Equal. subst head.
    rewrite Nat.eqb_refl in Slot. discriminate.
Qed.

Lemma pfind_exact : forall big entries key,
  pfind (pix_of big entries) key = ilook key entries.
Proof.
  intros [|] entries key; [|reflexivity].
  unfold pix_of, pfind, plook.
  pose proof (pindex_bucket _ entries (item_slot key)) as Bucket.
  assert (Read :
    (match nget (pindex entries) (item_slot key) with
     | Some bucket => ilook key bucket
     | None => None
     end) = ilook key (match nget (pindex entries) (item_slot key) with
                       | Some bucket => bucket
                       | None => []
                       end)).
  { destruct (nget (pindex entries) (item_slot key)); reflexivity. }
  rewrite Read, Bucket. apply ilook_slot_entries.
Qed.

Lemma rkof_exact : forall C key, rkof C key = rkey_of C key.
Proof.
  intros C key. destruct key; try reflexivity.
  unfold rkof, rkey_of, rk, rkey, capp.
  rewrite PublicContextFacts.capps_ilook. reflexivity.
Qed.

Lemma rkof_i_exact : forall C big key,
  rkof_i C (pix_of big (c_provisions C)) key = rkey_of C key.
Proof.
  intros C big key. rewrite <- rkof_exact.
  destruct key; try reflexivity.
  unfold rkof_i, rkof, rk_i, rk, capp_i, capp. now rewrite pfind_exact.
Qed.

Theorem sat_map_exact : forall C Cs big th needs,
  sat_map C Cs (pix_of big (c_provisions C)) (pix_of big (c_provisions Cs)) th needs =
  satisfaction_map C Cs th needs.
Proof.
  intros C Cs big th needs. unfold sat_map, satisfaction_map.
  apply map_ext. intro k. now rewrite !rkof_i_exact.
Qed.

Lemma capp_i_exact : forall C big k,
  capp_i (pix_of big (c_provisions C)) k = ilook k (c_apps C).
Proof.
  intros C big k. unfold capp_i.
  now rewrite pfind_exact, PublicContextFacts.capps_ilook.
Qed.

Lemma sat_coherent_sound : forall C Cs big th ns,
  sat_coherent C (pix_of big (c_provisions C)) (pix_of big (c_provisions Cs)) th ns = true ->
  forall k a, In k ns -> ilook k (c_apps Cs) = Some a ->
  forall t want, In (t, Some want) a ->
    match ilook (at_th C th k) (c_apps C) with
    | Some have => In (t, Some (canon C (app_subst th want))) have
    | None => True
    end.
Proof.
  intros C Cs big th ns Checked k a Need Capture t want Required.
  unfold sat_coherent in Checked.
  rewrite forallb_forall in Checked. specialize (Checked k Need).
  rewrite capp_i_exact, Capture in Checked.
  rewrite forallb_forall in Checked. specialize (Checked (t, Some want) Required).
  cbn [fst snd] in Checked. rewrite capp_i_exact in Checked.
  destruct (ilook (at_th C th k) (c_apps C)) as [have|]; [|exact I].
  apply existsb_exists in Checked as [[x value] [Found Equal]].
  cbn [fst snd] in Equal. apply andb_true_iff in Equal as [Key Value].
  apply Nat.eqb_eq in Key. apply EaControlSound.oty_eqb_exact in Value.
  now subst.
Qed.

Lemma sat_types_sound : forall C Cs big th ns,
  sat_types C (pix_of big (c_provisions C)) (pix_of big (c_provisions Cs)) th ns = true ->
  forall k p, In k ns -> ilook k (c_provisions Cs) = Some p ->
    exists p', ilook (at_th C th k) (c_provisions C) = Some p' /\
      option_map (canon_sig C) (provision_signature p') =
      option_map (sig_at C th) (provision_signature p).
Proof.
  intros C Cs big th ns Checked k p Need Capture.
  unfold sat_types in Checked.
  rewrite forallb_forall in Checked. specialize (Checked k Need).
  rewrite !pfind_exact, Capture in Checked.
  destruct (ilook (at_th C th k) (c_provisions C)) as [p'|] eqn:Provision;
    [|discriminate].
  exists p'. split; [reflexivity|]. now apply EaMergeSound.ofsig_eqb_spec.
Qed.

Lemma class_of_reads : forall C t x,
  (forall g u, In (g, u) (c_sub C) -> lookup_subst g (c_sub C) = Some u) ->
  In x (class_of C t) -> canon C (TAbs x) = t.
Proof.
  intros C t x Functional Member. unfold class_of in Member.
  apply in_app_or in Member as [Direct|Subst].
  - destruct t; try contradiction.
    destruct (lookup_subst t (c_sub C)) eqn:Read; [contradiction|].
    cbn in Direct. destruct Direct as [<-|[]].
    unfold canon. cbn [app_subst]. now rewrite Read.
  - apply in_flat_map in Subst as [[g u] [Pair Member]].
    cbn [fst snd] in Member. destruct (ty_eqb u t) eqn:Equal; [|contradiction].
    cbn in Member. destruct Member as [<-|[]].
    apply PublicTypeFacts.ty_eqb_spec in Equal. subst u.
    unfold canon. cbn [app_subst].
    now rewrite (Functional _ _ Pair).
Qed.

Lemma sat_merges_sound : forall C Cs,
  (forall g u, In (g, u) (c_sub Cs) -> lookup_subst g (c_sub Cs) = Some u) ->
  sat_merges C Cs = true ->
  forall g,
    (forall y,
      (exists x, (x = g \/ In x (gens (canon Cs (TAbs g)))) /\
        canon Cs (TAbs y) = canon Cs (TAbs x)) ->
      nlook y (c_ty C) <> None) ->
    canon C (app_subst (c_ty C) (TAbs g)) =
    canon C (app_subst (c_ty C) (canon Cs (TAbs g))).
Proof.
  intros C Cs Functional Checked g InForce.
  destruct (lookup_subst g (c_sub Cs)) as [u|] eqn:Read.
  2: { assert (canon Cs (TAbs g) = TAbs g) as Unchanged.
       { unfold canon. cbn [app_subst]. now rewrite Read. }
       now rewrite Unchanged. }
  pose proof (PublicOrientation.lookup_subst_pair _ _ _ Read) as Row.
  unfold sat_merges in Checked. rewrite forallb_forall in Checked.
  specialize (Checked (g, u) Row). unfold sat_merge1 in Checked.
  cbv beta iota zeta delta [fst] in Checked.
  assert (InForceChecked :
    forallb (bound_in C) (flat_map (fun x => class_of Cs (canon Cs (TAbs x)))
      (g :: gens (canon Cs (TAbs g)))) = true).
  {
    apply forallb_forall. intros y Member.
    apply in_flat_map in Member as [x [Related Class]].
    pose proof (class_of_reads _ _ _ Functional Class) as Equal.
    assert (Bound : nlook y (c_ty C) <> None).
    { apply InForce. exists x. split; [destruct Related as [Equal'|Related]; auto|exact Equal]. }
    unfold bound_in. destruct (nlook y (c_ty C)); [reflexivity|contradiction].
  }
  lazymatch type of Checked with
  | (if ?guard then _ else _) = true =>
      replace guard with true in Checked by (symmetry; exact InForceChecked)
  end. now apply PublicTypeFacts.ty_eqb_spec.
Qed.

Lemma pbucket_member : forall big entries slot q,
  In q (pbucket (pix_of big entries) slot) -> In q entries.
Proof.
  intros [|] entries slot q Member; [|exact Member].
  cbn [pix_of pbucket] in Member.
  rewrite pindex_bucket in Member. apply filter_In in Member. tauto.
Qed.

Lemma cval_go_member : forall entries v t,
  cval_go v entries = Some t ->
  exists a, In (IVal v, ProvidedVal t a) entries.
Proof.
  induction entries as [|[key provision] entries IH]; intros v t Read; [discriminate|].
  destruct key as [s|v0|s|r m th|s], provision; cbn [cval_go] in Read;
    try (apply IH in Read as [a Member]; exists a; now right).
  destruct (Nat.eqb v v0) eqn:Key.
  - apply Nat.eqb_eq in Key. injection Read as <-. subst. eauto with datatypes.
  - apply IH in Read as [a Member]. exists a. now right.
Qed.

Lemma cfn_go_member : forall entries f g,
  cfn_go f entries = Some g ->
  exists a, In (IFn f, ProvidedFn g a) entries.
Proof.
  induction entries as [|[key provision] entries IH]; intros f g Read; [discriminate|].
  destruct key as [s|s|f0|r m th|s], provision; cbn [cfn_go] in Read;
    try (apply IH in Read as [a Member]; exists a; now right).
  destruct (Nat.eqb f f0) eqn:Key.
  - apply Nat.eqb_eq in Key. injection Read as <-. subst. eauto with datatypes.
  - apply IH in Read as [a Member]. exists a. now right.
Qed.

Lemma cmth_go_member : forall entries receiver method signature,
  cmth_go receiver method entries = Some signature ->
  exists th a, In (IMth receiver method th, ProvidedMethod signature a) entries.
Proof.
  induction entries as [|[key provision] entries IH];
    intros receiver method signature Read; [discriminate|].
  destruct key as [s|s|s|r m th|s], provision; cbn [cmth_go] in Read;
    try (apply IH in Read as [capture [a Member]]; exists capture, a; now right).
  destruct (Nat.eqb method m && ty_eqb receiver r) eqn:Key.
  - apply andb_true_iff in Key as [Method Receiver].
    apply Nat.eqb_eq in Method. apply PublicTypeFacts.ty_eqb_spec in Receiver.
    injection Read as <-. subst. eauto with datatypes.
  - apply IH in Read as [capture [a Member]]. exists capture, a. now right.
Qed.

Lemma cfun_go_member : forall entries u fx,
  cfun_go u entries = Some fx -> In (IFun u, ProvidedFunctor fx) entries.
Proof.
  induction entries as [|[key provision] entries IH]; intros u fx Read; [discriminate|].
  destruct key as [s|s|s|r m th|u0], provision; cbn [cfun_go] in Read; try (right; now apply IH).
  destruct (Nat.eqb u u0) eqn:Key.
  - apply Nat.eqb_eq in Key. injection Read as <-. subst. now left.
  - right. now apply IH.
Qed.

Lemma is_some_value : forall A (o : option A), is_some o = true -> exists x, o = Some x.
Proof. intros A [x|] Found; [now exists x|discriminate]. Qed.

Lemma providesb_i_sound : forall C big k,
  providesb_i C (pix_of big (c_provisions C)) k = true -> provides C k.
Proof.
  intros C big k Checked. destruct k as [t|v|f|receiver method th|u]; cbn [providesb_i provides] in *.
  - apply is_some_value in Checked as [x ->]. discriminate.
  - apply is_some_value in Checked as [ty Found].
    apply cval_go_member in Found as [a Member]. apply pbucket_member in Member.
    apply PublicFamilyCoherent.nlook_in_some with (v := ty).
    unfold c_vals. apply in_flat_map. exists (IVal v, ProvidedVal ty a).
    split; [exact Member|now left].
  - apply is_some_value in Checked as [signature Found].
    apply cfn_go_member in Found as [a Member]. apply pbucket_member in Member.
    apply PublicFamilyCoherent.nlook_in_some with (v := signature).
    unfold c_fns. apply in_flat_map. exists (IFn f, ProvidedFn signature a).
    split; [exact Member|now left].
  - apply is_some_value in Checked as [signature Found].
    apply cmth_go_member in Found as [capture [a Member]]. apply pbucket_member in Member.
    unfold c_mths. revert Member. generalize (c_provisions C).
    intros entries Member.
    assert (InKey :
      In ((receiver, method), signature)
         (flat_map (fun q => match q with
           | (IMth r m _, ProvidedMethod g _) => [((r, m), g)]
           | _ => []
           end) entries)).
    { apply in_flat_map. exists (IMth receiver method capture, ProvidedMethod signature a).
      split; [exact Member|now left]. }
    clear Member. revert InKey.
    generalize (flat_map (fun q => match q with
      | (IMth r m _, ProvidedMethod g _) => [((r, m), g)]
      | _ => []
      end) entries).
    induction l as [|[[r m] g] rest IH]; [contradiction|].
    intros [Equal|Member]; cbn [mlook].
    + inversion Equal. subst. rewrite Nat.eqb_refl.
      rewrite (proj2 (PublicTypeFacts.ty_eqb_spec _ _) eq_refl). discriminate.
    + destruct (ty_eqb receiver r && Nat.eqb method m); [discriminate|exact (IH Member)].
  - apply is_some_value in Checked as [fixed Found].
    apply cfun_go_member in Found. apply pbucket_member in Found.
    apply PublicFamilyCoherent.nlook_in_some with (v := fixed).
    unfold c_funs. apply in_flat_map. exists (IFun u, ProvidedFunctor fixed).
    split; [exact Found|now left].
Qed.

Local Lemma filter_checks : forall A (test : A -> bool) xs,
  filter (fun x => negb (test x)) xs = [] -> forallb test xs = true.
Proof.
  intros A test xs Empty. apply forallb_forall. intros x Member.
  destruct (test x) eqn:Checked; [reflexivity|].
  assert (Rejected : In x (filter (fun y => negb (test y)) xs)).
  { apply filter_In. split; [exact Member|now rewrite Checked]. }
  rewrite Empty in Rejected. contradiction.
Qed.

Theorem check_needs_gen_sound : forall capture W who whot C Cs th ns big,
  (forall g u, In (g, u) (c_sub Cs) -> lookup_subst g (c_sub Cs) = Some u) ->
  check_needs_gen capture W who whot C Cs th ns (pix_of big (c_provisions C))
    (pix_of big (c_provisions Cs)) = Ok tt ->
  NeedsSatisfied C Cs th ns.
Proof.
  intros capture W who whot C Cs th ns big Functional Run.
  unfold check_needs_gen in Run. cbv zeta in Run.
  set (ix := pix_of big (c_provisions C)) in *.
  set (ixs := pix_of big (c_provisions Cs)) in *.
  apply rbind_ok in Run as (merged & Merge & Run).
  destruct (min_first (filter (fun q => negb (sat_merge1 C Cs q)) (c_sub Cs)))
    as [[x u]|] eqn:Merges; [discriminate Merge|].
  apply EaCallSound.min_first_none in Merges.
  apply filter_checks in Merges.
  set (missing := filter (fun k => negb (sat_provides1 C ix th k)) ns) in *.
  apply rbind_ok in Run as (provided & Provision & Run).
  destruct (filter (fun k => match at_th C th k with ITy _ => true | _ => false end) missing ++ missing)
    as [|key rest] eqn:Missing.
  2: {
    destruct (at_th C th key); try discriminate Provision.
    destruct (find _ _); discriminate Provision.
  }
  apply app_eq_nil in Missing as [_ Missing].
  unfold missing in Missing. apply filter_checks in Missing.
  apply rbind_ok in Run as (coherent & Coherence & Run).
  destruct (find (fun k => negb (sat_coherent1 C ix ixs th k)) ns) eqn:Coherent;
    [discriminate Coherence|].
  apply rbind_ok in Run as (typed & Types & Run).
  destruct (find (fun k => negb (sat_types1 C ix ixs th k)) ns) eqn:Typed;
    [discriminate Types|].
  split; [|split; [|split]].
  - intros k Member. rewrite forallb_forall in Missing. specialize (Missing k Member).
    unfold sat_provides1 in Missing. eapply providesb_i_sound. exact Missing.
  - apply (sat_coherent_sound C Cs big).
    apply forallb_forall. intros k Member.
    pose proof (find_none _ _ Coherent k Member) as Check.
    apply negb_false_iff in Check. unfold sat_coherent1, sat_coherent in Check.
    cbn [forallb] in Check. now apply andb_true_iff in Check as [Check _].
  - apply (sat_types_sound C Cs big).
    apply forallb_forall. intros k Member.
    pose proof (find_none _ _ Typed k Member) as Check.
    apply negb_false_iff in Check. unfold sat_types1, sat_types in Check.
    cbn [forallb] in Check. now apply andb_true_iff in Check as [Check _].
  - apply sat_merges_sound; assumption.
Qed.

Corollary check_needs_sound : forall W who whot C Cs th ns,
  (forall g u, In (g, u) (c_sub Cs) -> lookup_subst g (c_sub Cs) = Some u) ->
  check_needs W who whot C Cs th ns = Ok tt ->
  NeedsSatisfied C Cs th ns.
Proof.
  intros W who whot C Cs th ns Functional Checked.
  unfold check_needs, satisfaction_index in Checked.
  eapply check_needs_gen_sound; eassumption.
Qed.

Lemma translate_needs_exact : forall C Cs th ns,
  translate_needs C Cs th ns = satisfaction_map C Cs th ns.
Proof. intros. unfold translate_needs, satisfaction_index. apply sat_map_exact. Qed.

Theorem satisfies_gen_sound : forall capture W who whot C Cs th ns sm,
  (forall g u, In (g, u) (c_sub Cs) -> lookup_subst g (c_sub Cs) = Some u) ->
  satisfies_gen capture W who whot C Cs th ns = Ok sm ->
  Satisfies C Cs th ns sm.
Proof.
  intros capture W who whot C Cs th ns sm Functional Run.
  unfold satisfies_gen in Run. cbv zeta in Run.
  apply rbind_ok in Run as ([] & Checked & Emit). injection Emit as <-.
  split.
  - apply translate_needs_exact.
  - unfold satisfaction_index in Checked. eapply check_needs_gen_sound; eassumption.
Qed.

Corollary satisfies_sound : forall W who whot C Cs th ns sm,
  (forall g u, In (g, u) (c_sub Cs) -> lookup_subst g (c_sub Cs) = Some u) ->
  satisfies W who whot C Cs th ns = Ok sm ->
  Satisfies C Cs th ns sm.
Proof. exact (satisfies_gen_sound false). Qed.

Lemma ctxof_subst_functional : forall W f C ns,
  CtxOf W f C ns ->
  forall g u, In (g, u) (c_sub C) -> lookup_subst g (c_sub C) = Some u.
Proof.
  intros W f C ns (d & es & tm & er & tmr & sigma & classes & _ & _ & _ & Merge & _ & _ & -> & _).
  exact (proj2 (proj2 (proj2 (proj2 Merge)))).
Qed.

Lemma cfun_exact : forall C u, cfun C u = nlook u (c_funs C).
Proof.
  intros [types substitution classes entries locals] u. unfold cfun, c_funs.
  cbn [c_provisions]. induction entries as [|[key value] rest IH]; [reflexivity|].
  destruct key as [s|s|s|r m th|f], value;
    cbn [cfun_go flat_map app nlook]; try exact IH.
  destruct (Nat.eqb u f); [reflexivity|exact IH].
Qed.

Lemma cfun_lookup_in_keys : forall C u arguments,
  nlook u (c_funs C) = Some arguments -> In u (cfun_keys C).
Proof.
  intros C u arguments Lookup. unfold c_funs in Lookup.
  apply PublicTypeRead.nlook_in in Lookup.
  apply in_flat_map in Lookup as [[key entry] [Present Found]].
  unfold cfun_keys. apply in_flat_map. exists (key, entry). split; [exact Present|].
  destruct key, entry; cbn in *; try contradiction.
  destruct Found as [Equal|[]]. inversion Equal. now left.
Qed.

Lemma functx_subst_functional : forall W u C ns,
  FunCtx W u C ns ->
  forall g ty, In (g, ty) (c_sub C) -> lookup_subst g (c_sub C) = Some ty.
Proof.
  intros W u C ns (d & ps & a & r & b & es & tm & tm' & sigma & classes & implicit_types &
    _ & _ & _ & _ & _ & Merge & _ & _ & -> & _).
  exact (proj2 (proj2 (proj2 (proj2 Merge)))).
Qed.
