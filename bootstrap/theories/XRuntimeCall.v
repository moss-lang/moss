(* Completing a callee's runtime type meanings from its explicit application
   and the caller's captured type environment. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit MossX CoreMoss XCast XRuntimeEnv.
Import ListNotations.
Set Implicit Arguments.

Lemma call_type_item : forall A t, In t (tyreqs A) <-> In (ITy t) A.
Proof.
  intros A t. split; [|apply wf_ty_frees_aux3].
  unfold tyreqs. intro H. apply in_flat_map in H as [i [Hi Ht]].
  destruct i; simpl in Ht; try contradiction. destruct Ht as [<-|[]]. exact Hi.
Qed.

Lemma call_lookup_defined : forall th t,
  In t (map fst th) -> exists tau, lookup_subst t th = Some tau.
Proof.
  induction th as [|[u tau] r IH]; intros t H; [contradiction|].
  simpl in H |- *. destruct (Nat.eqb t u) eqn:E; [eauto|].
  apply IH. destruct H as [<-|H]; [rewrite Nat.eqb_refl in E; discriminate|exact H].
Qed.

Lemma call_missing_actual_available : forall Sg Phi th th0 t,
  sigma_regular (cS Phi) (cA Phi) ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  In t (map fst th) -> lookup_subst t th0 = None ->
  app_subst (cS Phi) (app_subst th0 (TAbs t)) =
    app_subst (cS Phi) (app_subst th (TAbs t)) ->
  In (ITy t) (cA Phi).
Proof.
  intros Sg Phi th th0 t Reg Wf Hdom Hnone Heq.
  apply call_type_item. destruct (in_dec Nat.eq_dec t (tyreqs (cA Phi))) as [Hin|Hnot]; [exact Hin|].
  exfalso. destruct Reg as [Rdom Rran].
  assert (Ns : lookup_subst t (cS Phi) = None).
  { apply lookup_subst_none. intro Ht. exact (Hnot (Rdom t Ht)). }
  destruct (call_lookup_defined th t Hdom) as [tau Htau].
  assert (Wt : wf_ty Sg Phi tau).
  { rewrite Forall_forall in Wf. exact (Wf (t,tau) (@callee_env_agree_aux1 t th tau Htau)). }
  change (app_subst (cS Phi) (match lookup_subst t th0 with Some x => x | None => TAbs t end) =
          app_subst (cS Phi) (match lookup_subst t th with Some x => x | None => TAbs t end)) in Heq.
  rewrite Hnone, Htau in Heq. cbn [app_subst] in Heq. rewrite Ns in Heq.
  apply Hnot. apply (frees_sigma_wf Wt (conj Rdom Rran) t). rewrite <- Heq. simpl. auto.
Qed.

Lemma call_lookup_inst : forall d th t,
  lookup_xdenv (ITy t) (xinst d th) =
    match lookup_subst t th with Some tau => Some (XDType (xground d tau)) | None => None end.
Proof.
  intros d th. rewrite xinst_map. induction th as [|[u tau] r IH]; intro t; simpl; [reflexivity|].
  destruct (Nat.eqb t u); [reflexivity|apply IH].
Qed.

Lemma call_lookup_app : forall d dc t,
  lookup_xdenv (ITy t) (d ++ dc) =
    match lookup_xdenv (ITy t) d with Some en => Some en | None => lookup_xdenv (ITy t) dc end.
Proof.
  induction d as [|[i en] r IH]; intros dc t; [reflexivity|].
  destruct i; simpl; try apply IH. destruct (Nat.eqb t t0); [reflexivity|apply IH].
Qed.

Theorem runtime_types_call : forall P Sg g Phi th th0 D PhiD d dc,
  runtime_types P g (cA Phi) d -> ctx_grounded g Phi ->
  wf_tele Sg empty_ctx D PhiD -> sat Sg Phi th D ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th0 ->
  (forall t, In t (tyreqs D) ->
    app_subst (cS Phi) (app_subst th0 (TAbs t)) =
    app_subst (cS Phi) (app_subst th (TAbs t))) ->
  (forall t, lookup_xdenv (ITy t) dc = None) ->
  runtime_types P (callee_grounding g Phi th (cA PhiD)) (cA PhiD) (xcall_env P d th0 dc).
Proof.
  intros P Sg g Phi th th0 D PhiD d dc Rt Cg Wtel Sat Wth Wth0 Eq Ndc Reify.
  specialize (Rt Reify). unfold xcall_env. rewrite Reify.
  intros t Ht.
  assert (Htd : In t (tyreqs (cA PhiD))) by (apply call_type_item; exact Ht).
  assert (HtD : In t (tyreqs D)).
  { apply (wf_tele_tyreqs Wtel) in Htd. destruct Htd as [Hno|Hyes]; [contradiction|exact Hyes]. }
  pose proof (Eq t HtD) as Eqt.
  destruct Cg as [Gr [Abs [Cov [Reg Reserved]]]].
  destruct Sat as [PhiD' [Tele [Dom Rest]]].
  assert (EPhi : PhiD'=PhiD) by (pose proof (wf_tele_ctx Wtel); congruence). subst PhiD'.
  assert (Hdom : In t (map fst th)) by (rewrite Dom; exact Htd).
  destruct (lookup_subst t th0) as [tau|] eqn:Eactual.
  - assert (Wtau : wf_ty Sg Phi tau).
    { rewrite Forall_forall in Wth0. exact (Wth0 (t,tau) (@callee_env_agree_aux1 t th0 tau Eactual)). }
    exists (xground d tau). split.
    + rewrite call_lookup_app, call_lookup_inst, Eactual. reflexivity.
    + change (xground d tau = match lookup_subst t (callee_grounding g Phi th (cA PhiD)) with Some x => x | None => TAbs t end).
      rewrite (callee_grounding_lookup g Phi th (cA PhiD) t Htd).
      rewrite (@types_match_reads g (cA Phi) d tau Rt Cov (wf_ty_frees Wtau)).
      rewrite <- (@absorbs_all g (cS Phi) Abs tau).
      change (app_subst (cS Phi) (match lookup_subst t th0 with Some x => x | None => TAbs t end) = app_subst (cS Phi) (app_subst th (TAbs t))) in Eqt.
      rewrite Eactual in Eqt. rewrite Eqt. reflexivity.
  - assert (Hin : In (ITy t) (cA Phi)).
    { exact (@call_missing_actual_available Sg Phi th th0 t Reg Wth Hdom Eactual Eqt). }
    destruct (@types_match_type_env g (cA Phi) d Rt t Hin) as [tau [Ltau Etau]].
    exists tau. split.
    + rewrite call_lookup_app, call_lookup_inst, Eactual.
      unfold xclose. rewrite call_lookup_app, Ndc. exact Ltau.
    + rewrite Etau. change (app_subst g (TAbs t) = match lookup_subst t (callee_grounding g Phi th (cA PhiD)) with Some x => x | None => TAbs t end).
      rewrite (callee_grounding_lookup g Phi th (cA PhiD) t Htd).
      rewrite <- (@absorbs_all g (cS Phi) Abs (TAbs t)).
      change (app_subst (cS Phi) (match lookup_subst t th0 with Some x => x | None => TAbs t end) = app_subst (cS Phi) (app_subst th (TAbs t))) in Eqt.
      rewrite Eactual in Eqt. rewrite Eqt. reflexivity.
Qed.

Lemma call_compose_typefree : forall sm d dc,
  Forall (fun p => is_ty_item (fst p) = false) sm ->
  xcompose d sm = Some dc -> forall t, lookup_xdenv (ITy t) dc = None.
Proof.
  induction sm as [|[req source] r IH]; intros d dc Hf Hc t.
  - injection Hc as <-. reflexivity.
  - inversion Hf as [|? ? Hreq Hr]; subst. cbn [xcompose] in Hc.
    destruct (lookup_xdenv source d); [|discriminate].
    destruct (xcompose d r) as [dr|] eqn:Er; [|discriminate]. injection Hc as <-.
    destruct req; try discriminate Hreq; simpl; eapply IH; eauto.
Qed.

Theorem runtime_types_capture : forall P Sg g Phi D PhiD dt dc,
  runtime_types P g (cA Phi) dt -> ctx_grounded g Phi ->
  sat0 Sg Phi D -> tele_ctx D = Some PhiD ->
  (forall t, lookup_xdenv (ITy t) dc = None) ->
  runtime_types P (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD))
    (cA PhiD) (xclose dt dc).
Proof.
  intros P Sg g Phi D PhiD dt dc Rt Cg Sat Tele Ndc Reify.
  specialize (Rt Reify).
  destruct Sat as [PhiD' [Tele' [Sat Hav]]].
  assert (EPhi : PhiD'=PhiD) by congruence. subst PhiD'.
  destruct Cg as [Gr [Abs Rest]]. intros t Ht.
  assert (Htd : In t (tyreqs (cA PhiD))) by (apply call_type_item; exact Ht).
  assert (Hin : In (ITy t) (cA Phi)).
  { apply avail_ty_in. rewrite Forall_forall in Hav. exact (Hav t Htd). }
  destruct (@types_match_type_env g (cA Phi) dt Rt t Hin) as [tau [Ltau Etau]].
  exists tau. split.
  - unfold xclose. rewrite call_lookup_app, Ndc. exact Ltau.
  - rewrite Etau. change (app_subst g (TAbs t) =
      match lookup_subst t (callee_grounding g Phi (id_app (cA PhiD)) (cA PhiD)) with
      | Some x => x | None => TAbs t end).
    rewrite (callee_grounding_lookup g Phi (id_app (cA PhiD)) (cA PhiD) t Htd).
    rewrite safety_case_bindfn_aux3. rewrite (@absorbs_all g (cS Phi) Abs (TAbs t)). reflexivity.
Qed.

Lemma call_lookup_filtered_subst : forall (keep : tsym -> bool) th t,
  lookup_subst t (filter (fun p => keep (fst p)) th) =
    if keep t then lookup_subst t th else None.
Proof.
  intros keep th. induction th as [|[u tau] r IH]; intro t; [destruct (keep t);reflexivity|].
  cbn [filter fst lookup_subst]. destruct (keep u) eqn:Ku; cbn [lookup_subst].
  - destruct (Nat.eqb t u) eqn:E.
    + apply Nat.eqb_eq in E. subst u. rewrite Ku. reflexivity.
    + exact (IH t).
  - rewrite IH. destruct (Nat.eqb t u) eqn:E; [apply Nat.eqb_eq in E; subst u; rewrite Ku; reflexivity|reflexivity].
Qed.

Theorem runtime_types_apply : forall P Sg g Phi gh gc Ah Ac d dc dl h th,
  runtime_types P g (cA Phi) d -> ctx_grounded g Phi ->
  Forall (fun p => wf_ty Sg Phi (snd p)) th -> covers th (tyreqs Ah) ->
  (forall t, In t (xp_inputs P h) <-> In t (tyreqs Ah)) ->
  (forall t, In t (tyreqs Ah) ->
    app_subst gh (TAbs t) = app_subst g (app_subst (cS Phi) (app_subst th (TAbs t)))) ->
  (forall t, In t (tyreqs Ac) -> app_subst gh (TAbs t) = app_subst gc (TAbs t)) ->
  runtime_types P gc Ac dc -> typefree dl ->
  runtime_types P gh (Ah ++ Ac) (xapply_env P d h th (dl ++ dc)).
Proof.
  intros P Sg g Phi gh gc Ah Ac d dc dl h th Rt Cg Wth Cov Inputs EqH EqC Tc Free Reify.
  specialize (Rt Reify). specialize (Tc Reify).
  destruct Cg as [Gr [Abs [Gcov Rest]]].
  unfold xapply_env, xcall_env. rewrite Reify. intros t Hi.
  assert (Ht : In t (tyreqs Ah ++ tyreqs Ac)).
  { rewrite <- tyreqs_app. apply call_type_item; exact Hi. }
  destruct (in_dec Nat.eq_dec t (tyreqs Ah)) as [Hh|Hn].
  - destruct (Cov t Hh) as [tau Etau].
    assert (Kept : existsb (Nat.eqb t) (xp_inputs P h) = true).
    { apply existsb_exists. exists t. split; [apply Inputs;exact Hh|apply Nat.eqb_refl]. }
    exists (xground d tau). split.
    + rewrite call_lookup_app, call_lookup_inst. pose proof (call_lookup_filtered_subst (fun u => existsb (Nat.eqb u) (xp_inputs P h)) th t) as Filt. cbn beta in Filt. unfold tsym in Filt.
      rewrite Kept, Etau in Filt. rewrite Filt. reflexivity.
    + rewrite (EqH t Hh). cbn [app_subst]. rewrite Etau.
      assert (Wtau : wf_ty Sg Phi tau).
      { rewrite Forall_forall in Wth. exact (Wth (t,tau) (@callee_env_agree_aux1 t th tau Etau)). }
      rewrite (@types_match_reads g (cA Phi) d tau Rt Gcov (wf_ty_frees Wtau)).
      symmetry; apply absorbs_all, Abs.
  - assert (Hc : In t (tyreqs Ac)) by (apply in_app_or in Ht; tauto).
    assert (Dropped : existsb (Nat.eqb t) (xp_inputs P h) = false).
    { apply Bool.not_true_is_false. intro Hk. apply existsb_exists in Hk as [u [Hu E]].
      apply Nat.eqb_eq in E. subst u. apply Hn, Inputs, Hu. }
    destruct (Tc t (proj1 (call_type_item Ac t) Hc)) as [tau [Lt Et]].
    exists tau. split.
    + rewrite call_lookup_app, call_lookup_inst.
      pose proof (call_lookup_filtered_subst (fun u => existsb (Nat.eqb u) (xp_inputs P h)) th t) as Filt. cbn beta in Filt. unfold tsym in Filt.
      rewrite Dropped in Filt. rewrite Filt.
      unfold xclose. rewrite !call_lookup_app, (typefree_lookup t Free), Lt. reflexivity.
    + rewrite Et. symmetry; apply EqC; exact Hc.
Qed.

Lemma call_lookup_filter : forall (keep : item -> bool) env i,
  lookup_xdenv i (filter (fun p => keep (fst p)) env) =
    if keep i then lookup_xdenv i env else None.
Proof.
  intros keep env. induction env as [|[j en] r IH]; intro i; [destruct (keep i);reflexivity|].
  cbn [filter fst]. destruct (keep j) eqn:Kj.
  - change ((if item_eqb i j then Some en else lookup_xdenv i (filter (fun p => keep (fst p)) r)) =
       if keep i then (if item_eqb i j then Some en else lookup_xdenv i r) else None).
    destruct (item_eqb i j) eqn:E.
    + apply item_eqb_eq in E. subst j. rewrite Kj. reflexivity.
    + apply IH.
  - change (lookup_xdenv i (filter (fun p => keep (fst p)) r) =
       if keep i then (if item_eqb i j then Some en else lookup_xdenv i r) else None).
    rewrite IH. destruct (item_eqb i j) eqn:E;
      [apply item_eqb_eq in E; subst j; rewrite Kj; reflexivity|reflexivity].
Qed.

Theorem runtime_types_result : forall P g gb gout A AB Dr th d db dr,
  runtime_types P g A d -> runtime_types P gb AB db ->
  incl (tyreqs Dr) (tyreqs AB) ->
  (forall t, In t (tyreqs Dr) -> app_subst gout (TAbs t) = app_subst gb (TAbs t)) ->
  (forall t, In t (tyreqs A) -> app_subst gout (TAbs t) = app_subst g (TAbs t)) ->
  typefree dr ->
  runtime_types P gout (app_tele th Dr ++ A) (xresult_env P db Dr dr ++ d).
Proof.
  intros P g gb gout A AB Dr th d db dr Rt Rb Inc EqOut EqOld Free Reify t Hi.
  specialize (Rt Reify). specialize (Rb Reify).
  assert (Ht : In t (tyreqs Dr ++ tyreqs A)).
  { rewrite <- tyreqs_app_tele with (th:=th). rewrite <- tyreqs_app. apply call_type_item; exact Hi. }
  unfold xresult_env. rewrite Reify. rewrite <- app_assoc.
  destruct (in_dec Nat.eq_dec t (tyreqs Dr)) as [Hout|Hnot].
  - assert (Hkey : existsb (item_eqb (ITy t)) Dr = true).
    { apply existsb_exists. exists (ITy t). split; [apply call_type_item;exact Hout|apply item_eqb_eq;reflexivity]. }
    destruct (@types_match_type_env gb AB db Rb t (proj1 (call_type_item AB t) (Inc t Hout))) as [tau [Lt Et]].
    exists tau. split.
    + rewrite lookup_xdenv_app_typefree by exact Free. rewrite call_lookup_app.
      unfold xresult_types.
      pose proof (call_lookup_filter (fun i => existsb (item_eqb i) Dr) (xtype_env db) (ITy t)) as Filt.
      cbn beta in Filt. rewrite Hkey in Filt. rewrite Filt,Lt. reflexivity.
    + rewrite Et. symmetry;apply EqOut;exact Hout.
  - assert (Hold : In t (tyreqs A)) by (apply in_app_or in Ht;tauto).
    assert (Hkey : existsb (item_eqb (ITy t)) Dr = false).
    { apply Bool.not_true_is_false. intro Hk. apply existsb_exists in Hk as [i [Hin E]].
      apply item_eqb_eq in E. subst i. apply Hnot,call_type_item,Hin. }
    destruct (Rt t (proj1 (call_type_item A t) Hold)) as [tau [Lt Et]].
    exists tau. split.
    + rewrite lookup_xdenv_app_typefree by exact Free. rewrite call_lookup_app.
      unfold xresult_types.
      pose proof (call_lookup_filter (fun i => existsb (item_eqb i) Dr) (xtype_env db) (ITy t)) as Filt.
      cbn beta in Filt. rewrite Hkey in Filt. rewrite Filt. exact Lt.
    + rewrite Et. symmetry;apply EqOld;exact Hold.
Qed.
