(* Captured provisions supply exactly the indexed keys a lambda declares. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit EaBase NMap.
From NM Require EaBind EaMergeSound EaSatisfiesSound PublicCallRealize PublicFormation PublicTypeFacts WorldSound.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.

Local Definition seen_items (seen : nmap (list item)) (slot : nat) : list item :=
  match nget seen slot with Some entries => entries | None => [] end.

Local Definition SeenItems (seen : nmap (list item)) (out : list item) : Prop :=
  forall k, In k (seen_items seen (item_slot k)) <-> In k out.

Local Lemma seen_add : forall seen out key,
  SeenItems seen out ->
  SeenItems (nset seen (item_slot key) (key :: seen_items seen (item_slot key))) (key :: out).
Proof.
  intros seen out key Indexed queried. unfold seen_items at 1. rewrite WorldSound.nget_nset.
  destruct (Nat.eqb_spec (item_slot queried) (item_slot key)) as [Same|Different].
  - cbn [In]. rewrite <- Same. rewrite (Indexed queried). reflexivity.
  - change (In queried (seen_items seen (item_slot queried)) <-> In queried (key :: out)).
    rewrite (Indexed queried). cbn [In]. split; [now right|].
    intros [Equal|Old]; [subst; contradiction|exact Old].
Qed.

Theorem iuniq_fast_exact : forall entries,
  NoDup (iuniq_fast entries) /\ forall key, In key (iuniq_fast entries) <-> In key entries.
Proof.
  intros entries. unfold iuniq_fast.
  set (step := fun (acc : list item * nmap (list item)) k =>
    let '(out, seen) := acc in
    let i := item_slot k in
    let b := seen_items seen i in
    if imem k b then acc else (k :: out, nset seen i (k :: b))) in *.
  change (NoDup (rev (fst (fold_left step entries ([], NE)))) /\
    forall key, In key (rev (fst (fold_left step entries ([], NE)))) <-> In key entries).
  assert (Fold : forall xs out seen, NoDup out -> SeenItems seen out ->
    let result := fst (fold_left step xs (out, seen)) in
    NoDup result /\ forall k, In k result <-> In k xs \/ In k out).
  { induction xs as [|key xs IH]; intros out seen Distinct Indexed; cbn [fold_left fst In].
    - split; [exact Distinct|tauto].
    - cbn [step fst snd].
      destruct (imem key (seen_items seen (item_slot key))) eqn:Known.
      + destruct (IH out seen Distinct Indexed) as [Unique Member].
        split; [exact Unique|]. intro k. rewrite Member. apply EaMergeSound.imem_spec in Known.
        apply (proj1 (Indexed key)) in Known. intuition congruence.
      + assert (New : ~ In key out).
        { intro Present. apply (proj2 (Indexed key)) in Present. apply EaMergeSound.imem_spec in Present. congruence. }
        assert (Distinct' : NoDup (key :: out)) by (constructor; assumption).
        destruct (IH (key :: out) _ Distinct' (seen_add _ _ _ Indexed))
          as [Unique Member].
        split; [exact Unique|]. intro k. rewrite Member. cbn [In]. tauto. }
  destruct (Fold entries [] NE (NoDup_nil _) ltac:(intro k; reflexivity)) as [Distinct Member].
  split; [now apply NoDup_rev|]. intro key. rewrite <- in_rev, Member. cbn [In]. tauto.
Qed.

Local Lemma indexed_member : forall C k p,
  In (k,p) (c_provisions C) ->
  In (k,p) (pbucket (PIdx (pindex (c_provisions C))) (item_slot k)).
Proof.
  intros C k p Member. cbn [pbucket].
  rewrite EaSatisfiesSound.pindex_bucket. apply filter_In. split; [exact Member|].
  cbn [fst]. apply Nat.eqb_refl.
Qed.

Local Lemma present_value : forall entries v t a,
  In (IVal v, ProvidedVal t a) entries -> is_some (cval_go v entries) = true.
Proof.
  induction entries as [|[key q] rest IH]; intros v t a Member; [contradiction|].
  destruct Member as [Same|Member].
  - inversion Same; subst key q. cbn [cval_go]. now rewrite Nat.eqb_refl.
  - destruct key, q; cbn [cval_go]; try (eapply IH; exact Member).
    match goal with
    | |- context [if ?same then _ else _] => destruct same; [reflexivity|eapply IH; exact Member]
    end.
Qed.

Local Lemma present_function : forall entries f sg a,
  In (IFn f, ProvidedFn sg a) entries -> is_some (cfn_go f entries) = true.
Proof.
  induction entries as [|[key q] rest IH]; intros f sg a Member; [contradiction|].
  destruct Member as [Same|Member].
  - inversion Same; subst key q. cbn [cfn_go]. now rewrite Nat.eqb_refl.
  - destruct key, q; cbn [cfn_go]; try (eapply IH; exact Member).
    match goal with
    | |- context [if ?same then _ else _] => destruct same; [reflexivity|eapply IH; exact Member]
    end.
Qed.

Local Lemma present_method : forall entries receiver method th sg a,
  In (IMth receiver method th, ProvidedMethod sg a) entries ->
  is_some (cmth_go receiver method entries) = true.
Proof.
  induction entries as [|[key q] rest IH]; intros receiver method th sg a Member; [contradiction|].
  destruct Member as [Same|Member].
  - inversion Same; subst key q. cbn [cmth_go].
    now rewrite Nat.eqb_refl, PublicTypeFacts.ty_eqb_refl.
  - destruct key, q; cbn [cmth_go]; try (eapply IH; exact Member).
    match goal with
    | |- context [if ?same then _ else _] =>
        destruct same; [reflexivity|eapply IH; exact Member]
    end.
Qed.

Local Lemma present_functor : forall entries u fx,
  In (IFun u, ProvidedFunctor fx) entries -> is_some (cfun_go u entries) = true.
Proof.
  induction entries as [|[key q] rest IH]; intros u fx Member; [contradiction|].
  destruct Member as [Same|Member].
  - inversion Same; subst key q. cbn [cfun_go]. now rewrite Nat.eqb_refl.
  - destruct key, q; cbn [cfun_go]; try (eapply IH; exact Member).
    match goal with
    | |- context [if ?same then _ else _] => destruct same; [reflexivity|eapply IH; exact Member]
    end.
Qed.

Local Lemma canonical_key_in_entries : forall C k,
  PublicCallRealize.KeysMatch (c_provisions C) -> provides C k ->
  (match k with ITy _ => False | _ => True end) -> k = provision_id k ->
  In k (map fst (c_provisions C)).
Proof.
  intros C [t|v|f|r m th|u] Match Provided Typed Canonical; try contradiction.
  - cbn [provides] in Provided. destruct (nlook v (c_vals C)) as [ty|] eqn:Found; [|contradiction].
    destruct (PublicCallRealize.vals_ilook _ _ _ Match Found) as [a Entry].
    exact (in_map fst _ _ (PublicFormation.ilook_in_key _ _ _ _ Entry)).
  - cbn [provides] in Provided. destruct (nlook f (c_fns C)) as [sg|] eqn:Found; [|contradiction].
    destruct (PublicCallRealize.fns_ilook _ _ _ Match Found) as [a Entry].
    exact (in_map fst _ _ (PublicFormation.ilook_in_key _ _ _ _ Entry)).
  - destruct th; [|discriminate Canonical].
    cbn [provides] in Provided. destruct (mlook r m (c_mths C)) as [sg|] eqn:Found; [|contradiction].
    destruct (PublicCallRealize.mths_ilook _ _ _ _ Match Found) as [a Entry].
    exact (in_map fst _ _ (PublicFormation.ilook_in_key _ _ _ _ Entry)).
  - cbn [provides] in Provided. destruct (nlook u (c_funs C)) as [fx|] eqn:Found; [|contradiction].
    pose proof (PublicCallRealize.funs_ilook _ _ _ Match Found) as Entry.
    exact (in_map fst _ _ (PublicFormation.ilook_in_key _ _ _ _ Entry)).
Qed.

Theorem lambda_need_keys_exact : forall C,
  PublicCallRealize.KeysMatch (c_provisions C) ->
  let keys := EaBind.lambda_need_keys C (PIdx (pindex (c_provisions C))) in
  NoDup keys /\ forall k, In k keys <->
    (provides C k /\ (match k with ITy _ => False | _ => True end) /\ k = provision_id k).
Proof.
  intros C Match. unfold EaBind.lambda_need_keys. cbn zeta.
  destruct (iuniq_fast_exact (map fst (c_provisions C))) as [Distinct Members].
  split; [now apply NoDup_filter|]. intros k. rewrite filter_In, Members.
  split.
  - intros [_ Guard].
    destruct k; try discriminate Guard.
    all: apply andb_true_iff in Guard as [Provided Canonical].
    all: split; [eapply (EaSatisfiesSound.providesb_i_sound C true); exact Provided|].
    all: split; [exact I|now apply PublicCallRealize.item_eqb_spec].
  - intros (Provided & Typed & Canonical).
    pose proof (canonical_key_in_entries _ _ Match Provided Typed Canonical) as Member.
    split; [exact Member|].
    apply in_map_iff in Member as [[key p] [Same Row]]. cbn [fst] in Same. subst key.
    pose proof (Match _ _ Row) as Shape. pose proof (indexed_member _ _ _ Row) as Bucket.
    destruct k as [t|v|f|r m th|u], p;
      try contradiction Shape; try contradiction Typed.
    all: try (destruct th; [|contradiction Shape]).
    all: try contradiction Shape.
    all: apply andb_true_iff; split; [|now apply PublicCallRealize.item_eqb_spec].
    + eapply present_value; exact Bucket.
    + eapply present_function; exact Bucket.
    + eapply present_method; exact Bucket.
    + eapply present_functor; exact Bucket.
Qed.

Theorem captured_cx_keys_match : forall C caps capvals,
  PublicCallRealize.KeysMatch (c_provisions C) ->
  PublicCallRealize.KeysMatch (c_provisions (EaBind.captured_cx C caps capvals)).
Proof.
  intros C caps capvals Match. unfold EaBind.captured_cx.
  assert (Capture : forall pairs start,
    PublicCallRealize.KeysMatch (c_provisions start) ->
    PublicCallRealize.KeysMatch (c_provisions
      (fold_left (fun C' q => let '(n, v) := q in
        match slook n (c_locals C) with
        | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
        | None => C' end) pairs start))).
  {
    induction pairs as [|[name value] pairs IH]; intros start Keys; [exact Keys|].
    cbn [fold_left]. apply IH. destruct (slook name (c_locals C)) as [[ty mu]|]; [|exact Keys].
    unfold provide, set_val, set_provision, add_local. cbn [c_provisions ilook fst item_eqb].
    rewrite Nat.eqb_refl. cbn [c_provisions].
    assert (Val : PublicCallRealize.KeysMatch [(IVal value, ProvidedVal ty [])]).
    { intros k p [Equal|[]]. inversion Equal. exact I. }
    apply PublicCallRealize.keys_match_set; [|exact Val].
    now apply PublicCallRealize.keys_match_set.
  }
  apply Capture. exact Match.
Qed.
