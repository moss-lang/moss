(* ------------------------------------------------------------------------ *)
(* MergeWf.v — a formed telescope's merge substitution is formed (Track B). *)
(*                                                                           *)
(* For a telescope formed item by item (wf_tele), every binding of the      *)
(* merged σ ([D43]) is a type formed in the merged context, and so is every *)
(* requirement read through σ (tele_canon_wf).  The unifier binds only      *)
(* σ-images of subterms of the receivers and θs being merged, which OkMth   *)
(* forms (unify_wf); subterms of formed types are formed; formation         *)
(* survives σ growing over the same items, because a nominal union          *)
(* member's head does not change under substitution (wf_ty_mono).          *)
(*                                                                           *)
(* Over a Σ whose tags' telescopes are type items only (tags_typeonly), as  *)
(* XBridge's Σ is: a tag's satisfaction then reads only the domain of its   *)
(* instantiation, so formation reads only which type items are in force.   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss.
Import ListNotations.

Section MergeWf.

Variable Sg : gsig.

Definition tags_typeonly : Prop :=
  forall n D tau0, g_tag Sg n = Some (D, tau0) -> Forall (fun i => is_ty_item i = true) D.

Hypothesis Htags : tags_typeonly.

(* ---- Telescopes of type items do not merge. ----------------------------- *)

Lemma merge_item_ty_sigma : forall Phi t Phi',
  merge_item Phi (ITy t) = Some Phi' -> cS Phi' = cS Phi /\ incl (cA Phi') (cA Phi ++ [ITy t]).
Proof.
  intros Phi t Phi' H. unfold merge_item in H.
  destruct (find_same_key (cS Phi) (cA Phi) (ITy t)) as [j|] eqn:Ef.
  - destruct (find_same_key_in _ _ _ Ef) as [_ Hk].
    destruct j as [u| | | |]; simpl in Hk; try discriminate.
    apply Nat.eqb_eq in Hk. subst u. simpl in H. rewrite Nat.eqb_refl in H.
    destruct (item_subst_eqb (cS Phi) (ITy t) (ITy t) && sigma_regular_b (cS Phi) (cA Phi)
              && idempotent_b (cS Phi)); [| discriminate].
    injection H as <-. split; [reflexivity |]. simpl. intros x Hx. apply in_or_app. left. exact Hx.
  - injection H as <-. split; [reflexivity | apply incl_refl].
Qed.

Lemma merge_tele_typeonly : forall D Phi Phi',
  Forall (fun i => is_ty_item i = true) D -> merge_tele Phi D = Some Phi' ->
  Forall (fun i => is_ty_item i = true) (cA Phi) ->
  cS Phi' = cS Phi /\ Forall (fun i => is_ty_item i = true) (cA Phi').
Proof.
  induction D as [|i D IH]; intros Phi Phi' HD H HA; simpl in H.
  - injection H as <-. split; [reflexivity | exact HA].
  - inversion HD as [|i0 D0 Hi HD']; subst.
    destruct i as [t| | | |]; simpl in Hi; try discriminate.
    destruct (merge_item Phi (ITy t)) as [Phi1|] eqn:Em; [| discriminate].
    destruct (merge_item_ty_sigma _ _ _ Em) as [Hs Hin].
    destruct (IH Phi1 Phi' HD' H) as [Hs' HA'].
    + rewrite Forall_forall in HA |- *. intros x Hx. apply Hin in Hx.
      apply in_app_or in Hx. destruct Hx as [Hx | [<- | []]]; [exact (HA x Hx) | reflexivity].
    + split; [rewrite Hs', Hs; reflexivity | exact HA'].
Qed.

(* A tag's satisfaction reads only the domain of θ. *)
Lemma sat_typeonly : forall Phi th D Phi' th',
  Forall (fun i => is_ty_item i = true) D -> sat Sg Phi th D -> map fst th' = map fst th ->
  sat Sg Phi' th' D.
Proof.
  intros Phi th D Phi' th' HD [PhiD [Htc [Hdom _]]] Hf.
  destruct (merge_tele_typeonly D empty_ctx PhiD HD Htc (Forall_nil _)) as [Hs HA].
  exists PhiD. split; [exact Htc |]. split; [rewrite Hf; exact Hdom |]. split.
  - eapply Forall_impl; [| exact HA]. intros i Hi. left. exact Hi.
  - unfold merge_respects. rewrite Hs. constructor.
Qed.

(* ---- Formation reads only the type items in force. ---------------------- *)

Lemma avail_ty_of_in' : forall Phi t, In (ITy t) (cA Phi) -> avail Phi (ITy t).
Proof.
  intros Phi t H. unfold avail, avail_b. apply existsb_exists. exists (ITy t).
  split; [exact H|]. unfold item_subst_eqb. apply item_eqb_eq. reflexivity.
Qed.

Lemma heads_of_nom : forall s ms, Forall is_nom ms -> heads_of s ms = map member_head ms.
Proof.
  intros s ms H. unfold heads_of. apply map_ext_in. intros m Hm.
  rewrite Forall_forall in H. destruct (H m Hm) as [n [th ->]]. reflexivity.
Qed.

Definition tyin (Phi Phi' : ctx) : Prop := forall t, In (ITy t) (cA Phi) -> In (ITy t) (cA Phi').

Lemma wf_ty_mono : forall Phi tau, wf_ty Sg Phi tau -> forall Phi', tyin Phi Phi' -> wf_ty Sg Phi' tau.
Proof.
  intros Phi tau H Phi' Hin. revert tau H.
  apply (@wf_ty_ind' Sg Phi (fun tau => wf_ty Sg Phi' tau)).
  - constructor.
  - intros t Hav. apply WfNeed. apply avail_ty_of_in'. apply Hin. exact (avail_ty_in Hav).
  - intros n D tau0 th Hg Hsat _ IH. eapply WfTag; [exact Hg | | exact IH].
    exact (sat_typeonly _ _ _ _ _ (Htags _ _ _ Hg) Hsat eq_refl).
  - intros ms _ IH Hnom Hnd. apply WfUnion; [exact IH | exact Hnom |].
    rewrite (heads_of_nom _ _ Hnom). rewrite (heads_of_nom _ _ Hnom) in Hnd. exact Hnd.
  - intros fts Hr _ IH. apply WfRec; [exact Hr | exact IH].
Qed.

(* ---- Substitution and subterms. ----------------------------------------- *)

Definition ranges_wf (Phi : ctx) (s : subst) : Prop := Forall (fun p => wf_ty Sg Phi (snd p)) s.

Lemma lookup_subst_in : forall t s r, lookup_subst t s = Some r -> In (t, r) s.
Proof.
  intros t s r. induction s as [|[t' r'] s IH]; simpl; [discriminate |].
  destruct (Nat.eqb t t') eqn:E; [| intros H; right; exact (IH H)].
  intros H. injection H as <-. apply Nat.eqb_eq in E. subst. left. reflexivity.
Qed.

Lemma map_fst_map_snd : forall (f : ty -> ty) (th : subst), map fst (map_snd f th) = map fst th.
Proof. intros f th. induction th as [|p th IH]; simpl; [reflexivity | rewrite IH; reflexivity]. Qed.

Lemma wf_ty_subst : forall Phi tau rho, wf_ty Sg Phi tau -> ranges_wf Phi rho ->
  wf_ty Sg Phi (app_subst rho tau).
Proof.
  intros Phi tau rho H Hr. revert tau H.
  apply (@wf_ty_ind' Sg Phi (fun tau => wf_ty Sg Phi (app_subst rho tau))).
  - constructor.
  - intros t Hav. simpl. destruct (lookup_subst t rho) as [r|] eqn:E.
    + apply lookup_subst_in in E. unfold ranges_wf in Hr. rewrite Forall_forall in Hr.
      exact (Hr _ E).
    + apply WfNeed. exact Hav.
  - intros n D tau0 th Hg Hsat _ IH. rewrite app_subst_nom. eapply WfTag; [exact Hg | |].
    + exact (sat_typeonly _ _ _ _ _ (Htags _ _ _ Hg) Hsat (map_fst_map_snd _ _)).
    + clear -IH. induction IH as [|p th Hp _ IHth]; constructor; [exact Hp | exact IHth].
  - intros ms _ IH Hnom Hnd. rewrite app_subst_union.
    assert (Hnom' : Forall is_nom (map (app_subst rho) ms)).
    { clear -Hnom. induction Hnom as [|m ms [n [th ->]] _ IHm]; constructor; [| exact IHm].
      exists n, (map_snd (app_subst rho) th). reflexivity. }
    apply WfUnion.
    + clear -IH. induction IH; constructor; assumption.
    + exact Hnom'.
    + rewrite (heads_of_nom _ _ Hnom'). rewrite (heads_of_nom _ _ Hnom) in Hnd.
      replace (map member_head (map (app_subst rho) ms)) with (map member_head ms); [exact Hnd |].
      rewrite map_map. apply map_ext_in. intros m Hm. rewrite Forall_forall in Hnom.
      destruct (Hnom m Hm) as [n [th ->]]. reflexivity.
  - intros fts Hr0 _ IH. rewrite app_subst_nom. apply WfRec; [exact Hr0 |].
    clear -IH. induction IH as [|p th Hp _ IHth]; constructor; [exact Hp | exact IHth].
Qed.

Lemma wf_ty_nom_args : forall Phi n th, wf_ty Sg Phi (TNom n th) -> Forall (fun p => wf_ty Sg Phi (snd p)) th.
Proof. intros Phi n th H. inversion H; subst; assumption. Qed.

Lemma wf_ty_union_members : forall Phi ms, wf_ty Sg Phi (TUnion ms) -> Forall (wf_ty Sg Phi) ms.
Proof. intros Phi ms H. inversion H; subst; assumption. Qed.

Lemma ranges_wf_extend : forall Phi acc t tau, ranges_wf Phi acc -> wf_ty Sg Phi tau ->
  ranges_wf Phi (extend_subst acc t tau).
Proof.
  intros Phi acc t tau Ha Ht. unfold extend_subst, ranges_wf. apply Forall_app. split.
  - unfold map_snd. rewrite Forall_map. eapply Forall_impl; [| exact Ha].
    intros p Hp. simpl. apply wf_ty_subst; [exact Hp |]. constructor; [exact Ht | constructor].
  - constructor; [exact Ht | constructor].
Qed.

(* ---- The unifier binds only formed types. ------------------------------- *)

Lemma unify_wf : forall Phi fuel,
  (forall acc a b acc', unify_tys fuel acc a b = Some acc' ->
     ranges_wf Phi acc -> wf_ty Sg Phi a -> wf_ty Sg Phi b -> ranges_wf Phi acc')
  /\ (forall acc th th' acc', unify_rows fuel acc th th' = Some acc' ->
     ranges_wf Phi acc -> Forall (fun p => wf_ty Sg Phi (snd p)) th ->
     Forall (fun p => wf_ty Sg Phi (snd p)) th' -> ranges_wf Phi acc')
  /\ (forall acc ms ms' acc', unify_alls fuel acc ms ms' = Some acc' ->
     ranges_wf Phi acc -> Forall (wf_ty Sg Phi) ms -> Forall (wf_ty Sg Phi) ms' -> ranges_wf Phi acc').
Proof.
  intros Phi fuel. induction fuel as [|k [IHt [IHr IHa]]].
  - split; [| split]; intros; simpl in *; discriminate.
  - split; [| split].
    + intros acc a b acc' H Hacc Ha Hb. simpl in H.
      pose proof (wf_ty_subst _ _ _ Ha Hacc) as Ha'. pose proof (wf_ty_subst _ _ _ Hb Hacc) as Hb'.
      destruct (app_subst acc a) as [|t|n th|ms] eqn:Ea;
        destruct (app_subst acc b) as [|u|n' th'|ms'] eqn:Eb;
        rewrite ?Ea in Ha'; rewrite ?Eb in Hb';
        try discriminate;
        repeat match goal with
               | H : (if ?c then _ else _) = Some _ |- _ => destruct c; [try discriminate | try discriminate]
               end;
        try (injection H as <-; first [exact Hacc | apply ranges_wf_extend; assumption]).
      * (* TNom, TNom *) exact (IHr _ _ _ _ H Hacc (wf_ty_nom_args _ _ _ Ha') (wf_ty_nom_args _ _ _ Hb')).
      * (* TUnion, TUnion *)
        exact (IHa _ _ _ _ H Hacc (wf_ty_union_members _ _ Ha') (wf_ty_union_members _ _ Hb')).
    + intros acc th th' acc' H Hacc Hth Hth'. simpl in H.
      destruct th as [|p r]; destruct th' as [|p' r']; try discriminate.
      * injection H as <-. exact Hacc.
      * destruct (Nat.eqb (fst p) (fst p')); [| discriminate].
        inversion Hth as [|? ? Hp Hr]; subst. inversion Hth' as [|? ? Hp' Hr']; subst.
        destruct (unify_tys k acc (snd p) (snd p')) as [acc1|] eqn:E1; [| discriminate].
        exact (IHr _ _ _ _ H (IHt _ _ _ _ E1 Hacc Hp Hp') Hr Hr').
    + intros acc ms ms' acc' H Hacc Hms Hms'. simpl in H.
      destruct ms as [|a r]; destruct ms' as [|a' r']; try discriminate.
      * injection H as <-. exact Hacc.
      * inversion Hms as [|? ? Hp Hr]; subst. inversion Hms' as [|? ? Hp' Hr']; subst.
        destruct (unify_tys k acc a a') as [acc1|] eqn:E1; [| discriminate].
        exact (IHa _ _ _ _ H (IHt _ _ _ _ E1 Hacc Hp Hp') Hr Hr').
Qed.

(* ---- The invariant of formation. ---------------------------------------- *)

Definition mth_items_wf (Phi : ctx) (A : list item) : Prop :=
  forall rt m th, In (IMth rt m th) A -> wf_ty Sg Phi rt /\ Forall (fun p => wf_ty Sg Phi (snd p)) th.

Definition merge_inv (Phi : ctx) : Prop := ranges_wf Phi (cS Phi) /\ mth_items_wf Phi (cA Phi).

Lemma ranges_wf_mono : forall Phi Phi' s, ranges_wf Phi s -> tyin Phi Phi' -> ranges_wf Phi' s.
Proof.
  intros Phi Phi' s H Hin. eapply Forall_impl; [| exact H]. intros p Hp. exact (wf_ty_mono _ _ Hp _ Hin).
Qed.

Lemma mth_items_wf_mono : forall Phi Phi' A, mth_items_wf Phi A -> tyin Phi Phi' -> mth_items_wf Phi' A.
Proof.
  intros Phi Phi' A H Hin rt m th Hi. destruct (H rt m th Hi) as [Hr Hth]. split.
  - exact (wf_ty_mono _ _ Hr _ Hin).
  - eapply Forall_impl; [| exact Hth]. intros p Hp. exact (wf_ty_mono _ _ Hp _ Hin).
Qed.

Lemma item_ok_mth : forall Phi rt m th, item_ok Sg Phi (IMth rt m th) ->
  wf_ty Sg Phi rt /\ Forall (fun p => wf_ty Sg Phi (snd p)) th.
Proof. intros Phi rt m th H. inversion H; subst. split; assumption. Qed.

Lemma merge_item_inv : forall Phi i Phi', merge_inv Phi -> item_ok Sg Phi i ->
  merge_item Phi i = Some Phi' -> merge_inv Phi'.
Proof.
  intros Phi i Phi' [Hs Hm] Hok H. unfold merge_item in H.
  destruct (find_same_key (cS Phi) (cA Phi) i) as [j|] eqn:Ef.
  - destruct (unify_items (merge_fuel (cS Phi) j i) (cS Phi) j i) as [s'|] eqn:Eu; [| discriminate].
    destruct (item_subst_eqb s' j i && sigma_regular_b s' (cA Phi) && idempotent_b s'); [| discriminate].
    injection H as <-.
    assert (Hin : tyin Phi (mkCtx (cA Phi) s')) by (intros t Ht; exact Ht).
    split; [| exact (mth_items_wf_mono _ _ _ Hm Hin)].
    apply (ranges_wf_mono Phi); [| exact Hin]. simpl.
    destruct (find_same_key_in _ _ _ Ef) as [Hj _].
    destruct j as [| | |rt m th|]; destruct i as [| | |rt' m' th'|]; cbn [unify_items] in Eu; try discriminate;
      try (match goal with E : (if ?c then _ else _) = _ |- _ => destruct c end;
           [injection Eu as <-; exact Hs | discriminate]).
    destruct (Nat.eqb m m'); [| discriminate].
    destruct (Hm rt m th Hj) as [Hrt Hth].
    destruct (item_ok_mth _ _ _ _ Hok) as [Hrt' Hth'].
    revert Eu. match goal with |- context [unify_tys ?f ?a ?b ?c] =>
      destruct (unify_tys f a b c) as [acc1|] eqn:E1; [| discriminate];
      destruct (unify_wf Phi f) as [Ht [Hr _]] end.
    intros Eu. exact (Hr _ _ _ _ Eu (Ht _ _ _ _ E1 Hs Hrt Hrt') Hth Hth').
  - injection H as <-.
    assert (Hin : tyin Phi (mkCtx (cA Phi ++ [i]) (cS Phi)))
      by (intros t Ht; simpl; apply in_or_app; left; exact Ht).
    split; [exact (ranges_wf_mono _ _ _ Hs Hin) |].
    intros rt m th Hi. simpl in Hi. apply in_app_or in Hi. destruct Hi as [Hi | [Hi | []]].
    + exact (mth_items_wf_mono _ _ _ Hm Hin rt m th Hi).
    + subst i. destruct (item_ok_mth _ _ _ _ Hok) as [Hrt Hth]. split.
      * exact (wf_ty_mono _ _ Hrt _ Hin).
      * eapply Forall_impl; [| exact Hth]. intros p Hp. exact (wf_ty_mono _ _ Hp _ Hin).
Qed.

Lemma wf_tele_inv : forall Phi D Phi', wf_tele Sg Phi D Phi' -> merge_inv Phi -> merge_inv Phi'.
Proof.
  intros Phi D Phi' H. induction H as [Phi | Phi i D Phi1 Phi2 Hok Hm _ IH]; intros Hinv.
  - exact Hinv.
  - apply IH. exact (merge_item_inv _ _ _ Hinv Hok Hm).
Qed.

(* Every binding of a formed telescope's merge is formed in its context. *)
Theorem tele_sigma_wf : forall D PhiD, wf_tele Sg empty_ctx D PhiD ->
  Forall (fun p => wf_ty Sg PhiD (snd p)) (cS PhiD).
Proof.
  intros D PhiD H. refine (proj1 (wf_tele_inv _ _ _ H _)).
  split; [constructor | intros rt m th []].
Qed.

Lemma tyreqs_in_item : forall t A, In t (tyreqs A) -> In (ITy t) A.
Proof.
  intros t A H. unfold tyreqs in H. apply in_flat_map in H. destruct H as [i [Hi Ht]].
  destruct i; simpl in Ht; try contradiction. destruct Ht as [<- | []]. exact Hi.
Qed.

(* So is every requirement read through it. *)
Theorem tele_canon_wf : forall D PhiD, wf_tele Sg empty_ctx D PhiD ->
  forall t, In t (tyreqs (cA PhiD)) -> wf_ty Sg PhiD (app_subst (cS PhiD) (TAbs t)).
Proof.
  intros D PhiD H t Ht. simpl. destruct (lookup_subst t (cS PhiD)) as [r|] eqn:E.
  - pose proof (tele_sigma_wf D PhiD H) as Hs. rewrite Forall_forall in Hs.
    exact (Hs _ (lookup_subst_in _ _ _ E)).
  - apply WfNeed. apply avail_ty_of_in'. exact (tyreqs_in_item _ _ Ht).
Qed.

End MergeWf.
