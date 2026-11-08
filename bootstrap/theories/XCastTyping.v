(* Local typing laws for native opaque heap handles and checked wrappers. *)
From Stdlib Require Import List Bool PrimInt63 PeanoNat.
From NM Require Import ExecutionModel CoreMoss HTrie MossX XHeap XTyping XCast.
Import ListNotations.
Set Implicit Arguments.

Section CastTyping.
Variables (Sg : gsig) (Rp : nsym -> option rep) (P : xprog).

Lemma handle_type_subst : forall k tau s,
  handle_type Rp k tau -> handle_type Rp k (app_subst s tau).
Proof.
  intros k tau s Hty. induction Hty.
  - rewrite app_subst_nom. constructor; assumption.
  - rewrite app_subst_union. eapply HtUnion; [exact IHHty|].
    apply in_map; assumption.
Qed.

Lemma handle_type_inst_subst : forall pay theta s k,
  handle_type Rp k (app_subst theta pay) ->
  handle_type Rp k (app_subst (map_snd (app_subst s) theta) pay).
Proof.
  intros pay. induction pay using ty_ind'; intros theta s k Hty.
  - inversion Hty.
  - simpl in *. rewrite lookup_map_snd.
    destruct (lookup_subst t theta); simpl in *; [apply handle_type_subst; assumption|inversion Hty].
  - rewrite app_subst_nom in *. inversion Hty; subst; constructor; assumption.
  - rewrite app_subst_union in *. inversion Hty as [|u ms' Hmember Hin]; subst.
    apply in_map_iff in Hin as [p [<- Hin]].
    eapply HtUnion; [|apply in_map; exact Hin].
    rewrite Forall_forall in H. eapply (H p Hin); exact Hmember.
Qed.

Lemma contents_map_subst : forall s th t,
  contents (map_snd (app_subst s) th) t = app_subst s (contents th t).
Proof.
  intros. unfold contents. rewrite lookup_map_snd.
  destruct (lookup_subst t th); reflexivity.
Qed.

Lemma handle_values : forall k tau, handle_type Rp k tau ->
  forall store c a, store (key a) = Some (k, c) ->
  xvt Sg Rp P store (XVRef a) tau /\ xvt Sg Rp P store (XVRefAt k c a) tau.
Proof.
  intros k tau Hty. induction Hty; intros store c a Hcell.
  - split; [eapply XvLegacyHandle|eapply XvHandle]; eauto.
  - destruct (IHHty store c a Hcell) as [Hlegacy Hchecked]. split;
      eapply XvInj; eauto.
Qed.

Lemma legacy_handle_value : forall k tau H c a,
  handle_type Rp k tau -> H (key a) = Some (k, c) ->
  xvt Sg Rp P H (XVRef a) tau.
Proof. intros k tau H c a Hty Hcell; exact (proj1 (@handle_values k tau Hty H c a Hcell)). Qed.

Lemma checked_handle_value : forall k tau H c a,
  handle_type Rp k tau -> H (key a) = Some (k, c) ->
  xvt Sg Rp P H (XVRefAt k c a) tau.
Proof. intros k tau H c a Hty Hcell; exact (proj2 (@handle_values k tau Hty H c a Hcell)). Qed.

Lemma raw_header_inv : forall H k c a tau,
  xvt Sg Rp P H (XVRefAt k c a) tau ->
  H (key a) = Some (k, c) /\ handle_type Rp k tau.
Proof.
  intros H k c a tau Hv. remember (XVRefAt k c a) as w eqn:E.
  induction Hv; inversion E; subst; eauto using HtNom, HtUnion.
  destruct (IHHv eq_refl) as [Hcell Hty]. split; eauto using HtUnion.
Qed.

Lemma cast_tag_payload : forall H n th w D pay (k : bool) x,
  Rp n = Some (if k then RArr x else RRef x) ->
  g_tag Sg n = Some (D, pay) ->
  xvt Sg Rp P H (XVTag n w) (TNom n th) ->
  xvt Sg Rp P H w (app_subst th pay).
Proof.
  intros H n th w D pay k x Hrep Htag Hv.
  inversion Hv; subst; try (destruct k; congruence).
  all: repeat match goal with
       | H1 : g_tag Sg ?head = Some (?D1, ?p1), H2 : g_tag Sg ?head = Some (?D2, ?p2) |- _ =>
           rewrite H1 in H2; inversion H2; subst; clear H2
       end.
  all: eauto using legacy_handle_value, checked_handle_value.
Qed.

Lemma gc_schema_guard : forall k n th x spec h expected,
  gc_schema Sg Rp P k n th x -> lookup_cast n (xp_cast P) = Some spec ->
  xc_handle spec = Some (h, expected) -> contents th h = expected.
Proof.
  intros k n th x spec h expected [spec' [Hlookup [_ [_ [Hguard _]]]]] Hspec Hhandle.
  rewrite Hlookup in Hspec. injection Hspec as <-.
  rewrite Hhandle in Hguard. exact Hguard.
Qed.

Lemma gc_receiver_typed : forall H w t k c,
  xvt Sg Rp P H w t -> is_ref Sg Rp P k t c ->
  exists n a, XS.xgc_receiver w = XVTag n (XVRef a) /\ H (key a) = Some (k, c).
Proof.
  intros H w t k c Hv [n [th [x [-> [Hr [Hc Hschema]]]]]].
  destruct k; inversion Hv; subst; try congruence;
    try (eexists; eexists; split; [reflexivity|congruence]).
  all: exfalso; match goal with
       | Hneq : contents ?th ?h <> ?expected,
         Hlook : lookup_cast ?n (xp_cast P) = Some ?spec,
         Hhandle : xc_handle ?spec = Some (?h, ?expected) |- _ =>
           apply Hneq; eapply gc_schema_guard; eauto
       end.
Qed.

Lemma checked_reference_value : forall H (k : bool) n th x c a,
  Rp n = Some (if k then RArr x else RRef x) ->
  contents th x = c -> gc_schema Sg Rp P k n th x ->
  H (key a) = Some (k, c) ->
  xvt Sg Rp P H (XVTag n (XVRefAt k c a)) (TNom n th).
Proof.
  intros H k n th x c a Hr <- [spec [_ [_ [_ [_ [D [pay [Hg Hpay]]]]]]]] Hcell.
  destruct k; simpl in Hr; [eapply XvArrAt|eapply XvRefAt]; eauto.
Qed.

Lemma gc_schema_subst : forall Fb (WF : xwf Sg Rp P Fb) s k n th x,
  gc_schema Sg Rp P k n th x ->
  gc_schema Sg Rp P k n (map_snd (app_subst s) th) x.
Proof.
  intros Fb WF s k n th x [spec [Hlook [Hkind [Hheld [Hgate [D [pay [Htag Hpayload]]]]]]]].
  exists spec. split; [exact Hlook|]. split; [exact Hkind|]. split; [exact Hheld|]. split.
  - destruct (xc_handle spec) as [[h expected]|] eqn:E; [|exact I].
    rewrite contents_map_subst, Hgate. apply app_subst_closed.
    eapply xwf_cast_closed; eauto.
  - exists D, pay. split; [exact Htag|].
    apply handle_type_inst_subst; exact Hpayload.
Qed.

Definition cast_keys (spec : xcastspec) (th : subst) : Prop :=
  (exists tau, lookup_subst (xc_held spec) th = Some tau) /\
  forall h expected, xc_handle spec = Some (h, expected) ->
    exists tau, lookup_subst h th = Some tau.

Lemma cast_lookup_domain : forall t th, In t (map fst th) ->
  exists tau, lookup_subst t th = Some tau.
Proof.
  intros t th. induction th as [|[u tau] th IH]; simpl; intros Hin; [contradiction|].
  destruct (Nat.eqb t u) eqn:E; [eauto|].
  destruct Hin as [->|Hin]; [rewrite Nat.eqb_refl in E; discriminate|]. apply IH; exact Hin.
Qed.

Lemma cast_keys_sat : forall Fb (WF : xwf Sg Rp P Fb) Phi n spec D pay th,
  lookup_cast n (xp_cast P) = Some spec -> g_tag Sg n = Some (D, pay) ->
  sat Sg Phi th D -> cast_keys spec th.
Proof.
  intros Fb WF Phi n spec D pay th Hlook Htag [PhiD [Htc [Hdom _]]].
  destruct (@xwf_cast_params Sg Rp P Fb WF n spec D pay Hlook Htag) as [Hheld Hhandle].
  split.
  - apply cast_lookup_domain. rewrite Hdom. apply (proj2 (tele_ctx_tyreqs D Htc _)); exact Hheld.
  - intros h expected E. apply cast_lookup_domain. rewrite Hdom.
    apply (proj2 (tele_ctx_tyreqs D Htc _)). eapply Hhandle; exact E.
Qed.

Lemma xcast_typed : forall H d n actual logical spec D pay w,
  lookup_cast n (xp_cast P) = Some spec ->
  Rp n = Some (if xc_array spec then RArr (xc_held spec) else RRef (xc_held spec)) ->
  g_tag Sg n = Some (D, pay) -> cast_keys spec actual ->
  (forall t tau, lookup_subst t actual = Some tau -> xground d tau = contents logical t) ->
  xvt Sg Rp P H w (app_subst logical pay) ->
  match XS.xcast P d n actual w with
  | XOk v => xvt Sg Rp P H v (TNom n logical)
  | XTrap _ => True
  | _ => False
  end.
Proof.
  intros H d n actual logical spec D pay w Hlook Hr Htag Hkeys Hground Hv.
  unfold XS.xcast. rewrite Hlook.
  destruct (match xc_handle spec with
             | None => true
             | Some (h, expected) => ty_eqb (xground d
                 (match lookup_subst h actual with Some t => t | None => TAbs h end)) expected
             end) eqn:Een.
  - destruct (lookup_subst (xc_held spec) actual) as [tau|] eqn:Ety; [|exact I].
    destruct w as [|n0 w| | | | | | |k meta a]; try exact I.
    destruct (Bool.eqb (xc_array spec) k && ty_eqb (xground d tau) meta) eqn:Em; [|exact I].
    apply andb_true_iff in Em as [Hk Hmeta]. apply Bool.eqb_prop in Hk.
    apply ty_eqb_eq in Hmeta.
    pose proof (Hground _ _ Ety) as Eground.
    assert (Econtent : meta = contents logical (xc_held spec)) by congruence.
    subst k; subst meta.
    destruct (raw_header_inv Hv) as [Hcell Hpayload].
    rewrite Eground in Hcell |- *.
    destruct (xc_array spec); simpl in Hr; [eapply XvArrAt|eapply XvRefAt]; eauto.
  - rename Een into Hgate.
    destruct (xc_handle spec) as [[h expected]|] eqn:Ehandle; simpl in Hgate; [|discriminate].
    destruct (proj2 Hkeys h expected Ehandle) as [tau Hlookup].
    rewrite Hlookup in Hgate. pose proof (Hground _ _ Hlookup) as Eground.
    eapply XvReboundRef with(k:=xc_array spec); eauto.
    intro Eequal. assert (ty_eqb (xground d tau) expected = true).
    { apply ty_eqb_eq. congruence. } congruence.
Qed.
End CastTyping.
