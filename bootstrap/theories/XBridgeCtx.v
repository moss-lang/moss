(* Context type meanings, including main's root context, follow from the
   declarative context walk. They need not be assumed by the bridge. *)
From Stdlib Require Import List Arith.
From NM Require Import ExecutionModel.
Import ListNotations.
Import ExecutionModel.Input ExecutionModel.Elaboration.
Local Open Scope list_scope.

Definition self_meanings (tm : list (nat * ty)) : Prop :=
  forall t x, nlook t tm = Some x -> x = TAbs t.

Lemma self_meanings_nil : self_meanings [].
Proof. intros t x H. discriminate. Qed.

Lemma self_meanings_add : forall tm t, self_meanings tm ->
  self_meanings (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end).
Proof.
  intros tm t H. destruct (nlook t tm); [exact H|].
  intros k x E. cbn in E. destruct (Nat.eqb_spec k t) as [->|N].
  - injection E as <-. reflexivity.
  - exact (H k x E).
Qed.

Lemma add_tys_self_meanings : forall xs tm, self_meanings tm ->
  self_meanings (add_tys xs tm).
Proof.
  induction xs as [|x xs IH]; intros tm H; [exact H|].
  apply IH. apply self_meanings_add. exact H.
Qed.

Fixpoint item_self_meanings W p ob tm sp es tm'
  (H : ItemEntries W p ob tm sp es tm') {struct H} :
  self_meanings tm -> self_meanings tm'
with items_self_meanings W p ob tm ss es tm'
  (H : ItemsEntries W p ob tm ss es tm') {struct H} :
  self_meanings tm -> self_meanings tm'.
Proof.
  - destruct H; intro Hs; try exact Hs.
    + apply self_meanings_add. exact Hs.
    + eapply items_self_meanings; eassumption.
  - destruct H; intro Hs; [exact Hs| |].
    + eapply items_self_meanings; [eassumption|].
      eapply item_self_meanings; eassumption.
    + eapply items_self_meanings; [eassumption|].
      eapply items_self_meanings; eassumption.
Qed.

Lemma root_items_self_meanings : forall W q es0 tm its es tm',
  RootItems W q es0 tm its es tm' -> self_meanings tm -> self_meanings tm'.
Proof.
  intros W q es0 tm its es tm' H. induction H; intro Hs; auto.
  apply IHRootItems. apply add_tys_self_meanings. exact Hs.
Qed.

Lemma root_ctx_self_meanings : forall W s es0 tm es tm',
  RootCtx W s es0 tm es tm' -> self_meanings tm -> self_meanings tm'.
Proof.
  intros W s es0 tm es tm' H. destruct H; intro Hs; [exact Hs|].
  eapply root_items_self_meanings; eassumption.
Qed.

Lemma nlook_map_meanings : forall sigma tm t,
  nlook t (map (fun q => (fst q, app_subst sigma (snd q))) tm) =
  option_map (app_subst sigma) (nlook t tm).
Proof.
  intros sigma tm t. induction tm as [|[k x] tm IH]; [reflexivity|].
  cbn. destruct (Nat.eqb t k); [reflexivity|exact IH].
Qed.

Theorem ctx_tys_self : forall W s C ns,
  CtxOf W s C ns -> forall t x,
  nlook t (c_ty C) = Some x -> x = canon C (TAbs t).
Proof.
  intros W s C ns (d & es & tm & er & tmr & sigma & Hd & Hi & Hr & Hm & -> & Hn) t x Hx.
  pose proof (items_self_meanings _ _ _ _ _ _ _ Hi self_meanings_nil) as Htm.
  pose proof (root_ctx_self_meanings _ _ _ _ _ _ Hr Htm) as Htmr.
  cbn [c_ty entries_cx] in Hx. rewrite nlook_map_meanings in Hx.
  destruct (nlook t tmr) as [y|] eqn:Ey; [|discriminate].
  injection Hx as <-. rewrite (Htmr t y Ey). reflexivity.
Qed.

Lemma nlook_type_key : forall (tm : list (nat * ty)) t x,
  nlook t tm = Some x -> In t (map fst tm).
Proof.
  intros tm. induction tm as [|[k y] tm IH]; intros t x H; [discriminate|].
  cbn in H |- *. destruct (Nat.eqb_spec t k) as [->|N].
  - left. reflexivity.
  - right. exact (IH t x H).
Qed.

Definition type_meanings (W : world) (tm : list (nat * ty)) : Prop :=
  forall t x, nlook t tm = Some x -> kind_sym W t = Some DKType.

Lemma type_meanings_nil : forall W, type_meanings W [].
Proof. intros W t x H. discriminate. Qed.

Lemma type_meanings_add : forall W tm t,
  type_meanings W tm -> kind_sym W t = Some DKType ->
  type_meanings W (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end).
Proof.
  intros W tm t Ht Hk. destruct (nlook t tm); [exact Ht|].
  intros k x E. cbn in E. destruct (Nat.eqb_spec k t) as [->|N].
  - exact Hk.
  - exact (Ht k x E).
Qed.

Lemma add_tys_type_meanings : forall W xs tm,
  (forall t, In t xs -> kind_sym W t = Some DKType) ->
  type_meanings W tm -> type_meanings W (add_tys xs tm).
Proof.
  intros W xs. induction xs as [|x xs IH]; intros tm Hxs Ht; [exact Ht|].
  apply IH.
  - intros t H. apply Hxs. right. exact H.
  - apply type_meanings_add; [exact Ht|]. apply Hxs. left. reflexivity.
Qed.

Fixpoint item_type_meanings W p ob tm sp es tm'
  (H : ItemEntries W p ob tm sp es tm') {struct H} :
  type_meanings W tm -> type_meanings W tm'
with items_type_meanings W p ob tm ss es tm'
  (H : ItemsEntries W p ob tm ss es tm') {struct H} :
  type_meanings W tm -> type_meanings W tm'.
Proof.
  - destruct H; intro Hs; try exact Hs.
    + apply type_meanings_add; assumption.
    + eapply items_type_meanings; eassumption.
  - destruct H; intro Hs; [exact Hs| |].
    + eapply items_type_meanings; [eassumption|].
      eapply item_type_meanings; eassumption.
    + eapply items_type_meanings; [eassumption|].
      eapply items_type_meanings; eassumption.
Qed.


Lemma root_items_type_meanings : forall W,
  (forall u t, Fixed W u t -> kind_sym W t = Some DKType) ->
  forall q es0 tm its es tm', RootItems W q es0 tm its es tm' ->
  type_meanings W tm -> type_meanings W tm'.
Proof.
  intros W Hfixed q es0 tm its es tm' H.
  induction H; intro Ht; auto.
  apply IHRootItems. apply add_tys_type_meanings; [|exact Ht].
  intros t Ht0. apply (Hfixed u t). apply H4. exact Ht0.
Qed.

Lemma root_ctx_type_meanings : forall W,
  (forall u t, Fixed W u t -> kind_sym W t = Some DKType) ->
  forall s es0 tm es tm', RootCtx W s es0 tm es tm' ->
  type_meanings W tm -> type_meanings W tm'.
Proof.
  intros W Hfixed s es0 tm es tm' H. destruct H; intro Ht; [exact Ht|].
  eapply root_items_type_meanings; eassumption.
Qed.

Theorem ctx_tys_declared : forall W,
  (forall u t, Fixed W u t -> kind_sym W t = Some DKType) ->
  forall s C ns, CtxOf W s C ns ->
  forall t x, nlook t (c_ty C) = Some x -> kind_sym W t = Some DKType.
Proof.
  intros W Hfixed s C ns
    (d & es & tm & er & tmr & sigma & Hd & Hi & Hr & Hm & -> & Hn) t x Hx.
  pose proof (items_type_meanings _ _ _ _ _ _ _ Hi (type_meanings_nil W)) as Htm.
  pose proof (root_ctx_type_meanings _ Hfixed _ _ _ _ _ Hr Htm) as Htmr.
  cbn [c_ty entries_cx] in Hx. rewrite nlook_map_meanings in Hx.
  destruct (nlook t tmr) as [y|] eqn:Ey; [|discriminate].
  exact (Htmr t y Ey).
Qed.

Lemma type_key_nlook : forall (tm : list (nat * ty)) t,
  In t (map fst tm) -> exists x, nlook t tm = Some x.
Proof.
  intros tm. induction tm as [|[k y] tm IH]; intros t H; [destruct H|].
  cbn in H |- *. destruct (Nat.eqb_spec t k) as [->|N].
  - exists y. reflexivity.
  - destruct H as [E|H]; [congruence|]. exact (IH t H).
Qed.

Definition type_domain_incl (tm tm' : list (nat * ty)) : Prop :=
  forall t, nlook t tm <> None -> nlook t tm' <> None.

Lemma type_domain_refl : forall tm, type_domain_incl tm tm.
Proof. intros tm t H. exact H. Qed.

Lemma type_domain_trans : forall a b c,
  type_domain_incl a b -> type_domain_incl b c -> type_domain_incl a c.
Proof. intros a b c H1 H2 t H. apply H2, H1, H. Qed.

Lemma type_domain_add : forall tm t,
  type_domain_incl tm (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end).
Proof.
  intros tm t. destruct (nlook t tm); [apply type_domain_refl|].
  intros k Hk. cbn. destruct (Nat.eqb k t); [discriminate|exact Hk].
Qed.

Lemma add_tys_domain : forall xs tm, type_domain_incl tm (add_tys xs tm).
Proof.
  induction xs as [|x xs IH]; intro tm; [apply type_domain_refl|].
  eapply type_domain_trans; [apply type_domain_add|apply IH].
Qed.

Fixpoint item_type_domain W p ob tm sp es tm'
  (H : ItemEntries W p ob tm sp es tm') {struct H} : type_domain_incl tm tm'
with items_type_domain W p ob tm ss es tm'
  (H : ItemsEntries W p ob tm ss es tm') {struct H} : type_domain_incl tm tm'.
Proof.
  - destruct H; try apply type_domain_refl.
    + apply type_domain_add.
    + eapply items_type_domain; eassumption.
  - destruct H; [apply type_domain_refl| |].
    + eapply type_domain_trans.
      * eapply item_type_domain; eassumption.
      * eapply items_type_domain; eassumption.
    + eapply type_domain_trans; eapply items_type_domain; eassumption.
Qed.


Lemma add_tys_contains : forall xs tm t, In t xs -> nlook t (add_tys xs tm) <> None.
Proof.
  induction xs as [|x xs IH]; intros tm t H; [destruct H|].
  destruct H as [->|H].
  - apply (add_tys_domain xs _ t). destruct (nlook t tm) eqn:E.
    + congruence.
    + cbn. rewrite Nat.eqb_refl. discriminate.
  - apply IH. exact H.
Qed.

Lemma root_items_type_domain : forall W q es0 tm its es tm',
  RootItems W q es0 tm its es tm' -> type_domain_incl tm tm'.
Proof.
  intros W q es0 tm its es tm' H. induction H; [apply type_domain_refl| |exact IHRootItems].
  eapply type_domain_trans; [apply add_tys_domain|exact IHRootItems].
Qed.

Lemma root_ctx_type_domain : forall W s es0 tm es tm',
  RootCtx W s es0 tm es tm' -> type_domain_incl tm tm'.
Proof.
  intros W s es0 tm es tm' H. destruct H; [apply type_domain_refl|].
  eapply root_items_type_domain; eassumption.
Qed.

Fixpoint item_fun_fixed W p ob tm sp es tm'
  (H : ItemEntries W p ob tm sp es tm') {struct H} :
  forall u fx, In (NFun u fx) es -> forall t, Fixed W u t -> nlook t tm' <> None
with items_fun_fixed W p ob tm ss es tm'
  (H : ItemsEntries W p ob tm ss es tm') {struct H} :
  forall u fx, In (NFun u fx) es -> forall t, Fixed W u t -> nlook t tm' <> None.
Proof.
  - destruct H; intros u0 fx0 Hin t0 Hfix; try (destruct Hin as [E|[]]; discriminate E);
      try solve [destruct Hin].
    + destruct Hin as [E|[]]. injection E as -> ->. apply H2. exact Hfix.
    + eapply items_fun_fixed; eassumption.
  - destruct H as [|p ob tm sp r es1 tm1 es2 tm2 H1 Hr|p ob tm l r es1 tm1 tl tr rest es2 tm2 H1 _ _ Hr];
      intros u fx Hin t Hfix; [destruct Hin| |].
    + apply in_app_or in Hin as [Hin|Hin].
      * eapply items_type_domain; [exact Hr|].
        eapply item_fun_fixed; [exact H1|exact Hin|exact Hfix].
      * eapply items_fun_fixed; [exact Hr|exact Hin|exact Hfix].
    + apply in_app_or in Hin as [Hin|[Hin|Hin]].
      * eapply items_type_domain; [exact Hr|]. eapply items_fun_fixed; [exact H1|exact Hin|exact Hfix].
      * discriminate Hin.
      * eapply items_fun_fixed; [exact Hr|exact Hin|exact Hfix].
Qed.

Lemma root_items_fun_fixed : forall W q es0 tm its es tm',
  RootItems W q es0 tm its es tm' ->
  forall u fx, In (NFun u fx) es -> forall t, Fixed W u t -> nlook t tm' <> None.
Proof.
  intros W q es0 tm its es tm' H. induction H; intros u0 fx0 Hin t Hfix;
    [destruct Hin| |eapply IHRootItems; eassumption].
  apply in_app_or in Hin as [Hin|Hin].
  - apply in_map_iff in Hin as (x & E & _). discriminate E.
  - destruct Hin as [E|Hin].
    + injection E as -> ->. eapply root_items_type_domain;[eassumption|].
      apply add_tys_contains. apply H4. exact Hfix.
    + eapply IHRootItems; eassumption.
Qed.

Lemma root_ctx_fun_fixed : forall W s es0 tm es tm',
  RootCtx W s es0 tm es tm' ->
  forall u fx, In (NFun u fx) es -> forall t, Fixed W u t -> nlook t tm' <> None.
Proof.
  intros W s es0 tm es tm' H. destruct H; intros u fx Hin t Hfix;
    [destruct Hin|]. eapply root_items_fun_fixed; eassumption.
Qed.

Theorem ctx_fun_fixed : forall W s C ns,
  CtxOf W s C ns -> forall u, In (IFun u) ns ->
  forall t, Fixed W u t -> nlook t (c_ty C) <> None.
Proof.
  intros W s C ns
    (d & es & tm & er & tmr & sigma & Hd & Hi & Hr & Hm & -> & [Hnd Hns])
    u Hin t Hfix.
  destruct (proj1 (Hns (IFun u)) Hin) as (e & He & Hkey).
  destruct e; cbn in Hkey; try discriminate. injection Hkey as ->.
  assert (Hdom : nlook t tmr <> None).
  { apply in_app_or in He as [He|He].
    - apply (root_ctx_type_domain _ _ _ _ _ _ Hr).
      eapply items_fun_fixed; eassumption.
    - eapply root_ctx_fun_fixed; eassumption. }
  cbn [c_ty entries_cx]. rewrite nlook_map_meanings.
  destruct (nlook t tmr); [discriminate|contradiction].
Qed.
