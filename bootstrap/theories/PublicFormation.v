(* Formation of types and contexts (T29). A formed type is built the way
   TyOf builds types: every nominal is applied at exactly its requirements,
   and every union has nominal members with distinct heads, at every depth.
   Substitution by formed types keeps types formed, and so do the readings
   TyOf, Brackets and SigOf over formed type tables. *)
From Stdlib Require Import List String Arith.
From NM Require Import Audit PublicTypeRead.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Formation.
Variable W : A.world.

Fixpoint TyFormed (t : ty) : Prop :=
  match t with
  | TMachine _ | TUnit | TAbs _ => True
  | TRecord fs => (fix go (fs : list (string * ty)) := match fs with [] => True | (_, u) :: r => TyFormed u /\ go r end) fs
  | TNom n xs => map fst xs = A.w_reqs W n /\
      (fix go (xs : list (nat * ty)) := match xs with [] => True | (_, u) :: r => TyFormed u /\ go r end) xs
  | TUnion ms => Forall (fun m => exists n xs, m = TNom n xs) ms /\ NoDup (map A.head ms) /\
      (fix go (ms : list ty) := match ms with [] => True | u :: r => TyFormed u /\ go r end) ms
  end.

Definition TmFormed (tm : list (nat * ty)) : Prop := forall x u, In (x, u) tm -> TyFormed u.

Lemma record_formed : forall fs, TyFormed (TRecord fs) <-> forall n u, In (n, u) fs -> TyFormed u.
Proof.
  intros fs. cbn. induction fs as [|[n u] r IH]; split; intros H.
  - intros n u [].
  - exact I.
  - destruct H as [Hu Hr]. intros n' u' [E|I]; [injection E as -> ->; exact Hu|exact (proj1 IH Hr n' u' I)].
  - split; [exact (H n u (or_introl eq_refl))|]. apply IH. intros n' u' I. exact (H n' u' (or_intror I)).
Qed.

Lemma args_formed : forall (xs : list (nat * ty)),
  (fix go (xs : list (nat * ty)) := match xs with [] => True | (_, u) :: r => TyFormed u /\ go r end) xs <->
  TmFormed xs.
Proof.
  intros xs. induction xs as [|[n u] r IH]; split; intros H.
  - intros x v [].
  - exact I.
  - destruct H as [Hu Hr]. intros x v [E|I]; [injection E as -> ->; exact Hu|exact (proj1 IH Hr x v I)].
  - split; [exact (H n u (or_introl eq_refl))|]. apply IH. intros x v I. exact (H x v (or_intror I)).
Qed.

Lemma members_formed : forall (ms : list ty),
  (fix go (ms : list ty) := match ms with [] => True | u :: r => TyFormed u /\ go r end) ms <->
  Forall TyFormed ms.
Proof.
  intros ms. induction ms as [|u r IH]; split; intros H.
  - constructor.
  - exact I.
  - destruct H as [Hu Hr]. constructor; [exact Hu|exact (proj1 IH Hr)].
  - inversion H; subst. split; [assumption|apply IH; assumption].
Qed.

Lemma nom_formed : forall n xs, TyFormed (TNom n xs) <-> map fst xs = A.w_reqs W n /\ TmFormed xs.
Proof. intros n xs. cbn [TyFormed]. rewrite args_formed. reflexivity. Qed.

Lemma union_formed : forall ms, TyFormed (TUnion ms) <->
  Forall (fun m => exists n xs, m = TNom n xs) ms /\ NoDup (map A.head ms) /\ Forall TyFormed ms.
Proof. intros ms. cbn [TyFormed]. rewrite members_formed. reflexivity. Qed.

Definition SubstFormed (s : subst) : Prop := forall x u, lookup_subst x s = Some u -> TyFormed u.

Lemma subst_formed : forall t s, SubstFormed s -> TyFormed t -> TyFormed (app_subst s t).
Proof.
  fix IH 1. intros [m|fields| |g|n xs|ms] s Sf Ft.
  - exact I.
  - rewrite subst_record. cbn [TyFormed] in Ft |- *.
    induction fields as [|[f u] r Tail]; cbn in Ft |- *; [exact I|].
    destruct Ft as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
  - exact I.
  - cbn [app_subst]. destruct (lookup_subst g s) as [u|] eqn:L; [exact (Sf g u L)|exact I].
  - rewrite subst_nom. cbn [TyFormed] in Ft |- *. destruct Ft as [Keys Args].
    split; [unfold PublicTypeRead.smap; rewrite map_map; exact Keys|]. clear Keys.
    unfold PublicTypeRead.smap. induction xs as [|[x u] r Tail]; cbn in Args |- *; [exact I|].
    destruct Args as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
  - rewrite subst_union. cbn [TyFormed] in Ft |- *. destruct Ft as [Nom [Nd Fm]].
    split; [|split].
    + rewrite Forall_forall in Nom |- *. intros m' I. apply in_map_iff in I as [m [<- I]].
      destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. eauto.
    + replace (map A.head (map (app_subst s) ms)) with (map A.head ms); [exact Nd|].
      rewrite map_map. apply map_ext_in. intros m I. rewrite Forall_forall in Nom.
      destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. reflexivity.
    + clear Nom Nd. induction ms as [|u r Tail]; cbn in Fm |- *; [exact I|].
      destruct Fm as [Fu Fr]. split; [exact (IH u s Sf Fu)|exact (Tail Fr)].
Qed.

Lemma tm_app_formed : forall l m, TmFormed l -> TmFormed m -> TmFormed (l ++ m).
Proof. intros l m Fl Fm x u I. apply in_app_or in I as [I|I]; [exact (Fl x u I)|exact (Fm x u I)]. Qed.

Lemma members_of_formed : forall t, TyFormed t -> Forall TyFormed (A.members t).
Proof.
  intros [m|fs| |g|n xs|ms] Ft; cbn [A.members]; try (constructor; [exact Ft|constructor]).
  apply union_formed in Ft as [_ [_ Fm]]. exact Fm.
Qed.

Lemma keys_of : forall (reqs : list nat) (xs : list (nat * ty)) (R : nat -> ty -> Prop),
  Forall2 (fun r ra => fst ra = r /\ R r (snd ra)) reqs xs -> map fst xs = reqs.
Proof. intros reqs xs R F; induction F as [|r ra rs ras [E _] _ IH]; cbn; [reflexivity|congruence]. Qed.

Fixpoint tyof_formed p tm th a t (D : A.TyOf W p tm th a t) {struct D} :
  TmFormed tm -> (forall x, th = Some x -> TyFormed x) -> TyFormed t
with br_formed p tm h x l (D : A.Brackets W p tm h x l) {struct D} : TmFormed tm -> TmFormed l.
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g0 k bs args R K Kk B F|p tm th pa a g0 d n body bs tau R D A B T];
      intros Tm Th.
    + cbn. split; [constructor|split; [constructor|exact I]].
    + exact I.
    + exact (Th t eq_refl).
    + cbn [TyFormed]. clear Nd. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [exact I|].
      destruct ft as [f t]. split; [exact (tyof_formed _ _ _ _ _ Hft Tm Th)|exact IH].
    + apply union_formed. split; [exact Fm|split; [exact Nd|]]. subst flat. clear Fm Nd.
      induction F as [|a t' ms' ts' Ht Fr IH]; [constructor|]. cbn [flat_map].
      apply Forall_app. split; [|exact IH].
      apply members_of_formed. exact (tyof_formed _ _ _ _ _ Ht Tm Th).
    + exact (Tm t tau (nlook_in _ _ _ _ L)).
    + apply nom_formed. split; [exact (keys_of _ _ (fun r v => A.nlook r (bs ++ tm)%list = Some v) F)|].
      pose proof (tm_app_formed _ _ (br_formed _ _ _ _ _ B Tm) Tm) as Btm. clear B.
      intros x u I. clear Kk. induction F as [|r ra rs ras [E1 L1] Fr IH]; [contradiction|].
      destruct I as [E|I]; [subst ra; exact (Btm _ _ (nlook_in _ _ _ _ L1))|exact (IH I)].
    + apply (tyof_formed _ _ _ _ _ T); [exact (tm_app_formed _ _ (br_formed _ _ _ _ _ B Tm) Tm)|intros x E; discriminate].
  - destruct D as [p tm h|p tm h bs l F]; intros Tm; [intros x u []|].
    intros x u I. apply (proj2 (in_rev _ _)) in I.
    induction F as [|b0 tb bs' l' (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; [contradiction|].
    destruct I as [E|I]; [|exact (IH I)]. destruct tb as [y v]. injection E as -> ->.
    exact (tyof_formed _ _ _ _ _ T1 Tm (fun x E => ltac:(discriminate E))).
Qed.

End Formation.
