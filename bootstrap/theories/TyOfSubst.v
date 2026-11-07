(* ------------------------------------------------------------------------ *)
(* TyOfSubst.v — Part V's TyOf (V.2) under a substitution (Track A).        *)
(*                                                                           *)
(* A type expression read with meanings tm, then substituted by s, is the   *)
(* same expression read with the meanings substituted (tyof_subst): every  *)
(* type TyOf builds is a meaning of tm or is built by a type former, which *)
(* substitution commutes with, a union's members being nominals.  So the   *)
(* elaborator's types, read with its own meanings, give the relation's     *)
(* reading with the canonical ones (σ applied: BodyTy.pd_ty_holds).  And a *)
(* type read with closed meanings is closed (tyof_closed).                  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith.
From NM Require Import Audit.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.
Notation asub := Audit.app_subst.

Section Subst.
Variable W : A.world.
Variable s : Audit.subst.
Import A.

Definition smap (l : list (nat * ty)) : list (nat * ty) := map (fun q => (fst q, asub s (snd q))) l.

Lemma asub_nom : forall n xs, asub s (TNom n xs) = TNom n (smap xs).
Proof. reflexivity. Qed.

Lemma asub_union : forall ms, asub s (TUnion ms) = TUnion (map (asub s) ms).
Proof. reflexivity. Qed.

Lemma nlook_app' : forall X k (l m : list (nat * X)), nlook k (l ++ m) = match nlook k l with Some v => Some v | None => nlook k m end.
Proof. intros X k l m. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Lemma nlook_smap : forall k l, nlook k (smap l) = option_map (asub s) (nlook k l).
Proof. intros k l. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Definition SubRel (tm tm' : list (nat * ty)) : Prop := forall k, nlook k tm' = option_map (asub s) (nlook k tm).

Lemma subrel_app : forall l tm tm', SubRel tm tm' -> SubRel (l ++ tm) (smap l ++ tm').
Proof.
  intros l tm tm' H k. rewrite !nlook_app', nlook_smap. destruct (nlook k l); [reflexivity | apply H].
Qed.

(* members commute with s on the members of a union TyOf reads *)
Lemma members_sub : forall t, (forall m, In m (members t) -> exists n xs, m = TNom n xs) ->
  members (asub s t) = map (asub s) (members t).
Proof.
  intros [|g|n xs|ms] H; cbn [members].
  - destruct (H TUnit (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - destruct (H (TAbs g) (or_introl eq_refl)) as (? & ? & E). discriminate E.
  - rewrite asub_nom. reflexivity.
  - rewrite asub_union. reflexivity.
Qed.

Lemma flat_sub : forall ts, Forall (fun m => exists n xs, m = TNom n xs /\ n <> rec_head) (flat_map members ts) ->
  flat_map members (map (asub s) ts) = map (asub s) (flat_map members ts).
Proof.
  intros ts H. induction ts as [|t r IH]; [reflexivity|]. cbn [map flat_map] in *.
  apply Forall_app in H as [H1 H2]. rewrite map_app, (IH H2). f_equal.
  apply members_sub. intros m Hm. rewrite Forall_forall in H1. destruct (H1 m Hm) as (n & xs & E & _). eauto.
Qed.

Fixpoint tyof_subst p tm th t a (H : TyOf W p tm th t a) {struct H} :
  forall tm', SubRel tm tm' -> TyOf W p tm' (option_map (asub s) th) t (asub s a)
with br_subst p tm g x l (H : Brackets W p tm g x l) {struct H} :
  forall tm', SubRel tm tm' -> Brackets W p tm' g x (smap l).
Proof.
  - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros tm' Hs.
    + apply TyNever.
    + apply TyUnit.
    + apply TyThis.
    + rewrite asub_nom. apply TyRec.
      induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [constructor|]. constructor; [|exact IH].
      split; [exact E1 | exact (tyof_subst _ _ _ _ _ Hft tm' Hs)].
    + subst flat. rewrite asub_union, <- (flat_sub ts Fm). apply TyUnion with (ts := map (asub s) ts).
      * clear Fm Nd. induction F as [|m t ms' ts' Ht Fr IH]; [constructor|].
        constructor; [exact (tyof_subst _ _ _ _ _ Ht tm' Hs) | exact IH].
      * reflexivity.
      * rewrite (flat_sub ts Fm). rewrite Forall_forall in Fm |- *. intros m Hm.
        apply in_map_iff in Hm as (m0 & <- & Hm0). destruct (Fm m0 Hm0) as (n & xs & -> & Hn).
        rewrite asub_nom. eauto.
      * rewrite (flat_sub ts Fm), map_map.
        replace (map (fun x => head (asub s x)) (flat_map members ts)) with (map head (flat_map members ts)); [exact Nd|].
        apply map_ext_in. intros m Hm. rewrite Forall_forall in Fm. destruct (Fm m Hm) as (n & xs & -> & _).
        rewrite asub_nom. reflexivity.
    + eapply TyType; [exact R | exact K | rewrite Hs, L; reflexivity].
    + rewrite asub_nom. eapply TyNom; [exact R | exact K | exact Kk | exact (br_subst _ _ _ _ _ B tm' Hs)|].
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

End Subst.

Section Closed.
Variable W : A.world.
Import A.

Definition Closed (tm : list (nat * ty)) : Prop := forall k t, nlook k tm = Some t -> gens t = [].

Lemma gens_nom_nil : forall n xs, Forall (fun p => gens (snd p) = []) xs -> gens (TNom n xs) = [].
Proof. intros n xs H. induction H as [|p r Hp Hr IH]; [reflexivity|]. cbn in *. rewrite Hp. exact IH. Qed.

Lemma gens_union_nil : forall ms, Forall (fun m => gens m = []) ms -> gens (TUnion ms) = [].
Proof. intros ms H. induction H as [|p r Hp Hr IH]; [reflexivity|]. cbn in *. rewrite Hp. exact IH. Qed.

Lemma gens_union_parts : forall ms, gens (TUnion ms) = [] -> Forall (fun m => gens m = []) ms.
Proof.
  intros ms. induction ms as [|m r IH]; intro H; [constructor|]. cbn in H.
  apply app_eq_nil in H as [H1 H2]. constructor; [exact H1 | exact (IH H2)].
Qed.

Lemma closed_app : forall l tm, Closed l -> Closed tm -> Closed (l ++ tm).
Proof.
  intros l tm Hl Ht k t H. rewrite nlook_app' in H. destruct (nlook k l) eqn:E; [injection H as <-; exact (Hl k _ E) | exact (Ht k t H)].
Qed.

Lemma nlook_in_pair : forall X k (l : list (nat * X)) x,
  nlook k l = Some x -> In (k, x) l.
Proof.
  intros X k l. induction l as [|[a y] l IH]; intros x H; [discriminate|].
  cbn in H. destruct (Nat.eqb_spec k a) as [->|N].
  - injection H as <-. left. reflexivity.
  - right. exact (IH x H).
Qed.

Fixpoint tyof_closed p tm th t a (H : TyOf W p tm th t a) {struct H} :
  Closed tm -> (forall x, th = Some x -> gens x = []) -> gens a = []
with br_closed p tm g x l (H : Brackets W p tm g x l) {struct H} : Closed tm -> Closed l.
Proof.
  - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros Ct Cth.
    + reflexivity.
    + reflexivity.
    + exact (Cth t eq_refl).
    + apply gens_nom_nil. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; [constructor|].
      constructor; [exact (tyof_closed _ _ _ _ _ Hft Ct Cth) | exact IH].
    + subst flat. apply gens_union_nil. clear Fm Nd.
      induction F as [|m t ms' ts' Ht Fr IH]; [constructor|]. cbn [flat_map]. apply Forall_app. split; [|exact IH].
      pose proof (tyof_closed _ _ _ _ _ Ht Ct Cth) as G.
      destruct t as [|g|n xs|ms0]; cbn [members]; try (constructor; [exact G | constructor]).
      exact (gens_union_parts ms0 G).
    + exact (Ct _ _ L).
    + apply gens_nom_nil. pose proof (closed_app _ _ (br_closed _ _ _ _ _ B Ct) Ct) as Cb.
      clear B. induction F as [|r ra rs ras [E1 E2] Fr IH]; [constructor|]. constructor; [exact (Cb _ _ E2) | exact IH].
    + refine (tyof_closed _ _ _ _ _ T (closed_app _ _ (br_closed _ _ _ _ _ B Ct) Ct) _). discriminate.
  - destruct H as [p tm g|p tm g bs l F]; intro Ct.
    + intros k t E. discriminate E.
    + assert (Cl : Forall (fun q => gens (snd q) = []) l).
      { induction F as [|b0 tb bs' l' (sp & sa & E1 & E2 & E3 & E4 & Ht) Fr IH]; [constructor|].
        constructor; [exact (tyof_closed _ _ _ _ _ Ht Ct (fun x E => ltac:(discriminate E)))|exact IH]. }
      intros k t E. apply nlook_in_pair in E.
      apply in_rev in E. rewrite Forall_forall in Cl. exact (Cl (k, t) E).
Qed.

End Closed.
