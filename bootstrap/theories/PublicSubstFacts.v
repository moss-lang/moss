(* Occurrence and support facts for simultaneous type substitution. These
   facts sit below merge classes and context realization. *)
From Stdlib Require Import List Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicTypeRead.
From NM Require PublicOrientation.
Import AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.
Import A.

Lemma lookup_subst_in : forall x (th : subst), In x (map fst th) -> lookup_subst x th <> None.
Proof.
  intros x th. induction th as [|[y u] rest IH]; intros In'; [contradiction|].
  cbn in *. destruct (Nat.eqb_spec x y); [discriminate|]. destruct In' as [E|In']; [congruence|exact (IH In')].
Qed.

Lemma lookup_subst_nlook : forall x s, lookup_subst x s = nlook x s.
Proof.
  intros x s. induction s as [|[y value] rest IH]; [reflexivity|].
  cbn. now destruct (Nat.eqb x y).
Qed.

Lemma lookup_subst_in_pair : forall x s t, lookup_subst x s = Some t -> In (x,t) s.
Proof. intros x s t Read. rewrite lookup_subst_nlook in Read. eapply PublicTypeRead.nlook_in; exact Read. Qed.

Lemma gens_subst : forall t s x, In x (A.gens (app_subst s t)) ->
  exists y, In y (A.gens t) /\
    ((lookup_subst y s = None /\ x = y) \/ (exists u, lookup_subst y s = Some u /\ In x (A.gens u))).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s x Ix.
  - contradiction.
  - rewrite subst_record in Ix. cbn in Ix |- *. induction fields as [|[f t] rest Tail]; [contradiction|].
    cbn in Ix. apply in_app_or in Ix as [Ix|Ix].
    + destruct (IH t s x Ix) as [y [Iy R]]. exists y. split; [apply in_or_app; left; exact Iy|exact R].
    + destruct (Tail Ix) as [y [Iy R]]. exists y. split; [apply in_or_app; right; exact Iy|exact R].
  - contradiction.
  - exists g. split; [left; reflexivity|]. cbn in Ix.
    destruct (lookup_subst g s) as [u|]; [right; eauto|left; destruct Ix as [<-|[]]; auto].
  - rewrite subst_nom in Ix. unfold PublicTypeRead.smap in Ix. cbn in Ix |- *. induction th as [|[y t] rest Tail]; [contradiction|].
    cbn in Ix. apply in_app_or in Ix as [Ix|Ix].
    + destruct (IH t s x Ix) as [z [Iz R]]. exists z. split; [apply in_or_app; left; exact Iz|exact R].
    + destruct (Tail Ix) as [z [Iz R]]. exists z. split; [apply in_or_app; right; exact Iz|exact R].
  - rewrite subst_union in Ix. cbn in Ix |- *. induction ms as [|t rest Tail]; [contradiction|].
    cbn in Ix. apply in_app_or in Ix as [Ix|Ix].
    + destruct (IH t s x Ix) as [z [Iz R]]. exists z. split; [apply in_or_app; left; exact Iz|exact R].
    + destruct (Tail Ix) as [z [Iz R]]. exists z. split; [apply in_or_app; right; exact Iz|exact R].
Qed.

Lemma gens_subst_hit : forall t s x y, In x (A.gens t) ->
  In y (A.gens (app_subst s (TAbs x))) -> In y (A.gens (app_subst s t)).
Proof.
  fix IH 1. intros [m|fields| |g|n th|ms] s x y Ix Iy.
  - contradiction.
  - rewrite subst_record. cbn in Ix |- *. induction fields as [|[f t] rest Tail]; [contradiction|].
    cbn in Ix |- *. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app;
      [left; exact (IH t s x y Ix Iy)|right; exact (Tail Ix)].
  - contradiction.
  - cbn in Ix. destruct Ix as [<-|[]]. exact Iy.
  - rewrite subst_nom. unfold PublicTypeRead.smap. cbn in Ix |- *. induction th as [|[z t] rest Tail]; [contradiction|].
    cbn in Ix |- *. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app;
      [left; exact (IH t s x y Ix Iy)|right; exact (Tail Ix)].
  - rewrite subst_union. cbn in Ix |- *. induction ms as [|t rest Tail]; [contradiction|].
    cbn in Ix |- *. apply in_app_or in Ix as [Ix|Ix]; apply in_or_app;
      [left; exact (IH t s x y Ix Iy)|right; exact (Tail Ix)].
Qed.

Lemma gens_subst_iff : forall t s y, In y (A.gens (app_subst s t)) <->
  exists x, In x (A.gens t) /\ In y (A.gens (app_subst s (TAbs x))).
Proof.
  intros t s y. split.
  - intro InTy. destruct (gens_subst t s y InTy) as [x [Ix [[None ->]|[v [Some InV]]]]];
      exists x; split; try exact Ix; cbn [app_subst]; [rewrite None; left; reflexivity|rewrite Some; exact InV].
  - intros [x [Ix Iy]]. exact (gens_subst_hit t s x y Ix Iy).
Qed.

Lemma subst_scoped : forall (K : list nat) s t,
  (forall g u, In (g, u) s -> incl (gens u) K) -> incl (gens t) K -> incl (gens (app_subst s t)) K.
Proof.
  intros K s t Range Sc x Ix. destruct (gens_subst t s x Ix) as [y [Iy [[_ ->]|[u [L Iu]]]]]; [exact (Sc y Iy)|].
  exact (Range y u (PublicOrientation.lookup_subst_pair _ _ _ L) x Iu).
Qed.

Lemma lookup_on_keys : forall (f : nat -> ty) (l : list (nat * ty)) x, In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f (fst q))) l) = Some (f x).
Proof.
  intros f l x. induction l as [|[a t] r IH]; intros I; [contradiction|]. cbn in I |- *.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.
