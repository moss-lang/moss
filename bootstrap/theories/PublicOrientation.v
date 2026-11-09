(* Merge orientation (criteria section 2, coherence). `MostGeneralMerge`
   leaves open which member of a merged class is its representative; two
   most general merges of one telescope differ only in that choice. The
   merge classes themselves do not depend on it: two types one most
   general merge reads as one type, every other one does too. A clause
   stated by class (as `CallTheta`'s principal clause states which
   requirements form a wholly unbound class) is therefore the same under
   every orientation. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Lemma mgm_unifies : forall s es, A.MostGeneralMerge s es -> A.Unifies s es.
Proof. intros s es [U _]. exact U. Qed.

(* Two most general merges of one telescope identify the same types. *)
Theorem mgm_same_classes : forall s s' es, A.MostGeneralMerge s es -> A.MostGeneralMerge s' es ->
  forall t u, app_subst s t = app_subst s u -> app_subst s' t = app_subst s' u.
Proof.
  intros s s' es [_ [_ [Gen _]]] M' t u E.
  rewrite <- (Gen s' (mgm_unifies s' es M') t), <- (Gen s' (mgm_unifies s' es M') u), E. reflexivity.
Qed.

Corollary mgm_classes_iff : forall s s' es, A.MostGeneralMerge s es -> A.MostGeneralMerge s' es ->
  forall t u, app_subst s t = app_subst s u <-> app_subst s' t = app_subst s' u.
Proof.
  intros s s' es M M' t u. split; [exact (mgm_same_classes s s' es M M' t u)|exact (mgm_same_classes s' s es M' M t u)].
Qed.

(* Each orientation reads a type as the other's reading, renamed: s' t is
   s' applied to s t, and back. *)
Theorem mgm_reorient : forall s s' es, A.MostGeneralMerge s es -> A.MostGeneralMerge s' es ->
  forall t, app_subst s' (app_subst s t) = app_subst s' t /\ app_subst s (app_subst s' t) = app_subst s t.
Proof.
  intros s s' es [U [_ [Gen _]]] [U' [_ [Gen' _]]] t. split; [exact (Gen s' U' t)|exact (Gen' s U t)].
Qed.

(* The merges clause of `Satisfies` is stated by class: a callee class
   wholly in force at the caller holds of the caller's meanings.  Where
   every symbol σ mentions is in force (an application's or a family's
   inputs), it gives every equation of σ, as the clause did when it was
   stated per equation. *)
Lemma functx_sub_function : forall W u C ns, A.FunCtx W u C ns ->
  forall g t, In (g, t) (A.c_sub C) -> lookup_subst g (A.c_sub C) = Some t.
Proof.
  intros W u C ns (d & ps & a & r & b & es & tm & tm' & σ & implicit & _ & _ & _ & _ & _ & MGM & -> & _).
  exact (proj2 (proj2 (proj2 (proj2 MGM)))).
Qed.

Lemma lookup_subst_pair : forall x (s : subst) t, lookup_subst x s = Some t -> In (x, t) s.
Proof.
  intros x s t. induction s as [|[y v] r IH]; cbn; [discriminate|].
  destruct (Nat.eqb x y) eqn:E; [intros H; injection H as <-; apply Nat.eqb_eq in E; subst; left; reflexivity|].
  intros H. right. exact (IH H).
Qed.

Theorem merges_by_pairs : forall C Cs (keys : list nat),
  (forall g t, In (g, t) (A.c_sub Cs) -> In g keys /\ incl (A.gens t) keys) ->
  (forall g t, In (g, t) (A.c_sub Cs) -> lookup_subst g (A.c_sub Cs) = Some t) ->
  (forall x, In x keys -> A.nlook x (A.c_ty C) <> None) ->
  (forall g, (forall y, (exists x, (x = g \/ In x (A.gens (A.canon Cs (TAbs g)))) /\
                          A.canon Cs (TAbs y) = A.canon Cs (TAbs x)) -> A.nlook y (A.c_ty C) <> None) ->
     A.canon C (app_subst (A.c_ty C) (TAbs g)) = A.canon C (app_subst (A.c_ty C) (A.canon Cs (TAbs g)))) ->
  forall g t, In (g, t) (A.c_sub Cs) ->
    (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty C) <> None) ->
    A.canon C (app_subst (A.c_ty C) (TAbs g)) = A.canon C (app_subst (A.c_ty C) t).
Proof.
  intros C Cs keys Dom Fun Force Cls g t Ig _.
  assert (Eg : A.canon Cs (TAbs g) = t) by (unfold A.canon; cbn; rewrite (Fun g t Ig); reflexivity).
  rewrite <- Eg. apply Cls. intros y (x & Ix & Ey). apply Force.
  assert (Kx : In x keys) by (destruct Ix as [->|Ix]; [exact (proj1 (Dom g t Ig))|rewrite Eg in Ix; exact (proj2 (Dom g t Ig) x Ix)]).
  unfold A.canon in Ey. cbn in Ey.
  destruct (lookup_subst y (A.c_sub Cs)) as [v|] eqn:Ly.
  - exact (proj1 (Dom y v (lookup_subst_pair _ _ _ Ly))).
  - destruct (lookup_subst x (A.c_sub Cs)) as [w|] eqn:Lx.
    + subst w. apply (proj2 (Dom x (TAbs y) (lookup_subst_pair _ _ _ Lx))). cbn. left. reflexivity.
    + injection Ey as ->. exact Kx.
Qed.
