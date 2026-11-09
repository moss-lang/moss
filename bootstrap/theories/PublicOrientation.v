(* Merge orientation (criteria section 2, coherence). `MostGeneralMerge`
   leaves open which member of a merged class is its representative; two
   most general merges of one telescope differ only in that choice. The
   merge classes themselves do not depend on it: two types one most
   general merge reads as one type, every other one does too. A clause
   stated by class (as `CallTheta`'s principal clause states which
   requirements form a wholly unbound class) is therefore the same under
   every orientation. *)
From Stdlib Require Import List.
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
