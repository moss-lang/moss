(* Type equations ([D90]) are symmetric.  An equation item `l = r` adds to
   its telescope the entries of the references its sides write
   (`ty_heads l ++ ty_heads r`, each read as an unbracketed item, so a
   type symbol among them is an `NTy`), then `NEq tl tr`, its sides read.
   Written the other way round, `r = l`, it adds the same entries for the
   references, in the other order, then `NEq tr tl`.  The merge conditions
   (`Unifies`) and the telescope's generations see neither the order of the
   entries nor the order of an equation's two sides, so a most general
   merge of one telescope is a most general merge of the other, and the
   two identify the same types (`PublicOrientation.mgm_classes_iff`):
   `X = Y` and `Y = X` give the same merge classes. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit PublicOrientation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* Every entry of es is one of es', up to the order of an equation's
   sides. *)
Definition SameUpToSwap (es es' : list A.entry) : Prop :=
  forall e, In e es -> In e es' \/ exists a b, e = A.NEq a b /\ In (A.NEq b a) es'.

Lemma unifies_same_swap : forall es es' s, SameUpToSwap es es' -> A.Unifies s es' -> A.Unifies s es.
Proof.
  intros es es' s S [U Ue]. split.
  - intros e1 e2 k I1 I2 K1 K2.
    destruct (S e1 I1) as [J1|(a & b & -> & _)]; [|discriminate K1].
    destruct (S e2 I2) as [J2|(a & b & -> & _)]; [|discriminate K2].
    exact (U e1 e2 k J1 J2 K1 K2).
  - intros l r I. destruct (S _ I) as [J|(a & b & E & J)].
    + exact (Ue l r J).
    + injection E as -> ->. symmetry. exact (Ue b a J).
Qed.

Lemma gens_same_swap : forall es es', SameUpToSwap es' es ->
  forall g, In g (flat_map A.entry_gens es') -> In g (flat_map A.entry_gens es).
Proof.
  intros es es' S g I. apply in_flat_map in I as [e [Ie Ig]]. apply in_flat_map.
  destruct (S e Ie) as [J|(a & b & -> & J)]; [exists e; split; assumption|].
  exists (A.NEq b a). split; [exact J|]. cbn [A.entry_gens] in Ig |- *.
  apply in_app_or in Ig as [Ig|Ig]; apply in_or_app; [right|left]; exact Ig.
Qed.

Lemma mgm_same_swap : forall es es' s, SameUpToSwap es es' -> SameUpToSwap es' es ->
  A.MostGeneralMerge s es' -> A.MostGeneralMerge s es.
Proof.
  intros es es' s S S' (U & Idem & Gen & Dom & Fun). split; [exact (unifies_same_swap es es' s S U)|].
  split; [exact Idem|]. split.
  - intros s0 U0. exact (Gen s0 (unifies_same_swap es' es s0 S' U0)).
  - split; [|exact Fun]. intros g t I. destruct (Dom g t I) as [Dg Dt].
    split; [exact (gens_same_swap es es' S' g Dg)|]. intros y Iy. exact (gens_same_swap es es' S' y (Dt y Iy)).
Qed.

(* Telescopes with the same entries, up to their order and to the order of
   each equation's sides, give the same merge classes. *)
Theorem same_swap_classes : forall es es' s s', SameUpToSwap es es' -> SameUpToSwap es' es ->
  A.MostGeneralMerge s es -> A.MostGeneralMerge s' es' ->
  forall t u, app_subst s t = app_subst s u <-> app_subst s' t = app_subst s' u.
Proof.
  intros es es' s s' S S' M M'. exact (mgm_classes_iff s s' es M (mgm_same_swap es es' s' S S' M')).
Qed.

(* In particular `l = r` and `r = l`, in one telescope. *)
Corollary eq_swap_classes : forall pre post tl tr s s',
  A.MostGeneralMerge s (pre ++ A.NEq tl tr :: post) -> A.MostGeneralMerge s' (pre ++ A.NEq tr tl :: post) ->
  forall t u, app_subst s t = app_subst s u <-> app_subst s' t = app_subst s' u.
Proof.
  intros pre post tl tr s s' M M'. apply (same_swap_classes (pre ++ A.NEq tl tr :: post) (pre ++ A.NEq tr tl :: post) s s'); [| |exact M|exact M'].
  - intros e I. apply in_app_or in I as [I|[<-|I]].
    + left. apply in_or_app. left. exact I.
    + right. exists tl, tr. split; [reflexivity|]. apply in_or_app. right. left. reflexivity.
    + left. apply in_or_app. right. right. exact I.
  - intros e I. apply in_app_or in I as [I|[<-|I]].
    + left. apply in_or_app. left. exact I.
    + right. exists tr, tl. split; [reflexivity|]. apply in_or_app. right. left. reflexivity.
    + left. apply in_or_app. right. right. exact I.
Qed.
