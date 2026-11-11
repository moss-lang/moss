(* Cross-context need identity (criteria section 2, coherence).

   `bind F = G;` (BFunClosure, LinkBody) and `bind F = H;` (BFunCopy)
   relate two functor contexts, each with its own most general merge.
   The two merges choose their representatives independently, so one
   need can be spelled `X.m` in F's context and `Y.m` in G's. Audit
   therefore reads G's (H's) needs through F's merge (`canon_item Cu`)
   before comparing them with F's: F's congruence decides "same need".

   This file proves what makes that reading coherent:
   - `need_reading_orientation_free_F`: whether a key, read through F's
     merge, is one of F's needs does not depend on F's orientation;
   - `need_reading_orientation_free_G`: G's spelling of a key, read
     through F's merge, does not depend on G's orientation, among the
     orientations whose readings of F's inputs hold in F (LinkBody's
     input-reading clause);
   - `link_need_unambiguous`: no need class of G meets two need classes of
     F. The input-reading clause rules the ambiguous case out
     (tests/errors/sameneed_ambiguous.moss). A copy compares H's readings with
     F's only at F's applications (CopyInterpretation, AUDIT-CHANGES #96),
     where two of F's needs that one class of H meets are one key. *)
From Stdlib Require Import List.
From NM Require Import Audit PublicTypeRead PublicTagCheck PublicCallRealize PublicOrientation.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section SameNeed.
Variable W : A.world.

(* ---- F's orientation ------------------------------------------------- *)

Lemma map_snd_nil : forall (s : subst) (a : subst), map_snd (app_subst s) a = [] <-> a = [].
Proof. intros s [|q a]; split; intros E; try reflexivity; discriminate. Qed.

Lemma entry_key_read : forall s e k, A.entry_key s e = Some (app_subst_item s k) ->
  (exists r m g a, e = A.NMth r m g a /\ (exists r' a', k = IMth r' m a' /\ a' = [] /\ app_subst s r = app_subst s r')) \/
  (A.entry_key [] e = Some k /\ match k with IMth _ _ _ => False | _ => True end).
Proof.
  intros s [t|v t a|f g a|r m g a|u fx|eql eqr] k E; cbn in E; try discriminate.
  - injection E as E. destruct k; cbn in E; try discriminate. injection E as ->. right. split; [reflexivity|exact I].
  - injection E as E. destruct k; cbn in E; try discriminate. injection E as ->. right. split; [reflexivity|exact I].
  - injection E as E. destruct k as [| | |r' m' a'|]; cbn in E; try discriminate.
    injection E as Er Em Ea. subst m'. left. exists r, m, g, a. split; [reflexivity|].
    exists r', a'. split; [reflexivity|]. split; [exact (proj1 (map_snd_nil s a') (eq_sym Ea))|exact Er].
  - injection E as E. destruct k; cbn in E; try discriminate. injection E as ->. right. split; [reflexivity|exact I].
Qed.

Lemma entry_key_of : forall s e k, (exists r m g a, e = A.NMth r m g a /\ (exists r' a', k = IMth r' m a' /\ a' = [] /\ app_subst s r = app_subst s r')) \/
  (A.entry_key [] e = Some k /\ match k with IMth _ _ _ => False | _ => True end) ->
  A.entry_key s e = Some (app_subst_item s k).
Proof.
  intros s e k [(r & m & g & a & -> & r' & a' & -> & -> & E)|[E K]].
  - cbn. rewrite E. reflexivity.
  - destruct e as [t|v t a|f g a|r m g a|u fx|eql eqr]; cbn in E; try discriminate; injection E as <-; try reflexivity.
    contradiction.
Qed.

(* Whether k, read through F's merge, is one of F's needs is the same for
   every most general merge of F's telescope. *)
Theorem need_reading_orientation_free_F : forall s s' es ns ns',
  A.MostGeneralMerge s es -> A.MostGeneralMerge s' es -> A.Needs s es ns -> A.Needs s' es ns' ->
  forall k, In (app_subst_item s k) ns <-> In (app_subst_item s' k) ns'.
Proof.
  intros s s' es ns ns' M M' [_ N] [_ N'] k.
  assert (Gen : forall s s' ns ns', A.MostGeneralMerge s es -> A.MostGeneralMerge s' es ->
     (forall k, In k ns <-> exists e, In e es /\ A.entry_key s e = Some k) ->
     (forall k, In k ns' <-> exists e, In e es /\ A.entry_key s' e = Some k) ->
     In (app_subst_item s k) ns -> In (app_subst_item s' k) ns').
  { clear. intros s s' ns ns' M M' N N' I. apply N in I as [e [Ie E]]. apply N'. exists e. split; [exact Ie|].
    apply entry_key_of. destruct (entry_key_read s e k E) as [(r & m & g & a & -> & r' & a' & -> & -> & Er)|R]; [left|right; exact R].
    exists r, m, g, a. split; [reflexivity|]. exists r', []. split; [reflexivity|]. split; [reflexivity|].
    exact (mgm_same_classes s s' es M M' r r' Er). }
  split; [exact (Gen s s' ns ns' M M' N N')|exact (Gen s' s ns' ns M' M N' N)].
Qed.

(* ---- G's readings, through F's merge ---------------------------------- *)

(* A table formed over its merge: each symbol reads as its class, and the
   merge maps only symbols of the table (`TableFormed`). *)
Definition Formed (C : A.cx) : Prop :=
  (forall k v, A.nlook k (A.c_ty C) = Some v -> v = app_subst (A.c_sub C) (TAbs k)) /\
  (forall g t, In (g, t) (A.c_sub C) -> In g (map fst (A.c_ty C)) /\ incl (A.gens t) (map fst (A.c_ty C))).

(* G's readings of F's inputs hold in F (LinkBody's and BFunCopy's
   input-reading clause). *)
Definition ReadsAgree (Cu Cg : A.cx) : Prop :=
  forall x tx, A.nlook x (A.c_ty Cg) = Some tx -> A.nlook x (A.c_ty Cu) <> None -> A.canon Cu (TAbs x) = A.canon Cu tx.

(* A type F's merge reads over F's table is over F's table. *)
Lemma unmerged_scoped : forall Cu, Formed Cu -> forall t,
  incl (A.gens (A.canon Cu t)) (map fst (A.c_ty Cu)) -> incl (A.gens t) (map fst (A.c_ty Cu)).
Proof.
  intros Cu [_ Dom] t Sc x Ix. destruct (lookup_subst x (A.c_sub Cu)) as [v|] eqn:Lx.
  - exact (proj1 (Dom x v (lookup_subst_in_pair _ _ _ Lx))).
  - exact (Sc x (PublicFormation.gens_keep t (A.c_sub Cu) x Ix Lx)).
Qed.

(* Over F's table, G's merge changes nothing F's merge sees. *)
Lemma reads_agree_canon : forall Cu Cg, Formed Cg -> ReadsAgree Cu Cg ->
  forall t, incl (A.gens t) (map fst (A.c_ty Cu)) -> A.canon Cu (A.canon Cg t) = A.canon Cu t.
Proof.
  intros Cu Cg [TabG DomG] RA t Sc. unfold A.canon. rewrite <- (app_subst_nil t) at 2.
  apply subst2_agree. intros x Ix. rewrite app_subst_nil.
  destruct (A.nlook x (A.c_ty Cg)) as [tx|] eqn:Lg.
  - pose proof (TabG x tx Lg) as E. rewrite <- E. symmetry. apply (RA x tx Lg).
    destruct (keys_in _ x (Sc x Ix)) as [v Lv]. rewrite Lv. discriminate.
  - assert (Ls : lookup_subst x (A.c_sub Cg) = None).
    { destruct (lookup_subst x (A.c_sub Cg)) as [v|] eqn:Ls; [|reflexivity]. exfalso.
      destruct (keys_in _ x (proj1 (DomG x v (lookup_subst_in_pair _ _ _ Ls)))) as [w Lw]. congruence. }
    cbn [app_subst]. rewrite Ls. reflexivity.
Qed.

(* Two orientations of G's merge, each reading F's inputs as F does,
   spell a type F's merge reads over F's table alike, read through F. *)
Theorem need_reading_orientation_free_G : forall Cu Cg Cg' es,
  Formed Cu -> Formed Cg -> Formed Cg' -> ReadsAgree Cu Cg -> ReadsAgree Cu Cg' ->
  A.MostGeneralMerge (A.c_sub Cg) es -> A.MostGeneralMerge (A.c_sub Cg') es ->
  forall t, incl (A.gens (A.canon Cu (A.canon Cg t))) (map fst (A.c_ty Cu)) ->
  A.canon Cu (A.canon Cg t) = A.canon Cu (A.canon Cg' t).
Proof.
  intros Cu Cg Cg' es FU FG FG' RA RA' M M' t Sc.
  pose proof (unmerged_scoped Cu FU _ Sc) as Sc'.
  rewrite <- (reads_agree_canon Cu Cg' FG' RA' (A.canon Cg t) Sc').
  unfold A.canon. rewrite (proj1 (mgm_reorient _ _ es M M' t)). reflexivity.
Qed.

(* ---- ambiguity ------------------------------------------------------- *)

(* If G's readings of F's inputs hold in F, two keys G identifies that
   both read, through F's merge, as needs of F read as the same one: no
   need class of G meets two need classes of F. *)
Theorem need_unambiguous : forall Cu nsu Cg, Formed Cu -> Formed Cg -> ReadsAgree Cu Cg ->
  (forall k, In k nsu -> incl (key_gens k) (map fst (A.c_ty Cu)) /\ NeedShape k) ->
  forall k1 k2, A.canon_item Cg k1 = A.canon_item Cg k2 ->
  In (A.canon_item Cu k1) nsu -> In (A.canon_item Cu k2) nsu -> A.canon_item Cu k1 = A.canon_item Cu k2.
Proof.
  intros Cu nsu Cg FU FG RA Sc k1 k2 E I1 I2.
  destruct k1 as [t1|v1|f1|r1 m1 a1|u1], k2 as [t2|v2|f2|r2 m2 a2|u2]; cbn in E |- *; try discriminate; try exact E.
  injection E as Er Em Ea. subst m2.
  destruct (Sc _ I1) as [S1 N1]. destruct (Sc _ I2) as [S2 N2]. cbn in S1, S2, N1, N2.
  apply (proj1 (map_snd_nil _ a1)) in N1. apply (proj1 (map_snd_nil _ a2)) in N2. subst a1 a2.
  change (app_subst (A.c_sub Cu) r1) with (A.canon Cu r1). change (app_subst (A.c_sub Cu) r2) with (A.canon Cu r2).
  rewrite <- (reads_agree_canon Cu Cg FG RA r1 (unmerged_scoped Cu FU r1 S1)).
  rewrite <- (reads_agree_canon Cu Cg FG RA r2 (unmerged_scoped Cu FU r2 S2)).
  unfold A.canon at 2 4. rewrite Er. reflexivity.
Qed.

Lemma functx_formed : forall u C ns, A.FunCtx W u C ns -> Formed C.
Proof. intros u C ns FC. destruct (functx_scoped W u C ns FC) as [Tab [_ [_ [_ [_ [_ Dom]]]]]]. split; assumption. Qed.

Lemma functx_needs_scoped : forall u C ns, A.FunCtx W u C ns ->
  forall k, In k ns -> incl (key_gens k) (map fst (A.c_ty C)) /\ NeedShape k.
Proof.
  intros u C ns FC k Ik. destruct (functx_scoped W u C ns FC) as [_ [Sc [_ [_ [_ [Shape _]]]]]].
  destruct (functx_needs_provided W u C ns FC k Ik) as [p Ip]. split; [exact (proj1 (proj2 (Sc k p Ip)))|exact (Shape k Ik)].
Qed.

(* The binding's input reading rules out an ambiguous declared need. *)
Corollary link_need_unambiguous : forall u gg Cu nsu Cg nsg,
  A.LinkBinding W u Cu nsu Cg nsg ->
  A.FunCtx W u Cu nsu -> A.FunCtx W gg Cg nsg ->
  forall k1 k2, A.canon_item Cg k1 = A.canon_item Cg k2 ->
  In (A.canon_item Cu k1) nsu -> In (A.canon_item Cu k2) nsu -> A.canon_item Cu k1 = A.canon_item Cu k2.
Proof.
  intros u gg Cu nsu Cg nsg Binding FCu FCg.
  apply (need_unambiguous Cu nsu Cg (functx_formed _ _ _ FCu) (functx_formed _ _ _ FCg)).
  - intros x tx L1 L2. exact (proj2 (@A.link_bind_inputs W u Cu nsu Cg nsg Binding x tx L1 L2)).
  - exact (functx_needs_scoped _ _ _ FCu).
Qed.

End SameNeed.
