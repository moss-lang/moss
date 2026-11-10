(* CF.ChainForwardCopySigs, proved (fwdsigs): applying F, whose closure is
   a copy chain F = u <- h <- ... <- h0 down to a platform family h0, the
   caller's method at the last position for a key and the family item the
   chain's sources give that position have agreeing signatures at the
   state's ground meanings.

   One valuation T reads every level: T x = ground d (θ x), the meaning the
   application gives u's input x (each level's inputs are among u's).
   - T satisfies each level's table (T (σ_v x) = T x): at u by θ's merges
     (PublicFamilyAgree.th_sigma), below by the copy step's comparison of
     h's merges at the copy site, read at T (PublicCopyApply.copy_read_at).
   - A fixed input means T x in each level's fixed meanings gfx (FixedMeans,
     the application's fixed clause, CCStep's agreement on h's).
   - A copy step u <- h compares u's result at Cu with h's at Ch, read
     through Cu's merge, after copy_subst (CopyInterpretation); read at T and
     the copy site's meanings, the two agree (PublicCopySubst.copy_ty_read):
     a fixed reading means T x (CopySite, gfx), and every other generation
     of a reading at Cu is in ys (it is a generation of Cu's table:
     result_gens_vals, PublicCopyApply.canon_gens_read).
   - h's result at Ch with the copy's roles and with its own correspond
     entry by entry (entries_rel_kinds, at θ_h = Ch's own readings), and the
     last position for a key is the same on both sides (pos_last): this
     needs h abstract (its mentions are its inputs), which Audit's BFunCopy
     asks (fwdsigs-AUDIT.md).
   - At the top, the caller's reading at C and u's at Cu correspond the
     same way at θ. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicTagCheck
  PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply PublicFamilyAgree PublicChainNew PublicChainFwd.
From NM Require EagerCopy PublicFormation PublicTypeFacts PublicCallerKeys PublicCopyFresh PublicCopySubst PublicScoping PublicFamilyStep PublicFamilyLink PublicMergeClass.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- the last position for a key, on two related readings --------------------- *)

Lemma keys_of_snoc : forall X (f : X -> option item) l x,
  keys_of f (l ++ [x]) = (keys_of f l ++ match f x with Some k => [k] | None => [] end)%list.
Proof. intros X f l x. unfold keys_of. rewrite flat_map_app. cbn. rewrite app_nil_r. reflexivity. Qed.

Lemma keys_of_len : forall (L : list (A.entry * A.entry)) (f1 f2 : A.entry -> option item),
  (forall p, In p L -> (f1 (fst p) = None <-> f2 (snd p) = None)) ->
  List.length (keys_of (fun p => f1 (fst p)) L) = List.length (keys_of (fun p => f2 (snd p)) L).
Proof.
  intros L f1 f2 Kd. induction L as [|p L IH]; [reflexivity|]. unfold keys_of in *. cbn [flat_map].
  rewrite !length_app, IH by (intros q Iq; exact (Kd q (or_intror Iq))).
  destruct (Kd p (or_introl eq_refl)) as [K1 K2].
  destruct (f1 (fst p)) eqn:E1; destruct (f2 (snd p)) eqn:E2; try reflexivity.
  - discriminate (K2 eq_refl).
  - discriminate (K1 eq_refl).
Qed.

Lemma flat_map_rev_snoc : forall (f : A.entry -> list (item * A.provision)) l x rest,
  (flat_map f (rev (l ++ [x])) ++ rest)%list = (f x ++ (flat_map f (rev l) ++ rest))%list.
Proof. intros f l x rest. rewrite rev_app_distr. cbn [rev app flat_map]. rewrite app_assoc. reflexivity. Qed.

Lemma ilook_skip : forall X k k' (p : X) rest, k <> k' -> A.ilook k ((k', p) :: rest) = A.ilook k rest.
Proof.
  intros X k k' p rest Ne. cbn. destruct (item_eqb k k') eqn:E; [|reflexivity].
  apply item_eqb_spec in E. contradiction.
Qed.

Lemma ilook_hit : forall X k (p : X) rest, A.ilook k ((k, p) :: rest) = Some p.
Proof. intros X k p rest. cbn. rewrite (proj2 (item_eqb_spec k k) eq_refl). reflexivity. Qed.

(* Pairs of entries (two readings of one result), keyed alike on both
   sides; two pairs with one first-side method key have one second-side key.
   The last second-side position for a method key is the last first-side
   position for its key, and each side's provision lookup finds that
   position's entry. *)
Lemma pos_last : forall (s1 s2 : subst) (L : list (A.entry * A.entry)),
  (forall p, In p L -> (A.entry_key s1 (fst p) = None <-> A.entry_key s2 (snd p) = None)) ->
  (forall p p' r m a, In p L -> In p' L -> A.entry_key s2 (snd p) = Some (IMth r m a) ->
     A.entry_key s1 (fst p) = A.entry_key s1 (fst p') -> A.entry_key s2 (snd p') = A.entry_key s2 (snd p)) ->
  forall j r m a, nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L) j = Some (IMth r m a) ->
  (forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L) j' = Some k' -> k' <> IMth r m a) ->
  exists p k1 q1 q2, In p L /\ A.entry_key s1 (fst p) = Some k1 /\ A.entry_key s2 (snd p) = Some (IMth r m a) /\
    nth_error (keys_of (fun p => A.entry_key s1 (fst p)) L) j = Some k1 /\
    (forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s1 (fst p)) L) j' = Some k' -> k' <> k1) /\
    A.entry_provision s1 (fun a => a) (fst p) = [(k1, q1)] /\ A.entry_provision s2 (fun a => a) (snd p) = [(IMth r m a, q2)] /\
    (forall rest, A.ilook k1 (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst L)) ++ rest) = Some q1) /\
    (forall rest, A.ilook (IMth r m a) (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd L)) ++ rest) = Some q2).
Proof.
  intros s1 s2 L. induction L as [|x L0 IH] using rev_ind; intros Kd Tr j r m a Nj Lj.
  { destruct j; cbn in Nj; discriminate. }
  assert (Kd0 : forall p, In p L0 -> (A.entry_key s1 (fst p) = None <-> A.entry_key s2 (snd p) = None))
    by (intros p Ip; apply Kd; apply in_or_app; left; exact Ip).
  assert (Tr0 : forall p p' r m a, In p L0 -> In p' L0 -> A.entry_key s2 (snd p) = Some (IMth r m a) ->
     A.entry_key s1 (fst p) = A.entry_key s1 (fst p') -> A.entry_key s2 (snd p') = A.entry_key s2 (snd p))
    by (intros p p' r0 m0 a0 Ip Ip'; apply Tr; apply in_or_app; left; assumption).
  assert (Ix : In x (L0 ++ [x])) by (apply in_or_app; right; left; reflexivity).
  pose proof (keys_of_len L0 (A.entry_key s1) (A.entry_key s2) Kd0) as Len.
  rewrite keys_of_snoc in Nj, Lj |- *.
  assert (R1 : forall rest, (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst (L0 ++ [x]))) ++ rest)%list =
    (A.entry_provision s1 (fun a => a) (fst x) ++ (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst L0)) ++ rest))%list)
    by (intros rest; rewrite map_app; apply flat_map_rev_snoc).
  assert (R2 : forall rest, (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd (L0 ++ [x]))) ++ rest)%list =
    (A.entry_provision s2 (fun a => a) (snd x) ++ (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd L0)) ++ rest))%list)
    by (intros rest; rewrite map_app; apply flat_map_rev_snoc).
  destruct (A.entry_key s2 (snd x)) as [a2|] eqn:E2.
  - destruct (A.entry_key s1 (fst x)) as [a1|] eqn:E1;
      [|exfalso; pose proof (proj1 (Kd x Ix) E1) as N; congruence].
    destruct (PublicFamilyAgree.entry_provision_of s1 (fun a => a) (fst x) a1 E1) as [px1 Ex1].
    destruct (PublicFamilyAgree.entry_provision_of s2 (fun a => a) (snd x) a2 E2) as [px2 Ex2].
    remember (List.length (keys_of (fun p => A.entry_key s2 (snd p)) L0)) as n eqn:En.
    destruct (Nat.lt_ge_cases j n) as [Lt|Ge].
    + rewrite nth_error_app1 in Nj by lia.
      assert (Lj0 : forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L0) j' = Some k' -> k' <> IMth r m a).
      { intros j' k' Lt' N'. apply (Lj j' k' Lt'). rewrite nth_error_app1; [exact N'|].
        apply nth_error_Some. rewrite N'. discriminate. }
      destruct (IH Kd0 Tr0 j r m a Nj Lj0) as (p & k1 & q1 & q2 & Ip & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
      assert (Ne2 : a2 <> IMth r m a).
      { intros ->. apply (Lj n (IMth r m a) Lt); [|reflexivity]. rewrite nth_error_app2 by lia.
        rewrite <- En, Nat.sub_diag. reflexivity. }
      assert (Ne1 : a1 <> k1).
      { intros ->. apply Ne2. pose proof (Tr p x r m a (in_or_app _ _ _ (or_introl Ip)) Ix K2 (eq_trans K1 (eq_sym E1))) as T2.
        rewrite E2, K2 in T2. injection T2 as T2. exact T2. }
      exists p, k1, q1, q2. split; [apply in_or_app; left; exact Ip|]. split; [exact K1|]. split; [exact K2|].
      split; [rewrite nth_error_app1 by lia; exact N1|]. split.
      * intros j' k' Lt' N'. destruct (Nat.lt_ge_cases j' n) as [Lt2|Ge2].
        -- rewrite nth_error_app1 in N' by lia. exact (L1 j' k' Lt' N').
        -- rewrite nth_error_app2 in N' by lia. rewrite Len in N'. destruct (j' - n) as [|z];
             cbn in N'; [injection N' as <-; exact Ne1|destruct z; cbn in N'; discriminate].
      * split; [exact P1|]. split; [exact P2|]. split.
        -- intros rest. rewrite R1, Ex1. cbn [app]. rewrite ilook_skip by (intros E; apply Ne1; symmetry; exact E). exact (I1 rest).
        -- intros rest. rewrite R2, Ex2. cbn [app]. rewrite ilook_skip by (intros E; apply Ne2; symmetry; exact E). exact (I2 rest).
    + rewrite nth_error_app2 in Nj by lia. rewrite <- En in Nj. destruct (j - n) as [|z] eqn:Ej; cbn in Nj; [|destruct z; cbn in Nj; discriminate].
      injection Nj as ->. assert (Ejn : j = n) by lia. subst j.
      exists x, a1, px1, px2. split; [exact Ix|]. split; [exact E1|]. split; [exact E2|].
      split; [rewrite nth_error_app2 by lia; rewrite Len, Nat.sub_diag; reflexivity|]. split.
      * intros j' k' Lt' N'. rewrite nth_error_app2 in N' by lia. rewrite Len in N'.
        destruct (j' - n) as [|z] eqn:Ez; [lia|].
        cbn in N'. destruct z; cbn in N'; discriminate.
      * split; [exact Ex1|]. split; [exact Ex2|]. split; intros rest; [rewrite R1, Ex1|rewrite R2, Ex2]; cbn [app]; apply ilook_hit.
  - assert (E1 : A.entry_key s1 (fst x) = None) by exact (proj2 (Kd x Ix) E2). rewrite E1.
    rewrite !app_nil_r in Nj, Lj |- *.
    destruct (IH Kd0 Tr0 j r m a Nj Lj) as (p & k1 & q1 & q2 & Ip & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
    exists p, k1, q1, q2. split; [apply in_or_app; left; exact Ip|]. do 6 (split; [assumption|]). split; intros rest.
    + rewrite R1, (PublicFamilyAgree.entry_provision_none s1 _ (fst x) E1). exact (I1 rest).
    + rewrite R2, (PublicFamilyAgree.entry_provision_none s2 _ (snd x) E2). exact (I2 rest).
Qed.

(* ---- substitutions a valuation reads through ----------------------------------- *)

Lemma lookup_subst_out : forall (f : nat * ty -> ty) (l : list (nat * ty)) x, ~ In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f q)) l) = None.
Proof.
  intros f l x. induction l as [|[y v] r IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [exfalso; apply N; left; reflexivity|].
  apply IH. intros I. apply N. right. exact I.
Qed.

Lemma app_subst_abs : forall s x, app_subst s (TAbs x) = match lookup_subst x s with Some v => v | None => TAbs x end.
Proof. reflexivity. Qed.

Lemma sig_inv : forall T s g, (forall t, app_subst T (app_subst s t) = app_subst T t) ->
  A.sig_subst T (A.sig_subst s g) = A.sig_subst T g.
Proof.
  intros T s [ps rt] Inv. unfold A.sig_subst. cbn. rewrite Inv, map_map. f_equal.
  apply map_ext. exact Inv.
Qed.

Lemma sigrel_eq : forall s1 s2 g1 g2, PublicOutputSubst.SigRel s1 s2 g1 g2 -> A.sig_subst s1 g1 = A.sig_subst s2 g2.
Proof. intros s1 s2 [p1 r1] [p2 r2] [Ep Er]. unfold A.sig_subst. cbn in *. rewrite Ep. unfold PublicOutputSubst.TyRel in Er. rewrite Er. reflexivity. Qed.


Section Static.
Variable W : A.world.

(* A FunCtx's table and merge, and what a valuation satisfying its table
   makes of them. *)
Lemma functx_mgm : forall v Cv ns, A.FunCtx W v Cv ns ->
  (forall t, app_subst (A.c_sub Cv) (app_subst (A.c_sub Cv) t) = app_subst (A.c_sub Cv) t) /\
  (forall g t, In (g, t) (A.c_sub Cv) -> lookup_subst g (A.c_sub Cv) = Some t).
Proof.
  intros v Cv ns (d & ps & a & r & b & es & tm & tm' & σ & classes & imp & _ & _ & _ & _ & _ & [_ [Id [_ [_ Fn]]]] & _ & _ & -> & _).
  cbn [A.entries_cx A.c_sub]. split; [exact Id|exact Fn].
Qed.

Lemma functx_ty_sub : forall v Cv ns, A.FunCtx W v Cv ns -> forall t, app_subst (A.c_ty Cv) t = app_subst (A.c_sub Cv) t.
Proof.
  intros v Cv ns Fc t. destruct (functx_scoped W v Cv ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  apply subst_agree_on. intros x _. cbn [app_subst]. rewrite lookup_subst_nlook'.
  destruct (A.nlook x (A.c_ty Cv)) as [tx|] eqn:L.
  - rewrite (TyA x tx L). reflexivity.
  - destruct (lookup_subst x (A.c_sub Cv)) as [u|] eqn:E; [|reflexivity]. exfalso.
    destruct (SubSc x u (lookup_subst_in_pair _ _ _ E)) as [Ix _].
    destruct (nlook_some _ x _ Ix) as [w Lw]. congruence.
Qed.

Definition TableOk (T : subst) (Cv : A.cx) : Prop :=
  forall x tx, A.nlook x (A.c_ty Cv) = Some tx -> app_subst T tx = app_subst T (TAbs x).

Lemma sub_inv : forall v Cv ns T, A.FunCtx W v Cv ns -> TableOk T Cv ->
  forall t, app_subst T (app_subst (A.c_sub Cv) t) = app_subst T t.
Proof.
  intros v Cv ns T Fc Ok t. apply PublicCopySubst.ty_agree. intros x _.
  destruct (functx_scoped W v Cv ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  cbn [app_subst]. destruct (lookup_subst x (A.c_sub Cv)) as [u|] eqn:E; [|reflexivity].
  destruct (SubSc x u (lookup_subst_in_pair _ _ _ E)) as [Ix _].
  destruct (nlook_some _ x _ Ix) as [w Lw]. pose proof (TyA x w Lw) as Ew. cbn [app_subst] in Ew. rewrite E in Ew. subst w.
  exact (Ok x u Lw).
Qed.

(* A FunCtx's readings θ_h of h's inputs in u's context Cu (canon Cu after
   Cu's table) read at T as T does. *)
Lemma th_inv : forall u Cu nsu T (l : list (nat * ty)), A.FunCtx W u Cu nsu -> TableOk T Cu ->
  forall t, app_subst T (app_subst (map (fun q => (fst q, A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q))))) l) t) =
    app_subst T t.
Proof.
  intros u Cu nsu T l Fc Ok t. apply PublicCopySubst.ty_agree. intros x _.
  destruct (List.In_dec Nat.eq_dec x (map fst l)) as [I|N].
  - assert (Lx : lookup_subst x (map (fun q => (fst q, A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q))))) l) =
      Some (A.canon Cu (app_subst (A.c_ty Cu) (TAbs x))))
      by exact (PublicFamilyAgree.lookup_keys (fun y => A.canon Cu (app_subst (A.c_ty Cu) (TAbs y))) l x I).
    rewrite app_subst_abs, Lx.
    unfold A.canon. rewrite (functx_ty_sub u Cu nsu Fc), !(sub_inv u Cu nsu T Fc Ok). reflexivity.
  - rewrite app_subst_abs.
    assert (Lx : lookup_subst x (map (fun q => (fst q, A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q))))) l) = None)
      by exact (lookup_subst_out (fun q => A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q)))) l x N).
    rewrite Lx. reflexivity.
Qed.

(* ---- what a reading in a FunCtx mentions --------------------------------------- *)

(* A result with no outputs read in a FunCtx Cu mentions only generations of
   Cu's table readings. *)
Theorem result_gens_vals : forall u0 Cu ns v o Cr ks, A.FunCtx W u0 Cu ns ->
  A.ProvideResult W v Cu o Cr ks -> (forall t, ~ A.FunOutputs W v t) ->
  forall k p, In k ks -> A.ilook (A.provision_id k) (A.c_provisions Cr) = Some p ->
  forall y, In y (A.provision_gens p) -> exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx).
Proof.
  intros u0 Cu ns v o Cr ks Fc PR NoO k p Ik L.
  pose proof (PublicCallerKeys.result_key_id W v Cu _ Cr ks k PR Ik) as Id. rewrite Id in L.
  destruct (functx_scoped W u0 Cu ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  destruct (functx_mgm u0 Cu ns Fc) as [Idm Fn].
  set (K := fun y => exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx)).
  assert (KIn : forall x w y, In (x, w) (A.c_ty Cu) -> In y (A.gens w) -> K y).
  { intros x w y Iq Iy. destruct (nlook_some _ x (A.c_ty Cu) (in_map fst _ _ Iq)) as [w' Lw].
    exists x, w'. split; [exact Lw|]. rewrite (TyA x w' Lw).
    rewrite <- (PublicCopyFresh.functx_ty_abs W u0 Cu ns Fc x w Iq). exact Iy. }
  assert (SubK : forall g u, In (g, u) (A.c_sub Cu) -> PublicFormation.GensIn K (A.gens u)).
  { intros g u Iq y Iy. destruct (SubSc g u Iq) as [Ig _].
    destruct (nlook_some _ g (A.c_ty Cu) Ig) as [w Lw]. apply (KIn g w y (nlook_in _ _ _ _ Lw)).
    rewrite (TyA g w Lw). cbn [app_subst]. rewrite (Fn g u Iq). exact Iy. }
  assert (TyK : forall x w, In (x, w) (A.c_ty Cu) -> PublicFormation.GensIn K (A.gens (A.canon Cu w))).
  { intros x w Iq y Iy. apply (KIn x w y Iq).
    rewrite (PublicCopyFresh.functx_ty_abs W u0 Cu ns Fc x w Iq) in Iy |- *. unfold A.canon in Iy. rewrite Idm in Iy. exact Iy. }
  pose proof PR as (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & EC).
  set (news := flat_map (A.entry_provision (A.c_sub Cu) (fun a => a)) (rev es)).
  assert (En : A.c_provisions Cr = (news ++ A.c_provisions Cu)%list) by (rewrite EC; reflexivity).
  destruct (PublicCallerKeys.provide_result_key_news W v Cu _ Cr ks k PR Ik news En) as [p0 I0].
  destruct (PublicFamilyStep.ilook_of_in _ k news p0 I0) as [p' [L' I']].
  rewrite En, (PublicFamilyStep.ilook_app_some _ k news _ p' L') in L. injection L as <-.
  assert (Sc0 : PublicFormation.GensIn K (PublicTypeRead.tm_gens (A.output_type_map Cu outs o))).
  { intros g Ig. destruct (PublicFormation.tm_gens_in _ g Ig) as [x [w [Iq Iw]]].
    unfold A.output_type_map in Iq. apply in_map_iff in Iq as [[y0 w0] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. exfalso. exact (NoO t (proj1 (Outs t) It)).
    - exact (TyK _ _ Iq g Iw). }
  pose proof (proj2 (PublicScoping.output_entries_gens W K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  unfold news in I'. apply in_flat_map in I' as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  exact (PublicScoping.entry_provision_gens K _ e k _ SubK (Es e Ie) Iq).
Qed.

(* A reading's roles do not matter when the functor has no outputs. *)
Lemma pr_noout : forall v C o1 o2 C' ks, A.ProvideResult W v C o1 C' ks -> (forall t, ~ A.FunOutputs W v t) ->
  A.ProvideResult W v C o2 C' ks.
Proof.
  intros v C o1 o2 C' ks (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & EC) NoO.
  assert (Eo : outs = []) by (destruct outs as [|t rest]; [reflexivity|exfalso; exact (NoO t (proj1 (Outs t) (or_introl eq_refl)))]).
  subst outs. exists d, ps, a, r, b, [], es, tm'. split; [exact D|]. split; [exact FP|]. split; [exact Outs|].
  split; [exact OIE|]. split; [exact RK|exact EC].
Qed.

(* ---- entries_rel, with the entries' kinds ------------------------------------- *)

Section Rel.
Variables (C : A.cx) (u : nat) (th : subst) (Cf : A.cx) (out2 : nat -> ty) (Cr : A.cx) (ks : list item).
Variables (Cu : A.cx) (needs : list item) (outputs : A.cx) (keys : list item).
Import A.
Hypothesis SubCf0 : c_sub Cf = c_sub C.
Hypothesis Parts : th = map (fun q => (fst q, canon Cf (app_subst (c_ty Cf) (TAbs (fst q))))) (c_ty Cu) /\
  (forall t, In t (map fst (c_ty Cu)) -> nlook t (c_ty Cf) <> None) /\
  (forall g t, In (g, t) (c_sub Cu) -> (forall x, In x (g :: gens t) -> nlook x (c_ty Cf) <> None) ->
     canon Cf (app_subst (c_ty Cf) (TAbs g)) = canon Cf (app_subst (c_ty Cf) t)) /\
  (forall d ps a r b, w_decl W u = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None) /\
  ProvideResult W u Cf out2 Cr ks.
Hypothesis FCu : FunCtx W u Cu needs.
Hypothesis NoOut : forall t, ~ FunOutputs W u t.
Hypothesis PRf : ProvideResult W u Cu (fun t => TAbs t) outputs keys.
Hypothesis Idem : forall t, canon C (canon C t) = canon C t.
Hypothesis KeysT : forall q pa x, resolve_sym W q pa = Some x ->
  nlook x (c_ty Cf) <> None -> nlook x (c_ty Cu) = None -> kind_sym W x = Some DKType.

Lemma entries_rel_kinds : exists tm1 tm2 L,
  outputs = output_cx Cu tm1 (map fst L) /\ keys = keys_of (entry_key (c_sub Cu)) (map fst L) /\
  Cr = output_cx Cf tm2 (map snd L) /\ ks = keys_of (entry_key (c_sub C)) (map snd L) /\
  Forall (fun p => EntryRelM W th (c_sub C) (map fst (c_ty Cu)) (fst p) (snd p) /\
    PublicCallerKeys.SameKind (fst p) (snd p)) L.
Proof.
  pose proof PRf as (d & ps & a & r & b & outs1 & es1 & tm1 & D & FP & O1 & E1 & K1 & Eo).
  pose proof Parts as [_ [_ [_ [_ PR]]]].
  destruct PR as (d' & ps' & a' & r' & b' & outs2 & es2 & tm2 & D' & FP' & O2 & E2 & K2 & ECr).
  rewrite D in D'. injection D' as <-. rewrite FP in FP'. injection FP' as <- <- <- <-.
  assert (N1 : outs1 = []) by (destruct outs1 as [|t rest]; [reflexivity|exfalso; exact (NoOut t (proj1 (O1 t) (or_introl eq_refl)))]).
  assert (N2 : outs2 = []) by (destruct outs2 as [|t rest]; [reflexivity|exfalso; exact (NoOut t (proj1 (O2 t) (or_introl eq_refl)))]).
  subst outs1 outs2.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E1) in *.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E2) in *.
  pose proof (proj2 (family_entries_rel W th (c_sub C) u (map fst (c_ty Cu))) _ _ _ _ _ _ E1 _ _ _ _ E2 (Forall2_nil _)
    (PublicFamilyAgree.agree_t W C u th Cf out2 Cr ks Cu needs SubCf0 Parts FCu Idem)
    (PublicFamilyAgree.dom_t W u th Cf out2 Cr ks Cu Parts KeysT)
    (PublicFamilyAgree.ment_in W u th Cf out2 Cr ks Cu needs Parts FCu NoOut d ps a r b D FP)
    (PublicFamilyAgree.kg_t W u Cu needs FCu)) as Fr.
  pose proof (proj2 (PublicCallerKeys.same_kind_entries W) _ _ _ _ _ _ E1 _ _ _ _ E2) as Fk.
  assert (Fb : Forall2 (fun e1 e2 => EntryRelM W th (c_sub C) (map fst (c_ty Cu)) e1 e2 /\ PublicCallerKeys.SameKind e1 e2) es1 es2).
  { clear -Fr Fk. revert es2 Fr Fk. induction es1 as [|e1 r1 IH]; intros es2 Fr Fk; inversion Fr; subst; inversion Fk; subst;
      constructor; [split; assumption|exact (IH _ ltac:(eassumption) ltac:(eassumption))]. }
  destruct (forall2_pairs _ _ _ _ _ Fb) as (L & -> & -> & FL).
  do 2 eexists. exists L. split; [exact Eo|]. split; [exact K1|]. split; [exact ECr|]. split; [|exact FL].
  rewrite K2. rewrite SubCf0. reflexivity.
Qed.
End Rel.

Lemma same_kind_none : forall s1 s2 e1 e2, PublicCallerKeys.SameKind e1 e2 ->
  (A.entry_key s1 e1 = None <-> A.entry_key s2 e2 = None).
Proof. intros s1 s2 [] [] K; cbn in K |- *; try contradiction; split; intros E; try discriminate; reflexivity. Qed.

(* A key of such a reading has its receiver over Cu's table readings too. *)
Theorem result_key_gens_vals : forall u0 Cu ns v o Cr ks, A.FunCtx W u0 Cu ns ->
  A.ProvideResult W v Cu o Cr ks -> (forall t, ~ A.FunOutputs W v t) ->
  forall r m a, In (IMth r m a) ks ->
  forall y, In y (A.gens r) -> exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx).
Proof.
  intros u0 Cu ns v o Cr ks Fc PR NoO r m a Ik.
  destruct (functx_scoped W u0 Cu ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  destruct (functx_mgm u0 Cu ns Fc) as [Idm Fn].
  set (K := fun y => exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx)).
  assert (KIn : forall x w y, In (x, w) (A.c_ty Cu) -> In y (A.gens w) -> K y).
  { intros x w y Iq Iy. destruct (nlook_some _ x (A.c_ty Cu) (in_map fst _ _ Iq)) as [w' Lw].
    exists x, w'. split; [exact Lw|]. rewrite (TyA x w' Lw).
    rewrite <- (PublicCopyFresh.functx_ty_abs W u0 Cu ns Fc x w Iq). exact Iy. }
  assert (SubK : forall g u, In (g, u) (A.c_sub Cu) -> PublicFormation.GensIn K (A.gens u)).
  { intros g u Iq y Iy. destruct (SubSc g u Iq) as [Ig _].
    destruct (nlook_some _ g (A.c_ty Cu) Ig) as [w Lw]. apply (KIn g w y (nlook_in _ _ _ _ Lw)).
    rewrite (TyA g w Lw). cbn [app_subst]. rewrite (Fn g u Iq). exact Iy. }
  assert (TyK : forall x w, In (x, w) (A.c_ty Cu) -> PublicFormation.GensIn K (A.gens (A.canon Cu w))).
  { intros x w Iq y Iy. apply (KIn x w y Iq).
    rewrite (PublicCopyFresh.functx_ty_abs W u0 Cu ns Fc x w Iq) in Iy |- *. unfold A.canon in Iy. rewrite Idm in Iy. exact Iy. }
  pose proof PR as (d & ps & a0 & r0 & b & outs & es & tm' & D & FP & Outs & OIE & RK & EC).
  assert (Sc0 : PublicFormation.GensIn K (PublicTypeRead.tm_gens (A.output_type_map Cu outs o))).
  { intros g Ig. destruct (PublicFormation.tm_gens_in _ g Ig) as [x [w [Iq Iw]]].
    unfold A.output_type_map in Iq. apply in_map_iff in Iq as [[y0 w0] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. exfalso. exact (NoO t (proj1 (Outs t) It)).
    - exact (TyK _ _ Iq g Iw). }
  pose proof (proj2 (PublicScoping.output_entries_gens W K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  rewrite Forall_forall in Es. unfold A.ResultKeys in RK. subst ks.
  apply in_flat_map in Ik as [e [Ie Ik]]. pose proof (Es e Ie) as Pe.
  destruct e as [t|v0 t a1|f g a1|r1 m1 g1 a1|u1 fx|eql eqr]; cbn in Ik; try contradiction; destruct Ik as [E|[]]; try discriminate.
  unfold A.mkey in E. injection E as <- <- <-. cbn in Pe. destruct Pe as [Pr _].
  exact (PublicFormation.subst_within K _ r1 SubK Pr).
Qed.

(* Cu's table readings are over Cu's keys. *)
Lemma table_gens_dom : forall v Cv ns x tx y, A.FunCtx W v Cv ns -> A.nlook x (A.c_ty Cv) = Some tx -> In y (A.gens tx) ->
  In y (map fst (A.c_ty Cv)).
Proof.
  intros v Cv ns x tx y Fc L Iy. destruct (functx_scoped W v Cv ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  rewrite (TyA x tx L) in Iy. cbn [app_subst] in Iy. destruct (lookup_subst x (A.c_sub Cv)) as [u|] eqn:E.
  - exact (proj2 (SubSc x u (lookup_subst_in_pair _ _ _ E)) y Iy).
  - destruct Iy as [<-|[]]. apply nlook_in in L. exact (in_map fst _ _ L).
Qed.

Lemma pr_keys_match : forall v C o C' ks, A.ProvideResult W v C o C' ks ->
  PublicCallRealize.KeysMatch (A.c_provisions C) -> PublicCallRealize.KeysMatch (A.c_provisions C').
Proof.
  intros v C o C' ks (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & _ & ->) KM k p I.
  cbn [A.output_cx A.c_provisions] in I. apply in_app_or in I as [I|I].
  - exact (PublicFamilyAgree.keys_match_entries _ _ k p I).
  - exact (KM k p I).
Qed.

End Static.

Module ChainSigs (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses (FL, and through it CF), so the
   obligations match. *)
Module FL := PublicFamilyLink.FamilyLink Prims I0.

Section Sigs.
Variable W : A.world.
Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

Definition Abstract (v : nat) : Prop :=
  forall d ps a r b, A.w_decl W v = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None.

Lemma abstract_of_ast : forall v, (exists n ps a r, A.ast_of W v = Some (ADFunctor n ps a r None)) -> Abstract v.
Proof.
  intros v (n & ps & a & r & E) d ps' a' r' b D FP. unfold A.ast_of in E. rewrite D in E. injection E as E.
  rewrite E in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity.
Qed.

Lemma ast_of_abstract : forall v Cv ns, A.FunCtx W v Cv ns -> Abstract v ->
  exists n ps a r, A.ast_of W v = Some (ADFunctor n ps a r None).
Proof.
  intros v Cv ns (d & ps & a & r & b & es & tm & tm' & σ & classes & imp & D & FP & _) Ab.
  pose proof (Ab d ps a r b D FP) as ->.
  destruct (A.d_ast d) eqn:Ed; cbn in FP; try discriminate. injection FP as -> -> -> ->.
  eexists _, _, _, _. unfold A.ast_of. rewrite D. cbn. rewrite Ed. reflexivity.
Qed.

Lemma chain_abs : forall v gfv sms rms h0 gf0 sms0 rms0, FL.CF.CN.CC.LR.PA.FS.CopyA.CopyChain W v gfv sms rms h0 gf0 sms0 rms0 ->
  Abstract h0 -> Abstract v.
Proof.
  intros v gfv sms rms h0 gf0 sms0 rms0 Ch. destruct Ch as [|u h Cu nsu Ch nsh gfx gfh sms rms rm h0 gf0 sms0 rms0 _ _ _ _ _ _ _ _ Site];
    intros A0; [exact A0|].
  destruct Site as (C1 & fx1 & fxh & d1 & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & Ab). exact (abstract_of_ast u Ab).
Qed.

(* ---- the chain, level by level ------------------------------------------------- *)

Section Chain.
Variables (h0 : nat) (Ch0 : A.cx) (needs0 : list item) (outputs : A.cx) (keys : list item).
Hypothesis FC0 : A.FunCtx W h0 Ch0 needs0.
Hypothesis NoH : forall t, ~ A.FunOutputs W h0 t.
Hypothesis PR0 : A.ProvideResult W h0 Ch0 (fun t => TAbs t) outputs keys.
Hypothesis Abs0 : Abstract h0.
Variable T : subst.

Let srcs := map (A.rkey_of outputs) keys.

(* What a level v's own reading at Cv gives: at each position, the method
   there and the root item the chain's sources give that position agree at
   T (receiver and signature). *)
Definition SigInv (v : nat) (Cv : A.cx) (rmsv : option (list (option nat))) : Prop :=
  exists Crv ksv, A.ProvideResult W v Cv (fun t => TAbs t) Crv ksv /\
  forall i r m a g a0, nth_error ksv i = Some (IMth r m a) ->
    A.ilook (IMth r m a) (A.c_provisions Crv) = Some (A.ProvidedMethod g a0) ->
    forall key, In key keys -> nth_error (closure_sources rmsv srcs) i = Some (A.rkey_of outputs key) ->
    exists rf a' gf a1, key = IMth rf m a' /\ A.ilook (IMth rf m a') (A.c_provisions outputs) = Some (A.ProvidedMethod gf a1) /\
      app_subst T r = app_subst T rf /\ A.sig_subst T g = A.sig_subst T gf.

Lemma entry_mth : forall s e r m a, A.entry_key s e = Some (IMth r m a) ->
  exists r0 g0 a0, e = A.NMth r0 m g0 a0 /\ r = app_subst s r0 /\ a = [].
Proof. intros s e r m a E; destruct e as [t|v t a1|f g a1|r1 m1 g1 a1|u fx|eql eqr]; cbn in E; try discriminate. unfold A.mkey in E. injection E as <- <- <-. exists r1, g1, a1. split; [reflexivity|split; reflexivity]. Qed.

(* The step's transfer condition for pos_last: two pairs with one first-side
   method key have one second-side key, when θ_1 ∘ σ_1 = θ_1. *)
Lemma tr_of : forall (s1 s2 th : subst) K (L : list (A.entry * A.entry)),
  Forall (fun p => EntryRelM W th s2 K (fst p) (snd p) /\ PublicCallerKeys.SameKind (fst p) (snd p)) L ->
  (forall t, app_subst th (app_subst s1 t) = app_subst th t) ->
  forall p p' r m a, In p L -> In p' L -> A.entry_key s2 (snd p) = Some (IMth r m a) ->
     A.entry_key s1 (fst p) = A.entry_key s1 (fst p') -> A.entry_key s2 (snd p') = A.entry_key s2 (snd p).
Proof.
  intros s1 s2 th K L FL ThS p p' r m a Ip Ip' K2 E1. rewrite Forall_forall in FL.
  destruct (entry_mth s2 (snd p) r m a K2) as (r2 & g2 & a2 & Ep2 & -> & ->).
  destruct (FL p Ip) as [R Sk]. rewrite Ep2 in R, Sk.
  destruct (fst p) as [?|?|?|r1 m1 g1 a1|?|? ?] eqn:Ep1; try contradiction. destruct R as (<- & Rr & _).
  cbn in E1. destruct (fst p') as [?|?|?|r1' m1' g1' a1'|?|? ?] eqn:Ep1'; cbn in E1; try discriminate.
  unfold A.mkey in E1. injection E1 as Er Em. subst m1'.
  destruct (FL p' Ip') as [R' Sk']. rewrite Ep1' in R', Sk'.
  destruct (snd p') as [?|?|?|r2' m2' g2' a2'|?|? ?] eqn:Ep2'; try contradiction. destruct R' as (<- & Rr' & _).
  rewrite K2. cbn. unfold PublicOutputSubst.TyRel in Rr, Rr'. rewrite <- Rr, <- Rr', <- (ThS r1), <- (ThS r1'), Er. reflexivity.
Qed.

Lemma kinds_of : forall (s1 s2 th : subst) K (L : list (A.entry * A.entry)),
  Forall (fun p => EntryRelM W th s2 K (fst p) (snd p) /\ PublicCallerKeys.SameKind (fst p) (snd p)) L ->
  forall p, In p L -> (A.entry_key s1 (fst p) = None <-> A.entry_key s2 (snd p) = None).
Proof. intros s1 s2 th K L FL p Ip. rewrite Forall_forall in FL. exact (same_kind_none s1 s2 _ _ (proj2 (FL p Ip))). Qed.

Lemma chain_sigs : forall v gfv sms rmsv h0' gf0 sms0 rms0, FL.CF.CN.CC.LR.PA.FS.CopyA.CopyChain W v gfv sms rmsv h0' gf0 sms0 rms0 ->
  h0' = h0 -> rms0 = None ->
  forall Cv nsv, A.FunCtx W v Cv nsv -> TableOk T Cv ->
  (forall x, A.Fixed W v x -> lookup_subst x gfv = Some (app_subst T (TAbs x))) ->
  SigInv v Cv rmsv.
Proof.
  intros v gfv sms rmsv h0' gf0 sms0 rms0 Ch.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh gfx gfh sms rms rm h0' gf0 sms0 rms0 Sub IH FCu FCh Incl FixH _ InCu Same Site];
    intros E0 Er Cv nsv FCv Tok FixT.
  - subst u rms. destruct (FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FCv FC0) as [Ecv _]. subst Cv.
    exists outputs, keys. split; [exact PR0|].
    intros i r m a g a0 Ki Lk key Ik Si. cbn [closure_sources] in Si. unfold srcs in Si.
    rewrite nth_error_map, Ki in Si. injection Si as Es.
    pose proof (PublicCallerKeys.rkey_of_inj outputs (IMth r m a) key Es) as Pid.
    rewrite (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys (IMth r m a) PR0 (nth_error_In _ _ Ki)),
      (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key PR0 Ik) in Pid.
    subst key. exists r, a, g, a0. split; [reflexivity|]. split; [exact Lk|]. split; reflexivity.
  - subst h0' rms0.
    pose proof (FL.CF.CN.CC.LR.PA.chain_no_outputs W h gfh sms rms h0 gf0 sms0 None Sub NoH) as NoHh.
    assert (NoU : forall t, ~ A.FunOutputs W u t) by (intros t Ft; exact (NoHh t (proj1 (FL.CF.CN.CC.LR.PA.FS.CopyA.S.same_result_outputs W u h Same t) Ft))).
    destruct (FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FCv FCu) as [Ecv _]. subst Cv.
    pose proof (sub_inv W u Cu nsu T FCu Tok) as InvU.
    (* H's merges hold at T: the copy's input comparison, read at T *)
    destruct (FL.CF.CN.CC.LR.PA.FS.CopyA.copy_read_at W u h Cu nsu Ch nsh rm gfx gfh T FCu FCh InCu Site FixT)
      as [RdT _].
    assert (TokH : TableOk T Ch).
    { intros x tx Lx. pose proof (RdT x tx Lx) as E. unfold A.canon in E.
      rewrite <- (InvU tx), <- E, InvU. reflexivity. }
    pose proof (sub_inv W h Ch nsh T FCh TokH) as InvH.
    assert (FixTh : forall x, A.Fixed W h x -> lookup_subst x gfh = Some (app_subst T (TAbs x))).
    { intros x Fh. destruct (FixH x Fh) as [Fu E]. rewrite E. exact (FixT x Fu). }
    destruct (IH eq_refl eq_refl Ch nsh FCh TokH FixTh) as (Crh & ksh & PRh & Inv).
    destruct Site as (C1 & fx1 & fxh & d1 & _ & Ab1 & _ & Fx1 & CO1 & Egfx & _ & _ & _ & _ & AbU).
    set (sigma := map (fun q => (fst q, A.canon C1 (snd q))) fx1) in CO1.
    destruct CO1 as (outs & gen & ys & cg & Cr & ks & Cr' & ks' & Outs & Nd & Avoid & FrG & FrC & Ys & _ & _ & PRu & PRh' & Lrm & Pos).
    exists Cr, ks. split; [exact (pr_noout W u Cu _ _ Cr ks PRu NoU)|].
    intros i r m a g a0 Ki Lk key Ik Si.
    assert (Ea : a = []).
    { pose proof (PublicCallerKeys.result_key_id W u Cu _ Cr ks (IMth r m a) PRu (nth_error_In _ _ Ki)) as E.
      cbn in E. injection E as E. exact (eq_sym E). }
    subst a.
    destruct (Pos i _ Ki) as (j & k' & p & p' & Rj & Kj & Ekey & Lastj & Lp & Lp' & Ep). cbn [A.provision_id] in Lp.
    rewrite Lk in Lp. injection Lp as <-.
    assert (Idk' : A.provision_id k' = k') by exact (PublicCallerKeys.result_key_id W h Ch _ Cr' ks' k' PRh' (nth_error_In _ _ Kj)).
    destruct k' as [?|?|?|r' m' a'|?]; cbn in Ekey; try discriminate. cbn in Idk'. injection Idk' as Ea'. subst a'.
    cbn in Ekey. injection Ekey as Er' ->. cbn [A.provision_id] in Lp'.
    (* the copy step at T *)
    assert (FunFx : forall x t t', In (x, t) fx1 -> In (x, t') fx1 -> t = t').
    { intros x t t' I I'. destruct (proj1 (Fx1 x t) I) as [_ E]. destruct (proj1 (Fx1 x t') I') as [_ E']. congruence. }
    assert (Gen : forall y, (exists z w, A.nlook z (A.c_ty Cu) = Some w /\ In y (A.gens w)) ->
      PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y).
    { intros y Ry. split.
      - intros s0 Ls. pose proof (lookup_subst_in_pair _ _ _ Ls) as Is. split.
        + intros z Iz Ic. refine (Avoid z _ (in_or_app _ _ _ (or_intror Ic))).
          apply in_flat_map. exists (y, s0). split; [exact Is|exact Iz].
        + unfold sigma in Is. apply in_map_iff in Is as [[x' tx] [E Itx]]. cbn in E. injection E as -> <-.
          destruct (proj1 (Fx1 y tx) Itx) as [Fu _].
          pose proof (FixT y Fu) as Fg. rewrite Egfx, (FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_ground_fixed _ fx1 y tx FunFx Itx) in Fg.
          assert (Fg' : app_subst (type_meanings d1) tx = app_subst T (TAbs y)) by congruence.
          change (ground d1 (A.canon C1 tx) = app_subst T (TAbs y)). rewrite (Ab1 tx). exact Fg'.
      - intros Ls Ny. exfalso. apply Ny. apply Ys. split; [exact Ry|exact Ls]. }
    assert (NdC : NoDup (map cg ys)) by exact (NoDup_app_remove_l _ _ Nd).
    pose proof (FL.CF.CN.CC.LR.PA.FS.CopyA.keys_sub _ _ InCu) as SubK.
    (* H's terms, over Ch's keys, read through F's merge *)
    assert (GenH : forall t, incl (A.gens t) (map fst (A.c_ty Ch)) -> forall y, In y (A.gens (A.canon Cu t)) ->
      PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y).
    { intros t Sc y Iy. apply Gen. apply (FL.CF.CN.CC.LR.PA.FS.CopyA.canon_gens_read W u Cu nsu t FCu); [|exact Iy].
      intros z Iz. exact (SubK z (Sc z Iz)). }
    assert (ChK : forall y, (exists x tx, A.nlook x (A.c_ty Ch) = Some tx /\ In y (A.gens tx)) -> In y (map fst (A.c_ty Ch))).
    { intros y (x & tx & Lx & Iy). exact (table_gens_dom W h Ch nsh x tx y FCh Lx Iy). }
    assert (Tr : forall t, (forall y, In y (A.gens t) -> PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y) ->
      app_subst (PublicCopySubst.cg_read ys cg T (type_meanings d1)) (app_subst (A.copy_subst sigma ys cg) t) = app_subst T t)
      by (intros t G; exact (PublicCopySubst.copy_ty_read sigma ys cg T (type_meanings d1) NdC t G)).
    (* the receivers *)
    assert (Rgen : forall y, In y (A.gens r) -> PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y)
      by (intros y Iy; exact (Gen y (result_key_gens_vals W u Cu nsu u _ Cr ks FCu PRu NoU r m [] (nth_error_In _ _ Ki) y Iy))).
    assert (R'gen : forall y, In y (A.gens (A.canon Cu r')) -> PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y).
    { apply GenH. intros y Iy. apply ChK. exact (result_key_gens_vals W h Ch nsh h _ Cr' ks' FCh PRh' NoHh r' m [] (nth_error_In _ _ Kj) y Iy). }
    assert (ErT : app_subst T r = app_subst T r').
    { transitivity (app_subst (PublicCopySubst.cg_read ys cg T (type_meanings d1)) (app_subst (A.copy_subst sigma ys cg) r));
        [symmetry; exact (Tr r Rgen)|].
      transitivity (app_subst (PublicCopySubst.cg_read ys cg T (type_meanings d1)) (app_subst (A.copy_subst sigma ys cg) (A.canon Cu r')));
        [apply f_equal; exact (eq_sym Er')|].
      rewrite (Tr _ R'gen). exact (InvU r'). }
    (* the signatures *)
    destruct p' as [?|?|g' a0'|?]; cbn in Ep; try discriminate.
    assert (Eg : A.sig_subst T g = A.sig_subst T g').
    { destruct g as [ps rt], g' as [ps' rt']. unfold A.sig_subst in Ep |- *. cbn in Ep |- *. injection Ep as Eps Ert _.
      assert (Gg : forall y, In y (A.provision_gens (A.ProvidedMethod (A.FSig ps rt) a0)) -> PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y)
        by (intros y Iy; exact (Gen y (result_gens_vals W u Cu nsu u _ Cr ks FCu PRu NoU _ _ (nth_error_In _ _ Ki) Lk y Iy))).
      assert (Gg' : forall y, In y (A.provision_gens (A.ProvidedMethod (A.FSig ps' rt') a0')) -> In y (map fst (A.c_ty Ch)))
        by (intros y Iy; exact (ChK y (result_gens_vals W h Ch nsh h _ Cr' ks' FCh PRh' NoHh _ _ (nth_error_In _ _ Kj) Lp' y Iy))).
      unfold A.provision_gens in Gg, Gg'. cbn [A.provision_signature A.sg_ret A.sg_params] in Gg, Gg'.
      assert (GH : forall t, In t (rt' :: ps') -> incl (A.gens t) (map fst (A.c_ty Ch))).
      { intros t It z Iz. apply Gg'. apply in_or_app. left. apply in_flat_map. exists t. split; assumption. }
      assert (GF : forall t, In t (rt :: ps) -> forall y, In y (A.gens t) -> PublicCopySubst.CopyGen sigma ys cg T (type_meanings d1) y).
      { intros t It y Iy. apply Gg. apply in_or_app. left. apply in_flat_map. exists t. split; assumption. }
      f_equal.
      - transitivity (map (app_subst (PublicCopySubst.cg_read ys cg T (type_meanings d1))) (map (app_subst (A.copy_subst sigma ys cg)) ps)).
        { symmetry. apply PublicCopySubst.tys_agree. intros y Iy. apply in_flat_map in Iy as [t [It Iy]].
          exact (PublicCopySubst.copy_gen_read sigma ys cg T (type_meanings d1) NdC y (GF t (or_intror It) y Iy)). }
        rewrite Eps.
        transitivity (map (app_subst T) (map (app_subst (A.c_sub Cu)) ps')).
        { apply PublicCopySubst.tys_agree. intros y Iy. apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It0]].
          exact (PublicCopySubst.copy_gen_read sigma ys cg T (type_meanings d1) NdC y (GenH t0 (GH t0 (or_intror It0)) y Iy)). }
        rewrite map_map. apply map_ext. intros t. exact (InvU t).
      - rewrite <- (Tr rt (GF rt (or_introl eq_refl))), Ert, (Tr (app_subst (A.c_sub Cu) rt') (GenH rt' (GH rt' (or_introl eq_refl)))).
        exact (InvU rt'). }
    (* h's result read twice at Ch: with the copy's roles and with its own *)
    destruct (functx_mgm W h Ch nsh FCh) as [IdmH FnH].
    destruct (functx_scoped W h Ch nsh FCh) as (TyAH & _ & _ & _ & _ & _ & SubScH).
    set (thh := map (fun q => (fst q, A.canon Ch (app_subst (A.c_ty Ch) (TAbs (fst q))))) (A.c_ty Ch)).
    assert (PartsH : thh = map (fun q => (fst q, A.canon Ch (app_subst (A.c_ty Ch) (TAbs (fst q))))) (A.c_ty Ch) /\
      (forall t, In t (map fst (A.c_ty Ch)) -> A.nlook t (A.c_ty Ch) <> None) /\
      (forall g0 t, In (g0, t) (A.c_sub Ch) -> (forall x, In x (g0 :: A.gens t) -> A.nlook x (A.c_ty Ch) <> None) ->
         A.canon Ch (app_subst (A.c_ty Ch) (TAbs g0)) = A.canon Ch (app_subst (A.c_ty Ch) t)) /\
      (forall d ps a r b, A.w_decl W h = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
      A.ProvideResult W h Ch (fun t => TAbs (gen t)) Cr' ks').
    { split; [reflexivity|]. split.
      { intros t It. destruct (nlook_some _ t _ It) as [w Lw]. rewrite Lw. discriminate. }
      split.
      { intros g0 t Iq _. rewrite !(functx_ty_sub W h Ch nsh FCh). unfold A.canon. rewrite !IdmH.
        rewrite (PublicChainSigs.app_subst_abs (A.c_sub Ch) g0), (FnH g0 t Iq). symmetry.
        pose proof (IdmH (TAbs g0)) as I0. rewrite (PublicChainSigs.app_subst_abs (A.c_sub Ch) g0), (FnH g0 t Iq) in I0. exact I0. }
      split; [exact (chain_abs h gfh sms rms h0 gf0 sms0 None Sub Abs0)|exact PRh']. }
    assert (KTh : forall q pa x, A.resolve_sym W q pa = Some x -> A.nlook x (A.c_ty Ch) <> None ->
      A.nlook x (A.c_ty Ch) = None -> A.kind_sym W x = Some A.DKType).
    { intros q pa x _ N N'. exfalso. exact (N N'). }
    destruct (entries_rel_kinds W Ch h thh Ch (fun t => TAbs (gen t)) Cr' ks' Ch nsh Crh ksh eq_refl PartsH FCh NoHh PRh
      (fun t => IdmH t) KTh) as (tm1 & tm2 & L & ECrh & EKh & ECr' & EK' & FL).
    pose proof (PublicFamilyAgree.th_sigma W Ch h thh Ch (fun t => TAbs (gen t)) Cr' ks' Ch nsh eq_refl PartsH FCh) as ThS.
    assert (Nj : nth_error (keys_of (fun p => A.entry_key (A.c_sub Ch) (snd p)) L) j = Some (IMth r' m [])).
    { rewrite <- keys_of_map, <- EK'. exact Kj. }
    assert (Lj : forall j' k'', j < j' -> nth_error (keys_of (fun p => A.entry_key (A.c_sub Ch) (snd p)) L) j' = Some k'' -> k'' <> IMth r' m []).
    { intros j' k'' Lt N E. subst k''. rewrite <- keys_of_map, <- EK' in N.
      destruct (Lastj j' _ N ltac:(cbn; rewrite Er'; reflexivity)) as [_ Le]. lia. }
    destruct (pos_last (A.c_sub Ch) (A.c_sub Ch) L (kinds_of _ _ _ _ L FL) (tr_of _ _ _ _ L FL ThS) j r' m [] Nj Lj)
      as (pp & k1 & q1 & q2 & Ipp & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
    rewrite Forall_forall in FL. destruct (FL pp Ipp) as [R Sk].
    destruct (entry_mth _ _ r' m [] K2) as (r2 & g2 & a2 & E2 & -> & _). rewrite E2 in R, Sk, P2.
    destruct (fst pp) as [?|?|?|r1 m1 g1 a1|?|? ?] eqn:E1; try contradiction. destruct R as (-> & Rr & Sg & _).
    cbn in P2, K1, P1. injection P2 as <-. injection K1 as <-. injection P1 as <-.
    assert (Eg' : A.ProvidedMethod g' a0' = A.ProvidedMethod (A.sig_subst (A.c_sub Ch) g2) a2).
    { pose proof (I2 (A.c_provisions Ch)) as I2'. rewrite ECr' in Lp'. cbn [A.output_cx A.c_provisions] in Lp'. congruence. }
    injection Eg' as -> _.
    (* the chain's source for i is h's for j *)
    rewrite EagerCopy.sources_compose, nth_error_map, Rj in Si. injection Si as Es.
    assert (LenS : List.length (closure_sources rms srcs) = List.length ks').
    { refine (proj1 (FL.CF.CN.CC.LR.PA.FS.CopyA.chain_sources_range W h gfh sms rms h0 gf0 sms0 None Sub eq_refl srcs _) _ _ _ _ PRh').
      intros C0 o C0' ks0 P0. unfold srcs. rewrite length_map.
      exact (PublicCallerKeys.provide_result_length W h0 Ch0 _ outputs keys C0 o C0' ks0 PR0 P0). }
    assert (Sj : nth_error (closure_sources rms srcs) j = Some (A.rkey_of outputs key)).
    { rewrite <- Es. apply nth_error_nth'. rewrite LenS. apply nth_error_Some. rewrite Kj. discriminate. }
    assert (Nh : nth_error ksh j = Some (A.mkey (app_subst (A.c_sub Ch) r1) m)).
    { rewrite EKh, keys_of_map. exact N1. }
    assert (Lkh : A.ilook (A.mkey (app_subst (A.c_sub Ch) r1) m) (A.c_provisions Crh) =
      Some (A.ProvidedMethod (A.sig_subst (A.c_sub Ch) g1) a1)).
    { rewrite ECrh. cbn [A.output_cx A.c_provisions]. exact (I1 (A.c_provisions Ch)). }
    destruct (Inv j _ m [] _ a1 Nh Lkh key Ik Sj) as (rf & a' & gf & a1' & Ekey & Lroot & Erf & Egf).
    pose proof (th_inv W h Ch nsh T (A.c_ty Ch) FCh TokH) as ThI. fold thh in ThI.
    exists rf, a', gf, a1'. split; [exact Ekey|]. split; [exact Lroot|]. split.
    + rewrite ErT. unfold PublicOutputSubst.TyRel in Rr. rewrite <- Rr, ThI, <- Erf, InvH. reflexivity.
    + rewrite Eg, <- (sigrel_eq _ _ _ _ Sg), (sig_inv T thh g1 ThI), <- Egf, (sig_inv T _ g1 InvH).
      reflexivity.
Qed.
End Chain.

(* ---- the obligation ------------------------------------------------------------ *)

Theorem chain_forward_copy_sigs : forall Prog argv read state_ok, FL.CF.ChainForwardCopySigs W Prog argv read state_ok.
Proof.
  intros Prog argv read state_ok F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st Pre0
    code sms rms gfx Look Nn Fm h0 gf0 Chn Ch0 needs outputs keys bindings FD i r m g0 key Ki Last Mg Ikey Si.
  pose proof FD as (FC0 & NoH & PRf & _ & _ & _ & Abst0 & _).
  pose proof AP as (_ & _ & FC & Inp & Th & FixC & Sat & _ & _ & _ & _ & _ & PR).
  pose proof Pre0 as (_ & _ & _ & _ & [Abs [_ [Idem KMc]]] & Fmd).
  destruct Sat as [_ [_ [_ [_ Merge0]]]].
  assert (Force0 : forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty C) <> None).
  { intros t It. destruct (keys_in _ t It) as [v Lv]. destruct (Inp t v Lv) as [here Lh]. rewrite Lh. discriminate. }
  pose proof (PublicMergeClass.functx_merges W u Cu ns C FC Force0 Merge0) as Merge.
  assert (AbU : Abstract u) by exact (chain_abs u gfx sms rms h0 gf0 None None Chn Abst0).
  pose proof (FL.CF.CN.CC.LR.PA.chain_no_outputs W u gfx sms rms h0 gf0 None None Chn NoH) as NoU.
  assert (Parts : th = map (fun q => (fst q, A.canon C (app_subst (A.c_ty C) (TAbs (fst q))))) (A.c_ty Cu) /\
    (forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty C) <> None) /\
    (forall g t, In (g, t) (A.c_sub Cu) -> (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty C) <> None) ->
       A.canon C (app_subst (A.c_ty C) (TAbs g)) = A.canon C (app_subst (A.c_ty C) t)) /\
    (forall d ps a r b, A.w_decl W u = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
    A.ProvideResult W u C (fun t => TAbs (gensof t)) C' ks).
  { split; [exact Th|]. split.
    { intros t It. destruct (keys_in _ t It) as [v Lv]. destruct (Inp t v Lv) as [here Lh]. rewrite Lh. discriminate. }
    split; [exact Merge|]. split; [exact AbU|exact PR]. }
  set (T := (map (fun q => (fst q, ground d (snd q))) th ++ type_meanings d)%list).
  assert (GT : forall t, ground d (app_subst th t) = app_subst T t) by (intros t; exact (PublicCopySubst.ground_app_subst d th t)).
  pose proof (PublicFamilyAgree.th_sigma W C u th C (fun t => TAbs (gensof t)) C' ks Cu ns eq_refl Parts FC) as ThS.
  destruct (functx_scoped W u Cu ns FC) as (TyA & _ & _ & _ & _ & _ & _).
  assert (Tok : TableOk T Cu).
  { intros x tx Lx. rewrite <- !GT, (TyA x tx Lx), ThS. reflexivity. }
  pose proof (sub_inv W u Cu ns T FC Tok) as InvU.
  assert (FixT : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))).
  { intros x Fx. destruct Fm as [Cov Mean]. destruct (Cov x Fx) as [tx Itx]. rewrite (Mean x tx Itx). f_equal.
    destruct (FixC x tx Itx) as [here [Lh Eh]].
    pose proof (FL.CF.CN.CC.LR.PA.FS.CopyA.S.fixed_in_functx W u Cu ns x (ast_of_abstract u Cu ns FC AbU) FC Fx) as Ix.
    assert (Lx : lookup_subst x th = Some (A.canon C (app_subst (A.c_ty C) (TAbs x)))).
    { rewrite Th. exact (PublicFamilyAgree.lookup_keys (fun y => A.canon C (app_subst (A.c_ty C) (TAbs y))) (A.c_ty Cu) x Ix). }
    rewrite <- GT, app_subst_abs, Lx, (app_subst_abs (A.c_ty C)), lookup_subst_nlook', Lh, Eh. symmetry. exact (Abs tx). }
  destruct (chain_sigs h0 Ch0 needs outputs keys FC0 NoH PRf Abst0 T u gfx sms rms h0 gf0 None None Chn eq_refl eq_refl
    Cu ns FC Tok FixT) as (Crv & ksv & PRv & Inv).
  pose proof (FL.CF.app_keys_typed W F C pa u fx Cu ns sm th gensof C' ks newgens outs (proj2 (proj2 (proj1 Fmd))) AP) as KT.
  destruct (entries_rel_kinds W C u th C (fun t => TAbs (gensof t)) C' ks Cu ns Crv ksv eq_refl Parts FC NoU PRv Idem KT)
    as (tm1 & tm2 & L & ECrv & EKv & EC' & EKs & FL).
  assert (Ni : nth_error (keys_of (fun p => A.entry_key (A.c_sub C) (snd p)) L) i = Some (IMth r m [])).
  { rewrite <- keys_of_map, <- EKs. exact Ki. }
  assert (Li : forall i' k', i < i' -> nth_error (keys_of (fun p => A.entry_key (A.c_sub C) (snd p)) L) i' = Some k' -> k' <> IMth r m []).
  { intros i' k' Lt N E. subst k'. rewrite <- keys_of_map, <- EKs in N. exact (Last i' Lt N). }
  destruct (pos_last (A.c_sub Cu) (A.c_sub C) L (kinds_of _ _ _ _ L FL) (tr_of _ _ _ _ L FL ThS) i r m [] Ni Li)
    as (pp & k1 & q1 & q2 & Ipp & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
  rewrite Forall_forall in FL. destruct (FL pp Ipp) as [R Sk].
  destruct (entry_mth _ _ r m [] K2) as (r2 & g2 & a2 & E2 & -> & _). rewrite E2 in R, Sk, P2.
  destruct (fst pp) as [?|?|?|r1 m1 g1 a1|?|? ?] eqn:E1; try contradiction. destruct R as (-> & Rr & Sg & _).
  cbn in P2, K1, P1. injection P2 as <-. injection K1 as <-. injection P1 as <-.
  (* the caller's method is entry i's *)
  assert (KM' : PublicCallRealize.KeysMatch (A.c_provisions C')) by exact (pr_keys_match W u C _ C' ks PR KMc).
  assert (Eg0 : g0 = A.sig_subst (A.c_sub C) g2).
  { change (A.c_mths C') with (PublicCallRealize.mths_of (A.c_provisions C')) in Mg.
    rewrite (PublicCallRealize.mths_by_ilook _ _ _ KM') in Mg. unfold A.mkey in Mg.
    pose proof (I2 (A.c_provisions C)) as I2'. rewrite EC' in Mg. cbn [A.output_cx A.c_provisions] in Mg.
    rewrite I2' in Mg. congruence. }
  subst g0.
  assert (Nk : nth_error ksv i = Some (IMth (app_subst (A.c_sub Cu) r1) m [])) by (rewrite EKv, keys_of_map; exact N1).
  assert (Lk : A.ilook (IMth (app_subst (A.c_sub Cu) r1) m []) (A.c_provisions Crv) =
    Some (A.ProvidedMethod (A.sig_subst (A.c_sub Cu) g1) a1))
    by (rewrite ECrv; cbn [A.output_cx A.c_provisions]; exact (I1 (A.c_provisions Cu))).
  destruct (Inv i _ m [] _ a1 Nk Lk key Ikey Si) as (rf & a' & gf & a1' & Ekey & Lroot & Erf & Egf).
  (* the root's item *)
  assert (KMo : PublicCallRealize.KeysMatch (A.c_provisions outputs)).
  { destruct (functx_scoped W h0 Ch0 needs FC0) as (_ & _ & KM0 & _). exact (pr_keys_match W h0 Ch0 _ outputs keys PRf KM0). }
  assert (Ea' : a' = []).
  { pose proof (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key PRf Ikey) as E. rewrite Ekey in E.
    cbn in E. injection E as E. exact (eq_sym E). }
  subst a'.
  assert (Mgf : A.mlook rf m (A.c_mths outputs) = Some gf).
  { change (A.c_mths outputs) with (PublicCallRealize.mths_of (A.c_provisions outputs)).
    rewrite (PublicCallRealize.mths_by_ilook _ _ _ KMo). unfold A.mkey. rewrite Lroot. reflexivity. }
  assert (Dom0 : forall y, In y (A.gens rf) \/ In y (flat_map A.gens (A.sg_ret gf :: A.sg_params gf)) -> In y (map fst (A.c_ty Ch0))).
  { intros y [Iy|Iy].
    - rewrite Ekey in Ikey.
      destruct (result_key_gens_vals W h0 Ch0 needs h0 _ outputs keys FC0 PRf NoH rf m [] Ikey y Iy) as (x & tx & Lx & Ix).
      exact (table_gens_dom W h0 Ch0 needs x tx y FC0 Lx Ix).
    - assert (Lr' : A.ilook (A.provision_id key) (A.c_provisions outputs) = Some (A.ProvidedMethod gf a1')) by (rewrite Ekey; exact Lroot).
      assert (Iy' : In y (A.provision_gens (A.ProvidedMethod gf a1'))) by (unfold A.provision_gens; cbn [A.provision_signature]; apply in_or_app; left; exact Iy).
      destruct (result_gens_vals W h0 Ch0 needs h0 _ outputs keys FC0 PRf NoH key _ Ikey Lr' y Iy') as (x & tx & Lx & Ix).
      exact (table_gens_dom W h0 Ch0 needs x tx y FC0 Lx Ix). }
  (* each caller type is θ of u's, which T reads as the root's *)
  assert (One : forall t1 tf, app_subst T (app_subst (A.c_sub Cu) t1) = app_subst T tf -> incl (A.gens tf) (map fst (A.c_ty Ch0)) ->
    FL.CF.CN.CC.LR.PA.SigAgreeG d Ch0 th (app_subst th t1) tf).
  { intros t1 tf E Inc. split; [exact Inc|]. rewrite !GT, <- E, InvU. reflexivity. }
  exists rf, [], gf. split; [exact Ekey|]. split; [exact Mgf|].
  pose proof (sigrel_eq _ _ _ _ Sg) as Eg. rewrite <- Eg. destruct g1 as [ps1 rt1], gf as [psf rtf].
  unfold A.sig_subst in Egf, Eg |- *. cbn [A.sg_ret A.sg_params] in *. injection Egf as Ep Er.
  unfold PublicOutputSubst.TyRel in Rr. rewrite <- Rr.
  constructor; [apply One; [exact Er|intros y Iy; apply Dom0; right; apply in_or_app; left; exact Iy]|].
  constructor; [apply One; [exact Erf|intros y Iy; apply Dom0; left; exact Iy]|].
  assert (DomP : forall t, In t psf -> incl (A.gens t) (map fst (A.c_ty Ch0))).
  { intros t It y Iy. apply Dom0. right. cbn [flat_map]. apply in_or_app. right. apply in_flat_map. exists t. split; assumption. }
  clear -Ep DomP One. revert psf Ep DomP. induction ps1 as [|t1 ps1 IH]; intros [|tf psf] Ep DomP; cbn in Ep; try discriminate; [constructor|].
  injection Ep as E Rest. constructor; [apply One; [rewrite <- E; reflexivity|exact (DomP tf (or_introl eq_refl))]|].
  apply IH; [exact Rest|intros t It; exact (DomP t (or_intror It))].
Qed.

End Sigs.
End ChainSigs.
