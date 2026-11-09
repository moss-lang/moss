(* CN.ChainRealizeOk (realize4): applying F, whose closure is a copy chain
   F = u <- h <- ... <- h0 down to a linked root h0 with body G, the
   caller's new provisions are realized by what the body provides.

   One valuation reads every level's inputs, T_in x = ground d (θ x) (the
   meaning the application gives u's input x), and each level v reads its
   own output roles gen_v t at the output's meaning M t in the body's
   final environment (read_meaning d3 t). A level's statement (LevelReal):
   at every position of v's own result, the chain's source for it holds a
   value that realizes the position's provision at that valuation (RealAt).
   - The root (root_level): the body's provision at the position's key
     (CC.body_provides) is typed at the body's meanings of the renamed
     roles; a role means M t (AliasOk of the body's context), and an input
     the body's table reads means T_in (the run fact,
     PublicLinkRun.link_run_fact), since every input a formal provision
     mentions is one of G's (the body adds only made symbols, which avoid
     h0's inputs: PublicLinkScope.items_scope, LinkData's made clause).
   - A copy step u <- h (copy_level): CopyOutputs compares u's result at Cu
     with h's at Cu, which read at T agree (PublicCopyFresh.copy_outputs_at);
     h's result at Cu and at Ch correspond entry by entry (level_rel, with
     outputs), and the last position for a key is the same on both sides
     (pos_last_all).
   - At the top (chain_realize_ok), the caller's reading at C and u's at Cu
     correspond the same way; the caller's new provisions are read at the
     state after the application, which grounds θ x as d does and each
     destination gensof t as M t. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicTagCheck
  PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply PublicFamilyAgree PublicChainNew PublicChainFwd
  PublicLinkResults PublicChainCore PublicLinkScope.
From NM Require PublicFormation PublicTypeFacts PublicCallerKeys PublicCopyFresh PublicScoping PublicFamilyStep PublicFamilyLink
  PublicChainSigs PublicOutputSubst PublicLinkRun PublicFamilySig.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- the last position for any key, on two related readings ----------------- *)

Lemma pos_last_all : forall (s1 s2 : subst) (L : list (A.entry * A.entry)),
  (forall p, In p L -> (A.entry_key s1 (fst p) = None <-> A.entry_key s2 (snd p) = None)) ->
  (forall p p' k, In p L -> In p' L -> A.entry_key s2 (snd p) = Some k ->
     A.entry_key s1 (fst p) = A.entry_key s1 (fst p') -> A.entry_key s2 (snd p') = Some k) ->
  forall j k, nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L) j = Some k ->
  (forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L) j' = Some k' -> k' <> k) ->
  exists p k1 q1 q2, In p L /\ A.entry_key s1 (fst p) = Some k1 /\ A.entry_key s2 (snd p) = Some k /\
    nth_error (keys_of (fun p => A.entry_key s1 (fst p)) L) j = Some k1 /\
    (forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s1 (fst p)) L) j' = Some k' -> k' <> k1) /\
    A.entry_provision s1 (fun a => a) (fst p) = [(k1, q1)] /\ A.entry_provision s2 (fun a => a) (snd p) = [(k, q2)] /\
    (forall rest, A.ilook k1 (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst L)) ++ rest) = Some q1) /\
    (forall rest, A.ilook k (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd L)) ++ rest) = Some q2).
Proof.
  intros s1 s2 L. induction L as [|x L0 IH] using rev_ind; intros Kd Tr j k Nj Lj.
  { destruct j; cbn in Nj; discriminate. }
  assert (Kd0 : forall p, In p L0 -> (A.entry_key s1 (fst p) = None <-> A.entry_key s2 (snd p) = None))
    by (intros p Ip; apply Kd; apply in_or_app; left; exact Ip).
  assert (Tr0 : forall p p' k, In p L0 -> In p' L0 -> A.entry_key s2 (snd p) = Some k ->
     A.entry_key s1 (fst p) = A.entry_key s1 (fst p') -> A.entry_key s2 (snd p') = Some k)
    by (intros p p' k0 Ip Ip'; apply Tr; apply in_or_app; left; assumption).
  assert (Ix : In x (L0 ++ [x])) by (apply in_or_app; right; left; reflexivity).
  pose proof (PublicChainSigs.keys_of_len L0 (A.entry_key s1) (A.entry_key s2) Kd0) as Len.
  rewrite PublicChainSigs.keys_of_snoc in Nj, Lj |- *.
  assert (R1 : forall rest, (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst (L0 ++ [x]))) ++ rest)%list =
    (A.entry_provision s1 (fun a => a) (fst x) ++ (flat_map (A.entry_provision s1 (fun a => a)) (rev (map fst L0)) ++ rest))%list)
    by (intros rest; rewrite map_app; apply PublicChainSigs.flat_map_rev_snoc).
  assert (R2 : forall rest, (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd (L0 ++ [x]))) ++ rest)%list =
    (A.entry_provision s2 (fun a => a) (snd x) ++ (flat_map (A.entry_provision s2 (fun a => a)) (rev (map snd L0)) ++ rest))%list)
    by (intros rest; rewrite map_app; apply PublicChainSigs.flat_map_rev_snoc).
  destruct (A.entry_key s2 (snd x)) as [a2|] eqn:E2.
  - destruct (A.entry_key s1 (fst x)) as [a1|] eqn:E1;
      [|exfalso; pose proof (proj1 (Kd x Ix) E1) as N; congruence].
    destruct (PublicFamilyAgree.entry_provision_of s1 (fun a => a) (fst x) a1 E1) as [px1 Ex1].
    destruct (PublicFamilyAgree.entry_provision_of s2 (fun a => a) (snd x) a2 E2) as [px2 Ex2].
    remember (List.length (keys_of (fun p => A.entry_key s2 (snd p)) L0)) as n eqn:En.
    destruct (Nat.lt_ge_cases j n) as [Lt|Ge].
    + rewrite nth_error_app1 in Nj by lia.
      assert (Lj0 : forall j' k', j < j' -> nth_error (keys_of (fun p => A.entry_key s2 (snd p)) L0) j' = Some k' -> k' <> k).
      { intros j' k' Lt' N'. apply (Lj j' k' Lt'). rewrite nth_error_app1; [exact N'|].
        apply nth_error_Some. rewrite N'. discriminate. }
      destruct (IH Kd0 Tr0 j k Nj Lj0) as (p & k1 & q1 & q2 & Ip & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
      assert (Ne2 : a2 <> k).
      { intros ->. apply (Lj n k Lt); [|reflexivity]. rewrite nth_error_app2 by lia.
        rewrite <- En, Nat.sub_diag. reflexivity. }
      assert (Ne1 : a1 <> k1).
      { intros ->. apply Ne2. pose proof (Tr p x k (in_or_app _ _ _ (or_introl Ip)) Ix K2 (eq_trans K1 (eq_sym E1))) as T2.
        rewrite E2 in T2. injection T2 as T2. exact T2. }
      exists p, k1, q1, q2. split; [apply in_or_app; left; exact Ip|]. split; [exact K1|]. split; [exact K2|].
      split; [rewrite nth_error_app1 by lia; exact N1|]. split.
      * intros j' k' Lt' N'. destruct (Nat.lt_ge_cases j' n) as [Lt2|Ge2].
        -- rewrite nth_error_app1 in N' by lia. exact (L1 j' k' Lt' N').
        -- rewrite nth_error_app2 in N' by lia. rewrite Len in N'. destruct (j' - n) as [|z];
             cbn in N'; [injection N' as <-; exact Ne1|destruct z; cbn in N'; discriminate].
      * split; [exact P1|]. split; [exact P2|]. split.
        -- intros rest. rewrite R1, Ex1. cbn [app]. rewrite PublicChainSigs.ilook_skip by (intros E; apply Ne1; symmetry; exact E). exact (I1 rest).
        -- intros rest. rewrite R2, Ex2. cbn [app]. rewrite PublicChainSigs.ilook_skip by (intros E; apply Ne2; symmetry; exact E). exact (I2 rest).
    + rewrite nth_error_app2 in Nj by lia. rewrite <- En in Nj. destruct (j - n) as [|z] eqn:Ej; cbn in Nj; [|destruct z; cbn in Nj; discriminate].
      injection Nj as ->. assert (Ejn : j = n) by lia. subst j.
      exists x, a1, px1, px2. split; [exact Ix|]. split; [exact E1|]. split; [exact E2|].
      split; [rewrite nth_error_app2 by lia; rewrite Len, Nat.sub_diag; reflexivity|]. split.
      * intros j' k' Lt' N'. rewrite nth_error_app2 in N' by lia. rewrite Len in N'.
        destruct (j' - n) as [|z] eqn:Ez; [lia|].
        cbn in N'. destruct z; cbn in N'; discriminate.
      * split; [exact Ex1|]. split; [exact Ex2|]. split; intros rest; [rewrite R1, Ex1|rewrite R2, Ex2]; cbn [app]; apply PublicChainSigs.ilook_hit.
  - assert (E1 : A.entry_key s1 (fst x) = None) by exact (proj2 (Kd x Ix) E2). rewrite E1.
    rewrite !app_nil_r in Nj, Lj |- *.
    destruct (IH Kd0 Tr0 j k Nj Lj) as (p & k1 & q1 & q2 & Ip & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
    exists p, k1, q1, q2. split; [apply in_or_app; left; exact Ip|]. do 6 (split; [assumption|]). split; intros rest.
    + rewrite R1, (PublicFamilyAgree.entry_provision_none s1 _ (fst x) E1). exact (I1 rest).
    + rewrite R2, (PublicFamilyAgree.entry_provision_none s2 _ (snd x) E2). exact (I2 rest).
Qed.

(* ---- output type maps ------------------------------------------------------- *)

Lemma nlook_map_out : forall (o : nat -> ty) outs k, In k outs -> A.nlook k (map (fun t => (t, o t)) outs) = Some (o k).
Proof.
  intros o outs k. induction outs as [|t rest IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec k t) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma nlook_map_out_none : forall (o : nat -> ty) outs k, ~ In k outs -> A.nlook k (map (fun t => (t, o t)) outs) = None.
Proof.
  intros o outs k. induction outs as [|t rest IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec k t) as [->|Ne]; [exfalso; apply N; left; reflexivity|]. apply IH. intros I. apply N. right. exact I.
Qed.

Lemma otm_out : forall C outs (o : nat -> ty) k, In k outs -> A.nlook k (A.output_type_map C outs o) = Some (A.canon C (o k)).
Proof.
  intros C outs o k I. unfold A.output_type_map. rewrite PublicFamilyAgree.nlook_map_snd, nlook_app', nlook_map_out by exact I.
  reflexivity.
Qed.

Lemma otm_in : forall C outs (o : nat -> ty) k, ~ In k outs ->
  A.nlook k (A.output_type_map C outs o) = option_map (A.canon C) (A.nlook k (A.c_ty C)).
Proof.
  intros C outs o k N. unfold A.output_type_map. rewrite PublicFamilyAgree.nlook_map_snd, nlook_app', nlook_map_out_none by exact N.
  reflexivity.
Qed.

(* A substitution after another one that covers a type's generations. *)
Lemma app_subst_app_dom : forall (s r : subst) t, incl (A.gens t) (map fst s) -> app_subst (s ++ r) t = app_subst s t.
Proof.
  intros s r t Inc. apply subst_agree_on. intros x Ix. cbn [app_subst].
  rewrite PublicCopyFresh.lookup_subst_cat. destruct (lookup_subst x s) eqn:L; [reflexivity|].
  exfalso. destruct (nlook_some _ x s (Inc x Ix)) as [w Lw]. rewrite lookup_subst_nlook' in L. congruence.
Qed.

Lemma lookup_out_keys : forall (f : nat * ty -> ty) (l : list (nat * ty)) r x, ~ In x (map fst l) ->
  lookup_subst x (map (fun q => (fst q, f q)) l ++ r) = lookup_subst x r.
Proof.
  intros f l r x N. rewrite PublicCopyFresh.lookup_subst_cat, (PublicChainSigs.lookup_subst_out f l x N). reflexivity.
Qed.

(* The role map of an injective output map. *)
Lemma lookup_roles : forall (g : nat -> nat) (o : nat -> ty) outs t, In t outs ->
  (forall a b, In a outs -> In b outs -> g a = g b -> a = b) ->
  lookup_subst (g t) (map (fun t => (g t, o t)) outs) = Some (o t).
Proof.
  intros g o outs t. induction outs as [|a rest IH]; intros I Inj; [contradiction|]. cbn.
  destruct (Nat.eqb_spec (g t) (g a)) as [E|Ne].
  - rewrite (Inj t a I (or_introl eq_refl) E). reflexivity.
  - destruct I as [<-|I]; [congruence|]. apply IH; [exact I|].
    intros a' b' Ia Ib E. exact (Inj a' b' (or_intror Ia) (or_intror Ib) E).
Qed.

(* A functor entry of a result lists every fixed input of its functor. *)
Lemma fun_entries_fixed : forall W,
  (forall p ob tm sp es tm', A.OutputItemEntries W p ob tm sp es tm' ->
     forall u fx, In (A.NFun u fx) es -> forall x, A.Fixed W u x <-> exists tx, In (x, tx) fx) /\
  (forall p ob tm sps es tm', A.OutputItemsEntries W p ob tm sps es tm' ->
     forall u fx, In (A.NFun u fx) es -> forall x, A.Fixed W u x <-> exists tx, In (x, tx) fx).
Proof.
  intros W.
  apply (PublicFormation.oies_mut W
    (fun p ob tm sp es tm' _ => forall u fx, In (A.NFun u fx) es -> forall x, A.Fixed W u x <-> exists tx, In (x, tx) fx)
    (fun p ob tm sps es tm' _ => forall u fx, In (A.NFun u fx) es -> forall x, A.Fixed W u x <-> exists tx, In (x, tx) fx)).
  - intros p ob tm pa a bs t R K Br u fx [E|[]]. discriminate.
  - intros p ob tm pa a bs s k R K Br Kk u fx [].
  - intros p ob tm a pa s d n t ty0 bs ap R D Ast Br Ty Ap u fx [E|[]]. discriminate.
  - intros p ob tm a pa s g bs ap d rcv n ps rt R K D Ast Br Sig Ap u fx [E|[]]. discriminate.
  - intros p ob tm a bs pa u d n ps args res fx R D Ast Br Av Fx u' fx' [E|[]] x. injection E as <- <-.
    split; [|intros [tx Ix]; exact (proj1 (proj1 (Fx x tx) Ix))]. intros Fxx.
    pose proof (Av x Fxx) as N. destruct (A.nlook x (bs ++ ob ++ tm)) as [tx|] eqn:L; [|exfalso; exact (N eq_refl)].
    exists tx. apply (proj2 (Fx x tx)). split; [exact Fxx|exact L].
  - intros p ob tm a pa dn r m rt g bs ap R Mn Br Rt Sig Ap u fx [E|[]]. discriminate.
  - intros p ob tm a pa c d n its bs es tm' R D Ast Br Its IH. exact IH.
  - intros p ob tm u fx [].
  - intros p ob tm sp rest es1 tm1 es2 tm2 One IH1 Rest IH2 u fx I. apply in_app_or in I as [I|I]; [exact (IH1 u fx I)|exact (IH2 u fx I)].
Qed.

(* ---- two readings of one result, with outputs ---------------------------------- *)

(* v's result read in its own context Cv with roles g1, and in a context C
   whose readings th of v's inputs, its merges and its keys are those of an
   application (Parts), with roles g2. Their entries correspond one to one
   at s1 = th ++ [g1 t := g2 t] and C's merges: every type, signature and
   functor reading (EntryRel), and every method's receiver exactly
   (EntryRelM). *)
Section LevelRel.
Variable W : A.world.
Variables (C : A.cx) (v : nat) (th : subst) (g1 g2 : nat -> nat) (outs : list nat).
Variables (Cv : A.cx) (needs : list item) (Cr1 : A.cx) (ks1 : list item) (Cr2 : A.cx) (ks2 : list item).
Import A.
Hypothesis Parts : th = map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cv) /\
  (forall t, In t (map fst (c_ty Cv)) -> nlook t (c_ty C) <> None) /\
  (forall g t, In (g, t) (c_sub Cv) -> (forall x, In x (g :: gens t) -> nlook x (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) t)) /\
  (forall d ps a r b, w_decl W v = Some d -> functor_parts (d_ast d) = Some (ps, a, r, b) -> b = None) /\
  ProvideResult W v C (fun t => TAbs (g2 t)) Cr2 ks2.
Hypothesis FCv : FunCtx W v Cv needs.
Hypothesis PR1 : ProvideResult W v Cv (fun t => TAbs (g1 t)) Cr1 ks1.
Hypothesis Idem : forall t, canon C (canon C t) = canon C t.
Hypothesis KeysT : forall q pa x, resolve_sym W q pa = Some x ->
  nlook x (c_ty C) <> None -> nlook x (c_ty Cv) = None -> kind_sym W x = Some DKType.
Hypothesis OutsV : forall t, In t outs <-> FunOutputs W v t.
Hypothesis G1fresh : forall t, In t outs -> ~ In (g1 t) (map fst (c_ty Cv)).
Hypothesis G1inj : forall a b, In a outs -> In b outs -> g1 a = g1 b -> a = b.
Hypothesis G2sub : forall t, In t outs -> lookup_subst (g2 t) (c_sub C) = None.

Lemma th_keys : map fst th = map fst (c_ty Cv).
Proof. rewrite (proj1 Parts), map_map. reflexivity. Qed.

Lemma g1_sub : forall t, In t outs -> lookup_subst (g1 t) (c_sub Cv) = None.
Proof.
  intros t It. destruct (lookup_subst (g1 t) (c_sub Cv)) as [w|] eqn:L; [|reflexivity]. exfalso.
  destruct (functx_scoped W v Cv needs FCv) as (_ & _ & _ & _ & _ & _ & SubSc).
  exact (G1fresh t It (proj1 (SubSc _ w (lookup_subst_in_pair _ _ _ L)))).
Qed.

Lemma s1_role : forall t, In t outs ->
  app_subst (th ++ map (fun t => (g1 t, TAbs (g2 t))) outs) (TAbs (g1 t)) = TAbs (g2 t).
Proof.
  intros t It. cbn [app_subst]. rewrite PublicCopyFresh.lookup_subst_cat.
  assert (N : lookup_subst (g1 t) th = None).
  { rewrite (proj1 Parts). apply PublicChainSigs.lookup_subst_out. exact (G1fresh t It). }
  rewrite N, (lookup_roles g1 (fun t => TAbs (g2 t)) outs t It G1inj). reflexivity.
Qed.

Lemma s1_dom : forall t, incl (gens t) (map fst (c_ty Cv)) ->
  app_subst (th ++ map (fun t => (g1 t, TAbs (g2 t))) outs) t = app_subst th t.
Proof. intros t Inc. apply app_subst_app_dom. rewrite th_keys. exact Inc. Qed.

Lemma t1_otm : forall outs1 k, ~ In k outs1 -> nlook k (output_type_map Cv outs1 (fun t => TAbs (g1 t))) = nlook k (PublicFamilyAgree.T1 Cv).
Proof. intros outs1 k N. rewrite otm_in by exact N. unfold PublicFamilyAgree.T1. rewrite otm_in by (intros []). reflexivity. Qed.
Lemma t2_otm : forall outs2 k, ~ In k outs2 -> nlook k (output_type_map C outs2 (fun t => TAbs (g2 t))) = nlook k (PublicFamilyAgree.T2 C).
Proof. intros outs2 k N. rewrite otm_in by exact N. unfold PublicFamilyAgree.T2. rewrite otm_in by (intros []). reflexivity. Qed.

Lemma level_rel : exists tm1 tm2 L,
  Cr1 = output_cx Cv tm1 (map fst L) /\ ks1 = keys_of (entry_key (c_sub Cv)) (map fst L) /\
  Cr2 = output_cx C tm2 (map snd L) /\ ks2 = keys_of (entry_key (c_sub C)) (map snd L) /\
  Forall (fun p => PublicOutputSubst.EntryRel (th ++ map (fun t => (g1 t, TAbs (g2 t))) outs) (c_sub C) tm1 tm2 (fst p) (snd p) /\
    EntryRelM W (th ++ map (fun t => (g1 t, TAbs (g2 t))) outs) (c_sub C) (map fst (c_ty Cv) ++ map g1 outs) (fst p) (snd p) /\
    PublicCallerKeys.SameKind (fst p) (snd p) /\
    PublicFormation.EntryP (PublicScoping.TyIn (fun y => In y (map fst (c_ty Cv) ++ map g1 outs))) (fst p) /\
    (forall u fx1 fx2, fst p = NFun u fx1 -> snd p = NFun u fx2 -> forall x t2, In (x, t2) fx2 -> exists t1, In (x, t1) fx1)) L.
Proof.
  set (s1 := (th ++ map (fun t => (g1 t, TAbs (g2 t))) outs)%list).
  set (K := (map fst (c_ty Cv) ++ map g1 outs)%list).
  pose proof PR1 as (d & ps & a & r & b & outs1 & es1 & tm1 & D & FP & O1 & E1 & K1 & Eo).
  pose proof Parts as [_ [InC [_ [Abst PR2]]]].
  destruct PR2 as (d' & ps' & a' & r' & b' & outs2 & es2 & tm2 & D' & FP' & O2 & E2 & K2 & ECr).
  rewrite D in D'. injection D' as <-. rewrite FP in FP'. injection FP' as <- <- <- <-.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E1) in *.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E2) in *.
  set (T1 := output_type_map Cv outs1 (fun t => TAbs (g1 t))) in *.
  set (T2 := output_type_map C outs2 (fun t => TAbs (g2 t))) in *.
  assert (O12 : forall k, In k outs1 <-> In k outs2) by (intros k; rewrite O1, O2; reflexivity).
  assert (O1o : forall k, In k outs1 <-> In k outs) by (intros k; rewrite O1, OutsV; reflexivity).
  pose proof (PublicFamilyAgree.agree_t W C v th C (fun t => TAbs (g2 t)) Cr2 ks2 Cv needs eq_refl Parts FCv Idem) as AgT.
  pose proof (PublicFamilyAgree.kg_t W v Cv needs FCv) as KgT.
  assert (GensT1 : forall k w, nlook k (PublicFamilyAgree.T1 Cv) = Some w -> incl (gens w) (map fst (c_ty Cv))).
  { intros k w L y Iy. apply KgT. cbn [app]. exact (PublicFormation.in_tm_gens _ y k w (nlook_in _ _ _ _ L) Iy). }
  assert (Ag : PublicTypeRead.Agree s1 (c_sub C) T1 T2).
  { intros k v1 v2 L1 L2. destruct (in_dec Nat.eq_dec k outs1) as [Ik|Nk].
    - pose proof (proj1 (O1o k) Ik) as Iko.
      unfold T1 in L1. rewrite otm_out in L1 by exact Ik. injection L1 as <-.
      unfold T2 in L2. rewrite otm_out in L2 by exact (proj1 (O12 k) Ik). injection L2 as <-.
      unfold canon. cbn [app_subst]. rewrite (g1_sub k Iko), (G2sub k Iko).
      unfold s1. rewrite (s1_role k Iko). cbn [app_subst]. rewrite (G2sub k Iko). reflexivity.
    - unfold T1 in L1. rewrite t1_otm in L1 by exact Nk. unfold T2 in L2. rewrite t2_otm in L2 by (rewrite <- O12; exact Nk).
      unfold s1. rewrite s1_dom by exact (GensT1 k v1 L1). exact (AgT k v1 v2 L1 L2). }
  assert (Dm : DomT W T1 T2).
  { split.
    - intros x N. unfold T2. destruct (in_dec Nat.eq_dec x outs1) as [Ix|Nx].
      + rewrite otm_out by exact (proj1 (O12 x) Ix). discriminate.
      + unfold T1 in N. rewrite otm_in in N by exact Nx. rewrite otm_in by (rewrite <- O12; exact Nx).
        destruct (nlook x (c_ty Cv)) eqn:Lx; [|exfalso; exact (N eq_refl)].
        pose proof (InC x (proj1 (PublicFamilyAgree.nlook_dom _ x _) ltac:(rewrite Lx; discriminate))) as N2.
        destruct (nlook x (c_ty C)); [discriminate|exfalso; exact (N2 eq_refl)].
    - intros q pa x R N2 N1. destruct (in_dec Nat.eq_dec x outs1) as [Ix|Nx].
      + unfold T1 in N1. rewrite otm_out in N1 by exact Ix. discriminate.
      + unfold T1 in N1. rewrite otm_in in N1 by exact Nx. unfold T2 in N2. rewrite otm_in in N2 by (rewrite <- O12; exact Nx).
        apply (KeysT q pa x R); [destruct (nlook x (c_ty C)); [discriminate|exfalso; exact (N2 eq_refl)]|].
        destruct (nlook x (c_ty Cv)); [discriminate|reflexivity]. }
  assert (Mt : forall sp t, In sp r -> ResultMentions W v (d_mod d) sp t -> nlook t ([] ++ T1) <> None).
  { intros sp t Isp Rm. cbn [app]. unfold T1. destruct (in_dec Nat.eq_dec t outs1) as [It|Nt].
    - rewrite otm_out by exact It. discriminate.
    - rewrite otm_in by exact Nt.
      assert (Mn : Mentions W v t) by (exists d, ps, a, r, b, sp; repeat split; try assumption; intros Ft; exact (Nt (proj2 (O1 t) Ft))).
      pose proof (PublicFamilyAgree.mentions_dom W v th C (fun t => TAbs (g2 t)) Cr2 ks2 Cv needs Parts FCv t Mn) as It.
      apply (proj2 (PublicFamilyAgree.nlook_dom _ t (c_ty Cv))) in It. destruct (nlook t (c_ty Cv)); [discriminate|exfalso; exact (It eq_refl)]. }
  assert (Kg : incl (tm_gens ([] ++ T1)) K).
  { intros y Iy. cbn [app] in Iy. destruct (PublicFormation.tm_gens_in _ y Iy) as [x [w [Iq Iw]]].
    unfold T1, output_type_map in Iq. apply in_map_iff in Iq as [[x0 w0] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. pose proof (proj1 (O1o t) It) as Ito.
      unfold canon in Iw. cbn [app_subst] in Iw. rewrite (g1_sub t Ito) in Iw. destruct Iw as [<-|[]].
      unfold K. apply in_or_app. right. apply in_map. exact Ito.
    - unfold K. apply in_or_app. left. apply KgT. cbn [app]. apply (PublicFormation.in_tm_gens _ y x (canon Cv w0)); [|exact Iw].
      unfold PublicFamilyAgree.T1, output_type_map. cbn [map app]. apply in_map_iff. exists (x, w0). split; [reflexivity|exact Iq]. }
  pose proof (proj2 (family_entries_rel W s1 (c_sub C) v K) _ _ _ _ _ _ E1 _ _ _ _ E2 (Forall2_nil _) Ag Dm
    (fun sp t Isp Rm => Mt sp t Isp Rm) Kg) as Fr.
  pose proof (proj2 (PublicOutputSubst.output_entries_rel W s1 (c_sub C)) _ _ _ _ _ _ E1 _ _ _ _ E2 (Forall2_nil _) Ag) as Fe.
  pose proof (proj2 (PublicCallerKeys.same_kind_entries W) _ _ _ _ _ _ E1 _ _ _ _ E2) as Fk.
  assert (Fb : Forall2 (fun e1 e2 => PublicOutputSubst.EntryRel s1 (c_sub C) T1 T2 e1 e2 /\ EntryRelM W s1 (c_sub C) K e1 e2 /\
    PublicCallerKeys.SameKind e1 e2) es1 es2).
  { clear -Fr Fe Fk. revert es2 Fr Fe Fk. induction es1 as [|e1 r1 IH]; intros es2 Fr Fe Fk; inversion Fr; subst; inversion Fe; subst;
      inversion Fk; subst; constructor; [split; [assumption|split; assumption]|exact (IH _ ltac:(eassumption) ltac:(eassumption) ltac:(eassumption))]. }
  pose proof (proj2 (PublicScoping.output_entries_gens W (fun y => In y K)) _ _ _ _ _ _ E1 ltac:(intros g []) (fun y Iy => Kg y Iy)) as Ge.
  destruct (forall2_pairs _ _ _ _ _ Fb) as (L & -> & -> & FL).
  exists T1, T2, L. split; [exact Eo|]. split; [exact K1|]. split; [exact ECr|]. split; [exact K2|].
  pose proof (proj2 (fun_entries_fixed W) _ _ _ _ _ _ E1) as Cv1.
  pose proof (proj2 (fun_entries_fixed W) _ _ _ _ _ _ E2) as Cv2.
  rewrite Forall_forall in FL, Ge |- *. intros q Iq. destruct (FL q Iq) as (R1 & R2 & R3).
  split; [exact R1|]. split; [exact R2|]. split; [exact R3|]. split; [exact (Ge (fst q) (in_map fst _ _ Iq))|].
  intros u fx1 fx2 E1q E2q x t2 I2.
  assert (Fx : Fixed W u x).
  { apply (proj2 (Cv2 u fx2 ltac:(rewrite <- E2q; exact (in_map snd _ _ Iq)) x)). exists t2. exact I2. }
  exact (proj1 (Cv1 u fx1 ltac:(rewrite <- E1q; exact (in_map fst _ _ Iq)) x) Fx).
Qed.
End LevelRel.

(* A result read in a FunCtx Cu with roles gen mentions only generations of
   Cu's table readings and roles. *)
Theorem result_gens_out : forall W u0 Cu ns v gen Cr ks, A.FunCtx W u0 Cu ns ->
  A.ProvideResult W v Cu (fun t => TAbs (gen t)) Cr ks ->
  (forall t, A.FunOutputs W v t -> lookup_subst (gen t) (A.c_sub Cu) = None) ->
  forall k p, In k ks -> A.ilook (A.provision_id k) (A.c_provisions Cr) = Some p ->
  forall y, In y (A.provision_gens p) ->
    (exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx)) \/ (exists t, A.FunOutputs W v t /\ y = gen t).
Proof.
  intros W u0 Cu ns v gen Cr ks Fc PR GS k p Ik L.
  pose proof (PublicCallerKeys.result_key_id W v Cu _ Cr ks k PR Ik) as Id. rewrite Id in L.
  destruct (functx_scoped W u0 Cu ns Fc) as (TyA & _ & _ & _ & _ & _ & SubSc).
  destruct (PublicChainSigs.functx_mgm W u0 Cu ns Fc) as [Idm Fn].
  set (K := fun y => (exists x tx, A.nlook x (A.c_ty Cu) = Some tx /\ In y (A.gens tx)) \/ (exists t, A.FunOutputs W v t /\ y = gen t)).
  assert (KIn : forall x w y, In (x, w) (A.c_ty Cu) -> In y (A.gens w) -> K y).
  { intros x w y Iq Iy. left. destruct (nlook_some _ x (A.c_ty Cu) (in_map fst _ _ Iq)) as [w' Lw].
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
  set (news := flat_map (A.entry_provision (A.c_sub Cu) (fun a => a)) (rev es)).
  assert (En : A.c_provisions Cr = (news ++ A.c_provisions Cu)%list) by (rewrite EC; reflexivity).
  destruct (PublicCallerKeys.provide_result_key_news W v Cu _ Cr ks k PR Ik news En) as [p0 I0].
  destruct (PublicFamilyStep.ilook_of_in _ k news p0 I0) as [p' [L' I']].
  rewrite En, (PublicFamilyStep.ilook_app_some _ k news _ p' L') in L. injection L as <-.
  assert (Sc0 : PublicFormation.GensIn K (PublicTypeRead.tm_gens (A.output_type_map Cu outs (fun t => TAbs (gen t))))).
  { intros g Ig. destruct (PublicFormation.tm_gens_in _ g Ig) as [x [w [Iq Iw]]].
    unfold A.output_type_map in Iq. apply in_map_iff in Iq as [[y0 w0] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. pose proof (proj1 (Outs t) It) as Ft.
      unfold A.canon in Iw. cbn [app_subst] in Iw. rewrite (GS t Ft) in Iw. destruct Iw as [<-|[]].
      right. exists t. split; [exact Ft|reflexivity].
    - exact (TyK _ _ Iq g Iw). }
  pose proof (proj2 (PublicScoping.output_entries_gens W K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  unfold news in I'. apply in_flat_map in I' as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  exact (PublicScoping.entry_provision_gens K _ e k _ SubK (Es e Ie) Iq).
Qed.

Lemma nodup_map_inj : forall (f : nat -> nat) l a b, NoDup (map f l) -> In a l -> In b l -> f a = f b -> a = b.
Proof.
  intros f l. induction l as [|x r IH]; intros a b Nd Ia Ib E; [contradiction|]. inversion Nd as [|y z Nx Nr]; subst.
  destruct Ia as [<-|Ia]; destruct Ib as [<-|Ib]; try reflexivity.
  - exfalso. apply Nx. rewrite E. apply in_map. exact Ib.
  - exfalso. apply Nx. rewrite <- E. apply in_map. exact Ia.
  - exact (IH a b Nr Ia Ib E).
Qed.

(* ---- one provision read at two valuations ------------------------------------- *)

(* Two provisions of one kind whose types agree, the first read at T1 and
   the second at T2; a functor provision's second readings are among the
   first's. *)
Definition PEq (T1 : subst) (p1 : A.provision) (T2 : subst) (p2 : A.provision) : Prop :=
  match p1, p2 with
  | A.ProvidedVal t1 _, A.ProvidedVal t2 _ => app_subst T1 t1 = app_subst T2 t2
  | A.ProvidedFn g1 _, A.ProvidedFn g2 _ => A.sig_subst T1 g1 = A.sig_subst T2 g2
  | A.ProvidedMethod g1 _, A.ProvidedMethod g2 _ => A.sig_subst T1 g1 = A.sig_subst T2 g2
  | A.ProvidedFunctor fx1, A.ProvidedFunctor fx2 =>
      forall x t2, In (x, t2) fx2 -> exists t1, In (x, t1) fx1 /\ app_subst T1 t1 = app_subst T2 t2
  | _, _ => False
  end.

(* Two keys, a method's receivers agreeing at T1 and T2. *)
Definition KeyRel (T1 T2 : subst) (k1 k2 : item) : Prop :=
  (forall r m a, k1 = IMth r m a -> exists r' a', k2 = IMth r' m a' /\ app_subst T1 r = app_subst T2 r') /\
  (forall u, k1 = IFun u -> k2 = IFun u).

Lemma peq_of_subst : forall T p p', A.provision_subst T p = A.provision_subst T p' -> PEq T p T p'.
Proof.
  intros T [t a|g a|g a|fx] [t' a'|g' a'|g' a'|fx'] E; cbn in E; try discriminate; cbn.
  - exact (f_equal (fun p => match p with A.ProvidedVal t _ => t | _ => TUnit end) E).
  - exact (f_equal (fun p => match p with A.ProvidedFn g _ => g | _ => A.FSig [] TUnit end) E).
  - exact (f_equal (fun p => match p with A.ProvidedMethod g _ => g | _ => A.FSig [] TUnit end) E).
  - pose proof (f_equal (fun p => match p with A.ProvidedFunctor fx => fx | _ => [] end) E) as H. cbv beta iota in H.
    intros x t2 I. assert (I' : In (x, app_subst T t2) (map (fun q => (fst q, app_subst T (snd q))) fx'))
      by (apply in_map_iff; exists (x, t2); split; [reflexivity|exact I]).
    rewrite <- H in I'. apply in_map_iff in I' as [[x1 t1] [Eq I1]]. injection Eq as -> Et.
    exists t1. split; [exact I1|exact Et].
Qed.

Lemma peq_trans : forall T1 p1 T2 p2 T3 p3, PEq T1 p1 T2 p2 -> PEq T2 p2 T3 p3 -> PEq T1 p1 T3 p3.
Proof.
  intros T1 [t1 a1|g1 a1|g1 a1|fx1] T2 [t2 a2|g2 a2|g2 a2|fx2] T3 [t3 a3|g3 a3|g3 a3|fx3] E1 E2; cbn in *;
    try contradiction; try congruence.
  intros x t I. destruct (E2 x t I) as [t' [I' Et']]. destruct (E1 x t' I') as [t'' [I'' Et'']].
  exists t''. split; [exact I''|congruence].
Qed.

Lemma keyrel_trans : forall T1 T2 T3 k1 k2 k3, KeyRel T1 T2 k1 k2 -> KeyRel T2 T3 k2 k3 -> KeyRel T1 T3 k1 k3.
Proof.
  intros T1 T2 T3 k1 k2 k3 [M1 F1] [M2 F2]. split.
  - intros r m a E. destruct (M1 r m a E) as [r' [a' [E' R']]]. destruct (M2 r' m a' E') as [r'' [a'' [E'' R'']]].
    exists r'', a''. split; [exact E''|congruence].
  - intros u E. exact (F2 u (F1 u E)).
Qed.

Lemma keyrel_id : forall T k k', A.provision_id k = A.provision_id k' -> KeyRel T T k k'.
Proof.
  intros T [t|v|f|r m a|u] [t'|v'|f'|r' m' a'|u'] E; cbn in E; try discriminate; split; intros; try discriminate.
  - injection H as -> -> ->. injection E as -> ->. exists r', a'. split; reflexivity.
  - injection E as ->. exact H.
Qed.

Section EntryPeq.
Variable W : A.world.
Variables (s1 s2 sg1 T1 T2 : subst) (tm1 tm2 : list (nat * ty)) (K : list nat).
Hypothesis Inv1 : forall t, app_subst T1 (app_subst sg1 t) = app_subst T1 t.
Hypothesis Inv2 : forall t, app_subst T2 (app_subst s2 t) = app_subst T2 t.
Hypothesis OnK : forall x, In x K -> app_subst T2 (app_subst s1 (TAbs x)) = app_subst T1 (TAbs x).

Lemma one_ty : forall t1 t2, app_subst s1 t1 = app_subst s2 t2 -> incl (A.gens t1) K ->
  app_subst T1 (app_subst sg1 t1) = app_subst T2 (app_subst s2 t2).
Proof.
  intros t1 t2 E Inc. rewrite Inv1, Inv2, <- (Inv2 t2), <- E. symmetry. apply PublicCopyFresh.ty_agree. intros x Ix. exact (OnK x (Inc x Ix)).
Qed.

Lemma one_tys : forall l1 l2, map (app_subst s1) l1 = map (app_subst s2) l2 -> Forall (fun t => incl (A.gens t) K) l1 ->
  map (app_subst T1) (map (app_subst sg1) l1) = map (app_subst T2) (map (app_subst s2) l2).
Proof.
  induction l1 as [|t1 r1 IH]; intros [|t2 r2] E F; cbn in E; try discriminate; [reflexivity|].
  injection E as E1 E2. inversion F; subst. cbn. rewrite (one_ty t1 t2 E1), (IH r2 E2); [reflexivity|assumption|assumption].
Qed.

Lemma one_sig : forall g1 g2, PublicOutputSubst.SigRel s1 s2 g1 g2 -> PublicFormation.SigP (PublicScoping.TyIn (fun y => In y K)) g1 ->
  A.sig_subst T1 (A.sig_subst sg1 g1) = A.sig_subst T2 (A.sig_subst s2 g2).
Proof.
  intros [ps1 rt1] [ps2 rt2] [Ep Er] [Fp Fr]. unfold A.sig_subst. cbn in *. f_equal.
  - apply one_tys; [exact Ep|]. rewrite Forall_forall in Fp |- *. intros t It y Iy. exact (Fp t It y Iy).
  - apply one_ty; [exact Er|intros y Iy; exact (Fr y Iy)].
Qed.

Lemma entry_peq : forall e1 e2 k1 q1 k2 q2,
  PublicOutputSubst.EntryRel s1 s2 tm1 tm2 e1 e2 -> EntryRelM W s1 s2 K e1 e2 ->
  PublicFormation.EntryP (PublicScoping.TyIn (fun y => In y K)) e1 ->
  A.entry_provision sg1 (fun a => a) e1 = [(k1, q1)] -> A.entry_provision s2 (fun a => a) e2 = [(k2, q2)] ->
  (forall u fx1 fx2, e1 = A.NFun u fx1 -> e2 = A.NFun u fx2 -> forall x t2, In (x, t2) fx2 -> exists t1, In (x, t1) fx1) ->
  PEq T1 q1 T2 q2 /\ KeyRel T1 T2 k1 k2.
Proof.
  intros [t|v1 t1 a1|f1 g1 a1|r1 m1 g1 a1|u1 fx1] [t'|v2 t2 a2|f2 g2 a2|r2 m2 g2 a2|u2 fx2] k1 q1 k2 q2 Rel RelM Ep P1 P2 Cov;
    cbn in Rel, RelM, P1, P2; try contradiction; try discriminate;
    injection P1 as <- <-; injection P2 as <- <-.
  - destruct Rel as (<- & Rt & _). destruct Ep as [Et _]. split; [cbn; exact (one_ty t1 t2 Rt (fun y Iy => Et y Iy))|].
    split; intros; discriminate.
  - destruct Rel as (<- & Rg & _). destruct Ep as [Eg _]. split; [cbn; exact (one_sig g1 g2 Rg Eg)|].
    split; intros; discriminate.
  - destruct RelM as (<- & Rr & Rg & _ & _). destruct Ep as (Er & Eg & _). split; [cbn; exact (one_sig g1 g2 Rg Eg)|].
    split; [|intros; discriminate]. intros r m a E. unfold A.mkey in E. injection E as <- <- <-.
    exists (app_subst s2 r2), []. split; [reflexivity|]. exact (one_ty r1 r2 Rr (fun y Iy => Er y Iy)).
  - destruct Rel as (<- & Rf). split.
    + cbn. intros x t I. apply in_map_iff in I as [[x2 t2] [E I2]]. injection E as -> <-.
      destruct (Cov u1 fx1 fx2 eq_refl eq_refl x t2 I2) as [t1 I1].
      exists (app_subst sg1 t1). split; [apply in_map_iff; exists (x, t1); split; [reflexivity|exact I1]|].
      apply one_ty; [exact (Rf x t1 t2 I1 I2)|intros y Iy; exact (Ep x t1 I1 y Iy)].
    + split; [intros; discriminate|]. intros u E. injection E as <-. reflexivity.
Qed.
End EntryPeq.

(* ---- valuations with roles -------------------------------------------------- *)

Lemma lookup_map_none : forall (g : nat -> nat) (M : nat -> ty) outs z, ~ In z (map g outs) ->
  lookup_subst z (map (fun t => (g t, M t)) outs) = None.
Proof.
  intros g M outs z. induction outs as [|t r IH]; intros N; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec z (g t)) as [E|Ne]; [exfalso; apply N; left; symmetry; exact E|].
  apply IH. intros I. apply N. right. exact I.
Qed.

Lemma tv_off : forall (g : nat -> nat) (M : nat -> ty) outs R t, (forall z, In z (A.gens t) -> ~ In z (map g outs)) ->
  app_subst (map (fun t => (g t, M t)) outs ++ R) t = app_subst R t.
Proof.
  intros g M outs R t N. apply subst_agree_on. intros z Iz. cbn [app_subst].
  rewrite PublicCopyFresh.lookup_subst_cat, (lookup_map_none g M outs z (N z Iz)). reflexivity.
Qed.

Lemma tv_role : forall (g : nat -> nat) (M : nat -> ty) outs R t, In t outs ->
  (forall a b, In a outs -> In b outs -> g a = g b -> a = b) ->
  app_subst (map (fun t => (g t, M t)) outs ++ R) (TAbs (g t)) = M t.
Proof.
  intros g M outs R t It Inj. cbn [app_subst]. rewrite PublicCopyFresh.lookup_subst_cat, (lookup_roles g M outs t It Inj). reflexivity.
Qed.

Lemma nth_map_error : forall X Y (f : X -> Y) l i x d, nth_error l i = Some x -> nth i (map f l) d = f x.
Proof.
  intros X Y f l i x d E. apply nth_error_nth. rewrite nth_error_map, E. reflexivity.
Qed.

Section Roles.
Variable W : A.world.
Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

Lemma fresh_off_dom : forall v Cv ns (g : nat -> nat) outs, A.FunCtx W v Cv ns -> (forall t, In t outs -> A.Fresh W (g t)) ->
  forall x, In x (map fst (A.c_ty Cv)) -> ~ In x (map g outs).
Proof.
  intros v Cv ns g outs Fc Fr x Ix I. apply in_map_iff in I as [t [<- It]].
  exact (PublicCopyFresh.functx_keys_declared W merge_formed mentions_typed v Cv ns Fc (g t) Ix (Fr t It)).
Qed.

Lemma fresh_not_sub : forall v Cv ns z, A.FunCtx W v Cv ns -> A.Fresh W z -> lookup_subst z (A.c_sub Cv) = None.
Proof.
  intros v Cv ns z Fc Fz. destruct (lookup_subst z (A.c_sub Cv)) as [w|] eqn:L; [|reflexivity]. exfalso.
  destruct (functx_scoped W v Cv ns Fc) as (_ & _ & _ & _ & _ & _ & SubSc).
  exact (PublicCopyFresh.functx_keys_declared W merge_formed mentions_typed v Cv ns Fc z (proj1 (SubSc z w (lookup_subst_in_pair _ _ _ L))) Fz).
Qed.

Lemma tableok_roles : forall v Cv ns (g : nat -> nat) (M : nat -> ty) outs R, A.FunCtx W v Cv ns ->
  (forall t, In t outs -> A.Fresh W (g t)) -> PublicChainSigs.TableOk R Cv ->
  PublicChainSigs.TableOk (map (fun t => (g t, M t)) outs ++ R) Cv.
Proof.
  intros v Cv ns g M outs R Fc Fr Ok x tx L.
  rewrite !tv_off; [exact (Ok x tx L)| |].
  - intros z [<-|[]]. apply (fresh_off_dom v Cv ns g outs Fc Fr). apply PublicFamilyAgree.nlook_dom. rewrite L. discriminate.
  - intros z Iz. apply (fresh_off_dom v Cv ns g outs Fc Fr). exact (PublicChainSigs.table_gens_dom W v Cv ns x tx z Fc L Iz).
Qed.
End Roles.

(* The transfer condition for pos_last_all: two pairs with one first-side
   key have one second-side key, when s1 absorbs the first side's merges. *)
Lemma tr_all : forall W (s1 s2 sg1 : subst) K tm1 tm2 (L : list (A.entry * A.entry)),
  (forall q, In q L -> PublicOutputSubst.EntryRel s1 s2 tm1 tm2 (fst q) (snd q) /\ EntryRelM W s1 s2 K (fst q) (snd q) /\
     PublicCallerKeys.SameKind (fst q) (snd q)) ->
  (forall t, app_subst s1 (app_subst sg1 t) = app_subst s1 t) ->
  forall p p' k, In p L -> In p' L -> A.entry_key s2 (snd p) = Some k ->
    A.entry_key sg1 (fst p) = A.entry_key sg1 (fst p') -> A.entry_key s2 (snd p') = Some k.
Proof.
  intros W s1 s2 sg1 K tm1 tm2 L FL Inv p p' k Ip Ip' K2 E1.
  destruct (FL p Ip) as (R & RM & Sk). destruct (FL p' Ip') as (R' & RM' & Sk').
  destruct p as [e1 e2], p' as [e1' e2']. cbn [fst snd] in *.
  destruct e1 as [?|?|?|r1 m1 g1 a1|?], e2 as [?|?|?|r2 m2 g2 a2|?]; cbn in Sk; try contradiction;
    destruct e1' as [?|?|?|r1' m1' g1' a1'|?], e2' as [?|?|?|r2' m2' g2' a2'|?]; cbn in Sk'; try contradiction;
    cbn in K2, E1, R, R', RM, RM' |- *; try discriminate.
  - injection K2 as <-. injection E1 as <-. destruct R as (<- & _). destruct R' as (<- & _). reflexivity.
  - injection K2 as <-. injection E1 as <-. destruct R as (<- & _). destruct R' as (<- & _). reflexivity.
  - rewrite <- K2. unfold A.mkey in E1 |- *. injection E1 as Er Em. destruct RM as (<- & Rr & _). destruct RM' as (<- & Rr' & _).
    subst. unfold PublicOutputSubst.TyRel in Rr, Rr'. rewrite <- Rr, <- Rr', <- (Inv r1), <- (Inv r1'), Er. reflexivity.
  - injection K2 as <-. injection E1 as <-. destruct R as (<- & _). destruct R' as (<- & _). reflexivity.
Qed.

Module ChainRealize (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses (CSg, and through it FL, CF and CN), so
   the obligations match. *)
Module CSg := PublicChainSigs.ChainSigs Prims I0.
Module FL := CSg.FL.
Module CF := FL.CF.
Module CN := CF.CN.
Module CC := CN.CC.
Module LR := CC.LR.
Module CA := CC.CA.
Module S := CC.S.
Module LRn := FL.LRn.

Section Real.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.
Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.
Variables (n : nat) (H3 : heap_types).

(* A closure callable at the types ps -> rt read at T. *)
Definition CallAt (T : subst) (ps : list ty) (rt : ty) (pv : provision_value) : Prop :=
  exists e ps' rt', S.CallableOk W Prog argv read state_ok n H3 e ps' rt' pv /\
    map (ground e) ps' = map (app_subst T) ps /\ ground e rt' = app_subst T rt.

(* pv realizes the provision p filed under k, its types read at T. *)
Definition RealAt (T : subst) (k : item) (p : A.provision) (pv : provision_value) : Prop :=
  match p with
  | A.ProvidedVal t _ => exists w, pv = ValueProvision w /\ value_typed W P H3 w (app_subst T t)
  | A.ProvidedFn g _ => CallAt T (A.sg_params g) (A.sg_ret g) pv
  | A.ProvidedMethod g _ => exists r m a, k = IMth r m a /\ CallAt T (r :: A.sg_params g) (A.sg_ret g) pv
  | A.ProvidedFunctor fx => exists u gfx, k = IFun u /\ S.ClosureOk W Prog argv read state_ok n H3 u gfx pv /\
      forall x tx, In (x, tx) fx -> lookup_subst x gfx = Some (app_subst T tx)
  end.

Lemma sig_parts : forall T T' g g', A.sig_subst T g = A.sig_subst T' g' ->
  map (app_subst T) (A.sg_params g) = map (app_subst T') (A.sg_params g') /\ app_subst T (A.sg_ret g) = app_subst T' (A.sg_ret g').
Proof. intros T T' [ps rt] [ps' rt'] E. unfold A.sig_subst in E. cbn in *. injection E as E1 E2. split; assumption. Qed.

Lemma real_transfer : forall T k p T' k' p' pv, RealAt T k p pv -> PEq T p T' p' -> KeyRel T T' k k' -> RealAt T' k' p' pv.
Proof.
  intros T k [t a|g a|g a|fx] T' k' [t' a'|g' a'|g' a'|fx'] pv R E [KM KF]; cbn in R, E |- *; try contradiction.
  - destruct R as [w [-> Tw]]. exists w. split; [reflexivity|]. rewrite <- E. exact Tw.
  - destruct R as (e & ps & rt & Ok & Ep & Er). destruct (sig_parts _ _ _ _ E) as [Sp Sr].
    exists e, ps, rt. split; [exact Ok|]. split; [rewrite Ep; exact Sp|rewrite Er; exact Sr].
  - destruct R as (r & m & a0 & -> & e & ps & rt & Ok & Ep & Er). destruct (sig_parts _ _ _ _ E) as [Sp Sr].
    destruct (KM r m a0 eq_refl) as (r' & a1 & -> & Rr). exists r', m, a1. split; [reflexivity|].
    exists e, ps, rt. split; [exact Ok|]. split; [rewrite Ep; cbn [map]; rewrite Rr, Sp; reflexivity|rewrite Er; exact Sr].
  - destruct R as (u & gfx & -> & Ok & Fx). exists u, gfx. split; [exact (KF u eq_refl)|]. split; [exact Ok|].
    intros x tx' I. destruct (E x tx' I) as [tx [I0 Et]]. rewrite (Fx x tx I0), Et. reflexivity.
Qed.

Section Chain.
Variables (h0 : nat) (Ch0 : A.cx) (ns0 : list item) (srcs : list item) (d3 : environment) (M : nat -> ty) (Tin : subst).
Hypothesis FC0 : A.FunCtx W h0 Ch0 ns0.
Hypothesis Abs0 : CSg.Abstract W h0.

(* A level's valuation: its roles at the outputs' meanings, its inputs at Tin. *)
Definition Tv (g : nat -> nat) (outs : list nat) : subst := (map (fun t => (g t, M t)) outs ++ Tin)%list.

(* Every position of v's own result (read at Cv with some fresh roles g)
   has a source (through the chain's positions rmsv) whose value realizes
   the position's provision at v's valuation. *)
Definition LevelReal (v : nat) (Cv : A.cx) (rmsv : list (list nat)) : Prop :=
  exists g outs Crv ksv, (forall t, In t outs <-> A.FunOutputs W v t) /\ (forall t, In t outs -> A.Fresh W (g t)) /\
    (forall a b, In a outs -> In b outs -> g a = g b -> a = b) /\
    A.ProvideResult W v Cv (fun t => TAbs (g t)) Crv ksv /\
    forall i k, nth_error ksv i = Some k -> exists p pv,
      A.ilook (A.provision_id k) (A.c_provisions Crv) = Some p /\
      lookup_provision (nth i (chain_sources rmsv srcs) (ITy 0)) d3 = Some pv /\ RealAt (Tv g outs) k p pv.

Hypothesis Root : LevelReal h0 Ch0 [].

Lemma chain_real : forall v gfv sms rmsv h0' gf0 sms0 rms0, CA.CopyChain W v gfv sms rmsv h0' gf0 sms0 rms0 ->
  h0' = h0 -> rms0 = [] -> forall Cv nsv, A.FunCtx W v Cv nsv -> PublicChainSigs.TableOk Tin Cv ->
  (forall x, A.Fixed W v x -> lookup_subst x gfv = Some (app_subst Tin (TAbs x))) -> LevelReal v Cv rmsv.
Proof.
  intros v gfv sms rmsv h0' gf0 sms0 rms0 Ch.
  induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh gfx gfh sms rms rm h0' gf0 sms0 rms0 Sub IH FCu FCh Incl FixH SameN SameT _ InCu Same Site];
    intros E0 Er Cv nsv FCv Tok FixT.
  - subst u rms. destruct (S.functx_unique W _ _ _ _ _ FCv FC0) as [-> _]. exact Root.
  - subst h0' rms0.
    destruct (S.functx_unique W _ _ _ _ _ FCv FCu) as [Ecv _]. subst Cv.
    pose proof (PublicChainSigs.sub_inv W u Cu nsu Tin FCu Tok) as InvU.
    assert (TokH : PublicChainSigs.TableOk Tin Ch).
    { intros x tx Lx. pose proof (SameT x tx Lx) as E. unfold A.canon in E.
      rewrite <- (InvU tx), <- E, InvU. reflexivity. }
    assert (FixTh : forall x, A.Fixed W h x -> lookup_subst x gfh = Some (app_subst Tin (TAbs x))).
    { intros x Fh. destruct (FixH x Fh) as [Fu E]. rewrite E. exact (FixT x Fu). }
    destruct (IH eq_refl eq_refl Ch nsh FCh TokH FixTh) as (gh & outsh & Crh & ksh & Oh & Frh & Injh & PRh & RealH).
    destruct Site as (C1 & fx1 & fxh & d1 & _ & Ab1 & _ & Fx1 & CO1 & Egfx & _ & _ & _ & _ & AbU).
    set (sigma := map (fun q => (fst q, A.canon C1 (snd q))) fx1) in CO1.
    destruct CO1 as (outs & gen & ys & cg & Cr & ks & Cr' & ks' & Outs & Nd & Avoid & FrG & FrC & Ys & PRu & PRh' & Eq & Lrm & Pos).
    assert (InjG : forall a b, In a outs -> In b outs -> gen a = gen b -> a = b)
      by (intros a b Ia Ib; exact (nodup_map_inj gen outs a b (NoDup_app_remove_r _ _ Nd) Ia Ib)).
    assert (HU : forall t, A.FunOutputs W h t -> In t outs) by (intros t Ft; apply Outs; exact (proj2 (S.same_result_outputs W u h Same t) Ft)).
    set (Tu := Tv gen outs).
    set (Th := Tv gh outsh).
    exists gen, outs, Cr, ks. split; [exact Outs|]. split; [exact FrG|]. split; [exact InjG|]. split; [exact PRu|].
    intros i k Ki.
    destruct (Pos i k Ki) as (j & k' & Rj & Kj & Idk & Lastj).
    destruct (Eq k (nth_error_In _ _ Ki)) as (p & p' & Lp & Lp' & Ep).
    (* the copy step at Tu *)
    set (rho0 := (map (fun t => (gen t, M t)) outs ++ type_meanings d1)%list).
    assert (FunFx : forall x t t', In (x, t) fx1 -> In (x, t') fx1 -> t = t').
    { intros x t t' I I'. destruct (proj1 (Fx1 x t) I) as [_ E]. destruct (proj1 (Fx1 x t') I') as [_ E']. congruence. }
    assert (GS : forall v0 t, (A.FunOutputs W v0 t -> In t outs) -> A.FunOutputs W v0 t -> lookup_subst (gen t) (A.c_sub Cu) = None)
      by (intros v0 t VO Ft; exact (fresh_not_sub W merge_formed mentions_typed u Cu nsu (gen t) FCu (FrG t (VO Ft)))).
    assert (Gen : forall v0 Crx ksx k0 q, A.ProvideResult W v0 Cu (fun t => TAbs (gen t)) Crx ksx -> (forall t, A.FunOutputs W v0 t -> In t outs) ->
      In k0 ksx -> A.ilook (A.provision_id k0) (A.c_provisions Crx) = Some q ->
      forall x, In x (A.provision_gens q) -> PublicCopyFresh.CopyGen sigma ys cg Tu rho0 x).
    { intros v0 Crx ksx k0 q PRx VO Ik0 Lq x Ix. split.
      - intros s0 Ls. pose proof (lookup_subst_in_pair _ _ _ Ls) as Is.
        assert (Av : forall z, In z (A.gens s0) -> ~ In z (map gen outs ++ map cg ys)).
        { intros z Iz. apply Avoid. apply in_flat_map. exists (x, s0). split; [exact Is|exact Iz]. }
        split; [intros z Iz Ic; exact (Av z Iz (in_or_app _ _ _ (or_intror Ic)))|].
        unfold sigma in Is. apply in_map_iff in Is as [[x' tx] [E Itx]]. cbn in E. injection E as -> <-.
        destruct (proj1 (Fx1 x tx) Itx) as [Fu _].
        pose proof (FixT x Fu) as Fg. rewrite Egfx, (S.lookup_ground_fixed _ fx1 x tx FunFx Itx) in Fg. injection Fg as Fg.
        unfold rho0. rewrite tv_off by (intros z Iz Ig; exact (Av z Iz (in_or_app _ _ _ (or_introl Ig)))).
        change (ground d1 (A.canon C1 tx) = app_subst Tu (TAbs x)). rewrite (Ab1 tx). unfold ground. rewrite Fg.
        unfold Tu, Tv. rewrite tv_off; [reflexivity|]. intros z [<-|[]].
        apply (fresh_off_dom W merge_formed mentions_typed u Cu nsu gen outs FCu FrG).
        exact (S.fixed_in_functx W u Cu nsu x AbU FCu Fu).
      - intros Ls Ny. destruct (result_gens_out W u Cu nsu v0 gen Crx ksx FCu PRx (fun t Ft => GS v0 t (VO t) Ft) k0 q Ik0 Lq x Ix)
          as [Kx|[t [Ft ->]]].
        + exfalso. apply Ny. apply Ys. split; [exact Kx|exact Ls].
        + pose proof (VO t Ft) as It. split; [exact (PublicCopyFresh.nodup_app_disj _ _ _ Nd (in_map gen _ _ It))|].
          unfold rho0, Tu, Tv. rewrite !tv_role by assumption. reflexivity. }
    assert (NdC : NoDup (map cg ys)) by exact (NoDup_app_remove_l _ _ Nd).
    pose proof (PublicCopyFresh.copy_outputs_at sigma ys cg Tu rho0 NdC _ p' Ep
      (Gen u Cr ks k p PRu (fun t F => proj2 (Outs t) F) (nth_error_In _ _ Ki) Lp)
      (Gen h Cr' ks' k' p' PRh' HU (nth_error_In _ _ Kj) ltac:(rewrite Idk; exact Lp'))) as Ept.
    (* h's result at Ch and at Cu *)
    destruct (PublicChainSigs.functx_mgm W u Cu nsu FCu) as [IdmU _].
    destruct (PublicChainSigs.functx_mgm W h Ch nsh FCh) as [_ FnH].
    destruct (functx_scoped W h Ch nsh FCh) as (TyAH & _ & _ & _ & _ & _ & SubScH).
    set (thh := map (fun q => (fst q, A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q))))) (A.c_ty Ch)).
    assert (PartsH : thh = map (fun q => (fst q, A.canon Cu (app_subst (A.c_ty Cu) (TAbs (fst q))))) (A.c_ty Ch) /\
      (forall t, In t (map fst (A.c_ty Ch)) -> A.nlook t (A.c_ty Cu) <> None) /\
      (forall g0 t, In (g0, t) (A.c_sub Ch) -> (forall x, In x (g0 :: A.gens t) -> A.nlook x (A.c_ty Cu) <> None) ->
         A.canon Cu (app_subst (A.c_ty Cu) (TAbs g0)) = A.canon Cu (app_subst (A.c_ty Cu) t)) /\
      (forall d ps a r b, A.w_decl W h = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
      A.ProvideResult W h Cu (fun t => TAbs (gen t)) Cr' ks').
    { split; [reflexivity|]. split.
      { intros t It. destruct (nlook_some _ t _ It) as [w Lw]. exact (InCu t w Lw). }
      split.
      { intros g0 t Iq _. rewrite !(PublicChainSigs.functx_ty_sub W u Cu nsu FCu). unfold A.canon. rewrite !IdmU.
        destruct (SubScH g0 t Iq) as [Ig _]. destruct (nlook_some _ g0 _ Ig) as [w Lw].
        pose proof (SameT g0 w Lw) as E. rewrite (TyAH g0 w Lw) in E. cbn [app_subst] in E. rewrite (FnH g0 t Iq) in E.
        exact E. }
      split; [exact (CSg.chain_abs W h gfh sms rms h0 gf0 sms0 [] Sub Abs0)|exact PRh']. }
    assert (KTh : forall q pa x, A.resolve_sym W q pa = Some x -> A.nlook x (A.c_ty Cu) <> None ->
      A.nlook x (A.c_ty Ch) = None -> A.kind_sym W x = Some A.DKType).
    { intros q pa x _ N _. destruct (PublicFormation.functx_cx_formed W merge_formed mentions_typed u Cu nsu FCu) as [_ [_ Ty]].
      apply Ty. apply PublicFamilyAgree.nlook_dom. exact N. }
    assert (G1f : forall t, In t outsh -> ~ In (gh t) (map fst (A.c_ty Ch)))
      by (intros t It I; exact (PublicCopyFresh.functx_keys_declared W merge_formed mentions_typed h Ch nsh FCh (gh t) I (Frh t It))).
    assert (G2s : forall t, In t outsh -> lookup_subst (gen t) (A.c_sub Cu) = None)
      by (intros t It; exact (fresh_not_sub W merge_formed mentions_typed u Cu nsu (gen t) FCu (FrG t (HU t (proj1 (Oh t) It))))).
    destruct (level_rel W Cu h thh gh gen outsh Ch nsh Crh ksh Cr' ks' PartsH FCh PRh (fun t => IdmU t) KTh Oh G1f Injh G2s)
      as (tm1 & tm2 & L & ECrh & EKh & ECr' & EK' & FL).
    set (s1r := (thh ++ map (fun t => (gh t, TAbs (gen t))) outsh)%list) in FL.
    assert (ThKeys : map fst thh = map fst (A.c_ty Ch)) by (unfold thh; rewrite map_map; reflexivity).
    pose proof (PublicFamilyAgree.th_sigma W Cu h thh Cu (fun t => TAbs (gen t)) Cr' ks' Ch nsh eq_refl PartsH FCh) as ThS.
    assert (S1Inv : forall t, app_subst s1r (app_subst (A.c_sub Ch) t) = app_subst s1r t).
    { intros t. apply PublicCopyFresh.ty_agree. intros x _. rewrite (PublicChainSigs.app_subst_abs (A.c_sub Ch) x).
      destruct (lookup_subst x (A.c_sub Ch)) as [w|] eqn:Lw; [|reflexivity].
      destruct (SubScH x w (lookup_subst_in_pair _ _ _ Lw)) as [Ix Iw].
      unfold s1r. rewrite app_subst_app_dom by (rewrite ThKeys; exact Iw).
      rewrite (app_subst_app_dom thh _ (TAbs x)) by (rewrite ThKeys; intros y [<-|[]]; exact Ix).
      pose proof (ThS (TAbs x)) as E. cbn [app_subst] in E. rewrite Lw in E. exact E. }
    assert (FLa : forall q, In q L -> PublicOutputSubst.EntryRel s1r (A.c_sub Cu) tm1 tm2 (fst q) (snd q) /\
      EntryRelM W s1r (A.c_sub Cu) (map fst (A.c_ty Ch) ++ map gh outsh) (fst q) (snd q) /\ PublicCallerKeys.SameKind (fst q) (snd q)).
    { rewrite Forall_forall in FL. intros q Iq. destruct (FL q Iq) as (R1 & R2 & R3 & _). exact (conj R1 (conj R2 R3)). }
    assert (Nj : nth_error (keys_of (fun p => A.entry_key (A.c_sub Cu) (snd p)) L) j = Some k').
    { rewrite <- keys_of_map, <- EK'. exact Kj. }
    assert (Lj : forall j' k'', j < j' -> nth_error (keys_of (fun p => A.entry_key (A.c_sub Cu) (snd p)) L) j' = Some k'' -> k'' <> k').
    { intros j' k'' Lt N E. subst k''. rewrite <- keys_of_map, <- EK' in N. exact (Lastj j' _ Lt N Idk). }
    destruct (pos_last_all (A.c_sub Ch) (A.c_sub Cu) L
      (fun q Iq => PublicChainSigs.same_kind_none _ _ _ _ (proj2 (proj2 (FLa q Iq))))
      (tr_all W s1r (A.c_sub Cu) (A.c_sub Ch) _ tm1 tm2 L FLa S1Inv) j k' Nj Lj)
      as (pp & k1 & q1 & q2 & Ipp & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
    assert (Idk' : A.provision_id k' = k') by exact (PublicCallerKeys.result_key_id W h Cu _ Cr' ks' k' PRh' (nth_error_In _ _ Kj)).
    assert (Eq2 : q2 = p').
    { pose proof (I2 (A.c_provisions Cu)) as I2'. rewrite ECr' in Lp'. cbn [A.output_cx A.c_provisions] in Lp'.
      rewrite <- Idk, Idk' in Lp'. congruence. }
    subst q2.
    assert (Nh : nth_error ksh j = Some k1) by (rewrite EKh, keys_of_map; exact N1).
    destruct (RealH j k1 Nh) as (ph & pv & Lph & Spv & Rh).
    assert (Eph : ph = q1).
    { rewrite (PublicCallerKeys.result_key_id W h Ch _ Crh ksh k1 PRh (nth_error_In _ _ Nh)) in Lph.
      pose proof (I1 (A.c_provisions Ch)) as I1'. rewrite ECrh in Lph. cbn [A.output_cx A.c_provisions] in Lph. congruence. }
    subst ph.
    (* the two valuations along h's two readings *)
    assert (TokU' : PublicChainSigs.TableOk Tu Cu) by exact (tableok_roles W merge_formed mentions_typed u Cu nsu gen M outs Tin FCu FrG Tok).
    assert (TokH' : PublicChainSigs.TableOk Th Ch) by exact (tableok_roles W merge_formed mentions_typed h Ch nsh gh M outsh Tin FCh Frh TokH).
    assert (OnK : forall x, In x (map fst (A.c_ty Ch) ++ map gh outsh) -> app_subst Tu (app_subst s1r (TAbs x)) = app_subst Th (TAbs x)).
    { intros x Ix. apply in_app_or in Ix as [Ix|Ix].
      - unfold s1r. rewrite (app_subst_app_dom thh _ (TAbs x)) by (rewrite ThKeys; intros y [<-|[]]; exact Ix).
        rewrite (PublicChainSigs.th_inv W u Cu nsu Tu (A.c_ty Ch) FCu TokU').
        unfold Tu, Th, Tv. rewrite !tv_off; [reflexivity| |].
        + intros z [<-|[]]. exact (fresh_off_dom W merge_formed mentions_typed h Ch nsh gh outsh FCh Frh x Ix).
        + intros z [<-|[]]. apply (fresh_off_dom W merge_formed mentions_typed u Cu nsu gen outs FCu FrG x).
          destruct (nlook_some _ x _ Ix) as [w Lw]. apply PublicFamilyAgree.nlook_dom. exact (InCu x w Lw).
      - apply in_map_iff in Ix as [t [<- It]]. unfold s1r.
        rewrite (s1_role W Cu h thh gh gen outsh Ch Cr' ks' PartsH G1f Injh t It).
        unfold Tu, Th, Tv. rewrite !tv_role; [reflexivity|exact It|exact Injh|exact (HU t (proj1 (Oh t) It))|exact InjG]. }
    rewrite Forall_forall in FL. destruct (FL pp Ipp) as (R1 & R2 & _ & R4 & R5).
    destruct (entry_peq W s1r (A.c_sub Cu) (A.c_sub Ch) Th Tu tm1 tm2 _
      (PublicChainSigs.sub_inv W h Ch nsh Th FCh TokH') (PublicChainSigs.sub_inv W u Cu nsu Tu FCu TokU') OnK
      (fst pp) (snd pp) k1 q1 k' p' R1 R2 R4 P1 P2 R5) as [PE KR].
    exists p, pv. split; [exact Lp|]. split.
    + rewrite PublicCallerKeys.chain_sources_snoc, (nth_map_error _ _ _ rm i j _ Rj). exact Spv.
    + apply (real_transfer Tu k' p' Tu k p pv (real_transfer Th k1 q1 Tu k' p' pv Rh PE KR)).
      * exact (peq_of_subst Tu p' p (eq_sym Ept)).
      * exact (keyrel_id Tu k' k Idk).
Qed.
End Chain.
End Real.
End ChainRealize.
