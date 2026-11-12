(* Applying a copy (T15): a functor bound to a copy of another runs its
   source's code at the source's interface, reached through the copy
   chain. This discharges PublicBodySlice's CopyApplyOk from the body
   run and two obligations about a closure's root, stated
   for a whole copy chain: what a completed linked body provides at the
   application (ChainResultsOk, LinkResultsOk read through copies) and
   what applying a platform family through copies does (ChainPlatformOk,
   PlatformApplyOk read through copies). LinkResultsOk and
   PlatformApplyOk are their chains of length zero. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import AuditBase.
From NM Require Import LegacyBodyJudgments.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBoolTyping PublicBodySafety PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts
  PublicCallRealize PublicBodySlice.
From NM Require AllocationFacts PublicFormation PublicCallerKeys EagerCopy PublicCopySubst.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Module CopyApply (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.

Section Copy.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Let P := program_of Prog argv read.
Variable state_ok : heap_types -> Prims.mstate -> Prop.

Notation Realizes := (S.Realizes W Prog argv read state_ok).
Notation ClosureOk := (S.ClosureOk W Prog argv read state_ok).
Notation Pre := (S.Pre W Prog argv read state_ok).

(* θ over a context's own inputs, each read by f. *)
Definition thof (f : nat -> ty) (C : A.cx) : subst := map (fun q => (fst q, f (fst q))) (A.c_ty C).

(* The copies from u down to the functor h0 whose closure u's is: each
   step is a `bind F = H;` (CCopy's facts), composing its smap and its
   result positions into its source's. *)
Inductive CopyChain : nat -> subst -> option smap -> option (list (option nat)) -> nat -> subst -> option smap -> option (list (option nat)) -> Prop :=
| CCRefl : forall u gfx sms rms, CopyChain u gfx sms rms u gfx sms rms
| CCStep : forall u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0,
    CopyChain h gfh sms rms h0 gf0 sms0 rms0 ->
    A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> S.NeedsAmong target nsh nsu ->
    (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
    (exists sg m, A.CopyInterpretation W sg u h Cu nsu nsh Ch rm target /\ gfx = S.ground_fixed m sg) ->
    (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
    A.SameResult W u h ->
    (* the copy site, syntactically (S.CopySite) *)
    S.CopySite W u h Cu nsu nsh Ch rm target gfx gfh ->
    CopyChain u gfx (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh)) (compose_positions rms rm) h0 gf0 sms0 rms0.

Lemma chain_fixed : forall u gfx sms rms h0 gf0 sms0 rms0, CopyChain u gfx sms rms h0 gf0 sms0 rms0 ->
  forall x, A.Fixed W h0 x -> A.Fixed W u x /\ lookup_subst x gf0 = lookup_subst x gfx.
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch. induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH _ _ _ Fx _ _ _ _];
    intros x F0; [split; [exact F0|reflexivity]|].
  destruct (IH x F0) as [Fh E]. destruct (Fx x Fh) as [Fu E']. split; [exact Fu|congruence].
Qed.

(* A copy chain's result positions read the root's sources in range:
   each copy's positions name positions of its source's result
   (CopyInterpretation), and every reading of one result has the same number of
   keys (PublicCallerKeys.provide_result_length). *)
Lemma chain_sources_range : forall u gfx sms rms h0 gf0 sms0 rms0, CopyChain u gfx sms rms h0 gf0 sms0 rms0 -> rms0 = None ->
  forall srcs, (forall C o C' ks, A.ProvideResult W h0 C o C' ks -> List.length srcs = List.length ks) ->
  (forall C o C' ks, A.ProvideResult W u C o C' ks -> List.length (closure_sources rms srcs) = List.length ks) /\
  (forall s, In s (closure_sources rms srcs) -> In s srcs).
Proof.
  intros u gfx sms rms h0 gf0 sms0 rms0 Ch. induction Ch as [u gfx sms rms|u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 sms0 rms0 _ IH _ _ _ _ CO _ _ _];
    intros E0 srcs Len.
  - subst rms. cbn [closure_sources]. split; [exact Len|intros s I; exact I].
  - destruct (IH E0 srcs Len) as [LenH InH].
    destruct CO as (sg & m & (outs & gen & ys & cg & Cr & ks & Cr' & ks' & _ & _ & _ & _ & _ & _ & PRu & PRh & Lrm & Pos) & _).
    assert (Rg : forall j, In j rm -> j < List.length (closure_sources rms srcs)).
    { intros j Ij. apply In_nth_error in Ij as [i Ei].
      assert (Li : i < List.length ks) by (rewrite <- Lrm; apply nth_error_Some; rewrite Ei; discriminate).
      destruct (nth_error ks i) as [k|] eqn:Ek; [|apply nth_error_None in Ek; lia].
      destruct (Pos i k Ek) as (j' & k' & q & q' & Ej & Ek' & _). rewrite Ei in Ej. injection Ej as <-.
      rewrite (LenH _ _ _ _ PRh). apply nth_error_Some. rewrite Ek'. discriminate. }
    rewrite EagerCopy.sources_compose. split.
    + intros C o C' ks2 PR2. rewrite length_map, Lrm. exact (PublicCallerKeys.provide_result_length W u _ _ _ _ _ _ _ _ PRu PR2).
    + intros s I. exact (InH s (PublicCallerKeys.map_nth_in _ rm Rg s I)).
Qed.

Lemma callee_meaning : forall f d C e x, In x (map fst (A.c_ty C)) ->
  lookup_subst x (type_meanings (S.callee_env d (thof f C) e)) = Some (ground d (f x)).
Proof.
  intros f d C e x I. unfold S.callee_env, thof. cbn [type_meanings]. rewrite map_map. cbn [fst snd].
  exact (S.lookup_on_keys (fun x => ground d (f x)) _ x I).
Qed.

Lemma nlook_keys : forall (l : list (nat * ty)) x v, A.nlook x l = Some v -> In x (map fst l).
Proof. intros l x v L. apply PublicTypeRead.nlook_in in L. change x with (fst (x, v)). apply in_map, L. Qed.

Lemma keys_sub : forall (l l' : list (nat * ty)), (forall x tx, A.nlook x l = Some tx -> A.nlook x l' <> None) ->
  forall x, In x (map fst l) -> In x (map fst l').
Proof.
  intros l l' Sub x I. destruct (keys_in _ x I) as [v Lv]. pose proof (Sub x v Lv) as N.
  destruct (A.nlook x l') as [v'|] eqn:L'; [exact (nlook_keys _ _ _ L')|congruence].
Qed.

(* What a provision says that an application reads: its kind, its
   signature and its fixed readings, each read through C's merge and at T. *)
Definition prov_shape (T : subst) (C : A.cx) (p : A.provision) : nat * option A.fsig * list (nat * ty) :=
  (match p with A.ProvidedVal _ _ => 0 | A.ProvidedFn _ _ => 1 | A.ProvidedMethod _ _ => 2 | A.ProvidedFunctor _ => 3 end,
   option_map (fun g => A.sig_subst T (A.sig_subst (A.c_sub C) g)) (A.provision_signature p),
   match p with A.ProvidedFunctor fx => map (fun q => (fst q, app_subst T (A.canon C (snd q)))) fx | _ => [] end).

Lemma table_gens : forall v Cv ns x tx, A.FunCtx W v Cv ns -> A.nlook x (A.c_ty Cv) = Some tx ->
  incl (A.gens tx) (map fst (A.c_ty Cv)).
Proof.
  intros v Cv ns x tx FC Lx y Iy. destruct (functx_scoped W v Cv ns FC) as (TyA & _ & _ & _ & _ & _ & SubSc).
  rewrite (TyA x tx Lx) in Iy. cbn [app_subst] in Iy. destruct (lookup_subst x (A.c_sub Cv)) as [w|] eqn:Lw.
  - exact (proj2 (SubSc x w (lookup_subst_in_pair _ _ _ Lw)) y Iy).
  - destruct Iy as [<-|[]]. exact (nlook_keys _ _ _ Lx).
Qed.

(* A copy's comparisons of inputs and needs (CopyInterpretation, at
   copy_subst) hold at any meanings T that read F's fixed inputs as the
   copy site did (its recorded readings gfx): every generation the compared
   terms mention is a generation of F's table readings, so it is a fixed
   one sigma reads, or one of ys (PublicCopySubst.copy_ty_read). *)
Lemma copy_read_at : forall u h Cu nsu Ch nsh rm target gfx gfh T,
  A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
  S.CopySite W u h Cu nsu nsh Ch rm target gfx gfh ->
  PublicContextFacts.ReadsMerge Cu (app_subst T) ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst T (TAbs x))) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> app_subst T (A.canon Cu (TAbs x)) = app_subst T (A.canon Cu tx)) /\
  (forall k, In k nsh -> exists p' p, A.ilook (A.provision_id k) (A.c_provisions Ch) = Some p' /\
     A.ilook (A.provision_id (target k)) (A.c_provisions Cu) = Some p /\
     A.copy_need_key Cu T k = A.copy_need_key Cu T (target k) /\
     prov_shape T Cu p' = prov_shape T Cu p).
Proof.
  intros u h Cu nsu Ch nsh rm target gfx gfh T FCu FCh Sub Site Reads FixT.
  destruct Site as (C1 & fx1 & fxh & d1 & _ & AbM & Section1 & Fx1 & CO1 & Egfx & _ & _ & _ & _ & AbU).
  pose proof (proj1 (S.absorbs_canon_iff C1 d1 Section1) AbM) as Ab1.
  set (recorded := map (fun q => (fst q, A.canon C1 (snd q))) fx1) in CO1.
  unfold A.CopyInterpretation in CO1.
  set (sigma := A.copy_fixed_readings Cu recorded) in CO1.
  destruct CO1 as (outs & gen & ys & cg & Cr & ks & Cr' & ks' & scope_after & Outs & Allocation & Ys & Inp & Nds & _).
  destruct (proj1 (AllocationFacts.copy_allocation_contract W recorded outs gen ys cg) (proj1 Allocation))
    as (Nd & Avoid & _ & _).
  assert (NdC : NoDup (map cg ys)) by exact (NoDup_app_remove_l _ _ Nd).
  destruct (functx_scoped W u Cu nsu FCu) as (_ & ScU & _).
  destruct (functx_scoped W h Ch nsh FCh) as (_ & ScH & _).
  pose proof (keys_sub _ _ Sub) as SubK.
  assert (FixedAt : forall x t, In (x, t) recorded ->
    (forall z, In z (A.gens t) -> ~ In z (map cg ys)) /\
    app_subst (type_meanings d1) t = app_subst T (TAbs x)).
  { intros x t Member. split.
    - intros z Iz Ic. refine (Avoid z _ (in_or_app _ _ _ (or_intror Ic))).
      apply in_flat_map. exists (x,t). split; [exact Member|exact Iz].
    - exact (proj2 (S.recorded_fixed_meaning W u C1 fx1 d1 gfx T x t Fx1 Ab1 Egfx FixT Member)). }
  assert (Tr : forall t, incl (A.gens t) (map fst (A.c_ty Cu)) ->
    app_subst (PublicCopySubst.cg_read ys cg T (type_meanings d1)) (app_subst (A.copy_subst sigma ys cg) (A.canon Cu t)) =
    app_subst T (A.canon Cu t)).
  { apply (PublicCopySubst.site_read_composition W u Cu nsu recorded ys cg T (type_meanings d1) FCu Reads NdC);
      [|exact FixedAt].
    intros y Reading Missing. apply Ys. exact (conj Reading Missing). }
  assert (TrE : forall a b, incl (A.gens a) (map fst (A.c_ty Cu)) -> incl (A.gens b) (map fst (A.c_ty Cu)) ->
    app_subst (A.copy_subst sigma ys cg) (A.canon Cu a) = app_subst (A.copy_subst sigma ys cg) (A.canon Cu b) ->
    app_subst T (A.canon Cu a) = app_subst T (A.canon Cu b)).
  { intros a b Sa Sb E. rewrite <- (Tr a Sa), <- (Tr b Sb), E. reflexivity. }
  assert (TrL : forall l1 l2, (forall t, In t l1 -> incl (A.gens t) (map fst (A.c_ty Cu))) ->
    (forall t, In t l2 -> incl (A.gens t) (map fst (A.c_ty Cu))) ->
    map (app_subst (A.copy_subst sigma ys cg)) (map (A.canon Cu) l1) = map (app_subst (A.copy_subst sigma ys cg)) (map (A.canon Cu) l2) ->
    map (app_subst T) (map (A.canon Cu) l1) = map (app_subst T) (map (A.canon Cu) l2)).
  { induction l1 as [|a l1 IH]; intros [|b l2] S1 S2 E; cbn in E |- *; try discriminate; [reflexivity|].
    injection E as Ea El. f_equal.
    - exact (TrE a b (S1 a (or_introl eq_refl)) (S2 b (or_introl eq_refl)) Ea).
    - exact (IH l2 (fun t I => S1 t (or_intror I)) (fun t I => S2 t (or_intror I)) El). }
  assert (TrF : forall (f1 f2 : list (nat * ty)), (forall x t, In (x, t) f1 -> incl (A.gens t) (map fst (A.c_ty Cu))) ->
    (forall x t, In (x, t) f2 -> incl (A.gens t) (map fst (A.c_ty Cu))) ->
    map (fun q => (fst q, app_subst (A.copy_subst sigma ys cg) (A.canon Cu (snd q)))) f1 =
      map (fun q => (fst q, app_subst (A.copy_subst sigma ys cg) (A.canon Cu (snd q)))) f2 ->
    map (fun q => (fst q, app_subst T (A.canon Cu (snd q)))) f1 = map (fun q => (fst q, app_subst T (A.canon Cu (snd q)))) f2).
  { induction f1 as [|[x a] f1 IH]; intros [|[y b] f2] S1 S2 E; cbn in E |- *; try discriminate; [reflexivity|].
    injection E as Exy Ea El. subst y. f_equal; [f_equal; exact (TrE a b (S1 x a (or_introl eq_refl)) (S2 x b (or_introl eq_refl)) Ea)|].
    exact (IH f2 (fun x0 t I => S1 x0 t (or_intror I)) (fun x0 t I => S2 x0 t (or_intror I)) El). }
  split.
  - intros x tx Lx. apply TrE; [| |exact (Inp x tx Lx)].
    + intros y [<-|[]]. destruct (A.nlook x (A.c_ty Cu)) as [w|] eqn:L; [exact (nlook_keys _ _ _ L)|exfalso; exact (Sub x tx Lx L)].
    + intros y Iy. exact (SubK y (table_gens h Ch nsh x tx FCh Lx y Iy)).
  - intros k Ik. destruct (Nds k Ik) as (p' & L' & _ & At & _).
    destruct At as [KeyEq (p & L & E)].
    exists p', p. split; [exact L'|]. split; [exact L|].
    destruct (ScH _ _ (S.ilook_in _ _ _ L')) as [Sg' [KeyScope' Sf']].
    destruct (ScU _ _ (S.ilook_in _ _ _ L)) as [Sg [KeyScope Sf]].
    split.
    { revert KeyScope KeyEq. generalize (target k) as k'.
      intros k' KeyScope KeyEq.
      unfold A.copy_need_key, A.canon_item in *.
      destruct k as [| | |r m a|], k' as [| | |r' m' a'|];
        cbn [A.provision_id app_subst_item map_snd key_gens] in *;
        try discriminate; try exact KeyEq.
      assert (Em : m = m') by congruence. subst m'. f_equal.
      injection KeyEq as Receiver.
      apply TrE; [|exact KeyScope|exact Receiver].
      intros y Iy. exact (SubK y (KeyScope' y Iy)). }
    assert (Sg'u : incl (PublicCallRealize.provision_sig_gens p') (map fst (A.c_ty Cu))) by (intros y Iy; exact (SubK y (Sg' y Iy))).
    unfold A.canon_provision in E. unfold A.canon in TrE, TrL, TrF.
    destruct p' as [t' a'|[ps' r'] a'|[ps' r'] a'|f'], p as [t a|[ps r] a|[ps r] a|f]; cbn in E; try discriminate; unfold prov_shape, A.sig_subst; cbn.
    + injection E as Et _. unfold PublicCallRealize.provision_sig_gens, A.sig_gens in Sg'u, Sg. cbn in Sg'u, Sg. rewrite app_nil_r in Sg'u, Sg.
      rewrite (TrE t' t Sg'u Sg Et). reflexivity.
    + injection E as Ep Er _. unfold PublicCallRealize.provision_sig_gens in Sg'u, Sg. cbn [A.provision_signature] in Sg'u, Sg.
      rewrite (TrE r' r (S.sig_gens_ret (A.FSig ps' r') _ Sg'u) (S.sig_gens_ret (A.FSig ps r) _ Sg) Er).
      pose proof (TrL ps' ps (fun t It => S.sig_gens_param (A.FSig ps' r') _ t Sg'u It) (fun t It => S.sig_gens_param (A.FSig ps r) _ t Sg It)
        ltac:(rewrite !map_map in *; exact Ep)) as Q.
      rewrite !map_map in Q |- *. rewrite Q. reflexivity.
    + injection E as Ep Er _. unfold PublicCallRealize.provision_sig_gens in Sg'u, Sg. cbn [A.provision_signature] in Sg'u, Sg.
      rewrite (TrE r' r (S.sig_gens_ret (A.FSig ps' r') _ Sg'u) (S.sig_gens_ret (A.FSig ps r) _ Sg) Er).
      pose proof (TrL ps' ps (fun t It => S.sig_gens_param (A.FSig ps' r') _ t Sg'u It) (fun t It => S.sig_gens_param (A.FSig ps r) _ t Sg It)
        ltac:(rewrite !map_map in *; exact Ep)) as Q.
      rewrite !map_map in Q |- *. rewrite Q. reflexivity.
    + injection E as Ef. rewrite !map_map in Ef. cbn in Ef.
      exact (f_equal (fun l => (3, @None A.fsig, l)) (TrF f' f (fun x t I y Iy => SubK y (Sf' x t I y Iy)) Sf Ef)).
Qed.

(* A realized merge gives a key and its canonical spelling the same meaning. *)
Lemma canon_copy_key_reads : forall C d k, S.AbsorbsCanon C d ->
  app_subst_item (type_meanings d) (A.provision_id k) =
  A.copy_need_key C (type_meanings d) k.
Proof.
  intros C d [t|v|f|r m a|u] Reads; try reflexivity.
  change (IMth (ground d r) m [] = IMth (ground d (A.canon C r)) m []).
  now rewrite Reads.
Qed.

(* One copy step: H's context is realized where F's is, its needs
   selected from F's by the copy's smap, its inputs read as F's are. *)
Lemma copy_step_realizes : forall rr H d (f : nat -> ty) e1 u Cu nsu h Ch nsh (target : item -> item),
  A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> (forall k, In k nsh -> In (target k) nsu) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx ->
     ground (S.callee_env d (thof f Cu) e1) (A.canon Cu (TAbs x)) = ground (S.callee_env d (thof f Cu) e1) (A.canon Cu tx)) ->
  (forall k, In k nsh -> exists p' p, A.ilook (A.provision_id k) (A.c_provisions Ch) = Some p' /\
     A.ilook (A.provision_id (target k)) (A.c_provisions Cu) = Some p /\
     app_subst_item (type_meanings (S.callee_env d (thof f Cu) e1)) (A.provision_id k) =
       app_subst_item (type_meanings (S.callee_env d (thof f Cu) e1)) (A.provision_id (target k)) /\
     prov_shape (type_meanings (S.callee_env d (thof f Cu) e1)) Cu p' = prov_shape (type_meanings (S.callee_env d (thof f Cu) e1)) Cu p) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
  Realizes rr Cu (S.callee_env d (thof f Cu) e1) H ->
  exists e1', select_provisions e1 (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh) = Some e1' /\
    Realizes rr Ch (S.callee_env d (thof f Ch) e1') H.
Proof.
  intros rr H d f e1 u Cu nsu h Ch nsh target FCu FCh Incl RdT NdT Sub RealU.
  destruct (S.functx_static W _ _ _ FCh) as [[CanH DomH] [[SectionH KMh] [NdH [ProvNsH [ShapeH [ScH FixH]]]]]].
  destruct (S.functx_static W _ _ _ FCu) as [[CanU DomU] [[SectionU KMu] [NdU [ProvNsU [ShapeU [ScU FixU]]]]]].
  set (ce := S.callee_env d (thof f Cu) e1) in *.
  pose proof RealU as [_ [[ValsU [FnsU [MthsU FunsU]]] _]].
  pose proof (S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ RealU) as AbsU.
  pose proof (keys_sub _ _ Sub) as SubK.
  destruct (select_ok e1 (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh)) as [e1' Sel].
  { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
    destruct (functx_needs_provided W u Cu nsu FCu _ (Incl k0 Ik)) as [p0 Ip].
    destruct (S.realized_key W Prog argv read state_ok _ _ _ _ _ _ RealU Ip (ShapeU _ (Incl k0 Ik))) as [pv0 L0].
    change (lookup_provision (A.rkey_of Cu (target k0)) e1) with (lookup_provision (A.rkey_of Cu (target k0)) ce). rewrite L0. discriminate. }
  exists e1'. split; [exact Sel|].
  set (ch := S.callee_env d (thof f Ch) e1').
  assert (GA : forall t, incl (A.gens t) (map fst (A.c_ty Ch)) -> ground ch t = ground ce t).
  { intros t Sc. unfold ground. apply subst_agree_on. intros x Ix. cbn [app_subst].
    unfold ch, ce. rewrite (callee_meaning f d Ch e1' x (Sc x Ix)), (callee_meaning f d Cu e1 x (SubK x (Sc x Ix))).
    reflexivity. }
  (* H's reading of a type over its inputs, through F's merge *)
  assert (GAc : forall t, incl (A.gens t) (map fst (A.c_ty Ch)) -> ground ch t = ground ce (A.canon Cu t)).
  { intros t Sc. transitivity (ground ce t); [exact (GA t Sc)|symmetry; exact (AbsU t)]. }
  assert (GAs : forall ts, (forall t, In t ts -> incl (A.gens t) (map fst (A.c_ty Ch))) ->
    map (ground ce) (map (A.canon Cu) ts) = map (ground ch) ts).
  { intros ts Sc. rewrite map_map. apply map_ext_in. intros t It. symmetry. exact (GAc t (Sc t It)). }
  assert (Look : forall k0 v, In k0 nsh -> lookup_provision (A.rkey_of Cu (target k0)) ce = Some v ->
    lookup_provision (A.rkey_of Ch k0) ch = Some v).
  { intros k0 v Ik L. change (lookup_provision (A.rkey_of Ch k0) ch) with (lookup_provision (A.rkey_of Ch k0) e1').
    rewrite (select_lookup e1 _ e1' _ Sel).
    rewrite (sm_find_map (A.rkey_of Ch) (fun k => A.rkey_of Cu (target k)) nsh k0 Ik).
    - exact L.
    - intros k' Ik' E. exact (rkey_of_injective Ch k' k0 (ShapeH k' Ik') (ShapeH k0 Ik) E). }
  split; [|split; [split; [|split; [|split]]|exact (conj SectionH KMh)]].
  - (* H's merges hold: they are F's readings *)
    apply (S.absorbs_canon_iff Ch ch SectionH).
    intros t. unfold A.canon. change (ground ch (app_subst (A.c_sub Ch) t) = ground ch t).
    unfold ground. rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x Ix. rewrite app_subst_nil.
    destruct (lookup_subst x (A.c_sub Ch)) as [t0|] eqn:Ls.
    2: { replace (app_subst (A.c_sub Ch) (TAbs x)) with (TAbs x) by (cbn; rewrite Ls; reflexivity). reflexivity. }
    replace (app_subst (A.c_sub Ch) (TAbs x)) with t0 by (cbn; rewrite Ls; reflexivity).
    pose proof (lookup_subst_in_pair _ _ _ Ls) as Ip. destruct (DomH x t0 Ip) as [Ixh Gt0].
    destruct (keys_in _ x Ixh) as [v Lx]. pose proof (CanH x v Lx) as Ev. cbn [app_subst] in Ev. rewrite Ls in Ev. subst v.
    pose proof (RdT x t0 Lx) as Rd.
    change (ground ch t0 = ground ch (TAbs x)).
    assert (Sx : incl (A.gens (TAbs x)) (map fst (A.c_ty Ch))) by (intros y [<-|[]]; exact Ixh).
    rewrite (GA t0 Gt0), (GA (TAbs x) Sx).
    transitivity (ground ce (A.canon Cu t0)); [symmetry; apply AbsU|].
    transitivity (ground ce (A.canon Cu (TAbs x))); [exact (eq_sym Rd)|apply AbsU].
  - (* vals *) intros v t Found.
    destruct (vals_ilook _ v t KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    destruct (NdT (IVal v) Ik) as (p' & p & L' & L & Read & Sh).
    assert (Match : target (IVal v) = (IVal v)).
    { destruct (target (IVal v)); cbn [A.provision_id app_subst_item] in Read;
        congruence. }
    rewrite Match in L. cbn [A.provision_id A.canon_item app_subst_item] in L', L.
    rewrite Il in L'. injection L' as <-.
    destruct p as [tu au|g0 a1|g0 a1|f0]; unfold prov_shape in Sh; cbn in Sh; try discriminate. injection Sh as Et.
    destruct (ValsU v _ (ilook_vals _ v _ _ KMu L)) as [w [Lw T]]. exists w.
    split; [apply (Look (IVal v) _ Ik); rewrite Match; exact Lw|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 _].
    unfold provision_sig_gens, A.sig_gens in S2. cbn in S2. rewrite app_nil_r in S2.
    rewrite (GAc t S2). replace (ground ce (A.canon Cu t)) with (ground ce tu); [exact T|].
    rewrite <- (AbsU tu). exact (eq_sym Et).
  - (* fns *) intros f' g Found.
    destruct (fns_ilook _ f' g KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    destruct (NdT (IFn f') Ik) as (p' & p & L' & L & Read & Sh).
    assert (Match : target (IFn f') = (IFn f')).
    { destruct (target (IFn f')); cbn [A.provision_id app_subst_item] in Read;
        congruence. }
    rewrite Match in L. cbn [A.provision_id A.canon_item app_subst_item] in L', L.
    rewrite Il in L'. injection L' as <-.
    destruct p as [tu au|gu a1|gu a1|f0]; unfold prov_shape in Sh; cbn in Sh; try discriminate.
    destruct g as [ps r], gu as [psu ru]. unfold A.sig_subst in Sh. cbn in Sh. injection Sh as Ep Er.
    destruct (FnsU f' _ (ilook_fns _ f' _ _ KMu L)) as [pv [Lp Ok]]. exists pv.
    split; [apply (Look (IFn f') _ Ik); rewrite Match; exact Lp|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 _]. cbn [provision_sig_gens A.provision_signature] in S2.
    apply (S.callable_transfer W Prog argv read state_ok rr rr H ce ch psu ps ru r pv (le_n _)); [| |exact Ok].
    + cbn [A.sg_params]. rewrite <- (GAs ps (fun t It => S.sig_gens_param (A.FSig ps r) _ t S2 It)).
      transitivity (map (ground ce) (map (A.canon Cu) psu)); [rewrite map_map; apply map_ext; intros t; symmetry; apply AbsU|].
      rewrite !map_map in Ep |- *. exact (eq_sym Ep).
    + cbn [A.sg_ret]. rewrite (GAc r (S.sig_gens_ret (A.FSig ps r) _ S2)). rewrite <- (AbsU ru). exact (eq_sym Er).
  - (* methods *) intros r m g Found.
    destruct (mths_ilook _ r m g KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    destruct (NdT (A.mkey r m) Ik) as (p' & p & L' & L & Read & Sh).
    destruct (target (A.mkey r m)) as [| | |r' m' a'|] eqn:Match;
      cbn [A.mkey A.provision_id app_subst_item] in Read; try discriminate.
    injection Read as SameReceiver SameMethod. subst m'.
    change (A.provision_id (A.mkey r m)) with (A.mkey r m) in L'.
    change (A.provision_id (IMth r' m a')) with (A.mkey r' m) in L.
    rewrite Il in L'. injection L' as <-.
    destruct p as [tu au|gu a1|gu a1|f0]; unfold prov_shape in Sh; cbn in Sh; try discriminate.
    destruct g as [ps rt], gu as [psu ru]. unfold A.sig_subst in Sh. cbn in Sh. injection Sh as Ep Er.
    destruct (MthsU _ m _ (ilook_mths _ _ m _ _ KMu L)) as [pv [Lp Ok]]. exists pv.
    split; [apply (Look (A.mkey r m) _ Ik); rewrite Match; exact Lp|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 K2]. cbn [provision_sig_gens A.provision_signature key_gens A.mkey] in S2, K2.
    apply (S.callable_transfer W Prog argv read state_ok rr rr H ce ch (r' :: psu) (r :: ps) ru rt pv (le_n _)); [| |exact Ok].
    + change (map (ground ce) (r' :: psu) = map (ground ch) (r :: ps))
        with (ground ce r' :: map (ground ce) psu = ground ch r :: map (ground ch) ps).
      change (ground ce r = ground ce r') in SameReceiver.
      f_equal; [rewrite <- SameReceiver; symmetry; apply GA; exact K2|].
      rewrite <- (GAs ps (fun t It => S.sig_gens_param (A.FSig ps rt) _ t S2 It)).
      transitivity (map (ground ce) (map (A.canon Cu) psu)); [rewrite map_map; apply map_ext; intros t; symmetry; apply AbsU|].
      rewrite !map_map in Ep |- *. exact (eq_sym Ep).
    + cbn [A.sg_ret]. rewrite (GAc rt (S.sig_gens_ret (A.FSig ps rt) _ S2)). rewrite <- (AbsU ru). exact (eq_sym Er).
  - (* functors *) intros u' fx Found.
    pose proof (funs_ilook _ u' fx KMh Found) as Il. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    destruct (FixH u' fx (S.ilook_in _ _ _ Il)) as [CovH ScFH].
    destruct (NdT (IFun u') Ik) as (p' & p & L' & L & Read & Sh).
    assert (Match : target (IFun u') = (IFun u')).
    { destruct (target (IFun u')); cbn [A.provision_id app_subst_item] in Read;
        congruence. }
    rewrite Match in L. cbn [A.provision_id A.canon_item app_subst_item] in L', L.
    rewrite Il in L'. injection L' as <-.
    destruct p as [tu au|gu a1|gu a1|fxu]; unfold prov_shape in Sh; cbn in Sh; try discriminate. injection Sh as Ef.
    assert (Lu : A.nlook u' (A.c_funs Cu) = Some fxu).
    { change (A.c_funs Cu) with (funs_of (A.c_provisions Cu)). rewrite (funs_by_ilook _ u' KMu), L. reflexivity. }
    destruct (FunsU u' fxu Lu) as [pv [gfx' [Lp [Ok [Cov Vm]]]]]. exists pv, gfx'.
    split; [apply (Look (IFun u') _ Ik); rewrite Match; exact Lp|]. split; [exact Ok|]. split; [exact CovH|].
    intros x tx I.
    assert (I1 : In (x, app_subst (type_meanings ce) (A.canon Cu tx)) (map (fun q => (fst q, app_subst (type_meanings ce) (A.canon Cu (snd q)))) fx))
      by exact (in_map (fun q => (fst q, app_subst (type_meanings ce) (A.canon Cu (snd q)))) fx (x, tx) I).
    pose proof (eq_ind _ (fun l => In (x, app_subst (type_meanings ce) (A.canon Cu tx)) l) I1 _ Ef) as I2.
    apply in_map_iff in I2 as [[x' tu] [Eq Itu]]. cbn in Eq. injection Eq as -> Et.
    rewrite (Vm x tu Itu). f_equal. rewrite (GAc tx (ScFH x tx I)), <- (AbsU tu). exact Et.
Qed.


(* copy_read_at at an application's callee environment, whose meanings
   read u's fixed inputs as the closure's fixed readings gfx say. *)
Lemma copy_step_reads : forall d (f : nat -> ty) e1 u Cu nsu h Ch nsh rm target gfx gfh,
  A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
  S.CopySite W u h Cu nsu nsh Ch rm target gfx gfh ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))) ->
  S.AbsorbsCanon Cu (S.callee_env d (thof f Cu) e1) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx ->
     ground (S.callee_env d (thof f Cu) e1) (A.canon Cu (TAbs x)) = ground (S.callee_env d (thof f Cu) e1) (A.canon Cu tx)) /\
  (forall k, In k nsh -> exists p' p, A.ilook (A.provision_id k) (A.c_provisions Ch) = Some p' /\
     A.ilook (A.provision_id (target k)) (A.c_provisions Cu) = Some p /\
     app_subst_item (type_meanings (S.callee_env d (thof f Cu) e1)) (A.provision_id k) =
       app_subst_item (type_meanings (S.callee_env d (thof f Cu) e1)) (A.provision_id (target k)) /\
     prov_shape (type_meanings (S.callee_env d (thof f Cu) e1)) Cu p' = prov_shape (type_meanings (S.callee_env d (thof f Cu) e1)) Cu p).
Proof.
  intros d f e1 u Cu nsu h Ch nsh rm target gfx gfh FCu FCh Sub Site Fixd Abs.
  pose proof Site as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & AbU).
  assert (FixT : forall x, A.Fixed W u x ->
    lookup_subst x gfx = Some (app_subst (type_meanings (S.callee_env d (thof f Cu) e1)) (TAbs x))).
  { intros x Fu. rewrite (Fixd x Fu). cbn [app_subst].
    rewrite (callee_meaning f d Cu e1 x (S.fixed_in_functx W u Cu nsu x AbU FCu Fu)). reflexivity. }
  pose proof (proj2 (PublicContextFacts.functor_context_merge W u Cu nsu FCu)) as SectionU.
  pose proof (proj2 (S.absorbs_canon_iff _ _ SectionU) Abs) as Reads.
  destruct (copy_read_at u h Cu nsu Ch nsh rm target gfx gfh _ FCu FCh Sub Site Reads FixT) as [Rd Nd].
  split; [exact Rd|].
  intros k Ik. destruct (Nd k Ik) as (p' & p & L' & L & Key & Shape).
  exists p', p. split; [exact L'|]. split; [exact L|]. split; [|exact Shape].
  rewrite (canon_copy_key_reads _ _ k Abs), (canon_copy_key_reads _ _ (target k) Abs).
  exact Key.
Qed.

Definition empty_env : environment := EnvironmentWithAliases [] [] [].

(* Down a closure's copy chain to its root: a linked body, whose context
   the application's environment realizes, or a platform family. *)
Lemma chain_entry : forall rr H u gfx pv, ClosureOk rr H u gfx pv ->
  forall code sms dc rms, pv = FunctorClosure code sms dc rms ->
  forall d (f : nat -> ty) Cu nsu e1, A.FunCtx W u Cu nsu ->
  Realizes rr Cu (S.callee_env d (thof f Cu) e1) H ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))) ->
  (exists h0 gf0 gg Ch0 nsh0 Cg nsg linktarget e2,
     CopyChain u gfx sms rms h0 gf0 (Some (A.link_map Ch0 Cg linktarget nsg)) None /\
     ClosureOk rr H h0 gf0 (FunctorClosure code (Some (A.link_map Ch0 Cg linktarget nsg)) dc None) /\
     (exists pins, LegacyBodyJudgments.LinkBody W Prog h0 gg code pins /\ S.PinsAgree W h0 Ch0 Cg dc gf0 pins /\
       PublicContextFacts.ClosureBinding W h0 Ch0 nsh0 Cg nsg linktarget dc gf0) /\ A.FunCtx W h0 Ch0 nsh0 /\ A.FunCtx W gg Cg nsg /\
     (forall x, In x (map fst (A.c_ty Ch0)) -> In x (map fst (A.c_ty Cu))) /\
     adapt_provisions e1 sms = Some e2 /\
     Realizes rr Cg (application_environment P d code (thof f Ch0) (append_environment e2 dc)) H) \/
  (exists h0 gf0, CopyChain u gfx sms rms h0 gf0 None None /\ dc = empty_env /\ Platform.Family W Prog h0 code /\
     ClosureOk rr H h0 gf0 (FunctorClosure code None dc None)).
Proof.
  intros rr H u gfx pv Ok. induction Ok as [u gg h Cu nsu Cg nsg linktarget dc gfx pins LB Binding FCu FCg FxIn AbU Caps CF PinsOk
    |u h Cu nsu Ch nsh target code sms dc rms rm gfx gfh Okh IH FCu FCh Incl Fx CO Sub SO Site|u code gfx Fam Mach];
    intros code0 sms0 dc0 rms0 E d f Cu0 nsu0 e1 FCu0 Real Fixd; injection E as <- <- <- <-.
  - left. destruct (S.functx_unique W _ _ _ _ _ FCu FCu0) as [<- <-].
    destruct (select_ok e1 (A.link_map Cu Cg linktarget nsg)) as [e2 Sel2].
    { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
      pose proof (S.linked_targets_in W u Cu nsu Cg nsg linktarget _ _ Binding k0 Ik) as Iu.
      destruct (functx_needs_provided W u Cu nsu FCu (A.linked_target linktarget k0) Iu) as [p0 Ip].
      destruct (S.functx_static W _ _ _ FCu) as [_ [_ [_ [_ [ShU _]]]]].
      destruct (S.realized_key W Prog argv read state_ok _ _ _ _ _ _ Real Ip (ShU (A.linked_target linktarget k0) Iu)) as [pv0 L0].
      change (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) e1) with (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) (S.callee_env d (thof f Cu) e1)).
      rewrite L0. discriminate. }
    exists u, gfx, gg, Cu, nsu, Cg, nsg, linktarget, e2.
    split; [constructor|]. split; [exact (S.CLink W Prog argv read state_ok rr H u gg h Cu nsu Cg nsg linktarget dc gfx pins LB Binding FCu FCg FxIn AbU Caps CF PinsOk)|].
    split; [exact (ex_intro _ pins (conj LB (conj PinsOk Binding)))|]. split; [exact FCu|]. split; [exact FCg|]. split; [intros x I; exact I|].
    split; [cbn [adapt_provisions]; exact Sel2|].
    assert (Keys : map fst (thof f Cu) = map fst (A.c_ty Cu)) by (unfold thof; rewrite map_map; reflexivity).
    apply (S.link_entry W Prog argv read state_ok rr H d (thof f Cu) e1 u gg h pins Cu nsu Cg nsg linktarget dc gfx e2
      LB Binding FCu FCg Keys FxIn Real); [|exact Caps|exact CF|exact Sel2].
    intros x Fx'. rewrite (Fixd x Fx'). unfold thof.
    rewrite (S.lookup_on_keys f _ x (FxIn x Fx')). reflexivity.
  - destruct (S.functx_unique W _ _ _ _ _ FCu FCu0) as [<- <-].
    pose proof (S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Reads.
    destruct (copy_step_reads d f e1 u Cu nsu h Ch nsh rm target gfx gfh FCu FCh Sub Site Fixd Reads) as [RdT NdT].
    destruct (copy_step_realizes rr H d f e1 u Cu nsu h Ch nsh target FCu FCh Incl RdT NdT Sub Real)
      as [e1' [Sel RealH]].
    assert (FixH : forall x, A.Fixed W h x -> lookup_subst x gfh = Some (ground d (f x))).
    { intros x Fh. destruct (Fx x Fh) as [Fu E]. rewrite E. exact (Fixd x Fu). }
    assert (Ad : forall e2, adapt_provisions e1' sms = Some e2 ->
      adapt_provisions e1 (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu (target k))) nsh)) = Some e2).
    { intros e2 A2. exact (EagerCopy.adapt_compose _ _ _ _ _ Sel A2). }
    destruct (IH code sms dc rms eq_refl d f Ch nsh e1' FCh RealH FixH) as
      [(h0 & gf0 & gg & Ch0 & nsh0 & Cg & nsg & linktarget & e2 & Chn & Ok0 & LB & FC0 & FCg & Keys & A2 & RealG)
      |(h0 & gf0 & Chn & Edc & Fam & Ok0)].
    + left. exists h0, gf0, gg, Ch0, nsh0, Cg, nsg, linktarget, e2.
      split; [exact (CCStep u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 _ _ Chn FCu FCh Incl Fx CO Sub SO Site)|].
      split; [exact Ok0|]. split; [exact LB|]. split; [exact FC0|]. split; [exact FCg|].
      split; [intros x I; exact (keys_sub _ _ Sub x (Keys x I))|].
      split; [exact (Ad e2 A2)|exact RealG].
    + right. exists h0, gf0.
      split; [exact (CCStep u h Cu nsu Ch nsh target gfx gfh sms rms rm h0 gf0 _ _ Chn FCu FCh Incl Fx CO Sub SO Site)|].
      split; [exact Edc|]. split; [exact Fam|exact Ok0].
  - right. exists u, gfx. split; [constructor|]. split; [reflexivity|]. split; [exact Fam|].
    exact (S.CPlatform W Prog argv read state_ok rr H u code gfx Fam Mach).
Qed.

Lemma app_meanings_ext : forall (b1 b2 : nat -> ty) (tb : list (nat * ty)) l, (forall x, b1 x = b2 x) ->
  map (fun s => (s, match lookup_subst s tb with
    | Some t => app_subst (map (fun s0 => (s0, b1 s0)) l) t | None => b1 s end)) l =
  map (fun s => (s, match lookup_subst s tb with
    | Some t => app_subst (map (fun s0 => (s0, b2 s0)) l) t | None => b2 s end)) l.
Proof.
  intros b1 b2 tb l Eb.
  assert (Em : map (fun s0 => (s0, b1 s0)) l = map (fun s0 => (s0, b2 s0)) l)
    by (apply map_ext; intros y; rewrite Eb; reflexivity).
  apply map_ext. intros s0. destruct (lookup_subst s0 tb); [rewrite Em; reflexivity|rewrite Eb; reflexivity].
Qed.

Lemma app_env_thof : forall d code f Cu Ch0 Cg tb outs bs srcs X,
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty Cg))
    (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg))) tb outs bs srcs) ->
  (forall x, In x (map fst (A.c_ty Ch0)) -> In x (map fst (A.c_ty Cu))) ->
  application_environment P d code (thof f Cu) X = application_environment P d code (thof f Ch0) X.
Proof.
  intros d code f Cu Ch0 Cg tb outs bs srcs X FB Keys. unfold application_environment.
  change (view_functor P code) with (A.funbody_of Prog code). rewrite FB. cbn [fc_inputs fc_linked fc_table].
  assert (E : forall x, (if existsb (Nat.eqb x) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
      then lookup_subst x (thof f Cu) else None) =
    (if existsb (Nat.eqb x) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
      then lookup_subst x (thof f Ch0) else None)).
  { intros x. destruct (existsb _ _) eqn:Ex; [|reflexivity].
    apply existsb_exists in Ex as [y [Iy Ey]]. apply Nat.eqb_eq in Ey. subst y.
    apply filter_In in Iy as [_ Ih]. apply existsb_exists in Ih as [z [Iz Ez]]. apply Nat.eqb_eq in Ez. subst z.
    unfold thof. rewrite (S.lookup_on_keys f _ x Iz), (S.lookup_on_keys f _ x (Keys x Iz)). reflexivity. }
  rewrite (app_meanings_ext _ (fun source =>
      match (if existsb (Nat.eqb source) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
        then lookup_subst source (thof f Ch0) else None) with
      | Some meaning => ground d meaning
      | None => match output_meaning X source with Some meaning => meaning | None => TAbs source end end)).
  - reflexivity.
  - intros x. cbv beta. rewrite (E x). reflexivity.
Qed.

(* ---- the obligations about a closure's root ------------------------------- *)

Notation StepOut := (S.StepOut W Prog argv read state_ok).

(* HYPOTHESIS (T15). LinkResultsOk read through copies: a completed linked
   body h0's code, applied for u (a copy of a copy ... of h0), provides
   u's result at the application. With the empty chain (u = h0) it is
   LinkResultsOk (link_results_of_chain). *)
Definition ChainResultsOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc rms gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr ks' binds0
    pins d1 d2 d3 g3 st3 H3 j',
  Pre rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc rms) ->
  ClosureOk rr H u gfx (FunctorClosure code sms dc rms) ->
  S.FixedMeans W u d fx gfx ->
  CopyChain u gfx sms rms h0 gf0 (Some (A.link_map Ch0 Cg linktarget nsg)) None ->
  ClosureOk rr H h0 gf0 (FunctorClosure code (Some (A.link_map Ch0 Cg linktarget nsg)) dc None) ->
  S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins ->
  PublicContextFacts.ClosureBinding W h0 Ch0 nsh0 Cg nsg linktarget dc gf0 ->
  S.PinsAgree W h0 Ch0 Cg dc gf0 pins ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  S.AR.O.I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> Pre rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr result,
    select_provisions d3 (result_selection (map (A.rkey_of C') ks) rms (S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')) = Some dr /\
    result_environment (map (fun t => (t, gensof t)) outs) d3 (map ITy outs') dr = Some result /\
    Pre rr (A.with_fresh F newgens) C' (append_environment result d) types H3 g st3 /\
    S.MeaningsGrow (map gensof outs) d (append_environment result d).

(* HYPOTHESIS (T15). PlatformApplyOk read through copies: applying a copy
   of a platform family installs the family's primitives. With the empty
   chain it is PlatformApplyOk (platform_apply_of_chain). *)
Definition ChainPlatformOk : Prop := forall k, S.Below W Prog argv read state_ok k ->
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr, rr <= S k -> forall j d types H g st, S j <= rr -> Pre rr F C d types H g st ->
  forall code sms rms gfx h0 gf0, lookup_provision (IFun u) d = Some (FunctorClosure code sms empty_env rms) ->
  ClosureOk rr H u gfx (FunctorClosure code sms empty_env rms) -> S.FixedMeans W u d fx gfx ->
  CopyChain u gfx sms rms h0 gf0 None None -> Platform.Family W Prog h0 code ->
  StepOut rr F (A.with_fresh F newgens) C' False (map gensof outs) (BApply u th (map (fun t => (t, gensof t)) outs) sm (map (A.rkey_of C') ks)) j d types H g st.

Lemma link_results_of_chain : ChainResultsOk -> S.LinkResultsOk W Prog argv read state_ok.
Proof.
  intros CR F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h dc gfx gg dg n ps a r bodies nsg Cg linktarget Fg wrap
    Cb Fb made outs' gen Cr ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm LD Binding PinsOk Sel Sel2 Run E3 Pre3.
  exact (CR F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h _ dc None gfx u gfx gg dg n ps a r bodies Cu ns Cg nsg linktarget
    Fg wrap Cb Fb made outs' gen Cr ks' binds0 pins d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm (CCRefl _ _ _ _) Ok LD Binding PinsOk Sel
    ltac:(cbn [adapt_provisions]; exact Sel2) Run E3 Pre3).
Qed.

Lemma platform_apply_of_chain : ChainPlatformOk -> S.PlatformApplyOk W Prog argv read state_ok.
Proof.
  intros CP k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code Look Fam.
  pose proof AP as (_ & Fx & _).
  pose proof Pre0 as [_ [_ [_ [_ [[_ [[_ [_ [_ Funs]]] _]] _]]]]].
  destruct (Funs u fx Fx) as [pv [gfx [Look' [Ok Fm]]]]. rewrite Look in Look'. injection Look' as <-.
  exact (CP k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code None None gfx u gfx
    Look Ok Fm (CCRefl _ _ _ _) Fam).
Qed.

(* ---- applying a copy -------------------------------------------------------- *)

Hypothesis mentions_typed : PublicFormation.MentionsTyped W.
Hypothesis family_sig_ok : PublicFormation.FamilySigReadOffOk W.
Hypothesis all_fn_elab : forall f dcl rcv n ps rt body,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> LegacyBodyJudgments.FnElab W Prog f.
Hypothesis World : exists b, A.WorldOf b W.
Hypothesis captured_in_force : S.CapturedInForceOk W.
Hypothesis chain_results : ChainResultsOk.
Hypothesis chain_platform : ChainPlatformOk.

(* CopyApplyOk: the closure d holds for u is a copy; its root, reached
   through the chain, runs at the root's interface. (The copy's facts the
   obligation lists are not needed: the closure's own derivation, which
   d realizes, carries them.) *)
Theorem copy_apply_ok : S.CopyApplyOk W Prog argv read state_ok.
Proof.
  intros k Bel J IH F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj LJ Pre0
    gfx0 h Cu0 nsu Ch nsh target code sms dc rms rm gfh Look _ _ _ _ _ _.
  pose proof AP as (R & Fx & FC & Inp & Th & Fixed & Sat & FreshG & Uniq & Outs & NdO & OutsIff & PR).
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [_ [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C Section) as Idem.
  pose proof (S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  destruct (Funs u fx Fx) as [pv [gfx [Look' [Ok Fm]]]]. rewrite Look in Look'. injection Look' as <-.
  set (sms' := compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 (target k))) nsh)) in *.
  set (rms' := compose_positions rms rm) in *.
  set (f := fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))).
  assert (Thf : th = thof f Cu) by exact Th.
  destruct (S.static_realizes_le W Prog argv read state_ok rr rr C d H u Cu ns sm th (le_n _) Real (S.functx_static W _ _ _ FC) Sat
    ltac:(rewrite Th; exact (S.apply_theta W C u Cu ns sm FC Section Inp ltac:(rewrite <- Th; exact Sat)))) as [d1 [Sel [_ RealU]]].
  rewrite Thf in RealU.
  assert (Fixd : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))).
  { intros x Fx'. destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. rewrite (Vm x tx Ixt).
    destruct (Fixed x tx Ixt) as [here [Lh Ceq]]. f_equal. unfold f.
    replace (app_subst (A.c_ty C) (TAbs x)) with here by (cbn [app_subst]; rewrite lookup_subst_nlook', Lh; reflexivity).
    rewrite <- (Abs tx), <- Ceq. reflexivity. }
  destruct (chain_entry rr H u gfx _ Ok code sms' dc rms' eq_refl d f Cu ns d1 FC RealU Fixd) as
    [(h0 & gf0 & gg & Ch0 & nsh0 & Cg & nsg & linktarget & d2 & Chn & Ok0 & LB & FC0 & FCg & Keys & A2 & RealG)
    |(h0 & gf0 & Chn & Edc & Fam & Ok0)].
  2: { subst dc. exact (chain_platform k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
         code sms' rms' gfx h0 gf0 Look Ok Fm Chn Fam). }
  destruct LB as [pins [LB [PinsOk Binding]]].
  destruct (S.link_facts W Prog _ _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Ch0' & nsh0' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
    & outs' & gen & Cr & ks' & binds0 & LD).
  pose proof LD as (FC0' & FCg' & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsI & OutM & NdG & FrG & PR' & RR & BO & FB & _).
  destruct (S.functx_unique W _ _ _ _ _ FC0 FC0') as [<- <-]. destruct (S.functx_unique W _ _ _ _ _ FCg FCg') as [<- <-].
  rewrite <- (app_env_thof d code f Cu Ch0 Cg _ _ _ _ (append_environment d2 dc) FB Keys), <- Thf in RealG.
  set (de := application_environment P d code th (append_environment d2 dc)) in *.
  assert (PreB : Pre rr Fg Cg de [] H [] st).
  { split; [apply (numbering_frame Fg []); rewrite This; exact Num|].
    split; [split; [|split]|split; [apply Forall2_nil|split; [exact St|split; [exact RealG|]]]].
    4: { split; [exact (PublicFormation.functx_cx_formed W mentions_typed gg Cg nsg FCg)|].
         split; [split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]|].
         apply (PublicScoping.functx_cxscoped W gg Cg nsg Fg FCg). intros t0 E0. rewrite This in E0. discriminate. }
    - intros n1 t mu Found. rewrite (S.functx_locals W _ _ _ FCg) in Found. discriminate.
    - intros t E. congruence.
    - apply S.alias_self; [reflexivity|exact (S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ RealG)| |].
      + destruct (functx_scoped W gg Cg nsg FCg) as [CanG _]. exact CanG.
      + intros k0 v0 L0. unfold de, application_environment. change (view_functor P code) with (A.funbody_of Prog code).
        rewrite FB. cbn [type_meanings fc_inputs]. rewrite S.lookup_map_nat; [discriminate|].
        change k0 with (fst (k0, v0)). apply in_map. exact (nlook_in _ _ _ _ L0). }
  intros rest. rewrite S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  destruct (S.items_chain W Prog argv read state_ok mentions_typed family_sig_ok all_fn_elab World
    captured_in_force (S j1) k Bel (IH (S j1) LJ) _ _ _ _ _ _ _ Items) as [bs [Ew Ch']].
  pose proof (Ew EUnit) as E0. rewrite E0 in BO. apply S.bindsof_fold in BO. subst binds0.
  rewrite S.apply_functor_eq, Look. change (view_functor (program_of Prog argv read) code) with (PublicBodySlice.A.funbody_of Prog code). rewrite FB.
  cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel, A2.
  change (application_environment (program_of Prog argv read) d code th (append_environment d2 dc)) with de.
  destruct (S.chain_run W Prog argv read state_ok (S j1) k Fg Cg Fb Cb _ made bs Ch' rr j1 de [] H [] st [] Lr ltac:(lia) ltac:(lia) PreB)
    as [[d3 [g3 [st3 [H3 [j' [_ [E3 [_ [Pre3 Run0]]]]]]]]]|[res [Run [NN Out]]]];
    [rewrite app_nil_r in Run0|rewrite app_nil_r in Run].
  - destruct j' as [|j'].
    { rewrite Run0. right. exists (S.AR.O.I.with_g g (S.AR.O.I.Evaluation OutOfFuel g3 st3)). split; [reflexivity|].
      exact (S.body_stop W Prog argv read state_ok Fg de H [] TUnit False False F d H types g (S.AR.O.I.Evaluation OutOfFuel g3 st3) Ret Loop
        ltac:(intros v E; discriminate) Logic.I). }
    assert (Run : S.AR.O.I.eval_bindings j1 (program_of Prog argv read) de [] st bs = inl (d3, g3, st3)) by (rewrite Run0; reflexivity).
    rewrite Run.
    destruct (chain_results F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms' dc rms' gfx h0 gf0 gg dg n0
      ps0 a0 rs bodies Ch0 nsh0 Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr ks' bs pins d1 d2 d3 g3 st3 H3 j1 Pre0 Look Ok Fm Chn Ok0 LD Binding PinsOk
      Sel A2 Run E3 Pre3) as [dr [result [Sdr [Rres [PreC GrowC]]]]].
    rewrite Sdr, Rres. left. exists (append_environment result d), g, st3, H3.
    split; [exact E3|]. split; [exact GrowC|]. split; [exact PreC|reflexivity].
  - rewrite Run. right. exists (S.AR.O.I.with_g g res). split; [reflexivity|].
    exact (S.body_stop W Prog argv read state_ok Fg de H [] TUnit _ False F d H types g res Ret Loop NN Out).
Qed.

End Copy.
End CopyApply.
