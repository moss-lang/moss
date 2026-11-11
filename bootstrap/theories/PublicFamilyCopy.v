(* FS.FamilyCopyRealizeOk (realize5): a family application of u, whose
   closure is a copy chain u <- h <- ... <- h0, runs the root's code and
   installs under each caller key a closure callable at the caller's
   signature.

   The run is copy_apply_ok's (PublicCopyApply): chain_entry down to the
   root, which is a linked body (run its bindings: items_chain,
   chain_run) or a platform family (its code only installs primitives:
   PublicPlatformApply.installs_run).

   The realization reuses PublicChainRealize with no outputs:
   - the root's LevelReal: a linked root's is root_level; a platform
     root's (platform_level) holds since each position's source is the
     primitive the code installs for the family's own item, callable at
     its signature (PrimsGroundOk), the code reading the root's inputs as
     the application's θ does (app_env_ground; a fixed machine input as
     its machine type, through the chain's fixed readings);
   - chain_real carries it up the chain to u at its own context Cu;
   - at the top (family_top), the caller's reading of u's result at the
     family context and u's at Cu correspond entry by entry (level_rel,
     with PublicFormation.family_keys_typed for its DomT clause, which is
     why FamilyCopyRealizeOk assumes TysScoped W C), and the caller's
     method at its last position is realized at the state d the
     application runs from: θ x is grounded by d as the level's input
     valuation reads x. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require AuditBase.
From NM Require AuditFacts.
From NM Require Import LegacyBodyJudgments.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicTagCheck
  PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply PublicFamilyAgree PublicChainNew PublicChainFwd
  PublicLinkResults PublicChainCore PublicLinkScope.
From NM Require PublicFormation PublicTypeFacts PublicCallerKeys PublicCopyFresh PublicCopySubst PublicScoping PublicFamilyStep PublicFamilyLink
  PublicChainSigs PublicOutputSubst PublicLinkRun PublicFamilySig PublicChainRealize.
From NM Require PublicMergeClass PublicContextFacts.
From NM Require PublicMerge.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Module FamilyCopy (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses (CRz, and through it everything else), so
   the obligations match. *)
Module CRz := PublicChainRealize.ChainRealize Prims I0.

Section Fam.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Local Notation P := (program_of Prog argv read).
Hypothesis World : exists b, A.WorldOf b W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

(* ---- the top: the caller's new methods --------------------------------------- *)

Section Top.
Variables (n : nat) (H3 : heap_types).
Variables (C : A.cx) (u : nat) (matching th : subst) (sm : smap) (Cr : A.cx) (ks : list item).
Hypothesis Ts : PublicFormation.TysScoped W C.
Hypothesis FA : AuditFacts.FamilyApplication W C u matching th sm Cr ks.
Variables (d d3 dr : environment) (rms : option (list (option nat))) (srcs : list item) (M : nat -> ty).
Hypothesis IdemC : forall t, A.canon C (A.canon C t) = A.canon C t.
Hypothesis KMc : PublicCallRealize.KeysMatch (A.c_provisions C).
Let Tin := (map (fun q => (fst q, ground d (snd q))) th ++ type_meanings d)%list.
Variables (Cu : A.cx) (ns : list item).
Hypothesis FC : A.FunCtx W u Cu ns.
Hypothesis LvU : CRz.LevelReal W Prog argv read state_ok n H3 srcs d3 M Tin u Cu rms.
Hypothesis Sdr : select_provisions d3 (result_selection (map (A.rkey_of Cr) ks) rms srcs) = Some dr.
Hypothesis LenS : List.length (closure_sources rms srcs) = List.length ks.

Lemma family_top : forall r m g, In (A.mkey r m) ks -> A.mlook r m (A.c_mths Cr) = Some g -> exists pv,
  lookup_provision (A.rkey Cr r m) dr = Some pv /\ CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CallableOk W Prog argv read state_ok n H3 d (r :: A.sg_params g) (A.sg_ret g) pv.
Proof.
  intros r0 m0 g0 Ik Mg.
  pose proof FA as (Cu1 & ns1 & (fx & Fx & Fam & Poly & FC1 & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FC1 FC) as [-> ->].
  destruct (PublicFormation.family_context_shape matching C) as [Ety [EsubF [EprF EloF]]].
  set (Cf := A.family_context C matching) in *.
  assert (CanF : forall t, A.canon Cf t = A.canon C t) by (intros t; unfold A.canon; rewrite EsubF; reflexivity).
  assert (IdemF : forall t, A.canon Cf (A.canon Cf t) = A.canon Cf t) by (intros t; rewrite !CanF; apply IdemC).
  assert (NoOut : forall t, ~ A.FunOutputs W u t) by (intros t Ft; exact (PublicFormation.family_no_outputs W u t (proj1 Fam) Ft)).
  assert (Parts : th = map (fun q => (fst q, A.canon Cf (app_subst (A.c_ty Cf) (TAbs (fst q))))) (A.c_ty Cu) /\
    (forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty Cf) <> None) /\
    (forall g t, In (g, t) (A.c_sub Cu) -> (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty Cf) <> None) ->
       A.canon Cf (app_subst (A.c_ty Cf) (TAbs g)) = A.canon Cf (app_subst (A.c_ty Cf) t)) /\
    (forall d ps a r b, A.w_decl W u = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
    A.ProvideResult W u Cf (fun t => TAbs t) Cr ks).
  { destruct Sat as [_ [_ [_ [_ Merge0]]]]. pose proof (PublicMergeClass.functx_merges W u Cu ns _ FC InF Merge0) as Merge. split; [exact Th|]. split; [exact InF|]. split; [exact Merge|]. split; [|exact PR].
    intros d' ps' a' r' b' D FP. destruct Fam as [Shape _]. destruct Shape as (d'' & n'' & ps'' & a'' & r'' & D' & Ast'' & _).
    rewrite D in D'. injection D' as <-. rewrite Ast'' in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity. }
  destruct LvU as (gu & outsu & Cru & ksu & Ou & Fru & Inju & PRu & RealU).
  destruct outsu as [|t0 o0]; [|exfalso; exact (NoOut t0 (proj1 (Ou t0) (or_introl eq_refl)))].
  assert (KT : forall q pa x, A.resolve_sym W q pa = Some x -> A.nlook x (A.c_ty Cf) <> None -> A.nlook x (A.c_ty Cu) = None ->
    A.kind_sym W x = Some A.DKType)
    by (intros q pa x _ Lx Nu; exact (PublicFormation.family_keys_typed W C u matching th sm Cr ks Ts FA Cu ns FC x Lx Nu)).
  destruct (PublicChainRealize.level_rel W Cf u th gu (fun t => t) [] Cu ns Cru ksu Cr ks Parts FC PRu IdemF KT Ou
    (fun t It => match It with end) (fun a b Ia => match Ia with end) (fun t It => match It with end))
    as (tm1 & tm2 & L & ECru & EKu & ECr & EKs & FL).
  cbn [map] in FL. rewrite app_nil_r in FL.
  set (K := (map fst (A.c_ty Cu) ++ [])%list) in FL.
  assert (ThKeys : map fst th = map fst (A.c_ty Cu)) by (rewrite Th, map_map; reflexivity).
  pose proof (PublicFamilyAgree.th_sigma W C u th Cf (fun t => TAbs t) Cr ks Cu ns EsubF Parts FC) as ThS.
  destruct (functx_scoped W u Cu ns FC) as (TyA & _ & _ & _ & _ & _ & SubSc).
  assert (S1Inv : forall t, app_subst th (app_subst (A.c_sub Cu) t) = app_subst th t) by exact ThS.
  assert (FLa : forall q, In q L -> PublicOutputSubst.EntryRel th (A.c_sub Cf) tm1 tm2 (fst q) (snd q) /\
    EntryRelM W th (A.c_sub Cf) K (fst q) (snd q) /\ PublicCallerKeys.SameKind (fst q) (snd q)).
  { rewrite Forall_forall in FL. intros q Iq. destruct (FL q Iq) as (R1 & R2 & R3 & _). exact (conj R1 (conj R2 R3)). }
  (* the position the selection reads the caller's key from *)
  set (rs := map (A.rkey_of Cr) ks).
  assert (LenR : List.length (closure_sources rms srcs) = List.length rs) by (unfold rs; rewrite length_map; exact LenS).
  destruct (In_nth_error _ _ Ik) as [i0 E0].
  assert (R0 : nth_error rs i0 = Some (A.rkey Cr r0 m0)) by (unfold rs; rewrite nth_error_map, E0; reflexivity).
  destruct (PublicCallerKeys.selection_at rs rms srcs i0 _ R0 LenR) as [s Fs].
  destruct (PublicCallerKeys.selection_last rs rms srcs _ s Fs LenR) as [i [Ri [Si Last]]].
  unfold rs in Ri. rewrite nth_error_map in Ri. destruct (nth_error ks i) as [ki|] eqn:Ki; [|discriminate]. injection Ri as Eki.
  assert (Eqk : ki = A.mkey r0 m0).
  { pose proof (PublicCallerKeys.rkey_of_inj Cr ki (A.mkey r0 m0) Eki) as Id.
    rewrite (PublicCallerKeys.result_key_id W u Cf _ Cr ks ki PR (nth_error_In _ _ Ki)) in Id. exact Id. }
  subst ki.
  assert (Ni : nth_error (keys_of (fun p => A.entry_key (A.c_sub Cf) (snd p)) L) i = Some (A.mkey r0 m0))
    by (rewrite <- keys_of_map, <- EKs; exact Ki).
  assert (Li : forall i' k', i < i' -> nth_error (keys_of (fun p => A.entry_key (A.c_sub Cf) (snd p)) L) i' = Some k' -> k' <> A.mkey r0 m0).
  { intros i' k' Lt N E. subst k'. rewrite <- keys_of_map, <- EKs in N. apply (Last i' Lt). unfold rs. rewrite nth_error_map, N. reflexivity. }
  destruct (PublicChainRealize.pos_last_all (A.c_sub Cu) (A.c_sub Cf) L
    (fun q Iq => PublicChainSigs.same_kind_none _ _ _ _ (proj2 (proj2 (FLa q Iq))))
    (PublicChainRealize.tr_all W th (A.c_sub Cf) (A.c_sub Cu) _ tm1 tm2 L FLa S1Inv) i _ Ni Li)
    as (pp & k1 & q1 & q2 & Ipp & K1 & K2 & N1 & L1 & P1 & P2 & I1 & I2).
  assert (Nu : nth_error ksu i = Some k1) by (rewrite EKu, keys_of_map; exact N1).
  destruct (RealU i k1 Nu) as (p & pv & Lp & Spv & Rp).
  assert (Ep : p = q1).
  { rewrite (PublicCallerKeys.result_key_id W u Cu _ Cru ksu k1 PRu (nth_error_In _ _ Nu)) in Lp.
    pose proof (I1 (A.c_provisions Cu)) as I1'. rewrite ECru in Lp. cbn [A.output_cx A.c_provisions] in Lp. congruence. }
  subst p.
  (* the level's valuation, and the state's *)
  assert (TokU : PublicChainSigs.TableOk Tin Cu).
  { intros x tx Lx. unfold Tin. rewrite <- !PublicCopySubst.ground_app_subst, (TyA x tx Lx), ThS. reflexivity. }
  assert (OnK : forall x, In x K -> app_subst (type_meanings d) (app_subst th (TAbs x)) = app_subst Tin (TAbs x)).
  { intros x _. change (ground d (app_subst th (TAbs x)) = app_subst Tin (TAbs x)).
    exact (PublicCopySubst.ground_app_subst d th (TAbs x)). }
  rewrite Forall_forall in FL. destruct (FL pp Ipp) as (R1 & R2 & _ & R4 & R5).
  pose proof (PublicChainRealize.entry_reading_of_rel W [] th (A.c_sub Cf) K tm1 tm2 _ _ R1 R2) as Reading.
  destruct (PublicChainRealize.entry_peq (A.Cx [] [] [] [] []) th (A.c_sub Cf) (A.c_sub Cu) Tin (type_meanings d) K
    (PublicChainSigs.sub_inv W u Cu ns Tin FC TokU)
    (PublicContextFacts.reads_empty_merge (A.Cx [] [] [] [] []) (app_subst (type_meanings d)) eq_refl)
    OnK (fst pp) (snd pp) k1 q1 _ q2 Reading R4 P1 P2 R5) as [PE KR].
  pose proof (CRz.real_transfer W Prog argv read state_ok n H3 _ _ _ _ _ _ pv Rp PE KR) as Rq.
  (* the caller's method there *)
  assert (KMf : PublicCallRealize.KeysMatch (A.c_provisions Cf)) by (rewrite EprF; exact KMc).
  pose proof (PublicChainSigs.pr_keys_match W u Cf _ Cr ks PR KMf) as KMr.
  destruct (mths_ilook _ r0 m0 g0 KMr Mg) as [a0 La].
  assert (Eq2 : q2 = A.ProvidedMethod g0 a0).
  { pose proof (I2 (A.c_provisions Cf)) as I2'. rewrite ECr in La. cbn [A.output_cx A.c_provisions] in La. congruence. }
  subst q2.
  destruct Rq as (r1 & m1 & a1 & E1 & e & ps & rt & Ok & Eps & Ert). unfold A.mkey in E1. injection E1 as <- <- _.
  exists pv. split.
  - rewrite (select_lookup _ _ _ _ Sdr). unfold rs in Fs. rewrite Fs.
    rewrite (nth_error_nth _ _ (ITy 0) Si) in Spv. exact Spv.
  - exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.callable_transfer W Prog argv read state_ok n n H3 e d ps (r0 :: A.sg_params g0) rt (A.sg_ret g0) pv (le_n n) Eps Ert Ok).
Qed.
End Top.

(* ---- a platform root's level --------------------------------------------------- *)

Section PlatformLevel.
Hypothesis prims_ground : CRz.CSg.FL.CF.CN.CC.LR.PA.PrimsGroundOk W Prog argv read state_ok.
Variables (rr : nat) (H : heap_types) (h0 code : nat) (Ch0 : A.cx) (needs : list item) (outputs : A.cx)
  (keys : list item) (bindings : list binding).
Hypothesis FD : CRz.CSg.FL.CF.CN.CC.LR.PA.FamilyData W Prog h0 code Ch0 needs outputs keys bindings.
Variables (d d2 : environment) (th : subst) (M : nat -> ty).
Let de := CRz.CSg.FL.CF.CN.CC.LR.PA.app_env Prog argv read d code th d2.
Let Tin := (map (fun q => (fst q, ground d (snd q))) th ++ type_meanings d)%list.
Hypothesis Gr : forall t, incl (A.gens t) (map fst (A.c_ty Ch0)) -> ground de t = ground d (app_subst th t).
Hypothesis MachG : forall x, In x (map fst (program_machines Prog)) ->
  A.Fixed W h0 x -> In x (map fst (A.c_ty Ch0)) -> ground de (TAbs x) = TMachine x.

(* Each position of the family's own result is the primitive its code
   installs for the item there, callable at the item's signature read at
   the application's input valuation. *)
Lemma platform_level : CRz.LevelReal W Prog argv read state_ok rr H (map (A.rkey_of outputs) keys) (CRz.CSg.FL.CF.CN.CC.LR.PA.installs de bindings)
  M Tin h0 Ch0 None.
Proof.
  pose proof FD as (FC0 & NoH & PRf & FB & Binds & _ & _ & _).
  destruct (functx_scoped W h0 Ch0 needs FC0) as (_ & _ & KM0 & _).
  pose proof (PublicChainSigs.pr_keys_match W h0 Ch0 _ outputs keys PRf KM0) as KMo.
  assert (GT : forall t, incl (A.gens t) (map fst (A.c_ty Ch0)) -> ground de t = app_subst Tin t)
    by (intros t Sc; rewrite (Gr t Sc); exact (PublicCopySubst.ground_app_subst d th t)).
  exists (fun t => t), [], outputs, keys.
  split; [intros t; split; [intros []|intros Ft; exact (NoH t Ft)]|].
  split; [intros t []|]. split; [intros a b []|]. split; [exact PRf|].
  intros i key Ki. pose proof (nth_error_In _ _ Ki) as Ik.
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [Ib [op [-> _]]]].
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.last_prim_some _ _ _ Ib) as [op' Lp].
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Forall2_In_r _ _ _ _ _ _ Binds (CRz.CSg.FL.CF.CN.CC.LR.PA.last_prim_in _ _ _ Lp)) as [key2 [Ik2 [op2 [Eb [MP [AK _]]]]]].
  injection Eb as Es2 <-.
  assert (Ek2 : key2 = key).
  { pose proof (PublicCallerKeys.rkey_of_inj outputs key2 key (eq_sym Es2)) as Pid.
    rewrite (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key2 PRf Ik2),
      (PublicCallerKeys.result_key_id W h0 Ch0 _ outputs keys key PRf Ik) in Pid. exact Pid. }
  subst key2.
  pose proof MP as (rf & m & a & dd & form & nm & pars & res & body & Ek & _). pose proof AK as AK'.
  rewrite Ek in AK'. destruct AK' as [gf [Mgf _]].
  destruct (mths_ilook _ rf m gf KMo Mgf) as [a' La].
  pose proof (prims_ground rr H de code h0 Ch0 needs outputs keys bindings FD MachG key op' rf m a gf Ik
    (CRz.CSg.FL.CF.CN.CC.LR.PA.last_prim_in _ _ _ Lp) MP AK Ek Mgf) as Call.
  assert (La' : A.ilook (A.provision_id key) (A.c_provisions outputs) = Some (A.ProvidedMethod gf a')) by (rewrite Ek; exact La).
  assert (Gs : forall t y, In t (A.sg_ret gf :: A.sg_params gf) -> In y (A.gens t) -> In y (map fst (A.c_ty Ch0))).
  { intros t y It Iy.
    assert (Ip : In y (A.provision_gens (A.ProvidedMethod gf a'))).
    { apply (CRz.rel_gens_sub (A.ProvidedMethod gf a') y).
      unfold PublicChainRealize.rel_gens, PublicCallRealize.provision_sig_gens, A.sig_gens. cbn [A.provision_signature].
      apply in_flat_map. exists t. split; assumption. }
    destruct (PublicCopyFresh.result_gens W mentions_typed h0 Ch0 needs h0 (fun t => t) outputs keys FC0 PRf
      (fun t Ft => match NoH t Ft with end) key _ Ik La' y Ip) as [I0|[t' [Ft _]]]; [exact I0|exfalso; exact (NoH t' Ft)]. }
  assert (Gr0 : forall y, In y (A.gens rf) -> In y (map fst (A.c_ty Ch0))).
  { intros y Iy. rewrite Ek in Ik.
    destruct (PublicChainRealize.result_key_gens_out W h0 Ch0 needs h0 (fun t => t) outputs keys FC0 PRf
      (fun t Ft => match NoH t Ft with end) rf m a Ik y Iy) as [(x' & tx & Lx & Ix')|[t' [Ft _]]].
    - exact (PublicChainSigs.table_gens_dom W h0 Ch0 needs x' tx y FC0 Lx Ix').
    - exfalso. exact (NoH t' Ft). }
  exists (A.ProvidedMethod gf a'), (primitive_closure de (A.rkey_of outputs key) op').
  split; [exact La'|]. split.
  { cbn [closure_sources]. rewrite (PublicChainRealize.nth_map_error _ _ _ keys i key _ Ki), CRz.CSg.FL.CF.CN.CC.LR.PA.installs_lookup, Lp. reflexivity. }
  unfold CRz.RealAt, CRz.CallAt. exists rf, m, a. split; [exact Ek|].
  exists de, (rf :: A.sg_params gf), (A.sg_ret gf). split; [exact Call|]. split.
  - cbn [map]. f_equal; [exact (GT rf Gr0)|]. apply map_ext_in. intros t It. apply GT. intros y Iy. exact (Gs t y (or_intror It) Iy).
  - apply GT. intros y Iy. exact (Gs _ y (or_introl eq_refl) Iy).
Qed.
End PlatformLevel.

(* ---- the obligation ------------------------------------------------------------ *)

Section Main.
Hypothesis family_sig_ok : PublicFormation.FamilySigReadOffOk W.
Hypothesis all_fn_elab : forall f dcl rcv n ps rt body,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> LegacyBodyJudgments.FnElab W Prog f.
Hypothesis captured_in_force : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CapturedInForceOk W.
Hypothesis link_results : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.LinkResultsOk W Prog argv read state_ok.
Hypothesis copy_apply : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.CopyApplyOk W Prog argv read state_ok.
Hypothesis platform_apply : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.PlatformApplyOk W Prog argv read state_ok.
Hypothesis prims_ground : CRz.CSg.FL.CF.CN.CC.LR.PA.PrimsGroundOk W Prog argv read state_ok.

Theorem family_copy_realize_ok : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.FamilyCopyRealizeOk W Prog argv read state_ok.
Proof.
  intros k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0
    fx0 gfx0 h Cu0 nsu Ch nsh target code sms dc rms rm gfh _ Look _ _ _ _ _ _.
  pose proof FA as (Cu & ns & (fx & Fx & Fam & Poly & FC & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (PublicFormation.family_context_shape matching C) as [Ety [EsubF [EprF EloF]]].
  set (Cf := A.family_context C matching) in *.
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [_ [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C0 Section) as Idem.
  pose proof (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  assert (FxC0 : A.nlook u (A.c_funs C0) = Some fx) by (unfold A.c_funs; rewrite <- Ep; exact Fx).
  destruct (Funs u fx FxC0) as [pv [gfx [Look' [Ok Fm]]]]. rewrite Look in Look'. injection Look' as <-.
  set (sms' := compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 (target k))) nsh)) in *.
  set (rms' := compose_positions rms rm) in *.
  set (f := fun x => A.canon Cf (app_subst (A.c_ty Cf) (TAbs x))).
  assert (Thf : th = CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.thof f Cu) by exact Th.
  assert (RealF : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok rr Cf d H)
    by (apply (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_ty_free W Prog argv read state_ok rr C0 Cf d H); [rewrite EsubF; exact Es|unfold Cf; rewrite (PublicContextFacts.family_context_merge matching C); exact Emg|
      rewrite EprF; exact Ep|exact Real]).
  assert (Inp : forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty Cf) = Some here).
  { intros t tt L. apply nlook_in in L. destruct (A.nlook t (A.c_ty Cf)) as [here|] eqn:Lh; [exists here; reflexivity|].
    exfalso. exact (InF t (in_map fst _ _ L) Lh). }
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.static_realizes_le W Prog argv read state_ok rr rr Cf d H u Cu ns sm th (le_n _) RealF
    (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_static W _ _ _ FC) Sat ltac:(rewrite Th; exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.apply_theta W Cf u Cu ns sm FC (proj1 (proj2 (proj2 RealF))) Inp ltac:(rewrite <- Th; exact Sat))))
    as [d1 [Sel [_ RealU]]].
  assert (KMf : PublicCallRealize.KeysMatch (A.c_provisions Cf)) by (rewrite EprF, Ep; exact KM).
  assert (FxF : A.nlook u (A.c_funs Cf) = Some fx) by (unfold A.c_funs; rewrite EprF; exact Fx).
  assert (AbU : CRz.CSg.Abstract W u).
  { intros d' ps' a' r' b' D FP. destruct Fam as [Shape _]. destruct Shape as (d'' & n'' & ps'' & a'' & r'' & D' & Ast'' & _).
    rewrite D in D'. injection D' as <-. rewrite Ast'' in FP. cbn in FP. injection FP as _ _ _ <-. reflexivity. }
  assert (Fixd : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))).
  { intros x Fx'. destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. rewrite (Vm x tx Ixt). f_equal. unfold f.
    assert (Ix : In x (map fst (A.c_ty Cu))) by exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.fixed_in_functx W u Cu ns x (CRz.CSg.ast_of_abstract W u Cu ns FC AbU) FC Fx').
    destruct (A.nlook x (A.c_ty Cf)) as [now|] eqn:Ln; [|exfalso; exact (InF x Ix Ln)].
    replace (app_subst (A.c_ty Cf) (TAbs x)) with now by (cbn [app_subst]; rewrite lookup_subst_nlook', Ln; reflexivity).
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.nlook_fixed_app fx x (Cov x Fx')) as [was [Nw Iw]].
    pose proof (FAg x Fx') as Ag.
    rewrite (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.ilook_apps_fun Cf u fx (funs_ilook _ u fx KMf FxF)), Ln, Nw in Ag.
    assert (E1 : Some (ground d tx) = Some (ground d was)) by (rewrite <- (Vm x tx Ixt), <- (Vm x was Iw); reflexivity).
    injection E1 as E1. rewrite E1.
    apply (proj1 RealF). eapply A.METrans; [exact Ag|].
    apply PublicMerge.merge_eq_read. exact (proj1 (proj1 (proj2 (proj2 RealF)))). }
  assert (RealU' : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Realizes W Prog argv read state_ok rr Cu (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.callee_env d (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.thof f Cu) d1) H) by (rewrite <- Thf; exact RealU).
  (* the input valuation every level of the chain reads *)
  set (Tin := (map (fun q => (fst q, ground d (snd q))) th ++ type_meanings d)%list).
  assert (Parts : th = map (fun q => (fst q, A.canon Cf (app_subst (A.c_ty Cf) (TAbs (fst q))))) (A.c_ty Cu) /\
    (forall t, In t (map fst (A.c_ty Cu)) -> A.nlook t (A.c_ty Cf) <> None) /\
    (forall g t, In (g, t) (A.c_sub Cu) -> (forall x, In x (g :: A.gens t) -> A.nlook x (A.c_ty Cf) <> None) ->
       A.canon Cf (app_subst (A.c_ty Cf) (TAbs g)) = A.canon Cf (app_subst (A.c_ty Cf) t)) /\
    (forall d ps a r b, A.w_decl W u = Some d -> A.functor_parts (A.d_ast d) = Some (ps, a, r, b) -> b = None) /\
    A.ProvideResult W u Cf (fun t => TAbs t) Cr ks).
  { destruct Sat as [_ [_ [_ [_ Merge0]]]]. pose proof (PublicMergeClass.functx_merges W u Cu ns _ FC InF Merge0) as Merge. split; [exact Th|]. split; [exact InF|]. split; [exact Merge|]. split; [exact AbU|exact PR]. }
  pose proof (PublicFamilyAgree.th_sigma W C u th Cf (fun t => TAbs t) Cr ks Cu ns EsubF Parts FC) as ThS.
  destruct (functx_scoped W u Cu ns FC) as (TyA & _ & _ & _ & _ & _ & _).
  assert (TokIn : PublicChainSigs.TableOk Tin Cu).
  { intros x tx Lx. unfold Tin. rewrite <- !PublicCopySubst.ground_app_subst, (TyA x tx Lx), ThS. reflexivity. }
  assert (TokTh : PublicChainSigs.TableOk th Cu) by (intros x tx Lx; rewrite (TyA x tx Lx), ThS; reflexivity).
  assert (ThL : forall x, In x (map fst (A.c_ty Cu)) -> lookup_subst x th = Some (f x))
    by (intros x Ix; rewrite Thf; exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_on_keys f _ x Ix)).
  assert (FixT : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (app_subst Tin (TAbs x))).
  { intros x Fx'. rewrite (Fixd x Fx'). f_equal. unfold Tin. rewrite <- PublicCopySubst.ground_app_subst.
    cbn [app_subst]. rewrite (ThL x (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.fixed_in_functx W u Cu ns x (CRz.CSg.ast_of_abstract W u Cu ns FC AbU) FC Fx')). reflexivity. }
  assert (IdemC : forall t, A.canon C (A.canon C t) = A.canon C t) by (intros t; unfold A.canon; rewrite Es; exact (Idem t)).
  assert (KMc : PublicCallRealize.KeysMatch (A.c_provisions C)) by (rewrite Ep; exact KM).
  destruct j as [|j1].
  { right. intros rest. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.bindings_apply_eq. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_entry W Prog argv read state_ok rr H u gfx _ Ok code sms' dc rms' eq_refl d f Cu ns d1 FC RealU' Fixd) as
    [(h0 & gf0 & gg & Ch0 & nsh0 & Cg & nsg & linktarget & d2 & Chn & Ok0 & [pins [LB [PinsOk Binding]]] & FC0 & FCg & Keys & A2 & RealG)
    |(h0 & gf0 & Chn & Edc & Fam0 & Ok0)].
  - (* a linked root: run its body *)
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.link_facts W Prog _ _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Ch0' & nsh0' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
      & outs' & gen & Cr' & ks' & binds0 & LD).
    pose proof LD as (FC0' & FCg' & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsI & OutM & NdG & FrG & PR' & RR & BO & FB & _).
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FC0 FC0') as [<- <-]. destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_unique W _ _ _ _ _ FCg FCg') as [<- <-].
    rewrite <- (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.app_env_thof Prog argv read d code f Cu Ch0 Cg _ _ _ _ (append_environment d2 dc) FB Keys), <- Thf in RealG.
    set (de := application_environment P d code th (append_environment d2 dc)) in *.
    assert (PreB : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.Pre W Prog argv read state_ok rr Fg Cg de [] H [] st).
    { split; [apply (numbering_frame Fg []); rewrite This; exact Num|].
      split; [split; [|split]|split; [apply Forall2_nil|split; [exact St|split; [exact RealG|]]]].
      4: { split; [exact (PublicFormation.functx_cx_formed W mentions_typed gg Cg nsg FCg)|].
           split; [split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]|].
           apply (PublicScoping.functx_cxscoped W gg Cg nsg Fg FCg). intros t0 E0. rewrite This in E0. discriminate. }
      - intros n1 t mu Found. rewrite (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.functx_locals W _ _ _ FCg) in Found. discriminate.
      - intros t E. congruence.
      - apply CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.alias_self; [reflexivity|exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ RealG)| |].
        + destruct (functx_scoped W gg Cg nsg FCg) as [CanG _]. exact CanG.
        + intros k0 v0 L0. unfold de, application_environment. change (view_functor P code) with (A.funbody_of Prog code).
          rewrite FB. cbn [type_meanings fc_inputs]. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.lookup_map_nat; [discriminate|].
          change k0 with (fst (k0, v0)). apply in_map. exact (nlook_in _ _ _ _ L0). }
    pose proof (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.case_BApplyAbstract W Prog argv read state_ok mentions_typed family_sig_ok all_fn_elab World
      captured_in_force link_results copy_apply platform_apply k Bel (S j1)) as Ap.
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.items_chain W Prog argv read state_ok mentions_typed family_sig_ok all_fn_elab World
      captured_in_force (S j1) k Bel Ap _ _ _ _ _ _ _ Items) as [bs [Ew Chb]].
    pose proof (Ew EUnit) as E0. rewrite E0 in BO. apply CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.bindsof_fold in BO. subst binds0.
    assert (AF : forall X, (match CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j1 P de [] st bs with
       | inl (d3, _, st3) => match select_provisions d3 (result_selection (map (A.rkey_of Cr) ks) rms' (fc_sources (FunctorCode
           (map fst (A.c_ty Cg)) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg))) (A.link_input_table Ch0 Cg)
           (map ITy outs') bs (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')))) with
           | Some dr => match result_environment [] d3 (map ITy outs') dr with
               | Some result => inl (result, st3) | None => inr (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation (Stuck CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.e_compose) [] st3) end
           | None => inr (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation (Stuck CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.e_compose) [] st3) end
       | inr rr => inr rr end) = X -> CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.apply_functor (S j1) P d u th [] sm (map (A.rkey_of Cr) ks) st = X).
    { intros X <-. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.apply_functor_eq, Look. change (view_functor P code) with (A.funbody_of Prog code). rewrite FB.
      cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel, A2. reflexivity. }
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.chain_run W Prog argv read state_ok (S j1) k Fg Cg Fb Cb _ made bs Chb rr j1 de [] H [] st [] Lr ltac:(lia) ltac:(lia) PreB)
      as [[d3 [g3 [st3 [H3 [j' [_ [E3 [_ [Pre3 Run0]]]]]]]]]|[res [Run [NN Out]]]];
      [rewrite app_nil_r in Run0|rewrite app_nil_r in Run].
    2: { right. intros rest. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.bindings_apply_eq, (AF _ eq_refl), Run. exists (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.with_g g res). split; [reflexivity|].
         exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.body_stop W Prog argv read state_ok Fg de H [] TUnit _ False F d H types g res Ret Loop NN Out). }
    destruct j' as [|j'].
    { right. intros rest. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.bindings_apply_eq, (AF _ eq_refl), Run0. exists (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.with_g g (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation OutOfFuel g3 st3)).
      split; [reflexivity|].
      exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.body_stop W Prog argv read state_ok Fg de H [] TUnit False False F d H types g (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation OutOfFuel g3 st3)
        Ret Loop ltac:(intros v E; discriminate) Logic.I). }
    assert (Run : CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j1 P de [] st bs = inl (d3, g3, st3)) by (rewrite Run0; reflexivity).
    pose proof Pre3 as [_ [_ [_ [_ [Real3 _]]]]].
    set (srcs := CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks').
    assert (LenR : forall C1 o C1' ks1, A.ProvideResult W h0 C1 o C1' ks1 -> List.length srcs = List.length ks1).
    { intros C1 o C1' ks1 P1. unfold srcs, CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.link_sources. rewrite length_map.
      exact (PublicCallerKeys.provide_result_length W h0 Ch0 _ Cr' ks' C1 o C1' ks1 PR' P1). }
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_sources_range W u gfx sms' rms' h0 gf0 _ None Chn eq_refl srcs LenR) as [LenU SrcIn].
    destruct (CRz.CSg.FL.CF.CN.CC.chain_selection W Prog argv read state_ok rr (A.link_site Cg Cb pins outs' gen) d3 H3 h0 Ch0 _ outs' gen Cr' ks'
      (map (A.rkey_of Cr) ks) rms' _ (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.realizes_link_site W Prog argv read state_ok rr Cg Cb pins outs' gen d3 H3 Real3) PR' RR SrcIn)
      as [dr Sdr].
    (* the root's level, carried up the chain *)
    assert (Abs0 : CRz.CSg.Abstract W h0)
      by exact (CRz.CSg.abstract_of_ast W h0 (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.closure_link_abstract W Prog argv read state_ok _ _ _ _ _ _ _ Ok0)).
    assert (ThDom : forall z, In z (map fst (A.c_ty Ch0)) -> lookup_subst z th <> None)
      by (intros z Iz; rewrite (ThL z (Keys z Iz)); discriminate).
    assert (Fix0 : forall y, A.Fixed W h0 y -> lookup_subst y gf0 = Some (app_subst Tin (TAbs y))).
    { intros y Fy. destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_fixed W _ _ _ _ _ _ _ _ Chn y Fy) as [Fu E].
      rewrite E. exact (FixT y Fu). }
    assert (E2 : type_meanings d2 = [] /\ type_aliases d2 = []).
    { destruct sms' as [sm'|]; cbn in A2.
      - exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.select_provisions_empty _ _ _ A2).
      - injection A2 as <-. exact (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.select_provisions_empty _ _ _ Sel). }
    destruct E2 as [Em2 Ea2].
    pose proof (CRz.root_level W Prog argv read state_ok mentions_typed rr H3 World h0 gg code dg n0 ps0 a0 rs bodies
      Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr' ks' bs pins LD linktarget dc gf0 Binding th d d3 _ j1 g3 st st3
      (PublicLinkRun.cmeaning_append_provisions d2 dc Em2 Ea2) ThDom Run Pre3 Fix0 Abs0 PinsOk) as Root.
    pose proof (CRz.chain_real W Prog argv read state_ok mentions_typed rr H3 h0 Ch0 nsh0 srcs d3 (CRz.CSg.FL.CF.CN.CC.LR.read_meaning d3) Tin
      FC0 Abs0 Root u gfx sms' rms' h0 gf0 _ None Chn eq_refl eq_refl Cu ns FC (PublicChainSigs.table_reads_merge W u Cu ns Tin FC TokIn) FixT) as LvU.
    pose proof (family_top rr H3 C u matching th sm Cr ks Ts FA d d3 dr rms' srcs (CRz.CSg.FL.CF.CN.CC.LR.read_meaning d3) IdemC KMc Cu ns FC LvU Sdr
      (LenU _ _ _ _ PR)) as New.
    left. exists (EnvironmentWithAliases [] [] (provision_values dr)), st3, H3.
    split; [exact E3|]. split; [exact (proj1 (proj2 (proj2 (proj2 Pre3))))|].
    split; [reflexivity|]. split; [reflexivity|]. split; [exact New|].
    apply AF. rewrite Run. cbn [fc_sources]. unfold srcs in Sdr. rewrite Sdr. reflexivity.
  - (* a platform root: its code installs the family's primitives *)
    subst dc.
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.family_data W Prog h0 code Fam0) as (Ch0 & needs & outputs & keys & bindings & FD).
    pose proof FD as (FC0 & NoH & PRf & FB & Binds & _ & AbsH & _).
    pose proof (CRz.CSg.FL.CF.CN.CC.LR.PA.chain_keys W _ _ _ _ _ _ _ _ Chn _ _ _ _ FC FC0) as Kin.
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.chain_adapt W Prog argv read state_ok _ _ _ _ _ _ Chn rr H d f Cu ns d1 FC RealU' Fixd) as [d2 A2].
    set (de := CRz.CSg.FL.CF.CN.CC.LR.PA.app_env Prog argv read d code th d2).
    assert (Gr : forall t, incl (A.gens t) (map fst (A.c_ty Ch0)) -> ground de t = ground d (app_subst th t))
      by exact (CRz.CSg.FL.CF.CN.CC.LR.PA.app_env_ground Prog argv read d code th d2 Ch0 Cu f _ _ _ FB Thf Kin).
    (* the code reads a fixed machine input as its machine type: the root
       closure records it, and the chain keeps the root's fixed readings *)
    assert (MachG : forall x, In x (map fst (program_machines Prog)) ->
      A.Fixed W h0 x -> In x (map fst (A.c_ty Ch0)) -> ground de (TAbs x) = TMachine x).
    { intros x Inm F0 Ix0.
      destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.closure_cases W Prog argv read state_ok _ _ _ _ _ Ok0) as [CL|[CCp|CP]].
      { destruct CL as (gg & h' & Cu' & nsu' & Cg & nsg & linktarget & dc & E & _). injection E; intros; discriminate. }
      { destruct CCp as (h' & Cu' & nsu' & Ch' & nsh' & target0 & code' & sms1 & dc & rms1 & rm1 & gfh1 & E & _). injection E; intros.
        destruct sms1; discriminate. }
      destruct CP as (code' & _ & _ & Mach).
      destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_fixed W _ _ _ _ _ _ _ _ Chn x F0) as [Fu Egf].
      pose proof (Mach x Inm F0) as Mx. rewrite Egf, (Fixd x Fu) in Mx. injection Mx as Mx.
      rewrite (Gr (TAbs x) ltac:(intros y [<-|[]]; exact Ix0)). cbn [app_subst]. rewrite (ThL x (Kin x Ix0)). exact Mx. }
    pose proof (platform_level prims_ground rr H h0 code Ch0 needs outputs keys bindings FD d d2 th (fun t => TAbs t) Gr MachG) as Root0.
    pose proof (CRz.chain_real W Prog argv read state_ok mentions_typed rr H h0 Ch0 needs (map (A.rkey_of outputs) keys)
      (CRz.CSg.FL.CF.CN.CC.LR.PA.installs de bindings) (fun t => TAbs t) Tin FC0 AbsH (fun _ => Root0) u gfx sms' rms' h0 gf0 None None Chn eq_refl eq_refl
      Cu ns FC (PublicChainSigs.table_reads_merge W u Cu ns Tin FC TokIn) FixT) as LvU.
    set (Dr := map (A.rkey_of outputs) keys) in *.
    assert (LenR : forall C1 o C1' ks1, A.ProvideResult W h0 C1 o C1' ks1 -> List.length Dr = List.length ks1).
    { intros C1 o C1' ks1 P1. unfold Dr. rewrite length_map.
      exact (PublicCallerKeys.provide_result_length W h0 Ch0 _ outputs keys C1 o C1' ks1 PRf P1). }
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.chain_sources_range W u gfx sms' rms' h0 gf0 None None Chn eq_refl Dr LenR) as [LenU SrcIn].
    assert (AllPrim : Forall CRz.CSg.FL.CF.CN.CC.LR.PA.IsPrim bindings).
    { clear -Binds. induction Binds as [|key b keys' bs' [op [-> _]] _ IH]; constructor; [do 2 eexists; reflexivity|exact IH]. }
    assert (AF : forall X, match CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.eval_bindings j1 P de [] st bindings with
       | inl (d3, _, st3) => match select_provisions d3 (result_selection (map (A.rkey_of Cr) ks) rms' Dr) with
           | Some dr => match result_environment [] d3 Dr dr with
               | Some result => inl (result, st3) | None => inr (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation (Stuck CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.e_compose) [] st3) end
           | None => inr (CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.Evaluation (Stuck CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.e_compose) [] st3) end
       | inr rr => inr rr end = X -> CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.AR.O.I.apply_functor (S j1) P d u th [] sm (map (A.rkey_of Cr) ks) st = X).
    { intros X <-. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.apply_functor_eq, Look. change (view_functor P code) with (A.funbody_of Prog code). rewrite FB.
      cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel, A2. reflexivity. }
    destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.installs_run P j1 bindings de [] st AllPrim) as [Run|Run].
    2: { right. intros rest. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.bindings_apply_eq, (AF _ eq_refl), Run. eexists. split; [reflexivity|].
         split; [intros v E; discriminate|exact Logic.I]. }
    assert (SrcKey : forall s, In s Dr -> exists op, CRz.CSg.FL.CF.CN.CC.LR.PA.last_prim s bindings = Some op).
    { intros s Is. unfold Dr in Is. apply in_map_iff in Is as [key [<- Ik]].
      destruct (CRz.CSg.FL.CF.CN.CC.LR.PA.forall2_in_l _ _ _ _ _ _ Binds Ik) as [b [Ib [op [-> _]]]]. exact (CRz.CSg.FL.CF.CN.CC.LR.PA.last_prim_some _ _ _ Ib). }
    destruct (select_ok (CRz.CSg.FL.CF.CN.CC.LR.PA.installs de bindings) (result_selection (map (A.rkey_of Cr) ks) rms' Dr)) as [dr Sdr].
    { intros [a b] Iq. cbn [snd]. destruct (PublicCallerKeys.selection_in _ _ _ _ _ Iq) as [_ Ib].
      destruct (SrcKey b (SrcIn b Ib)) as [op Lp]. rewrite CRz.CSg.FL.CF.CN.CC.LR.PA.installs_lookup, Lp. discriminate. }
    pose proof (family_top rr H C u matching th sm Cr ks Ts FA d (CRz.CSg.FL.CF.CN.CC.LR.PA.installs de bindings) dr rms' Dr (fun t => TAbs t) IdemC KMc
      Cu ns FC LvU Sdr (LenU _ _ _ _ PR)) as New.
    left. exists (EnvironmentWithAliases [] [] (provision_values dr)), st, H.
    split; [apply CRz.CSg.FL.CF.CN.CC.LR.PA.FS.CopyA.S.extends_refl|]. split; [exact St|]. split; [reflexivity|]. split; [reflexivity|]. split; [exact New|].
    apply AF. rewrite Run, Sdr. reflexivity.
Qed.
End Main.

End Fam.
End FamilyCopy.
