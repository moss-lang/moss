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
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBoolTyping PublicBodySafety PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts
  PublicCallRealize PublicBodySlice.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Module CopyApply (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.
Module I := S.I.

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
   step is a `bind F = H;` (CCopy's facts), appending its smap. *)
Inductive CopyChain : nat -> subst -> list smap -> nat -> subst -> list smap -> Prop :=
| CCRefl : forall u gfx sms, CopyChain u gfx sms u gfx sms
| CCStep : forall u h Cu nsu Ch nsh gfx gfh sms h0 gf0 sms0,
    CopyChain h gfh sms h0 gf0 sms0 ->
    A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> incl nsh nsu ->
    (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
    (forall k, In k nsh -> A.ilook (A.provision_id k) (A.c_provisions Ch) = A.ilook (A.provision_id k) (A.c_provisions Cu)) ->
    (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.canon Cu (TAbs x) = A.canon Cu tx) ->
    (exists sg m, A.CopyOutputs W sg u h Cu /\ gfx = S.ground_fixed m sg) ->
    (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
    CopyChain u gfx (sms ++ [map (fun k => (A.rkey_of Ch k, A.rkey_of Cu k)) nsh])%list h0 gf0 sms0.

Lemma chain_fixed : forall u gfx sms h0 gf0 sms0, CopyChain u gfx sms h0 gf0 sms0 ->
  forall x, A.Fixed W h0 x -> A.Fixed W u x /\ lookup_subst x gf0 = lookup_subst x gfx.
Proof.
  intros u gfx sms h0 gf0 sms0 Ch. induction Ch as [u gfx sms|u h Cu nsu Ch nsh gfx gfh sms h0 gf0 sms0 _ IH _ _ _ Fx _ _ _ _];
    intros x F0; [split; [exact F0|reflexivity]|].
  destruct (IH x F0) as [Fh E]. destruct (Fx x Fh) as [Fu E']. split; [exact Fu|congruence].
Qed.

Lemma adapt_snoc : forall d sms sm, adapt_provisions d (sms ++ [sm])%list =
  match select_provisions d sm with Some d' => adapt_provisions d' sms | None => None end.
Proof.
  intros d sms sm. induction sms as [|s rest IH]; cbn [app adapt_provisions].
  - destruct (select_provisions d sm); reflexivity.
  - rewrite IH. destruct (select_provisions d sm) as [d'|]; [|reflexivity]. reflexivity.
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

(* One copy step: H's context is realized where F's is, its needs
   selected from F's by the copy's smap, its inputs read as F's are. *)
Lemma copy_step_realizes : forall rr H d (f : nat -> ty) e1 u Cu nsu h Ch nsh,
  A.FunCtx W u Cu nsu -> A.FunCtx W h Ch nsh -> incl nsh nsu ->
  (forall k, In k nsh -> A.ilook (A.provision_id k) (A.c_provisions Ch) = A.ilook (A.provision_id k) (A.c_provisions Cu)) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.canon Cu (TAbs x) = A.canon Cu tx) ->
  (forall x tx, A.nlook x (A.c_ty Ch) = Some tx -> A.nlook x (A.c_ty Cu) <> None) ->
  Realizes rr Cu (S.callee_env d (thof f Cu) e1) H ->
  exists e1', select_provisions e1 (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu k)) nsh) = Some e1' /\
    Realizes rr Ch (S.callee_env d (thof f Ch) e1') H.
Proof.
  intros rr H d f e1 u Cu nsu h Ch nsh FCu FCh Incl SN ST Sub RealU.
  destruct (S.functx_static W _ _ _ FCh) as [[CanH DomH] [CnH [KMh [NdH [ProvNsH [ShapeH [ScH FixH]]]]]]].
  destruct (S.functx_static W _ _ _ FCu) as [[CanU DomU] [CnU [KMu [NdU [ProvNsU [ShapeU [ScU FixU]]]]]]].
  set (ce := S.callee_env d (thof f Cu) e1) in *.
  pose proof RealU as [AbsU [[ValsU [FnsU [MthsU FunsU]]] _]].
  pose proof (keys_sub _ _ Sub) as SubK.
  destruct (select_ok e1 (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu k)) nsh)) as [e1' Sel].
  { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
    destruct (functx_needs_provided W u Cu nsu FCu k0 (Incl k0 Ik)) as [p0 Ip].
    destruct (S.realized_key W Prog argv read state_ok _ _ _ _ _ _ RealU Ip (ShapeU k0 (Incl k0 Ik))) as [pv0 L0].
    change (lookup_provision (A.rkey_of Cu k0) e1) with (lookup_provision (A.rkey_of Cu k0) ce). rewrite L0. discriminate. }
  exists e1'. split; [exact Sel|].
  set (ch := S.callee_env d (thof f Ch) e1').
  assert (GA : forall t, incl (A.gens t) (map fst (A.c_ty Ch)) -> ground ch t = ground ce t).
  { intros t Sc. unfold ground. apply subst_agree_on. intros x Ix. cbn [app_subst].
    unfold ch, ce. rewrite (callee_meaning f d Ch e1' x (Sc x Ix)), (callee_meaning f d Cu e1 x (SubK x (Sc x Ix))).
    reflexivity. }
  assert (Look : forall k0 v, In k0 nsh -> lookup_provision (A.rkey_of Cu k0) ce = Some v ->
    lookup_provision (A.rkey_of Ch k0) ch = Some v).
  { intros k0 v Ik L. change (lookup_provision (A.rkey_of Ch k0) ch) with (lookup_provision (A.rkey_of Ch k0) e1').
    rewrite (select_lookup e1 _ e1' _ Sel).
    rewrite (sm_find_map (A.rkey_of Ch) (A.rkey_of Cu) nsh k0 Ik).
    - exact L.
    - intros k' Ik' E. exact (rkey_of_injective Ch k' k0 (ShapeH k' Ik') (ShapeH k0 Ik) E). }
  split; [|split; [split; [|split; [|split]]|split; [exact CnH|exact KMh]]].
  - (* H's merges hold: they are F's readings *)
    intros t. unfold A.canon. change (ground ch (app_subst (A.c_sub Ch) t) = ground ch t).
    unfold ground. rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x Ix. rewrite app_subst_nil.
    destruct (lookup_subst x (A.c_sub Ch)) as [t0|] eqn:Ls.
    2: { replace (app_subst (A.c_sub Ch) (TAbs x)) with (TAbs x) by (cbn; rewrite Ls; reflexivity). reflexivity. }
    replace (app_subst (A.c_sub Ch) (TAbs x)) with t0 by (cbn; rewrite Ls; reflexivity).
    pose proof (lookup_subst_in_pair _ _ _ Ls) as Ip. destruct (DomH x t0 Ip) as [Ixh Gt0].
    destruct (keys_in _ x Ixh) as [v Lx]. pose proof (CanH x v Lx) as Ev. cbn [app_subst] in Ev. rewrite Ls in Ev. subst v.
    pose proof (ST x t0 Lx) as Rd.
    change (ground ch t0 = ground ch (TAbs x)).
    assert (Sx : incl (A.gens (TAbs x)) (map fst (A.c_ty Ch))) by (intros y [<-|[]]; exact Ixh).
    rewrite (GA t0 Gt0), (GA (TAbs x) Sx).
    transitivity (ground ce (A.canon Cu t0)); [symmetry; apply AbsU|].
    transitivity (ground ce (A.canon Cu (TAbs x))); [rewrite Rd; reflexivity|apply AbsU].
  - (* vals *) intros v t Found.
    destruct (vals_ilook _ v t KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    pose proof (SN (IVal v) Ik) as E. cbn [A.provision_id] in E. rewrite Il in E. symmetry in E.
    destruct (ValsU v t (ilook_vals _ v t a0 KMu E)) as [w [L T]]. exists w.
    split; [exact (Look (IVal v) _ Ik L)|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 _].
    unfold provision_sig_gens, A.sig_gens in S2. cbn in S2. rewrite app_nil_r in S2.
    rewrite (GA t S2). exact T.
  - (* fns *) intros f' g Found.
    destruct (fns_ilook _ f' g KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    pose proof (SN (IFn f') Ik) as E. cbn [A.provision_id] in E. rewrite Il in E. symmetry in E.
    destruct (FnsU f' g (ilook_fns _ f' g a0 KMu E)) as [pv [L Ok]]. exists pv.
    split; [exact (Look (IFn f') _ Ik L)|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 _]. unfold provision_sig_gens in S2. cbn in S2.
    apply (S.callable_transfer W Prog argv read state_ok rr rr H ce ch (A.sg_params g) (A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv (le_n _)); [| |exact Ok].
    + apply map_ext_in. intros t It. symmetry. apply GA. intros y Iy. apply S2. unfold A.sig_gens.
      apply in_or_app; right; apply in_flat_map; exists t; split; assumption.
    + symmetry. apply GA. intros y Iy. apply S2. unfold A.sig_gens. apply in_or_app; left; exact Iy.
  - (* methods *) intros r m g Found.
    destruct (mths_ilook _ r m g KMh Found) as [a0 Il]. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    pose proof (SN (A.mkey r m) Ik) as E. change (A.provision_id (A.mkey r m)) with (A.mkey r m) in E. rewrite Il in E. symmetry in E.
    destruct (MthsU r m g (ilook_mths _ r m g a0 KMu E)) as [pv [L Ok]]. exists pv.
    split; [exact (Look (A.mkey r m) _ Ik L)|].
    destruct (ScH _ _ (S.ilook_in _ _ _ Il)) as [S2 K2]. unfold provision_sig_gens in S2. cbn in S2, K2.
    apply (S.callable_transfer W Prog argv read state_ok rr rr H ce ch (r :: A.sg_params g) (r :: A.sg_params g) (A.sg_ret g) (A.sg_ret g) pv (le_n _)); [| |exact Ok].
    + cbn [map]. f_equal; [symmetry; apply GA; exact K2|].
      apply map_ext_in. intros t It. symmetry. apply GA. intros y Iy. apply S2. unfold A.sig_gens.
      apply in_or_app; right; apply in_flat_map; exists t; split; assumption.
    + symmetry. apply GA. intros y Iy. apply S2. unfold A.sig_gens. apply in_or_app; left; exact Iy.
  - (* functors *) intros u' fx Found.
    pose proof (funs_ilook _ u' fx KMh Found) as Il. pose proof (ProvNsH _ _ (S.ilook_in _ _ _ Il)) as Ik.
    destruct (FixH u' fx (S.ilook_in _ _ _ Il)) as [_ ScFH].
    pose proof (SN (IFun u') Ik) as E. cbn [A.provision_id] in E. rewrite Il in E. symmetry in E.
    assert (Lu : A.nlook u' (A.c_funs Cu) = Some fx).
    { change (A.c_funs Cu) with (funs_of (A.c_provisions Cu)). rewrite (funs_by_ilook _ u' KMu), E. reflexivity. }
    destruct (FunsU u' fx Lu) as [pv [gfx' [L [Ok [Cov Vm]]]]]. exists pv, gfx'.
    split; [exact (Look (IFun u') _ Ik L)|]. split; [exact Ok|]. split; [exact Cov|].
    intros x tx I. rewrite (Vm x tx I). f_equal. symmetry. exact (GA tx (ScFH x tx I)).
Qed.

Hypothesis link_merges : S.LinkMergesOk W Prog.

Definition empty_env : environment := EnvironmentWithAliases [] [] [].

(* Down a closure's copy chain to its root: a linked body, whose context
   the application's environment realizes, or a platform family. *)
Lemma chain_entry : forall rr H u gfx pv, ClosureOk rr H u gfx pv ->
  forall code sms dc, pv = FunctorClosure code sms dc ->
  forall d (f : nat -> ty) Cu nsu e1, A.FunCtx W u Cu nsu ->
  Realizes rr Cu (S.callee_env d (thof f Cu) e1) H ->
  (forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))) ->
  (exists h0 gf0 gg Ch0 nsh0 Cg nsg e2,
     CopyChain u gfx sms h0 gf0 [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] /\
     ClosureOk rr H h0 gf0 (FunctorClosure code [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] dc) /\
     A.LinkBody W Prog h0 gg code /\ A.FunCtx W h0 Ch0 nsh0 /\ A.FunCtx W gg Cg nsg /\
     (forall x, In x (map fst (A.c_ty Ch0)) -> In x (map fst (A.c_ty Cu))) /\
     adapt_provisions e1 sms = Some e2 /\
     Realizes rr Cg (application_environment P d code (thof f Ch0) (append_environment e2 dc)) H) \/
  (exists h0 gf0, CopyChain u gfx sms h0 gf0 [] /\ dc = empty_env /\ Platform.Family W Prog h0 code /\
     ClosureOk rr H h0 gf0 (FunctorClosure code [] dc)).
Proof.
  intros rr H u gfx pv Ok. induction Ok as [u gg h Cu nsu Cg nsg dc gfx LB FCu FCg FxIn Caps CF
    |u h Cu nsu Ch nsh code sms dc gfx gfh Okh IH FCu FCh Incl Fx SN ST CO Sub|u code gfx Fam];
    intros code0 sms0 dc0 E d f Cu0 nsu0 e1 FCu0 Real Fixd; injection E as <- <- <-.
  - left. destruct (S.functx_unique W _ _ _ _ _ FCu FCu0) as [<- <-].
    destruct (select_ok e1 (map (fun k0 => (A.rkey_of Cg k0, A.rkey_of Cu k0)) (S.linked_needs nsu nsg))) as [e2 Sel2].
    { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
      unfold S.linked_needs in Ik. apply filter_In in Ik as [Ig E]. apply existsb_exists in E as [k1 [Iu E]]. apply item_eqb_spec in E. subst k1.
      destruct (functx_needs_provided W u Cu nsu FCu k0 Iu) as [p0 Ip].
      destruct (S.functx_static W _ _ _ FCu) as [_ [_ [_ [_ [_ [ShU _]]]]]].
      destruct (S.realized_key W Prog argv read state_ok _ _ _ _ _ _ Real Ip (ShU k0 Iu)) as [pv0 L0].
      change (lookup_provision (A.rkey_of Cu k0) e1) with (lookup_provision (A.rkey_of Cu k0) (S.callee_env d (thof f Cu) e1)).
      rewrite L0. discriminate. }
    exists u, gfx, gg, Cu, nsu, Cg, nsg, e2.
    split; [constructor|]. split; [exact (S.CLink W Prog argv read state_ok rr H u gg h Cu nsu Cg nsg dc gfx LB FCu FCg FxIn Caps CF)|].
    split; [exact LB|]. split; [exact FCu|]. split; [exact FCg|]. split; [intros x I; exact I|].
    split; [cbn [adapt_provisions]; exact Sel2|].
    assert (Keys : map fst (thof f Cu) = map fst (A.c_ty Cu)) by (unfold thof; rewrite map_map; reflexivity).
    apply (S.link_entry W Prog argv read state_ok link_merges rr H d (thof f Cu) e1 u gg h Cu nsu Cg nsg dc gfx e2
      LB FCu FCg Keys FxIn Real); [|exact Caps|exact CF|exact Sel2].
    intros x Fx' _. rewrite (Fixd x Fx'). unfold thof.
    rewrite (S.lookup_on_keys f _ x (FxIn x Fx')). reflexivity.
  - destruct (S.functx_unique W _ _ _ _ _ FCu FCu0) as [<- <-].
    destruct (copy_step_realizes rr H d f e1 u Cu nsu h Ch nsh FCu FCh Incl SN ST Sub Real) as [e1' [Sel RealH]].
    assert (FixH : forall x, A.Fixed W h x -> lookup_subst x gfh = Some (ground d (f x))).
    { intros x Fh. destruct (Fx x Fh) as [Fu E]. rewrite E. exact (Fixd x Fu). }
    assert (Ad : forall e2, adapt_provisions e1' sms = Some e2 ->
      adapt_provisions e1 (sms ++ [map (fun k => (A.rkey_of Ch k, A.rkey_of Cu k)) nsh])%list = Some e2).
    { intros e2 A2. rewrite adapt_snoc, Sel. exact A2. }
    destruct (IH code sms dc eq_refl d f Ch nsh e1' FCh RealH FixH) as
      [(h0 & gf0 & gg & Ch0 & nsh0 & Cg & nsg & e2 & Chn & Ok0 & LB & FC0 & FCg & Keys & A2 & RealG)
      |(h0 & gf0 & Chn & Edc & Fam & Ok0)].
    + left. exists h0, gf0, gg, Ch0, nsh0, Cg, nsg, e2.
      split; [exact (CCStep u h Cu nsu Ch nsh gfx gfh sms h0 gf0 _ Chn FCu FCh Incl Fx SN ST CO Sub)|].
      split; [exact Ok0|]. split; [exact LB|]. split; [exact FC0|]. split; [exact FCg|].
      split; [intros x I; exact (keys_sub _ _ Sub x (Keys x I))|].
      split; [exact (Ad e2 A2)|exact RealG].
    + right. exists h0, gf0.
      split; [exact (CCStep u h Cu nsu Ch nsh gfx gfh sms h0 gf0 _ Chn FCu FCh Incl Fx SN ST CO Sub)|].
      split; [exact Edc|]. split; [exact Fam|exact Ok0].
  - right. exists u, gfx. split; [constructor|]. split; [reflexivity|]. split; [exact Fam|].
    exact (S.CPlatform W Prog argv read state_ok rr H u code gfx Fam).
Qed.

Lemma app_env_thof : forall d code f Cu Ch0 Cg outs bs srcs X,
  A.funbody_of Prog code = Some (FunctorCode (map fst (A.c_ty Cg))
    (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg))) outs bs srcs) ->
  (forall x, In x (map fst (A.c_ty Ch0)) -> In x (map fst (A.c_ty Cu))) ->
  application_environment P d code (thof f Cu) X = application_environment P d code (thof f Ch0) X.
Proof.
  intros d code f Cu Ch0 Cg outs bs srcs X FB Keys. unfold application_environment.
  change (view_functor P code) with (A.funbody_of Prog code). rewrite FB. cbn [fc_inputs fc_linked].
  assert (E : forall x, (if existsb (Nat.eqb x) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
      then lookup_subst x (thof f Cu) else None) =
    (if existsb (Nat.eqb x) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
      then lookup_subst x (thof f Ch0) else None)).
  { intros x. destruct (existsb _ _) eqn:Ex; [|reflexivity].
    apply existsb_exists in Ex as [y [Iy Ey]]. apply Nat.eqb_eq in Ey. subst y.
    apply filter_In in Iy as [_ Ih]. apply existsb_exists in Ih as [z [Iz Ez]]. apply Nat.eqb_eq in Ez. subst z.
    unfold thof. rewrite (S.lookup_on_keys f _ x Iz), (S.lookup_on_keys f _ x (Keys x Iz)). reflexivity. }
  assert (M : forall l, map (fun source => (source,
      match (if existsb (Nat.eqb source) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
        then lookup_subst source (thof f Cu) else None) with
      | Some meaning => ground d meaning
      | None => match output_meaning X source with Some meaning => meaning | None => TAbs source end end)) l =
    map (fun source => (source,
      match (if existsb (Nat.eqb source) (filter (fun t => existsb (Nat.eqb t) (map fst (A.c_ty Ch0))) (map fst (A.c_ty Cg)))
        then lookup_subst source (thof f Ch0) else None) with
      | Some meaning => ground d meaning
      | None => match output_meaning X source with Some meaning => meaning | None => TAbs source end end)) l).
  { intros l. apply map_ext. intros x. rewrite (E x). reflexivity. }
  rewrite M. reflexivity.
Qed.

(* ---- the obligations about a closure's root ------------------------------- *)

Notation StepOut := (S.StepOut W Prog argv read state_ok).

(* HYPOTHESIS (T15). LinkResultsOk read through copies: a completed linked
   body h0's code, applied for u (a copy of a copy ... of h0), provides
   u's result at the application. With the empty chain (u = h0) it is
   LinkResultsOk (link_results_of_chain). *)
Definition ChainResultsOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0
    d1 d2 d3 g3 st3 H3 j',
  Pre rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc) ->
  ClosureOk rr H u gfx (FunctorClosure code sms dc) ->
  S.FixedMeans W u d fx gfx ->
  CopyChain u gfx sms h0 gf0 [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] ->
  ClosureOk rr H h0 gf0 (FunctorClosure code [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] dc) ->
  S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> Pre rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr result,
    select_provisions d3 (result_smap (result_substitution th (map (fun t => (t, gensof t)) outs)) (map ITy outs' ++ map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks')%list (S.link_sources Cb outs' gen Cr ks')) = Some dr /\
    result_environment d (map (fun t => (t, gensof t)) outs) d3 (map ITy outs' ++ map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks')%list dr = Some result /\
    Pre rr (A.with_fresh F newgens) C' (append_environment result d) types H3 g st3 /\
    S.MeaningsGrow (map gensof outs) d (append_environment result d).

(* HYPOTHESIS (T15). PlatformApplyOk read through copies: applying a copy
   of a platform family installs the family's primitives. With the empty
   chain it is PlatformApplyOk (platform_apply_of_chain). *)
Definition ChainPlatformOk : Prop := forall k, S.Below W Prog argv read state_ok k ->
  forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr, rr <= S k -> forall j d types H g st, S j <= rr -> Pre rr F C d types H g st ->
  forall code sms gfx h0 gf0, lookup_provision (IFun u) d = Some (FunctorClosure code sms empty_env) ->
  ClosureOk rr H u gfx (FunctorClosure code sms empty_env) -> S.FixedMeans W u d fx gfx ->
  CopyChain u gfx sms h0 gf0 [] -> Platform.Family W Prog h0 code ->
  StepOut rr F (A.with_fresh F newgens) C' False (map gensof outs) (BApply u th (map (fun t => (t, gensof t)) outs) sm) j d types H g st.

Lemma link_results_of_chain : ChainResultsOk -> S.LinkResultsOk W Prog argv read state_ok.
Proof.
  intros CR F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h dc gfx gg dg n ps a r bodies nsg Cg Fg wrap
    Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm LD Sel Sel2 Run E3 Pre3.
  exact (CR F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h _ dc gfx u gfx gg dg n ps a r bodies Cu ns Cg nsg
    Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm (CCRefl _ _ _) Ok LD Sel
    ltac:(cbn [adapt_provisions]; exact Sel2) Run E3 Pre3).
Qed.

Lemma platform_apply_of_chain : ChainPlatformOk -> S.PlatformApplyOk W Prog argv read state_ok.
Proof.
  intros CP k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code Look Fam.
  pose proof AP as (_ & Fx & _).
  pose proof Pre0 as [_ [_ [_ [_ [[_ [[_ [_ [_ Funs]]] _]] _]]]]].
  destruct (Funs u fx Fx) as [pv [gfx [Look' [Ok Fm]]]]. rewrite Look in Look'. injection Look' as <-.
  exact (CP k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0 code [] gfx u gfx
    Look Ok Fm (CCRefl _ _ _) Fam).
Qed.

(* ---- applying a copy -------------------------------------------------------- *)

Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.
Hypothesis family_sig_ok : PublicFormation.FamilySigReadOffOk W.
Hypothesis all_fn_elab : forall f dcl rcv n ps rt body,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> A.FnElab W Prog f.
Hypothesis World : exists b, A.WorldOf b W.
Hypothesis bind_merges : S.BindMergesOk W Prog.
Hypothesis captured_in_force : S.CapturedInForceOk.
Hypothesis chain_results : ChainResultsOk.
Hypothesis chain_platform : ChainPlatformOk.

(* CopyApplyOk: the closure d holds for u is a copy; its root, reached
   through the chain, runs at the root's interface. (The copy's facts the
   obligation lists are not needed: the closure's own derivation, which
   d realizes, carries them.) *)
Theorem copy_apply_ok : S.CopyApplyOk W Prog argv read state_ok.
Proof.
  intros k Bel J IH F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj LJ Pre0
    gfx0 h Cu0 nsu Ch nsh code sms dc gfh Look _ _ _ _ _ _ _ _.
  pose proof AP as (R & Fx & FC & Inp & Th & Fixed & Sat & FreshG & Uniq & Outs & NdO & OutsIff & PR).
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [Abs [[_ [_ [_ Funs]]] [Idem KM]]].
  destruct (Funs u fx Fx) as [pv [gfx [Look' [Ok Fm]]]]. rewrite Look in Look'. injection Look' as <-.
  set (sms' := (sms ++ [map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 k)) nsh])%list) in *.
  set (f := fun x => A.canon C (app_subst (A.c_ty C) (TAbs x))).
  assert (Thf : th = thof f Cu) by exact Th.
  destruct (S.static_realizes_le W Prog argv read state_ok rr rr C d H u Cu ns sm th (le_n _) Real (S.functx_static W _ _ _ FC) Sat
    ltac:(rewrite Th; exact (S.apply_theta W C u Cu ns sm FC Idem Inp ltac:(rewrite <- Th; exact Sat)))) as [d1 [Sel [_ RealU]]].
  rewrite Thf in RealU.
  assert (Fixd : forall x, A.Fixed W u x -> lookup_subst x gfx = Some (ground d (f x))).
  { intros x Fx'. destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. rewrite (Vm x tx Ixt).
    destruct (Fixed x tx Ixt) as [here [Lh Ceq]]. f_equal. unfold f.
    replace (app_subst (A.c_ty C) (TAbs x)) with here by (cbn [app_subst]; rewrite lookup_subst_nlook', Lh; reflexivity).
    rewrite <- (Abs tx), <- Ceq. reflexivity. }
  destruct (chain_entry rr H u gfx _ Ok code sms' dc eq_refl d f Cu ns d1 FC RealU Fixd) as
    [(h0 & gf0 & gg & Ch0 & nsh0 & Cg & nsg & d2 & Chn & Ok0 & LB & FC0 & FCg & Keys & A2 & RealG)
    |(h0 & gf0 & Chn & Edc & Fam & Ok0)].
  2: { subst dc. exact (chain_platform k Bel F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr Lr j d types H g st Lj Pre0
         code sms' gfx h0 gf0 Look Ok Fm Chn Fam). }
  destruct (S.link_facts W Prog _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Ch0' & nsh0' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
    & outs' & gen & Cr & ks' & binds0 & LD).
  pose proof LD as (FC0' & FCg' & LPeq & LRd & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsI & OutM & NdG & FrG & PR' & RR & BO & FB).
  destruct (S.functx_unique W _ _ _ _ _ FC0 FC0') as [<- <-]. destruct (S.functx_unique W _ _ _ _ _ FCg FCg') as [<- <-].
  rewrite <- (app_env_thof d code f Cu Ch0 Cg _ _ _ (append_environment d2 dc) FB Keys), <- Thf in RealG.
  set (de := application_environment P d code th (append_environment d2 dc)) in *.
  assert (PreB : Pre rr Fg Cg de [] H [] st).
  { split; [apply (numbering_frame Fg []); rewrite This; exact Num|].
    split; [split; [|split]|split; [apply Forall2_nil|split; [exact St|split; [exact RealG|]]]].
    4: { split; [exact (PublicFormation.functx_cx_formed W merge_formed mentions_typed gg Cg nsg FCg)|].
         split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]. }
    - intros n1 t mu Found. rewrite (S.functx_locals W _ _ _ FCg) in Found. discriminate.
    - intros t E. congruence.
    - apply S.alias_self; [reflexivity|exact (proj1 RealG)|].
      destruct (functx_scoped W gg Cg nsg FCg) as [CanG _]. exact CanG. }
  intros rest. rewrite S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  destruct (S.items_chain W Prog argv read state_ok merge_formed mentions_typed family_sig_ok all_fn_elab World bind_merges
    captured_in_force (S j1) k Bel (IH (S j1) LJ) _ _ _ _ _ _ _ Items) as [bs [Ew Ch']].
  pose proof (Ew EUnit) as E0. rewrite E0 in BO. apply S.bindsof_fold in BO. subst binds0.
  rewrite S.apply_functor_eq, Look. change (view_functor (program_of Prog argv read) code) with (PublicBodySlice.A.funbody_of Prog code). rewrite FB.
  cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel, A2.
  change (application_environment (program_of Prog argv read) d code th (append_environment d2 dc)) with de.
  destruct (S.chain_run W Prog argv read state_ok (S j1) k Fg Cg Fb Cb _ made bs Ch' rr j1 de [] H [] st [] Lr ltac:(lia) ltac:(lia) PreB)
    as [[d3 [g3 [st3 [H3 [j' [_ [E3 [_ [Pre3 Run0]]]]]]]]]|[res [Run [NN Out]]]];
    [rewrite app_nil_r in Run0|rewrite app_nil_r in Run].
  - destruct j' as [|j'].
    { rewrite Run0. right. exists (I.with_g g (I.Evaluation OutOfFuel g3 st3)). split; [reflexivity|].
      exact (S.body_stop W Prog argv read state_ok Fg de H [] TUnit False False F d H types g (I.Evaluation OutOfFuel g3 st3) Ret Loop
        ltac:(intros v E; discriminate) Logic.I). }
    assert (Run : I.eval_bindings j1 (program_of Prog argv read) de [] st bs = inl (d3, g3, st3)) by (rewrite Run0; reflexivity).
    rewrite Run.
    destruct (chain_results F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms' dc gfx h0 gf0 gg dg n0
      ps0 a0 rs bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' bs d1 d2 d3 g3 st3 H3 j1 Pre0 Look Ok Fm Chn Ok0 LD
      Sel A2 Run E3 Pre3) as [dr [result [Sdr [Rres [PreC GrowC]]]]].
    rewrite Sdr, Rres. left. exists (append_environment result d), g, st3, H3.
    split; [exact E3|]. split; [exact GrowC|]. split; [exact PreC|reflexivity].
  - rewrite Run. right. exists (I.with_g g res). split; [reflexivity|].
    exact (S.body_stop W Prog argv read state_ok Fg de H [] TUnit _ False F d H types g res Ret Loop NN Out).
Qed.

End Copy.
End CopyApply.
