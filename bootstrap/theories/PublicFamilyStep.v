(* FamilyStepOk (PublicBodySlice), from the application machinery.

   A family application `BApply u θ [] sm` is an abstract application of
   the family u at its family context `family_context C matching`, with no
   outputs and no new generations. Its run is the abstract application's
   run (PublicBodySlice.app_step): the closure found for u is linked (run
   its body: link_entry, chain_run), a copy, or a platform family. What
   it installs (FamilyResultOk) is a set of provisions, no type meanings
   and no aliases, under the run-time keys of the family's new method
   provisions, each callable at its signature.

   `family_run` proves this from family versions of the application
   obligations (`FamilyLinkResultsOk`, `FamilyCopyApplyOk`,
   `FamilyPlatformApplyOk`: AppPremises does not fit a family
   application, which has no source path and whose state realizes C0, not
   the family context). `realizes_family` shows the code's table with the
   family's provisions realized; `keep_family` keeps realized every
   context among C0's provisions whose methods agree with the family's at a
   shared run-time key (FamilyCoherent); `family_formed` shows the result
   context formed, the matching being read off (FamilyReadOff) an ok
   receiver in C0's scope. `family_step_ok` is then FamilyStepOk, with no
   premise beyond the application obligations. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import AuditBase.
From NM Require Import LegacyBodyJudgments.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBoolTyping PublicBodySafety PublicTypeFacts PublicArms PublicTagCheck PublicContextFacts
  PublicCallRealize PublicBodySlice.
From NM Require PublicFormation PublicCopyApply PublicCallerKeys.
From NM Require Import AuditFacts.
From NM Require PublicMerge.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

(* ---- a family's result adds method provisions only ------------------------- *)

Definition MethodEntry (e : A.entry) : Prop := exists r m g a, e = A.NMth r m g a.

Lemma family_entries_methods : forall W,
  (forall p ob tm sp es tm', A.OutputItemEntries W p ob tm sp es tm' ->
     A.FamilyResult W p sp -> Forall MethodEntry es) /\
  (forall p ob tm sps es tm', A.OutputItemsEntries W p ob tm sps es tm' ->
     Forall (A.FamilyResult W p) sps -> Forall MethodEntry es).
Proof.
  intros W.
  assert (CtxKind : forall p pa a s k, A.resolve_sym W p pa = Some s -> A.kind_sym W s = Some k ->
    A.FamilyResult W p (Spec (Some pa) None a) -> k = A.DKCtx).
  { intros p pa a s k R K Fr. inversion Fr as [|p0 pa0 a0 u d n its sps Ru Du Au Es Sub]; subst.
    assert (s = u) by congruence. subst u. unfold A.kind_sym in K. rewrite Du, Au in K. injection K as <-. reflexivity. }
  apply (PublicFormation.oies_mut W
    (fun p ob tm sp es tm' _ => A.FamilyResult W p sp -> Forall MethodEntry es)
    (fun p ob tm sps es tm' _ => Forall (A.FamilyResult W p) sps -> Forall MethodEntry es)).
  - intros p ob tm pa a bs t R K Br Fr. pose proof (CtxKind p pa a t _ R K Fr). discriminate.
  - intros p ob tm pa a bs s k R K Br Kk Fr. pose proof (CtxKind p pa a s k R K Fr) as ->.
    destruct Kk as [E|[E|[[E _]|[E _]]]]; discriminate E.
  - intros p ob tm a pa s d n t ty0 bs ap R D Ast Br Ty Ap Fr.
    inversion Fr as [|p0 pa0 a0 u d0 n0 its sps Ru Du Au Es Sub]; subst.
    assert (s = u) by congruence. subst u. rewrite D in Du. injection Du as <-. congruence.
  - intros p ob tm a pa s g bs ap d rcv n ps rt R K D Ast Br Sig Ap Fr.
    pose proof (CtxKind p pa a s _ R K Fr). discriminate.
  - intros p ob tm a bs pa u d n ps args res fx R D Ast Br Av Fx Fr.
    inversion Fr as [|p0 pa0 a0 u0 d0 n0 its sps Ru Du Au Es Sub]; subst.
    assert (u = u0) by congruence. subst u0. rewrite D in Du. injection Du as <-. congruence.
  - intros p ob tm a pa dn r m rt g bs ap R Mn Br Rt Sig Ap Fr.
    constructor; [exists rt, m, g, ap; reflexivity|constructor].
  - intros p ob tm a pa c d n its sps bs es tm' R D Ast Esps Br Its IH Fr.
    inversion Fr as [|p0 pa0 a0 u d0 n0 its0 sps0 Ru Du Au Es0 Sub]; subst.
    assert (c = u) by congruence. subst u. rewrite D in Du. injection Du as <-. rewrite Ast in Au.
    injection Au as <- Ei. apply map_CISpec_inj in Ei. subst sps0. exact (IH Sub).
  - intros p ob tm _. constructor.
  - intros p ob tm sp rest es1 tm1 es2 tm2 One IH1 Rest IH2 Fr.
    apply Forall_cons_iff in Fr as [F1 F2]. apply Forall_app. split; [exact (IH1 F1)|exact (IH2 F2)].
Qed.

(* The result of a family application: C's provisions, after new method
   provisions keyed exactly by ks. *)
Lemma family_news : forall W C u matching th sm Cr ks, AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  exists news, A.c_provisions Cr = (news ++ A.c_provisions C)%list /\ A.c_sub Cr = A.c_sub C /\
    A.c_locals Cr = A.c_locals C /\
    (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a) /\
    (forall k, In k ks <-> exists p, In (k, p) news).
Proof.
  intros W C u matching th sm Cr ks (Cu & ns & (fx & Fx & [Shape _] & Poly & FC & Th & InF & FAg & _ & PR & Nd) & _).
  destruct Shape as (d & n & ps & a & r & D & Ast & FrAll & _).
  destruct PR as (d' & ps' & a' & r' & b' & outs & es & tm' & D' & FP & Outs & OIE & RK & ->).
  rewrite D in D'. injection D' as <-. rewrite Ast in FP. cbn in FP. injection FP as <- <- <- <-.
  pose proof (proj2 (family_entries_methods W) _ _ _ _ _ _ OIE FrAll) as Ms. rewrite Forall_forall in Ms.
  destruct (PublicFormation.family_context_shape matching C) as [_ [Esub [Epr Elo]]].
  set (s := A.c_sub (A.family_context C matching)) in *.
  exists (flat_map (A.entry_provision s (fun a => a)) (rev es)).
  unfold A.output_cx. cbn [A.c_provisions A.c_sub A.c_locals]. rewrite Epr.
  split; [reflexivity|]. split; [exact Esub|]. split; [exact Elo|]. split.
  - intros k p I. apply in_flat_map in I as [e [Ie Ik]]. apply (proj2 (in_rev _ _)) in Ie.
    destruct (Ms e Ie) as (r0 & m0 & g0 & a0 & ->). cbn in Ik. destruct Ik as [E|[]]. injection E as <- <-.
    exists (app_subst s r0), m0, (A.sig_subst s g0), a0. split; reflexivity.
  - intros k. unfold A.ResultKeys in RK. subst ks. split.
    + intros I. apply in_flat_map in I as [e [Ie Ik]]. destruct (Ms e Ie) as (r0 & m0 & g0 & a0 & ->).
      cbn in Ik. destruct Ik as [<-|[]]. exists (A.ProvidedMethod (A.sig_subst s g0) a0).
      apply in_flat_map. exists (A.NMth r0 m0 g0 a0). split; [apply (proj1 (in_rev _ _)); exact Ie|left; reflexivity].
    + intros [p I]. apply in_flat_map in I as [e [Ie Ik]]. apply (proj2 (in_rev _ _)) in Ie.
      destruct (Ms e Ie) as (r0 & m0 & g0 & a0 & ->). cbn in Ik. destruct Ik as [E|[]]. injection E as <- _.
      apply in_flat_map. exists (A.NMth r0 m0 g0 a0). split; [exact Ie|left; reflexivity].
Qed.

(* ---- list facts ------------------------------------------------------------- *)

Lemma mlook_app : forall r m (l1 l2 : list ((ty * nat) * A.fsig)),
  A.mlook r m (l1 ++ l2) = match A.mlook r m l1 with Some g => Some g | None => A.mlook r m l2 end.
Proof.
  intros r m l1 l2. induction l1 as [|[[r' m'] g] rest IH]; [reflexivity|]. cbn.
  destruct (ty_eqb r r' && Nat.eqb m m'); [reflexivity|exact IH].
Qed.

Lemma mlook_mths_in : forall r m g l, A.mlook r m (mths_of l) = Some g ->
  exists th a, In (IMth r m th, A.ProvidedMethod g a) l.
Proof.
  intros r m g l. induction l as [|[k p] rest IH]; intros L; [discriminate|].
  unfold mths_of in L. cbn [flat_map] in L.
  destruct k as [t|v|f|r' m' th'|u]; destruct p as [t0 a0|g0 a0|g0 a0|fx]; cbn in L;
    try (destruct (IH L) as [th [a I]]; exists th, a; right; exact I).
  destruct (ty_eqb r r' && Nat.eqb m m') eqn:E.
  - injection L as <-. apply andb_prop in E as [E1 E2]. apply ty_eqb_spec in E1. apply Nat.eqb_eq in E2. subst.
    exists th', a0. left. reflexivity.
  - destruct (IH L) as [th [a I]]. exists th, a. right. exact I.
Qed.

Lemma mlook_mths_some : forall r m g a l, In (A.mkey r m, A.ProvidedMethod g a) l -> A.mlook r m (mths_of l) <> None.
Proof.
  intros r m g a l. induction l as [|[k p] rest IH]; intros I; [contradiction|].
  unfold mths_of. cbn [flat_map]. destruct I as [E|I].
  - injection E as E1 E2. subst k p. cbn. rewrite ty_eqb_refl, Nat.eqb_refl. discriminate.
  - destruct k as [t|v|f|r' m' th'|u]; destruct p as [t0 a0|g0 a0|g0 a0|fx]; cbn; try exact (IH I).
    destruct (ty_eqb r r' && Nat.eqb m m'); [discriminate|exact (IH I)].
Qed.

Lemma ilook_app_none : forall X k (l1 l2 : list (item * X)), (forall p, ~ In (k, p) l1) ->
  A.ilook k (l1 ++ l2) = A.ilook k l2.
Proof.
  intros X k l1 l2 Not. induction l1 as [|[k' p] rest IH]; [reflexivity|]. cbn.
  destruct (item_eqb k k') eqn:E.
  - apply item_eqb_spec in E. subst k'. exfalso. exact (Not p (or_introl eq_refl)).
  - apply IH. intros p' I. exact (Not p' (or_intror I)).
Qed.

Lemma ilook_app_some : forall X k (l1 l2 : list (item * X)) p, A.ilook k l1 = Some p -> A.ilook k (l1 ++ l2) = Some p.
Proof.
  intros X k l1 l2 p. induction l1 as [|[k' q] rest IH]; intros L; [discriminate|]. cbn in L |- *.
  destruct (item_eqb k k'); [exact L|exact (IH L)].
Qed.

Lemma ilook_of_in : forall X k (l : list (item * X)) p, In (k, p) l -> exists p', A.ilook k l = Some p' /\ In (k, p') l.
Proof.
  intros X k l p. induction l as [|[k' q] rest IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb k k') eqn:E.
  - apply item_eqb_spec in E. subst k'. exists q. split; [reflexivity|left; reflexivity].
  - destruct I as [Eq|I]; [injection Eq as -> ->; rewrite (proj2 (item_eqb_spec _ _) eq_refl) in E; discriminate|].
    destruct (IH I) as [p' [L I']]. exists p'. split; [exact L|right; exact I'].
Qed.

Lemma flat_map_methods_nil : forall X (f : item * A.provision -> list X) news,
  (forall k p, In (k, p) news -> exists r m g a, k = A.mkey r m /\ p = A.ProvidedMethod g a) ->
  (forall r m g a, f (A.mkey r m, A.ProvidedMethod g a) = []) -> flat_map f news = [].
Proof.
  intros X f news Meth Nil. induction news as [|[k p] rest IH]; [reflexivity|]. cbn.
  destruct (Meth k p (or_introl eq_refl)) as (r & m & g & a & -> & ->). rewrite Nil. cbn.
  apply IH. intros k' p' I. exact (Meth k' p' (or_intror I)).
Qed.

(* ---- the result context is formed ------------------------------------------ *)

(* A family applied in C, its matching read off an ok receiver in C0's
   scope, C's table read in C0's scope: C0's table with the family's
   provisions is formed (PublicFormation.family_application_ok, at C0's
   scope). *)
Lemma family_formed : forall W C0 C u matching th sm Cr ks ot m,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 -> A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks -> A.FamilyReadOff W C u ot m matching ->
  PublicFormation.TyOk W ot -> PublicFormation.ScopedTy W C0 ot ->
  (forall x v, In (x, v) (A.c_ty C) ->
     PublicFormation.TyOk W (A.canon C v) /\ PublicFormation.ScopedTy W C0 (A.canon C v)) ->
  PublicFormation.FormedCx W C0 ->
  PublicFormation.FormedCx W (A.Cx (A.c_ty C0) (A.c_sub Cr) (A.c_merge Cr) (A.c_provisions Cr) (A.c_locals Cr)).
Proof.
  intros W C0 C u matching th sm Cr ks ot m Es Emg Ep El FA Ro Ok Sc Tin [Cf0 [Ks0 Ts0]].
  pose proof FA as (Cu & ns & (fx & Fx & [Shape _] & Poly & FC & Th & InF & FAg & _ & PR & Nd) & _).
  destruct (PublicFormation.family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  set (K := PublicFormation.GenScoped W C0).
  assert (Mt : forall x v, In (x, v) matching ->
    PublicFormation.TyOk W (A.canon C v) /\ PublicFormation.GensIn K (A.gens (A.canon C v))).
  { destruct Ro as (marks & fr & fks & rf & a & Marks & PRm & Ikm & Dom & NdM & Ot & _).
    intros x v Iq. assert (Ix : In x (A.gens rf)) by (apply (proj2 (proj1 (Dom x) (in_map fst _ _ Iq)))).
    pose proof (PublicFormation.sub_subst (A.c_sub C) _ _
      (PublicFormation.sub_read rf matching x v Ix (PublicFormation.lookup_subst_nodup matching x v NdM Iq))) as Sb.
    change (PublicFormation.SubTy (A.canon C v) (A.canon C (app_subst matching rf))) in Sb. rewrite Ot in Sb.
    destruct (PublicFormation.sub_ok W _ _ Sb Ok) as [Ov Iv]. split; [exact Ov|intros g Ig; exact (Sc g (Iv g Ig))]. }
  set (Cf := A.family_context C matching) in *.
  assert (CanF : forall t, A.canon Cf t = A.canon C t) by (intros t; unfold A.canon; rewrite Esub; reflexivity).
  assert (P1 : PublicFormation.ReadOk W (A.c_sub Cf)) by (rewrite Esub, Es; exact (PublicFormation.cf_read W C0 Cf0)).
  assert (P2 : PublicFormation.ReadWithin K (A.c_sub Cf)).
  { rewrite Esub, Es. apply PublicFormation.canon_within. intros y Iy. exact Iy. }
  assert (P3 : forall x v, In (x, v) (A.c_ty Cf) ->
    PublicFormation.TyOk W (A.canon Cf v) /\ PublicFormation.GensIn K (A.gens (A.canon Cf v))).
  { intros x v Iq. rewrite CanF. rewrite Ety in Iq. apply in_app_or in Iq as [Iq|Iq];
      [apply (proj2 (in_rev _ _)) in Iq; exact (Mt x v Iq)|exact (Tin x v Iq)]. }
  assert (P4 : forall t, A.FunOutputs W u t -> PublicFormation.TyOk W (A.canon Cf (TAbs t)) /\
    PublicFormation.GensIn K (A.gens (A.canon Cf (TAbs t)))).
  { intros t Ft. exfalso. exact (PublicFormation.family_no_outputs W u t Shape Ft). }
  destruct (PublicFormation.provide_result_ok W u Cf (fun t => TAbs t) Cr ks K PR P1 P2 P3 P4)
    as [SubR [LoR [_ PrR]]].
  assert (ScE : PublicFormation.scope_gens (A.Cx (A.c_ty C0) (A.c_sub Cr) (A.c_merge Cr) (A.c_provisions Cr) (A.c_locals Cr))
    = PublicFormation.scope_gens C0) by (apply PublicFormation.scope_gens_ext; [reflexivity|cbn; rewrite SubR, Esub, Es; reflexivity]).
  split; [constructor|split].
  - cbn. exact (PublicFormation.cf_ty W C0 Cf0).
  - cbn. rewrite SubR, Esub, Es. exact (PublicFormation.cf_read W C0 Cf0).
  - cbn. rewrite (PublicContextFacts.family_application_merge W C u matching th sm Cr ks FA), Emg.
    exact (PublicFormation.cf_shapes W C0 Cf0).
  - cbn. intros k p Iq. destruct (PrR k p Iq) as [Iq'|[Kk [Pk _]]];
      [rewrite Epr, Ep in Iq'; exact (PublicFormation.cf_prov W C0 Cf0 k p Iq')|exact (conj Kk Pk)].
  - cbn. intros n t mu Iq. rewrite LoR, Elo, El in Iq. exact (PublicFormation.cf_locals W C0 Cf0 n t mu Iq).
  - intros r m' th' p Iq. apply (PublicFormation.scoped_ext W _ _ _ (eq_sym ScE)). cbn in Iq. destruct (PrR _ _ Iq) as [Iq'|[_ [_ Kr]]];
      [rewrite Epr, Ep in Iq'; exact (Ks0 r m' th' p Iq')|exact (Kr r m' th' eq_refl)].
  - intros x Ix. exact (Ts0 x Ix).
Qed.

Module FamilyStep (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CopyA := PublicCopyApply.CopyApply Prims I0.

Section Fam.
Variables (W : A.world) (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Local Notation P := (program_of Prog argv read).
Variable state_ok : heap_types -> Prims.mstate -> Prop.

(* The slice's premises the abstract application uses. *)
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.
Hypothesis family_sig_ok : PublicFormation.FamilySigReadOffOk W.
Hypothesis all_fn_elab : forall f dcl rcv n ps rt body,
  A.w_decl W f = Some dcl -> A.d_ast dcl = ADFn rcv n ps rt (Some body) -> LegacyBodyJudgments.FnElab W Prog f.
Hypothesis World : exists b, A.WorldOf b W.
Hypothesis captured_in_force : CopyA.S.CapturedInForceOk W.
Hypothesis link_results : CopyA.S.LinkResultsOk W Prog argv read state_ok.
Hypothesis copy_apply : CopyA.S.CopyApplyOk W Prog argv read state_ok.
Hypothesis platform_apply : CopyA.S.PlatformApplyOk W Prog argv read state_ok.

Local Notation Pre := (CopyA.S.Pre W Prog argv read state_ok).
Local Notation Realizes := (CopyA.S.Realizes W Prog argv read state_ok).
Local Notation CallableOk := (CopyA.S.CallableOk W Prog argv read state_ok).
Local Notation ClosureOk := (CopyA.S.ClosureOk W Prog argv read state_ok).
Local Notation Below := (CopyA.S.Below W Prog argv read state_ok).
Local Notation Outcome := (CopyA.S.AR.O.Outcome W P state_ok).

(* ---- what a family application installs -------------------------------------- *)

(* The provisions dr installs: under the run-time key of a new method
   provision of Cr (keyed in ks) only, each callable at its signature. *)
Definition FamilyNewOk (rr : nat) (Cr : A.cx) (ks : list item) (d dr : environment) (H : heap_types) : Prop :=
  (forall K pv, lookup_provision K dr = Some pv -> exists r m, In (A.mkey r m) ks /\ K = A.rkey Cr r m) /\
  (forall r m g, In (A.mkey r m) ks -> A.mlook r m (A.c_mths Cr) = Some g -> exists pv,
     lookup_provision (A.rkey Cr r m) dr = Some pv /\ CallableOk rr H d (r :: A.sg_params g) (A.sg_ret g) pv).

Definition FamilyResultOk (rr : nat) (Cr : A.cx) (ks : list item) (d dr : environment) (H : heap_types) : Prop :=
  type_meanings dr = [] /\ type_aliases dr = [] /\ FamilyNewOk rr Cr ks d dr H.

(* One family application, as a binding: it installs such provisions, or
   stops. *)
Definition FamilyStepOut (rr : nat) F (Cr : A.cx) (ks : list item) u th sm j d types H g st : Prop :=
  forall rest,
  (exists dr st' H', heap_extends H H' /\ state_ok H' st' /\ FamilyResultOk rr Cr ks d dr H' /\
     CopyA.S.AR.O.I.eval_bindings (S j) P d g st (BApply u th [] sm (map (A.rkey_of Cr) ks) :: rest) = CopyA.S.AR.O.I.eval_bindings j P (append_environment dr d) g st' rest) \/
  (exists res, CopyA.S.AR.O.I.eval_bindings (S j) P d g st (BApply u th [] sm (map (A.rkey_of Cr) ks) :: rest) = inr res /\
     (forall v, CopyA.S.AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit False res).

(* ---- the application obligations, for a family application -------------------- *)

(* HYPOTHESIS (T15), LinkResultsOk at a family application: a linked body
   that completes provides the family's methods at the application. The
   premises are LinkResultsOk's, with the family application (at the family
   context, from a state realizing C0) in place of AppPremises. *)
Definition FamilyLinkResultsOk : Prop := forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 -> A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall rr F d types H g st fx h dc gfx gg dg n ps a r bodies Cu ns Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr' ks' binds0
    pins d1 d2 d3 g3 st3 H3 j',
  Pre rr F C0 d types H g st -> A.nlook u (A.c_funs C0) = Some fx ->
  lookup_provision (IFun u) d = Some (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  ClosureOk rr H u gfx (FunctorClosure h (Some (A.link_map Cu Cg linktarget nsg)) dc None) ->
  CopyA.S.FixedMeans W u d fx gfx ->
  CopyA.S.LinkData W Prog u gg h dg n ps a r bodies Cu ns Cg nsg Fg wrap Cb Fb made outs' gen Cr' ks' binds0 pins ->
  PublicContextFacts.ClosureBinding W u Cu ns Cg nsg linktarget dc gfx ->
  CopyA.S.PinsAgree W u Cu Cg dc gfx pins ->
  select_provisions d sm = Some d1 ->
  select_provisions d1 (A.link_map Cu Cg linktarget nsg) = Some d2 ->
  CopyA.S.AR.O.I.eval_bindings j' P (application_environment P d h th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> Pre rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr,
    select_provisions d3 (result_selection (map (A.rkey_of Cr) ks) None (CopyA.S.link_sources (A.link_site Cg Cb pins outs' gen) outs' gen ks')) = Some dr /\
    FamilyNewOk rr Cr ks d dr H3.

(* HYPOTHESIS (T15), CopyApplyOk and PlatformApplyOk at a family
   application: applying a copy runs its source at the source's interface,
   and applying a platform family installs its primitives. *)
Definition FamilyCopyApplyOk : Prop := forall k, Below k -> forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall F rr j d types H g st, rr <= S k -> S j <= rr -> Pre rr F C0 d types H g st ->
  forall fx gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh,
  A.nlook u (A.c_funs C0) = Some fx ->
  lookup_provision (IFun u) d = Some (FunctorClosure code (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 (target k))) nsh)) dc (compose_positions rms rm)) ->
  ClosureOk rr H h gfh (FunctorClosure code sms dc rms) ->
  A.FunCtx W u Cu0 nsu -> A.FunCtx W h Ch nsh -> CopyA.S.NeedsAmong target nsh nsu ->
  (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
  CopyA.S.FixedMeans W u d fx gfx ->
  FamilyStepOut rr F Cr ks u th sm j d types H g st.
(* HYPOTHESIS (T15): FamilyCopyApplyOk's realization half. Applying a copy
   at a family application completes with provisions dr (no type meanings,
   no aliases) under which each new method of Cr is callable at its
   signature, or stops. That dr fills only the run-time keys of Cr's new
   methods is true by construction (Audit.result_selection files position
   i of the family's result under the caller's key ks[i];
   apply_functor_keys), so FamilyCopyApplyOk follows
   (family_copy_apply_of_realize). The former key half was false
   (tests/wasi/family_copy_linked_bracket.moss). *)
Definition FamilyCopyRealizeOk : Prop := forall k, Below k -> forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall F rr j d types H g st, rr <= S k -> S j <= rr -> Pre rr F C0 d types H g st ->
  forall fx gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh,
  A.nlook u (A.c_funs C0) = Some fx ->
  lookup_provision (IFun u) d = Some (FunctorClosure code (compose_adaptation sms (map (fun k => (A.rkey_of Ch k, A.rkey_of Cu0 (target k))) nsh)) dc (compose_positions rms rm)) ->
  ClosureOk rr H h gfh (FunctorClosure code sms dc rms) ->
  A.FunCtx W u Cu0 nsu -> A.FunCtx W h Ch nsh -> CopyA.S.NeedsAmong target nsh nsu ->
  (forall x, A.Fixed W h x -> A.Fixed W u x /\ lookup_subst x gfh = lookup_subst x gfx) ->
  CopyA.S.FixedMeans W u d fx gfx ->
  (exists dr st' H', heap_extends H H' /\ state_ok H' st' /\ type_meanings dr = [] /\ type_aliases dr = [] /\
     (forall r m g, In (A.mkey r m) ks -> A.mlook r m (A.c_mths Cr) = Some g -> exists pv,
        lookup_provision (A.rkey Cr r m) dr = Some pv /\ CallableOk rr H' d (r :: A.sg_params g) (A.sg_ret g) pv) /\
     CopyA.S.AR.O.I.apply_functor j P d u th [] sm (map (A.rkey_of Cr) ks) st = inl (dr, st')) \/
  (forall rest, exists res, CopyA.S.AR.O.I.eval_bindings (S j) P d g st (BApply u th [] sm (map (A.rkey_of Cr) ks) :: rest) = inr res /\
     (forall v, CopyA.S.AR.O.I.evaluation_signal res <> Normal v) /\ Outcome F d H types TUnit False res).

(* An application installs only under the caller's keys it is given. *)
Lemma apply_functor_keys : forall j d u th outs sm rs st dr st',
  CopyA.S.AR.O.I.apply_functor j P d u th outs sm rs st = inl (dr, st') ->
  forall K pv, lookup_provision K dr = Some pv -> In K rs.
Proof.
  intros j d u th outs sm rs st dr st' E K pv L. destruct j as [|j]; [discriminate|]. cbn in E.
  repeat match type of E with context[match ?x with _ => _ end] =>
    match x with context[result_environment] => fail 1 | _ => destruct x eqn:?; try discriminate E end end.
  unfold result_environment in E. destruct (read_result_types _ _ outs) as [[w a]|]; [|discriminate].
  injection E as <- _.
  match goal with Hs : select_provisions _ (result_selection rs ?rms ?srcs) = Some ?dr0 |- _ =>
    change (lookup_provision K dr0 = Some pv) in L; rewrite (select_lookup _ _ _ K Hs) in L;
    destruct (sm_find K (result_selection rs rms srcs)) as [s|] eqn:Fs; [|discriminate];
    exact (proj1 (PublicCallerKeys.selection_in _ _ _ _ _ (PublicCallerKeys.sm_find_some_in _ _ _ Fs))) end.
Qed.

Lemma family_copy_apply_of_realize : FamilyCopyRealizeOk -> FamilyCopyApplyOk.
Proof.
  intros Rz k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0
    fx gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh Fx Look Okh FCu0 FCh Incl FxC Fm.
  destruct (Rz k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0
    fx gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh Fx Look Okh FCu0 FCh Incl FxC Fm)
    as [(dr & st' & H' & Ext & St' & Tm & Ta & New & Ap)|Stop]; intros rest; [left|right; exact (Stop rest)].
  exists dr, st', H'. split; [exact Ext|]. split; [exact St'|]. split.
  - split; [exact Tm|]. split; [exact Ta|]. split; [|exact New].
    intros K pv L. pose proof (apply_functor_keys _ _ _ _ _ _ _ _ _ _ Ap K pv L) as IK.
    apply in_map_iff in IK as [k1 [<- Ik1]].
    destruct (family_news W C u matching th sm Cr ks FA) as (news & _ & _ & _ & NewM & Keys).
    destruct (proj1 (Keys k1) Ik1) as [p1 Ip1].
    destruct (NewM k1 p1 Ip1) as (r1 & m1 & _ & _ & -> & _). exists r1, m1. split; [exact Ik1|reflexivity].
  - rewrite CopyA.S.bindings_apply_eq, Ap. reflexivity.
Qed.

Definition FamilyPlatformApplyOk : Prop := forall k, Below k -> forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall F rr j d types H g st, rr <= S k -> S j <= rr -> Pre rr F C0 d types H g st ->
  forall code, lookup_provision (IFun u) d = Some (FunctorClosure code None (EnvironmentWithAliases [] [] []) None) ->
  Platform.Family W Prog u code ->
  FamilyStepOut rr F Cr ks u th sm j d types H g st.

Hypothesis family_link_results : FamilyLinkResultsOk.
Hypothesis family_copy_realize : FamilyCopyRealizeOk.
Let family_copy_apply := family_copy_apply_of_realize family_copy_realize.
Hypothesis family_platform_apply : FamilyPlatformApplyOk.

(* ---- the run --------------------------------------------------------------- *)

Lemma family_run : forall k, Below k -> forall C0 C u matching th sm Cr ks,
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 -> A.c_locals C = A.c_locals C0 ->
  PublicFormation.TysScoped W C -> AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  forall F rr j d types H g st, rr <= S k -> S j <= rr -> Pre rr F C0 d types H g st ->
  FamilyStepOut rr F Cr ks u th sm j d types H g st.
Proof.
  intros k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0.
  pose proof FA as (Cu & ns & (fx & Fx & Fam & Poly & FC & Th & InF & FAg & TypedNeeds & PR & Nd) & EmittedNeeds).
  pose proof (conj EmittedNeeds TypedNeeds) as Sat.
  destruct (PublicFormation.family_context_shape matching C) as [Ety [EsubF [EprF EloF]]].
  set (Cf := A.family_context C matching) in *.
  pose proof Pre0 as [Frame [Ctx [Lt [St [Real Fmd]]]]].
  pose proof Real as [_ [[_ [_ [_ Funs]]] [Section KM]]].
  pose proof (PublicContextFacts.section_canonical C0 Section) as Idem.
  pose proof (CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ Real) as Abs.
  assert (FxC0 : A.nlook u (A.c_funs C0) = Some fx) by (unfold A.c_funs; rewrite <- Ep; exact Fx).
  destruct (Funs u fx FxC0) as [pv [gfx [Look [Ok Fm]]]].
  destruct (CopyA.S.closure_cases W Prog argv read state_ok _ _ _ _ _ Ok) as [CL|[CC|CP]].
  2: { destruct CC as (h & Cu0 & nsu & Ch & nsh & target & code & sms & dc & rms & rm & gfh & -> & Okh & FCu0 & FCh & Incl & FxC).
       exact (family_copy_apply k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0
         fx gfx h Cu0 nsu Ch nsh target code sms dc rms rm gfh FxC0 Look Okh FCu0 FCh Incl FxC Fm). }
  2: { destruct CP as (code & -> & PFam & _).
       exact (family_platform_apply k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0
         code Look PFam). }
  destruct CL as (gg & h & Cu0 & nsu & Cg & nsg & linktarget & dc & -> & [pins [LB [PinsOk Binding]]] & FCu0 & FCg & FxIn & Caps & CF).
  destruct (CopyA.S.functx_unique W _ _ _ _ _ FC FCu0) as [<- <-].
  intros rest. rewrite CopyA.S.bindings_apply_eq.
  destruct j as [|j1].
  { right. eexists. split; [reflexivity|]. split; [intros v E; discriminate|exact Logic.I]. }
  subst th. set (th := map (fun q => (fst q, A.canon Cf (app_subst (A.c_ty Cf) (TAbs (fst q))))) (A.c_ty Cu)) in *.
  (* the state realizes the family context: it differs from C0 only in its table *)
  assert (RealF : Realizes rr Cf d H)
    by (apply (CopyA.S.realizes_ty_free W Prog argv read state_ok rr C0 Cf d H); [rewrite EsubF; exact Es|unfold Cf; rewrite (PublicContextFacts.family_context_merge matching C); exact Emg|
      rewrite EprF; exact Ep|exact Real]).
  assert (Inp : forall t tt, A.nlook t (A.c_ty Cu) = Some tt -> exists here, A.nlook t (A.c_ty Cf) = Some here).
  { intros t tt L. apply nlook_in in L. destruct (A.nlook t (A.c_ty Cf)) as [here|] eqn:Lh; [exists here; reflexivity|].
    exfalso. exact (InF t (in_map fst _ _ L) Lh). }
  destruct (CopyA.S.static_realizes_le W Prog argv read state_ok rr rr Cf d H u Cu ns sm th (le_n _) RealF
    (CopyA.S.functx_static W _ _ _ FC) Sat (CopyA.S.apply_theta W Cf u Cu ns sm FC (proj1 (proj2 (proj2 RealF))) Inp Sat)) as [d1 [Sel [_ RealU]]].
  destruct (CopyA.S.link_facts W Prog _ _ _ _ LB) as (dg & n0 & ps0 & a0 & rs & bodies & Cu' & nsu' & Cg' & nsg' & Fg & wrap & Cb & Fb & made
    & outs' & gen & Cr' & ks' & binds0 & LD).
  pose proof LD as (FCu' & FCg' & Dg & Ast & Mod & Ret & This & Loop & Num & Items & OutsI & OutM & NdG & FrG & PR' & RR & BO & FB & _).
  destruct (CopyA.S.functx_unique W _ _ _ _ _ FC FCu') as [<- <-]. destruct (CopyA.S.functx_unique W _ _ _ _ _ FCg FCg') as [<- <-].
  (* the closure's own needs, from those the caller selected *)
  destruct (select_ok d1 (A.link_map Cu Cg linktarget nsg)) as [d2 Sel2].
  { intros q Iq. apply in_map_iff in Iq as [k0 [<- Ik]]. cbn [snd].
    pose proof (CopyA.S.linked_targets_in W u Cu ns Cg nsg linktarget _ _ Binding k0 Ik) as Iu.
    destruct (functx_needs_provided W u Cu ns FC (A.linked_target linktarget k0) Iu) as [p0 Ip].
    destruct (CopyA.S.functx_static W _ _ _ FC) as [_ [_ [_ [_ [ShU _]]]]].
    destruct (CopyA.S.realized_key W Prog argv read state_ok _ _ _ _ _ _ RealU Ip (ShU (A.linked_target linktarget k0) Iu)) as [pv0 L0].
    change (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) d1) with (lookup_provision (A.rkey_of Cu (A.linked_target linktarget k0)) (CopyA.S.callee_env d th d1)). rewrite L0. discriminate. }
  assert (Keys : map fst th = map fst (A.c_ty Cu)) by (unfold th; rewrite map_map; reflexivity).
  (* F's fixed inputs: θ reads them in the family context as the closure recorded them (FixedAgreement) *)
  assert (KMf : PublicCallRealize.KeysMatch (A.c_provisions Cf)) by (rewrite EprF, Ep; exact KM).
  assert (FxF : A.nlook u (A.c_funs Cf) = Some fx) by (unfold A.c_funs; rewrite EprF; exact Fx).
  assert (GfxA : forall x, A.Fixed W u x -> lookup_subst x gfx = option_map (ground d) (lookup_subst x th)).
  { intros x Fx'. destruct Fm as [Cov Vm]. destruct (Cov x Fx') as [tx Ixt]. rewrite (Vm x tx Ixt).
    assert (Ix : In x (map fst (A.c_ty Cu))) by exact (FxIn x Fx').
    assert (Lx : lookup_subst x th = Some (A.canon Cf (app_subst (A.c_ty Cf) (TAbs x))))
      by (unfold th; exact (CopyA.S.lookup_on_keys (fun x => A.canon Cf (app_subst (A.c_ty Cf) (TAbs x))) _ x Ix)).
    rewrite Lx. cbn [option_map]. f_equal.
    destruct (A.nlook x (A.c_ty Cf)) as [now|] eqn:Ln; [|exfalso; exact (InF x Ix Ln)].
    replace (app_subst (A.c_ty Cf) (TAbs x)) with now by (cbn [app_subst]; rewrite lookup_subst_nlook', Ln; reflexivity).
    destruct (CopyA.S.nlook_fixed_app fx x (Cov x Fx')) as [was [Nw Iw]].
    pose proof (FAg x Fx') as Ag.
    rewrite (CopyA.S.ilook_apps_fun Cf u fx (funs_ilook _ u fx KMf FxF)), Ln, Nw in Ag.
    assert (E1 : Some (ground d tx) = Some (ground d was)) by (rewrite <- (Vm x tx Ixt), <- (Vm x was Iw); reflexivity).
    injection E1 as E1. rewrite E1.
    apply (proj1 RealF). eapply A.METrans; [exact Ag|].
    apply PublicMerge.merge_eq_read. exact (proj1 (proj1 (proj2 (proj2 RealF)))). }
  pose proof (CopyA.S.link_entry W Prog argv read state_ok rr H d th d1 u gg h pins Cu ns Cg nsg linktarget dc gfx d2
    LB Binding FC FCg Keys FxIn RealU GfxA Caps CF Sel2) as RealG.
  set (de := application_environment P d h th (append_environment d2 dc)) in *.
  assert (PreB : Pre rr Fg Cg de [] H [] st).
  { split; [apply (numbering_frame Fg []); rewrite This; exact Num|].
    split; [split; [|split]|split; [apply Forall2_nil|split; [exact St|split; [exact RealG|]]]].
    4: { split; [exact (PublicFormation.functx_cx_formed W mentions_typed gg Cg nsg FCg)|].
         split; [split; [rewrite Ret; apply PublicFormation.optok_none|rewrite This; apply PublicFormation.optok_none]|].
         apply (PublicScoping.functx_cxscoped W gg Cg nsg Fg FCg). intros t0 E0. rewrite This in E0. discriminate. }
    - intros n1 t mu Found. rewrite (CopyA.S.functx_locals W _ _ _ FCg) in Found. discriminate.
    - intros t E. congruence.
    - apply CopyA.S.alias_self; [reflexivity|exact (CopyA.S.realizes_absorbs_canon W Prog argv read state_ok _ _ _ _ RealG)| |].
      + destruct (functx_scoped W gg Cg nsg FCg) as [CanG _]. exact CanG.
      + intros k0 v0 L0. unfold de, application_environment. change (view_functor P h) with (A.funbody_of Prog h).
        rewrite FB. cbn [type_meanings fc_inputs]. rewrite CopyA.S.lookup_map_nat; [discriminate|].
        change k0 with (fst (k0, v0)). apply in_map. exact (nlook_in _ _ _ _ L0). }
  pose proof (CopyA.S.case_BApplyAbstract W Prog argv read state_ok mentions_typed family_sig_ok all_fn_elab World
    captured_in_force link_results copy_apply platform_apply k Bel (S j1)) as Ap.
  destruct (CopyA.S.items_chain W Prog argv read state_ok mentions_typed family_sig_ok all_fn_elab World
    captured_in_force (S j1) k Bel Ap _ _ _ _ _ _ _ Items) as [bs [Ew Ch]].
  pose proof (Ew EUnit) as E0. rewrite E0 in BO. apply CopyA.S.bindsof_fold in BO. subst binds0.
  rewrite CopyA.S.apply_functor_eq, Look. change (view_functor P h) with (A.funbody_of Prog h). rewrite FB.
  cbn [fc_bindings fc_outputs fc_sources]. rewrite Sel. cbn [adapt_provisions]. rewrite Sel2. fold de.
  destruct (CopyA.S.chain_run W Prog argv read state_ok (S j1) k Fg Cg Fb Cb _ made bs Ch rr j1 de [] H [] st [] Lr ltac:(lia) ltac:(lia) PreB)
    as [[d3 [g3 [st3 [H3 [j' [_ [E3 [_ [Pre3 Run0]]]]]]]]]|[res [Run [NN Out]]]];
    [rewrite app_nil_r in Run0|rewrite app_nil_r in Run].
  - destruct j' as [|j'].
    { rewrite Run0. right. exists (CopyA.S.AR.O.I.with_g g (CopyA.S.AR.O.I.Evaluation OutOfFuel g3 st3)). split; [reflexivity|].
      exact (CopyA.S.body_stop W Prog argv read state_ok Fg de H [] TUnit False False F d H types g (CopyA.S.AR.O.I.Evaluation OutOfFuel g3 st3)
        Ret Loop ltac:(intros v E; discriminate) Logic.I). }
    assert (Run : CopyA.S.AR.O.I.eval_bindings j1 P de [] st bs = inl (d3, g3, st3)) by (rewrite Run0; reflexivity).
    rewrite Run.
    destruct (family_link_results C0 C u matching th sm Cr ks Es Emg Ep El Ts FA rr F d types H g st fx h dc gfx gg dg n0 ps0 a0 rs bodies
      Cu ns Cg nsg linktarget Fg wrap Cb Fb made outs' gen Cr' ks' bs pins d1 d2 d3 g3 st3 H3 j1 Pre0 FxC0 Look Ok Fm LD Binding PinsOk Sel Sel2 Run E3 Pre3)
      as [dr [Sdr New]].
    rewrite Sdr. left. exists (EnvironmentWithAliases [] [] (provision_values dr)), st3, H3.
    split; [exact E3|]. split; [exact (proj1 (proj2 (proj2 (proj2 Pre3))))|].
    split; [split; [reflexivity|split; [reflexivity|exact New]]|reflexivity].
  - rewrite Run. right. exists (CopyA.S.AR.O.I.with_g g res). split; [reflexivity|].
    exact (CopyA.S.body_stop W Prog argv read state_ok Fg de H [] TUnit _ False F d H types g res Ret Loop NN Out).
Qed.

(* ---- the contexts after it ----------------------------------------------------- *)

(* The family's provisions, with the code's table: realized. *)
Lemma realizes_family : forall rr C0 C u matching th sm Cr ks d dr H H',
  A.c_sub C = A.c_sub C0 -> A.c_merge C = A.c_merge C0 ->
  A.c_provisions C = A.c_provisions C0 ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  Realizes rr C0 d H -> heap_extends H H' -> FamilyResultOk rr Cr ks d dr H' ->
  Realizes rr (CopyA.S.with_ty C0 Cr) (append_environment dr d) H'.
Proof.
  intros rr C0 C u matching th sm Cr ks d dr H H' Es Emg Ep FA Real Ext [Em [Ea [Dom New]]].
  destruct (family_news W _ _ _ _ _ _ _ FA) as [news [Epr [Esub [_ [Meth KsI]]]]].
  destruct (CopyA.S.realizes_extends W Prog argv read state_ok rr C0 d H H' Ext Real) as [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]].
  set (d' := append_environment dr d).
  assert (G : forall t, ground d' t = ground d t)
    by (intros t; unfold ground, d'; cbn [type_meanings append_environment]; rewrite Em; reflexivity).
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  assert (LookD : forall K, (forall r m, In (A.mkey r m) ks -> K <> A.rkey Cr r m) -> lookup_provision K d' = lookup_provision K d).
  { intros K NotK. unfold d'. rewrite CopyA.S.lookup_append_env. destruct (lookup_provision K dr) as [pv|] eqn:L; [|reflexivity].
    destruct (Dom K pv L) as [r [m [Ik ->]]]. exfalso. exact (NotK r m Ik eq_refl). }
  assert (NotM : forall K, (forall r m th, K <> IMth r m th) -> forall r m, In (A.mkey r m) ks -> K <> A.rkey Cr r m).
  { intros K Nm r m _ E. exact (Nm r m _ E). }
  set (Cw := CopyA.S.with_ty C0 Cr).
  assert (Epw : A.c_provisions Cw = (news ++ A.c_provisions C0)%list) by (unfold Cw; cbn; rewrite Epr, Ep; reflexivity).
  assert (Esw : A.c_sub Cw = A.c_sub C0) by (unfold Cw; cbn; rewrite Esub, Es; reflexivity).
  assert (CanonEq : forall t, A.canon Cw t = A.canon C0 t) by (intros t; unfold A.canon; rewrite Esw; reflexivity).
  assert (KMn : PublicCallRealize.KeysMatch news).
  { intros k p I. destruct (Meth k p I) as (r & m & g & a & -> & ->). exact Logic.I. }
  assert (KMw : PublicCallRealize.KeysMatch (A.c_provisions Cw)).
  { rewrite Epw. intros k p I. apply in_app_or in I as [I|I]; [exact (KMn k p I)|exact (KM k p I)]. }
  assert (MergeW : A.c_merge Cw = A.c_merge C0).
  { unfold Cw. change (A.c_merge Cr = A.c_merge C0).
    rewrite (PublicContextFacts.family_application_merge W _ _ _ _ _ _ _ FA). exact Emg. }
  split; [intros left right Eq; rewrite !G; apply Abs; now rewrite MergeW in Eq |].
  split; [|split; [|exact KMw]].
  2: { eapply PublicContextFacts.section_same_fields; eassumption. }
  split; [|split; [|split]].
  - intros v t Found. change (A.nlook v (vals_of (A.c_provisions Cw)) = Some t) in Found.
    rewrite Epw in Found. unfold vals_of in Found. rewrite flat_map_app, (flat_map_methods_nil _ _ news Meth) in Found by reflexivity.
    destruct (Vals v t Found) as [w [L T]]. exists w. rewrite G, LookD; [exact (conj L T)|].
    apply NotM. intros r m th0 E. discriminate.
  - intros f sg Found. change (A.nlook f (fns_of (A.c_provisions Cw)) = Some sg) in Found.
    rewrite Epw in Found. unfold fns_of in Found. rewrite flat_map_app, (flat_map_methods_nil _ _ news Meth) in Found by reflexivity.
    destruct (Fns f sg Found) as [pv [L Ok]]. exists pv. rewrite LookD; [|apply NotM; intros r m th0 E; discriminate].
    split; [exact L|]. exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv (le_n _) (Gm _) (eq_sym (G _)) Ok).
  - intros r m sg Found. change (A.mlook r m (mths_of (A.c_provisions Cw)) = Some sg) in Found.
    rewrite Epw in Found. unfold mths_of in Found. rewrite flat_map_app, mlook_app in Found. fold (mths_of news) in Found.
    destruct (A.mlook r m (mths_of news)) as [sg'|] eqn:Ln.
    + injection Found as <-.
      destruct (mlook_mths_in _ _ _ _ Ln) as [th0 [a0 I]]. destruct (Meth _ _ I) as (r1 & m1 & g1 & a1 & E1 & E2).
      unfold A.mkey in E1. injection E1 as Er Emm Et. subst r1 m1 th0.
      assert (Ik : In (A.mkey r m) ks) by (apply KsI; eexists; exact I).
      assert (Fc : A.mlook r m (A.c_mths Cr) = Some sg').
      { change (A.mlook r m (mths_of (A.c_provisions Cr)) = Some sg'). rewrite Epr. unfold mths_of.
        rewrite flat_map_app, mlook_app. fold (mths_of news). rewrite Ln. reflexivity. }
      destruct (New r m sg' Ik Fc) as [pv [L Ok]]. exists pv.
      change (A.rkey Cw r m) with (A.rkey Cr r m). unfold d'. rewrite CopyA.S.lookup_append_env, L.
      split; [reflexivity|].
      exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv (le_n _) (Gm _) (eq_sym (G _)) Ok).
    + destruct (Mths r m sg Found) as [pv [L Ok]]. exists pv.
      assert (NoNew : forall p, ~ In (A.mkey r m, p) news).
      { intros p I. destruct (Meth _ _ I) as (r1 & m1 & g1 & a1 & _ & ->). exact (mlook_mths_some _ _ _ _ _ I Ln). }
      assert (Rk : A.rkey Cw r m = A.rkey C0 r m).
      { unfold A.rkey. rewrite !CopyA.S.capps_ilook, Epw, (ilook_app_none _ _ _ _ NoNew).
        unfold A.theta_of, A.canon. rewrite Esw. reflexivity. }
      rewrite Rk, LookD.
      * split; [exact L|]. exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv (le_n _) (Gm _) (eq_sym (G _)) Ok).
      * intros r' m' Ik E. unfold A.rkey in E. injection E as <- <- _.
        destruct (proj1 (KsI _) Ik) as [p Ip]. exact (NoNew p Ip).
  - intros u' fx Found. change (A.nlook u' (funs_of (A.c_provisions Cw)) = Some fx) in Found.
    rewrite Epw in Found. unfold funs_of in Found. rewrite flat_map_app, (flat_map_methods_nil _ _ news Meth) in Found by reflexivity.
    destruct (Funs u' fx Found) as [pv [gfx [L [Ok [Cov Vm]]]]]. exists pv, gfx.
    rewrite LookD; [|apply NotM; intros r m th0 E; discriminate].
    split; [exact L|]. split; [exact Ok|]. split; [exact Cov|]. intros x tx I. rewrite G. exact (Vm x tx I).
Qed.

(* Any context among C0's provisions stays realized, given that a family
   provision shadowing one of C0's at its run-time key has its signature. *)
Lemma keep_family : forall rr C0 C u matching th sm Cr ks d dr H H',
  A.c_sub C = A.c_sub C0 -> A.c_provisions C = A.c_provisions C0 ->
  AuditFacts.FamilyApplication W C u matching th sm Cr ks ->
  heap_extends H H' -> FamilyResultOk rr Cr ks d dr H' ->
  forall Cx, incl (A.c_provisions Cx) (A.c_provisions C0) -> A.c_sub Cx = A.c_sub C0 ->
  CopyA.S.FamilyCoherent Cx Cr ks -> Realizes rr Cx d H -> Realizes rr Cx (append_environment dr d) H'.
Proof.
  intros rr C0 C u matching th sm Cr ks d dr H H' Es Ep FA Ext [Em [Ea [Dom New]]] Cx Inc Esx Coh RealX.
  destruct (family_news W _ _ _ _ _ _ _ FA) as [news [Epr [Esub [_ [Meth KsI]]]]].
  destruct (CopyA.S.realizes_extends W Prog argv read state_ok rr Cx d H H' Ext RealX) as [Abs [[Vals [Fns [Mths Funs]]] [Section KM]]].
  set (d' := append_environment dr d).
  assert (G : forall t, ground d' t = ground d t)
    by (intros t; unfold ground, d'; cbn [type_meanings append_environment]; rewrite Em; reflexivity).
  assert (Gm : forall ps, map (ground d) ps = map (ground d') ps) by (intros ps; apply map_ext; intros; symmetry; apply G).
  assert (LookN : forall K, (forall r m th, K <> IMth r m th) -> lookup_provision K d' = lookup_provision K d).
  { intros K Nm. unfold d'. rewrite CopyA.S.lookup_append_env. destruct (lookup_provision K dr) as [pv|] eqn:L; [|reflexivity].
    destruct (Dom K pv L) as [r [m [_ ->]]]. exfalso. exact (Nm r m _ eq_refl). }
  assert (KMn : PublicCallRealize.KeysMatch news).
  { intros k p I. destruct (Meth k p I) as (r & m & g & a & -> & ->). exact Logic.I. }
  split; [intros left right Eq; rewrite !G; exact (Abs left right Eq)|]. split; [|exact (conj Section KM)].
  split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [L T]]. exists w.
    rewrite G, LookN; [exact (conj L T)|intros r m th0 E; discriminate].
  - intros f sg Found. destruct (Fns f sg Found) as [pv [L Ok]]. exists pv.
    rewrite LookN; [|intros r m th0 E; discriminate]. split; [exact L|].
    exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv (le_n _) (Gm _) (eq_sym (G _)) Ok).
  - intros r m sg Found. destruct (Mths r m sg Found) as [pv [L Ok]].
    destruct (lookup_provision (A.rkey Cx r m) dr) as [pv'|] eqn:Ld.
    2: { exists pv. unfold d'. rewrite CopyA.S.lookup_append_env, Ld. split; [exact L|].
         exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv (le_n _) (Gm _) (eq_sym (G _)) Ok). }
    destruct (Dom _ _ Ld) as [r' [m' [Ik Ek]]].
    pose proof Ek as Ek0. unfold A.rkey at 1 2 in Ek. injection Ek as Er Emm Eth. subst r' m'.
    (* the family provision at that key *)
    destruct (proj1 (KsI _) Ik) as [p0 Ip0]. destruct (ilook_of_in _ _ _ _ Ip0) as [p1 [Ln Ip1]].
    destruct (Meth _ _ Ip1) as (r1 & m1 & g1 & a1 & _ & ->).
    assert (Lr : A.ilook (A.mkey r m) (A.c_provisions Cr) = Some (A.ProvidedMethod g1 a1))
      by (rewrite Epr; exact (ilook_app_some _ _ _ _ _ Ln)).
    assert (Fc : A.mlook r m (A.c_mths Cr) = Some g1).
    { change (A.mlook r m (mths_of (A.c_provisions Cr)) = Some g1). rewrite Epr. unfold mths_of.
      rewrite flat_map_app, mlook_app. fold (mths_of news). pose proof (ilook_mths news r m g1 a1 KMn Ln) as Mn. change (A.mlook r m (mths_of news) = Some g1) in Mn. rewrite Mn. reflexivity. }
    destruct (New r m g1 Ik Fc) as [pv1 [L1 Ok1]].
    rewrite <- Ek0, Ld in L1. injection L1 as <-.
    (* C0's provision there, and its θ *)
    destruct (mths_ilook (A.c_provisions Cx) r m sg KM Found) as [ax Lx].
    rewrite !CopyA.S.capps_ilook, Lx, Lr in Eth. cbn [option_map A.provision_inst] in Eth.
    pose proof (Coh r m sg ax g1 a1 Ik (CopyA.S.ilook_in _ _ _ Lx) Lr Eth) as <-.
    exists pv'. unfold d'. rewrite CopyA.S.lookup_append_env, Ld. split; [reflexivity|].
    exact (CopyA.S.callable_transfer W Prog argv read state_ok rr rr H' d d' _ _ _ _ pv' (le_n _) (Gm _) (eq_sym (G _)) Ok1).
  - intros u' fx Found. destruct (Funs u' fx Found) as [pv [gfx [L [Ok [Cov Vm]]]]]. exists pv, gfx.
    rewrite LookN; [|intros r m th0 E; discriminate].
    split; [exact L|]. split; [exact Ok|]. split; [exact Cov|]. intros x tx I. rewrite G. exact (Vm x tx I).
Qed.

(* Pre at a context with C0's table and locals, from Pre at C0, once its
   provisions are realized and it is formed. *)
Lemma pre_same_table : forall rr F C0 Cw d d' types H H' g st st',
  Pre rr F C0 d types H g st -> type_meanings d' = type_meanings d -> type_aliases d' = type_aliases d ->
  A.c_ty Cw = A.c_ty C0 -> A.c_locals Cw = A.c_locals C0 ->
  Realizes rr Cw d' H' -> heap_extends H H' -> state_ok H' st' -> PublicFormation.FormedCx W Cw ->
  PublicScoping.CxScoped W F Cw -> Pre rr F Cw d' types H' g st'.
Proof.
  intros rr F C0 Cw d d' types H H' g st st' [Frame [[Lo [Re Al]] [L [St [Real [Fc [Ff _]]]]]]] Em Ea Ety Elo Real' Ext St' Fw Cw'.
  assert (G : forall t, ground d' t = ground d t) by (intros t; unfold ground; rewrite Em; reflexivity).
  split; [exact Frame|]. split; [split; [|split]|split; [exact (locals_extends W P H H' g types Ext L)|split; [exact St'|split; [exact Real'|exact (conj Fw (conj Ff Cw'))]]]].
  - intros n t mu Found Nm. rewrite Elo in Found. rewrite G. exact (Lo n t mu Found Nm).
  - intros t Th. rewrite G. exact (Re t Th).
  - intros x tx Lx. rewrite Ety in Lx. unfold cmeaning, output_meaning. rewrite Ea, Em. rewrite G. exact (Al x tx Lx).
Qed.

(* ---- FamilyStepOk -------------------------------------------------------------- *)

Theorem family_step_ok : CopyA.S.FamilyStepOk W Prog argv read state_ok.
Proof.
  intros k Bel C0 C u matching th sm Cr ks ot m Es Emg Ep El FA Ro Ok Sc Tin Ts F rr j d types H g st Lr Lj Pre0 rest.
  destruct (family_run k Bel C0 C u matching th sm Cr ks Es Emg Ep El Ts FA F rr j d types H g st Lr Lj Pre0 rest)
    as [[dr [st' [H' [E [St' [Res Run]]]]]]|Stop]; [left|right; exact Stop].
  pose proof Res as [Em [Ea _]].
  destruct (family_news W _ _ _ _ _ _ _ FA) as [news [Epr [Esub [Elo _]]]].
  assert (Em' : type_meanings (append_environment dr d) = type_meanings d) by (cbn; rewrite Em; reflexivity).
  assert (Ea' : type_aliases (append_environment dr d) = type_aliases d) by (cbn; rewrite Ea; reflexivity).
  pose proof Pre0 as [_ [_ [_ [_ [Real0 [Fc0 [Ff0 Cs0]]]]]]].
  exists (append_environment dr d), st', H'.
  split; [exact E|]. split; [exact Em'|]. split; [exact Ea'|].
  split; [|split; [exact (keep_family rr C0 C u matching th sm Cr ks d dr H H' Es Ep FA E Res)|exact Run]].
  apply (pre_same_table rr F C0 (CopyA.S.with_ty C0 Cr) d (append_environment dr d) types H H' g st st' Pre0 Em' Ea'
    eq_refl); [cbn; rewrite Elo; exact El|exact (realizes_family rr C0 C u matching th sm Cr ks d dr H H' Es Emg Ep FA Real0 E Res)
    |exact E|exact St'| |].
  - exact (family_formed W C0 C u matching th sm Cr ks ot m Es Emg Ep El FA Ro Ok Sc Tin Fc0).
  - exact (PublicScoping.family_result_cxscoped W F C0 C u matching th sm Cr ks ot m Es Emg Ep El FA Ro Ok Sc
      (fun x v I => proj2 (Tin x v I)) Cs0).
Qed.

End Fam.
End FamilyStep.
