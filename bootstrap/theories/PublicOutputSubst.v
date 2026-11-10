(* Two readings of one functor result (OutputItemsEntries) at two type
   tables related by substitutions s1 and s2 (PublicTypeRead.Agree: where
   both tables define a symbol, its meanings agree after s1 and s2). Their
   entries correspond one to one: the same symbols, and every type,
   signature and recorded instantiation equal after s1 and s2.

   Stated for any two tables, so it serves both a linked body's formal
   result against the caller's (LinkResultsOk: output_type_map Cu outs'
   (TAbs o gen) against output_type_map C outs (TAbs o gensof), outputs
   included or not) and a platform family's own result against the
   caller's (PlatformResultAgrees).

   What it does not give where the two tables' domains differ:
   - a recorded instantiation's requirement that one table has and the
     other lacks (Some/None) is left unrelated (InstRel only relates
     Some/Some);
   - a method's receiver symbol present in one table and absent from the
     other is read differently by RecvTy (a meaning versus a nominal or
     alias reading); such an entry is related only by its method and
     instantiation, with the symbol exhibited (DomGap). With tables of one
     domain (DomSame), DomGap is impossible. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section OutputSubst.
Variable W : A.world.
Variables s1 s2 : subst.
Import A.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  repeat match goal with
  | H1 : w_decl W ?x = Some ?a, H2 : w_decl W ?x = Some ?b |- _ =>
      assert_fails (constr_eq a b); rewrite H1 in H2; injection H2 as H2; subst b
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Notation Agree := (PublicTypeRead.Agree s1 s2).
Notation Rel := (PublicTypeRead.Rel s1 s2).

Definition TyRel (t1 t2 : ty) : Prop := app_subst s1 t1 = app_subst s2 t2.
Definition SigRel (g1 g2 : fsig) : Prop :=
  map (app_subst s1) (sg_params g1) = map (app_subst s2) (sg_params g2) /\ TyRel (sg_ret g1) (sg_ret g2).
Definition InstRel (a1 a2 : inst) : Prop :=
  Forall2 (fun q1 q2 => fst q1 = fst q2 /\
    forall x1 x2, snd q1 = Some x1 -> snd q2 = Some x2 -> TyRel x1 x2) a1 a2.
Definition FxRel (fx1 fx2 : subst) : Prop :=
  forall x t1 t2, In (x, t1) fx1 -> In (x, t2) fx2 -> TyRel t1 t2.

(* A symbol one base table has and the other lacks. *)
Definition DomGap (tm1 tm2 : list (nat * ty)) : Prop :=
  exists r, ~ (nlook r tm1 = None <-> nlook r tm2 = None).
Definition DomSame (tm1 tm2 : list (nat * ty)) : Prop :=
  forall r, nlook r tm1 = None <-> nlook r tm2 = None.

Lemma domsame_nogap : forall tm1 tm2, DomSame tm1 tm2 -> ~ DomGap tm1 tm2.
Proof. intros tm1 tm2 D [r N]. exact (N (D r)). Qed.

Definition EntryRel (tm1 tm2 : list (nat * ty)) (e1 e2 : entry) : Prop :=
  match e1, e2 with
  | NTy t1, NTy t2 => t1 = t2
  | NVal v1 t1 a1, NVal v2 t2 a2 => v1 = v2 /\ TyRel t1 t2 /\ InstRel a1 a2
  | NFn f1 g1 a1, NFn f2 g2 a2 => f1 = f2 /\ SigRel g1 g2 /\ InstRel a1 a2
  | NMth r1 m1 g1 a1, NMth r2 m2 g2 a2 => m1 = m2 /\ InstRel a1 a2 /\
      ((TyRel r1 r2 /\ SigRel g1 g2) \/ DomGap tm1 tm2)
  | NFun u1 fx1, NFun u2 fx2 => u1 = u2 /\ FxRel fx1 fx2
  | _, _ => False
  end.

Lemma select_det : forall X (P : X -> Prop) l a b, Select P l a -> Select P l b -> a = b.
Proof.
  intros X P l a b Sa. revert b. induction Sa as [|x xs sel Px S IH|x xs sel Nx S IH]; intros b Sb;
    inversion Sb; subst; try reflexivity; try contradiction; f_equal; auto.
Qed.

Lemma appof_rel : forall s T1 T2 a1 a2, AppOf W s T1 a1 -> AppOf W s T2 a2 -> Agree T1 T2 -> InstRel a1 a2.
Proof.
  intros s T1 T2 a1 a2 [rq1 [E1 K1]] [rq2 [E2 K2]] Ag.
  assert (Rq : rq1 = rq2).
  { destruct K1 as [[M1 S1]|[M1 S1]]; destruct K2 as [[M2 S2]|[M2 S2]];
      [|congruence|congruence|congruence].
    destruct S1 as (sel1 & ex1 & S1 & O1 & ->). destruct S2 as (sel2 & ex2 & S2 & O2 & ->).
    assert (Ee : ex1 = ex2) by (eapply sig_outside_det; eassumption).
    rewrite (select_det _ _ _ _ _ S1 S2), Ee. reflexivity. }
  subst a1 a2 rq2. unfold InstRel. clear K1 K2. induction rq1 as [|r rest IH]; cbn; [constructor|constructor; [|exact IH]].
  split; [reflexivity|]. intros x1 x2 L1 L2. exact (Ag r x1 x2 L1 L2).
Qed.

Lemma obr_rel : forall p T1 T2 a bs1 bs2, OutputBrackets W p T1 a bs1 -> OutputBrackets W p T2 a bs2 ->
  Agree T1 T2 -> Forall2 Rel bs1 bs2.
Proof.
  intros p T1 T2 a bs1 bs2 D1 D2 Ag. destruct D1 as [|bs l F]; inversion D2; subst; clear D2; [constructor|].
  apply forall2_rev.
  match goal with H0 : Forall2 _ bs ?l0 |- _ => rename H0 into F2; rename l0 into l2' end.
  revert l2' F2. induction F as [|b0 tb bs' l1' (sp & sa & E1 & R1 & K1 & T1') Fr IH]; intros l2' F2;
    inversion F2 as [|? tb2 ? l2'' (sp' & sa' & E2 & R2 & K2 & T2') Fr2]; subst; constructor; [|apply IH; assumption].
  rewrite E1 in E2. injection E2 as <- <-. split; [congruence|].
  exact (tyof_rel W s1 s2 _ _ _ _ _ T1' _ _ _ T2' Ag (fun _ _ E _ => ltac:(discriminate E))).
Qed.

Lemma recv_rel : forall r T1 T2 rt1 rt2, RecvTy W r T1 rt1 -> RecvTy W r T2 rt2 -> Agree T1 T2 ->
  (nlook r T1 = None <-> nlook r T2 = None) -> TyRel rt1 rt2.
Proof.
  intros r T1 T2 rt1 rt2 R1 R2 Ag Dom. unfold RecvTy in R1, R2. unfold TyRel.
  destruct (nlook r T1) as [v1|] eqn:L1; destruct (nlook r T2) as [v2|] eqn:L2.
  - subst. exact (Ag r v1 v2 L1 L2).
  - exfalso. destruct Dom as [_ D]. discriminate (D eq_refl).
  - exfalso. destruct Dom as [D _]. discriminate (D eq_refl).
  - assert (Nom : forall args1 args2,
      Forall2 (fun x xa => fst xa = x /\ nlook x T1 = Some (snd xa)) (w_reqs W r) args1 ->
      Forall2 (fun x xa => fst xa = x /\ nlook x T2 = Some (snd xa)) (w_reqs W r) args2 ->
      app_subst s1 (TNom r args1) = app_subst s2 (TNom r args2)).
    { clear R1 R2 Dom. intros args1 args2 F1 F2. rewrite !subst_nom. f_equal. apply smap_rel.
      revert args2 F2. induction F1 as [|x xa xs xas [Ea La] Fr IH]; intros args2 F2;
        inversion F2 as [|? xb ? xbs [Eb Lb] Fr2]; subst; constructor; [|apply IH; assumption].
      split; [congruence|]. exact (Ag _ _ _ La Lb). }
    destruct (w_decl W r) as [d|].
    + destruct (d_ast d); try (destruct R1 as [_ [a1 [-> F1]]]; destruct R2 as [_ [a2 [-> F2]]]; exact (Nom a1 a2 F1 F2)).
      exact (tyof_rel W s1 s2 _ _ _ _ _ R1 _ _ _ R2 Ag (fun _ _ E _ => ltac:(discriminate E))).
    + destruct R1 as [a1 [-> F1]]. destruct R2 as [a2 [-> F2]]. exact (Nom a1 a2 F1 F2).
Qed.

Lemma prefix_dom : forall l1 l2 T1 T2 r, Forall2 Rel l1 l2 ->
  ((nlook r T1 = None <-> nlook r T2 = None) -> (nlook r (l1 ++ T1) = None <-> nlook r (l2 ++ T2) = None)).
Proof.
  intros l1 l2 T1 T2 r F D. rewrite !nlook_app'. destruct (rel_nlook s1 s2 l1 l2 F r) as [S _].
  destruct (nlook r l1) eqn:K1; destruct (nlook r l2) eqn:K2.
  - split; discriminate.
  - exfalso. destruct S as [_ S']. discriminate (S' eq_refl).
  - exfalso. destruct S as [S' _]. discriminate (S' eq_refl).
  - exact D.
Qed.

Ltac flat := repeat match goal with
  | H : _ \/ _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  | H : exists _, _ |- _ => destruct H end.

Theorem output_entries_rel :
  (forall p ob1 tm1 sp es1 tm1', OutputItemEntries W p ob1 tm1 sp es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' ->
     Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> Forall2 (EntryRel tm1 tm2) es1 es2) /\
  (forall p ob1 tm1 sps es1 tm1', OutputItemsEntries W p ob1 tm1 sps es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' ->
     Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> Forall2 (EntryRel tm1 tm2) es1 es2).
Proof.
  apply (PublicFormation.oies_mut W
    (fun p ob1 tm1 sp es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' ->
       Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> Forall2 (EntryRel tm1 tm2) es1 es2)
    (fun p ob1 tm1 sps es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' ->
       Forall2 Rel ob1 ob2 -> Agree tm1 tm2 -> Forall2 (EntryRel tm1 tm2) es1 es2)).
  - (* type *)
    intros p ob tm pa a bs t R K B ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    constructor; [|constructor]. cbn. congruence.
  - (* nothing *)
    intros p ob tm pa a bs s k R K B Kd ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; first [apply Forall2_nil|flat; kmis].
  - (* val *)
    intros p ob tm a pa s d n t ty0 bs ap R Dd Ast B T App ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' s); assert (s' = s) by congruence; subst s' end.
    match goal with H : w_decl W s = Some ?d' |- _ => assert_fails (constr_eq d' d); assert (d' = d) by congruence; subst d' end.
    rewrite Ast in *. match goal with H : ADVal _ _ _ = ADVal _ _ _ |- _ => injection H as <- <- end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (obr_rel _ _ _ _ _ _ B H Ag1) as Bs end.
    pose proof (agree_app s1 s2 _ _ _ _ Bs Ag1) as Ag2.
    constructor; [|constructor]. cbn. split; [reflexivity|]. split.
    + match goal with H : TyOf W _ _ None t ?t2 |- _ => exact (tyof_rel W s1 s2 _ _ _ _ _ T _ _ _ H Ag2 (fun _ _ E _ => ltac:(discriminate E))) end.
    + match goal with H : AppOf W s _ ?a2 |- _ => exact (appof_rel _ _ _ _ _ App H Ag2) end.
  - (* fn *)
    intros p ob tm a pa s g bs ap d rcv n ps rt R K Dd Ast B Sig App ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' s); assert (s' = s) by congruence; subst s' end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (obr_rel _ _ _ _ _ _ B H Ag1) as Bs end.
    pose proof (agree_app s1 s2 _ _ _ _ Bs Ag1) as Ag2.
    constructor; [|constructor]. cbn. split; [reflexivity|]. split.
    + match goal with H : SigOf W s _ None ?g2 |- _ => exact (sig_rel W s1 s2 _ _ _ _ _ _ _ Sig H Ag2 (fun _ _ E _ => ltac:(discriminate E))) end.
    + match goal with H : AppOf W s _ ?a2 |- _ => exact (appof_rel _ _ _ _ _ App H Ag2) end.
  - (* functor *)
    intros p ob tm a bs pa u d n ps args res fx R Dd Ast B Fx Rd ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' u); assert (s' = u) by congruence; subst s' end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (obr_rel _ _ _ _ _ _ B H Ag1) as Bs end.
    pose proof (agree_app s1 s2 _ _ _ _ Bs Ag1) as Ag2.
    constructor; [|constructor]. cbn. split; [reflexivity|].
    intros x t1 t2 I1 I2. destruct (proj1 (Rd x t1) I1) as [_ L1].
    match goal with H : FixedReadings W u _ ?fx2 |- _ => destruct (proj1 (H x t2) I2) as [_ L2] end.
    exact (Ag2 x t1 t2 L1 L2).
  - (* method *)
    intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sig App ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' r); assert (s' = r) by congruence; subst s' end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (obr_rel _ _ _ _ _ _ B H Ag1) as Bs end.
    pose proof (agree_app s1 s2 _ _ _ _ Bs Ag1) as Ag2.
    rename m into m1.
    match goal with |- Forall2 (EntryRel _ ?T2) _ _ => set (tmB := T2) in * end.
    match goal with H : MethodNamed W p r dn ?m2 |- _ => rename m2 into mm end.
    assert (Em : mm = m1).
    { match goal with H : MethodNamed W p r dn mm |- _ => destruct Mn as [A1|[N1 S1]]; destruct H as [A2|[N2 S2]] end.
      - unfold Attached in A1, A2. congruence.
      - exfalso. exact (N2 m1 A1).
      - exfalso. exact (N1 mm A2).
      - congruence. }
    subst mm.
    constructor; [|constructor]. cbn. split; [reflexivity|]. split.
    + match goal with H : AppOf W m1 _ ?a2 |- _ => exact (appof_rel _ _ _ _ _ App H Ag2) end.
    + assert (Dec : (nlook r tm = None <-> nlook r tmB = None) \/ DomGap tm tmB).
      { destruct (nlook r tm) eqn:K1; destruct (nlook r tmB) eqn:K2.
        - left. split; discriminate.
        - right. exists r. rewrite K1, K2. intros [_ D]. discriminate (D eq_refl).
        - right. exists r. rewrite K1, K2. intros [D _]. discriminate (D eq_refl).
        - left. split; reflexivity. }
      destruct Dec as [D|G]; [left|right; exact G].
      pose proof (prefix_dom _ _ _ _ r Bs (prefix_dom _ _ _ _ r Ob D)) as Df.
      match goal with H : RecvTy W r _ ?rt2 |- _ => pose proof (recv_rel _ _ _ _ _ Rv H Ag2 Df) as Rr end.
      split; [exact Rr|].
      match goal with H : SigOf W m1 _ (Some ?rt2) ?g2 |- _ =>
        exact (sig_rel W s1 s2 _ _ _ _ _ _ _ Sig H Ag2 (fun x1 x2 E1 E2 => ltac:(injection E1 as <-; injection E2 as <-; exact Rr))) end.
  - (* context *)
    intros p ob tm a pa c d n its bs es tm' R Dd Ast B Inner IH ob2 tm2 es2 tm2' D2 Ob Ag.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' c); assert (s' = c) by congruence; subst s' end.
    match goal with H : w_decl W c = Some ?d' |- _ => assert_fails (constr_eq d' d); assert (d' = d) by congruence; subst d' end.
    rewrite Ast in *. match goal with H : ADCtx _ _ = ADCtx _ _ |- _ => injection H as <- <- end.
    pose proof (agree_app s1 s2 _ _ _ _ Ob Ag) as Ag1.
    match goal with H : OutputBrackets W p (ob2 ++ _) a ?bs2 |- _ => pose proof (obr_rel _ _ _ _ _ _ B H Ag1) as Bs end.
    match goal with H : OutputItemsEntries W _ _ _ its ?es2 _ |- _ => exact (IH _ _ _ _ H (Forall2_app Bs Ob) Ag) end.
  - (* nil *)
    intros p ob tm ob2 tm2 es2 tm2' D2 Ob Ag. inversion D2; subst. constructor.
  - (* cons *)
    intros p ob tm sp rest es1 tm1 es2 tm2 D1 IH1 Ds IHs ob2 tm2x es2x tm2x' D2 Ob Ag.
    inversion D2; subst.
    match goal with H1 : OutputItemEntries W p ob2 tm2x sp ?a ?b, H2 : OutputItemsEntries W p ob2 ?b rest ?c ?e |- _ =>
      pose proof (IH1 _ _ _ _ H1 Ob Ag) as F1;
      rewrite (proj1 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ H1) in H2;
      rewrite (proj1 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ D1) in IHs;
      exact (Forall2_app F1 (IHs _ _ _ _ H2 Ob Ag)) end.
Qed.

(* The same, for two ProvideResult readings of one functor's result: in
   contexts C1 and C2, with output readings out1 and out2, whose output
   type maps agree after s1 and s2. *)
Corollary provide_result_rel : forall u C1 out1 C1' ks1 C2 out2 C2' ks2,
  ProvideResult W u C1 out1 C1' ks1 -> ProvideResult W u C2 out2 C2' ks2 ->
  (forall outs1 outs2, (forall t, In t outs1 <-> FunOutputs W u t) -> (forall t, In t outs2 <-> FunOutputs W u t) ->
     Agree (output_type_map C1 outs1 out1) (output_type_map C2 outs2 out2)) ->
  exists outs1 outs2 es1 es2,
    (forall t, In t outs1 <-> FunOutputs W u t) /\ (forall t, In t outs2 <-> FunOutputs W u t) /\
    C1' = output_cx C1 (output_type_map C1 outs1 out1) es1 /\ ResultKeys (c_sub C1) es1 ks1 /\
    C2' = output_cx C2 (output_type_map C2 outs2 out2) es2 /\ ResultKeys (c_sub C2) es2 ks2 /\
    Forall2 (EntryRel (output_type_map C1 outs1 out1) (output_type_map C2 outs2 out2)) es1 es2.
Proof.
  intros u C1 out1 C1' ks1 C2 out2 C2' ks2
    (d & ps & a & r & b & outs1 & es1 & tm1 & D1 & FP1 & O1 & E1 & K1 & ->)
    (d' & ps' & a' & r' & b' & outs2 & es2 & tm2 & D2 & FP2 & O2 & E2 & K2 & ->) Ag.
  rewrite D1 in D2. injection D2 as <-. rewrite FP1 in FP2. injection FP2 as <- <- <- <-.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E1) in *.
  rewrite (proj2 (PublicFormation.output_entries_tm W) _ _ _ _ _ _ E2) in *.
  exists outs1, outs2, es1, es2. split; [exact O1|]. split; [exact O2|]. split; [reflexivity|]. split; [exact K1|].
  split; [reflexivity|]. split; [exact K2|].
  exact (proj2 output_entries_rel _ _ _ _ _ _ E1 _ _ _ _ E2 (Forall2_nil _) (Ag outs1 outs2 O1 O2)).
Qed.

End OutputSubst.
