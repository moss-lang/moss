(* ------------------------------------------------------------------------ *)
(* CtxMerge.v — merged_walk's rounds against MostGeneralMerge and Needs     *)
(* (Track A, CtxOf from fn_info).                                           *)
(*                                                                           *)
(* 1. The reading IEs of a telescope is functional (TyOf, Brackets, SigOf, *)
(*    AppOf, RecvTy, MethodNamed are): every round of merged_walk, whatever *)
(*    merges it starts from, reads the same entries (ies_det).              *)
(* 2. Method keys: tr is injective on the types ety_ok admits (no nominal  *)
(*    headed 0, no union of nothing), given an injective field numbering.  *)
(* 3. A settled round (no new merge): σ = suf U unifies its entries         *)
(*    (settled_unifies), its needs are their Needs (settled_needs), and,    *)
(*    when every unifier respects U, σ is a MostGeneralMerge (settled_mgm).*)
(* 4. merged_walk: a round that settles adds no edge (edges_settled); each  *)
(*    unsettled round's edges are respected by every unifier of the entries*)
(*    (WS's ws_mg, chained across rounds through ies_det); so the last     *)
(*    round's σ is a MostGeneralMerge with the needs as Needs               *)
(*    (merged_walk_spec), for any walk whose rounds have a functional      *)
(*    reading and keep WS.                                                  *)
(* 5. fn_info's walk for a function that is not a forwarder and not `main`  *)
(*    (assumes_merged), from CtxWalk's assumes_walk, given MethStep.        *)
(* 6. The context's tables (types, vals, fns, methods at a canonical key)  *)
(*    against the walk's environment, read canonically.                     *)
(* 7. fn_info on a cache miss (fn_info_ctx): CtxOfIE, CtxOf with IEs for    *)
(*    ItemsEntries, of the function, with fi_needs as its needs and the     *)
(*    tables of 6 against fi_env.  Not yet: the cache (st_info), `main`'s  *)
(*    root context, forwarders, c_funs and c_apps.                          *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrame ReqsTotal BodySound BodyTy Merge CtxWalk.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.
Notation asub := Audit.app_subst.

(* ======================================================================== *)
(* 1. The reading is functional                                              *)
(* ======================================================================== *)

Section Det.
Variable W : A.world.
Import A.

Lemma pair_eta : forall X Y (p q : X * Y), fst p = fst q -> snd p = snd q -> p = q.
Proof. intros X Y [a b] [c d]; cbn; intros -> ->; reflexivity. Qed.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Fixpoint tyof_det p tm th t a (H : TyOf W p tm th t a) {struct H} : forall b, TyOf W p tm th t b -> a = b
with br_det p tm g x l (H : Brackets W p tm g x l) {struct H} : forall l', Brackets W p tm g x l' -> l = l'.
Proof.
  - destruct H as [p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros b' H'; inversion H'; subst; clear H'.
    + reflexivity.
    + reflexivity.
    + reflexivity.
    + f_equal. match goal with H0 : Forall2 _ fs ?ts0 |- _ => assert_fails (constr_eq H0 F); revert ts0 H0 end.
      induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; intros ts0 F0; inversion F0; subst; [reflexivity|].
      match goal with H0 : fst ?y = _ /\ _ |- _ => destruct H0 as [E2 Hy] end.
      f_equal; [apply pair_eta; [congruence | exact (tyof_det _ _ _ _ _ Hft _ Hy)] | apply IH; assumption].
    + match goal with H0 : Forall2 _ ms ?ts0 |- _ => assert_fails (constr_eq H0 F); rename ts0 into ts1 end.
      assert (E : ts = ts1).
      { match goal with H0 : Forall2 _ ms ts1 |- _ => clear -F H0 tyof_det; revert ts1 H0 end.
        induction F as [|m t ms' ts' Ht Fr IH]; intros ts0 F0; inversion F0; subst; [reflexivity|].
        f_equal; [exact (tyof_det _ _ _ _ _ Ht _ ltac:(eassumption)) | apply IH; assumption]. }
      subst. reflexivity.
    + congruence.
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => assert_fails (constr_eq H0 B); pose proof (br_det _ _ _ _ _ B _ H0) as <- end.
      f_equal. match goal with H0 : Forall2 _ (w_reqs W g) ?a0 |- _ => assert_fails (constr_eq H0 F); revert a0 H0 end.
      clear -F. induction F as [|r ra rs ras [E1 L1] Fr IH]; intros a0 F0; inversion F0; subst; [reflexivity|].
      match goal with H0 : fst ?y = _ /\ _ |- _ => destruct H0 as [E2 L2] end.
      f_equal; [apply pair_eta; congruence | apply IH; assumption].
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0. assert (d0 = d) by congruence. subst d0.
      rewrite A in *. match goal with H0 : ADAlias _ _ = ADAlias _ _ |- _ => injection H0 as <- <- end.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => assert_fails (constr_eq H0 B); pose proof (br_det _ _ _ _ _ B _ H0) as <- end.
      exact (tyof_det _ _ _ _ _ T _ ltac:(eassumption)).
  - destruct H as [p tm g|p tm g bs l F]; intros l' H'; inversion H'; subst; clear H'; [reflexivity|].
    f_equal. match goal with H0 : Forall2 _ bs ?l0 |- _ => assert_fails (constr_eq H0 F); revert l0 H0 end.
    induction F as [|b0 tb bs' l1 (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; intros l0 F0; inversion F0; subst; [reflexivity|].
    match goal with H0 : exists _ _, _ |- _ => destruct H0 as (sp' & sa' & E2 & R2 & K2 & Q2 & T2) end.
    rewrite E1 in E2. injection E2 as <- <-.
    f_equal; [apply pair_eta; [congruence|] | apply IH; assumption].
    exact (tyof_det _ _ _ _ _ T1 _ T2).
Qed.

Lemma forall2_tyof_det : forall p tm th l ts ts', Forall2 (fun (pt : name * aty) t => TyOf W p tm th (snd pt) t) l ts ->
  Forall2 (fun (pt : name * aty) t => TyOf W p tm th (snd pt) t) l ts' -> ts = ts'.
Proof.
  intros p tm th l ts ts' F. revert ts'. induction F as [|x y l1 l2 H Fr IH]; intros ts' F'; inversion F'; subst; [reflexivity|].
  f_equal; [exact (tyof_det _ _ _ _ _ H _ ltac:(eassumption)) | apply IH; assumption].
Qed.

Lemma sigof_det : forall s tm th g1 g2, SigOf W s tm th g1 -> SigOf W s tm th g2 -> g1 = g2.
Proof.
  intros s tm th [ps1 r1] [ps2 r2] (d & rcv & n & ps & rt & body & D & A & P1 & R1)
    (d' & rcv' & n' & ps' & rt' & body' & D' & A' & P2 & R2).
  rewrite D in D'. injection D' as <-. rewrite A in A'. injection A' as <- <- <- <- <-. cbn in *.
  f_equal; [exact (forall2_tyof_det _ _ _ _ _ _ P1 P2)|].
  destruct rt as [r|]; [exact (tyof_det _ _ _ _ _ R1 _ R2) | congruence].
Qed.

Lemma bools_iff_det : forall X (P : X -> Prop) l ms1 ms2,
  Forall2 (fun m r => m = true <-> P r) ms1 l -> Forall2 (fun m r => m = true <-> P r) ms2 l -> ms1 = ms2.
Proof.
  intros X P l ms1 ms2 F. revert ms2. induction F as [|m r ms l' H Fr IH]; intros ms2 F'; inversion F'; subst; [reflexivity|].
  f_equal; [|apply IH; assumption].
  match goal with H0 : (?y = true <-> _) |- _ => destruct m, y; [reflexivity | exfalso | exfalso | reflexivity];
    [apply diff_false_true, H0, H, eq_refl | apply diff_false_true, H, H0, eq_refl] end.
Qed.

Lemma dkind_eq_dec' : forall a b : dkind, {a = b} + {a <> b}.
Proof. decide equality. Defined.

Lemma appof_det : forall s tm a1 a2, AppOf W s tm a1 -> AppOf W s tm a2 -> a1 = a2.
Proof.
  intros s tm a1 a2 (rs1 & _ & -> & N1 & M1) (rs2 & _ & -> & N2 & M2). f_equal.
  destruct (kind_sym W s) as [k|] eqn:K; [destruct (dkind_eq_dec' k DKMeth) as [->|Ne]|].
  - destruct (M1 eq_refl) as (ms1 & -> & F1). destruct (M2 eq_refl) as (ms2 & -> & F2).
    rewrite (bools_iff_det _ _ _ _ _ F1 F2). reflexivity.
  - rewrite (N1 ltac:(congruence)), (N2 ltac:(congruence)). reflexivity.
  - rewrite (N1 ltac:(discriminate)), (N2 ltac:(discriminate)). reflexivity.
Qed.

Lemma recvty_det : forall r tm t1 t2, RecvTy W r tm t1 -> RecvTy W r tm t2 -> t1 = t2.
Proof.
  intros r tm t1 t2. unfold RecvTy. destruct (nlook r tm); [congruence|].
  intros (a1 & -> & F1) (a2 & -> & F2). f_equal. revert a2 F2. clear -F1.
  induction F1 as [|x xa xs xas [E1 L1] Fr IH]; intros a2 F2; inversion F2; subst; [reflexivity|].
  match goal with H0 : fst ?y = _ /\ _ |- _ => destruct H0 as [E2 L2] end.
  f_equal; [apply pair_eta; congruence | apply IH; assumption].
Qed.

Lemma methodnamed_det : forall p r n m1 m2, MethodNamed W p r n m1 -> MethodNamed W p r n m2 -> m1 = m2.
Proof.
  unfold MethodNamed, Attached. intros p r n m1 m2 [H1|[N1 H1]] [H2|[N2 H2]].
  - congruence.
  - exfalso. exact (N2 m1 H1).
  - exfalso. exact (N1 m2 H2).
  - congruence.
Qed.

End Det.

Scheme IE_mut := Minimality for IE Sort Prop with IEs_mut := Minimality for IEs Sort Prop.

Section DetIE.
Variable W : A.world.
Import A.

Ltac kmis' := exfalso;
  repeat match goal with
  | H : _ \/ _ |- _ => destruct H
  | H : exists _, _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  end; subst;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  repeat match goal with
  | H1 : w_decl W ?x = Some ?d1, H2 : w_decl W ?x = Some ?d2 |- _ =>
      assert_fails (constr_eq d1 d2); assert (d2 = d1) by congruence; subst d2; clear H2
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; congruence.

Ltac fin :=
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  | H1 : w_decl W ?x = Some ?d1, H2 : w_decl W ?x = Some ?d2 |- _ =>
      assert_fails (constr_eq d1 d2); assert (d2 = d1) by congruence; subst d2; clear H2
  | H1 : d_ast ?d = _, H2 : d_ast ?d = _ |- _ =>
      assert_fails (constr_eq H1 H2); rewrite H1 in H2; injection H2; intros; subst; clear H2
  | H1 : MethodNamed W ?p ?r ?n ?m1, H2 : MethodNamed W ?p ?r ?n ?m2 |- _ =>
      assert_fails (constr_eq m1 m2); pose proof (methodnamed_det W _ _ _ _ _ H1 H2); subst m2; clear H2
  | H1 : Brackets W ?p ?tm ?g ?a ?b1, H2 : Brackets W ?p ?tm ?g ?a ?b2 |- _ =>
      assert_fails (constr_eq b1 b2); pose proof (br_det W _ _ _ _ _ H1 _ H2); subst b2; clear H2
  | H1 : RecvTy W ?r ?tm ?t1, H2 : RecvTy W ?r ?tm ?t2 |- _ =>
      assert_fails (constr_eq t1 t2); pose proof (recvty_det W _ _ _ _ H1 H2); subst t2; clear H2
  | H1 : SigOf W ?s ?tm ?th ?g1, H2 : SigOf W ?s ?tm ?th ?g2 |- _ =>
      assert_fails (constr_eq g1 g2); pose proof (sigof_det W _ _ _ _ _ H1 H2); subst g2; clear H2
  | H1 : AppOf W ?s ?tm ?a1, H2 : AppOf W ?s ?tm ?a2 |- _ =>
      assert_fails (constr_eq a1 a2); pose proof (appof_det W _ _ _ _ H1 H2); subst a2; clear H2
  | H1 : TyOf W ?p ?tm ?th ?t ?a1, H2 : TyOf W ?p ?tm ?th ?t ?a2 |- _ =>
      assert_fails (constr_eq a1 a2); pose proof (tyof_det W _ _ _ _ _ H1 _ H2); subst a2; clear H2
  | IH : forall es2 tm2, IE W ?p ?ob ?tm ?sp es2 tm2 -> _, H : IE W ?p ?ob ?tm ?sp ?e2 ?t2 |- _ =>
      destruct (IH _ _ H); subst; clear H
  | IH : forall es2 tm2, IEs W ?p ?ob ?tm ?sp es2 tm2 -> _, H : IEs W ?p ?ob ?tm ?sp ?e2 ?t2 |- _ =>
      destruct (IH _ _ H); subst; clear H
  end; auto.

Lemma ie_det_all :
  (forall p ob tm sp es tm', IE W p ob tm sp es tm' -> forall es2 tm2, IE W p ob tm sp es2 tm2 -> es = es2 /\ tm' = tm2) /\
  (forall p ob tm its es tm', IEs W p ob tm its es tm' -> forall es2 tm2, IEs W p ob tm its es2 tm2 -> es = es2 /\ tm' = tm2).
Proof.
  split; [apply (IE_mut W
    (fun p ob tm sp es tm' => forall es2 tm2, IE W p ob tm sp es2 tm2 -> es = es2 /\ tm' = tm2)
    (fun p ob tm its es tm' => forall es2 tm2, IEs W p ob tm its es2 tm2 -> es = es2 /\ tm' = tm2))
   | apply (IEs_mut W
    (fun p ob tm sp es tm' => forall es2 tm2, IE W p ob tm sp es2 tm2 -> es = es2 /\ tm' = tm2)
    (fun p ob tm its es tm' => forall es2 tm2, IEs W p ob tm its es2 tm2 -> es = es2 /\ tm' = tm2))];
  intros; lazymatch goal with H : _ |- _ => inversion H; subst; clear H end;
  try solve [kmis']; fin.
Qed.

End DetIE.

Lemma ie_det : forall W p ob tm sp es1 tm1 es2 tm2, IE W p ob tm sp es1 tm1 -> IE W p ob tm sp es2 tm2 -> es1 = es2 /\ tm1 = tm2.
Proof. intros W p ob tm sp es1 tm1 es2 tm2 H1 H2. exact (proj1 (ie_det_all W) _ _ _ _ _ _ H1 _ _ H2). Qed.

Lemma ies_det : forall W p ob tm its es1 tm1 es2 tm2, IEs W p ob tm its es1 tm1 -> IEs W p ob tm its es2 tm2 -> es1 = es2 /\ tm1 = tm2.
Proof. intros W p ob tm its es1 tm1 es2 tm2 H1 H2. exact (proj2 (ie_det_all W) _ _ _ _ _ _ H1 _ _ H2). Qed.

(* ======================================================================== *)
(* 2. Method keys: tr is injective on the types ety_ok admits                *)
(* ======================================================================== *)

Section Inj.
Variable Fl : string -> nat.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Notation trF := (tr Fl).

Lemma map_inj_forall : forall X Y (f : X -> Y) (P : X -> Prop) xs ys,
  Forall (fun x => forall y, P y -> f x = f y -> x = y) xs -> Forall P ys -> map f xs = map f ys -> xs = ys.
Proof.
  intros X Y f P xs. induction xs as [|x r IH]; intros [|y r'] Fx Fy E; cbn in E; try discriminate; [reflexivity|].
  injection E as E1 E2. inversion Fx; subst. inversion Fy; subst. f_equal; [auto | eauto].
Qed.

Lemma forallb_Forall : forall X (f : X -> bool) l, forallb f l = true -> Forall (fun x => f x = true) l.
Proof. intros X f l H. apply Forall_forall. intros x Hx. rewrite forallb_forall in H. exact (H x Hx). Qed.

Lemma tr_inj : forall a b, ety_ok a = true -> ety_ok b = true -> trF a = trF b -> a = b.
Proof.
  induction a as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intros b Oa Ob E;
    destruct b as [| |g'|n' ys|ms'|fs']; cbn [tr] in E; try discriminate; try reflexivity.
  - destruct ms' as [|m r]; [discriminate Ob | discriminate E].
  - injection E as ->. reflexivity.
  - cbn [ety_ok] in Oa, Ob. apply andb_true_iff in Oa as [Na Oa]. apply andb_true_iff in Ob as [Nb Ob].
    injection E as <- E. f_equal.
    eapply (map_inj_forall _ _ _ (fun p => ety_ok (snd p) = true)); [| exact (forallb_Forall _ _ _ Ob) | exact E].
    pose proof (forallb_Forall _ _ _ Oa) as Fa. rewrite Forall_forall in IH, Fa |- *.
    intros [k x] Hx [k' y] Hy Exy. cbn in Exy |- *. injection Exy as <- Exy. f_equal.
    exact (IH (k, x) Hx y (Fa _ Hx) Hy Exy).
  - cbn [ety_ok] in Oa. apply andb_true_iff in Oa as [Na _]. injection E as En _. subst n.
    discriminate Na.
  - destruct ms as [|m r]; [discriminate Oa | discriminate E].
  - cbn [ety_ok] in Oa, Ob. destruct ms as [|m r]; [discriminate Oa|]. destruct ms' as [|m' r']; [discriminate Ob|].
    assert (E' : map trF (m :: r) = map trF (m' :: r')) by (injection E as E1 E2; cbn; rewrite E1, E2; reflexivity).
    clear E. rename E' into E. f_equal.
    eapply (map_inj_forall _ _ _ (fun x => ety_ok x = true)); [| exact (forallb_Forall _ _ _ Ob) | exact E].
    pose proof (forallb_Forall _ _ _ Oa) as Fa. rewrite Forall_forall in IH, Fa |- *.
    intros x Hx y Hy Exy. exact (IH x Hx y (Fa _ Hx) Hy Exy).
  - cbn [ety_ok] in Ob. apply andb_true_iff in Ob as [Nb _]. injection E as En _. subst n'.
    discriminate Nb.
  - cbn [ety_ok] in Oa, Ob. injection E as E. f_equal.
    eapply (map_inj_forall _ _ _ (fun p => ety_ok (snd p) = true)); [| exact (forallb_Forall _ _ _ Ob) | exact E].
    pose proof (forallb_Forall _ _ _ Oa) as Fa. rewrite Forall_forall in IH, Fa |- *.
    intros [k x] Hx [k' y] Hy Exy. cbn in Exy |- *. injection Exy as Ek Exy. rewrite (Finj _ _ Ek). f_equal.
    exact (IH (k, x) Hx y (Fa _ Hx) Hy Exy).
Qed.

End Inj.

(* ======================================================================== *)
(* 3. A settled round: σ = suf U unifies the entries; the needs             *)
(* ======================================================================== *)

Section Settled.
Variable Fl : string -> nat.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Notation trF := (tr Fl).

(* the item an elaborator key reads as *)
Definition key_tr (k : key) : item :=
  match k with
  | KyVal v => IVal v
  | KyFn f => IFn f
  | KyMth r m => IMth (trF r) m []
  | KyFun u => IFun u
  end.

Lemma sigma_tr : forall U e t, en_uf e = U -> Tri U -> asub (suf Fl U) (trF t) = trF (canon e t).
Proof. intros U e t E T. rewrite (tr_res Fl U (tri_rk U T)), canon_res, E. reflexivity. Qed.

Lemma entry_key_tr : forall U e l, en_uf e = U -> Tri U ->
  A.entry_key (suf Fl U) (tr_ent Fl l) = option_map key_tr (lkey e l).
Proof.
  intros U e [t|v y a|f g a|r m g a|u] E T; try reflexivity.
  cbn [tr_ent A.entry_key lkey option_map key_tr]. unfold A.mkey. rewrite (sigma_tr U e r E T). reflexivity.
Qed.

Lemma sig_tr : forall U e g, en_uf e = U -> Tri U ->
  A.sig_subst (suf Fl U) (tr_sig Fl g) = A.FSig (map trF (map (fun p => canon e (snd p)) (sg_params g))) (trF (canon e (sg_ret g))).
Proof.
  intros U e g E T. unfold A.sig_subst, tr_sig. cbn [A.sg_params A.sg_ret]. rewrite (sigma_tr U e _ E T). f_equal.
  rewrite !map_map. apply map_ext. intro p. exact (sigma_tr U e _ E T).
Qed.

Lemma sigcanon_tr : forall U e g g', en_uf e = U -> Tri U -> SigCanon e g g' ->
  A.sig_subst (suf Fl U) (tr_sig Fl g) = A.sig_subst (suf Fl U) (tr_sig Fl g').
Proof. intros U e g g' E T [P R]. rewrite !(sig_tr U e _ E T), P, R. reflexivity. Qed.

Lemma settled_unifies : forall U e tm les needs, WS Fl U e tm les needs -> en_uf e = U ->
  A.Unifies (suf Fl U) (map (tr_ent Fl) les).
Proof.
  intros U e tm les needs Hws E. pose proof (ws_tri _ _ _ _ _ _ Hws) as T. rewrite E in T.
  intros e1 e2 k H1 H2 K1 K2. apply in_map_iff in H1 as (l1 & <- & Hl1). apply in_map_iff in H2 as (l2 & <- & Hl2).
  rewrite (entry_key_tr U e _ E T) in K1. rewrite (entry_key_tr U e _ E T) in K2.
  destruct l1 as [t1|v1 y1 a1|f1 g1 a1|r1 m1 g1 a1|u1], l2 as [t2|v2 y2 a2|f2 g2 a2|r2 m2 g2 a2|u2];
    cbn [lkey option_map key_tr] in K1, K2; try discriminate; try (rewrite <- K2 in K1; discriminate K1);
    cbn [tr_ent A.entry_sig]; try reflexivity.
  - rewrite <- K2 in K1. injection K1 as <-.
    destruct (ws_vals _ _ _ _ _ _ Hws v1 y1 a1 Hl1) as (y0 & _ & G1 & _ & C1).
    destruct (ws_vals _ _ _ _ _ _ Hws v1 y2 a2 Hl2) as (y0' & _ & G2 & _ & C2).
    rewrite G1 in G2. injection G2 as <-. rewrite !(sigma_tr U e _ E T), (C1 E), (C2 E). reflexivity.
  - rewrite <- K2 in K1. injection K1 as <-.
    destruct (ws_fns _ _ _ _ _ _ Hws f1 g1 a1 Hl1) as (_ & g0 & _ & G1 & _ & C1).
    destruct (ws_fns _ _ _ _ _ _ Hws f1 g2 a2 Hl2) as (_ & g0' & _ & G2 & _ & C2).
    rewrite G1 in G2. injection G2 as <-. f_equal.
    rewrite (sigcanon_tr U e _ _ E T (C1 E)), (sigcanon_tr U e _ _ E T (C2 E)). reflexivity.
  - rewrite <- K2 in K1. injection K1 as Er <-.
    destruct (ws_mths _ _ _ _ _ _ Hws E r1 m1 g1 a1 Hl1) as (O1 & g0 & G1 & C1).
    destruct (ws_mths _ _ _ _ _ _ Hws E r2 m1 g2 a2 Hl2) as (O2 & g0' & G2 & C2).
    pose proof (tr_inj Fl Finj _ _ O1 O2 Er) as Ec. rewrite Ec in G1. rewrite G1 in G2. injection G2 as <-. f_equal.
    rewrite (sigcanon_tr U e _ _ E T C1), (sigcanon_tr U e _ _ E T C2). reflexivity.
Qed.

Lemma nodup_map_on : forall X Y (f : X -> Y) l, NoDup l ->
  (forall x y, In x l -> In y l -> f x = f y -> x = y) -> NoDup (map f l).
Proof.
  intros X Y f l N. induction N as [|x r Nx N IH]; intro Hi; cbn; constructor.
  - intro Hx. apply in_map_iff in Hx as (y & Ey & Hy). apply Nx.
    rewrite <- (Hi y x (or_intror Hy) (or_introl eq_refl) Ey). exact Hy.
  - apply IH. intros a c Ha Hc. apply Hi; right; assumption.
Qed.

Lemma settled_needs : forall U e tm les needs, WS Fl U e tm les needs -> en_uf e = U ->
  A.Needs (suf Fl U) (map (tr_ent Fl) les) (map key_tr needs).
Proof.
  intros U e tm les needs Hws E. pose proof (ws_tri _ _ _ _ _ _ Hws) as T. rewrite E in T.
  assert (Ok : forall k, In k needs -> match k with KyMth r _ => ety_ok r = true | _ => True end).
  { intros k Hk. apply (ws_needs _ _ _ _ _ _ Hws E k) in Hk as (l & Hl & Hk).
    destruct l as [|v y a|f g a|r m g a|u]; cbn in Hk; try discriminate; injection Hk as <-; try exact I.
    exact (proj1 (ws_mths _ _ _ _ _ _ Hws E r m g a Hl)). }
  split.
  - apply nodup_map_on; [exact (ws_nodup _ _ _ _ _ _ Hws)|]. intros x y Hx Hy Exy.
    pose proof (Ok x Hx) as Ox. pose proof (Ok y Hy) as Oy.
    destruct x as [x|x|rx mx|x], y as [y|y|ry my|y]; cbn in Exy; try discriminate; try (injection Exy as ->; reflexivity).
    injection Exy as Er ->. rewrite (tr_inj Fl Finj _ _ Ox Oy Er). reflexivity.
  - intro k. rewrite in_map_iff. split.
    + intros (k0 & <- & Hk0). apply (ws_needs _ _ _ _ _ _ Hws E k0) in Hk0 as (l & Hl & Hk).
      exists (tr_ent Fl l). split; [apply in_map; exact Hl|]. rewrite (entry_key_tr U e l E T), Hk. reflexivity.
    + intros (e1 & H1 & Hk). apply in_map_iff in H1 as (l & <- & Hl). rewrite (entry_key_tr U e l E T) in Hk.
      destruct (lkey e l) as [k0|] eqn:Ek; [|discriminate]. injection Hk as <-. exists k0. split; [reflexivity|].
      apply (ws_needs _ _ _ _ _ _ Hws E k0). exists l. split; assumption.
Qed.

(* the merge of a settled round, given that every unifier of the entries
   respects its edges (what the rounds before it give) *)
Lemma settled_mgm : forall U e tm les needs, WS Fl U e tm les needs -> en_uf e = U ->
  (forall sb, A.Unifies sb (map (tr_ent Fl) les) -> RespE Fl sb U) ->
  A.MostGeneralMerge (suf Fl U) (map (tr_ent Fl) les).
Proof.
  intros U e tm les needs Hws E Mg. pose proof (ws_tri _ _ _ _ _ _ Hws) as T. rewrite E in T.
  split; [exact (settled_unifies U e tm les needs Hws E)|]. split.
  - exact (suf_idem Fl U (tri_rk U T)).
  - intros sb Hu t. exact (resp_mg Fl sb U (tri_rk U T) (Mg sb Hu) t).
Qed.

End Settled.

(* ======================================================================== *)
(* 4. merged_walk's rounds                                                   *)
(* ======================================================================== *)

Lemma edges_settled : forall U nw, edges_eqb ([] ++ U ++ nw) ([] ++ U) = true -> nw = [].
Proof.
  intros U nw H. unfold edges_eqb in H. apply andb_true_iff in H as [H _]. apply Nat.eqb_eq in H.
  cbn [List.app] in H. rewrite length_app in H. destruct nw; [reflexivity | cbn in H; lia].
Qed.

Lemma tne_nil : forall Fl U e tm les needs, WS Fl U e tm les needs ->
  filter (fun p => negb (ety_eqb (snd p) (YAbs (fst p)))) (tymap e) = [].
Proof.
  intros Fl U e tm les needs Hws. pose proof (ws_tnabs _ _ _ _ _ _ Hws) as T. unfold tymap.
  induction (en_tn e) as [|[k p] r IH]; [reflexivity|]. cbn [map filter fst snd].
  rewrite (T k p (or_introl eq_refl)), ety_eqb_refl. cbn [negb]. apply IH. intros k' p' H. exact (T k' p' (or_intror H)).
Qed.

Section Rounds.
Variables (Fl : string -> nat) (walk : env -> M (env * list key)).
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis Hok : forall e, ok (walk e).
(* what a round reads: its entries (and types in force), the same in every
   round (IEs is functional) *)
Variable Rd : list A.entry -> list (nat * ty) -> Prop.
Hypothesis Hdet : forall es tm es' tm', Rd es tm -> Rd es' tm' -> es = es'.
(* what the state must satisfy before (kept going forwards) and after
   (kept going back) *)
Variables (Pre Post : st -> Prop).
Hypothesis Pre_fwd : forall s s', Fr s s' -> Pre s -> Pre s'.
Hypothesis Post_back : forall s s', Fr s s' -> Post s' -> Post s.
(* one round, from the merges U *)
Hypothesis Hround : forall U s e needs s', Tri U -> Inv s -> Pre s -> Post s' ->
  walk (env_with_edges [] U) s = Good ((e, needs), s') ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl U e tm les needs.

Definition MG (U : list (nat * ety)) : Prop :=
  forall es tm sb, Rd es tm -> A.Unifies sb es -> RespE Fl sb U.

Definition mw_go (mi : nat) :=
  fix go (n : nat) (tne ufe : list (nat * ety)) : M (env * list key) :=
     let! (e, needs) := walk (env_with_edges tne ufe) in
     let tne' := filter (fun p => negb (ety_eqb (snd p) (YAbs (fst p)))) (tymap e) in
     let ufe' := en_uf e in
     if edges_eqb (tne' ++ ufe') (tne ++ ufe) then ret (e, needs) else
     match n with
     | 0 => err mi "merging the context does not settle in 8 rounds (D43)"
     | S n' => go n' tne' ufe'
     end.

Lemma mw_go_eq : forall mi, merged_walk mi walk = mw_go mi 8 [] [].
Proof. reflexivity. Qed.

Lemma ok_mw_go : forall mi n tne ufe, ok (mw_go mi n tne ufe).
Proof.
  intros mi n. induction n as [|n IH]; intros tne ufe; cbn [mw_go];
    apply ok_bind; [apply Hok | | apply Hok |]; intros [e needs]; cbn beta iota zeta;
    destruct (edges_eqb _ _); try apply ok_ret; [unfold err; okt | apply IH].
Qed.

Lemma rounds : forall mi n U s e needs s',
  mw_go mi n [] U s = Good ((e, needs), s') ->
  Inv s -> Pre s -> Post s' -> Tri U -> MG U ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl (en_uf e) e tm les needs /\ MG (en_uf e).
Proof.
  intros mi n. induction n as [|n IH]; intros U s e needs s' H Hi Hp Hq T Mg; cbn [mw_go] in H;
    apply bind_inv in H as ([e1 n1] & s1 & H1 & H2); destruct (Hok _ s _ s1 Hi H1) as [Hi1 F1];
    cbn beta iota zeta in H2.
  - destruct (edges_eqb _ _) eqn:Ee; [|noerr H2]. injection H2; intros; subst.
    destruct (Hround U s e needs s' T Hi Hp Hq H1) as (les & tm & Rl & Hws).
    rewrite (tne_nil _ _ _ _ _ _ Hws) in Ee. destruct (ws_ext _ _ _ _ _ _ Hws) as (nw & En).
    rewrite En in Ee. pose proof (edges_settled _ _ Ee) as ->. rewrite app_nil_r in En.
    exists les, tm. rewrite En. auto.
  - destruct (edges_eqb _ _) eqn:Ee.
    + injection H2; intros; subst.
      destruct (Hround U s e needs s' T Hi Hp Hq H1) as (les & tm & Rl & Hws).
      rewrite (tne_nil _ _ _ _ _ _ Hws) in Ee. destruct (ws_ext _ _ _ _ _ _ Hws) as (nw & En).
      rewrite En in Ee. pose proof (edges_settled _ _ Ee) as ->. rewrite app_nil_r in En.
      exists les, tm. rewrite En. auto.
    + destruct (ok_mw_go mi n _ _ s1 _ s' Hi1 H2) as [_ F2].
      destruct (Hround U s e1 n1 s1 T Hi Hp (Post_back _ _ F2 Hq) H1) as (les & tm & Rl & Hws).
      rewrite (tne_nil _ _ _ _ _ _ Hws) in H2.
      apply (IH (en_uf e1) s1 e needs s' H2 Hi1 (Pre_fwd _ _ F1 Hp) Hq (ws_tri _ _ _ _ _ _ Hws)).
      intros es tm' sb Rs Hu. apply (ws_mg _ _ _ _ _ _ Hws sb (Mg es tm' sb Rs Hu)).
      rewrite (Hdet _ _ _ _ Rl Rs). exact Hu.
Qed.

(* merged_walk: its last round's entries, read under σ = suf (its merges),
   form a MostGeneralMerge, and its needs are their Needs *)
Theorem merged_walk_spec : forall mi s e needs s', merged_walk mi walk s = Good ((e, needs), s') ->
  Inv s -> Pre s -> Post s' ->
  exists les tm, Rd (map (tr_ent Fl) les) tm /\ WS Fl (en_uf e) e tm les needs /\
    A.MostGeneralMerge (suf Fl (en_uf e)) (map (tr_ent Fl) les) /\
    A.Needs (suf Fl (en_uf e)) (map (tr_ent Fl) les) (map (key_tr Fl) needs).
Proof.
  intros mi s e needs s' H Hi Hp Hq. rewrite mw_go_eq in H.
  destruct (rounds mi 8 [] s e needs s' H Hi Hp Hq TriNil (fun es tm sb _ _ g t Hin => match Hin with end))
    as (les & tm & Rl & Hws & Mg).
  exists les, tm. split; [exact Rl|]. split; [exact Hws|]. split.
  - apply (settled_mgm Fl Finj _ e tm les needs Hws eq_refl). intros sb Hu. exact (Mg _ _ sb Rl Hu).
  - exact (settled_needs Fl Finj _ e tm les needs Hws eq_refl).
Qed.

End Rounds.

(* ======================================================================== *)
(* 5. A function's assume nesting, merged (fn_info's walk, not `main`)       *)
(* ======================================================================== *)

(* fn_info's walk, as it is written there *)
Definition fi_walk (g : gsym) (e : env) : M (env * list key) :=
  let mi := gs_mod g in
  match gs_origin g with
  | OForwarder t =>
      let! tg := sym t in
      let! rs := reqs t in
      let! cf := ctx_fuel in
      let! (e, needs) := mfold (fun acc r => let! rg := sym r in
                                             add_symbol_item cf (fst acc) mi rg [] (snd acc)) (e, []) rs in
      add_symbol_item cf e mi tg [] needs
  | _ =>
      let! acc := mfold (fun acc it => add_assume_item (fst acc) mi it (snd acc)) (e, []) (gs_assumes g) in
      let! main := is_main g in
      if main then add_root_context (fst acc) mi (snd acc) else ret acc
  end.

Lemma fn_info_miss : forall g s i s', fn_info g s = Good (i, s') -> nget (st_info s) (gs_id g) = None ->
  exists e needs s1, merged_walk (gs_mod g) (fi_walk g) s = Good ((e, needs), s1) /\
    i = FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e) /\
    s' = St (st_w s1) (st_syms s1) (st_next s1) (st_reqs s1) (st_sigsyms s1) (nset (st_info s1) (gs_id g) i)
            (st_fns s1) (st_funs s1) (st_fwd s1) (st_links s1) (st_flds s1) (st_consts s1)
            (st_wrappers s1) (st_vars s1) (st_nvar s1) (st_memory_bound s1) (st_memory_used s1)
            (st_checked_total s1) (st_lang s1) (st_unsup s1).
Proof.
  intros g s i s' H N. unfold fn_info in H. apply bind_inv in H as (x & s0 & H0 & H). injection H0 as <- <-.
  rewrite N in H. apply bind_inv in H as ([e needs] & s1 & H1 & H). exists e, needs, s1. split; [exact H1|].
  cbn beta iota zeta in H. apply bind_inv in H as (u & s2 & H2 & H). injection H as <- <-. unfold upd in H2.
  injection H2 as _ <-. split; reflexivity.
Qed.

Section FnWalk.
Variables (w : Elab.world) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis MS : MethStep w R Fl.
Notation WW := (BodySound.W w R Fl).

Lemma ok_is_main : forall g, ok (is_main g).
Proof. intro g. unfold is_main. destruct (_ || _); okt. Qed.

(* For a function g that is not a forwarder and not `main`: the merged walk
   reads its assume nesting (IEs, the enclosing brackets apart), whose entries
   σ = suf (the merges) merges most generally, with the needs as Needs. *)
Theorem assumes_merged : forall g s e needs s',
  (forall t, gs_origin g <> OForwarder t) ->
  (forall s0 m s1, st_w s0 = w -> is_main g s0 = Good (m, s1) -> m = false) ->
  In (gs_mod g) (w_order w) ->
  merged_walk (gs_mod g) (fi_walk g) s = Good ((e, needs), s') ->
  Inv s -> st_w s = w -> Later w R Fl s' ->
  exists les tm, IEs WW (gm_path (get_mod w (gs_mod g))) [] [] (map A.assume_spec (gs_assumes g)) (map (tr_ent Fl) les) tm /\
    WS Fl (en_uf e) e tm les needs /\
    A.MostGeneralMerge (suf Fl (en_uf e)) (map (tr_ent Fl) les) /\
    A.Needs (suf Fl (en_uf e)) (map (tr_ent Fl) les) (map (key_tr Fl) needs).
Proof.
  intros g s e needs s' Nf Nm Hmi H Hi Hw L.
  assert (Ew : forall e0, fi_walk g e0 =
             (let! acc := mfold (fun acc it => add_assume_item (fst acc) (gs_mod g) it (snd acc)) (e0, []) (gs_assumes g) in
              let! main := is_main g in
              if main then add_root_context (fst acc) (gs_mod g) (snd acc) else ret acc)).
  { intro e0. unfold fi_walk. destruct (gs_origin g) eqn:O; try reflexivity. exfalso. exact (Nf _ eq_refl). }
  apply (merged_walk_spec Fl (fi_walk g) Finj
           ltac:(intro e0; rewrite Ew; apply ok_bind; [apply ok_mfold; intros; apply ok_add_assume_item|];
                 intro acc; apply ok_bind; [apply ok_is_main|]; intro m; destruct m;
                 [unfold add_root_context; okt | apply ok_ret])
           (fun es tm => IEs WW (gm_path (get_mod w (gs_mod g))) [] [] (map A.assume_spec (gs_assumes g)) es tm)
           (fun es tm es' tm' H1 H2 => proj1 (ies_det _ _ _ _ _ _ _ _ _ H1 H2))
           (fun s0 => st_w s0 = w) (Later w R Fl)
           (fun s0 s1 F E => fr_w' w s0 s1 F E) (fun s0 s1 F L1 => later_from w R Fl s0 s1 F L1))
    with (mi := gs_mod g) (s := s) (s' := s'); [|exact H | exact Hi | exact Hw | exact L].
  clear e needs s s' H Hi Hw L. intros U s e needs s' T Hi Hw L H. rewrite Ew in H.
  apply bind_inv in H as ([e1 n1] & s1 & H1 & H).
  destruct (ok_mfold _ _ _ (gs_assumes g) (env_with_edges [] U, []) (fun acc it => ok_add_assume_item (fst acc) (gs_mod g) it (snd acc)) s _ s1 Hi H1) as [Hi1 F1].
  apply bind_inv in H as (m & s2 & H2 & H). pose proof (Nm s1 m s2 (fr_w' w _ _ F1 Hw) H2) as Em. subst m.
  injection H as -> -> ->. destruct (ok_is_main g s1 false s' Hi1 H2) as [_ F2].
  destruct (assumes_walk w R Fl b Hload MS (gs_mod g) (gs_assumes g) _ [] s e needs s1 U [] [] H1 Hi Hw Hmi
              (later_from w R Fl _ _ F2 L) (ws_start Fl U T)) as (les & es & tm & I & M & Ws).
  exists les, tm. rewrite M. split; [exact I | exact Ws].
Qed.

End FnWalk.

(* ======================================================================== *)
(* 6. The context's tables against the walk's environment                    *)
(* ======================================================================== *)

Section Tables.
Variable Fl : string -> nat.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Notation trF := (tr Fl).

(* a signature in force, read canonically *)
Definition csig (e : env) (g : Elab.fsig) : A.fsig :=
  A.FSig (map trF (map (fun p => canon e (snd p)) (sg_params g))) (trF (canon e (sg_ret g))).

Lemma tri_nassoc : forall U g t, Tri U -> In (g, t) U -> nassoc g U = Some t.
Proof.
  intros U g t T. induction T as [|U g' t' T IH Hg _ _]; [intros []|]. intro Hin. rewrite Merge.nassoc_app'.
  apply in_app_iff in Hin as [Hin|[E|[]]].
  - rewrite (IH Hin). reflexivity.
  - injection E as <- <-. rewrite Hg. cbn. rewrite Nat.eqb_refl. reflexivity.
Qed.

(* σ respects the merges it resolves *)
Lemma suf_resp : forall U, Tri U -> RespE Fl (suf Fl U) U.
Proof.
  intros U T g t Hin. cbn [tr Audit.app_subst]. rewrite lookup_suf, (tri_nassoc U g t T Hin). cbn [option_map].
  rewrite (res_abs U (tri_rk U T) g), (tri_nassoc U g t T Hin). symmetry. exact (tr_res Fl U (tri_rk U T) t).
Qed.

Lemma tr_ent_val : forall l v ty a, tr_ent Fl l = A.NVal v ty a -> exists y a0, l = LVal v y a0 /\ ty = trF y.
Proof. intros l v ty a H; destruct l; cbn in H; try discriminate. injection H as <- <- _. eauto. Qed.
Lemma tr_ent_fn : forall l f g a, tr_ent Fl l = A.NFn f g a -> exists g0 a0, l = LFn f g0 a0 /\ g = tr_sig Fl g0.
Proof. intros l f g a H; destruct l; cbn in H; try discriminate. injection H as <- <- _. eauto. Qed.
Lemma tr_ent_mth : forall l r m g a, tr_ent Fl l = A.NMth r m g a -> exists r0 g0 a0, l = LMth r0 m g0 a0 /\ r = trF r0 /\ g = tr_sig Fl g0.
Proof. intros l r m g a H; destruct l; cbn in H; try discriminate. injection H as <- <- <- _. eauto 7. Qed.

Lemma nlook_map_snd : forall X Y (f : X -> Y) k (l : list (nat * X)),
  A.nlook k (map (fun q => (fst q, f (snd q))) l) = option_map f (A.nlook k l).
Proof. intros X Y f k l. induction l as [|[a x] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Definition cvals (s : tsubst) (es : list A.entry) : list (nat * ty) :=
  flat_map (fun e => match e with A.NVal v t _ => [(v, asub s t)] | _ => [] end) es.
Definition cfns (s : tsubst) (es : list A.entry) : list (nat * A.fsig) :=
  flat_map (fun e => match e with A.NFn f g _ => [(f, A.sig_subst s g)] | _ => [] end) es.
Definition cmths (s : tsubst) (es : list A.entry) : list ((ty * nat) * A.fsig) :=
  flat_map (fun e => match e with A.NMth r m g _ => [((asub s r, m), A.sig_subst s g)] | _ => [] end) es.

Lemma cvals_some : forall s es v t, A.nlook v (cvals s es) = Some t -> exists ty a, In (A.NVal v ty a) es /\ t = asub s ty.
Proof.
  intros s es v t. induction es as [|e0 r IH]; [discriminate|]. unfold cvals in *. cbn [flat_map].
  destruct e0 as [|v' ty a|f g a|r0 m g a|u]; cbn [List.app]; try (intro H; destruct (IH H) as (ty1 & a1 & Hi & E1); exists ty1, a1; split; [right; exact Hi | exact E1]).
  cbn [A.nlook]. destruct (Nat.eqb_spec v v') as [->|Ne].
  - intro H. injection H as <-. exists ty, a. split; [left; reflexivity | reflexivity].
  - intro H. destruct (IH H) as (ty' & a' & Hi & E). exists ty', a'. split; [right; exact Hi | exact E].
Qed.

Lemma cvals_none : forall s es v ty a, A.nlook v (cvals s es) = None -> ~ In (A.NVal v ty a) es.
Proof.
  intros s es v ty a. induction es as [|e0 r IH]; [intros _ []|]. unfold cvals in *. cbn [flat_map].
  destruct e0 as [|v' ty' a'|f g a'|r0 m g a'|u]; cbn [List.app]; try (intros H [E|Hi]; [discriminate | exact (IH H Hi)]).
  cbn [A.nlook]. destruct (Nat.eqb_spec v v') as [->|Ne]; [discriminate|].
  intros H [E|Hi]; [injection E as <- _ _; exact (Ne eq_refl) | exact (IH H Hi)].
Qed.

Lemma cfns_some : forall s es f g, A.nlook f (cfns s es) = Some g -> exists g0 a, In (A.NFn f g0 a) es /\ g = A.sig_subst s g0.
Proof.
  intros s es f g. induction es as [|e0 r IH]; [discriminate|]. unfold cfns in *. cbn [flat_map].
  destruct e0 as [|v ty a|f' g' a|r0 m g' a|u]; cbn [List.app]; try (intro H; destruct (IH H) as (g1 & a1 & Hi & E1); exists g1, a1; split; [right; exact Hi | exact E1]).
  cbn [A.nlook]. destruct (Nat.eqb_spec f f') as [->|Ne].
  - intro H. injection H as <-. exists g', a. split; [left; reflexivity | reflexivity].
  - intro H. destruct (IH H) as (g0 & a0 & Hi & E). exists g0, a0. split; [right; exact Hi | exact E].
Qed.

Lemma cfns_none : forall s es f g a, A.nlook f (cfns s es) = None -> ~ In (A.NFn f g a) es.
Proof.
  intros s es f g a. induction es as [|e0 r IH]; [intros _ []|]. unfold cfns in *. cbn [flat_map].
  destruct e0 as [|v ty a'|f' g' a'|r0 m g' a'|u]; cbn [List.app]; try (intros H [E|Hi]; [discriminate | exact (IH H Hi)]).
  cbn [A.nlook]. destruct (Nat.eqb_spec f f') as [->|Ne]; [discriminate|].
  intros H [E|Hi]; [injection E as <- _ _; exact (Ne eq_refl) | exact (IH H Hi)].
Qed.

Lemma cmths_some : forall s es r m g, A.mlook r m (cmths s es) = Some g ->
  exists r0 g0 a, In (A.NMth r0 m g0 a) es /\ asub s r0 = r /\ g = A.sig_subst s g0.
Proof.
  intros s es r m g. induction es as [|e0 rest IH]; [discriminate|]. unfold cmths in *. cbn [flat_map].
  destruct e0 as [|v ty a|f g' a|r0 m0 g' a|u]; cbn [List.app];
    try (intro H; destruct (IH H) as (r9 & g9 & a9 & Hi & E1 & E2); exists r9, g9, a9; split; [right; exact Hi | split; assumption]).
  cbn [A.mlook]. destruct (ty_eqb r (asub s r0) && Nat.eqb m m0) eqn:E.
  - apply andb_true_iff in E as [E1 E2]. apply CoreMoss.ty_eqb_eq in E1. apply Nat.eqb_eq in E2. subst.
    intro H. injection H as <-. exists r0, g', a. split; [left; reflexivity | split; reflexivity].
  - intro H. destruct (IH H) as (r1 & g0 & a0 & Hi & E1 & E2). exists r1, g0, a0. split; [right; exact Hi | split; assumption].
Qed.

Lemma cmths_none : forall s es r m r0 g a, A.mlook r m (cmths s es) = None -> In (A.NMth r0 m g a) es -> asub s r0 <> r.
Proof.
  intros s es r m r0 g a. induction es as [|e0 rest IH]; [intros _ []|]. unfold cmths in *. cbn [flat_map].
  destruct e0 as [|v ty a'|f g' a'|r1 m1 g' a'|u]; cbn [List.app]; try (intros H [Eq|Hi]; [discriminate | exact (IH H Hi)]).
  cbn [A.mlook]. destruct (ty_eqb r (asub s r1) && Nat.eqb m m1) eqn:E; [discriminate|].
  intros H [Eq|Hi]; [|exact (IH H Hi)]. injection Eq as -> -> _ _. intro Er. subst r.
  rewrite (proj2 (CoreMoss.ty_eqb_eq _ _) eq_refl), Nat.eqb_refl in E. discriminate.
Qed.

Variables (U : list (nat * ety)) (e : env) (tm : list (nat * ty)) (les : list lent) (needs : list key).
Hypothesis Hws : WS Fl U e tm les needs.
Hypothesis E : en_uf e = U.
Let T : Tri U := eq_ind _ Tri (ws_tri _ _ _ _ _ _ Hws) _ E.
Notation σ := (suf Fl U).
Notation es := (map (tr_ent Fl) les).

Lemma ctx_ty : forall k, A.nlook k (map (fun q => (fst q, asub σ (snd q))) tm) = option_map (fun y => trF (canon e y)) (tn_get e k).
Proof.
  intro k. rewrite nlook_map_snd, (ws_tn _ _ _ _ _ _ Hws k). destruct (tn_get e k); [|reflexivity].
  cbn. rewrite (sigma_tr Fl U e _ E T). reflexivity.
Qed.

Lemma ctx_vals : forall v, A.nlook v (cvals σ es) = option_map (fun y => trF (canon e y)) (nget (en_vals e) v).
Proof.
  intro v. destruct (A.nlook v (cvals σ es)) as [t|] eqn:H.
  - destruct (cvals_some _ _ _ _ H) as (ty & a & Hi & ->). apply in_map_iff in Hi as (l & El & Hl).
    destruct (tr_ent_val l v ty a El) as (y & a0 & -> & ->).
    destruct (ws_vals _ _ _ _ _ _ Hws v y a0 Hl) as (y0 & _ & G & _ & C). rewrite G. cbn.
    rewrite (sigma_tr Fl U e _ E T), (C E). reflexivity.
  - destruct (nget (en_vals e) v) as [y0|] eqn:G; [|reflexivity]. exfalso.
    destruct (ws_vals_in _ _ _ _ _ _ Hws v y0 G) as (a0 & Hl).
    apply (cvals_none σ es v (trF y0) (tr_inst Fl a0) H). apply (in_map (tr_ent Fl) _ _ Hl).
Qed.

Lemma ctx_fns : forall f, A.nlook f (cfns σ es) = option_map (csig e) (nget (en_fns e) f).
Proof.
  intro f. destruct (A.nlook f (cfns σ es)) as [g|] eqn:H.
  - destruct (cfns_some _ _ _ _ H) as (g1 & a & Hi & ->). apply in_map_iff in Hi as (l & El & Hl).
    destruct (tr_ent_fn l f g1 a El) as (g0 & a0 & -> & ->).
    destruct (ws_fns _ _ _ _ _ _ Hws f g0 a0 Hl) as (_ & g2 & _ & G & _ & C). rewrite G. cbn.
    rewrite (sigcanon_tr Fl U e _ _ E T (C E)). f_equal. exact (sig_tr Fl U e g2 E T).
  - destruct (nget (en_fns e) f) as [g0|] eqn:G; [|reflexivity]. exfalso.
    destruct (ws_fns_in _ _ _ _ _ _ Hws f g0 G) as (a0 & Hl).
    apply (cfns_none σ es f (tr_sig Fl g0) (tr_inst Fl a0) H). apply (in_map (tr_ent Fl) _ _ Hl).
Qed.

(* a method in force, asked at a canonical receiver key that tr reads apart *)
Lemma ctx_mths : forall k m, ety_ok k = true -> canon e k = k ->
  A.mlook (trF k) m (cmths σ es) = option_map (csig e) (get_mth e k m).
Proof.
  intros k m Ok Ck. destruct (A.mlook (trF k) m (cmths σ es)) as [g|] eqn:H.
  - destruct (cmths_some _ _ _ _ _ H) as (r1 & g1 & a & Hi & Er & ->). apply in_map_iff in Hi as (l & El & Hl).
    destruct (tr_ent_mth l r1 m g1 a El) as (r0 & g0 & a0 & -> & -> & ->).
    destruct (ws_mths _ _ _ _ _ _ Hws E r0 m g0 a0 Hl) as (O0 & g2 & G & C).
    rewrite (sigma_tr Fl U e _ E T) in Er. rewrite (tr_inj Fl Finj _ _ O0 Ok Er) in G. rewrite G. cbn.
    rewrite (sigcanon_tr Fl U e _ _ E T C). f_equal. exact (sig_tr Fl U e g2 E T).
  - destruct (get_mth e k m) as [g0|] eqn:G; [|reflexivity]. exfalso.
    destruct (ws_mths_in _ _ _ _ _ _ Hws k m g0 G) as (r0 & a0 & Hl & Rs).
    apply (cmths_none σ es (trF k) m (trF r0) (tr_sig Fl g0) (tr_inst Fl a0) H (in_map (tr_ent Fl) _ _ Hl)).
    rewrite (Rs σ ltac:(rewrite E; exact (suf_resp U T))), (sigma_tr Fl U e _ E T), Ck. reflexivity.
Qed.

End Tables.

(* ======================================================================== *)
(* 7. fn_info on a cache miss: the context its function forms               *)
(* ======================================================================== *)

(* CtxOf with the enclosing brackets apart (IEs), the form the bridge track
   gives ItemsEntries (Audit note 71 on proof-bridge). *)
Definition CtxOfIE (W : A.world) (s : nat) (C : A.cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ,
    A.w_decl W s = Some d /\
    IEs W (A.d_mod d) [] [] (map A.assume_spec (A.d_assumes d)) es tm /\
    A.RootCtx W s es tm er tmr /\
    A.MostGeneralMerge σ (es ++ er) /\ C = A.entries_cx σ tmr (es ++ er) /\ A.Needs σ (es ++ er) ns.

Section FnInfo.
Variables (w : Elab.world) (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis MS : MethStep w R Fl.
Notation WW := (BodySound.W w R Fl).
Notation trF := (tr Fl).

Lemma later_info : forall s c, Later w R Fl (St (st_w s) (st_syms s) (st_next s) (st_reqs s) (st_sigsyms s) c
            (st_fns s) (st_funs s) (st_fwd s) (st_links s) (st_flds s) (st_consts s)
            (st_wrappers s) (st_vars s) (st_nvar s) (st_memory_bound s) (st_memory_used s)
            (st_checked_total s) (st_lang s) (st_unsup s)) -> Later w R Fl s.
Proof. intros s c [L1 L2 L3 L4 L5]. constructor; assumption. Qed.

(* A function g of the program, neither a forwarder nor `main`, whose info
   fn_info computes (not cached): the context it forms, with its needs, and
   that context's tables read off fi_env (canonically, under its merges). *)
Theorem fn_info_ctx : forall g s i s',
  nget (w_syms w) (gs_id g) = Some g -> (forall t, gs_origin g <> OForwarder t) ->
  ~ A.IsMainFn WW (gs_id g) -> (forall s0 m s1, st_w s0 = w -> is_main g s0 = Good (m, s1) -> m = false) ->
  fn_info g s = Good (i, s') -> nget (st_info s) (gs_id g) = None ->
  Inv s -> st_w s = w -> Later w R Fl s' ->
  let e := fi_env i in
  exists C, CtxOfIE WW (gs_id g) C (map (key_tr Fl) (fi_needs i)) /\
    A.c_sub C = suf Fl (en_uf e) /\ fi_merges i = en_uf e /\
    (forall k, A.nlook k (A.c_ty C) = option_map (fun y => trF (canon e y)) (tn_get e k)) /\
    (forall v, A.nlook v (A.c_vals C) = option_map (fun y => trF (canon e y)) (nget (en_vals e) v)) /\
    (forall f, A.nlook f (A.c_fns C) = option_map (csig Fl e) (nget (en_fns e) f)) /\
    (forall k m, ety_ok k = true -> canon e k = k -> A.mlook (trF k) m (A.c_mths C) = option_map (csig Fl e) (get_mth e k m)).
Proof.
  intros g s i s' Hg Nf Nm Nm' H N Hi Hw L e0.
  destruct (fn_info_miss g s i s' H N) as (e & needs & s1 & H1 & -> & ->). subst e0.
  apply later_info in L.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hmi.
  destruct (assumes_merged w R Fl b Hload Finj MS g s e needs s1 Nf Nm' Hmi H1 Hi Hw L)
    as (les & tm & I & Hws & Mg & Nd).
  set (U := en_uf e) in *.
  assert (T : Tri U) by exact (ws_tri _ _ _ _ _ _ Hws).
  assert (Ef : forall t, canon (freeze_all_apps e) t = canon e t) by reflexivity.
  exists (A.entries_cx (suf Fl U) tm (map (tr_ent Fl) les)). cbn [fi_env fi_needs fi_merges].
  split; [|split; [reflexivity|split]].
  - exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)), (map (tr_ent Fl) les), tm, [], tm, (suf Fl U).
    rewrite !app_nil_r. split; [exact (w_decl_rd w R Fl _ _ Hg)|]. split; [exact I|]. split; [|auto].
    apply A.RootNone. left. exact Nm.
  - unfold merge_edges. rewrite (tne_nil _ _ _ _ _ _ Hws). reflexivity.
  - split; [|split; [|split]].
    + intro k. exact (ctx_ty Fl U e tm les needs Hws eq_refl k).
    + intro v. exact (ctx_vals Fl U e tm les needs Hws eq_refl v).
    + intro f. exact (ctx_fns Fl U e tm les needs Hws eq_refl f).
    + intros k m Ok Ck. exact (ctx_mths Fl Finj U e tm les needs Hws eq_refl k m Ok Ck).
Qed.

End FnInfo.
