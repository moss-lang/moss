(* A family's method provision agrees with the code's at a shared run-time
   key (T29): the slice's FamilyCoherentOk (PublicBodySlice), proved.

   Both provisions are readings of the method's one declaration. The code's
   is a reading at some table tm1 (PublicScoping.MethRead, part of a formed
   context's CxScoped); the family's is the reading OIEMeth makes, at a
   table tm2 with θ its AppOf there. The key fixes the receiver; equal θ
   (a method's θ records every symbol its signature mentions, sigclose)
   makes tm1 and tm2 agree, canonically, at every symbol the signature
   mentions. A reading depends only on the meanings at the symbols its type
   expression mentions (FreeIn, through brackets and the relevance-closed
   requirements of nominals and aliases: [tyof_on]), so the two signatures
   are equal.

   The family's method is named in a declaration's (so a loaded) module,
   which makes it a method symbol ([method_named_meth]), the case where θ
   records the mentioned symbols. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicFormation PublicScoping.
From NM Require PublicWorldFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- a method named in a loaded module is a method symbol ------------------- *)

Section Named.
Variables (b : bundle) (W : A.world).
Hypothesis Wo : A.WorldOf b W.
Import A.

Lemma exports_detached : forall p ns n e, Exports b W p ns n e -> forall m, ns = NSDetached -> e = ESym m ->
  kind_sym W m = Some DKMeth.
Proof.
  fix IH 5. intros p ns n e Ex m Ens Ee.
  destruct Ex as [p s d ns0 n0 D _ Fn|p mo t imp u e0 ns0 M I _ _ Ens0 Dis|p mo t imp a M I _].
  - injection Ee as ->. subst ns0. unfold kind_sym. rewrite D.
    destruct (d_ast d) as [|n1|n1 t1|n1 l1 t1|n1|n1 t1 o1|[| |r] n1 ps rt body|n1 l1|n1 o1 l1 l2 o2]; cbn in Fn; try discriminate; try reflexivity;
      injection Fn as E; discriminate E.
  - subst e0. cbn in Ens. subst ns0.
    destruct Dis as [Ex'|[Nd _]]; [exact (IH _ _ _ _ Ex' m Ens eq_refl)|].
    rewrite Nd in Ens. discriminate Ens.
  - discriminate Ee.
Qed.

Lemma method_named_meth : forall p r dn m, Loaded b p -> MethodNamed W p r dn m -> kind_sym W m = Some DKMeth.
Proof.
  intros p r dn m Lp [At|[_ Sc]].
  - apply (proj1 (@wo_attached b W Wo r dn m)) in At. destruct At as (d & rn & ps & rt & body & D & Ast & _).
    unfold kind_sym. rewrite D, Ast. reflexivity.
  - apply (proj1 (@wo_scope b W Wo p NSDetached dn (ESym m) Lp)) in Sc.
    destruct (scope_exports Sc) as [Ex|(mo & t & imp & _ & _ & Ex)];
      exact (exports_detached _ _ _ _ Ex m eq_refl eq_refl).
Qed.

End Named.

(* ---- a reading depends only on the meanings it mentions ---------------------- *)

Section On.
Variable W : A.world.
Variable s : subst.
Hypothesis Closed : A.RelevanceClosed W (A.w_reqs W).
Import A.

Definition AgreeOn (P : nat -> Prop) (tm1 tm2 : list (nat * ty)) : Prop :=
  forall k v1 v2, P k -> nlook k tm1 = Some v1 -> nlook k tm2 = Some v2 -> app_subst s v1 = app_subst s v2.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Lemma nlook_in_some : forall X k (l : list (nat * X)) v, In (k, v) l -> nlook k l <> None.
Proof.
  intros X k l v I. induction l as [|[a w] r IH]; [contradiction|]. cbn.
  destruct (Nat.eqb_spec k a) as [_|Ne]; [discriminate|].
  destruct I as [E|I]; [injection E as -> _; contradiction|exact (IH I)].
Qed.

Lemma Forall2_In_l' : forall X Y (R : X -> Y -> Prop) l l' x, Forall2 R l l' -> In x l -> exists y, In y l' /\ R x y.
Proof.
  intros X Y R l l' x F. induction F as [|a c l l' Rac Rest IH]; intros Ix; [contradiction|].
  destruct Ix as [<-|Ix]; [exists c; split; [left; reflexivity|exact Rac]|].
  destruct (IH Ix) as [y [Iy Ry]]. exists y. split; [right; exact Iy|exact Ry].
Qed.

(* A bracket application binds exactly the symbols its left sides name. *)
Lemma br_keys : forall p tm g x l, Brackets W p tm g x l ->
  forall bb y, In bb (match x with Some bs => bs | None => [] end) -> resolve_sym W p (fst bb) = Some y -> nlook y l <> None.
Proof.
  intros p tm g x l B. destruct B as [p tm g|p tm g bs l F]; [intros bb y []|].
  intros bb y Ib Ry. destruct (Forall2_In_l' _ _ _ _ _ _ F Ib) as [tb [It (sp & sa & _ & R & _)]].
  rewrite Ry in R. injection R as Ey. apply (nlook_in_some _ _ _ (snd tb)). apply (proj1 (in_rev _ _)).
  rewrite Ey. destruct tb. exact It.
Qed.

(* The requirements of a nominal, read through its brackets. *)
Lemma nom_agree : forall p tm1 tm2 pa a g bs1 bs2, resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType ->
  Brackets W p tm1 g a bs1 -> Forall2 (Rel s s) bs1 bs2 -> AgreeOn (FreeIn W (w_reqs W) p (ATRef pa a)) tm1 tm2 ->
  AgreeOn (fun x => In x (w_reqs W g)) (bs1 ++ tm1) (bs2 ++ tm2).
Proof.
  intros p tm1 tm2 pa a g bs1 bs2 R K B BR Ag x v1 v2 Ix L1 L2. rewrite nlook_app' in L1, L2.
  destruct (rel_nlook s s bs1 bs2 BR x) as [Same AgB].
  destruct (nlook x bs1) as [w1|] eqn:K1; destruct (nlook x bs2) as [w2|] eqn:K2.
  - injection L1 as <-. injection L2 as <-. exact (AgB x w1 w2 K1 K2).
  - exfalso. destruct Same as [_ S2]. discriminate (S2 eq_refl).
  - exfalso. destruct Same as [S1 _]. discriminate (S1 eq_refl).
  - apply (Ag x v1 v2); [|exact L1|exact L2]. apply (@FNom W (w_reqs W) p pa a g x R K Ix).
    intros [bb [Ib Rb]]. exact (br_keys _ _ _ _ _ B bb x Ib Rb K1).
Qed.

Lemma brackets_agree : forall p pa a tm1 tm2, resolve_sym W p pa <> None ->
  AgreeOn (FreeIn W (w_reqs W) p (ATRef pa a)) tm1 tm2 ->
  forall bb sp sa, In bb (match a with Some bs => bs | None => [] end) -> snd bb = Spec (Some sp) None sa ->
    AgreeOn (FreeIn W (w_reqs W) p (ATRef sp sa)) tm1 tm2.
Proof.
  intros p pa a tm1 tm2 R Ag bb sp sa Ib Eb k v1 v2 Fk. apply Ag.
  destruct bb as [bp sb]. cbn in Eb. subst sb. exact (@FBracket W (w_reqs W) p pa a bp sp None sa k R Ib Fk).
Qed.

Lemma args_rel : forall reqs bt1 bt2 args1 args2, AgreeOn (fun x => In x reqs) bt1 bt2 ->
  Forall2 (fun r ra => fst ra = r /\ nlook r bt1 = Some (snd ra)) reqs args1 ->
  Forall2 (fun r ra => fst ra = r /\ nlook r bt2 = Some (snd ra)) reqs args2 -> Forall2 (Rel s s) args1 args2.
Proof.
  intros reqs bt1 bt2 args1 args2 Ag F1. revert args2.
  induction F1 as [|r ra rs ras [E1 L1] Fr IH]; intros args2 F2; inversion F2 as [|? ra2 ? ras2 [E2 L2] Fr2]; subst;
    constructor.
  - split; [congruence|]. exact (Ag _ _ _ (or_introl eq_refl) L1 L2).
  - apply IH; [|exact Fr2]. intros k v1 v2 Ik. exact (Ag k v1 v2 (or_intror Ik)).
Qed.

Fixpoint tyof_on p tm1 th1 a t1 (D : TyOf W p tm1 th1 a t1) {struct D} :
  forall tm2 th2 t2, TyOf W p tm2 th2 a t2 -> AgreeOn (FreeIn W (w_reqs W) p a) tm1 tm2 ->
  (forall x1 x2, th1 = Some x1 -> th2 = Some x2 -> app_subst s x1 = app_subst s x2) ->
  app_subst s t1 = app_subst s t2
with br_on p tm1 g x l1 (D : Brackets W p tm1 g x l1) {struct D} :
  forall tm2 l2, Brackets W p tm2 g x l2 ->
  (forall bb sp sa, In bb (match x with Some bs => bs | None => [] end) -> snd bb = Spec (Some sp) None sa ->
     AgreeOn (FreeIn W (w_reqs W) p (ATRef sp sa)) tm1 tm2) ->
  Forall2 (Rel s s) l1 l2.
Proof.
  - destruct D as [p tm th|p tm th|p tm t|p tm th fs ts Nd F|p tm th ms ts flat F Ef Fm Nd
                  |p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|p tm th pa a g d n body bs tau R D A B T];
      intros tm2 th2 t2 D2 Ag This; inversion D2; subst; clear D2.
    + reflexivity.
    + reflexivity.
    + apply This; reflexivity.
    + rewrite !subst_record. f_equal.
      match goal with H0 : Forall2 _ fs ?ts0 |- _ => assert_fails (constr_eq H0 F); rename H0 into F2; rename ts0 into ts2 end.
      repeat match goal with H : NoDup _ |- _ => clear H end.
      assert (AgF : forall nt, In nt fs -> AgreeOn (FreeIn W (w_reqs W) p (snd nt)) tm tm2).
      { intros [nm at0] I k v1 v2 Fk. apply Ag. exact (@FRec W (w_reqs W) p fs nm at0 k I Fk). }
      clear Ag. revert ts2 F2. induction F as [|nt ft fs' ts' [E1 Hft] Fr IH]; intros ts2 F2;
        inversion F2 as [|? ft2 ? ts2' [E2 Hft2] Fr2]; subst; [reflexivity|].
      cbn. f_equal; [|apply IH; [intros nt0 I0; exact (AgF nt0 (or_intror I0))|assumption]].
      destruct ft, ft2; cbn in *. f_equal; [congruence|].
      exact (tyof_on _ _ _ _ _ Hft _ _ _ Hft2 (AgF nt (or_introl eq_refl)) This).
    + match goal with H0 : Forall2 _ ms ?ts0 |- _ => rename H0 into F2; rename ts0 into ts2 end.
      match goal with H0 : Forall _ (flat_map members ts2) |- _ => rename H0 into Fm2 end.
      rewrite !subst_union. f_equal.
      rewrite <- (flat_sub s ts Fm), <- (flat_sub s ts2 Fm2). f_equal.
      clear Fm Fm2. repeat match goal with H : NoDup _ |- _ => clear H end.
      assert (AgM : forall t, In t ms -> AgreeOn (FreeIn W (w_reqs W) p t) tm tm2).
      { intros t I k v1 v2 Fk. apply Ag. exact (@FUnion W (w_reqs W) p ms t k I Fk). }
      clear Ag. revert ts2 F2. induction F as [|m t ms' ts' Ht Fr IH]; intros ts2 F2;
        inversion F2; subst; [reflexivity|].
      cbn. f_equal; [|apply IH; [intros t0 I0; exact (AgM t0 (or_intror I0))|assumption]].
      exact (tyof_on _ _ _ _ _ Ht _ _ _ ltac:(eassumption) (AgM m (or_introl eq_refl)) This).
    + match goal with H0 : resolve_sym W p pa = Some ?t' |- _ => assert (t' = t) by congruence; subst t' end.
      apply (Ag t); [|eassumption|eassumption].
      apply (@FType W (w_reqs W) p pa None t R K). intros [bb [[] _]].
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => rename H0 into B2; rename bs0 into bs2 end.
      assert (Rn : resolve_sym W p pa <> None) by (rewrite R; discriminate).
      pose proof (br_on _ _ _ _ _ B _ _ B2 (brackets_agree p pa a tm tm2 Rn Ag)) as BR.
      assert (Kn : kind_sym W g <> Some DKType) by (rewrite K; destruct Kk as [->| ->]; discriminate).
      pose proof (nom_agree p tm tm2 pa a g bs bs2 R Kn B BR Ag) as Ag'.
      rewrite !subst_nom. f_equal. apply (smap_rel s s).
      match goal with H0 : Forall2 _ (w_reqs W g) ?a0 |- _ => rename H0 into F2; rename a0 into args2 end.
      exact (args_rel _ _ _ _ _ Ag' F F2).
    + kmis.
    + kmis.
    + kmis.
    + assert (g0 = g) by congruence. subst g0. assert (d0 = d) by congruence. subst d0.
      rewrite A in *. match goal with H0 : ADAlias _ _ = ADAlias _ _ |- _ => injection H0 as <- <- end.
      match goal with H0 : Brackets _ _ _ _ _ ?bs0 |- _ => rename H0 into B2; rename bs0 into bs2 end.
      assert (Rn : resolve_sym W p pa <> None) by (rewrite R; discriminate).
      pose proof (br_on _ _ _ _ _ B _ _ B2 (brackets_agree p pa a tm tm2 Rn Ag)) as BR.
      assert (Kn : kind_sym W g <> Some DKType) by (unfold kind_sym; rewrite D, A; discriminate).
      pose proof (nom_agree p tm tm2 pa a g bs bs2 R Kn B BR Ag) as Ag'.
      apply (tyof_on _ _ _ _ _ T _ _ _ ltac:(eassumption)); [|intros x1 x2 E; discriminate E].
      intros k v1 v2 Fk. apply Ag'. exact (proj2 (Closed g d [] body D ltac:(rewrite A; reflexivity)) k Fk).
  - destruct D as [p tm g|p tm g bs l F]; intros tm2 l2 D2 Ag; inversion D2; subst; clear D2; [constructor|].
    apply forall2_rev. rename l into l1.
    match goal with H0 : Forall2 _ bs ?l0 |- _ => rename H0 into F2; rename l0 into l2' end.
    revert l2' F2. induction F as [|b0 tb bs' l1' (sp & sa & E1 & R1 & K1 & Q1 & T1) Fr IH]; intros l2' F2;
      inversion F2 as [|? tb2 ? l2'' (sp' & sa' & E2 & R2 & K2 & Q2 & T2) Fr2]; subst; constructor;
      [|apply IH; [intros bb sp0 sa0 Ib Eb; exact (Ag bb sp0 sa0 (or_intror Ib) Eb)|assumption]].
    rewrite E1 in E2. injection E2 as <- <-.
    split; [congruence|].
    exact (tyof_on _ _ _ _ _ T1 _ _ _ T2 (Ag b0 sp sa (or_introl eq_refl) E1) (fun _ _ E _ => ltac:(discriminate E))).
Qed.

(* Two readings of one signature agree where the meanings of the symbols it
   mentions do. *)
Lemma sig_on : forall f tm1 tm2 th1 th2 g1 g2,
  SigOf W f tm1 th1 g1 -> SigOf W f tm2 th2 g2 -> AgreeOn (SigMentions W f) tm1 tm2 ->
  (forall x1 x2, th1 = Some x1 -> th2 = Some x2 -> app_subst s x1 = app_subst s x2) ->
  sig_subst s g1 = sig_subst s g2.
Proof.
  intros f tm1 tm2 th1 th2 [ps1 r1] [ps2 r2] (d & rcv & n & ps & rt & body & D & Ast & P1 & R1)
    (d' & rcv' & n' & ps' & rt' & body' & D' & Ast' & P2 & R2) Ag This.
  rewrite D in D'. injection D' as <-. rewrite Ast in Ast'. injection Ast' as <- <- <- <- <-. cbn in *.
  unfold sig_subst. cbn. f_equal.
  - assert (AgP : forall pt, In pt ps -> AgreeOn (FreeIn W (w_reqs W) (d_mod d) (snd pt)) tm1 tm2).
    { intros pt I k v1 v2 Fk. apply Ag. exists d, rcv, n, ps, rt, body. split; [exact D|split; [exact Ast|]].
      left. exists pt. split; assumption. }
    clear R1 R2 D Ast Ag. revert ps2 P2. induction P1 as [|pt t ps0 ts Ht Fr IH]; intros ps2 P2;
      inversion P2; subst; [reflexivity|]. cbn. f_equal; [|apply IH; [intros pt0 I0; exact (AgP pt0 (or_intror I0))|assumption]].
    exact (tyof_on _ _ _ _ _ Ht _ _ _ ltac:(eassumption) (AgP pt (or_introl eq_refl)) This).
  - destruct rt as [r|]; [|rewrite R1, R2; reflexivity].
    apply (tyof_on _ _ _ _ _ R1 _ _ _ R2); [|exact This].
    intros k v1 v2 Fk. apply Ag. exists d, rcv, n, ps, (Some r), body. split; [exact D|split; [exact Ast|]].
    right. exists r. split; [reflexivity|exact Fk].
Qed.

End On.

(* ---- the family agrees with the code's context ------------------------------- *)

Section Coherent.
Variables (b : bundle) (W : A.world).
Hypothesis Wo : A.WorldOf b W.
Import A.

Lemma ilook_some : forall X k (l : list (item * X)) v, In (k, v) l -> ilook k l <> None.
Proof.
  intros X k l v I. induction l as [|[k' w] r IH]; [contradiction|]. cbn.
  destruct (item_eqb k k') eqn:E; [discriminate|].
  destruct I as [Eq|I]; [|exact (IH I)]. injection Eq as -> _.
  rewrite (proj2 (PublicCallRealize.item_eqb_spec k k) eq_refl) in E. discriminate E.
Qed.

Lemma ilook_app_l : forall X k (l1 l2 : list (item * X)), ilook k l1 <> None -> ilook k (l1 ++ l2) = ilook k l1.
Proof.
  intros X k l1 l2. induction l1 as [|[k' w] r IH]; intros N; [contradiction N; reflexivity|]. cbn in *.
  destruct (item_eqb k k'); [reflexivity|exact (IH N)].
Qed.

(* A family result's provision at one of its keys: OIEMeth's reading. *)
Lemma family_member : forall u C out Cr ks r m g a, ProvideResult W u C out Cr ks -> In (mkey r m) ks ->
  ilook (mkey r m) (c_provisions Cr) = Some (ProvidedMethod g a) ->
  exists tm rt g0, SigOf W m tm (Some rt) g0 /\ AppOf W m tm a /\ canon C rt = r /\ g = canon_sig C g0 /\
    kind_sym W m = Some DKMeth.
Proof.
  intros u C out Cr ks r m g a (d & ps & a0 & r0 & b0 & outs & es & tm' & D & FP & Outs & OIE & RK & ->) Ik L.
  pose proof (proj2 (output_read W) _ _ _ _ _ _ OIE (ex_intro _ u (ex_intro _ d (conj D eq_refl)))) as Rd.
  cbn [output_cx c_provisions] in L.
  set (nw := flat_map (entry_provision (c_sub C) (fun a => a)) (rev es)) in L.
  assert (Nw : ilook (mkey r m) nw <> None).
  { rewrite RK in Ik. apply in_flat_map in Ik as [e [Ie Ik]].
    destruct e as [t|v t a1|f g1 a1|rt m1 g1 a1|u1 fx|eql eqr]; cbn in Ik; try contradiction;
      destruct Ik as [E|[]]; try discriminate.
    apply (ilook_some _ _ _ (ProvidedMethod (sig_subst (c_sub C) g1) a1)). unfold nw. apply in_flat_map.
    exists (NMth rt m1 g1 a1). split; [apply (proj1 (in_rev _ _)); exact Ie|]. cbn. left. rewrite E. reflexivity. }
  rewrite (ilook_app_l _ _ _ _ Nw) in L. apply PublicFormation.ilook_in_key in L.
  unfold nw in L. apply in_flat_map in L as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie.
  destruct e as [t|v t a1|f g1 a1|rt m1 g1 a1|u1 fx|eql eqr]; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; try discriminate.
  try unfold mkey in E. injection E as Er Em Eg Ea. subst r m1 g a1.
  destruct (Rd rt m g1 a Ie) as [tmx [Sg [App (s0 & ds & r1 & dn & Ds & Mn)]]].
  exists tmx, rt, g1. split; [exact Sg|split; [exact App|split; [reflexivity|split; [reflexivity|]]]].
  exact (method_named_meth b W Wo _ _ _ _ (PublicWorldFormation.decl_loaded b W Wo s0 ds Ds) Mn).
Qed.

Theorem family_coherent : forall F C, FormedCx W C /\ FrameFormed W F /\ CxScoped W F C ->
  forall C1 u matching th sm Cr ks, c_sub C1 = c_sub C -> incl (c_provisions C) (c_provisions C1) ->
  FamilyApplication W C1 u matching th sm Cr ks ->
  forall r m g a g' a', In (mkey r m) ks ->
  In (mkey r m, ProvidedMethod g a) (c_provisions C) ->
  ilook (mkey r m) (c_provisions Cr) = Some (ProvidedMethod g' a') ->
  theta_of C a = theta_of Cr a' -> g = g'.
Proof.
  intros F C [_ [_ [_ [_ [_ [_ [_ Pr]]]]]]] C1 u matching th sm Cr ks Es _ FA r m g a g' a' Ik Il L Eth.
  pose proof FA as (fx & Cu & ns & Fx & Fam & Poly & FC & Th & InF & FAg & Sat & PR & Nd).
  destruct (family_context_shape matching C1) as [_ [Esub _]].
  destruct (provide_result_shape W _ _ _ _ _ PR) as [SubR _].
  assert (Ss : c_sub Cr = c_sub C) by (rewrite SubR, Esub; exact Es).
  destruct (Pr r m g a Il) as (tm1 & rt1 & g1 & Sg1 & Er1 & -> & P1).
  destruct (family_member _ _ _ _ _ r m g' a' PR Ik L) as (tm2 & rt2 & g2 & Sg2 & App2 & Er2 & -> & Km).
  unfold canon_sig, canon in *. rewrite Esub, Es in Er2 |- *.
  change (sig_subst (c_sub C) g1 = sig_subst (c_sub C) g2).
  apply (sig_on W (c_sub C) (@wo_reqs_closed b W Wo) m tm1 tm2 (Some rt1) (Some rt2) g1 g2 Sg1 Sg2).
  - intros x v1 v2 Mx L1 L2. pose proof (P1 Km x v1 Mx L1) as I1. rewrite Eth in I1.
    unfold theta_of, known in I1. apply in_map_iff in I1 as [[x' t] [E It]]. cbn in E.
    injection E as -> Et. apply in_flat_map in It as [[y o] [Iy It]].
    destruct o as [t'|]; cbn in It; [|contradiction]. destruct It as [Eq|[]]. injection Eq as -> <-.
    destruct App2 as (reqs & -> & _). apply in_map_iff in Iy as [z [Ez _]]. injection Ez as -> Ez.
    rewrite L2 in Ez. injection Ez as ->. unfold canon in Et. rewrite Ss in Et. symmetry. exact Et.
  - intros x1 x2 E1 E2. injection E1 as <-. injection E2 as <-. rewrite Er1, Er2. reflexivity.
Qed.

End Coherent.
