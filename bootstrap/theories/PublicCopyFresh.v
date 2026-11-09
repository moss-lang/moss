(* The substitution argument behind a copy step (copyfresh, realizepf).
   CopyOutputs (Audit) compares F's and H's result provisions after
   sigma ++ [y := cg y | y in ys]: sigma is F's fixed readings at the copy
   site, ys the generations of F's input readings sigma does not read, and
   each cg y a fresh symbol. An application of F reads F's symbols at some
   meanings tau. This file shows that the comparison transfers to tau: read
   the compared provisions at rho, which sends cg y to tau's meaning of y
   and is rho0 elsewhere, and both sides become their reading at tau.

   The transfer needs that no cg y occurs in sigma's image (Fix below:
   a fixed reading's generations avoid the cg symbols). Audit's
   CopyOutputs requires this since realizepf (Fresh W only says
   undeclared, and a copy site can read a fixed input as a fresh
   application generation: examples/copy-fresh-collide.moss).

   Also: grounding after a substitution is one substitution
   (ground_app_subst), and the lemmas lift from types to provisions. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit PublicTypeRead PublicCallRealize.
From NM Require PublicFormation PublicScoping PublicCallerKeys PublicFamilyStep.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- composing substitutions ------------------------------------------------ *)

Lemma lookup_subst_map_app : forall (f : ty -> ty) x (s r : subst),
  lookup_subst x (map (fun q => (fst q, f (snd q))) s ++ r) =
  match lookup_subst x s with Some v => Some (f v) | None => lookup_subst x r end.
Proof. intros f x s r. induction s as [|[y v] rest IH]; [reflexivity|]. cbn. destruct (Nat.eqb x y); [reflexivity|exact IH]. Qed.

Lemma lookup_subst_cat : forall x (l1 l2 : subst),
  lookup_subst x (l1 ++ l2) = match lookup_subst x l1 with Some v => Some v | None => lookup_subst x l2 end.
Proof. intros x l1 l2. induction l1 as [|[y v] rest IH]; [reflexivity|]. cbn. destruct (Nat.eqb x y); [reflexivity|exact IH]. Qed.

(* Two substitutions in turn agree with a third on a type when they agree
   on each generation it mentions. *)
Lemma ty_agree : forall t s1 s2 s3,
  (forall x, In x (A.gens t) -> app_subst s1 (app_subst s2 (TAbs x)) = app_subst s3 (TAbs x)) ->
  app_subst s1 (app_subst s2 t) = app_subst s3 t.
Proof.
  intros t s1 s2 s3 Ag. rewrite <- (app_subst_nil (app_subst s3 t)). apply subst2_agree.
  intros x Ix. rewrite (Ag x Ix). symmetry. apply app_subst_nil.
Qed.

(* Grounding after a substitution is one substitution. *)
Lemma ground_app_subst : forall d s t,
  ground d (app_subst s t) = app_subst (map (fun q => (fst q, ground d (snd q))) s ++ type_meanings d) t.
Proof.
  intros d s t. unfold ground. apply ty_agree. intros x _. cbn [app_subst].
  rewrite (lookup_subst_map_app (app_subst (type_meanings d)) x s (type_meanings d)).
  destruct (lookup_subst x s) as [v|]; reflexivity.
Qed.

Lemma tys_agree : forall ts s1 s2 s3,
  (forall x, In x (flat_map A.gens ts) -> app_subst s1 (app_subst s2 (TAbs x)) = app_subst s3 (TAbs x)) ->
  map (app_subst s1) (map (app_subst s2) ts) = map (app_subst s3) ts.
Proof.
  induction ts as [|t r IH]; intros s1 s2 s3 Ag; [reflexivity|]. cbn.
  rewrite (ty_agree t s1 s2 s3), (IH s1 s2 s3); [reflexivity| |];
    intros x Ix; apply Ag; cbn; apply in_or_app; [right|left]; exact Ix.
Qed.

Lemma inst_agree : forall (a : A.inst) s1 s2 s3,
  (forall x, In x (flat_map (fun q => match snd q with Some t => A.gens t | None => [] end) a) ->
     app_subst s1 (app_subst s2 (TAbs x)) = app_subst s3 (TAbs x)) ->
  map (fun q => (fst q, option_map (app_subst s1) (snd q))) (map (fun q => (fst q, option_map (app_subst s2) (snd q))) a) =
  map (fun q => (fst q, option_map (app_subst s3) (snd q))) a.
Proof.
  induction a as [|[y [t|]] r IH]; intros s1 s2 s3 Ag; [reflexivity| |]; cbn.
  - rewrite (ty_agree t s1 s2 s3), (IH s1 s2 s3); [reflexivity| |];
      intros x Ix; apply Ag; cbn; apply in_or_app; [right|left]; exact Ix.
  - rewrite (IH s1 s2 s3); [reflexivity|]. intros x Ix. apply Ag. exact Ix.
Qed.

(* The same, for a provision: its signature and recorded instantiation. *)
Lemma provision_agree : forall p s1 s2 s3,
  (forall x, In x (A.provision_gens p) -> app_subst s1 (app_subst s2 (TAbs x)) = app_subst s3 (TAbs x)) ->
  A.provision_subst s1 (A.provision_subst s2 p) = A.provision_subst s3 p.
Proof.
  intros [t a|g a|g a|fx] s1 s2 s3 Ag; unfold A.provision_gens in Ag; cbn [A.provision_signature A.provision_inst] in Ag;
    cbn [A.provision_subst].
  - rewrite (ty_agree t s1 s2 s3), (inst_agree a s1 s2 s3); [reflexivity| |];
      intros x Ix; apply Ag; apply in_or_app; [right; exact Ix|left; cbn; rewrite app_nil_r; exact Ix].
  - destruct g as [ps rt]. unfold A.sig_subst. cbn [A.sg_params A.sg_ret] in *.
    rewrite (ty_agree rt s1 s2 s3), (tys_agree ps s1 s2 s3), (inst_agree a s1 s2 s3); [reflexivity| | |];
      intros x Ix; apply Ag; apply in_or_app; [right; exact Ix|left; cbn; apply in_or_app; right; exact Ix|left; cbn; apply in_or_app; left; exact Ix].
  - destruct g as [ps rt]. unfold A.sig_subst. cbn [A.sg_params A.sg_ret] in *.
    rewrite (ty_agree rt s1 s2 s3), (tys_agree ps s1 s2 s3), (inst_agree a s1 s2 s3); [reflexivity| | |];
      intros x Ix; apply Ag; apply in_or_app; [right; exact Ix|left; cbn; apply in_or_app; right; exact Ix|left; cbn; apply in_or_app; left; exact Ix].
  - f_equal. rewrite map_map. apply map_ext_in. intros [y t] It. cbn. f_equal. apply ty_agree.
    intros x Ix. apply Ag. apply in_or_app. right. apply in_flat_map. exists (y, Some t). split; [|exact Ix].
    apply in_map_iff. exists (y, t). split; [reflexivity|exact It].
Qed.

(* ---- the copy-step substitution --------------------------------------------- *)

Section CopyRead.
Variables (sigma : subst) (ys : list nat) (cg : nat -> nat) (tau rho0 : subst).

Definition cg_map : subst := map (fun y => (y, TAbs (cg y))) ys.
Definition cg_read : subst := map (fun y => (cg y, app_subst tau (TAbs y))) ys ++ rho0.

Hypothesis Ninj : NoDup (map cg ys).

Lemma lookup_cg_map_out : forall x, ~ In x ys -> lookup_subst x cg_map = None.
Proof.
  intros x N. unfold cg_map. induction ys as [|y r IH]; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [exfalso; apply N; left; reflexivity|].
  apply IH; [inversion Ninj; assumption|]. intros I. apply N. right. exact I.
Qed.

Lemma lookup_cg_map_in : forall x, In x ys -> lookup_subst x cg_map = Some (TAbs (cg x)).
Proof.
  intros x. unfold cg_map. clear Ninj. induction ys as [|y r IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x y) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma lookup_cg_read_in : forall y, In y ys -> lookup_subst (cg y) cg_read = Some (app_subst tau (TAbs y)).
Proof.
  intros y. unfold cg_read. induction ys as [|z r IH]; intros I; [contradiction|]. cbn.
  inversion Ninj as [|a b Nz Nr]; subst.
  destruct (Nat.eqb_spec (cg y) (cg z)) as [E|Ne].
  - destruct I as [<-|I]; [reflexivity|]. exfalso. apply Nz. rewrite <- E. apply in_map. exact I.
  - destruct I as [<-|I]; [congruence|]. exact (IH Nr I).
Qed.

Lemma lookup_cg_read_out : forall z, ~ In z (map cg ys) -> lookup_subst z cg_read = lookup_subst z rho0.
Proof.
  intros z N. unfold cg_read. clear Ninj. induction ys as [|y r IH]; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec z (cg y)) as [->|Ne]; [exfalso; apply N; left; reflexivity|].
  apply IH. intros I. apply N. right. exact I.
Qed.

(* Off the cg symbols, cg_read is rho0. *)
Lemma cg_read_off : forall t, (forall z, In z (A.gens t) -> ~ In z (map cg ys)) ->
  app_subst cg_read t = app_subst rho0 t.
Proof.
  intros t N. apply subst_agree_on. intros z Iz. cbn [app_subst]. rewrite (lookup_cg_read_out z (N z Iz)). reflexivity.
Qed.

(* What tau must make of a generation x of a compared provision: a fixed
   one (sigma reads it) its reading avoiding the cg symbols and meaning at
   rho0 what tau says; any other, outside ys (an output role, say), is no
   cg symbol and means at rho0 what tau says. A generation in ys that
   sigma does not read needs nothing: cg_read takes cg y to tau's y. *)
Definition CopyGen (x : nat) : Prop :=
  (forall s, lookup_subst x sigma = Some s ->
     (forall z, In z (A.gens s) -> ~ In z (map cg ys)) /\ app_subst rho0 s = app_subst tau (TAbs x)) /\
  (lookup_subst x sigma = None -> ~ In x ys -> ~ In x (map cg ys) /\ app_subst rho0 (TAbs x) = app_subst tau (TAbs x)).

Lemma copy_gen_read : forall x, CopyGen x ->
  app_subst cg_read (app_subst (sigma ++ cg_map) (TAbs x)) = app_subst tau (TAbs x).
Proof.
  intros x [Fx Ot].
  change (app_subst (sigma ++ cg_map) (TAbs x)) with
    (match lookup_subst x (sigma ++ cg_map) with Some v => v | None => TAbs x end).
  rewrite lookup_subst_cat.
  destruct (lookup_subst x sigma) as [s|] eqn:Ls.
  - destruct (Fx s eq_refl) as [Av E]. rewrite cg_read_off; [exact E|exact Av].
  - destruct (List.In_dec Nat.eq_dec x ys) as [I|N].
    + rewrite (lookup_cg_map_in x I).
      change (app_subst cg_read (TAbs (cg x))) with
        (match lookup_subst (cg x) cg_read with Some v => v | None => TAbs (cg x) end).
      rewrite (lookup_cg_read_in x I). reflexivity.
    + rewrite (lookup_cg_map_out x N). destruct (Ot eq_refl N) as [Nc E]. rewrite <- E.
      change (app_subst cg_read (TAbs x)) with
        (match lookup_subst x cg_read with Some v => v | None => TAbs x end).
      rewrite (lookup_cg_read_out x Nc). reflexivity.
Qed.

Lemma copy_ty_read : forall t, (forall x, In x (A.gens t) -> CopyGen x) ->
  app_subst cg_read (app_subst (sigma ++ cg_map) t) = app_subst tau t.
Proof. intros t G. apply ty_agree. intros x Ix. exact (copy_gen_read x (G x Ix)). Qed.

Lemma copy_provision_read : forall p, (forall x, In x (A.provision_gens p) -> CopyGen x) ->
  A.provision_subst cg_read (A.provision_subst (sigma ++ cg_map) p) = A.provision_subst tau p.
Proof. intros p G. apply provision_agree. intros x Ix. exact (copy_gen_read x (G x Ix)). Qed.

(* CopyOutputs' equation, read at an application's meanings tau. *)
Theorem copy_outputs_at : forall p p',
  A.provision_subst (sigma ++ cg_map) p = A.provision_subst (sigma ++ cg_map) p' ->
  (forall x, In x (A.provision_gens p) -> CopyGen x) -> (forall x, In x (A.provision_gens p') -> CopyGen x) ->
  A.provision_subst tau p = A.provision_subst tau p'.
Proof.
  intros p p' E G G'. rewrite <- (copy_provision_read p G), <- (copy_provision_read p' G'), E. reflexivity.
Qed.

End CopyRead.

(* ---- what a functor's result mentions -------------------------------------- *)

(* The generations of a result read in F's own context Cu with output roles
   gen are Cu's table keys (declared type symbols) or roles. So no fresh
   cg y is one of them unless it is a role (CopyGen's side conditions). *)

Section ResultGens.
Variable W : A.world.
Import A.

Lemma ies_tm_abs :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' ->
     (forall x v, In (x, v) tm -> v = TAbs x) -> forall x v, In (x, v) tm' -> v = TAbs x) /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' ->
     (forall x v, In (x, v) tm -> v = TAbs x) -> forall x v, In (x, v) tm' -> v = TAbs x).
Proof.
  apply (PublicCallRealize.ies_mut W
    (fun p ob tm sp es tm' _ => (forall x v, In (x, v) tm -> v = TAbs x) -> forall x v, In (x, v) tm' -> v = TAbs x)
    (fun p ob tm sps es tm' _ => (forall x v, In (x, v) tm -> v = TAbs x) -> forall x v, In (x, v) tm' -> v = TAbs x));
    intros; eauto.
  destruct (nlook t tm); [eauto|]. destruct H0 as [E|I]; [injection E as <- <-; reflexivity|eauto].
Qed.

Lemma functx_ty_abs : forall u Cu ns, FunCtx W u Cu ns -> forall x v, In (x, v) (c_ty Cu) -> v = app_subst (c_sub Cu) (TAbs x).
Proof.
  intros u Cu ns (d & ps & a & r & b & es & tm & tm' & σ & implicit & D & FP & IE & Imp & -> & MGM & -> & _ & _) x v I.
  cbn [entries_cx c_ty c_sub] in *. apply in_map_iff in I as [[y w] [E I]]. cbn in E. injection E as -> <-. f_equal.
  apply in_app_or in I as [I|I].
  - apply in_map_iff in I as [z [E _]]. injection E as -> <-. reflexivity.
  - exact (proj2 ies_tm_abs _ _ _ _ _ _ IE ltac:(intros ? ? []) x w I).
Qed.

Lemma subst_keys : forall (s : subst) (K : list nat) t, (forall g u, In (g, u) s -> incl (gens u) K) ->
  incl (gens t) K -> incl (gens (app_subst s t)) K.
Proof.
  intros s K t R Inc y Iy. destruct (gens_subst t s y Iy) as [y0 [I0 [[_ E]|[u [L Iu]]]]]; [subst y0; exact (Inc y I0)|].
  exact (R y0 u (lookup_subst_in_pair _ _ _ L) y Iu).
Qed.

(* Context formation's premises, as the body slice assumes them (T29). *)
Hypothesis merge_formed : PublicFormation.MergeFormed W.
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

Lemma functx_keys_declared : forall u Cu ns, FunCtx W u Cu ns -> forall x, In x (map fst (c_ty Cu)) -> ~ Fresh W x.
Proof.
  intros u Cu ns Fc x I Fr. destruct (PublicFormation.functx_cx_formed W merge_formed mentions_typed u Cu ns Fc) as [_ [_ Ty]].
  pose proof (Ty x I) as K. unfold kind_sym, Fresh in *. rewrite Fr in K. discriminate.
Qed.

(* Every provision a ProvideResult reading in F's context gives one of its
   keys mentions only Cu's table keys and the output roles. *)
Theorem result_gens : forall u0 Cu ns v gen Cr ks, FunCtx W u0 Cu ns ->
  ProvideResult W v Cu (fun t => TAbs (gen t)) Cr ks -> (forall t, FunOutputs W v t -> Fresh W (gen t)) ->
  forall k p, In k ks -> ilook (provision_id k) (c_provisions Cr) = Some p ->
  forall y, In y (provision_gens p) -> In y (map fst (c_ty Cu)) \/ exists t, FunOutputs W v t /\ y = gen t.
Proof.
  intros u0 Cu ns v gen Cr ks Fc PR FrG k p Ik L.
  pose proof (PublicCallerKeys.result_key_id W v Cu _ Cr ks k PR Ik) as Id. rewrite Id in L.
  destruct (functx_scoped W u0 Cu ns Fc) as (_ & _ & _ & _ & _ & _ & SubSc).
  set (K := fun y => In y (map fst (c_ty Cu)) \/ exists t, FunOutputs W v t /\ y = gen t).
  assert (SubK : forall g u, In (g, u) (c_sub Cu) -> PublicFormation.GensIn K (gens u)).
  { intros g u Iq y Iy. left. exact (proj2 (SubSc g u Iq) y Iy). }
  assert (Dom : forall g, lookup_subst g (c_sub Cu) <> None -> In g (map fst (c_ty Cu))).
  { intros g Lg. destruct (lookup_subst g (c_sub Cu)) as [u|] eqn:E; [|congruence].
    exact (proj1 (SubSc g u (lookup_subst_in_pair _ _ _ E))). }
  assert (Range : forall g u, In (g, u) (c_sub Cu) -> incl (gens u) (map fst (c_ty Cu))) by (intros g u Iq; exact (proj2 (SubSc g u Iq))).
  assert (TyK : forall x w, In (x, w) (c_ty Cu) -> PublicFormation.GensIn K (gens (canon Cu w))).
  { intros x w Iq y Iy. left. rewrite (functx_ty_abs u0 Cu ns Fc x w Iq) in Iy. unfold canon in Iy.
    refine (subst_keys _ _ _ Range (subst_keys _ _ _ Range _) y Iy).
    intros z [<-|[]]. change x with (fst (x, w)). apply in_map. exact Iq. }
  assert (OutK : forall t, FunOutputs W v t -> PublicFormation.GensIn K (gens (canon Cu (TAbs (gen t))))).
  { intros t Ft y Iy. unfold canon in Iy. cbn [app_subst] in Iy.
    destruct (lookup_subst (gen t) (c_sub Cu)) as [u|] eqn:E.
    - exfalso. apply (functx_keys_declared u0 Cu ns Fc (gen t)); [apply Dom; rewrite E; discriminate|exact (FrG t Ft)].
    - destruct Iy as [<-|[]]. right. exists t. split; [exact Ft|reflexivity]. }
  pose proof PR as (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & EC).
  set (news := flat_map (entry_provision (c_sub Cu) (fun a => a)) (rev es)).
  assert (En : c_provisions Cr = (news ++ c_provisions Cu)%list) by (rewrite EC; reflexivity).
  destruct (PublicCallerKeys.provide_result_key_news W v Cu _ Cr ks k PR Ik news En) as [p0 I0].
  destruct (PublicFamilyStep.ilook_of_in _ k news p0 I0) as [p' [L' I']].
  rewrite En, (PublicFamilyStep.ilook_app_some _ k news _ p' L') in L. injection L as <-.
  assert (Sc0 : PublicFormation.GensIn K (PublicTypeRead.tm_gens (output_type_map Cu outs (fun t => TAbs (gen t))))).
  { intros g Ig. destruct (PublicFormation.tm_gens_in _ g Ig) as [x [w [Iq Iw]]].
    unfold output_type_map in Iq. apply in_map_iff in Iq as [[y0 w0] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. exact (OutK t (proj1 (Outs t) It) g Iw).
    - exact (TyK _ _ Iq g Iw). }
  pose proof (proj2 (PublicScoping.output_entries_gens W K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  unfold news in I'. apply in_flat_map in I' as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  exact (PublicScoping.entry_provision_gens K _ e k _ SubK (Es e Ie) Iq).
Qed.

Lemma nodup_app_disj : forall (l1 l2 : list nat) a, NoDup (l1 ++ l2) -> In a l1 -> ~ In a l2.
Proof.
  induction l1 as [|b r IH]; intros l2 a Nd I I2; [contradiction|]. inversion Nd as [|c d Nb Nr]; subst.
  destruct I as [<-|I]; [apply Nb; apply in_or_app; right; exact I2|exact (IH l2 a Nr I I2)].
Qed.

(* The copy step at an application: CopyOutputs' comparison of F's result
   (Cr) with H's (Cr'), both read in F's context Cu with the roles gen,
   holds at any meanings tau that rho0 matches on the fixed readings, on
   Cu's other table keys (outside ys) and on the roles. *)
Theorem copy_outputs_read : forall sigma u h Cu ns rm, FunCtx W u Cu ns ->
  (forall t, FunOutputs W h t -> FunOutputs W u t) ->
  (forall x s, lookup_subst x sigma = Some s -> ~ Fresh W x) ->
  CopyOutputs W sigma u h Cu rm ->
  exists (outs : list nat) (gen : nat -> nat) (ys : list nat) (cg : nat -> nat) (Cr : cx) (ks : list item) (Cr' : cx) (ks' : list item),
    (forall t, In t outs <-> FunOutputs W u t) /\
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks /\ ProvideResult W h Cu (fun t => TAbs (gen t)) Cr' ks' /\
    length rm = length ks /\
    (forall i k, nth_error ks i = Some k -> exists j k',
       nth_error rm i = Some j /\ nth_error ks' j = Some k' /\ provision_id k' = provision_id k /\
       (forall j' k'', j < j' -> nth_error ks' j' = Some k'' -> provision_id k'' <> provision_id k)) /\
    forall tau rho0,
    (forall x s, lookup_subst x sigma = Some s -> app_subst rho0 s = app_subst tau (TAbs x)) ->
    (forall x, In x (map fst (c_ty Cu)) -> lookup_subst x sigma = None -> ~ In x ys ->
       app_subst rho0 (TAbs x) = app_subst tau (TAbs x)) ->
    (forall t, In t outs -> app_subst rho0 (TAbs (gen t)) = app_subst tau (TAbs (gen t))) ->
    forall k, In k ks -> exists p p',
      ilook (provision_id k) (c_provisions Cr) = Some p /\ ilook (provision_id k) (c_provisions Cr') = Some p' /\
      provision_subst tau p = provision_subst tau p'.
Proof.
  intros sigma u h Cu ns rm Fc HU Dom
    (outs & gen & ys & cg & Cr & ks & Cr' & ks' & Outs & Nd & Avoid & FrG & FrC & Ys & PRu & PRh & Eq & Lrm & Pos).
  exists outs, gen, ys, cg, Cr, ks, Cr', ks'. split; [exact Outs|]. split; [exact PRu|]. split; [exact PRh|].
  split; [exact Lrm|]. split; [exact Pos|].
  intros tau rho0 Fix Key Role k Ik. destruct (Eq k Ik) as [p [p' [L [L' E]]]]. exists p, p'.
  split; [exact L|]. split; [exact L'|].
  assert (NdC : NoDup (map cg ys)) by exact (NoDup_app_remove_l _ _ Nd).
  assert (Gen : forall v Crv ksv k0 pv, ProvideResult W v Cu (fun t => TAbs (gen t)) Crv ksv ->
    (forall t, FunOutputs W v t -> FunOutputs W u t) -> In k0 ksv ->
    ilook (provision_id k0) (c_provisions Crv) = Some pv ->
    forall x, In x (provision_gens pv) -> CopyGen sigma ys cg tau rho0 x).
  { intros v Crv ksv k0 pv PRv VU Ik0 Lv x Ix.
    assert (FrV : forall t, FunOutputs W v t -> Fresh W (gen t)) by (intros t Ft; exact (FrG t (proj2 (Outs t) (VU t Ft)))).
    split.
    - intros s Ls. split; [|exact (Fix x s Ls)].
      intros z Iz Ic. refine (Avoid z _ (in_or_app _ _ _ (or_intror Ic))).
      apply in_flat_map. exists (x, s). split; [exact (lookup_subst_in_pair _ _ _ Ls)|exact Iz].
    - intros Ls Ny. destruct (result_gens u Cu ns v gen Crv ksv Fc PRv FrV k0 pv Ik0 Lv x Ix) as [Kx|[t [Ft ->]]].
      + split; [|exact (Key x Kx Ls Ny)].
        intros Ic. apply in_map_iff in Ic as [y [Ey Iy]]. apply (functx_keys_declared u Cu ns Fc x Kx). rewrite <- Ey. exact (FrC y Iy).
      + assert (It : In t outs) by exact (proj2 (Outs t) (VU t Ft)). split; [|exact (Role t It)].
        exact (nodup_app_disj _ _ _ Nd (in_map gen _ _ It)). }
  apply (copy_outputs_at sigma ys cg tau rho0 NdC p p' E).
  - exact (Gen u Cr ks k p PRu (fun t F => F) Ik L).
  - assert (Ik' : In (provision_id k) ks').
    { apply In_nth_error in Ik as [i Ei]. destruct (Pos i k Ei) as (j & k' & _ & Ej & Idk & _).
      rewrite <- Idk, (PublicCallerKeys.result_key_id W h Cu _ Cr' ks' k' PRh (nth_error_In _ _ Ej)).
      exact (nth_error_In _ _ Ej). }
    intros x Ix. refine (Gen h Cr' ks' (provision_id k) p' PRh HU Ik' _ x Ix).
    destruct k as [| | |r m i|]; exact L'.
Qed.

End ResultGens.

(* ---- fresh symbols ---------------------------------------------------------- *)

(* A fresh symbol is none of a list of declared ones. *)
Lemma fresh_not_declared : forall W (l : list nat) z, (forall x, In x l -> ~ A.Fresh W x) -> A.Fresh W z -> ~ In z l.
Proof. intros W l z D Fz Iz. exact (D z Iz Fz). Qed.
