(* The substitution argument behind a copy step (copyfresh, realizepf),
   shared by the copy proofs: composing substitutions, and the transfer of
   an equation at a copy's substitution copy_subst sigma ys cg (Audit's
   CopyInterpretation) to an application's meanings tau. Read the compared
   terms at rho, which sends cg y to tau's meaning of y and is rho0
   elsewhere, and both sides become their reading at tau.

   The transfer needs that no cg y occurs in sigma's image (a fixed
   reading's generations avoid the cg symbols). Audit's CopyInterpretation
   requires this since realizepf (Fresh W only says undeclared, and a copy
   site can read a fixed input as a fresh application generation:
   examples/copy-fresh-collide.moss).

   Also: grounding after a substitution is one substitution
   (ground_app_subst), and the lemmas lift from types to provisions. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit PublicTypeRead PublicCallRealize.
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

(* CopyInterpretation' equation, read at an application's meanings tau. *)
Theorem copy_outputs_at : forall p p',
  A.provision_subst (sigma ++ cg_map) p = A.provision_subst (sigma ++ cg_map) p' ->
  (forall x, In x (A.provision_gens p) -> CopyGen x) -> (forall x, In x (A.provision_gens p') -> CopyGen x) ->
  A.provision_subst tau p = A.provision_subst tau p'.
Proof.
  intros p p' E G G'. rewrite <- (copy_provision_read p G), <- (copy_provision_read p' G'), E. reflexivity.
Qed.

End CopyRead.

(* ---- the generations of a substituted provision ----------------------------- *)

Lemma ty_gens_subst : forall s t y, In y (A.gens (app_subst s t)) ->
  exists z, In z (A.gens t) /\ In y (A.gens (app_subst s (TAbs z))).
Proof.
  intros s t y Iy. destruct (PublicCallRealize.gens_subst t s y Iy) as [z [Iz [[Lz ->]|[u [Lz Iu]]]]];
    exists z; split; try exact Iz; cbn [app_subst]; rewrite Lz; [left; reflexivity|exact Iu].
Qed.

Lemma tys_gens_subst : forall s l y, In y (flat_map A.gens (map (app_subst s) l)) ->
  exists z, In z (flat_map A.gens l) /\ In y (A.gens (app_subst s (TAbs z))).
Proof.
  intros s l y Iy. apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It0]].
  destruct (ty_gens_subst s t0 y Iy) as [z [Iz Iy']]. exists z. split; [apply in_flat_map; exists t0; split; assumption|exact Iy'].
Qed.

Lemma inst_gens_subst : forall s (a : A.inst) y,
  In y (flat_map (fun q => match snd q with Some t => A.gens t | None => [] end)
          (map (fun q => (fst q, option_map (app_subst s) (snd q))) a)) ->
  exists z, In z (flat_map (fun q => match snd q with Some t => A.gens t | None => [] end) a) /\
    In y (A.gens (app_subst s (TAbs z))).
Proof.
  intros s a y Iy. apply in_flat_map in Iy as [q [Iq Iy]]. apply in_map_iff in Iq as [[x [t|]] [<- Iq]]; cbn in Iy; [|contradiction].
  destruct (ty_gens_subst s t y Iy) as [z [Iz Iy']]. exists z. split; [|exact Iy'].
  apply in_flat_map. exists (x, Some t). split; assumption.
Qed.

Lemma provision_gens_subst : forall s p y, In y (A.provision_gens (A.provision_subst s p)) ->
  exists z, In z (A.provision_gens p) /\ In y (A.gens (app_subst s (TAbs z))).
Proof.
  intros s [t a|[ps r] a|[ps r] a|fx] y Iy; unfold A.provision_gens in Iy |- *; cbn [A.provision_subst A.provision_signature A.provision_inst A.sig_subst A.sg_ret A.sg_params] in Iy |- *;
    apply in_app_or in Iy as [Iy|Iy].
  - destruct (tys_gens_subst s [t] y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; left; exact Iz|exact Iy'].
  - destruct (inst_gens_subst s a y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; right; exact Iz|exact Iy'].
  - destruct (tys_gens_subst s (r :: ps) y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; left; exact Iz|exact Iy'].
  - destruct (inst_gens_subst s a y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; right; exact Iz|exact Iy'].
  - destruct (tys_gens_subst s (r :: ps) y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; left; exact Iz|exact Iy'].
  - destruct (inst_gens_subst s a y Iy) as [z [Iz Iy']]. exists z. split; [apply in_or_app; right; exact Iz|exact Iy'].
  - contradiction.
  - rewrite map_map in Iy. cbn in Iy. apply in_flat_map in Iy as [[x t] [Iq Iy]]. apply in_map_iff in Iq as [[x' t'] [E Iq]].
    cbn in E. injection E as -> <-. cbn in Iy.
    destruct (ty_gens_subst s t' y Iy) as [z [Iz Iy']]. exists z. split; [|exact Iy'].
    apply in_or_app. right. apply in_flat_map. exists (x, Some t'). split; [apply in_map_iff; exists (x, t'); split; [reflexivity|exact Iq]|exact Iz].
Qed.
