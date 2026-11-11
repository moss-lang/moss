(* What a functor's result mentions, for the copy proofs: the generations
   of a result read in a functor's context are its table keys or its
   output roles (result_gens). The substitution argument itself is in
   PublicCopySubst. *)
From Stdlib Require Import List Arith.
From NM Require AuditBase.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicCopySubst.
From NM Require PublicFormation PublicScoping PublicCallerKeys PublicFamilyStep.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.


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
  intros u Cu ns (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & D & FP & IE & Imp & -> & MGM & _ & _ & -> & _ & _) x v I.
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
Hypothesis mentions_typed : PublicFormation.MentionsTyped W.

Lemma functx_keys_declared : forall u Cu ns, FunCtx W u Cu ns -> forall x, In x (map fst (c_ty Cu)) -> ~ Fresh W x.
Proof.
  intros u Cu ns Fc x I Fr. destruct (PublicFormation.functx_cx_formed W mentions_typed u Cu ns Fc) as [_ [_ Ty]].
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

End ResultGens.

(* ---- fresh symbols ---------------------------------------------------------- *)

(* A fresh symbol is none of a list of declared ones. *)
Lemma fresh_not_declared : forall W (l : list nat) z, (forall x, In x l -> ~ A.Fresh W x) -> A.Fresh W z -> ~ In z l.
Proof. intros W l z D Fz Iz. exact (D z Iz Fz). Qed.
