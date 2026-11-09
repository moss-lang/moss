(* Toward LinkResultsOk (T15/T16): what a completed linked body leaves for
   the caller of `bind F;`. This file proves the runtime half of
   apply_functor's last steps: the result types are read from the body's
   final environment, their destinations are out of the caller's scope (so
   the writes, which shadow, re-ground nothing typed there), and the
   caller's meanings grow only by the destinations. *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice PublicCopyApply PublicPlatformApply.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- result types (read_result_types, fresh_result_types) ---------------- *)

Section ResultTypes.

Lemma read_result_types_ok : forall d3 Dr (outs : list nat) (dest : nat -> nat) (mt : nat -> ty),
  (forall t, In t outs -> existsb (item_eqb (ITy t)) Dr = true /\ output_meaning d3 t = Some (mt t)) ->
  read_result_types d3 Dr (map (fun t => (t, dest t)) outs) =
    Some (map (fun t => (dest t, mt t)) outs, map (fun t => (t, mt t)) outs).
Proof.
  intros d3 Dr outs dest mt Ok. induction outs as [|t rest IH]; [reflexivity|].
  cbn [map read_result_types]. destruct (Ok t (or_introl eq_refl)) as [E M]. rewrite E, M.
  rewrite IH; [reflexivity|]. intros t' I. exact (Ok t' (or_intror I)).
Qed.

Lemma item_eqb_ty_in : forall t (l : list item), In (ITy t) l -> existsb (item_eqb (ITy t)) l = true.
Proof.
  intros t l I. apply existsb_exists. exists (ITy t). split; [exact I|]. cbn. apply Nat.eqb_refl.
Qed.

End ResultTypes.

(* ---- what an application makes is free in the caller -------------------- *)

Section Made.
Variable W : A.world.

Lemma scope_context_gens : forall C x, In x (PublicFormation.scope_gens C) -> In x (A.context_gens C).
Proof.
  intros C x I. unfold PublicFormation.scope_gens in I. unfold A.context_gens.
  apply in_app_or in I as [I|I]; apply in_or_app; [left; exact I|right; apply in_or_app; left; exact I].
Qed.

Lemma spectypes_declared : forall p sp t, A.SpecTypes W p sp t -> ~ A.Fresh W t.
Proof.
  intros p sp t D. induction D as [p pa a t R K|p pa a c d n its sp t R Dd Ast I D IH]; [|exact IH].
  unfold A.Fresh. unfold A.kind_sym in K. intros E. rewrite E in K. discriminate.
Qed.

Lemma funoutputs_declared : forall u t, A.FunOutputs W u t -> ~ A.Fresh W t.
Proof. intros u t (d & ps & a & r & b & sp & _ & _ & _ & St). exact (spectypes_declared _ _ _ St). Qed.

(* A fresh generation that a FreshFor list holds is no key of a formed table. *)
Lemma fresh_not_key : forall F C gs g, PublicFormation.FormedCx W C -> A.FreshFor W F C gs -> In g gs ->
  ~ In g (map fst (A.c_ty C)).
Proof.
  intros F C gs g [_ [_ Ts]] [_ Fr] Ig Ik. rewrite Forall_forall in Fr. destruct (Fr g Ig) as [Fresh Out].
  destruct (Ts g Ik) as [Sc|Ks].
  - apply Out. apply in_or_app. right. apply in_or_app. left. exact (scope_context_gens C g Sc).
  - unfold A.Fresh in Fresh. unfold A.kind_sym in Ks. rewrite Fresh in Ks. discriminate.
Qed.

End Made.

Module LinkResults (Prims : PRIMITIVES) (I0 : INTERP Prims).
(* The instance PublicEntry uses (PA), so the obligations match. *)
Module PA := PublicPlatformApply.PlatformApply Prims I0.
Module CA := PA.CopyA.
Module S := CA.S.

Section Run.
Variable W : A.world.

(* PREMISE (T16; scratch/orch/results2-AUDIT.md section 2). An output that
   `bind F;` installs as itself (not in force here) is not mentioned by the
   context, nor by a method key's receiver. Otherwise the result's write
   re-grounds what mentions it. It can be mentioned only as an unbound
   declared symbol in family method provisions or their keys
   (examples/lambda-declared-output.moss, examples/family-unbound-need.moss:
   KeysScoped admits declared symbols), where re-grounding is harmless by
   parametricity (unproved). For the user to decide: either an Audit clause
   `~ In t (context_gens C ++ key_receiver_gens C)`, which rejects correct
   programs both implementations accept, or the parametricity proof. *)
Definition KeptOutputsUnmentioned : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs -> PublicFormation.FormedCx W C ->
  forall t, A.FunOutputs W u t -> A.nlook t (A.c_ty C) = None -> ~ In t (S.capture_gens C).

(* Each output destination is out of scope at the application: no key of
   the caller's table, no generation its context or frame types mention. *)
Lemma dest_scope : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  PublicFormation.FormedCx W C -> KeptOutputsUnmentioned ->
  forall t, In t outs ->
    ~ In (gensof t) (map fst (A.c_ty C)) /\ ~ In (gensof t) (S.capture_gens C) /\
    (forall r, A.fr_ret F = Some r -> ~ In (gensof t) (A.gens r)) /\
    (forall r, A.fr_this F = Some r -> ~ In (gensof t) (A.gens r)).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs
    AP Fc Ku t It. pose proof AP as (_ & _ & _ & _ & _ & _ & _ & FreshG & _ & Outs & _ & OutsI & _).
  pose proof (proj1 (OutsI t) It) as Ft.
  destruct (Outs t Ft) as [[None' [E Fr]]|[_ [Fresh Ig]]].
  - rewrite E. pose proof (funoutputs_declared W u t Ft) as Nf.
    split; [intros Ik; destruct (nlook_some _ t _ Ik) as [v L]; congruence|].
    split; [exact (Ku F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc t Ft None')|].
    split; [intros r Er I; apply Fr; apply in_or_app; left; rewrite Er; exact I|].
    intros r Er I; apply Fr; apply in_or_app; right; rewrite Er; exact I.
  - destruct FreshG as [Nd Fr]. rewrite Forall_forall in Fr. destruct (Fr _ Ig) as [_ Out].
    split; [exact (fresh_not_key W F C newgens _ Fc (conj Nd (proj2 (Forall_forall _ _) Fr)) Ig)|].
    split; [intros I; apply in_app_or in I as [I|I]; [apply Out; apply in_or_app; right; apply in_or_app; left; exact I|]|].
    { unfold A.key_receiver_gens in I. apply in_flat_map in I as [[k p] [Iq I]]. destruct k as [| | |r m a|]; try contradiction.
      destruct (proj1 (proj2 Fc) r m a p Iq _ I) as [Sc|K].
      - apply Out. apply in_or_app; right; apply in_or_app; left. exact (scope_context_gens C _ Sc).
      - unfold A.Fresh in Fresh. unfold A.kind_sym in K. rewrite Fresh in K. discriminate. }
    split; [intros r Er I; apply Out; apply in_or_app; right; apply in_or_app; right; apply in_or_app; left; rewrite Er; exact I|].
    intros r Er I; apply Out; apply in_or_app; right; apply in_or_app; right; apply in_or_app; right; rewrite Er; exact I.
Qed.

Lemma app_outputs_nodup : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs -> NoDup (map gensof outs).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs
    (_ & _ & _ & _ & _ & _ & _ & _ & Uniq & _ & Nd & OutsI & _).
  apply S.nodup_map_on; [exact Nd|]. intros a b Ia Ib E.
  exact (Uniq a b (proj1 (OutsI a) Ia) (proj1 (OutsI b) Ib) E).
Qed.

Definition read_meaning (d3 : environment) (t : nat) : ty :=
  match output_meaning d3 t with Some m => m | None => TUnit end.

Definition result_of (d3 : environment) (outs : list nat) (gensof : nat -> nat) (dr : environment) : environment :=
  EnvironmentWithAliases (map (fun t => (gensof t, read_meaning d3 t)) outs)
    (map (fun t => (t, read_meaning d3 t)) outs) (provision_values dr).

(* apply_functor's last step: the outputs are read off the body's final
   environment, their destinations are free in the caller, and the
   caller's meanings grow by them alone. *)
Lemma result_environment_ok : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall d d3 dr (outs' : list nat) (rest : list item),
  (forall t, In t outs' <-> A.FunOutputs W u t) ->
  (forall t, In t outs -> output_meaning d3 t <> None) ->
  result_environment d (map (fun t => (t, gensof t)) outs) d3 (map ITy outs' ++ rest)%list dr =
    Some (result_of d3 outs gensof dr).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d d3 dr outs' rest OutsI' Read.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsI & _).
  unfold result_environment.
  rewrite (read_result_types_ok d3 _ outs gensof (read_meaning d3)).
  - reflexivity.
  - intros t It. split.
    + apply item_eqb_ty_in. apply in_or_app. left. apply in_map. apply OutsI'. apply OutsI. exact It.
    + unfold read_meaning. destruct (output_meaning d3 t) as [m|] eqn:E; [reflexivity|].
      exfalso. exact (Read t It E).
Qed.

(* The same growth, over every output destination (the form the lead's
   reconciled statement uses: made = map gensof outs). *)
Lemma result_grows_by_destinations : forall d d3 dr (outs : list nat) (gensof : nat -> nat),
  S.MeaningsGrow (map gensof outs) d (append_environment (result_of d3 outs gensof dr) d).
Proof.
  intros d d3 dr outs gensof. exists (map (fun t => (gensof t, read_meaning d3 t)) outs). split; [reflexivity|].
  intros x Ix. rewrite map_map in Ix. exact Ix.
Qed.

End Run.

(* ---- the caller's state after the application ---------------------------- *)

Section After.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.

Lemma ground_grow_writes : forall made d d' t, S.MeaningsGrow made d d' ->
  (forall g, In g made -> ~ In g (A.gens t)) -> ground d' t = ground d t.
Proof. intros made d d' t Gr Av. exact (S.ground_grow made d d' t Gr Av). Qed.

(* Merge absorption survives meanings for generations the merges do not
   mention. *)
Lemma absorbs_grow : forall C C' made d d', S.Absorbs C d -> A.c_sub C' = A.c_sub C ->
  S.MeaningsGrow made d d' -> (forall g, In g made -> ~ In g (A.context_gens C)) -> S.Absorbs C' d'.
Proof.
  intros C C' made d d' Abs Es Gr Av t. unfold A.canon. rewrite Es. unfold ground.
  rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x _. cbn [app_subst lookup_subst].
  destruct (lookup_subst x (A.c_sub C)) as [u|] eqn:L; [|reflexivity].
  pose proof (lookup_subst_in_pair _ _ _ L) as Iq. destruct (S.sub_context_gens C x u Iq) as [Ix Iu].
  change (ground d' u = ground d' (TAbs x)).
  rewrite (ground_grow_writes made d d' u Gr (fun g Ig Ig' => Av g Ig (Iu g Ig'))).
  rewrite (ground_grow_writes made d d' (TAbs x) Gr (fun g Ig Ig' => Av g Ig ltac:(destruct Ig' as [<-|[]]; exact Ix))).
  pose proof (Abs (TAbs x)) as E. unfold A.canon in E. cbn [app_subst] in E. rewrite L in E. exact E.
Qed.

(* The caller's state after `bind F;`: everything but the result's own
   provisions and aliases carries over from the caller's, since the
   meanings grow only by generations nothing in scope mentions. *)
Lemma pre_after_result : forall rr F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall d types H g st d' H3 st3,
  S.Pre W Prog argv read state_ok rr F C d types H g st -> KeptOutputsUnmentioned W ->
  S.MeaningsGrow (map gensof outs) d d' -> heap_extends H H3 -> state_ok H3 st3 ->
  AliasOk C' d' -> S.EnvOk W Prog argv read state_ok rr C' d' H3 -> S.KeysMatch C' ->
  S.Pre W Prog argv read state_ok rr (A.with_fresh F newgens) C' d' types H3 g st3.
Proof.
  intros rr F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d types H g st d' H3 st3
    [Frame [[Lo [Re _]] [L [_ [[Abs [_ [Idem _]]] [Fc Ff]]]]]] Ms Gr Ext St3 Al Env KM.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & _ & _ & Outs & _ & _ & PR).
  destruct (PublicFormation.provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo _]].
  assert (Av : forall g0, In g0 (map gensof outs) -> ~ In g0 (A.context_gens C) /\ True /\
    (forall t, A.fr_ret F = Some t -> ~ In g0 (A.gens t)) /\ (forall t, A.fr_this F = Some t -> ~ In g0 (A.gens t))).
  { intros g0 Ig. apply in_map_iff in Ig as [t [<- It]].
    destruct (dest_scope W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP Fc Ms t It) as [_ [Nc [Nr Nt]]].
    exact (conj (fun I => Nc (S.cg_l C _ I)) (conj I (conj Nr Nt))). }
  split; [exact Frame|]. split; [split; [|split; [|exact Al]]|].
  - intros n t mu Found Nm. rewrite Elo in Found. cbn in Nm |- *.
    rewrite (ground_grow_writes (map gensof outs) d d' t Gr (fun g0 Ig Ig' => proj1 (Av g0 Ig) (S.local_context_gens C n t mu Found g0 Ig'))).
    exact (Lo n t mu Found Nm).
  - intros t Th. cbn in Th.
    rewrite (ground_grow_writes (map gensof outs) d d' t Gr (fun g0 Ig => proj2 (proj2 (proj2 (Av g0 Ig))) t Th)).
    exact (Re t Th).
  - split; [exact (locals_extends W P H H3 g types Ext L)|]. split; [exact St3|]. split.
    + split; [exact (absorbs_grow C C' (map gensof outs) d d' Abs Esub Gr (fun g0 Ig => proj1 (Av g0 Ig)))|].
      split; [exact Env|]. split; [intros t; unfold A.canon; rewrite Esub; exact (Idem t)|exact KM].
    + split; [|split; [exact (proj1 Ff)|exact (PublicScoping.apply_abstract_cxscoped W F _ C u gensof C' ks PR eq_refl (proj2 Ff))]].
      apply (PublicFormation.apply_abstract_formed W C u gensof C' ks Fc); [|exact PR].
      intros t Ft. destruct (Outs t Ft) as [[_ [E _]]|[_ [Fr _]]]; [left; exact E|right; exact Fr].
Qed.

(* ---- Pre does not bound the caller's meanings ---------------------------- *)

(* The realization only reads the meanings at types the context mentions
   (or its method keys' receivers mention). *)
Lemma envok_scoped : forall n C d d' H, S.KeysMatch C ->
  S.EnvOk W Prog argv read state_ok n C d H -> provision_values d' = provision_values d ->
  (forall t, incl (A.gens t) (S.capture_gens C) -> ground d' t = ground d t) ->
  S.EnvOk W Prog argv read state_ok n C d' H.
Proof.
  intros n C d d' H KM [Vals [Fns [Mths Funs]]] Ep G0.
  assert (G : forall t, incl (A.gens t) (A.context_gens C) -> ground d' t = ground d t)
    by (intros t I; apply G0; intros x Ix; exact (S.cg_l C x (I x Ix))).
  assert (Lk : forall k, lookup_provision k d' = lookup_provision k d) by (intros k; unfold lookup_provision; rewrite Ep; reflexivity).
  assert (Gm : forall ps, (forall t, In t ps -> incl (A.gens t) (A.context_gens C)) -> map (ground d) ps = map (ground d') ps).
  { intros ps Inc. apply map_ext_in. intros t It. symmetry. exact (G t (Inc t It)). }
  split; [|split; [|split]].
  - intros v t Found. destruct (Vals v t Found) as [w [Look T]]. exists w. rewrite Lk, G; [split; assumption|].
    destruct (vals_ilook _ v t KM Found) as [a Il]. apply S.ilook_in in Il.
    intros x Ix. apply (S.prov_context_gens C _ _ Il). exact (proj1 (S.sig_provision_gens (A.ProvidedVal t a) (A.FSig [] t) eq_refl) x Ix).
  - intros f g0 Found. destruct (Fns f g0 Found) as [pv [Look Ok]]. exists pv. rewrite Lk. split; [exact Look|].
    destruct (fns_ilook _ f g0 KM Found) as [a Il]. apply S.ilook_in in Il.
    destruct (S.sig_provision_gens (A.ProvidedFn g0 a) g0 eq_refl) as [Gr Gp].
    refine (S.callable_transfer W Prog argv read state_ok n n H d d' _ _ _ _ pv (le_n n) _ _ Ok).
    + apply Gm. intros t It x Ix. exact (S.prov_context_gens C _ _ Il x (Gp t It x Ix)).
    + symmetry. apply G. intros x Ix. exact (S.prov_context_gens C _ _ Il x (Gr x Ix)).
  - intros r m g0 Found. destruct (Mths r m g0 Found) as [pv [Look Ok]]. exists pv. rewrite Lk. split; [exact Look|].
    destruct (mths_ilook _ r m g0 KM Found) as [a Il]. apply S.ilook_in in Il.
    destruct (S.sig_provision_gens (A.ProvidedMethod g0 a) g0 eq_refl) as [Gr Gp].
    refine (S.callable_transfer W Prog argv read state_ok n n H d d' _ _ _ _ pv (le_n n) _ _ Ok).
    + rewrite (map_ext_in (ground d) (ground d') (r :: A.sg_params g0)); [reflexivity|].
      intros t [<-|It]; symmetry; [apply G0; exact (S.cg_key C r m [] _ Il)|].
      apply G. intros x Ix. exact (S.prov_context_gens C _ _ Il x (Gp t It x Ix)).
    + symmetry. apply G. intros x Ix. exact (S.prov_context_gens C _ _ Il x (Gr x Ix)).
  - intros u fx Found. destruct (Funs u fx Found) as [pv [gfx [Look [Ok [Cov Vm]]]]].
    exists pv, gfx. rewrite Lk. split; [exact Look|]. split; [exact Ok|]. split; [exact Cov|].
    intros x tx Ix. rewrite G; [exact (Vm x tx Ix)|].
    pose proof (funs_ilook _ u fx KM Found) as Il. apply S.ilook_in in Il.
    intros y Iy. apply (S.prov_context_gens C _ _ Il). unfold A.provision_gens. cbn [A.provision_signature].
    apply in_or_app. right. apply in_flat_map. exists (x, Some tx). split; [|exact Iy].
    cbn [A.provision_inst]. apply in_map_iff. exists (x, tx). split; [reflexivity|exact Ix].
Qed.

Lemma nlook_ty_context_gens : forall C x tx, A.nlook x (A.c_ty C) = Some tx -> incl (A.gens tx) (A.context_gens C).
Proof.
  intros C x tx L y Iy. unfold A.context_gens. apply in_or_app. right. apply in_or_app. left.
  apply in_flat_map. exists (x, tx). split; [exact (nlook_in _ _ _ _ L)|apply in_or_app; left; exact Iy].
Qed.

(* A caller state whose meanings differ only off the context's types (and
   off its keys) satisfies Pre as well. So Pre cannot rule out a meaning
   for a generation an application is about to make: result_environment
   shadows such a meaning rather than checking it is absent. *)
Lemma pre_meanings_free : forall rr F C d types H g st d',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  provision_values d' = provision_values d -> type_aliases d' = type_aliases d ->
  (forall x, In x (map fst (A.c_ty C)) -> lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d)) ->
  (forall t, incl (A.gens t) (S.capture_gens C ++ match A.fr_this F with Some r => A.gens r | None => [] end) ->
     ground d' t = ground d t) ->
  S.Pre W Prog argv read state_ok rr F C d' types H g st.
Proof.
  intros rr F C d types H g st d' [Frame [[Lo [Re Al]] [L [St [[Abs [Env [Idem KM]]] [Fc Ff]]]]]] Ep Ea Ek G.
  assert (Gk : forall t, incl (A.gens t) (S.capture_gens C) -> ground d' t = ground d t)
    by (intros t I; apply G; intros x Ix; apply in_or_app; left; exact (I x Ix)).
  assert (Gc : forall t, incl (A.gens t) (A.context_gens C) -> ground d' t = ground d t)
    by (intros t I; apply Gk; intros x Ix; exact (S.cg_l C x (I x Ix))).
  split; [exact Frame|]. split; [split; [|split]|].
  - intros n t mu Found Nm. rewrite Gc; [exact (Lo n t mu Found Nm)|exact (S.local_context_gens C n t mu Found)].
  - intros t Th. rewrite G; [exact (Re t Th)|]. intros x Ix. apply in_or_app. right. rewrite Th. exact Ix.
  - intros x tx Lx. unfold cmeaning, output_meaning. rewrite Ea.
    assert (Ik : In x (map fst (A.c_ty C))) by (change x with (fst (x, tx)); apply in_map; exact (nlook_in _ _ _ _ Lx)).
    rewrite (Ek x Ik), Gc; [exact (Al x tx Lx)|exact (nlook_ty_context_gens C x tx Lx)].
  - split; [exact L|]. split; [exact St|]. split; [|exact (conj Fc Ff)].
    split; [|split; [|exact (conj Idem KM)]].
    + intros t. unfold A.canon. unfold ground.
      rewrite <- (app_subst_nil t) at 2. apply subst2_agree. intros x _. cbn [app_subst lookup_subst].
      destruct (lookup_subst x (A.c_sub C)) as [u|] eqn:Ls.
      * pose proof (lookup_subst_in_pair _ _ _ Ls) as Iq. destruct (S.sub_context_gens C x u Iq) as [Ix Iu].
        change (ground d' u = ground d' (TAbs x)). rewrite (Gc u Iu), (Gc (TAbs x) ltac:(intros y [<-|[]]; exact Ix)).
        pose proof (Abs (TAbs x)) as E. unfold A.canon in E. cbn [app_subst] in E. rewrite Ls in E. exact E.
      * reflexivity.
    + exact (envok_scoped rr C d d' H KM Env Ep Gk).
Qed.

(* In particular, a meaning appended for a generation outside the context's
   keys, types and receiver. *)
Lemma pre_junk_meaning : forall rr F C d types H g st x m,
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  ~ In x (map fst (A.c_ty C)) ->
  ~ In x (S.capture_gens C ++ match A.fr_this F with Some r => A.gens r | None => [] end) ->
  S.Pre W Prog argv read state_ok rr F C
    (EnvironmentWithAliases (type_meanings d ++ [(x, m)]) (type_aliases d) (provision_values d)) types H g st.
Proof.
  intros rr F C d types H g st x m Pre0 Nk Ns.
  assert (Lk : forall y, y <> x -> lookup_subst y (type_meanings d ++ [(x, m)]) = lookup_subst y (type_meanings d)).
  { intros y Ne. rewrite !lookup_subst_nlook', nlook_app'. destruct (A.nlook y (type_meanings d)); [reflexivity|].
    cbn. destruct (Nat.eqb_spec y x); [contradiction|reflexivity]. }
  apply (pre_meanings_free rr F C d types H g st _ Pre0); [reflexivity|reflexivity| |].
  - intros y Iy. apply Lk. intros ->. exact (Nk Iy).
  - intros t Inc. unfold ground. cbn [type_meanings]. apply subst_agree_on. intros y Iy. cbn [app_subst].
    rewrite Lk; [reflexivity|]. intros ->. exact (Ns (Inc x Iy)).
Qed.

End After.

(* ---- assembly ----------------------------------------------------------- *)

Section Assemble.
Variable W : A.world.
Variables (Prog : program) (argv : list (list int)) (read : list int -> option (list int)).
Variable state_ok : heap_types -> Prims.mstate -> Prop.
Let P := program_of Prog argv read.

(* A copy chain keeps the type outputs (each copy's SameResult, through same_result_outputs). *)
Lemma chain_outputs : forall u gfx sms h0 gf0 sms0, CA.CopyChain W u gfx sms h0 gf0 sms0 ->
  forall t, A.FunOutputs W u t <-> A.FunOutputs W h0 t.
Proof.
  intros u gfx sms h0 gf0 sms0 Ch. induction Ch as [|u h Cu nsu Ch nsh gfx gfh sms h0 gf0 sms0 _ IH _ _ _ _ _ _ _ _ SO];
    intros t; [reflexivity|]. rewrite (S.same_result_outputs W u h SO t). exact (IH t).
Qed.

(* REMAINING (T15/T16), at a copy chain above a linked root (ChainResultsOk's
   premises): the result selection from the body keys succeeds, the outputs
   have meanings in the body's final environment, and the caller's context
   after the application is realized by the result's provisions and
   aliases. The formal/caller correspondence through θ and the output
   renaming (PublicOutputSubst) is what proves it. *)
Definition ChainCoreOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0
    d1 d2 d3 g3 st3 H3 j',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure code sms dc) ->
  S.ClosureOk W Prog argv read state_ok rr H u gfx (FunctorClosure code sms dc) ->
  S.FixedMeans W u d fx gfx ->
  CA.CopyChain W u gfx sms h0 gf0 [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] ->
  S.ClosureOk W Prog argv read state_ok rr H h0 gf0 (FunctorClosure code [map (fun k => (A.rkey_of Cg k, A.rkey_of Ch0 k)) (S.linked_needs nsh0 nsg)] dc) ->
  S.LinkData W Prog h0 gg code dg n ps a r bodies Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  adapt_provisions d1 sms = Some d2 ->
  S.I.eval_bindings j' P (application_environment P d code th (append_environment d2 dc)) [] st binds0 = inl (d3, g3, st3) ->
  heap_extends H H3 -> S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr,
    select_provisions d3 (result_smap (result_substitution th (role_outputs (map (fun t => (t, gen t)) outs') (map (fun t => (t, gensof t)) outs))) (map ITy outs' ++ map (A.rkey_of Cr) ks')%list (S.link_sources Cb outs' gen Cr ks')) = Some dr /\
    (forall t, In t outs -> output_meaning d3 t <> None) /\
    AliasOk C' (append_environment (result_of d3 outs gensof dr) d) /\
    S.EnvOk W Prog argv read state_ok rr C' (append_environment (result_of d3 outs gensof dr) d) H3 /\
    S.KeysMatch C'.

(* ChainResultsOk, from the correspondence and DeclaredScoped. *)
Theorem chain_results_scoped : ChainCoreOk ->
  KeptOutputsUnmentioned W -> CA.ChainResultsOk W Prog argv read state_ok.
Proof.
  intros Core DS F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Sel Sel2 Run Ext Pre3.
  destruct (Core F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st code sms dc gfx h0 gf0 gg dg n ps a r bodies
    Ch0 nsh0 Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm Chain Ok0 LD Sel Sel2 Run Ext Pre3)
    as [dr [Sdr [Read [Al [Env KM]]]]].
  pose proof Pre0 as [_ [_ [_ [_ [_ [Fc _]]]]]].
  pose proof DS as Sc.
  assert (OutsI' : forall t, In t outs' <-> A.FunOutputs W u t).
  { pose proof LD as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsR & _).
    intros t. rewrite (OutsR t). symmetry. exact (chain_outputs _ _ _ _ _ _ Chain t). }
  pose proof (result_environment_ok W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d d3 dr outs'
    (map (A.rkey_of Cr) ks') OutsI' Read) as Re.
  pose proof (result_grows_by_destinations d d3 dr outs gensof) as Gr.
  exists dr, (result_of d3 outs gensof dr). split; [exact Sdr|]. split; [exact Re|]. split; [|exact Gr].
  pose proof Pre3 as [_ [_ [_ [St3 _]]]].
  exact (pre_after_result W Prog argv read state_ok rr F C pa u fx Cu ns sm th gensof C' ks newgens outs AP
    d types H g st _ H3 st3 Pre0 Sc Gr Ext St3 Al Env KM).
Qed.

End Assemble.

End LinkResults.
