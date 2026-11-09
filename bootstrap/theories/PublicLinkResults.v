(* Toward LinkResultsOk (T15/T16): what a completed linked body leaves for
   the caller of `bind F;`. This file proves the runtime half of
   apply_functor's last steps: the result types are read from the body's
   final environment, their destinations are free in the caller, and the
   caller's meanings grow only by the generations the application makes
   (Audit's MadeFor). *)
From Stdlib Require Import List String Arith Bool Lia PrimInt63.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping
  PublicBodySafety PublicTypeFacts PublicCallRealize PublicBodySlice.
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

Lemma fresh_result_types_ok : forall (writes occupied : subst),
  NoDup (map fst writes) -> (forall x, In x (map fst writes) -> lookup_subst x occupied = None) ->
  fresh_result_types writes occupied = true.
Proof.
  intros writes. induction writes as [|[x m] rest IH]; intros occ Nd Free; [reflexivity|].
  cbn [fresh_result_types]. rewrite (Free x (or_introl eq_refl)).
  inversion Nd as [|? ? Nx Nd']; subst. apply IH; [exact Nd'|].
  intros y Iy. cbn. destruct (Nat.eqb_spec y x) as [->|Ne]; [contradiction|].
  exact (Free y (or_intror Iy)).
Qed.

Lemma item_eqb_ty_in : forall t (l : list item), In (ITy t) l -> existsb (item_eqb (ITy t)) l = true.
Proof.
  intros t l I. apply existsb_exists. exists (ITy t). split; [exact I|]. cbn. apply Nat.eqb_refl.
Qed.

(* Conversely, an occupied destination makes the application Stuck: without
   MeaningsScoped (or another bound on the caller's meanings) the step
   cannot complete, whatever the body does. *)
Lemma fresh_result_types_occupied : forall (writes occupied : subst) x m,
  In x (map fst writes) -> lookup_subst x occupied = Some m -> fresh_result_types writes occupied = false.
Proof.
  intros writes. induction writes as [|[y v] rest IH]; intros occ x m Ix L; [contradiction|].
  cbn [fresh_result_types]. destruct (lookup_subst y occ) as [w|] eqn:Ly; [reflexivity|].
  destruct Ix as [E|Ix]; [cbn in E; subst y; congruence|].
  apply (IH _ x (if Nat.eqb x y then v else m) Ix). cbn.
  destruct (Nat.eqb_spec x y) as [->|_]; [reflexivity|exact L].
Qed.

Lemma result_environment_occupied : forall d outputs d3 Dr dr writes aliases x m,
  read_result_types d3 Dr outputs = Some (writes, aliases) ->
  In x (map fst writes) -> lookup_subst x (type_meanings d) = Some m ->
  result_environment d outputs d3 Dr dr = None.
Proof.
  intros d outputs d3 Dr dr writes aliases x m R Ix L. unfold result_environment. rewrite R.
  rewrite (fresh_result_types_occupied writes _ x m Ix L). reflexivity.
Qed.

End ResultTypes.

(* ---- what an application makes is free in the caller -------------------- *)

Section Made.
Variable W : A.world.

(* The caller's meanings give a meaning only to a symbol in its type table
   or to a generation in scope (made earlier in the frame, or mentioned by
   the context). A NEW INVARIANT: Pre does not carry it (see
   scratch/orch/results-STATUS.md); without it `result_environment` can
   find a destination occupied. *)
Definition MeaningsScoped (F : A.frame) (C : A.cx) (d : environment) : Prop :=
  forall x, lookup_subst x (type_meanings d) <> None ->
    In x (map fst (A.c_ty C)) \/ In x (A.fr_fresh F ++ A.context_gens C).

Lemma scope_context_gens : forall C x, In x (PublicFormation.scope_gens C) -> In x (A.context_gens C).
Proof.
  intros C x I. unfold PublicFormation.scope_gens in I. unfold A.context_gens.
  apply in_app_or in I as [I|I]; apply in_or_app; [left; exact I|right; apply in_or_app; left; exact I].
Qed.

Lemma made_not_key : forall F C outs gs g, PublicFormation.FormedCx W C -> A.MadeFor W F C outs gs -> In g gs ->
  ~ In g (map fst (A.c_ty C)).
Proof.
  intros F C outs gs g [_ [_ Ts]] [_ Fr] Ig Ik. rewrite Forall_forall in Fr.
  destruct (Fr g Ig) as [Kind Out].
  destruct Kind as [Fresh|[_ None']].
  - destruct (Ts g Ik) as [Sc|Ks].
    + apply Out. apply in_or_app. right. apply in_or_app. left. exact (scope_context_gens C g Sc).
    + unfold A.Fresh in Fresh. unfold A.kind_sym in Ks. rewrite Fresh in Ks. discriminate.
  - destruct (nlook_some _ g _ Ik) as [v L]. congruence.
Qed.

Lemma made_free : forall F C outs gs d, PublicFormation.FormedCx W C -> A.MadeFor W F C outs gs ->
  MeaningsScoped F C d -> forall g, In g gs -> lookup_subst g (type_meanings d) = None.
Proof.
  intros F C outs gs d Fc Mf Sc g Ig. destruct (lookup_subst g (type_meanings d)) as [m|] eqn:L; [|reflexivity].
  exfalso. destruct (Sc g ltac:(rewrite L; discriminate)) as [Ik|If].
  - exact (made_not_key F C outs gs g Fc Mf Ig Ik).
  - destruct Mf as [_ Fr]. rewrite Forall_forall in Fr. destruct (Fr g Ig) as [_ Out]. apply Out.
    rewrite app_assoc. apply in_or_app. left. exact If.
Qed.

End Made.

Module LinkResults (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module S := PublicBodySlice.BodySlice Prims I0.

Section Run.
Variable W : A.world.

Lemma app_outputs_made : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall t, In t outs -> In (gensof t) newgens.
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs
    (_ & _ & _ & _ & _ & _ & _ & _ & _ & Outs & _ & OutsI & _) t It.
  destruct (Outs t (proj1 (OutsI t) It)) as [[_ [E I]]|[_ [_ I]]]; [rewrite E; exact I|exact I].
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
  PublicFormation.FormedCx W C -> MeaningsScoped F C d ->
  (forall t, In t outs' <-> A.FunOutputs W u t) ->
  (forall t, In t outs -> output_meaning d3 t <> None) ->
  result_environment d (map (fun t => (t, gensof t)) outs) d3 (map ITy outs' ++ rest)%list dr =
    Some (result_of d3 outs gensof dr) /\
  S.MeaningsGrow newgens d (append_environment (result_of d3 outs gensof dr) d).
Proof.
  intros F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d d3 dr outs' rest Fc Sc OutsI' Read.
  pose proof (app_outputs_made _ _ _ _ _ _ _ _ _ _ _ _ _ _ AP) as Made.
  pose proof (app_outputs_nodup _ _ _ _ _ _ _ _ _ _ _ _ _ _ AP) as Nd.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & Mf & _ & _ & _ & OutsI & _).
  split.
  - unfold result_environment.
    rewrite (read_result_types_ok d3 _ outs gensof (read_meaning d3)).
    + rewrite fresh_result_types_ok; [reflexivity| |].
      * rewrite map_map. cbn. exact Nd.
      * intros x Ix. rewrite map_map in Ix. cbn in Ix. apply in_map_iff in Ix as [t [<- It]].
        exact (made_free W F C outs newgens d Fc Mf Sc (gensof t) (Made t It)).
    + intros t It. split.
      * apply item_eqb_ty_in. apply in_or_app. left. apply in_map. apply OutsI'. apply OutsI. exact It.
      * unfold read_meaning. destruct (output_meaning d3 t) as [m|] eqn:E; [reflexivity|].
        exfalso. exact (Read t It E).
  - exists (map (fun t => (gensof t, read_meaning d3 t)) outs). split; [reflexivity|].
    intros x Ix. rewrite map_map in Ix. cbn in Ix. apply in_map_iff in Ix as [t [<- It]]. exact (Made t It).
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

Lemma made_avoids : forall F C outs gs, A.MadeFor W F C outs gs ->
  forall g, In g gs -> ~ In g (A.context_gens C) /\ ~ In g (A.fr_fresh F) /\
    (forall t, A.fr_ret F = Some t -> ~ In g (A.gens t)) /\ (forall t, A.fr_this F = Some t -> ~ In g (A.gens t)).
Proof.
  intros F C outs gs [_ Fr] g Ig. rewrite Forall_forall in Fr. destruct (Fr g Ig) as [_ Out].
  split; [intro I; apply Out; apply in_or_app; right; apply in_or_app; left; exact I|].
  split; [intro I; apply Out; apply in_or_app; left; exact I|].
  split; intros t E I; apply Out; apply in_or_app; right; apply in_or_app; right; apply in_or_app;
    [left; rewrite E; exact I|right; rewrite E; exact I].
Qed.

(* The caller's state after `bind F;`: everything but the result's own
   provisions and aliases carries over from the caller's, since the
   meanings grow only by generations nothing in scope mentions. *)
Lemma pre_after_result : forall rr F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall d types H g st d' H3 st3,
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  S.MeaningsGrow newgens d d' -> heap_extends H H3 -> state_ok H3 st3 ->
  AliasOk C' d' -> S.EnvOk W Prog argv read state_ok rr C' d' H3 -> S.KeysMatch C' ->
  S.Pre W Prog argv read state_ok rr (A.with_fresh F newgens) C' d' types H3 g st3.
Proof.
  intros rr F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d types H g st d' H3 st3
    [Frame [[Lo [Re _]] [L [_ [[Abs [_ [Idem _]]] [Fc Ff]]]]]] Gr Ext St3 Al Env KM.
  pose proof AP as (_ & _ & _ & _ & _ & _ & _ & Mf & _ & Outs & _ & _ & PR).
  destruct (PublicFormation.provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo _]].
  pose proof (made_avoids F C outs newgens Mf) as Av.
  split; [exact Frame|]. split; [split; [|split; [|exact Al]]|].
  - intros n t mu Found Nm. rewrite Elo in Found. cbn in Nm |- *.
    rewrite (ground_grow_writes newgens d d' t Gr (fun g0 Ig Ig' => proj1 (Av g0 Ig) (S.local_context_gens C n t mu Found g0 Ig'))).
    exact (Lo n t mu Found Nm).
  - intros t Th. cbn in Th.
    rewrite (ground_grow_writes newgens d d' t Gr (fun g0 Ig => proj2 (proj2 (proj2 (Av g0 Ig))) t Th)).
    exact (Re t Th).
  - split; [exact (locals_extends W P H H3 g types Ext L)|]. split; [exact St3|]. split.
    + split; [exact (absorbs_grow C C' newgens d d' Abs Esub Gr (fun g0 Ig => proj1 (Av g0 Ig)))|].
      split; [exact Env|]. split; [intros t; unfold A.canon; rewrite Esub; exact (Idem t)|exact KM].
    + split; [|exact Ff].
      apply (PublicFormation.apply_abstract_formed W C u gensof C' ks Fc); [|exact PR].
      intros t Ft. destruct (Outs t Ft) as [[_ [E _]]|[_ [Fr _]]]; [left; exact E|right; exact Fr].
Qed.

(* ---- Pre does not bound the caller's meanings ---------------------------- *)

(* The realization only reads the meanings at types the context mentions. *)
Lemma envok_scoped : forall n C d d' H, S.KeysMatch C -> PublicFormation.KeysScoped C ->
  S.EnvOk W Prog argv read state_ok n C d H -> provision_values d' = provision_values d ->
  (forall t, incl (A.gens t) (A.context_gens C) -> ground d' t = ground d t) ->
  S.EnvOk W Prog argv read state_ok n C d' H.
Proof.
  intros n C d d' H KM Ks [Vals [Fns [Mths Funs]]] Ep G.
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
    + apply Gm. intros t [<-|It] x Ix.
      * exact (scope_context_gens C x (Ks r m [] _ Il x Ix)).
      * exact (S.prov_context_gens C _ _ Il x (Gp t It x Ix)).
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
   for a generation an application is about to make, and LinkResultsOk
   needs a bound like MeaningsScoped. *)
Lemma pre_meanings_free : forall rr F C d types H g st d',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  provision_values d' = provision_values d -> type_aliases d' = type_aliases d ->
  (forall x, In x (map fst (A.c_ty C)) -> lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d)) ->
  (forall t, incl (A.gens t) (A.context_gens C ++ match A.fr_this F with Some r => A.gens r | None => [] end) ->
     ground d' t = ground d t) ->
  S.Pre W Prog argv read state_ok rr F C d' types H g st.
Proof.
  intros rr F C d types H g st d' [Frame [[Lo [Re Al]] [L [St [[Abs [Env [Idem KM]]] [Fc Ff]]]]]] Ep Ea Ek G.
  assert (Gc : forall t, incl (A.gens t) (A.context_gens C) -> ground d' t = ground d t)
    by (intros t I; apply G; intros x Ix; apply in_or_app; left; exact (I x Ix)).
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
    + exact (envok_scoped rr C d d' H KM (proj1 (proj2 Fc)) Env Ep Gc).
Qed.

(* In particular, a meaning appended for a generation outside the context's
   keys, types and receiver. *)
Lemma pre_junk_meaning : forall rr F C d types H g st x m,
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  ~ In x (map fst (A.c_ty C)) ->
  ~ In x (A.context_gens C ++ match A.fr_this F with Some r => A.gens r | None => [] end) ->
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

(* REMAINING (T15/T16): the result's own provisions, read at the caller.
   With the premises of LinkResultsOk: the refiled result keys are found
   in the body's final environment (so the result selection succeeds), its
   outputs have meanings there, and the caller's context after the
   application is realized by the result's provisions and aliases. This
   is the formal/caller correspondence through θ and the output renaming
   (see scratch/orch/results-STATUS.md). *)
Definition ResultCoreOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st h dc gfx gg dg n ps a r bodies nsg Cg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure h [map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)] dc) ->
  S.ClosureOk W Prog argv read state_ok rr H u gfx (FunctorClosure h [map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)] dc) ->
  S.FixedMeans W u d fx gfx ->
  S.LinkData W Prog u gg h dg n ps a r bodies Cu ns Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  select_provisions d1 (map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)) = Some d2 ->
  S.I.eval_bindings j' P (application_environment P d h th (append_environment d2 dc)) [] st (flat_map (A.refile (map (fun k0 => (A.at_site Cb (A.output_renaming outs' gen k0), A.output_renaming outs' gen (A.rkey_of Cr k0))) ks')) binds0) = inl (d3, g3, st3) ->
  heap_extends H H3 -> S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  exists dr,
    select_provisions d3 (inst_smap (result_substitution th (map (fun t => (t, gensof t)) outs)) (map ITy outs' ++ map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks')%list) = Some dr /\
    (forall t, In t outs -> output_meaning d3 t <> None) /\
    AliasOk C' (append_environment (result_of d3 outs gensof dr) d) /\
    S.EnvOk W Prog argv read state_ok rr C' (append_environment (result_of d3 outs gensof dr) d) H3 /\
    S.KeysMatch C'.

(* LinkResultsOk, for a caller whose meanings are scoped (MeaningsScoped:
   a new invariant that Pre must carry; without it the obligation is false,
   see scratch/orch/results-STATUS.md). *)
Definition LinkResultsScopedOk : Prop := forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
  S.AppPremises W F C pa u fx Cu ns sm th gensof C' ks newgens outs ->
  forall rr d types H g st h dc gfx gg dg n ps a r bodies nsg Cg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j',
  S.Pre W Prog argv read state_ok rr F C d types H g st ->
  lookup_provision (IFun u) d = Some (FunctorClosure h [map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)] dc) ->
  S.ClosureOk W Prog argv read state_ok rr H u gfx (FunctorClosure h [map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)] dc) ->
  S.FixedMeans W u d fx gfx ->
  S.LinkData W Prog u gg h dg n ps a r bodies Cu ns Cg nsg Fg wrap Cb Fb made outs' gen Cr ks' binds0 ->
  select_provisions d sm = Some d1 ->
  select_provisions d1 (map (fun k => (A.rkey_of Cg k, A.rkey_of Cu k)) (S.linked_needs ns nsg)) = Some d2 ->
  S.I.eval_bindings j' P (application_environment P d h th (append_environment d2 dc)) [] st (flat_map (A.refile (map (fun k0 => (A.at_site Cb (A.output_renaming outs' gen k0), A.output_renaming outs' gen (A.rkey_of Cr k0))) ks')) binds0) = inl (d3, g3, st3) ->
  heap_extends H H3 -> S.Pre W Prog argv read state_ok rr Fb Cb d3 [] H3 g3 st3 ->
  MeaningsScoped F C d ->
  exists dr result,
    select_provisions d3 (inst_smap (result_substitution th (map (fun t => (t, gensof t)) outs)) (map ITy outs' ++ map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks')%list) = Some dr /\
    result_environment d (map (fun t => (t, gensof t)) outs) d3 (map ITy outs' ++ map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks')%list dr = Some result /\
    S.Pre W Prog argv read state_ok rr (A.with_fresh F newgens) C' (append_environment result d) types H3 g st3 /\
    S.MeaningsGrow newgens d (append_environment result d).

Theorem link_results_scoped : ResultCoreOk -> LinkResultsScopedOk.
Proof.
  intros Core F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h dc gfx gg dg n ps a r bodies
    nsg Cg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm LD Sel Sel2 Run Ext Pre3 Sc.
  destruct (Core F C pa u fx Cu ns sm th gensof C' ks newgens outs AP rr d types H g st h dc gfx gg dg n ps a r bodies
    nsg Cg Fg wrap Cb Fb made outs' gen Cr ks' binds0 d1 d2 d3 g3 st3 H3 j' Pre0 Look Ok Fm LD Sel Sel2 Run Ext Pre3)
    as [dr [Sdr [Read [Al [Env KM]]]]].
  pose proof Pre0 as [_ [_ [_ [_ [_ [Fc _]]]]]].
  pose proof LD as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsI' & _).
  destruct (result_environment_ok W F C pa u fx Cu ns sm th gensof C' ks newgens outs AP d d3 dr outs'
    (map (fun k0 => A.output_renaming outs' gen (A.rkey_of Cr k0)) ks') Fc Sc OutsI' Read) as [Re Gr].
  exists dr, (result_of d3 outs gensof dr). split; [exact Sdr|]. split; [exact Re|]. split; [|exact Gr].
  pose proof Pre3 as [_ [_ [_ [St3 _]]]].
  exact (pre_after_result W Prog argv read state_ok rr F C pa u fx Cu ns sm th gensof C' ks newgens outs AP
    d types H g st _ H3 st3 Pre0 Gr Ext St3 Al Env KM).
Qed.

End Assemble.

End LinkResults.
