(* The run fact for a linked body (realize2).

   - [app_env_meaning]: the body's environment at an application
     (application_environment) gives an input x the caller's meaning of
     its table reading, θ read at the caller, when every symbol of that
     reading is a linked input θ covers.
   - [body_meaning_kept]: a run of binds changes no type meaning outside
     its destinations (PublicChainCore.bindings_writes).
   - With PublicLinkPins.link_dests_avoid (no destination is a declared
     symbol of M(F)), the body's final meanings of F's mentioned inputs are
     the application's. *)
From Stdlib Require Import List Arith Bool.
From NM Require Import Audit PublicTypeRead PublicValueTyping PublicLocalsTyping PublicBodySafety PublicCallRealize PublicCopyFresh.
From NM Require PublicChainCore PublicLinkPins.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Lemma lookup_map_key_nat : forall (f : nat -> ty) l x, In x l -> lookup_subst x (map (fun s => (s, f s)) l) = Some (f x).
Proof.
  intros f l x. induction l as [|a r IH]; intros I; [contradiction|]. cbn.
  destruct (Nat.eqb_spec x a) as [->|Ne]; [reflexivity|]. destruct I as [E|I]; [congruence|exact (IH I)].
Qed.

Lemma lookup_map_snd : forall (f : ty -> ty) (s : subst) x,
  lookup_subst x (map (fun q => (fst q, f (snd q))) s) = option_map f (lookup_subst x s).
Proof.
  intros f s x. induction s as [|[a t] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x a); [reflexivity|exact IH].
Qed.

Lemma lookup_subst_app' : forall x (l1 l2 : subst),
  lookup_subst x (l1 ++ l2) = match lookup_subst x l1 with Some v => Some v | None => lookup_subst x l2 end.
Proof. intros x l1 l2. rewrite !lookup_subst_nlook', nlook_app'. reflexivity. Qed.

(* The table reading of an input of a functor code. *)
Definition table_reading (code : functor_code) (x : nat) : ty :=
  match lookup_subst x (fc_table code) with Some t => t | None => TAbs x end.

Lemma app_env_meaning : forall P d h code th dc x,
  view_functor P h = Some code -> In x (fc_inputs code) ->
  (forall y, In y (A.gens (table_reading code x)) ->
     In y (fc_inputs code) /\ In y (fc_linked code) /\ lookup_subst y th <> None) ->
  lookup_subst x (type_meanings (application_environment P d h th dc)) =
    Some (ground d (app_subst th (table_reading code x))).
Proof.
  intros P d h code th dc x V Ix Lk. unfold application_environment. rewrite V. cbn [type_meanings].
  rewrite lookup_map_key_nat by exact Ix. f_equal.
  set (base := fun source => match (if existsb (Nat.eqb source) (fc_linked code) then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning dc source with Some meaning => meaning | None => TAbs source end end).
  assert (Base : forall y, In y (fc_linked code) -> lookup_subst y th <> None ->
    base y = ground d (app_subst th (TAbs y))).
  { intros y Il Th. unfold base.
    assert (E : existsb (Nat.eqb y) (fc_linked code) = true) by (apply existsb_exists; exists y; split; [exact Il|apply Nat.eqb_refl]).
    rewrite E. cbn [app_subst]. destruct (lookup_subst y th) as [v|]; [reflexivity|congruence]. }
  unfold table_reading in Lk |- *. destruct (lookup_subst x (fc_table code)) as [t|].
  - rewrite ground_app_subst. apply subst_agree_on. intros y Iy. destruct (Lk y Iy) as [Iin [Il Th]].
    cbn [app_subst]. rewrite lookup_map_key_nat by exact Iin. refine (eq_trans (Base y Il Th) _).
    rewrite lookup_subst_app', lookup_map_snd. cbn [app_subst].
    destruct (lookup_subst y th) as [v|]; [reflexivity|congruence].
  - destruct (Lk x (or_introl eq_refl)) as [_ [Il Th]]. exact (Base x Il Th).
Qed.

Module LinkRun (Prims : PRIMITIVES) (I0 : INTERP Prims).
Module CC := PublicChainCore.ChainCore Prims I0.
Module S := CC.S.

(* A run of binds keeps every meaning outside its destinations. *)
Lemma body_meaning_kept : forall j Pv d g st bs d' g' st',
  S.I.eval_bindings j Pv d g st bs = inl (d', g', st') ->
  forall x, ~ In x (CC.binding_dests bs) -> lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d).
Proof.
  intros j Pv d g st bs d' g' st' E x Nx.
  destruct (CC.bindings_writes j Pv d g st bs d' g' st' E) as [[w [Ew Iw]] _].
  rewrite Ew, lookup_subst_app'. destruct (lookup_subst x w) as [v|] eqn:L; [|reflexivity].
  exfalso. apply Nx, Iw. change x with (fst (x, v)). apply in_map. exact (lookup_subst_in_pair _ _ _ L).
Qed.

(* In a body bound to F, a declared symbol of M(F) keeps its meaning. *)
Lemma body_meaning_mentioned : forall W Prog u Fg Cg its wrap Cb Fb made binds,
  (forall x, In x (A.fr_pins Fg) <-> A.Mentions W u x) ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  forall j Pv d g st d' g' st', S.I.eval_bindings j Pv d g st binds = inl (d', g', st') ->
  forall x, A.Mentions W u x -> ~ A.Fresh W x ->
  lookup_subst x (type_meanings d') = lookup_subst x (type_meanings d).
Proof.
  intros W Prog u Fg Cg its wrap Cb Fb made binds Pins D B j Pv d g st d' g' st' E x Mx Nf.
  apply (body_meaning_kept j Pv d g st binds d' g' st' E). intros Ix.
  exact (PublicLinkPins.link_dests_avoid W Prog u Fg Cg its wrap Cb Fb made binds Pins D B x Ix Mx Nf).
Qed.

End LinkRun.
