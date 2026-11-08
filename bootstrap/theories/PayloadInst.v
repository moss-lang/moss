(* A declaration payload is a schema. Union nodes written in that schema
   flatten after parameter substitution, while parameter values themselves
   remain opaque: their nominal argument/layout identity is preserved. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel.
Import ListNotations.

Fixpoint payload_atoms (tau : ty) : list ty :=
  match tau with
  | TUnion ms => flat_map payload_atoms ms
  | _ => [tau]
  end.
(* ATUnion preserves a singleton union; it does not collapse to its member. *)
Definition payload_join (operands : list ty) : ty :=
  TUnion (flat_map payload_atoms operands).

Fixpoint payload_inst (th : subst) (template : ty) : ty :=
  match template with
  | TUnit => TUnit
  | TAbs t => match lookup_subst t th with Some tau => tau | None => TAbs t end
  | TNom n args => TNom n (map (fun p => (fst p,payload_inst th (snd p))) args)
  | TUnion ms => payload_join (map (payload_inst th) ms)
  end.

Lemma payload_inst_opaque : forall th t tau,
    lookup_subst t th = Some tau -> payload_inst th (TAbs t) = tau.
Proof. intros. cbn. rewrite H. reflexivity. Qed.

Example payload_union_argument :
  payload_inst [(3,TUnion [TNom 2 [];TNom 5 []])]
    (TUnion [TAbs 3;TNom 1 []]) = TUnion [TNom 2 [];TNom 5 [];TNom 1 []].
Proof. reflexivity. Qed.
Example payload_nested_argument :
  payload_inst [(3,TUnion [TNom 2 [];TNom 5 []])]
    (TNom 7 [(8,TUnion [TAbs 3;TNom 1 []])]) =
      TNom 7 [(8,TUnion [TNom 2 [];TNom 5 [];TNom 1 []])].
Proof. reflexivity. Qed.
Example payload_never_argument :
  payload_inst [(3,TUnion [])] (TUnion [TAbs 3;TNom 1 []]) = TUnion [TNom 1 []].
Proof. reflexivity. Qed.
Example payload_opaque_identity : forall tau,
    payload_inst [(3,tau)] (TAbs 3) = tau.
Proof. reflexivity. Qed.
Example payload_written_singleton :
    payload_inst [(3,TNom 2 [])] (TUnion [TAbs 3;TUnion []]) = TUnion [TNom 2 []].
Proof. reflexivity. Qed.


Definition payload_nominal (tau : ty) : Prop := exists n args, tau = TNom n args.
Inductive payload_shape : ty -> Prop :=
| PsUnit : payload_shape TUnit
| PsAbs : forall t, payload_shape (TAbs t)
| PsNom : forall n args,
    Forall (fun p => payload_shape (snd p)) args -> payload_shape (TNom n args)
| PsUnion : forall ms,
    Forall payload_shape ms -> Forall payload_nominal ms -> payload_shape (TUnion ms).

Definition payload_flat (tau : ty) : Prop :=
  match tau with
  | TUnion ms => Forall (fun t => forall inner, t <> TUnion inner) ms
  | _ => True
  end.

Lemma payload_shape_flat : forall tau, payload_shape tau -> payload_flat tau.
Proof.
  intros tau H. destruct H; cbn; try exact I.
  rewrite Forall_forall in H0 |- *. intros t Hin inner E.
  destruct (H0 t Hin) as [n [args Et]]. congruence.
Qed.

Inductive payload_covered (th : subst) : ty -> Prop :=
| PcUnit : payload_covered th TUnit
| PcAbs : forall t actual, lookup_subst t th = Some actual -> payload_covered th (TAbs t)
| PcNom : forall n args,
    Forall (fun p => payload_covered th (snd p)) args -> payload_covered th (TNom n args)
| PcUnion : forall ms,
    Forall (payload_covered th) ms -> payload_covered th (TUnion ms).
Definition payload_map_subst (g th : subst) : subst :=
  map (fun p => (fst p,app_subst g (snd p))) th.

Section PayloadTypeInduction.
Variable Q : ty -> Prop.
Hypothesis Qunit : Q TUnit.
Hypothesis Qabs : forall t, Q (TAbs t).
Hypothesis Qnom : forall n args, Forall (fun p => Q (snd p)) args -> Q (TNom n args).
Hypothesis Qunion : forall ms, Forall Q ms -> Q (TUnion ms).
Fixpoint payload_type_ind (tau : ty) {struct tau} : Q tau :=
  match tau with
  | TUnit => Qunit
  | TAbs t => Qabs t
  | TNom n args => Qnom n args ((fix go (xs : list (tsym * ty)) : Forall (fun p => Q (snd p)) xs :=
      match xs with [] => Forall_nil _ | p :: rest =>
      Forall_cons _ (payload_type_ind (snd p)) (go rest) end) args)
  | TUnion ms => Qunion ms ((fix go (xs : list ty) : Forall Q xs :=
      match xs with [] => Forall_nil _ | t :: rest =>
      Forall_cons _ (payload_type_ind t) (go rest) end) ms)
  end.
End PayloadTypeInduction.

Lemma payload_lookup_map : forall g th t,
    lookup_subst t (payload_map_subst g th) = option_map (app_subst g) (lookup_subst t th).
Proof.
  intros g th. induction th as [|[key tau] rest IH]; intro t; cbn; [reflexivity|].
  destruct (Nat.eqb t key); [reflexivity|exact (IH t)].
Qed.
Lemma payload_app_nom : forall g n args,
    app_subst g (TNom n args) = TNom n (map (fun p => (fst p,app_subst g (snd p))) args).
Proof. reflexivity. Qed.
Lemma payload_app_union : forall g ms,
    app_subst g (TUnion ms) = TUnion (map (app_subst g) ms).
Proof. reflexivity. Qed.

Lemma payload_forall_flat_map : forall A B (P : B -> Prop) (f : A -> list B) xs,
    Forall P (flat_map f xs) -> forall x, In x xs -> Forall P (f x).
Proof.
  intros A B P f xs H x Hin. rewrite Forall_forall in H |- *.
  intros b Hb. apply H,in_flat_map. exists x. auto.
Qed.
Lemma payload_map_flat_map : forall A B C (f : A -> list B) (h : B -> C) xs,
    map h (flat_map f xs) = flat_map (fun x => map h (f x)) xs.
Proof. intros A B C f h xs. induction xs; cbn; [reflexivity|]. rewrite map_app,IHxs. reflexivity. Qed.
Lemma payload_flat_map_map : forall A B C (f : B -> list C) (h : A -> B) xs,
    flat_map f (map h xs) = flat_map (fun x => f (h x)) xs.
Proof. intros A B C f h xs. induction xs; cbn; congruence. Qed.
Lemma payload_flat_map_ext : forall A B (f h : A -> list B) xs,
    (forall x, In x xs -> f x = h x) -> flat_map f xs = flat_map h xs.
Proof.
  intros A B f h xs. induction xs as [|x rest IH]; intro H; cbn; [reflexivity|].
  rewrite (H x (or_introl eq_refl)). f_equal. apply IH. intros y Hy. apply H. right. exact Hy.
Qed.

Lemma payload_atoms_nonunion : forall tau,
    Forall (fun t => forall inner, t <> TUnion inner) (payload_atoms tau).
Proof.
  induction tau using payload_type_ind; cbn.
  - constructor; [intros inner E; discriminate|constructor].
  - constructor; [intros inner E; discriminate|constructor].
  - constructor; [intros inner E; discriminate|constructor].
  - rewrite Forall_forall in H |- *. intros t Hin.
    apply in_flat_map in Hin as (m & Hm & Hin).
    exact ((proj1 (Forall_forall _ _) (H m Hm)) t Hin).
Qed.
Lemma payload_flat_atoms_identity : forall ms,
    Forall (fun t => forall inner, t <> TUnion inner) ms -> flat_map payload_atoms ms = ms.
Proof.
  intros ms H. induction H as [|t rest Ht Hr IH]; cbn; [reflexivity|].
  destruct t as [|key|n args|members]; try (cbn; rewrite IH; reflexivity).
  exfalso. apply (Ht members). reflexivity.
Qed.
Lemma payload_atoms_join : forall operands,
    payload_atoms (payload_join operands) = flat_map payload_atoms operands.
Proof.
  intro operands. unfold payload_join. cbn. apply payload_flat_atoms_identity.
  rewrite Forall_forall. intros t Hin. apply in_flat_map in Hin as (operand & _ & Hin).
  exact ((proj1 (Forall_forall _ _) (payload_atoms_nonunion operand)) t Hin).
Qed.

Lemma payload_atoms_app_nominal : forall tau g,
    Forall payload_nominal (payload_atoms tau) ->
    map (app_subst g) (payload_atoms tau) = payload_atoms (app_subst g tau).
Proof.
  induction tau using payload_type_ind; intros g Hnom.
  - inversion Hnom; subst. match goal with Hbad : payload_nominal TUnit |- _ =>
      destruct Hbad as [n [args E]]; discriminate end.
  - inversion Hnom; subst. match goal with Hbad : payload_nominal (TAbs _) |- _ =>
      destruct Hbad as [n [args E]]; discriminate end.
  - rewrite payload_app_nom. reflexivity.
  - cbn [payload_atoms] in Hnom |- *. rewrite payload_app_union. cbn [payload_atoms].
    rewrite payload_map_flat_map,payload_flat_map_map.
    apply payload_flat_map_ext. intros t Hin.
    rewrite Forall_forall in H. apply (H t Hin).
    eapply payload_forall_flat_map; eassumption.
Qed.

Lemma payload_shape_nom_args : forall n args,
    payload_shape (TNom n args) -> Forall (fun p => payload_shape (snd p)) args.
Proof. intros n args H. inversion H; assumption. Qed.
Lemma payload_shape_union_args : forall ms,
    payload_shape (TUnion ms) -> Forall payload_shape ms /\ Forall payload_nominal ms.
Proof. intros ms H. inversion H; auto. Qed.

(* The second clause supplies precisely the flattened-union induction needed
   by the first; opaque parameter values are never recursively rewritten. *)
Lemma payload_inst_commutes_aux : forall template th g,
    payload_covered th template ->
    (payload_shape (payload_inst th template) ->
      app_subst g (payload_inst th template) =
      payload_inst (payload_map_subst g th) template) /\
    (Forall payload_shape (payload_atoms (payload_inst th template)) ->
     Forall payload_nominal (payload_atoms (payload_inst th template)) ->
      map (app_subst g) (payload_atoms (payload_inst th template)) =
      payload_atoms (payload_inst (payload_map_subst g th) template)).
Proof.
  induction template using payload_type_ind; intros th g Hcover.
  - split; [reflexivity|]. cbn. intros _ Hbad. inversion Hbad; subst.
    match goal with H : payload_nominal TUnit |- _ => destruct H as [n [args E]]; discriminate end.
  - inversion Hcover as [|key actual Hlookup| |]; subst. cbn. rewrite Hlookup,payload_lookup_map,Hlookup. cbn.
    split; [reflexivity|]. intros _ Hnom. apply payload_atoms_app_nominal. exact Hnom.
  - inversion Hcover as [| |head actuals Hcovargs|]; subst.
    assert (Hraw : payload_shape (payload_inst th (TNom n args)) ->
      app_subst g (payload_inst th (TNom n args)) =
        payload_inst (payload_map_subst g th) (TNom n args)).
    { intro Hshape. apply payload_shape_nom_args in Hshape.
      cbn [payload_inst]. rewrite payload_app_nom. f_equal. rewrite map_map.
      apply map_ext_in. intros [key tau] Hin. cbn. f_equal.
      rewrite Forall_forall in H,Hshape,Hcovargs.
      apply (proj1 (H (key,tau) Hin th g (Hcovargs (key,tau) Hin))).
      exact (Hshape (key,payload_inst th tau) (in_map _ _ _ Hin)). }
    split; [exact Hraw|].
    intros Hshape _. inversion Hshape; subst.
    change ([app_subst g (payload_inst th (TNom n args))] =
      [payload_inst (payload_map_subst g th) (TNom n args)]).
    rewrite Hraw; [reflexivity|assumption].
  - inversion Hcover as [| | |members Hcovmembers]; subst.
    assert (Hleaves :
      Forall payload_shape (flat_map (fun t => payload_atoms (payload_inst th t)) ms) ->
      Forall payload_nominal (flat_map (fun t => payload_atoms (payload_inst th t)) ms) ->
      map (app_subst g) (flat_map (fun t => payload_atoms (payload_inst th t)) ms) =
      flat_map (fun t => payload_atoms (payload_inst (payload_map_subst g th) t)) ms).
    { intros Hshape Hnom. rewrite payload_map_flat_map.
      apply payload_flat_map_ext. intros t Hin.
      rewrite Forall_forall in H,Hcovmembers.
      apply (proj2 (H t Hin th g (Hcovmembers t Hin))).
      - exact (payload_forall_flat_map _ _ _ _ _ Hshape t Hin).
      - exact (payload_forall_flat_map _ _ _ _ _ Hnom t Hin). }
    split.
    + intro Hshape. cbn [payload_inst]. unfold payload_join.
      rewrite payload_app_union. f_equal.
      rewrite !payload_flat_map_map. apply Hleaves.
      * apply payload_shape_union_args in Hshape. rewrite payload_flat_map_map in Hshape. exact (proj1 Hshape).
      * apply payload_shape_union_args in Hshape. rewrite payload_flat_map_map in Hshape. exact (proj2 Hshape).
    + intros Hshape Hnom. cbn [payload_inst] in *.
      rewrite !payload_atoms_join in *. rewrite !payload_flat_map_map in *.
      apply Hleaves; assumption.
Qed.

Lemma payload_inst_commutes : forall template th g,
    payload_covered th template -> payload_shape (payload_inst th template) ->
    app_subst g (payload_inst th template) =
      payload_inst (payload_map_subst g th) template.
Proof. intros. eapply (proj1 (payload_inst_commutes_aux template th g H)); exact H0. Qed.

(* Immutable lowering reads source payload schemas. Legacy Core programmes
   retain their original syntactic substitution semantics. *)
Definition ordinary_payload (P : xprog) (th : subst) (template : ty) : ty :=
  if xp_immutable P then payload_inst th template else app_subst th template.
Definition ordinary_payload_ok (P : xprog) (th : subst) (template : ty) : Prop :=
  xp_immutable P = true ->
    payload_covered th template /\ payload_shape (payload_inst th template).
Definition xmember_payload (P : xprog) (Sg_tag : nsym -> option (tele * ty)) (mem : ty) : ty :=
  match mem with
  | TNom n th => match Sg_tag n with
      | Some (_,template) => ordinary_payload P th template
      | None => TUnit end
  | _ => TUnit end.

(* The selected GC schema validator checks the identity instance for a bare
   handle parameter. Written unions, including singleton unions, cannot pass
   that check; subsequent actual arguments therefore remain opaque. *)
Lemma payload_inst_identity_abs_inv : forall template th h,
    (forall t tau, lookup_subst t th = Some tau -> tau = TAbs t) ->
    payload_inst th template = TAbs h -> template = TAbs h.
Proof.
  intros template th h Hid Heq. destruct template; cbn in Heq; try discriminate.
  destruct (lookup_subst t th) eqn:E.
  - rewrite (Hid t t0 E) in Heq. congruence.
  - congruence.
Qed.

Lemma payload_inst_bare_parameter : forall t th,
    payload_inst th (TAbs t) = app_subst th (TAbs t).
Proof. reflexivity. Qed.

Lemma ordinary_payload_nom_parameter_ok : forall P th n args key tau,
    ordinary_payload_ok P th (TNom n args) -> In (key,tau) args ->
    ordinary_payload_ok P th tau.
Proof.
  intros P th n args key tau Hok Hin Mode.
  destruct (Hok Mode) as [Hcov Hshape]. inversion Hcov; subst.
  apply payload_shape_nom_args in Hshape. split.
  - match goal with Hargs : Forall (fun p => payload_covered _ (snd p)) _ |- _ =>
      rewrite Forall_forall in Hargs; exact (Hargs (key,tau) Hin) end.
  - rewrite Forall_forall in Hshape.
    exact (Hshape (key,payload_inst th tau) (in_map _ _ _ Hin)).
Qed.

Lemma payload_inst_nil_shape_identity : forall template,
    payload_shape template -> payload_inst [] template = template.
Proof.
  induction template using payload_type_ind; intro Hshape; try reflexivity.
  - cbn. f_equal. rewrite <- (map_id args) at 2. apply map_ext_in.
    intros [key tau] Hin. cbn. f_equal.
    apply payload_shape_nom_args in Hshape. rewrite Forall_forall in H,Hshape.
    exact (H (key,tau) Hin (Hshape (key,tau) Hin)).
  - cbn. unfold payload_join. apply payload_shape_union_args in Hshape.
    destruct Hshape as [Hshape Hnom].
    assert (Emap : map (payload_inst []) ms = ms).
    { rewrite <- (map_id ms) at 2. apply map_ext_in. intros t Hin.
      rewrite Forall_forall in H,Hshape. exact (H t Hin (Hshape t Hin)). }
    rewrite Emap. f_equal. apply payload_flat_atoms_identity.
    rewrite Forall_forall in Hnom |- *. intros t Hin inner Eq.
    destruct (Hnom t Hin) as [n [args ->]]. discriminate.
Qed.

Lemma ordinary_payload_nom : forall P th n args,
    ordinary_payload P th (TNom n args) =
      TNom n (map (fun p => (fst p,ordinary_payload P th (snd p))) args).
Proof. intros. unfold ordinary_payload. destruct (xp_immutable P); reflexivity. Qed.
