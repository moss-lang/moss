(* The world-level formation premises of the public safety theorem
   (PublicEntry.safety_obligations), proved from what an elaborating
   program satisfies:

   - mentions_typed_of: every symbol a functor's result mentions is a type
     symbol (PublicFormation.MentionsTyped), from [WorldOf]: requirements
     are type symbols, closed through the least relevance-closed reading.

   Context type formation follows the class-shape evidence directly in
   PublicFormation; no world-wide substitution-row obligation is needed. *)
From Stdlib Require Import List String Arith Bool Lia.
From NM Require Import Audit PublicTypeRead PublicFormation.
From NM Require Import AuditFacts.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- requirements are type symbols ------------------------------------------ *)

Section Requirements.
Variables (b : bundle) (W : A.world).
Hypothesis Wo : A.WorldOf b W.
Import A.

Lemma dkind_eq : forall k k' : dkind, {k = k'} + {k <> k'}.
Proof. decide equality. Qed.

Definition Typed (x : nat) : Prop := kind_sym W x = Some DKType.

(* An entity a loaded scope holds: a declared symbol or a loaded module. *)
Definition Good (e : ent) : Prop :=
  match e with ESym s => w_decl W s <> None | EMod t => Loaded b t end.

Lemma exports_good : forall p ns n e, Exports b W p ns n e -> Loaded b p -> Good e.
Proof.
  fix IH 5. intros p ns n e Ex Lp.
  destruct Ex as [p s d ns n D _ _|p m t imp u e ns M I _ _ _ Dis|p m t imp a M I _].
  - cbn. rewrite D. discriminate.
  - assert (Lt : Loaded b t) by (eapply LoadImport; [exact Lp|exact M|apply explicit_in_imports; exact I]).
    destruct Dis as [Ex'|[_ [_ Ex']]]; pose proof (IH _ _ _ _ Ex' Lt) as G; destruct e; exact G.
  - cbn. eapply LoadImport; [exact Lp|exact M|apply explicit_in_imports; exact I].
Qed.

Lemma scope_good : forall p ns n e, Loaded b p -> w_scope W p ns n = Some e -> Good e.
Proof.
  intros p ns n e Lp S. apply (proj1 (@wo_scope b W Wo p ns n e Lp)) in S.
  destruct S as [p ns n e Os|p m ns n e M Pre Ex _].
  - destruct Os as [p ns n e Ex|p m t imp ns n e M I _ Ex].
    + exact (exports_good _ _ _ _ Ex Lp).
    + apply (exports_good _ _ _ _ Ex). eapply LoadImport; [exact Lp|exact M|apply explicit_in_imports; exact I].
  - apply (exports_good _ _ _ _ Ex). eapply LoadImport; [exact Lp|exact M|apply prelude_in_imports; exact Pre].
Qed.

Lemma resolve_rest_good : forall segs e e', Good e -> resolve_rest W e segs = Some e' -> Good e'.
Proof.
  induction segs as [|seg r IH]; intros e e' Ge R; cbn in R; [injection R as <-; exact Ge|].
  destruct e as [s|t]; [discriminate|]. cbn in Ge.
  destruct (w_exp W t NSName seg) as [e1|] eqn:E1.
  - apply (IH e1); [|exact R]. apply (proj1 (@wo_exports b W Wo t NSName seg e1 Ge)) in E1.
    exact (exports_good _ _ _ _ E1 Ge).
  - destruct (w_exp W t NSAlias seg) as [e1|] eqn:E2; [|discriminate].
    apply (IH e1); [|exact R]. apply (proj1 (@wo_exports b W Wo t NSAlias seg e1 Ge)) in E2.
    exact (exports_good _ _ _ _ E2 Ge).
Qed.

Lemma resolve_sym_declared : forall p pa s, Loaded b p -> resolve_sym W p pa = Some s -> w_decl W s <> None.
Proof.
  intros p pa s Lp R. unfold resolve_sym, resolve in R.
  destruct pa as [|hd rest]; [discriminate|].
  assert (G : exists e, Good e /\ resolve_rest W e rest = Some (ESym s)).
  { destruct (w_scope W p NSName hd) as [e|] eqn:S1.
    - exists e. split; [exact (scope_good _ _ _ _ Lp S1)|]. destruct (resolve_rest W e rest) as [[]|]; congruence.
    - destruct (w_scope W p NSAlias hd) as [e|] eqn:S2; [|discriminate].
      exists e. split; [exact (scope_good _ _ _ _ Lp S2)|]. destruct (resolve_rest W e rest) as [[]|]; congruence. }
  destruct G as [e [Ge R']]. exact (resolve_rest_good _ _ _ Ge R').
Qed.

Lemma declared_kind : forall s, w_decl W s <> None -> kind_sym W s <> None.
Proof.
  intros s D. unfold kind_sym. destruct (w_decl W s) as [d|] eqn:E; [|contradiction].
  exact (proj2 (@wo_decl_sound b W Wo s d E)).
Qed.

Lemma decl_loaded : forall s d, w_decl W s = Some d -> Loaded b (d_mod d).
Proof. intros s d D. exact (proj1 (proj1 (@wo_decl_sound b W Wo s d D))). Qed.

Lemma spec_types_typed : forall p sp t, SpecTypes W p sp t -> Typed t.
Proof. intros p sp t S. induction S; assumption. Qed.

Lemma reqs_plain_typed : forall s k t, kind_sym W s = Some k -> k <> DKTag -> k <> DKAlias ->
  In t (w_reqs W s) -> Typed t.
Proof.
  intros s k t K Nt Na I.
  destruct (dkind_eq k DKType) as [->|Nty].
  - rewrite (@wo_reqs_none b W Wo s (or_introl K)) in I. contradiction.
  - destruct (dkind_eq k DKUnit) as [->|Nu].
    + rewrite (@wo_reqs_none b W Wo s (or_intror K)) in I. contradiction.
    + apply (proj1 (@wo_reqs_plain b W Wo s k K Nt Na Nty Nu t)) in I.
      destruct I as [d [pa [_ [_ S]]]]. exact (spec_types_typed _ _ _ S).
Qed.

Lemma freein_mono : forall (R R' : nat -> list nat) p a x, (forall g, incl (R g) (R' g)) ->
  FreeIn W R p a x -> FreeIn W R' p a x.
Proof.
  intros R R' p a x Inc F. induction F.
  - eapply FRec; eassumption.
  - eapply FUnion; eassumption.
  - eapply FType; eassumption.
  - eapply FNom; [eassumption|eassumption|apply Inc; eassumption|eassumption].
  - eapply FBracket; eassumption.
Qed.

Lemma freein_typed : forall (R : nat -> list nat) p a x, Loaded b p ->
  (forall g y, w_decl W g <> None -> In y (R g) -> Typed y) ->
  FreeIn W R p a x -> Typed x.
Proof.
  intros R p a x Lp RT F. induction F as [|? ? ? ? ? IH|pa a t Rs K _|pa a g x Rs K I _|]; try assumption.
  exact (RT g x (resolve_sym_declared _ _ _ Lp Rs) I).
Qed.

Definition nominal (g : nat) : bool :=
  match kind_sym W g with Some DKTag | Some DKAlias => true | _ => false end.
Definition is_typed (x : nat) : bool :=
  match kind_sym W x with Some DKType => true | _ => false end.
Definition typed_reqs (g : nat) : list nat :=
  if nominal g then filter is_typed (w_reqs W g) else w_reqs W g.

Lemma is_typed_spec : forall x, is_typed x = true <-> Typed x.
Proof.
  intros x. unfold is_typed, Typed. destruct (kind_sym W x) as [[]|]; split; intro H; congruence.
Qed.

Lemma typed_reqs_incl : forall g, incl (typed_reqs g) (w_reqs W g).
Proof.
  intros g y I. unfold typed_reqs in I. destruct (nominal g); [|exact I].
  apply filter_In in I. exact (proj1 I).
Qed.

Lemma typed_reqs_typed : forall g y, w_decl W g <> None -> In y (typed_reqs g) -> Typed y.
Proof.
  intros g y D I. unfold typed_reqs in I. destruct (nominal g) eqn:N.
  - apply filter_In in I. exact (proj1 (is_typed_spec y) (proj2 I)).
  - destruct (kind_sym W g) as [k|] eqn:K; [|exfalso; exact (declared_kind g D K)].
    apply (reqs_plain_typed g k y K); [intros ->; unfold nominal in N; rewrite K in N; discriminate
      |intros ->; unfold nominal in N; rewrite K in N; discriminate|exact I].
Qed.

Lemma nominal_body_kind : forall g d ps t, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) ->
  nominal g = true.
Proof.
  intros g d ps t D N. unfold nominal, kind_sym. rewrite D.
  destruct (d_ast d); cbn in N |- *; try discriminate; reflexivity.
Qed.

Lemma typed_reqs_closed : RelevanceClosed W typed_reqs.
Proof.
  intros g d ps t D N. pose proof (nominal_body_kind g d ps t D N) as Nom.
  destruct (@wo_reqs_closed b W Wo g d ps t D N) as [Listed Free]. split.
  - intros n x In' Rs. unfold typed_reqs. rewrite Nom. apply filter_In. split; [exact (Listed n x In' Rs)|].
    apply is_typed_spec. destruct (@wo_listed b W Wo g d ps t n D N In') as [x' [Rs' [d' [pa [_ [_ S]]]]]].
    rewrite Rs in Rs'. injection Rs' as <-. exact (spec_types_typed _ _ _ S).
  - intros x F. unfold typed_reqs. rewrite Nom. apply filter_In. split.
    + apply Free. exact (freein_mono _ _ _ _ _ typed_reqs_incl F).
    + apply is_typed_spec. exact (freein_typed _ _ _ _ (decl_loaded _ _ D) typed_reqs_typed F).
Qed.

(* Every requirement of a declared symbol is a type symbol. *)
Theorem reqs_typed : forall s t, w_decl W s <> None -> In t (w_reqs W s) -> Typed t.
Proof.
  intros s t D I. destruct (kind_sym W s) as [k|] eqn:K; [|exfalso; exact (declared_kind s D K)].
  destruct (dkind_eq k DKTag) as [->|Nt]; [|destruct (dkind_eq k DKAlias) as [->|Na]];
    [| |exact (reqs_plain_typed s k t K Nt Na I)];
  assert (Inc : incl (w_reqs W s) (typed_reqs s))
    by (apply (@wo_reqs_least b W Wo typed_reqs typed_reqs_closed); [|rewrite K; auto];
        intros s' Nt' Na'; unfold typed_reqs, nominal;
        destruct (kind_sym W s') as [[]|]; solve [reflexivity|contradiction]);
  apply (typed_reqs_typed s t D); exact (Inc t I).
Qed.

Lemma method_named_declared : forall p r dn m, Loaded b p -> MethodNamed W p r dn m -> w_decl W m <> None.
Proof.
  intros p r dn m Lp [At|[_ S]].
  - apply (proj1 (@wo_attached b W Wo r dn m)) in At. destruct At as [d [_ [_ [_ [_ [D _]]]]]].
    rewrite D. discriminate.
  - exact (scope_good _ _ _ _ Lp S).
Qed.

Lemma sig_mentions_typed : forall m t, SigMentions W m t -> Typed t.
Proof.
  intros m t (d & rcv & n & ps & rt & body & D & _ & [(pt & _ & F)|(r & _ & F)]);
    exact (freein_typed _ _ _ _ (decl_loaded _ _ D) (fun g y D I => reqs_typed g y D I) F).
Qed.

Lemma result_mentions_typed : forall self p sp t, ResultMentions W self p sp t -> Loaded b p -> Typed t.
Proof.
  intros self p sp t R. induction R as
    [self p pa a s t Rs K I _|self p pa dn a r t Rs Cases _|self p pa dn a r m t Rs Mn SM _
    |self p pa d a bp sp dr sa t _ _ F|self p pa a c dc n its sp t Rs Dc _ _ _ IH _
    |self p pa a u du n names args res sp t Rs Du _ _ _ _ IH _ _ _]; intros Lp.
  - destruct I as [I|[_ SM]]; [|exact (sig_mentions_typed s t SM)].
    destruct K as [K|K]; exact (reqs_plain_typed s _ t K ltac:(discriminate) ltac:(discriminate) I).
  - destruct Cases as [[-> K]|[_ I]]; [exact K|].
    exact (reqs_typed r t (resolve_sym_declared _ _ _ Lp Rs) I).
  - exact (sig_mentions_typed m t SM).
  - exact (freein_typed _ _ _ _ Lp (fun g y D I => reqs_typed g y D I) F).
  - exact (IH (decl_loaded _ _ Dc)).
  - exact (IH (decl_loaded _ _ Du)).
Qed.

Theorem mentions_typed_of : PublicFormation.MentionsTyped W.
Proof.
  intros u t (d & ps & a & r & b0 & sp & D & _ & _ & R & _).
  exact (result_mentions_typed _ _ _ _ R (decl_loaded _ _ D)).
Qed.

End Requirements.
