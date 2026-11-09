(* The world-level formation premises of the public safety theorem
   (PublicEntry.safety_obligations), proved from what an elaborating
   program satisfies:

   - mentions_typed_of: every symbol a functor's result mentions is a type
     symbol (PublicFormation.MentionsTyped), from [WorldOf]: requirements
     are type symbols, closed through the least relevance-closed reading;
   - merge_formed_of: a merge of formed entries maps each generation to a
     formed type (PublicFormation.MergeFormed), from most generality alone,
     by probing the merge with a unifier that marks unformed nodes. *)
From Stdlib Require Import List ListDec String Arith Bool Lia.
From NM Require Import Audit PublicTypeRead PublicFormation.
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

Lemma result_mentions_typed : forall self p sp t, ResultMentions W self p sp t -> Loaded b p -> Typed t.
Proof.
  intros self p sp t R. induction R as
    [self p pa a s t Rs K I _|self p pa dn a r t Rs Cases _|self p pa dn a r m t Rs Mn I _ _
    |self p pa d a bp sp dr sa t _ _ F|self p pa a c dc n its sp t Rs Dc _ _ _ IH _
    |self p pa a u du n names args res sp t Rs Du _ _ _ _ IH _ _ _]; intros Lp.
  - destruct K as [K|K]; exact (reqs_plain_typed s _ t K ltac:(discriminate) ltac:(discriminate) I).
  - destruct Cases as [[-> K]|[_ I]]; [exact K|].
    exact (reqs_typed r t (resolve_sym_declared _ _ _ Lp Rs) I).
  - exact (reqs_typed m t (method_named_declared _ _ _ _ Lp Mn) I).
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

(* ---- merges of formed entries are formed ------------------------------------- *)

(* The probe. [mark] keeps a formed node and wraps an unformed one in a
   union no formed type is (a union headed by `()`, or by `{}` beside the
   node), so it is injective ([unmark] undoes it) and fixes exactly the
   formed types. Applied after a merge σ it is again a unifier of formed
   entries, so most generality forces σ's values to be fixed by it. *)
Section Merge.
Variable W : A.world.
Import A.

Lemma onat_eq : forall x y : option nat, {x = y} + {x <> y}.
Proof. decide equality. apply Nat.eq_dec. Qed.

Definition is_nom (t : ty) : bool := match t with TNom _ _ => true | _ => false end.
Definition keys_ok (n : nat) (xs : list (nat * ty)) : bool :=
  if list_eq_dec Nat.eq_dec (map fst xs) (w_reqs W n) then true else false.
Definition union_ok (ms : list ty) : bool :=
  forallb is_nom ms && (if ListDec.NoDup_dec onat_eq (map head ms) then true else false).

Fixpoint mark (t : ty) : ty :=
  match t with
  | TMachine m => TMachine m
  | TUnit => TUnit
  | TAbs g => TAbs g
  | TRecord fs => TRecord ((fix go (l : list (string * ty)) :=
      match l with [] => [] | p :: r => (fst p, mark (snd p)) :: go r end) fs)
  | TNom n xs =>
      let xs' := (fix go (l : list (nat * ty)) :=
        match l with [] => [] | p :: r => (fst p, mark (snd p)) :: go r end) xs in
      if keys_ok n xs then TNom n xs' else TUnion [TRecord []; TNom n xs']
  | TUnion ms =>
      let ms' := (fix go (l : list ty) := match l with [] => [] | u :: r => mark u :: go r end) ms in
      if union_ok ms then TUnion ms' else TUnion (TUnit :: ms')
  end.

Fixpoint unmark (t : ty) : ty :=
  match t with
  | TRecord fs => TRecord ((fix go (l : list (string * ty)) :=
      match l with [] => [] | p :: r => (fst p, unmark (snd p)) :: go r end) fs)
  | TNom n xs => TNom n ((fix go (l : list (nat * ty)) :=
      match l with [] => [] | p :: r => (fst p, unmark (snd p)) :: go r end) xs)
  | TUnion ms =>
      match ms with
      | TUnit :: r => TUnion ((fix go (l : list ty) := match l with [] => [] | u :: r => unmark u :: go r end) r)
      | [TRecord []; m] => unmark m
      | _ => TUnion ((fix go (l : list ty) := match l with [] => [] | u :: r => unmark u :: go r end) ms)
      end
  | t => t
  end.

Lemma mark_record : forall fs, mark (TRecord fs) = TRecord (map (fun q => (fst q, mark (snd q))) fs).
Proof. intros. reflexivity. Qed.

Lemma mark_nom : forall n xs, mark (TNom n xs) =
  if keys_ok n xs then TNom n (map (fun q => (fst q, mark (snd q))) xs)
  else TUnion [TRecord []; TNom n (map (fun q => (fst q, mark (snd q))) xs)].
Proof. intros. reflexivity. Qed.

Lemma mark_union : forall ms, mark (TUnion ms) =
  if union_ok ms then TUnion (map mark ms) else TUnion (TUnit :: map mark ms).
Proof. intros. reflexivity. Qed.

Lemma unmark_nom : forall n xs, unmark (TNom n xs) = TNom n (map (fun q => (fst q, unmark (snd q))) xs).
Proof. intros. reflexivity. Qed.

Lemma unmark_record : forall fs, unmark (TRecord fs) = TRecord (map (fun q => (fst q, unmark (snd q))) fs).
Proof. intros. reflexivity. Qed.

Lemma unmark_union_plain : forall ms,
  match ms with TUnit :: _ => False | [TRecord []; _] => False | _ => True end ->
  unmark (TUnion ms) = TUnion (map unmark ms).
Proof.
  intros ms Sh. destruct ms as [|[| | | | |] r]; cbn in Sh; try contradiction; try reflexivity.
  destruct l as [|f l]; [|reflexivity]. destruct r as [|m [|m' r]]; [reflexivity|contradiction|reflexivity].
Qed.

Lemma unmark_union_unit : forall ms, unmark (TUnion (TUnit :: ms)) = TUnion (map unmark ms).
Proof. intros. reflexivity. Qed.

Lemma mark_head : forall t, is_nom t = true -> exists n ys, mark t = TNom n ys \/ mark t = TUnion [TRecord []; TNom n ys].
Proof.
  intros [| | | |n xs|] H; try discriminate. rewrite mark_nom. destruct (keys_ok n xs); eauto.
Qed.

Theorem unmark_mark : forall t, unmark (mark t) = t.
Proof.
  fix IH 1. intros [m|fs| |g|n xs|ms]; try reflexivity.
  - rewrite mark_record, unmark_record, map_map. cbn. f_equal.
    induction fs as [|[f u] r Tail]; [reflexivity|]. cbn. rewrite (IH u). f_equal. exact Tail.
  - assert (E : map (fun q => (fst q, unmark (snd q))) (map (fun q => (fst q, mark (snd q))) xs) = xs).
    { rewrite map_map. induction xs as [|[x u] r Tail]; [reflexivity|]. cbn. rewrite (IH u). f_equal. exact Tail. }
    rewrite mark_nom. destruct (keys_ok n xs).
    + rewrite unmark_nom, E. reflexivity.
    + cbn [unmark]. rewrite <- unmark_nom, unmark_nom, E. reflexivity.
  - assert (E : map unmark (map mark ms) = ms).
    { rewrite map_map. induction ms as [|u r Tail]; [reflexivity|]. cbn. rewrite (IH u). f_equal. exact Tail. }
    rewrite mark_union. destruct (union_ok ms) eqn:U.
    + rewrite unmark_union_plain; [rewrite E; reflexivity|].
      unfold union_ok in U. apply andb_prop in U as [Nom _].
      destruct ms as [|m r]; [exact I|]. cbn in Nom. apply andb_prop in Nom as [Nm _].
      destruct (mark_head m Nm) as [n [ys [Em|Em]]]; cbn [map]; rewrite Em; exact I.
    + rewrite unmark_union_unit, E. reflexivity.
Qed.

Lemma mark_inj : forall x y, mark x = mark y -> x = y.
Proof. intros x y E. rewrite <- (unmark_mark x), <- (unmark_mark y), E. reflexivity. Qed.

Lemma map_fixed : forall {A} (f : A -> A) (l : list A), map f l = l -> forall x, In x l -> f x = x.
Proof.
  intros A f l. induction l as [|a r IH]; intros E x I; [contradiction|]. cbn in E. injection E as E1 E2.
  destruct I as [<-|I]; [exact E1|exact (IH E2 x I)].
Qed.

Theorem mark_fixed_formed : forall t, mark t = t -> TyFormed W t.
Proof.
  fix IH 1. intros [m|fs| |g|n xs|ms] E; try exact I.
  - rewrite mark_record in E. injection E as E. apply record_formed. clear -IH E. revert E.
    induction fs as [|[f u] r Tail]; intros E f' u' Iu; [contradiction|]. cbn in E. injection E as Eu Er.
    destruct Iu as [Eq|Iu]; [injection Eq as <- <-; exact (IH u Eu)|exact (Tail Er f' u' Iu)].
  - rewrite mark_nom in E. destruct (keys_ok n xs) eqn:Kk; [|discriminate].
    assert (K : map fst xs = w_reqs W n)
      by (revert Kk; unfold keys_ok; destruct (list_eq_dec _ _ _); [auto|discriminate]).
    injection E as E. apply nom_formed. split; [exact K|]. clear -IH E. revert E.
    induction xs as [|[x u] r Tail]; intros E x' u' Iu; [contradiction|]. cbn in E. injection E as Eu Er.
    destruct Iu as [Eq|Iu]; [injection Eq as <- <-; exact (IH u Eu)|exact (Tail Er x' u' Iu)].
  - rewrite mark_union in E. destruct (union_ok ms) eqn:U.
    + injection E as E.
      assert (Fm : Forall (TyFormed W) ms).
      { clear -IH E. revert E. induction ms as [|u r Tail]; intros E; constructor; cbn in E; injection E as Eu Er;
          [exact (IH u Eu)|exact (Tail Er)]. }
      unfold union_ok in U. apply andb_prop in U as [Nom Nd].
      apply union_formed. split; [|split; [|exact Fm]].
      * apply Forall_forall. intros m Im. apply forallb_forall with (x := m) in Nom; [|exact Im].
        destruct m; try discriminate. eauto.
      * destruct (ListDec.NoDup_dec onat_eq (map head ms)); [assumption|discriminate].
    + injection E as E. apply (f_equal (@List.length ty)) in E. cbn in E. rewrite length_map in E. lia.
Qed.

Definition msubst (s : subst) : subst := map (fun q => (fst q, mark (snd q))) s.

Lemma lookup_msubst : forall g s, lookup_subst g (msubst s) = option_map mark (lookup_subst g s).
Proof.
  intros g s. induction s as [|[k v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb g k); [reflexivity|exact IH].
Qed.

(* On a formed type, marking after σ is applying σ marked. *)
Theorem mark_subst : forall x s, TyFormed W x -> mark (app_subst s x) = app_subst (msubst s) x.
Proof.
  fix IH 1. intros [m|fs| |g|n xs|ms] s Fx; try reflexivity.
  - rewrite !subst_record, mark_record, map_map. cbn. f_equal.
    rewrite record_formed in Fx. clear -IH Fx. induction fs as [|[f u] r Tail]; [reflexivity|]. cbn.
    rewrite (IH u s (Fx f u (or_introl eq_refl))). f_equal. apply Tail. intros f' u' I. exact (Fx f' u' (or_intror I)).
  - cbn [app_subst]. rewrite lookup_msubst. destruct (lookup_subst g s); reflexivity.
  - apply nom_formed in Fx as [K Tm]. rewrite !subst_nom, mark_nom.
    assert (Kk : keys_ok n (PublicTypeRead.smap s xs) = true).
    { unfold keys_ok, PublicTypeRead.smap. rewrite map_map. cbn. destruct (list_eq_dec _ _ _) as [|N]; [reflexivity|].
      exfalso. apply N. exact K. }
    rewrite Kk. f_equal. unfold PublicTypeRead.smap. rewrite map_map. cbn. clear -IH Tm.
    induction xs as [|[x u] r Tail]; [reflexivity|]. cbn.
    rewrite (IH u s (Tm x u (or_introl eq_refl))). f_equal. apply Tail. intros x' u' I. exact (Tm x' u' (or_intror I)).
  - apply union_formed in Fx as [Nom [Nd Fm]]. rewrite !subst_union, mark_union.
    assert (Heads : map head (map (app_subst s) ms) = map head ms).
    { rewrite map_map. apply map_ext_in. intros m I. rewrite Forall_forall in Nom.
      destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. reflexivity. }
    assert (Uk : union_ok (map (app_subst s) ms) = true).
    { unfold union_ok. apply andb_true_intro. split.
      - apply forallb_forall. intros m' I. apply in_map_iff in I as [m [<- I]]. rewrite Forall_forall in Nom.
        destruct (Nom m I) as [n [xs ->]]. rewrite subst_nom. reflexivity.
      - rewrite Heads. destruct (ListDec.NoDup_dec onat_eq (map head ms)) as [|N]; [reflexivity|contradiction]. }
    rewrite Uk, map_map. f_equal. clear -IH Fm. revert Fm. induction ms as [|u r Tail]; intros Fm; [reflexivity|].
    apply Forall_cons_iff in Fm as [Fu Fr]. cbn. rewrite (IH u s Fu). f_equal. exact (Tail Fr).
Qed.

(* A type σ fixes, σ marked fixes. *)
Theorem fixed_msubst : forall x s, app_subst s x = x -> app_subst (msubst s) x = x.
Proof.
  fix IH 1. intros [m|fs| |g|n xs|ms] s E; try reflexivity.
  - rewrite subst_record in E |- *. injection E as E. f_equal. clear -IH E. revert E.
    induction fs as [|[f u] r Tail]; intros E; [reflexivity|]. cbn in E |- *. injection E as Eu Er.
    rewrite (IH u s Eu). f_equal. exact (Tail Er).
  - cbn [app_subst] in E |- *. rewrite lookup_msubst. destruct (lookup_subst g s) as [u|]; [|reflexivity].
    subst u. reflexivity.
  - rewrite subst_nom in E |- *. injection E as E. f_equal. unfold PublicTypeRead.smap in E |- *. clear -IH E. revert E.
    induction xs as [|[x u] r Tail]; intros E; [reflexivity|]. cbn in E |- *. injection E as Eu Er.
    rewrite (IH u s Eu). f_equal. exact (Tail Er).
  - rewrite subst_union in E |- *. injection E as E. f_equal. clear -IH E. revert E.
    induction ms as [|u r Tail]; intros E; [reflexivity|]. cbn in E |- *. injection E as Eu Er.
    rewrite (IH u s Eu). f_equal. exact (Tail Er).
Qed.

Definition mark_item (k : item) : item :=
  match k with IMth r m th => IMth (mark r) m th | k => k end.
Definition mark_sig (g : fsig) : fsig := FSig (map mark (sg_params g)) (mark (sg_ret g)).

Lemma mark_item_inj : forall k k', mark_item k = mark_item k' -> k = k'.
Proof.
  intros [] [] E; cbn in E; try discriminate; try exact E.
  injection E as E1 -> ->. rewrite (mark_inj _ _ E1). reflexivity.
Qed.

Lemma sig_mark : forall s g, SigP (TyFormed W) g -> sig_subst (msubst s) g = mark_sig (sig_subst s g).
Proof.
  intros s [ps r] [Fp Fr]. unfold sig_subst, mark_sig. cbn in *. f_equal.
  - rewrite map_map. apply map_ext_in. intros t I. rewrite Forall_forall in Fp. symmetry. exact (mark_subst t s (Fp t I)).
  - symmetry. exact (mark_subst r s Fr).
Qed.

Lemma key_mark : forall s e, EntryP (TyFormed W) e -> entry_key (msubst s) e = option_map mark_item (entry_key s e).
Proof.
  intros s [x|v t a|f g a|r m g a|u fx] Fe; try reflexivity.
  cbn. unfold mkey. rewrite (mark_subst r s (proj1 Fe)). reflexivity.
Qed.

Lemma sig_entry_mark : forall s e, EntryP (TyFormed W) e -> entry_sig (msubst s) e = option_map mark_sig (entry_sig s e).
Proof.
  intros s [x|v t a|f g a|r m g a|u fx] Fe; try reflexivity; cbn.
  - unfold mark_sig. cbn. rewrite (mark_subst t s (proj1 Fe)). reflexivity.
  - rewrite (sig_mark s g (proj1 Fe)). reflexivity.
  - rewrite (sig_mark s g (proj1 (proj2 Fe))). reflexivity.
Qed.

Theorem unifies_mark : forall s es, Forall (EntryP (TyFormed W)) es -> Unifies s es -> Unifies (msubst s) es.
Proof.
  intros s es Fe U e1 e2 k I1 I2 K1 K2. rewrite Forall_forall in Fe.
  rewrite (key_mark s e1 (Fe e1 I1)) in K1. rewrite (key_mark s e2 (Fe e2 I2)) in K2.
  destruct (entry_key s e1) as [k1|] eqn:E1; [|discriminate]. destruct (entry_key s e2) as [k2|] eqn:E2; [|discriminate].
  cbn in K1, K2. injection K1 as K1. injection K2 as K2. rewrite <- K2 in K1. apply mark_item_inj in K1. subst k2.
  rewrite (sig_entry_mark s e1 (Fe e1 I1)), (sig_entry_mark s e2 (Fe e2 I2)), (U e1 e2 k1 I1 I2 E1 E2). reflexivity.
Qed.

(* The merge of formed entries maps each generation to a formed type. *)
Theorem merge_formed_of : PublicFormation.MergeFormed W.
Proof.
  intros s es [U [Id [Mg [_ Fun]]]] Fe g t I.
  pose proof (Fun g t I) as L.
  assert (Fix : app_subst s t = t).
  { pose proof (Id (TAbs g)) as E. cbn [app_subst] in E. rewrite L in E. exact E. }
  pose proof (Mg (msubst s) (unifies_mark s es Fe U) (TAbs g)) as E.
  cbn [app_subst] in E. rewrite L, lookup_msubst, L in E. cbn in E.
  rewrite (fixed_msubst t s Fix) in E. apply mark_fixed_formed. symmetry. exact E.
Qed.

End Merge.
