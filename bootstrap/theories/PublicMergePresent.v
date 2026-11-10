(* An old most-general merge can only introduce constructors that it reads
   from the telescope. If a structured image had no such occurrence,
   hiding it behind a fresh atom would produce a more general unifier. *)
From Stdlib Require Import String List Arith Bool Lia.
From NM Require Import Audit.
From NM Require PublicMerge PublicTypeRead PublicTypeFacts PublicOrientation
  PublicFormation PublicCallRealize.
Import Audit.Semantics Audit.Semantics.Elaboration ListNotations.
Local Open Scope list_scope.
Set Implicit Arguments.

Definition Structured (t : ty) : Prop := forall x, t <> TAbs x.

Definition ReadsShape (s : subst) (es : list entry) (q : ty) : Prop :=
  exists e t u, In e es /\ In t (entry_types e) /\ TypeSubterm u t /\
    Structured u /\ app_subst s u = q.

Local Fixpoint hide (q : ty) (a : tsym) (t : ty) {struct t} : ty :=
  if ty_eqb t q then TAbs a else
  match t with
  | TRecord fs => TRecord (map (fun p => (fst p, hide q a (snd p))) fs)
  | TNom n args => TNom n (map (fun p => (fst p, hide q a (snd p))) args)
  | TUnion ms => TUnion (map (hide q a) ms)
  | _ => t
  end.

Local Definition hidden_subst (q : ty) (a : tsym) (s : subst) : subst :=
  map (fun p => (fst p, hide q a (snd p))) s.

Local Definition Unseen (s : subst) (q t : ty) : Prop :=
  forall u, TypeSubterm u t -> Structured u -> app_subst s u <> q.

Lemma hide_lookup : forall q a s x,
  lookup_subst x (hidden_subst q a s) =
  option_map (hide q a) (lookup_subst x s).
Proof.
  intros q a s. induction s as [|[g t] s IH]; intro x; [reflexivity|].
  cbn. destruct (Nat.eqb x g); [reflexivity | apply IH].
Qed.

Lemma no_type_match : forall x y, x <> y -> ty_eqb x y = false.
Proof.
  intros x y N. destruct (ty_eqb x y) eqn:E; [|reflexivity].
  apply PublicTypeFacts.ty_eqb_spec in E. contradiction.
Qed.

Lemma unseen_here : forall s q t, Unseen s q t -> Structured t -> ty_eqb (app_subst s t) q = false.
Proof. intros s q t U S. apply no_type_match, U; [constructor | exact S]. Qed.

Lemma hide_read : forall q a s t, Structured q -> Unseen s q t ->
  app_subst (hidden_subst q a s) t = hide q a (app_subst s t).
Proof.
  intros q a s t Sq. induction t as [m|fs IH| |x|n args IH|ms IH]
    using PublicFormation.aty_ind; intro U.
  - pose proof (unseen_here U ltac:(unfold Structured; discriminate)) as E.
    cbn [app_subst] in E. cbn [app_subst hide]. rewrite E. reflexivity.
  - pose proof (unseen_here U ltac:(unfold Structured; discriminate)) as E.
    rewrite PublicTypeRead.subst_record in E.
    rewrite (PublicTypeRead.subst_record s), (PublicTypeRead.subst_record (hidden_subst q a s)).
    cbn [hide]. rewrite E. f_equal.
    rewrite map_map. apply map_ext_in. intros [f t] I. cbn [fst snd]. f_equal.
    apply (IH f t I). intros u Sub Su. apply U; [eapply SubtermRecord; eassumption | exact Su].
  - pose proof (unseen_here U ltac:(unfold Structured; discriminate)) as E.
    cbn [app_subst] in E. cbn [app_subst hide]. rewrite E. reflexivity.
  - cbn [app_subst]. rewrite hide_lookup. destruct (lookup_subst x s); [reflexivity |].
    cbn [option_map hide]. rewrite no_type_match; [reflexivity |].
    intros E. apply (Sq x). symmetry. exact E.
  - pose proof (unseen_here U ltac:(unfold Structured; discriminate)) as E.
    rewrite PublicTypeRead.subst_nom in E.
    rewrite (PublicTypeRead.subst_nom s), (PublicTypeRead.subst_nom (hidden_subst q a s)).
    unfold PublicTypeRead.smap in E |- *.
    cbn [hide]. rewrite E. f_equal. rewrite map_map. apply map_ext_in.
    intros [g t] I. cbn [fst snd]. f_equal.
    apply (IH g t I). intros u Sub Su. apply U; [eapply SubtermNominal; eassumption | exact Su].
  - pose proof (unseen_here U ltac:(unfold Structured; discriminate)) as E.
    rewrite PublicTypeRead.subst_union in E.
    rewrite (PublicTypeRead.subst_union s), (PublicTypeRead.subst_union (hidden_subst q a s)).
    cbn [hide]. rewrite E. f_equal.
    rewrite map_map. apply map_ext_in. intros t I. apply (IH t I).
    intros u Sub Su. apply U; [eapply SubtermUnion; eassumption | exact Su].
Qed.

Lemma hide_restore : forall q a t, ~ In a (gens t) ->
  app_subst [(a, q)] (hide q a t) = t.
Proof.
  intros q a t. induction t as [m|fs IH| |x|n args IH|ms IH]
    using PublicFormation.aty_ind; intro Fresh;
    cbn [hide];
    match goal with
    | |- app_subst _ (if ?b then _ else _) = _ => destruct b eqn:E
    end;
    try solve [apply PublicTypeFacts.ty_eqb_spec in E; rewrite E;
      cbn [app_subst lookup_subst]; now rewrite Nat.eqb_refl].
  - reflexivity.
  - rewrite PublicTypeRead.subst_record. f_equal. rewrite map_map.
    rewrite <- (map_id fs) at 2. apply map_ext_in. intros [f t] I. cbn [fst snd].
    f_equal. apply (IH f t I). intros InT. apply Fresh.
    rewrite PublicFormation.gens_record_flat. apply in_flat_map. exists (f, t). auto.
  - reflexivity.
  - cbn [app_subst lookup_subst]. destruct (Nat.eqb x a) eqn:Eq; [|reflexivity].
    apply Nat.eqb_eq in Eq. subst x. exfalso. apply Fresh. left. reflexivity.
  - rewrite PublicTypeRead.subst_nom. unfold PublicTypeRead.smap. f_equal.
    rewrite map_map. rewrite <- (map_id args) at 2. apply map_ext_in.
    intros [g t] I. cbn [fst snd]. f_equal. apply (IH g t I). intros InT. apply Fresh.
    rewrite PublicFormation.gens_nom_flat. apply in_flat_map. exists (g, t). auto.
  - rewrite PublicTypeRead.subst_union. f_equal. rewrite map_map.
    rewrite <- (map_id ms) at 2. apply map_ext_in. intros t I.
    apply (IH t I). intros InT. apply Fresh.
    rewrite PublicFormation.gens_union_flat. apply in_flat_map. exists t. auto.
Qed.

Lemma hide_entry_key_equal : forall q a s e e' k,
  Structured q ->
  (forall t, In t (entry_types e) -> Unseen s q t) ->
  (forall t, In t (entry_types e') -> Unseen s q t) ->
  (forall x t, In (x, t) s -> ~ In a (gens t)) ->
  ~ In a (entry_gens e) -> ~ In a (entry_gens e') ->
  entry_key (hidden_subst q a s) e = Some k ->
  entry_key (hidden_subst q a s) e' = Some k ->
  entry_key s e = entry_key s e'.
Proof.
  intros q a s e e' k Sq NoE NoE' Range Fresh Fresh' K K'.
  assert (E : entry_key (hidden_subst q a s) e =
              entry_key (hidden_subst q a s) e') by congruence.
  clear K K'.
  destruct e, e'; cbn [entry_key] in E |- *; try discriminate; try congruence.
  injection E as Receivers Methods.
  f_equal. unfold mkey. f_equal; [|assumption].
  assert (Avoid : forall t, ~ In a (gens t) -> ~ In a (gens (app_subst s t))).
  { intros t F I. apply PublicCallRealize.gens_subst in I.
    destruct I as (x & InX & [[_ ->] | (v & L & InV)]); [contradiction |].
    apply PublicOrientation.lookup_subst_pair in L. exact (Range x v L InV). }
  rewrite !hide_read in Receivers by
    first [exact Sq | solve [apply NoE; left; reflexivity] |
      solve [apply NoE'; left; reflexivity]].
  apply (f_equal (app_subst [(a, q)])) in Receivers.
  rewrite !hide_restore in Receivers by (apply Avoid; intro I;
    first [solve [apply Fresh; apply in_or_app; left; exact I] |
           solve [apply Fresh'; apply in_or_app; left; exact I]]).
  exact Receivers.
Qed.

Local Definition map_sig (f : ty -> ty) (g : fsig) : fsig :=
  FSig (map f (sg_params g)) (f (sg_ret g)).

Local Definition map_inst (f : ty -> ty) (a : subst) : subst :=
  map (fun p => (fst p, f (snd p))) a.

Lemma hide_entry_sig : forall q a s e,
  Structured q -> (forall t, In t (entry_types e) -> Unseen s q t) ->
  entry_sig (hidden_subst q a s) e = option_map (map_sig (hide q a)) (entry_sig s e).
Proof.
  intros q a s e Sq NoE. assert (Sig : forall g,
    (forall t, In t (sg_ret g :: sg_params g) -> Unseen s q t) ->
    sig_subst (hidden_subst q a s) g = map_sig (hide q a) (sig_subst s g)).
  { intros [ps t] UnseenG. unfold sig_subst, map_sig. cbn [sg_params sg_ret].
    rewrite map_map, hide_read by (try exact Sq; apply UnseenG; left; reflexivity).
    f_equal. apply map_ext_in. intros u I. apply hide_read; [exact Sq |].
    apply UnseenG. right. exact I. }
  destruct e; cbn [entry_sig option_map]; try reflexivity.
  - cbn [map_sig sg_params sg_ret map]. rewrite hide_read; [reflexivity | exact Sq |].
    apply NoE. left. reflexivity.
  - f_equal. apply Sig. intros t [<-|I]; apply NoE.
    + left. reflexivity.
    + right. apply in_or_app. left. exact I.
  - f_equal. apply Sig. intros t [<-|I]; apply NoE.
    + right. left. reflexivity.
    + right. right. apply in_or_app. left. exact I.
Qed.

Lemma hide_entry_inst : forall q a s e,
  Structured q -> (forall t, In t (entry_types e) -> Unseen s q t) ->
  entry_inst (hidden_subst q a s) e = option_map (map_inst (hide q a)) (entry_inst s e).
Proof.
  intros q a s e Sq NoE. rewrite !PublicMerge.entry_inst_raw.
  destruct (raw_entry_inst e) as [r|] eqn:Raw; [|reflexivity].
  cbn [option_map]. f_equal. unfold map_inst. rewrite map_map. apply map_ext_in.
  intros [g t] I. cbn [fst snd]. f_equal. apply hide_read; [exact Sq |].
  apply NoE. assert (It : In t (map snd r)) by (apply in_map_iff; exists (g,t); auto).
  destruct e; cbn [raw_entry_inst] in Raw; try discriminate; injection Raw as <-;
    cbn [entry_types]; repeat first [progress right | progress (apply in_or_app; right)]; exact It.
Qed.

Lemma hide_unifies : forall q a s es,
  Structured q -> ~ ReadsShape s es q ->
  (forall x t, In (x, t) s -> ~ In a (gens t)) ->
  ~ In a (flat_map entry_gens es) -> Unifies s es ->
  Unifies (hidden_subst q a s) es.
Proof.
  intros q a s es Sq NoShape Range Fresh [Collisions Eqns].
  assert (U : forall e t, In e es -> In t (entry_types e) -> Unseen s q t).
  { intros e t InE InT u Sub Su Eq. apply NoShape. exists e,t,u. auto. }
  assert (F : forall e, In e es -> ~ In a (entry_gens e)).
  { intros e InE InA. apply Fresh, in_flat_map. exists e. auto. }
  split.
  - intros e e' k InE InE' K K'.
    pose proof (@hide_entry_key_equal q a s e e' k Sq (fun t I => U e t InE I)
      (fun t I => U e' t InE' I) Range (F e InE) (F e' InE') K K') as Equal.
    destruct (entry_key s e) as [old|] eqn:Old; [|].
    + symmetry in Equal. destruct (Collisions e e' old InE InE' Old Equal) as [Sigs Insts].
      rewrite (@hide_entry_sig q a s e Sq (fun t I => U e t InE I)).
      rewrite (@hide_entry_sig q a s e' Sq (fun t I => U e' t InE' I)).
      rewrite (@hide_entry_inst q a s e Sq (fun t I => U e t InE I)).
      rewrite (@hide_entry_inst q a s e' Sq (fun t I => U e' t InE' I)).
      now rewrite Sigs, Insts.
    + destruct e; cbn [entry_key] in Old, K; discriminate.
  - intros l r I.
    rewrite (@hide_read q a s l Sq (U (NEq l r) l I (or_introl eq_refl))).
    rewrite (@hide_read q a s r Sq (U (NEq l r) r I (or_intror (or_introl eq_refl)))).
    rewrite (Eqns l r I). reflexivity.
Qed.

Lemma fresh_of_list : forall xs : list tsym, exists a, ~ In a xs.
Proof.
  intro xs. exists (S (fold_right Nat.max 0 xs)).
  assert (Bound : forall x, In x xs -> x <= fold_right Nat.max 0 xs).
  { induction xs as [|h rest IH]; [contradiction |].
    intros x [<-|I]; cbn; [apply Nat.le_max_l |].
    apply Nat.le_trans with (m := fold_right Nat.max 0 rest); [apply IH; exact I | apply Nat.le_max_r]. }
  intro I. specialize (Bound _ I). lia.
Qed.

Fixpoint subterms (t : ty) : list ty :=
  t :: match t with
       | TRecord fs => flat_map (fun p => subterms (snd p)) fs
       | TNom _ args => flat_map (fun p => subterms (snd p)) args
       | TUnion ms => flat_map subterms ms
       | _ => []
       end.

Lemma subterms_spec : forall t u, In u (subterms t) <-> TypeSubterm u t.
Proof.
  intros t u. split.
  - induction t as [m|fs IH| |x|n args IH|ms IH] using PublicFormation.aty_ind;
      cbn [subterms]; intros [<- | I]; try solve [apply SubtermSelf | contradiction].
    + apply in_flat_map in I as [[f t] [J I]]. cbn [snd] in I.
      eapply SubtermRecord; [exact J | apply (IH f t J); exact I].
    + apply in_flat_map in I as [[g t] [J I]]. cbn [snd] in I.
      eapply SubtermNominal; [exact J | apply (IH g t J); exact I].
    + apply in_flat_map in I as [t [J I]].
      eapply SubtermUnion; [exact J | apply (IH t J); exact I].
  - intro S. induction S; cbn [subterms].
    + destruct u; left; reflexivity.
    + right. apply in_flat_map. exists (f,t). auto.
    + right. apply in_flat_map. exists (x,t). auto.
    + right. apply in_flat_map. exists t. auto.
Qed.

Definition raw_subterms (es : list entry) : list ty :=
  flat_map (fun e => flat_map subterms (entry_types e)) es.

Lemma raw_subterms_spec : forall es u, In u (raw_subterms es) <->
  exists e t, In e es /\ In t (entry_types e) /\ TypeSubterm u t.
Proof.
  intros es u. unfold raw_subterms. rewrite in_flat_map. split.
  - intros [e [I J]]. apply in_flat_map in J as [t [K J]].
    exists e,t. repeat split; try assumption. apply subterms_spec. exact J.
  - intros [e [t [I [J K]]]]. exists e. split; [exact I|].
    apply in_flat_map. exists t. split; [exact J|]. apply subterms_spec. exact K.
Qed.

Definition structureb (t : ty) : bool := match t with TAbs _ => false | _ => true end.

Lemma structureb_spec : forall t, structureb t = true <-> Structured t.
Proof.
  intros []; split; cbn [structureb]; unfold Structured; try discriminate;
    try reflexivity; try solve [intros _ x E; discriminate E].
  intros H. exfalso. eapply H. reflexivity.
Qed.

Lemma reads_shape_dec : forall s es q, {ReadsShape s es q} + {~ ReadsShape s es q}.
Proof.
  intros s es q.
  destruct (existsb (fun u => structureb u && ty_eqb (app_subst s u) q) (raw_subterms es))
    eqn:E.
  - left. apply existsb_exists in E as [u [I E]].
    apply raw_subterms_spec in I as [e [t [Ie [It S]]]].
    apply andb_true_iff in E as [Su Eq]. apply structureb_spec in Su.
    apply PublicTypeFacts.ty_eqb_spec in Eq. exists e,t,u. auto.
  - right. intros (e & t & u & Ie & It & S & Su & Eq).
    assert (T : existsb (fun u => structureb u && ty_eqb (app_subst s u) q)
      (raw_subterms es) = true).
    { apply existsb_exists. exists u. split.
      - apply raw_subterms_spec. exists e,t. auto.
      - apply andb_true_iff. split; [apply structureb_spec | apply PublicTypeFacts.ty_eqb_spec]; assumption. }
    congruence.
Qed.

Theorem mgm_structured_image_reads_shape : forall s es x q,
  MostGeneralMerge s es -> app_subst s (TAbs x) = q -> Structured q ->
  ReadsShape s es q.
Proof.
  intros s es x q [Unifier [_ [Principal [Support _]]]] Image Sq.
  destruct (reads_shape_dec s es q) as [Witness | NoShape]; [exact Witness |].
  destruct (fresh_of_list (flat_map entry_gens es)) as [a Fresh].
  assert (Range : forall y t, In (y,t) s -> ~ In a (gens t)).
  { intros y t I InT. apply Fresh. exact (proj2 (Support y t I) a InT). }
  pose proof (@hide_unifies q a s es Sq NoShape Range Fresh Unifier) as U.
  specialize (Principal _ U (TAbs x)). rewrite Image in Principal.
  assert (UnseenAbs : Unseen s q (TAbs x)).
  { intros t Sub St. inversion Sub; subst. exfalso. exact (St x eq_refl). }
  rewrite (@hide_read q a s (TAbs x) Sq UnseenAbs), Image in Principal.
  destruct q; cbn [hide] in Principal; rewrite PublicTypeFacts.ty_eqb_refl in Principal;
    cbn [app_subst] in Principal; try discriminate.
  exfalso. eapply Sq. reflexivity.
Qed.
