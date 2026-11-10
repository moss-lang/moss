(* An old most-general merge can only introduce constructors that it reads
   from the telescope. If a structured image had no such occurrence,
   hiding it behind a fresh atom would produce a more general unifier. *)
From Stdlib Require Import String List Arith Bool Lia.
From NM Require Import Audit.
From NM Require PublicMerge PublicTypeRead PublicTypeFacts PublicOrientation
  PublicFormation PublicCallRealize PublicEquation.
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

Lemma subterm_gens : forall u t, TypeSubterm u t -> incl (gens u) (gens t).
Proof.
  intros u t S. induction S; intros g G; [exact G | | |].
  - rewrite PublicFormation.gens_record_flat. apply in_flat_map. exists (f,t). auto.
  - rewrite PublicFormation.gens_nom_flat. apply in_flat_map. exists (x,t). auto.
  - rewrite PublicFormation.gens_union_flat. apply in_flat_map. exists t. auto.
Qed.

Lemma abs_has_no_shape : forall t x, TypeSubterm t (TAbs x) -> Structured t -> False.
Proof.
  intros t x S H. inversion S; subst. exact (H x eq_refl).
Qed.

Lemma inst_subterm_support : forall a t u,
  In t (map snd (read_inst a)) -> TypeSubterm u t -> Structured u -> incl (gens u) (inst_gens a).
Proof.
  induction a as [|[x [v|]] a IH]; intros t u I Sub Su g G; cbn [read_inst inst_gens map fst snd flat_map] in *.
  - contradiction.
  - destruct I as [<- | I].
    + apply in_or_app. left. exact (@subterm_gens u v Sub g G).
    + apply in_or_app. right. exact (IH t u I Sub Su g G).
  - destruct I as [<- | I].
    + exfalso. eapply abs_has_no_shape; eassumption.
    + exact (IH t u I Sub Su g G).
Qed.

Lemma entry_subterm_support : forall e t u,
  In t (entry_types e) -> TypeSubterm u t -> Structured u -> incl (gens u) (entry_gens e).
Proof.
  intros e t u I Sub Su g G. pose proof (@subterm_gens u t Sub g G) as Gt.
  destruct e as [x|v tv a|f sig a|r m sig a|f fx|l r]; cbn [entry_types entry_gens] in I |- *.
  - destruct I as [<-|[]]. exfalso. eapply abs_has_no_shape; eassumption.
  - destruct I as [<-|I]; apply in_or_app.
    + left. exact Gt.
    + right. eapply inst_subterm_support; eassumption.
  - change (In t ((sg_ret sig :: sg_params sig) ++ map snd (read_inst a))) in I.
    apply in_app_or in I as [I|I]; apply in_or_app.
    + left. apply in_flat_map. exists t. auto.
    + right. eapply inst_subterm_support; eassumption.
  - destruct I as [<-|I]; apply in_or_app; [left; exact Gt|right].
    change (In t ((sg_ret sig :: sg_params sig) ++ map snd (read_inst a))) in I.
    apply in_app_or in I as [I|I]; apply in_or_app.
    + left. apply in_flat_map. exists t. auto.
    + right. eapply inst_subterm_support; eassumption.
  - apply in_map_iff in I as [[x q] [<- I]].
    apply in_flat_map. exists (x,q). auto.
  - destruct I as [<-|[<-|[]]]; apply in_or_app; auto.
Qed.

Lemma raw_subterm_support : forall es u,
  In u (raw_subterms es) -> Structured u -> incl (gens u) (flat_map entry_gens es).
Proof.
  intros es u InU Su g G. apply raw_subterms_spec in InU as [e [t [Ie [It S]]]].
  apply in_flat_map. exists e. split; [exact Ie | eapply entry_subterm_support; eassumption].
Qed.

Local Fixpoint type_depth (t : ty) : nat :=
  match t with
  | TRecord fs => S (fold_right Nat.max 0 (map (fun p => type_depth (snd p)) fs))
  | TNom _ args => S (fold_right Nat.max 0 (map (fun p => type_depth (snd p)) args))
  | TUnion ms => S (fold_right Nat.max 0 (map type_depth ms))
  | _ => 0
  end.

Lemma fold_max_bounds : forall ns x, In x ns -> x <= fold_right Nat.max 0 ns.
Proof.
  induction ns as [|h rest IH]; intros x I; [contradiction |].
  destruct I as [<-|I]; cbn.
  - apply Nat.le_max_l.
  - apply Nat.le_trans with (m := fold_right Nat.max 0 rest); [apply IH; exact I | apply Nat.le_max_r].
Qed.

Lemma raw_generation_depth : forall s t x, In x (gens t) ->
  type_depth (app_subst s (TAbs x)) <= type_depth (app_subst s t) /\
  (Structured t -> type_depth (app_subst s (TAbs x)) < type_depth (app_subst s t)).
Proof.
  intros s t. induction t as [m|fs IH| |g|n args IH|ms IH] using PublicFormation.aty_ind;
    intros x I.
  - contradiction.
  - rewrite PublicFormation.gens_record_flat in I. apply in_flat_map in I as [[f t] [InT I]].
    pose proof (proj1 (IH f t InT x I)) as L.
    rewrite PublicTypeRead.subst_record. cbn [type_depth]. rewrite map_map. cbn [snd].
    assert (D : In (type_depth (app_subst s t))
      (map (fun p : string * ty => type_depth (app_subst s (snd p))) fs)).
    { apply in_map_iff. exists (f,t). auto. }
    apply fold_max_bounds in D. split; [|intros _]; lia.
  - contradiction.
  - destruct I as [<- | []]. split; [lia | intros H; exfalso; exact (H g eq_refl)].
  - rewrite PublicFormation.gens_nom_flat in I. apply in_flat_map in I as [[f t] [InT I]].
    pose proof (proj1 (IH f t InT x I)) as L.
    rewrite PublicTypeRead.subst_nom. unfold PublicTypeRead.smap. cbn [type_depth]. rewrite map_map. cbn [snd].
    assert (D : In (type_depth (app_subst s t))
      (map (fun p : nat * ty => type_depth (app_subst s (snd p))) args)).
    { apply in_map_iff. exists (f,t). auto. }
    apply fold_max_bounds in D. split; [|intros _]; lia.
  - rewrite PublicFormation.gens_union_flat in I. apply in_flat_map in I as [t [InT I]].
    pose proof (proj1 (IH t InT x I)) as L.
    rewrite PublicTypeRead.subst_union. cbn [type_depth].
    assert (D : In (type_depth (app_subst s t)) (map type_depth (map (app_subst s) ms))).
    { apply in_map. apply in_map. exact InT. }
    apply fold_max_bounds in D. split; [|intros _]; lia.
Qed.

Local Definition telescope_symbols (es : list entry) : list tsym :=
  nodup Nat.eq_dec (flat_map entry_gens es).

Local Definition image (s : subst) (x : tsym) : ty := app_subst s (TAbs x).

Local Definition type_dec (t u : ty) : {t = u} + {t <> u}.
Proof.
  destruct (ty_eqb t u) eqn:E.
  - left. now apply PublicTypeFacts.ty_eqb_spec.
  - right. intros <-. now rewrite PublicTypeFacts.ty_eqb_refl in E.
Defined.

Local Definition images (s : subst) (es : list entry) : list ty :=
  nodup type_dec (map (image s) (telescope_symbols es)).

Local Definition members_of (s : subst) (es : list entry) (q : ty) : list tsym :=
  filter (fun x => ty_eqb (image s x) q) (telescope_symbols es).

Local Definition shape_of (s : subst) (es : list entry) (q : ty) : option ty :=
  if structureb q then
    find (fun u => structureb u && ty_eqb (app_subst s u) q) (raw_subterms es)
  else None.

Local Definition class_of (s : subst) (es : list entry) (q : ty) : mclass :=
  MClass (members_of s es q) (shape_of s es q).

Local Definition present (s : subst) (es : list entry) : merge :=
  map (class_of s es) (images s es).

Lemma class_members : forall s es q x,
  In x (mc_members (class_of s es q)) <->
  In x (flat_map entry_gens es) /\ image s x = q.
Proof.
  intros. cbn [class_of mc_members]. unfold members_of, telescope_symbols.
  now rewrite filter_In, nodup_In, PublicTypeFacts.ty_eqb_spec.
Qed.

Lemma class_shape : forall s es q t,
  mc_shape (class_of s es q) = Some t ->
  In t (raw_subterms es) /\ Structured t /\ app_subst s t = q.
Proof.
  intros s es q t E. cbn [class_of mc_shape] in E. unfold shape_of in E.
  destruct (structureb q); [|discriminate].
  apply find_some in E as [I E]. apply andb_true_iff in E as [S Eq].
  rewrite structureb_spec in S. rewrite PublicTypeFacts.ty_eqb_spec in Eq. auto.
Qed.

Lemma class_shape_exists : forall s es q, ReadsShape s es q -> Structured q ->
  exists t, mc_shape (class_of s es q) = Some t.
Proof.
  intros s es q (e & t & u & Ie & It & S & Su & Eq) Sq.
  cbn [class_of mc_shape]. unfold shape_of. apply structureb_spec in Sq. rewrite Sq.
  destruct (find (fun v => structureb v && ty_eqb (app_subst s v) q) (raw_subterms es))
    as [v|] eqn:F; [eauto |].
  assert (I : In u (raw_subterms es)) by (apply raw_subterms_spec; exists e,t; auto).
  apply find_none with (x := u) in F; [|exact I].
  apply structureb_spec in Su. rewrite Su, Eq, PublicTypeFacts.ty_eqb_refl in F. discriminate.
Qed.

Lemma image_class_present : forall s es x,
  In x (flat_map entry_gens es) ->
  In (class_of s es (image s x)) (present s es).
Proof.
  intros s es x I. unfold present. apply in_map. unfold images. apply nodup_In, in_map.
  unfold telescope_symbols. now apply nodup_In.
Qed.

Lemma present_class_image : forall s es c,
  In c (present s es) -> exists x, In x (flat_map entry_gens es) /\ c = class_of s es (image s x).
Proof.
  intros s es c I. unfold present in I. apply in_map_iff in I as [q [<- I]].
  unfold images in I. apply nodup_In, in_map_iff in I. destruct I as [x [<- I]].
  exists x. unfold telescope_symbols in I. apply nodup_In in I. auto.
Qed.

Lemma nodup_disjoint_append : forall A (xs ys : list A),
  NoDup xs -> NoDup ys -> (forall x, In x xs -> ~ In x ys) -> NoDup (xs ++ ys).
Proof.
  intros A xs ys ND Y. induction ND as [|x xs X ND IH]; cbn; intros D; [exact Y|].
  constructor.
  - intro I. apply in_app_or in I as [I|I]; [contradiction |].
    eapply D; [left; reflexivity | exact I].
  - apply IH. intros z I. apply D. right. exact I.
Qed.

Lemma present_disjoint : forall s es, NoDup (flat_map mc_members (present s es)).
Proof.
  intros s es. unfold present.
  assert (N : NoDup (images s es)) by (unfold images; apply NoDup_nodup).
  induction N as [|q qs New ND IH]; cbn; [constructor |].
  apply nodup_disjoint_append; [|exact IH |].
  - unfold class_of, members_of, telescope_symbols. cbn [mc_members].
    apply NoDup_filter, NoDup_nodup.
  - intros x I J. apply in_flat_map in J as [c [InC J]].
    apply in_map_iff in InC as [q' [<- InQ]].
    apply class_members in I, J. destruct I as [_ L], J as [_ R].
    apply New. congruence.
Qed.

Lemma present_solved : forall s es, Solved (present s es).
Proof.
  intros s es. split; [apply present_disjoint |].
  split.
  - intros c t C Shape. apply present_class_image in C as [z [_ ->]].
    exact (proj1 (proj2 (@class_shape s es (image s z) t Shape))).
  - exists (fun x => type_depth (image s x)). split.
    + intros c x y C X Y. apply present_class_image in C as [z [_ ->]].
      apply class_members in X, Y. rewrite (proj2 X), (proj2 Y). reflexivity.
    + intros c t x y C Shape X Y. apply present_class_image in C as [z [_ ->]].
      apply class_members in X as [_ X]. apply class_shape in Shape as [_ [S E]].
      rewrite X, <- E. exact (proj2 (@raw_generation_depth s t y Y) S).
Qed.

Lemma present_shapes_read : forall s es, ShapesRead es (present s es).
Proof.
  intros s es c t C Shape. apply present_class_image in C as [z [_ ->]].
  apply class_shape in Shape. apply raw_subterms_spec. exact (proj1 Shape).
Qed.

Lemma present_support : forall s es, incl (merge_symbols (present s es)) (flat_map entry_gens es).
Proof.
  intros s es x I. unfold merge_symbols in I.
  apply in_flat_map in I as [c [C I]]. apply present_class_image in C as [z [_ ->]].
  apply in_app_or in I as [Member|Shape]; [apply class_members in Member; tauto|].
  destruct (mc_shape (class_of s es (image s z))) as [t|] eqn:Sh; [|contradiction].
  apply class_shape in Sh as [Raw [S _]]. exact (@raw_subterm_support es t Raw S x Shape).
Qed.

Lemma present_old_section : forall s es, MostGeneralMerge s es -> SectionOf (present s es) s.
Proof.
  intros s es MGM. pose proof MGM as Details.
  destruct Details as [_ [Idempotent [_ [Support _]]]].
  split.
  - assert (Read : forall n x, type_depth (image s x) < n ->
      MergeEq (present s es) (TAbs x) (image s x)).
    { induction n as [|n IH]; intros x L; [lia |].
      destruct (in_dec Nat.eq_dec x (flat_map entry_gens es)) as [X|X].
      - set (c := class_of s es (image s x)).
        assert (C : In c (present s es)) by (apply image_class_present; exact X).
        assert (Member : In x (mc_members c)) by (apply class_members; auto).
        destruct (mc_shape c) as [t|] eqn:Shape.
        + eapply METrans; [eapply MEShape; eassumption |].
          apply class_shape in Shape as [_ [St E]]. rewrite <- E.
          rewrite <- (PublicTypeRead.app_subst_nil t) at 1. apply MECong.
          intros y Y. rewrite PublicTypeRead.app_subst_nil.
          apply IH. pose proof (proj2 (@raw_generation_depth s t y Y) St) as Bound.
          unfold image in L |- *. rewrite E in Bound. unfold image in Bound. lia.
        + assert (Abs : exists h, image s x = TAbs h).
          { destruct (structureb (image s x)) eqn:S.
            - apply structureb_spec in S.
              destruct (@class_shape_exists s es (image s x)
                (@mgm_structured_image_reads_shape s es x (image s x) MGM eq_refl S) S) as [t T].
              unfold c in Shape. congruence.
            - destruct (image s x); cbn [structureb] in S; try discriminate; eauto. }
          destruct Abs as [h E].
          assert (H : In h (flat_map entry_gens es)).
          { assert (I : In h (gens (image s x))) by (rewrite E; left; reflexivity).
            unfold image in I. apply PublicCallRealize.gens_subst in I.
            destruct I as (y & Y & [[_ ->] | (v & Lookup & InV)]).
            - destruct Y as [<-|[]]. exact X.
            - apply PublicOrientation.lookup_subst_pair in Lookup.
              exact (proj2 (Support y v Lookup) h InV). }
          assert (Id : image s h = image s x).
          { unfold image in E |- *. rewrite <- E. apply Idempotent. }
          rewrite E. eapply MEClass; [exact C | exact Member |].
          apply class_members. auto.
      - unfold image. cbn [app_subst].
        destruct (lookup_subst x s) as [v|] eqn:Lookup; [|apply MERefl].
        apply PublicOrientation.lookup_subst_pair in Lookup.
        exfalso. exact (X (proj1 (Support x v Lookup))). }
    intro x. apply (Read (S (type_depth (image s x)))). lia.
  - apply PublicMerge.merge_eq_sound. split.
    + intros c x y C X Y. apply present_class_image in C as [z [_ ->]].
      apply class_members in X as [_ X], Y as [_ Y].
      unfold image in X, Y. congruence.
    + intros c x t C X Sh. apply present_class_image in C as [z [_ ->]].
      apply class_members in X as [_ X]. apply class_shape in Sh as [_ [_ Sh]].
      unfold image in X, Sh. congruence.
Qed.

Lemma old_unifies_section : forall s es M, SectionOf M s -> Unifies s es -> UnifiesMerge M es.
Proof.
  intros s es M Section [Collision Equations]. split.
  - intros e e' k k' E E' K K' Eq.
    apply (PublicMerge.section_key_eq Section) in Eq.
    rewrite (@PublicMerge.raw_key_identity e k K), (@PublicMerge.raw_key_identity e' k' K') in Eq.
    assert (L : entry_key s e = Some (app_subst_item s k))
      by (rewrite PublicMerge.entry_key_raw, K; reflexivity).
    assert (R : entry_key s e' = Some (app_subst_item s k))
      by (rewrite PublicMerge.entry_key_raw, K', Eq; reflexivity).
    destruct (Collision e e' _ E E' L R) as [Sig Inst]. split.
    + apply PublicMerge.option_eq_map with (f := sig_subst s);
        [apply PublicMerge.section_sig_eq; exact Section |].
      rewrite <- !PublicMerge.entry_sig_raw. exact Sig.
    + apply PublicMerge.option_eq_map with (f := map (fun q => (fst q, app_subst s (snd q))));
        [apply PublicMerge.section_inst_eq; exact Section |].
      rewrite <- !PublicMerge.entry_inst_raw. exact Inst.
  - intros l r E. apply (PublicMerge.merge_section_kernel Section). now apply Equations.
Qed.

(* An identity substitution already has the empty presentation as a
   section, so it needs no scan of the telescope. *)
Definition present_merge (s : subst) (es : list entry) : merge :=
  match s with [] => [] | _ :: _ => present s es end.

Lemma empty_merge_section : SectionOf [] [].
Proof.
  split.
  - intros x. apply MERefl.
  - apply PublicMerge.merge_eq_sound. split; intros c; contradiction.
Qed.

Lemma empty_merge_of : forall es, Unifies [] es -> MergeOf es [].
Proof.
  intros es U. split.
  - split; [constructor |]. split; [intros c t [] |].
    exists (fun _ => 0). split; intros c; contradiction.
  - split; [intros c t [] |]. split; [intros x [] |].
    split.
    + eapply old_unifies_section; [apply empty_merge_section | exact U].
    + intros s _. apply PublicMerge.merge_eq_sound. split; intros c; contradiction.
Qed.

Theorem present_merge_of : forall s es, MostGeneralMerge s es ->
  MergeOf es (present_merge s es) /\ SectionOf (present_merge s es) s.
Proof.
  intros [|binding s] es MGM.
  - split; [apply empty_merge_of; exact (proj1 MGM) | apply empty_merge_section].
  - cbn [present_merge].
    pose proof (present_old_section MGM) as Section.
    destruct MGM as [Unifier [_ [Principal _]]].
    split; [|exact Section].
    split; [apply present_solved |]. split; [apply present_shapes_read |].
    split; [apply present_support |]. split; [eapply old_unifies_section; eassumption |].
    intros r U t u E. apply (PublicMerge.merge_section_kernel Section) in E.
    rewrite <- (Principal r U t), <- (Principal r U u). now rewrite E.
Qed.

Theorem mgm_has_merge : forall s es, MostGeneralMerge s es ->
  exists M, MergeOf es M /\ (forall t u, MergeEq M t u <-> app_subst s t = app_subst s u).
Proof.
  intros s es MGM. destruct (present_merge_of MGM) as [Merge Section].
  exists (present_merge s es). split; [exact Merge | apply PublicMerge.merge_section_kernel; exact Section].
Qed.

Corollary canon_iff_mergeeq : forall C M, SectionOf M (c_sub C) ->
  forall t u, canon C t = canon C u <-> MergeEq M t u.
Proof.
  intros C M Section t u. symmetry. apply PublicMerge.merge_section_kernel. exact Section.
Qed.

Theorem merge_same_swap : forall es es' M,
  PublicEquation.SameUpToSwap es es' -> PublicEquation.SameUpToSwap es' es ->
  MergeOf es M -> MergeOf es' M.
Proof.
  intros es es' M Forward Backward [S [Shapes [Support [UnifiesM Principal]]]].
  split; [exact S |]. split.
  - intros c t C Sh. destruct (Shapes c t C Sh) as (e & u & E & U & Sub).
    destruct (Forward e E) as [E' | (a & b & -> & E')].
    + exists e,u. auto.
    + exists (NEq b a),u. cbn [entry_types In] in U |- *. tauto.
  - split; [intros x I; eapply PublicEquation.gens_same_swap; [exact Forward|apply Support; exact I] |].
    split.
    + destruct (PublicMerge.solved_has_section S) as [s Section].
      eapply old_unifies_section; [exact Section |].
      eapply PublicEquation.unifies_same_swap; [exact Backward |].
      eapply PublicMerge.section_unifies; eassumption.
    + intros r U t u E. apply Principal; [|exact E].
      eapply PublicEquation.unifies_same_swap; eassumption.
Qed.
