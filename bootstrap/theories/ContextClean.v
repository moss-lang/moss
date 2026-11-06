(* Validity carried by context tables while duplicate provisions merge. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound BodyTr Merge CanonTypes SourceClean.
Import ListNotations.

Record ContextClean (e : env) : Prop := {
  cc_types : EnvClean e;
  cc_uf : UfOK (en_uf e);
  cc_vals : forall n t, nget (en_vals e) n = Some t -> ety_ok t = true;
  cc_fns : forall n sg, nget (en_fns e) n = Some sg -> SigOK sg;
  cc_mths : forall r m sg, get_mth e r m = Some sg -> SigOK sg
}.

Lemma context_clean_empty : ContextClean empty_env.
Proof. constructor.
- intros k p [].
- intros g t [].
- intros n t H; discriminate.
- intros n sg H; discriminate.
- intros r m sg H; discriminate.
Qed.

Lemma context_clean_with_uf : forall e uf,
  ContextClean e -> UfOK uf -> ContextClean (with_uf e uf).
Proof.
  intros e uf [T U V F M] U'. constructor; cbn [with_uf en_tn en_uf en_vals en_fns]; auto.
Qed.

Lemma unify_context_clean : forall (Fl : string -> nat) fuel e mi a b why s e' s',
  ContextClean e -> Tri (en_uf e) -> ety_ok a = true -> ety_ok b = true ->
  unify fuel e mi a b why s = Good (e',s') -> ContextClean e'.
Proof.
  intros Fl fuel e mi a b why s e' s' C T Oa Ob H.
  destruct (unify_spec Fl fuel e mi a b why s e' s' T H) as (_ & U & _).
  pose proof (unify_preserves_uf_ok fuel e mi a b why s e' s' (cc_uf _ C) Oa Ob H) as U'.
  rewrite (us_same _ _ U). apply context_clean_with_uf; assumption.
Qed.

Lemma unify_sig_context_clean : forall (Fl : string -> nat) e mi a b nm s e' s',
  ContextClean e -> Tri (en_uf e) -> SigOK a -> SigOK b ->
  unify_sig e mi a b nm s = Good (e',s') -> ContextClean e'.
Proof.
  intros Fl e mi a b nm s e' s' C T Oa Ob H.
  destruct (unify_sig_spec Fl e mi a b nm s e' s' T H) as (_ & U & _).
  pose proof (unify_sig_preserves_uf_ok e mi a b nm s e' s' (cc_uf _ C) Oa Ob H) as U'.
  rewrite (us_same _ _ U). apply context_clean_with_uf; assumption.
Qed.

Lemma context_clean_val : forall e n t,
  ContextClean e -> ety_ok t = true -> ContextClean (set_val e n t).
Proof.
  intros e n t [T U V F M] O. constructor; cbn [set_val en_tn en_uf en_vals en_fns]; auto.
  intros n' t' H. rewrite nget_nset in H. destruct (Nat.eqb n' n); [injection H as <-; exact O|exact (V _ _ H)].
Qed.

Lemma context_clean_fn : forall e n sg,
  ContextClean e -> SigOK sg -> ContextClean (set_fn e n sg).
Proof.
  intros e n sg [T U V F M] O. constructor; cbn [set_fn en_tn en_uf en_vals en_fns]; auto.
  intros n' sg' H. rewrite nget_nset in H. destruct (Nat.eqb n' n); [injection H as <-; exact O|exact (F _ _ H)].
Qed.

Lemma clean_in_aset : forall K V (eqb : K -> K -> bool) k (v : V) l x,
  In x (aset eqb k v l) -> x = (k,v) \/ In x l.
Proof.
  intros K V eqb k v l. induction l as [|[k' v'] l IH]; intros x H; cbn [aset] in H.
  - destruct H as [<-|[]]. auto.
  - destruct (eqb k k'); cbn in H |- *.
    + destruct H as [<-|H]; auto.
    + destruct H as [<-|H]; [right; left; reflexivity|]. destruct (IH _ H); auto.
Qed.

Lemma context_clean_type : forall e n stamp,
  ContextClean e -> ContextClean (set_tn e n (YAbs n) stamp).
Proof.
  intros e n stamp [T U V F M]. constructor; cbn [set_tn en_tn en_uf en_vals en_fns]; auto.
  intros k p Hp. apply clean_in_aset in Hp as [Hp|Hp].
  - injection Hp as -> Ep. subst p. reflexivity.
  - exact (T _ _ Hp).
Qed.

Lemma clean_assoc_aset : forall K V (eqb : K -> K -> bool) k (v : V) l,
  (forall x y, eqb x y = true <-> x = y) ->
  forall k', assoc eqb k' (aset eqb k v l) = if eqb k' k then Some v else assoc eqb k' l.
Proof.
  intros K V eqb k v l Eq k'. induction l as [|[x y] l IH]; cbn [aset assoc].
  - destruct (eqb k' k); reflexivity.
  - destruct (eqb k x) eqn:E.
    + apply Eq in E. subst x. cbn [assoc]. destruct (eqb k' k); reflexivity.
    + cbn [assoc]. destruct (eqb k' x) eqn:E'; [|exact IH].
      apply Eq in E'. subst k'. destruct (eqb x k) eqn:E''; [|reflexivity].
      apply Eq in E''. subst x. rewrite (proj2 (Eq k k) eq_refl) in E. discriminate.
Qed.

Lemma context_clean_method : forall e r m sg,
  ContextClean e -> SigOK sg -> ContextClean (set_mth e r m sg).
Proof.
  intros e r m sg [T U V F M] O. constructor; cbn [set_mth en_tn en_uf en_vals en_fns]; auto.
  intros r' m' sg' H. unfold get_mth in H. cbn [set_mth en_mths] in H.
  rewrite nget_nset in H. destruct (Nat.eqb m' m) eqn:Em.
  - apply Nat.eqb_eq in Em. subst m'.
    rewrite clean_assoc_aset in H by (intros x y; split; [apply ety_eqb_true|intro; subst; apply ety_eqb_refl]).
    destruct (ety_eqb r' r).
    + injection H as <-. exact O.
    + destruct (nget (en_mths e) m) as [l|] eqn:El; cbn [opt_default] in H; [|discriminate].
      exact (M r' m sg' ltac:(unfold get_mth; rewrite El; exact H)).
  - exact (M r' m' sg' H).
Qed.

Lemma context_clean_fun : forall e n stamp,
  ContextClean e -> ContextClean (set_fun e n stamp).
Proof. intros e n stamp [T U V F M]. constructor; assumption. Qed.

Lemma context_clean_app : forall e k a stamp,
  ContextClean e -> ContextClean (set_app e k a stamp).
Proof. intros e k a stamp [T U V F M]. constructor; assumption. Qed.

Lemma setdefault_context_clean : forall e k a s e' s',
  ContextClean e -> setdefault_app e k a s = Good (e',s') -> ContextClean e'.
Proof.
  intros e k a s e' s' C H. unfold setdefault_app in H. destruct (get_app e k).
  - injection H as <- _. exact C.
  - apply bind_inv in H as (stamp & s1 & H1 & H). injection H as <- _.
    apply context_clean_app. exact C.
Qed.

Lemma context_clean_tymap : forall e,
  ContextClean e -> TmClean (tymap e).
Proof. intros e C. apply tmclean_of_forall. apply tymap_ok. exact (cc_types _ C). Qed.

Lemma context_clean_uf_wf : forall e, ContextClean e -> UfWF (en_uf e).
Proof. intros e C. apply uf_ok_wf. exact (cc_uf _ C). Qed.

Lemma context_clean_vals_wf : forall e n t,
  ContextClean e -> nget (en_vals e) n = Some t -> WFty t.
Proof. intros e n t C H. apply ety_ok_wf. exact (cc_vals _ C n t H). Qed.

Definition SigWF (sg : fsig) : Prop :=
  Forall (fun p => WFty (snd p)) (sg_params sg) /\ WFty (sg_ret sg) /\
  match sg_this sg with Some t => WFty t | None => True end.

Lemma sig_ok_wf : forall sg, SigOK sg -> SigWF sg.
Proof.
  intros sg [Ps [R T]]. unfold SigWF. split.
  - rewrite Forall_forall in Ps |- *. intros p Hp. apply ety_ok_wf.
    exact (Ps _ (in_map snd _ _ Hp)).
  - split; [apply ety_ok_wf; exact R|]. destruct (sg_this sg); [apply ety_ok_wf; exact T|exact I].
Qed.

Lemma context_clean_fns_wf : forall e n sg,
  ContextClean e -> nget (en_fns e) n = Some sg -> SigWF sg.
Proof. intros e n sg C H. apply sig_ok_wf. exact (cc_fns _ C n sg H). Qed.

Lemma context_clean_mths_wf : forall e r m sg,
  ContextClean e -> get_mth e r m = Some sg -> SigWF sg.
Proof. intros e r m sg C H. apply sig_ok_wf. exact (cc_mths _ C r m sg H). Qed.

Lemma context_clean_types_wf : forall e k t,
  ContextClean e -> tn_get e k = Some t -> WFty t.
Proof.
  intros e k t C H. unfold tn_get in H. destruct (nassoc k (en_tn e)) as [p|] eqn:E; [|discriminate].
  injection H as <-. rewrite (cc_types _ C k p (Merge.nassoc_in _ _ _ _ E)). reflexivity.
Qed.

Lemma context_clean_same : forall e e',
  en_tn e'=en_tn e -> en_uf e'=en_uf e -> en_vals e'=en_vals e ->
  en_fns e'=en_fns e -> en_mths e'=en_mths e -> ContextClean e -> ContextClean e'.
Proof.
  intros e e' T U V F M [Ct Cu Cv Cf Cm]. constructor.
  - unfold EnvClean. rewrite T. exact Ct.
  - rewrite U. exact Cu.
  - rewrite V. exact Cv.
  - rewrite F. exact Cf.
  - unfold get_mth in *. rewrite M. exact Cm.
Qed.
