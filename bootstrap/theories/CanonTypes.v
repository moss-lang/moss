(* Canonical readings of elaborated types under a triangular merge. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodyTr Merge.
Import ListNotations.
Module A := ExecutionModel.Elaboration.

Lemma canon_uf : forall e e', en_uf e' = en_uf e -> forall t, canon e' t = canon e t.
Proof. intros e e' E t. unfold canon. rewrite E. reflexivity. Qed.

Lemma canon_idem : forall e t, Tri (en_uf e) -> canon e (canon e t) = canon e t.
Proof. intros e t T. rewrite !canon_res. exact (res_idem _ (tri_rk _ T) t). Qed.

Definition UfWF (uf : list (nat * ety)) : Prop := forall g t, In (g, t) uf -> WFty t.

Lemma canon_go_wf : forall uf, UfWF uf -> forall k t, WFty t -> WFty (canon_go k uf t).
Proof.
  intros uf U k. induction k as [|k IHk]; intros t Wt; [exact Wt|]. cbn [canon_go]. revert Wt.
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro Wt; try exact Wt.
  - destruct (nassoc g uf) as [t'|] eqn:E; [|exact Wt].
    destruct (ety_eqb t' (YAbs g)); [exact Wt|]. apply IHk. exact (U g t' (Merge.nassoc_in _ _ _ _ E)).
  - unfold WFty in *. cbn [ety_wf] in *. rewrite forallb_forall in Wt |- *. intros p Hp.
    apply in_map_iff in Hp as (q & <- & Hq). cbn [snd]. rewrite Forall_forall in IH. exact (IH q Hq (Wt q Hq)).
  - unfold WFty in *. destruct ms as [|m0 r]; [discriminate Wt|].
    change (forallb (fun m => match m with YNom _ _ => ety_wf m | _ => false end)
      (map (canon_go (S k) uf) (m0 :: r)) = true).
    cbn [ety_wf] in Wt. rewrite forallb_forall in Wt |- *. intros m Hm.
    apply in_map_iff in Hm as (q & <- & Hq). pose proof (Wt q Hq) as Wq.
    rewrite Forall_forall in IH. destruct q as [| | |n xs| |]; try discriminate Wq.
    exact (IH _ Hq Wq).
  - unfold WFty in *. cbn [ety_wf] in *. rewrite forallb_forall in Wt |- *. intros p Hp.
    apply in_map_iff in Hp as (q & <- & Hq). cbn [snd]. rewrite Forall_forall in IH. exact (IH q Hq (Wt q Hq)).
Qed.

Lemma canon_wf : forall e t, UfWF (en_uf e) -> WFty t -> WFty (canon e t).
Proof. intros e t U W0. exact (canon_go_wf _ U _ t W0). Qed.

Lemma tr_canon : forall Fl e t, Tri (en_uf e) ->
  ExecutionModel.app_subst (suf Fl (en_uf e)) (tr Fl t) = tr Fl (canon e t).
Proof. intros Fl e t T. rewrite (tr_res Fl _ (tri_rk _ T)), canon_res. reflexivity. Qed.

Lemma fits_canon_tr : forall Fl e t x, fits e t x = true ->
  A.fits (tr Fl (canon e t)) (tr Fl (canon e x)).
Proof. intros Fl e t x H. exact (fits_raw_tr Fl _ _ H). Qed.

Lemma opt_fits_canon_tr : forall Fl e t exp, Elab.opt_fits e t exp = true ->
  A.opt_fits (tr Fl (canon e t)) (option_map (fun x => tr Fl (canon e x)) exp).
Proof.
  intros Fl e t [x|] H; cbn; [exact (fits_canon_tr Fl e t x H)|exact I].
Qed.

(* Source nominal heads start above the reserved record head. These
   preservation lemmas supply the corresponding merge invariant. *)
Definition UfOK (uf : list (nat * ety)) : Prop :=
  forall g t, In (g, t) uf -> ety_ok t = true.

Lemma canon_go_ok : forall uf, UfOK uf -> forall k t,
  ety_ok t = true -> ety_ok (canon_go k uf t) = true.
Proof.
  intros uf U k. induction k as [|k IHk]; intros t Wt; [exact Wt|].
  cbn [canon_go]. revert Wt.
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro Wt; try exact Wt.
  - destruct (nassoc g uf) as [t'|] eqn:E; [|exact Wt].
    destruct (ety_eqb t' (YAbs g)); [exact Wt|]. apply IHk.
    exact (U g t' (Merge.nassoc_in _ _ _ _ E)).
  - cbn [ety_ok] in Wt |- *. apply andb_true_iff in Wt as [Hn Wt].
    rewrite Hn. cbn. rewrite forallb_forall in Wt |- *. intros p Hp.
    apply in_map_iff in Hp as (q & <- & Hq). cbn [snd].
    rewrite Forall_forall in IH. exact (IH q Hq (Wt q Hq)).
  - destruct ms as [|m ms]; [discriminate Wt|].
    change (forallb (fun q => match q with YNom _ _ => ety_ok q | _ => false end) (m :: ms) = true) in Wt.
    change (forallb (fun q => match q with YNom _ _ => ety_ok q | _ => false end)
      (map (canon_go (S k) uf) (m :: ms)) = true).
    rewrite forallb_forall in Wt |- *. intros q Hq. apply in_map_iff in Hq as (q0 & <- & Hq0).
    pose proof (Wt q0 Hq0) as Wq. rewrite Forall_forall in IH.
    destruct q0 as [| |g|n xs|qs|fs]; try discriminate Wq.
    exact (IH (YNom n xs) Hq0 Wq).
  - cbn [ety_ok] in Wt |- *. rewrite forallb_forall in Wt |- *.
    intros p Hp. apply in_map_iff in Hp as (q & <- & Hq). cbn [snd].
    rewrite Forall_forall in IH. exact (IH q Hq (Wt q Hq)).
Qed.

Lemma canon_ok : forall e t, UfOK (en_uf e) -> ety_ok t = true -> ety_ok (canon e t) = true.
Proof. intros e t U H. exact (canon_go_ok _ U _ t H). Qed.

Lemma set_uf_ok : forall e g t, UfOK (en_uf e) -> ety_ok t = true ->
  UfOK (en_uf (set_uf e g t)).
Proof.
  intros e g t U H. unfold set_uf. cbn [en_uf].
  unfold UfOK in *. induction (en_uf e) as [|[a y] uf IH]; intros h v Hin; cbn in Hin.
  - destruct Hin as [E|[]]. injection E as <- <-. exact H.
  - destruct (Nat.eqb g a).
    + destruct Hin as [E|Hin]; [injection E as <- <-; exact H|].
      apply (U h v). right. exact Hin.
    + destruct Hin as [E|Hin].
      * injection E as <- <-. apply (U a y). left. reflexivity.
      * apply (IH (fun h v Hin => U h v (or_intror Hin)) h v Hin).
Qed.

Definition PairOK (p : ety * ety) : Prop := ety_ok (fst p) = true /\ ety_ok (snd p) = true.

Lemma combine_ok : forall xs ys,
  Forall (fun t => ety_ok t = true) xs -> Forall (fun t => ety_ok t = true) ys ->
  Forall PairOK (combine xs ys).
Proof.
  intros xs. induction xs as [|x xs IH]; intros [|y ys] Hx Hy; try solve [constructor].
  inversion Hx; inversion Hy; subst. constructor; [unfold PairOK; cbn; split; assumption|]. apply IH; assumption.
Qed.

Lemma mfold_uf_ok : forall (step : env -> ety * ety -> M env),
  (forall e p s e' s', UfOK (en_uf e) -> PairOK p ->
     step e p s = Good (e', s') -> UfOK (en_uf e')) ->
  forall l e s e' s', Forall PairOK l -> UfOK (en_uf e) ->
    mfold step e l s = Good (e', s') -> UfOK (en_uf e').
Proof.
  intros step Hstep l. induction l as [|p l IH]; intros e s e' s' Hl U H; cbn [mfold] in H.
  - injection H as <- _. exact U.
  - inversion Hl; subst. apply bind_inv in H as (e1 & s1 & Hs1 & Hs2).
    eapply IH; [eassumption| |exact Hs2]. eapply Hstep; eassumption.
Qed.

Lemma nominal_args_ok : forall n xs, ety_ok (YNom n xs) = true ->
  Forall (fun t => ety_ok t = true) (map snd xs).
Proof.
  intros n xs H. cbn [ety_ok] in H. apply andb_true_iff in H as [_ H].
  rewrite Forall_map, Forall_forall. rewrite forallb_forall in H. exact H.
Qed.

Lemma union_members_ok : forall xs, ety_ok (YUnion xs) = true ->
  Forall (fun t => ety_ok t = true) xs.
Proof.
  intros [|x xs] H; [discriminate|]. cbn [ety_ok] in H.
  rewrite Forall_forall. rewrite forallb_forall in H. intros t Ht.
  specialize (H t Ht). destruct t; try discriminate H; exact H.
Qed.

Lemma record_fields_ok : forall xs, ety_ok (YRec xs) = true ->
  Forall (fun t => ety_ok t = true) (map snd xs).
Proof.
  intros xs H. cbn [ety_ok] in H. rewrite Forall_map, Forall_forall.
  rewrite forallb_forall in H. exact H.
Qed.

Theorem unify_preserves_uf_ok : forall fuel e mi a b why s e' s',
  UfOK (en_uf e) -> ety_ok a = true -> ety_ok b = true ->
  unify fuel e mi a b why s = Good (e', s') -> UfOK (en_uf e').
Proof.
  induction fuel as [|fuel IH]; intros e mi a b why s e' s' U Oa Ob H; [discriminate|].
  cbn [unify] in H.
  pose proof (canon_ok e a U Oa) as Ca. pose proof (canon_ok e b U Ob) as Cb.
  remember (canon e a) as ca. remember (canon e b) as cb.
  destruct (ety_eqb ca cb); [injection H as <- _; exact U|].
  destruct ca as [| |ga|na xa|xa|xa]; destruct cb as [| |gb|nb xb|xb|xb];
    try solve [Merge.noerr H].
  all: try (match type of H with
    | (if nmem ?g (ety_gens ?t) then _ else _) _ = _ =>
      destruct (nmem g (ety_gens t)); [Merge.noerr H|];
      injection H as <- _; apply set_uf_ok; assumption
    end).
  - destruct (Nat.eqb na nb); [|Merge.noerr H].
    destruct (list_eq_dec Nat.eq_dec (map fst xa) (map fst xb)); [|Merge.noerr H].
    eapply mfold_uf_ok; [| |exact U|exact H].
    + intros env_start [left_ty right_ty] state_start env_end state_end U_start [O_left O_right] Hstep.
      cbn [fst snd] in *.
      exact (IH env_start mi left_ty right_ty why state_start env_end state_end U_start O_left O_right Hstep).
    + apply combine_ok; [exact (nominal_args_ok _ _ Ca)|exact (nominal_args_ok _ _ Cb)].
  - destruct (Nat.eqb (List.length xa) (List.length xb)); [|Merge.noerr H].
    eapply mfold_uf_ok; [| |exact U|exact H].
    + intros env_start [left_ty right_ty] state_start env_end state_end U_start [O_left O_right] Hstep.
      cbn [fst snd] in *.
      exact (IH env_start mi left_ty right_ty why state_start env_end state_end U_start O_left O_right Hstep).
    + apply combine_ok; [exact (union_members_ok _ Ca)|exact (union_members_ok _ Cb)].
  - destruct (list_eq_dec string_dec (map fst xa) (map fst xb)); [|Merge.noerr H].
    eapply mfold_uf_ok; [| |exact U|exact H].
    + intros env_start [left_ty right_ty] state_start env_end state_end U_start [O_left O_right] Hstep.
      cbn [fst snd] in *.
      exact (IH env_start mi left_ty right_ty why state_start env_end state_end U_start O_left O_right Hstep).
    + apply combine_ok; [exact (record_fields_ok _ Ca)|exact (record_fields_ok _ Cb)].
Qed.

Definition SigOK (sg : fsig) : Prop :=
  Forall (fun t => ety_ok t = true) (map snd (sg_params sg)) /\
  ety_ok (sg_ret sg) = true /\
  match sg_this sg with Some t => ety_ok t = true | None => True end.

Lemma unify_sig_preserves_uf_ok : forall e mi a b nm s e' s',
  UfOK (en_uf e) -> SigOK a -> SigOK b ->
  unify_sig e mi a b nm s = Good (e', s') -> UfOK (en_uf e').
Proof.
  intros e mi a b nm s e' s' U [Pa [Ra Ta]] [Pb [Rb Tb]] H.
  unfold unify_sig in H.
  destruct (negb (Nat.eqb _ _)); [Merge.noerr H|].
  apply bind_inv in H as (e1 & s1 & Hp & H).
  assert (U1 : UfOK (en_uf e1)).
  { eapply mfold_uf_ok; [|exact (combine_ok _ _ Pa Pb)|exact U|exact Hp].
    intros env_start [left_ty right_ty] state_start env_end state_end U_start [O_left O_right] Hstep.
    cbn [fst snd] in Hstep.
    exact (unify_preserves_uf_ok 200 env_start mi left_ty right_ty _ state_start env_end state_end U_start O_left O_right Hstep). }
  apply bind_inv in H as (e2 & s2 & Hr & H).
  assert (U2 : UfOK (en_uf e2)).
  { exact (unify_preserves_uf_ok 200 e1 mi (sg_ret a) (sg_ret b) _ s1 e2 s2 U1 Ra Rb Hr). }
  destruct (sg_this a) as [ta|], (sg_this b) as [tb|]; cbn in Ta, Tb.
  - exact (unify_preserves_uf_ok 200 e2 mi ta tb _ s2 e' s' U2 Ta Tb H).
  - injection H as <- _. exact U2.
  - injection H as <- _. exact U2.
  - injection H as <- _. exact U2.
Qed.

Lemma is_never_true_tr : forall Fl t, is_never t = true -> tr Fl t = A.never.
Proof. intros Fl t H; destruct t; try discriminate; reflexivity. Qed.

Lemma join_canon_tr : forall Fl mi e a b exp s t s',
  join mi e a b exp s = Good (t, s') -> UfWF (en_uf e) ->
  WFty a -> WFty b -> (match exp with Some x => WFty x | None => True end) ->
  A.Join (tr Fl (canon e a)) (tr Fl (canon e b))
    (option_map (fun y => tr Fl (canon e y)) exp) (tr Fl (canon e t)) /\ WFty t.
Proof.
  intros Fl mi e a b exp s t s' H U Wa Wb Wx. unfold join in H.
  destruct exp as [x|].
  - injection H as <- <-. split; [constructor|exact Wx].
  - destruct (is_never (canon e a)) eqn:Ea.
    + injection H as <- <-. split; [|exact Wb].
      rewrite (is_never_true_tr Fl _ Ea). constructor.
    + destruct (is_never (canon e b) || ety_eqb (canon e a) (canon e b)) eqn:Eb;
        [|Merge.noerr H].
      injection H as <- <-. split; [|exact Wa]. apply A.JoinSame.
      * apply orb_true_iff in Eb as [Eb|Eb].
        -- left. exact (is_never_true_tr Fl _ Eb).
        -- right. rewrite (ety_eqb_true _ _ Eb). reflexivity.
      * intro E. apply (never_tr Fl _ (canon_wf e a U Wa)) in E.
        rewrite E in Ea. discriminate.
Qed.

Lemma members_ok : forall t, ety_ok t = true ->
  Forall (fun y => ety_ok y = true) (Elab.members t).
Proof.
  intros t H. destruct t as [| |g|n xs|ms|fs]; cbn [Elab.members];
    try solve [constructor | constructor; [exact H|constructor]].
  exact (union_members_ok ms H).
Qed.

Lemma union_result_ok : forall ts out,
  Forall (fun t => ety_ok t = true) ts -> out = flat_map Elab.members ts ->
  Forall (fun t => exists n xs, t = YNom n xs) out ->
  ety_ok (match out with [] => YNever | _ => YUnion out end) = true.
Proof.
  intros ts out H -> N. assert (F : Forall (fun t => ety_ok t = true) (flat_map Elab.members ts)).
  { clear N. induction H; cbn [flat_map]; [constructor|]. apply Forall_app. split; [apply members_ok; assumption|assumption]. }
  destruct (flat_map Elab.members ts); [reflexivity|]. cbn [ety_ok].
  apply forallb_forall. intros t Ht. rewrite Forall_forall in F,N.
  destruct (N t Ht) as (n & xs & ->). exact (F _ Ht).
Qed.

Lemma tymap_ok : forall e,
  (forall k p, In (k,p) (en_tn e) -> fst p = YAbs k) ->
  Forall (fun q => ety_ok (snd q) = true) (tymap e).
Proof.
  intros e H. unfold tymap. apply Forall_forall. intros q Hq.
  apply in_map_iff in Hq as ([k p] & <- & Hp). cbn [fst snd]. rewrite (H k p Hp). reflexivity.
Qed.

Lemma tr_canon_nom : forall Fl e n xs,
  tr Fl (canon e (YNom n xs)) = TNom n (map (fun p => (fst p,tr Fl (canon e (snd p)))) xs).
Proof. intros Fl e n xs. unfold canon. cbn [canon_go tr]. rewrite map_map. reflexivity. Qed.

Lemma tr_canon_rec : forall Fl e fs,
  tr Fl (canon e (YRec fs)) = TNom rec_head (map (fun p => (Fl (fst p),tr Fl (canon e (snd p)))) fs).
Proof. intros Fl e fs. unfold canon. cbn [canon_go tr]. rewrite map_map. reflexivity. Qed.

Lemma uf_ok_wf : forall uf, UfOK uf -> UfWF uf.
Proof. intros uf U k t H. apply ety_ok_wf. exact (U k t H). Qed.
