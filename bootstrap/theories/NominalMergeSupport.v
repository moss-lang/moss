(* Actual unification preserves declaration support and full nominal keys. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound BodyTr Merge NominalSupport WorldSound.
Import ListNotations.
Set Implicit Arguments.

Lemma set_uf_support : forall W e g t, UfSupport W (en_uf e) -> Support W t ->
 UfSupport W (en_uf (set_uf e g t)).
Proof.
 intros W e g t U H; unfold set_uf; cbn [en_uf]; unfold UfSupport in *.
 induction (en_uf e) as [|[a y] uf IH]; intros h v Hin; cbn in Hin.
 - destruct Hin as [E|[]]; injection E as <- <-; exact H.
 - destruct (Nat.eqb g a).
   + destruct Hin as [E|Hin]; [injection E as <- <-; exact H|apply (U h v); right; exact Hin].
   + destruct Hin as [E|Hin].
     * injection E as <- <-; apply (U a y); left; reflexivity.
     * apply (IH (fun h v Hin => U h v (or_intror Hin)) h v Hin).
Qed.
Definition PairSupport W (p : ety * ety) := Support W (fst p) /\ Support W (snd p).
Lemma combine_support : forall W xs ys,
 Forall (Support W) xs -> Forall (Support W) ys -> Forall (PairSupport W) (combine xs ys).
Proof.
 intros W xs; induction xs as [|x xs IH]; intros [|y ys] Hx Hy; try solve [constructor].
 inversion Hx; inversion Hy; subst; constructor; [split; assumption|apply IH; assumption].
Qed.
Lemma mfold_uf_support : forall W (step : env -> ety * ety -> M env),
 (forall e p s e' s', UfSupport W (en_uf e) -> PairSupport W p ->
  step e p s = Good(e',s') -> UfSupport W (en_uf e')) ->
 forall l e s e' s', Forall (PairSupport W) l -> UfSupport W (en_uf e) ->
 mfold step e l s = Good(e',s') -> UfSupport W (en_uf e').
Proof.
 intros W step Hstep l; induction l as [|p l IH]; intros e s e' s' Hl U H; cbn [mfold] in H.
 - injection H as <- _; exact U.
 - inversion Hl; subst; apply bind_inv in H as (e1 & s1 & Exec1 & Exec2).
   eapply IH; [eassumption| |exact Exec2]; eapply Hstep; eassumption.
Qed.
Lemma support_nominal_args : forall W n xs,
 Support W (YNom n xs) -> Forall (Support W) (map snd xs).
Proof. intros W n xs H; inversion H; subst; apply Forall_map; assumption. Qed.
Lemma support_union_members : forall W xs, Support W (YUnion xs) -> Forall (Support W) xs.
Proof. intros W xs H; inversion H; assumption. Qed.
Lemma support_record_fields : forall W xs, Support W (YRec xs) -> Forall (Support W) (map snd xs).
Proof. intros W xs H; inversion H; subst; apply Forall_map; assumption. Qed.

Theorem unify_preserves_uf_support : forall W fuel e mi a b why s e' s',
 UfSupport W (en_uf e) -> Support W a -> Support W b ->
 unify fuel e mi a b why s = Good(e',s') -> UfSupport W (en_uf e').
Proof.
 intros W fuel; induction fuel as [|fuel IH]; intros e mi a b why s e' s' U Oa Ob H; [discriminate|].
 cbn [unify] in H.
 pose proof (canon_support e U Oa) as Ca; pose proof (canon_support e U Ob) as Cb.
 remember (canon e a) as ca; remember (canon e b) as cb.
 destruct (ety_eqb ca cb); [injection H as <- _; exact U|].
 destruct ca as [| |ga|na xa|xa|xa]; destruct cb as [| |gb|nb xb|xb|xb]; try solve [Merge.noerr H].
 all: try (match type of H with
 | (if nmem ?g (ety_gens ?t) then _ else _) _ = _ =>
 destruct (nmem g (ety_gens t)); [Merge.noerr H|]; injection H as <- _; apply set_uf_support; assumption
 end).
 - destruct (Nat.eqb na nb); [|Merge.noerr H].
   destruct (list_eq_dec Nat.eq_dec (map fst xa) (map fst xb)); [|Merge.noerr H].
   eapply mfold_uf_support; [| |exact U|exact H].
   + intros ex [left right] sx ex' sx' Ux [OL OR] Step; cbn [fst snd] in *; exact (IH ex mi left right why sx ex' sx' Ux OL OR Step).
   + apply combine_support; [eapply support_nominal_args; exact Ca|eapply support_nominal_args; exact Cb].
 - destruct (Nat.eqb (List.length xa) (List.length xb)); [|Merge.noerr H].
   eapply mfold_uf_support; [| |exact U|exact H].
   + intros ex [left right] sx ex' sx' Ux [OL OR] Step; cbn [fst snd] in *; exact (IH ex mi left right why sx ex' sx' Ux OL OR Step).
   + apply combine_support; [apply support_union_members; exact Ca|apply support_union_members; exact Cb].
 - destruct (list_eq_dec string_dec (map fst xa) (map fst xb)); [|Merge.noerr H].
   eapply mfold_uf_support; [| |exact U|exact H].
   + intros ex [left right] sx ex' sx' Ux [OL OR] Step; cbn [fst snd] in *; exact (IH ex mi left right why sx ex' sx' Ux OL OR Step).
   + apply combine_support; [apply support_record_fields; exact Ca|apply support_record_fields; exact Cb].
Qed.

Lemma unify_sig_preserves_uf_support : forall W e mi a b nm s e' s',
 UfSupport W (en_uf e) -> SigSupport W a -> SigSupport W b ->
 unify_sig e mi a b nm s = Good(e',s') -> UfSupport W (en_uf e').
Proof.
 intros W e mi a b nm s e' s' U [Pa [Ra Ta]] [Pb [Rb Tb]] H.
 unfold unify_sig in H; destruct (negb (Nat.eqb _ _)); [Merge.noerr H|].
 apply bind_inv in H as (e1 & s1 & Hp & H).
 assert (U1 : UfSupport W (en_uf e1)).
 { eapply mfold_uf_support; [| |exact U|exact Hp].
   - intros ex [left right] sx ex' sx' Ux [OL OR] Step; cbn [fst snd] in Step.
     exact (@unify_preserves_uf_support W 200 ex mi left right _ sx ex' sx' Ux OL OR Step).
   - apply combine_support; apply Forall_map; assumption. }
 apply bind_inv in H as (e2 & s2 & Hr & H).
 pose proof (@unify_preserves_uf_support W 200 e1 mi (sg_ret a) (sg_ret b) _ s1 e2 s2 U1 Ra Rb Hr) as U2.
 destruct (sg_this a) as [ta|], (sg_this b) as [tb|]; cbn in Ta,Tb.
 - exact (@unify_preserves_uf_support W 200 e2 mi ta tb _ s2 e' s' U2 Ta Tb H).
 - injection H as <- _; exact U2.
 - injection H as <- _; exact U2.
 - injection H as <- _; exact U2.
Qed.

Lemma env_support_with_uf : forall W e uf,
 EnvSupport W e -> UfSupport W uf -> EnvSupport W (with_uf e uf).
Proof. intros W e uf [T U V F M L] U'; constructor; cbn [with_uf en_tn en_uf en_vals en_fns en_locals]; auto. Qed.
Lemma unify_env_support : forall W (Fl : string -> nat) fuel e mi a b why s e' s',
 EnvSupport W e -> Tri (en_uf e) -> Support W a -> Support W b ->
 unify fuel e mi a b why s = Good(e',s') -> EnvSupport W e'.
Proof.
 intros W Fl fuel e mi a b why s e' s' E T A B H.
 destruct (unify_spec Fl fuel e mi a b why s e' s' T H) as (_ & Us & _).
 rewrite (us_same _ _ Us); apply env_support_with_uf; auto.
 eapply unify_preserves_uf_support; [exact (es_uf E)|exact A|exact B|exact H].
Qed.
Lemma unify_sig_env_support : forall W (Fl : string -> nat) e mi a b nm s e' s',
 EnvSupport W e -> Tri (en_uf e) -> SigSupport W a -> SigSupport W b ->
 unify_sig e mi a b nm s = Good(e',s') -> EnvSupport W e'.
Proof.
 intros W Fl e mi a b nm s e' s' E T A B H.
 destruct (unify_sig_spec Fl e mi a b nm s e' s' T H) as (_ & Us & _).
 rewrite (us_same _ _ Us); apply env_support_with_uf; auto.
 eapply unify_sig_preserves_uf_support; [exact (es_uf E)|exact A|exact B|exact H].
Qed.

Lemma support_in_aset : forall K V (eqb : K -> K -> bool) k (v : V) l x,
 In x (aset eqb k v l) -> x = (k,v) \/ In x l.
Proof.
 intros K V eqb k v l; induction l as [|[k' v'] l IH]; intros x H; cbn [aset] in H.
 - destruct H as [<-|[]]; auto.
 - destruct (eqb k k'); cbn in H |- *.
   + destruct H as [<-|H]; auto.
   + destruct H as [<-|H]; [right;left;reflexivity|]; destruct (IH _ H); auto.
Qed.
Lemma support_assoc_aset : forall K V (eqb : K -> K -> bool) k (v : V) l,
 (forall x y, eqb x y = true <-> x = y) ->
 forall k', assoc eqb k' (aset eqb k v l) = if eqb k' k then Some v else assoc eqb k' l.
Proof.
 intros K V eqb k v l Eq k'; induction l as [|[x y] l IH]; cbn [aset assoc].
 - destruct (eqb k' k); reflexivity.
 - destruct (eqb k x) eqn:E.
   + apply Eq in E; subst x; cbn [assoc]; destruct (eqb k' k); reflexivity.
   + cbn [assoc]; destruct (eqb k' x) eqn:E'; [|exact IH].
     apply Eq in E'; subst k'; destruct (eqb x k) eqn:E''; [|reflexivity].
     apply Eq in E''; subst x; rewrite (proj2 (Eq k k) eq_refl) in E; discriminate.
Qed.
Lemma env_support_empty : forall W, EnvSupport W empty_env.
Proof.
 intro W; constructor.
 - intros k p [].
 - intros g t [].
 - intros n t H; discriminate.
 - intros n sg H; discriminate.
 - intros r m sg H; discriminate.
 - intros n p H; discriminate.
Qed.
Lemma env_support_type : forall W e n t stamp,
 EnvSupport W e -> Support W t -> EnvSupport W (set_tn e n t stamp).
Proof.
 intros W e n t stamp [T U V F M L] Ot; constructor; cbn [set_tn en_tn en_uf en_vals en_fns en_locals]; auto.
 intros k p Hp; apply support_in_aset in Hp as [E|H]; [injection E as -> Ep; subst; exact Ot|exact (T _ _ H)].
Qed.
Lemma env_support_val : forall W e n t,
 EnvSupport W e -> Support W t -> EnvSupport W (set_val e n t).
Proof.
 intros W e n t [T U V F M L] Ot; constructor; cbn [set_val en_tn en_uf en_vals en_fns en_locals]; auto.
 intros k y Hy; rewrite nget_nset in Hy; destruct (Nat.eqb k n); [injection Hy as <-;exact Ot|exact (V _ _ Hy)].
Qed.
Lemma env_support_fn : forall W e n sg,
 EnvSupport W e -> SigSupport W sg -> EnvSupport W (set_fn e n sg).
Proof.
 intros W e n sg [T U V F M L] O; constructor; cbn [set_fn en_tn en_uf en_vals en_fns en_locals]; auto.
 intros k s H; rewrite nget_nset in H; destruct (Nat.eqb k n); [injection H as <-;exact O|exact (F _ _ H)].
Qed.
Lemma env_support_method : forall W e r m sg,
 EnvSupport W e -> Support W r -> SigSupport W sg -> EnvSupport W (set_mth e r m sg).
Proof.
 intros W e r m sg [T U V F M L] Or Os; constructor; cbn [set_mth en_tn en_uf en_vals en_fns en_locals]; auto.
 intros r' m' sg' H; unfold get_mth in H; cbn [set_mth en_mths] in H.
 rewrite nget_nset in H; destruct (Nat.eqb m' m) eqn:Em.
 - apply Nat.eqb_eq in Em; subst m'.
   rewrite support_assoc_aset in H by (intros x y; split; [apply ety_eqb_true|intro;subst;apply ety_eqb_refl]).
   destruct (ety_eqb r' r) eqn:Er.
   + apply ety_eqb_true in Er; subst r'; injection H as <-; auto.
   + destruct (nget (en_mths e) m) as [l|] eqn:El; cbn [opt_default] in H; [|discriminate].
     exact (M r' m sg' ltac:(unfold get_mth; rewrite El; exact H)).
 - exact (M r' m' sg' H).
Qed.
Lemma env_support_fun : forall W e n stamp, EnvSupport W e -> EnvSupport W (set_fun e n stamp).
Proof. intros W e n stamp [T U V F M L]; constructor; assumption. Qed.
Lemma env_support_local : forall W e n t mu,
 EnvSupport W e -> Support W t -> EnvSupport W (set_local e n t mu).
Proof.
 intros W e n t mu [T U V F M L] Ot; constructor; cbn [set_local en_tn en_uf en_vals en_fns en_locals]; auto.
 intros n' p Hp; unfold sassoc in Hp; rewrite support_assoc_aset in Hp by (intros;apply String.eqb_eq).
 destruct (String.eqb n' n); [injection Hp as <-;exact Ot|exact (L _ _ Hp)].
Qed.
Lemma env_support_locals : forall W e ls,
 EnvSupport W e -> (forall n p, sassoc n ls = Some p -> Support W (fst p)) -> EnvSupport W (with_locals e ls).
Proof. intros W e ls [T U V F M L] L'; constructor; assumption. Qed.


Lemma join_support : forall W mi e a b exp s t s',
 join mi e a b exp s = Good(t,s') -> EnvSupport W e ->
 Support W a -> Support W b ->
 (match exp with Some x => Support W x | None => True end) -> Support W t.
Proof.
 intros W mi e a b exp s t s' H He Ha Hb Hexp.
 unfold join in H; destruct exp as [x|].
 - injection H as <- _; exact Hexp.
 - destruct (is_never (canon e a)); [injection H as <- _; exact Hb|].
   destruct (is_never (canon e b) || ety_eqb (canon e a) (canon e b));
   [injection H as <- _; exact Ha|Merge.noerr H].
Qed.
