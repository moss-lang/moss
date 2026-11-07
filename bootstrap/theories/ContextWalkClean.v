(* Raw type validity is preserved while assumptions and their contexts merge. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy Merge CanonTypes SourceClean ContextClean MethodClean.
Import ListNotations.
Local Open Scope list_scope.

Lemma setdefault_clean_tri : forall e k a s e' s',
  ContextClean e -> Tri (en_uf e) -> setdefault_app e k a s = Good (e',s') ->
  ContextClean e' /\ Tri (en_uf e').
Proof.
  intros e k a s e' s' C T H. split; [eapply setdefault_context_clean; eauto|].
  unfold setdefault_app in H. destruct (get_app e k).
  - injection H as <- _. exact T.
  - apply bind_inv in H as (stamp & s1 & H1 & H). injection H as <- _. exact T.
Qed.

Lemma cw_ok_unify_sig : forall e mi a b nm, ok (unify_sig e mi a b nm).
Proof. intros; unfold unify_sig; okt. Qed.
#[local] Hint Resolve cw_ok_unify_sig : okdb.
Lemma cw_ok_add_method_item : forall e mi r n subst needs, ok (add_method_item e mi r n subst needs).
Proof. intros; unfold add_method_item; okt. Qed.
#[local] Hint Resolve cw_ok_add_method_item : okdb.

Lemma unify_clean_tri : forall e mi old y why s e' s',
  ContextClean e -> Tri (en_uf e) -> ety_ok old = true -> ety_ok y = true ->
  unify 200 e mi old y why s = Good (e',s') -> ContextClean e' /\ Tri (en_uf e').
Proof.
  intros e mi old y why s e' s' C T O Y H. split.
  - exact (unify_context_clean (fun _ => 0) 200 e mi old y why s e' s' C T O Y H).
  - exact (us_tri _ _ (proj1 (proj2 (unify_spec (fun _ => 0) 200 e mi old y why s e' s' T H)))).
Qed.
Lemma unify_sig_clean_tri : forall e mi old sg why s e' s',
  ContextClean e -> Tri (en_uf e) -> SigOK old -> SigOK sg ->
  unify_sig e mi old sg why s = Good (e',s') -> ContextClean e' /\ Tri (en_uf e').
Proof.
  intros e mi old sg why s e' s' C T O Y H. split.
  - exact (unify_sig_context_clean (fun _ => 0) e mi old sg why s e' s' C T O Y H).
  - exact (us_tri _ _ (proj1 (proj2 (unify_sig_spec (fun _ => 0) e mi old sg why s e' s' T H)))).
Qed.

Section Walk.
Variable w : Elab.world. Context {P : xprogram}.
Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.

Ltac impossible H := exfalso; solve [repeat (cbn beta iota zeta in H;
  first [exact (err_not_good _ _ _ _ _ _ H) | discriminate H |
         apply bind_inv in H as (? & ? & ? & H)])].

(* Each extracted prefix preserves the state invariant; its remaining suffix
   supplies the eventual declaration cache reading needed by source typing. *)
Ltac cstep H Hi Hw L a si E Ii Wi Li :=
  match type of H with mbind ?m ?k ?s = Good (?v, ?sf) =>
    let O := fresh "O" in assert (O : ok m) by okt;
    apply bind_inv in H as (a & si & E & H);
    let F := fresh "F" in destruct (O _ _ _ Hi E) as [Ii F];
    pose proof (fr_w' w _ _ F Hw) as Wi;
    let Q := fresh "Q" in assert (Q : ok (k a)) by okt;
    let It := fresh "It" in let Ft := fresh "Ft" in destruct (Q _ _ _ Ii H) as [It Ft];
    pose proof (later_back w P R Fl _ _ Ft L) as Li;
    clear O Q It Ft
  end.

Lemma method_context_clean : forall e mi recv n subst needs s e' needs' s',
  add_method_item e mi recv n subst needs s = Good ((e',needs'),s') ->
  Inv s -> st_w s = w -> In mi (w_order w) ->
  nget (w_syms w) (gs_id recv) = Some recv -> TmClean subst ->
  ContextClean e -> Tri (en_uf e) -> Later w P R Fl s' ->
  ContextClean e' /\ Tri (en_uf e').
Proof.
  intros e mi recv n subst needs s e' needs' s' H Hi Hw Hmi Hr Ts C T L.
  unfold add_method_item in H.
  apply bind_inv in H as (mo & s1 & E1 & H).
  destruct mo as [m|]; [|impossible H].
  destruct (find_method_decl_loaded b w R Fl mi recv n s m s1 Hload Hmi Hr Hi Hw E1) as [-> Hm].
  assert (Tm : TmClean (subst ++ tymap e)) by (apply tmclean_app; [exact Ts|exact (context_clean_tymap _ C)]).
  cstep H Hi Hw L this s1 Ethis I1 W1 L1.
  assert (Othis : ety_ok this = true).
  { destruct (nassoc (gs_id recv) (subst ++ tymap e)) eqn:Nt.
    - injection Ethis as <- _. exact (Tm _ _ Nt).
    - exact (proj2 (proj2 (source_clean w R Fl b Hload 200)) mi recv (subst ++ tymap e) e s this s1 Ethis Hi Hw Hr Tm (cc_types _ C) L1). }
  cstep H I1 W1 L sg s2 Esg I2 W2 L2.
  pose proof (fn_sig_clean w R Fl b Hload m (subst ++ tymap e) e (Some this) s1 sg s2 Esg I1 W1 Hm Tm (cc_types _ C) Othis L2) as Osg.
  cstep H I2 W2 L k s3 Ek I3 W3 L3.
  cstep H I3 W3 L ec s4 Eec I4 W4 L4.
  assert (Cc : ContextClean ec /\ Tri (en_uf ec)).
  { destruct k as [| |r mid|]; try (injection Eec as <- _; split; assumption).
    destruct (get_mth e r mid) as [old|] eqn:Old.
    - split.
      + eapply (unify_sig_context_clean Fl); [exact C|exact T|exact (cc_mths _ C _ _ _ Old)|exact Osg|exact Eec].
      + exact (us_tri _ _ (proj1 (proj2 (unify_sig_spec Fl e mi old sg (gs_name m) s3 ec s4 T Eec)))).
    - injection Eec as <- _. split; [apply context_clean_method; assumption|exact T]. }
  cstep H I4 W4 L a s5 Ea I5 W5 L5.
  apply bind_inv in H as (ee & s6 & Eee & H). injection H as <- <- <-.
  exact (setdefault_clean_tri ec k a s5 ee s6 (proj1 Cc) (proj2 Cc) Eee).
Qed.
Definition SymWalkClean f : Prop := forall e mi g subst needs s e' needs' s',
  add_symbol_item f e mi g subst needs s = Good ((e',needs'),s') ->
  Inv s -> st_w s = w -> In mi (w_order w) ->
  nget (w_syms w) (gs_id g) = Some g -> TmClean subst ->
  ContextClean e -> Tri (en_uf e) -> Later w P R Fl s' ->
  ContextClean e' /\ Tri (en_uf e').
Definition SpecWalkClean f : Prop := forall e mi sp subst needs s e' needs' s',
  add_spec f e mi sp subst needs s = Good ((e',needs'),s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmClean subst ->
  ContextClean e -> Tri (en_uf e) -> Later w P R Fl s' ->
  ContextClean e' /\ Tri (en_uf e').

Lemma sym_walk_clean_step : forall f, SpecWalkClean f -> SymWalkClean (S f).
Proof.
  intros f IH e mi g subst needs s e' needs' s' H Hi Hw Hmi Hg Ts C T L.
  cbn [add_symbol_item] in H.
  cstep H Hi Hw L u s1 Etotal I1 W1 L1.
  cbn beta zeta in H.
  assert (Tm : TmClean (subst ++ tymap e)) by (apply tmclean_app; [exact Ts|exact (context_clean_tymap _ C)]).
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hgm.
  destruct (gs_kind g).
  - destruct (tn_get e (gs_id g)); [injection H as <- <- <-; split; assumption|].
    apply bind_inv in H as (stamp & s2 & E & H). injection H as <- <- <-.
    split; [apply context_clean_type; exact C|exact T].
  - injection H as <- <- <-. split; assumption.
  - injection H as <- <- <-. split; assumption.
  - injection H as <- <- <-. split; assumption.
  - destruct (gs_decl g) as [| | | | |n t init| | |]; try impossible H.
    destruct init; [injection H as <- <- <-; split; assumption|].
    cstep H I1 W1 L y s2 Ey I2 W2 L2.
    pose proof (ety_of_clean w R Fl b Hload t (gs_mod g) (subst ++ tymap e) e None s1 y s2 Ey I1 W1 Hgm Tm (cc_types _ C) I L2) as Oy.
    cstep H I2 W2 L ec s3 Eec I3 W3 L3.
    assert (Cc : ContextClean ec /\ Tri (en_uf ec)).
    { destruct (nget (en_vals e) (gs_id g)) as [old|] eqn:Old.
      - eapply unify_clean_tri; [exact C|exact T|exact (cc_vals _ C _ _ Old)|exact Oy|exact Eec].
      - injection Eec as <- _. split; [apply context_clean_val; assumption|exact T]. }
    cstep H I3 W3 L a s4 Ea I4 W4 L4.
    apply bind_inv in H as (ee & s5 & Eee & H). injection H as <- <- <-.
    exact (setdefault_clean_tri ec _ a s4 ee s5 (proj1 Cc) (proj2 Cc) Eee).
  - cstep H I1 W1 L sg s2 Esg I2 W2 L2.
    pose proof (fn_sig_clean w R Fl b Hload g (subst ++ tymap e) e None s1 sg s2 Esg I1 W1 Hg Tm (cc_types _ C) I L2) as Osg.
    cstep H I2 W2 L ec s3 Eec I3 W3 L3.
    assert (Cc : ContextClean ec /\ Tri (en_uf ec)).
    { destruct (nget (en_fns e) (gs_id g)) as [old|] eqn:Old.
      - eapply unify_sig_clean_tri; [exact C|exact T|exact (cc_fns _ C _ _ Old)|exact Osg|exact Eec].
      - injection Eec as <- _. split; [apply context_clean_fn; assumption|exact T]. }
    cstep H I3 W3 L a s4 Ea I4 W4 L4.
    cstep H I4 W4 L mem s5 Emem I5 W5 L5.
    cstep H I5 W5 L ms s6 Ems I6 W6 L6.
    cbn beta zeta in H.
    apply bind_inv in H as (ee & s7 & Eee & H). injection H as <- <- <-.
    eapply setdefault_clean_tri; [exact (proj1 Cc)|exact (proj2 Cc)|exact Eee].
  - destruct (gs_decl g) as [| | | | | | |n its|]; try impossible H.
    assert (Gen : forall its0 acc s0 res s3,
      mfold (fun acc sp => add_spec f (fst acc) (gs_mod g) sp subst (snd acc)) acc its0 s0 = Good (res,s3) ->
      Inv s0 -> st_w s0 = w -> ContextClean (fst acc) -> Tri (en_uf (fst acc)) ->
      Later w P R Fl s3 -> ContextClean (fst res) /\ Tri (en_uf (fst res))).
    { induction its0 as [|sp rest IR]; intros [ce ns] s0 [re nr] s3 Hf Ii Wi Ci Ti Li.
      - injection Hf as <- <- <-. split; assumption.
      - cbn [mfold] in Hf.
        cstep Hf Ii Wi Li a si Ea Ia Wa La.
        destruct a as [ae an]. cbn in *.
        destruct (IH ce (gs_mod g) sp subst ns s0 ae an si Ea Ii Wi Hgm Ts Ci Ti La) as [Ca Ta].
        exact (IR (ae,an) si (re,nr) s3 Hf Ia Wa Ca Ta Li). }
    exact (Gen its (e,needs) s1 (e',needs') s' H I1 W1 C T L).
  - impossible H.
  - destruct (gs_decl g) as [| | | | | | | |n ps args res body]; try impossible H.
    destruct body; [impossible H|].
    cstep H I1 W1 L stamp s2 Estamp I2 W2 L2.
    cstep H I2 W2 L xs s3 Exs I3 W3 L3.
    cstep H I3 W3 L u0 s4 Eu0 I4 W4 L4.
    cbn beta zeta in H.
    apply bind_inv in H as (ee & s5 & Eee & H). injection H as <- <- <-.
    eapply setdefault_clean_tri; [apply context_clean_fun; exact C|exact T|exact Eee].
Qed.

Lemma spec_walk_clean_step : forall f, SymWalkClean f -> SpecWalkClean (S f).
Proof.
  intros f IH e mi [p d a] outer needs s e' needs' s' H Hi Hw Hmi Ts C T L.
  cbn [add_spec] in H.
  cstep H Hi Hw L subst0 s1 Esub I1 W1 L1.
  assert (Ts0 : TmClean subst0).
  { destruct a as [bs|].
    - apply bind_inv in Esub as (ys & sy & Eys & Esub). injection Esub as <- <-.
      apply tmclean_app; [apply tmclean_of_forall|exact Ts].
      exact (proj1 (proj2 (source_clean w R Fl b Hload 200)) e mi bs outer s ys sy Eys Hi Hw Hmi Ts (cc_types _ C) L1).
    - injection Esub as <- _. exact Ts. }
  cbn beta zeta in H.
  destruct d as [dn|], p as [pp|]; try impossible H.
  - apply bind_inv in H as (r & s2 & Er & H).
    destruct (rts_spec w R Fl b Hload mi pp s1 r s2 I1 W1 Hmi Er) as (-> & _ & Hr & _).
    exact (method_context_clean e mi r dn subst0 needs s1 e' needs' s' H I1 W1 Hmi Hr Ts0 C T L).
  - apply bind_inv in H as (tg & s2 & Er & H).
    destruct (rpath_rel w R Fl b Hload mi pp s1 tg s2 W1 Hmi Er) as [-> Hres].
    destruct tg as [[x|j]|]; try impossible H.
    apply bind_inv in H as (g & s3 & Eg & H).
    destruct (sym_loader w R Fl b Hload _ _ x s1 g s3 I1 W1 Hres Eg) as [-> Hg].
    pose proof (load_world_ids b w Hload x g Hg) as Eid. rewrite <- Eid in Hg.
    cstep H I1 W1 L u s4 Eu I4 W4 L4.
    destruct (is_some a && match gs_kind g, gs_decl g with
              | KCtx, _ | KFn, _ | KVal, ADVal _ _ None => false | _, _ => true end);
      [impossible H|].
    exact (IH e mi g subst0 needs s4 e' needs' s' H I4 W4 Hmi Hg Ts0 C T L).
Qed.

Theorem context_walk_clean : forall f, SymWalkClean f /\ SpecWalkClean f.
Proof.
  induction f as [|f [IS IT]].
  - split; intros until 1; discriminate.
  - exact (conj (sym_walk_clean_step f IT) (spec_walk_clean_step f IS)).
Qed.
End Walk.
