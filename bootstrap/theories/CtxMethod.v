(* Method assumptions: table update and source reading. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTy Merge CanonTypes SourceClean ContextClean ContextWalkClean CtxWalk MethodClean MethodRows.
Import ListNotations.
Local Open Scope list_scope.
Module A := ExecutionModel.Elaboration.

Lemma get_mth_set_case : forall e r m sg rq mq,
  get_mth (set_mth e r m sg) rq mq =
  if Nat.eqb mq m && ety_eqb rq r then Some sg else get_mth e rq mq.
Proof.
  intros e r m sg rq mq. unfold get_mth. cbn [set_mth en_mths]. rewrite nget_nset.
  destruct (Nat.eqb mq m) eqn:E; [|reflexivity]. apply Nat.eqb_eq in E. subst mq.
  rewrite clean_assoc_aset by (intros x y; split; [apply ety_eqb_true|intro; subst; apply ety_eqb_refl]).
  cbn [andb]. destruct (ety_eqb rq r); [reflexivity|].
  destruct (nget (en_mths e) m); reflexivity.
Qed.

Section TableStep.
Variable Fl : string -> nat.

Lemma ws_method_common : forall U e e1 tm les needs r m sg a,
  WS Fl U e tm les needs ->
  en_tn e1 = en_tn e -> en_vals e1 = en_vals e -> en_fns e1 = en_fns e -> en_funs e1 = en_funs e ->
  (exists nw, en_uf e1 = en_uf e ++ nw) -> Tri (en_uf e1) -> ContextClean e1 -> sg_this sg = Some r ->
  (forall sb, RespE Fl sb U -> A.Unifies sb (map (tr_ent Fl) (les ++ [LMth r m sg a])) -> RespE Fl sb (en_uf e1)) ->
  (en_uf e1 = U -> forall rq mq gq aq, In (LMth rq mq gq aq) (les ++ [LMth r m sg a]) ->
    ety_ok (canon e1 rq) = true /\ exists g0, get_mth e1 (canon e1 rq) mq = Some g0 /\ SigCanon e1 gq g0) ->
  (forall rq mq g0, get_mth e1 rq mq = Some g0 -> exists r0 a0,
    In (LMth r0 mq g0 a0) (les ++ [LMth r m sg a]) /\
    forall sb, RespE Fl sb (en_uf e1) -> ExecutionModel.app_subst sb (tr Fl r0) = ExecutionModel.app_subst sb (tr Fl rq)) ->
  (en_uf e1 = U -> forall k, nofun k = true -> option_map fst (get_app e1 k) = lfirst e1 k (les ++ [LMth r m sg a])) ->
  MethodRowsUnique e1 -> (en_uf e1 = U -> MethodRowsCanonical e1) ->
  WS Fl U e1 tm (les ++ [LMth r m sg a]) (add_need needs (KyMth (canon e r) m)).
Proof.
  intros U e e1 tm les needs r m sg a W Tn Tv Tf Tu Ex Tr C Th Mg Mo Mi Ap Unique Canonical.
  assert (Hu : en_uf e1 = U -> en_uf e = U /\ en_uf e1 = en_uf e).
  { intro E. exact (uf_squeeze U e e1 (ws_ext _ _ _ _ _ _ W) Ex E). }
  constructor; auto.
  - intro k. unfold tn_get. rewrite Tn. exact (ws_tn _ _ _ _ _ _ W k).
  - rewrite Tn. exact (ws_tnabs _ _ _ _ _ _ W).
  - destruct (ws_ext _ _ _ _ _ _ W) as (nw & E), Ex as (nw1 & E1).
    exists (nw ++ nw1). rewrite E1,E,app_assoc. reflexivity.
  - intros v y av Hin. apply in_app_iff in Hin as [Hin|[Hin|[]]]; [|discriminate].
    destruct (ws_vals _ _ _ _ _ _ W _ _ _ Hin) as (y0 & a0 & G & Hy & Hc).
    exists y0,a0. rewrite Tv. split; [exact G|]. split; [apply in_or_app; left; exact Hy|].
    intro E. destruct (Hu E) as [E0 E01]. rewrite !(canon_same e e1 _ E01). exact (Hc E0).
  - intros v y G. rewrite Tv in G. destruct (ws_vals_in _ _ _ _ _ _ W _ _ G) as (av & H).
    exists av. apply in_or_app. left. exact H.
  - intros f g af Hin. apply in_app_iff in Hin as [Hin|[Hin|[]]]; [|discriminate].
    destruct (ws_fns _ _ _ _ _ _ W _ _ _ Hin) as (Ht & g0 & a0 & G & Hy & Hc).
    split; [exact Ht|]. exists g0,a0. rewrite Tf. split; [exact G|]. split; [apply in_or_app; left; exact Hy|].
    intro E. destruct (Hu E) as [E0 E01]. exact (sigcanon_same e e1 g g0 E01 (Hc E0)).
  - intros f g G. rewrite Tf in G. destruct (ws_fns_in _ _ _ _ _ _ W _ _ G) as (af & H).
    exists af. apply in_or_app. left. exact H.
  - intros E k. rewrite add_need_in, (ws_needs _ _ _ _ _ _ W (proj1 (Hu E)) k). split.
    + intros [(l & Hl & Hk)| ->].
      * exists l. split; [apply in_or_app; left; exact Hl|]. rewrite (lkey_same e e1 _ (proj2 (Hu E))). exact Hk.
      * exists (LMth r m sg a). split; [apply in_or_app; right; left; reflexivity|].
        cbn [lkey]. rewrite (canon_same e e1 _ (proj2 (Hu E))). reflexivity.
    + intros (l & Hl & Hk). apply in_app_iff in Hl as [Hl|[<-|[]]].
      * left. exists l. split; [exact Hl|]. rewrite <- (lkey_same e e1 _ (proj2 (Hu E))). exact Hk.
      * right. cbn [lkey] in Hk. rewrite (canon_same e e1 _ (proj2 (Hu E))) in Hk. injection Hk as <-. reflexivity.
  - apply add_need_nodup. exact (ws_nodup _ _ _ _ _ _ W).
  - intro u. rewrite Tu, (ws_funs _ _ _ _ _ _ W u). split.
    + intros[fx H];exists fx;apply in_or_app;left;exact H.
    + intros[fx H];apply in_app_iff in H as[H|[H|[]]];[exists fx;exact H|discriminate].
  - intros rq mq gq aq Hin. apply in_app_iff in Hin as [Hin|[Hin|[]]].
    + exact (ws_mthis _ _ _ _ _ _ W _ _ _ _ Hin).
    + injection Hin as <- <- <- <-. exact Th.
Qed.
End TableStep.
