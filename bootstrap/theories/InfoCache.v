(* ------------------------------------------------------------------------ *)
(* InfoCache.v — fn_info's cache (st_info) against CtxOf (Track A).         *)
(*                                                                           *)
(* Every entry the cache holds was computed by fn_info: the merged walk of *)
(* the symbol table's entry for its key, run from a state with Inv, whose  *)
(* end state is framed (Fr) by the current one (InfoInv).  Every step of   *)
(* the elaborator keeps InfoInv (okI): only fn_info writes the cache, and  *)
(* no function before it in Elab.v touches it (NI).  So fn_info, whether   *)
(* its answer is cached or not, forms the function's context              *)
(* (fn_info_ctx_any).                                                      *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameCore BodySoundCore BodyTy Merge CtxWalk CtxMerge FinalForwarders FinalLambdas MethodRows.
Import ListNotations.
Local Open Scope list_scope.

Module A := ExecutionModel.Elaboration.

Local Strategy opaque [Nat.of_num_uint FUEL].

(* ======================================================================== *)
(* 1. Steps that leave the cache alone                                       *)
(* ======================================================================== *)

From NM Require Export InfoInvariant.

Lemma later_fromL : forall w {P} R Fl s s', FrL s s' -> Later w P R Fl s' -> Later w P R Fl s.
Proof.
  intros w P R Fl s s' [Fw Ffl (u & Eu) Fla Fx Keep Fc Rows] [Lw Lf Lu Ll Lr Lc Lclosed Lnone Lbool Lforwarders Llambdas Lcontexts]. constructor.
  - congruence.
  - intros a n H. exact (Lf a n (Ffl a n H)).
  - rewrite Lu in Eu. destruct (st_unsup s); [reflexivity | discriminate].
  - congruence.
  - intros x l H. exact (Lr x l (Fx x l H)).
  - intros k x t H. exact (Lc k x t (Fc k (x, t) H)).
  - exact Lclosed.
  - exact Lnone.
  - exact Lbool.
  - eapply final_forwarders_keep;[exact Keep|exact Lforwarders].
  - eapply final_lambdas_keep;[exact Rows|exact Keep|exact Llambdas].
  - exact Lcontexts.
Qed.

(* x is no symbol of the loader's (a symbol the elaborator made) *)
Section Use.
Variable w : Elab.world. Context {P : xprogram}. Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Hypothesis Finj : forall a c, Fl a = Fl c -> a = c.
Hypothesis MS : @MethStep w P R Fl.
Notation WW := (BodySoundCore.W w R Fl).
Notation trF := (tr Fl).

(* what fn_info_ctx concludes of an info i of g *)
Definition FnCtx (g : gsym) (i : fninfo) : Prop :=
  let e := fi_env i in
  exists C, CtxOfIE WW (gs_id g) C (map (key_tr Fl) (fi_needs i)) /\
    A.c_sub C = suf Fl (en_uf e) /\ fi_merges i = en_uf e /\
    (forall k, A.nlook k (A.c_ty C) = option_map (fun y => trF (canon e y)) (tn_get e k)) /\
    (forall v, A.nlook v (A.c_vals C) = option_map (fun y => trF (canon e y)) (nget (en_vals e) v)) /\
    (forall f, A.nlook f (A.c_fns C) = option_map (csig Fl e) (nget (en_fns e) f)) /\
    (forall k m, ety_ok k = true -> canon e k = k -> A.mlook (trF k) m (A.c_mths C) = option_map (csig Fl e) (get_mth e k m)) /\
    (forall u, A.nlook u (A.c_funs C) = option_map (fun p => A.known (tr_inst Fl (fst p))) (get_app e (KyFun u)) /\
       is_some(A.nlook u(A.c_funs C))=is_some(nget(en_funs e)u)) /\
    (forall k, nofun k = true -> key_ok k = true ->
       A.ilook (key_tr Fl k) (A.c_apps C) = option_map (fun p => tr_inst Fl (fst p)) (get_app e k)) /\
    MethodRowsUnique e /\ MethodRowsCanonical e /\
    (forall r m sg,A.mlook r m(A.c_mths C)=Some sg ->
     exists raw,ety_ok raw=true/\canon e raw=raw/\trF raw=r).

Lemma walk_ctx : forall g s e needs s1,
  nget (w_syms w) (gs_id g) = Some g -> (forall t, gs_origin g <> OForwarder t) ->
  ~ A.IsMainFn WW (gs_id g) -> (forall s0 m s2, st_w s0 = w -> is_main g s0 = Good (m, s2) -> m = false) ->
  merged_walk (gs_mod g) (fi_walk g) s = Good ((e, needs), s1) -> Inv s -> st_w s = w -> Later w P R Fl s1 ->
  FnCtx g (FInfo (freeze_all_apps e) needs
              (map (fun k => (k, match get_app (freeze_all_apps e) k with Some (a, _) => a | None => [] end)) needs)
              (merge_edges e)).
Proof.
  intros g s e needs s1 Hg Nf Nm Nm' H1 Hi Hw L.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hmi.
  destruct (assumes_merged w R Fl b Hload Finj MS g s e needs s1 Nf Nm' Hmi H1 Hi Hw L)
    as (les & tm & I & Hws & Mg & Nd).
  set (U := en_uf e) in *.
  exists (A.entries_cx (suf Fl U) tm (map (tr_ent Fl) les)). cbn [fi_env fi_needs fi_merges].
  split; [|split; [reflexivity|split]].
  - exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)), (map (tr_ent Fl) les), tm, [], tm, (suf Fl U).
    rewrite !app_nil_r. split; [exact (w_decl_rd w R Fl _ _ Hg)|]. split; [exact I|]. split; [|auto].
    apply A.RootNone. left. exact Nm.
  - unfold merge_edges. rewrite (tne_nil _ _ _ _ _ _ Hws). reflexivity.
  - split; [|split; [|split; [|split; [|split]]]].
    + intro k. exact (ctx_ty Fl U e tm les needs Hws eq_refl k).
    + intro v. exact (ctx_vals Fl U e tm les needs Hws eq_refl v).
    + intro f. exact (ctx_fns Fl U e tm les needs Hws eq_refl f).
    + intros k m Ok Ck. exact (ctx_mths Fl Finj U e tm les needs Hws eq_refl k m Ok Ck).
    + intro u. exact (ctx_funpack Fl Finj U e tm les needs Hws eq_refl u).
    + split;[intros k Nk Ok;exact(ctx_apps Fl Finj U e tm les needs Hws eq_refl k Nk Ok)|].
      split;[exact(ws_method_unique _ _ _ _ _ _ Hws)|].
      split;[exact(ws_method_canonical _ _ _ _ _ _ Hws eq_refl)|].
      intros r m sg;exact(ctx_mths_domain Fl U e tm les needs Hws eq_refl r m sg).
Qed.

(* fn_info of a function of the program, neither a forwarder nor `main`,
   from a state with the cache's invariant: its info is that function's
   context, whether it was cached or not *)
Theorem fn_info_ctx_any : forall g s i s',
  nget (w_syms w) (gs_id g) = Some g -> (forall t, gs_origin g <> OForwarder t) ->
  ~ A.IsMainFn WW (gs_id g) -> (forall s0 m s1, st_w s0 = w -> is_main g s0 = Good (m, s1) -> m = false) ->
  fn_info g s = Good (i, s') -> Inv s -> InfoInv s -> st_w s = w -> Later w P R Fl s' ->
  FnCtx g i.
Proof.
  intros g s i s' Hg Nf Nm Nm' H Hi I Hw L.
  assert (Hb : load_world b = Good (st_w s)) by (rewrite Hw; exact Hload).
  assert (Gs : nget (st_syms s) (gs_id g) = Some g) by (apply (iv_loader _ Hi b Hb); rewrite Hw; exact Hg).
  destruct (nget (st_info s) (gs_id g)) as [j|] eqn:Ej.
  - (* cached *)
    pose proof H as H0. unfold fn_info in H0. apply bind_inv in H0 as (x & s2 & E & H0). injection E as <- <-.
    rewrite Ej in H0. injection H0 as <- <-.
    destruct (I _ _ Ej) as [(g' & s0 & e & needs & s1 & G0 & Hi0 & H1 & -> & F)|N].
    + destruct (ok_merged_fi g' s0 _ s1 Hi0 H1) as [_ F1].
      assert (W0 : st_w s0 = w) by (rewrite <- Hw, (fl_w _ _ F), (fr_w _ _ F1); reflexivity).
      assert (g' = g).
      { assert (Hb0 : load_world b = Good (st_w s0)) by (rewrite W0; exact Hload).
        pose proof (iv_loader _ Hi0 b Hb0 (gs_id g) g ltac:(rewrite W0; exact Hg)) as E0. congruence. }
      subst g'. exact (walk_ctx g s0 e needs s1 Hg Nf Nm Nm' H1 Hi0 W0 (later_fromL w R Fl _ _ F L)).
    + exfalso. pose proof (proj1 N b Hb) as N0. rewrite Hw in N0. congruence.
  - (* computed *)
    destruct (fn_info_miss g s i s' H Ej) as (g' & e & needs & s1 & Eg & H1 & -> & ->).
    rewrite Gs in Eg. injection Eg as <-.
    exact (walk_ctx g s e needs s1 Hg Nf Nm Nm' H1 Hi Hw (later_info w R Fl s1 _ L)).
Qed.

(* Without merges (BodySoundCore's CxRel, for now), the context's tables are
   fi_env's read by tr, in CxRel's form *)
Corollary fnctx_nomerge : forall g i, FnCtx g i -> en_uf (fi_env i) = [] ->
  let e := fi_env i in
  exists C, CtxOfIE WW (gs_id g) C (map (key_tr Fl) (fi_needs i)) /\ A.c_sub C = [] /\ fi_merges i = [] /\
    (forall k, A.nlook k (A.c_ty C) = option_map trF (tn_get e k)) /\
    (forall v, A.nlook v (A.c_vals C) = option_map trF (nget (en_vals e) v)) /\
    (forall f, A.nlook f (A.c_fns C) =
       option_map (fun g => A.FSig (map (fun p => trF (snd p)) (sg_params g)) (trF (sg_ret g))) (nget (en_fns e) f)) /\
    (forall k m, ety_ok k = true -> A.mlook (trF k) m (A.c_mths C) =
       option_map (fun g => A.FSig (map (fun p => trF (snd p)) (sg_params g)) (trF (sg_ret g))) (get_mth e k m)) /\
    (forall u, A.nlook u (A.c_funs C) = option_map (fun p => A.known (tr_inst Fl (fst p))) (get_app e (KyFun u)) /\
       is_some(A.nlook u(A.c_funs C))=is_some(nget(en_funs e)u)) /\
    (forall k, nofun k = true -> key_ok k = true ->
       A.ilook (key_tr Fl k) (A.c_apps C) = option_map (fun p => tr_inst Fl (fst p)) (get_app e k)).
Proof.
  intros g i (C & Hc & Hs & Hm & Ht & Hv & Hf & Hmt & Hu & Ha & Rows & Canonical & Domain) U e. subst e.
  assert (Cn : forall t, canon (fi_env i) t = t) by (intro t; exact (canon_nil _ t U)).
  assert (Sg : forall g0, csig Fl (fi_env i) g0 = A.FSig (map (fun p => trF (snd p)) (sg_params g0)) (trF (sg_ret g0))).
  { intro g0. unfold csig. rewrite Cn, map_map. f_equal. apply map_ext. intro p. rewrite Cn. reflexivity. }
  exists C. split; [exact Hc|]. split; [rewrite Hs, U; reflexivity|]. split; [rewrite Hm; exact U|].
  split; [intro k; rewrite Ht; destruct (tn_get _ k); cbn; [rewrite Cn|]; reflexivity|].
  split; [intro v; rewrite Hv; destruct (nget _ v); cbn; [rewrite Cn|]; reflexivity|].
  split; [intro f; rewrite Hf; destruct (nget _ f); cbn; [rewrite Sg|]; reflexivity|].
  split; [|exact (conj Hu Ha)].
  intros k m Ok. rewrite (Hmt k m Ok (Cn k)). destruct (get_mth _ k m); cbn; [rewrite Sg|]; reflexivity.
Qed.

End Use.

(* ======================================================================== *)
(* 5. Forwarders: their needs                                                *)
(* ======================================================================== *)

(* The relation reads no CtxOf for a forwarder (FPForward: a Fresh symbol
   whose satisfaction map is [(IFn g, IFn g)]); what the elaborator owes is
   that a forwarder's needs are exactly its target's key.  Its walk assumes
   the target's requirements, type symbols (of a fn: its assume nesting's,
   WorldOf), which add no need, and then the target, a fn. *)

(* merged_walk answers with one of its rounds *)
Lemma mw_round : forall (walk : env -> M (env * list key)) (P : list key -> Prop) (Pre Post : st -> Prop) mi,
  (forall e, ok (walk e)) -> (forall s s', Fr s s' -> Pre s -> Pre s') -> (forall s s', Fr s s' -> Post s' -> Post s) ->
  (forall e0 s e needs s', Inv s -> Pre s -> Post s' -> walk e0 s = Good ((e, needs), s') -> P needs) ->
  forall n tne ufe s e needs s', mw_go walk mi n tne ufe s = Good ((e, needs), s') -> Inv s -> Pre s -> Post s' -> P needs.
Proof.
  intros walk P Pre Post mi Hok Pf Pb Hr n. induction n as [|n IH]; intros tne ufe s e needs s' H Hi Hp Hq;
    cbn [mw_go] in H; apply bind_inv in H as ([e1 n1] & s1 & H1 & H2); destruct (Hok _ s _ s1 Hi H1) as [Hi1 F1];
    cbn beta iota zeta in H2.
  - destruct (edges_eqb _ _); [|noerr H2]. injection H2; intros; subst. exact (Hr _ _ _ _ _ Hi Hp Hq H1).
  - destruct (edges_eqb _ _).
    + injection H2; intros; subst. exact (Hr _ _ _ _ _ Hi Hp Hq H1).
    + exact (IH _ _ s1 e needs s' H2 Hi1 (Pf _ _ F1 Hp) Hq).
Qed.

Lemma asi_ty_needs : forall f e mi g needs s e' n' s', add_symbol_item f e mi g [] needs s = Good ((e', n'), s') ->
  gs_kind g = KType -> n' = needs.
Proof.
  intros [|f] e mi g needs s e' n' s' H K; [discriminate|]. cbn [add_symbol_item] in H.
  apply bind_inv in H as (u & s1 & _ & H). cbn beta zeta in H. rewrite K in H.
  destruct (tn_get e (gs_id g)); [injection H as _ <- _; reflexivity|].
  apply bind_inv in H as (st0 & s2 & _ & H). injection H as _ <- _. reflexivity.
Qed.

Lemma asi_fn_needs : forall f e mi g needs s e' n' s', add_symbol_item f e mi g [] needs s = Good ((e', n'), s') ->
  gs_kind g = KFn -> n' = add_need needs (KyFn (gs_id g)).
Proof.
  intros [|f] e mi g needs s e' n' s' H K; [discriminate|]. cbn [add_symbol_item] in H.
  apply bind_inv in H as (u & s1 & _ & H). cbn beta zeta in H. rewrite K in H.
  apply bind_inv in H as (sg & s2 & _ & H).
  apply bind_inv in H as (e1 & s3 & _ & H).
  apply bind_inv in H as (a & s4 & _ & H).
  apply bind_inv in H as (mem & s5 & _ & H).
  apply bind_inv in H as (ms & s6 & _ & H). cbn beta zeta in H.
  apply bind_inv in H as (eu & s6u & _ & H).
  apply bind_inv in H as (e2 & s7 & _ & H). injection H as _ <- _. reflexivity.
Qed.

Section Fwd.
Variable w : Elab.world. Context {P : xprogram}. Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySoundCore.W w R Fl).

(* the target's requirements are type symbols of the loader's *)
Definition ReqTypes (t : nat) : Prop :=
  forall r, In r (R t) -> exists rg, nget (w_syms w) r = Some rg /\ gs_kind rg = KType.

Lemma spectypes_type : forall p sp r, A.SpecTypes WW p sp r -> A.kind_sym WW r = Some A.DKType.
Proof. intros p sp r H. induction H; assumption. Qed.

(* of a fn, they are (WorldOf: its assume nesting's type symbols) *)
Lemma fn_req_types : forall t tg, A.WorldOf b WW -> nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t.
Proof.
  intros t tg Wo Ht K r Hr.
  assert (Kt : A.kind_sym WW t = Some A.DKFn) by (rewrite (kind_sym_rd w R Fl b Hload _ _ Ht), K; reflexivity).
  apply (proj1 (A.wo_reqs_plain Wo t Kt ltac:(discriminate) ltac:(discriminate) ltac:(discriminate) ltac:(discriminate) r)) in Hr.
  destruct Hr as (d & pa & _ & _ & St). pose proof (spectypes_type _ _ _ St) as Kr.
  unfold A.kind_sym in Kr. destruct (A.w_decl WW r) eqn:Dr; [|discriminate].
  cbn [A.w_decl BodySoundCore.W W_read] in Dr. destruct (nget (w_syms w) r) as [rg|] eqn:Er; [|discriminate].
  exists rg. split; [reflexivity|]. apply kconv_type.
  pose proof (kind_sym_rd w R Fl b Hload _ _ Er) as Kr'. unfold A.kind_sym in Kr'.
  cbn [A.w_decl BodySoundCore.W W_read] in Kr'. rewrite Er in Kr'. injection Dr as <-. rewrite Kr in Kr'.
  injection Kr' as E. exact (eq_sym E).
Qed.

Lemma sym_loader_st : forall x g s g' s', Inv s -> st_w s = w -> nget (w_syms w) x = Some g -> sym x s = Good (g', s') ->
  g' = g /\ s' = s.
Proof.
  intros x g s g' s' Hi Hw Hg H. unfold sym in H.
  rewrite (iv_loader _ Hi b ltac:(rewrite Hw; exact Hload) x g ltac:(rewrite Hw; exact Hg)) in H.
  injection H as <- <-. auto.
Qed.

Lemma fwd_reqs_needs : forall cf mi rs acc s res s',
  mfold (fun acc r => let! rg := sym r in add_symbol_item cf (fst acc) mi rg [] (snd acc)) acc rs s = Good (res, s') ->
  Inv s -> st_w s = w -> (forall r, In r rs -> exists rg, nget (w_syms w) r = Some rg /\ gs_kind rg = KType) ->
  snd res = snd acc.
Proof.
  intros cf mi rs. induction rs as [|r rest IH]; intros acc s res s' H Hi Hw Ht; cbn [mfold] in H.
  - injection H as <- _. reflexivity.
  - apply bind_inv in H as (a1 & s1 & H1 & H2). apply bind_inv in H1 as (rg & s0 & Hs & H1).
    destruct (Ht r (or_introl eq_refl)) as (rg' & Er & Kr).
    destruct (sym_loader_st _ _ _ _ _ Hi Hw Er Hs) as [-> ->].
    destruct a1 as [e1 n1]. destruct (ok_add_symbol_item cf (fst acc) mi rg' [] (snd acc) s _ s1 Hi H1) as [Hi1 F1].
    rewrite (IH (e1, n1) s1 res s' H2 Hi1 (fr_w' w _ _ F1 Hw) (fun r0 H0 => Ht r0 (or_intror H0))).
    exact (asi_ty_needs _ _ _ _ _ _ _ _ _ H1 Kr).
Qed.

Lemma fwd_walk_needs : forall g t tg e0 s e needs s', gs_origin g = OForwarder t ->
  nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t ->
  fi_walk g e0 s = Good ((e, needs), s') -> Inv s -> st_w s = w -> Later w P R Fl s' -> needs = [KyFn t].
Proof.
  intros g t tg e0 s e needs s' O Ht K Rt H Hi Hw L. unfold fi_walk in H. rewrite O in H.
  apply bind_inv in H as (tg' & s1 & H1 & H). destruct (sym_loader_st _ _ _ _ _ Hi Hw Ht H1) as [-> ->].
  destruct (bstep _ _ _ _ _ _ _ (ok_reqs t) Hi H) as (rs & s2 & H2 & H3 & Hi2 & F2).
  destruct (bstep _ _ _ _ _ _ _ ok_ctx_fuel Hi2 H3) as (cf & s3 & _ & H4 & Hi3 & F3).
  assert (Okf : forall acc r, ok (let! rg := sym r in add_symbol_item cf (fst acc) (gs_mod g) rg [] (snd acc)))
    by (intros; apply ok_bind; [unfold sym; okt | intro; apply ok_add_symbol_item]).
  destruct (bstep _ _ _ _ _ _ _ (ok_mfold _ _ _ rs (e0, []) Okf) Hi3 H4) as ([e4 n4] & s4 & H5 & H6 & Hi4 & F4).
  destruct (ok_add_symbol_item cf e4 (gs_mod g) tg [] n4 s4 _ s' Hi4 H6) as [_ F6].
  assert (Er : R t = rs) by (apply (@reqs_R w P R Fl _ s rs s2 Hi H2); exact (later_from w R Fl _ _ F3 (later_from w R Fl _ _ F4 (later_from w R Fl _ _ F6 L)))).
  pose proof (fwd_reqs_needs _ _ _ _ _ _ _ H5 Hi3 (fr_w' w _ _ F3 (fr_w' w _ _ F2 Hw)) ltac:(rewrite <- Er; exact Rt)) as N4.
  cbn [snd] in N4. subst n4.
  rewrite (asi_fn_needs _ _ _ _ _ _ _ _ _ H6 K), (load_world_ids b w Hload t tg Ht). reflexivity.
Qed.

(* fn_info of a forwarder (the symbol table's entry for its number), cached
   or not: its needs are its target's key *)
Theorem fn_info_fwd : forall g0 g t tg s i s',
  nget (st_syms s) (gs_id g0) = Some g -> gs_origin g = OForwarder t ->
  nget (w_syms w) t = Some tg -> gs_kind tg = KFn -> ReqTypes t ->
  fn_info g0 s = Good (i, s') -> Inv s -> InfoInv s -> st_w s = w -> Later w P R Fl s' ->
  fi_needs i = [KyFn t].
Proof.
  intros g0 g t tg s i s' Gs O Ht K Rt H Hi I Hw L.
  assert (Gen : forall g' s0 e needs s1, nget (st_syms s0) (gs_id g0) = Some g' -> g' = g -> Inv s0 -> st_w s0 = w ->
            Later w P R Fl s1 -> merged_walk (gs_mod g') (fi_walk g') s0 = Good ((e, needs), s1) -> needs = [KyFn t]).
  { intros g' s0 e needs s1 _ -> Hi0 Hw0 L1 H1. rewrite (mw_go_eq (fi_walk g)) in H1.
    exact (mw_round (fi_walk g) (fun n => n = [KyFn t]) (fun s2 => st_w s2 = w) (Later w P R Fl) (gs_mod g)
             (ok_fi_walk g) (fun s2 s3 F E => fr_w' w s2 s3 F E) (fun s2 s3 F L2 => later_from w R Fl s2 s3 F L2)
             (fun e0 s2 e2 n2 s3 Hi2 Hw2 L3 H2 => fwd_walk_needs g t tg e0 s2 e2 n2 s3 O Ht K Rt H2 Hi2 Hw2 L3)
             8 [] [] s0 e needs s1 H1 Hi0 Hw0 L1). }
  destruct (nget (st_info s) (gs_id g0)) as [j|] eqn:Ej.
  - pose proof H as H0. unfold fn_info in H0. apply bind_inv in H0 as (x & s2 & E & H0). injection E as <- <-.
    rewrite Ej in H0. injection H0 as <- <-.
    destruct (I _ _ Ej) as [(g' & s0 & e & needs & s1 & G0 & Hi0 & H1 & -> & F)|[_ (g' & G' & Nf)]].
    + destruct (ok_merged_fi g' s0 _ s1 Hi0 H1) as [_ F1].
      assert (W0 : st_w s0 = w) by (rewrite <- Hw, (fl_w _ _ F), (fr_w _ _ F1); reflexivity).
      assert (Eg : g' = g) by (pose proof (fl_keep _ _ F _ _ (fr_keep _ _ F1 _ _ G0)) as G1; congruence).
      exact (Gen g' s0 e needs s1 G0 Eg Hi0 W0 (later_fromL w R Fl _ _ F L) H1).
    + exfalso. rewrite Gs in G'. injection G' as <-. exact (Nf t O).
  - destruct (fn_info_miss g0 s i s' H Ej) as (g' & e & needs & s1 & Eg & H1 & -> & ->).
    exact (Gen g' s e needs s1 Eg ltac:(congruence) Hi Hw (later_info w R Fl s1 _ L) H1).
Qed.

End Fwd.
