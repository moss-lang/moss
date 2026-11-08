(* Agreement of the cached closure compiler with the slot interpreter. *)
From Stdlib Require Import List Arith Lia.
From NM Require Import ExecutionModel CoreMoss MossX Prune SlotX FoldX CloScalar CachedClo CachedEnvFacts.
From NM Require FastPath.
Import ListNotations.
Set Implicit Arguments.

Section Scalar.
Variable R : rprog.
Local Notation P := (rp_x R).
Local Notation csev := (CachedClo.csev R).
Local Notation csevl := (CachedClo.csevl R).

Lemma csev_eq_n : forall n e d g st,
  sdepth e <= n -> csev e (cache_of d) g st = sev P d g st e.
Proof.
  induction n as [|n IH]; intros e d g st Hn; [destruct e; simpl in Hn; lia|].
  assert (Args : forall args d g st,
    needed args <= S n -> csevl args (cache_of d) g st = sevl P d g args st).
  { induction args as [|a rest IHr]; intros d0 g0 st0 Bound; [reflexivity|].
    simpl in Bound.
    change (csevl (a :: rest)) with (cscons (csev a) (csevl rest)).
    unfold cscons. simpl sevl. rewrite IH by lia.
    destruct (sev P d0 g0 st0 a) as [[w st1]|]; [rewrite IHr by lia|]; reflexivity. }
  destruct e as [x|l|t e1|l args|j args body|e0 p b]; simpl in Hn.
  - reflexivity.
  - reflexivity.
  - simpl. rewrite IH by lia. reflexivity.
  - change (csev (SPrim l args) (cache_of d) g st) with
      (match SlotX.rlook l d with
       | Some (QPrim p) =>
           match csevl args (cache_of d) g st with
           | Some (ws, st1) =>
               match run_prim_at P p None ws st1 with
               | (XOk w, st2) => Some (w, st2)
               | _ => None
               end
           | None => None
           end
       | _ => None
       end).
    rewrite sev_prim. destruct (SlotX.rlook l d) as [[]|]; try reflexivity.
    fold (needed args) in Hn. rewrite Args by lia. reflexivity.
  - change (csev (SCall j args body) (cache_of d) g st) with
      (match SlotX.rnth2 j d with
       | Some _ =>
           match csevl args (cache_of d) g st with
           | Some (ws, st1) => csev body (cache_of d) (mkv ws) st1
           | None => None
           end
       | None => None
       end).
    rewrite sev_call. fold (needed args) in Hn.
    destruct (SlotX.rnth2 j d); try reflexivity.
    rewrite Args by lia. destruct (sevl P d g args st) as [[ws st1]|]; try reflexivity.
    apply IH. lia.
  - simpl. rewrite IH by lia. destruct (sev P d g st e0) as [[w st1]|]; try reflexivity.
    destruct (match_pat p w); try reflexivity. apply IH. lia.
Qed.

Lemma csev_eq : forall e d g st, csev e (cache_of d) g st = sev P d g st e.
Proof. intros; apply csev_eq_n with (n := sdepth e); lia. Qed.

Lemma csevl_eq : forall args d g st, csevl args (cache_of d) g st = sevl P d g args st.
Proof.
  induction args as [|a rest IH]; intros d g st; [reflexivity|].
  change (csevl (a :: rest)) with (cscons (csev a) (csevl rest)).
  unfold cscons. simpl sevl. rewrite csev_eq.
  destruct (sev P d g st a) as [[w st1]|]; [rewrite IH|]; reflexivity.
Qed.

Lemma cfast_eq : forall ss sb pl d g st,
  match csevl ss (cache_of d) g st with
  | Some (ws, st1) =>
      match CachedClo.rcompose (cache_of d) pl with
      | Some d' => csev sb d' (mkv ws) st1
      | None => None
      end
  | None => None
  end = fastcall P d g st pl ss sb.
Proof.
  intros. unfold fastcall. rewrite csevl_eq, cache_of_compose.
  destruct (sevl P d g ss st) as [[ws st1]|]; try reflexivity.
  destruct (SlotX.rcompose d pl); [apply csev_eq|reflexivity].
Qed.
End Scalar.

(* Successful bindings and applications return another canonical cache.
   Their failure results, like expression results, are unchanged. *)
Definition lift_bresult (r : (renv * xvenv * mstate) + xres) :
    (cached_env * xvenv * mstate) + xres :=
  match r with
  | inl (d, g, st) => inl (cache_of d, g, st)
  | inr err => inr err
  end.

Definition lift_apply_result (r : (renv * mstate) + xres) :
    (cached_env * mstate) + xres :=
  match r with
  | inl (d, st) => inl (cache_of d, st)
  | inr err => inr err
  end.

Definition tbl_ok (R : rprog) (tbl : fsym -> option (code * code))
    (ftbl : usym -> option (tele * bcode * option (nat * cached_env))) : Prop :=
  (forall f, tbl f =
     match rp_fn R f with
     | Some (bs, bd) => Some (cc R tbl ftbl bs, cc R tbl ftbl bd)
     | None => None
     end) /\
  (forall h, ftbl h =
     match rp_fun R h with
     | Some (Dr, bs) => Some (Dr, ccbs R tbl ftbl bs, prepare_scalar_cache Dr bs)
     | None => None
     end).

Section Agreement.
Variable R : rprog.
Variable tbl : fsym -> option (code * code).
Variable ftbl : usym -> option (tele * bcode * option (nat * cached_env)).
Hypothesis Tables : tbl_ok R tbl ftbl.
Local Notation cc := (CachedClo.cc R tbl ftbl).
Local Notation ccb := (CachedClo.ccb R tbl ftbl).
Local Notation ccl := (CachedClo.ccl R tbl ftbl).
Local Notation ccbs := (CachedClo.ccbs R tbl ftbl).
Local Notation capply := (CachedClo.capply R ftbl).
Local Notation capply_r := (CachedClo.capply_r R ftbl).
Local Notation cbrs_c := (CachedClo.cbrs_c R tbl ftbl).

Local Definition crec (fs : list (fld * rexpr)) : lcode :=
  fold_right (fun p cr => let '(_, e) := p in lcons (cc e) cr) lnil fs.

Local Lemma rfind_carm_cc : forall t arms,
  rfind_carm t (map (fun a => let '(t, x, eb) := a in (t, x, cc eb)) arms)
  = match rfind_arm t arms with Some (x, eb) => Some (x, cc eb) | None => None end.
Proof. intros t arms; induction arms as [|[[t' x] eb] r IH]; simpl; auto. destruct (Nat.eqb t t'); auto. Qed.

Local Lemma rselect_carm_cc : forall arms w,
  rselect_carm (map (fun a => let '(p, eb) := a in (p, cc eb)) arms) w
  = match rselect_arm arms w with Some (bs, eb) => Some (bs, cc eb) | None => None end.
Proof. intros arms w; induction arms as [|[p eb] r IH]; simpl; auto. destruct (match_pat p w); auto. Qed.

Local Lemma rfind_cbranch_cc : forall h brs,
  rfind_cbranch h (cbrs_c brs)
  = match rfind_branch h brs with Some (bs, pairs) => Some (ccbs bs, pairs) | None => None end.
Proof. intros h brs; induction brs as [|[[h' bs] pairs] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto. Qed.

Local Lemma cc_while : forall c b k d g st,
  cc (RWhile c b) (S k) d g st =
  match cc c k d g st with
  | XR (XOk w) g1 st1 =>
      match truthy (rp_x R) w with
      | Some true =>
          match cc b k d g1 st1 with
          | XR (XOk _) g2 st2 => cc (RWhile c b) k d g2 st2
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | Some false => XR (XOk XVUnit) g1 st1
      | None => XR (XErr e_bool) g1 st1
      end
  | r => r
  end.
Proof. reflexivity. Qed.

Local Lemma cc_loop : forall b k d g st,
  cc (RLoop b) (S k) d g st =
  match cc b k d g st with
  | XR (XOk _) g2 st2 => cc (RLoop b) k d g2 st2
  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
  | r => r
  end.
Proof. reflexivity. Qed.

Local Definition agree (n : nat) : Prop :=
  (forall e d g st, cc e n (cache_of d) g st = rinterp n R d g st e)
  /\ (forall l d g st, ccl l n (cache_of d) g st = rinterp_list n R d g st l)
  /\ (forall fs d g st, crec fs n (cache_of d) g st = rinterp_list n R d g st (map snd fs))
  /\ (forall bs d g st, ccbs bs n (cache_of d) g st = lift_bresult (rinterp_binds n R d g st bs))
  /\ (forall l th pl d st, capply l th pl n (cache_of d) st = lift_apply_result (rinterp_apply n R d l th pl st))
  /\ (forall l brs d g st, cbranch l (cbrs_c brs) n (cache_of d) g st = lift_bresult (rinterp_branch n R d g st l brs))
  /\ (forall l inputs outputs d st, capply_r l inputs outputs n (cache_of d) st = lift_apply_result (rinterp_apply_r n R d l inputs outputs st)).

Local Ltac fold_lists :=
  repeat match goal with
  | |- context [fold_right ?F lnil ?a] =>
      lazymatch type of a with
      | list rexpr => change (fold_right F lnil a) with (ccl a)
      | list (fld * rexpr) => change (fold_right F lnil a) with (crec a)
      end
  | |- context [map ?F ?brs] =>
      lazymatch type of brs with
      | list (usym * list rbind * plan) => change (map F brs) with (cbrs_c brs)
      end
  end.

Local Ltac cache_normalize :=
  repeat first
  [ progress rewrite ?cache_of_rlook, ?cache_of_cast, ?cache_of_flag, ?cache_of_prim,
      ?cache_of_push, ?cache_of_append, ?cache_of_bind_ty,
      ?cache_of_compose, ?cache_of_compose_dyn, ?cache_of_chain, ?cache_of_close,
      ?cache_of_call, ?cache_of_apply, ?cache_of_result_env, ?cache_of_inst_keys
  | progress cbn [CachedClo.rlook CachedClo.rnth2 CachedClo.rtyped_cast
      cache_raw option_map lift_bresult lift_apply_result] ].

Local Lemma agree_step : forall k, agree k -> agree (S k).
Proof.
  intros k [IHe [IHl [IHr [IHb [IHa [IHbr IHar]]]]]].
  destruct Tables as [Htbl Hftbl].
  Ltac dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar :=
    repeat first
    [ solve [reflexivity]
    | match goal with
      | |- context [match csevl ?R ?ss (cache_of ?d) ?g ?st with _ => _ end] => rewrite cfast_eq
      | |- context [cc ?e ?k (cache_of ?d) ?g ?st] => rewrite (IHe e d g st)
      | |- context [ccl ?l ?k (cache_of ?d) ?g ?st] => rewrite (IHl l d g st)
      | |- context [crec ?l ?k (cache_of ?d) ?g ?st] => rewrite (IHr l d g st)
      | |- context [ccbs ?l ?k (cache_of ?d) ?g ?st] => rewrite (IHb l d g st)
      | |- context [capply ?l ?th ?pl ?k (cache_of ?d) ?st] => rewrite (IHa l th pl d st)
      | |- context [capply_r ?l ?ins ?outs ?k (cache_of ?d) ?st] => rewrite (IHar l ins outs d st)
      | |- context [cbranch ?l (cbrs_c ?brs) ?k (cache_of ?d) ?g ?st] =>
          rewrite (IHbr l brs d g st)
      | |- context [match tbl ?f with _ => _ end] =>
          rewrite (Htbl f); destruct (rp_fn R f) as [[? ?]|]
      | |- context [match ftbl ?h with _ => _ end] =>
          rewrite (Hftbl h); destruct (rp_fun R h) as [[? ?]|]
      | |- context [if ?b then cc ?x else cc ?y] => destruct b
      | |- context [match (if ?b then ?x else None) with _ => _ end] =>
          destruct (if b then x else None) as [[? ?]|]
      end
    | progress cache_normalize
    | match goal with
      | |- context [match rinterp ?k ?R ?d ?g ?st ?e with _ => _ end] =>
          destruct (rinterp k R d g st e) as [[| | | | | | |] ? ?]
      | |- context [match rinterp_list ?k ?R ?d ?g ?st ?e with _ => _ end] =>
          destruct (rinterp_list k R d g st e) as [[[? ?] ?]|?]
      | |- context [rinterp_binds ?k ?R ?d ?g ?st ?e] =>
          destruct (rinterp_binds k R d g st e) as [[[? ?] ?]|?]
      | |- context [rinterp_apply ?k ?R ?d ?l ?th ?pl ?st] =>
          destruct (rinterp_apply k R d l th pl st) as [[? ?]|?]
      | |- context [rinterp_apply_r ?k ?R ?d ?l ?ins ?outs ?st] =>
          destruct (rinterp_apply_r k R d l ins outs st) as [[? ?]|?]
      | |- context [rinterp_branch ?k ?R ?d ?g ?st ?l ?brs] =>
          destruct (rinterp_branch k R d g st l brs) as [[[? ?] ?]|?]
      | |- context [match SlotX.rlook ?l ?d with _ => _ end] =>
          destruct (SlotX.rlook l d) as [[]|]
      | |- context [match SlotX.rcompose ?d ?pl with _ => _ end] =>
          destruct (SlotX.rcompose d pl)
      | |- context [match SlotX.rcompose_dyn ?d ?pl with _ => _ end] =>
          destruct (SlotX.rcompose_dyn d pl)
      | |- context [match SlotX.rchain ?d ?sms with _ => _ end] =>
          destruct (SlotX.rchain d sms)
      | |- context [match truthy ?P ?w with _ => _ end] => destruct (truthy P w) as [[]|]
      end
    ]; try reflexivity.
  split; [|split; [|split; [|split; [|split; [|split]]]]].
  - intros e d g st. destruct e.
    all: try (rewrite cc_while; cbn [rinterp]; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: try (rewrite cc_loop; cbn [rinterp]; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: cbn [CachedClo.cc rinterp]; fold_lists.
    all: dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
    all: try (match goal with |- context [match ?w with XVTag _ _ => _ | _ => _ end] => destruct w end;
              cbn iota; try reflexivity; rewrite rfind_carm_cc;
              match goal with |- context [rfind_arm ?t ?a] => destruct (rfind_arm t a) as [[? ?]|] end;
              dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar).
    all: try (rewrite rselect_carm_cc;
              match goal with |- context [rselect_arm ?a ?w] => destruct (rselect_arm a w) as [[? ?]|] end;
              dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar).
  - intros l d g st. destruct l as [|e r]; [reflexivity|].
    change (ccl (e :: r)) with (match e with
      | RVar x => lvar x (ccl r) | RVl l => lval l (ccl r)
      | _ => lcons (cc e) (ccl r)
      end).
    destruct e; cbn [lvar lval lcons rinterp_list].
    all: try (rewrite (IHl r d g st); destruct k as [|k']; [reflexivity|]; cbn [rinterp]; cache_normalize;
              first [destruct (lookup_xvenv _ g) | destruct (SlotX.rlook _ d) as [[]|]]; reflexivity).
    all: rewrite IHe; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros fs d g st. destruct fs as [|[f e] r]; [reflexivity|].
    change (crec ((f, e) :: r)) with (lcons (cc e) (crec r)).
    cbn [lcons map snd rinterp_list]. dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros bs d g st. destruct bs as [|b r]; [reflexivity|].
    change (ccbs (b :: r)) with (ccb b (ccbs r)).
    destruct b; cbn [CachedClo.ccb rinterp_binds];
      try match goal with e : rexpr |- _ => fold (cc e) end;
      fold_lists; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l th pl d st. cbn [CachedClo.capply rinterp_apply].
    change (CachedClo.rlook l (cache_of d)) with (SlotX.rlook l d).
    destruct (SlotX.rlook l d) as [[]|]; try reflexivity.
    rewrite Hftbl. destruct (rp_fun R u) as [[Dr bs]|]; [|reflexivity].
    rewrite cache_of_compose. destruct (SlotX.rcompose d pl) as [d1|]; [|reflexivity].
    cbn [option_map]. rewrite cache_of_chain. destruct (SlotX.rchain d1 l0) as [d2|]; [|reflexivity].
    unfold prepare_scalar_cache. destruct (prepare_scalar Dr bs) as [[count result]|] eqn:Prepared.
    + cbn [option_map fst snd].
      rewrite cache_of_inst_keys, instantiate_scalar_fast_exact.
      rewrite (@prepared_scalar_execution Dr bs count result R k _ st th Prepared).
      destruct (Nat.ltb count k); reflexivity.
    + dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l brs d g st. cbn [cbranch rinterp_branch].
    dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
    all: rewrite rfind_cbranch_cc;
      match goal with |- context [rfind_branch ?h ?a] => destruct (rfind_branch h a) as [[? ?]|] end;
      dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l inputs outputs d st. cbn [CachedClo.capply_r rinterp_apply_r].
    dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
Qed.

Theorem cc_agrees : forall n e d g st, cc e n (cache_of d) g st = rinterp n R d g st e.
Proof.
  intros n. assert (H : agree n).
  { induction n as [|k IH].
    - repeat split; intros;
        repeat match goal with
        | e : rexpr |- _ => destruct e; reflexivity
        | l : list _ |- _ => destruct l as [|[]]; reflexivity
        end.
    - apply agree_step, IH. }
  apply H.
Qed.
End Agreement.

Theorem crun_main_eq : forall R tbl ftbl fuel main d0 st0,
  tbl_ok R tbl ftbl ->
  CachedClo.crun_main tbl fuel main d0 st0 = rrun_main fuel R main d0 st0.
Proof.
  intros R tbl ftbl fuel main d0 st0 Tables.
  unfold CachedClo.crun_main, rrun_main.
  rewrite (proj1 Tables). destruct (rp_fn R main) as [[bs bd]|]; try reflexivity.
  rewrite (cc_agrees Tables). reflexivity.
Qed.

(* The driver supplies the same folded program as the original closure
   engine and fills its tables with cached closures and scalar templates. *)
Theorem engine_agrees : forall p argv rd fuel st0 tb M N R' tbl ftbl,
  let R := rprog_of (oracle_program p) argv rd in
  (forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd)) ->
  rp_x R' = rp_x R ->
  (forall f, rp_fn R' f =
     match rp_fn R f with
     | Some (bs, bd) => Some (guard_fold_e (rp_x R) tb M N bs, guard_fold_e (rp_x R) tb M N bd)
     | None => None
     end) ->
  (forall h, rp_fun R' h =
     match rp_fun R h with
     | Some (Dr, bs) => Some (Dr, map (guard_fold_b (rp_x R) tb M N) bs)
     | None => None
     end) ->
  tbl_ok R' tbl ftbl ->
  let P := program_of p argv rd in
  stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ CachedClo.crun_main tbl fuel (xo_main p) (xo_env p) st0 =
     run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tb M N R' tbl ftbl R Htb Hx Hfn Hfun Tables P.
  rewrite (crun_main_eq fuel (xo_main p) (xo_env p) st0 Tables).
  rewrite (@guard_fold_run_main R R' tb M N fuel (xo_main p) (xo_env p) st0 Htb Hx Hfn Hfun).
  apply slot_oracle_agrees.
Qed.

Theorem fast_engine_agrees : forall p argv rd fuel st0 tb M N R' tbl ftbl,
  let q := FastPath.fast_program p in
  let R := rprog_of (oracle_program q) argv rd in
  (forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd)) ->
  rp_x R' = rp_x R ->
  (forall f, rp_fn R' f =
     match rp_fn R f with
     | Some (bs, bd) => Some (guard_fold_e (rp_x R) tb M N bs, guard_fold_e (rp_x R) tb M N bd)
     | None => None
     end) ->
  (forall h, rp_fun R' h =
     match rp_fun R h with
     | Some (Dr, bs) => Some (Dr, map (guard_fold_b (rp_x R) tb M N) bs)
     | None => None
     end) ->
  tbl_ok R' tbl ftbl ->
  let P := program_of p argv rd in
  FastPath.fo (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ stuck (run_main fuel P (xo_main p) (xo_env p) st0)
  \/ CachedClo.crun_main tbl fuel (xo_main q) (xo_env q) st0 =
     run_main fuel P (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tb M N R' tbl ftbl q R Htb Hx Hfn Hfun Tables P.
  destruct (FastPath.fast_run p argv rd fuel st0) as [Fuel|Fast]; [left; exact Fuel|].
  unfold FastPath.run_of in Fast.
  destruct (@engine_agrees q argv rd fuel st0 tb M N R' tbl ftbl Htb Hx Hfn Hfun Tables)
    as [Stuck|Runs]; unfold P; rewrite <- Fast.
  - right; left; exact Stuck.
  - right; right; exact Runs.
Qed.
