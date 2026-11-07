(* Closure compilation preserves the split-environment interpreter exactly. *)
From Stdlib Require Import List Arith.
From NM Require Import Audit MossX LazyX LazyClo LazyInterpFacts LazyFoldFacts.
From NM Require Prune FastPath.
Import ListNotations.
Set Implicit Arguments.
Lemma mkv_eq : forall ws, LazyClo.mkv ws = mk_xvenv ws.
Proof.
  unfold LazyClo.mkv, mk_xvenv. intro ws. generalize 0.
  induction ws as [|w r IH]; intro i; simpl; auto. f_equal. apply IH.
Qed.
Definition tbl_ok (P : xprog) (tbl : fsym -> option code)
    (ftbl : usym -> option (tele * bcode)) : Prop :=
  (forall f, tbl f = option_map (LazyClo.cc P tbl ftbl) (xp_fn P f)) /\
  (forall h, ftbl h = option_map (fun '(Dr,bs) => (Dr,LazyClo.ccbs P tbl ftbl bs)) (xp_fun P h)).
Section Agreement.
Variable P : xprog.
Variable tbl : fsym -> option code.
Variable ftbl : usym -> option (tele * bcode).
Hypothesis Tables : tbl_ok P tbl ftbl.
Local Notation cc := (LazyClo.cc P tbl ftbl).
Local Notation ccb := (LazyClo.ccb P tbl ftbl).
Local Notation ccl := (LazyClo.ccl P tbl ftbl).
Local Notation ccbs := (LazyClo.ccbs P tbl ftbl).
Local Notation capply := (LazyClo.capply P ftbl).
Local Notation capply_r := (LazyClo.capply_r P ftbl).
Local Notation cbrs_c := (LazyClo.cbrs_c P tbl ftbl).

Local Definition crec (fs : list (fld * xexpr)) : lcode :=
  fold_right (fun p cr => let '(_, e) := p in lcons (cc e) cr) lnil fs.

Local Lemma rfind_carm_cc : forall t arms,
  rfind_carm t (map (fun a => let '(t, x, eb) := a in (t, x, cc eb)) arms)
  = match find_xarm t arms with Some (x, eb) => Some (x, cc eb) | None => None end.
Proof. intros t arms; induction arms as [|[[t' x] eb] r IH]; simpl; auto. destruct (Nat.eqb t t'); auto. Qed.

Local Lemma rselect_carm_cc : forall arms w,
  rselect_carm (map (fun a => let '(p, eb) := a in (p, cc eb)) arms) w
  = match select_arm arms w with Some (bs, eb) => Some (bs, cc eb) | None => None end.
Proof. intros arms w; induction arms as [|[p eb] r IH]; simpl; auto. destruct (match_pat p w); auto. Qed.

Local Lemma rfind_cbranch_cc : forall h brs,
  rfind_cbranch h (cbrs_c brs)
  = match find_branch h brs with Some (bs, pairs) => Some (ccbs bs, pairs) | None => None end.
Proof. intros h brs; induction brs as [|[[h' bs] pairs] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto. Qed.

Local Lemma cc_while : forall c b k d g st,
  cc (XWhile c b) (S k) d g st =
  match cc c k d g st with
  | XR (XOk w) g1 st1 =>
      match truthy (P) w with
      | Some true =>
          match cc b k d g1 st1 with
          | XR (XOk _) g2 st2 => cc (XWhile c b) k d g2 st2
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
  cc (XLoop b) (S k) d g st =
  match cc b k d g st with
  | XR (XOk _) g2 st2 => cc (XLoop b) k d g2 st2
  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
  | r => r
  end.
Proof. reflexivity. Qed.

Local Definition agree (n : nat) : Prop :=
  (forall e d g st, cc e n d g st = fxinterp n P d g st e)
  /\ (forall l d g st, ccl l n d g st = fxinterp_list n P d g st l)
  /\ (forall fs d g st, crec fs n d g st = fxinterp_list n P d g st (map snd fs))
  /\ (forall bs d g st, ccbs bs n d g st = fxinterp_binds n P d g st bs)
  /\ (forall l th pl d st, capply l th pl n d st = fxinterp_apply n P d l th pl st)
  /\ (forall l brs d g st, cbranch l (cbrs_c brs) n d g st = fxinterp_branch n P d g st l brs)
  /\ (forall l inputs outputs d st, capply_r l inputs outputs n d st = fxinterp_apply_r n P d l inputs outputs st).

Local Ltac fold_lists :=
  repeat match goal with
  | |- context [fold_right ?F lnil ?a] =>
      lazymatch type of a with
      | list xexpr => change (fold_right F lnil a) with (ccl a)
      | list (fld * xexpr) => change (fold_right F lnil a) with (crec a)
      end
  | |- context [map ?F ?brs] =>
      lazymatch type of brs with
      | list (usym * list xbind * smap) => change (map F brs) with (cbrs_c brs)
      end
  end.

Local Ltac cache_normalize :=
  progress (unfold LazyClo.rlook, LazyClo.rtyped_cast, LazyClo.cache_raw,
    LazyClo.cache_of, LazyClo.cpush, LazyClo.cappend, LazyClo.rcompose,
    LazyClo.rcompose_dyn, LazyClo.rchain, LazyClo.rcall_env, LazyClo.rapply_env,
    LazyClo.rresult_env, LazyClo.rbind_ty, LazyClo.rprim_entry, LazyClo.rclose;
    cbn [fst snd]).

Local Lemma agree_step : forall k, agree k -> agree (S k).
Proof.
  intros k [IHe [IHl [IHr [IHb [IHa [IHbr IHar]]]]]].
  destruct Tables as [Htbl Hftbl].
  Ltac dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar :=
    repeat first
    [ solve [reflexivity]
    | match goal with
      | |- context [cc ?e ?k ?d ?g ?st] => rewrite (IHe e d g st)
      | |- context [ccl ?l ?k ?d ?g ?st] => rewrite (IHl l d g st)
      | |- context [crec ?l ?k ?d ?g ?st] => rewrite (IHr l d g st)
      | |- context [ccbs ?l ?k ?d ?g ?st] => rewrite (IHb l d g st)
      | |- context [capply ?l ?th ?pl ?k ?d ?st] => rewrite (IHa l th pl d st)
      | |- context [capply_r ?l ?ins ?outs ?k ?d ?st] => rewrite (IHar l ins outs d st)
      | |- context [cbranch ?l (cbrs_c ?brs) ?k ?d ?g ?st] =>
          rewrite (IHbr l brs d g st)
      | |- context [match tbl ?f with _ => _ end] =>
          rewrite (Htbl f); destruct (xp_fn P f) as [?|]; cbn [option_map]
      | |- context [match ftbl ?h with _ => _ end] =>
          rewrite (Hftbl h); destruct (xp_fun P h) as [[? ?]|]; cbn [option_map]
      | |- context [if ?b then cc ?x else cc ?y] => destruct b
      | |- context [match (if ?b then ?x else None) with _ => _ end] =>
          destruct (if b then x else None) as [[? ?]|]
      end
    | progress rewrite mkv_eq
    | progress cache_normalize
    | match goal with
      | |- context [match fxinterp ?k ?R ?d ?g ?st ?e with _ => _ end] =>
          destruct (fxinterp k P d g st e) as [[| | | | | | |] ? ?]
      | |- context [match fxinterp_list ?k ?R ?d ?g ?st ?e with _ => _ end] =>
          destruct (fxinterp_list k P d g st e) as [[[? ?] ?]|?]
      | |- context [fxinterp_binds ?k ?R ?d ?g ?st ?e] =>
          destruct (fxinterp_binds k P d g st e) as [[[? ?] ?]|?]
      | |- context [fxinterp_apply ?k ?R ?d ?l ?th ?pl ?st] =>
          destruct (fxinterp_apply k P d l th pl st) as [[? ?]|?]
      | |- context [fxinterp_apply_r ?k ?R ?d ?l ?ins ?outs ?st] =>
          destruct (fxinterp_apply_r k P d l ins outs st) as [[? ?]|?]
      | |- context [fxinterp_branch ?k ?R ?d ?g ?st ?l ?brs] =>
          destruct (fxinterp_branch k P d g st l brs) as [[[? ?] ?]|?]
      | |- context [match flookup ?l ?d with _ => _ end] =>
          destruct (flookup l d) as [[]|]
      | |- context [match fcompose ?d ?pl with _ => _ end] =>
          destruct (fcompose d pl)
      | |- context [match fcompose ?d ?pl with _ => _ end] =>
          destruct (fcompose d pl)
      | |- context [match fchain ?d ?sms with _ => _ end] =>
          destruct (fchain d sms)
      | |- context [match truthy ?P ?w with _ => _ end] => destruct (truthy P w) as [[]|]
      end
    ]; try reflexivity.
  split; [|split; [|split; [|split; [|split; [|split]]]]].
  - intros e d g st. destruct e.
    all: try (rewrite cc_while; cbn [fxinterp]; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: try (rewrite cc_loop; cbn [fxinterp]; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar; fail).
    all: cbn [LazyClo.cc fxinterp]; fold_lists.
    all: try (apply LazyFoldFacts.fold_call_agrees; cbn [fxinterp]).
    all: dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
    all: try (match goal with |- context [match ?w with XVTag _ _ => _ | _ => _ end] => destruct w end;
              cbn iota; try reflexivity; rewrite rfind_carm_cc;
              match goal with |- context [find_xarm ?t ?a] => destruct (find_xarm t a) as [[? ?]|] end;
              dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar).
    all: try (rewrite rselect_carm_cc;
              match goal with |- context [select_arm ?a ?w] => destruct (select_arm a w) as [[? ?]|] end;
              dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar).
  - intros l d g st. destruct l as [|e r]; [reflexivity|].
    change (ccl (e :: r)) with (match e with
      | XVar x => lvar x (ccl r) | XVl v => lval (IVal v) (ccl r)
      | _ => lcons (cc e) (ccl r)
      end).
    destruct e; cbn [lvar lval lcons fxinterp_list].
    all: try (rewrite (IHl r d g st); destruct k as [|k']; [reflexivity|]; cbn [fxinterp]; try cache_normalize;
              first [destruct (lookup_xvenv _ g) | destruct (flookup _ d) as [[]|]]; reflexivity).
    all: rewrite IHe; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros fs d g st. destruct fs as [|[f e] r]; [reflexivity|].
    change (crec ((f, e) :: r)) with (lcons (cc e) (crec r)).
    cbn [lcons map snd fxinterp_list]. dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros bs d g st. destruct bs as [|b r]; [reflexivity|].
    change (ccbs (b :: r)) with (ccb b (ccbs r)).
    destruct b; cbn [LazyClo.ccb fxinterp_binds];
      try match goal with e : xexpr |- _ => fold (cc e) end;
      fold_lists; dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l th pl d st. cbn [LazyClo.capply fxinterp_apply].
    dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l brs d g st. cbn [cbranch fxinterp_branch].
    dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
    all: rewrite rfind_cbranch_cc;
      match goal with |- context [find_branch ?h ?a] => destruct (find_branch h a) as [[? ?]|] end;
      dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
  - intros l inputs outputs d st. cbn [LazyClo.capply_r fxinterp_apply_r].
    dig Htbl Hftbl IHe IHl IHr IHb IHa IHbr IHar.
Qed.

Theorem cc_agrees : forall n e d g st, cc e n d g st = fxinterp n P d g st e.
Proof.
  intros n. assert (H : agree n).
  { induction n as [|k IH].
    - repeat split; intros;
        repeat match goal with
        | e : xexpr |- _ => destruct e; first [reflexivity | apply LazyFoldFacts.fold_call_agrees; reflexivity]
        | l : list _ |- _ => destruct l as [|[]]; reflexivity
        end.
    - apply agree_step, IH. }
  apply H.
Qed.
End Agreement.


Theorem crun_main_eq : forall P tbl ftbl fuel main d0 st0,
  tbl_ok P tbl ftbl ->
  LazyClo.crun_main tbl fuel main d0 st0 = run_main fuel P main d0 st0.
Proof.
  intros P tbl ftbl fuel main d0 st0 Tables.
  rewrite <- (LazyInterpFacts.frun_main_eq fuel P main d0 st0).
  unfold LazyClo.crun_main, LazyX.frun_main.
  rewrite (proj1 Tables). destruct (xp_fn P main) as [bd|]; cbn [option_map]; try reflexivity.
  rewrite (cc_agrees Tables). reflexivity.
Qed.

Theorem engine_agrees : forall p argv rd fuel st0 tbl ftbl,
  tbl_ok (program_of (Prune.oracle_program p) argv rd) tbl ftbl ->
  Prune.stuck (run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) \/
  LazyClo.crun_main tbl fuel (xo_main p) (xo_env p) st0 =
    run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tbl ftbl Tables.
  rewrite (crun_main_eq fuel (xo_main p) (xo_env p) st0 Tables).
  pose proof (Prune.oracle_agrees p argv rd fuel st0) as H.
  cbv zeta in H. rewrite FastX.frun_main_eq in H. exact H.
Qed.

Theorem fast_engine_agrees : forall p argv rd fuel st0 tbl ftbl,
  let q := FastPath.fast_program p in
  tbl_ok (program_of (Prune.oracle_program q) argv rd) tbl ftbl ->
  FastPath.fo (run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) \/
  Prune.stuck (run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) \/
  LazyClo.crun_main tbl fuel (xo_main q) (xo_env q) st0 =
    run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0.
Proof.
  intros p argv rd fuel st0 tbl ftbl q Tables.
  destruct (FastPath.fast_run p argv rd fuel st0) as [Fuel|Same]; [left; exact Fuel|].
  unfold FastPath.run_of in Same.
  destruct (@engine_agrees q argv rd fuel st0 tbl ftbl Tables) as [Stuck|Runs]; rewrite <- Same.
  - right; left; exact Stuck.
  - right; right; exact Runs.
Qed.
