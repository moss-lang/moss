(* Body bindings retain the context's already solved merge. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import NMap Sx Elab ElabSound.
Import ListNotations.
Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

Lemma provided_at_uf : forall e k g tm meth s e' s',
  provided_at e k g tm meth s = Good (e',s') -> en_uf e' = en_uf e.
Proof.
  intros e k g tm meth s e' s' H. unfold provided_at in H.
  apply bind_inv in H as (a & s1 & H1 & H).
  apply bind_inv in H as (stamp & s2 & H2 & H). injection H as <- _. reflexivity.
Qed.

Lemma mfold_env_uf : forall X (step : env -> X -> M env),
  (forall e x s e' s', step e x s = Good (e',s') -> en_uf e' = en_uf e) ->
  forall xs e s e' s', mfold step e xs s = Good (e',s') -> en_uf e' = en_uf e.
Proof.
  intros X step S xs. induction xs as [|x xs IH]; intros e s e' s' H; cbn [mfold] in H.
  - injection H as <- _. reflexivity.
  - apply bind_inv in H as (e1 & s1 & H1 & H). rewrite (IH _ _ _ _ H). exact (S _ _ _ _ _ H1).
Qed.

Lemma mfold_pair_uf : forall X (step : env * list key -> X -> M (env * list key)),
  (forall e ks x s e' ks' s', step (e,ks) x s = Good ((e',ks'),s') -> en_uf e' = en_uf e) ->
  forall xs e ks s e' ks' s', mfold step (e,ks) xs s = Good ((e',ks'),s') -> en_uf e' = en_uf e.
Proof.
  intros X step S xs. induction xs as [|x xs IH]; intros e ks s e' ks' s' H; cbn [mfold] in H.
  - injection H as <- _ _. reflexivity.
  - apply bind_inv in H as ([e1 ks1] & s1 & H1 & H). rewrite (IH _ _ _ _ _ _ H). exact (S _ _ _ _ _ _ _ H1).
Qed.

Ltac uf_noerr H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
    first [exact (err_not_good _ _ _ _ _ _ H) | discriminate H
          | apply bind_inv in H as (? & ? & ? & H)])].

Ltac uf_leaf H :=
  lazymatch type of H with
  | ret _ _ = _ => idtac
  | Good _ = Good _ => idtac
  end;
  first [solve [inversion H; subst; reflexivity]
        |solve [inversion H; subst;
           match goal with Hp : provided_at _ _ _ _ _ _ = Good _ |- _ =>
             rewrite (provided_at_uf _ _ _ _ _ _ _ _ Hp)
           end; cbn [set_val set_fn set_mth en_uf];
           repeat match goal with |- context [match ?k with KyVal _ => _ | _ => _ end] => destruct k end;
           reflexivity]
        |uf_noerr H].

Ltac uf_peel H rec :=
  cbn beta iota zeta in H;
  first [uf_leaf H |rec H
  |lazymatch type of H with err _ _ _ = _ => uf_noerr H | fail _ _ = _ => discriminate H end
  |lazymatch type of H with
    | mbind _ _ _ = _ =>
        let a := fresh "a" in let s1 := fresh "s1" in let H1 := fresh "Hstep" in
        apply bind_inv in H as (a & s1 & H1 & H); uf_peel H rec
    | context [match ?v with _ => _ end] => destruct v eqn:?; uf_peel H rec
    end].

Lemma provide_spec_uf : forall fuel e fg mi sp outer ks s e' ks' s',
  provide_spec fuel e fg mi sp outer ks s = Good ((e',ks'),s') -> en_uf e' = en_uf e.
Proof.
  induction fuel as [|fuel IH]; intros e fg mi sp outer ks s e' ks' s' H; [uf_noerr H|].
  cbn [provide_spec] in H. destruct sp as [p d a].
  uf_peel H ltac:(fun E =>
    first [solve [eapply IH; exact E]
          |solve [eapply mfold_pair_uf; [|exact E];
                  intros; cbn [fst snd] in *; eapply IH; eassumption]]).
Qed.

Lemma provide_result_uf : forall e fg gens s e' ks s',
  provide_result e fg gens s = Good ((e',ks),s') -> en_uf e' = en_uf e.
Proof.
  intros e fg gens s e' ks s' H. unfold provide_result in H.
  apply bind_inv in H as (ts & s1 & H1 & H).
  apply bind_inv in H as (e1 & s2 & H2 & H).
  assert (E1 : en_uf e1 = en_uf e).
  { eapply mfold_env_uf; [|exact H2]. intros env_start t state_start env_end state_end Hstep.
    cbn beta in Hstep. destruct (nassoc t gens); [apply bind_inv in Hstep as (stamp & ss & _ & Hstep)|]; injection Hstep as <- _; reflexivity. }
  destruct (functor_parts fg) as [[args res] body].
  apply bind_inv in H as (n & s3 & H3 & H).
  rewrite <- E1. eapply mfold_pair_uf; [|exact H].
  intros. cbn [fst snd] in *. eapply provide_spec_uf; eassumption.
Qed.

Lemma mfold_triple_uf : forall X (step : list xstmt * env * cx -> X -> M (list xstmt * env * cx)),
  (forall xs e c x s ys e' c' s', step (xs,e,c) x s = Good ((ys,e',c'),s') -> en_uf e' = en_uf e) ->
  forall its xs e c s ys e' c' s', mfold step (xs,e,c) its s = Good ((ys,e',c'),s') -> en_uf e' = en_uf e.
Proof.
  intros X step S its. induction its as [|it its IH]; intros xs e c s ys e' c' s' H; cbn [mfold] in H.
  - inversion H; subst; reflexivity.
  - apply bind_inv in H as ([[zs e1] c1] & s1 & H1 & H).
    rewrite (IH _ _ _ _ _ _ _ _ H). exact (S _ _ _ _ _ _ _ _ _ H1).
Qed.

Definition Env3 (p : list xstmt * env * cx) := snd (fst p).
Definition Env4 (p : list xstmt * env * cx * bool) := snd (fst (fst p)).
Definition KeepsUF {A} (getenv : A -> env) (e : env) (m : M A) : Prop :=
  forall s p s', m s = Good (p,s') -> en_uf (getenv p) = en_uf e.

Record BodyUF fuel : Prop := {
  uf_stmt : forall c e st, KeepsUF Env4 e (check_stmt fuel c e st);
  uf_stmts : forall c e ss, KeepsUF Env4 e (check_stmts fuel c e ss);
  uf_bind : forall c e sp y, KeepsUF Env3 e (check_bind fuel c e sp y);
  uf_apply : forall c e sp, KeepsUF Env3 e (apply_functor fuel c e sp);
  uf_apply_body : forall c e g, KeepsUF Env3 e (apply_body fuel c e g);
  uf_apply_abstract : forall c e g, KeepsUF Env3 e (apply_abstract fuel c e g);
  uf_bind_functor : forall c e g y, KeepsUF snd e (bind_functor fuel c e g y)
}.

Ltac uf_body_facts IH :=
  repeat match goal with
  | H : check_stmt _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_stmt _ IH _ _ _ _ _ _ H) as E; clear H
  | H : check_stmts _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_stmts _ IH _ _ _ _ _ _ H) as E; clear H
  | H : check_bind _ _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_bind _ IH _ _ _ _ _ _ _ H) as E; clear H
  | H : apply_functor _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_apply _ IH _ _ _ _ _ _ H) as E; clear H
  | H : apply_body _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_apply_body _ IH _ _ _ _ _ _ H) as E; clear H
  | H : apply_abstract _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_apply_abstract _ IH _ _ _ _ _ _ H) as E; clear H
  | H : bind_functor _ _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (uf_bind_functor _ IH _ _ _ _ _ _ _ H) as E; clear H
  | H : provided_at _ _ _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (provided_at_uf _ _ _ _ _ _ _ _ H) as E; clear H
  | H : provide_result _ _ _ _ = Good _ |- _ =>
      let E := fresh "EU" in pose proof (provide_result_uf _ _ _ _ _ _ _ H) as E; clear H
  end; cbn [Env3 Env4 fst snd set_tn set_val set_fn set_mth set_fun set_app set_local en_uf] in *.

Ltac uf_body_peel H IH :=
  cbn beta iota zeta in H;
  first [solve [lazymatch type of H with ret _ _ = _ => inversion H; subst end;
                uf_body_facts IH; congruence]
  |solve [uf_body_facts IH; congruence]
  |lazymatch type of H with err _ _ _ = _ => uf_noerr H | fail _ _ = _ => discriminate H end
  |lazymatch type of H with
    | mbind _ _ _ = _ =>
        let a := fresh "a" in let s1 := fresh "s1" in let H1 := fresh "Hstep" in
        apply bind_inv in H as (a & s1 & H1 & H); uf_body_peel H IH
    | context [match ?v with _ => _ end] => destruct v eqn:?; uf_body_peel H IH
    end].

Lemma body_uf_zero : BodyUF 0.
Proof. constructor; unfold KeepsUF; intros; discriminate. Qed.

Lemma bind_functor_uf_step : forall fuel, BodyUF fuel ->
  forall c e g y, KeepsUF snd e (bind_functor (S fuel) c e g y).
Proof.
  intros fuel IH c e g y s p s' H. cbn [bind_functor] in H.
  uf_body_peel H IH.
Qed.

Lemma check_bind_uf_step : forall fuel, BodyUF fuel ->
  forall c e sp y, KeepsUF Env3 e (check_bind (S fuel) c e sp y).
Proof.
  intros fuel IH c e sp y s p s' H. cbn [check_bind] in H.
  uf_body_peel H IH.
Qed.

Lemma apply_abstract_uf_step : forall fuel, BodyUF fuel ->
  forall c e g, KeepsUF Env3 e (apply_abstract (S fuel) c e g).
Proof.
  intros fuel IH c e g s p s' H. cbn [apply_abstract] in H.
  uf_body_peel H IH.
Qed.

Lemma apply_functor_uf_step : forall fuel, BodyUF fuel ->
  forall c e sp, KeepsUF Env3 e (apply_functor (S fuel) c e sp).
Proof.
  intros fuel IH c e sp s p s' H. cbn [apply_functor] in H.
  uf_body_peel H IH.
Qed.

Lemma body_binds_fold_uf : forall fuel, BodyUF fuel ->
  forall its xs e c s ys e' c' s',
  mfold (fun acc it =>
    let '(xs,e,c) := acc in
    let! r := match it with (sp,None) => apply_functor fuel c e sp
                         | (sp,Some y) => check_bind fuel c e sp y end in
    let '(ys,e',c') := r in ret (xs ++ ys,e',c')) (xs,e,c) its s = Good ((ys,e',c'),s') ->
  en_uf e' = en_uf e.
Proof.
  intros fuel IH its xs e c s ys e' c' s' H.
  eapply mfold_triple_uf; [|exact H].
  intros xs0 e0 c0 [sp yo] s0 ys0 e1 c1 s1 Hstep. destruct yo; cbn beta iota zeta in Hstep.
  all: uf_body_peel Hstep IH.
Qed.

Ltac uf_body_fold_facts IH :=
  repeat match goal with
  | H : mfold _ (_,_,_) _ _ = Good ((_,_,_),_) |- _ =>
      let E := fresh "EUfold" in
      pose proof (body_binds_fold_uf _ IH _ _ _ _ _ _ _ _ _ H) as E; clear H
  end.

Ltac uf_body_complete H IH :=
  cbn beta iota zeta in H;
  first [solve [lazymatch type of H with ret _ _ = _ => inversion H; subst end;
                uf_body_fold_facts IH; uf_body_facts IH; congruence]
  |solve [uf_body_fold_facts IH; uf_body_facts IH; congruence]
  |lazymatch type of H with err _ _ _ = _ => uf_noerr H | fail _ _ = _ => discriminate H end
  |lazymatch type of H with
    | mbind _ _ _ = _ =>
        let a := fresh "a" in let s1 := fresh "s1" in let H1 := fresh "Hstep" in
        apply bind_inv in H as (a & s1 & H1 & H); uf_body_complete H IH
    | context [match ?v with _ => _ end] => destruct v eqn:?; uf_body_complete H IH
    end].

Lemma apply_body_uf_step : forall fuel, BodyUF fuel ->
  forall c e g, KeepsUF Env3 e (apply_body (S fuel) c e g).
Proof.
  intros fuel IH c e g s p s' H. cbn [apply_body] in H.
  destruct (nmem (gs_id g) (cx_applying c)); [uf_noerr H|].
  destruct (functor_parts g) as [[args res] [binds|]].
  - apply bind_inv in H as ([[xs e1] c1] & s1 & H1 & H). injection H as <- _. cbn [Env3 fst snd].
    eapply body_binds_fold_uf; [exact IH|exact H1].
  - inversion H; subst; reflexivity.
Qed.

Lemma check_stmt_uf_step : forall fuel, BodyUF fuel ->
  forall c e st, KeepsUF Env4 e (check_stmt (S fuel) c e st).
Proof.
  intros fuel IH c e st s p s' H. cbn [check_stmt] in H.
  destruct st; try solve [uf_body_peel H IH].
  apply bind_inv in H as ([[xs e1] c1] & s1 & H1 & H). injection H as <- _. cbn [Env4 fst snd].
  eapply body_binds_fold_uf; [exact IH|exact H1].
Qed.

Lemma check_stmts_uf_step : forall fuel, BodyUF fuel ->
  forall c e ss, KeepsUF Env4 e (check_stmts (S fuel) c e ss).
Proof.
  intros fuel IH c e ss s p s' H. cbn [check_stmts] in H.
  uf_body_complete H IH.
Qed.

Theorem body_uf : forall fuel, BodyUF fuel.
Proof.
  induction fuel as [|fuel IH]; [exact body_uf_zero|]. constructor.
  - exact (check_stmt_uf_step fuel IH).
  - exact (check_stmts_uf_step fuel IH).
  - exact (check_bind_uf_step fuel IH).
  - exact (apply_functor_uf_step fuel IH).
  - exact (apply_body_uf_step fuel IH).
  - exact (apply_abstract_uf_step fuel IH).
  - exact (bind_functor_uf_step fuel IH).
Qed.

Corollary check_stmt_uf : forall fuel c e st s xs e' c' d s',
  check_stmt fuel c e st s = Good ((xs,e',c',d),s') -> en_uf e' = en_uf e.
Proof. intros. exact (uf_stmt _ (body_uf fuel) _ _ _ _ _ _ H). Qed.

Corollary check_stmts_uf : forall fuel c e ss s xs e' c' d s',
  check_stmts fuel c e ss s = Good ((xs,e',c',d),s') -> en_uf e' = en_uf e.
Proof. intros. exact (uf_stmts _ (body_uf fuel) _ _ _ _ _ _ H). Qed.
