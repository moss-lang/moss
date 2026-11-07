From Stdlib Require Import List Bool Arith.
From NM Require Import Audit MossX FastX LazyX LazyEnvFacts.
Import ListNotations.
Set Implicit Arguments.
Definition brel (a : (fenv * xvenv * mstate) + xres) (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (fe, g, st), inl (d, g', st') => envrel fe d /\ g = g' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition arel (a : (fenv * mstate) + xres) (b : (xdenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, st), inl (l', st') => envrel l l' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition rrel (a : (fenv * xvenv * mstate) + xres)
    (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, g, st), inl (l', g', st') => envrel l l' /\ g = g' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition AGREE (k : nat) : Prop :=
  (forall P fe d g st e, envrel fe d -> fxinterp k P fe g st e = xinterp k P d g st e)
  /\ (forall P fe d g st l, envrel fe d -> fxinterp_list k P fe g st l = xinterp_list k P d g st l)
  /\ (forall P fe d g st bs, envrel fe d ->
        brel (fxinterp_binds k P fe g st bs) (xinterp_binds k P d g st bs))
  /\ (forall P fe d u th sm st, envrel fe d ->
        arel (fxinterp_apply k P fe u th sm st) (xinterp_apply k P d u th sm st))
  /\ (forall P fe d g st u brs, envrel fe d ->
        rrel (fxinterp_branch k P fe g st u brs) (xinterp_branch k P d g st u brs))
  /\ (forall P fe d u inputs outputs st, envrel fe d ->
        arel (fxinterp_apply_r k P fe u inputs outputs st) (xinterp_apply_r k P d u inputs outputs st)).

(* Case on a lookup in related environments. *)
Ltac lk H i :=
  let Hl := fresh "Hl" in
  pose proof (envrel_lookup i H) as Hl; unfold orel in Hl;
  destruct (flookup i _), (lookup_xdenv i _); try contradiction;
  try (inversion Hl; subst; clear Hl).

Ltac cp H sm :=
  let Hc := fresh "Hc" in
  pose proof (fcompose_rel sm H) as Hc; unfold crel in Hc;
  destruct (fcompose _ sm), (xcompose _ sm); try contradiction.

Ltac dfn := match goal with |- context [xp_fn ?P ?x] => destruct (xp_fn P x) end.

Lemma agree_step : forall k, AGREE k -> AGREE (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa [IHr IHar]]]]].
  split; [|split; [|split; [|split; [|split]]]].
  - intros P fe d g st e H. destruct e; simpl.
    all: try (erewrite ?(IHe P fe d) by exact H; reflexivity).
    + (* XVl *) lk H (IVal v); auto.
    + (* XTag *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[v| | | | | | |] gg ss]; auto.
      rewrite (fcast_rel P n s v H). reflexivity.
    + (* XCallD *)
      destruct (xp_fn P f); auto. rewrite (IHl P fe d) by exact H.
      destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
      cp H s0; auto. f_equal. apply IHe, fcall_env_rel; [exact H|auto].
    + (* XCallA *)
      lk H (IFn f); auto.
      * dfn; auto. rewrite (IHl P fe d) by exact H.
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
        f_equal. apply IHe; auto.
      * rewrite (IHl P fe d) by exact H. reflexivity.
      * rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMeth *)
      lk H i; auto.
      * dfn; auto. rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H.
        destruct (xinterp_list k P d g0 st0 l) as [[[ws g1] st1]|]; auto.
        f_equal. apply IHe; auto.
      * rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H. reflexivity.
      * rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMatch *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct w0; auto. destruct (find_xarm n l) as [[x eb]|]; auto.
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XLet *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XBindTy *) apply IHe, fbind_ty_rel, H.
    + (* XBindVal *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      apply IHe, envrel_add; auto. constructor.
    + (* XBindFn *)
      cp H s; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + (* XBindMth *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + (* XBindFun *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + (* XBindFunA *)
      lk H (IFun u0); auto. apply IHe, envrel_add; auto. constructor; auto.
    + (* XApply *)
      pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHe, envrel_merge; auto.
      * subst; reflexivity.
    + (* XApplyR *)
      pose proof (IHar P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply_r k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply_r k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHe, envrel_merge; auto.
      * subst; reflexivity.
    + (* XSeq *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    + (* XIf *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (truthy P w0) as [[]|]; auto; try (apply IHe; assumption).
    + (* XWhile *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (truthy P w0) as [[]|]; auto; try (apply IHe; assumption).
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g0 st0 e2) as [[w1| | | | | | |] g1 st1]; auto; try (apply IHe; assumption).
    + (* XLoop *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    + (* XRec *)
      rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMatchP *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (select_arm l w0) as [[bs eb]|]; auto.
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XBindFunSym *) apply IHe, envrel_add; auto. constructor.
    + (* XBindFunCopy *)
      pose proof (envrel_lookup (IFun u0) H) as Hl; unfold orel in Hl.
      destruct (flookup (IFun u0) fe), (lookup_xdenv (IFun u0) d); try contradiction; auto.
      apply IHe, envrel_add; auto.
    + (* XApplyBr *)
      pose proof (IHr P fe d g st u l H) as Hr.
      destruct (fxinterp_branch k P fe g st u l) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u l) as [[[dr' g1'] st1']|r1']; simpl in Hr;
        try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply IHe, envrel_merge; auto.
      * subst; reflexivity.
    + (* XBindPrim *) apply IHe, envrel_add; [apply fprim_entry_rel|]; exact H.
  - intros P fe d g st l H. destruct l as [|e r]; simpl; auto.
    rewrite (IHe P fe d) by exact H.
    destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    rewrite (IHl P fe d) by exact H. reflexivity.
  - intros P fe d g st bs H. destruct bs as [|b r]; simpl; [auto|].
    destruct b; simpl.
    + apply IHb, fbind_ty_rel, H.
    + rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st x) as [[w0| | | | | | |] g0 st0]; simpl; auto.
      apply IHb, envrel_add; auto. constructor.
    + cp H s; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|auto].
    + lk H (IFun u0); simpl; auto. apply IHb, envrel_add; auto. constructor; auto.
    + pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHb, envrel_merge; auto.
      * subst; reflexivity.
    + pose proof (IHar P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply_r k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply_r k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHb, envrel_merge; auto.
      * subst; reflexivity.
    + apply IHb, envrel_add; auto. constructor.
    + pose proof (envrel_lookup (IFun u0) H) as Hl; unfold orel in Hl.
      destruct (flookup (IFun u0) fe), (lookup_xdenv (IFun u0) d); try contradiction; simpl; auto.
      apply IHb, envrel_add; auto.
    + pose proof (IHr P fe d g st u l H) as Hr.
      destruct (fxinterp_branch k P fe g st u l) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u l) as [[[dr' g1'] st1']|r1']; simpl in Hr;
        try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply IHb, envrel_merge; auto.
      * subst; reflexivity.
    + apply IHb, envrel_add; [apply fprim_entry_rel|]; exact H.
  - intros P fe d u th sm st H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context [xp_fun ?P ?x] => destruct (xp_fun P x) as [[Dr bs]|] end;
      simpl; auto.
    cp H sm; simpl; auto.
    match goal with
    | Hc : envrel ?a ?b |- context [fchain ?a ?sms] =>
        pose proof (fchain_rel sms Hc) as Hch; unfold crel in Hch;
        destruct (fchain a sms) as [l1|], (xrun_chain b sms) as [l2|];
        try contradiction; simpl; auto
    end.
    match goal with
    | Hd : envrel ?fe0 ?d0 |- context [fxinterp_binds k P (fapply_env P fe h th (fmerge l1 ?fe0)) [] st bs] =>
        pose proof (IHb P (fapply_env P fe h th (fmerge l1 fe0)) (xapply_env P d h th (l2 ++ d0)) [] st bs (fapply_env_rel P h th H (envrel_merge Hch Hd))) as Hb;
        destruct (fxinterp_binds k P (fapply_env P fe h th (fmerge l1 fe0)) [] st bs) as [[[d3 g3] st3]|r3],
                 (xinterp_binds k P (xapply_env P d h th (l2 ++ d0)) [] st bs) as [[[d3' g3'] st3']|r3'];
        simpl in Hb; try contradiction; auto
    end.
    destruct Hb as [Hd3 [-> ->]].
    cp Hd3 (inst_smap th Dr); simpl; auto. split; [apply fresult_env_rel; assumption|reflexivity].
  - intros P fe d g st u brs H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context [find_branch ?h brs] =>
      destruct (find_branch h brs) as [[bs pairs]|] end; simpl; auto.
    pose proof (IHb P fe d g st bs H) as Hb.
    destruct (fxinterp_binds k P fe g st bs) as [[[d3 g3] st3]|r3],
             (xinterp_binds k P d g st bs) as [[[d3' g3'] st3']|r3']; simpl in Hb;
      try contradiction; auto.
    destruct Hb as [Hd3 [-> ->]].
    cp Hd3 pairs; simpl; auto.
  - intros P fe d u inputs outputs st H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context[xp_fun ?P ?h] => destruct(xp_fun P h)as[[Dr bs]|]end; simpl;auto.
    cp H inputs; simpl;auto.
    match goal with Hc:envrel ?a ?b |-context[fchain ?a ?sms] =>
      pose proof(fchain_rel sms Hc)as Hch; unfold crel in Hch;
      destruct(fchain a sms)as[l1|],(xrun_chain b sms)as[l2|]; try contradiction;simpl;auto end.
    match goal with Hd:envrel ?fe0 ?d0 |-context[fxinterp_binds k P (fmerge l1 ?fe0) [] st bs] =>
      pose proof(IHb P (fmerge l1 fe0)(l2++d0) [] st bs (envrel_merge Hch Hd))as Hb;
      destruct(fxinterp_binds k P (fmerge l1 fe0) [] st bs)as[[[d3 g3]st3]|r3],
      (xinterp_binds k P (l2++d0) [] st bs)as[[[d3' g3']st3']|r3'];simpl in Hb;try contradiction;auto end.
    destruct Hb as[Hd3[-> ->]].
    cp Hd3 (xp_exports P h); simpl;auto.
    match goal with |-context[fcompose ?named outputs] =>
      match goal with Hn:envrel named ?named' |- _ => cp Hn outputs end end;
      simpl;auto.

Qed.

Theorem fxinterp_agrees : forall fuel P fe d g st e,
  envrel fe d -> fxinterp fuel P fe g st e = xinterp fuel P d g st e.
Proof.
  intros fuel. assert (H : AGREE fuel).
  { induction fuel as [|k IH].
    - repeat split; intros; simpl; auto.
    - apply agree_step, IH. }
  apply H.
Qed.

Theorem frun_main_eq : forall fuel P main d0 st0,
  frun_main fuel P main d0 st0 = run_main fuel P main d0 st0.
Proof.
  intros. unfold frun_main, run_main. destruct (xp_fn P main) as [body|]; auto.
  rewrite (fxinterp_agrees fuel P [] st0 body (conv_env_rel d0)). reflexivity.
Qed.
