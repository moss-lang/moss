(* The break scan establishes either answer only with enough evidence.
   Exhaustion has no logical consequence. *)
From Stdlib Require Import List Bool.
From NM Require AuditBase.
From NM Require Import Audit EaDecide.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

Definition break_answer (result : option bool) (fact : Prop) : Prop :=
  match result with
  | Some true => fact
  | Some false => ~ fact
  | None => True
  end.

Lemma break_answer_iff : forall result (observed fact : Prop),
  (observed <-> fact) -> break_answer result observed -> break_answer result fact.
Proof. intros [[|]|]; cbn; tauto. Qed.

Lemma break_or_sound : forall left right (A B : Prop),
  break_answer left A -> break_answer (right tt) B ->
  break_answer (break_or left right) (A \/ B).
Proof. intros [[|]|] right A B; cbn [break_or]; destruct (right tt) as [[|]|]; cbn; tauto. Qed.

Lemma any_break_sound : forall A (scan : A -> option bool) (P : A -> Prop) items,
  (forall item, In item items -> break_answer (scan item) (P item)) ->
  break_answer (any_break scan items) (exists item, In item items /\ P item).
Proof.
  intros A scan P items. induction items as [|item items IH]; intro Checked; cbn [any_break].
  - intros [? [Impossible _]]. exact Impossible.
  - eapply break_answer_iff with (observed := P item \/ exists next, In next items /\ P next).
    + split.
      * intros [Here|[next [Present Break]]]; [exists item|exists next]; cbn; auto.
      * intros [found [[<-|Present] Break]]; [now left|right; eauto].
    + apply break_or_sound; [apply Checked; now left|].
      apply IH. intros next Present. apply Checked. now right.
Qed.

Lemma optional_break_sound : forall A (scan : A -> option bool) (P : A -> Prop) item,
  (forall x, break_answer (scan x) (P x)) ->
  break_answer (optional_break scan item) (exists x, item = Some x /\ P x).
Proof.
  intros A scan P [item|] Checked; cbn [optional_break].
  - eapply break_answer_iff with (observed := P item); [|apply Checked].
    split; [eauto|intros [x [Equal Break]]; now injection Equal as <-].
  - intros [x [Impossible _]]. discriminate.
Qed.

Theorem break_scans_sound : forall W fuel,
  (forall md e, break_answer (breaks W fuel md e) (Breaks W md e)) /\
  (forall md b, break_answer (block_breaks W fuel md b) (BlockBreaks W md b)) /\
  (forall md s, break_answer (stmt_breaks W fuel md s) (StmtBreaks W md s)) /\
  (forall md it, break_answer (item_breaks W fuel md it) (ItemBreaks W md it)).
Proof.
  intros W fuel. induction fuel as [|fuel [Expr [Block [Stmt Item]]]].
  { repeat split; exact I. }
  repeat split.
  - intros md e.
    destruct e as [| |path brackets|path brackets args|receiver name|receiver path args|
      path brackets fields|cond body other|scrutinee arms|ret| |body|operator input|
      operator left right|text|params result body]; cbn [breaks];
      try solve [intro Impossible; inversion Impossible]; try solve [constructor].
    + eapply break_answer_iff with (observed := exists arg, In arg args /\ Breaks W md arg).
      * split; [intros [arg [Present Break]]; eapply BkCall; eassumption|intro Break; inversion Break; subst; eauto].
      * apply any_break_sound. intros. apply Expr.
    + eapply break_answer_iff with (observed := Breaks W md receiver);
        [split; [apply BkField|intro Break; now inversion Break]|apply Expr].
    + eapply break_answer_iff with
        (observed := Breaks W md receiver \/ exists arg, In arg args /\ Breaks W md arg).
      * split; [intros [Break|[arg [Present Break]]]; eauto using BkMethRecv, BkMethArg|
          intro Break; inversion Break; subst; eauto].
      * apply break_or_sound; [apply Expr|apply any_break_sound; intros; apply Expr].
    + eapply break_answer_iff with (observed := exists field, In field fields /\
        exists y, snd field = Some y /\ Breaks W md y).
      * split.
        -- intros [[name [value|]] [Present [y [Equal Break]]]]; [|discriminate].
           injection Equal as <-. eapply BkRec; eassumption.
        -- intro Break. inversion Break; subst. eexists (_, Some _). cbn. eauto.
      * apply any_break_sound. intros. apply optional_break_sound, Expr.
    + eapply break_answer_iff with (observed := Breaks W md cond \/ BlockBreaks W md body \/
        exists y, other = Some y /\ Breaks W md y).
      * split.
        -- intros [Condition|[Then|[y [-> Else]]]]; eauto using BkIfC, BkIfB, BkIfE.
        -- intro Break. inversion Break; subst; eauto 6.
      * apply break_or_sound; [apply Expr|]. apply break_or_sound;
          [apply Block|apply optional_break_sound, Expr].
    + eapply break_answer_iff with
        (observed := Breaks W md scrutinee \/ exists arm, In arm arms /\ Breaks W md (snd arm)).
      * split.
        -- intros [Subject|[[pattern branch] [Present Break]]]; eauto using BkMatchS, BkMatchA.
        -- intro Break. inversion Break; subst; [now left|]. right. eexists (_, _). cbn. eauto.
      * apply break_or_sound; [apply Expr|apply any_break_sound; intros; apply Expr].
    + eapply break_answer_iff with (observed := exists y, ret = Some y /\ Breaks W md y).
      * split; [intros [y [-> Break]]; now apply BkReturn|intro Break; inversion Break; subst; eauto].
      * apply optional_break_sound, Expr.
    + eapply break_answer_iff with (observed := BlockBreaks W md body);
        [split; [apply BkBlock|intro Break; now inversion Break]|apply Block].
    + eapply break_answer_iff with (observed := Breaks W md input);
        [split; [apply BkUnary|intro Break; now inversion Break]|apply Expr].
    + eapply break_answer_iff with (observed := Breaks W md left \/ Breaks W md right).
      * split; [intros [L|R]; eauto using BkBinL, BkBinR|intro Break; inversion Break; auto].
      * apply break_or_sound; apply Expr.
  - intros md [statements tail]. cbn [block_breaks].
    eapply break_answer_iff with (observed := (exists s, In s statements /\ StmtBreaks W md s) \/
      exists y, tail = Some y /\ Breaks W md y).
    + split.
      * intros [[s [Present Break]]|[y [-> Break]]]; eauto using BBStmt, BBTail.
      * intro Break. inversion Break; subst; eauto 6.
    + apply break_or_sound; [apply any_break_sound; intros; apply Stmt|apply optional_break_sound, Expr].
  - intros md statement. destruct statement as
      [name annotation e|name annotation e|name e|items|e body|body|e]; cbn [stmt_breaks];
      try solve [intro Impossible; inversion Impossible];
      try (eapply break_answer_iff with (observed := Breaks W md e);
        [split; [constructor; assumption|intro Break; now inversion Break]|apply Expr]).
    eapply break_answer_iff with (observed := exists item, In item items /\ ItemBreaks W md item).
    + split; [intros [it [Present Break]]; eapply SBBind; eassumption|intro Break; inversion Break; subst; eauto].
    + apply any_break_sound. intros. apply Item.
  - intros md [spec [e|]]; cbn [item_breaks fst snd].
    + eapply break_answer_iff with (observed := Breaks W md e);
        [split; [apply IBExpr|intro Break; now inversion Break]|apply Expr].
    + destruct spec as [[path|] [method|] [brackets|]];
        try solve [intro Impossible; inversion Impossible].
      destruct (resolve_sym W md path) as [u|] eqn:Resolve;
        [|intro Break; inversion Break; congruence].
      destruct (w_decl W u) as [d|] eqn:Decl;
        [|intro Break; inversion Break; subst; congruence].
      destruct (d_ast d) as [| | | | | | | |n ps needs outputs [bodies|]] eqn:Ast;
        try solve [intro Break; inversion Break; subst; congruence].
      eapply break_answer_iff with
        (observed := exists it, In it (concat bodies) /\ ItemBreaks W (d_mod d) it).
      * split.
        -- intros [it [Present Break]]. eapply IBApply; eassumption.
        -- intro Break. inversion Break as [|md' path' u' d' n' ps' needs' outputs' bodies' it
             Resolve' Decl' Ast' Present Child]; subst.
           rewrite Resolve in Resolve'. injection Resolve' as <-.
           rewrite Decl in Decl'. injection Decl' as <-.
           rewrite Ast in Ast'. inversion Ast'; subst. eauto.
      * apply any_break_sound. intros. apply Item.
Qed.

Theorem block_breaks_yes : forall W fuel md b,
  block_breaks W fuel md b = Some true -> BlockBreaks W md b.
Proof.
  intros W fuel md b Result. pose proof (proj1 (proj2 (break_scans_sound W fuel)) md b) as Checked.
  now rewrite Result in Checked.
Qed.

Theorem block_breaks_no : forall W fuel md b,
  block_breaks W fuel md b = Some false -> ~ BlockBreaks W md b.
Proof.
  intros W fuel md b Result. pose proof (proj1 (proj2 (break_scans_sound W fuel)) md b) as Checked.
  now rewrite Result in Checked.
Qed.
