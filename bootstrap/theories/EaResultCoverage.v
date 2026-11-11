(* Coverage of result dependencies: repeated declaration visits can be erased. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaMentionsSound EaDependencyFuel EaContextFuel EaResultSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Definition pair_eq_dec (a b : nat * nat) : {a=b}+{a<>b}.
Proof. decide equality; apply Nat.eq_dec. Defined.

Section ResultPaths.
Variable W : world.

(* Leaves contain no recursive context/functor dependency. *)
Inductive DependencyLeaf : nat -> string -> spec -> nat -> Prop :=
| LeafValFn : forall self p pa a s t,
    resolve_sym W p pa = Some s -> (kind_sym W s = Some DKVal \/ kind_sym W s = Some DKFn) ->
    (In t (w_reqs W s) \/ kind_sym W s = Some DKFn /\ SigMentions W s t) ->
    ~ (exists b, In b (bracket_list a) /\ resolve_sym W p (fst b) = Some t) ->
    DependencyLeaf self p (Spec (Some pa) None a) t
| LeafReceiver : forall self p pa dn a r t,
    resolve_sym W p pa = Some r ->
    (t = r /\ kind_sym W r = Some DKType \/ kind_sym W r <> Some DKType /\ In t (w_reqs W r)) ->
    ~ (exists b, In b (bracket_list a) /\ resolve_sym W p (fst b) = Some t) ->
    DependencyLeaf self p (Spec (Some pa) (Some dn) a) t
| LeafMeth : forall self p pa dn a r m t,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m -> SigMentions W m t ->
    ~ (exists b, In b (bracket_list a) /\ resolve_sym W p (fst b) = Some t) ->
    DependencyLeaf self p (Spec (Some pa) (Some dn) a) t
| LeafBracket : forall self p pa d a bp sp dr sa t,
    resolve_sym W p pa <> None ->
    In (bp, Spec (Some sp) dr sa) (bracket_list a) ->
    FreeIn W (w_reqs W) p (ATRef sp sa) t ->
    DependencyLeaf self p (Spec (Some pa) d a) t.

(* The pair records the same identity that the executable puts on its stack. *)
Inductive ResultPath : nat -> string -> spec -> nat -> list (nat * nat) -> Prop :=
| PathLeaf : forall self p syntax x, DependencyLeaf self p syntax x -> ResultPath self p syntax x []
| PathCtx : forall self p pa a c dc n its sp t tail,
    resolve_sym W p pa = Some c -> w_decl W c = Some dc -> d_ast dc = ADCtx n its ->
    In (CISpec sp) its -> ResultPath self (d_mod dc) sp t tail ->
    ~ (exists b, In b (bracket_list a) /\ resolve_sym W p (fst b) = Some t) ->
    ResultPath self p (Spec (Some pa) None a) t ((self,c) :: tail)
| PathFunctor : forall self p pa a u du n names args res sp t tail,
    resolve_sym W p pa = Some u -> w_decl W u = Some du ->
    d_ast du = ADFunctor n (Some names) args res None -> u <> self ->
    In sp res -> ResultPath u (d_mod du) sp t tail -> ~ FunOutputs W u t ->
    ~ (exists name, In name names /\ resolve_sym W (d_mod du) [name] = Some t) ->
    ~ (exists b, In b (bracket_list a) /\ resolve_sym W p (fst b) = Some t) ->
    ResultPath self p (Spec (Some pa) None a) t ((u,u) :: tail).

Definition NodeTail (key : nat * nat) (x : nat) (tail : list (nat * nat)) : Prop :=
  (exists d n items child,
    w_decl W (snd key) = Some d /\ d_ast d = ADCtx n items /\
    In (CISpec child) items /\ ResultPath (fst key) (d_mod d) child x tail) \/
  (fst key = snd key /\ exists d n names args results child,
    w_decl W (snd key) = Some d /\ d_ast d = ADFunctor n (Some names) args results None /\
    In child results /\ ResultPath (snd key) (d_mod d) child x tail /\
    ~ FunOutputs W (snd key) x /\
    ~ (exists name, In name names /\ resolve_sym W (d_mod d) [name] = Some x)).

Lemma path_occurrence : forall self p syntax x trace,
  ResultPath self p syntax x trace -> NoDup trace -> forall key, In key trace ->
  exists tail, NodeTail key x tail /\ NoDup (key :: tail).
Proof.
  intros self p syntax x trace Path. induction Path; intros Distinct key Member; [contradiction| |].
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
    destruct Member as [<-|Member].
    + exists tail. split; [left; cbn; exists dc, n, its, sp; repeat split; assumption|exact Distinct].
    + exact (IHPath TailDistinct key Member).
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
    destruct Member as [<-|Member].
    + exists tail. split; [right; cbn; split; [reflexivity|]| exact Distinct].
      exists du, n, names, args, res, sp. repeat split; assumption.
    + exact (IHPath TailDistinct key Member).
Qed.

Theorem result_mentions_path : forall self p syntax x,
  ResultMentions W self p syntax x -> exists trace, ResultPath self p syntax x trace /\ NoDup trace.
Proof.
  intros self p syntax x Read. induction Read.
  - exists []. split; [apply PathLeaf; eapply LeafValFn; eassumption|constructor].
  - exists []. split; [apply PathLeaf; eapply LeafReceiver; eassumption|constructor].
  - exists []. split; [apply PathLeaf; eapply LeafMeth; eassumption|constructor].
  - exists []. split; [apply PathLeaf; eapply LeafBracket; eassumption|constructor].
  - destruct IHRead as [tail [Path Distinct]].
    destruct (in_dec pair_eq_dec (self,c) tail) as [Repeated|Fresh].
    + destruct (path_occurrence _ _ _ _ _ Path Distinct _ Repeated) as [suffix [Node NoDup]].
      destruct Node as [(d' & n' & its' & child & Declaration & Ast & Member & Child)|
        [Same (d' & n' & names & args & results & child & Declaration & Ast & Rest)]].
      * cbn in *. rewrite H0 in Declaration. injection Declaration as <-.
        rewrite H1 in Ast. injection Ast as <- <-.
        exists ((self,c) :: suffix). split; [eapply PathCtx; eassumption|exact NoDup].
      * cbn in *. rewrite H0 in Declaration. injection Declaration as <-.
        rewrite H1 in Ast. discriminate.
    + exists ((self,c) :: tail). split; [eapply PathCtx; eassumption|constructor; assumption].
  - destruct IHRead as [tail [Path Distinct]].
    destruct (in_dec pair_eq_dec (u,u) tail) as [Repeated|Fresh].
    + destruct (path_occurrence _ _ _ _ _ Path Distinct _ Repeated) as [suffix [Node NoDup]].
      destruct Node as [(d' & n' & items & child & Declaration & Ast & Rest)|
        [Same (d' & n' & names' & args' & results & child & Declaration & Ast & Member & Child & Output & Named)]].
      * cbn in *. rewrite H0 in Declaration. injection Declaration as <-.
        rewrite H1 in Ast. discriminate.
      * cbn in *. rewrite H0 in Declaration. injection Declaration as <-.
        rewrite H1 in Ast. injection Ast as <- <- <- <-.
        exists ((u,u) :: suffix). split; [eapply PathFunctor; eassumption|exact NoDup].
    + exists ((u,u) :: tail). split; [eapply PathFunctor; eassumption|constructor; assumption].
Qed.

Lemma method_named_complete : forall p receiver name method,
  MethodNamed W p receiver name method -> method_named W p receiver name = Some method.
Proof.
  intros p receiver name method [Attached|[Absent Scope]]; unfold method_named.
  - unfold AuditBase.Semantics.Elaboration.Attached in Attached. rewrite Attached. reflexivity.
  - destruct (w_attached W receiver name) as [other|] eqn:Found.
    + exfalso. apply (Absent other). exact Found.
    + rewrite Scope. reflexivity.
Qed.

Lemma unbound_complete : forall p application values x,
  In x values ->
  ~ (exists b, In b (bracket_list application) /\ resolve_sym W p (fst b) = Some x) ->
  In x (filter (fun t => negb (nmem t (bracket_bound W p application))) values).
Proof.
  intros p application values x Member Apart. apply filter_In. split; [exact Member|].
  apply negb_true_iff. destruct (nmem x (bracket_bound W p application)) eqn:Seen; [|reflexivity].
  exfalso. apply Apart, bracket_bound_members, nmem_spec. exact Seen.
Qed.

Lemma leaf_eventually_read : forall self p syntax x,
  DependencyLeaf self p syntax x -> exists needed, forall fuel stack,
    needed <= fuel -> In x (result_mentions_go W fuel stack self p syntax).
Proof.
  intros self p syntax x Leaf. destruct Leaf.
  - exists (S (signature_depth W s)). intros [|fuel] stack Enough; [lia|].
    cbn [result_mentions_go]. rewrite H. apply in_or_app. left.
    destruct H0 as [Val|Fn].
    + assert (Kind : kind_is W s DKVal = true) by (apply kind_is_spec; exact Val).
      rewrite Kind. apply unbound_complete; [|exact H2]. destruct H1 as [Requirement|[NotVal Mention]]; [exact Requirement|congruence].
    + assert (Kind : kind_is W s DKFn = true) by (apply kind_is_spec; exact Fn).
      assert (NotVal : kind_is W s DKVal = false).
      { destruct (kind_is W s DKVal) eqn:Read; [apply kind_is_spec in Read; congruence|reflexivity]. }
      rewrite NotVal, Kind. apply unbound_complete; [|exact H2]. apply in_or_app.
      destruct H1 as [Requirement|[_ Mention]]; [left; exact Requirement|right].
      eapply sig_mentions_complete_above; [exact Mention|lia].
  - exists 1. intros [|fuel] stack Enough; [lia|].
    cbn [result_mentions_go]. rewrite H. apply in_or_app. left. apply unbound_complete; [|exact H1].
    apply in_or_app. left. destruct H0 as [[Equal Kind]|[NotType Member]].
    + subst t. assert (Read : kind_is W r DKType = true) by (apply kind_is_spec; exact Kind).
      rewrite Read. left. reflexivity.
    + destruct (kind_is W r DKType) eqn:Kind; [apply kind_is_spec in Kind; contradiction|exact Member].
  - exists (S (signature_depth W m)). intros [|fuel] stack Enough; [lia|].
    cbn [result_mentions_go]. rewrite H. apply in_or_app. left. apply unbound_complete; [|exact H2].
    apply in_or_app. right. rewrite (method_named_complete _ _ _ _ H0).
    eapply sig_mentions_complete_above; [exact H1|lia].
  - exists (S (type_depth (ATRef sp sa))). intros [|fuel] stack Enough; [lia|].
    cbn [result_mentions_go]. destruct (resolve_sym W p pa) as [symbol|] eqn:Resolved; [|contradiction].
    apply in_or_app. right. apply in_flat_map. exists (bp, Spec (Some sp) dr sa).
    split; [exact H0|]. eapply free_in_complete_above; [exact H1|lia].
Qed.

Lemma path_eventually_read : forall self p syntax x trace,
  ResultPath self p syntax x trace -> NoDup trace ->
  exists needed, forall fuel stack, needed <= fuel ->
    (forall key, In key trace -> ~ In key stack) ->
    In x (result_mentions_go W fuel stack self p syntax).
Proof.
  intros self p syntax x trace Path. induction Path; intro Distinct.
  - destruct (leaf_eventually_read _ _ _ _ H) as [needed Read].
    exists needed. intros fuel stack Enough Apart. exact (Read fuel stack Enough).
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
    destruct (IHPath TailDistinct) as [needed Read]. exists (S needed).
    intros [|fuel] stack Enough Apart; [lia|].
    cbn [result_mentions_go]. rewrite H.
    assert (Kind : kind_sym W c = Some DKCtx).
    { unfold kind_sym. rewrite H0. cbn. rewrite H1. reflexivity. }
    assert (NotVal : kind_is W c DKVal = false).
    { unfold kind_is. rewrite Kind. reflexivity. }
    assert (NotFn : kind_is W c DKFn = false).
    { unfold kind_is. rewrite Kind. reflexivity. }
    rewrite NotVal, NotFn, H0, H1. apply in_or_app. left.
    assert (NotSeen : pmem (self,c) stack = false).
    { apply pmem_false_spec. apply Apart. left. reflexivity. }
    rewrite NotSeen. apply unbound_complete; [|exact H3].
    apply in_flat_map. exists sp. split; [apply ctx_specs_members; exact H2|].
    apply Read; [lia|]. intros key Member [Equal|Seen].
    + subst key. contradiction.
    + apply (Apart key (or_intror Member)), Seen.
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
    destruct (IHPath TailDistinct) as [needed Read]. exists (S needed).
    intros [|fuel] stack Enough Apart; [lia|].
    cbn [result_mentions_go]. rewrite H.
    assert (Kind : kind_sym W u = Some DKFunctor).
    { unfold kind_sym. rewrite H0. cbn. rewrite H1. reflexivity. }
    assert (NotVal : kind_is W u DKVal = false).
    { unfold kind_is. rewrite Kind. reflexivity. }
    assert (NotFn : kind_is W u DKFn = false).
    { unfold kind_is. rewrite Kind. reflexivity. }
    rewrite NotVal, NotFn, H0, H1. apply in_or_app. left.
    assert (Other : Nat.eqb u self = false) by (apply Nat.eqb_neq; exact H2).
    assert (NotSeen : pmem (u,u) stack = false).
    { apply pmem_false_spec. apply Apart. left. reflexivity. }
    rewrite Other, NotSeen. cbn [orb]. apply unbound_complete; [|exact H6].
    apply filter_In. split.
    + apply in_flat_map. exists sp. split; [exact H3|].
      apply Read; [lia|]. intros key Member [Equal|Seen].
      * subst key. contradiction.
      * apply (Apart key (or_intror Member)), Seen.
    + apply andb_true_iff. split; apply negb_true_iff.
      * destruct (nmem t (fun_outputs W (S fuel) u)) eqn:Output; [|reflexivity].
        exfalso. apply H4. eapply fun_outputs_sound. apply nmem_spec. exact Output.
      * destruct (nmem t (flat_map (fun name => match resolve_sym W (d_mod du) [name] with
          Some value => [value] | None => [] end) names)) eqn:Named; [|reflexivity].
        exfalso. apply H5, named_members, nmem_spec. exact Named.
Qed.

Theorem result_mentions_eventually_complete : forall self p syntax x,
  ResultMentions W self p syntax x -> exists needed, forall fuel,
  needed <= fuel -> In x (result_mentions W fuel self p syntax).
Proof.
  intros self p syntax x Read.
  destruct (result_mentions_path _ _ _ _ Read) as [trace [Path Distinct]].
  destruct (path_eventually_read _ _ _ _ _ Path Distinct) as [needed Reads].
  exists needed. intros fuel Enough. unfold result_mentions. apply Reads; [exact Enough|].
  intros key Member Empty. exact Empty.
Qed.

End ResultPaths.

Theorem mentions_eventually_complete : forall W symbol x,
  Mentions W symbol x -> exists needed, forall fuel,
  needed <= fuel -> In x (mentions W fuel symbol).
Proof.
  intros W symbol x (d & parameters & arguments & results & body & syntax & Declaration & Parts & Member & Read & NotOutput).
  destruct (result_mentions_eventually_complete W _ _ _ _ Read) as [needed Covered].
  exists needed. intros fuel Enough. unfold mentions, functor_decl.
  rewrite Declaration, Parts, sort_uniq_members. apply filter_In. split.
  - apply in_flat_map. exists syntax. split; [exact Member|apply Covered; exact Enough].
  - apply negb_true_iff. destruct (nmem x (fun_outputs W fuel symbol)) eqn:Output; [|reflexivity].
    exfalso. apply NotOutput. eapply fun_outputs_sound. apply nmem_spec. exact Output.
Qed.
