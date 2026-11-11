(* One finite source-depth budget suffices for all dependency readings. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaMentionsSound EaDependencyFuel
  EaContextFuel EaResultSound EaFixedSound EaResultCoverage.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Section SourceDepth.
Variable W : world.
Variable depth : nat.
Hypothesis signatures_bounded : forall symbol, signature_depth W symbol <= depth.
Hypothesis context_depths : forall symbol d n items child,
  w_decl W symbol = Some d -> d_ast d = ADCtx n items ->
  In (CISpec child) items -> spec_depth child <= depth.
Hypothesis result_depths : forall symbol d n parameters arguments results body child,
  w_decl W symbol = Some d -> d_ast d = ADFunctor n parameters arguments results body ->
  In child results -> spec_depth child <= depth.

Lemma leaf_read_above : forall self p syntax x,
  DependencyLeaf W self p syntax x -> spec_depth syntax <= depth -> forall fuel stack,
  depth < fuel -> In x (result_mentions_go W fuel stack self p syntax).
Proof.
  intros self p syntax x Leaf Syntax. destruct Leaf; intros [|fuel] stack Enough; try lia.
  - cbn [result_mentions_go]. rewrite H. apply in_or_app. left.
    destruct H0 as [Val|Fn].
    + assert (Kind : kind_is W s DKVal = true) by (apply kind_is_spec; exact Val).
      rewrite Kind. apply unbound_complete; [|exact H2].
      destruct H1 as [Requirement|[NotVal Mention]]; [exact Requirement|congruence].
    + assert (Kind : kind_is W s DKFn = true) by (apply kind_is_spec; exact Fn).
      assert (NotVal : kind_is W s DKVal = false).
      { destruct (kind_is W s DKVal) eqn:Read; [apply kind_is_spec in Read; congruence|reflexivity]. }
      rewrite NotVal, Kind. apply unbound_complete; [|exact H2]. apply in_or_app.
      destruct H1 as [Requirement|[_ Mention]]; [left; exact Requirement|right].
      eapply sig_mentions_complete_above; [exact Mention|pose proof (signatures_bounded s); lia].
  - cbn [result_mentions_go]. rewrite H. apply in_or_app. left. apply unbound_complete; [|exact H1].
    apply in_or_app. left. destruct H0 as [[Equal Kind]|[NotType Member]].
    + subst t. assert (Read : kind_is W r DKType = true) by (apply kind_is_spec; exact Kind).
      rewrite Read. left. reflexivity.
    + destruct (kind_is W r DKType) eqn:Kind; [apply kind_is_spec in Kind; contradiction|exact Member].
  - cbn [result_mentions_go]. rewrite H. apply in_or_app. left. apply unbound_complete; [|exact H2].
    apply in_or_app. right. rewrite (method_named_complete W _ _ _ _ H0).
    eapply sig_mentions_complete_above; [exact H1|pose proof (signatures_bounded m); lia].
  - cbn [result_mentions_go]. destruct (resolve_sym W p pa) as [symbol|] eqn:Resolved; [|contradiction].
    apply in_or_app. right. apply in_flat_map. exists (bp, Spec (Some sp) dr sa).
    split; [exact H0|]. eapply free_in_complete_above; [exact H1|].
    pose proof (largest_member _ (fun b => spec_depth (snd b))
      (bracket_list a) (bp, Spec (Some sp) dr sa) H0) as Child.
    cbn [snd spec_depth] in Child. cbn [spec_depth] in Syntax.
    destruct a, sa; cbn [bracket_list type_depth largest fold_right] in *; unfold largest in *; cbn [fold_right] in *; lia.
Qed.

Lemma path_read_above : forall self p syntax x trace,
  ResultPath W self p syntax x trace -> NoDup trace -> spec_depth syntax <= depth ->
  forall fuel stack, depth + List.length trace < fuel ->
  (forall key, In key trace -> ~ In key stack) ->
  In x (result_mentions_go W fuel stack self p syntax).
Proof.
  intros self p syntax x trace Path. induction Path; intros Distinct Syntax [|fuel] stack Enough Apart; try lia.
  - eapply leaf_read_above; [exact H|exact Syntax|cbn [List.length] in Enough; lia].
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
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
    apply IHPath; [exact TailDistinct|eapply context_depths; eassumption|cbn [List.length] in Enough; lia|].
    intros key Member [Equal|Seen].
    + subst key. contradiction.
    + apply (Apart key (or_intror Member)), Seen.
  - inversion Distinct as [|head rest Fresh TailDistinct]; subst.
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
      apply IHPath; [exact TailDistinct|eapply result_depths; eassumption|cbn [List.length] in Enough; lia|].
      intros key Member [Equal|Seen].
      * subst key. contradiction.
      * apply (Apart key (or_intror Member)), Seen.
    + apply andb_true_iff. split; apply negb_true_iff.
      * destruct (nmem t (fun_outputs W (S fuel) u)) eqn:Output; [|reflexivity].
        exfalso. apply H4. eapply fun_outputs_sound. apply nmem_spec. exact Output.
      * destruct (nmem t (flat_map (fun name => match resolve_sym W (d_mod du) [name] with
          Some value => [value] | None => [] end) names)) eqn:Named; [|reflexivity].
        exfalso. apply H5, named_members, nmem_spec. exact Named.
Qed.

End SourceDepth.

Section FinitePaths.
Variable W : world.
Variable bound initial_self : nat.
Hypothesis declarations_bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.

Lemma path_pairs : forall self p syntax x trace,
  ResultPath W self p syntax x trace -> In self (reader_nodes bound initial_self) ->
  incl trace (reader_pairs bound initial_self).
Proof.
  intros self p syntax x trace Path. induction Path; intros Self key Member; [contradiction| |].
  - destruct Member as [<-|Member].
    + apply reader_pair; [exact Self|exact (declarations_bounded _ _ H0)].
    + exact (IHPath Self key Member).
  - assert (Node : In u (reader_nodes bound initial_self)).
    { right. apply in_seq. split; [lia|exact (declarations_bounded _ _ H0)]. }
    destruct Member as [<-|Member].
    + apply reader_pair; [exact Node|exact (declarations_bounded _ _ H0)].
    + exact (IHPath Node key Member).
Qed.

Lemma path_length : forall p syntax x trace,
  ResultPath W initial_self p syntax x trace -> NoDup trace -> List.length trace <= S bound * bound.
Proof.
  intros p syntax x trace Path Distinct.
  rewrite <- (reader_pairs_length bound initial_self).
  apply NoDup_incl_length; [exact Distinct|]. eapply path_pairs; [exact Path|left; reflexivity].
Qed.

End FinitePaths.

(* Only syntax consulted by a dependency reader contributes to this bound. *)
Definition declaration_depth (syntax : adecl) : nat :=
  match syntax with
  | ADFn _ _ parameters result _ =>
      Nat.max (largest (fun parameter => type_depth (snd parameter)) parameters)
        (match result with Some syntax => type_depth syntax | None => 0 end)
  | ADCtx _ items => largest spec_depth (ctx_specs items)
  | ADFunctor _ _ _ results _ => largest spec_depth results
  | _ => 0
  end.

Definition metadata_depth (W : world) (bound : nat) : nat :=
  Nat.max bound (largest (fun symbol => match w_decl W symbol with
    Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound)).

Definition metadata_budget (W : world) (bound : nat) : nat :=
  metadata_depth W bound + S bound * bound.

Section MetadataBounds.
Variable W : world.
Variable bound : nat.
Hypothesis declarations_bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.

Lemma declaration_depth_bounded : forall symbol d,
  w_decl W symbol = Some d -> declaration_depth (d_ast d) <= metadata_depth W bound.
Proof.
  intros symbol d Declaration.
  assert (Symbol : In symbol (seq 0 bound)).
  { apply in_seq. split; [lia|exact (declarations_bounded _ _ Declaration)]. }
  pose proof (largest_member _ (fun symbol => match w_decl W symbol with
    Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound) symbol Symbol) as Member.
  cbn in Member. rewrite Declaration in Member. unfold metadata_depth.
  eapply Nat.le_trans; [exact Member|apply Nat.le_max_r].
Qed.

Lemma signatures_depth_bounded : forall symbol,
  signature_depth W symbol <= metadata_depth W bound.
Proof.
  intro symbol. unfold signature_depth.
  destruct (w_decl W symbol) as [d|] eqn:Declaration; [|lia].
  pose proof (declaration_depth_bounded _ _ Declaration) as Bound.
  destruct (d_ast d); cbn [declaration_depth] in Bound; try exact Bound; lia.
Qed.

Lemma contexts_depth_bounded : forall symbol d n items child,
  w_decl W symbol = Some d -> d_ast d = ADCtx n items ->
  In (CISpec child) items -> spec_depth child <= metadata_depth W bound.
Proof.
  intros symbol d n items child Declaration Ast Member.
  pose proof (declaration_depth_bounded _ _ Declaration) as Bound.
  rewrite Ast in Bound. cbn [declaration_depth] in Bound.
  apply ctx_specs_members in Member.
  eapply Nat.le_trans; [exact (largest_member _ spec_depth _ _ Member)|exact Bound].
Qed.

Lemma results_depth_bounded : forall symbol d n parameters arguments results body child,
  w_decl W symbol = Some d -> d_ast d = ADFunctor n parameters arguments results body ->
  In child results -> spec_depth child <= metadata_depth W bound.
Proof.
  intros symbol d n parameters arguments results body child Declaration Ast Member.
  pose proof (declaration_depth_bounded _ _ Declaration) as Bound.
  rewrite Ast in Bound. cbn [declaration_depth] in Bound.
  eapply Nat.le_trans; [exact (largest_member _ spec_depth _ _ Member)|exact Bound].
Qed.

Theorem result_mentions_complete_above : forall fuel self p syntax x,
  metadata_budget W bound < fuel -> spec_depth syntax <= metadata_depth W bound ->
  ResultMentions W self p syntax x -> In x (result_mentions W fuel self p syntax).
Proof.
  intros fuel self p syntax x Enough Syntax Read.
  destruct (result_mentions_path W _ _ _ _ Read) as [trace [Path Distinct]].
  pose proof (path_length W bound self declarations_bounded _ _ _ _ Path Distinct) as Length.
  unfold result_mentions. eapply path_read_above.
  - exact signatures_depth_bounded.
  - exact contexts_depth_bounded.
  - exact results_depth_bounded.
  - exact Path.
  - exact Distinct.
  - exact Syntax.
  - unfold metadata_budget in Enough. lia.
  - intros key Member Empty. exact Empty.
Qed.

Theorem result_mentions_exact : forall fuel self p syntax,
  metadata_budget W bound < fuel -> spec_depth syntax <= metadata_depth W bound -> forall x,
  In x (result_mentions W fuel self p syntax) <-> ResultMentions W self p syntax x.
Proof.
  intros fuel self p syntax Enough Syntax x. split.
  - apply (result_mentions_sound W bound self declarations_bounded).
    unfold metadata_budget, metadata_depth in Enough.
    pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl W symbol with
      Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound))). lia.
  - apply result_mentions_complete_above; assumption.
Qed.

Theorem mentions_exact : forall fuel symbol,
  metadata_budget W bound < fuel -> forall x,
  In x (mentions W fuel symbol) <-> Mentions W symbol x.
Proof.
  intros fuel symbol Enough x. split.
  - apply (mentions_sound W bound declarations_bounded).
    unfold metadata_budget, metadata_depth in Enough.
    pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl W symbol with
      Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound))). lia.
  - intros (d & parameters & arguments & results & body & syntax & Declaration & Parts & Member & Read & NotOutput).
    unfold mentions, functor_decl. rewrite Declaration, Parts, sort_uniq_members.
    apply filter_In. split.
    + apply in_flat_map. exists syntax. split; [exact Member|].
      apply result_mentions_complete_above; [exact Enough| |exact Read].
      unfold functor_parts in Parts.
      destruct (d_ast d) eqn:Ast; try discriminate.
      injection Parts as <- <- <- <-. eapply results_depth_bounded; eassumption.
    + apply negb_true_iff. destruct (nmem x (fun_outputs W fuel symbol)) eqn:Output; [|reflexivity].
      exfalso. apply NotOutput. eapply fun_outputs_sound. apply nmem_spec. exact Output.
Qed.

Theorem poly_exact : forall fuel symbol,
  metadata_budget W bound < fuel -> forall x,
  In x (poly W fuel symbol) <-> Poly W symbol x.
Proof.
  intros fuel symbol Enough x. unfold poly.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|] eqn:Read.
  - destruct body as [bindings|].
    + split; [contradiction|].
      intros (found & name & params & inputs & outputs & Declaration & Ast & PolyRead).
      unfold functor_decl in Read. rewrite Declaration, Ast in Read. discriminate.
    + destruct parameters as [names|].
      * pose proof (poly_explicit_exact W symbol d names arguments results Read fuel x) as Exact.
        unfold poly in Exact. rewrite Read in Exact. exact Exact.
      * destruct (functor_decl_read _ _ _ _ _ _ _ Read) as [name [Declaration Ast]].
        rewrite (mentions_exact fuel symbol Enough x). split.
        -- intro Mention. exists d, name, None, arguments, results.
           split; [exact Declaration|]. split; [exact Ast|exact Mention].
        -- intros (found & found_name & params & inputs & outputs & Found & FoundAst & Mention).
           rewrite Declaration in Found. injection Found as <-.
           rewrite Ast in FoundAst. injection FoundAst as <- <- <- <-. exact Mention.
  - split; [contradiction|].
    intros (d & name & params & inputs & outputs & Declaration & Ast & PolyRead).
    unfold functor_decl in Read. rewrite Declaration, Ast in Read. discriminate.
Qed.

Theorem fixed_exact : forall fuel symbol,
  metadata_budget W bound < fuel -> forall x,
  In x (fixed W fuel symbol) <-> Fixed W symbol x.
Proof.
  intros fuel symbol Enough x. unfold fixed, Fixed. rewrite filter_In.
  rewrite (mentions_exact fuel symbol Enough x), negb_true_iff, nmem_false_spec.
  split; intros [Mention NotPoly]; split; [exact Mention| |exact Mention|].
  - intro PolyRead. apply NotPoly, (proj2 (poly_exact fuel symbol Enough x)), PolyRead.
  - intro PolyRead. apply NotPoly, (proj1 (poly_exact fuel symbol Enough x)), PolyRead.
Qed.

Theorem fixed_readings_exact : forall fuel symbol tm,
  metadata_budget W bound < fuel -> FixedReadings W symbol tm (fixed_readings W fuel symbol tm).
Proof.
  intros fuel symbol tm Enough x meaning. unfold fixed_readings.
  rewrite in_flat_map. split.
  - intros [source [Member Lookup]].
    destruct (nlook source tm) as [found|] eqn:Found; [|contradiction].
    destruct Lookup as [Equal|[]]. injection Equal as <- <-. split; [|exact Found].
    apply (proj1 (fixed_exact fuel symbol Enough source)), Member.
  - intros [Fixed Lookup]. exists x. split.
    + apply (proj2 (fixed_exact fuel symbol Enough x)), Fixed.
    + rewrite Lookup. left. reflexivity.
Qed.

End MetadataBounds.

From NM Require EaMain EaWorldSound.

Corollary bundle_world_r_fixed_readings : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> forall symbol tm,
  FixedReadings W symbol tm (fixed_readings W (S (metadata_budget W bound)) symbol tm).
Proof.
  intros bundle W bound Run symbol tm. apply (fixed_readings_exact W bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Run).
  - lia.
Qed.

Corollary bundle_world_r_dependency_readers_exact : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) ->
  let fuel := S (metadata_budget W bound) in
  (forall symbol x, In x (mentions W fuel symbol) <-> Mentions W symbol x) /\
  (forall symbol x, In x (poly W fuel symbol) <-> Poly W symbol x) /\
  (forall symbol x, In x (fixed W fuel symbol) <-> Fixed W symbol x).
Proof.
  intros bundle W bound Run. cbn zeta. split.
  - intro symbol. apply (mentions_exact W bound (EaWorldSound.bundle_world_r_bound _ _ _ Run)). lia.
  - split; intro symbol.
    + apply (poly_exact W bound (EaWorldSound.bundle_world_r_bound _ _ _ Run)). lia.
    + apply (fixed_exact W bound (EaWorldSound.bundle_world_r_bound _ _ _ Run)). lia.
Qed.
