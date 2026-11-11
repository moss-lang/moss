(* Root-context formation consumes exact fixed-input and main readings.
   These hypotheses are explicit until actual environment initialization is
   connected to the dependency budget. No evaluator or Audit rule changes. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit AuditFacts EaBase EaTypes EaCtx EaTypesSound EaMentionsSound EaDependencyFuel.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Theorem lang_sym_complete : forall W short ns name symbol,
  LangSym W short ns name symbol -> lang_sym W short ns name = Some symbol.
Proof.
  intros W short ns name symbol (d & p & Declaration & Library & Scope).
  unfold LibraryModule in Library. unfold lang_sym.
  destruct (w_library W) as [directory|] eqn:Found; [|contradiction].
  subst p. rewrite Scope, Declaration. reflexivity.
Qed.

Theorem lang_item_sym_complete : forall W item symbol,
  LangItem W item symbol -> lang_item_sym W item = Some symbol.
Proof.
  intros W item symbol. unfold LangItem, lang_item_sym.
  destruct (lang_item_key item) as [[short ns] name]. apply lang_sym_complete.
Qed.

Lemma ascending_nodup : forall xs, Ascending xs -> NoDup xs.
Proof.
  intro xs. induction xs as [|x xs IH]; intro Sorted; [constructor|].
  constructor.
  - intro Member. pose proof (ascending_lt _ _ Sorted x Member). lia.
  - apply IH. exact (ascending_tail _ _ Sorted).
Qed.

Lemma mentions_nodup : forall W fuel u, NoDup (mentions W fuel u).
Proof.
  intros W fuel u. unfold mentions.
  destruct (functor_decl W u) as [[d [[[parameters arguments] results] body]]|].
  - apply ascending_nodup, sort_uniq_ascending.
  - constructor.
Qed.

Lemma fixed_nodup : forall W fuel u, NoDup (fixed W fuel u).
Proof. intros W fuel u. unfold fixed. apply NoDup_filter, mentions_nodup. Qed.

Lemma has_nfun_spec : forall u entries,
  has_nfun u entries = true <-> exists fx, In (NFun u fx) entries.
Proof.
  intros u entries. unfold has_nfun. rewrite existsb_exists. split.
  - intros (e & Member & Checked). destruct e; try discriminate.
    apply Nat.eqb_eq in Checked. subst. eauto.
  - intros (fx & Member). exists (NFun u fx). split; [exact Member|].
    cbn. apply Nat.eqb_refl.
Qed.

Section Roots.
Variable E : env.
Let W := e_W E.
Let fuel := e_metadata_fuel E.
Hypothesis FixedExact : forall u x, In x (fixed W fuel u) <-> Fixed W u x.
Hypothesis FixedReads : forall u tm, FixedReadings W u tm (fixed_readings W fuel u tm).

Theorem root_items_sound : forall q its es0 tm entries meanings,
  root_items E q es0 tm its = Ok (entries, meanings) ->
  RootItems W q es0 tm its entries meanings.
Proof.
  intros q its. induction its as [|sp rest IH]; intros es0 tm entries meanings Run.
  - cbn [root_items] in Run. injection Run as <- <-. constructor.
  - cbn [root_items] in Run. fold W fuel in Run.
    assert (Skip :
      (match sp with
       | CISpec (Spec (Some path) None _) => forall u,
           resolve_sym W q path = Some u -> kind_sym W u = Some DKFunctor ->
           exists fx, In (NFun u fx) es0
       | _ => True end) ->
      (rbind (root_items E q es0 tm rest)
        (fun result => let '(es, tm') := result in Ok (es, tm')) = Ok (entries, meanings)) ->
      RootItems W q es0 tm (sp :: rest) entries meanings).
    { intros Permitted Step. apply rbind_ok in Step as [[tail types] [Recursive Finish]].
      injection Finish as <- <-. apply RISkip; [exact Permitted|].
      exact (IH _ _ _ _ Recursive). }
    destruct sp as [[[path|] [detached|] brackets]|left right];
      try (apply Skip; [exact I|exact Run]).
    destruct (resolve_sym W q path) as [u|] eqn:Resolved.
    2: { apply Skip; [|exact Run]. intros other Equal. congruence. }
    destruct (w_decl W u) as [d|] eqn:Declaration.
    2: { apply Skip; [|exact Run]. intros other Equal Kind. injection Equal as <-.
         unfold kind_sym in Kind. rewrite Declaration in Kind. discriminate. }
    destruct (d_ast d) as [items declarations|name|name syntax|name parameters syntax|
      name|name syntax initial|receiver name parameters result body|name items|
      name parameters inputs outputs body] eqn:Ast;
      try (apply Skip; [|exact Run]; intros other Equal Kind; injection Equal as <-;
        unfold kind_sym in Kind; rewrite Declaration, Ast in Kind;
        try discriminate; destruct receiver; discriminate).
    destruct body as [body|].
    + destruct (has_nfun u es0) eqn:Present; [|discriminate].
      apply Skip; [|exact Run]. intros other Equal Kind. injection Equal as <-.
      apply has_nfun_spec. exact Present.
    + destruct (has_nfun u es0) eqn:Present.
      * apply Skip; [|exact Run]. intros other Equal Kind. injection Equal as <-.
        apply has_nfun_spec. exact Present.
      * apply rbind_ok in Run as [[tail types] [Recursive Finish]]. injection Finish as <- <-.
        eapply RIFun with (u:=u) (du:=d) (n:=name) (ps:=parameters)
          (args:=inputs) (res:=outputs) (xs:=fixed W fuel u)
          (fx:=fixed_readings W fuel u (add_tys (fixed W fuel u) tm)).
        -- exact Resolved.
        -- exact Declaration.
        -- exact Ast.
        -- intro Member. apply has_nfun_spec in Member. congruence.
        -- apply fixed_nodup.
        -- exact (FixedExact u).
        -- apply FixedReads.
        -- exact (IH _ _ _ _ Recursive).
Qed.

Hypothesis MainExact : forall s, In s (e_mains E) <-> IsMainFn W s.

Theorem root_ctx_sound : forall s es0 tm entries meanings,
  root_ctx E s es0 tm = Ok (entries, meanings) ->
  RootCtx W s es0 tm entries meanings.
Proof.
  intros s es0 tm entries meanings Run. unfold root_ctx in Run. fold W in Run.
  destruct (nmem s (e_mains E)) eqn:Main.
  2: { injection Run as <- <-. apply RootNone. left. intro IsMain.
       apply MainExact, nmem_spec in IsMain. congruence. }
  assert (IsMain : IsMainFn W s) by (apply MainExact, nmem_spec; exact Main).
  destruct (lang_item_sym W LIRoot) as [gc|] eqn:Root.
  2: { injection Run as <- <-. apply RootNone. right.
       intros g d n its Item. apply lang_item_sym_complete in Item. congruence. }
  destruct (w_decl W gc) as [d|] eqn:Declaration.
  2: { injection Run as <- <-. apply RootNone. right.
       intros g found n its Item Found. apply lang_item_sym_complete in Item.
       rewrite Root in Item. injection Item as <-. congruence. }
  destruct (d_ast d) as [items declarations|name|name syntax|name parameters syntax|
      name|name syntax initial|receiver name parameters result body|name items|
      name parameters inputs outputs body] eqn:Ast;
    try (injection Run as <- <-; apply RootNone; right;
      intros g found n its Item Found; apply lang_item_sym_complete in Item;
      rewrite Root in Item; injection Item as <-; rewrite Declaration in Found;
      injection Found as <-; rewrite Ast; discriminate).
  eapply RootGc; [exact IsMain|apply lang_item_sym_sound; exact Root|
    exact Declaration|exact Ast|].
  exact (root_items_sound _ _ _ _ _ _ Run).
Qed.
End Roots.

From NM Require Import EaResultBudget EaMetadataSound EaMainSymbols EaMain EaWorldSound ElabWorld.

Theorem root_ctx_sound_at_budget : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  (forall s, In s (e_mains E) <-> IsMainFn (e_W E) s) ->
  forall s es0 tm entries meanings,
  root_ctx E s es0 tm = Ok (entries, meanings) ->
  RootCtx (e_W E) s es0 tm entries meanings.
Proof.
  intros E bound Bounded Enough Main s es0 tm entries meanings Run.
  apply (root_ctx_sound E); [| |exact Main|exact Run].
  - intro symbol. apply (EaResultBudget.fixed_exact _ bound Bounded); exact Enough.
  - intros symbol types. apply (fixed_readings_exact _ bound Bounded); exact Enough.
Qed.

Corollary bundle_world_r_root_ctx_sound : forall b W bound,
  EaMain.bundle_world_r b = Ok (W, bound) -> forall contexts strategy,
  let modules := map am_path (b_mods b) in
  let E := Env (ElabWorld.with_context W contexts) strategy modules
    (EaMain.main_symbols W modules) EaMain.elab_fuel (EaMain.metadata_fuel W bound) in
  forall s es0 tm entries meanings,
  root_ctx E s es0 tm = Ok (entries, meanings) ->
  RootCtx (e_W E) s es0 tm entries meanings.
Proof.
  intros b W bound Run contexts strategy. cbn zeta.
  apply (root_ctx_sound_at_budget _ bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Run).
  - exact (metadata_fuel_sufficient W bound).
  - exact (bundle_world_r_main_symbols_exact _ _ _ Run).
Qed.
