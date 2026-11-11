(* Declaration checking uses the typed readers and verifies coverage of
   every declared functor output. Constant bodies are checked by assembly. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaBudget EaTypes EaTypesSound.
From NM Require Import LegacyBodyJudgments.
From NM Require EaMain EaCtx EaFamily EaDecide EaEntriesSound EaOutputSound
  EaContextTableSound EaResultLeafSound EaDependencyFuel EaContextReadFacts
  EaMetadataSound EaWorldSound ElabWorld WorldSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma context_table_functor_complete : forall E bound symbol context,
  symbol < bound -> kind_sym (e_W E) symbol = Some DKFunctor ->
  EaCtx.fun_ctx E symbol = Ok context ->
  NMap.nget (EaCtx.context_table E bound) symbol = Some context.
Proof.
  intros E bound symbol context Bounded Kind Computed. unfold EaCtx.context_table.
  assert (Fold : forall symbols table,
    In symbol symbols \/ NMap.nget table symbol = Some context ->
    NMap.nget (fold_left (fun table s =>
      match kind_sym (e_W E) s with
      | Some DKFn | Some DKMeth | Some DKVal =>
          match EaCtx.assume_ctx E s with Ok result => NMap.nset table s result | Err _ => table end
      | Some DKFunctor =>
          match EaCtx.fun_ctx E s with Ok result => NMap.nset table s result | Err _ => table end
      | _ => table
      end) symbols table) symbol = Some context).
  {
    intros symbols. induction symbols as [|s rest IH]; intros table Found; [now destruct Found|].
    cbn [fold_left]. apply IH.
    destruct (Nat.eq_dec s symbol) as [->|Different].
    - rewrite Kind, Computed. right. apply WorldSound.nget_nset_same.
    - destruct Found as [[Equal|Present]|Read]; [contradiction|now left|].
      right. destruct (kind_sym (e_W E) s) as [[]|]; try exact Read;
        lazymatch goal with |- context [match ?compute with Ok _ => _ | Err _ => _ end] =>
          destruct compute
        end;
        try exact Read; rewrite WorldSound.nget_nset_other by congruence; exact Read.
  }
  apply Fold. left. apply in_seq. lia.
Qed.

Local Lemma as_written_self_all : forall E bound,
  EaMain.self_all E bound =
  as_written (filter (fun t => kind_is (e_W E) t DKType) (seq 0 bound)).
Proof. reflexivity. Qed.

Local Lemma body_binds_sound : forall E p syntax k,
  In k (EaMain.body_binds E p syntax) -> BodyBinds (e_W E) p syntax k.
Proof.
  intros E p [[[pa|] dn br] [y|]] k Found; cbn [EaMain.body_binds] in Found; try contradiction.
  - destruct (resolve_sym (e_W E) p pa) as [s|] eqn:Resolve; [|contradiction].
    destruct Found as [<-|[]]. now apply BBItem.
  - destruct (resolve_sym (e_W E) p pa) as [s|] eqn:Resolve; [|contradiction].
    destruct (kind_is (e_W E) s DKFunctor) eqn:Kind; [|contradiction].
    eapply BBApply; [exact Resolve|now apply kind_is_spec|].
    eapply EaResultLeafSound.result_leaves_sound. exact Found.
Qed.

Local Lemma total_body : forall E u d n ps a r bodies C C' ks,
  w_decl (e_W E) u = Some d -> d_ast d = ADFunctor n ps a r (Some bodies) ->
  EaFamily.provide_result E u C (fun t => TAbs t) = Ok (C', ks) ->
  filter (fun k => negb (existsb (EaDecide.leaf_eqb k)
    (flat_map (EaMain.body_binds E (d_mod d)) (List.concat bodies))))
    (EaDecide.result_leaves (e_W E) (e_fuel E) u) = [] ->
  FunctorTotal (e_W E) u.
Proof.
  intros E u d n ps a r bodies C C' ks Declared Ast Results NoMissing.
  intros d' n' ps' a' r' bodies' k Declared' Ast' Leaf.
  rewrite Declared in Declared'. injection Declared' as <-.
  rewrite Ast in Ast'. injection Ast' as <- <- <- <- <-.
  assert (Promised : In k (EaDecide.result_leaves (e_W E) (e_fuel E) u)).
  { eapply EaResultLeafSound.result_leaves_covered_after_read; eassumption. }
  destruct (existsb (EaDecide.leaf_eqb k)
    (flat_map (EaMain.body_binds E (d_mod d)) (List.concat bodies))) eqn:Bound.
  - apply existsb_exists in Bound as (actual & Written & Match).
    apply EaResultLeafSound.leaf_eqb_spec in Match. subst actual.
    apply in_flat_map in Written as (syntax & Present & Binds).
    exists syntax. split; [exact Present|eapply body_binds_sound; exact Binds].
  - assert (Missing : In k (filter (fun key => negb (existsb (EaDecide.leaf_eqb key)
        (flat_map (EaMain.body_binds E (d_mod d)) (List.concat bodies))))
        (EaDecide.result_leaves (e_W E) (e_fuel E) u))).
    { apply filter_In. split; [exact Promised|now rewrite Bound]. }
    now rewrite NoMissing in Missing.
Qed.

Section Declarations.
Variable E : env.
Variable bound : nat.
Let W := e_W E.
Hypotheses (Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound)
  (Enough : metadata_budget W bound < e_metadata_fuel E)
  (Contexts : EaContextTableSound.ContextTableSound W)
  (FunctorContexts : forall symbol C ns, symbol < bound -> kind_sym W symbol = Some DKFunctor ->
    EaCtx.fun_ctx E symbol = Ok (C, ns) -> w_context W symbol = Some (C, ns)).

Theorem decl_forms_sound : forall P symbol,
  (forall d n t i, w_decl W symbol = Some d -> d_ast d = ADVal n t (Some i) -> ConstElab W P d t i) ->
  EaMain.decl_forms E bound symbol = Ok tt -> DeclForms W P symbol.
Proof.
  intros P symbol Constants Run d Declared.
  unfold EaMain.decl_forms in Run. cbv zeta in Run. fold W in Run. rewrite Declared in Run.
  destruct (d_ast d) as [assumes declarations|n|n t|n ps t|n|n t initial|
    recv n ps ret body|n items|n ps a r bodies] eqn:Ast; try exact I.
  - apply rbind_ok in Run as (ignored & _ & Run).
    apply rbind_ok in Run as (tau & Read & _).
    rewrite as_written_self_all in Read. do 2 eexists. eapply ty_of_sound; exact Read.
  - apply rbind_ok in Run as (fields & Fields & Run).
    apply rbind_ok in Run as (tau & Read & _).
    rewrite as_written_self_all in Read. do 2 eexists. eapply ty_of_sound; exact Read.
  - apply rbind_ok in Run as (tau & Read & _).
    rewrite as_written_self_all in Read. split.
    + do 2 eexists. eapply ty_of_sound; exact Read.
    + intros i Eq. subst initial. eapply Constants; eassumption.
  - destruct recv.
    + apply rbind_ok in Run as (g & Read & _).
      rewrite as_written_self_all in Read. do 2 eexists. eapply sig_of_sound; exact Read.
    + apply rbind_ok in Run as (g & Read & _).
      rewrite as_written_self_all in Read. do 3 eexists. eapply sig_of_sound; exact Read.
    + apply rbind_ok in Run as (g & Read & _).
      rewrite as_written_self_all in Read. do 3 eexists. eapply sig_of_sound; exact Read.
  - apply rbind_ok in Run as ([entries meanings] & Read & _).
    rewrite as_written_self_all in Read. do 3 eexists.
    eapply (proj2 (EaEntriesSound.entries_sound_at_budget E bound Bounded Enough (e_fuel E))).
    exact Read.
  - assert (Kind : kind_sym W symbol = Some DKFunctor) by
      (unfold kind_sym; now rewrite Declared, Ast).
    apply rbind_ok in Run as ([C ns] & Read & Run).
    assert (Recorded : w_context W symbol = Some (C, ns)).
    {
      destruct (w_context W symbol) as [[C' ns']|] eqn:Context; [now injection Read as <- <-|].
      rewrite <- Context. eapply FunctorContexts; [eapply Bounded; exact Declared|exact Kind|exact Read].
    }
    pose proof (Contexts _ _ _ Recorded) as Functor. rewrite Kind in Functor.
    apply rbind_ok in Run as ([C' keys] & Result & Run).
    pose proof (EaOutputSound.provide_result_sound_at_budget E bound Bounded Enough _ _ _ _ _ Result)
      as Output. rewrite as_written_self_all in Output.
    exists C, ns, (filter (fun t => kind_is W t DKType) (seq 0 bound)), C', keys. split; [exact Functor|split; [exact Output|]].
    destruct bodies as [bodies|].
    + match type of Run with
      | context [match ?missing with [] => _ | _ :: _ => _ end] =>
          destruct missing as [|head tail] eqn:Coverage; [|discriminate]
      end.
      eapply total_body; [exact Declared|exact Ast|exact Result|exact Coverage].
    + intros d' n' ps' a' r' bodies' k Declared' Ast' _.
      rewrite Declared in Declared'. injection Declared' as <-. congruence.
Qed.

Theorem decls_form_sound : forall P,
  (forall symbol d n t i, w_decl W symbol = Some d -> d_ast d = ADVal n t (Some i) -> ConstElab W P d t i) ->
  EaMain.decls_form E bound = Ok tt -> DeclsForm W P.
Proof.
  intros P Constants Run symbol d Declared.
  unfold EaMain.decls_form in Run. apply rbind_ok in Run as (checked & Checks & _).
  assert (Present : In symbol (seq 0 bound)) by
    (apply in_seq; pose proof (Bounded _ _ Declared); lia).
  destruct (EaDependencyFuel.rmap_member_read _ _ _ _ _ Checks _ Present) as (u & Read).
  destruct (EaMain.decl_forms E bound symbol) as [[]|message] eqn:Checked; [|discriminate Read].
  exact (decl_forms_sound P symbol (Constants symbol) Checked d Declared).
Qed.
End Declarations.

Theorem elaborate_declarations_sound : forall strategy bundle W P,
  EaMain.elaborate_with strategy bundle = Ok (W, P) ->
  (forall symbol d n t i, w_decl W symbol = Some d -> d_ast d = ADVal n t (Some i) -> ConstElab W P d t i) ->
  DeclsForm W P.
Proof.
  intros strategy bundle W P Run Constants.
  pose proof (EaContextTableSound.elaborate_with_context_table_sound _ _ _ _ Run) as Contexts.
  unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata in Run.
  destruct (EaMain.bundle_world_r bundle) as [[W0 bound]|message] eqn:World; [|discriminate].
  set (modules := map am_path (b_mods bundle)) in Run.
  set (mains := EaMain.main_symbols W0 modules) in Run.
  set (metadata := EaMain.metadata_fuel W0 bound) in Run.
  set (E0 := Env W0 strategy modules mains EaMain.elab_fuel metadata) in Run.
  set (table := NMap.nget (EaCtx.context_table E0 bound)) in Run.
  set (W' := ElabWorld.with_context W0 table) in Run.
  set (E := Env W' strategy modules mains EaMain.elab_fuel metadata) in Run.
  destruct (EaMain.assumes_complete E bound) as [[]|message] eqn:Assumptions; [|discriminate].
  destruct (EaMain.decls_form E bound) as [[]|message] eqn:Decls; [|discriminate].
  destruct (EaMain.assemble E bound (b_entry bundle) (init_state bound))
    as [[program final]|message] eqn:Assembled; [|discriminate].
  injection Run as <- <-.
  apply (decls_form_sound E bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ World).
  - exact (EaMetadataSound.metadata_fuel_sufficient W0 bound).
  - exact Contexts.
  - intros symbol C ns Bounded Kind Read.
    change (table symbol = Some (C, ns)). unfold table.
    apply context_table_functor_complete; [exact Bounded|exact Kind|].
    rewrite <- (EaContextReadFacts.fun_ctx_context W0 table strategy modules mains EaMain.elab_fuel metadata).
    exact Read.
  - exact Constants.
  - exact Decls.
Qed.

Lemma checked_functor_result_read : forall E bound u,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaMain.decls_form E bound = Ok tt -> kind_sym (e_W E) u = Some DKFunctor ->
  exists C out C' keys, EaFamily.provide_result E u C out = Ok (C', keys).
Proof.
  intros E bound u Bounded Checks Kind. unfold kind_sym in Kind.
  destruct (w_decl (e_W E) u) as [d|] eqn:Declared; [|discriminate].
  destruct (d_ast d) as [| | | | | |receiver fn_name fn_params fn_result fn_body| |name parameters arguments results bodies]
    eqn:Ast; try discriminate Kind.
  1: destruct receiver; discriminate Kind.
  assert (Present : In u (seq 0 bound)) by
    (apply in_seq; pose proof (Bounded _ _ Declared); lia).
  unfold EaMain.decls_form in Checks. apply EaTypesSound.rbind_ok in Checks as (checked & Checks & _).
  destruct (EaDependencyFuel.rmap_member_read _ _ _ _ _ Checks _ Present) as (result & Read).
  destruct (EaMain.decl_forms E bound u) as [formed|message] eqn:Formed; [|discriminate Read].
  unfold EaMain.decl_forms in Formed. rewrite Declared, Ast in Formed.
  apply EaTypesSound.rbind_ok in Formed as ([context needs] & _ & Formed).
  apply EaTypesSound.rbind_ok in Formed as ([called keys] & Provide & _).
  do 4 eexists. exact Provide.
Qed.

Local Lemma checked_result_leaves_exact : forall E bound u,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaMain.decls_form E bound = Ok tt -> kind_sym (e_W E) u = Some DKFunctor ->
  forall key, In key (EaDecide.result_leaves (e_W E) (e_fuel E) u) <-> ResultLeaf (e_W E) u key.
Proof.
  intros E bound u Bounded Decls Kind key. split; [apply EaResultLeafSound.result_leaves_sound|].
  destruct (checked_functor_result_read E bound u Bounded Decls Kind) as (C & out & C' & keys & Read).
  eapply EaResultLeafSound.result_leaves_covered_after_read; exact Read.
Qed.

Local Lemma result_leaf_mem : forall key keys,
  existsb (EaDecide.leaf_eqb key) keys = true <-> In key keys.
Proof.
  intros key keys. rewrite existsb_exists. split.
  - intros [found [Member Read]]. apply EaResultLeafSound.leaf_eqb_spec in Read. now subst found.
  - intro Member. exists key. split; [exact Member|apply EaResultLeafSound.leaf_eqb_spec; reflexivity].
Qed.

Theorem same_result_exact_at_declarations : forall E bound u gg,
  (forall n d, w_decl (e_W E) n = Some d -> n < bound) ->
  EaMain.decls_form E bound = Ok tt ->
  kind_sym (e_W E) u = Some DKFunctor -> kind_sym (e_W E) gg = Some DKFunctor ->
  EaDecide.same_result (e_W E) (e_fuel E) u gg = true <-> SameResult (e_W E) u gg.
Proof.
  intros E bound u gg Bounded Decls Target Provider.
  pose proof (checked_result_leaves_exact E bound _ Bounded Decls Target) as Left.
  pose proof (checked_result_leaves_exact E bound _ Bounded Decls Provider) as Right.
  unfold EaDecide.same_result, SameResult. rewrite andb_true_iff, !forallb_forall.
  setoid_rewrite result_leaf_mem. setoid_rewrite Left. setoid_rewrite Right.
  split.
  - intros [Forward Backward] key. split; [apply Forward|apply Backward].
  - intros Same. split; intros key; apply Same.
Qed.
