(* The source derivations and merge witnesses behind context-table entries. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaCtx ElabWorld NMap EaTypesSound EaEntriesSound EaRootSound EaMergeSound EaUnifySound EaFixedSound EaResultBudget.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

Definition ContextComputed (E : env) (symbol : nat) (result : cx * list item) : Prop :=
  match kind_sym (e_W E) symbol with
  | Some DKFn | Some DKMeth | Some DKVal => assume_ctx E symbol = Ok result
  | Some DKFunctor => fun_ctx E symbol = Ok result
  | _ => False
  end.
Definition TableComputed (E : env) (table : nmap (cx * list item)) : Prop :=
  forall symbol result, nget table symbol = Some result -> ContextComputed E symbol result.

Lemma registered_computation : forall E table symbol result,
  TableComputed E table -> ContextComputed E symbol result ->
  TableComputed E (nset table symbol result).
Proof.
  intros E table symbol result Previous Computed other value Found.
  rewrite WorldSound.nget_nset in Found. destruct (Nat.eqb other symbol) eqn:Equal.
  - apply Nat.eqb_eq in Equal. subst other. injection Found as <-. exact Computed.
  - exact (Previous _ _ Found).
Qed.

Theorem context_table_computed : forall E bound, TableComputed E (context_table E bound).
Proof.
  intros E bound. unfold context_table.
  assert (Fold : forall symbols table, TableComputed E table -> TableComputed E
    (fold_left (fun table symbol =>
       match kind_sym (e_W E) symbol with
       | Some DKFn | Some DKMeth | Some DKVal =>
           match assume_ctx E symbol with Ok result => nset table symbol result | Err _ => table end
       | Some DKFunctor =>
           match fun_ctx E symbol with Ok result => nset table symbol result | Err _ => table end
       | _ => table end) symbols table)).
  { intro symbols. induction symbols as [|symbol symbols IH]; intros table Previous;
      cbn [fold_left]; [exact Previous|]. apply IH.
    destruct (kind_sym (e_W E) symbol) as [[]|] eqn:Kind; try exact Previous;
      first
      [ destruct (assume_ctx E symbol) as [result|message] eqn:Read;
        [apply registered_computation; [exact Previous|unfold ContextComputed; rewrite Kind; exact Read]|exact Previous]
      | destruct (fun_ctx E symbol) as [result|message] eqn:Read;
        [apply registered_computation; [exact Previous|unfold ContextComputed; rewrite Kind; exact Read]|exact Previous] ]. }
  apply Fold. intros symbol result Found. rewrite WorldSound.nget_NE in Found. discriminate.
Qed.

Lemma assume_ctx_recorded : forall W symbol C needs,
  AssumeCtx W symbol C needs -> w_context W symbol = Some (C, needs) -> CtxOf W symbol C needs.
Proof.
  intros W symbol C needs (d & es & tm & er & tmr & sigma & classes & Read) Recorded.
  exists d, es, tm, er, tmr, sigma, classes.
  destruct Read as (Declaration & Items & Root & Principal & Merge & Section & Context & Needs).
  repeat first [assumption | split].
Qed.

Section Formation.
Variable E : env.
Variable bound : nat.
Let W := e_W E.
Hypothesis Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.
Hypothesis Enough : metadata_budget W bound < e_metadata_fuel E.
Hypothesis Main : forall s, In s (e_mains E) <-> IsMainFn W s.
Theorem assume_ctx_sound : forall symbol C ns,
  assume_ctx E symbol = Ok (C, ns) -> AssumeCtx W symbol C ns.
Proof.
  intros symbol C ns Run. unfold assume_ctx in Run.
  apply rbind_ok in Run as [d [Declaration Run]]. apply decl_of_sound in Declaration.
  apply rbind_ok in Run as [[entries meanings] [Inputs Run]].
  apply rbind_ok in Run as [[roots root_meanings] [Root Run]].
  apply rbind_ok in Run as [[sigma classes] [Merge Finish]]. injection Finish as <- <-.
  assert (Items : ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) entries meanings).
  { exact (proj2 (entries_sound_at_budget E bound Bounded Enough (e_fuel E)) _ _ _ _ _ _ _ Inputs). }
  assert (Rooted : RootCtx W symbol entries meanings roots root_meanings).
  { exact (root_ctx_sound_at_budget E bound Bounded Enough Main _ _ _ _ _ Root). }
  destruct (merge_sound E _ _ _ Merge) as (MostGeneral & Classes & Section).
  exists d, entries, meanings, roots, root_meanings, sigma, classes.
  repeat first [assumption | split | reflexivity | apply needs_exact].
Qed.

Lemma implicit_inputs_exact : forall A (body : option A) symbol (meanings : subst) x,
  In x (match body with
    | None => filter (fun t => negb (is_some (nlook t meanings)))
        (EaTypes.mentions W (e_metadata_fuel E) symbol)
    | Some _ => [] end) <->
  body = None /\ Mentions W symbol x /\ nlook x meanings = None.
Proof.
  intros A [body|] symbol meanings x; cbn; [split; intuition discriminate|].
  rewrite filter_In, negb_true_iff.
  rewrite (EaResultBudget.mentions_exact W bound Bounded _ symbol Enough x).
  destruct (nlook x meanings); cbn [is_some]; intuition discriminate.
Qed.

Theorem fun_ctx_sound : forall symbol C ns,
  fun_ctx E symbol = Ok (C, ns) -> w_context W symbol = Some (C, ns) -> FunCtx W symbol C ns.
Proof.
  intros symbol C ns Run Recorded. unfold fun_ctx in Run. fold W in Run.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|] eqn:Functor;
    cbn [rbind] in Run; [|discriminate].
  apply rbind_ok in Run as [[entries meanings] [Inputs Run]].
  apply rbind_ok in Run as [[sigma classes] [Merge Finish]]. injection Finish as <- <-.
  destruct (functor_decl_read _ _ _ _ _ _ _ Functor) as [name [Declaration Ast]].
  assert (Parts : functor_parts (d_ast d) = Some (parameters, arguments, results, body)).
  { unfold functor_parts. rewrite Ast. reflexivity. }
  assert (Items : ItemsEntries W (d_mod d) [] [] (map CISpec arguments) entries meanings).
  { exact (proj2 (entries_sound_at_budget E bound Bounded Enough (e_fuel E)) _ _ _ _ _ _ _ Inputs). }
  destruct (merge_sound E _ _ _ Merge) as (MostGeneral & Classes & Section).
  exists d, parameters, arguments, results, body, entries, meanings,
    (map (fun t => (t, TAbs t))
      (match body with None => filter (fun t => negb (is_some (nlook t meanings)))
        (EaTypes.mentions W (e_metadata_fuel E) symbol) | Some _ => [] end) ++ meanings),
    sigma, classes,
    (match body with None => filter (fun t => negb (is_some (nlook t meanings)))
        (EaTypes.mentions W (e_metadata_fuel E) symbol) | Some _ => [] end).
  repeat first [assumption | exact (implicit_inputs_exact _ body symbol meanings) |
    split | reflexivity | apply needs_exact].
Qed.
End Formation.

Definition ContextTableSound (W : world) : Prop :=
  forall symbol C ns, w_context W symbol = Some (C, ns) ->
    match kind_sym W symbol with
    | Some DKFn | Some DKMeth | Some DKVal => CtxOf W symbol C ns
    | Some DKFunctor => FunCtx W symbol C ns
    | _ => False
    end.

Theorem installed_context_table_sound : forall E bound,
  (forall symbol d, w_decl (e_W E) symbol = Some d -> symbol < bound) ->
  metadata_budget (e_W E) bound < e_metadata_fuel E ->
  (forall s, In s (e_mains E) <-> IsMainFn (e_W E) s) ->
  (forall symbol, w_context (e_W E) symbol = nget (context_table E bound) symbol) ->
  ContextTableSound (e_W E).
Proof.
  intros E bound Bounded Enough Main Installed symbol C ns Found.
  assert (Computed : ContextComputed E symbol (C, ns)).
  { apply (context_table_computed E bound). rewrite <- Installed. exact Found. }
  unfold ContextComputed in Computed.
  destruct (kind_sym (e_W E) symbol) as [[]|]; try exact Computed.
  all: first
    [ eapply assume_ctx_recorded;
      [exact (assume_ctx_sound E bound Bounded Enough Main _ _ _ Computed)|exact Found]
    | exact (fun_ctx_sound E bound Bounded Enough _ _ _ Computed Found) ].
Qed.

From NM Require EaMain EaMainSymbols EaMetadataSound EaWorldSound EaContextReadFacts.

Corollary bundle_world_r_context_table_sound : forall b W0 bound,
  EaMain.bundle_world_r b = Ok (W0, bound) -> forall strategy,
  let modules := map am_path (b_mods b) in
  let mains := EaMain.main_symbols W0 modules in
  let metadata := EaMain.metadata_fuel W0 bound in
  let E0 := Env W0 strategy modules mains EaMain.elab_fuel metadata in
  ContextTableSound (ElabWorld.with_context W0 (nget (context_table E0 bound))).
Proof.
  intros b W0 bound Loaded strategy. cbn zeta.
  set (modules := map am_path (b_mods b)).
  set (mains := EaMain.main_symbols W0 modules).
  set (metadata := EaMain.metadata_fuel W0 bound).
  set (E0 := Env W0 strategy modules mains EaMain.elab_fuel metadata).
  set (W := ElabWorld.with_context W0 (nget (context_table E0 bound))).
  set (E := Env W strategy modules mains EaMain.elab_fuel metadata).
  change (ContextTableSound (e_W E)). apply (installed_context_table_sound E bound).
  - exact (EaWorldSound.bundle_world_r_bound _ _ _ Loaded).
  - exact (EaMetadataSound.metadata_fuel_sufficient W0 bound).
  - exact (EaMainSymbols.bundle_world_r_main_symbols_exact _ _ _ Loaded).
  - intro symbol. unfold E, W, E0. cbn [e_W ElabWorld.with_context w_context].
    rewrite EaContextReadFacts.context_table_context. reflexivity.
Qed.

Theorem elaborate_with_context_table_sound : forall strategy b W program,
  EaMain.elaborate_with strategy b = Ok (W, program) -> ContextTableSound W.
Proof.
  intros strategy b W program Run.
  unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata in Run.
  destruct (EaMain.bundle_world_r b) as [[W0 bound]|message] eqn:Loaded; [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-. exact (bundle_world_r_context_table_sound _ _ _ Loaded strategy).
Qed.
