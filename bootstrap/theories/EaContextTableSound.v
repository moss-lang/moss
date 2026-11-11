(* The source derivations and merge witnesses behind context-table entries. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaCtx ElabWorld NMap EaTypesSound EaEntriesSound EaRootSound EaMergeSound EaFixedSound EaResultBudget.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

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

(* Only actual source telescopes are passed to the unifier here. Arbitrary
   entry lists can contain absent instantiation placeholders with no type
   declaration in the telescope. *)
Definition ContextTelescope (W : world) (symbol : nat) (entries : list entry) : Prop :=
  (exists d assumed meanings roots root_meanings,
    w_decl W symbol = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) assumed meanings /\
    RootCtx W symbol assumed meanings roots root_meanings /\ entries = assumed ++ roots) \/
  (exists d parameters arguments results body meanings,
    w_decl W symbol = Some d /\ functor_parts (d_ast d) = Some (parameters, arguments, results, body) /\
    ItemsEntries W (d_mod d) [] [] (map CISpec arguments) entries meanings).

Definition ContextMergeSound (E : env) : Prop :=
  forall symbol entries sigma classes, ContextTelescope (e_W E) symbol entries ->
    EaCtx.merge E entries = Ok (sigma, classes) -> MostGeneralMerge sigma entries.

Section Formation.
Variable E : env.
Variable bound : nat.
Let W := e_W E.
Hypothesis Bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.
Hypothesis Enough : metadata_budget W bound < e_metadata_fuel E.
Hypothesis Main : forall s, In s (e_mains E) <-> IsMainFn W s.
(* This is the remaining unifier algorithm obligation, not an assumption
   supplied by the public safety theorem or the host interface. *)
Hypothesis Principal : ContextMergeSound E.

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
  assert (Telescope : ContextTelescope W symbol (entries ++ roots)).
  { left. exists d, entries, meanings, roots, root_meanings. repeat first [assumption | split | reflexivity]. }
  pose proof (Principal _ _ _ _ Telescope Merge) as MostGeneral.
  destruct (merge_presentation E _ _ _ Merge MostGeneral) as [Classes Section].
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
  assert (Telescope : ContextTelescope W symbol entries).
  { right. exists d, parameters, arguments, results, body, meanings. auto. }
  pose proof (Principal _ _ _ _ Telescope Merge) as MostGeneral.
  destruct (merge_presentation E _ _ _ Merge MostGeneral) as [Classes Section].
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
