(* Result dependency reads are sound above the visited-pair fuel budget. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaMentionsSound EaContextFuel.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma pmem_spec : forall x pairs, pmem x pairs = true <-> In x pairs.
Proof.
  intros [a b] pairs. induction pairs as [|[c d] pairs IH]; cbn; [split; [discriminate|contradiction]|].
  rewrite orb_true_iff, andb_true_iff, !Nat.eqb_eq, IH. intuition congruence.
Qed.

Lemma pmem_false_spec : forall x pairs, pmem x pairs = false <-> ~ In x pairs.
Proof.
  intros x pairs. split.
  - intros Absent Present. apply pmem_spec in Present. congruence.
  - intro Absent. destruct (pmem x pairs) eqn:Present; [|reflexivity].
    exfalso. apply Absent, pmem_spec. exact Present.
Qed.

Lemma ctx_specs_members : forall sp items, In sp (ctx_specs items) <-> In (CISpec sp) items.
Proof.
  intros sp items. induction items as [|[item|left right] items IH]; cbn; [reflexivity| |].
  - rewrite IH. intuition congruence.
  - rewrite IH. intuition congruence.
Qed.

Section ResultReads.
Variable W : world.
Variable bound initial_self : nat.
Hypothesis declarations_bounded : forall symbol d, w_decl W symbol = Some d -> symbol < bound.

Definition reader_nodes : list nat := initial_self :: seq 0 bound.
Definition reader_pairs : list (nat * nat) :=
  flat_map (fun self => map (fun symbol => (self, symbol)) (seq 0 bound)) reader_nodes.
Definition stack_ok (stack : list (nat * nat)) : Prop := NoDup stack /\ incl stack reader_pairs.
Definition result_budget (stack : list (nat * nat)) : nat :=
  bound + (List.length reader_pairs - List.length stack).

Lemma reader_pair : forall self symbol,
  In self reader_nodes -> symbol < bound -> In (self, symbol) reader_pairs.
Proof.
  intros self symbol Self Symbol. unfold reader_pairs. apply in_flat_map.
  exists self. split; [exact Self|]. apply in_map. apply in_seq. lia.
Qed.

Lemma extend_stack : forall stack pair,
  stack_ok stack -> In pair reader_pairs -> ~ In pair stack -> stack_ok (pair :: stack).
Proof.
  intros stack pair [Distinct Included] Pair Fresh. split; [constructor; assumption|].
  intros key [<-|Member]; [exact Pair|apply Included; exact Member].
Qed.

Lemma stack_budget_step : forall stack pair fuel,
  stack_ok (pair :: stack) -> result_budget stack < S fuel -> result_budget (pair :: stack) < fuel.
Proof.
  intros stack pair fuel [Distinct Included] Enough.
  pose proof (NoDup_incl_length Distinct Included) as Size.
  unfold result_budget in *. cbn [List.length] in *. lia.
Qed.

Lemma unbound_member : forall p application values x,
  In x (filter (fun t => negb (nmem t (bracket_bound W p application))) values) ->
  In x values /\ ~ exists b, In b (bracket_list application) /\ resolve_sym W p (fst b) = Some x.
Proof.
  intros p application values x Present. apply filter_In in Present as [Member Unbound].
  split; [exact Member|]. intro Bound. apply bracket_bound_members, nmem_spec in Bound.
  rewrite Bound in Unbound. discriminate.
Qed.

Lemma named_members : forall p names x,
  In x (flat_map (fun name => match resolve_sym W p [name] with Some t => [t] | None => [] end) names)
    <-> exists name, In name names /\ resolve_sym W p [name] = Some x.
Proof.
  intros p names x. rewrite in_flat_map. split.
  - intros [name [Member Resolve]]. exists name. split; [exact Member|].
    destruct (resolve_sym W p [name]); [destruct Resolve as [<-|[]]; reflexivity|contradiction].
  - intros [name [Member Resolve]]. exists name. split; [exact Member|]. rewrite Resolve. left. reflexivity.
Qed.

Lemma result_bracket_sound : forall fuel self p pa detached application x,
  resolve_sym W p pa <> None ->
  In x (flat_map (fun b => match snd b with
    | Spec (Some sp) _ sa => free_in W fuel (w_reqs W) p (ATRef sp sa)
    | _ => [] end) (bracket_list application)) ->
  ResultMentions W self p (Spec (Some pa) detached application) x.
Proof.
  intros fuel self p pa detached application x Resolve Present.
  apply in_flat_map in Present as [[bp [head name bindings]] [Member Present]].
  destruct head as [sp|]; [|contradiction].
  eapply RMBracket; [exact Resolve|exact Member|exact (free_in_sound W _ _ _ _ _ Present)].
Qed.

Theorem result_mentions_go_sound : forall fuel stack self p syntax x,
  stack_ok stack -> In self reader_nodes -> result_budget stack < fuel ->
  In x (result_mentions_go W fuel stack self p syntax) -> ResultMentions W self p syntax x.
Proof.
  induction fuel as [|fuel IH]; intros stack self p [head detached application] x Stack Self Enough Present;
    [contradiction|].
  destruct head as [pa|]; [|contradiction]. cbn [result_mentions_go] in Present.
  destruct (resolve_sym W p pa) as [symbol|] eqn:Resolve; [|contradiction].
  apply in_app_or in Present as [Own|Bracket].
  - destruct detached as [name|].
    + apply unbound_member in Own as [Own Unbound]. apply in_app_or in Own as [Receiver|Method].
      * eapply RMReceiver; [exact Resolve| |exact Unbound].
        destruct (kind_is W symbol DKType) eqn:TypeChecked.
        -- destruct Receiver as [<-|[]]. left. split; [reflexivity|apply kind_is_spec; exact TypeChecked].
        -- right. split; [|exact Receiver]. intro IsType. apply kind_is_spec in IsType. congruence.
      * destruct (method_named W p symbol name) as [method|] eqn:MethodRead; [|contradiction].
        eapply RMMeth; [exact Resolve|exact (method_named_sound W _ _ _ _ MethodRead)|
          exact (sig_mentions_sound W _ _ _ Method)|exact Unbound].
    + destruct (kind_is W symbol DKVal) eqn:Val.
      * apply unbound_member in Own as [Member Unbound].
        eapply RMValFn; [exact Resolve|left; apply kind_is_spec; exact Val|left; exact Member|exact Unbound].
      * destruct (kind_is W symbol DKFn) eqn:Fn.
        -- apply unbound_member in Own as [Member Unbound]. apply in_app_or in Member as [Requirement|Mention].
           ++ eapply RMValFn; [exact Resolve|right; apply kind_is_spec; exact Fn|left; exact Requirement|exact Unbound].
           ++ eapply RMValFn; [exact Resolve|right; apply kind_is_spec; exact Fn|right; split|exact Unbound].
              ** apply kind_is_spec. exact Fn.
              ** exact (sig_mentions_sound W _ _ _ Mention).
        -- destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
           destruct (d_ast d) as [items declarations|declaration_name|declaration_name body|
             declaration_name parameters payload|declaration_name|declaration_name value_type initial|
             receiver declaration_name parameters result body|declaration_name items|
             declaration_name parameters arguments results body] eqn:Ast; try contradiction.
           ++ destruct (pmem (self, symbol) stack) eqn:Seen; [contradiction|].
              apply pmem_false_spec in Seen.
              assert (Next : stack_ok ((self, symbol) :: stack)).
              { apply extend_stack; [exact Stack|apply reader_pair; [exact Self|exact (declarations_bounded _ _ Declaration)]|exact Seen]. }
              apply unbound_member in Own as [Member Unbound].
              apply in_flat_map in Member as [child [Member Read]].
              eapply RMCtx; [exact Resolve|exact Declaration|exact Ast|apply ctx_specs_members; exact Member| |exact Unbound].
              apply (IH _ _ _ _ _ Next Self (stack_budget_step _ _ _ Next Enough) Read).
           ++ destruct parameters as [names|], body as [bindings|]; try contradiction.
              destruct (Nat.eqb symbol self || pmem (symbol, symbol) stack) eqn:Seen; [contradiction|].
              apply orb_false_iff in Seen as [Other Fresh]. apply Nat.eqb_neq in Other.
              apply pmem_false_spec in Fresh.
              assert (SymbolBound : symbol < bound) by exact (declarations_bounded _ _ Declaration).
              assert (SymbolNode : In symbol reader_nodes).
              { right. apply in_seq. lia. }
              assert (Next : stack_ok ((symbol, symbol) :: stack)).
              { apply extend_stack; [exact Stack|apply reader_pair; assumption|exact Fresh]. }
              apply unbound_member in Own as [Member Unbound].
              apply filter_In in Member as [Member Filtered].
              apply andb_true_iff in Filtered as [NotOutput NotNamed].
              apply in_flat_map in Member as [child [Member Read]].
              eapply RMFunctor; [exact Resolve|exact Declaration|exact Ast|exact Other|exact Member|
                exact (IH _ _ _ _ _ Next SymbolNode (stack_budget_step _ _ _ Next Enough) Read)| | |exact Unbound].
              ** intro Output. apply (fun_outputs_complete_above W bound declarations_bounded (S fuel) symbol x) in Output.
                 --- apply nmem_spec in Output. rewrite Output in NotOutput. discriminate.
                 --- unfold result_budget in Enough. lia.
              ** intros [bound_name [Name ResolveName]].
                 assert (Named : In x (flat_map (fun name => match resolve_sym W (d_mod d) [name] with Some t => [t] | None => [] end) names)).
                 { apply named_members. exists bound_name. split; assumption. }
                 apply nmem_spec in Named. rewrite Named in NotNamed. discriminate.
  - apply (result_bracket_sound (S fuel) self p pa detached application x); [congruence|exact Bracket].
Qed.


Lemma reader_pairs_length : List.length reader_pairs = S bound * bound.
Proof.
  assert (Rows : forall nodes : list nat, List.length
    (flat_map (fun self => map (fun symbol => (self, symbol)) (seq 0 bound)) nodes)
      = List.length nodes * bound).
  { intro nodes. induction nodes as [|self nodes IH]; [reflexivity|].
    cbn [flat_map]. rewrite app_length, map_length, length_seq, IH. reflexivity. }
  unfold reader_pairs. rewrite Rows.
  change (S (List.length (seq 0 bound)) * bound = S bound * bound).
  rewrite length_seq. reflexivity.
Qed.

Corollary result_mentions_sound : forall fuel p syntax x,
  bound + S bound * bound < fuel ->
  In x (result_mentions W fuel initial_self p syntax) ->
  ResultMentions W initial_self p syntax x.
Proof.
  intros fuel p syntax x Enough Present. unfold result_mentions in Present.
  apply (result_mentions_go_sound fuel [] initial_self p syntax x); [|left; reflexivity| |exact Present].
  - split; [constructor|intros key Empty; contradiction].
  - unfold result_budget. cbn [List.length]. rewrite Nat.sub_0_r, reader_pairs_length. exact Enough.
Qed.

End ResultReads.

Theorem mentions_sound : forall W bound,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  forall fuel symbol x, bound + S bound * bound < fuel ->
  In x (mentions W fuel symbol) -> Mentions W symbol x.
Proof.
  intros W bound Bounded fuel symbol x Enough Present. unfold mentions, functor_decl in Present.
  destruct (w_decl W symbol) as [d|] eqn:Declaration; [|contradiction].
  destruct (functor_parts (d_ast d)) as [[[[parameters arguments] results] body]|] eqn:Parts;
    [|contradiction].
  rewrite sort_uniq_members in Present. apply filter_In in Present as [Present NotOutput].
  apply in_flat_map in Present as [syntax [Member Read]].
  exists d, parameters, arguments, results, body, syntax.
  split; [exact Declaration|]. split; [exact Parts|]. split; [exact Member|]. split.
  - exact (result_mentions_sound W bound symbol Bounded fuel _ _ _ Enough Read).
  - intro Output. apply (fun_outputs_complete_above W bound Bounded fuel symbol x) in Output; [|lia].
    apply nmem_spec in Output. rewrite Output in NotOutput. discriminate.
Qed.


From NM Require EaMain EaWorldSound.

Corollary bundle_world_r_result_readers_sound : forall bundle W bound fuel,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> bound + S bound * bound < fuel ->
  (forall self p syntax x, In x (result_mentions W fuel self p syntax) ->
    ResultMentions W self p syntax x) /\
  (forall symbol x, In x (mentions W fuel symbol) -> Mentions W symbol x).
Proof.
  intros bundle W bound fuel Run Enough. split.
  - intros self p syntax x Present.
    exact (result_mentions_sound W bound self (EaWorldSound.bundle_world_r_bound _ _ _ Run)
      fuel p syntax x Enough Present).
  - intros symbol x Present.
    exact (mentions_sound W bound (EaWorldSound.bundle_world_r_bound _ _ _ Run)
      fuel symbol x Enough Present).
Qed.
