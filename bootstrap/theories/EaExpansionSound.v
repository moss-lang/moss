(* The cycle checker accepts only after checking a level on every edge.
   Soundness uses the checked inequalities, independent of how levels are found. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit NMap EaBase EaTypes EaTypesSound EaExpand.
From NM Require EaReadClosure PublicCallRealize PublicTypeRead WorldSound.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Definition GraphEdge (g : graph) (a b : vkey) (strict : bool) : Prop :=
  In a (g_verts g) /\ In (b, strict) (succs g a).

Theorem bounded_sound : forall g,
  bounded g = true ->
  exists level : vkey -> nat, forall a b strict,
    GraphEdge g a b strict -> level b + (if strict then 1 else 0) <= level a.
Proof.
  intros g Checked. unfold bounded, check_ranks in Checked.
  exists (getn (component_ranks g)). intros a b strict [Vertex Edge].
  apply forallb_forall with (x := a) in Checked; [|exact Vertex].
  apply forallb_forall with (x := (b, strict)) in Checked; [|exact Edge].
  apply Nat.leb_le. exact Checked.
Qed.

Local Lemma vget_vset_same : forall A (m : vmap A) key x,
  vget (vset m key x) key = Some x.
Proof.
  intros A m [a b] x. unfold vget, vset. cbn [fst snd].
  now rewrite !WorldSound.nget_nset_same.
Qed.

Local Lemma vget_vset_other : forall A (m : vmap A) written key x,
  key <> written -> vget (vset m written x) key = vget m key.
Proof.
  intros A m [a b] [c d] x Different. unfold vget, vset. cbn [fst snd].
  destruct (Nat.eq_dec c a) as [->|First].
  - rewrite WorldSound.nget_nset_same.
    rewrite WorldSound.nget_nset_other by congruence.
    destruct (nget m a); reflexivity.
  - rewrite WorldSound.nget_nset_other by exact First. reflexivity.
Qed.

Local Lemma vkey_eq_dec : forall a b : vkey, {a = b} + {a <> b}.
Proof. decide equality; apply Nat.eq_dec. Qed.

Definition GraphExtends (old new : graph) : Prop :=
  forall a b strict, GraphEdge old a b strict -> GraphEdge new a b strict.

Definition VerticesListed (g : graph) : Prop :=
  forall a edges, vget (g_adj g) a = Some edges -> In a (g_verts g).

Local Lemma gedge_vertices : forall g a b strict,
  incl (g_verts g) (g_verts (gedge g a b strict)).
Proof.
  intros g a b strict old Member. unfold gedge. cbn [g_verts].
  destruct (vget (g_adj g) a); [exact Member|now right].
Qed.

Local Lemma gedge_lists_vertices : forall g a b strict,
  VerticesListed g -> VerticesListed (gedge g a b strict).
Proof.
  intros g a b strict Listed key edges Found.
  unfold gedge in Found. cbn [g_adj] in Found.
  destruct (vkey_eq_dec key a) as [->|Different].
  - unfold gedge. cbn [g_verts].
    destruct (vget (g_adj g) a) eqn:Old; [eapply Listed; exact Old|now left].
  - apply gedge_vertices. eapply Listed.
    rewrite vget_vset_other in Found by exact Different. exact Found.
Qed.

Local Lemma gedge_extends : forall g a b strict,
  GraphExtends g (gedge g a b strict).
Proof.
  intros g a b strict old target flag [Vertex Edge]. split; [now apply gedge_vertices|].
  unfold gedge, succs in *. cbn [g_adj].
  destruct (vkey_eq_dec old a) as [->|Different].
  - rewrite vget_vset_same. now right.
  - rewrite vget_vset_other by exact Different. exact Edge.
Qed.

Local Lemma gedge_inserts : forall g a b strict,
  VerticesListed g -> GraphEdge (gedge g a b strict) a b strict.
Proof.
  intros g a b strict Listed. unfold GraphEdge, gedge, succs. cbn [g_verts g_adj].
  rewrite vget_vset_same. split; [|now left].
  destruct (vget (g_adj g) a) as [edges|] eqn:Lookup; [eapply Listed; exact Lookup|now left].
Qed.

Definition GrowsGraph (write : graph -> graph) : Prop :=
  forall g, VerticesListed g -> VerticesListed (write g) /\ GraphExtends g (write g).

Local Lemma graph_noop : GrowsGraph (fun g => g).
Proof. intros g Listed. now split. Qed.

Local Lemma graph_add : forall a b strict, GrowsGraph (fun g => gedge g a b strict).
Proof. intros a b strict g Listed. split; [now apply gedge_lists_vertices|apply gedge_extends]. Qed.

Local Lemma graph_fold_grows : forall A (step : graph -> A -> graph),
  (forall x, GrowsGraph (fun g => step g x)) ->
  forall xs, GrowsGraph (fun g => fold_left step xs g).
Proof.
  intros A step Grows xs. induction xs as [|x xs IH]; [apply graph_noop|].
  intros g Listed. cbn [fold_left].
  destruct (Grows x g Listed) as [Next First].
  destruct (IH _ Next) as [Final Last].
  split; [exact Final|]. intros a b strict Edge. auto.
Qed.

Local Lemma graph_fold_contains : forall A (step : graph -> A -> graph),
  (forall x, GrowsGraph (fun g => step g x)) ->
  forall xs x g, In x xs -> VerticesListed g ->
  forall a b strict, (forall start, VerticesListed start -> GraphEdge (step start x) a b strict) ->
  GraphEdge (fold_left step xs g) a b strict.
Proof.
  intros A step Grows xs. induction xs as [|head tail IH];
    intros x g Present Listed a b strict Insert; [contradiction|].
  cbn [fold_left]. destruct (Grows head g Listed) as [Next First].
  destruct Present as [<-|Present].
  - destruct (graph_fold_grows A step Grows tail _ Next) as [_ Last].
    apply Last, Insert, Listed.
  - eapply IH; eassumption.
Qed.

Section Types.
Variable W : world.
Variable fuel : nat.

Local Lemma type_graph_step_grows : forall tag,
  GrowsGraph (fun g =>
    if kind_is W tag DKTag then
      match payload_of W fuel tag (as_written (w_reqs W tag)) with
      | Ok payload =>
          fold_left (fun g na =>
            fold_left (fun g ytau =>
              fold_left (fun g xs => gedge g (tag, fst xs) (fst na, fst ytau) (snd xs))
                (ty_occurrences (snd ytau)) g) (snd na) g) (ty_nominals payload) g
      | Err _ => g
      end
    else g).
Proof.
  intro tag. destruct (kind_is W tag DKTag); [|apply graph_noop].
  destruct (payload_of W fuel tag (as_written (w_reqs W tag))); [|apply graph_noop].
  do 3 (apply graph_fold_grows; intro). apply graph_add.
Qed.

Theorem type_graph_covers : forall bound,
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload -> tag < bound) ->
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload ->
    payload_of W fuel tag (as_written (w_reqs W tag)) = Ok payload) ->
  forall a b strict, TypeEdge W a b strict -> GraphEdge (type_graph W fuel bound) a b strict.
Proof.
  intros bound Bounded Reads [tag formal] [callee argument] strict Edge.
  destruct Edge as (payload & args & tau & Payload & Nominal & Argument & Occurrence).
  cbn [fst snd] in Payload, Nominal, Argument, Occurrence.
  change (PayloadOf W tag (as_written (w_reqs W tag)) payload) in Payload.
  unfold type_graph.
  apply graph_fold_contains with (x := tag).
  - exact type_graph_step_grows.
  - apply in_seq. pose proof (Bounded _ _ Payload). lia.
  - intros a edges Found. discriminate Found.
  - intros start Listed.
    assert (Kind : kind_is W tag DKTag = true) by
      (apply kind_is_spec; eapply PublicTypeRead.payload_tag; exact Payload).
    unfold as_written in Reads. rewrite Kind, (Reads _ _ Payload). apply graph_fold_contains with (x := (callee, args)).
    + intros na. do 2 (apply graph_fold_grows; intro). apply graph_add.
    + exact Nominal.
    + exact Listed.
    + intros intermediate ListIntermediate. cbn [fst snd].
      apply graph_fold_contains with (x := (argument, tau)).
      * intros ytau. apply graph_fold_grows. intro xs. apply graph_add.
      * exact Argument.
      * exact ListIntermediate.
      * intros finish ListFinish. cbn [fst snd].
        apply graph_fold_contains with (x := (formal, strict)).
        -- intros xs. apply graph_add.
        -- exact Occurrence.
        -- exact ListFinish.
        -- intros final ListFinal. now apply gedge_inserts.
Qed.

Theorem type_graph_sound : forall bound,
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload -> tag < bound) ->
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload ->
    payload_of W fuel tag (as_written (w_reqs W tag)) = Ok payload) ->
  bounded (type_graph W fuel bound) = true -> TypesBounded W.
Proof.
  intros bound Bounded Reads Checked.
  destruct (bounded_sound _ Checked) as [level Levels].
  exists level. intros a b strict Edge. apply Levels.
  now apply type_graph_covers.
Qed.
End Types.

Section Instantiations.
Variables (P : program) (fns : nmap expr) (funs : nmap functor_code).
Hypotheses (Functions : forall f, nget fns f = body_of P f)
  (Functors : forall h, nget funs h = funbody_of P h).

Local Lemma sites_exact : forall n, sites_of fns funs n = node_sites P n.
Proof. intros [f|h]; cbn [sites_of node_sites]; now rewrite ?Functions, ?Functors. Qed.

Local Lemma table_exact : forall n, table_of funs n = node_table P n.
Proof. intros [f|h]; cbn [table_of node_table]; now rewrite ?Functors. Qed.

Local Lemma table_read_step : forall n x t v strict,
  In (x, t) (node_table P n) -> In (v, strict) (ty_occurrences t) ->
  In (x, (v, strict)) (read_steps fns funs n).
Proof.
  intros n x t v strict Table Read. unfold read_steps.
  apply in_or_app. left. apply in_flat_map. exists (x, t). split.
  - now rewrite table_exact.
  - apply in_map with (f := fun q => (x, q)) in Read. exact Read.
Qed.

Local Lemma output_read_step : forall n th outputs source t x tau v strict,
  In (SiteApply th outputs) (node_sites P n) -> In (source, t) outputs ->
  In (x, tau) th -> In (v, strict) (ty_occurrences tau) ->
  In (t, (v, true)) (read_steps fns funs n).
Proof.
  intros n th outputs source t x tau v strict Site Output Argument Read.
  unfold read_steps. apply in_or_app. right. apply in_flat_map.
  exists (SiteApply th outputs). split; [now rewrite sites_exact|].
  apply in_flat_map. exists (source, t). split; [exact Output|].
  apply in_flat_map. exists (x, tau). split; [exact Argument|].
  apply in_map with (f := fun q => (t, (fst q, true))) in Read. exact Read.
Qed.

Local Lemma closed_covers_node_reads : forall n t u strict,
  NodeReads P n t u strict -> forall closed prior,
  EaReadClosure.ReadsClosed (read_steps fns funs n) closed -> In (t, prior) closed ->
  In (u, prior || strict) closed.
Proof.
  intros n t u strict Read. induction Read as [t|x tau v strict u next Table Occurrence Read IH|
    th outputs source t x tau v strict u next Site Output Argument Occurrence Read IH];
    intros closed prior Closed Present.
  - now rewrite orb_false_r.
  - rewrite orb_assoc. apply IH; [exact Closed|].
    eapply (Closed (x, prior) (v, prior || strict)); [exact Present|].
    exists strict. split; [eapply table_read_step; eassumption|reflexivity].
  - assert (Next : In (v, true) closed).
    { eapply (Closed (t, prior) (v, true)); [exact Present|].
      exists true. split; [eapply output_read_step; eassumption|].
      cbn [snd]. now rewrite orb_true_r. }
    specialize (IH closed true Closed Next). now rewrite orb_true_r.
Qed.

Local Lemma node_reads_covered : forall n v u strict,
  NodeReads P n v u strict ->
  let steps := read_steps fns funs n in
  In (u, strict) (reads_closure (S (2 * List.length steps)) steps [(v, false)] [(v, false)]).
Proof.
  intros n v u strict Read.
  destruct (EaReadClosure.reads_closure_covers (read_steps fns funs n) v) as [Self Closed].
  exact (closed_covers_node_reads _ _ _ _ Read _ false Closed Self).
Qed.

Local Lemma node_graph_grows : forall n, GrowsGraph (fun g => node_graph P fns funs g n).
Proof.
  intros n. unfold node_graph.
  do 5 (apply graph_fold_grows; intro). apply graph_add.
Qed.

Local Lemma node_graph_covers : forall g a b strict,
  VerticesListed g -> InstantiationEdge P a b strict ->
  GraphEdge (node_graph P fns funs g (fst a)) (node_id (fst a), snd a) (node_id (fst b), snd b) strict.
Proof.
  intros g [n u] [target argument] strict Listed Edge.
  destruct Edge as (site & th & tau & v & direct & indirect & Site & Target & Arg & Occurs & Read & ->).
  cbn [fst snd] in Site, Target, Arg, Read |- *.
  unfold node_graph.
  apply graph_fold_contains with (x := site).
  - intro. do 4 (apply graph_fold_grows; intro). apply graph_add.
  - now rewrite sites_exact.
  - exact Listed.
  - intros start ListedStart. apply graph_fold_contains with (x := (target, th)).
    + intro. do 3 (apply graph_fold_grows; intro). apply graph_add.
    + exact Target.
    + exact ListedStart.
    + intros middle ListedMiddle. cbn [fst snd]. apply graph_fold_contains with (x := (argument, tau)).
      * intro. do 2 (apply graph_fold_grows; intro). apply graph_add.
      * exact Arg.
      * exact ListedMiddle.
      * intros finish ListedFinish. cbn [fst snd].
        apply graph_fold_contains with (x := (v, direct)).
        -- intro. apply graph_fold_grows. intro. apply graph_add.
        -- exact Occurs.
        -- exact ListedFinish.
        -- intros last ListedLast. cbn [fst snd].
           apply graph_fold_contains with (x := (u, indirect)).
           ++ intro. apply graph_add.
           ++ now apply node_reads_covered.
           ++ exact ListedLast.
           ++ intros final ListedFinal. now apply gedge_inserts.
Qed.
End Instantiations.

Local Lemma merge_graph_step : forall n section (row : nat * ty),
  GrowsGraph (fun g =>
    let g := fold_left (fun g vs => gedge g (node_id n, fst vs) (node_id n, fst row) (snd vs))
      (ty_occurrences (app_subst section (TAbs (fst row)))) g in
    match app_subst section (TAbs (fst row)) with TAbs z => gedge g (node_id n, fst row) (node_id n, z) false | _ => g end).
Proof.
  intros n section row g Listed.
  destruct (graph_fold_grows _ (fun g vs => gedge g (node_id n, fst vs) (node_id n, fst row) (snd vs))
    (fun _ => graph_add _ _ _) (ty_occurrences (app_subst section (TAbs (fst row)))) g Listed) as [Next Grown].
  destruct (app_subst section (TAbs (fst row))); try (now split).
  split; [now apply gedge_lists_vertices|]. intros a b strict Edge. apply gedge_extends. now apply Grown.
Qed.

Local Lemma merge_graph_grows : forall n section, GrowsGraph (fun g => merge_graph g n section).
Proof. intros n section. unfold merge_graph. apply graph_fold_grows, merge_graph_step. Qed.

Local Lemma merge_occurrence : forall g n section t v strict,
  VerticesListed g -> In (v, strict) (ty_occurrences (app_subst section (TAbs t))) ->
  (v = t /\ strict = false) \/ GraphEdge (merge_graph g n section) (node_id n, v) (node_id n, t) strict.
Proof.
  intros g n section t v strict Listed Occurrence.
  destruct (lookup_subst t (section)) as [meaning|] eqn:Lookup.
  2: {
    cbn [app_subst] in Occurrence. rewrite Lookup in Occurrence.
    cbn [ty_occurrences] in Occurrence. destruct Occurrence as [Equal|[]].
    inversion Equal. now left.
  }
  rewrite PublicCallRealize.lookup_subst_nlook' in Lookup.
  apply PublicTypeRead.nlook_in in Lookup.
  right. unfold merge_graph. apply graph_fold_contains with (x := (t, meaning)).
  - apply merge_graph_step.
  - exact Lookup.
  - exact Listed.
  - intros start ListedStart. cbn [fst snd].
    assert (Added : GraphEdge
      (fold_left (fun g vs => gedge g (node_id n, fst vs) (node_id n, t) (snd vs))
        (ty_occurrences (app_subst section (TAbs t))) start) (node_id n, v) (node_id n, t) strict).
    {
      apply graph_fold_contains with (x := (v, strict)); try assumption.
      - intro. apply graph_add.
      - intros. now apply gedge_inserts.
    }
    destruct (app_subst section (TAbs t)); try exact Added. now apply gedge_extends.
Qed.

Local Lemma merge_alias : forall g n section t z,
  VerticesListed g -> app_subst section (TAbs t) = TAbs z ->
  z = t \/ GraphEdge (merge_graph g n section) (node_id n, t) (node_id n, z) false.
Proof.
  intros g n section t z Listed Alias.
  destruct (lookup_subst t (section)) as [meaning|] eqn:Lookup.
  2: {
    cbn [app_subst] in Alias. rewrite Lookup in Alias. inversion Alias.
    now left.
  }
  rewrite PublicCallRealize.lookup_subst_nlook' in Lookup.
  apply PublicTypeRead.nlook_in in Lookup.
  right. unfold merge_graph. apply graph_fold_contains with (x := (t, meaning)).
  - apply merge_graph_step.
  - exact Lookup.
  - exact Listed.
  - intros start ListedStart. cbn [fst snd]. rewrite Alias. apply gedge_inserts.
    eapply graph_fold_grows; [|exact ListedStart]. intro. apply graph_add.
Qed.

Section ProgramGraph.
Variables (W : world) (P : program) (fns : nmap expr)
  (funs : nmap functor_code) (next : nat).
Hypotheses (Functions : forall f, nget fns f = body_of P f)
  (Functors : forall h, nget funs h = funbody_of P h)
  (CodeWithin : forall n, CodePresent P n -> In n (code_nodes fns funs next)).

Local Definition code_graph := fold_left (node_graph P fns funs) (code_nodes fns funs next) gempty.
Local Definition add_context g n := match code_section W P n with
  | Some section => merge_graph g n section
  | None => g
  end.

Local Lemma code_graph_listed : VerticesListed code_graph.
Proof.
  unfold code_graph.
  eapply graph_fold_grows; [intro; apply node_graph_grows|intros a edges Found; discriminate Found].
Qed.

Local Lemma add_context_grows : forall n, GrowsGraph (fun g => add_context g n).
Proof.
  intro n. unfold add_context. destruct (code_section W P n).
  - apply merge_graph_grows.
  - apply graph_noop.
Qed.

Local Lemma instantiation_edge_covered : forall a b strict,
  InstantiationEdge P a b strict ->
  GraphEdge (instantiation_graph W P fns funs next)
    (node_id (fst a), snd a) (node_id (fst b), snd b) strict.
Proof.
  intros a b strict Edge.
  unfold instantiation_graph. fold code_graph. fold add_context.
  apply (proj2 (graph_fold_grows _ add_context add_context_grows _ _ code_graph_listed)).
  unfold code_graph. apply graph_fold_contains with (x := fst a).
  - intro; apply node_graph_grows.
  - apply CodeWithin. destruct Edge as (site & th & tau & v & s & s' & Site & _).
    destruct (fst a) as [f|h]; cbn [CodePresent node_sites] in *;
      intro Missing; now rewrite Missing in Site.
  - intros n edges Lookup. discriminate Lookup.
  - intros start Listed. eapply node_graph_covers; eassumption.
Qed.

Theorem instantiation_graph_sound :
  bounded (instantiation_graph W P fns funs next) = true ->
  InstantiationsBounded W P.
Proof.
  intro Checked. destruct (bounded_sound _ Checked) as [level Levels].
  exists (fun nt => level (node_id (fst nt), snd nt)). split.
  - intros a b strict Edge. apply Levels. now apply instantiation_edge_covered.
  - split.
    + intros n section t v strict [HasCode ContextRead] Occurrence. cbn [fst snd].
      pose proof (CodeWithin n HasCode) as Present.
      destruct (Nat.eq_dec v t) as [->|Different]; destruct strict;
        try lia.
      all: first [apply (Levels _ _ true)|apply (Levels _ _ false)].
      all: unfold instantiation_graph; fold code_graph; fold add_context.
      all: apply graph_fold_contains with (x := n);
        [exact add_context_grows|exact Present|exact code_graph_listed|].
      all: intros g Listed; unfold add_context; rewrite ContextRead.
      all: destruct (merge_occurrence g n section t _ _ Listed Occurrence) as [[Equal Strict]|Added];
        [congruence|exact Added].
    + intros n section t z [HasCode ContextRead] Alias. cbn [fst snd].
      destruct (Nat.eq_dec z t) as [->|Different]; [reflexivity|].
      pose proof (CodeWithin n HasCode) as Present.
      rewrite <- Nat.add_0_r at 1.
      change (level (node_id n, z) + (if false then 1 else 0) <= level (node_id n, t)).
      apply Levels.
      unfold instantiation_graph. fold code_graph. fold add_context.
      apply graph_fold_contains with (x := n);
        [exact add_context_grows|exact Present|exact code_graph_listed|].
      intros g Listed. unfold add_context. rewrite ContextRead.
      destruct (merge_alias g n section t z Listed Alias) as [Equal|Added]; [contradiction|exact Added].
Qed.
End ProgramGraph.

Lemma code_nodes_contain : forall fns funs next n,
  (match n with
   | CodeFn f => f < next /\ nget fns f <> None
   | CodeFunctor h => h < next /\ nget funs h <> None
   end) ->
  In n (code_nodes fns funs next).
Proof.
  intros fns funs next [f|h] [Bounded Found]; unfold code_nodes; apply in_flat_map.
  - exists f. split; [apply in_seq; lia|]. apply in_or_app. left.
    destruct (nget fns f); [now left|contradiction].
  - exists h. split; [apply in_seq; lia|]. apply in_or_app. right.
    destruct (nget funs h); [now left|contradiction].
Qed.

Theorem expansion_ok_sound : forall W fuel P fns funs bound next,
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload -> tag < bound) ->
  (forall tag payload, PayloadOf W tag (as_written (w_reqs W tag)) payload ->
    payload_of W fuel tag (as_written (w_reqs W tag)) = Ok payload) ->
  (forall f, nget fns f = body_of P f) ->
  (forall h, nget funs h = funbody_of P h) ->
  (forall f body, nget fns f = Some body -> f < next) ->
  (forall h code, nget funs h = Some code -> h < next) ->
  expansion_ok W fuel P fns funs bound next = true ->
  NoExpandingCycle W P.
Proof.
  intros W fuel P fns funs bound next Declared Payloads Functions Functors
    FunctionBounds FunctorBounds Checked.
  unfold expansion_ok in Checked. apply andb_true_iff in Checked as [Types Codes].
  split.
  - eapply type_graph_sound; eassumption.
  - eapply instantiation_graph_sound; try eassumption.
    intros [f|h] Present; apply code_nodes_contain.
    + cbn [CodePresent] in Present. rewrite <- Functions in Present.
      destruct (nget fns f) as [body|] eqn:Found; [|contradiction].
      split; [eapply FunctionBounds; exact Found|discriminate].
    + cbn [CodePresent] in Present. rewrite <- Functors in Present.
      destruct (nget funs h) as [body|] eqn:Found; [|contradiction].
      split; [eapply FunctorBounds; exact Found|discriminate].
Qed.
