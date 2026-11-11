(* Polymorphic and fixed dependency reads use the same source declaration. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaMentionsSound EaDependencyFuel EaResultSound.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma functor_decl_read : forall W symbol d parameters arguments results body,
  functor_decl W symbol = Some (d, (parameters, arguments, results, body)) ->
  exists name, w_decl W symbol = Some d /\
    d_ast d = ADFunctor name parameters arguments results body.
Proof.
  intros W symbol d parameters arguments results body Read. unfold functor_decl in Read.
  destruct (w_decl W symbol) as [found|] eqn:Declaration; [|discriminate].
  destruct (d_ast found) as [items declarations|name|name syntax|name params payload|name|name ty initial|
    receiver name params result code|name items|name params inputs outputs code] eqn:Ast; try discriminate.
  injection Read as <- <- <- <- <-.
  exists name. split.
  - reflexivity.
  - exact Ast.
Qed.

Theorem poly_explicit_exact : forall W symbol d names arguments results,
  functor_decl W symbol = Some (d, (Some names, arguments, results, None)) ->
  forall fuel x, In x (poly W fuel symbol) <-> Poly W symbol x.
Proof.
  intros W symbol d names arguments results Read fuel x.
  destruct (functor_decl_read _ _ _ _ _ _ _ Read) as [name [Declaration Ast]].
  unfold poly. rewrite Read, sort_uniq_members, named_members. split.
  - intro Member. exists d, name, (Some names), arguments, results.
    split; [exact Declaration|]. split; [exact Ast|exact Member].
  - intros (found & found_name & parameters & inputs & outputs & Found & FoundAst & Member).
    rewrite Declaration in Found. injection Found as <-.
    rewrite Ast in FoundAst. injection FoundAst as <- <- <- <-. exact Member.
Qed.

Theorem poly_sound : forall W bound,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  forall fuel symbol x, bound + S bound * bound < fuel ->
  In x (poly W fuel symbol) -> Poly W symbol x.
Proof.
  intros W bound Bounded fuel symbol x Enough Present. unfold poly in Present.
  destruct (functor_decl W symbol) as [[d [[[parameters arguments] results] body]]|] eqn:Read;
    [|contradiction].
  destruct body as [bindings|]; [contradiction|]. destruct parameters as [names|].
  - apply (proj1 (poly_explicit_exact _ _ _ _ _ _ Read fuel x)).
    unfold poly. rewrite Read. exact Present.
  - destruct (functor_decl_read _ _ _ _ _ _ _ Read) as [name [Declaration Ast]].
    exists d, name, None, arguments, results. split; [exact Declaration|]. split; [exact Ast|].
    exact (mentions_sound W bound Bounded fuel symbol x Enough Present).
Qed.

Theorem fixed_sound : forall W bound,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  forall fuel symbol x, bound + S bound * bound < fuel ->
  In x (fixed W fuel symbol) -> Fixed W symbol x.
Proof.
  intros W bound Bounded fuel symbol x Enough Present. unfold fixed in Present.
  apply filter_In in Present as [Mention Absent].
  apply negb_true_iff, nmem_false_spec in Absent.
  pose proof (mentions_sound W bound Bounded fuel symbol x Enough Mention) as SemanticMention.
  split; [exact SemanticMention|].
  destruct SemanticMention as (d & parameters & arguments & results & body & syntax & Declaration & Parts & Rest).
  destruct body as [bindings|].
  - intros (found & name & params & inputs & outputs & Found & Ast & PolyRead).
    rewrite Declaration in Found. injection Found as <-.
    unfold functor_parts in Parts. rewrite Ast in Parts. discriminate.
  - destruct parameters as [names|].
    + intro PolyRead. apply Absent.
      assert (Read : functor_decl W symbol = Some (d, (Some names, arguments, results, None))).
      { unfold functor_decl. rewrite Declaration, Parts. reflexivity. }
      exact (proj2 (poly_explicit_exact W symbol d names arguments results Read fuel x) PolyRead).
    + exfalso. apply Absent. unfold poly, functor_decl.
      rewrite Declaration, Parts. exact Mention.
Qed.

Theorem fixed_readings_member_sound : forall W bound,
  (forall symbol d, w_decl W symbol = Some d -> symbol < bound) ->
  forall fuel symbol tm x t, bound + S bound * bound < fuel ->
  In (x,t) (fixed_readings W fuel symbol tm) -> Fixed W symbol x /\ nlook x tm = Some t.
Proof.
  intros W bound Bounded fuel symbol tm x t Enough Present. unfold fixed_readings in Present.
  apply in_flat_map in Present as [source [Member Read]].
  destruct (nlook source tm) as [meaning|] eqn:Lookup; [|contradiction].
  destruct Read as [Equal|[]]. injection Equal as <- <-.
  split; [exact (fixed_sound W bound Bounded fuel symbol source Enough Member)|exact Lookup].
Qed.

Lemma fixed_unbracketed_impossible : forall W symbol d arguments results,
  functor_decl W symbol = Some (d, (None, arguments, results, None)) ->
  forall x, ~ Fixed W symbol x.
Proof.
  intros W symbol d arguments results Read x [Mention NotPoly]. apply NotPoly.
  destruct (functor_decl_read _ _ _ _ _ _ _ Read) as [name [Declaration Ast]].
  exists d, name, None, arguments, results. split; [exact Declaration|].
  split; [exact Ast|exact Mention].
Qed.

Lemma fixed_unbracketed_empty : forall W symbol d arguments results,
  functor_decl W symbol = Some (d, (None, arguments, results, None)) ->
  forall fuel, fixed W fuel symbol = [].
Proof.
  intros W symbol d arguments results Read fuel. unfold fixed, poly. rewrite Read.
  remember (mentions W fuel symbol) as members.
  destruct (filter (fun t => negb (nmem t members)) members) as [|x rest] eqn:Filtered; [reflexivity|].
  assert (Present : In x (filter (fun t => negb (nmem t members)) members)).
  { rewrite Filtered. left. reflexivity. }
  apply filter_In in Present as [Member Absent]. apply nmem_spec in Member.
  rewrite Member in Absent. discriminate.
Qed.

Theorem fixed_readings_unbracketed : forall W symbol d arguments results,
  functor_decl W symbol = Some (d, (None, arguments, results, None)) ->
  forall fuel tm, FixedReadings W symbol tm (fixed_readings W fuel symbol tm).
Proof.
  intros W symbol d arguments results Read fuel tm. unfold fixed_readings.
  rewrite (fixed_unbracketed_empty _ _ _ _ _ Read fuel).
  unfold FixedReadings. intros x t. split; [contradiction|].
  intros [Fixed _]. exact (fixed_unbracketed_impossible _ _ _ _ _ Read x Fixed).
Qed.


From NM Require EaMain EaWorldSound.

Corollary bundle_world_r_fixed_readers_sound : forall bundle W bound fuel,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> bound + S bound * bound < fuel ->
  (forall symbol x, In x (poly W fuel symbol) -> Poly W symbol x) /\
  (forall symbol x, In x (fixed W fuel symbol) -> Fixed W symbol x) /\
  (forall symbol tm x t, In (x,t) (fixed_readings W fuel symbol tm) ->
    Fixed W symbol x /\ nlook x tm = Some t).
Proof.
  intros bundle W bound fuel Run Enough. split.
  - intros symbol x Member. exact (poly_sound W bound
      (EaWorldSound.bundle_world_r_bound _ _ _ Run) fuel symbol x Enough Member).
  - split.
    + intros symbol x Member. exact (fixed_sound W bound
        (EaWorldSound.bundle_world_r_bound _ _ _ Run) fuel symbol x Enough Member).
    + intros symbol tm x t Member. exact (fixed_readings_member_sound W bound
        (EaWorldSound.bundle_world_r_bound _ _ _ Run) fuel symbol tm x t Enough Member).
Qed.
