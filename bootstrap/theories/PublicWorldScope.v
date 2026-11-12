(* Exact source export/scope clauses for the public metadata reading.
   These concern shared source syntax only, not runtime typing/refinement. *)
From Stdlib Require Import List String.
From NM Require AuditBase.
From NM Require Import Audit PublicWorld.
From NM Require ExecutionModel Elab WorldSound ComponentContracts Sx.
Import ListNotations.
Module A := AuditBase.Semantics.Elaboration.
Module M := ExecutionModel.Elaboration.

Definition implementation_entity (entity : A.ent) : M.ent :=
  match entity with A.ESym symbol => M.ESym symbol | A.EMod path => M.EMod path end.

Lemma public_namespaces_roundtrip : forall ns,
  namespace_of (implementation_namespace ns) = ns.
Proof. destruct ns; reflexivity. Qed.

Lemma entities_roundtrip : forall entity,
  implementation_entity (entity_of entity) = entity.
Proof. destruct entity; reflexivity. Qed.

Lemma public_entities_roundtrip : forall entity,
  entity_of (implementation_entity entity) = entity.
Proof. destruct entity; reflexivity. Qed.

Lemma filed_name_of : forall declaration,
  A.filed_name declaration =
    option_map (fun q => (namespace_of (fst q), snd q)) (M.filed_name declaration).
Proof.
  intro declaration; destruct declaration;
    try match goal with receiver : Audit.Input.fnrecv |- _ => destruct receiver end;
    reflexivity.
Qed.

Lemma implementation_filed_name : forall declaration,
  M.filed_name declaration =
    option_map (fun q => (implementation_namespace (fst q), snd q)) (A.filed_name declaration).
Proof.
  intro declaration; destruct declaration;
    try match goal with receiver : Audit.Input.fnrecv |- _ => destruct receiver end;
    reflexivity.
Qed.

Lemma public_declaration_read : forall W symbol declaration,
  A.w_decl (world_of W) symbol = Some declaration ->
  exists raw, M.w_decl W symbol = Some raw /\ declaration = declaration_of raw.
Proof.
  intros W symbol declaration Found.
  change (option_map declaration_of (M.w_decl W symbol) = Some declaration) in Found.
  destruct (M.w_decl W symbol) as [raw|] eqn:Raw; cbn in Found; [|discriminate].
  injection Found as <-. exists raw; auto.
Qed.

Theorem exports_of : forall bundle W path ns name entity,
  M.Exports bundle W path ns name entity ->
  A.Exports bundle (world_of W) path (namespace_of ns) name (entity_of entity).
Proof.
  intros bundle W. fix IH 5. intros path ns name entity Export.
  destruct Export as [path symbol declaration ns name Read Module Filed |
    path module target import use entity ns Module Imported NotGlob Used Namespace Export |
    path module target import alias Module Imported Alias].
  - eapply A.ExOwn with (d := declaration_of declaration).
    + change (option_map declaration_of (M.w_decl W symbol) = Some (declaration_of declaration)).
      rewrite Read; reflexivity.
    + exact Module.
    + change (A.filed_name (M.d_ast declaration) = Some (namespace_of ns,name)).
      rewrite filed_name_of, Filed; reflexivity.
  - destruct entity as [symbol|module_path]; cbn [namespace_of entity_of].
    + eapply A.ExUse with (m:=module) (t:=target) (imp:=import) (u:=use) (ns:=namespace_of ns) (e:=A.ESym symbol).
      * exact Module.
      * exact Imported.
      * exact NotGlob.
      * exact Used.
      * rewrite Namespace. destruct (Audit.Input.u_dot use); reflexivity.
      * destruct Export as [Direct|[Plain [Absent Indirect]]].
        -- left. exact (IH target ns (Audit.Input.u_name use) (M.ESym symbol) Direct).
        -- right. split; [exact Plain|]. split.
           ++ change (option_map entity_of (M.w_exp W target M.NSName (Audit.Input.u_name use)) = None).
              rewrite Absent; reflexivity.
           ++ exact (IH target M.NSAlias (Audit.Input.u_name use) (M.ESym symbol) Indirect).
    + eapply A.ExUse with (m:=module) (t:=target) (imp:=import) (u:=use) (ns:=namespace_of ns) (e:=A.EMod module_path).
      * exact Module.
      * exact Imported.
      * exact NotGlob.
      * exact Used.
      * rewrite Namespace. destruct (Audit.Input.u_dot use); reflexivity.
      * destruct Export as [Direct|[Plain [Absent Indirect]]].
        -- left. exact (IH target ns (Audit.Input.u_name use) (M.EMod module_path) Direct).
        -- right. split; [exact Plain|]. split.
           ++ change (option_map entity_of (M.w_exp W target M.NSName (Audit.Input.u_name use)) = None).
              rewrite Absent; reflexivity.
           ++ exact (IH target M.NSAlias (Audit.Input.u_name use) (M.EMod module_path) Indirect).
  - eapply A.ExAlias; eassumption.
Qed.

Theorem exports_implementation_of : forall bundle W path ns name entity,
  A.Exports bundle (world_of W) path ns name entity ->
  M.Exports bundle W path (implementation_namespace ns) name (implementation_entity entity).
Proof.
  intros bundle W. fix IH 5. intros path ns name entity Export.
  destruct Export as [path symbol declaration ns name Read Module Filed |
    path module target import use entity ns Module Imported NotGlob Used Namespace Export |
    path module target import alias Module Imported Alias].
  - destruct (public_declaration_read W symbol declaration Read) as [raw [Raw ->]].
    change (A.filed_name (M.d_ast raw) = Some (ns,name)) in Filed.
    eapply M.ExOwn with (d := raw).
    + exact Raw.
    + exact Module.
    + rewrite implementation_filed_name, Filed; reflexivity.
  - destruct entity as [symbol|module_path]; cbn [implementation_namespace implementation_entity].
    + eapply M.ExUse with (m:=module) (t:=target) (imp:=import) (u:=use) (ns:=implementation_namespace ns) (e:=M.ESym symbol).
      * exact Module.
      * exact Imported.
      * exact NotGlob.
      * exact Used.
      * rewrite Namespace. destruct (Audit.Input.u_dot use); reflexivity.
      * destruct Export as [Direct|[Plain [Absent Indirect]]].
        -- left. exact (IH target ns (Audit.Input.u_name use) (A.ESym symbol) Direct).
        -- right. split; [exact Plain|]. split.
           ++ change (option_map entity_of (M.w_exp W target M.NSName (Audit.Input.u_name use)) = None) in Absent.
              destruct (M.w_exp W target M.NSName (Audit.Input.u_name use)); [discriminate|reflexivity].
           ++ exact (IH target A.NSAlias (Audit.Input.u_name use) (A.ESym symbol) Indirect).
    + eapply M.ExUse with (m:=module) (t:=target) (imp:=import) (u:=use) (ns:=implementation_namespace ns) (e:=M.EMod module_path).
      * exact Module.
      * exact Imported.
      * exact NotGlob.
      * exact Used.
      * rewrite Namespace. destruct (Audit.Input.u_dot use); reflexivity.
      * destruct Export as [Direct|[Plain [Absent Indirect]]].
        -- left. exact (IH target ns (Audit.Input.u_name use) (A.EMod module_path) Direct).
        -- right. split; [exact Plain|]. split.
           ++ change (option_map entity_of (M.w_exp W target M.NSName (Audit.Input.u_name use)) = None) in Absent.
              destruct (M.w_exp W target M.NSName (Audit.Input.u_name use)); [discriminate|reflexivity].
           ++ exact (IH target A.NSAlias (Audit.Input.u_name use) (A.EMod module_path) Indirect).
  - eapply M.ExAlias; eassumption.
Qed.

Theorem exports_iff : forall bundle W path ns name entity,
  A.Exports bundle (world_of W) path ns name entity <->
    M.Exports bundle W path (implementation_namespace ns) name (implementation_entity entity).
Proof.
  intros. split; [apply exports_implementation_of|].
  intro Export. apply exports_of in Export.
  rewrite public_namespaces_roundtrip, public_entities_roundtrip in Export; exact Export.
Qed.

Lemma entity_option_iff : forall entities entity,
  option_map entity_of entities = Some entity <-> entities = Some (implementation_entity entity).
Proof.
  intros entities entity. split.
  - intro Found. destruct entities as [raw|]; cbn in Found; [|discriminate].
    injection Found as Equal. apply (f_equal implementation_entity) in Equal.
    rewrite entities_roundtrip in Equal. now rewrite Equal.
  - intro Found. rewrite Found. cbn. rewrite public_entities_roundtrip; reflexivity.
Qed.

Theorem ordinary_scope_of : forall bundle W path ns name entity,
  M.OrdinaryScope bundle W path ns name entity ->
  A.OrdinaryScope bundle (world_of W) path (namespace_of ns) name (entity_of entity).
Proof.
  intros bundle W path ns name entity Ordinary. destruct Ordinary.
  - apply A.ScExported. exact (exports_of _ _ _ _ _ _ H).
  - eapply A.ScGlob; [exact H|exact H0|exact H1|].
    exact (exports_of _ _ _ _ _ _ H2).
Qed.

Theorem ordinary_scope_implementation_of : forall bundle W path ns name entity,
  A.OrdinaryScope bundle (world_of W) path ns name entity ->
  M.OrdinaryScope bundle W path (implementation_namespace ns) name (implementation_entity entity).
Proof.
  intros bundle W path ns name entity Ordinary. destruct Ordinary.
  - apply M.ScExported. exact (exports_implementation_of _ _ _ _ _ _ H).
  - eapply M.ScGlob; [exact H|exact H0|exact H1|].
    exact (exports_implementation_of _ _ _ _ _ _ H2).
Qed.

Theorem scope_of : forall bundle W path ns name entity,
  M.InScope bundle W path ns name entity ->
  A.InScope bundle (world_of W) path (namespace_of ns) name (entity_of entity).
Proof.
  intros bundle W path ns name entity Scoped. destruct Scoped as
    [path ns name entity Ordinary | path module ns name entity Module Prelude Export Absent].
  - apply A.ScPrimary. exact (ordinary_scope_of _ _ _ _ _ _ Ordinary).
  - eapply A.ScPrelude; [exact Module|exact Prelude| |].
    + exact (exports_of _ _ _ _ _ _ Export).
    + intros other Visible. apply (Absent (implementation_entity other)).
      pose proof (ordinary_scope_implementation_of _ _ _ _ _ _ Visible) as Old.
      rewrite namespaces_roundtrip in Old; exact Old.
Qed.

Theorem scope_implementation_of : forall bundle W path ns name entity,
  A.InScope bundle (world_of W) path ns name entity ->
  M.InScope bundle W path (implementation_namespace ns) name (implementation_entity entity).
Proof.
  intros bundle W path ns name entity Scoped. destruct Scoped as
    [path ns name entity Ordinary | path module ns name entity Module Prelude Export Absent].
  - apply M.ScPrimary. exact (ordinary_scope_implementation_of _ _ _ _ _ _ Ordinary).
  - eapply M.ScPrelude; [exact Module|exact Prelude| |].
    + exact (exports_implementation_of _ _ _ _ _ _ Export).
    + intros other Visible. apply (Absent (entity_of other)).
      pose proof (ordinary_scope_of _ _ _ _ _ _ Visible) as Public.
      rewrite public_namespaces_roundtrip in Public; exact Public.
Qed.

Theorem scope_iff : forall bundle W path ns name entity,
  A.InScope bundle (world_of W) path ns name entity <->
    M.InScope bundle W path (implementation_namespace ns) name (implementation_entity entity).
Proof.
  intros. split; [apply scope_implementation_of|].
  intro Scoped. apply scope_of in Scoped.
  rewrite public_namespaces_roundtrip, public_entities_roundtrip in Scoped; exact Scoped.
Qed.

Theorem load_world_exports_exact : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  forall path ns name entity, A.Loaded bundle path ->
    (A.w_exp (world_read w requirements) path ns name = Some entity <->
      A.Exports bundle (world_read w requirements) path ns name entity).
Proof.
  intros bundle w requirements Loaded path ns name entity PathLoaded.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  change (option_map entity_of
    (M.w_exp (WorldSound.W_read w requirements (fun _ => 0)) path (implementation_namespace ns) name) = Some entity <->
    A.Exports bundle (world_of (WorldSound.W_read w requirements (fun _ => 0))) path ns name entity).
  rewrite entity_option_iff, exports_iff.
  apply (ComponentContracts.SourceReading.lr_exports _ _ Reading).
  apply loaded_implementation_of; exact PathLoaded.
Qed.

Theorem load_world_scope_exact : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  forall path ns name entity, A.Loaded bundle path ->
    (A.w_scope (world_read w requirements) path ns name = Some entity <->
      A.InScope bundle (world_read w requirements) path ns name entity).
Proof.
  intros bundle w requirements Loaded path ns name entity PathLoaded.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  change (option_map entity_of
    (M.w_scope (WorldSound.W_read w requirements (fun _ => 0)) path (implementation_namespace ns) name) = Some entity <->
    A.InScope bundle (world_of (WorldSound.W_read w requirements (fun _ => 0))) path ns name entity).
  rewrite entity_option_iff, scope_iff.
  apply (ComponentContracts.SourceReading.lr_scope _ _ Reading).
  apply loaded_implementation_of; exact PathLoaded.
Qed.

Theorem load_world_acyclic : forall bundle w,
  LoadWorld.load_world bundle = Sx.Good w ->
  exists rank : string -> nat, forall path module target import,
    A.Loaded bundle path -> A.module_of bundle path = Some module ->
    In (target,import) (A.imports_of bundle module) ->
    A.module_of bundle target <> None /\ rank target < rank path.
Proof.
  intros bundle w Loaded.
  pose proof (WorldSound.load_world_reads bundle w (fun _ => []) (fun _ => 0) Loaded) as Reading.
  destruct (ComponentContracts.SourceReading.lr_acyclic _ _ Reading) as [rank Acyclic].
  exists rank. intros path module target import PathLoaded Module Imported.
  apply (Acyclic path module target import); [apply loaded_implementation_of; exact PathLoaded|exact Module|exact Imported].
Qed.

Lemma module_of_symbol : forall W symbol,
  A.mod_of (world_of W) symbol = M.mod_of W symbol.
Proof.
  intros W symbol. unfold A.mod_of, M.mod_of, world_of; cbn.
  destruct (M.w_decl W symbol); reflexivity.
Qed.

Theorem attached_declaration_iff : forall W receiver name symbol,
  A.AttachedDecl (world_of W) receiver name symbol <->
    M.AttachedDecl W receiver name symbol.
Proof.
  intros W receiver name symbol. split.
  - intros [declaration [rn [ps [rt [body [Found [Ast [Module Scoped]]]]]]]].
    destruct (public_declaration_read W symbol declaration Found) as [raw [Raw ->]].
    exists raw, rn, ps, rt, body. split; [exact Raw|]. split; [exact Ast|].
    split.
    + rewrite <- module_of_symbol; exact Module.
    + change (option_map entity_of (M.w_scope W (M.d_mod raw) M.NSName rn) = Some (A.ESym receiver)) in Scoped.
      apply entity_option_iff in Scoped. exact Scoped.
  - intros [declaration [rn [ps [rt [body [Found [Ast [Module Scoped]]]]]]]].
    exists (declaration_of declaration), rn, ps, rt, body. split.
    + change (option_map declaration_of (M.w_decl W symbol) = Some (declaration_of declaration)).
      rewrite Found; reflexivity.
    + split; [exact Ast|]. split.
      * change (M.d_mod declaration = A.mod_of (world_of W) receiver).
        rewrite module_of_symbol; exact Module.
      * change (option_map entity_of (M.w_scope W (M.d_mod declaration) M.NSName rn) = Some (A.ESym receiver)).
        rewrite Scoped; reflexivity.
Qed.

Theorem load_world_attached_exact : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  forall receiver name symbol,
    A.w_attached (world_read w requirements) receiver name = Some symbol <->
      A.AttachedDecl (world_read w requirements) receiver name symbol.
Proof.
  intros bundle w requirements Loaded receiver name symbol.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  change (M.w_attached (WorldSound.W_read w requirements (fun _ => 0)) receiver name = Some symbol <->
    A.AttachedDecl (world_of (WorldSound.W_read w requirements (fun _ => 0))) receiver name symbol).
  rewrite attached_declaration_iff.
  exact (ComponentContracts.SourceReading.lr_attached _ _ Reading receiver name symbol).
Qed.

Theorem load_world_library : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  A.w_library (world_read w requirements) = Audit.Loader.library_dir (Audit.Input.b_prelude bundle).
Proof.
  intros bundle w requirements Loaded.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  exact (ComponentContracts.SourceReading.lr_library _ _ Reading).
Qed.

(* The loader establishes every other WorldOf clause for any requirements
   table. This assembler isolates exactly the six semantic requirements
   obligations still owed by the elaborator. *)
Theorem load_world_world_of_from_requirements : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  let W := world_read w requirements in
  (forall symbol, NoDup (requirements symbol)) ->
  (forall symbol kind, A.kind_sym W symbol = Some kind ->
    kind <> A.DKTag -> kind <> A.DKAlias -> kind <> A.DKType ->
    forall parameter, In parameter (requirements symbol) <-> A.AssumeTypes W symbol parameter) ->
  (forall symbol, A.kind_sym W symbol = Some A.DKType \/ A.BareTag W symbol ->
    requirements symbol = []) ->
  A.RelevanceClosed W requirements ->
  (forall candidate, A.RelevanceClosed W candidate ->
    (forall symbol, A.has_nominal_body W symbol = false -> candidate symbol = requirements symbol) ->
    forall symbol, A.has_nominal_body W symbol = true ->
      incl (requirements symbol) (candidate symbol)) ->
  (forall symbol declaration parameters payload name,
    A.w_decl W symbol = Some declaration ->
    A.nominal_body (A.d_ast declaration) = Some (parameters,payload) ->
    In name parameters -> exists parameter,
      A.resolve_sym W (A.d_mod declaration) [name] = Some parameter /\
      A.AssumeTypes W symbol parameter) ->
  A.WorldOf bundle W.
Proof.
  intros bundle w requirements Loaded W NoDuplicates Plain NoneRequired Closed Least Listed.
  constructor.
  - exact (load_world_acyclic bundle w Loaded).
  - intros symbol declaration Found.
    destruct (load_world_declarations bundle w requirements symbol declaration Loaded Found)
      as [_ Provenance]. exact Provenance.
  - intros declaration Declared Kind.
    exact (load_world_declaration_complete bundle w requirements declaration Loaded Declared Kind).
  - exact (load_world_exports_exact bundle w requirements Loaded).
  - exact (load_world_scope_exact bundle w requirements Loaded).
  - exact (load_world_attached_exact bundle w requirements Loaded).
  - exact NoDuplicates.
  - exact Plain.
  - exact NoneRequired.
  - exact Closed.
  - exact Least.
  - exact Listed.
  - exact (load_world_library bundle w requirements Loaded).
Qed.
