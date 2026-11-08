(* A source-world reading for direct public elaboration. The source syntax
   is shared, but declaration/entity/world records are separate datatypes.
   These results transport metadata and path resolution only: they do not
   assert public WorldOf, context formation, or elaboration soundness. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
From NM Require ExecutionModel Elab WorldSound ComponentContracts NMap Sx.
Import ListNotations.
Module A := Audit.Semantics.Elaboration.
Module M := ExecutionModel.Elaboration.

Definition namespace_of (ns : M.nspace) : A.nspace :=
  match ns with M.NSName => A.NSName | M.NSDetached => A.NSDetached | M.NSAlias => A.NSAlias end.
Definition implementation_namespace (ns : A.nspace) : M.nspace :=
  match ns with A.NSName => M.NSName | A.NSDetached => M.NSDetached | A.NSAlias => M.NSAlias end.
Definition declaration_of (d : M.decl) : A.decl :=
  A.Decl (M.d_mod d) (M.d_ast d) (M.d_assumes d).
Definition implementation_declaration (d : A.decl) : M.decl :=
  M.Decl (A.d_mod d) (A.d_ast d) (A.d_assumes d).
Definition entity_of (e : M.ent) : A.ent :=
  match e with M.ESym symbol => A.ESym symbol | M.EMod path => A.EMod path end.
Definition world_of (W : M.world) : A.world :=
  A.World
    (fun symbol => option_map declaration_of (M.w_decl W symbol))
    (fun path ns name => option_map entity_of (M.w_scope W path (implementation_namespace ns) name))
    (fun path ns name => option_map entity_of (M.w_exp W path (implementation_namespace ns) name))
    (M.w_attached W) (M.w_reqs W) (M.w_library W).

(* Field numbering is not part of the public world. Its arbitrary dummy
   value below is ignored by world_of and imposes no injectivity premise. *)
Definition world_read (w : Elab.world) (requirements : nat -> list nat) : A.world :=
  world_of (WorldSound.W_read w requirements (fun _ => 0)).
Definition resolved_entity (w : Elab.world) (entity : Elab.ent) : A.ent :=
  match entity with
  | Elab.ESym symbol => A.ESym symbol
  | Elab.EMod module => A.EMod (Elab.gm_path (Elab.get_mod w module))
  end.

Lemma namespaces_roundtrip : forall ns,
  implementation_namespace (namespace_of ns) = ns.
Proof. destruct ns; reflexivity. Qed.

Lemma declarations_roundtrip : forall declaration,
  declaration_of (implementation_declaration declaration) = declaration.
Proof. destruct declaration; reflexivity. Qed.

Lemma declaration_of_injective : forall left right,
  declaration_of left = declaration_of right -> left = right.
Proof. intros [lm la ls] [rm ra rs] Equal. injection Equal as -> -> ->. reflexivity. Qed.

Lemma kind_none_iff : forall declaration,
  M.kind_of declaration = None <-> A.kind_of declaration = None.
Proof.
  intro declaration; destruct declaration;
    try match goal with receiver : Audit.Input.fnrecv |- _ => destruct receiver end;
    cbn [M.kind_of A.kind_of]; split; intro Equal; try discriminate; reflexivity.
Qed.

Theorem declaration_lookup : forall w requirements symbol declaration,
  NMap.nget (Elab.w_syms w) symbol = Some declaration ->
  A.w_decl (world_read w requirements) symbol =
    Some (A.Decl (Elab.gm_path (Elab.get_mod w (Elab.gs_mod declaration)))
      (Elab.gs_decl declaration) (Elab.gs_assumes declaration)).
Proof.
  intros w requirements symbol declaration Found.
  unfold world_read, world_of, WorldSound.W_read; cbn.
  rewrite Found. reflexivity.
Qed.

Theorem requirement_lookup : forall w requirements symbol,
  A.w_reqs (world_read w requirements) symbol = requirements symbol.
Proof. reflexivity. Qed.

Theorem attached_lookup : forall w requirements receiver name,
  A.w_attached (world_read w requirements) receiver name =
    match NMap.nget (Elab.w_syms w) receiver with
    | Some symbol => Elab.assoc Elab.attached_eqb (receiver,name)
        (Elab.gm_attached (Elab.get_mod w (Elab.gs_mod symbol)))
    | None => None
    end.
Proof. reflexivity. Qed.

Theorem scope_lookup : forall w requirements path ns name,
  A.w_scope (world_read w requirements) path (namespace_of ns) name =
    option_map entity_of
      (M.w_scope (WorldSound.W_read w requirements (fun _ => 0)) path ns name).
Proof. intros; unfold world_read, world_of; cbn. rewrite namespaces_roundtrip. reflexivity. Qed.

Theorem export_lookup : forall w requirements path ns name,
  A.w_exp (world_read w requirements) path (namespace_of ns) name =
    option_map entity_of
      (M.w_exp (WorldSound.W_read w requirements (fun _ => 0)) path ns name).
Proof. intros; unfold world_read, world_of; cbn. rewrite namespaces_roundtrip. reflexivity. Qed.

Lemma resolve_rest_of : forall W path entity,
  A.resolve_rest (world_of W) (entity_of entity) path =
    option_map entity_of (M.resolve_rest W entity path).
Proof.
  intros W path; induction path as [|name rest IH]; intro entity; [reflexivity|].
  destruct entity as [symbol|module]; [reflexivity|].
  cbn [A.resolve_rest M.resolve_rest world_of implementation_namespace entity_of A.w_exp].
  destruct (M.w_exp W module M.NSName name) as [found|] eqn:Name.
  - apply IH.
  - destruct (M.w_exp W module M.NSAlias name) as [found|] eqn:Alias; [apply IH|reflexivity].
Qed.

Theorem resolve_of : forall W module path,
  A.resolve (world_of W) module path = option_map entity_of (M.resolve W module path).
Proof.
  intros W module [|name rest]; [reflexivity|].
  cbn [A.resolve M.resolve world_of implementation_namespace A.w_scope].
  destruct (M.w_scope W module M.NSName name) as [found|] eqn:Name.
  - apply resolve_rest_of.
  - destruct (M.w_scope W module M.NSAlias name) as [found|] eqn:Alias;
      [apply resolve_rest_of|reflexivity].
Qed.

Theorem load_world_resolves : forall bundle w requirements,
  Elab.load_world bundle = Sx.Good w ->
  forall module path entity,
    In module (Elab.w_order w) ->
    Elab.resolve_path w module path = Sx.Good entity ->
    A.resolve (world_read w requirements)
      (Elab.gm_path (Elab.get_mod w module)) path = option_map (resolved_entity w) entity.
Proof.
  intros bundle w requirements Loaded module path entity Member Resolved.
  unfold world_read. rewrite resolve_of.
  rewrite (WorldSound.load_world_resolves bundle w requirements (fun _ => 0)
    Loaded module path entity Member Resolved).
  destruct entity as [entity|]; [destruct entity|]; reflexivity.
Qed.

Lemma loaded_of : forall bundle path, M.Loaded bundle path -> A.Loaded bundle path.
Proof.
  intros bundle path Loaded. induction Loaded.
  - constructor.
  - eapply A.LoadImport; eassumption.
Qed.

Lemma occurs_of : forall declarations assumes declaration,
  M.OccursIn declarations assumes declaration -> A.OccursIn declarations assumes declaration.
Proof.
  intros declarations assumes declaration Occurs. induction Occurs.
  - apply A.OccHere; assumption.
  - eapply A.OccNested; eassumption.
Qed.

Theorem declared_of : forall bundle declaration,
  M.Declared bundle declaration -> A.Declared bundle (declaration_of declaration).
Proof.
  intros bundle declaration [Loaded [module [Found Occurs]]].
  split; [apply loaded_of; exact Loaded|]. exists module.
  split; [exact Found|apply occurs_of; exact Occurs].
Qed.

Lemma loaded_implementation_of : forall bundle path, A.Loaded bundle path -> M.Loaded bundle path.
Proof.
  intros bundle path Loaded. induction Loaded.
  - constructor.
  - eapply M.LoadImport; eassumption.
Qed.

Lemma occurs_implementation_of : forall declarations assumes declaration,
  A.OccursIn declarations assumes declaration -> M.OccursIn declarations assumes declaration.
Proof.
  intros declarations assumes declaration Occurs. induction Occurs.
  - apply M.OccHere; assumption.
  - eapply M.OccNested; eassumption.
Qed.

Theorem declared_implementation_of : forall bundle declaration,
  A.Declared bundle declaration -> M.Declared bundle (implementation_declaration declaration).
Proof.
  intros bundle declaration [Loaded [module [Found Occurs]]].
  split; [apply loaded_implementation_of; exact Loaded|]. exists module.
  split; [exact Found|apply occurs_implementation_of; exact Occurs].
Qed.

Theorem load_world_loaded : forall bundle w,
  Elab.load_world bundle = Sx.Good w ->
  forall module, In module (Elab.w_order w) ->
    A.Loaded bundle (Elab.gm_path (Elab.get_mod w module)).
Proof.
  intros bundle w Loaded module Member. apply loaded_of.
  exact (WorldSound.load_world_loaded bundle w Loaded module Member).
Qed.

Theorem load_world_declarations : forall bundle w requirements symbol declaration,
  Elab.load_world bundle = Sx.Good w ->
  A.w_decl (world_read w requirements) symbol = Some declaration ->
  symbol <> 0 /\ A.Declared bundle declaration /\ A.kind_of (A.d_ast declaration) <> None.
Proof.
  intros bundle w requirements symbol declaration Loaded Found.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  change (option_map declaration_of
    (M.w_decl (WorldSound.W_read w requirements (fun _ => 0)) symbol) = Some declaration) in Found.
  destruct (M.w_decl (WorldSound.W_read w requirements (fun _ => 0)) symbol)
    as [raw|] eqn:Raw; cbn in Found; [|discriminate].
  injection Found as <-.
  destruct (ComponentContracts.SourceReading.lr_decl_sound _ _ Reading symbol raw Raw)
    as [Nonzero [Declared Kind]].
  split; [exact Nonzero|]. split; [apply declared_of; exact Declared|].
  intro PublicNone. apply Kind. apply kind_none_iff. exact PublicNone.
Qed.

Theorem load_world_declaration_complete : forall bundle w requirements declaration,
  Elab.load_world bundle = Sx.Good w ->
  A.Declared bundle declaration -> A.kind_of (A.d_ast declaration) <> None ->
  exists symbol, A.w_decl (world_read w requirements) symbol = Some declaration /\
    forall other, A.w_decl (world_read w requirements) other = Some declaration -> other = symbol.
Proof.
  intros bundle w requirements declaration Loaded Declared Kind.
  pose proof (WorldSound.load_world_reads bundle w requirements (fun _ => 0) Loaded) as Reading.
  destruct (ComponentContracts.SourceReading.lr_decl_complete _ _ Reading
    (implementation_declaration declaration) (declared_implementation_of _ _ Declared))
    as [symbol [Found Unique]].
  { intro RawNone. apply Kind. apply kind_none_iff. exact RawNone. }
  exists symbol. split.
  - change (option_map declaration_of
      (M.w_decl (WorldSound.W_read w requirements (fun _ => 0)) symbol) = Some declaration).
    rewrite Found. cbn. rewrite declarations_roundtrip. reflexivity.
  - intros other Other.
    change (option_map declaration_of
      (M.w_decl (WorldSound.W_read w requirements (fun _ => 0)) other) = Some declaration) in Other.
    destruct (M.w_decl (WorldSound.W_read w requirements (fun _ => 0)) other)
      as [raw|] eqn:Raw; cbn in Other; [|discriminate].
    assert (raw = implementation_declaration declaration).
    { apply declaration_of_injective. rewrite declarations_roundtrip. injection Other; auto. }
    subst raw. apply Unique; exact Raw.
Qed.
