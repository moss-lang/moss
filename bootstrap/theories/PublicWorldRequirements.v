(* Requirement semantics are source-level predicates. Their exact transport
   needs only the public metadata reader, without runtime model refinement. *)
From Stdlib Require Import List String.
From NM Require AuditBase.
From NM Require Import Audit PublicWorld PublicWorldScope.
From NM Require ExecutionModel ComponentContracts.
From NM Require Import WorldSound Sx.
Import ListNotations.
Module A := AuditBase.Semantics.Elaboration.
Module M := ExecutionModel.Elaboration.

Definition public_kind (kind : M.dkind) : A.dkind :=
  match kind with
  | M.DKType => A.DKType | M.DKUnit | M.DKTag => A.DKTag | M.DKAlias => A.DKAlias
  | M.DKVal => A.DKVal | M.DKFn => A.DKFn
  | M.DKCtx => A.DKCtx | M.DKMeth => A.DKMeth | M.DKFunctor => A.DKFunctor
  end.

Lemma kind_symbol_of : forall W symbol,
  A.kind_sym (world_of W) symbol = option_map public_kind (M.kind_sym W symbol).
Proof.
  intros W symbol. unfold A.kind_sym, M.kind_sym, world_of; cbn.
  destruct (M.w_decl W symbol) as [[path ast assumes]|]; [|reflexivity].
  destruct ast; try match goal with recv : Audit.Input.fnrecv |- _ => destruct recv end;
    reflexivity.
Qed.

Lemma kind_symbol_iff : forall W symbol,
  A.kind_sym (world_of W) symbol = Some A.DKType <-> M.kind_sym W symbol = Some M.DKType.
Proof.
  intros W symbol. rewrite kind_symbol_of.
  destruct (M.kind_sym W symbol) as [[]|]; split; intro Equal; discriminate || reflexivity.
Qed.

Lemma bare_tag_iff : forall W symbol,
  A.BareTag (world_of W) symbol <-> M.kind_sym W symbol = Some M.DKUnit.
Proof.
  intros W symbol. unfold A.BareTag, A.ast_of, M.kind_sym, world_of; cbn.
  destruct (M.w_decl W symbol) as [[path ast assumes]|]; [|split; [intros [? H]|intro H]; discriminate].
  destruct ast as [| | | |bare_name| |[]| |]; cbn;
    split; try solve [intros [? H]; discriminate || reflexivity];
    intro H; try discriminate; eexists; reflexivity.
Qed.

Lemma has_nominal_body_of : forall W symbol,
  A.has_nominal_body (world_of W) symbol =
    match M.kind_sym W symbol with Some M.DKTag | Some M.DKAlias => true | _ => false end.
Proof.
  intros W symbol. unfold A.has_nominal_body, A.ast_of, M.kind_sym, world_of; cbn.
  destruct (M.w_decl W symbol) as [[path ast assumes]|]; [|reflexivity].
  destruct ast; try match goal with recv : Audit.Input.fnrecv |- _ => destruct recv end;
    reflexivity.
Qed.

Lemma resolve_symbol_of : forall W module path,
  A.resolve_sym (world_of W) module path = M.resolve_sym W module path.
Proof.
  intros W module path. unfold A.resolve_sym, M.resolve_sym. rewrite resolve_of.
  destruct (M.resolve W module path) as [[symbol|target]|]; reflexivity.
Qed.

Theorem spec_types_of : forall W module spec symbol,
  M.SpecTypes W module spec symbol -> A.SpecTypes (world_of W) module spec symbol.
Proof.
  intros W module spec symbol Typed. induction Typed as
    [module path args symbol Resolved Kind |
     module path args context declaration name items spec symbol Resolved Found Ast Member Typed IH].
  - apply A.STType; [rewrite resolve_symbol_of; exact Resolved|].
    apply kind_symbol_iff; exact Kind.
  - eapply A.STCtx with (d:=declaration_of declaration).
    + rewrite resolve_symbol_of; exact Resolved.
    + change (option_map declaration_of (M.w_decl W context) = Some (declaration_of declaration)).
      rewrite Found; reflexivity.
    + exact Ast.
    + exact Member.
    + exact IH.
Qed.

Theorem spec_types_implementation_of : forall W module spec symbol,
  A.SpecTypes (world_of W) module spec symbol -> M.SpecTypes W module spec symbol.
Proof.
  intros W module spec symbol Typed. induction Typed as
    [module path args symbol Resolved Kind |
     module path args context declaration name items spec symbol Resolved Found Ast Member Typed IH].
  - apply M.STType; [rewrite <- resolve_symbol_of; exact Resolved|].
    exact (proj1 (kind_symbol_iff W symbol) Kind).
  - destruct (public_declaration_read W context declaration Found) as [raw [Raw Equal]].
    subst declaration. eapply M.STCtx with (d:=raw).
    + rewrite <- resolve_symbol_of; exact Resolved.
    + exact Raw.
    + exact Ast.
    + exact Member.
    + exact IH.
Qed.

Theorem spec_types_iff : forall W module spec symbol,
  A.SpecTypes (world_of W) module spec symbol <-> M.SpecTypes W module spec symbol.
Proof. split; [apply spec_types_implementation_of|apply spec_types_of]. Qed.

Theorem assume_types_iff : forall W symbol parameter,
  A.AssumeTypes (world_of W) symbol parameter <-> M.AssumeTypes W symbol parameter.
Proof.
  intros W symbol parameter. split.
  - intros [declaration [path [Found [Member Typed]]]].
    destruct (public_declaration_read W symbol declaration Found) as [raw [Raw ->]].
    exists raw, path. split; [exact Raw|]. split; [exact Member|].
    apply spec_types_iff; exact Typed.
  - intros [declaration [path [Found [Member Typed]]]].
    exists (declaration_of declaration), path. split.
    + change (option_map declaration_of (M.w_decl W symbol) = Some (declaration_of declaration)).
      rewrite Found; reflexivity.
    + split; [exact Member|]. apply spec_types_iff; exact Typed.
Qed.

Theorem free_in_of : forall W requirements module ty symbol,
  M.FreeIn W requirements module ty symbol ->
    A.FreeIn (world_of W) requirements module ty symbol.
Proof.
  intros W requirements module ty symbol Free. induction Free as
    [fields name ty symbol Member Free IH | members ty symbol Member Free IH |
     path args symbol Resolved Kind Unbound |
     path args nominal symbol Resolved NotType Required Unbound |
     path args binding spec default sargs symbol Resolved Member Free IH].
  - eapply A.FRec; eassumption.
  - eapply A.FUnion; eassumption.
  - apply A.FType.
    + rewrite resolve_symbol_of; exact Resolved.
    + apply kind_symbol_iff; exact Kind.
    + intros [binding [Member Equal]]. apply Unbound. exists binding. split; [exact Member|].
      rewrite <- resolve_symbol_of; exact Equal.
  - eapply A.FNom.
    + rewrite resolve_symbol_of; exact Resolved.
    + rewrite kind_symbol_iff; exact NotType.
    + exact Required.
    + intros [binding [Member Equal]]. apply Unbound. exists binding. split; [exact Member|].
      rewrite <- resolve_symbol_of; exact Equal.
  - eapply A.FBracket.
    + rewrite resolve_symbol_of; exact Resolved.
    + exact Member.
    + exact IH.
Qed.

Theorem free_in_implementation_of : forall W requirements module ty symbol,
  A.FreeIn (world_of W) requirements module ty symbol ->
    M.FreeIn W requirements module ty symbol.
Proof.
  intros W requirements module ty symbol Free. induction Free as
    [fields name ty symbol Member Free IH | members ty symbol Member Free IH |
     path args symbol Resolved Kind Unbound |
     path args nominal symbol Resolved NotType Required Unbound |
     path args binding spec default sargs symbol Resolved Member Free IH].
  - eapply M.FRec; eassumption.
  - eapply M.FUnion; eassumption.
  - apply M.FType.
    + rewrite <- resolve_symbol_of; exact Resolved.
    + exact (proj1 (kind_symbol_iff W symbol) Kind).
    + intros [binding [Member Equal]]. apply Unbound. exists binding. split; [exact Member|].
      rewrite resolve_symbol_of; exact Equal.
  - eapply M.FNom.
    + rewrite <- resolve_symbol_of; exact Resolved.
    + rewrite <- (kind_symbol_iff W nominal); exact NotType.
    + exact Required.
    + intros [binding [Member Equal]]. apply Unbound. exists binding. split; [exact Member|].
      rewrite resolve_symbol_of; exact Equal.
  - eapply M.FBracket.
    + rewrite <- resolve_symbol_of; exact Resolved.
    + exact Member.
    + exact IH.
Qed.

Theorem free_in_iff : forall W requirements module ty symbol,
  A.FreeIn (world_of W) requirements module ty symbol <->
    M.FreeIn W requirements module ty symbol.
Proof. split; [apply free_in_implementation_of|apply free_in_of]. Qed.

Lemma nominal_body_same : forall declaration,
  A.nominal_body declaration = M.nominal_body declaration.
Proof. destruct declaration; reflexivity. Qed.

Theorem relevance_closed_iff : forall W requirements,
  A.RelevanceClosed (world_of W) requirements <-> M.RelevanceClosed W requirements.
Proof.
  intros W requirements. split.
  - intros Closed symbol declaration parameters payload Found Body.
    assert (Read : A.w_decl (world_of W) symbol = Some (declaration_of declaration)).
    { change (option_map declaration_of (M.w_decl W symbol) = Some (declaration_of declaration)).
      rewrite Found; reflexivity. }
    destruct (Closed symbol (declaration_of declaration) parameters payload Read Body) as [Listed Free].
    split.
    + intros name parameter Member Resolved. apply (Listed name parameter Member).
      rewrite resolve_symbol_of; exact Resolved.
    + intros parameter Occurs. apply Free. apply free_in_iff; exact Occurs.
  - intros Closed symbol declaration parameters payload Found Body.
    destruct (public_declaration_read W symbol declaration Found) as [raw [Raw ->]].
    change (M.nominal_body (M.d_ast raw) = Some (parameters,payload)) in Body.
    destruct (Closed symbol raw parameters payload Raw Body) as [Listed Free]. split.
    + intros name parameter Member Resolved. apply (Listed name parameter Member).
      rewrite <- resolve_symbol_of; exact Resolved.
    + intros parameter Occurs. apply Free. apply free_in_iff; exact Occurs.
Qed.

Theorem load_world_world_of_from_reading : forall bundle w requirements,
  LoadWorld.load_world bundle = Sx.Good w ->
  ComponentContracts.SourceReading.ReqsReading
    (WorldSound.W_read w requirements (fun _ => 0)) ->
  A.WorldOf bundle (world_read w requirements).
Proof.
  intros bundle w requirements Loaded Reading.
  set (W := WorldSound.W_read w requirements (fun _ => 0)) in *.
  apply load_world_world_of_from_requirements; [exact Loaded| | | | | |].
  - exact (ComponentContracts.SourceReading.rr_nodup _ Reading).
  - intros symbol kind Kind NotTag NotAlias NotType parameter.
    change (In parameter (M.w_reqs W symbol) <-> A.AssumeTypes (world_of W) symbol parameter).
    rewrite assume_types_iff.
    change (A.kind_sym (world_of W) symbol = Some kind) in Kind.
    rewrite kind_symbol_of in Kind.
    destruct (M.kind_sym W symbol) as [raw|] eqn:Raw; [|discriminate].
    injection Kind as <-.
    eapply ComponentContracts.SourceReading.rr_plain; [exact Reading|exact Raw|..];
      destruct raw; cbn in *; congruence.
  - intros symbol Kind. apply (ComponentContracts.SourceReading.rr_none _ Reading symbol).
    destruct Kind as [TypeCase|Bare].
    + left. exact (proj1 (kind_symbol_iff W symbol) TypeCase).
    + right. exact (proj1 (bare_tag_iff W symbol) Bare).
  - apply relevance_closed_iff. exact (ComponentContracts.SourceReading.rr_closed _ Reading).
  - intros candidate Closed Agrees symbol Body.
    change (A.has_nominal_body (world_of W) symbol = true) in Body.
    rewrite has_nominal_body_of in Body.
    apply (ComponentContracts.SourceReading.rr_least _ Reading candidate).
    + apply relevance_closed_iff; exact Closed.
    + intros other NotTag NotAlias. apply Agrees.
      change (A.has_nominal_body (world_of W) other = false). rewrite has_nominal_body_of.
      destruct (M.kind_sym W other) as [[]|]; congruence.
    + destruct (M.kind_sym W symbol) as [[]|]; cbn in Body; try discriminate; auto.
  - intros symbol declaration parameters payload name Found Body Member.
    destruct (public_declaration_read W symbol declaration Found) as [raw [Raw Equal]].
    subst declaration.
    destruct (ComponentContracts.SourceReading.rr_listed _ Reading symbol raw parameters payload name Raw Body Member)
      as [parameter [Resolved Assumed]].
    exists parameter. split.
    + change (A.resolve_sym (world_of W) (M.d_mod raw) [name] = Some parameter).
      rewrite resolve_symbol_of; exact Resolved.
    + exact (proj2 (assume_types_iff W symbol parameter) Assumed).
Qed.
