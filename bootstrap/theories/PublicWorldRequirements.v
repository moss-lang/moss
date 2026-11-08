(* Requirement semantics are source-level predicates. Their exact transport
   needs only the public metadata reader, without runtime model refinement. *)
From Stdlib Require Import List String.
From NM Require Import Audit PublicWorld PublicWorldScope.
From NM Require ExecutionModel ComponentContracts.
From NM Require Import Elab WorldSound ReqsTotal ReqsSolve ElabFrame ElabSound NMap Sx.
Import ListNotations.
Module A := Audit.Semantics.Elaboration.
Module M := ExecutionModel.Elaboration.

Definition implementation_kind (kind : A.dkind) : M.dkind :=
  match kind with
  | A.DKType => M.DKType | A.DKUnit => M.DKUnit | A.DKAlias => M.DKAlias
  | A.DKTag => M.DKTag | A.DKVal => M.DKVal | A.DKFn => M.DKFn
  | A.DKCtx => M.DKCtx | A.DKMeth => M.DKMeth | A.DKFunctor => M.DKFunctor
  end.

Lemma kind_symbol_iff : forall W symbol kind,
  A.kind_sym (world_of W) symbol = Some kind <->
    M.kind_sym W symbol = Some (implementation_kind kind).
Proof.
  intros W symbol kind. unfold A.kind_sym, M.kind_sym, world_of; cbn.
  destruct (M.w_decl W symbol) as [[path ast assumes]|]; [|split; discriminate].
  destruct ast; try match goal with recv : Audit.Input.fnrecv |- _ => destruct recv end;
    destruct kind; cbn [declaration_of A.d_ast M.d_ast A.kind_of M.kind_of implementation_kind];
    split; intro Equal; try discriminate; reflexivity.
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
    exact (proj1 (kind_symbol_iff W symbol A.DKType) Kind).
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
    + exact (proj1 (kind_symbol_iff W symbol A.DKType) Kind).
    + intros [binding [Member Equal]]. apply Unbound. exists binding. split; [exact Member|].
      rewrite resolve_symbol_of; exact Equal.
  - eapply M.FNom.
    + rewrite <- resolve_symbol_of; exact Resolved.
    + rewrite <- (kind_symbol_iff W nominal A.DKType); exact NotType.
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
  Elab.load_world bundle = Sx.Good w ->
  ComponentContracts.SourceReading.ReqsReading
    (WorldSound.W_read w requirements (fun _ => 0)) ->
  A.WorldOf bundle (world_read w requirements).
Proof.
  intros bundle w requirements Loaded Reading.
  set (W := WorldSound.W_read w requirements (fun _ => 0)) in *.
  apply load_world_world_of_from_requirements; [exact Loaded| | | | | |].
  - exact (ComponentContracts.SourceReading.rr_nodup _ Reading).
  - intros symbol kind Kind NotTag NotAlias NotType NotUnit parameter.
    change (In parameter (M.w_reqs W symbol) <-> A.AssumeTypes (world_of W) symbol parameter).
    rewrite assume_types_iff.
    apply (ComponentContracts.SourceReading.rr_plain _ Reading symbol (implementation_kind kind)).
    + apply kind_symbol_iff; exact Kind.
    + destruct kind; cbn in *; congruence.
    + destruct kind; cbn in *; congruence.
    + destruct kind; cbn in *; congruence.
    + destruct kind; cbn in *; congruence.
  - intros symbol Kind. apply (ComponentContracts.SourceReading.rr_none _ Reading symbol).
    destruct Kind as [TypeCase|UnitCase].
    + left. exact (proj1 (kind_symbol_iff W symbol A.DKType) TypeCase).
    + right. exact (proj1 (kind_symbol_iff W symbol A.DKUnit) UnitCase).
  - apply relevance_closed_iff. exact (ComponentContracts.SourceReading.rr_closed _ Reading).
  - intros candidate Closed Agrees symbol Kind.
    apply (ComponentContracts.SourceReading.rr_least _ Reading candidate).
    + apply relevance_closed_iff; exact Closed.
    + intros other NotTag NotAlias. apply Agrees.
      * intro Tag. apply NotTag. exact (proj1 (kind_symbol_iff W other A.DKTag) Tag).
      * intro Alias. apply NotAlias. exact (proj1 (kind_symbol_iff W other A.DKAlias) Alias).
    + destruct Kind as [Tag|Alias].
      * left. exact (proj1 (kind_symbol_iff W symbol A.DKTag) Tag).
      * right. exact (proj1 (kind_symbol_iff W symbol A.DKAlias) Alias).
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

(* Extract the requirements milestone directly, before the old model's
   independent field-numbering clause is introduced. *)
Theorem elaborate_requirements_reading : forall bundle program,
  Elab.elaborate bundle = Sx.Good program ->
  exists w state requirements,
    Elab.load_world bundle = Sx.Good w /\
    Elab.elab_program (ElabFrame.s_init w) = Sx.Good (program,state) /\
    (forall symbol parameters, NMap.nget (Elab.st_reqs state) symbol = Some parameters ->
      requirements symbol = parameters) /\
    ComponentContracts.SourceReading.ReqsReading
      (WorldSound.W_read w requirements (fun _ => 0)).
Proof.
  intros b p Accepted.
  destruct (ElabFrame.elaborate_frame b p Accepted)
    as (w & s & Loaded & Elaborated & Sw & Sk & _ & _ & _ & Hq & Hrc & Hrl).
  destruct (ReqsSolve.load_world_inv b w Loaded) as (ls & e & HI & Ew).
  exists w, s, (ReqsTotal.Rfin b ls e (Elab.st_reqs s)).
  split; [exact Loaded|]. split; [exact Elaborated|]. split.
  { intros symbol parameters Found. unfold ReqsTotal.Rfin. rewrite Found; reflexivity. }
  rewrite Ew in Sk, Hrc, Hrl, Elaborated |- *. rewrite Ew in Sw.
  apply (ReqsTotal.reqs_reading b ls e HI (Elab.st_syms s));
    [exact Sk | | exact Hrc | exact Hrl |].
  - intros symbol parameters Found.
    destruct (Hq symbol parameters Found) as (Unique & declaration & FoundDecl & Plain & Empty).
    split; [exact Unique|]. exists declaration. split; [exact FoundDecl|].
    rewrite Sw in Plain. split; [exact Plain|exact Empty].
  - unfold Elab.elab_program in Elaborated.
    apply ElabSound.bind_inv in Elaborated as (u & s1 & Checked & _).
    apply (ReqsTotal.check_listed_ok b ls e HI
      (ElabFrame.s_init (Elab.World (Elab.ls_mods ls) (Elab.ls_order ls) e
        (Elab.ls_syms ls) (Elab.ls_next_sym ls) (Elab.ls_files ls)
        (Audit.Loader.library_dir (Audit.Input.b_prelude b)))) u s1);
      [split; [reflexivity|intros symbol declaration Found; exact Found]|exact Checked].
Qed.

Theorem elaborate_public_world_of : forall bundle program,
  Elab.elaborate bundle = Sx.Good program ->
  exists w state requirements,
    Elab.load_world bundle = Sx.Good w /\
    Elab.elab_program (ElabFrame.s_init w) = Sx.Good (program,state) /\
    (forall symbol parameters, NMap.nget (Elab.st_reqs state) symbol = Some parameters ->
      requirements symbol = parameters) /\
    A.WorldOf bundle (world_read w requirements).
Proof.
  intros bundle program Accepted.
  destruct (elaborate_requirements_reading bundle program Accepted)
    as (w & state & requirements & Loaded & Elaborated & Cached & Reading).
  exists w, state, requirements. split; [exact Loaded|]. split; [exact Elaborated|].
  split; [exact Cached|]. apply load_world_world_of_from_reading; assumption.
Qed.
