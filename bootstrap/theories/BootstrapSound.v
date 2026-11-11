(* The executable source-to-program path and its public build contract. *)
From Stdlib Require Import String.
From NM Require Audit AuditBase BaseHost BootstrapBuild EaAssumesSound
  EaBase EaDeclarationSound EaMain EaWorldSound SnapshotLoader Sx.
From NM Require Import LegacyBodyJudgments.
From NM Require BodyProgramTyping.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Module Public := BaseHost.Public.
Module Platform := AuditBase.Platform.

Definition build (prelude : file_path) (snapshot : source) (entry : file_path) : option program :=
  match SnapshotLoader.load_from (source_read snapshot) "/" (path_text prelude) (path_text entry) with
  | Sx.Good bundle => match EaMain.elaborate bundle with
                      | EaBase.Ok (_, program) => Some program
                      | EaBase.Err _ => None
                      end
  | Sx.Bad _ => None
  end.

(* The body-checking contract used to assemble the whole-program proof. *)
Definition BodiesSound : Prop :=
  forall bundle W program,
    EaMain.elaborate bundle = EaBase.Ok (W, program) ->
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
      FnElab W program f) /\
    (forall symbol d n t i, w_decl W symbol = Some d -> d_ast d = ADVal n t (Some i) ->
      ConstElab W program d t i).

Theorem elaborate_sound_from_bodies : BodiesSound ->
  forall bundle W program, EaMain.elaborate bundle = EaBase.Ok (W, program) ->
  ElaboratesIn bundle W program /\ Platform.Linked W program.
Proof.
  intros Bodies bundle W program Run.
  destruct (Bodies _ _ _ Run) as [Functions Constants].
  pose proof (EaWorldSound.elaborate_with_world_sound _ _ _ _ Run) as World.
  destruct (BootstrapBuild.elaborate_main_environment _ _ _ _ Run) as (Main & Entry & Environment).
  destruct (BootstrapBuild.elaborate_platform_sound _ _ _ _ Run) as (_ & _ & Lang & Bool).
  pose proof (BootstrapBuild.elaborate_with_expansion_bounded _ _ _ _ Run) as Expansion.
  pose proof (EaAssumesSound.elaborate_with_assumes_complete _ _ _ _ Run) as Assumes.
  pose proof (EaDeclarationSound.elaborate_declarations_sound _ _ _ _ Run Constants) as Declarations.
  pose proof (BootstrapBuild.elaborate_finite_layouts _ _ _ _ Run) as Layouts.
  pose proof (BootstrapBuild.elaborate_linked _ _ _ _ Run) as Linked.
  split; [|exact Linked].
  apply BodyProgramTyping.separated_elaboration_readback.
  unfold LegacyElaboratesIn. repeat first [assumption | split].
Qed.

Theorem loaded_build_sound_from_bodies : BodiesSound ->
  forall prelude snapshot entry bundle W program,
  Audit.Loader.Bundled (source_read snapshot) "/" (path_text prelude) (path_text entry) bundle ->
  EaMain.elaborate bundle = EaBase.Ok (W, program) ->
  Public.Builds prelude snapshot entry program /\ Public.Compatible program Public.default_interface.
Proof.
  intros Bodies prelude snapshot entry bundle W program Loaded Elaborated.
  destruct (elaborate_sound_from_bodies Bodies _ _ _ Elaborated) as [Typed Linked].
  split.
  - exists bundle, W. exact (conj Loaded (conj Typed Linked)).
  - apply BaseHost.default_compatible. exists bundle, W. exact (conj Typed Linked).
Qed.

Local Opaque SnapshotLoader.load_from EaMain.elaborate.
Theorem build_sound_from_bodies : BodiesSound ->
  forall prelude snapshot entry program,
  build prelude snapshot entry = Some program ->
  Public.Builds prelude snapshot entry program /\ Public.Compatible program Public.default_interface.
Proof.
  intros Bodies prelude snapshot entry program Run. unfold build in Run.
  destruct (SnapshotLoader.load_from (source_read snapshot) "/" (path_text prelude) (path_text entry))
    as [bundle|message] eqn:Loaded; [|discriminate].
  destruct (EaMain.elaborate bundle) as [[W found]|message] eqn:Elaborated; [|discriminate].
  injection Run as <-.
  eapply loaded_build_sound_from_bodies; [exact Bodies| |exact Elaborated].
  eapply SnapshotLoader.load_from_sound. exact Loaded.
Qed.
