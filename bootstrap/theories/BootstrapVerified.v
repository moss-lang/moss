(* Soundness of the executable source-to-program path. *)
From NM Require Import Audit.
From NM Require AuditBase BaseHost BootstrapSound EaBase EaBodyAssembly EaMain.
Import Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Theorem elaborate_sound : forall bundle W program,
  EaMain.elaborate bundle = EaBase.Ok (W, program) ->
  ElaboratesIn bundle W program /\ AuditBase.Platform.Linked W program.
Proof. exact (BootstrapSound.elaborate_sound_from_bodies EaBodyAssembly.elaborate_bodies_sound). Qed.

Theorem build_sound : forall prelude snapshot entry program,
  BootstrapSound.build prelude snapshot entry = Some program ->
  BaseHost.Public.Builds prelude snapshot entry program /\
  BaseHost.Public.Compatible program BaseHost.Public.default_interface.
Proof. exact (BootstrapSound.build_sound_from_bodies EaBodyAssembly.elaborate_bodies_sound). Qed.
