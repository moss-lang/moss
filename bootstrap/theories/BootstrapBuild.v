From Stdlib Require Import String.
From NM Require Audit EaBase EaMain EaWorldSound SnapshotLoader Sx.

Module L := Audit.Loader.
Module E := Audit.Semantics.Elaboration.

(* The first two stages of the driver build the same world as the audit
   semantics on the bundle loaded from source. *)
Theorem load_elaborate_world : forall read root prelude entry bundle world program,
  SnapshotLoader.load_from read root prelude entry = Sx.Good bundle ->
  EaMain.elaborate bundle = EaBase.Ok (world, program) ->
  L.Bundled read root prelude entry bundle /\ E.WorldOf bundle world.
Proof.
  intros read root prelude entry bundle world program Loaded Elaborated.
  split.
  - exact (SnapshotLoader.load_from_sound _ _ _ _ _ Loaded).
  - eapply EaWorldSound.elaborate_with_world_sound. exact Elaborated.
Qed.
