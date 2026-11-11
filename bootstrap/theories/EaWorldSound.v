(* The executable world builder preserves the loader's reading and bounds. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap LoadWorld Sx EaBase EaMain ElabWorld PublicWorld WorldSound ReqsSolve.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.

Theorem bundle_world_r_sound : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> WorldOf bundle W.
Proof.
  intros bundle W bound Run. apply ElabWorld.bundle_world_sound.
  unfold EaMain.bundle_world_r in Run. unfold ElabWorld.bundle_world.
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-. reflexivity.
Qed.

Lemma load_world_decl_bounded : forall bundle w requirements,
  LoadWorld.load_world bundle = Good w -> forall symbol declaration,
  w_decl (PublicWorld.world_read w requirements) symbol = Some declaration ->
  symbol < w_nsyms w.
Proof.
  intros bundle w requirements Loaded symbol declaration Found.
  destruct (ReqsSolve.load_world_inv _ _ Loaded) as (state & entry & Inv & World).
  subst w. unfold PublicWorld.world_read, PublicWorld.world_of, WorldSound.W_read in Found.
  cbn in Found |- *.
  destruct (nget (ls_syms state) symbol) as [source|] eqn:Source; [|discriminate].
  exact (proj1 (proj2 (WorldSound.li_syms _ _ Inv symbol source Source))).
Qed.

Theorem bundle_world_r_bound : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> forall symbol declaration,
  w_decl W symbol = Some declaration -> symbol < bound.
Proof.
  intros bundle W bound Run. unfold EaMain.bundle_world_r in Run.
  destruct (LoadWorld.load_world bundle) as [loaded|message] eqn:Loaded; [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-. exact (load_world_decl_bounded _ _ _ Loaded).
Qed.

From NM Require Import EaContextFuel.

Corollary bundle_world_r_context_readers_exact : forall bundle W bound fuel,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> bound < fuel ->
  (forall p syntax x, In x (EaTypes.spec_types W fuel p syntax) <-> SpecTypes W p syntax x) /\
  (forall symbol x, In x (EaTypes.fun_outputs W fuel symbol) <-> FunOutputs W symbol x).
Proof.
  intros bundle W bound fuel Run Enough. split.
  - intros p syntax x. apply (EaContextFuel.spec_types_exact W bound).
    + exact (bundle_world_r_bound _ _ _ Run).
    + exact Enough.
  - intros symbol x. apply (EaContextFuel.fun_outputs_exact W bound).
    + exact (bundle_world_r_bound _ _ _ Run).
    + exact Enough.
Qed.

Theorem elaborate_with_world_sound : forall strategy bundle W program,
  EaMain.elaborate_with strategy bundle = Ok (W, program) -> WorldOf bundle W.
Proof.
  intros strategy bundle W program Run. unfold EaMain.elaborate_with in Run.
  destruct (EaMain.bundle_world_r bundle) as [[source_world bound]|message] eqn:WorldRead;
    [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-.
  apply ElabWorld.world_of_context. exact (bundle_world_r_sound _ _ _ WorldRead).
Qed.
