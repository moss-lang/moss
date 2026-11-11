(* The executable initializes source-set fuel from the proved finite budget. *)
From Stdlib Require Import List String Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaDepth EaBudget EaDependencyFuel
  EaContextFuel EaResultBudget EaMain EaWorldSound ElabWorld.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.

Theorem metadata_fuel_sufficient : forall W bound,
  metadata_budget W bound < EaMain.metadata_fuel W bound.
Proof.
  intros W bound. unfold EaMain.metadata_fuel.
  pose proof (Nat.le_max_r EaMain.elab_fuel (S (metadata_budget W bound))). lia.
Qed.

Theorem metadata_fuel_legacy : forall W bound,
  metadata_budget W bound < EaMain.elab_fuel -> EaMain.metadata_fuel W bound = EaMain.elab_fuel.
Proof. intros W bound Enough. unfold EaMain.metadata_fuel. apply Nat.max_l. lia. Qed.

Theorem elaborate_with_legacy_metadata : forall strategy bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> metadata_budget W bound < EaMain.elab_fuel ->
  EaMain.elaborate_with strategy bundle =
    EaMain.elaborate_with_metadata strategy (fun _ _ => EaMain.elab_fuel) bundle.
Proof.
  intros strategy bundle W bound Run Enough.
  unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata. rewrite Run.
  rewrite (metadata_fuel_legacy _ _ Enough). reflexivity.
Qed.

Lemma metadata_budget_context : forall W contexts bound,
  metadata_budget (ElabWorld.with_context W contexts) bound = metadata_budget W bound.
Proof. reflexivity. Qed.

Lemma metadata_depth_context : forall W contexts bound,
  metadata_depth (ElabWorld.with_context W contexts) bound = metadata_depth W bound.
Proof. reflexivity. Qed.

Lemma metadata_fuel_context : forall W contexts bound,
  EaMain.metadata_fuel (ElabWorld.with_context W contexts) bound = EaMain.metadata_fuel W bound.
Proof. intros W contexts bound. unfold EaMain.metadata_fuel. rewrite metadata_budget_context. reflexivity. Qed.

Theorem bundle_world_r_metadata_exact : forall bundle W bound,
  EaMain.bundle_world_r bundle = Ok (W, bound) -> forall contexts,
  let W' := ElabWorld.with_context W contexts in
  let fuel := EaMain.metadata_fuel W bound in
  (forall p syntax x, In x (spec_types W' fuel p syntax) <-> SpecTypes W' p syntax x) /\
  (forall symbol x, In x (fun_outputs W' fuel symbol) <-> FunOutputs W' symbol x) /\
  (forall symbol x, In x (mentions W' fuel symbol) <-> Mentions W' symbol x) /\
  (forall symbol x, In x (poly W' fuel symbol) <-> Poly W' symbol x) /\
  (forall symbol x, In x (fixed W' fuel symbol) <-> Fixed W' symbol x) /\
  (forall symbol tm, FixedReadings W' symbol tm (fixed_readings W' fuel symbol tm)) /\
  (forall symbol tm, AppOf W' symbol tm (app_of W' fuel symbol tm)).
Proof.
  intros bundle W bound Run contexts. cbn zeta.
  assert (Bounded : forall symbol d, w_decl (ElabWorld.with_context W contexts) symbol = Some d -> symbol < bound).
  { exact (EaWorldSound.bundle_world_r_bound _ _ _ Run). }
  pose proof (metadata_fuel_sufficient W bound) as Enough.
  assert (Bound : bound < EaMain.metadata_fuel W bound).
  { unfold metadata_budget, metadata_depth in Enough.
    pose proof (Nat.le_max_l bound (largest (fun symbol => match w_decl W symbol with
      Some d => declaration_depth (d_ast d) | None => 0 end) (seq 0 bound))). lia. }
  split.
  - intros p syntax x. apply (spec_types_exact _ bound Bounded); exact Bound.
  - split.
    + intros symbol x. apply (fun_outputs_exact _ bound Bounded); exact Bound.
    + split.
      * intros symbol x. apply (mentions_exact _ bound Bounded); exact Enough.
      * split.
        -- intros symbol x. apply (poly_exact _ bound Bounded); exact Enough.
        -- split.
           ++ intros symbol x. apply (fixed_exact _ bound Bounded); exact Enough.
           ++ split.
              ** intros symbol tm. apply (fixed_readings_exact _ bound Bounded); exact Enough.
              ** intros symbol tm. apply app_of_sound.
                 pose proof (signatures_depth_bounded _ bound Bounded symbol) as Depth.
                 unfold metadata_budget in Enough. rewrite metadata_depth_context in Depth. lia.
Qed.
