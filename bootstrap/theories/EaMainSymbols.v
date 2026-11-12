(* The executable main enumeration reads exactly the root-context predicate.
   WorldOf constrains scopes only on loaded paths, so completeness uses the
   actual loader's stronger fact that every nonempty scope is a bundle path. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypesSound EaMain EaWorldSound NMap LoadWorld Sx.
From NM Require PublicWorld WorldSound ReqsSolve ElabWorld.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Lemma uniq_members : forall x xs, In x (uniq xs) <-> In x xs.
Proof.
  intros x xs. unfold uniq. rewrite <- in_rev.
  assert (Fold : forall ys acc,
    In x (fold_left (fun acc y => if nmem y acc then acc else y :: acc) ys acc)
      <-> In x ys \/ In x acc).
  { intro ys. induction ys as [|y ys IH]; intro acc; cbn; [tauto|].
    rewrite IH. destruct (nmem y acc) eqn:Member; cbn.
    - apply nmem_spec in Member. intuition congruence.
    - tauto. }
  rewrite Fold. cbn. tauto.
Qed.

Lemma module_main_read : forall W p s,
  In s (EaMain.module_main W p) <->
  exists d n ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).
Proof.
  intros W p s. split.
  - unfold EaMain.module_main. intro Member.
    destruct (w_scope W p NSName "main") as [[symbol|path]|] eqn:Scope; try contradiction.
    unfold ast_of in Member. destruct (w_decl W symbol) as [d|] eqn:Declaration; try contradiction.
    cbn in Member. destruct (d_ast d) as [items declarations|name|name syntax|name names arity syntax|
      name|name syntax initial|receiver name parameters result body|name items|
      name names inputs outputs body] eqn:Ast; try contradiction.
    destruct receiver; try contradiction. destruct Member as [<-|[]].
    exists d, name, parameters, result, body. auto.
  - intros (d & name & parameters & result & body & Declaration & Ast & Scope).
    unfold EaMain.module_main, ast_of. rewrite Scope, Declaration. cbn [option_map]. rewrite Ast. left. reflexivity.
Qed.

Theorem main_symbols_read : forall W modules s,
  In s (EaMain.main_symbols W modules) <->
  exists p d n ps rt body, In p modules /\
    w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).
Proof.
  intros W modules s. unfold EaMain.main_symbols. rewrite uniq_members, in_flat_map.
  split.
  - intros (p & Member & Read). apply module_main_read in Read as (d & n & ps & rt & body & Read).
    exists p, d, n, ps, rt, body. auto.
  - intros (p & d & n & ps & rt & body & Member & Read).
    exists p. split; [exact Member|]. apply module_main_read.
    exists d, n, ps, rt, body. exact Read.
Qed.

Theorem main_symbols_exact : forall W modules,
  (forall p s, w_scope W p NSName "main" = Some (ESym s) -> In p modules) ->
  forall s, In s (EaMain.main_symbols W modules) <-> IsMainFn W s.
Proof.
  intros W modules Paths s. rewrite main_symbols_read. split.
  - intros (p & d & n & ps & rt & body & Member & Read).
    exists d, n, ps, rt, body, p. exact Read.
  - intros (d & n & ps & rt & body & p & Declaration & Ast & Scope).
    exists p, d, n, ps, rt, body. split; [exact (Paths p s Scope)|].
    auto.
Qed.

Lemma module_of_path : forall b p m,
  module_of b p = Some m -> In p (map am_path (b_mods b)).
Proof.
  intros b p m Found. unfold module_of in Found. apply find_some in Found as [Member Path].
  apply String.eqb_eq in Path. apply in_map_iff. exists m. auto.
Qed.

Lemma find_mod_path : forall w p order m,
  WorldSound.find_mod w p order = Some m ->
  exists i, In i order /\ LoadWorld.get_mod w i = m /\ gm_path m = p.
Proof.
  intros w p order. induction order as [|i order IH]; intros m Found; [discriminate|].
  cbn [WorldSound.find_mod] in Found.
  destruct (String.eqb (gm_path (LoadWorld.get_mod w i)) p) eqn:Path.
  - injection Found as <-. apply String.eqb_eq in Path. exists i.
    split; [left; reflexivity|]. split; [reflexivity|exact Path].
  - destruct (IH _ Found) as (j & Member & Read & Path'). exists j.
    split; [right; exact Member|]. auto.
Qed.

Theorem load_world_scope_path : forall b w requirements p ns name entity,
  LoadWorld.load_world b = Sx.Good w ->
  w_scope (PublicWorld.world_read w requirements) p ns name = Some entity ->
  In p (map am_path (b_mods b)).
Proof.
  intros b w requirements p ns name entity Loaded Scope.
  unfold PublicWorld.world_read, PublicWorld.world_of, WorldSound.W_read in Scope.
  cbn in Scope.
  destruct (WorldSound.find_mod w p (w_order w)) as [m|] eqn:Found; [|discriminate].
  destruct (find_mod_path _ _ _ _ Found) as (i & Index & Read & Path).
  destruct (ReqsSolve.load_world_inv _ _ Loaded) as (state & entry & Inv & World).
  subst w. cbn [w_order] in Index.
  apply (WorldSound.li_order _ _ Inv) in Index as (q & Present).
  destruct (WorldSound.li_loaded _ _ Inv q i Present) as (_ & actual & source & Actual & Name & Source & _).
  unfold LoadWorld.get_mod in Read. cbn [w_mods] in Read. rewrite Actual in Read.
  cbn [opt_default] in Read. subst actual. rewrite Path in Name. subst q.
  apply (module_of_path _ _ _ Source).
Qed.

Theorem bundle_world_r_main_symbols_exact : forall b W bound,
  EaMain.bundle_world_r b = Ok (W, bound) ->
  forall s, In s (EaMain.main_symbols W (map am_path (b_mods b))) <-> IsMainFn W s.
Proof.
  intros b W bound Run. unfold EaMain.bundle_world_r in Run.
  destruct (LoadWorld.load_world b) as [loaded|message] eqn:Loaded; [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-. apply main_symbols_exact.
  intros p s Scope. exact (load_world_scope_path _ _ _ _ _ _ _ Loaded Scope).
Qed.

Theorem elaborate_with_main_symbols_exact : forall strategy b W program,
  EaMain.elaborate_with strategy b = Ok (W, program) ->
  forall s, In s (EaMain.main_symbols W (map am_path (b_mods b))) <-> IsMainFn W s.
Proof.
  intros strategy b W program Run. unfold EaMain.elaborate_with, EaMain.elaborate_with_metadata in Run.
  destruct (EaMain.bundle_world_r b) as [[source_world bound]|message] eqn:WorldRead;
    [|discriminate].
  repeat match type of Run with
  | context [match ?step with _ => _ end] => destruct step eqn:?; try discriminate
  end.
  injection Run as <- <-.
  exact (bundle_world_r_main_symbols_exact _ _ _ WorldRead).
Qed.
