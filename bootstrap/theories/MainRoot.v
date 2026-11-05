(* ------------------------------------------------------------------------ *)
(* MainRoot.v — `main` and its root context (Track A, CtxOf from fn_info). *)
(*                                                                           *)
(* 1. A name a loaded module has in scope, for a symbol that module         *)
(*    declares, is the name the declaration is filed under (imports come    *)
(*    from modules of lower rank).                                          *)
(* 2. Elab.is_main against Part V's IsMainFn, for a symbol of the loader's: *)
(*    a fn without receiver that its own module has in scope as `main`.     *)
(*    Like IsMainFn, it does not count a `main` imported under that name    *)
(*    (Audit note 41, open).                                                *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrame ReqsTotal BodySound BodyTy Merge CtxWalk CtxMerge InfoCache.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

(* ======================================================================== *)
(* 1. A module's own names                                                   *)
(* ======================================================================== *)

Section Scope.
Variables (b : bundle) (W : A.world).
Variable rank : string -> nat.
Hypothesis Hrank : forall p m t imp, A.Loaded b p -> A.module_of b p = Some m -> In (t, imp) (A.imports_of b m) ->
  A.module_of b t <> None /\ rank t < rank p.

Fixpoint exports_rank p ns n e (H : A.Exports b W p ns n e) {struct H} : A.Loaded b p ->
  forall s d, e = A.ESym s -> A.w_decl W s = Some d ->
  (A.d_mod d = p /\ A.filed_name (A.d_ast d) = Some (ns, n)) \/ rank (A.d_mod d) < rank p.
Proof.
  destruct H as [p s0 d0 ns n D M F|p m t imp u e ns Hm Hin Hg Hu Ens Hx|p m t imp a Hm Hin Ha];
    intros Hl s d Es Hd.
  - injection Es as ->. rewrite D in Hd. injection Hd as <-. left. auto.
  - right. destruct (Hrank p m t imp Hl Hm Hin) as [_ Lt].
    pose proof (@A.LoadImport b p m t imp Hl Hm Hin) as Hlt.
    destruct Hx as [Hx|(_ & _ & Hx)];
      (destruct (exports_rank _ _ _ _ Hx Hlt s d Es Hd) as [[E _]|L]; [rewrite E; exact Lt | lia]).
  - discriminate.
Qed.

(* a symbol in scope in a loaded module p under n, declared in p, is filed
   there under n *)
Lemma scope_own : forall p ns n s d, A.Loaded b p -> A.InScope b W p ns n (A.ESym s) ->
  A.w_decl W s = Some d -> A.d_mod d = p -> A.filed_name (A.d_ast d) = Some (ns, n).
Proof.
  intros p ns n s d Hl H Hd Hm. remember (A.ESym s) as es eqn:Es. destruct H as [p ns n e Hx|p m t imp ns n e Hm' Hin Hg Hx].
  - destruct (exports_rank _ _ _ _ Hx Hl s d Es Hd) as [[_ F]|L]; [exact F | rewrite Hm in L; lia].
  - exfalso. destruct (Hrank p m t imp Hl Hm' Hin) as [_ Lt].
    destruct (exports_rank _ _ _ _ Hx (@A.LoadImport b p m t imp Hl Hm' Hin) s d Es Hd) as [[E _]|L]; [rewrite Hm in E; subst t; lia | rewrite Hm in L; lia].
Qed.

End Scope.

(* ======================================================================== *)
(* 2. is_main against IsMainFn                                               *)
(* ======================================================================== *)

Lemma load_world_recv : forall b w, load_world b = Good w ->
  forall x g, nget (w_syms w) x = Some g -> gs_recv g = None \/ gs_kind g = KMethod.
Proof.
  intros b w H x g Hg. destruct (load_world_inv b w H) as (ls & e & L & ->). exact (li_recv _ _ L x g Hg).
Qed.

(* a symbol is filed under its name in its own module's table *)
Lemma load_world_names : forall b w, load_world b = Good w ->
  forall x g n, nget (w_syms w) x = Some g -> A.filed_name (gs_decl g) = Some (A.NSName, n) ->
  sassoc n (gm_names (get_mod w (gs_mod g))) = Some x.
Proof.
  intros b w H x g n Hg Hf. destruct (load_world_inv b w H) as (ls & e & L & ->).
  destruct (li_symfacts _ _ L x g Hg) as (m & p & am & Em & _ & _ & _ & _ & Hn & _).
  unfold get_mod. cbn [w_mods]. rewrite Em. exact (Hn n Hf).
Qed.

Section Main.
Variable w : Elab.world. Context {P : xprogram}. Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.
Notation WW := (BodySound.W w R Fl).

Lemma kind_fn_decl : forall x g, nget (w_syms w) x = Some g -> gs_kind g = KFn ->
  exists n ps rt body, gs_decl g = ADFn RPlain n ps rt body.
Proof.
  intros x g Hg K. pose proof (load_world_kind b w Hload x g Hg) as KO. unfold KindOK in KO. rewrite K in KO.
  destruct (gs_decl g) as [| | | | | |[| |] n ps rt body| |]; cbn in KO; try discriminate. eauto.
Qed.

Lemma scope_alias_mod : forall p n e, A.w_scope WW p A.NSAlias n = Some e -> exists q, e = A.EMod q.
Proof.
  intros p n e H. cbn [A.w_scope BodySound.W W_read] in H. destruct (find_mod w p (w_order w)); [|discriminate].
  destruct (sassoc n _); cbn in H; [injection H as <-; eauto | discriminate].
Qed.

(* is_main reads IsMainFn, for a symbol of the loader's *)
Theorem is_main_spec : forall g s m s', nget (w_syms w) (gs_id g) = Some g -> st_w s = w ->
  is_main g s = Good (m, s') -> (m = true <-> A.IsMainFn WW (gs_id g)).
Proof.
  intros g s m s' Hg Hw H. pose proof (w_decl_rd w R Fl _ _ Hg) as Wd.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hmi.
  unfold is_main in H.
  destruct (negb (kind_eqb (gs_kind g) KFn) || is_some (gs_recv g)) eqn:Ek.
  - injection H as <- _. split; [discriminate|]. intros (d & ps & rt & body & D & Da & _).
    rewrite Wd in D. injection D as <-. cbn [A.d_ast] in Da.
    pose proof (load_world_kind b w Hload _ _ Hg) as KO. unfold KindOK in KO. rewrite Da in KO.
    cbn in KO. injection KO as KO. change A.DKFn with (kconv KFn) in KO. apply kconv_inj in KO.
    destruct (load_world_recv b w Hload _ _ Hg) as [Rn|Km]; [|rewrite <- KO in Km; discriminate].
    rewrite <- KO, Rn in Ek. cbn in Ek. discriminate.
  - apply orb_false_iff in Ek as [Ek Er]. apply negb_false_iff, kind_eqb_spec in Ek.
    unfold the_mod in H. apply bind_inv in H as (gm & s1 & E1 & H). injection E1 as <- <-. rewrite Hw in H.
    injection H as <- _.
    destruct (kind_fn_decl _ _ Hg Ek) as (n & ps & rt & body & Dg).
    split.
    + intro Em. destruct (sassoc "main"%string (gm_names (get_mod w (gs_mod g)))) as [x|] eqn:Ex; [|discriminate].
      apply Nat.eqb_eq in Em. subst x.
      assert (Rp : Elab.resolve_path w (gs_mod g) ["main"%string] = Good (Some (Elab.ESym (gs_id g))))
        by (unfold Elab.resolve_path; rewrite Ex; reflexivity).
      pose proof (load_world_resolves b w R Fl Hload _ _ _ Hmi Rp) as Ra. cbn [option_map ent_of] in Ra.
      assert (Sc : A.w_scope WW (gm_path (get_mod w (gs_mod g))) A.NSName "main"%string = Some (A.ESym (gs_id g))).
      { unfold A.resolve in Ra. change (W_read w R Fl) with (BodySound.W w R Fl) in Ra. destruct (A.w_scope WW _ A.NSName _) as [e0|] eqn:E0; [exact Ra|].
        exfalso. destruct (A.w_scope WW _ A.NSAlias _) as [e1|] eqn:E1; [|discriminate].
        cbn [A.resolve_rest] in Ra. injection Ra as ->. destruct (scope_alias_mod _ _ _ E1) as (q & Eq). discriminate. }
      destruct (load_world_reads b w R Fl Hload) as [(rank & Hrank) _ _ _ Lsc _].
      pose proof (proj1 (Lsc _ _ _ _ (load_world_loaded b w Hload _ Hmi)) Sc) as Is.
      pose proof (scope_own b WW rank Hrank _ _ _ _ _ (load_world_loaded b w Hload _ Hmi) Is Wd eq_refl) as Fd.
      cbn [A.d_ast] in Fd. rewrite Dg in Fd. cbn in Fd. injection Fd as ->.
      exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)), ps, rt, body.
      split; [exact Wd|]. split; [exact Dg | exact Sc].
    + intros (d & ps' & rt' & body' & D & Da & _). rewrite Wd in D. injection D as <-. cbn [A.d_ast] in Da.
      rewrite (load_world_names b w Hload _ _ "main"%string Hg ltac:(rewrite Da; reflexivity)).
      apply Nat.eqb_refl.
Qed.

(* what fn_info_ctx asks of a function that is not `main` *)
Corollary not_main : forall g, nget (w_syms w) (gs_id g) = Some g -> ~ A.IsMainFn WW (gs_id g) ->
  forall s0 m s1, st_w s0 = w -> is_main g s0 = Good (m, s1) -> m = false.
Proof.
  intros g Hg N s0 m s1 Hw H. destruct m; [|reflexivity].
  exfalso. exact (N (proj1 (is_main_spec g s0 true s1 Hg Hw H) eq_refl)).
Qed.

End Main.
