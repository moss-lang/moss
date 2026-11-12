(* ------------------------------------------------------------------------ *)
(* ElabWorld.v — the static world of a bundle, computed (elaborator E-M1).   *)
(*                                                                           *)
(* `world b` reads a bundle as Audit's `world`: the loader's tables          *)
(* (LoadWorld.load_world), every symbol's requirement list, and the check that   *)
(* a tag lists only types its nesting requires (LoadListed.check). The     *)
(* requirement lists are the least fixed points ReqsTotal describes, run    *)
(* with an empty cache, so they depend on nothing the elaborator does.      *)
(* ReqsTotal's Kleene iteration `it` is a specification: as a function it   *)
(* recomputes every level, so it is exponential in the number of rounds.    *)
(* Here each round is tabulated in an NMap and the iteration stops at the   *)
(* first round that adds nothing, which may come before ReqsTotal's bound;  *)
(* the lists then have the same members as ReqsTotal's (`kfix_same`), in    *)
(* possibly another order, which WorldOf leaves free.                       *)
(*                                                                           *)
(* Main result: `world_sound`, whenever `world b = Some W`,                 *)
(* `Audit.WorldOf b W`.  `w_context` is left out (every symbol `None`):     *)
(* WorldOf does not constrain it, and `world_of_context` puts any table in  *)
(* its place without changing WorldOf (design §1.1).                        *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require ExecutionModel ComponentContracts.
From NM Require Import NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsTotal ElabFrameCore LoadListedFacts.
From NM Require PublicWorld PublicWorldRequirements.
Import ListNotations.

Module M := ExecutionModel.Elaboration.
Module AE := AuditBase.Semantics.Elaboration.

(* ======================================================================== *)
(* 1. Kleene iteration, tabulated                                           *)
(* ======================================================================== *)

(* Two lists with the same members. *)
Definition same (a b : list nat) : Prop := incl a b /\ incl b a.

Definition look (T : nmap (list nat)) (x : nat) : list nat :=
  match nget T x with Some l => l | None => [] end.

(* The table of f below N. *)
Definition tab (N : nat) (f : nat -> list nat) : nmap (list nat) :=
  fold_left (fun T x => nset T x (f x)) (seq 0 N) NE.

Lemma look_tab_go : forall f n s T x,
  look (fold_left (fun T x => nset T x (f x)) (seq s n) T) x =
  if (s <=? x) && (x <? s + n) then f x else look T x.
Proof.
  intros f n. induction n as [|n IH]; intros s T x.
  - cbn [seq fold_left].
    destruct (Nat.leb_spec s x), (Nat.ltb_spec x (s + 0)); cbn; try reflexivity; lia.
  - cbn [seq fold_left]. rewrite IH.
    assert (Hlook : look (nset T s (f s)) x = if Nat.eqb x s then f s else look T x).
    { unfold look. rewrite nget_nset. destruct (Nat.eqb x s); reflexivity. }
    rewrite Hlook.
    destruct (Nat.eqb_spec x s) as [E|Hne].
    + subst x. replace (S s <=? s) with false by (symmetry; apply Nat.leb_gt; lia).
      replace (s <=? s) with true by (symmetry; apply Nat.leb_le; lia).
      replace (s <? s + S n) with true by (symmetry; apply Nat.ltb_lt; lia). reflexivity.
    + destruct (Nat.leb_spec (S s) x), (Nat.leb_spec s x), (Nat.ltb_spec x (S s + n)), (Nat.ltb_spec x (s + S n));
        cbn; try reflexivity; lia.
Qed.

Lemma look_tab : forall N f x, look (tab N f) x = if x <? N then f x else [].
Proof.
  intros N f x. unfold tab. rewrite look_tab_go.
  replace (0 <=? x) with true by (symmetry; apply Nat.leb_le; lia). cbn [andb Nat.add].
  destruct (x <? N); [reflexivity|]. unfold look. rewrite nget_NE. reflexivity.
Qed.

Inductive kstep := KDone (T : nmap (list nat)) | KMore (T : nmap (list nat)).

(* At most `fuel` rounds of G from T: KDone T when a round adds nothing
   to T below N, KMore with the last table otherwise. *)
Fixpoint krun (G : (nat -> list nat) -> nat -> list nat) (N fuel : nat) (T : nmap (list nat)) : kstep :=
  match fuel with
  | 0 => KMore T
  | S f =>
      let T' := tab N (G (look T)) in
      if forallb (fun x => inclb (look T' x) (look T x)) (seq 0 N) then KDone T
      else krun G N f T'
  end.

(* (N+1)^2 rounds in all, which bounds ReqsTotal's measure, without
   writing the square as a unary number. *)
Fixpoint krun2 (G : (nat -> list nat) -> nat -> list nat) (N outer : nat) (T : nmap (list nat))
  : option (nmap (list nat)) :=
  match outer with
  | 0 => None
  | S o => match krun G N (S N) T with KDone T' => Some T' | KMore T' => krun2 G N o T' end
  end.

Definition kfix (G : (nat -> list nat) -> nat -> list nat) (N : nat) : option (nmap (list nat)) :=
  krun2 G N (S N) NE.

Section KleeneExec.

Variable N : nat.
Variable F : (nat -> list nat) -> nat -> list nat.
Hypothesis F_mono : forall R R', (forall x, incl (R x) (R' x)) -> forall x, incl (F R x) (F R' x).
Hypothesis F_nodup : forall R x, NoDup (F R x).
Hypothesis F_bound : forall R x y, In y (F R x) -> y < N.
Hypothesis F_out : forall R x, N <= x -> F R x = [].
(* the executable operator: the same members as F, from the same members *)
Variable G : (nat -> list nat) -> nat -> list nat.
Hypothesis G_F : forall R R', (forall x, same (R x) (R' x)) -> forall x, same (G R x) (F R' x).
Hypothesis G_nodup : forall R x, NoDup (G R x).

Definition KInv (k : nat) (T : nmap (list nat)) : Prop :=
  (forall x, same (look T x) (it F k x)) /\ (forall x, NoDup (look T x)).

Lemma kinv_init : KInv 0 NE.
Proof.
  split; intro x; unfold look; rewrite nget_NE; cbn; [split; intros y []|constructor].
Qed.

Lemma kinv_step : forall k T, KInv k T -> KInv (S k) (tab N (G (look T))).
Proof.
  intros k T [Same Nd]. split; intro x; rewrite look_tab; destruct (Nat.ltb_spec x N) as [Hx|Hx].
  - exact (G_F (look T) (it F k) Same x).
  - change (it F (S k) x) with (F (it F k) x). rewrite (F_out _ x Hx). split; intros y [].
  - apply G_nodup.
  - constructor.
Qed.

Definition kstable (k : nat) : Prop := forall x, incl (it F (S k) x) (it F k x).

Lemma kstable_check : forall k T, KInv k T ->
  forallb (fun x => inclb (look (tab N (G (look T))) x) (look T x)) (seq 0 N) = true -> kstable k.
Proof.
  intros k T I Check x. destruct (Nat.ltb_spec x N) as [Hx|Hx].
  - rewrite forallb_forall in Check. specialize (Check x ltac:(apply in_seq; lia)).
    apply inclb_spec in Check. destruct (kinv_step k T I) as [Same' _]. destruct I as [Same _].
    intros y Hy. apply (proj1 (Same x)), Check, (proj2 (Same' x)), Hy.
  - change (it F (S k) x) with (F (it F k) x). rewrite (F_out _ x Hx). intros y [].
Qed.

Lemma krun_spec : forall fuel k T, KInv k T ->
  match krun G N fuel T with
  | KDone T' => exists j, KInv j T' /\ kstable j
  | KMore T' => exists j, KInv j T'
  end.
Proof.
  intro fuel. induction fuel as [|f IH]; intros k T I; [exists k; exact I|]. cbn [krun].
  destruct (forallb _ (seq 0 N)) eqn:Check.
  - exists k. split; [exact I | exact (kstable_check k T I Check)].
  - exact (IH (S k) _ (kinv_step k T I)).
Qed.

Lemma krun2_spec : forall outer k T T', KInv k T -> krun2 G N outer T = Some T' ->
  exists j, KInv j T' /\ kstable j.
Proof.
  intro outer. induction outer as [|o IH]; intros k T T' I Run; [discriminate|]. cbn [krun2] in Run.
  pose proof (krun_spec (S N) k T I) as Spec.
  destruct (krun G N (S N) T) as [Td|Tm].
  - injection Run as <-. exact Spec.
  - destruct Spec as [j Ij]. exact (IH j Tm T' Ij Run).
Qed.

Lemma it_chain_le : forall k m, k <= m -> forall x, incl (it F k x) (it F m x).
Proof.
  intros k m Hle. induction Hle as [|m _ IH]; intros x; [apply incl_refl|].
  intros y Hy. apply (it_chain F F_mono m x), IH, Hy.
Qed.

Lemma stable_above : forall k, kstable k -> forall m, k <= m -> forall x, incl (it F m x) (it F k x).
Proof.
  intros k Hs m Hle. induction Hle as [|m _ IH]; intros x; [apply incl_refl|].
  intros y Hy. change (In y (F (it F m) x)) in Hy.
  apply Hs. change (In y (F (it F k) x)). exact (F_mono _ _ IH x y Hy).
Qed.

Lemma stable_lfp : forall k, kstable k -> forall x, same (it F k x) (lfp N F x).
Proof.
  intros k Hs x. assert (HK : kstable (K N)) by exact (lfp_closed N F F_mono F_nodup F_bound F_out).
  unfold lfp. destruct (Nat.le_ge_cases k (K N)) as [Hle|Hge].
  - split; [exact (it_chain_le k (K N) Hle x) | exact (stable_above k Hs (K N) Hle x)].
  - split; [exact (stable_above (K N) HK k Hge x) | exact (it_chain_le (K N) k Hge x)].
Qed.

(* the tabulated fixed point has the members of ReqsTotal's lfp *)
Theorem kfix_same : forall T, kfix G N = Some T ->
  forall x, same (look T x) (lfp N F x) /\ NoDup (look T x).
Proof.
  intros T Run x. destruct (krun2_spec (S N) 0 NE T kinv_init Run) as (j & [Same Nd] & Hs).
  split; [|exact (Nd x)]. destruct (Same x) as [A1 A2]. destruct (stable_lfp j Hs x) as [B1 B2].
  split; intros y Hy; [apply B1, A1, Hy | apply A2, B2, Hy].
Qed.

End KleeneExec.

(* ======================================================================== *)
(* 2. The requirement lists, computed                                       *)
(* ======================================================================== *)

Section Reqs.

Variable b : bundle.
Variable ls : load_st.
Variable e : nat.

Local Notation w := (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls)
  (Loader.library_dir (b_prelude b))).
Local Notation W := (W0r w).
Local Notation N := (ls_next_sym ls).

(* ReqsTotal's Passume, Pfix, mixR and Fnom, with the context table and
   the fixed lists as parameters: at CT and Pfix they are ReqsTotal's own
   (by conversion). *)
Definition PassumeX (CTR : nat -> list nat) (s : nat) : list nat :=
  match M.w_decl W s with
  | Some d => clean ls (walkset b ls e CTR (M.d_mod d) (LoadWorld.assume_specs (M.d_assumes d)))
  | None => []
  end.

Definition PfixX (CTR : nat -> list nat) (h : nat) : list nat :=
  if plain_kb (M.kind_sym W h) then PassumeX CTR h else [].

Definition mixP (P R : nat -> list nat) (h : nat) : list nat :=
  if nominal_kb (M.kind_sym W h) then R h else P h.

Definition FnomP (P R : nat -> list nat) (g : nat) : list nat :=
  match M.w_decl W g with
  | Some d => match M.nominal_body (M.d_ast d) with
              | Some (ps, t) => clean ls (listedW b ls e (M.d_mod d) ps ++ freeT b ls e (mixP P R) (M.d_mod d) t)
              | None => []
              end
  | None => []
  end.

(* The tables: contexts' type symbols, the fixed lists, the nominals'. *)
Definition reqs_tables : option (nmap (list nat) * nmap (list nat)) :=
  match kfix (Fctx b ls e) N with
  | None => None
  | Some CTt =>
      let Ptab := tab N (PfixX (look CTt)) in
      match kfix (FnomP (look Ptab)) N with
      | None => None
      | Some Ltab => Some (Ptab, Ltab)
      end
  end.

Definition reqs_of (Ptab Ltab : nmap (list nat)) (x : nat) : list nat :=
  if nominal_kb (M.kind_sym W x) then look Ltab x else look Ptab x.

Hypothesis HI : LInv b ls.

Lemma same_refl : forall l, same l l.
Proof. intro l. split; apply incl_refl. Qed.

Lemma same_sym : forall l l', same l l' -> same l' l.
Proof. intros l l' [A1 A2]. split; assumption. Qed.

Lemma same_clean : forall l l', same l l' -> same (clean ls l) (clean ls l').
Proof.
  intros l l' [A1 A2]. split; intros y Hy; apply in_clean in Hy as [Hy Hn]; apply in_clean;
    split; auto.
Qed.

Lemma CT_exec : forall CTt, kfix (Fctx b ls e) N = Some CTt ->
  forall x, same (look CTt x) (CT b ls e x).
Proof.
  intros CTt Run x.
  refine (proj1 (kfix_same N (Fctx b ls e) (Fctx_mono b ls e) (Fctx_nodup b ls e)
    (Fctx_bound b ls e) (Fctx_out b ls e HI) (Fctx b ls e) _ (Fctx_nodup b ls e) CTt Run x)).
  intros R R' Same y. split; apply Fctx_mono; intro z; apply Same.
Qed.

Lemma PfixX_same : forall CTR, (forall x, same (CTR x) (CT b ls e x)) ->
  forall h, same (PfixX CTR h) (Pfix b ls e h).
Proof.
  intros CTR Same h. unfold PfixX, Pfix, PassumeX, Passume.
  destruct (plain_kb (M.kind_sym _ h)); [|apply same_refl].
  destruct (M.w_decl _ h) as [d|]; [|apply same_refl]. apply same_clean.
  split; apply walkset_mono; intro z; apply Same.
Qed.

Lemma PfixX_nodup : forall CTR h, NoDup (PfixX CTR h).
Proof.
  intros CTR h. unfold PfixX, PassumeX. destruct (plain_kb (M.kind_sym W h)); [|constructor].
  destruct (M.w_decl W h); [apply clean_nodup|constructor].
Qed.

Lemma kind_out : forall h, N <= h -> M.kind_sym W h = None.
Proof. intros h Hh. unfold M.kind_sym. rewrite (decl_out b ls e HI h Hh). reflexivity. Qed.

Lemma Pt_same : forall CTR, (forall x, same (CTR x) (CT b ls e x)) ->
  forall h, same (look (tab N (PfixX CTR)) h) (Pfix b ls e h).
Proof.
  intros CTR Same h. rewrite look_tab. destruct (Nat.ltb_spec h N) as [Hh|Hh].
  - exact (PfixX_same CTR Same h).
  - pose proof (kind_out h Hh) as K.  unfold Pfix. rewrite K. apply same_refl.
Qed.

Lemma FnomP_same : forall P P' R R', (forall x, same (P x) (P' x)) -> (forall x, same (R x) (R' x)) ->
  forall g, same (FnomP P R g) (FnomP P' R' g).
Proof.
  assert (Half : forall P P' R R', (forall x, same (P x) (P' x)) -> (forall x, same (R x) (R' x)) ->
    forall g, incl (FnomP P R g) (FnomP P' R' g)).
  { intros P P' R R' SP SR g y Hy. unfold FnomP in *. destruct (M.w_decl W g) as [d|]; [|exact Hy].
    destruct (M.nominal_body (M.d_ast d)) as [[ps t]|]; [|exact Hy].
    apply in_clean in Hy as [Hy Hn]. apply in_clean. split; [|exact Hn].
    apply in_app_or in Hy as [Hy|Hy]; apply in_or_app; [left; exact Hy|right].
    apply freeT_spec. apply freeT_spec in Hy. revert Hy. apply FreeIn_mono.
    intros h _ _. unfold mixP. destruct (nominal_kb (M.kind_sym W h)); [apply SR | apply SP]. }
  intros P P' R R' SP SR g. split; [apply Half; assumption|].
  apply Half; intro x; apply same_sym; [apply SP | apply SR].
Qed.

Lemma FnomP_nodup : forall P R g, NoDup (FnomP P R g).
Proof.
  intros P R g. unfold FnomP. destruct (M.w_decl W g) as [d|]; [|constructor].
  destruct (M.nominal_body (M.d_ast d)) as [[ps t]|]; [apply clean_nodup|constructor].
Qed.

(* the tables have the members of ReqsTotal's lists, without repeats *)
Theorem reqs_tables_same : forall Ptab Ltab, reqs_tables = Some (Ptab, Ltab) ->
  forall x, same (reqs_of Ptab Ltab x) (Rfin b ls e (@NMap.NE (list nat)) x) /\ NoDup (reqs_of Ptab Ltab x).
Proof.
  intros Ptab Ltab Run x. unfold reqs_tables in Run.
  destruct (kfix (Fctx b ls e) N) as [CTt|] eqn:RunC; [|discriminate].
  destruct (kfix (FnomP (look (tab N (PfixX (look CTt))))) N) as [Ltab'|] eqn:RunL; [|discriminate].
  injection Run as <- <-.
  pose proof (CT_exec CTt RunC) as SC.
  pose proof (Pt_same _ SC) as SP.
  unfold reqs_of, Rfin. rewrite nget_NE.
  destruct (nominal_kb (M.kind_sym _ x)).
  - assert (G_F : forall R R', (forall x, same (R x) (R' x)) ->
      forall x, same (FnomP (look (tab N (PfixX (look CTt)))) R x) (Fnom b ls e R' x)).
    { intros R R' SR y. exact (FnomP_same _ (Pfix b ls e) R R' SP SR y). }
    exact (kfix_same N (Fnom b ls e) (Fnom_mono b ls e) (Fnom_nodup b ls e) (Fnom_bound b ls e)
      (Fnom_out b ls e HI) _ G_F (FnomP_nodup _) Ltab' RunL x).
  - split; [exact (SP x)|]. rewrite look_tab. destruct (x <? N); [apply PfixX_nodup|constructor].
Qed.

End Reqs.

(* ======================================================================== *)
(* 3. A reading's lists may be reordered                                    *)
(* ======================================================================== *)

Import ComponentContracts.SourceReading.

Lemma closed_same : forall Wm R R', M.RelevanceClosed Wm R ->
  (forall x, same (R x) (R' x)) -> M.RelevanceClosed Wm R'.
Proof.
  intros Wm R R' C S g d ps t Hd Hb. destruct (C g d ps t Hd Hb) as [C1 C2]. split.
  - intros n x Hn Hr. apply (proj1 (S g)), (C1 n x Hn Hr).
  - intros x Hx. apply (proj1 (S g)), C2. revert Hx. apply FreeIn_mono. intros h _ _. apply (proj2 (S h)).
Qed.

Lemma nominal_kind : forall Wm g d ps t, M.w_decl Wm g = Some d -> M.nominal_body (M.d_ast d) = Some (ps, t) ->
  nominal_kb (M.kind_sym Wm g) = true.
Proof.
  intros Wm g d ps t Hd Hb. unfold M.kind_sym. rewrite Hd.
  destruct (M.d_ast d); cbn in Hb |- *; try discriminate; reflexivity.
Qed.

Lemma not_nominal : forall k, nominal_kb k = false -> k <> Some M.DKTag /\ k <> Some M.DKAlias.
Proof. intros [[]|] H; cbn in H; split; congruence. Qed.

(* WorldOf fixes the members of each list, and its order only per symbol:
   a reading stays one when its lists are reordered. *)
Theorem reading_same : forall w R R' F, ReqsReading (W_read w R F) ->
  (forall x, same (R x) (R' x)) -> (forall x, NoDup (R' x)) -> ReqsReading (W_read w R' F).
Proof.
  intros w R R' F Rd S Nd. constructor.
  - exact Nd.
  - intros s k Hk H1 H2 H3 H4 t.
    pose proof (rr_plain _ Rd s k Hk H1 H2 H3 H4 t) as P.
    change (In t (R' s) <-> M.AssumeTypes (W_read w R' F) s t).
    change (In t (R s) <-> M.AssumeTypes (W_read w R F) s t) in P.
    rewrite W0_assume in P |- *.
    split; intro H; [apply P, (proj2 (S s)), H | apply (proj1 (S s)), P, H].
  - intros s Hk. pose proof (rr_none _ Rd s Hk) as E. change (R' s = []). change (R s = []) in E.
    destruct (R' s) as [|y r] eqn:E'; [reflexivity|]. exfalso.
    assert (Hy : In y (R s)) by (apply (proj2 (S s)); rewrite E'; left; reflexivity).
    rewrite E in Hy. destruct Hy.
  - change (M.RelevanceClosed (W_read w R' F) R'). apply (closed_same _ R); [|exact S].
    apply (closed_RF w R F). exact (rr_closed _ Rd).
  - intros Rc Hc Ha g Hg.
    set (Rc2 := fun s => if nominal_kb (M.kind_sym (W_read w R F) s) then Rc s else R s).
    assert (C2 : M.RelevanceClosed (W_read w R F) Rc2).
    { intros g' d ps t Hd Hb. pose proof (closed_RF w R' F R F Rc Hc) as Hc'.
      destruct (Hc' g' d ps t Hd Hb) as [C1 C3].
      assert (E : Rc2 g' = Rc g') by (unfold Rc2; rewrite (nominal_kind _ g' d ps t Hd Hb); reflexivity).
      rewrite E. split; [exact C1|]. intros x Hx. apply C3. revert Hx. apply FreeIn_mono.
      intros h _ _. unfold Rc2. destruct (nominal_kb (M.kind_sym (W_read w R F) h)) eqn:Ek; [apply incl_refl|].
      destruct (not_nominal _ Ek) as [NT NA]. rewrite (Ha h NT NA). apply (proj1 (S h)). }
    assert (Agree : forall s, M.kind_sym (W_read w R F) s <> Some M.DKTag ->
      M.kind_sym (W_read w R F) s <> Some M.DKAlias -> Rc2 s = M.w_reqs (W_read w R F) s).
    { intros s NT NA. unfold Rc2.
      destruct (M.kind_sym (W_read w R F) s) as [[]|]; cbn; try reflexivity; congruence. }
    pose proof (rr_least _ Rd Rc2 C2 Agree g Hg) as L.
    assert (E : Rc2 g = Rc g).
    { unfold Rc2. destruct Hg as [Eg|Eg];
        [change (M.kind_sym (W_read w R F) g = Some M.DKTag) in Eg
        |change (M.kind_sym (W_read w R F) g = Some M.DKAlias) in Eg]; rewrite Eg; reflexivity. }
    rewrite E in L. intros y Hy. apply L, (proj2 (S g)), Hy.
  - intros g d ps t n Hd Hb Hn. destruct (rr_listed _ Rd g d ps t n Hd Hb Hn) as (x & Hr & Ha).
    exists x. split.
    + rewrite (rs_RF w R' F R F). exact Hr.
    + apply W0_assume. apply W0_assume in Ha. exact Ha.
Qed.

(* ======================================================================== *)
(* 4. The world                                                             *)
(* ======================================================================== *)

(* A loader state with the loader world's tables (the fields the
   requirement functions read). *)
Definition ls_of (w : LoadWorld.world) : load_st :=
  LSt [] (w_mods w) (w_files w) (w_order w) (w_syms w) (w_nsyms w) 0.

Definition world_requirements (b : bundle) (w : LoadWorld.world) : option (nat -> list nat) :=
  match reqs_tables b (ls_of w) (w_entry w) with
  | Some (Ptab, Ltab) => Some (reqs_of b (ls_of w) (w_entry w) Ptab Ltab)
  | None => None
  end.

(* The static world of a bundle: the loader's tables, the least
   requirement lists, and every tag's listed parameters checked.  Its
   `w_context` is empty (E-M2 fills it, through `with_context`). *)
Definition bundle_world (b : bundle) : option AE.world :=
  match load_world b with
  | Bad _ => None
  | Good w =>
      match LoadListed.check w with
      | Bad _ => None
      | Good _ =>
          match world_requirements b w with
          | Some R => Some (PublicWorld.world_read w R)
          | None => None
          end
      end
  end.

Theorem bundle_world_sound : forall b W, bundle_world b = Some W -> AE.WorldOf b W.
Proof.
  intros b W Run. unfold bundle_world in Run.
  destruct (load_world b) as [w|m] eqn:Hw; [|discriminate].
  destruct (LoadListed.check w) as [[]|m] eqn:Hc; [|discriminate].
  assert (Hlegacy : check_listed (s_init w) = Good (tt, s_init w)).
  { rewrite check_initial_equal, Hc. reflexivity. }
  destruct (world_requirements b w) as [R|] eqn:Hr; [|discriminate].
  injection Run as <-.
  apply PublicWorldRequirements.load_world_world_of_from_reading; [exact Hw|].
  destruct (load_world_inv b w Hw) as (ls & e & HI & Ew). subst w.
  unfold world_requirements in Hr.
  change (reqs_tables b (ls_of
    (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls)
      (Loader.library_dir (b_prelude b))))
    (w_entry (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls)
      (Loader.library_dir (b_prelude b))))) with (reqs_tables b ls e) in Hr.
  change (reqs_of b (ls_of
    (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls)
      (Loader.library_dir (b_prelude b))))
    (w_entry (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls)
      (Loader.library_dir (b_prelude b))))) with (reqs_of b ls e) in Hr.
  destruct (reqs_tables b ls e) as [[Ptab Ltab]|] eqn:Ht; [|discriminate].
  injection Hr as <-.
  change (ReqsReading (W_read (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls)
    (ls_files ls) (Loader.library_dir (b_prelude b))) (reqs_of b ls e Ptab Ltab) (fun _ => 0))).
  apply (reading_same _ (Rfin b ls e NMap.NE)).
  - apply (reqs_reading b ls e HI (ls_syms ls) NMap.NE).
    + intros i g Hi; exact Hi.
    + intros x l H. rewrite nget_NE in H. discriminate.
    + apply RC_empty.
    + apply RL_empty.
    + apply (check_listed_ok b ls e HI (s_init (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls)
        (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)))) tt (s_init (LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls)
        (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)))));
        [split; [reflexivity|intros i g Hi; exact Hi]|exact Hlegacy].
  - intro x. apply same_sym. exact (proj1 (reqs_tables_same b ls e HI Ptab Ltab Ht x)).
  - intro x. exact (proj2 (reqs_tables_same b ls e HI Ptab Ltab Ht x)).
Qed.

(* ======================================================================== *)
(* 5. Any context table                                                     *)
(* ======================================================================== *)

(* WorldOf does not read `w_context`, so a world keeps WorldOf when its
   context table is replaced (E-M2 forms the table after the world). *)
Definition with_context (W : AE.world) (T : nat -> option (AE.cx * list AuditBase.Semantics.item)) : AE.world :=
  AE.World (AE.w_decl W) (AE.w_scope W) (AE.w_exp W) (AE.w_attached W) (AE.w_reqs W) (AE.w_library W) (AE.w_root_item W) T.

Section Context.

Variables (d : nat -> option AE.decl) (sc ex : string -> AE.nspace -> name -> option AE.ent)
  (at_ : nat -> name -> option nat) (rq : nat -> list nat) (lib : option string)
  (root : option (string * name))
  (c c' : nat -> option (AE.cx * list AuditBase.Semantics.item)).

Local Notation W := (AE.World d sc ex at_ rq lib root c).
Local Notation W' := (AE.World d sc ex at_ rq lib root c').

Fixpoint exports_ctx b p ns n e (H : AE.Exports b W p ns n e) {struct H} : AE.Exports b W' p ns n e.
Proof.
  destruct H as [p s dd ns n Hd Hm Hf | p m t imp u e ns Hm Hin Hg Hu Hns Hor | p m t imp a Hm Hin Ha].
  - eapply AE.ExOwn; [exact Hd | exact Hm | exact Hf].
  - eapply AE.ExUse; [exact Hm | exact Hin | exact Hg | exact Hu | exact Hns |].
    destruct Hor as [H1|(Hd & Hn & H2)];
      [left; exact (exports_ctx b t ns (Audit.Input.u_name u) e H1)
      |right; exact (conj Hd (conj Hn (exports_ctx b t AE.NSAlias (Audit.Input.u_name u) e H2)))].
  - eapply AE.ExAlias; [exact Hm | exact Hin | exact Ha].
Defined.

Lemma ordinary_ctx : forall b p ns n e, AE.OrdinaryScope b W p ns n e -> AE.OrdinaryScope b W' p ns n e.
Proof.
  intros b p ns n e H. destruct H as [p ns n e H | p m t imp ns n e Hm Hin Hg H].
  - apply AE.ScExported, exports_ctx, H.
  - eapply AE.ScGlob; [exact Hm | exact Hin | exact Hg | apply exports_ctx, H].
Qed.

Lemma resolve_rest_ctx : forall e segs, AE.resolve_rest W e segs = AE.resolve_rest W' e segs.
Proof.
  intros e segs. revert e. induction segs as [|seg r IH]; intro e; [reflexivity|].
  destruct e as [s|t]; cbn [AE.resolve_rest AE.w_exp]; [reflexivity|].
  destruct (ex t AE.NSName seg); [apply IH|]. destruct (ex t AE.NSAlias seg); [apply IH|reflexivity].
Qed.

Lemma resolve_sym_ctx : forall p pa, AE.resolve_sym W p pa = AE.resolve_sym W' p pa.
Proof.
  intros p [|hd r]; [reflexivity|]. unfold AE.resolve_sym, AE.resolve. cbn [AE.w_scope].
  destruct (sc p AE.NSName hd) as [e|]; [rewrite resolve_rest_ctx; reflexivity|].
  destruct (sc p AE.NSAlias hd) as [e|]; [rewrite resolve_rest_ctx; reflexivity|reflexivity].
Qed.

Lemma spec_types_ctx : forall p sp t, AE.SpecTypes W p sp t -> AE.SpecTypes W' p sp t.
Proof.
  intros p sp t H. induction H as [p pa a t Hr Hk | p pa a cc dd n its sp t Hr Hd Ha Hin _ IH].
  - eapply AE.STType; [rewrite <- resolve_sym_ctx; exact Hr | exact Hk].
  - eapply AE.STCtx; [rewrite <- resolve_sym_ctx; exact Hr | exact Hd | exact Ha | exact Hin | exact IH].
Qed.

Lemma free_in_ctx : forall R p t x, AE.FreeIn W R p t x -> AE.FreeIn W' R p t x.
Proof.
  intros R p t x H.
  induction H as [fs n t x Hin _ IH | ms t x Hin _ IH | pa a t Hr Hk Hb | pa a g x Hr Hk Hx Hb
                 | pa a bpath sp dd sa x Hr Hin _ IH].
  - eapply AE.FRec; [exact Hin | exact IH].
  - eapply AE.FUnion; [exact Hin | exact IH].
  - eapply AE.FType; [rewrite <- resolve_sym_ctx; exact Hr | exact Hk |].
    intros (bb & Hbb & E). apply Hb. exists bb. split; [exact Hbb|]. rewrite resolve_sym_ctx. exact E.
  - eapply AE.FNom; [rewrite <- resolve_sym_ctx; exact Hr | exact Hk | exact Hx |].
    intros (bb & Hbb & E). apply Hb. exists bb. split; [exact Hbb|]. rewrite resolve_sym_ctx. exact E.
  - eapply AE.FBracket; [rewrite <- resolve_sym_ctx; exact Hr | exact Hin | exact IH].
Qed.

End Context.

Theorem world_of_context : forall b W T, AE.WorldOf b W -> AE.WorldOf b (with_context W T).
Proof.
  intros b [d sc ex at_ rq lib root c] T Wo. unfold with_context; cbn [AE.w_decl AE.w_scope AE.w_exp
    AE.w_attached AE.w_reqs AE.w_library].
  assert (Assume : forall s t, AE.AssumeTypes (AE.World d sc ex at_ rq lib root c) s t <-> AE.AssumeTypes (AE.World d sc ex at_ rq lib root T) s t).
  { intros s t. split; intros (dd & pa & Hd & Hin & Hs); exists dd, pa;
      (split; [exact Hd|]); (split; [exact Hin|]); eapply spec_types_ctx; exact Hs. }
  assert (Closed : forall R, AE.RelevanceClosed (AE.World d sc ex at_ rq lib root c) R <-> AE.RelevanceClosed (AE.World d sc ex at_ rq lib root T) R).
  { intro R. split; intros C g dd ps t Hd Hb; destruct (C g dd ps t Hd Hb) as [C1 C2];
      (split; [exact C1|]); intros x Hx; apply C2; revert Hx; apply free_in_ctx. }
  assert (Scope : forall p ns n e, AE.InScope b (AE.World d sc ex at_ rq lib root c) p ns n e <-> AE.InScope b (AE.World d sc ex at_ rq lib root T) p ns n e).
  { intros p ns n e. split; intro H; destruct H as [p ns n e H | p m ns n e Hm Hp He Hno].
    - apply AE.ScPrimary; eapply ordinary_ctx; exact H.
    - eapply AE.ScPrelude; [exact Hm | exact Hp | eapply exports_ctx; exact He |].
      intros e' H'. apply (Hno e'); eapply ordinary_ctx; exact H'.
    - apply AE.ScPrimary; eapply ordinary_ctx; exact H.
    - eapply AE.ScPrelude; [exact Hm | exact Hp | eapply exports_ctx; exact He |].
      intros e' H'. apply (Hno e'); eapply ordinary_ctx; exact H'. }
  constructor.
  - exact (AE.wo_acyclic Wo).
  - exact (AE.wo_decl_sound Wo).
  - exact (AE.wo_decl_complete Wo).
  - intros p ns n e Hl. rewrite (@AE.wo_exports _ _ Wo p ns n e Hl).
    split; apply exports_ctx.
  - intros p ns n e Hl. rewrite (@AE.wo_scope _ _ Wo p ns n e Hl). apply Scope.
  - exact (AE.wo_attached Wo).
  - exact (AE.wo_reqs_nodup Wo).
  - intros s k Hk H1 H2 H3 t. rewrite <- Assume. exact (@AE.wo_reqs_plain _ _ Wo s k Hk H1 H2 H3 t).
  - exact (AE.wo_reqs_none Wo).
  - apply Closed. exact (AE.wo_reqs_closed Wo).
  - intros R C. apply (@AE.wo_reqs_least _ _ Wo R). apply Closed, C.
  - intros g dd ps t n Hd Hb Hn. destruct (@AE.wo_listed _ _ Wo g dd ps t n Hd Hb Hn) as (x & Hr & Ha).
    exists x. split; [exact Hr | apply Assume, Ha].
  - exact (AE.wo_library Wo).
Qed.

(* The world with a context table in place. *)
Definition bundle_world_with (T : nat -> option (AE.cx * list AuditBase.Semantics.item)) (b : bundle) : option AE.world :=
  option_map (fun W => with_context W T) (bundle_world b).

Theorem bundle_world_with_sound : forall T b W, bundle_world_with T b = Some W -> AE.WorldOf b W.
Proof.
  intros T b W Run. unfold bundle_world_with in Run.
  destruct (bundle_world b) as [W0|] eqn:H; [|discriminate]. injection Run as <-.
  apply world_of_context, bundle_world_sound, H.
Qed.
