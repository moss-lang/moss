(* ------------------------------------------------------------------------ *)
(* ElabSound.v — what is proved about the Gallina elaborator (Elab.v).      *)
(*                                                                           *)
(* Proved here:                                                              *)
(*  1. Totality and determinism: `elaborate` is a Gallina function, so it    *)
(*     answers every bundle with exactly one Good program or Bad error.     *)
(*     (Stated for the record; they are true by construction.)              *)
(*  2. D1, no search, on the core: `resolve_prov` is a search-free decision *)
(*     procedure for T-Meth's provision choice: when it answers, the answer *)
(*     is in force, a provision at the receiver's type, and the only one   *)
(*     modulo σ (resolve_prov_sound); when exactly one exists modulo σ, it  *)
(*     answers (resolve_prov_complete); and every well-typed method call's  *)
(*     recorded provision is what it answers (typed_call_resolves): the     *)
(*     elaborated choice is a function of the context and the key.          *)
(*  3. Satisfaction maps (ς) the elaborator chooses: `require_needs`, when  *)
(*     it succeeds, maps exactly the callee's needs, in order, each to a    *)
(*     provision in force here (require_needs_sound); and the map it emits  *)
(*     has the callee's needs as its domain (smap_of_domain).               *)
(*  4. Method lookup is a partial function of (receiver, method) in the     *)
(*     elaborator's environments: one entry per key (get_mth_set,           *)
(*     get_mth_other), so a call has at most one provision to choose.       *)
(*                                                                           *)
(* Stated, not proved (section TODO below): elaboration soundness for the  *)
(* core fragment, `elab_sound_core`.                                        *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import CoreMoss NMap Sx Elab.
Import ListNotations.

(* ===================== 1. Totality and determinism ======================= *)

Corollary elaborate_total : forall b,
  (exists p, elaborate b = Good p) \/ (exists msg, elaborate b = Bad msg).
Proof. intro b. destruct (elaborate b); eauto. Qed.

Corollary elaborate_deterministic : forall b p1 p2,
  elaborate b = Good p1 -> elaborate b = Good p2 -> p1 = p2.
Proof. intros b p1 p2 H1 H2. rewrite H1 in H2. injection H2 as ->. reflexivity. Qed.

(* ===================== 2. D1: provision choice on the core ================ *)

Definition prov_at_b (s : subst) (tau0 : ty) (m : msym) (i : item) : bool :=
  match app_subst_item s i with
  | IMth r m' _ => ty_eqb r (ExecutionModel.app_subst s tau0) && Nat.eqb m m'
  | _ => false
  end.

Lemma prov_at_b_spec : forall s tau0 m i,
  prov_at_b s tau0 m i = true <-> is_prov_at s tau0 m i.
Proof.
  intros s tau0 m i. unfold prov_at_b, is_prov_at. split.
  - destruct (app_subst_item s i) eqn:E; try discriminate.
    intro H. apply andb_true_iff in H as [Ht Hm].
    apply ty_eqb_eq in Ht. apply Nat.eqb_eq in Hm. subst. eauto.
  - intros [th' Hp]. rewrite Hp. apply andb_true_iff. split.
    + apply ty_eqb_eq. reflexivity.
    + apply Nat.eqb_eq. reflexivity.
Qed.

(* The choice: the provisions at the key, which must all be one modulo σ. *)
Definition resolve_prov (Phi : ctx) (tau0 : ty) (m : msym) : option item :=
  match filter (prov_at_b (cS Phi) tau0 m) (cA Phi) with
  | [] => None
  | i :: r =>
      if forallb (fun j => item_eqb (app_subst_item (cS Phi) j) (app_subst_item (cS Phi) i)) r
      then Some i else None
  end.

Theorem resolve_prov_sound : forall Phi tau0 m i,
  resolve_prov Phi tau0 m = Some i ->
  In i (cA Phi) /\ is_prov_at (cS Phi) tau0 m i /\
  forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
    app_subst_item (cS Phi) i1 = app_subst_item (cS Phi) i.
Proof.
  intros Phi tau0 m i H. unfold resolve_prov in H.
  destruct (filter (prov_at_b (cS Phi) tau0 m) (cA Phi)) as [|i0 r] eqn:Hf; [discriminate|].
  destruct (forallb _ r) eqn:Ha; [|discriminate]. injection H as <-.
  assert (Hin0 : In i0 (filter (prov_at_b (cS Phi) tau0 m) (cA Phi))) by (rewrite Hf; left; reflexivity).
  apply filter_In in Hin0 as [Hin0 Hp0].
  split; [exact Hin0|]. split; [apply prov_at_b_spec; exact Hp0|].
  intros i1 Hin1 Hp1.
  assert (Hin1' : In i1 (filter (prov_at_b (cS Phi) tau0 m) (cA Phi)))
    by (apply filter_In; split; [exact Hin1 | apply prov_at_b_spec; exact Hp1]).
  rewrite Hf in Hin1'. destruct Hin1' as [<- | Hr]; [reflexivity|].
  rewrite forallb_forall in Ha. apply item_eqb_eq. exact (Ha i1 Hr).
Qed.

Theorem resolve_prov_complete : forall Phi tau0 m i,
  In i (cA Phi) -> is_prov_at (cS Phi) tau0 m i ->
  (forall i1, In i1 (cA Phi) -> is_prov_at (cS Phi) tau0 m i1 ->
     app_subst_item (cS Phi) i1 = app_subst_item (cS Phi) i) ->
  exists i', resolve_prov Phi tau0 m = Some i'
             /\ app_subst_item (cS Phi) i' = app_subst_item (cS Phi) i.
Proof.
  intros Phi tau0 m i Hin Hp Huniq. unfold resolve_prov.
  assert (Hall : forall j, In j (filter (prov_at_b (cS Phi) tau0 m) (cA Phi)) ->
                 app_subst_item (cS Phi) j = app_subst_item (cS Phi) i).
  { intros j Hj. apply filter_In in Hj as [Hj Hpj]. apply Huniq; [exact Hj|].
    apply prov_at_b_spec. exact Hpj. }
  destruct (filter (prov_at_b (cS Phi) tau0 m) (cA Phi)) as [|i0 r] eqn:Hf.
  - exfalso. assert (H : In i (filter (prov_at_b (cS Phi) tau0 m) (cA Phi)))
      by (apply filter_In; split; [exact Hin | apply prov_at_b_spec; exact Hp]).
    rewrite Hf in H. exact H.
  - assert (H0 : app_subst_item (cS Phi) i0 = app_subst_item (cS Phi) i) by (apply Hall; left; reflexivity).
    replace (forallb _ r) with true.
    + exists i0. split; [reflexivity | exact H0].
    + symmetry. apply forallb_forall. intros j Hj. apply item_eqb_eq.
      rewrite (Hall j (or_intror Hj)). symmetry. exact H0.
Qed.

(* D1 for the core judgment: the provision a well-typed method call records
   is what the search-free procedure computes from the context and the key
   (modulo σ) — the elaborator has nothing to search for. *)
Theorem typed_call_resolves : forall Sg Phi G e0 m i args t,
  has_ty Sg Phi G (EMeth e0 m i args) t ->
  exists rt0 th0 i', i = IMth rt0 m th0
    /\ resolve_prov Phi rt0 m = Some i'
    /\ app_subst_item (cS Phi) i' = app_subst_item (cS Phi) i.
Proof.
  intros Sg Phi G e0 m i args t H.
  destruct (meth_typed_unique H) as [rt0 [th0 [-> [Hin Huniq]]]].
  assert (Hp : is_prov_at (cS Phi) rt0 m (IMth rt0 m th0)) by (unfold is_prov_at; simpl; eexists; reflexivity).
  destruct (resolve_prov_complete Phi rt0 m (IMth rt0 m th0) Hin Hp Huniq) as [i' [Hr Heq]].
  exists rt0, th0, i'. auto.
Qed.

(* ===================== 3. Monad facts ===================================== *)

Lemma bind_inv : forall A B (m : M A) (f : A -> M B) s b s'',
  mbind m f s = Good (b, s'') -> exists a s', m s = Good (a, s') /\ f a s' = Good (b, s'').
Proof.
  intros A B m f s b s'' H. unfold mbind in H.
  destruct (m s) as [[a s']|e]; [eauto | discriminate].
Qed.

Lemma fail_not_good : forall A msg s (a : A) s', fail msg s = Good (a, s') -> False.
Proof. intros. discriminate. Qed.

Lemma err_not_good : forall A mi msg s (a : A) s', err mi msg s = Good (a, s') -> False.
Proof.
  intros A mi msg s a s' H. unfold err in H.
  apply bind_inv in H as [p [s1 [_ H]]]. discriminate.
Qed.

Lemma mmap_forall2 : forall A B (f : A -> M B) (P : A -> B -> Prop),
  (forall x s y s', f x s = Good (y, s') -> P x y) ->
  forall l s ys s', mmap f l s = Good (ys, s') -> Forall2 P l ys.
Proof.
  intros A B f P Hf l. induction l as [|x r IH]; intros s ys s' H; simpl in H.
  - injection H as <- _. constructor.
  - apply bind_inv in H as [y [s1 [Hx H]]].
    apply bind_inv in H as [ys' [s2 [Hr H]]].
    injection H as <- _. constructor; [exact (Hf _ _ _ _ Hx) | exact (IH _ _ _ Hr)].
Qed.

(* ===================== 4. Satisfaction maps ============================== *)

(* require_needs (FnChecker.require_needs): when it succeeds, its pairs
   are the callee's needs, in order, each sent to a provision in force. *)
Theorem require_needs_sound : forall mi e g info s ps s',
  require_needs mi e g info s = Good (ps, s') ->
  map fst ps = fi_needs info /\ Forall (fun p => key_present e (snd p) = true) ps.
Proof.
  intros mi e g info s ps s' H. unfold require_needs in H.
  apply bind_inv in H as [u [s1 [_ H]]].
  apply bind_inv in H as [pairs [s2 [Hm H]]].
  set (P := fun (need : key) (p : key * key * bool) =>
              let '(a, b, _) := p in a = need /\ key_present e b = true).
  assert (HF : Forall2 P (fi_needs info) pairs).
  { refine (mmap_forall2 _ _ _ P _ _ _ _ _ Hm).
    intros need s0 y s0' Hy. destruct need as [v|f|r m|u0]; cbv beta iota in Hy.
    - destruct (key_present e (KyVal v)) eqn:Hk.
      + injection Hy as <- _. split; [reflexivity | exact Hk].
      + apply bind_inv in Hy as [x [sx [_ Hy]]]. exfalso. exact (err_not_good _ _ _ _ _ _ Hy).
    - destruct (key_present e (KyFn f)) eqn:Hk.
      + injection Hy as <- _. split; [reflexivity | exact Hk].
      + apply bind_inv in Hy as [x [sx [_ Hy]]]. exfalso. exact (err_not_good _ _ _ _ _ _ Hy).
    - apply bind_inv in Hy as [mg [sa [_ Hy]]].
      apply bind_inv in Hy as [k [sb [_ Hy]]].
      destruct (key_present e k) eqn:Hk.
      + injection Hy as <- _. split; [reflexivity | exact Hk].
      + apply bind_inv in Hy as [x [sx [_ Hy]]]. exfalso. exact (err_not_good _ _ _ _ _ _ Hy).
    - destruct (key_present e (KyFun u0)) eqn:Hk.
      + injection Hy as <- _. split; [reflexivity | exact Hk].
      + apply bind_inv in Hy as [x [sx [_ Hy]]]. exfalso. exact (err_not_good _ _ _ _ _ _ Hy). }
  destruct (filter _ pairs) as [|[[need k] chk] rest] eqn:Hb.
  - injection H as <- _. clear Hb Hm.
    induction HF as [|need p needs ps' Hp HF IH]; simpl; [split; constructor|].
    destruct p as [[a b] c]. destruct Hp as [-> Hk]. destruct IH as [IH1 IH2].
    split; [simpl; f_equal; exact IH1 | constructor; [exact Hk | exact IH2]].
  - apply bind_inv in H as [x [sx [_ H]]]. exfalso. exact (err_not_good _ _ _ _ _ _ H).
Qed.

(* ===================== 5. Lookup is a partial function ==================== *)

Lemma assoc_aset_same : forall K V (eqb : K -> K -> bool) (k : K) (v : V) l,
  (forall a, eqb a a = true) ->
  assoc eqb k (aset eqb k v l) = Some v.
Proof.
  intros K V eqb k v l Heq. induction l as [|[k' v'] r IH]; simpl.
  - rewrite Heq. reflexivity.
  - destruct (eqb k k') eqn:E; simpl.
    + rewrite Heq. reflexivity.
    + rewrite E. exact IH.
Qed.

Lemma assoc_aset_other : forall K V (eqb : K -> K -> bool) (k k2 : K) (v : V) l,
  (forall a b, eqb a b = true <-> a = b) -> k2 <> k ->
  assoc eqb k2 (aset eqb k v l) = assoc eqb k2 l.
Proof.
  intros K V eqb k k2 v l Heq Hne. induction l as [|[k' v'] r IH]; simpl.
  - destruct (eqb k2 k) eqn:E; [apply Heq in E; contradiction | reflexivity].
  - destruct (eqb k k') eqn:E; simpl.
    + apply Heq in E. subst k'.
      destruct (eqb k2 k) eqn:E2; [apply Heq in E2; contradiction | reflexivity].
    + destruct (eqb k2 k'); [reflexivity | exact IH].
Qed.

Lemma ety_eqb_refl : forall a, ety_eqb a a = true.
Proof.
  fix IH 1. intros [| |g|n xs|ms|fs]; simpl.
  - reflexivity.
  - reflexivity.
  - apply Nat.eqb_refl.
  - rewrite Nat.eqb_refl. simpl. revert xs. fix IHl 1. intros [|[k t] r]; [reflexivity|].
    rewrite Nat.eqb_refl, (IH t), (IHl r). reflexivity.
  - revert ms. fix IHl 1. intros [|t r]; [reflexivity|].
    rewrite (IH t), (IHl r). reflexivity.
  - revert fs. fix IHl 1. intros [|[k t] r]; [reflexivity|].
    rewrite String.eqb_refl, (IH t), (IHl r). reflexivity.
Qed.

Lemma nget_nset_go_same : forall A fuel (m : nmap A) k x,
  S k < 2 ^ fuel -> nget (nset_go fuel m k x) k = Some x.
Proof.
  intros A fuel. induction fuel as [|f IH]; intros m k x Hk; [simpl in Hk; lia|].
  destruct k as [|k']; [destruct m; reflexivity|].
  assert (H2 : 2 * Nat.div2 k' <= k') by (rewrite Nat.div2_div; apply Nat.mul_div_le; lia).
  assert (Hb : S (Nat.div2 k') < 2 ^ f) by (rewrite Nat.pow_succ_r' in Hk; lia).
  simpl. destruct m as [|l v r]; destruct (is_even_nat k') eqn:E; simpl; rewrite ?E;
    apply IH; exact Hb.
Qed.

(* A method provision, once set, is what lookup finds at its key: the
   environment holds one entry per (receiver, method), so a method call has
   at most one provision to choose and the choice is the latest bind's. *)
Theorem get_mth_set : forall e t m s,
  S m < 2 ^ 64 -> get_mth (set_mth e t m s) t m = Some s.
Proof.
  intros e t m s _. unfold get_mth, set_mth. cbn [en_mths]. unfold nset.
  rewrite (nget_nset_go_same _ (S m) _ m _ (Nat.pow_gt_lin_r 2 (S m) (le_n 2))).
  apply assoc_aset_same. exact ety_eqb_refl.
Qed.

(* ===================== 6. TODO: elaboration soundness for the core ======== *)

(* What remains is stated here precisely and NOT proved.  Every Prop below
   is a Definition, not an axiom or an Admitted lemma; nothing above
   depends on it.

   The core fragment: a program every body of which is in the image of
   MossX's embedding of Core Moss (so, by xinterp_conservative, the oracle
   runs it exactly as CoreMoss.interp does). *)
Definition in_core (b : xexpr) : Prop := exists e, emb_expr e = b.

Definition core_program (p : xprogram) : Prop :=
  forall f b, In (f, b) (xo_fns p) -> in_core b.

(* Elaboration soundness for the core fragment: a well-formed signature Σ
   exists whose bodies are exactly what was elaborated, with main defined,
   closed and unit-valued (CoreMoss.wf_prog). *)
Definition elab_sound_core : Prop :=
  forall b p, elaborate b = Good p -> core_program p ->
    exists Sg, wf_prog Sg (xo_main p)
      /\ forall f body, In (f, body) (xo_fns p) ->
           exists D S e, g_fn Sg f = Some (D, S, Some e) /\ emb_expr e = body.

(* The lemmas it needs, as statements (each is a Prop the proof of
   elab_sound_core would establish):

   TODO_sigma_of: the elaborator can return Σ alongside the program — each
     symbol's telescope is its fn_info's base environment (type items from
     en_tn, the rest from fi_needs, merged as merge_tele does), and each
     signature fn_sig's, through ety_ty.  Not yet produced: Elab.v keeps
     only what the dynamics needs.

   TODO_needs_smap_ok: for a direct call, require_needs's pairs, through
     key_item, are smap_ok at the identity application of the callee's
     telescope (domain: require_needs_sound above; targets: in force, and
     item_matches modulo σ, which needs keys built by method_key to agree
     with the core's app_subst_item).

   TODO_method_items_theta: method items need their application θ (the
     elaborator keys IMth with θ = [], as milestone 1 did); sat at a method
     whose telescope has type items needs θ = provision_app's instantiation.

   TODO_synth_typed: synth/check_stmts/synth_block are sound for has_ty
     on core-fragment output, with Γ the en_locals of the env, by fuel
     induction over the mutual fixpoint, rule by rule (T-Sub at every
     `fits` into a union).

   TODO_match_core: the elaborator emits MossX's XMatchP for every match;
     a match whose arms are each a tag head with a binder should be emitted
     as XMatch (the core's EMatch) for the fragment to be non-trivial.     *)
