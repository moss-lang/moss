(* Facts about the legacy elaborator used by the loader and metadata proofs. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab.
Import ListNotations.

Corollary elaborate_total : forall b,
  (exists p, elaborate b = Good p) \/ (exists msg, elaborate b = Bad msg).
Proof. intro b. destruct (elaborate b); eauto. Qed.

Corollary elaborate_deterministic : forall b p1 p2,
  elaborate b = Good p1 -> elaborate b = Good p2 -> p1 = p2.
Proof. intros b p1 p2 H1 H2. rewrite H1 in H2. injection H2 as ->. reflexivity. Qed.

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
