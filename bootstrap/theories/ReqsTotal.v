(* ------------------------------------------------------------------------ *)
(* ReqsTotal.v — requirement lists for every symbol in a loader world.      *)
(*                                                                           *)
(* The elaborator caches requirement lists only for the symbols it asks     *)
(* about.  For the others the reading needs lists too: AssumeTypes for a     *)
(* val, fn, method, context or functor, and the least relevance-closed sets *)
(* for tags and aliases.  Both are least fixed points over the loader's      *)
(* finitely many symbols, computed here by a Kleene iteration that provably *)
(* stabilizes (section Kleene), so nothing depends on how deep or whether    *)
(* cyclic contexts nest.  The reading's lists are the elaborator's cached    *)
(* list where there is one, and these elsewhere (Rfin); ReqsReading holds of *)
(* it (reqs_reading). ElabWorld uses these sets to prove the current        *)
(* executable world builder's WorldOf reading.                              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ComponentContracts.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrameCore.
Import ListNotations.
Local Open Scope list_scope.

Module A := ExecutionModel.Elaboration.
Import ComponentContracts.SourceReading.

Local Strategy opaque [Nat.of_num_uint].

(* ======================================================================== *)
(* 1. A monotone operator on finitely many finite sets has a least fixed    *)
(*    point, reached by iteration                                            *)
(* ======================================================================== *)

Definition inclb (a b : list nat) : bool := forallb (fun y => existsb (Nat.eqb y) b) a.

Lemma inclb_spec : forall a b, inclb a b = true <-> incl a b.
Proof.
  intros a b. unfold inclb. rewrite forallb_forall. split.
  - intros H y Hy. specialize (H y Hy). apply existsb_exists in H as (z & Hz & E). apply Nat.eqb_eq in E. subst. exact Hz.
  - intros H y Hy. apply existsb_exists. exists y. split; [exact (H y Hy) | apply Nat.eqb_refl].
Qed.

Lemma forallb_false_ex : forall A (f : A -> bool) l, forallb f l = false -> exists x, In x l /\ f x = false.
Proof.
  intros A f l. induction l as [|x r IH]; cbn; [discriminate|]. intro H.
  destruct (f x) eqn:E; [destruct (IH H) as (y & Hy & Fy); exists y; split; [right; exact Hy | exact Fy] | exists x; split; [left; reflexivity | exact E]].
Qed.

Fixpoint sum_map (f : nat -> nat) (l : list nat) : nat :=
  match l with [] => 0 | x :: r => f x + sum_map f r end.

Lemma sum_map_le : forall f g l, (forall x, In x l -> f x <= g x) -> sum_map f l <= sum_map g l.
Proof. intros f g l H. induction l as [|x r IH]; cbn; [lia|]. pose proof (H x (or_introl eq_refl)). pose proof (IH (fun y Hy => H y (or_intror Hy))). lia. Qed.

Lemma sum_map_lt : forall f g l, (forall x, In x l -> f x <= g x) -> (exists x, In x l /\ f x < g x) ->
  sum_map f l < sum_map g l.
Proof.
  intros f g l H (x & Hx & Hlt). induction l as [|y r IH]; [destruct Hx|]. cbn.
  destruct Hx as [<-|Hx].
  - pose proof (sum_map_le f g r (fun z Hz => H z (or_intror Hz))). lia.
  - pose proof (H y (or_introl eq_refl)). pose proof (IH (fun z Hz => H z (or_intror Hz)) Hx). lia.
Qed.

Lemma sum_map_bound : forall f l k, (forall x, In x l -> f x <= k) -> sum_map f l <= List.length l * k.
Proof. intros f l k H. induction l as [|x r IH]; cbn; [lia|]. pose proof (H x (or_introl eq_refl)). pose proof (IH (fun y Hy => H y (or_intror Hy))). lia. Qed.

Lemma nodup_bound_length : forall N l, NoDup l -> (forall y, In y l -> y < N) -> List.length l <= N.
Proof.
  intros N l Hn Hb. rewrite <- (length_seq N 0). apply NoDup_incl_length; [exact Hn|].
  intros y Hy. apply in_seq. pose proof (Hb y Hy). lia.
Qed.

Section Kleene.

Variable N : nat.
Variable F : (nat -> list nat) -> nat -> list nat.
Hypothesis F_mono : forall R R', (forall x, incl (R x) (R' x)) -> forall x, incl (F R x) (F R' x).
Hypothesis F_nodup : forall R x, NoDup (F R x).
Hypothesis F_bound : forall R x y, In y (F R x) -> y < N.
Hypothesis F_out : forall R x, N <= x -> F R x = [].

Fixpoint it (k : nat) : nat -> list nat :=
  match k with 0 => fun _ => [] | S k => F (it k) end.

Lemma it_chain : forall k x, incl (it k x) (it (S k) x).
Proof.
  induction k as [|k IH]; intros x; [intros y []|]. change (incl (F (it k) x) (F (it (S k)) x)).
  apply F_mono. exact IH.
Qed.

Lemma it_nodup : forall k x, NoDup (it k x).
Proof. intros [|k] x; [constructor | apply F_nodup]. Qed.

Lemma it_bound : forall k x y, In y (it k x) -> y < N.
Proof. intros [|k] x y H; [destruct H | exact (F_bound _ _ _ H)]. Qed.

Lemma it_out : forall k x, N <= x -> it k x = [].
Proof. intros [|k] x H; [reflexivity | exact (F_out _ _ H)]. Qed.

(* every iterate is inside anything F maps into itself *)
Lemma it_least : forall (Q : nat -> nat -> Prop),
  (forall R, (forall x y, In y (R x) -> Q x y) -> forall x y, In y (F R x) -> Q x y) ->
  forall k x y, In y (it k x) -> Q x y.
Proof. intros Q HQ k. induction k as [|k IH]; intros x y H; [destruct H | exact (HQ _ IH x y H)]. Qed.

Definition stableb (k : nat) : bool := forallb (fun x => inclb (it (S k) x) (it k x)) (seq 0 N).

Definition stable (k : nat) : Prop := forall x, incl (it (S k) x) (it k x).

Lemma stableb_spec : forall k, stableb k = true -> stable k.
Proof.
  intros k H x. destruct (le_lt_dec N x) as [Hx|Hx].
  - rewrite (it_out (S k) x Hx). intros y [].
  - unfold stableb in H. rewrite forallb_forall in H. apply inclb_spec, H, in_seq. lia.
Qed.

Lemma stable_next : forall k, stable k -> stable (S k).
Proof. intros k H x. change (incl (F (it (S k)) x) (F (it k) x)). apply F_mono. exact H. Qed.

Definition measure (k : nat) : nat := sum_map (fun x => List.length (it k x)) (seq 0 N).

Lemma measure_bound : forall k, measure k <= N * N.
Proof.
  intro k. unfold measure.
  assert (H : sum_map (fun x => List.length (it k x)) (seq 0 N) <= List.length (seq 0 N) * N).
  { apply sum_map_bound. intros x _. apply nodup_bound_length; [apply it_nodup | apply it_bound]. }
  rewrite length_seq in H. exact H.
Qed.

Lemma measure_grows : forall k, stableb k = false -> measure k < measure (S k).
Proof.
  intros k H. unfold measure. apply sum_map_lt.
  - intros x _. apply NoDup_incl_length; [apply it_nodup | apply it_chain].
  - unfold stableb in H. apply forallb_false_ex in H as (x & Hx & Hb). exists x. split; [exact Hx|].
    destruct (Nat.lt_ge_cases (List.length (it k x)) (List.length (it (S k) x))) as [Hl|Hl]; [exact Hl|].
    exfalso. assert (I : incl (it (S k) x) (it k x)) by (apply (NoDup_length_incl (it_nodup k x)); [exact Hl | apply it_chain]).
    apply inclb_spec in I. congruence.
Qed.

Lemma measure_or_stable : forall k, (exists j, j < k /\ stableb j = true) \/ k <= measure k.
Proof.
  induction k as [|k IH]; [right; lia|].
  destruct IH as [(j & Hj & Hs)|Hm]; [left; exists j; split; [lia | exact Hs]|].
  destruct (stableb k) eqn:Hs; [left; exists k; split; [lia | exact Hs]|].
  right. pose proof (measure_grows k Hs). lia.
Qed.

Definition K := S (N * N).

Definition lfp : nat -> list nat := it K.

Lemma stable_from : forall j k, j <= k -> stable j -> stable k.
Proof. intros j k Hjk Hj. induction Hjk as [|k _ IH]; [exact Hj | exact (stable_next k IH)]. Qed.

(* the iteration has reached a fixed point by K rounds *)
Lemma lfp_closed : forall x, incl (F lfp x) (lfp x).
Proof.
  intro x. destruct (measure_or_stable K) as [(j & Hj & Hs)|Hm].
  - assert (Hle : j <= K) by (apply Nat.lt_le_incl; exact Hj).
    exact (stable_from j K Hle (stableb_spec j Hs) x).
  - exfalso. pose proof (measure_bound K) as Hb. unfold K in Hm, Hb. lia.
Qed.

Lemma lfp_nodup : forall x, NoDup (lfp x).
Proof. intro x. apply it_nodup. Qed.

Lemma lfp_least : forall (Q : nat -> nat -> Prop),
  (forall R, (forall x y, In y (R x) -> Q x y) -> forall x y, In y (F R x) -> Q x y) ->
  forall x y, In y (lfp x) -> Q x y.
Proof. intros Q HQ. exact (it_least Q HQ K). Qed.

End Kleene.


(* ======================================================================== *)
(* 2. The world's lists, computed                                            *)
(* ======================================================================== *)

Section Total.

Variable b : bundle.
Variable ls : load_st.
Variable e : nat.
Hypothesis HI : LInv b ls.

Let w := LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)).
Let W := W0r w.
Let N := ls_next_sym ls.

Lemma decl_none' : forall x, nget (ls_syms ls) x = None -> A.w_decl W x = None.
Proof. intros x H. exact (decl_none b ls e (fun _ => []) (fun _ => 0) x H). Qed.

Lemma rsym_loader' : forall p pa x, A.resolve_sym W p pa = Some x -> exists g, nget (ls_syms ls) x = Some g.
Proof. intros p pa x H. exact (rsym_loader b ls e HI (fun _ => []) (fun _ => 0) p pa x H). Qed.

Lemma loader_lt : forall x g, nget (ls_syms ls) x = Some g -> x < N.
Proof. intros x g H. exact (proj1 (proj2 (li_syms _ _ HI x g H))). Qed.

Lemma decl_loader : forall x d, A.w_decl W x = Some d -> exists g, nget (ls_syms ls) x = Some g.
Proof.
  intros x d H. destruct (nget (ls_syms ls) x) as [g|] eqn:E; [exists g; reflexivity|].
  rewrite (decl_none' x E) in H. discriminate.
Qed.

Lemma decl_lt : forall x d, A.w_decl W x = Some d -> x < N.
Proof. intros x d H. destruct (decl_loader x d H) as (g & Hg). exact (loader_lt x g Hg). Qed.

Lemma decl_out : forall x, N <= x -> A.w_decl W x = None.
Proof.
  intros x H. destruct (A.w_decl W x) as [d|] eqn:E; [|reflexivity]. pose proof (decl_lt x d E). lia.
Qed.

Lemma rsym_lt : forall p pa x, A.resolve_sym W p pa = Some x -> x < N.
Proof. intros p pa x H. destruct (rsym_loader' p pa x H) as (g & Hg). exact (loader_lt x g Hg). Qed.

Lemma kind_lt : forall x k, A.kind_sym W x = Some k -> x < N.
Proof.
  intros x k H. unfold A.kind_sym in H. destruct (A.w_decl W x) as [d|] eqn:E; [exact (decl_lt x d E) | discriminate].
Qed.

Definition clean (l : list nat) : list nat := nodup Nat.eq_dec (filter (fun y => y <? N) l).

Lemma in_clean : forall y l, In y (clean l) <-> In y l /\ y < N.
Proof. intros y l. unfold clean. rewrite nodup_In, filter_In, Nat.ltb_lt. tauto. Qed.

Lemma clean_nodup : forall l, NoDup (clean l).
Proof. intro l. apply NoDup_nodup. Qed.

(* ---- the type symbols of contexts (SpecTypes) ---- *)

Definition item_types (q : string) (sp : spec) : list nat :=
  match sp with
  | Spec (Some pa) None _ =>
      match A.resolve_sym W q pa with
      | Some t => match A.kind_sym W t with Some A.DKType => [t] | _ => [] end
      | None => []
      end
  | _ => []
  end.

Definition item_ctxs (q : string) (sp : spec) : list nat :=
  match sp with
  | Spec (Some pa) None _ =>
      match A.resolve_sym W q pa with
      | Some c => match A.w_decl W c with
                  | Some d => match A.d_ast d with ADCtx _ _ => [c] | _ => [] end
                  | None => []
                  end
      | None => []
      end
  | _ => []
  end.

Definition walkset (R : nat -> list nat) (q : string) (its : list spec) : list nat :=
  flat_map (fun sp => item_types q sp ++ flat_map R (item_ctxs q sp)) its.

Lemma walkset_mono : forall R R' q its, (forall x, incl (R x) (R' x)) -> incl (walkset R q its) (walkset R' q its).
Proof.
  intros R R' q its H y Hy. unfold walkset in *. apply in_flat_map in Hy as (sp & Hsp & Hy).
  apply in_flat_map. exists sp. split; [exact Hsp|]. apply in_app_or in Hy as [Hy|Hy]; apply in_or_app; [left; exact Hy|].
  right. apply in_flat_map in Hy as (c & Hc & Hy). apply in_flat_map. exists c. split; [exact Hc | exact (H c y Hy)].
Qed.

Definition Fctx (R : nat -> list nat) (c : nat) : list nat :=
  match A.w_decl W c with
  | Some d => match A.d_ast d with ADCtx _ its => clean (walkset R (A.d_mod d) (flat_map citem_specs its)) | _ => [] end
  | None => []
  end.

Definition CT : nat -> list nat := lfp N Fctx.

Lemma Fctx_mono : forall R R', (forall x, incl (R x) (R' x)) -> forall x, incl (Fctx R x) (Fctx R' x).
Proof.
  intros R R' H x y Hy. unfold Fctx in *. destruct (A.w_decl W x) as [d|]; [|exact Hy].
  destruct (A.d_ast d); try exact Hy. apply in_clean in Hy as [Hy Hn]. apply in_clean. split; [|exact Hn].
  exact (walkset_mono R R' _ _ H y Hy).
Qed.

Lemma Fctx_nodup : forall R x, NoDup (Fctx R x).
Proof. intros R x. unfold Fctx. destruct (A.w_decl W x) as [d|]; [|constructor]. destruct (A.d_ast d); try constructor. apply clean_nodup. Qed.

Lemma Fctx_bound : forall R x y, In y (Fctx R x) -> y < N.
Proof.
  intros R x y H. unfold Fctx in H. destruct (A.w_decl W x) as [d|]; [|destruct H].
  destruct (A.d_ast d); try destruct H. exact (proj2 (proj1 (in_clean _ _) H)).
Qed.

Lemma Fctx_out : forall R x, N <= x -> Fctx R x = [].
Proof. intros R x H. unfold Fctx. rewrite (decl_out x H). reflexivity. Qed.

Lemma spec_lt : forall q sp t, A.SpecTypes W q sp t -> t < N.
Proof. intros q sp t H. induction H; [exact (rsym_lt _ _ _ H) | exact IHSpecTypes]. Qed.

Lemma walkset_sound : forall R q its t,
  (forall c t, In t (R c) -> exists d n its', A.w_decl W c = Some d /\ A.d_ast d = ADCtx n its' /\
                               exists sp, In sp (flat_map citem_specs its') /\ A.SpecTypes W (A.d_mod d) sp t) ->
  In t (walkset R q its) -> exists sp, In sp its /\ A.SpecTypes W q sp t.
Proof.
  intros R q its t HR H. unfold walkset in H. apply in_flat_map in H as (sp & Hsp & H). exists sp. split; [exact Hsp|].
  destruct sp as [[pa|] [n0|] a]; cbn [item_types item_ctxs app flat_map] in H; try contradiction.
  destruct (A.resolve_sym W q pa) as [x|] eqn:Er; [|destruct H].
  apply in_app_or in H as [H|H].
  - destruct (A.kind_sym W x) as [[]|] eqn:Ek; cbn in H; try contradiction. destruct H as [<-|[]]. apply A.STType; assumption.
  - apply in_flat_map in H as (c & Hc & H). destruct (A.w_decl W x) as [d|] eqn:Ed; [|cbn in Hc; contradiction].
    destruct (A.d_ast d) eqn:Ea; cbn in Hc; try contradiction. destruct Hc as [<-|[]].
    destruct (HR x t H) as (d' & n' & its' & Ed' & Ea' & sp' & Hsp' & Hst). rewrite Ed in Ed'. injection Ed' as <-.
    eapply A.STCtx; [exact Er | exact Ed | exact Ea' | exact Hsp' | exact Hst].
Qed.

Lemma CT_sound : forall c t, In t (CT c) -> exists d n its, A.w_decl W c = Some d /\ A.d_ast d = ADCtx n its /\
  exists sp, In sp (flat_map citem_specs its) /\ A.SpecTypes W (A.d_mod d) sp t.
Proof.
  apply (lfp_least N Fctx). intros R HR c t H. unfold Fctx in H. destruct (A.w_decl W c) as [d|] eqn:Ed; [|destruct H].
  destruct (A.d_ast d) eqn:Ea; try destruct H. apply in_clean in H as [H _].
  exists d, n, l. split; [reflexivity|]. split; [exact Ea|]. exact (walkset_sound R _ _ t HR H).
Qed.

Lemma CT_complete : forall q sp t, A.SpecTypes W q sp t -> In t (walkset CT q [sp]).
Proof.
  intros q sp t H. induction H as [q pa a t Er Kt | q pa a c d n its sp t Er Ed Ea Hsp Hst IH].
  - cbn. rewrite Er, Kt. left. reflexivity.
  - cbn [walkset flat_map item_types item_ctxs]. rewrite Er, Ed, Ea. rewrite app_nil_r. apply in_or_app. right.
    cbn [flat_map]. rewrite app_nil_r. apply (lfp_closed N Fctx Fctx_mono Fctx_nodup Fctx_bound Fctx_out c).
    unfold Fctx. rewrite Ed, Ea. apply in_clean. split; [|exact (spec_lt _ _ _ Hst)].
    unfold walkset. apply in_flat_map. exists sp. split; [exact Hsp|].
    unfold walkset in IH. cbn [flat_map] in IH. rewrite app_nil_r in IH. exact IH.
Qed.

(* the requirements of a val, fn, method, context or functor *)
Definition Passume (s : nat) : list nat :=
  match A.w_decl W s with
  | Some d => clean (walkset CT (A.d_mod d) (LoadWorld.assume_specs (A.d_assumes d)))
  | None => []
  end.

Lemma Passume_spec : forall s t, In t (Passume s) <-> A.AssumeTypes W s t.
Proof.
  intros s t. unfold Passume. split.
  - destruct (A.w_decl W s) as [d|] eqn:Ed; [|intros []]. intro H. apply in_clean in H as [H _].
    destruct (walkset_sound CT _ _ t CT_sound H) as (sp & Hsp & Hst).
    exists d, sp. split; [exact Ed|]. split; [exact Hsp | exact Hst].
  - intros (d & sp & Ed & Hin & Hst). rewrite Ed. apply in_clean. split; [|exact (spec_lt _ _ _ Hst)].
    pose proof (CT_complete _ _ _ Hst) as H. unfold walkset in *. cbn [flat_map] in H. rewrite app_nil_r in H.
    apply in_flat_map. exists sp. split; [exact Hin|exact H].
Qed.

Lemma Passume_nodup : forall s, NoDup (Passume s).
Proof. intro s. unfold Passume. destruct (A.w_decl W s); [apply clean_nodup | constructor]. Qed.


(* ---- FreeIn, computed ---- *)

Definition bound_of (p : string) (bs : list (path * spec)) : list nat :=
  flat_map (fun b0 => match A.resolve_sym W p (fst b0) with Some x => [x] | None => [] end) bs.

Lemma in_bound_of : forall p bs x, In x (bound_of p bs) <-> exists b0, In b0 bs /\ A.resolve_sym W p (fst b0) = Some x.
Proof.
  intros p bs x. unfold bound_of. rewrite in_flat_map. split.
  - intros (b0 & Hb & Hx). exists b0. split; [exact Hb|]. destruct (A.resolve_sym W p (fst b0)); [destruct Hx as [<-|[]]; reflexivity | destruct Hx].
  - intros (b0 & Hb & Hx). exists b0. split; [exact Hb|]. rewrite Hx. left. reflexivity.
Qed.

Definition here_of (R : nat -> list nat) (g : nat) : list nat :=
  match A.kind_sym W g with Some A.DKType => [g] | _ => R g end.

Fixpoint freeSpec (R : nat -> list nat) (p : string) (sp0 : spec) {struct sp0} : list nat :=
  match sp0 with
  | Spec (Some pa) _ a =>
      match A.resolve_sym W p pa with
      | None => []
      | Some g =>
          filter (fun y => negb (nmem y (bound_of p (match a with Some bs => bs | None => [] end)))) (here_of R g) ++
          match a with
          | None => []
          | Some bs => (fix go (l : list (path * spec)) : list nat :=
                          match l with [] => [] | (_, s') :: r => freeSpec R p s' ++ go r end) bs
          end
      end
  | _ => []
  end.

Fixpoint freeT (R : nat -> list nat) (p : string) (t : aty) {struct t} : list nat :=
  match t with
  | ATRec fs => (fix go (l : list (name * aty)) : list nat :=
                   match l with [] => [] | (_, t') :: r => freeT R p t' ++ go r end) fs
  | ATUnion ms => (fix go (l : list aty) : list nat :=
                     match l with [] => [] | t' :: r => freeT R p t' ++ go r end) ms
  | ATRef pa a => freeSpec R p (Spec (Some pa) None a)
  | _ => []
  end.

Lemma freeSpec_eq : forall R p pa d a, freeSpec R p (Spec (Some pa) d a) =
  match A.resolve_sym W p pa with
  | None => []
  | Some g => filter (fun y => negb (nmem y (bound_of p (match a with Some bs => bs | None => [] end)))) (here_of R g) ++
              flat_map (fun b0 => freeSpec R p (snd b0)) (match a with Some bs => bs | None => [] end)
  end.
Proof.
  intros R p pa d a. cbn [freeSpec]. destruct (A.resolve_sym W p pa); [|reflexivity]. f_equal.
  destruct a as [bs|]; [|reflexivity]. induction bs as [|[bp s'] r IH]; [reflexivity|]. cbn [flat_map snd]. rewrite <- IH. reflexivity.
Qed.

Lemma freeT_rec : forall R p fs, freeT R p (ATRec fs) = flat_map (fun nt => freeT R p (snd nt)) fs.
Proof. intros R p fs. induction fs as [|[n t] r IH]; [reflexivity|]. cbn [flat_map snd]. rewrite <- IH. reflexivity. Qed.

Lemma freeT_union : forall R p ms, freeT R p (ATUnion ms) = flat_map (fun t => freeT R p t) ms.
Proof. intros R p ms. induction ms as [|t r IH]; [reflexivity|]. cbn [flat_map]. rewrite <- IH. reflexivity. Qed.

Fixpoint spec_size (sp0 : spec) : nat :=
  match sp0 with
  | Spec _ _ None => 1
  | Spec _ _ (Some bs) => S ((fix go (l : list (path * spec)) : nat :=
                                match l with [] => 0 | (_, s') :: r => spec_size s' + go r end) bs)
  end.

Lemma spec_size_in : forall o d bs b0, In b0 bs -> spec_size (snd b0) < spec_size (Spec o d (Some bs)).
Proof.
  intros o d bs b0 H. cbn [spec_size]. induction bs as [|[bp s'] r IH]; [destruct H|].
  destruct H as [<-|H]; cbn [snd]; [lia|]. specialize (IH H). lia.
Qed.

Fixpoint aty_size (t : aty) : nat :=
  match t with
  | ATRec fs => S ((fix go (l : list (name * aty)) : nat := match l with [] => 0 | (_, t') :: r => aty_size t' + go r end) fs)
  | ATUnion ms => S ((fix go (l : list aty) : nat := match l with [] => 0 | t' :: r => aty_size t' + go r end) ms)
  | _ => 1
  end.

Lemma aty_size_rec : forall fs nt, In nt fs -> aty_size (snd nt) < aty_size (ATRec fs).
Proof.
  intros fs nt H. cbn [aty_size]. induction fs as [|[n t] r IH]; [destruct H|].
  destruct H as [<-|H]; cbn [snd]; [lia|]. specialize (IH H). lia.
Qed.

Lemma aty_size_union : forall ms t, In t ms -> aty_size t < aty_size (ATUnion ms).
Proof.
  intros ms t H. cbn [aty_size]. induction ms as [|t' r IH]; [destruct H|].
  destruct H as [<-|H]; [lia|]. specialize (IH H). lia.
Qed.

Lemma freeSpec_mid : forall R p pa d a, freeSpec R p (Spec (Some pa) d a) = freeSpec R p (Spec (Some pa) None a).
Proof. intros. rewrite !freeSpec_eq. reflexivity. Qed.

Lemma freeSpec_sound : forall R p n sp0 x, spec_size sp0 < n -> In x (freeSpec R p sp0) ->
  match sp0 with Spec (Some pa) _ a => A.FreeIn W R p (ATRef pa a) x | _ => False end.
Proof.
  intros R p n. induction n as [|n IH]; intros sp0 x Hn H; [lia|].
  destruct sp0 as [[pa|] d a]; [|exact H].
  rewrite freeSpec_eq in H. destruct (A.resolve_sym W p pa) as [g|] eqn:Er; [|destruct H].
  apply in_app_or in H as [H|H].
  - apply filter_In in H as [H Hb]. apply negb_true_iff in Hb.
    assert (Hnb : ~ (exists b0, In b0 (match a with Some bs => bs | None => [] end) /\ A.resolve_sym W p (fst b0) = Some x)).
    { intro Hex. apply in_bound_of, nmem_in in Hex. congruence. }
    unfold here_of in H. destruct (A.kind_sym W g) as [[]|] eqn:Ek;
      try (eapply A.FNom; [exact Er | rewrite Ek; discriminate | exact H | exact Hnb]).
    destruct H as [<-|[]]. apply A.FType; assumption.
  - apply in_flat_map in H as (b0 & Hb0 & H).
    assert (Hs : spec_size (snd b0) < n).
    { destruct a as [bs|]; [|destruct Hb0]. pose proof (spec_size_in (Some pa) d bs b0 Hb0). lia. }
    pose proof (IH (snd b0) x Hs H) as Hf. destruct b0 as [bp [[sp|] d' sa]]; [|destruct Hf].
    eapply A.FBracket; [rewrite Er; discriminate | exact Hb0 | exact Hf].
Qed.

Lemma freeT_sound : forall R p n t x, aty_size t < n -> In x (freeT R p t) -> A.FreeIn W R p t x.
Proof.
  intros R p n. induction n as [|n IH]; intros t x Hn H; [lia|].
  destruct t as [| | |fs|ms|pa a]; try destruct H.
  - rewrite freeT_rec in H. apply in_flat_map in H as ([m t'] & Hin & H).
    pose proof (aty_size_rec fs (m, t') Hin). eapply A.FRec; [exact Hin | apply IH; [cbn [snd] in *; lia | exact H]].
  - rewrite freeT_union in H. apply in_flat_map in H as (t' & Hin & H).
    pose proof (aty_size_union ms t' Hin). eapply A.FUnion; [exact Hin | apply IH; [lia | exact H]].
  - exact (freeSpec_sound R p (S (spec_size (Spec (Some pa) None a))) _ x (Nat.lt_succ_diag_r _) H).
Qed.

Lemma freeT_complete : forall R p t x, A.FreeIn W R p t x -> In x (freeT R p t).
Proof.
  intros R p t x H. induction H as [fs n t x Hin Hf IH | ms t x Hin Hf IH | pa a t Hr Hk Hb | pa a g x Hr Hk Hx Hb
                                    | pa a bpath sp d sa x Hr Hin Hf IH].
  - rewrite freeT_rec. apply in_flat_map. exists (n, t). split; assumption.
  - rewrite freeT_union. apply in_flat_map. exists t. split; assumption.
  - cbn [freeT]. rewrite freeSpec_eq, Hr. apply in_or_app. left. apply filter_In. split.
    + unfold here_of. rewrite Hk. left. reflexivity.
    + apply negb_true_iff. destruct (nmem t _) eqn:E; [|reflexivity]. exfalso. apply Hb, in_bound_of, nmem_in, E.
  - cbn [freeT]. rewrite freeSpec_eq, Hr. apply in_or_app. left. apply filter_In. split.
    + unfold here_of. destruct (A.kind_sym W g) as [[]|]; try exact Hx. exfalso. apply Hk. reflexivity.
    + apply negb_true_iff. destruct (nmem x _) eqn:E; [|reflexivity]. exfalso. apply Hb, in_bound_of, nmem_in, E.
  - cbn [freeT]. rewrite freeSpec_eq. destruct (A.resolve_sym W p pa) as [g|] eqn:Er; [|contradiction].
    apply in_or_app. right. apply in_flat_map. exists (bpath, Spec (Some sp) d sa). split; [exact Hin|].
    cbn [snd]. rewrite freeSpec_mid. exact IH.
Qed.

Lemma freeT_spec : forall R p t x, In x (freeT R p t) <-> A.FreeIn W R p t x.
Proof.
  intros R p t x. split; [apply (freeT_sound R p (S (aty_size t))); lia | apply freeT_complete].
Qed.

Lemma FreeIn_bound : forall R p t x, (forall h y, In y (R h) -> y < N) -> A.FreeIn W R p t x -> x < N.
Proof.
  intros R p t x HR H. induction H as [| | pa a t Hr _ _ | pa a g x _ _ Hx _ |]; auto.
  - exact (rsym_lt _ _ _ Hr).
  - exact (HR _ _ Hx).
Qed.

(* ---- the relevance of tags and aliases ---- *)

Definition nominal_kb (k : option A.dkind) : bool :=
  match k with Some A.DKTag | Some A.DKAlias => true | _ => false end.

Definition plain_kb (k : option A.dkind) : bool :=
  match k with Some A.DKVal | Some A.DKFn | Some A.DKMeth | Some A.DKCtx | Some A.DKFunctor => true | _ => false end.

(* the fixed lists: of a val, fn, method, context or functor AssumeTypes,
   of anything else (types, units) none *)
Definition Pfix (h : nat) : list nat := if plain_kb (A.kind_sym W h) then Passume h else [].

Definition mixR (R : nat -> list nat) (h : nat) : list nat :=
  if nominal_kb (A.kind_sym W h) then R h else Pfix h.

Definition listedW (q : string) (ps : list name) : list nat :=
  flat_map (fun n => match A.resolve_sym W q [n] with Some x => [x] | None => [] end) ps.

Definition Fnom (R : nat -> list nat) (g : nat) : list nat :=
  match A.w_decl W g with
  | Some d => match A.nominal_body (A.d_ast d) with
              | Some (ps, t) => clean (listedW (A.d_mod d) ps ++ freeT (mixR R) (A.d_mod d) t)
              | None => []
              end
  | None => []
  end.

Definition Lnom : nat -> list nat := lfp N Fnom.

Lemma Fnom_mono : forall R R', (forall x, incl (R x) (R' x)) -> forall x, incl (Fnom R x) (Fnom R' x).
Proof.
  intros R R' H x y Hy. unfold Fnom in *. destruct (A.w_decl W x) as [d|]; [|exact Hy].
  destruct (A.nominal_body (A.d_ast d)) as [[ps t]|]; [|exact Hy].
  apply in_clean in Hy as [Hy Hn]. apply in_clean. split; [|exact Hn]. apply in_app_or in Hy as [Hy|Hy]; apply in_or_app; [left; exact Hy|].
  right. apply freeT_spec. apply freeT_spec in Hy. revert Hy. apply FreeIn_mono.
  intros h _ _. unfold mixR. destruct (nominal_kb (A.kind_sym W h)); [apply H | apply incl_refl].
Qed.

Lemma Fnom_nodup : forall R x, NoDup (Fnom R x).
Proof.
  intros R x. unfold Fnom. destruct (A.w_decl W x) as [d|]; [|constructor].
  destruct (A.nominal_body (A.d_ast d)) as [[ps t]|]; [apply clean_nodup | constructor].
Qed.

Lemma Fnom_bound : forall R x y, In y (Fnom R x) -> y < N.
Proof.
  intros R x y H. unfold Fnom in H. destruct (A.w_decl W x) as [d|]; [|destruct H].
  destruct (A.nominal_body (A.d_ast d)) as [[ps t]|]; [exact (proj2 (proj1 (in_clean _ _) H)) | destruct H].
Qed.

Lemma Fnom_out : forall R x, N <= x -> Fnom R x = [].
Proof. intros R x H. unfold Fnom. rewrite (decl_out x H). reflexivity. Qed.

Lemma Lnom_closed : forall x, incl (Fnom Lnom x) (Lnom x).
Proof. exact (lfp_closed N Fnom Fnom_mono Fnom_nodup Fnom_bound Fnom_out). Qed.

Lemma Lnom_nodup : forall x, NoDup (Lnom x).
Proof. intro x. apply (lfp_nodup N Fnom Fnom_nodup). Qed.

Lemma Lnom_bound : forall x y, In y (Lnom x) -> y < N.
Proof. intros x y H. exact (it_bound N Fnom Fnom_bound _ x y H). Qed.

Lemma Pfix_bound : forall h y, In y (Pfix h) -> y < N.
Proof.
  intros h y H. unfold Pfix, Passume in H. destruct (plain_kb (A.kind_sym W h)); [|destruct H].
  destruct (A.w_decl W h); [exact (proj2 (proj1 (in_clean _ _) H)) | destruct H].
Qed.

Lemma nominal_body_kind : forall x d ps t, A.w_decl W x = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) ->
  nominal_kb (A.kind_sym W x) = true.
Proof.
  intros x d ps t Hd Hb. unfold A.kind_sym. rewrite Hd. destruct (A.d_ast d); cbn in Hb |- *; try discriminate; reflexivity.
Qed.

(* Lnom, read with the fixed lists, is relevance-closed *)
Lemma mix_closed : A.RelevanceClosed W (mixR Lnom).
Proof.
  intros g d ps t Hd Hb. assert (Hm : mixR Lnom g = Lnom g) by (unfold mixR; rewrite (nominal_body_kind g d ps t Hd Hb); reflexivity).
  rewrite Hm. split.
  - intros n x Hn Hr. apply Lnom_closed. unfold Fnom. rewrite Hd, Hb. apply in_clean. split; [|exact (rsym_lt _ _ _ Hr)].
    apply in_or_app. left. unfold listedW. apply in_flat_map. exists n. split; [exact Hn|]. rewrite Hr. left. reflexivity.
  - intros x Hx. apply Lnom_closed. unfold Fnom. rewrite Hd, Hb. apply in_clean. split.
    + apply in_or_app. right. apply freeT_spec. exact Hx.
    + apply (FreeIn_bound (mixR Lnom) (A.d_mod d) t x); [|exact Hx].
      intros h y Hy. unfold mixR in Hy. destruct (nominal_kb (A.kind_sym W h));
        [exact (Lnom_bound _ _ Hy) | exact (Pfix_bound _ _ Hy)].
Qed.

(* Lnom is inside every relevance-closed R that holds the fixed lists *)
Lemma Lnom_least : forall R, A.RelevanceClosed W R ->
  (forall h, nominal_kb (A.kind_sym W h) = false -> incl (Pfix h) (R h)) ->
  forall g, nominal_kb (A.kind_sym W g) = true -> incl (Lnom g) (R g).
Proof.
  intros R Hc Hp g Hg y Hy. revert Hg.
  refine (lfp_least N Fnom (fun x y => nominal_kb (A.kind_sym W x) = true -> In y (R x)) _ g y Hy).
  intros R' HR' x z Hz Hx. unfold Fnom in Hz. destruct (A.w_decl W x) as [d|] eqn:Hd; [|destruct Hz].
  destruct (A.nominal_body (A.d_ast d)) as [[ps t]|] eqn:Hb; [|destruct Hz].
  destruct (Hc x d ps t Hd Hb) as [C1 C2]. apply in_clean in Hz as [Hz _]. apply in_app_or in Hz as [Hz|Hz].
  - unfold listedW in Hz. apply in_flat_map in Hz as (n & Hn & Hz).
    destruct (A.resolve_sym W (A.d_mod d) [n]) as [x'|] eqn:Er; [|destruct Hz]. destruct Hz as [<-|[]]. exact (C1 n x' Hn Er).
  - apply C2. apply freeT_spec in Hz. revert Hz. apply FreeIn_mono. intros h _ _. unfold mixR.
    destruct (nominal_kb (A.kind_sym W h)) eqn:Eh; [|exact (Hp h Eh)]. intros u Hu. exact (HR' h u Hu Eh).
Qed.


(* ======================================================================== *)
(* 3. The reading's lists: the cache where the elaborator has one           *)
(* ======================================================================== *)

Lemma loader_kind' : forall x g, nget (ls_syms ls) x = Some g -> A.kind_sym W x = Some (kconv (gs_kind g)).
Proof. intros x g H. exact (loader_kind b ls e HI (fun _ => []) (fun _ => 0) x g H). Qed.

Lemma visible_loader' : forall x, A.kind_sym W x <> None -> exists g, nget (ls_syms ls) x = Some g.
Proof. intros x H. exact (visible_loader b ls e (fun _ => []) (fun _ => 0) x H). Qed.

Lemma headin_visible' : forall p t h, HeadIn W p t h -> A.kind_sym W h <> None.
Proof. intros p t h H. exact (headin_visible b ls e HI (fun _ => []) (fun _ => 0) p t h H). Qed.

Lemma nomW_kb : forall x, NomW W x <-> nominal_kb (A.kind_sym W x) = true.
Proof. intro x. unfold NomW. destruct (A.kind_sym W x) as [[]|]; cbn; split; intro H; try discriminate; auto; destruct H; discriminate. Qed.

Variable syms : nmap gsym.
Variable c : nmap (list nat).
Hypothesis Hkeep : forall i g, nget (ls_syms ls) i = Some g -> nget syms i = Some g.
Hypothesis Hreq : forall x l, nget c x = Some l -> NoDup l /\ exists g, nget syms x = Some g /\
  (kind_plain (gs_kind g) = true -> forall t, In t l <-> A.AssumeTypes W x t) /\
  (gs_kind g = KType \/ gs_kind g = KUnit -> l = []).
Hypothesis Hrc : RC W c.
Hypothesis Hrl : RL W c.
Hypothesis Hlisted : forall g d ps t n, A.w_decl W g = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) -> In n ps ->
  exists x, A.resolve_sym W (A.d_mod d) [n] = Some x /\ A.AssumeTypes W g x.

(* a cached list of a symbol that is no tag or alias is the fixed one *)
Lemma cached_fix : forall x l, nget c x = Some l -> nominal_kb (A.kind_sym W x) = false -> A.kind_sym W x <> None ->
  forall t, In t l <-> In t (Pfix x).
Proof.
  intros x l Hc Hn Hv t. destruct (Hreq x l Hc) as (_ & g & Hg & P1 & P2).
  destruct (visible_loader' x Hv) as (g0 & Hg0). rewrite (Hkeep x g0 Hg0) in Hg. injection Hg as <-.
  unfold Pfix. rewrite (loader_kind' x g0 Hg0) in Hn |- *.
  destruct (gs_kind g0) eqn:Kg; cbn in Hn |- *; try discriminate.
  1, 2: rewrite (P2 ltac:(auto)); tauto.
  all: rewrite (P1 eq_refl), Passume_spec; tauto.
Qed.

Definition Rfin (x : nat) : list nat :=
  match nget c x with
  | Some l => l
  | None => if nominal_kb (A.kind_sym W x) then Lnom x else Pfix x
  end.

Lemma cache_in_L : forall g l, nominal_kb (A.kind_sym W g) = true -> nget c g = Some l -> incl l (Lnom g).
Proof.
  intros g l Hg Hl. assert (E : mixR Lnom g = Lnom g) by (unfold mixR; rewrite Hg; reflexivity). rewrite <- E.
  apply (Hrl (mixR Lnom) mix_closed); [|apply nomW_kb, Hg | exact Hl].
  intros h l' Hv Hn Hh y Hy. assert (Hk : nominal_kb (A.kind_sym W h) = false)
    by (destruct (nominal_kb (A.kind_sym W h)) eqn:E'; [exfalso; apply Hn, nomW_kb, E' | reflexivity]).
  unfold mixR. rewrite Hk. exact (proj1 (cached_fix h l' Hh Hk Hv y) Hy).
Qed.

(* the cache where it has a tag or alias, Lnom elsewhere *)
Definition R2 (x : nat) : list nat :=
  if nominal_kb (A.kind_sym W x) then match nget c x with Some l => l | None => Lnom x end else Pfix x.

Lemma R2_closed : A.RelevanceClosed W R2.
Proof.
  intros g d ps t Hd Hb. pose proof (nominal_body_kind g d ps t Hd Hb) as Hg.
  destruct (nget c g) as [l|] eqn:Hl;
    [assert (E2 : R2 g = l) by (unfold R2; rewrite Hg, Hl; reflexivity)
    |assert (E2 : R2 g = Lnom g) by (unfold R2; rewrite Hg, Hl; reflexivity)]; rewrite E2.
  - destruct (Hrc g l (proj2 (nomW_kb g) Hg) Hl d ps t Hd Hb) as (F1 & F2 & F3 & _).
    assert (Ecv : cv c g = l) by (unfold cv; rewrite Hl; reflexivity). rewrite Ecv in F2, F3.
    split; [exact F2|]. intros x Hx. apply F3. revert Hx. apply FreeIn_mono. intros h Hh Hk.
    pose proof (F1 h Hh Hk) as Hc. destruct (nget c h) as [lh|] eqn:Eh; [|contradiction].
    unfold R2, cv. rewrite Eh. cbn [opt_default].
    destruct (nominal_kb (A.kind_sym W h)) eqn:Ek; [apply incl_refl|].
    intros y Hy. exact (proj2 (cached_fix h lh Eh Ek (headin_visible' _ _ _ Hh) y) Hy).
  - destruct (mix_closed g d ps t Hd Hb) as [M1 M2].
    assert (E : mixR Lnom g = Lnom g) by (unfold mixR; rewrite Hg; reflexivity). rewrite E in M1, M2.
    split; [exact M1|]. intros x Hx. apply M2. revert Hx. apply FreeIn_mono. intros h _ _.
    unfold R2, mixR. destruct (nominal_kb (A.kind_sym W h)) eqn:Ek; [|apply incl_refl].
    destruct (nget c h) as [lh|] eqn:Eh; [exact (cache_in_L h lh Ek Eh) | apply incl_refl].
Qed.

Lemma L_in_cache : forall g l, nominal_kb (A.kind_sym W g) = true -> nget c g = Some l -> incl (Lnom g) l.
Proof.
  intros g l Hg Hl. assert (E : R2 g = l) by (unfold R2; rewrite Hg, Hl; reflexivity). rewrite <- E.
  apply (Lnom_least R2 R2_closed); [|exact Hg]. intros h Hh. unfold R2. rewrite Hh. apply incl_refl.
Qed.

(* the reading's lists agree with R2 at every symbol of the world *)
Lemma Rfin_R2 : forall x, A.kind_sym W x <> None -> forall y, In y (Rfin x) <-> In y (R2 x).
Proof.
  intros x Hv y. unfold Rfin, R2. destruct (nominal_kb (A.kind_sym W x)) eqn:Ek.
  - destruct (nget c x); tauto.
  - destruct (nget c x) as [l|] eqn:Hl; [exact (cached_fix x l Hl Ek Hv y) | tauto].
Qed.

Lemma Rfin_closed : A.RelevanceClosed W Rfin.
Proof.
  intros g d ps t Hd Hb. pose proof (nominal_body_kind g d ps t Hd Hb) as Hg.
  assert (Hv : A.kind_sym W g <> None) by (intro E; rewrite E in Hg; discriminate).
  destruct (R2_closed g d ps t Hd Hb) as [C1 C2]. split.
  - intros n x Hn Hr. apply Rfin_R2; [exact Hv | exact (C1 n x Hn Hr)].
  - intros x Hx. apply Rfin_R2; [exact Hv|]. apply C2. revert Hx. apply FreeIn_mono. intros h Hh _ y Hy.
    exact (proj1 (Rfin_R2 h (headin_visible' _ _ _ Hh) y) Hy).
Qed.

Lemma Rfin_least : forall R, A.RelevanceClosed W R ->
  (forall h, A.kind_sym W h <> Some A.DKTag -> A.kind_sym W h <> Some A.DKAlias -> R h = Rfin h) ->
  forall g, (A.kind_sym W g = Some A.DKTag \/ A.kind_sym W g = Some A.DKAlias) -> incl (Rfin g) (R g).
Proof.
  intros R Hc Ha g Hg.
  assert (HL : incl (Lnom g) (R g)).
  { apply (Lnom_least R Hc); [|destruct Hg as [E|E]; rewrite E; reflexivity].
    intros h Hh. assert (Ht : A.kind_sym W h <> Some A.DKTag) by (intro E; rewrite E in Hh; discriminate).
    assert (Hal : A.kind_sym W h <> Some A.DKAlias) by (intro E; rewrite E in Hh; discriminate).
    rewrite (Ha h Ht Hal). intros y Hy. unfold Rfin.
    destruct (nget c h) as [l|] eqn:Hl; [|rewrite Hh; exact Hy].
    destruct (A.kind_sym W h) as [k|] eqn:Ek.
    - apply (cached_fix h l Hl); [rewrite Ek; exact Hh | rewrite Ek; discriminate | exact Hy].
    - unfold Pfix in Hy. rewrite Ek in Hy. destruct Hy. }
  assert (Hgk : nominal_kb (A.kind_sym W g) = true) by (destruct Hg as [E|E]; rewrite E; reflexivity).
  unfold Rfin. destruct (nget c g) as [l|] eqn:Hl; [|rewrite Hgk; exact HL].
  intros y Hy. apply HL, (cache_in_L g l Hgk Hl), Hy.
Qed.

Lemma Rfin_nodup : forall x, NoDup (Rfin x).
Proof.
  intro x. unfold Rfin. destruct (nget c x) as [l|] eqn:Hl; [exact (proj1 (Hreq x l Hl))|].
  destruct (nominal_kb (A.kind_sym W x)); [apply Lnom_nodup|]. unfold Pfix.
  destruct (plain_kb (A.kind_sym W x)); [apply Passume_nodup | constructor].
Qed.

Lemma Rfin_plain : forall x k, A.kind_sym W x = Some k ->
  k <> A.DKTag -> k <> A.DKAlias -> k <> A.DKType -> k <> A.DKUnit ->
  forall t, In t (Rfin x) <-> A.AssumeTypes W x t.
Proof.
  intros x k Hk H1 H2 H3 H4 t. assert (Hn : nominal_kb (A.kind_sym W x) = false) by (rewrite Hk; destruct k; cbn; congruence).
  assert (Hp : Pfix x = Passume x) by (unfold Pfix; rewrite Hk; destruct k; cbn; congruence).
  unfold Rfin. destruct (nget c x) as [l|] eqn:Hl.
  - rewrite (cached_fix x l Hl Hn ltac:(rewrite Hk; discriminate) t), Hp. apply Passume_spec.
  - rewrite Hn, Hp. apply Passume_spec.
Qed.

Lemma Rfin_none : forall x, A.kind_sym W x = Some A.DKType \/ A.kind_sym W x = Some A.DKUnit -> Rfin x = [].
Proof.
  intros x Hk. assert (Hn : nominal_kb (A.kind_sym W x) = false) by (destruct Hk as [E|E]; rewrite E; reflexivity).
  assert (Hp : Pfix x = []) by (unfold Pfix; destruct Hk as [E|E]; rewrite E; reflexivity).
  unfold Rfin. destruct (nget c x) as [l|] eqn:Hl; [|rewrite Hn; exact Hp].
  destruct l as [|y r]; [reflexivity|]. exfalso.
  pose proof (proj1 (cached_fix x (y :: r) Hl Hn ltac:(destruct Hk as [E|E]; rewrite E; discriminate) y) (or_introl eq_refl)) as H.
  rewrite Hp in H. destruct H.
Qed.

End Total.

(* ======================================================================== *)
(* 4. In the reading W_read w Rfin F                                         *)
(* ======================================================================== *)

Lemma FreeIn_RF : forall w R1 F1 R2 F2 R p t x,
  A.FreeIn (W_read w R1 F1) R p t x -> A.FreeIn (W_read w R2 F2) R p t x.
Proof.
  intros w R1 F1 R2 F2 R p t x H.
  induction H as [fs n t x Hin _ IH | ms t x Hin _ IH | pa a t Hr Hk Hb | pa a g x Hr Hk Hx Hb | pa a bpath sp d sa x Hr Hin _ IH].
  - eapply A.FRec; eauto.
  - eapply A.FUnion; eauto.
  - apply A.FType; [rewrite (rs_RF w R2 F2 R1 F1); exact Hr | exact Hk |].
    intros (b0 & Hb0 & E). apply Hb. exists b0. split; [exact Hb0 | rewrite (rs_RF w R1 F1 R2 F2); exact E].
  - eapply A.FNom; [rewrite (rs_RF w R2 F2 R1 F1); exact Hr | exact Hk | exact Hx |].
    intros (b0 & Hb0 & E). apply Hb. exists b0. split; [exact Hb0 | rewrite (rs_RF w R1 F1 R2 F2); exact E].
  - eapply A.FBracket; [rewrite (rs_RF w R2 F2 R1 F1); exact Hr | exact Hin | exact IH].
Qed.

Lemma closed_RF : forall w R1 F1 R2 F2 R, A.RelevanceClosed (W_read w R1 F1) R -> A.RelevanceClosed (W_read w R2 F2) R.
Proof.
  intros w R1 F1 R2 F2 R H g d ps t Hd Hb. destruct (H g d ps t Hd Hb) as [C1 C2]. split.
  - intros n x Hn Hr. apply (C1 n x Hn). rewrite (rs_RF w R1 F1 R2 F2). exact Hr.
  - intros x Hx. apply C2. exact (FreeIn_RF w R2 F2 R1 F1 R _ t x Hx).
Qed.

Theorem reqs_reading : forall b ls e, LInv b ls ->
  let w := LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)) in
  forall syms c,
  (forall i g, nget (ls_syms ls) i = Some g -> nget syms i = Some g) ->
  (forall x l, nget c x = Some l -> NoDup l /\ exists g, nget syms x = Some g /\
     (kind_plain (gs_kind g) = true -> forall t, In t l <-> A.AssumeTypes (W0r w) x t) /\
     (gs_kind g = KType \/ gs_kind g = KUnit -> l = [])) ->
  RC (W0r w) c -> RL (W0r w) c ->
  (forall g d ps t n, A.w_decl (W0r w) g = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) -> In n ps ->
     exists x, A.resolve_sym (W0r w) (A.d_mod d) [n] = Some x /\ A.AssumeTypes (W0r w) g x) ->
  forall F, ReqsReading (W_read w (Rfin b ls e c) F).
Proof.
  intros b ls e HI w syms c Hk Hq Hrc Hrl Hl F. constructor.
  - exact (Rfin_nodup b ls e syms c Hq).
  - intros x k Hx H1 H2 H3 H4 t. rewrite (W0_assume w (Rfin b ls e c) F x t).
    exact (Rfin_plain b ls e HI syms c Hk Hq x k Hx H1 H2 H3 H4 t).
  - exact (Rfin_none b ls e HI syms c Hk Hq).
  - apply (closed_RF w (fun _ => []) (fun _ => 0)). exact (Rfin_closed b ls e HI syms c Hk Hq Hrc Hrl).
  - intros R Hc Ha g Hg. apply (Rfin_least b ls e HI syms c Hk Hq Hrl R); [exact (closed_RF w _ _ _ _ R Hc) | exact Ha | exact Hg].
  - intros g d ps t n Hd Hb Hn. destruct (Hl g d ps t n Hd Hb Hn) as (x & Hr & Ha). exists x. split.
    + rewrite (rs_RF w (Rfin b ls e c) F (fun _ => []) (fun _ => 0)). exact Hr.
    + apply (W0_assume w (Rfin b ls e c) F). exact Ha.
Qed.

(* ======================================================================== *)
(* 5. Every tag's listed parameters (Elab.check_listed, ExecutionModel note 40)       *)
(* ======================================================================== *)

Lemma miter_each : forall X (f : X -> M unit) l s u s',
  (forall x, In x l -> forall u s', f x s = Good (u, s') -> s' = s) ->
  miter f l s = Good (u, s') -> forall x, In x l -> exists u', f x s = Good (u', s).
Proof.
  intros X f l. induction l as [|x r IH]; intros s u s' Hr H y Hy; [destruct Hy|]. cbn [miter] in H.
  apply bind_inv in H as (u1 & s1 & H1 & H2). pose proof (Hr x (or_introl eq_refl) u1 s1 H1) as ->.
  destruct Hy as [<-|Hy]; [exists u1; exact H1|].
  exact (IH s u s' (fun z Hz => Hr z (or_intror Hz)) H2 y Hy).
Qed.

Lemma check_listed_ok : forall b ls e, LInv b ls ->
  let w := LoadWorld.World (ls_mods ls) (ls_order ls) e (ls_syms ls) (ls_next_sym ls) (ls_files ls) (Loader.library_dir (b_prelude b)) in
  forall s u s', GS b ls e s -> check_listed s = Good (u, s') ->
  forall g d ps t n, A.w_decl (W0r w) g = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) -> In n ps ->
  exists x, A.resolve_sym (W0r w) (A.d_mod d) [n] = Some x /\ A.AssumeTypes (W0r w) g x.
Proof.
  intros b ls e HI w s u s' Hg H g d ps t n Hd Hb Hn.
  unfold check_listed in H. apply bind_inv in H as (s0 & s1 & E1 & H). injection E1 as <- <-.
  destruct Hg as [Gw Gk]. pose proof (conj Gw Gk : GS b ls e s) as Hg. rewrite Gw in H. cbn [w_syms w_nsyms] in H.
  destruct (decl_loader b ls e g d Hd) as (g0 & Hg0).
  destruct (ldecl b ls e HI (fun _ => []) (fun _ => 0) g g0 Hg0) as (Dg & _).
  assert (Ed : A.w_decl (W0r w) g = Some (A.Decl (pathi b ls e (gs_mod g0)) (gs_decl g0) (gs_assumes g0))) by exact Dg.
  rewrite Ed in Hd. injection Hd as <-. cbn [A.d_ast A.d_mod] in Hb |- *.
  destruct (li_syms _ _ HI g g0 Hg0) as (_ & Hlt & Hid & _).
  assert (Hin : In g (seq 0 (ls_next_sym ls))) by (apply in_seq; lia).
  match type of H with miter ?f ?l ?s0 = _ =>
    assert (Hr : forall x, In x l -> forall u0 s2, f x s0 = Good (u0, s2) -> s2 = s0) end.
  { intros i _ u0 s2 H2. cbv beta in H2. destruct (nget (ls_syms ls) i) as [gi|]; [|injection H2 as _ <-; reflexivity].
    destruct (kind_eqb (gs_kind gi) KTag); [|injection H2 as _ <-; reflexivity].
    apply bind_inv in H2 as (l & s3 & H3 & H2). destruct (listed_spec b ls e HI (fun _ => []) (fun _ => 0) 0 gi s l s3 Hg H3) as [-> _].
    injection H2 as _ <-. reflexivity. }
  destruct (miter_each _ _ _ _ _ _ Hr H g Hin) as (u' & Hs). cbv beta in Hs.
  rewrite Hg0 in Hs. pose proof (li_kind _ _ HI g g0 Hg0) as Kd. unfold KindOK in Kd.
  destruct (gs_decl g0) as [| | |tn tps arity tt| | | | |] eqn:Dd; cbn in Hb; try discriminate; injection Hb as <- <-; [destruct Hn|].
  assert (Kt : gs_kind g0 = KTag) by (destruct (gs_kind g0); cbn in Kd; congruence).
  rewrite Kt in Hs. cbn [kind_eqb] in Hs. apply bind_inv in Hs as (lst & s3 & H3 & _).
  destruct (listed_spec b ls e HI (fun _ => []) (fun _ => 0) 0 g0 s lst s3 Hg H3) as [_ Hlok].
  rewrite Hid in Hlok. specialize (Hlok Hg0).
  assert (Hb' : A.nominal_body (A.d_ast (A.Decl (pathi b ls e (gs_mod g0)) (gs_decl g0) (gs_assumes g0))) = Some (tps, tt))
    by (cbn; rewrite Dd; reflexivity).
  destruct (Hlok _ tps tt Dg eq_refl) as [_ L2]. destruct (L2 n Hn) as (x & Hrs & _ & Ha). exists x. split; [exact Hrs | exact Ha].
Qed.
