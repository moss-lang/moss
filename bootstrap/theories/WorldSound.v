(* ------------------------------------------------------------------------ *)
(* WorldSound.v — the elaborator's static reading of a bundle is a reading  *)
(* in the sense of the DRAFT relation (Audit.v, Part V, V.1): the loader    *)
(* half of `WorldOf`.                                                        *)
(*                                                                           *)
(* `W_read w R F` is the Audit world the loader's `w` (Elab.load_world)     *)
(* denotes: its symbols, scopes, exports and attached-method table, with    *)
(* requirements R and field numbering F supplied from outside (the          *)
(* elaborator computes both lazily, during elaboration).  Proved here:      *)
(*                                                                           *)
(*   load_world_reads : load_world b = Good w -> LoaderReading b (W_read w R F) *)
(*                                                                           *)
(* where `LoaderReading` is exactly the fields of `WorldOf` that are about  *)
(* the loader (acyclic imports, symbols = declarations, D44 exports and     *)
(* scopes, D61 attached methods), and                                       *)
(*                                                                           *)
(*   worldof_of_reading : LoaderReading b W -> ReqsReading W -> injective   *)
(*                        (w_fld W) -> WorldOf b W                           *)
(*                                                                           *)
(* so what `WorldOf` still needs of the elaborator is its requirements      *)
(* (ReqsReading: type_requirements computes the least relevance-closed      *)
(* sets, D71/D73) and its field numbering (an injective extension of        *)
(* st_flds); see the section `world_todo` at the end.                       *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

(* The elaborator's fuels are decimal literals (Nat.of_num_uint ...); left
   transparent, the kernel computes them in unary while checking that a
   definition and its unfolding agree. *)
Local Strategy opaque [Nat.of_num_uint].

(* ======================================================================== *)
(* 1. Utilities                                                              *)
(* ======================================================================== *)

Lemma b2n_le1 : forall b, Nat.b2n b <= 1.
Proof. intros []; simpl; lia. Qed.

Lemma even_div2_inj : forall a b,
  is_even_nat a = is_even_nat b -> Nat.div2 a = Nat.div2 b -> a = b.
Proof.
  intros a b He Hd. unfold is_even_nat in He.
  pose proof (Nat.div2_odd a) as Ha. pose proof (Nat.div2_odd b) as Hb.
  assert (Ea : a - 2 * Nat.div2 a = Nat.b2n (Nat.odd a)) by lia.
  assert (Eb : b - 2 * Nat.div2 b = Nat.b2n (Nat.odd b)) by lia.
  rewrite Ea, Eb in He.
  destruct (Nat.odd a), (Nat.odd b); simpl in *; try discriminate; lia.
Qed.

Lemma nget_NE : forall A j, nget (@NE A) j = None.
Proof. intros A []; reflexivity. Qed.

Lemma nget_nset_go_other : forall A fuel (m : nmap A) k k' x,
  k' <> k -> nget (nset_go fuel m k x) k' = nget m k'.
Proof.
  intros A fuel. induction fuel as [|f IH]; intros m k k' x Hne; [reflexivity|].
  cbn [nset_go].
  assert (Hm : exists l v r, (match m with NE => (NE, None, NE) | NN l v r => (l, v, r) end) = (l, v, r)
                             /\ forall j, nget m j = nget (NN l v r) j).
  { destruct m as [|l v r].
    - exists NE, None, NE. split; [reflexivity|]. intros [|j]; [reflexivity|].
      cbn. destruct (is_even_nat j); reflexivity.
    - exists l, v, r. split; reflexivity. }
  destruct Hm as (l & v & r & -> & Hm). rewrite Hm.
  destruct k as [|k1], k' as [|k1']; try (exfalso; apply Hne; reflexivity); cbn [nget]; try reflexivity.
  - destruct (is_even_nat k1); reflexivity.
  - destruct (is_even_nat k1) eqn:E1, (is_even_nat k1') eqn:E2; cbn [nget]; rewrite ?E1, ?E2; try reflexivity;
      apply IH; intro Hd; apply Hne; f_equal; apply even_div2_inj; congruence.
Qed.

Lemma nget_nset_same : forall A (m : nmap A) k x, nget (nset m k x) k = Some x.
Proof.
  intros. unfold nset. apply nget_nset_go_same. apply Nat.pow_gt_lin_r. lia.
Qed.

Lemma nget_nset_other : forall A (m : nmap A) k k' x, k' <> k -> nget (nset m k x) k' = nget m k'.
Proof. intros. unfold nset. apply nget_nset_go_other. exact H. Qed.

Lemma nget_nset : forall A (m : nmap A) k k' x,
  nget (nset m k x) k' = if Nat.eqb k' k then Some x else nget m k'.
Proof.
  intros. destruct (Nat.eqb_spec k' k) as [->|Hne].
  - apply nget_nset_same.
  - apply nget_nset_other. exact Hne.
Qed.

Lemma assoc_app : forall K V (eqb : K -> K -> bool) (k : K) (l1 l2 : list (K * V)),
  assoc eqb k (l1 ++ l2) = match assoc eqb k l1 with Some v => Some v | None => assoc eqb k l2 end.
Proof.
  intros K V eqb k l1 l2. induction l1 as [|[k' v'] r IH]; simpl; [reflexivity|].
  destruct (eqb k k'); [reflexivity | exact IH].
Qed.

Lemma sassoc_app : forall V n (l1 l2 : list (string * V)),
  sassoc n (l1 ++ l2) = match sassoc n l1 with Some v => Some v | None => sassoc n l2 end.
Proof. intros. apply assoc_app. Qed.

Lemma sassoc_one : forall V n k (v : V), sassoc n [(k, v)] = if String.eqb n k then Some v else None.
Proof. intros. reflexivity. Qed.

Lemma sassoc_snoc_old : forall V n k (v x : V) l,
  sassoc n l = Some x -> sassoc n (l ++ [(k, v)]) = Some x.
Proof. intros V n k v x l H. rewrite sassoc_app, H. reflexivity. Qed.

Lemma sassoc_snoc_new : forall V n (v : V) l,
  sassoc n l = None -> sassoc n (l ++ [(n, v)]) = Some v.
Proof. intros V n v l H. rewrite sassoc_app, H, sassoc_one, String.eqb_refl. reflexivity. Qed.

Lemma sassoc_snoc_inv : forall V n k (v x : V) l,
  sassoc n (l ++ [(k, v)]) = Some x -> sassoc n l = Some x \/ (sassoc n l = None /\ n = k /\ x = v).
Proof.
  intros V n k v x l H. rewrite sassoc_app in H. destruct (sassoc n l) eqn:E; [left; exact H|].
  right. rewrite sassoc_one in H. destruct (String.eqb_spec n k); [|discriminate]. injection H as <-. auto.
Qed.

Lemma attached_eqb_spec : forall a b, attached_eqb a b = true <-> a = b.
Proof.
  intros [a1 a2] [b1 b2]. unfold attached_eqb. simpl. rewrite andb_true_iff, Nat.eqb_eq, String.eqb_eq.
  split; [intros [-> ->]; reflexivity | intro H; injection H as -> ->; auto].
Qed.

Lemma aassoc_snoc_inv : forall V k k' (v x : V) l,
  assoc attached_eqb k (l ++ [(k', v)]) = Some x ->
  assoc attached_eqb k l = Some x \/ (assoc attached_eqb k l = None /\ k = k' /\ x = v).
Proof.
  intros V k k' v x l H. rewrite assoc_app in H. destruct (assoc attached_eqb k l) eqn:E; [left; exact H|].
  right. simpl in H. destruct (attached_eqb k k') eqn:Ek; [|discriminate].
  apply attached_eqb_spec in Ek. injection H as <-. auto.
Qed.

Lemma aassoc_snoc_old : forall V k k' (v x : V) l,
  assoc attached_eqb k l = Some x -> assoc attached_eqb k (l ++ [(k', v)]) = Some x.
Proof. intros V k k' v x l H. rewrite assoc_app, H. reflexivity. Qed.

Lemma aassoc_snoc_new : forall V k (v : V) l,
  assoc attached_eqb k l = None -> assoc attached_eqb k (l ++ [(k, v)]) = Some v.
Proof.
  intros V k v l H. rewrite assoc_app, H. simpl.
  rewrite (proj2 (attached_eqb_spec k k) eq_refl). reflexivity.
Qed.

Lemma smem_false : forall x l, smem x l = false -> ~ In x l.
Proof.
  intros x l. induction l as [|y r IH]; simpl; [auto|].
  intros H [<-|Hr]; apply orb_false_iff in H as [H1 H2].
  - rewrite String.eqb_refl in H1. discriminate.
  - exact (IH H2 Hr).
Qed.

Lemma find_module_of : forall b p, find_module p (b_mods b) = A.module_of b p.
Proof.
  intros b p. unfold A.module_of. induction (b_mods b) as [|m r IH]; simpl; [reflexivity|].
  destruct (String.eqb (am_path m) p); [reflexivity | exact IH].
Qed.

(* ======================================================================== *)
(* 2. Declaring a module's symbols (Elab.declare)                            *)
(* ======================================================================== *)

Definition is_attached (d : adecl) : Prop :=
  exists rn n ps rt body, d = ADFn (RAttached rn) n ps rt body.

Lemma attached_not_filed : forall d, is_attached d -> A.filed_name d = None.
Proof. intros d (rn & n & ps & rt & body & ->). reflexivity. Qed.

(* A symbol's kind, as Part V reads it off its declaration. *)
Definition kconv (k : kind) : A.dkind :=
  match k with
  | KType => A.DKType | KUnit => A.DKUnit | KAlias => A.DKAlias | KTag => A.DKTag | KVal => A.DKVal
  | KFn => A.DKFn | KCtx => A.DKCtx | KMethod => A.DKMeth | KFunctor => A.DKFunctor
  end.

Definition KindOK (g : gsym) : Prop := A.kind_of (gs_decl g) = Some (kconv (gs_kind g)).

Section Declare.

Variable mi : nat.
Variable f : afile.
Variable path : string.
Variable syms0 : nmap gsym.
Variable next0 : nat.
Hypothesis Hold : forall s, next0 <= s -> nget syms0 s = None.

Definition Leaf (asm : list (Input.path * option name)) (d : adecl) : Prop := A.OccursIn (f_decls f) asm d.

Record DInv (st : decl_st) : Prop := {
  di_next : next0 <= ds_next st;
  di_old : forall s, s < next0 -> nget (ds_syms st) s = nget syms0 s;
  di_above : forall s, ds_next st <= s -> nget (ds_syms st) s = None;
  di_sym : forall s g, next0 <= s -> nget (ds_syms st) s = Some g ->
     gs_id g = s /\ gs_mod g = mi /\ Leaf (gs_assumes g) (gs_decl g) /\ A.kind_of (gs_decl g) <> None /\
     (forall n, A.filed_name (gs_decl g) = Some (A.NSName, n) -> sassoc n (gm_names (ds_mod st)) = Some s) /\
     (forall n, A.filed_name (gs_decl g) = Some (A.NSDetached, n) -> sassoc n (gm_detached (ds_mod st)) = Some s) /\
     (is_attached (gs_decl g) ->
        exists rn n ps rt body r gr, gs_decl g = ADFn (RAttached rn) n ps rt body /\
          sassoc rn (gm_names (ds_mod st)) = Some r /\ nget (ds_syms st) r = Some gr /\ next0 <= r /\
          gs_mod gr = mi /\ assoc attached_eqb (r, n) (gm_attached (ds_mod st)) = Some s);
  di_names : forall n s, sassoc n (gm_names (ds_mod st)) = Some s ->
     next0 <= s /\ exists g, nget (ds_syms st) s = Some g /\ A.filed_name (gs_decl g) = Some (A.NSName, n);
  di_detached : forall n s, sassoc n (gm_detached (ds_mod st)) = Some s ->
     next0 <= s /\ exists g, nget (ds_syms st) s = Some g /\ A.filed_name (gs_decl g) = Some (A.NSDetached, n);
  di_attached : forall r n s, assoc attached_eqb (r, n) (gm_attached (ds_mod st)) = Some s ->
     next0 <= s /\ exists g rn ps rt body, nget (ds_syms st) s = Some g /\
       gs_decl g = ADFn (RAttached rn) n ps rt body /\ sassoc rn (gm_names (ds_mod st)) = Some r;
  di_pending : forall d asm, In (d, asm) (ds_pending st) -> Leaf asm d /\ is_attached d;
  di_path : gm_path (ds_mod st) = path;
  di_aliases : gm_aliases (ds_mod st) = [];
  di_kind : forall s g, nget (ds_syms st) s = Some g -> next0 <= s -> KindOK g
}.

Definition HasSym (st : decl_st) (d : adecl) (asm : list (Input.path * option name)) : Prop :=
  exists s g, next0 <= s /\ nget (ds_syms st) s = Some g /\ gs_decl g = d /\ gs_assumes g = asm.

Definition Has (st : decl_st) d asm : Prop := HasSym st d asm \/ In (d, asm) (ds_pending st).

Lemma dinv_sym_lt : forall st s g, DInv st -> nget (ds_syms st) s = Some g -> s < ds_next st.
Proof.
  intros st s g H Hs. destruct (Nat.lt_ge_cases s (ds_next st)) as [|Hge]; [assumption|].
  rewrite (di_above _ H s Hge) in Hs. discriminate.
Qed.

(* A fresh symbol filed under a plain name. *)
Lemma fresh_name_spec : forall st n k d asm st1,
  define_name (DSt (ds_mod st) (nset (ds_syms st) (ds_next st) (GSym (ds_next st) mi n k d None asm OSource))
                   (S (ds_next st)) (ds_pending st)) n (ds_next st) = Good st1 ->
  A.filed_name d = Some (A.NSName, n) -> A.kind_of d <> None -> Leaf asm d ->
  A.kind_of d = Some (kconv k) ->
  DInv st -> DInv st1 /\ (forall d' a, Has st d' a -> Has st1 d' a) /\ HasSym st1 d asm.
Proof.
  intros st n k d asm st1 Hdef Hf Hk Hl Hkd H.
  unfold define_name in Hdef. cbn [ds_mod ds_syms ds_next ds_pending] in Hdef.
  destruct (sassoc n (gm_names (ds_mod st))) eqn:En; [discriminate|].
  injection Hdef as <-.
  set (id := ds_next st) in *.
  set (g := GSym id mi n k d None asm OSource).
  assert (Hid : forall s g', nget (ds_syms st) s = Some g' -> s <> id).
  { intros s g' Hs E. subst s. pose proof (dinv_sym_lt _ _ _ H Hs). unfold id in *. lia. }
  assert (Hget : forall s g', nget (ds_syms st) s = Some g' ->
            nget (nset (ds_syms st) id g) s = Some g').
  { intros s g' Hs. rewrite nget_nset_other; [exact Hs | exact (Hid _ _ Hs)]. }
  assert (Hnot : A.filed_name d = Some (A.NSName, n) -> ~ is_attached d).
  { intros E Ha. rewrite (attached_not_filed _ Ha) in E. discriminate. }
  split; [|split].
  - constructor; cbn [ds_mod ds_syms ds_next ds_pending gm_names gm_detached gm_attached gm_path gm_aliases].
    + pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [exact (di_old _ H s Hs)|]. pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [apply (di_above _ H); unfold id in *; lia | unfold id in *; lia].
    + intros s g' Hs Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec s id) as [->|Hne].
      * injection Hg as <-. unfold g. cbn [gs_id gs_mod gs_assumes gs_decl].
        split; [reflexivity|]. split; [reflexivity|]. split; [exact Hl|]. split; [exact Hk|].
        split; [|split; [|]].
        -- intros n' E. rewrite Hf in E. injection E as <-. apply sassoc_snoc_new. exact En.
        -- intros n' E. rewrite Hf in E. discriminate.
        -- intro Ha. exfalso. exact (Hnot Hf Ha).
      * destruct (di_sym _ H s g' Hs Hg) as (H1 & H2 & H3 & H4 & H5 & H6 & H7).
        split; [exact H1|]. split; [exact H2|]. split; [exact H3|]. split; [exact H4|].
        split; [intros n' E; apply sassoc_snoc_old; exact (H5 n' E)|].
        split; [exact H6|].
        intro Ha. destruct (H7 Ha) as (rn & n' & ps & rt & body & r & gr & E1 & E2 & E3 & E4 & E5 & E6).
        exists rn, n', ps, rt, body, r, gr. split; [exact E1|].
        split; [apply sassoc_snoc_old; exact E2|]. split; [apply Hget; exact E3|]. auto.
    + intros n' s Hs. apply sassoc_snoc_inv in Hs as [Hs|(_ & -> & ->)].
      * destruct (di_names _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
        exists g'. split; [apply Hget; exact Hg' | exact Hfg].
      * split; [exact (di_next _ H)|]. exists g. rewrite nget_nset_same. split; [reflexivity | exact Hf].
    + intros n' s Hs. destruct (di_detached _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
      exists g'. split; [apply Hget; exact Hg' | exact Hfg].
    + intros r n' s Hs. destruct (di_attached _ H r n' s Hs) as [Hle (g' & rn & ps & rt & body & Hg' & E1 & E2)].
      split; [exact Hle|]. exists g', rn, ps, rt, body. split; [apply Hget; exact Hg'|].
      split; [exact E1 | apply sassoc_snoc_old; exact E2].
    + exact (di_pending _ H).
    + exact (di_path _ H).
    + exact (di_aliases _ H).
    + intros s0 g0 Hg0 Hs0. rewrite nget_nset in Hg0. destruct (Nat.eqb_spec s0 id) as [->|Hne0].
      * injection Hg0 as <-. unfold KindOK, g. cbn [gs_decl gs_kind]. exact Hkd.
      * exact (di_kind _ H s0 g0 Hg0 Hs0).
  - intros d' a [(s & g' & Hle & Hs & E1 & E2)|Hp]; [left|right; exact Hp].
    exists s, g'. split; [exact Hle|]. split; [apply Hget; exact Hs|]. auto.
  - exists id, g. cbn [ds_syms]. rewrite nget_nset_same. split; [exact (di_next _ H)|]. auto.
Qed.

(* A fresh symbol filed under a detached name. *)
Lemma fresh_detached_spec : forall st n d asm,
  sassoc n (gm_detached (ds_mod st)) = None ->
  A.filed_name d = Some (A.NSDetached, n) -> A.kind_of d <> None -> Leaf asm d ->
  A.kind_of d = Some (kconv KMethod) ->
  DInv st ->
  let id := ds_next st in
  let st1 := DSt (ds_mod st) (nset (ds_syms st) id (GSym id mi ("." ++ n) KMethod d None asm OSource)) (S id) (ds_pending st) in
  let m1 := ds_mod st1 in
  let st2 := DSt (GMod (gm_path m1) (gm_names m1) (gm_detached m1 ++ [(n, id)])
                       (gm_attached m1) (gm_aliases m1) (gm_exp_names m1)
                       (gm_exp_detached m1) (gm_exp_aliases m1))
                 (ds_syms st1) (ds_next st1) (ds_pending st1) in
  DInv st2 /\ (forall d' a, Has st d' a -> Has st2 d' a) /\ HasSym st2 d asm.
Proof.
  intros st n d asm En Hf Hk Hl Hkd H id st1 m1 st2.
  subst st2 m1 st1. set (g := GSym id mi ("." ++ n) KMethod d None asm OSource).
  assert (Hid : forall s g', nget (ds_syms st) s = Some g' -> s <> id).
  { intros s g' Hs E. subst s. pose proof (dinv_sym_lt _ _ _ H Hs). unfold id in *. lia. }
  assert (Hget : forall s g', nget (ds_syms st) s = Some g' ->
            nget (nset (ds_syms st) id g) s = Some g').
  { intros s g' Hs. rewrite nget_nset_other; [exact Hs | exact (Hid _ _ Hs)]. }
  assert (Hnot : ~ is_attached d).
  { intro Ha. rewrite (attached_not_filed _ Ha) in Hf. discriminate. }
  split; [|split].
  - constructor; cbn [ds_mod ds_syms ds_next ds_pending gm_names gm_detached gm_attached gm_path gm_aliases].
    + pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [exact (di_old _ H s Hs)|]. pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [apply (di_above _ H); unfold id in *; lia | unfold id in *; lia].
    + intros s g' Hs Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec s id) as [->|Hne].
      * injection Hg as <-. unfold g. cbn [gs_id gs_mod gs_assumes gs_decl].
        split; [reflexivity|]. split; [reflexivity|]. split; [exact Hl|]. split; [exact Hk|].
        split; [|split; [|]].
        -- intros n' E. rewrite Hf in E. discriminate.
        -- intros n' E. rewrite Hf in E. injection E as <-. apply sassoc_snoc_new. exact En.
        -- intro Ha. exfalso. exact (Hnot Ha).
      * destruct (di_sym _ H s g' Hs Hg) as (H1 & H2 & H3 & H4 & H5 & H6 & H7).
        split; [exact H1|]. split; [exact H2|]. split; [exact H3|]. split; [exact H4|].
        split; [exact H5|].
        split; [intros n' E; apply sassoc_snoc_old; exact (H6 n' E)|].
        intro Ha. destruct (H7 Ha) as (rn & n' & ps & rt & body & r & gr & E1 & E2 & E3 & E4 & E5 & E6).
        exists rn, n', ps, rt, body, r, gr. split; [exact E1|].
        split; [exact E2|]. split; [apply Hget; exact E3|]. auto.
    + intros n' s Hs. destruct (di_names _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
      exists g'. split; [apply Hget; exact Hg' | exact Hfg].
    + intros n' s Hs. apply sassoc_snoc_inv in Hs as [Hs|(_ & -> & ->)].
      * destruct (di_detached _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
        exists g'. split; [apply Hget; exact Hg' | exact Hfg].
      * split; [exact (di_next _ H)|]. exists g. rewrite nget_nset_same. split; [reflexivity | exact Hf].
    + intros r n' s Hs. destruct (di_attached _ H r n' s Hs) as [Hle (g' & rn & ps & rt & body & Hg' & E1 & E2)].
      split; [exact Hle|]. exists g', rn, ps, rt, body. split; [apply Hget; exact Hg'|].
      split; [exact E1 | exact E2].
    + exact (di_pending _ H).
    + exact (di_path _ H).
    + exact (di_aliases _ H).
    + intros s0 g0 Hg0 Hs0. rewrite nget_nset in Hg0. destruct (Nat.eqb_spec s0 id) as [->|Hne0].
      * injection Hg0 as <-. unfold KindOK, g. cbn [gs_decl gs_kind]. exact Hkd.
      * exact (di_kind _ H s0 g0 Hg0 Hs0).
  - intros d' a [(s & g' & Hle & Hs & E1 & E2)|Hp]; [left|right; exact Hp].
    exists s, g'. split; [exact Hle|]. split; [apply Hget; exact Hs|]. auto.
  - exists id, g. cbn [ds_syms]. rewrite nget_nset_same. split; [exact (di_next _ H)|]. auto.
Qed.

Lemma dinv_pending : forall st l, DInv st ->
  (forall d a, In (d, a) l -> Leaf a d /\ is_attached d) ->
  DInv (DSt (ds_mod st) (ds_syms st) (ds_next st) l).
Proof.
  intros st l H Hl. destruct H. constructor; cbn [ds_mod ds_syms ds_next ds_pending]; assumption.
Qed.

Lemma occ_cons : forall d rest a d', A.OccursIn rest a d' -> A.OccursIn (d :: rest) a d'.
Proof.
  intros d rest a d' H. inversion H as [ds0 d0 Hin Hna | ds0 its inner asm0 d0 Hin Hocc]; subst.
  - apply A.OccHere; [right; exact Hin | exact Hna].
  - eapply A.OccNested; [right; exact Hin | exact Hocc].
Qed.

Lemma occ_cons_inv : forall d rest a d', A.OccursIn (d :: rest) a d' ->
  A.OccursIn rest a d' \/
  ((d' = d /\ a = [] /\ forall its inner, d <> ADAssume its inner) \/
   (exists its inner a', d = ADAssume its inner /\ A.OccursIn inner a' d' /\ a = its ++ a')).
Proof.
  intros d rest a d' H. inversion H as [ds0 d0 Hin Hna | ds0 its inner asm0 d0 Hin Hocc]; subst.
  - destruct Hin as [<-|Hin]; [right; left; auto | left; apply A.OccHere; assumption].
  - destruct Hin as [E|Hin]; [right; right; exists its, inner, asm0; auto | left; eapply A.OccNested; eassumption].
Qed.

Lemma rbind_good : forall A B (r : res A) (k : A -> res B) b,
  rbind r k = Good b -> exists a, r = Good a /\ k a = Good b.
Proof. intros A B r k b H. destruct r as [a|m]; [exists a; auto | discriminate]. Qed.

Lemma declare_walk_spec : forall fuel st asm ds st',
  declare_walk fuel mi st asm ds = Good st' ->
  (forall asm' d, A.OccursIn ds asm' d -> Leaf (asm ++ asm') d) ->
  DInv st ->
  DInv st' /\ (forall d a, Has st d a -> Has st' d a) /\
  (forall asm' d, A.OccursIn ds asm' d -> A.kind_of d <> None -> Has st' d (asm ++ asm')).
Proof.
  induction fuel as [|fuel IH]; intros st asm ds st' Hw Hl H; [discriminate|].
  destruct ds as [|d rest].
  - injection Hw as <-. split; [exact H|]. split; [auto|].
    intros asm' d0 Ho. inversion Ho; subst; simpl in *; contradiction.
  - cbn [declare_walk] in Hw. apply rbind_good in Hw as (st1 & Hd & Hrest).
    assert (HL0 : (forall its inner, d <> ADAssume its inner) -> Leaf asm d).
    { intro Hna. rewrite <- (app_nil_r asm). apply Hl. apply A.OccHere; [left; reflexivity | exact Hna]. }
    assert (Hstep : DInv st1 /\ (forall d0 a, Has st d0 a -> Has st1 d0 a) /\
       (forall asm' d', ((d' = d /\ asm' = [] /\ forall its inner, d <> ADAssume its inner) \/
                         (exists its inner a', d = ADAssume its inner /\ A.OccursIn inner a' d' /\ asm' = its ++ a')) ->
                        A.kind_of d' <> None -> Has st1 d' (asm ++ asm'))).
    { assert (Hnamed : forall n k, A.filed_name d = Some (A.NSName, n) -> A.kind_of d <> None ->
                (forall its inner, d <> ADAssume its inner) ->
                define_name (DSt (ds_mod st) (nset (ds_syms st) (ds_next st) (GSym (ds_next st) mi n k d None asm OSource))
                                 (S (ds_next st)) (ds_pending st)) n (ds_next st) = Good st1 ->
                A.kind_of d = Some (kconv k) ->
                DInv st1 /\ (forall d0 a, Has st d0 a -> Has st1 d0 a) /\
                (forall asm' d', ((d' = d /\ asm' = [] /\ forall its inner, d <> ADAssume its inner) \/
                                  (exists its inner a', d = ADAssume its inner /\ A.OccursIn inner a' d' /\ asm' = its ++ a')) ->
                                 A.kind_of d' <> None -> Has st1 d' (asm ++ asm'))).
      { intros n k Hf Hk Hna Hdef Hkd.
        destruct (fresh_name_spec st n k d asm st1 Hdef Hf Hk (HL0 Hna) Hkd H) as (D1 & D2 & D3).
        split; [exact D1|]. split; [exact D2|].
        intros asm' d' [(-> & -> & _)|(its & inner & a' & E & _)] _.
        - rewrite app_nil_r. left. exact D3.
        - exfalso. exact (Hna its inner E). }
      destruct d as [its inner|n|n t|n ps t|n|n t i|[| |rn] n ps rt body|n its|n ps a r b];
        cbv beta iota zeta in Hd; try unfold fresh_sym in Hd; cbv beta iota zeta in Hd;
        try (apply (Hnamed _ _ eq_refl ltac:(discriminate) ltac:(discriminate) Hd eq_refl)).
      - (* assume *)
        destruct (IH st (asm ++ its) inner st1 Hd) as (D1 & D2 & D3).
        + intros a d' Ho. rewrite <- app_assoc. apply Hl. eapply A.OccNested; [left; reflexivity | exact Ho].
        + exact H.
        + split; [exact D1|]. split; [exact D2|].
          intros asm' d' [(_ & _ & Hna)|(its' & inner' & a' & E & Ho & ->)] Hk.
          * exfalso. exact (Hna its inner eq_refl).
          * injection E as <- <-. rewrite app_assoc. exact (D3 a' d' Ho Hk).
      - (* detached method *)
        destruct body as [body|]; [unfold err_in in Hd; discriminate|].
        destruct (sassoc n (gm_detached (ds_mod st))) eqn:En; [unfold err_in in Hd; discriminate|].
        injection Hd as <-.
        assert (Hna : forall its inner, ADFn RDetached n ps rt None <> ADAssume its inner) by discriminate.
        destruct (fresh_detached_spec st n (ADFn RDetached n ps rt None) asm En eq_refl ltac:(discriminate) (HL0 Hna) eq_refl H)
          as (D1 & D2 & D3).
        split; [exact D1|]. split; [exact D2|].
        intros asm' d' [(-> & -> & _)|(its & inner & a' & E & _)] _.
        + rewrite app_nil_r. left. exact D3.
        + discriminate E.
      - (* attached method: pending *)
        injection Hd as <-.
        assert (Hna : forall its inner, ADFn (RAttached rn) n ps rt body <> ADAssume its inner) by discriminate.
        split; [|split].
        + apply dinv_pending; [exact H|]. intros d0 a Hin. apply in_app_or in Hin as [Hin|[E|[]]].
          * exact (di_pending _ H d0 a Hin).
          * injection E as <- <-. split; [exact (HL0 Hna) | exists rn, n, ps, rt, body; reflexivity].
        + intros d0 a [Hs|Hp]; [left; exact Hs | right; cbn [ds_pending]; apply in_or_app; left; exact Hp].
        + intros asm' d' [(-> & -> & _)|(its & inner & a' & E & _)] _.
          * rewrite app_nil_r. right. cbn [ds_pending]. apply in_or_app. right. left. reflexivity.
          * discriminate E. }
    destruct Hstep as (H1 & P1 & C1).
    destruct (IH st1 asm rest st' Hrest (fun a d' Ho => Hl a d' (occ_cons _ _ _ _ Ho)) H1) as (H2 & P2 & C2).
    split; [exact H2|]. split; [intros; apply P2, P1; assumption|].
    intros asm' d' Ho Hk. destruct (occ_cons_inv _ _ _ _ Ho) as [Hr|Hh].
    + apply C2; assumption.
    + apply P2. apply C1; assumption.
Qed.

Lemma fresh_attached_spec : forall st rn n ps rt body asm rid,
  sassoc rn (gm_names (ds_mod st)) = Some rid ->
  assoc attached_eqb (rid, n) (gm_attached (ds_mod st)) = None ->
  Leaf asm (ADFn (RAttached rn) n ps rt body) ->
  DInv st ->
  let d := ADFn (RAttached rn) n ps rt body in
  let id := ds_next st in
  let st1 := DSt (ds_mod st) (nset (ds_syms st) id (GSym id mi (rn ++ "." ++ n) KMethod d (Some rid) asm OSource)) (S id) (ds_pending st) in
  let m1 := ds_mod st1 in
  let st2 := DSt (GMod (gm_path m1) (gm_names m1) (gm_detached m1)
                       (gm_attached m1 ++ [((rid, n), id)])
                       (gm_aliases m1) (gm_exp_names m1)
                       (gm_exp_detached m1) (gm_exp_aliases m1))
                 (ds_syms st1) (ds_next st1) (ds_pending st1) in
  DInv st2 /\ (forall d' a, HasSym st d' a -> HasSym st2 d' a) /\ HasSym st2 d asm /\
  ds_pending st2 = ds_pending st.
Proof.
  intros st rn n ps rt body asm rid Hr Ha Hl H d id st1 m1 st2.
  subst st2 m1 st1. set (g := GSym id mi (rn ++ "." ++ n) KMethod d (Some rid) asm OSource).
  assert (Hid : forall s g', nget (ds_syms st) s = Some g' -> s <> id).
  { intros s g' Hs E. subst s. pose proof (dinv_sym_lt _ _ _ H Hs). unfold id in *. lia. }
  assert (Hget : forall s g', nget (ds_syms st) s = Some g' ->
            nget (nset (ds_syms st) id g) s = Some g').
  { intros s g' Hs. rewrite nget_nset_other; [exact Hs | exact (Hid _ _ Hs)]. }
  destruct (di_names _ H rn rid Hr) as [Hrle (gr & Hgr & _)].
  destruct (di_sym _ H rid gr Hrle Hgr) as (_ & Hgrm & _).
  split; [|split; [|split]].
  - constructor; cbn [ds_mod ds_syms ds_next ds_pending gm_names gm_detached gm_attached gm_path gm_aliases].
    + pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [exact (di_old _ H s Hs)|]. pose proof (di_next _ H). unfold id. lia.
    + intros s Hs. rewrite nget_nset_other; [apply (di_above _ H); unfold id in *; lia | unfold id in *; lia].
    + intros s g' Hs Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec s id) as [->|Hne].
      * injection Hg as <-. unfold g. cbn [gs_id gs_mod gs_assumes gs_decl].
        split; [reflexivity|]. split; [reflexivity|]. split; [exact Hl|]. split; [unfold d; discriminate|].
        split; [intros n' E; discriminate E|]. split; [intros n' E; discriminate E|].
        intros _. exists rn, n, ps, rt, body, rid, gr. split; [reflexivity|]. split; [exact Hr|].
        split; [apply Hget; exact Hgr|]. split; [exact Hrle|]. split; [exact Hgrm|].
        apply aassoc_snoc_new. exact Ha.
      * destruct (di_sym _ H s g' Hs Hg) as (H1 & H2 & H3 & H4 & H5 & H6 & H7).
        split; [exact H1|]. split; [exact H2|]. split; [exact H3|]. split; [exact H4|].
        split; [exact H5|]. split; [exact H6|].
        intro Hat. destruct (H7 Hat) as (rn' & n' & ps' & rt' & body' & r & gr' & E1 & E2 & E3 & E4 & E5 & E6).
        exists rn', n', ps', rt', body', r, gr'. split; [exact E1|].
        split; [exact E2|]. split; [apply Hget; exact E3|]. split; [exact E4|]. split; [exact E5|].
        apply aassoc_snoc_old. exact E6.
    + intros n' s Hs. destruct (di_names _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
      exists g'. split; [apply Hget; exact Hg' | exact Hfg].
    + intros n' s Hs. destruct (di_detached _ H n' s Hs) as [Hle (g' & Hg' & Hfg)]. split; [exact Hle|].
      exists g'. split; [apply Hget; exact Hg' | exact Hfg].
    + intros r n' s Hs. apply aassoc_snoc_inv in Hs as [Hs|(_ & E & ->)].
      * destruct (di_attached _ H r n' s Hs) as [Hle (g' & rn' & ps' & rt' & body' & Hg' & E1 & E2)].
        split; [exact Hle|]. exists g', rn', ps', rt', body'. split; [apply Hget; exact Hg'|].
        split; [exact E1 | exact E2].
      * injection E as -> ->. split; [exact (di_next _ H)|].
        exists g, rn, ps, rt, body. rewrite nget_nset_same. split; [reflexivity|]. split; [reflexivity | exact Hr].
    + exact (di_pending _ H).
    + exact (di_path _ H).
    + exact (di_aliases _ H).
    + intros s0 g0 Hg0 Hs0. rewrite nget_nset in Hg0. destruct (Nat.eqb_spec s0 id) as [->|Hne0].
      * injection Hg0 as <-. unfold KindOK, g. cbn [gs_decl gs_kind]. reflexivity.
      * exact (di_kind _ H s0 g0 Hg0 Hs0).
  - intros d' a (s & g' & Hle & Hs & E1 & E2).
    exists s, g'. split; [exact Hle|]. split; [apply Hget; exact Hs|]. auto.
  - exists id, g. cbn [ds_syms]. rewrite nget_nset_same. split; [exact (di_next _ H)|]. auto.
  - reflexivity.
Qed.

Lemma declare_attached_spec : forall ps st st',
  declare_attached mi st ps = Good st' ->
  (forall d a, In (d, a) ps -> Leaf a d /\ is_attached d) ->
  DInv st ->
  DInv st' /\ (forall d a, HasSym st d a -> HasSym st' d a) /\ ds_pending st' = ds_pending st /\
  (forall d a, In (d, a) ps -> HasSym st' d a).
Proof.
  induction ps as [|[d asm] rest IH]; intros st st' Hw Hl H.
  - injection Hw as <-. split; [exact H|]. split; [auto|]. split; [reflexivity|]. intros d a [].
  - cbn [declare_attached] in Hw. apply rbind_good in Hw as (st1 & Hd & Hrest).
    destruct (Hl d asm (or_introl eq_refl)) as [Hl0 (rn & n & ps0 & rt & body & ->)].
    cbv beta iota zeta in Hd.
    destruct (sassoc rn (gm_names (ds_mod st))) as [rid|] eqn:Er; [|unfold err_in in Hd; discriminate].
    destruct (assoc attached_eqb (rid, n) (gm_attached (ds_mod st))) eqn:Ea; [unfold err_in in Hd; discriminate|].
    unfold fresh_sym in Hd. cbv beta iota zeta in Hd. injection Hd as <-.
    destruct (fresh_attached_spec st rn n ps0 rt body asm rid Er Ea Hl0 H) as (D1 & D2 & D3 & D4).
    destruct (IH _ st' Hrest (fun d a Hin => Hl d a (or_intror Hin)) D1) as (E1 & E2 & E3 & E4).
    split; [exact E1|]. split; [intros; apply E2, D2; assumption|].
    split; [rewrite E3; exact D4|].
    intros d a [E|Hin]; [injection E as <- <-; apply E2; exact D3 | exact (E4 d a Hin)].
Qed.

(* What `declare` makes of a file: DInv for the module it returns, with no
   pending item, and a symbol for every declaration of the file. *)
Lemma declare_spec : forall m0 syms' next',
  declare mi path f syms0 next0 = Good (m0, syms', next') ->
  DInv (DSt m0 syms' next' []) /\
  gm_exp_names m0 = gm_names m0 /\ gm_exp_detached m0 = gm_detached m0 /\ gm_exp_aliases m0 = [] /\
  (forall asm d, Leaf asm d -> A.kind_of d <> None -> HasSym (DSt m0 syms' next' []) d asm).
Proof.
  intros m0 syms' next' Hd. unfold declare in Hd.
  revert Hd. generalize 100000. intros fuel Hd.
  apply rbind_good in Hd as (st1 & Hw & Hd).
  apply rbind_good in Hd as (st2 & Ha & Hd).
  injection Hd as <- <- <-.
  assert (H0 : DInv (DSt (GMod path [] [] [] [] [] [] []) syms0 next0 [])).
  { constructor; cbn [ds_mod ds_syms ds_next ds_pending gm_names gm_detached gm_attached gm_path gm_aliases].
    - lia.
    - reflexivity.
    - exact Hold.
    - intros s g Hs Hg. rewrite (Hold s Hs) in Hg. discriminate.
    - intros n s E. discriminate E.
    - intros n s E. discriminate E.
    - intros r n s E. discriminate E.
    - intros d a [].
    - reflexivity.
    - reflexivity.
    - intros s g Hg Hs. rewrite (Hold s Hs) in Hg. discriminate. }
  destruct (declare_walk_spec _ _ _ _ _ Hw (fun a d Ho => Ho) H0) as (W1 & _ & W3).
  assert (W1' : DInv (DSt (ds_mod st1) (ds_syms st1) (ds_next st1) [])) by (apply dinv_pending; [exact W1 | intros d a []]).
  destruct (declare_attached_spec _ _ _ Ha (di_pending _ W1) W1') as (A1 & A2 & A3 & A4).
  assert (HI : DInv (DSt (GMod (gm_path (ds_mod st2)) (gm_names (ds_mod st2)) (gm_detached (ds_mod st2))
                            (gm_attached (ds_mod st2)) (gm_aliases (ds_mod st2)) (gm_names (ds_mod st2))
                            (gm_detached (ds_mod st2)) []) (ds_syms st2) (ds_next st2) [])).
  { destruct A1. constructor; cbn [ds_mod ds_syms ds_next ds_pending gm_names gm_detached gm_attached gm_path gm_aliases] in *;
      try assumption.
    - intros d a []. }
  split; [exact HI|]. split; [reflexivity|]. split; [reflexivity|]. split; [reflexivity|].
  intros asm d Hl Hk. destruct (W3 asm d Hl Hk) as [Hs|Hp].
  - destruct (A2 d asm Hs) as (s & g & E1 & E2 & E3 & E4). exists s, g. auto.
  - destruct (A4 d asm Hp) as (s & g & E1 & E2 & E3 & E4). exists s, g. auto.
Qed.

End Declare.

(* ======================================================================== *)
(* 3. Linking a module's imports (Elab.link)                                 *)
(* ======================================================================== *)

Lemma sassoc_aset : forall V (t : list (string * V)) n s k,
  sassoc k (aset String.eqb n s t) = if String.eqb k n then Some s else sassoc k t.
Proof.
  intros V t n s k. induction t as [|[k' v'] r IH]; cbn [aset].
  - reflexivity.
  - destruct (String.eqb_spec n k') as [<-|Hne].
    + unfold sassoc. cbn [assoc]. destruct (String.eqb k n); reflexivity.
    + unfold sassoc in *. cbn [assoc]. rewrite IH.
      destruct (String.eqb_spec k k') as [->|Hk]; destruct (String.eqb_spec k' n) as [E|_];
        try (exfalso; apply Hne; symmetry; exact E); try reflexivity.
      all: destruct (String.eqb k n); reflexivity.
Qed.

Lemma sassoc_fun : forall V k (t : list (string * V)) x y, sassoc k t = Some x -> sassoc k t = Some y -> x = y.
Proof. intros. congruence. Qed.

Lemma bind_name_spec : forall mm t n s t', bind_name mm t n s = Good t' ->
  forall k x, sassoc k t' = Some x <-> sassoc k t = Some x \/ (k = n /\ x = s).
Proof.
  intros mm t n s t' H k x. unfold bind_name in H.
  destruct (sassoc n t) as [s'|] eqn:E.
  - destruct (Nat.eqb_spec s s') as [<-|]; [|unfold err_in in H; discriminate].
    injection H as <-. split; [auto|]. intros [H|[-> ->]]; [exact H | exact E].
  - injection H as <-. split.
    + intro H. apply sassoc_snoc_inv in H as [H|(_ & -> & ->)]; auto.
    + intros [H|[-> ->]]; [apply sassoc_snoc_old; exact H | apply sassoc_snoc_new; exact E].
Qed.

Lemma bind_all_spec : forall mm xs t t', bind_all mm t xs = Good t' ->
  forall k x, sassoc k t' = Some x <-> sassoc k t = Some x \/ sassoc k xs = Some x.
Proof.
  intros mm xs. induction xs as [|[n s] r IH]; intros t t' H k x.
  - injection H as <-. split; [auto|]. intros [H|H]; [exact H | discriminate H].
  - cbn [bind_all] in H. apply rbind_good in H as (t1 & H1 & H2).
    pose proof (bind_name_spec _ _ _ _ _ H1) as S1. pose proof (IH _ _ H2) as S2.
    assert (Hns : sassoc n t' = Some s) by (apply S2; left; apply S1; right; auto).
    unfold sassoc at 3. cbn [assoc]. fold (@sassoc nat).
    rewrite S2, S1. destruct (String.eqb_spec k n) as [->|Hne]; split.
    + intros [[H|[_ ->]]|H]; auto.
      right. f_equal. apply (sassoc_fun _ n t'); [exact Hns | apply S2; right; exact H].
    + intros [H|H]; [left; left; exact H | injection H as <-; left; right; auto].
    + intros [[H|[E _]]|H]; [auto | contradiction | auto].
    + intros [H|H]; [left; left; exact H | right; exact H].
Qed.

Definition tbl (m : gmod) (exp : bool) (ns : A.nspace) : list (name * nat) :=
  match ns with
  | A.NSName => if exp then gm_exp_names m else gm_names m
  | A.NSDetached => if exp then gm_exp_detached m else gm_detached m
  | A.NSAlias => if exp then gm_exp_aliases m else gm_aliases m
  end.

Definition Rel := A.nspace -> name -> nat -> Prop.

Record Rep (m : gmod) (RE RS : Rel) : Prop := {
  rep_exp : forall ns n x, sassoc n (tbl m true ns) = Some x <-> RE ns n x;
  rep_scope : forall ns n x, sassoc n (tbl m false ns) = Some x <-> RS ns n x;
  rep_sub : forall ns n x, RE ns n x -> RS ns n x }.

Lemma rep_ext : forall m RE RS RE' RS', Rep m RE RS ->
  (forall ns n x, RE ns n x <-> RE' ns n x) -> (forall ns n x, RS ns n x <-> RS' ns n x) -> Rep m RE' RS'.
Proof.
  intros m RE RS RE' RS' [H1 H2 H3] E1 E2. constructor.
  - intros. rewrite H1. apply E1.
  - intros. rewrite H2. apply E2.
  - intros ns n x H. apply E2, H3, E1, H.
Qed.

Definition nspace_eq_dec : forall a b : A.nspace, {a = b} + {a <> b}.
Proof. decide equality. Defined.

Definition add1 (R : Rel) ns0 n0 x0 : Rel := fun ns n x => R ns n x \/ (ns = ns0 /\ n = n0 /\ x = x0).

Lemma rep_add1 : forall m m' RE RS ns0 n0 x0 mm,
  Rep m RE RS ->
  bind_name mm (tbl m false ns0) n0 x0 = Good (tbl m' false ns0) ->
  tbl m' true ns0 = aset String.eqb n0 x0 (tbl m true ns0) ->
  (forall ns b, ns <> ns0 -> tbl m' b ns = tbl m b ns) ->
  Rep m' (add1 RE ns0 n0 x0) (add1 RS ns0 n0 x0).
Proof.
  intros m m' RE RS ns0 n0 x0 mm R Hb He Ho.
  pose proof (bind_name_spec _ _ _ _ _ Hb) as Sb.
  assert (Hcons : forall x', sassoc n0 (tbl m true ns0) = Some x' -> x' = x0).
  { intros x' Hx. assert (Hs : sassoc n0 (tbl m false ns0) = Some x') by (apply (rep_scope _ _ _ R), (rep_sub _ _ _ R), (rep_exp _ _ _ R), Hx).
    apply (sassoc_fun _ n0 (tbl m' false ns0)); [apply Sb; left; exact Hs | apply Sb; right; auto]. }
  constructor.
  - intros ns n x. unfold add1. destruct (nspace_eq_dec ns ns0) as [->|Hne].
    + rewrite He, sassoc_aset, <- (rep_exp _ _ _ R). destruct (String.eqb_spec n n0) as [->|Hn].
      * split; [intro H; injection H as <-; right; auto|].
        intros [H|(_ & _ & ->)]; [f_equal; symmetry; exact (Hcons _ H) | reflexivity].
      * split; [auto|]. intros [H|(_ & E & _)]; [exact H | contradiction].
    + rewrite (Ho ns true Hne), <- (rep_exp _ _ _ R). split; [auto|].
      intros [H|(E & _)]; [exact H | contradiction].
  - intros ns n x. unfold add1. destruct (nspace_eq_dec ns ns0) as [->|Hne].
    + rewrite Sb, (rep_scope _ _ _ R). split.
      * intros [H|(-> & ->)]; auto.
      * intros [H|(_ & -> & ->)]; auto.
    + rewrite (Ho ns false Hne), <- (rep_scope _ _ _ R). split; [auto|].
      intros [H|(E & _)]; [exact H | contradiction].
  - intros ns n x [H|H]; [left; apply (rep_sub _ _ _ R); exact H | right; exact H].
Qed.

Definition local_of (u : ause) : name := match u_alias u with Some (_, a) => a | None => u_name u end.

Definition UseRaw (tm : gmod) (u : ause) (ns : A.nspace) (x : nat) : Prop :=
  (u_dot u = true /\ ns = A.NSDetached /\ sassoc (u_name u) (gm_exp_detached tm) = Some x) \/
  (u_dot u = false /\ ns = A.NSName /\ sassoc (u_name u) (gm_exp_names tm) = Some x) \/
  (u_dot u = false /\ ns = A.NSAlias /\ sassoc (u_name u) (gm_exp_names tm) = None /\
   sassoc (u_name u) (gm_exp_aliases tm) = Some x).

Definition addU tm us (R : Rel) : Rel :=
  fun ns n x => R ns n x \/ exists u, In u us /\ local_of u = n /\ UseRaw tm u ns x.

Lemma link_uses_spec : forall us m tm ti m' RE RS, link_uses m tm ti us = Good m' -> Rep m RE RS ->
  Rep m' (addU tm us RE) (addU tm us RS) /\ gm_path m' = gm_path m /\ gm_attached m' = gm_attached m.
Proof.
  induction us as [|u rest IH]; intros m tm ti m' RE RS H R.
  - injection H as <-. split; [|auto]. eapply rep_ext; [exact R| |];
      intros ns n x; unfold addU; split; auto; intros [H|(u & [] & _)]; exact H.
  - cbn [link_uses] in H. apply rbind_good in H as (m1 & H1 & H2).
    assert (Hstep : exists ns0 x0, Rep m1 (add1 RE ns0 (local_of u) x0) (add1 RS ns0 (local_of u) x0) /\
                      (forall ns x, UseRaw tm u ns x <-> ns = ns0 /\ x = x0) /\
                      gm_path m1 = gm_path m /\ gm_attached m1 = gm_attached m).
    { unfold local_of. destruct (u_dot u) eqn:Ed.
      - destruct (sassoc (u_name u) (gm_exp_detached tm)) as [s|] eqn:Es; [|unfold err_in in H1; discriminate].
        apply rbind_good in H1 as (dt & Hb & H1). injection H1 as <-.
        exists A.NSDetached, s. split; [|split; [|auto]].
        + eapply rep_add1; [exact R | exact Hb | reflexivity |]. intros [] b Hne; try reflexivity; contradiction.
        + intros ns x. unfold UseRaw. rewrite Ed. split.
          * intros [(_ & -> & E)|[(E & _)|(E & _)]]; try discriminate. split; congruence.
          * intros [-> ->]. left. auto.
      - destruct (sassoc (u_name u) (gm_exp_names tm)) as [s|] eqn:Es.
        + apply rbind_good in H1 as (nm & Hb & H1). injection H1 as <-.
          exists A.NSName, s. split; [|split; [|auto]].
          * eapply rep_add1; [exact R | exact Hb | reflexivity |]. intros [] b Hne; try reflexivity; contradiction.
          * intros ns x. unfold UseRaw. rewrite Ed, Es. split.
            -- intros [(E & _)|[(_ & -> & E)|(_ & _ & E & _)]]; try discriminate. split; congruence.
            -- intros [-> ->]. right. left. auto.
        + destruct (sassoc (u_name u) (gm_exp_aliases tm)) as [s|] eqn:Ea; [|unfold err_in in H1; discriminate].
          apply rbind_good in H1 as (al & Hb & H1). injection H1 as <-.
          exists A.NSAlias, s. split; [|split; [|auto]].
          * eapply rep_add1; [exact R | exact Hb | reflexivity |]. intros [] b Hne; try reflexivity; contradiction.
          * intros ns x. unfold UseRaw. rewrite Ed, Es, Ea. split.
            -- intros [(E & _)|[(_ & _ & E)|(_ & -> & _ & E)]]; try discriminate. split; congruence.
            -- intros [-> ->]. right. right. auto. }
    destruct Hstep as (ns0 & x0 & R1 & Hu & P1 & A1).
    destruct (IH _ _ _ _ _ _ H2 R1) as (R2 & P2 & A2).
    split; [|split; congruence].
    assert (Heq : forall (R0 : Rel) ns n x, addU tm rest (add1 R0 ns0 (local_of u) x0) ns n x <-> addU tm (u :: rest) R0 ns n x).
    { intros R0 ns n x. unfold addU, add1. split.
      - intros [[H|(-> & -> & ->)]|(u' & Hin & E & Hr)].
        + left. exact H.
        + right. exists u. split; [left; reflexivity|]. split; [reflexivity|]. apply Hu. auto.
        + right. exists u'. split; [right; exact Hin|]. auto.
      - intros [H|(u' & [<-|Hin] & E & Hr)].
        + left. left. exact H.
        + left. right. apply Hu in Hr as [-> ->]. auto.
        + right. exists u'. auto. }
    eapply rep_ext; [exact R2 | |]; intros; apply Heq.
Qed.

Definition ImpRaw (tm : gmod) (ti : nat) (imp : aimport) : Rel := fun ns n x =>
  (i_alias imp = Some n /\ ns = A.NSAlias /\ x = ti) \/
  (i_glob imp = false /\ exists u, In u (i_uses imp) /\ local_of u = n /\ UseRaw tm u ns x).

Definition GlobRaw (tm : gmod) (imp : aimport) : Rel := fun ns n x =>
  i_glob imp = true /\ sassoc n (tbl tm true ns) = Some x.

Lemma link_spec : forall m tm ti imp m' RE RS, link m tm ti imp = Good m' -> Rep m RE RS ->
  Rep m' (fun ns n x => RE ns n x \/ ImpRaw tm ti imp ns n x)
         (fun ns n x => RS ns n x \/ ImpRaw tm ti imp ns n x \/ GlobRaw tm imp ns n x) /\
  gm_path m' = gm_path m /\ gm_attached m' = gm_attached m.
Proof.
  intros m tm ti imp m' RE RS H R. unfold link in H.
  apply rbind_good in H as (m1 & H1 & H2).
  assert (Hal : Rep m1 (fun ns n x => RE ns n x \/ (i_alias imp = Some n /\ ns = A.NSAlias /\ x = ti))
                       (fun ns n x => RS ns n x \/ (i_alias imp = Some n /\ ns = A.NSAlias /\ x = ti)) /\
                gm_path m1 = gm_path m /\ gm_attached m1 = gm_attached m).
  { destruct (i_alias imp) as [a|] eqn:Ea.
    - apply rbind_good in H1 as (al & Hb & H1). injection H1 as <-. split; [|auto].
      assert (R1 := rep_add1 m (with_names m (gm_names m) (gm_detached m) al (gm_exp_names m) (gm_exp_detached m)
                         (aset String.eqb a ti (gm_exp_aliases m))) RE RS A.NSAlias a ti m R Hb
                         ltac:(reflexivity) ltac:(intros [] [] Hne; try reflexivity; contradiction)).
      eapply rep_ext; [exact R1| |]; intros ns n x; unfold add1; split;
        (intros [Hx|(E1 & E2 & E3)]; [left; exact Hx | right]); try (injection E1 as <-); subst; auto.
    - injection H1 as <-. split; [|auto]. eapply rep_ext; [exact R| |]; intros ns n x; split; auto;
        (intros [Hx|(E & _)]; [exact Hx | discriminate E]). }
  destruct Hal as (R1 & P1 & A1).
  destruct (i_glob imp) eqn:Eg.
  - apply rbind_good in H2 as (nm & Hn & H2). apply rbind_good in H2 as (dt & Hd & H2).
    apply rbind_good in H2 as (al & Ha & H2). injection H2 as <-.
    split; [|auto].
    pose proof (bind_all_spec _ _ _ _ Hn) as Sn. pose proof (bind_all_spec _ _ _ _ Hd) as Sd.
    pose proof (bind_all_spec _ _ _ _ Ha) as Sa.
    constructor.
    + intros ns n x. assert (E : tbl (with_names m1 nm dt al (gm_exp_names m1) (gm_exp_detached m1) (gm_exp_aliases m1)) true ns
                               = tbl m1 true ns) by (destruct ns; reflexivity).
      rewrite E, (rep_exp _ _ _ R1). unfold ImpRaw. rewrite Eg. split.
      * intros [Hx|Hx]; [left; exact Hx | right; left; exact Hx].
      * intros [Hx|[Hx|(E' & _)]]; [left; exact Hx | right; exact Hx | discriminate].
    + intros ns n x. unfold ImpRaw, GlobRaw. rewrite Eg.
      assert (E : sassoc n (tbl (with_names m1 nm dt al (gm_exp_names m1) (gm_exp_detached m1) (gm_exp_aliases m1)) false ns) = Some x
                  <-> sassoc n (tbl m1 false ns) = Some x \/ sassoc n (tbl tm true ns) = Some x)
        by (destruct ns; [apply Sn | apply Sd | apply Sa]).
      rewrite E, (rep_scope _ _ _ R1). split.
      * intros [[Hx|Hx]|Hx]; [left; exact Hx | right; left; left; exact Hx | right; right; auto].
      * intros [Hx|[[Hx|(E' & _)]|(_ & Hx)]]; [left; left; exact Hx | left; right; exact Hx | discriminate | right; exact Hx].
    + intros ns n x [Hx|Hx]; [|right; left; exact Hx].
      destruct (rep_sub _ _ _ R1 _ _ _ (or_introl Hx)) as [H'|H']; [left; exact H' | right; left; left; exact H'].
  - destruct (link_uses_spec _ _ _ _ _ _ _ H2 R1) as (R2 & P2 & A2).
    split; [|split; congruence].
    eapply rep_ext; [exact R2| |]; intros ns n x; unfold addU, ImpRaw, GlobRaw; rewrite Eg; split.
    + intros [[Hx|Hx]|Hx]; [left; exact Hx | right; left; exact Hx | right; right; auto].
    + intros [Hx|[Hx|(E' & Hx)]]; [left; left; exact Hx | left; right; exact Hx | right; exact Hx].
    + intros [[Hx|Hx]|Hx]; [left; exact Hx | right; left; left; exact Hx | right; left; right; auto].
    + intros [Hx|[[Hx|(E' & Hx)]|(E' & _)]]; [left; left; exact Hx | left; right; exact Hx | right; exact Hx | discriminate].
Qed.

Definition links_fn (mods : nmap gmod) :=
  fix links (m : gmod) (ds : list (nat * aimport)) : res gmod :=
    match ds with
    | [] => Good m
    | (ti, imp) :: r =>
        match nget mods ti with
        | Some tm => let* m' := link m tm ti imp in links m' r
        | None => Bad "internal: module not loaded"
        end
    end.

Lemma links_spec : forall mods ds m m' RE RS, links_fn mods m ds = Good m' -> Rep m RE RS ->
  Rep m' (fun ns n x => RE ns n x \/ exists ti imp tm, In (ti, imp) ds /\ nget mods ti = Some tm /\ ImpRaw tm ti imp ns n x)
         (fun ns n x => RS ns n x \/ exists ti imp tm, In (ti, imp) ds /\ nget mods ti = Some tm /\
                                         (ImpRaw tm ti imp ns n x \/ GlobRaw tm imp ns n x)) /\
  gm_path m' = gm_path m /\ gm_attached m' = gm_attached m.
Proof.
  intros mods ds. induction ds as [|[ti imp] r IH]; intros m m' RE RS H R.
  - injection H as <-. split; [|auto]. eapply rep_ext; [exact R| |]; intros ns n x; split; auto;
      (intros [Hx|(? & ? & ? & [] & _)]; exact Hx).
  - cbn in H. destruct (nget mods ti) as [tm|] eqn:Et; [|discriminate].
    apply rbind_good in H as (m1 & H1 & H2).
    destruct (link_spec _ _ _ _ _ _ _ H1 R) as (R1 & P1 & A1).
    destruct (IH _ _ _ _ H2 R1) as (R2 & P2 & A2).
    split; [|split; congruence].
    eapply rep_ext; [exact R2| |]; intros ns n x; split.
    + intros [[Hx|Hx]|(ti' & imp' & tm' & Hin & Ht & Hx)].
      * left. exact Hx.
      * right. exists ti, imp, tm. split; [left; reflexivity|]. auto.
      * right. exists ti', imp', tm'. split; [right; exact Hin|]. auto.
    + intros [Hx|(ti' & imp' & tm' & [E|Hin] & Ht & Hx)].
      * left. left. exact Hx.
      * injection E as <- <-. rewrite Et in Ht. injection Ht as <-. left. right. exact Hx.
      * right. exists ti', imp', tm'. auto.
    + intros [[Hx|[Hx|Hx]]|(ti' & imp' & tm' & Hin & Ht & Hx)].
      * left. exact Hx.
      * right. exists ti, imp, tm. split; [left; reflexivity|]. auto.
      * right. exists ti, imp, tm. split; [left; reflexivity|]. auto.
      * right. exists ti', imp', tm'. split; [right; exact Hin|]. auto.
    + intros [Hx|(ti' & imp' & tm' & [E|Hin] & Ht & [Hx|Hx])].
      * left. left. exact Hx.
      * injection E as <- <-. rewrite Et in Ht. injection Ht as <-. left. right. left. exact Hx.
      * injection E as <- <-. rewrite Et in Ht. injection Ht as <-. left. right. right. exact Hx.
      * right. exists ti', imp', tm'. auto.
      * right. exists ti', imp', tm'. auto.
Qed.

Lemma forall2_in_r : forall X Y (R : X -> Y -> Prop) l1 l2, Forall2 R l1 l2 -> forall y, In y l2 -> exists x, In x l1 /\ R x y.
Proof.
  intros X Y R l1 l2 H. induction H as [|x0 y0 r1 r2 Hr Hf IH]; intros y Hy; [destruct Hy|].
  destruct Hy as [E|Hy].
  - subst y. exists x0. split; [left; reflexivity | exact Hr].
  - destruct (IH y Hy) as (x & Hx & Rx). exists x. split; [right; exact Hx | exact Rx].
Qed.

Lemma forall2_in_l : forall X Y (R : X -> Y -> Prop) l1 l2, Forall2 R l1 l2 -> forall x, In x l1 -> exists y, In y l2 /\ R x y.
Proof.
  intros X Y R l1 l2 H. induction H as [|x0 y0 r1 r2 Hr Hf IH]; intros x Hx; [destruct Hx|].
  destruct Hx as [E|Hx].
  - subst x. exists y0. split; [left; reflexivity | exact Hr].
  - destruct (IH x Hx) as (y & Hy & Ry). exists y. split; [right; exact Hy | exact Ry].
Qed.

(* ======================================================================== *)
(* 4. Loading (Elab.load): the invariant                                     *)
(* ======================================================================== *)

Lemma filed_not_alias : forall d n, A.filed_name d <> Some (A.NSAlias, n).
Proof.
  intros d n. destruct d as [| | | | | |[| |] | |]; cbn; congruence.
Qed.

Section Load.

Variable b : bundle.

Definition OwnRaw (syms : nmap gsym) (i : nat) : Rel := fun ns n x =>
  exists g, nget syms x = Some g /\ gs_mod g = i /\ A.filed_name (gs_decl g) = Some (ns, n).

Definition ImpsRaw (st : load_st) (am : amodule) : Rel := fun ns n x =>
  exists t imp tj tm, In (t, imp) (A.imports_of b am) /\ In (t, tj) (ls_loaded st) /\
    nget (ls_mods st) tj = Some tm /\ ImpRaw tm tj imp ns n x.

Definition GlobsRaw (st : load_st) (am : amodule) : Rel := fun ns n x =>
  exists t imp tj tm, In (t, imp) (A.imports_of b am) /\ In (t, tj) (ls_loaded st) /\
    nget (ls_mods st) tj = Some tm /\ GlobRaw tm imp ns n x.

Definition keys (st : load_st) : list string := map fst (ls_loaded st).

Record LInv (st : load_st) : Prop := {
  li_nodup : NoDup (keys st);
  li_loaded : forall p i, In (p, i) (ls_loaded st) ->
    i < ls_next_mod st /\ exists m am, nget (ls_mods st) i = Some m /\ gm_path m = p /\
      A.module_of b p = Some am /\
      (forall t imp, In (t, imp) (A.imports_of b am) -> exists j, In (t, j) (ls_loaded st) /\ j < i);
  li_order : forall i, In i (ls_order st) <-> exists p, In (p, i) (ls_loaded st);
  li_mods : forall i m, nget (ls_mods st) i = Some m -> exists p, In (p, i) (ls_loaded st);
  li_syms : forall s g, nget (ls_syms st) s = Some g ->
    0 < s /\ s < ls_next_sym st /\ gs_id g = s /\ exists p, In (p, gs_mod g) (ls_loaded st);
  li_above : forall s, ls_next_sym st <= s -> nget (ls_syms st) s = None;
  li_pos : 0 < ls_next_sym st;
  li_symfacts : forall s g, nget (ls_syms st) s = Some g ->
    exists m p am, nget (ls_mods st) (gs_mod g) = Some m /\ In (p, gs_mod g) (ls_loaded st) /\
      A.module_of b p = Some am /\ A.OccursIn (f_decls (am_file am)) (gs_assumes g) (gs_decl g) /\
      A.kind_of (gs_decl g) <> None /\
      (forall n, A.filed_name (gs_decl g) = Some (A.NSName, n) -> sassoc n (gm_names m) = Some s) /\
      (forall n, A.filed_name (gs_decl g) = Some (A.NSDetached, n) -> sassoc n (gm_detached m) = Some s) /\
      (forall rn n ps rt body, gs_decl g = ADFn (RAttached rn) n ps rt body ->
         exists r gr, sassoc rn (gm_names m) = Some r /\ nget (ls_syms st) r = Some gr /\
           gs_mod gr = gs_mod g /\ assoc attached_eqb (r, n) (gm_attached m) = Some s);
  li_complete : forall p i m am, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
    A.module_of b p = Some am -> forall asm d, A.OccursIn (f_decls (am_file am)) asm d -> A.kind_of d <> None ->
    exists s g, nget (ls_syms st) s = Some g /\ gs_mod g = i /\ gs_decl g = d /\ gs_assumes g = asm;
  li_attached : forall i m r n s, nget (ls_mods st) i = Some m ->
    assoc attached_eqb (r, n) (gm_attached m) = Some s ->
    exists g rn ps rt body, nget (ls_syms st) s = Some g /\ gs_mod g = i /\
      gs_decl g = ADFn (RAttached rn) n ps rt body /\ sassoc rn (gm_names m) = Some r;
  li_tables : forall p i m am, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
    A.module_of b p = Some am ->
    Rep m (fun ns n x => OwnRaw (ls_syms st) i ns n x \/ ImpsRaw st am ns n x)
          (fun ns n x => OwnRaw (ls_syms st) i ns n x \/ ImpsRaw st am ns n x \/ GlobsRaw st am ns n x);
  li_kind : forall s g, nget (ls_syms st) s = Some g -> KindOK g
}.

Lemma loaded_fun : forall st p i j, LInv st -> In (p, i) (ls_loaded st) -> In (p, j) (ls_loaded st) -> i = j.
Proof.
  intros st p i j H. pose proof (li_nodup _ H) as N. unfold keys in N. revert N.
  generalize (ls_loaded st). intro l. induction l as [|[q k] r IH]; intros N Hi Hj; [destruct Hi|].
  inversion N as [|x l' Hn N']; subst.
  destruct Hi as [Ei|Hi]; destruct Hj as [Ej|Hj].
  - congruence.
  - injection Ei as -> ->. exfalso. apply Hn. apply in_map_iff. exists (p, j). auto.
  - injection Ej as -> ->. exfalso. apply Hn. apply in_map_iff. exists (p, i). auto.
  - exact (IH N' Hi Hj).
Qed.

Lemma loaded_inj : forall st p q i, LInv st -> In (p, i) (ls_loaded st) -> In (q, i) (ls_loaded st) -> p = q.
Proof.
  intros st p q i H Hp Hq.
  destruct (li_loaded _ H p i Hp) as [_ (m & am & Em & Pm & _)].
  destruct (li_loaded _ H q i Hq) as [_ (m' & am' & Em' & Pm' & _)].
  congruence.
Qed.

(* The module being added: index mi, path p, its file's module am. *)
Section Add.

Variable st1 : load_st.
Variable p : string.
Variable am : amodule.
Variable deps : list (nat * aimport).
Variables m0 m1 : gmod.
Variable syms : nmap gsym.
Variable next : nat.

Hypothesis H1 : LInv st1.
Hypothesis Hmod : A.module_of b p = Some am.
Hypothesis Hnew : ~ In p (keys st1).
Hypothesis Hdeps : Forall2 (fun d e => snd e = snd d /\ In (fst d, fst e) (ls_loaded st1)) (A.imports_of b am) deps.
Hypothesis Hdecl : declare (ls_next_mod st1) p (am_file am) (ls_syms st1) (ls_next_sym st1) = Good (m0, syms, next).
Hypothesis Hlinks : links_fn (ls_mods st1) m0 deps = Good m1.

Local Notation mi := (ls_next_mod st1).
Local Notation st' := (LSt ((p, ls_next_mod st1) :: ls_loaded st1) (nset (ls_mods st1) (ls_next_mod st1) m1)
               (nset (ls_files st1) (ls_next_mod st1) (am_file am))
               (ls_order st1 ++ [ls_next_mod st1]) syms next (S (ls_next_mod st1))).

Lemma add_old_lt : forall q i, In (q, i) (ls_loaded st1) -> i < mi.
Proof. intros q i Hq. exact (proj1 (li_loaded _ H1 q i Hq)). Qed.

Lemma add_old_ne : forall q i, In (q, i) (ls_loaded st1) -> i <> mi.
Proof. intros q i Hq. pose proof (add_old_lt q i Hq). lia. Qed.

Lemma add_old_mods : forall i, i <> mi -> nget (ls_mods st') i = nget (ls_mods st1) i.
Proof. intros i Hi. cbn [ls_mods]. apply nget_nset_other. exact Hi. Qed.

Lemma add_D : DInv mi (am_file am) p (ls_syms st1) (ls_next_sym st1) (DSt m0 syms next []) /\
  gm_exp_names m0 = gm_names m0 /\ gm_exp_detached m0 = gm_detached m0 /\ gm_exp_aliases m0 = [] /\
  (forall asm d, Leaf (am_file am) asm d -> A.kind_of d <> None ->
     HasSym (ls_next_sym st1) (DSt m0 syms next []) d asm).
Proof. exact (declare_spec mi (am_file am) p (ls_syms st1) (ls_next_sym st1) (li_above _ H1) m0 syms next Hdecl). Qed.

Lemma add_dinv : DInv mi (am_file am) p (ls_syms st1) (ls_next_sym st1) (DSt m0 syms next []).
Proof. exact (proj1 add_D). Qed.

Lemma add_di_old : forall s, s < ls_next_sym st1 -> nget syms s = nget (ls_syms st1) s.
Proof. intros s Hs. exact (di_old _ _ _ _ _ _ add_dinv s Hs). Qed.

Lemma add_syms_old : forall s g, nget (ls_syms st1) s = Some g -> nget syms s = Some g.
Proof.
  intros s g Hs. destruct (li_syms _ H1 s g Hs) as (_ & Hlt & _).
  rewrite (add_di_old s Hlt). exact Hs.
Qed.

Lemma add_syms_new : forall s g, nget syms s = Some g -> nget (ls_syms st1) s = None ->
  ls_next_sym st1 <= s /\ gs_mod g = mi.
Proof.
  intros s g Hs Hn. destruct (Nat.lt_ge_cases s (ls_next_sym st1)) as [Hlt|Hge].
  - rewrite (add_di_old s Hlt) in Hs. congruence.
  - split; [exact Hge|]. exact (proj1 (proj2 (di_sym _ _ _ _ _ _ add_dinv s g Hge Hs))).
Qed.

Lemma add_syms_split : forall s g, nget syms s = Some g ->
  (nget (ls_syms st1) s = Some g) \/ (ls_next_sym st1 <= s /\ gs_mod g = mi).
Proof.
  intros s g Hs. destruct (nget (ls_syms st1) s) as [g'|] eqn:E.
  - left. rewrite (add_syms_old _ _ E) in Hs. congruence.
  - right. exact (add_syms_new _ _ Hs E).
Qed.

Lemma add_own_old : forall i, i <> mi -> forall ns n x, OwnRaw syms i ns n x <-> OwnRaw (ls_syms st1) i ns n x.
Proof.
  intros i Hi ns n x. unfold OwnRaw. split.
  - intros (g & Hg & Em & Ef). destruct (add_syms_split _ _ Hg) as [Hg'|(_ & E)]; [|congruence].
    exists g. auto.
  - intros (g & Hg & Em & Ef). exists g. split; [apply add_syms_old; exact Hg | auto].
Qed.

Lemma add_loaded_old : forall t j, In (t, j) (ls_loaded st1) ->
  forall j', In (t, j') (ls_loaded st') -> j' = j.
Proof.
  intros t j Ht j' [E|Hj'].
  - injection E as E1 E2. subst t. exfalso. apply Hnew. apply in_map_iff. exists (p, j). auto.
  - exact (loaded_fun _ _ _ _ H1 Hj' Ht).
Qed.

Lemma add_imps_old : forall am' q i, In (q, i) (ls_loaded st1) -> A.module_of b q = Some am' ->
  forall ns n x, ImpsRaw st' am' ns n x <-> ImpsRaw st1 am' ns n x.
Proof.
  intros am' q i Hq Hm ns n x.
  destruct (li_loaded _ H1 q i Hq) as [_ (m & am'' & _ & _ & Hm' & Himp)].
  rewrite Hm in Hm'. injection Hm' as <-.
  unfold ImpsRaw. split.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    destruct (Himp t imp Hin) as (j & Hj & _).
    pose proof (add_loaded_old t j Hj tj Ht) as ->.
    rewrite add_old_mods in Htm; [|exact (add_old_ne _ _ Hj)].
    exists t, imp, j, tm. auto.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    exists t, imp, tj, tm. split; [exact Hin|]. split; [right; exact Ht|].
    rewrite add_old_mods; [|exact (add_old_ne _ _ Ht)]. auto.
Qed.

Lemma add_globs_old : forall am' q i, In (q, i) (ls_loaded st1) -> A.module_of b q = Some am' ->
  forall ns n x, GlobsRaw st' am' ns n x <-> GlobsRaw st1 am' ns n x.
Proof.
  intros am' q i Hq Hm ns n x.
  destruct (li_loaded _ H1 q i Hq) as [_ (m & am'' & _ & _ & Hm' & Himp)].
  rewrite Hm in Hm'. injection Hm' as <-.
  unfold GlobsRaw. split.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    destruct (Himp t imp Hin) as (j & Hj & _).
    pose proof (add_loaded_old t j Hj tj Ht) as ->.
    rewrite add_old_mods in Htm; [|exact (add_old_ne _ _ Hj)].
    exists t, imp, j, tm. auto.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    exists t, imp, tj, tm. split; [exact Hin|]. split; [right; exact Ht|].
    rewrite add_old_mods; [|exact (add_old_ne _ _ Ht)]. auto.
Qed.

(* m0's tables are the module's own declarations. *)
Lemma add_rep0 : Rep m0 (OwnRaw syms mi) (OwnRaw syms mi).
Proof.
  destruct add_D as (DI & En & Ed & Ea & _).
  assert (Hown : forall ns n x, sassoc n (tbl m0 false ns) = Some x <-> OwnRaw syms mi ns n x).
  { intros ns n x. unfold OwnRaw. destruct ns; cbn [tbl].
    - split.
      + intro Hx. destruct (di_names _ _ _ _ _ _ DI n x Hx) as [Hle (g & Hg & Hf)].
        exists g. split; [exact Hg|]. split; [exact (proj1 (proj2 (di_sym _ _ _ _ _ _ DI x g Hle Hg)))| exact Hf].
      + intros (g & Hg & Em & Hf). destruct (add_syms_split _ _ Hg) as [Hg'|(Hle & _)].
        * destruct (li_syms _ H1 x g Hg') as (_ & _ & _ & q & Hq). rewrite Em in Hq.
          exfalso. exact (add_old_ne _ _ Hq eq_refl).
        * exact (proj1 (proj2 (proj2 (proj2 (proj2 (di_sym _ _ _ _ _ _ DI x g Hle Hg))))) n Hf).
    - split.
      + intro Hx. destruct (di_detached _ _ _ _ _ _ DI n x Hx) as [Hle (g & Hg & Hf)].
        exists g. split; [exact Hg|]. split; [exact (proj1 (proj2 (di_sym _ _ _ _ _ _ DI x g Hle Hg)))| exact Hf].
      + intros (g & Hg & Em & Hf). destruct (add_syms_split _ _ Hg) as [Hg'|(Hle & _)].
        * destruct (li_syms _ H1 x g Hg') as (_ & _ & _ & q & Hq). rewrite Em in Hq.
          exfalso. exact (add_old_ne _ _ Hq eq_refl).
        * exact (proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (di_sym _ _ _ _ _ _ DI x g Hle Hg)))))) n Hf).
    - pose proof (di_aliases _ _ _ _ _ _ DI) as EA. cbn [ds_mod] in EA. rewrite EA. split; [discriminate|].
      intros (g & _ & _ & Hf). exfalso. exact (filed_not_alias _ _ Hf). }
  constructor.
  - intros ns n x. rewrite <- Hown. destruct ns; cbn [tbl]; rewrite ?En, ?Ed, ?Ea; try reflexivity.
    pose proof (di_aliases _ _ _ _ _ _ DI) as EA. cbn [ds_mod] in EA. rewrite EA. reflexivity.
  - exact Hown.
  - auto.
Qed.

Lemma add_imps_new : forall ns n x,
  (exists ti imp tm, In (ti, imp) deps /\ nget (ls_mods st1) ti = Some tm /\ ImpRaw tm ti imp ns n x)
  <-> ImpsRaw st' am ns n x.
Proof.
  intros ns n x. unfold ImpsRaw. split.
  - intros (ti & imp & tm & Hin & Ht & Hr).
    destruct (forall2_in_r _ _ _ _ _ Hdeps _ Hin) as ([t imp'] & Hin' & E & Hl). cbn in E, Hl. subst imp'.
    exists t, imp, ti, tm. split; [exact Hin'|]. split; [right; exact Hl|].
    rewrite add_old_mods; [|exact (add_old_ne _ _ Hl)]. auto.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    destruct (forall2_in_l _ _ _ _ _ Hdeps _ Hin) as ([ti imp'] & Hin' & E & Hl). cbn in E, Hl. subst imp'.
    pose proof (add_loaded_old _ _ Hl _ Ht) as ->.
    rewrite add_old_mods in Htm; [|exact (add_old_ne _ _ Hl)].
    exists ti, imp, tm. auto.
Qed.

Lemma add_globs_new : forall ns n x,
  (exists ti imp tm, In (ti, imp) deps /\ nget (ls_mods st1) ti = Some tm /\ GlobRaw tm imp ns n x)
  <-> GlobsRaw st' am ns n x.
Proof.
  intros ns n x. unfold GlobsRaw. split.
  - intros (ti & imp & tm & Hin & Ht & Hr).
    destruct (forall2_in_r _ _ _ _ _ Hdeps _ Hin) as ([t imp'] & Hin' & E & Hl). cbn in E, Hl. subst imp'.
    exists t, imp, ti, tm. split; [exact Hin'|]. split; [right; exact Hl|].
    rewrite add_old_mods; [|exact (add_old_ne _ _ Hl)]. auto.
  - intros (t & imp & tj & tm & Hin & Ht & Htm & Hr).
    destruct (forall2_in_l _ _ _ _ _ Hdeps _ Hin) as ([ti imp'] & Hin' & E & Hl). cbn in E, Hl. subst imp'.
    pose proof (add_loaded_old _ _ Hl _ Ht) as ->.
    rewrite add_old_mods in Htm; [|exact (add_old_ne _ _ Hl)].
    exists ti, imp, tm. auto.
Qed.

Lemma add_rep1 : Rep m1 (fun ns n x => OwnRaw syms mi ns n x \/ ImpsRaw st' am ns n x)
                       (fun ns n x => OwnRaw syms mi ns n x \/ ImpsRaw st' am ns n x \/ GlobsRaw st' am ns n x)
                 /\ gm_path m1 = p /\ gm_attached m1 = gm_attached m0.
Proof.
  destruct (links_spec _ _ _ _ _ _ Hlinks add_rep0) as (R & P & At).
  split; [|split; [rewrite P; exact (di_path _ _ _ _ _ _ add_dinv) | exact At]].
  eapply rep_ext; [exact R| |]; intros ns n x.
  - rewrite <- add_imps_new. reflexivity.
  - rewrite <- add_imps_new, <- add_globs_new. split.
    + intros [Hx|(ti & imp & tm & Hin & Ht & [Hr|Hr])].
      * left. exact Hx.
      * right. left. exists ti, imp, tm. auto.
      * right. right. exists ti, imp, tm. auto.
    + intros [Hx|[(ti & imp & tm & Hin & Ht & Hr)|(ti & imp & tm & Hin & Ht & Hr)]]; [left; exact Hx| |];
        right; exists ti, imp, tm; auto.
Qed.

Lemma add_names_transfer : forall n s, sassoc n (gm_names m0) = Some s -> sassoc n (gm_names m1) = Some s.
Proof.
  intros n s H. destruct add_rep1 as (R & _ & _).
  apply (rep_scope _ _ _ R A.NSName). left.
  exact (proj1 (rep_scope _ _ _ add_rep0 A.NSName n s) H).
Qed.

Lemma add_linv : LInv st'.
Proof.
  destruct add_D as (DI & _ & _ & _ & Dc).
  destruct add_rep1 as (R1 & P1 & A1).
  assert (Hm1 : nget (ls_mods st') mi = Some m1) by (cbn [ls_mods]; apply nget_nset_same).
  assert (Hcase : forall q i, In (q, i) (ls_loaded st') -> (q = p /\ i = mi) \/ (In (q, i) (ls_loaded st1) /\ i <> mi)).
  { intros q i [E|Hq]; [injection E as <- <-; left; auto | right; split; [exact Hq | exact (add_old_ne _ _ Hq)]]. }
  assert (Hnext : ls_next_sym st1 <= next) by exact (di_next _ _ _ _ _ _ DI).
  constructor.
  - unfold keys. cbn [ls_loaded map fst]. constructor; [exact Hnew | exact (li_nodup _ H1)].
  - intros q i Hq. destruct (Hcase q i Hq) as [[-> ->]|[Hq' Hne]].
    + split; [cbn; lia|]. exists m1, am. split; [exact Hm1|]. split; [exact P1|]. split; [exact Hmod|].
      intros t imp Hin. destruct (forall2_in_l _ _ _ _ _ Hdeps _ Hin) as ([ti imp'] & _ & _ & Hl). cbn in Hl.
      exists ti. split; [right; exact Hl | exact (add_old_lt _ _ Hl)].
    + destruct (li_loaded _ H1 q i Hq') as [Hlt (m & am' & Em & Pm & Mm & Im)].
      split; [cbn; lia|]. exists m, am'. split; [rewrite add_old_mods; [exact Em | exact Hne]|].
      split; [exact Pm|]. split; [exact Mm|].
      intros t imp Hin. destruct (Im t imp Hin) as (j & Hj & Hlt'). exists j. split; [right; exact Hj | exact Hlt'].
  - intro i. cbn [ls_order ls_loaded]. rewrite in_app_iff. split.
    + intros [Hi|[<-|[]]]; [destruct (proj1 (li_order _ H1 i) Hi) as (q & Hq); exists q; right; exact Hq | exists p; left; reflexivity].
    + intros (q & [E|Hq]); [injection E as _ <-; right; left; reflexivity | left; apply (li_order _ H1); exists q; exact Hq].
  - intros i m Hm. destruct (Nat.eq_dec i mi) as [->|Hne]; [exists p; left; reflexivity|].
    rewrite add_old_mods in Hm; [|exact Hne]. destruct (li_mods _ H1 i m Hm) as (q & Hq). exists q. right. exact Hq.
  - intros s g Hs. cbn [ls_syms ls_next_sym ls_loaded]. destruct (add_syms_split _ _ Hs) as [Hs'|(Hle & Hgm)].
    + destruct (li_syms _ H1 s g Hs') as (H0 & Hlt & Hid & q & Hq). split; [exact H0|]. split; [lia|].
      split; [exact Hid|]. exists q. right. exact Hq.
    + pose proof (li_pos _ H1). split; [lia|]. split; [exact (dinv_sym_lt _ _ _ _ _ _ _ _ DI Hs)|].
      split; [exact (proj1 (di_sym _ _ _ _ _ _ DI s g Hle Hs))|]. exists p. rewrite Hgm. left. reflexivity.
  - exact (di_above _ _ _ _ _ _ DI).
  - pose proof (li_pos _ H1). cbn. lia.
  - intros s g Hs. cbn [ls_syms ls_mods ls_loaded] in *. destruct (add_syms_split _ _ Hs) as [Hs'|(Hle & Hgm)].
    + destruct (li_symfacts _ H1 s g Hs') as (m & q & am' & Em & Hq & Mm & Oc & Kd & Fn & Fd & Fa).
      exists m, q, am'. split; [rewrite nget_nset_other; [exact Em | exact (add_old_ne _ _ Hq)]|].
      split; [right; exact Hq|]. split; [exact Mm|]. split; [exact Oc|]. split; [exact Kd|].
      split; [exact Fn|]. split; [exact Fd|].
      intros rn n ps rt body Ed. destruct (Fa rn n ps rt body Ed) as (r & gr & E1 & E2 & E3 & E4).
      exists r, gr. split; [exact E1|]. split; [apply add_syms_old; exact E2|]. auto.
    + destruct (di_sym _ _ _ _ _ _ DI s g Hle Hs) as (_ & _ & Oc & Kd & Fn & Fd & Fa).
      exists m1, p, am. rewrite Hgm. split; [apply nget_nset_same|]. split; [left; reflexivity|].
      split; [exact Hmod|]. split; [exact Oc|]. split; [exact Kd|].
      split; [intros n Hf; apply add_names_transfer; exact (Fn n Hf)|].
      split; [intros n Hf; apply (rep_scope _ _ _ R1 A.NSDetached); left; exists g; auto|].
      intros rn n ps rt body Ed.
      destruct (Fa ltac:(exists rn, n, ps, rt, body; exact Ed)) as (rn' & n' & ps' & rt' & body' & r & gr & E0 & E1 & E2 & _ & E4 & E5).
      rewrite Ed in E0. injection E0 as <- <- <- <- <-.
      exists r, gr. split; [apply add_names_transfer; exact E1|]. split; [exact E2|]. split; [exact E4|].
      rewrite A1. exact E5.
  - intros q i m am' Hq Hm Hma asm d Ho Hk. cbn [ls_syms]. destruct (Hcase q i Hq) as [[-> ->]|[Hq' Hne]].
    + rewrite Hm1 in Hm. injection Hm as <-. rewrite Hmod in Hma. injection Hma as <-.
      destruct (Dc asm d Ho Hk) as (s & g & Hle & Hs & E1 & E2).
      exists s, g. split; [exact Hs|]. split; [exact (proj1 (proj2 (di_sym _ _ _ _ _ _ DI s g Hle Hs)))|]. auto.
    + rewrite add_old_mods in Hm; [|exact Hne].
      destruct (li_complete _ H1 q i m am' Hq' Hm Hma asm d Ho Hk) as (s & g & Hs & E1 & E2 & E3).
      exists s, g. split; [apply add_syms_old; exact Hs|]. auto.
  - intros i m r n s Hm Ha. cbn [ls_syms]. destruct (Nat.eq_dec i mi) as [->|Hne].
    + rewrite Hm1 in Hm. injection Hm as <-. rewrite A1 in Ha.
      destruct (di_attached _ _ _ _ _ _ DI r n s Ha) as [Hle (g & rn & ps & rt & body & Hs & Ed & Er)].
      exists g, rn, ps, rt, body. split; [exact Hs|].
      split; [exact (proj1 (proj2 (di_sym _ _ _ _ _ _ DI s g Hle Hs)))|]. split; [exact Ed|].
      apply add_names_transfer. exact Er.
    + rewrite add_old_mods in Hm; [|exact Hne].
      destruct (li_attached _ H1 i m r n s Hm Ha) as (g & rn & ps & rt & body & Hs & E1 & E2 & E3).
      exists g, rn, ps, rt, body. split; [apply add_syms_old; exact Hs|]. auto.
  - intros q i m am' Hq Hm Hma. destruct (Hcase q i Hq) as [[-> ->]|[Hq' Hne]].
    + rewrite Hm1 in Hm. injection Hm as <-. rewrite Hmod in Hma. injection Hma as <-. exact R1.
    + rewrite add_old_mods in Hm; [|exact Hne].
      pose proof (li_tables _ H1 q i m am' Hq' Hm Hma) as R.
      eapply rep_ext; [exact R| |]; intros ns n x; cbn [ls_syms];
        rewrite (add_own_old i Hne), (add_imps_old am' q i Hq' Hma); [reflexivity|].
      rewrite (add_globs_old am' q i Hq' Hma). reflexivity.
  - intros s g Hs. cbn [ls_syms] in Hs. destruct (add_syms_split _ _ Hs) as [Hs'|(Hle & _)].
    + exact (li_kind _ H1 s g Hs').
    + exact (di_kind _ _ _ _ _ _ DI s g Hs Hle).
Qed.

End Add.

Inductive Reach (p : string) : string -> Prop :=
| RRefl : Reach p p
| RStep : forall q am t imp, Reach p q -> A.module_of b q = Some am -> In (t, imp) (A.imports_of b am) -> Reach p t.

Lemma reach_first : forall p am tp imp q, A.module_of b p = Some am -> In (tp, imp) (A.imports_of b am) ->
  Reach tp q -> Reach p q.
Proof.
  intros p am tp imp q Hm Hin H. induction H as [|q am' t imp' H IH Hm' Hin'].
  - eapply RStep; [apply RRefl | exact Hm | exact Hin].
  - eapply RStep; [exact IH | exact Hm' | exact Hin'].
Qed.

Lemma sassoc_in : forall V k (l : list (string * V)) v, sassoc k l = Some v -> In (k, v) l.
Proof.
  intros V k l v. induction l as [|[k' v'] r IH]; [discriminate|].
  unfold sassoc in *. cbn [assoc]. destruct (String.eqb_spec k k') as [<-|_].
  - intro E. injection E as <-. left. reflexivity.
  - intro H. right. exact (IH H).
Qed.

Lemma sassoc_none : forall V k (l : list (string * V)), sassoc k l = None -> ~ In k (map fst l).
Proof.
  intros V k l. induction l as [|[k' v'] r IH]; [intros _ []|].
  unfold sassoc in *. cbn [assoc map fst]. destruct (String.eqb_spec k k') as [<-|Hne]; [discriminate|].
  intros H [E|Hin]; [exact (Hne (eq_sym E)) | exact (IH H Hin)].
Qed.

Definition LoadPost (loading : list string) (p : string) (st : load_st) (i : nat) (st' : load_st) : Prop :=
  LInv st' /\ (forall q j, In (q, j) (ls_loaded st) -> In (q, j) (ls_loaded st')) /\ In (p, i) (ls_loaded st') /\
  (forall q j, In (q, j) (ls_loaded st') -> In (q, j) (ls_loaded st) \/ Reach p q) /\
  (forall x, In x loading -> ~ In x (keys st) -> ~ In x (keys st')).

Definition go_deps (k : nat) (loading : list string) (p : string) :=
  fix go (ds : list (string * aimport)) (st : load_st) : res (list (nat * aimport) * load_st) :=
    match ds with
    | [] => Good ([], st)
    | (tp, imp) :: r =>
        let* (ti, st1) := load k b (p :: loading) tp st in
        let* (rest, st2) := go r st1 in
        Good ((ti, imp) :: rest, st2)
    end.

Lemma go_deps_spec : forall k loading p,
  (forall loading' q st i st', load k b loading' q st = Good (i, st') -> LInv st -> LoadPost loading' q st i st') ->
  forall ds st deps st', go_deps k loading p ds st = Good (deps, st') -> LInv st ->
  LInv st' /\ (forall q j, In (q, j) (ls_loaded st) -> In (q, j) (ls_loaded st')) /\
  Forall2 (fun d e => snd e = snd d /\ In (fst d, fst e) (ls_loaded st')) ds deps /\
  (forall q j, In (q, j) (ls_loaded st') -> In (q, j) (ls_loaded st) \/ exists tp imp, In (tp, imp) ds /\ Reach tp q) /\
  (forall x, In x (p :: loading) -> ~ In x (keys st) -> ~ In x (keys st')).
Proof.
  intros k loading p IH ds. induction ds as [|[tp imp] r IHr]; intros st deps st' H Hi.
  - injection H as <- <-. split; [exact Hi|]. split; [auto|]. split; [constructor|]. split; [auto|]. auto.
  - cbn in H. apply rbind_good in H as ([ti st1] & H1 & H2).
    apply rbind_good in H2 as ([rest st2] & H2 & H3). injection H3 as <- <-.
    destruct (IH _ _ _ _ _ H1 Hi) as (L1 & M1 & In1 & R1 & N1).
    destruct (IHr _ _ _ H2 L1) as (L2 & M2 & F2 & R2 & N2).
    split; [exact L2|]. split; [intros; apply M2, M1; assumption|].
    split; [constructor; [split; [reflexivity | apply M2; exact In1] | exact F2]|].
    split.
    + intros q j Hq. destruct (R2 q j Hq) as [Hq1|(tp' & imp' & Hin & Hr)].
      * destruct (R1 q j Hq1) as [Hq0|Hr]; [left; exact Hq0 | right; exists tp, imp; split; [left; reflexivity | exact Hr]].
      * right. exists tp', imp'. split; [right; exact Hin | exact Hr].
    + intros x Hx Hn. apply (N2 x Hx). apply (N1 x Hx Hn).
Qed.

Lemma load_spec : forall k loading p st i st', load k b loading p st = Good (i, st') -> LInv st ->
  LoadPost loading p st i st'.
Proof.
  induction k as [|k IH]; intros loading p st i st' H Hi; [discriminate|].
  cbn [load] in H.
  destruct (sassoc p (ls_loaded st)) as [i0|] eqn:Ep.
  - injection H as <- <-. split; [exact Hi|]. split; [auto|]. split; [apply sassoc_in; exact Ep|].
    split; [auto|]. auto.
  - destruct (smem p loading) eqn:Es; [discriminate|].
    destruct (find_module p (b_mods b)) as [am|] eqn:Ef; [|discriminate].
    rewrite find_module_of in Ef.
    apply rbind_good in H as ([deps st1] & Hg & H).
    change (go_deps k loading p (A.imports_of b am) st = Good (deps, st1)) in Hg.
    destruct (go_deps_spec k loading p IH _ _ _ _ Hg Hi) as (L1 & M1 & F1 & R1 & N1).
    apply rbind_good in H as ([[m0 syms] next] & Hd & H).
    apply rbind_good in H as (m1 & Hl & H).
    change (links_fn (ls_mods st1) m0 deps = Good m1) in Hl.
    injection H as <- <-.
    assert (Hnew : ~ In p (keys st1)) by (apply (N1 p (or_introl eq_refl)); apply sassoc_none; exact Ep).
    pose proof (add_linv st1 p am deps m0 m1 syms next L1 Ef Hnew F1 Hd Hl) as L2.
    split; [exact L2|]. split; [intros q j Hq; right; apply M1; exact Hq|].
    split; [left; reflexivity|]. split.
    + intros q j [E|Hq]; [injection E as <- _; right; apply RRefl|].
      destruct (R1 q j Hq) as [Hq0|(tp & imp & Hin & Hr)]; [left; exact Hq0|].
      right. exact (reach_first p am tp imp q Ef Hin Hr).
    + intros x Hx Hn [E|Hin].
      * cbn in E. subst x. exact (smem_false _ _ Es Hx).
      * exact (N1 x (or_intror Hx) Hn Hin).
Qed.

End Load.

(* ======================================================================== *)
(* 5. The Audit world the loader's world denotes                             *)
(* ======================================================================== *)

Fixpoint find_mod (w : Elab.world) (p : string) (l : list nat) : option gmod :=
  match l with
  | [] => None
  | i :: r => let m := get_mod w i in if String.eqb (gm_path m) p then Some m else find_mod w p r
  end.

(* A table entry as an Audit entity: a symbol, or (an alias) a module path. *)
Definition aent (w : Elab.world) (ns : A.nspace) (x : nat) : A.ent :=
  match ns with A.NSAlias => A.EMod (gm_path (get_mod w x)) | _ => A.ESym x end.

Definition W_read (w : Elab.world) (R : nat -> list nat) (F : string -> nat) : A.world :=
  A.World
    (fun x => match nget (w_syms w) x with
              | Some g => Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g))
              | None => None
              end)
    (fun p ns n => match find_mod w p (w_order w) with
                   | Some m => option_map (aent w ns) (sassoc n (tbl m false ns))
                   | None => None end)
    (fun p ns n => match find_mod w p (w_order w) with
                   | Some m => option_map (aent w ns) (sassoc n (tbl m true ns))
                   | None => None end)
    (fun r n => match nget (w_syms w) r with
                | Some g => assoc attached_eqb (r, n) (gm_attached (get_mod w (gs_mod g)))
                | None => None
                end)
    R F.

(* The fields of WorldOf about the loader: everything but requirements and
   field numbering. *)
Record LoaderReading (b : bundle) (W : A.world) : Prop := {
  lr_acyclic : exists rank : string -> nat, forall p m t imp,
      A.Loaded b p -> A.module_of b p = Some m -> In (t, imp) (A.imports_of b m) ->
      A.module_of b t <> None /\ rank t < rank p;
  lr_decl_sound : forall s d, A.w_decl W s = Some d -> s <> 0 /\ A.Declared b d /\ A.kind_of (A.d_ast d) <> None;
  lr_decl_complete : forall d, A.Declared b d -> A.kind_of (A.d_ast d) <> None ->
      exists s, A.w_decl W s = Some d /\ forall s', A.w_decl W s' = Some d -> s' = s;
  lr_exports : forall p ns n e, A.Loaded b p -> (A.w_exp W p ns n = Some e <-> A.Exports b W p ns n e);
  lr_scope : forall p ns n e, A.Loaded b p -> (A.w_scope W p ns n = Some e <-> A.InScope b W p ns n e);
  lr_attached : forall r n s, A.w_attached W r n = Some s <-> A.AttachedDecl W r n s
}.

(* The rest: requirements ([D71]/[D73]) and field numbering. *)
Record ReqsReading (W : A.world) : Prop := {
  rr_nodup : forall s, NoDup (A.w_reqs W s);
  rr_plain : forall s k, A.kind_sym W s = Some k ->
      k <> A.DKTag -> k <> A.DKAlias -> k <> A.DKType -> k <> A.DKUnit ->
      forall t, In t (A.w_reqs W s) <-> A.AssumeTypes W s t;
  rr_none : forall s, A.kind_sym W s = Some A.DKType \/ A.kind_sym W s = Some A.DKUnit -> A.w_reqs W s = [];
  rr_closed : A.RelevanceClosed W (A.w_reqs W);
  rr_least : forall R, A.RelevanceClosed W R ->
      (forall s, A.kind_sym W s <> Some A.DKTag -> A.kind_sym W s <> Some A.DKAlias -> R s = A.w_reqs W s) ->
      forall g, (A.kind_sym W g = Some A.DKTag \/ A.kind_sym W g = Some A.DKAlias) -> incl (A.w_reqs W g) (R g);
  rr_listed : forall g d ps t n, A.w_decl W g = Some d -> A.nominal_body (A.d_ast d) = Some (ps, t) -> In n ps ->
      exists x, A.resolve_sym W (A.d_mod d) [n] = Some x /\ A.AssumeTypes W g x
}.

Theorem worldof_of_reading : forall b W,
  LoaderReading b W -> ReqsReading W -> (forall a c, A.w_fld W a = A.w_fld W c -> a = c) -> A.WorldOf b W.
Proof.
  intros b W [L1 L2 L3 L4 L5 L6] [R1 R2 R3 R4 R5 R6] F. constructor; assumption.
Qed.

Section Final.

Variable b : bundle.
Variable st : load_st.
Variable e : nat.
Hypothesis HI : LInv b st.
Hypothesis He : In (b_entry b, e) (ls_loaded st).
Hypothesis Hr : forall q j, In (q, j) (ls_loaded st) -> Reach b (b_entry b) q.
Variables (R : nat -> list nat) (F : string -> nat).

Let w := Elab.World (ls_mods st) (ls_order st) e (ls_syms st) (ls_next_sym st) (ls_files st).
Let W := W_read w R F.

Lemma fin_get_mod : forall i m, nget (ls_mods st) i = Some m -> get_mod w i = m.
Proof. intros i m H. unfold get_mod. cbn [w_mods w]. rewrite H. reflexivity. Qed.

Lemma fin_path : forall p i, In (p, i) (ls_loaded st) -> exists m am, nget (ls_mods st) i = Some m /\
  get_mod w i = m /\ gm_path m = p /\ A.module_of b p = Some am.
Proof.
  intros p i H. destruct (li_loaded _ _ HI p i H) as [_ (m & am & Em & Pm & Mm & _)].
  exists m, am. split; [exact Em|]. split; [apply fin_get_mod; exact Em|]. auto.
Qed.

Lemma fin_loaded : forall p, A.Loaded b p <-> exists i, In (p, i) (ls_loaded st).
Proof.
  intro p. split.
  - intro H. induction H as [|p m t imp H IH Hm Hin].
    + exists e. exact He.
    + destruct IH as (i & Hi). destruct (li_loaded _ _ HI p i Hi) as [_ (m' & am & _ & _ & Mm & Im)].
      rewrite Hm in Mm. injection Mm as <-. destruct (Im t imp Hin) as (j & Hj & _). exists j. exact Hj.
  - intros (i & Hi). pose proof (Hr p i Hi) as Hreach. clear Hi. induction Hreach as [|q am t imp _ IH Hm Hin].
    + apply A.LoadEntry.
    + eapply A.LoadImport; [exact IH | exact Hm | exact Hin].
Qed.

Lemma fin_find : forall p i m, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  find_mod w p (w_order w) = Some m.
Proof.
  intros p i m Hi Hm. cbn [w_order w].
  assert (Hall : forall j, In j (ls_order st) -> exists q, In (q, j) (ls_loaded st)) by (intros j Hj; apply (li_order _ _ HI); exact Hj).
  assert (Hin : In i (ls_order st)) by (apply (li_order _ _ HI); exists p; exact Hi).
  revert Hall Hin. generalize (ls_order st). intro l. induction l as [|j r IH]; intros Hall Hin; [destruct Hin|].
  cbn [find_mod]. destruct (Hall j (or_introl eq_refl)) as (q & Hq).
  destruct (fin_path q j Hq) as (mj & _ & Ej & Gj & Pj & _). rewrite Gj, Pj.
  destruct (String.eqb_spec q p) as [->|Hne].
  - pose proof (loaded_fun _ _ _ _ _ HI Hq Hi) as ->. rewrite Ej in Hm. injection Hm as ->. reflexivity.
  - destruct Hin as [->|Hin].
    + exfalso. apply Hne. exact (loaded_inj _ _ _ _ _ HI Hq Hi).
    + apply IH; [intros j' Hj'; apply Hall; right; exact Hj' | exact Hin].
Qed.

Lemma fin_exp : forall p i m ns n, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.w_exp W p ns n = option_map (aent w ns) (sassoc n (tbl m true ns)).
Proof. intros p i m ns n Hi Hm. cbn [A.w_exp W W_read]. rewrite (fin_find p i m Hi Hm). reflexivity. Qed.

Lemma fin_scope : forall p i m ns n, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.w_scope W p ns n = option_map (aent w ns) (sassoc n (tbl m false ns)).
Proof. intros p i m ns n Hi Hm. cbn [A.w_scope W W_read]. rewrite (fin_find p i m Hi Hm). reflexivity. Qed.

Lemma fin_decl : forall s, A.w_decl W s = match nget (ls_syms st) s with
  | Some g => Some (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)) | None => None end.
Proof. reflexivity. Qed.

(* the module a symbol is in, by its path *)
Lemma fin_sym_mod : forall s g, nget (ls_syms st) s = Some g ->
  exists q m am, In (q, gs_mod g) (ls_loaded st) /\ nget (ls_mods st) (gs_mod g) = Some m /\
    get_mod w (gs_mod g) = m /\ gm_path m = q /\ A.module_of b q = Some am.
Proof.
  intros s g Hs. destruct (li_syms _ _ HI s g Hs) as (_ & _ & _ & q & Hq).
  destruct (fin_path q _ Hq) as (m & am & E1 & E2 & E3 & E4). exists q, m, am. auto.
Qed.

Lemma fin_path_inj : forall q i, In (q, i) (ls_loaded st) -> forall j q', In (q', j) (ls_loaded st) ->
  gm_path (get_mod w j) = q -> j = i.
Proof.
  intros q i Hq j q' Hq' E. destruct (fin_path q' j Hq') as (m & _ & _ & Gm & Pm & _).
  rewrite Gm, Pm in E. subst q'. exact (loaded_fun _ _ _ _ _ HI Hq' Hq).
Qed.

Lemma fin_own : forall p i ns n s, In (p, i) (ls_loaded st) ->
  (exists d, A.w_decl W s = Some d /\ A.d_mod d = p /\ A.filed_name (A.d_ast d) = Some (ns, n)) <->
  OwnRaw (ls_syms st) i ns n s.
Proof.
  intros p i ns n s Hi. unfold OwnRaw. rewrite fin_decl. split.
  - intros (d & Hd & Em & Ef). destruct (nget (ls_syms st) s) as [g|] eqn:Hs; [|discriminate].
    injection Hd as <-. cbn in Em, Ef. exists g. split; [reflexivity|]. split; [|exact Ef].
    destruct (li_syms _ _ HI s g Hs) as (_ & _ & _ & q & Hq). exact (fin_path_inj p i Hi _ q Hq Em).
  - intros (g & Hs & Em & Ef). rewrite Hs. eexists. split; [reflexivity|]. cbn. split; [|exact Ef].
    destruct (fin_path p i Hi) as (m & _ & Em' & Gm & Pm & _). rewrite Em, Gm. exact Pm.
Qed.

Lemma fin_imports : forall p i am t imp, In (p, i) (ls_loaded st) -> A.module_of b p = Some am ->
  In (t, imp) (A.imports_of b am) -> exists j tm, In (t, j) (ls_loaded st) /\ j < i /\ nget (ls_mods st) j = Some tm.
Proof.
  intros p i am t imp Hi Hm Hin. destruct (li_loaded _ _ HI p i Hi) as [_ (m & am' & _ & _ & Mm & Im)].
  rewrite Hm in Mm. injection Mm as <-. destruct (Im t imp Hin) as (j & Hj & Hlt).
  destruct (fin_path t j Hj) as (tm & _ & Etm & _). exists j, tm. auto.
Qed.

Lemma exports_at : forall i p m am, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.module_of b p = Some am ->
  (forall ns n x, sassoc n (tbl m true ns) = Some x -> A.Exports b W p ns n (aent w ns x)) /\
  (forall ns n e0, A.Exports b W p ns n e0 -> exists x, sassoc n (tbl m true ns) = Some x /\ e0 = aent w ns x).
Proof.
  intro i. induction i as [i IH] using lt_wf_ind. intros p m am Hi Hm Hma.
  pose proof (li_tables _ _ HI p i m am Hi Hm Hma) as Rp.
  (* the targets: their exports are their tables *)
  assert (Ht : forall t imp, In (t, imp) (A.imports_of b am) -> exists j tm, In (t, j) (ls_loaded st) /\
             nget (ls_mods st) j = Some tm /\
             forall ns n e0, A.w_exp W t ns n = Some e0 <-> A.Exports b W t ns n e0).
  { intros t imp Hin. destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm & Hj & Hlt & Htm).
    destruct (fin_path t j Hj) as (tm' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
    destruct (IH j Hlt t tm amt Hj Htm Hamt) as [T1 T2]. exists j, tm. split; [exact Hj|]. split; [exact Htm|].
    intros ns n e0. rewrite (fin_exp t j tm ns n Hj Htm). split.
    - intro H. destruct (sassoc n (tbl tm true ns)) as [x|] eqn:Ex; [|discriminate]. injection H as <-. exact (T1 ns n x Ex).
    - intro H. destruct (T2 ns n e0 H) as (x & Ex & ->). rewrite Ex. reflexivity. }
  split.
  - intros ns n x Hx. apply (rep_exp _ _ _ Rp) in Hx as [Ho|(t & imp & tj & tm & Hin & Htj & Htm & Hraw)].
    + apply (fin_own p i ns n x Hi) in Ho as (d & Hd & Em & Ef).
      assert (Hns : ns <> A.NSAlias) by (intro E; subst ns; exact (filed_not_alias _ _ Ef)).
      replace (aent w ns x) with (A.ESym x) by (destruct ns; [reflexivity | reflexivity | contradiction]).
      eapply A.ExOwn; [exact Hd | exact Em | exact Ef].
    + destruct (Ht t imp Hin) as (j & tm' & Hj & Htm' & Teq).
      pose proof (loaded_fun _ _ _ _ _ HI Htj Hj) as <-. rewrite Htm in Htm'. injection Htm' as <-.
      destruct Hraw as [(Ea & -> & ->)|(Eg & u & Hu & El & Hraw)].
      * destruct (fin_path t tj Htj) as (tm' & _ & Etm & Gt & Pt & _). rewrite Htm in Etm. injection Etm as <-.
        cbn [aent]. rewrite Gt, Pt. eapply A.ExAlias; [exact Hma | exact Hin | exact Ea].
      * assert (Teq' : forall ns n e0, option_map (aent w ns) (sassoc n (tbl tm true ns)) = Some e0 <-> A.Exports b W t ns n e0)
          by (intros ns' n' e9; rewrite <- (fin_exp t tj tm ns' n' Htj Htm); apply Teq).
        clear Teq. rename Teq' into Teq.
        destruct Hraw as [(Ed & -> & Ex)|[(Ed & -> & Ex)|(Ed & -> & En & Ex)]].
        -- pose proof (proj1 (Teq A.NSDetached (u_name u) (A.ESym x))) as T. cbn [tbl] in T. rewrite Ex in T.
           specialize (T eq_refl).
           pose proof (@A.ExUse b W p am t imp u (A.ESym x) A.NSDetached Hma Hin Eg Hu) as X.
           rewrite Ed in X. specialize (X eq_refl (or_introl T)). unfold local_of in El. rewrite El in X. exact X.
        -- pose proof (proj1 (Teq A.NSName (u_name u) (A.ESym x))) as T. cbn [tbl] in T. rewrite Ex in T.
           specialize (T eq_refl).
           pose proof (@A.ExUse b W p am t imp u (A.ESym x) A.NSName Hma Hin Eg Hu) as X.
           rewrite Ed in X. specialize (X eq_refl (or_introl T)). unfold local_of in El. rewrite El in X. exact X.
        -- pose proof (proj1 (Teq A.NSAlias (u_name u) (aent w A.NSAlias x))) as T. cbn [tbl] in T. rewrite Ex in T.
           specialize (T eq_refl).
           assert (Tn : A.w_exp W t A.NSName (u_name u) = None) by (rewrite (fin_exp t tj tm _ _ Htj Htm); cbn [tbl]; rewrite En; reflexivity).
           pose proof (@A.ExUse b W p am t imp u (aent w A.NSAlias x) A.NSName Hma Hin Eg Hu) as X.
           rewrite Ed in X. specialize (X eq_refl (or_intror (conj eq_refl (conj Tn T)))).
           unfold local_of in El. rewrite El in X. exact X.
  - intros ns n e0 Hx. inversion Hx as [p' s d ns' n' Hd Em Ef | p' am' t imp u e1 ns' Hma' Hin Eg Hu Ens Hor | p' am' t imp a Hma' Hin Ea]; subst.
    + exists s. split; [|destruct ns; [reflexivity | reflexivity | exfalso; exact (filed_not_alias _ _ Ef)]].
      apply (rep_exp _ _ _ Rp). left. apply (fin_own _ i ns n s Hi). exists d. auto.
    + rewrite Hma in Hma'. injection Hma' as <-.
      destruct (Ht t imp Hin) as (j & tm & Hj & Htm & Teq).
      assert (Hraw : forall x nsx, UseRaw tm u nsx x -> sassoc (local_of u) (tbl m true nsx) = Some x).
      { intros x nsx Hu'. apply (rep_exp _ _ _ Rp). right. exists t, imp, j, tm. split; [exact Hin|].
        split; [exact Hj|]. split; [exact Htm|]. right. split; [exact Eg|]. exists u. auto. }
      assert (Teq' : forall ns n e0, option_map (aent w ns) (sassoc n (tbl tm true ns)) = Some e0 <-> A.Exports b W t ns n e0)
        by (intros ns' n' e9; rewrite <- (fin_exp t j tm ns' n' Hj Htm); apply Teq).
      clear Teq. rename Teq' into Teq.
      destruct (u_dot u) eqn:Ed.
      * destruct Hor as [H|(E & _)]; [|discriminate].
        apply Teq in H. cbn [tbl] in H. destruct (sassoc (u_name u) (gm_exp_detached tm)) as [x|] eqn:Ex; [|discriminate].
        injection H as <-. exists x. split; [|reflexivity]. apply Hraw. left. auto.
      * destruct Hor as [H|(_ & En & H)].
        -- apply Teq in H. cbn [tbl] in H. destruct (sassoc (u_name u) (gm_exp_names tm)) as [x|] eqn:Ex; [|discriminate].
           injection H as <-. exists x. split; [|reflexivity]. apply Hraw. right. left. auto.
        -- apply Teq in H. cbn [tbl] in H. destruct (sassoc (u_name u) (gm_exp_aliases tm)) as [x|] eqn:Ex; [|discriminate].
           injection H as <-. exists x. split; [|reflexivity]. apply Hraw. right. right.
           split; [exact Ed|]. split; [reflexivity|]. split; [|exact Ex].
           rewrite (fin_exp t j tm _ _ Hj Htm) in En. cbn [tbl] in En.
           destruct (sassoc (u_name u) (gm_exp_names tm)); [discriminate | reflexivity].
    + rewrite Hma in Hma'. injection Hma' as <-.
      destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm & Hj & _ & Htm).
      exists j. split.
      * apply (rep_exp _ _ _ Rp). right. exists t, imp, j, tm. split; [exact Hin|]. split; [exact Hj|].
        split; [exact Htm|]. left. auto.
      * destruct (fin_path t j Hj) as (tm' & _ & _ & Gt & Pt & _). cbn [aent]. rewrite Gt, Pt. reflexivity.
Qed.

Lemma exports_iff : forall p i m am ns n e0, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.module_of b p = Some am -> (A.w_exp W p ns n = Some e0 <-> A.Exports b W p ns n e0).
Proof.
  intros p i m am ns n e0 Hi Hm Hma. destruct (exports_at i p m am Hi Hm Hma) as [T1 T2].
  rewrite (fin_exp p i m ns n Hi Hm). split.
  - intro H. destruct (sassoc n (tbl m true ns)) as [x|] eqn:Ex; [|discriminate]. injection H as <-. exact (T1 ns n x Ex).
  - intro H. destruct (T2 ns n e0 H) as (x & Ex & ->). rewrite Ex. reflexivity.
Qed.

Lemma scope_iff : forall p i m am ns n e0, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.module_of b p = Some am -> (A.w_scope W p ns n = Some e0 <-> A.InScope b W p ns n e0).
Proof.
  intros p i m am ns n e0 Hi Hm Hma.
  pose proof (li_tables _ _ HI p i m am Hi Hm Hma) as Rp.
  destruct (exports_at i p m am Hi Hm Hma) as [T1 T2].
  rewrite (fin_scope p i m ns n Hi Hm). split.
  - intro H. destruct (sassoc n (tbl m false ns)) as [x|] eqn:Ex; [|discriminate]. injection H as <-.
    apply (rep_scope _ _ _ Rp) in Ex as [Ho|[Hm'|(t & imp & tj & tm & Hin & Htj & Htm & Eg & Hx)]].
    + apply A.ScExported. apply T1. apply (rep_exp _ _ _ Rp). left. exact Ho.
    + apply A.ScExported. apply T1. apply (rep_exp _ _ _ Rp). right. exact Hm'.
    + destruct (fin_path t tj Htj) as (tm' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
      destruct (exports_at tj t tm amt Htj Htm Hamt) as [U1 _].
      eapply A.ScGlob; [exact Hma | exact Hin | exact Eg | exact (U1 ns n x Hx)].
  - intro H. inversion H as [p' ns' n' e' Hx | p' am' t imp ns' n' e' Hma' Hin Eg Hx]; subst.
    + destruct (T2 ns n e0 Hx) as (x & Ex & ->).
      assert (Es : sassoc n (tbl m false ns) = Some x)
        by (apply (rep_scope _ _ _ Rp); apply (rep_sub _ _ _ Rp); apply (rep_exp _ _ _ Rp); exact Ex).
      rewrite Es. reflexivity.
    + rewrite Hma in Hma'. injection Hma' as <-.
      destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm & Hj & _ & Htm).
      destruct (fin_path t j Hj) as (tm' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
      destruct (exports_at j t tm amt Hj Htm Hamt) as [_ U2].
      destruct (U2 ns n e0 Hx) as (x & Ex & ->).
      assert (Es : sassoc n (tbl m false ns) = Some x).
      { apply (rep_scope _ _ _ Rp). right. right. exists t, imp, j, tm. split; [exact Hin|].
        split; [exact Hj|]. split; [exact Htm|]. split; [exact Eg | exact Ex]. }
      rewrite Es. reflexivity.
Qed.

Lemma filed_none_attached : forall d, A.filed_name d = None -> A.kind_of d <> None -> is_attached d.
Proof.
  intros d Hf Hk. destruct d as [its inner| | | | | |[| |rn] n ps rt body| |]; cbn in Hf; try discriminate.
  - exfalso. apply Hk. reflexivity.
  - exists rn, n, ps, rt, body. reflexivity.
Qed.

Lemma final_reading : LoaderReading b W.
Proof.
  constructor.
  - (* acyclic *)
    exists (fun q => match sassoc q (ls_loaded st) with Some i => i | None => 0 end).
    intros p m t imp Hl Hm Hin. apply fin_loaded in Hl as (i & Hi).
    destruct (li_loaded _ _ HI p i Hi) as [_ (m' & am & _ & _ & Mm & Im)].
    rewrite Hm in Mm. injection Mm as <-. destruct (Im t imp Hin) as (j & Hj & Hlt).
    destruct (li_loaded _ _ HI t j Hj) as [_ (_ & amt & _ & _ & Hmt & _)].
    split; [rewrite Hmt; discriminate|].
    assert (Hs : forall q k, In (q, k) (ls_loaded st) -> sassoc q (ls_loaded st) = Some k).
    { intros q k Hq. destruct (sassoc q (ls_loaded st)) as [k'|] eqn:E.
      - f_equal. exact (loaded_fun _ _ _ _ _ HI (sassoc_in _ _ _ _ E) Hq).
      - exfalso. apply (sassoc_none _ _ _ E). apply in_map_iff. exists (q, k). auto. }
    rewrite (Hs t j Hj), (Hs p i Hi). exact Hlt.
  - (* declarations sound *)
    intros s d Hd. rewrite fin_decl in Hd. destruct (nget (ls_syms st) s) as [g|] eqn:Hs; [|discriminate].
    injection Hd as <-. destruct (li_syms _ _ HI s g Hs) as (Hpos & _).
    destruct (li_symfacts _ _ HI s g Hs) as (m & q & am & Em & Hq & Hma & Oc & Kd & _).
    split; [lia|]. split; [|exact Kd].
    destruct (fin_path q _ Hq) as (m' & _ & Em' & Gm & Pm & _). cbn [A.d_mod A.d_ast A.d_assumes].
    rewrite Gm, Pm. split; [apply fin_loaded; exists (gs_mod g); exact Hq|].
    exists am. split; [exact Hma | exact Oc].
  - (* declarations complete, one symbol each *)
    intros d [Hl (am & Hma & Oc)] Hk. apply fin_loaded in Hl as (i & Hi).
    destruct (fin_path _ i Hi) as (m & am' & Em & Gm & Pm & Hma'). rewrite Hma in Hma'. injection Hma' as <-.
    destruct (li_complete _ _ HI _ i m am Hi Em Hma _ _ Oc Hk) as (s & g & Hs & Egm & Ed & Ea).
    assert (Hdecl : forall s' g', nget (ls_syms st) s' = Some g' -> A.w_decl W s' = Some d ->
              gs_mod g' = i /\ gs_decl g' = A.d_ast d /\ gs_assumes g' = A.d_assumes d).
    { intros s' g' Hs' Hd'. rewrite fin_decl, Hs' in Hd'. injection Hd' as Hd'. subst d. cbn.
      split; [|auto]. destruct (li_syms _ _ HI s' g' Hs') as (_ & _ & _ & q & Hq).
      exact (fin_path_inj _ i Hi _ q Hq eq_refl). }
    exists s. split.
    + rewrite fin_decl, Hs. destruct d as [dm da das]. cbn in *. rewrite Egm, Gm, Pm, Ed, Ea. reflexivity.
    + intros s' Hd'. rewrite fin_decl in Hd'. destruct (nget (ls_syms st) s') as [g'|] eqn:Hs'; [|discriminate].
      assert (Hd'' : A.w_decl W s' = Some d) by (rewrite fin_decl, Hs'; exact Hd').
      destruct (Hdecl s' g' Hs' Hd'') as (Em' & Ed' & Ea').
      destruct (li_symfacts _ _ HI s g Hs) as (m1 & _ & _ & Em1 & _ & _ & _ & _ & Fn & Fd & Fa).
      destruct (li_symfacts _ _ HI s' g' Hs') as (m2 & _ & _ & Em2 & _ & _ & _ & _ & Fn' & Fd' & Fa').
      rewrite Egm in Em1. rewrite Em' in Em2. rewrite Em in Em1, Em2. injection Em1 as <-. injection Em2 as <-.
      rewrite Ed' in Fn', Fd', Fa'. rewrite Ed in Fn, Fd, Fa.
      destruct (A.filed_name (A.d_ast d)) as [[[| |] n]|] eqn:Ef.
      * pose proof (Fn n eq_refl). pose proof (Fn' n eq_refl). congruence.
      * pose proof (Fd n eq_refl). pose proof (Fd' n eq_refl). congruence.
      * exfalso. exact (filed_not_alias _ _ Ef).
      * destruct (filed_none_attached _ Ef Hk) as (rn & n & ps & rt & body & Eatt).
        destruct (Fa rn n ps rt body Eatt) as (r & _ & Er & _ & _ & Ear).
        destruct (Fa' rn n ps rt body Eatt) as (r' & _ & Er' & _ & _ & Ear').
        rewrite Er in Er'. injection Er' as <-. congruence.
  - (* exports *)
    intros p ns n e0 Hl. apply fin_loaded in Hl as (i & Hi).
    destruct (fin_path p i Hi) as (m & am & Em & _ & _ & Hma). exact (exports_iff p i m am ns n e0 Hi Em Hma).
  - (* scopes *)
    intros p ns n e0 Hl. apply fin_loaded in Hl as (i & Hi).
    destruct (fin_path p i Hi) as (m & am & Em & _ & _ & Hma). exact (scope_iff p i m am ns n e0 Hi Em Hma).
  - (* attached methods *)
    intros r n s. split.
    + intro H. cbn [A.w_attached W W_read] in H.
      destruct (nget (w_syms w) r) as [gr|] eqn:Hr'; [|discriminate]. cbn [w_syms w] in Hr'.
      destruct (fin_sym_mod r gr Hr') as (q & m & am & Hq & Em & Gm & Pm & Hma). rewrite Gm in H.
      destruct (li_attached _ _ HI _ m r n s Em H) as (g & rn & ps & rt & body & Hs & Egm & Ed & Er).
      exists (A.Decl (gm_path (get_mod w (gs_mod g))) (gs_decl g) (gs_assumes g)), rn, ps, rt, body.
      split; [rewrite fin_decl, Hs; reflexivity|]. cbn [A.d_mod A.d_ast]. split; [exact Ed|].
      unfold A.mod_of. rewrite fin_decl, Hr'. cbn [A.d_mod]. rewrite Egm. split; [reflexivity|].
      rewrite Gm, Pm. rewrite (fin_scope q _ m _ rn Hq Em). cbn [tbl]. rewrite Er. reflexivity.
    + intros (d & rn & ps & rt & body & Hd & Ea & Em & Hsc).
      rewrite fin_decl in Hd. destruct (nget (ls_syms st) s) as [g|] eqn:Hs; [|discriminate].
      injection Hd as <-. cbn [A.d_ast A.d_mod] in Ea, Em, Hsc.
      destruct (li_symfacts _ _ HI s g Hs) as (m & q & am & Emm & Hq & _ & _ & _ & _ & _ & Fa).
      destruct (Fa rn n ps rt body Ea) as (r' & gr & Er & Hgr & Egr & Ear).
      rewrite (fin_get_mod _ _ Emm) in Hsc.
      destruct (fin_path q _ Hq) as (m' & _ & Em' & _ & Pm & _). rewrite Emm in Em'. injection Em' as <-.
      rewrite Pm in Hsc. rewrite (fin_scope q _ m _ rn Hq Emm) in Hsc. cbn [tbl] in Hsc. rewrite Er in Hsc.
      injection Hsc as <-.
      cbn [A.w_attached W W_read w_syms w]. rewrite Hgr. rewrite Egr, (fin_get_mod _ _ Emm). exact Ear.
Qed.

(* ---- name resolution: the elaborator's resolve_path is the relation's ---- *)

Lemma alias_loaded : forall i p m am, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.module_of b p = Some am ->
  forall ex n x, sassoc n (tbl m ex A.NSAlias) = Some x -> exists q, In (q, x) (ls_loaded st).
Proof.
  intro i. induction i as [i IH] using lt_wf_ind. intros p m am Hi Hm Hma ex n x Hx.
  pose proof (li_tables _ _ HI p i m am Hi Hm Hma) as Rp.
  assert (Hs : sassoc n (tbl m false A.NSAlias) = Some x)
    by (destruct ex; [apply (rep_scope _ _ _ Rp), (rep_sub _ _ _ Rp), (rep_exp _ _ _ Rp); exact Hx | exact Hx]).
  apply (rep_scope _ _ _ Rp) in Hs as [(g & _ & _ & Hf)|[(t & imp & tj & tm & Hin & Htj & Htm & Hq9)|(t & imp & tj & tm & Hin & Htj & Htm & _ & Hq9)]].
  - exfalso. exact (filed_not_alias _ _ Hf).
  - destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm' & Hj & Hlt & _).
    pose proof (loaded_fun _ _ _ _ _ HI Htj Hj) as ->.
    destruct (fin_path t j Hj) as (tm'' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
    destruct Hq9 as [(_ & _ & ->)|(_ & u & _ & _ & [(_ & E & _)|[(_ & E & _)|(_ & _ & _ & Ex)]])]; try discriminate.
    + exists t. exact Hj.
    + exact (IH j Hlt t tm amt Hj Htm Hamt true (u_name u) x Ex).
  - destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm' & Hj & Hlt & _).
    pose proof (loaded_fun _ _ _ _ _ HI Htj Hj) as ->.
    destruct (fin_path t j Hj) as (tm'' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
    exact (IH j Hlt t tm amt Hj Htm Hamt true n x Hq9).
Qed.

Definition conv_ent (e0 : Elab.ent) : A.ent :=
  match e0 with Elab.ESym s => A.ESym s | Elab.EMod j => A.EMod (gm_path (get_mod w j)) end.

Definition go_res (m : gmod) :=
  fix go (cur : Elab.ent) (hd : string) (segs : list string) : res (option Elab.ent) :=
    match segs with
    | [] => Good (Some cur)
    | seg :: r =>
        match cur with
        | Elab.ESym _ => err_in m ("`" ++ hd ++ "` is not a module")%string
        | Elab.EMod ci =>
            let cm := get_mod w ci in
            match sassoc seg (gm_exp_names cm) with
            | Some s => go (Elab.ESym s) seg r
            | None =>
                match sassoc seg (gm_exp_aliases cm) with
                | Some s => go (Elab.EMod s) seg r
                | None => err_in m ("`" ++ gm_path cm ++ "` does not export `" ++ seg ++ "`")%string
                end
            end
        end
    end.

Lemma go_res_agrees : forall m segs cur hd r,
  (match cur with Elab.EMod j => exists q, In (q, j) (ls_loaded st) | _ => True end) ->
  go_res m cur hd segs = Good r -> A.resolve_rest W (conv_ent cur) segs = option_map conv_ent r.
Proof.
  intros m segs. induction segs as [|seg rest IH]; intros cur hd r Hc H.
  - injection H as <-. reflexivity.
  - cbn [go_res] in H. destruct cur as [s|j]; [unfold err_in in H; discriminate|].
    destruct Hc as (q & Hq). destruct (fin_path q j Hq) as (cm & amq & Ecm & Gcm & Pcm & Hamq).
    cbn [A.resolve_rest conv_ent]. rewrite Gcm in H |- *. rewrite Pcm.
    rewrite (fin_exp q j cm A.NSName seg Hq Ecm), (fin_exp q j cm A.NSAlias seg Hq Ecm). cbn [tbl aent].
    destruct (sassoc seg (gm_exp_names cm)) as [s|] eqn:En.
    + cbn [option_map]. apply (IH (Elab.ESym s) seg r I H).
    + destruct (sassoc seg (gm_exp_aliases cm)) as [k|] eqn:Ea; [|unfold err_in in H; discriminate].
      cbn [option_map]. change (A.EMod (gm_path (get_mod w k))) with (conv_ent (Elab.EMod k)).
      apply (IH (Elab.EMod k) seg r); [|exact H].
      exact (alias_loaded j q cm amq Hq Ecm Hamq true seg k Ea).
Qed.

(* Whatever the elaborator resolves a path to in a loaded module, the
   relation's `resolve` resolves it to (and where the elaborator finds
   nothing, so does the relation). *)
Theorem resolve_path_agrees : forall p i m pa r, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  Elab.resolve_path w i pa = Good r -> A.resolve W p pa = option_map conv_ent r.
Proof.
  intros p i m pa r Hi Hm H. destruct (fin_path p i Hi) as (m' & am & Em & Gm & Pm & Hma).
  rewrite Hm in Em. injection Em as <-.
  unfold Elab.resolve_path in H. rewrite Gm in H. destruct pa as [|hd rest]; [injection H as <-; reflexivity|].
  cbn [A.resolve]. rewrite (fin_scope p i m A.NSName hd Hi Hm), (fin_scope p i m A.NSAlias hd Hi Hm). cbn [tbl aent].
  destruct (sassoc hd (gm_names m)) as [s|] eqn:En.
  - cbn [option_map]. change (A.ESym s) with (conv_ent (Elab.ESym s)).
    exact (go_res_agrees m rest (Elab.ESym s) hd r I H).
  - destruct (sassoc hd (gm_aliases m)) as [k|] eqn:Ea; cbn [option_map] in H |- *.
    + change (A.EMod (gm_path (get_mod w k))) with (conv_ent (Elab.EMod k)).
      apply (go_res_agrees m rest (Elab.EMod k) hd r); [|exact H].
      exact (alias_loaded i p m am Hi Hm Hma false hd k Ea).
    + injection H as <-. reflexivity.
Qed.

(* ---- requirements of a val/fn/method/context/functor: assume_type_syms ---- *)

Lemma sym_entries : forall i p m am, In (p, i) (ls_loaded st) -> nget (ls_mods st) i = Some m ->
  A.module_of b p = Some am ->
  forall ex ns n x, ns <> A.NSAlias -> sassoc n (tbl m ex ns) = Some x -> exists g, nget (ls_syms st) x = Some g.
Proof.
  intro i. induction i as [i IH] using lt_wf_ind. intros p m am Hi Hm Hma ex ns n x Hns Hx.
  pose proof (li_tables _ _ HI p i m am Hi Hm Hma) as Rp.
  assert (Hs : sassoc n (tbl m false ns) = Some x)
    by (destruct ex; [apply (rep_scope _ _ _ Rp), (rep_sub _ _ _ Rp), (rep_exp _ _ _ Rp); exact Hx | exact Hx]).
  apply (rep_scope _ _ _ Rp) in Hs as [(g & Hg & _)|[(t & imp & tj & tm & Hin & Htj & Htm & Hq9)|(t & imp & tj & tm & Hin & Htj & Htm & _ & Hq9)]].
  - exists g. exact Hg.
  - destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm' & Hj & Hlt & _).
    pose proof (loaded_fun _ _ _ _ _ HI Htj Hj) as ->.
    destruct (fin_path t j Hj) as (tm'' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
    destruct Hq9 as [(_ & E & _)|(_ & u & _ & _ & [(_ & -> & Ex)|[(_ & -> & Ex)|(_ & E & _)]])];
      try (exfalso; exact (Hns E)).
    + exact (IH j Hlt t tm amt Hj Htm Hamt true A.NSDetached (u_name u) x ltac:(discriminate) Ex).
    + exact (IH j Hlt t tm amt Hj Htm Hamt true A.NSName (u_name u) x ltac:(discriminate) Ex).
  - destruct (fin_imports p i am t imp Hi Hma Hin) as (j & tm' & Hj & Hlt & _).
    pose proof (loaded_fun _ _ _ _ _ HI Htj Hj) as ->.
    destruct (fin_path t j Hj) as (tm'' & amt & Etm & _ & _ & Hamt). rewrite Htm in Etm. injection Etm as <-.
    exact (IH j Hlt t tm amt Hj Htm Hamt true ns n x Hns Hq9).
Qed.

Lemma find_mod_loaded : forall p cm, find_mod w p (w_order w) = Some cm ->
  exists j q am, In (q, j) (ls_loaded st) /\ nget (ls_mods st) j = Some cm /\ A.module_of b q = Some am.
Proof.
  intros p cm. cbn [w_order w].
  assert (Hall : forall j, In j (ls_order st) -> exists q, In (q, j) (ls_loaded st)) by (intros j Hj; apply (li_order _ _ HI); exact Hj).
  revert Hall. generalize (ls_order st). intro l. induction l as [|j r IHl]; intros Hall H; [discriminate|].
  cbn [find_mod] in H. destruct (String.eqb (gm_path (get_mod w j)) p).
  - injection H as <-. destruct (Hall j (or_introl eq_refl)) as (q & Hq).
    destruct (fin_path q j Hq) as (m & am & Em & Gm & _ & Hma). exists j, q, am. rewrite Gm. auto.
  - apply IHl; [intros j' Hj'; apply Hall; right; exact Hj' | exact H].
Qed.

Lemma table_esym : forall (tab : string -> A.nspace -> name -> option A.ent) ex p ns n x,
  (forall p ns n, tab p ns n = match find_mod w p (w_order w) with
                               | Some m => option_map (aent w ns) (sassoc n (tbl m ex ns)) | None => None end) ->
  tab p ns n = Some (A.ESym x) -> exists g, nget (ls_syms st) x = Some g.
Proof.
  intros tab ex p ns n x Ht H. rewrite Ht in H.
  destruct (find_mod w p (w_order w)) as [cm|] eqn:Ef; [|discriminate].
  destruct (find_mod_loaded p cm Ef) as (j & q & am & Hq & Ej & Hma).
  destruct (sassoc n (tbl cm ex ns)) as [y|] eqn:Ey; [|discriminate]. cbn in H.
  destruct ns; cbn in H; try discriminate; injection H as ->.
  - exact (sym_entries j q cm am Hq Ej Hma ex A.NSName n x ltac:(discriminate) Ey).
  - exact (sym_entries j q cm am Hq Ej Hma ex A.NSDetached n x ltac:(discriminate) Ey).
Qed.

Lemma resolved_loader : forall p pa x, A.resolve W p pa = Some (A.ESym x) -> exists g, nget (ls_syms st) x = Some g.
Proof.
  assert (Hrest : forall segs e0 x, A.resolve_rest W e0 segs = Some (A.ESym x) ->
            e0 = A.ESym x \/ exists t ns seg, A.w_exp W t ns seg = Some (A.ESym x)).
  { induction segs as [|seg r IH]; intros e0 x H; cbn [A.resolve_rest] in H.
    - injection H as ->. left. reflexivity.
    - destruct e0 as [s0|t]; [discriminate|]. right.
      destruct (A.w_exp W t A.NSName seg) as [e1|] eqn:E1.
      + destruct (IH e1 x H) as [->|X]; [exists t, A.NSName, seg; exact E1 | exact X].
      + destruct (A.w_exp W t A.NSAlias seg) as [e1|] eqn:E2; [|discriminate].
        destruct (IH e1 x H) as [->|X]; [exists t, A.NSAlias, seg; exact E2 | exact X]. }
  intros p [|hd rest] x H; cbn [A.resolve] in H; [discriminate|].
  assert (Hsc : forall ns, A.w_scope W p ns hd = Some (A.ESym x) -> exists g, nget (ls_syms st) x = Some g)
    by (intros ns E; exact (table_esym (A.w_scope W) false p ns hd x (fun _ _ _ => eq_refl) E)).
  assert (Hex : forall t ns seg, A.w_exp W t ns seg = Some (A.ESym x) -> exists g, nget (ls_syms st) x = Some g)
    by (intros t ns seg E; exact (table_esym (A.w_exp W) true t ns seg x (fun _ _ _ => eq_refl) E)).
  destruct (A.w_scope W p A.NSName hd) as [e1|] eqn:E1.
  - destruct (Hrest rest e1 x H) as [->|(t & ns & seg & X)]; [exact (Hsc _ E1) | exact (Hex _ _ _ X)].
  - destruct (A.w_scope W p A.NSAlias hd) as [e1|] eqn:E2; [|discriminate].
    destruct (Hrest rest e1 x H) as [->|(t & ns & seg & X)]; [exact (Hsc _ E2) | exact (Hex _ _ _ X)].
Qed.

(* the states elaboration reaches keep the loader's world and symbols *)
Definition GS (s : Elab.st) : Prop :=
  st_w s = w /\ forall i g, nget (ls_syms st) i = Some g -> nget (st_syms s) i = Some g.

Definition pathi (mi : nat) : string := gm_path (get_mod w mi).

Lemma esym_resolve : forall mi pa s r s', GS s -> In mi (ls_order st) ->
  Elab.resolve_sym mi pa s = Good (r, s') ->
  s' = s /\ match r with
            | Some g => A.resolve_sym W (pathi mi) pa = Some (gs_id g) /\ nget (ls_syms st) (gs_id g) = Some g
            | None => A.resolve_sym W (pathi mi) pa = None
            end.
Proof.
  intros mi pa s r s' [Gw Gk] Hmi H. unfold Elab.resolve_sym in H.
  apply bind_inv in H as (r0 & s1 & H1 & H). unfold rpath in H1. rewrite Gw in H1.
  destruct (Elab.resolve_path w mi pa) as [r1|m] eqn:Er; [|discriminate]. injection H1 as -> E2. subst s1.
  destruct (proj1 (li_order _ _ HI mi) Hmi) as (q & Hq).
  destruct (fin_path q mi Hq) as (m & am & Em & Gm & Pm & _).
  pose proof (resolve_path_agrees q mi m pa r0 Hq Em Er) as Ra.
  assert (Hp : pathi mi = q) by (unfold pathi; rewrite Gm; exact Pm).
  unfold A.resolve_sym. rewrite Hp, Ra.
  destruct r0 as [[x|j]|].
  - apply bind_inv in H as (g & s2 & H2 & H). unfold sym in H2.
    destruct (nget (st_syms s) x) as [g'|] eqn:Ex; [|discriminate]. injection H2 as <- <-. injection H as <- <-.
    split; [reflexivity|].
    destruct (resolved_loader q pa x ltac:(rewrite Ra; reflexivity)) as (g0 & Hg0).
    rewrite (Gk x g0 Hg0) in Ex. injection Ex as <-.
    destruct (li_syms _ _ HI x g0 Hg0) as (_ & _ & Hid & _). rewrite Hid. auto.
  - injection H as <- <-. auto.
  - injection H as <- <-. auto.
Qed.

Lemma kconv_type : forall k, kconv k = A.DKType -> k = KType.
Proof. intros []; cbn; congruence. Qed.
Lemma kconv_ctx : forall k, kconv k = A.DKCtx -> k = KCtx.
Proof. intros []; cbn; congruence. Qed.

Lemma loader_decl : forall x g, nget (ls_syms st) x = Some g ->
  A.w_decl W x = Some (A.Decl (pathi (gs_mod g)) (gs_decl g) (gs_assumes g)) /\
  A.kind_sym W x = Some (kconv (gs_kind g)) /\ In (gs_mod g) (ls_order st).
Proof.
  intros x g Hg. assert (D : A.w_decl W x = Some (A.Decl (pathi (gs_mod g)) (gs_decl g) (gs_assumes g)))
    by (rewrite fin_decl, Hg; reflexivity).
  split; [exact D|]. split; [unfold A.kind_sym; rewrite D; exact (li_kind _ _ HI x g Hg)|].
  destruct (li_syms _ _ HI x g Hg) as (_ & _ & _ & q & Hq). apply (li_order _ _ HI). exists q. exact Hq.
Qed.

(* the property of a walker of a context's items, at module mi *)
Definition WalkOk (wk : nat -> list spec -> list nat -> Elab.M (list nat)) : Prop :=
  forall mi its out s l s', GS s -> In mi (ls_order st) -> NoDup out -> wk mi its out s = Good (l, s') ->
    s' = s /\ NoDup l /\ incl out l /\
    (forall t, In t l -> In t out \/ exists sp, In sp its /\ A.SpecTypes W (pathi mi) sp t) /\
    (forall sp t, In sp its -> A.SpecTypes W (pathi mi) sp t -> In t l).

Definition wstep (wk : nat -> list spec -> list nat -> Elab.M (list nat)) (mi : nat) (p : path) (out : list nat)
    : Elab.M (list nat) :=
  let! tg := Elab.resolve_sym mi p in
  match tg with
  | Some g =>
      if kind_eqb (gs_kind g) KType then ret (if nmem (gs_id g) out then out else out ++ [gs_id g])
      else if kind_eqb (gs_kind g) KCtx then
        match gs_decl g with
        | ADCtx _ its => wk (gs_mod g) its out
        | _ => ret out
        end
      else ret out
  | None => ret out
  end.

Lemma nmem_in : forall x l, nmem x l = true <-> In x l.
Proof.
  intros x l. induction l as [|y r IH]; cbn; [split; [discriminate | intros []]|].
  rewrite orb_true_iff, Nat.eqb_eq, IH. split; intros [H|H]; auto.
Qed.

Lemma kind_eqb_spec : forall a c, kind_eqb a c = true <-> a = c.
Proof. intros [] []; cbn; split; intro H; congruence. Qed.

Lemma wstep_ok : forall wk, WalkOk wk -> forall mi p out s l s', GS s -> In mi (ls_order st) -> NoDup out ->
  wstep wk mi p out s = Good (l, s') ->
  s' = s /\ NoDup l /\ incl out l /\
  (forall t, In t l -> In t out \/ forall a, A.SpecTypes W (pathi mi) (Spec (Some p) None a) t) /\
  (forall a t, A.SpecTypes W (pathi mi) (Spec (Some p) None a) t -> In t l).
Proof.
  intros wk Hwk mi p out s l s' Hgs Hmi Hnd H. unfold wstep in H.
  apply bind_inv in H as (r & s1 & H1 & H). destruct (esym_resolve mi p s r s1 Hgs Hmi H1) as [-> Hr0].
  destruct r as [g|].
  - destruct Hr0 as [Er Hg]. destruct (loader_decl _ g Hg) as (Dg & Kg & Og).
    destruct (kind_eqb (gs_kind g) KType) eqn:Kt.
    + apply kind_eqb_spec in Kt. injection H as <- <-.
      assert (Hin : In (gs_id g) (if nmem (gs_id g) out then out else out ++ [gs_id g])).
      { destruct (nmem (gs_id g) out) eqn:Em; [apply nmem_in; exact Em | apply in_or_app; right; left; reflexivity]. }
      split; [reflexivity|]. split.
      { destruct (nmem (gs_id g) out) eqn:Em; [exact Hnd|]. apply NoDup_app; [exact Hnd | repeat constructor; auto|].
        intros x Hx [<-|[]]. assert (nmem (gs_id g) out = true) by (apply nmem_in; exact Hx). congruence. }
      split; [intros x Hx; destruct (nmem (gs_id g) out); [exact Hx | apply in_or_app; left; exact Hx]|].
      split.
      * intros t Ht. destruct (nmem (gs_id g) out); [left; exact Ht|].
        apply in_app_or in Ht as [Ht|[<-|[]]]; [left; exact Ht|]. right. intro a.
        apply A.STType; [exact Er | rewrite Kg, Kt; reflexivity].
      * intros a t Hst. inversion Hst as [p0 pa a0 t0 Er' Kt' | p0 pa a0 c d n its sp t0 Er' Dc Ec Hsp Hsub]; subst.
        -- rewrite Er in Er'. injection Er' as <-. exact Hin.
        -- rewrite Er in Er'. injection Er' as <-. rewrite Dg in Dc. injection Dc as <-. cbn in Ec.
           exfalso. pose proof (li_kind _ _ HI _ g Hg) as Kd. unfold KindOK in Kd. rewrite Ec, Kt in Kd. discriminate.
    + destruct (kind_eqb (gs_kind g) KCtx) eqn:Kc.
      * apply kind_eqb_spec in Kc. pose proof (li_kind _ _ HI _ g Hg) as Kd. unfold KindOK in Kd. rewrite Kc in Kd.
        destruct (gs_decl g) as [| | | | | | [| |] ? ? ? ? | n its |] eqn:Ed; cbn in Kd; try discriminate.
        destruct (Hwk (gs_mod g) its out s l s' Hgs Og Hnd H) as (E1 & E2 & E3 & E4 & E5).
        split; [exact E1|]. split; [exact E2|]. split; [exact E3|]. split.
        -- intros t Ht. destruct (E4 t Ht) as [Ho|(sp & Hsp & Hst)]; [left; exact Ho|]. right. intro a.
           eapply A.STCtx; [exact Er | exact Dg | reflexivity | exact Hsp | exact Hst].
        -- intros a t Hst. inversion Hst as [p0 pa a0 t0 Er' Kt' | p0 pa a0 c d n' its' sp t0 Er' Dc Ec Hsp Hsub]; subst.
           ++ rewrite Er in Er'. injection Er' as <-. rewrite Kg, Kc in Kt'. discriminate.
           ++ rewrite Er in Er'. injection Er' as <-. rewrite Dg in Dc. injection Dc as <-. cbn in Ec, Hsub.
              try rewrite Ed in Ec. injection Ec as <- <-. exact (E5 sp t Hsp Hsub).
      * injection H as <- <-. split; [reflexivity|]. split; [exact Hnd|]. split; [intros x Hx; exact Hx|].
        split; [intros t Ht; left; exact Ht|].
        intros a t Hst. exfalso. inversion Hst as [p0 pa a0 t0 Er' Kt' | p0 pa a0 c d n' its' sp t0 Er' Dc Ec Hsp Hsub]; subst.
        -- rewrite Er in Er'. injection Er' as <-. rewrite Kg in Kt'. injection Kt' as Kt'.
           apply kconv_type in Kt'. rewrite Kt' in Kt. discriminate.
        -- rewrite Er in Er'. injection Er' as <-. rewrite Dg in Dc. injection Dc as <-. cbn in Ec.
           pose proof (li_kind _ _ HI _ g Hg) as Kd. unfold KindOK in Kd. rewrite Ec in Kd. injection Kd as Kd.
           symmetry in Kd. apply kconv_ctx in Kd. rewrite Kd in Kc. discriminate.
  - injection H as <- <-. split; [reflexivity|]. split; [exact Hnd|]. split; [intros x Hx; exact Hx|].
    split; [intros t Ht; left; exact Ht|].
    intros a t Hst. exfalso. inversion Hst; subst; congruence.
Qed.

(* a fold of steps over a list, each answering its element's types *)
Lemma fold_walk : forall X (step : list nat -> X -> Elab.M (list nat)) (P : X -> nat -> Prop),
  (forall x out s l s', GS s -> NoDup out -> step out x s = Good (l, s') ->
     s' = s /\ NoDup l /\ incl out l /\ (forall t, In t l -> In t out \/ P x t) /\ (forall t, P x t -> In t l)) ->
  forall xs out s l s', GS s -> NoDup out -> mfold step out xs s = Good (l, s') ->
  s' = s /\ NoDup l /\ incl out l /\ (forall t, In t l -> In t out \/ exists x, In x xs /\ P x t) /\
  (forall x t, In x xs -> P x t -> In t l).
Proof.
  intros X step P Hs xs. induction xs as [|x r IH]; intros out s l s' Hg Hnd H; cbn [mfold] in H.
  - injection H as <- <-. split; [reflexivity|]. split; [exact Hnd|]. split; [intros y Hy; exact Hy|].
    split; [intros t Ht; left; exact Ht | intros x t []].
  - apply bind_inv in H as (o1 & s1 & H1 & H2).
    destruct (Hs x out s o1 s1 Hg Hnd H1) as (-> & N1 & I1 & S1 & C1).
    destruct (IH o1 s l s' Hg N1 H2) as (-> & N2 & I2 & S2 & C2).
    split; [reflexivity|]. split; [exact N2|]. split; [intros y Hy; apply I2, I1, Hy|]. split.
    + intros t Ht. destruct (S2 t Ht) as [Ho|(y & Hy & Py)].
      * destruct (S1 t Ho) as [Ho'|Px]; [left; exact Ho' | right; exists x; split; [left; reflexivity | exact Px]].
      * right. exists y. split; [right; exact Hy | exact Py].
    + intros y t [<-|Hy] Py; [apply I2, C1, Py | exact (C2 y t Hy Py)].
Qed.

(* ---- the walk of an assume nesting: each context entered once (Audit
   note 30), so the walk is exact for nestings of any depth, cycles
   included ---- *)

(* the item sp, written in module q, names the type t, or the context c *)
Definition Direct (q : string) (sp : spec) (t : nat) : Prop :=
  exists pa a, sp = Spec (Some pa) None a /\ A.resolve_sym W q pa = Some t /\ A.kind_sym W t = Some A.DKType.

Definition DirCtx (q : string) (sp : spec) (c : nat) : Prop :=
  exists pa a d n its, sp = Spec (Some pa) None a /\ A.resolve_sym W q pa = Some c /\
    A.w_decl W c = Some d /\ A.d_ast d = ADCtx n its.

(* every item of context c has its type in out and its context in V *)
Definition Finished (V out : list nat) (c : nat) : Prop :=
  forall d n its, A.w_decl W c = Some d -> A.d_ast d = ADCtx n its -> forall sp, In sp its ->
    (forall t, Direct (A.d_mod d) sp t -> In t out) /\ (forall c', DirCtx (A.d_mod d) sp c' -> In c' V).

Lemma finished_mono : forall V out V' out' c, incl V V' -> incl out out' -> Finished V out c -> Finished V' out' c.
Proof.
  intros V out V' out' c Iv Io H d n its Hd Ha sp Hsp. destruct (H d n its Hd Ha sp Hsp) as [H1 H2].
  split; [intros t Ht; apply Io, H1, Ht | intros c' Hc; apply Iv, H2, Hc].
Qed.

(* the walk from (V, out) over items its, in module q, ends at (V', out') *)
Definition WalkPost (q : string) (its : list spec) (V out V' out' : list nat) : Prop :=
  NoDup out' /\ incl out out' /\ incl V V' /\
  (forall t, In t out' -> In t out \/ exists sp, In sp its /\ A.SpecTypes W q sp t) /\
  (forall sp t, In sp its -> Direct q sp t -> In t out') /\
  (forall sp c, In sp its -> DirCtx q sp c -> In c V') /\
  (forall c, In c V' -> ~ In c V -> Finished V' out' c).

Lemma fold_walk2 : forall X (step : list nat * list nat -> X -> Elab.M (list nat * list nat))
    (P D K : X -> nat -> Prop),
  (forall x V out s V' out' s', GS s -> NoDup out -> step (V, out) x s = Good ((V', out'), s') ->
     s' = s /\ NoDup out' /\ incl out out' /\ incl V V' /\ (forall t, In t out' -> In t out \/ P x t) /\
     (forall t, D x t -> In t out') /\ (forall c, K x c -> In c V') /\
     (forall c, In c V' -> ~ In c V -> Finished V' out' c)) ->
  forall xs V out s V' out' s', GS s -> NoDup out -> mfold step (V, out) xs s = Good ((V', out'), s') ->
  s' = s /\ NoDup out' /\ incl out out' /\ incl V V' /\
  (forall t, In t out' -> In t out \/ exists x, In x xs /\ P x t) /\
  (forall x t, In x xs -> D x t -> In t out') /\ (forall x c, In x xs -> K x c -> In c V') /\
  (forall c, In c V' -> ~ In c V -> Finished V' out' c).
Proof.
  intros X step P D K Hs xs. induction xs as [|x r IH]; intros V out s V' out' s' Hg Hnd H; cbn [mfold] in H.
  - injection H as <- <- <-. split; [reflexivity|]. split; [exact Hnd|]. split; [intros y Hy; exact Hy|].
    split; [intros y Hy; exact Hy|]. split; [intros t Ht; left; exact Ht|].
    split; [intros x t []|]. split; [intros x c []|]. intros c Hc Hn. contradiction.
  - apply bind_inv in H as ([V1 o1] & s1 & H1 & H2).
    destruct (Hs x V out s V1 o1 s1 Hg Hnd H1) as (-> & N1 & Io1 & Iv1 & S1 & D1 & K1 & F1).
    destruct (IH V1 o1 s V' out' s' Hg N1 H2) as (-> & N2 & Io2 & Iv2 & S2 & D2 & K2 & F2).
    split; [reflexivity|]. split; [exact N2|]. split; [intros y Hy; apply Io2, Io1, Hy|].
    split; [intros y Hy; apply Iv2, Iv1, Hy|]. split.
    + intros t Ht. destruct (S2 t Ht) as [Ho|(y & Hy & Py)].
      * destruct (S1 t Ho) as [Ho'|Px]; [left; exact Ho' | right; exists x; split; [left; reflexivity | exact Px]].
      * right. exists y. split; [right; exact Hy | exact Py].
    + split; [intros y t [<-|Hy] Py; [apply Io2, D1, Py | exact (D2 y t Hy Py)]|].
      split; [intros y c [<-|Hy] Py; [apply Iv2, K1, Py | exact (K2 y c Hy Py)]|].
      intros c Hc Hn. destruct (in_dec Nat.eq_dec c V1) as [H1c|H1c].
      * exact (finished_mono _ _ _ _ c Iv2 Io2 (F1 c H1c Hn)).
      * exact (F2 c Hc H1c).
Qed.

Lemma direct_sym : forall q pa a t g, A.resolve_sym W q pa = Some (gs_id g) -> nget (ls_syms st) (gs_id g) = Some g ->
  Direct q (Spec (Some pa) None a) t -> t = gs_id g /\ gs_kind g = KType.
Proof.
  intros q pa a t g Er Hg (pa' & a' & E & Er' & Kt). injection E as <- <-. rewrite Er in Er'. injection Er' as <-.
  split; [reflexivity|]. destruct (loader_decl _ g Hg) as (_ & Kg & _). rewrite Kg in Kt. injection Kt as Kt.
  exact (kconv_type _ Kt).
Qed.

Lemma dirctx_sym : forall q pa a c g, A.resolve_sym W q pa = Some (gs_id g) -> nget (ls_syms st) (gs_id g) = Some g ->
  DirCtx q (Spec (Some pa) None a) c -> c = gs_id g /\ gs_kind g = KCtx /\ exists n its, gs_decl g = ADCtx n its.
Proof.
  intros q pa a c g Er Hg (pa' & a' & d & n & its & E & Er' & Dd & Ad). injection E as <- <-.
  rewrite Er in Er'. injection Er' as <-. split; [reflexivity|].
  destruct (loader_decl _ g Hg) as (Dg & _ & _). rewrite Dg in Dd. injection Dd as <-. cbn in Ad.
  pose proof (li_kind _ _ HI _ g Hg) as Kd. unfold KindOK in Kd. rewrite Ad in Kd. injection Kd as Kd.
  split; [symmetry in Kd; exact (kconv_ctx _ Kd) | exists n, its; exact Ad].
Qed.

Lemma walk_ok : forall fuel mi its V out s V' out' s', GS s -> In mi (ls_order st) -> NoDup out ->
  walk_specs fuel mi its (V, out) s = Good ((V', out'), s') -> s' = s /\ WalkPost (pathi mi) its V out V' out'.
Proof.
  induction fuel as [|f IH]; intros mi its V out s V' out' s' Hg Hmi Hnd H; [discriminate|].
  cbn [walk_specs] in H.
  match type of H with mfold ?stp _ _ _ = _ =>
    pose proof (fold_walk2 spec stp (fun sp t => A.SpecTypes W (pathi mi) sp t) (Direct (pathi mi)) (DirCtx (pathi mi)))
      as FW
  end.
  lapply FW; [intro FW'; destruct (FW' its V out s V' out' s' Hg Hnd H) as (E1 & N & Io & Iv & S & D & K & Fn);
              split; [exact E1 | exact (conj N (conj Io (conj Iv (conj S (conj D (conj K Fn))))))] | clear FW].
  intros [[p|] [n|] a] V0 o0 s0 V1 o1 s1 Hg0 Hnd0 H0; cbn beta iota in H0;
    try (injection H0 as <- <- <-; split; [reflexivity|]; split; [exact Hnd0|]; split; [intros y Hy; exact Hy|];
         split; [intros y Hy; exact Hy|]; split; [intros t Ht; left; exact Ht|];
         split; [intros t (pa & a' & E & _); discriminate E|];
         split; [intros c (pa & a' & d & n' & its' & E & _); discriminate E|]; intros c Hc Hn; contradiction).
  apply bind_inv in H0 as (r & s2 & H1 & H0). destruct (esym_resolve mi p s0 r s2 Hg0 Hmi H1) as [-> Hr0].
  destruct r as [g|].
  2: { injection H0 as <- <- <-. split; [reflexivity|]. split; [exact Hnd0|]. split; [intros y Hy; exact Hy|].
       split; [intros y Hy; exact Hy|]. split; [intros t Ht; left; exact Ht|].
       split; [intros t (pa & a' & E & Er & _); injection E as <- <-; rewrite Hr0 in Er; discriminate Er|].
       split; [intros c (pa & a' & d & n' & its' & E & Er & _); injection E as <- <-; rewrite Hr0 in Er; discriminate Er|].
       intros c Hc Hn. contradiction. }
  destruct Hr0 as [Er Hgl]. destruct (loader_decl _ g Hgl) as (Dg & Kg & Og). cbn [fst snd] in H0.
  destruct (kind_eqb (gs_kind g) KType) eqn:Kt.
  - (* a type *)
    apply kind_eqb_spec in Kt. injection H0 as <- <- <-.
    assert (Hin : In (gs_id g) (if nmem (gs_id g) o0 then o0 else o0 ++ [gs_id g])).
    { destruct (nmem (gs_id g) o0) eqn:Em; [apply nmem_in; exact Em | apply in_or_app; right; left; reflexivity]. }
    split; [reflexivity|]. split.
    { destruct (nmem (gs_id g) o0) eqn:Em; [exact Hnd0|]. apply NoDup_app; [exact Hnd0 | repeat constructor; auto|].
      intros x Hx [<-|[]]. assert (nmem (gs_id g) o0 = true) by (apply nmem_in; exact Hx). congruence. }
    split; [intros x Hx; destruct (nmem (gs_id g) o0); [exact Hx | apply in_or_app; left; exact Hx]|].
    split; [intros y Hy; exact Hy|]. split.
    { intros t Ht. destruct (nmem (gs_id g) o0); [left; exact Ht|].
      apply in_app_or in Ht as [Ht|[<-|[]]]; [left; exact Ht|]. right.
      apply A.STType; [exact Er | rewrite Kg, Kt; reflexivity]. }
    split; [intros t Hd; destruct (direct_sym _ _ _ _ g Er Hgl Hd) as [-> _]; exact Hin|].
    split; [intros c Hc; destruct (dirctx_sym _ _ _ _ g Er Hgl Hc) as (_ & Kc & _); congruence|].
    intros c Hc Hn. contradiction.
  - destruct (kind_eqb (gs_kind g) KCtx) eqn:Kc.
    2: { (* neither *)
      injection H0 as <- <- <-. split; [reflexivity|]. split; [exact Hnd0|]. split; [intros y Hy; exact Hy|].
      split; [intros y Hy; exact Hy|]. split; [intros t Ht; left; exact Ht|].
      split; [intros t Hd; destruct (direct_sym _ _ _ _ g Er Hgl Hd) as [_ E]; rewrite E in Kt; discriminate Kt|].
      split; [intros c Hc'; destruct (dirctx_sym _ _ _ _ g Er Hgl Hc') as (_ & E & _); rewrite E in Kc; discriminate Kc|].
      intros c Hc' Hn. contradiction. }
    apply kind_eqb_spec in Kc.
    assert (NotD : forall t, ~ Direct (pathi mi) (Spec (Some p) None a) t)
      by (intros t Hd; destruct (direct_sym _ _ _ _ g Er Hgl Hd) as [_ E]; rewrite E in Kt; discriminate Kt).
    destruct (nmem (gs_id g) V0) eqn:Ev.
    + (* entered before *)
      injection H0 as <- <- <-. split; [reflexivity|]. split; [exact Hnd0|]. split; [intros y Hy; exact Hy|].
      split; [intros y Hy; exact Hy|]. split; [intros t Ht; left; exact Ht|].
      split; [intros t Hd; destruct (NotD t Hd)|].
      split; [intros c Hc'; destruct (dirctx_sym _ _ _ _ g Er Hgl Hc') as (-> & _); apply nmem_in; exact Ev|].
      intros c Hc' Hn. contradiction.
    + pose proof (li_kind _ _ HI _ g Hgl) as Kd. unfold KindOK in Kd. rewrite Kc in Kd.
      destruct (gs_decl g) as [| | | | | | [| |] ? ? ? ? | n0 its0 |] eqn:Ed; cbn in Kd; try discriminate.
      destruct (IH (gs_mod g) its0 (gs_id g :: V0) o0 s0 V1 o1 s1 Hg0 Og Hnd0 H0)
        as (-> & N1 & Io1 & Iv1 & S1 & D1 & K1 & F1).
      split; [reflexivity|]. split; [exact N1|]. split; [exact Io1|].
      split; [intros y Hy; apply Iv1; right; exact Hy|]. split.
      { intros t Ht. destruct (S1 t Ht) as [Ho|(sp & Hsp & Hst)]; [left; exact Ho|]. right.
        eapply A.STCtx; [exact Er | exact Dg | cbn; reflexivity | exact Hsp | exact Hst]. }
      split; [intros t Hd; destruct (NotD t Hd)|].
      split; [intros c Hc'; destruct (dirctx_sym _ _ _ _ g Er Hgl Hc') as (-> & _); apply Iv1; left; reflexivity|].
      intros c Hc' Hn. destruct (Nat.eq_dec c (gs_id g)) as [->|Hne].
      * intros d n1 its1 Hd Ha sp Hsp. rewrite Dg in Hd. injection Hd as <-. cbn in Ha |- *.
        try rewrite Ed in Ha. injection Ha as <- <-. split; [intros t0; exact (D1 sp t0 Hsp) | intros c0; exact (K1 sp c0 Hsp)].
      * apply F1; [exact Hc'|]. intros [E|E]; [exact (Hne (eq_sym E)) | exact (Hn E)].
Qed.

(* completeness: what a context reaches through finished contexts *)
Lemma spec_complete : forall V out, (forall c, In c V -> Finished V out c) ->
  forall q sp t, A.SpecTypes W q sp t ->
  (forall t', Direct q sp t' -> In t' out) -> (forall c, DirCtx q sp c -> In c V) -> In t out.
Proof.
  intros V out HF q sp t H. induction H as [q pa a t Er Kt | q pa a c d n its sp t Er Dd Ad Hsp _ IH]; intros HD HK.
  - apply HD. exists pa, a. auto.
  - assert (Hc : In c V) by (apply HK; exists pa, a, d, n, its; auto).
    destruct (HF c Hc d n its Dd Ad sp Hsp) as [F1 F2]. exact (IH F1 F2).
Qed.

(* The requirements of a val, fn, method, context or functor: what the
   elaborator computes is exactly AssumeTypes, without repeats. *)
Theorem assume_type_syms_exact : forall g s l s', GS s -> nget (ls_syms st) (gs_id g) = Some g ->
  assume_type_syms g s = Good (l, s') ->
  s' = s /\ NoDup l /\ forall t, In t l <-> A.AssumeTypes W (gs_id g) t.
Proof.
  intros g s l s' Hg Hgid H.
  destruct (loader_decl _ g Hgid) as (Dg & _ & Og).
  unfold assume_type_syms, nesting_types, walk_fuel in H.
  apply bind_inv in H as (n & s1 & H1 & H). apply bind_inv in H1 as (s2 & s3 & E1 & E2).
  injection E1 as <- <-. injection E2 as <- <-.
  apply bind_inv in H as ([V' o'] & s4 & H4 & H). injection H as <- <-.
  destruct (walk_ok _ (gs_mod g) (assume_specs (gs_assumes g)) [] [] s V' o' s4 Hg Og (NoDup_nil _) H4)
    as (-> & N & _ & _ & S & D & K & Fn).
  split; [reflexivity|]. split; [exact N|]. intro t. split.
  - intro Ht. destruct (S t Ht) as [[]|(sp & Hsp & Hst)].
    unfold assume_specs in Hsp. apply in_flat_map in Hsp as ([p [o|]] & Hin & Hsp); [destruct Hsp|].
    destruct Hsp as [<-|[]].
    exists (A.Decl (pathi (gs_mod g)) (gs_decl g) (gs_assumes g)), p.
    split; [exact Dg|]. split; [exact Hin|]. exact Hst.
  - intros (d & pa & Dd & Hin & Hst). rewrite Dg in Dd. injection Dd as <-. cbn in Hin, Hst.
    assert (Hsp : In (Spec (Some pa) None None) (assume_specs (gs_assumes g)))
      by (apply in_flat_map; exists (pa, None); split; [exact Hin | left; reflexivity]).
    exact (spec_complete V' o' (fun c Hc => Fn c Hc (fun E => E)) _ _ t Hst (fun t0 => D _ t0 Hsp) (fun c0 => K _ c0 Hsp)).
Qed.

End Final.

(* ======================================================================== *)
(* 6. The theorem                                                            *)
(* ======================================================================== *)

Lemma linv_init : forall b, LInv b (LSt [] NE NE [] NE 1 0).
Proof.
  intro b. constructor; cbn [ls_loaded ls_mods ls_order ls_syms ls_next_sym ls_next_mod keys map].
  - constructor.
  - intros p i [].
  - intro i. split; [intros []|intros (p & [])].
  - intros i m H. rewrite nget_NE in H. discriminate.
  - intros s g H. rewrite nget_NE in H. discriminate.
  - intros s _. apply nget_NE.
  - lia.
  - intros s g H. rewrite nget_NE in H. discriminate.
  - intros p i m am [].
  - intros i m r n s H. rewrite nget_NE in H. discriminate.
  - intros p i m am [].
  - intros s g H. rewrite nget_NE in H. discriminate.
Qed.

(* Track A, step 1 of elaborate_sound: the loader's reading of a bundle
   satisfies every condition of WorldOf about names and declarations. *)
Theorem load_world_reads : forall b w R F, load_world b = Good w -> LoaderReading b (W_read w R F).
Proof.
  intros b w R F H. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _ & He & Hr & _).
  apply (final_reading b st e L He). intros q j Hq. destruct (Hr q j Hq) as [[]|Hq']. exact Hq'.
Qed.

(* The elaborator's entity, as the relation's. *)
Definition ent_of (w : Elab.world) (e0 : Elab.ent) : A.ent :=
  match e0 with Elab.ESym s => A.ESym s | Elab.EMod j => A.EMod (gm_path (get_mod w j)) end.

(* Name resolution agrees: in every module of the loader's world, what the
   elaborator resolves a path to is what Part V's `resolve` does. *)
Theorem load_world_resolves : forall b w R F, load_world b = Good w ->
  forall i pa r, In i (w_order w) -> Elab.resolve_path w i pa = Good r ->
  A.resolve (W_read w R F) (gm_path (get_mod w i)) pa = option_map (ent_of w) r.
Proof.
  intros b w R F H i pa r Hi Hres. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _ & He & Hr & _).
  assert (Hr' : forall q j, In (q, j) (ls_loaded st) -> Reach b (b_entry b) q)
    by (intros q j Hq; destruct (Hr q j Hq) as [[]|Hq']; exact Hq').
  cbn [w_order] in Hi. destruct (proj1 (li_order _ _ L i) Hi) as (p & Hp).
  destruct (li_loaded _ _ L p i Hp) as [_ (m & am & Em & Pm & _)].
  pose proof (resolve_path_agrees b st e L R F p i m pa r Hp Em Hres) as X.
  rewrite (fin_get_mod st e i m Em), Pm. exact X.
Qed.

(* The requirements of a val, fn, method, context or functor, as the
   elaborator computes them (Elab.assume_type_syms) in any state that keeps
   the loader's world and symbols (ElabFrame: every state elaboration
   reaches): exactly Part V's AssumeTypes, without repeats. *)
Theorem load_world_assume_types : forall b w R F, load_world b = Good w ->
  forall s g l s', st_w s = w -> (forall i g0, nget (w_syms w) i = Some g0 -> nget (st_syms s) i = Some g0) ->
  nget (w_syms w) (gs_id g) = Some g ->
  assume_type_syms g s = Good (l, s') ->
  s' = s /\ NoDup l /\ forall t, In t l <-> A.AssumeTypes (W_read w R F) (gs_id g) t.
Proof.
  intros b w R F H s g l s' Hsw Hsk Hg Ha. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _ & He & Hr & _).
  assert (Hr' : forall q j, In (q, j) (ls_loaded st) -> Reach b (b_entry b) q)
    by (intros q j Hq; destruct (Hr q j Hq) as [[]|Hq']; exact Hq').
  exact (assume_type_syms_exact b st e L R F g s l s' (conj Hsw Hsk) Hg Ha).
Qed.

Lemma load_world_ids : forall b w, load_world b = Good w ->
  forall i g, nget (w_syms w) i = Some g -> gs_id g = i.
Proof.
  intros b w H i g Hg. unfold load_world in H.
  apply rbind_good in H as ([e st] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  exact (proj1 (proj2 (proj2 (li_syms _ _ L i g Hg)))).
Qed.

(* What the elaborator reads a bundle as, when it accepts it. *)
Corollary elaborate_world_reads : forall b p, elaborate b = Good p ->
  exists w, load_world b = Good w /\ forall R F, LoaderReading b (W_read w R F).
Proof.
  intros b p H. unfold elaborate in H. destruct (load_world b) as [w|m] eqn:E; [|discriminate].
  exists w. split; [reflexivity|]. intros R F. exact (load_world_reads b w R F E).
Qed.

(* ======================================================================== *)
(* 7. Field numbering: an injective extension of the elaborator's table     *)
(* ======================================================================== *)

(* An injective code of strings, base 257. *)
Fixpoint str_code (s : string) : nat :=
  match s with
  | EmptyString => 0
  | String c r => S (Ascii.nat_of_ascii c) + 257 * str_code r
  end.

Lemma str_code_inj : forall a b, str_code a = str_code b -> a = b.
Proof.
  induction a as [|c r IH]; intros [|c' r'] H; cbn [str_code] in H; try lia; [reflexivity|].
  pose proof (Ascii.nat_ascii_bounded c) as Bc. pose proof (Ascii.nat_ascii_bounded c') as Bc'.
  assert (E1 : S (Ascii.nat_of_ascii c) = S (Ascii.nat_of_ascii c') /\ str_code r = str_code r').
  { assert (M1 : (S (Ascii.nat_of_ascii c) + 257 * str_code r) mod 257 = S (Ascii.nat_of_ascii c))
      by (rewrite (Nat.mul_comm 257), Nat.Div0.mod_add; apply Nat.mod_small; lia).
    assert (M2 : (S (Ascii.nat_of_ascii c') + 257 * str_code r') mod 257 = S (Ascii.nat_of_ascii c'))
      by (rewrite (Nat.mul_comm 257), Nat.Div0.mod_add; apply Nat.mod_small; lia).
    rewrite H in M1. rewrite M1 in M2. split; [exact M2|]. lia. }
  destruct E1 as [Ec Er]. f_equal.
  - rewrite <- (Ascii.ascii_nat_embedding c), <- (Ascii.ascii_nat_embedding c'). f_equal. lia.
  - exact (IH r' Er).
Qed.

(* The elaborator's table where it has the name, else past every number in
   it.  `bound` is any number at least every number the table holds. *)
Definition fld_ext (flds : list (string * nat)) (bound : nat) (f : string) : nat :=
  match sassoc f flds with Some n => n | None => S bound + str_code f end.

Theorem fld_ext_inj : forall flds bound,
  (forall a c n, sassoc a flds = Some n -> sassoc c flds = Some n -> a = c) ->
  (forall a n, sassoc a flds = Some n -> n <= bound) ->
  forall a c, fld_ext flds bound a = fld_ext flds bound c -> a = c.
Proof.
  intros flds bound Hinj Hb a c. unfold fld_ext.
  destruct (sassoc a flds) as [na|] eqn:Ea, (sassoc c flds) as [nc|] eqn:Ec; intro H.
  - subst. exact (Hinj a c nc Ea Ec).
  - pose proof (Hb a na Ea). lia.
  - pose proof (Hb c nc Ec). lia.
  - apply str_code_inj. lia.
Qed.

(* The elaborator numbers fields 1, 2, … in order of first use (Elab.fld_id);
   a table so built satisfies fld_ext_inj's premises. *)
Definition Numbered (flds : list (string * nat)) : Prop :=
  NoDup (map fst flds) /\ map snd flds = seq 1 (List.length flds).

Lemma numbered_snoc : forall flds f, Numbered flds -> sassoc f flds = None ->
  Numbered (flds ++ [(f, S (List.length flds))]).
Proof.
  intros flds f [N Hs] Hf. split.
  - rewrite map_app. cbn. apply NoDup_app; [exact N | repeat constructor; auto |].
    intros x Hx [<-|[]]. exact (sassoc_none _ _ _ Hf Hx).
  - rewrite map_app, Hs, length_app. cbn. rewrite Nat.add_1_r, seq_S. reflexivity.
Qed.

Lemma numbered_inj : forall flds, Numbered flds ->
  (forall a c n, sassoc a flds = Some n -> sassoc c flds = Some n -> a = c) /\
  (forall a n, sassoc a flds = Some n -> n <= List.length flds).
Proof.
  intros flds [N Hs].
  assert (Hin : forall a n, sassoc a flds = Some n -> In (a, n) flds) by (intros; apply sassoc_in; assumption).
  assert (Hbound : forall a n, In (a, n) flds -> 1 <= n <= List.length flds).
  { intros a n H. assert (In n (map snd flds)) by (apply in_map_iff; exists (a, n); auto).
    rewrite Hs in H0. apply in_seq in H0. lia. }
  split.
  - intros a c n Ha Hc. apply Hin in Ha, Hc.
    assert (Hnd : NoDup (map snd flds)) by (rewrite Hs; apply seq_NoDup).
    clear Hs Hin Hbound. induction flds as [|[k v] r IH]; [destruct Ha|].
    inversion N as [|? ? Nk Nr]; subst. inversion Hnd as [|? ? Vk Vr]; subst.
    destruct Ha as [Ea|Ha]; destruct Hc as [Ec|Hc].
    + congruence.
    + injection Ea as -> ->. exfalso. apply Vk. apply in_map_iff. exists (c, n). auto.
    + injection Ec as -> ->. exfalso. apply Vk. apply in_map_iff. exists (a, n). auto.
    + exact (IH Nr Ha Hc Vr).
  - intros a n Ha. exact (proj2 (Hbound a n (Hin a n Ha))).
Qed.

(* fld_id keeps a table Numbered. *)
Lemma fld_id_numbered : forall f s n s', fld_id f s = Good (n, s') -> Numbered (st_flds s) ->
  Numbered (st_flds s') /\ sassoc f (st_flds s') = Some n.
Proof.
  intros f s n s' H Hn. unfold fld_id in H. cbn in H.
  destruct (sassoc f (st_flds s)) as [m|] eqn:E.
  - injection H as <- <-. auto.
  - injection H as <- <-. cbn. split; [apply numbered_snoc; assumption|].
    apply sassoc_snoc_new. exact E.
Qed.

(* ======================================================================== *)
(* world_todo: what WorldOf still needs, stated and NOT proved              *)
(* ======================================================================== *)

(* With s the elaborator's final state (RelSupport.run_elab keeps it) and
   w = st_w s the loader's world:

   TODO_reqs_reading:
     ReqsReading (W_read w R F) for an R that agrees with every requirement
     list the elaborator uses.  PROVED so far: a loader symbol's kind is its
     declaration's (LInv's li_kind, KindOK), every scope or export entry
     that is not a module alias is a loader symbol (sym_entries,
     resolved_loader), and Elab.assume_type_syms computes exactly
     AssumeTypes, without repeats, in any state elaboration reaches
     (assume_type_syms_exact, load_world_assume_types).  Remaining:
     (a) DONE (ElabFrame.v, ReqOK in elab_frame): every cached list is
         without repeats and of a symbol; of a val/fn/method/context/
         functor it is AssumeTypes, of a type or unit [].  (The elaborator's
         two tag/alias writes go through set_nominal_reqs, a guard and a
         dedup that change nothing on any input: Audit.v note 29.)
     (b) for tags and aliases, that solve_requirements' iteration from the
         listed parameters reaches the least relevance-closed sets
         (rr_closed, rr_least, rr_listed): a Kleene-iteration argument
         over each group, with the cached lists of earlier groups final;
     (c) R for the symbols the elaborator never asked about: an
         independent, proved computation of the same sets (AssumeTypes is
         a reachability through contexts that may be cyclic, which the
         elaborator's 64-step walk does not finish), or RelSupport.run_elab's
         fold of `reqs` over every symbol, proved to succeed on accepted
         bundles (it need not: an unused context nested 65 deep).
     Estimate: (a) 300 lines, (b) 1000-1500, (c) 500-800.

   TODO_fld_injective and TODO_syms_frame: DONE in ElabFrame.v
   (elab_frame, elaborate_frame): every function of the elaborator keeps
   the loader's world and symbols, makes only non-source symbols, fresh in
   W_read, and numbers fields 1, 2, …, so fld_ext of the final table is an
   injective field numbering. *)
