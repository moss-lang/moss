(* ------------------------------------------------------------------------ *)
(* ReqsStable.v — a requirement list, once cached, never changes (Track A). *)
(*                                                                           *)
(* The elaborator's requirement cache (st_reqs) is rewritten only inside    *)
(* solve_requirements, and only for the group it gathered, which was not    *)
(* cached when it started; every other write is of a symbol not yet cached. *)
(* So the cache only grows (rq_group: Ext from before to after every call   *)
(* of the requirement functions), and `type_requirements x` leaves its      *)
(* answer cached under x (reqs_cached). These local cache laws remain       *)
(* available to the loader requirement proofs; the retired elaborator's    *)
(* whole-program frame proof is no longer part of the certificate.         *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve.
Import ListNotations.
Local Open Scope list_scope.

Local Strategy opaque [Nat.of_num_uint].

(* What a computation keeps of the state: a predicate, from start to end. *)
Definition Pres (P : st -> Prop) {A} (m : M A) : Prop :=
  forall s a s', P s -> m s = Good (a, s') -> P s'.

Lemma pres_ret : forall P A (a : A), Pres P (ret a).
Proof. intros P A a s x s' H E. injection E as _ <-. exact H. Qed.

Lemma pres_bind : forall P A B (m : M A) (k : A -> M B), Pres P m -> (forall a, Pres P (k a)) -> Pres P (mbind m k).
Proof.
  intros P A B m k Hm Hk s b s'' H E. apply bind_inv in E as (a & s' & E1 & E2).
  exact (Hk a _ _ _ (Hm _ _ _ H E1) E2).
Qed.

Lemma pres_mmap : forall P A B (f : A -> M B) l, (forall x, In x l -> Pres P (f x)) -> Pres P (mmap f l).
Proof.
  intros P A B f l. induction l as [|x r IH]; intro Hf; cbn [mmap]; [apply pres_ret|].
  apply pres_bind; [exact (Hf x (or_introl eq_refl))|]. intro y.
  apply pres_bind; [exact (IH (fun z Hz => Hf z (or_intror Hz)))|]. intro; apply pres_ret.
Qed.

Lemma pres_mfold : forall P A B (f : A -> B -> M A) l a, (forall a x, In x l -> Pres P (f a x)) -> Pres P (mfold f a l).
Proof.
  intros P A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply pres_ret|].
  apply pres_bind; [exact (Hf a x (or_introl eq_refl))|]. intro a'. exact (IH a' (fun b y Hy => Hf b y (or_intror Hy))).
Qed.

Lemma pres_miter : forall P A (f : A -> M unit) l, (forall x, In x l -> Pres P (f x)) -> Pres P (miter f l).
Proof.
  intros P A f l. induction l as [|x r IH]; intro Hf; cbn [miter]; [apply pres_ret|].
  apply pres_bind; [exact (Hf x (or_introl eq_refl))|]. intro. exact (IH (fun z Hz => Hf z (or_intror Hz))).
Qed.

Lemma pres_reader : forall P A (m : M A), (forall s a s', m s = Good (a, s') -> s' = s) -> Pres P m.
Proof. intros P A m H s a s' Hp E. rewrite (H s a s' E). exact Hp. Qed.

Lemma pres_fail : forall P A msg, Pres P (@fail A msg).
Proof. intros P A msg s a s' _ E. discriminate E. Qed.

Lemma pres_err : forall P A mi msg, Pres P (@err A mi msg).
Proof. intros P A mi msg s a s' _ E. exfalso. exact (err_not_good _ _ _ _ _ _ E). Qed.

(* The cache only grows. *)
Definition RQ {A} (m : M A) : Prop := forall s a s', m s = Good (a, s') -> Ext (st_reqs s) (st_reqs s').

(* a predicate closed under growth of the cache *)
Definition ExtClosed (P : st -> Prop) : Prop :=
  forall s s', P s -> Ext (st_reqs s) (st_reqs s') -> P s'.

Lemma rq_pres : forall P A (m : M A), ExtClosed P -> RQ m -> Pres P m.
Proof. intros P A m Hc Hm s a s' Hp E. exact (Hc s s' Hp (Hm s a s' E)). Qed.

Lemma pres_rq : forall A (m : M A), (forall c0, Pres (fun s => Ext c0 (st_reqs s)) m) -> RQ m.
Proof. intros A m H s a s' E. exact (H (st_reqs s) s a s' (ext_refl _) E). Qed.

(* readers *)
Lemma mmap_same : forall X Y (f : X -> M Y) xs s ys s',
  (forall x s y s1, f x s = Good (y, s1) -> s1 = s) -> mmap f xs s = Good (ys, s') -> s' = s.
Proof.
  intros X Y f xs. induction xs as [|x r IH]; intros s ys s' Hf H; cbn [mmap] in H.
  - injection H as _ <-. reflexivity.
  - apply bind_inv in H as (y & s1 & H1 & H). rewrite (Hf _ _ _ _ H1) in H.
    apply bind_inv in H as (ys' & s2 & H2 & H). rewrite (IH _ _ _ Hf H2) in H. injection H as _ <-. reflexivity.
Qed.

Lemma mn_reader : forall fuel t mi s l s', mentioned_nominals fuel t mi s = Good (l, s') -> s' = s.
Proof.
  induction fuel as [|f IH]; intros t mi s l s' H; [discriminate|]. cbn [mentioned_nominals] in H.
  destruct t as [| | |fs|ms|p a]; try (injection H as _ <-; reflexivity).
  - apply bind_inv in H as (ls0 & s1 & H1 & H). injection H as _ <-.
    exact (mmap_same _ _ _ _ _ _ _ (fun x s0 y s2 E => IH _ _ _ _ _ E) H1).
  - apply bind_inv in H as (ls0 & s1 & H1 & H). injection H as _ <-.
    exact (mmap_same _ _ _ _ _ _ _ (fun x s0 y s2 E => IH _ _ _ _ _ E) H1).
  - apply bind_inv in H as (tg & s1 & H1 & H). rewrite (resolve_sym_reader _ _ _ _ _ H1) in H.
    apply bind_inv in H as (more & s2 & H2 & H). injection H as _ <-.
    refine (mmap_same _ _ _ _ _ _ _ _ H2).
    intros [bp [[sp|] d sa]] s0 y s3 E; cbn in E; [exact (IH _ _ _ _ _ E) | injection E as _ <-; reflexivity].
Qed.

(* the group solve_requirements gathers: what it holds was uncached, or
   seen before; what it was asked about and is uncached, it holds *)
Lemma collect_group : forall n stack seen s G s', collect_fn n stack seen s = Good (G, s') ->
  s' = s /\ (forall x, In x G -> In x seen \/ nget (st_reqs s) x = None) /\
  (forall x, In x seen -> In x G) /\ (forall x, In x stack -> nget (st_reqs s) x = None -> In x G).
Proof.
  induction n as [|n IH]; intros stack seen s G s' H; [discriminate|]. cbn [collect_fn] in H.
  destruct stack as [|x rest].
  - injection H as <- <-. split; [reflexivity|].
    split; [intros x Hx; left; apply in_rev; exact Hx|]. split; [intros x Hx; apply in_rev; rewrite rev_involutive; exact Hx | intros x []].
  - apply bind_inv in H as (c & s1 & H1 & H). apply cached_reqs_inv in H1 as [-> ->].
    destruct (nmem x seen || is_some (nget (st_reqs s) x)) eqn:Ex.
    + destruct (IH rest seen s G s' H) as (-> & A1 & A2 & A3). split; [reflexivity|]. split; [exact A1|].
      split; [exact A2|]. intros y [<-|Hy] Hn; [|exact (A3 y Hy Hn)].
      apply orb_true_iff in Ex as [Ex|Ex]; [apply A2; apply nmem_in; exact Ex|].
      rewrite Hn in Ex. discriminate Ex.
    + apply bind_inv in H as (g & s2 & H2 & H). apply sym_inv in H2 as [-> _].
      apply bind_inv in H as (more & s3 & H3 & H).
      assert (E3 : s3 = s) by (destruct (decl_ty (gs_decl g)); [exact (mn_reader _ _ _ _ _ _ H3) | injection H3 as _ <-; reflexivity]).
      subst s3. destruct (IH (more ++ rest) (x :: seen) s G s' H) as (-> & A1 & A2 & A3).
      split; [reflexivity|]. split.
      * intros y Hy. destruct (A1 y Hy) as [[<-|Hy']|Hy']; [right | left; exact Hy' | right; exact Hy'].
        apply orb_false_iff in Ex as [_ Ex]. destruct (nget (st_reqs s) x); [discriminate Ex | reflexivity].
      * split; [intros y Hy; apply A2; right; exact Hy|].
        intros y [<-|Hy] Hn; [apply A2; left; reflexivity | apply A3; [apply in_or_app; right; exact Hy | exact Hn]].
Qed.

(* a write of a group member, which was uncached at the start *)
Lemma ext_set_new : forall (c0 c : nmap (list nat)) (x : nat) (l : list nat), Ext c0 c -> nget c0 x = None -> Ext c0 (nset c x l).
Proof.
  intros c0 c x l H Hx y l' Hy. rewrite nget_nset_other; [exact (H y l' Hy)|]. intros ->. congruence.
Qed.

Lemma pres_set_nominal : forall (c0 c2 : nmap (list nat)) x v, nget c0 x = None ->
  Pres (fun s => Ext c0 (st_reqs s) /\ forall y, nget c2 y <> None -> nget (st_reqs s) y <> None) (set_nominal_reqs x v).
Proof.
  intros c0 c2 x v Hx s u s' [H1 H2] E. destruct (set_nominal_inv x v s u s' E) as (g & _ & [[_ ->]|[_ ->]]); [|split; assumption].
  cbn [st_reqs s_reqs]. split; [exact (ext_set_new _ _ _ _ H1 Hx)|].
  intros y Hy. rewrite nget_nset. destruct (Nat.eqb y x); [discriminate | exact (H2 y Hy)].
Qed.

Lemma miter_set_nominal : forall (c0 : nmap (list nat)) L s u s',
  (forall x, In x L -> nget c0 x = None) -> Ext c0 (st_reqs s) ->
  miter (fun x => set_nominal_reqs x []) L s = Good (u, s') ->
  Ext c0 (st_reqs s') /\ st_syms s' = st_syms s /\ (forall y, nget (st_reqs s) y <> None -> nget (st_reqs s') y <> None) /\
  (forall x g, In x L -> nget (st_syms s) x = Some g -> nominal_k (gs_kind g) = true -> nget (st_reqs s') x <> None).
Proof.
  intros c0 L. induction L as [|x r IH]; intros s u s' HL He H; cbn [miter] in H.
  - injection H as _ <-. split; [exact He|]. split; [reflexivity|]. split; [auto | intros x g []].
  - apply bind_inv in H as (u1 & s1 & H1 & H).
    assert (S1 : Ext c0 (st_reqs s1) /\ st_syms s1 = st_syms s /\ (forall y, nget (st_reqs s) y <> None -> nget (st_reqs s1) y <> None) /\
                 forall g, nget (st_syms s) x = Some g -> nominal_k (gs_kind g) = true -> nget (st_reqs s1) x <> None).
    { destruct (set_nominal_inv x [] s u1 s1 H1) as (g & Hg & [[Hk ->]|[Hk ->]]).
      - cbn [st_reqs st_syms s_reqs]. split; [exact (ext_set_new _ _ _ _ He (HL x (or_introl eq_refl)))|].
        split; [reflexivity|]. split; [intros y Hy; rewrite nget_nset; destruct (Nat.eqb y x); [discriminate | exact Hy]|].
        intros g' _ _. rewrite nget_nset_same. discriminate.
      - split; [exact He|]. split; [reflexivity|]. split; [auto|]. intros g' Hg' Hk'. rewrite Hg in Hg'. injection Hg' as <-. congruence. }
    destruct S1 as (E1 & Y1 & G1 & C1).
    destruct (IH s1 u s' (fun y Hy => HL y (or_intror Hy)) E1 H) as (E2 & Y2 & G2 & C2).
    split; [exact E2|]. split; [congruence|]. split; [intros y Hy; apply G2, G1, Hy|].
    intros y g [<-|Hy] Hg Hk; [apply G2; exact (C1 g Hg Hk)|]. apply (C2 y g Hy); [rewrite Y1; exact Hg | exact Hk].
Qed.

Lemma rq_cached_reqs : forall x, forall P, Pres P (cached_reqs x).
Proof. intros x P. apply pres_reader. intros s a s' E. exact (proj1 (cached_reqs_inv _ _ _ _ E)). Qed.

Lemma pres_sym : forall x P, Pres P (sym x).
Proof. intros x P. apply pres_reader. intros s a s' E. exact (proj1 (sym_inv _ _ _ _ E)). Qed.

Lemma pres_resolve_sym : forall mi pa P, Pres P (Elab.resolve_sym mi pa).
Proof. intros mi pa P. apply pres_reader. exact (resolve_sym_reader mi pa). Qed.

Lemma pres_assume_type_syms : forall g P, Pres P (assume_type_syms g).
Proof. intros g P. apply pres_reader. exact (assume_type_syms_reader g). Qed.

(* The requirement functions only grow the cache; solve_requirements leaves
   its root cached. *)
Lemma rq_group : forall fuel,
  (forall t mi out, RQ (free_type_syms fuel t mi out)) /\
  (forall x, RQ (type_requirements fuel x)) /\
  (forall root, RQ (solve_requirements fuel root)) /\
  (forall g, RQ (listed_params fuel g)) /\
  (forall root s u s', solve_requirements fuel root s = Good (u, s') ->
     nget (st_reqs s) (gs_id root) = None -> nget (st_syms s) (gs_id root) = Some root ->
     nominal_k (gs_kind root) = true -> nget (st_reqs s') (gs_id root) <> None).
Proof.
  (* listed_params reads only *)
  assert (Hlp : forall f g, RQ (listed_params f g)).
  { intros f g s a s' H. assert (E0 : s' = s); [|rewrite E0; apply ext_refl].
    destruct f; cbn [listed_params] in H;
    (destruct (gs_decl g) as [| | |n0 [|n1 ns] arity t0| | | | |]; try (injection H as _ <-; reflexivity);
     apply bind_inv in H as (nest & s1 & H1 & H); rewrite (assume_type_syms_reader _ _ _ _ H1) in H;
     refine (mfold_reader _ _ _ _ _ _ _ _ _ H);
     intros o n s0 r s0' E; apply bind_inv in E as (t & s2 & E1 & E); rewrite (resolve_sym_reader _ _ _ _ _ E1) in E;
     destruct t as [t'|]; [destruct (nmem (gs_id t') nest); [injection E as _ <-; reflexivity|] |];
     exfalso; exact (err_not_good _ _ _ _ _ _ E)). }
  induction fuel as [|f IH].
  { split; [intros t mi out s a s' H; discriminate H|]. split; [intros x s a s' H; discriminate H|].
    split; [intros r s a s' H; discriminate H|]. split; [exact (Hlp 0)|]. intros r s u s' H; discriminate H. }
  destruct IH as (I0 & I1 & I2 & I3 & I4).
  assert (Ifts : forall t mi out, RQ (free_type_syms (S f) t mi out)).
  { intros t mi out. apply pres_rq. intro c0.
    assert (EC : ExtClosed (fun s => Ext c0 (st_reqs s))) by (intros s s' H1 H2; exact (ext_trans _ _ _ H1 H2)).
    cbn [free_type_syms]. destruct t as [| | |fs|ms|p a].
    1, 2, 3: apply pres_ret.
    - apply pres_mfold. intros o x _. apply rq_pres; [exact EC | apply I0].
    - apply pres_mfold. intros o x _. apply rq_pres; [exact EC | apply I0].
    - apply pres_bind; [apply pres_resolve_sym|]. intros [g|]; [|apply pres_ret].
      apply pres_bind; [apply pres_mmap; intros b0 _; apply pres_bind; [apply pres_resolve_sym | intro; apply pres_ret]|].
      intro bound. apply pres_bind.
      + apply pres_mfold. intros o [bp [[sp|] d sa]] _; [apply rq_pres; [exact EC | apply I0] | apply pres_ret].
      + intro o. destruct (kind_eqb (gs_kind g) KType); [apply pres_ret|].
        apply pres_bind; [apply rq_pres; [exact EC | apply I1] | intro; apply pres_ret]. }
  assert (Isolve : forall root, RQ (solve_requirements (S f) root) /\
            forall s u s', solve_requirements (S f) root s = Good (u, s') ->
              nget (st_reqs s) (gs_id root) = None -> nget (st_syms s) (gs_id root) = Some root ->
              nominal_k (gs_kind root) = true -> nget (st_reqs s') (gs_id root) <> None).
  { intro root.
    assert (Main : forall s u s', solve_requirements (S f) root s = Good (u, s') ->
              Ext (st_reqs s) (st_reqs s') /\
              (nget (st_reqs s) (gs_id root) = None -> nget (st_syms s) (gs_id root) = Some root ->
               nominal_k (gs_kind root) = true -> nget (st_reqs s') (gs_id root) <> None)).
    { intros s u s' H. cbn [solve_requirements] in H.
      apply bind_inv in H as (G & s1 & H1 & H).
      assert (H1' : collect_fn 100000 [gs_id root] [] s = Good (G, s1)) by exact H1.
      destruct (collect_group _ _ _ _ _ _ H1') as (-> & G1 & _ & G3).
      assert (Gnew : forall x, In x G -> nget (st_reqs s) x = None) by (intros x Hx; destruct (G1 x Hx) as [[]|E]; exact E).
      apply bind_inv in H as (u1 & s2 & H2 & H).
      destruct (miter_set_nominal (st_reqs s) G s u1 s2 Gnew (ext_refl _) H2) as (E2 & Y2 & _ & C2).
      set (J := fun s0 => Ext (st_reqs s) (st_reqs s0) /\ forall y, nget (st_reqs s2) y <> None -> nget (st_reqs s0) y <> None).
      assert (EC : ExtClosed J).
      { intros a c [Ja Jb] Hx. split; [exact (ext_trans _ _ _ Ja Hx)|]. intros y Hy. exact (ext_cached _ _ _ Hx (Jb y Hy)). }
      assert (J2 : J s2) by (split; [exact E2 | auto]).
      assert (Hrest : J s').
      { refine (pres_bind J _ _ _ _ _ _ _ _ _ J2 H).
        - apply pres_mmap. intros x _. apply pres_bind; [apply pres_sym|]. intro g.
          apply pres_bind; [apply rq_pres; [exact EC | apply I3] | intro; apply pres_ret].
        - intro ll. assert (Hit : forall n, Pres J (iterate_fn f G ll n)).
          { induction n as [|n IHn]; [apply pres_fail|]. cbn [iterate_fn].
            apply pres_bind; [|intros [|]; [exact IHn | apply pres_ret]].
            apply pres_mfold. intros ch x Hx. unfold round_step.
            apply pres_bind; [apply pres_sym|]. intro g. apply pres_bind.
            { destruct (decl_ty (gs_decl g)); [apply rq_pres; [exact EC | apply I0] | apply pres_ret]. }
            intro found. apply pres_bind; [apply rq_cached_reqs|]. intro old.
            destruct (list_eq_dec Nat.eq_dec found (opt_default [] old)); [apply pres_ret|].
            apply pres_bind; [apply pres_set_nominal; exact (Gnew x Hx) | intro; apply pres_ret]. }
          exact (Hit 10000). }
      destruct Hrest as [R1 R2]. split; [exact R1|].
      intros Hn Hs Hk. apply R2. apply (C2 (gs_id root) root); [|exact Hs|exact Hk].
      apply G3; [left; reflexivity | exact Hn]. }
    split; [intros s u s' H; exact (proj1 (Main s u s' H)) | intros s u s' H; exact (proj2 (Main s u s' H))]. }
  assert (Itr : forall x, RQ (type_requirements (S f) x)).
  { intros x s a s' H. cbn [type_requirements] in H.
    apply bind_inv in H as (c & s1 & H1 & H). apply cached_reqs_inv in H1 as [-> ->].
    destruct (nget (st_reqs s) x) as [l|] eqn:Hc; [injection H as _ <-; apply ext_refl|].
    apply bind_inv in H as (g & s2 & H2 & H). apply sym_inv in H2 as [-> _].
    destruct (gs_kind g).
    1, 2: apply bind_inv in H as (u & s3 & H3 & H); unfold set_reqs, upd in H3; injection H3 as <- <-;
          injection H as _ <-; cbn [st_reqs s_reqs]; exact (ext_set_new _ _ _ _ (ext_refl _) Hc).
    1, 2: apply bind_inv in H as (u & s3 & H3 & H); apply bind_inv in H as (c' & s4 & H4 & H);
          apply cached_reqs_inv in H4 as [-> _]; injection H as _ <-; exact (I2 g _ _ _ H3).
    all: apply bind_inv in H as (out & s3 & H3 & H); rewrite (assume_type_syms_reader _ _ _ _ H3) in H;
         apply bind_inv in H as (u & s4 & H4 & H); unfold set_reqs, upd in H4; injection H4 as <- <-;
         injection H as _ <-; cbn [st_reqs s_reqs]; exact (ext_set_new _ _ _ _ (ext_refl _) Hc). }
  split; [exact Ifts|]. split; [exact Itr|]. split; [intro r; exact (proj1 (Isolve r))|].
  split; [exact (Hlp (S f))|]. intro r. exact (proj2 (Isolve r)).
Qed.

Lemma rq_type_requirements : forall fuel x, RQ (type_requirements fuel x).
Proof. intros fuel x. exact (proj1 (proj2 (rq_group fuel)) x). Qed.

Lemma rq_solve_requirements : forall fuel root, RQ (solve_requirements fuel root).
Proof. intros fuel root. exact (proj1 (proj2 (proj2 (rq_group fuel))) root). Qed.

(* `type_requirements x` leaves its answer cached under x *)
Lemma reqs_cached : forall fuel x s l s', (forall g, nget (st_syms s) x = Some g -> gs_id g = x) ->
  type_requirements fuel x s = Good (l, s') -> nget (st_reqs s') x = Some l.
Proof.
  intros [|f] x s l s' Hid H; [discriminate H|]. cbn [type_requirements] in H.
  apply bind_inv in H as (c & s1 & H1 & H). apply cached_reqs_inv in H1 as [-> ->].
  destruct (nget (st_reqs s) x) as [l0|] eqn:Hc; [injection H as <- <-; exact Hc|].
  apply bind_inv in H as (g & s2 & H2 & H). apply sym_inv in H2 as [-> Hg].
  pose proof (Hid g Hg) as Eid.
  destruct (gs_kind g) eqn:Kg.
  1, 2: apply bind_inv in H as (u & s3 & H3 & H); unfold set_reqs, upd in H3; injection H3 as <- <-;
        injection H as <- <-; cbn [st_reqs s_reqs]; apply nget_nset_same.
  1, 2: apply bind_inv in H as (u & s3 & H3 & H); apply bind_inv in H as (c' & s4 & H4 & H);
        apply cached_reqs_inv in H4 as [-> ->]; injection H as <- <-;
        pose proof (proj2 (proj2 (proj2 (proj2 (rq_group f)))) g s u s3 H3 ltac:(rewrite Eid; exact Hc)
                      ltac:(rewrite Eid; exact Hg) ltac:(rewrite Kg; reflexivity)) as Hn;
        rewrite Eid in Hn; destruct (nget (st_reqs s3) x); [reflexivity | exfalso; exact (Hn eq_refl)].
  all: apply bind_inv in H as (out & s3 & H3 & H); rewrite (assume_type_syms_reader _ _ _ _ H3) in H;
       apply bind_inv in H as (u & s4 & H4 & H); unfold set_reqs, upd in H4; injection H4 as <- <-;
       injection H as <- <-; cbn [st_reqs s_reqs]; apply nget_nset_same.
Qed.
