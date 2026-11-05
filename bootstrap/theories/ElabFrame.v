(* ------------------------------------------------------------------------ *)
(* ElabFrame.v — what every step of the elaborator leaves alone (Track A,  *)
(* WorldSound.v's TODO_syms_frame): it never changes the loader's world,   *)
(* never changes or removes a symbol, makes only non-source symbols, and   *)
(* numbers record fields 1, 2, … in order of first use.                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound.
Import ListNotations.

Local Strategy opaque [Nat.of_num_uint].

(* The cached requirement lists (st_reqs), as far as they are local facts:
   without repeats, of a symbol in force, and for a val, fn, method,
   context or functor Part V's AssumeTypes, for a type or unit none. *)
Definition W0 (w : Elab.world) : A.world := W_read w (fun _ => []) (fun _ => 0).

Definition kind_plain (k : kind) : bool :=
  match k with KTag | KAlias | KType | KUnit => false | _ => true end.

Definition ReqOK (s : st) : Prop :=
  forall x l, nget (st_reqs s) x = Some l -> NoDup l /\ exists g, nget (st_syms s) x = Some g /\
    (kind_plain (gs_kind g) = true -> forall t, In t l <-> A.AssumeTypes (W0 (st_w s)) x t) /\
    (gs_kind g = KType \/ gs_kind g = KUnit -> l = []).

(* The invariant of a state, and what a step keeps. *)
Record Inv (s : st) : Prop := {
  iv_above : forall i, st_next s <= i -> nget (st_syms s) i = None;
  iv_flds : Numbered (st_flds s);
  iv_ids : forall i g, nget (st_syms s) i = Some g -> gs_id g = i;
  iv_loader : forall b, load_world b = Good (st_w s) ->
    forall i g, nget (w_syms (st_w s)) i = Some g -> nget (st_syms s) i = Some g;
  iv_fresh : forall b, load_world b = Good (st_w s) ->
    forall i g, nget (w_syms (st_w s)) i = None -> nget (st_syms s) i = Some g -> gs_assumes g = [];
  iv_reqs : forall b, load_world b = Good (st_w s) -> ReqOK s
}.

Record Fr (s s' : st) : Prop := {
  fr_w : st_w s' = st_w s;
  fr_next : st_next s <= st_next s';
  fr_keep : forall i g, nget (st_syms s) i = Some g -> nget (st_syms s') i = Some g;
  fr_new : forall i g, nget (st_syms s) i = None -> nget (st_syms s') i = Some g -> gs_origin g <> OSource;
  fr_new_above : forall i g, nget (st_syms s) i = None -> nget (st_syms s') i = Some g -> st_next s <= i;
  fr_flds : forall f n, sassoc f (st_flds s) = Some n -> sassoc f (st_flds s') = Some n
}.

Lemma fr_refl : forall s, Fr s s.
Proof. intro s. constructor; auto; intros i g H1 H2; congruence. Qed.

Lemma fr_trans : forall s1 s2 s3, Fr s1 s2 -> Fr s2 s3 -> Fr s1 s3.
Proof.
  intros s1 s2 s3 [A1 B1 C1 D1 G1 E1] [A2 B2 C2 D2 G2 E2]. constructor; auto.
  - congruence.
  - lia.
  - intros i g H1 H3. destruct (nget (st_syms s2) i) as [g'|] eqn:E.
    + rewrite (C2 i g' E) in H3. injection H3 as <-. exact (D1 i g' H1 E).
    + exact (D2 i g E H3).
  - intros i g H1 H3. destruct (nget (st_syms s2) i) as [g'|] eqn:E.
    + rewrite (C2 i g' E) in H3. injection H3 as <-. exact (G1 i g' H1 E).
    + pose proof (G2 i g E H3). lia.
Qed.

Definition ok {A} (m : M A) : Prop :=
  forall s a s', Inv s -> m s = Good (a, s') -> Inv s' /\ Fr s s'.

Arguments ok : simpl never.

Lemma ok_ret : forall A (a : A), ok (ret a).
Proof. intros A a s x s' Hi H. injection H as <- <-. split; [exact Hi | apply fr_refl]. Qed.

Lemma ok_bind : forall A B (m : M A) (k : A -> M B), ok m -> (forall a, ok (k a)) -> ok (mbind m k).
Proof.
  intros A B m k Hm Hk s b s'' Hi H. apply bind_inv in H as (a & s' & H1 & H2).
  destruct (Hm _ _ _ Hi H1) as [Hi' F1]. destruct (Hk a _ _ _ Hi' H2) as [Hi'' F2].
  split; [exact Hi'' | exact (fr_trans _ _ _ F1 F2)].
Qed.

Lemma ok_fail : forall A msg, @ok A (fail msg).
Proof. intros A msg s a s' _ H. discriminate. Qed.

Lemma ok_get : ok get.
Proof. intros s a s' Hi H. injection H as <- <-. split; [exact Hi | apply fr_refl]. Qed.

Lemma ok_lift : forall A (r : res A), ok (lift r).
Proof. intros A r s a s' Hi H. unfold lift in H. destruct r; [injection H as <- <-|discriminate]. split; [exact Hi | apply fr_refl]. Qed.

(* a state change that keeps the world, the counter, the symbols, the fields *)
Lemma ok_keep : forall A (f : st -> A * st),
  (forall s, st_w (snd (f s)) = st_w s /\ st_next (snd (f s)) = st_next s /\
             st_syms (snd (f s)) = st_syms s /\ st_flds (snd (f s)) = st_flds s /\
             st_reqs (snd (f s)) = st_reqs s) ->
  ok (fun s => Good (f s)).
Proof.
  intros A f Hf s a s' [Ia If Id Il Ifr Ir] H. injection H as E. destruct (Hf s) as (E1 & E2 & E3 & E4 & E5).
  rewrite E in E1, E2, E3, E4, E5. cbn in E1, E2, E3, E4, E5.
  split; [constructor; [rewrite E2, E3; exact Ia | rewrite E4; exact If | rewrite E3; exact Id
                       | rewrite E1, E3; exact Il | rewrite E1, E3; exact Ifr
                       | intros b Hb; rewrite E1 in Hb; unfold ReqOK; rewrite E1, E3, E5; exact (Ir b Hb)]|].
  constructor; [exact E1 | lia | rewrite E3; auto | rewrite E3; intros; congruence | rewrite E3; intros; congruence | rewrite E4; auto].
Qed.

Lemma ok_fresh : ok fresh.
Proof.
  intros s a s' [Ia If Id Il Ifr Ir] H. unfold fresh in H. injection H as <- <-. cbn.
  split; [constructor; cbn; [intros i Hi; apply Ia; lia | exact If | exact Id | exact Il | exact Ifr | exact Ir]|].
  constructor; cbn; auto; intros; congruence.
Qed.

Lemma ok_new_sym : forall mi n k d o, o <> OSource -> ok (new_sym mi n k d o).
Proof.
  intros mi n k d o Ho s a s' [Ia If Id Il Ifr Ir] H. unfold new_sym in H.
  apply bind_inv in H as (id & s1 & H1 & H). unfold fresh in H1. injection H1 as <- <-.
  apply bind_inv in H as (u & s2 & H2 & H). unfold upd in H2. injection H2 as <- <-.
  injection H as <- <-. unfold s_syms. cbn -[nset nget].
  assert (Hne : forall i g, nget (st_syms s) i = Some g -> i <> st_next s).
  { intros i g Hg E. subst i. rewrite (Ia (st_next s) (le_n _)) in Hg. discriminate. }
  split.
  - constructor; cbn -[nset nget].
    + intros i Hi. rewrite nget_nset_other by lia. apply Ia. lia.
    + exact If.
    + intros i g Hg. cbn -[nset nget] in Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)) as [->|]; [injection Hg as <-; reflexivity | exact (Id i g Hg)].
    + intros b Hb i g Hg. cbn -[nset nget] in *. pose proof (Il b Hb i g Hg) as Hg'. rewrite nget_nset_other; [exact Hg' | exact (Hne _ _ Hg')].
    + intros b Hb i g Hn Hg. cbn -[nset nget] in *. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)) as [->|]; [injection Hg as <-; reflexivity | exact (Ifr b Hb i g Hn Hg)].
    + intros b Hb x l Hx. cbn -[nset nget] in *. destruct (Ir b Hb x l Hx) as (Nd & g & Hg & P1 & P2). split; [exact Nd|].
      exists g. rewrite nget_nset_other; [auto | exact (Hne _ _ Hg)].
  - constructor; cbn -[nset nget]; auto.
    + intros i g Hg. rewrite nget_nset_other; [exact Hg|]. intro E. subst i. rewrite (Ia (st_next s) (le_n _)) in Hg. discriminate.
    + intros i g Hn Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)); [|congruence].
      injection Hg as <-. exact Ho.
    + intros i g Hn Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)); [lia|congruence].
Qed.

Lemma ok_fld_id : forall f, ok (fld_id f).
Proof.
  intros f s a s' [Ia If Id Il Ifr Ir] H. pose proof (fld_id_numbered f s a s' H If) as [Nn _].
  unfold fld_id in H. cbn in H. destruct (sassoc f (st_flds s)) as [m|] eqn:E.
  - injection H as <- <-. split; [constructor; assumption | apply fr_refl].
  - injection H as <- <-. unfold s_flds. cbn. split; [constructor; cbn; assumption|].
    constructor; cbn; auto; try (intros; congruence).
    intros f' n Hf. apply sassoc_snoc_old. exact Hf.
Qed.

Lemma ok_mmap : forall A B (f : A -> M B) l, (forall x, ok (f x)) -> ok (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply ok_ret|].
  apply ok_bind; [apply Hf | intro y; apply ok_bind; [exact IH | intro; apply ok_ret]].
Qed.

Lemma ok_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, ok (f a x)) -> ok (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply ok_ret|].
  apply ok_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma ok_miter : forall A (f : A -> M unit) l, (forall x, ok (f x)) -> ok (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply ok_ret|].
  apply ok_bind; [apply Hf | intro; exact IH].
Qed.

(* a step that only reads the state *)
Lemma ok_reader : forall A (m : M A), (forall s a s', m s = Good (a, s') -> s' = s) -> ok m.
Proof. intros A m Hm s a s' Hi H. rewrite (Hm _ _ _ H). split; [exact Hi | apply fr_refl]. Qed.

Ltac reader :=
  apply ok_reader; let s := fresh "s" in let a := fresh "a" in let s' := fresh "s'" in let H := fresh "H" in
  intros s a s' H;
  repeat (match type of H with context [match ?x with _ => _ end] => destruct x end);
  try discriminate; congruence.

Create HintDb okdb.
#[export] Hint Resolve ok_ret ok_fail ok_get ok_lift ok_fresh ok_fld_id : okdb.
#[export] Hint Extern 1 (ok (new_sym _ _ _ _ _)) => (apply ok_new_sym; discriminate) : okdb.

Ltac head_of t := lazymatch t with ?f _ => head_of f | _ => t end.

(* Decompose `ok t` down to primitives, unfolding definitions as needed. *)
Ltac fintros := repeat (lazymatch goal with |- forall _, _ => intro end).

(* a local `fix` on a nat (fuel) argument: induction on it *)
Ltac fixind1 :=
  lazymatch goal with |- ok (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac fixind3 :=
  lazymatch goal with |- ok (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

Ltac okt :=
  repeat (fintros; lazymatch goal with
  | |- ok (mbind _ _) => apply ok_bind
  | |- ok (mmap _ _) => apply ok_mmap
  | |- ok (mfold _ _ _) => apply ok_mfold
  | |- ok (miter _ _) => apply ok_miter
  | |- ok (upd _) => unfold upd; apply ok_keep; intro; split; [reflexivity|split; [reflexivity|split; [reflexivity|split; reflexivity]]]
  | |- ok (fun s => Good (_, _)) => apply ok_keep; intro; split; [reflexivity|split; [reflexivity|split; [reflexivity|split; reflexivity]]]
  | |- ok (fun s => _) => reader
  | |- ok ?t =>
      first [ solve [auto with okdb]
            | (lazymatch t with ?F _ => is_fix F end; fixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; fixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

(* ======================================================================== *)
(* Every function of the elaborator keeps the frame                          *)
(* ======================================================================== *)

Lemma ok_mentioned_nominals : forall fuel t mi, ok (mentioned_nominals fuel t mi).
Proof. induction fuel; fintros; cbn [mentioned_nominals]; okt. Qed.
#[export] Hint Resolve ok_mentioned_nominals : okdb.

Lemma ok_walk_specs : forall fuel mi sp out, ok (walk_specs fuel mi sp out).
Proof. induction fuel; fintros; cbn [walk_specs]; okt. Qed.
#[export] Hint Resolve ok_walk_specs : okdb.

(* the cache writes *)
Lemma ok_set_nominal_reqs : forall x l, ok (set_nominal_reqs x l).
Proof.
  intros x l s a s2 Hi H. unfold set_nominal_reqs in H.
  apply bind_inv in H as (g & s1 & H1 & H). unfold sym in H1.
  destruct (nget (st_syms s) x) as [g'|] eqn:Hx; [|discriminate]. injection H1 as <- <-.
  destruct (kind_eqb (gs_kind g') KTag || kind_eqb (gs_kind g') KAlias) eqn:Kn.
  - unfold set_reqs, upd in H. injection H as <- <-. destruct Hi as [Ia If Id Il Ifr Ir]. unfold s_reqs. cbn -[nset nget].
    split.
    + constructor; cbn -[nset nget]; try assumption.
      intros b Hb y l' Hy. cbn -[nset nget] in *. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y x) as [->|Hne].
      * injection Hy as <-. split; [apply NoDup_nodup|]. exists g'. split; [exact Hx|].
        apply orb_true_iff in Kn as [Kn|Kn];
          (apply kind_eqb_spec in Kn; rewrite Kn; cbn; split; [discriminate | intros [E|E]; discriminate E]).
      * exact (Ir b Hb y l' Hy).
    + constructor; cbn; auto; intros; congruence.
  - injection H as <- <-. split; [exact Hi | apply fr_refl].
Qed.
#[export] Hint Resolve ok_set_nominal_reqs : okdb.

Lemma rr_eq : forall W1 W2, (forall p ns n, A.w_exp W1 p ns n = A.w_exp W2 p ns n) ->
  forall segs e, A.resolve_rest W1 e segs = A.resolve_rest W2 e segs.
Proof.
  intros W1 W2 Hx segs. induction segs as [|seg r IH]; intros [x|t]; cbn [A.resolve_rest]; try reflexivity.
  rewrite !Hx. destruct (A.w_exp W2 t A.NSName seg); [apply IH|].
  destruct (A.w_exp W2 t A.NSAlias seg); [apply IH | reflexivity].
Qed.

Lemma rs_RF : forall w R F R' F' p pa,
  A.resolve_sym (W_read w R F) p pa = A.resolve_sym (W_read w R' F') p pa.
Proof.
  intros w R F R' F' p [|hd r]; [reflexivity|]. unfold A.resolve_sym, A.resolve.
  change (A.w_scope (W_read w R' F')) with (A.w_scope (W_read w R F)).
  destruct (A.w_scope (W_read w R F) p A.NSName hd) as [e|];
    [|destruct (A.w_scope (W_read w R F) p A.NSAlias hd) as [e|]; [|reflexivity]];
    rewrite (rr_eq (W_read w R F) (W_read w R' F') (fun _ _ _ => eq_refl)); reflexivity.
Qed.

Lemma W0_assume : forall w R F x t, A.AssumeTypes (W_read w R F) x t <-> A.AssumeTypes (W0 w) x t.
Proof.
  assert (Hs : forall w R F R' F' p sp t, A.SpecTypes (W_read w R F) p sp t -> A.SpecTypes (W_read w R' F') p sp t).
  { intros w R F R' F' p sp t H.
    induction H as [p pa a t Hr Hk | p pa a c d n its sp t Hr Hd Ha Hin _ IH].
    - apply A.STType; [rewrite <- (rs_RF w R F); exact Hr | exact Hk].
    - eapply A.STCtx; [rewrite <- (rs_RF w R F); exact Hr | exact Hd | exact Ha | exact Hin | exact IH]. }
  intros w R F x t. unfold W0.
  split; intros (d & pa & D & I & S); exists d, pa; (split; [exact D | split; [exact I | eapply Hs; exact S]]).
Qed.

Lemma ok_type_requirements_step : forall f x,
  (forall g, ok (solve_requirements f g)) -> ok (type_requirements (S f) x).
Proof.
  intros f x Hsolve s a s' Hi H. cbn [type_requirements] in H.
  apply bind_inv in H as (c & s1 & H1 & H). unfold cached_reqs in H1.
  apply bind_inv in H1 as (s0 & s0' & E1 & E2). injection E1 as <- <-. injection E2 as <- <-.
  destruct (nget (st_reqs s) x) as [l|] eqn:Hc.
  { injection H as <- <-. split; [exact Hi | apply fr_refl]. }
  apply bind_inv in H as (g & s2 & H2 & H). unfold sym in H2.
  destruct (nget (st_syms s) x) as [g'|] eqn:Hx; [|discriminate]. injection H2 as <- <-.
  assert (Hset : forall s1 l, Inv s1 -> nget (st_syms s1) x = Some g' ->
            (forall b, load_world b = Good (st_w s1) -> NoDup l /\
               (kind_plain (gs_kind g') = true -> forall t, In t l <-> A.AssumeTypes (W0 (st_w s1)) x t) /\
               (gs_kind g' = KType \/ gs_kind g' = KUnit -> l = [])) ->
            Inv (s_reqs (fun m => nset m x l) s1) /\ Fr s1 (s_reqs (fun m => nset m x l) s1)).
  { intros s1 l [Ja Jf Jd Jl Jfr Jr] Hx1 Hl. unfold s_reqs. cbn -[nset nget]. split.
    - constructor; cbn -[nset nget]; try assumption.
      intros b Hb y l' Hy. cbn -[nset nget] in *. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y x) as [->|Hne].
      + injection Hy as <-. destruct (Hl b Hb) as (Nd & P1 & P2). split; [exact Nd|]. exists g'. auto.
      + exact (Jr b Hb y l' Hy).
    - constructor; cbn; auto; intros; congruence. }
  assert (Hplain : forall out s3, assume_type_syms g' s = Good (out, s3) ->
            Inv s3 /\ Fr s s3 /\ forall b, load_world b = Good (st_w s3) ->
              NoDup out /\ forall t, In t out <-> A.AssumeTypes (W0 (st_w s3)) x t).
  { intros out s3 H3.
    assert (Ok3 : ok (assume_type_syms g')) by (unfold assume_type_syms; okt).
    destruct (Ok3 s out s3 Hi H3) as [I3 F3]. split; [exact I3|]. split; [exact F3|].
    intros b Hb. rewrite (fr_w _ _ F3) in Hb |- *.
    destruct (nget (w_syms (st_w s)) x) as [gw|] eqn:Hw.
    - pose proof (iv_loader _ Hi b Hb x gw Hw) as Egw. rewrite Hx in Egw. injection Egw as <-.
      pose proof (iv_ids _ Hi x g' Hx) as Eid.
      assert (Hgw : nget (w_syms (st_w s)) (gs_id g') = Some g') by (rewrite Eid; exact Hw).
      destruct (load_world_assume_types b (st_w s) (fun _ => []) (fun _ => 0) Hb s g' out s3 eq_refl
                  (iv_loader _ Hi b Hb) Hgw H3) as (_ & Nd & Ex).
      rewrite Eid in Ex. split; [exact Nd | exact Ex].
    - pose proof (iv_fresh _ Hi b Hb x g' Hw Hx) as Ea. unfold assume_type_syms in H3. rewrite Ea in H3.
      cbn in H3. injection H3 as <- <-.
      split; [constructor|]. intro t. split; [intros []|].
      intros (d & pa & D & _). cbn [A.w_decl W0 W_read] in D. rewrite Hw in D. discriminate D. }
  destruct (gs_kind g') eqn:Kg.
  1, 2: (* a type or a unit: none *)
    apply bind_inv in H as (u & s3 & H3 & H); unfold set_reqs, upd in H3; injection H3 as <- <-;
    injection H as <- <-;
    (refine (Hset s [] Hi Hx _); intros b Hb; split; [constructor|]; split; [intro E; discriminate E | reflexivity]).
  1, 2: (* an alias or a tag: solve_requirements, then read the cache *)
    apply bind_inv in H as (u & s3 & H3 & H);
    destruct (Hsolve g' s u s3 Hi H3) as [I3 F3];
    apply bind_inv in H as (c' & s4 & H4 & H); unfold cached_reqs in H4;
    apply bind_inv in H4 as (s5 & s5' & E3 & E4); injection E3 as <- <-; injection E4 as <- <-;
    injection H as <- <-; split; [exact I3 | exact F3].
  all: (* a val, fn, context, method or functor: assume_type_syms, cached *)
    apply bind_inv in H as (out & s3 & H3 & H);
    apply bind_inv in H as (u & s4 & H4 & H); unfold set_reqs, upd in H4; injection H4 as <- <-;
    injection H as <- <-;
    destruct (Hplain out s3 H3) as (I3 & F3 & Hp);
    assert (Hl : forall b, load_world b = Good (st_w s3) -> NoDup out /\
               (kind_plain (gs_kind g') = true -> forall t, In t out <-> A.AssumeTypes (W0 (st_w s3)) x t) /\
               (gs_kind g' = KType \/ gs_kind g' = KUnit -> out = []))
      by (intros b Hb; destruct (Hp b Hb) as [Nd Ex]; rewrite Kg; split; [exact Nd|];
          split; [intros _; exact Ex | intros [E|E]; discriminate E]);
    rewrite Kg in Hl;
    destruct (Hset s3 out I3 (fr_keep _ _ F3 x g' Hx) Hl) as [I4 F4];
    split; [exact I4 | exact (fr_trans _ _ _ F3 F4)].
Qed.

Lemma ok_free_type_syms_group : forall fuel,
  (forall t mi out, ok (free_type_syms fuel t mi out)) /\
  (forall s, ok (type_requirements fuel s)) /\
  (forall root, ok (solve_requirements fuel root)) /\
  (forall g, ok (listed_params fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ _))); fintros; cbn [free_type_syms type_requirements solve_requirements listed_params]; okt. }
  destruct IH as (I0 & I1 & I2 & I3).
  refine (conj _ (conj _ (conj _ _))).
  - fintros; cbn [free_type_syms]; okt.
  - intro s. apply ok_type_requirements_step. exact I2.
  - fintros; cbn [solve_requirements]; okt.
  - fintros; cbn [listed_params]; okt.
Qed.

Lemma ok_free_type_syms : forall fuel t mi out, ok (free_type_syms fuel t mi out).
Proof. intros fuel t mi out. exact ((proj1 (ok_free_type_syms_group fuel)) t mi out). Qed.
#[export] Hint Resolve ok_free_type_syms : okdb.

Lemma ok_type_requirements : forall fuel s, ok (type_requirements fuel s).
Proof. intros fuel s. exact ((proj1 (proj2 (ok_free_type_syms_group fuel))) s). Qed.
#[export] Hint Resolve ok_type_requirements : okdb.

Lemma ok_solve_requirements : forall fuel root, ok (solve_requirements fuel root).
Proof. intros fuel root. exact ((proj1 (proj2 (proj2 (ok_free_type_syms_group fuel)))) root). Qed.
#[export] Hint Resolve ok_solve_requirements : okdb.

Lemma ok_listed_params : forall fuel g, ok (listed_params fuel g).
Proof. intros fuel g. exact ((proj2 (proj2 (proj2 (ok_free_type_syms_group fuel)))) g). Qed.
#[export] Hint Resolve ok_listed_params : okdb.

Lemma ok_show_go_group : forall fuel,
  (forall t, ok (show_go fuel t)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [show_go]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [show_go]; okt.
Qed.

Lemma ok_show_go : forall fuel t, ok (show_go fuel t).
Proof. intros fuel t. exact ((ok_show_go_group fuel) t). Qed.
#[export] Hint Resolve ok_show_go : okdb.

Lemma ok_unify_group : forall fuel,
  (forall e mi a b why, ok (unify fuel e mi a b why)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [unify]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [unify]; okt.
Qed.

Lemma ok_unify : forall fuel e mi a b why, ok (unify fuel e mi a b why).
Proof. intros fuel e mi a b why. exact ((ok_unify_group fuel) e mi a b why). Qed.
#[export] Hint Resolve ok_unify : okdb.

Lemma ok_elab_type_group : forall fuel,
  (forall t mi tm e this, ok (elab_type fuel t mi tm e this)) /\
  (forall e mi bs outer, ok (app_subst fuel e mi bs outer)) /\
  (forall mi g tm e, ok (symbol_type fuel mi g tm e)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; okt. }
  destruct IH as (I0 & I1 & I2).
  refine (conj _ (conj _ _)); fintros; cbn [elab_type app_subst symbol_type]; okt.
Qed.

Lemma ok_elab_type : forall fuel t mi tm e this, ok (elab_type fuel t mi tm e this).
Proof. intros fuel t mi tm e this. exact ((proj1 (ok_elab_type_group fuel)) t mi tm e this). Qed.
#[export] Hint Resolve ok_elab_type : okdb.

Lemma ok_app_subst : forall fuel e mi bs outer, ok (app_subst fuel e mi bs outer).
Proof. intros fuel e mi bs outer. exact ((proj1 (proj2 (ok_elab_type_group fuel))) e mi bs outer). Qed.
#[export] Hint Resolve ok_app_subst : okdb.

Lemma ok_symbol_type : forall fuel mi g tm e, ok (symbol_type fuel mi g tm e).
Proof. intros fuel mi g tm e. exact ((proj2 (proj2 (ok_elab_type_group fuel))) mi g tm e). Qed.
#[export] Hint Resolve ok_symbol_type : okdb.


Lemma ok_result_mentions_go_group : forall fuel,
  (forall self mi specs out, ok (result_mentions_go fuel self mi specs out)) /\
  (forall g, ok (functor_fixed_go fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; okt. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [result_mentions_go functor_fixed_go]; okt.
Qed.

Lemma ok_result_mentions_go : forall fuel self mi specs out, ok (result_mentions_go fuel self mi specs out).
Proof. intros fuel self mi specs out. exact ((proj1 (ok_result_mentions_go_group fuel)) self mi specs out). Qed.
#[export] Hint Resolve ok_result_mentions_go : okdb.

Lemma ok_functor_fixed_go : forall fuel g, ok (functor_fixed_go fuel g).
Proof. intros fuel g. exact ((proj2 (ok_result_mentions_go_group fuel)) g). Qed.
#[export] Hint Resolve ok_functor_fixed_go : okdb.

Lemma ok_add_symbol_item_group : forall fuel,
  (forall e mi g subst needs, ok (add_symbol_item fuel e mi g subst needs)) /\
  (forall e mi sp outer needs, ok (add_spec fuel e mi sp outer needs)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; okt. }
  destruct IH as (I0 & I1).
  refine (conj _ _); fintros; cbn [add_symbol_item add_spec]; okt.
Qed.

Lemma ok_add_symbol_item : forall fuel e mi g subst needs, ok (add_symbol_item fuel e mi g subst needs).
Proof. intros fuel e mi g subst needs. exact ((proj1 (ok_add_symbol_item_group fuel)) e mi g subst needs). Qed.
#[export] Hint Resolve ok_add_symbol_item : okdb.

Lemma ok_add_spec : forall fuel e mi sp outer needs, ok (add_spec fuel e mi sp outer needs).
Proof. intros fuel e mi sp outer needs. exact ((proj2 (ok_add_symbol_item_group fuel)) e mi sp outer needs). Qed.
#[export] Hint Resolve ok_add_spec : okdb.

Lemma ok_ety_ty_go_group : forall fuel,
  (forall t, ok (ety_ty_go fuel t)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [ety_ty_go]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [ety_ty_go]; okt.
Qed.

Lemma ok_ety_ty_go : forall fuel t, ok (ety_ty_go fuel t).
Proof. intros fuel t. exact ((ok_ety_ty_go_group fuel) t). Qed.
#[export] Hint Resolve ok_ety_ty_go : okdb.

Lemma ok_slots_go_group : forall fuel,
  (forall e t visiting, ok (slots_go fuel e t visiting)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [slots_go]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [slots_go]; okt.
Qed.

Lemma ok_slots_go : forall fuel e t visiting, ok (slots_go fuel e t visiting).
Proof. intros fuel e t visiting. exact ((ok_slots_go_group fuel) e t visiting). Qed.
#[export] Hint Resolve ok_slots_go : okdb.

Lemma ok_expand_spec_group : forall fuel,
  (forall mi sp out, ok (expand_spec fuel mi sp out)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [expand_spec]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [expand_spec]; okt.
Qed.

Lemma ok_expand_spec : forall fuel mi sp out, ok (expand_spec fuel mi sp out).
Proof. intros fuel mi sp out. exact ((ok_expand_spec_group fuel) mi sp out). Qed.
#[export] Hint Resolve ok_expand_spec : okdb.

Lemma ok_functor_total_group : forall fuel,
  (forall g, ok (functor_total fuel g)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [functor_total]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [functor_total]; okt.
Qed.

Lemma ok_functor_total : forall fuel g, ok (functor_total fuel g).
Proof. intros fuel g. exact ((ok_functor_total_group fuel) g). Qed.
#[export] Hint Resolve ok_functor_total : okdb.

Lemma ok_provide_spec_group : forall fuel,
  (forall e fg mi sp outer out, ok (provide_spec fuel e fg mi sp outer out)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [provide_spec]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [provide_spec]; okt.
Qed.

Lemma ok_provide_spec : forall fuel e fg mi sp outer out, ok (provide_spec fuel e fg mi sp outer out).
Proof. intros fuel e fg mi sp outer out. exact ((ok_provide_spec_group fuel) e fg mi sp outer out). Qed.
#[export] Hint Resolve ok_provide_spec : okdb.

Lemma ok_family_leaves_ok_group : forall fuel,
  (forall mi ps specs, ok (family_leaves_ok fuel mi ps specs)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [family_leaves_ok]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [family_leaves_ok]; okt.
Qed.

Lemma ok_family_leaves_ok : forall fuel mi ps specs, ok (family_leaves_ok fuel mi ps specs).
Proof. intros fuel mi ps specs. exact ((ok_family_leaves_ok_group fuel) mi ps specs). Qed.
#[export] Hint Resolve ok_family_leaves_ok : okdb.

Lemma ok_family_leaves_group : forall fuel,
  (forall e mi specs outer, ok (family_leaves fuel e mi specs outer)).
Proof.
  induction fuel as [|fuel IH].
  { idtac; fintros; cbn [family_leaves]; okt. }
  pose proof IH as I0.
  idtac; fintros; cbn [family_leaves]; okt.
Qed.

Lemma ok_family_leaves : forall fuel e mi specs outer, ok (family_leaves fuel e mi specs outer).
Proof. intros fuel e mi specs outer. exact ((ok_family_leaves_group fuel) e mi specs outer). Qed.
#[export] Hint Resolve ok_family_leaves : okdb.

Lemma ok_synth_group : forall fuel,
  (forall c e x exp, ok (synth fuel c e x exp)) /\
  (forall c e a exp, ok (synth_arg fuel c e a exp)) /\
  (forall c e b exp, ok (synth_block fuel c e b exp)) /\
  (forall c e ss, ok (check_stmts fuel c e ss)) /\
  (forall c e s, ok (check_stmt fuel c e s)) /\
  (forall c e y, ok (check_bool fuel c e y)) /\
  (forall c e x exp, ok (synth_if fuel c e x exp)) /\
  (forall c e s arms exp, ok (synth_match fuel c e s arms exp)) /\
  (forall g, ok (const_val fuel g)) /\
  (forall c e p a, ok (synth_path fuel c e p a)) /\
  (forall c e nm sg args, ok (check_args fuel c e nm sg args)) /\
  (forall c e p a args, ok (synth_call fuel c e p a args)) /\
  (forall c e p a fs, ok (synth_record fuel c e p a fs)) /\
  (forall c e o n, ok (synth_field fuel c e o n)) /\
  (forall c e o p args, ok (synth_method fuel c e o p args)) /\
  (forall c e ox oty m args, ok (provided_call fuel c e ox oty m args)) /\
  (forall c e fg th want, ok (apply_family fuel c e fg th want)) /\
  (forall c e info, ok (demand_needs fuel c e info)) /\
  (forall c e lm n op operand args, ok (synth_operator fuel c e lm n op operand args)) /\
  (forall c e o n args why, ok (lang_call fuel c e o n args why)) /\
  (forall c e ds why, ok (int_literal fuel c e ds why)) /\
  (forall c e v why, ok (float_of fuel c e v why)) /\
  (forall c e text, ok (synth_literal fuel c e text)) /\
  (forall c e sp y, ok (check_bind fuel c e sp y)) /\
  (forall c e y, ok (expect_fn_provider fuel c e y)) /\
  (forall c e pv wanted kind, ok (match_fn_sig fuel c e pv wanted kind)) /\
  (forall pv, ok (wrapper fuel pv)) /\
  (forall tg, ok (forwarder fuel tg)) /\
  (forall g, ok (lower_body fuel g)) /\
  (forall c e ps rt body bound sg this what, ok (lambda_provider fuel c e ps rt body bound sg this what)) /\
  (forall c e sp, ok (apply_functor fuel c e sp)) /\
  (forall c e g, ok (apply_body fuel c e g)) /\
  (forall c e fg, ok (apply_abstract fuel c e fg)) /\
  (forall c e fg y, ok (bind_functor fuel c e fg y)) /\
  (forall fg bg, ok (link_body fuel fg bg)).
Proof.
  induction fuel as [|fuel IH].
  { refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okt. }
  destruct IH as (I0 & I1 & I2 & I3 & I4 & I5 & I6 & I7 & I8 & I9 & I10 & I11 & I12 & I13 & I14 & I15 & I16 & I17 & I18 & I19 & I20 & I21 & I22 & I23 & I24 & I25 & I26 & I27 & I28 & I29 & I30 & I31 & I32 & I33 & I34).
  refine (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ (conj _ _)))))))))))))))))))))))))))))))))); fintros; cbn [synth synth_arg synth_block check_stmts check_stmt check_bool synth_if synth_match const_val synth_path check_args synth_call synth_record synth_field synth_method provided_call apply_family demand_needs synth_operator lang_call int_literal float_of synth_literal check_bind expect_fn_provider match_fn_sig wrapper forwarder lower_body lambda_provider apply_functor apply_body apply_abstract bind_functor link_body]; okt.
Qed.

Lemma ok_synth : forall fuel c e x exp, ok (synth fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (ok_synth_group fuel)) c e x exp). Qed.
#[export] Hint Resolve ok_synth : okdb.

Lemma ok_synth_arg : forall fuel c e a exp, ok (synth_arg fuel c e a exp).
Proof. intros fuel c e a exp. exact ((proj1 (proj2 (ok_synth_group fuel))) c e a exp). Qed.
#[export] Hint Resolve ok_synth_arg : okdb.

Lemma ok_synth_block : forall fuel c e b exp, ok (synth_block fuel c e b exp).
Proof. intros fuel c e b exp. exact ((proj1 (proj2 (proj2 (ok_synth_group fuel)))) c e b exp). Qed.
#[export] Hint Resolve ok_synth_block : okdb.

Lemma ok_check_stmts : forall fuel c e ss, ok (check_stmts fuel c e ss).
Proof. intros fuel c e ss. exact ((proj1 (proj2 (proj2 (proj2 (ok_synth_group fuel))))) c e ss). Qed.
#[export] Hint Resolve ok_check_stmts : okdb.

Lemma ok_check_stmt : forall fuel c e s, ok (check_stmt fuel c e s).
Proof. intros fuel c e s. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))) c e s). Qed.
#[export] Hint Resolve ok_check_stmt : okdb.

Lemma ok_check_bool : forall fuel c e y, ok (check_bool fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))) c e y). Qed.
#[export] Hint Resolve ok_check_bool : okdb.

Lemma ok_synth_if : forall fuel c e x exp, ok (synth_if fuel c e x exp).
Proof. intros fuel c e x exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))) c e x exp). Qed.
#[export] Hint Resolve ok_synth_if : okdb.

Lemma ok_synth_match : forall fuel c e s arms exp, ok (synth_match fuel c e s arms exp).
Proof. intros fuel c e s arms exp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))) c e s arms exp). Qed.
#[export] Hint Resolve ok_synth_match : okdb.

Lemma ok_const_val : forall fuel g, ok (const_val fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))) g). Qed.
#[export] Hint Resolve ok_const_val : okdb.

Lemma ok_synth_path : forall fuel c e p a, ok (synth_path fuel c e p a).
Proof. intros fuel c e p a. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))) c e p a). Qed.
#[export] Hint Resolve ok_synth_path : okdb.

Lemma ok_check_args : forall fuel c e nm sg args, ok (check_args fuel c e nm sg args).
Proof. intros fuel c e nm sg args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))) c e nm sg args). Qed.
#[export] Hint Resolve ok_check_args : okdb.

Lemma ok_synth_call : forall fuel c e p a args, ok (synth_call fuel c e p a args).
Proof. intros fuel c e p a args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))) c e p a args). Qed.
#[export] Hint Resolve ok_synth_call : okdb.

Lemma ok_synth_record : forall fuel c e p a fs, ok (synth_record fuel c e p a fs).
Proof. intros fuel c e p a fs. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))) c e p a fs). Qed.
#[export] Hint Resolve ok_synth_record : okdb.

Lemma ok_synth_field : forall fuel c e o n, ok (synth_field fuel c e o n).
Proof. intros fuel c e o n. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))) c e o n). Qed.
#[export] Hint Resolve ok_synth_field : okdb.

Lemma ok_synth_method : forall fuel c e o p args, ok (synth_method fuel c e o p args).
Proof. intros fuel c e o p args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))) c e o p args). Qed.
#[export] Hint Resolve ok_synth_method : okdb.

Lemma ok_provided_call : forall fuel c e ox oty m args, ok (provided_call fuel c e ox oty m args).
Proof. intros fuel c e ox oty m args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))) c e ox oty m args). Qed.
#[export] Hint Resolve ok_provided_call : okdb.

Lemma ok_apply_family : forall fuel c e fg th want, ok (apply_family fuel c e fg th want).
Proof. intros fuel c e fg th want. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))) c e fg th want). Qed.
#[export] Hint Resolve ok_apply_family : okdb.

Lemma ok_demand_needs : forall fuel c e info, ok (demand_needs fuel c e info).
Proof. intros fuel c e info. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))) c e info). Qed.
#[export] Hint Resolve ok_demand_needs : okdb.

Lemma ok_synth_operator : forall fuel c e lm n op operand args, ok (synth_operator fuel c e lm n op operand args).
Proof. intros fuel c e lm n op operand args. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))) c e lm n op operand args). Qed.
#[export] Hint Resolve ok_synth_operator : okdb.

Lemma ok_lang_call : forall fuel c e o n args why, ok (lang_call fuel c e o n args why).
Proof. intros fuel c e o n args why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))) c e o n args why). Qed.
#[export] Hint Resolve ok_lang_call : okdb.

Lemma ok_int_literal : forall fuel c e ds why, ok (int_literal fuel c e ds why).
Proof. intros fuel c e ds why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))) c e ds why). Qed.
#[export] Hint Resolve ok_int_literal : okdb.

Lemma ok_float_of : forall fuel c e v why, ok (float_of fuel c e v why).
Proof. intros fuel c e v why. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))) c e v why). Qed.
#[export] Hint Resolve ok_float_of : okdb.

Lemma ok_synth_literal : forall fuel c e text, ok (synth_literal fuel c e text).
Proof. intros fuel c e text. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))) c e text). Qed.
#[export] Hint Resolve ok_synth_literal : okdb.

Lemma ok_check_bind : forall fuel c e sp y, ok (check_bind fuel c e sp y).
Proof. intros fuel c e sp y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))) c e sp y). Qed.
#[export] Hint Resolve ok_check_bind : okdb.

Lemma ok_expect_fn_provider : forall fuel c e y, ok (expect_fn_provider fuel c e y).
Proof. intros fuel c e y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))) c e y). Qed.
#[export] Hint Resolve ok_expect_fn_provider : okdb.

Lemma ok_match_fn_sig : forall fuel c e pv wanted kind, ok (match_fn_sig fuel c e pv wanted kind).
Proof. intros fuel c e pv wanted kind. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))) c e pv wanted kind). Qed.
#[export] Hint Resolve ok_match_fn_sig : okdb.

Lemma ok_wrapper : forall fuel pv, ok (wrapper fuel pv).
Proof. intros fuel pv. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))) pv). Qed.
#[export] Hint Resolve ok_wrapper : okdb.

Lemma ok_forwarder : forall fuel tg, ok (forwarder fuel tg).
Proof. intros fuel tg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))) tg). Qed.
#[export] Hint Resolve ok_forwarder : okdb.

Lemma ok_lower_body : forall fuel g, ok (lower_body fuel g).
Proof. intros fuel g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))) g). Qed.
#[export] Hint Resolve ok_lower_body : okdb.

Lemma ok_lambda_provider : forall fuel c e ps rt body bound sg this what, ok (lambda_provider fuel c e ps rt body bound sg this what).
Proof. intros fuel c e ps rt body bound sg this what. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))) c e ps rt body bound sg this what). Qed.
#[export] Hint Resolve ok_lambda_provider : okdb.

Lemma ok_apply_functor : forall fuel c e sp, ok (apply_functor fuel c e sp).
Proof. intros fuel c e sp. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))))) c e sp). Qed.
#[export] Hint Resolve ok_apply_functor : okdb.

Lemma ok_apply_body : forall fuel c e g, ok (apply_body fuel c e g).
Proof. intros fuel c e g. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))) c e g). Qed.
#[export] Hint Resolve ok_apply_body : okdb.

Lemma ok_apply_abstract : forall fuel c e fg, ok (apply_abstract fuel c e fg).
Proof. intros fuel c e fg. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel)))))))))))))))))))))))))))))))))) c e fg). Qed.
#[export] Hint Resolve ok_apply_abstract : okdb.

Lemma ok_bind_functor : forall fuel c e fg y, ok (bind_functor fuel c e fg y).
Proof. intros fuel c e fg y. exact ((proj1 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))))) c e fg y). Qed.
#[export] Hint Resolve ok_bind_functor : okdb.

Lemma ok_link_body : forall fuel fg bg, ok (link_body fuel fg bg).
Proof. intros fuel fg bg. exact ((proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (proj2 (ok_synth_group fuel))))))))))))))))))))))))))))))))))) fg bg). Qed.
#[export] Hint Resolve ok_link_body : okdb.


Lemma ok_elab_program : ok elab_program.
Proof. unfold elab_program. okt. Qed.

(* ======================================================================== *)
(* The frame of a whole elaboration (WorldSound.v's TODO_syms_frame and      *)
(* TODO_fld_injective)                                                       *)
(* ======================================================================== *)

Definition s_init (w : Elab.world) : st :=
  St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0 false false [] (lang_modules w) [].

Lemma load_world_syms : forall b w, load_world b = Good w ->
  0 < w_nsyms w /\ forall i, w_nsyms w <= i -> nget (w_syms w) i = None.
Proof.
  intros b w H. unfold load_world in H. apply rbind_good in H as ([e st0] & Hl & H). injection H as <-.
  destruct (load_spec b _ _ _ _ _ _ Hl (linv_init b)) as (L & _).
  split; [exact (li_pos _ _ L) | exact (li_above _ _ L)].
Qed.

Theorem elab_frame : forall b w p s, load_world b = Good w -> elab_program (s_init w) = Good (p, s) ->
  st_w s = w /\
  (* the loader's symbols are kept, unchanged *)
  (forall i g, nget (w_syms w) i = Some g -> nget (st_syms s) i = Some g) /\
  (* every symbol elaboration makes is no source symbol, and is fresh in the
     reading of the bundle (Audit.v Part V's `Fresh`) *)
  (forall i g, nget (w_syms w) i = None -> nget (st_syms s) i = Some g ->
     gs_origin g <> OSource /\ forall R F, A.Fresh (W_read w R F) i) /\
  (* record fields are numbered 1, 2, …, so fld_ext of them is injective *)
  Numbered (st_flds s) /\
  (forall a c, fld_ext (st_flds s) (List.length (st_flds s)) a = fld_ext (st_flds s) (List.length (st_flds s)) c -> a = c) /\
  (* every requirement list it cached is without repeats, of a symbol, and
     for a val, fn, method, context or functor Part V's AssumeTypes, for a
     type or unit none *)
  ReqOK s.
Proof.
  intros b w p s Hw He. destruct (load_world_syms b w Hw) as [Hpos Habove].
  assert (I0 : Inv (s_init w)).
  { constructor; cbn [st_next st_syms st_flds st_w st_reqs s_init].
    - exact Habove.
    - split; [constructor | reflexivity].
    - exact (load_world_ids b w Hw).
    - intros b' Hb' i g Hg. exact Hg.
    - intros b' Hb' i g Hn Hg. rewrite Hn in Hg. discriminate.
    - intros b' Hb' x l Hx. destruct x; discriminate Hx. }
  destruct (ok_elab_program _ _ _ I0 He) as [[_ Nf _ _ _ Ir] [Fw _ Fk Fn Fa _]].
  cbn [st_w st_syms st_next s_init] in Fw, Fk, Fn, Fa.
  split; [exact Fw|]. split; [exact Fk|]. split.
  - intros i g Hn Hg. split; [exact (Fn i g Hn Hg)|]. intros R F. split.
    + cbn [A.w_decl W_read]. rewrite Hn. reflexivity.
    + pose proof (Fa i g Hn Hg). lia.
  - split; [exact Nf|]. split; [destruct (numbered_inj _ Nf) as [Ninj Nb]; exact (fld_ext_inj _ _ Ninj Nb)|].
    apply (Ir b). rewrite Fw. exact Hw.
Qed.

Corollary elaborate_frame : forall b p, elaborate b = Good p ->
  exists w s, load_world b = Good w /\ elab_program (s_init w) = Good (p, s) /\
  st_w s = w /\
  (forall i g, nget (w_syms w) i = Some g -> nget (st_syms s) i = Some g) /\
  (forall i g, nget (w_syms w) i = None -> nget (st_syms s) i = Some g ->
     gs_origin g <> OSource /\ forall R F, A.Fresh (W_read w R F) i) /\
  Numbered (st_flds s) /\
  (forall a c, fld_ext (st_flds s) (List.length (st_flds s)) a = fld_ext (st_flds s) (List.length (st_flds s)) c -> a = c) /\
  ReqOK s.
Proof.
  intros b p H. unfold elaborate in H. destruct (load_world b) as [w|m] eqn:Hw; [|discriminate].
  cbn [rbind] in H. change (match elab_program (s_init w) with Good (p, _) => Good p | Bad m => Bad m end = Good p) in H.
  destruct (elab_program (s_init w)) as [[p' s]|m] eqn:He; [|discriminate].
  injection H as <-. exists w, s. split; [reflexivity|]. split; [exact He|].
  exact (elab_frame b w p' s Hw He).
Qed.
