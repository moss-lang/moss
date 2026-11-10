(* ------------------------------------------------------------------------ *)
(* ElabFrameCore.v — what every step of the elaborator leaves alone (Track A,  *)
(* WorldSound.v's TODO_syms_frame): it never changes the loader's world,   *)
(* never changes or removes a symbol, makes only non-source symbols, and   *)
(* numbers record fields 1, 2, … in order of first use.                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable.
Import ListNotations.

Local Strategy opaque [Nat.of_num_uint].

(* The cached requirement lists (st_reqs), as far as they are local facts:
   without repeats, of a symbol in force, and for a val, fn, method,
   context or functor Part V's AssumeTypes, for a type or unit none. *)
Definition W0 (w : LoadWorld.world) : A.world := W_read w (fun _ => []) (fun _ => 0).

Definition kind_plain (k : kind) : bool :=
  match k with KTag | KAlias | KType | KUnit => false | _ => true end.

Definition ReqOK (s : st) : Prop :=
  forall x l, nget (st_reqs s) x = Some l -> NoDup l /\ exists g, nget (st_syms s) x = Some g /\
    (kind_plain (gs_kind g) = true -> forall t, In t l <-> A.AssumeTypes (W0 (st_w s)) x t) /\
    (gs_kind g = KType \/ gs_kind g = KUnit -> l = []).

(* The cached requirement lists of tags and aliases, against the loader's
   world: closed and least (ReqsSolve.v). *)
Definition RelOK (s : st) : Prop :=
  forall b, load_world b = Good (st_w s) -> RC (W0r (st_w s)) (st_reqs s) /\ RL (W0r (st_w s)) (st_reqs s).

Lemma relok_same : forall s s', st_w s' = st_w s -> st_reqs s' = st_reqs s -> RelOK s -> RelOK s'.
Proof. intros s s' E1 E2 H b Hb. rewrite E1, E2 in *. exact (H b Hb). Qed.

(* The invariant of a state, and what a step keeps. *)
Record Inv (s : st) : Prop := {
  iv_above : forall i, st_next s <= i -> nget (st_syms s) i = None;
  iv_flds : Numbered (st_flds s);
  iv_ids : forall i g, nget (st_syms s) i = Some g -> gs_id g = i;
  iv_loader : forall b, load_world b = Good (st_w s) ->
    forall i g, nget (w_syms (st_w s)) i = Some g -> nget (st_syms s) i = Some g;
  iv_fresh : forall b, load_world b = Good (st_w s) ->
    forall i g, nget (w_syms (st_w s)) i = None -> nget (st_syms s) i = Some g -> gs_assumes g = [];
  iv_reqs : forall b, load_world b = Good (st_w s) -> ReqOK s;
  (* a symbol elaboration makes is never a tag or a unit, so the head of a
     nominal type is the loader's (BodyData.v) *)
  iv_kind : forall b, load_world b = Good (st_w s) ->
    forall i g, nget (w_syms (st_w s)) i = None -> nget (st_syms s) i = Some g -> gs_kind g <> KTag /\ gs_kind g <> KUnit
}.

(* The locals numbered so far: distinct names, numbered 0, 1, … in order
   (BodySound.v reads a body's frame off them). *)
Definition VShape (s : st) : Prop :=
  NoDup (map fst (st_vars s)) /\ map snd (st_vars s) = seq 0 (List.length (st_vars s)) /\
  st_nvar s = List.length (st_vars s).

(* What a step does to the body-level state: locals only numbered anew,
   bodies only emitted (prepended), unsupported constructs only noted, the
   library modules unchanged. *)
Record FrX (s s' : st) : Prop := {
  fx_vars : exists l, st_vars s' = st_vars s ++ l;
  fx_vshape : VShape s -> VShape s';
  fx_fns : exists l, st_fns s' = l ++ st_fns s;
  fx_funs : exists l, st_funs s' = l ++ st_funs s;
  fx_unsup : exists l, st_unsup s' = st_unsup s ++ l;
  fx_lang : st_lang s' = st_lang s;
  (* defined vals' cache only grows (BodySound's Later reads its entries) *)
  fx_consts : forall k r, nget (st_consts s) k = Some r -> nget (st_consts s') k = Some r
}.

Lemma frx_same : forall s s', st_vars s' = st_vars s -> st_nvar s' = st_nvar s -> st_fns s' = st_fns s ->
  st_funs s' = st_funs s -> st_unsup s' = st_unsup s -> st_lang s' = st_lang s -> st_consts s' = st_consts s -> FrX s s'.
Proof.
  intros s s' E1 E2 E3 E4 E5 E6 E7. constructor.
  - exists []. rewrite app_nil_r. exact E1.
  - unfold VShape. rewrite E1, E2. auto.
  - exists []. exact E3.
  - exists []. exact E4.
  - exists []. rewrite app_nil_r. exact E5.
  - exact E6.
  - rewrite E7. auto.
Qed.

Lemma frx_trans : forall s1 s2 s3, FrX s1 s2 -> FrX s2 s3 -> FrX s1 s3.
Proof.
  intros s1 s2 s3 [(l1 & V1) S1 (f1 & N1) (g1 & G1) (u1 & U1) L1 K1] [(l2 & V2) S2 (f2 & N2) (g2 & G2) (u2 & U2) L2 K2].
  constructor.
  - exists (l1 ++ l2). rewrite V2, V1, app_assoc. reflexivity.
  - auto.
  - exists (f2 ++ f1). rewrite N2, N1, app_assoc. reflexivity.
  - exists (g2 ++ g1). rewrite G2, G1, app_assoc. reflexivity.
  - exists (u1 ++ u2). rewrite U2, U1, app_assoc. reflexivity.
  - congruence.
  - auto.
Qed.

#[export] Hint Extern 1 (FrX _ _) => (apply frx_same; reflexivity) : core.

Record Fr0 (s s' : st) : Prop := {
  fr_w : st_w s' = st_w s;
  fr_next : st_next s <= st_next s';
  fr_keep : forall i g, nget (st_syms s) i = Some g -> nget (st_syms s') i = Some g;
  fr_new : forall i g, nget (st_syms s) i = None -> nget (st_syms s') i = Some g -> gs_origin g <> OSource;
  fr_new_above : forall i g, nget (st_syms s) i = None -> nget (st_syms s') i = Some g -> st_next s <= i;
  fr_flds : forall f n, sassoc f (st_flds s) = Some n -> sassoc f (st_flds s') = Some n;
  fr_x : FrX s s'
}.

Record Fr (s s' : st) : Prop := {
  fr_0 :> Fr0 s s';
  fr_rel : RelOK s -> RelOK s';
  (* a cached requirement list never changes (ReqsStable.v) *)
  fr_ext : Ext (st_reqs s) (st_reqs s')
}.

Lemma fr0_refl : forall s, Fr0 s s.
Proof. intro s. constructor; auto; intros i g H1 H2; congruence. Qed.

Lemma fr_refl : forall s, Fr s s.
Proof. intro s. constructor; [apply fr0_refl | auto | apply ext_refl]. Qed.

Lemma fr_of0 : forall s s', Fr0 s s' -> st_w s' = st_w s -> st_reqs s' = st_reqs s -> Fr s s'.
Proof. intros s s' F E1 E2. constructor; [exact F | exact (relok_same s s' E1 E2) | rewrite E2; apply ext_refl]. Qed.

Lemma fr0_trans : forall s1 s2 s3, Fr0 s1 s2 -> Fr0 s2 s3 -> Fr0 s1 s3.
Proof.
  intros s1 s2 s3 [A1 B1 C1 D1 G1 E1 X1] [A2 B2 C2 D2 G2 E2 X2]. constructor; auto.
  - congruence.
  - lia.
  - intros i g H1 H3. destruct (nget (st_syms s2) i) as [g'|] eqn:E.
    + rewrite (C2 i g' E) in H3. injection H3 as <-. exact (D1 i g' H1 E).
    + exact (D2 i g E H3).
  - intros i g H1 H3. destruct (nget (st_syms s2) i) as [g'|] eqn:E.
    + rewrite (C2 i g' E) in H3. injection H3 as <-. exact (G1 i g' H1 E).
    + pose proof (G2 i g E H3). lia.
  - exact (frx_trans _ _ _ X1 X2).
Qed.

Lemma fr_trans : forall s1 s2 s3, Fr s1 s2 -> Fr s2 s3 -> Fr s1 s3.
Proof.
  intros s1 s2 s3 [F1 R1 X1] [F2 R2 X2]. constructor; [|auto|exact (ext_trans _ _ _ X1 X2)]. revert F1 F2. apply fr0_trans.
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
             st_reqs (snd (f s)) = st_reqs s /\ FrX s (snd (f s))) ->
  ok (fun s => Good (f s)).
Proof.
  intros A f Hf s a s' [Ia If Id Il Ifr Ir Ik] H. injection H as E. destruct (Hf s) as (E1 & E2 & E3 & E4 & E5 & E6).
  rewrite E in E1, E2, E3, E4, E5, E6. cbn in E1, E2, E3, E4, E5, E6.
  split; [constructor; [rewrite E2, E3; exact Ia | rewrite E4; exact If | rewrite E3; exact Id
                       | rewrite E1, E3; exact Il | rewrite E1, E3; exact Ifr
                       | intros b Hb; rewrite E1 in Hb; unfold ReqOK; rewrite E1, E3, E5; exact (Ir b Hb)
                       | rewrite E1, E3; exact Ik]|].
  apply fr_of0; [|exact E1 | exact E5].
  constructor; [exact E1 | lia | rewrite E3; auto | rewrite E3; intros; congruence | rewrite E3; intros; congruence | rewrite E4; auto | exact E6].
Qed.

Lemma ok_reify_update : forall f, ok (upd (s_reify f)).
Proof.
  intro f. unfold upd. apply ok_keep. intro s. cbn.
  split; [reflexivity|]. split; [reflexivity|].
  split; [reflexivity|]. split; [reflexivity|].
  split; [reflexivity|]. apply frx_same; reflexivity.
Qed.

Lemma ok_reify_code : forall A owner (m : M A), ok m -> ok (in_reify_code owner m).
Proof.
  intros A owner m Hm. unfold in_reify_code.
  apply ok_bind; [apply ok_get|]. intro prior_state.
  apply ok_bind; [apply ok_reify_update|]. intro u.
  apply ok_bind; [exact Hm|]. intro a.
  apply ok_bind; [apply ok_reify_update|]. intro u'. apply ok_ret.
Qed.

Lemma ok_fresh : ok fresh.
Proof.
  intros s a s' [Ia If Id Il Ifr Ir Ik] H. unfold fresh in H. injection H as <- <-. cbn.
  split; [constructor; cbn; [intros i Hi; apply Ia; lia | exact If | exact Id | exact Il | exact Ifr | exact Ir | exact Ik]|].
  apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; intros; congruence.
Qed.

Lemma ok_fresh_marker:forall e,ok(fresh_marker e).
Proof.
 intros e s a s' [Ia If Id Il Ifr Ir Ik] H;unfold fresh_marker in H;injection H as <- <-.
 pose proof(Nat.le_max_l(st_next s)(marker_floor e))as Bound.
 split;[constructor;cbn;[intros i Hi;apply Ia;lia|exact If|exact Id|exact Il|exact Ifr|exact Ir|exact Ik]|].
 apply fr_of0;[|reflexivity|reflexivity]. constructor;cbn;auto;try lia;intros;congruence.
Qed.
Lemma ok_fresh_marker_for:forall e r,ok(fresh_marker_for e r).
Proof.
 intros e r s a s' [Ia If Id Il Ifr Ir Ik] H;unfold fresh_marker_for in H;injection H as <- <-.
 pose proof(Nat.le_max_l(st_next s)(Nat.max(marker_floor e)(fold_right(fun g top=>Nat.max(S g)top)0(ety_gens r))))as Bound.
 split;[constructor;cbn;[intros i Hi;apply Ia;lia|exact If|exact Id|exact Il|exact Ifr|exact Ir|exact Ik]|].
 apply fr_of0;[|reflexivity|reflexivity]. constructor;cbn;auto;try lia;intros;congruence.
Qed.

Lemma ok_new_sym : forall mi n k d o, o <> OSource -> k <> KTag /\ k <> KUnit -> ok (new_sym mi n k d o).
Proof.
  intros mi n k d o Ho Hk s a s' [Ia If Id Il Ifr Ir Ik] H. unfold new_sym in H.
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
    + intros b Hb i g Hn Hg. cbn -[nset nget] in *. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)) as [->|]; [injection Hg as <-; exact Hk | exact (Ik b Hb i g Hn Hg)].
  - apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn -[nset nget]; auto.
    + intros i g Hg. rewrite nget_nset_other; [exact Hg|]. intro E. subst i. rewrite (Ia (st_next s) (le_n _)) in Hg. discriminate.
    + intros i g Hn Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)); [|congruence].
      injection Hg as <-. exact Ho.
    + intros i g Hn Hg. rewrite nget_nset in Hg. destruct (Nat.eqb_spec i (st_next s)); [lia|congruence].
Qed.

Lemma ok_fld_id : forall f, ok (fld_id f).
Proof.
  intros f s a s' [Ia If Id Il Ifr Ir Ik] H. pose proof (fld_id_numbered f s a s' H If) as [Nn _].
  unfold fld_id in H. cbn in H. destruct (sassoc f (st_flds s)) as [m|] eqn:E.
  - injection H as <- <-. split; [constructor; assumption | apply fr_refl].
  - injection H as <- <-. unfold s_flds. cbn. split; [constructor; cbn; assumption|].
    apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; try (intros; congruence).
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

(* The four steps that change the body-level state. *)
Lemma ok_with_vars : forall A init (m : M A), ok m -> ok (with_vars init m).
Proof.
  intros A init m Hm s a s' Hi H. unfold with_vars in H.
  apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-.
  apply bind_inv in H as (r & s3 & H3 & H).
  apply bind_inv in H as (u' & s4 & E2 & H). unfold upd in E2. injection E2 as <- <-.
  injection H as <- <-.
  assert (Hi2 : Inv (s_vars init (List.length init) s)) by (destruct Hi; constructor; assumption).
  destruct (Hm _ _ _ Hi2 H3) as [Hi3 [[F1 F2 F3 F4 F5 F6 [_ _ X3 X4 X5 X6 X7]] Fr3 Fx3]].
  split; [destruct Hi3; constructor; assumption|]. constructor.
  - constructor; cbn [s_vars st_w st_next st_syms st_flds st_vars st_nvar st_fns st_funs st_unsup st_lang st_consts] in *; auto.
    constructor; cbn [s_vars st_w st_next st_syms st_flds st_vars st_nvar st_fns st_funs st_unsup st_lang st_consts] in *; auto.
    + exists []. rewrite app_nil_r. reflexivity.
  - intro Hr. apply Fr3. exact Hr.
  - exact Fx3.
Qed.

Lemma ok_local_var : forall x, ok (local_var x).
Proof.
  intros x s a s' Hi H. unfold local_var in H. apply bind_inv in H as (s0 & s1 & E0 & H). injection E0 as <- <-.
  destruct (sassoc x (st_vars s)) as [n|] eqn:Ex.
  - injection H as <- <-. split; [exact Hi | apply fr_refl].
  - apply bind_inv in H as (u & s2 & E1 & H). unfold upd in E1. injection E1 as <- <-. injection H as <- <-.
    split; [destruct Hi; constructor; assumption|].
    apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; try (intros; congruence).
    constructor; cbn; auto.
    + exists [(x, st_nvar s)]. reflexivity.
    + intros (Nd & Sn & Nv). unfold VShape. cbn. rewrite !map_app, !length_app. cbn.
      split; [|split].
      * apply NoDup_app; [exact Nd | constructor; [intros []|constructor] |].
        intros y Hy [<-|[]]. apply in_map_iff in Hy as ([y' k] & E & Hy). cbn in E. subst y'.
        clear -Ex Hy. induction (st_vars s) as [|[y k'] r IH]; [destruct Hy|]. cbn in Ex.
        destruct (String.eqb_spec x y); [discriminate|]. destruct Hy as [E|Hy]; [congruence | exact (IH Ex Hy)].
      * rewrite Sn, seq_app. cbn. rewrite Nv. reflexivity.
      * lia.
    + exists []. reflexivity.
    + exists []. reflexivity.
    + exists []. rewrite app_nil_r. reflexivity.
Qed.

Lemma ok_emit_fn : forall g x, ok (emit_fn g x).
Proof.
  intros g x s a s' Hi H. unfold emit_fn, upd in H. injection H as <- <-.
  split; [destruct Hi; constructor; assumption|].
  apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; try (intros; congruence).
  constructor; cbn; auto.
  - exists []. rewrite app_nil_r. reflexivity.
  - exists [(g, x)]. reflexivity.
  - exists []. reflexivity.
  - exists []. rewrite app_nil_r. reflexivity.
Qed.

Lemma ok_funs_cons : forall x, ok (upd (s_funs (fun l => x :: l))).
Proof.
  intros x s a s' Hi H. unfold upd in H. injection H as <- <-.
  split; [destruct Hi; constructor; assumption|].
  apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; try (intros; congruence).
  constructor; cbn; auto.
  - exists []. rewrite app_nil_r. reflexivity.
  - exists []. reflexivity.
  - exists [x]. reflexivity.
  - exists []. rewrite app_nil_r. reflexivity.
Qed.

(* const_val's cache write: a new entry only *)
Lemma ok_consts_add : forall k x,
  ok (upd (s_consts (fun m => match nget m k with Some _ => m | None => nset m k x end))).
Proof.
  intros k x s a s' Hi H. unfold upd in H. injection H as <- <-.
  split; [destruct Hi; constructor; assumption|].
  apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn -[nset nget]; auto; try (intros; congruence).
  constructor; cbn -[nset nget]; auto.
  - exists []. rewrite app_nil_r. reflexivity.
  - exists []. reflexivity.
  - exists []. reflexivity.
  - exists []. rewrite app_nil_r. reflexivity.
  - intros j r Hj. destruct (nget (st_consts s) k) as [y|] eqn:Ek; [exact Hj|].
    rewrite nget_nset. destruct (Nat.eqb_spec j k) as [->|]; [congruence | exact Hj].
Qed.

Lemma ok_unsupported : forall why, ok (unsupported why).
Proof.
  intros why s a s' Hi H. unfold unsupported in H. injection H as <- <-.
  split; [destruct Hi; constructor; assumption|].
  apply fr_of0; [|reflexivity|reflexivity]. constructor; cbn; auto; try (intros; congruence).
  constructor; cbn; auto.
  - exists []. rewrite app_nil_r. reflexivity.
  - exists []. reflexivity.
  - exists []. reflexivity.
  - exists [why]. reflexivity.
Qed.

Ltac reader :=
  apply ok_reader; let s := fresh "s" in let a := fresh "a" in let s' := fresh "s'" in let H := fresh "H" in
  intros s a s' H;
  repeat (match type of H with context [match ?x with _ => _ end] => destruct x end);
  try discriminate H; congruence.

Create HintDb okdb.
#[export]Hint Resolve ok_fresh_marker ok_fresh_marker_for:okdb.
#[export] Hint Resolve ok_ret ok_fail ok_get ok_lift ok_fresh ok_fld_id ok_local_var ok_emit_fn ok_unsupported : okdb.
#[export] Hint Extern 1 (ok (new_sym _ _ _ _ _)) => (apply ok_new_sym; [discriminate | split; discriminate]) : okdb.

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
  | |- ok (upd (s_reify _)) => apply ok_reify_update
  | |- ok (upd (s_funs (fun l => ?x :: l))) => apply ok_funs_cons
  | |- ok (upd (s_consts _)) => apply ok_consts_add
  | |- ok (upd _) => unfold upd; apply ok_keep; intro; split; [reflexivity|split; [reflexivity|split; [reflexivity|split; [reflexivity|split; [reflexivity|auto]]]]]
  | |- ok (fun s => Good (_, _)) => apply ok_keep; intro; split; [reflexivity|split; [reflexivity|split; [reflexivity|split; [reflexivity|split; [reflexivity|auto]]]]]
  | |- ok (in_reify_code _ _) => apply ok_reify_code
  | |- ok (upd (s_reify _)) => apply ok_reify_update
  | |- ok (with_vars _ _) => apply ok_with_vars
  | |- ok (fun s => _) => reader
  | |- ok ?t =>
      first [ solve [auto with okdb]
            | (lazymatch t with ?F _ => is_fix F end; fixind1)
            | (lazymatch t with ?F _ _ _ => is_fix F end; fixind3)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

(* The frame without the requirement cache's closedness: what a step of
   solve_requirements keeps in its middle, where the group is not yet
   closed. *)
Definition ok0 {A} (m : M A) : Prop :=
  forall s a s', Inv s -> m s = Good (a, s') -> Inv s' /\ Fr0 s s'.

Arguments ok0 : simpl never.

Lemma ok_ok0 : forall A (m : M A), ok m -> ok0 m.
Proof. intros A m H s a s' Hi Hm. destruct (H s a s' Hi Hm) as [Hi' F]. exact (conj Hi' (fr_0 _ _ F)). Qed.

Lemma ok0_bind : forall A B (m : M A) (k : A -> M B), ok0 m -> (forall a, ok0 (k a)) -> ok0 (mbind m k).
Proof.
  intros A B m k Hm Hk s b s'' Hi H. apply bind_inv in H as (a & s' & H1 & H2).
  destruct (Hm _ _ _ Hi H1) as [Hi' F1]. destruct (Hk a _ _ _ Hi' H2) as [Hi'' F2].
  split; [exact Hi'' | exact (fr0_trans _ _ _ F1 F2)].
Qed.

Lemma ok0_ret : forall A (a : A), ok0 (ret a).
Proof. intros. apply ok_ok0, ok_ret. Qed.

Lemma ok0_mmap : forall A B (f : A -> M B) l, (forall x, ok0 (f x)) -> ok0 (mmap f l).
Proof.
  intros A B f l Hf. induction l as [|x r IH]; cbn [mmap]; [apply ok0_ret|].
  apply ok0_bind; [apply Hf | intro y; apply ok0_bind; [exact IH | intro; apply ok0_ret]].
Qed.

Lemma ok0_mfold : forall A B (f : A -> B -> M A) l a, (forall a x, ok0 (f a x)) -> ok0 (mfold f a l).
Proof.
  intros A B f l. induction l as [|x r IH]; intros a Hf; cbn [mfold]; [apply ok0_ret|].
  apply ok0_bind; [apply Hf | intro a'; apply IH; exact Hf].
Qed.

Lemma ok0_miter : forall A (f : A -> M unit) l, (forall x, ok0 (f x)) -> ok0 (miter f l).
Proof.
  intros A f l Hf. induction l as [|x r IH]; cbn [miter]; [apply ok0_ret|].
  apply ok0_bind; [apply Hf | intro; exact IH].
Qed.

Ltac fixind1_0 :=
  lazymatch goal with |- ok0 (?F ?N) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N as n; intro n; induction n as [|n IHn]; cbn beta iota end.
Ltac fixind3_0 :=
  lazymatch goal with |- ok0 (?F ?N ?a ?b) =>
    let n := fresh "n" in let IHn := fresh "IHn" in
    generalize N, a, b; intro n; induction n as [|n IHn]; intros; cbn beta iota end.

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
Lemma ok0_set_nominal_reqs : forall x l, ok0 (set_nominal_reqs x l).
Proof.
  intros x l s a s2 Hi H. unfold set_nominal_reqs in H.
  apply bind_inv in H as (g & s1 & H1 & H). unfold sym in H1.
  destruct (nget (st_syms s) x) as [g'|] eqn:Hx; [|discriminate]. injection H1 as <- <-.
  destruct (kind_eqb (gs_kind g') KTag || kind_eqb (gs_kind g') KAlias) eqn:Kn.
  - unfold set_reqs, upd in H. injection H as <- <-. destruct Hi as [Ia If Id Il Ifr Ir Ik]. unfold s_reqs. cbn -[nset nget].
    split.
    + constructor; cbn -[nset nget]; try assumption.
      intros b Hb y l' Hy. cbn -[nset nget] in *. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y x) as [->|Hne].
      * injection Hy as <-. split; [apply NoDup_nodup|]. exists g'. split; [exact Hx|].
        apply orb_true_iff in Kn as [Kn|Kn];
          (apply kind_eqb_spec in Kn; rewrite Kn; cbn; split; [discriminate | intros [E|E]; discriminate E]).
      * exact (Ir b Hb y l' Hy).
    + constructor; cbn; auto; intros; congruence.
  - injection H as <- <-. split; [exact Hi | apply fr0_refl].
Qed.

Ltac okt0 :=
  repeat (fintros; lazymatch goal with
  | |- ok0 (mbind _ _) => apply ok0_bind
  | |- ok0 (mmap _ _) => apply ok0_mmap
  | |- ok0 (mfold _ _ _) => apply ok0_mfold
  | |- ok0 (miter _ _) => apply ok0_miter
  | |- ok0 (set_nominal_reqs _ _) => apply ok0_set_nominal_reqs
  | |- ok0 (fun s => _) => apply ok_ok0; reader
  | |- ok0 ?t =>
      first [ solve [auto]
            | solve [apply ok_ok0; auto with okdb]
            | (lazymatch t with ?F _ => is_fix F end; fixind1_0)
            | (lazymatch t with ?F _ _ _ => is_fix F end; fixind3_0)
            | match t with context [match ?x with _ => _ end] => destruct x end
            | let h := head_of t in progress unfold h ]
  end).

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
            nget (st_reqs s1) x = None -> nominal_k (gs_kind g') = false ->
            (forall b, load_world b = Good (st_w s1) -> NoDup l /\
               (kind_plain (gs_kind g') = true -> forall t, In t l <-> A.AssumeTypes (W0 (st_w s1)) x t) /\
               (gs_kind g' = KType \/ gs_kind g' = KUnit -> l = [])) ->
            Inv (s_reqs (fun m => nset m x l) s1) /\ Fr s1 (s_reqs (fun m => nset m x l) s1)).
  { intros s1 l [Ja Jf Jd Jl Jfr Jr Jk] Hx1 Hc1 Hk1 Hl. unfold s_reqs. cbn -[nset nget]. split.
    - constructor; cbn -[nset nget]; try assumption.
      intros b Hb y l' Hy. cbn -[nset nget] in *. rewrite nget_nset in Hy. destruct (Nat.eqb_spec y x) as [->|Hne].
      + injection Hy as <-. destruct (Hl b Hb) as (Nd & P1 & P2). split; [exact Nd|]. exists g'. auto.
      + exact (Jr b Hb y l' Hy).
    - constructor; [constructor; cbn; auto; intros; congruence| |cbn [st_reqs]; exact (ext_set_new _ _ _ _ (ext_refl _) Hc1)].
      intros Hr b Hb. cbn [st_w st_reqs] in Hb |- *. destruct (Hr b Hb) as [Hrc Hrl].
      apply (rel_set_plain_w b (st_w s1) (st_reqs s1) x l Hb Hrc Hrl Hc1).
      intros g0 Hg0. rewrite (Jl b Hb x g0 Hg0) in Hx1. injection Hx1 as <-. exact Hk1. }
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
    (refine (Hset s [] Hi Hx Hc ltac:(try rewrite Kg; reflexivity) _); intros b Hb; split; [constructor|]; split; [intro E; discriminate E | reflexivity]).
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
    pose proof (assume_type_syms_reader _ _ _ _ H3) as E3s;
    destruct (Hset s3 out I3 (fr_keep _ _ F3 x g' Hx) ltac:(rewrite E3s; exact Hc) ltac:(try rewrite Kg; reflexivity) Hl) as [I4 F4];
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
  - (* solve_requirements: its middle keeps only the frame (ok0); the
       cache's closedness is ReqsSolve.solve_rel *)
    intros root s a s' Hi H.
    assert (O0 : ok0 (solve_requirements (S fuel) root)) by (cbn [solve_requirements]; okt0).
    destruct (O0 s a s' Hi H) as [Hi' F0]. split; [exact Hi'|]. constructor; [exact F0| |exact (rq_solve_requirements _ root s a s' H)].
    intros Hr b Hb. rewrite (fr_w _ _ F0) in Hb |- *. destruct (Hr b Hb) as [Hrc Hrl].
    exact (solve_rel_w b (st_w s) fuel root s a s' Hb eq_refl (iv_loader _ Hi b Hb) Hrc Hrl H).
  - fintros; cbn [listed_params]; okt.
Qed.

Lemma ok_free_type_syms : forall fuel t mi out, ok (free_type_syms fuel t mi out).
Proof. intros fuel t mi out. exact ((proj1 (ok_free_type_syms_group fuel)) t mi out). Qed.
#[export] Hint Resolve ok_free_type_syms : okdb.

Lemma ok_type_requirements : forall fuel s, ok (type_requirements fuel s).
Proof. intros fuel s. exact ((proj1 (proj2 (ok_free_type_syms_group fuel))) s). Qed.
#[export] Hint Resolve ok_type_requirements : okdb.

Lemma reqs_frame_eq : forall x, reqs x = type_requirements 200 x.
Proof. reflexivity. Qed.
Lemma ok_reqs_frame : forall x, ok (reqs x).
Proof. intro x. rewrite reqs_frame_eq. apply ok_type_requirements. Qed.
#[export] Hint Resolve ok_reqs_frame : okdb.
Local Strategy opaque [reqs].

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

Lemma ok_check_literal_sig : forall e mi m oty args sg,
  ok (check_literal_sig e mi m oty args sg).
Proof.
  intros. unfold check_literal_sig. destruct (literal_sig_ok e oty args sg); okt.
Qed.
#[export] Hint Resolve ok_check_literal_sig : okdb.

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

Lemma ok_provide_result : forall e fg roles, ok (provide_result e fg roles).
Proof. intros; unfold provide_result; okt. Qed.
#[export] Hint Resolve ok_provide_result : okdb.

Lemma ok_fresh_target_output_view : forall e fg, ok (fresh_target_output_view e fg).
Proof. intros; unfold fresh_target_output_view; okt. Qed.
#[export] Hint Resolve ok_fresh_target_output_view : okdb.

Lemma ok_bind_fixed_present : forall mi e fg xs, ok (bind_fixed_present mi e fg xs).
Proof. intros; unfold bind_fixed_present; okt. Qed.
#[export] Hint Resolve ok_bind_fixed_present : okdb.

Lemma ok_copy_fixed_current : forall mi e fg xs, ok (copy_fixed_current mi e fg xs).
Proof. intros; unfold copy_fixed_current; okt. Qed.
#[export] Hint Resolve ok_copy_fixed_current : okdb.

Lemma ok_defined_link_inputs_compatible : forall mi site ef eg fg bg fixed fneeds gneeds,
  ok(defined_link_inputs_compatible mi site ef eg fg bg fixed fneeds gneeds).
Proof. intros;unfold defined_link_inputs_compatible;okt. Qed.
#[export]Hint Resolve ok_defined_link_inputs_compatible:okdb.

Lemma ok_check_link_fixed_current : forall mi site fg bg fixed,
  ok(check_link_fixed_current mi site fg bg fixed).
Proof. intros;unfold check_link_fixed_current;okt. Qed.
#[export] Hint Resolve ok_check_link_fixed_current : okdb.

Lemma ok_check_constant_effects : forall mi x, ok (check_constant_effects mi x).
Proof. intros; unfold check_constant_effects; okt. Qed.
#[export] Hint Resolve ok_check_constant_effects : okdb.

Lemma ok_check_provider_effects : forall mi bs, ok (check_provider_effects mi bs).
Proof. intros; unfold check_provider_effects; okt. Qed.
#[export] Hint Resolve ok_check_provider_effects : okdb.

Lemma ok_check_captured_reading : forall mi site prototype formal actual,
  ok(check_captured_reading mi site prototype formal actual).
Proof. intros;unfold check_captured_reading;okt. Qed.
#[export] Hint Resolve ok_check_captured_reading : okdb.

Lemma ok_copy_output_compatible : forall mi site ef eg fg bg fixed fneeds gneeds, ok (copy_output_compatible mi site ef eg fg bg fixed fneeds gneeds).
Proof. intros; unfold copy_output_compatible; okt. Qed.
#[export] Hint Resolve ok_copy_output_compatible : okdb.

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


(* Share the context walk's frame proof across callers and validation. *)
Lemma ok_merged_walk_frame : forall mi walk,
  (forall e, ok (walk e)) -> ok (merged_walk mi walk).
Proof. intros mi walk H. unfold merged_walk. okt. Qed.
#[export] Hint Resolve ok_merged_walk_frame : okdb.

Lemma ok_fn_info_frame : forall g, ok (fn_info g).
Proof.
  intro g0. unfold fn_info.
  apply ok_bind; [apply ok_get | intro snapshot].
  destruct (nget (st_info snapshot) (gs_id g0)) as [info|].
  - apply ok_ret.
  - apply ok_bind; [okt | intro g].
    cbn beta zeta.
    apply ok_bind.
    + apply ok_merged_walk_frame. intro e.
      destruct (gs_origin g); okt.
    + intros [e needs]. cbn beta iota zeta. okt.
Qed.
#[export] Hint Resolve ok_fn_info_frame : okdb.
Local Strategy opaque [fn_info].

Lemma ok_check_assumed_requirements_frame : forall mi e k,
  ok (check_assumed_requirements mi e k).
Proof. intros. unfold check_assumed_requirements. okt. Qed.
#[export] Hint Resolve ok_check_assumed_requirements_frame : okdb.

Lemma ok_check_assumes_frame : ok check_assumes.
Proof. unfold check_assumes. okt. Qed.
#[export] Hint Resolve ok_check_assumes_frame : okdb.
Local Strategy opaque [check_assumed_requirements check_assumes].

Lemma ok_remember_reify_constant : forall id start, ok (remember_reify_constant id start).
Proof. intros. unfold remember_reify_constant. okt. Qed.
#[export] Hint Resolve ok_remember_reify_constant : okdb.

Lemma ok_replay_reify_constant : forall id, ok (replay_reify_constant id).
Proof. intros. unfold replay_reify_constant. okt. Qed.
#[export] Hint Resolve ok_replay_reify_constant : okdb.

(* The executable initial state is shared by operational input invariants. *)
Lemma ok_fun_env : forall g, ok (fun_env g).
Proof. intros;unfold fun_env;okt. all:apply ok_merged_walk_frame;intros;okt. Qed.

Definition s_init (w : LoadWorld.world) : st :=
  St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0 false false [] (lang_modules w) [] (ReifyData.empty_reify_metadata ety key).

