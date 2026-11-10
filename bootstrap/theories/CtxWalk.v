(* ------------------------------------------------------------------------ *)
(* CtxWalk.v — the assume walk (Elab.add_symbol_item, add_spec) against    *)
(* Part V's ItemEntries (Track A, CtxOf from fn_info).                      *)
(*                                                                           *)
(* IE/IEs are API aliases of Part V's ItemEntries/ItemsEntries.  Their    *)
(* enclosing brackets ob stay apart from the threaded meanings tm, as the *)
(* elaborator keeps them.  The abstract-functor fixed-input guard follows *)
(* from the actual visitor and its successful availability check.         *)
(* Each run of the walk, from any merges U, relates the entries it makes   *)
(* (`lent`, the elaborator's, which `tr_ent` reads as Part V's) to its     *)
(* environment (WS): the types in force, the merges (triangular, extending *)
(* U, each respected by whatever respects U and unifies the entries), the  *)
(* vals, fns, methods and abstract functors in force and their entries,   *)
(* and, when the run made no new merge, that entries under one key agree   *)
(* up to the merges (a method's receiver key then ety_ok, so that tr reads *)
(* keys apart) and that each provision's recorded instantiation is the     *)
(* first entry's under its key (setdefault_app keeps the first).            *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ReqsStable ElabFrameCore BodySoundCore BodyTy Merge CanonTypes SourceClean ContextClean ContextWalkClean ResultMentionsCover ResultMentionsExact MethodRows.
Import ListNotations.
Local Open Scope list_scope.

Module A := ExecutionModel.Elaboration.
Notation asub := ExecutionModel.app_subst.
Notation tsubst := ExecutionModel.subst.

Local Strategy opaque [Nat.of_num_uint FUEL reqs type_requirements].

(* ======================================================================== *)
(* 1. ItemEntries with the enclosing brackets apart                          *)
(* ======================================================================== *)

Definition IE:=A.ItemEntries.
Definition IEs:=A.ItemsEntries.
Definition IEType:=@A.IEType.
Definition IENothing:=@A.IENothing.
Definition IEVal:=@A.IEVal.
Definition IEFn:=@A.IEFn.
Definition IEFun:=@A.IEFun.
Definition IEMeth:=@A.IEMeth.
Definition IECtx:=@A.IECtx.
Definition IsNil:=@A.IsNil.
Definition IsCons:=@A.IsCons.


(* ======================================================================== *)
(* 2. The elaborator's entries                                               *)
(* ======================================================================== *)

Inductive lent :=
| LTy (t : nat)
| LVal (v : nat) (y : ety) (a : Elab.inst)
| LFn (f : nat) (g : Elab.fsig) (a : Elab.inst)
| LMth (r : ety) (m : nat) (g : Elab.fsig) (a : Elab.inst)
| LFun (u : nat) (fx : list (nat * ety)).

(* ety_ok (BodySoundCore.v): a type tr reads apart from every other such type. *)

Section Walk.
Variable w : Elab.world. Context {P : xprogram}. Variables (R : nat -> list nat) (Fl : string -> nat) (b : bundle).
Hypothesis Hload : load_world b = Good w.

Notation WW := (BodySoundCore.W w R Fl).
Notation trF := (tr Fl).
Notation path_of mi := (gm_path (get_mod w mi)).

Definition tr_inst (a : Elab.inst) : A.inst := map (fun q => (fst q, option_map trF (snd q))) a.
Definition tr_sig (g : Elab.fsig) : A.fsig := A.FSig (map (fun p => trF (snd p)) (sg_params g)) (trF (sg_ret g)).

Definition raw_fixed_app (fx : list (nat * ety)) : Elab.inst :=
  map (fun q => (fst q, Some (snd q))) fx.
Definition tr_fixed (fx : list (nat * ety)) : ExecutionModel.subst :=
  map (fun q => (fst q, trF (snd q))) fx.

Definition tr_ent (l : lent) : A.entry :=
  match l with
  | LTy t => A.NTy t
  | LVal v y a => A.NVal v (trF y) (tr_inst a)
  | LFn f g a => A.NFn f (tr_sig g) (tr_inst a)
  | LMth r m g a => A.NMth (trF r) m (tr_sig g) (tr_inst a)
  | LFun u fx => A.NFun u (tr_fixed fx)
  end.

(* the key an entry is filed under in the environment e *)
Definition lkey (e : env) (l : lent) : option key :=
  match l with
  | LTy _ => None
  | LVal v _ _ => Some (KyVal v)
  | LFn f _ _ => Some (KyFn f)
  | LMth r m _ _ => Some (KyMth (canon e r) m)
  | LFun u _ => Some (KyFun u)
  end.

(* the instantiation an entry records, under the key it is filed under *)
Definition lapp (e : env) (l : lent) : option (key * Elab.inst) :=
  match l with
  | LVal v _ a => Some (KyVal v, a)
  | LFn f _ a => Some (KyFn f, a)
  | LMth r m _ a => Some (KyMth (canon e r) m, a)
  | LFun u fx => Some (KyFun u, raw_fixed_app fx)
  | _ => None
  end.

(* the first entry's instantiation under k: setdefault_app keeps the first
   one recorded, as the relation's ilook reads the first *)
Fixpoint lfirst (e : env) (k : key) (les : list lent) : option Elab.inst :=
  match les with
  | [] => None
  | l :: r => match lapp e l with
              | Some (k', a) => if key_eqb k k' then Some a else lfirst e k r
              | None => lfirst e k r
              end
  end.

(* a key the relation records an instantiation of (entries_cx's c_apps has
   none for a functor) *)
(* Compatibility name: every provision now retains an application. *)
Definition nofun (k : key) : bool := true.

(* two signatures alike up to the merges *)
Definition SigCanon (e : env) (g g0 : Elab.fsig) : Prop :=
  map (fun p => canon e (snd p)) (sg_params g) = map (fun p => canon e (snd p)) (sg_params g0) /\
  canon e (sg_ret g) = canon e (sg_ret g0).

(* the instantiation an entry of a val, fn or method records *)
Definition linst (l : lent) : option Elab.inst :=
  match l with LVal _ _ a | LFn _ _ a | LMth _ _ _ a => Some a | _ => None end.

(* a recorded instantiation is an entry's, under a key every respecting σ
   reads as that entry's (Δ4: unify_app equates the two) *)
Definition AppIn (e : env) (k : key) (a0 : Elab.inst) (les : list lent) : Prop :=
  match k with
  | KyVal v => exists y, In (LVal v y a0) les
  | KyFn f => exists g, In (LFn f g a0) les
  | KyMth r m => (exists r0 g, In (LMth r0 m g a0) les /\
      forall sb, RespE Fl sb (en_uf e) -> asub sb (trF r0) = asub sb (trF r)) /\ get_mth e r m <> None
  | KyFun _ => True
  end.

(* a run of the walk, from the merges U *)
Record WS (U : list (nat * ety)) (e : env) (tm : list (nat * ty)) (les : list lent) (needs : list key) : Prop := {
  ws_tn : forall k, A.nlook k tm = option_map trF (tn_get e k);
  ws_tnabs : forall k p, In (k, p) (en_tn e) -> fst p = YAbs k;
  ws_tri : Tri (en_uf e);
  ws_ext : exists nw, en_uf e = U ++ nw;
  ws_mg : forall sb, RespE Fl sb U -> A.Unifies sb (map tr_ent les) -> RespE Fl sb (en_uf e);
  ws_vals : forall v y a, In (LVal v y a) les -> exists y0 a0, nget (en_vals e) v = Some y0 /\
              In (LVal v y0 a0) les /\ (en_uf e = U -> canon e y = canon e y0);
  ws_vals_in : forall v y0, nget (en_vals e) v = Some y0 -> exists a0, In (LVal v y0 a0) les;
  ws_fns : forall f g a, In (LFn f g a) les -> sg_this g = None /\ exists g0 a0, nget (en_fns e) f = Some g0 /\
              In (LFn f g0 a0) les /\ (en_uf e = U -> SigCanon e g g0);
  ws_fns_in : forall f g0, nget (en_fns e) f = Some g0 -> exists a0, In (LFn f g0 a0) les;
  (* methods, filed under their receivers' canonical types: read only when
     the run made no new merge (what a later merge changes is a key) *)
  ws_mths : en_uf e = U -> forall r m g a, In (LMth r m g a) les -> ety_ok (canon e r) = true /\
              exists g0, get_mth e (canon e r) m = Some g0 /\ SigCanon e g g0;
  ws_mths_in : forall r m g0, get_mth e r m = Some g0 -> exists r0 a0, In (LMth r0 m g0 a0) les /\
              forall sb, RespE Fl sb (en_uf e) -> asub sb (trF r0) = asub sb (trF r);
  ws_needs : en_uf e = U -> forall k, In k needs <-> exists l, In l les /\ lkey e l = Some k;
  ws_nodup : NoDup needs;
  (* the abstract functors in force are the functor entries *)
  ws_funs : forall u, nget (en_funs e) u <> None <-> exists fx, In (LFun u fx) les;
  (* with no new merge, each provision's recorded instantiation is the first
     entry's under its key *)
  ws_apps : en_uf e = U -> forall k, nofun k = true -> option_map fst (get_app e k) = lfirst e k les;
  ws_mthis : forall r m g a, In (LMth r m g a) les -> sg_this g = Some r;
  ws_clean : ContextClean e;
  ws_method_unique : MethodRowsUnique e;
  ws_method_canonical : en_uf e = U -> MethodRowsCanonical e;
  (* Δ4: with no new merge, each entry's instantiation agrees with the first
     one under its key up to the merges *)
  ws_insts : en_uf e = U -> forall l k a, In l les -> lkey e l = Some k -> linst l = Some a ->
    exists a0, lfirst e k les = Some a0 /\ InstCanon e a0 a;
  ws_apps_in : forall k a0, option_map fst (get_app e k) = Some a0 -> AppIn e k a0 les
}.


(* ---- small facts ---- *)

Lemma unifies_app : forall sb l1 l2, A.Unifies sb (l1 ++ l2) -> A.Unifies sb l1.
Proof. intros sb l1 l2 H e1 e2 k H1 H2. apply H; apply in_or_app; left; assumption. Qed.

Lemma tn_get_set_tn : forall e t y st k, tn_get (set_tn e t y st) k = if Nat.eqb k t then Some y else tn_get e k.
Proof.
  intros e t y st k. unfold tn_get, set_tn. cbn [en_tn]. induction (en_tn e) as [|[a v] r IH]; cbn.
  - destruct (Nat.eqb k t); reflexivity.
  - destruct (Nat.eqb_spec t a) as [->|Ne]; cbn.
    + destruct (Nat.eqb k a); reflexivity.
    + destruct (Nat.eqb_spec k a) as [->|]; [|exact IH]. destruct (Nat.eqb_spec a t); [congruence | reflexivity].
Qed.

Lemma canon_same : forall e e' t, en_uf e' = en_uf e -> canon e' t = canon e t.
Proof. intros e e' t E. unfold canon. rewrite E. reflexivity. Qed.

Lemma lkey_same : forall e e' l, en_uf e' = en_uf e -> lkey e' l = lkey e l.
Proof. intros e e' [] E; cbn; try reflexivity. rewrite (canon_same e e' _ E). reflexivity. Qed.

Lemma key_eqb_true : forall k k', key_eqb k k' = true -> k = k'.
Proof.
  intros [x|x|t m|x] [y|y|t' m'|y] H; cbn in H; try discriminate;
    try (apply Nat.eqb_eq in H; subst; reflexivity).
  apply andb_true_iff in H as [H1 H2]. apply Nat.eqb_eq in H1. apply ety_eqb_true in H2. subst. reflexivity.
Qed.

Lemma key_mem_in : forall k l, key_mem k l = true <-> In k l.
Proof.
  intros k l. induction l as [|x r IH]; cbn; [split; [discriminate | intros []]|].
  rewrite orb_true_iff, IH. split.
  - intros [H|H]; [left; symmetry; exact (key_eqb_true _ _ H) | right; exact H].
  - intros [E|H]; [left; subst x | right; exact H].
    destruct k as [x|x|t m|x]; cbn; rewrite ?Nat.eqb_refl, ?ety_eqb_refl; reflexivity.
Qed.

Lemma key_eqb_refl : forall k, key_eqb k k = true.
Proof. intros [x|x|t m|x]; cbn; rewrite ?Nat.eqb_refl, ?ety_eqb_refl; reflexivity. Qed.

Lemma key_eqb_spec : forall k k', key_eqb k k' = true <-> k = k'.
Proof. intros k k'. split; [apply key_eqb_true | intros <-; apply key_eqb_refl]. Qed.

Lemma lapp_same : forall e e' l, en_uf e' = en_uf e -> lapp e' l = lapp e l.
Proof. intros e e' [] E; cbn; try reflexivity. rewrite (canon_same e e' _ E). reflexivity. Qed.

Lemma lfirst_same : forall e e' k les, en_uf e' = en_uf e -> lfirst e' k les = lfirst e k les.
Proof.
  intros e e' k les E. induction les as [|l r IH]; [reflexivity|]. cbn. rewrite (lapp_same e e' l E), IH. reflexivity.
Qed.

Lemma lfirst_app : forall e k les l, lfirst e k (les ++ [l]) =
  match lfirst e k les with
  | Some a => Some a
  | None => match lapp e l with Some (k', a) => if key_eqb k k' then Some a else None | None => None end
  end.
Proof.
  intros e k les l. induction les as [|l0 r IH]; cbn.
  - destruct (lapp e l) as [[k' a]|]; [destruct (key_eqb k k')|]; reflexivity.
  - destruct (lapp e l0) as [[k' a]|]; [destruct (key_eqb k k'); [reflexivity|] |]; exact IH.
Qed.

Lemma lkey_lapp : forall e l, lkey e l = None -> lapp e l = None.
Proof. intros e [] H; cbn in *; congruence. Qed.

Lemma lfirst_nokey : forall e k les l, lapp e l = None -> lfirst e k (les ++ [l]) = lfirst e k les.
Proof. intros e k les l H. rewrite lfirst_app, H. destruct (lfirst e k les); reflexivity. Qed.

Lemma assoc_aset_key : forall V k (v : V) l k',
  assoc key_eqb k' (aset key_eqb k v l) = if key_eqb k' k then Some v else assoc key_eqb k' l.
Proof.
  intros V k v l k'. induction l as [|[k0 v0] r IH]; cbn.
  - destruct (key_eqb k' k); reflexivity.
  - destruct (key_eqb k k0) eqn:E0; cbn.
    + apply key_eqb_true in E0. subst k0. destruct (key_eqb k' k); reflexivity.
    + destruct (key_eqb k' k0) eqn:E1; [|exact IH].
      apply key_eqb_true in E1. subst k0. destruct (key_eqb k' k) eqn:E2; [|reflexivity].
      apply key_eqb_true in E2. subst k. rewrite key_eqb_refl in E0. discriminate.
Qed.

(* setdefault_app records an instantiation only under a key that has none *)
Lemma setdefault_apps : forall e k a s e' s', setdefault_app e k a s = Good (e', s') ->
  forall k', option_map fst (get_app e' k') =
    if key_eqb k' k then match option_map fst (get_app e k) with Some a0 => Some a0 | None => Some a end
    else option_map fst (get_app e k').
Proof.
  intros e k a s e' s' H k'. unfold setdefault_app in H. destruct (get_app e k) as [p|] eqn:G.
  - injection H as <- _. destruct (key_eqb k' k) eqn:E; [|reflexivity]. apply key_eqb_true in E. subst. rewrite G. reflexivity.
  - apply bind_inv in H as (n & s1 & _ & H). injection H as <- _. unfold get_app, set_app. cbn [en_apps].
    rewrite assoc_aset_key. destruct (key_eqb k' k); reflexivity.
Qed.

Lemma add_need_in : forall needs k k', In k' (add_need needs k) <-> In k' needs \/ k' = k.
Proof.
  intros needs k k'. unfold add_need. destruct (key_mem k needs) eqn:E.
  - apply key_mem_in in E. split; [auto | intros [H| ->]; auto].
  - rewrite in_app_iff. cbn. split; intros [H|H]; auto; destruct H as [H|[]]; auto.
Qed.

Lemma add_need_nodup : forall needs k, NoDup needs -> NoDup (add_need needs k).
Proof.
  intros needs k N. unfold add_need. destruct (key_mem k needs) eqn:E; [exact N|].
  apply NoDup_app; [exact N | constructor; [intros []|constructor] |].
  intros x Hx [<-|[]]. apply key_mem_in in Hx. congruence.
Qed.

Lemma in_aset : forall K V (eqb : K -> K -> bool) k (v : V) l x, In x (aset eqb k v l) -> x = (k, v) \/ In x l.
Proof.
  intros K V eqb k v l x. induction l as [|[k' v'] r IH]; cbn; [intros [<-|[]]; left; reflexivity|].
  destruct (eqb k k'); cbn; intros [<-|H]; auto. destruct (IH H); auto.
Qed.

Lemma get_mth_same : forall e e' r m, en_mths e' = en_mths e -> get_mth e' r m = get_mth e r m.
Proof. intros e e' r m E. unfold get_mth. rewrite E. reflexivity. Qed.

Lemma ws_tn_get : forall U e tm les needs, WS U e tm les needs -> forall k y, tn_get e k = Some y -> y = YAbs k.
Proof.
  intros U e tm les needs Hws k y H. unfold tn_get in H. destruct (nassoc k (en_tn e)) as [p|] eqn:E; [|discriminate].
  injection H as <-. exact (ws_tnabs _ _ _ _ _ Hws k p (Merge.nassoc_in _ _ _ _ E)).
Qed.

Lemma sigcanon_same : forall e e1 g g0, en_uf e1 = en_uf e -> SigCanon e g g0 -> SigCanon e1 g g0.
Proof.
  intros e e1 g g0 E [P1 P2]. unfold SigCanon. rewrite !(canon_same e e1 _ E). split; [|exact P2].
  rewrite (map_ext _ _ (fun p => canon_same e e1 (snd p) E)), (map_ext (fun p => canon e1 (snd p)) _ (fun p => canon_same e e1 (snd p) E)).
  exact P1.
Qed.

(* ---- Δ4: instantiations ---- *)

Lemma instcanon_refl : forall e a, InstCanon e a a.
Proof.
  intros e a. split; [reflexivity|]. induction a as [|q r IH]; cbn; constructor; [reflexivity|exact IH].
Qed.

Lemma instcanon_same : forall e e' a a', en_uf e' = en_uf e -> InstCanon e a a' -> InstCanon e' a a'.
Proof.
  intros e e' a a' E [F C]. split; [exact F|]. eapply Forall_impl; [|exact C].
  intros p H. rewrite !(canon_same e e' _ E). exact H.
Qed.

Lemma appin_mono : forall e e' k a0 les l', (exists n, en_uf e' = en_uf e ++ n) ->
  (forall r m, get_mth e r m <> None -> get_mth e' r m <> None) ->
  AppIn e k a0 les -> AppIn e' k a0 (les ++ l').
Proof.
  intros e e' [v|f|r m|u] a0 les l' (n & En) Mm H; unfold AppIn in H |- *; try exact I.
  - destruct H as (y & H). exists y. apply in_or_app. left. exact H.
  - destruct H as (g & H). exists g. apply in_or_app. left. exact H.
  - destruct H as ((r0 & g & H & Eq) & Gm). split; [|exact (Mm r m Gm)]. exists r0, g. split; [apply in_or_app; left; exact H|].
    intros sb Hr. apply Eq. rewrite En in Hr. exact (resp_app Fl _ _ _ Hr).
Qed.

Lemma mths_same_mono : forall e e', en_mths e' = en_mths e -> forall r m, get_mth e r m <> None -> get_mth e' r m <> None.
Proof. intros e e' E r m H. rewrite (get_mth_same e e' r m E). exact H. Qed.

Lemma lfirst_keep : forall e k les l a0, lfirst e k les = Some a0 -> lfirst e k (les ++ [l]) = Some a0.
Proof. intros e k les l a0 H. rewrite lfirst_app, H. reflexivity. Qed.

Lemma inst_read_tr : forall sb a,
  A.inst_read sb (tr_inst a) = map (fun q => (fst q, asub sb (trF (inst_ety q)))) a.
Proof.
  intros sb a. unfold A.inst_read, tr_inst. rewrite map_map. apply map_ext. intros [x [y|]]; reflexivity.
Qed.

Lemma instresp_of_read : forall sb old a, A.inst_read sb (tr_inst old) = A.inst_read sb (tr_inst a) ->
  InstResp Fl sb old a.
Proof.
  intros sb old a E. rewrite !inst_read_tr in E. unfold InstResp. revert a E.
  induction old as [|q r IH]; intros [|q' r'] E; cbn in E |- *; try discriminate; constructor.
  - injection E as _ E _. exact E.
  - injection E as _ _ E. exact (IH r' E).
Qed.

(* the walk's environment changed only where the entries do not look *)
Lemma ws_env : forall U e e' tm les needs, WS U e tm les needs ->
  en_tn e' = en_tn e -> en_uf e' = en_uf e -> en_vals e' = en_vals e -> en_fns e' = en_fns e ->
  en_mths e' = en_mths e -> en_funs e' = en_funs e -> en_apps e' = en_apps e -> WS U e' tm les needs.
Proof.
  intros U e e' tm les needs [T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19] E1 E2 E3 E4 E5 E6 E7.
  assert (Etn : forall k, tn_get e' k = tn_get e k) by (intro k; unfold tn_get; rewrite E1; reflexivity).
  constructor.
  - intro k. rewrite Etn. apply T1.
  - rewrite E1. exact T2.
  - rewrite E2. exact T3.
  - rewrite E2. exact T4.
  - rewrite E2. exact T5.
  - intros v y a H. destruct (T6 v y a H) as (y0 & a0 & H1 & H2 & H3). exists y0, a0. rewrite E3. split; [exact H1|]. split; [exact H2|].
    intro E. rewrite !(canon_same e e' _ E2). rewrite E2 in E. exact (H3 E).
  - rewrite E3. exact T7.
  - intros f g a H. destruct (T8 f g a H) as (Ht & g0 & a0 & H1 & H2 & H3). split; [exact Ht|]. exists g0, a0.
    rewrite E4. split; [exact H1|]. split; [exact H2|]. intro E. rewrite E2 in E. exact (sigcanon_same e e' g g0 E2 (H3 E)).
  - rewrite E4. exact T9.
  - intros E r m g a H. rewrite E2 in E. destruct (Q1 E r m g a H) as (Ok & g0 & G & C).
    rewrite (canon_same e e' _ E2), (get_mth_same e e' _ _ E5). split; [exact Ok|]. exists g0.
    split; [exact G | exact (sigcanon_same e e' g g0 E2 C)].
  - intros r m g0 G. rewrite (get_mth_same e e' _ _ E5) in G. rewrite E2. exact (Q2 r m g0 G).
  - intros E k. rewrite E2 in E. rewrite (T10 E k). split; intros (l & Hl & Hk); exists l; split; auto;
      [rewrite (lkey_same e e' l E2) | rewrite <- (lkey_same e e' l E2)]; exact Hk.
  - exact T11.
  - rewrite E6. exact T12.
  - intros E k Hk. unfold get_app. rewrite E7, (lfirst_same e e' k les E2). rewrite E2 in E. exact (T13 E k Hk).
  - exact T14.
  - exact (context_clean_same e e' E1 E2 E3 E4 E5 T15).
  - intros m rows At. rewrite E5 in At. exact(T16 _ _ At).
  - intro E. rewrite E2 in E. exact(proj2(method_rows_same e e' E5 E2 T16(T17 E))).
  - intros E l k a Hl Hk Hi. rewrite E2 in E. rewrite (lkey_same e e' l E2) in Hk.
    destruct (T18 E l k a Hl Hk Hi) as (a0 & F & C). exists a0.
    rewrite (lfirst_same e e' k les E2). split; [exact F | exact (instcanon_same e e' _ _ E2 C)].
  - intros k a0 G. unfold get_app in G. rewrite E7 in G. fold (get_app e k) in G.
    rewrite <- (app_nil_r les). apply (appin_mono e e' k a0 les []); [exists []; rewrite app_nil_r; exact E2 | exact (mths_same_mono e e' E5) | exact (T19 k a0 G)].
Qed.

(* adding entries without keys or table changes *)
Lemma ws_les_nokey : forall U e tm les needs l, WS U e tm les needs -> lkey e l = None ->
  (forall v y a, l <> LVal v y a) -> (forall f g a, l <> LFn f g a) -> (forall r m g a, l <> LMth r m g a) ->
  WS U e tm (les ++ [l]) needs.
Proof.
  intros U e tm les needs l [T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19] Hk Hv Hf Hm.
  assert (I19 : forall k a0, option_map fst (get_app e k) = Some a0 -> AppIn e k a0 (les ++ [l])).
  { intros k a0 G. apply (appin_mono e e k a0 les [l]); [exists []; rewrite app_nil_r; reflexivity | exact (fun _ _ H => H) | exact (T19 k a0 G)]. }
  assert (I18 : en_uf e = U -> forall l' k a, In l' (les ++ [l]) -> lkey e l' = Some k -> linst l' = Some a ->
            exists a0, lfirst e k (les ++ [l]) = Some a0 /\ InstCanon e a0 a).
  { intros E l' k a Hl Hk' Hi. apply in_app_iff in Hl as [Hl|[<-|[]]]; [|congruence].
    destruct (T18 E l' k a Hl Hk' Hi) as (a0 & F & C). exists a0. split; [exact (lfirst_keep e k les l a0 F) | exact C]. }
  constructor; auto.
  - intros sb Hr Hu. apply T5; [exact Hr|]. rewrite map_app in Hu. exact (unifies_app _ _ _ Hu).
  - intros v y a H. apply in_app_iff in H as [H|[H|[]]]; [|exfalso; exact (Hv v y a H)].
    destruct (T6 v y a H) as (y0 & a0 & H1 & H2 & H3). exists y0, a0. split; [exact H1|]. split; [apply in_or_app; left; exact H2 | exact H3].
  - intros v y0 H. destruct (T7 v y0 H) as (a0 & H1). exists a0. apply in_or_app. left. exact H1.
  - intros f g a H. apply in_app_iff in H as [H|[H|[]]]; [|exfalso; exact (Hf f g a H)].
    destruct (T8 f g a H) as (Ht & g0 & a0 & H1 & H2 & H3). split; [exact Ht|]. exists g0, a0. split; [exact H1|].
    split; [apply in_or_app; left; exact H2 | exact H3].
  - intros f g0 H. destruct (T9 f g0 H) as (a0 & H1). exists a0. apply in_or_app. left. exact H1.
  - intros E r m g a H. apply in_app_iff in H as [H|[H|[]]]; [exact (Q1 E r m g a H) | exfalso; exact (Hm r m g a H)].
  - intros r m g0 G. destruct (Q2 r m g0 G) as (r0 & a0 & H1 & H2). exists r0, a0. split; [apply in_or_app; left; exact H1 | exact H2].
  - intros E k. rewrite (T10 E k). split; intros (l' & Hl & Hk').
    + exists l'. split; [apply in_or_app; left; exact Hl | exact Hk'].
    + apply in_app_iff in Hl as [Hl|[<-|[]]]; [exists l'; auto | congruence].
  - intro u. rewrite T12. split.
    + intros [fx H];exists fx;apply in_or_app;left;exact H.
    + intros [fx H];apply in_app_iff in H as[H|[Eq|[]]].
      * exists fx;exact H.
      * subst l;discriminate Hk.

  - intros E k' Hk'. rewrite (lfirst_nokey _ _ _ _ (lkey_lapp _ _ Hk)). exact (T13 E k' Hk').
  - intros r m g a Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]].
    + exact (T14 _ _ _ _ Hin).
    + exfalso. exact (Hm _ _ _ _ Eq).

Qed.


(* ---- frames ---- *)

Lemma bstep : forall A B (m : M A) (k : A -> M B) s r s'', ok m -> Inv s -> mbind m k s = Good (r, s'') ->
  exists a s1, m s = Good (a, s1) /\ k a s1 = Good (r, s'') /\ Inv s1 /\ Fr s s1.
Proof.
  intros A B m k s r s'' Hm Hi H. apply bind_inv in H as (a & s1 & H1 & H2).
  destruct (Hm s a s1 Hi H1) as [Hi1 F1]. exists a, s1. auto.

Qed.

Lemma ok_check_totality : forall mi g subst e, ok (check_totality mi g subst e).
Proof. intros. unfold check_totality. destruct subst; [apply ok_ret|]. apply ok_bind; [apply ok_reqs|]. intro rs. okt. Qed.
Lemma ok_provision_app : forall g tm meth, ok (provision_app g tm meth).
Proof. intros. unfold provision_app. apply ok_bind; [apply ok_reqs|]. intro rs. okt. Qed.
Lemma ok_unify_app : forall e mi k a nm, ok (unify_app e mi k a nm).
Proof. intros. unfold unify_app. okt. Qed.
Lemma ok_setdefault_app : forall e k a, ok (setdefault_app e k a).
Proof. intros. unfold setdefault_app. okt. Qed.
Lemma ok_fn_sig : forall g tm e this, ok (fn_sig g tm e this).
Proof. intros. unfold fn_sig. apply ok_bind; [apply ok_mmap; intro; apply ok_bind; [apply ok_ety_of | intro; apply ok_ret]|]. intro. apply ok_bind; [destruct (fn_ret g); [apply ok_ety_of | apply ok_ret] | intro; apply ok_ret]. Qed.

Lemma setdefault_env : forall e k a s e' s', setdefault_app e k a s = Good (e', s') ->
  en_tn e' = en_tn e /\ en_uf e' = en_uf e /\ en_vals e' = en_vals e /\ en_fns e' = en_fns e /\ en_mths e' = en_mths e /\
  en_funs e' = en_funs e.
Proof.
  intros e k a s e' s' H. unfold setdefault_app in H. destruct (get_app e k).
  - injection H as <- _. auto 7.
  - apply bind_inv in H as (n & s1 & _ & H). injection H as <- _. auto 7.
Qed.



(* the recorded instantiations after setdefault_app records (k, a) for l *)
Lemma appin_step : forall e eb e2 les l k a s s',
  (forall k0 a0, option_map fst (get_app e k0) = Some a0 -> AppIn e k0 a0 les) ->
  en_apps eb = en_apps e -> (exists n, en_uf eb = en_uf e ++ n) ->
  (forall r m, get_mth e r m <> None -> get_mth eb r m <> None) ->
  lapp e l = Some (k, a) -> (forall r m, k = KyMth r m -> get_mth eb r m <> None) ->
  setdefault_app eb k a s = Good (e2, s') ->
  forall k0 a0, option_map fst (get_app e2 k0) = Some a0 -> AppIn e2 k0 a0 (les ++ [l]).
Proof.
  intros e eb e2 les l k a s s' Old Ea Ex Mb La Mk Hsd k0 a0 G.
  destruct (setdefault_env _ _ _ _ _ _ Hsd) as (_ & D2 & _ & _ & D5 & _).
  assert (Ex2 : exists n, en_uf e2 = en_uf e ++ n) by (rewrite D2; exact Ex).
  assert (M2 : forall r m, get_mth e r m <> None -> get_mth e2 r m <> None)
    by (intros r m H; apply (mths_same_mono eb e2 D5); exact (Mb r m H)).
  assert (Gb : forall k1, get_app eb k1 = get_app e k1) by (intro k1; unfold get_app; rewrite Ea; reflexivity).
  rewrite (setdefault_apps _ _ _ _ _ _ Hsd k0), !Gb in G.
  destruct (key_eqb k0 k) eqn:Ek.
  - apply key_eqb_true in Ek. subst k0. destruct (option_map fst (get_app e k)) as [a1|] eqn:G1.
    + injection G as <-. exact (appin_mono e e2 k a1 les [l] Ex2 M2 (Old k a1 G1)).
    + injection G as <-. destruct l as [t|v y a'|f g a'|r m g a'|u fx]; cbn [lapp] in La; try discriminate;
        injection La as <- <-; unfold AppIn.
      * exists y. apply in_or_app. right. left. reflexivity.
      * exists g. apply in_or_app. right. left. reflexivity.
      * split; [|apply (mths_same_mono eb e2 D5); exact (Mk _ m eq_refl)].
        exists r, g. split; [apply in_or_app; right; left; reflexivity|]. intros sb Hr.
        rewrite D2 in Hr. destruct Ex as (n & En). rewrite En in Hr. apply resp_app in Hr.
        symmetry. rewrite canon_res, (resp_res Fl sb _ Hr). reflexivity.
      * exact I.
  - exact (appin_mono e e2 k0 a0 les [l] Ex2 M2 (Old k0 a0 G)).
Qed.

(* with no new merge, a new entry agrees with the first under its key *)
Lemma insts_new : forall U e eu e2 les l k a mi nm s0 s0' s s',
  (en_uf e = U -> forall k', nofun k' = true -> option_map fst (get_app e k') = lfirst e k' les) ->
  forall e1, en_apps e1 = en_apps e -> (exists n, en_uf e1 = en_uf e ++ n) -> (exists n, en_uf e = U ++ n) ->
  unify_app e1 mi k a nm s0 = Good (eu, s0') -> Tri (en_uf e1) ->
  setdefault_app eu k a s = Good (e2, s') -> lapp e2 l = Some (k, a) ->
  en_uf e2 = U -> exists a0, lfirst e2 k (les ++ [l]) = Some a0 /\ InstCanon e2 a0 a.
Proof.
  intros U e eu e2 les l k a mi nm s0 s0' s s' Ap e1 Ea Ex1 Ex0 Hu T1 Hsd La E.
  destruct (unify_app_spec Fl _ _ _ _ _ _ _ _ T1 Hu) as (_ & Uu & _ & Sm).
  destruct (setdefault_env _ _ _ _ _ _ Hsd) as (_ & D2 & _).
  assert (Eu : en_uf eu = U) by (rewrite <- D2; exact E).
  destruct Ex1 as (n1 & E1). destruct (us_ext _ _ Uu) as (n2 & E2). destruct Ex0 as (n0 & E0).
  rewrite Eu, E1, E0, <- !app_assoc in E2. pose proof (len_app_nil _ _ _ E2) as N.
  apply app_eq_nil in N as [-> N]. apply app_eq_nil in N as [-> ->]. rewrite !app_nil_r in *.
  assert (Ee : en_uf e2 = en_uf e) by congruence.
  rewrite (lfirst_same e e2 k _ Ee), lfirst_app.
  rewrite (lapp_same e e2 l Ee) in La. rewrite La, key_eqb_refl.
  destruct (lfirst e k les) as [a0|] eqn:F.
  - exists a0. split; [reflexivity|].
    assert (G : option_map fst (get_app e1 k) = Some a0)
      by (unfold get_app; rewrite Ea; fold (get_app e k); rewrite (Ap E0 k eq_refl); exact F).
    destruct (get_app e1 k) as [[old st]|] eqn:G1; [|discriminate]. injection G as ->.
    destruct (Sm a0 st eq_refl) as (_ & _ & Ci & _).
    apply (instcanon_same e1); [congruence|]. apply Ci. congruence.
  - exists a. split; [reflexivity | apply instcanon_refl].
Qed.

(* the relation's reading of the instantiation provision_app records *)
Lemma provision_app_rel : forall g tm tmr e s a s', Inv s -> provision_app g tm false s = Good (a, s') ->
  Later w P R Fl s' -> TmRel Fl tm tmr e -> A.kind_sym WW (gs_id g) <> Some A.DKMeth -> A.AppOf WW (gs_id g) tmr (tr_inst a).
Proof.
  intros g tm tmr e s a s' Hi H L Ht Hk. unfold provision_app in H. apply bind_inv in H as (rs & s1 & H1 & H).
  cbn beta iota in H. apply bind_inv in H as (rs' & s2 & H2 & H). injection H2 as <- <-. injection H as <- <-.
  assert (Er : R (gs_id g) = rs) by exact (reqs_R w R Fl _ s rs s1 Hi H1 L).
  exists rs. split; [|split; [intros _; exact (eq_sym Er) | intro E; contradiction]].
  unfold tr_inst. rewrite map_map. apply map_ext. intro r. cbn [fst snd]. f_equal. symmetry. apply (proj1 Ht).
Qed.

Lemma tmrel_walk : forall U e tm les needs subst ob, WS U e tm les needs -> map (trp Fl) subst = ob ->
  Forall (fun q => WFty (snd q)) subst -> TmRel Fl (subst ++ tymap e) (ob ++ tm) e.
Proof.
  intros U e tm les needs subst ob Hws Hob Hwf. apply (tmrel_app Fl); [|exact Hob | exact Hwf].
  apply tmrel_tymap; [|exact (ws_tn _ _ _ _ _ Hws)].
  intros t y H. rewrite (ws_tn_get _ _ _ _ _ Hws t y H). reflexivity.
Qed.


Lemma uf_squeeze : forall U e e1, (exists n1, en_uf e = U ++ n1) -> (exists n2, en_uf e1 = en_uf e ++ n2) ->
  en_uf e1 = U -> en_uf e = U /\ en_uf e1 = en_uf e.
Proof.
  intros U e e1 (n1 & E1) (n2 & E2) E. rewrite E2, E1, <- app_assoc in E.
  pose proof (len_app_nil _ _ _ (eq_sym E)) as N. apply app_eq_nil in N as [-> ->].
  rewrite app_nil_r in E1, E2. split; [exact E1 | exact E2].
Qed.

(* the instantiation an entry records, kept by setdefault_app *)
Lemma apps_step : forall U e e1 e2 les l k a s s',
  (en_uf e = U -> forall k', nofun k' = true -> option_map fst (get_app e k') = lfirst e k' les) ->
  en_apps e1 = en_apps e -> (exists n, en_uf e = U ++ n) -> (exists n, en_uf e1 = en_uf e ++ n) ->
  lapp e l = Some (k, a) -> setdefault_app e1 k a s = Good (e2, s') ->
  en_uf e2 = U -> forall k', nofun k' = true -> option_map fst (get_app e2 k') = lfirst e2 k' (les ++ [l]).
Proof.
  intros U e e1 e2 les l k a s s' Ap Ea Ex1 Ex2 La H E k' Hk'.
  destruct (setdefault_env _ _ _ _ _ _ H) as (_ & D2 & _).
  rewrite D2 in E. destruct (uf_squeeze U e e1 Ex1 Ex2 E) as [E0 E01].
  rewrite (setdefault_apps _ _ _ _ _ _ H k'), (lfirst_same e e2) by (rewrite D2; exact E01).
  assert (G : forall k0, get_app e1 k0 = get_app e k0) by (intro k0; unfold get_app; rewrite Ea; reflexivity).
  rewrite !G, lfirst_app, La, <- (Ap E0 k' Hk').
  destruct (key_eqb k' k) eqn:Ek.
  - apply key_eqb_true in Ek. subst k'. destruct (option_map fst (get_app e k)); reflexivity.
  - destruct (option_map fst (get_app e k')); reflexivity.
Qed.


(* the tables move only where the new entry's key is *)
Record TabStep (e e1 : env) : Prop := {
  tb_tn : en_tn e1 = en_tn e;
  tb_ustep : exists n2, en_uf e1 = en_uf e ++ n2;
  tb_tri : Tri (en_uf e1);
  tb_mths : en_mths e1 = en_mths e
}.

Lemma ws_step_common : forall U e e1 tm les needs l k,
  WS U e tm les needs -> TabStep e e1 -> ContextClean e1 -> lkey e l = Some k -> (forall r m g a, l <> LMth r m g a) ->
  (forall sb, RespE Fl sb U -> A.Unifies sb (map tr_ent (les ++ [l])) -> RespE Fl sb (en_uf e1)) ->
  (forall v y a, In (LVal v y a) (les ++ [l]) -> exists y0 a0, nget (en_vals e1) v = Some y0 /\
     In (LVal v y0 a0) (les ++ [l]) /\ (en_uf e1 = U -> canon e1 y = canon e1 y0)) ->
  (forall v y0, nget (en_vals e1) v = Some y0 -> exists a0, In (LVal v y0 a0) (les ++ [l])) ->
  (forall f g a, In (LFn f g a) (les ++ [l]) -> sg_this g = None /\ exists g0 a0, nget (en_fns e1) f = Some g0 /\
     In (LFn f g0 a0) (les ++ [l]) /\ (en_uf e1 = U -> SigCanon e1 g g0)) ->
  (forall f g0, nget (en_fns e1) f = Some g0 -> exists a0, In (LFn f g0 a0) (les ++ [l])) ->
  (forall u, nget (en_funs e1) u <> None <-> exists fx, In (LFun u fx) (les ++ [l])) ->
  (en_uf e1 = U -> forall k', nofun k' = true -> option_map fst (get_app e1 k') = lfirst e1 k' (les ++ [l])) ->
  (en_uf e1 = U -> forall k a, lkey e1 l = Some k -> linst l = Some a ->
     exists a0, lfirst e1 k (les ++ [l]) = Some a0 /\ InstCanon e1 a0 a) ->
  (forall k a0, option_map fst (get_app e1 k) = Some a0 -> AppIn e1 k a0 (les ++ [l])) ->
  WS U e1 tm (les ++ [l]) (add_need needs k).
Proof.
  intros U e e1 tm les needs l k Hws [Tn (n2 & Ex) Tr Tm] Cclean Hk Hm Mg V1 V2 F1 F2 Fu Ap Inew Ain.
  assert (Etn : forall k, tn_get e1 k = tn_get e k) by (intro; unfold tn_get; rewrite Tn; reflexivity).
  assert (Ins : en_uf e1 = U -> forall l' k a, In l' (les ++ [l]) -> lkey e1 l' = Some k -> linst l' = Some a ->
            exists a0, lfirst e1 k (les ++ [l]) = Some a0 /\ InstCanon e1 a0 a).
  { intros E l' k' a Hl Hk' Hi. apply in_app_iff in Hl as [Hl|[<-|[]]]; [|exact (Inew E k' a Hk' Hi)].
    destruct (uf_squeeze U e e1 (ws_ext _ _ _ _ _ Hws) (ex_intro _ n2 Ex) E) as [E0 E01].
    rewrite (lkey_same e e1 _ E01) in Hk'. destruct (ws_insts _ _ _ _ _ Hws E0 l' k' a Hl Hk' Hi) as (a0 & F & C).
    exists a0. rewrite (lfirst_same e e1 _ _ E01). split; [exact (lfirst_keep e k' les l a0 F) | exact (instcanon_same e e1 _ _ E01 C)]. }
  constructor; auto.
  - intro k'. rewrite Etn. apply (ws_tn _ _ _ _ _ Hws).
  - rewrite Tn. exact (ws_tnabs _ _ _ _ _ Hws).
  - destruct (ws_ext _ _ _ _ _ Hws) as (n1 & E1). exists (n1 ++ n2). rewrite Ex, E1, app_assoc. reflexivity.
  - intros E r m g a H. apply in_app_iff in H as [H|[H|[]]]; [|exfalso; exact (Hm r m g a H)].
    destruct (uf_squeeze U e e1 (ws_ext _ _ _ _ _ Hws) (ex_intro _ n2 Ex) E) as [E0 E01].
    destruct (ws_mths _ _ _ _ _ Hws E0 r m g a H) as (Ok & g0 & G & C).
    rewrite (canon_same e e1 _ E01), (get_mth_same e e1 _ _ Tm). split; [exact Ok|]. exists g0.
    split; [exact G | exact (sigcanon_same e e1 g g0 E01 C)].
  - intros r m g0 G. rewrite (get_mth_same e e1 _ _ Tm) in G. destruct (ws_mths_in _ _ _ _ _ Hws r m g0 G) as (r0 & a0 & H1 & H2).
    exists r0, a0. split; [apply in_or_app; left; exact H1|]. intros sb Hr. apply H2. rewrite Ex in Hr. exact (resp_app Fl _ _ _ Hr).
  - intros E k'. destruct (uf_squeeze U e e1 (ws_ext _ _ _ _ _ Hws) (ex_intro _ n2 Ex) E) as [E0 E01].
    rewrite add_need_in, (ws_needs _ _ _ _ _ Hws E0 k'). split.
    + intros [(l' & Hl & Hk')| ->]; [exists l'; split; [apply in_or_app; left; exact Hl|] | exists l; split; [apply in_or_app; right; left; reflexivity|]];
        rewrite (lkey_same e e1 _ E01); assumption.
    + intros (l' & Hl & Hk'). rewrite (lkey_same e e1 _ E01) in Hk'. apply in_app_iff in Hl as [Hl|[<-|[]]].
      * left. exists l'. auto.
      * right. congruence.
  - apply add_need_nodup. exact (ws_nodup _ _ _ _ _ Hws).
  - intros r m g a Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]].
    + exact (ws_mthis _ _ _ _ _ Hws _ _ _ _ Hin).
    + exfalso. exact (Hm _ _ _ _ Eq).

  - intros m rows At. rewrite Tm in At. exact(ws_method_unique _ _ _ _ _ Hws _ _ At).
  - intro E. destruct(uf_squeeze U e e1 (ws_ext _ _ _ _ _ Hws)(ex_intro _ n2 Ex)E)as[E0 E01].
    exact(proj2(method_rows_same e e1 Tm E01(ws_method_unique _ _ _ _ _ Hws)(ws_method_canonical _ _ _ _ _ Hws E0))).
Qed.

(* a merge of a with b, then of the instantiations recorded under k with a *)
Definition UOKI (e : env) (old y : ety) (k : key) (a : Elab.inst) (e' : env) : Prop :=
  UStep e e' /\
  (forall sb, RespE Fl sb (en_uf e) -> asub sb (trF old) = asub sb (trF y) ->
     (forall a0 st, get_app e k = Some (a0, st) -> InstResp Fl sb a0 a) -> RespE Fl sb (en_uf e')) /\
  (en_uf e' = en_uf e -> canon e old = canon e y).

Lemma compose_app : forall e e1 eu k a mi nm s0 s0', Tri (en_uf e1) -> (exists n, en_uf e1 = en_uf e ++ n) ->
  en_apps e1 = en_apps e -> unify_app e1 mi k a nm s0 = Good (eu, s0') ->
  UStep e1 eu /\ (get_app e k = None -> eu = e1) /\
  (forall sb, RespE Fl sb (en_uf e1) -> (forall a0 st, get_app e k = Some (a0, st) -> InstResp Fl sb a0 a) ->
     RespE Fl sb (en_uf eu)) /\
  (en_uf eu = en_uf e -> en_uf e1 = en_uf e).
Proof.
  intros e e1 eu k a mi nm s0 s0' T (n1 & E1) Ea H.
  destruct (unify_app_spec Fl _ _ _ _ _ _ _ _ T H) as (_ & U & Nn & Sm).
  assert (G : get_app e1 k = get_app e k) by (unfold get_app; rewrite Ea; reflexivity).
  split; [exact U|]. split; [intro N; apply Nn; rewrite G; exact N|]. split.
  - intros sb Hr Hi. destruct (get_app e1 k) as [[a0 st]|] eqn:G1.
    + exact (proj1 (proj2 (Sm a0 st eq_refl)) sb Hr (Hi a0 st (eq_sym G))).
    + rewrite (Nn eq_refl). exact Hr.
  - intro E. destruct (us_ext _ _ U) as (n2 & E2). rewrite E, E1, <- app_assoc in E2.
    pose proof (len_app_nil _ _ _ E2) as N. apply app_eq_nil in N as [-> _]. rewrite app_nil_r in E1. exact E1.
Qed.

Lemma appin_of_get : forall e k a0 st les, (forall k0 a1, option_map fst (get_app e k0) = Some a1 -> AppIn e k0 a1 les) ->
  get_app e k = Some (a0, st) -> AppIn e k a0 les.
Proof. intros e k a0 st les H G. apply H. rewrite G. reflexivity. Qed.

Lemma compose_val : forall U e e1 eu tm les needs x y a mi nm s0 s0',
  WS U e tm les needs ->
  ((nget (en_vals e) x = None /\ e1 = set_val e x y) \/
   (exists old, nget (en_vals e) x = Some old /\ UOK Fl e old y e1)) ->
  unify_app e1 mi (KyVal x) a nm s0 = Good (eu, s0') ->
  ((nget (en_vals e) x = None /\ eu = set_val e x y) \/
   (exists old, nget (en_vals e) x = Some old /\ UOKI e old y (KyVal x) a eu)).
Proof.
  intros U e e1 eu tm les needs x y a mi nm s0 s0' Hws C Hua.
  assert (Ea : en_apps e1 = en_apps e /\ Tri (en_uf e1) /\ exists n, en_uf e1 = en_uf e ++ n).
  { destruct C as [[N ->]|(old & O & U1 & _)].
    - split; [reflexivity|]. split; [exact (ws_tri _ _ _ _ _ Hws)|]. exists []. rewrite app_nil_r. reflexivity.
    - split; [rewrite (us_same _ _ U1); reflexivity|]. split; [exact (us_tri _ _ U1) | exact (us_ext _ _ U1)]. }
  destruct Ea as (Ea & T1 & Ex1).
  destruct (compose_app e e1 eu (KyVal x) a mi nm s0 s0' T1 Ex1 Ea Hua) as (Uu & Nn & Ru & Cu).
  destruct C as [[N ->]|(old & O & U1 & R1 & C1)].
  - left. split; [exact N|]. apply Nn.
    destruct (get_app e (KyVal x)) as [[a0 st]|] eqn:G; [|reflexivity]. exfalso.
    destruct (appin_of_get e _ a0 st les (ws_apps_in _ _ _ _ _ Hws) G) as (y0 & Hin).
    destruct (ws_vals _ _ _ _ _ Hws x y0 a0 Hin) as (y1 & _ & Gv & _). congruence.
  - right. exists old. split; [exact O|]. split; [exact (ustep_trans _ _ _ U1 Uu)|]. split.
    + intros sb Hr E Hi. apply Ru; [exact (R1 sb Hr E) | exact Hi].
    + intro E. exact (C1 (Cu E)).
Qed.

Lemma ws_val : forall U e e0 e1 e2 tm les needs x y a mi nm s0 s0' s s',
  WS U e tm les needs ->
  ((nget (en_vals e) x = None /\ e0 = set_val e x y) \/
   (exists old, nget (en_vals e) x = Some old /\ UOK Fl e old y e0)) ->
  unify_app e0 mi (KyVal x) a nm s0 = Good (e1, s0') ->
  setdefault_app e1 (KyVal x) a s = Good (e2, s') ->
  ContextClean e2 ->
  WS U e2 tm (les ++ [LVal x y a]) (add_need needs (KyVal x)).
Proof.
  intros U e e0 e1 e2 tm les needs x y a mi nm s0 s0' s s' Hws C0 Hua Hsd Cclean.
  assert (Ea0 : en_apps e0 = en_apps e /\ Tri (en_uf e0) /\ exists n, en_uf e0 = en_uf e ++ n).
  { destruct C0 as [[N ->]|(old & O & U1 & _)].
    - split; [reflexivity|]. split; [exact (ws_tri _ _ _ _ _ Hws)|]. exists []. rewrite app_nil_r. reflexivity.
    - split; [rewrite (us_same _ _ U1); reflexivity|]. split; [exact (us_tri _ _ U1) | exact (us_ext _ _ U1)]. }
  destruct Ea0 as (Ea0 & Tr0 & Ex0).
  pose proof (compose_val U e e0 e1 tm les needs x y a mi nm s0 s0' Hws C0 Hua) as C.
  destruct (setdefault_env _ _ _ _ _ _ Hsd) as (D1 & D2 & D3 & D4 & D5 & D6).
  destruct Hws as [T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19] eqn:Hws0.
  assert (Ts : TabStep e e1 /\ en_fns e1 = en_fns e /\
               (nget (en_vals e1) x = Some (match nget (en_vals e) x with Some old => old | None => y end)) /\
               (forall v, v <> x -> nget (en_vals e1) v = nget (en_vals e) v) /\
               en_funs e1 = en_funs e /\ en_apps e1 = en_apps e).
  { destruct C as [[N ->]|(old & O & U1 & _ & _)].
    - split; [constructor; [reflexivity | exists []; rewrite app_nil_r; reflexivity | exact T3 | reflexivity]|].
      split; [reflexivity|]. rewrite N. cbn [set_val en_vals en_funs en_apps]. split; [apply nget_nset_same|].
      split; [|split; reflexivity]. intros v Hv. apply nget_nset_other. exact Hv.
    - rewrite (us_same _ _ U1). cbn [with_uf en_tn en_uf en_vals en_fns en_funs en_apps]. rewrite O.
      split; [constructor; [reflexivity | exact (us_ext _ _ U1) | exact (us_tri _ _ U1) | reflexivity]|]. auto. }
  destruct Ts as (Ts & Ef & Ex & Eo & Efu & Eap).
  assert (Ts2 : TabStep e e2)
    by (destruct Ts as [A1 A2 A3 A4]; constructor; [rewrite D1; exact A1 | rewrite D2; exact A2 | rewrite D2; exact A3 | rewrite D5; exact A4]).
  rewrite <- D4 in Ef. rewrite <- D3 in Ex, Eo.
  assert (Hu : en_uf e2 = U -> en_uf e = U /\ en_uf e2 = en_uf e)
    by (intro E; exact (uf_squeeze U e e2 T4 (tb_ustep _ _ Ts2) E)).
  eapply ws_step_common; [exact Hws | exact Ts2 | exact Cclean | reflexivity | discriminate | | | | | | | | |].
  - intros sb Hr Hun. rewrite D2. assert (R0 : RespE Fl sb (en_uf e)).
    { apply T5; [exact Hr|]. rewrite map_app in Hun. exact (unifies_app _ _ _ Hun). }
    assert (Hi : forall a0 st, get_app e (KyVal x) = Some (a0, st) -> InstResp Fl sb a0 a).
    { intros a0 st G. destruct (appin_of_get e _ a0 st les T19 G) as (y0 & Hin).
      assert (E := Hun (tr_ent (LVal x y0 a0)) (tr_ent (LVal x y a)) (IVal x)).
      rewrite map_app in E. cbn [tr_ent A.entry_key A.entry_inst] in E.
      assert (Ei : Some (A.inst_read sb (tr_inst a0)) = Some (A.inst_read sb (tr_inst a))).
      { apply E; [| |reflexivity|reflexivity]; apply in_or_app; [left | right].
        - change (A.NVal x (trF y0) (tr_inst a0)) with (tr_ent (LVal x y0 a0)). apply in_map. exact Hin.
        - left. reflexivity. }
      injection Ei as Ei. exact (instresp_of_read sb a0 a Ei). }
    destruct C as [[N ->]|(old & O & U1 & R1 & _)].
    { exact R0. }
    apply (R1 sb R0); [|exact Hi].
    destruct (T7 x old O) as (a0 & Ho).
    assert (E := Hun (tr_ent (LVal x old a0)) (tr_ent (LVal x y a)) (IVal x)).
    cbn [tr_ent A.entry_key A.entry_sig] in E. rewrite map_app in E.
    assert (Sg : Some (A.FSig [] (asub sb (trF old))) = Some (A.FSig [] (asub sb (trF y)))).
    { apply E; [| |reflexivity|reflexivity]; apply in_or_app; [left | right].
      - change (A.NVal x (trF old) (tr_inst a0)) with (tr_ent (LVal x old a0)). apply in_map. exact Ho.
      - left. reflexivity. }
    injection Sg as Sg. exact Sg.
  - intros v y' a' Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]].
    + destruct (T6 v y' a' Hin) as (y0 & a0 & G & Hy0 & Hc). exists y0, a0.
      assert (Ev : nget (en_vals e2) v = Some y0).
      { destruct (Nat.eq_dec v x) as [->|Ne]; [|rewrite (Eo v Ne); exact G]. rewrite Ex, G. reflexivity. }
      split; [exact Ev|]. split; [apply in_or_app; left; exact Hy0|].
      intro E. destruct (Hu E) as [E0 E01]. rewrite !(canon_same e e2 _ E01). exact (Hc E0).
    + injection Eq as <- <- <-. destruct C as [[N ->]|(old & O & U1 & _ & C1)].
      * exists y, a. rewrite N in Ex. split; [exact Ex|]. split; [apply in_or_app; right; left; reflexivity | reflexivity].
      * destruct (T7 x old O) as (a0 & Ho). exists old, a0. rewrite O in Ex. split; [exact Ex|].
        split; [apply in_or_app; left; exact Ho|]. intro E. destruct (Hu E) as [E0 E01].
        rewrite !(canon_same e e2 _ E01). symmetry. exact (C1 (eq_trans (eq_sym D2) E01)).
  - intros v y0 G. destruct (Nat.eq_dec v x) as [->|Ne].
    + rewrite Ex in G. destruct (nget (en_vals e) x) as [old|] eqn:O.
      * injection G as <-. destruct (T7 x old O) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
      * injection G as <-. exists a. apply in_or_app. right. left. reflexivity.
    + rewrite (Eo v Ne) in G. destruct (T7 v y0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intros f0 g0 a0 Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]]; [|discriminate].
    destruct (T8 f0 g0 a0 Hin) as (Ht & g1 & a1 & G & Hg1 & Hc). split; [exact Ht|]. exists g1, a1.
    rewrite Ef. split; [exact G|]. split; [apply in_or_app; left; exact Hg1|].
    intro E. destruct (Hu E) as [E0 E01]. exact (sigcanon_same _ _ _ _ E01 (Hc E0)).
  - intros f0 g0 G. rewrite Ef in G. destruct (T9 f0 g0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intro u. rewrite D6, Efu, T12. split.
    + intros [fx H];exists fx;apply in_or_app;left;exact H.
    + intros [fx H];apply in_app_iff in H as[H|[H|[]]];[exists fx;exact H|discriminate H].
  - exact (apps_step U e e1 e2 les (LVal x y a) (KyVal x) a s s' T13 Eap T4 (tb_ustep _ _ Ts) eq_refl Hsd).
  - intros E k a' Hk Hi'. cbn [lkey linst] in Hk, Hi'. injection Hk as <-. injection Hi' as <-.
    exact (insts_new U e e1 e2 les (LVal x y a) (KyVal x) a mi nm s0 s0' s s' T13 e0 Ea0 Ex0 T4 Hua Tr0 Hsd eq_refl E).
  - exact (appin_step e e1 e2 les (LVal x y a) (KyVal x) a s s' T19 Eap (tb_ustep _ _ Ts) (mths_same_mono e e1 (tb_mths _ _ Ts)) eq_refl (fun _ _ E => ltac:(discriminate E)) Hsd).
Qed.

Lemma combine_map_eq : forall (F : ety -> ety) xs ys, List.length xs = List.length ys ->
  Forall (fun p => F (fst p) = F (snd p)) (combine xs ys) -> map F xs = map F ys.
Proof.
  intros F xs. induction xs as [|x r IH]; intros [|y r'] L Fa; cbn in *; try discriminate; [reflexivity|].
  inversion Fa as [|? ? E Fr]; subst. cbn in E. rewrite E. f_equal. apply IH; [lia | exact Fr].
Qed.

Lemma ws_fn : forall U e e0 e1 e2 tm les needs x g a mi0 nm0 s1 s1' s0 s0',
  WS U e tm les needs -> sg_this g = None ->
  ((nget (en_fns e) x = None /\ e0 = set_fn e x g) \/
   (exists old mi nm s s', nget (en_fns e) x = Some old /\ Tri (en_uf e) /\ unify_sig e mi old g nm s = Good (e0, s'))) ->
  unify_app e0 mi0 (KyFn x) a nm0 s1 = Good (e1, s1') ->
  setdefault_app e1 (KyFn x) a s0 = Good (e2, s0') ->
  ContextClean e2 ->
  WS U e2 tm (les ++ [LFn x g a]) (add_need needs (KyFn x)).
Proof.
  intros U e e0 e1 e2 tm les needs x g a mi0 nm0 s1 s1' s0 s0' Hws Hthis C Hua Hsd Cclean.
  destruct (setdefault_env _ _ _ _ _ _ Hsd) as (D1 & D2 & D3 & D4 & D5 & D6).
  destruct Hws as [T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19] eqn:Hws0.
  assert (C' : (nget (en_fns e) x = None /\ e0 = set_fn e x g) \/
               (exists old, nget (en_fns e) x = Some old /\ UStep e e0 /\
                  List.length (sg_params old) = List.length (sg_params g) /\
                  (forall sb, RespE Fl sb (en_uf e) -> SigResp Fl sb old g -> RespE Fl sb (en_uf e0)) /\
                  (en_uf e0 = en_uf e -> Forall (fun p => canon e (fst p) = canon e (snd p))
                       (combine (map snd (sg_params old)) (map snd (sg_params g))) /\ canon e (sg_ret old) = canon e (sg_ret g)))).
  { destruct C as [C|(old & mi & nm & s & s' & O & Tr & H)]; [left; exact C|]. right. exists old. split; [exact O|].
    destruct (unify_sig_spec Fl e mi old g nm s e0 s' Tr H) as (_ & U1 & L1 & R1 & C1). auto. }
  clear C.
  assert (Ea0 : en_apps e0 = en_apps e /\ Tri (en_uf e0) /\ exists n, en_uf e0 = en_uf e ++ n).
  { destruct C' as [[N ->]|(old & O & U1 & _)].
    - split; [reflexivity|]. split; [exact T3|]. exists []. rewrite app_nil_r. reflexivity.
    - split; [rewrite (us_same _ _ U1); reflexivity|]. split; [exact (us_tri _ _ U1) | exact (us_ext _ _ U1)]. }
  destruct Ea0 as (Ea0 & Tr0 & Ex0).
  destruct (compose_app e e0 e1 (KyFn x) a mi0 nm0 s1 s1' Tr0 Ex0 Ea0 Hua) as (Uu & Nn & Ru & Cu).
  assert (C : (nget (en_fns e) x = None /\ e1 = set_fn e x g) \/
               (exists old, nget (en_fns e) x = Some old /\ UStep e e1 /\
                  List.length (sg_params old) = List.length (sg_params g) /\
                  (forall sb, RespE Fl sb (en_uf e) -> SigResp Fl sb old g ->
                     (forall a0 st, get_app e (KyFn x) = Some (a0, st) -> InstResp Fl sb a0 a) -> RespE Fl sb (en_uf e1)) /\
                  (en_uf e1 = en_uf e -> Forall (fun p => canon e (fst p) = canon e (snd p))
                       (combine (map snd (sg_params old)) (map snd (sg_params g))) /\ canon e (sg_ret old) = canon e (sg_ret g)))).
  { destruct C' as [[N ->]|(old & O & U1 & L1 & R1 & C1)].
    - left. split; [exact N|]. apply Nn.
      destruct (get_app e (KyFn x)) as [[a0 st]|] eqn:G; [|reflexivity]. exfalso.
      destruct (appin_of_get e _ a0 st les T19 G) as (g0 & Hin).
      destruct (T8 x g0 a0 Hin) as (_ & g1 & _ & Gf & _). congruence.
    - right. exists old. split; [exact O|]. split; [exact (ustep_trans _ _ _ U1 Uu)|]. split; [exact L1|]. split.
      + intros sb Hr E Hi. apply Ru; [exact (R1 sb Hr E) | exact Hi].
      + intro E. exact (C1 (Cu E)). }
  assert (Ts : TabStep e e1 /\ en_vals e1 = en_vals e /\
               (nget (en_fns e1) x = Some (match nget (en_fns e) x with Some old => old | None => g end)) /\
               (forall v, v <> x -> nget (en_fns e1) v = nget (en_fns e) v) /\
               en_funs e1 = en_funs e /\ en_apps e1 = en_apps e).
  { destruct C as [[N ->]|(old & O & U1 & _)].
    - split; [constructor; [reflexivity | exists []; rewrite app_nil_r; reflexivity | exact T3 | reflexivity]|].
      split; [reflexivity|]. rewrite N. cbn [set_fn en_fns en_funs en_apps]. split; [apply nget_nset_same|].
      split; [|split; reflexivity]. intros v Hv. apply nget_nset_other. exact Hv.
    - rewrite (us_same _ _ U1). cbn [with_uf en_tn en_uf en_vals en_fns en_funs en_apps]. rewrite O.
      split; [constructor; [reflexivity | exact (us_ext _ _ U1) | exact (us_tri _ _ U1) | reflexivity]|]. auto. }
  destruct Ts as (Ts & Ev & Ex & Eo & Efu & Eap).
  assert (Ts2 : TabStep e e2)
    by (destruct Ts as [A1 A2 A3 A4]; constructor; [rewrite D1; exact A1 | rewrite D2; exact A2 | rewrite D2; exact A3 | rewrite D5; exact A4]).
  rewrite <- D3 in Ev. rewrite <- D4 in Ex, Eo.
  assert (Hu : en_uf e2 = U -> en_uf e = U /\ en_uf e2 = en_uf e)
    by (intro E; exact (uf_squeeze U e e2 T4 (tb_ustep _ _ Ts2) E)).
  eapply ws_step_common; [exact Hws | exact Ts2 | exact Cclean | reflexivity | discriminate | | | | | | | | |].
  - intros sb Hr Hun. rewrite D2. assert (R0 : RespE Fl sb (en_uf e)).
    { apply T5; [exact Hr|]. rewrite map_app in Hun. exact (unifies_app _ _ _ Hun). }
    assert (Hi : forall a0 st, get_app e (KyFn x) = Some (a0, st) -> InstResp Fl sb a0 a).
    { intros a0 st G. destruct (appin_of_get e _ a0 st les T19 G) as (g0 & Hin).
      assert (E := Hun (tr_ent (LFn x g0 a0)) (tr_ent (LFn x g a)) (IFn x)).
      rewrite map_app in E. cbn [tr_ent A.entry_key A.entry_inst] in E.
      assert (Ei : Some (A.inst_read sb (tr_inst a0)) = Some (A.inst_read sb (tr_inst a))).
      { apply E; [| |reflexivity|reflexivity]; apply in_or_app; [left | right].
        - change (A.NFn x (tr_sig g0) (tr_inst a0)) with (tr_ent (LFn x g0 a0)). apply in_map. exact Hin.
        - left. reflexivity. }
      injection Ei as Ei. exact (instresp_of_read sb a0 a Ei). }
    destruct C as [[N ->]|(old & O & U1 & L1 & R1 & _)]; [exact R0|]. apply (R1 sb R0); [|exact Hi].
    destruct (T9 x old O) as (a0 & Ho). destruct (T8 x old a0 Ho) as (Hto & _).
    assert (E := Hun (tr_ent (LFn x old a0)) (tr_ent (LFn x g a)) (IFn x)).
    cbn [tr_ent A.entry_key A.entry_sig] in E. rewrite map_app in E.
    assert (Sg : Some (A.sig_subst sb (tr_sig old)) = Some (A.sig_subst sb (tr_sig g))).
    { apply E; [| |reflexivity|reflexivity]; apply in_or_app; [left | right].
      - change (A.NFn x (tr_sig old) (tr_inst a0)) with (tr_ent (LFn x old a0)). apply in_map. exact Ho.
      - left. reflexivity. }
    injection Sg as Sp Sr. unfold SigResp. rewrite Hto. split; [|split; [exact Sr | exact I]].
    rewrite !map_map in Sp. rewrite <- (map_map snd (fun y => asub sb (trF y))) in Sp.
    rewrite <- (map_map snd (fun y => asub sb (trF y)) (sg_params g)) in Sp.
    exact (Merge.map_combine (fun y => asub sb (trF y)) _ _ Sp).
  - intros v y' a' Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]]; [|discriminate].
    destruct (T6 v y' a' Hin) as (y0 & a0 & G & Hy0 & Hc). exists y0, a0. rewrite Ev.
    split; [exact G|]. split; [apply in_or_app; left; exact Hy0|].
    intro E. destruct (Hu E) as [E0 E01]. rewrite !(canon_same e e2 _ E01). exact (Hc E0).
  - intros v y0 G. rewrite Ev in G. destruct (T7 v y0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intros f0 g' a' Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]].
    + destruct (T8 f0 g' a' Hin) as (Ht & g1 & a1 & G & Hg1 & Hc). split; [exact Ht|]. exists g1, a1.
      assert (Ef : nget (en_fns e2) f0 = Some g1).
      { destruct (Nat.eq_dec f0 x) as [->|Ne]; [|rewrite (Eo f0 Ne); exact G]. rewrite Ex, G. reflexivity. }
      split; [exact Ef|]. split; [apply in_or_app; left; exact Hg1|].
      intro E. destruct (Hu E) as [E0 E01]. exact (sigcanon_same _ _ _ _ E01 (Hc E0)).
    + injection Eq as <- <- <-. split; [exact Hthis|]. destruct C as [[N ->]|(old & O & U1 & L1 & _ & C1)].
      * exists g, a. rewrite N in Ex. split; [exact Ex|]. split; [apply in_or_app; right; left; reflexivity|].
        intros _. split; reflexivity.
      * destruct (T9 x old O) as (a0 & Ho). exists old, a0. rewrite O in Ex. split; [exact Ex|].
        split; [apply in_or_app; left; exact Ho|]. intro E. destruct (Hu E) as [E0 E01].
        destruct (C1 (eq_trans (eq_sym D2) E01)) as [P1 P2]. apply (sigcanon_same e); [exact E01|]. split; [|exact (eq_sym P2)].
        symmetry. rewrite <- !(map_map snd (canon e)). apply combine_map_eq; [rewrite !length_map; exact L1 | exact P1].
  - intros f0 g0 G. destruct (Nat.eq_dec f0 x) as [->|Ne].
    + rewrite Ex in G. destruct (nget (en_fns e) x) as [old|] eqn:O.
      * injection G as <-. destruct (T9 x old O) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
      * injection G as <-. exists a. apply in_or_app. right. left. reflexivity.
    + rewrite (Eo f0 Ne) in G. destruct (T9 f0 g0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intro u. rewrite D6, Efu, T12. split.
    + intros [fx H];exists fx;apply in_or_app;left;exact H.
    + intros [fx H];apply in_app_iff in H as[H|[H|[]]];[exists fx;exact H|discriminate H].
  - exact (apps_step U e e1 e2 les (LFn x g a) (KyFn x) a s0 s0' T13 Eap T4 (tb_ustep _ _ Ts) eq_refl Hsd).
  - intros E k a' Hk Hi'. cbn [lkey linst] in Hk, Hi'. injection Hk as <-. injection Hi' as <-.
    exact (insts_new U e e1 e2 les (LFn x g a) (KyFn x) a mi0 nm0 s1 s1' s0 s0' T13 e0 Ea0 Ex0 T4 Hua Tr0 Hsd eq_refl E).
  - exact (appin_step e e1 e2 les (LFn x g a) (KyFn x) a s0 s0' T19 Eap (tb_ustep _ _ Ts) (mths_same_mono e e1 (tb_mths _ _ Ts)) eq_refl (fun _ _ E => ltac:(discriminate E)) Hsd).
Qed.

Lemma ws_fun : forall U e e2 tm les needs u st fx s s', WS U e tm les needs ->
  setdefault_app (set_fun e u st) (KyFun u) (raw_fixed_app fx) s = Good (e2, s') ->
  ContextClean e2 ->
  WS U e2 tm (les ++ [LFun u fx]) (add_need needs (KyFun u)).
Proof.
  intros U e e2 tm les needs u st fx s s' Hws Hsd Cclean.
  destruct (setdefault_env _ _ _ _ _ _ Hsd) as (E1 & E2 & E3 & E4 & E5 & E6). cbn [set_fun en_tn en_uf en_vals en_fns en_mths en_funs] in *.
  assert (Hu : en_uf e2 = U -> en_uf e = U) by (rewrite E2; auto).
  eapply ws_step_common; [exact Hws | constructor; [exact E1 | exists []; rewrite app_nil_r; exact E2 | rewrite E2; exact (ws_tri _ _ _ _ _ Hws) | exact E5] | exact Cclean | reflexivity | discriminate | | | | | | | | |].
  - intros sb Hr Hun. rewrite E2. apply (ws_mg _ _ _ _ _ Hws sb Hr). rewrite map_app in Hun. exact (unifies_app _ _ _ Hun).
  - intros v y a0 Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]]; [|discriminate].
    destruct (ws_vals _ _ _ _ _ Hws v y a0 Hin) as (y0 & a1 & G & Hy & Hc). exists y0, a1. rewrite E3.
    split; [exact G|]. split; [apply in_or_app; left; exact Hy|]. intro E. rewrite !(canon_same e e2 _ E2). exact (Hc (Hu E)).
  - intros v y0 G. rewrite E3 in G. destruct (ws_vals_in _ _ _ _ _ Hws v y0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intros f0 g a0 Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]]; [|discriminate].
    destruct (ws_fns _ _ _ _ _ Hws f0 g a0 Hin) as (Ht & g1 & a1 & G & Hg1 & Hc). split; [exact Ht|]. exists g1, a1. rewrite E4.
    split; [exact G|]. split; [apply in_or_app; left; exact Hg1|]. intro E. exact (sigcanon_same _ _ _ _ E2 (Hc (Hu E))).
  - intros f0 g0 G. rewrite E4 in G. destruct (ws_fns_in _ _ _ _ _ Hws f0 g0 G) as (a0 & Ho). exists a0. apply in_or_app. left. exact Ho.
  - intro u'. rewrite E6, nget_nset. destruct (Nat.eqb_spec u' u) as [->|Ne].
    + split; [intros _;exists fx;apply in_or_app;right;left;reflexivity|discriminate].
    + rewrite (ws_funs _ _ _ _ _ Hws u'). split.
      * intros [fx0 H];exists fx0;apply in_or_app;left;exact H.
      * intros [fx0 H];apply in_app_iff in H as[H|[H|[]]];[exists fx0;exact H|].
        injection H as Eq _;congruence.
  - eapply apps_step with(e1:=set_fun e u st)(s:=s)(s':=s');[exact(ws_apps _ _ _ _ _ Hws)|reflexivity|exact(ws_ext _ _ _ _ _ Hws)| |reflexivity|exact Hsd].
    exists [];rewrite app_nil_r;reflexivity.
  - intros _ k a' _ Hi. discriminate Hi.
  - eapply (appin_step e (set_fun e u st) e2 les (LFun u fx) (KyFun u) (raw_fixed_app fx) s s' (ws_apps_in _ _ _ _ _ Hws));
      [reflexivity | exists []; rewrite app_nil_r; reflexivity | exact (mths_same_mono e _ eq_refl) | reflexivity | intros ? ? E; discriminate E | exact Hsd].
Qed.

Lemma ety_of_rel : forall t mi tm tmr e s d s', ety_of t mi tm e None s = Good (d, s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> TmRel Fl tm tmr e -> Later w P R Fl s' ->
  A.TyOf WW (path_of mi) tmr None t (trF d) /\ WFty d.
Proof.
  intros t mi tm tmr e s d s' H Hi Hw Hmi Ht L. rewrite ety_of_eq in H.
  exact (proj1 (ty_all w R Fl b Hload 200) t mi tm e None s d s' tmr H Hi Hw Hmi Ht I L).
Qed.

Lemma fn_sig_rel : forall g tm tmr e s sg s' rcv n ps rt body, fn_sig g tm e None s = Good (sg, s') ->
  Inv s -> st_w s = w -> nget (w_syms w) (gs_id g) = Some g -> gs_decl g = ADFn rcv n ps rt body ->
  TmRel Fl tm tmr e -> Later w P R Fl s' ->
  A.SigOf WW (gs_id g) tmr None (tr_sig sg) /\ sg_this sg = None.
Proof.
  intros g tm tmr e s sg s' rcv n ps rt body H Hi Hw Hg D Ht L.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hgm.
  unfold fn_sig in H. destruct (bstep _ _ _ _ _ _ _ (ok_mmap _ _ _ _ (fun p => ok_bind _ _ _ _ (ok_ety_of _ _ _ _ _) (fun _ => ok_ret _ _))) Hi H)
    as (ps' & s1 & H1 & H2 & Hi1 & F1).
  apply bind_inv in H2 as (r & s2 & H3 & H4). injection H4 as <- <-.
  assert (Ok3 : ok (match fn_ret g with Some t => ety_of t (gs_mod g) tm e None | None => ret YUnit end))
    by (destruct (fn_ret g); [apply ok_ety_of | apply ok_ret]).
  destruct (Ok3 s1 r s2 Hi1 H3) as [_ F3].
  pose proof (fr_w' w _ _ F1 Hw) as Hw1.
  split; [|reflexivity].
  exists (A.Decl (path_of (gs_mod g)) (gs_decl g) (gs_assumes g)), rcv, n, ps, rt, body.
  split; [exact (w_decl_rd w R Fl _ _ Hg)|]. split; [exact D|]. cbn [A.d_mod tr_sig A.sg_params A.sg_ret].
  unfold fn_params, fn_ret in *. rewrite D in H1, H3. split.
  - assert (Q : Forall2 (fun p q => fst q = fst p /\ A.TyOf WW (path_of (gs_mod g)) tmr None (snd p) (trF (snd q))) ps ps').
    { refine (mmap_ok_rel w R Fl _ _ _ _ ps s ps' s1 _ _ H1 Hi Hw (later_from w R Fl _ _ F3 L)).
      - intro p. apply ok_bind; [apply ok_ety_of | intro; apply ok_ret].
      - intros p _ s0 q s3 E Hi0 Hw0 L0. apply bind_inv in E as (y & s4 & E1 & E2). injection E2 as <- <-.
        split; [reflexivity|]. exact (proj1 (ety_of_rel _ _ _ _ _ _ _ _ E1 Hi0 Hw0 Hgm Ht L0)). }
    clear -Q. induction Q as [|p q l l' [_ Hq] Hl IH]; constructor; [exact Hq | exact IH].
  - destruct rt as [t|]; [|injection H3 as <- _; reflexivity].
    exact (proj1 (ety_of_rel _ _ _ _ _ _ _ _ H3 Hi1 Hw1 Hgm Ht L)).
Qed.

Lemma ok_memory_sym : ok memory_sym.
Proof. unfold memory_sym. okt. Qed.
Lemma ok_functor_fixed : forall g, ok (functor_fixed g).
Proof. intro g. unfold functor_fixed. okt. Qed.

(* ======================================================================== *)
(* 3. The walk's steps                                                       *)
(* ======================================================================== *)

Definition blessb (g : gsym) : bool :=
  match gs_kind g, gs_decl g with
  | KCtx, _ | KFn, _ | KVal, ADVal _ _ None => false
  | _, _ => true
  end.

(* what assuming the symbol x under the brackets sb reads as *)
Definition ISym (x : nat) (sb tm : list (nat * ty)) (es : list A.entry) (tm' : list (nat * ty)) : Prop :=
  (A.kind_sym WW x = Some A.DKType /\ es = [A.NTy x] /\
     tm' = match A.nlook x tm with Some _ => tm | None => (x, TAbs x) :: tm end) \/
  (exists k, A.kind_sym WW x = Some k /\
     (k = A.DKUnit \/ k = A.DKTag \/ k = A.DKAlias \/
      (k = A.DKVal /\ exists d n t i, A.w_decl WW x = Some d /\ A.d_ast d = ADVal n t (Some i))) /\
     es = [] /\ tm' = tm) \/
  (exists d n t ty0 ap, A.w_decl WW x = Some d /\ A.d_ast d = ADVal n t None /\
     A.TyOf WW (A.d_mod d) (sb ++ tm) None t ty0 /\ A.AppOf WW x (sb ++ tm) ap /\ es = [A.NVal x ty0 ap] /\ tm' = tm) \/
  (exists g ap, A.kind_sym WW x = Some A.DKFn /\ A.SigOf WW x (sb ++ tm) None g /\ A.AppOf WW x (sb ++ tm) ap /\
     es = [A.NFn x g ap] /\ tm' = tm) \/
  (exists d n ps args res fx, A.w_decl WW x = Some d /\ A.d_ast d = ADFunctor n ps args res None /\
     (forall t,A.Fixed WW x t->A.nlook t tm<>None) /\ A.FixedReadings WW x tm fx /\ es = [A.NFun x fx] /\ tm' = tm) \/
  (exists d n its, A.w_decl WW x = Some d /\ A.d_ast d = ADCtx n its /\ IEs WW (A.d_mod d) sb tm its es tm').

Definition SymOK' (f : nat) : Prop :=
  forall e mi g subst needs s e' needs' s' U tm les ob,
  add_symbol_item f e mi g subst needs s = Good ((e', needs'), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> nget (w_syms w) (gs_id g) = Some g -> Later w P R Fl s' ->
  WS U e tm les needs -> map (trp Fl) subst = ob -> Forall (fun q => ety_ok (snd q) = true) subst ->
  exists les' es tm', ISym (gs_id g) ob tm es tm' /\ map tr_ent les' = es /\ WS U e' tm' (les ++ les') needs'.

Definition SpecOK' (f : nat) : Prop :=
  forall e mi sp outer needs s e' needs' s' U tm les ob,
  add_spec f e mi sp outer needs s = Good ((e', needs'), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> Later w P R Fl s' ->
  WS U e tm les needs -> map (trp Fl) outer = ob -> Forall (fun q => ety_ok (snd q) = true) outer ->
  exists les' es tm', IE WW (path_of mi) ob tm sp es tm' /\ map tr_ent les' = es /\ WS U e' tm' (les ++ les') needs'.

(* Method items are not proved yet: what their step owes. *)
Definition MethStep : Prop :=
  forall e mi r dn subst needs s e' needs' s' U tm les ob,
  add_method_item e mi r dn subst needs s = Good ((e', needs'), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> nget (w_syms w) (gs_id r) = Some r -> Later w P R Fl s' ->
  WS U e tm les needs -> map (trp Fl) subst = ob -> Forall (fun q => ety_ok (snd q) = true) subst ->
  exists m rt g ap les', A.MethodNamed WW (path_of mi) (gs_id r) dn m /\ A.kind_sym WW m = Some A.DKMeth /\
    A.RecvTy WW (gs_id r) (ob ++ tm) rt /\ A.SigOf WW m (ob ++ tm) (Some rt) g /\ A.AppOf WW m (ob ++ tm) ap /\
    map tr_ent les' = [A.NMth rt m g ap] /\ WS U e' tm (les ++ les') needs'.

Lemma sym_zero : SymOK' 0.
Proof. intros until 1. discriminate. Qed.
Lemma spec_zero : SpecOK' 0.
Proof. intros until 1. discriminate. Qed.

Lemma ws_nil_les : forall U e tm les needs, WS U e tm les needs -> WS U e tm (les ++ []) needs.
Proof. intros. rewrite app_nil_r. assumption. Qed.

Lemma sym_step : forall f, SpecOK' f -> SymOK' (S f).
Proof.
  intros f IHs e mi g subst needs s e' needs' s' U tm les ob H Hi Hw Hmi Hg L Hws Hob Hvalid.
  assert (Hwf : Forall (fun q => WFty (snd q)) subst).
  { eapply Forall_impl; [|exact Hvalid]. intros q Hq. apply ety_ok_wf. exact Hq. }
  pose proof (proj1 (context_walk_clean w R Fl b Hload (S f)) e mi g subst needs s e' needs' s' H Hi Hw Hmi Hg
    (tmclean_of_forall subst Hvalid) (ws_clean _ _ _ _ _ Hws) (ws_tri _ _ _ _ _ Hws) L) as [Cout Tout].
  cbn [add_symbol_item] in H.
  destruct (bstep _ _ _ _ _ _ _ (ok_check_totality mi g subst e) Hi H) as (u & s1 & _ & H' & Hi1 & F1).
  clear H. rename H' into H. cbn beta zeta in H. pose proof (fr_w' w _ _ F1 Hw) as Hw1.
  pose proof (tmrel_walk _ _ _ _ _ _ _ Hws Hob Hwf) as TR.
  pose proof (load_world_kind b w Hload _ _ Hg) as KO. unfold KindOK in KO.
  pose proof (kind_sym_rd w R Fl b Hload _ _ Hg) as Ks.
  pose proof (w_decl_rd w R Fl _ _ Hg) as Wd.
  pose proof (loader_mod_in w R Fl b Hload _ _ Hg) as Hgm.
  destruct (gs_kind g) eqn:K; cbn [kconv] in Ks, KO.
  - (* type *)
    exists [LTy (gs_id g)], [A.NTy (gs_id g)].
    destruct (tn_get e (gs_id g)) as [y|] eqn:Et.
    + injection H as <- <- <-. exists tm. split; [left; split; [exact Ks|split; [reflexivity|]]|].
      * rewrite (ws_tn _ _ _ _ _ Hws), Et. reflexivity.
      * split; [reflexivity|]. apply ws_les_nokey; [exact Hws | reflexivity | discriminate | discriminate | discriminate].
    + apply bind_inv in H as (st0 & s2 & _ & H). injection H as <- <- <-.
      exists ((gs_id g, TAbs (gs_id g)) :: tm). split; [left; split; [exact Ks|split; [reflexivity|]]|].
      * rewrite (ws_tn _ _ _ _ _ Hws), Et. reflexivity.
      * split; [reflexivity|]. apply ws_les_nokey; [|reflexivity | discriminate | discriminate | discriminate].
        destruct Hws as [T1 T2 T3 T4 T5 T6 T7 T8 T9 Q1 Q2 T10 T11 T12 T13 T14 T15 T16 T17 T18 T19].
        assert (Ee : forall k, tn_get (set_tn e (gs_id g) (YAbs (gs_id g)) st0) k =
                               if Nat.eqb k (gs_id g) then Some (YAbs (gs_id g)) else tn_get e k) by apply tn_get_set_tn.
        constructor; cbn [set_tn en_uf en_vals en_fns].
        -- intro k. rewrite Ee. cbn [A.nlook]. destruct (Nat.eqb_spec k (gs_id g)) as [->|]; [reflexivity | apply T1].
        -- intros k p Hk. apply in_aset in Hk as [Hk|Hk]; [injection Hk as -> Ep; subst p; reflexivity | exact (T2 k p Hk)].
        -- exact T3.
        -- exact T4.
        -- exact T5.
        -- exact T6.
        -- exact T7.
        -- exact T8.
        -- exact T9.
        -- exact Q1.
        -- exact Q2.
        -- intros E k. rewrite (T10 E k). reflexivity.
        -- exact T11.
        -- exact T12.
        -- intros E k Hk. rewrite (lfirst_same e (set_tn e (gs_id g) (YAbs (gs_id g)) st0) k les eq_refl). exact (T13 E k Hk).
        -- exact T14.
        -- exact Cout.
        -- exact T16.
        -- exact T17.
        -- intros E l k a Hl Hk Hia. rewrite (lkey_same e (set_tn e (gs_id g) (YAbs (gs_id g)) st0) l eq_refl) in Hk.
           destruct (T18 E l k a Hl Hk Hia) as (a0 & F & C). exists a0.
           rewrite (lfirst_same e (set_tn e (gs_id g) (YAbs (gs_id g)) st0) k les eq_refl).
           split; [exact F | exact (instcanon_same e (set_tn e (gs_id g) (YAbs (gs_id g)) st0) _ _ eq_refl C)].
        -- intros k a0 G. rewrite <- (app_nil_r les).
           apply (appin_mono e (set_tn e (gs_id g) (YAbs (gs_id g)) st0) k a0 les []); [exists []; rewrite app_nil_r; reflexivity | exact (mths_same_mono e _ eq_refl) | exact (T19 k a0 G)].
  - (* unit *)
    injection H as <- <- <-. exists [], [], tm. split; [right; left; exists A.DKUnit; auto|].
    split; [reflexivity | exact (ws_nil_les _ _ _ _ _ Hws)].
  - (* alias *)
    injection H as <- <- <-. exists [], [], tm. split; [right; left; exists A.DKAlias; auto 6|].
    split; [reflexivity | exact (ws_nil_les _ _ _ _ _ Hws)].
  - (* tag *)
    injection H as <- <- <-. exists [], [], tm. split; [right; left; exists A.DKTag; auto 6|].
    split; [reflexivity | exact (ws_nil_les _ _ _ _ _ Hws)].
  - (* val *)
    destruct (gs_decl g) as [| | | | |n t [i|]|rcv0 ? ? ? ?| |] eqn:D; cbn in KO; try discriminate KO;
      try (destruct rcv0; discriminate KO); try rewrite D in Wd.
    + (* defined: nothing in force *)
      injection H as <- <- <-. exists [], [], tm. split; [|split; [reflexivity | exact (ws_nil_les _ _ _ _ _ Hws)]].
      right; left. exists A.DKVal. split; [exact Ks|]. split; [|split; reflexivity].
      right; right; right. split; [reflexivity|]. eexists _, n, t, i. split; [exact Wd | reflexivity].
    + (* assumed *)
      destruct (bstep _ _ _ _ _ _ _ (ok_ety_of _ _ _ _ _) Hi1 H) as (y & s2 & Hy & H2 & Hi2 & F2). clear H.
      assert (Ok1 : ok (match nget (en_vals e) (gs_id g) with
                        | Some old => unify 200 e mi old y (" for `" ++ gs_name g ++ "`")%string
                        | None => ret (set_val e (gs_id g) y) end))
        by (destruct (nget (en_vals e) (gs_id g)); [apply ok_unify | apply ok_ret]).
      destruct (bstep _ _ _ _ _ _ _ Ok1 Hi2 H2) as (e1 & s3 & He1 & H3 & Hi3 & F3). clear H2.
      destruct (bstep _ _ _ _ _ _ _ (ok_provision_app _ _ _) Hi3 H3) as (a & s4 & Ha & H4 & Hi4 & F4). clear H3.
      destruct (bstep _ _ _ _ _ _ _ (ok_unify_app _ _ _ _ _) Hi4 H4) as (eu & s4u & Heu & H4u & Hi4u & F4u). clear H4.
      destruct (bstep _ _ _ _ _ _ _ (ok_setdefault_app _ _ _) Hi4u H4u) as (e2 & s5 & He2 & H5 & Hi5 & F5). clear H4u.
      injection H5 as <- <- <-.
      pose proof (later_from w R Fl _ _ F5 L) as L4u. pose proof (later_from w R Fl _ _ F4u L4u) as L4.
      pose proof (later_from w R Fl _ _ F4 L4) as L3.
      pose proof (later_from w R Fl _ _ F3 L3) as L2.
      destruct (ety_of_rel _ _ _ _ _ _ _ _ Hy Hi1 Hw1 Hgm TR L2) as [Ty Wy].
      assert (Ap : A.AppOf WW (gs_id g) (ob ++ tm) (tr_inst a))
        by (apply (provision_app_rel g _ _ e _ _ _ Hi3 Ha L4 TR); rewrite Ks; discriminate).
      exists [LVal (gs_id g) y a], [A.NVal (gs_id g) (trF y) (tr_inst a)], tm. split.
      { right; right; left. eexists _, n, t, (trF y), (tr_inst a). split; [exact Wd|]. split; [reflexivity|].
        split; [exact Ty|]. split; [exact Ap|]. split; reflexivity. }
      split; [reflexivity|].
      apply (ws_val _ e e1 eu _ _ _ _ _ _ _ mi (gs_name g) s4 s4u s4u s5); [exact Hws | | exact Heu | exact He2 | exact Cout]. destruct (nget (en_vals e) (gs_id g)) as [old|] eqn:O.
      * right. exists old. split; [reflexivity|].
        exact (proj2 (unify_spec Fl _ _ _ _ _ _ _ _ _ (ws_tri _ _ _ _ _ Hws) He1)).
      * left. injection He1 as <- _. split; reflexivity.
  - (* fn *)
    destruct (gs_decl g) as [| | | | | |rcv n ps rt body| |] eqn:D; cbn in KO; try discriminate KO.
    destruct rcv; try discriminate KO. try rewrite D in Wd.
    destruct (bstep _ _ _ _ _ _ _ (ok_fn_sig _ _ _ _) Hi1 H) as (sg & s2 & Hs & H2 & Hi2 & F2). clear H.
    assert (Ok1 : ok (match nget (en_fns e) (gs_id g) with
                      | Some old => unify_sig e mi old sg (gs_name g)
                      | None => ret (set_fn e (gs_id g) sg) end))
      by (destruct (nget (en_fns e) (gs_id g)); [unfold unify_sig; okt; apply ok_unify | apply ok_ret]).
    destruct (bstep _ _ _ _ _ _ _ Ok1 Hi2 H2) as (e1 & s3 & He1 & H3 & Hi3 & F3). clear H2.
    destruct (bstep _ _ _ _ _ _ _ (ok_provision_app _ _ _) Hi3 H3) as (a & s4 & Ha & H4 & Hi4 & F4). clear H3.
    apply bind_inv in H4 as (mem & s5 & Hm & H5). unfold is_memory_instruction in Hm. injection Hm as <- <-.
    destruct (bstep _ _ _ _ _ _ _ ok_memory_sym Hi4 H5) as (ms & s6 & _ & H6 & Hi6 & F6). clear H5.
    cbn beta iota zeta in H6.
    destruct (bstep _ _ _ _ _ _ _ (ok_unify_app _ _ _ _ _) Hi6 H6) as (eu & s6u & Heu & H6u & Hi6u & F6u). clear H6.
    destruct (bstep _ _ _ _ _ _ _ (ok_setdefault_app _ _ _) Hi6u H6u) as (e2 & s7 & He2 & H7 & Hi7 & F7). clear H6u.
    injection H7 as <- <- <-.
    pose proof (later_from w R Fl _ _ F7 L) as L6u. pose proof (later_from w R Fl _ _ F6u L6u) as L6.
    pose proof (later_from w R Fl _ _ F6 L6) as L4.
    pose proof (later_from w R Fl _ _ F4 L4) as L3. pose proof (later_from w R Fl _ _ F3 L3) as L2.
    destruct (fn_sig_rel g _ _ e _ _ _ _ _ _ _ _ Hs Hi1 Hw1 Hg D TR L2) as [Sg Th].
    assert (Ap : A.AppOf WW (gs_id g) (ob ++ tm) (tr_inst a))
      by (apply (provision_app_rel g _ _ e _ _ _ Hi3 Ha L4 TR); rewrite Ks; discriminate).
    exists [LFn (gs_id g) sg a], [A.NFn (gs_id g) (tr_sig sg) (tr_inst a)], tm. split.
    { right; right; right; left. exists (tr_sig sg), (tr_inst a). split; [exact Ks|]. split; [exact Sg|].
      split; [exact Ap|]. split; reflexivity. }
    split; [reflexivity|].
    apply (ws_fn _ e e1 eu _ _ _ _ _ _ _ mi (gs_name g) s6 s6u s6u s7); [exact Hws | exact Th | | exact Heu | exact He2 | exact Cout]. destruct (nget (en_fns e) (gs_id g)) as [old|] eqn:O.
    + right. exists old, mi, (gs_name g), s2, s3. split; [reflexivity|]. split; [exact (ws_tri _ _ _ _ _ Hws) | exact He1].
    + left. injection He1 as <- _. split; reflexivity.
  - (* context *)
    destruct (gs_decl g) as [| | | | | |rcv0 ? ? ? ?|n its|] eqn:D; cbn in KO; try discriminate KO;
      try (destruct rcv0; discriminate KO). try rewrite D in Wd.
    assert (Gen : forall its0 acc s0 res s3, mfold (fun acc sp => add_spec f (fst acc) (gs_mod g) sp subst (snd acc)) acc its0 s0 = Good (res, s3) ->
              Inv s0 -> st_w s0 = w -> Later w P R Fl s3 -> forall tm0 les0, WS U (fst acc) tm0 les0 (snd acc) ->
              exists les' es tm', IEs WW (path_of (gs_mod g)) ob tm0 its0 es tm' /\ map tr_ent les' = es /\
                WS U (fst res) tm' (les0 ++ les') (snd res)).
    { induction its0 as [|sp r IH]; intros acc s0 res s3 Hm Hi0 Hw0 L0 tm0 les0 W0; cbn [mfold] in Hm.
      - injection Hm as <- <-. exists [], [], tm0. split; [constructor|]. split; [reflexivity|]. rewrite app_nil_r. exact W0.
      - destruct (bstep _ _ _ _ _ _ _ (ok_add_spec _ _ _ _ _ _) Hi0 Hm) as ([e1 n1] & s4 & H1 & H2 & Hi4 & F4).
        pose proof (fr_w' w _ _ F4 Hw0) as Hw4.
        destruct (ok_mfold _ _ _ r (e1, n1) (fun acc sp => ok_add_spec _ _ _ _ _ _) s4 res s3 Hi4 H2) as [_ F5].
        destruct (IHs _ _ _ _ _ _ _ _ _ U tm0 les0 ob H1 Hi0 Hw0 Hgm (later_from w R Fl _ _ F5 L0) W0 Hob Hvalid)
          as (l1 & es1 & tm1 & I1 & M1 & W1).
        destruct (IH (e1, n1) s4 res s3 H2 Hi4 Hw4 L0 tm1 (les0 ++ l1) W1) as (l2 & es2 & tm2 & I2 & M2 & W2).
        exists (l1 ++ l2), (es1 ++ es2), tm2. split; [exact (IsCons _ _ _ _ _ _ _ _ _ _ I1 I2)|].
        split; [rewrite map_app, M1, M2; reflexivity | rewrite app_assoc; exact W2]. }
    destruct (Gen its (e, needs) s1 (e', needs') s' H Hi1 Hw1 L tm les Hws) as (les' & es & tm' & I & M & W').
    exists les', es, tm'. split; [|split; [exact M | exact W']]. right; right; right; right; right.
    eexists _, n, its. split; [exact Wd|]. split; [reflexivity | exact I].
  - (* method *)
    noerr H.
  - (* functor *)
    destruct (gs_decl g) as [| | | | | |rcv0 ? ? ? ?| |n ps args res [bd|]] eqn:D; cbn in KO; try discriminate KO;
      try (destruct rcv0; discriminate KO); [noerr H|]. try rewrite D in Wd.
    destruct (bstep _ _ _ _ _ _ _ ok_fresh Hi1 H) as (st0 & s2 & _ & H2 & Hi2 & F2). clear H.
    destruct (bstep _ _ _ _ _ _ _ (ok_functor_fixed _) Hi2 H2) as (xs & s3 & Hfixed & H3 & Hi3 & F3). clear H2.
    apply bind_inv in H3 as (u3 & s4 & Hab & H4).
    destruct (filter _ xs) eqn:Ab in Hab; [|noerr Hab]. injection Hab as _ <-.
    destruct (bstep _ _ _ _ _ _ _ (ok_setdefault_app _ _ _) Hi3 H4) as (e2 & s5 & He2 & H5 & Hi5 & F5). clear H4.
    injection H5 as <- <- <-.
    assert(L3:Later w P R Fl s3)by exact(later_from w R Fl _ _ F5 L).
    assert(FixedIn:forall t,A.Fixed WW(gs_id g)t->In t xs).
    { intros t HF;exact(functor_fixed_covers w (P:=P) R Fl b Hload g n ps args res s2 xs s3 Hg D Hfixed Hi2
       ltac:(rewrite(fr_w _ _ F2);exact Hw1)L3 t HF). }
    assert(Present:forall t,In t xs->tn_get e t<>None).
    { intros t Hin Etn. assert(Missing:In t(filter(fun x=>negb(is_some(tn_get e x)))xs)).
      { apply filter_In;split;[exact Hin|rewrite Etn;reflexivity]. }
      rewrite Ab in Missing;exact Missing. }
    assert(Guard:forall t,A.Fixed WW(gs_id g)t->A.nlook t tm<>None).
    { intros t HF;rewrite(ws_tn _ _ _ _ _ Hws t).
      destruct(tn_get e t)eqn:Et;[discriminate|exfalso;exact(Present t(FixedIn t HF)Et)]. }
    pose proof(functor_fixed_exact w (P:=P) R Fl b Hload g n ps args res s2 xs s3 Hg D Hfixed Hi2
      ltac:(rewrite(fr_w _ _ F2);exact Hw1)L3)as FixedExact.
    set(fx:=map(fun x=>(x,opt_default YUnit(tn_get e x)))xs).
    assert(Apps:raw_fixed_app fx=map(fun x=>(x,tn_get e x))xs).
    { unfold raw_fixed_app,fx;rewrite map_map;apply map_ext_in;intros x Hin.
      cbn[fst snd];destruct(tn_get e x)eqn:Y;[reflexivity|exfalso;exact(Present x Hin Y)]. }
    assert(Read:A.FixedReadings WW(gs_id g)tm(tr_fixed fx)).
    { unfold A.FixedReadings,tr_fixed,fx;rewrite map_map;intros x tx;split.
      - intro Hx;apply in_map_iff in Hx as[y[Eq Hin]]. cbn[fst snd]in Eq;injection Eq as <- <-.
        split;[apply FixedExact;exact Hin|]. rewrite(ws_tn _ _ _ _ _ Hws y).
        destruct(tn_get e y)eqn:Y;[reflexivity|exfalso;exact(Present y Hin Y)].
      - intros[HF Hty]. apply in_map_iff;exists x;split;[|apply FixedIn;exact HF].
        cbn[fst snd]. rewrite(ws_tn _ _ _ _ _ Hws x)in Hty.
        destruct(tn_get e x)eqn:Y;[injection Hty as <-;reflexivity|discriminate]. }
    exists [LFun (gs_id g)fx], [A.NFun (gs_id g)(tr_fixed fx)], tm. split.
    { right;right;right;right;left. eexists _,n,ps,args,res,(tr_fixed fx).
      split;[exact Wd|]. split;[reflexivity|]. split;[exact Guard|]. split;[exact Read|]. auto. }
    split;[reflexivity|].
    apply(ws_fun U e e2 tm les needs(gs_id g)st0 fx s3 s5 Hws);[rewrite Apps;exact He2|exact Cout].

Qed.


Lemma kconv_inj : forall k1 k2, kconv k1 = kconv k2 -> k1 = k2.
Proof. intros [] []; cbn; intro H; try discriminate; reflexivity. Qed.

#[local] Hint Resolve ok_unify ok_reqs ok_symbol_type ok_ety_of ok_fn_sig ok_provision_app ok_setdefault_app ok_show : okdb.

Lemma ok_add_method_item : forall e mi r n subst needs, ok (add_method_item e mi r n subst needs).
Proof. intros. unfold add_method_item. okt; unfold unify_sig; okt. Qed.

(* the brackets of an item, as the elaborator reads them into its subst *)
Lemma subst_rel : forall (a : option (list (path * spec))) e mi outer s subst0 s' U tm les needs ob,
  (match a with
   | Some bs => mbind (app_subst 200 e mi bs outer) (fun l => ret (l ++ outer))
   | None => ret outer
   end) s = Good (subst0, s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> Later w P R Fl s' -> WS U e tm les needs ->
  map (trp Fl) outer = ob -> Forall (fun q => ety_ok (snd q) = true) outer ->
  exists bsr, map (trp Fl) subst0 = bsr ++ ob /\ Forall (fun q => ety_ok (snd q) = true) subst0 /\ (a = None -> bsr = []) /\
    forall x, (forall bs, a = Some bs -> forall b0 y, In b0 bs -> A.resolve_sym WW (path_of mi) (fst b0) = Some y ->
                 A.kind_sym WW y = Some A.DKType -> A.kind_sym WW x <> Some A.DKMeth -> In y (R x)) ->
      A.Brackets WW (path_of mi) (ob ++ tm) x a bsr.
Proof.
  intros a e mi outer s subst0 s' U tm les needs ob H Hi Hw Hmi L Hws Hob Hvalid.
  assert (Hwf : Forall (fun q => WFty (snd q)) outer).
  { eapply Forall_impl; [|exact Hvalid]. intros q Hq. apply ety_ok_wf. exact Hq. }
  destruct a as [bs|].
  - apply bind_inv in H as (l & s1 & H1 & H2). injection H2 as <- <-.
    pose proof (tmrel_walk _ _ _ _ _ _ _ Hws Hob Hwf) as TR.
    destruct (proj1 (proj2 (ty_all w R Fl b Hload 200)) e mi bs outer s l s1 (ob ++ tm) H1 Hi Hw Hmi TR L)
      as (lr & Fa & El & Wl).
    pose proof (proj1 (proj2 (source_clean w R Fl b Hload 200)) e mi bs outer s l s1 H1 Hi Hw Hmi
      (tmclean_of_forall outer Hvalid) (cc_types _ (ws_clean _ _ _ _ _ Hws)) L) as Cl.
    exists (rev lr). split; [rewrite map_app, El, Hob; reflexivity|]. split; [apply Forall_app; split; [exact Cl|exact Hvalid]|].
    split; [discriminate|]. intros x Rel. apply A.BrSome.
    assert (Gen : forall bs', incl bs' bs -> forall lr', Forall2 (BrRel w R Fl (path_of mi) (ob ++ tm)) bs' lr' ->
              Forall2 (fun b0 tb => exists sp sa, snd b0 = Spec (Some sp) None sa /\
                 A.resolve_sym WW (path_of mi) (fst b0) = Some (fst tb) /\ A.kind_sym WW (fst tb) = Some A.DKType /\
                 (A.kind_sym WW x <> Some A.DKMeth -> In (fst tb) (A.w_reqs WW x)) /\
                 A.TyOf WW (path_of mi) (ob ++ tm) None (ATRef sp sa) (snd tb)) bs' lr').
    { intros bs' Hin lr' F. induction F as [|b0 tb r r' Hb Hr' IHr]; constructor.
      - destruct Hb as (sp & sa & E1 & E2 & E3 & E4). exists sp, sa. split; [exact E1|]. split; [exact E2|].
        split; [exact E3|]. split; [|exact E4]. exact (Rel bs eq_refl b0 _ (Hin b0 (or_introl eq_refl)) E2 E3).
      - apply IHr. intros z Hz. apply Hin. right. exact Hz. }
    exact (Gen bs (incl_refl _) lr Fa).
  - injection H as <- <-. exists []. split; [exact Hob|]. split; [exact Hvalid|]. split; [reflexivity|].
    intros x _. apply A.BrNone.
Qed.

Lemma isym_ie : forall mi pp g a bsr ob tm es tm', A.resolve_sym WW (path_of mi) pp = Some (gs_id g) ->
  nget (w_syms w) (gs_id g) = Some g -> (blessb g = true -> a = None) ->
  A.Brackets WW (path_of mi) (ob ++ tm) (gs_id g) a bsr -> ISym (gs_id g) (bsr ++ ob) tm es tm' ->
  IE WW (path_of mi) ob tm (Spec (Some pp) None a) es tm'.
Proof.
  intros mi pp g a bsr ob tm es tm' Rs Hg Nb B Is.
    pose proof (kind_sym_rd w R Fl b Hload _ _ Hg) as Ks.
    pose proof (w_decl_rd w R Fl _ _ Hg) as Wd.
    assert (Bt : forall k, gs_kind g = k -> blessb g = match k with KCtx | KFn => false | KVal => blessb g | _ => true end)
      by (intros k Ek; unfold blessb; rewrite Ek; destruct k; reflexivity).
    destruct Is as [(K & -> & ->)|[(k & K & Hk & -> & ->)|[(d & n & t & ty0 & ap & D & Da & Ty & Ap & -> & ->)|
                   [(g0 & ap & K & Sg & Ap & -> & ->)|[(d & n & ps & args & res & fx & D & Da & Guard & Read & -> & ->)|(d & n & its & D & Da & Is)]]]]].
    + (* type *)
      rewrite Ks in K. injection K as K. apply kconv_type in K. rewrite (Nb ltac:(rewrite (Bt _ K); reflexivity)).
      exact (IEType _ _ _ _ _ _ Rs (eq_trans Ks (f_equal _ (f_equal kconv K)))).
    + (* nothing *)
      pose proof K as K0. assert (Ea : a = None).
      { apply Nb. rewrite Ks in K. injection K as K.
        destruct Hk as [->|[->|[->|(-> & d & n & t & i & D & Da)]]];
          [change A.DKUnit with (kconv KUnit) in K | change A.DKTag with (kconv KTag) in K
          | change A.DKAlias with (kconv KAlias) in K | change A.DKVal with (kconv KVal) in K];
          apply kconv_inj in K; try rewrite (Bt _ K); try reflexivity.
        unfold blessb. rewrite K. rewrite Wd in D. injection D as <-. cbn in Da. rewrite Da. reflexivity. }
      rewrite Ea. exact (IENothing _ _ _ _ _ _ _ Rs K0 Hk).
    + (* val *)
      rewrite <- app_assoc in Ty, Ap. exact (IEVal _ _ _ _ _ _ _ _ _ _ _ _ _ Rs D Da B Ty Ap).
    + (* fn *)
      rewrite <- app_assoc in Sg, Ap. exact (IEFn _ _ _ _ _ _ _ _ _ _ Rs K B Sg Ap).
    + (* functor *)
      assert (Ea : a = None).
      { apply Nb. rewrite Wd in D. injection D as <-. cbn in Da. unfold blessb. rewrite Da.
        pose proof (load_world_kind b w Hload _ _ Hg) as KO. unfold KindOK in KO. rewrite Da in KO. cbn in KO.
        injection KO as KO. change A.DKFunctor with (kconv KFunctor) in KO. apply kconv_inj in KO. rewrite <- KO. reflexivity. }
      rewrite Ea. exact(IEFun _ _ _ _ _ _ _ _ _ _ _ _ Rs D Da Guard Read).
    + (* context *)
      exact (IECtx _ _ _ _ _ _ _ _ _ _ _ _ _ Rs D Da B Is).
Qed.

Lemma spec_step : forall f, SymOK' f -> MethStep -> SpecOK' (S f).
Proof.
  intros f IHy MS e mi sp outer needs s e' needs' s' U tm les ob H Hi Hw Hmi L Hws Hob Hwf.
  destruct sp as [p d a]. cbn [add_spec] in H.
  assert (Ok0 : ok (match a with
                    | Some bs => mbind (app_subst 200 e mi bs outer) (fun l => ret (l ++ outer))
                    | None => ret outer end))
    by (destruct a; [apply ok_bind; [apply ok_app_subst | intro; apply ok_ret] | apply ok_ret]).
  destruct (bstep _ _ _ _ _ _ _ Ok0 Hi H) as (subst0 & s1 & H1 & H2 & Hi1 & F1). clear H.
  cbn beta zeta in H2. pose proof (fr_w' w _ _ F1 Hw) as Hw1.
  destruct d as [dn|], p as [pp|]; try noerr H2.
  - (* a method item *)
    apply bind_inv in H2 as (r & s2 & H3 & H4).
    destruct (rts_spec w R Fl b Hload mi pp s1 r s2 Hi1 Hw1 Hmi H3) as (-> & Rs & Hr & _).
    destruct (ok_add_method_item e mi r dn subst0 needs s1 (e', needs') s' Hi1 H4) as [_ F4].
    destruct (subst_rel a e mi outer s subst0 s1 U tm les needs ob H1 Hi Hw Hmi (later_from w R Fl _ _ F4 L) Hws Hob Hwf)
      as (bsr & Ms & Ws & Bn & Br).
    destruct (MS e mi r dn subst0 needs s1 e' needs' s' U tm les (bsr ++ ob) H4 Hi1 Hw1 Hmi Hr L Hws Ms Ws)
      as (m & rt & g & ap & les' & Mn & Km & Rt & Sg & Ap & Me & W').
    exists les', [A.NMth rt m g ap], tm. split; [|split; [exact Me | exact W']].
    rewrite <- app_assoc in Rt, Sg, Ap.
    apply (IEMeth _ _ _ _ _ _ _ _ _ _ _ bsr ap Rs Mn); [|exact Rt | exact Sg | exact Ap].
    apply Br. intros bs _ b0 y _ _ _ Hk. exfalso. exact (Hk Km).
  - (* a symbol item *)
    apply bind_inv in H2 as (tg & s2 & H3 & H4).
    destruct (rpath_rel w R Fl b Hload mi pp s1 tg s2 Hw1 Hmi H3) as (-> & Hres).
    destruct tg as [[x|j]|]; try noerr H4.
    apply bind_inv in H4 as (g & s3 & H5 & H6).
    destruct (sym_loader w R Fl b Hload _ _ x s1 g s3 Hi1 Hw1 Hres H5) as (-> & Hg).
    pose proof (load_world_ids b w Hload x g Hg) as Eid. rewrite <- Eid in Hg.
    assert (Rs : A.resolve_sym WW (path_of mi) pp = Some (gs_id g))
      by (unfold A.resolve_sym; change (A.resolve (W_read w R Fl)) with (A.resolve WW); rewrite Hres, Eid; reflexivity).
    assert (Okr : ok (match a with Some bs => check_relevant mi g bs | None => ret tt end))
      by (destruct a; [unfold check_relevant; okt | apply ok_ret]).
    destruct (bstep _ _ _ _ _ _ _ Okr Hi1 H6) as (u & s4 & Hrel & H7 & Hi4 & F4). clear H6.
    pose proof (fr_w' w _ _ F4 Hw1) as Hw4.
    change (match gs_kind g, gs_decl g with
            | KCtx, _ | KFn, _ | KVal, ADVal _ _ None => false
            | _, _ => true end) with (blessb g) in H7.
    destruct (is_some a && blessb g) eqn:Bl; [noerr H7|].
    assert (Nb : blessb g = true -> a = None) by (intro E; rewrite E, andb_true_r in Bl; destruct a; [discriminate | reflexivity]).
    destruct (ok_add_symbol_item f e mi g subst0 needs s4 (e', needs') s' Hi4 H7) as [_ F7].
    pose proof (later_from w R Fl _ _ F7 L) as L4. pose proof (later_from w R Fl _ _ F4 L4) as L1.
    destruct (subst_rel a e mi outer s subst0 s1 U tm les needs ob H1 Hi Hw Hmi L1 Hws Hob Hwf)
      as (bsr & Ms & Ws & Bn & Br).
    destruct (IHy e mi g subst0 needs s4 e' needs' s' U tm les _ H7 Hi4 Hw4 Hmi Hg L Hws Ms Ws) as (les' & es & tm' & Is & Me & W').
    exists les', es, tm'. split; [|split; [exact Me | exact W']].
    assert (Rel : forall bs, a = Some bs -> forall b0 y, In b0 bs -> A.resolve_sym WW (path_of mi) (fst b0) = Some y ->
                    A.kind_sym WW y = Some A.DKType -> A.kind_sym WW (gs_id g) <> Some A.DKMeth -> In y (R (gs_id g))).
    { intros bs -> b0 y Hb Hy Ky _. exact (check_relevant_spec w R Fl b Hload mi g bs s1 u s4 Hi1 Hw1 Hmi Hrel L4 b0 y Hb Hy Ky). }
    exact (isym_ie mi pp g a bsr ob tm es tm' Rs Hg Nb (Br (gs_id g) Rel) Is).
Qed.


Section Walks.
Hypothesis MS : MethStep.

Lemma walk_all : forall f, SymOK' f /\ SpecOK' f.
Proof.
  induction f as [|f [IY IS]]; [exact (conj sym_zero spec_zero)|].
  exact (conj (sym_step f IS) (spec_step f IY MS)).
Qed.

Lemma ok_ctx_fuel : ok ctx_fuel.
Proof. unfold ctx_fuel, walk_fuel. okt. Qed.

Lemma assume_step : forall e mi it needs s e' needs' s' U tm les,
  add_assume_item e mi it needs s = Good ((e', needs'), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> Later w P R Fl s' -> WS U e tm les needs ->
  exists les' es tm', IE WW (path_of mi) [] tm (A.assume_spec it) es tm' /\ map tr_ent les' = es /\
    WS U e' tm' (les ++ les') needs'.
Proof.
  intros e mi [pp [dn|]] needs s e' needs' s' U tm les H Hi Hw Hmi L Hws; cbn [add_assume_item A.assume_spec fst snd] in *.
  - apply bind_inv in H as (r & s2 & H3 & H4).
    destruct (rts_spec w R Fl b Hload mi pp s r s2 Hi Hw Hmi H3) as (-> & Rs & Hr & _).
    destruct (MS e mi r dn [] needs s e' needs' s' U tm les [] H4 Hi Hw Hmi Hr L Hws eq_refl (Forall_nil _))
      as (m & rt & g & ap & les' & Mn & Km & Rt & Sg & Ap & Me & W').
    exists les', [A.NMth rt m g ap], tm. split; [|split; [exact Me | exact W']].
    exact (IEMeth _ _ _ _ _ _ _ _ _ _ _ [] ap Rs Mn (A.BrNone _ _ _ _) Rt Sg Ap).
  - apply bind_inv in H as (tg & s2 & H3 & H4).
    destruct (rpath_rel w R Fl b Hload mi pp s tg s2 Hw Hmi H3) as (-> & Hres).
    destruct tg as [[x|j]|]; try noerr H4.
    apply bind_inv in H4 as (g & s3 & H5 & H6).
    destruct (sym_loader w R Fl b Hload _ _ x s g s3 Hi Hw Hres H5) as (-> & Hg).
    pose proof (load_world_ids b w Hload x g Hg) as Eid. rewrite <- Eid in Hg.
    assert (Rs : A.resolve_sym WW (path_of mi) pp = Some (gs_id g))
      by (unfold A.resolve_sym; change (A.resolve (W_read w R Fl)) with (A.resolve WW); rewrite Hres, Eid; reflexivity).
    destruct (bstep _ _ _ _ _ _ _ ok_ctx_fuel Hi H6) as (cf & s4 & _ & H7 & Hi4 & F4). clear H6.
    pose proof (fr_w' w _ _ F4 Hw) as Hw4.
    destruct (proj1 (walk_all cf) e mi g [] needs s4 e' needs' s' U tm les [] H7 Hi4 Hw4 Hmi Hg L Hws eq_refl (Forall_nil _))
      as (les' & es & tm' & Is & Me & W').
    exists les', es, tm'. split; [|split; [exact Me | exact W']].
    exact (isym_ie mi pp g None [] [] tm es tm' Rs Hg (fun _ => eq_refl) (A.BrNone _ _ _ _) Is).
Qed.

#[local] Hint Resolve ok_add_method_item ok_ctx_fuel ok_add_symbol_item : okdb.
Lemma ok_add_assume_item : forall e mi it needs, ok (add_assume_item e mi it needs).
Proof. intros e mi [p0 [d0|]] needs; unfold add_assume_item; okt. Qed.

Lemma assumes_walk : forall mi its e needs s e' needs' s' U tm les,
  mfold (fun acc it => add_assume_item (fst acc) mi it (snd acc)) (e, needs) its s = Good ((e', needs'), s') ->
  Inv s -> st_w s = w -> In mi (w_order w) -> Later w P R Fl s' -> WS U e tm les needs ->
  exists les' es tm', IEs WW (path_of mi) [] tm (map A.assume_spec its) es tm' /\ map tr_ent les' = es /\
    WS U e' tm' (les ++ les') needs'.
Proof.
  intros mi its. induction its as [|it r IH]; intros e needs s e' needs' s' U tm les H Hi Hw Hmi L Hws; cbn [mfold map] in *.
  - injection H as <- <- <-. exists [], [], tm. split; [constructor|]. split; [reflexivity|]. rewrite app_nil_r. exact Hws.
  - assert (Ok : forall acc it, ok (add_assume_item (fst acc) mi it (snd acc))) by (intros; apply ok_add_assume_item).
    destruct (bstep _ _ _ _ _ _ _ (Ok (e, needs) it) Hi H) as ([e1 n1] & s1 & H1 & H2 & Hi1 & F1).
    pose proof (fr_w' w _ _ F1 Hw) as Hw1.
    destruct (ok_mfold _ _ _ r (e1, n1) Ok s1 (e', needs') s' Hi1 H2) as [_ F2].
    destruct (assume_step e mi it needs s e1 n1 s1 U tm les H1 Hi Hw Hmi (later_from w R Fl _ _ F2 L) Hws)
      as (l1 & es1 & tm1 & I1 & M1 & W1).
    destruct (IH e1 n1 s1 e' needs' s' U tm1 (les ++ l1) H2 Hi1 Hw1 Hmi L W1) as (l2 & es2 & tm2 & I2 & M2 & W2).
    exists (l1 ++ l2), (es1 ++ es2), tm2. split; [exact (IsCons _ _ _ _ _ _ _ _ _ _ I1 I2)|].
    split; [rewrite map_app, M1, M2; reflexivity | rewrite app_assoc; exact W2].
Qed.

End Walks.

(* a run starts from the merges U alone *)
Lemma ws_start : forall U, Tri U -> UfOK U -> WS U (env_with_edges [] U) [] [] [].
Proof.
  intros U T O. constructor; cbn; auto.
  - intros k p [].
  - exists []. rewrite app_nil_r. reflexivity.
  - intros v y a [].
  - intros v y0 H. discriminate.
  - intros f g a [].
  - intros f g0 H. discriminate.
  - intros _ r m g a [].
  - intros r m g0 H. discriminate.
  - intros _ k. split; [intros [] | intros (l & [] & _)].
  - constructor.
  - intro u. split; [intro H;exfalso;exact (H eq_refl) | intros[fx[]]].
  - intros r m g a [].
  - constructor; cbn; try (intros; discriminate).
    + intros k p [].
    + exact O.
  - intros m rows At;discriminate At.
  - intros E m rows At;discriminate At.
  - intros _ l k a [].
  - intros k a0 G. discriminate G.
Qed.
End Walk.
