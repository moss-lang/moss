(* ------------------------------------------------------------------------ *)
(* InterpreterFast.v: InterpreterLazy's interpreter with trie-keyed provisions.         *)
(*                                                                           *)
(* InterpreterLazy (like Audit's reference interpreter) keeps an environment's    *)
(* provisions as an association list, searched with item_eqb from the      *)
(* front. Here the same interpreter, clause for clause, keeps them in a     *)
(* store of tries keyed by the item's symbol, one per item kind; a method   *)
(* item's trie holds, per method symbol, a short bucket of (receiver, θ)    *)
(* entries searched from the front. Type meanings are InterpreterLazy's,          *)
(* unchanged.                                                                *)
(*                                                                           *)
(* A store is related to a list when every lookup agrees (ERel, with the    *)
(* provisions related in turn). Proved, for every primitive implementation *)
(* and every fuel:                                                          *)
(*  - feval_agrees: on related environments, `feval` returns exactly what  *)
(*    InterpreterLazy's `leval` returns (bindings and applications: related      *)
(*    environments, equal locals and states);                               *)
(*  - frun_main_agrees: `frun_main` on `fast_env d0` is `run_main` on d0.   *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith String.
From NM Require Import Audit NatTrie InterpreterLazy.
From NM Require Import PublicValueTyping.
Import Audit.Semantics ListNotations.

Local Open Scope nat_scope.

(* ===================== Tries ============================================ *)

(* Union, `a` winning, and the combination of entries both hold. *)
Fixpoint tcombine {A} (f : A -> A -> A) (a b : tmap A) : tmap A :=
  match a, b with
  | TE, _ | TL _ None, _ => b
  | _, TE | _, TL _ None => a
  | TL k (Some x), _ => tset b k (match tget b k with Some y => f x y | None => x end)
  | _, TL k (Some y) => tset a k (match tget a k with Some x => f x y | None => y end)
  | TN l x r, TN l' y r' =>
      TN (tcombine f l l')
        (match x, y with Some x, Some y => Some (f x y) | Some x, None => Some x | None, y => y end)
        (tcombine f r r')
  end.

Local Lemma tget_combine_leaf_left : forall A (f : A -> A -> A) b j x k,
  tget (tset b j (match tget b j with Some y => f x y | None => x end)) k =
  match tget (TL j (Some x)) k, tget b k with
  | Some x, Some y => Some (f x y) | Some x, None => Some x | None, y => y end.
Proof.
  intros. rewrite tget_tset. cbn [tget]. destruct (Nat.eqb k j) eqn:E.
  - apply Nat.eqb_eq in E. subst k. destruct (tget b j); reflexivity.
  - destruct (tget b k); reflexivity.
Qed.

Local Lemma tget_combine_leaf_right : forall A (f : A -> A -> A) a j y k,
  tget (tset a j (match tget a j with Some x => f x y | None => y end)) k =
  match tget a k, tget (TL j (Some y)) k with
  | Some x, Some y => Some (f x y) | Some x, None => Some x | None, y => y end.
Proof.
  intros. rewrite tget_tset. cbn [tget]. destruct (Nat.eqb k j) eqn:E.
  - apply Nat.eqb_eq in E. subst k. destruct (tget a j); reflexivity.
  - destruct (tget a k); reflexivity.
Qed.

Local Lemma tget_empty_leaf : forall A j k, tget (@TL A j None) k = None.
Proof. intros. cbn [tget]. destruct (Nat.eqb k j); reflexivity. Qed.

Lemma tget_tcombine : forall A (f : A -> A -> A) a b k,
  tget (tcombine f a b) k =
  match tget a k, tget b k with
  | Some x, Some y => Some (f x y) | Some x, None => Some x | None, y => y end.
Proof.
  intros A f a. induction a as [|j [x|]|l IHl v r IHr]; intros b k.
  - reflexivity.
  - destruct b as [|i [y|]|l' v' r']; cbn [tcombine].
    + destruct (tget (TL j (Some x)) k); reflexivity.
    + apply tget_combine_leaf_left.
    + rewrite tget_empty_leaf. destruct (tget (TL j (Some x)) k); reflexivity.
    + apply tget_combine_leaf_left.
  - destruct b; cbn [tcombine]; rewrite tget_empty_leaf; reflexivity.
  - destruct b as [|j [x|]|l' v' r']; cbn [tcombine].
    + destruct (tget (TN l v r) k); reflexivity.
    + apply tget_combine_leaf_right.
    + rewrite tget_empty_leaf. destruct (tget (TN l v r) k); reflexivity.
    + cbn [tget]. destruct (Nat.eqb k 0); [destruct v, v'; reflexivity|].
      destruct (tleft k); [apply IHl|apply IHr].
Qed.

(* ===================== Stores =========================================== *)

Inductive fprov : Type :=
| FValue : value -> fprov
| FFunction : fsym -> fenv -> fprov
| FFunctor : usym -> option smap -> fenv -> option (list (option nat)) -> fprov
| FPrimitive : op -> option ty -> fprov
with fenv : Type :=
| FEnv : lsubst -> lsubst -> fstore -> fenv
with fstore : Type :=
| FStore : tmap fprov -> tmap fprov -> tmap fprov
    -> tmap (list ((ty * subst) * fprov)) -> list (tsym * fprov) -> fstore
| FOverlay : item -> fprov -> fstore -> fstore.

Definition fmeanings (d : fenv) := match d with FEnv ts _ _ => ts end.
Definition faliases (d : fenv) := match d with FEnv _ a _ => a end.
Definition fstore_of (d : fenv) := match d with FEnv _ _ s => s end.

Definition fempty_store := FStore TE TE TE TE [].
Definition fempty := FEnv (DS []) (DS []) fempty_store.

Fixpoint bucket_get (r : ty) (th : subst) (b : list ((ty * subst) * fprov)) : option fprov :=
  match b with
  | [] => None
  | ((r', th'), p) :: rest => if ty_eqb r r' && subst_eqb th th' then Some p else bucket_get r th rest
  end.
Fixpoint tys_get (t : tsym) (l : list (tsym * fprov)) : option fprov :=
  match l with
  | [] => None
  | (t', p) :: rest => if Nat.eqb t t' then Some p else tys_get t rest
  end.

Fixpoint fget (i : item) (s : fstore) : option fprov :=
  match s with
  | FStore vs fs us ms ts =>
      match i with
      | ITy t => tys_get t ts
      | IVal v => tget vs v
      | IFn f => tget fs f
      | IFun u => tget us u
      | IMth r m th => match tget ms m with Some b => bucket_get r th b | None => None end
      end
  | FOverlay j p tail => if fast_item_eqb i j then Some p else fget i tail
  end.

Definition fset_tries (i : item) (p : fprov) (s : fstore) : fstore :=
  match s with
  | FStore vs fs us ms ts =>
      match i with
      | ITy t => FStore vs fs us ms ((t, p) :: ts)
      | IVal v => FStore (tset vs v p) fs us ms ts
      | IFn f => FStore vs (tset fs f p) us ms ts
      | IFun u => FStore vs fs (tset us u p) ms ts
      | IMth r m th =>
          FStore vs fs us
            (tset ms m (((r, th), p) :: match tget ms m with Some b => b | None => [] end)) ts
      end
  | FOverlay _ _ _ => FOverlay i p s
  end.

Fixpoint compact (s : fstore) : fstore :=
  match s with
  | FStore _ _ _ _ _ => s
  | FOverlay i p rest => fset_tries i p (compact rest)
  end.

Definition fset (i : item) (p : fprov) (s : fstore) : fstore :=
  fset_tries i p (compact s).

(* `a`'s entries before `b`'s, as list append. *)
Definition funion_tries (a b : fstore) : fstore :=
  match a, b with
  | FStore va fa ua ma ta, FStore vb fb ub mb tb =>
      FStore (tcombine (fun x _ => x) va vb) (tcombine (fun x _ => x) fa fb)
        (tcombine (fun x _ => x) ua ub) (tcombine (fun x y => x ++ y) ma mb) (ta ++ tb)
  | _, _ => a
  end.

Definition funion (a b : fstore) : fstore := funion_tries (compact a) (compact b).

Lemma bucket_get_app : forall r th a b,
  bucket_get r th (a ++ b) = match bucket_get r th a with Some p => Some p | None => bucket_get r th b end.
Proof.
  intros r th a b. induction a as [|[[r' th'] p] rest IH]; [reflexivity|].
  cbn. destruct (ty_eqb r r' && subst_eqb th th'); [reflexivity|exact IH].
Qed.
Lemma tys_get_app : forall t a b,
  tys_get t (a ++ b) = match tys_get t a with Some p => Some p | None => tys_get t b end.
Proof.
  intros t a b. induction a as [|[t' p] rest IH]; [reflexivity|].
  cbn. destruct (Nat.eqb t t'); [reflexivity|exact IH].
Qed.

Lemma fget_fset_tries : forall i j p s,
  fget i (fset_tries j p s) = if item_eqb i j then Some p else fget i s.
Proof.
  intros i j p [vs fs us ms ts|key entry tail].
  2: { cbn [fset_tries fget]. rewrite fast_item_eqb_spec. reflexivity. }
  destruct i, j; cbn [fget fset_tries item_eqb]; try reflexivity;
    rewrite ?tget_tset; try reflexivity.
  all: destruct (Nat.eqb m m0) eqn:E.
  all: try (rewrite andb_false_r, andb_false_l; reflexivity).
  apply Nat.eqb_eq in E. subst m0. cbn [bucket_get].
  destruct (ty_eqb t t0), (subst_eqb s s0); cbn; try reflexivity;
    destruct (tget ms m); reflexivity.
Qed.

Lemma fget_compact : forall i s, fget i (compact s) = fget i s.
Proof.
  intros i s. induction s; [reflexivity|].
  cbn [compact fget]. rewrite fget_fset_tries, IHs, fast_item_eqb_spec. reflexivity.
Qed.

Lemma fget_fset : forall i j p s,
  fget i (fset j p s) = if item_eqb i j then Some p else fget i s.
Proof. intros. unfold fset. rewrite fget_fset_tries, fget_compact. reflexivity. Qed.

Lemma compact_store : forall s, exists vs fs us ms ts, compact s = FStore vs fs us ms ts.
Proof.
  induction s as [vs fs us ms ts|key entry tail IH].
  - do 5 eexists. reflexivity.
  - destruct IH as (vs & fs & us & ms & ts & E). cbn [compact]. rewrite E.
    destruct key; cbn [fset_tries]; do 5 eexists; reflexivity.
Qed.

Lemma fget_funion_tries : forall i va fa ua ma ta vb fb ub mb tb,
  fget i (funion_tries (FStore va fa ua ma ta) (FStore vb fb ub mb tb)) =
  match fget i (FStore va fa ua ma ta) with Some p => Some p | None => fget i (FStore vb fb ub mb tb) end.
Proof.
  intros. destruct i; cbn [fget funion_tries].
  - apply tys_get_app.
  - rewrite tget_tcombine. destruct (tget va v), (tget vb v); reflexivity.
  - rewrite tget_tcombine. destruct (tget fa f), (tget fb f); reflexivity.
  - rewrite tget_tcombine. destruct (tget ma m) as [x|], (tget mb m) as [y|]; try reflexivity.
    + apply bucket_get_app.
    + destruct (bucket_get t s x); reflexivity.
  - rewrite tget_tcombine. destruct (tget ua u), (tget ub u); reflexivity.
Qed.

Lemma fget_funion : forall i a b,
  fget i (funion a b) = match fget i a with Some p => Some p | None => fget i b end.
Proof.
  intros i a b. unfold funion.
  rewrite <- (fget_compact i a), <- (fget_compact i b).
  destruct (compact_store a) as (va & fa & ua & ma & ta & Ea).
  destruct (compact_store b) as (vb & fb & ub & mb & tb & Eb).
  rewrite Ea, Eb. apply fget_funion_tries.
Qed.

Lemma fget_empty : forall i, fget i fempty_store = None.
Proof. intros []; reflexivity. Qed.

(* ===================== Relation to InterpreterLazy's environments ============== *)

Inductive orel {A B} (R : A -> B -> Prop) : option A -> option B -> Prop :=
| ORNone : orel R None None
| ORSome : forall x y, R x y -> orel R (Some x) (Some y).

Inductive PRel : fprov -> lprov -> Prop :=
| PRValue : forall w, PRel (FValue w) (LValue w)
| PRFunction : forall f fd ld, ERel fd ld -> PRel (FFunction f fd) (LFunction f ld)
| PRFunctor : forall u a fd ld q, ERel fd ld -> PRel (FFunctor u a fd q) (LFunctor u a ld q)
| PRPrimitive : forall p o, PRel (FPrimitive p o) (LPrimitive p o)
with ERel : fenv -> lenv -> Prop :=
| ERelI : forall ts a s ps,
    (forall i, orel PRel (fget i s) (llookup_entry i ps)) -> ERel (FEnv ts a s) (LEnv ts a ps).

(* An eager conversion, used once for main's initial environment. *)
Fixpoint fast_prov (p : lprov) : fprov :=
  match p with
  | LValue w => FValue w
  | LFunction f d => FFunction f (fast_env d)
  | LFunctor u a d q => FFunctor u a (fast_env d) q
  | LPrimitive p o => FPrimitive p o
  end
with fast_env (d : lenv) : fenv :=
  match d with
  | LEnv ts a ps => FEnv ts a
      ((fix go (l : list (item * lprov)) : fstore :=
          match l with [] => fempty_store | (i, p) :: r => fset i (fast_prov p) (go r) end) ps)
  end.

Fixpoint fast_prov_rel (p : lprov) : PRel (fast_prov p) p
with fast_env_rel (d : lenv) : ERel (fast_env d) d.
Proof.
  - destruct p; cbn [fast_prov]; constructor; apply fast_env_rel.
  - destruct d as [ts a ps]. cbn [fast_env]. constructor. intro i.
    induction ps as [|[j p] r IH]; cbn [llookup_entry].
    + rewrite fget_empty. constructor.
    + rewrite fget_fset, <- fast_item_eqb_spec. destruct (fast_item_eqb i j).
      * constructor. apply fast_prov_rel.
      * exact IH.
Qed.

(* Number the arguments in one pass over the values. *)
Fixpoint parameter_values_from (i : nat) (ws : list value) : locals :=
  match ws with
  | [] => []
  | w :: rest => (i, w) :: parameter_values_from (S i) rest
  end.
Definition fast_parameter_values := parameter_values_from 0.

Lemma parameter_values_from_spec : forall i ws,
  parameter_values_from i ws = combine (seq i (List.length ws)) ws.
Proof. intros i ws. revert i. induction ws; cbn; intros; [reflexivity|now rewrite IHws]. Qed.

Lemma fast_parameter_values_spec : forall ws, fast_parameter_values ws = parameter_values ws.
Proof. apply parameter_values_from_spec. Qed.

(* ===================== Environment operations ============================ *)

(* The type meanings of a fast environment, as an InterpreterLazy environment
   without provisions: InterpreterLazy's type operations read only meanings. *)
Definition lshadow (d : fenv) : lenv := LEnv (fmeanings d) (faliases d) [].

Definition flookup_provision (i : item) (d : fenv) := fget i (fstore_of d).
Definition finstall (key : item) (entry : fprov) (d : fenv) :=
  FEnv (fmeanings d) (faliases d) (fset key entry (fstore_of d)).
Definition fappend (a b : fenv) :=
  FEnv (dappend (fmeanings a) (fmeanings b)) (dappend (faliases a) (faliases b)) (funion (fstore_of a) (fstore_of b)).
Definition ftyped_capture (d : fenv) (types : type_capture) (dc : fenv) : fenv :=
  FEnv (ldefer_all (fmeanings d) (capture_generations types))
    (ldefer_all (fmeanings d) (capture_aliases types)) (fstore_of dc).
Definition fprimitive_closure (d : fenv) (i : item) (p : op) : fprov :=
  FPrimitive p (match i with
    | IMth tau _ _ => Some (lapp (fmeanings d) tau)
    | _ => None end).
(* A primitive binding often introduces the very next lookup. Keep that
   row in front of the trie and fold it back before another installation. *)
Definition finstall_primitive (d : fenv) (i : item) (p : op) : fenv :=
  FEnv (fmeanings d) (faliases d)
    (FOverlay i (fprimitive_closure d i p) (compact (fstore_of d))).

(* Selecting provisions constructs only the store. The new environment
   has empty type tables, shared by all selections. *)
Fixpoint fselect_store (s : fstore) (sm : smap) : option fstore :=
  match sm with
  | [] => Some fempty_store
  | (req, src) :: rest =>
      match fget src s with
      | None => None
      | Some en => option_map (fset req en) (fselect_store s rest)
      end
  end.
Definition selected_environment (s : fstore) : fenv := FEnv (DS []) (DS []) s.
Definition fselect_provisions (d : fenv) (sm : smap) : option fenv :=
  option_map selected_environment (fselect_store (fstore_of d) sm).

Lemma fselect_provisions_cons : forall d req src rest,
  fselect_provisions d ((req, src) :: rest) =
  match flookup_provision src d, fselect_provisions d rest with
  | Some en, Some d' => Some (finstall req en d')
  | _, _ => None
  end.
Proof.
  intros. unfold fselect_provisions, flookup_provision. cbn [fselect_store].
  destruct (fget src (fstore_of d)), (fselect_store (fstore_of d) rest); reflexivity.
Qed.
Definition fadapt_provisions (d : fenv) (a : option smap) : option fenv :=
  match a with
  | None => Some d
  | Some sm => fselect_provisions d sm
  end.
Definition fbind_type (d : fenv) (t : tsym) (tau : ty) : fenv :=
  FEnv (fmeanings d) (DS ((t, ldefer (fmeanings d) tau) :: dforce (faliases d))) (fstore_of d).
Definition fcall_environment (d : fenv) (th : subst) (dc : fenv) : fenv :=
  FEnv (ldefer_onto (fmeanings d) th (fmeanings dc))
    (ldefer_onto (fmeanings d) th (faliases dc)) (fstore_of dc).
(* A direct call selects provisions into an environment with no captured
   types. Its two type tables are the same instantiated parameters. *)
Definition fprepare_call (d : fenv) (th : subst) (sm : smap) : option fenv :=
  match th with
  | [] => fselect_provisions d sm
  | _ => option_map (fun s => let meanings := ldefer_all (fmeanings d) th in FEnv meanings meanings s)
           (fselect_store (fstore_of d) sm)
  end.

Lemma fprepare_call_spec : forall d th sm,
  fprepare_call d th sm = option_map (fcall_environment d th) (fselect_provisions d sm).
Proof.
  intros d th sm. unfold fprepare_call. destruct th as [|q th].
  - unfold fselect_provisions. destruct (fselect_store (fstore_of d) sm); reflexivity.
  - unfold fselect_provisions.
    destruct (fselect_store (fstore_of d) sm); cbn [option_map]; [|reflexivity].
    unfold fcall_environment, selected_environment, ldefer_all, ldefer_onto.
    cbn [fmeanings faliases fstore_of dforce]. rewrite app_nil_r. reflexivity.
Qed.

Definition fapplication_environment (P : program_view) (d : fenv) (h : usym) (th : subst) (dc : fenv) : fenv :=
  let e := lapplication_environment P (lshadow d) h th (lshadow dc) in
  FEnv (lmeanings e) (laliases e) (fstore_of dc).
Definition fresult_environment (outputs : list (tsym * tsym))
    (d3 : fenv) (Dr : tele) (dr : fenv) : option fenv :=
  match lread_result_types (lshadow d3) Dr outputs with
  | Some (writes, aliases) => Some (FEnv (DS writes) (DS aliases) (fstore_of dr))
  | None => None
  end.

(* ===================== The operations preserve the relation ============= *)

Lemma erel_meanings : forall fd ld, ERel fd ld -> fmeanings fd = lmeanings ld /\ faliases fd = laliases ld.
Proof. intros fd ld H. destruct H. split; reflexivity. Qed.

Lemma erel_lookup : forall fd ld i, ERel fd ld ->
  orel PRel (flookup_provision i fd) (llookup_provision i ld).
Proof. intros fd ld i H. destruct H as [ts a s ps H]. apply H. Qed.

Lemma lookup_cases : forall fd ld i, ERel fd ld ->
  (flookup_provision i fd = None /\ llookup_provision i ld = None) \/
  exists x y, flookup_provision i fd = Some x /\ llookup_provision i ld = Some y /\ PRel x y.
Proof.
  intros fd ld i H. destruct (erel_lookup fd ld i H) as [|x y R]; [left; split; reflexivity|right].
  exists x, y. repeat split; assumption.
Qed.

Lemma erel_install : forall fd ld i p q, ERel fd ld -> PRel p q -> ERel (finstall i p fd) (linstall i q ld).
Proof.
  intros fd ld i p q H R. destruct H as [ts a s ps H]. cbn. constructor. intro j.
  rewrite fget_fset. cbn [llookup_entry]. rewrite fast_item_eqb_spec.
  destruct (item_eqb j i); [constructor; exact R|apply H].
Qed.

Lemma erel_append : forall a la b lb, ERel a la -> ERel b lb -> ERel (fappend a b) (lappend la lb).
Proof.
  intros a la b lb Ha Hb. destruct Ha as [ts a' s ps Ha]. destruct Hb as [ts' a'' s' ps' Hb].
  cbn. constructor. intro i. rewrite fget_funion.
  assert (E : forall l1 l2, llookup_entry i (l1 ++ l2) =
    match llookup_entry i l1 with Some p => Some p | None => llookup_entry i l2 end).
  { intros l1 l2. induction l1 as [|[j p] r IH]; [reflexivity|]. cbn.
    destruct (fast_item_eqb i j); [reflexivity|exact IH]. }
  rewrite E. cbn [fstore_of lprovs]. destruct (Ha i); [apply Hb|constructor; assumption].
Qed.

Lemma erel_empty : ERel fempty lempty.
Proof. constructor. intro i. rewrite fget_empty. constructor. Qed.

Lemma erel_call : forall fd ld th fc lc, ERel fd ld -> ERel fc lc ->
  ERel (fcall_environment fd th fc) (lcall_environment ld th lc).
Proof.
  intros fd ld th fc lc H Hc. destruct (erel_meanings fd ld H) as [Em _].
  unfold fcall_environment, lcall_environment. rewrite Em.
  destruct Hc. constructor. assumption.
Qed.

Lemma erel_typed_capture : forall fd ld types fc lc, ERel fd ld -> ERel fc lc ->
  ERel (ftyped_capture fd types fc) (ltyped_capture ld types lc).
Proof.
  intros fd ld types fc lc H Hc. destruct (erel_meanings fd ld H) as [Em _].
  unfold ftyped_capture, ltyped_capture. rewrite Em. destruct Hc as [ts a s ps Hc]. cbn.
  constructor. exact Hc.
Qed.

Lemma erel_bind_type : forall fd ld t tau, ERel fd ld -> ERel (fbind_type fd t tau) (lbind_type ld t tau).
Proof.
  intros fd ld t tau H. destruct H as [ts a s ps H]. cbn. constructor. exact H.
Qed.

Lemma prel_primitive_closure : forall fd ld i p, ERel fd ld ->
  PRel (fprimitive_closure fd i p) (lprimitive_closure ld i p).
Proof.
  intros fd ld i p H. destruct (erel_meanings fd ld H) as [Em _].
  unfold fprimitive_closure, lprimitive_closure, lground. rewrite Em. constructor.
Qed.

Lemma erel_install_primitive : forall fd ld i p, ERel fd ld ->
  ERel (finstall_primitive fd i p) (linstall i (lprimitive_closure ld i p) ld).
Proof.
  intros fd ld i p H. pose proof (prel_primitive_closure fd ld i p H) as R.
  destruct H as [ts a s ps H]. constructor. intro j.
  cbn [finstall_primitive fget linstall llookup_entry]. rewrite fget_compact.
  destruct (fast_item_eqb j i); [constructor; exact R|apply H].
Qed.

Lemma erel_select : forall fd ld sm, ERel fd ld ->
  orel ERel (fselect_provisions fd sm) (lselect_provisions ld sm).
Proof.
  intros fd ld sm H. induction sm as [|[req s] r IH].
  - constructor. apply erel_empty.
  - rewrite fselect_provisions_cons. cbn [lselect_provisions].
    destruct (lookup_cases fd ld s H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2.
    + destruct (fselect_provisions fd r), (lselect_provisions ld r); constructor.
    + destruct IH as [|d1 d2 R']; constructor. apply erel_install; assumption.
Qed.

Lemma erel_adapt : forall fd ld a, ERel fd ld ->
  orel ERel (fadapt_provisions fd a) (ladapt_provisions ld a).
Proof. intros fd ld [sm|] H; [apply erel_select; exact H|constructor; exact H]. Qed.

Lemma lshadow_application : forall P fd ld h th fc lc, ERel fd ld -> ERel fc lc ->
  ERel (fapplication_environment P fd h th fc) (lapplication_environment P ld h th lc).
Proof.
  intros P fd ld h th fc lc H Hc. destruct H as [ts a s ps H]. destruct Hc as [ts' a' s' ps' Hc].
  unfold fapplication_environment, lapplication_environment. cbn. constructor. exact Hc.
Qed.

Lemma erel_result : forall outputs fd ld Dr fr lr, ERel fd ld -> ERel fr lr ->
  orel ERel (fresult_environment outputs fd Dr fr) (lresult_environment outputs ld Dr lr).
Proof.
  intros outputs fd ld Dr fr lr H Hr. destruct H as [ts a s ps H].
  unfold fresult_environment, lresult_environment.
  assert (E : lread_result_types (lshadow (FEnv ts a s)) Dr outputs = lread_result_types (LEnv ts a ps) Dr outputs).
  { induction outputs as [|[source destination] rest IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. }
  rewrite E. destruct (lread_result_types (LEnv ts a ps) Dr outputs) as [[w al]|]; [|constructor].
  constructor. destruct Hr as [ts' a' s' ps' Hr]. cbn. constructor. exact Hr.
Qed.

(* ===================== The interpreter =================================== *)

Module FastInterpreter (Import Prims : PRIMITIVES) (Reference : INTERP Prims).
Module L := LazyInterpreter Prims Reference.
Module I := L.I.

Fixpoint feval (fuel : nat) (P : program_view) (d : fenv) (g : locals) (st : mstate)
    (e : expr) {struct fuel} : I.evaluation :=
  match fuel with
  | 0 => I.Evaluation OutOfFuel g st
  | S k =>
      match e with
      | EVar x => match lookup_local x g with
                  | Some w => I.Evaluation (Normal w) g st
                  | None => I.Evaluation (Stuck I.e_var) g st
                  end
      | EUnit => I.Evaluation (Normal VUnit) g st
      | EVal v => match flookup_provision (IVal v) d with
                 | Some (FValue w) => I.Evaluation (Normal w) g st
                 | _ => I.Evaluation (Stuck I.e_val) g st
                 end
      | ETag n th e1 =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (L.lcheck_tag P (lshadow d) n th w) g1 st1
          | r => r
          end
      | ECall f th sm args =>
          match view_function P f with
          | Some body =>
              match feval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  match fprepare_call d th sm with
                  | Some d' => I.ret_call g1 (feval k P d' (fast_parameter_values ws) st1 body)
                  | None => I.Evaluation (Stuck I.e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => I.Evaluation (Stuck I.e_nobody) g st
          end
      | EInvoke f args =>
          match flookup_provision (IFn f) d with
          | Some (FFunction f' df) =>
              match view_function P f' with
              | Some body =>
                  match feval_arguments k P d g st args with
                  | inl (ws, g1, st1) => I.ret_call g1 (feval k P df (fast_parameter_values ws) st1 body)
                  | inr r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (FPrimitive p held) =>
              match feval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := I.run_prim_at P p held ws st1 in I.Evaluation o g1 st2
              | inr r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      | EMethod e0 m prov args =>
          match flookup_provision prov d with
          | Some (FFunction f' d') =>
              match view_function P f' with
              | Some body =>
                  match feval k P d g st e0 with
                  | I.Evaluation (Normal w0) g0 st0 =>
                      match feval_arguments k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          I.ret_call g1 (feval k P d' (fast_parameter_values (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => I.Evaluation (Stuck I.e_nobody) g st
              end
          | Some (FPrimitive p held) =>
              match feval k P d g st e0 with
              | I.Evaluation (Normal w0) g0 st0 =>
                  match feval_arguments k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := I.run_prim_at P p held (w0 :: ws) st1 in I.Evaluation o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => I.Evaluation (Stuck I.e_entry) g st
          end
      | ELet x e1 e2 =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w1) g1 st1 => I.pop1 (feval k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | EBind binding e1 =>
          match feval_bindings k P d g st [binding] with
          | inl (d1, g1, st1) => feval k P d1 g1 st1 e1
          | inr result => result
          end
      | ESeq e1 e2 =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal _) g1 st1 => feval k P d g1 st1 e2
          | r => r
          end
      | EAssign x e1 =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match assign_local x w g1 with
              | Some g2 => I.Evaluation (Normal VUnit) g2 st1
              | None => I.Evaluation (Stuck I.e_assign) g1 st1
              end
          | r => r
          end
      | EIf c e1 e2 =>
          match feval k P d g st c with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true => feval k P d g1 st1 e1
              | Some false => feval k P d g1 st1 e2
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      | EWhile c b =>
          match feval k P d g st c with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.truthy P w with
              | Some true =>
                  match feval k P d g1 st1 b with
                  | I.Evaluation (Normal _) g2 st2 => feval k P d g2 st2 (EWhile c b)
                  | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
                  | r => r
                  end
              | Some false => I.Evaluation (Normal VUnit) g1 st1
              | None => I.Evaluation (Stuck I.e_bool) g1 st1
              end
          | r => r
          end
      | ELoop b =>
          match feval k P d g st b with
          | I.Evaluation (Normal _) g2 st2 => feval k P d g2 st2 (ELoop b)
          | I.Evaluation Breaking g2 st2 => I.Evaluation (Normal VUnit) g2 st2
          | r => r
          end
      | EReturn e1 =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 => I.Evaluation (Returning w) g1 st1
          | r => r
          end
      | EBreak => I.Evaluation Breaking g st
      | ERecord fs =>
          match feval_arguments k P d g st (map snd fs) with
          | inl (ws, g1, st1) => I.Evaluation (Normal (VRecord (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | EField e1 f =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.get_field f w with
              | Some v => I.Evaluation (Normal v) g1 st1
              | None => I.Evaluation (Stuck I.e_field) g1 st1
              end
          | r => r
          end
      | EMatch e0 arms =>
          match feval k P d g st e0 with
          | I.Evaluation (Normal w) g1 st1 =>
              match I.select_arm arms w with
              | Some (bs, eb) =>
                  I.popn (List.length bs) (feval k P d (bs ++ g1) st1 eb)
              | None => I.Evaluation (Stuck I.e_pat) g1 st1
              end
          | r => r
          end
      end
  end
with feval_arguments (fuel : nat) (P : program_view) (d : fenv) (g : locals) (st : mstate)
    (l : list expr) {struct fuel} : (list value * locals * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w) g1 st1 =>
              match feval_arguments k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with feval_bindings (fuel : nat) (P : program_view) (d : fenv) (g : locals) (st : mstate)
    (bs : list binding) {struct fuel} : (fenv * locals * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | BType t tau :: r => feval_bindings k P (fbind_type d t tau) g st r
      | BValue v e1 :: r =>
          match feval k P d g st e1 with
          | I.Evaluation (Normal w1) g1 st1 => feval_bindings k P (finstall (IVal v) (FValue w1) d) g1 st1 r
          | rr => inr rr
          end
      | BFunction f f' types sm :: r =>
          match fselect_provisions d sm with
          | Some dc => feval_bindings k P (finstall (IFn f) (FFunction f' (ftyped_capture d types dc)) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BMethod tauk m th f' types sm :: r =>
          match fselect_provisions d sm with
          | Some dc => feval_bindings k P (finstall (IMth tauk m th) (FFunction f' (ftyped_capture d types dc)) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BFunctor u h types sm smc :: r =>
          match fselect_provisions d smc with
          | Some dc => feval_bindings k P (finstall (IFun u) (FFunctor h (Some sm) (ftyped_capture d types dc) None) d) g st r
          | None => inr (I.Evaluation (Stuck I.e_compose) g st)
          end
      | BCopyFunctor u u' sm rm :: r =>
          match flookup_provision (IFun u') d with
          | Some (FFunctor h a dc p) =>
              feval_bindings k P (finstall (IFun u) (FFunctor h (compose_adaptation a sm) dc (compose_positions p rm)) d) g st r
          | _ => inr (I.Evaluation (Stuck I.e_fun) g st)
          end
      | BApply u th outputs sm rs :: r =>
          match fapply_functor k P d u th outputs sm rs st with
          | inl (dr, st1) => feval_bindings k P (fappend dr d) g st1 r
          | inr rr => inr (I.with_g g rr)
          end
      | BPrimitive i p :: r => feval_bindings k P (finstall_primitive d i p) g st r
      end
  end
with fapply_functor (fuel : nat) (P : program_view) (d : fenv) (u : usym) (th : subst)
    (outputs : list (tsym * tsym)) (sm : smap) (rs : list item) (st : mstate) {struct fuel}
    : (fenv * mstate) + I.evaluation :=
  match fuel with
  | 0 => inr (I.Evaluation OutOfFuel [] st)
  | S k =>
      match flookup_provision (IFun u) d with
      | Some (FFunctor h a dc p) =>
          match view_functor P h with
          | Some code =>
              let Dr := fc_outputs code in
              let bs := fc_bindings code in
              match fselect_provisions d sm with
              | Some d1 =>
                  match fadapt_provisions d1 a with
                  | Some d2 =>
                      match feval_bindings k P (fapplication_environment P d h th (fappend d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match fselect_provisions d3 (result_selection rs p (fc_sources code)) with
                          | Some dr => match fresult_environment outputs d3 Dr dr with
                              | Some result => inl (result, st3)
                              | None => inr (I.Evaluation (Stuck I.e_compose) [] st3) end
                          | None => inr (I.Evaluation (Stuck I.e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (I.Evaluation (Stuck I.e_chain) [] st)
                  end
              | None => inr (I.Evaluation (Stuck I.e_compose) [] st)
              end
          | None => inr (I.Evaluation (Stuck I.e_nobody) [] st)
          end
      | _ => inr (I.Evaluation (Stuck I.e_fun) [] st)
      end
  end.

Definition frun_main (fuel : nat) (P : program_view) (main : fsym) (d0 : fenv)
    (st0 : mstate) : I.evaluation :=
  match view_function P main with
  | Some body => I.ret_call [] (feval fuel P d0 [] st0 body)
  | None => I.Evaluation (Stuck I.e_nobody) [] st0
  end.

(* ===================== Agreement ========================================= *)

Definition brel (r1 : (fenv * locals * mstate) + I.evaluation) (r2 : (lenv * locals * mstate) + I.evaluation) : Prop :=
  match r1, r2 with
  | inl (fd, g, s), inl (ld, g', s') => ERel fd ld /\ g = g' /\ s = s'
  | inr e, inr e' => e = e'
  | _, _ => False
  end.
Definition arel (r1 : (fenv * mstate) + I.evaluation) (r2 : (lenv * mstate) + I.evaluation) : Prop :=
  match r1, r2 with
  | inl (fd, s), inl (ld, s') => ERel fd ld /\ s = s'
  | inr e, inr e' => e = e'
  | _, _ => False
  end.

Lemma check_tag_rel : forall P fd ld n th w, ERel fd ld ->
  L.lcheck_tag P (lshadow fd) n th w = L.lcheck_tag P ld n th w.
Proof. intros P fd ld n th w H. destruct H. reflexivity. Qed.

Theorem feval_agrees : forall k,
  (forall P fd ld g st e, ERel fd ld -> feval k P fd g st e = L.leval k P ld g st e) /\
  (forall P fd ld g st l, ERel fd ld -> feval_arguments k P fd g st l = L.leval_arguments k P ld g st l) /\
  (forall P fd ld g st bs, ERel fd ld -> brel (feval_bindings k P fd g st bs) (L.leval_bindings k P ld g st bs)) /\
  (forall P fd ld u th outputs sm rs st, ERel fd ld ->
     arel (fapply_functor k P fd u th outputs sm rs st) (L.lapply_functor k P ld u th outputs sm rs st)).
Proof.
  induction k as [|k [IHe [IHa [IHb IHf]]]].
  - repeat split; intros; cbn; reflexivity.
  - assert (Eargs : forall P fd ld g st l, ERel fd ld ->
      feval_arguments k P fd g st l = L.leval_arguments k P ld g st l) by exact IHa.
    (* Bindings and applications, as case splits on their relation. *)
    assert (Bcases : forall P fd ld g st bs, ERel fd ld ->
      (exists e, feval_bindings k P fd g st bs = inr e /\ L.leval_bindings k P ld g st bs = inr e) \/
      (exists fd' ld' g' s', feval_bindings k P fd g st bs = inl (fd', g', s') /\
         L.leval_bindings k P ld g st bs = inl (ld', g', s') /\ ERel fd' ld')).
    { intros P fd ld g st bs H. pose proof (IHb P fd ld g st bs H) as R.
      destruct (feval_bindings k P fd g st bs) as [[[fd' g'] s']|e],
               (L.leval_bindings k P ld g st bs) as [[[ld' g''] s'']|e']; cbn in R; try contradiction.
      - destruct R as (R & <- & <-). right. exists fd', ld', g', s'. auto.
      - subst e'. left. exists e. auto. }
    assert (Acases : forall P fd ld u th outputs sm rs st, ERel fd ld ->
      (exists e, fapply_functor k P fd u th outputs sm rs st = inr e /\ L.lapply_functor k P ld u th outputs sm rs st = inr e) \/
      (exists fd' ld' s', fapply_functor k P fd u th outputs sm rs st = inl (fd', s') /\
         L.lapply_functor k P ld u th outputs sm rs st = inl (ld', s') /\ ERel fd' ld')).
    { intros P fd ld u th outputs sm rs st H. pose proof (IHf P fd ld u th outputs sm rs st H) as R.
      destruct (fapply_functor k P fd u th outputs sm rs st) as [[fd' s']|e],
               (L.lapply_functor k P ld u th outputs sm rs st) as [[ld' s'']|e']; cbn in R; try contradiction.
      - destruct R as (R & <-). right. exists fd', ld', s'. auto.
      - subst e'. left. exists e. auto. }
    repeat split.
    + intros P fd ld g st e H. destruct e; cbn [feval L.leval].
      * reflexivity.
      * reflexivity.
      * destruct (lookup_cases fd ld (IVal v) H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2;
          [reflexivity|destruct R; reflexivity].
      * rewrite (IHe P fd ld g st e H). destruct (L.leval k P ld g st e) as [[] ? ?]; try reflexivity.
        rewrite (check_tag_rel P fd ld n s v H). reflexivity.
      * destruct (view_function P f); [|reflexivity]. rewrite (IHa P fd ld g st l H).
        destruct (L.leval_arguments k P ld g st l) as [[[ws g1] st1]|r]; [|reflexivity].
        rewrite fprepare_call_spec.
        destruct (erel_select fd ld s0 H) as [|d1 d2 R]; [reflexivity|].
        cbn [option_map]. rewrite fast_parameter_values_spec, (IHe P _ _ _ _ _ (erel_call fd ld s d1 d2 H R)).
        reflexivity.
      * destruct (lookup_cases fd ld (IFn f) H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2; [reflexivity|].
        destruct R as [w|f' fd' ld' R|u a fd' ld' q R|p o]; try reflexivity.
        -- destruct (view_function P f'); [|reflexivity]. rewrite (IHa P fd ld g st l H).
           destruct (L.leval_arguments k P ld g st l) as [[[ws g1] st1]|r]; [|reflexivity].
           rewrite fast_parameter_values_spec, (IHe P _ _ _ _ _ R). reflexivity.
        -- rewrite (IHa P fd ld g st l H). reflexivity.
      * destruct (lookup_cases fd ld i H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2; [reflexivity|].
        destruct R as [w|f' fd' ld' R|u a fd' ld' q R|p o]; try reflexivity.
        -- destruct (view_function P f'); [|reflexivity]. rewrite (IHe P fd ld g st e H).
           destruct (L.leval k P ld g st e) as [[] g0 st0]; try reflexivity.
           rewrite (IHa P fd ld g0 st0 l H).
           destruct (L.leval_arguments k P ld g0 st0 l) as [[[ws g1] st1]|r]; [|reflexivity].
           rewrite fast_parameter_values_spec, (IHe P _ _ _ _ _ R). reflexivity.
        -- rewrite (IHe P fd ld g st e H).
           destruct (L.leval k P ld g st e) as [[] g0 st0]; try reflexivity.
           rewrite (IHa P fd ld g0 st0 l H). reflexivity.
      * rewrite (IHe P fd ld g st e1 H). destruct (L.leval k P ld g st e1) as [[] g1 st1]; try reflexivity.
        rewrite (IHe P fd ld _ _ e2 H). reflexivity.
      * destruct (Bcases P fd ld g st [b] H) as [(e' & E1 & E2)|(fd' & ld' & g' & s' & E1 & E2 & R)];
          rewrite E1, E2; [reflexivity|]. apply (IHe P _ _ _ _ _ R).
      * rewrite (IHe P fd ld g st e1 H). destruct (L.leval k P ld g st e1) as [[] g1 st1]; try reflexivity.
        apply (IHe P _ _ _ _ _ H).
      * rewrite (IHe P fd ld g st e H). reflexivity.
      * rewrite (IHe P fd ld g st e1 H). destruct (L.leval k P ld g st e1) as [[] g1 st1]; try reflexivity.
        destruct (I.truthy P v) as [[]|]; try reflexivity; apply (IHe P _ _ _ _ _ H).
      * rewrite (IHe P fd ld g st e1 H). destruct (L.leval k P ld g st e1) as [[] g1 st1]; try reflexivity.
        destruct (I.truthy P v) as [[]|]; try reflexivity.
        rewrite (IHe P fd ld g1 st1 e2 H). destruct (L.leval k P ld g1 st1 e2) as [[] g2 st2]; try reflexivity.
        apply (IHe P _ _ _ _ _ H).
      * rewrite (IHe P fd ld g st e H). destruct (L.leval k P ld g st e) as [[] g2 st2]; try reflexivity.
        apply (IHe P _ _ _ _ _ H).
      * rewrite (IHe P fd ld g st e H). reflexivity.
      * reflexivity.
      * rewrite (IHa P fd ld g st _ H). reflexivity.
      * rewrite (IHe P fd ld g st e H). reflexivity.
      * rewrite (IHe P fd ld g st e H). destruct (L.leval k P ld g st e) as [[] g1 st1]; try reflexivity.
        destruct (I.select_arm l v) as [[bs eb]|]; [|reflexivity]. rewrite (IHe P _ _ _ _ _ H). reflexivity.
    + intros P fd ld g st l H. destruct l as [|e1 r]; cbn [feval_arguments L.leval_arguments]; [reflexivity|].
      rewrite (IHe P fd ld g st e1 H). destruct (L.leval k P ld g st e1) as [[] g1 st1]; try reflexivity.
      rewrite (IHa P fd ld g1 st1 r H). reflexivity.
    + intros P fd ld g st bs H. destruct bs as [|b bs]; cbn [feval_bindings L.leval_bindings].
      { cbn. auto. }
      destruct b.
      * apply IHb. apply erel_bind_type. exact H.
      * rewrite (IHe P fd ld g st e H). destruct (L.leval k P ld g st e) as [[] g1 st1]; cbn; try reflexivity.
        apply IHb. apply erel_install; [exact H|constructor].
      * destruct (erel_select fd ld s H) as [|d1 d2 R]; [cbn; reflexivity|].
        apply IHb. apply erel_install; [exact H|]. constructor. apply erel_typed_capture; assumption.
      * destruct (erel_select fd ld s0 H) as [|d1 d2 R]; [cbn; reflexivity|].
        apply IHb. apply erel_install; [exact H|]. constructor. apply erel_typed_capture; assumption.
      * destruct (erel_select fd ld s0 H) as [|d1 d2 R]; [cbn; reflexivity|].
        apply IHb. apply erel_install; [exact H|]. constructor. apply erel_typed_capture; assumption.
      * destruct (lookup_cases fd ld (IFun u0) H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2; [cbn; reflexivity|].
        destruct R as [w|f' fd' ld' R|u' a fd' ld' q R|p o]; try (cbn; reflexivity).
        apply IHb. apply erel_install; [exact H|]. constructor. exact R.
      * destruct (Acases P fd ld u s l s0 l0 st H) as [(e' & E1 & E2)|(fd' & ld' & s' & E1 & E2 & R)];
          rewrite E1, E2; [cbn; reflexivity|].
        apply IHb. apply erel_append; assumption.
      * apply IHb. apply erel_install_primitive. exact H.
    + intros P fd ld u th outputs sm rs st H. cbn [fapply_functor L.lapply_functor].
      destruct (lookup_cases fd ld (IFun u) H) as [[E1 E2]|(x & y & E1 & E2 & R)]; rewrite E1, E2; [cbn; reflexivity|].
      destruct R as [w|f' fd' ld' R|h a fc lc q R|p o]; try (cbn; reflexivity).
      destruct (view_functor P h) as [code|]; [|cbn; reflexivity]. cbv zeta.
      destruct (erel_select fd ld sm H) as [|d1 l1 R1]; [cbn; reflexivity|].
      destruct (erel_adapt d1 l1 a R1) as [|d2 l2 R2]; [cbn; reflexivity|].
      destruct (Bcases P _ _ [] st (fc_bindings code)
          (lshadow_application P fd ld h th _ _ H (erel_append _ _ _ _ R2 R)))
        as [(e' & F1 & F2)|(fd3 & ld3 & g3 & s3 & F1 & F2 & R3)]; rewrite F1, F2; [cbn; reflexivity|].
      destruct (erel_select fd3 ld3 (result_selection rs q (fc_sources code)) R3) as [|dr lr Rr]; [cbn; reflexivity|].
      destruct (erel_result outputs fd3 ld3 (fc_outputs code) dr lr R3 Rr) as [|res lres Rres]; cbn; auto.
Qed.

Theorem frun_main_agrees : forall fuel P main d0 st0,
  frun_main fuel P main (fast_env (lift_env d0)) st0 = I.run_main fuel P main d0 st0.
Proof.
  intros. rewrite <- L.lrun_main_agrees. unfold frun_main, L.lrun_main.
  destruct (view_function P main); [|reflexivity].
  rewrite (proj1 (feval_agrees fuel) P _ _ [] st0 e (fast_env_rel (lift_env d0))). reflexivity.
Qed.

End FastInterpreter.
