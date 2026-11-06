(* ------------------------------------------------------------------------ *)
(* FastX.v — MossX's interpreter with map-keyed runtime environments        *)
(* (milestone 3, speed step 1).                                             *)
(*                                                                           *)
(* Audit.v's `XSem.xinterp` keeps the runtime environment (items to         *)
(* entries) as an association list, looked up with `item_eqb`.  Here the    *)
(* same interpreter, clause for clause, keeps it as five binary tries keyed *)
(* by the item's symbol (one per item kind; a method item's trie holds a    *)
(* short bucket of (receiver type, θ) pairs per method symbol).  Nothing    *)
(* else changes: locals, the machine, the program, the results.            *)
(*                                                                           *)
(* Proved, against MossX's instance XS of Audit's XSem:                     *)
(*  - fxinterp_agrees: on related environments, `fxinterp` returns exactly  *)
(*    what `XS.xinterp` returns (the very same `xres`), for every fuel;     *)
(*  - frun_main_eq: `frun_main` = `XS.run_main`, on every program,          *)
(*    main, initial environment, machine and fuel.                          *)
(* So the oracle extracted from `frun_main` computes Audit's semantics, and *)
(* every theorem about `XS.run_main` (e.g. xinterp_conservative) carries    *)
(* over to it unchanged.                                                    *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit MossX.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 1. Tries keyed by nat ============================= *)

(* key 0 is the root; key k+1 goes left if k is even, right if odd, with
   key k/2 below (NMap.v's layout; the insertion here is bounded by the key
   itself, so the laws hold for every key). *)
Inductive tmap (A : Type) : Type :=
| TE : tmap A
| TN : tmap A -> option A -> tmap A -> tmap A.
Arguments TE {A}.
Arguments TN {A}.

Definition evenb (k : nat) : bool := Nat.eqb (k - 2 * Nat.div2 k) 0.

(* The step from a nonzero key to its child: which side, and the child key.
   Extract2.v realizes both on OCaml ints (k = 0 included). *)
Definition tleft (k : nat) : bool := evenb (k - 1).
Definition tdown (k : nat) : nat := Nat.div2 (k - 1).

Fixpoint tget {A} (m : tmap A) (k : nat) {struct m} : option A :=
  match m with
  | TE => None
  | TN l v r =>
      if Nat.eqb k 0 then v else if tleft k then tget l (tdown k) else tget r (tdown k)
  end.

Fixpoint tset_go {A} (fuel : nat) (m : tmap A) (k : nat) (x : A) : tmap A :=
  match fuel with
  | 0 => m
  | S f =>
      match m with
      | TE =>
          if Nat.eqb k 0 then TN TE (Some x) TE
          else if tleft k then TN (tset_go f TE (tdown k) x) None TE
          else TN TE None (tset_go f TE (tdown k) x)
      | TN l v r =>
          if Nat.eqb k 0 then TN l (Some x) r
          else if tleft k then TN (tset_go f l (tdown k) x) v r
          else TN l v (tset_go f r (tdown k) x)
      end
  end.

Lemma tleft_S : forall k, tleft (S k) = evenb k.
Proof. intro k. unfold tleft. simpl. rewrite Nat.sub_0_r. reflexivity. Qed.
Lemma tdown_S : forall k, tdown (S k) = Nat.div2 k.
Proof. intro k. unfold tdown. simpl. rewrite Nat.sub_0_r. reflexivity. Qed.

Definition tset {A} (m : tmap A) (k : nat) (x : A) : tmap A := tset_go (S k) m k x.

Lemma evenb_split : forall k,
  (evenb k = true /\ k = 2 * Nat.div2 k) \/ (evenb k = false /\ k = S (2 * Nat.div2 k)).
Proof.
  intro k. unfold evenb. pose proof (Nat.div2_odd k) as H.
  destruct (Nat.odd k); simpl in H.
  - right. split; [|lia]. apply Nat.eqb_neq. lia.
  - left. split; [|lia]. apply Nat.eqb_eq. lia.
Qed.

Lemma tget_empty : forall A k, @tget A TE k = None.
Proof. reflexivity. Qed.

Lemma tget_tset_go : forall A f (m : tmap A) k x j, k < f ->
  tget (tset_go f m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof.
  induction f as [|f IH]; intros m k x j Hk; [lia|].
  assert (Hdiv : forall k', S k' < S f -> Nat.div2 k' < f)
    by (intros k' H'; pose proof (Nat.div2_odd k'); destruct (Nat.odd k'); simpl in *; lia).
  assert (Hpar : forall j' k', evenb j' = evenb k' ->
             Nat.eqb (Nat.div2 j') (Nat.div2 k') = Nat.eqb j' k').
  { intros j' k' E. simpl.
    destruct (evenb_split k') as [[Ek Hk']|[Ek Hk']];
    destruct (evenb_split j') as [[Ej Hj']|[Ej Hj']]; rewrite Ek, Ej in E; try discriminate;
    destruct (Nat.eqb (Nat.div2 j') (Nat.div2 k')) eqn:E1, (Nat.eqb j' k') eqn:E2; auto;
    first [apply Nat.eqb_eq in E1 | apply Nat.eqb_neq in E1];
    first [apply Nat.eqb_eq in E2 | apply Nat.eqb_neq in E2]; try lia;
    exfalso; apply E1; subst; reflexivity. }
  assert (Hne : forall j' k', evenb j' <> evenb k' -> Nat.eqb j' k' = false).
  { intros j' k' E. apply Nat.eqb_neq. intros ->. auto. }
  destruct m as [|l v r]; destruct k as [|k']; destruct j as [|j']; simpl;
    rewrite ?tleft_S, ?tdown_S; simpl; rewrite ?tleft_S, ?tdown_S; auto.
  all: try (destruct (evenb j'); reflexivity).
  all: try (destruct (evenb k'); reflexivity).
  all: destruct (evenb k') eqn:Ek; simpl;
       rewrite ?IH by (apply Hdiv; exact Hk); simpl; rewrite ?tleft_S, ?tdown_S;
       destruct (evenb j') eqn:Ej;
       first [rewrite Hpar by congruence; reflexivity
             | rewrite Hne by congruence; reflexivity].
Qed.

Lemma tget_tset : forall A (m : tmap A) k x j,
  tget (tset m k x) j = if Nat.eqb j k then Some x else tget m j.
Proof. intros. apply tget_tset_go. lia. Qed.

(* ===================== 2. Map-keyed environments ========================= *)

Inductive fentry : Type :=
| FVal    : xvalue -> fentry
| FClo    : fsym -> fenv -> fentry
| FFun    : usym -> list smap -> fenv -> fentry
| FPrim   : prim -> fentry
| FFunSym : usym -> fentry
| FType : ty -> fentry
| FPrimAt : prim -> ty -> fentry
(* one trie per item kind: types, vals, fns, functors; methods by symbol,
   each a bucket of (receiver type, θ, entry), the most recent first *)
with fenv : Type :=
| FEnv : tmap fentry -> tmap fentry -> tmap fentry -> tmap fentry
         -> tmap (list (ty * subst * fentry)) -> subst -> fenv.

Definition fempty : fenv := FEnv TE TE TE TE TE [].

Fixpoint bfind (r : ty) (th : subst) (b : list (ty * subst * fentry)) : option fentry :=
  match b with
  | [] => None
  | (r', th', en) :: b' => if ty_eqb r r' && subst_eqb th th' then Some en else bfind r th b'
  end.

Definition flookup (i : item) (d : fenv) : option fentry :=
  match d with
  | FEnv mt mv mf mu mm meta =>
      match i with
      | ITy t => tget mt t
      | IVal v => tget mv v
      | IFn f => tget mf f
      | IFun u => tget mu u
      | IMth r m th => match tget mm m with Some b => bfind r th b | None => None end
      end
  end.

Definition fadd (i : item) (en : fentry) (d : fenv) : fenv :=
  match d with
  | FEnv mt mv mf mu mm meta =>
      match i with
      | ITy t => FEnv (tset mt t en) mv mf mu mm (match en with FType tau => (t,tau)::meta | _ => meta end)
      | IVal v => FEnv mt (tset mv v en) mf mu mm meta
      | IFn f => FEnv mt mv (tset mf f en) mu mm meta
      | IFun u => FEnv mt mv mf (tset mu u en) mm meta
      | IMth r m th =>
          FEnv mt mv mf mu
               (tset mm m ((r, th, en) :: match tget mm m with Some b => b | None => [] end)) meta
      end
  end.

Lemma flookup_fadd : forall i i' en d,
  flookup i (fadd i' en d) = if item_eqb i i' then Some en else flookup i d.
Proof.
  intros i i' en [mt mv mf mu mm meta].
  destruct i, i'; simpl; rewrite ?tget_tset; auto.
  destruct (Nat.eqb m m0) eqn:E; simpl.
  - apply Nat.eqb_eq in E; subst. rewrite andb_true_r. simpl.
    destruct (ty_eqb t t0 && subst_eqb s s0); auto.
    destruct (tget mm m0); auto.
  - rewrite andb_false_r. simpl. auto.
Qed.

Definition fapp (l : list (item * fentry)) (d : fenv) : fenv :=
  fold_right (fun p acc => fadd (fst p) (snd p) acc) d l.

Fixpoint fcompose (d : fenv) (sm : smap) : option (list (item * fentry)) :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match flookup s d, fcompose d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint fchain (d1 : list (item * fentry)) (sms : list smap) : option (list (item * fentry)) :=
  match sms with
  | [] => Some d1
  | sm :: r =>
      match fchain d1 r with
      | Some d' => fcompose (fapp d' fempty) sm
      | None => None
      end
  end.

(* The initial environment, from Audit's list form. *)
Fixpoint conv_entry (en : xdentry) : fentry :=
  match en with
  | XDVal w => FVal w
  | XDClo f d =>
      FClo f ((fix go (l : list (item * xdentry)) : fenv :=
                 match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (go r) end) d)
  | XDFun h sms d =>
      FFun h sms ((fix go (l : list (item * xdentry)) : fenv :=
                     match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (go r) end) d)
  | XDPrim p => FPrim p
  | XDFunSym h => FFunSym h
  | XDType tau => FType tau
  | XDPrimAt p tau => FPrimAt p tau
  end.

Fixpoint conv_env (l : list (item * xdentry)) : fenv :=
  match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (conv_env r) end.

Definition ftypes (d : fenv) : subst := match d with FEnv _ _ _ _ _ meta => meta end.
Definition fground (d : fenv) (tau : ty) : ty := app_subst (ftypes d) tau.

(* ===================== 3. Representation ================================= *)

(* A map-keyed environment represents a list one when every item looks up
   to related entries (or to nothing in both); entries are related
   structurally, captured environments by the same relation. *)
Inductive erel : fentry -> xdentry -> Prop :=
| RVal : forall w, erel (FVal w) (XDVal w)
| RClo : forall f fe d, envrel fe d -> erel (FClo f fe) (XDClo f d)
| RFun : forall h sms fe d, envrel fe d -> erel (FFun h sms fe) (XDFun h sms d)
| RPrim : forall p, erel (FPrim p) (XDPrim p)
| RFunSym : forall h, erel (FFunSym h) (XDFunSym h)
| RType : forall tau, erel (FType tau) (XDType tau)
| RPrimAt : forall p tau, erel (FPrimAt p tau) (XDPrimAt p tau)
with envrel : fenv -> xdenv -> Prop :=
| ERel : forall fe d,
    (forall i, flookup i fe = None -> lookup_xdenv i d = None) ->
    (forall i a, flookup i fe = Some a -> exists b, lookup_xdenv i d = Some b /\ erel a b) ->
    ftypes fe = xdtypes d ->
    envrel fe d.

Definition orel (a : option fentry) (b : option xdentry) : Prop :=
  match a, b with
  | Some x, Some y => erel x y
  | None, None => True
  | _, _ => False
  end.

Lemma envrel_lookup : forall fe d i, envrel fe d -> orel (flookup i fe) (lookup_xdenv i d).
Proof.
  intros fe d i H. inversion H as [? ? Hn Hs Ht]; subst. unfold orel.
  destruct (flookup i fe) as [a|] eqn:E.
  - destruct (Hs i a E) as [b [-> Hb]]. exact Hb.
  - rewrite (Hn i E). exact I.
Qed.

Lemma envrel_intro : forall fe d, ftypes fe = xdtypes d -> (forall i, orel (flookup i fe) (lookup_xdenv i d)) -> envrel fe d.
Proof.
  intros fe d Ht H. constructor.
  - intros i E. specialize (H i). rewrite E in H. unfold orel in H.
    destruct (lookup_xdenv i d); [contradiction|auto].
  - intros i a E. specialize (H i). rewrite E in H. unfold orel in H.
    destruct (lookup_xdenv i d) as [b|]; [eauto|contradiction].
  - exact Ht.
Qed.

Lemma envrel_empty : envrel fempty [].
Proof. apply envrel_intro; [reflexivity|]. intros []; simpl; auto. Qed.

Lemma envrel_add : forall fe d i a b,
  erel a b -> envrel fe d -> envrel (fadd i a fe) ((i, b) :: d).
Proof.
  intros fe d i a b Hab H. apply envrel_intro.
  - inversion H as [? ? Hn Hs Ht]. destruct fe, i; inversion Hab; subst; simpl in *; congruence.
  - intro j.
  rewrite flookup_fadd. simpl. destruct (item_eqb j i); simpl; auto.
  apply envrel_lookup, H.
Qed.

Definition lrel (l : list (item * fentry)) (l' : xdenv) : Prop :=
  Forall2 (fun p q => fst p = fst q /\ erel (snd p) (snd q)) l l'.

Lemma envrel_app : forall l l' fe d, lrel l l' -> envrel fe d -> envrel (fapp l fe) (l' ++ d).
Proof.
  intros l l' fe d H. induction H as [|[i a] [j b] l l' [Hij Hab] _ IH]; simpl in *; auto.
  intro Hd. subst. apply envrel_add; auto.
Qed.

Lemma envrel_of_lrel : forall l l', lrel l l' -> envrel (fapp l fempty) l'.
Proof.
  intros. rewrite <- (app_nil_r l'). apply envrel_app; auto using envrel_empty.
Qed.

Definition crel (a : option (list (item * fentry))) (b : option xdenv) : Prop :=
  match a, b with
  | Some x, Some y => lrel x y
  | None, None => True
  | _, _ => False
  end.

Lemma fcompose_rel : forall fe d sm, envrel fe d -> crel (fcompose fe sm) (xcompose d sm).
Proof.
  intros fe d sm H. induction sm as [|[req s] r IH]; simpl.
  - constructor.
  - pose proof (envrel_lookup s H) as Hl. unfold orel in Hl.
    destruct (flookup s fe), (lookup_xdenv s d); try contradiction;
    destruct (fcompose fe r), (xcompose d r); simpl in *; try contradiction; auto.
    constructor; auto.
Qed.

Lemma fchain_rel : forall sms l l', lrel l l' -> crel (fchain l sms) (xrun_chain l' sms).
Proof.
  induction sms as [|sm r IH]; intros l l' H; simpl; auto.
  specialize (IH l l' H).
  destruct (fchain l r), (xrun_chain l' r); simpl in *; try contradiction; auto.
  apply fcompose_rel, envrel_of_lrel, IH.
Qed.

Lemma conv_rel : forall en, erel (conv_entry en) en.
Proof.
  fix IH 1. intros [w|f d|h sms d|p|h|tau|p tau]; simpl; constructor.
  all: induction d as [|[i e] r IHd]; [apply envrel_empty|].
  all: apply envrel_add; [apply IH|exact IHd].
Qed.

Lemma conv_env_rel : forall d, envrel (conv_env d) d.
Proof.
  induction d as [|[i e] r IH]; simpl; [apply envrel_empty|].
  apply envrel_add; [apply conv_rel|exact IH].
Qed.

(* A separate type stream keeps the semantics of xdtypes even when an
   ill-shaped ITy entry shadows a later XDType in the ordinary lookup map. *)
Definition ftype_pairs (meta : subst) : xdenv :=
  map (fun p => (ITy (fst p), XDType (snd p))) meta.
Definition ftype_env (fe : fenv) : fenv := conv_env (ftype_pairs (ftypes fe)).

Fixpoint toverlay {A} (a b : tmap A) : tmap A :=
  match a, b with
  | TE, _ => b
  | _, TE => a
  | TN al av ar, TN bl bv br => TN (toverlay al bl)
       (match av with Some v => Some v | None => bv end) (toverlay ar br)
  end.
Lemma tget_toverlay : forall A (a b : tmap A) k,
  tget (toverlay a b) k = match tget a k with Some x => Some x | None => tget b k end.
Proof.
  intros A a. induction a as [|al IL av ar IR]; intros b k; [reflexivity|].
  destruct b as [|bl bv br]; cbn [toverlay tget].
  - destruct (Nat.eqb k 0); [destruct av|destruct (tleft k); destruct (tget al (tdown k)); destruct (tget ar (tdown k))]; reflexivity.
  - destruct (Nat.eqb k 0); [destruct av; reflexivity|].
    destruct (tleft k); [apply IL|apply IR].
Qed.
Definition fattach_types (fe dc : fenv) : fenv :=
  match dc, ftype_env fe with
  | FEnv mt mv mf mu mm meta, FEnv type_map _ _ _ _ _ =>
      FEnv (toverlay mt type_map) mv mf mu mm (meta ++ ftypes fe)
  end.
Definition fbind_ty (P : xprog) (fe : fenv) (t : tsym) (tau : ty) : fenv :=
  if xreify P then fadd (ITy t) (FType (fground fe tau)) fe else fe.
Definition finst (fe : fenv) (th : subst) : list (item * fentry) :=
  map (fun p => (ITy (fst p), FType (fground fe (snd p)))) th.
Definition fcall_env (P : xprog) (fe : fenv) (th : subst) (dc : fenv) : fenv :=
  if xreify P then fapp (finst fe th) (fattach_types fe dc) else dc.
Definition fapply_env (P : xprog) (fe : fenv) (h : usym) (th : subst) (dc : fenv) : fenv :=
  fcall_env P fe (filter (fun p => existsb (Nat.eqb (fst p)) (xp_inputs P h)) th) dc.

Definition fresult_types (Dr : tele) (fe : fenv) : list (item * fentry) :=
  map (fun p => (ITy (fst p), FType (snd p)))
    (filter (fun p => existsb (item_eqb (ITy (fst p))) Dr) (ftypes fe)).
Definition fresult_env (P : xprog) (fe : fenv) (Dr : tele) (dr : list (item * fentry)) : list (item * fentry) :=
  if xreify P then dr ++ fresult_types Dr fe else dr.

Definition fprim_entry (fe : fenv) (i : item) (p : prim) : fentry :=
  match p, i with
  | PBox _, IMth tau _ _ | PArrNew _, IMth tau _ _ => FPrimAt p (fground fe tau)
  | _, _ => FPrim p
  end.
Definition fcast (P : xprog) (fe : fenv) (n : nsym) (th : subst) (v : xvalue) : xout :=
  xcast P (ftype_pairs (ftypes fe)) n th v.

Lemma xdtypes_pairs : forall meta, xdtypes (ftype_pairs meta) = meta.
Proof. induction meta as [|[t tau] r IH]; simpl; congruence. Qed.
Lemma xtype_env_pairs : forall d, xtype_env d = ftype_pairs (xdtypes d).
Proof.
  induction d as [|[i en] r IH]; [reflexivity|].
  destruct i; destruct en; cbn [xtype_env xdtypes ftype_pairs]; try exact IH; rewrite IH; reflexivity.
Qed.
Lemma envrel_types : forall fe d, envrel fe d -> ftypes fe = xdtypes d.
Proof. intros fe d H. inversion H; assumption. Qed.
Lemma fground_rel : forall fe d, envrel fe d -> forall tau, fground fe tau = xground d tau.
Proof. intros fe d H tau. unfold fground, xground. rewrite (envrel_types H). reflexivity. Qed.
Lemma ftype_env_rel : forall fe d, envrel fe d -> envrel (ftype_env fe) (xtype_env d).
Proof.
  intros fe d H. unfold ftype_env. rewrite xtype_env_pairs, <- (envrel_types H). apply conv_env_rel.
Qed.
Lemma flookup_type_env_other : forall fe i,
  (match i with ITy _ => False | _ => True end) -> flookup i (ftype_env fe) = None.
Proof.
  intros fe i Hi. unfold ftype_env, ftype_pairs. generalize (ftypes fe). intro meta.
  induction meta as [|[t tau] r IH]; [destruct i; simpl; auto|].
  change (flookup i (fadd (ITy t) (FType tau) (conv_env (ftype_pairs r))) = None).
  rewrite flookup_fadd. destruct i; simpl in *; auto; contradiction.
Qed.
Lemma flookup_attach : forall fe dc i,
  flookup i (fattach_types fe dc) =
    match flookup i dc with Some a => Some a | None => flookup i (ftype_env fe) end.
Proof.
  intros fe [mt mv mf mu mm meta] i.
  pose proof (flookup_type_env_other fe i) as Ot.
  unfold fattach_types. destruct (ftype_env fe) as [type_map tv tf tu tm ts].
  destruct i; cbn [flookup] in *; rewrite ?tget_toverlay; auto;
    rewrite Ot by exact I; repeat match goal with |- context [tget ?m ?k] => destruct (tget m k) | |- context [bfind ?r ?th ?b] => destruct (bfind r th b) end; reflexivity.
Qed.
Lemma lookup_xdenv_app : forall a b i,
  lookup_xdenv i (a ++ b) = match lookup_xdenv i a with Some x => Some x | None => lookup_xdenv i b end.
Proof.
  induction a as [|[j en] a IH]; intros b i; simpl; [reflexivity|].
  destruct (item_eqb i j); [reflexivity|apply IH].
Qed.
Lemma xdtypes_app : forall a b, xdtypes (a ++ b) = xdtypes a ++ xdtypes b.
Proof.
  induction a as [|[i en] a IH]; intros b; simpl; [reflexivity|].
  destruct i; destruct en; cbn [xdtypes]; rewrite IH; reflexivity.
Qed.
Lemma fattach_types_rel : forall fe d fc dc, envrel fe d -> envrel fc dc ->
  envrel (fattach_types fe fc) (xclose d dc).
Proof.
  intros fe d fc dc H Hc. apply envrel_intro.
  - unfold xclose. rewrite xdtypes_app, xtype_env_pairs, xdtypes_pairs.
    pose proof (envrel_types Hc) as Mt.
    unfold fattach_types. destruct fc; destruct (ftype_env fe); simpl.
    cbn [ftypes] in Mt. rewrite Mt, (envrel_types H). reflexivity.
  - intro i. rewrite flookup_attach. unfold xclose. rewrite lookup_xdenv_app.
    pose proof (envrel_lookup i Hc) as Q. unfold orel in Q.
    destruct (flookup i fc), (lookup_xdenv i dc); try contradiction; auto.
    apply envrel_lookup, ftype_env_rel, H.
Qed.
Lemma fbind_ty_rel : forall P fe d t tau, envrel fe d ->
  envrel (fbind_ty P fe t tau) (xbind_ty P d t tau).
Proof.
  intros P fe d t tau H. unfold fbind_ty, xbind_ty. destruct (xreify P); auto.
  rewrite (fground_rel H). apply envrel_add; [constructor|exact H].
Qed.
Lemma finst_rel : forall fe d th, envrel fe d -> lrel (finst fe th) (xinst d th).
Proof.
  intros fe d th H. rewrite xinst_map; unfold finst. induction th as [|[t tau] r IH]; [constructor|].
  constructor; [simpl; split; [reflexivity|rewrite (fground_rel H); constructor]|exact IH].
Qed.
Lemma fcall_env_rel : forall P fe d th fc dc, envrel fe d -> envrel fc dc ->
  envrel (fcall_env P fe th fc) (xcall_env P d th dc).
Proof.
  intros P fe d th fc dc H Hc. unfold fcall_env, xcall_env. destruct (xreify P); auto.
  apply envrel_app; [apply finst_rel; exact H|apply fattach_types_rel; assumption].
Qed.
Lemma fapply_env_rel : forall P fe d h th fc dc, envrel fe d -> envrel fc dc ->
  envrel (fapply_env P fe h th fc) (xapply_env P d h th dc).
Proof. intros; unfold fapply_env, xapply_env; apply fcall_env_rel; assumption. Qed.
Lemma fprim_entry_rel : forall fe d i p, envrel fe d -> erel (fprim_entry fe i p) (xprim_entry d i p).
Proof.
  intros fe d i p H. destruct p; destruct i; cbn [fprim_entry xprim_entry];
    try constructor; rewrite (fground_rel H); constructor.
Qed.
Lemma fcast_rel : forall P fe d n th v, envrel fe d -> fcast P fe n th v = xcast P d n th v.
Proof.
  intros P fe d n th v H. unfold fcast, xcast, xground.
  rewrite xdtypes_pairs, (envrel_types H). reflexivity.
Qed.

Lemma xresult_pairs : forall Dr meta,
  filter (fun p => existsb (item_eqb (fst p)) Dr) (ftype_pairs meta) =
    ftype_pairs (filter (fun p => existsb (item_eqb (ITy (fst p))) Dr) meta).
Proof.
  intros Dr meta. induction meta as [|[t tau] r IH]; [reflexivity|].
  cbn [ftype_pairs xtype_env filter fst snd]. destruct (existsb (item_eqb (ITy t)) Dr) eqn:E; simpl; rewrite IH; rewrite ?E; reflexivity.
Qed.
Lemma fresult_types_rel : forall Dr fe d, envrel fe d -> lrel (fresult_types Dr fe) (xresult_types Dr d).
Proof.
  intros Dr fe d H. unfold xresult_types. rewrite xtype_env_pairs, <- (envrel_types H).
  rewrite xresult_pairs. unfold fresult_types, ftype_pairs.
  generalize (filter (fun p => existsb (item_eqb (ITy (fst p))) Dr) (ftypes fe)). intro meta.
  induction meta as [|[t tau] r IH]; constructor; [simpl; split; [reflexivity|constructor]|exact IH].
Qed.
Lemma lrel_app : forall a b c d, lrel a b -> lrel c d -> lrel (a ++ c) (b ++ d).
Proof. intros a b c d H Hc. induction H; simpl; auto. constructor; auto. Qed.
Lemma fresult_env_rel : forall P Dr fe d dr dr', envrel fe d -> lrel dr dr' ->
  lrel (fresult_env P fe Dr dr) (xresult_env P d Dr dr').
Proof.
  intros P Dr fe d dr dr' H Hd. unfold fresult_env, xresult_env. destruct (xreify P); auto.
  apply lrel_app; auto. apply fresult_types_rel; exact H.
Qed.

(* ===================== 4. The interpreter ================================ *)

(* Over MossX's instance `XS` of Audit's `XSem` (the concrete primitive
   semantics), so that the agreement is with the very interpreter MossX.v's
   theorems (xinterp_conservative, ...) are about.  Nothing below depends on
   the primitives: `run_prim` is passed through untouched. *)

Fixpoint fxinterp (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (e : xexpr) {struct fuel} : xres :=
  match fuel with
  | 0 => XR XFuel g st
  | S k =>
      match e with
      | XVar x => match lookup_xvenv x g with
                  | Some w => XR (XOk w) g st
                  | None => XR (XErr e_var) g st
                  end
      | XUnit => XR (XOk XVUnit) g st
      | XVl v => match flookup (IVal v) d with
                 | Some (FVal w) => XR (XOk w) g st
                 | _ => XR (XErr e_val) g st
                 end
      | XTag n th e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (fcast P d n th w) g1 st1
          | r => r
          end
      | XCallD f th sm args =>
          match xp_fn P f with
          | Some body =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  match fcompose d sm with
                  | Some d' => ret_call g1 (fxinterp k P (fcall_env P d th (fapp d' fempty)) (mk_xvenv ws) st1 body)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      | XCallA f args =>
          match flookup (IFn f) d with
          | Some (FClo f' df) =>
              match xp_fn P f' with
              | Some body =>
                  match fxinterp_list k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (fxinterp k P df (mk_xvenv ws) st1 body)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (FPrimAt p tau) =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMeth e0 m prov args =>
          match flookup prov d with
          | Some (FClo f' d') =>
              match xp_fn P f' with
              | Some body =>
                  match fxinterp k P d g st e0 with
                  | XR (XOk w0) g0 st0 =>
                      match fxinterp_list k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (fxinterp k P d' (mk_xvenv (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (FPrim p) =>
              match fxinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match fxinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (FPrimAt p tau) =>
              match fxinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match fxinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p (Some tau) (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMatch e0 arms =>
          match fxinterp k P d g st e0 with
          | XR (XOk (XVTag n w')) g1 st1 =>
              match find_xarm n arms with
              | Some (x, eb) => pop1 (fxinterp k P d ((x, w') :: g1) st1 eb)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      | XLet x e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => pop1 (fxinterp k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | XBindTy t tau e1 => fxinterp k P (fbind_ty P d t tau) g st e1
      | XBindVal v e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp k P (fadd (IVal v) (FVal w1) d) g1 st1 e2
          | r => r
          end
      | XBindFn f f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IFn f) (FClo f' (fattach_types d (fapp dc fempty))) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindMth tauk m th f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IMth tauk m th) (FClo f' (fattach_types d (fapp dc fempty))) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFun u h sm smc e1 =>
          match fcompose d smc with
          | Some dc => fxinterp k P (fadd (IFun u) (FFun h [sm] (fattach_types d (fapp dc fempty))) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFunA u u' sm e1 =>
          match flookup (IFun u') d with
          | Some (FFun h sms dc) =>
              fxinterp k P (fadd (IFun u) (FFun h (sms ++ [sm]) dc) d) g st e1
          | _ => XR (XErr e_fun) g st
          end
      | XApply u th sm e1 =>
          match fxinterp_apply k P d u th sm st with
          | inl (dr, st1) => fxinterp k P (fapp dr d) g st1 e1
          | inr r => with_g g r
          end
      | XApplyR u inputs outputs e1 =>
          match fxinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => fxinterp k P (fapp dr d) g st1 e1
          | inr rr => with_g g rr
          end
      | XSeq e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk _) g1 st1 => fxinterp k P d g1 st1 e2
          | r => r
          end
      | XAssign x e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match assign_xvenv x w g1 with
              | Some g2 => XR (XOk XVUnit) g2 st1
              | None => XR (XErr e_assign) g1 st1
              end
          | r => r
          end
      | XIf c e1 e2 =>
          match fxinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true => fxinterp k P d g1 st1 e1
              | Some false => fxinterp k P d g1 st1 e2
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XWhile c b =>
          match fxinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match fxinterp k P d g1 st1 b with
                  | XR (XOk _) g2 st2 => fxinterp k P d g2 st2 (XWhile c b)
                  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                  | r => r
                  end
              | Some false => XR (XOk XVUnit) g1 st1
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XLoop b =>
          match fxinterp k P d g st b with
          | XR (XOk _) g2 st2 => fxinterp k P d g2 st2 (XLoop b)
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | XReturn e1 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
          | r => r
          end
      | XBreak => XR XBrk g st
      | XRec fs =>
          match fxinterp_list k P d g st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | XField e1 f =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match get_field f w with
              | Some v => XR (XOk v) g1 st1
              | None => XR (XErr e_field) g1 st1
              end
          | r => r
          end
      | XMatchP e0 arms =>
          match fxinterp k P d g st e0 with
          | XR (XOk w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (fxinterp k P d (bs ++ g1) st1 eb)
              | None => XR (XErr e_pat) g1 st1
              end
          | r => r
          end
      | XBindFunSym u h e1 => fxinterp k P (fadd (IFun u) (FFunSym h) d) g st e1
      | XBindFunCopy u u' e1 =>
          match flookup (IFun u') d with
          | Some en => fxinterp k P (fadd (IFun u) en d) g st e1
          | None => XR (XErr e_fun) g st
          end
      | XApplyBr u brs e1 =>
          match fxinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => fxinterp k P (fapp dr d) g1 st1 e1
          | inr r => r
          end
      | XBindPrim i p e1 => fxinterp k P (fadd i (fprim_entry d i p) d) g st e1
      | XConst _ initializer => match k with O => XR XFuel g st
          | S _ => fxinterp k P d g st initializer end
      end
  end
with fxinterp_list (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (l : list xexpr) {struct fuel} : (list xvalue * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match fxinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match fxinterp_list k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with fxinterp_binds (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (bs : list xbind) {struct fuel} : (fenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | XBTy t tau :: r => fxinterp_binds k P (fbind_ty P d t tau) g st r
      | XBVal v e1 :: r =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp_binds k P (fadd (IVal v) (FVal w1) d) g1 st1 r
          | rr => inr rr
          end
      | XBFn f f' sm :: r =>
          match fcompose d sm with
          | Some dc => fxinterp_binds k P (fadd (IFn f) (FClo f' (fattach_types d (fapp dc fempty))) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBMth tauk m th f' sm :: r =>
          match fcompose d sm with
          | Some dc =>
              fxinterp_binds k P (fadd (IMth tauk m th) (FClo f' (fattach_types d (fapp dc fempty))) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFun u h sm smc :: r =>
          match fcompose d smc with
          | Some dc => fxinterp_binds k P (fadd (IFun u) (FFun h [sm] (fattach_types d (fapp dc fempty))) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFunA u u' sm :: r =>
          match flookup (IFun u') d with
          | Some (FFun h sms dc) =>
              fxinterp_binds k P (fadd (IFun u) (FFun h (sms ++ [sm]) dc) d) g st r
          | _ => inr (XR (XErr e_fun) g st)
          end
      | XBApply u th sm :: r =>
          match fxinterp_apply k P d u th sm st with
          | inl (dr, st1) => fxinterp_binds k P (fapp dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBApplyR u inputs outputs :: r =>
          match fxinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => fxinterp_binds k P (fapp dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBFunSym u h :: r => fxinterp_binds k P (fadd (IFun u) (FFunSym h) d) g st r
      | XBFunCopy u u' :: r =>
          match flookup (IFun u') d with
          | Some en => fxinterp_binds k P (fadd (IFun u) en d) g st r
          | None => inr (XR (XErr e_fun) g st)
          end
      | XBApplyBr u brs :: r =>
          match fxinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => fxinterp_binds k P (fapp dr d) g1 st1 r
          | inr rr => inr rr
          end
      | XBPrim i p :: r => fxinterp_binds k P (fadd i (fprim_entry d i p) d) g st r
      end
  end
with fxinterp_apply (fuel : nat) (P : xprog) (d : fenv) (u : usym) (th : subst)
    (sm : smap) (st : mstate) {struct fuel} : (list (item * fentry) * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFun h sms dc) =>
          match xp_fun P h with
          | Some (Dr, bs) =>
              match fcompose d sm with
              | Some d1 =>
                  match fchain d1 sms with
                  | Some d2 =>
                      match fxinterp_binds k P (fapply_env P d h th (fapp d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match fcompose d3 (inst_smap th Dr) with
                          | Some dr => inl (fresult_env P d3 Dr dr, st3)
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end
with fxinterp_apply_r (fuel : nat) (P : xprog) (d : fenv) (u : usym) (inputs outputs : smap) (st : mstate) {struct fuel} : (list (item * fentry) * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFun h sms dc) =>
          match xp_fun P h with
          | Some (_, bs) =>
              match fcompose d inputs with
              | Some d1 =>
                  match fchain d1 sms with
                  | Some d2 =>
                      match fxinterp_binds k P (fapp d2 dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match fcompose d3 (xp_exports P h) with
                          | Some named =>
                              match fcompose (fapp named fempty) outputs with
                              | Some dr => inl (dr, st3)
                              | None => inr (XR (XErr e_compose) [] st3)
                              end
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end
with fxinterp_branch (fuel : nat) (P : xprog) (d : fenv) (g : xvenv) (st : mstate)
    (u : usym) (brs : list (usym * list xbind * smap)) {struct fuel}
    : (list (item * fentry) * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match flookup (IFun u) d with
      | Some (FFunSym h) =>
          match find_branch h brs with
          | Some (bs, pairs) =>
              match fxinterp_binds k P d g st bs with
              | inl (d3, g3, st3) =>
                  match fcompose d3 pairs with
                  | Some dr => inl (dr, g3, st3)
                  | None => inr (XR (XErr e_compose) g3 st3)
                  end
              | inr rr => inr rr
              end
          | None => inr (XR (XErr e_branch) g st)
          end
      | _ => inr (XR (XErr e_fun) g st)
      end
  end.

Definition frun_main (fuel : nat) (P : xprog) (main : fsym) (d0 : xdenv)
    (st0 : mstate) : xres :=
  match xp_fn P main with
  | Some body => ret_call [] (fxinterp fuel P (conv_env d0) [] st0 body)
  | None => XR (XErr e_nobody) [] st0
  end.

(* ===================== 5. Agreement ====================================== *)

Definition brel (a : (fenv * xvenv * mstate) + xres) (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (fe, g, st), inl (d, g', st') => envrel fe d /\ g = g' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition arel (a : (list (item * fentry) * mstate) + xres) (b : (xdenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, st), inl (l', st') => lrel l l' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition rrel (a : (list (item * fentry) * xvenv * mstate) + xres)
    (b : (xdenv * xvenv * mstate) + xres) : Prop :=
  match a, b with
  | inl (l, g, st), inl (l', g', st') => lrel l l' /\ g = g' /\ st = st'
  | inr r, inr r' => r = r'
  | _, _ => False
  end.

Definition AGREE (k : nat) : Prop :=
  (forall P fe d g st e, envrel fe d -> fxinterp k P fe g st e = xinterp k P d g st e)
  /\ (forall P fe d g st l, envrel fe d -> fxinterp_list k P fe g st l = xinterp_list k P d g st l)
  /\ (forall P fe d g st bs, envrel fe d ->
        brel (fxinterp_binds k P fe g st bs) (xinterp_binds k P d g st bs))
  /\ (forall P fe d u th sm st, envrel fe d ->
        arel (fxinterp_apply k P fe u th sm st) (xinterp_apply k P d u th sm st))
  /\ (forall P fe d g st u brs, envrel fe d ->
        rrel (fxinterp_branch k P fe g st u brs) (xinterp_branch k P d g st u brs))
  /\ (forall P fe d u inputs outputs st, envrel fe d ->
        arel (fxinterp_apply_r k P fe u inputs outputs st) (xinterp_apply_r k P d u inputs outputs st)).

(* Case on a lookup in related environments. *)
Ltac lk H i :=
  let Hl := fresh "Hl" in
  pose proof (envrel_lookup i H) as Hl; unfold orel in Hl;
  destruct (flookup i _), (lookup_xdenv i _); try contradiction;
  try (inversion Hl; subst; clear Hl).

Ltac cp H sm :=
  let Hc := fresh "Hc" in
  pose proof (fcompose_rel sm H) as Hc; unfold crel in Hc;
  destruct (fcompose _ sm), (xcompose _ sm); try contradiction.

Ltac dfn := match goal with |- context [xp_fn ?P ?x] => destruct (xp_fn P x) end.

Lemma agree_step : forall k, AGREE k -> AGREE (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa [IHr IHar]]]]].
  split; [|split; [|split; [|split; [|split]]]].
  - intros P fe d g st e H. destruct e; simpl.
    all: try (erewrite ?(IHe P fe d) by exact H; reflexivity).
    + (* XVl *) lk H (IVal v); auto.
    + (* XTag *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[v| | | | | | |] gg ss]; auto.
      rewrite (fcast_rel P n s v H). reflexivity.
    + (* XCallD *)
      destruct (xp_fn P f); auto. rewrite (IHl P fe d) by exact H.
      destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
      cp H s0; auto. f_equal. apply IHe, fcall_env_rel; [exact H|apply envrel_of_lrel; auto].
    + (* XCallA *)
      lk H (IFn f); auto.
      * dfn; auto. rewrite (IHl P fe d) by exact H.
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
        f_equal. apply IHe; auto.
      * rewrite (IHl P fe d) by exact H. reflexivity.
      * rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMeth *)
      lk H i; auto.
      * dfn; auto. rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H.
        destruct (xinterp_list k P d g0 st0 l) as [[[ws g1] st1]|]; auto.
        f_equal. apply IHe; auto.
      * rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H. reflexivity.
      * rewrite (IHe P fe d) by exact H.
        destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
        rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMatch *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct w0; auto. destruct (find_xarm n l) as [[x eb]|]; auto.
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XLet *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XBindTy *) apply IHe, fbind_ty_rel, H.
    + (* XBindVal *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      apply IHe, envrel_add; auto. constructor.
    + (* XBindFn *)
      cp H s; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + (* XBindMth *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + (* XBindFun *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + (* XBindFunA *)
      lk H (IFun u0); auto. apply IHe, envrel_add; auto. constructor; auto.
    + (* XApply *)
      pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHe, envrel_app; auto.
      * subst; reflexivity.
    + (* XApplyR *)
      pose proof (IHar P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply_r k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply_r k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHe, envrel_app; auto.
      * subst; reflexivity.
    + (* XSeq *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    + (* XIf *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (truthy P w0) as [[]|]; auto; try (apply IHe; assumption).
    + (* XWhile *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (truthy P w0) as [[]|]; auto; try (apply IHe; assumption).
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g0 st0 e2) as [[w1| | | | | | |] g1 st1]; auto; try (apply IHe; assumption).
    + (* XLoop *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    + (* XRec *)
      rewrite (IHl P fe d) by exact H. reflexivity.
    + (* XMatchP *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct (select_arm l w0) as [[bs eb]|]; auto.
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XBindFunSym *) apply IHe, envrel_add; auto. constructor.
    + (* XBindFunCopy *)
      pose proof (envrel_lookup (IFun u0) H) as Hl; unfold orel in Hl.
      destruct (flookup (IFun u0) fe), (lookup_xdenv (IFun u0) d); try contradiction; auto.
      apply IHe, envrel_add; auto.
    + (* XApplyBr *)
      pose proof (IHr P fe d g st u l H) as Hr.
      destruct (fxinterp_branch k P fe g st u l) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u l) as [[[dr' g1'] st1']|r1']; simpl in Hr;
        try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply IHe, envrel_app; auto.
      * subst; reflexivity.
    + (* XBindPrim *) apply IHe, envrel_add; [apply fprim_entry_rel|]; exact H.
  - intros P fe d g st l H. destruct l as [|e r]; simpl; auto.
    rewrite (IHe P fe d) by exact H.
    destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    rewrite (IHl P fe d) by exact H. reflexivity.
  - intros P fe d g st bs H. destruct bs as [|b r]; simpl; [auto|].
    destruct b; simpl.
    + apply IHb, fbind_ty_rel, H.
    + rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st x) as [[w0| | | | | | |] g0 st0]; simpl; auto.
      apply IHb, envrel_add; auto. constructor.
    + cp H s; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply fattach_types_rel; [exact H|apply envrel_of_lrel; auto].
    + lk H (IFun u0); simpl; auto. apply IHb, envrel_add; auto. constructor; auto.
    + pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHb, envrel_app; auto.
      * subst; reflexivity.
    + pose proof (IHar P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply_r k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply_r k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
        try contradiction.
      * destruct Ha as [Hdr ->]. apply IHb, envrel_app; auto.
      * subst; reflexivity.
    + apply IHb, envrel_add; auto. constructor.
    + pose proof (envrel_lookup (IFun u0) H) as Hl; unfold orel in Hl.
      destruct (flookup (IFun u0) fe), (lookup_xdenv (IFun u0) d); try contradiction; simpl; auto.
      apply IHb, envrel_add; auto.
    + pose proof (IHr P fe d g st u l H) as Hr.
      destruct (fxinterp_branch k P fe g st u l) as [[[dr g1] st1]|r1],
               (xinterp_branch k P d g st u l) as [[[dr' g1'] st1']|r1']; simpl in Hr;
        try contradiction.
      * destruct Hr as [Hdr [-> ->]]. apply IHb, envrel_app; auto.
      * subst; reflexivity.
    + apply IHb, envrel_add; [apply fprim_entry_rel|]; exact H.
  - intros P fe d u th sm st H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context [xp_fun ?P ?x] => destruct (xp_fun P x) as [[Dr bs]|] end;
      simpl; auto.
    cp H sm; simpl; auto.
    match goal with
    | Hc : lrel ?a ?b |- context [fchain ?a ?sms] =>
        pose proof (fchain_rel sms Hc) as Hch; unfold crel in Hch;
        destruct (fchain a sms) as [l1|], (xrun_chain b sms) as [l2|];
        try contradiction; simpl; auto
    end.
    match goal with
    | Hd : envrel ?fe0 ?d0 |- context [fxinterp_binds k P (fapply_env P fe h th (fapp l1 ?fe0)) [] st bs] =>
        pose proof (IHb P (fapply_env P fe h th (fapp l1 fe0)) (xapply_env P d h th (l2 ++ d0)) [] st bs (fapply_env_rel P h th H (envrel_app Hch Hd))) as Hb;
        destruct (fxinterp_binds k P (fapply_env P fe h th (fapp l1 fe0)) [] st bs) as [[[d3 g3] st3]|r3],
                 (xinterp_binds k P (xapply_env P d h th (l2 ++ d0)) [] st bs) as [[[d3' g3'] st3']|r3'];
        simpl in Hb; try contradiction; auto
    end.
    destruct Hb as [Hd3 [-> ->]].
    cp Hd3 (inst_smap th Dr); simpl; auto. split; [apply fresult_env_rel; assumption|reflexivity].
  - intros P fe d g st u brs H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context [find_branch ?h brs] =>
      destruct (find_branch h brs) as [[bs pairs]|] end; simpl; auto.
    pose proof (IHb P fe d g st bs H) as Hb.
    destruct (fxinterp_binds k P fe g st bs) as [[[d3 g3] st3]|r3],
             (xinterp_binds k P d g st bs) as [[[d3' g3'] st3']|r3']; simpl in Hb;
      try contradiction; auto.
    destruct Hb as [Hd3 [-> ->]].
    cp Hd3 pairs; simpl; auto.
  - intros P fe d u inputs outputs st H. simpl.
    lk H (IFun u); simpl; auto.
    match goal with |- context[xp_fun ?P ?h] => destruct(xp_fun P h)as[[Dr bs]|]end; simpl;auto.
    cp H inputs; simpl;auto.
    match goal with Hc:lrel ?a ?b |-context[fchain ?a ?sms] =>
      pose proof(fchain_rel sms Hc)as Hch; unfold crel in Hch;
      destruct(fchain a sms)as[l1|],(xrun_chain b sms)as[l2|]; try contradiction;simpl;auto end.
    match goal with Hd:envrel ?fe0 ?d0 |-context[fxinterp_binds k P (fapp l1 ?fe0) [] st bs] =>
      pose proof(IHb P (fapp l1 fe0)(l2++d0) [] st bs (envrel_app Hch Hd))as Hb;
      destruct(fxinterp_binds k P (fapp l1 fe0) [] st bs)as[[[d3 g3]st3]|r3],
      (xinterp_binds k P (l2++d0) [] st bs)as[[[d3' g3']st3']|r3'];simpl in Hb;try contradiction;auto end.
    destruct Hb as[Hd3[-> ->]].
    cp Hd3 (xp_exports P h); simpl;auto.
    match goal with |-context[fcompose(fapp ?named fempty) outputs] =>
      match goal with Hn:lrel named ?named' |- _ => cp (envrel_of_lrel Hn) outputs end end;
      simpl;auto.

Qed.

Theorem fxinterp_agrees : forall fuel P fe d g st e,
  envrel fe d -> fxinterp fuel P fe g st e = xinterp fuel P d g st e.
Proof.
  intros fuel. assert (H : AGREE fuel).
  { induction fuel as [|k IH].
    - repeat split; intros; simpl; auto.
    - apply agree_step, IH. }
  apply H.
Qed.

Theorem frun_main_eq : forall fuel P main d0 st0,
  frun_main fuel P main d0 st0 = run_main fuel P main d0 st0.
Proof.
  intros. unfold frun_main, run_main. destruct (xp_fn P main) as [body|]; auto.
  rewrite (fxinterp_agrees fuel P [] st0 body (conv_env_rel d0)). reflexivity.
Qed.
