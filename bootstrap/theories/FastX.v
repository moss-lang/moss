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
(* one trie per item kind: types, vals, fns, functors; methods by symbol,
   each a bucket of (receiver type, θ, entry), the most recent first *)
with fenv : Type :=
| FEnv : tmap fentry -> tmap fentry -> tmap fentry -> tmap fentry
         -> tmap (list (ty * subst * fentry)) -> fenv.

Definition fempty : fenv := FEnv TE TE TE TE TE.

Fixpoint bfind (r : ty) (th : subst) (b : list (ty * subst * fentry)) : option fentry :=
  match b with
  | [] => None
  | (r', th', en) :: b' => if ty_eqb r r' && subst_eqb th th' then Some en else bfind r th b'
  end.

Definition flookup (i : item) (d : fenv) : option fentry :=
  match d with
  | FEnv mt mv mf mu mm =>
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
  | FEnv mt mv mf mu mm =>
      match i with
      | ITy t => FEnv (tset mt t en) mv mf mu mm
      | IVal v => FEnv mt (tset mv v en) mf mu mm
      | IFn f => FEnv mt mv (tset mf f en) mu mm
      | IFun u => FEnv mt mv mf (tset mu u en) mm
      | IMth r m th =>
          FEnv mt mv mf mu
               (tset mm m ((r, th, en) :: match tget mm m with Some b => b | None => [] end))
      end
  end.

Lemma flookup_fadd : forall i i' en d,
  flookup i (fadd i' en d) = if item_eqb i i' then Some en else flookup i d.
Proof.
  intros i i' en [mt mv mf mu mm].
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
  end.

Fixpoint conv_env (l : list (item * xdentry)) : fenv :=
  match l with [] => fempty | (i, e) :: r => fadd i (conv_entry e) (conv_env r) end.

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
with envrel : fenv -> xdenv -> Prop :=
| ERel : forall fe d,
    (forall i, flookup i fe = None -> lookup_xdenv i d = None) ->
    (forall i a, flookup i fe = Some a -> exists b, lookup_xdenv i d = Some b /\ erel a b) ->
    envrel fe d.

Definition orel (a : option fentry) (b : option xdentry) : Prop :=
  match a, b with
  | Some x, Some y => erel x y
  | None, None => True
  | _, _ => False
  end.

Lemma envrel_lookup : forall fe d i, envrel fe d -> orel (flookup i fe) (lookup_xdenv i d).
Proof.
  intros fe d i H. inversion H as [? ? Hn Hs]; subst. unfold orel.
  destruct (flookup i fe) as [a|] eqn:E.
  - destruct (Hs i a E) as [b [-> Hb]]. exact Hb.
  - rewrite (Hn i E). exact I.
Qed.

Lemma envrel_intro : forall fe d, (forall i, orel (flookup i fe) (lookup_xdenv i d)) -> envrel fe d.
Proof.
  intros fe d H. constructor.
  - intros i E. specialize (H i). rewrite E in H. unfold orel in H.
    destruct (lookup_xdenv i d); [contradiction|auto].
  - intros i a E. specialize (H i). rewrite E in H. unfold orel in H.
    destruct (lookup_xdenv i d) as [b|]; [eauto|contradiction].
Qed.

Lemma envrel_empty : envrel fempty [].
Proof. apply envrel_intro. intros []; simpl; auto. Qed.

Lemma envrel_add : forall fe d i a b,
  erel a b -> envrel fe d -> envrel (fadd i a fe) ((i, b) :: d).
Proof.
  intros fe d i a b Hab H. apply envrel_intro. intro j.
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
  fix IH 1. intros [w|f d|h sms d|p|h]; simpl; constructor.
  all: induction d as [|[i e] r IHd]; [apply envrel_empty|].
  all: apply envrel_add; [apply IH|exact IHd].
Qed.

Lemma conv_env_rel : forall d, envrel (conv_env d) d.
Proof.
  induction d as [|[i e] r IH]; simpl; [apply envrel_empty|].
  apply envrel_add; [apply conv_rel|exact IH].
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
          | XR (XOk w) g1 st1 => XR (XOk (XVTag n w)) g1 st1
          | r => r
          end
      | XCallD f th sm args =>
          match xp_fn P f with
          | Some body =>
              match fxinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  match fcompose d sm with
                  | Some d' => ret_call g1 (fxinterp k P (fapp d' fempty) (mk_xvenv ws) st1 body)
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
                  let (o, st2) := run_prim P p ws st1 in XR o g1 st2
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
                      let (o, st2) := run_prim P p (w0 :: ws) st1 in XR o g1 st2
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
      | XBindTy _ _ e1 => fxinterp k P d g st e1
      | XBindVal v e1 e2 =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp k P (fadd (IVal v) (FVal w1) d) g1 st1 e2
          | r => r
          end
      | XBindFn f f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IFn f) (FClo f' (fapp dc fempty)) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindMth tauk m th f' sm e1 =>
          match fcompose d sm with
          | Some dc => fxinterp k P (fadd (IMth tauk m th) (FClo f' (fapp dc fempty)) d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFun u h sm smc e1 =>
          match fcompose d smc with
          | Some dc => fxinterp k P (fadd (IFun u) (FFun h [sm] (fapp dc fempty)) d) g st e1
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
      | XBindPrim i p e1 => fxinterp k P (fadd i (FPrim p) d) g st e1
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
      | XBTy _ _ :: r => fxinterp_binds k P d g st r
      | XBVal v e1 :: r =>
          match fxinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => fxinterp_binds k P (fadd (IVal v) (FVal w1) d) g1 st1 r
          | rr => inr rr
          end
      | XBFn f f' sm :: r =>
          match fcompose d sm with
          | Some dc => fxinterp_binds k P (fadd (IFn f) (FClo f' (fapp dc fempty)) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBMth tauk m th f' sm :: r =>
          match fcompose d sm with
          | Some dc =>
              fxinterp_binds k P (fadd (IMth tauk m th) (FClo f' (fapp dc fempty)) d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFun u h sm smc :: r =>
          match fcompose d smc with
          | Some dc => fxinterp_binds k P (fadd (IFun u) (FFun h [sm] (fapp dc fempty)) d) g st r
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
      | XBPrim i p :: r => fxinterp_binds k P (fadd i (FPrim p) d) g st r
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
                      match fxinterp_binds k P (fapp d2 dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match fcompose d3 (inst_smap th Dr) with
                          | Some dr => inl (dr, st3)
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
        rrel (fxinterp_branch k P fe g st u brs) (xinterp_branch k P d g st u brs)).

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
  intros k [IHe [IHl [IHb [IHa IHr]]]].
  split; [|split; [|split; [|split]]].
  - intros P fe d g st e H. destruct e; simpl.
    all: try (erewrite ?(IHe P fe d) by exact H; reflexivity).
    + (* XVl *) lk H (IVal v); auto.
    + (* XCallD *)
      destruct (xp_fn P f); auto. rewrite (IHl P fe d) by exact H.
      destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
      cp H s0; auto. f_equal. apply IHe, envrel_of_lrel; auto.
    + (* XCallA *)
      lk H (IFn f); auto.
      * dfn; auto. rewrite (IHl P fe d) by exact H.
        destruct (xinterp_list k P d g st l) as [[[ws g1] st1]|]; auto.
        f_equal. apply IHe; auto.
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
    + (* XMatch *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      destruct w0; auto. destruct (find_xarm n l) as [[x eb]|]; auto.
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XLet *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      rewrite (IHe P fe d) by exact H. reflexivity.
    + (* XBindVal *)
      rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st e1) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
      apply IHe, envrel_add; auto. constructor.
    + (* XBindFn *)
      cp H s; auto. apply IHe, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + (* XBindMth *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + (* XBindFun *)
      cp H s0; auto. apply IHe, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + (* XBindFunA *)
      lk H (IFun u0); auto. apply IHe, envrel_add; auto. constructor; auto.
    + (* XApply *)
      pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
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
    + (* XBindPrim *) apply IHe, envrel_add; auto. constructor.
  - intros P fe d g st l H. destruct l as [|e r]; simpl; auto.
    rewrite (IHe P fe d) by exact H.
    destruct (xinterp k P d g st e) as [[w0| | | | | | |] g0 st0]; auto; try (apply IHe; assumption).
    rewrite (IHl P fe d) by exact H. reflexivity.
  - intros P fe d g st bs H. destruct bs as [|b r]; simpl; [auto|].
    destruct b; simpl.
    + apply IHb; auto.
    + rewrite (IHe P fe d) by exact H.
      destruct (xinterp k P d g st x) as [[w0| | | | | | |] g0 st0]; simpl; auto.
      apply IHb, envrel_add; auto. constructor.
    + cp H s; simpl; auto. apply IHb, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + cp H s0; simpl; auto. apply IHb, envrel_add; auto. constructor. apply envrel_of_lrel; auto.
    + lk H (IFun u0); simpl; auto. apply IHb, envrel_add; auto. constructor; auto.
    + pose proof (IHa P fe d u s s0 st H) as Ha.
      destruct (fxinterp_apply k P fe u s s0 st) as [[dr st1]|r1],
               (xinterp_apply k P d u s s0 st) as [[dr' st1']|r1']; simpl in Ha;
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
    + apply IHb, envrel_add; auto. constructor.
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
    | Hd : envrel ?fe0 ?d0 |- context [fxinterp_binds k P (fapp l1 ?fe0) [] st bs] =>
        pose proof (IHb P (fapp l1 fe0) (l2 ++ d0) [] st bs (envrel_app Hch Hd)) as Hb;
        destruct (fxinterp_binds k P (fapp l1 fe0) [] st bs) as [[[d3 g3] st3]|r3],
                 (xinterp_binds k P (l2 ++ d0) [] st bs) as [[[d3' g3'] st3']|r3'];
        simpl in Hb; try contradiction; auto
    end.
    destruct Hb as [Hd3 [-> ->]].
    cp Hd3 (inst_smap th Dr); simpl; auto.
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
