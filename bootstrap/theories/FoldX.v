(* ------------------------------------------------------------------------ *)
(* FoldX.v — folding calls to small fns, proved.                             *)
(*                                                                           *)
(* In the self-hosted compiler most calls are to tiny library fns: `two()`  *)
(* is `w::i32_add(w::i32_one, w::i32_one)`, `raw(n)` is                     *)
(* `match n { Num(v) => v }`, `slot(d, i)` is arithmetic on `eight()` and   *)
(* `four()`.  A direct call `RCallD f true pl args` composes f's            *)
(* environment, builds its locals and runs its body; `tos` instead          *)
(* translates the body (and, to a bounded depth, the bodies of the fns it   *)
(* calls) into one `sexpr` over the caller's environment, its lookups       *)
(* remapped through the satisfaction map (`rml`).  `fold_e` replaces such a *)
(* call by `RCallF f pl args D s`: when the fuel is at least D and `sev`    *)
(* evaluates s, that is the call's result; otherwise the call runs as an    *)
(* RCallD.                                                                   *)
(*                                                                           *)
(* Proved (kernel primitives only):                                         *)
(*  - fold_sound: if tos gives s for e, s evaluates, and the fuel is at     *)
(*    least sdepth s, then rinterp on e gives exactly s's result (with the  *)
(*    locals unchanged);                                                    *)
(*  - fold_agrees: rinterp on the folded program = rinterp on the program,  *)
(*    for every fuel, environment, locals and machine;                      *)
(*  - fold_run_main: rrun_main on the folded program = rrun_main.           *)
(* `tb` (the bodies tos may unfold) is a parameter with the obligation that *)
(* they are the program's (Htb); the driver passes the very table it runs.  *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List Bool Arith Lia PrimInt63.
From NM Require Import Audit CoreMoss MossX FastX Elab Prune SlotX.
Import ListNotations.

Set Implicit Arguments.
Local Open Scope nat_scope.

(* ===================== 1. Remapping lookups ============================= *)

(* where a lookup in `rcompose d pl` reads from, as a lookup in d *)
Definition rml (pl : plan) (l : rlk) : rlk :=
  let src j := match nth_error pl j with Some (_, l') => l' | None => LMiss end in
  match l with
  | LAt j => src j
  | LMiss => LMiss
  | LDyn i => match pos i (map fst pl) with Some j => src j | None => LMiss end
  end.

Definition rmlo (pl : option plan) (l : rlk) : rlk :=
  match pl with None => l | Some pl => rml pl l end.

Definition remap_plan (pl : option plan) (pl2 : plan) : plan :=
  map (fun p => (fst p, rmlo pl (snd p))) pl2.

(* every source a position; the largest *)
Fixpoint maxat (pl : plan) : option nat :=
  match pl with
  | [] => Some 0
  | (_, LAt j) :: r => match maxat r with Some m => Some (Nat.max j m) | None => None end
  | _ :: _ => None
  end.

(* ===================== 2. The translation ================================ *)

Section Tos.
Variable tb : fsym -> option rexpr.   (* the bodies a call may unfold *)

(* m: structural fuel; n: how many calls deep to unfold *)
Fixpoint tos (m n : nat) (pl : option plan) (e : rexpr) {struct m} : option sexpr :=
  match m with
  | 0 => None
  | S m' =>
      match e with
      | RVar x => Some (SVar x)
      | RVl l => Some (SVl (rmlo pl l))
      | RTag t th e1 => match tos m' n pl e1 with Some s1 => Some (STag t s1) | None => None end
      | RCallA l args =>
          match tosl m' n pl args with Some ss => Some (SPrim (rmlo pl l) ss) | None => None end
      | RCallD f th true pl2 args =>
          match n with
          | 0 => None
          | S n' =>
              match tb f with
              | Some bs =>
                  let pl2' := remap_plan pl pl2 in
                  match maxat pl2' with
                  | Some j =>
                      match tosl m' n pl args, tos m' n' (Some pl2') bs with
                      | Some ss, Some sb => Some (SCall j ss sb)
                      | _, _ => None
                      end
                  | None => None
                  end
              | None => None
              end
          end
      | RMatchP e0 [(p, b)] =>
          match tos m' n pl e0, tos m' n pl b with
          | Some s0, Some sb => Some (SMatch1 s0 p sb)
          | _, _ => None
          end
      | _ => None
      end
  end
with tosl (m n : nat) (pl : option plan) (l : list rexpr) {struct m} : option (list sexpr) :=
  match m with
  | 0 => None
  | S m' =>
      match l with
      | [] => Some []
      | a :: r => match tos m' n pl a, tosl m' n pl r with
                  | Some s, Some ss => Some (s :: ss)
                  | _, _ => None
                  end
      end
  end.
End Tos.

(* the fuel a folded expression's original needs *)
Fixpoint sdepth (e : sexpr) : nat :=
  match e with
  | SVar _ | SVl _ => 1
  | STag _ e1 => S (sdepth e1)
  | SPrim _ args => S ((fix nd (l : list sexpr) : nat :=
                          match l with [] => 1 | a :: r => S (Nat.max (sdepth a) (nd r)) end) args)
  | SCall _ args body =>
      S (Nat.max ((fix nd (l : list sexpr) : nat :=
                     match l with [] => 1 | a :: r => S (Nat.max (sdepth a) (nd r)) end) args)
                  (sdepth body))
  | SMatch1 e0 _ b => S (Nat.max (sdepth e0) (sdepth b))
  end.

Fixpoint needed (l : list sexpr) : nat :=
  match l with [] => 1 | a :: r => S (Nat.max (sdepth a) (needed r)) end.

Lemma sdepth_prim : forall l args, sdepth (SPrim l args) = S (needed args).
Proof. reflexivity. Qed.
Lemma sdepth_call : forall j args body, sdepth (SCall j args body) = S (Nat.max (needed args) (sdepth body)).
Proof. reflexivity. Qed.

Lemma sev_prim : forall P d g st l args, sev P d g st (SPrim l args) =
  match rlook l d with
  | Some (QPrim p) =>
      match sevl P d g args st with
      | Some (ws, st1) => match run_prim_at P p None ws st1 with (XOk w, st2) => Some (w, st2) | _ => None end
      | None => None
      end
  | _ => None
  end.
Proof. reflexivity. Qed.

Lemma sev_call : forall P d g st j args body, sev P d g st (SCall j args body) =
  match rnth2 j d with
  | Some _ => match sevl P d g args st with
              | Some (ws, st1) => sev P d (mkv ws) st1 body
              | None => None
              end
  | None => None
  end.
Proof. reflexivity. Qed.

(* ===================== 3. Remapping is reading through the plan ========== *)

Definition remaps (pl : option plan) (d d' : renv) : Prop :=
  forall l, rlook (rmlo pl l) d = rlook l d'.

Lemma rnth_le : forall j j' d, j' <= j -> rnth j d <> None -> rnth j' d <> None.
Proof.
  induction j as [|j IH]; intros j' [|[i en] r] Hle H; simpl in *; try congruence.
  - assert (j' = 0) by lia. subst. simpl. congruence.
  - destruct j' as [|j']; simpl; [congruence|]. apply IH; auto. lia.
Qed.

Lemma rcompose_nth : forall d pl d', rcompose d pl = Some d' ->
  forall j, rnth j d' = match nth_error pl j with Some (_, l) => rlook l d | None => None end.
Proof.
  intros d pl. induction pl as [|[req l] r IH]; simpl; intros d' H j.
  - inversion H; subst. destruct j; reflexivity.
  - destruct (rlook l d) as [en|] eqn:El; [|discriminate].
    destruct (rcompose d r) as [d0|] eqn:E; [|discriminate]. inversion H; subst.
    destruct j as [|j]; simpl; auto.
Qed.

Lemma rcompose_keys : forall d pl d', rcompose d pl = Some d' -> map fst d' = map fst pl.
Proof.
  intros d pl. induction pl as [|[req l] r IH]; simpl; intros d' H.
  - inversion H; reflexivity.
  - destruct (rlook l d); [|discriminate].
    destruct (rcompose d r) as [d0|] eqn:E; [|discriminate]. inversion H; subst. simpl. f_equal; auto.
Qed.

Lemma rml_ok : forall d pl d', rcompose d pl = Some d' -> forall l, rlook (rml pl l) d = rlook l d'.
Proof.
  intros d pl d' H l. unfold rml. destruct l as [j| |i]; simpl.
  - rewrite rnth2_eq, (rcompose_nth _ _ H j). destruct (nth_error pl j) as [[? l']|]; reflexivity.
  - reflexivity.
  - rewrite rlookup_pos, (rcompose_keys _ _ H).
    destruct (pos i (map fst pl)) as [j|]; [|reflexivity].
    rewrite (rcompose_nth _ _ H j). destruct (nth_error pl j) as [[? l']|]; reflexivity.
Qed.

Lemma remap_plan_compose : forall pl pl2 d d', remaps pl d d' ->
  rcompose d (remap_plan pl pl2) = rcompose d' pl2.
Proof.
  intros pl pl2 d d' H. induction pl2 as [|[req l] r IH]; simpl; auto.
  rewrite H, IH. reflexivity.
Qed.

Lemma maxat_le : forall pl j, maxat pl = Some j ->
  forall q, In q pl -> exists j', snd q = LAt j' /\ j' <= j.
Proof.
  induction pl as [|[req l] r IH]; simpl; intros j H q Hq; [contradiction|].
  destruct l as [j0| |i]; try discriminate.
  destruct (maxat r) as [m|] eqn:Em; [|discriminate]. inversion H; subst.
  destruct Hq as [<-|Hq].
  - exists j0. split; [reflexivity|lia].
  - destruct (IH m eq_refl q Hq) as [j' [E Hle]]. exists j'. split; auto. lia.
Qed.

Lemma maxat_compose : forall pl j d, maxat pl = Some j -> rnth2 j d <> None ->
  exists d', rcompose d pl = Some d'.
Proof.
  intros pl j d Hm Hn. rewrite rnth2_eq in Hn.
  assert (Hall : forall q, In q pl -> rlook (snd q) d <> None).
  { intros q Hq. destruct (maxat_le _ Hm q Hq) as [j' [-> Hle]]. simpl.
    rewrite rnth2_eq. eapply rnth_le; eauto. }
  clear Hm Hn. induction pl as [|[req l] r IH]; simpl; eauto.
  assert (Hl := Hall (req, l) (or_introl eq_refl)). simpl in Hl.
  destruct (rlook l d) as [en|]; [|congruence].
  destruct IH as [d0 ->]; [intros; apply Hall; right; auto|]. eauto.
Qed.

Lemma remaps_compose : forall pl pl2 d d' d'', remaps pl d d' -> rcompose d' pl2 = Some d'' ->
  remaps (Some (remap_plan pl pl2)) d d''.
Proof.
  intros pl pl2 d d' d'' H Hc. intro l. simpl. apply rml_ok.
  rewrite (remap_plan_compose pl2 H). exact Hc.
Qed.

(* ===================== 4. Folding is sound =============================== *)

Lemma xcast_plain : forall P d n th w, xreify P = false -> xcast P d n th w = XOk (XVTag n w).
Proof.
 intros P d n th w H; unfold xreify in H; apply orb_false_iff in H as [_ H]; unfold xcast.
 destruct (xp_cast P); [reflexivity|discriminate].
Qed.

Section Sound.
Variable R : rprog.
Hypothesis Hplain : xreify (rp_x R) = false.
Variable tb : fsym -> option rexpr.
Hypothesis Htb : forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd).

Local Notation P := (rp_x R).

Definition SOUND (k : nat) : Prop :=
  (forall m n pl e s d d' g st w st', tos tb m n pl e = Some s -> remaps pl d d' ->
     sev P d g st s = Some (w, st') -> sdepth s <= k -> rinterp k R d' g st e = XR (XOk w) g st')
  /\ (forall m n pl l ss d d' g st ws st', tosl tb m n pl l = Some ss -> remaps pl d d' ->
     sevl P d g ss st = Some (ws, st') -> needed ss <= k -> rinterp_list k R d' g st l = inl (ws, g, st')).

Lemma sound_step : forall k, SOUND k -> SOUND (S k).
Proof.
  intros k [IHe IHl]. split.
  - intros m n pl e s d d' g st w st' Ht Hr Hs Hk.
    destruct m as [|m]; [discriminate|]. cbn [tos] in Ht.
    destruct e; try discriminate.
    + (* RVar *) inversion Ht; subst. simpl in Hs. simpl.
      destruct (lookup_xvenv v g); inversion Hs; subst; reflexivity.
    + (* RVl *) inversion Ht; subst. simpl in Hs. rewrite Hr in Hs. simpl.
      destruct (rlook r d') as [[]|]; inversion Hs; subst; reflexivity.
    + (* RTag *)
      destruct (tos tb m n pl e) as [s1|] eqn:E; inversion Ht; subst. simpl in Hs, Hk.
      destruct (sev P d g st s1) as [[w1 st1]|] eqn:Es; inversion Hs; subst.
      simpl. rewrite (IHe _ _ _ _ _ _ _ _ _ _ _ E Hr Es) by lia. rewrite rtyped_cast_exact, xcast_plain by exact Hplain; reflexivity.
    + (* RCallD *)
      destruct b; [|discriminate]. destruct n as [|n']; [discriminate|].
      destruct (tb f) as [bs|] eqn:Etb; [|discriminate].
      destruct (maxat (remap_plan pl p)) as [j|] eqn:Em; [|discriminate].
      destruct (tosl tb m (S n') pl l) as [ss|] eqn:El; [|discriminate].
      destruct (tos tb m n' (Some (remap_plan pl p)) bs) as [sb|] eqn:Eb; [|discriminate].
      inversion Ht; subst. rewrite sev_call in Hs. rewrite sdepth_call in Hk.
      destruct (rnth2 j d) eqn:En; [|discriminate].
      destruct (sevl P d g ss st) as [[ws st1]|] eqn:Esl; [|discriminate].
      destruct (Htb Etb) as [bd Hfn].
      destruct (@maxat_compose _ _ d Em) as [d2 Hc2]; [congruence|].
      rewrite (remap_plan_compose p Hr) in Hc2.
      cbn [rinterp]. rewrite Hfn.
      rewrite (IHl _ _ _ _ _ _ _ _ _ _ _ El Hr Esl) by lia.
      rewrite Hc2. unfold rcall_env; rewrite Hplain; simpl.
      rewrite (IHe _ _ _ _ _ _ _ _ _ _ _ Eb (remaps_compose p Hr Hc2) Hs) by lia.
      reflexivity.
    + (* RCallA *)
      destruct (tosl tb m n pl l) as [ss|] eqn:El; inversion Ht; subst.
      rewrite sev_prim, Hr in Hs. rewrite sdepth_prim in Hk. cbn [rinterp].
      destruct (rlook r d') as [[w0|b0 f0 dc0|h0 sms0 dc0|tau0|p0 tau0|p0|h0]|]; try discriminate.
      destruct (sevl P d g ss st) as [[ws st1]|] eqn:Esl; [|discriminate].
      rewrite (IHl _ _ _ _ _ _ _ _ _ _ _ El Hr Esl) by lia.
      destruct (run_prim_at P p0 None ws st1) as [[]  st2]; inversion Hs; subst; reflexivity.
    + (* RMatchP *)
      destruct l as [|[p b] [|]]; try discriminate.
      destruct (tos tb m n pl e) as [s0|] eqn:E0; [|discriminate].
      destruct (tos tb m n pl b) as [sb|] eqn:Eb; [|discriminate].
      inversion Ht; subst. simpl in Hs, Hk.
      destruct (sev P d g st s0) as [[w0 st1]|] eqn:Es0; [|discriminate].
      cbn [rinterp]. rewrite (IHe _ _ _ _ _ _ _ _ _ _ _ E0 Hr Es0) by lia.
      simpl. destruct (match_pat p w0) as [bs| |] eqn:Emp; try discriminate.
      rewrite (IHe _ _ _ _ _ _ _ _ _ _ _ Eb Hr Hs) by lia.
      simpl. rewrite skipn_app, Nat.sub_diag, skipn_all. reflexivity.
  - intros m n pl l ss d d' g st ws st' Ht Hr Hs Hk.
    destruct m as [|m]; [discriminate|]. cbn [tosl] in Ht.
    destruct l as [|a r].
    + inversion Ht; subst. simpl in Hs. inversion Hs; subst. reflexivity.
    + destruct (tos tb m n pl a) as [s|] eqn:Ea; [|discriminate].
      destruct (tosl tb m n pl r) as [ss'|] eqn:Er; [|discriminate].
      inversion Ht; subst. simpl in Hs, Hk.
      destruct (sev P d g st s) as [[w st1]|] eqn:Es; [|discriminate].
      destruct (sevl P d g ss' st1) as [[ws' st2]|] eqn:Esl; [|discriminate].
      inversion Hs; subst. cbn [rinterp_list].
      rewrite (IHe _ _ _ _ _ _ _ _ _ _ _ Ea Hr Es) by lia.
      rewrite (IHl _ _ _ _ _ _ _ _ _ _ _ Er Hr Esl) by lia. reflexivity.
Qed.

Theorem fold_sound : forall k, SOUND k.
Proof.
  induction k as [|k IH].
  - split; intros; [destruct s|destruct ss]; simpl in *; lia.
  - apply sound_step, IH.
Qed.

End Sound.

(* ===================== 5. The pass ======================================= *)

Section Pass.
Variable tb : fsym -> option rexpr.
Variable M N : nat.   (* tos's structural fuel and unfolding depth *)

Fixpoint fold_e (e : rexpr) {struct e} : rexpr :=
  match e with
  | RVar _ | RUnit | RVl _ | RBreak => e
  | RTag t th e1 => RTag t th (fold_e e1)
  | RCallD f th b pl args =>
      if b then
        match tb f with
        | Some bs =>
            match tosl tb M N None args, tos tb M N None bs with
            | Some ss, Some sb =>
                RCallF f pl (map fold_e args) (Nat.max (needed ss) (sdepth sb)) ss sb
            | _, _ => RCallD f th b pl (map fold_e args)
            end
        | None => RCallD f th b pl (map fold_e args)
        end
      else RCallD f th b pl (map fold_e args)
  | RCallA l args => RCallA l (map fold_e args)
  | RMeth e0 l args => RMeth (fold_e e0) l (map fold_e args)
  | RMatch e0 arms => RMatch (fold_e e0) (map (fun a => match a with (t, x, b) => (t, x, fold_e b) end) arms)
  | RLet x e1 e2 => RLet x (fold_e e1) (fold_e e2)
  | RBindTy t tau e1 => RBindTy t tau (fold_e e1)
  | RBindVal v e1 e2 => RBindVal v (fold_e e1) (fold_e e2)
  | RBindClo i f b pl e1 => RBindClo i f b pl (fold_e e1)
  | RBindFun u h sm pl e1 => RBindFun u h sm pl (fold_e e1)
  | RBindFunA u l sm e1 => RBindFunA u l sm (fold_e e1)
  | RApply l th pl e1 => RApply l th pl (fold_e e1)
  | RApplyR l inputs outputs e1 => RApplyR l inputs outputs (fold_e e1)
  | RSeq e1 e2 => RSeq (fold_e e1) (fold_e e2)
  | RAssign x e1 => RAssign x (fold_e e1)
  | RIf c e1 e2 => RIf (fold_e c) (fold_e e1) (fold_e e2)
  | RWhile c b => RWhile (fold_e c) (fold_e b)
  | RLoop b => RLoop (fold_e b)
  | RReturn e1 => RReturn (fold_e e1)
  | RRec fs => RRec (map (fun p => match p with (f, e1) => (f, fold_e e1) end) fs)
  | RField e1 f => RField (fold_e e1) f
  | RMatchP e0 arms => RMatchP (fold_e e0) (map (fun a => match a with (p, b) => (p, fold_e b) end) arms)
  | RBindFunSym u h e1 => RBindFunSym u h (fold_e e1)
  | RBindFunCopy u l e1 => RBindFunCopy u l (fold_e e1)
  | RApplyBr l brs e1 =>
      RApplyBr l (map (fun br => match br with (h, bs, pl) => (h, map fold_b bs, pl) end) brs) (fold_e e1)
  | RBindPrim i p e1 => RBindPrim i p (fold_e e1)
  | RCallF f pl args D ss sb => RCallF f pl (map fold_e args) D ss sb
  end
with fold_b (b : rbind) {struct b} : rbind :=
  match b with
  | RBVal v e1 => RBVal v (fold_e e1)
  | RBApplyBr l brs =>
      RBApplyBr l (map (fun br => match br with (h, bs, pl) => (h, map fold_b bs, pl) end) brs)
  | _ => b
  end.

Definition fbrs (brs : list (usym * list rbind * plan)) : list (usym * list rbind * plan) :=
  map (fun br => match br with (h, bs, pl) => (h, map fold_b bs, pl) end) brs.

End Pass.

Section Agree.
Variable R R' : rprog.
Hypothesis Hplain : xreify (rp_x R) = false.
Variable tb : fsym -> option rexpr.
Variable M N : nat.
Hypothesis Htb : forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs, bd).
(* R' is R with every body folded *)
Hypothesis Hx : rp_x R' = rp_x R.
Hypothesis Hfn : forall f, rp_fn R' f =
  match rp_fn R f with Some (bs, bd) => Some (fold_e tb M N bs, fold_e tb M N bd) | None => None end.
Hypothesis Hfun : forall h, rp_fun R' h =
  match rp_fun R h with Some (Dr, bs) => Some (Dr, map (fold_b tb M N) bs) | None => None end.

Local Notation fe := (fold_e tb M N).
Local Notation fb := (fold_b tb M N).

Definition AGREE (k : nat) : Prop :=
  (forall e d g st, rinterp k R' d g st (fe e) = rinterp k R d g st e)
  /\ (forall l d g st, rinterp_list k R' d g st (map fe l) = rinterp_list k R d g st l)
  /\ (forall bs d g st, rinterp_binds k R' d g st (map fb bs) = rinterp_binds k R d g st bs)
  /\ (forall l th pl d st, rinterp_apply k R' d l th pl st = rinterp_apply k R d l th pl st)
  /\ (forall l brs d g st, rinterp_branch k R' d g st l (fbrs tb M N brs) = rinterp_branch k R d g st l brs)
  /\(forall l inputs outputs d st, rinterp_apply_r k R' d l inputs outputs st=rinterp_apply_r k R d l inputs outputs st).

Lemma rfind_arm_fe : forall t arms, rfind_arm t (map (fun a => match a with (t, x, b) => (t, x, fe b) end) arms)
  = match rfind_arm t arms with Some (x, b) => Some (x, fe b) | None => None end.
Proof. intros t arms. induction arms as [|[[t' x] b] r IH]; simpl; auto. destruct (Nat.eqb t t'); auto. Qed.

Lemma rselect_arm_fe : forall arms w, rselect_arm (map (fun a => match a with (p, b) => (p, fe b) end) arms) w
  = match rselect_arm arms w with Some (bs, b) => Some (bs, fe b) | None => None end.
Proof. intros arms w. induction arms as [|[p b] r IH]; simpl; auto. destruct (match_pat p w); auto. Qed.

Lemma rfind_branch_fb : forall h brs, rfind_branch h (fbrs tb M N brs)
  = match rfind_branch h brs with Some (bs, pl) => Some (map fb bs, pl) | None => None end.
Proof. intros h brs. induction brs as [|[[h' bs] pl] r IH]; simpl; auto. destruct (Nat.eqb h h'); auto. Qed.

Lemma rec_fe : forall (fs : list (fld * rexpr)), map snd (map (fun p => match p with (f, e1) => (f, fe e1) end) fs) = map fe (map snd fs)
  /\ map fst (map (fun p => match p with (f, e1) => (f, fe e1) end) fs) = map fst fs.
Proof. induction fs as [|[f e] r [IH1 IH2]]; simpl; auto. rewrite IH1, IH2. auto. Qed.

Lemma remaps_id : forall d, remaps None d d.
Proof. intros d l. reflexivity. Qed.

Lemma agree_step : forall k, AGREE k -> AGREE (S k).
Proof.
  intros k [IHe [IHl [IHb [IHa [IHbr IHar]]]]].
  Ltac dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun :=
    repeat (match goal with
    | |- context [xreify (rp_x R)] => rewrite Hplain
    | |- context [rcall_env ?P ?d ?th ?dc] => unfold rcall_env
    | |- context [rinterp ?k R' ?d ?g ?st (fold_e ?t ?m ?n ?e)] => rewrite (IHe e d g st)
    | |- context [rinterp_list ?k R' ?d ?g ?st (map ?F ?l)] => rewrite (IHl l d g st)
    | |- context [rinterp_binds ?k R' ?d ?g ?st (map ?F ?l)] => rewrite (IHb l d g st)
    | |- context [rinterp_apply ?k R' ?d ?l ?th ?pl ?st] => rewrite (IHa l th pl d st)
    | |- context [rinterp_apply_r ?k R' ?d ?l ?inputs ?outputs ?st] => rewrite(IHar l inputs outputs d st)
    | |- context [rinterp_branch ?k R' ?d ?g ?st ?l (fbrs ?t ?m ?n ?brs)] => rewrite (IHbr l brs d g st)
    | |- context [RWhile (fold_e ?t ?m ?n ?c) (fold_e ?t ?m ?n ?b)] =>
        change (RWhile (fold_e t m n c) (fold_e t m n b)) with (fold_e t m n (RWhile c b))
    | |- context [RLoop (fold_e ?t ?m ?n ?b)] =>
        change (RLoop (fold_e t m n b)) with (fold_e t m n (RLoop b))
    | |- context [rp_x R'] => rewrite Hx
    | |- context [rp_fn R' ?f] => rewrite (Hfn f); destruct (rp_fn R f) as [[? ?]|]
    | |- context [rp_fun R' ?f] => rewrite (Hfun f); destruct (rp_fun R f) as [[? ?]|]
    | |- context [match rinterp ?k R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp k R d g st e) as [[| | | | | | |] ? ?]
    | |- context [match rinterp_list ?k R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp_list k R d g st e) as [[[? ?] ?]|?]
    | |- context [match rinterp_binds ?k R ?d ?g ?st ?e with _ => _ end] =>
        destruct (rinterp_binds k R d g st e) as [[[? ?] ?]|?]
    | |- context [match rinterp_apply ?k R ?d ?l ?th ?pl ?st with _ => _ end] =>
        destruct (rinterp_apply k R d l th pl st) as [[? ?]|?]
    | |- context [match rinterp_apply_r ?k R ?d ?l ?inputs ?outputs ?st with _ => _ end] =>
        destruct(rinterp_apply_r k R d l inputs outputs st)as[[? ?]|?]
    | |- context [match rinterp_branch ?k R ?d ?g ?st ?l ?brs with _ => _ end] =>
        destruct (rinterp_branch k R d g st l brs) as [[[? ?] ?]|?]
    | |- context [match rlook ?l ?d with _ => _ end] => destruct (rlook l d) as [[]|]
    | |- context [match rcompose ?d ?pl with _ => _ end] => destruct (rcompose d pl)
    | |- context [match rchain ?d ?pl with _ => _ end] => destruct (rchain d pl)
    | |- context [match truthy ?P ?w with _ => _ end] => destruct (truthy P w) as [[]|]
    | |- context [if ?b then fold_e ?t ?m ?n ?x else fold_e ?t ?m ?n ?y] => destruct b
    | |- context [match (if ?c then ?x else None) with _ => _ end] =>
        destruct (if c then x else None) as [[? ?]|]
    end; cbn beta iota zeta); try reflexivity.
  split; [|split; [|split; [|split; [|split]]]].
  - intros e d g st. destruct e.
    all: try (cbn [fold_e]; cbn [rinterp]; dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun; fail).
    + (* RCallD *)
      match goal with |- context [RCallD ?f0 ?th0 ?b0 ?p0 ?a0] =>
        rename f0 into f, b0 into b, p0 into pl, a0 into args end.
      cbn [fold_e]. destruct b.
      2: { cbn [rinterp]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun. }
      destruct (tb f) as [bs|] eqn:Etb.
      2: { cbn [rinterp]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun. }
      destruct (tosl tb M N None args) as [ss|] eqn:El;
        [|cbn [rinterp]; dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun].
      destruct (tos tb M N None bs) as [sb|] eqn:Eb;
        [|cbn [rinterp]; dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun].
      cbn [rinterp]. rewrite Hx.
      destruct (Nat.leb (Nat.max (needed ss) (sdepth sb)) k) eqn:Ek.
      * destruct (fastcall (rp_x R) d g st pl ss sb) as [[w st2]|] eqn:Ef.
        -- apply Nat.leb_le in Ek. unfold fastcall in Ef.
           destruct (sevl (rp_x R) d g ss st) as [[ws st1]|] eqn:Esl; [|discriminate].
           destruct (rcompose d pl) as [d'|] eqn:Ec; [|discriminate].
           destruct (Htb Etb) as [bd Hfn0]. rewrite Hfn0. cbn beta iota zeta. unfold rcall_env; rewrite Hplain; simpl.
           rewrite (proj2 (fold_sound R Hplain tb Htb k) _ _ _ _ _ _ _ _ _ _ _ El (remaps_id d) Esl) by lia.
           cbn beta iota zeta.
           rewrite (proj1 (fold_sound R Hplain tb Htb k) _ _ _ _ _ _ _ _ _ _ _ Eb (remaps_id d') Ef) by lia.
           reflexivity.
        -- dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
      * dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
    + (* RMatch *)
      cbn [fold_e rinterp]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
      all: try (match goal with |- context [match ?w with XVTag _ _ => _ | _ => _ end] => destruct w end;
                cbn iota; try reflexivity; rewrite rfind_arm_fe;
                match goal with |- context [rfind_arm ?t ?a] => destruct (rfind_arm t a) as [[? ?]|] end;
                dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun).
    + (* RRec *)
      cbn [fold_e rinterp]. match goal with |- context [map snd (map ?F ?fs)] => destruct (rec_fe fs) as [-> ->] end. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
    + (* RMatchP *)
      cbn [fold_e rinterp]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
      all: rewrite rselect_arm_fe;
        match goal with |- context [rselect_arm ?a ?w] => destruct (rselect_arm a w) as [[? ?]|] end;
        dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
    + (* RApplyBr *)
      cbn [fold_e rinterp]. match goal with brs : list (usym * list rbind * plan) |- _ => change (map _ brs) with (fbrs tb M N brs) end. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
  - intros l d g st. destruct l as [|e r]; [reflexivity|]. cbn [map rinterp_list].
    dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
  - intros bs d g st. destruct bs as [|b r]; [reflexivity|]. cbn [map].
    destruct b; cbn [fold_b rinterp_binds];
      try match goal with e : rexpr |- _ => fold (fold_e tb M N e) end;
      try (match goal with brs : list (usym * list rbind * plan) |- _ => change (map _ brs) with (fbrs tb M N brs) end);
      dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
  - intros l th pl d st. cbn [rinterp_apply]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
  - intros l brs d g st. cbn [rinterp_branch]. dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
    all: rewrite rfind_branch_fb;
      match goal with |- context [rfind_branch ?h ?a] => destruct (rfind_branch h a) as [[? ?]|] end;
      dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
  - intros l inputs outputs d st;cbn[rinterp_apply_r];dig IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.

Qed.

Theorem fold_agrees : forall k e d g st, rinterp k R' d g st (fe e) = rinterp k R d g st e.
Proof.
  intros k. assert (H : AGREE k).
  { induction k as [|k IH].
    - repeat split; intros; simpl; auto.
    - apply agree_step, IH. }
  apply H.
Qed.

Theorem fold_run_main : forall fuel main d0 st0,
  rrun_main fuel R' main d0 st0 = rrun_main fuel R main d0 st0.
Proof.
  intros. unfold rrun_main. rewrite Hfn. destruct (rp_fn R main) as [[bs bd]|]; auto.
  rewrite fold_agrees. reflexivity.
Qed.

End Agree.

(* Runtime table implementations may differ while their lookups agree. *)
Section Ext.
Variable R R' : rprog.
Hypothesis Hx : rp_x R' = rp_x R.
Hypothesis Hfn : forall f, rp_fn R' f = rp_fn R f.
Hypothesis Hfun : forall h, rp_fun R' h = rp_fun R h.
Lemma rinterp_ext_all : forall k,
 (forall e d g st, rinterp k R' d g st e = rinterp k R d g st e) /\
 (forall l d g st, rinterp_list k R' d g st l = rinterp_list k R d g st l) /\
 (forall bs d g st, rinterp_binds k R' d g st bs = rinterp_binds k R d g st bs) /\
 (forall l th pl d st, rinterp_apply k R' d l th pl st = rinterp_apply k R d l th pl st) /\
 (forall l brs d g st, rinterp_branch k R' d g st l brs = rinterp_branch k R d g st l brs)
  /\(forall l inputs outputs d st, rinterp_apply_r k R' d l inputs outputs st=rinterp_apply_r k R d l inputs outputs st).
Proof.
 induction k as [|k [IHe [IHl [IHb [IHa [IHbr IHar]]]]]].
 { repeat split; reflexivity. }
 Ltac finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun := repeat first
 [ progress rewrite ?Hx, ?Hfn, ?Hfun, ?IHe, ?IHl, ?IHb, ?IHa, ?IHbr, ?IHar
 | match goal with
   | |- context [match ?w with XVTag _ _ => _ | _ => _ end] => destruct w
   | |- context [match rinterp k R ?d ?g ?st ?e with _ => _ end] => destruct (rinterp k R d g st e) as [[] ? ?]
   | |- context [match rinterp_list k R ?d ?g ?st ?l with _ => _ end] => destruct (rinterp_list k R d g st l) as [[[? ?] ?]|?]
   | |- context [match rinterp_binds k R ?d ?g ?st ?bs with _ => _ end] => destruct (rinterp_binds k R d g st bs) as [[[? ?] ?]|?]
   | |- context [match rinterp_apply k R ?d ?l ?th ?pl ?st with _ => _ end] => destruct (rinterp_apply k R d l th pl st) as [[? ?]|?]
   | |- context [match rinterp_apply_r k R ?d ?l ?inputs ?outputs ?st with _ => _ end] => destruct(rinterp_apply_r k R d l inputs outputs st)as[[? ?]|?]
   | |- context [match rinterp_branch k R ?d ?g ?st ?l ?bs with _ => _ end] => destruct (rinterp_branch k R d g st l bs) as [[[? ?] ?]|?]
   | |- context [match rlook ?l ?d with _ => _ end] => destruct (rlook l d) as [[]|]
   | |- context [match rp_fn R ?f with _ => _ end] => destruct (rp_fn R f) as [[? ?]|]
   | |- context [match rp_fun R ?h with _ => _ end] => destruct (rp_fun R h) as [[? ?]|]
   | |- context [match rcompose ?d ?pl with _ => _ end] => destruct (rcompose d pl)
   | |- context [match rchain ?d ?sms with _ => _ end] => destruct (rchain d sms)
   | |- context [match rfind_branch ?h ?bs with _ => _ end] => destruct (rfind_branch h bs) as [[? ?]|]
   | |- context [match rfind_arm ?h ?bs with _ => _ end] => destruct (rfind_arm h bs) as [[? ?]|]
   | |- context [match rselect_arm ?bs ?w with _ => _ end] => destruct (rselect_arm bs w) as [[? ?]|]
   | |- context [match truthy ?P ?w with _ => _ end] => destruct (truthy P w) as [[]|]
   | |- context [if ?b then rinterp k R' _ _ _ _ else _] => destruct b
   end ]; try reflexivity.
 split; [|split; [|split; [|split; [|split]]]].
 - intros e d g st; destruct e; cbn [rinterp]; finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
 - intros l d g st; destruct l; cbn [rinterp_list]; finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
 - intros bs d g st; destruct bs as [|b bs]; [reflexivity|]; destruct b; cbn [rinterp_binds]; finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
 - intros l th pl d st; cbn [rinterp_apply]; finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
 - intros l brs d g st; cbn [rinterp_branch]; finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.
 - intros l inputs outputs d st;cbn[rinterp_apply_r];finish_ext k IHe IHl IHb IHa IHbr IHar Hx Hfn Hfun.

Qed.
Theorem rrun_main_ext : forall fuel main d0 st0,
 rrun_main fuel R' main d0 st0 = rrun_main fuel R main d0 st0.
Proof.
 intros; unfold rrun_main; rewrite Hfn; destruct (rp_fn R main) as [[bs bd]|]; auto.
 rewrite (proj1 (rinterp_ext_all fuel)); reflexivity.
Qed.
End Ext.

Definition guard_fold_e P tb M N e := if xreify P then e else fold_e tb M N e.
Definition guard_fold_b P tb M N b := if xreify P then b else fold_b tb M N b.

Theorem guard_fold_run_main : forall R R' tb M N fuel main d0 st0,
 (forall f bs, tb f = Some bs -> exists bd, rp_fn R f = Some (bs,bd)) ->
 rp_x R' = rp_x R ->
 (forall f, rp_fn R' f = match rp_fn R f with
  | Some (bs,bd) => Some (guard_fold_e (rp_x R) tb M N bs,guard_fold_e (rp_x R) tb M N bd)
  | None => None end) ->
 (forall h, rp_fun R' h = match rp_fun R h with
  | Some (Dr,bs) => Some (Dr,map (guard_fold_b (rp_x R) tb M N) bs)
  | None => None end) ->
 rrun_main fuel R' main d0 st0 = rrun_main fuel R main d0 st0.
Proof.
 intros R R' tb M N fuel main d0 st0 Htb Hx Hfn Hfun.
 unfold guard_fold_e,guard_fold_b in *; destruct (xreify (rp_x R)) eqn:Ep.
 - eapply rrun_main_ext; eauto.
   + intro f; rewrite Hfn; destruct (rp_fn R f) as [[bs bd]|]; reflexivity.
   + intro h; rewrite Hfun; destruct (rp_fun R h) as [[Dr bs]|]; simpl; auto; rewrite map_id; reflexivity.
 - eapply fold_run_main; eauto.
Qed.
