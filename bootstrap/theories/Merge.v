(* ------------------------------------------------------------------------ *)
(* Merge.v — the merges a context makes ([D43]: Elab.unify, Elab.canon)    *)
(* against Part V's σ (Track A, CtxOf; ExecutionModel note 55).                      *)
(*                                                                           *)
(* The elaborator keeps its merges as a list `uf` of edges g ↦ t, each made *)
(* by unify from a generation g not merged yet to a canonical type without  *)
(* g (the occurs check): the list is triangular (Tri), so following edges   *)
(* from any type ends after at most `length uf` steps, which is canon's     *)
(* fuel.  Its resolution `Res uf` is then a substitution σ = suf uf on the *)
(* relation's types: tr (Res uf t) = σ (tr t), and σ is idempotent.         *)
(* Whatever respects the edges (RespE) is unchanged by σ first, which is   *)
(* what most-generality asks; and unify keeps the list triangular, adds     *)
(* only edges every unifier of its two sides respects, and when it adds    *)
(* none, the two sides already resolve alike.                               *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrameCore BodyTr.
From NM Require ExecutionTypeFacts.
Import ListNotations.
Local Open Scope list_scope.

Module A := ExecutionModel.Elaboration.
Notation asub := ExecutionModel.app_subst.
Notation tsubst := ExecutionModel.subst.

Ltac noerr H := exfalso;
  solve [repeat (cbn beta iota zeta in H;
                 first [ exact (err_not_good _ _ _ _ _ _ H) | discriminate H
                       | apply bind_inv in H as (? & ? & ? & H) ])].

(* ======================================================================== *)
(* 1. Generations                                                            *)
(* ======================================================================== *)

Lemma gens_nom : forall n xs h, In h (ety_gens (YNom n xs)) <-> exists p, In p xs /\ In h (ety_gens (snd p)).
Proof.
  intros n xs h. cbn [ety_gens]. induction xs as [|[a y] r IH]; cbn [snd].
  - split; [intros []| intros (p & [] & _)].
  - rewrite in_app_iff, IH. split.
    + intros [H|(p & Hp & H)]; [exists (a, y); split; [left; reflexivity | exact H] | exists p; split; [right; exact Hp | exact H]].
    + intros (p & [<-|Hp] & H); [left; exact H | right; exists p; auto].
Qed.

Lemma gens_union : forall ms h, In h (ety_gens (YUnion ms)) <-> exists m, In m ms /\ In h (ety_gens m).
Proof.
  intros ms h. cbn [ety_gens]. induction ms as [|y r IH].
  - split; [intros []| intros (p & [] & _)].
  - rewrite in_app_iff, IH. split.
    + intros [H|(p & Hp & H)]; [exists y; split; [left; reflexivity | exact H] | exists p; split; [right; exact Hp | exact H]].
    + intros (p & [<-|Hp] & H); [left; exact H | right; exists p; auto].
Qed.

Lemma gens_rec : forall fs h, In h (ety_gens (YRec fs)) <-> exists p, In p fs /\ In h (ety_gens (snd p)).
Proof.
  intros fs h. cbn [ety_gens]. induction fs as [|[a y] r IH]; cbn [snd].
  - split; [intros []| intros (p & [] & _)].
  - rewrite in_app_iff, IH. split.
    + intros [H|(p & Hp & H)]; [exists (a, y); split; [left; reflexivity | exact H] | exists p; split; [right; exact Hp | exact H]].
    + intros (p & [<-|Hp] & H); [left; exact H | right; exists p; auto].
Qed.

Lemma nassoc_app' : forall V k (l m : list (nat * V)),
  nassoc k (l ++ m) = match nassoc k l with Some v => Some v | None => nassoc k m end.
Proof. intros V k l m. induction l as [|[a v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb k a); [reflexivity | exact IH]. Qed.

Lemma nassoc_in : forall V k (l : list (nat * V)) v, nassoc k l = Some v -> In (k, v) l.
Proof.
  intros V k l v. induction l as [|[a w] r IH]; cbn; [discriminate|].
  destruct (Nat.eqb_spec k a) as [->|]; [intros [=<-]; left; reflexivity | intro H; right; exact (IH H)].
Qed.

Lemma nassoc_none : forall V k (l : list (nat * V)), nassoc k l = None -> ~ In k (map fst l).
Proof.
  intros V k l. induction l as [|[a w] r IH]; cbn; [auto|].
  destruct (Nat.eqb_spec k a) as [->|Ne]; [discriminate|]. intros H [E|Hin]; [congruence | exact (IH H Hin)].
Qed.

Lemma aset_absent : forall V k (v : V) l, nassoc k l = None -> aset Nat.eqb k v l = l ++ [(k, v)].
Proof.
  intros V k v l. induction l as [|[a w] r IH]; cbn; [reflexivity|].
  destruct (Nat.eqb_spec k a); [discriminate|]. intro H. rewrite (IH H). reflexivity.
Qed.

(* ======================================================================== *)
(* 2. Triangular merges and their ranks                                      *)
(* ======================================================================== *)

Inductive Tri : list (nat * ety) -> Prop :=
| TriNil : Tri []
| TriSnoc : forall uf g t, Tri uf -> nassoc g uf = None -> ~ In g (ety_gens t) ->
    (forall h, In h (ety_gens t) -> nassoc h uf = None) -> Tri (uf ++ [(g, t)]).

(* a key's rank: how many keys from it to the end, itself included *)
Fixpoint rk (uf : list (nat * ety)) (g : nat) : nat :=
  match uf with [] => 0 | (k, _) :: r => if Nat.eqb g k then S (List.length r) else rk r g end.

Lemma rk_le : forall uf g, rk uf g <= List.length uf.
Proof. intros uf g. induction uf as [|[k v] r IH]; cbn; [lia|]. destruct (Nat.eqb g k); lia. Qed.

Lemma rk_none : forall uf g, nassoc g uf = None -> rk uf g = 0.
Proof.
  intros uf g. induction uf as [|[k v] r IH]; cbn; [auto|]. destruct (Nat.eqb g k); [discriminate | exact IH].
Qed.

Lemma rk_some : forall uf g t, nassoc g uf = Some t -> 1 <= rk uf g.
Proof.
  intros uf g t. induction uf as [|[k v] r IH]; cbn; [discriminate|]. destruct (Nat.eqb g k); [lia | exact IH].
Qed.

Lemma rk_snoc : forall uf g t h, rk (uf ++ [(g, t)]) h =
  match nassoc h uf with Some _ => S (rk uf h) | None => if Nat.eqb h g then 1 else 0 end.
Proof.
  intros uf g t h. induction uf as [|[k v] r IH]; cbn.
  - destruct (Nat.eqb h g); reflexivity.
  - destruct (Nat.eqb h k); [rewrite length_app; cbn; f_equal; lia | exact IH].
Qed.

Definition RkOK (uf : list (nat * ety)) : Prop :=
  forall k t, nassoc k uf = Some t -> forall h, In h (ety_gens t) -> rk uf h < rk uf k.

Lemma tri_rk : forall uf, Tri uf -> RkOK uf.
Proof.
  intros uf T. induction T as [|uf g t T IH Hg Ho Hc]; intros k t0 Hk h Hh; [discriminate|].
  rewrite nassoc_app' in Hk. rewrite !rk_snoc.
  destruct (nassoc k uf) as [t1|] eqn:Ek.
  - injection Hk as <-. pose proof (IH k t1 Ek h Hh) as L.
    destruct (nassoc h uf) as [t2|] eqn:Eh; [lia|]. destruct (Nat.eqb h g); lia.
  - cbn in Hk. destruct (Nat.eqb_spec k g) as [->|]; [|discriminate]. injection Hk as <-.
    rewrite (Hc h Hh). destruct (Nat.eqb_spec h g) as [->|]; [contradiction | lia].
Qed.

(* ======================================================================== *)
(* 3. Resolution: canon's fuel suffices                                      *)
(* ======================================================================== *)

Section Res.
Variable uf : list (nat * ety).
Hypothesis R : RkOK uf.

Definition Need (k : nat) (t : ety) : Prop := forall h, In h (ety_gens t) -> rk uf h < k.

Lemma need_nom : forall k n xs p, Need k (YNom n xs) -> In p xs -> Need k (snd p).
Proof. intros k n xs p N Hp h Hh. apply N, gens_nom. exists p. split; assumption. Qed.
Lemma need_union : forall k ms m, Need k (YUnion ms) -> In m ms -> Need k m.
Proof. intros k ms m N Hp h Hh. apply N, gens_union. exists m. split; assumption. Qed.
Lemma need_rec : forall k fs p, Need k (YRec fs) -> In p fs -> Need k (snd p).
Proof. intros k fs p N Hp h Hh. apply N, gens_rec. exists p. split; assumption. Qed.

Lemma no_self : forall g t, nassoc g uf = Some t -> ety_eqb t (YAbs g) = false.
Proof.
  intros g t H. destruct (ety_eqb t (YAbs g)) eqn:E; [|reflexivity].
  apply ety_eqb_true in E. subst t. pose proof (R g _ H g (or_introl eq_refl)). lia.
Qed.

(* a type without generations is its own resolution *)
Lemma canon_nogens : forall k t, (forall h, ~ In h (ety_gens t)) -> canon_go k uf t = t.
Proof.
  intros [|k] t; [reflexivity|]. cbn [canon_go].
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro N; try reflexivity.
  - exfalso. exact (N g (or_introl eq_refl)).
  - f_equal. rewrite <- (map_id xs) at 2. apply map_ext_in. intros [a y] Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. apply (IH (a, y) Hp). intros h Hh. apply (N h), gens_nom. exists (a, y). split; assumption.
  - f_equal. rewrite <- (map_id ms) at 2. apply map_ext_in. intros y Hp.
    rewrite Forall_forall in IH. apply (IH y Hp). intros h Hh. apply (N h), gens_union. exists y. split; assumption.
  - f_equal. rewrite <- (map_id fs) at 2. apply map_ext_in. intros [a y] Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. apply (IH (a, y) Hp). intros h Hh. apply (N h), gens_rec. exists (a, y). split; assumption.
Qed.

Lemma need0 : forall t, Need 0 t -> forall h, ~ In h (ety_gens t).
Proof. intros t N h Hh. specialize (N h Hh). lia. Qed.

Lemma canon_go_fuel : forall k k' t, Need k t -> Need k' t -> canon_go k uf t = canon_go k' uf t.
Proof.
  induction k as [|k IH]; intros k' t N N'.
  - rewrite (canon_nogens k' t (need0 t N)). reflexivity.
  - destruct k' as [|k']; [rewrite (canon_nogens (S k) t (need0 t N')); reflexivity|].
    cbn [canon_go]. revert N N'.
    induction t as [| |g|n xs IHt|ms IHt|fs IHt] using ety_ind'; intros N N'; try reflexivity.
    + destruct (nassoc g uf) as [t'|] eqn:E; [|reflexivity]. rewrite (no_self g t' E).
      assert (Lg : forall h, In h (ety_gens t') -> rk uf h < rk uf g) by exact (R g t' E).
      specialize (N g (or_introl eq_refl)). specialize (N' g (or_introl eq_refl)).
      apply IH; intros h Hh; specialize (Lg h Hh); lia.
    + f_equal. apply map_ext_in. intros p Hp. f_equal. rewrite Forall_forall in IHt.
      exact (IHt p Hp (need_nom _ _ _ _ N Hp) (need_nom _ _ _ _ N' Hp)).
    + f_equal. apply map_ext_in. intros m Hp. rewrite Forall_forall in IHt.
      exact (IHt m Hp (need_union _ _ _ N Hp) (need_union _ _ _ N' Hp)).
    + f_equal. apply map_ext_in. intros p Hp. f_equal. rewrite Forall_forall in IHt.
      exact (IHt p Hp (need_rec _ _ _ N Hp) (need_rec _ _ _ N' Hp)).
Qed.

Definition Res (t : ety) : ety := canon_go (S (List.length uf)) uf t.

Lemma need_full : forall t, Need (S (List.length uf)) t.
Proof. intros t h _. pose proof (rk_le uf h). lia. Qed.

Lemma res_abs : forall g, Res (YAbs g) = match nassoc g uf with Some t' => Res t' | None => YAbs g end.
Proof.
  intro g. unfold Res at 1. cbn [canon_go]. destruct (nassoc g uf) as [t'|] eqn:E; [|reflexivity].
  rewrite (no_self g t' E). apply canon_go_fuel; [|apply need_full].
  intros h Hh. pose proof (R g t' E h Hh). pose proof (rk_le uf g). lia.
Qed.

Lemma res_nom : forall n xs, Res (YNom n xs) = YNom n (map (fun p => (fst p, Res (snd p))) xs).
Proof. reflexivity. Qed.
Lemma res_union : forall ms, Res (YUnion ms) = YUnion (map Res ms).
Proof. reflexivity. Qed.
Lemma res_rec : forall fs, Res (YRec fs) = YRec (map (fun p => (fst p, Res (snd p))) fs).
Proof. reflexivity. Qed.
Lemma res_unit : Res YUnit = YUnit.
Proof. reflexivity. Qed.
Lemma res_never : Res YNever = YNever.
Proof. reflexivity. Qed.

(* what canon leaves has no merged generation *)
Definition Canon (t : ety) : Prop := forall h, In h (ety_gens t) -> nassoc h uf = None.

Lemma canon_go_canon : forall k t, Need k t -> Canon (canon_go k uf t).
Proof.
  induction k as [|k IH]; intros t N.
  - cbn. intros h Hh. exfalso. exact (need0 t N h Hh).
  - cbn [canon_go]. revert N.
    induction t as [| |g|n xs IHt|ms IHt|fs IHt] using ety_ind'; intros N h Hh; try (cbn in Hh; contradiction).
    + destruct (nassoc g uf) as [t'|] eqn:E.
      * rewrite (no_self g t' E) in Hh. refine (IH t' _ h Hh).
        intros h' Hh'. pose proof (R g t' E h' Hh'). specialize (N g (or_introl eq_refl)). lia.
      * cbn in Hh. destruct Hh as [<-|[]]. exact E.
    + apply gens_nom in Hh as (p & Hp & Hh). apply in_map_iff in Hp as (q & <- & Hq). cbn [snd] in Hh.
      rewrite Forall_forall in IHt. exact (IHt q Hq (need_nom _ _ _ _ N Hq) h Hh).
    + apply gens_union in Hh as (m & Hm & Hh). apply in_map_iff in Hm as (q & <- & Hq).
      rewrite Forall_forall in IHt. exact (IHt q Hq (need_union _ _ _ N Hq) h Hh).
    + apply gens_rec in Hh as (p & Hp & Hh). apply in_map_iff in Hp as (q & <- & Hq). cbn [snd] in Hh.
      rewrite Forall_forall in IHt. exact (IHt q Hq (need_rec _ _ _ N Hq) h Hh).
Qed.

Lemma res_canon : forall t, Canon (Res t).
Proof. intro t. apply canon_go_canon, need_full. Qed.

Lemma canon_fix : forall k t, Canon t -> canon_go k uf t = t.
Proof.
  intros [|k] t; [reflexivity|]. cbn [canon_go].
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro N; try reflexivity.
  - rewrite (N g (or_introl eq_refl)). reflexivity.
  - f_equal. rewrite <- (map_id xs) at 2. apply map_ext_in. intros [a y] Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. apply (IH (a, y) Hp). intros h Hh. apply N, gens_nom. exists (a, y). split; assumption.
  - f_equal. rewrite <- (map_id ms) at 2. apply map_ext_in. intros y Hp.
    rewrite Forall_forall in IH. apply (IH y Hp). intros h Hh. apply N, gens_union. exists y. split; assumption.
  - f_equal. rewrite <- (map_id fs) at 2. apply map_ext_in. intros [a y] Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. apply (IH (a, y) Hp). intros h Hh. apply N, gens_rec. exists (a, y). split; assumption.
Qed.

Lemma res_idem : forall t, Res (Res t) = Res t.
Proof. intro t. unfold Res at 1. apply canon_fix, res_canon. Qed.

End Res.

(* canon is Res *)
Lemma canon_res : forall e t, canon e t = Res (en_uf e) t.
Proof. reflexivity. Qed.

(* ======================================================================== *)
(* 4. The relation's σ                                                        *)
(* ======================================================================== *)

Section Sigma.
Variable Fl : string -> nat.
Notation trF := (tr Fl).

Definition suf (uf : list (nat * ety)) : tsubst :=
  map (fun p => (fst p, trF (Res uf (YAbs (fst p))))) uf.

Lemma lookup_map_keys : forall (f : nat -> ty) g (l : list (nat * ety)),
  lookup_subst g (map (fun p => (fst p, f (fst p))) l) = option_map (fun _ => f g) (nassoc g l).
Proof.
  intros f g l. induction l as [|[k v] r IH]; [reflexivity|]. cbn.
  destruct (Nat.eqb_spec g k) as [->|]; [reflexivity | exact IH].
Qed.

Lemma lookup_suf : forall uf g,
  lookup_subst g (suf uf) = option_map (fun _ => trF (Res uf (YAbs g))) (nassoc g uf).
Proof. intros uf g. unfold suf. exact (lookup_map_keys (fun x => trF (Res uf (YAbs x))) g uf). Qed.

Lemma app_subst_nom : forall s n xs,
  asub s (TNom n xs) = TNom n (map (fun p => (fst p, asub s (snd p))) xs).
Proof. intros s n xs. cbn. f_equal. Qed.

Lemma app_subst_union : forall s ms, asub s (TUnion ms) = TUnion (map (asub s) ms).
Proof. intros s ms. cbn. f_equal. Qed.

Lemma tr_res : forall uf, RkOK uf -> forall t, asub (suf uf) (trF t) = trF (Res uf t).
Proof.
  intros uf R t. induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'.
  - reflexivity.
  - reflexivity.
  - cbn [tr ExecutionModel.app_subst]. rewrite lookup_suf, (res_abs uf R g).
    destruct (nassoc g uf) as [t'|] eqn:E; reflexivity.
  - cbn [tr]. rewrite app_subst_nom, res_nom. cbn [tr]. f_equal. rewrite !map_map. apply map_ext_in.
    intros p Hp. cbn [fst snd]. f_equal. rewrite Forall_forall in IH. exact (IH p Hp).
  - cbn [tr]. rewrite app_subst_union, res_union. cbn [tr]. f_equal. rewrite !map_map. apply map_ext_in.
    intros m Hm. rewrite Forall_forall in IH. exact (IH m Hm).
  - cbn [tr]. rewrite app_subst_nom, res_rec. cbn [tr]. f_equal. rewrite !map_map. apply map_ext_in.
    intros p Hp. cbn [fst snd]. f_equal. rewrite Forall_forall in IH. exact (IH p Hp).
Qed.

Lemma tgens_nom : forall n xs h, In h (A.gens (TNom n xs)) <-> exists p, In p xs /\ In h (A.gens (snd p)).
Proof.
  intros n xs h. cbn [A.gens]. induction xs as [|[a y] r IH]; cbn [snd].
  - split; [intros []| intros (p & [] & _)].
  - rewrite in_app_iff, IH. split.
    + intros [H|(p & Hp & H)]; [exists (a, y); split; [left; reflexivity | exact H] | exists p; split; [right; exact Hp | exact H]].
    + intros (p & [<-|Hp] & H); [left; exact H | right; exists p; auto].
Qed.

Lemma tgens_union : forall ms h, In h (A.gens (TUnion ms)) <-> exists m, In m ms /\ In h (A.gens m).
Proof.
  intros ms h. cbn [A.gens]. induction ms as [|y r IH].
  - split; [intros []| intros (p & [] & _)].
  - rewrite in_app_iff, IH. split.
    + intros [H|(p & Hp & H)]; [exists y; split; [left; reflexivity | exact H] | exists p; split; [right; exact Hp | exact H]].
    + intros (p & [<-|Hp] & H); [left; exact H | right; exists p; auto].
Qed.

(* a substitution leaves alone a type none of whose generations it maps *)
Lemma app_subst_off : forall s t, (forall h, In h (A.gens t) -> lookup_subst h s = None) -> asub s t = t.
Proof.
  intros s t. induction t as [|g|n xs IH|ms IH] using ExecutionTypeFacts.ty_ind'; intro N.
  - reflexivity.
  - cbn. rewrite (N g (or_introl eq_refl)). reflexivity.
  - rewrite app_subst_nom. f_equal. rewrite <- (map_id xs) at 2. apply map_ext_in. intros [a y] Hp.
    cbn [fst snd]. f_equal. rewrite Forall_forall in IH. apply (IH (a, y) Hp).
    intros h Hh. apply N, tgens_nom. exists (a, y). split; assumption.
  - rewrite app_subst_union. f_equal. rewrite <- (map_id ms) at 2. apply map_ext_in. intros y Hp.
    rewrite Forall_forall in IH. apply (IH y Hp). intros h Hh. apply N, tgens_union. exists y. split; assumption.
Qed.

Lemma suf_idem : forall uf, RkOK uf -> forall t, asub (suf uf) (asub (suf uf) t) = asub (suf uf) t.
Proof.
  intros uf R t. induction t as [|g|n xs IH|ms IH] using ExecutionTypeFacts.ty_ind'.
  - reflexivity.
  - cbn [ExecutionModel.app_subst]. destruct (lookup_subst g (suf uf)) as [v|] eqn:E.
    + rewrite lookup_suf in E. destruct (nassoc g uf); [|discriminate]. injection E as <-.
      apply app_subst_off. intros h Hh. rewrite gens_tr in Hh. rewrite lookup_suf.
      rewrite (res_canon uf R (YAbs g) h Hh). reflexivity.
    + cbn [ExecutionModel.app_subst]. rewrite E. reflexivity.
  - rewrite !app_subst_nom, map_map. f_equal. apply map_ext_in. intros p Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. exact (IH p Hp).
  - rewrite !app_subst_union, map_map. f_equal. apply map_ext_in. intros m Hm.
    rewrite Forall_forall in IH. exact (IH m Hm).
Qed.

(* ---- respecting the edges ---- *)

Definition RespE (s : tsubst) (uf : list (nat * ety)) : Prop :=
  forall g t, In (g, t) uf -> asub s (TAbs g) = asub s (trF t).

Lemma resp_app : forall s uf nw, RespE s (uf ++ nw) -> RespE s uf.
Proof. intros s uf nw H g t Hin. apply H, in_or_app. left. exact Hin. Qed.

Lemma resp_canon_go : forall s uf, RespE s uf -> forall k t, asub s (trF (canon_go k uf t)) = asub s (trF t).
Proof.
  intros s uf Hr k. induction k as [|k IH]; intro t; [reflexivity|]. cbn [canon_go].
  induction t as [| |g|n xs IHt|ms IHt|fs IHt] using ety_ind'; try reflexivity.
  - destruct (nassoc g uf) as [t'|] eqn:E; [|reflexivity].
    destruct (ety_eqb t' (YAbs g)); [reflexivity|]. rewrite IH. symmetry. exact (Hr g t' (nassoc_in _ _ _ _ E)).
  - cbn [tr]. rewrite !app_subst_nom, !map_map. f_equal. apply map_ext_in. intros p Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IHt. exact (IHt p Hp).
  - cbn [tr]. rewrite !app_subst_union, !map_map. f_equal. apply map_ext_in. intros m Hm.
    rewrite Forall_forall in IHt. exact (IHt m Hm).
  - cbn [tr]. rewrite !app_subst_nom, !map_map. f_equal. apply map_ext_in. intros p Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IHt. exact (IHt p Hp).
Qed.

Lemma resp_res : forall s uf, RespE s uf -> forall t, asub s (trF (Res uf t)) = asub s (trF t).
Proof. intros s uf H t. exact (resp_canon_go s uf H _ t). Qed.

(* most-generality: whatever respects the edges is unchanged by σ first *)
Lemma resp_mg : forall s uf, RkOK uf -> RespE s uf ->
  forall t, asub s (asub (suf uf) t) = asub s t.
Proof.
  intros s uf R H t. induction t as [|g|n xs IH|ms IH] using ExecutionTypeFacts.ty_ind'.
  - reflexivity.
  - cbn [ExecutionModel.app_subst]. rewrite lookup_suf. destruct (nassoc g uf) eqn:E; [|reflexivity]. cbn [option_map].
    rewrite (resp_res s uf H (YAbs g)). reflexivity.
  - rewrite app_subst_nom, !app_subst_nom, map_map. f_equal. apply map_ext_in. intros p Hp. cbn [fst snd]. f_equal.
    rewrite Forall_forall in IH. exact (IH p Hp).
  - rewrite app_subst_union, !app_subst_union, map_map. f_equal. apply map_ext_in. intros m Hm.
    rewrite Forall_forall in IH. exact (IH m Hm).
Qed.


(* ======================================================================== *)
(* 5. unify                                                                   *)
(* ======================================================================== *)

Definition with_uf (e : env) (u : list (nat * ety)) : env :=
  Env (en_tn e) u (en_vals e) (en_fns e) (en_mths e) (en_funs e) (en_locals e) (en_apps e).

Lemma with_uf_same : forall e, with_uf e (en_uf e) = e.
Proof. intros [? ? ? ? ? ? ? ?]. reflexivity. Qed.

Lemma with_uf_twice : forall e u v, with_uf (with_uf e u) v = with_uf e v.
Proof. reflexivity. Qed.

(* a step of merging: edges only added, the list triangular, nothing else changed *)
Record UStep (e e' : env) : Prop := {
  us_ext : exists nw, en_uf e' = en_uf e ++ nw;
  us_tri : Tri (en_uf e');
  us_same : e' = with_uf e (en_uf e')
}.

Lemma ustep_refl : forall e, Tri (en_uf e) -> UStep e e.
Proof. intros e T. constructor; [exists []; rewrite app_nil_r; reflexivity | exact T | symmetry; apply with_uf_same]. Qed.

Lemma ustep_trans : forall e1 e2 e3, UStep e1 e2 -> UStep e2 e3 -> UStep e1 e3.
Proof.
  intros e1 e2 e3 [(n1 & E1) T1 S1] [(n2 & E2) T2 S2]. constructor; [exists (n1 ++ n2) | exact T2 |].
  - rewrite E2, E1, app_assoc. reflexivity.
  - rewrite S2, S1. reflexivity.
Qed.

Lemma ustep_same_uf : forall e e', UStep e e' -> en_uf e' = en_uf e -> e' = e.
Proof. intros e e' [_ _ S] E. rewrite S, E. apply with_uf_same. Qed.

(* what a merge of a with b leaves *)
Definition UOK (e : env) (a b : ety) (e' : env) : Prop :=
  UStep e e' /\
  (forall sb, RespE sb (en_uf e) -> asub sb (trF a) = asub sb (trF b) -> RespE sb (en_uf e')) /\
  (en_uf e' = en_uf e -> canon e a = canon e b).

Lemma uok_sym : forall e a b e', UOK e a b e' -> UOK e b a e'.
Proof.
  intros e a b e' (U & H1 & H2). split; [exact U|]. split.
  - intros sb Hr E. exact (H1 sb Hr (eq_sym E)).
  - intro E. symmetry. exact (H2 E).
Qed.

Lemma canon_tri : forall e t, Tri (en_uf e) -> Canon (en_uf e) (canon e t).
Proof. intros e t T. rewrite canon_res. exact (res_canon _ (tri_rk _ T) t). Qed.

Lemma len_app_nil : forall A (l nw : list A), l = l ++ nw -> nw = [].
Proof. intros A l nw E. apply (f_equal (@List.length A)) in E. rewrite length_app in E. destruct nw; [reflexivity | cbn in E; lia]. Qed.

Lemma bind_edge : forall e a b g, Tri (en_uf e) -> canon e a = YAbs g -> nmem g (ety_gens (canon e b)) = false ->
  UOK e a b (set_uf e g (canon e b)).
Proof.
  intros e a b g T Ea Hn.
  assert (Ng : nassoc g (en_uf e) = None) by (apply (canon_tri e a T); rewrite Ea; left; reflexivity).
  assert (Ee : en_uf (set_uf e g (canon e b)) = en_uf e ++ [(g, canon e b)])
    by (cbn [set_uf en_uf]; apply aset_absent; exact Ng).
  split; [|split].
  - constructor; [exists [(g, canon e b)]; exact Ee | rewrite Ee | reflexivity].
    apply TriSnoc; [exact T | exact Ng | |].
    + intro Hin. apply nmem_in in Hin. congruence.
    + exact (canon_tri e b T).
  - intros sb Hr E. rewrite Ee. intros h t Hin. apply in_app_iff in Hin as [Hin|[Eq|[]]]; [exact (Hr h t Hin)|].
    injection Eq as <- <-. change (TAbs g) with (trF (YAbs g)). rewrite <- Ea, !canon_res, !(resp_res sb _ Hr). exact E.
  - rewrite Ee. intro E. pose proof (len_app_nil _ _ _ (eq_sym E)). discriminate.
Qed.

Lemma mfold_unify : forall (step : env -> ety * ety -> M env),
  (forall e p s e' s', Tri (en_uf e) -> step e p s = Good (e', s') -> s' = s /\ UOK e (fst p) (snd p) e') ->
  forall l e s e' s', Tri (en_uf e) -> mfold step e l s = Good (e', s') ->
  s' = s /\ UStep e e' /\
  (forall sb, RespE sb (en_uf e) -> Forall (fun p => asub sb (trF (fst p)) = asub sb (trF (snd p))) l ->
     RespE sb (en_uf e')) /\
  (en_uf e' = en_uf e -> Forall (fun p => canon e (fst p) = canon e (snd p)) l).
Proof.
  intros step Hs l. induction l as [|p r IH]; intros e s e' s' T H; cbn [mfold] in H.
  - injection H as <- <-. split; [reflexivity|]. split; [exact (ustep_refl e T)|]. split; [auto|]. intros _. constructor.
  - apply bind_inv in H as (e1 & s1 & H1 & H). destruct (Hs e p s e1 s1 T H1) as (-> & U1 & R1 & C1).
    destruct (IH e1 s e' s' (us_tri _ _ U1) H) as (-> & U2 & R2 & C2).
    split; [reflexivity|]. split; [exact (ustep_trans _ _ _ U1 U2)|]. split.
    + intros sb Hr F. inversion F as [|? ? Hp Hrest]; subst. exact (R2 sb (R1 sb Hr Hp) Hrest).
    + intro E. destruct (us_ext _ _ U1) as (n1 & E1). destruct (us_ext _ _ U2) as (n2 & E2).
      rewrite E2, E1, <- app_assoc in E. pose proof (len_app_nil _ _ _ (eq_sym E)) as N.
      apply app_eq_nil in N as [-> ->]. rewrite app_nil_r in E1, E2.
      pose proof (ustep_same_uf _ _ U1 E1) as ->. constructor; [exact (C1 E1) | exact (C2 E2)].
Qed.

Lemma map_pair_combine : forall X (K : X -> nat) (F : ety -> ty) (xs ys : list (X * ety)),
  map (fun p => (K (fst p), F (snd p))) xs = map (fun p => (K (fst p), F (snd p))) ys ->
  Forall (fun p => F (fst p) = F (snd p)) (combine (map snd xs) (map snd ys)).
Proof.
  intros X K F xs. induction xs as [|[a x] r IH]; intros [|[b y] r'] E; cbn in *; try discriminate; constructor.
  - injection E as _ E _. exact E.
  - injection E as _ _ E. exact (IH r' E).
Qed.

Lemma map_combine : forall (F : ety -> ty) (xs ys : list ety), map F xs = map F ys ->
  Forall (fun p => F (fst p) = F (snd p)) (combine xs ys).
Proof.
  intros F xs. induction xs as [|x r IH]; intros [|y r'] E; cbn in *; try discriminate; constructor.
  - injection E as E _. exact E.
  - injection E as _ E. exact (IH r' E).
Qed.

Lemma combine_eq : forall A (xs ys : list A), List.length xs = List.length ys ->
  Forall (fun p => fst p = snd p) (combine xs ys) -> xs = ys.
Proof.
  intros A xs. induction xs as [|x r IH]; intros [|y r'] L F; cbn in *; try discriminate; [reflexivity|].
  inversion F as [|? ? E Fr]; subst. cbn in E. subst. f_equal. apply IH; [lia | exact Fr].
Qed.

Lemma pairs_eq : forall X (xs ys : list (X * ety)), map fst xs = map fst ys -> map snd xs = map snd ys -> xs = ys.
Proof.
  intros X xs. induction xs as [|[a x] r IH]; intros [|[b y] r'] E1 E2; cbn in *; try discriminate; [reflexivity|].
  injection E1 as -> E1. injection E2 as -> E2. f_equal. exact (IH r' E1 E2).
Qed.

Lemma canon_parts : forall e (xs ys : list ety), Forall (Canon (en_uf e)) xs -> Forall (Canon (en_uf e)) ys ->
  List.length xs = List.length ys -> Forall (fun p => canon e (fst p) = canon e (snd p)) (combine xs ys) -> xs = ys.
Proof.
  intros e xs ys Cx Cy L F. apply combine_eq; [exact L|]. rewrite Forall_forall in *.
  intros [x y] Hin. specialize (F _ Hin). cbn [fst snd] in F |- *. rewrite !canon_res in F.
  unfold Res in F. rewrite !canon_fix in F; [exact F| |].
  - apply Cy. exact (in_combine_r _ _ _ _ Hin).
  - apply Cx. exact (in_combine_l _ _ _ _ Hin).
Qed.

Lemma canon_nom_parts : forall e n xs, Canon (en_uf e) (YNom n xs) -> Forall (Canon (en_uf e)) (map snd xs).
Proof.
  intros e n xs C. apply Forall_forall. intros y Hy. apply in_map_iff in Hy as (p & <- & Hp).
  intros h Hh. apply C, gens_nom. exists p. split; assumption.
Qed.
Lemma canon_union_parts : forall e ms, Canon (en_uf e) (YUnion ms) -> Forall (Canon (en_uf e)) ms.
Proof. intros e ms C. apply Forall_forall. intros y Hy h Hh. apply C, gens_union. exists y. split; assumption. Qed.
Lemma canon_rec_parts : forall e fs, Canon (en_uf e) (YRec fs) -> Forall (Canon (en_uf e)) (map snd fs).
Proof.
  intros e fs C. apply Forall_forall. intros y Hy. apply in_map_iff in Hy as (p & <- & Hp).
  intros h Hh. apply C, gens_rec. exists p. split; assumption.
Qed.

Lemma resp_canon_eq : forall sb e a b, RespE sb (en_uf e) ->
  asub sb (trF a) = asub sb (trF b) -> asub sb (trF (canon e a)) = asub sb (trF (canon e b)).
Proof. intros sb e a b Hr E. rewrite !canon_res, !(resp_res sb _ Hr). exact E. Qed.

Lemma unify_spec : forall f e mi a b why s e' s', Tri (en_uf e) -> unify f e mi a b why s = Good (e', s') ->
  s' = s /\ UOK e a b e'.
Proof.
  induction f as [|f IH]; intros e mi a b why s e' s' T H; [discriminate|].
  cbn [unify] in H.
  assert (Hs : forall e0 p s0 e1 s1, Tri (en_uf e0) -> unify f e0 mi (fst p) (snd p) why s0 = Good (e1, s1) ->
            s1 = s0 /\ UOK e0 (fst p) (snd p) e1) by (intros e0 p s0 e1 s1 T0 H0; exact (IH _ _ _ _ _ _ _ _ T0 H0)).
  pose proof (canon_tri e a T) as Ca. pose proof (canon_tri e b T) as Cb.
  destruct (ety_eqb (canon e a) (canon e b)) eqn:Eab.
  { injection H as <- <-. split; [reflexivity|]. split; [exact (ustep_refl e T)|]. split; [auto|].
    intros _. exact (ety_eqb_true _ _ Eab). }
  (* the common part of the structural cases *)
  assert (Struct : forall (l : list (ety * ety)) e1 s1,
            mfold (fun e p => unify f e mi (fst p) (snd p) why) e l s = Good (e1, s1) ->
            (forall sb, asub sb (trF (canon e a)) = asub sb (trF (canon e b)) ->
               Forall (fun p => asub sb (trF (fst p)) = asub sb (trF (snd p))) l) ->
            (Forall (fun p => canon e (fst p) = canon e (snd p)) l -> canon e a = canon e b) ->
            s1 = s /\ UOK e a b e1).
  { intros l e1 s1 Hm Hl Hc. destruct (mfold_unify _ Hs l e s e1 s1 T Hm) as (-> & U & R1 & C1).
    split; [reflexivity|]. split; [exact U|]. split.
    - intros sb Hr E. exact (R1 sb Hr (Hl sb (resp_canon_eq sb e a b Hr E))).
    - intro E. exact (Hc (C1 E)). }
  remember (canon e a) as ca eqn:Ea. remember (canon e b) as cb eqn:Eb. symmetry in Ea, Eb.
  destruct ca as [| |g|n xs|ms|fs].
  3: { cbn beta iota zeta in H. destruct (nmem g (ety_gens cb)) eqn:Hn; [noerr H|]. injection H as <- <-.
       split; [reflexivity|]. rewrite <- Eb in Hn |- *. exact (bind_edge e a b g T Ea Hn). }
  all: destruct cb as [| |g'|n' ys|ms'|fs']; cbn beta iota zeta in H; try noerr H.
  all: try (match type of H with context [nmem ?g (ety_gens ?t)] =>
              destruct (nmem g (ety_gens t)) eqn:Hn; [noerr H|]; injection H as <- <-; split; [reflexivity|];
              rewrite <- Ea in Hn |- *; apply uok_sym; exact (bind_edge e b a g T Eb Hn) end).
  - (* nominal *)
    destruct (Nat.eqb_spec n n') as [<-|]; [|noerr H].
    destruct (list_eq_dec Nat.eq_dec (map fst xs) (map fst ys)) as [Ek|]; [|noerr H].
    apply (Struct _ _ _ H).
    + intros sb E. cbn [tr] in E. rewrite !app_subst_nom, !map_map in E. injection E as E.
      exact (map_pair_combine _ (fun x => x) (fun y => asub sb (trF y)) xs ys E).
    + intro F. f_equal. apply pairs_eq; [exact Ek|].
      apply (canon_parts e); [exact (canon_nom_parts e n xs Ca) | exact (canon_nom_parts e n ys Cb) | | exact F].
      rewrite !length_map. apply (f_equal (@List.length _)) in Ek. rewrite !length_map in Ek. exact Ek.
  - (* union *)
    destruct (Nat.eqb_spec (List.length ms) (List.length ms')) as [L|]; [|noerr H].
    apply (Struct _ _ _ H).
    + intros sb E. cbn [tr] in E. rewrite !app_subst_union, !map_map in E. injection E as E.
      exact (map_combine (fun y => asub sb (trF y)) ms ms' E).
    + intro F. f_equal.
      exact (canon_parts e ms ms' (canon_union_parts e ms Ca) (canon_union_parts e ms' Cb) L F).
  - (* record *)
    destruct (list_eq_dec string_dec (map fst fs) (map fst fs')) as [Ek|]; [|noerr H].
    apply (Struct _ _ _ H).
    + intros sb E. cbn [tr] in E. rewrite !app_subst_nom, !map_map in E. injection E as E.
      exact (map_pair_combine _ Fl (fun y => asub sb (trF y)) fs fs' E).
    + intro F. f_equal. apply pairs_eq; [exact Ek|].
      apply (canon_parts e); [exact (canon_rec_parts e fs Ca) | exact (canon_rec_parts e fs' Cb) | | exact F].
      rewrite !length_map. apply (f_equal (@List.length _)) in Ek. rewrite !length_map in Ek. exact Ek.
Qed.

Lemma ustep_back : forall e e1 e2, UStep e e1 -> UStep e1 e2 -> en_uf e2 = en_uf e -> en_uf e1 = en_uf e /\ en_uf e2 = en_uf e1.
Proof.
  intros e e1 e2 U1 U2 E. destruct (us_ext _ _ U1) as (n1 & E1). destruct (us_ext _ _ U2) as (n2 & E2).
  rewrite E2, E1, <- app_assoc in E. pose proof (len_app_nil _ _ _ (eq_sym E)) as N.
  apply app_eq_nil in N as [-> ->]. rewrite app_nil_r in E1, E2. split; [exact E1 | exact E2].
Qed.

Definition SigResp (sb : tsubst) (a b : Elab.fsig) : Prop :=
  Forall (fun p => asub sb (trF (fst p)) = asub sb (trF (snd p))) (combine (map snd (sg_params a)) (map snd (sg_params b))) /\
  asub sb (trF (sg_ret a)) = asub sb (trF (sg_ret b)) /\
  match sg_this a, sg_this b with Some x, Some y => asub sb (trF x) = asub sb (trF y) | _, _ => True end.

Lemma unify_sig_spec : forall e mi a b nm s e' s', Tri (en_uf e) -> unify_sig e mi a b nm s = Good (e', s') ->
  s' = s /\ UStep e e' /\ List.length (sg_params a) = List.length (sg_params b) /\
  (forall sb, RespE sb (en_uf e) -> SigResp sb a b -> RespE sb (en_uf e')) /\
  (en_uf e' = en_uf e ->
     Forall (fun p => canon e (fst p) = canon e (snd p)) (combine (map snd (sg_params a)) (map snd (sg_params b))) /\
     canon e (sg_ret a) = canon e (sg_ret b)).
Proof.
  intros e mi a b nm s e' s' T H. unfold unify_sig in H.
  destruct (Nat.eqb_spec (List.length (sg_params a)) (List.length (sg_params b))) as [L|]; [|noerr H].
  cbn [negb] in H. apply bind_inv in H as (e1 & s1 & H1 & H). apply bind_inv in H as (e2 & s2 & H2 & H).
  destruct (mfold_unify (fun e0 p => unify 200 e0 mi (fst p) (snd p) (" for `" ++ nm ++ "`")%string)
              (fun e0 p s0 e3 s3 T0 H0 => unify_spec 200 e0 mi (fst p) (snd p) _ s0 e3 s3 T0 H0) _ e s e1 s1 T H1)
    as (-> & U1 & R1 & C1).
  destruct (unify_spec _ _ _ _ _ _ _ _ _ (us_tri _ _ U1) H2) as (-> & U2 & R2 & C2).
  assert (Last : s' = s /\ UStep e2 e' /\
            (forall sb, RespE sb (en_uf e2) -> match sg_this a, sg_this b with Some x, Some y => asub sb (trF x) = asub sb (trF y) | _, _ => True end -> RespE sb (en_uf e'))).
  { destruct (sg_this a) as [x|], (sg_this b) as [y|];
      try (injection H as <- <-; split; [reflexivity|]; split; [exact (ustep_refl _ (us_tri _ _ U2)) | auto]).
    destruct (unify_spec _ _ _ _ _ _ _ _ _ (us_tri _ _ U2) H) as (-> & U3 & R3 & _). auto. }
  destruct Last as (-> & U3 & R3).
  split; [reflexivity|]. split; [exact (ustep_trans _ _ _ (ustep_trans _ _ _ U1 U2) U3)|]. split; [exact L|]. split.
  - intros sb Hr (P1 & P2 & P3). apply R3; [|exact P3]. apply R2; [exact (R1 sb Hr P1)|]. exact P2.
  - intro E. destruct (ustep_back _ _ _ (ustep_trans _ _ _ U1 U2) U3 E) as [E12 _].
    destruct (ustep_back _ _ _ U1 U2 E12) as [E1 E2].
    pose proof (ustep_same_uf _ _ U1 E1) as ->. split; [exact (C1 E1) | exact (C2 E2)].
Qed.

(* Δ4: the instantiations unify_app equates, read by tr *)
Definition InstResp (sb : tsubst) (old a : Elab.inst) : Prop :=
  Forall (fun p => asub sb (trF (fst p)) = asub sb (trF (snd p))) (combine (map inst_ety old) (map inst_ety a)).
Definition InstCanon (e : env) (old a : Elab.inst) : Prop :=
  map fst old = map fst a /\ Forall (fun p => canon e (fst p) = canon e (snd p)) (combine (map inst_ety old) (map inst_ety a)).

Lemma unify_app_spec : forall e mi k a nm s e' s', Tri (en_uf e) -> unify_app e mi k a nm s = Good (e', s') ->
  s' = s /\ UStep e e' /\ (get_app e k = None -> e' = e) /\
  (forall old st, get_app e k = Some (old, st) ->
     forallb (fun q => ety_okb (inst_ety q)) (old ++ a) = true /\
     (forall sb, RespE sb (en_uf e) -> InstResp sb old a -> RespE sb (en_uf e')) /\
     (en_uf e' = en_uf e -> InstCanon e old a) /\ map fst old = map fst a).
Proof.
  intros e mi k a nm s e' s' T H. unfold unify_app in H. destruct (get_app e k) as [[old st]|] eqn:G.
  - destruct (list_eq_dec Nat.eq_dec (map fst old) (map fst a)) as [Ed|]; [|noerr H].
    destruct (forallb (fun q => ety_okb (inst_ety q)) (old ++ a)) eqn:Ok; [|noerr H].
    destruct (mfold_unify _ (fun e0 p s0 e3 s3 T0 H0 => unify_spec 200 e0 mi (fst p) (snd p) _ s0 e3 s3 T0 H0) _ e s e' s' T H)
      as (-> & U & R1 & C1).
    split; [reflexivity|]. split; [exact U|]. split; [discriminate|].
    intros old' st' E. injection E as <- <-. split; [exact Ok|]. split; [exact R1|]. split; [|exact Ed].
    intro E. split; [exact Ed | exact (C1 E)].
  - injection H as <- <-. split; [reflexivity|]. split; [exact (ustep_refl e T)|]. split; [reflexivity|]. discriminate.
Qed.

End Sigma.
