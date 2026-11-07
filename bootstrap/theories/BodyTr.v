(* ------------------------------------------------------------------------ *)
(* BodyTr.v — elaborated types as Part V's types (Track A): tr, the types  *)
(* the elaborator makes (WFty), the receivers tr reads apart (ety_ok), and *)
(* canon without merges.  Split from BodySound.v so that Merge.v, which    *)
(* BodySound's CxRel reads (canonical types under the merges), comes       *)
(* between them.                                                            *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab.
From NM Require CoreMoss.
Import ListNotations.
Local Open Scope list_scope.

Module A := Audit.Elaboration.

(* ======================================================================== *)
(* 1. Elaborated types as Part V's types                                     *)
(* ======================================================================== *)

(* An induction principle for ety through its nested lists. *)
Section EtyInd.
  Variable Q : ety -> Prop.
  Hypothesis HU : Q YUnit.
  Hypothesis HN : Q YNever.
  Hypothesis HA : forall g, Q (YAbs g).
  Hypothesis HNom : forall n xs, Forall (fun p => Q (snd p)) xs -> Q (YNom n xs).
  Hypothesis HUn : forall ms, Forall Q ms -> Q (YUnion ms).
  Hypothesis HRec : forall fs, Forall (fun p => Q (snd p)) fs -> Q (YRec fs).

  Fixpoint ety_ind' (t : ety) : Q t :=
    match t with
    | YUnit => HU
    | YNever => HN
    | YAbs g => HA g
    | YNom n xs => HNom n xs ((fix go (l : list (nat * ety)) : Forall (fun p => Q (snd p)) l :=
                                 match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' (snd p)) (go r) end) xs)
    | YUnion ms => HUn ms ((fix go (l : list ety) : Forall Q l :=
                             match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' p) (go r) end) ms)
    | YRec fs => HRec fs ((fix go (l : list (string * ety)) : Forall (fun p => Q (snd p)) l :=
                            match l with [] => Forall_nil _ | p :: r => Forall_cons _ (ety_ind' (snd p)) (go r) end) fs)
    end.
End EtyInd.

Section Tr.
Variable Fl : string -> nat.

Fixpoint tr (t : ety) : ty :=
  match t with
  | YUnit => TUnit
  | YNever => TUnion []
  | YAbs g => TAbs g
  | YNom n xs => TNom n (map (fun p => (fst p, tr (snd p))) xs)
  | YUnion ms => TUnion (map tr ms)
  | YRec fs => TNom A.rec_head (map (fun p => (Fl (fst p), tr (snd p))) fs)
  end.

Lemma ety_eqb_true : forall a b, ety_eqb a b = true -> a = b.
Proof.
  induction a as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intros [| |g'|n' ys|ms'|fs'] H; cbn in H;
    try discriminate; try reflexivity.
  - apply Nat.eqb_eq in H. subst. reflexivity.
  - apply andb_true_iff in H as [En H]. apply Nat.eqb_eq in En. subst n'. f_equal.
    revert ys H. induction IH as [|[s t] r Ht Hr IHr]; intros [|[s' t'] ys] H; cbn in H; try (cbn in Ht); try discriminate; [reflexivity|].
    apply andb_true_iff in H as [H Hr']. apply andb_true_iff in H as [Es Et].
    apply Nat.eqb_eq in Es. subst. rewrite (Ht t' Et). f_equal. exact (IHr ys Hr').
  - f_equal. revert ms' H. induction IH as [|t r Ht Hr IHr]; intros [|t' ms'] H; cbn in H; try discriminate; [reflexivity|].
    apply andb_true_iff in H as [Et Hr']. rewrite (Ht t' Et). f_equal. exact (IHr ms' Hr').
  - f_equal. revert fs' H. induction IH as [|[s t] r Ht Hr IHr]; intros [|[s' t'] fs'] H; cbn in H; try (cbn in Ht); try discriminate; [reflexivity|].
    apply andb_true_iff in H as [H Hr']. apply andb_true_iff in H as [Es Et].
    apply String.eqb_eq in Es. subst. rewrite (Ht t' Et). f_equal. exact (IHr fs' Hr').
Qed.

Lemma ety_mem_in : forall t l, ety_mem t l = true -> In t l.
Proof.
  intros t l. induction l as [|x r IH]; cbn; [discriminate|].
  intro H. apply orb_true_iff in H as [H|H]; [left; symmetry; exact (ety_eqb_true _ _ H) | right; exact (IH H)].
Qed.

Definition tr_mut (m : Elab.lmut) : A.lmut :=
  match m with Elab.LLet => A.LLet | Elab.LVar => A.LVar | Elab.LCaptured => A.LCaptured end.

Lemma members_tr : forall t, A.members (tr t) = map tr (Elab.members t).
Proof. intros [| | | | |]; reflexivity. Qed.

(* The types the elaborator makes.  `|` as it writes it: no type it makes
   is a union of nothing, anywhere in it (elab_type, Audit note 42(d)), so
   is_never and the relation's never agree on its types and on the types
   inside them (a record's fields, a tag's arguments: BodyData.v); and a
   union's members are tag or unit applications ([D15]: elab_type, and
   substitution keeps them so), so a `match` reads its heads as the
   relation does (BodyData.v). *)
Fixpoint ety_wf (t : ety) : bool :=
  match t with
  | YUnion [] => false
  | YUnion ms => forallb (fun m => match m with YNom _ _ => ety_wf m | _ => false end) ms
  | YNom _ xs => forallb (fun p => ety_wf (snd p)) xs
  | YRec fs => forallb (fun p => ety_wf (snd p)) fs
  | _ => true
  end.

Definition WFty (t : ety) : Prop := ety_wf t = true.

Lemma never_tr : forall t, WFty t -> (tr t = A.never <-> t = YNever).
Proof.
  intros t W. unfold A.never. split; intro H; [|subst; reflexivity].
  destruct t as [| | |n xs|ms|fs]; cbn in H; try discriminate; [reflexivity|].
  destruct ms as [|m r]; [discriminate W | discriminate].
Qed.

Lemma is_never_tr : forall t, WFty t -> is_never t = ty_eqb (tr t) A.never.
Proof.
  intros t W. destruct (is_never t) eqn:E1, (ty_eqb (tr t) A.never) eqn:E2; try reflexivity.
  - exfalso. destruct t; try discriminate; cbn in E2; discriminate.
  - exfalso. apply CoreMoss.ty_eqb_eq in E2. apply (never_tr t W) in E2. subst. discriminate.
Qed.

(* D17: what the elaborator's fits accepts (without merges), the relation's
   fits accepts. *)
Lemma fits_raw_tr : forall t x, fits_raw t x = true -> A.fits (tr t) (tr x).
Proof.
  intros t x H. unfold fits_raw in H. apply orb_true_iff in H as [H|H]; [apply orb_true_iff in H as [H|H]|].
  - left. rewrite (ety_eqb_true _ _ H). reflexivity.
  - right; left. destruct t; try discriminate. reflexivity.
  - destruct x as [| | | |ms|]; try discriminate. right; right. exists (map tr ms). split; [reflexivity|].
    intros m Hm. rewrite members_tr in Hm. apply in_map_iff in Hm as (m0 & <- & Hm0).
    rewrite forallb_forall in H. apply in_map. apply ety_mem_in. exact (H m0 Hm0).
Qed.

Lemma opt_fits_tr : forall t exp, match exp with Some x => fits_raw t x | None => true end = true ->
  A.opt_fits (tr t) (option_map tr exp).
Proof. intros t [x|] H; cbn; [exact (fits_raw_tr t x H) | exact I]. Qed.

End Tr.

(* A type whose reading by tr no other such type shares (tr_inj, CtxMerge.v):
   no nominal headed by the records' reserved 0, no union of nothing.  The
   receivers methods are filed under are such types (CtxWalk.WS). *)
Fixpoint ety_ok (t : ety) : bool :=
  match t with
  | YNom h xs => negb (Nat.eqb h 0) && forallb (fun p => ety_ok (snd p)) xs
  | YUnion ms => match ms with [] => false | _ =>
      forallb (fun m => match m with YNom _ _ => ety_ok m | _ => false end) ms end
  | YRec fs => forallb (fun p => ety_ok (snd p)) fs
  | _ => true
  end.

Lemma ety_ok_wf : forall t, ety_ok t = true -> WFty t.
Proof.
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; intro H; try reflexivity.
  - cbn [ety_ok] in H. apply andb_true_iff in H as [_ H]. unfold WFty. cbn [ety_wf].
    rewrite forallb_forall in H |- *. intros p Hp. rewrite Forall_forall in IH.
    exact (IH p Hp (H p Hp)).
  - destruct ms as [|m ms]; [discriminate H|]. unfold WFty. cbn [ety_wf].
    cbn [ety_ok] in H. rewrite forallb_forall in H |- *. intros q Hq.
    pose proof (H q Hq) as Hn. rewrite Forall_forall in IH.
    destruct q as [| |g|n xs|qs|fs]; try discriminate Hn.
    exact (IH (YNom n xs) Hq Hn).
  - cbn [ety_ok] in H. unfold WFty. cbn [ety_wf].
    rewrite forallb_forall in H |- *. intros p Hp. rewrite Forall_forall in IH.
    exact (IH p Hp (H p Hp)).
Qed.

(* canon without merges is the identity *)
Lemma canon_go_nil : forall fuel t, canon_go fuel [] t = t.
Proof.
  intros [|f] t; [reflexivity|]. cbn [canon_go].
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; try reflexivity.
  - f_equal. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn [map fst snd] in *. rewrite Hy. f_equal. exact IHr.
  - f_equal. induction IH as [|y r Hy Hr IHr]; [reflexivity|]. cbn [map] in *. rewrite Hy. f_equal. exact IHr.
  - f_equal. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn [map fst snd] in *. rewrite Hy. f_equal. exact IHr.
Qed.

Lemma canon_nil : forall e t, en_uf e = [] -> canon e t = t.
Proof. intros e t H. unfold canon. rewrite H. apply canon_go_nil. Qed.

Lemma fits_nil : forall e t x, en_uf e = [] -> fits e t x = fits_raw t x.
Proof. intros e t x H. unfold fits. rewrite !canon_nil by exact H. reflexivity. Qed.


Section Gens.
Variable Fl : string -> nat.
Notation trF := (tr Fl).

Lemma gens_tr : forall t, A.gens (trF t) = ety_gens t.
Proof.
  induction t as [| |g|n xs IH|ms IH|fs IH] using ety_ind'; try reflexivity.
  - cbn. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
  - cbn. induction IH as [|y r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
  - cbn. induction IH as [|[a y] r Hy Hr IHr]; [reflexivity|]. cbn in *. rewrite Hy. f_equal. exact IHr.
Qed.

End Gens.

Lemma nmem_in : forall x l, nmem x l = true <-> In x l.
Proof.
  intros x l. induction l as [|y r IH]; cbn; [split; [discriminate | intros []]|].
  rewrite orb_true_iff, IH, Nat.eqb_eq. split; intros [H|H]; auto.
Qed.
