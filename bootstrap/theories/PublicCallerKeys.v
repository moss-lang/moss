(* The caller-keyed result install (Audit.result_selection), static and
   list facts. An application files position i of F's declared result
   under the caller's key for it, read from the root's source for i
   through each copy's positions (chain_sources). Positions are
   context-independent: two readings of one declared result have entries
   of the same kinds (same_kind_entries), so the same number of keys, a
   key at the same position in each (provide_result_length). *)
From Stdlib Require Import String List Arith Bool Lia.
From NM Require Import Audit PublicTypeRead PublicCallRealize.
From NM Require PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

(* ---- two readings of one result have entries of the same kinds ------------- *)

Definition SameKind (e1 e2 : A.entry) : Prop :=
  match e1, e2 with
  | A.NTy _, A.NTy _ | A.NVal _ _ _, A.NVal _ _ _ | A.NFn _ _ _, A.NFn _ _ _
  | A.NMth _ _ _ _, A.NMth _ _ _ _ | A.NFun _ _, A.NFun _ _ => True
  | _, _ => False
  end.

Section Kinds.
Variable W : A.world.
Import A.

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  repeat match goal with
  | H1 : w_decl W ?x = Some ?a, H2 : w_decl W ?x = Some ?b |- _ =>
      assert_fails (constr_eq a b); rewrite H1 in H2; injection H2 as H2; subst b
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Ltac flat := repeat match goal with
  | H : _ \/ _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  | H : exists _, _ |- _ => destruct H end.

Theorem same_kind_entries :
  (forall p ob1 tm1 sp es1 tm1', OutputItemEntries W p ob1 tm1 sp es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' -> Forall2 SameKind es1 es2) /\
  (forall p ob1 tm1 sps es1 tm1', OutputItemsEntries W p ob1 tm1 sps es1 tm1' ->
     forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' -> Forall2 SameKind es1 es2).
Proof.
  apply (PublicFormation.oies_mut W
    (fun p ob1 tm1 sp es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemEntries W p ob2 tm2 sp es2 tm2' ->
       Forall2 SameKind es1 es2)
    (fun p ob1 tm1 sps es1 tm1' _ => forall ob2 tm2 es2 tm2', OutputItemsEntries W p ob2 tm2 sps es2 tm2' ->
       Forall2 SameKind es1 es2)).
  - intros p ob tm pa a bs t R Kd B ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - intros p ob tm pa a bs s k R Kd B Kk ob2 tm2 es2 tm2' D2.
    inversion D2; subst; first [apply Forall2_nil|flat; kmis].
  - intros p ob tm a pa s d n t ty0 bs ap R Dd Ast B T App ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - intros p ob tm a pa s g bs ap d rcv n ps rt R Kd Dd Ast B Sig App ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - intros p ob tm a bs pa u' d n ps args res fx R Dd Ast B Fx Rd ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sig App ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis). repeat constructor.
  - intros p ob tm a pa c d n its bs es tm' R Dd Ast B Inner IH ob2 tm2 es2 tm2' D2.
    inversion D2; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?s' |- _ => assert_fails (constr_eq s' c); assert (s' = c) by congruence; subst s' end.
    match goal with H : w_decl W c = Some ?d' |- _ => assert_fails (constr_eq d' d); assert (d' = d) by congruence; subst d' end.
    rewrite Ast in *. match goal with H : ADCtx _ _ = ADCtx _ _ |- _ => injection H as <- <- end.
    match goal with H : OutputItemsEntries W _ _ _ its ?es2 _ |- _ => exact (IH _ _ _ _ H) end.
  - intros p ob tm ob2 tm2 es2 tm2' D2. inversion D2; subst. constructor.
  - intros p ob tm sp rest es1 tm1 es2 tm2 D1 IH1 Ds IHs ob2 tm2x es2x tm2x' D2.
    inversion D2; subst.
    match goal with H1 : OutputItemEntries W p ob2 tm2x sp ?a ?b, H2 : OutputItemsEntries W p ob2 ?b rest ?c ?e |- _ =>
      exact (Forall2_app (IH1 _ _ _ _ H1) (IHs _ _ _ _ H2)) end.
Qed.

(* Two readings of u's declared result: their entries are of the same
   kinds, position by position. *)
Lemma provide_result_kinds : forall u C1 o1 C1' ks1 C2 o2 C2' ks2,
  ProvideResult W u C1 o1 C1' ks1 -> ProvideResult W u C2 o2 C2' ks2 ->
  exists es1 es2, ks1 = flat_map (fun e => match entry_key (c_sub C1) e with Some k => [k] | None => [] end) es1 /\
    ks2 = flat_map (fun e => match entry_key (c_sub C2) e with Some k => [k] | None => [] end) es2 /\
    Forall2 SameKind es1 es2.
Proof.
  intros u C1 o1 C1' ks1 C2 o2 C2' ks2 (d & ps & a & r & b & outs & es1 & tm1 & D & FP & _ & E1 & K1 & _)
    (d' & ps' & a' & r' & b' & outs' & es2 & tm2 & D' & FP' & _ & E2 & K2 & _).
  rewrite D in D'. injection D' as <-. rewrite FP in FP'. injection FP' as <- <- <- <-.
  exists es1, es2. split; [exact K1|]. split; [exact K2|].
  exact (proj2 same_kind_entries _ _ _ _ _ _ E1 _ _ _ _ E2).
Qed.

End Kinds.

Lemma same_kind_keys : forall s1 s2 es1 es2, Forall2 SameKind es1 es2 ->
  Forall2 (fun k1 k2 => match k1, k2 with
    | IMth _ _ _, IMth _ _ _ | IVal _, IVal _ | IFn _, IFn _ | IFun _, IFun _ => True
    | _, _ => False end)
    (flat_map (fun e => match A.entry_key s1 e with Some k => [k] | None => [] end) es1)
    (flat_map (fun e => match A.entry_key s2 e with Some k => [k] | None => [] end) es2).
Proof.
  intros s1 s2 es1 es2 F. induction F as [|e1 e2 l1 l2 Sk _ IH]; [constructor|]. cbn [flat_map].
  destruct e1, e2; cbn in Sk; try contradiction; cbn; try exact IH; repeat constructor; exact IH.
Qed.

Lemma provide_result_length : forall W u C1 o1 C1' ks1 C2 o2 C2' ks2,
  A.ProvideResult W u C1 o1 C1' ks1 -> A.ProvideResult W u C2 o2 C2' ks2 -> List.length ks1 = List.length ks2.
Proof.
  intros W u C1 o1 C1' ks1 C2 o2 C2' ks2 P1 P2.
  destruct (provide_result_kinds W u C1 o1 C1' ks1 C2 o2 C2' ks2 P1 P2) as (es1 & es2 & -> & -> & F).
  exact (Forall2_length (same_kind_keys _ _ _ _ F)).
Qed.

(* ---- result keys carry no instantiation ----------------------------------- *)

Lemma result_key_id : forall W u C o C' ks k, A.ProvideResult W u C o C' ks -> In k ks -> A.provision_id k = k.
Proof.
  intros W u C o C' ks k (d & ps & a & r & b & outs & es & tm & _ & _ & _ & _ & -> & _) Ik.
  apply in_flat_map in Ik as [e [_ Ik]].
  destruct e; cbn in Ik; try contradiction; destruct Ik as [<-|[]]; reflexivity.
Qed.

Lemma rkey_of_inj : forall C k1 k2, A.rkey_of C k1 = A.rkey_of C k2 -> A.provision_id k1 = A.provision_id k2.
Proof.
  intros C [t1|v1|f1|r1 m1 i1|u1] [t2|v2|f2|r2 m2 i2|u2]; cbn; unfold A.rkey; intros E; try discriminate; try exact E.
  injection E as -> -> _. reflexivity.
Qed.

(* ---- chain_sources and the selection ---------------------------------------- *)

Lemma chain_sources_snoc : forall rms rm srcs,
  chain_sources (rms ++ [rm]) srcs = map (fun j => nth j (chain_sources rms srcs) (ITy 0)) rm.
Proof. intros rms rm srcs. unfold chain_sources. rewrite fold_left_app. reflexivity. Qed.

(* A copy's positions in range of its source's positions. *)
Definition PositionsIn (rm : list nat) (n m : nat) : Prop :=
  List.length rm = n /\ forall j, In j rm -> j < m.

Lemma map_nth_in : forall (s : list item) rm, (forall j, In j rm -> j < List.length s) ->
  forall x, In x (map (fun j => nth j s (ITy 0)) rm) -> In x s.
Proof.
  intros s rm Rg x Ix. apply in_map_iff in Ix as [j [<- Ij]]. apply nth_In. exact (Rg j Ij).
Qed.

(* The selection's lookup: a later position wins. *)
Lemma sm_find_rev_combine : forall (rs ss : list item) K s, sm_find K (rev (combine rs ss)) = Some s ->
  exists i, nth_error rs i = Some K /\ nth_error ss i = Some s /\
    forall i', i < i' -> nth_error rs i' = Some K -> nth_error ss i' = None.
Proof.
  intros rs ss K s. revert ss. induction rs as [|r rs IH] using rev_ind; intros ss F; [cbn in F; discriminate|].
  destruct (Nat.lt_ge_cases (length rs) (length ss)) as [Lt|Ge].
  - destruct (nth_error ss (length rs)) as [sv|] eqn:Es.
    2: { apply nth_error_None in Es. lia. }
    assert (Ec : combine (rs ++ [r]) ss = combine rs (firstn (length rs) ss) ++ [(r, sv)]).
    { clear -Es Lt. revert ss Es Lt. induction rs as [|r0 rs IH]; intros [|s0 ss] Es Lt; cbn in *; try lia.
      - injection Es as ->. destruct ss; reflexivity.
      - f_equal. apply IH; [exact Es|lia]. }
    rewrite Ec, rev_app_distr in F. cbn [rev app sm_find] in F.
    destruct (item_eqb K r) eqn:Er.
    + apply item_eqb_spec in Er. subst r. injection F as <-.
      exists (length rs). rewrite nth_error_app2, Nat.sub_diag by lia. split; [reflexivity|]. split; [exact Es|].
      intros i' Li Ni. rewrite nth_error_app2 in Ni by lia.
      destruct (i' - length rs) as [|[|q]] eqn:Q; [lia|discriminate|cbn in Ni; destruct q; discriminate].
    + destruct (IH (firstn (length rs) ss) F) as [i [Ri [Si Last]]].
      assert (Li : i < length rs) by (apply nth_error_Some; rewrite Ri; discriminate).
      exists i. rewrite nth_error_app1 by exact Li. split; [exact Ri|].
      split; [rewrite nth_error_firstn in Si; destruct (i <? length rs); [exact Si|discriminate]|].
      intros i' Lt' Ni. destruct (Nat.lt_ge_cases i' (length rs)) as [L'|G'].
      * rewrite nth_error_app1 in Ni by exact L'. pose proof (Last i' Lt' Ni) as N.
        rewrite nth_error_firstn in N. apply Nat.ltb_lt in L'. rewrite L' in N. exact N.
      * rewrite nth_error_app2 in Ni by exact G'.
        destruct (i' - length rs) as [|q] eqn:Q.
        -- cbn in Ni. injection Ni as Ni. subst r. exfalso. assert (T : item_eqb K K = true) by (apply item_eqb_spec; reflexivity). congruence.
        -- destruct q; discriminate.
  - assert (Ec : combine (rs ++ [r]) ss = combine rs ss).
    { clear -Ge. revert ss Ge. induction rs as [|r0 rs IH]; intros [|s0 ss] Ge; cbn in *; try reflexivity; try lia.
      f_equal. apply IH. lia. }
    rewrite Ec in F. destruct (IH ss F) as [i [Ri [Si Last]]].
    assert (Li : i < length rs) by (apply nth_error_Some; rewrite Ri; discriminate).
    exists i. rewrite nth_error_app1 by exact Li. split; [exact Ri|]. split; [exact Si|].
    intros i' Lt' Ni. apply nth_error_None. destruct (Nat.lt_ge_cases i' (length rs)) as [L'|G'].
    + rewrite nth_error_app1 in Ni by exact L'. apply nth_error_None. exact (Last i' Lt' Ni).
    + lia.
Qed.

Lemma sm_find_rev_combine_none : forall (rs ss : list item) K, sm_find K (rev (combine rs ss)) = None ->
  forall i, nth_error rs i = Some K -> nth_error ss i = None.
Proof.
  intros rs ss K F i Ri. destruct (nth_error ss i) as [s|] eqn:Si; [|reflexivity]. exfalso.
  assert (I : In (K, s) (rev (combine rs ss))).
  { apply (proj1 (in_rev _ _)). clear F. revert ss i Ri Si. induction rs as [|r rs IH]; intros [|s0 ss] [|i] Ri Si; cbn in *; try discriminate.
    - injection Ri as ->. injection Si as ->. left. reflexivity.
    - right. exact (IH ss i Ri Si). }
  revert F I. generalize (rev (combine rs ss)). intros l. induction l as [|[a b] l IH]; intros F I; [exact I|].
  cbn in F. destruct (item_eqb K a) eqn:E; [discriminate|]. destruct I as [Eq|I].
  - injection Eq as -> ->. assert (T : item_eqb K K = true) by (apply item_eqb_spec; reflexivity). congruence.
  - exact (IH F I).
Qed.

Lemma in_combine_nth : forall (rs ss : list item) a b, In (a, b) (combine rs ss) ->
  exists i, nth_error rs i = Some a /\ nth_error ss i = Some b.
Proof.
  induction rs as [|r rs IH]; intros [|s ss] a b I; cbn in I; try contradiction.
  destruct I as [E|I]; [injection E as <- <-; exists 0; split; reflexivity|].
  destruct (IH ss a b I) as [i [Ri Si]]. exists (S i). split; assumption.
Qed.

(* ---- a result's new provisions and its keys --------------------------------- *)

Lemma provide_result_key_news : forall W u C out C' ks k, A.ProvideResult W u C out C' ks -> In k ks ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list -> exists p, In (k, p) news.
Proof.
  intros W u C out C' ks k (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & RK & ->) Ik news En.
  cbn [A.output_cx A.c_provisions] in En. apply app_inv_tail in En. subst news.
  unfold A.ResultKeys in RK. subst ks. apply in_flat_map in Ik as [e [Ie Ik]].
  destruct (A.entry_key (A.c_sub C) e) as [k'|] eqn:Ek; [|contradiction].
  destruct Ik as [->|[]].
  assert (Ep : exists p, A.entry_provision (A.c_sub C) (fun a => a) e = [(k, p)])
    by (destruct e; cbn in Ek |- *; try discriminate; injection Ek as <-; eexists; reflexivity).
  destruct Ep as [p Ep].
  exists p. apply in_flat_map. exists e. split; [apply (proj1 (in_rev _ _)) in Ie; exact Ie|]. rewrite Ep. left. reflexivity.
Qed.

Lemma provide_result_news_key : forall W u C out C' ks, A.ProvideResult W u C out C' ks ->
  forall news, A.c_provisions C' = (news ++ A.c_provisions C)%list -> forall k p, In (k, p) news -> In k ks.
Proof.
  intros W u C out C' ks (d & ps & a & r & b & outs & es & tm' & _ & _ & _ & _ & RK & ->) news En k p I.
  cbn [A.output_cx A.c_provisions] in En. apply app_inv_tail in En. subst news.
  unfold A.ResultKeys in RK. subst ks. apply in_flat_map in I as [e [Ie I]]. apply (proj2 (in_rev _ _)) in Ie.
  apply in_flat_map. exists e. split; [exact Ie|].
  destruct e; cbn in I |- *; try contradiction; destruct I as [E|[]]; injection E as <- _; left; reflexivity.
Qed.

(* The selection's keys are the caller's keys. *)
Lemma sm_find_some_in : forall K (l : smap) s, sm_find K l = Some s -> In (K, s) l.
Proof.
  intros K l s. induction l as [|[a b] l IH]; intros F; [discriminate|]. cbn in F.
  destruct (item_eqb K a) eqn:E; [apply item_eqb_spec in E; subst a; injection F as <-; left; reflexivity|].
  right. exact (IH F).
Qed.

Lemma sm_find_of_in : forall K (l : smap) s, In (K, s) l -> exists s', sm_find K l = Some s'.
Proof.
  intros K l s. induction l as [|[a b] l IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb K a) eqn:E; [eexists; reflexivity|].
  destruct I as [Eq|I]; [injection Eq as -> ->; assert (T : item_eqb K K = true) by (apply item_eqb_spec; reflexivity); congruence|].
  exact (IH I).
Qed.

Lemma selection_in : forall rs rms srcs K s, In (K, s) (result_selection rs rms srcs) ->
  In K rs /\ In s (chain_sources rms srcs).
Proof.
  intros rs rms srcs K s I. unfold result_selection in I. apply (proj2 (in_rev _ _)) in I.
  destruct (in_combine_nth _ _ _ _ I) as [i [R S]]. split; [exact (nth_error_In _ _ R)|exact (nth_error_In _ _ S)].
Qed.

Lemma combine_nth_in : forall (rs ss : list item) i a b, nth_error rs i = Some a -> nth_error ss i = Some b ->
  In (a, b) (combine rs ss).
Proof.
  induction rs as [|r rs IH]; intros [|s ss] [|i] a b Ra Sb; cbn in *; try discriminate.
  - injection Ra as <-. injection Sb as <-. left. reflexivity.
  - right. exact (IH ss i a b Ra Sb).
Qed.

Lemma ilook_of_in : forall X k (l : list (item * X)) p, In (k, p) l -> exists p', A.ilook k l = Some p'.
Proof.
  intros X k l p. induction l as [|[k' p'] l IH]; intros I; [contradiction|]. cbn.
  destruct (item_eqb k k') eqn:E; [eexists; reflexivity|].
  destruct I as [Eq|I]; [injection Eq as -> ->; assert (T : item_eqb k k = true) by (apply item_eqb_spec; reflexivity); congruence|].
  exact (IH I).
Qed.

Lemma forall2_nth : forall X Y (R : X -> Y -> Prop) l1 l2 i x y, Forall2 R l1 l2 ->
  nth_error l1 i = Some x -> nth_error l2 i = Some y -> R x y.
Proof.
  intros X Y R l1 l2 i x y F. revert i. induction F as [|a b l1 l2 Rab F IH]; intros [|i] E1 E2; cbn in *; try discriminate.
  - injection E1 as <-. injection E2 as <-. exact Rab.
  - exact (IH i E1 E2).
Qed.

Lemma nth_error_same_length : forall X Y (l1 : list X) (l2 : list Y) i x, List.length l1 = List.length l2 ->
  nth_error l1 i = Some x -> exists y, nth_error l2 i = Some y.
Proof.
  intros X Y l1 l2 i x L E. destruct (nth_error l2 i) as [y|] eqn:E2; [eexists; reflexivity|].
  apply nth_error_None in E2. assert (i < List.length l1) by (apply nth_error_Some; rewrite E; discriminate). lia.
Qed.

(* The selection under a caller key: its last position, whose source is
   the selection's. *)
Lemma selection_last : forall rs rms srcs K s, sm_find K (result_selection rs rms srcs) = Some s ->
  List.length (chain_sources rms srcs) = List.length rs ->
  exists i, nth_error rs i = Some K /\ nth_error (chain_sources rms srcs) i = Some s /\
    forall i', i < i' -> nth_error rs i' <> Some K.
Proof.
  intros rs rms srcs K s F L. destruct (sm_find_rev_combine _ _ _ _ F) as [i [R [S Last]]].
  exists i. split; [exact R|]. split; [exact S|]. intros i' Lt E.
  pose proof (Last i' Lt E) as N. destruct (nth_error_same_length _ _ rs (chain_sources rms srcs) i' K (eq_sym L) E) as [y Y].
  congruence.
Qed.

Lemma selection_at : forall rs rms srcs i K, nth_error rs i = Some K ->
  List.length (chain_sources rms srcs) = List.length rs ->
  exists s, sm_find K (result_selection rs rms srcs) = Some s.
Proof.
  intros rs rms srcs i K R L. destruct (nth_error_same_length _ _ rs (chain_sources rms srcs) i K (eq_sym L) R) as [y Y].
  apply (sm_find_of_in K _ y). unfold result_selection. apply (proj1 (in_rev _ _)). exact (combine_nth_in _ _ _ _ _ R Y).
Qed.
