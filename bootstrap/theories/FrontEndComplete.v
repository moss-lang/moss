(* ======================================================================== *)
(* FrontEndComplete.v — the parser is complete for ExecutionModel.Syntax.Derives:   *)
(* every derivation is what Parse.parse_moss finds (Part VI's               *)
(* parser_complete).                                                         *)
(*                                                                           *)
(* Each nonterminal is proved by induction on the parser's fuel n: from a   *)
(* derivation of X at a token list ts that ends in EOF (`Ends`), with       *)
(* `c + 32 * length ts <= n` (c a small constant per function, larger than *)
(* that of every function it calls before consuming a token), the parser   *)
(* function returns the derivation's AST and rest, and the rest is shorter *)
(* (`Ok`, or `Ok1`, strictly, for what always consumes a token).  The      *)
(* parser's fuel, 32 * (length ts + 1), covers every c.  The cases follow  *)
(* the grammar's rules one by one.                                          *)
(* ======================================================================== *)

From Stdlib Require Import String Ascii List Bool Arith Lia.
From Stdlib Require Import Strings.Byte.
From NM Require Import ExecutionModel Parse FrontEnd.
Import ListNotations.
Import ExecutionModel.Input ExecutionModel.Syntax.
Local Open Scope string_scope.

(* ===================== Tools ============================================== *)

Definition FirstDone (P : Prop) : Prop := True.

Lemma Ok1_Ok {A} (r : pr A) v ts rest : Ok1 r v ts rest -> Ok r v ts rest.
Proof. intros (? & ? & ?). repeat split; auto; lia. Qed.

Lemma kind_at2 ts : kind_at 2 ts = kind (advance (advance ts)).
Proof. destruct ts as [|t1 [|t2 [|t3 [|t4 r]]]]; reflexivity. Qed.
Lemma kind_at3 ts : kind_at 3 ts = kind (advance (advance (advance ts))).
Proof. destruct ts as [|t1 [|t2 [|t3 [|t4 [|t5 r]]]]]; reflexivity. Qed.

Lemma OptTk_eat_eq k ts ts' : OptTk k ts ts' -> snd (eat k ts) = ts'.
Proof.
  unfold eat. intros [[E ->] | [N ->]].
  - rewrite at_eq; auto.
  - rewrite at_neq; auto.
Qed.

(* Use the lemma or hypothesis L on the derivation H, at fuel n when L
   takes one; rewrite the call it describes. *)
Ltac rw E := rewrite E; cbv beta iota zeta delta [pbind].
Ltac ih_gen L H n :=
  let X := fresh "X" in
  first [ epose proof (L _ _ _ H) as X | epose proof (L _ _ _ _ H) as X
        | epose proof (L _ _ _ _ _ H) as X | epose proof (L _ _ _ _ _ _ H) as X
        | epose proof (L _ _ _ _ _ _ _ H) as X ];
  let He := fresh in
  lazymatch type of X with Ends ?t -> _ => assert (He : Ends t) by assumption end;
  specialize (X He);
  lazymatch type of X with
  | forall m : nat, _ => specialize (X n)
  | _ => idtac end;
  lazymatch type of X with ?b -> _ => specialize (X ltac:(lia)) end;
  lazymatch type of X with
  | exists l, _ => let l := fresh "last" in destruct X as (l & X)
  | _ => idtac end;
  let E := fresh "Ec" in let E1 := fresh "En" in let L1 := fresh "Ln" in
  destruct X as (E & E1 & L1); rw E.
Ltac ih L H := ih_gen L H 0.
Ltac ext L H n := ih_gen L H n.
Ltac ihx L H := ih_gen L H 0.

Ltac known ::= first [ assumption | congruence | discriminate
                    | (intro; congruence) | (simpl; intros; intuition congruence)
                    | (let E := fresh in intro E; rewrite E in *; simpl in *; intuition discriminate) ].
Ltac step := tks; decide_.
(* the same, finding the derivation *)
Ltac ihh L := match goal with H : _ |- _ => ih L H end.
Ltac exth L n := match goal with H : _ |- _ => ext L H n end.
Ltac fin := unfold Ok, Ok1; repeat split; auto; lia.

(* A comma list, its items by the tactic `tac` (goal: the item parser's Ok). *)
Ltac clist H n tac :=
  match goal with |- context [@comma_list ?A n ?item ?cl ?t] =>
  match type of H with CommaList ?X _ _ ?xs ?r =>
  let Hi := fresh "Hi" in
  assert (Hi : forall t' v r, X t' v r -> Ends t' -> length t' <= length t -> Ok (item t') v t' r)
    by (let a := fresh in let b := fresh in let c := fresh in let d := fresh in
        let e := fresh in let f := fresh in intros a b c d e f; tac);
  let E := fresh "Ec" in let E1 := fresh "En" in let L1 := fresh "Ln" in
  destruct (@comma_list_complete _ X item cl _ _ _ H ltac:(assumption) Hi n ltac:(lia)) as (E & E1 & L1);
  replace (@comma_list A n item cl t) with (@POk (list A) xs r) by (symmetry; exact E);
  cbv beta iota zeta delta [pbind] end end.

Ltac clistx n tac := match goal with H : CommaList _ _ _ _ _ |- _ => clist H n tac end.

(* ===================== Names and applications ============================= *)

Lemma head_first ts h rest : Head ts h rest -> kind ts = "NAME" \/ kind ts = "DOT".
Proof.
  destruct 1 as [? ? ? Hp _ | ? ? ? ? ? ? Hp _ _ | ? ? ? ? [E _] _];
    [left; eapply path_kind; eauto | left; eapply path_kind; eauto | right; auto].
Qed.

Lemma spec_group : forall n,
  (forall ts v rest, Spec_ ts v rest -> Ends ts -> 6 + 32 * length ts <= n -> Ok1 (parse_spec n ts) v ts rest) /\
  (forall ts v rest, App ts v rest -> Ends ts -> 5 + 32 * length ts <= n -> Ok (parse_app_opt n ts) v ts rest) /\
  (forall ts v rest, AppBinding ts v rest -> Ends ts -> 7 + 32 * length ts <= n -> Ok1 (parse_binding n ts) v ts rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3)]; [repeat split; intros; lia|].
  repeat (match goal with |- _ /\ _ => split end); intros ts v rest H He Hn.
  - destruct H as [? ? ? ? ? ? Hh Ha]. simpl.
    inversion Hh as [? ? ? Hp Hk | ? ? ? ? ? ? Hp Ht Hx | ? ? ? ? Ht Hx]; subst.
    + pose proof (path_kind _ _ _ Hp). step. ext path_complete Hp n. step. ih IH2 Ha. fin.
    + pose proof (path_kind _ _ _ Hp). step. ext path_complete Hp n. step. ih IH2 Ha. fin.
    + step. ih IH2 Ha. fin.
  - destruct H as [? Hk | ? ? ? ? ? Ht Hl Ht2]; simpl.
    + step. fin.
    + step. clist Hl n ltac:(apply Ok1_Ok; apply IH3; auto; lia). step. fin.
  - destruct H as [? ? ? ? ? ? Hp Ht Hs]. simpl. pose proof (path_kind _ _ _ Hp).
    ext path_complete Hp n. step. ih IH1 Hs. fin.
Qed.

Lemma spec_complete ts v rest : Spec_ ts v rest -> Ends ts -> forall n, 6 + 32 * length ts <= n -> Ok1 (parse_spec n ts) v ts rest.
Proof. intros H He n Hn. apply (spec_group n); auto. Qed.
Lemma app_complete ts v rest : App ts v rest -> Ends ts -> forall n, 5 + 32 * length ts <= n -> Ok (parse_app_opt n ts) v ts rest.
Proof. intros H He n Hn. apply (spec_group n); auto. Qed.

Lemma assume_item_complete ts v rest : AssumeItem ts v rest -> Ends ts -> forall n, length ts <= n ->
  Ok1 (parse_assume_item n ts) v ts rest.
Proof.
  unfold AssumeItem. destruct v as [p d]. simpl. intros H He n Hn. unfold parse_assume_item.
  inversion H as [? ? ? Hp Hk | ? ? ? ? ? ? Hp Ht Hx | ]; subst.
  - ext path_complete Hp n. step. fin.
  - ext path_complete Hp n. step. fin.
Qed.

(* ===================== Types ============================================== *)

Definition MembersTail (ts : list token) (ms : list aty) (rest : list token) : Prop :=
  (ms = [] /\ kind ts <> "PIPE" /\ rest = ts) \/ (exists ts2, Tk "PIPE" ts ts2 /\ Members ts2 ms rest).

Lemma members_split ts ms rest : Members ts ms rest ->
  exists m ms' t', ms = m :: ms' /\ TypeAtom ts m t' /\ MembersTail t' ms' rest.
Proof.
  destruct 1 as [? ? ? Ha Hk | ? ? ? ? ? ? Ha Ht Hm].
  - do 3 eexists. split; [reflexivity|]. split; [eauto|]. left; auto.
  - do 3 eexists. split; [reflexivity|]. split; [eauto|]. right; eauto.
Qed.

Lemma type_group : forall n,
  (forall ts v rest, Type_ ts v rest -> Ends ts -> 9 + 32 * length ts <= n -> Ok1 (parse_type n ts) v ts rest) /\
  (forall ts v rest, MembersTail ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok (union_rest n ts) v ts rest) /\
  (forall ts v rest, TypeAtom ts v rest -> Ends ts -> 8 + 32 * length ts <= n -> Ok1 (parse_type_atom n ts) v ts rest) /\
  (forall ts v rest, RecordType ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_record_type n ts) v ts rest) /\
  (forall ts v rest, TypeField ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (type_field n ts) v ts rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3 & IH4 & IH5)]; [repeat split; intros; lia|].
  repeat (match goal with |- _ /\ _ => split end); intros ts v rest H He Hn.
  - simpl. unfold eat, at_type_atom. inversion H as [? ? Ht Hn' | ? ? ms ? Ho Hm]; subst.
    + step. try rewrite at_any_nin by auto. simpl. fin.
    + destruct (members_split _ _ _ Hm) as (m & ms' & t' & -> & Ha & Hmt).
      destruct Ho as [Ht | [Hk ->]].
      * step. try rewrite at_any_in by (apply type_atom_first in Ha; auto). simpl.
        ih IH3 Ha. ih IH2 Hmt. destruct ms'; fin.
      * rewrite at_neq by auto. simpl. ih IH3 Ha. ih IH2 Hmt. destruct ms'; fin.
  - simpl. destruct H as [(-> & Hk & ->) | (ts2 & Ht & Hm)].
    + rewrite at_neq by auto. fin.
    + destruct (members_split _ _ _ Hm) as (m & ms' & t' & -> & Ha & Hmt).
      step. ih IH3 Ha. ih IH2 Hmt. fin.
  - simpl. destruct H as [? ? ? ? ? Hp Ha | ? ? Ht | ? ? ? Ht Ht2 | ? ? ? ? ? Ht Hty Ht2 | ? ? ? Hr].
    + pose proof (path_kind _ _ _ Hp). step. ext path_complete Hp n. ext app_complete Ha n. fin.
    + step. fin.
    + step. fin.
    + pose proof (type_first _ _ _ Hty). step. ih IH1 Hty.
      step. fin.
    + destruct Hr as [? ? ? ? ? Ht Hl Ht2] eqn:Er. step. ih IH4 Hr. fin.
  - simpl. destruct H as [? ? ? ? ? Ht Hl Ht2]. step.
    clist Hl n ltac:(apply Ok1_Ok; apply IH5; auto; lia). step. fin.
  - simpl. destruct H as [? ? ? ? ? ? Hx Ht Hty]. step. ih IH1 Hty. fin.
Qed.

Lemma type_complete ts v rest : Type_ ts v rest -> Ends ts -> forall n, 9 + 32 * length ts <= n -> Ok1 (parse_type n ts) v ts rest.
Proof. intros H He n Hn. apply (type_group n); auto. Qed.
Lemma record_type_complete ts v rest : RecordType ts v rest -> Ends ts -> forall n, 3 + 32 * length ts <= n ->
  Ok1 (parse_record_type n ts) v ts rest.
Proof. intros H He n Hn. apply (type_group n); auto. Qed.

Lemma opt_type_complete ts v rest : OptType ts v rest -> Ends ts -> forall n, 9 + 32 * length ts <= n ->
  Ok (parse_opt_type n ts) v ts rest.
Proof.
  intros H He n Hn. unfold parse_opt_type, eat. destruct H as [? Hk | ? ? ? ? Ht Hty].
  - rewrite at_neq by auto. fin.
  - step. ext type_complete Hty n. fin.
Qed.

(* ===================== Patterns =========================================== *)

Lemma pattern_first ts p rest : Pattern ts p rest ->
  kind ts = "UNDERSCORE" \/ kind ts = "NAME" \/ kind ts = "LBRACE".
Proof.
  destruct 1 as [? ? (E & _) | ? ? ? Hp _ | ? ? ? ? ? ? ? Hp _ _ _ | ? ? ? ? ? Hp _ | ? ? ? Hr];
    try (right; left; eapply path_kind; eauto; fail); auto.
  destruct Hr as [? ? ? ? ? [E _] _ _]. auto.
Qed.

Lemma pattern_group : forall n,
  (forall ts v rest, Pattern ts v rest -> Ends ts -> 4 + 32 * length ts <= n -> Ok1 (parse_pattern n ts) v ts rest) /\
  (forall ts v rest, RecordPattern ts v rest -> Ends ts -> 3 + 32 * length ts <= n ->
     forall p, Ok1 (parse_record_pattern n p ts) (APRec p v) ts rest) /\
  (forall ts v rest, PatField ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (pat_field n ts) v ts rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3)]; [repeat split; intros; lia|].
  repeat (match goal with |- _ /\ _ => split end); intros ts v rest H He Hn.
  - simpl. destruct H as [? ? Hx | ? ? ? Hp Hk | ? ? ? ? ? ? ? Hp Ht Hq Ht2 | ? ? ? ? ? Hp Hr
                         | ? ? ? Hr].
    + destruct Hx as (E1 & ->). destruct (ends_advance ts He ltac:(congruence)).
      rewrite at_neq by congruence. rewrite at_eq by auto. fin.
    + pose proof (path_kind _ _ _ Hp). rewrite !at_neq by congruence. rewrite at_eq by auto.
      ext path_complete Hp n. step. fin.
    + pose proof (path_kind _ _ _ Hp). rewrite !at_neq by congruence. rewrite at_eq by auto.
      ext path_complete Hp n.
      pose proof (pattern_first _ _ _ Hq). step. ih IH1 Hq. step. fin.
    + pose proof (path_kind _ _ _ Hp). rewrite !at_neq by congruence. rewrite at_eq by auto.
      ext path_complete Hp n.
      destruct Hr as [? ? ? ? ? Ht Hl Ht2] eqn:Er. step.
      destruct (IH2 _ _ _ Hr ltac:(assumption) ltac:(lia) (Some p)) as (E & E1 & L1). rw E. fin.
    + destruct Hr as [? ? ? ? ? Ht Hl Ht2] eqn:Er. step.
      destruct (IH2 _ _ _ Hr ltac:(assumption) ltac:(lia) None) as (E & E1 & L1). rw E. fin.
  - intros p. simpl. destruct H as [? ? ? ? ? Ht Hl Ht2]. step.
    clist Hl n ltac:(apply Ok1_Ok; apply IH3; auto; lia). step. fin.
  - simpl. destruct H as [? ? ? Hx Hk | ? ? ? ? ? ? Hx Ht Hq].
    + step. fin.
    + pose proof (pattern_first _ _ _ Hq). step. ih IH1 Hq. fin.
Qed.

Lemma pattern_complete ts v rest : Pattern ts v rest -> Ends ts -> forall n, 4 + 32 * length ts <= n ->
  Ok1 (parse_pattern n ts) v ts rest.
Proof. intros H He n Hn. apply (pattern_group n); auto. Qed.

(* ===================== Expressions: first tokens and operator shapes ====== *)

Ltac first_tk := let H := fresh in intro H; destruct H;
  match goal with |- kind ?t = _ => match goal with Hk : Tk _ t _ |- _ => exact (proj1 Hk) end end.
Lemma block_first ts b rest : Block ts b rest -> kind ts = "LBRACE".
Proof. first_tk. Qed.
Lemma lambda_first ts e rest : Lambda ts e rest -> kind ts = "LPAREN".
Proof. first_tk. Qed.
Lemma cond_first ts e rest : Cond ts e rest -> kind ts = "IF".
Proof. first_tk. Qed.
Lemma bind_first ts w rest : Bind ts w rest -> kind ts = "BIND".
Proof. first_tk. Qed.

Definition UFIRST := ["HYPHEN"; "EXCLAM"; "LPAREN"; "THIS"; "INTEGER"; "DECIMAL"; "STRING"; "LBRACE"; "NAME"].

Ltac start_kind :=
  match goal with |- In (kind ?t) _ =>
    first [ match goal with Hk : Tk _ t _ |- _ => rewrite (proj1 Hk) end
          | match goal with Hk : kind t = _ |- _ => rewrite Hk end
          | match goal with Hk : Path t _ _ |- _ => rewrite (path_kind _ _ _ Hk) end
          | match goal with Hk : Block t _ _ |- _ => rewrite (block_first _ _ _ Hk) end
          | match goal with Hk : Lambda t _ _ |- _ => rewrite (lambda_first _ _ _ Hk) end
          | match goal with Hk : In (kind t) _ |- _ => simpl in Hk |- *; intuition fail end ] end;
  simpl; intuition.

Lemma primary_first nr ts e rest : Primary nr ts e rest ->
  In (kind ts) ["LPAREN"; "THIS"; "INTEGER"; "DECIMAL"; "STRING"; "LBRACE"; "NAME"].
Proof. intro H; destruct H; start_kind. Qed.

Lemma unary_first nr ts e rest : Unary nr ts e rest -> In (kind ts) UFIRST.
Proof.
  intro H. destruct H as [? ? ? ? ? [E _] _ _ | ? ? ? ? ? [E _] _ | ? ? ? ? ? ? Hp _].
  - rewrite E. simpl. auto.
  - rewrite E. simpl. auto.
  - apply primary_first in Hp. simpl in *. intuition.
Qed.

Lemma term_first nr ts e rest : Term nr ts e rest -> In (kind ts) UFIRST.
Proof. destruct 1 as [? ? ? ? ? ? Hu _ _ | ? ? ? ? ? ? ? ? ? Hu _ _ _ _ _]; eapply unary_first; eauto. Qed.
Lemma operand_first nr ts e rest : Operand nr ts e rest -> In (kind ts) UFIRST.
Proof.
  destruct 1 as [? ? ? ? ? ? ? Hu _ _ _ | ? ? ? ? Ha _]; [eapply unary_first; eauto|].
  destruct Ha as [? ? ? ? ? ? Ht _]. eapply term_first; eauto.
Qed.
Lemma operators_first nr ts e rest : Operators nr ts e rest -> In (kind ts) UFIRST.
Proof.
  destruct 1 as [? ? ? ? ? ? ? Hu _ _ _ _ | ? ? ? ? ? ? ? Ho _ _ _ _ | ? ? ? ? Ho _];
    [eapply unary_first | eapply operand_first | eapply operand_first]; eauto.
Qed.

Definition EFIRST := ["IF"; "MATCH"; "RETURN"; "BREAK"; "HYPHEN"; "EXCLAM"; "LPAREN"; "THIS"; "INTEGER";
                      "DECIMAL"; "STRING"; "LBRACE"; "NAME"].
Lemma expr_first nr ts e rest : Expr nr ts e rest -> In (kind ts) EFIRST.
Proof.
  intro H. destruct H; unfold EFIRST.
  all: try (match goal with Hc : Cond _ _ _ |- _ => rewrite (cond_first _ _ _ Hc); simpl; auto end).
  all: try (match goal with Ho : Operators _ _ _ _ |- _ =>
              apply operators_first in Ho; unfold UFIRST in Ho; simpl in *; intuition end).
  all: start_kind.
Qed.

Lemma lambda_ahead ts e rest : Lambda ts e rest -> at_lambda ts = true.
Proof.
  intro H. destruct H as [ts ts1 ps ts2 ts3 ret ts4 ts5 body ts6 [E1 ->] Hl [E2 ->] Ho [E3 ->] _].
  unfold at_lambda. rewrite kind_at1, kind_at2, kind_at3.
  destruct Hl as [? Hc | ? x ? Hc Hx Hk | ? x ? ? ? ? Hc Hx [E4 ->] _].
  - simpl in Hc. destruct Hc as [Hc|[]]. rewrite <- Hc. simpl.
    destruct Ho as [? Hk | ? ? ? ? [E5 ->] _]; [rewrite E3|rewrite E5]; reflexivity.
  - destruct Hx as [? ? ? ? ? (Ex & _ & ->) Hot]. rewrite Ex. simpl.
    destruct Hot as [? Hk' | ? ? ? ? [E5 ->] _].
    + rewrite E2. simpl. destruct Ho as [? Hk'' | ? ? ? ? [E6 ->] _]; [rewrite E3|rewrite E6]; reflexivity.
    + rewrite E5. reflexivity.
  - destruct Hx as [? ? ? ? ? (Ex & _ & ->) Hot]. rewrite Ex. simpl.
    destruct Hot as [? Hk' | ? ? ? ? [E5 ->] _].
    + rewrite E4. reflexivity.
    + rewrite E5. reflexivity.
Qed.

Lemma neg_dot_bool ts : ~ neg_literal_dot ts ->
  (at_any ["INTEGER"; "DECIMAL"] ts && (kind_at 1 ts =? "DOT"))%bool = false.
Proof.
  unfold neg_literal_dot. intro H. destruct (at_any ["INTEGER"; "DECIMAL"] ts) eqn:E1; auto.
  destruct (kind_at 1 ts =? "DOT")%string eqn:E2; auto.
  exfalso. apply H. split; [apply at_any_true; auto | apply String.eqb_eq; auto].
Qed.

Lemma assign_bool ts : ~ assign_ahead ts -> (at_ "NAME" ts && (kind_at 1 ts =? "EQUAL"))%bool = false.
Proof.
  unfold assign_ahead. intro H. destruct (at_ "NAME" ts) eqn:E1; auto.
  destruct (kind_at 1 ts =? "EQUAL")%string eqn:E2; auto.
  exfalso. apply H. split; [apply at_true; auto | apply String.eqb_eq; auto].
Qed.

Lemma unary_op_none_of k : ~ In k ["HYPHEN"; "EXCLAM"] -> unary_op k = None.
Proof.
  intro H. unfold unary_op. destruct (k =? "HYPHEN")%string eqn:E1.
  - apply String.eqb_eq in E1. subst. simpl in H. intuition.
  - destruct (k =? "EXCLAM")%string eqn:E2; auto. apply String.eqb_eq in E2. subst. simpl in H. intuition.
Qed.

Lemma chain_end X ops acc ts e rest : LeftChain X ops acc ts e rest -> binop_in ops ts = false ->
  e = acc /\ rest = ts.
Proof.
  destruct 1 as [| ? ? op ? ? ? ? Hb Hin _ _]; auto.
  intro H. unfold binop_in in H. rewrite Hb in H. rewrite In_mem in H by auto. discriminate.
Qed.

Lemma term_split nr ts e rest : Term nr ts e rest ->
  exists u tsu, Unary nr ts u tsu /\ TermRest nr u tsu e rest.
Proof.
  destruct 1 as [? ? u ts1 e ts2 Hu Hc Hf | ? ? u ts1 e ts2 op r ts3 Hu Hc Hb Hin Hr Hn].
  - exists u, ts1. split; auto. exists e, ts2. split; auto.
  - exists u, ts1. split; auto. exists e, ts2. split; auto. right. eauto 10.
Qed.

Definition OperandRest (nr : bool) (first : aexpr) (ts : list token) (e : aexpr) (rest : list token) : Prop :=
  (exists op r, binop ts = Some op /\ In op SHIFT /\ Unary nr (advance ts) r rest /\ e = AEBinary op first r) \/
  (exists t ts1, TermRest nr first ts t ts1 /\ LeftChain (Term nr) ADDITIVE t ts1 e rest /\
                 binop_in SHIFT rest = false).

Lemma operand_split nr ts e rest : Operand nr ts e rest ->
  exists u tsu, Unary nr ts u tsu /\ OperandRest nr u tsu e rest.
Proof.
  destruct 1 as [? ? l ts1 op r ts2 Hu Hb Hin Hr | ? ? e ts1 Ha Hs].
  - exists l, ts1. split; auto. left. eauto 10.
  - destruct Ha as [? ? t ts2 e' ts3 Ht Hc]. destruct (term_split _ _ _ _ Ht) as (u & tsu & Hu & Htr).
    exists u, tsu. split; auto. right. eauto 10.
Qed.

Lemma binop_adv ts o : binop ts = Some o -> Ends ts -> Ends (advance ts) /\ length (advance ts) < length ts.
Proof.
  intros Hb He. apply ends_advance; auto. intro E. unfold binop in Hb. rewrite E in Hb. discriminate.
Qed.

Definition ARITH := ["<<"; ">>"; "*"; "/"; "%"; "+"; "-"].

Lemma not_in_ops l o ts : binop ts = Some o -> ~ In o l -> binop_in l ts = false.
Proof. intros Hb Hn. unfold binop_in. rewrite Hb. destruct (mem o l) eqn:E; auto. apply mem_In in E. contradiction. Qed.

(* An operand that does not continue with an arithmetic operator is its
   first unary. *)
Lemma operand_next nr u tsu e rest : OperandRest nr u tsu e rest ->
  (e = u /\ rest = tsu) \/ (exists o, binop tsu = Some o /\ In o ARITH).
Proof.
  intro H. destruct (binop tsu) as [o|] eqn:Eb.
  - destruct (in_dec string_dec o ARITH) as [Hin|Hn]; [right; eauto|left].
    destruct H as [(op & r & Hb & Hin & _) | (t & ts1 & (e1 & ts2 & Hc & Hf) & Ha & Hs)].
    + rewrite Hb in Eb. injection Eb as ->. exfalso. apply Hn. simpl in Hin |- *. intuition.
    + destruct (chain_end _ _ _ _ _ _ Hc) as [-> ->]; [apply (not_in_ops _ o); auto; intro; apply Hn; simpl in *; intuition|].
      destruct Hf as [(_ & -> & ->) | (op & r & Hb & Hin & _)].
      * apply (chain_end _ _ _ _ _ _ Ha). apply (not_in_ops _ o); auto. intro; apply Hn; simpl in *; intuition.
      * rewrite Hb in Eb. injection Eb as ->. exfalso. apply Hn. simpl in Hin |- *. intuition.
  - left. destruct H as [(op & r & Hb & _) | (t & ts1 & (e1 & ts2 & Hc & Hf) & Ha & Hs)]; [congruence|].
    destruct (chain_end _ _ _ _ _ _ Hc) as [-> ->]; [apply binop_in_none; auto|].
    destruct Hf as [(_ & -> & ->) | (op & r & Hb & _)]; [|congruence].
    apply (chain_end _ _ _ _ _ _ Ha). apply binop_in_none; auto.
Qed.

Lemma arith_not_shift nr first ts t ts1 e rest : TermRest nr first ts t ts1 ->
  LeftChain (Term nr) ADDITIVE t ts1 e rest -> binop_in SHIFT rest = false -> binop_in SHIFT ts = false.
Proof.
  intros (e1 & ts2 & Hc & Hf) Ha Hs. destruct (binop ts) as [o|] eqn:Eb; [|apply binop_in_none; auto].
  destruct (in_dec string_dec o SHIFT) as [Hin|Hn]; [|apply (not_in_ops _ o); auto].
  exfalso. assert (No : forall l, (forall x, In x l -> ~ In x SHIFT) -> binop_in l ts = false).
  { intros l Hl. apply (not_in_ops _ o); auto. intro. eapply Hl; eauto. }
  destruct (chain_end _ _ _ _ _ _ Hc) as [-> ->]; [apply No; simpl; intuition subst; simpl in *; intuition discriminate|].
  destruct Hf as [(_ & -> & ->) | (op & r & Hb & Hin2 & _)].
  - destruct (chain_end _ _ _ _ _ _ Ha) as [-> ->]; [apply No; simpl; intuition subst; simpl in *; intuition discriminate|].
    unfold binop_in in Hs. rewrite Eb, In_mem in Hs by auto. discriminate.
  - rewrite Hb in Eb. injection Eb as ->. simpl in *. intuition subst; discriminate.
Qed.

Lemma mem_bitwise_false o : In o (COMPARISON ++ ARITH) -> mem o BITWISE = false.
Proof. simpl. intuition subst; reflexivity. Qed.

(* ===================== Expressions ======================================== *)

Definition PostfixD (nr : bool) (ts : list token) (e : aexpr) (rest : list token) : Prop :=
  exists p ts1, Primary nr ts p ts1 /\ Suffixes p ts1 e rest.

Lemma OptTk_ends k ts ts' : OptTk k ts ts' -> k <> "EOF" -> Ends ts -> Ends ts' /\ length ts' <= length ts.
Proof.
  intros [Ht | [_ ->]] Hk He; [|auto]. destruct (Tk_ends _ _ _ Ht Hk He). split; auto; lia.
Qed.

(* first-token facts of the derivations in the context *)
Ltac done1 H := let T := type of H in
  lazymatch goal with _ : FirstDone T |- _ => fail | _ => assert (FirstDone T) by exact I end.
Ltac firsts := repeat match goal with
  | H : Pattern _ _ _ |- _ => done1 H; pose proof (pattern_first _ _ _ H)
  | H : Block _ _ _ |- _ => done1 H; pose proof (block_first _ _ _ H)
  | H : Cond _ _ _ |- _ => done1 H; pose proof (cond_first _ _ _ H)
  | H : Expr _ _ _ _ |- _ => done1 H; pose proof (expr_first _ _ _ _ H)
  | H : Operators _ _ _ _ |- _ => done1 H; pose proof (operators_first _ _ _ _ H)
  | H : Unary _ _ _ _ |- _ => done1 H; pose proof (unary_first _ _ _ _ H)
  | H : Primary _ _ _ _ |- _ => done1 H; pose proof (primary_first _ _ _ _ H)
  | H : Lambda _ _ _ |- _ => done1 H; pose proof (lambda_first _ _ _ H)
  | H : Bind _ _ _ |- _ => done1 H; pose proof (bind_first _ _ _ H)
  | H : Path _ _ _ |- _ => done1 H; pose proof (path_kind _ _ _ H)
  | H : Type_ _ _ _ |- _ => done1 H; pose proof (type_first _ _ _ H)
  | H : TypeAtom _ _ _ |- _ => done1 H; pose proof (type_atom_first _ _ _ H)
  | H : Head _ _ _ |- _ => done1 H; pose proof (head_first _ _ _ H)
  end;
  unfold EFIRST, UFIRST in *.
Ltac ihp L P := match goal with H : ?T |- _ => lazymatch T with context [P] => ih L H end end.
Ltac exp L P n := match goal with H : ?T |- _ => lazymatch T with context [P] => ext L H n end end.

Lemma expr_group : forall n,
  (forall nr ts v rest, Expr nr ts v rest -> Ends ts -> 16 + 32 * length ts <= n -> Ok1 (parse_expr n nr ts) v ts rest) /\
  (forall ts v rest, Cond ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_if n ts) v ts rest) /\
  (forall ts v rest, Arms ts v rest -> Ends ts -> 6 + 32 * length ts <= n -> Ok (parse_arms n ts) v ts rest) /\
  (forall nr ts v rest, Operators nr ts v rest -> Ends ts -> 14 + 32 * length ts <= n -> Ok1 (parse_operators n nr ts) v ts rest) /\
  (forall nr op e ts v rest, LeftChain (Unary nr) [op] e ts v rest -> Ends ts -> 3 + 32 * length ts <= n ->
     Ok (bit_loop n nr op e ts) v ts rest) /\
  (forall nr first ts v rest, OperandRest nr first ts v rest -> Ends ts -> 7 + 32 * length ts <= n ->
     exists last, Ok (parse_operand n nr first ts) (v, last) ts rest) /\
  (forall nr last e ts v rest, LeftChain (Term nr) ADDITIVE e ts v rest -> Ends ts -> 3 + 32 * length ts <= n ->
     exists last', Ok (add_loop n nr e last ts) (v, last') ts rest) /\
  (forall nr first ts v rest, TermRest nr first ts v rest -> Ends ts -> 5 + 32 * length ts <= n ->
     exists last, Ok (parse_term n nr first ts) (v, last) ts rest) /\
  (forall nr last e ts v rest, LeftChain (Unary nr) ["*"] e ts v rest -> Ends ts -> 3 + 32 * length ts <= n ->
     exists last', Ok (mul_loop n nr e last ts) (v, last') ts rest) /\
  (forall nr ts v rest, Unary nr ts v rest -> Ends ts -> 12 + 32 * length ts <= n -> Ok1 (parse_unary n nr ts) v ts rest) /\
  (forall nr ts v rest, PostfixD nr ts v rest -> Ends ts -> 11 + 32 * length ts <= n -> Ok1 (parse_postfix n nr ts) v ts rest) /\
  (forall e ts v rest, Suffixes e ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok (postfix_loop n e ts) v ts rest) /\
  (forall ts v rest, Lambda ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_lambda n ts) v ts rest) /\
  (forall nr ts v rest, Primary nr ts v rest -> Ends ts -> 10 + 32 * length ts <= n -> Ok1 (parse_primary n nr ts) v ts rest) /\
  (forall ts v rest, Block ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_block n ts) v ts rest) /\
  (forall ts ss tl rest, Stmts ts ss tl rest -> Ends ts -> 18 + 32 * length ts <= n -> Ok (block_loop n ts) (ss, tl) ts rest) /\
  (forall ts v rest, Bind ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_bind n ts) v ts rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3 & IH4 & IH5 & IH6 & IH7 & IH8 & IH9 & IH10 & IH11 & IH12 & IH13
                      & IH14 & IH15 & IH16 & IH17)];
    [repeat (match goal with |- _ /\ _ => split end); intros; lia|].
  repeat (match goal with |- _ /\ _ => split end).
  (* parse_expr *)
  - intros nr ts v rest H He Hn. simpl. destruct H as [? ? ? ? Hc | | | | | ? ? ? ? Ho].
    + pose proof (cond_first _ _ _ Hc). step. ih IH2 Hc. fin.
    + step. ihp IH1 Expr. step. ihp IH3 Arms. step. fin.
    + step. fin.
    + step. ihp IH1 Expr. fin.
    + step. fin.
    + pose proof (operators_first _ _ _ _ Ho) as F. unfold UFIRST in F. step. ih IH4 Ho. fin.
  (* parse_if *)
  - intros ts v rest H He Hn. simpl. destruct H; firsts.
    + step. ihp IH1 Expr. ihp IH15 Block. unfold eat. step. fin.
    + step. ihp IH1 Expr. ihp IH15 Block. unfold eat. step. ihp IH2 Cond. fin.
    + step. ihp IH1 Expr. ihp IH15 Block. unfold eat. step. ihp IH15 Block. fin.
  (* parse_arms *)
  - intros ts v rest H He Hn. simpl. destruct H as [| ? ? ? ? ? Hb ? ? Hp Ht Hbody Ha]; firsts.
    + step. fin.
    + step. exp pattern_complete Pattern n. step. destruct Hbody as [? ? ? ? Hb' Ho | | ]; firsts.
      * step. ihp IH15 Block. rewrite (OptTk_eat_eq _ _ _ Ho).
        destruct (OptTk_ends _ _ _ Ho ltac:(discriminate) ltac:(assumption)). ihp IH3 Arms. fin.
      * step. ihp IH1 Expr. step. ihp IH3 Arms. fin.
      * step. ihp IH1 Expr. step. ihp IH3 Arms. fin.
  (* parse_operators *)
  - intros nr ts v rest H He Hn. simpl. destruct H as [? ? u ts1 op e ts2 Hu Hb Hin Hc Hr
                                                    | ? ? l ts1 op r ts2 Ho Hb Hin Ho2 Hr | ? ? e ts1 Ho Hr].
    + ih IH10 Hu. rewrite Hb, In_mem by auto. ih IH5 Hc. rewrite Hr. fin.
    + destruct (operand_split _ _ _ _ Ho) as (u & tsu & Hu & Hor). ih IH10 Hu.
      assert (Ho' : exists o, binop tsu = Some o /\ mem o BITWISE = false).
      { destruct (operand_next _ _ _ _ _ Hor) as [[-> ->] | (o & Hbo & Hio)].
        - exists op. split; auto. apply mem_bitwise_false. apply in_or_app. auto.
        - exists o. split; auto. apply mem_bitwise_false. apply in_or_app. auto. }
      destruct Ho' as (o & Hbo & Hmo). rewrite Hbo, Hmo.
      ihp IH6 OperandRest. rewrite Hb, In_mem by auto.
      destruct (binop_adv _ _ Hb ltac:(assumption)).
      destruct (operand_split _ _ _ _ Ho2) as (u2 & tsu2 & Hu2 & Hor2). ih IH10 Hu2. ihp IH6 OperandRest.
      rewrite Hr. fin.
    + destruct (operand_split _ _ _ _ Ho) as (u & tsu & Hu & Hor). ih IH10 Hu.
      destruct (operand_next _ _ _ _ _ Hor) as [[-> ->] | (o & Hbo & Hio)].
      * rewrite Hr. fin.
      * rewrite Hbo, mem_bitwise_false by (apply in_or_app; auto). ihp IH6 OperandRest. rewrite Hr. fin.
  (* bit_loop *)
  - intros nr op e ts v rest H He Hn. simpl. destruct H as [acc ? Hb | acc ? o r ts1 e' ts2 Hb Hin Hu Hc].
    + destruct (binop ts) as [o|] eqn:Eb; [|fin].
      unfold binop_in, mem in Hb. rewrite Eb in Hb. simpl in Hb.
      assert (Hm : (o =? op)%string = false) by (destruct (o =? op)%string; auto). rewrite Hm. fin.
    + destruct Hin as [<-|[]]. rewrite Hb, String.eqb_refl.
      assert (kind ts <> "EOF") by (intro E; unfold binop in Hb; rewrite E in Hb; discriminate).
      destruct (ends_advance _ He H). ih IH10 Hu. ih IH5 Hc. fin.
  (* parse_operand *)
  - intros nr first ts v rest H He Hn. simpl.
    destruct H as [(op & r & Hb & Hin & Hu & ->) | (t & ts1 & Htr & Hc & Hs)].
    + rewrite (binop_in_some _ _ _ Hb), In_mem by auto. rewrite Hb.
      assert (kind ts <> "EOF") by (intro E; unfold binop in Hb; rewrite E in Hb; discriminate).
      destruct (ends_advance _ He H). ih IH10 Hu. eexists. fin.
    + rewrite (arith_not_shift _ _ _ _ _ _ _ Htr Hc Hs).
      ihp IH8 TermRest. ih IH7 Hc.
      match goal with |- exists _, Ok (match ?l with _ => _ end) _ _ _ => destruct l end; [rewrite Hs|]; eexists; fin.
  (* add_loop *)
  - intros nr last e ts v rest H He Hn. simpl. destruct H as [acc ? Hb | acc ? o r ts1 e' ts2 Hb Hin Ht Hc].
    + exists last. destruct (binop ts) as [o|] eqn:Eb; [|fin].
      unfold binop_in in Hb. rewrite Eb in Hb. rewrite Hb. fin.
    + rewrite Hb, In_mem by auto.
      assert (kind ts <> "EOF") by (intro E; unfold binop in Hb; rewrite E in Hb; discriminate).
      destruct (ends_advance _ He H).
      destruct (term_split _ _ _ _ Ht) as (u & tsu & Hu & Htr). ih IH10 Hu. ihp IH8 TermRest.
      ih IH7 Hc. eexists. fin.
  (* parse_term *)
  - intros nr first ts v rest H He Hn. simpl. destruct H as (e1 & ts2 & Hc & Hf).
    ih IH9 Hc.
    destruct Hf as [(Hb & -> & ->) | (op & r & Hb & Hin & Hu & Hn' & ->)].
    + eexists. destruct (binop ts2) as [o|] eqn:Eb; [|fin].
      unfold binop_in in Hb. rewrite Eb in Hb. rewrite Hb. fin.
    + rewrite Hb, In_mem by auto.
      assert (kind ts2 <> "EOF") by (intro E'; unfold binop in Hb; rewrite E' in Hb; discriminate).
      destruct (binop_adv _ _ Hb ltac:(assumption)). ih IH10 Hu. rewrite Hn'. eexists. fin.
  (* mul_loop *)
  - intros nr last e ts v rest H He Hn. simpl. destruct H as [acc ? Hb | acc ? o r ts1 e' ts2 Hb Hin Hu Hc].
    + rewrite Hb. exists last. fin.
    + destruct Hin as [<-|[]]. rewrite (binop_in_some _ _ _ Hb). simpl.
      assert (kind ts <> "EOF") by (intro E; unfold binop in Hb; rewrite E in Hb; discriminate).
      destruct (ends_advance _ He H). ih IH10 Hu.
      ih IH9 Hc. eexists. fin.
  (* parse_unary *)
  - intros nr ts v rest H He Hn. simpl. destruct H as [? ? ts1 u ts2 Ht Hnd Hu | ? ? ts1 u ts2 Ht Hu | ? ? p ts1 e ts2 Hp Hs].
    + tks. rewrite Ek. simpl. rewrite neg_dot_bool by auto. ih IH10 Hu. fin.
    + tks. rewrite Ek. simpl. ih IH10 Hu. fin.
    + pose proof (primary_first _ _ _ _ Hp) as F. rewrite unary_op_none_of by (simpl in *; intuition congruence).
      assert (Hd : PostfixD nr ts e ts2) by (exists p, ts1; auto). ih IH11 Hd. fin.
  (* parse_postfix *)
  - intros nr ts v rest (p & ts1 & Hp & Hs) He Hn. simpl. ih IH14 Hp. ih IH12 Hs. fin.
  (* postfix_loop *)
  - intros e ts v rest H He Hn. simpl. destruct H as [? ? Hk | | ? ? ? x ? ? ? Ht Hp Hk Hs]; firsts.
    + step. fin.
    + step. exp path_complete Path n. step. clistx n ltac:(apply Ok1_Ok; apply IH1; auto; lia). step. ihp IH12 Suffixes. fin.
    + step. exp path_complete Path n. step. ihp IH12 Suffixes. fin.
  (* parse_lambda *)
  - intros ts v rest H He Hn. simpl. destruct H as [? ? ps ? ? ret ? ? body ? Ht Hl Ht2 Ho Ht3 Hb].
    step. clist Hl n ltac:(match goal with H : LamParam _ _ _ |- _ => destruct H as [? ? ? ? ? Hx Hot] end;
                           step; exp opt_type_complete OptType n; fin).
    step. exp opt_type_complete OptType n. step. ihp IH1 Expr. fin.
  (* parse_primary *)
  - intros nr ts v rest H He Hn. simpl. destruct H; firsts.
    + rewrite at_eq by auto. rewrite (lambda_ahead _ _ _ H). cbn [andb]. ihp IH13 Lambda. fin.
    + tks. rewrite at_eq by auto. rewrite H0. cbn [andb]. step. fin.
    + tks. rewrite at_eq by auto. rewrite H0. cbn [andb]. step. ihp IH1 Expr. step. fin.
    + step. cbn [andb]. step. fin.
    + step. cbn [andb]. step. destruct (ends_advance ts He); [intro E; rewrite E in *; simpl in *; intuition discriminate|]. fin.
    + step. cbn [andb]. step. rewrite H0. destruct (ends_advance ts He); [intro E; rewrite E in *; simpl in *; intuition discriminate|]. fin.
    + step. cbn [andb negb]. step. ihp IH15 Block. fin.
    + step. cbn [andb negb]. step. exp path_complete Path n. exp app_complete App n. step.
      destruct H2 as [-> | Hk]; [cbn [negb]; rewrite andb_false_r | rewrite at_neq by auto]; cbn [andb]; fin.
    + step. cbn [andb negb]. step. exp path_complete Path n. exp app_complete App n. step.
      clistx n ltac:(apply Ok1_Ok; apply IH1; auto; lia). step. fin.
    + step. cbn [andb negb]. step. exp path_complete Path n. exp app_complete App n. step. cbn [andb negb].
      clistx n ltac:(match goal with H : RecField _ _ _ |- _ => destruct H end;
                     step; try ihp IH1 Expr; fin).
      step. fin.
  (* parse_block *)
  - intros ts v rest H He Hn. simpl. destruct H. step. ihp IH16 Stmts. step. fin.
  (* block_loop *)
  - intros ts ss tl rest H He Hn. simpl. destruct H; firsts.
    + step. fin.
    + step. cbn [orb]. step. exp opt_type_complete OptType n. step. ihp IH1 Expr. step. ihp IH16 Stmts. fin.
    + step. cbn [orb]. step. exp opt_type_complete OptType n. step. ihp IH1 Expr. step. ihp IH16 Stmts. fin.
    + step. cbn [orb]. step. ihp IH17 Bind. ihp IH16 Stmts. fin.
    + step. cbn [orb]. step. ihp IH1 Expr. ihp IH15 Block. ihp IH16 Stmts. fin.
    + step. cbn [orb]. step. ihp IH15 Block. ihp IH16 Stmts. fin.
    + step. cbn [orb]. step. ihp IH1 Expr. ihp IH15 Block. ihp IH16 Stmts. fin.
    + rewrite kind_at1. step. cbn [orb andb]. step. match goal with H : kind (advance ts) = _ |- _ => rewrite H end. rewrite String.eqb_refl. cbv iota. ihp IH1 Expr. step. ihp IH16 Stmts. fin.
    + rewrite assign_bool by auto. step. cbn [orb andb]. step. ihp IH1 Expr. step. ihp IH16 Stmts. fin.
    + rewrite assign_bool by auto. step. cbn [orb andb]. step. ihp IH1 Expr. step. ihp IH16 Stmts. fin.
    + rewrite assign_bool by auto. step. cbn [orb andb]. step. ihp IH1 Expr. step. fin.
  (* parse_bind *)
  - intros ts v rest H He Hn. simpl. destruct H. step.
    clistx n ltac:(match goal with H : Binding _ _ _ |- _ => destruct H end;
                   firsts; exp spec_complete Spec_ n; step; try ihp IH1 Expr; fin).
    step. fin.
Qed.


Lemma expr_complete nr ts v rest : Expr nr ts v rest -> Ends ts -> forall n, 16 + 32 * length ts <= n ->
  Ok1 (parse_expr n nr ts) v ts rest.
Proof. intros H He n Hn. apply (expr_group n); auto. Qed.
Lemma block_complete ts v rest : Block ts v rest -> Ends ts -> forall n, 3 + 32 * length ts <= n ->
  Ok1 (parse_block n ts) v ts rest.
Proof. intros H He n Hn. apply (expr_group n); auto. Qed.
Lemma bind_complete ts v rest : Bind ts v rest -> Ends ts -> forall n, 3 + 32 * length ts <= n ->
  Ok1 (parse_bind n ts) v ts rest.
Proof. intros H He n Hn. apply (expr_group n); auto. Qed.

(* ===================== Declarations and files ============================= *)

Lemma name_list_complete ts xs rest : NameList ts xs rest -> Ends ts -> forall n, 1 + length ts <= n ->
  Ok1 (name_list n ts) xs ts rest.
Proof.
  intros (ts1 & ts2 & Ht & Hl & Ht2) He n Hn. unfold name_list. step.
  clist Hl n ltac:(match goal with H : TkName _ _ _ |- _ => rewrite (expect_name_eq _ _ _ H); tks end; fin).
  step. fin.
Qed.

Lemma binds_loop_complete : forall n ts v rest, Binds ts v rest -> Ends ts -> 4 + 32 * length ts <= n ->
  Ok (binds_loop n ts) v ts rest.
Proof.
  induction n as [|n IH]; intros ts v rest H He Hn; [lia|]. simpl.
  destruct H as [? Hk | ? ? ? ? ? Hb Hbs].
  - step. fin.
  - pose proof (bind_first _ _ _ Hb). step. ext bind_complete Hb n. ih IH Hbs. fin.
Qed.

Lemma decl_first ts d rest : Decl ts d rest -> In (kind ts) ["ASSUME"; "TYPE"; "TAG"; "VAL"; "FN"; "CONTEXT"; "FUNCTOR"].
Proof.
  intro H; destruct H;
    match goal with Hk : Tk _ ts _ |- _ => rewrite (proj1 Hk) end; simpl; intuition.
Qed.

Lemma record_type_first ts t rest : RecordType ts t rest -> kind ts = "LBRACE".
Proof. first_tk. Qed.
Lemma namelist_first ts xs rest : NameList ts xs rest -> kind ts = "LBRACKET".
Proof. intros (? & ? & [E _] & _). auto. Qed.

Ltac param_tac n := match goal with H : Param _ _ _ |- _ => destruct H end;
  step; exp type_complete Type_ n; fin.

Lemma decl_group : forall n,
  (forall ts v rest, Decl ts v rest -> Ends ts -> 3 + 32 * length ts <= n -> Ok1 (parse_decl n ts) v ts rest) /\
  (forall ts v rest, Decls ts v rest -> Ends ts -> 4 + 32 * length ts <= n -> Ok (decls_until n ts) v ts rest).
Proof.
  induction n as [|n (IH1 & IH2)]; [split; intros; lia|].
  split; intros ts v rest H He Hn; simpl.
  - destruct H; firsts.
    (* assume *)
    + step. clistx n ltac:(apply Ok1_Ok; eapply assume_item_complete; eauto; lia).
      step. ihp IH2 Decls. step. fin.
    (* type, alias *)
    + step. fin.
    + step. exp type_complete Type_ n. step. fin.
    (* unit *)
    + step. fin.
    (* tag with a record payload *)
    + pose proof (record_type_first _ _ _ H2).
      destruct H1 as [(Hk & -> & ->) | Hnl].
      * step. exp record_type_complete RecordType n. step. fin.
      * pose proof (namelist_first _ _ _ Hnl). step. exp name_list_complete NameList n. step.
        exp record_type_complete RecordType n. step. fin.
    (* tag with a parenthesized payload *)
    + destruct H1 as [(Hk & -> & ->) | Hnl].
      * step. exp type_complete Type_ n. destruct t; try (exfalso; eapply H4; reflexivity); step; fin.
      * pose proof (namelist_first _ _ _ Hnl). step. exp name_list_complete NameList n. step.
        exp type_complete Type_ n. destruct t; try (exfalso; eapply H4; reflexivity); step; fin.
    (* val *)
    + step. exp type_complete Type_ n. step. fin.
    + step. exp type_complete Type_ n. step. exp expr_complete Expr n. step. fin.
    (* fn *)
    + destruct H0; step; clistx n ltac:(param_tac n); step; exp opt_type_complete OptType n; step; fin.
    +
      destruct H0; step; clistx n ltac:(param_tac n); step; exp opt_type_complete OptType n; step;
        exp block_complete Block n; fin.
    (* context *)
    + step. clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step. fin.
    (* functors *)
    + destruct H1 as [(Hk & -> & ->) | (l & -> & Hnl)].
      * step. clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step.
        clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step. fin.
      * pose proof (namelist_first _ _ _ Hnl). step. exp name_list_complete NameList n. step.
        clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step.
        clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step. fin.
    + step. clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step.
      clistx n ltac:(apply Ok1_Ok; eapply spec_complete; eauto; lia). step.
      match goal with Hb : Binds _ _ _ |- _ => destruct (binds_loop_complete n _ _ _ Hb ltac:(assumption) ltac:(lia)) as (Eb & ? & ?); rw Eb end. step. fin.
  - destruct H as [? Hk | ? ? ? ? ? Hd Hds].
    + step. fin.
    + pose proof (decl_first _ _ _ Hd). step. ih IH1 Hd. ih IH2 Hds. fin.
Qed.

Lemma decl_complete ts v rest : Decl ts v rest -> Ends ts -> forall n, 3 + 32 * length ts <= n ->
  Ok1 (parse_decl n ts) v ts rest.
Proof. intros H He n Hn. apply (decl_group n); auto. Qed.

Lemma use_name_complete ts u rest : UseName ts u rest -> Ends ts -> Ok (use_name ts) u ts rest.
Proof.
  destruct u as [d x]. intros (ts1 & Hc & Hx) He. simpl in Hc, Hx. unfold use_name, eat.
  destruct Hc as [(-> & Ht) | (-> & Hk & ->)].
  - step. fin.
  - rewrite at_neq by auto. step. fin.
Qed.

Lemma use_complete ts u rest : Use ts u rest -> Ends ts -> Ok (use_item ts) u ts rest.
Proof.
  intros H He. unfold use_item. destruct H as [? d x ts1 Hu Hk | ? d x ts1 ts2 y ts3 Hu Ht Hu2].
  - destruct (use_name_complete _ _ _ Hu He) as (E & E1 & L1). rw E. step. fin.
  - destruct (use_name_complete _ _ _ Hu He) as (E & E1 & L1). rw E. step.
    destruct (use_name_complete _ _ _ Hu2 ltac:(assumption)) as (E2 & E3 & L3). rw E2.
    rewrite eqb_reflx. fin.
Qed.

Lemma import_complete ts m rest : Import_ ts m rest -> Ends ts -> forall n, length ts <= n ->
  Ok1 (parse_import n ts) m ts rest.
Proof.
  intros H He n Hn. unfold parse_import.
  destruct H as [? ts1 s ts2 alias ts3 glob uses ts4 ts5 Ht Hs Ha Hu Ht5]. step.
  destruct Ha as [(Hk & -> & ->) | (ts' & a & -> & Hta & Hn')]; step;
  (destruct Hu as [(Hk' & -> & -> & ->) | [(ts'' & Ht1 & Ht2 & -> & ->) | (ts'' & Ht1 & Hk' & -> & Hl)]];
   step; [fin | fin | clistx n ltac:(match goal with H : Use ?t _ _ |- _ => match goal with He : Ends t |- _ =>
                    destruct (use_complete _ _ _ H He) as (? & ? & ?) end end; fin); step; fin]).
Qed.

Lemma imports_complete : forall n ts v rest, Imports ts v rest -> Ends ts -> 3 + 32 * length ts <= n ->
  Ok (imports_loop n ts) v ts rest.
Proof.
  induction n as [|n IH]; intros ts v rest H He Hn; [lia|]. simpl.
  destruct H as [? Hk | ? m ts1 ms ts2 Hm Hms].
  - step. fin.
  - assert (kind ts = "IMPORT") by (destruct Hm as [? ? ? ? ? ? ? ? ? ? [E _] _ _ _ _]; auto).
    step. ext import_complete Hm n. ih IH Hms. fin.
Qed.

Lemma decls_to_eof_complete : forall n ts v rest, DeclsToEnd ts v rest -> Ends ts -> 4 + 32 * length ts <= n ->
  Ok (decls_to_eof n ts) v ts rest.
Proof.
  induction n as [|n IH]; intros ts v rest H He Hn; [lia|]. simpl.
  destruct H as [? Hk | ? d ts1 ds ts2 Hd Hds].
  - step. fin.
  - pose proof (decl_first _ _ _ Hd). step. ext decl_complete Hd n. ih IH Hds. fin.
Qed.

Theorem parse_tokens_complete ts f : File ts f -> Ends ts -> exists rest, parse_tokens ts = POk f rest.
Proof.
  intros (ts1 & ts2 & Hi & Hd) He. unfold parse_tokens. cbv zeta.
  destruct (imports_complete (32 * (length ts + 1)) _ _ _ Hi He ltac:(lia)) as (E & E1 & L1). rewrite E.
  cbv beta iota delta [pbind].
  destruct (decls_to_eof_complete (32 * (length ts + 1)) _ _ _ Hd E1 ltac:(lia)) as (E2 & E3 & L3).
  rewrite E2. cbv beta iota delta [pbind]. destruct f. eauto.
Qed.

(* Part VI's parser_complete. *)
Theorem parse_moss_complete s f : Derives (list_byte_of_string s) f -> parse_moss s = Good f.
Proof.
  intros (ts & Hl & Hf). unfold source_bytes in Hl. unfold parse_moss.
  rewrite (lex_complete _ _ Hl).
  destruct (parse_tokens_complete _ _ Hf (lexes_ends _ _ _ Hl)) as (rest & ->). reflexivity.
Qed.
