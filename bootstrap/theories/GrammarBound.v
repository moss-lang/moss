(* A checked bound on the size of Menhir derivations. Give each terminal
   credit for 128 nodes. A production spends one node and the sum of the
   weights of its children. Checking the resulting inequalities suffices
   for all trees, independently of how the weights were found. *)
From Stdlib Require Import List ZArith Lia FMapPositive.
From MenhirLib Require Import Alphabet.
From NM Require MossGrammar.
Import ListNotations.
Import MossGrammar.Gram.
Local Open Scope Z_scope.

Definition weights := PositiveMap.t Z.

Definition nonterminal_key := @inj _ nonterminalNum.

Definition symbol_weight (ws : weights) (s : symbol) : option Z :=
  match s with
  | T _ => Some (1 - 128)
  | NT n => PositiveMap.find (nonterminal_key n) ws
  end.

Fixpoint sum_weights (ws : weights) (ss : list symbol) : option Z :=
  match ss with
  | [] => Some 0
  | s :: ss =>
      match symbol_weight ws s, sum_weights ws ss with
      | Some w, Some rest => Some (w + rest)
      | _, _ => None
      end
  end.

Definition production_weight (ws : weights) (p : production) : option Z :=
  option_map (Z.add 1) (sum_weights ws (prod_rhs_rev p)).

Definition relax (ws : weights) (p : production) : weights :=
  match production_weight ws p with
  | None => ws
  | Some w =>
      let k := nonterminal_key (prod_lhs p) in
      let w := match PositiveMap.find k ws with Some old => Z.max w old | None => w end in
      PositiveMap.add k w ws
  end.

Definition tree_weights : weights :=
  Nat.iter (List.length (all_list (A := nonterminal)))
    (fun ws => fold_left relax (all_list (A := production)) ws)
    (PositiveMap.empty _).

Definition production_bounded (p : production) : bool :=
  match production_weight tree_weights p, symbol_weight tree_weights (NT (prod_lhs p)) with
  | Some rhs, Some lhs => Z.leb rhs lhs
  | _, _ => false
  end.

Lemma all_productions_bounded : forallb production_bounded (all_list (A := production)) = true.
Proof. vm_compute. reflexivity. Qed.

Definition weight (s : symbol) : Z :=
  match symbol_weight tree_weights s with Some w => w | None => 0 end.
Definition weight_sum (ss : list symbol) : Z := fold_right (fun s w => weight s + w) 0 ss.

Lemma sum_weights_sound ss w : sum_weights tree_weights ss = Some w -> weight_sum ss = w.
Proof.
  revert w. induction ss as [|s ss IH]; intros w H; cbn [sum_weights weight_sum fold_right] in *.
  - now injection H as <-.
  - destruct (symbol_weight tree_weights s) as [a|] eqn:E; [|discriminate].
    destruct (sum_weights tree_weights ss) as [b|]; [|discriminate].
    injection H as <-. change (weight s + weight_sum ss = a + b).
    unfold weight at 1. rewrite E, (IH b eq_refl). reflexivity.
Qed.

Lemma production_inequality p :
  1 + weight_sum (prod_rhs_rev p) <= weight (NT (prod_lhs p)).
Proof.
  pose proof (proj1 (forallb_forall _ _) all_productions_bounded p (all_list_forall p)) as H.
  unfold production_bounded, production_weight in H.
  destruct (sum_weights tree_weights (prod_rhs_rev p)) as [r|] eqn:Er; [|discriminate].
  destruct (symbol_weight tree_weights (NT (prod_lhs p))) as [l|] eqn:El; [|discriminate].
  change (Z.leb (1 + r) l = true) in H. apply Z.leb_le in H.
  unfold weight. now rewrite El, (sum_weights_sound _ _ Er).
Qed.

Lemma tree_bound :
  forall s word (t : parse_tree s word),
    Z.of_nat (pt_size t) <= 128 * Z.of_nat (List.length word) + weight s.
Proof.
  fix go 3 with (go_list (ss : list symbol) (word : list token) (t : parse_tree_list ss word) {struct t} :
    Z.of_nat (ptl_size t) <= 128 * Z.of_nat (List.length word) + weight_sum ss).
  - intros s word [tok|p w children].
    + change (1 <= 128 * 1 + (1 - 128)). lia.
    + cbn [pt_size]. rewrite Nat2Z.inj_succ.
      pose proof (go_list _ _ children). pose proof (production_inequality p). lia.
  - intros ss word [|symbols w children s u child].
    + cbn [ptl_size weight_sum fold_right List.length]. lia.
    + cbn [ptl_size weight_sum fold_right]. rewrite length_app, !Nat2Z.inj_add.
      pose proof (go _ _ child). pose proof (go_list _ _ children). fold (weight_sum symbols). lia.
Qed.

Lemma file_tree_bound word (tree : parse_tree (NT file'nt) word) :
  (pt_size tree <= 128 * S (List.length word))%nat.
Proof.
  pose proof (tree_bound _ _ tree).
  assert (weight (NT file'nt) <= 128) by (vm_compute; discriminate).
  lia.
Qed.
