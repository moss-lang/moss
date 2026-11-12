(* The lexer and the generated Menhir parser find every file derivation. *)
From Stdlib Require Import String List Arith Lia.
From NM Require Import ExecutionModel Parse FrontEnd.
From NM Require MossGrammar MossActions GrammarBound.
Import ExecutionModel.Input ExecutionModel.Syntax.

Lemma file_fuel_sufficient ts
    (tree : MossGrammar.Gram.parse_tree (MossGrammar.Gram.NT MossGrammar.Gram.file'nt)
      (map MossGrammar.of_token ts)) :
  MossGrammar.Gram.pt_size tree <= 2 ^ parse_log_fuel ts.
Proof.
  pose proof (GrammarBound.file_tree_bound _ tree) as Bound.
  rewrite length_map in Bound.
  assert (Log : S (List.length ts) <= 2 ^ Nat.log2_up (S (List.length ts))).
  { apply (proj2 (Nat.log2_up_le_pow2 _ _ (Nat.lt_0_succ _))). reflexivity. }
  unfold parse_log_fuel. rewrite Nat.pow_add_r.
  change (MossGrammar.Gram.pt_size tree <= 256 * 2 ^ Nat.log2_up (S (List.length ts))).
  lia.
Qed.

Theorem parse_tokens_complete ts f : File ts f -> parse_tokens ts = MossActions.Checked f.
Proof.
  intros (tree & Value). unfold parse_tokens.
  pose proof (MossGrammar.file_complete (parse_log_fuel ts) _ padding tree) as Complete.
  pose proof (file_fuel_sufficient ts tree) as Fuel.
  destruct (MossGrammar.file (parse_log_fuel ts) (LR.app_buf (map MossGrammar.of_token ts) padding))
    as [st t | | result rest]; try contradiction; try lia.
  destruct Complete as (Result & -> & _). now rewrite Result, Value.
Qed.

Theorem parse_moss_complete s f : Derives (list_byte_of_string s) f -> parse_moss s = Good f.
Proof.
  intros (ts & Hl & Hf). unfold parse_moss. unfold source_bytes in Hl.
  rewrite (lex_complete _ _ Hl), (parse_tokens_complete _ _ Hf). reflexivity.
Qed.
