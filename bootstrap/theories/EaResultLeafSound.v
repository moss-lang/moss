(* Leaves compared by the functor result and body coverage checks. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit EaBase EaTypes EaTypesSound EaDecide EaFamily EaOutputSound.
From NM Require EaDependencyFuel.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma leaf_eqb_spec : forall a b, leaf_eqb a b = true <-> a = b.
Proof.
  intros [s [n|]] [r [m|]]; unfold leaf_eqb; cbn [fst snd];
    rewrite andb_true_iff, Nat.eqb_eq; try rewrite String.eqb_eq;
    split; intuition congruence.
Qed.

Theorem result_spec_leaves_sound : forall W fuel p sp k,
  In k (result_spec_leaves W fuel p sp) -> ResultSpecLeaf W p sp k.
Proof.
  intros W fuel. induction fuel as [|fuel IH]; intros p [[pa|] [dn|] br] k Present;
    cbn [result_spec_leaves] in Present; try contradiction.
  - destruct (resolve_sym W p pa) as [s|] eqn:Resolve; [|contradiction].
    destruct Present as [<-|[]]. now apply RSLMethod.
  - destruct (resolve_sym W p pa) as [s|] eqn:Resolve; [|contradiction].
    destruct (w_decl W s) as [d|] eqn:Declared; [|contradiction].
    destruct (d_ast d) as [items declarations|n|n body|n ps pay|n|n t init|
      recv n ps ret body|n items|n ps ins outs body] eqn:Ast.
    all: try match type of Present with
      | In ?key (if result_already_provided ?a then [] else _) =>
          destruct (result_already_provided a) eqn:Given; [contradiction|];
          destruct Present as [<-|[]];
          eapply RSLPlain; [exact Resolve|exact Declared|rewrite Ast; discriminate|rewrite Ast; exact Given]
      end.
    apply in_flat_map in Present as (sp & Member & Read).
    eapply RSLContext; [exact Resolve|exact Declared|exact Ast|exact Member|now apply IH].
Qed.

Theorem result_leaves_sound : forall W fuel u k,
  In k (result_leaves W fuel u) -> ResultLeaf W u k.
Proof.
  intros W fuel u k Present. unfold result_leaves in Present.
  destruct (w_decl W u) as [d|] eqn:Declared; [|contradiction].
  destruct (d_ast d) as [| | | | | | | |n ps inputs outputs body] eqn:Ast;
    try contradiction.
  apply in_flat_map in Present as (sp & Member & Read).
  eapply RL; [exact Declared|exact Ast|exact Member|eapply result_spec_leaves_sound; exact Read].
Qed.

Local Lemma result_spec_leaves_more_fuel : forall W fuel p sp k,
  In k (result_spec_leaves W fuel p sp) -> In k (result_spec_leaves W (S fuel) p sp).
Proof.
  intros W fuel. induction fuel as [|fuel IH]; intros p [[pa|] [dn|] br] k Present;
    cbn [result_spec_leaves] in Present |- *; try contradiction; try exact Present.
  destruct (resolve_sym W p pa) as [s|] eqn:Resolve; [|contradiction].
  destruct (w_decl W s) as [d|] eqn:Declared; [|contradiction].
  destruct (d_ast d); try exact Present.
  apply in_flat_map in Present as (sp & Member & Read).
  apply in_flat_map. exists sp. split; [exact Member|now apply IH].
Qed.

Section SuccessfulReads.
Variable E : env.
Let W := e_W E.

Theorem result_spec_leaves_covered : forall fuel,
  (forall stack p ob tm sp es meanings k,
    output_item_entries E fuel stack p ob tm sp = Ok (es, meanings) ->
    ResultSpecLeaf W p sp k -> In k (result_spec_leaves W fuel p sp)) /\
  (forall stack p ob tm specs es meanings sp k,
    output_items_entries E fuel stack p ob tm specs = Ok (es, meanings) ->
    In sp specs -> ResultSpecLeaf W p sp k -> In k (result_spec_leaves W fuel p sp)).
Proof.
  induction fuel as [|fuel [Item Items]]; [split; intros; discriminate|].
  split.
  - intros stack p ob tm sp es meanings k Run Leaf. destruct Leaf as
      [p pa dn br s Resolve|p pa br s d Resolve Declared NotContext Missing|
       p pa br s d n context sp k Resolve Declared Ast Present Leaf].
    + cbn [result_spec_leaves]. now rewrite Resolve; left.
    + cbn [result_spec_leaves]. rewrite Resolve, Declared.
      destruct (d_ast d) eqn:Ast; try (rewrite Missing; now left).
      exfalso. eapply NotContext. reflexivity.
    + cbn [result_spec_leaves]. rewrite Resolve, Declared, Ast.
      cbn [output_item_entries] in Run. fold W in Run.
      rewrite Resolve in Run. cbn [rbind] in Run.
      unfold decl_of in Run. rewrite Declared in Run. cbn [rbind] in Run.
      apply rbind_ok in Run as (brackets & Brackets & Run). rewrite Ast in Run.
      destruct (nmem s stack); [discriminate|]. cbn [rbind] in Run.
      destruct (ctx_spec_items context) as [specs|] eqn:Specs; [|discriminate].
      cbn [rbind] in Run.
      apply in_flat_map. exists sp. split; [exact Present|].
      pose proof (ctx_spec_items_sound _ _ Specs) as Shape.
      assert (Flatten : flat_map citem_specs (map CISpec specs) = specs).
      { clear - specs. induction specs; cbn [map flat_map citem_specs app]; [reflexivity|now rewrite IHspecs]. }
      rewrite Shape, Flatten in Present.
      eapply Items; [exact Run|exact Present|exact Leaf].
  - intros stack p ob tm specs es meanings sp k Run Present Leaf.
    destruct specs as [|head specs]; [contradiction|].
    cbn [output_items_entries] in Run.
    apply rbind_ok in Run as ([head_entries head_meanings] & First & Run).
    apply rbind_ok in Run as ([tail_entries tail_meanings] & Rest & Run).
    apply result_spec_leaves_more_fuel.
    destruct Present as [<-|Present]; [eapply Item|eapply Items]; eassumption.
Qed.

Theorem result_leaves_covered_after_read : forall u C out C' ks k,
  provide_result E u C out = Ok (C', ks) ->
  ResultLeaf W u k -> In k (result_leaves W (e_fuel E) u).
Proof.
  intros u C out C' ks k Run Leaf. revert Run.
  destruct Leaf as [u d n ps a r b sp k Declared Ast Present Leaf]. intro Run.
  unfold provide_result, functor_decl in Run. fold W in Run. rewrite Declared in Run.
  unfold functor_parts in Run. rewrite Ast in Run. cbn [rbind] in Run.
  apply rbind_ok in Run as ([entries meanings] & Items & Run).
  unfold result_leaves. rewrite Declared, Ast.
  apply in_flat_map. exists sp. split; [exact Present|].
  eapply (proj2 (result_spec_leaves_covered (e_fuel E))); eassumption.
Qed.
End SuccessfulReads.
