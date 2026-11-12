(* Successful executable pattern reads satisfy the audited pattern rules. *)
From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaExpr EaTypesSound.
From NM Require Import LegacyBodyJudgments.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope list_scope.

Lemma member_with_head_sound : forall st symbol arguments,
  member_with_head st symbol = Some arguments -> In (TNom symbol arguments) (members st).
Proof.
  intros st symbol arguments Run. unfold member_with_head in Run.
  destruct (find (fun member => match member with TNom head _ => Nat.eqb symbol head | _ => false end)
    (members st)) as [member|] eqn:Found; [|discriminate].
  destruct member; try discriminate. injection Run as <-.
  apply find_some in Found as [Member Equal]. apply Nat.eqb_eq in Equal. now subst.
Qed.

Section Patterns.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.

Theorem field_pats_sound : forall F tag types syntax bindings fields,
  field_pats E F tag types syntax = Ok (bindings, fields) ->
  FieldPats W F types syntax bindings fields.
Proof.
  intros F tag types syntax. induction syntax as [|[name pattern] rest IH]; intros bindings fields Run.
  - cbn [field_pats] in Run. injection Run as <- <-. constructor.
  - cbn [field_pats] in Run.
    destruct (slook name types) as [t|] eqn:Field; cbn [rbind] in Run; [|discriminate].
    apply rbind_ok in Run as [[tail_bindings tail_fields] [Tail Run]].
    pose proof (IH _ _ Tail) as TailSound.
    destruct pattern as [pattern|].
    + destruct pattern as [|path|path payload|path entries]; try discriminate.
      * injection Run as <- <-. eapply FPWild; eassumption.
      * destruct path as [|binder [|next path]]; try discriminate.
        fold W in Run.
        destruct (resolve W (fr_mod F) [binder]) eqn:Unbound; cbn [is_some negb rbind] in Run; [discriminate|].
        destruct (slook binder (fr_var F)) eqn:Numbered; cbn [is_some rbind] in Run; [|discriminate].
        injection Run as <- <-. eapply FPBind; [exact Field|exact Unbound| |exact TailSound].
        unfold Named. rewrite Numbered. discriminate.
    + destruct (slook name (fr_var F)) eqn:Numbered; cbn [is_some rbind] in Run; [|discriminate].
      injection Run as <- <-. eapply FPPun; [exact Field| |exact TailSound].
      unfold Named. rewrite Numbered. discriminate.
Qed.

(* Abstract the messages so the local tag reader's successful path can be
   reused by path, payload and record patterns. *)
Lemma tag_member_read_sound : forall F st path no_name bad_kind no_member symbol arguments,
  (let! s := ropt (resolve_sym W (fr_mod F) path) no_name in
   let! _ := rcheck (kind_is W s DKTag) bad_kind in
   let! xs := ropt (member_with_head st s) (no_member s) in
   Ok (s, xs)) = Ok (symbol, arguments) ->
  resolve_sym W (fr_mod F) path = Some symbol /\
  kind_sym W symbol = Some DKTag /\
  In (TNom symbol arguments) (members st).
Proof.
  intros F st path no_name bad_kind no_member symbol arguments Run.
  destruct (resolve_sym W (fr_mod F) path) as [s|] eqn:Resolve; cbn [rbind] in Run; [|discriminate].
  destruct (kind_is W s DKTag) eqn:Kind; cbn [rbind] in Run; [|discriminate].
  destruct (member_with_head st s) as [xs|] eqn:Member; cbn [rbind] in Run; [|discriminate].
  injection Run as <- <-. split; [reflexivity|]. split.
  - now apply kind_is_spec in Kind.
  - exact (member_with_head_sound _ _ _ Member).
Qed.

Theorem pat_of_sound : forall F st syntax pattern bindings covered catch,
  pat_of E F st syntax = Ok (pattern, bindings, covered, catch) ->
  PatOf W F st syntax pattern bindings covered catch.
Proof.
  intros F st syntax pattern bindings covered catch Run.
  destruct syntax as [|path|path payload|path fields].
  - unfold pat_of in Run. injection Run as <- <- <- <-. constructor.
  - destruct path as [|name [|next rest]]; [discriminate| |].
    1: { unfold pat_of in Run. fold W fuel in Run.
         destruct (resolve W (fr_mod F) [name]) eqn:Global.
         - apply rbind_ok in Run as [[symbol arguments] [Read Run]].
           apply tag_member_read_sound in Read as [Resolve [Kind Member]].
           injection Run as <- <- <- <-.
           eapply PNominal; eassumption.
         - destruct (slook name (fr_var F)) eqn:Numbered; cbn [is_some rbind] in Run; [|discriminate].
           injection Run as <- <- <- <-. apply PBinder; [exact Global|].
           unfold Named. rewrite Numbered. discriminate. }
    unfold pat_of in Run. fold W fuel in Run.
    apply rbind_ok in Run as [[symbol arguments] [Read Run]].
    apply tag_member_read_sound in Read as [Resolve [Kind Member]].
    injection Run as <- <- <- <-.
    eapply PNominal; eassumption.
  - destruct payload as [|payload|head payload|head fields]; try discriminate.
    destruct payload as [|binder [|next rest]]; try discriminate.
    unfold pat_of in Run. fold W fuel in Run.
    apply rbind_ok in Run as [[symbol arguments] [Read Run]].
    apply tag_member_read_sound in Read as [Resolve [Kind Member]].
    destruct (bare_tagb W symbol) eqn:Bare; cbn [rbind negb] in Run; [discriminate|].
    apply rbind_ok in Run as [payload_type [Payload Run]].
    destruct (resolve W (fr_mod F) [binder]) eqn:Unbound; cbn [is_some negb rbind] in Run; [discriminate|].
    destruct (slook binder (fr_var F)) eqn:Numbered; cbn [is_some rbind] in Run; [|discriminate].
    injection Run as <- <- <- <-.
    eapply PTagBind; [exact Resolve|intro H; apply bare_tagb_spec in H; congruence|exact Member|
      eapply payload_of_sound; exact Payload|exact Unbound|].
    unfold Named. rewrite Numbered. discriminate.
  - destruct path as [path|]; [|discriminate]. unfold pat_of in Run. fold W fuel in Run.
    apply rbind_ok in Run as [[symbol arguments] [Read Run]].
    apply tag_member_read_sound in Read as [Resolve [Kind Member]].
    destruct (bare_tagb W symbol) eqn:Bare; cbn [rbind negb] in Run; [discriminate|].
    apply rbind_ok in Run as [payload_type [Payload Run]].
    destruct payload_type as [machine|types| |abstract|tag type_arguments|alternatives]; try discriminate.
    destruct (record_pattern_arity fields types) eqn:Arity; cbn [rbind] in Run; [|discriminate].
    apply rbind_ok in Run as [[names vars] [Fields Run]].
    destruct (snodupb (map fst names)) eqn:Distinct; cbn [rbind] in Run; [|discriminate].
    injection Run as <- <- <- <-.
    eapply PRecord; [exact Resolve|exact Kind|exact Member|
      eapply payload_of_sound; exact Payload|exact Arity|eapply field_pats_sound; exact Fields|apply snodupb_sound; exact Distinct].
Qed.

End Patterns.
