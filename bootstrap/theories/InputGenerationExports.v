(* Runtime output meanings must be transferred from the body's source-name
   slots to the fresh generation used by this particular application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit InputGenerationRuntime.
Import ListNotations.
Local Open Scope list_scope.

(* Kept separately named while the executable integration is developed. *)
Definition frame_result_generations(theta:subst)(outputs:tele):subst:=
 flat_map(fun i=>match i with
 |ITy source=>match lookup_subst source theta with
   |Some(TAbs fresh)=>[(fresh,TAbs source)]|_=>[]end
 |_=>[]end)outputs.
Definition frame_result_environment(P:xprog)(theta:subst)(body:xdenv)(outputs:tele)(returned:xdenv):xdenv:=
 if xreify P then returned++xinst body(frame_result_generations theta outputs)++xresult_types outputs body
 else returned.

Lemma frame_result_generation_member : forall theta outputs source fresh,
 In(ITy source)outputs->lookup_subst source theta=Some(TAbs fresh)->
 In(fresh,TAbs source)(frame_result_generations theta outputs).
Proof.
 intros theta outputs source fresh Member Read;unfold frame_result_generations.
 apply in_flat_map;exists(ITy source);split;[exact Member|].
 rewrite Read;left;reflexivity.
Qed.
Lemma frame_result_generations_exact : forall theta outputs entry,
 In entry(frame_result_generations theta outputs)<->
 exists source fresh,entry=(fresh,TAbs source)/\In(ITy source)outputs/\
 lookup_subst source theta=Some(TAbs fresh).
Proof.
 intros theta outputs entry;split.
 - unfold frame_result_generations;intro Member;apply in_flat_map in Member as[item[Member Hit]].
   destruct item as[source| | | |];try contradiction.
   destruct(lookup_subst source theta)as[meaning|]eqn:Read;[|contradiction].
   destruct meaning as[|fresh| |];try contradiction.
   destruct Hit as[<-|[]];exists source,fresh;auto.
 - intros[source[fresh[-> [Member Read]]]];eapply frame_result_generation_member;eauto.
Qed.
Lemma frame_result_generation_read : forall theta outputs source fresh,
 NoDup(map fst(frame_result_generations theta outputs))->
 In(ITy source)outputs->lookup_subst source theta=Some(TAbs fresh)->
 lookup_subst fresh(frame_result_generations theta outputs)=Some(TAbs source).
Proof.
 intros;apply frame_lookup_member;[assumption|eapply frame_result_generation_member;eassumption].
Qed.
Lemma frame_output_prefix_types : forall returned,
 Forall(fun p=>is_ty_item(fst p)=false)returned->xdtypes returned=[].
Proof.
 intros returned;induction returned as[|[item entry]returned IH];intro Free;[reflexivity|].
 inversion Free as[|p ps Head Tail];subst;cbn in Head.
 destruct item;cbn in Head;try discriminate;cbn;apply IH;exact Tail.
Qed.
Lemma frame_output_prefix_lookup : forall returned g,
 Forall(fun p=>is_ty_item(fst p)=false)returned->lookup_xdenv(ITy g)returned=None.
Proof.
 intros returned;induction returned as[|[item entry]returned IH];intros g Free;[reflexivity|].
 inversion Free as[|p ps Head Tail];subst;cbn in Head.
 destruct item;cbn in Head;try discriminate;cbn;apply IH;exact Tail.
Qed.
Theorem frame_result_private_lookup : forall P theta body outputs returned source fresh,
 xreify P=true->Forall(fun p=>is_ty_item(fst p)=false)returned->
 NoDup(map fst(frame_result_generations theta outputs))->
 In(ITy source)outputs->lookup_subst source theta=Some(TAbs fresh)->
 lookup_xdenv(ITy fresh)(frame_result_environment P theta body outputs returned)=
 Some(XDType(xground body(TAbs source))).
Proof.
 intros P theta body outputs returned source fresh Live Free Unique Member Read.
 unfold frame_result_environment;rewrite Live,frame_environment_append,frame_output_prefix_lookup by exact Free.
 rewrite frame_inst_type_read,(frame_result_generation_read theta outputs source fresh Unique Member Read).
 reflexivity.
Qed.
Theorem frame_result_private_grounding : forall P theta body outputs returned source fresh,
 xreify P=true->Forall(fun p=>is_ty_item(fst p)=false)returned->
 NoDup(map fst(frame_result_generations theta outputs))->
 In(ITy source)outputs->lookup_subst source theta=Some(TAbs fresh)->
 xground(frame_result_environment P theta body outputs returned)(TAbs fresh)=
 xground body(TAbs source).
Proof.
 intros P theta body outputs returned source fresh Live Free Unique Member Read.
 unfold xground at 1;cbn[app_subst].
 unfold frame_result_environment;rewrite Live,frame_types_append,frame_output_prefix_types by exact Free.
 cbn[app];rewrite frame_inst_ground_read,(frame_result_generation_read theta outputs source fresh Unique Member Read).
 reflexivity.
Qed.
Lemma frame_result_uses_fresh_output : forall outputs inputs declared source fresh,
 NoDup(map fst outputs)->In(source,TAbs fresh)outputs->In(ITy source)declared->
 In(fresh,TAbs source)(frame_result_generations(outputs++inputs)declared).
Proof.
 intros;eapply frame_result_generation_member;[eassumption|].
 apply fresh_output_precedes_old_input;assumption.
Qed.
Example frame_output_shadowing_keeps_the_new_generation:
 frame_result_generations[(2,TAbs 20);(2,TNom 99[])][ITy 2]=[(20,TAbs 2)] /\
 xinst[(ITy 2,XDType(TNom 100[]))]
  (frame_result_generations[(2,TAbs 20);(2,TNom 99[])][ITy 2])=
 [(ITy 20,XDType(TNom 100[]))].
Proof. split;reflexivity. Qed.
