(* Actual fresh-frame slots number a lambda's parameters after its receiver. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore LambdaTrace LambdaOwnContext.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Lemma lambda_slot_names:forall(names:list string)start,
 map fst(combine names(seq start(List.length names)))=names.
Proof. induction names;intro start;cbn;[reflexivity|rewrite IHnames;reflexivity]. Qed.
Lemma lambda_slot_numbers:forall(names:list string)start,
 map snd(combine names(seq start(List.length names)))=seq start(List.length names).
Proof. induction names;intro start;cbn;[reflexivity|rewrite IHnames;reflexivity]. Qed.
Lemma lambda_slot_at:forall(names:list string)start i n,
 NoDup names->nth_error names i=Some n->
 sassoc n(combine names(seq start(List.length names)))=Some(start+i).
Proof.
 intro names;induction names as[|first names IH];intros start i n Nd At;destruct i;cbn in At;try discriminate.
 - injection At as <-;cbn[List.length sassoc assoc combine seq];rewrite String.eqb_refl;f_equal;lia.
 - inversion Nd as[|x xs Absent Tail];subst.
   cbn[List.length sassoc assoc combine seq];destruct(String.eqb n first)eqn:Eq.
   + apply String.eqb_eq in Eq;subst n;exfalso;apply Absent;eapply nth_error_In;exact At.
   + rewrite(IH(S start)i n Tail At);f_equal;lia.
Qed.
Lemma lambda_slots_shape:forall ps this s,
 NoDup(map fst(lambda_own_slots ps this))->
 VShape(s_vars(lambda_own_slots ps this)(List.length(lambda_own_slots ps this))s).
Proof.
 intros ps this s Nd;split;[exact Nd|];split;[|reflexivity].
 change(map snd(lambda_own_slots ps this)=seq 0(List.length(lambda_own_slots ps this))).
 destruct this;cbn[lambda_own_slots app List.length map snd].
 - rewrite lambda_slot_numbers,combine_length,seq_length,Nat.min_id;reflexivity.
 - rewrite lambda_slot_numbers,combine_length,seq_length,Nat.min_id;reflexivity.
Qed.
Lemma lambda_slots_at:forall ps this i n,
 NoDup(map fst(lambda_own_slots ps this))->nth_error(map fst ps)i=Some n->
 sassoc n(lambda_own_slots ps this)=Some(match this with Some _=>S i|None=>i end).
Proof.
 intros ps this i n Nd At;destruct this;cbn[lambda_own_slots app List.length map fst]in *.
 - rewrite lambda_slot_names in Nd;inversion Nd as[|x xs Absent Tail];subst.
   cbn[sassoc assoc fst snd];destruct(String.eqb n "this"%string)eqn:Eq.
   + apply String.eqb_eq in Eq;subst n;exfalso;apply Absent;eapply nth_error_In;exact At.
   + exact(lambda_slot_at(map fst ps)1 i n Tail At).
 - rewrite lambda_slot_names in Nd;exact(lambda_slot_at(map fst ps)0 i n Nd At).
Qed.
Theorem lambda_numbering_final:forall Fl original F sg ps this initial last,
 st_vars initial=lambda_own_slots ps this->NoDup(map fst(lambda_own_slots ps this))->
 VShape last->Fr initial last->
 A.Numbering(lambda_body_frame Fl original F sg this(st_vars last))
 (option_map(fun t=>tr Fl(canon original t))this)(map fst ps).
Proof.
 intros Fl original F sg ps this initial last Vars Nd[Names[Slots Count]]Frame.
 unfold A.Numbering;cbn[lambda_body_frame A.fr_var];split;[exact Names|];split.
 - change(NoDup(map snd(st_vars last)));rewrite Slots;apply seq_NoDup.
 - intros i n At;rewrite slook_sassoc.
   destruct(fx_vars _ _(fr_x _ _ Frame))as[extra Eq];rewrite Eq,Vars;apply sassoc_app_some.
   pose proof(lambda_slots_at ps this i n Nd At)as Read.
   destruct this;exact Read.
Qed.
