(* Installing actual signature parameters preserves the complete body context. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySound BodyTr CanonTypes NominalSupport FnBodyContext FnBodySlots.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Local Open Scope list_scope.
Definition body_parameter_types Fl e (parameters:list(name*ety)):=
 map(fun p=>(fst p,tr Fl(canon e(snd p))))parameters.
Lemma body_parameter_context:forall w R Fl c e F C parameters,
 CxRel w R Fl c e F C->Forall(fun p=>ety_ok(snd p)=true)parameters->
 Forall(fun p=>Support(BodySound.W w R Fl)(snd p))parameters->
 CxRel w R Fl c
 (fold_left(fun acc p=>set_local acc(fst p)(snd p)Elab.LLet)parameters e)F
 (fold_left(fun acc p=>A.add_local acc(fst p)(snd p)A.LLet)(body_parameter_types Fl e parameters)C).
Proof.
 intros w R Fl c e F C parameters;revert e C.
 induction parameters as[|[n t]parameters IH];intros e C Context Clean Supported.
 - exact Context.
 - inversion Clean as[|p ps Good Rest];subst;inversion Supported as[|p ps Support RestSupport];subst.
   cbn[fold_left body_parameter_types map fst snd].
   pose proof(cxrel_local w R Fl c e F C n t Elab.LLet Context(ety_ok_wf t Good)Support)as Next.
   cbn[tr_mut]in Next.
   pose proof(IH(set_local e n t Elab.LLet)_ Next Rest RestSupport)as Done.
   exact Done.
Qed.

Lemma parameter_slots_fst:forall(names:list string)start,
 map fst(combine names(seq start(List.length names)))=names.
Proof. intros names;induction names;intro start;cbn;[reflexivity|rewrite IHnames;reflexivity]. Qed.
Lemma parameter_slots_snd:forall(names:list string)start,
 map snd(combine names(seq start(List.length names)))=seq start(List.length names).
Proof. intros names;induction names;intro start;cbn;[reflexivity|rewrite IHnames;reflexivity]. Qed.
Lemma parameter_slots_at:forall(names:list string)start i n,
 NoDup names->nth_error names i=Some n->
 sassoc n(combine names(seq start(List.length names)))=Some(start+i).
Proof.
 intros names;induction names as[|first names IH];intros start i n Nd At;destruct i;cbn in At;try discriminate.
 - injection At as <-;cbn[List.length sassoc assoc combine seq];rewrite String.eqb_refl;f_equal;lia.
 - inversion Nd as[|x xs Absent Tail];subst.
   cbn[List.length sassoc assoc combine seq]. destruct(String.eqb n first)eqn:Equal.
   + apply String.eqb_eq in Equal;subst n;exfalso;apply Absent;eapply nth_error_In;exact At.
   + rewrite(IH(S start)i n Tail At);f_equal;lia.
Qed.
Lemma parameter_numbering:forall w Fl g e sg names,
 NoDup names->
 A.Numbering(initial_body_frame w Fl g e sg(combine names(seq 0(List.length names))))None names.
Proof.
 intros w Fl g e sg names Nd;unfold A.Numbering;cbn[A.fr_var initial_body_frame];split.
 - rewrite parameter_slots_fst;exact Nd.
 - split;[rewrite parameter_slots_snd;apply seq_NoDup|].
   intros i n At;rewrite slook_sassoc;change(sassoc n(combine names(seq 0(List.length names)))=Some i).
   exact(parameter_slots_at names 0 i n Nd At).
Qed.
Definition body_parameter_slots := FnBodySlots.body_parameter_slots.
Lemma parameter_numbering_any:forall w Fl g e sg this names,
 NoDup(map fst(body_parameter_slots this names))->
 A.Numbering(initial_body_frame w Fl g e sg(body_parameter_slots this names))
 (option_map(fun t=>tr Fl(canon e t))this)names.
Proof.
 intros w Fl g e sg this names Nd;unfold A.Numbering;cbn[A.fr_var initial_body_frame].
 split;[exact Nd|];split.
 - destruct this;cbn[body_parameter_slots FnBodySlots.body_parameter_slots map fst snd app List.length]in *.
   + rewrite parameter_slots_snd. change(NoDup(seq 0(S(List.length names))));apply seq_NoDup.
   + rewrite parameter_slots_snd;apply seq_NoDup.
 - intros i n At;rewrite slook_sassoc.
   destruct this;cbn[body_parameter_slots FnBodySlots.body_parameter_slots app List.length option_map]in *.
   + cbn[map fst]in Nd;rewrite parameter_slots_fst in Nd.
     inversion Nd as[|x xs Absent Tail];subst.
     cbn[sassoc assoc fst snd]. destruct(String.eqb n "this"%string)eqn:Eq.
     * apply String.eqb_eq in Eq;subst n;exfalso;apply Absent;eapply nth_error_In;exact At.
     * exact(parameter_slots_at names 1 i n Tail At).
   + rewrite parameter_slots_fst in Nd;exact(parameter_slots_at names 0 i n Nd At).
Qed.
Lemma body_parameter_shape:forall this names,
 NoDup(map fst(body_parameter_slots this names))->forall s,
 VShape(s_vars(body_parameter_slots this names)(List.length(body_parameter_slots this names))s).
Proof.
 intros this names Nd s;split;[exact Nd|];split;[|reflexivity].
 change(map snd(body_parameter_slots this names)=seq 0(List.length(body_parameter_slots this names))).
 destruct this;cbn[body_parameter_slots FnBodySlots.body_parameter_slots app List.length map snd].
 - rewrite parameter_slots_snd,combine_length,seq_length,Nat.min_id;reflexivity.
 - rewrite parameter_slots_snd,combine_length,seq_length,Nat.min_id;reflexivity.
Qed.
Lemma parameter_numbering_final:forall w Fl g e sg this names initial final,
 st_vars initial=body_parameter_slots this names->
 NoDup(map fst(body_parameter_slots this names))->VShape final->Fr initial final->
 A.Numbering(initial_body_frame w Fl g e sg(st_vars final))
 (option_map(fun t=>tr Fl(canon e t))this)names.
Proof.
 intros w Fl g e sg this names initial final Vars Nd[Names[Slots Count]]Frame.
 unfold A.Numbering;cbn[A.fr_var initial_body_frame];split;[exact Names|];split.
 - change(NoDup(map snd(st_vars final)));rewrite Slots;apply seq_NoDup.
 - intros i n At;rewrite slook_sassoc.
   destruct Frame as[[World Next Keep New Above Fields[[extra Eq] Shape Rest]] Rel Ext].
   rewrite Eq,Vars;apply sassoc_app_some.
   pose proof(parameter_numbering_any w Fl g e sg this names Nd)as[_[_ Numbered]].
   specialize(Numbered i n At);rewrite slook_sassoc in Numbered;exact Numbered.
Qed.
Lemma body_parameter_uf:forall parameters e,
 en_uf(fold_left(fun acc p=>set_local acc(fst p)(snd p)Elab.LLet)parameters e)=en_uf e.
Proof. induction parameters as[|[n t]parameters IH];intro e;[reflexivity|cbn[fold_left];rewrite IH;reflexivity]. Qed.
Lemma body_parameter_canon:forall parameters e t,
 canon(fold_left(fun acc p=>set_local acc(fst p)(snd p)Elab.LLet)parameters e)t=canon e t.
Proof. intros;apply canon_uf,body_parameter_uf. Qed.

Lemma body_parameter_types_combine:forall Fl e parameters,
 body_parameter_types Fl e parameters=combine(map fst parameters)(map(fun p=>tr Fl(canon e(snd p)))parameters).
Proof.
 intros Fl e parameters;induction parameters as[|[n t]parameters IH];[reflexivity|].
 change((n,tr Fl(canon e t))::body_parameter_types Fl e parameters=
 (n,tr Fl(canon e t))::combine(map fst parameters)(map(fun p=>tr Fl(canon e(snd p)))parameters)).
 f_equal;exact IH.
Qed.
