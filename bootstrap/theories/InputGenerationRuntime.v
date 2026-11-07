(* Explicit transfer data for a privately scoped linked functor. These pure
   helpers specify transfer order without changing the language semantics. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit.
Import ListNotations.
Local Open Scope list_scope.

Record link_input_frame:Type:=LinkInputFrame{
 link_frame_inputs:subst;             (* private generation -> formal source type *)
 link_frame_captures:subst;           (* private generation -> caller type *)
 link_frame_names:subst;              (* source type name -> private type *)
 link_frame_renaming:subst            (* old generation -> private type *)
}.
(* Applications carry fresh outputs before their unique input arguments.
   Reading from the right recovers the input's meaning before output shadowing. *)
Definition old_input_arguments(theta:subst):subst:=rev theta.
Definition link_supplied_inputs(frame:link_input_frame)(theta:subst):subst:=
 map(fun p=>(fst p,app_subst(old_input_arguments theta)(snd p)))(link_frame_inputs frame).
Definition link_capture_environment(frame:link_input_frame)(caller captured:xdenv):xdenv:=
 xinst caller(link_frame_captures frame)++xclose caller captured.
Definition link_apply_base(caller captured:xdenv)(supplied:subst):xdenv:=
 xinst caller supplied++xclose caller captured.
Definition link_apply_environment(frame:link_input_frame)(caller captured:xdenv)(supplied:subst):xdenv:=
 let base:=link_apply_base caller captured supplied in
 xinst base(link_frame_names frame)++base.
Definition link_export_arguments(supplied theta:subst):subst:=supplied++theta.

Lemma frame_lookup_append : forall g left right,
 lookup_subst g(left++right)=
 match lookup_subst g left with Some t=>Some t|None=>lookup_subst g right end.
Proof.
 intros g left;induction left as[|[name t]left IH];intro right;cbn;[reflexivity|].
 destruct(Nat.eqb g name);[reflexivity|exact(IH right)].
Qed.
Lemma frame_lookup_member : forall xs g t,
 NoDup(map fst xs)->In(g,t)xs->lookup_subst g xs=Some t.
Proof.
 intros xs;induction xs as[|[name raw]xs IH];intros g t Unique Hit;[contradiction|].
 inversion Unique as[|n ns Absent Tail];subst.
 cbn[lookup_subst];destruct Hit as[Eq|Hit].
 - injection Eq as <- <-;rewrite Nat.eqb_refl;reflexivity.
 - destruct(Nat.eqb g name)eqn:Eq;[|apply IH;assumption].
   apply Nat.eqb_eq in Eq;subst g;exfalso;apply Absent.
   apply in_map with(f:=@fst tsym ty)in Hit;exact Hit.
Qed.
Lemma frame_lookup_absent : forall xs g,
 ~In g(map fst xs)->lookup_subst g xs=None.
Proof.
 intros xs;induction xs as[|[name raw]xs IH];intros g Absent;cbn;[reflexivity|].
 destruct(Nat.eqb g name)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst;exfalso;apply Absent;left;reflexivity.
 - apply IH;intro Hit;apply Absent;right;exact Hit.
Qed.
Lemma old_input_survives_output_shadowing : forall outputs inputs g old,
 NoDup(map fst inputs)->In(g,old)inputs->
 lookup_subst g(old_input_arguments(outputs++inputs))=Some old.
Proof.
 intros outputs inputs g old Unique Hit;unfold old_input_arguments;rewrite rev_app_distr,frame_lookup_append.
 assert(Read:lookup_subst g(rev inputs)=Some old).
 { apply frame_lookup_member.
   - rewrite map_rev;apply NoDup_rev;exact Unique.
   - apply(proj1(in_rev inputs(g,old)));exact Hit. }
 rewrite Read;reflexivity.
Qed.
Lemma fresh_output_precedes_old_input : forall outputs inputs g fresh,
 NoDup(map fst outputs)->In(g,fresh)outputs->
 lookup_subst g(outputs++inputs)=Some fresh.
Proof. intros;rewrite frame_lookup_append,(frame_lookup_member _ _ _ H H0);reflexivity. Qed.
Lemma link_supplied_input_read : forall frame theta private source,
 lookup_subst private(link_frame_inputs frame)=Some source->
 lookup_subst private(link_supplied_inputs frame theta)=
 Some(app_subst(old_input_arguments theta)source).
Proof.
 intros frame theta private source Read;unfold link_supplied_inputs.
 assert(MapRead:forall xs,lookup_subst private
  (map(fun p=>(fst p,app_subst(old_input_arguments theta)(snd p)))xs)=
  option_map(app_subst(old_input_arguments theta))(lookup_subst private xs)).
 { intro xs;induction xs as[|[name raw]xs IH];cbn;[reflexivity|].
   destruct(Nat.eqb private name);[reflexivity|exact IH]. }
 rewrite MapRead,Read;reflexivity.
Qed.
Lemma link_export_supplied_first : forall supplied theta private meaning,
 lookup_subst private supplied=Some meaning->
 lookup_subst private(link_export_arguments supplied theta)=Some meaning.
Proof. intros;unfold link_export_arguments;rewrite frame_lookup_append,H;reflexivity. Qed.
Lemma link_export_preserves_other_arguments : forall supplied theta name,
 ~In name(map fst supplied)->
 lookup_subst name(link_export_arguments supplied theta)=lookup_subst name theta.
Proof. intros;unfold link_export_arguments;rewrite frame_lookup_append,frame_lookup_absent by exact H;reflexivity. Qed.

Lemma frame_inst_map : forall caller theta,
 xinst caller theta=map(fun p=>(ITy(fst p),XDType(xground caller(snd p))))theta.
Proof. intros caller [];reflexivity. Qed.
Lemma frame_inst_type_read : forall caller theta rest g,
 lookup_xdenv(ITy g)(xinst caller theta++rest)=
 match lookup_subst g theta with
 |Some raw=>Some(XDType(xground caller raw))
 |None=>lookup_xdenv(ITy g)rest end.
Proof.
 intros caller theta;rewrite frame_inst_map.
 induction theta as[|[name raw]theta IH];intros rest g;cbn;[reflexivity|].
 destruct(Nat.eqb g name);[reflexivity|apply IH].
Qed.
Lemma frame_environment_append : forall item first second,
 lookup_xdenv item(first++second)=
 match lookup_xdenv item first with Some entry=>Some entry|None=>lookup_xdenv item second end.
Proof.
 intros item first;induction first as[|[key entry]first IH];intro second;cbn;[reflexivity|].
 destruct(item_eqb item key);[reflexivity|exact(IH second)].
Qed.
Lemma link_capture_read : forall frame caller captured private source,
 lookup_subst private(link_frame_captures frame)=Some source->
 lookup_xdenv(ITy private)(link_capture_environment frame caller captured)=
 Some(XDType(xground caller source)).
Proof. intros;unfold link_capture_environment;rewrite frame_inst_type_read,H;reflexivity. Qed.
Lemma link_apply_preserves_capture : forall frame current captured supplied private frozen,
 ~In private(map fst(link_frame_names frame))->
 ~In private(map fst supplied)->
 lookup_xdenv(ITy private)captured=Some(XDType frozen)->
 lookup_xdenv(ITy private)(link_apply_environment frame current captured supplied)=Some(XDType frozen).
Proof.
 intros frame current captured supplied private frozen Names Inputs Captured.
 unfold link_apply_environment;rewrite frame_inst_type_read,frame_lookup_absent by exact Names.
 unfold link_apply_base;rewrite frame_inst_type_read,frame_lookup_absent by exact Inputs.
 unfold xclose;rewrite frame_environment_append,Captured;reflexivity.
Qed.
Theorem private_capture_survives_caller_rebinding : forall frame old current captured supplied private source,
 lookup_subst private(link_frame_captures frame)=Some source->
 ~In private(map fst(link_frame_names frame))->~In private(map fst supplied)->
 lookup_xdenv(ITy private)
  (link_apply_environment frame current(link_capture_environment frame old captured)supplied)=
 Some(XDType(xground old source)).
Proof. intros;eapply link_apply_preserves_capture;eauto using link_capture_read. Qed.

Lemma link_supplied_input_labels : forall frame theta,
 map fst(link_supplied_inputs frame theta)=map fst(link_frame_inputs frame).
Proof. intros;unfold link_supplied_inputs;rewrite map_map;apply map_ext;intros [name t];reflexivity. Qed.
Lemma frame_types_append : forall first second,
 xdtypes(first++second)=xdtypes first++xdtypes second.
Proof.
 induction first as[|[item entry]first IH];intro second;[reflexivity|].
 destruct item,entry;cbn;rewrite IH;reflexivity.
Qed.
Lemma frame_inst_ground_read : forall caller theta rest g,
 lookup_subst g(xdtypes(xinst caller theta++rest))=
 match lookup_subst g theta with
 |Some raw=>Some(xground caller raw)
 |None=>lookup_subst g(xdtypes rest)end.
Proof.
 intros caller theta;rewrite frame_inst_map.
 induction theta as[|[name raw]theta IH];intros rest g;cbn;[reflexivity|].
 destruct(Nat.eqb g name);[reflexivity|apply IH].
Qed.
Theorem private_capture_grounding_survives_caller_rebinding : forall frame old current captured supplied private source,
 lookup_subst private(link_frame_captures frame)=Some source->
 ~In private(map fst(link_frame_names frame))->~In private(map fst supplied)->
 xground(link_apply_environment frame current(link_capture_environment frame old captured)supplied)(TAbs private)=
 xground old source.
Proof.
 intros frame old current captured supplied private source Capture Names Supplied.
 unfold xground at 1;cbn[app_subst].
 unfold link_apply_environment.
 rewrite frame_inst_ground_read,frame_lookup_absent by exact Names.
 unfold link_apply_base;rewrite frame_inst_ground_read,frame_lookup_absent by exact Supplied.
 unfold xclose;rewrite frame_types_append,frame_lookup_append.
 unfold link_capture_environment;rewrite frame_inst_ground_read,Capture;reflexivity.
Qed.
Lemma frame_body_name_rebind_preserves_capture : forall P d name private meaning,
 private<>name->
 lookup_xdenv(ITy private)(xbind_ty P d name meaning)=lookup_xdenv(ITy private)d.
Proof.
 intros P d name private meaning Different;unfold xbind_ty;destruct(xreify P);[|reflexivity].
 cbn[lookup_xdenv item_eqb];destruct(Nat.eqb private name)eqn:Eq;[|reflexivity].
 apply Nat.eqb_eq in Eq;contradiction.
Qed.

Definition runtime_shadow_frame:link_input_frame:=
 LinkInputFrame [(10,TAbs 2)] [(11,TAbs 7)] [(2,TAbs 10)] [(2,TAbs 10)].
Example private_input_recovers_shadowed_source_argument:
 link_supplied_inputs runtime_shadow_frame[(2,TAbs 3);(2,TNom 99[])]=[(10,TNom 99[])].
Proof. reflexivity. Qed.
Definition runtime_capture_frame:link_input_frame:=
 LinkInputFrame [] [(10,TAbs 2)] [(2,TAbs 10)] [(2,TAbs 10)].
Example private_capture_is_older_than_callers_rebinding:
 let old:=[(ITy 2,XDType(TNom 99[]))] in
 let current:=[(ITy 2,XDType(TNom 100[]))] in
 let closed:=link_capture_environment runtime_capture_frame old [] in
 let inside:=link_apply_environment runtime_capture_frame current closed [] in
 lookup_xdenv(ITy 10)inside=Some(XDType(TNom 99[])) /\
 lookup_xdenv(ITy 2)inside=Some(XDType(TNom 99[])) /\
 xground inside(TAbs 10)=TNom 99[].
Proof. repeat split;reflexivity. Qed.
