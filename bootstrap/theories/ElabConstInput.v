(* Cache induction with a genuine input provenance invariant.
   InfoInv travels forward through actual prefixes (okI); unlike the final
   static facts in ca_L, it is never pulled backwards from the last state. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv InfoInvariant InfoPreserve BodySound FwdInvariant FwdReadonly FwdPreserve FwdSynthInv FwdProgramInv.
Import ListNotations.
Lemma info_reqs_ni:forall x,NI(reqs x).
Proof. intros;unfold reqs;apply ni_type_requirements. Qed.
#[export]Hint Resolve info_reqs_ni:nidb.
Local Strategy opaque [slots_go elab_type app_subst symbol_type reqs type_requirements].
Lemma input_merged_ni:forall mi walk,(forall e,NI(walk e))->NI(merged_walk mi walk).
Proof. intros mi walk H;unfold merged_walk;nit. Qed.
#[export]Hint Extern 1 (NI(merged_walk _ _))=>apply input_merged_ni;intros;nit:nidb.
Local Strategy opaque [merged_walk].
Lemma input_fun_env_ok:forall g,ok(fun_env g).
Proof. intros;unfold fun_env;okt. all:apply ok_merged_walk_frame;intros;okt. Qed.
Lemma input_fun_env_ni:forall g,NI(fun_env g).
Proof. intros;unfold fun_env;nit. Qed.
#[export]Hint Resolve input_fun_env_ok:okdb.
#[export]Hint Resolve input_fun_env_ni:nidb.
Lemma info_ety_of:forall t mi tm e this,okI(ety_of t mi tm e this).
Proof. intros;apply okI_annotation_type. Qed.
#[export]Hint Resolve info_ety_of:okIdb.
Lemma input_ety_ni:forall t mi tm e this,NI(ety_of t mi tm e this).
Proof. intros;unfold ety_of;nit. Qed.
#[local]Hint Resolve input_ety_ni:nidb.
#[local]Hint Resolve ok_ety_of:okdb.
Lemma input_payload_type_ok:forall t e,ok(payload_type t e).
Proof. intros;unfold payload_type;okt. Qed.
Lemma input_payload_type_ni:forall t e,NI(payload_type t e).
Proof. intros;unfold payload_type;nit. Qed.
#[export]Hint Resolve input_payload_type_ok:okdb.
#[export]Hint Resolve input_payload_type_ni:nidb.
Lemma input_check_relevant_ok:forall mi g a,ok(check_relevant mi g a).
Proof. intros;unfold check_relevant;okt. Qed.
Lemma input_check_relevant_ni:forall mi g a,NI(check_relevant mi g a).
Proof. intros;unfold check_relevant;nit. Qed.
#[export]Hint Resolve input_check_relevant_ok:okdb.
#[export]Hint Resolve input_check_relevant_ni:nidb.
Lemma input_fn_sig_ok:forall g tm e this,ok(fn_sig g tm e this).
Proof. intros;unfold fn_sig;okt. Qed.
Lemma input_fn_sig_ni:forall g tm e this,NI(fn_sig g tm e this).
Proof. intros;unfold fn_sig;nit. Qed.
#[export]Hint Resolve input_fn_sig_ok:okdb.
#[export]Hint Resolve input_fn_sig_ni:nidb.
Lemma info_compile_pattern:forall mi e p sty heads,okI(compile_pattern mi e p sty heads).
Proof. intros;apply okI_ni;[unfold compile_pattern;okt|unfold compile_pattern;nit|unfold compile_pattern;keepst]. Qed.
#[export]Hint Resolve info_compile_pattern:okIdb.
Local Strategy opaque [compile_pattern].
Definition okqi(C:Cache){A}(m:M A):Prop:=
 ok m/\forall s a s',Inv s->InfoInv s->FwdInv s->m s=Good(a,s')->ca_L C s'->ca_Q C s->ca_Q C s'.
Arguments okqi:simpl never.
Section CacheInput.
Variable C:Cache.
Lemma okqi_keeps:forall A(m:M A),ok m->keeps(ca_proj C)m->okqi C m.
Proof.
 intros A m O K. split;[exact O|]. intros s a s' Hi Ii Fi H L Q.
 destruct(O _ _ _ Hi H)as[_ F]. exact(ca_Q_keep C s s' Hi F(K _ _ _ H)L Q).
Qed.
Lemma okqi_ret:forall A(a:A),okqi C(ret a).
Proof. intros;apply okqi_keeps;[apply ok_ret|apply keeps_ret]. Qed.
Lemma okqi_bind:forall A B(m:M A)(k:A->M B),
 okI m->okF m->okqi C m->(forall a,okqi C(k a))->okqi C(mbind m k).
Proof.
 intros A B m k Im Fm[Om Qm]Hk. split;[apply ok_bind;[exact Om|intro a;exact(proj1(Hk a))]|].
 intros s b s'' Hi Ii Fi H L Q. apply bind_inv in H as(a&s'&H1&H2).
 destruct(Om _ _ _ Hi H1)as[Hi' _]. destruct(Hk a)as[Ok Qk].
 destruct(Ok _ _ _ Hi' H2)as[_ F2].
 exact(Qk s' b s'' Hi'(proj2 Im _ _ _ Hi Ii H1)(proj2 Fm _ _ _ Hi Fi H1)H2 L(Qm s a s' Hi Ii Fi H1(ca_L_back C _ _ F2 L)Q)).
Qed.
Lemma okqi_mmap:forall A B(f:A->M B)l,
 (forall x,okI(f x))->(forall x,okF(f x))->(forall x,okqi C(f x))->okqi C(mmap f l).
Proof.
 intros A B f l If Ff Qf. induction l as[|x r IH];cbn[mmap];[apply okqi_ret|].
 apply okqi_bind;[apply If|apply Ff|apply Qf|intro y;apply okqi_bind;[apply okI_mmap;exact If|apply okF_mmap;exact Ff|exact IH|intro;apply okqi_ret]].
Qed.
Lemma okqi_mfold:forall A B(f:A->B->M A)l a,
 (forall a x,okI(f a x))->(forall a x,okF(f a x))->(forall a x,okqi C(f a x))->okqi C(mfold f a l).
Proof.
 intros A B f l. induction l as[|x r IH];intros a If Ff Qf;cbn[mfold];[apply okqi_ret|].
 apply okqi_bind;[apply If|apply Ff|apply Qf|intro a';apply IH;assumption].
Qed.
Lemma okqi_miter:forall A(f:A->M unit)l,
 (forall x,okI(f x))->(forall x,okF(f x))->(forall x,okqi C(f x))->okqi C(miter f l).
Proof.
 intros A f l If Ff Qf. induction l as[|x r IH];cbn[miter];[apply okqi_ret|].
 apply okqi_bind;[apply If|apply Ff|apply Qf|intro;exact IH].
Qed.
Lemma okqi_with_vars_inv:forall A init(m:M A)s a s',
 Inv s->with_vars init m s=Good(a,s')->
 exists s3,m(s_vars init(List.length init)s)=Good(a,s3)/\s'=s_vars(st_vars s)(st_nvar s)s3/\Inv(s_vars init(List.length init)s).
Proof.
 intros A init m s a s' Hi H. unfold with_vars in H.
 apply bind_inv in H as(s0&s1&E0&H);injection E0 as <- <-.
 apply bind_inv in H as(u&s2&E1&H);unfold upd in E1;injection E1 as <- <-.
 apply bind_inv in H as(r&s3&H3&H).
 apply bind_inv in H as(u'&s4&E2&H);unfold upd in E2;injection E2 as <- <-.
 injection H as <- <-. exists s3. split;[exact H3|]. split;[reflexivity|]. destruct Hi;constructor;assumption.
Qed.
Lemma okqi_with_vars:forall A init(m:M A),okqi C m->okqi C(with_vars init m).
Proof.
 intros A init m Hm. split;[apply ok_with_vars;exact(proj1 Hm)|].
 intros s a s' Hi Ii Fi H L Q.
 destruct(okqi_with_vars_inv A init m s a s' Hi H)as(s3&H3&->&Hi1).
 apply ca_Q_vars. apply ca_L_vars in L.
 exact(proj2 Hm _ _ _ Hi1(info_vars _ _ _ Ii)Fi H3 L(ca_Q_vars C _ _ _ Q)).
Qed.
End CacheInput.
Create HintDb okqidb.
#[export]Hint Resolve okqi_ret:okqidb.
Ltac okqi_keeps_leaf:=apply okqi_keeps;[solve[okt]|cache_proj;solve[keepst]].
Ltac okqi_keeps_lemma:=apply okqi_keeps;[solve[auto with okdb]|cache_proj;solve[auto with keepsdb]].
Ltac qifind1:=lazymatch goal with|-okqi _ (?F ?N)=>
 let n:=fresh "n" in let IHn:=fresh "IHn" in generalize N as n;intro n;induction n as[|n IHn];cbn beta iota end.
Ltac qifind3:=lazymatch goal with|-okqi _ (?F ?N ?a ?b)=>
 let n:=fresh "n" in let IHn:=fresh "IHn" in generalize N,a,b;intro n;induction n as[|n IHn];intros;cbn beta iota end.
Ltac input_info:=first[
 lazymatch goal with
 | |-okI ?m=>let h:=head_of m in
 lazymatch h with
 | compile_pattern => apply info_compile_pattern
 | synth => apply okI_synth
 | synth_arg => apply okI_synth_arg
 | synth_block => apply okI_synth_block
 | check_stmts => apply okI_check_stmts
 | check_stmt => apply okI_check_stmt
 | check_bool => apply okI_check_bool
 | synth_if => apply okI_synth_if
 | synth_match => apply okI_synth_match
 | const_val => apply okI_const_val
 | synth_path => apply okI_synth_path
 | check_args => apply okI_check_args
 | synth_call => apply okI_synth_call
 | synth_record => apply okI_synth_record
 | synth_field => apply okI_synth_field
 | synth_method => apply okI_synth_method
 | provided_call => apply okI_provided_call
 | apply_family => apply okI_apply_family
 | demand_needs => apply okI_demand_needs
 | synth_operator => apply okI_synth_operator
 | lang_call => apply okI_lang_call
 | int_literal => apply okI_int_literal
 | float_of => apply okI_float_of
 | synth_literal => apply okI_synth_literal
 | check_bind => apply okI_check_bind
 | expect_fn_provider => apply okI_expect_fn_provider
 | match_fn_sig => apply okI_match_fn_sig
 | wrapper => apply okI_wrapper
 | forwarder => apply okI_forwarder
 | lower_body => apply okI_lower_body
 | lambda_provider => apply okI_lambda_provider
 | apply_functor => apply okI_apply_functor
 | apply_body => apply okI_apply_body
 | apply_abstract => apply okI_apply_abstract
 | bind_functor => apply okI_bind_functor
 | link_body => apply okI_link_body
 | fn_info => apply okI_fn_info
 end end
 |lazymatch goal with
  | |-okI(match ?x with _=>_ end)=>destruct x;input_info
  | |-okI(mbind(new_sym _ _ _ _ _)_)=>okIt
  | |-okI(mbind _ _)=>apply okI_bind;[input_info|intros;input_info]
  | |-okI(mfold _ _ _)=>apply okI_mfold;intros;input_info
  | |-okI(mmap _ _)=>apply okI_mmap;intros;input_info
  | |-okI(miter _ _)=>apply okI_miter;intros;input_info
  | |-okI(with_vars _ _)=>apply okI_with_vars;input_info
  end
 |solve[apply okI_ni;[auto with okdb|auto with nidb|keepst]]
 |solve[apply okI_ni;[okt|nit|keepst]]|solve[okIt]].
Create HintDb inputfwdb.
Ltac input_fwd_named:=lazymatch goal with|-okF ?m=>let h:=head_of m in
 lazymatch h with
 | synth => apply okF_synth
 | synth_arg => apply okF_synth_arg
 | synth_block => apply okF_synth_block
 | check_stmts => apply okF_check_stmts
 | check_stmt => apply okF_check_stmt
 | check_bool => apply okF_check_bool
 | synth_if => apply okF_synth_if
 | synth_match => apply okF_synth_match
 | const_val => apply okF_const_val
 | synth_path => apply okF_synth_path
 | check_args => apply okF_check_args
 | synth_call => apply okF_synth_call
 | synth_record => apply okF_synth_record
 | synth_field => apply okF_synth_field
 | synth_method => apply okF_synth_method
 | provided_call => apply okF_provided_call
 | apply_family => apply okF_apply_family
 | demand_needs => apply okF_demand_needs
 | synth_operator => apply okF_synth_operator
 | lang_call => apply okF_lang_call
 | int_literal => apply okF_int_literal
 | float_of => apply okF_float_of
 | synth_literal => apply okF_synth_literal
 | check_bind => apply okF_check_bind
 | expect_fn_provider => apply okF_expect_fn_provider
 | match_fn_sig => apply okF_match_fn_sig
 | wrapper => apply okF_wrapper
 | forwarder => apply okF_forwarder
 | lower_body => apply okF_lower_body
 | lambda_provider => apply okF_lambda_provider
 | apply_functor => apply okF_apply_functor
 | apply_body => apply okF_apply_body
 | apply_abstract => apply okF_apply_abstract
 | bind_functor => apply okF_bind_functor
 | link_body => apply okF_link_body
 | lower_all => exact okF_lower_all
 end end.

Ltac input_fwd:=repeat(fintros;first[solve[input_fwd_named]|solve[auto with inputfwdb]|
 lazymatch goal with
 | |-okF(mbind _ _)=>apply okF_bind
 | |-okF(mmap _ _)=>apply okF_mmap
 | |-okF(miter _ _)=>apply okF_miter
 | |-okF(mfold _ _ _)=>apply okF_mfold
 | |-okF(with_vars _ _)=>apply okF_with_vars
 | |-okF(in_reify_code _ _)=>unfold in_reify_code
 | |-okF ?m=>first[solve[fw_kept]|lazymatch m with
   |upd _=>fw_leaf
   |(fun s=>_)=>fw_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|let h:=head_of m in progress unfold h]
   end]
 end]).
Ltac okqit_with ext:=repeat(fintros;first[ext|lazymatch goal with
 | |-okqi _ ?t=>first[solve[auto with okqidb]|lazymatch t with
  |mbind _ _=>first[apply okqi_bind;[solve[input_info]|solve[input_fwd]| |]|solve[okqi_keeps_leaf]]
  |mmap _ _=>apply okqi_mmap;[intros;solve[input_info]|intros;solve[input_fwd]|]
  |mfold _ _ _=>apply okqi_mfold;[intros;solve[input_info]|intros;solve[input_fwd]|]
  |miter _ _=>apply okqi_miter;[intros;solve[input_info]|intros;solve[input_fwd]|]
  |with_vars _ _=>apply okqi_with_vars
  |upd _=>okqi_keeps_leaf
  |(fun s=>_)=>okqi_keeps_leaf
  |_=>first[solve[okqi_keeps_lemma]|(lazymatch t with ?F _=>is_fix F end;qifind1)
    |(lazymatch t with ?F _ _ _=>is_fix F end;qifind3)
    |match t with context[match ?x with _=>_ end]=>destruct x end
    |let h:=head_of t in progress unfold h]end]end]).
Ltac okqit:=okqit_with fail.
