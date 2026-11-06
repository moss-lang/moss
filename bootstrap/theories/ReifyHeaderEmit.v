(* Body compilation preserves all earlier protected function headers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly
 FnPresentStable FnPresentEmit FnPresentSynth ReifyHeader ReifyHeaderReadonly ReifyHeaderStable.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Create HintDb hsdb.
Lemma hs_record_event:forall event,HS(record_reify_event event).
Proof.
 intro event;apply hs_keeps.
 - apply pf_keeps;unfold record_reify_event;[okt|keepst].
 - intros s [] final Run f;exact(record_event_header event s final Run f).
Qed.
#[export]Hint Resolve hs_record_event:hsdb.
Lemma hs_record_exports:forall owner types items inputs promised actual roles,
 HS(record_reify_exports_at owner types items inputs promised actual roles).
Proof.
 intros;apply hs_keeps.
 - apply pf_keeps;unfold record_reify_exports_at;[okt|keepst].
 - intros s [] final Run f;exact(record_exports_header _ _ _ _ _ _ _ _ _ Run f).
Qed.
#[export]Hint Resolve hs_record_exports:hsdb.
Lemma hs_fresh_marker:forall e,HS(fresh_marker e).
Proof. intro e;apply hs_keeps;[apply pf_fresh_marker|apply header_keeps_of_rows;unfold fresh_marker;keepst]. Qed.
#[export]Hint Resolve hs_fresh_marker:hsdb.
#[local]Hint Resolve hs_ret hse_emit hse_register:hsdb.
Ltac hs_kept:=apply hs_keeps;[solve[apply pf_keeps;[okt|auto with keepsdb]]|apply header_keeps_of_rows;solve[auto with keepsdb]].
Ltac hs_leaf:=apply hs_keeps;[apply pf_keeps;[solve[okt]|solve[keepst]]|apply header_keeps_of_rows;solve[keepst]].
Ltac hst:=repeat(fintros;first[solve[auto with hsdb]|lazymatch goal with
 | |-HSE ?key(emit_fn ?key _)=>apply hse_emit
 | |-HSE ?key(register_reify_code(ROFunction ?key)_ _ _)=>apply hse_register
 | |-HS(mbind(new_sym _ _ _ _ _)_)=>apply hs_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-HS(mbind _ _)=>apply hs_bind
 | |-HS(mmap _ _)=>apply hs_mmap
 | |-HS(miter _ _)=>apply hs_miter
 | |-HS(mfold _ _ _)=>apply hs_mfold
 | |-HS(with_vars _ _)=>apply hs_with_vars
 | |-HS(in_reify_code _ _)=>unfold in_reify_code
 | |-HSE _(mbind(sym _)_)=>apply hse_bind_sym;intros
 | |-HSE _(mbind _ _)=>apply hse_bind
 | |-HSE _(mmap _ _)=>apply hse_mmap
 | |-HSE _(miter _ _)=>apply hse_miter
 | |-HSE _(mfold _ _ _)=>apply hse_mfold
 | |-HSE _(with_vars _ _)=>apply hse_with_vars
 | |-HSE _(in_reify_code _ _)=>unfold in_reify_code
 | H:gs_id ?g = ?id |-HSE ?id(lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-HSE _ ?m=>first[solve[apply hse_of_hs;hst]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-HS ?m=>first[solve[hs_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>hs_leaf
   |(fun s=>_)=>hs_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).

Lemma hs_wrapper:forall fuel pv,HS(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];hst. Qed.

Lemma hse_lower_body_step:forall fuel,
 (forall c e b exp,HS(synth_block fuel c e b exp))->
 forall g,HSE(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];hst. Qed.
Lemma hs_forwarder_step:forall fuel,(forall g,HSE(gs_id g)(lower_body fuel g))->forall tg,HS(forwarder(S fuel)tg).
Proof. intros fuel Lower tg;cbn[forwarder];hst. Qed.
Lemma hs_lambda_provider_step:forall fuel,
 (forall c e b exp,HS(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,HS(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];hst. Qed.
