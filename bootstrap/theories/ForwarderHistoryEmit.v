From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly SymReadonly FnPresentSynth FnBoundSynth
 ForwarderHistory ForwarderHistoryOps ForwarderHistoryStep ForwarderExpectHistory.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Create HintDb ghdb.
Lemma gh_fresh_marker:forall e,GH(fresh_marker e).
Proof. intro e;apply gh_keeps;[apply ok_fresh_marker|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity]. Qed.
#[export]Hint Resolve gh_fresh_marker:ghdb.
#[local]Hint Resolve gh_ret gha_emit:ghdb.
Ltac gh_kept:=apply gh_keeps;[solve[okt]|solve[auto with keepsdb]|solve[auto with keepsdb]].
Ltac gh_leaf:=apply gh_keeps;[solve[okt]|solve[keepst]|solve[keepst]].
Ltac ght:=repeat(fintros;first[solve[auto with ghdb]|lazymatch goal with
 | |-GH_at ?key(emit_fn ?key _)=>apply gha_emit
 | H:gs_id ?g = ?key |-GH_at ?key(lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-GH(mbind(new_sym _ _ _ _ _)_)=>apply gh_new_sym_bind;[discriminate|split;discriminate|intros;discriminate|intro]
 | |-GH(mbind _ _)=>apply gh_bind
 | |-GH(mmap _ _)=>apply gh_mmap
 | |-GH(miter _ _)=>apply gh_miter
 | |-GH(mfold _ _ _)=>apply gh_mfold
 | |-GH(with_vars _ _)=>apply gh_with_vars
 | |-GH(in_reify_code _ _)=>unfold in_reify_code
 | |-GH_at _(mbind(sym _)_)=>apply gha_bind_sym;intros
 | |-GH_at _(mbind _ _)=>apply gha_bind
 | |-GH_at _(mmap _ _)=>apply gha_mmap
 | |-GH_at _(miter _ _)=>apply gha_miter
 | |-GH_at _(mfold _ _ _)=>apply gha_mfold
 | |-GH_at _(with_vars _ _)=>apply gha_with_vars
 | |-GH_at _(in_reify_code _ _)=>unfold in_reify_code
 | |-GH_at _ ?m=>first[solve[apply gh_at_of_gh;ght]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-GH ?m=>first[solve[gh_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>gh_leaf
   |(fun s=>_)=>gh_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).

Lemma gh_wrapper:forall fuel pv,GH(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];ght. Qed.

Lemma gh_lower_body_step:forall fuel,
 (forall c e b exp,GH(synth_block fuel c e b exp))->
 forall g,GH_at(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];ght. Qed.

Lemma gh_lambda_provider_step:forall fuel,
 (forall c e b exp,GH(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,
 GH(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];ght. Qed.
Lemma gh_forwarder_zero:forall tg,GHF 0 tg.
Proof. intro tg;split;[apply FnPresentSynth.pf_forwarder|]. split;[apply FnBoundSynth.bf_forwarder|].
 intros b w s g final Load I Bound W Loaded E;discriminate E. Qed.
