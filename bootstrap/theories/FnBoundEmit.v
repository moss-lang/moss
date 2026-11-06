From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma bf_with_vars:forall A init(m:M A),BF m ->BF(with_vars init m).
Proof.
 intros A init m[O K];split;[apply ok_with_vars;exact O|].
 intros s a final I FI E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-.
 injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(K _ _ _ IM FI Body).
Qed.
Lemma bfa_with_vars:forall A key init(m:M A),BF_at key m ->BF_at key(with_vars init m).
Proof.
 intros A key init m[O K];split;[apply ok_with_vars;exact O|].
 intros s a final I FI Bound E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-.
 injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(K _ _ _ IM FI Bound Body).
Qed.
Create HintDb bfdb.
Lemma bf_fresh_marker:forall e,BF(fresh_marker e).
Proof. intro e;apply bf_keeps;[apply ok_fresh_marker|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity]. Qed.
#[export]Hint Resolve bf_fresh_marker:bfdb.
#[local]Hint Resolve bf_ret bfa_emit:bfdb.
Ltac bf_kept:=apply bf_keeps;[solve[okt]|solve[auto with keepsdb]].
Ltac bf_leaf:=apply bf_keeps;[solve[okt]|solve[keepst]].
Ltac bft:=repeat(fintros;first[solve[auto with bfdb]|lazymatch goal with
 | |-BF_at ?key(emit_fn ?key _)=>apply bfa_emit
 | H:gs_id ?g = ?key |-BF_at ?key(lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-BF(mbind(new_sym _ _ _ _ _)_)=>apply bf_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-BF(mbind _ _)=>apply bf_bind
 | |-BF(mmap _ _)=>apply bf_mmap
 | |-BF(miter _ _)=>apply bf_miter
 | |-BF(mfold _ _ _)=>apply bf_mfold
 | |-BF(with_vars _ _)=>apply bf_with_vars
 | |-BF(in_reify_code _ _)=>unfold in_reify_code
 | |-BF_at _(mbind(sym _)_)=>apply bfa_bind_sym;intros
 | |-BF_at _(mbind _ _)=>apply bfa_bind
 | |-BF_at _(mmap _ _)=>apply bfa_mmap
 | |-BF_at _(miter _ _)=>apply bfa_miter
 | |-BF_at _(mfold _ _ _)=>apply bfa_mfold
 | |-BF_at _(with_vars _ _)=>apply bfa_with_vars
 | |-BF_at _(in_reify_code _ _)=>unfold in_reify_code
 | |-BF_at _ ?m=>first[solve[apply bfa_of_bf;bft]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-BF ?m=>first[solve[bf_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>bf_leaf
   |(fun s=>_)=>bf_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).

Lemma bf_wrapper:forall fuel pv,BF(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];bft. Qed.

Lemma bf_forwarder_step:forall fuel,
 (forall g,BF_at(gs_id g)(lower_body fuel g))->forall tg,BF(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;cbn[forwarder];bft.
 all:match goal with H:gs_id ?g = ?id |- _ =>rewrite <-H;apply Lower end.
Qed.

Lemma bf_lower_body_step:forall fuel,
 (forall c e b exp,BF(synth_block fuel c e b exp))->
 forall g,BF_at(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];bft. Qed.

Lemma bf_lambda_provider_step:forall fuel,
 (forall c e b exp,BF(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,
 BF(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];bft. Qed.
