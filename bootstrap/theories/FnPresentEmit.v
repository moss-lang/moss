From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnPresentStable.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma pfe_with_vars:forall A owner init(m:M A),
 PF_except owner m ->PF_except owner(with_vars init m).
Proof.
 intros A owner init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros s a final InvS Run key body N Bound Lookup;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable _ _ _ InvMid Body key body N Bound Lookup).
Qed.
Lemma pf_with_vars:forall A init(m:M A),PF m ->PF(with_vars init m).
Proof.
 intros A init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros s a final InvS Run key body Bound Lookup;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable _ _ _ InvMid Body key body Bound Lookup).
Qed.

Create HintDb pfdb.
Lemma pf_fresh_marker:forall e,PF(fresh_marker e).
Proof. intro e;apply pf_keeps;[apply ok_fresh_marker|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity]. Qed.
#[export]Hint Resolve pf_fresh_marker:pfdb.
#[local]Hint Resolve pf_ret pfe_emit:pfdb.
Lemma pf_keeps_first : forall A (m : M A), keeps st_fns m -> ok m -> PF m.
Proof. intros A m Keeps0 Valid; apply pf_keeps; assumption. Qed.

Ltac pf_kept:=apply pf_keeps_first;[solve[auto with keepsdb]|solve[okt]].
Ltac pf_leaf:=apply pf_keeps_first;[solve[keepst]|solve[okt]].
Ltac pft:=repeat(fintros;first[solve[auto with pfdb]|lazymatch goal with
 | |-PF_except ?key(emit_fn ?key _)=>apply pfe_emit
 | |-PF(mbind(new_sym _ _ _ _ _)_)=>apply pf_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-PF(mbind _ _)=>apply pf_bind
 | |-PF(mmap _ _)=>apply pf_mmap
 | |-PF(miter _ _)=>apply pf_miter
 | |-PF(mfold _ _ _)=>apply pf_mfold
 | |-PF(with_vars _ _)=>apply pf_with_vars
 | |-PF(in_reify_code _ _)=>unfold in_reify_code
 | |-PF_except _(mbind _ _)=>apply pfe_bind
 | |-PF_except _(mmap _ _)=>apply pfe_mmap
 | |-PF_except _(miter _ _)=>apply pfe_miter
 | |-PF_except _(mfold _ _ _)=>apply pfe_mfold
 | |-PF_except _(with_vars _ _)=>apply pfe_with_vars
 | |-PF_except _(in_reify_code _ _)=>unfold in_reify_code
 | |-PF_except _ ?m=>first[solve[apply pfe_of_pf;pft]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-PF ?m=>first[solve[pf_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>pf_leaf
   |(fun s=>_)=>pf_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).

Lemma pf_wrapper:forall fuel pv,PF(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];pft. Qed.

Lemma pfe_lower_body_step:forall fuel,
 (forall c e b exp,PF(synth_block fuel c e b exp))->
 forall g,PF_except(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];pft. Qed.
Lemma pf_lower_body_step:forall fuel,
 (forall c e b exp,PF(synth_block fuel c e b exp))->forall g,PF(lower_body(S fuel)g).
Proof.
 intros fuel Body g;split;[apply ok_lower_body|].
 intros s value final I Run key body Bound Lookup.
 destruct(Nat.eq_dec key(gs_id g))as[Same|Other].
 - subst key;cbn[lower_body]in Run;apply bind_inv in Run as[done[mid[Read Run]]].
   cbn[lowered mbind get ret]in Read;rewrite Lookup in Read;cbn[is_some]in Read;injection Read as <- <-.
   injection Run as <- <-;exact Lookup.
 - exact(proj2(pfe_lower_body_step fuel Body g)_ _ _ I Run key body Other Bound Lookup).
Qed.
Lemma pf_forwarder_step:forall fuel,(forall g,PF(lower_body fuel g))->forall tg,PF(forwarder(S fuel)tg).
Proof. intros fuel Lower tg;cbn[forwarder];pft. Qed.
Lemma pf_lambda_provider_step:forall fuel,
 (forall c e b exp,PF(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,PF(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];pft. Qed.
