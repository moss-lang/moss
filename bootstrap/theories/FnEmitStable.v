(* Function-table framing of generated wrappers and recursion markers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnHistory FnStable.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].

Lemma nfu_with_vars:forall A key init(m:M A),
 NF_unloaded key m ->NF_unloaded key(with_vars init m).
Proof.
 intros A key init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros b s a final InvS Load Fresh Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable b _ _ _ InvMid Load Fresh Body).
Qed.
Lemma nf_with_vars:forall A init(m:M A),NF m ->NF(with_vars init m).
Proof.
 intros A init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros b s a final InvS Load Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable b _ _ _ InvMid Load Body).
Qed.

Create HintDb nfdb.
#[local]Hint Resolve nf_ret nf_emit_unloaded:nfdb.
Ltac nf_kept:=apply nf_keeps;[solve[okt]|solve[auto with keepsdb]].
Ltac nf_leaf:=apply nf_keeps;[solve[okt]|solve[keepst]].
Ltac nft:=repeat(fintros;first[solve[auto with nfdb]|lazymatch goal with
 | |-NF_unloaded ?key(emit_fn ?key _)=>apply nf_emit_unloaded
 | H:gs_id ?g = ?key |-NF_unloaded ?key(lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-NF(mbind(new_sym _ _ _ _ _)_)=>apply nf_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-NF(mbind _ _)=>apply nf_bind
 | |-NF(mmap _ _)=>apply nf_mmap
 | |-NF(miter _ _)=>apply nf_miter
 | |-NF(mfold _ _ _)=>apply nf_mfold
 | |-NF(with_vars _ _)=>apply nf_with_vars
 | |-NF(in_reify_code _ _)=>unfold in_reify_code
 | |-NF_unloaded _(mbind(sym _)_)=>apply nfu_bind_sym;intros
 | |-NF_unloaded _(mbind _ _)=>apply nfu_bind
 | |-NF_unloaded _(mmap _ _)=>apply nfu_mmap
 | |-NF_unloaded _(miter _ _)=>apply nfu_miter
 | |-NF_unloaded _(mfold _ _ _)=>apply nfu_mfold
 | |-NF_unloaded _(with_vars _ _)=>apply nfu_with_vars
 | |-NF_unloaded _(in_reify_code _ _)=>unfold in_reify_code
 | |-NF_unloaded _ ?m=>first[solve[apply nf_unloaded_of_nf;nft]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-NF ?m=>first[solve[nf_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>nf_leaf
   |(fun s=>_)=>nf_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).

Lemma nf_wrapper:forall fuel pv,NF(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];nft. Qed.

Lemma nf_forwarder_step:forall fuel,
 (forall g,NF_unloaded(gs_id g)(lower_body fuel g))->forall tg,NF(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;cbn[forwarder];nft.
 all:match goal with H:gs_id ?g = ?id |- _ =>rewrite <-H;apply Lower end.
Qed.

Lemma nf_lower_body_step:forall fuel,
 (forall c e b exp,NF(synth_block fuel c e b exp))->
 forall g,NF_unloaded(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];nft. Qed.

Lemma nf_lambda_provider_step:forall fuel,
 (forall c e b exp,NF(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,
 NF(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];nft. Qed.
