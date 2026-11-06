(* One fresh lambda owner receives one body, after nested source compilation. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound FnBoundEmit FnBoundSynth
 LambdaReadonly LambdaReadonlyEmit LambdaReadonlySynth LambdaRows LambdaRowsPreserve.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma lc_with_vars:forall A init(m:M A),LC m->LC(with_vars init m).
Proof.
 intros A init m[B M];split;[apply bf_with_vars;exact B|].
 intros s a final I Bound Rows Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[Get Run]]];injection Get as <- <-.
 apply bind_inv in Run as[u[s2[Update Run]]];unfold upd in Update;injection Update as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[Restore Run]]];unfold upd in Restore;injection Restore as <- <-.
 injection Run as <- <-.
 assert(Im:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(M _ _ _ Im Bound Rows Body).
Qed.
Lemma lcp_with_vars:forall A owner init(m:M A),LCP owner m->LCP owner(with_vars init m).
Proof.
 intros A owner init m[B M];split;[apply bfa_with_vars;exact B|].
 intros s a final I Bound Rows Symbol Empty Below Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[Get Run]]];injection Get as <- <-.
 apply bind_inv in Run as[u[s2[Update Run]]];unfold upd in Update;injection Update as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[Restore Run]]];unfold upd in Restore;injection Restore as <- <-.
 injection Run as <- <-.
 assert(Im:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(M _ _ _ Im Bound Rows Symbol Empty Below Body).
Qed.
Create HintDb lcdb.
#[local]Hint Resolve lc_ret:lcdb.
Ltac lc_kept:=apply lc_keeps;[first[solve[auto with bfdb]|solve[bft]]|solve[auto with keepsdb]].
Ltac lc_leaf:=apply lc_keeps;[solve[bft]|solve[keepst]].
Ltac lct:=repeat(fintros;first[solve[auto with lcdb]|lazymatch goal with
 | |-LC(mbind(new_sym _ _ _ _ OLambda)_)=>apply lc_new_lambda_bind;[split;discriminate|intro]
 | |-LC(mbind _ _)=>apply lc_bind
 | |-LC(mmap _ _)=>apply lc_mmap
 | |-LC(miter _ _)=>apply lc_miter
 | |-LC(mfold _ _ _)=>apply lc_mfold
 | |-LC(with_vars _ _)=>apply lc_with_vars
 | |-LC(in_reify_code _ _)=>unfold in_reify_code
 | |-LCP ?owner(emit_fn ?owner _)=>apply lcp_emit
 | |-LCP ?owner(mbind(emit_fn ?owner _)_)=>apply lcp_bind_after;[apply lcp_emit|intro]
 | |-LCP _(mbind _ _)=>apply lcp_bind_before;[|solve[lrt]|intro]
 | |-LCP _(with_vars _ _)=>apply lcp_with_vars
 | |-LCP _(in_reify_code _ _)=>unfold in_reify_code
 | |-LCP _ ?m=>first[solve[apply lcp_of_lc;lct]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-LC ?m=>first[solve[lc_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>lc_leaf
   |(fun s=>_)=>lc_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).
Lemma lc_wrapper:forall fuel pv,LC(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];lct. Qed.
Lemma lc_lambda_provider_step:forall fuel,
 (forall c e b exp,LC(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,LC(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];lct. Qed.
