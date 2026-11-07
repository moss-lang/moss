(* Ordinary compiler operations compose genuine new-function receipts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly SymReadonly FnBound FnBoundEmit FnPresentStable
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 ReifyHeaderPresent FunctionFrameHistory.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ff_new_sym:forall mi name kind decl origin,
 origin<>OSource->kind<>KTag/\kind<>KUnit->FF(new_sym mi name kind decl origin).
Proof.
 intros mi name kind decl origin Source Kind;apply ff_keeps.
 - apply ok_new_sym;assumption.
 - unfold new_sym;keepst.
 - unfold new_sym;keepst.
Qed.
Lemma ff_record_event:forall event,FF(record_reify_event event).
Proof.
 intro;split;[apply hp_of_hs,hs_record_event|];split.
 - apply bf_keeps;unfold record_reify_event;[okt|keepst].
 - intros s value final I Bound Run f body Present Absent.
   unfold record_reify_event,upd in Run;injection Run as <- <-;cbn[s_reify st_fns]in Present;congruence.
Qed.
Lemma ff_record_exports:forall owner types items inputs promised actual roles,
 FF(record_reify_exports_at owner types items inputs promised actual roles).
Proof.
 intros;split;[apply hp_of_hs,hs_record_exports|];split.
 - apply bf_keeps;unfold record_reify_exports_at;[okt|keepst].
 - intros s value final I Bound Run f body Present Absent.
   unfold record_reify_exports_at,upd in Run;injection Run as <- <-;cbn[s_reify st_fns]in Present;congruence.
Qed.
Lemma ff_with_vars:forall A initial(m:M A),HS m->FF m->FF(with_vars initial m).
Proof.
 intros A initial m H[P[B New]];split;[apply hp_of_hs,hs_with_vars;exact H|].
 split;[apply bf_with_vars;exact B|].
 intros s value final I Bound Run;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 apply bind_inv in Run as[u[s2[Assign Run]]];unfold upd in Assign;injection Assign as <- <-.
 apply bind_inv in Run as[result[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[Restore Run]]];unfold upd in Restore;injection Restore as <- <-;injection Run as <- <-.
 assert(I0:Inv(s_vars initial(List.length initial)s))by(destruct I;constructor;assumption).
 exact(New _ _ _ I0 Bound Body).
Qed.
Create HintDb ffdb.
#[export]Hint Resolve ff_ret ff_record_event ff_record_exports:ffdb.
Lemma ff_keeps_first : forall A (m : M A), keeps st_fns m -> keeps function_headers m -> ok m -> FF m.
Proof. intros A m Keeps0 Keeps1 Valid; apply ff_keeps; assumption. Qed.

Ltac ff_kept:=apply ff_keeps_first;[solve[auto with keepsdb]|solve[auto with keepsdb]|solve[okt]].
Ltac ff_leaf:=apply ff_keeps_first;[solve[keepst]|solve[keepst]|solve[okt]].
Ltac fft:=repeat(fintros;first[solve[auto with ffdb]|lazymatch goal with
 | |-FF(new_sym _ _ _ _ _)=>apply ff_new_sym;[discriminate|split;discriminate]
 | |-FF(mbind _ _)=>apply ff_bind
 | |-FF(mmap _ _)=>apply ff_mmap
 | |-FF(miter _ _)=>apply ff_miter
 | |-FF(mfold _ _ _)=>apply ff_mfold
 | |-FF(with_vars _ _)=>apply ff_with_vars;[solve[auto with hsdb]|]
 | |-FF(in_reify_code _ _)=>unfold in_reify_code
 | |-FF ?m=>first[solve[ff_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>ff_leaf
   |(fun s=>_)=>ff_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).
