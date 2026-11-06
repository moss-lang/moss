(* Actual lambda/eta allocation returns after its own body has been emitted. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly SymReadonly
 FnBound FnBoundEmit FnBoundSynth FnPresentStable FnPresentEmit FnPresentSynth
 LambdaCompletion LambdaCompletionOps ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition OwnerDone(owner:nat){A}(m:M A):Prop:=forall s value final,
 m s=Good(value,final)->exists body,nassoc owner(st_fns final)=Some body.
Arguments OwnerDone:simpl never.
Lemma owner_done_bind_before:forall A B owner(m:M A)(k:A->M B),
 (forall a,OwnerDone owner(k a))->OwnerDone owner(mbind m k).
Proof. intros A B owner m k K s value final E;apply bind_inv in E as[a[mid[E1 E2]]];exact(K a _ _ _ E2). Qed.
Lemma owner_done_emit:forall owner body,OwnerDone owner(emit_fn owner body).
Proof. intros owner body s value final E;unfold emit_fn,upd in E;injection E as <- <-.
 exists body;cbn[s_fns st_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity. Qed.
Lemma owner_done_bind_after:forall A B owner(m:M A)(k:A->M B),
 OwnerDone owner m->(forall a,keeps st_fns(k a))->OwnerDone owner(mbind m k).
Proof. intros A B owner m k Done K s value final E;apply bind_inv in E as[a[mid[E1 E2]]].
 rewrite(K a _ _ _ E2);exact(Done _ _ _ E1). Qed.
Lemma owner_done_err:forall A owner mi msg,OwnerDone owner(@err A mi msg).
Proof. intros A owner mi msg s value final E;exfalso;exact(err_not_good _ _ _ _ _ _ E). Qed.
Ltac owner_done_t:=repeat(fintros;first[
 solve[apply owner_done_emit]|solve[apply owner_done_err]|
 lazymatch goal with
 | |-OwnerDone ?owner(mbind(emit_fn ?owner _)_)=>apply owner_done_bind_after;[apply owner_done_emit|intro;keepst]
 | |-OwnerDone _(mbind _ _)=>apply owner_done_bind_before;intro
 | |-OwnerDone _ ?m=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
    let h:=head_of m in progress unfold h]
 end]).
Lemma ch_new_lambda_bind:forall A mi name kind decl(k:nat->M A),
 kind<>KTag/\kind<>KUnit->(forall id,CH_at id(k id))->(forall id,OwnerDone id(k id))->
 CH(mbind(new_sym mi name kind decl OLambda)k).
Proof.
 intros A mi name kind decl k Kind K Done.
 split;[apply pf_new_sym_bind;[discriminate|exact Kind|intro;exact(proj1(K id))]|].
 split;[apply bf_new_sym_bind;[discriminate|exact Kind|intro;exact(proj1(proj2(K id)))]|].
 intros b w s value final Load I Bound W E;apply bind_inv in E as[id[mid[New Rest]]].
 destruct(ok_new_sym mi name kind decl OLambda ltac:(discriminate)Kind _ _ _ I New)as[Im Frame].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Allocated]].
 assert(Bmid:FnBound mid).
 {eapply fn_bound_keeps;[exact Bound|exact Frame|]. assert(Knew:keeps st_fns(new_sym mi name kind decl OLambda))by(unfold new_sym;keepst);exact(Knew _ _ _ New). }
 assert(Wmid:st_w mid=w)by(rewrite(fr_w _ _ Frame);exact W).
 pose proof(proj2(proj2(K id))b w _ _ _ Load Im Bmid Wmid ltac:(rewrite Next;lia)Rest)as Hist.
 intros key g Lookup Origin Missing;destruct(Nat.eq_dec key id)as[->|Ne].
 - exact(Done id _ _ _ Rest).
 - eapply Hist;[exact Lookup|exact Origin|].
   rewrite(@completion_new_sym_other_symbols _ _ _ _ _ _ _ _ _ New Ne);exact Missing.
Qed.
Create HintDb chdb.
#[local]Hint Resolve ch_ret:chdb.
Ltac ch_kept:=apply ch_keeps;[solve[okt]|solve[auto with keepsdb]|solve[auto with keepsdb]].
Ltac ch_leaf:=apply ch_keeps;[solve[okt]|solve[keepst]|solve[keepst]].
Ltac cht:=repeat(fintros;first[solve[auto with chdb]|lazymatch goal with
 | |-CH_at ?owner(emit_fn ?owner _)=>apply cha_emit
 | H : gs_id ?g = ?key |- CH_at ?key (lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-CH(mbind(new_sym _ _ _ _ OLambda)_)=>apply ch_new_lambda_bind;[split;discriminate|intro|intro;owner_done_t]
 | |-CH(mbind(new_sym _ _ _ _ _)_)=>apply ch_new_sym_bind;[discriminate|split;discriminate|discriminate|intro]
 | |-CH(mbind _ _)=>apply ch_bind
 | |-CH(mmap _ _)=>apply ch_mmap
 | |-CH(miter _ _)=>apply ch_miter
 | |-CH(mfold _ _ _)=>apply ch_mfold
 | |-CH(with_vars _ _)=>apply ch_with_vars
 | |-CH(in_reify_code _ _)=>unfold in_reify_code
 | |-CH_at _(mbind(sym _)_)=>apply cha_bind_sym;intros
 | |-CH_at _(mbind _ _)=>apply cha_bind
 | |-CH_at _(mmap _ _)=>apply cha_mmap
 | |-CH_at _(miter _ _)=>apply cha_miter
 | |-CH_at _(mfold _ _ _)=>apply cha_mfold
 | |-CH_at _(with_vars _ _)=>apply cha_with_vars
 | |-CH_at _(in_reify_code _ _)=>unfold in_reify_code
 | |-CH_at _ ?m=>first[solve[apply ch_at_of_ch;cht]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-CH ?m=>first[solve[ch_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>ch_leaf
   |(fun s=>_)=>ch_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).
Lemma ch_wrapper:forall fuel pv,CH(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];cht. Qed.
Lemma cha_lower_body_step:forall fuel,
 (forall c e b exp,CH(synth_block fuel c e b exp))->forall g,CH_at(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];cht. Qed.
Lemma ch_forwarder_step:forall fuel,(forall g,CH_at(gs_id g)(lower_body fuel g))->forall tg,CH(forwarder(S fuel)tg).
Proof. intros fuel Lower tg;cbn[forwarder];cht. Qed.
Lemma ch_lambda_provider_step:forall fuel,
 (forall c e b exp,CH(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,CH(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];cht. Qed.
