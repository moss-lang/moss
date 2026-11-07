(* Completion receipts for every newly emitted function, independent of
   symbol-table classification. Pending own-body markers are handled by
   the complete lower_body invocation. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly ReifyHeaderReadonly
 FnBound FnPresentStable ReifyHeader ReifyHeaderStable ReifyHeaderPresent ReifyFrameReady
 ReifyWalkData ReifyReach ReifyEta ReifyProgramWalk LambdaOrigin.
Import ListNotations.
Set Implicit Arguments.
Definition FunctionFrameOrigin(source:st)(f:nat)(body:xexpr):Prop:=
 (exists origin,LambdaOrigin f body(function_header source f)origin)/\
 function_frame_node_ready source(RnFunction f).
Definition NewFunctionFrames(initial final:st):Prop:=forall f body,
 nassoc f(st_fns final)=Some body->nassoc f(st_fns initial)=None->
 FunctionFrameOrigin final f body.
Lemma source_eta_equal:forall a b f,
 function_header b f=function_header a f->nget(st_syms b)f=nget(st_syms a)f->
 nassoc f(st_fns b)=nassoc f(st_fns a)->source_eta_callee b f=source_eta_callee a f.
Proof.
 intros a b f Header Symbol Body;unfold function_header in Header.
 unfold source_eta_callee;change(assoc Nat.eqb f(st_fns b))with(nassoc f(st_fns b));
 change(assoc Nat.eqb f(st_fns a))with(nassoc f(st_fns a));rewrite Symbol,Body.
 destruct(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify b)))as[bc|];
 destruct(assoc reify_owner_eqb(ROFunction f)(rm_codes(st_reify a)))as[ac|];
 cbn[option_map code_header]in Header;try discriminate;try reflexivity.
 injection Header as Owner Scope Inputs Signature;rewrite Signature;reflexivity.
Qed.
Lemma frame_ready_transport:forall a b f body,
 Fr a b->function_header b f=function_header a f->
 nassoc f(st_fns a)=Some body->nassoc f(st_fns b)=Some body->
 function_frame_node_ready a(RnFunction f)->function_frame_node_ready b(RnFunction f).
Proof.
 intros a b f body Frame Header Before After Ready.
 destruct Ready as[[code[Code[signature Sig]]]|[code[original[Code[Sig Eta]]]]].
 - eapply function_header_signature_ready.
   rewrite Header;unfold function_header;rewrite Code;cbn[option_map];unfold code_header;rewrite Sig;reflexivity.
 - assert(Symbol:nget(st_syms b)f=nget(st_syms a)f).
   {destruct(nget(st_syms a)f)as[symbol|]eqn:Old.
    - exact(fr_keep _ _ Frame _ _ Old).
    - unfold source_eta_callee in Eta;rewrite Code,Old in Eta.
      destruct(assoc Nat.eqb f(st_fns a));discriminate. }
   eapply function_header_eta_ready.
   + rewrite Header;unfold function_header;rewrite Code;cbn[option_map];unfold code_header;rewrite Sig;reflexivity.
   + rewrite(source_eta_equal a b f Header Symbol ltac:(congruence));exact Eta.
Qed.
Lemma function_frame_origin_transport:forall a b f body,
 Fr a b->function_header b f=function_header a f->
 nassoc f(st_fns a)=Some body->nassoc f(st_fns b)=Some body->
 FunctionFrameOrigin a f body->FunctionFrameOrigin b f body.
Proof.
 intros a b f body Frame Header Before After[Origin Ready];split.
 - rewrite Header;exact Origin.
 - eapply frame_ready_transport;eassumption.
Qed.
Lemma function_lookup_below:forall s f body,
 FnBound s->nassoc f(st_fns s)=Some body->f<st_next s.
Proof.
 intros s f body Bound Present;destruct(Nat.lt_ge_cases f(st_next s));[assumption|].
 rewrite(@fn_bound_fresh_absent s f Bound H)in Present;discriminate.
Qed.
Definition FF{A}(m:M A):Prop:=HP m/\BF m/\forall s value final,
 Inv s->FnBound s->m s=Good(value,final)->NewFunctionFrames s final.
Definition FF_bound(g:gsym){A}(m:M A):Prop:=HP m/\BF_at(gs_id g)m/\forall s value final,
 Inv s->FnBound s->gs_id g<st_next s->nget(st_syms s)(gs_id g)=Some g->
 m s=Good(value,final)->NewFunctionFrames s final.
Arguments FF:simpl never.
Arguments FF_bound:simpl never.
Lemma ff_ret:forall A(a:A),FF(ret a).
Proof.
 intros;split;[apply hp_of_hs,hs_ret|];split;[apply bf_ret|].
 intros s value final I Bound Run;injection Run as <- <-.
 intros f body Present Absent;congruence.
Qed.
Lemma ff_bind:forall A B(m:M A)(k:A->M B),FF m->(forall a,FF(k a))->FF(mbind m k).
Proof.
 intros A B m k[Pm[Bm New]]K;split;[apply hp_bind;[exact Pm|intro;exact(proj1(K a))]|].
 split;[apply bf_bind;[exact Bm|intro;exact(proj1(proj2(K a)))]|].
 intros s value final I Bound Run;apply bind_inv in Run as[a[mid[First Last]]].
 destruct(proj1(proj1 Pm)_ _ _ I First)as[Im Frame].
 assert(Bmid:FnBound mid)by(exact(proj2 Bm _ _ _ I Bound First)).
 destruct(proj1(proj1(proj1(K a)))_ _ _ Im Last)as[If Tail].
 intros f body Present Absent.
 destruct(nassoc f(st_fns mid))as[old|]eqn:Middle.
 - assert(Below:f<st_next mid)by(eapply function_lookup_below;eassumption).
   pose proof(proj2(proj1(proj1(K a)))_ _ _ Im Last f old Below Middle)as Stable.
   rewrite Present in Stable;injection Stable as <-.
   eapply function_frame_origin_transport;[exact Tail|
    exact(proj2(proj1(K a))_ _ _ Im Last f body Below Middle)|exact Middle|exact Present|].
   exact(New _ _ _ I Bound First _ _ Middle Absent).
 - exact(proj2(proj2(K a))_ _ _ Im Bmid Last _ _ Present Middle).
Qed.
Lemma ff_mmap:forall A B(f:A->M B)xs,(forall x,FF(f x))->FF(mmap f xs).
Proof.
 intros A B f xs F;induction xs;cbn[mmap];[apply ff_ret|].
 apply ff_bind;[apply F|intro;apply ff_bind;[exact IHxs|intro;apply ff_ret]].
Qed.
Lemma ff_miter:forall A(f:A->M unit)xs,(forall x,FF(f x))->FF(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply ff_ret|apply ff_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma ff_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,FF(f a x))->FF(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply ff_ret|apply ff_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma ff_keeps:forall A(m:M A),ok m->keeps st_fns m->keeps function_headers m->FF m.
Proof.
 intros A m O F H;split.
 - apply hp_of_hs,hs_keeps;[apply pf_keeps;assumption|apply header_keeps_of_rows;exact H].
 - split;[apply bf_keeps;assumption|].
   intros s value final I Bound Run f body Present Absent.
   rewrite(F _ _ _ Run),Absent in Present;discriminate.
Qed.
