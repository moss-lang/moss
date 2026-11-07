(* A fresh abstract-function adapter delegates its row receipt to its real
   physical lower_body call. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly FnBound FnBoundEmit FnBoundSynth ForwarderTrace ReifyHeaderSynth
 ReifyHeaderPresent LambdaOrigin FunctionFrameHistory FunctionFrameOps.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ff_forwarder_step:forall fuel,
 (forall g,FF_bound g(lower_body fuel g))->forall target,FF(forwarder(S fuel)target).
Proof.
 intros fuel Lower target;split;[apply hp_of_hs,hs_forwarder|];split;[apply bf_forwarder|].
 intros initial value final I Bound Run.
 destruct(nget(st_fwd initial)(gs_id target))as[old|]eqn:Found.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Found Run)as Read.
   unfold sym in Read;destruct(nget(st_syms initial)old);[injection Read as <- <-|discriminate].
   intros f body Present Missing;congruence.
 - destruct(forwarder_miss_trace _ _ _ _ _ Found Run)as[fuel0[id[allocated[cached[ready
    [Fuel[Id[New[Cache[Read Body]]]]]]]]]].
   injection Fuel as Equal;subst fuel0.
   pose proof(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id0[Next Symbol]].
   unfold upd in Cache;injection Cache as <-.
   unfold sym in Read;cbn[s_fwd st_syms]in Read;rewrite Symbol in Read;injection Read as <- <-.
   set(g:=GSym id(gs_mod target)(gs_name target)KFn(forwarder_decl target)None[](OForwarder(gs_id target)))in *.
   destruct(ok_new_sym(gs_mod target)(gs_name target)KFn(forwarder_decl target)(OForwarder(gs_id target))
    ltac:(discriminate)ltac:(split;discriminate)_ _ _ I New)as[Ia Frame].
   assert(Ir:Inv(s_fwd(fun m=>nset m(gs_id target)id)allocated))by(destruct Ia;constructor;assumption).
   assert(Rows:st_fns(s_fwd(fun m=>nset m(gs_id target)id)allocated)=st_fns initial).
   {change(st_fns allocated=st_fns initial).
    assert(Keep:keeps st_fns(new_sym(gs_mod target)(gs_name target)KFn(forwarder_decl target)(OForwarder(gs_id target))))by(unfold new_sym;keepst).
    exact(Keep _ _ _ New). }
   assert(Br:FnBound(s_fwd(fun m=>nset m(gs_id target)id)allocated)).
   {intros f body Member;rewrite Rows in Member;pose proof(Bound _ _ Member);pose proof(fr_next _ _ Frame).
    change(f<st_next allocated);lia. }
   assert(Below:gs_id g<st_next(s_fwd(fun m=>nset m(gs_id target)id)allocated)).
   {change(id<st_next allocated);rewrite Next;lia. }
   assert(Physical:nget(st_syms(s_fwd(fun m=>nset m(gs_id target)id)allocated))(gs_id g)=Some g)by(exact Symbol).
   intros f body Present Missing.
   eapply(proj2(proj2(Lower g))_ _ _ Ir Br Below Physical Body _ _ Present).
   rewrite Rows;exact Missing.
Qed.
