From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly LambdaReadonly ForwarderTrace ForwarderHistory.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma lre_with_vars:forall A owner init(m:M A),
 LR_except owner m ->LR_except owner(with_vars init m).
Proof.
 intros A owner init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros s a final InvS Run key g N Lookup Origin;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable _ _ _ InvMid Body key g N Lookup Origin).
Qed.
Lemma lr_with_vars:forall A init(m:M A),LR m ->LR(with_vars init m).
Proof.
 intros A init m[OK Stable];split;[apply ok_with_vars;exact OK|].
 intros s a final InvS Run key g Lookup Origin;unfold with_vars in Run.
 apply bind_inv in Run as[s0[s1[R0 Run]]];injection R0 as <- <-.
 apply bind_inv in Run as[u[s2[R1 Run]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in Run as[r[s3[Body Run]]].
 apply bind_inv in Run as[u'[s4[R2 Run]]];unfold upd in R2;injection R2 as <- <-.
 injection Run as <- <-.
 assert(InvMid:Inv(s_vars init(List.length init)s))by(destruct InvS;constructor;assumption).
 exact(Stable _ _ _ InvMid Body key g Lookup Origin).
Qed.

Create HintDb lrdb.
Lemma lr_fresh_marker:forall e,LR(fresh_marker e).
Proof. intro e;apply lr_keeps;[apply ok_fresh_marker|unfold fresh_marker,keeps;intros;injection H as _ <-;reflexivity]. Qed.
#[export]Hint Resolve lr_fresh_marker:lrdb.
#[local]Hint Resolve lr_ret lre_emit:lrdb.
Lemma lr_keeps_first : forall A (m : M A), keeps st_fns m -> ok m -> LR m.
Proof. intros A m Keeps0 Valid; apply lr_keeps; assumption. Qed.

Ltac lr_kept:=apply lr_keeps_first;[solve[auto with keepsdb]|solve[okt]].
Ltac lr_leaf:=apply lr_keeps_first;[solve[keepst]|solve[okt]].
Ltac lrt:=repeat(fintros;first[solve[auto with lrdb]|lazymatch goal with
 | |-LR_except ?key(emit_fn ?key _)=>apply lre_emit
 | |-LR(mbind(new_sym _ _ _ _ _)_)=>apply lr_new_sym_bind;[discriminate|split;discriminate|intro]
 | |-LR(mbind _ _)=>apply lr_bind
 | |-LR(mmap _ _)=>apply lr_mmap
 | |-LR(miter _ _)=>apply lr_miter
 | |-LR(mfold _ _ _)=>apply lr_mfold
 | |-LR(with_vars _ _)=>apply lr_with_vars
 | |-LR(in_reify_code _ _)=>unfold in_reify_code
 | |-LR_except _(mbind _ _)=>apply lre_bind
 | |-LR_except _(mmap _ _)=>apply lre_mmap
 | |-LR_except _(miter _ _)=>apply lre_miter
 | |-LR_except _(mfold _ _ _)=>apply lre_mfold
 | |-LR_except _(with_vars _ _)=>apply lre_with_vars
 | |-LR_except _(in_reify_code _ _)=>unfold in_reify_code
 | |-LR_except _ ?m=>first[solve[apply lre_of_lr;lrt]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-LR ?m=>first[solve[lr_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>lr_leaf
   |(fun s=>_)=>lr_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).


Lemma lr_wrapper:forall fuel pv,LR(wrapper fuel pv).
Proof. intros[|fuel]pv;cbn[wrapper];lrt. Qed.
Lemma lre_lower_body_step:forall fuel,
 (forall c e b exp,LR(synth_block fuel c e b exp))->
 forall g,LR_except(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];lrt. Qed.
Lemma lr_forwarder_step:forall fuel,
 (forall g,LR_except(gs_id g)(lower_body fuel g))->forall tg,LR(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;split;[apply ok_forwarder|].
 intros s value final I Run key old Lookup Lambda.
 destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Cached.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Cached Run)as Sym.
   unfold sym in Sym;destruct(nget(st_syms s)id);[injection Sym as <- <-;reflexivity|discriminate].
 - destruct(forwarder_miss_trace _ _ _ _ _ Cached Run)as[f[id[allocated[cached[ready[Fuel[Id[New[Cache[Symbol Body]]]]]]]]]].
   injection Fuel as <-.
   destruct(ok_new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))
    ltac:(discriminate)ltac:(split;discriminate)_ _ _ I New)as[IA FA].
   assert(Ocache:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
   destruct(Ocache _ _ _ IA Cache)as[IC FC].
   assert(Osym:ok(sym id))by(unfold sym;okt).
   destruct(Osym _ _ _ IC Symbol)as[IR FR].
   assert(Gid:gs_id value=id).
   {unfold sym in Symbol;destruct(nget(st_syms cached)id)as[g|]eqn:G;[injection Symbol as <- <-|discriminate].
    exact(iv_ids cached IC id g G). }
   assert(Ne:key<>gs_id value)by(rewrite Gid,Id;pose proof(@symbol_below_next s key old I Lookup);lia).
   rewrite(proj2(Lower value)_ _ _ IR Body key old Ne
    (fr_keep _ _ FR _ _ (fr_keep _ _ FC _ _ (fr_keep _ _ FA _ _ Lookup)))Lambda).
   assert(Knew:keeps st_fns(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
   assert(Kcache:keeps st_fns(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by keepst.
   assert(Ksym:keeps st_fns(sym id))by(unfold sym;keepst).
   unfold lambda_owner_rows;rewrite(Ksym _ _ _ Symbol),(Kcache _ _ _ Cache),(Knew _ _ _ New);reflexivity.
Qed.
Lemma lr_lambda_provider_step:forall fuel,
 (forall c e b exp,LR(synth_block fuel c e b exp))->
 forall c e ps rt body bound sg this what,LR(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof. intros fuel Body c e ps rt body bound sg this what;cbn[lambda_provider];lrt. Qed.
