(* Forwarders have their own origin; lowering one cannot overwrite a lambda. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly FnBound FnBoundSynth
 LambdaRows LambdaRowsPreserve LambdaRowsEmit LambdaRowsLower ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma lc_forwarder_step:forall fuel,(forall g,LCLower fuel g)->forall tg,LC(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;split;[apply bf_forwarder|].
 intros s value final I Bound Rows Run.
 destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Cached.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Cached Run)as Symbol.
   unfold sym in Symbol;destruct(nget(st_syms s)id);[injection Symbol as <- <-;exact Rows|discriminate].
 - destruct(forwarder_miss_trace _ _ _ _ _ Cached Run)as[f[id[allocated[cached[ready[Fuel[Id[New[Cache[Symbol Body]]]]]]]]]].
   injection Fuel as <-.
   assert(Onew:ok(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by okt.
   assert(Knew:keeps st_fns(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
   destruct(@lambda_rows_readonly_run _ _ _ _ _ Onew Knew I Bound Rows New)as[IA[BA[RA FA]]].
   assert(Ocache:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
   assert(Kcache:keeps st_fns(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by keepst.
   destruct(@lambda_rows_readonly_run _ _ _ _ _ Ocache Kcache IA BA RA Cache)as[IC[BC[RC FC]]].
   unfold sym in Symbol;destruct(nget(st_syms cached)id)as[g|]eqn:Lookup;[injection Symbol as <- <-|discriminate].
   assert(Gid:gs_id g=id)by exact(iv_ids cached IC id g Lookup).
   assert(Origin:gs_origin g=OForwarder(gs_id tg)).
   {destruct(forwarder_miss_symbol _ _ _ _ _ Cached Run)as[_[Origin _]];exact Origin. }
   apply(proj2(Lower g)cached tt final IC BC RC).
   + rewrite Gid;exact Lookup.
   + rewrite Origin;discriminate.
   + exact Body.
Qed.
Lemma lc_lower_body_zero:forall g,LCLower 0 g.
Proof. intro g;split;[apply bf_lower_body_at|]. intros;discriminate H4. Qed.
