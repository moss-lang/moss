(* A real cache hit or miss returns the recorded forwarder symbol. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame FwdInvariant ForwarderTrace.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Theorem forwarder_return_symbol:forall fuel tg s g final,
 Inv s->FwdInv s->forwarder fuel tg s=Good(g,final)->
 gs_origin g=OForwarder(gs_id tg)/\nget(st_syms final)(gs_id g)=Some g.
Proof.
 intros fuel tg s g final I FI Run;destruct(nget(st_fwd s)(gs_id tg))as[id|]eqn:Cached.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Cached Run)as Sym.
   destruct(FI _ _ Cached)as[old[Lookup Origin]]. unfold sym in Sym;rewrite Lookup in Sym;injection Sym as <- <-.
   split;[exact Origin|rewrite(iv_ids s I id old Lookup);exact Lookup].
 - pose proof(forwarder_miss_symbol _ _ _ _ _ Cached Run)as[Id[Origin Rest]]. split;[exact Origin|].
   destruct(forwarder_miss_trace _ _ _ _ _ Cached Run)as[f[id[allocated[cached[ready[Fuel[Fresh[New[Install[Sym Body]]]]]]]]]].
   assert(Onew:ok(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by okt.
   destruct(Onew _ _ _ I New)as[IA FA].
   assert(Ocache:ok(upd(s_fwd(fun m=>nset m(gs_id tg)id))))by okt.
   destruct(Ocache _ _ _ IA Install)as[IC FC].
   unfold sym in Sym;destruct(nget(st_syms cached)id)as[old|]eqn:Lookup;[injection Sym as <- <-|discriminate].
   destruct(ok_lower_body f old _ _ _ IC Body)as[IF FF].
   rewrite(iv_ids cached IC id old Lookup);exact(fr_keep _ _ FF _ _ Lookup).
Qed.
