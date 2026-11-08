(* The sole allocator of OForwarder completes that new function before return. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv FnReadonly
 FnBound FnBoundSynth FnPresentStable FnPresentSynth ForwarderTrace ForwarderCompleted
 ForwarderHistory ForwarderHistoryOps.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL reqs type_requirements].
Definition GHF(fuel:nat)(tg:gsym):Prop:=PF(forwarder fuel tg)/\BF(forwarder fuel tg)/\
 forall b w s g final,load_world b=Good w->Inv s->FnBound s->st_w s=w->LoadedAbstract w tg->
 forwarder fuel tg s=Good(g,final)->forwarder_history w s final.
Arguments GHF:simpl never.
Lemma gh_forwarder_step:forall fuel,(forall g,GH_at(gs_id g)(lower_body fuel g))->forall tg,GHF(S fuel)tg.
Proof.
 intros fuel Lower tg;split;[apply pf_forwarder|]. split;[apply bf_forwarder|].
 intros b w s g final Load I Bound World Loaded Run.
 destruct(nget(st_fwd s)(gs_id tg))as[cached|]eqn:Cache.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Cache Run)as Sym.
   unfold sym in Sym;destruct(nget(st_syms s)cached);[injection Sym as <- <-|discriminate].
   apply forwarder_history_refl.
 - destruct Loaded as[Loaded[Kind[n[ps[rt Decl]]]]].
   pose proof(forwarder_miss_completed b w(S fuel)tg n ps rt s g final Load Loaded Decl Kind I Bound World Cache Run)
    as[OwnId[OwnOrigin[OwnBelow Complete]]].
   destruct(forwarder_miss_trace _ _ _ _ _ Cache Run)as[f[id[allocated[cached[ready[Fuel[Id[New[Install[Sym Body]]]]]]]]]].
   injection Fuel as <-.
   assert(Onew:ok(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by okt.
   destruct(Onew _ _ _ I New)as[IA FA].
   destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id0[Next Lookup]].
   assert(BAllocated:FnBound allocated).
   {eapply fn_bound_keeps;[exact Bound|exact FA|]. assert(F:keeps st_fns(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst). exact(F _ _ _ New). }
   unfold upd in Install;injection Install as <-.
   unfold sym in Sym;cbn[s_fwd st_syms]in Sym;rewrite Lookup in Sym;injection Sym as <- <-.
   set(provider:=GSym id(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)None[](OForwarder(gs_id tg)))in *.
   set(start:=s_fwd(fun m=>nset m(gs_id tg)id)allocated)in *.
   assert(IS:Inv start)by(unfold start;destruct IA;constructor;assumption).
   assert(BS:FnBound start)by exact BAllocated.
   assert(WS:st_w start=w)by(unfold start;cbn[s_fwd st_w];rewrite(fr_w _ _ FA);exact World).
   assert(Below:gs_id provider<st_next start)by(unfold provider,start;cbn[gs_id s_fwd st_next];rewrite Next;lia).
   pose proof(proj2(proj2(Lower provider))b w _ _ _ Load IS BS WS Below Body)as Hist.
   destruct(proj1(proj1(Lower provider))_ _ _ IS Body)as[IF FF].
   intros key h target FinalLookup Origin Missing.
   destruct(Nat.eq_dec key id)as[Same|Different].
   + subst key.
     assert(StartLookup:nget(st_syms start)id=Some provider)by exact Lookup.
     pose proof(fr_keep _ _ FF _ _ StartLookup)as FinalOwn;rewrite FinalLookup in FinalOwn;injection FinalOwn as Eq;subst h.
     change(OForwarder(gs_id tg)=OForwarder target)in Origin;injection Origin as <-.
     exists tg;split;[split;[exact Loaded|split;[exact Kind|repeat eexists;exact Decl]]|].
     repeat split;try reflexivity;exact Complete.
   + eapply Hist;[exact FinalLookup|exact Origin|].
     unfold start;cbn[s_fwd st_syms]. rewrite(@new_sym_other_symbols _ _ _ _ _ _ _ _ _ New Different);exact Missing.
Qed.
