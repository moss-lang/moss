(* Reading an authenticated cache entry identifies its actual source creator. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabKeeps
 InfoInvariant InfoPreserve FwdInvariant LinkCacheInvariant LinkBodyTrace ProviderPhysical
 ForwarderTrace SymReadonly.
Import ListNotations.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL nat_to_string nat_to_string_go].
Theorem cached_link_source_identity : forall s fg bg code,
 LinkInv s ->PhysicalProvider s fg ->PhysicalProvider s bg ->gs_id fg<>gs_id bg ->
 link_lookup s(gs_id fg)(gs_id bg)=Some code ->
 exists fuel before after,
 Inv before /\InfoInv before /\FwdInv before /\
 PhysicalProvider before fg /\PhysicalProvider before bg /\
 link_lookup before(gs_id fg)(gs_id bg)=None /\
 link_body(S fuel)fg bg before=Good(code,after) /\LinkFrame after s.
Proof.
 intros s fg bg code Linked Target Provider Different Hit.
 destruct(source_link_cached_creator _ _ _ _ Linked Hit Different)as
 [fuel[creatorTarget[creatorProvider[before[after[TargetId[ProviderId
 [Valid[Cached[Forwarded[TargetBefore[ProviderBefore[Miss[Run Frame]]]]]]]]]]]]]].
 destruct(ok_link_body(S fuel)creatorTarget creatorProvider _ _ _ Valid Run)as[_ Execution].
 pose proof(fl_keep _ _ (link_frame_source _ _ Frame)_ _
  (fr_keep _ _ Execution _ _ TargetBefore))as TargetNow.
 pose proof(fl_keep _ _ (link_frame_source _ _ Frame)_ _
  (fr_keep _ _ Execution _ _ ProviderBefore))as ProviderNow.
 unfold PhysicalProvider in Target,Provider.
 rewrite Target in TargetNow;injection TargetNow as <-.
 rewrite Provider in ProviderNow;injection ProviderNow as <-.
 exists fuel,before,after;repeat first[assumption|split].
Qed.

Lemma link_finish_symbol : forall fg bg ef promised eb rts dr roles binds s h final,
 link_trace_finish fg bg ef promised eb rts dr roles binds s=Good(h,final)->
 nget(st_syms final)h=Some(GSym h(gs_mod bg)(gs_name bg)KFunctor(gs_decl bg)None[](OLink(gs_id fg)(gs_id bg))).
Proof.
 intros fg bg ef promised eb rts dr roles binds s h final Finish.
 destruct(link_body_fresh_emission _ _ _ _ _ _ _ _ _ _ _ _ Finish)as
 [exported[allocated[dri[indexed[Exports[New[Index[Final[Rows Cache]]]]]]]]].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[_[_ Lookup]].
 assert(Symbols:keeps st_syms(mmap(fun k=>let! it:=key_item_at promised k in ret(output_role_item roles it))dr))by keepst.
 rewrite Final;cbn[s_links s_funs st_syms].
 rewrite(Symbols _ _ _ Index);exact Lookup.
Qed.
Lemma source_link_symbol : forall fuel fg bg s h final,
 link_lookup s(gs_id fg)(gs_id bg)=None->link_body(S fuel)fg bg s=Good(h,final)->
 nget(st_syms final)h=Some(GSym h(gs_mod bg)(gs_name bg)KFunctor(gs_decl bg)None[](OLink(gs_id fg)(gs_id bg))).
Proof.
 intros fuel fg bg s h final Miss Run.
 destruct(link_body_fresh_inputs _ _ _ _ _ _ Miss Run)as
 [ef[fneeds[sf[eg[gneeds[sg[xf[sx[rts[sr[sc[EF[EG[Fixed[Types[Capture After]]]]]]]]]]]]]]]].
 destruct(link_body_fresh_body _ _ _ _ _ _ _ _ _ _ _ After)as
 [promised[dr[roles[sv[registered[xs[eb[cb[bodyFinal[View[Register[Body Tail]]]]]]]]]]]].
 destruct(link_body_fresh_checks fg bg ef eg promised eb xf rts dr roles xs bodyFinal h final Tail)as
 [raw[sb[se[renames[sn[st[si[ss[Raw[Effects[Rename[TypeGuard[ItemGuard[Readings Finish]]]]]]]]]]]]]].
 eapply link_finish_symbol;exact Finish.
Qed.
Theorem cached_link_symbol : forall s target provider code,
 LinkInv s ->target<>provider ->link_lookup s target provider=Some code ->
 exists owner,nget(st_syms s)code=Some owner /\gs_origin owner=OLink target provider.
Proof.
 intros s target provider code Linked Different Hit.
 destruct(source_link_cached_creator _ _ _ _ Linked Hit Different)as
 [fuel[fg[bg[before[after[Target[Provider[Valid[Cached[Forwarded[FT[GT[Miss[Run Frame]]]]]]]]]]]]]].
 rewrite <-Target,<-Provider in Miss.
 pose proof(source_link_symbol fuel fg bg before code after Miss Run)as Lookup.
 eexists;split;[exact(fl_keep _ _ (link_frame_source _ _ Frame)_ _ Lookup)|].
 cbn[gs_origin];now rewrite Target,Provider.
Qed.
