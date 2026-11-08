(* Actual compiler operations preserve generated-code origin receipts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame ElabInv
 ForwarderHistory ForwarderTrace FnReadonly SymReadonly FnBound FnBoundEmit FnBoundSynth FnPresentStable FnPresentEmit FnPresentSynth
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 LambdaOrigin LambdaOriginOps LambdaTrace LambdaHeader EtaHeader.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma lha_register:forall owner e needs sg,LH_at owner(register_reify_code(ROFunction owner)e needs sg).
Proof.
 intros;split;[apply hse_register|];split.
 - apply bfa_of_bf,bf_keeps;unfold register_reify_code;[okt|keepst].
 - intros b w s value final Load I Bound W Below Run;apply new_lambda_origins_same_symbols.
   unfold register_reify_code,upd in Run;injection Run as <- <-;reflexivity.
Qed.
Create HintDb lhdb.
Lemma lh_record_event:forall event,LH(record_reify_event event).
Proof.
 intros;split;[apply hs_record_event|];split.
 - apply bf_keeps;unfold record_reify_event;[okt|keepst].
 - intros b w s value final Load I Bound W Run;apply new_lambda_origins_same_symbols.
   unfold record_reify_event,upd in Run;injection Run as <- <-;reflexivity.
Qed.
Lemma lh_record_exports:forall owner types items inputs promised actual roles,
 LH(record_reify_exports_at owner types items inputs promised actual roles).
Proof.
 intros;split;[apply hs_record_exports|];split.
 - apply bf_keeps;unfold record_reify_exports_at;[okt|keepst].
 - intros b w s value final Load I Bound W Run;apply new_lambda_origins_same_symbols.
   unfold record_reify_exports_at,upd in Run;injection Run as <- <-;reflexivity.
Qed.
#[export]Hint Resolve lh_record_event lh_record_exports:lhdb.
#[local]Hint Resolve lh_ret:lhdb.
Lemma lh_keeps_first : forall A (m : M A), keeps st_fns m -> keeps st_syms m -> keeps function_headers m -> ok m -> LH m.
Proof. intros A m Functions Symbols Headers Valid; apply lh_keeps; assumption. Qed.

Ltac lh_kept:=apply lh_keeps_first;[solve[auto with keepsdb]|solve[auto with keepsdb]|solve[auto with keepsdb]|solve[okt]].
Ltac lh_leaf:=apply lh_keeps_first;[solve[keepst]|solve[keepst]|solve[keepst]|solve[okt]].
Ltac lht:=repeat(fintros;first[solve[auto with lhdb]|lazymatch goal with
 | |-LH_at ?owner(emit_fn ?owner _)=>apply lha_emit
 | |-LH_at ?owner(register_reify_code(ROFunction ?owner)_ _ _)=>apply lha_register
 | H : gs_id ?g = ?key |- LH_at ?key (lower_body _ ?g)=>rewrite <-H;solve[auto]
 | |-LH(mbind(new_sym _ _ _ _ OLambda)_)=>fail 1
 | |-LH(mbind(new_sym _ _ _ _ _)_)=>apply lh_new_sym_bind;[discriminate|split;discriminate|split;[discriminate|intro;discriminate]|intro]
 | |-LH(mbind _ _)=>apply lh_bind
 | |-LH(mmap _ _)=>apply lh_mmap
 | |-LH(miter _ _)=>apply lh_miter
 | |-LH(mfold _ _ _)=>apply lh_mfold
 | |-LH(with_vars _ _)=>apply lh_with_vars
 | |-LH(in_reify_code _ _)=>unfold in_reify_code
 | |-LH_at _(mbind(sym _)_)=>apply lha_bind_sym;intros
 | |-LH_at _(mbind _ _)=>apply lha_bind
 | |-LH_at _(mmap _ _)=>apply lha_mmap
 | |-LH_at _(miter _ _)=>apply lha_miter
 | |-LH_at _(mfold _ _ _)=>apply lha_mfold
 | |-LH_at _(with_vars _ _)=>apply lha_with_vars
 | |-LH_at _(in_reify_code _ _)=>unfold in_reify_code
 | |-LH_at _ ?m=>first[solve[apply lh_at_of_lh;lht]|match m with context[match ?x with _=>_ end]=>destruct x end]
 | |-LH ?m=>first[solve[lh_kept]|lazymatch m with
   |lower_body _ _=>fail 1
   |synth_block _ _ _ _ _=>fail 1
   |upd _=>lh_leaf
   |(fun s=>_)=>lh_leaf
   |_=>first[match m with context[match ?x with _=>_ end]=>destruct x end|
      let h:=head_of m in progress unfold h]
   end]
 end]).
Lemma lha_lower_body_step:forall fuel,
 (forall c e b exp,LH(synth_block fuel c e b exp))->forall g,LH_at(gs_id g)(lower_body(S fuel)g).
Proof. intros fuel Body g;cbn[lower_body];lht. Qed.
From NM Require Import ReifyBodyHeader.
Definition forwarder_generated_tail fuel tg id:M gsym:=
 upd(s_fwd(fun m=>nset m(gs_id tg)id));;
 let! generated:=sym id in lower_body fuel generated;;ret generated.
Lemma lha_forwarder_tail:forall fuel,
 (forall g,LH_at(gs_id g)(lower_body fuel g))->
 forall tg id,LH_at id(forwarder_generated_tail fuel tg id).
Proof. intros;unfold forwarder_generated_tail;lht. Qed.
Lemma lh_forwarder_step:forall fuel,(forall g,LH_at(gs_id g)(lower_body fuel g))->forall tg,LH(forwarder(S fuel)tg).
Proof.
 intros fuel Lower tg;split;[apply hs_forwarder_step;intro;exact(proj1(Lower g))|].
 split;[apply bf_forwarder_step;intro;exact(proj1(proj2(Lower g)))|].
 intros b w s value final Load I Bound World Run.
 destruct(nget(st_fwd s)(gs_id tg))as[old|]eqn:Cached.
 - pose proof(forwarder_hit_trace _ _ _ _ _ _ Cached Run)as Read.
   unfold sym in Read;destruct(nget(st_syms s)old);[injection Read as <- <-|discriminate].
   apply new_lambda_origins_same_symbols;reflexivity.
 - destruct(forwarder_miss_trace _ _ _ _ _ Cached Run)as[f[id[allocated[cached[ready
    [Fuel[Id[New[Cache[Read Body]]]]]]]]]].
   injection Fuel as Eq;subst f.
   pose proof(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id0[Next Symbol]].
   unfold upd in Cache;injection Cache as <-.
   unfold sym in Read;cbn[s_fwd st_syms]in Read;rewrite Symbol in Read;injection Read as <- <-.
   set(g:=GSym id(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)None[](OForwarder(gs_id tg)))in *.
   assert(Tail:forwarder_generated_tail fuel tg id allocated=Good(g,final)).
   {unfold forwarder_generated_tail,mbind,upd,sym;cbn[s_fwd st_syms];rewrite Symbol.
    change(mbind(lower_body fuel g)(fun _=>ret g)(s_fwd(fun m=>nset m(gs_id tg)id)allocated)=Good(g,final)).
    unfold mbind;rewrite Body;reflexivity. }
   eapply(@lambda_origin_created_run _ (gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))
    (forwarder_generated_tail fuel tg)b w s s id allocated g final);
   [discriminate|split;discriminate|intro;apply lha_forwarder_tail;exact Lower|
    exact Load|exact I|exact Bound|exact World|apply fr_refl|
    apply new_lambda_origins_same_symbols;reflexivity|exact New|exact Tail|].
   assert(Ia:Inv allocated)by(exact(proj1(ok_new_sym (gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg)) ltac:(discriminate)ltac:(split;discriminate)_ _ _ I New))).
   assert(Ir:Inv(s_fwd(fun m=>nset m(gs_id tg)id)allocated))by(destruct Ia;constructor;assumption).
   assert(Absent:nassoc id(st_fns(s_fwd(fun m=>nset m(gs_id tg)id)allocated))=None).
   {change(nassoc id(st_fns allocated)=None).
    assert(Keep:keeps st_fns(new_sym(gs_mod tg)(gs_name tg)KFn(forwarder_decl tg)(OForwarder(gs_id tg))))by(unfold new_sym;keepst).
    rewrite(Keep _ _ _ New),Id;apply fn_bound_fresh_absent;[exact Bound|lia]. }
   assert(Has:exists block,fn_body g=Some block)by(unfold g,fn_body,forwarder_decl;cbn beta iota zeta;eexists;reflexivity).
   destruct Has as[block Has].
   assert(Bready:gs_id g<st_next(s_fwd(fun m=>nset m(gs_id tg)id)allocated))by(change(id<st_next allocated);rewrite Next;lia).
   destruct(completed_body_header _ _ _ _ _ Has Absent Bready Ir Body)
    as(f0&info&si&this&sr&sg&ss&marker&registered&x&t&bt&bodyFinal&Facts).
   destruct Facts as(F0&Info&Receiver&Signature&Marker&Register&Compiled&Present&Header).
   exists x;split;[exact Present|].
   assert(Physical:nget(st_syms(s_fwd(fun m=>nset m(gs_id tg)id)allocated))id=Some g)by(exact Symbol).
   right;right;exists fuel,g,block,(s_fwd(fun m=>nset m(gs_id tg)id)allocated),final.
   repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
Qed.
Lemma lh_capture_one:forall c e acc q,LH(lambda_capture_one c e acc q).
Proof. intros c e[[le pre]lets][n[t mut]];unfold lambda_capture_one;lht. Qed.
Lemma lh_generated_body:forall fuel,
 (forall c e block expected,LH(synth_block fuel c e block expected))->
 forall c e ps body sg this pid lets,LH(lambda_generated_body fuel c e ps body sg this pid lets).
Proof. intros fuel Body c e ps body sg this pid lets;unfold lambda_generated_body;lht. Qed.
Definition lambda_generated_tail(fuel:nat)(c:cx)(e:env)(ps:list(name*option aty))(body:aexpr)
 (sg:fsig)(this:option ety)(le:env)(lets:list(string*nat*ety))(pre:list xstmt)(pid:nat):M(nat*list xstmt*smap):=
 register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)(Some(lambda_own_signature ps sg this));;
 let! compiled:=lambda_generated_body fuel c(lambda_own_env ps sg this le lets)ps body sg this pid lets in
 emit_fn pid compiled;;
 upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)));;
 let! sm:=smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))in
 ret(pid,pre,sm).
Lemma lha_generated_tail:forall fuel,
 (forall c e block expected,LH(synth_block fuel c e block expected))->
 forall c e ps body sg this le lets pre pid,
 LH_at pid(lambda_generated_tail fuel c e ps body sg this le lets pre pid).
Proof.
 intros fuel Body c e ps body sg this le lets pre pid;unfold lambda_generated_tail;lht.
Qed.
Lemma lh_lambda_provider_step:forall fuel,
 (forall c e block expected,LH(synth_block fuel c e block expected))->
 forall c e ps rt body bound sg this what,LH(lambda_provider(S fuel)c e ps rt body bound sg this what).
Proof.
 intros fuel Body c e ps rt body bound sg this what;split;[apply hs_lambda_provider|].
 split;[apply bf_lambda_provider|].
 intros b w s value final Load I Bounded World Run.
 destruct value as[[pid pre]sm].
 pose proof Run as WholeRun.
 destruct(lambda_provider_stages_tables _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run I)
 as(f&le&lets&captureStart&beforePid&afterPid&registered&compiled&bodyFinal&emitted&cached&
 Fuel&Capture&Ic&Fc&Symbols&Functions&New&Register&BodyRun&Emit&Cache&Transfer&Ip&Ir).
 assert(f=fuel)by congruence;subst f.
 assert(Cap:LH(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body))).
 {apply lh_mfold;intros;apply lh_capture_one. }
 assert(Bc:FnBound captureStart)by(exact(fn_bound_keeps Bounded Fc Functions)).
 assert(Wc:st_w captureStart=w)by(rewrite(fr_w _ _ Fc);exact World).
 destruct(proj1(proj1(proj1 Cap))_ _ _ Ic Capture)as[Ib Fb].
 assert(Bp:FnBound beforePid)by exact(proj2(proj1(proj2 Cap))_ _ _ Ic Bc Capture).
 assert(Wp:st_w beforePid=w)by(rewrite(fr_w _ _ Fb);exact Wc).
 assert(Prefix:NewLambdaOrigins s beforePid).
 {eapply new_lambda_origins_trans;[exact Ic|exact Fc|exact Fb|
 exact(proj2(proj1(proj1 Cap))_ _ _ Ic Capture)| | |exact(proj2(proj2 Cap)b w _ _ _ Load Ic Bc Wc Capture)].
 - intros key code Below Present;apply(proj2(proj1 Cap)_ _ _ Ic Capture key Below).
   left;exists code;exact Present.
 - apply new_lambda_origins_same_symbols;exact Symbols. }
 assert(Tail:lambda_generated_tail fuel c e ps body sg this le lets pre pid afterPid=Good((pid,pre,sm),final)).
 {unfold lambda_generated_tail,mbind;rewrite Register,BodyRun,Emit,Cache,Transfer;reflexivity. }
 eapply(@lambda_origin_created_run _ (cx_mod c) (gs_name bound) KFn (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body)))) OLambda (lambda_generated_tail fuel c e ps body sg this le lets pre) b w s beforePid pid afterPid (pid,pre,sm) final);
 [discriminate|split;discriminate|intro;apply lha_generated_tail;exact Body|exact Load|exact Ip|exact Bp|exact Wp|
 eapply fr_trans;[exact Fc|exact Fb]|exact Prefix|exact New|exact Tail|].
 assert(Kf:keeps st_fns(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))
 by(unfold smap_at;keepst).
 assert(Present:nassoc pid(st_fns final)=Some compiled).
 {rewrite(Kf _ _ _ Transfer);unfold upd in Cache;injection Cache as <-.
  unfold emit_fn,upd in Emit;injection Emit as <-.
  cbn[s_info st_fns s_fns nassoc assoc];rewrite Nat.eqb_refl;reflexivity. }
 exists compiled;split;[exact Present|].
 left;exists(S fuel),c,e,ps,rt,body,bound,sg,this,what,s,pre,sm,final.
 repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
Qed.
Definition eta_generated_tail(provider:gsym)(info:fninfo)(sm:smap)(id:nat):M nat:=
 let! th:=call_theta(fi_env info)provider in
 register_reify_code(ROFunction id)(fi_env info)(fi_needs info)None;;
 emit_fn id(XCallD(gs_id provider)th sm(map XVar(seq 1(List.length(fn_params provider)))));;
 upd(s_wrappers(fun m=>nset m(gs_id provider)id));;
 ret id.
Lemma lha_eta_tail:forall provider info sm id,LH_at id(eta_generated_tail provider info sm id).
Proof. intros;unfold eta_generated_tail;lht. Qed.
Lemma lh_wrapper:forall fuel provider,LH(wrapper fuel provider).
Proof.
 intros[|fuel]provider;[apply lh_keeps;unfold wrapper;[okt|keepst|keepst|keepst]|].
 split;[apply hs_wrapper|];split;[apply bf_wrapper|].
 intros b w s value final Load I Bounded World Run;pose proof Run as WholeRun.
 cbn[wrapper wrapper_cached]in Run;apply bind_inv in Run as[s0[s1[Read Run]]];injection Read as <- <-.
 destruct(nget(st_wrappers s)(gs_id provider))as[old|]eqn:Found.
 - injection Run as <- <-;apply new_lambda_origins_same_symbols;reflexivity.
 - apply bind_inv in Run as[physical[physicalState[Physical Run]]].
   unfold sym in Physical;destruct(nget(st_syms s)(gs_id provider))as[stored|]eqn:Stored;[|discriminate].
   injection Physical as <- <-.
   pose proof Run as CreateRun;unfold wrapper_create in Run.
   apply bind_inv in Run as[info[s2[Info Run]]].
   apply bind_inv in Run as[sm[before[Transfer Run]]].
   apply bind_inv in Run as[id[mid[New Run]]].
   apply bind_inv in Run as[th[s5[Theta Run]]].
   apply bind_inv in Run as[u[s6[Register Run]]];destruct u.
   apply bind_inv in Run as[u[s7[Emit Run]]];destruct u.
   apply bind_inv in Run as[u[s8[Cache Run]]];destruct u;injection Run as <- <-.
   destruct(ok_fn_info_frame _ _ _ _ I Info)as[I2 F2].
   assert(OT:ok(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by(unfold smap_at;okt).
   destruct(OT _ _ _ I2 Transfer)as[Ib Fb].
   assert(Frames:Fr s before)by(eapply fr_trans;eassumption).
   assert(Kt:keeps st_syms(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by(unfold smap_at;keepst).
   assert(Kf:keeps st_fns(smap_at(fi_env info)(fi_env info)(map(fun k=>(k,k))(fi_needs info))))by(unfold smap_at;keepst).
   assert(Symbols:st_syms before=st_syms s).
   {rewrite(Kt _ _ _ Transfer);exact(ks_fn_info_frame stored _ _ _ Info). }
   assert(Functions:st_fns before=st_fns s).
   {rewrite(Kf _ _ _ Transfer);exact(kf_fn_info_frame stored _ _ _ Info). }
   assert(Bp:FnBound before)by exact(fn_bound_keeps Bounded Frames Functions).
   assert(Wp:st_w before=w)by(rewrite(fr_w _ _ Frames);exact World).
   assert(Tail:eta_generated_tail stored info sm id mid=Good(id,s8)).
   {unfold eta_generated_tail,mbind;rewrite Theta,Register,Emit,Cache;reflexivity. }
   eapply(@lambda_origin_created_run _ (gs_mod stored) (gs_name stored++"$recv") KFn (gs_decl stored) OLambda (eta_generated_tail stored info sm) b w s before id mid id s8);
   [discriminate|split;discriminate|intro;apply lha_eta_tail|exact Load|exact Ib|exact Bp|exact Wp|exact Frames|
   apply new_lambda_origins_same_symbols;exact Symbols|exact New|exact Tail|].
   destruct(wrapper_create_header _ _ _ _ CreateRun)as[read[theta[transfer[Header[Present Ready]]]]].
   eexists;split;[exact Present|].
   right;left;exists(S fuel),provider,s,s8.
   repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
Qed.
