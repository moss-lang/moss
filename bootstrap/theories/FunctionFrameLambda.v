(* A lambda invocation retains the body recursion's frames and installs its
   own real code-entry signature and creator receipt. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv
 FnReadonly FnBound FnBoundEmit FnBoundSynth FnPresentStable
 ReifyHeader ReifyHeaderReadonly ReifyHeaderStable ReifyHeaderEmit ReifyHeaderSynth
 ReifyHeaderPresent ReifyFrameReady LambdaOrigin LambdaTrace LambdaHeader
 FunctionFrameHistory FunctionFrameOps.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Lemma ff_lambda_inner:forall fuel,
 (forall c e b exp,FF(synth_block fuel c e b exp))->
 forall c e ps body sg this(lets:list(string*nat*ety)),
 FF(let! locals:=mmap(fun q=>let '(n,v,_):=q in let!slot:=local_var n in ret(SLet slot(XVl v)))lets in
    let! (compiled,_,_):=synth_block fuel(lambda_own_context c ps sg this)e
       (ABlock[](Some body))(Some(sg_ret sg))in ret(fold_stmts locals compiled)).
Proof.
 intros fuel IH c e ps body sg this lets.
 apply ff_bind.
 - apply ff_mmap;intros[[n v]t];fft.
 - intro locals;apply ff_bind;[apply IH|intros[[compiled ty]bound];apply ff_ret].
Qed.
Lemma ff_lambda_generated_body:forall fuel,
 (forall c e b exp,FF(synth_block fuel c e b exp))->
 forall c e ps body sg this pid lets,FF(lambda_generated_body fuel c e ps body sg this pid lets).
Proof.
 intros fuel IH c e ps body sg this pid lets;unfold lambda_generated_body.
 assert(Inner:=ff_lambda_inner fuel IH c e ps body sg this lets).
 assert(Scoped:FF(with_vars(lambda_own_slots ps this)
  (let! locals:=mmap(fun q=>let '(n,v,_):=q in let!slot:=local_var n in ret(SLet slot(XVl v)))lets in
   let! (compiled,_,_):=synth_block fuel(lambda_own_context c ps sg this)e
       (ABlock[](Some body))(Some(sg_ret sg))in ret(fold_stmts locals compiled)))).
 {apply ff_with_vars;[|exact Inner].
  apply hs_bind.
  - apply hs_mmap;intros[[n v]t];apply hs_bind;
     [apply hs_keeps;[apply pf_keeps;[okt|keepst]|apply header_keeps_of_rows;keepst]|intro;apply hs_ret].
  - intro locals;apply hs_bind;[apply hs_synth_block|intros[[compiled ty]bound];apply hs_ret]. }
 unfold in_reify_code;fft.
Qed.
Lemma ff_lambda_provider_step:forall fuel,
 (forall c e b exp,FF(synth_block fuel c e b exp))->
 forall c e ps rt expression bound sg this what,
 FF(lambda_provider(S fuel)c e ps rt expression bound sg this what).
Proof.
 intros fuel IH c e ps rt expression bound sg this what.
 split;[apply hp_of_hs,hs_lambda_provider|];split;[apply bf_lambda_provider|].
 intros initial value final I Binitial Run.
 destruct value as[[pid pre]sm];pose proof Run as WholeRun.
 destruct(lambda_provider_stages_tables _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run I)
 as(f&le&lets&captureStart&beforePid&afterPid&registered&compiled&bodyFinal&emitted&cached&
 Fuel&Capture&Ic&Fc&Symbols&Functions&New&Register&Body&Emit&Cache&Transfer&Ip&Ir).
 injection Fuel as SameFuel;subst f.
 assert(Ocapture:ok(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps expression)))
 by(apply ok_mfold;intros;apply ok_lambda_capture_one).
 destruct(Ocapture _ _ _ Ic Capture)as[Ib Fcapture].
 assert(Onew:ok(new_sym(cx_mod c)(gs_name bound)KFn
  (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some expression))))OLambda))by okt.
 destruct(Onew _ _ _ Ip New)as[Ia Fnew].
 assert(Oregister:ok(register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)
  (all_keys le)(Some(lambda_own_signature ps sg this))))by(unfold register_reify_code;okt).
 destruct(Oregister _ _ _ Ia Register)as[Ireg Fregister].
 assert(Kcapture:keeps st_fns(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps expression)))
 by(apply keeps_mfold;intros[[ce cp]cl][n[t kind]];unfold lambda_capture_one;keepst).
 assert(Knew:keeps st_fns(new_sym(cx_mod c)(gs_name bound)KFn
  (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some expression))))OLambda))by(unfold new_sym;keepst).
 assert(Kregister:keeps st_fns(register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)
  (all_keys le)(Some(lambda_own_signature ps sg this))))by(unfold register_reify_code;keepst).
 assert(Rows:st_fns registered=st_fns initial).
 {rewrite(Kregister _ _ _ Register),(Knew _ _ _ New),(Kcapture _ _ _ Capture),Functions;reflexivity. }
 assert(Breg:FnBound registered).
 {eapply fn_bound_keeps;[exact Binitial| |exact Rows].
  exact(fr_trans _ _ _ Fc(fr_trans _ _ _ Fcapture(fr_trans _ _ _ Fnew Fregister))). }
 pose proof(ff_lambda_generated_body fuel IH c(lambda_own_env ps sg this le lets)ps expression sg this pid lets)as Block.
 destruct(proj1(proj1(proj1 Block))_ _ _ Ir Body)as[Ibody Fbody].
 assert(Oemit:ok(emit_fn pid compiled))by apply ok_emit_fn.
 destruct(Oemit _ _ _ Ibody Emit)as[Iem Femit].
 assert(Ocache:ok(upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)))))by okt.
 destruct(Ocache _ _ _ Iem Cache)as[Icache Fcache].
 assert(Otransfer:ok(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))by(unfold smap_at;okt).
 destruct(Otransfer _ _ _ Icache Transfer)as[If Ftransfer].
 assert(Kfunctions:keeps st_fns(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))by(unfold smap_at;keepst).
 assert(Kheaders:keeps function_headers(smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))))by(unfold smap_at;keepst).
 assert(AfterCache:cached=s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets))emitted).
 {unfold upd in Cache;injection Cache;auto. }
 assert(AfterEmit:emitted=s_fns(fun rows=>(pid,compiled)::rows)bodyFinal).
 {unfold emit_fn,upd in Emit;injection Emit;auto. }
 assert(HeaderTail:forall f,function_header final f=function_header bodyFinal f).
 {intro f;rewrite(function_header_lookup final f),(Kheaders _ _ _ Transfer),<-function_header_lookup.
  rewrite AfterCache,AfterEmit;reflexivity. }
 intros f body Present Missing;destruct(Nat.eq_dec f pid)as[->|Other].
 - destruct(@lambda_provider_header(S fuel)c e ps rt expression bound sg this what initial pid pre sm final WholeRun I)
    as(own&captured&code&Header&Lookup).
   rewrite Present in Lookup;injection Lookup as <-.
   split.
   + exists OLambda;left;exists(S fuel),c,e,ps,rt,expression,bound,sg,this,what,initial,pre,sm,final.
     repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
   + eapply function_header_signature_ready;exact Header.
 - assert(Middle:nassoc f(st_fns bodyFinal)=Some body).
   {rewrite(Kfunctions _ _ _ Transfer),AfterCache,AfterEmit in Present.
    cbn[s_info st_fns s_fns nassoc assoc]in Present.
    assert(Eq:Nat.eqb f pid=false)by(apply Nat.eqb_neq;exact Other).
    rewrite Eq in Present;exact Present. }
   assert(NotRegistered:nassoc f(st_fns registered)=None)by(rewrite Rows;exact Missing).
   eapply function_frame_origin_transport;[exact(fr_trans _ _ _ Femit(fr_trans _ _ _ Fcache Ftransfer))|
    exact(HeaderTail f)|exact Middle|exact Present|].
   exact(proj2(proj2 Block)_ _ _ Ir Breg Body _ _ Middle NotRegistered).
Qed.
