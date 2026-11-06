(* Actual anonymous-function stages, before semantic capture reasoning. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv BodySound
 LambdaAnnotations LambdaLocalNames BodyTy SymReadonly FnReadonly.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ety_of smap_at].
Fixpoint lambda_parameter_duplicate(names:list string):option string:=
 match names with[]=>None|n::rest=>if smem n rest then Some n else lambda_parameter_duplicate rest end.
Lemma ks_lambda_parameters_trace:forall c e this what q,keeps st_syms(lambda_parameter_check c e this what q).
Proof. intros c e this what[[pn[ty|]][n want]];unfold lambda_parameter_check;keepst. Qed.
Lemma ks_lambda_return_trace:forall c e this what rt want,keeps st_syms(lambda_return_check c e this what rt want).
Proof. intros c e this what[ty|]want;unfold lambda_return_check;keepst. Qed.
Lemma kf_lambda_parameters_trace:forall c e this what q,keeps st_fns(lambda_parameter_check c e this what q).
Proof. intros c e this what[[pn[ty|]][n want]];unfold lambda_parameter_check;keepst. Qed.
Lemma kf_lambda_return_trace:forall c e this what rt want,keeps st_fns(lambda_return_check c e this what rt want).
Proof. intros c e this what[ty|]want;unfold lambda_return_check;keepst. Qed.
Definition lambda_captured(c:cx)(e:env)(ps:list(name*option aty))(body:aexpr):=
 filter(fun q=>smem(fst q)(free_names_e 10000 body)&&negb(smem(fst q)(map fst ps)))
  (match cx_applying c with[]=>en_locals e|_=>[]end).
Definition lambda_capture_one(c:cx)(e:env)
 (acc:env*list xstmt*list(string*nat*ety))(q:string*(ety*lmut)):=
 let '(le,pre,lets):=acc in let '(n,(t,_)):=q in
 let! v:=new_sym(cx_mod c)n KVal(ADVal n ATUnit None)OCaptured in
 let! stamp:=fresh in
 let le:=set_app(set_val le v t)(KyVal v)[]stamp in
 let! slot:=local_var n in
 record_reify_event(REValue ety key v(canon e t)(reify_arguments e));;
 ret(le,List.app pre[SBindVal v(XVar slot)],List.app lets[(n,v,t)]).
Definition lambda_own_signature(ps:list(name*option aty))(sg:fsig)(this:option ety):=
 FSig(combine(map fst ps)(map snd(sg_params sg)))(sg_ret sg)this.
Definition lambda_own_env(ps:list(name*option aty))(sg:fsig)(this:option ety)
 (base:env)(lets:list(string*nat*ety)):=
 fold_left(fun e q=>let '(n,_,t):=q in set_local e n t LCaptured)lets
 (fold_left(fun e p=>set_local e(fst p)(snd p)LLet)(sg_params(lambda_own_signature ps sg this))base).
Definition lambda_own_context(c:cx)(ps:list(name*option aty))(sg:fsig)(this:option ety):=
 Cx(cx_mod c)(lambda_own_signature ps sg this)this 0(cx_applying c)
 "; an anonymous function bound to a fn has no receiver, and does not capture an enclosing one (D70)"[](cx_pins c).
Definition lambda_own_slots(ps:list(name*option aty))(this:option ety):=
 let prefix:=match this with Some _=>[("this",0)]|None=>[]end in
 List.app prefix(combine(map fst ps)(seq(List.length prefix)(List.length(map fst ps)))).
Definition lambda_own_info(ps:list(name*option aty))(sg:fsig)(this:option ety)
 (base:env)(lets:list(string*nat*ety)):=
 let le:=lambda_own_env ps sg this base lets in let needs:=all_keys base in
 FInfo le needs(map(fun k=>(k,match get_app le k with Some(a,_)=>a|None=>[]end))needs)[].
Definition lambda_generated_body(fuel:nat)(c:cx)(e:env)(ps:list(name*option aty))
 (body:aexpr)(sg:fsig)(this:option ety)(pid:nat)(lets:list(string*nat*ety)):=
 in_reify_code(ROFunction pid)(with_vars(lambda_own_slots ps this)(
 let! locals:=mmap(fun q=>let '(n,v,_):=q in let! slot:=local_var n in ret(SLet slot(XVl v)))lets in
 let! (compiled,_,_):=synth_block fuel(lambda_own_context c ps sg this)e(ABlock[](Some body))(Some(sg_ret sg))in
 ret(fold_stmts locals compiled))).
Lemma ok_lambda_capture_one:forall c e acc q,ok(lambda_capture_one c e acc q).
Proof. intros c e[[le pre]lets][n[t kind]];unfold lambda_capture_one;okt. Qed.
Theorem lambda_provider_stages_tables:forall fuel c e ps rt body bound sg this what s pid pre sm final,
 lambda_provider fuel c e ps rt body bound sg this what s=Good((pid,pre,sm),final)->
 Inv s->exists f le lets captureStart beforePid afterPid registered compiled bodyFinal emitted cached,
 fuel=S f/\
 mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)captureStart=Good((le,pre,lets),beforePid)/\
 Inv captureStart/\Fr s captureStart/\st_syms captureStart=st_syms s/\st_fns captureStart=st_fns s/\
 new_sym(cx_mod c)(gs_name bound)KFn
 (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda beforePid=Good(pid,afterPid)/\
 register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)
  (Some(lambda_own_signature ps sg this))afterPid=Good(tt,registered)/\
 lambda_generated_body f c(lambda_own_env ps sg this le lets)ps body sg this pid lets registered=Good(compiled,bodyFinal)/\
 emit_fn pid compiled bodyFinal=Good(tt,emitted)/\
 upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)))emitted=Good(tt,cached)/\
 smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))cached=Good(sm,final)/\
 Inv beforePid/\Inv registered.
Proof.
 intros[|f]c e ps rt body bound sg this what s pid pre sm final Run I;[discriminate|].
 cbn[lambda_provider]in Run.
 destruct(negb(Nat.eqb(List.length ps)(List.length(sg_params sg))));[exfalso;exact(err_not_good _ _ _ _ _ _ Run)|].
 fold(lambda_parameter_duplicate(map fst ps))in Run.
 destruct(lambda_parameter_duplicate(map fst ps));[exfalso;exact(err_not_good _ _ _ _ _ _ Run)|].
 apply bind_inv in Run as[u[s1[Params Run]]].
 change(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))s=Good(u,s1))in Params.
 assert(Op:ok(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))))
 by(apply ok_miter;intro;apply ok_lambda_parameter_check).
 destruct(Op _ _ _ I Params)as[I1 F1].
 apply bind_inv in Run as[u2[s2[Return Run]]].
 change(lambda_return_check c e this what rt(sg_ret sg)s1=Good(u2,s2))in Return.
 assert(Or:ok(lambda_return_check c e this what rt(sg_ret sg)))by(unfold lambda_return_check;okt;apply ok_ety_of).
 destruct(Or _ _ _ I1 Return)as[I2 F2].
 assert(PrefixSymbols:st_syms s2=st_syms s).
 {rewrite(ks_lambda_return_trace c e this what rt(sg_ret sg)_ _ _ Return).
  assert(Kp:keeps st_syms(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))))
   by(apply keeps_miter;intro;apply ks_lambda_parameters_trace).
  exact(Kp _ _ _ Params). }
 assert(PrefixFunctions:st_fns s2=st_fns s).
 {rewrite(kf_lambda_return_trace c e this what rt(sg_ret sg)_ _ _ Return).
  assert(Kp:keeps st_fns(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))))
   by(apply keeps_miter;intro;apply kf_lambda_parameters_trace).
  exact(Kp _ _ _ Params). }
 apply bind_inv in Run as[[[le pre0]lets][beforePid[Capture Run]]].
 change(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)s2=Good((le,pre0,lets),beforePid))in Capture.
 assert(Oc:ok(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)))
 by(apply ok_mfold;intros;apply ok_lambda_capture_one).
 destruct(Oc _ _ _ I2 Capture)as[Ip Fp].
 apply bind_inv in Run as[id[afterPid[New Run]]].
 destruct(ok_new_sym(cx_mod c)(gs_name bound)KFn(ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda ltac:(discriminate)ltac:(split;discriminate)_ _ _ Ip New)as[Ia Fa].
 apply bind_inv in Run as[namecheck[checked[Names Run]]].
 destruct(lambda_local_names_checked _ _ _ _ _ Names)as[Same _];subst checked.
 apply bind_inv in Run as[u3[registered[Register Run]]];destruct u3.
 apply bind_inv in Run as[compiled[bodyFinal[Body Run]]].
 apply bind_inv in Run as[u4[emitted[Emit Run]]];destruct u4.
 apply bind_inv in Run as[u5[cached[Cache Run]]];destruct u5.
 apply bind_inv in Run as[transfer[last[Transfer Run]]];injection Run as <- <- <- <-.
 exists f,le,lets,s2,beforePid,afterPid,registered,compiled,bodyFinal,emitted,cached.
 repeat first[assumption|reflexivity|solve[eapply fr_trans;eassumption]|lazymatch goal with |- _ /\ _=>split end].
 match type of Register with ?m _ = _ =>
   assert(Oreg:ok m)by(unfold register_reify_code;apply ok_reify_update)
 end.
 exact(proj1(Oreg _ _ _ Ia Register)).
Qed.

Theorem lambda_provider_stages:forall fuel c e ps rt body bound sg this what s pid pre sm final,
 lambda_provider fuel c e ps rt body bound sg this what s=Good((pid,pre,sm),final)->
 Inv s->exists f le lets captureStart beforePid afterPid registered compiled bodyFinal emitted cached,
 fuel=S f/\
 mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)captureStart=Good((le,pre,lets),beforePid)/\
 Inv captureStart/\Fr s captureStart/\
 new_sym(cx_mod c)(gs_name bound)KFn
 (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda beforePid=Good(pid,afterPid)/\
 register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)
  (Some(lambda_own_signature ps sg this))afterPid=Good(tt,registered)/\
 lambda_generated_body f c(lambda_own_env ps sg this le lets)ps body sg this pid lets registered=Good(compiled,bodyFinal)/\
 emit_fn pid compiled bodyFinal=Good(tt,emitted)/\
 upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)))emitted=Good(tt,cached)/\
 smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))cached=Good(sm,final)/\
 Inv beforePid/\Inv registered.
Proof.
 intros fuel c e ps rt body bound sg this what s pid pre sm final Run I.
 destruct(lambda_provider_stages_tables _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Run I)
 as(f&le&lets&captureStart&beforePid&afterPid&registered&compiled&bodyFinal&emitted&cached&
 Fuel&Capture&Ic&Fc&Symbols&Functions&New&Register&Body&Emit&Cache&Transfer&Ip&Ir).
 exists f,le,lets,captureStart,beforePid,afterPid,registered,compiled,bodyFinal,emitted,cached.
 repeat first[assumption|reflexivity|lazymatch goal with |- _ /\ _=>split end].
Qed.
