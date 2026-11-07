(* A single actual lambda trace retaining both frame-name validity and all
   operational/capture/body/emit receipts. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv BodySound
 LambdaAnnotations LambdaLocalNames BodyTy SymReadonly FnReadonly LambdaTrace LambdaCaptureFold.
Import ListNotations.
Local Open Scope string_scope.
Local Strategy opaque[Nat.of_num_uint FUEL ety_of smap_at].
Theorem lambda_provider_stages_named:forall fuel c e ps rt body bound sg this what s pid pre sm final,
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
 NoDup(map fst(lambda_own_slots ps this)++map capture_name lets)/\
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
 destruct(lambda_local_names_checked _ _ _ _ _ Names)as[Same Unique];subst checked.
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
