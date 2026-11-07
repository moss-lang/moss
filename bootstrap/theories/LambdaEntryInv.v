(* Prefix input invariants propagate forward through actual annotation,
   capture allocation and header registration; none is read backwards. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound ElabFrame ElabInv
 InfoInvariant InfoPreserve WrapperReadonly FwdInvariant FwdReadonly FwdPreserve
 BodySoundCore LambdaAnnotations LambdaTrace.
Import ListNotations.
Definition LambdaEntryReadonly{X}(m:M X):Prop:=NI m/\keeps st_wrappers m/\keeps st_fwd m.
Lemma lambda_entry_cache:forall X(m:M X),ok m->LambdaEntryReadonly m->okI m.
Proof. intros X m Ok[N[Wr Fwd]];apply okI_ni;assumption. Qed.
Lemma lambda_entry_step:forall X(m:M X)s x sf,
 ok m->LambdaEntryReadonly m->m s=Good(x,sf)->Inv s->InfoInv s->FwdInv s->
 Inv sf/\InfoInv sf/\FwdInv sf/\Fr s sf.
Proof.
 intros X m s x sf Ok Readonly Run Hi Cache Fwd.
 destruct(Ok _ _ _ Hi Run)as[If Frame].
 pose proof(proj2(lambda_entry_cache _ _ Ok Readonly)_ _ _ Hi Cache Run)as Cf.
 destruct Readonly as[N[Wr Keep]].
 assert(Ff:FwdInv sf)by(eapply fwd_inv_frame;[exact Fwd|exact Frame|exact(Keep _ _ _ Run)]).
 split;[exact If|];split;[exact Cf|];split;assumption.
Qed.
Lemma lambda_parameter_readonly:forall c e this what q,LambdaEntryReadonly(lambda_parameter_check c e this what q).
Proof.
 intros c e this what[[pn[ty|]][name want]];
 (unfold lambda_parameter_check;split;[nit|split;keepst]).
Qed.
Lemma lambda_return_readonly:forall c e this what rt want,LambdaEntryReadonly(lambda_return_check c e this what rt want).
Proof. intros c e this what[ty|]want;(unfold lambda_return_check;split;[nit|split;keepst]). Qed.
Lemma lambda_capture_readonly:forall c e acc q,LambdaEntryReadonly(lambda_capture_one c e acc q).
Proof.
 intros c e[[le pre]lets][n[t mu]];unfold lambda_capture_one;split;[nit|split;keepst].
Qed.
Lemma lambda_new_readonly:forall mi name body,
 LambdaEntryReadonly(new_sym mi name KFn(ADFn RPlain name[]None(Some(ABlock[](Some body))))OLambda).
Proof. intros;unfold new_sym;split;[nit|split;keepst]. Qed.
Lemma lambda_register_readonly:forall owner e needs sg,LambdaEntryReadonly(register_reify_code owner e needs sg).
Proof. intros;unfold register_reify_code;split;[nit|split;keepst]. Qed.
Lemma lambda_entry_miter:forall X(f:X->M unit)xs,
 (forall x,LambdaEntryReadonly(f x))->LambdaEntryReadonly(miter f xs).
Proof.
 intros X f xs Read;split;[apply ni_miter;intro x;exact(proj1(Read x))|].
 split;apply keeps_miter;intro x;[exact(proj1(proj2(Read x)))|exact(proj2(proj2(Read x)))].
Qed.
Lemma lambda_entry_mfold:forall X Y(f:Y->X->M Y)ys xs,
 (forall y x,LambdaEntryReadonly(f y x))->LambdaEntryReadonly(mfold f ys xs).
Proof.
 intros X Y f ys xs Read;split;[apply ni_mfold;intros y x;exact(proj1(Read y x))|].
 split;apply keeps_mfold;intros y x;[exact(proj1(proj2(Read y x)))|exact(proj2(proj2(Read y x)))].
Qed.
Theorem lambda_entry_invariants:forall c e ps rt body bound sg this what s sp captureStart le pre lets beforePid pid afterPid registered,
 miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))s=Good(tt,sp)->
 lambda_return_check c e this what rt(sg_ret sg)sp=Good(tt,captureStart)->
 mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)captureStart=Good((le,pre,lets),beforePid)->
 new_sym(cx_mod c)(gs_name bound)KFn(ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda beforePid=Good(pid,afterPid)->
 register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)(Some(lambda_own_signature ps sg this))afterPid=Good(tt,registered)->
 Inv s->InfoInv s->FwdInv s->
 Inv captureStart/\InfoInv captureStart/\FwdInv captureStart/\
 Inv beforePid/\InfoInv beforePid/\FwdInv beforePid/\
 Inv registered/\InfoInv registered/\FwdInv registered.
Proof.
 intros c e ps rt body bound sg this what s sp captureStart le pre lets beforePid pid afterPid registered Params Return Capture New Register I Cache Fwd.
 assert(Op:ok(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))))by(apply ok_miter;intro;apply ok_lambda_parameter_check).
 destruct(lambda_entry_step _ _ _ _ _ Op(lambda_entry_miter _ _ _ (lambda_parameter_readonly c e this what))Params I Cache Fwd)as[I1[C1[F1 Fr1]]].
 assert(Or:ok(lambda_return_check c e this what rt(sg_ret sg)))by(unfold lambda_return_check;okt;apply ok_ety_of).
 destruct(lambda_entry_step _ _ _ _ _ Or(lambda_return_readonly c e this what rt(sg_ret sg))Return I1 C1 F1)as[I2[C2[F2 Fr2]]].
 assert(Oc:ok(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)))by(apply ok_mfold;intros;apply ok_lambda_capture_one).
 destruct(lambda_entry_step _ _ _ _ _ Oc(lambda_entry_mfold _ _ _ _ _ (lambda_capture_readonly c e))Capture I2 C2 F2)as[Ip[Cp[Fp Frp]]].
 assert(On:ok(new_sym(cx_mod c)(gs_name bound)KFn(ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda))by okt.
 destruct(lambda_entry_step _ _ _ _ _ On(lambda_new_readonly _ _ _)New Ip Cp Fp)as[Ia[Ca[Fa Fra]]].
 assert(Oreg:ok(register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)(Some(lambda_own_signature ps sg this))))by(unfold register_reify_code;apply ok_reify_update).
 destruct(lambda_entry_step _ _ _ _ _ Oreg(lambda_register_readonly _ _ _ _)Register Ia Ca Fa)as[Ir[Cr[Fr Frr]]].
 repeat first[assumption|lazymatch goal with|-_/\_=>split end].
Qed.
