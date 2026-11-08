(* Actual successful lambda suffixes supply the earlier static/body frames. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData ExecutionModel NMap Sx Elab ElabSound ElabFrame BodySoundCore
 LambdaTrace LambdaSmapIdentity.
Import ListNotations.
Lemma ok_lambda_generated:forall fuel c en ps body sg this pid lets,
 ok(lambda_generated_body fuel c en ps body sg this pid lets).
Proof. intros;unfold lambda_generated_body,in_reify_code;okt. Qed.
Section Stages.
Variables(w:Elab.world)(P:xprogram)(R:nat->list nat)(Fl:string->nat).
Theorem lambda_stage_frames:forall fuel c e F ps body bound sg this le lets beforePid pid afterPid registered compiled bodyFinal emitted cached sm final,
 new_sym(cx_mod c)(gs_name bound)KFn(ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda beforePid=Good(pid,afterPid)->
 register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)(Some(lambda_own_signature ps sg this))afterPid=Good(tt,registered)->
 lambda_generated_body fuel c(lambda_own_env ps sg this le lets)ps body sg this pid lets registered=Good(compiled,bodyFinal)->
 emit_fn pid compiled bodyFinal=Good(tt,emitted)->
 upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)))emitted=Good(tt,cached)->
 smap_at(lambda_own_env ps sg this le lets)e(map(fun k=>(k,k))(all_keys le))cached=Good(sm,final)->
 Inv beforePid->Post w P R Fl final F->
 Later w P R Fl bodyFinal/\Later w P R Fl emitted/\Post w P R Fl beforePid F/\
 Inv cached/\Fr afterPid bodyFinal.
Proof.
 intros fuel c e F ps body bound sg this le lets beforePid pid afterPid registered compiled bodyFinal emitted cached sm final
 New Register Body Emit Cache Sm I Hp.
 assert(On:ok(new_sym(cx_mod c)(gs_name bound)KFn(ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda))by okt.
 destruct(On _ _ _ I New)as[Ia Fa].
 assert(Or:ok(register_reify_code(ROFunction pid)(lambda_own_env ps sg this le lets)(all_keys le)(Some(lambda_own_signature ps sg this))))by(unfold register_reify_code;apply ok_reify_update).
 destruct(Or _ _ _ Ia Register)as[Ir Fr].
 destruct(ok_lambda_generated _ _ _ _ _ _ _ _ _ _ _ _ Ir Body)as[Ib Fb].
 assert(Oe:ok(emit_fn pid compiled))by okt;destruct(Oe _ _ _ Ib Emit)as[Ie Fe].
 assert(Oc:ok(upd(s_info(fun m=>nset m pid(lambda_own_info ps sg this le lets)))))by okt.
 destruct(Oc _ _ _ Ie Cache)as[Ic Fc].
 destruct(ok_lambda_smap _ _ _ _ _ _ Ic Sm)as[If Fsm].
 pose proof(later_back w P R Fl _ _ Fsm(proj1 Hp))as Lc.
 pose proof(later_back w P R Fl _ _ Fc Lc)as Le.
 pose proof(later_back w P R Fl _ _ Fe Le)as Lb.
 split;[exact Lb|];split;[exact Le|];split.
 - eapply post_back;[|exact Hp].
   exact(fr_trans _ _ _ Fa(fr_trans _ _ _ Fr(fr_trans _ _ _ Fb(fr_trans _ _ _ Fe(fr_trans _ _ _ Fc Fsm))))).
 - split;[exact Ic|exact(fr_trans _ _ _ Fr Fb)].
Qed.
End Stages.
