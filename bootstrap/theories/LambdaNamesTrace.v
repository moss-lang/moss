(* Successful lambda generation checks all frame names, including captured
   locals and the implicit receiver, before compiling its body. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound LambdaTrace LambdaLocalNames LambdaCaptureFold.
Import ListNotations.
Local Open Scope list_scope.
Lemma lambda_frame_names:forall fuel c e ps rt body bound sg this what s pid pre sm final,
 lambda_provider fuel c e ps rt body bound sg this what s=Good((pid,pre,sm),final)->
 exists le lets captureStart beforePid afterPid,
 mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)captureStart=
  Good((le,pre,lets),beforePid)/\
 new_sym(cx_mod c)(gs_name bound)KFn
 (ADFn RPlain(gs_name bound)[]None(Some(ABlock[](Some body))))OLambda beforePid=Good(pid,afterPid)/\
 NoDup(map fst(lambda_own_slots ps this)++map capture_name lets).
Proof.
 intros[|f]c e ps rt body bound sg this what s pid pre sm final Run.
 - discriminate Run.
 - cbn[lambda_provider]in Run.
   destruct(negb(Nat.eqb(List.length ps)(List.length(sg_params sg))));[exfalso;eapply err_not_good;exact Run|].
   fold(lambda_parameter_duplicate(map fst ps))in Run.
   destruct(lambda_parameter_duplicate(map fst ps));[exfalso;eapply err_not_good;exact Run|].
   apply bind_inv in Run as(u1&sp&Params&Run).
   apply bind_inv in Run as(u2&captureStart&Return&Run).
   apply bind_inv in Run as([ [le pre0]lets]&beforePid&Capture&Run).
   change(mfold(lambda_capture_one c e)(with_locals e[],[],[])(lambda_captured c e ps body)captureStart=Good((le,pre0,lets),beforePid))in Capture.
   apply bind_inv in Run as(id&afterPid&New&Run).
   apply bind_inv in Run as(u3&checked&Names&Run);destruct u3.
   destruct(lambda_local_names_checked _ _ _ _ _ Names)as[Same Unique];subst checked.
   apply bind_inv in Run as(u4&registered&Register&Run).
   apply bind_inv in Run as(compiled&bodyFinal&Body&Run).
   apply bind_inv in Run as(u5&emitted&Emit&Run).
   apply bind_inv in Run as(u6&cached&Cache&Run).
   apply bind_inv in Run as(transfer&last&Transfer&Run);injection Run as <- <- <- <-.
   exists le,lets,captureStart,beforePid,afterPid;split;[exact Capture|];split;[exact New|exact Unique].
Qed.
