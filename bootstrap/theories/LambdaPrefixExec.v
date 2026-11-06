(* The successful lambda-provider prefix proves its annotation premises. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy BodyTr
 LambdaAnnotations ForwarderLowerCode.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL ety_of].
Section Prefix.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Notation tc:=(fun e t=>tr Fl(canon e t)).
Theorem lambda_annotation_prefix:forall fuel c e F C ps rt body bound sg this what s pid pre sm final,
 lambda_provider fuel c e ps rt body bound sg this what s=Good((pid,pre,sm),final)->
 Inv s->CxRel w R Fl c e F C->opt_wf this->Later w P R Fl final->
 List.length ps=List.length(sg_params sg)/\NoDup(map fst ps)/\
 Forall2(ParameterAnnotation w R Fl F C e this)ps(map(fun p=>tc e(snd p))(sg_params sg))/\
 match rt with
 |Some a=>exists actual,A.TyOf WW(A.fr_mod F)(A.c_ty C)(option_map(tc e)this)a actual/\A.canon C actual=A.canon C(tc e(sg_ret sg))
 |None=>True end.
Proof.
 intros[|fuel]c e F C ps rt body bound sg this what s pid pre sm final Run Hi Hc Wthis L;[discriminate|].
 cbn[lambda_provider]in Run.
 destruct(negb(Nat.eqb(List.length ps)(List.length(sg_params sg))))eqn:Len.
 - exfalso;exact(err_not_good _ _ _ _ _ _ Run).
 - apply negb_false_iff in Len;apply Nat.eqb_eq in Len.
   fold(parameter_duplicate(map fst ps))in Run.
   destruct(parameter_duplicate(map fst ps))as[duplicate|]eqn:Dup.
   + exfalso;exact(err_not_good _ _ _ _ _ _ Run).
   + apply bind_inv in Run as[u[s1[Params Run]]].
     change(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))s=Good(u,s1))in Params.
     assert(Op:ok(miter(lambda_parameter_check c e this what)(combine ps(sg_params sg))))
     by(apply ok_miter;intro;apply ok_lambda_parameter_check).
     destruct(Op _ _ _ Hi Params)as[I1 F1].
     apply bind_inv in Run as[u2[s2[Return Tail]]].
     change(lambda_return_check c e this what rt(sg_ret sg)s1=Good(u2,s2))in Return.
     assert(Or:ok(lambda_return_check c e this what rt(sg_ret sg))).
     {unfold lambda_return_check;destruct rt;okt;apply ok_ety_of. }
     destruct(Or _ _ _ I1 Return)as[I2 F2].
     match type of Tail with ?m _ = _=>assert(Otail:ok m)by okt end.
     destruct(Otail _ _ _ I2 Tail)as[Ifinal Ft].
     split;[exact Len|]. split;[exact(parameter_duplicate_none _ Dup)|]. split.
     * exact(lambda_parameters_checked w (P:=P) R Fl b Hload c e F C this what ps(sg_params sg)s u s1 Len Params Hi Hc Wthis
       (later_back w P R Fl _ _ (fr_trans _ _ _ F2 Ft)L)).
     * exact(lambda_return_checked w (P:=P) R Fl b Hload c e F C this what rt(sg_ret sg)s1 u2 s2 Return I1 Hc Wthis
       (later_back w P R Fl _ _ Ft L)).
Qed.
End Prefix.
