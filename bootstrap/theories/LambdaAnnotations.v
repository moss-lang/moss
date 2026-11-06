(* Successful lambda annotation checks produce the actual LP premises. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound BodyTy BodyTr
 LambdaTypeRead CanonTypes.
Import ListNotations.
Local Open Scope string_scope.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL ety_of].
Definition lambda_parameter_check(c:cx)(e:env)(this:option ety)(what:string)
 (q:(name*option aty)*(name*ety)):M unit:=
 let '((pn,pt),(_,want)):=q in
 match pt with
 |Some t=>let! actual:=ety_of t(cx_mod c)(tymap e)e this in
   if ety_eqb(canon e actual)(canon e want)then ret tt else
   let! a:=show actual in let! b:=show want in
   err(cx_mod c)("parameter `"++pn++"` of the anonymous function bound to "++what++
   " is annotated `"++a++"`, but "++what++" declares `"++b++"` there (D70)")
 |None=>ret tt end.
Lemma ok_lambda_parameter_check:forall c e this what q,ok(lambda_parameter_check c e this what q).
Proof. intros c e this what[[pn[ty|]][n want]];unfold lambda_parameter_check;okt. all:apply ok_ety_of. Qed.
Section Checks.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Notation tc:=(fun e t=>tr Fl(canon e t)).
Definition ParameterAnnotation(F:A.frame)(C:A.cx)(e:env)(this:option ety)(pq:name*option aty)(want:ty):Prop:=
 match snd pq with
 |Some a=>exists actual,A.TyOf WW(A.fr_mod F)(A.c_ty C)(option_map(tc e)this)a actual/\A.canon C actual=A.canon C want
 |None=>True end.
Lemma parameter_annotation_checked:forall c e F C this what pn pt name want s u final,
 lambda_parameter_check c e this what((pn,pt),(name,want))s=Good(u,final)->
 Inv s->CxRel w R Fl c e F C->opt_wf this->Later w P R Fl final->
 ParameterAnnotation F C e this(pn,pt)(tc e want).
Proof.
 intros c e F C this what pn[ty|]name want s u final Run Hi Hc Wthis L;[|exact I].
 unfold lambda_parameter_check in Run;apply bind_inv in Run as[actual[mid[Read Run]]].
 destruct(ok_ety_of _ _ _ _ _ _ _ _ Hi Read)as[Im Frame].
 destruct(ety_eqb(canon e actual)(canon e want))eqn:Equal.
 - injection Run as <- <-;apply ety_eqb_true in Equal.
   destruct(lambda_annotation_read w (P:=P) R Fl b Hload c e F C this ty s actual mid Read Hi Hc Wthis L)as[Ty WF].
   unfold ParameterAnnotation;cbn[snd]. exists(tc e actual);split;[exact Ty|rewrite Equal;reflexivity].
 - apply bind_inv in Run as[a[s1[Show Run]]];apply bind_inv in Run as[b0[s2[Show2 Run]]].
   exfalso;exact(err_not_good _ _ _ _ _ _ Run).
Qed.
Theorem lambda_parameters_checked:forall c e F C this what ps wanted s u final,
 List.length ps=List.length wanted->
 miter(lambda_parameter_check c e this what)(combine ps wanted)s=Good(u,final)->
 Inv s->CxRel w R Fl c e F C->opt_wf this->Later w P R Fl final->
 Forall2(ParameterAnnotation F C e this)ps(map(fun p=>tc e(snd p))wanted).
Proof.
 intros c e F C this what ps;induction ps as[|[pn pt]ps IH];intros[|[name want]wanted]s u final Len Run Hi Hc Wthis L;
 cbn[List.length]in Len;try discriminate Len.
 - constructor.
 - cbn[combine miter]in Run;apply bind_inv in Run as[unitv[mid[Head Tail]]].
   destruct(ok_lambda_parameter_check c e this what((pn,pt),(name,want))_ _ _ Hi Head)as[Im Fm].
   assert(Otail:ok(miter(lambda_parameter_check c e this what)(combine ps wanted)))
   by(apply ok_miter;intro;apply ok_lambda_parameter_check).
   destruct(Otail _ _ _ Im Tail)as[Ifinal Ft].
   cbn[map snd];constructor.
   + exact(parameter_annotation_checked c e F C this what pn pt name want s unitv mid Head Hi Hc Wthis
     (later_back w P R Fl _ _ Ft L)).
   + eapply IH;[lia|exact Tail|exact Im|exact Hc|exact Wthis|exact L].
Qed.
Definition lambda_return_check(c:cx)(e:env)(this:option ety)(what:string)(rt:option aty)(want:ety):M unit:=
 match rt with
 |Some t=>let! actual:=ety_of t(cx_mod c)(tymap e)e this in
   if ety_eqb(canon e actual)(canon e want)then ret tt else
   let! a:=show actual in let! b:=show want in
   err(cx_mod c)("the anonymous function bound to "++what++" is annotated to return `"++a++
   "`, but "++what++" returns `"++b++"` (D70)")
 |None=>ret tt end.
Theorem lambda_return_checked:forall c e F C this what rt want s u final,
 lambda_return_check c e this what rt want s=Good(u,final)->
 Inv s->CxRel w R Fl c e F C->opt_wf this->Later w P R Fl final->
 match rt with
 |Some a=>exists actual,A.TyOf WW(A.fr_mod F)(A.c_ty C)(option_map(tc e)this)a actual/\A.canon C actual=A.canon C(tc e want)
 |None=>True end.
Proof.
 intros c e F C this what[ty|]want s u final Run Hi Hc Wthis L;[|exact I].
 unfold lambda_return_check in Run;apply bind_inv in Run as[actual[mid[Read Run]]].
 destruct(ety_eqb(canon e actual)(canon e want))eqn:Equal.
 - injection Run as <- <-;apply ety_eqb_true in Equal.
   destruct(lambda_annotation_read w (P:=P) R Fl b Hload c e F C this ty s actual mid Read Hi Hc Wthis L)as[Ty WF].
   exists(tc e actual);split;[exact Ty|rewrite Equal;reflexivity].
 - apply bind_inv in Run as[a[s1[Show Run]]];apply bind_inv in Run as[b0[s2[Show2 Run]]].
   exfalso;exact(err_not_good _ _ _ _ _ _ Run).
Qed.
End Checks.
