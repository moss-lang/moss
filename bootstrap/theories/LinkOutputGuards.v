(* Successful post-body checks certify the actual output presence and readings. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ReifyData Audit NMap Sx Elab ElabSound BodySoundCore LinkBodyTrace NeedExec.
Import ListNotations.
Definition link_output_type_made(eg eb:env)(t:nat):bool:=
 match nassoc t(en_tn eb),nassoc t(en_tn eg)with
 |Some(_,s1),Some(_,s0)=>negb(Nat.eqb s1 s0)
 |Some _,None=>true|None,_=>false end.
Definition link_output_item_made(eg eb:env)(k:key):bool:=
 match get_app eb k,get_app eg k with
 |Some(_,s1),Some(_,s0)=>negb(Nat.eqb s1 s0)
 |Some _,None=>true
 |_,_=>match k with KyFun u=>is_some(nget(en_funs eb)u)|_=>false end end.
Lemma link_output_types_checked:forall fg bg eg eb rts s u final,
 link_trace_types_guard fg bg eg eb rts s=Good(u,final)->
 forall t,In t rts->link_output_type_made eg eb t=true.
Proof.
 intros fg bg eg eb rts s u final Run t Hit.
 destruct(miter_checked _ _ _ _ _ _ Run t Hit)as[sx[ux[sx' Step]]].
 change((if link_output_type_made eg eb t then ret tt else
 let! tg:=sym t in err(gs_mod bg)("`"++gs_name bg++"`, bound to `"++gs_name fg++"`, does not provide the type `"++gs_name tg++"` (D78)"))%string sx=Good(ux,sx'))in Step.
 destruct(link_output_type_made eg eb t)eqn:Made;[reflexivity|].
 apply bind_inv in Step as[g[sg[Sym Bad]]];exfalso;eapply err_not_good;exact Bad.
Qed.
Lemma link_output_items_checked:forall fg bg eg eb tys keys s u final,
 link_trace_items_guard fg bg eg eb tys keys s=Good(u,final)->
 forall k,In k keys->link_output_item_made eg eb(link_trace_body_key eb tys k)=true.
Proof.
 intros fg bg eg eb tys keys s u final Run k Hit.
 destruct(miter_checked _ _ _ _ _ _ Run k Hit)as[sx[ux[sx' Step]]].
 apply bind_inv in Step as[shown[ss[Show Step]]].
 change((if negb(link_output_item_made eg eb(link_trace_body_key eb tys k))
 then err(gs_mod bg)("`"++gs_name bg++"`, bound to `"++gs_name fg++"`, does not provide "++shown++" (D78)")
 else ret tt)%string ss=Good(ux,sx'))in Step.
 destruct(link_output_item_made eg eb(link_trace_body_key eb tys k))eqn:Made;[reflexivity|].
 exfalso;eapply err_not_good;exact Step.
Qed.
Lemma link_output_readings_checked:forall fg bg promised eb tys keys s u final,
 link_trace_readings_guard fg bg promised eb tys keys s=Good(u,final)->
 forall k,In k keys->
 (match k with KyFun _=>result_functor_fixed_ok eb promised tys(link_trace_body_key eb tys k)k=true|_=>True end)/\
 (match key_sig eb(link_trace_body_key eb tys k),key_sig promised k with
 |Some(p1,r1),Some(p2,r2)=>List.length p1=List.length p2 /\
 forallb(fun q=>ety_eqb(canon eb(fst q))(promised_result_type eb tys(snd q)))
   (combine(r1::p1)(r2::p2))=true
 |_,_=>True end).
Proof.
 intros fg bg promised eb tys keys s u final Run k Hit.
 destruct(miter_checked _ _ _ _ _ _ Run k Hit)as[sx[ux[sx' Step]]].
 apply bind_inv in Step as[unitv[sm[Fixed Step]]];split.
 - destruct k;try exact I.
   destruct(result_functor_fixed_ok eb promised tys(link_trace_body_key eb tys(KyFun n))(KyFun n))eqn:Check;[reflexivity|].
   apply bind_inv in Fixed as[shown[ss[Show Bad]]];exfalso;eapply err_not_good;exact Bad.
 - destruct(key_sig eb(link_trace_body_key eb tys k))as[[p1 r1]|]eqn:Body;[|exact I].
   destruct(key_sig promised k)as[[p2 r2]|]eqn:Promise;[|exact I].
   destruct(Nat.eqb(List.length p1)(List.length p2)&&
    forallb(fun q=>ety_eqb(canon eb(fst q))(promised_result_type eb tys(snd q)))
     (combine(r1::p1)(r2::p2)))eqn:Check.
   + apply andb_true_iff in Check as[Lengths Equal];split;[apply Nat.eqb_eq;exact Lengths|exact Equal].
   + apply bind_inv in Step as[shown[ss[Show Bad]]];exfalso;eapply err_not_good;exact Bad.
Qed.
Lemma link_output_types_forall2:forall eb tys xs ys,
 List.length xs=List.length ys->
 forallb(fun q=>ety_eqb(canon eb(fst q))(promised_result_type eb tys(snd q)))(combine xs ys)=true->
 Forall2(fun x y=>canon eb x=promised_result_type eb tys y)xs ys.
Proof.
 intros eb tys xs;induction xs as[|x xs IH];intros[|y ys]Lengths Equal;try discriminate;[constructor|].
 cbn[combine forallb fst snd]in Equal;apply andb_true_iff in Equal as[Head Tail].
 constructor;[apply ety_eqb_true;exact Head|apply IH;[injection Lengths;auto|exact Tail]].
Qed.
