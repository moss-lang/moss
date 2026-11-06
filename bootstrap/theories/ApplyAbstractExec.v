(* Explicit phases of the actual abstract-functor application. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx ReifyData Elab ElabSound ElabFrame BodySound BodyTr CtxMerge FunctorFixedExec.
Import ListNotations.
Local Open Scope list_scope.
Definition abstract_info(ef:env)(needs:list key):fninfo:=
 FInfo ef needs(map(fun k=>(k,match get_app ef k with Some(a,_)=>a|None=>[]end))needs)(merge_edges ef).
Definition output_generation(e:env)(fg:gsym)(t:nat):M(nat*nat):=
 match tn_get e t with
 |None=>ret(t,t)
 |Some _=>let! tgg:=sym t in
   let! gnew:=new_sym(gs_mod tgg)(gs_name tgg++"@"++gs_name fg)%string KType
     (ADType(gs_name tgg))(OFresh t(gs_id fg))in ret(t,gnew)
 end.
Definition input_argument(e:env)(t:nat):M(nat*ty):=
 let! y:=ety_ty(canon e(opt_default YUnit(tn_get e t)))in ret(t,y).
Definition output_arguments(gens:list(nat*nat)):M(list(nat*ty)):=
 mmap(fun p=>ret(fst p,TAbs(snd p)))(filter(fun p=>negb(Nat.eqb(fst p)(snd p)))gens).
Definition abstract_tail(c:cx)(e:env)(fg:gsym)(ef:env)(needs:list key)(pairs:list(key*key))
 :M(list xstmt*env*cx):=
 let argtys:=map fst(en_tn ef)in
 let! rts:=result_types fg in
 let! gens:=mmap(output_generation e fg)rts in
 let! th:=mmap(input_argument e)argtys in
 let! th2:=output_arguments gens in
 let! sm:=smap_at ef e pairs in
 let! (e',output_keys):=provide_result e fg gens in
 record_reify_event(REApply ety key(gs_id fg)needs output_keys
  (map(fun t=>(t,canon e(opt_default YUnit(tn_get e t))))argtys)
  (map(fun p=>(fst p,YAbs(snd p)))gens)(reify_scope_of e'));;
 ret([SApply(gs_id fg)(th++th2)sm],e',cx_with_fresh c(cx_fresh c++map snd gens)).
Lemma apply_abstract_phases:forall fuel c e fg s xs e' c' s',
 apply_abstract fuel c e fg s=Good((xs,e',c'),s')->
 exists ef needs sf xf sx sy sz pairs sp,
 is_some(nget(en_funs e)(gs_id fg))=true /\
 fun_env fg s=Good((ef,needs),sf) /\ functor_fixed fg sf=Good(xf,sx) /\
 miter(fixed_guard(cx_mod c)e fg(match get_app e(KyFun(gs_id fg))with Some(a,_)=>a|None=>[]end))xf sx=Good(tt,sy) /\
 miter(input_guard(cx_mod c)e fg)(map fst(en_tn ef))sy=Good(tt,sz) /\
 require_needs(cx_mod c)e fg(abstract_info ef needs)sz=Good(pairs,sp) /\
 abstract_tail c e fg ef needs pairs sp=Good((xs,e',c'),s').
Proof.
 intros[|fuel]c e fg s xs e' c' s' E;[discriminate E|].
 cbn[apply_abstract]in E.
 destruct(is_some(nget(en_funs e)(gs_id fg)))eqn:Av;[|discriminate E].
 cbn[negb]in E. apply bind_inv in E as([ef needs]&sf&Env&E).
 apply bind_inv in E as(xf&sx&Fix&E).
 apply bind_inv in E as([]&sy&Guard&E).
 apply bind_inv in E as([]&sz&Input&E).
 apply bind_inv in E as(pairs&sp&Req&E).
 exists ef,needs,sf,xf,sx,sy,sz,pairs,sp.
 repeat split;try exact Av;try exact Env;try exact Fix;try exact Guard;try exact Input;try exact Req.
 exact E.
Qed.

Lemma abstract_tail_phases:forall c e fg ef needs pairs s xs e' c' s',
 abstract_tail c e fg ef needs pairs s=Good((xs,e',c'),s')->
 exists rts sr gens sg th st th2 st2 sm ss output_keys so,
 result_types fg s=Good(rts,sr) /\
 mmap(output_generation e fg)rts sr=Good(gens,sg) /\
 mmap(input_argument e)(map fst(en_tn ef))sg=Good(th,st) /\
 output_arguments gens st=Good(th2,st2) /\ smap_at ef e pairs st2=Good(sm,ss) /\
 provide_result e fg gens ss=Good((e',output_keys),so) /\
 record_reify_event(REApply ety key(gs_id fg)needs output_keys
  (map(fun t=>(t,canon e(opt_default YUnit(tn_get e t))))(map fst(en_tn ef)))
  (map(fun p=>(fst p,YAbs(snd p)))gens)(reify_scope_of e'))so=Good(tt,s') /\
 xs=[SApply(gs_id fg)(th++th2)sm] /\ c'=cx_with_fresh c(cx_fresh c++map snd gens).
Proof.
 intros c e fg ef needs pairs s xs e' c' s' E;unfold abstract_tail in E.
 apply bind_inv in E as(rts&sr&R&E).
 apply bind_inv in E as(gens&sg&G&E).
 apply bind_inv in E as(th&st&T&E).
 apply bind_inv in E as(th2&st2&T2&E).
 apply bind_inv in E as(sm&ss&S&E).
 apply bind_inv in E as([eo output_keys]&so&O&E).
 apply bind_inv in E as([]&se&Meta&E).
 injection E as <- <- <- <-.
 exists rts,sr,gens,sg,th,st,th2,st2,sm,ss,output_keys,so;repeat split;assumption.
Qed.
