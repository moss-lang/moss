(* Actual functor outputs install the fixed readings of the completed output
   scope, including bracket overrides, rather than an ambient approximation. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrame BodySound
 BindFunctorCtx FunctorDecl BodyTy ProvideResultFixed ResultMentionsExact ResultMentionsNoDup.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition provide_functor_code(e:env)(g:gsym)(tm:list(nat*ety))(out:list key):M(env*list key):=
 let!stamp:=fresh in let!xs:=functor_fixed g in
 let e:=set_fun e(gs_id g)stamp in let!sa:=fresh in
 ret(set_app e(KyFun(gs_id g))(map(fun x=>(x,option_map(canon e)(nassoc x tm)))xs)sa,
 out++[KyFun(gs_id g)]).
Section Functor.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySound.W w R Fl).
Theorem provide_functor_context_read:forall c e F C g tm tmr out s e' keys s' n names args res,
 provide_functor_code e g tm out s=Good((e',keys),s')->
 Inv s->st_w s=w->nget(w_syms w)(gs_id g)=Some g->
 gs_decl g=ADFunctor n names args res None->Later w P R Fl s'->
 CxRel w R Fl c e F C->
 (forall k,A.nlook k tmr=option_map(fun y=>tr Fl(canon e y))(nassoc k tm))->
 (forall x,A.Fixed W(gs_id g)x->A.nlook x tmr<>None)->
 exists fx,
 A.FixedReadings W(gs_id g)tmr fx /\keys=out++[KyFun(gs_id g)] /\
 CxRel w R Fl c e' F(A.set_fun C(gs_id g)fx).
Proof.
 intros c e F C g tm tmr out s e' keys s' n names args res Run Hi Hw Hg D L HC M Av.
 unfold provide_functor_code in Run.
 apply bind_inv in Run as(stamp&ss&Fresh&Run).
 apply bind_inv in Run as(xs&sx&Fixed&Run).
 apply bind_inv in Run as(sa&sf&Fresh2&Run);injection Run as <- <- <-.
 destruct(ok_fresh _ _ _ Hi Fresh)as[Is Fs].
 assert(Of:ok(functor_fixed g))by(unfold functor_fixed;okt).
 destruct(Of _ _ _ Is Fixed)as[Ix Fx].
 destruct(ok_fresh _ _ _ Ix Fresh2)as[_ F2].
 pose proof(later_back w P R Fl _ _ F2 L)as Lx.
 assert(Ws:st_w ss=w)by(rewrite(fr_w _ _ Fs);exact Hw).
 pose proof(functor_fixed_exact w (P:=P) R Fl b Hload g n names args res ss xs sx Hg D Fixed Is Ws Lx)as Exact.
 pose proof(functor_fixed_nodup g ss xs sx Fixed)as Nd.
 set(fx:=output_fixed_values tmr xs).
 assert(Read:A.FixedReadings W(gs_id g)tmr fx).
 {unfold A.FixedReadings,fx;exact(output_fixed_values_exact W(gs_id g)tmr xs Exact Av). }
 exists fx;split;[exact Read|];split;[reflexivity|].
 eapply(cxrel_functor w R Fl);[exact HC| | | |].
 - exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),n,names,args,res.
   split;[exact(w_decl_rd w R Fl _ _ Hg)|exact D].
 - unfold fx;rewrite output_fixed_values_keys;exact Nd.
 - intros x tx Hx;exact(proj1((proj1(Read x tx))Hx)).
 - unfold fun_tr_inst;rewrite map_map.
   transitivity(map(fun x=>(x,A.nlook x tmr))xs).
   + apply map_ext;intro x;cbn[fst snd option_map].
     f_equal.
     rewrite M;destruct(nassoc x tm);reflexivity.
   + unfold fx,A.fixed_app;apply output_fixed_values_apps.
     intros x Hx;apply Av;apply Exact;exact Hx.
Qed.
End Functor.
