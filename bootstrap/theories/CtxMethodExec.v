(* The actual method assumption step satisfies ItemEntries and its tables. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrame BodySound BodyTy
 Merge CanonTypes SourceClean ContextClean ContextWalkClean CtxWalk CtxMethod CtxMethodTable
 MethodLookup MethodClean MethodKind SigSound FreeTypesExec.
Import ListNotations.
Module A:=Audit.Elaboration.
Local Strategy opaque[Nat.of_num_uint FUEL reqs type_requirements].
Section Method.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySound.W w R Fl).
Lemma recvty_exec:forall mi recv tm tmr e s this s',
 (match nassoc(gs_id recv)tm with Some y=>ret y|None=>symbol_type 200 mi recv tm e end)s=Good(this,s')->
 Inv s->st_w s=w->nget(w_syms w)(gs_id recv)=Some recv->TmRel Fl tm tmr e->Later w P R Fl s'->
 A.RecvTy WW(gs_id recv)tmr(tr Fl this)/\WFty this.
Proof.
 intros mi recv tm tmr e s this s' H Hi Hw Hr TR L.
 destruct(nassoc(gs_id recv)tm)as[y|]eqn:Nt.
 - injection H as <- <-. split.
   + unfold A.RecvTy. rewrite(proj1 TR(gs_id recv)),Nt;reflexivity.
   + exact(proj1(proj2 TR)_ _ Nt).
 - destruct(proj2(proj2(ty_all w R Fl b Hload 200))mi recv tm e s this s' tmr H Hi Hw Hr TR L)as[Wt Sem].
   split;[|exact Wt]. unfold A.RecvTy. rewrite(proj1 TR(gs_id recv)),Nt.
   rewrite(w_decl_rd w R Fl _ _ Hr). cbn[A.d_ast A.d_mod].
   pose proof(load_world_kind b w Hload _ _ Hr)as KO. unfold KindOK in KO.
   destruct(gs_kind recv)eqn:K;cbn in Sem;try contradiction.
   + rewrite(proj1 TR(gs_id recv)),Nt in Sem;discriminate Sem.
   + destruct Sem as(args&E&Fs). destruct(gs_decl recv);try(exists args;split;[exact E|exact Fs]).
     cbn[A.kind_of kconv]in KO;discriminate KO.
   + destruct Sem as(n&body&D&Et). rewrite D;exact Et.
   + destruct Sem as(args&E&Fs). destruct(gs_decl recv);try(exists args;split;[exact E|exact Fs]).
     cbn[A.kind_of kconv]in KO;discriminate KO.
Qed.
Ltac impossible H:=exfalso;solve[repeat(cbn beta iota zeta in H;
 first[exact(err_not_good _ _ _ _ _ _ H)|discriminate H|apply bind_inv in H as(?&?&?&H)])].
Ltac cstep H Hi Hw L a si E Ii Wi Li:=
 match type of H with mbind ?m ?k ?s=Good(?v,?sf)=>
 let O:=fresh "O" in assert(O:ok m)by okt;
 apply bind_inv in H as(a&si&E&H);
 let F:=fresh "F" in destruct(O _ _ _ Hi E)as[Ii F];
 pose proof(fr_w' w _ _ F Hw)as Wi;
 let Q:=fresh "Q" in assert(Q:ok(k a))by okt;
 let It:=fresh "It" in let Ft:=fresh "Ft" in destruct(Q _ _ _ Ii H)as[It Ft];
 pose proof(later_back w P R Fl _ _ Ft L)as Li;clear O Q It Ft end.
Theorem meth_step_holds:@MethStep w P R Fl.
Proof.
 intros e mi recv dn subst needs s e' needs' s' U tm les ob H Hi Hw Hmi Hr L W Hob Hvalid.
 assert(Hwf:Forall(fun q=>WFty(snd q))subst).
 { eapply Forall_impl;[|exact Hvalid]. intros q Hq;exact(ety_ok_wf _ Hq). }
 assert(Ts:TmClean subst)by(apply tmclean_of_forall;exact Hvalid).
 destruct(method_context_clean w R Fl b Hload e mi recv dn subst needs s e' needs' s' H Hi Hw Hmi Hr Ts
  (ws_clean _ _ _ _ _ _ W)(ws_tri _ _ _ _ _ _ W)L)as[Cout Tout].
 pose proof(tmrel_walk Fl U e tm les needs subst ob W Hob Hwf)as TR.
 assert(Tm:TmClean(subst++tymap e))by(apply tmclean_app;[exact Ts|apply context_clean_tymap;exact(ws_clean _ _ _ _ _ _ W)]).
 unfold add_method_item in H. apply bind_inv in H as(mo&s1&Efind&H).
 destruct mo as[m|];[|impossible H].
 pose proof(find_method_decl_named b w R Fl mi recv dn s m s1 Hload Hmi Hr Hi Hw Efind)as[Es Named].
 destruct(find_method_decl_loaded b w R Fl mi recv dn s m s1 Hload Hmi Hr Hi Hw Efind)as[-> Hm].
 pose proof(method_named_kind b w R Fl mi(gs_id recv)dn(gs_id m)Hload Hmi Named)as Km.
 destruct(loaded_method_fn w R Fl m Hm Km)as(rcv&n&ps&rt&body&Dm).
 cstep H Hi Hw L this s1 Ethis I1 W1 L1.
 destruct(recvty_exec mi recv(subst++tymap e)(ob++tm)e s this s1 Ethis Hi Hw Hr TR L1)as[Rthis Wthis].
 assert(Othis:ety_ok this=true).
 { destruct(nassoc(gs_id recv)(subst++tymap e))eqn:Nt.
   - injection Ethis as <- _. exact(Tm _ _ Nt).
   - exact(proj2(proj2(source_clean w R Fl b Hload 200))mi recv(subst++tymap e)e s this s1 Ethis Hi Hw Hr Tm
      (cc_types _(ws_clean _ _ _ _ _ _ W))L1). }
 cstep H I1 W1 L sg s2 Esg I2 W2 L2.
 destruct(fn_sig_rel_at w R Fl b Hload m(subst++tymap e)(ob++tm)e(Some this)s1 sg s2 rcv n ps rt body
  Esg I1 W1 Hm Dm TR Wthis L2)as[Sg Hthis].
 cstep H I2 W2 L k s3 Ek I3 W3 L3.
 assert(K:k=KyMth(canon e this)(gs_id m)).
 { unfold method_key in Ek. destruct(head(canon e this)),(gs_recv m);try(injection Ek as <- _;reflexivity). impossible Ek. }
 subst k. cstep H I3 W3 L ec s4 Eec I4 W4 L4.
 assert(C:(get_mth e(canon e this)(gs_id m)=None/\ec=set_mth e(canon e this)(gs_id m)sg)\/
  exists old mi0 nm si sf,get_mth e(canon e this)(gs_id m)=Some old/\unify_sig e mi0 old sg nm si=Good(ec,sf)).
 { destruct(get_mth e(canon e this)(gs_id m))as[old|]eqn:Old.
   - right;exists old,mi,(gs_name m),s3,s4;split;[first[exact Old|reflexivity]|exact Eec].
   - left;split;[first[exact Old|reflexivity]|]. injection Eec as <- _;reflexivity. }
 cstep H I4 W4 L a s5 Ea I5 W5 L5.
 pose proof(method_provision_app_rel w R Fl b Hload m rcv n ps rt body(subst++tymap e)(ob++tm)e s4 a s5
  Hm Dm Km Ea I4 W4 L5 TR)as App.
 apply bind_inv in H as(ee&s6&Eee&H). injection H as <- <- <-.
 exists(gs_id m),(tr Fl this),(tr_sig Fl sg),(tr_inst Fl a),[LMth this(gs_id m)sg a].
 split;[exact Named|]. split;[exact Km|]. split;[exact Rthis|]. split;[exact Sg|]. split;[exact App|]. split;[reflexivity|].
 exact(ws_mth Fl U e ec ee tm les needs this(gs_id m)sg a s5 s6 W Hthis Othis C Eee Cout).
Qed.
End Method.
