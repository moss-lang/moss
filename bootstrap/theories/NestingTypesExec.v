(* The actual item-type enumeration, including nested contexts. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ReqsSolve ElabFrameCore BodySoundCore BodyTy.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Theorem load_world_nesting_types:forall b w R Fl,
 load_world b=Good w->forall mi specs s ts s',
 st_w s=w->(forall x g,nget(w_syms w)x=Some g->nget(st_syms s)x=Some g)->In mi(w_order w)->
 nesting_types mi specs s=Good(ts,s')->
 s'=s/\NoDup ts/\forall t,In t ts<->exists sp,In sp specs/\A.SpecTypes(W_read w R Fl)(gm_path(get_mod w mi))sp t.
Proof.
 intros b w R Fl Hload mi specs s ts s' Hw Keep Hmi H.
 unfold load_world in Hload. apply rbind_good in Hload as([e st]&Hl&Hload). injection Hload as <-.
 destruct(load_spec b _ _ _ _ _ _ Hl(linv_init b))as(L&_).
 unfold nesting_types,walk_fuel in H. apply bind_inv in H as(n&s1&H1&H).
 apply bind_inv in H1 as(x&s2&E1&E2). injection E1 as <- <-. injection E2 as <- <-.
 apply bind_inv in H as([V' out']&s4&Hwalk&H). injection H as <- <-.
 destruct(walk_ok b st e L R Fl _ mi specs [] [] s V' out' s4(conj Hw Keep)Hmi(NoDup_nil _)Hwalk)
  as(->&Nd&_&_&Sound&Directs&Contexts&Finished).
 split;[reflexivity|]. split;[exact Nd|]. intro t. split.
 - intro Ht. destruct(Sound t Ht)as[[]|(sp&Hin&Hs)]. exists sp;split;assumption.
 - intros(sp&Hin&Hs).
   exact(spec_complete b st e R Fl V' out'(fun c Hc=>Finished c Hc(fun E=>E))_ _ t Hs
    (fun t0=>Directs _ t0 Hin)(fun c0=>Contexts _ c0 Hin)).
Qed.
Section Outputs.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem result_types_rel:forall g n ps args res body s xs s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n ps args res body->
 result_types g s=Good(xs,s')->Inv s->st_w s=w->
 s'=s/\NoDup xs/\forall t,In t xs<->A.FunOutputs WW(gs_id g)t.
Proof.
 intros g n ps args res body s xs s' Hg D H Hi Hw.
 unfold result_types,Elab.functor_parts in H. rewrite D in H. cbn beta iota in H.
 destruct(load_world_nesting_types b w R Fl Hload(gs_mod g)res s xs s' Hw
  (fun x g0 G=>iv_loader _ Hi b ltac:(rewrite Hw;exact Hload)x g0 ltac:(rewrite Hw;exact G))
  (loader_mod_in w R Fl b Hload _ _ Hg)H)as[-> [Nd Types]].
 split;[reflexivity|]. split;[exact Nd|]. intro t. rewrite Types. unfold A.FunOutputs. split.
 - intros(sp&Hin&Hs). exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),ps,args,res,body,sp.
   split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split;[cbn[A.functor_parts A.d_ast];rewrite D;reflexivity|].
   split;assumption.
 - intros(d&ps'&args'&res'&body'&sp&Hd&Hp&Hin&Hs).
   rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
   cbn[A.d_ast A.functor_parts]in Hp. rewrite D in Hp. injection Hp as <- <- <- <-.
   exists sp;split;assumption.
Qed.
End Outputs.

Section Parameters.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation WW:=(BodySoundCore.W w R Fl).
Theorem functor_params_names:forall g n names args res body s keys s',
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n(Some names)args res body->
 functor_params g s=Good(Some keys,s')->Inv s->st_w s=w->
 s'=s/\Forall2(fun name key=>A.resolve_sym WW(gm_path(get_mod w(gs_mod g)))[name]=Some key/\
  A.kind_sym WW key=Some A.DKType)names keys.
Proof.
 intros g n names args res body s keys s' Hg D H Hi Hw.
 unfold functor_params,functor_brackets in H. rewrite D in H.
 apply bind_inv in H as(keys0&s1&Hmap&Hret). injection Hret as <- <-.
 assert(Hmi:In(gs_mod g)(w_order w))by exact(loader_mod_in w R Fl b Hload _ _ Hg).
 clear Hg D. revert s keys0 s1 Hmap Hi Hw.
 induction names as[|name names IH];intros s keys s' H Hi Hw;cbn[mmap]in H.
 - injection H as <- <-. split;[reflexivity|constructor].
 - apply bind_inv in H as(key&s1&Ekey&H). apply bind_inv in Ekey as(tg&s2&Eres&Ekey).
   destruct(esym_rel w R Fl b Hload(gs_mod g)[name]s tg s2 Hi Hw Hmi Eres)as[-> Sem].
   destruct tg as[tg|];[|exfalso;exact(err_not_good _ _ _ _ _ _ Ekey)].
   destruct(kind_eqb(gs_kind tg)KType)eqn:Kt;[|exfalso;exact(err_not_good _ _ _ _ _ _ Ekey)].
   injection Ekey as <- <-.
   apply bind_inv in H as(ks&s3&Htail&H). destruct(IH s ks s3 Htail Hi Hw)as[-> Rel].
   injection H as <- <-. split;[reflexivity|]. constructor;[|exact Rel].
   destruct Sem as[Rs Gt]. split;[exact Rs|]. apply kind_eqb_spec in Kt.
   rewrite(kind_sym_rd w R Fl b Hload _ _ Gt),Kt;reflexivity.
Qed.
End Parameters.

Section FixedDeclarations.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation WW:=(BodySoundCore.W w R Fl).
Lemma fixed_named_iff:forall g n names args res t,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n(Some names)args res None->
 A.Fixed WW(gs_id g)t<->
 ((exists sp,In sp res/\A.ResultMentions WW(gs_id g)(gm_path(get_mod w(gs_mod g)))sp t)/\
  ~A.FunOutputs WW(gs_id g)t/\
  ~(exists name,In name names/\A.resolve_sym WW(gm_path(get_mod w(gs_mod g)))[name]=Some t)).
Proof.
 intros g n names args res t Hg D. split.
 - intros[(d&ps&a&r&body&sp&Hd&Hp&Hin&Hm&Hout)Hpoly].
   rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
   cbn[A.d_ast A.functor_parts]in Hp. rewrite D in Hp. injection Hp as <- <- <- <-.
   split;[exists sp;split;assumption|]. split;[exact Hout|].
   intros Hex. apply Hpoly. unfold A.Poly. exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)). exists n. exists(Some names). exists args. exists res.
   split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split;[exact D|exact Hex].
 - intros[(sp&Hin&Hm)[Hout Hnames]]. split.
   + unfold A.Mentions. exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)). exists(Some names). exists args. exists res. exists None. exists sp.
     split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split;[cbn[A.d_ast A.functor_parts];rewrite D;reflexivity|].
     split;[exact Hin|split;assumption].
   + intros(d&n'&ps'&a'&r'&Hd&Hp&Hpoly).
     rewrite(w_decl_rd w R Fl _ _ Hg)in Hd;injection Hd as <-.
     cbn[A.d_ast]in Hp. rewrite D in Hp. injection Hp as <- <- <- <-.
     exact(Hnames Hpoly).
Qed.
Lemma fixed_unbracketed_false:forall g n args res t,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n None args res None->~A.Fixed WW(gs_id g)t.
Proof.
 intros g n args res t Hg D[Hm Hp]. apply Hp.
 exists(A.Decl(gm_path(get_mod w(gs_mod g)))(gs_decl g)(gs_assumes g)),n,None,args,res.
 split;[exact(w_decl_rd w R Fl _ _ Hg)|]. split;[exact D|exact Hm].
Qed.
End FixedDeclarations.
