(* The actual D84 family classifier enforces receiver-determined polymorphism. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 FreeVisitorCore NestingTypesExec ResultMentionsExact FunctorContext FamilyShapeExec MethodLookup MethodLookupNone.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[FUEL reqs type_requirements free_type_syms].
Lemma family_checked_keys:forall ps keyed said,
 negb(existsb(fun t=>nmem t ps&&negb(nmem t keyed))said)=true->
 forall t,In t ps->In t said->In t keyed.
Proof.
 intros ps keyed said H t P S;apply negb_true_iff in H.
 assert(M:nmem t ps=true)by(apply nmem_in;exact P).
 destruct(nmem t keyed)eqn:K;[apply nmem_in;exact K|].
 assert(E:existsb(fun t=>nmem t ps&&negb(nmem t keyed))said=true).
 {apply existsb_exists;exists t;split;[exact S|rewrite M,K;reflexivity]. }
 congruence.
Qed.
Section Classifier.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Notation path_of mi:=(gm_path(get_mod w mi)).
Lemma family_loaded_poly:forall g n names args res t,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.Poly W(gs_id g)t<->match names with None=>A.Mentions W(gs_id g)t|Some ns=>exists name,In name ns/\A.resolve_sym W(path_of(gs_mod g))[name]=Some t end.
Proof.
 intros g n names args res t Hg D;split.
 - intros(d&n0&ps&a&r&Decl&Ast&Poly).
   change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
   rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
   cbn[A.d_ast A.d_mod]in Ast,Poly;rewrite D in Ast;injection Ast as <- <- <- <-;exact Poly.
 - intro Poly;exists(A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g)),n,names,args,res.
   split;[exact(w_decl_read w R Fl _ _ Hg)|];split;[exact D|exact Poly].
Qed.
Lemma family_names_members:forall p names keys,
 Forall2(fun name key=>A.resolve_sym W p[name]=Some key/\A.kind_sym W key=Some A.DKType)names keys->
 forall t,In t keys<->exists name,In name names/\A.resolve_sym W p[name]=Some t.
Proof.
 intros p names keys H;induction H as[|name key names keys[Hr Hk]H IH];intro t;cbn[In].
 - split;[intros[]|intros(n&[]&_)].
 - rewrite IH;split.
   + intros[<-|(n&Hin&Res)];[exists name;split;[left;reflexivity|exact Hr]|exists n;split;[right;exact Hin|exact Res]].
   + intros(n&[<-|Hin]&Res);[left;congruence|right;exists n;split;assumption].
Qed.
Lemma family_params_poly:forall g n names args res s ps sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 A.FamilyShape W(gs_id g)->family_params g s=Good(ps,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->forall t,In t ps<->A.Poly W(gs_id g)t.
Proof.
 intros g n names args res s ps sf Hg D Shape E Hi Hw L.
 unfold family_params in E;apply bind_inv in E as(opt&sp&Params&E).
 destruct names as[names|].
 - destruct opt as[keys|].
   2:{unfold functor_params,functor_brackets in Params;rewrite D in Params;
     apply bind_inv in Params as(ks&ss&Map&Bad);discriminate Bad. }
   destruct(functor_params_names w R Fl b Hload g n names args res None s keys sp Hg D Params Hi Hw)as[-> Names].
   injection E as <- <-. intro t;rewrite(family_loaded_poly g n(Some names)args res t Hg D).
   exact(family_names_members _ _ _ Names t).
 - unfold functor_params,functor_brackets in Params;rewrite D in Params;injection Params as <- <-.
   pose proof(result_mentions_exact w (P:=P)R Fl b Hload g n None args res None s ps sf Hg D E Hi Hw L)as M.
   assert(Out:forall t,In t([]:list nat)<->A.FunOutputs W(gs_id g)t).
   {intro t;split;[intros[]|intro H;exfalso;exact(family_shape_no_outputs W(gs_id g)Shape t H)]. }
   pose proof(mentions_from_lists w R Fl g n None args res ps[]Hg D M Out)as Ment.
   intro t;rewrite(family_loaded_poly g n None args res t Hg D),<-Ment;cbn[In];tauto.
Qed.
Definition family_rhs_type(q:path*spec):aty:=match snd q with Spec(Some pa)_ a=>ATRef pa a|_=>ATUnit end.
Lemma family_rhs_rel:forall mi bs out s found sf,
 mfold(fun o b0=>match snd b0 with Spec(Some sp)_ sa=>free_type_syms 200(ATRef sp sa)mi o|_=>ret o end)out bs s=Good(found,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 forall t,In t found<->In t out\/(exists q,In q bs/\A.FreeIn W R(path_of mi)(family_rhs_type q)t).
Proof.
 intros mi bs out s found sf E Hi Hw Hmi L.
 apply(free_fold w (P:=P) R Fl _
 (fun q o=>match snd q with Spec(Some pa)_ a=>free_type_syms 200(ATRef pa a)mi o|_=>ret o end)
 family_rhs_type mi bs)with(s:=s)(s':=sf);try assumption.
 - intros[p[pa d a]]o;destruct pa;[apply ok_free_type_syms|apply ok_ret].
 - intros[p[pa d a]]_ o si ys ss E0 I0 W0 M0 L0;destruct pa as[pa|];cbn[family_rhs_type]in *.
   + exact(free_type_syms_rel w R Fl b Hload 200(ATRef pa a)mi o si ys ss E0 I0 W0 M0 L0).
   + injection E0 as <- <-;intro t;split;[intro T;left;exact T|intros[T|T];[exact T|inversion T]].
Qed.
Lemma family_signature_rel:forall m rcv nm ps rt body out s params sp said sf,
 nget(w_syms w)(gs_id m)=Some m->gs_decl m=ADFn rcv nm ps rt body->
 mfold(fun o q=>free_type_syms 200(snd q)(gs_mod m)o)out(fn_params m)s=Good(params,sp)->
 (match fn_ret m with Some t=>free_type_syms 200 t(gs_mod m)params|None=>ret params end)sp=Good(said,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->
 forall t,In t said<->In t out\/A.SigMentions W(gs_id m)t.
Proof.
 intros m rcv nm ps rt body out s params sp said sf Hg D Params Ret Hi Hw L.
 unfold fn_params in Params;rewrite D in Params;cbn beta iota in Params.
 unfold fn_ret in Ret;rewrite D in Ret;cbn beta iota in Ret.
 destruct(ok_mfold _ _ _ ps out(fun o q=>ok_free_type_syms 200(snd q)(gs_mod m)o)_ _ _ Hi Params)as[Ip Fp].
 assert(Wp:st_w sp=w)by(rewrite(fr_w _ _ Fp);exact Hw).
 assert(Or:ok(match rt with Some t=>free_type_syms 200 t(gs_mod m)params|None=>ret params end)).
 {destruct rt;[apply ok_free_type_syms|apply ok_ret]. }
 destruct(Or _ _ _ Ip Ret)as[_ Fr].
 pose proof(free_fold w (P:=P) R Fl _ (fun q o=>free_type_syms 200(snd q)(gs_mod m)o)snd(gs_mod m)ps
 (fun q o=>ok_free_type_syms 200(snd q)(gs_mod m)o)(fun q _=>free_type_syms_rel w R Fl b Hload 200(snd q)(gs_mod m))
 out s params sp Params Hi Hw(loader_mod_in w R Fl b Hload _ _ Hg)(later_back w P R Fl _ _ Fr L))as Members.
 intro t;rewrite(signature_mentions_fn w R Fl m rcv nm ps rt body t Hg D).
 destruct rt as[ty|].
 - pose proof(free_type_syms_rel w R Fl b Hload 200 ty(gs_mod m)params sp said sf Ret Ip Wp
 (loader_mod_in w R Fl b Hload _ _ Hg)L t)as Rty.
   rewrite Rty,Members;split.
   + intros[[Old|Param]|Return];[left;exact Old|right;left;exact Param|right;right;exists ty;split;[reflexivity|exact Return]].
   + intros[Old|[Param|(ty'&Eq&Return)]];[left;left;exact Old|left;right;exact Param|].
     injection Eq as <-;right;exact Return.
 - injection Ret as <- <-;rewrite Members;split.
   + intros[Old|Param];[left;exact Old|right;left;exact Param].
   + intros[Old|[Param|(ty&Bad&Return)]];[left;exact Old|right;exact Param|discriminate Bad].
Qed.
Lemma family_method_keyed:forall u f mi ps pa dn a s sf,
 family_leaf_step f mi ps(Spec(Some pa)(Some dn)a)s=Good(true,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 (forall t,A.Poly W u t->In t ps)->A.FamilyKeyed W u(path_of mi)(Spec(Some pa)(Some dn)a).
Proof.
 intros u f mi ps pa dn a s sf E Hi Hw Hmi L Cover.
 unfold family_leaf_step in E;apply bind_inv in E as(ro&ss&Resolve&E).
 destruct(esym_rel w R Fl b Hload mi pa s ro ss Hi Hw Hmi Resolve)as[-> Rel].
 destruct ro as[recv|];[|discriminate E];destruct Rel as[Res Hr].
 apply bind_inv in E as(mo&sm&Find&E);destruct mo as[m|];[|discriminate E].
 destruct(find_method_decl_named b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Find)as[Same Named].
 destruct(MethodLookupNone.find_method_decl_loaded b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Find)
 as[-> [Hm(rcv&nm&params&rt&body&Dm)]].
 apply bind_inv in E as(keyed&sk&Key&E).
 destruct(ok_free_type_syms 200(ATRef pa None)mi[] _ _ _ Hi Key)as[Ik Fk].
 assert(Wk:st_w sk=w)by(rewrite(fr_w _ _ Fk);exact Hw).
 apply bind_inv in E as(bl&sb&Bound&E).
 destruct(signature_bound w R Fl b Hload mi(opt_default[]a)sk bl sb Bound Ik Wk Hmi)as[-> BoundRel].
 apply bind_inv in E as(rhs&sr&Rhs&E).
 assert(Orhs:ok(mfold(fun o b0=>match snd b0 with Spec(Some sp)_ sa=>free_type_syms 200(ATRef sp sa)mi o|_=>ret o end)[](opt_default[]a)))by okt.
 destruct(Orhs _ _ _ Ik Rhs)as[Ir Fr].
 assert(Wr:st_w sr=w)by(rewrite(fr_w _ _ Fr);exact Wk).
 apply bind_inv in E as(sigparams&sp&Params&E).
 destruct(ok_mfold _ _ _ (fn_params m)rhs(fun o q=>ok_free_type_syms 200(snd q)(gs_mod m)o)
 _ _ _ Ir Params)as[Ip Fp].
 apply bind_inv in E as(said&st&Return&E).
 assert(Oret:ok(match fn_ret m with Some t=>free_type_syms 200 t(gs_mod m)sigparams|None=>ret sigparams end)).
 {destruct(fn_ret m);[apply ok_free_type_syms|apply ok_ret]. }
 destruct(Oret _ _ _ Ip Return)as[_ Ft].
 injection E as Check <-.
 pose proof(later_back w P R Fl _ _ Ft L)as Lp.
 pose proof(later_back w P R Fl _ _ Fp Lp)as Lr.
 pose proof(later_back w P R Fl _ _ Fr Lr)as Lk.
 pose proof(free_type_syms_rel w R Fl b Hload 200(ATRef pa None)mi[]s keyed sk Key Hi Hw Hmi Lk)as KeyRel.
 pose proof(family_rhs_rel mi(opt_default[]a)[]sk rhs sr Rhs Ik Wk Hmi Lr)as RhsRel.
 pose proof(family_signature_rel m rcv nm params rt body rhs sr sigparams sp said st Hm Dm Params Return Ir Wr L)as SigRel.
 eapply A.FKMethod with(r:=gs_id recv)(m:=gs_id m);[exact Res|exact Named|].
 intros t Poly Said NoBound.
 assert(Nb:~In t(List.concat bl)).
 {intro Ht;apply BoundRel in Ht as(b0&Hb&ResolveB);apply NoBound;exists b0;split;assumption. }
 assert(Raw:In t said).
 {apply SigRel;destruct Said as[Sig|RhsFree];[right;exact Sig|left].
  apply RhsRel;right;destruct RhsFree as(bp&pa0&dn0&bs&Hin&Free).
  exists(bp,Spec(Some pa0)dn0 bs);split;[exact Hin|exact Free]. }
 assert(Mb:nmem t(List.concat bl)=false).
 {destruct(nmem t(List.concat bl))eqn:Mb;[exfalso;apply Nb;apply nmem_in;exact Mb|reflexivity]. }
 assert(Filtered:In t(filter(fun t=>negb(nmem t(List.concat bl)))said++filter(fun t=>nmem t(List.concat bl)&&nmem t keyed)said)).
 {apply in_app_iff;left;apply filter_In;split;[exact Raw|rewrite Mb;reflexivity]. }
 pose proof(family_checked_keys ps keyed _ Check t(Cover t Poly)Filtered)as Has.
 apply KeyRel in Has as[[]|Free];exact Free.
Qed.
Lemma family_true_fold:forall X(step:X->M bool)(Q:X->Prop),
 (forall x,ok(step x))->
 (forall x si sf,step x si=Good(true,sf)->Inv si->st_w si=w->Later w P R Fl sf->Q x)->
 forall xs s sf,mfold(fun yes x=>if negb yes then ret false else step x)true xs s=Good(true,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->Forall Q xs.
Proof.
 intros X step Q Ok Sound xs;induction xs as[|x xs IH];intros s sf E Hi Hw L;[constructor|].
 cbn[mfold]in E;apply bind_inv in E as(yes&sm&First&Tail);cbn[negb]in First.
 destruct yes;[|rewrite false_family_fold in Tail;discriminate Tail].
 destruct(Ok x _ _ _ Hi First)as[Im Fm].
 assert(Otail:ok(mfold(fun yes x=>if negb yes then ret false else step x)true xs)).
 {apply ok_mfold;intros yes y;destruct yes;cbn[negb];[apply Ok|apply ok_ret]. }
 destruct(Otail _ _ _ Im Tail)as[_ Ft].
 constructor.
 - exact(Sound x s sm First Hi Hw(later_back w P R Fl _ _ Ft L)).
 - exact(IH sm sf Tail Im(fr_w' w _ _ Fm Hw)L).
Qed.
Theorem family_leaves_keyed:forall fuel u mi ps specs s sf,
 family_leaves_ok fuel mi ps specs s=Good(true,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 (forall t,A.Poly W u t->In t ps)->Forall(A.FamilyKeyed W u(path_of mi))specs.
Proof.
 induction fuel as[|f IH];intros u mi ps specs s sf E Hi Hw Hmi L Cover;[discriminate E|].
 change(mfold(fun yes sp=>if negb yes then ret false else family_leaf_step f mi ps sp)true specs s=Good(true,sf))in E.
 eapply family_true_fold with(step:=family_leaf_step f mi ps);[apply ok_family_leaf_step| |exact E|exact Hi|exact Hw|exact L].
 intros[pa dn a]si ss H Ii Wi Li;destruct pa as[pa|];destruct dn as[dn|].
 - eapply family_method_keyed;eassumption.
 - unfold family_leaf_step in H;apply bind_inv in H as(go&sr&Resolve&H).
   destruct(esym_rel w R Fl b Hload mi pa si go sr Ii Wi Hmi Resolve)as[-> Rel].
   destruct go as[g|];[|discriminate H];destruct Rel as[Res Hg].
   destruct(gs_kind g);try discriminate H;destruct(gs_decl g)eqn:D;try discriminate H.
   destruct(Nat.eqb(List.length(ctx_specs l))(List.length l))eqn:Len;[|discriminate H].
   apply Nat.eqb_eq,ctx_specs_all in Len.
   eapply A.FKContext with(c:=gs_id g)(d:=A.Decl(path_of(gs_mod g))(gs_decl g)(gs_assumes g))(sps:=ctx_specs l).
   + exact Res.
   + exact(w_decl_read w R Fl _ _ Hg).
   + exact D.
   + exact Len.
   + eapply IH;[exact H|exact Ii|exact Wi|exact(loader_mod_in w R Fl b Hload _ _ Hg)|exact Li|exact Cover].
 - discriminate H.
 - discriminate H.
Qed.
Theorem is_family_sound:forall g n names args res s sf,
 nget(w_syms w)(gs_id g)=Some g->gs_decl g=ADFunctor n names args res None->
 is_family g s=Good(true,sf)->Inv s->st_w s=w->Later w P R Fl sf->A.Family W(gs_id g).
Proof.
 intros g n names args res s sf Hg D E Hi Hw L.
 pose proof(is_family_shape w R Fl b Hload g n names args res s sf Hg D E Hi Hw)as Shape.
 unfold is_family,functor_parts in E;rewrite D in E.
 destruct(negb(kind_eqb(gs_kind g)KFunctor))eqn:K;[discriminate E|].
 apply bind_inv in E as(ps&sp&Params&E).
 assert(Op:ok(family_params g))by(unfold family_params,functor_params;okt).
 destruct(Op _ _ _ Hi Params)as[Ip Fp].
 assert(Wp:st_w sp=w)by(rewrite(fr_w _ _ Fp);exact Hw).
 apply bind_inv in E as(f&ss&Fuel&Scan).
 assert(Of:ok walk_fuel)by okt;destruct(Of _ _ _ Ip Fuel)as[If Ff].
 destruct(ok_family_leaves_ok f(gs_mod g)ps res _ _ _ If Scan)as[_ Fs].
 pose proof(later_back w P R Fl _ _ Ff(later_back w P R Fl _ _ Fs L))as Lp.
 pose proof(family_params_poly g n names args res s ps sp Hg D Shape Params Hi Hw Lp)as Poly.
 pose proof(family_leaves_keyed f(gs_id g)(gs_mod g)ps res ss sf Scan If(fr_w' w _ _ Ff Wp)
 (loader_mod_in w R Fl b Hload _ _ Hg)L(fun t H=>proj2(Poly t)H))as Keys.
 split;[exact Shape|].
 intros d n0 ps0 args0 res0 Decl Ast.
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 cbn[A.d_ast A.d_mod]in Ast|-*;rewrite D in Ast;injection Ast as <- <- <- <-;exact Keys.
Qed.
Theorem is_family_loaded:forall g s sf,
 nget(w_syms w)(gs_id g)=Some g->is_family g s=Good(true,sf)->
 Inv s->st_w s=w->Later w P R Fl sf->A.Family W(gs_id g).
Proof.
 intros g s sf Hg E Hi Hw L.
 destruct(is_family_loaded_shape w R Fl b Hload g s sf Hg E Hi Hw)
 as(d&n&names&args&res&Decl&Ast&Shape).
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-;cbn[A.d_ast]in Ast.
 eapply is_family_sound;[exact Hg|exact Ast|exact E|exact Hi|exact Hw|exact L].
Qed.
End Classifier.
