(* Reverse correctness of successful family leaf classification. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound WorldSound ElabFrame BodySoundCore BodyTr BodyTy
 FreeVisitorCore NestingTypesExec FamilyShapeExec FamilyKeysExec MethodLookup MethodLookupNone ContextCheckExec.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Local Strategy opaque[FUEL reqs type_requirements free_type_syms].
Section Reverse.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Notation path_of mi:=(gm_path(get_mod w mi)).
Lemma family_method_classifies:forall u f mi ps pa dn a s answer sf,
 family_leaf_step f mi ps(Spec(Some pa)(Some dn)a)s=Good(answer,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 (forall t,In t ps->A.Poly W u t)->
 A.FamilyKeyed W u(path_of mi)(Spec(Some pa)(Some dn)a)->answer=true.
Proof.
 intros u f mi ps pa dn a s answer sf E Hi Hw Hmi L Poly FK.
 inversion FK as[p0 pa0 dn0 a0 r0 m0 Res0 Named0 Determines|];subst p0 pa0 dn0 a0.
 unfold family_leaf_step in E;apply bind_inv in E as(ro&ss&Resolve&E).
 destruct(esym_rel w R Fl b Hload mi pa s ro ss Hi Hw Hmi Resolve)as[-> Rel].
 destruct ro as[recv|].
 2:{cbn in Rel;congruence. }
 destruct Rel as[Res Hr].
 assert(Rid:r0=gs_id recv)by congruence;subst r0.
 apply bind_inv in E as(mo&sm&Find&E);destruct mo as[m|].
 2:{destruct(find_method_decl_none b w R Fl mi recv dn s sm Hload Hmi Hr Hw Find)as[Same NoneM];
    exfalso;exact(NoneM m0 Named0). }
 destruct(find_method_decl_named b w R Fl mi recv dn s m sm Hload Hmi Hr Hi Hw Find)as[Same Named].
 assert(Mid:m0=gs_id m)by(eapply method_named_unique;[exact Named0|exact Named]);subst m0.
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
 pose proof(family_rhs_rel w R Fl b Hload mi(opt_default[]a)[]sk rhs sr Rhs Ik Wk Hmi Lr)as RhsRel.
 pose proof(family_signature_rel w R Fl b Hload m rcv nm params rt body rhs sr sigparams sp said st Hm Dm Params Return Ir Wr L)as SigRel.
 rewrite <-Check;apply negb_true_iff;apply Bool.not_true_is_false.
 intro Bad;apply existsb_exists in Bad as(t&Filtered&Bad).
 apply andb_true_iff in Bad as[Pt Miss];apply negb_true_iff in Miss.
 assert(NotKey:~In t keyed).
 {intro InK;assert(nmem t keyed=true)by(apply nmem_in;exact InK);congruence. }
 apply in_app_iff in Filtered as[Unbound|BoundKey].
 - apply filter_In in Unbound as[Raw Unbound];apply negb_true_iff in Unbound.
   assert(Nb:~A.FamilyBracketBound W(path_of mi)a t).
   {intros(b0&Hb&ResolveB);assert(Hbt:In t(List.concat bl)).
    {apply BoundRel;exists b0;split;assumption. }
    assert(nmem t(List.concat bl)=true)by(apply nmem_in;exact Hbt);congruence. }
   assert(Said:A.SigMentions W(gs_id m)t\/A.FamilyBracketFree W(path_of mi)a t).
   {apply SigRel in Raw as[Old|Sig];[right|left;exact Sig].
    apply RhsRel in Old as[Empty|(q&Hq&Free)];[contradiction|].
    destruct q as[bp [p dn0 bs]];destruct p as[pp|];cbn[family_rhs_type snd]in Free.
    - exists bp,pp,dn0,bs;split;assumption.
    - inversion Free. }
   assert(Free:A.FreeIn W R(path_of mi)(ATRef pa None)t).
   {apply Determines;[apply Poly;apply nmem_in;exact Pt|exact Said|exact Nb]. }
   apply NotKey;apply KeyRel;right;exact Free.
 - apply filter_In in BoundKey as[Raw BoundKey];apply andb_true_iff in BoundKey as[_ Keyed].
   apply NotKey;apply nmem_in;exact Keyed.
Qed.
Lemma family_bool_fold_classifies:forall X(step:X->M bool)(Q:X->Prop),
 (forall x,ok(step x))->
 (forall x si answer sf,step x si=Good(answer,sf)->Inv si->st_w si=w->Later w P R Fl sf->Q x->answer=true)->
 forall xs si answer sf,
 mfold(fun ok x=>if negb ok then ret false else step x)true xs si=Good(answer,sf)->
 Inv si->st_w si=w->Later w P R Fl sf->Forall Q xs->answer=true.
Proof.
 intros X step Q Ok Leaf xs;induction xs as[|x xs IH];intros si answer sf E Hi Hw L Hq.
 - injection E as <- <-;reflexivity.
 - apply Forall_cons_iff in Hq as[Qx Qxs].
   cbn[mfold negb]in E;apply bind_inv in E as(first&sm&Head&Tail).
   destruct(Ok x _ _ _ Hi Head)as[Im Fm].
   assert(Otail:ok(mfold(fun ok x=>if negb ok then ret false else step x)first xs)).
   {apply ok_mfold;intros v z;destruct v;cbn[negb];[apply Ok|apply ok_ret]. }
   destruct(Otail _ _ _ Im Tail)as[_ Ft].
   pose proof(Leaf x si first sm Head Hi Hw(later_back w P R Fl _ _ Ft L)Qx)as ->.
   eapply IH;[exact Tail|exact Im|rewrite(fr_w _ _ Fm);exact Hw|exact L|exact Qxs].
Qed.
Definition FamilyContextBound(fuel:nat)(stack:list nat)(mi:nat)(specs:list spec):Prop:=
 forall pa a h,In(Spec(Some pa)None a)specs->
 resolve_path w mi pa=Good(Some(ESym(gs_id h)))->
 nget(w_syms w)(gs_id h)=Some h->gs_kind h=KCtx->
 context_acyclic fuel w stack h=Good tt.
Lemma loaded_family_id_bound:forall h,
 nget(w_syms w)(gs_id h)=Some h->gs_id h<w_nsyms w.
Proof.
 intros h Hg;destruct(load_world_syms b w Hload)as[_ Above].
 destruct(Nat.lt_ge_cases(gs_id h)(w_nsyms w));[assumption|].
 rewrite(Above _ H)in Hg;discriminate Hg.
Qed.
Theorem family_leaves_classifies:forall fuel mi ps specs stack s answer sf u,
 family_leaves_ok fuel mi ps specs s=Good(answer,sf)->
 Inv s->st_w s=w->In mi(w_order w)->Later w P R Fl sf->
 (forall t,In t ps->A.Poly W u t)->Forall(A.FamilyKeyed W u(path_of mi))specs->
 FamilyContextBound fuel stack mi specs->NoDup stack->
 (forall x,In x stack->x<w_nsyms w)->fuel+List.length stack=S(w_nsyms w)->answer=true.
Proof.
 induction fuel as[|fuel IH];intros mi ps specs stack s answer sf u E Hi Hw Hmi L Poly Keyed Covered Nd Bound Count.
 - assert(Short:List.length stack<=w_nsyms w).
   {rewrite <-(length_seq(w_nsyms w)0);apply NoDup_incl_length;[exact Nd|].
    intros x Hx;apply in_seq;pose proof(Bound x Hx);lia. }
   cbn in Count;lia.
 - change(mfold(fun ok sp=>if negb ok then ret false else family_leaf_step fuel mi ps sp)true specs s=Good(answer,sf))in E.
   eapply family_bool_fold_classifies with
    (Q:=fun sp=>A.FamilyKeyed W u(path_of mi)sp/\
      forall pa a h,sp=Spec(Some pa)None a->
      resolve_path w mi pa=Good(Some(ESym(gs_id h)))->
      nget(w_syms w)(gs_id h)=Some h->gs_kind h=KCtx->
      context_acyclic(S fuel)w stack h=Good tt).
   + apply ok_family_leaf_step.
   + intros[pa dn a]si ans ss Run Ii Wi Li[FK One].
     destruct pa as[pa|],dn as[dn|];try solve[inversion FK].
     * eapply family_method_classifies;[exact Run|exact Ii|exact Wi|exact Hmi|exact Li|exact Poly|exact FK].
     * inversion FK as[|p0 pa0 a0 ctx d name its sps Resolve0 Decl Ast Esps Key];subst p0 pa0 a0.
       unfold family_leaf_step in Run;apply bind_inv in Run as(go&sr&Resolve&Run).
       destruct(esym_rel w R Fl b Hload mi pa si go sr Ii Wi Hmi Resolve)as[-> Rel].
       destruct go as[h|];[|cbn in Rel;congruence].
       destruct Rel as[Res Hg].
       assert(Id:ctx=gs_id h)by congruence;subst ctx.
       change(A.w_decl(W_read w R Fl)(gs_id h)=Some d)in Decl.
       rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
       change(gs_decl h=ADCtx name its)in Ast.
       pose proof(kind_sym_read b w R Fl Hload _ _ Hg)as Kind.
       unfold A.kind_sym in Kind;rewrite(w_decl_read w R Fl _ _ Hg)in Kind;cbn[A.d_ast]in Kind.
       rewrite Ast in Kind;cbn[A.kind_of]in Kind;injection Kind as Kind.
       assert(K:gs_kind h=KCtx)by(apply kconv_ctx;symmetry;exact Kind).
       rewrite K,Ast in Run. cbv beta iota in Run.
       rewrite Esps,ctx_specs_map,length_map,Nat.eqb_refl in Run.
       destruct(context_resolve_path mi pa si h si Ii Resolve)as[_ Path];rewrite Wi in Path.
       pose proof(One pa a h eq_refl Path Hg K)as Checked.
       pose proof(context_acyclic_specs fuel w stack h name its Checked K Ast)as Children.
       rewrite Esps,ctx_specs_map in Children.
       eapply IH with(stack:=gs_id h::stack)(u:=u);[exact Run|exact Ii|exact Wi|
        exact(loader_mod_in w R Fl b Hload _ _ Hg)|exact Li|exact Poly|exact Key| | | |].
       -- intros pp aa child Hin Path0 Hg0 K0;eapply context_check_specs_member;eauto.
       -- constructor;[eapply context_acyclic_notin;exact Checked|exact Nd].
       -- intros x[<-|Hx];[exact(loaded_family_id_bound h Hg)|exact(Bound x Hx)].
       -- cbn[List.length];lia.
   + exact E.
   + exact Hi.
   + exact Hw.
   + exact L.
   + apply Forall_forall;intros sp Hsp;split.
     * rewrite Forall_forall in Keyed;exact(Keyed sp Hsp).
     * intros pa a h Eq Path Hg K;subst sp;eapply Covered;eauto.
Qed.
Theorem is_family_classifies:forall g s answer sf sc,
 nget(w_syms w)(gs_id g)=Some g->is_family g s=Good(answer,sf)->
 check_contexts sc=Good(tt,sc)->st_w sc=w->
 Inv s->st_w s=w->Later w P R Fl sf->A.Family W(gs_id g)->answer=true.
Proof.
 intros g s answer sf sc Hg E Contexts Wsc Hi Hw L Fam.
 destruct(proj1 Fam)as(d&n&names&args&res&Decl&D&Shape).
 change(A.w_decl(W_read w R Fl)(gs_id g)=Some d)in Decl.
 rewrite(w_decl_read w R Fl _ _ Hg)in Decl;injection Decl as <-.
 change(gs_decl g=ADFunctor n names args res None)in D.
 unfold is_family,functor_parts in E;rewrite D in E.
 pose proof(kind_sym_read b w R Fl Hload _ _ Hg)as Kind.
 unfold A.kind_sym in Kind;rewrite(w_decl_read w R Fl _ _ Hg)in Kind;cbn[A.d_ast]in Kind.
 rewrite D in Kind;cbn[A.kind_of]in Kind;injection Kind as Kind.
 assert(K:gs_kind g=KFunctor)by(destruct(gs_kind g);cbn[kconv]in Kind;congruence).
 rewrite K in E;cbn[kind_eqb negb]in E.
 apply bind_inv in E as(ps&sp&Params&E).
 assert(Op:ok(family_params g))by(unfold family_params,functor_params;okt).
 destruct(Op _ _ _ Hi Params)as[Ip Fp].
 assert(Wp:st_w sp=w)by(rewrite(fr_w _ _ Fp);exact Hw).
 apply bind_inv in E as(f&ss&Fuel&E).
 unfold walk_fuel in Fuel;cbn[mbind get ret]in Fuel;rewrite Wp in Fuel;injection Fuel as <- <-.
 assert(Os:ok(family_leaves_ok(S(w_nsyms w))(gs_mod g)ps res))by apply ok_family_leaves_ok.
 destruct(Os _ _ _ Ip E)as[_ Fscan].
 assert(Poly:forall t,In t ps->A.Poly W(gs_id g)t).
 {intros t Ht;apply(family_params_poly w (P:=P)R Fl b Hload g n names args res s ps sp Hg D(proj1 Fam)
 Params Hi Hw(later_back w P R Fl _ _ Fscan L)t);exact Ht. }
 eapply family_leaves_classifies with(stack:=[])(u:=gs_id g);[exact E|exact Ip|exact Wp|
  exact(loader_mod_in w R Fl b Hload _ _ Hg)|exact L|exact Poly| | |exact(NoDup_nil _)| |cbn[List.length];lia].
 - exact(proj2 Fam _ _ _ _ _ (w_decl_read w R Fl _ _ Hg)D).
 - intros pa a h Hin Path Loaded Kh;exact(proj2(check_contexts_loaded b w sc sc h Hload Contexts Wsc Loaded)).
 - intros x[].
Qed.
End Reverse.
