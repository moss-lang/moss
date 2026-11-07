(* The selected family is a genuinely classified loaded abstract target. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr BodyTy
 FunctorDecl FunctorDeclExec NMapEnum FamilyKeysExec PMatchExec NominalSupport.
Import ListNotations.
Local Open Scope string_scope.
Local Notation "x +++ y":=(List.app x y)(at level 60,right associativity).
Module A:=Audit.Elaboration.
Definition family_candidate:=(gsym*list(nat*ety))%type.
Definition family_leaf:=(nat*gsym*gsym*list(nat*ety))%type.
Definition family_candidate_step(mi:nat)(e em:env)(r:ety)(mid u:nat)(fg:gsym)
 (marks back:list(nat*nat))(found:list family_candidate)(lf:family_leaf):M(list family_candidate):=

          let '(lmi, rg, m, subst) := lf in
          if negb (Nat.eqb (gs_id m) mid) then ret found else
          let! pat := receiver_at em lmi rg subst in
          let th := pmatch_full back pat r [] in
          let mentioned := filter (fun q => nmem (snd q) (ety_gens pat)) marks in
          if negb (forallb (fun q => is_some (nassoc (fst q) th)) mentioned) then ret found else
          let at_th := canon e (ety_subst_full (map (fun q => (snd q, opt_default (YAbs (snd q)) (nassoc (fst q) th))) marks) pat) in
          if negb (ety_eqb at_th r) then ret found else
          match filter (fun q => Nat.eqb (gs_id (fst q)) u) found with
          | (_, th0) :: _ =>
              if forallb (fun q => match nassoc (fst q) th0 with Some y => ety_eqb y (snd q) | None => false end) th
                 && Nat.eqb (List.length th) (List.length th0) then ret found
              else let! sr := show r in
                   err mi ("`" ++ sr ++ gs_name m ++ "` is two items of the family `" ++ gs_name fg ++
                           "` here, so which one is meant is ambiguous (D84 rule 7)")
          | [] => ret (found +++ [(fg, th)])
          end.
Definition family_search_step(mi:nat)(e:env)(r:ety)(mid:nat)(found:list family_candidate)(u:nat):M(list family_candidate):=

      let! fg := sym u in
      let! fam := is_family fg in
      if negb fam then ret found else
      let! ps := family_params fg in
      let! marks := mmap (fun p => let! k := fresh_marker_for e r in ret (p, k)) ps in
      let! em := mfold (fun e q => let! s := fresh in ret (set_tn e (fst q) (YAbs (snd q)) s)) e marks in
      let '(_, res, _) := functor_parts fg in
      let! n := walk_fuel in
      let! leaves := family_leaves n em (gs_mod fg) res [] in
      let back := map (fun q => (snd q, fst q)) marks in
mfold(family_candidate_step mi e em r mid u fg marks back)found leaves.
Definition family_search(mi:nat)(e:env)(r:ety)(mid:nat):M(list family_candidate):=
 mfold(family_search_step mi e r mid)[](map fst(elems(en_funs e))).
Definition FamilyFound(Q:family_candidate->Prop)(found:list family_candidate):Prop:=
 Forall Q found.
Local Strategy opaque[FUEL pmatch ety_subst_full].
Lemma family_candidate_preserves:forall Q mi e em r mid u fg marks back found lf s found' sf,
 family_candidate_step mi e em r mid u fg marks back found lf s=Good(found',sf)->
 FamilyFound Q found->(forall pat,Q(fg,pmatch_full back pat r []))->FamilyFound Q found'.
Proof.
 intros Q mi e em r mid u fg marks back found [[[lmi rg]m]sub]s found' sf E Old Fam.
 unfold family_candidate_step in E;cbn beta iota zeta in E.
 match type of E with context[if ?test then ret found else _]=>
 destruct test;[injection E as <- <-;exact Old|]end.
 apply bind_inv in E as(pat&sp&Read&E);cbn beta iota zeta in E.
 match type of E with context[if ?test then ret found else _]=>
 destruct test;[injection E as <- <-;exact Old|]end.
 match type of E with context[if ?test then ret found else _]=>
 destruct test;[injection E as <- <-;exact Old|]end.
 destruct(filter(fun q=>Nat.eqb(gs_id(fst q))u)found)as[|[other th0]rest]eqn:Seen.
 - injection E as <- <-;apply Forall_app;split;[exact Old|constructor;[exact(Fam pat)|constructor]].
 - cbn beta iota zeta in E.
   match type of E with context[if ?test then ret found else _]=>
   destruct test;[injection E as <- <-;exact Old|]end.
   apply bind_inv in E as(name&ss&Show&E);exfalso;eapply err_not_good;exact E.
Qed.
Lemma family_candidates_preserve:forall Q mi e em r mid u fg marks back leaves found s found' sf,
 mfold(family_candidate_step mi e em r mid u fg marks back)found leaves s=Good(found',sf)->
 FamilyFound Q found->(forall pat,Q(fg,pmatch_full back pat r []))->FamilyFound Q found'.
Proof.
 intros Q mi e em r mid u fg marks back leaves;induction leaves as[|lf rest IH];intros found s found' sf E Old Fam.
 - injection E as <- <-;exact Old.
 - cbn[mfold]in E;apply bind_inv in E as(next&sn&Step&Tail).
   eapply IH;[exact Tail|eapply family_candidate_preserves;[exact Step|exact Old|exact Fam]|exact Fam].
Qed.
Lemma family_member_search:forall mi e r mid s g th sf,
 family_member mi e r mid s=Good(Some(g,th),sf)->family_search mi e r mid s=Good([(g,th)],sf).
Proof.
 intros mi e r mid s g th sf E;unfold family_member in E.
 apply bind_inv in E as(found&ss&Search&E).
 change(family_search mi e r mid s=Good(found,ss))in Search.
 destruct found as[|[g0 th0]rest];[discriminate E|].
 destruct rest as[|other rest].
 - injection E as <- <- <-;exact Search.
 - destruct other as[g1 th1]. apply bind_inv in E as(nm&sr&Show&E).
   apply bind_inv in E as(m&sm&Sym&E);exfalso;eapply err_not_good;exact E.
Qed.
Lemma ok_family_receiver:forall e mi rg sub,ok(receiver_at e mi rg sub).
Proof. intros;unfold receiver_at;okt. Qed.
Lemma ok_family_params:forall g,ok(family_params g).
Proof. intros;unfold family_params,functor_params;okt. Qed.
Lemma ok_family_classify:forall g,ok(is_family g).
Proof. intros;unfold is_family;okt. Qed.
#[local]Hint Resolve ok_family_receiver ok_family_params ok_family_classify:okdb.
Local Strategy opaque[receiver_at family_params is_family].
Lemma ok_family_candidate_step:forall mi e em r mid u fg marks back found lf,
 ok(family_candidate_step mi e em r mid u fg marks back found lf).
Proof. intros mi e em r mid u fg marks back found [[[lmi rg]m]sub];unfold family_candidate_step;okt. Qed.
#[local]Hint Resolve ok_family_candidate_step:okdb.
Local Strategy opaque[family_candidate_step].
Lemma ok_family_search_step:forall mi e r mid found u,ok(family_search_step mi e r mid found u).
Proof. intros;unfold family_search_step;okt. Qed.
#[local]Hint Resolve ok_family_search_step:okdb.
Lemma family_marker_keys:forall e receiver (ps:list nat) s marks sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->map fst marks=ps.
Proof.
 intros e receiver ps;induction ps as[|p ps IH];intros s marks sf E;cbn[mmap]in E.
 - injection E as <- _;reflexivity.
 - apply bind_inv in E as(q&sq&Head&E);apply bind_inv in Head as(k&sk&Alloc&Ret);injection Ret as <- <-.
   apply bind_inv in E as(rest&sr&Tail&E);injection E as <- _;cbn[map fst snd];f_equal;eapply IH;exact Tail.
Qed.
Section Classified.
Variable w:Elab.world. Context{P:xprogram}.
Variables(R:nat->list nat)(Fl:string->nat)(b:bundle).
Hypothesis Hload:load_world b=Good w.
Notation W:=(BodySoundCore.W w R Fl).
Definition Classified(q:family_candidate):Prop:=nget(w_syms w)(gs_id(fst q))=Some(fst q)/\A.Family W(gs_id(fst q)).
Lemma family_search_step_classified:forall c e F C mi r mid found u stamp s found' sf,
 CxRel w R Fl c e F C->nget(en_funs e)u=Some stamp->
 family_search_step mi e r mid found u s=Good(found',sf)->
 Inv s->st_w s=w->Later w P R Fl sf->FamilyFound Classified found->FamilyFound Classified found'.
Proof.
 intros c e F C mi r mid found u stamp s found' sf HC Have E Hi Hw L Old.
 unfold family_search_step in E;apply bind_inv in E as(fg&ss&Sym&E).
 destruct(cxrel_functor_sym w R Fl b Hload c e F C u stamp s fg ss HC Have Sym Hi Hw)
 as[->[Hg(n&names&args&res&D)]].
 apply bind_inv in E as(fam&sc&Class&E).
 destruct(ok_family_classify fg _ _ _ Hi Class)as[Ic Fc].
 destruct fam;cbn[negb]in E.
 2:{injection E as <- <-;exact Old. }
 match type of E with ?m sc=Good(found',sf)=>assert(Otail:ok m)by okt end.
 destruct(Otail _ _ _ Ic E)as[_ Ft].
 pose proof(FamilyKeysExec.is_family_loaded w (P:=P)R Fl b Hload fg s sc Hg Class Hi Hw
 (later_back w P R Fl _ _ Ft L))as Fam.
 apply bind_inv in E as(ps&sp&Params&E).
 apply bind_inv in E as(marks&sm&Marks&E).
 apply bind_inv in E as(em&se&Env&E).
 unfold functor_parts in E;rewrite D in E;cbn beta iota zeta in E.
 apply bind_inv in E as(f&sn&Fuel&E).
 apply bind_inv in E as(leaves&sl&Leaves&E).
 eapply family_candidates_preserve;[exact E|exact Old|intro pat;split;assumption].
Qed.
Lemma family_search_closed:forall c e F C mi r mid xs found s found' sf,
 CxRel w R Fl c e F C->(forall u,In u xs->exists stamp,nget(en_funs e)u=Some stamp)->
 mfold(family_search_step mi e r mid)found xs s=Good(found',sf)->
 Inv s->st_w s=w->Later w P R Fl sf->FamilyFound Classified found->FamilyFound Classified found'.
Proof.
 intros c e F C mi r mid xs;induction xs as[|u rest IH];intros found s found' sf HC Have E Hi Hw L Old.
 - injection E as <- <-;exact Old.
 - cbn[mfold]in E;apply bind_inv in E as(next&sn&Step&Tail).
   destruct(ok_family_search_step mi e r mid found u _ _ _ Hi Step)as[In Fn].
   assert(Otail:ok(mfold(family_search_step mi e r mid)next rest))by(apply ok_mfold;apply ok_family_search_step).
   destruct(Otail _ _ _ In Tail)as[_ Ft].
   destruct(Have u(or_introl eq_refl))as(stamp&Row).
   assert(New:FamilyFound Classified next).
   {eapply family_search_step_classified;[exact HC|exact Row|exact Step|exact Hi|exact Hw|
    exact(later_back w P R Fl _ _ Ft L)|exact Old]. }
   eapply IH;[exact HC| |exact Tail|exact In|exact(fr_w' w _ _ Fn Hw)|exact L|exact New].
   intros v Hv;apply Have;right;exact Hv.
Qed.
Theorem family_member_classified:forall c e F C mi r mid s g th sf,
 CxRel w R Fl c e F C->family_member mi e r mid s=Good(Some(g,th),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->
 nget(w_syms w)(gs_id g)=Some g/\A.Family W(gs_id g).
Proof.
 intros c e F C mi r mid s g th sf HC E Hi Hw L.
 pose proof(family_member_search _ _ _ _ _ _ _ _ E)as Search.
 assert(Have:forall u,In u(map fst(elems(en_funs e)))->exists stamp,nget(en_funs e)u=Some stamp).
 {intros u Hu;apply in_map_iff in Hu as([uid stamp]&Eq&Hin);cbn[fst]in Eq;subst uid.
 exists stamp;apply elems_lookup;exact Hin. }
 pose proof(family_search_closed c e F C mi r mid _ _ _ _ _ HC Have Search Hi Hw L(Forall_nil _))as Closed.
 inversion Closed;subst;assumption.
Qed.
Definition TypedCandidate(r:ety)(q:family_candidate):Prop:=Classified q/\
 Forall(fun p=>WFty(snd p))(snd q)/\Forall(fun p=>Support W(snd p))(snd q)/\
 NoDup(map fst(snd q))/\Forall(fun p=>A.Poly W(gs_id(fst q))(fst p))(snd q).
Lemma family_search_step_typed:forall c e F C mi r mid found u stamp s found' sf,
 CxRel w R Fl c e F C->nget(en_funs e)u=Some stamp->
 family_search_step mi e r mid found u s=Good(found',sf)->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->Support W r->
 FamilyFound(TypedCandidate r)found->FamilyFound(TypedCandidate r)found'.
Proof.
 intros c e F C mi r mid found u stamp s found' sf HC Have E Hi Hw L Wr Sr Old.
 unfold family_search_step in E;apply bind_inv in E as(fg&ss&Sym&E).
 destruct(cxrel_functor_sym w R Fl b Hload c e F C u stamp s fg ss HC Have Sym Hi Hw)
 as[->[Hg(n&names&args&res&D)]].
 apply bind_inv in E as(fam&sc&Class&E).
 destruct(ok_family_classify fg _ _ _ Hi Class)as[Ic Fc].
 destruct fam;cbn[negb]in E;[|injection E as <- <-;exact Old].
 match type of E with ?m sc=Good(found',sf)=>assert(Otail:ok m)by okt end.
 destruct(Otail _ _ _ Ic E)as[_ Ft].
 pose proof(FamilyKeysExec.is_family_loaded w (P:=P)R Fl b Hload fg s sc Hg Class Hi Hw
 (later_back w P R Fl _ _ Ft L))as Fam.
 apply bind_inv in E as(ps&sp&Params&E).
 destruct(ok_family_params fg _ _ _ Ic Params)as[Ip Fp].
 match type of E with ?m sp=Good(found',sf)=>assert(Orest:ok m)by okt end.
 destruct(Orest _ _ _ Ip E)as[_ Fr].
 pose proof(family_params_poly w (P:=P)R Fl b Hload fg n names args res sc ps sp Hg D(proj1 Fam)
 Params Ic(fr_w' w _ _ Fc Hw)(later_back w P R Fl _ _ Fr L))as Poly.
 apply bind_inv in E as(marks&sm&Marks&E).
 pose proof(family_marker_keys _ _ _ _ _ _ Marks)as Mk.
 apply bind_inv in E as(em&se&Env&E).
 unfold functor_parts in E;rewrite D in E;cbn beta iota zeta in E.
 apply bind_inv in E as(f&sn&Fuel&E);apply bind_inv in E as(leaves&sl&Leaves&E).
 eapply family_candidates_preserve;[exact E|exact Old|].
 intro pat;split;[split;assumption|];cbn[fst snd];unfold pmatch_full.
 split;[apply pmatch_types_wf;[exact Wr|constructor]|].
 split;[apply pmatch_types_support;[exact Sr|constructor]|].
 split;[apply pmatch_keys_nodup;constructor|].
 pose proof(pmatch_keys_domain (ety_depth pat)(map(fun q=>(snd q,fst q))marks)pat r[](Forall_nil _))as Dom.
 eapply Forall_impl;[|exact Dom].
 intros[p t](g&Lookup);cbn[fst snd]in *.
 apply Poly;rewrite <-Mk.
 apply pmatch_lookup_member in Lookup;apply in_map_iff in Lookup as([p0 k]&Eq&Hin).
 cbn[fst snd]in Eq;injection Eq as <- <-;apply in_map_iff;exists(p0,k);split;[reflexivity|exact Hin].
Qed.
Lemma family_search_typed:forall c e F C mi r mid xs found s found' sf,
 CxRel w R Fl c e F C->(forall u,In u xs->exists stamp,nget(en_funs e)u=Some stamp)->
 mfold(family_search_step mi e r mid)found xs s=Good(found',sf)->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->Support W r->FamilyFound(TypedCandidate r) found->FamilyFound(TypedCandidate r) found'.
Proof.
 intros c e F C mi r mid xs;induction xs as[|u rest IH];intros found s found' sf HC Have E Hi Hw L Wr Sr Old.
 - injection E as <- <-;exact Old.
 - cbn[mfold]in E;apply bind_inv in E as(next&sn&Step&Tail).
   destruct(ok_family_search_step mi e r mid found u _ _ _ Hi Step)as[In Fn].
   assert(Otail:ok(mfold(family_search_step mi e r mid)next rest))by(apply ok_mfold;apply ok_family_search_step).
   destruct(Otail _ _ _ In Tail)as[_ Ft].
   destruct(Have u(or_introl eq_refl))as(stamp&Row).
   assert(New:FamilyFound(TypedCandidate r) next).
   {eapply family_search_step_typed;[exact HC|exact Row|exact Step|exact Hi|exact Hw|
    exact(later_back w P R Fl _ _ Ft L)|exact Wr|exact Sr|exact Old]. }
   eapply IH;[exact HC| |exact Tail|exact In|exact(fr_w' w _ _ Fn Hw)|exact L|exact Wr|exact Sr|exact New].
   intros v Hv;apply Have;right;exact Hv.
Qed.
Theorem family_member_substitution:forall c e F C mi r mid s g th sf,
 CxRel w R Fl c e F C->family_member mi e r mid s=Good(Some(g,th),sf)->
 Inv s->st_w s=w->Later w P R Fl sf->WFty r->Support W r->
 (nget(w_syms w)(gs_id g)=Some g/\A.Family W(gs_id g))/\
 Forall(fun p=>WFty(snd p))th/\Forall(fun p=>Support W(snd p))th/\
 NoDup(map fst th)/\Forall(fun p=>A.Poly W(gs_id g)(fst p))th.
Proof.
 intros c e F C mi r mid s g th sf HC E Hi Hw L Wr Sr.
 pose proof(family_member_search _ _ _ _ _ _ _ _ E)as Search.
 assert(Have:forall u,In u(map fst(elems(en_funs e)))->exists stamp,nget(en_funs e)u=Some stamp).
 {intros u Hu;apply in_map_iff in Hu as([uid stamp]&Eq&Hin);cbn[fst]in Eq;subst uid.
 exists stamp;apply elems_lookup;exact Hin. }
 pose proof(family_search_typed c e F C mi r mid _ _ _ _ _ HC Have Search Hi Hw L Wr Sr(Forall_nil _))as Closed.
 inversion Closed;subst;assumption.
Qed.
End Classified.
