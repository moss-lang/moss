(* Exact, optional scalar-family preparation for closure execution. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit CoreMoss MossX SlotX InputGenerationExports.
Import ListNotations.
Definition scalar_rb(b:rbind):bool:=match b with
 |RBPrim _ p=>match p with PBox _|PArrNew _=>false|_=>true end|_=>false end.
Definition scalar_rb_entry(b:rbind):item*rentry:=match b with
 |RBPrim i p=>(i,QPrim p)|_=>(ITy 0,QVal XVUnit)end.
Definition scalar_result_keys(Dr:tele):bool:=forallb(fun i=>negb(is_ty_item i))Dr.
Definition scalar_formal(Dr:tele):smap:=map(fun i=>(i,i))Dr.
Definition prepare_scalar(Dr:tele)(bs:list rbind):option(nat*renv):=
 if scalar_result_keys Dr &&forallb scalar_rb bs then
 match rcompose_dyn(rev(map scalar_rb_entry bs))(scalar_formal Dr)with
 |Some result=>Some(length bs,result)|None=>None end else None.
Definition instantiate_scalar(th:subst)(result:renv):renv:=
 map(fun p=>(app_subst_item th(fst p),snd p))result.

Lemma scalar_binds:forall R bs k d g st,forallb scalar_rb bs=true->
 rinterp_binds k R d g st bs=
 if Nat.ltb(length bs)k then inl(rev(map scalar_rb_entry bs)++d,g,st)
 else inr(XR XFuel g st).
Proof.
 intros R bs;induction bs as[|b bs IH];intros[|k]d g st H;cbn;auto.
 destruct b;cbn in H;try discriminate;apply andb_true_iff in H as[Prim Rest].
 assert(E:rprim_entry d i p=QPrim p)by(destruct p;try discriminate Prim;destruct i;reflexivity).
 rewrite E,IH by assumption;rewrite <-app_assoc;reflexivity.
Qed.
Lemma scalar_lookup_app:forall env base i a,rlookup i env=Some a->rlookup i(env++base)=Some a.
Proof.
 induction env as[|[j q]env IH];intros base i a H;cbn in H |- *;[discriminate|].
 destruct(item_eqb i j);[exact H|apply IH;exact H].
Qed.
Lemma scalar_compose:forall Dr env result th base,
 scalar_result_keys Dr=true->rcompose_dyn env(scalar_formal Dr)=Some result->
 rcompose_dyn(env++base)(inst_smap th Dr)=Some(instantiate_scalar th result).
Proof.
 induction Dr as[|i Dr IH];intros env result th base Keys Read.
 -cbn in Read;injection Read as <-;reflexivity.
 -cbn[scalar_result_keys forallb]in Keys;apply andb_true_iff in Keys as[Key Keys].
 apply negb_true_iff in Key.
 change(match rlookup i env,rcompose_dyn env(scalar_formal Dr)with
 |Some en,Some r=>Some((i,en)::r)|_,_=>None end=Some result)in Read.
 destruct(rlookup i env)as[a|]eqn:Found;[|discriminate].
 destruct(rcompose_dyn env(scalar_formal Dr))as[rest|]eqn:Tail;[|discriminate].
 injection Read as <-.
 unfold inst_smap;cbn[filter];rewrite Key;cbn[negb map rcompose_dyn].
 assert(L:rlookup i(env++base)=Some a).
 {apply scalar_lookup_app;exact Found. }
 rewrite L;fold(inst_smap th Dr);rewrite(IH env rest th base Keys Tail);reflexivity.
Qed.
Lemma scalar_result_types:forall Dr d,scalar_result_keys Dr=true->rresult_types Dr d=[].
Proof.
 intros Dr d H;unfold scalar_result_keys in H;unfold rresult_types.
 induction d as[|[i a]ds IH];[reflexivity|];destruct i;try exact IH;destruct a;try exact IH.
 cbn[rtype_env filter fst].
 assert(E:existsb(item_eqb(ITy t))Dr=false).
 {destruct(existsb(item_eqb(ITy t))Dr)eqn:E;[|reflexivity].
 apply existsb_exists in E as[j[In Eq]];apply item_eqb_eq in Eq;subst j.
 rewrite forallb_forall in H;specialize(H _ In);discriminate. }
 rewrite E;exact IH.
Qed.
Theorem prepared_scalar_execution:forall Dr bs count result R k d st th,
 prepare_scalar Dr bs=Some(count,result)->
 (match rinterp_binds k R d [] st bs with
 |inl(d3,_,st3)=>match rcompose_dyn d3(inst_smap th Dr)with
  |Some dr=>inl(rresult_env(rp_x R)th d3 Dr dr,st3)
  |None=>inr(XR(XErr e_compose)[]st3)end
 |inr rr=>inr rr end)=
 if Nat.ltb count k then inl(instantiate_scalar th result,st)else inr(XR XFuel [] st).
Proof.
 intros Dr bs count result R k d st th H;unfold prepare_scalar in H.
 destruct(scalar_result_keys Dr&&forallb scalar_rb bs)eqn:Good;[|discriminate].
 apply andb_true_iff in Good as[Keys Binds].
 destruct(rcompose_dyn(rev(map scalar_rb_entry bs))(scalar_formal Dr))as[formal|]eqn:Read;[|discriminate].
 injection H as <- <-;rewrite(scalar_binds R bs k d [] st Binds).
 destruct(Nat.ltb(length bs)k);[|reflexivity].
 rewrite(scalar_compose Dr _ formal th d Keys Read).
 unfold rresult_env;destruct(xreify(rp_x R));[rewrite(scalar_result_types Dr _ Keys),(xresult_generations_typefree th Dr Keys);cbn[rinst];rewrite app_nil_r|];reflexivity.
Qed.

Lemma scalar_item_nil:forall i,app_subst_item [] i=i.
Proof.
 intros i;destruct i;try reflexivity;cbn[app_subst_item];rewrite app_subst_nil.
 f_equal;induction s as[|[tk tau]s IH];[reflexivity|].
 unfold map_snd in *;cbn[map];rewrite app_subst_nil,IH;reflexivity.
Qed.
Definition instantiate_scalar_fast(th:subst)(result:renv):renv:=
 match th with []=>result|_::_=>instantiate_scalar th result end.
Lemma instantiate_scalar_fast_exact:forall th result,
 instantiate_scalar_fast th result=instantiate_scalar th result.
Proof.
 intros[|p th]result;[|reflexivity];unfold instantiate_scalar_fast,instantiate_scalar.
 induction result as[|[i a]r IH];[reflexivity|];cbn[map fst snd];rewrite scalar_item_nil,<-IH;reflexivity.
Qed.
