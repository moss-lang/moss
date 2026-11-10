(* Result mention availability descends through context output lists and
   supplies the fixed inputs of functor outputs, including bracket overrides. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel Sx.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma output_domain_lookup_app:forall V k(l r:list(nat*V)),
 A.nlook k(l++r)=match A.nlook k l with Some v=>Some v|None=>A.nlook k r end.
Proof.
 intros V k l;induction l as[|[x y]l IH];intro r;[reflexivity|].
 cbn[A.nlook app];destruct(Nat.eqb k x);[reflexivity|exact(IH r)].
Qed.
Lemma output_nlook_member:forall V k(v:V)l,
 In(k,v)l->A.nlook k l<>None.
Proof.
 intros V k v l;induction l as[|[x y]l IH];intros H;[contradiction|].
 cbn[A.nlook];destruct(Nat.eqb k x)eqn:E;[discriminate|].
 destruct H as[Eq|H];[injection Eq as <- <-;rewrite Nat.eqb_refl in E;discriminate|exact(IH H)].
Qed.
Lemma output_pair_member:forall X Y(R:X->Y->Prop)xs ys,
 Forall2 R xs ys->forall x,In x xs->exists y,In y ys/\R x y.
Proof.
 intros X Y R xs ys F;induction F as[|x y xs ys Rel F IH];intros z Inz.
 - contradiction.
 - destruct Inz as[Eq|Hin].
   + subst z;exists y;split;[left;reflexivity|exact Rel].
   + destruct(IH z Hin)as[q[Inq Rq]];exists q;split;[right;exact Inq|exact Rq].
Qed.
Lemma output_bracket_bound:forall W p tm a bs,
 A.OutputBrackets W p tm a bs->forall b,
 In b(match a with Some xs=>xs|None=>[]end)->forall t,
 A.resolve_sym W p(fst b)=Some t->A.nlook t bs<>None.
Proof.
 intros W p tm a bs H b Inb t Res;destruct H as[|xs ys F];[contradiction|].
 destruct(output_pair_member _ _ _ _ _ F b Inb)as[q[Inq(sp&sa&Spec&Key&Kind&Read)]].
 assert(E:fst q=t)by congruence.
 destruct q as[x ty];cbn[fst]in E;subst x.
 eapply output_nlook_member;rewrite <- in_rev;exact Inq.
Qed.
Lemma output_bracket_unbound:forall W p tm a bs t,
 A.OutputBrackets W p tm a bs->A.nlook t bs=None->
 ~(exists b,In b(match a with Some xs=>xs|None=>[]end)/\A.resolve_sym W p(fst b)=Some t).
Proof.
 intros W p tm a bs t B N(b&Inb&R).
 exact(output_bracket_bound W p tm a bs B b Inb t R N).
Qed.
Lemma output_context_domain:forall W self p pa a c dc n its tm bs,
 A.resolve_sym W p pa=Some c->A.w_decl W c=Some dc->A.d_ast dc=ADCtx n its->
 A.OutputBrackets W p tm a bs->
 (forall t,A.ResultMentions W self p(Spec(Some pa)None a)t->A.nlook t tm<>None)->
 forall sp,In(CISpec sp)its->forall t,A.ResultMentions W self(A.d_mod dc)sp t->
 A.nlook t(bs++tm)<>None.
Proof.
 intros W self p pa a c dc n its tm bs Res Decl Ast Br Dom sp Insp t Ment.
 rewrite output_domain_lookup_app.
 destruct(A.nlook t bs)eqn:N;[discriminate|].
 apply Dom;eapply A.RMCtx;try eassumption.
 exact(output_bracket_unbound W p tm a bs t Br N).
Qed.
Lemma output_functor_domain:forall W self p pa a u du n ps args res tm bs,
 A.resolve_sym W p pa=Some u->A.w_decl W u=Some du->A.d_ast du=ADFunctor n ps args res None->
 A.OutputBrackets W p tm a bs->
 (forall t,A.ResultMentions W self p(Spec(Some pa)None a)t->A.nlook t tm<>None)->
 (forall t,A.Mentions W self t->A.nlook t tm<>None)->
 forall t,A.Fixed W u t->A.nlook t(bs++tm)<>None.
Proof.
 intros W self p pa a u du n ps args res tm bs Res Decl Ast Br Dom Self t[M NP].
 rewrite output_domain_lookup_app;destruct(A.nlook t bs)eqn:N;[discriminate|].
 destruct(Nat.eq_dec u self)as[Eq|Ne];[subst u;exact(Self t M)|].
 destruct ps as[names|].
 - destruct M as(d&ps'&args'&res'&body'&sp&D&P&Insp&Ment&NoOut).
   rewrite Decl in D;injection D as <-;rewrite Ast in P;cbn[A.functor_parts]in P.
   injection P as <- <- <- <-.
   apply Dom;eapply A.RMFunctor;try eassumption.
   + intros(x&Inx&Rx);apply NP;exists du,n,(Some names),args,res.
     split;[exact Decl|];split;[exact Ast|];exists x;split;assumption.
   + exact(output_bracket_unbound W p tm a bs t Br N).
 - exfalso;apply NP;exists du,n,None,args,res;split;[exact Decl|];split;[exact Ast|exact M].
Qed.
