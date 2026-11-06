(* Every genuine abstract result dependency is installed as an input. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit.
Import ListNotations Audit.Input.
Module A:=Audit.Elaboration.
Lemma input_type_map_member:forall xs t,In t xs->
 A.nlook t(map(fun x=>(x,TAbs x))xs)=Some(TAbs t).
Proof.
 intro xs;induction xs as[|x rest IH];intros t Hin;[contradiction|].
 cbn[map A.nlook fst snd];destruct(Nat.eqb_spec t x)as[->|Ne];[reflexivity|].
 apply IH;destruct Hin as[Eq|Hin];[congruence|exact Hin].
Qed.
Lemma input_nlook_app:forall X k(xs ys:list(nat*X)),
 A.nlook k(xs++ys)=match A.nlook k xs with Some v=>Some v|None=>A.nlook k ys end.
Proof. intros X k xs;induction xs as[|[x v]rest IH];intro ys;cbn[List.app A.nlook fst snd];[reflexivity|destruct(Nat.eqb k x);auto]. Qed.
Lemma input_nlook_map:forall X Y(f:X->Y)k(xs:list(nat*X)),
 A.nlook k(map(fun q=>(fst q,f(snd q)))xs)=option_map f(A.nlook k xs).
Proof. intros X Y f k xs;induction xs as[|[x v]rest IH];cbn[A.nlook map fst snd];[reflexivity|destruct(Nat.eqb k x);auto]. Qed.
Theorem fun_ctx_mentions_available:forall W u C ns d n ps args res t,
 A.w_decl W u=Some d->A.d_ast d=ADFunctor n ps args res None->
 A.FunCtx W u C ns->A.Mentions W u t->A.nlook t(A.c_ty C)<>None.
Proof.
 intros W u C ns d n ps args res t Decl Ast
 (d0&ps0&args0&res0&body&es&tm&tm'&sigma&implicit&D0&Parts&Read&Inputs&Types&Merge&Ctx&Needs)Mention.
 assert(d0=d)by congruence;subst d0;rewrite Ast in Parts;cbn[A.functor_parts]in Parts;injection Parts as <- <- <- <-.
 rewrite Ctx;unfold A.entries_cx;cbn[A.c_ty];rewrite input_nlook_map,Types,input_nlook_app.
 destruct(A.nlook t tm)as[v|]eqn:Here.
 - destruct(A.nlook t(map(fun x=>(x,TAbs x))implicit));discriminate.
 - assert(In t implicit)by(apply Inputs;repeat split;assumption).
   rewrite(input_type_map_member implicit t H);discriminate.
Qed.
