(* Existing source type readings survive the addition of previously absent meanings. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit TyOfSubst ProvideResultSubst TySupport.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition LookupExtends(tm tm':list(nat*ty)):Prop:=
 forall x y,A.nlook x tm=Some y->A.nlook x tm'=Some y.
Lemma lookup_extends_app:forall bs tm tm',LookupExtends tm tm'->LookupExtends(bs++tm)(bs++tm').
Proof.
 intros bs tm tm' E x y H;rewrite !nlook_app' in H|-*.
 destruct(A.nlook x bs)as[v|];[exact H|apply E;exact H].
Qed.
Definition overlay_missing(seed tm:list(nat*ty)):list(nat*ty):=
 map(fun q=>(fst q,match A.nlook(fst q)tm with Some v=>v|None=>snd q end))seed++tm.
Lemma lookup_overlay_missing:forall seed tm k,
 A.nlook k(overlay_missing seed tm)=match A.nlook k tm with Some v=>Some v|None=>A.nlook k seed end.
Proof.
 intros seed;induction seed as[|[j v]seed IH];intros tm k.
 - cbn[overlay_missing map List.app];destruct(A.nlook k tm);reflexivity.
 - cbn[overlay_missing map List.app fst snd A.nlook].
   destruct(Nat.eqb_spec k j)as[->|Ne];[destruct(A.nlook j tm);reflexivity|exact(IH tm k)].
Qed.
Lemma lookup_overlay_extends:forall seed tm,LookupExtends tm(overlay_missing seed tm).
Proof. intros seed tm k v E;rewrite lookup_overlay_missing,E;reflexivity. Qed.
Lemma lookup_seed_member:forall (seed:list(nat*ty)) k v,A.nlook k seed=Some v->In(k,v)seed.
Proof.
 intro seed;induction seed as[|[j w]seed IH];intros k v E;[discriminate E|].
 cbn[A.nlook]in E;destruct(Nat.eqb_spec k j)as[->|Ne];
 [injection E as <-;left;reflexivity|right;exact(IH k v E)].
Qed.
Section Reading.
Variable W:A.world.
Fixpoint tyof_extends p tm th t ty(H:A.TyOf W p tm th t ty){struct H}:
 forall tm',LookupExtends tm tm'->A.TyOf W p tm' th t ty
with brackets_extends p tm g a bs(H:A.Brackets W p tm g a bs){struct H}:
 forall tm',LookupExtends tm tm'->A.Brackets W p tm' g a bs.
Proof.
 - destruct H as[p tm th|p tm th|p tm t|p tm th fs ts F|p tm th ms ts flat F Ef Fm Nd|
     p tm th pa t tau R K L|p tm th pa a g k bs args R K Kk B F|
     p tm th pa a g d n body bs tau R D Ast B T];intros tm' E.
   + constructor.
   + constructor.
   + constructor.
   + constructor;induction F as[|nt ft fs ts[Key Ty]F IH];constructor;[|exact IH].
     split;[exact Key|exact(tyof_extends _ _ _ _ _ Ty tm' E)].
   + eapply A.TyUnion;[|exact Ef|exact Fm|exact Nd].
     clear Ef Fm Nd;induction F as[|m ty ms ts Ty F IH].
     * constructor.
     * constructor;[exact(tyof_extends _ _ _ _ _ Ty tm' E)|exact IH].
   + eapply A.TyType;[exact R|exact K|apply E;exact L].
   + eapply A.TyNom;[exact R|exact K|exact Kk|exact(brackets_extends _ _ _ _ _ B tm' E)|].
     pose proof(lookup_extends_app bs tm tm' E)as Ex.
     induction F as[|r q rs args[Key Read]F IH];constructor;[|exact IH].
     split;[exact Key|apply Ex;exact Read].
   + eapply A.TyAlias;[exact R|exact D|exact Ast|exact(brackets_extends _ _ _ _ _ B tm' E)|].
     exact(tyof_extends _ _ _ _ _ T(bs++tm')(lookup_extends_app bs tm tm' E)).
 - destruct H as[p tm g|p tm g args l F];intros tm' E;[constructor|].
   apply A.BrSome;induction F as[|b tb args l(sp&sa&Eq&R&K&Req&Ty)F IH];constructor;[|exact IH].
   exists sp,sa;split;[exact Eq|];split;[exact R|];split;[exact K|];split;[exact Req|].
   exact(tyof_extends _ _ _ _ _ Ty tm' E).
Qed.
Theorem output_brackets_extends:forall p tm a bs,
 A.OutputBrackets W p tm a bs->forall tm',LookupExtends tm tm'->A.OutputBrackets W p tm' a bs.
Proof.
 intros p tm a bs H tm' E;destruct H as[|args l F];[constructor|].
 apply A.OBrSome;induction F as[|b tb args l(sp&sa&Eq&R&K&Ty)F IH];constructor;[|exact IH].
 exists sp,sa;split;[exact Eq|];split;[exact R|];split;[exact K|].
 exact(tyof_extends _ _ _ _ _ Ty tm' E).
Qed.
Theorem signature_extends:forall s tm th sg,
 A.SigOf W s tm th sg->forall tm',LookupExtends tm tm'->A.SigOf W s tm' th sg.
Proof.
 intros s tm th sg(d&rcv&n&ps&rt&body&Decl&Ast&Params&Ret)tm' E.
 exists d,rcv,n,ps,rt,body;split;[exact Decl|];split;[exact Ast|];split.
 - clear Decl Ast Ret;induction Params as[|pt t ps ts Ty Params IH].
   + constructor.
   + constructor;[exact(tyof_extends _ _ _ _ _ Ty tm' E)|exact IH].
 - destruct rt as[t|];[exact(tyof_extends _ _ _ _ _ Ret tm' E)|exact Ret].
Qed.
Theorem receiver_extends_preserving_head:forall r tm ty,
 A.RecvTy W r tm ty->forall tm',LookupExtends tm tm'->
 A.nlook r tm'=A.nlook r tm->A.RecvTy W r tm' ty.
Proof.
 intros r tm ty Read tm' E Head;unfold A.RecvTy in Read|-*;rewrite Head.
 destruct(A.nlook r tm)as[y|];[exact Read|].
 assert(Args:forall args,
   Forall2(fun x q=>fst q=x/\A.nlook x tm=Some(snd q))(A.w_reqs W r)args->
   Forall2(fun x q=>fst q=x/\A.nlook x tm'=Some(snd q))(A.w_reqs W r)args).
 {clear Read Head;intros args H;induction H as[|x q xs qs[Key Lookup]H IH].
  - constructor.
  - constructor;[split;[exact Key|apply E;exact Lookup]|exact IH]. }
 destruct(A.w_decl W r)as[d|];[destruct(A.d_ast d)|];
 try solve[destruct Read as(args&Eq&Reads);exists args;split;[exact Eq|apply Args;exact Reads]].
 eapply tyof_extends;[exact Read|exact E].
Qed.
Definition TypeExtension(tm tm':list(nat*ty)):Prop:=
 LookupExtends tm tm'/\forall r,A.kind_sym W r<>Some A.DKType->A.nlook r tm'=A.nlook r tm.
Theorem overlay_type_extension:forall seed tm,
 Forall(fun q=>A.kind_sym W(fst q)=Some A.DKType)seed->TypeExtension tm(overlay_missing seed tm).
Proof.
 intros seed tm Keys;split;[apply lookup_overlay_extends|].
 intros r Other;rewrite lookup_overlay_missing;destruct(A.nlook r tm)as[v|];[reflexivity|].
 destruct(A.nlook r seed)as[v|]eqn:Read;[|reflexivity].
 exfalso;apply Other;rewrite Forall_forall in Keys;exact(Keys(r,v)(lookup_seed_member seed r v Read)).
Qed.
Theorem receiver_extends_supported:forall b r tm ty,
 A.WorldOf b W->TySupport W ty->A.RecvTy W r tm ty->
 forall tm',TypeExtension tm tm'->A.RecvTy W r tm' ty.
Proof.
 intros b r tm ty Wo Support Read tm'[Ext NonType].
 destruct(A.nlook r tm)as[t|]eqn:Head.
 - eapply receiver_extends_preserving_head;[exact Read|exact Ext|].
   rewrite(Ext r t Head),Head;reflexivity.
 - assert(Dec:{A.kind_sym W r=Some A.DKType}+{A.kind_sym W r<>Some A.DKType}).
   {destruct(A.kind_sym W r)as[k|];[destruct k|];first[left;reflexivity|right;discriminate]. }
   destruct Dec as[TyKind|Other].
   2:{eapply receiver_extends_preserving_head;[exact Read|exact Ext|apply NonType;exact Other]. }
   pose proof TyKind as Kind;unfold A.kind_sym in Kind.
   destruct(A.w_decl W r)as[d|]eqn:Decl;[|discriminate Kind].
   destruct(A.d_ast d)eqn:Ast;cbn[A.kind_of]in Kind;try discriminate Kind.
   all:try solve[match goal with rcv:Audit.Input.fnrecv|-_=>destruct rcv;discriminate Kind end].
   unfold A.RecvTy in Read;rewrite Head,Decl,Ast in Read.
   destruct Read as(args&Eq&F);subst ty.
   assert(Nz:r<>A.rec_head)by(exact(proj1(@A.wo_decl_sound b W Wo r d Decl))).
   inversion Support;subst.
   + match goal with Hk:(A.kind_sym W _=Some A.DKUnit\/A.kind_sym W _=Some A.DKTag)|-_=>destruct Hk;congruence end.
   + contradiction.
Qed.
End Reading.
