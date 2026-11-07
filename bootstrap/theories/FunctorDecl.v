(* Functor environment rows name declared abstract targets, not implementation IDs. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit Sx.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition FunctorDecl(W:A.world)(u:nat):Prop:=
 exists d n names args res,A.w_decl W u=Some d/\A.d_ast d=ADFunctor n names args res None.
Definition EntriesFunctorDecl(W:A.world)(es:list A.entry):Prop:=
 forall u fx,In(A.NFun u fx)es->FunctorDecl W u.
Lemma entries_functor_append:forall W es1 es2,
 EntriesFunctorDecl W es1->EntriesFunctorDecl W es2->EntriesFunctorDecl W(es1++es2).
Proof. intros W es1 es2 H1 H2 u fx H;apply in_app_or in H;destruct H;[eapply H1|eapply H2];eassumption. Qed.
Lemma entries_functor_types:forall W xs,EntriesFunctorDecl W(map A.NTy xs).
Proof. intros W xs u fx H;apply in_map_iff in H as(t&Bad&Hin);discriminate Bad. Qed.
Scheme functor_item_mut:=Minimality for A.ItemEntries Sort Prop
 with functor_items_mut:=Minimality for A.ItemsEntries Sort Prop.
Lemma items_functor_decls:forall W p ob tm specs es tm',
 A.ItemsEntries W p ob tm specs es tm'->EntriesFunctorDecl W es.
Proof.
 intro W;apply(functor_items_mut W
 (fun p ob tm sp es tm'=>EntriesFunctorDecl W es)
 (fun p ob tm specs es tm'=>EntriesFunctorDecl W es)).
 - intros p ob tm pa t Res Kind u fx[Bad|[]];discriminate Bad.
 - intros p ob tm pa s k Res Kind Br u fx[].
 - intros p ob tm a pa s d n t ty0 bs ap Res Decl Ast Br Ty App u fx[Bad|[]];discriminate Bad.
 - intros p ob tm a pa s sg bs ap Res Kind Br Sig App u fx[Bad|[]];discriminate Bad.
 - intros p ob tm pa u d n names args res fx Res Decl Ast Fixed Read u0 fx0[Eq|[]].
   injection Eq as <- <-;exists d,n,names,args,res;split;assumption.
 - intros p ob tm a pa dn r m rt sg bs ap Res Named Br Recv Sig App u fx[Bad|[]];discriminate Bad.
 - intros p ob tm a pa c d n its bs es tm' Res Decl Ast Br Entries IH;exact IH.
 - intros p ob tm u fx[].
 - intros p ob tm sp rest es1 tm1 es2 tm2 Entry IH1 Entries IH2.
   apply entries_functor_append;assumption.
Qed.
Lemma root_items_functor_decls:forall W p es0 tm specs es tm',
 A.RootItems W p es0 tm specs es tm'->EntriesFunctorDecl W es.
Proof.
 intros W p es0 tm specs es tm' H;induction H.
 - intros u fx[].
 - apply entries_functor_append;[apply entries_functor_types|].
   apply entries_functor_append;[|exact IHRootItems].
   intros u0 fx0[Eq|[]];injection Eq as <- <-;exists du,n,ps,args,res0;split;assumption.
 - exact IHRootItems.
Qed.
Lemma root_functor_decls:forall W s es0 tm es tm',
 A.RootCtx W s es0 tm es tm'->EntriesFunctorDecl W es.
Proof.
 intros W s es0 tm es tm' H;destruct H.
 - intros u fx[].
 - eapply root_items_functor_decls;eassumption.
Qed.
Lemma entries_functor_row:forall sigma tm es u fx,
 A.nlook u(A.c_funs(A.entries_cx sigma tm es))=Some fx->exists raw,In(A.NFun u raw)es.
Proof.
 intros sigma tm es;induction es as[|entry rest IH];intros u fx H;[discriminate H|].
 destruct entry as[|v ty app|f sg app|r m sg app|u0 raw];cbn[A.entries_cx A.c_funs flat_map List.app]in H;
 try(destruct(IH _ _ H)as(a&Hin);exists a;right;exact Hin).
 cbn[A.nlook]in H;destruct(Nat.eqb u u0)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst u0;exists raw;left;reflexivity.
 - destruct(IH _ _ H)as(a&Hin);exists a;right;exact Hin.
Qed.
Lemma ctxof_functor_decls:forall W s C ns,A.CtxOf W s C ns->
 forall u fx,A.nlook u(A.c_funs C)=Some fx->FunctorDecl W u.
Proof.
 intros W s C ns(d&es&tm&er&tmr&sigma&Decl&Items&Root&Merge&Eq&Needs)u fx H.
 rewrite Eq in H;destruct(entries_functor_row _ _ _ _ _ H)as(raw&Hin).
 eapply entries_functor_append;[eapply items_functor_decls;exact Items|eapply root_functor_decls;exact Root|exact Hin].
Qed.
Lemma functx_functor_decls:forall W s C ns,A.FunCtx W s C ns->
 forall u fx,A.nlook u(A.c_funs C)=Some fx->FunctorDecl W u.
Proof.
 intros W s C ns(d&names&args&res&body&es&tm&tm'&sigma&implicit&Decl&Parts&Items&Impl&Ty&Merge&Eq&Needs)u fx H.
 rewrite Eq in H;destruct(entries_functor_row _ _ _ _ _ H)as(raw&Hin).
 eapply items_functor_decls;[exact Items|exact Hin].
Qed.
