(* Genuine source context rows give the finite graph its physical functor names. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap NMapEnum Sx Elab ElabSound WorldSound ElabFrame
 BodySound FunctorDecl ForwarderHistory ReifyData ReifyOpaqueFunctors.
Import ListNotations.
Module A:=Audit.Elaboration.
Lemma functor_declaration_loaded:forall b w R Fl u,
 load_world b=Good w->FunctorDecl(W_read w R Fl)u->
 exists g,nget(w_syms w)u=Some g/\gs_kind g=KFunctor.
Proof.
 intros b w R Fl u Load(d&name&params&args&results&Decl&Ast).
 unfold W_read in Decl;cbn[A.w_decl]in Decl.
 destruct(nget(w_syms w)u)as[g|]eqn:Loaded;[|discriminate].
 injection Decl as <-;cbn[A.d_ast]in Ast.
 pose proof(load_world_kind b w Load _ _ Loaded)as Kind;unfold KindOK in Kind;rewrite Ast in Kind.
 exists g;split;[reflexivity|].
 destruct(gs_kind g);cbn[A.kind_of kconv]in Kind;try discriminate;reflexivity.
Qed.
Theorem declared_functor_physical:forall b w R Fl s u,
 load_world b=Good w->Inv s->st_w s=w->FunctorDecl(W_read w R Fl)u->
 source_functor_identity s u=true/\u<st_next s.
Proof.
 intros b w R Fl s u Load I World Decl.
 destruct(functor_declaration_loaded _ _ _ _ _ Load Decl)as[g[Loaded Kind]].
 assert(Here:nget(st_syms s)u=Some g).
 {apply(iv_loader _ I b ltac:(rewrite World;exact Load));rewrite World;exact Loaded. }
 split;[unfold source_functor_identity;rewrite Here,Kind;reflexivity|eapply symbol_below_next;eassumption].
Qed.
Lemma all_keys_functor:forall e u,
 In(KyFun u)(all_keys e)->nget(en_funs e)u<>None.
Proof.
 intros e u Member;unfold all_keys in Member;repeat apply in_app_or in Member as[Member|Member].
 - apply in_map_iff in Member as([v t]&Bad&Member);discriminate Bad.
 - apply in_map_iff in Member as([f sg]&Bad&Member);discriminate Bad.
 - apply in_flat_map in Member as([m rows]&Entry&Member).
   apply in_map_iff in Member as([r sg]&Bad&Member);discriminate Bad.
 - apply in_map_iff in Member as([f stamp]&Eq&Member);injection Eq as <-.
   rewrite(proj1(elems_lookup _ _ _ _)Member);discriminate.
Qed.
Theorem context_scope_functor_physical:forall b w R Fl c e F C s u,
 load_world b=Good w->Inv s->st_w s=w->CxRel w R Fl c e F C->
 In(KyFun u)(rs_items ety key(reify_scope_of e))->
 source_functor_identity s u=true/\u<st_next s.
Proof.
 intros b w R Fl c e F C s u Load I World Context Member.
 change(In(KyFun u)(all_keys e))in Member.
 pose proof(all_keys_functor _ _ Member)as Available.
 destruct(nget(en_funs e)u)as[stamp|]eqn:Raw;[|contradiction].
 pose proof(cr_funspresent _ _ _ _ _ _ _ Context u)as Present;rewrite Raw in Present.
 destruct(A.nlook u(A.c_funs C))as[fixed|]eqn:Semantic;[|discriminate Present].
 eapply declared_functor_physical;[exact Load|exact I|exact World|].
 exact(cr_fun_decls _ _ _ _ _ _ _ Context _ _ Semantic).
Qed.
