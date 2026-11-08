(* Chosen methods are function declarations in the method namespace. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound BodySoundCore BodyTy MethodLookup.
Import ListNotations.
Module A:=Audit.Elaboration.
Section Exports.
Variables(b:bundle)(W:A.world).
Fixpoint exports_detached_kind p ns n e(H:A.Exports b W p ns n e){struct H}:
 forall s,ns=A.NSDetached->e=A.ESym s->A.kind_sym W s=Some A.DKMeth.
Proof.
 destruct H as[p s0 d ns n Hd Hm Hf|p m t imp u e ns Hm Hi Hg Hu Ens Hx|p m t imp a Hm Hi Ha];intros s En Es.
 - injection Es as <-. subst ns. unfold A.kind_sym. rewrite Hd. cbn[A.d_ast].
   destruct(A.d_ast d)as[tn|un|an aliasTy|tag ps pay|its ds|vn vt init|rcv fnn ps rt body|cn its|fgn ps args res body];cbn[A.filed_name]in Hf;try discriminate Hf.
   destruct rcv;cbn[A.filed_name]in Hf;try discriminate Hf;reflexivity.
 - subst e. cbn in En. subst ns. destruct Hx as[Hx|[Dot[Hnone Hx]]].
   + exact(exports_detached_kind _ _ _ _ Hx s En eq_refl).
   + rewrite Dot in En. discriminate En.
 - discriminate Es.
Qed.
Lemma scope_detached_kind:LoaderReading b W->forall p n s,
 A.Loaded b p->A.w_scope W p A.NSDetached n=Some(A.ESym s)->A.kind_sym W s=Some A.DKMeth.
Proof.
 intros L p n s Hl Hs. apply(proj1(lr_scope _ _ L p A.NSDetached n(A.ESym s)Hl))in Hs.
 destruct(@A.scope_exports b W p A.NSDetached n(A.ESym s)Hs)as[He|(m&t&imp&Hm&Hi&He)];
 exact(exports_detached_kind _ _ _ _ He s eq_refl eq_refl).
Qed.
End Exports.
Theorem method_named_kind:forall b w R Fl mi r n m,
 load_world b=Good w->In mi(w_order w)->
 A.MethodNamed(W_read w R Fl)(gm_path(get_mod w mi))r n m->
 A.kind_sym(W_read w R Fl)m=Some A.DKMeth.
Proof.
 intros b w R Fl mi r n m Hload Hmi[Ha|[_ Hs]].
 - apply(proj1(lr_attached _ _ (load_world_reads b w R Fl Hload)r n m))in Ha.
   destruct Ha as(d&rn&ps&rt&body&Hd&Hdecl&Hrest).
   unfold A.kind_sym. rewrite Hd. cbn[A.d_ast]. rewrite Hdecl. reflexivity.
 - exact(scope_detached_kind b(W_read w R Fl)(load_world_reads b w R Fl Hload)_ _ _
    (load_world_loaded b w Hload mi Hmi)Hs).
Qed.
Theorem loaded_method_fn:forall w R Fl g,
 nget(w_syms w)(gs_id g)=Some g->A.kind_sym(W_read w R Fl)(gs_id g)=Some A.DKMeth->
 exists rcv n ps rt body,gs_decl g=ADFn rcv n ps rt body.
Proof.
 intros w R Fl g Hg Hm. unfold A.kind_sym in Hm. rewrite(w_decl_read w R Fl _ _ Hg)in Hm. cbn[A.d_ast]in Hm.
 destruct(gs_decl g);cbn[A.kind_of]in Hm;try discriminate Hm. repeat eexists;reflexivity.
Qed.
