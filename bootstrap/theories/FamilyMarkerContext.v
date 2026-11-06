(* Actual marked environments and the specialization's unchanged caller meanings. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound CoreMoss BodySound BodyTr BindType
 CanonTypes NominalSupport FamilyMarkersExec FamilyMarkedSubst.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition family_mark_env_step(e:env)(q:nat*nat):M env:=
 let! stamp:=fresh in ret(set_tn e(fst q)(YAbs(snd q))stamp).
Lemma reify_scope_tn_value:forall e t raw stamp,
 In(t,(raw,stamp))(en_tn e)->In raw(reify_scope_types e).
Proof.
 intros e t raw stamp Hin;unfold reify_scope_types;apply in_app_iff;left.
 unfold tymap;rewrite map_map;apply in_map_iff;exists(t,(raw,stamp));split;[reflexivity|exact Hin].
Qed.
Lemma marker_subst_fixes_scope:forall Fl e receiver ps s marks sf target,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->
 forall raw,In raw(reify_scope_types e)->
 Audit.app_subst(family_marker_subst marks target)(tr Fl(canon e raw))=tr Fl(canon e raw).
Proof.
 intros Fl e receiver ps s marks sf target Alloc raw Hraw;apply app_subst_none.
 intros g Gens;apply lookup_subst_none;intro InKeys.
 unfold family_marker_subst in InKeys;rewrite map_map in InKeys.
 change(In g(map snd marks))in InKeys;apply in_map_iff in InKeys as([p g0]&Eq&Hin).
 cbn[snd]in Eq;subst g0.
 change(In g(A.gens(tr Fl(canon e raw))))in Gens;rewrite gens_tr in Gens.
 pose proof(family_markers_avoid_canonical_caller e receiver ps s marks sf Alloc p g Hin raw Hraw g Gens);lia.
Qed.
Section Marked.
Variable w:Elab.world.
Variables(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma family_mark_env_at:forall marks c e0 e F C s em sf,
 mfold family_mark_env_step e marks s=Good(em,sf)->en_uf e=en_uf e0->CxRel w R Fl c e F C->
 (forall p g,In(p,g)marks->canon e0(YAbs g)=YAbs g)->
 en_uf em=en_uf e0/\
 CxRel w R Fl c em F(fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))marks C).
Proof.
 intros marks;induction marks as[|[p g]rest IH];intros c e0 e F C s em sf Install Uf HC Fixed.
 - injection Install as <- <-;split;assumption.
 - cbn[mfold]in Install;apply bind_inv in Install as(en&sn&Head&Tail).
   unfold family_mark_env_step in Head;cbn[fst snd]in Head.
   apply bind_inv in Head as(stamp&st&Fresh&Ret);injection Ret as <- <-.
   cbn[fold_left fst snd].
   assert(Can:canon e(YAbs g)=YAbs g).
   {rewrite(canon_uf e0 e Uf);apply(Fixed p g);left;reflexivity. }
   assert(HC1:CxRel w R Fl c(set_tn e p(YAbs g)stamp)F(A.set_ty C p(TAbs g))).
   {pose proof(cxrel_type w R Fl c e F C p(YAbs g)stamp HC eq_refl(@SupportAbs W g))as H.
    rewrite Can in H;exact H. }
   eapply IH;[exact Tail|exact Uf|exact HC1|].
   intros p0 g0 Hin;apply(Fixed p0 g0);right;exact Hin.
Qed.
Theorem family_marked_context:forall c e receiver F C ps s marks sm em sf,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sm)->
 mfold family_mark_env_step e marks sm=Good(em,sf)->CxRel w R Fl c e F C->
 en_uf em=en_uf e/\
 CxRel w R Fl c em F(fold_left(fun C q=>A.set_ty C(fst q)(TAbs(snd q)))marks C).
Proof.
 intros c e receiver F C ps s marks sm em sf Alloc Install HC.
 eapply family_mark_env_at;[exact Install|reflexivity|exact HC|].
 intros p g Hin;exact(proj2(proj2(family_markers_avoid_caller e receiver ps s marks sm Alloc p g Hin))).
Qed.
Theorem family_marker_subst_fixes_caller:forall c e receiver F C ps s marks sf target,
 mmap(fun p=>let!k:=fresh_marker_for e receiver in ret(p,k))ps s=Good(marks,sf)->CxRel w R Fl c e F C->
 forall k y,A.nlook k(A.c_ty C)=Some y->Audit.app_subst(family_marker_subst marks target)y=y.
Proof.
 intros c e receiver F C ps s marks sf target Alloc HC k y Have.
 pose proof(cr_ty _ _ _ _ _ _ _ HC k)as Read;rewrite Have in Read.
 destruct(tn_get e k)as[raw|]eqn:Raw;[|discriminate Read].
 cbn beta in Read;cbn[option_map]in Read;injection Read as ->.
 eapply marker_subst_fixes_scope;[exact Alloc|].
 unfold tn_get in Raw;destruct(nassoc k(en_tn e))as[[v stamp]|]eqn:Look;[|discriminate Raw].
 injection Raw as <-;eapply reify_scope_tn_value;eapply support_nassoc_in;exact Look.
Qed.
End Marked.
