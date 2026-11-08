(* Renaming the temporary output-role generations in declarative result reads. *)
From Stdlib Require Import List Bool.
From NM Require Import ExecutionModel TyOfSubst SigReadingSubst ProvideResultSubst ProvideResultExec.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition link_output_entry (sb:subst)(ent:A.entry):A.entry:=
 match ent with
 |A.NTy t=>A.NTy t
 |A.NVal v ty a=>A.NVal v(ExecutionModel.app_subst sb ty)(output_app_subst sb a)
 |A.NFn f sg a=>A.NFn f(A.sig_subst sb sg)(output_app_subst sb a)
 |A.NMth rt m sg a=>A.NMth(ExecutionModel.app_subst sb rt)m(A.sig_subst sb sg)(output_app_subst sb a)
 |A.NFun u fx=>A.NFun u(smap sb fx)
 end.
Lemma link_output_fixed_subst:forall W sb u tm fx,
 A.FixedReadings W u tm fx->forall tm',SubRel sb tm tm'->
 A.FixedReadings W u tm'(smap sb fx).
Proof.
 intros W sb u tm fx Fixed tm' Rel x tx;split.
 - intro Hit;apply in_map_iff in Hit as[[y raw][Eq Hit]].
   injection Eq as <- <-;destruct(proj1(Fixed _ _)Hit)as[Domain Lookup].
   split;[exact Domain|rewrite Rel,Lookup;reflexivity].
 - intros[Domain Lookup];rewrite Rel in Lookup.
   destruct(A.nlook x tm)as[raw|]eqn:Raw;[injection Lookup as <-|discriminate].
   apply in_map_iff;exists(x,raw);split;[reflexivity|apply Fixed;auto].
Qed.
Combined Scheme link_output_mut from output_entry_mut,output_entries_mut.
Lemma link_output_entries_subst_group:forall W sb,
 (forall p ob tm sp es tm',A.OutputItemEntries W p ob tm sp es tm'->
 forall target,SubRel sb tm target->
 A.OutputItemEntries W p(smap sb ob)target sp(map(link_output_entry sb)es)target)/\
 (forall p ob tm specs es tm',A.OutputItemsEntries W p ob tm specs es tm'->
 forall target,SubRel sb tm target->
 A.OutputItemsEntries W p(smap sb ob)target specs(map(link_output_entry sb)es)target).
Proof.
 intros W sb;apply(link_output_mut W
  (fun p ob tm sp es tm'=>forall target,SubRel sb tm target->A.OutputItemEntries W p(smap sb ob)target sp(map(link_output_entry sb)es)target)
  (fun p ob tm specs es tm'=>forall target,SubRel sb tm target->A.OutputItemsEntries W p(smap sb ob)target specs(map(link_output_entry sb)es)target)).
 - intros p ob tm pa a bs t Resolve Kind Br target Rel.
   cbn[map link_output_entry];eapply A.OIEType;[exact Resolve|exact Kind|].
   eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
 - intros p ob tm pa a bs s k Resolve Kind Br Empty target Rel.
   cbn[map];eapply A.OIENothing;[exact Resolve|exact Kind| |exact Empty].
   eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
 - intros p ob tm a pa s d n t ty bs ap Resolve Decl Ast Br Ty App target Rel.
   cbn[map link_output_entry];eapply A.OIEVal;[exact Resolve|exact Decl|exact Ast| | |].
   + eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
   + exact(tyof_subst W sb _ _ _ _ _ Ty _ (subrel_app sb bs _ _ (subrel_app sb ob _ _ Rel))).
   + eapply output_appof_subst;[exact App|apply subrel_app,subrel_app;exact Rel].
 - intros p ob tm a pa s g bs ap d rcv n ps rt Resolve Kind Decl Ast Br Sig App target Rel.
   cbn[map link_output_entry];eapply A.OIEFn;[exact Resolve|exact Kind|exact Decl|exact Ast| | |].
   + eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
   + exact(sigof_subst W sb _ _ _ _ Sig _ (subrel_app sb bs _ _ (subrel_app sb ob _ _ Rel))).
   + eapply output_appof_subst;[exact App|apply subrel_app,subrel_app;exact Rel].
 - intros p ob tm a bs pa u d n ps args res fx Resolve Decl Ast Br Av Fixed target Rel.
   cbn[map link_output_entry];eapply A.OIEFun;[exact Resolve|exact Decl|exact Ast| | |].
   + eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
   + intros x Domain;rewrite(subrel_app sb bs _ _ (subrel_app sb ob _ _ Rel)).
     destruct(A.nlook x(bs++ob++tm))eqn:Lookup;[discriminate|exfalso;exact(Av x Domain Lookup)].
   + eapply link_output_fixed_subst;[exact Fixed|apply subrel_app,subrel_app;exact Rel].
 - intros p ob tm a pa dn r m rt g bs ap Resolve Method Br Recv Sig App target Rel.
   cbn[map link_output_entry];eapply A.OIEMeth;[exact Resolve|exact Method| | | |].
   + eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
   + eapply output_receiver_subst;[exact Recv|apply subrel_app,subrel_app;exact Rel].
   + exact(sigof_subst W sb _ _ _ _ Sig _ (subrel_app sb bs _ _ (subrel_app sb ob _ _ Rel))).
   + eapply output_appof_subst;[exact App|apply subrel_app,subrel_app;exact Rel].
 - intros p ob tm a pa c d n its bs es tm' Resolve Decl Ast Br Entries IH target Rel.
   cbn[map];eapply A.OIECtx;[exact Resolve|exact Decl|exact Ast| |].
   + eapply output_brackets_subst;[exact Br|apply subrel_app;exact Rel].
   + unfold smap in IH|-*;rewrite map_app in IH;apply IH;exact Rel.
 - intros p ob tm target Rel;constructor.
 - intros p ob tm sp rest es1 tm1 es2 tm2 First IH1 Tail IH2 target Rel.
   rewrite map_app;eapply A.OIsCons;[apply IH1;exact Rel|].
   apply IH2;rewrite(output_item_types _ _ _ _ _ _ _ First);exact Rel.
Qed.
Lemma link_output_entries_subst:forall W sb p ob tm specs es tm',
 A.OutputItemsEntries W p ob tm specs es tm'->forall target,SubRel sb tm target->
 A.OutputItemsEntries W p(smap sb ob)target specs(map(link_output_entry sb)es)target.
Proof. intros;exact(proj2(link_output_entries_subst_group W sb)_ _ _ _ _ _ H target H0). Qed.
