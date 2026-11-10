(* A semantic method result retains the written context/bracket route to its leaf. *)
From Stdlib Require Import List String.
From NM Require Import ExecutionModel Sx FamilyResultFrame.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Inductive FamilyOutputLeaf(W:A.world):string->list(nat*ty)->list(nat*ty)->spec->ty->nat->Prop:=
|FOLMethod:forall p ob tm pa dn a r m rt bs,
 A.resolve_sym W p pa=Some r->A.MethodNamed W p r dn m->
 A.OutputBrackets W p(ob++tm)a bs->A.RecvTy W r(bs++ob++tm)rt->
 FamilyOutputLeaf W p ob tm(Spec(Some pa)(Some dn)a)rt m
|FOLContext:forall p ob tm pa a c d n its bs sp rt m,
 A.resolve_sym W p pa=Some c->A.w_decl W c=Some d->A.d_ast d=ADCtx n its->
 A.OutputBrackets W p(ob++tm)a bs->In(CISpec sp)its->
 FamilyOutputLeaf W(A.d_mod d)(bs++ob)tm sp rt m->
 FamilyOutputLeaf W p ob tm(Spec(Some pa)None a)rt m.
Lemma family_output_leaf_origin:
 (forall W p ob tm sp es tm',A.OutputItemEntries W p ob tm sp es tm'->
 tm'=tm/\forall rt m sg ap,In(A.NMth rt m sg ap)es->FamilyOutputLeaf W p ob tm sp rt m)/\
 (forall W p ob tm specs es tm',A.OutputItemsEntries W p ob tm specs es tm'->
 tm'=tm/\forall rt m sg ap,In(A.NMth rt m sg ap)es->
 exists sp,In sp specs/\FamilyOutputLeaf W p ob tm sp rt m).
Proof.
 assert(All:forall W,
 (forall p ob tm sp es tm',A.OutputItemEntries W p ob tm sp es tm'->
 tm'=tm/\forall rt m sg ap,In(A.NMth rt m sg ap)es->FamilyOutputLeaf W p ob tm sp rt m)/\
 (forall p ob tm specs es tm',A.OutputItemsEntries W p ob tm specs es tm'->
 tm'=tm/\forall rt m sg ap,In(A.NMth rt m sg ap)es->exists sp,In sp specs/\FamilyOutputLeaf W p ob tm sp rt m)).
 {intro W;apply(family_output_ind W).
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[Bad|[]];discriminate Bad].
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[]].
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[Bad|[]];discriminate Bad].
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[Bad|[]];discriminate Bad].
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[Bad|[]];discriminate Bad].
 - intros p ob tm a pa dn r m rt sg bs ap Res Named Br Rec Sig App.
   split;[reflexivity|intros rt0 m0 sg0 ap0[Eq|[]];injection Eq as <- <- <- <-;econstructor;eassumption].
 - intros p ob tm a pa c d n its sps bs es tm' Res Decl Ast Esps Br Read[Same IH].
   split;[exact Same|intros rt m sg ap Hin;destruct(IH rt m sg ap Hin)as(sp&Hsp&Leaf);econstructor;try eassumption].
   subst its;apply in_map;exact Hsp.
 - intros;split;[reflexivity|intros rt0 m0 sg0 ap0[]].
 - intros p ob tm sp rest es1 tm1 es2 tm2 Read1[Same1 IH1]Read2[Same2 IH2].
   subst tm1 tm2;split;[reflexivity|intros rt m sg ap Hin;apply in_app_iff in Hin as[Left|Right]].
   + exists sp;split;[left;reflexivity|exact(IH1 rt m sg ap Left)].
   + destruct(IH2 rt m sg ap Right)as(sp0&Hin&Leaf);exists sp0;split;[right;exact Hin|exact Leaf]. }
 split;intros W;[exact(proj1(All W))|exact(proj2(All W))].
Qed.
Theorem provide_result_method_leaf:forall W u C out C' keys rt m,
 A.ProvideResult W u C out C' keys->In(A.mkey rt m)keys->
 exists d ps args results body outs sp raw,
 A.w_decl W u=Some d/\A.functor_parts(A.d_ast d)=Some(ps,args,results,body)/\
 (forall t,In t outs<->A.FunOutputs W u t)/\In sp results/\
 FamilyOutputLeaf W(A.d_mod d)[](A.output_type_map C outs out)sp raw m/\
 A.canon C raw=rt.
Proof.
 intros W u C out C' keys rt m Provide Key.
 destruct Provide as(d&ps&args&results&body&outs&es&tm&Decl&Parts&Outputs&Read&Keys&Final).
 unfold A.ResultKeys in Keys;rewrite Keys in Key;apply in_flat_map in Key as(e&Hin&Key).
 destruct e as[t|v ty ap|f sg ap|raw mid sg ap|h fx|eql eqr];cbn[A.entry_key]in Key;
 try contradiction;try(destruct Key as[Bad|[]];discriminate Bad).
 destruct Key as[Eq|[]];injection Eq as Same Mid;subst mid.
 destruct(proj2 family_output_leaf_origin W(A.d_mod d)[](A.output_type_map C outs out)results es tm Read)
 as[_ Origin];destruct(Origin raw m sg ap Hin)as(sp&Hsp&Leaf).
 exists d,ps,args,results,body,outs,sp,raw;repeat split;try assumption;try congruence.
 all:apply Outputs.
Qed.
