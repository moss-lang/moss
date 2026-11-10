(* A family's semantic result updates method provisions only. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import ExecutionModel Sx.
From NM Require Import AuditFacts.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition FamilyMethodEntry(e:A.entry):Prop:=
 exists r m sg app,e=A.NMth r m sg app.
Scheme family_output_item_ind:=Induction for A.OutputItemEntries Sort Prop
with family_output_items_ind:=Induction for A.OutputItemsEntries Sort Prop.
Combined Scheme family_output_ind from family_output_item_ind,family_output_items_ind.
Lemma family_output_methods:
 (forall W p ob tm sp es tm',A.OutputItemEntries W p ob tm sp es tm'->
    A.FamilyResult W p sp->Forall FamilyMethodEntry es/\tm'=tm)/\
 (forall W p ob tm specs es tm',A.OutputItemsEntries W p ob tm specs es tm'->
    Forall(A.FamilyResult W p)specs->Forall FamilyMethodEntry es/\tm'=tm).
Proof.
 assert(ContextKind:forall W p pa a s k,
   A.resolve_sym W p pa=Some s->A.kind_sym W s=Some k->
   A.FamilyResult W p(Spec(Some pa)None a)->k=A.DKCtx).
 {intros W p pa a s k Res Kind Family;
  inversion Family as[|p0 pa0 a0 u d n its sps ResU DeclU AstU Esps Sub];subst p0 pa0 a0.
  assert(s=u)by congruence;subst u;unfold A.kind_sym in Kind;rewrite DeclU,AstU in Kind;injection Kind;auto. }
 assert(All:forall W,
  (forall p ob tm sp es tm',A.OutputItemEntries W p ob tm sp es tm'->
    A.FamilyResult W p sp->Forall FamilyMethodEntry es/\tm'=tm)/\
  (forall p ob tm specs es tm',A.OutputItemsEntries W p ob tm specs es tm'->
    Forall(A.FamilyResult W p)specs->Forall FamilyMethodEntry es/\tm'=tm)).
 {intros W;apply(family_output_ind W
    (fun p ob tm sp es tm' _=>A.FamilyResult W p sp->Forall FamilyMethodEntry es/\tm'=tm)
    (fun p ob tm specs es tm' _=>Forall(A.FamilyResult W p)specs->Forall FamilyMethodEntry es/\tm'=tm)).
   + intros p ob tm pa a bs t Res Kind Br HF.
     pose proof(ContextKind W p pa a t A.DKType Res Kind HF);discriminate.
   + intros p ob tm pa a bs s k Res Kind Br K HF.
     pose proof(ContextKind W p pa a s k Res Kind HF)as ->.
     destruct K as[Bad|[Bad|[Bad|[[Bad _]|[Bad _]]]]];discriminate Bad.
   + intros p ob tm a pa s d n t ty bs ap Res Decl Ast Br Ty App HF.
     inversion HF;subst;assert(s=u)by congruence;subst u.
     assert(d=d0)by congruence;subst d0;congruence.
   + intros p ob tm a pa s sg bs ap d rcv n ps rt Res Kind Decl Ast Br Sig App HF.
     pose proof(ContextKind W p pa a s A.DKFn Res Kind HF);discriminate.
   + intros p ob tm a bs pa u d n ps args res fx Res Decl Ast Br Av Fixed HF.
     inversion HF;subst;assert(u=u0)by congruence;subst u0.
     assert(d=d0)by congruence;subst d0;congruence.
   + intros p ob tm a pa dn r m rt sg bs ap Res Named Br Recv Sig App HF.
     split;[constructor;[exists rt,m,sg,ap;reflexivity|constructor]|reflexivity].
   + intros p ob tm a pa ctx d n its sps bs es tm' Res Decl Ast Esps Br Read IH HF.
     inversion HF;subst;assert(ctx=u)by congruence;subst u.
     assert(d=d0)by congruence;subst d0.
     assert(Ei:map CISpec sps=map CISpec sps0)by congruence;apply map_CISpec_inj in Ei;subst sps0.
     apply IH;assumption.
   + intros p ob tm H;split;[constructor|reflexivity].
   + intros p ob tm sp rest es1 tm1 es2 tm2 Read1 IH1 Read2 IH2 HF.
     apply Forall_cons_iff in HF as[One Rest].
     destruct(IH1 One)as[Entries1 ->];destruct(IH2 Rest)as[Entries2 ->].
     split;[apply Forall_app;split;assumption|reflexivity]. }
 split;intros W;[exact(proj1(All W))|exact(proj2(All W))].
Qed.
Lemma family_method_flatmap_nil:forall X(f:A.entry->list X)es,
 Forall FamilyMethodEntry es->(forall r m sg app,f(A.NMth r m sg app)=[])->flat_map f es=[].
Proof.
 intros X f es H Empty;induction H as[|e es He H IH];[reflexivity|].
 destruct He as(r&m&sg&app&->);cbn[flat_map];rewrite Empty,IH;reflexivity.
Qed.
Theorem family_result_method_entries:forall W u C out C' ks,
 A.Family W u->A.ProvideResult W u C out C' ks->
 exists tm es,Forall FamilyMethodEntry es/\A.ResultKeys(A.c_sub C)es ks/\C'=A.output_cx C tm es.
Proof.
 intros W u C out C' ks[Shape Keyed]Provide.
 destruct Shape as(d0&n0&ps0&a0&r0&Decl0&Ast&Family).
 destruct Provide as(d&ps&a&r&body&outs&es&tm&Decl&Parts&Out&Read&Keys&Final).
 assert(d0=d)by congruence;subst d0;rewrite Ast in Parts;cbn[A.functor_parts]in Parts.
 injection Parts as <- <- <- <-.
 destruct(proj2 family_output_methods W(A.d_mod d)[](A.output_type_map C outs out)r0 es tm Read Family)as[Entries _].
 exists tm,es;split;[exact Entries|];split;assumption.
Qed.
Theorem family_result_frame:forall W u C out C' ks,
 A.Family W u->A.ProvideResult W u C out C' ks->
 A.c_sub C'=A.c_sub C/\A.c_vals C'=A.c_vals C/\A.c_fns C'=A.c_fns C/\
 A.c_funs C'=A.c_funs C/\A.c_locals C'=A.c_locals C.
Proof.
 intros W u C out C' ks Family Provide.
 destruct(family_result_method_entries W u C out C' ks Family Provide)as(tm&es&Entries&Keys&->).
 unfold A.output_cx;cbn[A.c_sub A.c_vals A.c_fns A.c_funs A.c_locals].
 assert(Rev:Forall FamilyMethodEntry(rev es))by(apply Forall_rev;exact Entries).
 assert(Vempty:flat_map(fun e=>match e with A.NVal v t _=>[(v,A.canon C t)]|_=>[]end)(rev es)=[]).
 {eapply family_method_flatmap_nil;[exact Rev|intros;reflexivity]. }
 assert(Fempty:flat_map(fun e=>match e with A.NFn f sg _=>[(f,A.sig_subst(A.c_sub C)sg)]|_=>[]end)(rev es)=[]).
 {eapply family_method_flatmap_nil;[exact Rev|intros;reflexivity]. }
 assert(Uempty:flat_map(fun e=>match e with A.NFun u fx=>[(u,A.fixed_subst(A.c_sub C)fx)]|_=>[]end)(rev es)=[]).
 {eapply family_method_flatmap_nil;[exact Rev|intros;reflexivity]. }
 rewrite Vempty,Fempty,Uempty;repeat split.
Qed.
Theorem family_restore_frame:forall W u C Ci Cr out ks,
 A.Family W u->A.ProvideResult W u Ci out Cr ks->
 A.c_funs(A.family_restore C Cr)=A.c_funs Ci/\
 A.c_ty(A.family_restore C Cr)=A.c_ty C.
Proof.
 intros;split;[exact(proj1(proj2(proj2(proj2(family_result_frame _ _ _ _ _ _ H H0)))))|reflexivity].
Qed.
(* Any method first found after output installation is one of its slots. *)
From NM Require Import CoreMoss.
Lemma family_new_method_lookup:forall new old r m sg,
 A.mlook r m old=None->A.mlook r m(new++old)=Some sg->
 exists g,In((r,m),g)new.
Proof.
 intros new;induction new as[|[[rr mm]gg]rest IH];intros old r m sg Miss Found.
 - cbn[List.app]in Found;rewrite Miss in Found;discriminate Found.
 - cbn[List.app A.mlook]in Found.
   destruct(ty_eqb r rr&&Nat.eqb m mm)eqn:Eq.
   + apply andb_true_iff in Eq as[Er Em];apply ty_eqb_eq in Er;apply Nat.eqb_eq in Em;subst rr mm.
     exists gg;left;reflexivity.
   + destruct(IH old r m sg Miss Found)as(g&Hg);exists g;right;exact Hg.
Qed.
Theorem result_method_key_new:forall C tm es ks r m sg,
 A.ResultKeys(A.c_sub C)es ks->A.mlook r m(A.c_mths C)=None->
 A.mlook r m(A.c_mths(A.output_cx C tm es))=Some sg->In(A.mkey r m)ks.
Proof.
 intros C tm es ks r m sg Keys Miss Found.
 unfold A.output_cx in Found;cbn[A.c_mths]in Found.
 destruct(family_new_method_lookup _ _ _ _ _ Miss Found)as(g&New).
 apply in_flat_map in New as(e&He&New).
 destruct e as[t|v ty app|f sig app|rt mid sig app|u fx|eql eqr];cbn in New;try contradiction.
 destruct New as[Eq|[]];injection Eq as <- <- <-.
 unfold A.ResultKeys in Keys;rewrite Keys;apply in_flat_map.
 exists(A.NMth rt mid sig app);split.
 - apply in_rev;exact He.
 - cbn[A.entry_key A.mkey];left;reflexivity.
Qed.
