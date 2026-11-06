(* Lambda captures and parameters have disjoint names, so their local-table
   installation order is observationally identical. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound BodySound NominalSupport LambdaCaptureTables.
Import ListNotations.
Local Open Scope list_scope.
Module A:=Audit.Elaboration.
Definition lambda_add_locals(rows:list(string*(ty*A.lmut)))(C:A.cx):A.cx:=
 fold_left(fun C q=>A.add_local C(fst q)(fst(snd q))(snd(snd q)))rows C.
Lemma lambda_locals_shape:forall rows C,
 lambda_add_locals rows C=capture_context_locals C(rev rows++A.c_locals C).
Proof.
 intro rows;induction rows as[|[n[t mu]]rows IH];intro C.
 - destruct C;reflexivity.
 - unfold lambda_add_locals at 1;cbn[fold_left fst snd].
   change(lambda_add_locals rows(A.add_local C n t mu)=
    capture_context_locals C(rev((n,(t,mu))::rows)++A.c_locals C)).
   rewrite IH;cbn[rev].
 rewrite <-app_assoc;destruct C;reflexivity.
Qed.
Lemma lambda_lookup_app:forall V n(l r:list(string*V)),
 A.slook n(l++r)=match A.slook n l with Some v=>Some v|None=>A.slook n r end.
Proof.
 intros V n l;induction l as[|[k v]l IH];intro r;[reflexivity|].
 cbn[A.slook List.app];destruct(String.eqb n k);[reflexivity|exact(IH r)].
Qed.
Lemma lambda_lookup_member:forall V n(l:list(string*V))v,
 A.slook n l=Some v->In n(map fst l).
Proof.
 intros V n l;induction l as[|[k old]l IH];intros v Lookup;[discriminate Lookup|].
 cbn[A.slook]in Lookup;cbn[map fst].
 destruct(String.eqb_spec n k)as[<-|Ne];[left;reflexivity|right;exact(IH _ Lookup)].
Qed.
Lemma lambda_disjoint_order:forall V(k:string)(l r base:list(string*V)),
 (forall n,In n(map fst l)->~In n(map fst r))->
 A.slook k(l++r++base)=A.slook k(r++l++base).
Proof.
 intros V k l r base Disjoint;rewrite !lambda_lookup_app.
 destruct(A.slook k l)as[x|]eqn:L;destruct(A.slook k r)as[y|]eqn:R;try reflexivity.
 exfalso;exact(Disjoint k(lambda_lookup_member _ _ _ _ L)(lambda_lookup_member _ _ _ _ R)).
Qed.
Lemma lambda_local_order:forall rows1 rows2 C k,
 (forall n,In n(map fst rows1)->~In n(map fst rows2))->
 A.slook k(A.c_locals(lambda_add_locals rows1(lambda_add_locals rows2 C)))=
 A.slook k(A.c_locals(lambda_add_locals rows2(lambda_add_locals rows1 C))).
Proof.
 intros rows1 rows2 C k Disjoint;rewrite !lambda_locals_shape.
 cbn[capture_context_locals A.c_locals].
 apply lambda_disjoint_order;intros n Hn Hr.
 rewrite map_rev in Hn,Hr;apply in_rev in Hn;apply in_rev in Hr.
 exact(Disjoint n Hn Hr).
Qed.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySound.W w R Fl).
Lemma lambda_source_locals:forall rows c e F C,
 CxRel w R Fl c e F C->
 Forall(fun q=>WFty(fst(snd q))/\Support W(fst(snd q)))rows->
 CxRel w R Fl c
 (fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))rows e)F
 (lambda_add_locals(map(fun q=>(fst q,(tr Fl(canon e(fst(snd q))),tr_mut(snd(snd q)))))rows)C).
Proof.
 intro rows;induction rows as[|[n[t mu]]rows IH];intros c e F C HC Good;[exact HC|].
 inversion Good as[|q rest[Wt St]Tail];subst.
 unfold lambda_add_locals at 1;cbn[map fold_left fst snd].
 pose proof(cxrel_local w R Fl c e F C n t mu HC Wt St)as Next.
 pose proof(IH c(set_local e n t mu)F(A.add_local C n(tr Fl(canon e t))(tr_mut mu))Next Tail)as Done.
 exact Done.
Qed.
End Context.

Definition lambda_typed_locals(Fl:string->nat)(e:env)(rows:list(string*(ety*lmut))):list(string*(ty*A.lmut)):=
 map(fun q=>(fst q,(tr Fl(canon e(fst(snd q))),tr_mut(snd(snd q)))))rows.
Lemma lambda_typed_names:forall Fl e rows,map fst(lambda_typed_locals Fl e rows)=map fst rows.
Proof. intros;unfold lambda_typed_locals;rewrite map_map;apply map_ext;intros[n[t mu]];reflexivity. Qed.
Lemma lambda_add_locals_app:forall rows1 rows2 C,
 lambda_add_locals(rows1++rows2)C=lambda_add_locals rows2(lambda_add_locals rows1 C).
Proof. intros;unfold lambda_add_locals;rewrite fold_left_app;reflexivity. Qed.
Lemma lambda_relocalize:forall rows1 rows2 C,
 capture_context_locals(lambda_add_locals rows1(lambda_add_locals rows2 C))
  (A.c_locals(lambda_add_locals rows2(lambda_add_locals rows1 C)))=
 lambda_add_locals rows2(lambda_add_locals rows1 C).
Proof. intros;rewrite !lambda_locals_shape;destruct C;reflexivity. Qed.
Theorem lambda_disjoint_source_context:forall w R Fl rows1 rows2 c e F C,
 CxRel w R Fl c e F C->
 Forall(fun q=>WFty(fst(snd q))/\Support(BodySound.W w R Fl)(fst(snd q)))rows1->
 Forall(fun q=>WFty(fst(snd q))/\Support(BodySound.W w R Fl)(fst(snd q)))rows2->
 (forall n,In n(map fst rows1)->~In n(map fst rows2))->
 CxRel w R Fl c
 (fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))rows2
 (fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))rows1 e))F
 (lambda_add_locals(lambda_typed_locals Fl e rows1)(lambda_add_locals(lambda_typed_locals Fl e rows2)C)).
Proof.
 intros w R Fl rows1 rows2 c e F C HC Good1 Good2 Disjoint.
 assert(All:Forall(fun q=>WFty(fst(snd q))/\Support(BodySound.W w R Fl)(fst(snd q)))(rows1++rows2))
 by(apply Forall_app;split;assumption).
 pose proof(lambda_source_locals w R Fl(rows1++rows2)c e F C HC All)as Actual.
 change(CxRel w R Fl c
 (fold_left(fun en q=>set_local en(fst q)(fst(snd q))(snd(snd q)))(rows1++rows2)e)F
 (lambda_add_locals(lambda_typed_locals Fl e(rows1++rows2))C))in Actual.
 unfold lambda_typed_locals in Actual;rewrite map_app,lambda_add_locals_app,fold_left_app in Actual.
 rewrite <-(lambda_relocalize(lambda_typed_locals Fl e rows2)(lambda_typed_locals Fl e rows1)C).
 eapply cxrel_capture_locals;[exact Actual|].
 intro n;apply lambda_local_order;intros k Left Right;rewrite lambda_typed_names in Left,Right;exact(Disjoint k Left Right).
Qed.
