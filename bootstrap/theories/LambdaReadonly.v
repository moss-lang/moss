(* Existing lambda owners, including pending owners without a body yet,
   retain their function-table lookup throughout nested source elaboration. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrame ElabInv FnReadonly ForwarderHistory.
Import ListNotations.
Set Implicit Arguments.
Definition lambda_owner_rows(key:nat)(s:st):list(nat*xexpr):=
 filter(fun p=>Nat.eqb key(fst p))(st_fns s).
Definition lambda_rows_stable(s final:st):Prop:=forall key owner,
 nget(st_syms s)key=Some owner->gs_origin owner=OLambda->
 lambda_owner_rows key final=lambda_owner_rows key s.
Definition LR{A}(m:M A):Prop:=ok m/\forall s a final,Inv s->m s=Good(a,final)->lambda_rows_stable s final.
Definition LR_except(owner:nat){A}(m:M A):Prop:=ok m/\forall s a final,Inv s->m s=Good(a,final)->
 forall key g,key<>owner->nget(st_syms s)key=Some g->gs_origin g=OLambda->
 lambda_owner_rows key final=lambda_owner_rows key s.
Arguments LR:simpl never.
Arguments LR_except:simpl never.
Lemma lr_keeps:forall A(m:M A),ok m->keeps st_fns m->LR m.
Proof. intros A m O K;split;[exact O|]. intros s a final I E key g Lookup Origin;unfold lambda_owner_rows;rewrite(K _ _ _ E);reflexivity. Qed.
Lemma lr_ret:forall A(a:A),LR(ret a).
Proof. intros;apply lr_keeps;[apply ok_ret|apply keeps_ret]. Qed.
Lemma lr_bind:forall A B(m:M A)(k:A->M B),LR m->(forall a,LR(k a))->LR(mbind m k).
Proof.
 intros A B m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I E key g Lookup Origin;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[Im F].
 rewrite(proj2(K a)_ _ _ Im E2 key g(fr_keep _ _ F _ _ Lookup)Origin).
 exact(M _ _ _ I E1 key g Lookup Origin).
Qed.
Lemma lr_mmap:forall A B(f:A->M B)xs,(forall x,LR(f x))->LR(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply lr_ret|apply lr_bind;[apply F|intro;apply lr_bind;[exact IHxs|intro;apply lr_ret]]]. Qed.
Lemma lr_miter:forall A(f:A->M unit)xs,(forall x,LR(f x))->LR(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply lr_ret|apply lr_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lr_mfold:forall A B(f:A->B->M A)xs a,(forall a x,LR(f a x))->LR(mfold f a xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply lr_ret|apply lr_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma lre_of_lr:forall A owner(m:M A),LR m->LR_except owner m.
Proof. intros A owner m[O M];split;[exact O|]. intros s a final I E key g Ne Lookup Origin;exact(M _ _ _ I E key g Lookup Origin). Qed.
Lemma lre_emit:forall owner body,LR_except owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply ok_emit_fn|]. intros s a final I E key g Ne Lookup Origin.
 unfold emit_fn,upd in E;injection E as <- <-;cbn[lambda_owner_rows s_fns st_fns filter fst].
 destruct(Nat.eqb key owner)eqn:Eq;[apply Nat.eqb_eq in Eq;contradiction|reflexivity].
Qed.
Lemma lre_bind:forall A B owner(m:M A)(k:A->M B),
 LR_except owner m->(forall a,LR_except owner(k a))->LR_except owner(mbind m k).
Proof.
 intros A B owner m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I E key g Ne Lookup Origin;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[Im F].
 rewrite(proj2(K a)_ _ _ Im E2 key g Ne(fr_keep _ _ F _ _ Lookup)Origin).
 exact(M _ _ _ I E1 key g Ne Lookup Origin).
Qed.
Lemma lre_mmap:forall A B owner(f:A->M B)xs,(forall x,LR_except owner(f x))->LR_except owner(mmap f xs).
Proof. intros A B owner f xs F;induction xs;cbn[mmap];[apply lre_of_lr,lr_ret|apply lre_bind;[apply F|intro;apply lre_bind;[exact IHxs|intro;apply lre_of_lr,lr_ret]]]. Qed.
Lemma lre_miter:forall A owner(f:A->M unit)xs,(forall x,LR_except owner(f x))->LR_except owner(miter f xs).
Proof. intros A owner f xs F;induction xs;cbn[miter];[apply lre_of_lr,lr_ret|apply lre_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma lre_mfold:forall A B owner(f:A->B->M A)xs a,(forall a x,LR_except owner(f a x))->LR_except owner(mfold f a xs).
Proof. intros A B owner f xs;induction xs;intros acc F;cbn[mfold];[apply lre_of_lr,lr_ret|apply lre_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma lr_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource->kind<>KTag/\kind<>KUnit->(forall id,LR_except id(k id))->LR(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Origin Kind K;split.
 - apply ok_bind;[apply ok_new_sym;assumption|intro;exact(proj1(K a))].
 - intros s value final I E key g Lookup Lambda.
   apply bind_inv in E as[id[mid[New Rest]]].
   destruct(ok_new_sym mi name kind decl orig Origin Kind _ _ _ I New)as[Im F].
   assert(Id:id=st_next s).
   {unfold new_sym in New;apply bind_inv in New as[n[s1[Fresh New]]];unfold fresh in Fresh;injection Fresh as <- <-.
    apply bind_inv in New as[u[s2[Upd New]]];injection New as <- <-;reflexivity. }
   assert(Ne:key<>id)by(pose proof(@symbol_below_next s key g I Lookup);lia).
   rewrite(proj2(K id)_ _ _ Im Rest key g Ne(fr_keep _ _ F _ _ Lookup)Lambda).
   assert(Knew:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst).
   unfold lambda_owner_rows;rewrite(Knew _ _ _ New);reflexivity.
Qed.
