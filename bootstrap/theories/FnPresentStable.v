(* A visible body below the current allocation frontier is never replaced.
   This statement includes generated providers and recursion markers. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import Audit NMap Sx Elab ElabSound ElabFrameCore ElabInv FnReadonly.
Import ListNotations.
Set Implicit Arguments.
Definition fn_present_stable(s s':st):Prop:=forall key body,
 key<st_next s ->nassoc key(st_fns s)=Some body ->nassoc key(st_fns s')=Some body.
Definition PF{A}(m:M A):Prop:=ok m /\forall s a s',Inv s ->m s=Good(a,s')->fn_present_stable s s'.
Definition PF_except(owner:nat){A}(m:M A):Prop:=ok m /\forall s a s',Inv s ->m s=Good(a,s')->
 forall key body,key<>owner ->key<st_next s ->nassoc key(st_fns s)=Some body ->nassoc key(st_fns s')=Some body.
Arguments PF:simpl never.
Arguments PF_except:simpl never.
Lemma pf_keeps:forall A(m:M A),ok m ->keeps st_fns m ->PF m.
Proof. intros A m O K;split;[exact O|]. intros s a s' I E key body B H;rewrite(K _ _ _ E);exact H. Qed.
Lemma pf_ret:forall A(a:A),PF(ret a).
Proof. intros;apply pf_keeps;[apply ok_ret|apply keeps_ret]. Qed.
Lemma pf_bind:forall A B(m:M A)(k:A->M B),PF m ->(forall a,PF(k a))->PF(mbind m k).
Proof.
 intros A B m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I E key body Bound Lookup;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[IM F]. apply(proj2(K a)_ _ _ IM E2 key body).
 - pose proof(fr_next _ _ F);lia.
 - exact(M _ _ _ I E1 key body Bound Lookup).
Qed.
Lemma pf_mmap:forall A B(f:A->M B)xs,(forall x,PF(f x))->PF(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply pf_ret|apply pf_bind;[apply F|intro;apply pf_bind;[exact IHxs|intro;apply pf_ret]]]. Qed.
Lemma pf_miter:forall A(f:A->M unit)xs,(forall x,PF(f x))->PF(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply pf_ret|apply pf_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma pf_mfold:forall A B(f:A->B->M A)xs a,(forall a x,PF(f a x))->PF(mfold f a xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply pf_ret|apply pf_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma pfe_of_pf:forall A owner(m:M A),PF m ->PF_except owner m.
Proof. intros A owner m[O P];split;[exact O|]. intros s a s' I E key body N B H;exact(P _ _ _ I E key body B H). Qed.
Lemma pfe_emit:forall owner body,PF_except owner(emit_fn owner body).
Proof.
 intros owner body;split;[apply ok_emit_fn|]. intros s a s' I E key old N B H.
 unfold emit_fn,upd in E;injection E as <- <-;cbn[s_fns st_fns nassoc assoc].
 destruct(Nat.eqb key owner)eqn:Eq;[apply Nat.eqb_eq in Eq;contradiction|exact H].
Qed.
Lemma pfe_bind:forall A B owner(m:M A)(k:A->M B),
 PF_except owner m ->(forall a,PF_except owner(k a))->PF_except owner(mbind m k).
Proof.
 intros A B owner m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I E key body N Bound Lookup;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[IM F]. apply(proj2(K a)_ _ _ IM E2 key body N).
 - pose proof(fr_next _ _ F);lia.
 - exact(M _ _ _ I E1 key body N Bound Lookup).
Qed.
Lemma pfe_mmap:forall A B owner(f:A->M B)xs,(forall x,PF_except owner(f x))->PF_except owner(mmap f xs).
Proof. intros A B owner f xs F;induction xs;cbn[mmap];[apply pfe_of_pf,pf_ret|apply pfe_bind;[apply F|intro;apply pfe_bind;[exact IHxs|intro;apply pfe_of_pf,pf_ret]]]. Qed.
Lemma pfe_miter:forall A owner(f:A->M unit)xs,(forall x,PF_except owner(f x))->PF_except owner(miter f xs).
Proof. intros A owner f xs F;induction xs;cbn[miter];[apply pfe_of_pf,pf_ret|apply pfe_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma pfe_mfold:forall A B owner(f:A->B->M A)xs a,(forall a x,PF_except owner(f a x))->PF_except owner(mfold f a xs).
Proof. intros A B owner f xs;induction xs;intros acc F;cbn[mfold];[apply pfe_of_pf,pf_ret|apply pfe_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma pf_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource ->kind<>KTag /\kind<>KUnit ->(forall id,PF_except id(k id))->PF(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Origin Kind K;split.
 - apply ok_bind;[apply ok_new_sym;assumption|intro;exact(proj1(K a))].
 - intros s value final I E key body Bound Lookup.
   apply bind_inv in E as[id[mid[New Rest]]].
   destruct(ok_new_sym mi name kind decl orig Origin Kind _ _ _ I New)as[IM F].
   assert(Id:id=st_next s).
   {unfold new_sym in New;apply bind_inv in New as[n[s1[Fresh New]]]. unfold fresh in Fresh;injection Fresh as <- <-.
    apply bind_inv in New as[u[s2[Upd New]]];injection New as <- <-;reflexivity. }
   apply(proj2(K id)_ _ _ IM Rest key body);[lia|pose proof(fr_next _ _ F);lia|].
   unfold new_sym in New;apply bind_inv in New as[n[s1[Fresh New]]]. unfold fresh in Fresh;injection Fresh as <- <-.
   apply bind_inv in New as[u[s2[Upd New]]];unfold upd in Upd;injection Upd as <- <-.
   injection New as <- <-;exact Lookup.
Qed.
