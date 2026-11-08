(* Fresh allocation prevents later functor bodies from shadowing earlier rows. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FinalLinks FunctorReadonly ForwarderTrace.
Import ListNotations.
Set Implicit Arguments.
Local Strategy opaque[Nat.of_num_uint FUEL ELAB_FUEL].
Definition FT{A}(m:M A):Prop:=ok m /\forall s a final,Inv s ->FunctorRows s ->FunctorBound s ->m s=Good(a,final)->FunctorRows final /\FunctorBound final.
Definition FT_at(key:nat){A}(m:M A):Prop:=ok m /\forall s a final,Inv s ->FunctorRows s ->FunctorBound s ->~In key(map fst(st_funs s))->key<st_next s ->m s=Good(a,final)->FunctorRows final /\FunctorBound final.
Arguments FT:simpl never.
Arguments FT_at:simpl never.
Lemma ft_keeps:forall A(m:M A),keeps st_funs m ->ok m ->FT m.
Proof.
 intros A m K O;split;[exact O|]. intros s a final I R B E;split.
 - eapply functor_rows_keeps;[exact(K _ _ _ E)|exact R].
 - eapply functor_bound_keeps;[exact(proj2(O _ _ _ I E))|exact(K _ _ _ E)|exact B].
Qed.
Lemma ft_ret:forall A(a:A),FT(ret a).
Proof. intros;apply ft_keeps;[apply keeps_ret|apply ok_ret]. Qed.
Lemma ft_bind:forall A B(m:M A)(k:A->M B),FT m ->(forall a,FT(k a))->FT(mbind m k).
Proof.
 intros A B m k[O M]K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I R Bnd E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(M _ _ _ I R Bnd E1)as[RM BM].
 exact(proj2(K a)_ _ _(proj1(O _ _ _ I E1))RM BM E2).
Qed.
Lemma ft_mmap:forall A B(f:A->M B)xs,(forall x,FT(f x))->FT(mmap f xs).
Proof. intros A B f xs F;induction xs;cbn[mmap];[apply ft_ret|apply ft_bind;[apply F|intro;apply ft_bind;[exact IHxs|intro;apply ft_ret]]]. Qed.
Lemma ft_miter:forall A(f:A->M unit)xs,(forall x,FT(f x))->FT(miter f xs).
Proof. intros A f xs F;induction xs;cbn[miter];[apply ft_ret|apply ft_bind;[apply F|intro;exact IHxs]]. Qed.
Lemma ft_mfold:forall A B(f:A->B->M A)xs acc,(forall a x,FT(f a x))->FT(mfold f acc xs).
Proof. intros A B f xs;induction xs;intros acc F;cbn[mfold];[apply ft_ret|apply ft_bind;[apply F|intro;apply IHxs;exact F]]. Qed.
Lemma ft_with_vars:forall A init(m:M A),FT m ->FT(with_vars init m).
Proof.
 intros A init m[O K];split;[apply ok_with_vars;exact O|].
 intros s a final I R B E;unfold with_vars in E.
 apply bind_inv in E as[s0[s1[R0 E]]];injection R0 as <- <-.
 apply bind_inv in E as[u[s2[R1 E]]];unfold upd in R1;injection R1 as <- <-.
 apply bind_inv in E as[r[s3[Body E]]].
 apply bind_inv in E as[u'[s4[R2 E]]];unfold upd in R2;injection R2 as <- <-.
 injection E as <- <-.
 assert(IM:Inv(s_vars init(List.length init)s))by(destruct I;constructor;assumption).
 exact(K _ _ _ IM R B Body).
Qed.
Lemma fta_of_ft:forall A key(m:M A),FT m ->FT_at key m.
Proof. intros A key m[O K];split;[exact O|]. intros s a final I R B N L E;exact(K _ _ _ I R B E). Qed.
Lemma fta_bind_quiet:forall A B key(m:M A)(k:A->M B),FT m ->keeps st_funs m ->(forall a,FT_at key(k a))->FT_at key(mbind m k).
Proof.
 intros A B key m k[O M]Quiet K;split;[apply ok_bind;[exact O|intro;exact(proj1(K a))]|].
 intros s value final I R Bnd Absent Below E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(O _ _ _ I E1)as[IM F];destruct(M _ _ _ I R Bnd E1)as[RM BM].
 apply(proj2(K a)mid value final IM RM BM);[rewrite(Quiet _ _ _ E1);exact Absent|pose proof(fr_next _ _ F);lia|exact E2].
Qed.
Lemma fta_emit:forall key code,FT_at key(upd(s_funs(fun rows=>(key,code)::rows))).
Proof.
 intros key code;split;[okt|]. intros s a final I R B N L E;unfold upd in E;injection E as <- <-;split.
 - apply functor_rows_emit;assumption.
 - apply functor_bound_emit;assumption.
Qed.
Lemma fta_emit_bind:forall A key code(k:unit->M A),
 (forall a,FT(k a))->FT_at key(mbind(upd(s_funs(fun rows=>(key,code)::rows)))k).
Proof.
 intros A key code k K;split;[apply ok_bind;[exact(proj1(fta_emit key code))|intro;exact(proj1(K a))]|].
 intros s value final I R B N L E;apply bind_inv in E as[a[mid[E1 E2]]].
 destruct(proj1(fta_emit key code)_ _ _ I E1)as[IM F].
 destruct(proj2(fta_emit key code)_ _ _ I R B N L E1)as[RM BM].
 exact(proj2(K a)_ _ _ IM RM BM E2).
Qed.
Lemma ft_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource ->kind<>KTag /\kind<>KUnit ->(forall id,FT_at id(k id))->FT(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Origin Kind K;split;[apply ok_bind;[apply ok_new_sym;assumption|intro;exact(proj1(K a))]|].
 intros s value final I R B E;apply bind_inv in E as[id[mid[New Rest]]].
 destruct(ok_new_sym mi name kind decl orig Origin Kind _ _ _ I New)as[IM F].
 destruct(new_sym_shape _ _ _ _ _ _ _ _ New)as[Id[Next Lookup]].
 assert(Quiet:st_funs mid=st_funs s).
 {assert(Q:keeps st_funs(new_sym mi name kind decl orig))by(unfold new_sym;keepst);exact(Q _ _ _ New). }
 assert(RM:FunctorRows mid)by(eapply functor_rows_keeps;eassumption).
 assert(BM:FunctorBound mid)by(eapply functor_bound_keeps;eassumption).
 apply(proj2(K id)mid value final IM RM BM).
 - rewrite Quiet;apply functor_bound_absent;[exact B|lia].
 - rewrite Next;lia.
 - exact Rest.
Qed.
