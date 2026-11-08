(* The output tables are a sequential, last-occurrence-wins update. *)
From Stdlib Require Import List.
From NM Require Import ExecutionModel.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Lemma output_cx_append:forall C tm es1 es2,
 A.output_cx C tm(es1++es2)=A.output_cx(A.output_cx C tm es1)tm es2.
Proof.
 intros;unfold A.output_cx,A.canon;
 cbn[A.c_sub A.c_ty A.c_vals A.c_fns A.c_mths A.c_funs A.c_apps A.c_locals].
 rewrite rev_app_distr;repeat rewrite flat_map_app;repeat rewrite app_assoc;reflexivity.
Qed.
Lemma output_cx_type:forall C tm t,
 A.output_cx C tm[A.NTy t]=A.output_cx C tm[].
Proof. reflexivity. Qed.
Lemma output_cx_value:forall C tm v t a,
 A.output_cx C tm[A.NVal v t a]=
 A.provide C(IVal v)a(A.set_val(A.output_cx C tm[])v(A.canon C t)).
Proof. reflexivity. Qed.
Lemma output_cx_function:forall C tm f sg a,
 A.output_cx C tm[A.NFn f sg a]=
 A.provide C(IFn f)a(A.set_fn(A.output_cx C tm[])f(A.sig_subst(A.c_sub C)sg)).
Proof. reflexivity. Qed.
Lemma output_cx_method:forall C tm r m sg a,
 A.output_cx C tm[A.NMth r m sg a]=
 A.provide C(A.mkey(A.canon C r)m)a
 (A.set_mth(A.output_cx C tm[])(A.canon C r)m(A.sig_subst(A.c_sub C)sg)).
Proof. reflexivity. Qed.
Lemma output_cx_functor:forall C tm u fx,
 A.output_cx C tm[A.NFun u fx]=
 A.set_fun(A.output_cx C tm[])u(A.fixed_subst(A.c_sub C)fx).
Proof. reflexivity. Qed.

Lemma output_type_map_override:forall C t rest out,
 A.nlook t(A.output_type_map C(t::rest)out)=Some(A.canon C(out t)).
Proof. intros;unfold A.output_type_map;cbn[map app A.nlook];now rewrite PeanoNat.Nat.eqb_refl. Qed.
(* The old input equation for role 1 must not erase its new generation 2. *)
Example output_role_precedes_old_merge:
 let C:=A.Cx[(1,TUnit)][(1,TUnit)][][][][][][]in
 A.nlook 1(A.output_type_map C[1](fun _=>TAbs 2))=Some(TAbs 2) /\
 A.canon C(TAbs 1)=TUnit.
Proof. split;reflexivity. Qed.
