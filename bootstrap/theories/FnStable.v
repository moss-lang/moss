(* The elaborator's nested helpers never replace a visible loader function.
   Loaded lower_body calls are handled separately by their completed run. *)
From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel NMap Sx Elab ElabSound ElabFrame ElabInv FnHistory FnReadonly.
Import ListNotations.
Set Implicit Arguments.

Definition fn_stable(w:Elab.world)(s s':st):Prop:=
 forall f,loaded_fn_key w f ->nassoc f(st_fns s')=nassoc f(st_fns s).
Lemma fn_stable_refl:forall w s,fn_stable w s s.
Proof. intros w s f Loaded;reflexivity. Qed.
Lemma fn_stable_trans:forall w a b c,
 fn_stable w a b ->fn_stable w b c ->fn_stable w a c.
Proof. intros w a b c AB BC f Loaded;rewrite(BC f Loaded);apply AB;exact Loaded. Qed.
Lemma fn_stable_history:forall w a b,fn_stable w a b ->fn_history w a b.
Proof. intros w a b Stable f body Loaded Lookup;left;rewrite <-(Stable f Loaded);exact Lookup. Qed.
Lemma fn_stable_emit:forall w s key body,~loaded_fn_key w key ->
 fn_stable w s(s_fns(fun fs=>(key,body)::fs)s).
Proof.
 intros w s key body Fresh f Loaded;cbn[s_fns st_fns nassoc assoc].
 destruct(Nat.eqb f key)eqn:Same;[apply Nat.eqb_eq in Same;subst f;contradiction|reflexivity].
Qed.

Definition NF{A}(m:M A):Prop:=ok m /\forall b s a s',
 Inv s ->load_world b=Good(st_w s)->m s=Good(a,s')->fn_stable(st_w s)s s'.
Definition NF_unloaded(key:nat){A}(m:M A):Prop:=ok m /\forall b s a s',
 Inv s ->load_world b=Good(st_w s)->~loaded_fn_key(st_w s)key ->
 m s=Good(a,s')->fn_stable(st_w s)s s'.
Arguments NF:simpl never.
Arguments NF_unloaded:simpl never.

Lemma nf_keeps:forall A(m:M A),ok m ->keeps st_fns m ->NF m.
Proof.
 intros A m OK Keep;split;[exact OK|].
 intros b s a s' InvS Load Run f Loaded;rewrite(Keep s a s' Run);reflexivity.
Qed.
Lemma nf_ret:forall A(a:A),NF(ret a).
Proof. intros;apply nf_keeps;[apply ok_ret|apply keeps_ret]. Qed.
Lemma nf_bind:forall A B(m:M A)(k:A->M B),NF m ->(forall a,NF(k a))->NF(mbind m k).
Proof.
 intros A B m k[OKm Nm]K;split.
 - apply ok_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value s'' InvS Load Run.
   apply bind_inv in Run as[a[s'[Rm Rk]]].
   destruct(OKm s a s' InvS Rm)as[InvMid Frame].
   pose proof(Nm b s a s' InvS Load Rm)as First.
   assert(LoadMid:load_world b=Good(st_w s'))by(rewrite(fr_w _ _ Frame);exact Load).
   pose proof(proj2(K a)b s' value s'' InvMid LoadMid Rk)as Last.
   rewrite(fr_w _ _ Frame)in Last;eapply fn_stable_trans;eassumption.
Qed.
Lemma nf_mmap:forall A B(f:A->M B)xs,(forall x,NF(f x))->NF(mmap f xs).
Proof.
 intros A B f xs F;induction xs as[|x xs IH];cbn[mmap];[apply nf_ret|].
 apply nf_bind;[apply F|intro y;apply nf_bind;[exact IH|intro ys;apply nf_ret]].
Qed.
Lemma nf_miter:forall A(f:A->M unit)xs,(forall x,NF(f x))->NF(miter f xs).
Proof.
 intros A f xs F;induction xs as[|x xs IH];cbn[miter];[apply nf_ret|].
 apply nf_bind;[apply F|intro;exact IH].
Qed.
Lemma nf_mfold:forall A B(f:A->B->M A)xs a,
 (forall a x,NF(f a x))->NF(mfold f a xs).
Proof.
 intros A B f xs;induction xs as[|x xs IH];intros a F;cbn[mfold];[apply nf_ret|].
 apply nf_bind;[apply F|intro a';apply IH;exact F].
Qed.
Lemma nf_unloaded_of_nf:forall A key(m:M A),NF m ->NF_unloaded key m.
Proof. intros A key m[OK N];split;[exact OK|];intros;eapply N;eassumption. Qed.

(* Source symbols are strictly below the allocation counter. A fresh wrapper
   or lambda can therefore emit its body without shadowing any source body. *)
Lemma nf_emit_unloaded:forall key body,NF_unloaded key(emit_fn key body).
Proof.
 intros key body;split;[apply ok_emit_fn|].
 intros b s a s' InvS Load Fresh Run;unfold emit_fn,upd in Run;injection Run as <- <-.
 apply fn_stable_emit;exact Fresh.
Qed.

Lemma nfu_bind:forall A B key(m:M A)(k:A->M B),
 NF_unloaded key m ->(forall a,NF_unloaded key(k a))->NF_unloaded key(mbind m k).
Proof.
 intros A B key m k[OKm Nm]K;split.
 - apply ok_bind;[exact OKm|intro a;exact(proj1(K a))].
 - intros b s value s'' InvS Load Fresh Run.
   apply bind_inv in Run as[a[s'[Rm Rk]]].
   destruct(OKm s a s' InvS Rm)as[InvMid Frame].
   pose proof(Nm b s a s' InvS Load Fresh Rm)as First.
   assert(LoadMid:load_world b=Good(st_w s'))by(rewrite(fr_w _ _ Frame);exact Load).
   assert(FreshMid:~loaded_fn_key(st_w s')key)by(rewrite(fr_w _ _ Frame);exact Fresh).
   pose proof(proj2(K a)b s' value s'' InvMid LoadMid FreshMid Rk)as Last.
   rewrite(fr_w _ _ Frame)in Last;eapply fn_stable_trans;eassumption.
Qed.
Lemma nfu_mmap:forall A B key(f:A->M B)xs,
 (forall x,NF_unloaded key(f x))->NF_unloaded key(mmap f xs).
Proof.
 intros A B key f xs F;induction xs as[|x xs IH];cbn[mmap].
 - apply nf_unloaded_of_nf,nf_ret.
 - apply nfu_bind;[apply F|intro y;apply nfu_bind;[exact IH|intro ys;apply nf_unloaded_of_nf,nf_ret]].
Qed.
Lemma nfu_miter:forall A key(f:A->M unit)xs,
 (forall x,NF_unloaded key(f x))->NF_unloaded key(miter f xs).
Proof.
 intros A key f xs F;induction xs as[|x xs IH];cbn[miter].
 - apply nf_unloaded_of_nf,nf_ret.
 - apply nfu_bind;[apply F|intro;exact IH].
Qed.
Lemma nfu_mfold:forall A B key(f:A->B->M A)xs a,
 (forall a x,NF_unloaded key(f a x))->NF_unloaded key(mfold f a xs).
Proof.
 intros A B key f xs;induction xs as[|x xs IH];intros a F;cbn[mfold].
 - apply nf_unloaded_of_nf,nf_ret.
 - apply nfu_bind;[apply F|intro a';apply IH;exact F].
Qed.

(* Fresh IDs are outside the loader even after the symbol itself has been
   installed in the mutable symbol map. The loader's world is unchanged. *)
Lemma new_sym_unloaded:forall b mi name kind decl orig s key s',
 Inv s ->load_world b=Good(st_w s)->
 new_sym mi name kind decl orig s=Good(key,s')->
 key=st_next s /\st_w s'=st_w s /\~loaded_fn_key(st_w s')key.
Proof.
 intros b mi name kind decl orig s key s' InvS Load Run.
 unfold new_sym in Run;apply bind_inv in Run as[id[s1[Fresh Run]]].
 unfold fresh in Fresh;injection Fresh as <- <-.
 apply bind_inv in Run as[u[s2[Upd Run]]];unfold upd in Upd;injection Upd as <- <-.
 injection Run as <- <-;split;[reflexivity|]. split;[reflexivity|].
 change(~loaded_fn_key(st_w s)(st_next s));intro Loaded.
 pose proof(@loaded_fn_below_next b s(st_next s)Load InvS Loaded);lia.
Qed.

Lemma nf_new_sym_bind:forall A mi name kind decl orig(k:nat->M A),
 orig<>OSource ->kind<>KTag /\kind<>KUnit ->
 (forall id,NF_unloaded id(k id))->NF(mbind(new_sym mi name kind decl orig)k).
Proof.
 intros A mi name kind decl orig k Origin Kind K;split.
 - apply ok_bind;[apply ok_new_sym;assumption|intro id;exact(proj1(K id))].
 - intros b s value final InvS Load Run.
   apply bind_inv in Run as[id[mid[Make Rest]]].
   destruct(ok_new_sym mi name kind decl orig Origin Kind _ _ _ InvS Make)as[InvMid Frame].
   destruct(@new_sym_unloaded b mi name kind decl orig s id mid InvS Load Make)
     as[Id[World Fresh]].
   assert(LoadMid:load_world b=Good(st_w mid))by(rewrite World;exact Load).
   pose proof(proj2(K id)b mid value final InvMid LoadMid Fresh Rest)as Stable.
   rewrite World in Stable.
   eapply fn_stable_trans;[|exact Stable].
   assert(Kept:keeps st_fns(new_sym mi name kind decl orig))by(unfold new_sym;keepst).
   intros f Loaded;rewrite(Kept s id mid Make);reflexivity.
Qed.

Lemma nfu_bind_sym:forall A owner id(k:gsym->M A),
 (forall g,gs_id g=id ->NF_unloaded owner(k g))->NF_unloaded owner(mbind(sym id)k).
Proof.
 intros A owner id k K;split.
 - intros s value final InvS Run;apply bind_inv in Run as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate].
   injection Read as <- <-.
   exact(proj1(K got(iv_ids s InvS id got Lookup))s value final InvS Rest).
 - intros b s value final InvS Load Fresh Run;apply bind_inv in Run as[g[mid[Read Rest]]].
   unfold sym in Read;destruct(nget(st_syms s)id)as[got|]eqn:Lookup;[|discriminate].
   injection Read as <- <-.
   exact(proj2(K got(iv_ids s InvS id got Lookup))b s value final InvS Load Fresh Rest).
Qed.
