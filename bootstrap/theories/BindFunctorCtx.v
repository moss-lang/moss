(* Installing a functor retains its actual fixed-input snapshot. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit NMap Sx Elab ElabSound WorldSound ElabFrameCore BodySoundCore BodyTr NominalSupport NominalMergeSupport FunctorDecl.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition fun_tr_inst(Fl:string->nat)(a:Elab.inst):A.inst:=
 map(fun q=>(fst q,option_map(tr Fl)(snd q)))a.
Lemma fun_inst_lookup:forall Fl a x,
 nassoc x(fun_tr_inst Fl a)=option_map(option_map(tr Fl))(nassoc x a).
Proof.
 intros Fl a;induction a as[|[k v]a IH];intro x;[reflexivity|].
 cbn[nassoc assoc fun_tr_inst map fst snd];destruct(Nat.eqb x k);[reflexivity|exact(IH x)].
Qed.
Lemma fixed_app_lookup:forall fx x,
 nassoc x(A.fixed_app fx)=option_map Some(A.nlook x fx).
Proof.
 intros fx;induction fx as[|[k v]fx IH];intro x;[reflexivity|].
 cbn[nassoc assoc A.fixed_app A.nlook map fst snd];destruct(Nat.eqb x k);[reflexivity|exact(IH x)].
Qed.
Lemma nlook_unique_member:forall(fx:list(nat*ty))x tx,
 NoDup(map fst fx)->In(x,tx)fx->A.nlook x fx=Some tx.
Proof.
 intros fx;induction fx as[|[k y]fx IH];intros x tx Nd Hin;[contradiction|].
 inversion Nd as[|v vs Absent Rest];subst v vs.
 cbn[A.nlook]. destruct(Nat.eqb x k)eqn:E.
 - apply Nat.eqb_eq in E;subst k. destruct Hin as[Eq|Hin];[injection Eq as <-;reflexivity|].
   exfalso;apply Absent;apply in_map_iff;exists(x,tx);auto.
 - destruct Hin as[Eq|Hin];[injection Eq as -> _;rewrite Nat.eqb_refl in E;discriminate|auto].
Qed.
Lemma fun_snapshot_member:forall Fl a fx x tx,
 fun_tr_inst Fl a=A.fixed_app fx->NoDup(map fst fx)->In(x,tx)fx->
 exists raw,nassoc x a=Some(Some raw)/\tx=tr Fl raw.
Proof.
 intros Fl a fx x tx Apps Nd Hin.
 pose proof(nlook_unique_member fx x tx Nd Hin)as Look.
 pose proof(fixed_app_lookup fx x)as F;rewrite Look in F.
 rewrite<-Apps,fun_inst_lookup in F.
 destruct(nassoc x a)as[[raw|]|]eqn:E;cbn[option_map]in F;try discriminate.
 injection F as Eq;exists raw;auto.
Qed.
Lemma fun_key_eqb_spec:forall k k',key_eqb k k'=true<->k=k'.
Proof.
 intros k k';split.
 - destruct k as[x|x|t m|x],k' as[y|y|t' m'|y];cbn;intro H;try discriminate;
   try(apply Nat.eqb_eq in H;subst;reflexivity).
   apply andb_true_iff in H as[H1 H2];apply Nat.eqb_eq in H1;apply ety_eqb_true in H2;subst;reflexivity.
 - intros<-;destruct k as[x|x|t m|x];cbn;rewrite ?Nat.eqb_refl,?ety_eqb_refl;reflexivity.
Qed.
Lemma fun_app_other:forall e u stamp a sa k,
 key_eqb k(KyFun u)=false->
 get_app(set_app(set_fun e u stamp)(KyFun u)a sa)k=get_app e k.
Proof.
 intros e u stamp a sa k Ne;unfold get_app,set_app,set_fun;cbn[en_apps].
 rewrite support_assoc_aset by apply fun_key_eqb_spec.
 rewrite Ne;reflexivity.
Qed.
Lemma fun_app_same:forall e u stamp a sa,
 get_app(set_app(set_fun e u stamp)(KyFun u)a sa)(KyFun u)=Some(a,sa).
Proof.
 intros;unfold get_app,set_app,set_fun;cbn[en_apps].
 rewrite support_assoc_aset by apply fun_key_eqb_spec.
 rewrite (proj2(fun_key_eqb_spec _ _)eq_refl);reflexivity.
Qed.
Lemma fun_env_support_app:forall W e k a stamp,
 EnvSupport W e->EnvSupport W(set_app e k a stamp).
Proof. intros W e k a stamp[T U V F M L];constructor;auto. Qed.
Section Context.
Variables(w:Elab.world)(R:nat->list nat)(Fl:string->nat).
Notation W:=(BodySoundCore.W w R Fl).
Theorem cxrel_functor:forall c e F C u stamp a sa fx,
 CxRel w R Fl c e F C->FunctorDecl W u->NoDup(map fst fx)->
 (forall x tx,In(x,tx)fx->A.Fixed W u x)->fun_tr_inst Fl a=A.fixed_app fx->
 CxRel w R Fl c(set_app(set_fun e u stamp)(KyFun u)a sa)F(A.set_fun C u fx).
Proof.
 intros c e F C u stamp a sa fx H Decl Nd Fixed Apps.
 pose proof(cr_apps _ _ _ _ _ _ _ H)as Ma.
 pose proof(cr_vapps _ _ _ _ _ _ _ H)as Va.
 pose proof(cr_fapps _ _ _ _ _ _ _ H)as Fa.
 pose proof(cr_funapps _ _ _ _ _ _ _ H)as Ua.
 pose proof(cr_funspresent _ _ _ _ _ _ _ H)as Fu.
 pose proof(cr_funfixed _ _ _ _ _ _ _ H)as Fx.
 pose proof(cr_support _ _ _ _ _ _ _ H)as Support.
 pose proof(cr_fun_decls _ _ _ _ _ _ _ H)as DeclOld.
 destruct H;constructor;auto.
 - apply fun_env_support_app;apply env_support_fun;exact Support.
 - intros r m Clean;cbn[A.set_fun A.c_apps A.ilook A.mkey Audit.item_eqb].
   rewrite fun_app_other by reflexivity;exact(Ma r m Clean).
 - intro v;cbn[A.set_fun A.c_apps A.ilook Audit.item_eqb].
   rewrite fun_app_other by reflexivity;exact(Va v).
 - intro f;cbn[A.set_fun A.c_apps A.ilook Audit.item_eqb].
   rewrite fun_app_other by reflexivity;exact(Fa f).
 - intro v;cbn[A.set_fun A.c_apps A.ilook Audit.item_eqb].
   destruct(Nat.eqb v u)eqn:E;[apply Nat.eqb_eq in E;subst v;rewrite fun_app_same;cbn[option_map fst];f_equal;exact(eq_sym Apps)|].
   rewrite fun_app_other by exact E;exact(Ua v).
 - intro v;cbn[A.set_fun A.c_funs A.nlook set_app set_fun en_funs].
   rewrite nget_nset;destruct(Nat.eqb v u);[reflexivity|exact(Fu v)].
 - intros v fxs x tx Lookup Hin;cbn[A.set_fun A.c_funs A.nlook]in Lookup.
   destruct(Nat.eqb v u)eqn:E.
   + apply Nat.eqb_eq in E;subst v;injection Lookup as <-.
     split;[exact(Fixed x tx Hin)|].
     destruct(fun_snapshot_member Fl a fx x tx Apps Nd Hin)as(raw&Look&Eq).
     exists a,sa,raw;split;[apply fun_app_same|auto].
   + destruct(Fx v fxs x tx Lookup Hin)as[Fix(a0&s0&raw&Look&At&Eq)].
     split;[exact Fix|]. exists a0,s0,raw;split;[rewrite fun_app_other by exact E;exact Look|auto].
 - intros v fxs Lookup;cbn[A.set_fun A.c_funs A.nlook]in Lookup.
   destruct(Nat.eqb v u)eqn:E.
   + apply Nat.eqb_eq in E;subst v;exact Decl.
   + exact(DeclOld _ _ Lookup).
Qed.
End Context.
