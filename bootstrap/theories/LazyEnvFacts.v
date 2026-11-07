(* Agreement laws for split environments with deferred runtime types. *)
From Stdlib Require Import List Bool Arith Lia.
From NM Require Import Audit MossX FastX LazyX.
Import ListNotations.
Set Implicit Arguments.
Local Open Scope nat_scope.
Lemma flookup_fadd : forall i j en d,
 flookup i(fadd j en d)=if item_eqb i j then Some en else flookup i d.
Proof. intros i j en [vs ts ms];destruct i,j;reflexivity. Qed.
Lemma tlookup_app : forall t a b,
 tlookup t(a++b)=match tlookup t a with Some en=>Some en|None=>tlookup t b end.
Proof.
 intros t a;induction a as[|[u en]a IH];intro b;cbn;[reflexivity|].
 destruct(Nat.eqb t u);[reflexivity|apply IH].
Qed.
Lemma dlookup_app : forall i a b,
 dlookup i(a++b)=match dlookup i a with Some en=>Some en|None=>dlookup i b end.
Proof.
 intros i a;induction a as[|[j en]a IH];intro b;cbn;[reflexivity|].
 destruct(item_eqb i j);[reflexivity|apply IH].
Qed.
Lemma flookup_merge : forall a b i,
 flookup i(fmerge a b)=match flookup i a with Some en=>Some en|None=>flookup i b end.
Proof. intros [av atp am][bv btp bm]i;destruct i;cbn[flookup fmerge];auto using tlookup_app,dlookup_app. Qed.
Lemma ground_exact : forall tau get s,
 (forall t,get t=lookup_subst t s)->ground get tau=Audit.app_subst s tau.
Proof.
 fix IH 1;intros tau get s Read;destruct tau as[|t|n th|ts];cbn[ground Audit.app_subst].
 - reflexivity.
 - rewrite Read;reflexivity.
 - f_equal;induction th as[|[t tau]th Tail];cbn;[reflexivity|].
   rewrite(IH tau get s Read),Tail;reflexivity.
 - f_equal;induction ts as[|tau ts Tail];cbn;[reflexivity|].
   rewrite(IH tau get s Read),Tail;reflexivity.
Qed.
Inductive tyrel : option fentry->option ty->Prop:=
| TyNone : tyrel None None
| TySome : forall suspended,tyrel(Some(FType suspended))(Some(force_ty suspended)).
Lemma tyrel_force : forall a b,tyrel a b->
 (match a with Some(FType v)=>Some(force_ty v)|_=>None end)=b.
Proof. intros a b H;destruct H;reflexivity. Qed.
Lemma tyrel_choice : forall a b x y,tyrel a x->tyrel b y->
 tyrel(match a with Some v=>Some v|None=>b end)(match x with Some v=>Some v|None=>y end).
Proof. intros a b x y A B;destruct A;cbn;[exact B|constructor]. Qed.
Inductive erel : fentry->xdentry->Prop:=
| RVal : forall w,erel(FVal w)(XDVal w)
| RClo : forall f fe d,envrel fe d->erel(FClo f fe)(XDClo f d)
| RFun : forall h sms fe d,envrel fe d->erel(FFun h sms fe)(XDFun h sms d)
| RPrim : forall p,erel(FPrim p)(XDPrim p)
| RFunSym : forall h,erel(FFunSym h)(XDFunSym h)
| RType : forall suspended,erel(FType suspended)(XDType(force_ty suspended))
| RPrimAt : forall p tau,erel(FPrimAt p tau)(XDPrimAt p tau)
with envrel : fenv->xdenv->Prop:=
| ERel : forall fe d,
 (forall i,flookup i fe=None->lookup_xdenv i d=None)->
 (forall i a,flookup i fe=Some a->exists b,lookup_xdenv i d=Some b/\erel a b)->
 (forall t,tyrel(tlookup t(meta fe))(lookup_subst t(xdtypes d)))->envrel fe d.
Definition orel(a:option fentry)(b:option xdentry):Prop:=match a,b with
 |Some x,Some y=>erel x y|None,None=>True|_,_=>False end.
Lemma envrel_lookup : forall fe d i,envrel fe d->orel(flookup i fe)(lookup_xdenv i d).
Proof.
 intros fe d i H;inversion H as[? ? Hn Hs Ht];subst;unfold orel.
 destruct(flookup i fe)as[a|]eqn:E.
 - destruct(Hs i a E)as[b[-> Hb]];exact Hb.
 - rewrite(Hn i E);exact I.
Qed.
Lemma envrel_meta : forall fe d,envrel fe d->forall t,
 tyrel(tlookup t(meta fe))(lookup_subst t(xdtypes d)).
Proof. intros fe d H;inversion H;assumption. Qed.
Lemma envrel_intro : forall fe d,
 (forall t,tyrel(tlookup t(meta fe))(lookup_subst t(xdtypes d)))->
 (forall i,orel(flookup i fe)(lookup_xdenv i d))->envrel fe d.
Proof.
 intros fe d Ht H;constructor.
 - intros i E;specialize(H i);rewrite E in H;unfold orel in H.
   destruct(lookup_xdenv i d);[contradiction|reflexivity].
 - intros i a E;specialize(H i);rewrite E in H;unfold orel in H.
   destruct(lookup_xdenv i d)as[b|];[eauto|contradiction].
 - exact Ht.
Qed.
Lemma envrel_empty : envrel fempty [].
Proof. apply envrel_intro;[intro t;constructor|];intros[];cbn;exact I. Qed.
Lemma envrel_add : forall fe d i a b,
 erel a b->envrel fe d->envrel(fadd i a fe)((i,b)::d).
Proof.
 intros [vs ts ms]d i a b Ha Hd;apply envrel_intro.
 - intro key;pose proof(envrel_meta Hd key)as Types.
   destruct i;inversion Ha;subst;cbn[meta fadd xdtypes lookup_subst tlookup];try exact Types.
   destruct(Nat.eqb key t);[constructor|exact Types].
 - intro j;rewrite flookup_fadd;cbn[lookup_xdenv].
   destruct(item_eqb j i);[exact Ha|apply envrel_lookup;exact Hd].
Qed.
Lemma lookup_subst_app : forall t a b,
 lookup_subst t(a++b)=match lookup_subst t a with Some tau=>Some tau|None=>lookup_subst t b end.
Proof.
 intros t a;induction a as[|[u tau]a IH];intro b;cbn;[reflexivity|].
 destruct(Nat.eqb t u);[reflexivity|apply IH].
Qed.
Lemma envrel_merge : forall fa a fb b,
 envrel fa a->envrel fb b->envrel(fmerge fa fb)(a++b).
Proof.
 intros [av atp am]a[bv btp bm]b A B;apply envrel_intro.
 - intro t;cbn[fmerge meta];rewrite tlookup_app,FastX.xdtypes_app,lookup_subst_app.
   apply tyrel_choice;[apply(envrel_meta A)|apply(envrel_meta B)].
 - intro i;rewrite flookup_merge,FastX.lookup_xdenv_app.
   pose proof(envrel_lookup i A)as L.
   destruct(flookup i(FEnv av atp am)),(lookup_xdenv i a);cbn in L;try contradiction;[exact L|].
   apply envrel_lookup;exact B.
Qed.
Definition crel(a:option fenv)(b:option xdenv):Prop:=match a,b with
 |Some fe,Some d=>envrel fe d|None,None=>True|_,_=>False end.
Lemma fcompose_rel : forall fe d sm,envrel fe d->crel(fcompose fe sm)(xcompose d sm).
Proof.
 intros fe d sm H;induction sm as[|[req src]sm IH];cbn;[apply envrel_empty|].
 pose proof(envrel_lookup src H)as Read;unfold orel in Read.
 destruct(flookup src fe),(lookup_xdenv src d);try contradiction;
 destruct(fcompose fe sm),(xcompose d sm);cbn in *;try contradiction;auto.
 apply envrel_add;assumption.
Qed.
Lemma fchain_rel : forall sms fe d,envrel fe d->crel(fchain fe sms)(xrun_chain d sms).
Proof.
 induction sms as[|sm sms IH];intros fe d H;cbn;[exact H|].
 specialize(IH fe d H).
 destruct(fchain fe sms),(xrun_chain d sms);cbn in *;try contradiction;auto.
 apply fcompose_rel;exact IH.
Qed.
Lemma conv_rel : forall en,erel(conv_entry en)en.
Proof.
 fix IH 1;intros[w|f d|h sms d|p|h|tau|p tau];cbn;constructor.
 all:induction d as[|[i en]d Tail];[apply envrel_empty|].
 all:apply envrel_add;[apply IH|exact Tail].
Qed.
Lemma conv_env_rel : forall d,envrel(conv_env d)d.
Proof.
 induction d as[|[i en]d IH];cbn;[apply envrel_empty|].
 apply envrel_add;[apply conv_rel|exact IH].
Qed.
Lemma fground_rel : forall fe d,envrel fe d->forall tau,fground fe tau=xground d tau.
Proof.
 intros fe d H tau;unfold fground,xground;apply ground_exact;intro t.
 unfold type_get,type_get_meta;apply tyrel_force;apply envrel_meta;exact H.
Qed.
Lemma lookup_type_pairs : forall types i,
 lookup_xdenv i(FastX.ftype_pairs types)=match i with
 |ITy t=>option_map XDType(lookup_subst t types)|_=>None end.
Proof.
 induction types as[|[u tau]types IH];intro i;destruct i;cbn[FastX.ftype_pairs map lookup_xdenv lookup_subst item_eqb fst snd option_map];try reflexivity;try apply IH.
 destruct(Nat.eqb t u);[reflexivity|apply IH].
Qed.
Lemma tyrel_orel : forall a b,tyrel a b->orel a(option_map XDType b).
Proof. intros a b H;destruct H;cbn;[exact I|constructor]. Qed.
Lemma type_rows_env_rel : forall rows types,
 (forall t,tyrel(tlookup t rows)(lookup_subst t types))->
 envrel(FEnv [] rows rows)(FastX.ftype_pairs types).
Proof.
 intros rows types Types;apply envrel_intro.
 - intro t;cbn[meta];rewrite FastX.xdtypes_pairs;apply Types.
 - intro i;rewrite lookup_type_pairs;destruct i;cbn[flookup dlookup];try exact I.
   apply tyrel_orel;apply Types.
Qed.
Definition ftype_env fe:=FEnv [](meta fe)(meta fe).
Lemma ftype_env_rel : forall fe d,envrel fe d->envrel(ftype_env fe)(xtype_env d).
Proof.
 intros fe d H;rewrite FastX.xtype_env_pairs;apply type_rows_env_rel;apply envrel_meta;exact H.
Qed.
Lemma fattach_merge : forall d dc,fattach_types d dc=fmerge dc(ftype_env d).
Proof. intros [] [];cbn[fattach_types fmerge ftype_env];rewrite app_nil_r;reflexivity. Qed.
Lemma fattach_types_rel : forall fe d fc dc,envrel fe d->envrel fc dc->
 envrel(fattach_types fe fc)(xclose d dc).
Proof.
 intros fe d fc dc Hd Hc;rewrite fattach_merge;unfold xclose.
 apply envrel_merge;[exact Hc|apply ftype_env_rel;exact Hd].
Qed.
Lemma deferred_ground_rel : forall fe d tau,envrel fe d->
 force_ty(defer_ground(meta fe)tau)=xground d tau.
Proof. intros fe d tau H;rewrite force_defer_ground;apply fground_rel;exact H. Qed.
Lemma fbind_ty_rel : forall P fe d t tau,envrel fe d->
 envrel(fbind_ty P fe t tau)(xbind_ty P d t tau).
Proof.
 intros P fe d t tau H;unfold fbind_ty,xbind_ty;destruct(xreify P);[|exact H].
 apply envrel_add;[rewrite <-(deferred_ground_rel tau H);constructor|exact H].
Qed.
Lemma finst_rel : forall fe d th,envrel fe d->envrel(finst fe th)(xinst d th).
Proof.
 intros fe d th H;rewrite xinst_map;induction th as[|[t tau]th IH];cbn[finst fold_right map fst snd].
 - apply envrel_empty.
 - apply envrel_add;[rewrite <-(deferred_ground_rel tau H);constructor|exact IH].
Qed.
Lemma fcall_env_rel : forall P fe d th fc dc,envrel fe d->envrel fc dc->
 envrel(fcall_env P fe th fc)(xcall_env P d th dc).
Proof.
 intros P fe d th fc dc H Hc;unfold fcall_env,xcall_env;destruct(xreify P);[|exact Hc].
 apply envrel_merge;[apply finst_rel;exact H|apply fattach_types_rel;assumption].
Qed.
Lemma fapply_env_rel : forall P fe d h th fc dc,envrel fe d->envrel fc dc->
 envrel(fapply_env P fe h th fc)(xapply_env P d h th dc).
Proof. intros;unfold fapply_env,xapply_env;apply fcall_env_rel;assumption. Qed.
Lemma fprim_entry_rel : forall fe d i p,envrel fe d->erel(fprim_entry fe i p)(xprim_entry d i p).
Proof.
 intros fe d i p H;destruct p;destruct i;cbn[fprim_entry xprim_entry];try constructor;
 rewrite(fground_rel H);constructor.
Qed.
Lemma fcast_rel : forall P fe d n th v,envrel fe d->fcast P fe n th v=xcast P d n th v.
Proof.
 intros P fe d n th v H;unfold fcast,xcast.
 destruct(lookup_cast n(xp_cast P))as[spec|];[|reflexivity].
 destruct(xc_handle spec)as[[h expected]|];cbn;[rewrite(fground_rel H)|].
 all:destruct(lookup_subst(xc_held spec)th)as[tau|];[|reflexivity].
 all:destruct v;try reflexivity.
 all:rewrite(fground_rel H);reflexivity.
Qed.
Lemma tlookup_filter_keys : forall keep t rows,
 tlookup t(filter(fun p=>keep(fst p))rows)=if keep t then tlookup t rows else None.
Proof.
 intros keep t rows;induction rows as[|[u en]rows IH];cbn[filter fst tlookup];[destruct(keep t);reflexivity|].
 destruct(Nat.eqb t u)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst u;destruct(keep t)eqn:Keep;cbn[tlookup];rewrite ?Nat.eqb_refl,?IH,?Keep;reflexivity.
 - destruct(keep u);cbn[tlookup];rewrite ?Eq,IH;destruct(keep t);reflexivity.
Qed.
Lemma lookup_subst_filter_keys : forall keep t rows,
 lookup_subst t(filter(fun p=>keep(fst p))rows)=if keep t then lookup_subst t rows else None.
Proof.
 intros keep t rows;induction rows as[|[u tau]rows IH];cbn[filter fst lookup_subst];[destruct(keep t);reflexivity|].
 destruct(Nat.eqb t u)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst u;destruct(keep t)eqn:Keep;cbn[lookup_subst];rewrite ?Nat.eqb_refl,?IH,?Keep;reflexivity.
 - destruct(keep u);cbn[lookup_subst];rewrite ?Eq,IH;destruct(keep t);reflexivity.
Qed.
Lemma fresult_types_rel : forall Dr fe d,envrel fe d->
 envrel(fresult_types Dr fe)(xresult_types Dr d).
Proof.
 intros Dr fe d H;unfold fresult_types,xresult_types.
 rewrite FastX.xtype_env_pairs,FastX.xresult_pairs.
 apply type_rows_env_rel;intro t.
 rewrite (tlookup_filter_keys (fun u=>existsb(item_eqb(ITy u))Dr) t(meta fe)).
 rewrite (lookup_subst_filter_keys (fun u=>existsb(item_eqb(ITy u))Dr) t(xdtypes d)).
 destruct(existsb(item_eqb(ITy t))Dr);[apply envrel_meta;exact H|constructor].
Qed.
Lemma fresult_env_rel : forall P Dr fe d dr raw,envrel fe d->envrel dr raw->
 envrel(fresult_env P fe Dr dr)(xresult_env P d Dr raw).
Proof.
 intros P Dr fe d dr raw H Hr;unfold fresult_env,xresult_env;destruct(xreify P);[|exact Hr].
 apply envrel_merge;[exact Hr|apply fresult_types_rel;exact H].
Qed.
