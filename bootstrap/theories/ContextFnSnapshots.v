(* Function context rows retain one shared signature/application snapshot. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import ExecutionModel FnSnapshot TyOfSubst SigReadingSubst ProvideResultSubst SourceReadingExtend.
Import ListNotations.
Module A:=ExecutionModel.Elaboration.
Definition entries_fn_readings(W:A.world)(es:list A.entry):Prop:=
 forall f sg app,In(A.NFn f sg app)es->exists tm,A.SigOf W f tm None sg/\A.AppOf W f tm app.
Lemma entries_fn_append:forall W a b,
 entries_fn_readings W a->entries_fn_readings W b->entries_fn_readings W(a++b).
Proof. intros W a b Left Right f sg app H;apply in_app_or in H;destruct H;[eapply Left|eapply Right];eassumption. Qed.
Scheme fn_item_mut:=Minimality for A.ItemEntries Sort Prop
 with fn_items_mut:=Minimality for A.ItemsEntries Sort Prop.
Lemma items_fn_readings:forall W p ob tm specs es tm',
 A.ItemsEntries W p ob tm specs es tm'->entries_fn_readings W es.
Proof.
 intro W;apply(fn_items_mut W
 (fun p ob tm sp es tm'=>entries_fn_readings W es)
 (fun p ob tm specs es tm'=>entries_fn_readings W es)).
 - intros p ob tm pa t Res Kind f sg app[Bad|[]];discriminate.
 - intros p ob tm pa s k Res Kind Other f sg app[].
 - intros p ob tm a pa s d n t ty0 bs ap Res Decl Ast Br Ty App f sg app[Bad|[]];discriminate.
 - intros p ob tm a pa s sg bs ap Res Kind Br Sig App f sg0 app[Eq|[]].
   injection Eq as <- <- <-;eexists;split;eassumption.
 - intros p ob tm pa u d n names args res fx Res Decl Ast Fixed Read f sg app[Bad|[]];discriminate.
 - intros p ob tm a pa dn r m rt sg bs ap Res Named Br Recv Sig App f sg0 app[Bad|[]];discriminate.
 - intros p ob tm a pa c d n its bs es tm' Res Decl Ast Br Entries IH;exact IH.
 - intros p ob tm f sg app[].
 - intros p ob tm sp rest es1 tm1 es2 tm2 Entry IH1 Entries IH2;apply entries_fn_append;assumption.
Qed.
Lemma root_items_fn_readings:forall W p es0 tm specs es tm',
 A.RootItems W p es0 tm specs es tm'->entries_fn_readings W es.
Proof.
 intros W p es0 tm specs es tm' H;induction H.
 - intros f sg app[].
 - apply entries_fn_append.
   + intros f sg app InTy;apply in_map_iff in InTy as[t[Bad _]];discriminate.
   + apply entries_fn_append;[intros f sg app[Bad|[]];discriminate|exact IHRootItems].
 - exact IHRootItems.
Qed.
Lemma root_fn_readings:forall W s es0 tm es tm',
 A.RootCtx W s es0 tm es tm'->entries_fn_readings W es.
Proof. intros W s es0 tm es tm' H;destruct H;[intros f sg app[]|eapply root_items_fn_readings;eassumption]. Qed.
Definition snapshot_inst(sigma:subst)(finaltm:list(nat*ty))(app:A.inst):A.inst:=
 map(fun q=>(fst q,option_map(ExecutionModel.app_subst sigma)
  (match snd q with Some t=>Some t|None=>A.nlook(fst q)finaltm end)))app.
Lemma frozen_fn_reading:forall W sigma finaltm f sg app rawtm,
 A.SigOf W f rawtm None sg->A.AppOf W f rawtm app->
 exists tm,A.SigOf W f tm None(A.sig_subst sigma sg)/\A.AppOf W f tm(snapshot_inst sigma finaltm app).
Proof.
 intros W sigma finaltm f sg app rawtm Sig App.
 set(base:=overlay_missing finaltm rawtm).
 assert(Extended:A.SigOf W f base None sg).
 {eapply signature_extends;[exact Sig|apply lookup_overlay_extends]. }
 exists(smap sigma base);split.
 - exact(sigof_subst W sigma f base None sg Extended(smap sigma base)ltac:(intro key;apply nlook_smap)).
 - destruct App as[rs[Eq[Order Method]]].
   exists rs;split;[|split;assumption].
   unfold snapshot_inst;rewrite Eq,map_map;apply map_ext;intro key;cbn[fst snd].
   rewrite nlook_smap;unfold base;rewrite lookup_overlay_missing.
   destruct(A.nlook key rawtm);reflexivity.
Qed.
Lemma entries_fn_row:forall sigma tm es f sg,
 A.nlook f(A.c_fns(A.entries_cx sigma tm es))=Some sg->
 exists raw app,In(A.NFn f raw app)es/\sg=A.sig_subst sigma raw/\
 A.ilook(IFn f)(A.c_apps(A.entries_cx sigma tm es))=Some(snapshot_inst sigma tm app).
Proof.
 intros sigma tm es;induction es as[|entry rest IH];intros f sg Lookup;[discriminate|].
 destruct entry as[t|v ty app|f0 raw app|r m sg0 app|u fx];
 cbn[A.entries_cx A.c_fns A.c_apps flat_map List.app A.entry_key A.ilook A.nlook item_eqb]in *;
 try(destruct(IH _ _ Lookup)as[raw[a[Row[Sig App]]]];exists raw,a;split;[right;exact Row|];split;assumption).
 destruct(Nat.eqb f f0)eqn:Eq.
 - apply Nat.eqb_eq in Eq;subst f0;injection Lookup as <-;exists raw,app;split;[left;reflexivity|];split;reflexivity.
 - destruct(IH _ _ Lookup)as[raw'[a[Row[Sig App]]]];exists raw',a;split;[right;exact Row|];split;assumption.
Qed.
Theorem ctxof_fn_snapshots:forall W f C ns,A.CtxOf W f C ns->FnSnapshots W C.
Proof.
 intros W f C ns(d&es&tm&er&tmr&sigma&Decl&Items&Root&Merge&Eq&Needs)g sg Lookup.
 rewrite Eq in Lookup|-*;destruct(entries_fn_row _ _ _ _ _ Lookup)as[raw[app[Row[Sig App]]]].
 assert(Readings:entries_fn_readings W(es++er)).
 {apply entries_fn_append;[eapply items_fn_readings;exact Items|eapply root_fn_readings;exact Root]. }
 destruct(Readings g raw app Row)as[rawtm[Signature Application]].
 destruct(frozen_fn_reading W sigma tmr g raw app rawtm Signature Application)as[snapshot[SG AP]].
 exists snapshot,(snapshot_inst sigma tmr app);rewrite Sig;split;[exact SG|];split;assumption.
Qed.
