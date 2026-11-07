(* Context formation gives each functor row a functional fixed snapshot.
   Duplicate list occurrences do not create competing readings of a key. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit FunctorDecl.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition entries_fixed_readings(W:A.world)(es:list A.entry):Prop:=
 forall u fx,In(A.NFun u fx)es->exists tm,A.FixedReadings W u tm fx.
Lemma entries_fixed_append:forall W a b,
 entries_fixed_readings W a->entries_fixed_readings W b->entries_fixed_readings W(a++b).
Proof. intros W a b A B u fx H;apply in_app_or in H;destruct H;[eapply A|eapply B];eassumption. Qed.
Lemma entries_fixed_types:forall W ts,entries_fixed_readings W(map A.NTy ts).
Proof. intros W ts u fx H;apply in_map_iff in H as[t[Bad _]];discriminate. Qed.
Scheme fixed_item_mut:=Minimality for A.ItemEntries Sort Prop
 with fixed_items_mut:=Minimality for A.ItemsEntries Sort Prop.
Lemma items_fixed_readings:forall W p ob tm specs es tm',
 A.ItemsEntries W p ob tm specs es tm'->entries_fixed_readings W es.
Proof.
 intro W;apply(fixed_items_mut W
 (fun p ob tm sp es tm'=>entries_fixed_readings W es)
 (fun p ob tm specs es tm'=>entries_fixed_readings W es)).
 - intros p ob tm pa t Res Kind u fx[Bad|[]];discriminate.
 - intros p ob tm pa s k Res Kind Br u fx[].
 - intros p ob tm a pa s d n t ty0 bs ap Res Decl Ast Br Ty App u fx[Bad|[]];discriminate.
 - intros p ob tm a pa s sg bs ap Res Kind Br Sig App u fx[Bad|[]];discriminate.
 - intros p ob tm pa u d n names args res fx Res Decl Ast Fixed Read u0 fx0[Eq|[]].
   injection Eq as <- <-;exists tm;exact Read.
 - intros p ob tm a pa dn r m rt sg bs ap Res Named Br Recv Sig App u fx[Bad|[]];discriminate.
 - intros p ob tm a pa c d n its bs es tm' Res Decl Ast Br Entries IH;exact IH.
 - intros p ob tm u fx[].
 - intros p ob tm sp rest es1 tm1 es2 tm2 Entry IH1 Entries IH2.
   apply entries_fixed_append;assumption.
Qed.
Lemma root_items_fixed_readings:forall W p es0 tm specs es tm',
 A.RootItems W p es0 tm specs es tm'->entries_fixed_readings W es.
Proof.
 intros W p es0 tm specs es tm' H;induction H.
 - intros u fx[].
 - apply entries_fixed_append;[apply entries_fixed_types|].
   apply entries_fixed_append;[|exact IHRootItems].
   intros u0 fx0[Eq|[]];injection Eq as <- <-;eexists;eassumption.
 - exact IHRootItems.
Qed.
Lemma root_fixed_readings:forall W s es0 tm es tm',
 A.RootCtx W s es0 tm es tm'->entries_fixed_readings W es.
Proof. intros W s es0 tm es tm' H;destruct H;[intros u fx[]|eapply root_items_fixed_readings;eassumption]. Qed.

Lemma fixed_subst_reading_member:forall W u tm fx sigma x tx,
 A.FixedReadings W u tm fx->In(x,tx)(A.fixed_subst sigma fx)->
 A.Fixed W u x /\forall y,In(x,y)(A.fixed_subst sigma fx)->y=tx.
Proof.
 intros W u tm fx sigma x tx Read Hin.
 unfold A.fixed_subst in Hin;apply in_map_iff in Hin as[[key raw][Eq Hin]].
 injection Eq as Keys Values;subst key tx.
 destruct(proj1(Read x raw)Hin)as[Fixed At];split;[exact Fixed|].
 intros y Hy;unfold A.fixed_subst in Hy;apply in_map_iff in Hy as[[key2 raw2][Eq2 Hin2]].
 injection Eq2 as Keys2 Values2;subst key2 y.
 destruct(proj1(Read x raw2)Hin2)as[_ At2];congruence.
Qed.
Lemma nlook_consistent_member:forall X(xs:list(nat*X))x value,
 In(x,value)xs->(forall other,In(x,other)xs->other=value)->A.nlook x xs=Some value.
Proof.
 intros X xs;induction xs as[|[key first]rest IH];intros x value Hin Cons;[contradiction|].
 cbn[A.nlook];destruct(Nat.eqb x key)eqn:E.
 - apply Nat.eqb_eq in E;subst key;rewrite(Cons first(or_introl eq_refl));reflexivity.
 - apply IH;[destruct Hin as[Eq|Hin];[injection Eq as -> _;rewrite Nat.eqb_refl in E;discriminate|exact Hin]|].
   intros other H;apply Cons;right;exact H.
Qed.
Lemma entries_fixed_row:forall sigma tm es u fx,
 A.nlook u(A.c_funs(A.entries_cx sigma tm es))=Some fx->
 exists raw,In(A.NFun u raw)es/\fx=A.fixed_subst sigma raw.
Proof.
 intros sigma tm es;induction es as[|entry rest IH];intros u fx H;[discriminate|].
 destruct entry as[t|v ty app|f sg app|r m sg app|u0 raw];cbn[A.entries_cx A.c_funs flat_map List.app]in H;
 try(destruct(IH _ _ H)as[a[Hin Eq]];exists a;split;[right;exact Hin|exact Eq]).
 cbn[A.nlook]in H;destruct(Nat.eqb u u0)eqn:E.
 - apply Nat.eqb_eq in E;subst u0;injection H as <-;exists raw;split;[left;reflexivity|reflexivity].
 - destruct(IH _ _ H)as[a[Hin Eq]];exists a;split;[right;exact Hin|exact Eq].
Qed.

Theorem ctxof_fixed_snapshot_functional:forall W f C ns,
 A.CtxOf W f C ns->forall u fx,A.nlook u(A.c_funs C)=Some fx->
 forall x tx,In(x,tx)fx->A.Fixed W u x /\A.nlook x fx=Some tx.
Proof.
 intros W f C ns(d&es&tm&er&tmr&sigma&Decl&Items&Root&Merge&Eq&Needs)u fx Lookup x tx Hin.
 rewrite Eq in Lookup;destruct(entries_fixed_row _ _ _ _ _ Lookup)as[raw[InRow ->]].
 assert(Readings:entries_fixed_readings W(es++er)).
 { apply entries_fixed_append;[eapply items_fixed_readings;exact Items|eapply root_fixed_readings;exact Root]. }
 destruct(Readings u raw InRow)as[fixed_tm Fixed].
 destruct(fixed_subst_reading_member W u fixed_tm raw sigma x tx Fixed Hin)as[Domain Cons].
 split;[exact Domain|apply nlook_consistent_member;assumption].
Qed.

Lemma entries_functor_app_exact:forall sigma tm es u fx,
 A.nlook u(A.c_funs(A.entries_cx sigma tm es))=Some fx->
 A.ilook(IFun u)(A.c_apps(A.entries_cx sigma tm es))=Some(A.fixed_app fx).
Proof.
 intros sigma tm es;induction es as[|entry rest IH];intros u fx Lookup;[discriminate|].
 destruct entry as[t|v ty app|f sg app|r m sg app|u0 raw];
 cbn[A.entries_cx A.c_funs A.c_apps flat_map List.app A.entry_key A.ilook A.nlook item_eqb]in *;
 try exact(IH _ _ Lookup).
 destruct(Nat.eqb u u0);[injection Lookup as <-;reflexivity|exact(IH _ _ Lookup)].
Qed.
Theorem ctxof_functor_app_exact:forall W f C ns,
 A.CtxOf W f C ns->forall u fx,A.nlook u(A.c_funs C)=Some fx->
 A.ilook(IFun u)(A.c_apps C)=Some(A.fixed_app fx).
Proof.
 intros W f C ns(d&es&tm&er&tmr&sigma&Decl&Items&Root&Merge&Eq&Needs)u fx Lookup.
 rewrite Eq in Lookup|-*;apply entries_functor_app_exact;exact Lookup.
Qed.
