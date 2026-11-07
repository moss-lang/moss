(* Recorded application rows always name provisions present in the context. *)
From Stdlib Require Import List Bool Arith.
From NM Require Import Audit Sx CoreMoss.
Import ListNotations.
Module A:=Audit.Elaboration.
Definition AppsProvided(C:A.cx):Prop:=forall key app,In(key,app)(A.c_apps C)->
 match key with ITy _=>False|_=>A.provides C key end.
Lemma context_nlook_member : forall X key(value:X)rows,
 In(key,value)rows -> A.nlook key rows<>None.
Proof.
 intros X key value rows;induction rows as[|[first found]rest IH];intros Member;[contradiction|].
 cbn[A.nlook];destruct(Nat.eqb key first)eqn:Same;[discriminate|].
 apply IH;destruct Member as[Eq|Member];[injection Eq as <- _;rewrite Nat.eqb_refl in Same;discriminate|exact Member].
Qed.
Lemma context_mlook_member : forall receiver method sg rows,
 In((receiver,method),sg)rows -> A.mlook receiver method rows<>None.
Proof.
 intros receiver method sg rows;induction rows as[|[[r m]sig]rest IH];intros Member;[contradiction|].
 cbn[A.mlook];destruct(ty_eqb receiver r&&Nat.eqb method m)eqn:Same;[discriminate|].
 apply IH;destruct Member as[Eq|Member];[|exact Member].
 injection Eq as <- <- _.
 assert(ty_eqb r r=true)by(apply ty_eqb_eq;reflexivity).
 rewrite H,Nat.eqb_refl in Same;discriminate.
Qed.
Lemma context_nlook_append : forall X key(before after:list(nat*X)),
 A.nlook key after<>None -> A.nlook key(before++after)<>None.
Proof.
 intros X key before;induction before as[|[name value]rest IH];intros after Present;[exact Present|].
 cbn[List.app A.nlook];destruct(Nat.eqb key name);[discriminate|apply IH;exact Present].
Qed.
Lemma context_mlook_append : forall receiver method before after,
 A.mlook receiver method after<>None -> A.mlook receiver method(before++after)<>None.
Proof.
 intros receiver method before;induction before as[|[[r m]sg]rest IH];intros after Present;[exact Present|].
 cbn[List.app A.mlook];destruct(ty_eqb receiver r&&Nat.eqb method m);[discriminate|apply IH;exact Present].
Qed.
Lemma entries_apps_provided : forall sigma tm es,AppsProvided(A.entries_cx sigma tm es).
Proof.
 intros sigma tm es key app Member.
 unfold A.entries_cx in Member;cbn[A.c_apps]in Member.
 apply in_flat_map in Member as[entry[Entry Pair]].
 destruct entry as[t|v ty ap|f sg ap|r m sg ap|u fx];cbn[A.entry_key A.mkey]in Pair;
 try contradiction;destruct Pair as[Eq|[]];injection Eq as <- <-;
 cbn[A.provides A.entries_cx A.c_vals A.c_fns A.c_mths A.c_funs].
 - apply(context_nlook_member _ _ (Audit.app_subst sigma ty));apply in_flat_map;exists(A.NVal v ty ap);split;[exact Entry|left;reflexivity].
 - apply(context_nlook_member _ _ (A.sig_subst sigma sg));apply in_flat_map;exists(A.NFn f sg ap);split;[exact Entry|left;reflexivity].
 - apply(context_mlook_member _ _ (A.sig_subst sigma sg));apply in_flat_map;exists(A.NMth r m sg ap);split;[exact Entry|left;reflexivity].
 - apply(context_nlook_member _ _ (A.fixed_subst sigma fx));apply in_flat_map;exists(A.NFun u fx);split;[exact Entry|left;reflexivity].
Qed.
Lemma app_presence_set_ty : forall C t ty,AppsProvided C -> AppsProvided(A.set_ty C t ty).
Proof. intros C t ty Old key app Member;specialize(Old key app Member);destruct key;exact Old. Qed.
Lemma app_presence_set_val : forall C v ty,AppsProvided C -> AppsProvided(A.set_val C v ty).
Proof.
 intros C v ty Old key app Member;specialize(Old key app Member);destruct key as[t0|v0|f0|r0 m0 th0|u0];try exact Old.
 cbn[A.provides A.set_val A.c_vals A.nlook];destruct(Nat.eqb v0 v);[discriminate|exact Old].
Qed.
Lemma app_presence_set_fn : forall C f sg,AppsProvided C -> AppsProvided(A.set_fn C f sg).
Proof.
 intros C f sg Old key app Member;specialize(Old key app Member);destruct key as[t0|v0|f0|r0 m0 th0|u0];try exact Old.
 cbn[A.provides A.set_fn A.c_fns A.nlook];destruct(Nat.eqb f0 f);[discriminate|exact Old].
Qed.
Lemma app_presence_set_mth : forall C r m sg,AppsProvided C -> AppsProvided(A.set_mth C r m sg).
Proof.
 intros C r m sg Old key app Member;specialize(Old key app Member);destruct key as[t0|v0|f0|r0 m0 th0|u0];try exact Old.
 cbn[A.provides A.set_mth A.c_mths A.mlook];destruct(ty_eqb r0 r&&Nat.eqb m0 m);[discriminate|exact Old].
Qed.
Lemma app_presence_set_fun : forall C u fx,AppsProvided C -> AppsProvided(A.set_fun C u fx).
Proof.
 intros C u fx Old key app[Eq|Member].
 - injection Eq as <- <-;cbn[A.provides A.set_fun A.c_funs A.nlook];rewrite Nat.eqb_refl;discriminate.
 - specialize(Old key app Member);destruct key as[t0|v0|f0|r0 m0 th0|u0];try exact Old.
   cbn[A.provides A.set_fun A.c_funs A.nlook];destruct(Nat.eqb u0 u);[discriminate|exact Old].
Qed.
Lemma app_presence_provide : forall C C1 key app,
 AppsProvided C1 -> A.provides C1 key -> (forall t,key<>ITy t) -> AppsProvided(A.provide C key app C1).
Proof.
 intros C C1 key app Old Present NonType other actual[Eq|Member].
 - injection Eq as <- <-;destruct key;try exact Present;exfalso;eapply NonType;reflexivity.
 - specialize(Old other actual Member);destruct other;exact Old.
Qed.

Lemma output_apps_provided : forall C tm es,
 AppsProvided C -> AppsProvided(A.output_cx C tm es).
Proof.
 intros C tm es Old key app Member.
 cbn[A.output_cx A.c_apps]in Member;apply in_app_or in Member as[New|Previous].
 - apply in_flat_map in New as[entry[Entry Pair]].
   destruct entry as[t|v ty ap|f sg ap|r m sg ap|u fx];cbn[A.entry_key A.mkey]in Pair;
   try contradiction;destruct Pair as[Eq|[]];injection Eq as <- <-;
   cbn[A.provides A.output_cx A.c_vals A.c_fns A.c_mths A.c_funs].
   + apply(context_nlook_member _ _ (A.canon C ty));apply in_or_app;left;apply in_flat_map;
     exists(A.NVal v ty ap);split;[exact Entry|left;reflexivity].
   + apply(context_nlook_member _ _ (A.sig_subst(A.c_sub C)sg));apply in_or_app;left;apply in_flat_map;
     exists(A.NFn f sg ap);split;[exact Entry|left;reflexivity].
   + apply(context_mlook_member _ _ (A.sig_subst(A.c_sub C)sg));apply in_or_app;left;apply in_flat_map;
     exists(A.NMth r m sg ap);split;[exact Entry|left;reflexivity].
   + apply(context_nlook_member _ _ (A.fixed_subst(A.c_sub C)fx));apply in_or_app;left;apply in_flat_map;
     exists(A.NFun u fx);split;[exact Entry|left;reflexivity].
 - specialize(Old key app Previous);destruct key;try contradiction;
   cbn[A.provides A.output_cx A.c_vals A.c_fns A.c_mths A.c_funs];
   first[apply context_nlook_append|apply context_mlook_append];exact Old.
Qed.
Lemma provide_result_apps : forall W u C out C' keys,
 A.ProvideResult W u C out C' keys -> AppsProvided C -> AppsProvided C'.
Proof.
 intros W u C out C' keys(d&ps&args&res&body&outs&es&tm&Decl&Parts&Outputs&Entries&Keys&Eq)Old.
 subst C';apply output_apps_provided;exact Old.
Qed.

Section Bindings.
Variables(W:A.world)(P:xprogram).
Fixpoint binditems_apps F C its wrap Cout Fout made
 (H:A.BindItems W P F C its wrap Cout Fout made){struct H}:
 AppsProvided C -> AppsProvided Cout
with binditem_apps F C it wrap Cout Fout made
 (H:A.BindItem W P F C it wrap Cout Fout made){struct H}:
 AppsProvided C -> AppsProvided Cout.
Proof.
 - destruct H;intro Old;[exact Old|].
   apply(binditems_apps _ _ _ _ _ _ _ H0);apply(binditem_apps _ _ _ _ _ _ _ H);exact Old.
 - destruct H;intro Old.
   + apply app_presence_set_ty;exact Old.
   + apply app_presence_provide.
     * apply app_presence_set_val;exact Old.
     * cbn[A.provides A.set_val A.c_vals A.nlook];rewrite Nat.eqb_refl;discriminate.
     * intros;discriminate.
   + apply app_presence_provide.
     * apply app_presence_set_fn;exact Old.
     * cbn[A.provides A.set_fn A.c_fns A.nlook];rewrite Nat.eqb_refl;discriminate.
     * intros;discriminate.
   + apply app_presence_provide.
     * apply app_presence_set_fn;exact Old.
     * cbn[A.provides A.set_fn A.c_fns A.nlook];rewrite Nat.eqb_refl;discriminate.
     * intros;discriminate.
   + apply app_presence_provide.
     * apply app_presence_set_mth;exact Old.
     * cbn[A.mkey A.provides A.set_mth A.c_mths A.mlook].
       assert(ReceiverSelf:ty_eqb(A.canon C rt)(A.canon C rt)=true)by(apply ty_eqb_eq;reflexivity).
       rewrite ReceiverSelf,Nat.eqb_refl;discriminate.
     * intros;discriminate.
   + eapply binditems_apps;eassumption.
   + eapply provide_result_apps;eassumption.
   + apply app_presence_set_fun;exact Old.
   + apply app_presence_set_fun;exact Old.
Qed.
End Bindings.

Lemma functx_apps_provided : forall W u C needs,
 A.FunCtx W u C needs -> AppsProvided C.
Proof.
 intros W u C needs(d&names&args&res&body&es&tm&tm'&sigma&implicit&Decl&Parts&Inputs&Implicit&Types&Merge&Eq&Needs).
 subst C;apply entries_apps_provided.
Qed.
Lemma context_app_lookup_in : forall (rows:list(item*A.inst)) key app,
 A.ilook key rows=Some app -> In(key,app)rows.
Proof.
 intros rows;induction rows as[|[first value]rest IH];intros key app Lookup;[discriminate|].
 cbn[A.ilook]in Lookup;destruct(item_eqb key first)eqn:Same.
 - apply item_eqb_eq in Same;subst first;injection Lookup as <-;left;reflexivity.
 - right;apply IH;exact Lookup.
Qed.
Lemma context_app_lookup_provided : forall C key app,
 AppsProvided C -> A.ilook key(A.c_apps C)=Some app -> A.provides C key.
Proof.
 intros C key app Provided Lookup.
 specialize(Provided key app(context_app_lookup_in _ _ _ Lookup)).
 destruct key;[contradiction|exact Provided|exact Provided|exact Provided|exact Provided].
Qed.
