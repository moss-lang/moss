(* The converse direction of a copy chain's result agreement (clause B of
   ItemsAgree), by counting methods: every item of the chain's root
   platform family h0 is one of the caller's new methods, given the
   forward direction (each new method is filed under the run-time key of
   one of h0's items).

   h0's item has a leaf (receiver symbol s, name dn), resolving its method
   m0 (MethodNamed in h0's module). The copy u has the same leaf
   (SameResult along the chain), resolving its method m in u's module.
   Either both are s's attached method dn, so m = m0, or both come from a
   module's detached scope, so both are detached methods (scope_detached).
   The forward direction files u's item under the key of an item of h0
   with method m, which is then h0's item (Platform.MethodsDistinct). *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize.
From NM Require PublicFormation PublicFamilyAgree PublicTypeFacts.
From NM Require Import AuditFacts.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section ChainAgree.
Variables (b : bundle) (W : A.world).
Hypothesis Wo : A.WorldOf b W.
Import A.

(* ---- a detached scope holds detached methods ---------------------------- *)

Fixpoint exports_detached p ns n e (X : Exports b W p ns n e) {struct X} :
  ns = NSDetached -> forall m, e = ESym m -> Platform.DetachedMethod W m.
Proof.
  destruct X as [p s d ns n D Md Fn|p mo t imp u e ns Mo It Gl Iu Ens Ex|p mo t imp a Mo It Al];
    intros Ens' m Ee.
  - injection Ee as <-. subst ns.
    destruct (d_ast d) as [a1 a2|n0|n0 t0|n0 ps0 t0|n0|n0 t0 i0|rcv n' ps rt body|n0 its|n0 ps0 a0 r0 b0] eqn:Ast;
      cbn in Fn; try discriminate; try (injection Fn as En _; discriminate En).
    destruct rcv; cbn in Fn; try discriminate; try (injection Fn as En _; discriminate En).
    exists d, n', ps, rt, body. split; [exact D|exact Ast].
  - subst e. cbn in Ens'. subst ns.
    destruct (u_dot u) eqn:Dot; [|discriminate].
    destruct Ex as [Ex|[Df _]]; [|congruence].
    exact (exports_detached _ _ _ _ Ex eq_refl m eq_refl).
  - discriminate.
Qed.

Lemma scope_detached : forall p n m, Loaded b p -> w_scope W p NSDetached n = Some (ESym m) ->
  Platform.DetachedMethod W m.
Proof.
  intros p n m L Sc. apply (proj1 (@wo_scope b W Wo p NSDetached n (ESym m) L)) in Sc.
  destruct (scope_exports Sc) as [Ex|(mo & t & imp & _ & _ & Ex)];
    exact (exports_detached _ _ _ _ Ex eq_refl m eq_refl).
Qed.

(* ---- a result's method entries and its leaves ------------------------------- *)

Ltac kmis := exfalso;
  repeat match goal with
  | H1 : resolve_sym W ?p ?pa = Some ?x, H2 : resolve_sym W ?p ?pa = Some ?y |- _ =>
      assert_fails (constr_eq x y); assert (y = x) by congruence; subst y; clear H2
  end;
  repeat match goal with
  | H1 : w_decl W ?x = Some ?a, H2 : w_decl W ?x = Some ?b |- _ =>
      assert_fails (constr_eq a b); rewrite H1 in H2; injection H2 as H2; subst b
  end;
  unfold kind_sym in *;
  repeat match goal with H : w_decl W _ = Some _ |- _ => rewrite H in *; clear H end;
  repeat match goal with H : d_ast _ = _ |- _ => rewrite H in *; clear H end;
  cbn in *; intuition congruence.

Ltac flat := repeat match goal with
  | H : _ \/ _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  | H : exists _, _ |- _ => destruct H end.

Lemma decl_loaded : forall s d, w_decl W s = Some d -> Loaded b (d_mod d).
Proof. intros s d D. exact (proj1 (proj1 (@wo_decl_sound b W Wo s d D))). Qed.

(* A method entry of a reading comes from a method leaf of its items. *)
Lemma entry_leaf :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
     forall rt m g a, In (NMth rt m g a) es -> exists s dn p',
       ResultSpecLeaf W p sp (s, Some dn) /\ MethodNamed W p' s dn m /\ (Loaded b p -> Loaded b p')) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
     forall rt m g a, In (NMth rt m g a) es -> exists sp s dn p', In sp sps /\
       ResultSpecLeaf W p sp (s, Some dn) /\ MethodNamed W p' s dn m /\ (Loaded b p -> Loaded b p')).
Proof.
  apply (PublicFormation.oies_mut W
    (fun p ob tm sp es tm' _ => forall rt m g a, In (NMth rt m g a) es -> exists s dn p',
       ResultSpecLeaf W p sp (s, Some dn) /\ MethodNamed W p' s dn m /\ (Loaded b p -> Loaded b p'))
    (fun p ob tm sps es tm' _ => forall rt m g a, In (NMth rt m g a) es -> exists sp s dn p', In sp sps /\
       ResultSpecLeaf W p sp (s, Some dn) /\ MethodNamed W p' s dn m /\ (Loaded b p -> Loaded b p'))).
  all: try (intros; match goal with H : In _ _ |- _ => destruct H as [E|[]]; discriminate E end).
  - intros; contradiction.
  - intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sig App rt' m' g' a' [E|[]].
    injection E as <- <- <- <-. exists r, dn, p. split; [eapply RSLMethod; exact R|].
    split; [exact Mn|exact (fun L => L)].
  - intros p ob tm a pa c d n its sps bs es tm' R D Ast Esps B Its IH rt m g a' I.
    destruct (IH rt m g a' I) as (sp & s & dn & p' & Isp & Lf & Mn & Ld).
    rewrite <- citem_specs_map, <- Esps in Isp.
    exists s, dn, p'. split; [eapply RSLContext; eassumption|].
    split; [exact Mn|]. intros _. exact (Ld (decl_loaded c d D)).
  - intros; contradiction.
  - intros p ob tm sp rest es1 tm1 es2 tm2 H1 IH1 H2 IH2 rt m g a I.
    apply in_app_or in I as [I|I].
    + destruct (IH1 rt m g a I) as (s & dn & p' & Lf & Mn & Ld).
      exists sp, s, dn, p'. split; [left; reflexivity|]. exact (conj Lf (conj Mn Ld)).
    + destruct (IH2 rt m g a I) as (sp' & s & dn & p' & Isp & Lf & Mn & Ld).
      exists sp', s, dn, p'. split; [right; exact Isp|]. exact (conj Lf (conj Mn Ld)).
Qed.

(* An item of a list of items is read, its entries among the list's. *)
Lemma items_member : forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
  forall sp, In sp sps -> exists tm1 es1 tm2, OutputItemEntries W p ob tm1 sp es1 tm2 /\ incl es1 es.
Proof.
  intros p ob tm sps es tm' D. induction D as [p ob tm|p ob tm sp0 rest es1 tm1 es2 tm2 H1 H2 IH];
    intros sp I; [contradiction|].
  destruct I as [<-|I].
  - exists tm, es1, tm1. split; [exact H1|]. intros e Ie. apply in_or_app. left. exact Ie.
  - destruct (IH sp I) as (t1 & e1 & t2 & H & Inc). exists t1, e1, t2. split; [exact H|].
    intros e Ie. apply in_or_app. right. exact (Inc e Ie).
Qed.

(* A method leaf of an item has a method entry in its reading. *)
Lemma leaf_entry : forall p sp k, ResultSpecLeaf W p sp k -> forall s dn, k = (s, Some dn) ->
  forall ob tm es tm', OutputItemEntries W p ob tm sp es tm' ->
  exists rt m g a p', In (NMth rt m g a) es /\ MethodNamed W p' s dn m /\ (Loaded b p -> Loaded b p').
Proof.
  intros p sp k Lf. induction Lf as [p pa dn0 br s0 R|p pa br s0 d R D Nc Np|p pa br c d n its sp k R D Ast Isp Lf IH];
    intros s dn Ek ob tm es tm' OIE.
  - injection Ek as <- <-. inversion OIE; subst.
    match goal with H : resolve_sym W p pa = Some ?r |- _ => assert (r = s0) by congruence; subst r end.
    do 5 eexists. split; [left; reflexivity|]. split; [eassumption|exact (fun L => L)].
  - discriminate.
  - subst k. inversion OIE; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?c' |- _ => assert (c' = c) by congruence; subst c' end.
    match goal with H : w_decl W c = Some ?d' |- _ => assert (d' = d) by congruence; subst d' end.
    match goal with H : d_ast d = ADCtx _ (map CISpec _) |- _ => rewrite H in Ast; injection Ast as -> Ei; subst its end.
    rewrite citem_specs_map in Isp.
    match goal with H : OutputItemsEntries W _ _ _ _ _ _ |- _ => rename H into HI end.
    destruct (items_member _ _ _ _ _ _ HI sp Isp) as (t1 & e1 & t2 & Hm & Inc).
    destruct (IH s dn eq_refl _ _ _ _ Hm) as (rt & m & g & a & p' & I & Mn & Ld).
    exists rt, m, g, a, p'. split; [exact (Inc _ I)|]. split; [exact Mn|].
    intros _. exact (Ld (decl_loaded c d D)).
Qed.

(* ---- the counting --------------------------------------------------------- *)

Lemma mlook_in_some : forall l r m g a, In (IMth r m [], ProvidedMethod g a) l ->
  exists g', mlook r m (mths_of l) = Some g'.
Proof.
  induction l as [|[k p] rest IH]; intros r m g a I; [contradiction|].
  destruct I as [E|I].
  - injection E as -> ->. cbn. rewrite (proj2 (PublicTypeFacts.ty_eqb_spec r r) eq_refl), Nat.eqb_refl. cbn. eauto.
  - destruct (IH r m g a I) as [g' L]. unfold mths_of. cbn [flat_map]. fold (mths_of rest).
    destruct k as [x|x|x|r1 m1 a1|x], p as [t0 a0|g0 a0|g0 a0|fx]; cbn [app]; try (exists g'; exact L).
    cbn. destruct (ty_eqb r r1 && Nat.eqb m m1); [eauto|exists g'; exact L].
Qed.

Lemma functor_decl : forall d ps a r b0, functor_parts (d_ast d) = Some (ps, a, r, b0) ->
  exists n, d_ast d = ADFunctor n ps a r b0.
Proof.
  intros d ps a r b0 F. destruct (d_ast d); try discriminate. injection F as <- <- <- <-. eauto.
Qed.

(* Two resolutions of one leaf's method are equal, or both detached. *)
Lemma named_same : forall p0 p s dn m0 m, Loaded b p0 -> Loaded b p ->
  MethodNamed W p0 s dn m0 -> MethodNamed W p s dn m ->
  m0 = m \/ (Platform.DetachedMethod W m0 /\ Platform.DetachedMethod W m).
Proof.
  intros p0 p s dn m0 m L0 L [A0|[N0 S0]] [A1|[N1 S1]].
  - left. unfold Attached in A0, A1. congruence.
  - exfalso. exact (N1 m0 A0).
  - exfalso. exact (N0 m A1).
  - right. split; [exact (scope_detached p0 dn m0 L0 S0)|exact (scope_detached p dn m L S1)].
Qed.

(* Clause B of the agreement, from the forward clause: u (the caller's
   reading, at C) has every leaf of h0 (the chain's root family, read at
   its inputs Ch0), whose items have distinct methods, at most one
   detached. *)
Theorem chain_cover : forall u h0, (forall l, ResultLeaf W h0 l -> ResultLeaf W u l) ->
  forall C out C' ks, ProvideResult W u C out C' ks ->
  forall Ch0 outputs keys, ProvideResult W h0 Ch0 (fun t => TAbs t) outputs keys ->
  Platform.MethodsDistinct W keys -> (forall key, In key keys -> exists r m a, key = IMth r m a) ->
  forall news th, c_provisions C' = (news ++ c_provisions C)%list ->
  (forall r m g, mlook r m (mths_of news) = Some g ->
     exists key, In key keys /\ app_subst_item th (rkey_of outputs key) = rkey C' r m) ->
  forall key, In key keys -> exists r m g,
    mlook r m (mths_of news) = Some g /\ app_subst_item th (rkey_of outputs key) = rkey C' r m.
Proof.
  intros u h0 Lv C out C' ks PR Ch0 outputs keys PRf MD KeysM news th Enews Fwd key Ik.
  destruct PRf as (d0 & ps0 & a0 & r0 & b0 & outs0 & es0 & tm0 & D0 & FP0 & _ & OIE0 & RK0 & EO).
  pose proof Ik as Ik'. rewrite RK0 in Ik'. apply in_flat_map in Ik' as [e [Ie Ke]].
  destruct (KeysM key Ik) as (rk & mk & ak & Ek).
  destruct e as [t0|v t0 a'|f g0 a'|rt0 m0 g0 a'|u' fx|eql eqr]; cbn in Ke; try contradiction; destruct Ke as [Ke|[]]; subst key; try discriminate.
  destruct (proj2 entry_leaf _ _ _ _ _ _ OIE0 rt0 m0 g0 a' Ie) as (sp0 & s & dn & p0 & Isp0 & Lf0 & Mn0 & Ld0).
  destruct (functor_decl d0 _ _ _ _ FP0) as [n0 Ast0].
  assert (Lh : ResultLeaf W h0 (s, Some dn)) by (eapply RL; eassumption). pose proof (Lv _ Lh) as Lu.
  destruct PR as (d & ps & a & r & bb & outs & es & tm' & D & FP & _ & OIE & RK & EC).
  inversion Lu as [u1 d1 n1 ps1 a1 r1 b1 sp k D1 Ast1 Isp Lf]; subst.
  rewrite D in D1. injection D1 as <-. rewrite Ast1 in FP. injection FP as <- <- <- <-.
  destruct (items_member _ _ _ _ _ _ OIE sp Isp) as (t1 & e1 & t2 & Hm & Inc).
  destruct (leaf_entry _ _ _ Lf s dn eq_refl _ _ _ _ Hm) as (rt & m & g & a2 & p & I & Mn & Ld).
  (* the caller's new provisions are its entries' *)
  assert (En : news = flat_map (entry_provision (c_sub C) (fun a => a)) (rev es)).
  { cbn in Enews. symmetry. exact (app_inv_tail _ _ _ Enews). }
  assert (Ip : In (mkey (app_subst (c_sub C) rt) m, ProvidedMethod (sig_subst (c_sub C) g) a2) news).
  { rewrite En. apply in_flat_map. exists (NMth rt m g a2). split; [apply (proj1 (in_rev _ _)), Inc, I|left; reflexivity]. }
  destruct (mlook_in_some _ _ _ _ _ Ip) as [g' Lg].
  destruct (Fwd _ _ _ Lg) as [key' [Ik2 Ek2]].
  destruct (KeysM key' Ik2) as (r2 & m2 & a3 & ->).
  assert (Em : m2 = m).
  { cbn [rkey_of] in Ek2. unfold rkey in Ek2. cbn [app_subst_item] in Ek2. injection Ek2 as _ Em _. exact Em. }
  subst m2.
  assert (Same : m0 = m \/ (Platform.DetachedMethod W m0 /\ Platform.DetachedMethod W m)).
  { apply (named_same p0 p s dn m0 m); [exact (Ld0 (decl_loaded h0 d0 D0))|exact (Ld (decl_loaded u d D))|exact Mn0|exact Mn]. }
  assert (EkK : IMth r2 m a3 = mkey (app_subst (c_sub Ch0) rt0) m0)
    by (apply (MD (IMth r2 m a3) (mkey (app_subst (c_sub Ch0) rt0) m0) r2 _ m m0 a3 [] Ik2 Ik eq_refl eq_refl);
        destruct Same as [->|[S1 S2]]; [left; reflexivity|right; exact (conj S2 S1)]).
  exists (app_subst (c_sub C) rt), m, g'. split; [exact Lg|]. rewrite <- EkK. exact Ek2.
Qed.

(* ---- a copy of a method-only result has method entries only ------------- *)

(* A result holds no equation ([D90]), so its reading has no equation entry. *)
Lemma output_no_eq :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' -> forall l r, ~ In (NEq l r) es) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' -> forall l r, ~ In (NEq l r) es).
Proof.
  apply (PublicFormation.oies_mut W (fun p ob tm sp es tm' _ => forall l r, ~ In (NEq l r) es)
                                   (fun p ob tm sps es tm' _ => forall l r, ~ In (NEq l r) es));
    intros; intro I;
    first [ solve [cbn in I; intuition discriminate]
          | solve [match goal with H : forall l r, ~ In (NEq l r) ?es, I : In (NEq _ _) ?es |- _ => exact (H _ _ I) end]
          | (apply in_app_or in I; destruct I as [I|I];
             match goal with H : forall l r, ~ In (NEq l r) _ |- _ => exact (H _ _ I) end) ].
Qed.

Definition Plain (e : entry) : Prop := match e with NMth _ _ _ _ => False | _ => True end.
Definition esym (e : entry) : nat :=
  match e with NTy t => t | NVal v _ _ => v | NFn f _ _ => f | NFun u _ => u | NMth _ m _ _ => m | NEq _ _ => 0 end.

(* A plain entry of a reading is a type its items name, or comes from a
   plain leaf of its items. *)
Definition PlainFrom (p : string) (sp : spec) (e : entry) : Prop :=
  match e with NTy t => SpecTypes W p sp t | NMth _ _ _ _ => False | _ => ResultSpecLeaf W p sp (esym e, None) end.

Lemma entry_plain_leaf :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
     forall e, In e es -> Plain e -> PlainFrom p sp e) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
     forall e, In e es -> Plain e -> exists sp, In sp sps /\ PlainFrom p sp e).
Proof.
  apply (PublicFormation.oies_mut W
    (fun p ob tm sp es tm' _ => forall e, In e es -> Plain e -> PlainFrom p sp e)
    (fun p ob tm sps es tm' _ => forall e, In e es -> Plain e -> exists sp, In sp sps /\ PlainFrom p sp e)).
  - intros p ob tm pa a bs t R K B e [<-|[]] _. cbn. eapply STType; eassumption.
  - intros; contradiction.
  - intros p ob tm a pa s0 d n t ty0 bs ap R D Ast B T App e [<-|[]] _. cbn.
    eapply RSLPlain; [exact R|exact D| |]; rewrite Ast; [intros n' its E; discriminate|reflexivity].
  - intros p ob tm a pa s0 g bs ap d rcv n ps rt R K D Ast B Sig App e [<-|[]] _. cbn.
    eapply RSLPlain; [exact R|exact D| |]; rewrite Ast; [intros n' its E; discriminate|destruct rcv; reflexivity].
  - intros p ob tm a bs pa u' d n ps args res fx R D Ast B Fx Rd e [<-|[]] _. cbn.
    eapply RSLPlain; [exact R|exact D| |]; rewrite Ast; [intros n' its E; discriminate|reflexivity].
  - intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sig App e [<-|[]] Pl. contradiction.
  - intros p ob tm a pa c d n its sps bs es tm' R D Ast Esps B Its IH e I Pl.
    destruct (IH e I Pl) as (sp & Isp & X).
    rewrite <- citem_specs_map, <- Esps in Isp.
    destruct e; cbn in X |- *; try contradiction; [eapply STCtx; eassumption|..]; eapply RSLContext; eassumption.
  - intros; contradiction.
  - intros p ob tm sp rest es1 tm1 es2 tm2 H1 IH1 H2 IH2 e I Pl.
    apply in_app_or in I as [I|I].
    + exists sp. split; [left; reflexivity|exact (IH1 e I Pl)].
    + destruct (IH2 e I Pl) as (sp' & Isp & X). exists sp'. split; [right; exact Isp|exact X].
Qed.

Lemma already_kind : forall a k, kind_of a = Some k -> k = DKUnit \/ k = DKTag \/ k = DKAlias ->
  result_already_provided a = true.
Proof.
  intros a k K Ek. destruct a as [| | | | | |rcv n ps rt body| | ]; cbn in K; try discriminate;
    try (destruct rcv); injection K as <-; flat; discriminate || reflexivity.
Qed.

(* A plain leaf of an item has a plain entry in its reading. *)
Lemma leaf_plain_entry : forall p sp k, ResultSpecLeaf W p sp k -> forall s, k = (s, None) ->
  forall ob tm es tm', OutputItemEntries W p ob tm sp es tm' -> exists e, In e es /\ Plain e.
Proof.
  intros p sp k Lf. induction Lf as [p pa dn0 br s0 R|p pa br s0 d R D Nc Np|p pa br c d n its sp k R D Ast Isp Lf IH];
    intros s Ek ob tm es tm' OIE.
  - discriminate.
  - inversion OIE; subst; try (eexists; split; [left; reflexivity|exact Logic.I]).
    2: { exfalso. match goal with H : resolve_sym W p pa = Some ?x |- _ => assert (x = s0) by congruence; subst x end.
      match goal with H : w_decl W s0 = Some ?d' |- _ => assert (d' = d) by congruence; subst d' end.
      match goal with H : d_ast d = ADCtx _ _ |- _ => exact (Nc _ _ H) end. }
    exfalso. match goal with H : resolve_sym W p pa = Some ?x |- _ => assert (x = s0) by congruence; subst x end.
    match goal with K : kind_sym W s0 = Some ?k, O : _ \/ _ |- _ => rename K into Ks; rename O into Ok end.
    unfold kind_sym in Ks. rewrite D in Ks.
    destruct Ok as [->|[->|[->|[[-> (d' & n' & t' & i' & D' & A')]|[-> (d' & r' & n' & ps' & rt' & b' & D' & A')]]]]].
    + rewrite (already_kind _ _ Ks (or_introl eq_refl)) in Np. discriminate.
    + rewrite (already_kind _ _ Ks (or_intror (or_introl eq_refl))) in Np. discriminate.
    + rewrite (already_kind _ _ Ks (or_intror (or_intror eq_refl))) in Np. discriminate.
    + rewrite D in D'. injection D' as <-. rewrite A' in Np. discriminate.
    + rewrite D in D'. injection D' as <-. rewrite A' in Np, Ks. destruct r'; cbn in Ks; try discriminate.
  - subst k. inversion OIE; subst; try (flat; kmis).
    match goal with H : resolve_sym W p pa = Some ?c' |- _ => assert (c' = c) by congruence; subst c' end.
    match goal with H : w_decl W c = Some ?d' |- _ => assert (d' = d) by congruence; subst d' end.
    match goal with H : d_ast d = ADCtx _ (map CISpec _) |- _ => rewrite H in Ast; injection Ast as -> Ei; subst its end.
    rewrite citem_specs_map in Isp.
    match goal with H : OutputItemsEntries W _ _ _ _ _ _ |- _ => rename H into HI end.
    destruct (items_member _ _ _ _ _ _ HI sp Isp) as (t1 & e1 & t2 & Hm & Inc).
    destruct (IH s eq_refl _ _ _ _ Hm) as (e & I & Pl). exists e. split; [exact (Inc _ I)|exact Pl].
Qed.

(* A copy u of a method-only family h0 (the same leaves, no outputs)
   adds methods only to the caller's provisions. *)
Theorem copy_news_methods : forall u h0, (forall l, ResultLeaf W u l -> ResultLeaf W h0 l) ->
  (forall t, ~ FunOutputs W u t) -> (forall t, ~ FunOutputs W h0 t) ->
  forall Ch0 out0 outputs keys, ProvideResult W h0 Ch0 out0 outputs keys ->
  (forall key, In key keys -> exists r m a, key = IMth r m a) ->
  forall C out C' ks, ProvideResult W u C out C' ks ->
  exists news, c_provisions C' = (news ++ c_provisions C)%list /\
    (forall k p, In (k, p) news -> exists r m g a, k = mkey r m /\ p = ProvidedMethod g a).
Proof.
  intros u h0 Lv NoU NoH Ch0 out0 outputs keys PRf KeysM C out C' ks PR.
  destruct PR as (d & ps & a & r & bb & outs & es & tm' & D & FP & _ & OIE & _ & EC).
  exists (flat_map (entry_provision (c_sub C) (fun a => a)) (rev es)). split; [subst C'; reflexivity|].
  intros k pr I. apply in_flat_map in I as [e [Ie Ip]]. apply (proj2 (in_rev _ _)) in Ie.
  destruct e as [t0|v t0 a'|f g0 a'|rt m g a'|u' fx|eql eqr]; cbn in Ip;
    [contradiction| | | destruct Ip as [E|[]]; injection E as <- <-; do 4 eexists; split; reflexivity| |contradiction]; exfalso.
  all: destruct (proj2 entry_plain_leaf _ _ _ _ _ _ OIE _ Ie Logic.I) as (sp & Isp & Lf); cbn in Lf.
  all: destruct (functor_decl d _ _ _ _ FP) as [n Ast].
  all: match goal with Lf : ResultSpecLeaf W _ _ ?l |- _ => assert (Lh : ResultLeaf W h0 l) by (apply Lv; eapply RL; eassumption) end.
  all: destruct PRf as (d0 & ps0 & a0 & r0 & b0 & outs0 & es0 & tm0 & D0 & FP0 & _ & OIE0 & RK0 & _).
  all: inversion Lh as [h1 d1 n1 ps1 a1 r1 b1 sp1 k1 D1 Ast1 Isp1 Lf1]; subst.
  all: rewrite D0 in D1; injection D1 as <-; rewrite Ast1 in FP0; injection FP0 as <- <- <- <-.
  all: destruct (items_member _ _ _ _ _ _ OIE0 sp1 Isp1) as (t1 & e1 & t2 & Hm & Inc).
  all: destruct (leaf_plain_entry _ _ _ Lf1 _ eq_refl _ _ _ _ Hm) as (e0 & I0 & Pl0).
  all: pose proof (Inc _ I0) as I0'.
  all: destruct e0 as [y0|v0 y1 a0'|f0 g0' a0'|rt0 m0 g0' a0'|u0 fx0|eql eqr]; try contradiction.
  all: try solve [exact (proj2 output_no_eq _ _ _ _ _ _ OIE0 _ _ I0')].
  all: try (destruct (proj2 entry_plain_leaf _ _ _ _ _ _ OIE0 _ I0' Logic.I) as (sp2 & Isp2 & St); cbn in St;
    apply (NoH y0); exists d0, ps1, a1, r1, b1, sp2;
    split; [exact D0|split; [rewrite Ast1; reflexivity|split; [exact Isp2|exact St]]]).
  all: match type of I0' with In ?e0 _ =>
    assert (Ik : In (match entry_key (c_sub Ch0) e0 with Some k => k | None => ITy 0 end) keys)
      by (rewrite RK0; apply in_flat_map; exists e0; split; [exact I0'|cbn; left; reflexivity]) end.
  all: destruct (KeysM _ Ik) as (r2 & m2 & a2 & E2); discriminate E2.
Qed.

End ChainAgree.
