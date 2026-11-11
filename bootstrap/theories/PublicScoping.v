(* Scoping of elaborated types (T29).

   PublicFormation's [FormedCx] says that a context's tables are ok and
   that its method keys and type-table keys are in scope ([ScopedTy]: in
   [scope_gens]). This file adds the matching
   facts for the rest of a context, [CxScoped]: its locals' types, its
   provisions' types and instantiations, and the frame's receiver type.

   - [ex_scoped] (a fixpoint alongside [PublicFormation.ex_ok]): an
     elaborated expression's type is scoped, and bind items keep
     [CxScoped]. A block may not let a type it made escape ([D78]), so a
     block's type is scoped in the context around it.
   - [literal_scoped]: a literal's type is scoped.
   - [demand_at_th_scoped]: a callee need read through θ at the call
     site is ok and scoped (rule B puts every requirement in force there,
     through the callee's merge class).
   - Formation of the initial contexts: [ctxof_scoped], [functx_scoped],
     and the steps the slice takes. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicFormation PublicContextFacts.
From NM Require PublicMerge.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Scoping.
Variable W : A.world.
Import A.

Definition LocalsScoped (C : cx) : Prop :=
  forall n t mu, In (n, (t, mu)) (c_locals C) -> ScopedTy W C t.
Definition ProvsScoped (C : cx) : Prop :=
  forall k p, In (k, p) (c_provisions C) -> GensIn (GenScoped W C) (provision_gens p).
Definition ThisScoped (F : frame) (C : cx) : Prop :=
  forall t, fr_this F = Some t -> ScopedTy W C t.
(* The canonical view consumed by the current family-key proof. The
   merge view and canonical storage facts below recover this view through
   a section, so method agreement can be stated without representatives. *)
Definition MethReadCanon (C : cx) (r : ty) (m : nat) (g : fsig) (a : inst) : Prop :=
  exists tm rt g0, SigOf W m tm (Some rt) g0 /\ canon C rt = r /\ g = canon_sig C g0 /\
    (kind_sym W m = Some DKMeth -> forall x v, SigMentions W m x -> nlook x tm = Some v ->
       In (x, canon C v) (theta_of C a)).
(* Agreement with a declaration uses its merge classes. The instantiation
   stores raw readings, compared here up to the context's merge. *)
Definition MethReadMerge (C : cx) (r : ty) (m : nat) (g : fsig) (a : inst) : Prop :=
  exists tm rt g0, SigOf W m tm (Some rt) g0 /\
    MergeEq (c_merge C) rt r /\ SigEq (c_merge C) g g0 /\
    (kind_sym W m = Some DKMeth -> forall x v, SigMentions W m x -> nlook x tm = Some v ->
      exists u, In (x, u) (known a) /\ MergeEq (c_merge C) u v).

(* Canonical storage is retained while the rules still emit canonical keys. *)
Definition MethodCanonical (C : cx) (r : ty) (g : fsig) : Prop :=
  canon C r = r /\ canon_sig C g = g.

Definition MethRead (C : cx) (r : ty) (m : nat) (g : fsig) (a : inst) : Prop :=
  MethReadMerge C r m g a /\ MethodCanonical C r g.

Lemma canon_sig_idempotent : forall C, CxSection C ->
  forall g, canon_sig C (canon_sig C g) = canon_sig C g.
Proof.
  intros C Section [ps ret]. unfold canon_sig, sig_subst. cbn.
  f_equal.
  - rewrite map_map. apply map_ext. intro t. exact (section_canonical C Section t).
  - exact (section_canonical C Section ret).
Qed.

Lemma methread_merge_iff : forall C r m g a, CxSection C ->
  MethRead C r m g a <-> MethReadCanon C r m g a.
Proof.
  intros C r m g a Section. split.
  - intros [(tm & rt & g0 & Sig & Receiver & Signature & Theta) [Rc Gc]].
    exists tm, rt, g0. split; [exact Sig|]. split.
    + apply (context_canon_iff_mergeeq C Section) in Receiver. now rewrite Rc in Receiver.
    + split.
      * apply (PublicMerge.section_sig_eq Section) in Signature.
        change (canon_sig C g = canon_sig C g0) in Signature. now rewrite Gc in Signature.
      * intros Km x v Mentions Lookup. destruct (Theta Km x v Mentions Lookup) as [u [Inu Equal]].
        apply (context_canon_iff_mergeeq C Section) in Equal.
        unfold theta_of. apply in_map_iff. exists (x, u). split; [change ((x, canon C u) = (x, canon C v)); now rewrite Equal|exact Inu].
  - intros (tm & rt & g0 & Sig & <- & -> & Theta).
    split.
    + exists tm, rt, g0. split; [exact Sig|]. split.
      * apply PublicMerge.merge_eq_read. exact (proj1 Section).
      * split.
        -- apply (PublicMerge.section_sig_eq Section), canon_sig_idempotent. exact Section.
        -- intros Km x v Mentions Lookup.
           pose proof (Theta Km x v Mentions Lookup) as InTheta.
           unfold theta_of in InTheta. apply in_map_iff in InTheta as [[y u] [Equal Inu]].
           cbn in Equal. injection Equal as -> Equal. exists u. split; [exact Inu|].
           apply (context_canon_iff_mergeeq C Section). exact Equal.
    + split; [apply section_canonical|apply canon_sig_idempotent]; exact Section.
Qed.

Definition MethsReadCanon (C : cx) : Prop :=
  (forall t, canon C (canon C t) = canon C t) /\
  (forall x v, In (x, v) (c_ty C) -> canon C v = v) /\
  (forall r m g a, In (mkey r m, ProvidedMethod g a) (c_provisions C) -> MethReadCanon C r m g a).

Definition MethsRead (C : cx) : Prop :=
  CxSection C /\
  (forall x v, In (x, v) (c_ty C) -> canon C v = v) /\
  (forall r m g a, In (mkey r m, ProvidedMethod g a) (c_provisions C) -> MethRead C r m g a).

Lemma methsread_canon : forall C, MethsRead C -> MethsReadCanon C.
Proof.
  intros C [Section [Table Methods]]. split; [exact (section_canonical C Section)|].
  split; [exact Table|]. intros r m g a InMethod.
  apply (methread_merge_iff C r m g a Section), Methods. exact InMethod.
Qed.

Lemma methsread_canon_iff : forall C, CxSection C ->
  MethsRead C <-> MethsReadCanon C.
Proof.
  intros C Section. split; [apply methsread_canon|].
  intros [_ [Table Methods]]. split; [exact Section|]. split; [exact Table|].
  intros r m g a InMethod. apply (methread_merge_iff C r m g a Section), Methods. exact InMethod.
Qed.

Lemma methsread_receiver_canonical : forall C r m g a,
  MethsRead C -> In (mkey r m, ProvidedMethod g a) (c_provisions C) ->
  canon C r = r.
Proof.
  intros C r m g a [_ [_ Methods]] InMethod.
  exact (proj1 (proj2 (Methods r m g a InMethod))).
Qed.

Lemma methsread_mlook_canonical : forall C r m g,
  MethsRead C -> KeysMatch (c_provisions C) ->
  mlook r m (c_mths C) = Some g -> canon C r = r.
Proof.
  intros C r m g Methods Keys Lookup.
  destruct (mths_ilook (c_provisions C) r m g Keys Lookup) as [a Entry].
  exact (methsread_receiver_canonical C r m g a Methods (ilook_in_key _ _ _ _ Entry)).
Qed.

Definition CxScoped (F : frame) (C : cx) : Prop :=
  LocalsScoped C /\ ProvsScoped C /\ ThisScoped F C /\ MethsRead C.
Definition OptScoped (C : cx) (o : option ty) : Prop := forall t, o = Some t -> ScopedTy W C t.

(* ---- method provisions read their declarations ------------------------------ *)

Lemma Forall2_In_r' : forall X Y (R : X -> Y -> Prop) l l' y, Forall2 R l l' -> In y l' -> exists x, In x l /\ R x y.
Proof.
  intros X Y R l l' y F. induction F as [|a b l l' Rab Rest IH]; intros Iy; [contradiction|].
  destruct Iy as [<-|Iy]; [exists a; split; [left; reflexivity|exact Rab]|].
  destruct (IH Iy) as [x [Ix Rx]]. exists x. split; [right; exact Ix|exact Rx].
Qed.

Lemma methread_sub : forall C C' r m g a,
  c_sub C' = c_sub C -> c_merge C' = c_merge C ->
  MethRead C r m g a -> MethRead C' r m g a.
Proof.
  intros C C' r m g a Sub Merge [Reading Canonical]. split.
  - unfold MethReadMerge in *. now rewrite Merge.
  - unfold MethodCanonical, canon_sig, canon in *. now rewrite Sub.
Qed.

Lemma methsread_narrow : forall C C', c_ty C' = c_ty C -> c_sub C' = c_sub C ->
  c_merge C' = c_merge C ->
  incl (c_provisions C') (c_provisions C) -> MethsRead C -> MethsRead C'.
Proof.
  intros C C' Ety Esub Merge Inc [Section [Tc Pr]]. split; [|split].
  - eapply section_same_fields; eassumption.
  - intros x v Iq. unfold canon. rewrite Esub. rewrite Ety in Iq. exact (Tc x v Iq).
  - intros r m g a Iq. exact (methread_sub C C' r m g a Esub Merge (Pr r m g a (Inc _ Iq))).
Qed.

Lemma methsread_set_ty : forall C t tau, MethsRead C -> MethsRead (set_ty C t (canon C tau)).
Proof.
  intros C t tau [Section [Tc Pr]]. split; [exact Section|split].
  - intros x v [E|Iq]; [injection E as <- <-; exact (section_canonical C Section tau)|exact (Tc x v Iq)].
  - intros r m g a Iq. exact (methread_sub C _ r m g a eq_refl eq_refl (Pr r m g a Iq)).
Qed.

Lemma set_provision_in : forall C k p q, In q (c_provisions (set_provision C k p)) ->
  q = (k, p) \/ (In q (c_provisions C) /\ item_eqb k (fst q) = false).
Proof.
  intros C k p q [E|Iq]; [left; symmetry; exact E|right]. apply filter_In in Iq as [Iq B].
  split; [exact Iq|]. destruct (item_eqb k (fst q)); [discriminate|reflexivity].
Qed.

Lemma methsread_set_provision : forall C k p, MethsRead C ->
  (forall r m g a, k = mkey r m -> p = ProvidedMethod g a -> MethRead C r m g a) ->
  MethsRead (set_provision C k p).
Proof.
  intros C k p [Section [Tc Pr]] New. split; [exact Section|split; [exact Tc|]].
  intros r m g a Iq. apply (methread_sub C); [reflexivity|reflexivity|].
  destruct (set_provision_in C k p _ Iq) as [E|[Iq' _]];
    [injection E as E1 E2; exact (New r m g a (eq_sym E1) (eq_sym E2))|exact (Pr r m g a Iq')].
Qed.

Lemma methsread_provide : forall C k a C1, (forall r m, k <> mkey r m) -> MethsRead C1 -> MethsRead (provide C k a C1).
Proof.
  intros C k a C1 Nk M1. unfold provide.
  destruct (ilook k (c_provisions C1)) as [[t a0|g a0|g a0|fx]|]; try exact M1;
    apply methsread_set_provision; try exact M1; intros r m g' a' E _; exfalso; exact (Nk r m E).
Qed.

Lemma provide_tables' : forall C k a C1, c_ty (provide C k a C1) = c_ty C1 /\ c_sub (provide C k a C1) = c_sub C1 /\
  c_locals (provide C k a C1) = c_locals C1.
Proof.
  intros C k a C1. unfold provide. destruct (ilook k (c_provisions C1)) as [[t a0|g a0|g a0|fx]|]; repeat split.
Qed.

Lemma item_eqb_refl' : forall k, item_eqb k k = true.
Proof. intros k. apply (proj2 (item_eqb_spec k k)). reflexivity. Qed.

(* `bind R.m = …;` files the signature, then the instantiation. *)
Lemma provide_set_mth : forall C r m sg ap,
  provide C (mkey r m) ap (set_mth C r m sg) = set_provision (set_mth C r m sg) (mkey r m) (ProvidedMethod sg ap).
Proof. intros C r m sg ap. unfold provide, set_mth. cbn [c_provisions set_provision ilook]. rewrite item_eqb_refl'. reflexivity. Qed.

Lemma methsread_meth_bind : forall C r m sg ap, MethsRead C -> MethRead C r m sg ap ->
  MethsRead (provide C (mkey r m) ap (set_mth C r m sg)).
Proof.
  intros C r m sg ap [Section [Tc Pr]] New. rewrite provide_set_mth. split; [exact Section|split; [exact Tc|]].
  intros r' m' g a Iq. apply (methread_sub C); [reflexivity|reflexivity|].
  destruct (set_provision_in _ _ _ _ Iq) as [E|[Iq' B]].
  - injection E as -> -> -> ->. exact New.
  - unfold set_mth in Iq'. destruct (set_provision_in _ _ _ _ Iq') as [E|[Iq'' _]]; [|exact (Pr _ _ _ _ Iq'')].
    rewrite E in B. cbn [fst] in B. rewrite item_eqb_refl' in B. discriminate B.
Qed.

Lemma select_in : forall (P : nat -> Prop) l sel x, Select P l sel -> In x l -> P x -> In x sel.
Proof.
  intros P l sel x S. induction S as [|y ys sel' Py S IH|y ys sel' Ny S IH]; intros Ix Px; [contradiction| |].
  - destruct Ix as [<-|Ix]; [left; reflexivity|right; exact (IH Ix Px)].
  - destruct Ix as [<-|Ix]; [contradiction|exact (IH Ix Px)].
Qed.

(* A method's θ records every symbol its signature mentions (sigclose). *)
Lemma appof_mentioned : forall m tm a x, AppOf W m tm a -> kind_sym W m = Some DKMeth -> SigMentions W m x ->
  In (x, nlook x tm) a.
Proof.
  intros m tm a x (reqs & -> & [[_ (sel & extra & Sel & [_ Out] & ->)]|[Nk _]]) K Mx; [|exfalso; exact (Nk K)].
  apply in_map_iff. exists x. split; [reflexivity|]. apply in_or_app.
  destruct (in_dec Nat.eq_dec x (w_reqs W m)) as [I|NI];
    [left; exact (select_in _ _ _ x Sel I Mx)|right; exact (proj2 (Out x) (conj Mx NI))].
Qed.

Lemma theta_in : forall C a x v, In (x, Some v) a -> In (x, canon C v) (theta_of C a).
Proof.
  intros C a x v I. unfold theta_of. apply in_map_iff. exists (x, v). split; [reflexivity|].
  unfold known. apply in_flat_map. exists (x, Some v). split; [exact I|left; reflexivity].
Qed.

Lemma methread_of : forall C m tm rt g0 a, CxSection C -> SigOf W m tm (Some rt) g0 -> AppOf W m tm a ->
  MethRead C (canon C rt) m (canon_sig C g0) a.
Proof.
  intros C m tm rt g0 a Section Sg App. apply (methread_merge_iff _ _ _ _ _ Section). exists tm, rt, g0. split; [exact Sg|split; [reflexivity|split; [reflexivity|]]].
  intros K x v Mx L. apply theta_in. rewrite <- L. exact (appof_mentioned m tm a x App K Mx).
Qed.

(* A telescope's context: each method entry a reading at its own table,
   frozen under an idempotent merge. *)
Definition EntriesRead (es : list entry) : Prop :=
  forall rt m g a, In (NMth rt m g a) es -> exists tmx, SigOf W m tmx (Some rt) g /\ AppOf W m tmx a.

Lemma methsread_entries : forall σ classes tm es, SectionOf classes σ ->
  EntriesRead es -> MethsRead (entries_cx σ classes tm es).
Proof.
  intros σ classes tm es Section Es.
  pose proof (section_canonical (entries_cx σ classes tm es) Section) as Id.
  change (forall t, app_subst σ (app_subst σ t) = app_subst σ t) in Id.
  split; [exact Section|split].
  - intros x v Iq. cbn in Iq. apply in_map_iff in Iq as [[x0 v0] [E _]]. injection E as _ <-. exact (Id v0).
  - intros r m g a Iq. cbn [c_provisions entries_cx] in Iq. apply in_flat_map in Iq as [e [Ie Iq]].
    destruct e as [t|v t a0|f g0 a0|rt m0 g0 a0|u fx|eql eqr]; cbn in Iq; try contradiction;
      destruct Iq as [E|[]]; try discriminate.
    try unfold mkey in E. injection E as Er Em Eg Ea. subst r m0 g a.
    destruct (Es rt m g0 a0 Ie) as [tmx [Sg App]].
    apply (methread_merge_iff (entries_cx σ classes tm es) _ _ _ _ Section). exists tmx, rt, g0.
    split; [exact Sg|split; [reflexivity|split; [reflexivity|]]].
    intros K x v Mx L. pose proof (appof_mentioned m tmx a0 x App K Mx) as I0. rewrite L in I0.
    match goal with |- In _ (theta_of _ ?a') => assert (I : In (x, Some (app_subst σ v)) a') end.
    { apply in_map_iff. exists (x, Some v). split; [reflexivity|exact I0]. }
    pose proof (theta_in (entries_cx σ classes tm es) _ _ _ I) as T.
    change (canon (entries_cx σ classes tm es) (app_subst σ v)) with (app_subst σ (app_subst σ v)) in T.
    rewrite Id in T. exact T.
Qed.

Lemma entries_read :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> EntriesRead es) /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' -> EntriesRead es).
Proof.
  apply (ies_mut W (fun p ob tm sp es tm' _ => EntriesRead es) (fun p ob tm sps es tm' _ => EntriesRead es));
    try (unfold EntriesRead; intros; match goal with H : In (NMth _ _ _ _) _ |- _ => cbn in H; solve [intuition discriminate] end).
  - intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sg App rt0 m0 g0 a0 [E|[]].
    injection E as <- <- <- <-. exists (bs ++ ob ++ tm). split; assumption.
  - intros p ob tm a pa c d n its bs es tm' R D Ast B Inner IH. exact IH.
  - intros p ob tm sp r es1 tm1 es2 tm2 H1 IH1 H2 IH2 rt0 m0 g0 a0 I0.
    apply in_app_or in I0 as [I0|I0]; [exact (IH1 _ _ _ _ I0)|exact (IH2 _ _ _ _ I0)].
  - intros p ob tm l r es1 tm1 tl tr rest es2 tm2 H1 IH1 Tl Tr H2 IH2 rt0 m0 g0 a0 I0.
    apply in_app_or in I0 as [I0|[I0|I0]]; [exact (IH1 _ _ _ _ I0)|discriminate I0|exact (IH2 _ _ _ _ I0)].
Qed.

Lemma root_read : forall q es0 tm its es tm', RootItems W q es0 tm its es tm' -> forall rt m g a, ~ In (NMth rt m g a) es.
Proof.
  intros q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros rt0 m0 g0 a1 I; [exact I| |exact (IH _ _ _ _ I)].
  apply in_app_or in I as [I|I]; [apply in_map_iff in I as [x [E _]]; discriminate|].
  destruct I as [E|I]; [discriminate|exact (IH _ _ _ _ I)].
Qed.

(* A functor's result: each method entry a reading, of a method named in
   a declaration's module (so a loaded one). *)
Definition OutputRead (es : list entry) : Prop :=
  forall rt m g a, In (NMth rt m g a) es -> exists tmx, SigOf W m tmx (Some rt) g /\ AppOf W m tmx a /\
    exists s ds r0 dn, w_decl W s = Some ds /\ MethodNamed W (d_mod ds) r0 dn m.

Lemma output_read :
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' -> (exists s ds, w_decl W s = Some ds /\ d_mod ds = p) -> OutputRead es) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' -> (exists s ds, w_decl W s = Some ds /\ d_mod ds = p) -> OutputRead es).
Proof.
  apply (oies_mut W (fun p ob tm sp es tm' _ => (exists s ds, w_decl W s = Some ds /\ d_mod ds = p) -> OutputRead es)
                    (fun p ob tm sps es tm' _ => (exists s ds, w_decl W s = Some ds /\ d_mod ds = p) -> OutputRead es));
    try (unfold OutputRead; intros; match goal with H : In (NMth _ _ _ _) _ |- _ => cbn in H; solve [intuition discriminate] end).
  - intros p ob tm a pa dn r m rt g bs ap R Mn B Rv Sg App [s [ds [Ds Ep]]] rt0 m0 g0 a0 [E|[]].
    injection E as <- <- <- <-. exists (bs ++ ob ++ tm). split; [exact Sg|split; [exact App|]].
    exists s, ds, r, dn. split; [exact Ds|rewrite Ep; exact Mn].
  - intros p ob tm a pa c d n its sps bs es tm' R D Ast Esps B Inner IH _. apply IH. exists c, d. split; [exact D|reflexivity].
  - intros p ob tm sp r es1 tm1 es2 tm2 H1 IH1 H2 IH2 Dp rt0 m0 g0 a0 I0.
    apply in_app_or in I0 as [I0|I0]; [exact (IH1 Dp _ _ _ _ I0)|exact (IH2 Dp _ _ _ _ I0)].
Qed.

(* A functor's result read in C0: a new method provision is a reading,
   canonical in C0; the rest are C0's. *)
Lemma provide_result_meths : forall u C0 out C' ks, ProvideResult W u C0 out C' ks ->
  forall r m g a, In (mkey r m, ProvidedMethod g a) (c_provisions C') ->
  In (mkey r m, ProvidedMethod g a) (c_provisions C0) \/
  (In (mkey r m) ks /\ exists tmx rt g0, SigOf W m tmx (Some rt) g0 /\ AppOf W m tmx a /\ canon C0 rt = r /\ g = canon_sig C0 g0 /\
     exists s ds r0 dn, w_decl W s = Some ds /\ MethodNamed W (d_mod ds) r0 dn m).
Proof.
  intros u C0 out C' ks (d & ps & a0 & r0 & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->) r m g a Iq.
  pose proof (proj2 output_read _ _ _ _ _ _ OIE (ex_intro _ u (ex_intro _ d (conj D eq_refl)))) as Rd.
  cbn [output_cx c_provisions] in Iq. apply in_app_or in Iq as [Iq|Iq]; [right|left; exact Iq].
  apply in_flat_map in Iq as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie.
  destruct e as [t|v t a1|f g1 a1|rt m1 g1 a1|u1 fx|eql eqr]; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; try discriminate.
  try unfold mkey in E. injection E as Er Em Eg Ea. subst r m1 g a1.
  split.
  - rewrite RK. apply in_flat_map. exists (NMth rt m g1 a). split; [exact Ie|left; reflexivity].
  - destruct (Rd rt m g1 a Ie) as [tmx [Sg [App Nm]]]. exists tmx, rt, g1. repeat split; try assumption.
Qed.

Lemma methsread_provide_result : forall u C0 out C' ks, ProvideResult W u C0 out C' ks -> MethsRead C0 -> MethsRead C'.
Proof.
  intros u C0 out C' ks PR [Section [Tc Pr]].
  destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [_ [outs [_ Ety]]]].
  pose proof (provided_result_merge W _ _ _ _ _ PR) as Merge.
  assert (Section' : CxSection C') by (eapply section_same_fields; eassumption).
  split; [exact Section'|split].
  - intros x v Iq. rewrite Ety in Iq. apply in_map_iff in Iq as [[x0 v0] [E _]]. injection E as _ <-.
    change (canon C0 v0) with (app_subst (c_sub C0) v0). rewrite <- Esub.
    exact (section_canonical C' Section' v0).
  - intros r m g a Iq. apply (methread_sub C0); [exact Esub|exact Merge|].
    destruct (provide_result_meths u C0 out C' ks PR r m g a Iq) as [I|[_ (tmx & rt & g0 & Sg & App & <- & -> & _)]];
      [exact (Pr r m g a I)|exact (methread_of C0 m tmx rt g0 a Section Sg App)].
Qed.

(* A reading at a canonical table is canonical. *)
Lemma tyof_canon : forall C p tm th a t, (forall x v, In (x, v) tm -> canon C v = v) ->
  (forall x, th = Some x -> canon C x = x) -> TyOf W p tm th a t -> canon C t = t.
Proof.
  intros C p tm th a t Tc Th T. rewrite <- (app_subst_nil t) at 2.
  apply (tyof_rel W (c_sub C) [] p tm th a t T tm th t T).
  - intros k v1 v2 L1 L2. rewrite L1 in L2. injection L2 as <-. rewrite app_subst_nil. exact (Tc k v1 (nlook_in _ _ _ _ L1)).
  - intros x1 x2 E1 E2. rewrite E1 in E2. injection E2 as <-. rewrite app_subst_nil. exact (Th x1 E1).
Qed.

Lemma brackets_canon : forall C p tm g a bs, (forall x v, In (x, v) tm -> canon C v = v) ->
  Brackets W p tm g a bs -> forall x v, In (x, v) bs -> canon C v = v.
Proof.
  intros C p tm g a bs Tc B. revert Tc. destruct B as [p tm g|p tm g bs l F]; intros Tc; [intros x v []|].
  intros x v Iq. apply (proj2 (in_rev _ _)) in Iq. destruct (Forall2_In_r' _ _ _ _ _ _ F Iq) as [b0 [_ (sp & sa & _ & _ & _ & _ & T)]].
  exact (tyof_canon C _ _ None _ _ Tc ltac:(intros x0 E; discriminate E) T).
Qed.

Lemma sigof_canon : forall C s tm th g, (forall x v, In (x, v) tm -> canon C v = v) ->
  (forall x, th = Some x -> canon C x = x) -> SigOf W s tm th g -> canon_sig C g = g.
Proof.
  intros C s tm th [ps rt] Tc Th Sg.
  assert (Ag : forall k v1 v2, nlook k tm = Some v1 -> nlook k tm = Some v2 -> app_subst (c_sub C) v1 = app_subst [] v2).
  { intros k v1 v2 L1 L2. rewrite L1 in L2. injection L2 as <-. rewrite app_subst_nil. exact (Tc k v1 (nlook_in _ _ _ _ L1)). }
  assert (Ag2 : forall x1 x2, th = Some x1 -> th = Some x2 -> app_subst (c_sub C) x1 = app_subst [] x2).
  { intros x1 x2 E1 E2. rewrite E1 in E2. injection E2 as <-. rewrite app_subst_nil. exact (Th x1 E1). }
  destruct (sig_rel W (c_sub C) [] s tm tm th th _ _ Sg Sg Ag Ag2) as [Ep Er].
  cbn in Ep, Er. unfold canon_sig. cbn. rewrite app_subst_nil in Er. f_equal; [|exact Er].
  change (map (app_subst (c_sub C)) ps = ps). rewrite Ep. clear. induction ps as [|t r IH]; [reflexivity|]. cbn. rewrite app_subst_nil, IH. reflexivity.
Qed.

(* ---- generations ------------------------------------------------------------ *)

Lemma scoped_mono : forall C C' t, incl (scope_gens C) (scope_gens C') -> ScopedTy W C t -> ScopedTy W C' t.
Proof. intros C C' t Inc Sc y Iy. exact (inscope_mono W C C' y Inc (Sc y Iy)). Qed.

Lemma gensin_mono : forall (K K' : nat -> Prop) l, (forall y, K y -> K' y) -> GensIn K l -> GensIn K' l.
Proof. intros K K' l Imp G y Iy. exact (Imp y (G y Iy)). Qed.

Lemma tm_scoped : forall C y, In y (tm_gens (c_ty C)) -> GenScoped W C y.
Proof. intros C y Iy. exact (ty_scope C y Iy). Qed.

Lemma gens_canon : forall C t y, In y (gens (canon C t)) -> In y (gens t) \/ In y (scope_gens C).
Proof.
  intros C t y Iy. unfold canon in Iy. destruct (gens_subst t (c_sub C) y Iy) as [z [Iz [[_ ->]|[u [L Iu]]]]];
    [left; exact Iz|right; exact (scope_sub C z u y (lookup_subst_in_pair _ _ _ L) Iu)].
Qed.

Lemma never_gens : forall y, ~ In y (gens never).
Proof. intros y []. Qed.

Lemma unit_gens : forall y, ~ In y (gens TUnit).
Proof. intros y []. Qed.

Lemma sig_ret_gens : forall p g a, provision_signature p = Some g -> provision_inst p = a ->
  incl (gens (sg_ret g)) (provision_gens p).
Proof.
  intros p g a Sig _ x Ix. unfold provision_gens. rewrite Sig. apply in_or_app. left. apply in_flat_map.
  exists (sg_ret g). split; [left; reflexivity|exact Ix].
Qed.

Lemma vals_in : forall C v t, nlook v (c_vals C) = Some t -> exists a, In (IVal v, ProvidedVal t a) (c_provisions C).
Proof.
  intros C v t L. apply nlook_in in L. unfold c_vals in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->. eexists. exact Iq.
Qed.

Lemma fns_in : forall C f g, nlook f (c_fns C) = Some g -> exists a, In (IFn f, ProvidedFn g a) (c_provisions C).
Proof.
  intros C f g L. apply nlook_in in L. unfold c_fns in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as -> ->. eexists. exact Iq.
Qed.

Lemma mths_in : forall C r m g, mlook r m (c_mths C) = Some g -> exists k a, In (k, ProvidedMethod g a) (c_provisions C).
Proof.
  intros C r m g L. destruct (mlook_in _ _ _ _ L) as [q L']. unfold c_mths in L'. apply in_flat_map in L' as [[k p] [Iq Ik]].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as _ E2; subst. do 2 eexists. exact Iq.
Qed.

Lemma val_scoped : forall C v t, ProvsScoped C -> nlook v (c_vals C) = Some t -> ScopedTy W C t.
Proof.
  intros C v t Ps L y Iy. destruct (vals_in C v t L) as [a Iq]. apply (Ps _ _ Iq). unfold provision_gens. cbn.
  apply in_or_app. left. apply in_or_app. left. exact Iy.
Qed.

Lemma fn_ret_scoped : forall C f g, ProvsScoped C -> nlook f (c_fns C) = Some g -> ScopedTy W C (sg_ret g).
Proof.
  intros C f g Ps L y Iy. destruct (fns_in C f g L) as [a Iq]. apply (Ps _ _ Iq).
  exact (sig_ret_gens (ProvidedFn g a) g a eq_refl eq_refl y Iy).
Qed.

Lemma mth_ret_scoped : forall C r m g, ProvsScoped C -> mlook r m (c_mths C) = Some g -> ScopedTy W C (sg_ret g).
Proof.
  intros C r m g Ps L y Iy. destruct (mths_in C r m g L) as [k [a Iq]]. apply (Ps _ _ Iq).
  exact (sig_ret_gens (ProvidedMethod g a) g a eq_refl eq_refl y Iy).
Qed.

Lemma tyof_tm_gens : forall p tm this a t y, TyOf W p tm this a t -> In y (gens t) ->
  In y (tm_gens tm) \/ (exists x, this = Some x /\ In y (gens x)).
Proof. intros p tm this a t y T Iy. exact (tyof_gens W _ _ _ _ _ T y Iy). Qed.

Lemma sigof_gens : forall s tm this g y, SigOf W s tm this g -> In y (sig_gens g) ->
  In y (tm_gens tm) \/ (exists x, this = Some x /\ In y (gens x)).
Proof.
  intros s tm this [ps r] y (d & rcv & n & args & rt & body & D & Ast & Ps & R) Iy.
  cbn [sg_ret sg_params] in Ps, R. unfold sig_gens in Iy. cbn [sg_ret sg_params flat_map] in Iy.
  apply in_app_or in Iy as [Ir|Ip].
  - destruct rt as [rta|]; cbn in R; [exact (tyof_gens W _ _ _ _ _ R y Ir)|subst r; contradiction].
  - apply in_flat_map in Ip as [t [It Iy]]. cbn in It. clear R D Ast. revert It. induction Ps as [|pt t' ps' ts' Ht Fr IH]; intros It; [contradiction|].
    destruct It as [<-|It]; [exact (tyof_gens W _ _ _ _ _ Ht y Iy)|exact (IH It)].
Qed.

Lemma sig_ret_in : forall g y, In y (gens (sg_ret g)) -> In y (sig_gens g).
Proof. intros g y Iy. unfold sig_gens. cbn. apply in_or_app. left. exact Iy. Qed.

Lemma sig_param_in : forall g t y, In t (sg_params g) -> In y (gens t) -> In y (sig_gens g).
Proof. intros g t y It Iy. unfold sig_gens. cbn. apply in_or_app. right. apply in_flat_map. exists t. split; assumption. Qed.

Lemma provision_gens_sig : forall p g y, provision_signature p = Some g ->
  In y (provision_gens p) -> In y (sig_gens g) \/ In y (inst_gens (provision_inst p)).
Proof.
  intros p g y Sig Iy. unfold provision_gens in Iy. rewrite Sig in Iy. apply in_app_or in Iy as [Iy|Iy]; [left|right]; exact Iy.
Qed.

(* ---- context steps ---------------------------------------------------------- *)

Lemma scope_add_local : forall C n t mu, scope_gens (add_local C n t mu) = scope_gens C.
Proof. reflexivity. Qed.

Lemma add_local_scoped : forall F C n t mu, CxScoped F C -> ScopedTy W C t -> CxScoped F (add_local C n t mu).
Proof.
  intros F C n t mu [Ls [Ps Ts]] St. split; [|split; [exact Ps|exact Ts]].
  intros n' t' mu' [E|Iq]; [injection E as -> -> ->; exact St|exact (Ls _ _ _ Iq)].
Qed.

Lemma add_locals_scoped : forall F mu l C, CxScoped F C -> (forall n t, In (n, t) l -> ScopedTy W C t) ->
  CxScoped F (fold_left (fun C q => add_local C (fst q) (snd q) mu) l C).
Proof.
  intros F mu l. induction l as [|[n t] r IH]; intros C Cs Sc; cbn; [exact Cs|].
  apply IH; [apply add_local_scoped; [exact Cs|exact (Sc n t (or_introl eq_refl))]|].
  intros n' t' Iq. exact (Sc n' t' (or_intror Iq)).
Qed.

Lemma add_locals_scope : forall mu l C, scope_gens (fold_left (fun C q => add_local C (fst q) (snd q) mu) l C) = scope_gens C.
Proof. intros mu l. induction l as [|[n t] r IH]; intros C; cbn; [reflexivity|]. rewrite IH. reflexivity. Qed.

Lemma set_ty_scoped : forall F C t v, CxScoped F C -> CxScoped F (set_ty C t (canon C v)).
Proof.
  intros F C t v [Ls [Ps [Ts Mr]]]. pose proof (scope_set_ty C t (canon C v)) as Inc. split; [|split; [|split]].
  - intros n t' mu Iq. exact (scoped_mono _ _ _ Inc (Ls n t' mu Iq)).
  - intros k p Iq y Iy. exact (inscope_mono W _ _ y Inc (Ps k p Iq y Iy)).
  - intros t' E. exact (scoped_mono _ _ _ Inc (Ts t' E)).
  - exact (methsread_set_ty C t v Mr).
Qed.

Lemma set_provision_scoped : forall F C k p, CxScoped F C -> GensIn (GenScoped W C) (provision_gens p) ->
  (forall r m g a, k = mkey r m -> p = ProvidedMethod g a -> MethRead C r m g a) ->
  CxScoped F (set_provision C k p).
Proof.
  intros F C k p [Ls [Ps [Ts Mr]]] Sp New. split; [exact Ls|split; [|split; [exact Ts|exact (methsread_set_provision C k p Mr New)]]].
  intros k' p' [E|Iq]; [injection E as <- <-; exact Sp|]. apply filter_In in Iq as [Iq _]. exact (Ps _ _ Iq).
Qed.

(* The scoping parts of providing (any key): what bind items share. *)
Lemma provide_scoped3 : forall F C k a C1, LocalsScoped C1 /\ ProvsScoped C1 /\ ThisScoped F C1 -> GensIn (GenScoped W C1) (inst_gens a) ->
  LocalsScoped (provide C k a C1) /\ ProvsScoped (provide C k a C1) /\ ThisScoped F (provide C k a C1).
Proof.
  intros F C k a C1 Cs Sa. destruct (provide_tables' C k a C1) as [Ety [Esub Elo]].
  assert (Sc : scope_gens (provide C k a C1) = scope_gens C1) by exact (scope_gens_ext _ _ Ety Esub).
  pose proof Cs as [Ls [Ps Ts]]. split; [|split].
  - intros n t mu I. rewrite Elo in I. exact (scoped_ext W _ _ _ (eq_sym Sc) (Ls n t mu I)).
  - intros k' p' Iq y Iy. unfold GenScoped. rewrite Sc. revert Iq. unfold provide.
    destruct (ilook k (c_provisions C1)) as [p0|] eqn:L; [|intros Iq; exact (Ps k' p' Iq y Iy)].
    apply ilook_in_key in L. pose proof (Ps k p0 L) as S0.
    destruct p0 as [t a0|g a0|g a0|fx]; intros Iq; try exact (Ps k' p' Iq y Iy);
      (destruct (set_provision_in C1 k _ _ Iq) as [E|[Iq' _]]; [injection E as -> ->|exact (Ps k' p' Iq' y Iy)]);
      unfold provision_gens in Iy; cbn [provision_signature provision_inst] in Iy;
      apply in_app_or in Iy as [Iy|Iy]; try exact (Sa y Iy);
      apply S0; unfold provision_gens; cbn [provision_signature]; apply in_or_app; left; exact Iy.
  - intros t E. exact (scoped_ext W _ _ _ (eq_sym Sc) (Ts t E)).
Qed.

Lemma provide_scoped : forall F C k a C1, CxScoped F C1 -> GensIn (GenScoped W C1) (inst_gens a) ->
  (forall r m, k <> mkey r m) -> CxScoped F (provide C k a C1).
Proof.
  intros F C k a C1 Cs Sa Nk. destruct (provide_scoped3 F C k a C1 (conj (proj1 Cs) (conj (proj1 (proj2 Cs)) (proj1 (proj2 (proj2 Cs))))) Sa) as [Ls [Ps Ts]].
  exact (conj Ls (conj Ps (conj Ts (methsread_provide C k a C1 Nk (proj2 (proj2 (proj2 Cs))))))).
Qed.

Lemma set_val_scoped : forall F C v t, CxScoped F C -> ScopedTy W C t -> CxScoped F (set_val C v t).
Proof.
  intros F C v t Cs St. apply set_provision_scoped; [exact Cs| |intros r m g a E; discriminate E].
  intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  rewrite !app_nil_r in Iy. exact (St y Iy).
Qed.

Lemma set_sig_scoped : forall F C k g, CxScoped F C -> GensIn (GenScoped W C) (sig_gens g) ->
  CxScoped F (set_provision C k (ProvidedFn g [])).
Proof.
  intros F C k g Cs Sg. apply set_provision_scoped; [exact Cs| |intros r m g' a E E'; discriminate E'];
    intros y Iy; unfold provision_gens in Iy; cbn [provision_signature provision_inst] in Iy;
    rewrite app_nil_r in Iy; exact (Sg y Iy).
Qed.

Lemma set_fun_scoped : forall F C u fx, CxScoped F C -> (forall x tx, In (x, tx) fx -> nlook x (c_ty C) = Some tx) ->
  CxScoped F (set_fun C u fx).
Proof.
  intros F C u fx Cs Fx. apply set_provision_scoped; [exact Cs| |intros r m g a E; discriminate E]. intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  apply in_flat_map in Iy as [[x o] [Iq Iy]]. apply in_map_iff in Iq as [[x' tx] [E Iq]]. cbn in E. injection E as -> <-.
  apply tm_scoped. exact (nlook_gens _ _ _ _ (Fx _ _ Iq) Iy).
Qed.

Lemma cxscoped_narrow : forall F C C', c_ty C' = c_ty C -> c_sub C' = c_sub C -> c_merge C' = c_merge C -> c_locals C' = c_locals C ->
  incl (c_provisions C') (c_provisions C) -> CxScoped F C -> CxScoped F C'.
Proof.
  intros F C C' Ety Esub Merge Eloc Inc [Ls [Ps [Ts Mr]]]. pose proof (scope_gens_ext C' C Ety Esub) as Sc.
  split; [|split; [|split]].
  - intros n t mu I. rewrite Eloc in I. exact (scoped_ext W _ _ _ (eq_sym Sc) (Ls n t mu I)).
  - intros k p I y Iy. unfold GenScoped. rewrite Sc. exact (Ps k p (Inc _ I) y Iy).
  - intros t E. exact (scoped_ext W _ _ _ (eq_sym Sc) (Ts t E)).
  - exact (methsread_narrow C C' Ety Esub Merge Inc Mr).
Qed.

(* ---- a functor's result ------------------------------------------------------ *)

Definition TyIn (K : nat -> Prop) (t : ty) : Prop := GensIn K (gens t).

Lemma obr_gens' : forall p tm a bs, OutputBrackets W p tm a bs -> forall g, In g (tm_gens bs) -> In g (tm_gens tm).
Proof.
  intros p tm a bs B. destruct B as [|bs0 l F]; [intros g []|].
  assert (H : incl (tm_gens l) (tm_gens tm)).
  { induction F as [|b tb bs' l' (sp & sa & E1 & R1 & K1 & T1) Fr IH]; [intros g []|].
    destruct tb as [y v]. cbn in T1. intros g Ig. cbn in Ig. apply in_app_or in Ig as [Ig|Ig]; [|exact (IH g Ig)].
    exact (tyof_none_gens W _ _ _ _ T1 g Ig). }
  intros g Ig. apply H. destruct (tm_gens_in _ g Ig) as [x [u [Iq Iu]]].
  apply (proj2 (in_rev _ _)) in Iq. exact (in_tm_gens _ _ _ _ Iq Iu).
Qed.

Lemma inst_in_gens : forall a x t y, In (x, Some t) a -> In y (gens t) -> In y (inst_gens a).
Proof. intros a x t y Iq Iy. unfold inst_gens. apply in_flat_map. exists (x, Some t). split; assumption. Qed.

Lemma output_entries_gens : forall (K : nat -> Prop),
  (forall p ob tm sp es tm', OutputItemEntries W p ob tm sp es tm' ->
     GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es) /\
  (forall p ob tm sps es tm', OutputItemsEntries W p ob tm sps es tm' ->
     GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es).
Proof.
  intros K.
  assert (Full : forall p ob tm a bs, OutputBrackets W p (ob ++ tm) a bs -> GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) ->
    GensIn K (tm_gens (bs ++ ob ++ tm))).
  { intros p ob tm a bs B SOb STm y Iy. assert (Two : forall y, In y (tm_gens (ob ++ tm)) -> K y)
      by (intros z Iz; apply tm_gens_app in Iz as [Iz|Iz]; [exact (SOb z Iz)|exact (STm z Iz)]).
    apply tm_gens_app in Iy as [Iy|Iy]; [exact (Two y (obr_gens' _ _ _ _ B y Iy))|exact (Two y Iy)]. }
  assert (Inst : forall s tm a, AppOf W s tm a -> GensIn K (tm_gens tm) -> InstP (TyIn K) a).
  { intros s tm a App S x t Iq y Iy. exact (S y (appof_scoped W _ _ _ App y (inst_in_gens a x t y Iq Iy))). }
  assert (Sig : forall s tm this g, SigOf W s tm this g -> GensIn K (tm_gens tm) -> (forall x, this = Some x -> TyIn K x) ->
    SigP (TyIn K) g).
  { intros s tm this g Sg S Th. assert (All : forall y, In y (sig_gens g) -> K y).
    { intros y Iy. destruct (sigof_gens s tm this g y Sg Iy) as [I|[x [E Ix]]]; [exact (S y I)|exact (Th x E y Ix)]. }
    split; [|intros y Iy; exact (All y (sig_ret_in g y Iy))].
    apply Forall_forall. intros t It y Iy. exact (All y (sig_param_in g t y It Iy)). }
  apply (oies_mut W
    (fun p ob tm sp es tm' _ => GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es)
    (fun p ob tm sps es tm' _ => GensIn K (tm_gens ob) -> GensIn K (tm_gens tm) -> Forall (EntryP (TyIn K)) es)).
  - (* type *) intros. repeat constructor.
  - (* nothing *) intros. constructor.
  - (* val *) intros p ob tm a pa s d n t ty0 bs ap R D Ast B T App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    split; [intros y Iy; exact (Fl y (tyof_none_gens W _ _ _ _ T y Iy))|exact (Inst _ _ _ App Fl)].
  - (* fn *) intros p ob tm a pa s g bs ap d rcv n ps rt R Kd D Ast B Sg App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    split; [exact (Sig _ _ _ _ Sg Fl (fun x E => ltac:(discriminate)))|exact (Inst _ _ _ App Fl)].
  - (* functor *) intros p ob tm a bs pa u d n ps args res fx R D Ast B Fixd FR SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl. constructor; [|constructor]. cbn.
    intros x tx Iq y Iy. destruct (proj1 (FR x tx) Iq) as [_ L]. exact (Fl y (nlook_gens _ _ _ _ L Iy)).
  - (* method *) intros p ob tm a pa dn r m rt g bs ap R MN B Recv Sg App SOb STm.
    pose proof (Full _ _ _ _ _ B SOb STm) as Fl.
    assert (Rt : TyIn K rt) by (intros y Iy; exact (Fl y (recv_gens W _ _ _ Recv y Iy))).
    constructor; [|constructor]. cbn.
    split; [exact Rt|split; [exact (Sig _ _ _ _ Sg Fl (fun x E => ltac:(injection E as <-; exact Rt)))|exact (Inst _ _ _ App Fl)]].
  - (* context *) intros p ob tm a pa c d n its sps bs es tm' R D Ast Esps B Rest IH SOb STm.
    apply IH; [|exact STm]. intros y Iy. apply tm_gens_app in Iy as [Iy|Iy]; [|exact (SOb y Iy)].
    apply (obr_gens' _ _ _ _ B) in Iy. apply tm_gens_app in Iy as [Iy|Iy]; [exact (SOb y Iy)|exact (STm y Iy)].
  - intros. constructor.
  - intros p ob tm sp rest es1 tm1 es2 tm2 H1 IH1 H2 IH2 SOb STm.
    pose proof (proj1 (output_entries_tm W) _ _ _ _ _ _ H1) as ->.
    apply Forall_app. split; [exact (IH1 SOb STm)|exact (IH2 SOb STm)].
Qed.

Lemma entry_provision_gens_read_f : forall (K : nat -> Prop) s freeze e k p,
  ReadWithin K s -> (forall a, InstP (TyIn K) a -> InstP (TyIn K) (freeze a)) ->
  EntryP (TyIn K) e -> In (k, p) (entry_provision s freeze e) -> GensIn K (provision_gens p).
Proof.
  intros K s freeze [t|v t a|f g a|r m g a|u fx|eql eqr] k p Read Fz Ok Iq; cbn in Iq; try contradiction;
    destruct Iq as [E|[]]; injection E as <- <-; cbn in Ok.
  - destruct Ok as [Ot Oa]. intros y Iy. unfold provision_gens in Iy. cbn in Iy. rewrite app_nil_r in Iy.
    apply in_app_or in Iy as [Iy|Iy]; [exact (Read t Ot y Iy)|].
    apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Fz a Oa x w Iq y Iy)|contradiction].
  - destruct Ok as [[Ps Rt] Oa]. intros y Iy. unfold provision_gens in Iy. cbn [provision_signature provision_inst] in Iy.
    apply in_app_or in Iy as [Iy|Iy].
    + cbn in Iy. apply in_app_or in Iy as [Iy|Iy]; [exact (Read _ Rt y Iy)|].
      apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It]].
      rewrite Forall_forall in Ps. exact (Read t0 (Ps t0 It) y Iy).
    + apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Fz a Oa x w Iq y Iy)|contradiction].
  - destruct Ok as [_ [[Ps Rt] Oa]]. intros y Iy. unfold provision_gens in Iy. cbn [provision_signature provision_inst] in Iy.
    apply in_app_or in Iy as [Iy|Iy].
    + cbn in Iy. apply in_app_or in Iy as [Iy|Iy]; [exact (Read _ Rt y Iy)|].
      apply in_flat_map in Iy as [t [It Iy]]. apply in_map_iff in It as [t0 [<- It]].
      rewrite Forall_forall in Ps. exact (Read t0 (Ps t0 It) y Iy).
    + apply in_flat_map in Iy as [[x [w|]] [Iq Iy]]; [exact (Fz a Oa x w Iq y Iy)|contradiction].
  - intros y Iy. unfold provision_gens in Iy. cbn in Iy. apply in_flat_map in Iy as [[x o] [Iq Iy]].
    unfold fixed_subst in Iq. rewrite map_map in Iq. apply in_map_iff in Iq as [[x' w] [E Iq]]. cbn in E. injection E as -> <-.
    cbn in Iy. exact (Read w (Ok _ w Iq) y Iy).
Qed.

Lemma entry_provision_gens_f : forall (K : nat -> Prop) s freeze e k p,
  (forall g u, In (g, u) s -> GensIn K (gens u)) -> (forall a, InstP (TyIn K) a -> InstP (TyIn K) (freeze a)) ->
  EntryP (TyIn K) e -> In (k, p) (entry_provision s freeze e) -> GensIn K (provision_gens p).
Proof.
  intros K s freeze e k p Rows Freeze Entry InProv.
  apply (entry_provision_gens_read_f K s freeze e k p); [|exact Freeze|exact Entry|exact InProv].
  intros t Within. exact (subst_within K s t Rows Within).
Qed.

Lemma entry_provision_gens : forall (K : nat -> Prop) s e k p,
  (forall g u, In (g, u) s -> GensIn K (gens u)) -> EntryP (TyIn K) e ->
  In (k, p) (entry_provision s (fun a => a) e) -> GensIn K (provision_gens p).
Proof. intros K s e k p Range Ok Iq. exact (entry_provision_gens_f K s (fun a => a) e k p Range (fun a H => H) Ok Iq). Qed.

(* A functor's result read in C0: each new provision mentions only K. *)
Lemma provide_result_gens : forall u C0 out C' ks (K : nat -> Prop),
  ProvideResult W u C0 out C' ks ->
  ReadWithin K (c_sub C0) ->
  (forall x v, In (x, v) (c_ty C0) -> GensIn K (gens (canon C0 v))) ->
  (forall t, FunOutputs W u t -> GensIn K (gens (canon C0 (out t)))) ->
  forall k p, In (k, p) (c_provisions C') -> In (k, p) (c_provisions C0) \/ GensIn K (provision_gens p).
Proof.
  intros u C0 out C' ks K (d & ps & a & r & b & outs & es & tm' & D & FP & Outs & OIE & RK & ->) Read Ty Out.
  assert (Sc0 : GensIn K (tm_gens (output_type_map C0 outs out))).
  { intros g Ig. destruct (tm_gens_in _ g Ig) as [x [v [Iq Iv]]].
    unfold output_type_map in Iq. apply in_map_iff in Iq as [[y w] [E Iq]]. cbn in E. injection E as -> <-.
    apply in_app_or in Iq as [Iq|Iq].
    - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. exact (Out t (proj1 (Outs t) It) g Iv).
    - exact (Ty _ _ Iq g Iv). }
  pose proof (proj2 (output_entries_gens K) _ _ _ _ _ _ OIE ltac:(intros g []) Sc0) as Es.
  intros k p Iq. cbn in Iq. apply in_app_or in Iq as [Iq|Iq]; [right|left; exact Iq].
  apply in_flat_map in Iq as [e [Ie Iq]]. apply (proj2 (in_rev _ _)) in Ie. rewrite Forall_forall in Es.
  exact (entry_provision_gens_read_f K _ (fun a => a) e k p Read (fun a H => H) (Es e Ie) Iq).
Qed.

(* A family applied in C at a matching scoped in C adds only provisions
   scoped in C. *)
Lemma family_application_gens : forall C u matching th sm Cr ks,
  FamilyApplication W C u matching th sm Cr ks ->
  (forall x v, In (x, v) matching -> ScopedTy W C (canon C v)) ->
  forall k p, In (k, p) (c_provisions Cr) -> In (k, p) (c_provisions C) \/ GensIn (GenScoped W C) (provision_gens p).
Proof.
  intros C u matching th sm Cr ks (formal & needs & (fixed & Fx & Fam & Poly & FC & Args & InF & FA & _ & PR & Nd) & _) Mt.
  destruct (family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  intros k p Iq. rewrite <- Epr.
  apply (provide_result_gens _ _ _ _ _ (GenScoped W C) PR); [| | |exact Iq].
  - rewrite Esub. apply canon_within. intros y Iy. exact Iy.
  - intros x v Iq' y Iy. rewrite Ety in Iq'. unfold canon in Iy. rewrite Esub in Iy. apply in_app_or in Iq' as [Iq'|Iq'].
    + apply (proj2 (in_rev _ _)) in Iq'. exact (Mt x v Iq' y Iy).
    + exact (scope_ty_canon C x v y Iq' Iy).
  - intros t Ft. exfalso. exact (family_no_outputs W u t (proj1 Fam) Ft).
Qed.

(* A family applied in C (C0's merge, provisions and locals, a table read
   in C0's scope), its matching read off a receiver scoped in C0: C0's
   table with the family's provisions keeps CxScoped. *)
Lemma family_result_cxscoped : forall F C0 C u matching th sm Cr ks ot m,
  c_sub C = c_sub C0 -> c_merge C = c_merge C0 -> c_provisions C = c_provisions C0 -> c_locals C = c_locals C0 ->
  FamilyApplication W C u matching th sm Cr ks -> FamilyReadOff W C u ot m matching ->
  TyOk W ot -> ScopedTy W C0 ot -> (forall x v, In (x, v) (c_ty C) -> ScopedTy W C0 (canon C v)) ->
  CxScoped F C0 -> CxScoped F (Cx (c_ty C0) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr)).
Proof.
  intros F C0 C u matching th sm Cr ks ot m Es Merge Ep El FA Ro Ok Sc Tin [Ls [Ps [Ts Mr]]].
  pose proof FA as (Cu & ns & (fx & Fx & [Shape _] & Poly & FC & Th & InF & FAg & _ & PR & Nd) & _).
  destruct (family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  set (K := GenScoped W C0).
  assert (Mt : forall x v, In (x, v) matching -> GensIn K (gens (canon C v))).
  { destruct Ro as (marks & fr & fks & rf & a & Marks & PRm & Ikm & Dom & NdM & Ot & _).
    intros x v Iq. assert (Ix : In x (gens rf)) by (apply (proj2 (proj1 (Dom x) (in_map fst _ _ Iq)))).
    pose proof (sub_subst (c_sub C) _ _ (sub_read rf matching x v Ix (lookup_subst_nodup matching x v NdM Iq))) as Sb.
    change (SubTy (canon C v) (canon C (app_subst matching rf))) in Sb. rewrite Ot in Sb.
    destruct (sub_ok W _ _ Sb Ok) as [_ Iv]. intros g Ig. exact (Sc g (Iv g Ig)). }
  destruct (provide_result_shape W _ _ _ _ _ PR) as [SubR [LoR _]].
  assert (ScE : scope_gens (Cx (c_ty C0) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr)) = scope_gens C0)
    by (apply scope_gens_ext; [reflexivity|cbn; rewrite SubR, Esub, Es; reflexivity]).
  split; [|split].
  - intros n t mu Iq. cbn in Iq. rewrite LoR, Elo, El in Iq. exact (scoped_ext W _ _ _ (eq_sym ScE) (Ls n t mu Iq)).
  - intros k p Iq y Iy. unfold GenScoped. rewrite ScE. fold (GenScoped W C0 y). cbn in Iq.
    destruct (provide_result_gens _ _ _ _ _ K PR) with (k := k) (p := p) as [Iq'|G]; [| | |exact Iq| |exact (G y Iy)].
    + rewrite Esub, Es. apply canon_within. intros z Iz. exact Iz.
    + intros x v Iv z Iz. unfold canon in Iz. rewrite Esub in Iz. fold (canon C v) in Iz.
      rewrite Ety in Iv. apply in_app_or in Iv as [Iv|Iv]; [apply (proj2 (in_rev _ _)) in Iv; exact (Mt x v Iv z Iz)|exact (Tin x v Iv z Iz)].
    + intros t Ft. exfalso. exact (family_no_outputs W u t Shape Ft).
    + rewrite Epr, Ep in Iq'. exact (Ps k p Iq' y Iy).
  - split; [intros t E; exact (scoped_ext W _ _ _ (eq_sym ScE) (Ts t E))|].
    assert (Sub0 : c_sub (Cx (c_ty C0) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr)) = c_sub C0) by (cbn; rewrite SubR, Esub, Es; reflexivity).
    pose proof (family_application_merge W _ _ _ _ _ _ _ FA) as MergeR.
    assert (Merge0 : c_merge (Cx (c_ty C0) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr)) = c_merge C0).
    { cbn. now rewrite MergeR, Merge. }
    destruct Mr as [Section [Tc Pr]]. split; [|split].
    + eapply section_same_fields; eassumption.
    + intros x v Iq. unfold canon. rewrite Sub0. exact (Tc x v Iq).
    + intros r m0 g a Iq. cbn in Iq.
      destruct (provide_result_meths _ _ _ _ _ PR r m0 g a Iq) as [I|[_ (tmx & rt & g0 & Sg & App & <- & -> & _)]].
      * rewrite Epr, Ep in I. exact (methread_sub C0 _ _ _ _ _ Sub0 Merge0 (Pr _ _ _ _ I)).
      * apply (methread_sub (family_context C matching)).
        -- rewrite Sub0, Esub, Es. reflexivity.
        -- cbn. rewrite MergeR, family_context_merge. reflexivity.
        -- apply (methread_of _ m0 tmx rt g0 a); [|exact Sg|exact App].
           eapply section_same_fields; [rewrite Esub, Es; reflexivity|rewrite family_context_merge; exact Merge|exact Section].
Qed.


(* ---- readings in a context ----------------------------------------------------- *)

Lemma Forall2_In_r : forall X Y (R : X -> Y -> Prop) l l' y, Forall2 R l l' -> In y l' -> exists x, In x l /\ R x y.
Proof.
  intros X Y R l l' y F. induction F as [|a b l l' Rab Rest IH]; intros Iy; [contradiction|].
  destruct Iy as [<-|Iy]; [exists a; split; [left; reflexivity|exact Rab]|].
  destruct (IH Iy) as [x [Ix Rx]]. exists x. split; [right; exact Ix|exact Rx].
Qed.

Lemma nom_reqs_scoped : forall C p s a bs reqs xs n,
  Brackets W p (c_ty C) s a bs ->
  Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) reqs xs ->
  ScopedTy W C (TNom n xs).
Proof.
  intros C p s a bs reqs xs n B Fx y Iy. rewrite gens_nom in Iy. destruct (tm_gens_in _ y Iy) as [x [u [Iq Iu]]].
  apply tm_scoped. destruct (Forall2_In_r _ _ _ _ _ _ Fx Iq) as [r [_ [_ L]]]. cbn in L.
  pose proof (nlook_gens _ _ _ _ L Iu) as I. apply tm_gens_app in I as [I|I]; [exact (br_gens W _ _ _ _ _ B y I)|exact I].
Qed.

Lemma tyof_scoped_ctx : forall C p this a t, TyOf W p (c_ty C) this a t -> OptScoped C this -> ScopedTy W C t.
Proof.
  intros C p this a t T Th y Iy. destruct (tyof_gens W _ _ _ _ _ T y Iy) as [I|[x [E Ix]]]; [exact (tm_scoped C y I)|exact (Th x E y Ix)].
Qed.

Lemma payload_gens : forall s xs pay y, PayloadOf W s xs pay -> In y (gens pay) -> In y (tm_gens xs).
Proof. intros s xs pay y [s' xs' d n ps t pay' D Ast T] Iy. exact (tyof_none_gens W _ _ _ _ T y Iy). Qed.

Lemma join_scoped : forall C a b exp t, Join a b exp t -> ScopedTy W C a -> ScopedTy W C b -> OptScoped C exp -> ScopedTy W C t.
Proof. intros C a b exp t J Sa Sb Se. destruct J; [exact (Se _ eq_refl)|exact Sb|exact Sa]. Qed.

Lemma branch_ty_scoped : forall C b t, ScopedTy W C t -> ScopedTy W C (branch_ty b t).
Proof. intros C [ss [y|]] t St; cbn; [exact St|intros y' []]. Qed.

Lemma never_scoped : forall C, ScopedTy W C never.
Proof. intros C y []. Qed.

Lemma unit_scoped : forall C, ScopedTy W C TUnit.
Proof. intros C y []. Qed.

Lemma fieldpats_in : forall F ftys fs bs xs, FieldPats W F ftys fs bs xs -> forall n t, In (n, t) bs -> exists f, In (f, t) ftys.
Proof.
  intros F ftys fs bs xs FP. induction FP as [|n t r bs xs S Nm FP IH|n t r bs xs S FP IH|n y t r bs xs S R Nm FP IH];
    intros n' t' Ib; [contradiction| |exact (IH n' t' Ib)|].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exists n; exact (slook_in _ _ _ _ S)|exact (IH _ _ Ib)].
  - destruct Ib as [E|Ib]; [injection E as <- <-; exists n; exact (slook_in _ _ _ _ S)|exact (IH _ _ Ib)].
Qed.

Lemma patof_scoped : forall C F st pat xp binds hs ca, PatOf W F st pat xp binds hs ca -> ScopedTy W C st ->
  forall n t, In (n, t) binds -> ScopedTy W C t.
Proof.
  intros C F st pat xp binds hs ca Pt Sc.
  destruct Pt as [|n R Nm|pa s k xs R K Kk Im|pa s xs y pay R K Im Pay Ry Nm|pa s xs fs ftys bs fxs R K Im Pay FP Nd];
    intros n' t' Ib.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. exact Sc.
  - contradiction.
  - destruct Ib as [E|[]]. injection E as _ <-. intros z Iz. apply Sc. apply (member_gens _ _ _ Im).
    rewrite gens_nom. exact (payload_gens _ _ _ z Pay Iz).
  - destruct (fieldpats_in _ _ _ _ _ FP n' t' Ib) as [f If]. intros z Iz. apply Sc. apply (member_gens _ _ _ Im).
    rewrite gens_nom. apply (payload_gens _ _ _ z Pay). rewrite gens_record. apply in_flat_map. exists (f, t'). split; assumption.
Qed.

Lemma literal_scoped : forall C tr x t, Literal W C tr x t -> ProvsScoped C -> ScopedTy W C t.
Proof.
  intros C tr x t L Ps.
  induction L as [z t Lz Found|o t Lo Found|a b x y r t m La IHa Lb IHb Lm Op|a b x y r t m La IHa Lb IHb Lm Op
                 |a b x y r t m La IHa Lb IHb Lm Op|a x ti tp t f La IHa Lf Found Fits].
  - exact (val_scoped C _ _ Ps Found).
  - exact (val_scoped C _ _ Ps Found).
  - exact IHa.
  - exact IHa.
  - exact IHa.
  - exact (fn_ret_scoped C _ _ Ps Found).
Qed.

Lemma provide_tables : forall C k a C1, c_ty (provide C k a C1) = c_ty C1 /\ c_sub (provide C k a C1) = c_sub C1 /\
  c_locals (provide C k a C1) = c_locals C1.
Proof.
  intros C k a C1. unfold provide. destruct (ilook k (c_provisions C1)) as [[t a0|g a0|g a0|fx]|]; repeat split.
Qed.

Lemma provide_scope : forall C k a C1, scope_gens (provide C k a C1) = scope_gens C1.
Proof. intros C k a C1. destruct (provide_tables C k a C1) as [E1 [E2 _]]. exact (scope_gens_ext _ _ E1 E2). Qed.

Lemma set_ty_back : forall C t v, (forall y, In y (gens v) -> In y (scope_gens C)) ->
  forall y, In y (scope_gens (set_ty C t v)) -> In y (scope_gens C).
Proof.
  intros C t v Sv y Iy. unfold scope_gens in Iy. cbn [set_ty c_sub c_ty flat_map] in Iy.
  apply in_app_or in Iy as [Iy|Iy]; [unfold scope_gens; apply in_or_app; left; exact Iy|].
  apply in_app_or in Iy as [Iy|Iy]; [|unfold scope_gens; apply in_or_app; right; exact Iy].
  apply in_app_or in Iy as [Iy|Iy]; [exact (Sv y Iy)|].
  change (In y (gens (canon C v))) in Iy. destruct (gens_canon C v y Iy) as [I|I]; [exact (Sv y I)|exact I].
Qed.

(* `bind F;` for F abstract: the result's scope is the old one and the
   output destinations. *)
Lemma apply_abstract_scope : forall C u gensof C' ks, ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
  incl (scope_gens C) (scope_gens C') /\
  (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ exists t, FunOutputs W u t /\ y = gensof t).
Proof.
  intros C u gensof C' ks PR.
  destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs [OutsI Ety]]]].
  assert (InTy : forall x v, In (x, v) (output_type_map C outs (fun t => TAbs (gensof t))) ->
    In (x, app_subst (c_sub C') v) (c_ty C')).
  { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
  assert (OldIn : forall x v, In (x, v) (c_ty C) -> In (x, canon C v) (output_type_map C outs (fun t => TAbs (gensof t)))).
  { intros x v Iq. unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iq]. }
  split.
  - intros y Iy. unfold scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    + apply in_flat_map in Iy as [[g v] [Iq Iy]]. rewrite <- Esub in Iq. destruct Iy as [E|Iy].
      * cbn in E. subst y. exact (scope_sub_key C' _ _ Iq).
      * exact (scope_sub C' g v y Iq Iy).
    + apply in_flat_map in Iy as [[x v] [Iq Iy]]. cbn in Iy.
      pose proof (table_scope C' x (canon C v) (InTy _ _ (OldIn _ _ Iq))) as T.
      apply in_app_or in Iy as [Iy|Iy]; [|exact (T y Iy)].
      destruct (lookup_subst y (c_sub C)) as [w|] eqn:L.
      * rewrite <- Esub in L. exact (scope_sub_key C' y w (lookup_subst_in_pair _ _ _ L)).
      * exact (T y (gens_keep _ _ _ Iy L)).
  - intros y Iy. unfold scope_gens in Iy. apply in_app_or in Iy as [Iy|Iy].
    + left. unfold scope_gens. apply in_or_app. left. rewrite Esub in Iy. exact Iy.
    + apply in_flat_map in Iy as [[x w] [Iq Iy]]. cbn [fst snd] in Iy. rewrite Ety in Iq.
      apply in_map_iff in Iq as [[x1 w1] [E Iq]]. cbn in E. injection E as -> <-.
      unfold output_type_map in Iq. apply in_map_iff in Iq as [[x2 w2] [E Iq]]. cbn in E. injection E as -> <-.
      assert (Back : forall z, In z (gens w2) -> In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t).
      { intros z Iz. apply in_app_or in Iq as [Iq|Iq].
        - apply in_map_iff in Iq as [t [E It]]. injection E as <- <-. cbn in Iz. destruct Iz as [<-|[]].
          right. exists t. split; [exact (proj1 (OutsI t) It)|reflexivity].
        - left. exact (scope_ty C _ _ z Iq Iz). }
      assert (Can : forall t z, In z (gens (canon C t)) -> (forall z, In z (gens t) -> In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t) ->
        In z (scope_gens C) \/ exists t, FunOutputs W u t /\ z = gensof t).
      { intros t z Iz Bt. destruct (gens_canon C t z Iz) as [I|I]; [exact (Bt z I)|left; exact I]. }
      unfold canon in Iy. rewrite Esub in Iy. fold (canon C) in Iy.
      apply in_app_or in Iy as [Iy|Iy].
      * apply (Can _ y Iy). intros z Iz. exact (Can _ z Iz Back).
      * apply (Can _ y Iy). intros z Iz. apply (Can _ z Iz). intros z' Iz'. exact (Can _ z' Iz' Back).
Qed.

(* `bind F;` for F abstract keeps CxScoped (BApplyAbstract). *)
Lemma apply_abstract_cxscoped : forall F F' C u gensof C' ks, ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
  fr_this F' = fr_this F -> CxScoped F C -> CxScoped F' C'.
Proof.
  intros F F' C u gensof C' ks PR Th [Ls [Ps [Ts Mr]]].
  destruct (apply_abstract_scope C u gensof C' ks PR) as [Inc _].
  destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs' [OutsI' Ety]]]].
  assert (InTy : forall x v, In (x, v) (output_type_map C outs' (fun t => TAbs (gensof t))) ->
    In (x, app_subst (c_sub C') v) (c_ty C')).
  { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
  split; [|split].
  - intros n t mu Iq. rewrite Elo in Iq. exact (scoped_mono _ _ _ Inc (Ls n t mu Iq)).
  - intros k p Iq.
    destruct (provide_result_gens _ _ _ _ _ (GenScoped W C') PR) with (k := k) (p := p) as [Iq'|G]; [| | |exact Iq| |exact G].
    + rewrite <- Esub. apply canon_within. intros y Iy. exact Iy.
    + intros x v Iv y Iy. apply (table_scope C' x (canon C v)); [|exact Iy]. apply InTy.
      unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iv].
    + intros t Ft y Iy. apply (table_scope C' t (canon C (TAbs (gensof t)))); [|exact Iy]. apply InTy.
      unfold output_type_map. apply in_map_iff. exists (t, TAbs (gensof t)). split; [reflexivity|].
      apply in_or_app; left. apply in_map_iff. exists t. split; [reflexivity|exact (proj2 (OutsI' t) Ft)].
    + intros y Iy. exact (inscope_mono W _ _ y Inc (Ps k p Iq' y Iy)).
  - split; [intros t E; rewrite Th in E; exact (scoped_mono _ _ _ Inc (Ts t E))|].
    exact (methsread_provide_result _ _ _ _ _ PR Mr).
Qed.

End Scoping.

(* ---- the contexts bodies start in ------------------------------------------------ *)

Section Initial.
Variable W : A.world.
Import A.

Lemma entry_gens_entryp : forall (K : nat -> Prop) e, GensIn K (entry_gens e) -> EntryP (TyIn K) e.
Proof.
  intros K [t|v t a|f g a|r m g a|u fx|eql eqr] G; cbn; try exact Logic.I.
  - split; [intros y Iy; apply G; cbn; apply in_or_app; left; exact Iy|].
    intros x w Iq y Iy. apply G. cbn. apply in_or_app. right. exact (inst_in_gens a x w y Iq Iy).
  - split; [|intros x w Iq y Iy; apply G; cbn; apply in_or_app; right; exact (inst_in_gens a x w y Iq Iy)].
    split; [apply Forall_forall; intros t It y Iy; apply G; cbn; apply in_or_app; left; exact (sig_param_in g t y It Iy)|].
    intros y Iy. apply G. cbn. apply in_or_app. left. exact (sig_ret_in g y Iy).
  - split; [intros y Iy; apply G; cbn; apply in_or_app; left; exact Iy|].
    split; [|intros x w Iq y Iy; apply G; cbn; apply in_or_app; right; apply in_or_app; right; exact (inst_in_gens a x w y Iq Iy)].
    split; [apply Forall_forall; intros t It y Iy; apply G; cbn; apply in_or_app; right; apply in_or_app; left; exact (sig_param_in g t y It Iy)|].
    intros y Iy. apply G. cbn. apply in_or_app. right. apply in_or_app. left. exact (sig_ret_in g y Iy).
  - intros x w Iq y Iy. apply G. cbn. apply in_flat_map. exists (x, w). split; assumption.
  - split; intros y Iy; apply G; cbn; apply in_or_app; [left|right]; exact Iy.
Qed.

Lemma entries_cx_scoped : forall F σ classes tm es, IdForm tm -> (forall e, In e es -> incl (entry_gens e) (map fst tm)) ->
  incl (merge_symbols classes) (map fst tm) -> ThisScoped W F (entries_cx σ classes tm es) ->
  SectionOf classes σ -> EntriesRead W es ->
  CxScoped W F (entries_cx σ classes tm es).
Proof.
  intros F σ classes tm es Id Eg Classes Th Section Rd. set (K := fun y => In y (scope_gens (entries_cx σ classes tm es))).
  assert (Key : forall x, In x (map fst tm) -> K x) by (intros x Ix; exact (idform_scope σ classes tm es x Id Ix)).
  assert (Read : forall t, TyIn K t -> TyIn K (app_subst σ t)).
  { intros t Within. refine (@PublicMerge.section_gens_bound classes σ Section _ _ t Within).
    intros y Iy. exact (Key y (Classes y Iy)). }
  split; [intros n t mu []|split; [|exact (conj Th (methsread_entries W σ classes tm es Section Rd))]].
  intros k p Iq y Iy. cbn in Iq. apply in_flat_map in Iq as [e [Ie Iq]].
  refine (entry_provision_gens_read_f K σ _ e k p Read _ (entry_gens_entryp K e (fun y Iy => Key y (Eg e Ie y Iy))) Iq y Iy).
  intros a Ia x w Iq' z Iz. apply in_map_iff in Iq' as [[x0 o] [E Iq']]. cbn in E. injection E as E0 E. subst x.
  destruct o as [t0|]; cbn in E.
  - injection E as <-. exact (Read t0 (Ia _ _ Iq') z Iz).
  - destruct (nlook x0 tm) as [t0|] eqn:L; cbn in E; [|discriminate]. injection E as <-.
    apply (Read t0); [|exact Iz]. intros y' Iy'.
    pose proof (idform_nlook tm x0 t0 Id L) as ->. destruct Iy' as [E'|[]]. subst y'. apply Key.
    change x0 with (fst (x0, TAbs x0)). apply in_map. exact (nlook_in _ _ _ _ L).
Qed.

Theorem ctxof_cxscoped : forall f C ns F, CtxOf W f C ns -> ThisScoped W F C -> CxScoped W F C.
Proof.
  intros f C ns F (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & Merge & Section & -> & _ & _) Th.
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  assert (Tmr : IdForm tmr /\ incl (map fst tm) (map fst tmr) /\ Scoped tmr (flat_map entry_gens er)).
  { destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI].
    - split; [exact IdT|]. split; [intros x Ix; exact Ix|]. intros g [].
    - exact (root_formed W _ _ _ _ _ _ RI IdT). }
  destruct Tmr as [IdR [SubR ErR]].
  assert (All : Scoped tmr (flat_map entry_gens (es ++ er))).
  { intros x Ix. rewrite flat_map_app in Ix. apply in_app_or in Ix as [Ix|Ix]; [exact (SubR x (EsT x Ix))|exact (ErR x Ix)]. }
  assert (Rd : EntriesRead W (es ++ er)).
  { intros rt m g a I. apply in_app_or in I as [I|I]; [exact (proj2 (entries_read W) _ _ _ _ _ _ IE _ _ _ _ I)|].
    exfalso. destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI]; [exact I|exact (root_read W _ _ _ _ _ _ RI _ _ _ _ I)]. }
  apply entries_cx_scoped; [exact IdR| | |exact Th|exact Section|exact Rd].
  - intros e Ie y Iy. apply All. apply in_flat_map. exists e. split; assumption.
  - intros y Iy. apply All. exact (proj1 (proj2 (proj2 Merge)) y Iy).
Qed.

Theorem functx_cxscoped : forall u C ns F, FunCtx W u C ns -> ThisScoped W F C -> CxScoped W F C.
Proof.
  intros u C ns F (d & ps & a & r & b & es & tm & tm' & σ & classes & implicit & D & FP & IE & Imp & -> & MGM & Merge & Section & -> & _ & _) Th.
  destruct (proj2 (entries_formed W) _ _ _ _ _ _ IE ltac:(constructor) ltac:(intros g [])) as [IdT [_ EsT]].
  assert (Id' : IdForm (map (fun t => (t, TAbs t)) implicit ++ tm)).
  { apply Forall_app. split; [|exact IdT]. apply Forall_forall. intros q Iq. apply in_map_iff in Iq as [t [<- _]]. reflexivity. }
  assert (Sub : forall x, In x (map fst tm) -> In x (map fst (map (fun t => (t, TAbs t)) implicit ++ tm))).
  { intros x Ix. rewrite map_app. apply in_or_app. right. exact Ix. }
  apply entries_cx_scoped; [exact Id'| | |exact Th|exact Section|exact (proj2 (entries_read W) _ _ _ _ _ _ IE)].
  - intros e Ie y Iy. apply Sub. apply EsT. apply in_flat_map. exists e. split; assumption.
  - intros y Iy. apply Sub, EsT. exact (proj1 (proj2 (proj2 Merge)) y Iy).
Qed.

(* ---- a callee's needs, read at the call site ---------------------------------- *)

Definition KeysTyped (tm : list (nat * ty)) : Prop := forall x, In x (map fst tm) -> kind_sym W x = Some DKType.

Lemma entries_keys_typed' :
  (forall p ob tm sp es tm', ItemEntries W p ob tm sp es tm' -> KeysTyped tm -> KeysTyped tm') /\
  (forall p ob tm sps es tm', ItemsEntries W p ob tm sps es tm' -> KeysTyped tm -> KeysTyped tm').
Proof.
  apply (ies_mut W (fun p ob tm sp es tm' _ => KeysTyped tm -> KeysTyped tm')
                   (fun p ob tm sps es tm' _ => KeysTyped tm -> KeysTyped tm'));
    intros; try assumption.
  - destruct (nlook t tm); [assumption|]. intros x [<-|Ix]; [assumption|auto].
  - auto.
  - auto.
  - auto.
Qed.

Lemma root_keys_typed : MentionsTyped W -> forall q es0 tm its es tm', RootItems W q es0 tm its es tm' ->
  KeysTyped tm -> KeysTyped tm'.
Proof.
  intros Mt q es0 tm its es tm' Root. induction Root as
    [es0 tm|es0 tm p a rest u du n ps args res xs fx es tm' R D Ast Fresh Nd Xs FR Rest IH|es0 tm sp rest es tm' Skip Rest IH];
    intros Kt.
  - exact Kt.
  - apply IH. intros x Ix. destruct (add_tys_keys xs tm x Ix) as [Ix'|Ix']; [|exact (Kt x Ix')].
    apply (Mt u). exact (proj1 (proj1 (Xs x) Ix')).
  - exact (IH Kt).
Qed.

Lemma ctxof_keys_typed : MentionsTyped W -> forall f C ns, CtxOf W f C ns ->
  forall x, In x (map fst (c_ty C)) -> kind_sym W x = Some DKType.
Proof.
  intros Mt f C ns (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & _ & _ & -> & _ & _) x Ix.
  cbn in Ix. rewrite map_map in Ix. cbn in Ix.
  pose proof (proj2 entries_keys_typed' _ _ _ _ _ _ IE (fun y (Iy : In y []) => match Iy with end)) as Kt.
  destruct Root as [_|gc d0 n0 its er' tmr' _ _ _ _ RI]; [exact (Kt x Ix)|].
  exact (root_keys_typed Mt _ _ _ _ _ _ RI Kt x Ix).
Qed.

Lemma need_provision : forall f Cf ns k, CtxOf W f Cf ns -> In k ns -> exists p, In (k, p) (c_provisions Cf).
Proof.
  intros f Cf ns k (d & es & tm & er & tmr & σ & classes & D & IE & Root & MGM & _ & _ & -> & [Nd Keys] & _) Ik.
  destruct (proj1 (Keys k) Ik) as [e [Ie Ek]]. cbn [c_provisions entries_cx].
  destruct e as [t|v t a|f' g a|r m g a|u fx|eql eqr]; cbn in Ek; try discriminate; injection Ek as <-;
    eexists; apply in_flat_map; eexists; (split; [exact Ie|cbn; left; reflexivity]).
Qed.

Lemma lookup_comp : forall (f : ty -> ty) (l s : subst) x,
  lookup_subst x (map (fun q => (fst q, f (snd q))) l ++ s) =
  match lookup_subst x l with Some v => Some (f v) | None => lookup_subst x s end.
Proof.
  intros f l s x. induction l as [|[y v] r IH]; [reflexivity|]. cbn. destruct (Nat.eqb x y); [reflexivity|exact IH].
Qed.

Lemma map_pair_id : forall (l : subst), map (fun q => (fst q, snd q)) l = l.
Proof. induction l as [|[x v] r IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Lemma lookup_keys_app : forall (f : nat -> ty) l (s : subst) z, In z l ->
  lookup_subst z (map (fun z => (z, f z)) l ++ s) = Some (f z).
Proof.
  intros f l s z. induction l as [|a l IH]; intros Iz; [contradiction|].
  cbn. destruct (Nat.eqb_spec z a) as [->|Ne]; [reflexivity|].
  destruct Iz as [E|Iz]; [congruence|exact (IH Iz)].
Qed.

(* A callee's method need read through θ at the site X (the caller's
   table, with its merge; rule B's companion): ok, and scoped where X's
   table is.  Rule B puts each requirement in force at X through the
   callee's merge class, so θ reads it as a type X's table means. *)
Theorem demand_at_th_scoped : forall C X f Cf ns th r m, MentionsTyped W -> FormedCx W C ->
  c_sub X = c_sub C -> CxSection X ->
  (forall x v, In (x, v) (c_ty X) -> TyOk W (canon X v) /\ ScopedTy W C (canon X v)) ->
  CtxOf W f Cf ns -> CallTheta W X Cf f th -> RequirementsInForce X Cf ->
  In (mkey r m) (map (at_th X th) ns) -> TyOk W r /\ ScopedTy W C r.
Proof.
  intros C X f Cf ns th r m Mt Fc Es SectionX Tin Ctx [Keys [Bound Merges]] RB Ir.
  assert (Read : ReadsMerge Cf (fun t => canon X (app_subst th t))).
  { intros t u Equal. apply (context_canon_iff_mergeeq X SectionX).
    exact (PublicMerge.merge_eq_substitute Merges Equal). }
  pose proof (ctxof_cx_formed W Mt f Cf ns Ctx) as [CfF _].
  destruct (function_context_merge W f Cf ns Ctx) as [SolvedCf Section].
  destruct (ctx_table_formed W f Cf ns Ctx) as [_ Dom].
  assert (CX : forall t, canon X t = canon C t) by (intros t; unfold canon; rewrite Es; reflexivity).
  set (P := fun t => TyOk W t /\ ScopedTy W C t).
  assert (Canon : forall t, P t -> P (canon X t)).
  { intros t [Ok Sc]. rewrite CX. split; [exact (canon_ok W C t (proj1 Fc) Ok)|exact (canon_scoped W C t Sc)]. }
  (* a requirement in force at X *)
  assert (InF : forall y, In y (map fst (c_ty Cf)) -> nlook y (c_ty X) <> None -> P (canon X (app_subst th (TAbs y)))).
  { intros y Iy Ny. pose proof (Bound y Iy Ny) as By. destruct (nlook y (c_ty X)) as [v|] eqn:Lv; [|congruence].
    cbn [app_subst]. rewrite By. cbn [app_subst]. rewrite lookup_subst_nlook', Lv.
    apply Canon. exact (Tin y v (nlook_in _ _ _ _ Lv)). }
  (* a type over Cf's keys, each read as P *)
  assert (Over : forall t, TyOk W t -> incl (gens t) (map fst (c_ty Cf)) ->
            (forall z, In z (gens t) -> P (canon X (app_subst th (TAbs z)))) -> P (canon X (app_subst th t))).
  { intros t Ok Sc Pz. set (rd := fun z => canon X (app_subst th (TAbs z))).
    set (comp := map (fun z => (z, rd z)) (gens t) ++ []).
    assert (Lk : forall z, In z (gens t) -> lookup_subst z comp = Some (rd z)).
    { intros z Iz. unfold comp. exact (lookup_keys_app rd (gens t) [] z Iz). }
    assert (Ec : canon X (app_subst th t) = app_subst comp t).
    { unfold canon at 1. rewrite <- (app_subst_nil (app_subst comp t)). apply subst2_agree.
      intros z Iz. rewrite app_subst_nil. cbn [app_subst]. rewrite (Lk z Iz). reflexivity. }
    rewrite Ec. split.
    - apply (tyok_subst W comp t); [|exact Ok]. apply tmok_app.
      + intros x w Iq. apply in_map_iff in Iq as [z [E Iz]]. injection E as <- <-. exact (proj1 (Pz z Iz)).
      + intros x w [].
    - intros y Iy. destruct (gens_subst t comp y Iy) as [z [Iz [[Nz _]|[w [L Iw]]]]].
      + rewrite (Lk z Iz) in Nz. discriminate.
      + rewrite (Lk z Iz) in L. injection L as <-. exact (proj2 (Pz z Iz) y Iw). }
  (* A bound member determines the reading of its whole class. *)
  assert (Through : forall x, In x (map fst (c_ty Cf)) -> InForceClass X (c_merge Cf) x ->
    P (canon X (app_subst th (TAbs x)))).
  { intros x Ix [y [Equal Ny]].
    pose proof (proj2 (context_canon_iff_mergeeq Cf Section _ _) Equal) as CanonEq.
    assert (Iy : In y (map fst (c_ty Cf))).
    { destruct (lookup_subst y (c_sub Cf)) as [u|] eqn:Ly; [exact (proj1 (Dom y u (lookup_subst_in_pair _ _ _ Ly)))|].
      unfold canon in CanonEq. cbn [app_subst] in CanonEq. rewrite Ly in CanonEq.
      destruct (lookup_subst x (c_sub Cf)) as [u'|] eqn:Lx.
      - subst u'. exact (proj2 (Dom x (TAbs y) (lookup_subst_in_pair _ _ _ Lx)) y (or_introl eq_refl)).
      - injection CanonEq as ->. exact Ix. }
    rewrite <- (Read _ _ Equal). exact (InF y Iy Ny). }
  (* The structured case reads each leaf through a bound class member. *)
  assert (Req : forall x, In x (map fst (c_ty Cf)) -> P (canon X (app_subst th (TAbs x)))).
  { intros x Ix. destruct (RB x Ix) as [Class|[NA AllF]]; [exact (Through x Ix Class)|].
    assert (ReadClass : MergeEq (c_merge Cf) (TAbs x) (canon Cf (TAbs x))).
    { apply PublicMerge.merge_eq_read. exact (proj1 Section). }
    assert (Expanded : expand (c_merge Cf) (canon Cf (TAbs x)) = canon Cf (TAbs x)).
    { apply section_output_expanded; [exact SolvedCf|exact Section]. }
    pose proof (PublicMerge.expand_outer SolvedCf ReadClass) as Outer. rewrite Expanded in Outer.
    pose proof (proj1 (@PublicMerge.type_outer_nonabs _ _ Outer) NA) as NonAbs.
    pose proof (proj1 (@PublicMerge.expanded_in_force X _ _ _ SolvedCf ReadClass) AllF) as Leaves.
    rewrite Expanded in Leaves.
    rewrite (Read _ _ ReadClass).
    destruct (lookup_subst x (c_sub Cf)) as [u|] eqn:Lx.
    2: { exfalso. apply (NonAbs x). unfold canon. cbn [app_subst]. rewrite Lx. reflexivity. }
    assert (Eu : canon Cf (TAbs x) = u) by (unfold canon; cbn [app_subst]; rewrite Lx; reflexivity).
    rewrite Eu in Leaves |- *. pose proof (lookup_subst_in_pair _ _ _ Lx) as Iq.
    apply Over.
    - rewrite <- Eu. apply (cf_read W Cf CfF).
      apply tyok_abs, typed_abstract. exact (ctxof_keys_typed Mt f Cf ns Ctx x Ix).
    - exact (proj2 (Dom x u Iq)).
    - intros z Iz. exact (Through z (proj2 (Dom x u Iq) z Iz) (Leaves z Iz)). }
  apply in_map_iff in Ir as [k [E Ik]].
  destruct k as [t0|v0|f0|r0 m0 a0|u0]; cbn in E; try discriminate. injection E as Er -> ->.
  destruct (need_provision f Cf ns _ Ctx Ik) as [p Ip].
  destruct (cf_prov W Cf CfF _ _ Ip) as [Or _]. cbn in Or.
  pose proof (proj2 (ctx_provision_scoped W f Cf ns Ctx _ _ Ip)) as Kg. cbn in Kg.
  rewrite <- Er. apply Over; [exact Or|exact Kg|]. intros z Iz. exact (Req z (Kg z Iz)).
Qed.

(* A fn or method body's context: the assumes' context with the
   parameters, its receiver (if any) read there. *)
Theorem fn_body_cxscoped : forall f C ns F this g names, CtxOf W f C ns -> SigOf W f (c_ty C) this g ->
  fr_this F = this -> OptScoped W C this ->
  CxScoped W F (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine names (sg_params g)) C).
Proof.
  intros f C ns F this g names Ctx Sig Th Sth.
  apply add_locals_scoped; [apply (ctxof_cxscoped f C ns F Ctx); intros t E; rewrite Th in E; exact (Sth t E)|].
  intros n t Iq y Iy. apply in_combine_r in Iq.
  destruct (sigof_gens W _ _ _ _ y Sig (sig_param_in g t y Iq Iy)) as [I|[x [E Ix]]]; [exact (tm_scoped W C y I)|exact (Sth x E y Ix)].
Qed.

(* An attached method's receiver, at its requirements' meanings. *)
Lemma attached_this_scoped : forall C r args,
  Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args -> ScopedTy W C (TNom r args).
Proof.
  intros C r args Fx y Iy. rewrite gens_nom in Iy. destruct (tm_gens_in _ y Iy) as [x [u [Iq Iu]]].
  destruct (Forall2_In_r _ _ _ _ _ _ Fx Iq) as [x' [_ [_ L]]]. exact (tm_scoped W C y (nlook_gens _ _ _ _ L Iu)).
Qed.

Lemma sig_scoped_none : forall C s g, SigOf W s (c_ty C) None g ->
  forall t, In t (sg_ret g :: sg_params g) -> ScopedTy W C t.
Proof.
  intros C s g Sig t It y Iy.
  assert (Ig : In y (sig_gens g)) by (unfold sig_gens; apply in_flat_map; exists t; split; assumption).
  destruct (sigof_gens W _ _ _ _ y Sig Ig) as [I|[x [E _]]]; [exact (tm_scoped W C y I)|discriminate].
Qed.

Lemma meth_sig_scoped : forall C p p' m a bs ar rt sg, Brackets W p (c_ty C) m a bs ->
  TyOf W p' (bs ++ c_ty C) None ar rt -> SigOf W m (bs ++ c_ty C) (Some rt) sg ->
  forall t, In t (rt :: sg_ret sg :: sg_params sg) -> ScopedTy W C t.
Proof.
  intros C p p' m a bs ar rt sg B T Sig.
  assert (Full : forall y, In y (tm_gens (bs ++ c_ty C)) -> GenScoped W C y).
  { intros y Iy. apply tm_scoped. apply tm_gens_app in Iy as [Iy|Iy]; [exact (br_gens W _ _ _ _ _ B y Iy)|exact Iy]. }
  assert (Rt : ScopedTy W C rt) by (intros y Iy; exact (Full y (tyof_none_gens W _ _ _ _ T y Iy))).
  intros t [<-|It] y Iy; [exact (Rt y Iy)|].
  assert (Ig : In y (sig_gens sg)) by (unfold sig_gens; apply in_flat_map; exists t; split; assumption).
  destruct (sigof_gens W _ _ _ _ y Sig Ig) as [I|[x [E Ix]]]; [exact (Full y I)|injection E as <-; exact (Rt y Ix)].
Qed.

(* A lambda provider's body context (LambdaProvider's Cl with the
   parameters), its signature and receiver read in C. *)
Theorem lambda_cxscoped : forall C caps capvals Fl names ts, LocalsScoped W C -> ProvsScoped W C ->
  (forall t, fr_this Fl = Some t -> ScopedTy W C t) -> (forall t, In t ts -> ScopedTy W C t) -> MethsRead W C ->
  CxScoped W Fl (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine names ts)
    (fold_left (fun C' q => let '(n, v) := q in
                    match slook n (c_locals C) with
                    | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                    | None => C' end)
                 (combine caps capvals)
                 (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) []))).
Proof.
  intros C caps capvals Fl names ts Ls Ps Th Ts Mr.
  assert (Step : forall l C0, scope_gens C0 = scope_gens C -> CxScoped W Fl C0 ->
    let C1 := fold_left (fun C' q => let '(n, v) := q in
                    match slook n (c_locals C) with
                    | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                    | None => C' end) l C0 in
    scope_gens C1 = scope_gens C /\ CxScoped W Fl C1).
  { induction l as [|[n v] r IH]; intros C0 E0 Cs0; [split; assumption|]. cbn. apply IH.
    - destruct (slook n (c_locals C)) as [[t mu]|]; [|exact E0].
      change (scope_gens (add_local (provide C0 (IVal v) [] (set_val C0 v t)) n t LCaptured) = scope_gens C).
      rewrite scope_add_local, provide_scope. exact E0.
    - destruct (slook n (c_locals C)) as [[t mu]|] eqn:L; [|exact Cs0].
      assert (St : ScopedTy W C0 t) by (apply (scoped_ext W _ _ _ (eq_sym E0)); exact (Ls n t mu (slook_in _ _ _ _ L))).
      apply add_local_scoped; [|apply (scoped_ext W C0 _ t); [rewrite provide_scope; reflexivity|exact St]].
      apply provide_scoped; [exact (set_val_scoped W Fl C0 v t Cs0 St)|intros y []|intros r0 m0 E; discriminate E]. }
  destruct (Step (combine caps capvals) (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) [])) as [E1 Cs1].
  - reflexivity.
  - split; [intros n t mu []|split; [exact Ps|exact (conj Th Mr)]].
  - apply add_locals_scoped; [exact Cs1|]. intros n t Iq. apply (scoped_ext W _ _ _ (eq_sym E1)). exact (Ts t (in_combine_r _ _ _ _ Iq)).
Qed.

End Initial.

(* ---- elaboration keeps types scoped ---------------------------------------------- *)

Section Elab.
Variable W : A.world.
Variable Prog : program.
Import A.

Hypothesis units_bare : UnitsBare W.
Hypothesis family_sig_ok : FamilySigReadOffOk W.

(* What a run of bind items does to the scope: it grows, by at most the
   generations the items made. *)
Definition BindScoped (F : frame) (C C' : cx) (F' : frame) (made : list nat) : Prop :=
  CxScoped W F' C' /\ incl (scope_gens C) (scope_gens C') /\
  (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ In y made) /\ fr_this F' = fr_this F.

Lemma ex_ok' : forall F C a x t, Ex W Prog F C a None x t -> FormedCx W C -> FrameFormed W F -> TyOk W t.
Proof. intros F C a x t D Fc Ff. exact (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ D Fc Ff (optok_none W)). Qed.

Lemma optscoped_none : forall C, OptScoped W C None.
Proof. intros C t E. discriminate. Qed.

Fixpoint ex_scoped F C a exp x t (D : Ex W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with branch_scoped F C a exp x t (D : Branch W Prog F C a exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with arms_scoped F C st arms exp xarms t (D : Arms W Prog F C st arms exp xarms t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W st -> ScopedTy W C st -> OptScoped W C exp -> ScopedTy W C t
with armslist_scoped F C st arms exp xarms hs ca t (D : ArmsList W Prog F C st arms exp xarms hs ca t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W st -> ScopedTy W C st -> OptScoped W C exp -> ScopedTy W C t
with methcall_scoped F C ox ot pa args x t (D : MethCall W Prog F C ox ot pa args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W ot -> ScopedTy W C ot -> ScopedTy W C t
with provided_scoped F C ox ot m args x t (D : Provided W Prog F C ox ot m args x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> TyOk W ot -> ScopedTy W C ot -> ScopedTy W C t
with blke_scoped F C b exp x t (D : BlkE W Prog F C b exp x t) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp -> ScopedTy W C t
with blk_scoped F C ss tl exp x t made (D : Blk W Prog F C ss tl exp x t made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> OptScoped W C exp ->
  forall y, In y (gens t) -> GenScoped W C y \/ In y made
with binditems_scoped F C its wrap C' F' made (D : BindItems W Prog F C its wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> BindScoped F C C' F' made
with binditem_scoped F C it wrap C' F' made (D : BindItem W Prog F C it wrap C' F' made) {struct D} :
  FormedCx W C -> FrameFormed W F -> CxScoped W F C -> BindScoped F C C' F' made.
Proof.
  (* ---- Ex ---- *)
  - destruct D as [F C exp|F C exp t Th|F C exp n t mu L Nm|F C exp pa s Hl R K|F C exp pa s d n t0 t Hl R D' Ast L
                  |F C exp pa s d n t0 i t x Hl R D' Ast As T Ex0|F C exp pa a args s bs xs pay y v ty R K B Fx Pay Ea Ey Fi
                  |F C exp pa args f Cf ns th g xs wrap C' R K Def Ctx Dm Needs CT Sig As
                  |F C exp pa args f g xs R K Abs L As|F C exp pa bs args f x t R K Br Dx|F C exp o pa args ox ot x t Eo Mc
                  |F C exp op y lib n m ox ot x t Op Ls Ey Hd Pv|F C exp op l r lib n m ox ot x t Op Ls El Hd Pv
                  |F C exp text x t Nt Lt|F C exp c b cx' bx bt Cd Bb Of Bt|F C exp c b y cx' bx bt ex' et t Cd Bb Br Of J
                  |F C exp s arms sx st xarms t Es Ar|F C exp b x t Bb|F C exp Rt|F C exp e expected v actual Rt Ee Fi
                  |F C exp Lp|F C exp pa a fs s bs xs pay ftys xfs R K B Fx Pay Ep Nd Len F2
                  |F C exp o n ox ot rec t Eo Rc Ft];
      intros Fc Ff Cs Se; pose proof Cs as [Lc [Ps [Ts Mr]]].
    + exact (unit_scoped W C).
    + exact (Ts t Th).
    + exact (Lc n t mu (slook_in _ _ _ _ L)).
    + intros y' [].
    + exact (val_scoped W C _ _ Ps L).
    + intros y' Iy. destruct (tyof_none_gens W _ _ _ _ T y' Iy).
    + exact (nom_reqs_scoped W C _ _ _ _ _ _ _ B Fx).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      intros y' Iy. destruct (sigof_gens W _ _ _ _ y' Sig (sig_ret_in g y' Iy)) as [I|[x0 [E _]]]; [|discriminate].
      rewrite Ety in I. exact (tm_scoped W C y' I).
    + exact (fn_ret_scoped W C _ _ Ps L).
    + exact (ex_scoped _ _ _ _ _ _ Dx Fc Ff Cs (optscoped_none C)).
    + pose proof (ex_scoped _ _ _ _ _ _ Eo Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ Eo Fc Ff) as Ot.
      exact (methcall_scoped _ _ _ _ _ _ _ _ Mc Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + pose proof (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ Ey Fc Ff) as Ot.
      exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + pose proof (ex_scoped _ _ _ _ _ _ El Fc Ff Cs (optscoped_none C)) as So.
      pose proof (ex_ok' _ _ _ _ _ El Fc Ff) as Ot.
      exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs (canon_ok W C ot (proj1 Fc) Ot) (canon_scoped W C ot So)).
    + exact (literal_scoped W C _ _ _ Lt Ps).
    + exact (unit_scoped W C).
    + pose proof (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se) as Hb. pose proof (branch_scoped _ _ _ _ _ _ Br Fc Ff Cs Se) as He.
      exact (join_scoped W C _ _ _ _ J (branch_ty_scoped W C b bt Hb) He Se).
    + pose proof (ex_scoped _ _ _ _ _ _ Es Fc Ff Cs (optscoped_none C)) as Hs.
      pose proof (ex_ok' _ _ _ _ _ Es Fc Ff) as Ot.
      exact (arms_scoped _ _ _ _ _ _ _ Ar Fc Ff Cs (canon_ok W C st (proj1 Fc) Ot) (canon_scoped W C st Hs) Se).
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se)].
    + exact (never_scoped W C).
    + exact (never_scoped W C).
    + exact (never_scoped W C).
    + exact (nom_reqs_scoped W C _ _ _ _ _ _ _ B Fx).
    + pose proof (ex_scoped _ _ _ _ _ _ Eo Fc Ff Cs (optscoped_none C)) as So.
      assert (Rs : ScopedTy W C rec).
      { destruct Rc as [(s & xs & -> & Kt & Pay)| ->]; [|exact So].
        intros y' Iy. apply So. rewrite gens_nom. exact (payload_gens W _ _ _ y' Pay Iy). }
      destruct Ft as [ftys [-> S]]. intros y' Iy. apply Rs. rewrite gens_record. apply in_flat_map.
      exists (n, t). split; [exact (slook_in _ _ _ _ S)|exact Iy].
  (* ---- Branch ---- *)
  - destruct D as [F C b exp x t Bb|F C y exp x t Nb Ey]; intros Fc Ff Cs Se.
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (branch_ty_scoped W C b t (blke_scoped _ _ _ _ _ _ Bb Fc Ff Cs Se))].
    + exact (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Se).
  (* ---- Arms ---- *)
  - destruct D as [F C exp|F C st arms exp xarms covered catch t Nn Al Exh]; intros Fc Ff Cs Os Ss Se.
    + destruct exp as [e|]; [exact (Se e eq_refl)|exact (never_scoped W C)].
    + exact (armslist_scoped _ _ _ _ _ _ _ _ _ Al Fc Ff Cs Os Ss Se).
  (* ---- ArmsList ---- *)
  - destruct D as [F C st pat body exp xp binds hs ca bx bt Pt Br Of
                  |F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t Ne Pt Br Of Al J]; intros Fc Ff Cs Os Ss Se.
    + pose proof (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) as Fb.
      pose proof (add_locals_scoped W F LLet binds C Cs (patof_scoped W C _ _ _ _ _ _ _ Pt Ss)) as Cb.
      pose proof (add_locals_scope LLet binds C) as Eb.
      apply (scoped_ext W _ _ _ Eb). apply (branch_scoped _ _ _ _ _ _ Br Fb Ff Cb).
      intros e E. exact (scoped_ext W _ _ _ (eq_sym Eb) (Se e E)).
    + pose proof (add_locals_formed W LLet binds C Fc (patof_ok W _ _ _ _ _ _ _ Pt Os)) as Fb.
      pose proof (add_locals_scoped W F LLet binds C Cs (patof_scoped W C _ _ _ _ _ _ _ Pt Ss)) as Cb.
      pose proof (add_locals_scope LLet binds C) as Eb.
      assert (Hb : ScopedTy W C bt).
      { apply (scoped_ext W _ _ _ Eb). apply (branch_scoped _ _ _ _ _ _ Br Fb Ff Cb).
        intros e E. exact (scoped_ext W _ _ _ (eq_sym Eb) (Se e E)). }
      pose proof (armslist_scoped _ _ _ _ _ _ _ _ _ Al Fc Ff Cs Os Ss Se) as Hr.
      exact (join_scoped W C _ _ _ _ J Hb Hr Se).
  (* ---- MethCall ---- *)
  - destruct D as [F C ox ot pa args m x t Len Rd Pv|F C ox ot n args h att m x t Hd At Sc Ml Pv
                  |F C ox ot n args h att Ca ns th g xs rargs wrap Cd Hd At Nw Def Ra Ctx Dm Needs CT Sig As
                  |F C ox ot n args h att x t Hd At Nw Abs Pv|F C ox ot n args m x t Na Sc Pv]; intros Fc Ff Cs Ot So.
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + destruct (demand_shape W _ _ _ _ _ _ Dm) as [Ety _].
      destruct (fold_set_ty_shape rargs C) as [Ety' _].
      intros y Iy. destruct (sigof_gens W _ _ _ _ y Sig (sig_ret_in g y Iy)) as [I|[x0 [E Ix]]]; [|injection E as <-; exact (So y Ix)].
      rewrite Ety, Ety' in I. apply tm_gens_app in I as [I|I]; [|exact (tm_scoped W C y I)].
      apply So. subst rargs. destruct (tm_gens_in _ y I) as [x0 [u [Iq Iu]]]. apply (proj2 (in_rev _ _)) in Iq.
      destruct ot; try contradiction. rewrite gens_nom. exact (in_tm_gens _ _ _ _ Iq Iu).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
    + exact (provided_scoped _ _ _ _ _ _ _ _ Pv Fc Ff Cs Ot So).
  (* ---- Provided ---- *)
  - destruct D as [F C ox ot m args g xs Ml As|F C ox ot m args u matching th sm C' ks g xs Ml FAp Ro Ik Of Ml' As];
      intros Fc Ff Cs Ot So; pose proof Cs as [Ls [Ps [Ts Mr]]].
    + exact (mth_ret_scoped W C _ _ _ Ps Ml).
    + destruct (mths_in C' _ _ _ Ml') as [k [a Iq]].
      assert (Mt : forall x v, In (x, v) matching -> ScopedTy W C (canon C v))
        by (intros x v Iv; exact (proj2 (family_matching_scoped W _ _ _ _ _ Ro Ot So x v Iv))).
      intros y Iy. pose proof (sig_ret_gens (ProvidedMethod g a) g a eq_refl eq_refl y Iy) as Ip.
      destruct (family_application_gens W _ _ _ _ _ _ _ FAp Mt _ _ Iq) as [Iq'|G]; [exact (Ps _ _ Iq' y Ip)|exact (G y Ip)].
  (* ---- BlkE ---- *)
  - destruct D as [F C ss tl exp x t made Bk Esc Of]; intros Fc Ff Cs Se.
    intros y Iy. destruct (blk_scoped _ _ _ _ _ _ _ _ Bk Fc Ff Cs Se y Iy) as [G|Im]; [exact G|].
    exfalso. exact (Esc y Im Iy).
  (* ---- Blk ---- *)
  - destruct D as [F C y exp x t Ey|F C exp
                  |F C st n ann y rest tl exp declared v vt t0 xr t made mu Ls Ann Ey Dc Nm Rest
                  |F C n y rest tl exp tv v vt xr t made L Ey Fi Rest Nm|F C y exp v vt Ey
                  |F C y rest tl exp v vt xr t made Ne Ey Rest|F C c b rest tl exp cx' bx bt xr t made Cd Bb Rest
                  |F C b rest tl exp bx bt xr t made Ne Bb Rest|F C b exp bx bt Bb Nb
                  |F C its rest tl exp wrap C' F' xr t made made' Bi Rest]; intros Fc Ff Cs Se.
    + intros y' Iy. left. exact (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Se y' Iy).
    + intros y' [].
    + assert (Od : OptOk W declared /\ OptScoped W C declared).
      { destruct ann as [a|].
        - destruct Ann as [d [T ->]]. split; [apply optok_some; exact (tyof_ok W _ _ _ _ _ T (cf_ty _ _ (proj1 Fc)) (proj2 Ff))|].
          intros d' E. injection E as <-. apply (tyof_scoped_ctx W C _ _ _ _ T). exact (proj1 (proj2 (proj2 Cs))).
        - rewrite Ann. split; [exact (optok_none W)|exact (optscoped_none C)]. }
      destruct Od as [Od Sd].
      pose proof (ex_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ Ey Fc Ff Od) as Hvt.
      assert (Ht0 : TyOk W t0 /\ ScopedTy W C t0).
      { destruct declared as [d|]; [destruct Dc as [_ ->]; exact (conj (Od d eq_refl) (Sd d eq_refl))|].
        rewrite Dc. exact (conj Hvt (ex_scoped _ _ _ _ _ _ Ey Fc Ff Cs Sd)). }
      exact (blk_scoped _ _ _ _ _ _ _ _ Rest (add_local_formed W C n t0 mu Fc (proj1 Ht0)) Ff
        (add_local_scoped W F C n t0 mu Cs (proj2 Ht0)) Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + destruct (ty_eqb vt never); intros y' [].
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + exact (blk_scoped _ _ _ _ _ _ _ _ Rest Fc Ff Cs Se).
    + intros y' [].
    + destruct (binditems_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ Bi Fc Ff) as [Fc' Ff'].
      destruct (binditems_scoped _ _ _ _ _ _ _ Bi Fc Ff Cs) as [Cs' [Inc [Back _]]].
      intros y' Iy. destruct (blk_scoped _ _ _ _ _ _ _ _ Rest Fc' Ff' Cs' (fun e E => scoped_mono W _ _ _ Inc (Se e E)) y' Iy)
        as [G|Im].
      * destruct (Back y' G) as [I|I]; [left; exact I|right; apply in_or_app; left; exact I].
      * right. apply in_or_app. right. exact Im.
  (* ---- BindItems ---- *)
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 B1 B2]; intros Fc Ff Cs.
    + split; [exact Cs|split; [intros y I; exact I|split; [intros y I; left; exact I|reflexivity]]].
    + destruct (binditem_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ B1 Fc Ff) as [Fc1 Ff1].
      destruct (binditem_scoped _ _ _ _ _ _ _ B1 Fc Ff Cs) as [Cs1 [Inc1 [Back1 Th1]]].
      destruct (binditems_scoped _ _ _ _ _ _ _ B2 Fc1 Ff1 Cs1) as [Cs2 [Inc2 [Back2 Th2]]].
      split; [exact Cs2|split; [intros y I; exact (Inc2 y (Inc1 y I))|split; [|rewrite Th2; exact Th1]]].
      intros y I. destruct (Back2 y I) as [I1|I1]; [destruct (Back1 y I1) as [I2|I2]; [left; exact I2|]|];
        right; apply in_or_app; [left|right]; assumption.
  (* ---- BindItem ---- *)
  - destruct D as [F C pa t rp ra tau R K Np T
                  |F C pa v d n t0 i declared yv x xt a R D' Ast T Ey Fi App
                  |F C pa f g sg pg sm a types R K Sig FP App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a yp r m bs rt sg target pre sm ap types R MN B T Sig MP App NoFam
                  |F C pa u d n ps a r bodies wrap C' F' made R D' Ast Na Bi
                  |F C pa a Typed
                  |F C pa u pg h Cu nsu Ch nsh target sm fx
                  |F C pa u pg gg Cu nsu Cg nsg h pins sm smc fx]; intros Fc Ff Cs.
    + assert (Sv : forall y, In y (gens (canon C tau)) -> In y (scope_gens C)).
      { intros y Iy. destruct (gens_canon C tau y Iy) as [I|I]; [exact (ty_scope C y (tyof_none_gens W _ _ _ _ T y I))|exact I]. }
      split; [exact (set_ty_scoped W F C t _ Cs)|split; [exact (scope_set_ty C t _)|split; [|reflexivity]]].
      intros y I. left. exact (set_ty_back C t _ Sv y I).
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [apply set_val_scoped; [exact Cs|exact (tyof_scoped_ctx W C _ _ _ _ T (optscoped_none C))]| |intros r0 m0 E; discriminate E].
      intros z Iz. exact (tm_scoped W C z (appof_scoped W _ _ _ App z Iz)).
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [|intros y Iy; exact (tm_scoped W C y (appof_scoped W _ _ _ App y Iy))|intros r0 m0 E; discriminate E].
      apply (set_sig_scoped W F C (IFn f) sg Cs). intros y Iy.
      destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E _]]]; [exact (tm_scoped W C y I)|discriminate].
    + split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      apply provide_scoped; [|intros y Iy; exact (tm_scoped W C y (appof_scoped W _ _ _ App y Iy))|intros r0 m0 E; discriminate E].
      apply (set_sig_scoped W F C (IFn f) sg Cs). intros y Iy.
      destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E _]]]; [exact (tm_scoped W C y I)|discriminate].
    + assert (Full : forall y, In y (tm_gens (bs ++ c_ty C)) -> GenScoped W C y).
      { intros y Iy. apply tm_scoped. apply tm_gens_app in Iy as [Iy|Iy]; [exact (br_gens W _ _ _ _ _ B y Iy)|exact Iy]. }
      split; [|rewrite provide_scope; split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]]].
      assert (Cs1 : CxScoped W F (set_provision C (mkey (canon C rt) m) (ProvidedFn sg []))).
      { apply (set_sig_scoped W F C _ sg Cs). intros y Iy.
        destruct (sigof_gens W _ _ _ _ y Sig Iy) as [I|[x0 [E Ix]]]; [exact (Full y I)|].
        injection E as <-. exact (Full y (tyof_none_gens W _ _ _ _ T y Ix)). }
      assert (Cs2 : LocalsScoped W (set_mth C (canon C rt) m sg) /\ ProvsScoped W (set_mth C (canon C rt) m sg) /\
        ThisScoped W F (set_mth C (canon C rt) m sg)).
      { destruct Cs1 as [L1 [P1 [T1 _]]]. split; [exact L1|split; [|exact T1]].
        intros k p [E|Iq]; [|exact (P1 k p (or_intror Iq))]. injection E as <- <-.
        exact (P1 _ (ProvidedFn sg []) (or_introl eq_refl)). }
      destruct (provide_scoped3 W F C (mkey (canon C rt) m) ap _ Cs2 (fun y Iy => Full y (appof_scoped W _ _ _ App y Iy))) as [L3 [P3 T3]].
      split; [exact L3|split; [exact P3|split; [exact T3|]]].
      destruct Cs as [_ [_ [_ Mr]]]. pose proof Mr as [Section [Tc _]].
      assert (TcB : forall x v, In (x, v) (bs ++ c_ty C) -> canon C v = v).
      { intros x v Iq. apply in_app_or in Iq as [Iq|Iq]; [exact (brackets_canon W C _ _ _ _ _ Tc B x v Iq)|exact (Tc x v Iq)]. }
      assert (Rt : canon C rt = rt) by exact (tyof_canon W C _ _ None _ _ TcB ltac:(intros x0 E; discriminate E) T).
      apply methsread_meth_bind; [exact Mr|].
      rewrite <- (sigof_canon W C m _ (Some rt) sg TcB ltac:(intros x0 E; injection E as <-; exact Rt) Sig).
      exact (methread_of W C m _ rt sg ap Section Sig App).
    + destruct (binditems_scoped _ _ _ _ _ _ _ Bi Fc Ff Cs) as [[Ls' [Ps' [Ts' Mr']]] [Inc [Back Th]]].
      split; [split; [exact Ls'|split; [exact Ps'|split; [|exact Mr']]]|split; [exact Inc|split; [exact Back|reflexivity]]].
      intros t E. cbn in E. apply Ts'. rewrite Th. exact E.
    + destruct a as [u Cu ns th outs gensof newgens C' ks].
      destruct Typed as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OutsI & PR & _).
      destruct (apply_abstract_scope W _ _ _ _ _ PR) as [Inc Back].
      destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs' [OutsI' Ety]]]].
      split; [exact (apply_abstract_cxscoped W F (with_fresh F newgens) C u gensof C' ks PR eq_refl Cs)|split; [exact Inc|split; [|reflexivity]]].
      intros y I. destruct (Back y I) as [I'|[t [Ft ->]]]; [left; exact I'|right].
      apply in_map. exact (proj2 (OutsI t) Ft).
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [apply set_fun_scoped; [exact Cs|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))]|] end.
      split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]].
    + match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        split; [apply set_fun_scoped; [exact Cs|intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq))]|] end.
      split; [intros z I; exact I|split; [intros z I; left; exact I|reflexivity]].
Qed.

End Elab.
