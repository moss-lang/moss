(* Family signatures (PublicFormation.FamilySigOk).

   FamilySigOk as stated quantifies over every FamilyApplication, whose
   matching is constrained only to P(F) symbols, not to the values a
   demanded key determines. It is false for accepted programs: a family
   whose result is a bracketed context, e.g.

     type R; type X; type Y;
     assume Y { tag Foo(Y); }
     assume X { fn R.m(): X; }
     assume X { context Ctx = R.m; }
     functor Fam[Y]: R, Y -> Ctx[X = Foo];

   (accepted by the oracle and native; Fam is a Family: R.m's signature
   mentions no P(F) symbol), applied in a formed context
   `Cx [(R, TAbs R)] [] [(IFun Fam, ProvidedFunctor [(R, TAbs R)])] []`
   at the matching [(Y, bad)] for any `bad` that is not TyOk (say TAbs Foo),
   provides R.m at ot = TAbs R (TyOk) with result type
   TNom Foo [(Y, bad)], which is not TyOk. Neither FamilyKeyed nor the
   oracle's family_leaves_ok looks at a context leaf's brackets, and
   nothing but FamilyReadOff ties the matching to the key.

   What does hold, and is all the use site needs (PublicFormation.ex_ok's
   family Provided case has the FamilyReadOff premise in hand), is the
   same statement under FamilyReadOff: [family_sig_ok_readoff]. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicArms PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section FamilySig.
Variable W : A.world.
Import A.

(* A matching read off an ok receiver has ok values, within its generations. *)
Lemma readoff_values_ok : forall C u ot m matching,
  FamilyReadOff W C u ot m matching -> TyOk W ot ->
  forall x v, In (x, v) matching -> TyOk W (canon C v) /\ incl (gens (canon C v)) (gens ot).
Proof.
  intros C u ot m matching (marks & fr & fks & rf & a & Marks & PR & Ik & Dom & Nd & Ot & _) Ok x v Iq.
  assert (Ix : In x (gens rf)) by (apply (proj2 (proj1 (Dom x) (in_map fst _ _ Iq)))).
  pose proof (sub_subst (c_sub C) _ _ (sub_read rf matching x v Ix (lookup_subst_nodup matching x v Nd Iq))) as Sb.
  change (SubTy (canon C v) (canon C (app_subst matching rf))) in Sb. rewrite Ot in Sb.
  exact (sub_ok W _ _ Sb Ok).
Qed.

Theorem family_sig_ok_readoff : PublicFormation.FamilySigReadOffOk W.
Proof.
  intros C u matching th sm Cr ks ot m g FAp Ro Fc Ot _ Ml.
  destruct FAp as (formal & needs & (fixed & Fx & Fam & Poly & FC & Args & InF & FA & _ & PR & Nd) & _).
  pose proof (proj1 Fc) as Cf.
  set (K := scope_gens C ++ gens ot).
  destruct (family_context_shape matching C) as [Ety [Esub [Epr Elo]]].
  assert (P1 : ReadOk W (c_sub (family_context C matching))) by (rewrite Esub; exact (cf_read _ _ Cf)).
  assert (P2 : ReadWithin (fun y => In y K) (c_sub (family_context C matching))).
  { rewrite Esub. apply canon_within. intros y Iy. apply in_or_app. left. exact Iy. }
  assert (P3 : forall x v, In (x, v) (c_ty (family_context C matching)) ->
     TyOk W (canon (family_context C matching) v) /\ incl (gens (canon (family_context C matching) v)) K).
  { intros x v Iq. rewrite Ety in Iq. unfold canon. rewrite Esub. apply in_app_or in Iq as [Iq|Iq].
    - apply (proj2 (in_rev _ _)) in Iq. destruct (readoff_values_ok C u ot m matching Ro Ot x v Iq) as [Ov Iv].
      split; [exact Ov|]. intros y Iy. apply in_or_app. right. exact (Iv y Iy).
    - split; [exact (canon_ok W C v Cf (cf_ty _ _ Cf _ _ Iq))|]. intros y Iy. apply in_or_app. left.
      exact (scope_ty_canon C x v y Iq Iy). }
  assert (P4 : forall t, FunOutputs W u t -> TyOk W (canon (family_context C matching) (TAbs t)) /\
     incl (gens (canon (family_context C matching) (TAbs t))) K).
  { intros t Ft. exfalso. exact (family_no_outputs W u t (proj1 Fam) Ft). }
  destruct (provide_result_ok W _ _ _ _ _ _ PR P1 P2 P3 P4) as [_ [_ [_ Pr']]].
  destruct (mlook_in _ _ _ _ Ml) as [q L]. unfold c_mths in L. apply in_flat_map in L as [[k p] [Iq Ik]].
  assert (Pk : ProvOk W p).
  { destruct (Pr' k p Iq) as [Iq'|[_ [Pk _]]]; [|exact Pk].
    rewrite Epr in Iq'. exact (proj2 (cf_prov _ _ Cf k p Iq')). }
  destruct Pk as [Sg _].
  destruct k, p; cbn in Ik; try contradiction; destruct Ik as [E|[]]; injection E as _ E2; subst.
  exact (Sg _ eq_refl).
Qed.

End FamilySig.
