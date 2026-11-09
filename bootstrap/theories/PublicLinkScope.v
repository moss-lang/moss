(* What a run of bind items adds to a context mentions only its scope
   (linknewpf). PublicScoping's [binditems_scoped] allows a provision to
   mention a declared type symbol out of scope as well (a family demand
   may read an unbound requirement as itself), but bind items never do:
   every provision and key a bind item adds is read through the type
   table (or the brackets), so it mentions only [scope_gens]. Together
   with [binditems_scoped]'s growth by `made` this bounds a body's new
   provisions by the context it starts from. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicCallRealize PublicFormation PublicScoping.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Scope.
Variable W : A.world.
Variable Prog : program.
Import A.
Hypothesis units_bare : UnitsBare W.
Hypothesis family_sig_ok : FamilySigReadOffOk W.

Definition ProvScope (C C' : cx) : Prop := forall k p, In (k, p) (c_provisions C') ->
  In (k, p) (c_provisions C) \/ (incl (provision_gens p) (scope_gens C') /\ incl (PublicCallRealize.key_gens k) (scope_gens C')).

Definition StepScope (C C' : cx) (m : list nat) : Prop :=
  incl (scope_gens C) (scope_gens C') /\ (forall y, In y (scope_gens C') -> In y (scope_gens C) \/ In y m) /\
  ProvScope C C' /\ c_sub C' = c_sub C.

Lemma set_provision_in : forall C k0 p0 k p, In (k, p) (c_provisions (set_provision C k0 p0)) ->
  (k, p) = (k0, p0) \/ In (k, p) (c_provisions C).
Proof.
  intros C k0 p0 k p I. cbn in I. destruct I as [E|I]; [left; symmetry; exact E|right].
  apply filter_In in I as [I _]. exact I.
Qed.

Lemma provide_set_in : forall C k0 p0 a k p, In (k, p) (c_provisions (provide C k0 a (set_provision C k0 p0))) ->
  In (k, p) (c_provisions C) \/ (k = k0 /\ incl (provision_gens p) (provision_gens p0 ++ inst_gens a)).
Proof.
  intros C k0 p0 a k p I. unfold provide in I.
  assert (L : ilook k0 (c_provisions (set_provision C k0 p0)) = Some p0) by (cbn; rewrite item_eqb_refl'; reflexivity).
  rewrite L in I.
  assert (Old : forall q, In (k, p) (c_provisions (set_provision (set_provision C k0 p0) k0 q)) ->
    (k, p) = (k0, q) \/ In (k, p) (c_provisions C) \/ (k, p) = (k0, p0)).
  { intros q Iq. destruct (set_provision_in _ _ _ _ _ Iq) as [E|Iq']; [left; exact E|right].
    destruct (set_provision_in _ _ _ _ _ Iq') as [E|Iq'']; [right; exact E|left; exact Iq'']. }
  assert (Fin : forall q, incl (provision_gens q) (provision_gens p0 ++ inst_gens a) ->
    In (k, p) (c_provisions (set_provision (set_provision C k0 p0) k0 q)) ->
    In (k, p) (c_provisions C) \/ (k = k0 /\ incl (provision_gens p) (provision_gens p0 ++ inst_gens a))).
  { intros q Sq Iq. destruct (Old q Iq) as [E|[Ic|E]].
    - injection E as -> ->. right. split; [reflexivity|exact Sq].
    - left. exact Ic.
    - injection E as -> ->. right. split; [reflexivity|intros y Iy; apply in_or_app; left; exact Iy]. }
  destruct p0 as [t a0|g a0|g a0|fx].
  - apply (Fin (ProvidedVal t a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - apply (Fin (ProvidedFn g a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - apply (Fin (ProvidedMethod g a)); [|exact I]. intros y Iy. unfold provision_gens in Iy |- *. cbn in Iy |- *.
    apply in_app_or in Iy as [Iy|Iy]; apply in_or_app; [left; apply in_or_app; left; exact Iy|right; exact Iy].
  - destruct (set_provision_in _ _ _ _ _ I) as [E|Ic]; [injection E as -> ->; right; split; [reflexivity|intros y Iy; apply in_or_app; left; exact Iy]|left; exact Ic].
Qed.

Lemma scope_provide_set : forall C k0 p0 a, scope_gens (provide C k0 a (set_provision C k0 p0)) = scope_gens C.
Proof. intros C k0 p0 a. rewrite provide_scope. reflexivity. Qed.

Lemma sub_provide_set : forall C k0 p0 a, c_sub (provide C k0 a (set_provision C k0 p0)) = c_sub C.
Proof. intros C k0 p0 a. destruct (provide_tables C k0 a (set_provision C k0 p0)) as [_ [E _]]. exact E. Qed.

(* A provision made at C, scoped: then the step is scoped. *)
Lemma step_provide : forall C k0 p0 a,
  incl (provision_gens p0 ++ inst_gens a) (scope_gens C) -> incl (PublicCallRealize.key_gens k0) (scope_gens C) ->
  StepScope C (provide C k0 a (set_provision C k0 p0)) [].
Proof.
  intros C k0 p0 a Sp Sk. unfold StepScope. rewrite scope_provide_set, sub_provide_set.
  split; [intros y I; exact I|split; [intros y I; left; exact I|split; [|reflexivity]]].
  intros k p I. rewrite scope_provide_set. destruct (provide_set_in _ _ _ _ _ _ I) as [Ic|[-> Sg]]; [left; exact Ic|right].
  split; [intros y Iy; exact (Sp y (Sg y Iy))|exact Sk].
Qed.

Lemma step_set_fun : forall C u fx, (forall x tx, In (x, tx) fx -> nlook x (c_ty C) = Some tx) ->
  StepScope C (set_fun C u fx) [].
Proof.
  intros C u fx Fx. split; [intros y I; exact I|split; [intros y I; left; exact I|split; [|reflexivity]]].
  intros k p I. destruct (set_provision_in _ _ _ _ _ I) as [E|Ic]; [|left; exact Ic].
  injection E as -> ->. right. split; [|intros y []].
  intros y Iy. unfold provision_gens in Iy. cbn in Iy.
  apply in_flat_map in Iy as [[x o] [Iq Iy]]. apply in_map_iff in Iq as [[x' tx] [E Iq]]. cbn in E. injection E as -> <-.
  change (In y (scope_gens C)). apply ty_scope. exact (nlook_gens _ _ _ _ (Fx _ _ Iq) Iy).
Qed.

Lemma step_trans : forall C C1 C2 m1 m2, StepScope C C1 m1 -> StepScope C1 C2 m2 -> StepScope C C2 (m1 ++ m2).
Proof.
  intros C C1 C2 m1 m2 [I1 [B1 [P1 S1]]] [I2 [B2 [P2 S2]]].
  split; [intros y I; exact (I2 y (I1 y I))|split; [|split; [|rewrite S2; exact S1]]].
  - intros y I. destruct (B2 y I) as [I'|I']; [destruct (B1 y I') as [I''|I'']; [left; exact I''|]|];
      right; apply in_or_app; [left|right]; assumption.
  - intros k p I. destruct (P2 k p I) as [I'|S]; [|right; exact S].
    destruct (P1 k p I') as [I''|[Sa Sb]]; [left; exact I''|right].
    split; intros y Iy; [exact (I2 y (Sa y Iy))|exact (I2 y (Sb y Iy))].
Qed.

Lemma pg_split : forall p y, In y (provision_gens p) ->
  (exists g, provision_signature p = Some g /\ In y (sig_gens g)) \/ In y (inst_gens (provision_inst p)).
Proof.
  intros p y I. unfold provision_gens in I. apply in_app_or in I as [I|I]; [|right; exact I].
  destruct (provision_signature p) as [g|]; [left; exists g; split; [reflexivity|exact I]|contradiction].
Qed.

Lemma tm_in_scope : forall C y, In y (tm_gens (c_ty C)) -> In y (scope_gens C).
Proof. intros C y I. exact (ty_scope C y I). Qed.

Fixpoint item_scope F C it w C' F' m (D : BindItem W Prog F C it w C' F' m) {struct D} :
  FormedCx W C -> FrameFormed W F -> StepScope C C' m
with items_scope F C its w C' F' m (D : BindItems W Prog F C its w C' F' m) {struct D} :
  FormedCx W C -> FrameFormed W F -> StepScope C C' m.
Proof.
  - destruct D as [F C pa t rp ra tau R K Np T
                  |F C pa v d n t0 i declared yv x xt a R D' Ast T Ey Fi App
                  |F C pa f g sg pg sm a types R K Sig FP App
                  |F C pa f sg ps rt body pid pre sm a R K Sig LP App
                  |F C pa dn a yp r m bs rt sg target pre sm ap types R MN B T Sig MP App NoFam
                  |F C pa u d n ps a r bodies wrap C' F' made R D' Ast Na Bi
                  |F C pa u fx Cu ns sm th gensof C' ks newgens outs R Fx FC In' Th Fix Sat FF Inj Outs Nd OutsI PR
                  |F C pa u pg h Cu nsu Ch nsh sm fx
                  |F C pa u pg gg Cu nsu Cg nsg h sm smc fx]; intros Fc Ff.
    + (* bind T = tau *)
      assert (Sv : forall y, In y (gens (canon C tau)) -> In y (scope_gens C)).
      { intros y Iy. destruct (gens_canon C tau y Iy) as [I|I]; [exact (ty_scope C y (tyof_none_gens W _ _ _ _ T y I))|exact I]. }
      split; [exact (scope_set_ty C t _)|split; [intros y I; left; exact (set_ty_back C t _ Sv y I)|split; [|reflexivity]]].
      intros k p I. left. exact I.
    + (* bind v = e *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. cbn in Iy'. rewrite app_nil_r in Iy'. apply tm_in_scope. exact (tyof_none_gens W _ _ _ _ T y Iy').
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind f = g *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E _]]]; [exact (tm_in_scope C y I)|discriminate].
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind f = lambda *)
      apply step_provide; [|intros y []].
      intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
      * injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E _]]]; [exact (tm_in_scope C y I)|discriminate].
      * cbn in Iy'. contradiction.
      * apply tm_in_scope. exact (appof_scoped W _ _ _ App y Iy).
    + (* bind R.m = provider *)
      assert (Full : forall y, In y (tm_gens (bs ++ c_ty C)) -> In y (scope_gens C)).
      { intros y Iy. apply tm_in_scope. apply tm_gens_app in Iy as [Iy|Iy]; [exact (br_gens W _ _ _ _ _ B y Iy)|exact Iy]. }
      assert (Rt : forall y, In y (gens rt) -> In y (scope_gens C)) by (intros y Iy; exact (Full y (tyof_none_gens W _ _ _ _ T y Iy))).
      apply step_provide.
      * intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [destruct (pg_split _ _ Iy) as [[g0 [E Iy']]|Iy']|].
        -- injection E as <-. destruct (sigof_gens W _ _ _ _ y Sig Iy') as [I|[x0 [E Ix]]]; [exact (Full y I)|].
           injection E as <-. exact (Rt y Ix).
        -- cbn in Iy'. contradiction.
        -- exact (Full y (appof_scoped W _ _ _ App y Iy)).
      * intros y Iy. cbn in Iy. destruct (gens_canon C rt y Iy) as [I|I]; [exact (Rt y I)|exact I].
    + (* bind F; F with a body *)
      exact (items_scope _ _ _ _ _ _ _ Bi Fc Ff).
    + (* bind F; F abstract *)
      destruct (apply_abstract_scope W _ _ _ _ _ PR) as [Inc Back].
      destruct (provide_result_shape W _ _ _ _ _ PR) as [Esub [Elo [outs' [OutsI' Ety]]]].
      pose proof Fc as [Cf [Ks Ts]].
      assert (InTy : forall x v, In (x, v) (output_type_map C outs' (fun t => TAbs (gensof t))) ->
        In (x, app_subst (c_sub C') v) (c_ty C')).
      { intros x v Iq. rewrite Ety, Esub. apply in_map_iff. exists (x, v). split; [reflexivity|exact Iq]. }
      assert (OldIn : forall x v, In (x, v) (c_ty C) -> In (x, canon C v) (output_type_map C outs' (fun t => TAbs (gensof t)))).
      { intros x v Iq. unfold output_type_map. apply in_map_iff. exists (x, v). split; [reflexivity|apply in_or_app; right; exact Iq]. }
      assert (OutIn : forall t, In t outs' -> In (t, canon C (TAbs (gensof t))) (output_type_map C outs' (fun t => TAbs (gensof t)))).
      { intros t It. unfold output_type_map. apply in_map_iff. exists (t, TAbs (gensof t)). split; [reflexivity|].
        apply in_or_app; left. apply in_map_iff. exists t. split; [reflexivity|exact It]. }
      assert (OutAbs : forall t, FunOutputs W u t -> Abstract W (gensof t)).
      { intros t Ft. destruct (Outs t Ft) as [[_ [E _]]|[_ [Fr _]]];
          [rewrite E; exact (typed_abstract W _ (outputs_typed W _ _ Ft))|exact (fresh_abstract W _ Fr)]. }
      set (K := fun y => In y (scope_gens C')).
      assert (SubK : forall g v, In (g, v) (c_sub C) -> GensIn K (gens v)).
      { intros g v Iq y Iy. rewrite <- Esub in Iq. exact (scope_sub C' g v y Iq Iy). }
      assert (TyK : forall x v, In (x, v) (c_ty C) -> TyOk W (canon C v) /\ GensIn K (gens (canon C v))).
      { intros x v Iq. split; [exact (canon_ok W C v Cf (cf_ty _ _ Cf _ _ Iq))|].
        exact (table_scope C' x (canon C v) (InTy _ _ (OldIn _ _ Iq))). }
      assert (OutK : forall t, FunOutputs W u t ->
         TyOk W (canon C (TAbs (gensof t))) /\ GensIn K (gens (canon C (TAbs (gensof t))))).
      { intros t Ft. split; [exact (canon_ok W C _ Cf (tyok_abs W _ (OutAbs t Ft)))|].
        exact (table_scope C' t _ (InTy _ _ (OutIn t (proj2 (OutsI' t) Ft)))). }
      destruct (provide_result_ok W _ _ _ _ _ K PR (cf_sub _ _ Cf) SubK TyK OutK) as [_ [_ [_ Pr]]].
      split; [exact Inc|split; [|split; [|exact Esub]]].
      * intros y I. destruct (Back y I) as [I'|[t [Ft ->]]]; [left; exact I'|right].
        apply in_map. exact (proj2 (OutsI t) Ft).
      * intros k p Iq. destruct (Pr k p Iq) as [Iq'|[_ [_ Kr]]]; [left; exact Iq'|].
        destruct (provide_result_gens W _ _ _ _ _ K PR SubK (fun x v I => proj2 (TyK x v I)) (fun t F => proj2 (OutK t F)) k p Iq)
          as [Iq'|G]; [left; exact Iq'|right].
        split; [exact G|]. destruct k as [| | |r0 m0 th0|]; cbn [PublicCallRealize.key_gens]; try (intros y []).
        exact (Kr r0 m0 th0 eq_refl).
    + (* bind F = H, a copy *)
      match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        apply step_set_fun; intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq)) end.
    + (* bind F = G, a closure *)
      match goal with Fx : forall x tx, In (x, tx) fx <-> _ |- _ =>
        apply step_set_fun; intros x tx Iq; exact (proj2 (proj1 (Fx x tx) Iq)) end.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2]; intros Fc Ff.
    + split; [intros y I; exact I|split; [intros y I; left; exact I|split; [intros k p I; left; exact I|reflexivity]]].
    + destruct (binditem_ok W Prog units_bare family_sig_ok _ _ _ _ _ _ _ D1 Fc Ff) as [Fc1 Ff1].
      exact (step_trans _ _ _ _ _ (item_scope _ _ _ _ _ _ _ D1 Fc Ff) (items_scope _ _ _ _ _ _ _ D2 Fc1 Ff1)).
Qed.

End Scope.
