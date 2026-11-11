(* Static walks over a linked body's bind items (linknewpf).
   - [items_applies]: every abstract application a run of bind items
     elaborates to binds outputs of the functor it applies, and every
     generation the items make is such an output's destination.
   - So a family body (FamilyBindings: it applies only families, which
     have no outputs) makes nothing and writes no type meaning:
     [family_binds_no_dests]. *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead PublicFormation.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Walk.
Variable W : A.world.
Variable Prog : program.

Definition binding_apply (b : binding) : list (nat * list (nat * nat)) :=
  match b with BApply u _ outs _ _ => [(u, outs)] | _ => [] end.

(* The applications the run of binds an expression starts with makes. *)
Fixpoint applies (e : expr) : list (nat * list (nat * nat)) :=
  match e with
  | EBind b r => binding_apply b ++ applies r
  | _ => []
  end.

Definition OutsOk (p : nat * list (nat * nat)) : Prop :=
  forall q, In q (snd p) -> A.FunOutputs W (fst p) (fst q).

Definition WrapApplies (w : expr -> expr) (m : list nat) : Prop :=
  forall x, incl (applies x) (applies (w x)) /\
    (forall p, In p (applies (w x)) -> In p (applies x) \/ OutsOk p) /\
    (forall y, In y m -> exists p, In p (applies (w x)) /\ OutsOk p /\ In y (map snd (snd p))).

Lemma wrap_bind : forall b, binding_apply b = [] -> WrapApplies (fun x => EBind b x) [].
Proof.
  intros b E x. cbn [applies]. rewrite E. cbn [app]. split; [intros p I; exact I|split; [intros p I; left; exact I|intros y []]].
Qed.

Lemma lambda_applies : forall F C ps rt body sg this pid pre sm,
  A.LambdaProvider W Prog F C ps rt body sg this pid pre sm -> forall x, applies (pre x) = applies x.
Proof.
  intros F C ps rt body sg this pid pre sm LP. destruct LP. intros x. unfold A.translate_lambda_captures. cbv beta.
  match goal with |- applies (fold_right _ x ?l) = _ => generalize l end.
  intros l. induction l as [|q l IH]; [reflexivity|]. cbn. exact IH.
Qed.

Lemma meth_applies : forall F C y sg rt target types pre sm,
  A.MethProvider W Prog F C y sg rt target types pre sm -> forall x, applies (pre x) = applies x.
Proof.
  intros F C y sg rt target types pre sm MP. destruct MP; intros x; try reflexivity.
  match goal with LP : A.LambdaProvider _ _ _ _ _ _ _ _ _ _ _ _ |- _ => exact (lambda_applies _ _ _ _ _ _ _ _ _ _ LP x) end.
Qed.

Lemma wrap_pre : forall (pre : expr -> expr) b, (forall x, applies (pre x) = applies x) -> binding_apply b = [] ->
  WrapApplies (fun x => pre (EBind b x)) [].
Proof.
  intros pre b Pre E x. rewrite Pre. cbn [applies]. rewrite E. cbn [app].
  split; [intros p I; exact I|split; [intros p I; left; exact I|intros y []]].
Qed.

Fixpoint item_applies F C it w C' F' m (D : A.BindItem W Prog F C it w C' F' m) {struct D} : WrapApplies w m
with items_applies F C its w C' F' m (D : A.BindItems W Prog F C its w C' F' m) {struct D} : WrapApplies w m.
Proof.
  - destruct D.
    all: try (apply wrap_bind; reflexivity).
    + apply (wrap_pre pre); [exact (lambda_applies _ _ _ _ _ _ _ _ _ _ H2)|reflexivity].
    + apply (wrap_pre pre); [exact (meth_applies _ _ _ _ _ _ _ _ _ H4)|reflexivity].
    + exact (items_applies _ _ _ _ _ _ _ H3).
    + (* an abstract application *)
      match goal with
      | Typed : A.TypesApplication _ _ _ _ ?a |- _ =>
          destruct a as [u Cu ns th outs gensof newgens C' ks];
          destruct Typed as (_ & _ & _ & _ & _ & _ & _ & _ & _ & _ & OI & _)
      end.
      intros x. unfold A.translate_application, apply_output_expression.
      cbn [A.app_functor A.app_identity A.app_outputs applies binding_apply app].
      split; [intros p I; right; exact I|split].
      * intros p [<-|I]; [right|left; exact I].
        intros q Iq. cbn in Iq. apply in_map_iff in Iq as [t [<- It]]. exact (proj1 (OI t) It).
      * intros y Iy. exists (u, map (fun t => (t, gensof t)) outs). split; [left; reflexivity|split].
        -- intros q Iq. cbn in Iq. apply in_map_iff in Iq as [t [<- It]]. exact (proj1 (OI t) It).
        -- cbn [snd]. rewrite map_map. exact Iy.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2].
    + intros x. split; [intros p I; exact I|split; [intros p I; left; exact I|intros y []]].
    + pose proof (item_applies _ _ _ _ _ _ _ D1) as A1. pose proof (items_applies _ _ _ _ _ _ _ D2) as A2.
      intros x. destruct (A2 x) as [I2 [B2 M2]]. destruct (A1 (w2 x)) as [I1 [B1 M1]]. cbv beta.
      split; [intros p I; exact (I1 p (I2 p I))|split].
      * intros p I. destruct (B1 p I) as [I'|O]; [exact (B2 p I')|right; exact O].
      * intros y Iy. apply in_app_or in Iy as [Iy|Iy]; [exact (M1 y Iy)|].
        destruct (M2 y Iy) as [p [Ip Op]]. exists p. split; [exact (I1 p Ip)|exact Op].
Qed.

Lemma bindsof_applies : forall e l, A.BindsOf e l -> applies e = flat_map binding_apply l.
Proof. intros e l B. induction B as [|b body rest B IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Definition binding_dests (bs : list binding) : list nat :=
  flat_map (fun b => match b with BApply _ _ outs _ _ => map snd outs | _ => [] end) bs.

(* A family body applies only families, which have no outputs: it makes
   no generation, and no application in it has an output. *)
Theorem family_binds_no_dests : forall F C its wrap C' F' made binds,
  A.BindItems W Prog F C its wrap C' F' made -> A.BindsOf (wrap EUnit) binds -> A.FamilyBindings W binds ->
  made = [] /\ binding_dests binds = [] /\
  (forall u th outs sm rs, In (BApply u th outs sm rs) binds -> outs = []).
Proof.
  intros F C its wrap C' F' made binds D B Fb.
  destruct (items_applies _ _ _ _ _ _ _ D EUnit) as [_ [Bk Mk]]. cbn [applies] in Bk.
  rewrite (bindsof_applies _ _ B) in Bk, Mk. unfold A.FamilyBindings in Fb. rewrite Forall_forall in Fb.
  assert (NoOut : forall p, In p (flat_map binding_apply binds) -> snd p = []).
  { intros p Ip. destruct (Bk p Ip) as [[]|O].
    apply in_flat_map in Ip as [b [Ib Ip]]. pose proof (Fb b Ib) as Fbb.
    destruct b; cbn in Ip; try contradiction. destruct Ip as [<-|[]].
    cbn in Fbb |- *. destruct l as [|q l]; [reflexivity|exfalso].
    exact (family_no_outputs W _ _ (proj1 Fbb) (O q (or_introl eq_refl))). }
  assert (Ap : forall u th outs sm rs, In (BApply u th outs sm rs) binds -> outs = []).
  { intros u th outs sm rs I. apply (NoOut (u, outs)). apply in_flat_map. exists (BApply u th outs sm rs).
    split; [exact I|left; reflexivity]. }
  split; [|split; [|exact Ap]].
  - destruct made as [|y m]; [reflexivity|exfalso]. destruct (Mk y (or_introl eq_refl)) as [p [Ip [_ Iy]]].
    rewrite (NoOut p Ip) in Iy. exact Iy.
  - clear -Ap. induction binds as [|b r IH]; [reflexivity|]. cbn.
    assert (IH' : binding_dests r = []) by (apply IH; intros u' th' o' sm' rs' I; exact (Ap u' th' o' sm' rs' (or_intror I))).
    destruct b; cbn; try exact IH'.
    rewrite (Ap _ _ _ _ _ (or_introl eq_refl)). exact IH'.
Qed.

End Walk.
