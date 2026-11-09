(* A linked body's destinations avoid F's inputs (realize2).

   Every type meaning a run of bind items writes is at the destination of
   an abstract application in it (PublicChainCore.bindings_writes). Each
   destination is among the generations the items make
   ([items_dests_made]), and LinkBody's made clause keeps those out of
   F's inputs: [link_dests_out]. (BApplyAbstract once also kept a kept
   output out of the frame's pins; the made clause implies that for a
   linked body's own binds, so it was dropped, consol.) *)
From Stdlib Require Import List String Arith Bool.
From NM Require Import Audit PublicTypeRead.
Import Audit.Input Audit.Semantics ListNotations.
Module A := Audit.Semantics.Elaboration.

Section Pins.
Variable W : A.world.
Variable Prog : program.

Definition bind_apply (b : binding) : list (nat * list (nat * nat)) :=
  match b with BApply u _ outs _ _ => [(u, outs)] | _ => [] end.

(* The applications of the run of binds an expression starts with. *)
Fixpoint expr_applies (e : expr) : list (nat * list (nat * nat)) :=
  match e with
  | EBind b r => bind_apply b ++ expr_applies r
  | _ => []
  end.

(* An application's destinations are among the generations made. *)
Definition DestsOk (made : list nat) (p : nat * list (nat * nat)) : Prop :=
  forall q, In q (snd p) -> In (snd q) made.

Definition WrapDests (made : list nat) (w : expr -> expr) : Prop :=
  forall x p, In p (expr_applies (w x)) -> In p (expr_applies x) \/ DestsOk made p.

Lemma dests_ok_mono : forall m m' p, incl m m' -> DestsOk m p -> DestsOk m' p.
Proof. intros m m' p I O q Iq. exact (I _ (O q Iq)). Qed.

Lemma wrap_dests_bind : forall b, bind_apply b = [] -> WrapDests [] (fun x => EBind b x).
Proof. intros b E x p I. cbn [expr_applies] in I. rewrite E in I. left. exact I. Qed.

Lemma lambda_expr_applies : forall F C ps rt body sg this pid pre sm,
  A.LambdaProvider W Prog F C ps rt body sg this pid pre sm -> forall x, expr_applies (pre x) = expr_applies x.
Proof.
  intros F C ps rt body sg this pid pre sm LP. destruct LP. intros x. cbv beta.
  match goal with |- expr_applies (fold_right _ x ?l) = _ => generalize l end.
  intros l. induction l as [|q l IH]; [reflexivity|]. cbn. exact IH.
Qed.

Lemma meth_expr_applies : forall F C y sg rt target types pre sm,
  A.MethProvider W Prog F C y sg rt target types pre sm -> forall x, expr_applies (pre x) = expr_applies x.
Proof.
  intros F C y sg rt target types pre sm MP. destruct MP; intros x; try reflexivity.
  match goal with LP : A.LambdaProvider _ _ _ _ _ _ _ _ _ _ _ _ |- _ => exact (lambda_expr_applies _ _ _ _ _ _ _ _ _ _ LP x) end.
Qed.

Lemma wrap_dests_pre : forall (pre : expr -> expr) b, (forall x, expr_applies (pre x) = expr_applies x) ->
  bind_apply b = [] -> WrapDests [] (fun x => pre (EBind b x)).
Proof. intros pre b Pre E x p I. rewrite Pre in I. cbn [expr_applies] in I. rewrite E in I. left. exact I. Qed.

Fixpoint item_dests F C it w C' F' m (D : A.BindItem W Prog F C it w C' F' m) {struct D} :
  WrapDests m w
with items_dests F C its w C' F' m (D : A.BindItems W Prog F C its w C' F' m) {struct D} :
  WrapDests m w.
Proof.
  - destruct D.
    all: try (apply wrap_dests_bind; reflexivity).
    + apply (wrap_dests_pre pre); [exact (lambda_expr_applies _ _ _ _ _ _ _ _ _ _ H2)|reflexivity].
    + apply (wrap_dests_pre pre); [exact (meth_expr_applies _ _ _ _ _ _ _ _ _ H4)|reflexivity].
    + exact (items_dests _ _ _ _ _ _ _ H3).
    + (* an abstract application: its destinations are what it makes *)
      intros x p I. unfold apply_output_expression in I. cbn [expr_applies bind_apply app] in I.
      destruct I as [<-|I]; [right|left; exact I].
      intros q Iq. cbn [snd] in Iq. apply in_map_iff in Iq as [t [<- It]]. cbn [snd].
      apply in_map; exact It.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2].
    + intros x p I. left. exact I.
    + pose proof (item_dests _ _ _ _ _ _ _ D1) as W1. pose proof (items_dests _ _ _ _ _ _ _ D2) as W2.
      intros x p I. cbv beta in I. destruct (W1 (w2 x) p I) as [I'|O].
      * destruct (W2 x p I') as [I''|O]; [left; exact I''|right].
        apply (dests_ok_mono m2); [intros z Iz; apply in_or_app; right; exact Iz|exact O].
      * right. apply (dests_ok_mono m1); [intros z Iz; apply in_or_app; left; exact Iz|exact O].
Qed.

Lemma bindsof_expr_applies : forall e l, A.BindsOf e l -> expr_applies e = flat_map bind_apply l.
Proof. intros e l B. induction B as [|b body rest B IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Definition bind_dests (bs : list binding) : list nat :=
  flat_map (fun b => match b with BApply _ _ outs _ _ => map snd outs | _ => [] end) bs.

(* Every destination of the binds a run of items elaborates to is among
   the generations the items make. *)
Theorem items_dests_made : forall F C its wrap C' F' made binds,
  A.BindItems W Prog F C its wrap C' F' made -> A.BindsOf (wrap EUnit) binds ->
  forall y, In y (bind_dests binds) -> In y made.
Proof.
  intros F C its wrap C' F' made binds D B y Iy.
  pose proof (items_dests _ _ _ _ _ _ _ D) as Wp.
  unfold bind_dests in Iy. apply in_flat_map in Iy as [b [Ib Iy]].
  destruct b as [| | | | | |u th outs sm rs|]; try contradiction.
  apply in_map_iff in Iy as [q [<- Iq]].
  assert (Ip : In (u, outs) (expr_applies (wrap EUnit))).
  { rewrite (bindsof_expr_applies _ _ B). apply in_flat_map. exists (BApply u th outs sm rs). split; [exact Ib|left; reflexivity]. }
  destruct (Wp EUnit _ Ip) as [[]|O]. exact (O q Iq).
Qed.

(* With LinkBody's clause that nothing the body makes is an input of F,
   no destination is. *)
Corollary link_dests_out : forall (Cu : A.cx) Fg Cg its wrap Cb Fb made binds,
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  forall y, In y (bind_dests binds) -> A.nlook y (A.c_ty Cu) = None.
Proof.
  intros Cu Fg Cg its wrap Cb Fb made binds Out D B y Iy.
  exact (Out y (items_dests_made _ _ _ _ _ _ _ _ D B y Iy)).
Qed.

End Pins.
