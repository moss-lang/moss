(* A linked body's destinations avoid F's inputs (realize2).

   Every type meaning a run of bind items writes is at the destination of
   an abstract application in it (PublicChainCore.bindings_writes). Each
   destination is fresh (`Fresh W`: undeclared) or a kept output, which
   BApplyAbstract keeps out of the frame's pins. Bind items keep the
   frame's pins, so in a body bound to F (pins = M(F)) every destination
   avoids M(F) and, being undeclared or outside M(F), is no mentioned
   input of F: [link_dests_avoid]. *)
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

(* An application's destinations are fresh or outside the pins. *)
Definition DestsOk (pins made : list nat) (p : nat * list (nat * nat)) : Prop :=
  forall q, In q (snd p) -> (A.Fresh W (snd q) \/ ~ In (snd q) pins) /\ In (snd q) made.

Definition WrapPins (pins made : list nat) (w : expr -> expr) : Prop :=
  forall x p, In p (expr_applies (w x)) -> In p (expr_applies x) \/ DestsOk pins made p.

Lemma dests_ok_mono : forall pins m m' p, incl m m' -> DestsOk pins m p -> DestsOk pins m' p.
Proof. intros pins m m' p I O q Iq. destruct (O q Iq) as [A B]. exact (conj A (I _ B)). Qed.

Lemma wrap_pins_bind : forall pins b, bind_apply b = [] -> WrapPins pins [] (fun x => EBind b x).
Proof. intros pins b E x p I. cbn [expr_applies] in I. rewrite E in I. left. exact I. Qed.

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

Lemma wrap_pins_pre : forall pins (pre : expr -> expr) b, (forall x, expr_applies (pre x) = expr_applies x) ->
  bind_apply b = [] -> WrapPins pins [] (fun x => pre (EBind b x)).
Proof. intros pins pre b Pre E x p I. rewrite Pre in I. cbn [expr_applies] in I. rewrite E in I. left. exact I. Qed.

Fixpoint item_pins F C it w C' F' m (D : A.BindItem W Prog F C it w C' F' m) {struct D} :
  A.fr_pins F' = A.fr_pins F /\ WrapPins (A.fr_pins F) m w
with items_pins F C its w C' F' m (D : A.BindItems W Prog F C its w C' F' m) {struct D} :
  A.fr_pins F' = A.fr_pins F /\ WrapPins (A.fr_pins F) m w.
Proof.
  - destruct D.
    all: try (split; [reflexivity|apply wrap_pins_bind; reflexivity]).
    + split; [reflexivity|]. apply (wrap_pins_pre _ pre); [exact (lambda_expr_applies _ _ _ _ _ _ _ _ _ _ H2)|reflexivity].
    + split; [reflexivity|]. apply (wrap_pins_pre _ pre); [exact (meth_expr_applies _ _ _ _ _ _ _ _ _ H4)|reflexivity].
    + split; [reflexivity|]. exact (proj2 (items_pins _ _ _ _ _ _ _ H3)).
    + (* an abstract application *)
      split; [reflexivity|]. intros x p I. unfold apply_output_expression in I. cbn [expr_applies bind_apply app] in I.
      destruct I as [<-|I]; [right|left; exact I].
      intros q Iq. cbn [snd] in Iq. apply in_map_iff in Iq as [t [<- It]]. cbn [snd].
      split; [|apply in_map; exact It].
      match goal with
      | OI : forall t, In t outs <-> A.FunOutputs W u t,
        Os : forall t, A.FunOutputs W u t -> _ |- _ =>
          destruct (Os t (proj1 (OI t) It)) as [[_ [E [_ [_ Np]]]]|[_ [Fr _]]]; [right; rewrite E; exact Np|left; exact Fr]
      end.
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2].
    + split; [reflexivity|]. intros x p I. left. exact I.
    + destruct (item_pins _ _ _ _ _ _ _ D1) as [P1 W1]. destruct (items_pins _ _ _ _ _ _ _ D2) as [P2 W2].
      rewrite P1 in W2. split; [rewrite P2; exact P1|].
      intros x p I. cbv beta in I. destruct (W1 (w2 x) p I) as [I'|O].
      * destruct (W2 x p I') as [I''|O]; [left; exact I''|right].
        apply (dests_ok_mono _ m2); [intros z Iz; apply in_or_app; right; exact Iz|exact O].
      * right. apply (dests_ok_mono _ m1); [intros z Iz; apply in_or_app; left; exact Iz|exact O].
Qed.

Lemma bindsof_expr_applies : forall e l, A.BindsOf e l -> expr_applies e = flat_map bind_apply l.
Proof. intros e l B. induction B as [|b body rest B IH]; [reflexivity|]. cbn. rewrite IH. reflexivity. Qed.

Definition bind_dests (bs : list binding) : list nat :=
  flat_map (fun b => match b with BApply _ _ outs _ _ => map snd outs | _ => [] end) bs.

(* Every destination of the binds a run of items elaborates to is fresh
   or outside the frame's pins. *)
Theorem items_dests_pins : forall F C its wrap C' F' made binds,
  A.BindItems W Prog F C its wrap C' F' made -> A.BindsOf (wrap EUnit) binds ->
  forall y, In y (bind_dests binds) -> (A.Fresh W y \/ ~ In y (A.fr_pins F)) /\ In y made.
Proof.
  intros F C its wrap C' F' made binds D B y Iy.
  destruct (items_pins _ _ _ _ _ _ _ D) as [_ Wp].
  unfold bind_dests in Iy. apply in_flat_map in Iy as [b [Ib Iy]].
  destruct b as [| | | | | |u th outs sm rs|]; try contradiction.
  apply in_map_iff in Iy as [q [<- Iq]].
  assert (Ip : In (u, outs) (expr_applies (wrap EUnit))).
  { rewrite (bindsof_expr_applies _ _ B). apply in_flat_map. exists (BApply u th outs sm rs). split; [exact Ib|left; reflexivity]. }
  destruct (Wp EUnit _ Ip) as [[]|O]. exact (O q Iq).
Qed.

(* In a body bound to F (pins = M(F)), a destination is not a declared
   symbol of M(F). *)
Corollary link_dests_avoid : forall u Fg Cg its wrap Cb Fb made binds,
  (forall x, In x (A.fr_pins Fg) <-> A.Mentions W u x) ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  forall y, In y (bind_dests binds) -> A.Mentions W u y -> ~ A.Fresh W y -> False.
Proof.
  intros u Fg Cg its wrap Cb Fb made binds Pins D B y Iy My Nf.
  destruct (items_dests_pins _ _ _ _ _ _ _ _ D B y Iy) as [[Fr|Np] _]; [exact (Nf Fr)|exact (Np (proj2 (Pins y) My))].
Qed.

(* With LinkBody's clause that nothing the body makes is an input of F,
   no destination is. *)
Corollary link_dests_out : forall (Cu : A.cx) Fg Cg its wrap Cb Fb made binds,
  (forall t, In t made -> A.nlook t (A.c_ty Cu) = None) ->
  A.BindItems W Prog Fg Cg its wrap Cb Fb made -> A.BindsOf (wrap EUnit) binds ->
  forall y, In y (bind_dests binds) -> A.nlook y (A.c_ty Cu) = None.
Proof.
  intros Cu Fg Cg its wrap Cb Fb made binds Out D B y Iy.
  exact (Out y (proj2 (items_dests_pins _ _ _ _ _ _ _ _ D B y Iy))).
Qed.

End Pins.
