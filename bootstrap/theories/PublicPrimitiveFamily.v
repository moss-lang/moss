(* The language-side execution shape of a primitive-only family.  This
   proof does not need a platform's operation signatures or entry rules. *)
From Stdlib Require Import List.
From NM Require AuditBase.
From NM Require Import Audit.
From NM Require Import LegacyBodyJudgments.
Import Audit.Input AuditBase.Semantics ListNotations.
Module A := AuditBase.Semantics.Elaboration.

Definition IsPrim (b : binding) : Prop := exists key operation, b = BPrimitive key operation.
Definition PrimitiveFamilyCode (Prog : program) (code : usym) : Prop :=
  exists body, A.funbody_of Prog code = Some body /\ Forall IsPrim (fc_bindings body).

Section SourceBodies.
Variables (W : A.world) (Prog : program).

(* ---- no source item binds a primitive ---------------------------------------- *)

(* The run of binds an expression starts with binds no primitive. *)
Fixpoint NoPrim (e : expr) : Prop :=
  match e with
  | EBind b r => match b with BPrimitive _ _ => False | _ => True end /\ NoPrim r
  | _ => True
  end.

Lemma lambda_np : forall F C ps rt body sg this pid pre sm,
  LegacyBodyJudgments.LambdaProvider W Prog F C ps rt body sg this pid pre sm -> forall x, NoPrim x -> NoPrim (pre x).
Proof.
  intros F C ps rt body sg this pid pre sm LP. destruct LP. intros x Nx. unfold A.translate_lambda_captures. cbv beta.
  match goal with |- NoPrim (fold_right _ x ?l) => generalize l end.
  intros l. induction l as [|q l IH]; [exact Nx|].
  cbn [fold_right]. split; [exact Logic.I|exact IH].
Qed.

Lemma meth_np : forall F C y sg rt target types pre sm,
  LegacyBodyJudgments.MethProvider W Prog F C y sg rt target types pre sm -> forall x, NoPrim x -> NoPrim (pre x).
Proof.
  intros F C y sg rt target types pre sm MP. destruct MP; intros x Nx; try exact Nx.
  match goal with LP : LegacyBodyJudgments.LambdaProvider _ _ _ _ _ _ _ _ _ _ _ _ |- _ => exact (lambda_np _ _ _ _ _ _ _ _ _ _ LP x Nx) end.
Qed.

Fixpoint item_np F C it w C' F' m (D : LegacyBodyJudgments.BindItem W Prog F C it w C' F' m) {struct D} :
  forall x, NoPrim x -> NoPrim (w x)
with items_np F C its w C' F' m (D : LegacyBodyJudgments.BindItems W Prog F C its w C' F' m) {struct D} :
  forall x, NoPrim x -> NoPrim (w x).
Proof.
  - destruct D; intros x0 Nx.
    all: try (cbn; split; [exact Logic.I|exact Nx]).
    all: first
      [ match goal with LP : LegacyBodyJudgments.LambdaProvider _ _ _ _ _ _ _ _ _ _ _ _ |- _ =>
          apply (lambda_np _ _ _ _ _ _ _ _ _ _ LP); cbn; split; [exact Logic.I|exact Nx] end
      | match goal with MP : LegacyBodyJudgments.MethProvider _ _ _ _ _ _ _ _ _ _ _ |- _ =>
          apply (meth_np _ _ _ _ _ _ _ _ _ MP); cbn; split; [exact Logic.I|exact Nx] end
      | match goal with Di : LegacyBodyJudgments.BindItems _ _ _ _ _ _ _ _ _ |- _ => exact (items_np _ _ _ _ _ _ _ Di x0 Nx) end ].
  - destruct D as [F C|F C it r w1 C1 F1 m1 w2 C2 F2 m2 D1 D2]; intros x0 Nx; [exact Nx|].
    exact (item_np _ _ _ _ _ _ _ D1 _ (items_np _ _ _ _ _ _ _ D2 x0 Nx)).
Qed.

Lemma bindsof_np : forall e l, LegacyBodyJudgments.BindsOf e l -> NoPrim e -> forall k op, ~ In (BPrimitive k op) l.
Proof.
  intros e l B. induction B as [|b body rest B IH]; intros N k op I; [exact I|].
  destruct N as [Nb Nr]. destruct I as [->|I]; [exact Nb|exact (IH Nr k op I)].
Qed.

(* A nonempty primitive-only body cannot also be a linked source body. *)
Theorem primitive_code_not_linked : forall code target provider pins fc,
  PrimitiveFamilyCode Prog code -> LegacyBodyJudgments.LinkBody W Prog target provider code pins ->
  A.funbody_of Prog code = Some fc -> fc_bindings fc = [].
Proof.
  intros code target provider pins fc (actual & Body & Primitive) Linked View.
  rewrite Body in View. injection View as <-. destruct Linked.
  match goal with
  | Bo : LegacyBodyJudgments.BindsOf (?w EUnit) ?bindings, Bi : LegacyBodyJudgments.BindItems _ _ _ _ _ ?w _ _ _,
    Translation : LegacyBodyJudgments.TranslatesLinkedBody _ _ _ _ _ _ _ _ _ ?bindings _ |- _ =>
      destruct Translation as [_ [Fh _]];
      rewrite Body in Fh; injection Fh as Same; subst actual;
      pose proof (bindsof_np _ _ Bo (items_np _ _ _ _ _ _ _ Bi EUnit Logic.I)) as Excludes;
      cbn [A.translate_linked_body fc_bindings] in Primitive |- *;
      destruct bindings as [|binding tail]; [reflexivity|exfalso]
  end.
  inversion Primitive as [|binding' tail' Head Rest]; subst.
  destruct Head as [key [operation ->]]. apply (Excludes key operation). left. reflexivity.
Qed.
End SourceBodies.
