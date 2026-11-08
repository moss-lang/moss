(* ------------------------------------------------------------------------ *)
(* RelFacts.v — facts about the DRAFT elaboration relation (ExecutionModel.v, Part V) *)
(* that need no elaborator: D1, search-free method resolution.               *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel ComponentContracts.
Import ListNotations.
Import ExecutionModel.Elaboration.

(* An attached method is one symbol per (receiver, name). *)
Theorem attached_unique : forall b W r n a1 a2,
  WorldOf b W -> Attached W r n a1 -> Attached W r n a2 -> a1 = a2.
Proof. intros b W r n a1 a2 _ H1 H2. unfold Attached in *. congruence. Qed.

Lemma fold_set_ty_funs : forall th C,
  c_funs (fold_left (fun C q => set_ty C (fst q) (snd q)) th C) = c_funs C.
Proof. induction th as [|q r IH]; intro C; simpl; [reflexivity|]. rewrite IH. reflexivity. Qed.

(* The provision a call of method m at receiver type ot uses. *)
Lemma provided_choice : forall W P F C ox ot m args x1 t1 x2 t2,
  Provided W P F C ox ot m args x1 t1 -> Provided W P F C ox ot m args x2 t2 ->
  choice_of x1 = choice_of x2.
Proof.
  intros W P F C ox ot m args x1 t1 x2 t2 H1 H2.
  inversion H1; subst; inversion H2; subst; simpl; try reflexivity; try congruence.
  (* two families *)
  destruct (Nat.eq_dec u u0) as [->|Hne]; [reflexivity|].
  exfalso.
  match goal with
  | Hu : OnlyFamily W C u _ |- _ => apply (Hu u0 (not_eq_sym Hne)); do 4 eexists; eauto
  end.
Qed.

Ltac uniq_heads :=
  repeat match goal with
  | H1 : head ?t = Some ?h1, H2 : head ?t = Some ?h2 |- _ =>
      assert (h1 = h2) by congruence; subst h2; clear H2
  end.

Ltac uniq_att :=
  repeat match goal with
  | WO : WorldOf _ ?W, H1 : Attached ?W ?h ?n ?a1, H2 : Attached ?W ?h ?n ?a2 |- _ =>
      assert (a1 = a2) by (eapply attached_unique; [exact WO | exact H1 | exact H2]); subst a2; clear H2
  end.

Ltac uniq_lookups :=
  repeat match goal with
  | H1 : ?x = Some (ESym ?m1), H2 : ?x = Some (ESym ?m2) |- _ =>
      rewrite H1 in H2; injection H2 as ?E; subst m2
  | H1 : ?x = Some ?m1, H2 : ?x = Some ?m2 |- _ =>
      match type of m1 with nat => rewrite H1 in H2; injection H2 as ?E; subst m2 end
  end.

Lemma demand_choice_direct:forall W C0 wanted used C wrap C' f th sm xs,
 Demand W C0 wanted used C wrap C'->choice_of(wrap(XCallD f th sm xs))=Some(CDirect f).
Proof.
 intros W C0 wanted used C wrap C' f th sm xs H;induction H;[reflexivity|].
 cbn[choice_of];destruct(wrap(XCallD f th sm xs));cbn[choice_of]in IHDemand|-*;try exact IHDemand;discriminate IHDemand.
Qed.
Ltac choice_demands :=
 repeat match goal with
 | D:Demand _ _ _ _ _ ?wrap _ |-context[choice_of(?wrap(XCallD ?f ?th ?sm ?xs))]=>
 rewrite(demand_choice_direct _ _ _ _ _ _ _ f th sm xs D)
 end.

Theorem method_choice_unique : forall b W P F C ox ot pa args x1 t1 x2 t2,
  WorldOf b W ->
  MethCall W P F C ox ot pa args x1 t1 ->
  MethCall W P F C ox ot pa args x2 t2 ->
  choice_of x1 = choice_of x2.
Proof.
  intros b W P F C ox ot pa args x1 t1 x2 t2 WO H1 H2.
  inversion H1; subst; inversion H2; subst;
    try (match goal with H : Datatypes.length [_] > 1 |- _ => simpl in H; lia end);
    uniq_heads; uniq_att; uniq_lookups;
    choice_demands;
    try (eapply provided_choice; eassumption);
    try reflexivity;
    exfalso;
    try (match goal with
         | Hn : ~ (exists m, _ /\ _) |- _ => apply Hn; eexists; split; eassumption
         end);
    try (match goal with
         | Hn : forall h att, head ?t = Some h -> ~ Attached ?W ?h' ?n att |- _ => eapply Hn; eassumption
         end);
    try (match goal with
         | Ha : exists rn ps rt b, ast_of _ ?a = Some _, Hb : exists rn ps rt, ast_of _ ?a = Some _ |- _ =>
             destruct Ha as (? & ? & ? & ? & Ha); destruct Hb as (? & ? & ? & Hb); congruence
         end).
Qed.
