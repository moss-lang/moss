(* ======================================================================== *)
(* FrontEnd.v — Parse.v's lexer and parser against ExecutionModel.Syntax.Derives     *)
(* (Part VI's FRONT_END): soundness and completeness.                       *)
(* ======================================================================== *)

From Stdlib Require Import String Ascii List Bool Arith Lia.
From Stdlib Require Import Strings.Byte.
From NM Require Import ExecutionModel Parse.
Import ListNotations.
Import ExecutionModel.Input ExecutionModel.Syntax.
Local Open Scope string_scope.

(* ######################################################################## *)
(* Basics                                                                    *)
(* ######################################################################## *)

Lemma pbind_ok {A B} (r : pr A) (k : A -> list token -> pr B) v rest :
  pbind r k = POk v rest -> exists a ts', r = POk a ts' /\ k a ts' = POk v rest.
Proof. destruct r; simpl; [eauto | discriminate]. Qed.

(* Kinds: equality is decidable, by `token_kind_eqb`. *)
Lemma token_kind_index_inj a b : token_kind_index a = token_kind_index b -> a = b.
Proof.
  set (all :=
    [NAME; STRING; INTEGER; DECIMAL; AS; ASSUME; FUNCTOR; BIND; BREAK; CONTEXT;
     ELSE; FN; FOR; IF; IMPORT; LET; LOOP; MATCH; RETURN; TAG; THIS_TYPE; THIS;
     TYPE; USE; VAL; VAR; WHILE; IN; UNDERSCORE; EXCLAM; PERCENT; AMPERSAND;
     LPAREN; RPAREN; STAR; PLUS; COMMA; HYPHEN; DOT; SLASH; COLON; SEMI; LESS;
     EQUAL; GREATER; LBRACKET; RBRACKET; CARET; LBRACE; PIPE; RBRACE;
     EXCLAM_EQUAL; COLON_COLON; EQUAL_GREATER; LESS_LESS; LESS_EQUAL;
     EQUAL_EQUAL; GREATER_EQUAL; GREATER_GREATER; HYPHEN_GREATER; EOF]).
  assert (R : forall k, nth (token_kind_index k) all EOF = k) by (destruct k; reflexivity).
  intro H. rewrite <- (R a), <- (R b), H. reflexivity.
Qed.
Lemma token_kind_eqb_eq a b : token_kind_eqb a b = true <-> a = b.
Proof.
  unfold token_kind_eqb. rewrite Nat.eqb_eq. split; [apply token_kind_index_inj | intros ->; reflexivity].
Qed.
Lemma token_kind_eqb_refl a : token_kind_eqb a a = true.
Proof. apply token_kind_eqb_eq. reflexivity. Qed.
Lemma token_kind_eqb_neq a b : token_kind_eqb a b = false <-> a <> b.
Proof. rewrite <- token_kind_eqb_eq. destruct (token_kind_eqb a b); intuition discriminate. Qed.
Lemma token_kind_eqb_spec a b : reflect (a = b) (token_kind_eqb a b).
Proof. apply iff_reflect. symmetry. apply token_kind_eqb_eq. Qed.

Lemma at_true k ts : at_ k ts = true -> kind ts = k.
Proof. unfold at_. apply token_kind_eqb_eq. Qed.
Lemma at_false k ts : at_ k ts = false -> kind ts <> k.
Proof. unfold at_. intros H E. apply token_kind_eqb_neq in H. auto. Qed.
Lemma at_any_true ks ts : at_any ks ts = true -> In (kind ts) ks.
Proof.
  unfold at_any. intro H. apply existsb_exists in H as (k & Hin & E).
  apply token_kind_eqb_eq in E. subst. auto.
Qed.
Lemma at_any_false ks ts : at_any ks ts = false -> ~ In (kind ts) ks.
Proof.
  unfold at_any. intros H Hin. assert (existsb (token_kind_eqb (kind ts)) ks = true).
  { apply existsb_exists. exists (kind ts). split; auto. apply token_kind_eqb_refl. }
  congruence.
Qed.
Lemma mem_In s l : mem s l = true -> In s l.
Proof. unfold mem. intro H. apply existsb_exists in H as (k & Hin & E). apply String.eqb_eq in E. subst. auto. Qed.
Lemma In_mem s l : In s l -> mem s l = true.
Proof. unfold mem. intro H. apply existsb_exists. exists s. split; auto. apply String.eqb_refl. Qed.

Lemma mem_false s l : mem s l = false -> ~ In s l.
Proof. intros H Hin. apply In_mem in Hin. congruence. Qed.

Lemma Tk_intro k ts : kind ts = k -> Tk k ts (advance ts).
Proof. split; auto. Qed.

Lemma expect_ok k ts t ts' : expect k ts = POk t ts' -> Tk k ts ts' /\ t = peek ts.
Proof.
  unfold expect. destruct (at_ k ts) eqn:E; [|discriminate].
  intro H. injection H as <- <-. apply at_true in E. split; [split|]; auto.
Qed.

Lemma expect_name_ok ts x ts' : expect_name ts = POk x ts' -> TkName ts x ts'.
Proof.
  unfold expect_name. intro H. apply pbind_ok in H as (t & ts1 & H1 & H2).
  apply expect_ok in H1 as [[E1 E2] ->]. injection H2 as <- <-. repeat split; auto.
Qed.

Arguments at_ : simpl never.
Arguments at_any : simpl never.
Arguments expect : simpl never.
Arguments expect_name : simpl never.
Arguments eat : simpl never.
Arguments err : simpl never.
Arguments mixed : simpl never.
Arguments kind : simpl never.
Arguments kind_at : simpl never.
Arguments advance : simpl never.
Arguments peek : simpl never.
Arguments at_lambda : simpl never.
Arguments binop : simpl never.
Arguments binop_in : simpl never.
Arguments mem : simpl never.

(* Unpack a successful parser run. *)
Ltac crush :=
  repeat match goal with
  | H : context [eat ?k ?ts] |- _ =>
      let E := fresh "E" in unfold eat in H; destruct (at_ k ts) eqn:E; cbv beta iota zeta in H
  | H : match ?o with Some _ => _ | None => _ end = POk _ _ |- _ => let E := fresh "E" in destruct o eqn:E
  | H : match ?o with [] => _ | _ :: _ => _ end = POk _ _ |- _ => let E := fresh "E" in destruct o eqn:E
  | H : pbind _ _ = POk _ _ |- _ =>
      let a := fresh "a" in let t := fresh "ts" in let H1 := fresh "H" in
      apply pbind_ok in H; destruct H as (a & t & H1 & H)
  | H : POk _ _ = POk _ _ |- _ => injection H; clear H; intros; subst
  | H : PErr _ _ = POk _ _ |- _ => discriminate H
  | H : err _ _ = POk _ _ |- _ => discriminate H
  | H : fuel_err _ = POk _ _ |- _ => discriminate H
  | H : mixed _ _ = POk _ _ |- _ => discriminate H
  | H : expect _ _ = POk _ _ |- _ =>
      let H1 := fresh "Tk" in apply expect_ok in H; destruct H as [H1 H]; subst
  | H : expect_name _ = POk _ _ |- _ => apply expect_name_ok in H
  | H : at_ _ _ = true |- _ => apply at_true in H
  | H : at_ _ _ = false |- _ => apply at_false in H
  | H : at_any _ _ = true |- _ => apply at_any_true in H
  | H : at_any _ _ = false |- _ => apply at_any_false in H
  | H : mem _ _ = true |- _ => apply mem_In in H
  | H : mem _ _ = false |- _ => apply mem_false in H
  | H : (if ?b then _ else _) = POk _ _ |- _ => let E := fresh "E" in destruct b eqn:E
  | H : context [match ?p with (_, _) => _ end] |- _ => is_var p; destruct p; cbv beta iota in H
  | H : (_ && _)%bool = true |- _ => apply andb_prop in H; destruct H
  | H : (_ && _)%bool = false |- _ => apply andb_false_iff in H; destruct H
  | H : (_ || _)%bool = true |- _ => apply orb_prop in H; destruct H
  | H : (_ || _)%bool = false |- _ => apply orb_false_iff in H; destruct H
  | H : String.eqb _ _ = true |- _ => apply String.eqb_eq in H
  | H : String.eqb _ _ = false |- _ => apply String.eqb_neq in H
  | H : token_kind_eqb _ _ = true |- _ => apply token_kind_eqb_eq in H
  | H : token_kind_eqb _ _ = false |- _ => apply token_kind_eqb_neq in H
  | H : negb _ = true |- _ => apply negb_true_iff in H
  | H : negb _ = false |- _ => apply negb_false_iff in H
  end.

Global Hint Resolve Tk_intro : fe.
Global Hint Unfold Tk TkName : fe.

(* ######################################################################## *)
(* Soundness: a successful parse is a derivation                             *)
(* ######################################################################## *)

Lemma comma_list_sound {A} (X : list token -> A -> list token -> Prop) (item : list token -> pr A) closers :
  (forall ts v rest, item ts = POk v rest -> X ts v rest) ->
  forall n ts xs rest, comma_list n item closers ts = POk xs rest -> CommaList X closers ts xs rest.
Proof.
  intros Hi. induction n as [|n IH]; intros ts xs rest H; simpl in H; crush.
  all: first [ now apply CLNil | solve [eapply CLCons; eauto with fe] | eapply CLLast; eauto ].
Qed.

Lemma kind_at1 ts : kind_at 1 ts = kind (advance ts).
Proof. destruct ts as [|t [|t2 [|t3 r]]]; reflexivity. Qed.

Global Hint Constructors CommaList Path Head Spec_ App AppBinding Type_ Members TypeAtom RecordType
  TypeField OptType Pattern RecordPattern PatField : fe.
Global Hint Resolve or_introl or_intror conj : fe.

Lemma path_rest_sound : forall n acc ts p rest, path_rest n acc ts = POk p rest ->
  forall ts0 x acc', acc = x :: acc' -> TkName ts0 x ts -> exists q, p = (rev acc' ++ q)%list /\ Path ts0 q rest.
Proof.
  induction n as [|n IH]; intros acc ts p rest H ts0 x acc' -> Hx; simpl in H; crush.
  - edestruct IH as (q & -> & Hq); [eassumption | reflexivity | eassumption |].
    exists (x :: q). split; [simpl; rewrite <- app_assoc; reflexivity|].
    eapply PathMore; eauto with fe.
  - exists [x]. split; [unfold rev'; rewrite <- rev_alt; reflexivity|]. eapply PathOne; eauto.
Qed.

Lemma parse_path_sound n ts p rest : parse_path n ts = POk p rest -> Path ts p rest.
Proof.
  unfold parse_path. intro H. crush.
  edestruct path_rest_sound as (q & -> & Hq); [eassumption | reflexivity | eassumption | exact Hq].
Qed.

Lemma path_first ts p rest : Path ts p rest ->
  exists q, p = tx (peek ts) :: q /\ (q = [] -> kind (advance ts) <> COLON_COLON).
Proof.
  intro H. destruct H as [ts x ts1 [E1 [E2 E3]] Hk | ts x ts1 ts2 p ts3 [E1 [E2 E3]] [E4 E5] Hp].
  - exists []. subst. split; auto.
  - inversion Hp; subst; eexists; split; [reflexivity| | reflexivity|]; discriminate.
Qed.

Lemma spec_sound : forall n,
  (forall ts v rest, parse_spec n ts = POk v rest -> Spec_ ts v rest) /\
  (forall ts v rest, parse_app_opt n ts = POk v rest -> App ts v rest) /\
  (forall ts v rest, parse_binding n ts = POk v rest -> AppBinding ts v rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3)]; [repeat split; intros; discriminate|].
  repeat split; intros ts v rest H; simpl in H; crush;
    repeat match goal with H : parse_path _ _ = POk _ _ |- _ => apply parse_path_sound in H end;
    try match goal with H : parse_app_opt _ _ = POk _ _ |- _ => apply IH2 in H end;
    try match goal with H : parse_spec _ _ = POk _ _ |- _ => apply IH1 in H end;
    try match goal with H : comma_list _ _ _ _ = POk _ _ |- _ => apply (comma_list_sound AppBinding) in H; [|exact IH3] end;
    eauto 6 with fe.
Qed.

Lemma app_sound n ts v rest : parse_app_opt n ts = POk v rest -> App ts v rest.
Proof. apply spec_sound. Qed.
Lemma spec1_sound n ts v rest : parse_spec n ts = POk v rest -> Spec_ ts v rest.
Proof. apply spec_sound. Qed.

Lemma parse_type_ref_sound n ts t rest : parse_type_ref n ts = POk t rest -> TypeRef ts t rest.
Proof.
  unfold parse_type_ref, TypeRef. intro H. crush.
  apply parse_path_sound in H0. apply app_sound in H1. exists a, a0, ts0. auto.
Qed.

Lemma parse_assume_item_sound n ts it rest : parse_assume_item n ts = POk it rest -> AssumeItem ts it rest.
Proof.
  unfold parse_assume_item, eq_side_err. intro H. crush; apply parse_path_sound in H0.
  all: try solve [eapply (AssumeHead _ (_, Some _)); simpl; eauto with fe].
  all: try solve [eapply (AssumeHead _ (_, None)); simpl; eauto with fe].
  all: match goal with Ha : parse_app_opt _ ?t0 = POk ?ap ?t1, Hr : parse_type_ref _ ?t2 = POk _ _,
                       Hk : Tk EQUAL ?t1 ?t2, Hp : Path _ ?pp ?t0 |- _ =>
         apply parse_type_ref_sound in Hr; apply app_sound in Ha; apply AssumeEq;
         exists t1, t2; simpl; split; [exists pp, ap, t0; auto|]; split; [exact Hk|exact Hr] end.
Qed.

Lemma parse_ctx_item_sound n ts it rest : parse_ctx_item n ts = POk it rest -> CtxItem ts it rest.
Proof.
  unfold parse_ctx_item, eq_side_err. intro H. crush.
  all: try solve [apply CtxSpec; [eapply spec1_sound; eassumption|auto]].
  destruct a as [[p|] [d|] a]; crush.
  match goal with Hr : parse_type_ref _ _ = POk _ _ |- _ => apply parse_type_ref_sound in Hr; rename Hr into Hr0 end.
  apply spec1_sound in H0.
  inversion H0 as [? ? ? ? ? ? Hh Ha]; subst.
  inversion Hh as [? ? ? Hp Hk| |]; subst.
  apply CtxEq. exists ts0, (advance ts0). split; [eexists _, a, _; split; [exact Hp|split; [exact Ha|reflexivity]]|].
  split; [split; auto|exact Hr0].
Qed.

Ltac use_sound :=
  repeat match goal with
  | H : parse_path _ _ = POk _ _ |- _ => apply parse_path_sound in H
  | H : parse_app_opt _ _ = POk _ _ |- _ => apply app_sound in H
  | H : parse_spec _ _ = POk _ _ |- _ => apply spec1_sound in H
  | H : parse_assume_item _ _ = POk _ _ |- _ => apply parse_assume_item_sound in H
  | H : parse_ctx_item _ _ = POk _ _ |- _ => apply parse_ctx_item_sound in H
  end.

(* Apply the induction hypotheses (named IH1, IH2, ...) to every parser run. *)
Ltac feed :=
  repeat match goal with
  | H : _ = POk _ _ |- _ =>
      match goal with IH : context [POk] |- _ => apply IH in H end
  | H : exists _, _ |- _ => destruct H
  | H : _ /\ _ |- _ => destruct H
  end.

Ltac feed_lists :=
  repeat match goal with
  | H : comma_list _ _ _ _ = POk _ _ |- _ =>
      eapply comma_list_sound in H;
      [| let ts := fresh in let v := fresh in let r := fresh in let Hi := fresh in
         intros ts v r Hi;
         try (match goal with IH : context [POk] |- _ => apply IH in Hi end);
         exact Hi ]
  end.

Definition MembersAfter (ts : list token) (ms : list aty) (rest : list token) : Prop :=
  forall ts0 m, TypeAtom ts0 m ts -> Members ts0 (m :: ms) rest.

Lemma type_sound : forall n,
  (forall ts v rest, parse_type n ts = POk v rest -> Type_ ts v rest) /\
  (forall ts v rest, union_rest n ts = POk v rest -> MembersAfter ts v rest) /\
  (forall ts v rest, parse_type_atom n ts = POk v rest -> TypeAtom ts v rest) /\
  (forall ts v rest, parse_record_type n ts = POk v rest -> RecordType ts v rest) /\
  (forall ts v rest, type_field n ts = POk v rest -> TypeField ts v rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3 & IH4 & IH5)]; [repeat split; intros; discriminate|].
  repeat split.
  - intros ts v rest H. simpl in H. unfold eat, at_type_atom in H.
    destruct (at_ PIPE ts) eqn:Ep; simpl in H; crush; feed.
    + apply TyNever; auto with fe.
    + match goal with H : MembersAfter _ _ _, Ha : TypeAtom _ _ _ |- _ => apply H in Ha end.
      match goal with Hm : Members ?t (?m :: ?ms) _ |- _ => apply (TyMembers ts t (m :: ms)) end;
        unfold OptTk; auto with fe.
    + match goal with H : MembersAfter _ _ _, Ha : TypeAtom _ _ _ |- _ => apply H in Ha end.
      match goal with Hm : Members ?t (?m :: ?ms) _ |- _ => apply (TyMembers ts t (m :: ms)) end;
        unfold OptTk; auto with fe.
    + match goal with H : MembersAfter _ _ _, Ha : TypeAtom _ _ _ |- _ => apply H in Ha end.
      match goal with Hm : Members ?t (?m :: ?ms) _ |- _ => apply (TyMembers ts t (m :: ms)) end;
        unfold OptTk; auto with fe.
    + match goal with H : MembersAfter _ _ _, Ha : TypeAtom _ _ _ |- _ => apply H in Ha end.
      match goal with Hm : Members ?t (?m :: ?ms) _ |- _ => apply (TyMembers ts t (m :: ms)) end;
        unfold OptTk; auto with fe.
  - intros ts v rest H ts0 m Hm. simpl in H. crush; feed.
    all: unfold MembersAfter in *.
    + eapply MemMore; eauto with fe.
    + apply MemOne; auto.
  - intros ts v rest H. simpl in H. crush; use_sound; feed; eauto with fe.
  - intros ts v rest H. simpl in H. crush; feed_lists; eauto with fe.
  - intros ts v rest H. simpl in H. crush; feed; eauto with fe.
Qed.

Lemma type1_sound n ts v rest : parse_type n ts = POk v rest -> Type_ ts v rest.
Proof. apply type_sound. Qed.
Lemma record_type_sound n ts v rest : parse_record_type n ts = POk v rest -> RecordType ts v rest.
Proof. apply type_sound. Qed.

Ltac use_sound ::=
  repeat match goal with
  | H : parse_path _ _ = POk _ _ |- _ => apply parse_path_sound in H
  | H : parse_app_opt _ _ = POk _ _ |- _ => apply app_sound in H
  | H : parse_spec _ _ = POk _ _ |- _ => apply spec1_sound in H
  | H : parse_assume_item _ _ = POk _ _ |- _ => apply parse_assume_item_sound in H
  | H : parse_ctx_item _ _ = POk _ _ |- _ => apply parse_ctx_item_sound in H
  | H : parse_type _ _ = POk _ _ |- _ => apply type1_sound in H
  | H : parse_record_type _ _ = POk _ _ |- _ => apply record_type_sound in H
  end.

Lemma opt_type_sound n ts v rest : parse_opt_type n ts = POk v rest -> OptType ts v rest.
Proof.
  unfold parse_opt_type, eat. intro H. destruct (at_ COLON ts) eqn:E; crush; use_sound; eauto with fe.
Qed.

Definition RecPatAfter (p : option path) (ts : list token) (v : apat) (rest : list token) : Prop :=
  exists fs, v = APRec p fs /\ RecordPattern ts fs rest.

Lemma pattern_sound : forall n,
  (forall ts v rest, parse_pattern n ts = POk v rest -> Pattern ts v rest) /\
  (forall p ts v rest, parse_record_pattern n p ts = POk v rest -> RecPatAfter p ts v rest) /\
  (forall ts v rest, pat_field n ts = POk v rest -> PatField ts v rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3)]; [repeat split; intros; discriminate|].
  repeat split.
  - intros ts v rest H. simpl in H. crush; use_sound; feed; unfold RecPatAfter in *; feed; subst.
    all: try solve [eauto with fe].
    all: first
      [ apply PatWild; split; auto
      | apply PatPath; auto; simpl; intros [K|[K|[]]]; auto ].
  - intros p ts v rest H. simpl in H. crush; feed_lists; unfold RecPatAfter; eauto with fe.
  - intros ts v rest H. simpl in H. crush; feed; eauto with fe.
Qed.

Lemma pattern1_sound n ts v rest : parse_pattern n ts = POk v rest -> Pattern ts v rest.
Proof. apply pattern_sound. Qed.

Ltac use_sound ::=
  repeat match goal with
  | H : parse_path _ _ = POk _ _ |- _ => apply parse_path_sound in H
  | H : parse_app_opt _ _ = POk _ _ |- _ => apply app_sound in H
  | H : parse_spec _ _ = POk _ _ |- _ => apply spec1_sound in H
  | H : parse_assume_item _ _ = POk _ _ |- _ => apply parse_assume_item_sound in H
  | H : parse_ctx_item _ _ = POk _ _ |- _ => apply parse_ctx_item_sound in H
  | H : parse_type _ _ = POk _ _ |- _ => apply type1_sound in H
  | H : parse_record_type _ _ = POk _ _ |- _ => apply record_type_sound in H
  | H : parse_opt_type _ _ = POk _ _ |- _ => apply opt_type_sound in H
  | H : parse_pattern _ _ = POk _ _ |- _ => apply pattern1_sound in H
  end.
Global Hint Unfold OptTk : fe.

(* ===================== Expressions ======================================== *)

Global Hint Constructors LeftChain Expr Cond Arms ArmBody Operators Operand Additive Term Unary
  Suffixes Primary RecField Lambda Block Stmts Bind Binding : fe.

Lemma TkName_intro ts ts' x : Tk NAME ts ts' -> tx (peek ts) = x -> TkName ts x ts'.
Proof. intros [E1 E2] E3. repeat split; auto. Qed.
Lemma OptTk_eat k ts : OptTk k ts (snd (eat k ts)).
Proof.
  unfold OptTk, eat. destruct (at_ k ts) eqn:E; simpl.
  - left. apply at_true in E. split; auto.
  - right. apply at_false in E. auto.
Qed.
Lemma TkName_self ts : kind ts = NAME -> TkName ts (tx (peek ts)) (advance ts).
Proof. intro. repeat split; auto. Qed.
Global Hint Resolve TkName_intro OptTk_eat TkName_self : fe.

Lemma unary_op_some k op : unary_op k = Some op -> (k = HYPHEN /\ op = "-") \/ (k = EXCLAM /\ op = "!").
Proof. unfold unary_op. destruct k; intro H; inversion H; auto. Qed.
Lemma unary_op_none k : unary_op k = None -> k <> HYPHEN /\ k <> EXCLAM.
Proof. unfold unary_op. destruct k; intro H; try discriminate H; split; discriminate. Qed.

Lemma binop_in_false l ts : (forall o, binop ts = Some o -> ~ In o l) -> binop_in l ts = false.
Proof.
  unfold binop_in. destruct (binop ts) as [o|]; auto. intro H.
  destruct (mem o l) eqn:E; auto. apply mem_In in E. exfalso. apply (H o); auto.
Qed.
Ltac not_binop :=
  apply binop_in_false; intros ? ?;
  repeat match goal with H1 : binop ?t = Some _, H2 : binop ?t = Some _ |- _ =>
           rewrite H1 in H2; injection H2; clear H2; intro; subst end;
  first [ assumption | congruence | simpl; intuition congruence ].

Lemma binop_in_true l ts : binop_in l ts = true -> exists o, binop ts = Some o /\ In o l.
Proof. unfold binop_in. destruct (binop ts) as [o|]; [|discriminate]. intro H. apply mem_In in H. eauto. Qed.

Lemma binop_in_none l ts : binop ts = None -> binop_in l ts = false.
Proof. unfold binop_in. intros ->. reflexivity. Qed.
Lemma binop_in_some l ts op : binop ts = Some op -> binop_in l ts = mem op l.
Proof. unfold binop_in. intros ->. reflexivity. Qed.

(* What the parser's left-factored functions establish, given the operand
   they were handed. *)
Definition TermRest (nr : bool) (first : aexpr) (ts : list token) (e : aexpr) (rest : list token) : Prop :=
  exists e1 ts2, LeftChain (Unary nr) ["*"] first ts e1 ts2 /\
    ((binop_in FRACTION ts2 = false /\ e = e1 /\ rest = ts2) \/
     (exists op r, binop ts2 = Some op /\ In op FRACTION /\ Unary nr (advance ts2) r rest /\
                   binop_in ("*" :: FRACTION) rest = false /\ e = AEBinary op e1 r)).

Definition MulAfter nr e (last : option string) ts (v : aexpr * option string) rest : Prop :=
  LeftChain (Unary nr) ["*"] e ts (fst v) rest /\
  ((rest = ts /\ fst v = e /\ snd v = last) \/ snd v = Some "*").
Definition TermAfter nr first ts (v : aexpr * option string) rest : Prop :=
  TermRest nr first ts (fst v) rest /\ (snd v = None -> rest = ts /\ fst v = first).
Definition AddAfter nr e (last : option string) ts (v : aexpr * option string) rest : Prop :=
  LeftChain (Term nr) ADDITIVE e ts (fst v) rest /\
  ((rest = ts /\ fst v = e /\ snd v = last) \/ snd v <> None).
Definition OperandAfter nr first ts (v : aexpr * option string) rest : Prop :=
  (exists op r, binop ts = Some op /\ In op SHIFT /\ Unary nr (advance ts) r rest /\ fst v = AEBinary op first r) \/
  (exists t ts1, TermRest nr first ts t ts1 /\ LeftChain (Term nr) ADDITIVE t ts1 (fst v) rest /\
                 binop_in SHIFT rest = false).
Definition StmtsOk ts (v : list astmt * option aexpr) rest : Prop := Stmts ts (fst v) (snd v) rest.

Lemma term_of_rest nr ts u ts1 e rest : Unary nr ts u ts1 -> TermRest nr u ts1 e rest -> Term nr ts e rest.
Proof.
  intros Hu (e1 & ts2 & Hc & [(Hf & -> & ->) | (op & r & Ho & Hin & Hr & Hn & ->)]).
  - eapply TermProduct; eauto.
  - eapply TermQuotient; eauto.
Qed.

Lemma operand_of_after nr ts u ts1 v rest : Unary nr ts u ts1 -> OperandAfter nr u ts1 v rest ->
  Operand nr ts (fst v) rest.
Proof.
  intros Hu [(op & r & Ho & Hin & Hr & ->) | (t & ts2 & Ht & Hc & Hs)].
  - eapply OperandShift; eauto.
  - apply OperandAdditive; auto. eapply AdditiveG; [eapply term_of_rest|]; eauto.
Qed.

Lemma operand_unary nr ts u ts1 : Unary nr ts u ts1 -> binop ts1 = None -> Operand nr ts u ts1.
Proof.
  intros Hu Hb. apply OperandAdditive; [|apply binop_in_none; auto].
  eapply AdditiveG; [eapply TermProduct|]; eauto; try apply ChainEnd; apply binop_in_none; auto.
Qed.

Ltac grind := crush; use_sound; feed; feed_lists; subst;
  repeat match goal with
  | H : unary_op _ = Some _ |- _ => apply unary_op_some in H; destruct H as [[? ?]|[? ?]]; subst
  | H : binop ?ts = None |- context [binop_in _ ?ts] => rewrite (binop_in_none _ _ H)
  | H : binop ?ts = Some _ |- context [binop_in _ ?ts] => rewrite (binop_in_some _ _ _ H)
  end.

Ltac list_with X :=
  match goal with
  | H : comma_list _ _ _ _ = POk _ _ |- _ =>
      apply (comma_list_sound X) in H;
      [| let ts := fresh in let v := fresh in let r := fresh in let Hi := fresh in
         intros ts v r Hi; solve [crush; use_sound; feed; subst; eauto 6 with fe] ]
  end.

Global Hint Constructors BindForm BindFormField : fe.

Lemma bind_form_sound : forall n,
  (forall ts v rest, parse_bind_form n ts = POk v rest -> BindForm ts v rest) /\
  (forall ts v rest, parse_bind_field n ts = POk v rest -> BindFormField ts v rest).
Proof.
  induction n as [|n [Form Field]]; [split; intros; discriminate|].
  split; intros ts v rest H; simpl in H; crush.
  - list_with BindFormField. grind. eauto with fe.
  - grind. eauto with fe.
  - grind. eauto with fe.
  - grind. eauto with fe.
Qed.

Lemma bind_form1_sound n ts v rest : parse_bind_form n ts = POk v rest -> BindForm ts v rest.
Proof. apply bind_form_sound. Qed.
Local Hint Resolve bind_form1_sound : fe.

Ltac read_bind_form :=
  repeat match goal with
  | H : parse_bind_form ?n _ = POk _ _ |- _ => apply (proj1 (bind_form_sound n)) in H
  | H : negb _ = false |- _ => apply Bool.negb_false_iff in H
  end.

Lemma expr_sound : forall n,
  (forall nr ts v rest, parse_expr n nr ts = POk v rest -> Expr nr ts v rest) /\
  (forall ts v rest, parse_if n ts = POk v rest -> Cond ts v rest) /\
  (forall ts v rest, parse_arms n ts = POk v rest -> Arms ts v rest) /\
  (forall nr ts v rest, parse_operators n nr ts = POk v rest -> Operators nr ts v rest) /\
  (forall nr op e ts v rest, bit_loop n nr op e ts = POk v rest -> LeftChain (Unary nr) [op] e ts v rest) /\
  (forall nr first ts v rest, parse_operand n nr first ts = POk v rest -> OperandAfter nr first ts v rest) /\
  (forall nr e last ts v rest, add_loop n nr e last ts = POk v rest -> AddAfter nr e last ts v rest) /\
  (forall nr first ts v rest, parse_term n nr first ts = POk v rest -> TermAfter nr first ts v rest) /\
  (forall nr e last ts v rest, mul_loop n nr e last ts = POk v rest -> MulAfter nr e last ts v rest) /\
  (forall nr ts v rest, parse_unary n nr ts = POk v rest -> Unary nr ts v rest) /\
  (forall nr ts v rest, parse_postfix n nr ts = POk v rest -> Unary nr ts v rest) /\
  (forall e ts v rest, postfix_loop n e ts = POk v rest -> Suffixes e ts v rest) /\
  (forall ts v rest, parse_lambda n ts = POk v rest -> Lambda ts v rest) /\
  (forall nr ts v rest, parse_primary n nr ts = POk v rest -> Primary nr ts v rest) /\
  (forall ts v rest, parse_block n ts = POk v rest -> Block ts v rest) /\
  (forall ts v rest, block_loop n ts = POk v rest -> StmtsOk ts v rest) /\
  (forall ts v rest, parse_bind n ts = POk v rest -> Bind ts v rest).
Proof.
  induction n as [|n (IH1 & IH2 & IH3 & IH4 & IH5 & IH6 & IH7 & IH8 & IH9 & IH10 & IH11 & IH12 & IH13
                      & IH14 & IH15 & IH16 & IH17)];
    [repeat (match goal with |- _ /\ _ => split end); intros; discriminate|].
  repeat (match goal with |- _ /\ _ => split end).
  (* parse_expr *)
  - intros nr ts v rest H. simpl in H. grind; eauto 7 with fe.
  (* parse_if *)
  - intros ts v rest H. simpl in H. grind; eauto 7 with fe.
  (* parse_arms *)
  - intros ts v rest H. simpl in H. grind; eauto 7 with fe.
  (* parse_operators *)
  - intros nr ts v rest H. simpl in H. grind.
    all: repeat match goal with H : OperandAfter _ _ _ (_, _) _ |- _ =>
           eapply operand_of_after in H; [|eassumption]; simpl in H end.
    all: try first [ solve [eapply OpsBitwise; eauto] | solve [eapply OpsCompare; eauto]
               | solve [apply OpsOperand; auto; apply operand_unary; auto] ].
  (* bit_loop *)
  - intros nr op e ts v rest H. simpl in H. grind.
    all: first [ eapply ChainStep; eauto; left; auto | apply ChainEnd; not_binop ].
  (* parse_operand *)
  - intros nr first ts v rest H. simpl in H. grind.
    + apply binop_in_true in E as (o & Eo & Ho). rewrite Eo. left. eauto 6.
    + unfold TermAfter, AddAfter in *. simpl in *. right. intuition eauto.
    + unfold TermAfter, AddAfter in *. simpl in *. right.
      destruct H1 as [Hc [(-> & -> & <-) | Hn]]; [|congruence].
      destruct H0 as [Ht Hl]. destruct (Hl eq_refl) as [-> ->]. eauto.
  (* add_loop *)
  - intros nr e last ts v rest H. simpl in H. grind.
    + unfold AddAfter, TermAfter in *. destruct H as [Hc Hd]. destruct H1 as [Ht _]. split.
      * eapply ChainStep; eauto. eapply term_of_rest; eauto.
      * right. destruct Hd as [(_ & _ & ->) | Hd]; [discriminate | auto].
    + split; [apply ChainEnd; not_binop | left; auto].
    + split; [apply ChainEnd; apply binop_in_none; auto | left; auto].
  (* parse_term *)
  - intros nr first ts v rest H. simpl in H. grind.
    all: unfold TermAfter, MulAfter, TermRest in *; simpl in *.
    + split; [|discriminate]. destruct H0 as [Hc _]. do 2 eexists. split; [eauto|]. right. eauto 10.
    + destruct H0 as [Hc Hd]. split.
      * do 2 eexists. split; [eauto|]. left. split; [not_binop|auto].
      * intros ->. destruct Hd as [(-> & -> & _)|]; [auto|discriminate].
    + destruct H0 as [Hc Hd]. split.
      * do 2 eexists. split; [eauto|]. left. split; [apply binop_in_none|]; auto.
      * intros ->. destruct Hd as [(-> & -> & _)|]; [auto|discriminate].
  (* mul_loop *)
  - intros nr e last ts v rest H. simpl in H. grind.
    all: unfold MulAfter in *; simpl in *.
    + apply binop_in_true in E as (o & Eo & [<-|[]]). destruct H as [Hc Hd]. split.
      * eapply ChainStep; eauto. left; auto.
      * right. destruct Hd as [(_ & _ & ->)|]; auto.
    + split; [apply ChainEnd|left]; auto.
  (* parse_unary *)
  - intros nr ts v rest H. simpl in H. grind.
    all: try congruence.
    all: first [ assumption | solve [eapply UnaryNot; eauto with fe]
               | eapply UnaryNeg; eauto with fe; unfold neg_literal_dot; intuition ].
  (* parse_postfix *)
  - intros nr ts v rest H. simpl in H. grind. eauto with fe.
  (* postfix_loop *)
  - intros e ts v rest H. simpl in H. grind.
    all: eauto 6 with fe.
  (* parse_lambda *)
  - intros ts v rest H. simpl in H. crush. list_with BindForm. grind. read_bind_form. eauto 8 with fe.
  (* parse_primary *)
  - intros nr ts v rest H. simpl in H. crush; try list_with RecField; grind.
    all: try congruence.
    all: try solve [eauto 7 with fe].
  (* parse_block *)
  - intros ts v rest H. simpl in H. grind. unfold StmtsOk in *. simpl in *. eauto with fe.
  (* block_loop *)
  - intros ts v rest H. simpl in H. grind. read_bind_form.
    all: unfold StmtsOk in *; simpl in *.
    all: try (destruct (at_ VAR ts) eqn:?; crush).
    all: rewrite ?kind_at1 in *.
    all: try congruence.
    all: try solve [eauto 8 with fe].
    all: try solve [eapply StBraced; [unfold assign_ahead; rewrite ?kind_at1; intuition | assumption
                                     | eassumption | simpl; intuition | eassumption]].
    all: try solve [eapply StTail; eauto with fe; unfold assign_ahead; rewrite ?kind_at1; intuition].
    all: try solve [eapply StExpr; eauto with fe; unfold assign_ahead; rewrite ?kind_at1; intuition].
  (* parse_bind *)
  - intros ts v rest H. simpl in H. crush. list_with Binding. grind. eauto with fe.
Qed.

Definition expr1_sound n := proj1 (expr_sound n).
Lemma block_sound n ts v rest : parse_block n ts = POk v rest -> Block ts v rest.
Proof. apply expr_sound. Qed.
Lemma bind_sound n ts v rest : parse_bind n ts = POk v rest -> Bind ts v rest.
Proof. apply expr_sound. Qed.

(* ===================== Declarations and files ============================= *)

Global Hint Constructors Decl Decls Binds Function_ Use Import_ Imports DeclsToEnd : fe.

Ltac use_sound ::=
  repeat match goal with
  | H : parse_path _ _ = POk _ _ |- _ => apply parse_path_sound in H
  | H : parse_app_opt _ _ = POk _ _ |- _ => apply app_sound in H
  | H : parse_spec _ _ = POk _ _ |- _ => apply spec1_sound in H
  | H : parse_assume_item _ _ = POk _ _ |- _ => apply parse_assume_item_sound in H
  | H : parse_ctx_item _ _ = POk _ _ |- _ => apply parse_ctx_item_sound in H
  | H : parse_type _ _ = POk _ _ |- _ => apply type1_sound in H
  | H : parse_record_type _ _ = POk _ _ |- _ => apply record_type_sound in H
  | H : parse_opt_type _ _ = POk _ _ |- _ => apply opt_type_sound in H
  | H : parse_pattern _ _ = POk _ _ |- _ => apply pattern1_sound in H
  | H : parse_expr _ _ _ = POk _ _ |- _ => apply expr1_sound in H
  | H : parse_block _ _ = POk _ _ |- _ => apply block_sound in H
  | H : parse_bind _ _ = POk _ _ |- _ => apply bind_sound in H
  end.

Lemma name_list_sound n ts v rest : name_list n ts = POk v rest -> NameList ts v rest.
Proof.
  unfold name_list. intro H. crush. list_with TkName. unfold NameList. eauto 6 with fe.
Qed.

Lemma binds_loop_sound : forall n ts v rest, binds_loop n ts = POk v rest -> Binds ts v rest.
Proof.
  induction n as [|n IH1]; intros ts v rest H; simpl in H; grind; eauto with fe.
Qed.

Lemma decl_sound : forall n,
  (forall ts v rest, parse_decl n ts = POk v rest -> Decl ts v rest) /\
  (forall ts v rest, decls_until n ts = POk v rest -> Decls ts v rest).
Proof.
  induction n as [|n (IH1 & IH2)]; [split; intros; discriminate|].
  split; intros ts v rest H; simpl in H; crush; try list_with AssumeItem; try list_with CtxItem; try list_with BindForm;
    try list_with Spec_; try list_with Spec_; grind; read_bind_form;
    repeat match goal with
    | H : name_list _ _ = POk _ _ |- _ => apply name_list_sound in H
    | H : binds_loop _ _ = POk _ _ |- _ => apply binds_loop_sound in H
    end.
  all: try congruence.
  all: try match goal with H : match ?l with [] => _ | _ :: _ => _ end = _ |- _ => destruct l; crush end.
  all: try match goal with |- Decl _ (ADFn ?r ?x _ _ _) _ =>
         first [ solve [eapply (DFn _ _ (r, x)); eauto 7 with fe]
               | solve [eapply (DFnAbstract _ _ (r, x)); eauto 7 with fe] ] end.
  all: try solve [eauto 8 with fe].
  all: try discriminate.
  all: try match goal with H : _ = POk _ _, Ht : Type_ _ ?t _ |- _ => is_var t; destruct t; crush end.
  all: try solve [eapply DBareTag; eauto with fe; unfold TagParams; eauto with fe].
  all: try solve [eapply DTagRecord; eauto with fe; unfold TagParams; eauto with fe].
  all: try solve [eapply DTag; eauto with fe; [unfold TagParams; eauto with fe | intros ? ?; discriminate]].
Qed.

Lemma decl1_sound n ts v rest : parse_decl n ts = POk v rest -> Decl ts v rest.
Proof. apply decl_sound. Qed.

Lemma use_name_sound ts v rest : use_name ts = POk v rest -> UseName ts v rest.
Proof. unfold use_name, UseName. intro H. crush; simpl; eauto 6 with fe. Qed.

Lemma use_item_sound ts v rest : use_item ts = POk v rest -> Use ts v rest.
Proof.
  unfold use_item. intro H. crush;
    repeat match goal with H : use_name _ = POk _ _ |- _ => apply use_name_sound in H end;
    try congruence.
  - match goal with H : Bool.eqb _ _ = true |- _ => apply Bool.eqb_prop in H end. subst. eauto with fe.
  - eauto with fe.
Qed.

Lemma import_sound n ts v rest : parse_import n ts = POk v rest -> Import_ ts v rest.
Proof.
  unfold parse_import. intro H. crush;
    try (match goal with H : comma_list _ _ _ _ = POk _ _ |- _ =>
           apply (comma_list_sound Use) in H; [|exact use_item_sound] end).
  all: eapply ImportG; eauto with fe; unfold TkText; eauto 10 with fe.
  all: match goal with H : Tk STRING _ _ |- _ => destruct H end; repeat split; auto.
Qed.

Lemma imports_sound : forall n ts v rest, imports_loop n ts = POk v rest -> Imports ts v rest.
Proof.
  induction n as [|n IH1]; intros ts v rest H; simpl in H; crush;
    repeat match goal with H : parse_import _ _ = POk _ _ |- _ => apply import_sound in H end;
    feed; eauto with fe.
Qed.

Lemma decls_to_eof_sound : forall n ts v rest, decls_to_eof n ts = POk v rest -> DeclsToEnd ts v rest.
Proof.
  induction n as [|n IH1]; intros ts v rest H; simpl in H; crush;
    repeat match goal with H : parse_decl _ _ = POk _ _ |- _ => apply decl1_sound in H end;
    feed; eauto with fe.
Qed.

Theorem parse_tokens_sound ts f rest : parse_tokens ts = POk f rest -> File ts f.
Proof.
  unfold parse_tokens. intro H. crush.
  repeat match goal with
  | H : imports_loop _ _ = POk _ _ |- _ => apply imports_sound in H
  | H : decls_to_eof _ _ = POk _ _ |- _ => apply decls_to_eof_sound in H
  end.
  do 2 eexists. split; eassumption.
Qed.

(* ===================== The lexer ========================================== *)

Lemma span_spec p cs a b : span p cs = (a, b) ->
  cs = (a ++ b)%list /\ forallb p a = true /\ starts p b = false.
Proof.
  revert a b. induction cs as [|c r IH]; intros a b H; simpl in H.
  - injection H as <- <-. auto.
  - destruct (p c) eqn:E.
    + destruct (span p r) as [a' b'] eqn:Es. injection H as <- <-.
      destruct (IH _ _ eq_refl) as (-> & H1 & H2). simpl. rewrite E, H1. auto.
    + injection H as <- <-. simpl. auto.
Qed.

Lemma lex_string_other i j c r acc : c <> x0a -> c <> """"%byte -> c <> "\"%byte ->
  lex_string i j (c :: r) acc = lex_string i (S j) r (c :: acc).
Proof. intros H1 H2 H3. destruct c; try reflexivity; congruence. Qed.

Lemma lex_string_sound_n : forall n cs i j acc b r', length cs <= n ->
  lex_string i j cs acc = inl (b, r') ->
  exists body, cs = (body ++ """"%byte :: r')%list /\ b = (rev acc ++ body)%list /\ StrBody body.
Proof.
  induction n as [|n IH]; intros cs i j acc b r' Hl H; [destruct cs; [discriminate | simpl in Hl; lia]|].
  destruct cs as [|c r]; [discriminate|]. simpl in Hl.
  destruct (Byte.eqb c x0a) eqn:E1; [apply Byte.byte_dec_bl in E1; subst; discriminate|].
  destruct (Byte.eqb c """"%byte) eqn:E2.
  - apply Byte.byte_dec_bl in E2. subst. simpl in H. injection H as <- <-.
    exists []. unfold rev'. rewrite <- rev_alt, app_nil_r. repeat split; constructor.
  - destruct (Byte.eqb c "\"%byte) eqn:E3.
    + apply Byte.byte_dec_bl in E3. subst. simpl in H.
      destruct r as [|d r]; [discriminate|]. destruct (is_escape d) eqn:Ed; [|discriminate].
      simpl in Hl. destruct (IH r _ _ _ _ _ ltac:(lia) H) as (body & -> & -> & Hb).
      exists ("\"%byte :: d :: body). simpl. rewrite <- !app_assoc. repeat split; auto.
      apply SBEsc; auto.
    + rewrite lex_string_other in H; try (intro; subst; discriminate).
      destruct (IH r _ _ _ _ _ ltac:(lia) H) as (body & -> & -> & Hb).
      exists (c :: body). simpl. rewrite <- !app_assoc. repeat split; auto.
      apply SBByte; auto; intro; subst; discriminate.
Qed.

Lemma lex_string_sound cs i j acc b r' : lex_string i j cs acc = inl (b, r') ->
  exists body, cs = (body ++ """"%byte :: r')%list /\ b = (rev acc ++ body)%list /\ StrBody body.
Proof. apply lex_string_sound_n with (n := length cs). lia. Qed.

Lemma byte_dot_other {A} (c : byte) (X Y : A) : c <> "."%byte ->
  match c with "."%byte => X | _ => Y end = Y.
Proof. intro H. destruct c; congruence. Qed.

Lemma frac_spec r1 kd frac r2 :
  match r1 with
  | "."%byte :: d :: r' =>
      if is_digit d then let (ds2, r'') := span is_digit r' in (DECIMAL, "."%byte :: d :: ds2, r'')
      else (INTEGER, [], r1)
  | _ => (INTEGER, [], r1)
  end = (kd, frac, r2) ->
  (kd = INTEGER /\ frac = [] /\ r2 = r1 /\ forall c r, r1 = "."%byte :: c :: r -> is_digit c = false) \/
  (kd = DECIMAL /\ exists d ds2, frac = "."%byte :: d :: ds2 /\ forallb is_digit (d :: ds2) = true /\
                       r1 = (frac ++ r2)%list /\ starts is_digit r2 = false).
Proof.
  intro H. destruct r1 as [|c1 r1].
  - injection H as <- <- <-. left. repeat split; auto. discriminate.
  - destruct (Byte.byte_eq_dec c1 "."%byte) as [->|Hc].
    + destruct r1 as [|d r'].
      * injection H as <- <- <-. left. repeat split; auto. discriminate.
      * destruct (is_digit d) eqn:Ed.
        -- destruct (span is_digit r') as [ds2 r''] eqn:Es. apply span_spec in Es as (-> & H1 & H2).
           injection H as <- <- <-. right. split; auto. exists d, ds2. simpl. rewrite Ed, H1. auto.
        -- injection H as <- <- <-. left. repeat split; auto. intros c r E. injection E as -> ->. auto.
    + rewrite (byte_dot_other c1) in H by exact Hc. injection H as <- <- <-. left.
      repeat split; auto. intros c r E. injection E as E. congruence.
Qed.

Lemma span_digits_first d r eds r4 : is_digit d = true -> span is_digit (d :: r) = (eds, r4) ->
  exists ds', eds = d :: ds'.
Proof. intros Hd H. simpl in H. rewrite Hd in H. destruct (span is_digit r). injection H as <- _. eauto. Qed.

Lemma lex_number_sound c r i t r' j : is_digit c = true -> lex_number c r i = inl (t, r', j) ->
  exists cs k, (c :: r) = (cs ++ r')%list /\ Lexeme cs k r' /\ t = Tok k (string_of_list_byte cs) i /\
               j = i + length cs.
Proof.
  intros Hc H. unfold lex_number in H.
  destruct (span is_digit r) as [ds r1] eqn:Hs. apply span_spec in Hs as (-> & Hds & _).
  match type of H with context [match ?m with (_, _) => _ end] =>
    destruct m as [[kd frac] r2] eqn:Hf end.
  apply frac_spec in Hf.
  assert (Hfr : Fraction frac /\ (ds ++ r1 = ds ++ frac ++ r2)%list /\
                (frac = [] -> forall c r, r2 = "."%byte :: c :: r -> is_digit c = false) /\
                kd = match frac with [] => INTEGER | _ => DECIMAL end).
  { destruct Hf as [(-> & -> & -> & Hn) | (-> & d & ds2 & -> & Hd & -> & _)].
    - repeat split; auto. left; auto.
    - repeat split; auto. right; eauto. discriminate. }
  clear Hf. destruct Hfr as (Hfrac & Hr1 & Hdot & Hkd). rewrite Hr1. clear Hr1 r1.
  (* the exponent *)
  assert (Hnone : forall r', starts is_name_cont r' = false ->
            (forall c0 r0, frac = [] -> r' = "."%byte :: c0 :: r0 -> is_digit c0 = false) ->
            exists cs k, (c :: ds ++ frac ++ r')%list = (cs ++ r')%list /\ Lexeme cs k r' /\
              Tok kd (string_of_list_byte ((c :: ds) ++ frac)) i = Tok k (string_of_list_byte cs) i /\
              i + length (c :: ds) + length frac = i + length cs).
  { intros r'' Hn Hd. exists (c :: ds ++ frac ++ [])%list, kd.
    rewrite !app_nil_r. split; [simpl; rewrite app_assoc; reflexivity|]. split.
    - replace kd with (match frac, @nil byte with [], [] => INTEGER | _, _ => DECIMAL end)
        by (subst kd; destruct frac; reflexivity).
      replace (c :: ds ++ frac)%list with (c :: ds ++ frac ++ [])%list by (rewrite app_nil_r; reflexivity).
      apply LxNumber; auto. simpl; rewrite Hc; auto. left; auto. intros Hf0 _. eauto.
    - simpl. rewrite app_length. split; [reflexivity | lia]. }
  assert (Hsome : forall ex r4, Exponent ex -> ex <> [] -> starts is_name_cont r4 = false ->
            exists cs k, (c :: ds ++ frac ++ ex ++ r4)%list = (cs ++ r4)%list /\ Lexeme cs k r4 /\
              Tok DECIMAL (string_of_list_byte (((c :: ds) ++ frac) ++ ex)) i = Tok k (string_of_list_byte cs) i /\
              i + length (c :: ds) + length frac + length ex = i + length cs).
  { intros ex r4 Hex Hne Hn. exists (c :: ds ++ frac ++ ex)%list, DECIMAL.
    split; [simpl; rewrite !app_assoc; reflexivity|]. split.
    - assert (L : Lexeme (c :: ds ++ frac ++ ex) (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end) r4).
      { apply LxNumber; auto. simpl; rewrite Hc; auto. intros _ E. contradiction. }
      replace (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end) with DECIMAL in L
        by (destruct frac, ex; congruence). exact L.
    - simpl. rewrite <- !app_assoc, !app_length. split; [reflexivity | lia]. }
  destruct r2 as [|e [|s r3]].
  - injection H as <- <- <-.
    destruct (Hnone [] eq_refl ltac:(intros; discriminate)) as (cs & k & F1 & F2 & F3 & F4).
    exists cs, k. auto.
  - destruct (is_name_cont e) eqn:En; [discriminate|]. injection H as <- <- <-.
    destruct (Hnone [e] ltac:(simpl; auto) ltac:(intros; discriminate)) as (cs & k & F1 & F2 & F3 & F4).
    exists cs, k. auto.
  - destruct (is_e e && is_sign s) eqn:E1.
    + destruct r3 as [|d r3]; [discriminate|]. destruct (is_digit d) eqn:Ed; [|discriminate].
      destruct (span is_digit (d :: r3)) as [eds r4] eqn:Hs.
      destruct (span_digits_first _ _ _ _ Ed Hs) as [ds' ->].
      apply span_spec in Hs as (Hr3 & Hdg & _).
      destruct (MAX_EXPONENT_DIGITS <? length (strip_zeros (d :: ds')))%nat eqn:Em; [discriminate|].
      apply andb_prop in E1 as [Ee Es].
      assert (Hex : Exponent ([e; s] ++ d :: ds')%list).
      { right. exists e, [s], d, ds'. split; [reflexivity|]. repeat split; auto. right; eauto.
        apply Nat.ltb_ge in Em. unfold MAX_EXPONENT_DIGITS in Em. lia. }
      destruct r4 as [|b r4].
      * injection H as <- <- <-.
        edestruct (Hsome _ [] Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | reflexivity |].
        exists cs, k. rewrite Hr3, <- F1. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
      * destruct (is_name_cont b) eqn:Eb; [discriminate|]. injection H as <- <- <-.
        edestruct (Hsome _ (b :: r4) Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | simpl; auto |].
        exists cs, k. rewrite Hr3, <- F1. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
    + destruct (is_e e && is_digit s) eqn:E2.
      * destruct (span is_digit (s :: r3)) as [eds r4] eqn:Hs.
        apply andb_prop in E2 as [Ee Es].
        destruct (span_digits_first _ _ _ _ Es Hs) as [ds' ->].
        apply span_spec in Hs as (Hr3 & Hdg & _).
        destruct (MAX_EXPONENT_DIGITS <? length (strip_zeros (s :: ds')))%nat eqn:Em; [discriminate|].
        assert (Hex : Exponent ([e] ++ s :: ds')%list).
        { right. exists e, [], s, ds'. split; [reflexivity|]. repeat split; auto.
          apply Nat.ltb_ge in Em. unfold MAX_EXPONENT_DIGITS in Em. lia. }
        destruct r4 as [|b r4].
        -- injection H as <- <- <-.
           edestruct (Hsome _ [] Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | reflexivity |].
           exists cs, k. rewrite <- F1. rewrite Hr3. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
        -- destruct (is_name_cont b) eqn:Eb; [discriminate|]. injection H as <- <- <-.
           edestruct (Hsome _ (b :: r4) Hex) as (cs & k & F1 & F2 & F3 & F4); [discriminate | simpl; auto |].
           exists cs, k. rewrite <- F1, Hr3. simpl length in *. repeat split; auto; try lia; simpl; rewrite <- ?app_assoc; auto.
      * destruct (is_name_cont e) eqn:En; [discriminate|]. injection H as <- <- <-.
        destruct (Hnone (e :: s :: r3) ltac:(simpl; auto)) as (cs & k & F1 & F2 & F3 & F4).
        { intros c0 r0 Hf0 E. injection E as -> -> ->. eapply Hdot; eauto. }
        exists cs, k. auto.
Qed.

Lemma lex_tok i cs k r ts all j : Lexeme cs k r -> Lexes j r ts -> all = (cs ++ r)%list ->
  j = i + length cs -> Lexes i all (Tok k (string_of_list_byte cs) i :: ts).
Proof. intros H Hl -> ->. apply LexToken; auto. Qed.

Definition LexStep (i : nat) (cs : list byte) (acc : list token) (r' : list byte) (i' : nat)
  (acc' : list token) : Prop :=
  (acc' = acc /\ forall T, Lexes i' r' T -> Lexes i cs T) \/
  (exists t, acc' = t :: acc /\ forall T, Lexes i' r' T -> Lexes i cs (t :: T)).

Lemma lex_char_sound c r i acc r' i' acc' : lex_char c r i acc = inl (r', i', acc') ->
  LexStep i (c :: r) acc r' i' acc'.
Proof.
  unfold lex_char, LexStep. intro H.
  destruct (is_ws c) eqn:Ews.
  { injection H as <- <- <-. left. split; auto. intros. apply LexSpace; auto. }
  destruct (Byte.eqb c "#"%byte) eqn:Eh.
  { apply Byte.byte_dec_bl in Eh. subst.
    destruct (span (fun b => negb (Byte.eqb b x0a)) r) as [com rest] eqn:Es.
    apply span_spec in Es as (-> & H1 & H2). injection H as <- <- <-. left. split; auto.
    intros. apply LexComment; auto. }
  destruct r as [|d r0] eqn:Er.
  2: destruct (two_char c d) as [k|] eqn:E2.
  2: { injection H as <- <- <-. right. eexists. split; [reflexivity|]. intros T HT.
       eapply lex_tok; [apply LxSym2; eauto | exact HT | reflexivity | simpl; lia]. }
  all: destruct (one_char c) as [k|] eqn:E1.
  1, 3: injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT;
        (eapply lex_tok; [apply LxSym1; [exact E1|] | exact HT | reflexivity | simpl; lia]);
        intros b r1 Eb; first [discriminate | injection Eb as <- <-; auto].
  all: destruct (is_digit c) eqn:Ed.
  1, 3: match type of H with context [lex_number ?cc ?x ?ii] =>
          destruct (lex_number cc x ii) as [[[t r1] j]|[m o]] eqn:En; [|discriminate] end;
        injection H as <- <- <-; apply lex_number_sound in En as (cs & k & Ec & Hl & -> & ->); auto;
        right; eexists; split; [reflexivity|]; intros T HT; eapply lex_tok; eauto.
  all: destruct (is_name_start c) eqn:Ens.
  1, 3: match type of H with context [span is_name_cont ?x] =>
          destruct (span is_name_cont x) as [ns r1] eqn:Es end;
        apply span_spec in Es as (Es & H1 & H2);
        injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT;
        (eapply lex_tok; [apply LxWord; eauto | exact HT | simpl; rewrite Es; reflexivity | simpl; lia]).
  all: destruct (Byte.eqb c """"%byte) eqn:Eq; [|discriminate].
  all: apply Byte.byte_dec_bl in Eq; subst.
  all: match type of H with context [lex_string ?ii (S ?ii) ?x []] =>
         destruct (lex_string ii (S ii) x []) as [[body r1]|[m o]] eqn:Es; [|discriminate] end.
  all: apply lex_string_sound in Es as (body' & Es & Eb & Hb); simpl in Eb; subst body'.
  all: try (destruct body; discriminate).
  all: injection H as <- <- <-; right; eexists; split; [reflexivity|]; intros T HT.
  all: eapply lex_tok; [apply LxString; eauto | exact HT | rewrite Es; simpl; rewrite <- app_assoc; reflexivity
                       | simpl; rewrite app_length; simpl; lia].
Qed.

Lemma lex_go_sound : forall fuel cs i acc ts, lex_go fuel cs i acc = LOk ts ->
  exists ts', ts = (rev acc ++ ts')%list /\ Lexes i cs ts'.
Proof.
  induction fuel as [|fuel IH]; intros cs i acc ts H; simpl in H; [discriminate|].
  destruct cs as [|c r].
  - injection H as <-. exists [Tok EOF "" i]. unfold rev'. rewrite <- rev_alt. split; [reflexivity|constructor].
  - destruct (lex_char c r i acc) as [[[r' i'] acc']|[m o]] eqn:E; [|discriminate].
    apply lex_char_sound in E. apply IH in H as (ts' & -> & Hl).
    destruct E as [(-> & Hs) | (t & -> & Hs)].
    + exists ts'. auto.
    + exists (t :: ts'). simpl. rewrite <- app_assoc. auto.
Qed.

Theorem lex_sound cs ts : lex cs = LOk ts -> Lexes 0 cs ts.
Proof. unfold lex. intro H. apply lex_go_sound in H as (ts' & -> & H). exact H. Qed.

(* Part VI's parser_sound. *)
Theorem parse_moss_sound s f : parse_moss s = Good f -> Derives (list_byte_of_string s) f.
Proof.
  unfold parse_moss. intro H.
  destruct (lex (map fst (raw_source (newlines (list_byte_of_string s))))) as [ts|m o] eqn:El; [|discriminate].
  destruct (parse_tokens ts) as [f' rest|m o] eqn:Ep; [|discriminate].
  injection H as <-. exists ts. split; [apply lex_sound; auto | eapply parse_tokens_sound; eauto].
Qed.

(* ######################################################################## *)
(* Completeness: a derivation is what the parser finds                       *)
(* ######################################################################## *)

(* ===================== The lexer ========================================== *)

Lemma span_app p a b : forallb p a = true -> starts p b = false -> span p (a ++ b)%list = (a, b).
Proof.
  induction a as [|c a IH]; intros Ha Hb; simpl in *.
  - destruct b as [|d b]; simpl in *; [reflexivity|]. rewrite Hb. reflexivity.
  - apply andb_prop in Ha as [H1 H2]. rewrite H1, IH; auto.
Qed.

Lemma split_unique p a b a' b' : forallb p a = true -> forallb p a' = true ->
  starts p b = false -> starts p b' = false -> (a ++ b = a' ++ b')%list -> a = a' /\ b = b'.
Proof.
  intros Ha Ha' Hb Hb' E.
  assert (E1 := span_app p a b Ha Hb). assert (E2 := span_app p a' b' Ha' Hb').
  rewrite E in E1. rewrite E1 in E2. injection E2 as -> ->. auto.
Qed.

Lemma lex_string_complete i j body r acc : StrBody body ->
  lex_string i j (body ++ """"%byte :: r)%list acc = inl ((rev acc ++ body)%list, r).
Proof.
  intro H. revert i j acc. induction H as [|c r0 H1 H2 H3 Hb IH|d r0 Hd Hb IH]; intros i j acc.
  - simpl. unfold rev'. rewrite <- rev_alt, app_nil_r. reflexivity.
  - simpl app. rewrite lex_string_other by auto. rewrite IH. simpl. rewrite <- app_assoc. reflexivity.
  - simpl. rewrite Hd, IH. simpl. rewrite <- !app_assoc. reflexivity.
Qed.

(* Facts about the first byte of each kind of token. *)
Lemma two_char_first a b k : two_char a b = Some k ->
  is_ws a = false /\ a <> "#"%byte /\ is_digit a = false /\ is_name_start a = false /\ a <> """"%byte.
Proof. destruct a; simpl; try discriminate; intros; repeat split; discriminate. Qed.

Lemma one_char_first a k : one_char a = Some k ->
  is_ws a = false /\ a <> "#"%byte /\ is_digit a = false /\ is_name_start a = false /\ a <> """"%byte.
Proof. destruct a; simpl; try discriminate; intros; repeat split; discriminate. Qed.

Lemma name_first c : is_name_start c = true ->
  is_ws c = false /\ c <> "#"%byte /\ (forall b, two_char c b = None) /\ one_char c = None /\
  is_digit c = false /\ c <> """"%byte.
Proof. destruct c; simpl; try discriminate; intros; repeat split; try discriminate; reflexivity. Qed.

Lemma digit_first c : is_digit c = true ->
  is_ws c = false /\ c <> "#"%byte /\ (forall b, two_char c b = None) /\ one_char c = None /\
  is_name_cont c = true.
Proof. destruct c; simpl; try discriminate; intros; repeat split; try discriminate; reflexivity. Qed.

Lemma e_name_cont e : is_e e = true -> is_name_cont e = true /\ is_digit e = false /\ e <> "."%byte.
Proof. destruct e; simpl; try discriminate; intros; repeat split; discriminate. Qed.
Lemma sign_not_digit s : is_sign s = true -> is_digit s = false.
Proof. destruct s; simpl; try discriminate; reflexivity. Qed.
Lemma digit_name_cont d : is_digit d = true -> is_name_cont d = true.
Proof. destruct d; simpl; try discriminate; reflexivity. Qed.
Lemma starts_digit_of_name_cont r : starts is_name_cont r = false -> starts is_digit r = false.
Proof. destruct r as [|d r]; simpl; auto. destruct (is_digit d) eqn:E; auto. rewrite digit_name_cont; auto. Qed.

Lemma frac_ex_not_digit frac ex rest : Fraction frac -> Exponent ex -> starts is_name_cont rest = false ->
  starts is_digit (frac ++ ex ++ rest)%list = false.
Proof.
  intros [->|(d & ds & -> & _)] [->|(e & sign & d' & ds' & -> & He & _)] Hr; simpl; auto.
  - apply starts_digit_of_name_cont; auto.
  - apply e_name_cont in He. apply He.
Qed.

Ltac fin_num frac :=
  match goal with |- inl (Tok ?k1 ?t1 _, _, ?n1) = inl (Tok ?k2 ?t2 _, _, ?n2) =>
    replace n1 with n2 by (simpl; rewrite ?app_length; simpl; rewrite ?app_length; simpl; lia);
    replace t1 with t2 by (f_equal; simpl; rewrite <- ?app_assoc, ?app_nil_r; reflexivity) end;
  first [reflexivity | destruct frac; reflexivity].

Lemma lex_number_complete d ds frac ex rest i :
  forallb is_digit (d :: ds) = true -> Fraction frac -> Exponent ex -> starts is_name_cont rest = false ->
  (frac = [] -> ex = [] -> forall c r, rest = "."%byte :: c :: r -> is_digit c = false) ->
  lex_number d (ds ++ frac ++ ex ++ rest)%list i =
    inl (Tok (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end)
             (string_of_list_byte (d :: ds ++ frac ++ ex)%list) i, rest,
         i + length (d :: ds ++ frac ++ ex)%list).
Proof.
  intros Hd Hf He Hr Hdot. simpl in Hd. apply andb_prop in Hd as [Hd Hds].
  unfold lex_number.
  rewrite span_app by (auto; apply frac_ex_not_digit; auto).
  match goal with |- context [match ?m with (_, _) => _ end] => destruct m as [[kd frac'] r2] eqn:Hm end.
  apply frac_spec in Hm.
  assert (Hfr : kd = match frac with [] => INTEGER | _ => DECIMAL end /\ frac' = frac /\ r2 = (ex ++ rest)%list).
  { destruct Hf as [->|(d1 & ds1 & -> & Hd1)]; destruct Hm as [(-> & -> & -> & Hn) | (-> & d2 & ds2 & -> & Hd2 & E & Hs)].
    - auto.
    - exfalso. simpl in E. destruct He as [->|(e & sign & d3 & ds3 & -> & Hee & _)].
      + simpl in E. subst rest. simpl in Hd2. apply andb_prop in Hd2 as [Hd2 _].
        rewrite (Hdot eq_refl eq_refl d2 _ eq_refl) in Hd2. discriminate.
      + injection E as E. subst e. discriminate.
    - exfalso. simpl in Hd1. apply andb_prop in Hd1 as [Hd1 _]. rewrite (Hn d1 _ eq_refl) in Hd1. discriminate.
    - injection E as E. simpl in E.
      assert (Hsplit := split_unique is_digit (d1 :: ds1) (ex ++ rest) (d2 :: ds2) r2 Hd1 Hd2
                          (frac_ex_not_digit [] ex rest (or_introl eq_refl) He Hr) Hs).
      destruct Hsplit as [E1 E2]; [subst; simpl; rewrite H; reflexivity|]. injection E1 as -> ->. auto. }
  destruct Hfr as (-> & -> & ->). clear Hm.
  destruct He as [->|(e & sign & d3 & ds3 & -> & Hee & Hsign & Hd3 & Hlen)].
  - (* no exponent *)
    simpl app. destruct rest as [|b r'].
    + simpl. fin_num frac.
    + destruct r' as [|b2 r'].
      * simpl in Hr. simpl. rewrite Hr. fin_num frac.
      * assert (Hb : is_e b = false).
        { destruct (is_e b) eqn:E; auto. apply e_name_cont in E. simpl in Hr. destruct E as [E _]. congruence. }
        simpl in Hr. simpl. rewrite Hb. simpl. rewrite Hr. fin_num frac.
  - (* an exponent *)
    destruct Hsign as [-> | (s & -> & Hs)].
    + simpl app.
      assert (Hns : is_sign d3 = false).
      { simpl in Hd3. apply andb_prop in Hd3 as [Hd3 _]. destruct d3; simpl in *; try discriminate; reflexivity. }
      simpl in Hd3. apply andb_prop in Hd3 as [Hd3 Hds3].
      cbn [app]. rewrite Hee, Hns, Hd3. simpl.
      rewrite Hd3, span_app by (auto; apply starts_digit_of_name_cont; auto).
      assert (Hl : (MAX_EXPONENT_DIGITS <? length (strip_zeros (d3 :: ds3)))%nat = false)
        by (apply Nat.ltb_ge; unfold MAX_EXPONENT_DIGITS; lia).
      cbv zeta. rewrite Hl.
      destruct rest as [|b r']; simpl in Hr |- *; [|rewrite Hr]; fin_num frac.
    + simpl in Hd3. apply andb_prop in Hd3 as [Hd3 Hds3].
      cbn [app]. rewrite Hee, Hs. simpl. rewrite Hd3.
      change (d3 :: ds3 ++ rest)%list with ((d3 :: ds3) ++ rest)%list.
      rewrite span_app by (simpl; rewrite ?Hd3; auto; apply starts_digit_of_name_cont; auto).
      assert (Hl : (MAX_EXPONENT_DIGITS <? length (strip_zeros (d3 :: ds3)))%nat = false)
        by (apply Nat.ltb_ge; unfold MAX_EXPONENT_DIGITS; lia).
      cbv zeta. rewrite Hl.
      destruct rest as [|b r']; simpl in Hr |- *; [|rewrite Hr]; fin_num frac.
Qed.

Ltac fin_off := match goal with |- inl (_, ?n1, _) = inl (_, ?n2, _) =>
  replace n2 with n1 by (simpl; rewrite ?app_length; simpl; lia) end; reflexivity.

Lemma lexeme_char cs k r i acc : Lexeme cs k r ->
  exists c rest0, (cs ++ r)%list = c :: rest0 /\
    lex_char c rest0 i acc = inl (r, i + length cs, Tok k (string_of_list_byte cs) i :: acc).
Proof.
  intro H. destruct H as [a b k rest Hk | a k rest Hk Hn | c ns rest Hc Hns Hr | body rest Hb
                         | d ds frac ex rest Hd Hf He Hr Hdot].
  - exists a, (b :: rest). split; [reflexivity|].
    destruct (two_char_first _ _ _ Hk) as (H1 & H2 & _). unfold lex_char. rewrite H1.
    destruct (Byte.eqb a "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    rewrite Hk. fin_off.
  - exists a, rest. split; [reflexivity|].
    destruct (one_char_first _ _ Hk) as (H1 & H2 & _). unfold lex_char. rewrite H1.
    destruct (Byte.eqb a "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match rest with
             | [] => None
             | d :: r' => match two_char a d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct rest as [|b r']; auto. rewrite (Hn b r' eq_refl). reflexivity. }
    rewrite Hk. fin_off.
  - exists c, (ns ++ rest)%list. split; [reflexivity|].
    destruct (name_first _ Hc) as (H1 & H2 & H3 & H4 & H5 & H6). unfold lex_char. rewrite H1.
    destruct (Byte.eqb c "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match (ns ++ rest)%list with
             | [] => None
             | d :: r' => match two_char c d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (ns ++ rest)%list as [|b r']; auto. rewrite H3. reflexivity. }
    rewrite H4, H5, Hc, span_app by auto. fin_off.
  - exists """"%byte, (body ++ [""""%byte] ++ rest)%list. split; [simpl; rewrite <- app_assoc; reflexivity|].
    unfold lex_char. simpl is_ws. cbv iota beta.
    replace (match (body ++ [""""%byte] ++ rest)%list with
             | [] => None
             | d :: r' => match two_char """"%byte d with Some k0 => Some (k0, d, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (body ++ [""""%byte] ++ rest)%list as [|b r']; auto. }
    simpl one_char. simpl is_digit. simpl is_name_start. cbv iota beta. simpl Byte.eqb. cbv iota beta.
    simpl app. rewrite lex_string_complete by auto. simpl.
    fin_off.
  - simpl in Hd. apply andb_prop in Hd as [Hd Hds].
    exists d, (ds ++ frac ++ ex ++ rest)%list. split; [simpl; rewrite <- !app_assoc; reflexivity|].
    destruct (digit_first _ Hd) as (H1 & H2 & H3 & H4 & H5). unfold lex_char. rewrite H1.
    destruct (Byte.eqb d "#"%byte) eqn:E; [apply Byte.byte_dec_bl in E; contradiction|].
    replace (match (ds ++ frac ++ ex ++ rest)%list with
             | [] => None
             | d0 :: r' => match two_char d d0 with Some k0 => Some (k0, d0, r') | None => None end
             end) with (@None (token_kind * byte * list byte)).
    2: { destruct (ds ++ frac ++ ex ++ rest)%list as [|b r']; auto. rewrite H3. reflexivity. }
    rewrite H4, Hd. rewrite lex_number_complete; auto. simpl. rewrite Hd, Hds. reflexivity.
Qed.

Lemma lex_go_complete i cs ts : Lexes i cs ts ->
  forall fuel acc, length cs < fuel -> lex_go fuel cs i acc = LOk (rev acc ++ ts)%list.
Proof.
  induction 1 as [i | i c r ts Hws H IH | i com r ts Hcom Hr H IH | i cs k r ts Hx H IH];
    intros fuel acc Hf; (destruct fuel as [|fuel]; [simpl in Hf; lia|]).
  - simpl. unfold rev'. rewrite <- rev_alt. reflexivity.
  - simpl. unfold lex_char. rewrite Hws. apply IH. simpl in Hf. lia.
  - simpl. unfold lex_char. simpl is_ws. cbv iota beta. simpl Byte.eqb. cbv iota beta.
    rewrite span_app by auto. apply IH. simpl in Hf. rewrite app_length in Hf. lia.
  - destruct (lexeme_char _ _ _ i acc Hx) as (c & rest0 & E & Hc).
    rewrite E. simpl. rewrite Hc. rewrite IH.
    + simpl. rewrite <- app_assoc. reflexivity.
    + rewrite E in Hf. simpl in Hf.
      assert (length (cs ++ r)%list = S (length rest0)) by (rewrite E; reflexivity).
      rewrite app_length in H0. destruct cs; [inversion Hx|]. simpl in H0. lia.
Qed.

Theorem lex_complete cs ts : Lexes 0 cs ts -> lex cs = LOk ts.
Proof. intro H. unfold lex. rewrite (lex_go_complete _ _ _ H); auto. Qed.

(* ===================== Token lists that end in EOF ======================== *)

(* The lexer's output ends with the EOF token, which no rule consumes; so
   every token consumed shortens the list.  The parser's fuel bound rests on
   that. *)
Definition Ends (ts : list token) : Prop := exists pre x o, ts = (pre ++ [Tok EOF x o])%list.

Lemma lexes_ends i cs ts : Lexes i cs ts -> Ends ts.
Proof.
  induction 1 as [i| | |i cs k r ts Hx H (pre & x & o & ->)]; auto.
  - exists [], "", i. reflexivity.
  - exists (Tok k (string_of_list_byte cs) i :: pre), x, o. reflexivity.
Qed.

Lemma ends_len ts : Ends ts -> 1 <= length ts.
Proof. intros (pre & x & o & ->). rewrite app_length. simpl. lia. Qed.

Lemma ends_advance ts : Ends ts -> kind ts <> EOF -> Ends (advance ts) /\ length (advance ts) < length ts.
Proof.
  intros (pre & x & o & ->) Hk. destruct pre as [|t pre]; [exfalso; apply Hk; reflexivity|].
  simpl. replace (advance (t :: pre ++ [Tok EOF x o])%list) with (pre ++ [Tok EOF x o])%list
    by (destruct pre; reflexivity).
  split; [exists pre, x, o; reflexivity | lia].
Qed.

Lemma Tk_ends k ts ts' : Tk k ts ts' -> k <> EOF -> Ends ts -> Ends ts' /\ length ts' < length ts.
Proof. intros [E ->] Hk He. apply ends_advance; congruence. Qed.

Lemma at_eq k ts : kind ts = k -> at_ k ts = true.
Proof. unfold at_. intros ->. apply token_kind_eqb_refl. Qed.
Lemma at_neq k ts : kind ts <> k -> at_ k ts = false.
Proof. unfold at_. intro H. apply token_kind_eqb_neq. auto. Qed.
Lemma at_any_in ks ts : In (kind ts) ks -> at_any ks ts = true.
Proof. unfold at_any. intro H. apply existsb_exists. exists (kind ts). split; auto. apply token_kind_eqb_refl. Qed.
Lemma at_any_nin ks ts : ~ In (kind ts) ks -> at_any ks ts = false.
Proof.
  unfold at_any. intro H. destruct (existsb (token_kind_eqb (kind ts)) ks) eqn:E; auto.
  apply existsb_exists in E as (k & Hin & Ek). apply token_kind_eqb_eq in Ek. subst. contradiction.
Qed.
Lemma expect_eq k ts : kind ts = k -> expect k ts = POk (peek ts) (advance ts).
Proof. intro H. unfold expect. rewrite at_eq; auto. Qed.
Lemma expect_name_eq ts x ts' : TkName ts x ts' -> expect_name ts = POk x ts'.
Proof. intros (E1 & E2 & ->). unfold expect_name. rewrite expect_eq; auto. simpl. rewrite E2. reflexivity. Qed.

Lemma expect_name_eq' ts : kind ts = NAME -> expect_name ts = POk (tx (peek ts)) (advance ts).
Proof. intro E. unfold expect_name. rewrite expect_eq; auto. Qed.

(* What a completeness lemma concludes: the parser finds it, and the rest
   is shorter (strictly, `Ok1`, for what always consumes a token). *)
Definition Ok {A} (r : pr A) (v : A) (ts rest : list token) : Prop :=
  r = POk v rest /\ Ends rest /\ length rest <= length ts.
Definition Ok1 {A} (r : pr A) (v : A) (ts rest : list token) : Prop :=
  r = POk v rest /\ Ends rest /\ length rest < length ts.

(* Every Tk in the context: record the shorter list and its end. *)
Ltac tks :=
  repeat match goal with
  | H : Tk ?k ?ts ?ts', He : Ends ?ts |- _ =>
      let E1 := fresh "En" in let L1 := fresh "Ln" in
      destruct (Tk_ends k ts ts' H ltac:(discriminate) He) as [E1 L1];
      let E := fresh "Ek" in let A := fresh in destruct H as [E A]; subst ts'
  | H : TkText ?k ?ts ?x ?ts', He : Ends ?ts |- _ =>
      let E1 := fresh "En" in let L1 := fresh "Ln" in
      destruct (Tk_ends k ts ts' (conj (proj1 H) (proj2 (proj2 H))) ltac:(discriminate) He) as [E1 L1];
      let E := fresh "Ek" in let X := fresh "Ex" in let A := fresh in destruct H as (E & X & A); subst ts'
  | H : TkName ?ts ?x ?ts', He : Ends ?ts |- _ =>
      let E1 := fresh "En" in let L1 := fresh "Ln" in
      destruct (Tk_ends NAME ts ts' (conj (proj1 H) (proj2 (proj2 H))) ltac:(discriminate) He) as [E1 L1];
      let E := fresh "Ek" in let X := fresh "Ex" in let A := fresh in destruct H as (E & X & A); subst ts'
  end.

(* Decide the parser's tests from what is known of the next tokens. *)
Ltac known := first [ assumption | congruence | discriminate
                    | (intro; congruence) | (simpl; intros; intuition congruence) ].
Ltac decide_ :=
  repeat first
    [ rewrite at_eq by known | rewrite at_neq by known
    | rewrite at_any_in by known | rewrite at_any_nin by known
    | rewrite expect_eq by known
    | match goal with H : TkName ?ts ?x _ |- context [expect_name ?ts] => rewrite (expect_name_eq _ _ _ H) end
    | rewrite expect_name_eq' by known
    | match goal with H : tx (peek ?t) = ?x |- context [tx (peek ?t)] => rewrite H end
    | progress (cbv beta iota zeta delta [pbind]) ].

Lemma comma_list_complete {A} (X : list token -> A -> list token -> Prop) item closers :
  forall ts xs rest, CommaList X closers ts xs rest -> Ends ts ->
  (forall t v r, X t v r -> Ends t -> length t <= length ts -> Ok (item t) v t r) ->
  forall m, length ts < m -> Ok (comma_list m item closers ts) xs ts rest.
Proof.
  induction 1 as [ts Hc | ts x ts1 Hc Hx Hk | ts x ts1 ts2 xs ts3 Hc Hx Ht Hl IH];
    intros He Hi m Hm; (destruct m as [|m]; [lia|]); simpl.
  - rewrite at_any_in by auto. split; auto.
  - rewrite at_any_nin by auto. destruct (Hi _ _ _ Hx He ltac:(lia)) as (-> & E1 & L1). simpl.
    rewrite at_neq by auto. split; auto.
  - rewrite at_any_nin by auto. destruct (Hi _ _ _ Hx He ltac:(lia)) as (-> & E1 & L1). simpl.
    destruct (Tk_ends _ _ _ Ht ltac:(discriminate) E1) as [E2 L2]. destruct Ht as [Ek ->].
    rewrite at_eq by auto.
    destruct (IH E2 ltac:(intros; apply Hi; auto; lia) m ltac:(lia)) as (-> & E3 & L3). simpl.
    split; [reflexivity | split; auto; lia].
Qed.

(* ===================== Names, specs, types, patterns ====================== *)

Lemma path_rest_complete : forall ts p rest, Path ts p rest -> Ends ts ->
  forall x q, p = x :: q -> forall n acc, length ts <= n ->
  Ok (path_rest n (x :: acc) (advance ts)) ((rev acc ++ x :: q)%list) ts rest /\ length rest < length ts.
Proof.
  induction 1 as [ts y ts1 Hy Hk | ts y ts1 ts2 p ts3 Hy Ht Hp IH]; intros He x q Eq n acc Hn;
    injection Eq as <- <-; tks; (destruct n as [|n]; [lia|]); simpl.
  - rewrite at_neq by auto. unfold rev'. rewrite <- rev_alt. simpl. repeat split; auto; lia.
  - rewrite at_eq by auto.
    match goal with E : Ends (advance (advance ts)) |- _ => rename E into E2 end.
    assert (Hq := path_first _ _ _ Hp). destruct Hq as (q' & -> & _).
    assert (Hk2 : kind (advance (advance ts)) = NAME)
      by (destruct Hp as [? ? ? (E & _) | ? ? ? ? ? ? (E & _) _ _]; auto).
    rewrite expect_name_eq' by auto. simpl.
    edestruct (IH E2 _ q' eq_refl n (y :: acc)) as ((Ep & E3 & L3) & L4); [lia|].
    unfold Ok. repeat split; auto; try lia.
    etransitivity; [exact Ep|]. simpl. rewrite <- app_assoc. reflexivity.
Qed.

Lemma path_complete ts p rest : Path ts p rest -> Ends ts -> forall n, length ts <= n -> Ok1 (parse_path n ts) p ts rest.
Proof.
  intros Hp He n Hn. unfold parse_path.
  assert (Hx := path_first _ _ _ Hp). destruct Hx as (q & -> & _).
  assert (Hk : kind ts = NAME) by (destruct Hp as [? ? ? (E & _) | ? ? ? ? ? ? (E & _) _ _]; auto).
  rewrite expect_name_eq' by auto. simpl.
  destruct (path_rest_complete _ _ _ Hp He _ _ eq_refl n [] Hn) as ((Ep & E1 & L1) & L2).
  repeat split; auto.
Qed.

(* ===================== Properties of the grammar ========================== *)

(* D68, as a property of the grammar: an operator expression is never
   followed by a binary operator.  So two operators the partial order does
   not relate, written together unparenthesized, have no derivation. *)
Theorem operators_maximal nr ts e rest : Operators nr ts e rest -> binop rest = None.
Proof. destruct 1; auto. Qed.

Lemma path_kind ts p rest : Path ts p rest -> kind ts = NAME.
Proof. destruct 1 as [? ? ? (E & _) _ | ? ? ? ? ? ? (E & _) _ _]; auto. Qed.

(* A type starts with `|`, a name, `This`, `(` or `{`. *)
Lemma type_atom_first ts t rest : TypeAtom ts t rest -> In (kind ts) [NAME; THIS_TYPE; LPAREN; LBRACE].
Proof.
  intro H. destruct H as [? ? ? ? ? Hp _ | ? ? [E _] | ? ? ? [E _] _ | ? ? ? ? ? [E _] _ _ | ? ? ? Hr].
  - rewrite (path_kind _ _ _ Hp). simpl. auto.
  - rewrite E. simpl. auto.
  - rewrite E. simpl. auto.
  - rewrite E. simpl. auto.
  - destruct Hr as [? ? ? ? ? [E _] _ _]. rewrite E. simpl. auto.
Qed.

Lemma type_first ts t rest : Type_ ts t rest -> In (kind ts) [PIPE; NAME; THIS_TYPE; LPAREN; LBRACE].
Proof.
  intro H. destruct H as [ts ts1 [E _] _ | ts ts1 ms ts2 Hp Hm].
  - rewrite E. simpl. auto.
  - destruct Hp as [[E _]|[_ ->]]; [rewrite E; simpl; auto|].
    destruct Hm as [? ? ? Ha _ | ? ? ? ? ? ? Ha _ _]; apply type_atom_first in Ha; simpl in *; tauto.
Qed.

(* ===================== Completeness ===================================== *)

(* The parser's completeness, Part VI's parser_complete, is proved in
   FrontEndComplete.v from the lemmas above (lex_complete, lexes_ends,
   comma_list_complete, path_complete) by induction on the parser's fuel. *)

(* Parse.walk applies SourceNames.wf_names at the AST boundary. The
   SnapshotLoader proofs consume the parsed AST directly. *)
