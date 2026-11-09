(* ------------------------------------------------------------------------ *)
(* RelFamily.v — the D84 family example: a member of a family on demand    *)
(* (rule 7).  The DRAFT relation relates `pick` to an application of the   *)
(* family at θ = (Item ↦ A) around the call.  m4: the elaborator now        *)
(* implements rule 7, and its output for `pick` is that very term           *)
(* (family_elaborator_agrees), so pick_elab relates the elaborator's own    *)
(* output to the source.                                                    *)
(*                                                                           *)
(*   type Item;  tag Box(Item);  tag A;                                      *)
(*   assume Item { fn Box.get(): Item; }                                     *)
(*   functor Fam[Item]: Item -> Box.get;                                     *)
(*   assume Fam { fn pick(b: Box[Item=A]): A { b.get() } }                   *)
(* (scratch/m3-audit/probes/family.moss, parsed with the d84 branch's        *)
(* parser, without the prelude.)                                             *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith Lia.
From NM Require Import ExecutionModel CoreMoss MossX NMap Sx Elab RelSupport BxFamily.
Import ListNotations.
Import ExecutionModel.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* m4: the elaborator's bodies (the program has no `main`, so they are read
   after `lower_all`, before `elab_program` looks for one) *)
Definition family_fns : list (nat * xexpr) := Eval vm_compute in
  match read_bundle family_sx with
  | Good b => match load_world b with
              | Good w => match lower_all (St w (w_syms w) (w_nsyms w) NE NE NE [] [] NE [] [] NE NE [] 0
                                                false false [] (lang_modules w) [] (ReifyData.empty_reify_metadata Elab.ety Elab.key)) with
                          | Good (_, s) => st_fns s
                          | Bad _ => []
                          end
              | Bad _ => []
              end
  | Bad _ => []
  end.

Definition S_fa : st := Eval vm_compute in match run_world family_sx with Good s => s | Bad _ => dummy_st end.
Definition W_fa : world := W_of S_fa.
Definition fam_mod : string := "scratch/m3-audit/probes/family.moss".

(* Symbols: Item 1, Box 2, A 3, Fam 4, pick 5, Box.get 6. *)
Example family_symbols :
  map (resolve_sym W_fa fam_mod) [["Item"]; ["Box"]; ["A"]; ["Fam"]; ["pick"]]
  = [Some 1; Some 2; Some 3; Some 4; Some 5].
Proof. vm_compute. reflexivity. Qed.

(* What the relation says `pick` elaborates to: `bind Fam;` at Item = A,
   in a scope of its own, around the call of the provision it makes. *)
Definition pick_body : xexpr :=
  XApply 4 [(1, TNom 3 [])] []
    (XMeth (XVar 0) 6 (IMth (TNom 2 [(1, TNom 3 [])]) 6 [(1, TNom 3 [])]) []).

Definition P_fa : xprogram := XProgram [(5, pick_body)] [] 0 0 0 5 [] [] [] [] [] false [] [].

(* m4: what the elaborator makes of `pick` is the body the relation gives *)
Theorem family_elaborator_agrees : nlook 5 family_fns = Some pick_body.
Proof. vm_compute. reflexivity. Qed.

Ltac brackets :=
  first [ apply BrNone
        | (eapply BrSome;
           repeat (refine (@Forall2_cons _ _ _ _ (_, _) _ _ _ _);
                   [ simpl; do 2 eexists; split; [reflexivity|]; split; [comp|]; split; [comp|];
                     split; [vm_compute; auto | tyof] |]);
           apply Forall2_nil) ].

Ltac tyof_b :=
  first [ tyof
        | (eapply TyNom; [comp | comp | (left; reflexivity) || (right; reflexivity) | brackets | forall2_args]) ].

(* The only thing Fam's result mentions, and its only polymorphic input, is Item. *)
Lemma fam_poly : forall t, Poly W_fa 4 t -> t = 1.
Proof.
  intros t (d & n & ps & a & r & Hd & Ha & Hp). vm_compute in Hd. injection Hd as <-.
  simpl in Ha. injection Ha as <- <- <- <-. destruct Hp as (x & [<-|[]] & Hx).
  vm_compute in Hx. injection Hx as <-. reflexivity.
Qed.

Lemma fam_family : Family W_fa 4.
Proof.
 split.
 - do 5 eexists; split; [comp|]. split; [comp|]. repeat constructor.
 - intros d n ps a r Hd Ha;vm_compute in Hd;injection Hd as <-.
   cbn in Ha;injection Ha as <- <- <- <-.
   apply Forall_cons;[|apply Forall_nil].
   eapply FKMethod;[comp|methodnamed|].
   intros t Hp _ _;apply fam_poly in Hp;subst t.
   eapply FNom with(g:=2);[comp|vm_compute;discriminate|vm_compute;auto|].
   intros [b[H _]];contradiction.
Qed.

(* Box.get's signature mentions only Item, its requirement. *)
Lemma get_sig_in : forall t, SigMentions W_fa 6 t -> In t (w_reqs W_fa 6).
Proof.
  intros t H. destruct H as (d & rcv & n & ps & rt & body & Hd & Ha & [(pt & Hpt & F)|(r0 & Hr & F)]);
    vm_compute in Hd; injection Hd as <-; vm_compute in Ha; injection Ha as <- <- <- <- <-;
    [destruct Hpt|injection Hr as <-].
  inversion F as [| |pa a t' Rs K N|pa a g x Rs K I N|pa a bp sp dd sa x Rs I F']; subst.
  - vm_compute in Rs. injection Rs as <-. vm_compute. left. reflexivity.
  - vm_compute in Rs. injection Rs as <-. vm_compute in K. exfalso. exact (K eq_refl).
  - vm_compute in I. destruct I.
Qed.
Ltac sig_in_reqs ::= apply get_sig_in; assumption.

Lemma fam_mentions : forall t, Mentions W_fa 4 t -> t = 1.
Proof.
  intros t (d & ps & a & r & b & sp & Hd & Hp & Hin & Hm & _). vm_compute in Hd. injection Hd as <-.
  simpl in Hp. injection Hp as <- <- <- <-. destruct Hin as [<-|[]].
  inversion Hm; subst.
  - match goal with H : resolve_sym _ _ _ = Some ?r |- _ => vm_compute in H; injection H as <- end.
    match goal with H : (t = 2 /\ _) \/ _ |- _ =>
      destruct H as [[-> Hk]|[_ Hi]]; [vm_compute in Hk; discriminate Hk |] end.
    vm_compute in Hi. destruct Hi as [<-|[]]. reflexivity.
  - match goal with H : resolve_sym _ _ _ = Some ?r |- _ => vm_compute in H; injection H as <- end.
    match goal with H : MethodNamed _ _ _ _ ?m |- _ =>
      destruct H as [Hat|(_ & Hs)]; [ | vm_compute in Hs; discriminate Hs ] end.
    unfold Attached in Hat. vm_compute in Hat. injection Hat as <-.
    match goal with H : SigMentions _ _ _ |- _ => apply get_sig_in in H; vm_compute in H; destruct H as [<-|[]] end.
    reflexivity.
  - match goal with H : In _ [] |- _ => destruct H end.
Qed.

(* So Fam has no fixed inputs (note 70: nothing to have in force). *)
Lemma fam_no_fixed : forall t, ~ Fixed W_fa 4 t.
Proof.
  intros t [Hm Hp]. apply fam_mentions in Hm. subst t. apply Hp.
  do 5 eexists. split; [comp |]. split; [comp |].
  simpl. eexists. split; [left; reflexivity | comp].
Qed.

Ltac fixed_hook ::= intros ?x ?Hx; exfalso; exact (fam_no_fixed _ Hx).

Lemma fam_no_outputs : forall t, ~ FunOutputs W_fa 4 t.
Proof.
  intros t (d & ps & a & r & b & sp & Hd & Hp & Hin & Hs). vm_compute in Hd. injection Hd as <-.
  simpl in Hp. injection Hp as <- <- <- <-. destruct Hin as [<-|[]]. inversion Hs.
Qed.

Definition C_pick : cx := Cx [] [] [] [] [] [(4, [])] [(IFun 4,[])] [].

Theorem pick_elab : FnElab W_fa P_fa 5.
Proof.
  intros d rcv n ps rt b Hd Ha. vm_compute in Hd. injection Hd as <-. simpl in Ha.
  injection Ha as <- <- <- <- <-.
  exists C_pick, [IFun 4], None, (FSig [TNom 2 [(1, TNom 3 [])]] (TNom 3 [])),
         (Frame fam_mod (TNom 3 []) None false [("b", 0)] [] [] []).
  eexists; eexists.
  split.
  { (* [D20] assume Fam: the family is in force *)
    eexists; eexists; eexists; eexists; eexists; exists []. split; [comp|]. split; [items|]. split; [rootctx|].
    split; [apply mgm_nil; vm_compute; reflexivity|]. split; [vm_compute; reflexivity|].
    split; [repeat constructor; simpl; tauto|]. intro k; simpl; split.
    - intros [<-|[]]. exists (NFun 4 []). split; [left; reflexivity | reflexivity].
    - intros (e & [<-|[]] & E). injection E as <-. left; reflexivity. }
  split; [reflexivity|].
  split.
  { do 6 eexists; split; [comp|]; split; [comp|]; split.
    - simpl. repeat (apply Forall2_cons; [simpl; tyof_b|]); apply Forall2_nil.
    - simpl. tyof_b. }
  do 7 (split; [reflexivity|]).
  split; [numbering|].
  split.
  {
  simpl. eapply BlkECheck with (made := []); [| intros ? [] | simpl; left; vm_compute; reflexivity].
  apply BlkTail.
  (* the call: Box.get is an abstract attached method, not provided here *)
  eapply ExMeth; [eapply ExLocal; [comp | compne] |].
  simpl. eapply MCAttachedAbstract.
  - reflexivity.
  - unfold Attached. comp.
  - intros (m & Hm & _). vm_compute in Hm. discriminate Hm.
  - do 3 eexists. comp.
  - (* [D84] rule 7: Fam produces the key at θ = (Item ↦ A) *)
    eapply PrFamily with (matching := [(1, TNom 3 [])]) (th := [(1, TNom 3 [])]) (g := FSig [] (TNom 3 [])) (u := 4).
    + comp.
    + comp.
    + apply fam_family.
    + apply Forall_cons;[|apply Forall_nil]. cbn.
      do 5 eexists;split;[comp|];split;[comp|].
      exists "Item";split;[left;reflexivity|comp].
    + (* Fam's own terms: its input Item *)
      eexists; eexists; eexists; eexists; eexists; eexists; eexists; eexists; exists [], [].
      split; [comp|]. split; [comp|]. split; [items|].
      split.
      { intro t. split; [intros []|]. intros (_ & Hm & Hn).
        apply fam_mentions in Hm. subst. vm_compute in Hn. discriminate Hn. }
      split; [reflexivity|]. split; [apply mgm_nil; vm_compute; reflexivity|].
      split; [reflexivity | apply needs_keys_of].
    + intros t Hp _. apply fam_poly in Hp. subst. eexists. left. reflexivity.
    + intros [|[|t]] tt H;vm_compute in H;try discriminate.
      injection H as <-;exists(TNom 3 []);split;[reflexivity|left;reflexivity].
    + intros x Hfixed;exfalso;exact(fam_no_fixed x Hfixed).
    + (* its result, read at θ *)
      do 5 eexists. exists []. do 2 eexists. split; [comp|]. split; [comp|].
      split; [intro t; split; [intros []| intro Ho; apply fam_no_outputs in Ho; destruct Ho]|].
      split; [simpl; eapply OIsCons; [eapply OIEMeth; [comp|methodnamed|apply OBrNone|recvty|sigof|appof]|apply OIsNil]|].
      split; [unfold ResultKeys;reflexivity | reflexivity].
    + vm_compute. left. reflexivity.
    + (* the only family in force *)
      intros u' Hne (fx & th' & C' & ks & Hf & _). simpl in Hf.
      destruct (Nat.eqb u' 4) eqn:E; [apply Nat.eqb_eq in E; contradiction | discriminate Hf].
    + apply satisfies_by; vm_compute; reflexivity.
    + comp.
    + apply ArgsNil.
  }
  vm_compute; reflexivity.
Qed.
