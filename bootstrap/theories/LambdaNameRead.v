(* The complete source name reader agrees with structural lexical mentions. *)
From Stdlib Require Import List String Bool.
From NM Require Import ExecutionModel Sx Elab SourceSyntaxInd.
Import ListNotations.
Local Open Scope list_scope.
Module A:=ExecutionModel.Elaboration.
Definition NamesSoundE(x:aexpr):Prop:=forall n,In n(free_names_complete_e x)->A.Mentions_name x n.
Definition NamesSoundB(b:ablock):Prop:=forall n,In n(free_names_complete_b b)->A.Mentions_name(AEBlock b)n.
Definition NamesSoundS(s:astmt):Prop:=forall n,In n(free_names_complete_s s)->
 exists child,A.Stmt_sub child s/\A.Mentions_name child n.
Lemma block_mention_in_if:forall b n condition otherwise,
 A.Mentions_name(AEBlock b)n->A.Mentions_name(AEIf condition b otherwise)n.
Proof.
 intros[statements tail]n condition otherwise H;inversion H;subst.
 inversion H0;subst.
 - eapply A.MNSub;[apply A.SubIfB;right;reflexivity|eassumption].
 - eapply A.MNSub;[apply A.SubIfB;left;exists s;split;eassumption|eassumption].
Qed.
Theorem complete_name_reader_sound:
 (forall x,NamesSoundE x)/\(forall b,NamesSoundB b)/\(forall s,NamesSoundS s).
Proof.
 assert(AllE:forall x,NamesSoundE x).
 {apply(SourceSyntaxInd.aexpr_ind' NamesSoundE NamesSoundB NamesSoundS).
  - intros n H;contradiction.
  - intros n H;contradiction.
  - intros[|p[|q rest]][br|]n H;cbn[free_names_complete_e]in H;try contradiction.
    destruct H as[Eq|[]];subst;apply A.MNPath.
  - intros p app args IH n H;apply in_flat_map in H as[child[Member Read]].
    eapply A.MNSub;[apply A.SubCallArg;exact Member|].
    exact((proj1(Forall_forall _ _)IH) child Member n Read).
  - intros obj field IH n H;eapply A.MNSub;[apply A.SubField|exact(IH n H)].
  - intros obj path args Obj IH n H;apply in_app_or in H as[Read|Read].
    + eapply A.MNSub;[apply A.SubMethRecv|exact(Obj n Read)].
    + apply in_flat_map in Read as[child[Member Read]].
      eapply A.MNSub;[apply A.SubMethArg;exact Member|exact((proj1(Forall_forall _ _)IH) child Member n Read)].
  - intros path app fields IH n H;apply in_flat_map in H as[[field value][Member Read]].
    pose proof((proj1(Forall_forall _ _)IH) _ Member)as Child.
    destruct value as[child|].
    + eapply A.MNSub;[eapply A.SubRec;exact Member|exact(Child n Read)].
    + cbn[snd]in Read;destruct Read as[Eq|[]];subst n.
      eapply A.MNSub;[apply A.SubRecPun;exact Member|apply A.MNPath].
  - intros condition block otherwise Cond Block Other n H;apply in_app_or in H as[Read|Read].
    + eapply A.MNSub;[apply A.SubIfC|exact(Cond n Read)].
    + apply in_app_or in Read as[Read|Read].
      * apply block_mention_in_if;exact(Block n Read).
      * destruct otherwise as[child|];[|contradiction].
        eapply A.MNSub;[apply A.SubIfE|exact(Other n Read)].
  - intros scrutinee arms Scrutinee IH n H;apply in_app_or in H as[Read|Read].
    + eapply A.MNSub;[apply A.SubMatchS|exact(Scrutinee n Read)].
    + apply in_flat_map in Read as[[pat child][Member Read]].
      eapply A.MNSub;[apply A.SubMatchA with(pt:=pat);exact Member|exact((proj1(Forall_forall _ _)IH) _ Member n Read)].
  - intros[child|]IH n H;[|contradiction]. eapply A.MNSub;[apply A.SubReturn|exact(IH n H)].
  - intros n H;contradiction.
  - intros block IH n H;exact(IH n H).
  - intros op child IH n H;eapply A.MNSub;[apply A.SubUnary|exact(IH n H)].
  - intros op left right Left Right n H;apply in_app_or in H as[Read|Read].
    + eapply A.MNSub;[apply A.SubBinL|exact(Left n Read)].
    + eapply A.MNSub;[apply A.SubBinR|exact(Right n Read)].
  - intros literal n H;contradiction.
  - intros ps rt child IH n H;eapply A.MNSub;[apply A.SubLam|exact(IH n H)].
  - intros statements tail IH Tail n H;apply in_app_or in H as[Read|Read].
    + apply in_flat_map in Read as[statement[Member Read]].
      destruct((proj1(Forall_forall _ _)IH) statement Member n Read)as[child[Sub Mention]].
      eapply A.MNSub;[eapply A.SubBlockS;[exact Member|exact Sub]|exact Mention].
    + destruct tail as[child|];[|contradiction].
      eapply A.MNSub;[apply A.SubBlockT|exact(Tail n Read)].
  - intros name ty child IH n H;exists child;split;[apply A.SSLet|exact(IH n H)].
  - intros name ty child IH n H;exists child;split;[apply A.SSVar|exact(IH n H)].
  - intros name child IH n H;destruct H as[Eq|Read].
    + subst n;exists(AEPath[name]None);split;[apply A.SSAssignName|apply A.MNPath].
    + exists child;split;[apply A.SSAssign|exact(IH n Read)].
  - intros items IH n H;apply in_flat_map in H as[[sp value][Member Read]].
    pose proof((proj1(Forall_forall _ _)IH) _ Member)as Child;destruct value as[child|];[|contradiction].
    exists child;split;[eapply A.SSBind;exact Member|exact(Child n Read)].
  - intros condition block Cond Block n H;apply in_app_or in H as[Read|Read].
    + exists condition;split;[apply A.SSWhileC|exact(Cond n Read)].
    + exists(AEBlock block);split;[apply A.SSWhileB|exact(Block n Read)].
  - intros block IH n H;exists(AEBlock block);split;[apply A.SSLoop|exact(IH n H)].
  - intros child IH n H;exists child;split;[apply A.SSExpr|exact(IH n H)]. }
 split;[exact AllE|]. split.
 - intros b n H;exact(AllE(AEBlock b)n H).
 - intros s n H.
   pose proof(AllE(AEBlock(ABlock[s]None))n)as One.
   cbn[free_names_complete_e free_names_complete_b flat_map List.app]in One.
   rewrite !app_nil_r in One;specialize(One H).
   inversion One;subst;inversion H0;subst.
   match goal with Member:In ?stmt[s]|-_=>destruct Member as[Eq|[]];subst stmt end.
   eauto.
Qed.

Fixpoint sub_names_monotone(source target:aexpr)(Step:A.Sub_expr source target){struct Step}:
 forall n,In n(free_names_complete_e source)->In n(free_names_complete_e target)
with statement_names_monotone(source:aexpr)(target:astmt)(Step:A.Stmt_sub source target){struct Step}:
 forall n,In n(free_names_complete_e source)->In n(free_names_complete_s target).
Proof.
 - destruct Step as[p app args child Member|obj field|obj path args|obj path args child Member|
    path app fields field child Member|path app fields field Member|condition block otherwise|
    condition statements tail otherwise child Choice|condition block child|scrutinee arms|
    scrutinee arms pat child Member|child|statements child|statements tail statement child Member Sub|
    op child|op left right|op left right|ps rt child];intros n Read;cbn[free_names_complete_e].
   + apply in_flat_map;exists child;split;assumption.
   + exact Read.
   + apply in_or_app;left;exact Read.
   + apply in_or_app;right;apply in_flat_map;exists child;split;assumption.
   + apply in_flat_map;exists(field,Some child);split;[exact Member|exact Read].
   + apply in_flat_map;exists(field,None);split;[exact Member|exact Read].
   + apply in_or_app;left;exact Read.
   + apply in_or_app;right;apply in_or_app;left;cbn[free_names_complete_b].
     destruct Choice as[[statement[Member Sub]]|Tail].
     * apply in_or_app;left;apply in_flat_map;exists statement;split;[exact Member|].
       exact(statement_names_monotone _ _ Sub n Read).
     * subst tail;apply in_or_app;right;exact Read.
   + apply in_or_app;right;apply in_or_app;right;exact Read.
   + apply in_or_app;left;exact Read.
   + apply in_or_app;right;apply in_flat_map;exists(pat,child);split;assumption.
   + exact Read.
   + cbn[free_names_complete_b];apply in_or_app;right;exact Read.
   + cbn[free_names_complete_b];apply in_or_app;left;apply in_flat_map;exists statement;split;[exact Member|].
     exact(statement_names_monotone _ _ Sub n Read).
   + exact Read.
   + apply in_or_app;left;exact Read.
   + apply in_or_app;right;exact Read.
   + exact Read.
 - destruct Step as[name ty child|name ty child|name child|name child|child|
    condition block|condition block|block|items sp child Member];intros n Read;cbn[free_names_complete_s].
   + exact Read.
   + exact Read.
   + right;exact Read.
   + cbn[free_names_complete_e]in Read;destruct Read as[Eq|[]];left;exact Eq.
   + exact Read.
   + apply in_or_app;left;exact Read.
   + apply in_or_app;right;exact Read.
   + exact Read.
   + apply in_flat_map;exists(sp,Some child);split;[exact Member|exact Read].
Defined.
Theorem complete_name_reader_complete:forall x n,A.Mentions_name x n->In n(free_names_complete_e x).
Proof.
 intros x n Mention;induction Mention.
 - cbn[free_names_complete_e];left;reflexivity.
 - exact(sub_names_monotone _ _ H _ IHMention).
Qed.
Theorem free_names_exact:forall fuel x n,
 In n(free_names_e fuel x)<->A.Mentions_name x n.
Proof.
 intros fuel x n;split;[apply(proj1 complete_name_reader_sound x n)|apply complete_name_reader_complete].
Qed.
