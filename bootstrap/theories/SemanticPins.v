(* Pins restrict elaboration. A successful derivation remains valid when
   those restrictions are removed from its surrounding frame. *)
From Stdlib Require Import List String Bool Arith.
From NM Require Import Audit Sx.
Import ListNotations.
Module A := Audit.Elaboration.
Definition erase_pins (F : A.frame) : A.frame :=
 A.Frame (A.fr_mod F) (A.fr_ret F) (A.fr_this F) (A.fr_loop F)
  (A.fr_var F) (A.fr_fresh F) (A.fr_applying F) [].
Local Ltac pin_simpl := cbn [erase_pins A.fr_mod A.fr_ret A.fr_this A.fr_loop
 A.fr_var A.fr_fresh A.fr_applying A.fr_pins A.var_of A.Named A.Numbering] in *.

Section Erasure.
Variables (W : A.world) (P : xprogram).
Lemma erase_field_patterns : forall F ts fs bs xs,
 A.FieldPats W F ts fs bs xs -> A.FieldPats W (erase_pins F) ts fs bs xs.
Proof.
 intros F ts fs bs xs H;induction H;
 [eapply A.FPNil with (F:=erase_pins F)|eapply A.FPPun with (F:=erase_pins F)|
 eapply A.FPWild with (F:=erase_pins F)|eapply A.FPBind with (F:=erase_pins F)];pin_simpl;eauto.
Qed.
Lemma erase_pattern : forall F st pat xp bs hs ca,
 A.PatOf W F st pat xp bs hs ca -> A.PatOf W (erase_pins F) st pat xp bs hs ca.
Proof.
 intros F st pat xp bs hs ca H;destruct H;
 [eapply A.PWild with (F:=erase_pins F)|eapply A.PBinder with (F:=erase_pins F)|
 eapply A.PUnit with (F:=erase_pins F)|eapply A.PTagBind with (F:=erase_pins F)|
 eapply A.PRecord with (F:=erase_pins F)];pin_simpl;eauto using erase_field_patterns.
Qed.
Lemma erase_lambda_provider : forall F C ps rt body sg this pid pre sm,
 A.LambdaProvider W P F C ps rt body sg this pid pre sm ->
 A.LambdaProvider W P (erase_pins F) C ps rt body sg this pid pre sm.
Proof.
 intros F C ps rt body sg this pid pre sm H.
 destruct H.
 pin_simpl.
 eapply (@A.LP W P (erase_pins F) C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm);pin_simpl;eauto.
Qed.
Lemma erase_fn_provider : forall F C y sg this pid sm,
 A.FnProvider W P F C y sg this pid sm ->
 A.FnProvider W P (erase_pins F) C y sg this pid sm.
Proof. intros F C y sg this pid sm H;destruct H;pin_simpl;
 [eapply A.FPDefined|eapply A.FPForward];pin_simpl;eauto. Qed.
Lemma erase_meth_provider : forall F C y sg rt pid pre sm,
 A.MethProvider W P F C y sg rt pid pre sm ->
 A.MethProvider W P (erase_pins F) C y sg rt pid pre sm.
Proof. intros F C y sg rt pid pre sm H;destruct H;pin_simpl;
 [eapply A.MPAttached|eapply A.MPWrapped|eapply A.MPLambda];pin_simpl;
 eauto using erase_fn_provider,erase_lambda_provider,erase_pattern. Qed.

(* The recursive proof functions below also recurse through record fields.
   The standard mutual induction scheme omits that nested Forall2 case. *)

Fixpoint erase_ex (F : A.frame) (C : A.cx) (e : aexpr) (exp : option ty) (x : xexpr) (t : ty)
 (H : A.Ex W P F C e exp x t) {struct H} :
 A.Ex W P (erase_pins F) C e exp x t
with erase_args (F : A.frame) (C : A.cx) (ys : list aexpr) (ts : list ty) (xs : list xexpr)
 (H : A.Args W P F C ys ts xs) {struct H} :
 A.Args W P (erase_pins F) C ys ts xs
with erase_cond (F : A.frame) (C : A.cx) (y : aexpr) (x : xexpr)
 (H : A.Cond W P F C y x) {struct H} :
 A.Cond W P (erase_pins F) C y x
with erase_branch (F : A.frame) (C : A.cx) (y : aexpr) (exp : option ty) (x : xexpr) (t : ty)
 (H : A.Branch W P F C y exp x t) {struct H} :
 A.Branch W P (erase_pins F) C y exp x t
with erase_arms (F : A.frame) (C : A.cx) (st : ty) (arms : list (apat * aexpr)) (exp : option ty) (xs : list (xpat * xexpr)) (t : ty)
 (H : A.Arms W P F C st arms exp xs t) {struct H} :
 A.Arms W P (erase_pins F) C st arms exp xs t
with erase_armslist (F : A.frame) (C : A.cx) (st : ty) (arms : list (apat * aexpr)) (exp : option ty) (xs : list (xpat * xexpr)) (heads : list nat) (catch : bool) (t : ty)
 (H : A.ArmsList W P F C st arms exp xs heads catch t) {struct H} :
 A.ArmsList W P (erase_pins F) C st arms exp xs heads catch t
with erase_methcall (F : A.frame) (C : A.cx) (ox : xexpr) (ot : ty) (pa : path) (ys : list aexpr) (x : xexpr) (t : ty)
 (H : A.MethCall W P F C ox ot pa ys x t) {struct H} :
 A.MethCall W P (erase_pins F) C ox ot pa ys x t
with erase_provided (F : A.frame) (C : A.cx) (ox : xexpr) (ot : ty) (m : nat) (ys : list aexpr) (x : xexpr) (t : ty)
 (H : A.Provided W P F C ox ot m ys x t) {struct H} :
 A.Provided W P (erase_pins F) C ox ot m ys x t
with erase_blke (F : A.frame) (C : A.cx) (body : ablock) (exp : option ty) (x : xexpr) (t : ty)
 (H : A.BlkE W P F C body exp x t) {struct H} :
 A.BlkE W P (erase_pins F) C body exp x t
with erase_blk (F : A.frame) (C : A.cx) (ss : list astmt) (tl : option aexpr) (exp : option ty) (x : xexpr) (t : ty) (made : list nat)
 (H : A.Blk W P F C ss tl exp x t made) {struct H} :
 A.Blk W P (erase_pins F) C ss tl exp x t made
with erase_binditems (F : A.frame) (C : A.cx) (its : list (spec * option aexpr)) (wrap : xexpr -> xexpr) (Cout : A.cx) (Fout : A.frame) (made : list nat)
 (H : A.BindItems W P F C its wrap Cout Fout made) {struct H} :
 A.BindItems W P (erase_pins F) C its wrap Cout (erase_pins Fout) made
with erase_binditem (F : A.frame) (C : A.cx) (it : spec * option aexpr) (wrap : xexpr -> xexpr) (Cout : A.cx) (Fout : A.frame) (made : list nat)
 (H : A.BindItem W P F C it wrap Cout Fout made) {struct H} :
 A.BindItem W P (erase_pins F) C it wrap Cout (erase_pins Fout) made .
Proof.

 - destruct H.
   + pin_simpl;eapply A.ExUnit with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExThis with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExLocal with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExUnitTag with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExVal with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExConst with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExTag with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExCallD with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExCallDDemand with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExCallA with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExMeth with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExUnary with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExBinary with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExLit with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExIfThen with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExIfElse with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExMatch with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExBlock with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExReturnUnit with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExReturn with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExBreak with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ExRecord with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
     clear H4 H6.
     match goal with Read : Forall2 _ ftys xfs |- Forall2 _ ftys xfs =>
       induction Read as [|ft xf restF restX One More Tail]
     end.
     * constructor.
     * constructor;[|exact Tail].
       destruct One as [Id [n [v [vt [Fld [Member [Expr Fits]]]]]]].
       split;[exact Id|];exists n,v,vt;repeat split;try assumption.
       eapply erase_ex;exact Expr.
   + pin_simpl;eapply A.ExField with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.ArgsNil with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ArgsCons with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.CondBool with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.BranchBlock with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BranchExpr with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.ArmsEmptyNever with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ArmsSome with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.ArmsOne with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.ArmsMore with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.MCQualified with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.MCProvisionWins with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.MCAttachedDefined with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.MCAttachedDefinedDemand with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.MCAttachedAbstract with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.MCDetached with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.PrIn with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.PrFamily with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.BlkECheck with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.BlkTail with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkEmpty with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkLet with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkAssign with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkExprLast with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkExpr with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BlkWhile with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
     match goal with B : A.BlkE W P (A.with_loop F) ?C ?b ?exp ?x ?t |- _ => exact (erase_blke (A.with_loop F) C b exp x t B) end.
   + pin_simpl;eapply A.BlkLoop with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
     match goal with B : A.BlkE W P (A.with_loop F) ?C ?b ?exp ?x ?t |- _ => exact (erase_blke (A.with_loop F) C b exp x t B) end.
   + pin_simpl;eapply A.BlkLoopLast with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
     match goal with B : A.BlkE W P (A.with_loop F) ?C ?b ?exp ?x ?t |- _ => exact (erase_blke (A.with_loop F) C b exp x t B) end.
   + pin_simpl;eapply A.BlkBind with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.BINil with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BICons with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
 - destruct H.
   + pin_simpl;eapply A.BTypeBind with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BValBind with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BFnBind with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BFnLambda with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BMethBind with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BApplyBody with (F:=erase_pins F) (F':=erase_pins F');pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
     exact (erase_binditems (A.with_mod F (A.d_mod d) u) C (List.concat bodies) wrap C' F' made H3).
   + pin_simpl;eapply A.BApplyAbstract with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BFunCopy with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
   + pin_simpl;eapply A.BFunClosure with (F:=erase_pins F);pin_simpl;eauto using erase_ex,erase_args,erase_cond,erase_branch,erase_arms,erase_armslist,erase_methcall,erase_provided,erase_blke,erase_blk,erase_binditems,erase_binditem,erase_fn_provider,erase_meth_provider,erase_lambda_provider,erase_pattern.
Defined.

Theorem binditems_erase_pins : forall F C its wrap Cout Fout made,
 A.BindItems W P F C its wrap Cout Fout made ->
 A.BindItems W P (erase_pins F) C its wrap Cout (erase_pins Fout) made.
Proof. exact erase_binditems. Qed.
End Erasure.

