(* Separate typing and translation for local control and function calls.
   Typing chooses a plan over source names; translation uses numbered locals
   and encodes the checked context and demand choices. *)
From Stdlib Require Import List String.
From NM Require Import Audit.
Import ListNotations Audit.Input Audit.Semantics Audit.Semantics.Elaboration.
Local Open Scope string_scope.

Inductive expression_plan : Type :=
| PlanUnit
| PlanThis
| PlanLocal (n : name)
| PlanBlock (body : block_plan)
| PlanIf (condition : expression_plan) (then_body : block_plan) (otherwise : option expression_plan)
| PlanReturnUnit
| PlanReturn (result : expression_plan)
| PlanBreak
| PlanDefinedCall (f : nat) (formal : cx) (needs : list item)
    (arguments : subst) (demands : list family_use) (call_site : cx) (args : list expression_plan)
| PlanAbstractCall (f : nat) (args : list expression_plan)
with block_plan : Type :=
| PlanEmpty
| PlanTail (result : expression_plan)
| PlanLet (n : name) (declared : ty) (mode : lmut) (initializer : expression_plan) (rest : block_plan)
| PlanAssign (n : name) (value : expression_plan) (rest : block_plan)
| PlanExprLast (value : expression_plan)
| PlanExprThen (value : expression_plan) (rest : block_plan)
| PlanWhile (condition : expression_plan) (body rest : block_plan)
| PlanLoop (body rest : block_plan)
| PlanLoopLast (body : block_plan).

Inductive TypesExpr (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesUnit : forall F C exp, TypesExpr W F C AEUnit exp PlanUnit TUnit
| TypesThis : forall F C exp t, fr_this F = Some t -> TypesExpr W F C AEThis exp PlanThis t
| TypesLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    TypesExpr W F C (AEPath [n] None) exp (PlanLocal n) t
| TypesBlock : forall F C exp b plan t,
    TypesBlkE W F C b exp plan t ->
    TypesExpr W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => t end)
| TypesIfThen : forall F C exp c b cp bp bt,
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    TypesExpr W F C (AEIf c b None) exp (PlanIf cp bp None) TUnit
| TypesIfElse : forall F C exp c b e cp bp bt ep et t,
    TypesCond W F C c cp -> TypesBlkE W F C b exp bp bt ->
    TypesBranch W F C e exp ep et -> opt_fits et exp -> Join (branch_ty b bt) et exp t ->
    TypesExpr W F C (AEIf c b (Some e)) exp (PlanIf cp bp (Some ep)) t
| TypesReturnUnit : forall F C exp, fr_ret F = Some TUnit ->
    TypesExpr W F C (AEReturn None) exp PlanReturnUnit never
| TypesReturn : forall F C exp e expected plan actual,
    fr_ret F = Some expected ->
    TypesExpr W F C e (Some expected) plan actual -> fits actual expected ->
    TypesExpr W F C (AEReturn (Some e)) exp (PlanReturn plan) never
| TypesBreak : forall F C exp, fr_loop F = true -> TypesExpr W F C AEBreak exp PlanBreak never
| TypesDefinedCall : forall F C exp pa args f formal needs arguments g plans demands call_site,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f formal needs ->
    Demand W C (map (at_th C arguments) needs) [] C demands call_site ->
    NeedsSatisfied call_site formal arguments needs -> CallTheta W call_site formal f arguments ->
    SigOf W f (c_ty call_site) None g ->
    TypesArgs W F C args (sg_params g) plans ->
    RequirementsInForce C formal ->
    TypesExpr W F C (AECall pa None args) exp
      (PlanDefinedCall f formal needs arguments demands call_site plans) (sg_ret g)
| TypesAbstractCall : forall F C exp pa args f g plans,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g -> TypesArgs W F C args (sg_params g) plans ->
    TypesExpr W F C (AECall pa None args) exp (PlanAbstractCall f plans) (sg_ret g)
with TypesBlkE (W : world) : frame -> cx -> ablock -> option ty -> block_plan -> ty -> Prop :=
| TypesBlkCheck : forall F C ss tl exp plan t made,
    TypesBlk W (without_pins F) C ss tl exp plan t made ->
    (forall g, In g made -> ~ In g (gens t)) -> opt_fits t exp ->
    TypesBlkE W F C (ABlock ss tl) exp plan t
with TypesBlk (W : world) : frame -> cx -> list astmt -> option aexpr -> option ty -> block_plan -> ty -> list nat -> Prop :=
| TypesTail : forall F C y exp plan t,
    TypesExpr W F C y exp plan t -> TypesBlk W F C [] (Some y) exp (PlanTail plan) t []
| TypesEmpty : forall F C exp, TypesBlk W F C [] None exp PlanEmpty TUnit []
| TypesLet : forall F C st n ann y rest tl exp declared v vt t0 cont t made mu,
    LetStmt st n ann y mu ->
    (match ann with
    | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
    | None => declared = None end) ->
    TypesExpr W F C y declared v vt ->
    (match declared with Some d => fits vt d /\ t0 = d | None => t0 = vt end) ->
    Named F n ->
    TypesBlk W F (add_local C n t0 mu) rest tl exp cont t made ->
    TypesBlk W F C (st :: rest) tl exp (PlanLet n t0 mu v cont) t made
| TypesAssign : forall F C n y rest tl exp tv v vt cont t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    TypesExpr W F C y (Some tv) v vt -> fits vt tv ->
    TypesBlk W F C rest tl exp cont t made -> Named F n ->
    TypesBlk W F C (ASAssign n y :: rest) tl exp (PlanAssign n v cont) t made
| TypesExprLast : forall F C y exp v vt,
    TypesExpr W F C y None v vt ->
    TypesBlk W F C [ASExpr y] None exp (PlanExprLast v) (if ty_eqb vt never then never else TUnit) []
| TypesExprThen : forall F C y rest tl exp v vt cont t made,
    (rest <> [] \/ tl <> None) ->
    TypesExpr W F C y None v vt -> TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASExpr y :: rest) tl exp (PlanExprThen v cont) t made
| TypesWhile : forall F C c b rest tl exp cp bp bt cont t made,
    TypesCond W F C c cp -> TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASWhile c b :: rest) tl exp (PlanWhile cp bp cont) t made
| TypesLoop : forall F C b rest tl exp bp bt cont t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks W (fr_mod F) b) ->
    TypesBlkE W (with_loop F) C b None bp bt ->
    TypesBlk W F C rest tl exp cont t made ->
    TypesBlk W F C (ASLoop b :: rest) tl exp (PlanLoop bp cont) t made
| TypesLoopLast : forall F C b exp bp bt,
    TypesBlkE W (with_loop F) C b None bp bt -> ~ BlockBreaks W (fr_mod F) b ->
    TypesBlk W F C [ASLoop b] None exp (PlanLoopLast bp) never []
with TypesArgs (W : world) : frame -> cx -> list aexpr -> list ty -> list expression_plan -> Prop :=
| TypesArgsNil : forall F C, TypesArgs W F C [] [] []
| TypesArgsCons : forall F C a rest t ts plan plans actual,
    TypesExpr W F C a (Some t) plan actual -> fits (canon C actual) (canon C t) ->
    TypesArgs W F C rest ts plans -> TypesArgs W F C (a :: rest) (t :: ts) (plan :: plans)
with TypesCond (W : world) : frame -> cx -> aexpr -> expression_plan -> Prop :=
| TypesBool : forall F C c plan t b,
    LangItem W LIBool b -> TypesExpr W F C c (Some (TNom b [])) plan t ->
    fits t (TNom b []) -> TypesCond W F C c plan
with TypesBranch (W : world) : frame -> cx -> aexpr -> option ty -> expression_plan -> ty -> Prop :=
| TypesBranchBlock : forall F C b exp plan t,
    TypesBlkE W F C b exp plan t ->
    TypesBranch W F C (AEBlock b) exp (PlanBlock plan) (match exp with Some e => e | None => branch_ty b t end)
| TypesBranchExpr : forall F C e exp plan t,
    (forall b, e <> AEBlock b) -> TypesExpr W F C e exp plan t ->
    TypesBranch W F C e exp plan t.

Fixpoint translate_expr (F : frame) (C : cx) (plan : expression_plan) : expr :=
  match plan with
  | PlanUnit => EUnit
  | PlanThis => EVar 0
  | PlanLocal n => EVar (var_of F n)
  | PlanBlock body => translate_block (without_pins F) C body
  | PlanIf condition body other =>
      EIf (translate_expr F C condition) (translate_block (without_pins F) C body)
        (match other with Some plan => translate_expr F C plan | None => EUnit end)
  | PlanReturnUnit => EReturn EUnit
  | PlanReturn result => EReturn (translate_expr F C result)
  | PlanBreak => EBreak
  | PlanDefinedCall f formal needs arguments demands call_site args =>
      translate_demands C demands (translate_call call_site formal f arguments needs
        (map (translate_expr F C) args))
  | PlanAbstractCall f args => EInvoke f (map (translate_expr F C) args)
  end
with translate_block (F : frame) (C : cx) (plan : block_plan) : expr :=
  match plan with
  | PlanEmpty => EUnit
  | PlanTail result => translate_expr F C result
  | PlanLet n t mode initializer rest =>
      ELet (var_of F n) (translate_expr F C initializer) (translate_block F (add_local C n t mode) rest)
  | PlanAssign n value rest =>
      ESeq (EAssign (var_of F n) (translate_expr F C value)) (translate_block F C rest)
  | PlanExprLast value => ESeq (translate_expr F C value) EUnit
  | PlanExprThen value rest => ESeq (translate_expr F C value) (translate_block F C rest)
  | PlanWhile condition body rest =>
      ESeq (EWhile (translate_expr F C condition) (translate_block (without_pins (with_loop F)) C body))
        (translate_block F C rest)
  | PlanLoop body rest =>
      ESeq (ELoop (translate_block (without_pins (with_loop F)) C body)) (translate_block F C rest)
  | PlanLoopLast body => ESeq (ELoop (translate_block (without_pins (with_loop F)) C body)) EUnit
  end.

Scheme types_expr_ind := Induction for TypesExpr Sort Prop
with types_blke_ind := Induction for TypesBlkE Sort Prop
with types_blk_ind := Induction for TypesBlk Sort Prop
with types_args_ind := Induction for TypesArgs Sort Prop
with types_cond_ind := Induction for TypesCond Sort Prop
with types_branch_ind := Induction for TypesBranch Sort Prop.
Combined Scheme body_typing_ind from types_expr_ind, types_blke_ind, types_blk_ind, types_args_ind,
  types_cond_ind, types_branch_ind.

Theorem body_translation_agrees : forall W P,
  (forall F C e expected plan t, TypesExpr W F C e expected plan t ->
    Ex W P F C e expected (translate_expr F C plan) t) /\
  (forall F C block expected plan t, TypesBlkE W F C block expected plan t ->
    BlkE W P F C block expected (translate_block (without_pins F) C plan) t) /\
  (forall F C statements tail expected plan t made, TypesBlk W F C statements tail expected plan t made ->
    Blk W P F C statements tail expected (translate_block F C plan) t made) /\
  (forall F C args types plans, TypesArgs W F C args types plans ->
    Args W P F C args types (map (translate_expr F C) plans)) /\
  (forall F C condition plan, TypesCond W F C condition plan ->
    Cond W P F C condition (translate_expr F C plan)) /\
  (forall F C branch expected plan t, TypesBranch W F C branch expected plan t ->
    Branch W P F C branch expected (translate_expr F C plan) t).
Proof.
  intros W P. apply body_typing_ind; cbn [translate_expr translate_block map]; intros;
    eauto 8 using ExUnit, ExThis, ExLocal, ExBlock, ExIfThen, ExIfElse, ExReturnUnit, ExReturn,
    ExBreak, ExCallD, ExCallA, BlkECheck, BlkTail, BlkEmpty, BlkLet, BlkAssign, BlkExprLast,
    BlkExpr, BlkWhile, BlkLoop, BlkLoopLast, ArgsNil, ArgsCons, CondBool, BranchBlock, BranchExpr.
Qed.

(* Slot numbers are absent from the plan. Only the set of source names
   is checked, so translation can choose any valid numbering. *)
Definition renumber_body_frame (F : frame) (slots : list (name * nat)) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) slots (fr_fresh F) (fr_applying F) (fr_pins F).

Definition SameLocalNames (F : frame) (slots : list (name * nat)) : Prop :=
  forall n, Named F n <-> slook n slots <> None.

Local Lemma renumbered_named : forall F slots n,
  SameLocalNames F slots -> Named F n -> Named (renumber_body_frame F slots) n.
Proof. intros F slots n Names NamedBefore. exact (proj1 (Names n) NamedBefore). Qed.

Theorem body_typing_renumber : forall W,
  (forall F C e expected plan t, TypesExpr W F C e expected plan t ->
    forall slots, SameLocalNames F slots -> TypesExpr W (renumber_body_frame F slots) C e expected plan t) /\
  (forall F C block expected plan t, TypesBlkE W F C block expected plan t ->
    forall slots, SameLocalNames F slots -> TypesBlkE W (renumber_body_frame F slots) C block expected plan t) /\
  (forall F C statements tail expected plan t made, TypesBlk W F C statements tail expected plan t made ->
    forall slots, SameLocalNames F slots ->
    TypesBlk W (renumber_body_frame F slots) C statements tail expected plan t made) /\
  (forall F C args types plans, TypesArgs W F C args types plans ->
    forall slots, SameLocalNames F slots -> TypesArgs W (renumber_body_frame F slots) C args types plans) /\
  (forall F C condition plan, TypesCond W F C condition plan ->
    forall slots, SameLocalNames F slots -> TypesCond W (renumber_body_frame F slots) C condition plan) /\
  (forall F C branch expected plan t, TypesBranch W F C branch expected plan t ->
    forall slots, SameLocalNames F slots -> TypesBranch W (renumber_body_frame F slots) C branch expected plan t).
Proof.
  intros W. apply body_typing_ind; intros;
    eauto 9 using TypesUnit, TypesThis, TypesLocal, TypesBlock, TypesIfThen, TypesIfElse, TypesReturnUnit,
    TypesReturn, TypesBreak, TypesDefinedCall, TypesAbstractCall, TypesBlkCheck, TypesTail, TypesEmpty,
    TypesLet, TypesAssign, TypesExprLast, TypesExprThen, TypesWhile, TypesLoop, TypesLoopLast,
    TypesArgsNil, TypesArgsCons, TypesBool, TypesBranchBlock, TypesBranchExpr, renumbered_named.
Qed.

(* A source-only fragment for checking the reverse bridge. Names listed
   in scope are local at that occurrence, including after let/var. *)
Inductive LocalExpr : list name -> aexpr -> Prop :=
| LocalUnit : forall scope, LocalExpr scope AEUnit
| LocalPath : forall scope n, In n scope -> LocalExpr scope (AEPath [n] None)
| LocalReturnUnit : forall scope, LocalExpr scope (AEReturn None)
| LocalReturn : forall scope result, LocalExpr scope result -> LocalExpr scope (AEReturn (Some result))
| LocalBlockExpr : forall scope statements tail,
    LocalBlock scope statements tail -> LocalExpr scope (AEBlock (ABlock statements tail))
with LocalBlock : list name -> list astmt -> option aexpr -> Prop :=
| LocalEmpty : forall scope, LocalBlock scope [] None
| LocalTail : forall scope result, LocalExpr scope result -> LocalBlock scope [] (Some result)
| LocalLet : forall scope statement n ann initializer mode rest tail,
    LetStmt statement n ann initializer mode -> LocalExpr scope initializer ->
    LocalBlock (n :: scope) rest tail -> LocalBlock scope (statement :: rest) tail.

Scheme local_expr_ind := Induction for LocalExpr Sort Prop
with local_block_ind := Induction for LocalBlock Sort Prop.
Combined Scheme local_body_ind from local_expr_ind, local_block_ind.

Definition LocalsInScope (scope : list name) (C : cx) : Prop :=
  forall n, In n scope -> exists t mode, slook n (c_locals C) = Some (t, mode).

Local Lemma local_scope_extends : forall scope C n t mode,
  LocalsInScope scope C -> LocalsInScope (n :: scope) (add_local C n t mode).
Proof.
  intros scope C n t mode Known other Member. cbn [add_local c_locals slook].
  destruct (String.eqb_spec other n) as [->|Different]; [eauto|].
  destruct Member as [Same|Member]; [congruence|now apply Known].
Qed.

Theorem local_source_has_plan : forall W P,
  (forall scope e, LocalExpr scope e -> forall F C expected code t,
    LocalsInScope scope C -> Ex W P F C e expected code t ->
    exists plan, TypesExpr W F C e expected plan t /\ translate_expr F C plan = code) /\
  (forall scope statements tail, LocalBlock scope statements tail -> forall F C expected code t made,
    LocalsInScope scope C -> Blk W P F C statements tail expected code t made ->
    exists plan, TypesBlk W F C statements tail expected plan t made /\ translate_block F C plan = code).
Proof.
  intros W P. apply local_body_ind.
  - intros scope F C expected code t Known Typed. inversion Typed; subst.
    exists PlanUnit. split; [constructor|reflexivity].
  - intros scope n Member F C expected code t Known Typed.
    destruct (Known n Member) as (local_type & mode & Read).
    inversion Typed; subst; try solve [
      eexists (PlanLocal _); split; [eapply TypesLocal; eassumption|reflexivity]].
    all: exfalso; match goal with
      | NotLocal : forall n', [?local] = [n'] -> slook n' (c_locals ?context) = None |- _ =>
        specialize (NotLocal local eq_refl); congruence
      end.
  - intros scope F C expected code t Known Typed. inversion Typed; subst.
    exists PlanReturnUnit. split; [now constructor|reflexivity].
  - intros scope result Local IH F C expected code t Known Typed.
    inversion Typed; subst.
    match goal with
    | Child : Ex W P F C result (Some ?want) ?value ?actual |- _ =>
      destruct (IH F C (Some want) value actual Known Child) as (plan & TypedPlan & <-)
    end.
    exists (PlanReturn plan). split; [eapply TypesReturn; eassumption|reflexivity].
  - intros scope statements tail Local IH F C expected code t Known Typed.
    inversion Typed; subst.
    match goal with
    | Body : BlkE W P F C (ABlock statements tail) expected _ _ |- _ => inversion Body; subst
    end.
    match goal with
    | Raw : Blk W P (without_pins F) C statements tail expected ?code ?ty ?made |- _ =>
      destruct (IH (without_pins F) C expected code ty made Known Raw) as (plan & TypedPlan & <-)
    end.
    exists (PlanBlock plan). split; [eapply TypesBlock, TypesBlkCheck; eassumption|reflexivity].
  - intros scope F C expected code t made Known Typed. inversion Typed; subst.
    exists PlanEmpty. split; [constructor|reflexivity].
  - intros scope result Local IH F C expected code t made Known Typed. inversion Typed; subst.
    match goal with
    | Child : Ex W P F C result expected ?code ?ty |- _ =>
      destruct (IH F C expected code ty Known Child) as (plan & TypedPlan & <-)
    end.
    exists (PlanTail plan). split; [now constructor|reflexivity].
  - intros scope statement n ann initializer mode rest tail Form Init IHinit Rest IHrest
      F C expected code t made Known Typed.
    inversion Form; subst; clear Form; inversion Typed; subst.
    all: match goal with
    | Form : LetStmt _ _ _ _ _ |- _ => inversion Form; subst; clear Form
    end.
    all: match goal with
    | InitTyped : Ex ?world ?program ?frame ?context ?source ?want ?value ?actual,
      RestTyped : Blk ?world ?program ?frame (add_local ?context ?local ?local_type ?local_mode)
        ?remaining ?last ?expect ?continued ?result_type ?new_names |- _ =>
      destruct (IHinit frame context want value actual Known InitTyped) as (init_plan & InitPlan & <-);
      destruct (IHrest frame (add_local context local local_type local_mode) expect continued result_type new_names
        (local_scope_extends scope context local local_type local_mode Known) RestTyped) as (rest_plan & RestPlan & <-);
      exists (PlanLet local local_type local_mode init_plan rest_plan); split;
      [eapply TypesLet; [constructor|eassumption|eassumption|eassumption|eassumption|eassumption]|reflexivity]
    end.
Qed.

Definition local_example_frame : frame :=
  Frame "/main.moss" (Some TUnit) None false [("saved", 1)] [] [] [].

Definition local_example_source : aexpr :=
  AEBlock (ABlock [ASLet "saved" None AEUnit]
    (Some (AEReturn (Some (AEPath ["saved"] None))))).

Definition local_example_plan : expression_plan :=
  PlanBlock (PlanLet "saved" TUnit LLet PlanUnit
    (PlanTail (PlanReturn (PlanLocal "saved")))).

Example local_example_types : forall W,
  TypesExpr W local_example_frame (Cx [] [] [] [] []) local_example_source None local_example_plan never.
Proof.
  intro W. unfold local_example_source, local_example_plan.
  apply (TypesBlock W _ _ None _ _ never). eapply TypesBlkCheck with (made := []).
  - eapply TypesLet with (declared := None) (vt := TUnit).
    + constructor.
    + reflexivity.
    + constructor.
    + reflexivity.
    + discriminate.
    + apply TypesTail. eapply TypesReturn with (expected := TUnit) (actual := TUnit).
      * reflexivity.
      * eapply TypesLocal; [reflexivity|discriminate].
      * now left.
  - intros g [].
  - exact I.
Qed.

Example local_example_translation :
  translate_expr local_example_frame (Cx [] [] [] [] []) local_example_plan =
  ELet 1 EUnit (EReturn (EVar 1)).
Proof. reflexivity. Qed.
