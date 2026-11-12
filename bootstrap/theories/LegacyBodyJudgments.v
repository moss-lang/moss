(* Combined body judgments used by the safety and elaborator proofs.
   BodyProgramTyping proves their agreement with the separated rules in Audit. *)
From Stdlib Require Import List String.
From NM Require AuditBase.
From NM Require Import Audit.
Import ListNotations Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Set Implicit Arguments.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition TranslatesForwarder (W : world) (P : program) (handle : forwarder_handle)
    (sg : fsig) (number : nat) : Prop :=
  Fresh W number /\ body_of P number = Some (forwarder_body handle sg).

Definition TranslatesLinkedBody (W : world) (P : program) (u gg h : nat)
    (Cu Cg site : cx) (outs : list nat) (bindings : list binding) (ks : list item) : Prop :=
  Fresh W h /\
  funbody_of P h =
    Some (translate_linked_body Cu Cg outs bindings
      (map (fun k => rkey_of site (at_site site k)) ks)) /\
  nlook h (linked_origins (program_origins P)) = Some (u, gg).

Definition FamilyBindings (W : world) (bindings : list binding) : Prop :=
  Forall (fun binding => match binding with
    | BValue _ _ => False
    | BApply family _ _ _ _ => Family W family
    | _ => True end) bindings.

Inductive BindsOf : expr -> list binding -> Prop :=
| BOEnd : BindsOf EUnit []
| BOBind : forall binding body rest,
    BindsOf body rest -> BindsOf (EBind binding body) (binding :: rest).

(* A literal's binary arithmetic call uses a provision already in force,
   or a unique family applied in a scope around that call ([D84] rule 7).
   Its one parameter and result are exactly the literal's own type. *)
Inductive LitOp (W : world) (C : cx) (t : ty) (m : nat) (ox oy : expr) : expr -> Prop :=
| LitOpIn :
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitOp W C t m ox oy (EMethod ox m (rkey C t m) [oy])
| LitOpFamily : forall u matching th formal needs C' ks,
    mlook t m (c_mths C) = None ->
    TypesFamilyApplication W C u matching th formal needs C' ks -> FamilyReadOff W C u t m matching ->
    In (mkey t m) ks -> OnlyFamily W C u (mkey t m) ->
    mlook t m (c_mths C') = Some (FSig [t] t) -> MatchingInForce C matching ->
    LitOp W C t m ox oy (translate_family_use C (FamilyUse u matching th formal needs C' ks) (EMethod ox m (rkey C' t m) [oy])).

(* Interpret exactly the prescribed tree through contextual arithmetic.
   A family may supply an operation, using the same rule as an explicit call. *)
Inductive Literal (W : world) (C : cx) : literal_tree -> expr -> ty -> Prop :=
| LiteralZero : forall z t,
    LangItem W LIZero z -> nlook z (c_vals C) = Some t ->
    Literal W C LZero (EVal z) t
| LiteralOne : forall o t,
    LangItem W LIOne o -> nlook o (c_vals C) = Some t ->
    Literal W C LOne (EVal o) t
| LiteralAdd : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangItem W LIAdd m -> LitOp W C t m x y result ->
    Literal W C (LAdd a b) result t
| LiteralMul : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangItem W LIMul m -> LitOp W C t m x y result ->
    Literal W C (LMul a b) result t
| LiteralDiv : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangItem W LIDiv m -> LitOp W C t m x y result ->
    Literal W C (LDiv a b) result t
| LiteralFloat : forall a x ti tp t f,
    Literal W C a x ti -> LangItem W LIFloat f ->
    nlook f (c_fns C) = Some (FSig [tp] t) -> fits ti tp ->
    Literal W C (LFloat a) (EInvoke f [x]) t.

(* The fields of a record pattern `P{…}` against the payload's fields
   `ftys`, in the order written: `x` binds the field x to a local x, `x =
   y` to a local y, `x = _` binds nothing (note 35). *)
Inductive FieldPats (W : world) (F : frame) (ftys : list (fld * ty))
    : list (name * option apat) -> list (name * ty) -> list (fld * var) -> Prop :=
| FPNil : FieldPats W F ftys [] [] []
| FPPun : forall n t r bs xs,
    slook n ftys = Some t -> Named F n -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, None) :: r) ((n, t) :: bs) ((n, var_of F n) :: xs)
| FPWild : forall n t r bs xs,
    slook n ftys = Some t -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some APWild) :: r) bs xs
| FPBind : forall n y t r bs xs,
    slook n ftys = Some t -> resolve W (fr_mod F) [y] = None -> Named F y ->
    FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some (APPath [y])) :: r) ((y, t) :: bs) ((n, var_of F y) :: xs).

(* The pattern judgment: a source pattern, in frame F, against scrutinee
   type `st` compiles to an pattern, binding locals; `hs` are the heads it
   covers and `catch` whether it matches everything.  A head names the one
   member of st with that head ([D15], note 33). *)
Inductive PatOf (W : world) (F : frame) (st : ty) : apat -> pattern -> list (name * ty) -> list nat -> bool -> Prop :=
| PWild : PatOf W F st APWild (PAny None) [] [] true
| PBinder : forall n,
    resolve W (fr_mod F) [n] = None -> Named F n ->
    PatOf W F st (APPath [n]) (PAny (Some (var_of F n))) [(n, st)] [] true
| PNominal : forall pa s xs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    In (TNom s xs) (members st) ->
    PatOf W F st (APPath pa) (PHead s None []) [] [s] false
(* `S(y)`: y is the payload, at the member's arguments; a struct tag's is a
   bare record type (note 35) *)
| PTagBind : forall pa s xs y pay,
    resolve_sym W (fr_mod F) pa = Some s -> ~ BareTag W s -> In (TNom s xs) (members st) ->
    PayloadOf W s xs pay ->
    resolve W (fr_mod F) [y] = None -> Named F y ->
    PatOf W F st (APTag pa (APPath [y])) (PHead s (Some (var_of F y)) []) [(y, pay)] [s] false
(* `S{x, x = y, x = _}`: S's payload is a record, its fields bound by name.
   The names it binds are distinct: a repeated binder would be typed by its
   last field but read from its first. *)
| PRecord : forall pa s xs fs ftys bs fxs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
    PayloadOf W s xs (TRecord ftys) ->
    FieldPats W F ftys fs bs fxs -> NoDup (map fst bs) ->
    PatOf W F st (APRec (Some pa) fs) (PHead s None fxs) bs [s] false.

(* Main judgments.
   `Ex W P F C e exp x t`: in frame F and context C, the expression e,
   checked against the expected type `exp` if any, elaborates to x of type t.
   `Blk W P F C ss tl exp x t`: a block (statements ss, optional tail).
   `Call W P F C callee args x t` etc. are its pieces. *)
Inductive Ex (W : world) (P : program) : frame -> cx -> aexpr -> option ty -> expr -> ty -> Prop :=
| ExUnit : forall F C exp, Ex W P F C AEUnit exp EUnit TUnit
| ExThis : forall F C exp t, fr_this F = Some t -> Ex W P F C AEThis exp (EVar 0) t
| ExLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    Ex W P F C (AEPath [n] None) exp (EVar (var_of F n)) t
| ExBareTag : forall F C exp pa s,
    (forall n, pa = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> BareTag W s ->
    Ex W P F C (AEPath pa None) exp (ETag s [] EUnit) (TNom s [])
| ExVal : forall F C exp pa s d n t0 t,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 None ->
    nlook s (c_vals C) = Some t ->
    Ex W P F C (AEPath pa None) exp (EVal s) t
(* a defined val (no assumptions yet, [D50]) is its initializer, inlined,
   elaborated in an empty context *)
| ExConst : forall F C exp pa s d n t0 i t x,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 (Some i) ->
    d_assumes d = [] -> TyOf W (d_mod d) [] None t0 t ->
    (exists F0 t', Ex W P F0 (Cx [] [] [] [] []) i (Some t) x t' /\ fits t' t /\
                   fr_mod F0 = d_mod d /\ fr_ret F0 = None /\
                   fr_this F0 = None /\ fr_loop F0 = false /\
                   fr_fresh F0 = [] /\ fr_applying F0 = [] /\ fr_pins F0 = [] /\
                   Numbering F0 None []) ->
    Ex W P F C (AEPath pa None) exp x t
(* a tag applied to its payload *)
| ExTag : forall F C exp pa a args s bs xs pay y v ty,
    resolve_sym W (fr_mod F) pa = Some s -> ~ BareTag W s ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay ->
    args = [y] -> Ex W P F C y (Some pay) v ty -> fits ty pay ->
    Ex W P F C (AECall pa a args) exp (ETag s xs v) (TNom s xs)
(* [D20]/[D21] a defined fn: its needs answered from here (sm), its
   signature read with the meanings here *)
| ExCallD : forall F C exp pa args f Cf ns th g xs demands C',
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns ->
    (* family demands read f's needs through θ (rule B's companion) *)
    Demand W C (map (at_th C th) ns) [] C demands C' ->
    NeedsSatisfied C' Cf th ns -> CallTheta W C' Cf f th -> SigOf W f (c_ty C') None g ->
    (* Q4: the arguments in the caller's context, as at every call
       (MCAttachedDefined, PrFamily): what the call demands is not in force
       for them, so an argument means what it means outside the call (D77:
       a family supplying a key on demand is not a provision of it), and a
       family's result does not shadow what the caller provides. An
       argument means what `let a = e; f(a)` would (as a bracket call's
       arguments, ExCallBr, D89). AUDIT-CHANGES #72;
       tests/wasi/args_context_provision_wins.moss, tests/wasi/args_context_shadow_call.moss *)
    Args W P F C args (sg_params g) xs ->
    (* rule B *)
    RequirementsInForce C Cf ->
    Ex W P F C (AECall pa None args) exp
       (translate_demands C demands (translate_call C' Cf f th ns xs)) (sg_ret g)
(* an abstract fn: whatever provides it here *)
| ExCallA : forall F C exp pa args f g xs,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (EInvoke f xs) (sg_ret g)
(* D89: a fn applied to brackets is the call under those binds, its
   arguments evaluated first (Syntax.call_block) *)
| ExCallBr : forall F C exp pa bs args f x t,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    CallBrackets W (fr_mod F) f bs ->
    Ex W P F C (Syntax.call_block pa bs args) None x t ->
    Ex W P F C (AECall pa (Some bs) args) exp x t
| ExMeth : forall F C exp o pa args ox ot x t,
    Ex W P F C o None ox ot ->
    MethCall W P F C ox (canon C ot) pa args x t ->
    Ex W P F C (AEMeth o pa args) exp x t
(* [D68] operators: the receiver is the (left) operand ([D85]: and the
   `for.` operators, note 60) *)
| ExUnary : forall F C exp op y lib n m ox ot x t,
    operator_method op true = Some (lib, n) -> LangSym W lib NSDetached n m ->
    Ex W P F C y None ox ot -> head (canon C ot) <> None ->
    Provided W P F C ox (canon C ot) m [] x t ->
    Ex W P F C (AEUnary op y) exp x t
| ExBinary : forall F C exp op l r lib n m ox ot x t,
    operator_method op false = Some (lib, n) -> LangSym W lib NSDetached n m ->
    Ex W P F C l None ox ot -> head (canon C ot) <> None ->
    Provided W P F C ox (canon C ot) m [r] x t ->
    Ex W P F C (AEBinary op l r) exp x t
(* [D68] literals *)
| ExLit : forall F C exp text x t,
    NumericText text -> Literal W C (literal_tree_of text) x t ->
    Ex W P F C (AELit text) exp x t
| ExIfThen : forall F C exp c b cx' bx bt,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    Ex W P F C (AEIf c b None) exp (EIf cx' bx EUnit) TUnit
| ExIfElse : forall F C exp c b y cx' bx bt ex' et t,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    Branch W P F C y exp ex' et -> opt_fits et exp -> Join (branch_ty b bt) et exp t ->
    Ex W P F C (AEIf c b (Some y)) exp (EIf cx' bx ex') t
| ExMatch : forall F C exp s arms sx st xarms t,
    Ex W P F C s None sx st ->
    Arms W P F C (canon C st) arms exp xarms t ->
    Ex W P F C (AEMatch s arms) exp (EMatch sx xarms) t
| ExBlock : forall F C exp b x t,
    BlkE W P F C b exp x t -> Ex W P F C (AEBlock b) exp x (match exp with Some e => e | None => t end)
| ExReturnUnit : forall F C exp,
    fr_ret F = Some TUnit ->
    Ex W P F C (AEReturn None) exp (EReturn EUnit) never
| ExReturn : forall F C exp e expected v actual,
    fr_ret F = Some expected ->
    Ex W P F C e (Some expected) v actual -> fits actual expected ->
    Ex W P F C (AEReturn (Some e)) exp (EReturn v) never
| ExBreak : forall F C exp, fr_loop F = true -> Ex W P F C AEBreak exp EBreak never
| ExRecord : forall F C exp pa a fs s bs xs pay ftys xfs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay -> pay = TRecord ftys ->
    (* every field given once, in any order; the record holds them in the payload's order *)
    NoDup (map fst fs) -> List.length fs = List.length ftys ->
    Forall2 (fun ft xf => fst xf = fst ft /\
               exists n v vt, n = fst ft /\ In (n, v) fs /\
                 Ex W P F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
                 fits vt (snd ft)) ftys xfs ->
    Ex W P F C (AERec pa a fs) exp (ETag s xs (ERecord xfs)) (TNom s xs)
| ExField : forall F C exp o n ox ot rec t,
    Ex W P F C o None ox ot ->
    (* a tag's payload, or a bare record (note 44) *)
    ((exists s xs, ot = TNom s xs /\ kind_sym W s = Some DKTag /\ PayloadOf W s xs rec) \/ rec = ot) ->
    (exists ftys, rec = TRecord ftys /\ slook n ftys = Some t) ->
    Ex W P F C (AEField o n) exp (EField ox (n)) t
| ExBareRecord : forall F C exp fs ftys xfs,
    NoDup (map fst fs) -> List.length fs = List.length ftys ->
    record_literal_shape fs exp ftys ->
    Forall2 (fun ft xf => fst xf = fst ft /\
               exists n v vt, n = fst ft /\ In (n, v) fs /\
                 Ex W P F C (record_field_source n v) (record_field_expected exp (snd ft)) (snd xf) vt /\
                 record_field_fits exp vt (snd ft)) ftys xfs ->
    Ex W P F C (AEBareRecord fs) exp (ERecord xfs) (TRecord ftys)

(* the payload type of a tag at its arguments *)

with Args (W : world) (P : program) : frame -> cx -> list aexpr -> list ty -> list expr -> Prop :=
| ArgsNil : forall F C, Args W P F C [] [] []
| ArgsCons : forall F C a r t ts x xs at',
    Ex W P F C a (Some t) x at' -> fits (canon C at') (canon C t) ->
    Args W P F C r ts xs -> Args W P F C (a :: r) (t :: ts) (x :: xs)

(* [D45] a condition is a Bool (lib/bool.moss's) *)
with Cond (W : world) (P : program) : frame -> cx -> aexpr -> expr -> Prop :=
| CondBool : forall F C c x t b,
    LangItem W LIBool b ->
    Ex W P F C c (Some (TNom b [])) x t -> fits t (TNom b []) -> Cond W P F C c x

(* a branch of an `if` (its `else`) or a match arm: a block has its
   branch type, or the expected one (note 42) *)
with Branch (W : world) (P : program) : frame -> cx -> aexpr -> option ty -> expr -> ty -> Prop :=
| BranchBlock : forall F C b exp x t,
    BlkE W P F C b exp x t ->
    Branch W P F C (AEBlock b) exp x (match exp with Some e => e | None => branch_ty b t end)
| BranchExpr : forall F C y exp x t,
    (forall b, y <> AEBlock b) -> Ex W P F C y exp x t -> Branch W P F C y exp x t

(* branches agree, or the expected type decides *)

with Arms (W : world) (P : program) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (pattern * expr) -> ty -> Prop :=
| ArmsEmptyNever : forall F C exp, Arms W P F C never [] exp [] (match exp with Some e => e | None => never end)
| ArmsSome : forall F C st arms exp xarms covered catch t,
    st <> never ->
    ArmsList W P F C st arms exp xarms covered catch t ->
    (* exhaustive: a catch-all arm, or every member's head covered *)
    (catch = true \/ (members st <> [] /\ forall m h, In m (members st) -> head m = Some h -> In h covered)) ->
    Arms W P F C st arms exp xarms t

with ArmsList (W : world) (P : program) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (pattern * expr) -> list nat -> bool -> ty -> Prop :=
| ArmsOne : forall F C st pat body exp xp binds hs ca bx bt,
    PatOf W F st pat xp binds hs ca ->
    Branch W P F (fold_left (fun C q => add_local C (fst q) (snd q) LLet) binds C) body exp bx bt ->
    opt_fits bt exp ->
    ArmsList W P F C st [(pat, body)] exp [(xp, bx)] hs ca bt
| ArmsMore : forall F C st pat body rest exp xp binds hs ca bx bt xr hr cr tr t,
    rest <> [] ->
    PatOf W F st pat xp binds hs ca ->
    Branch W P F (fold_left (fun C q => add_local C (fst q) (snd q) LLet) binds C) body exp bx bt ->
    opt_fits bt exp ->
    ArmsList W P F C st rest exp xr hr cr tr ->
    Join bt tr exp t ->
    ArmsList W P F C st ((pat, body) :: rest) exp ((xp, bx) :: xr) (hs ++ hr) (ca || cr) t

(* [D61]/[D77] which method a call `o.p(args)` means.  `ot` is the
   receiver's canonical type. *)
with MethCall (W : world) (P : program) : frame -> cx -> expr -> ty -> path -> list aexpr -> expr -> ty -> Prop :=
(* `o.mod::m(...)`: the detached method reached through its module *)
| MCQualified : forall F C ox ot pa args m x t,
    List.length pa > 1 -> resolve_detached W (fr_mod F) pa = Some m ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot pa args x t
(* [D77] rule 4: the provision of the detached name in scope wins over an
   attached method, wherever the context provides it at the receiver *)
| MCProvisionWins : forall F C ox ot n args h att m x t,
    head ot = Some h -> Attached W h n att ->
    w_scope W (fr_mod F) NSDetached n = Some (ESym m) -> mlook ot m (c_mths C) <> None ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot [n] args x t
(* [D61] rule 1, a defined attached method: a direct call, at the
   receiver's instantiation ([D74]) *)
| MCAttachedDefined : forall F C ox ot n args h att Ca ns th g xs rargs demands Cd,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    Demand W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)
      (map (at_th (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) th) ns) []
      (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) demands Cd ->
    NeedsSatisfied Cd Ca th ns -> CallTheta W Cd Ca att th -> SigOf W att (c_ty Cd) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    RequirementsInForce (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) Ca ->
    MethCall W P F C ox ot [n] args
      (translate_demands (family_context C rargs) demands (translate_call Cd Ca att th ns (ox :: xs))) (sg_ret g)
(* an abstract attached method: a key the context provides ([D36]) *)
| MCAttachedAbstract : forall F C ox ot n args h att x t,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt, ast_of W att = Some (ADFn (RAttached rn) n ps rt None)) ->
    Provided W P F C ox ot att args x t ->
    MethCall W P F C ox ot [n] args x t
(* [D61] rule 2: no attached method; the detached name in scope *)
| MCDetached : forall F C ox ot n args m x t,
    (forall h att, head ot = Some h -> ~ Attached W h n att) ->
    w_scope W (fr_mod F) NSDetached n = Some (ESym m) ->
    Provided W P F C ox ot m args x t ->
    MethCall W P F C ox ot [n] args x t

(* [D62] a call of method m on a receiver of type ot: the context's one
   provision at key (ot, m), whatever it is bound to *)
with Provided (W : world) (P : program) : frame -> cx -> expr -> ty -> nat -> list aexpr -> expr -> ty -> Prop :=
| PrIn : forall F C ox ot m args g xs,
    mlook ot m (c_mths C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Provided W P F C ox ot m args (EMethod ox m (rkey C ot m) xs) (sg_ret g)
(* [D84] rule 7: no provision here, but a family F in force produces the
   key at θ (read off the key: θ instantiates P(F) so that a result item's
   receiver becomes ot); the call is elaborated as if preceded by `bind F;`
   at θ, in a scope of its own: `apply_expression F θ sm` around the call.  No search:
   at most one family in a context produces a key (DECISIONS.md, rule 7). *)
| PrFamily : forall F C ox ot m args u matching th formal needs C' ks g xs,
    mlook ot m (c_mths C) = None ->
    TypesFamilyApplication W C u matching th formal needs C' ks -> FamilyReadOff W C u ot m matching ->
    In (mkey ot m) ks -> OnlyFamily W C u (mkey ot m) ->
    mlook ot m (c_mths C') = Some g -> Args W P F C args (sg_params g) xs ->
    MatchingInForce C matching ->
    Provided W P F C ox ot m args (translate_family_use C (FamilyUse u matching th formal needs C' ks) (EMethod ox m (rkey C' ot m) xs)) (sg_ret g)

(* a block whose type is checked against `exp` and may not let a value of
   a generation it made escape ([D78]) *)
with BlkE (W : world) (P : program) : frame -> cx -> ablock -> option ty -> expr -> ty -> Prop :=
(* a nested block may rebind a pin for its own use: only the linked
   body's own binds reach its result (tests/wasi/block_pin_rebind.moss) *)
| BlkECheck : forall F C ss tl exp x t made,
    Blk W P (without_pins F) C ss tl exp x t made ->
    (forall g, In g made -> ~ In g (gens t)) ->
    opt_fits t exp ->
    BlkE W P F C (ABlock ss tl) exp x t

(* `Blk ... x t made`: `made` are the generations its binds made *)
with Blk (W : world) (P : program) : frame -> cx -> list astmt -> option aexpr -> option ty -> expr -> ty -> list nat -> Prop :=
| BlkTail : forall F C y exp x t, Ex W P F C y exp x t -> Blk W P F C [] (Some y) exp x t []
| BlkEmpty : forall F C exp, Blk W P F C [] None exp EUnit TUnit []
| BlkLet : forall F C st n ann y rest tl exp declared v vt t0 xr t made mu,
    LetStmt st n ann y mu ->
    match ann with
    | Some a => exists d, TyOf W (fr_mod F) (c_ty C) (fr_this F) a d /\ declared = Some d
    | None => declared = None
    end ->
    Ex W P F C y declared v vt ->
    (match declared with Some d => fits vt d /\ t0 = d | None => t0 = vt end) ->
    Named F n ->
    Blk W P F (add_local C n t0 mu) rest tl exp xr t made ->
    Blk W P F C (st :: rest) tl exp (ELet (var_of F n) v xr) t made
| BlkAssign : forall F C n y rest tl exp tv v vt xr t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    Ex W P F C y (Some tv) v vt -> fits vt tv ->
    Blk W P F C rest tl exp xr t made ->
    Named F n ->
    Blk W P F C (ASAssign n y :: rest) tl exp (ESeq (EAssign (var_of F n) v) xr) t made
| BlkExprLast : forall F C y exp v vt,
    Ex W P F C y None v vt ->
    Blk W P F C [ASExpr y] None exp (ESeq v EUnit) (if ty_eqb vt never then never else TUnit) []
| BlkExpr : forall F C y rest tl exp v vt xr t made,
    (rest <> [] \/ tl <> None) ->
    Ex W P F C y None v vt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASExpr y :: rest) tl exp (ESeq v xr) t made
| BlkWhile : forall F C c b rest tl exp cx' bx bt xr t made,
    Cond W P F C c cx' -> BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASWhile c b :: rest) tl exp (ESeq (EWhile cx' bx) xr) t made
| BlkLoop : forall F C b rest tl exp bx bt xr t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks W (fr_mod F) b) ->
    BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASLoop b :: rest) tl exp (ESeq (ELoop bx) xr) t made
(* a block whose last statement is a `loop` no `break` leaves cannot fall
   through (note 42) *)
| BlkLoopLast : forall F C b exp bx bt,
    BlkE W P (with_loop F) C b None bx bt -> ~ BlockBreaks W (fr_mod F) b ->
    Blk W P F C [ASLoop b] None exp (ESeq (ELoop bx) EUnit) never []
(* a bind statement: its items in order, each wrapping the rest *)
| BlkBind : forall F C its rest tl exp wrap C' F' xr t made made',
    BindItems W P F C its wrap C' F' made ->
    Blk W P F' C' rest tl exp xr t made' ->
    Blk W P F C (ASBind its :: rest) tl exp (wrap xr) t (made ++ made')

(* `BindItems ... wrap C' F' made`: a run of bind items elaborates to the
   binds `wrap` puts around whatever follows them. *)
with BindItems (W : world) (P : program) : frame -> cx -> list (spec * option aexpr) -> (expr -> expr) -> cx -> frame -> list nat -> Prop :=
| BINil : forall F C, BindItems W P F C [] (fun x => x) C F []
| BICons : forall F C it r w1 C1 F1 m1 w2 C2 F2 m2,
    BindItem W P F C it w1 C1 F1 m1 -> BindItems W P F1 C1 r w2 C2 F2 m2 ->
    BindItems W P F C (it :: r) (fun x => w1 (w2 x)) C2 F2 (m1 ++ m2)

with BindItem (W : world) (P : program) : frame -> cx -> (spec * option aexpr) -> (expr -> expr) -> cx -> frame -> list nat -> Prop :=
(* [D72] `bind T = τ;` starts a new generation: from here T means τ (read
   here); what was typed before keeps the old meaning. Runtime closures
   retain the corresponding type interpretations for checked heap casts.
   [D84] 3d: in a body bound to F, protected result inputs may not be rebound. *)
| BTypeBind : forall F C pa t rp ra tau,
    resolve_sym W (fr_mod F) pa = Some t -> kind_sym W t = Some DKType ->
    ~ In t (fr_pins F) ->
    TyOf W (fr_mod F) (c_ty C) None (ATRef rp ra) tau ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath rp ra)) (bind_type_expression t (canon C tau))
             (set_ty C t (canon C tau)) F []
(* `bind v = e;` *)
| BValBind : forall F C pa v d n t0 i declared y x xt a,
    resolve_sym W (fr_mod F) pa = Some v -> w_decl W v = Some d -> d_ast d = ADVal n t0 i ->
    TyOf W (d_mod d) (c_ty C) None t0 declared ->
    Ex W P F C y (Some declared) x xt -> fits xt declared ->
    AppOf W v (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some y) (bind_value_expression v x)
             (provide C (IVal v) a (set_val C v declared)) F []
(* `bind f = g;`: g a defined fn whose signature, read here, is f's *)
| BFnBind : forall F C pa f g sg pg sm a types,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    FnProvider W P F C (AEPath pg None) sg None g types sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (bind_function_expression f g types sm)
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D70] `bind f = |ps| body;`: see LambdaProvider *)
| BFnLambda : forall F C pa f sg ps rt body pid pre sm a,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    LambdaProvider W P F C ps rt body sg None pid pre sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AELam ps rt body)) (fun x => pre (bind_function_expression f pid (lexical_capture C) sm x))
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D61] `bind R.m[brackets] = provider;`: the key is R read here (under
   the brackets), the provider an attached defined method of R's head, a
   defined fn (eta-expanded to take the receiver, [D54]), or a lambda *)
| BMethBind : forall F C pa dn a y r m bs rt sg target pre sm ap types,
    resolve_sym W (fr_mod F) pa = Some r -> MethodNamed W (fr_mod F) r dn m ->
    Brackets W (fr_mod F) (c_ty C) m a bs ->
    TyOf W (fr_mod F) (bs ++ c_ty C) None (ATRef pa None) rt ->
    SigOf W m (bs ++ c_ty C) (Some rt) sg ->
    MethProvider W P F C y sg rt target types pre sm ->
    AppOf W m (bs ++ c_ty C) ap ->
    (* [D84] rule 7: a member of a family in force is not bound directly *)
    (forall u, ~ FamilyProduces W C u (mkey (canon C rt) m)) ->
    (* Q3: the method item is not a defined (attached) method: a call
       resolves to the definition (MCAttachedDefined), never to the key,
       so the provision could not be used (criteria section 6; the
       oracle's check_bind; tests/errors/bind_attached_defined.moss). A
       detached method, or an abstract attached one, is bound as before. *)
    ~ (exists rc nm ps rt0 b, ast_of W m = Some (ADFn rc nm ps rt0 (Some b))) ->
    BindItem W P F C (Spec (Some pa) (Some dn) a, Some y)
             (fun x => pre (bind_method_expression (canon C rt) m (theta_of C ap) target types sm x))
             (provide C (mkey (canon C rt) m) ap (set_mth C (canon C rt) m sg)) F []
(* [D55] `bind F;` for F with a body: F's binds, here, read in F's module *)
| BApplyBody : forall F C pa u d n ps a r bodies wrap C' F' made,
    resolve_sym W (fr_mod F) pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps a r (Some bodies) -> ~ In u (fr_applying F) ->
    BindItems W P (with_mod F (d_mod d) u) C (List.concat bodies) wrap C' F' made ->
    BindItem W P F C (Spec (Some pa) None None, None) wrap C'
             (carry_frame_allocations F F') made
(* [D78]: all output destinations are made here, so values of their
   types may not leave the block. *)
| BApplyAbstract : forall F C pa a,
    TypesApplication W F C pa a ->
    BindItem W P F C (Spec (Some pa) None None, None)
      (translate_application C a) (app_result a)
      (frame_after_allocation F (extend_allocation_scope (body_allocation_scope W F C) (app_allocated a)))
      (map (app_identity a) (app_outputs a))
(* [D78]/[D84] rule 4: `bind F = H;`, H abstract in force: H's inputs
   are among F's, and F's binding is H's *)
| BFunCopy : forall F C pa u pg h Cu nsu Ch nsh target sm fx rm,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some h ->
    (* only an abstract functor is bound (D78; the oracle's bind_functor,
       tests/errors/copy_into_body.moss) *)
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    nlook h (c_funs C) <> None ->
    FunCtx W u Cu nsu -> FunCtx W h Ch nsh ->
    (* Copying requires the target fixed inputs now and preserves every
       known fixed interpretation carried by the existing source binding. *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    (forall x, Fixed W h x -> Fixed W u x) ->
    FixedAgreement W C h ->
    SameResult W u h ->
    sm = map (fun k => (rkey_of Ch k, rkey_of Cu (target k))) nsh ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    (* H's inputs, implicit ones too, are among F's (the oracle's input
       interpretation check fails on one F lacks) *)
    (forall x tx, nlook x (c_ty Ch) = Some tx -> nlook x (c_ty Cu) <> None) ->
    (* and H reads its inputs, needs and result as F does at every
       application of F (the oracle: input and output interpretation,
       D78) *)
    CopyInterpretation W (map (fun q => (fst q, canon C (snd q))) fx) u h Cu nsu nsh Ch rm target ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (copy_functor_expression u h sm rm) (set_fun C u fx) F []
(* [D84] rule 3, `bind F = G;`, G with a body: a closure.  G's inputs
   that F takes (read through F's merge) are linked (supplied by each
   application), the rest are
   captured here (vals by value) and must be in force here; G's body is
   checked once, in its own terms, as the core functor body h (LinkBody,
   which also asks that G has a body).
   The closure keeps types as a lambda does (`lexical_capture`): the
   table's readings and the meanings of what the context mentions, method
   keys' receivers included, so a captured need keyed at a declared symbol
   not in force reads as it was provided (tests/errors/functor_captured_unbound.moss,
   family4-AUDIT.md). *)
| BFunClosure : forall F C pa u pg gg Cu nsu Cg nsg h pins target sm smc fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some gg ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    (* F's and G's contexts, as the world records them (LinkBody states
       that they are their FunCtx) *)
    w_context W u = Some (Cu, nsu) -> w_context W gg = Some (Cg, nsg) ->
    (* 3a: X(F) in force here *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u gg ->
    LinkBinding W u Cu nsu Cg nsg (map (fun q => (fst q, canon C (snd q))) fx) (link_private_readings C Cu Cg) (link_adopted_inputs C Cu Cg) target -> LinkBody W P u gg h pins ->
    (* the linked needs: those of G that are F's, read through F's merge *)
    sm = link_map Cu Cg target nsg ->
    (* 3b: the captured needs are answered here as a call would answer
       them, at G's inputs read here: provided, coherent, at G's view of
       their types, and G's merges holding of the meanings here *)
    Satisfies C Cg (map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg))
      (captured_needs target nsg) smc ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    (* rule B for the closure: G's inputs that F does not take are
       captured here, so each is in force here, through G's merge class
       (inputs F takes are linked at each application) *)
    (forall x, In x (map fst (c_ty Cg)) -> ~ In x (map fst (c_ty Cu)) -> InForceThrough C Cg x) ->
    (* Q6: each pin of a fixed input agrees with the bind site: the
       input's meaning here is the pin's, read with G's types as they are
       here (the oracle's link check, "it binds `Tok` to ..., and here
       `Tok` is ..."; tests/errors/pin_fixed_disagree.moss) *)
    (forall x tx, In (x, tx) pins -> exists here, nlook x (c_ty C) = Some here /\
       (forall y, In y (gens tx) -> nlook y (c_ty C) <> None) /\
       canon C here = canon C (app_subst (map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg)) tx)) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (bind_functor_expression u h (lexical_capture C) sm smc) (set_fun C u fx) F []

(* [D66] a fn provider: a defined fn (its needs answered here), or an
   abstract fn in force, through a fresh defined forwarder whose body calls
   it; its signature read here must be the bound fn's. *)
with FnProvider (W : world) (P : program) : frame -> cx -> aexpr -> fsig -> option ty -> nat -> type_capture -> smap -> Prop :=
| FPDefined : forall F C pa g sg Cg ns sm th,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists n ps rt b, ast_of W g = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W g Cg ns -> Satisfies C Cg th ns sm -> CallTheta W C Cg g th ->
    SigOf W g (c_ty C) None (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    RequirementsInForce C Cg ->
    FnProvider W P F C (AEPath pa None) sg None g (TypeCapture th th) sm
| FPForward : forall F C pa sg handle w,
    TypesForwarder W F C pa sg handle -> TranslatesForwarder W P handle sg w ->
    FnProvider W P F C (AEPath pa None) sg None w (TypeCapture [] [])
      [(IFn (forwarder_source handle), IFn (forwarder_source handle))]

(* a method provider (the receiver is the provider's first argument) *)
with MethProvider (W : world) (P : program) : frame -> cx -> aexpr -> fsig -> ty -> nat -> type_capture -> (expr -> expr) -> smap -> Prop :=
| MPAttached : forall F C rp n rg att rt sg Ca ns sm th rargs Cd,
    resolve_sym W (fr_mod F) rp = Some rg -> Attached W rg n att ->
    (exists rn ps r b, ast_of W att = Some (ADFn (RAttached rn) n ps r (Some b))) ->
    canon C rt = TNom rg rargs ->
    (* as at a direct call, the method is entered at the key's receiver:
       its requirements read as the receiver's arguments *)
    Cd = fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C ->
    CtxOf W att Ca ns -> Satisfies Cd Ca th ns sm -> CallTheta W Cd Ca att th ->
    SigOf W att (c_ty Cd) (Some (canon C rt))
      (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    RequirementsInForce Cd Ca ->
    MethProvider W P F C (AEField (AEPath rp None) n) sg rt att (TypeCapture th th) (fun x => x) sm
| MPWrapped : forall F C pa sg rt g sm w n Cg nsg types,
    FnProvider W P F C (AEPath pa None) sg None g types sm ->
    CtxOf W g Cg nsg -> n = List.length (sg_params sg) ->
    Fresh W w ->
    body_of P w = Some (ECall g (theta_ctx Cg) (map (fun k => (rkey_of Cg k, rkey_of Cg k)) nsg) (map EVar (seq 1 n))) ->
    MethProvider W P F C (AEPath pa None) sg rt w types (fun x => x) sm
| MPLambda : forall F C ps r body sg rt pid pre sm,
    LambdaProvider W P F C ps r body sg (Some rt) pid pre sm ->
    MethProvider W P F C (AELam ps r body) sg rt pid (lexical_capture C) pre sm

(* [D70] an anonymous function at a bind: its parameters are the bound
   signature's (annotations, if any, must agree); each local of the
   enclosing body it mentions is captured by value — a fresh val bound here
   to the local's value; the function is a fresh defined fn assuming
   everything in force here (with the captured vals), its body read with
   the captured names as those vals (`let x = v;` in front); its needs are
   answered by the same items here. *)
with LambdaProvider (W : world) (P : program) : frame -> cx -> list (name * option aty) -> option aty -> aexpr -> fsig -> option ty -> nat -> (expr -> expr) -> smap -> Prop :=
| LP : forall F C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm,
    List.length ps = List.length (sg_params sg) -> NoDup (map fst ps) ->
    Forall2 (fun pq t => match snd pq with
                         | Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ MergeEq (c_merge C) t' t
                         | None => True end) ps (sg_params sg) ->
    match rt with Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ MergeEq (c_merge C) t' (sg_ret sg)
                 | None => True end ->
    TypesLambdaCaptures W F C (map fst ps) body caps capvals ->
    Fresh W pid ->
    (* the provider's own context: everything here, the captured vals in
       force (as LCaptured locals: not assignable), no other locals *)
    Cl = fold_left (fun C' q => let '(n, v) := q in
                      match slook n (c_locals C) with
                      | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                      | None => C' end)
                   (combine caps capvals)
                   (Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) []) ->
    (* its frame: numbered afresh, parameters (after `this`) from 0 *)
    fr_mod Fl = fr_mod F -> fr_ret Fl = Some (sg_ret sg) -> fr_this Fl = this -> fr_loop Fl = false ->
    (* the generations in scope here stay taken in its body: the closure
       keeps their meanings, so no application there may make them anew *)
    incl (fr_fresh F ++ context_gens C) (fr_fresh Fl) ->
    Numbering Fl this (map fst ps) -> Forall (Named Fl) caps -> Forall (Named F) caps ->
    BlkE W P Fl (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
                           (combine (map fst ps) (sg_params sg)) Cl)
         (ABlock [] (Some body)) (Some (sg_ret sg)) lbody ltype ->
    fits ltype (sg_ret sg) ->
    body_of P pid = Some (translate_lambda_body Fl (combine caps capvals) lbody) ->
    (* its needs: every key in force here, each answered by itself *)
    (forall k, In k keys <-> (provides Cl k /\
       (match k with ITy _ => False | _ => True end) /\ k = provision_id k)) ->
    NoDup keys -> sm = map (fun k => (rkey_of Cl k, rkey_of C k)) keys ->
    nlook pid (lambda_sections (program_origins P)) = Some (c_sub C) ->
    LambdaProvider W P F C ps rt body sg this pid (translate_lambda_captures F (combine caps capvals)) sm

(* [D78]/[D84] 3e: G's result is F's, item by item: the same leaves
   (keys, as written, through contexts). *)

(* [D84] 3b–3e: G's binds checked once, in G's own terms — G's context
   formed from its own inputs, each output of F as itself — producing the
   core functor body h: its result telescope (F's output types, then F's
   result items), and its binds. 3d: pins may not rebind P(F) ∩ M(F) (Q6).
   3e: every item of F's result is made by the binds, at the types F
   promises. LinkBinding checks each binding of that body. *)
with LinkBody (W : world) (P : program) : nat -> nat -> nat -> subst -> Prop :=
| LB : forall u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 pins,
    w_decl W gg = Some dg -> d_ast dg = ADFunctor n ps a r (Some bodies) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    fr_mod Fg = d_mod dg -> fr_ret Fg = None ->
    fr_this Fg = None -> fr_loop Fg = false ->
    (* its locals are numbered as a body's are *)
    Numbering Fg None [] ->
    (* 3d, Q6: the protected inputs Π are the polymorphic types F's result
       mentions, P(F) ∩ M(F); a fixed input may be pinned (bound by the
       body), whether or not G takes it, and BFunClosure checks that the
       pin agrees with the bind site (AUDIT-CHANGES #95) *)
    (forall x, In x (fr_pins Fg) <-> Mentions W u x /\ Poly W u x) ->
    BindItems W P Fg Cg (List.concat bodies) wrap Cb Fb made ->
    (* no generation the body makes is an input of F: each application
       supplies F's inputs, and a body write would replace one under a
       key the caller files at its own reading (a kept output at an
       unmentioned merge representative, tests/errors/link_kept_output_rep.moss;
       scratch/orch/realize2-AUDIT.md) *)
    (forall t, In t made -> nlook t (c_ty Cu) = None) ->
    TypesLinkOutputs W u Cu Cb outs gen Cr ks ->
    (* a key the pins re-read must be one the body made, not one of G's
       own provisions (the oracle's and native's "does not bind", by
       written leaf; tests/errors/pin_fixed_key_need.moss, AUDIT-CHANGES #73) *)
    (forall k, In k ks ->
       at_site (link_site Cg Cb pins outs gen) k <> at_site (link_site Cg Cb [] outs gen) k ->
       ilook (provision_id (at_site (link_site Cg Cb pins outs gen) k)) (c_provisions Cg) = None) ->
    (* Every output has the promised signature and known type meanings. *)
    ResultRealizes (link_site Cg Cb pins outs gen) Cr
      (role_reads (link_site Cg Cb pins outs gen) outs gen ++ pin_reads Cb pins) ks ->
    (* h's body, as a run of binds, is what `wrap` puts around nothing *)
    BindsOf (wrap EUnit) binds0 ->
    (Family W u -> FamilyBindings W binds0) ->
    (* The caller installs each result under its own key for the same
       position. Emission uses the body's runtime key at that position. *)
    TranslatesLinkedBody W P u gg h Cu Cg (link_site Cg Cb pins outs gen) outs binds0 ks ->
    (* the pins of fixed inputs: what the body binds each to, in G's input
       terms (each bind reads its type where it is made, and a later bind
       writes only its own alias; tests/wasi/pin_taken_chain.moss).  A fixed
       input that G takes is pinned when the body leaves it at another
       reading than G's own (tests/wasi/pin_taken_one.moss, #95) *)
    (forall x tx, In (x, tx) pins <->
       Fixed W u x /\ nlook x (c_ty Cb) = Some tx /\
       (In x (map fst (c_ty Cg)) -> nlook x (c_ty Cg) <> Some tx)) ->
    (* a pin is a type over G's own inputs (each its own merge class),
       none of them an input of F that applications supply (as the oracle:
       "a type each application ... supplies") nor a generation the body
       makes *)
    (forall x tx y, In (x, tx) pins -> In y (gens tx) ->
       In y (map fst (c_ty Cg)) /\ lookup_subst y (c_sub Cg) = None /\ ~ In y made /\
       (In y (map fst (c_ty Cu)) -> Fixed W u y)) ->
    LinkBody W P u gg h pins.

(* A defined fn or method f: its body, under the context its assumes form
   (and, for an attached method, the receiver as `this` at its
   requirements' generations), with the parameters numbered from 0 (after
   `this`), elaborates to `body_of P f`. *)
Definition FnElab (W : world) (P : program) (f : nat) : Prop :=
  forall d rcv n ps rt b,
    w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  exists C ns this g F x t,
    CtxOf W f C ns /\
    FunctionReceiver W f rcv n C this /\
    SigOf W f (c_ty C) this g /\
    fr_mod F = d_mod d /\ fr_ret F = Some (sg_ret g) /\ fr_this F = this /\ fr_loop F = false /\
    fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F this (map fst ps) /\
    BlkE W P F (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine (map fst ps) (sg_params g)) C)
         b (Some (sg_ret g)) x t /\
    body_of P f = Some x.

(* A defined val's initializer elaborates as ExConst reads it at a use: no
   assumptions, its type read in an empty context, in an empty frame. *)
Definition ConstElab (W : world) (P : program) (d : decl) (t0 : aty) (i : aexpr) : Prop :=
  d_assumes d = [] /\ exists t, TyOf W (d_mod d) [] None t0 t /\
    exists F0 x t', Ex W P F0 (Cx [] [] [] [] []) i (Some t) x t' /\ fits t' t /\
      fr_mod F0 = d_mod d /\ fr_ret F0 = None /\ fr_this F0 = None /\ fr_loop F0 = false /\
      fr_fresh F0 = [] /\ fr_applying F0 = [] /\ fr_pins F0 = [] /\ Numbering F0 None [].

Definition DeclForms (W : world) (P : program) : nat -> Prop :=
  DeclFormsWith W (ConstElab W P).

Definition DeclsForm (W : world) (P : program) : Prop := forall s, DeclForms W P s.

Definition LegacyElaboratesIn (b : bundle) (W : world) (P : program) : Prop :=
  ElaboratesWith b W P (FnElab W P) (ConstElab W P).
