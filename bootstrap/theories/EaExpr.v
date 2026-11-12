(* ------------------------------------------------------------------------ *)
(* EaExpr.v — expressions and blocks (Audit V.6): Ex, Args, Cond, Branch,  *)
(* Arms, ArmsList, MethCall, Provided, BlkE, Blk; PatOf, FieldPats;        *)
(* Literal, LitOp.                                                          *)
(*                                                                           *)
(* Rule selection is deterministic: Audit's rules are told apart by their  *)
(* side conditions, which are decided here in the same order.  Bind        *)
(* statements, constants (ExConst) and D89's expansion (ExCallBr) go       *)
(* through the open-recursion record R, one call level lower.              *)
(* ------------------------------------------------------------------------ *)

From Stdlib Require Import List String Bool Arith.
From NM Require AuditBase.
From NM Require Import Audit EaBase EaTypes EaCall EaFamily EaDecide.
Import ListNotations.
Import Audit.Input AuditBase.Semantics AuditBase.Semantics.Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Section Expr.
Variable E : env.
Let W := e_W E.
Let fuel := e_fuel E.
Variable R : rec.

Definition need_named (F : frame) (n : name) : M unit :=
  check (is_some (slook n (fr_var F))) ("local `" ++ n ++ "` is not numbered").

(* the parameter names of a fn or method symbol *)
Definition param_names (s : nat) : list name :=
  match ast_of W s with Some (ADFn _ _ ps _ _) => map fst ps | _ => [] end.
Definition tag_name (s : nat) : string := bq (sym_name W s).
(* `join`, with the old oracle's report *)
Definition join_m (a b : ty) (exp : option ty) : M ty :=
  match join a b exp with
  | Ok t => ret t
  | Err _ => fail ("branches disagree: " ++ qty W a ++ " vs " ++ qty W b ++ " (annotate an expected type)")
  end.

(* ---- Literals ([D68]) ---- *)

(* SOUND: lit_op C t m ox oy = Ok x -> LitOp W C t m ox oy x *)
Definition lit_sig_msg (m : nat) : string := "a literal's " ++ bq (mname W m) ++ " must take and return its own type (D68)".
Definition lit_op (C : cx) (t : ty) (m : nat) (ox oy : expr) : er expr :=
  match cmth C t m with
  | Some g => if fsig_eqb g (FSig [t] t) then Ok (EMethod ox m (rk C t m) [oy])
              else Err (lit_sig_msg m)
  | None =>
      let! (((((u, mt), th), sm), C'), ks) := family_for E C C t m in
      match cmth C' t m with
      | Some g => if fsig_eqb g (FSig [t] t)
                  then Ok (apply_expression u th sm (map (rkof C') ks) (EMethod ox m (rk C' t m) [oy]))
                  else Err (lit_sig_msg m)
      | None => Err (no_method_msg E C' t m)
      end
  end.

(* SOUND: literal C tr = Ok (x, t) -> Literal W C tr x t *)
Fixpoint literal (why : string) (C : cx) (tr : literal_tree) : er (expr * ty) :=
  let bin a b op :=
    let! (x, t) := literal why C a in
    let! (y, t') := literal why C b in
    let! _ := rcheck (ty_eqb t t') (why ++ " reads at two types, " ++ qty W t ++ " and " ++ qty W t') in
    let! m := ropt (lang_sym W "num" NSDetached op) (why ++ " means `." ++ op ++ "` of lib/num.moss, which is not loaded here") in
    let! r := lit_op C t m x y in Ok (r, t) in
  match tr with
  | LZero => let! z := ropt (lang_sym W "int" NSName "zero") (why ++ " means `zero` of lib/int.moss, which is not loaded here") in
             let! t := ropt (cval C z) (why ++ " means `zero`, which is not available in the context here (assume or bind it)") in Ok (EVal z, t)
  | LOne => let! o := ropt (lang_sym W "int" NSName "one") (why ++ " means `one` of lib/int.moss, which is not loaded here") in
            let! t := ropt (cval C o) (why ++ " means `one`, which is not available in the context here (assume or bind it)") in Ok (EVal o, t)
  | LAdd a b => bin a b "add"
  | LMul a b => bin a b "mul"
  | LDiv a b => bin a b "div"
  | LFloat a =>
      let! (x, ti) := literal why C a in
      let! f := ropt (lang_sym W "float" NSName "float") (why ++ " means `float` of lib/float.moss, which is not loaded here") in
      match cfn C f with
      | Some (FSig [tp] t) => let! _ := rcheck (fitsb ti tp) ("argument of `float` is " ++ qty W ti ++ ", expected " ++ qty W tp) in Ok (EInvoke f [x], t)
      | Some _ => Err (why ++ " means `float`, which takes one argument")
      | None => Err (why ++ " means `float`, which is not available in the context here (assume or bind it)")
      end
  end.

(* ---- Patterns ---- *)

Definition member_with_head (st : ty) (s : nat) : option (list (nat * ty)) :=
  match find (fun m => match m with TNom s' _ => Nat.eqb s s' | _ => false end) (members st) with
  | Some (TNom _ xs) => Some xs
  | _ => None
  end.

(* SOUND: field_pats F ftys fs = Ok (bs, xs) -> FieldPats W F ftys fs bs xs *)
Fixpoint field_pats (F : frame) (g : nat) (ftys : list (fld * ty)) (fs : list (name * option apat)) : er (list (name * ty) * list (fld * var)) :=
  match fs with
  | [] => Ok ([], [])
  | (n, op) :: r =>
      let! t := ropt (slook n ftys) (tag_name g ++ " has no field `" ++ n ++ "`") in
      let! (bs, xs) := field_pats F g ftys r in
      match op with
      | None => let! _ := rcheck (is_some (slook n (fr_var F))) "internal: a pattern binder is not numbered" in
                Ok ((n, t) :: bs, (n, var_of F n) :: xs)
      | Some APWild => Ok (bs, xs)
      | Some (APPath [y]) =>
          let! _ := rcheck (negb (is_some (resolve W (fr_mod F) [y]))) "a pattern binder is a declared name" in
          let! _ := rcheck (is_some (slook y (fr_var F))) "internal: a pattern binder is not numbered" in
          Ok ((y, t) :: bs, (n, var_of F y) :: xs)
      | _ => Err "v0 record patterns support binders only"
      end
  end.

(* SOUND: pat_of F st p = Ok (xp, binds, hs, catch) -> PatOf W F st p xp binds hs catch *)
Definition pat_of (F : frame) (st : ty) (p : apat) : er (pattern * list (name * ty) * list nat * bool) :=
  let tag_of pa := let! s := ropt (resolve_sym W (fr_mod F) pa) (bq (show_path pa) ++ " is not a tag") in
                   let! _ := rcheck (kind_is W s DKTag) (bq (show_path pa) ++ " is not a matchable pattern") in
                   let! xs := ropt (member_with_head st s) (tag_name s ++ " is not a member of " ++ qty W st) in
                   Ok (s, xs) in
  match p with
  | APWild => Ok (PAny None, [], [], true)
  | APPath [n] =>
      match resolve W (fr_mod F) [n] with
      | None => let! _ := rcheck (is_some (slook n (fr_var F))) "internal: a pattern binder is not numbered" in
                Ok (PAny (Some (var_of F n)), [(n, st)], [], true)
      | Some _ =>
          let! (s, _) := tag_of [n] in
          Ok (PHead s None [], [], [s], false)
      end
  | APPath pa =>
      let! (s, _) := tag_of pa in
      Ok (PHead s None [], [], [s], false)
  | APTag pa (APPath [y]) =>
      let! (s, xs) := tag_of pa in
      let! _ := rcheck (negb (bare_tagb W s)) (bq (show_path pa) ++ " has no explicit payload") in
      let! pay := payload_of W fuel s xs in
      let! _ := rcheck (negb (is_some (resolve W (fr_mod F) [y]))) "a pattern binder is a declared name" in
      let! _ := rcheck (is_some (slook y (fr_var F))) "internal: a pattern binder is not numbered" in
      Ok (PHead s (Some (var_of F y)) [], [(y, pay)], [s], false)
  | APRec (Some pa) fs =>
      let! (s, xs) := tag_of pa in
      let! _ := rcheck (negb (bare_tagb W s)) (bq (show_path pa) ++ " has no explicit payload") in
      let! pay := payload_of W fuel s xs in
      match pay with
      | TRecord ftys =>
          let! (bs, fxs) := field_pats F s ftys fs in
          let! _ := rcheck (snodupb (map fst bs)) "a pattern binds a name twice" in
          Ok (PHead s None fxs, bs, [s], false)
      | _ => Err (tag_name s ++ " does not have a record payload")
      end
  | APRec None _ => Err "v0 record patterns need a tag head"
  | _ => Err "v0 supports only a binder as a tag pattern payload"
  end.

Definition add_binds (C : cx) (binds : list (name * ty)) : cx :=
  fold_left (fun C q => add_local C (fst q) (snd q) LLet) binds C.

Definition constant_frame (d : decl) (initial : aexpr) : frame :=
  Frame (d_mod d) None None false
    (mk_frame_vars false [] (ex_names W fuel (d_mod d) initial)) [] [] [].

Definition read_constant (s : nat) (d : decl) (t0 : aty) (i : aexpr) : M (expr * ty) :=
  let* cached := get_const s in
  match cached with
  | Some xt => ret xt
  | None =>
      let* _ := check (match d_assumes d with [] => true | _ => false end) ("the constant " ++ qs W s ++ " assumes items; a constant's initializer reads only constants") in
      let* t := pure (ty_of W fuel (d_mod d) [] None t0) in
      let F0 := constant_frame d i in
      let* (x, t') := r_ex R F0 (Cx [] [] [] [] []) i (Some t) in
      let* _ := check (fitsb t' t) (qs W s ++ " is declared " ++ qty W t ++ " but initialized with " ++ qty W t') in
      let* _ := put_const s (x, t) in
      ret (x, t)
  end.

Definition read_global_value (F : frame) (C : cx) (pa : path) : M (expr * ty) :=
  let* s := lift (resolve_sym W (fr_mod F) pa) (bq (show_path pa) ++ " is not in scope") in
  let* d := lift (w_decl W s) "internal: unknown symbol" in
  match d_ast d with
  | ADBareTag _ => ret (ETag s [] EUnit, TNom s [])
  | ADVal _ _ None =>
      let* t := lift (cval C s) (qs W s ++ " is not available in the context here (assume or bind it)") in ret (EVal s, t)
  | ADVal _ t0 (Some i) =>
      read_constant s d t0 i
  | ADFn _ _ _ _ _ => fail (qs W s ++ " is a function; call it (no first-class fns yet)")
  | ADTag _ _ _ => fail (qs W s ++ " is a tag; construct it with a payload")
  | _ => fail (qs W s ++ " is not a value")
  end.

(* Resolve the call's types and provisions before emitting an instruction.
   The argument expressions are elaborated in the caller's original scope. *)
Record prepared_call := PreparedCall {
  prepared_signature : fsig;
  prepared_types : subst;
  prepared_provisions : smap;
  prepared_demands : wrapper
}.

Definition check_defined_call (symbol : nat) (C : cx) (this : option ty) : er prepared_call :=
  let who := ("calling " ++ qs W symbol)%string in
  let! (formal, needs) := ropt (w_context W symbol) ("the context of " ++ qs W symbol ++ " does not form") in
  let! _ := rule_b W who C formal in
  let! initial := call_theta W who C formal in
  let wanted := map (at_th C initial) needs in
  let! (wrap, called) := demand E C wanted in
  let! types := call_theta W who called formal in
  let! _ := rcheck (ilist_eqb wanted (map (at_th C types) needs))
    (who ++ ": the keys it needs read otherwise once the families it demands are applied (rule B)") in
  let! provisions := satisfies W who who called formal types needs in
  let! signature := sig_of W fuel symbol (c_ty called) this in
  Ok (PreparedCall signature types provisions wrap).

Definition emit_defined_call (symbol : nat) (call : prepared_call) (args : list expr) : expr :=
  prepared_demands call (ECall symbol (prepared_types call) (prepared_provisions call) args).

(* ---- The main recursion ---- *)

(* SOUND (given RecSound E P R):
   ex n F C e exp = Ok (x, t) -> Ex W P F C e exp x t;
   args_ .. -> Args; cond -> Cond; branch -> Branch; arms -> Arms;
   arms_list -> ArmsList; meth_call -> MethCall; provided -> Provided;
   blke -> BlkE; blk -> Blk. *)
Definition conditional_tags : er (nat * nat * nat) :=
  let! bool_tag := ropt (lang_sym W "bool" NSName "Bool") "the library's Bool is unavailable" in
  let! true_tag := ropt (lang_sym W "bool" NSName "True") "the library's True is unavailable" in
  let! false_tag := ropt (lang_sym W "bool" NSName "False") "the library's False is unavailable" in
  Ok (bool_tag, true_tag, false_tag).

Fixpoint ex (n : nat) (F : frame) (C : cx) (e : aexpr) (exp : option ty) {struct n} : M (expr * ty) :=
  match n with
  | 0 => fail "expression: too deep"
  | S n' =>
  match e with
  | AEUnit => ret (EUnit, TUnit)
  | AEThis => let* t := lift (fr_this F) "`this` is only legal inside method bodies" in ret (EVar 0, t)
  | AEPath pa None =>
      let local := match pa with [x] => slook x (c_locals C) | _ => None end in
      match local, pa with
      | Some (t, _), [x] => let* _ := need_named F x in ret (EVar (var_of F x), t)
      | _, _ =>
          read_global_value F C pa
      end
  | AEPath pa (Some _) => fail (bq (show_path pa ++ "[…]") ++ ": only a type or a tag constructor takes brackets")
  | AECall pa a argl =>
      let* s := lift (resolve_sym W (fr_mod F) pa) (bq (show_path pa) ++ " is not in scope") in
      let* d := lift (w_decl W s) "internal: unknown symbol" in
      match d_ast d with
      | ADTag _ _ _ =>
          let* bs := pure (brackets W fuel (fr_mod F) (c_ty C) s a) in
          let* xs := pure (rmap (fun r => let! t := ropt (nlook r (bs ++ c_ty C))
                                                ("type " ++ bq (sym_name W r) ++ " (required by " ++ qs W s ++
                                                 ") is not available in the context here") in Ok (r, t)) (w_reqs W s)) in
          let* pay := pure (payload_of W fuel s xs) in
          match argl with
          | [y] => let* (v, vt) := ex n' F C y (Some pay) in
                   let* _ := check (fitsb vt pay) ("payload of " ++ qs W s ++ " is " ++ qty W vt ++ ", expected " ++ qty W pay) in
                   ret (ETag s xs v, TNom s xs)
          | _ => fail ("tag " ++ qs W s ++ " takes exactly one payload")
          end
      | ADFn RPlain _ _ _ body =>
          match a with
          | Some bs =>
              let* _ := miter (fun b =>
                         match resolve_sym W (fr_mod F) (fst b) with
                         | None => fail (bq (show_path (fst b)) ++ " is not in scope")
                         | Some x =>
                             if kind_is W x DKType && nmem x (w_reqs W s) then
                               match snd b with
                               | Spec (Some _) None _ => ret tt
                               | _ => fail ("a bracket application on a call of " ++ qs W s ++ " binds " ++ qs W x ++
                                            " to a method; it binds a type to a type (D89)")
                               end
                             else fail ("a bracket application on a call of " ++ qs W s ++ " binds " ++ qs W x ++
                                        ", which is not a type " ++ qs W s ++ " requires (D22, D89)")
                         end) bs in
              r_ex R F C (Syntax.call_block pa bs argl) None
          | None =>
              match body with
              | Some _ =>
                  let* call := pure (check_defined_call s C None) in
                  let g := prepared_signature call in
                  (* Q4 (#72): the arguments in the caller's context *)
                  let* xs := args n' F C (dname W s) (param_names s) argl (sg_params g) in
                  ret (emit_defined_call s call xs, sg_ret g)
              | None =>
                  let* g := lift (cfn C s) (qs W s ++ " is not available in the context here (assume or bind it)") in
                  let* xs := args n' F C (dname W s) (param_names s) argl (sg_params g) in
                  ret (EInvoke s xs, sg_ret g)
              end
          end
      | _ => fail (qs W s ++ " is not callable")
      end
  | AEMeth o pa argl =>
      let* (ox, ot) := ex n' F C o None in
      meth_call n' F C ox (canon C ot) pa argl
  | AEUnary op y =>
      let* (lib, nm) := lift (operator_method op true) "unknown operator" in
      let* m := lift (lang_sym W lib NSDetached nm) ("operator " ++ bq nm ++ " needs lib/" ++ lib ++ ".moss to be loaded") in
      let* (ox, ot) := ex n' F C y None in
      let* _ := check (is_some (head (canon C ot))) (qty W (canon C ot) ++ " has no methods (it has no nominal head)") in
      provided n' F C ox (canon C ot) m []
  | AEBinary op l r =>
      let* (lib, nm) := lift (operator_method op false) "unknown operator" in
      let* m := lift (lang_sym W lib NSDetached nm) ("operator " ++ bq nm ++ " needs lib/" ++ lib ++ ".moss to be loaded") in
      let* (ox, ot) := ex n' F C l None in
      let* _ := check (is_some (head (canon C ot))) (qty W (canon C ot) ++ " has no methods (it has no nominal head)") in
      provided n' F C ox (canon C ot) m [r]
  | AELit text =>
      let* _ := check (numeric_text text) "not a numeric literal" in
      pure (literal ("the literal `" ++ text ++ "`") C (literal_tree_of text))
  | AEIf c b None =>
      let* cx' := cond n' F C c in
      let* (bx, bt) := blke n' F C b exp in
      let* _ := check (opt_fitsb TUnit exp) "an `if` without `else` yields `()`" in
      let* _ := check (ty_eqb bt TUnit || ty_eqb bt never)
                 ("an `if` without `else` yields `()`, but its branch yields " ++ qty W bt) in
      let* (bool_tag, true_tag, false_tag) := pure conditional_tags in
      ret (conditional_expression (var_of F conditional_payload_name) bool_tag true_tag false_tag cx' bx EUnit, TUnit)
  | AEIf c b (Some y) =>
      let* cx' := cond n' F C c in
      let* (bx, bt) := blke n' F C b exp in
      let* (ex', et) := branch n' F C y exp in
      let* _ := check (opt_fitsb et exp)
                 ("the `else` branch yields " ++ qty W et ++ match exp with Some x => ", expected " ++ qty W x | None => "" end) in
      let* t := join_m (branch_ty b bt) et exp in
      let* (bool_tag, true_tag, false_tag) := pure conditional_tags in
      ret (conditional_expression (var_of F conditional_payload_name) bool_tag true_tag false_tag cx' bx ex', t)
  | AEMatch s arms0 =>
      let* (sx, st) := ex n' F C s None in
      let* (xarms, t) := arms n' F C (canon C st) arms0 exp in
      ret (EMatch sx xarms, t)
  | AEBlock b =>
      let* (x, t) := blke n' F C b exp in
      ret (x, match exp with Some e => e | None => t end)
  | AEReturn None =>
      let* _ := check (oty_eqb (fr_ret F) (Some TUnit))
                 match fr_ret F with
                 | Some r => "`return` with no value, but the function returns " ++ qty W r
                 | None => "a constant initializer or a bound functor body cannot return from its caller" end in
      ret (EReturn EUnit, never)
  | AEReturn (Some y) =>
      let* expected := lift (fr_ret F) "a constant initializer or a bound functor body cannot return from its caller" in
      let* (v, actual) := ex n' F C y (Some expected) in
      let* _ := check (fitsb actual expected) ("return of " ++ qty W actual ++ ", expected " ++ qty W expected) in
      ret (EReturn v, never)
  | AEBreak => let* _ := check (fr_loop F) "`break` outside of a loop" in ret (EBreak, never)
  | AERec pa a fs =>
      let* s := lift (resolve_sym W (fr_mod F) pa) (bq (show_path pa) ++ " is not a tag with a record payload") in
      let* _ := check (kind_is W s DKTag && negb (bare_tagb W s)) (bq (show_path pa) ++ " is not a tag with a record payload") in
      let* bs := pure (brackets W fuel (fr_mod F) (c_ty C) s a) in
      let* xs := pure (rmap (fun r => let! t := ropt (nlook r (bs ++ c_ty C))
                                            ("type " ++ bq (sym_name W r) ++ " (required by " ++ qs W s ++
                                             ") is not available in the context here") in Ok (r, t)) (w_reqs W s)) in
      let* pay := pure (payload_of W fuel s xs) in
      match pay with
      | TRecord ftys =>
          let* _ := match find (fun q => negb (is_some (slook (fst q) ftys))) fs with
                    | Some (f, _) => fail (qs W s ++ " has no field `" ++ f ++ "`")
                    | None => ret tt end in
          let* _ := match suniq_dup (map fst fs) with
                    | Some f => fail ("duplicate field `" ++ f ++ "`")
                    | None => ret tt end in
          let* _ := match filter (fun q => negb (existsb (fun q' => String.eqb (fst q') (fst q)) fs)) ftys with
                    | [] => ret tt
                    | missing => fail ("missing field(s) " ++ String.concat ", " (map fst missing) ++ " of " ++ qs W s)
                    end in
          let* _ := check (Nat.eqb (List.length fs) (List.length ftys)) ("the fields of " ++ qs W s) in
          let* xfs := mmap_fields n' F C fs ftys in
          ret (ETag s xs (ERecord xfs), TNom s xs)
      | _ => fail (qs W s ++ " does not have a record payload")
      end
  | AEBareRecord fs =>
      let* _ := match suniq_dup (map fst fs) with
                | Some f => fail ("duplicate field `" ++ f ++ "`")
                | None => ret tt end in
      match exp with
      | None =>
          let* (ftys, xfs) := synth_fields n' F C fs in
          ret (ERecord xfs, TRecord ftys)
      | Some (TRecord ftys) =>
          let* _ := match suniq_dup (map fst ftys) with
                    | Some f => fail ("duplicate expected field `" ++ f ++ "`")
                    | None => ret tt end in
          let* _ := check (Nat.eqb (List.length fs) (List.length ftys)) "wrong number of record fields" in
          let* xfs := mmap_fields n' F C fs ftys in
          ret (ERecord xfs, TRecord ftys)
      | Some _ => fail "a record literal needs a record type"
      end
  | AEField o nm =>
      let* (ox, ot) := ex n' F C o None in
      let* rc := match ot with
                 | TNom s xs => if kind_is W s DKTag then pure (payload_of W fuel s xs) else ret ot
                 | _ => ret ot end in
      match rc with
      | TRecord ftys => let* t := lift (slook nm ftys) (qty W ot ++ " has no field `" ++ nm ++ "`") in ret (EField ox nm, t)
      | _ => fail (qty W ot ++ " has no fields")
      end
  | AELam _ _ _ => fail "an anonymous function may only be the right side of a fn or method bind (D70)"
  end
  end
with mmap_fields (n : nat) (F : frame) (C : cx) (fs : list (name * option aexpr)) (ftys : list (fld * ty)) {struct n}
  : M (list (fld * expr)) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match ftys with
      | [] => ret []
      | (f, ft) :: r =>
          let* v := lift (match find (fun q => String.eqb (fst q) f) fs with Some q => Some (snd q) | None => None end)
                         ("missing field(s) " ++ f) in
          let* (x, vt) := ex n' F C (match v with Some y => y | None => AEPath [f] None end) (Some ft) in
          let* _ := check (fitsb vt ft) ("field `" ++ f ++ "` is " ++ qty W vt ++ ", expected " ++ qty W ft) in
          let* rest := mmap_fields n' F C fs r in
          ret ((f, x) :: rest)
      end
  end
with synth_fields (n : nat) (F : frame) (C : cx) (fs : list (name * option aexpr)) {struct n}
    : M (list (fld * ty) * list (fld * expr)) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match fs with
      | [] => ret ([], [])
      | (f, syntax) :: rest =>
          let* (value, actual) := ex n' F C (record_field_source f syntax) None in
          let* (types, values) := synth_fields n' F C rest in
          ret ((f, actual) :: types, (f, value) :: values)
      end
  end
with args (n : nat) (F : frame) (C : cx) (who : string) (pns : list name) (l : list aexpr) (ts : list ty) {struct n} : M (list expr) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      if negb (Nat.eqb (List.length l) (List.length ts)) then
        fail (bq who ++ " takes " ++ nat_str (List.length ts) ++ " argument(s), got " ++ nat_str (List.length l))
      else
      match l, ts with
      | [], [] => ret []
      | a :: r, t :: ts' =>
          let* (x, at') := ex n' F C a (Some t) in
          let pn := match pns with p :: _ => p | [] => "?" end in
          let* _ := check (fitsb (canon C at') (canon C t))
                     ("argument `" ++ pn ++ "` of " ++ bq who ++ " is " ++ qty W (canon C at') ++ ", expected " ++ qty W (canon C t)) in
          let* xs := args n' F C who (tl pns) r ts' in
          ret (x :: xs)
      | _, _ => fail (bq who ++ " takes " ++ nat_str (List.length ts) ++ " argument(s), got " ++ nat_str (List.length l))
      end
  end
with cond (n : nat) (F : frame) (C : cx) (c : aexpr) {struct n} : M expr :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      let* b := lift (lang_sym W "bool" NSName "Bool") "`if`/`while` need lib/bool.moss to be loaded (D45)" in
      let* (x, t) := ex n' F C c (Some (TNom b [])) in
      let* _ := check (fitsb t (TNom b [])) ("a condition is " ++ qty W t ++ ", expected `Bool`") in
      ret x
  end
with branch (n : nat) (F : frame) (C : cx) (y : aexpr) (exp : option ty) {struct n} : M (expr * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match y with
      | AEBlock b => let* (x, t) := blke n' F C b exp in
                     ret (x, match exp with Some e => e | None => branch_ty b t end)
      | _ => ex n' F C y exp
      end
  end
with arms (n : nat) (F : frame) (C : cx) (st : ty) (l : list (apat * aexpr)) (exp : option ty) {struct n}
  : M (list (pattern * expr) * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      if ty_eqb st never then
        match l with
        | [] => ret ([], match exp with Some e => e | None => never end)
        | _ => fail "a match on `|` takes no arms"
        end
      else
        let* _ := check (negb (is_nil l))
                   ("match is not exhaustive; missing " ++
                    match members st with [] => sty W st | ms => String.concat ", " (map (sty W) ms) end) in
        let* (((xarms, covered), catch), t) := arms_list n' F C st l exp in
        let missing := filter (fun m => match head m with Some h => negb (nmem h covered) | None => false end) (members st) in
        let* _ := check (catch || (negb (is_nil (members st)) && is_nil missing))
                   ("match is not exhaustive; missing " ++
                    match missing with [] => sty W st | _ => String.concat ", " (map (sty W) missing) end) in
        ret (xarms, t)
  end
with arms_list (n : nat) (F : frame) (C : cx) (st : ty) (l : list (apat * aexpr)) (exp : option ty) {struct n}
  : M (list (pattern * expr) * list nat * bool * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match l with
      | [] => fail "a match takes at least one arm"
      | (pat, body) :: rest =>
          let* (((xp, binds), hs), ca) := pure (pat_of F st pat) in
          let* (bx, bt) := branch n' F (add_binds C binds) body exp in
          let* _ := check (opt_fitsb bt exp)
                     ("match arm yields " ++ qty W bt ++ match exp with Some x => ", expected " ++ qty W x | None => "" end) in
          match rest with
          | [] => ret ([(xp, bx)], hs, ca, bt)
          | _ =>
              let* (((xr, hr), cr), tr) := arms_list n' F C st rest exp in
              let* t := join_m bt tr exp in
              ret ((xp, bx) :: xr, hs ++ hr, ca || cr, t)
          end
      end
  end
with meth_call (n : nat) (F : frame) (C : cx) (ox : expr) (ot : ty) (pa : path) (argl : list aexpr) {struct n} : M (expr * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match pa with
      | [nm] =>
          let det := match w_scope W (fr_mod F) NSDetached nm with Some (ESym m) => Some m | _ => None end in
          let att := match head ot with Some h => w_attached W h nm | None => None end in
          match att with
          | Some at_ =>
              let det_provided := match det with Some m => is_some (cmth C ot m) | None => false end in
              if det_provided then
                match det with Some m => provided n' F C ox ot m argl | None => fail "impossible" end
              else
                let* d := lift (w_decl W at_) "internal: unknown method" in
                match d_ast d with
                | ADFn (RAttached _) _ _ _ (Some _) =>
                    let rargs := match ot with TNom _ xs => xs | _ => [] end in
                    let C1 := fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C in
                    let* call := pure (check_defined_call at_ C1 (Some ot)) in
                    let g := prepared_signature call in
                    let* xs := args n' F C (dname W at_) (param_names at_) argl (sg_params g) in
                    ret (emit_defined_call at_ call (ox :: xs), sg_ret g)
                | ADFn (RAttached _) _ _ _ None => provided n' F C ox ot at_ argl
                | _ => fail ("internal: " ++ qs W at_ ++ " is not an attached method")
                end
          | None =>
              match det with
              | Some m => provided n' F C ox ot m argl
              | None =>
                  match head ot with
                  | Some _ => fail ("`." ++ nm ++ "` is not a detached method in scope; import it, or call it as `mod::m` (D61)")
                  | None => fail (qty W ot ++ " has no methods (it has no nominal head)")
                  end
              end
          end
      | _ =>
          let* m := lift (resolve_detached W (fr_mod F) pa) (bq (show_path pa) ++ " is not a detached method in scope") in
          provided n' F C ox ot m argl
      end
  end
with provided (n : nat) (F : frame) (C : cx) (ox : expr) (ot : ty) (m : nat) (argl : list aexpr) {struct n} : M (expr * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match cmth C ot m with
      | Some g =>
          let* xs := args n' F C (dname W m) (param_names m) argl (sg_params g) in
          ret (EMethod ox m (rk C ot m) xs, sg_ret g)
      | None =>
          let* (((((u, mt), th), sm), C'), ks) := pure (family_for E C C ot m) in
          let* g := lift (cmth C' ot m) (bq (sym_name W u) ++ " does not provide " ++ bq (sty W ot ++ mname W m) ++ " here (D84 rule 7)") in
          let* xs := args n' F C (dname W m) (param_names m) argl (sg_params g) in
          ret (apply_expression u th sm (map (rkof C') ks) (EMethod ox m (rk C' ot m) xs), sg_ret g)
      end
  end
with blke (n : nat) (F : frame) (C : cx) (b : ablock) (exp : option ty) {struct n} : M (expr * ty) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match b with
      | ABlock ss tl =>
          let* ((x, t), made) := blk n' (without_pins F) C ss tl exp in
          let* _ := match find (fun g => nmem g made) (gens t) with
                    | Some g => fail ("this block yields " ++ qty W t ++ ", which mentions " ++ qs W g ++
                                      ", a type `bind F;` made in it: a value of that type cannot leave the block (D78)")
                    | None => ret tt end in
          let* _ := check (opt_fitsb t exp)
                     ("the block yields " ++ qty W t ++ match exp with Some x => ", expected " ++ qty W x | None => "" end) in
          ret (x, t)
      end
  end
with blk (n : nat) (F : frame) (C : cx) (ss : list astmt) (tl : option aexpr) (exp : option ty) {struct n}
  : M (expr * ty * list nat) :=
  match n with
  | 0 => fail "too deep"
  | S n' =>
      match ss with
      | [] =>
          match tl with
          | Some y => let* (x, t) := ex n' F C y exp in ret (x, t, [])
          | None => ret (EUnit, TUnit, [])
          end
      | st :: rest =>
          match st with
          | ASLet x ann y | ASVar x ann y =>
              let mu := match st with ASVar _ _ _ => LVar | _ => LLet end in
              let* declared := match ann with
                               | Some a => let* d := pure (ty_of W fuel (fr_mod F) (c_ty C) (fr_this F) a) in ret (Some d)
                               | None => ret None end in
              let* (v, vt) := ex n' F C y declared in
              let* t0 := match declared with
                         | Some d => let* _ := check (fitsb vt d) ("`" ++ x ++ "` is declared " ++ qty W d ++ " but initialized with " ++ qty W vt) in ret d
                         | None => ret vt end in
              let* _ := need_named F x in
              let* ((xr, t), made) := blk n' F (add_local C x t0 mu) rest tl exp in
              ret (ELet (var_of F x) v xr, t, made)
          | ASAssign x y =>
              match slook x (c_locals C) with
              | Some (tv, LVar) =>
                  let* (v, vt) := ex n' F C y (Some tv) in
                  let* _ := check (fitsb vt tv) ("cannot assign " ++ qty W vt ++ " to `" ++ x ++ ": " ++ sty W tv ++ "`") in
                  let* ((xr, t), made) := blk n' F C rest tl exp in
                  let* _ := need_named F x in
                  ret (ESeq (EAssign (var_of F x) v) xr, t, made)
              | Some (_, LCaptured) => fail ("`" ++ x ++ "` is a local captured by an anonymous function, which captures it by value and may not assign it (D70)")
              | Some _ => fail ("`" ++ x ++ "` is not a `var`")
              | None => fail ("`" ++ x ++ "` is not in scope")
              end
          | ASExpr y =>
              match rest, tl with
              | [], None =>
                  let* (v, vt) := ex n' F C y None in
                  ret (ESeq v EUnit, if ty_eqb vt never then never else TUnit, [])
              | _, _ =>
                  let* (v, vt) := ex n' F C y None in
                  let* ((xr, t), made) := blk n' F C rest tl exp in
                  ret (ESeq v xr, t, made)
              end
          | ASWhile c b =>
              let* cx' := cond n' F C c in
              let* (bx, _) := blke n' (with_loop F) C b None in
              let* ((xr, t), made) := blk n' F C rest tl exp in
              ret (ESeq (EWhile cx' bx) xr, t, made)
          | ASLoop b =>
              let falls_through := match rest, tl with
                | [], None => block_breaks W fuel (fr_mod F) b
                | _, _ => Some true
                end in
              match falls_through with
              | None => fail "too deep"
              | Some more =>
                  let* (bx, _) := blke n' (with_loop F) C b None in
                  if more then
                    let* ((xr, t), made) := blk n' F C rest tl exp in
                    ret (ESeq (ELoop bx) xr, t, made)
                  else ret (ESeq (ELoop bx) EUnit, never, [])
              end
          | ASBind its =>
              let* (((wrap, C'), F'), made) := r_bind_items R F C its in
              let* ((xr, t), made') := blk n' F' C' rest tl exp in
              ret (wrap xr, t, made ++ made')
          end
      end
  end.

End Expr.
