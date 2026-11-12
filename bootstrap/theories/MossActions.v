(* Semantic actions for MossGrammar.vy. The grammar consumes tokens.
   These functions validate the shapes it has already recognized and
   construct the surface AST. *)
From Stdlib Require Import String List Bool.
From NM Require Import Surface.
Import ListNotations Input Syntax.
Local Open Scope string_scope.

Inductive checked (A : Type) :=
| Checked : A -> checked A
| Invalid : string -> nat -> checked A.
Arguments Checked {A} _.
Arguments Invalid {A} _ _.

Definition check_bind {A B} (x : checked A) (f : A -> checked B) : checked B :=
  match x with Checked a => f a | Invalid message offset => Invalid message offset end.
Definition check_map {A B} (f : A -> B) (x : checked A) : checked B :=
  check_bind x (fun a => Checked (f a)).
Notation "'let!' x ':=' v 'in' e" :=
  (check_bind v (fun x => e)) (at level 200, x pattern, v at level 100, e at level 200).

Fixpoint check_list {A} (xs : list (checked A)) : checked (list A) :=
  match xs with
  | [] => Checked []
  | x :: xs => let! a := x in check_map (cons a) (check_list xs)
  end.

Definition check_optional {A} (x : option (checked A)) : checked (option A) :=
  match x with None => Checked None | Some x => check_map Some x end.

(* A name, call, tuple or record inside parentheses can be part of an
   expression or a binding. Carry both readings until the surrounding
   production selects one. Only a direct, unqualified name is a shorthand
   record field. Track a literal's first suffix for the D68 diagnostic. *)
Record form := Form {
  form_offset : nat;
  as_expr : checked aexpr;
  as_binding : checked binding_form;
  short_name : option name;
  direct_literal : option (string * option name)
}.

Definition expression (offset : nat) (e : checked aexpr) : form :=
  Form offset e (Invalid "expected a binding form" offset) None None.

Definition with_annotation (f : form) (ann_offset : nat) (ty : aty) : form :=
  let binding :=
    let! b := as_binding f in
    match annotate_binding b (Some ty) with
    | Some b => Checked b
    | None => Invalid "a binding can only have one annotation at each level" ann_offset
    end in
  Form (form_offset f) (Invalid "unexpected type annotation" ann_offset) binding None None.

Definition bindings (fs : list form) : checked (list binding_form) :=
  check_list (map as_binding fs).
Definition expressions (fs : list form) : checked (list aexpr) :=
  check_list (map as_expr fs).

Record reference := Ref {
  ref_offset : nat;
  ref_path : path;
  ref_app : sapp
}.

Definition reference_form (r : reference) : form :=
  let '(Ref offset p app) := r in
  match p, app with
  | [x], None => Form offset (Checked (AEPath p app)) (Checked (BindName x None)) (Some x) None
  | _, _ => expression offset (Checked (AEPath p app))
  end.

Record parens := Parens {
  parens_offset : nat;
  parens_items : list form;
  parens_tuple : bool
}.

Definition grouped (p : parens) : form :=
  let '(Parens offset fs tuple) := p in
  match fs, tuple with
  | [one], false => Form offset (as_expr one) (as_binding one) None None
  | _, _ => Form offset (check_map tuple_expr (expressions fs))
                     (check_map (fun bs => tuple_binding bs None) (bindings fs)) None None
  end.

Definition call_form (r : reference) (p : parens) : form :=
  let '(Ref offset pa app) := r in
  let b := match app with
    | None => check_map (fun bs => positional_binding pa bs None) (bindings (parens_items p))
    | Some _ => Invalid "a tag binding cannot have an application" offset
    end in
  Form offset (check_map (AECall pa app) (expressions (parens_items p))) b None None.

Record field := Field {
  expr_field : checked (name * option aexpr);
  binding_field : checked (name * binding_form)
}.

Definition short_field (offset : nat) (x : name) (ann : option (nat * aty)) : field :=
  match ann with
  | None => Field (Checked (x, None)) (Checked (x, BindName x None))
  | Some (ann_offset, ty) => Field (Invalid "unexpected type annotation" ann_offset)
                          (Checked (x, BindName x (Some ty)))
  end.

Definition short_reference (r : reference) (ann : option (nat * aty)) : field :=
  match ref_path r, ref_app r with
  | [x], None => short_field (ref_offset r) x ann
  | _, _ => let err := "a shorthand field needs a single name" in
            Field (Invalid err (ref_offset r)) (Invalid err (ref_offset r))
  end.

Definition long_field (x : name) (f : form) : field :=
  Field (check_map (fun e => (x, Some e)) (as_expr f))
        (check_map (fun b => (x, b)) (as_binding f)).

Definition record_form (offset : nat) (r : option reference) (fields : list field) : form :=
  let es := check_list (map expr_field fields) in
  let bs := check_list (map binding_field fields) in
  let '(e, b) :=
    match r with
    | None => (check_map AEBareRecord es, check_map (fun fs => BindProduct false fs None) bs)
    | Some (Ref _ p app) =>
        (check_map (AERec p app) es,
         match app with
         | None => check_map (fun fs => BindTag p None (BindProduct false fs None) None) bs
         | Some _ => Invalid "a tag binding cannot have an application" offset
         end)
    end in
  Form offset e b None None.

Definition literal_form (t : token) : form :=
  Form (toff t) (Checked (AELit (lit_norm (tx t))))
    (Invalid "expected a binding form" (toff t)) None (Some (tx t, None)).

Definition string_form (t : token) : form :=
  let value := string_value (tx t) in
  expression (toff t) (if string_utf8_valid value then Checked (string_desugar value)
                       else Invalid "invalid UTF-8 in string literal" (toff t)).

Definition suffix_action (dot : token) (p : path) (args : option parens) (f : form) : form :=
  let e :=
    let! e := as_expr f in
    match args, p with
    | Some p_args, _ => check_map (AEMeth e p) (expressions (parens_items p_args))
    | None, [x] => Checked (AEField e x false)
    | None, _ => Invalid "field access takes a single name" (toff dot)
    end in
  let lit := match direct_literal f with
    | Some (text, None) => Some (text, Some (hd "m" p))
    | other => other
    end in
  Form (form_offset f) e (Invalid "a binding cannot have a field or method suffix" (toff dot)) None lit.

Definition unary_form (op : token) (f : form) : form :=
  expression (toff op)
    (match tx op, direct_literal f with
     | "-", Some (text, Some method) =>
         Invalid ("`-` before a literal with a method call: write `(-" ++ text ++ ")." ++ method
                  ++ "(...)` or `-(" ++ text ++ "." ++ method ++ "(...))` (D68)") (form_offset f)
     | text, _ => check_map (AEUnary text) (as_expr f)
     end).

(* D68: bitwise chains are homogeneous, shifts and comparisons do not chain,
   and a term permits at most one final division or remainder. The input to
   these folds consists of already parsed unary operands and operator tokens. *)
Definition operators := list (token * form).
Definition operator_kind_in (kinds : list string) (op : token) : bool := mem (tx op) kinds.

Definition mixed_operator {A} (prev : option token) (op : token) : checked A :=
  let next := tx op in
  let message :=
    match prev with
    | Some t =>
        let before := tx t in
        if mem before COMPARISON && mem next COMPARISON then "comparisons do not chain"
        else if mem before SHIFT && mem next SHIFT then "shifts do not chain"
        else if String.eqb before next then "`" ++ next ++ "` does not chain here"
        else "`" ++ before ++ "` and `" ++ next ++ "` are not ordered"
    | None => "`None` and `" ++ next ++ "` are not ordered"
    end in
  Invalid (message ++ "; add parentheses (D68)") (toff op).

Fixpoint fold_same_operator (op : token) (e : aexpr) (ops : operators) : checked aexpr :=
  match ops with
  | [] => Checked e
  | (next, f) :: rest =>
      if String.eqb (tx op) (tx next) then
        let! rhs := as_expr f in fold_same_operator op (AEBinary (tx op) e rhs) rest
      else mixed_operator (Some op) next
  end.

Fixpoint fold_product (e : aexpr) (last : option token) (ops : operators)
    : checked (aexpr * option token * operators) :=
  match ops with
  | (op, f) :: rest =>
      if String.eqb (tx op) "*" then
        let! rhs := as_expr f in fold_product (AEBinary "*" e rhs) (Some op) rest
      else if operator_kind_in FRACTION op then
        let! rhs := as_expr f in
        match rest with
        | (next, _) :: _ =>
            if operator_kind_in ("*" :: FRACTION) next then mixed_operator (Some op) next
            else Checked (AEBinary (tx op) e rhs, Some op, rest)
        | [] => Checked (AEBinary (tx op) e rhs, Some op, [])
        end
      else Checked (e, last, ops)
  | [] => Checked (e, last, [])
  end.

Fixpoint fold_sum (fuel : nat) (e : aexpr) (last : option token) (ops : operators)
    : checked (aexpr * option token * operators) :=
  match fuel, ops with
  | S fuel, (op, f) :: rest =>
      if operator_kind_in ADDITIVE op then
        let! rhs := as_expr f in
        let! (term, _, rest) := fold_product rhs None rest in
        fold_sum fuel (AEBinary (tx op) e term) (Some op) rest
      else Checked (e, last, ops)
  | _, _ => Checked (e, last, ops)
  end.

Definition fold_operand (e : aexpr) (ops : operators)
    : checked (aexpr * option token * operators) :=
  match ops with
  | (op, f) :: rest =>
      if operator_kind_in SHIFT op then
        let! rhs := as_expr f in Checked (AEBinary (tx op) e rhs, Some op, rest)
      else
        let! (term, last, rest) := fold_product e None ops in
        fold_sum (List.length rest) term last rest
  | [] => Checked (e, None, [])
  end.

Definition fold_operators (first : form) (ops : operators) : form :=
  match ops with
  | [] => first
  | (op, _) :: _ =>
      expression (form_offset first)
        (let! e := as_expr first in
         if operator_kind_in BITWISE op then fold_same_operator op e ops else
         let! (lhs, last, rest) := fold_operand e ops in
         match rest with
         | [] => Checked lhs
         | (cmp, f) :: rest =>
             if operator_kind_in COMPARISON cmp then
               let! rhs := as_expr f in
               let! (rhs, last, rest) := fold_operand rhs rest in
               match rest with
               | [] => Checked (AEBinary (tx cmp) lhs rhs)
               | (next, _) :: _ => mixed_operator (match last with None => Some cmp | _ => last end) next
               end
             else mixed_operator last cmp
         end)
  end.

Definition postfix (first : form) (suffixes : list (form -> form)) : form :=
  fold_left (fun f suffix => suffix f) suffixes first.

Definition return_form (t : token) (value : option form) : form :=
  expression (toff t) (check_map AEReturn (check_optional (option_map as_expr value))).

Definition lambda_form (head : parens * option aty) (body : form) : form :=
  let '(p, ret) := head in
  let offset := parens_offset p in
  expression offset
    (let! bs := bindings (parens_items p) in
     if parameter_bindings_valid bs then
       let! e := as_expr body in
       Checked (AELam (lambda_signature 0 bs) ret (bind_lambda_parameters bs e))
     else Invalid "duplicate parameter binding" offset).

(* A block may use a final expression as its result. A bare block with just
   a name can also be a shorthand record binding when selected by its caller. *)
Definition block_parts := (checked (list astmt) * option form)%type.
Definition empty_block : block_parts := (Checked [], None).

Definition make_block (b : block_parts) : checked ablock :=
  let! ss := fst b in
  let! tail := check_optional (option_map as_expr (snd b)) in
  Checked (ABlock ss tail).

Definition block_form (t : token) (b : block_parts) : form :=
  let offset := toff t in
  let binder :=
    match fst b, snd b with
    | Checked [], Some tail =>
        match short_name tail with
        | Some x => Checked (BindProduct false [(x, BindName x None)] None)
        | None => Invalid "expected a record binding" offset
        end
    | _, _ => Invalid "expected a record binding" offset
    end in
  Form offset (check_map AEBlock (make_block b)) binder None None.

Definition prepend_statements (ss : checked (list astmt)) (b : block_parts) : block_parts :=
  (let! head := ss in check_map (app head) (fst b), snd b).

Definition statement (s : checked astmt) : checked (list astmt) :=
  check_map (fun s => [s]) s.

Definition end_block_expr (e : form) (next : option block_parts) : block_parts :=
  match next with
  | None => (Checked [], Some e)
  | Some b => prepend_statements (statement (check_map ASExpr (as_expr e))) b
  end.

Definition destructuring_stmt (mutable : bool) (target : checked binding_form)
    (ann_offset : nat) (rhs : form) : checked (list astmt) :=
  let! b := target in
  if binding_valid b then check_map (destructure mutable b) (as_expr rhs)
  else Invalid "duplicate name in destructuring binding" ann_offset.

Definition if_form (t : token) (test : form) (yes : checked ablock)
    (no : option (checked aexpr)) : form :=
  expression (toff t)
    (let! test := as_expr test in let! yes := yes in
     check_map (AEIf test yes) (check_optional no)).

Definition match_form (t : token) (value : form) (arms : list (checked (apat * aexpr))) : form :=
  expression (toff t) (let! value := as_expr value in check_map (AEMatch value) (check_list arms)).

Definition fun_decl (t : token) (head : fnrecv * name) (params : list form)
    (tail : option aty * option (checked ablock)) : checked adecl :=
  let '(recv, x) := head in
  let '(ret, body) := tail in
  let! bs := bindings params in
  if parameter_bindings_valid bs then
    match parameter_signature 0 bs with
    | None => Invalid "a function parameter needs a complete type" (toff t)
    | Some sig =>
        match body with
        | None =>
            if forallb (abstract_binding_valid false) bs then Checked (ADFn recv x sig ret None)
            else Invalid "an abstract parameter cannot destructure tags or annotate a product and its parts" (toff t)
        | Some body => check_map (fun b => ADFn recv x sig ret (Some (bind_fn_parameters bs b))) body
        end
    end
  else Invalid "duplicate parameter binding" (toff t).

(* Continuations used only for statements beginning with a bare block.
   Their final value tells the enclosing block whether the expression is a
   statement or the block result. *)
Definition block_end := option block_parts.
Definition free_tail := (form -> form * operators * block_end)%type.
Definition free_operand := (form * operators * block_end)%type.

Definition free_finish (next : block_end) : free_tail := fun f => (f, [], next).
Definition free_binary (op : token) (rhs : free_operand) : free_tail :=
  fun lhs => let '(rhs, ops, next) := rhs in (lhs, (op, rhs) :: ops, next).
Definition free_suffix (suffix : form -> form) (rest : free_tail) : free_tail :=
  fun f => rest (suffix f).

Definition map_free_operand (f : form -> form) (operand : free_operand) : free_operand :=
  let '(head, ops, next) := operand in (f head, ops, next).

Definition map_free_expr (f : form -> form) (operand : free_operand) : free_operand :=
  let '(head, ops, next) := operand in (f (fold_operators head ops), [], next).

Definition finish_free_statement (operand : free_operand) : block_parts :=
  let '(head, ops, next) := operand in end_block_expr (fold_operators head ops) next.
