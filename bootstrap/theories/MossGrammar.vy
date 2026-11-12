%{
(* Moss's token grammar. The generated parse-tree relation is used by Audit;
   the generated LR parser is used by the reference implementation. *)
From Stdlib Require Import List String.
From NM Require Import Surface MossActions.
Import ListNotations Surface.Input.
Local Open Scope string_scope.
%}

%token <Syntax.token> NAME STRING INTEGER DECIMAL
%token <Syntax.token> AS ASSUME FUNCTOR BIND BREAK CONTEXT ELSE FN FOR IF IMPORT LET LOOP MATCH
%token <Syntax.token> RETURN TAG THIS_TYPE THIS TYPE USE VAL VAR WHILE IN UNDERSCORE
%token <Syntax.token> EXCLAM PERCENT AMPERSAND LPAREN RPAREN STAR PLUS COMMA HYPHEN
%token <Syntax.token> DOT SLASH COLON SEMI LESS EQUAL GREATER LBRACKET RBRACKET CARET LBRACE PIPE RBRACE
%token <Syntax.token> EXCLAM_EQUAL COLON_COLON EQUAL_GREATER LESS_LESS LESS_EQUAL
%token <Syntax.token> EQUAL_EQUAL GREATER_EQUAL GREATER_GREATER HYPHEN_GREATER EOF
%token PADDING

%start <checked afile> file

%type <aimport> import_decl
%type <name> alias dot_alias
%type <bool * list ause> uses
%type <ause> use_item
%type <nat * path> path
%type <reference> type_ref
%type <list (path * spec)> app
%type <path * spec> app_binding
%type <spec> spec
%type <name> method_name
%type <aitem> assume_item
%type <checked citem> ctx_item
%type <aty> moss_type inhabited_type type_atom record_type
%type <list aty> type_members
%type <name * aty> type_field
%type <nat * aty> annotation equation_rhs
%type <checked binding_form> bind_target
%type <apat> pattern
%type <list (name * option apat)> pattern_record
%type <name * option apat> pattern_field
%type <form> expr arm_expr closed_expr open_expr
%type <form> conditional match_expr open_unary condition open_condition
%type <form> fenced_open_unary fenced_open_expr arm_closed_unary
%type <form * MossActions.operators> fenced_operators
%type <checked aexpr> alternative
%type <list (checked (apat * aexpr))> arms
%type <form> operators(full_primary,open_unary)
%type <form> operators(restricted_primary,fenced_open_unary)
%type <MossActions.operators> operator_tail(full_primary,open_unary)
%type <MossActions.operators> operator_tail(restricted_primary,fenced_open_unary)
%type <Syntax.token> binop unop
%type <form> closed_unary(full_primary) closed_unary(restricted_primary) naked_path
%type <form> safe_unary(full_primary) safe_unary(restricted_primary)
%type <form> safe_atom(full_primary) safe_atom(restricted_primary) safe_atom(nonblock_primary)
%type <parens * option aty> lambda_head
%type <parens * (nat * aty)> annotated_group
%type <form> cover_expr
%type <form -> form> suffix
%type <parens> arguments
%type <form> restricted_primary path_primary nonblock_primary full_primary free_primary
%type <form> named_record bare_record bare_block
%type <field> record_short annotated_short record_field
%type <form> annotated_path
%type <checked ablock> block
%type <block_parts> block_body nonempty_block_body body_no_minus body_no_paren body_no_brace
%type <block_parts> body(body_ops_all) body(body_ops_no_minus) body(body_ops_no_paren) body(body_ops_no_brace)
%type <block_end> explicit_end free_end(nonempty_block_body) free_end(body_no_minus)
%type <block_end> free_end(body_no_paren) free_end(body_no_brace)
%type <form> body_ops_all body_ops_no_minus body_ops_no_paren body_ops_no_brace
%type <block_parts> free_bare_block
%type <free_tail> free_after_value
%type <Syntax.token -> path -> free_tail> free_after_member
%type <reference -> free_operand> free_after_ref
%type <free_operand> free_operand free_lambda_body
%type <checked (list astmt)> statement
%type <checked (list (spec * option aexpr))> bind
%type <checked (spec * option aexpr)> bind_item
%type <checked adecl> decl
%type <list name> listed_names
%type <checked aexpr> value_initializer
%type <fnrecv * name> function_name
%type <option aty * option (checked ablock)> fn_tail
%type <option (checked ablock)> fn_body
%type <option (checked (list (list (spec * option aexpr))))> functor_body

%type <option name> maybe(alias) maybe(dot_alias) maybe(method_name)
%type <option (bool * list ause)> maybe(uses)
%type <sapp> maybe(app)
%type <option (nat * aty)> maybe(annotation) maybe(equation_rhs)
%type <option (checked aexpr)> maybe(alternative) maybe(value_initializer)
%type <option parens> maybe(arguments)
%type <option (list name)> maybe(listed_names)
%type <list aimport> many(import_decl)
%type <list (checked adecl)> many(decl)
%type <list (form -> form)> many(suffix)
%type <list (checked (list (spec * option aexpr)))> many(bind)
%type <list ause> items(use_item) nonempty_items(use_item)
%type <list (path * spec)> items(app_binding) nonempty_items(app_binding)
%type <list aty> items(moss_type) nonempty_items(moss_type)
%type <list (name * aty)> items(type_field) nonempty_items(type_field)
%type <list form> items(cover_expr) nonempty_items(cover_expr)
%type <list apat> items(pattern) nonempty_items(pattern)
%type <list (name * option apat)> items(pattern_field) nonempty_items(pattern_field)
%type <list field> items(record_field) nonempty_items(record_field)
%type <list (checked (spec * option aexpr))> items(bind_item) nonempty_items(bind_item)
%type <list aitem> items(assume_item) nonempty_items(assume_item)
%type <list (checked citem)> items(ctx_item) nonempty_items(ctx_item)
%type <list spec> items(spec) nonempty_items(spec)
%type <list Syntax.token> items(NAME) nonempty_items(NAME)
%type <option Syntax.token> maybe(PIPE) maybe(COMMA)
%%

maybe(X):
| { None }
| x = X { Some x }

many(X):
| { [] }
| x = X xs = many(X) { x :: xs }

items(X):
| { [] }
| xs = nonempty_items(X) { xs }

nonempty_items(X):
| x = X { [x] }
| x = X COMMA xs = items(X) { x :: xs }

file:
| is = many(import_decl) ds = many(decl) EOF { check_map (AFile is) (check_list ds) }

import_decl:
| IMPORT s = STRING a = maybe(alias) u = maybe(uses) SEMI
  { let '(glob, us) := match u with Some u => u | None => (false, []) end in
    AImport (Syntax.string_value (Syntax.tx s)) a glob us }

alias:
| AS n = NAME { Syntax.tx n }

uses:
| USE STAR { (true, []) }
| USE us = items(use_item) { (false, us) }

use_item:
| n = NAME a = maybe(alias) { AUse false (Syntax.tx n) (option_map (fun x => (false, x)) a) }
| DOT n = NAME a = maybe(dot_alias) { AUse true (Syntax.tx n) (option_map (fun x => (true, x)) a) }

dot_alias:
| AS DOT n = NAME { Syntax.tx n }

path:
| n = NAME { (Syntax.toff n, [Syntax.tx n]) }
| n = NAME COLON_COLON p = path { (Syntax.toff n, Syntax.tx n :: snd p) }

type_ref:
| p = path a = maybe(app) { Ref (fst p) (snd p) a }

app:
| LBRACKET bs = items(app_binding) RBRACKET { bs }

app_binding:
| p = path EQUAL s = spec { (snd p, s) }

spec:
| p = path m = maybe(method_name) a = maybe(app) { Spec (Some (snd p)) m a }
| m = method_name a = maybe(app) { Spec None (Some m) a }

method_name:
| DOT n = NAME { Syntax.tx n }

assume_item:
| p = path { AIHead (snd p, None) }
| p = path m = method_name { AIHead (snd p, Some m) }
| l = type_ref EQUAL r = type_ref { AIEq (ATRef (ref_path l) (ref_app l)) (ATRef (ref_path r) (ref_app r)) }

ctx_item:
| s = spec r = maybe(equation_rhs)
  { match r, s with
    | None, _ => Checked (CISpec s)
    | Some (_, ty), Spec (Some p) None a => Checked (CIEq (ATRef p a) ty)
    | Some (offset, _), _ => Invalid "each side of a type equation must be a type reference" offset
    end }

equation_rhs:
| t = EQUAL r = type_ref { (Syntax.toff t, ATRef (ref_path r) (ref_app r)) }

moss_type:
| PIPE { ATNever }
| t = inhabited_type { t }

inhabited_type:
| maybe(PIPE) t = type_atom rest = type_members
  { match rest with [] => t | _ => ATUnion (t :: rest) end }

type_members:
| { [] }
| PIPE t = type_atom rest = type_members { t :: rest }

type_atom:
| r = type_ref { ATRef (ref_path r) (ref_app r) }
| THIS_TYPE { ATThis }
| LPAREN RPAREN { ATUnit }
| LPAREN t = moss_type RPAREN { t }
| LPAREN t = moss_type COMMA ts = items(moss_type) RPAREN { Syntax.tuple_type (t :: ts) }
| t = record_type { t }

record_type:
| LBRACE fs = items(type_field) RBRACE { ATRec fs }

type_field:
| n = NAME COLON t = moss_type { (Syntax.tx n, t) }

annotation:
| c = COLON t = moss_type { (Syntax.toff c, t) }

bind_target:
| f = cover_expr { as_binding f }
| f = cover_expr COMMA fs = items(cover_expr)
  { check_map (fun bs => Syntax.tuple_binding bs None) (bindings (f :: fs)) }

pattern:
| UNDERSCORE { APWild }
| p = path { APPath (snd p) }
| p = path LPAREN ps = items(pattern) RPAREN { Syntax.positional_pattern (snd p) ps }
| p = path fs = pattern_record { APRec (Some (snd p)) fs }
| fs = pattern_record { APRec None fs }

pattern_record:
| LBRACE fs = items(pattern_field) RBRACE { fs }

pattern_field:
| n = NAME { (Syntax.tx n, None) }
| n = NAME EQUAL p = pattern { (Syntax.tx n, Some p) }

(* IF, MATCH and BREAK end before any following operators or suffixes.
   A lambda with one of those bodies can itself take a suffix or operator.
   Other lambda bodies consume the rest of the operator expression. *)
expr:
| e = closed_expr { e }
| e = open_expr { e }

closed_expr:
| e = conditional { e }
| e = match_expr { e }
| t = BREAK { expression (Syntax.toff t) (Checked AEBreak) }
| t = RETURN e = closed_expr { return_form t (Some e) }

open_expr:
| t = RETURN { return_form t None }
| t = RETURN e = open_expr { return_form t (Some e) }
| e = operators(full_primary,open_unary) { e }

conditional:
| t = IF test = condition yes = block no = maybe(alternative) { if_form t test yes no }

alternative:
| ELSE b = block { check_map AEBlock b }
| ELSE e = conditional { as_expr e }

match_expr:
| t = MATCH e = condition LBRACE arms = arms RBRACE { match_form t e arms }

arms:
| { [] }
| p = pattern EQUAL_GREATER b = bare_block maybe(COMMA) rest = arms
  { check_map (fun e => (p, e)) (as_expr b) :: rest }
| p = pattern EQUAL_GREATER e = arm_expr { [check_map (fun e => (p, e)) (as_expr e)] }
| p = pattern EQUAL_GREATER e = arm_expr COMMA rest = arms
  { check_map (fun e => (p, e)) (as_expr e) :: rest }

arm_expr:
| e = closed_expr { e }
| t = RETURN { return_form t None }
| t = RETURN e = open_expr { return_form t (Some e) }
| first = arm_closed_unary ops = operator_tail(full_primary,open_unary) { fold_operators first ops }
| e = open_unary { e }

arm_closed_unary:
| e = safe_atom(nonblock_primary) { e }
| e = naked_path { e }
| op = unop e = safe_unary(full_primary) { unary_form op e }

operators(P, O):
| first = closed_unary(P) ops = operator_tail(P, O) { fold_operators first ops }
| e = O { e }

operator_tail(P, O):
| { [] }
| op = binop e = closed_unary(P) ops = operator_tail(P, O) { (op, e) :: ops }
| op = binop e = O { [(op, e)] }

binop:
| t = STAR { t } | t = SLASH { t } | t = PERCENT { t } | t = PLUS { t } | t = HYPHEN { t }
| t = LESS_LESS { t } | t = GREATER_GREATER { t }
| t = LESS { t } | t = LESS_EQUAL { t } | t = GREATER { t } | t = GREATER_EQUAL { t }
| t = EQUAL_EQUAL { t } | t = EXCLAM_EQUAL { t }
| t = AMPERSAND { t } | t = CARET { t } | t = PIPE { t }

unop:
| t = HYPHEN { t } | t = EXCLAM { t }

closed_unary(P):
| e = safe_unary(P) { e }
| e = naked_path { e }

naked_path:
| r = type_ref { reference_form r }
| op = unop e = naked_path { unary_form op e }

safe_unary(P):
| op = unop e = safe_unary(P) { unary_form op e }
| e = safe_atom(P) { e }

safe_atom(P):
| e = P ss = many(suffix) { postfix e ss }
| r = type_ref a = arguments ss = many(suffix) { postfix (call_form r a) ss }
| r = type_ref s = suffix ss = many(suffix) { postfix (reference_form r) (s :: ss) }
| h = lambda_head e = closed_expr ss = many(suffix) { postfix (lambda_form h e) ss }

open_unary:
| op = unop e = open_unary { unary_form op e }
| h = lambda_head e = open_expr { lambda_form h e }

(* A condition is immediately followed by a brace. A path in an open
   lambda body still takes its own record payload greedily, so the body
   can end before the condition's brace only at a safe unary operand. *)
condition:
| e = closed_expr { e }
| e = open_condition { e }

open_condition:
| t = RETURN e = open_condition { return_form t (Some e) }
| e = operators(restricted_primary, fenced_open_unary) { e }

fenced_open_unary:
| op = unop e = fenced_open_unary { unary_form op e }
| h = lambda_head e = fenced_open_expr { lambda_form h e }

fenced_open_expr:
| t = RETURN e = fenced_open_expr { return_form t (Some e) }
| e = fenced_operators { fold_operators (fst e) (snd e) }

fenced_operators:
| e = safe_unary(full_primary) { (e, []) }
| a = closed_unary(full_primary) op = binop b = fenced_operators
  { (a, (op, fst b) :: snd b) }
| e = fenced_open_unary { (e, []) }

lambda_head:
| p = arguments EQUAL_GREATER { (p, None) }
| p = annotated_group EQUAL_GREATER { (fst p, Some (snd (snd p))) }

annotated_group:
| p = arguments t = annotation { (p, t) }

(* The shared forms in parentheses can be used as expressions or binding
   forms. Their actions carry both readings and the caller selects one. *)
cover_expr:
| e = expr { e }
| p = annotated_group { with_annotation (grouped (fst p)) (fst (snd p)) (snd (snd p)) }
| e = annotated_path { e }
| e = named_record t = annotation { with_annotation e (fst t) (snd t) }
| e = bare_record t = annotation { with_annotation e (fst t) (snd t) }
| e = bare_block t = annotation { with_annotation e (fst t) (snd t) }

suffix:
| d = DOT p = path a = maybe(arguments) { suffix_action d (snd p) a }

arguments:
| t = LPAREN RPAREN { Parens (Syntax.toff t) [] false }
| t = LPAREN e = cover_expr RPAREN { Parens (Syntax.toff t) [e] false }
| t = LPAREN e = cover_expr COMMA es = items(cover_expr) RPAREN
  { Parens (Syntax.toff t) (e :: es) true }

restricted_primary:
| p = arguments { grouped p }
| t = THIS { expression (Syntax.toff t) (Checked AEThis) }
| t = INTEGER { literal_form t }
| t = DECIMAL { literal_form t }
| t = STRING { string_form t }

path_primary:
| r = type_ref a = maybe(arguments)
  { match a with None => reference_form r | Some a => call_form r a end }

nonblock_primary:
| e = restricted_primary { e }
| e = named_record { e }
| e = bare_record { e }

named_record:
| r = type_ref LBRACE fs = items(record_field) RBRACE { record_form (ref_offset r) (Some r) fs }

bare_record:
| t = LBRACE RBRACE { record_form (Syntax.toff t) None [] }
| t = LBRACE f = record_short COMMA fs = items(record_field) RBRACE
  { record_form (Syntax.toff t) None (f :: fs) }
| t = LBRACE n = NAME EQUAL e = cover_expr RBRACE
  { record_form (Syntax.toff t) None [long_field (Syntax.tx n) e] }
| t = LBRACE n = NAME EQUAL e = cover_expr COMMA fs = items(record_field) RBRACE
  { record_form (Syntax.toff t) None (long_field (Syntax.tx n) e :: fs) }
| t = LBRACE f = annotated_short RBRACE { record_form (Syntax.toff t) None [f] }

record_short:
| r = type_ref { short_reference r None }
| f = annotated_short { f }

annotated_short:
| r = type_ref t = annotation { short_reference r (Some t) }

annotated_path:
| e = path_primary t = annotation { with_annotation e (fst t) (snd t) }

full_primary:
| e = nonblock_primary { e }
| e = bare_block { e }

bare_block:
| t = LBRACE b = nonempty_block_body RBRACE { block_form t b }

record_field:
| n = NAME t = maybe(annotation) { short_field (Syntax.toff n) (Syntax.tx n) t }
| n = NAME EQUAL e = cover_expr { long_field (Syntax.tx n) e }

block:
| LBRACE b = block_body RBRACE { make_block b }

block_body:
| { empty_block }
| b = nonempty_block_body { b }

nonempty_block_body:
| b = body(body_ops_all) { b }
| b = free_bare_block { b }

body_no_minus:
| b = body(body_ops_no_minus) { b }
| b = free_bare_block { b }

body_no_paren:
| b = body(body_ops_no_paren) { b }
| b = free_bare_block { b }

body_no_brace:
| b = body(body_ops_no_brace) { b }

body(Ops):
| s = statement rest = block_body { prepend_statements s rest }
| e = conditional rest = free_end(nonempty_block_body) { end_block_expr e rest }
| e = match_expr rest = free_end(nonempty_block_body) { end_block_expr e rest }
| t = BREAK rest = explicit_end { end_block_expr (expression (Syntax.toff t) (Checked AEBreak)) rest }
| t = RETURN rest = explicit_end { end_block_expr (return_form t None) rest }
| t = RETURN e = expr rest = explicit_end { end_block_expr (return_form t (Some e)) rest }
| e = Ops rest = explicit_end { end_block_expr e rest }

explicit_end:
| { None }
| SEMI b = block_body { Some b }

free_end(Next):
| b = explicit_end { b }
| b = Next { Some b }

body_ops_all:
| e = body_ops_no_minus { e }
| t = HYPHEN e = closed_unary(full_primary) ops = operator_tail(full_primary, open_unary)
  { fold_operators (unary_form t e) ops }
| t = HYPHEN e = open_unary { unary_form t e }

body_ops_no_minus:
| e = body_ops_no_paren { e }
| p = arguments ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (grouped p) ss) ops }
| h = lambda_head e = closed_expr ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (lambda_form h e) ss) ops }
| h = lambda_head e = open_expr { lambda_form h e }

body_ops_no_paren:
| e = body_ops_no_brace { e }
| e = bare_record ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix e ss) ops }

body_ops_no_brace:
| t = THIS ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (expression (Syntax.toff t) (Checked AEThis)) ss) ops }
| t = INTEGER ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (literal_form t) ss) ops }
| t = DECIMAL ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (literal_form t) ss) ops }
| t = STRING ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (string_form t) ss) ops }
| r = type_ref a = maybe(arguments) ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix (match a with None => reference_form r | Some a => call_form r a end) ss) ops }
| e = named_record ss = many(suffix) ops = operator_tail(full_primary, open_unary)
  { fold_operators (postfix e ss) ops }
| t = EXCLAM e = closed_unary(full_primary) ops = operator_tail(full_primary, open_unary)
  { fold_operators (unary_form t e) ops }
| t = EXCLAM e = open_unary { unary_form t e }

(* A bare block can start an expression statement without a semicolon.
   Continue its expression greedily before looking for another statement:
   a minus is binary here, and parentheses or braces may belong to the
   previous path. The following nonterminals express these three boundaries. *)
free_bare_block:
| e = bare_block rest = free_after_value { finish_free_statement (rest e) }

free_after_value:
| rest = free_end(body_no_minus) { free_finish rest }
| d = DOT p = path rest = free_after_member { rest d (snd p) }
| op = binop rest = free_operand { free_binary op rest }

free_after_member:
| rest = free_end(body_no_paren)
  { fun d p => free_suffix (suffix_action d p None) (free_finish rest) }
| a = arguments rest = free_after_value
  { fun d p => free_suffix (suffix_action d p (Some a)) rest }
| d = DOT p = path rest = free_after_member
  { fun dot member => free_suffix (suffix_action dot member None) (rest d (snd p)) }
| op = binop rest = free_operand
  { fun dot member => free_suffix (suffix_action dot member None) (free_binary op rest) }

free_operand:
| op = unop rest = free_operand { map_free_operand (unary_form op) rest }
| r = type_ref rest = free_after_ref { rest r }
| e = free_primary rest = free_after_value { rest e }
| h = lambda_head e = closed_expr rest = free_after_value { rest (lambda_form h e) }
| h = lambda_head rest = free_lambda_body { map_free_expr (lambda_form h) rest }

free_after_ref:
| rest = free_end(body_no_brace) { fun r => free_finish rest (reference_form r) }
| a = arguments rest = free_after_value { fun r => rest (call_form r a) }
| LBRACE fs = items(record_field) RBRACE rest = free_after_value
  { fun r => rest (record_form (ref_offset r) (Some r) fs) }
| d = DOT p = path rest = free_after_member { fun r => rest d (snd p) (reference_form r) }
| op = binop rest = free_operand { fun r => free_binary op rest (reference_form r) }

free_primary:
| e = restricted_primary { e }
| e = bare_record { e }
| e = bare_block { e }

free_lambda_body:
| t = RETURN rest = explicit_end { free_finish rest (return_form t None) }
| t = RETURN rest = free_lambda_body { map_free_expr (fun e => return_form t (Some e)) rest }
| rest = free_operand { rest }

statement:
| LET b = bind_target t = EQUAL e = expr SEMI { destructuring_stmt false b (Syntax.toff t) e }
| VAR b = bind_target t = EQUAL e = expr SEMI { destructuring_stmt true b (Syntax.toff t) e }
| n = NAME EQUAL e = expr SEMI { statement (check_map (ASAssign (Syntax.tx n)) (as_expr e)) }
| b = bind { statement (check_map ASBind b) }
| WHILE e = condition b = block
  { statement (let! e := as_expr e in check_map (ASWhile e) b) }
| LOOP b = block { statement (check_map ASLoop b) }
| FOR n = NAME IN e = condition b = block
  { statement (let! e := as_expr e in check_map (Syntax.for_desugar (Syntax.tx n) e) b) }

bind:
| BIND xs = items(bind_item) SEMI { check_list xs }

bind_item:
| s = spec { Checked (s, None) }
| s = spec EQUAL e = expr { check_map (fun e => (s, Some e)) (as_expr e) }

decl:
| ASSUME its = items(assume_item) LBRACE ds = many(decl) RBRACE
  { check_map (ADAssume its) (check_list ds) }
| TYPE n = NAME SEMI { Checked (ADType (Syntax.tx n)) }
| TYPE n = NAME EQUAL t = moss_type SEMI { Checked (ADAlias (Syntax.tx n) t) }
| TAG n = NAME SEMI { Checked (ADBareTag (Syntax.tx n)) }
| TAG n = NAME listed_names t = SEMI
  { Invalid ("a tag with listed parameters has a payload; a unit tag is `tag " ++ Syntax.tx n ++ ";` (D73)")
      (Syntax.toff t) }
| TAG n = NAME ps = maybe(listed_names) t = record_type SEMI
  { Checked (ADTag (Syntax.tx n) (match ps with None => [] | Some ps => ps end) None t) }
| TAG n = NAME ps = maybe(listed_names) LPAREN ts = items(moss_type) RPAREN SEMI
  { Checked (ADTag (Syntax.tx n) (match ps with None => [] | Some ps => ps end)
      (Some (List.length ts)) (Syntax.positional_payload_type ts)) }
| VAL n = NAME COLON t = moss_type i = maybe(value_initializer) SEMI
  { check_map (ADVal (Syntax.tx n) t) (check_optional i) }
| t = FN h = function_name LPAREN ps = items(cover_expr) RPAREN tail = fn_tail { fun_decl t h ps tail }
| CONTEXT n = NAME EQUAL xs = items(ctx_item) SEMI { check_map (ADCtx (Syntax.tx n)) (check_list xs) }
| t = FUNCTOR n = NAME ps = maybe(listed_names) COLON xs = items(spec) HYPHEN_GREATER ys = items(spec) b = functor_body
  { match ps, b with
    | Some _, Some _ => Invalid "only an abstract functor lists polymorphic inputs (D84)" (Syntax.toff t)
    | _, _ => check_map (ADFunctor (Syntax.tx n) ps xs ys) (check_optional b)
    end }

listed_names:
| LBRACKET ns = items(NAME) RBRACKET { map Syntax.tx ns }

value_initializer:
| EQUAL e = expr { as_expr e }

function_name:
| n = NAME { (RPlain, Syntax.tx n) }
| r = NAME DOT n = NAME { (RAttached (Syntax.tx r), Syntax.tx n) }
| DOT n = NAME { (RDetached, Syntax.tx n) }

(* A bare pipe followed by a brace starts a record type, so an abstract
   function may use bare | but a body after it requires parentheses. *)
fn_tail:
| b = fn_body { (None, b) }
| COLON PIPE SEMI { (Some ATNever, None) }
| COLON t = inhabited_type b = fn_body { (Some t, b) }

fn_body:
| SEMI { None }
| b = block { Some b }

functor_body:
| SEMI { None }
| LBRACE bs = many(bind) RBRACE { Some (check_list bs) }
%%

(* All terminals keep the lexer token, including its source offset. *)
Definition of_token (t : Syntax.token) : token :=
  match Syntax.tk t with
  | Syntax.NAME => NAME t
  | Syntax.STRING => STRING t
  | Syntax.INTEGER => INTEGER t
  | Syntax.DECIMAL => DECIMAL t
  | Syntax.AS => AS t
  | Syntax.ASSUME => ASSUME t
  | Syntax.FUNCTOR => FUNCTOR t
  | Syntax.BIND => BIND t
  | Syntax.BREAK => BREAK t
  | Syntax.CONTEXT => CONTEXT t
  | Syntax.ELSE => ELSE t
  | Syntax.FN => FN t
  | Syntax.FOR => FOR t
  | Syntax.IF => IF t
  | Syntax.IMPORT => IMPORT t
  | Syntax.LET => LET t
  | Syntax.LOOP => LOOP t
  | Syntax.MATCH => MATCH t
  | Syntax.RETURN => RETURN t
  | Syntax.TAG => TAG t
  | Syntax.THIS_TYPE => THIS_TYPE t
  | Syntax.THIS => THIS t
  | Syntax.TYPE => TYPE t
  | Syntax.USE => USE t
  | Syntax.VAL => VAL t
  | Syntax.VAR => VAR t
  | Syntax.WHILE => WHILE t
  | Syntax.IN => IN t
  | Syntax.UNDERSCORE => UNDERSCORE t
  | Syntax.EXCLAM => EXCLAM t
  | Syntax.PERCENT => PERCENT t
  | Syntax.AMPERSAND => AMPERSAND t
  | Syntax.LPAREN => LPAREN t
  | Syntax.RPAREN => RPAREN t
  | Syntax.STAR => STAR t
  | Syntax.PLUS => PLUS t
  | Syntax.COMMA => COMMA t
  | Syntax.HYPHEN => HYPHEN t
  | Syntax.DOT => DOT t
  | Syntax.SLASH => SLASH t
  | Syntax.COLON => COLON t
  | Syntax.SEMI => SEMI t
  | Syntax.LESS => LESS t
  | Syntax.EQUAL => EQUAL t
  | Syntax.GREATER => GREATER t
  | Syntax.LBRACKET => LBRACKET t
  | Syntax.RBRACKET => RBRACKET t
  | Syntax.CARET => CARET t
  | Syntax.LBRACE => LBRACE t
  | Syntax.PIPE => PIPE t
  | Syntax.RBRACE => RBRACE t
  | Syntax.EXCLAM_EQUAL => EXCLAM_EQUAL t
  | Syntax.COLON_COLON => COLON_COLON t
  | Syntax.EQUAL_GREATER => EQUAL_GREATER t
  | Syntax.LESS_LESS => LESS_LESS t
  | Syntax.LESS_EQUAL => LESS_EQUAL t
  | Syntax.EQUAL_EQUAL => EQUAL_EQUAL t
  | Syntax.GREATER_EQUAL => GREATER_EQUAL t
  | Syntax.GREATER_GREATER => GREATER_GREATER t
  | Syntax.HYPHEN_GREATER => HYPHEN_GREATER t
  | Syntax.EOF => EOF t
  end.

Definition source_token (t : token) : Syntax.token :=
  match t with
  | NAME source | STRING source | INTEGER source | DECIMAL source
  | AS source | ASSUME source | FUNCTOR source | BIND source
  | BREAK source | CONTEXT source | ELSE source | FN source
  | FOR source | IF source | IMPORT source | LET source
  | LOOP source | MATCH source | RETURN source | TAG source
  | THIS_TYPE source | THIS source | TYPE source | USE source
  | VAL source | VAR source | WHILE source | IN source
  | UNDERSCORE source | EXCLAM source | PERCENT source | AMPERSAND source
  | LPAREN source | RPAREN source | STAR source | PLUS source
  | COMMA source | HYPHEN source | DOT source | SLASH source
  | COLON source | SEMI source | LESS source | EQUAL source
  | GREATER source | LBRACKET source | RBRACKET source | CARET source
  | LBRACE source | PIPE source | RBRACE source | EXCLAM_EQUAL source
  | COLON_COLON source | EQUAL_GREATER source | LESS_LESS source | LESS_EQUAL source
  | EQUAL_EQUAL source | GREATER_EQUAL source | GREATER_GREATER source | HYPHEN_GREATER source
  | EOF source
  => source
  | PADDING _ => Syntax.eof_tok
  end.

Definition is_padding (t : token) : bool :=
  match t with PADDING _ => true | _ => false end.
