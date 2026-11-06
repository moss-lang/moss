(* ======================================================================== *)
(* Audit.v — everything a reader must trust about the Moss reference        *)
(* implementation, and nothing else.                                         *)
(*                                                                           *)
(* This file imports no implementation module (no CoreMoss, MossX, Elab,    *)
(* Sx, ...): only the Rocq standard library.  It is read in this order:     *)
(*                                                                           *)
(*   Part I    The input: the s-expression dump of a parsed bundle, and the *)
(*             surface AST it is read into.  m4: the dump is now made in    *)
(*             Gallina from the source text (Parse.v: lexer, parser,        *)
(*             `dump_file`, and `load_bundle`, bundle.py's walk and         *)
(*             collect.Loader's path resolution), so what is trusted is the *)
(*             OCaml driver's `read` (reading a file) and nothing of Python.*)
(*             Part I.2 (module Syntax) specifies the lexer and parser:     *)
(*             `Derives`, a declarative grammar from a file's bytes to its  *)
(*             AST (DRAFT), the subject of FRONT_END in Part VI; Part I.3   *)
(*             the dump and the loader, `Bundled`, the subject of LOADER.   *)
(*   Part II   Elaborated programs: Core Moss's syntax (types, items,       *)
(*             terms, Σ) and MossX's extension (records, loops, memory,     *)
(*             primitives) — what the elaborator outputs.                   *)
(*   Part III  Their dynamic semantics: Core Moss's big-step `eval` and     *)
(*             fueled `interp`, and MossX's `xinterp`/`run_main`, the       *)
(*             latter over an ABSTRACT primitive semantics (PRIM_SEMANTICS) *)
(*             — see the comment there for what that leaves trusted.        *)
(*   Part IV   The primitives' signatures and the one law the theorems use. *)
(*   Part V    The declarative elaboration relation (DRAFT, for the         *)
(*             designer's review).                                           *)
(*   Part VI   The obligations, as module types: PROVED_NOW (what          *)
(*             Certified.v binds today) and COMPLETE (the whole contract).  *)
(*                                                                           *)
(* What is deliberately NOT here: the typing judgment `has_ty` of Core Moss *)
(* and everything about it (contexts, satisfaction, merging, wf_gsig, the   *)
(* logical relation).  The guarantee is phrased as: an accepted program      *)
(* never goes wrong.  So typing is a proof device that lives in CoreMoss.v. *)
(* The definitions of Parts II–III are verbatim moves out of CoreMoss.v and *)
(* MossX.v (which now import this file); none was changed.                 *)
(* ======================================================================== *)

From Stdlib Require Import PrimInt63 Uint63 PrimFloat PArray ZArith.
From Stdlib Require Import String Ascii List Bool Arith.
From Stdlib Require Strings.Byte.
Import ListNotations.

(* ######################################################################## *)
(* Part I. The input                                                         *)
(* ######################################################################## *)

(* The elaborator's input is a *bundle*: the entry module's path, the
   prelude's path, and for each module its path, whether it imports the
   prelude, the canonical paths its imports name (resolved by the file
   system, py/bundle.py), and its parse, as sexpr.py dumps it.  This part is
   moved verbatim from Sx.v; the readers are total and report a malformed
   dump as `Bad`.  A reader can check them against sexpr.py's one-letter
   node tags.  It lives in a module of its own because its result type
   `res`/`Good`/`Bad` would collide with the core's `res`/`Ok`/`Err`. *)
Module Input.
Local Open Scope string_scope.

(* ===================== Results ============================================ *)

Inductive res (A : Type) : Type :=
| Good : A -> res A
| Bad  : string -> res A.
Arguments Good {A}.
Arguments Bad {A}.

Definition rbind {A B : Type} (r : res A) (f : A -> res B) : res B :=
  match r with Good a => f a | Bad m => Bad m end.

Notation "'let*' x ':=' r 'in' k" := (rbind r (fun x => k))
  (at level 200, x pattern, r at level 100, k at level 200).

Fixpoint rmap {A B : Type} (f : A -> res B) (l : list A) : res (list B) :=
  match l with
  | [] => Good []
  | x :: r => let* y := f x in let* ys := rmap f r in Good (y :: ys)
  end.

(* ===================== Tokens ============================================= *)

Inductive sx : Type :=
| SA : string -> sx                 (* an atom *)
| SL : bool -> list sx -> sx.       (* a list; true for [ … ] *)

(* ===================== The surface AST (ast.py) =========================== *)

Definition name := string.
Definition path := list name.

(* Spec: `path.dot[apps]`; a binding of an application is `path = spec`. *)
Inductive spec : Type :=
| Spec : option path -> option name -> option (list (path * spec)) -> spec.

Definition sapp := option (list (path * spec)).

Inductive aty : Type :=
| ATNever | ATThis | ATUnit
| ATRec   : list (name * aty) -> aty
| ATUnion : list aty -> aty
| ATRef   : path -> sapp -> aty.

Inductive apat : Type :=
| APWild
| APPath : path -> apat
| APTag  : path -> apat -> apat
| APRec  : option path -> list (name * option apat) -> apat.

Inductive aexpr : Type :=
| AEUnit | AEThis
| AEPath   : path -> sapp -> aexpr
| AECall   : path -> sapp -> list aexpr -> aexpr
| AEField  : aexpr -> name -> aexpr
| AEMeth   : aexpr -> path -> list aexpr -> aexpr
| AERec    : path -> sapp -> list (name * option aexpr) -> aexpr
| AEIf     : aexpr -> ablock -> option aexpr -> aexpr
| AEMatch  : aexpr -> list (apat * aexpr) -> aexpr
| AEReturn : option aexpr -> aexpr
| AEBreak
| AEBlock  : ablock -> aexpr
| AEUnary  : name -> aexpr -> aexpr
| AEBinary : name -> aexpr -> aexpr -> aexpr
| AELit    : string -> aexpr
| AELam    : list (name * option aty) -> option aty -> aexpr -> aexpr
with ablock : Type :=
| ABlock : list astmt -> option aexpr -> ablock
with astmt : Type :=
| ASLet    : name -> option aty -> aexpr -> astmt
| ASVar    : name -> option aty -> aexpr -> astmt
| ASAssign : name -> aexpr -> astmt
| ASBind   : list (spec * option aexpr) -> astmt
| ASWhile  : aexpr -> ablock -> astmt
| ASLoop   : ablock -> astmt
| ASExpr   : aexpr -> astmt.

Inductive fnrecv : Type := RPlain | RDetached | RAttached (r : name).

Inductive adecl : Type :=
| ADAssume  : list (path * option name) -> list adecl -> adecl
| ADType    : name -> adecl
| ADAlias   : name -> aty -> adecl
| ADTag     : name -> list name -> aty -> adecl
| ADUnit    : name -> adecl
| ADVal     : name -> aty -> option aexpr -> adecl
| ADFn      : fnrecv -> name -> list (name * aty) -> option aty -> option ablock -> adecl
| ADCtx     : name -> list spec -> adecl
(* brackets P(F) (D84 rule 1; None: unbracketed, the 61b7b76 syntax), args,
   result, and the body: None for an abstract functor (D78) *)
| ADFunctor : name -> option (list name) -> list spec -> list spec
              -> option (list (list (spec * option aexpr))) -> adecl.

(* An import's `use` item: (dotted, name), and its alias if any. *)
Record ause := AUse { u_dot : bool; u_name : name; u_alias : option (bool * name) }.

Record aimport := AImport {
  i_path  : string;
  i_alias : option name;
  i_glob  : bool;
  i_uses  : list ause
}.

Record afile := AFile { f_imports : list aimport; f_decls : list adecl }.

(* ===================== String helpers ===================================== *)

Fixpoint split_on_colons (cs : list ascii) (acc : list ascii) : list (list ascii) :=
  match cs with
  | [] => [rev acc]
  | ":"%char :: ":"%char :: r => rev acc :: split_on_colons r []
  | c :: r => split_on_colons r (c :: acc)
  end.

Definition split_path (s : string) : path :=
  map string_of_list_ascii (split_on_colons (list_ascii_of_string s) []).

Definition join_path (p : path) : string := String.concat "::" p.

(* ===================== Reading the dump =================================== *)

Definition bad {A} (what : string) : res A := Bad ("malformed dump: " ++ what).

Definition atom (x : sx) : res string :=
  match x with SA a => Good a | _ => bad "atom" end.

Definition items (x : sx) : res (list sx) :=
  match x with SL true l => Good l | _ => bad "list" end.

(* `_` is sexpr.py's None. *)
Definition opt_atom (x : sx) : res (option string) :=
  match x with SA "_" => Good None | SA a => Good (Some a) | _ => bad "optional atom" end.

Definition opt_path (x : sx) : res (option path) :=
  let* o := opt_atom x in Good (option_map split_path o).

Fixpoint read_spec (fuel : nat) (x : sx) : res spec :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "I"; p; d; a] =>
          let* p' := opt_path p in
          let* d' := opt_atom d in
          let* a' := read_app k a in
          Good (Spec p' d' a')
      | _ => bad "spec"
      end
  end
with read_app (fuel : nat) (x : sx) : res sapp :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SA "_" => Good None
      | SL true bs =>
          let* l := rmap (fun b => match b with
                                   | SL false [SA "J"; SA p; s] =>
                                       let* s' := read_spec k s in Good (split_path p, s')
                                   | _ => bad "binding"
                                   end) bs in
          Good (Some l)
      | _ => bad "application"
      end
  end.

Fixpoint read_ty (fuel : nat) (x : sx) : res aty :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "a"] => Good ATUnit
      | SL false [SA "b"] => Good ATNever
      | SL false [SA "c"] => Good ATThis
      | SL false [SA "d"; SA p; a] =>
          let* a' := read_app k a in Good (ATRef (split_path p) a')
      | SL false [SA "f"; SL true fs] =>
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "h"; SA n; t] =>
                                         let* t' := read_ty k t in Good (n, t')
                                     | _ => bad "record field type"
                                     end) fs in
          Good (ATRec fs')
      | SL false [SA "g"; SL true ms] =>
          let* ms' := rmap (read_ty k) ms in Good (ATUnion ms')
      | _ => bad "type"
      end
  end.

Definition read_opt_ty (fuel : nat) (x : sx) : res (option aty) :=
  match x with
  | SA "_" => Good None
  | _ => let* t := read_ty fuel x in Good (Some t)
  end.

Fixpoint read_pat (fuel : nat) (x : sx) : res apat :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "D"] => Good APWild
      | SL false [SA "E"; SA p; SA "_"] => Good (APPath (split_path p))
      | SL false [SA "E"; SA p; sub] =>
          let* s := read_pat k sub in Good (APTag (split_path p) s)
      | SL false [SA "F"; p; SL true fs] =>
          let* p' := opt_path p in
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "G"; SA n; SA "_"] => Good (n, None)
                                     | SL false [SA "G"; SA n; s] =>
                                         let* s' := read_pat k s in Good (n, Some s')
                                     | _ => bad "pattern field"
                                     end) fs in
          Good (APRec p' fs')
      | _ => bad "pattern"
      end
  end.

Fixpoint read_expr (fuel : nat) (x : sx) : res aexpr :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "i"] => Good AEUnit
      | SL false [SA "j"] => Good AEThis
      | SL false [SA "k"; SA p; a] =>
          let* a' := read_app k a in Good (AEPath (split_path p) a')
      | SL false [SA "l"; SL false [SA "k"; SA p; a]; SL true args] =>
          let* a' := read_app k a in
          let* args' := rmap (read_expr k) args in
          Good (AECall (split_path p) a' args')
      | SL false [SA "m"; o; SA f] =>
          let* o' := read_expr k o in Good (AEField o' f)
      | SL false [SA "n"; o; SA p; SL true args] =>
          let* o' := read_expr k o in
          let* args' := rmap (read_expr k) args in
          Good (AEMeth o' (split_path p) args')
      | SL false [SA "o"; SL false [SA "k"; SA p; a]; SL true fs] =>
          let* a' := read_app k a in
          let* fs' := rmap (fun f => match f with
                                     | SL false [SA "p"; SA n; SA "_"] => Good (n, None)
                                     | SL false [SA "p"; SA n; v] =>
                                         let* v' := read_expr k v in Good (n, Some v')
                                     | _ => bad "record field"
                                     end) fs in
          Good (AERec (split_path p) a' fs')
      | SL false [SA "q"; c; t; e] =>
          let* c' := read_expr k c in
          let* t' := read_block k t in
          let* e' := (match e with
                      | SA "_" => Good None
                      | _ => let* e1 := read_expr k e in Good (Some e1)
                      end) in
          Good (AEIf c' t' e')
      | SL false [SA "r"; s; SL true arms] =>
          let* s' := read_expr k s in
          let* arms' := rmap (fun a => match a with
                                       | SL false [SA "H"; p; b] =>
                                           let* p' := read_pat k p in
                                           let* b' := read_expr k b in Good (p', b')
                                       | _ => bad "arm"
                                       end) arms in
          Good (AEMatch s' arms')
      | SL false [SA "s"; SA "_"] => Good (AEReturn None)
      | SL false [SA "s"; e] => let* e' := read_expr k e in Good (AEReturn (Some e'))
      | SL false [SA "t"] => Good AEBreak
      | SL false (SA "u" :: _) => let* b := read_block k x in Good (AEBlock b)
      | SL false [SA "W"; SA op; e] => let* e' := read_expr k e in Good (AEUnary op e')
      | SL false [SA "X"; SA op; l; r] =>
          let* l' := read_expr k l in let* r' := read_expr k r in Good (AEBinary op l' r')
      | SL false [SA "Y"; SA t] => Good (AELit t)
      | SL false [SA "Z"; SL true ps; ret; body] =>
          let* ps' := rmap (fun p => match p with
                                     | SL false [SA "K"; SA n; t] =>
                                         let* t' := read_opt_ty k t in Good (n, t')
                                     | _ => bad "lambda parameter"
                                     end) ps in
          let* ret' := read_opt_ty k ret in
          let* b := read_expr k body in
          Good (AELam ps' ret' b)
      | _ => bad "expression"
      end
  end
with read_block (fuel : nat) (x : sx) : res ablock :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "u"; SL true ss; t] =>
          let* ss' := rmap (read_stmt k) ss in
          let* t' := (match t with
                      | SA "_" => Good None
                      | _ => let* e := read_expr k t in Good (Some e)
                      end) in
          Good (ABlock ss' t')
      | _ => bad "block"
      end
  end
with read_stmt (fuel : nat) (x : sx) : res astmt :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "v"; SA n; t; e] =>
          let* t' := read_opt_ty k t in let* e' := read_expr k e in Good (ASLet n t' e')
      | SL false [SA "w"; SA n; t; e] =>
          let* t' := read_opt_ty k t in let* e' := read_expr k e in Good (ASVar n t' e')
      | SL false [SA "x"; SA n; e] => let* e' := read_expr k e in Good (ASAssign n e')
      | SL false [SA "y"; _] => let* b := read_bind k x in Good (ASBind b)
      | SL false [SA "A"; c; b] =>
          let* c' := read_expr k c in let* b' := read_block k b in Good (ASWhile c' b')
      | SL false [SA "B"; b] => let* b' := read_block k b in Good (ASLoop b')
      | SL false [SA "C"; e] => let* e' := read_expr k e in Good (ASExpr e')
      | _ => bad "statement"
      end
  end
with read_bind (fuel : nat) (x : sx) : res (list (spec * option aexpr)) :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "y"; SL true its] =>
          rmap (fun it => match it with
                          | SL false [SA "z"; s; SA "_"] =>
                              let* s' := read_spec k s in Good (s', None)
                          | SL false [SA "z"; s; v] =>
                              let* s' := read_spec k s in
                              let* v' := read_expr k v in Good (s', Some v')
                          | _ => bad "bind item"
                          end) its
      | _ => bad "bind"
      end
  end.

Fixpoint read_decl (fuel : nat) (x : sx) : res adecl :=
  match fuel with
  | 0 => bad "too deep"
  | S k =>
      match x with
      | SL false [SA "M"; SL true its; SL true ds] =>
          let* its' := rmap (fun it => match it with
                                       | SL false [SA "I"; SA p; d; SA "_"] =>
                                           let* d' := opt_atom d in Good (split_path p, d')
                                       | _ => bad "assume item"
                                       end) its in
          let* ds' := rmap (read_decl k) ds in
          Good (ADAssume its' ds')
      | SL false [SA "N"; SA n] => Good (ADType n)
      | SL false [SA "O"; SA n; t] => let* t' := read_ty k t in Good (ADAlias n t')
      | SL false [SA "P"; SA n; SL true ps; t] =>
          let* ps' := rmap atom ps in let* t' := read_ty k t in Good (ADTag n ps' t')
      | SL false [SA "Q"; SA n] => Good (ADUnit n)
      | SL false [SA "R"; SA n; t; i] =>
          let* t' := read_ty k t in
          let* i' := (match i with
                      | SA "_" => Good None
                      | _ => let* e := read_expr k i in Good (Some e)
                      end) in
          Good (ADVal n t' i')
      | SL false [SA "S"; SA r; SA n; SL true ps; ret; body] =>
          let rcv := match r with "." => RDetached | "_" => RPlain | _ => RAttached r end in
          let* ps' := rmap (fun p => match p with
                                     | SL false [SA "K"; SA pn; t] =>
                                         let* t' := read_ty k t in Good (pn, t')
                                     | _ => bad "parameter"
                                     end) ps in
          let* ret' := read_opt_ty k ret in
          let* body' := (match body with
                         | SA "_" => Good None
                         | _ => let* b := read_block k body in Good (Some b)
                         end) in
          Good (ADFn rcv n ps' ret' body')
      | SL false [SA "T"; SA n; SL true ss] =>
          let* ss' := rmap (read_spec k) ss in Good (ADCtx n ss')
      | SL false (SA "U" :: SA n :: rest) =>
          (* 61b7b76: name args result body; the d84 branch adds the
             brackets after the name *)
          let* (ps, a, r, b) := match rest with
                                | [SL true a; SL true r; b] => Good (None, a, r, b)
                                | [SA "_"; SL true a; SL true r; b] => Good (None, a, r, b)
                                | [SL true ps; SL true a; SL true r; b] =>
                                    let* ps' := rmap atom ps in Good (Some ps', a, r, b)
                                | _ => bad "functor"
                                end in
          let* a' := rmap (read_spec k) a in
          let* r' := rmap (read_spec k) r in
          let* b' := (match b with
                      | SA "_" => Good None
                      | SL true bs => let* l := rmap (read_bind k) bs in Good (Some l)
                      | _ => bad "functor body"
                      end) in
          Good (ADFunctor n ps a' r' b')
      | _ => bad "declaration"
      end
  end.

(* `.name>alias`: a use item, with the dots that mark detached names. *)
Definition read_use_side (s : string) : bool * name :=
  match s with
  | String "."%char r => (true, r)
  | _ => (false, s)
  end.

Fixpoint split_gt (cs : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match cs with
  | [] => (rev acc, None)
  | ">"%char :: r => (rev acc, Some r)
  | c :: r => split_gt r (c :: acc)
  end.

Definition read_use (x : sx) : res ause :=
  let* s := atom x in
  let (l, r) := split_gt (list_ascii_of_string s) [] in
  let (d, n) := read_use_side (string_of_list_ascii l) in
  Good (AUse d n (option_map (fun r => read_use_side (string_of_list_ascii r)) r)).

Definition read_import (x : sx) : res aimport :=
  match x with
  | SL false [SA "L"; SA p; a; g; SL true us] =>
      let* a' := opt_atom a in
      let* us' := rmap read_use us in
      Good (AImport p a' (match g with SA "*" => true | _ => false end) us')
  | _ => bad "import"
  end.

Definition dump_fuel : nat := 100000.

Definition read_file (x : sx) : res afile :=
  match x with
  | SL false [SA "$"; SL true is; SL true ds] =>
      let* is' := rmap read_import is in
      let* ds' := rmap (read_decl dump_fuel) ds in
      Good (AFile is' ds')
  | _ => bad "file"
  end.

(* A bundle (py/bundle.py): the entry, the prelude, and each module's path,
   whether it imports the prelude, its imports' resolved paths, its dump. *)
Record amodule := AModule {
  am_path : string; am_prelude : bool; am_targets : list string; am_file : afile
}.

Record bundle := Bundle { b_entry : string; b_prelude : string; b_mods : list amodule }.

Definition read_module (m : sx) : res amodule :=
  match m with
  | SL false [SA "module"; SA path; SA w; SL false ts; f] =>
      let* ts' := rmap atom ts in
      let* f' := read_file f in
      Good (AModule path (String.eqb w "1") ts' f')
  | _ => bad "module"
  end.

Definition read_bundle (x : sx) : res bundle :=
  match x with
  | SL false (SA "bundle" :: SA e :: SA p :: ms) =>
      let* ms' := rmap read_module ms in
      Good (Bundle e p ms')
  | _ => bad "bundle"
  end.
End Input.


(* ######################################################################## *)
(* Part I.2  The concrete syntax: `Derives`, the front end's specification   *)
(* ######################################################################## *)

(* DRAFT, for the designer's review.  `Derives bytes f` says that the source
   file `bytes` has the surface AST `f` (above).  It is the specification
   of Parse.v's lexer and parser: Part VI's FRONT_END obliges
   `Parse.parse_moss` to be sound and complete for it; FrontEnd.v and
   FrontEndComplete.v prove both, and Certified.v binds them.

   It is in two layers, as docs/reference/syntax.md is:

   1. Lexical structure (`Lexes`): the bytes are a sequence of whitespace,
      comments and tokens, each token the longest that fits (`Lexeme`).
   2. A grammar over tokens, one relation per nonterminal, each named after
      the nonterminal of the thesis proposal's grammar figure (proposal.typ,
      <grammar>) that it refines, with that figure's letter: File F, Import
      m, Use u, Path p, Head h, Spec s, Reference r, Type τ, Declaration d,
      Function f, Block b, Statement i, Bind w, Binding β, Expression e,
      Binary ⊕, Unary ⊖, Conditional c, Arm a, Pattern π.

   HOW TO READ A RULE.  A grammar relation `X ts v rest` says: the token
   list `ts` begins with an X whose AST is `v`, and `rest` is what follows
   it (so `ts` = the X's tokens ++ `rest`).  A rule's premises read left to
   right as the production does: `Tk "LET" ts0 ts1` is the token `let`,
   `TkName ts1 x ts2` a name `x`, `Expr false ts2 e ts3` an expression, and
   so on, each starting where the last ended.  Premises about `kind ts`
   (the kind of the next token, not consumed) are LOOKAHEAD conditions:
   they are what makes the grammar unambiguous, and each says, in the
   comment above its rule, why it is there.

   Unlike the figure, the grammar is unambiguous: every token list has at
   most one parse.  Where the figure is ambiguous the refinement is one of
   these, each a design decision of the language:
   - precedence and associativity of operators: D68's partial order, the
     layers Operators > Comparison/Bitwise > Operand > Additive > Term >
     Unary below.  Two operators that the order does not relate have NO
     derivation when they meet unparenthesized (`a & b | c`, `a < b < c`,
     `1 << n + 1`): that is the syntax error;
   - where an expression ends: every repetition (a path's `::`, a chain of
     `*`, suffixes `.f`, a union's `|`, ...) takes as much as it can (the
     lookahead conditions "not followed by ..."), and an operator
     expression may not be followed by a binary operator at all (D68);
   - where a record expression `p { ... }` and a block may stand (not in
     the condition of an `if`/`while`, the scrutinee of a `match`, the
     collection of a `for`: the `nr` flag, "no record");
   - statements: a `;` may be left out after an expression statement only
     when the statement STARTS with `if`, `match` or `{` (see `Stmts`);
   - `for` is sugar: `Derives` relates a `for` statement directly to its
     desugaring `for_desugar` (D85), since the AST, the elaborator's input,
     has no `for` node.  Why: the AST is what Part V's elaboration relation
     reads, and D85 defines `for` by this desugaring, so stating it here,
     in eight lines, is the most direct reading of D85; adding a node would
     change the AST, the dump, the elaborator and Part V for no gain in
     what is guaranteed.

   CHANGES THE PROOFS FORCED (the grammar shaped to the parser).  Each is a
   place where the grammar says a little more than the language needs, in
   the parser's terms, so that soundness and completeness are proved by
   following the parser step by step; none changes which files derive what,
   except where marked [!]:
   (a) Tokens are consumed as the parser consumes them, `ts' = advance ts`,
       which never steps past the final EOF token: this only matters for
       token lists that do not end in EOF, which `Lexes` never produces.
   (b) Exit conditions of every repetition are stated in the rule that ends
       it ("not followed by `,`/`::`/`*`/`.`/`|`..."), where the parser
       peeks, although the enclosing rule usually implies them.  Likewise
       the statement rules repeat "not `x = ...`" (`assign_ahead`), which
       the expression rules imply, and an arm body not starting with `{`
       is the expression form (a `{` there is always the block form).
   (c) Literals: the AST of a numeric literal is its text normalized by
       `lit_norm` (an exponent as its value: `1E+03` is `1e3`), and of an
       import path the string's value (`string_value`, escapes decoded):
       the parser does both, and the dump format fixes them.
   (d) [!] The source is read with universal newlines (`newlines`: `\r\n`
       and a lone `\r` are `\n`), as the parser reads it.
   (e) The lambda/parenthesis decision is the parser's two-to-three-token
       lookahead `at_lambda` (D70), stated as such. *)

Module Syntax.
Import Stdlib.Strings.Byte.
Import Input.
Local Open Scope string_scope.

(* ===================== Tokens ============================================= *)

(* A token: its kind (lex.py's `Kind` name: "NAME", "LPAREN", "EOF", ...),
   its text, and its byte offset (only error messages use it). *)
Record token := Tok { tk : string; tx : string; toff : nat }.

(* Character classes. *)
Definition is_digit (b : byte) : bool := match b with "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true | _ => false end%byte.
Definition is_alpha (b : byte) : bool := match b with "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" => true | _ => false end%byte.
Definition is_name_start (b : byte) : bool := is_alpha b || Byte.eqb b "_"%byte.
Definition is_name_cont (b : byte) : bool := is_name_start b || is_digit b.
Definition is_ws (b : byte) : bool := match b with " " | x09 | x0d | x0a => true | _ => false end%byte.
Definition is_e (b : byte) : bool := match b with "e" | "E" => true | _ => false end%byte.
Definition is_sign (b : byte) : bool := match b with "+" | "-" => true | _ => false end%byte.
Definition is_escape (b : byte) : bool := match b with """" | "\" | "n" | "r" | "t" => true | _ => false end%byte.

(* The keywords, and the kind of each. *)
Definition KEYWORDS : list (string * string) :=
  [("as", "AS"); ("assume", "ASSUME"); ("functor", "FUNCTOR"); ("bind", "BIND");
   ("break", "BREAK"); ("context", "CONTEXT"); ("else", "ELSE"); ("fn", "FN");
   ("for", "FOR"); ("if", "IF"); ("import", "IMPORT"); ("let", "LET");
   ("loop", "LOOP"); ("match", "MATCH"); ("return", "RETURN"); ("tag", "TAG");
   ("This", "THIS_TYPE"); ("this", "THIS"); ("type", "TYPE"); ("use", "USE");
   ("val", "VAL"); ("var", "VAR"); ("while", "WHILE"); ("in", "IN");
   (* (f) `_` alone is a word of its own, the wildcard pattern, and not a
      name (`_x` and `x_` are names) *)
   ("_", "UNDERSCORE")].

Fixpoint assoc (k : string) (l : list (string * string)) : option string :=
  match l with [] => None | (a, b) :: r => if String.eqb a k then Some b else assoc k r end.

(* The symbols: one character, and two (the longer always wins). *)
Definition one_char (b : byte) : option string :=
  match b with
  | "!"%byte => Some "EXCLAM" | "%"%byte => Some "PERCENT" | "&"%byte => Some "AMPERSAND"
  | "("%byte => Some "LPAREN" | ")"%byte => Some "RPAREN" | "*"%byte => Some "STAR"
  | "+"%byte => Some "PLUS" | ","%byte => Some "COMMA" | "-"%byte => Some "HYPHEN"
  | "."%byte => Some "DOT" | "/"%byte => Some "SLASH" | ":"%byte => Some "COLON"
  | ";"%byte => Some "SEMI" | "<"%byte => Some "LESS" | "="%byte => Some "EQUAL"
  | ">"%byte => Some "GREATER" | "["%byte => Some "LBRACKET" | "]"%byte => Some "RBRACKET"
  | "^"%byte => Some "CARET" | "{"%byte => Some "LBRACE" | "|"%byte => Some "PIPE"
  | "}"%byte => Some "RBRACE"
  | _ => None
  end.

Definition two_char (a b : byte) : option string :=
  match a, b with
  | "!"%byte, "="%byte => Some "EXCLAM_EQUAL" | ":"%byte, ":"%byte => Some "COLON_COLON"
  | "="%byte, ">"%byte => Some "EQUAL_GREATER" | "<"%byte, "<"%byte => Some "LESS_LESS"
  | "<"%byte, "="%byte => Some "LESS_EQUAL" | "="%byte, "="%byte => Some "EQUAL_EQUAL"
  | ">"%byte, "="%byte => Some "GREATER_EQUAL" | ">"%byte, ">"%byte => Some "GREATER_GREATER"
  | "-"%byte, ">"%byte => Some "HYPHEN_GREATER"
  | _, _ => None
  end.

(* Does the input start with a byte of the class? *)
Definition starts (p : byte -> bool) (cs : list byte) : bool :=
  match cs with c :: _ => p c | [] => false end.

Fixpoint strip_zeros (ds : list byte) : list byte :=
  match ds with "0"%byte :: r => strip_zeros r | _ => ds end.

(* (d) Universal newlines: `\r\n` and a lone `\r` read as `\n`. *)
Fixpoint newlines (cs : list byte) : list byte :=
  match cs with
  | x0d :: x0a :: r => x0a :: newlines r
  | x0d :: r => x0a :: newlines r
  | c :: r => c :: newlines r
  | [] => []
  end.

(* A string literal's body: plain bytes (not `"`, `\` or a newline) and the
   escapes `\"`, `\\`, `\n`, `\r`, `\t`. *)
Inductive StrBody : list byte -> Prop :=
| SBEnd : StrBody []
| SBByte : forall c r, c <> """"%byte -> c <> "\"%byte -> c <> x0a -> StrBody r -> StrBody (c :: r)
| SBEsc : forall d r, is_escape d = true -> StrBody r -> StrBody ("\"%byte :: d :: r).

(* A numeric literal's parts (D68): a fraction `.` digit+, and an exponent
   `e`/`E`, an optional sign, digit+ of value at most 999. *)
Definition Fraction (frac : list byte) : Prop :=
  frac = [] \/ exists d ds, frac = "."%byte :: d :: ds /\ forallb is_digit (d :: ds) = true.
Definition Exponent (ex : list byte) : Prop :=
  ex = [] \/ exists e sign d ds, ex = e :: sign ++ d :: ds /\ is_e e = true /\
              (sign = [] \/ exists s, sign = [s] /\ is_sign s = true) /\
              forallb is_digit (d :: ds) = true /\ length (strip_zeros (d :: ds)) <= 3.

(* `Lexeme cs k rest`: the bytes `cs` are one token of kind `k` when `rest`
   follows; the conditions on `rest` make it the longest token there. *)
Inductive Lexeme : list byte -> string -> list byte -> Prop :=
| LxSym2 : forall a b k rest, two_char a b = Some k -> Lexeme [a; b] k rest
| LxSym1 : forall a k rest, one_char a = Some k ->
    (forall b r, rest = b :: r -> two_char a b = None) -> Lexeme [a] k rest
(* a name or keyword (`in` is a keyword, of `for`) *)
| LxWord : forall c ns rest,
    is_name_start c = true -> forallb is_name_cont ns = true -> starts is_name_cont rest = false ->
    Lexeme (c :: ns) (match assoc (string_of_list_byte (c :: ns)) KEYWORDS with
                      | Some k => k | None => "NAME" end) rest
| LxString : forall body rest, StrBody body ->
    Lexeme (""""%byte :: body ++ [""""%byte]) "STRING" rest
(* digits, then the optional fraction and exponent; a numeric literal may
   not run into a letter, digit or `_`, and a digit run followed by `.` and
   a digit takes the fraction *)
| LxNumber : forall d ds frac ex rest,
    forallb is_digit (d :: ds) = true -> Fraction frac -> Exponent ex ->
    starts is_name_cont rest = false ->
    (frac = [] -> ex = [] -> forall c r, rest = "."%byte :: c :: r -> is_digit c = false) ->
    Lexeme (d :: ds ++ frac ++ ex)
           (match frac, ex with [], [] => "INTEGER" | _, _ => "DECIMAL" end) rest.

(* `Lexes i cs ts`: the bytes `cs`, starting at offset `i`, are the tokens
   `ts`: whitespace and comments (`#` to the end of the line) between
   tokens, and the token "EOF" at the end. *)
Inductive Lexes : nat -> list byte -> list token -> Prop :=
| LexEnd : forall i, Lexes i [] [Tok "EOF" "" i]
| LexSpace : forall i c r ts, is_ws c = true -> Lexes (S i) r ts -> Lexes i (c :: r) ts
| LexComment : forall i com r ts,
    forallb (fun b => negb (Byte.eqb b x0a)) com = true ->
    starts (fun b => negb (Byte.eqb b x0a)) r = false ->
    Lexes (S i + length com) r ts -> Lexes i ("#"%byte :: com ++ r) ts
| LexToken : forall i cs k r ts, Lexeme cs k r -> Lexes (i + length cs) r ts ->
    Lexes i (cs ++ r) (Tok k (string_of_list_byte cs) i :: ts).

(* ===================== Reading tokens ===================================== *)

(* The next token (the final EOF stands for itself and for anything after
   it), the n-th token ahead, and what follows the next one. *)
Definition eof_tok : token := Tok "EOF" "" 0.
Definition peek (ts : list token) : token := match ts with t :: _ => t | [] => eof_tok end.
Fixpoint peekn (n : nat) (ts : list token) : token :=
  match ts with
  | [] => eof_tok
  | [t] => t
  | t :: r => match n with 0 => t | S m => peekn m r end
  end.
Definition kind (ts : list token) : string := tk (peek ts).
Definition kind_at (n : nat) (ts : list token) : string := tk (peekn n ts).
Definition advance (ts : list token) : list token :=
  match ts with [_] => ts | _ :: r => r | [] => [] end.

(* `Tk k ts ts'`: a token of kind k, then ts'.  `TkText k ts x ts'`: the
   same, whose text is x; `TkName` for a name. *)
Definition Tk (k : string) (ts ts' : list token) : Prop := kind ts = k /\ ts' = advance ts.
Definition TkText (k : string) (ts : list token) (x : string) (ts' : list token) : Prop :=
  kind ts = k /\ tx (peek ts) = x /\ ts' = advance ts.
Definition TkName := TkText "NAME".
(* An optional token. *)
Definition OptTk (k : string) (ts ts' : list token) : Prop :=
  Tk k ts ts' \/ (kind ts <> k /\ ts' = ts).

(* List[X]: X, separated by `,`, with an optional trailing comma, up to
   (not including) one of the `closers`. *)
Inductive CommaList {A : Type} (X : list token -> A -> list token -> Prop) (closers : list string)
  : list token -> list A -> list token -> Prop :=
| CLNil : forall ts, In (kind ts) closers -> CommaList X closers ts [] ts
| CLLast : forall ts x ts1, ~ In (kind ts) closers -> X ts x ts1 -> kind ts1 <> "COMMA" ->
    CommaList X closers ts [x] ts1
| CLCons : forall ts x ts1 ts2 xs ts3, ~ In (kind ts) closers -> X ts x ts1 -> Tk "COMMA" ts1 ts2 ->
    CommaList X closers ts2 xs ts3 -> CommaList X closers ts (x :: xs) ts3.

(* ===================== Names: Path p, Head h, Spec s, Reference r ========= *)

(* p ::= x | p::x *)
Inductive Path : list token -> path -> list token -> Prop :=
| PathOne : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> "COLON_COLON" -> Path ts [x] ts1
| PathMore : forall ts x ts1 ts2 p ts3, TkName ts x ts1 -> Tk "COLON_COLON" ts1 ts2 ->
    Path ts2 p ts3 -> Path ts (x :: p) ts3.

(* h ::= p | p.x | .x *)
Inductive Head : list token -> option path * option name -> list token -> Prop :=
| HeadPath : forall ts p ts1, Path ts p ts1 -> kind ts1 <> "DOT" -> Head ts (Some p, None) ts1
| HeadMeth : forall ts p ts1 ts2 x ts3, Path ts p ts1 -> Tk "DOT" ts1 ts2 -> TkName ts2 x ts3 ->
    Head ts (Some p, Some x) ts3
| HeadDot : forall ts ts1 x ts2, Tk "DOT" ts ts1 -> TkName ts1 x ts2 -> Head ts (None, Some x) ts2.

(* s ::= h | h[p = s, ...]   (the brackets are `App`, optional) *)
Inductive Spec_ : list token -> spec -> list token -> Prop :=
| SpecG : forall ts p d ts1 a ts2, Head ts (p, d) ts1 -> App ts1 a ts2 -> Spec_ ts (Input.Spec p d a) ts2
with App : list token -> sapp -> list token -> Prop :=
| AppNone : forall ts, kind ts <> "LBRACKET" -> App ts None ts
| AppSome : forall ts ts1 bs ts2 ts3, Tk "LBRACKET" ts ts1 ->
    CommaList AppBinding ["RBRACKET"] ts1 bs ts2 -> Tk "RBRACKET" ts2 ts3 -> App ts (Some bs) ts3
with AppBinding : list token -> path * spec -> list token -> Prop :=
| AppBindingG : forall ts p ts1 ts2 s ts3, Path ts p ts1 -> Tk "EQUAL" ts1 ts2 -> Spec_ ts2 s ts3 ->
    AppBinding ts (p, s) ts3.

(* r ::= p | p[p = s, ...] *)
Definition Reference (ts : list token) (r : path * sapp) (rest : list token) : Prop :=
  exists ts1, Path ts (fst r) ts1 /\ App ts1 (snd r) rest.

(* ===================== Types τ ============================================ *)

(* τ ::= r | This | () | (τ) | {x: τ, ...} | τ | τ | `|`
   Refined: a type is a bare `|` (the uninhabited type), or one or more
   atoms separated by `|`, optionally preceded by `|`; several atoms are a
   union, one is itself. *)
Inductive Type_ : list token -> aty -> list token -> Prop :=
| TyNever : forall ts ts1, Tk "PIPE" ts ts1 ->
    ~ In (kind ts1) ["NAME"; "THIS_TYPE"; "LPAREN"; "LBRACE"] -> Type_ ts ATNever ts1
| TyMembers : forall ts ts1 ms ts2, OptTk "PIPE" ts ts1 -> Members ts1 ms ts2 ->
    Type_ ts (match ms with [m] => m | _ => ATUnion ms end) ts2
with Members : list token -> list aty -> list token -> Prop :=
| MemOne : forall ts t ts1, TypeAtom ts t ts1 -> kind ts1 <> "PIPE" -> Members ts [t] ts1
| MemMore : forall ts t ts1 ts2 ms ts3, TypeAtom ts t ts1 -> Tk "PIPE" ts1 ts2 ->
    Members ts2 ms ts3 -> Members ts (t :: ms) ts3
with TypeAtom : list token -> aty -> list token -> Prop :=
| TARef : forall ts p ts1 a ts2, Path ts p ts1 -> App ts1 a ts2 -> TypeAtom ts (ATRef p a) ts2
| TAThis : forall ts ts1, Tk "THIS_TYPE" ts ts1 -> TypeAtom ts ATThis ts1
| TAUnit : forall ts ts1 ts2, Tk "LPAREN" ts ts1 -> Tk "RPAREN" ts1 ts2 -> TypeAtom ts ATUnit ts2
| TAParen : forall ts ts1 t ts2 ts3, Tk "LPAREN" ts ts1 -> Type_ ts1 t ts2 -> Tk "RPAREN" ts2 ts3 ->
    TypeAtom ts t ts3
| TARecord : forall ts t ts1, RecordType ts t ts1 -> TypeAtom ts t ts1
with RecordType : list token -> aty -> list token -> Prop :=
| RecordTypeG : forall ts ts1 fs ts2 ts3, Tk "LBRACE" ts ts1 ->
    CommaList TypeField ["RBRACE"] ts1 fs ts2 -> Tk "RBRACE" ts2 ts3 -> RecordType ts (ATRec fs) ts3
with TypeField : list token -> name * aty -> list token -> Prop :=
| TypeFieldG : forall ts x ts1 ts2 t ts3, TkName ts x ts1 -> Tk "COLON" ts1 ts2 -> Type_ ts2 t ts3 ->
    TypeField ts (x, t) ts3.

(* (`:` τ)? *)
Inductive OptType : list token -> option aty -> list token -> Prop :=
| OptTypeNone : forall ts, kind ts <> "COLON" -> OptType ts None ts
| OptTypeSome : forall ts ts1 t ts2, Tk "COLON" ts ts1 -> Type_ ts1 t ts2 -> OptType ts (Some t) ts2.

(* ===================== Patterns π ========================================= *)

(* π ::= _ | p | p(π) | {x = π, ...} | p{x = π, ...}   (`= π` optional);
   `_` is its own token, not a name (f) *)
Inductive Pattern : list token -> apat -> list token -> Prop :=
| PatWild : forall ts ts1, Tk "UNDERSCORE" ts ts1 -> Pattern ts APWild ts1
(* a path not followed by `(` or `{` *)
| PatPath : forall ts p ts1, Path ts p ts1 -> ~ In (kind ts1) ["LBRACE"; "LPAREN"] ->
    Pattern ts (APPath p) ts1
| PatTag : forall ts p ts1 ts2 q ts3 ts4, Path ts p ts1 -> Tk "LPAREN" ts1 ts2 ->
    Pattern ts2 q ts3 -> Tk "RPAREN" ts3 ts4 -> Pattern ts (APTag p q) ts4
| PatRecord : forall ts p ts1 fs ts2, Path ts p ts1 -> RecordPattern ts1 fs ts2 ->
    Pattern ts (APRec (Some p) fs) ts2
| PatAnon : forall ts fs ts1, RecordPattern ts fs ts1 -> Pattern ts (APRec None fs) ts1
with RecordPattern : list token -> list (name * option apat) -> list token -> Prop :=
| RecordPatternG : forall ts ts1 fs ts2 ts3, Tk "LBRACE" ts ts1 ->
    CommaList PatField ["RBRACE"] ts1 fs ts2 -> Tk "RBRACE" ts2 ts3 -> RecordPattern ts fs ts3
with PatField : list token -> name * option apat -> list token -> Prop :=
| PatFieldShort : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> "EQUAL" -> PatField ts (x, None) ts1
| PatFieldLong : forall ts x ts1 ts2 q ts3, TkName ts x ts1 -> Tk "EQUAL" ts1 ts2 ->
    Pattern ts2 q ts3 -> PatField ts (x, Some q) ts3.

(* ===================== Operators: Binary ⊕, Unary ⊖ ======================= *)

(* ⊕: the token kinds of the binary operators, and the operator each is. *)
Definition BINARY : list (string * string) :=
  [("STAR", "*"); ("SLASH", "/"); ("PERCENT", "%"); ("PLUS", "+"); ("HYPHEN", "-");
   ("LESS_LESS", "<<"); ("GREATER_GREATER", ">>"); ("EQUAL_EQUAL", "==");
   ("EXCLAM_EQUAL", "!="); ("LESS", "<"); ("GREATER", ">"); ("LESS_EQUAL", "<=");
   ("GREATER_EQUAL", ">="); ("AMPERSAND", "&"); ("CARET", "^"); ("PIPE", "|")].
(* D68's classes *)
Definition FRACTION := ["/"; "%"].
Definition ADDITIVE := ["+"; "-"].
Definition SHIFT := ["<<"; ">>"].
Definition COMPARISON := ["=="; "!="; "<"; ">"; "<="; ">="].
Definition BITWISE := ["&"; "^"; "|"].
Definition mem (s : string) (l : list string) : bool := existsb (String.eqb s) l.
(* the binary operator next, if any *)
Definition binop (ts : list token) : option string := assoc (kind ts) BINARY.
Definition binop_in (l : list string) (ts : list token) : bool :=
  match binop ts with Some o => mem o l | None => false end.

(* `ops`-chains, left-associated: `acc op1 X1 op2 X2 ...`, every op one of
   `ops`, as long as the next operator is one (D68: `a - b + c` is
   `(a - b) + c`). *)
Inductive LeftChain (X : list token -> aexpr -> list token -> Prop) (ops : list string)
  : aexpr -> list token -> aexpr -> list token -> Prop :=
| ChainEnd : forall acc ts, binop_in ops ts = false -> LeftChain X ops acc ts acc ts
| ChainStep : forall acc ts op r ts1 e ts2, binop ts = Some op -> In op ops ->
    X (advance ts) r ts1 -> LeftChain X ops (AEBinary op acc r) ts1 e ts2 ->
    LeftChain X ops acc ts e ts2.

(* ===================== Literals and lookahead ============================= *)

(* A literal's AST text: an exponent as its value (`1E+03` is `1e3`,
   `2e-0` is `2e0`); (c). *)
Fixpoint digits_value (ds : list byte) (acc : nat) : nat :=
  match ds with
  | d :: r => digits_value r (acc * 10 + (Byte.to_nat d - 48))
  | [] => acc
  end.
Definition hexdig (n : nat) : string :=
  match n with 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4" | 5 => "5"
  | 6 => "6" | 7 => "7" | 8 => "8" | 9 => "9" | 10 => "a" | 11 => "b" | 12 => "c"
  | 13 => "d" | 14 => "e" | _ => "f" end.
Fixpoint nat_digits (fuel n : nat) (acc : string) : string :=
  match fuel with
  | 0 => acc
  | S f => let acc := hexdig (n mod 10) ++ acc in
           if Nat.ltb n 10 then acc else nat_digits f (n / 10) acc
  end.
Definition show_nat (n : nat) : string := nat_digits 25 n "".
Fixpoint span (p : byte -> bool) (cs : list byte) : list byte * list byte :=
  match cs with
  | c :: r => if p c then let (a, b) := span p r in (c :: a, b) else ([], cs)
  | [] => ([], [])
  end.

(* D88: line-leading > followed by one space or end of line is raw text.
   Expand raw runs to parenthesized quoted expressions before the ordinary
   grammar. Each emitted byte retains its source offset for diagnostics.
   Modes: 0 ordinary, 1 quoted, 2 escaped, 3 comment, 4 raw. *)
Definition raw_indent (c : byte) : bool := Byte.eqb c " "%byte || Byte.eqb c x09.
Definition raw_after (cs : list byte) : bool :=
  match cs with [] => true | c :: _ => Byte.eqb c " "%byte || Byte.eqb c x0a end.
Definition raw_marker (cs : list byte) : bool :=
  match cs with ">"%byte :: r => raw_after r | _ => false end.
Definition raw_escape (c : byte) : list byte :=
  if Byte.eqb c "\"%byte || Byte.eqb c """"%byte then ["\"%byte; c] else [c].

Fixpoint raw_expand (fuel : nat) (cs : list byte) (off mode : nat) (bol : bool)
  : list (byte * nat) :=
  match fuel with
  | 0 => []
  | S fuel =>
    match cs with
    | [] => if Nat.eqb mode 4 then [("\"%byte, off); ("n"%byte, off); (""""%byte, off); (")"%byte, off)] else []
    | c :: r =>
      let emit bs tail := app (map (fun b => (b, off)) bs) tail in
      if Nat.eqb mode 4 then
        if Byte.eqb c x0a then
          let '(indent, rest) := span raw_indent r in
          if raw_marker rest then
            let after := tl rest in
            let '(skip, body) := match after with " "%byte :: body => (1, body) | _ => (0, after) end in
            emit ["\"%byte; "n"%byte]
              (raw_expand fuel body (off + 2 + length indent + skip) 4 false)
          else emit ["\"%byte; "n"%byte; """"%byte; ")"%byte; c] (raw_expand fuel r (S off) 0 true)
        else emit (raw_escape c) (raw_expand fuel r (S off) 4 false)
      else if Nat.eqb mode 1 then
        emit [c] (raw_expand fuel r (S off)
          (if Byte.eqb c "\"%byte then 2 else if Byte.eqb c """"%byte then 0 else 1) false)
      else if Nat.eqb mode 2 then emit [c] (raw_expand fuel r (S off) 1 false)
      else if Nat.eqb mode 3 then
        emit [c] (raw_expand fuel r (S off) (if Byte.eqb c x0a then 0 else 3) (Byte.eqb c x0a))
      else if bol && Byte.eqb c ">"%byte && raw_after r then
        let '(skip, body) := match r with " "%byte :: body => (1, body) | _ => (0, r) end in
        emit ["("%byte; """"%byte] (raw_expand fuel body (S off + skip) 4 false)
      else emit [c] (raw_expand fuel r (S off)
        (if Byte.eqb c "#"%byte then 3 else if Byte.eqb c """"%byte then 1 else 0)
        (Byte.eqb c x0a || (bol && raw_indent c)))
    end
  end.
Definition raw_source (cs : list byte) : list (byte * nat) :=
  raw_expand (S (length cs)) cs 0 0 true.
Definition source_bytes (cs : list byte) : list byte := map fst (raw_source (newlines cs)).


Definition lit_norm (t : string) : string :=
  let cs := list_byte_of_string t in
  let (pre, post) := span (fun b => negb (is_e b)) cs in
  match post with
  | [] => t
  | _ :: ex =>
      let '(neg, ds) := match ex with
                        | "-"%byte :: ds => (true, ds)
                        | "+"%byte :: ds => (false, ds)
                        | ds => (false, ds)
                        end in
      let v := digits_value ds 0 in
      string_of_list_byte pre ++ "e" ++ (if neg && negb (Nat.eqb v 0) then "-" else "") ++ show_nat v
  end.

(* A string token's value: the quotes dropped, the escapes decoded; (c). *)
Fixpoint unescape (cs : list byte) : list byte :=
  match cs with
  | "\"%byte :: d :: r =>
      (match d with "n" => x0a | "r" => x0d | "t" => x09 | _ => d end)%byte :: unescape r
  | c :: r => c :: unescape r
  | [] => []
  end.
Definition string_value (t : string) : string :=
  string_of_list_byte (unescape (removelast (tl (list_byte_of_string t)))).

(* Quoted strings are ordinary contextual calls over validated UTF-8 scalars.
   Payload bytes name byte::x00 .. byte::xff vals; only byte lengths and
   offsets use integer literals. Three/four-byte prefixes are ordinary record
   constructors expressing valid lead/second-byte combinations in their types.
   No primitive or new expression is involved. *)
Definition byte_name (b : byte) : string :=
  let n := Byte.to_nat b in "x" ++ hexdig (n / 16) ++ hexdig (n mod 16).
Definition byte_between (lo hi : nat) (b : byte) : bool :=
  Nat.leb lo (Byte.to_nat b) && Nat.leb (Byte.to_nat b) hi.
Definition utf8_cont (b : byte) : bool := byte_between 128 191 b.
Definition utf8_three (a b : byte) : bool :=
  (byte_between 224 224 a && byte_between 160 191 b) ||
  (byte_between 225 236 a && utf8_cont b) ||
  (byte_between 237 237 a && byte_between 128 159 b) ||
  (byte_between 238 239 a && utf8_cont b).
Definition utf8_four (a b : byte) : bool :=
  (byte_between 240 240 a && byte_between 144 191 b) ||
  (byte_between 241 243 a && utf8_cont b) ||
  (byte_between 244 244 a && byte_between 128 143 b).
Fixpoint utf8_valid (bytes : list byte) : bool :=
  match bytes with
  | [] => true
  | a :: rest =>
      if byte_between 0 127 a then utf8_valid rest else
      match rest with
      | b :: rest =>
          if byte_between 194 223 a && utf8_cont b then utf8_valid rest else
          match rest with
          | c :: rest =>
              if utf8_three a b && utf8_cont c then utf8_valid rest else
              match rest with
              | d :: rest => utf8_four a b && utf8_cont c && utf8_cont d && utf8_valid rest
              | [] => false
              end
          | [] => false
          end
      | [] => false
      end
  end.
Definition string_utf8_valid (value : string) : bool := utf8_valid (list_byte_of_string value).
Arguments string_utf8_valid : simpl never.
Definition utf8_width (b : byte) : nat :=
  if byte_between 0 127 b then 1 else
  if byte_between 194 223 b then 2 else
  if byte_between 224 239 b then 3 else
  if byte_between 240 244 b then 4 else 0.
Definition byte_expr (b : byte) : aexpr := AEPath ["byte"; byte_name b] None.
Definition string_prefix (head : string) (a b : byte) : aexpr :=
  AERec ["byte"; head] None [("lead", Some (byte_expr a)); ("second", Some (byte_expr b))].
Definition string_chunk_args (payload : list byte) : list aexpr :=
  match payload with
  | [a; b; c] =>
      [string_prefix (if byte_between 224 224 a then "ThreeE0" else
                      if byte_between 237 237 a then "ThreeED" else "ThreeOrdinary") a b;
       byte_expr c]
  | [a; b; c; d] =>
      [string_prefix (if byte_between 240 240 a then "FourF0" else
                      if byte_between 244 244 a then "FourF4" else "FourMiddle") a b;
       byte_expr c; byte_expr d]
  | _ => map byte_expr payload
  end.
Definition string_write_chunk (payload : list byte) (index : nat) : astmt :=
  ASAssign "return"
    (AECall ["string_write" ++ show_nat (List.length payload)] None
       ([AEPath ["return"] None; AELit (show_nat index)] ++ string_chunk_args payload)).
Fixpoint string_writes (bytes : list byte) (index : nat) : list astmt :=
  match bytes with
  | [] => []
  | a :: rest =>
      match utf8_width a with
      | 1 => string_write_chunk [a] index :: string_writes rest (index + 1)
      | 2 => match rest with
             | b :: rest => string_write_chunk [a; b] index :: string_writes rest (index + 2)
             | _ => [] end
      | 3 => match rest with
             | b :: c :: rest => string_write_chunk [a; b; c] index :: string_writes rest (index + 3)
             | _ => [] end
      | 4 => match rest with
             | b :: c :: d :: rest => string_write_chunk [a; b; c; d] index :: string_writes rest (index + 4)
             | _ => [] end
      | _ => []
      end
  end.
Definition string_desugar (value : string) : aexpr :=
  let bytes := list_byte_of_string value in
  AEBlock (ABlock
    (ASVar "return" None (AECall ["string_begin"] None
       [AELit (show_nat (List.length bytes))]) :: string_writes bytes 0)
    (Some (AECall ["string_finish"] None [AEPath ["return"] None]))).

(* (e) D70: at `(`, a lambda is next: `()` or `(x)` followed by `=>` or
   `:`, or `(x` followed by `:` or `,`. *)
Definition at_lambda (ts : list token) : bool :=
  let arrow k := String.eqb k "EQUAL_GREATER" || String.eqb k "COLON" in
  if String.eqb (kind_at 1 ts) "RPAREN" then arrow (kind_at 2 ts)
  else if String.eqb (kind_at 1 ts) "NAME" then
    if String.eqb (kind_at 2 ts) "COLON" || String.eqb (kind_at 2 ts) "COMMA" then true
    else String.eqb (kind_at 2 ts) "RPAREN" && arrow (kind_at 3 ts)
  else false.

(* D68: `-` before a literal with a method call or field (`-1.abs()`) is
   refused: `-` binds looser than `.`, and nobody means `-(1.abs())`. *)
Definition neg_literal_dot (ts : list token) : Prop :=
  In (kind ts) ["INTEGER"; "DECIMAL"] /\ kind_at 1 ts = "DOT".

(* A statement `x = e;` is next. *)
Definition assign_ahead (ts : list token) : Prop := kind ts = "NAME" /\ kind_at 1 ts = "EQUAL".

(* D85: `for x in coll { body }` is
   `{ let for = coll; var while = for.begin();
      while for.more(while) { let x = for.get(while); while = for.after(while); { body } } }`
   with the four calls as `for.`-operators; the cursor's names `for` and
   `while` are keywords, so no program can name them. *)
Definition ref (x : string) : aexpr := AEPath [x] None.
Definition for_desugar (x : name) (coll : aexpr) (body : ablock) : astmt :=
  let c := "for" in let i := "while" in
  let loop := ASWhile (AEBinary "for.more" (ref c) (ref i))
                (ABlock [ASLet x None (AEBinary "for.get" (ref c) (ref i));
                         ASAssign i (AEBinary "for.after" (ref c) (ref i));
                         ASExpr (AEBlock body)] None) in
  ASExpr (AEBlock (ABlock [ASLet c None coll;
                           ASVar i None (AEUnary "for.begin" (ref c));
                           loop] None)).

(* ===================== Expressions e, Blocks b, Statements i ============== *)

(* Every relation below that takes `nr` ("no record") is the same in two
   contexts: `nr = true` in the condition of an `if`/`while`, the scrutinee
   of a `match` and the collection of a `for`, where a path followed by `{`
   is not a record expression and a `{` does not open a block expression
   (the `{` there opens the body); `nr = false` everywhere else.
   Parenthesized, an expression is in `nr = false` again. *)

(* e ::= c | match e {a ...} | return e? | break | (the operator forms)
   `return` takes an expression unless `;`, `,`, `}` or `)` follows. *)
Inductive Expr : bool -> list token -> aexpr -> list token -> Prop :=
| ECond : forall nr ts e ts1, Cond ts e ts1 -> Expr nr ts e ts1
| EMatch : forall nr ts ts1 s ts2 ts3 arms ts4 ts5, Tk "MATCH" ts ts1 -> Expr true ts1 s ts2 ->
    Tk "LBRACE" ts2 ts3 -> Arms ts3 arms ts4 -> Tk "RBRACE" ts4 ts5 -> Expr nr ts (AEMatch s arms) ts5
| EReturnNone : forall nr ts ts1, Tk "RETURN" ts ts1 -> In (kind ts1) ["SEMI"; "COMMA"; "RBRACE"; "RPAREN"] ->
    Expr nr ts (AEReturn None) ts1
| EReturn : forall nr ts ts1 e ts2, Tk "RETURN" ts ts1 -> ~ In (kind ts1) ["SEMI"; "COMMA"; "RBRACE"; "RPAREN"] ->
    Expr nr ts1 e ts2 -> Expr nr ts (AEReturn (Some e)) ts2
| EBreak : forall nr ts ts1, Tk "BREAK" ts ts1 -> Expr nr ts AEBreak ts1
| EOperators : forall nr ts e ts1, Operators nr ts e ts1 -> Expr nr ts e ts1

(* c ::= if e b (else b | else c)? *)
with Cond : list token -> aexpr -> list token -> Prop :=
| CondIf : forall ts ts1 c ts2 t ts3, Tk "IF" ts ts1 -> Expr true ts1 c ts2 -> Block ts2 t ts3 ->
    kind ts3 <> "ELSE" -> Cond ts (AEIf c t None) ts3
| CondElseIf : forall ts ts1 c ts2 t ts3 ts4 e ts5, Tk "IF" ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 t ts3 -> Tk "ELSE" ts3 ts4 -> Cond ts4 e ts5 -> Cond ts (AEIf c t (Some e)) ts5
| CondElse : forall ts ts1 c ts2 t ts3 ts4 b ts5, Tk "IF" ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 t ts3 -> Tk "ELSE" ts3 ts4 -> Block ts4 b ts5 -> Cond ts (AEIf c t (Some (AEBlock b))) ts5

(* a ::= π => e, | π => b ,?   — and the last arm's `,` is optional in
   both forms; an arm body starting with `{` is the block form (b). *)
with Arms : list token -> list (apat * aexpr) -> list token -> Prop :=
| ArmsEnd : forall ts, kind ts = "RBRACE" -> Arms ts [] ts
| ArmsCons : forall ts p ts1 ts2 body ts3 arms ts4, Pattern ts p ts1 -> Tk "EQUAL_GREATER" ts1 ts2 ->
    ArmBody ts2 body ts3 -> Arms ts3 arms ts4 -> Arms ts ((p, body) :: arms) ts4
with ArmBody : list token -> aexpr -> list token -> Prop :=
| ArmBlock : forall ts b ts1 ts2, Block ts b ts1 -> OptTk "COMMA" ts1 ts2 -> ArmBody ts (AEBlock b) ts2
| ArmExpr : forall ts e ts1 ts2, kind ts <> "LBRACE" -> Expr false ts e ts1 -> Tk "COMMA" ts1 ts2 ->
    ArmBody ts e ts2
| ArmExprLast : forall ts e ts1, kind ts <> "LBRACE" -> Expr false ts e ts1 -> kind ts1 = "RBRACE" ->
    ArmBody ts e ts1

(* e ⊕ e, D68's partial order.  An operator expression is one of
     Bitwise    = Unary (op Unary)+, one op of `&` `^` `|` throughout,
     Comparison = Operand cmp Operand,
     Operand,
   and is never followed by another binary operator: two operators the
   order does not relate have no derivation.  *)
with Operators : bool -> list token -> aexpr -> list token -> Prop :=
| OpsBitwise : forall nr ts u ts1 op e ts2, Unary nr ts u ts1 -> binop ts1 = Some op -> In op BITWISE ->
    LeftChain (Unary nr) [op] u ts1 e ts2 -> binop ts2 = None -> Operators nr ts e ts2
| OpsCompare : forall nr ts l ts1 op r ts2, Operand nr ts l ts1 -> binop ts1 = Some op ->
    In op COMPARISON -> Operand nr (advance ts1) r ts2 -> binop ts2 = None ->
    Operators nr ts (AEBinary op l r) ts2
| OpsOperand : forall nr ts e ts1, Operand nr ts e ts1 -> binop ts1 = None -> Operators nr ts e ts1

(* Operand = Unary (`<<` | `>>`) Unary | Additive  (a shift's operands are
   unary: `1 << n + 1` is refused) *)
with Operand : bool -> list token -> aexpr -> list token -> Prop :=
| OperandShift : forall nr ts l ts1 op r ts2, Unary nr ts l ts1 -> binop ts1 = Some op -> In op SHIFT ->
    Unary nr (advance ts1) r ts2 -> Operand nr ts (AEBinary op l r) ts2
| OperandAdditive : forall nr ts e ts1, Additive nr ts e ts1 -> binop_in SHIFT ts1 = false ->
    Operand nr ts e ts1

(* Additive = Term ((`+` | `-`) Term)*, left-associated *)
with Additive : bool -> list token -> aexpr -> list token -> Prop :=
| AdditiveG : forall nr ts t ts1 e ts2, Term nr ts t ts1 -> LeftChain (Term nr) ADDITIVE t ts1 e ts2 ->
    Additive nr ts e ts2

(* Term = Unary (`*` Unary)* ((`/` | `%`) Unary)?: a quotient or remainder
   ends a term (`a / b * c`, `a / b / c` are refused) *)
with Term : bool -> list token -> aexpr -> list token -> Prop :=
| TermProduct : forall nr ts u ts1 e ts2, Unary nr ts u ts1 -> LeftChain (Unary nr) ["*"] u ts1 e ts2 ->
    binop_in FRACTION ts2 = false -> Term nr ts e ts2
| TermQuotient : forall nr ts u ts1 e ts2 op r ts3, Unary nr ts u ts1 ->
    LeftChain (Unary nr) ["*"] u ts1 e ts2 -> binop ts2 = Some op -> In op FRACTION ->
    Unary nr (advance ts2) r ts3 -> binop_in ("*" :: FRACTION) ts3 = false ->
    Term nr ts (AEBinary op e r) ts3

(* ⊖e: `-` and `!` *)
with Unary : bool -> list token -> aexpr -> list token -> Prop :=
| UnaryNeg : forall nr ts ts1 u ts2, Tk "HYPHEN" ts ts1 -> ~ neg_literal_dot ts1 -> Unary nr ts1 u ts2 ->
    Unary nr ts (AEUnary "-" u) ts2
| UnaryNot : forall nr ts ts1 u ts2, Tk "EXCLAM" ts ts1 -> Unary nr ts1 u ts2 ->
    Unary nr ts (AEUnary "!" u) ts2
| UnaryPostfix : forall nr ts p ts1 e ts2, Primary nr ts p ts1 -> Suffixes p ts1 e ts2 -> Unary nr ts e ts2

(* e.p(e, ...) | e.x: suffixes, as many as follow *)
with Suffixes : aexpr -> list token -> aexpr -> list token -> Prop :=
| SufEnd : forall e ts, kind ts <> "DOT" -> Suffixes e ts e ts
| SufMethod : forall e ts ts1 p ts2 ts3 args ts4 ts5 e' ts6, Tk "DOT" ts ts1 -> Path ts1 p ts2 ->
    Tk "LPAREN" ts2 ts3 -> CommaList (Expr false) ["RPAREN"] ts3 args ts4 -> Tk "RPAREN" ts4 ts5 ->
    Suffixes (AEMeth e p args) ts5 e' ts6 -> Suffixes e ts e' ts6
| SufField : forall e ts ts1 x ts2 e' ts3, Tk "DOT" ts ts1 -> Path ts1 [x] ts2 -> kind ts2 <> "LPAREN" ->
    Suffixes (AEField e x) ts2 e' ts3 -> Suffixes e ts e' ts3

(* (e) | () | this | b | int | dec | r | r(e, ...) | r{x = e, ...}, and
   the lambda (x: τ, ...): τ => e (D70), which the figure lists under β *)
with Primary : bool -> list token -> aexpr -> list token -> Prop :=
| PrimLambda : forall nr ts e ts1, Lambda ts e ts1 -> Primary nr ts e ts1
| PrimUnit : forall nr ts ts1 ts2, Tk "LPAREN" ts ts1 -> at_lambda ts = false -> Tk "RPAREN" ts1 ts2 ->
    Primary nr ts AEUnit ts2
| PrimParen : forall nr ts ts1 e ts2 ts3, Tk "LPAREN" ts ts1 -> at_lambda ts = false ->
    Expr false ts1 e ts2 -> Tk "RPAREN" ts2 ts3 -> Primary nr ts e ts3
| PrimThis : forall nr ts ts1, Tk "THIS" ts ts1 -> Primary nr ts AEThis ts1
| PrimLit : forall nr ts, In (kind ts) ["INTEGER"; "DECIMAL"] ->
    Primary nr ts (AELit (lit_norm (tx (peek ts)))) (advance ts)
| PrimString : forall nr ts, kind ts = "STRING" ->
    string_utf8_valid (string_value (tx (peek ts))) = true ->
    Primary nr ts (string_desugar (string_value (tx (peek ts)))) (advance ts)
| PrimBlock : forall ts b ts1, Block ts b ts1 -> Primary false ts (AEBlock b) ts1
| PrimRef : forall nr ts p ts1 a ts2, Path ts p ts1 -> App ts1 a ts2 -> kind ts2 <> "LPAREN" ->
    (nr = true \/ kind ts2 <> "LBRACE") -> Primary nr ts (AEPath p a) ts2
| PrimCall : forall nr ts p ts1 a ts2 ts3 args ts4 ts5, Path ts p ts1 -> App ts1 a ts2 ->
    Tk "LPAREN" ts2 ts3 -> CommaList (Expr false) ["RPAREN"] ts3 args ts4 -> Tk "RPAREN" ts4 ts5 ->
    Primary nr ts (AECall p a args) ts5
| PrimRecord : forall ts p ts1 a ts2 ts3 fs ts4 ts5, Path ts p ts1 -> App ts1 a ts2 ->
    Tk "LBRACE" ts2 ts3 -> CommaList RecField ["RBRACE"] ts3 fs ts4 -> Tk "RBRACE" ts4 ts5 ->
    Primary false ts (AERec p a fs) ts5
with RecField : list token -> name * option aexpr -> list token -> Prop :=
| RecFieldShort : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> "EQUAL" -> RecField ts (x, None) ts1
| RecFieldLong : forall ts x ts1 ts2 e ts3, TkName ts x ts1 -> Tk "EQUAL" ts1 ts2 -> Expr false ts2 e ts3 ->
    RecField ts (x, Some e) ts3
with Lambda : list token -> aexpr -> list token -> Prop :=
| LambdaG : forall ts ts1 ps ts2 ts3 ret ts4 ts5 body ts6, Tk "LPAREN" ts ts1 ->
    CommaList LamParam ["RPAREN"] ts1 ps ts2 -> Tk "RPAREN" ts2 ts3 -> OptType ts3 ret ts4 ->
    Tk "EQUAL_GREATER" ts4 ts5 -> Expr false ts5 body ts6 -> Lambda ts (AELam ps ret body) ts6
with LamParam : list token -> name * option aty -> list token -> Prop :=
| LamParamG : forall ts x ts1 t ts2, TkName ts x ts1 -> OptType ts1 t ts2 -> LamParam ts (x, t) ts2

(* b ::= { i ... e? } *)
with Block : list token -> ablock -> list token -> Prop :=
| BlockG : forall ts ts1 ss tl ts2 ts3, Tk "LBRACE" ts ts1 -> Stmts ts1 ss tl ts2 -> Tk "RBRACE" ts2 ts3 ->
    Block ts (ABlock ss tl) ts3

(* i ...  e?: the statements of a block, and its trailing expression.
   i ::= let x (: τ)? = e; | var x (: τ)? = e; | x = e; | w
       | while e b | loop b | for x in e b | e; | e
   An expression statement without `;` must START with `if`, `match` or
   `{` and not be followed by `;` or `}` (then it is the trailing
   expression).  The expression rules are tried only where no keyword
   statement and no `x = ...` is next (b). *)
with Stmts : list token -> list astmt -> option aexpr -> list token -> Prop :=
| StEnd : forall ts, kind ts = "RBRACE" -> Stmts ts [] None ts
| StLet : forall ts ts1 x ts2 t ts3 ts4 e ts5 ts6 ss tl ts7, Tk "LET" ts ts1 -> TkName ts1 x ts2 ->
    OptType ts2 t ts3 -> Tk "EQUAL" ts3 ts4 -> Expr false ts4 e ts5 -> Tk "SEMI" ts5 ts6 ->
    Stmts ts6 ss tl ts7 -> Stmts ts (ASLet x t e :: ss) tl ts7
| StVar : forall ts ts1 x ts2 t ts3 ts4 e ts5 ts6 ss tl ts7, Tk "VAR" ts ts1 -> TkName ts1 x ts2 ->
    OptType ts2 t ts3 -> Tk "EQUAL" ts3 ts4 -> Expr false ts4 e ts5 -> Tk "SEMI" ts5 ts6 ->
    Stmts ts6 ss tl ts7 -> Stmts ts (ASVar x t e :: ss) tl ts7
| StBind : forall ts w ts1 ss tl ts2, Bind ts w ts1 -> Stmts ts1 ss tl ts2 -> Stmts ts (ASBind w :: ss) tl ts2
| StWhile : forall ts ts1 c ts2 b ts3 ss tl ts4, Tk "WHILE" ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 b ts3 -> Stmts ts3 ss tl ts4 -> Stmts ts (ASWhile c b :: ss) tl ts4
| StLoop : forall ts ts1 b ts2 ss tl ts3, Tk "LOOP" ts ts1 -> Block ts1 b ts2 -> Stmts ts2 ss tl ts3 ->
    Stmts ts (ASLoop b :: ss) tl ts3
| StFor : forall ts ts1 x ts2 ts3 coll ts4 body ts5 ss tl ts6, Tk "FOR" ts ts1 -> TkName ts1 x ts2 ->
    Tk "IN" ts2 ts3 -> Expr true ts3 coll ts4 -> Block ts4 body ts5 -> Stmts ts5 ss tl ts6 ->
    Stmts ts (for_desugar x coll body :: ss) tl ts6
| StAssign : forall ts x ts1 ts2 e ts3 ts4 ss tl ts5, TkName ts x ts1 -> Tk "EQUAL" ts1 ts2 ->
    Expr false ts2 e ts3 -> Tk "SEMI" ts3 ts4 -> Stmts ts4 ss tl ts5 -> Stmts ts (ASAssign x e :: ss) tl ts5
| StExpr : forall ts e ts1 ts2 ss tl ts3, ~ assign_ahead ts -> Expr false ts e ts1 -> Tk "SEMI" ts1 ts2 ->
    Stmts ts2 ss tl ts3 -> Stmts ts (ASExpr e :: ss) tl ts3
| StBraced : forall ts e ts1 ss tl ts2, ~ assign_ahead ts -> In (kind ts) ["IF"; "MATCH"; "LBRACE"] ->
    Expr false ts e ts1 -> ~ In (kind ts1) ["SEMI"; "RBRACE"] -> Stmts ts1 ss tl ts2 ->
    Stmts ts (ASExpr e :: ss) tl ts2
| StTail : forall ts e ts1, ~ assign_ahead ts -> Expr false ts e ts1 -> kind ts1 = "RBRACE" ->
    Stmts ts [] (Some e) ts1

(* w ::= bind β, ...;    β ::= s | s = e *)
with Bind : list token -> list (spec * option aexpr) -> list token -> Prop :=
| BindG : forall ts ts1 bs ts2 ts3, Tk "BIND" ts ts1 -> CommaList Binding ["SEMI"] ts1 bs ts2 ->
    Tk "SEMI" ts2 ts3 -> Bind ts bs ts3
with Binding : list token -> spec * option aexpr -> list token -> Prop :=
| BindingApply : forall ts s ts1, Spec_ ts s ts1 -> kind ts1 <> "EQUAL" -> Binding ts (s, None) ts1
| BindingBind : forall ts s ts1 ts2 e ts3, Spec_ ts s ts1 -> Tk "EQUAL" ts1 ts2 -> Expr false ts2 e ts3 ->
    Binding ts (s, Some e) ts3.

(* ===================== Declarations d ===================================== *)

(* [x, ...], as a tag's or functor's brackets *)
Definition NameList (ts : list token) (xs : list name) (rest : list token) : Prop :=
  exists ts1 ts2, Tk "LBRACKET" ts ts1 /\ CommaList TkName ["RBRACKET"] ts1 xs ts2 /\ Tk "RBRACKET" ts2 rest.

(* f ::= x | x.x | .x *)
Inductive Function_ : list token -> fnrecv * name -> list token -> Prop :=
| FnPlain : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> "DOT" -> Function_ ts (RPlain, x) ts1
| FnAttached : forall ts r ts1 ts2 x ts3, TkName ts r ts1 -> Tk "DOT" ts1 ts2 -> TkName ts2 x ts3 ->
    Function_ ts (RAttached r, x) ts3
| FnDetached : forall ts ts1 x ts2, Tk "DOT" ts ts1 -> TkName ts1 x ts2 -> Function_ ts (RDetached, x) ts2.

Inductive Param : list token -> name * aty -> list token -> Prop :=
| ParamG : forall ts x ts1 ts2 t ts3, TkName ts x ts1 -> Tk "COLON" ts1 ts2 -> Type_ ts2 t ts3 ->
    Param ts (x, t) ts3.

(* an assume's item: h, but not `.x` *)
Definition AssumeItem (ts : list token) (it : path * option name) (rest : list token) : Prop :=
  Head ts (Some (fst it), snd it) rest.

(* `[x, ...]` after `tag x`, possibly absent or empty *)
Definition TagParams (ts : list token) (xs : list name) (rest : list token) : Prop :=
  (kind ts <> "LBRACKET" /\ xs = [] /\ rest = ts) \/ NameList ts xs rest.

(* d ::= assume h, ... { d ... } | type x; | type x = τ; | val x: τ (= e)?;
       | tag x; | tag x([x, ...])? (τ); | tag x([x, ...])? {x: τ, ...};
       | fn f(x: τ, ...) (: τ)? ; | fn f(x: τ, ...) (: τ)? b
       | context x = s, ...; | functor x([x, ...])?: s, ... -> s, ...;
       | functor x: s, ... -> s, ... { w ... } *)
Inductive Decl : list token -> adecl -> list token -> Prop :=
| DAssume : forall ts ts1 its ts2 ts3 ds ts4 ts5, Tk "ASSUME" ts ts1 ->
    CommaList AssumeItem ["LBRACE"] ts1 its ts2 -> Tk "LBRACE" ts2 ts3 -> Decls ts3 ds ts4 ->
    Tk "RBRACE" ts4 ts5 -> Decl ts (ADAssume its ds) ts5
| DType : forall ts ts1 x ts2 ts3, Tk "TYPE" ts ts1 -> TkName ts1 x ts2 -> Tk "SEMI" ts2 ts3 ->
    Decl ts (ADType x) ts3
| DAlias : forall ts ts1 x ts2 ts3 t ts4 ts5, Tk "TYPE" ts ts1 -> TkName ts1 x ts2 -> Tk "EQUAL" ts2 ts3 ->
    Type_ ts3 t ts4 -> Tk "SEMI" ts4 ts5 -> Decl ts (ADAlias x t) ts5
(* `tag x;`, without brackets: `tag x[];` is refused (D73) *)
| DUnit : forall ts ts1 x ts2 ts3, Tk "TAG" ts ts1 -> TkName ts1 x ts2 -> kind ts2 <> "LBRACKET" ->
    Tk "SEMI" ts2 ts3 -> Decl ts (ADUnit x) ts3
| DTagRecord : forall ts ts1 x ts2 ps ts3 t ts4 ts5, Tk "TAG" ts ts1 -> TkName ts1 x ts2 ->
    TagParams ts2 ps ts3 -> RecordType ts3 t ts4 -> Tk "SEMI" ts4 ts5 -> Decl ts (ADTag x ps t) ts5
(* a record payload is written with braces, not `({...})` (D83) *)
| DTag : forall ts ts1 x ts2 ps ts3 ts4 t ts5 ts6 ts7, Tk "TAG" ts ts1 -> TkName ts1 x ts2 ->
    TagParams ts2 ps ts3 -> Tk "LPAREN" ts3 ts4 -> Type_ ts4 t ts5 -> (forall fs, t <> ATRec fs) ->
    Tk "RPAREN" ts5 ts6 -> Tk "SEMI" ts6 ts7 -> Decl ts (ADTag x ps t) ts7
| DVal : forall ts ts1 x ts2 ts3 t ts4 ts5, Tk "VAL" ts ts1 -> TkName ts1 x ts2 -> Tk "COLON" ts2 ts3 ->
    Type_ ts3 t ts4 -> Tk "SEMI" ts4 ts5 -> Decl ts (ADVal x t None) ts5
| DValDefined : forall ts ts1 x ts2 ts3 t ts4 ts5 e ts6 ts7, Tk "VAL" ts ts1 -> TkName ts1 x ts2 ->
    Tk "COLON" ts2 ts3 -> Type_ ts3 t ts4 -> Tk "EQUAL" ts4 ts5 -> Expr false ts5 e ts6 ->
    Tk "SEMI" ts6 ts7 -> Decl ts (ADVal x t (Some e)) ts7
| DFnAbstract : forall ts ts1 rx ts2 ts3 ps ts4 ts5 ret ts6 ts7, Tk "FN" ts ts1 -> Function_ ts1 rx ts2 ->
    Tk "LPAREN" ts2 ts3 -> CommaList Param ["RPAREN"] ts3 ps ts4 -> Tk "RPAREN" ts4 ts5 ->
    OptType ts5 ret ts6 -> Tk "SEMI" ts6 ts7 -> Decl ts (ADFn (fst rx) (snd rx) ps ret None) ts7
| DFn : forall ts ts1 rx ts2 ts3 ps ts4 ts5 ret ts6 b ts7, Tk "FN" ts ts1 -> Function_ ts1 rx ts2 ->
    Tk "LPAREN" ts2 ts3 -> CommaList Param ["RPAREN"] ts3 ps ts4 -> Tk "RPAREN" ts4 ts5 ->
    OptType ts5 ret ts6 -> Block ts6 b ts7 -> Decl ts (ADFn (fst rx) (snd rx) ps ret (Some b)) ts7
| DContext : forall ts ts1 x ts2 ts3 ss ts4 ts5, Tk "CONTEXT" ts ts1 -> TkName ts1 x ts2 ->
    Tk "EQUAL" ts2 ts3 -> CommaList Spec_ ["SEMI"] ts3 ss ts4 -> Tk "SEMI" ts4 ts5 -> Decl ts (ADCtx x ss) ts5
(* an abstract functor, its brackets optional (D78, D84) *)
| DFunctorAbstract : forall ts ts1 x ts2 ps ts3 ts4 args ts5 ts6 res ts7 ts8, Tk "FUNCTOR" ts ts1 ->
    TkName ts1 x ts2 ->
    ((kind ts2 <> "LBRACKET" /\ ps = None /\ ts3 = ts2) \/ exists l, ps = Some l /\ NameList ts2 l ts3) ->
    Tk "COLON" ts3 ts4 -> CommaList Spec_ ["HYPHEN_GREATER"] ts4 args ts5 -> Tk "HYPHEN_GREATER" ts5 ts6 ->
    CommaList Spec_ ["LBRACE"; "SEMI"] ts6 res ts7 -> Tk "SEMI" ts7 ts8 ->
    Decl ts (ADFunctor x ps args res None) ts8
(* a functor with a body has no brackets (D84) *)
| DFunctor : forall ts ts1 x ts2 ts3 args ts4 ts5 res ts6 ts7 ws ts8 ts9, Tk "FUNCTOR" ts ts1 ->
    TkName ts1 x ts2 -> kind ts2 <> "LBRACKET" -> Tk "COLON" ts2 ts3 ->
    CommaList Spec_ ["HYPHEN_GREATER"] ts3 args ts4 -> Tk "HYPHEN_GREATER" ts4 ts5 ->
    CommaList Spec_ ["LBRACE"; "SEMI"] ts5 res ts6 -> Tk "LBRACE" ts6 ts7 -> Binds ts7 ws ts8 ->
    Tk "RBRACE" ts8 ts9 -> Decl ts (ADFunctor x None args res (Some ws)) ts9
with Decls : list token -> list adecl -> list token -> Prop :=
| DeclsEnd : forall ts, kind ts = "RBRACE" -> Decls ts [] ts
| DeclsCons : forall ts d ts1 ds ts2, Decl ts d ts1 -> Decls ts1 ds ts2 -> Decls ts (d :: ds) ts2
with Binds : list token -> list (list (spec * option aexpr)) -> list token -> Prop :=
| BindsEnd : forall ts, kind ts = "RBRACE" -> Binds ts [] ts
| BindsCons : forall ts w ts1 ws ts2, Bind ts w ts1 -> Binds ts1 ws ts2 -> Binds ts (w :: ws) ts2.

(* ===================== Imports m, Uses u, the File F ====================== *)

(* u ::= x | x as x | .x | .x as .x   (a rename keeps the dottedness) *)
Definition UseName (ts : list token) (u : bool * name) (rest : list token) : Prop :=
  exists ts1, ((fst u = true /\ Tk "DOT" ts ts1) \/ (fst u = false /\ kind ts <> "DOT" /\ ts1 = ts)) /\
              TkName ts1 (snd u) rest.
Inductive Use : list token -> ause -> list token -> Prop :=
| UsePlain : forall ts d x ts1, UseName ts (d, x) ts1 -> kind ts1 <> "AS" -> Use ts (AUse d x None) ts1
| UseRenamed : forall ts d x ts1 ts2 y ts3, UseName ts (d, x) ts1 -> Tk "AS" ts1 ts2 ->
    UseName ts2 (d, y) ts3 -> Use ts (AUse d x (Some (d, y))) ts3.

(* m ::= import str (as x)? (use * | use u, ...)?; *)
Inductive Import_ : list token -> aimport -> list token -> Prop :=
| ImportG : forall ts ts1 s ts2 alias ts3 glob uses ts4 ts5, Tk "IMPORT" ts ts1 ->
    TkText "STRING" ts1 s ts2 ->
    ((kind ts2 <> "AS" /\ alias = None /\ ts3 = ts2) \/
     exists ts' a, alias = Some a /\ Tk "AS" ts2 ts' /\ TkName ts' a ts3) ->
    ((kind ts3 <> "USE" /\ glob = false /\ uses = [] /\ ts4 = ts3) \/
     (exists ts', Tk "USE" ts3 ts' /\ Tk "STAR" ts' ts4 /\ glob = true /\ uses = []) \/
     (exists ts', Tk "USE" ts3 ts' /\ kind ts' <> "STAR" /\ glob = false /\
                  CommaList Use ["SEMI"] ts' uses ts4)) ->
    Tk "SEMI" ts4 ts5 -> Import_ ts (AImport (string_value s) alias glob uses) ts5.

Inductive Imports : list token -> list aimport -> list token -> Prop :=
| ImportsEnd : forall ts, kind ts <> "IMPORT" -> Imports ts [] ts
| ImportsCons : forall ts m ts1 ms ts2, Import_ ts m ts1 -> Imports ts1 ms ts2 -> Imports ts (m :: ms) ts2.

Inductive DeclsToEnd : list token -> list adecl -> list token -> Prop :=
| EndOfFile : forall ts, kind ts = "EOF" -> DeclsToEnd ts [] ts
| DeclThen : forall ts d ts1 ds ts2, Decl ts d ts1 -> DeclsToEnd ts1 ds ts2 -> DeclsToEnd ts (d :: ds) ts2.

(* F ::= m ... d ... (then the end of the file) *)
Definition File (ts : list token) (f : afile) : Prop :=
  exists ts1 ts2, Imports ts (f_imports f) ts1 /\ DeclsToEnd ts1 (f_decls f) ts2.

(* The specification: the bytes of a source file have the AST f. *)
Definition Derives (bytes : list byte) (f : afile) : Prop :=
  exists ts, Lexes 0 (source_bytes bytes) ts /\ File ts f.

End Syntax.

(* ######################################################################## *)
(* Part I.3  The dump and the loader                                        *)
(* ######################################################################## *)

(* The dump: an AST as the s-expression the readers of Part I.1 read
   (sexpr.py's format).  Moved verbatim from Parse.v: the loader's
   specification below is stated with it. *)
Module Dump.
Import Input.
Local Open Scope string_scope.

Definition nd (tag : string) (xs : list sx) : sx := SL false (SA tag :: xs).
Definition sl (xs : list sx) : sx := SL true xs.
Definition dopt {X} (f : X -> sx) (o : option X) : sx :=
  match o with None => SA "_" | Some x => f x end.
Definition dpath (p : path) : sx := SA (join_path p).

Fixpoint dspec (s : spec) : sx :=
  match s with
  | Spec p d a =>
      nd "I" [dopt dpath p; dopt SA d;
              match a with
              | None => SA "_"
              | Some bs => sl (map (fun '(bp, bs1) => nd "J" [dpath bp; dspec bs1]) bs)
              end]
  end.

Definition dapp (a : sapp) : sx :=
  match a with
  | None => SA "_"
  | Some bs => sl (map (fun '(bp, s) => nd "J" [dpath bp; dspec s]) bs)
  end.

Fixpoint dty (t : aty) : sx :=
  match t with
  | ATNever => nd "b" []
  | ATThis => nd "c" []
  | ATUnit => nd "a" []
  | ATRec fs => nd "f" [sl (map (fun '(x, t1) => nd "h" [SA x; dty t1]) fs)]
  | ATUnion ms => nd "g" [sl (map dty ms)]
  | ATRef p a => nd "d" [dpath p; dapp a]
  end.

Fixpoint dpat (p : apat) : sx :=
  match p with
  | APWild => nd "D" []
  | APPath q => nd "E" [dpath q; SA "_"]
  | APTag q s => nd "E" [dpath q; dpat s]
  | APRec q fs => nd "F" [dopt dpath q; sl (map (fun '(x, s) => nd "G" [SA x; dopt dpat s]) fs)]
  end.

Fixpoint dexpr (e : aexpr) : sx :=
  match e with
  | AEUnit => nd "i" []
  | AEThis => nd "j" []
  | AEPath p a => nd "k" [dpath p; dapp a]
  | AECall p a args => nd "l" [nd "k" [dpath p; dapp a]; sl (map dexpr args)]
  | AEField o f => nd "m" [dexpr o; SA f]
  | AEMeth o p args => nd "n" [dexpr o; dpath p; sl (map dexpr args)]
  | AERec p a fs => nd "o" [nd "k" [dpath p; dapp a];
                            sl (map (fun '(x, v) => nd "p" [SA x; dopt dexpr v]) fs)]
  | AEIf c t el => nd "q" [dexpr c; dblock t; dopt dexpr el]
  | AEMatch s arms => nd "r" [dexpr s; sl (map (fun '(p, b) => nd "H" [dpat p; dexpr b]) arms)]
  | AEReturn o => nd "s" [dopt dexpr o]
  | AEBreak => nd "t" []
  | AEBlock b => dblock b
  | AEUnary op x => nd "W" [SA op; dexpr x]
  | AEBinary op l r => nd "X" [SA op; dexpr l; dexpr r]
  | AELit t => nd "Y" [SA t]
  | AELam ps ret body => nd "Z" [sl (map (fun '(x, t) => nd "K" [SA x; dopt dty t]) ps);
                                 dopt dty ret; dexpr body]
  end
with dblock (b : ablock) : sx :=
  match b with
  | ABlock ss t => nd "u" [sl (map dstmt ss); dopt dexpr t]
  end
with dstmt (s : astmt) : sx :=
  match s with
  | ASLet x t e => nd "v" [SA x; dopt dty t; dexpr e]
  | ASVar x t e => nd "w" [SA x; dopt dty t; dexpr e]
  | ASAssign x e => nd "x" [SA x; dexpr e]
  | ASBind its => nd "y" [sl (map (fun '(s, v) => nd "z" [dspec s; dopt dexpr v]) its)]
  | ASWhile c b => nd "A" [dexpr c; dblock b]
  | ASLoop b => nd "B" [dblock b]
  | ASExpr e => nd "C" [dexpr e]
  end.

Definition dbind (its : list (spec * option aexpr)) : sx :=
  nd "y" [sl (map (fun '(s, v) => nd "z" [dspec s; dopt dexpr v]) its)].

Fixpoint ddecl (d : adecl) : sx :=
  match d with
  | ADAssume its ds => nd "M" [sl (map (fun '(p, o) => nd "I" [dpath p; dopt SA o; SA "_"]) its);
                               sl (map ddecl ds)]
  | ADType x => nd "N" [SA x]
  | ADAlias x t => nd "O" [SA x; dty t]
  | ADTag x ps t => nd "P" [SA x; sl (map SA ps); dty t]
  | ADUnit x => nd "Q" [SA x]
  | ADVal x t i => nd "R" [SA x; dty t; dopt dexpr i]
  | ADFn r x ps ret body =>
      nd "S" [SA (match r with RDetached => "." | RPlain => "_" | RAttached q => q end); SA x;
              sl (map (fun '(p, t) => nd "K" [SA p; dty t]) ps); dopt dty ret; dopt dblock body]
  | ADCtx x its => nd "T" [SA x; sl (map dspec its)]
  | ADFunctor x ps args res body =>
      nd "U" [SA x; dopt (fun l => sl (map SA l)) ps; sl (map dspec args); sl (map dspec res);
              dopt (fun bs => sl (map dbind bs)) body]
  end.

Definition duse_side (d : bool) (x : name) : string := (if d then "." else "") ++ x.
Definition duse (u : ause) : sx :=
  SA (duse_side (u_dot u) (u_name u) ++
      match u_alias u with None => "" | Some (d, y) => ">" ++ duse_side d y end).

Definition dimport (i : aimport) : sx :=
  nd "L" [SA (i_path i); dopt SA (i_alias i); SA (if i_glob i then "*" else "_");
          sl (map duse (i_uses i))].

Definition dump_file (f : afile) : sx :=
  nd "$" [sl (map dimport (f_imports f)); sl (map ddecl (f_decls f))].

End Dump.

(* The loader (py/bundle.py; collect.Loader's normalize, resolve and
   wants_prelude, moved verbatim from Parse.v), and `Bundled`, what a
   bundle loaded from a file system `read` must be. *)
Module Loader.
Import Stdlib.Strings.Byte.
Import Input.
Local Open Scope string_scope.

Fixpoint split_bytes (sep : byte) (cs : list byte) (cur : list byte) : list (list byte) :=
  match cs with
  | [] => [rev' cur]
  | c :: r => if Byte.eqb c sep then rev' cur :: split_bytes sep r [] else split_bytes sep r (c :: cur)
  end.

(* PurePosixPath's parts after the root: empty and `.` segments dropped *)
Definition segs (s : string) : list string :=
  map string_of_list_byte
    (filter (fun seg => negb (match seg with [] => true | ["."%byte] => true | _ => false end))
            (split_bytes "/"%byte (list_byte_of_string s) [])).

Definition is_abs (s : string) : bool :=
  match list_byte_of_string s with "/"%byte :: _ => true | _ => false end.

(* Loader.normalize(base, path), with `root` the working directory *)
Definition normalize (root : string) (base : option string) (p : string) : string :=
  let '(jabs, jsegs) :=
    if is_abs p then (true, segs p)
    else match base with
         | None => (false, segs p)
         | Some b => (is_abs b, app (removelast (segs b)) (segs p))
         end in
  let parts := if jabs then "/" :: jsegs else "/" :: app (segs root) jsegs in
  let acc := fold_left (fun acc part =>
                if String.eqb part ".." then
                  match acc with h :: t => if String.eqb h ".." then part :: acc else t | [] => [part] end
                else part :: acc) parts [] in
  match rev' acc with
  | [] => "."
  | "/" :: rest => "/" ++ String.concat "/" rest
  | ps => String.concat "/" ps
  end.

Definition first_segment (p : string) : string :=
  match split_bytes "/"%byte (list_byte_of_string p) [] with
  | s :: _ => string_of_list_byte s
  | [] => ""
  end.

(* Loader.resolve (D64): `.`/`..`-relative and absolute paths are the
   importer's; any other names a file of the library, the prelude's
   directory *)
Definition resolve (root prelude importer p : string) : string :=
  let first := first_segment p in
  if is_abs p || String.eqb first "." || String.eqb first ".." then normalize root (Some importer) p
  else normalize root (Some prelude) p.

(* The directory prefix, including its final slash, preserves relative
   paths as well as absolute ones. Disabling the auto-prelude retains
   the default library directory, `lib/` relative to the working directory. *)
Fixpoint directory_cut (p : string) (pos cut : nat) : nat :=
  match p with
  | EmptyString => cut
  | String c rest => directory_cut rest (S pos)
      (if Ascii.eqb c "/"%char then S pos else cut)
  end.
Definition directory (p : string) : string := substring 0 (directory_cut p 0 0) p.
Definition library_dir (prelude : string) : option string :=
  if String.eqb prelude "" then Some "lib/" else Some (directory prelude).

Definition parent_str (p : string) : string :=
  "/" ++ String.concat "/" (removelast (segs p)).

Definition wants_prelude (prelude p : string) : bool :=
  negb (String.eqb p prelude) && negb (String.prefix (parent_str prelude ++ "/") p).


(* `Bundled read root prelude entry b`: the bundle b is what loading
   `entry` with `prelude` means.  Its entry and prelude are the given
   paths, normalized against the working directory `root`; it has each
   module once, the entry among them; and each module is a file that
   `read` gives, whose bytes derive an AST f (Syntax.Derives), and the
   module holds exactly f (the dump reads back without changing its AST), whether it wants the prelude,
   and its imports resolved (D64); and every module it imports, and the
   prelude if it wants it, is in the bundle too. *)
Definition Bundled (read : string -> option string) (root prelude_in entry_in : string) (b : bundle) : Prop :=
  b_entry b = normalize root None entry_in /\ b_prelude b = normalize root None prelude_in /\
  NoDup (map am_path (b_mods b)) /\ In (b_entry b) (map am_path (b_mods b)) /\
  forall m, In m (b_mods b) ->
    exists text f, read (am_path m) = Some text /\ Syntax.Derives (list_byte_of_string text) f /\
      read_file (Dump.dump_file f) = Good (am_file m) /\ am_file m = f /\
      am_prelude m = wants_prelude (b_prelude b) (am_path m) /\
      am_targets m = map (fun i => resolve root (b_prelude b) (am_path m) (i_path i)) (f_imports f) /\
      (forall t, In t (am_targets m) -> In t (map am_path (b_mods b))) /\
      (am_prelude m = true -> In (b_prelude b) (map am_path (b_mods b))).
End Loader.

(* ######################################################################## *)
(* Part II. Elaborated programs                                              *)
(* ######################################################################## *)

Set Implicit Arguments.

(* ---- II.1 Core Moss (moved from CoreMoss.v; section numbers are its) ---- *)

(* ===================== 1. Symbols ======================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)
Definition usym := nat.  (* functor symbols [D55]/[D78] *)

(* The distinguished receiver type of method signatures. *)
Definition this_sym : tsym := 0.

(* ===================== 2. Types and substitutions ======================== *)

Inductive ty : Type :=
| TUnit  : ty
| TAbs   : tsym -> ty                       (* abstract type symbol (incl. This) *)
| TNom   : nsym -> list (tsym * ty) -> ty   (* total applied nominal reference *)
| TUnion : list ty -> ty.                   (* members must be nominal refs *)

Definition subst := list (tsym * ty).

Fixpoint lookup_subst (t : tsym) (s : subst) : option ty :=
  match s with
  | [] => None
  | (t', tau) :: r => if Nat.eqb t t' then Some tau else lookup_subst t r
  end.

(* Applying a substitution.  Substitutions accumulated by the statics are
   idempotent by construction (bind t = τ stores στ), so one pass suffices. *)
Fixpoint app_subst (s : subst) (tau : ty) {struct tau} : ty :=
  match tau with
  | TUnit => TUnit
  | TAbs t => match lookup_subst t s with Some tau' => tau' | None => TAbs t end
  | TNom n th =>
      TNom n ((fix go (l : list (tsym * ty)) : list (tsym * ty) :=
                 match l with
                 | [] => []
                 | p :: r => (fst p, app_subst s (snd p)) :: go r
                 end) th)
  | TUnion ms =>
      TUnion ((fix go (l : list ty) : list ty :=
                 match l with
                 | [] => []
                 | t' :: r => app_subst s t' :: go r
                 end) ms)
  end.

(* Boolean equality on types. *)
Fixpoint ty_eqb (a b : ty) {struct a} : bool :=
  match a, b with
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => Nat.eqb t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

(* ===================== 3. Items and telescopes =========================== *)

(* A method item carries the *receiver type* it is provided at: the paper's
   §1 key, the pair (receiver type, method symbol), plus the application θ
   that interprets the method's own requirements (deviation 6). *)
Inductive item : Type :=
| ITy  : tsym -> item
| IVal : vsym -> item
| IFn  : fsym -> item
| IMth : ty -> msym -> subst -> item
| IFun : usym -> item.   (* an abstract functor [D78]: at no instantiation *)

Definition tele := list item.

Fixpoint subst_eqb (a b : subst) : bool :=
  match a, b with
  | [], [] => true
  | p1 :: r1, p2 :: r2 =>
      Nat.eqb (fst p1) (fst p2) && ty_eqb (snd p1) (snd p2) && subst_eqb r1 r2
  | _, _ => false
  end.

Definition item_eqb (a b : item) : bool :=
  match a, b with
  | ITy t1, ITy t2 => Nat.eqb t1 t2
  | IVal v1, IVal v2 => Nat.eqb v1 v2
  | IFn f1, IFn f2 => Nat.eqb f1 f2
  | IMth r1 m1 th1, IMth r2 m2 th2 =>
      ty_eqb r1 r2 && Nat.eqb m1 m2 && subst_eqb th1 th2
  | IFun u1, IFun u2 => Nat.eqb u1 u2
  | _, _ => false
  end.

(* The head a union member discriminates by. *)
Definition member_head (tau : ty) : option nsym :=
  match tau with
  | TNom n _ => Some n
  | _ => None
  end.

Definition map_snd (A B C : Type) (f : B -> C) (l : list (A * B)) : list (A * C) :=
  map (fun p => (fst p, f (snd p))) l.

(* A substitution acts on an item through the types it carries. *)
Definition app_subst_item (s : subst) (i : item) : item :=
  match i with
  | ITy t => ITy t
  | IVal v => IVal v
  | IFn f => IFn f
  | IMth rt m th => IMth (app_subst s rt) m (map_snd (app_subst s) th)
  | IFun u => IFun u
  end.

Definition is_ty_item (i : item) : bool :=
  match i with ITy _ => true | _ => false end.

(* ===================== 5. Expressions and values ========================= *)

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

Inductive expr : Type :=
| EVar     : var -> expr
| EUnit    : expr
| EVl      : vsym -> expr
| ETag     : nsym -> subst -> expr -> expr
| ECallD   : fsym -> subst -> smap -> list expr -> expr   (* defined fn call *)
| ECallA   : fsym -> list expr -> expr                    (* abstract fn call *)
| EMeth    : expr -> msym -> item -> list expr -> expr    (* item: the provision *)
| EMatch   : expr -> list (nsym * var * expr) -> expr
| ELet     : var -> expr -> expr -> expr
| EBindTy  : tsym -> ty -> expr -> expr
| EBindVal : vsym -> expr -> expr -> expr
| EBindFn  : fsym -> fsym -> smap -> expr -> expr
| EBindMth : ty -> msym -> subst -> fsym -> smap -> expr -> expr
(* Abstract functors [D78].  `bind φ = g;` binds φ to a functor g with a
   body (ς: g's argument items to φ's); `bind φ = ψ;` copies the binding of
   an abstract ψ in force (ς: ψ's argument items to φ's); `bind φ;` applies
   whatever φ is bound to (ς: φ's argument items to the items in force),
   and is an existential unpack when φ's result holds types. *)
| EBindFun  : usym -> usym -> smap -> smap -> expr -> expr
| EBindFunA : usym -> usym -> smap -> expr -> expr
| EApply    : usym -> subst -> smap -> expr -> expr.
(* Rule A: EBindFun u h sm smc e also records smc, the bind site's items
   answering h's captured inputs; EApply u θ sm e applies at θ. *)
(* In EBindMth the first field is the receiver type; its head is the key. *)

(* A functor body [D55]: a run of binds, the same five forms as the
   expression-level binds plus an application, with no continuation — the
   body's provisions are what it leaves in force.  Functors are elaborated
   away where they are applied by name (deviation 5); a body survives into
   the core only as the binding of an abstract functor [D78]. *)
Inductive bnd : Type :=
| BTy    : tsym -> ty -> bnd
| BVal   : vsym -> expr -> bnd
| BFn    : fsym -> fsym -> smap -> bnd
| BMth   : ty -> msym -> subst -> fsym -> smap -> bnd
| BFun   : usym -> usym -> smap -> smap -> bnd
| BFunA  : usym -> usym -> smap -> bnd
| BApply : usym -> subst -> smap -> bnd.

Inductive value : Type :=
| VUnit : value
| VTag  : nsym -> value -> value.

(* ===================== 6. Global signatures ============================== *)

Record fnsig : Type := mkSig { fs_params : list ty; fs_ret : ty }.

(* Σ: every symbol's kind, signature, and (concatenated) enclosing telescope.
   Method signatures may mention TAbs this_sym; the receiver is understood as
   an implicit first argument. *)
Record gsig : Type := mkGsigInterfaces {
  g_ty  : tsym -> bool;
  g_tag : nsym -> option (tele * ty);
  g_val : vsym -> option (tele * ty);
  g_fn  : fsym -> option (tele * fnsig * option expr);
  g_mth : msym -> option (tele * fnsig);
  (* a functor: its argument and result telescopes, and its body — None
     for an abstract functor `functor F: Args -> Result;` [D78] *)
  g_fun : usym -> option (tele * tele * option (list bnd));
  (* Rule A (D84 rule 1).  An abstract functor's *fixed* type inputs X(F).
     wf_gsig (wf_fun_fixed) makes it exactly M(F) ∖ P(F): the type symbols
     its result mentions (`fun_mentions`, closed through relevance), less
     its type outputs, less its brackets. *)
  g_fixed : usym -> list tsym;
  (* D84 rule 1: the brackets P(F) of an abstract functor `F[P…]`, read at
     each application.  An unbracketed `functor F: …;` elaborates to
     P(F) = M(F), so X(F) = ∅; a functor with a body has no brackets. *)
  g_poly : usym -> list tsym;
  (* A functor with a body: the inputs it captures where it is bound, and
     its pins — manifest equations on captured types (Leroy 1994): `bind
     Int = Num` in GcArrays's body elaborates to the pin (Int, Num).  Any
     item may be captured, vals included (D84 3b: vals by value); that a
     family's bindings bind no vals is a condition on families (rule 7,
     `family_binding`), not one the calculus imposes on capture. *)
  g_cap : usym -> tele * subst;
  (* A genuine nominal type interface whose payload is not observed. *)
  g_opaque : nsym -> option tele;
  (* A source interface with no well-formed callable schema. It can be
     assumed in a telescope, but has no runtime inhabitants. *)
  g_opaque_fun : usym -> bool
}.

Definition mkGsigFull ty tag val fn mth functor fixed poly cap opaque : gsig :=
  mkGsigInterfaces ty tag val fn mth functor fixed poly cap opaque (fun _ => false).

(* Existing calculi declare only payload-bearing nominal types. *)
Definition mkGsig ty tag val fn mth functor fixed poly cap : gsig :=
  mkGsigInterfaces ty tag val fn mth functor fixed poly cap (fun _ => None) (fun _ => false).

(* ---- II.2 MossX, the extension the elaborator targets (moved from
   MossX.v; section numbers are its) ---- *)

Local Open Scope nat_scope.

(* ===================== 1. Values ========================================= *)

Definition fld := nat.  (* record field names, interned *)

Inductive xvalue : Type :=
| XVUnit : xvalue
| XVTag  : nsym -> xvalue -> xvalue
| XVI32  : int -> xvalue                (* invariant kept by every op: < 2^32 *)
| XVF64  : float -> xvalue
| XVRec  : list (fld * xvalue) -> xvalue
| XVF32  : float -> xvalue              (* a binary64 holding a binary32 value *)
| XVI64  : Z -> xvalue                  (* invariant kept by every op: [0, 2^64) *)
| XVRef  : int -> xvalue                (* legacy primitive heap address *)
| XVRefAt : bool -> ty -> int -> xvalue. (* allocation kind and ground held type *)

(* ===================== 2. Primitives ===================================== *)

Inductive i32bin := IAdd | ISub | IMul | IDivS | IDivU | IRemS | IRemU
                  | IAnd | IOr | IXor | IShl | IShrS | IShrU | IRotl | IRotr.
Inductive i32cmp := CEq | CNe | CLtS | CLtU | CGtS | CGtU | CLeS | CLeU
                  | CGeS | CGeU.
Inductive i32un  := UEqz | UClz | UCtz | UPopcnt | UExt8 | UExt16.
Inductive f64bin := FAdd | FSub | FMul | FDiv | FMin | FMax | FCopysign.
Inductive f64un  := FAbs | FNeg | FCeil | FFloor | FTrunc | FNearest | FSqrt.
Inductive f64cmp := FEq | FNe | FLt | FGt | FLe | FGe.
Inductive conv   := CTruncS | CTruncU | CTruncSatS | CTruncSatU
                  | CConvS | CConvU.     (* i32 <-> f64 *)
Inductive loadk  := L32 | L8S | L8U | L16S | L16U.
Inductive storek := S32 | S8 | S16.
Inductive i64un  := U64Eqz | U64Clz | U64Ctz | U64Popcnt | U64Ext8 | U64Ext16 | U64Ext32.
Inductive load64k := L64 | L64_8S | L64_8U | L64_16S | L64_16U | L64_32S | L64_32U.
Inductive store64k := S64 | S64_8 | S64_16 | S64_32.
(* i64 <-> floats: trunc (s/u, sat or not) from f64/f32, convert to f64/f32 *)
Inductive conv64 := C64Trunc (from32 : bool) (signed : bool) (sat : bool)
                  | C64Conv (to32 : bool) (signed : bool).

Inductive prim : Type :=
| PI32Bin  : i32bin -> prim
| PI32Cmp  : i32cmp -> prim
| PI32Un   : i32un -> prim
| PF64Bin  : f64bin -> prim
| PF64Cmp  : f64cmp -> prim
| PF64Un   : f64un -> prim
| PConv    : conv -> prim
| PLoad    : loadk -> prim
| PStore   : storek -> prim
| PF64Load | PF64Store
| PMemSize | PMemGrow | PMemCopy | PMemFill
| PUnreachable
| PFdWrite | PProcExit | PArgsSizesGet | PArgsGet | PPathOpen | PFdRead | PFdClose
| PI32Bool
(* f32: every result rounded to binary32 *)
| PF32Bin  : f64bin -> prim
| PF32Cmp  : f64cmp -> prim
| PF32Un   : f64un -> prim
| PConv32  : conv -> prim               (* i32 <-> f32 *)
| PF32Load | PF32Store | PDemote | PPromote | PI32ReinterpF32 | PF32ReinterpI32
(* i64 *)
| PI64Bin  : i32bin -> prim
| PI64Cmp  : i32cmp -> prim
| PI64Un   : i64un -> prim
| PWrap | PExtendS | PExtendU
| PI64Load : load64k -> prim
| PI64Store : store64k -> prim
| PConv64  : conv64 -> prim
| PI64ReinterpF64 | PF64ReinterpI64
(* D75/D81: what `bind Heap;`, `bind Mutable;`, `bind Arrays;` provide;
   PBox and PArrNew carry the `Ref` and `Array` tags *)
| PBox : nsym -> prim | PDeref | PRefSet
| PArrNew : nsym -> prim | PArrLen | PArrGet | PArrSet | PArrFill | PArrCopy
(* declared by the library but not modelled: running one stops the
   program with XUns, which the oracle reports as "unsupported" *)
| PUnsupported : nat -> prim.

(* ===================== 3. Syntax ========================================= *)

Inductive xpat : Type :=
| XPAny  : option var -> xpat                          (* _ or a binder *)
| XPHead : nsym -> option var -> list (fld * var) -> xpat.

(* The core's constructors, one for one (XVar .. XApply), then the
   extensions.  `smap` pairs in XApplyBr are (out, key): what the branch
   made under `key` is provided under `out` after it. *)
Inductive xexpr : Type :=
| XVar      : var -> xexpr
| XUnit     : xexpr
| XVl       : vsym -> xexpr
| XTag      : nsym -> subst -> xexpr -> xexpr
| XCallD    : fsym -> subst -> smap -> list xexpr -> xexpr
| XCallA    : fsym -> list xexpr -> xexpr
| XMeth     : xexpr -> msym -> item -> list xexpr -> xexpr
| XMatch    : xexpr -> list (nsym * var * xexpr) -> xexpr
| XLet      : var -> xexpr -> xexpr -> xexpr
| XBindTy   : tsym -> ty -> xexpr -> xexpr
| XBindVal  : vsym -> xexpr -> xexpr -> xexpr
| XBindFn   : fsym -> fsym -> smap -> xexpr -> xexpr
| XBindMth  : ty -> msym -> subst -> fsym -> smap -> xexpr -> xexpr
| XBindFun  : usym -> usym -> smap -> smap -> xexpr -> xexpr
| XBindFunA : usym -> usym -> smap -> xexpr -> xexpr
| XApply    : usym -> subst -> smap -> xexpr -> xexpr
| XApplyR   : usym -> smap -> smap -> xexpr -> xexpr
(* extensions *)
| XSeq      : xexpr -> xexpr -> xexpr
| XAssign   : var -> xexpr -> xexpr
| XIf       : xexpr -> xexpr -> xexpr -> xexpr
| XWhile    : xexpr -> xexpr -> xexpr
| XLoop     : xexpr -> xexpr
| XReturn   : xexpr -> xexpr
| XBreak    : xexpr
| XRec      : list (fld * xexpr) -> xexpr
| XField    : xexpr -> fld -> xexpr
| XMatchP   : xexpr -> list (xpat * xexpr) -> xexpr
| XBindFunSym  : usym -> usym -> xexpr -> xexpr   (* bind F = H (by name) *)
| XBindFunCopy : usym -> usym -> xexpr -> xexpr   (* bind F = G (abstract) *)
| XApplyBr  : usym -> list (usym * list xbind * smap) -> xexpr -> xexpr
| XBindPrim : item -> prim -> xexpr -> xexpr
| XConst : vsym -> xexpr -> xexpr
with xbind : Type :=
| XBTy    : tsym -> ty -> xbind
| XBVal   : vsym -> xexpr -> xbind
| XBFn    : fsym -> fsym -> smap -> xbind
| XBMth   : ty -> msym -> subst -> fsym -> smap -> xbind
| XBFun   : usym -> usym -> smap -> smap -> xbind
| XBFunA  : usym -> usym -> smap -> xbind
| XBApply : usym -> subst -> smap -> xbind
| XBApplyR : usym -> smap -> smap -> xbind
| XBFunSym  : usym -> usym -> xbind
| XBFunCopy : usym -> usym -> xbind
| XBApplyBr : usym -> list (usym * list xbind * smap) -> xbind
| XBPrim  : item -> prim -> xbind.

(* ===================== 4. Runtime structures ============================= *)

Inductive xdentry : Type :=
| XDVal    : xvalue -> xdentry
| XDClo    : fsym -> list (item * xdentry) -> xdentry
| XDFun    : usym -> list smap -> list (item * xdentry) -> xdentry
| XDPrim   : prim -> xdentry
| XDFunSym : usym -> xdentry
| XDType : ty -> xdentry
| XDPrimAt : prim -> ty -> xdentry.

Definition xdenv := list (item * xdentry).
Definition xvenv := list (var * xvalue).

(* Ground types are retained at runtime because native GC casts compare the
   allocation's generated Wasm type. All GC types are distinct projections of
   the emitter's single recursive group, keyed by the ground held type. *)
Fixpoint xdtypes (d : xdenv) : subst :=
  match d with
  | [] => []
  | (ITy t, XDType tau) :: r => (t, tau) :: xdtypes r
  | _ :: r => xdtypes r
  end.
Definition xground (d : xdenv) (tau : ty) : ty := app_subst (xdtypes d) tau.
Fixpoint xtype_env (d : xdenv) : xdenv :=
  match d with
  | [] => []
  | (ITy t, XDType tau) :: r => (ITy t, XDType tau) :: xtype_env r
  | _ :: r => xtype_env r
  end.
Definition xinst (d : xdenv) (th : subst) : xdenv :=
  match th with
  | [] => []
  | _ :: _ =>
      let types := xdtypes d in
      map (fun p => (ITy (fst p), XDType (app_subst types (snd p)))) th
  end.
Lemma xinst_map : forall d th,
  xinst d th = map (fun p => (ITy (fst p), XDType (xground d (snd p)))) th.
Proof. intros d [|p th]; reflexivity. Qed.
Definition xclose (d dc : xdenv) : xdenv := dc ++ xtype_env d.
Definition xprim_entry (d : xdenv) (i : item) (p : prim) : xdentry :=
  match p, i with
  | PBox _, IMth tau _ _ | PArrNew _, IMth tau _ _ => XDPrimAt p (xground d tau)
  | _, _ => XDPrim p
  end.

Fixpoint lookup_xdenv (i : item) (d : xdenv) : option xdentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_xdenv i r
  end.

Fixpoint lookup_xvenv (x : var) (g : xvenv) : option xvalue :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_xvenv x r
  end.

Fixpoint assign_xvenv (x : var) (w : xvalue) (g : xvenv) : option xvenv :=
  match g with
  | [] => None
  | (x', w') :: r =>
      if Nat.eqb x x' then Some ((x', w) :: r)
      else match assign_xvenv x w r with
           | Some r' => Some ((x', w') :: r')
           | None => None
           end
  end.

Fixpoint xcompose (d : xdenv) (sm : smap) : option xdenv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_xdenv s d, xcompose d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

Fixpoint xrun_chain (d : xdenv) (sms : list smap) : option xdenv :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match xrun_chain d r with
      | Some d' => xcompose d' sm
      | None => None
      end
  end.

Fixpoint find_xarm (n : nsym) (arms : list (nsym * var * xexpr))
  : option (var * xexpr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_xarm n r
  end.

Definition mk_xvenv (ws : list xvalue) : xvenv := combine (seq 0 (List.length ws)) ws.

Fixpoint find_branch (h : usym) (brs : list (usym * list xbind * smap))
  : option (list xbind * smap) :=
  match brs with
  | [] => None
  | (h', bs, pairs) :: r => if Nat.eqb h h' then Some (bs, pairs) else find_branch h r
  end.

(* The program: function bodies, core functor bodies, and the symbols the
   extended constructs need to know (Bool's units, for `if`). *)
Record xcastspec : Type := XCastSpec {
  xc_array : bool;
  xc_held : tsym;
  (* Ref and Array are ordinary wrappers when their respective raw
     handle payload is rebound to a different representation. *)
  xc_handle : option (tsym * ty)
}.
Record xprog : Type := mkXProg {
  xp_fn    : fsym -> option xexpr;
  xp_fun   : usym -> option (tele * list xbind);
  xp_bool  : nsym;
  xp_true  : nsym;
  xp_false : nsym;
  xp_argv  : list (list int);
  (* the host filesystem, an input like argv: a path's bytes, if readable *)
  xp_read  : list int -> option (list int);
  (* selected Ref/Array tag -> (array?, held-type parameter) *)
  xp_cast : list (nsym * xcastspec);
  xp_inputs : usym -> list tsym;
  xp_exports : usym -> smap;
  xp_immutable : bool;
  xp_fixed_inst : usym -> subst;
  (* Actual immutable fixed inputs; their meanings are xp_fixed_inst. *)
  xp_fixed_keys : usym -> list tsym
}.

(* Core declarations use erased fixed dependencies. Immutable provisioning
   reads the actual fixed keys and their captured interpretations here. *)
Definition runtime_fixed (Sg : gsig) (P : xprog) (u : usym) : list tsym :=
  if xp_immutable P then xp_fixed_keys P u else g_fixed Sg u.
Definition runtime_signature (Sg : gsig) (P : xprog) : gsig :=
  mkGsigInterfaces (g_ty Sg) (g_tag Sg) (g_val Sg) (g_fn Sg) (g_mth Sg) (g_fun Sg)
    (runtime_fixed Sg P) (g_poly Sg) (g_cap Sg) (g_opaque Sg) (g_opaque_fun Sg).

Fixpoint lookup_cast (n : nsym) (cs : list (nsym * xcastspec)) : option xcastspec :=
  match cs with
  | [] => None
  | (n', c) :: r => if Nat.eqb n n' then Some c else lookup_cast n r
  end.
Definition xreify (P : xprog) : bool :=
  xp_immutable P || match xp_cast P with [] => false | _ => true end.
Definition xbind_ty (P : xprog) (d : xdenv) (t : tsym) (tau : ty) : xdenv :=
  if xreify P then (ITy t, XDType (xground d tau)) :: d else d.
Definition xcall_env (P : xprog) (d : xdenv) (th : subst) (dc : xdenv) : xdenv :=
  if xreify P then xinst d th ++ xclose d dc else dc.

(* A linked provider receives only its own formal type inputs. Extra
   abstract-functor inputs must not override lexical captured meanings. *)
Definition xapply_env (P : xprog) (d : xdenv) (h : usym) (th : subst) (dc : xdenv) : xdenv :=
  xcall_env P d
    (filter (fun p => existsb (Nat.eqb (fst p)) (xp_inputs P h)) th) dc.

(* Applying a functor exports the type meanings its result declares, beside
   its executable non-type provisions. Captured input types remain private. *)
Definition xresult_types (Dr : tele) (d3 : xdenv) : xdenv :=
  filter (fun p => existsb (item_eqb (fst p)) Dr) (xtype_env d3).
Definition xresult_env (P : xprog) (d3 : xdenv) (Dr : tele) (dr : xdenv) : xdenv :=
  if xreify P then dr ++ xresult_types Dr d3 else dr.

(* Outcomes.  XOk/XErr/XFuel are the core's Ok/Err/OutOfFuel; XRet and XBrk
   are control transfers; XTrap is a Wasm trap (well-defined failure),
   XExit a WASI proc_exit, XUns a primitive this model does not implement.
   The nat in XErr/XTrap/XUns is a diagnostic code only. *)
Inductive xout : Type :=
| XOk   : xvalue -> xout
| XRet  : xvalue -> xout
| XBrk  : xout
| XErr  : nat -> xout
| XTrap : nat -> xout
| XExit : int -> xout
| XUns  : nat -> xout
| XFuel : xout.

(* What the elaborator outputs (moved from Elab.v): the bodies of every
   defined fn (and of the fns elaboration synthesizes: forwarders, lambda
   providers, wrappers), the core functor bodies `bind F = G;` made, Bool's
   symbols, `main`, main's initial runtime environment (the platform's
   provisions for what main assumes), and the names of primitives the
   model does not implement (PUnsupported i is the i-th). *)
Record xprogram : Type := XProgram {
  xo_fns : list (nat * xexpr);
  xo_funs : list (nat * (tele * list xbind));
  xo_bool : nat; xo_true : nat; xo_false : nat;
  xo_main : nat;
  xo_env : list (item * xdentry);
  xo_prims : list string;
  xo_cast : list (nsym * xcastspec);
  xo_inputs : list (usym * list tsym);
  xo_exports : list (usym * smap);
  xo_immutable : bool;
  xo_fixed_inst : list (usym * subst);
  xo_fixed_keys : list (usym * list tsym)
}.

(* ######################################################################## *)
(* Part III. Dynamic semantics                                               *)
(* ######################################################################## *)

(* ---- III.1 Core Moss (moved from CoreMoss.v) ---- *)

(* ===================== 10. Dynamics ====================================== *)

(* Runtime contexts: val items carry values; fn and method items carry
   (code, captured context) pairs — the dynamic residue of [D2]. *)
Inductive dentry : Type :=
| DVal : value -> dentry
| DClo : fsym -> list (item * dentry) -> dentry
(* a functor binding [D78]: the functor with a body that answers, and the
   chain of re-keyings out to the abstract functor's arguments — no
   captured context: the arguments are supplied at each application *)
| DFun : usym -> list smap -> list (item * dentry) -> dentry.
(* Rule A: DFun h sms dc also holds dc, the entries h captured where it was
   bound — a closure, as DClo's. *)

Definition denv := list (item * dentry).
Definition venv := list (var * value).

Fixpoint lookup_denv (i : item) (d : denv) : option dentry :=
  match d with
  | [] => None
  | (i', en) :: r => if item_eqb i i' then Some en else lookup_denv i r
  end.

Fixpoint lookup_venv (x : var) (g : venv) : option value :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_venv x r
  end.

(* δ ∘ ς: the caller's provisions, restricted and re-keyed to the callee's
   own requirement names. *)
Fixpoint compose_smap (d : denv) (sm : smap) : option denv :=
  match sm with
  | [] => Some []
  | (req, s) :: r =>
      match lookup_denv s d, compose_smap d r with
      | Some en, Some d' => Some ((req, en) :: d')
      | _, _ => None
      end
  end.

(* Running a binding's chain of re-keyings: the outermost link (the last)
   first, so the result is keyed by the bound functor's own arguments. *)
Fixpoint run_chain (d : denv) (sms : list smap) : option denv :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match run_chain d r with
      | Some d' => compose_smap d' sm
      | None => None
      end
  end.

(* The identity re-keying of a result telescope's non-type items: running
   it restricts an environment to exactly the result [D78]. *)
Definition id_smap (D : tele) : smap :=
  map (fun i => (i, i)) (filter (fun i => negb (is_ty_item i)) D).

(* Rule A: an application at θ files each result entry under the result
   item read at θ — the key its scope refers to it by. *)
Definition inst_smap (th : subst) (D : tele) : smap :=
  map (fun i => (app_subst_item th i, i)) (filter (fun i => negb (is_ty_item i)) D).

Fixpoint find_arm (n : nsym) (arms : list (nsym * var * expr))
  : option (var * expr) :=
  match arms with
  | [] => None
  | (n', x, eb) :: r => if Nat.eqb n n' then Some (x, eb) else find_arm n r
  end.

Definition mk_venv (ws : list value) : venv :=
  combine (seq 0 (length ws)) ws.

(* Big-step evaluation (Figure 7 of the paper), with a functor body's run
   and an application [D78] defined alongside: an application looks up the
   binding, re-keys the caller's environment to the bound functor's own
   arguments, runs its binds there, and keeps exactly the result. *)
Inductive eval (Sg : gsig) : denv -> venv -> expr -> value -> Prop :=
| EvUnit : forall d g,
    eval Sg d g EUnit VUnit
| EvVar : forall d g x w,
    lookup_venv x g = Some w ->
    eval Sg d g (EVar x) w
| EvVal : forall d g v w,
    lookup_denv (IVal v) d = Some (DVal w) ->
    eval Sg d g (EVl v) w
| EvTag : forall d g n th e w,
    eval Sg d g e w ->
    eval Sg d g (ETag n th e) (VTag n w)
| EvLet : forall d g x e1 e2 w1 w,
    eval Sg d g e1 w1 ->
    eval Sg d ((x, w1) :: g) e2 w ->
    eval Sg d g (ELet x e1 e2) w
| EvMatch : forall d g e0 arms n w' x eb w,
    eval Sg d g e0 (VTag n w') ->
    find_arm n arms = Some (x, eb) ->
    eval Sg d ((x, w') :: g) eb w ->
    eval Sg d g (EMatch e0 arms) w
| EvCallD : forall d g f th sm args D S body ws d' w,
    g_fn Sg f = Some (D, S, Some body) ->
    Forall2 (eval Sg d g) args ws ->
    compose_smap d sm = Some d' ->
    eval Sg d' (mk_venv ws) body w ->
    eval Sg d g (ECallD f th sm args) w
| EvCallA : forall d g f args f' df D' S' body ws w,
    lookup_denv (IFn f) d = Some (DClo f' df) ->
    g_fn Sg f' = Some (D', S', Some body) ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg df (mk_venv ws) body w ->
    eval Sg d g (ECallA f args) w
| EvMeth : forall d g e0 m prov args f' d' D' S' body w0 ws w,
    lookup_denv prov d = Some (DClo f' d') ->
    g_fn Sg f' = Some (D', S', Some body) ->
    eval Sg d g e0 w0 ->
    Forall2 (eval Sg d g) args ws ->
    eval Sg d' (mk_venv (w0 :: ws)) body w ->
    eval Sg d g (EMeth e0 m prov args) w
| EvBindTy : forall d g t tau e w,
    eval Sg d g e w ->
    eval Sg d g (EBindTy t tau e) w
| EvBindVal : forall d g v e1 e w1 w,
    eval Sg d g e1 w1 ->
    eval Sg ((IVal v, DVal w1) :: d) g e w ->
    eval Sg d g (EBindVal v e1 e) w
| EvBindFn : forall d g f f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IFn f, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindFn f f' sm e) w
| EvBindMth : forall d g tauk m th f' sm dc e w,
    compose_smap d sm = Some dc ->
    eval Sg ((IMth tauk m th, DClo f' dc) :: d) g e w ->
    eval Sg d g (EBindMth tauk m th f' sm e) w
| EvBindFun : forall d g u h sm smc dc e w,
    compose_smap d smc = Some dc ->
    eval Sg ((IFun u, DFun h [sm] dc) :: d) g e w ->
    eval Sg d g (EBindFun u h sm smc e) w
| EvBindFunA : forall d g u u' sm h sms dc e w,
    lookup_denv (IFun u') d = Some (DFun h sms dc) ->
    eval Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e w ->
    eval Sg d g (EBindFunA u u' sm e) w
| EvApply : forall d g u th sm dr e w,
    run_apply Sg d u th sm dr ->
    eval Sg (dr ++ d) g e w ->
    eval Sg d g (EApply u th sm e) w
with run_binds (Sg : gsig) : denv -> list bnd -> denv -> Prop :=
| RbNil : forall d, run_binds Sg d [] d
| RbTy : forall d t tau r d',
    run_binds Sg d r d' ->
    run_binds Sg d (BTy t tau :: r) d'
| RbVal : forall d v e1 w1 r d',
    eval Sg d [] e1 w1 ->
    run_binds Sg ((IVal v, DVal w1) :: d) r d' ->
    run_binds Sg d (BVal v e1 :: r) d'
| RbFn : forall d f f' sm dc r d',
    compose_smap d sm = Some dc ->
    run_binds Sg ((IFn f, DClo f' dc) :: d) r d' ->
    run_binds Sg d (BFn f f' sm :: r) d'
| RbMth : forall d tauk m th f' sm dc r d',
    compose_smap d sm = Some dc ->
    run_binds Sg ((IMth tauk m th, DClo f' dc) :: d) r d' ->
    run_binds Sg d (BMth tauk m th f' sm :: r) d'
| RbFun : forall d u h sm smc dc r d',
    compose_smap d smc = Some dc ->
    run_binds Sg ((IFun u, DFun h [sm] dc) :: d) r d' ->
    run_binds Sg d (BFun u h sm smc :: r) d'
| RbFunA : forall d u u' sm h sms dc r d',
    lookup_denv (IFun u') d = Some (DFun h sms dc) ->
    run_binds Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r d' ->
    run_binds Sg d (BFunA u u' sm :: r) d'
| RbApply : forall d u th sm dr r d',
    run_apply Sg d u th sm dr ->
    run_binds Sg (dr ++ d) r d' ->
    run_binds Sg d (BApply u th sm :: r) d'
with run_apply (Sg : gsig) : denv -> usym -> subst -> smap -> denv -> Prop :=
(* Rule A: the body runs over its linked arguments, then what its binding
   captured; the result is filed under its items read at θ. *)
| RunApply : forall d u th sm h sms dc Dh Dr bs d1 d2 d3 dr,
    lookup_denv (IFun u) d = Some (DFun h sms dc) ->
    g_fun Sg h = Some (Dh, Dr, Some bs) ->
    compose_smap d sm = Some d1 ->
    run_chain d1 sms = Some d2 ->
    run_binds Sg (d2 ++ dc) bs d3 ->
    compose_smap d3 (inst_smap th Dr) = Some dr ->
    run_apply Sg d u th sm dr.

(* An executable twin: a fueled definitional interpreter.  It is the
   deterministic implementation of `eval`, and its Err result is the
   instrumented error of Theorem 4.2 (any missing lookup or shape mismatch). *)
Inductive res : Type :=
| Ok        : value -> res
| Err       : res
| OutOfFuel : res.

Fixpoint interp (fuel : nat) (Sg : gsig) (d : denv) (g : venv) (e : expr)
  {struct fuel} : res :=
  match fuel with
  | 0 => OutOfFuel
  | S k =>
      let interp_args := interp_list k Sg d g in
      match e with
      | EVar x => match lookup_venv x g with
                  | Some w => Ok w
                  | None => Err
                  end
      | EUnit => Ok VUnit
      | EVl v => match lookup_denv (IVal v) d with
                 | Some (DVal w) => Ok w
                 | _ => Err
                 end
      | ETag n th e1 =>
          match interp k Sg d g e1 with
          | Ok w => Ok (VTag n w)
          | rr => rr
          end
      | ECallD f th sm args =>
          match g_fn Sg f with
          | Some (_, _, Some body) =>
              match interp_args args with
              | inl ws =>
                  match compose_smap d sm with
                  | Some d' => interp k Sg d' (mk_venv ws) body
                  | None => Err
                  end
              | inr rr => rr
              end
          | _ => Err
          end
      | ECallA f args =>
          match lookup_denv (IFn f) d with
          | Some (DClo f' df) =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp_args args with
                  | inl ws => interp k Sg df (mk_venv ws) body
                  | inr rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMeth e0 m prov args =>
          match lookup_denv prov d with
          | Some (DClo f' d') =>
              match g_fn Sg f' with
              | Some (_, _, Some body) =>
                  match interp k Sg d g e0 with
                  | Ok w0 =>
                      match interp_args args with
                      | inl ws => interp k Sg d' (mk_venv (w0 :: ws)) body
                      | inr rr => rr
                      end
                  | rr => rr
                  end
              | _ => Err
              end
          | _ => Err
          end
      | EMatch e0 arms =>
          match interp k Sg d g e0 with
          | Ok (VTag n w') =>
              match find_arm n arms with
              | Some (x, eb) => interp k Sg d ((x, w') :: g) eb
              | None => Err
              end
          | Ok VUnit => Err
          | rr => rr
          end
      | ELet x e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg d ((x, w1) :: g) e2
          | rr => rr
          end
      | EBindTy _ _ e1 => interp k Sg d g e1
      | EBindVal v e1 e2 =>
          match interp k Sg d g e1 with
          | Ok w1 => interp k Sg ((IVal v, DVal w1) :: d) g e2
          | rr => rr
          end
      | EBindFn f f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IFn f, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindMth tauk m th f' sm e1 =>
          match compose_smap d sm with
          | Some dc => interp k Sg ((IMth tauk m th, DClo f' dc) :: d) g e1
          | None => Err
          end
      | EBindFun u h sm smc e1 =>
          match compose_smap d smc with
          | Some dc => interp k Sg ((IFun u, DFun h [sm] dc) :: d) g e1
          | None => Err
          end
      | EBindFunA u u' sm e1 =>
          match lookup_denv (IFun u') d with
          | Some (DFun h sms dc) =>
              interp k Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) g e1
          | _ => Err
          end
      | EApply u th sm e1 =>
          match interp_apply k Sg d u th sm with
          | inl dr => interp k Sg (dr ++ d) g e1
          | inr rr => rr
          end
      end
  end
with interp_list (fuel : nat) (Sg : gsig) (d : denv) (g : venv)
    (l : list expr) {struct fuel} : sum (list value) res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match l with
      | [] => inl []
      | e1 :: r =>
          match interp k Sg d g e1 with
          | Ok w =>
              match interp_list k Sg d g r with
              | inl ws => inl (w :: ws)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with interp_binds (fuel : nat) (Sg : gsig) (d : denv) (bs : list bnd)
    {struct fuel} : sum denv res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match bs with
      | [] => inl d
      | BTy _ _ :: r => interp_binds k Sg d r
      | BVal v e1 :: r =>
          match interp k Sg d [] e1 with
          | Ok w1 => interp_binds k Sg ((IVal v, DVal w1) :: d) r
          | rr => inr rr
          end
      | BFn f f' sm :: r =>
          match compose_smap d sm with
          | Some dc => interp_binds k Sg ((IFn f, DClo f' dc) :: d) r
          | None => inr Err
          end
      | BMth tauk m th f' sm :: r =>
          match compose_smap d sm with
          | Some dc => interp_binds k Sg ((IMth tauk m th, DClo f' dc) :: d) r
          | None => inr Err
          end
      | BFun u h sm smc :: r =>
          match compose_smap d smc with
          | Some dc => interp_binds k Sg ((IFun u, DFun h [sm] dc) :: d) r
          | None => inr Err
          end
      | BFunA u u' sm :: r =>
          match lookup_denv (IFun u') d with
          | Some (DFun h sms dc) =>
              interp_binds k Sg ((IFun u, DFun h (sms ++ [sm]) dc) :: d) r
          | _ => inr Err
          end
      | BApply u th sm :: r =>
          match interp_apply k Sg d u th sm with
          | inl dr => interp_binds k Sg (dr ++ d) r
          | inr rr => inr rr
          end
      end
  end
with interp_apply (fuel : nat) (Sg : gsig) (d : denv) (u : usym) (th : subst)
    (sm : smap) {struct fuel} : sum denv res :=
  match fuel with
  | 0 => inr OutOfFuel
  | S k =>
      match lookup_denv (IFun u) d with
      | Some (DFun h sms dc) =>
          match g_fun Sg h with
          | Some (_, Dr, Some bs) =>
              match compose_smap d sm with
              | Some d1 =>
                  match run_chain d1 sms with
                  | Some d2 =>
                      match interp_binds k Sg (d2 ++ dc) bs with
                      | inl d3 =>
                          match compose_smap d3 (inst_smap th Dr) with
                          | Some dr => inl dr
                          | None => inr Err
                          end
                      | inr rr => inr rr
                      end
                  | None => inr Err
                  end
              | None => inr Err
              end
          | _ => inr Err
          end
      | _ => inr Err
      end
  end.

(* Specialization of code (moved from Specialization.v): substitute a
   grounding g of the type symbols into an elaborated term's type
   annotations, following type binds.  Provision keys and θ of
   applications stay (they are runtime keys). *)
Fixpoint spec (g : subst) (e : expr) : expr :=
  match e with
  | EVar x => EVar x
  | EUnit => EUnit
  | EVl v => EVl v
  | ETag n th e1 => ETag n (map_snd (app_subst g) th) (spec g e1)
  | ECallD f th sm args =>
      ECallD f (map_snd (app_subst g) th) sm
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: r => spec g a :: go r end) args)
  | ECallA f args =>
      ECallA f ((fix go (l : list expr) : list expr :=
                   match l with [] => [] | a :: r => spec g a :: go r end) args)
  | EMeth e0 m prov args =>
      EMeth (spec g e0) m prov
        ((fix go (l : list expr) : list expr :=
            match l with [] => [] | a :: r => spec g a :: go r end) args)
  | EMatch e0 arms =>
      EMatch (spec g e0)
        ((fix go (l : list (nsym * var * expr)) :=
            match l with
            | [] => []
            | (n, x, eb) :: r => (n, x, spec g eb) :: go r
            end) arms)
  | ELet x e1 e2 => ELet x (spec g e1) (spec g e2)
  | EBindTy t tau e1 =>
      EBindTy t (app_subst g tau) (spec ((t, app_subst g tau) :: g) e1)
  | EBindVal v e1 e2 => EBindVal v (spec g e1) (spec g e2)
  | EBindFn f f' sm e1 => EBindFn f f' sm (spec g e1)
  | EBindMth tk m th f' sm e1 => EBindMth tk m th f' sm (spec g e1)
  | EBindFun u h sm smc e1 => EBindFun u h sm smc (spec g e1)
  | EBindFunA u w sm e1 => EBindFunA u w sm (spec g e1)
  (* Rule A: an application's θ keys its result, so it stays a label, as
     provision keys do *)
  | EApply u th sm e1 => EApply u th sm (spec g e1)
  end.


(* ---- III.2 MossX, over an abstract primitive semantics ----

   The interpreter below is MossX.v's, verbatim, except that the machine
   state and what a primitive does are parameters: a Wasm instruction or a
   WASI call is `run_prim P p args st`, for whatever `run_prim` an
   instantiation supplies.  MossX.v supplies the concrete one (paged linear
   memory over PArray, IEEE binary64/binary32 via PrimFloat, wrapping i32/i64
   arithmetic, WASI fd_write/args/path_open/fd_read, the GC heap) and
   instantiates this functor with it.

   What this leaves trusted: nothing any obligation of Part VI depends on.
   Every obligation either holds for every primitive semantics (determinism,
   conservativity, the elaboration relation) or assumes exactly the law
   `PRIM_LAW` of Part IV of it.  Whether MossX's concrete primitives are the
   *right* meaning of `i32.add` or `fd_write` matters to the differential
   tests against the compilers, not to any theorem; it is checked there, not
   here. *)
Module Type PRIM_SEMANTICS.
  Parameter mstate : Type.
  Parameter run_prim : xprog -> prim -> list xvalue -> mstate -> xout * mstate.
End PRIM_SEMANTICS.

Module XSem (Import Prims : PRIM_SEMANTICS).
Record xres : Type := XR { xr_out : xout; xr_g : xvenv; xr_st : mstate }.

(* ===================== 9. The interpreter ================================ *)

(* Diagnostic codes for XErr (stuck states) *)
Definition e_var := 1.      Definition e_val := 2.      Definition e_compose := 3.
Definition e_nobody := 4.   Definition e_break := 5.    Definition e_entry := 6.
Definition e_match := 7.    Definition e_chain := 8.    Definition e_assign := 9.
Definition e_bool := 10.    Definition e_field := 11.   Definition e_branch := 12.
Definition e_fun := 13.     Definition e_pat := 14.

Definition ret_call (g : xvenv) (r : xres) : xres :=
  match r with
  | XR o _ st => XR (match o with XRet w => XOk w | XBrk => XErr e_break | o => o end) g st
  end.

Definition pop1 (r : xres) : xres :=
  match r with XR o g st => XR o (tl g) st end.

Definition popn (n : nat) (r : xres) : xres :=
  match r with XR o g st => XR o (skipn n g) st end.

Definition with_g (g : xvenv) (r : xres) : xres :=
  match r with XR o _ st => XR o g st end.

(* A checked source wrapper is the native ref.cast_null operation. The
   metadata is the allocation's runtime type header, carried by an opaque
   handle; extracting/repacking a handle never changes that header. *)
Definition xcast (P : xprog) (d : xdenv) (n : nsym) (th : subst) (w : xvalue) : xout :=
  match lookup_cast n (xp_cast P) with
  | None => XOk (XVTag n w)
  | Some spec =>
      let enabled := match xc_handle spec with
                     | None => true
                     | Some (h, expected) => ty_eqb (xground d (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
                     end in
      if enabled then
        match lookup_subst (xc_held spec) th, w with
        | Some tau, XVRefAt k' tau' _ =>
            if Bool.eqb (xc_array spec) k' && ty_eqb (xground d tau) tau'
            then XOk (XVTag n w) else XTrap 37
        | _, _ => XTrap 37
        end
      else XOk (XVTag n w)
  end.
Definition xgc_receiver (w : xvalue) : xvalue :=
  match w with
  | XVTag n (XVRefAt _ _ a) => XVTag n (XVRef a)
  | _ => w
  end.
Definition xgc_args (p : prim) (ws : list xvalue) : list xvalue :=
  match p, ws with
  | PArrCopy, [r; i; s; j; count] => [xgc_receiver r; i; xgc_receiver s; j; count]
  | PDeref, r :: args | PRefSet, r :: args | PArrLen, r :: args
  | PArrGet, r :: args | PArrSet, r :: args | PArrFill, r :: args => xgc_receiver r :: args
  | _, _ => ws
  end.
Definition xallocation (p : prim) (tau : ty) (o : xout) : xout :=
  match p, o with
  | PBox _, XOk (XVTag n (XVRef a)) => XOk (XVTag n (XVRefAt false tau a))
  | PArrNew _, XOk (XVTag n (XVRef a)) => XOk (XVTag n (XVRefAt true tau a))
  | _, _ => o
  end.
Definition run_prim_at (P : xprog) (p : prim) (ot : option ty)
    (ws : list xvalue) (st : mstate) : xout * mstate :=
  let '(o, st') := run_prim P p (xgc_args p ws) st in
  (match ot with Some tau => xallocation p tau o | None => o end, st').

Definition truthy (P : xprog) (w : xvalue) : option bool :=
  match w with
  | XVTag n XVUnit => Some (Nat.eqb n (xp_true P))
  | XVTag _ (XVTag n XVUnit) => Some (Nat.eqb n (xp_true P))
  | _ => None
  end.

Fixpoint assoc_fld (f : fld) (fs : list (fld * xvalue)) : option xvalue :=
  match fs with
  | [] => None
  | (f', w) :: r => if Nat.eqb f f' then Some w else assoc_fld f r
  end.

Definition get_field (f : fld) (w : xvalue) : option xvalue :=
  match w with
  | XVRec fs => assoc_fld f fs
  | XVTag _ (XVRec fs) => assoc_fld f fs
  | _ => None
  end.

Inductive pm : Type := PMatch (bs : xvenv) | PNo | PBad.

Fixpoint bind_fields (fs : list (fld * var)) (r : list (fld * xvalue)) : option xvenv :=
  match fs with
  | [] => Some []
  | (f, x) :: rest =>
      match assoc_fld f r, bind_fields rest r with
      | Some w, Some bs => Some ((x, w) :: bs)
      | _, _ => None
      end
  end.

Definition match_pat (p : xpat) (w : xvalue) : pm :=
  match p with
  | XPAny None => PMatch []
  | XPAny (Some x) => PMatch [(x, w)]
  | XPHead n ob fs =>
      match w with
      | XVTag n' pay =>
          if Nat.eqb n n' then
            let b1 := match ob with Some x => [(x, pay)] | None => [] end in
            match fs with
            | [] => PMatch b1
            | _ => match pay with
                   | XVRec r => match bind_fields fs r with
                                | Some bs => PMatch (bs ++ b1)
                                | None => PBad
                                end
                   | _ => PBad
                   end
            end
          else PNo
      | _ => PNo
      end
  end.

Fixpoint select_arm (arms : list (xpat * xexpr)) (w : xvalue)
  : option (xvenv * xexpr) :=
  match arms with
  | [] => None
  | (p, eb) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, eb)
      | PNo => select_arm r w
      | PBad => None
      end
  end.

Fixpoint xinterp (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (e : xexpr) {struct fuel} : xres :=
  match fuel with
  | 0 => XR XFuel g st
  | S k =>
      match e with
      | XVar x => match lookup_xvenv x g with
                  | Some w => XR (XOk w) g st
                  | None => XR (XErr e_var) g st
                  end
      | XUnit => XR (XOk XVUnit) g st
      | XVl v => match lookup_xdenv (IVal v) d with
                 | Some (XDVal w) => XR (XOk w) g st
                 | _ => XR (XErr e_val) g st
                 end
      | XTag n th e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (xcast P d n th w) g1 st1
          | r => r
          end
      | XCallD f th sm args =>
          match xp_fn P f with
          | Some body =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  match xcompose d sm with
                  | Some d' => ret_call g1 (xinterp k P (xcall_env P d th d') (mk_xvenv ws) st1 body)
                  | None => XR (XErr e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => XR (XErr e_nobody) g st
          end
      | XCallA f args =>
          match lookup_xdenv (IFn f) d with
          | Some (XDClo f' df) =>
              match xp_fn P f' with
              | Some body =>
                  match xinterp_list k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (xinterp k P df (mk_xvenv ws) st1 body)
                  | inr r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (XDPrim p) =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p None ws st1 in XR o g1 st2
              | inr r => r
              end
          | Some (XDPrimAt p tau) =>
              match xinterp_list k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p (Some tau) ws st1 in XR o g1 st2
              | inr r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMeth e0 m prov args =>
          match lookup_xdenv prov d with
          | Some (XDClo f' d') =>
              match xp_fn P f' with
              | Some body =>
                  match xinterp k P d g st e0 with
                  | XR (XOk w0) g0 st0 =>
                      match xinterp_list k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (xinterp k P d' (mk_xvenv (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => XR (XErr e_nobody) g st
              end
          | Some (XDPrim p) =>
              match xinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match xinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p None (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | Some (XDPrimAt p tau) =>
              match xinterp k P d g st e0 with
              | XR (XOk w0) g0 st0 =>
                  match xinterp_list k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p (Some tau) (w0 :: ws) st1 in XR o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => XR (XErr e_entry) g st
          end
      | XMatch e0 arms =>
          match xinterp k P d g st e0 with
          | XR (XOk (XVTag n w')) g1 st1 =>
              match find_xarm n arms with
              | Some (x, eb) => pop1 (xinterp k P d ((x, w') :: g1) st1 eb)
              | None => XR (XErr e_match) g1 st1
              end
          | XR (XOk _) g1 st1 => XR (XErr e_match) g1 st1
          | r => r
          end
      | XLet x e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => pop1 (xinterp k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | XBindTy t tau e1 => xinterp k P (xbind_ty P d t tau) g st e1
      | XBindVal v e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => xinterp k P ((IVal v, XDVal w1) :: d) g1 st1 e2
          | r => r
          end
      | XBindFn f f' sm e1 =>
          match xcompose d sm with
          | Some dc => xinterp k P ((IFn f, XDClo f' (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindMth tauk m th f' sm e1 =>
          match xcompose d sm with
          | Some dc => xinterp k P ((IMth tauk m th, XDClo f' (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFun u h sm smc e1 =>
          match xcompose d smc with
          | Some dc => xinterp k P ((IFun u, XDFun h [sm] (xclose d dc)) :: d) g st e1
          | None => XR (XErr e_compose) g st
          end
      | XBindFunA u u' sm e1 =>
          match lookup_xdenv (IFun u') d with
          | Some (XDFun h sms dc) =>
              xinterp k P ((IFun u, XDFun h (sms ++ [sm]) dc) :: d) g st e1
          | _ => XR (XErr e_fun) g st
          end
      | XApply u th sm e1 =>
          match xinterp_apply k P d u th sm st with
          | inl (dr, st1) => xinterp k P (dr ++ d) g st1 e1
          | inr r => with_g g r
          end
      | XApplyR u inputs outputs e1 =>
          match xinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => xinterp k P (dr ++ d) g st1 e1
          | inr rr => with_g g rr
          end
      (* extensions *)
      | XSeq e1 e2 =>
          match xinterp k P d g st e1 with
          | XR (XOk _) g1 st1 => xinterp k P d g1 st1 e2
          | r => r
          end
      | XAssign x e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match assign_xvenv x w g1 with
              | Some g2 => XR (XOk XVUnit) g2 st1
              | None => XR (XErr e_assign) g1 st1
              end
          | r => r
          end
      | XIf c e1 e2 =>
          match xinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true => xinterp k P d g1 st1 e1
              | Some false => xinterp k P d g1 st1 e2
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XWhile c b =>
          match xinterp k P d g st c with
          | XR (XOk w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match xinterp k P d g1 st1 b with
                  | XR (XOk _) g2 st2 => xinterp k P d g2 st2 (XWhile c b)
                  | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
                  | r => r
                  end
              | Some false => XR (XOk XVUnit) g1 st1
              | None => XR (XErr e_bool) g1 st1
              end
          | r => r
          end
      | XLoop b =>
          match xinterp k P d g st b with
          | XR (XOk _) g2 st2 => xinterp k P d g2 st2 (XLoop b)
          | XR XBrk g2 st2 => XR (XOk XVUnit) g2 st2
          | r => r
          end
      | XReturn e1 =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 => XR (XRet w) g1 st1
          | r => r
          end
      | XBreak => XR XBrk g st
      | XRec fs =>
          match xinterp_list k P d g st (map snd fs) with
          | inl (ws, g1, st1) => XR (XOk (XVRec (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | XField e1 f =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match get_field f w with
              | Some v => XR (XOk v) g1 st1
              | None => XR (XErr e_field) g1 st1
              end
          | r => r
          end
      | XMatchP e0 arms =>
          match xinterp k P d g st e0 with
          | XR (XOk w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (xinterp k P d (bs ++ g1) st1 eb)
              | None => XR (XErr e_pat) g1 st1
              end
          | r => r
          end
      | XBindFunSym u h e1 => xinterp k P ((IFun u, XDFunSym h) :: d) g st e1
      | XBindFunCopy u u' e1 =>
          match lookup_xdenv (IFun u') d with
          | Some en => xinterp k P ((IFun u, en) :: d) g st e1
          | None => XR (XErr e_fun) g st
          end
      | XApplyBr u brs e1 =>
          match xinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => xinterp k P (dr ++ d) g1 st1 e1
          | inr r => r
          end
      | XBindPrim i p e1 => xinterp k P ((i, xprim_entry d i p) :: d) g st e1
      | XConst _ initializer => match k with
          | O => XR XFuel g st
          | S _ => xinterp k P d g st initializer end
      end
  end
with xinterp_list (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (l : list xexpr) {struct fuel} : (list xvalue * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match xinterp k P d g st e1 with
          | XR (XOk w) g1 st1 =>
              match xinterp_list k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with xinterp_binds (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (bs : list xbind) {struct fuel} : (xdenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | XBTy t tau :: r => xinterp_binds k P (xbind_ty P d t tau) g st r
      | XBVal v e1 :: r =>
          match xinterp k P d g st e1 with
          | XR (XOk w1) g1 st1 => xinterp_binds k P ((IVal v, XDVal w1) :: d) g1 st1 r
          | rr => inr rr
          end
      | XBFn f f' sm :: r =>
          match xcompose d sm with
          | Some dc => xinterp_binds k P ((IFn f, XDClo f' (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBMth tauk m th f' sm :: r =>
          match xcompose d sm with
          | Some dc => xinterp_binds k P ((IMth tauk m th, XDClo f' (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFun u h sm smc :: r =>
          match xcompose d smc with
          | Some dc => xinterp_binds k P ((IFun u, XDFun h [sm] (xclose d dc)) :: d) g st r
          | None => inr (XR (XErr e_compose) g st)
          end
      | XBFunA u u' sm :: r =>
          match lookup_xdenv (IFun u') d with
          | Some (XDFun h sms dc) =>
              xinterp_binds k P ((IFun u, XDFun h (sms ++ [sm]) dc) :: d) g st r
          | _ => inr (XR (XErr e_fun) g st)
          end
      | XBApply u th sm :: r =>
          match xinterp_apply k P d u th sm st with
          | inl (dr, st1) => xinterp_binds k P (dr ++ d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBApplyR u inputs outputs :: r =>
          match xinterp_apply_r k P d u inputs outputs st with
          | inl (dr, st1) => xinterp_binds k P (dr ++ d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | XBFunSym u h :: r => xinterp_binds k P ((IFun u, XDFunSym h) :: d) g st r
      | XBFunCopy u u' :: r =>
          match lookup_xdenv (IFun u') d with
          | Some en => xinterp_binds k P ((IFun u, en) :: d) g st r
          | None => inr (XR (XErr e_fun) g st)
          end
      | XBApplyBr u brs :: r =>
          match xinterp_branch k P d g st u brs with
          | inl (dr, g1, st1) => xinterp_binds k P (dr ++ d) g1 st1 r
          | inr rr => inr rr
          end
      | XBPrim i p :: r => xinterp_binds k P ((i, xprim_entry d i p) :: d) g st r
      end
  end
with xinterp_apply (fuel : nat) (P : xprog) (d : xdenv) (u : usym) (th : subst)
    (sm : smap) (st : mstate) {struct fuel} : (xdenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFun h sms dc) =>
          match xp_fun P h with
          | Some (Dr, bs) =>
              match xcompose d sm with
              | Some d1 =>
                  match xrun_chain d1 sms with
                  | Some d2 =>
                      match xinterp_binds k P (xapply_env P d h th (d2 ++ dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match xcompose d3 (inst_smap th Dr) with
                          | Some dr => inl (xresult_env P d3 Dr dr, st3)
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end
(* Immutable application maps carry every type and provision explicitly. *)
with xinterp_apply_r (fuel : nat) (P : xprog) (d : xdenv) (u : usym)
    (inputs outputs : smap) (st : mstate) {struct fuel} : (xdenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel [] st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFun h sms dc) =>
          match xp_fun P h with
          | Some (_, bs) =>
              match xcompose d inputs with
              | Some d1 =>
                  match xrun_chain d1 sms with
                  | Some d2 =>
                      match xinterp_binds k P (d2 ++ dc) [] st bs with
                      | inl (d3, _, st3) =>
                          match xcompose d3 (xp_exports P h) with
                          | Some named =>
                              match xcompose named outputs with
                              | Some dr => inl (dr, st3)
                              | None => inr (XR (XErr e_compose) [] st3)
                              end
                          | None => inr (XR (XErr e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (XR (XErr e_chain) [] st)
                  end
              | None => inr (XR (XErr e_compose) [] st)
              end
          | None => inr (XR (XErr e_nobody) [] st)
          end
      | _ => inr (XR (XErr e_fun) [] st)
      end
  end
(* the bootstrap IR's `bind F;`: run the branch of the functor F is bound to,
   in a scope of its own, and keep exactly the result's entries *)
with xinterp_branch (fuel : nat) (P : xprog) (d : xdenv) (g : xvenv) (st : mstate)
    (u : usym) (brs : list (usym * list xbind * smap)) {struct fuel}
    : (xdenv * xvenv * mstate) + xres :=
  match fuel with
  | 0 => inr (XR XFuel g st)
  | S k =>
      match lookup_xdenv (IFun u) d with
      | Some (XDFunSym h) =>
          match find_branch h brs with
          | Some (bs, pairs) =>
              match xinterp_binds k P d g st bs with
              | inl (d3, g3, st3) =>
                  match xcompose d3 pairs with
                  | Some dr => inl (dr, g3, st3)
                  | None => inr (XR (XErr e_compose) g3 st3)
                  end
              | inr rr => inr rr
              end
          | None => inr (XR (XErr e_branch) g st)
          end
      | _ => inr (XR (XErr e_fun) g st)
      end
  end.

(* Running `main`: its body in the initial environment (the platform's
   provisions for main's needs) and the initial machine. *)
Definition run_main (fuel : nat) (P : xprog) (main : fsym) (d0 : xdenv)
    (st0 : mstate) : xres :=
  match xp_fn P main with
  | Some body => ret_call [] (xinterp fuel P d0 [] st0 body)
  | None => XR (XErr e_nobody) [] st0
  end.
End XSem.

(* ---- III.3 The core inside MossX (moved from MossX.v): the embedding of
   Core Moss terms, runtime entries and programs into MossX, and when a
   core result and a MossX outcome agree.  The obligation
   `xinterp_conservative` (Part VI) says MossX runs embedded core programs
   exactly as Core Moss does. ---- *)

Fixpoint emb_value (w : value) : xvalue :=
  match w with
  | VUnit => XVUnit
  | VTag n w1 => XVTag n (emb_value w1)
  end.

(* The embedding of the core: syntax, runtime entries, environments. *)
Fixpoint emb_expr (e : expr) : xexpr :=
  match e with
  | EVar x => XVar x
  | EUnit => XUnit
  | EVl v => XVl v
  | ETag n th e1 => XTag n th (emb_expr e1)
  | ECallD f th sm args => XCallD f th sm (map emb_expr args)
  | ECallA f args => XCallA f (map emb_expr args)
  | EMeth e0 m prov args => XMeth (emb_expr e0) m prov (map emb_expr args)
  | EMatch e0 arms =>
      XMatch (emb_expr e0)
             (map (fun a => match a with (n, x, eb) => (n, x, emb_expr eb) end) arms)
  | ELet x e1 e2 => XLet x (emb_expr e1) (emb_expr e2)
  | EBindTy t tau e1 => XBindTy t tau (emb_expr e1)
  | EBindVal v e1 e2 => XBindVal v (emb_expr e1) (emb_expr e2)
  | EBindFn f f' sm e1 => XBindFn f f' sm (emb_expr e1)
  | EBindMth tauk m th f' sm e1 => XBindMth tauk m th f' sm (emb_expr e1)
  | EBindFun u h sm smc e1 => XBindFun u h sm smc (emb_expr e1)
  | EBindFunA u u' sm e1 => XBindFunA u u' sm (emb_expr e1)
  | EApply u th sm e1 => XApply u th sm (emb_expr e1)
  end.

Definition emb_arms (arms : list (nsym * var * expr)) : list (nsym * var * xexpr) :=
  map (fun a => match a with (n, x, eb) => (n, x, emb_expr eb) end) arms.

Definition emb_bnd (b : bnd) : xbind :=
  match b with
  | BTy t tau => XBTy t tau
  | BVal v e1 => XBVal v (emb_expr e1)
  | BFn f f' sm => XBFn f f' sm
  | BMth tauk m th f' sm => XBMth tauk m th f' sm
  | BFun u h sm smc => XBFun u h sm smc
  | BFunA u u' sm => XBFunA u u' sm
  | BApply u th sm => XBApply u th sm
  end.

Fixpoint emb_dentry (en : dentry) : xdentry :=
  match en with
  | DVal w => XDVal (emb_value w)
  | DClo f d => XDClo f (map (fun p => match p with (i, en') => (i, emb_dentry en') end) d)
  | DFun h sms dc =>
      XDFun h sms (map (fun p => match p with (i, en') => (i, emb_dentry en') end) dc)
  end.

Definition emb_denv (d : denv) : xdenv :=
  map (fun p => match p with (i, en) => (i, emb_dentry en) end) d.
Definition emb_venv (g : venv) : xvenv :=
  map (fun p => match p with (x, w) => (x, emb_value w) end) g.

(* the core program as an extended one: the same bodies, embedded *)
Definition emb_prog (Sg : gsig) : xprog :=
  mkXProg (fun f => match g_fn Sg f with
                    | Some (_, _, Some b) => Some (emb_expr b)
                    | _ => None
                    end)
          (fun u => match g_fun Sg u with
                    | Some (_, Dr, Some bs) => Some (Dr, map emb_bnd bs)
                    | _ => None
                    end)
          0 0 0 [] (fun _ => None) [] (fun _ => []) (fun _ => []) false (fun _ => []) (fun _ => []).

Definition res_agree (r : res) (o : xout) : Prop :=
  match r, o with
  | Ok w, XOk w' => w' = emb_value w
  | Err, XErr _ => True
  | OutOfFuel, XFuel => True
  | _, _ => False
  end.


(* ######################################################################## *)
(* Part IV. The primitives' signatures                                       *)
(* ######################################################################## *)

(* ===================== 11. Primitives, typed by signature ================ *)

Inductive xty : Type := XTI32 | XTI64 | XTF32 | XTF64 | XTUnit | XTBool
(* Track B: an address as `Memories`' members receive it (an `Addr`, a tag
   over an I32), and any tag (the memory a member is a method of) *)
| XTAddr | XTTag.

Definition xvty (P : xprog) (w : xvalue) (t : xty) : Prop :=
  match t, w with
  | XTI32, XVI32 _ | XTI64, XVI64 _ | XTF32, XVF32 _ | XTF64, XVF64 _ => True
  | XTUnit, XVUnit => True
  | XTBool, XVTag b (XVTag t' XVUnit) => b = xp_bool P /\ (t' = xp_true P \/ t' = xp_false P)
  | XTAddr, XVTag _ (XVI32 _) => True
  | XTTag, XVTag _ _ => True
  | _, _ => False
  end.

(* The signatures of lib/wasm.moss, lib/wasip1.moss and wasistd's i32_bool
   (Moss's I32 / I64 / F32 / F64 / () / Bool).  The GC primitives and the
   unmodelled ones have none here: their results depend on the heap. *)
Definition prim_sig (p : prim) : option (list xty * xty) :=
  match p with
  | PI32Bin _ | PI32Cmp _ => Some ([XTI32; XTI32], XTI32)
  | PI32Un _ => Some ([XTI32], XTI32)
  | PF64Bin _ => Some ([XTF64; XTF64], XTF64)
  | PF64Cmp _ => Some ([XTF64; XTF64], XTI32)
  | PF64Un _ => Some ([XTF64], XTF64)
  | PConv (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([XTF64], XTI32)
  | PConv (CConvS | CConvU) => Some ([XTI32], XTF64)
  | PLoad _ => Some ([XTI32], XTI32)
  | PStore _ => Some ([XTI32; XTI32], XTUnit)
  | PF64Load => Some ([XTI32], XTF64)
  | PF64Store => Some ([XTI32; XTF64], XTUnit)
  | PMemSize => Some ([], XTI32)
  | PMemGrow => Some ([XTI32], XTI32)
  | PMemCopy | PMemFill => Some ([XTI32; XTI32; XTI32], XTUnit)
  | PUnreachable => Some ([], XTUnit)
  | PFdWrite => Some ([XTI32; XTI32; XTI32; XTI32], XTI32)
  | PProcExit => Some ([XTI32], XTUnit)
  | PArgsSizesGet | PArgsGet => Some ([XTI32; XTI32], XTI32)
  | PPathOpen => Some ([XTI32; XTI32; XTI32; XTI32; XTI32; XTI64; XTI64; XTI32; XTI32], XTI32)
  | PFdRead => Some ([XTI32; XTI32; XTI32; XTI32], XTI32)
  | PFdClose => Some ([XTI32], XTI32)
  | PI32Bool => Some ([XTI32], XTBool)
  | PF32Bin _ => Some ([XTF32; XTF32], XTF32)
  | PF32Cmp _ => Some ([XTF32; XTF32], XTI32)
  | PF32Un _ => Some ([XTF32], XTF32)
  | PConv32 (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([XTF32], XTI32)
  | PConv32 (CConvS | CConvU) => Some ([XTI32], XTF32)
  | PF32Load => Some ([XTI32], XTF32)
  | PF32Store => Some ([XTI32; XTF32], XTUnit)
  | PDemote => Some ([XTF64], XTF32)
  | PPromote => Some ([XTF32], XTF64)
  | PI32ReinterpF32 => Some ([XTF32], XTI32)
  | PF32ReinterpI32 => Some ([XTI32], XTF32)
  | PI64Bin _ => Some ([XTI64; XTI64], XTI64)
  | PI64Cmp _ => Some ([XTI64; XTI64], XTI32)
  | PI64Un U64Eqz => Some ([XTI64], XTI32)
  | PI64Un _ => Some ([XTI64], XTI64)
  | PWrap => Some ([XTI64], XTI32)
  | PExtendS | PExtendU => Some ([XTI32], XTI64)
  | PI64Load _ => Some ([XTI32], XTI64)
  | PI64Store _ => Some ([XTI32; XTI64], XTUnit)
  | PConv64 (C64Trunc f32 _ _) => Some ([if f32 then XTF32 else XTF64], XTI64)
  | PConv64 (C64Conv to32 _) => Some ([XTI64], if to32 then XTF32 else XTF64)
  | PI64ReinterpF64 => Some ([XTF64], XTI64)
  | PF64ReinterpI64 => Some ([XTI64], XTF64)
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy | PUnsupported _ => None
  end.

(* Track B.  The memory primitives as the platform provides them, as
   methods of `Memories` (D79 as amended, DRAFT NOTE 23): an address
   argument is an `Addr`, and memory.size and memory.grow receive the
   memory they are a method of.  MossX's primitives read the bare address
   (MossX.mem_args); `prim_mlaw` (Part VI) is the law at these
   signatures. *)
Definition prim_msig (p : prim) : option (list xty * xty) :=
  match p with
  | PLoad _ => Some ([XTAddr], XTI32)
  | PStore _ => Some ([XTAddr; XTI32], XTUnit)
  | PF64Load => Some ([XTAddr], XTF64)
  | PF64Store => Some ([XTAddr; XTF64], XTUnit)
  | PF32Load => Some ([XTAddr], XTF32)
  | PF32Store => Some ([XTAddr; XTF32], XTUnit)
  | PI64Load _ => Some ([XTAddr], XTI64)
  | PI64Store _ => Some ([XTAddr; XTI64], XTUnit)
  | PMemCopy => Some ([XTAddr; XTAddr; XTI32], XTUnit)
  | PMemFill => Some ([XTAddr; XTI32; XTI32], XTUnit)
  | PMemSize => Some ([XTTag], XTI32)
  | PMemGrow => Some ([XTTag; XTI32], XTI32)
  | _ => None
  end.

(* Which library fn is which primitive: the elaborator gives `main`'s need of
   a lib/wasm.moss or lib/wasip1.moss fn named n the primitive
   `prim_of_name n` (moved from Elab.v).  An auditor checks this table
   against lib/wasm.moss's and lib/wasip1.moss's names. *)
Section PrimNames.
Local Open Scope string_scope.
Definition strip_prefix (pre s : string) : option string :=
  if String.prefix pre s then Some (substring (String.length pre) (String.length s - String.length pre) s) else None.

Definition i32bin_of (s : string) : option i32bin :=
  match s with
  | "add" => Some IAdd | "sub" => Some ISub | "mul" => Some IMul | "div_s" => Some IDivS
  | "div_u" => Some IDivU | "rem_s" => Some IRemS | "rem_u" => Some IRemU | "and" => Some IAnd
  | "or" => Some IOr | "xor" => Some IXor | "shl" => Some IShl | "shr_s" => Some IShrS
  | "shr_u" => Some IShrU | "rotl" => Some IRotl | "rotr" => Some IRotr | _ => None
  end.
Definition i32cmp_of (s : string) : option i32cmp :=
  match s with
  | "eq" => Some CEq | "ne" => Some CNe | "lt_s" => Some CLtS | "lt_u" => Some CLtU
  | "gt_s" => Some CGtS | "gt_u" => Some CGtU | "le_s" => Some CLeS | "le_u" => Some CLeU
  | "ge_s" => Some CGeS | "ge_u" => Some CGeU | _ => None
  end.
Definition i32un_of (s : string) : option i32un :=
  match s with
  | "eqz" => Some UEqz | "clz" => Some UClz | "ctz" => Some UCtz | "popcnt" => Some UPopcnt
  | "extend8_s" => Some UExt8 | "extend16_s" => Some UExt16 | _ => None
  end.
Definition f64bin_of (s : string) : option f64bin :=
  match s with
  | "add" => Some FAdd | "sub" => Some FSub | "mul" => Some FMul | "div" => Some FDiv
  | "min" => Some FMin | "max" => Some FMax | "copysign" => Some FCopysign | _ => None
  end.
Definition f64un_of (s : string) : option f64un :=
  match s with
  | "abs" => Some FAbs | "neg" => Some FNeg | "ceil" => Some FCeil | "floor" => Some FFloor
  | "trunc" => Some FTrunc | "nearest" => Some FNearest | "sqrt" => Some FSqrt | _ => None
  end.
Definition f64cmp_of (s : string) : option f64cmp :=
  match s with
  | "eq" => Some FEq | "ne" => Some FNe | "lt" => Some FLt | "gt" => Some FGt
  | "le" => Some FLe | "ge" => Some FGe | _ => None
  end.

Definition prim_other (s : string) : option prim :=
  match s with
  | "i32_load" => Some (PLoad L32) | "i32_load8_s" => Some (PLoad L8S) | "i32_load8_u" => Some (PLoad L8U)
  | "i32_load16_s" => Some (PLoad L16S) | "i32_load16_u" => Some (PLoad L16U)
  | "i32_store" => Some (PStore S32) | "i32_store8" => Some (PStore S8) | "i32_store16" => Some (PStore S16)
  | "f64_load" => Some PF64Load | "f64_store" => Some PF64Store
  | "memory_size" => Some PMemSize | "memory_grow" => Some PMemGrow
  | "memory_copy" => Some PMemCopy | "memory_fill" => Some PMemFill
  | "unreachable" => Some PUnreachable | "fd_write" => Some PFdWrite | "proc_exit" => Some PProcExit
  | "args_sizes_get" => Some PArgsSizesGet | "args_get" => Some PArgsGet
  | "path_open" => Some PPathOpen | "fd_read" => Some PFdRead | "fd_close" => Some PFdClose
  | "i32_bool" => Some PI32Bool
  | "i32_trunc_f64_s" => Some (PConv CTruncS) | "i32_trunc_f64_u" => Some (PConv CTruncU)
  | "i32_trunc_sat_f64_s" => Some (PConv CTruncSatS) | "i32_trunc_sat_f64_u" => Some (PConv CTruncSatU)
  | "f64_convert_i32_s" => Some (PConv CConvS) | "f64_convert_i32_u" => Some (PConv CConvU)
  | "i32_trunc_f32_s" => Some (PConv32 CTruncS) | "i32_trunc_f32_u" => Some (PConv32 CTruncU)
  | "i32_trunc_sat_f32_s" => Some (PConv32 CTruncSatS) | "i32_trunc_sat_f32_u" => Some (PConv32 CTruncSatU)
  | "f32_convert_i32_s" => Some (PConv32 CConvS) | "f32_convert_i32_u" => Some (PConv32 CConvU)
  | "f32_load" => Some PF32Load | "f32_store" => Some PF32Store | "f32_demote_f64" => Some PDemote
  | "f64_promote_f32" => Some PPromote | "i32_reinterpret_f32" => Some PI32ReinterpF32
  | "f32_reinterpret_i32" => Some PF32ReinterpI32
  | "i64_eqz" => Some (PI64Un U64Eqz) | "i64_clz" => Some (PI64Un U64Clz) | "i64_ctz" => Some (PI64Un U64Ctz)
  | "i64_popcnt" => Some (PI64Un U64Popcnt) | "i64_extend8_s" => Some (PI64Un U64Ext8)
  | "i64_extend16_s" => Some (PI64Un U64Ext16) | "i64_extend32_s" => Some (PI64Un U64Ext32)
  | "i32_wrap_i64" => Some PWrap | "i64_extend_i32_s" => Some PExtendS | "i64_extend_i32_u" => Some PExtendU
  | "i64_load" => Some (PI64Load L64) | "i64_load8_s" => Some (PI64Load L64_8S) | "i64_load8_u" => Some (PI64Load L64_8U)
  | "i64_load16_s" => Some (PI64Load L64_16S) | "i64_load16_u" => Some (PI64Load L64_16U)
  | "i64_load32_s" => Some (PI64Load L64_32S) | "i64_load32_u" => Some (PI64Load L64_32U)
  | "i64_store" => Some (PI64Store S64) | "i64_store8" => Some (PI64Store S64_8)
  | "i64_store16" => Some (PI64Store S64_16) | "i64_store32" => Some (PI64Store S64_32)
  | "i64_reinterpret_f64" => Some PI64ReinterpF64 | "f64_reinterpret_i64" => Some PF64ReinterpI64
  | "i64_trunc_f64_s" => Some (PConv64 (C64Trunc false true false))
  | "i64_trunc_f64_u" => Some (PConv64 (C64Trunc false false false))
  | "i64_trunc_f32_s" => Some (PConv64 (C64Trunc true true false))
  | "i64_trunc_f32_u" => Some (PConv64 (C64Trunc true false false))
  | "i64_trunc_sat_f64_s" => Some (PConv64 (C64Trunc false true true))
  | "i64_trunc_sat_f64_u" => Some (PConv64 (C64Trunc false false true))
  | "i64_trunc_sat_f32_s" => Some (PConv64 (C64Trunc true true true))
  | "i64_trunc_sat_f32_u" => Some (PConv64 (C64Trunc true false true))
  | "f64_convert_i64_s" => Some (PConv64 (C64Conv false true))
  | "f64_convert_i64_u" => Some (PConv64 (C64Conv false false))
  | "f32_convert_i64_s" => Some (PConv64 (C64Conv true true))
  | "f32_convert_i64_u" => Some (PConv64 (C64Conv true false))
  | _ => None
  end.

Definition prim_of_name (s : string) : option prim :=
  match prim_other s with
  | Some p => Some p
  | None =>
      let try_ {A} (pre : string) (f : string -> option A) (k : A -> prim) : option prim :=
        match strip_prefix pre s with
        | Some r => option_map k (f r)
        | None => None
        end in
      let first (l : list (option prim)) := fold_right (fun o acc => match o with Some _ => o | None => acc end) None l in
      first [try_ "i32_" i32bin_of PI32Bin; try_ "i32_" i32cmp_of PI32Cmp; try_ "i32_" i32un_of PI32Un;
             try_ "f64_" f64bin_of PF64Bin; try_ "f64_" f64cmp_of PF64Cmp; try_ "f64_" f64un_of PF64Un;
             try_ "f32_" f64bin_of PF32Bin; try_ "f32_" f64cmp_of PF32Cmp; try_ "f32_" f64un_of PF32Un;
             try_ "i64_" i32bin_of PI64Bin; try_ "i64_" i32cmp_of PI64Cmp]
  end.

End PrimNames.

(* ######################################################################## *)
(* Part V. The declarative elaboration relation — DRAFT                      *)
(* ######################################################################## *)

(* STATUS: DRAFT, for the designer's review.  The elaborator (Elab.v) is
   neither shown sound nor complete for it; Certified.v lists both as
   pending.  RelExamples.v checks it on examples.  Proved toward soundness
   (WorldSound.v, bound as Certified.world_reads): the loader's reading of
   an accepted bundle meets every condition of WorldOf (V.1) except the
   requirement lists and the field numbering.

   WHAT IT SAYS.  `Elaborates b p` relates a bundle `b` (Part I) to an
   elaborated program `p` (Part II): `p` is *an* elaboration of `b` if
   there is a static reading `W` of the bundle (which declaration each
   symbol number is, what each name in each module denotes, each symbol's
   requirements) satisfying the conditions of V.1, and every defined
   function's body in `p` is related to its source by the typed judgment
   of V.4–V.6 under the context its assumes form (V.3).

   WHAT MAKES IT DECLARATIVE.  It fixes no algorithm, no traversal order,
   no fuel, and no numbering.  Symbol numbers, local-variable numbers,
   record-field numbers, fresh generations and synthesized symbols are all
   existentially chosen (subject only to being distinct where that is
   observable).  Name resolution is stated as "this name denotes that
   symbol iff it is declared or imported so" (D44 then follows: if two
   symbols qualify there is no reading `W`).  Merging (D43) is a most
   general unifier, not a union-find run.  Requirement and need lists are
   sets.  Which provision a call uses (D61/D62/D77) is a case analysis on
   what is declared and what the context provides, and the context holds at
   most one provision per key, so there is nothing to search (D1).

   HOW TO READ IT.  V.1 the static world; V.2 types; V.3 context formation;
   V.4 calls, needs and coherence; V.5 functors (D84); V.6 expressions,
   statements, binds; V.7 the program.  Each rule cites the decision it
   encodes.  The list "DRAFT NOTES" at the end records every place where
   the draft departs from the elaborator, and the spec gaps it surfaced.
*)

Module Elaboration.
Import Input.
Local Open Scope string_scope.
Local Open Scope list_scope.

(* ======================================================================== *)
(* V.0 Small helpers: association lists, as the context's tables            *)
(* ======================================================================== *)

Fixpoint nlook {A : Type} (k : nat) (l : list (nat * A)) : option A :=
  match l with [] => None | (k', a) :: r => if Nat.eqb k k' then Some a else nlook k r end.

Fixpoint slook {A : Type} (k : string) (l : list (string * A)) : option A :=
  match l with [] => None | (k', a) :: r => if String.eqb k k' then Some a else slook k r end.

Fixpoint ilook {A : Type} (k : item) (l : list (item * A)) : option A :=
  match l with [] => None | (k', a) :: r => if item_eqb k k' then Some a else ilook k r end.

(* An instantiation: for each type symbol, what it meant (None: not in force). *)
Definition inst := list (nat * option ty).

(* A signature as elaboration reads it: parameter types and result. *)
Record fsig := FSig { sg_params : list ty; sg_ret : ty }.

Definition sig_subst (s : subst) (g : fsig) : fsig :=
  FSig (map (app_subst s) (sg_params g)) (app_subst s (sg_ret g)).

(* The generations (abstract type symbols) a type mentions. *)
Fixpoint gens (t : ty) : list nat :=
  match t with
  | TUnit => []
  | TAbs g => [g]
  | TNom _ xs => (fix go (l : list (tsym * ty)) := match l with [] => [] | p :: r => gens (snd p) ++ go r end) xs
  | TUnion ms => (fix go (l : list ty) := match l with [] => [] | m :: r => gens m ++ go r end) ms
  end.

(* Records have no declaration: their elaborated type is a reserved head 0
   (never a symbol) applied to their fields, by interned field number. *)
Definition rec_head : nat := 0.

Definition head (t : ty) : option nat :=
  match t with TNom n _ => Some n | TAbs g => Some g | _ => None end.

Definition members (t : ty) : list ty :=
  match t with TUnion ms => ms | _ => [t] end.

Definition never : ty := TUnion [].

(* D17: a value of type t is accepted where `expected` is wanted: equal,
   divergent, or injected into a union that has all its members. *)
Definition fits (t expected : ty) : Prop :=
  t = expected \/ t = never \/
  (exists ms, expected = TUnion ms /\ forall m, In m (members t) -> In m ms).

Definition opt_fits (t : ty) (exp : option ty) : Prop :=
  match exp with Some e => fits t e | None => True end.

(* ======================================================================== *)
(* V.1 The static world: what each symbol is, what each name denotes        *)
(* ======================================================================== *)

(* The modules of a bundle, and the imports of one (the prelude first, when
   the module wants it, as an unqualified glob import). *)
Definition module_of (b : bundle) (p : string) : option amodule :=
  find (fun m => String.eqb (am_path m) p) (b_mods b).

Definition imports_of (b : bundle) (m : amodule) : list (string * aimport) :=
  (if am_prelude m then [(b_prelude b, AImport (b_prelude b) None true [])] else [])
  ++ combine (am_targets m) (f_imports (am_file m)).

Inductive Loaded (b : bundle) : string -> Prop :=
| LoadEntry : Loaded b (b_entry b)
| LoadImport : forall p m t imp,
    Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) -> Loaded b t.

(* A declaration together with where it is: its module and the assume
   items enclosing it, outermost first ([D20]: requirements nest by union). *)
Record decl := Decl { d_mod : string; d_ast : adecl; d_assumes : list (path * option name) }.

Inductive OccursIn : list adecl -> list (path * option name) -> adecl -> Prop :=
| OccHere : forall ds d,
    In d ds -> (forall its inner, d <> ADAssume its inner) -> OccursIn ds [] d
| OccNested : forall ds its inner asm d,
    In (ADAssume its inner) ds -> OccursIn inner asm d -> OccursIn ds (its ++ asm) d.

Definition Declared (b : bundle) (d : decl) : Prop :=
  Loaded b (d_mod d) /\
  exists m, module_of b (d_mod d) = Some m /\ OccursIn (f_decls (am_file m)) (d_assumes d) (d_ast d).

Inductive dkind := DKType | DKUnit | DKAlias | DKTag | DKVal | DKFn | DKCtx | DKMeth | DKFunctor.

Definition kind_of (d : adecl) : option dkind :=
  match d with
  | ADType _ => Some DKType | ADUnit _ => Some DKUnit | ADAlias _ _ => Some DKAlias
  | ADTag _ _ _ => Some DKTag | ADVal _ _ _ => Some DKVal | ADCtx _ _ => Some DKCtx
  | ADFunctor _ _ _ _ _ => Some DKFunctor
  | ADFn RPlain _ _ _ _ => Some DKFn
  | ADFn _ _ _ _ _ => Some DKMeth
  | ADAssume _ _ => None
  end.

(* The name a declaration is filed under: plain names (types, units, tags,
   aliases, vals, fns, contexts, functors) and detached methods `.m`; an
   attached method `R.m` is filed under its receiver instead ([D61]). *)
Inductive nspace := NSName | NSDetached | NSAlias.

Definition filed_name (d : adecl) : option (nspace * name) :=
  match d with
  | ADFn RDetached n _ _ _ => Some (NSDetached, n)
  | ADFn (RAttached _) _ _ _ _ => None
  | ADAssume _ _ => None
  | _ => match d with
         | ADType n | ADUnit n | ADAlias n _ | ADTag n _ _ | ADVal n _ _ | ADCtx n _
         | ADFunctor n _ _ _ _ | ADFn _ n _ _ _ => Some (NSName, n)
         | _ => None
         end
  end.

(* What a name in a module's scope denotes: a symbol, or (an alias) a module. *)
Inductive ent := ESym (s : nat) | EMod (p : string).

(* The static world: a reading of the bundle.  It is a witness: the
   relation asks only that one exist satisfying `WorldOf`. *)
Record world := World {
  w_decl  : nat -> option decl;                      (* symbol -> declaration *)
  w_scope : string -> nspace -> name -> option ent;  (* module, namespace, name -> denotation *)
  w_exp   : string -> nspace -> name -> option ent;  (* what a module exports *)
  w_attached : nat -> name -> option nat;            (* receiver symbol, name -> attached method *)
  w_reqs  : nat -> list nat;                         (* a symbol's requirements (type symbols) *)
  w_fld   : string -> nat;                           (* record field numbering *)
  w_library : option string                         (* prelude's directory prefix *)
}.

Definition kind_sym (W : world) (s : nat) : option dkind :=
  match w_decl W s with Some d => kind_of (d_ast d) | None => None end.

Definition ast_of (W : world) (s : nat) : option adecl := option_map d_ast (w_decl W s).
Definition mod_of (W : world) (s : nat) : string :=
  match w_decl W s with Some d => d_mod d | None => "" end.

(* [D44]/[D61]/[D64]: what a module exports, and what its scope holds.
   A module exports its own declarations and what it imported by `use`
   (with the use's local name), not what a glob import brought in.  Its
   scope holds what it exports, everything its glob imports export, and
   the module aliases of its imports. *)
Inductive Exports (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ExOwn : forall p s d ns n,
    w_decl W s = Some d -> d_mod d = p -> filed_name (d_ast d) = Some (ns, n) ->
    Exports b W p ns n (ESym s)
| ExUse : forall p m t imp u e ns,
    module_of b p = Some m -> In (t, imp) (imports_of b m) -> i_glob imp = false ->
    In u (i_uses imp) ->
    ns = (if u_dot u then NSDetached else NSName) ->
    (* a plain name is the symbol t exports under it, else the module alias
       (as path resolution reads it): note 25 *)
    (Exports b W t ns (u_name u) e \/
     (u_dot u = false /\ w_exp W t NSName (u_name u) = None /\ Exports b W t NSAlias (u_name u) e)) ->
    Exports b W p (match e with EMod _ => NSAlias | _ => ns end)
            (match u_alias u with Some (_, a) => a | None => u_name u end) e
| ExAlias : forall p m t imp a,
    module_of b p = Some m -> In (t, imp) (imports_of b m) -> i_alias imp = Some a ->
    Exports b W p NSAlias a (EMod t).

Inductive InScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScExported : forall p ns n e, Exports b W p ns n e -> InScope b W p ns n e
| ScGlob : forall p m t imp ns n e,
    module_of b p = Some m -> In (t, imp) (imports_of b m) -> i_glob imp = true ->
    Exports b W t ns n e -> InScope b W p ns n e.

(* Path resolution: the first segment in scope, the rest through exports. *)
Fixpoint resolve_rest (W : world) (e : ent) (segs : list name) : option ent :=
  match segs with
  | [] => Some e
  | seg :: r =>
      match e with
      | ESym _ => None
      | EMod t =>
          match w_exp W t NSName seg with
          | Some e' => resolve_rest W e' r
          | None => match w_exp W t NSAlias seg with Some e' => resolve_rest W e' r | None => None end
          end
      end
  end.

Definition resolve (W : world) (p : string) (pa : path) : option ent :=
  match pa with
  | [] => None
  | hd :: rest =>
      match w_scope W p NSName hd with
      | Some e => resolve_rest W e rest
      | None => match w_scope W p NSAlias hd with Some e => resolve_rest W e rest | None => None end
      end
  end.

Definition resolve_sym (W : world) (p : string) (pa : path) : option nat :=
  match resolve W p pa with Some (ESym s) => Some s | _ => None end.

(* A detached method name: `.m` in scope, or `mod::m` ([D61]). *)
Definition resolve_detached (W : world) (p : string) (pa : path) : option nat :=
  match rev pa with
  | [] => None
  | [n] => match w_scope W p NSDetached n with Some (ESym s) => Some s | _ => None end
  | n :: rpre => match resolve W p (rev rpre) with
                 | Some (EMod t) => match w_exp W t NSDetached n with Some (ESym s) => Some s | _ => None end
                 | _ => None
                 end
  end.

(* [D61]: an attached method `R.m` is declared in R's own file. *)
Definition AttachedDecl (W : world) (r : nat) (n : name) (s : nat) : Prop :=
  exists d rn ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn (RAttached rn) n ps rt body /\
    d_mod d = mod_of W r /\ w_scope W (d_mod d) NSName rn = Some (ESym r).

(* The attached method of receiver r named n, as the world files it. *)
Definition Attached (W : world) (r : nat) (n : name) (s : nat) : Prop := w_attached W r n = Some s.

(* [D20]/[D21]: the type symbols an assume nesting requires, through the
   contexts it names.  For a val, fn, method, context or functor these are
   its requirements. *)
Inductive SpecTypes (W : world) : string -> spec -> nat -> Prop :=
| STType : forall p pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> SpecTypes W p (Spec (Some pa) None a) t
| STCtx : forall p pa a c d n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    In sp its -> SpecTypes W (d_mod d) sp t -> SpecTypes W p (Spec (Some pa) None a) t.

Definition AssumeTypes (W : world) (s : nat) (t : nat) : Prop :=
  exists d pa, w_decl W s = Some d /\ In (pa, None) (d_assumes d) /\
               SpecTypes W (d_mod d) (Spec (Some pa) None None) t.

(* [D71]/[D73]: a nominal's requirements are its *relevant* parameters: the
   ones it lists, and the type symbols its payload mentions, closed through
   the nominals it mentions (less what brackets bind there).  `FreeIn R`
   reads a type expression's free type symbols against a candidate R. *)
Inductive FreeIn (W : world) (R : nat -> list nat) (p : string) : aty -> nat -> Prop :=
| FRec : forall fs n t x, In (n, t) fs -> FreeIn W R p t x -> FreeIn W R p (ATRec fs) x
| FUnion : forall ms t x, In t ms -> FreeIn W R p t x -> FreeIn W R p (ATUnion ms) x
| FType : forall pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    FreeIn W R p (ATRef pa a) t
| FNom : forall pa a g x,
    resolve_sym W p pa = Some g -> kind_sym W g <> Some DKType -> In x (R g) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some x) ->
    FreeIn W R p (ATRef pa a) x
| FBracket : forall pa a bpath sp d sa x,
    (* the head resolves to a symbol (note 31) *)
    resolve_sym W p pa <> None ->
    In (bpath, Spec (Some sp) d sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W R p (ATRef sp sa) x -> FreeIn W R p (ATRef pa a) x.

Definition nominal_body (d : adecl) : option (list name * aty) :=
  match d with ADTag _ ps t => Some (ps, t) | ADAlias _ t => Some ([], t) | _ => None end.

Definition RelevanceClosed (W : world) (R : nat -> list nat) : Prop :=
  forall g d ps t, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) ->
    (forall n x, In n ps -> resolve_sym W (d_mod d) [n] = Some x -> In x (R g)) /\
    (forall x, FreeIn W R (d_mod d) t x -> In x (R g)).

(* The conditions on a reading W of b. *)
Record WorldOf (b : bundle) (W : world) : Prop := {
  (* imports are acyclic and every module imported is in the bundle *)
  wo_acyclic : exists rank : string -> nat, forall p m t imp,
      Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) ->
      module_of b t <> None /\ rank t < rank p;
  (* every symbol is a declaration of the bundle, and every declaration is
     exactly one symbol (0 is never a symbol: records use it, V.0) *)
  wo_decl_sound : forall s d, w_decl W s = Some d -> s <> 0 /\ Declared b d /\ kind_of (d_ast d) <> None;
  wo_decl_complete : forall d, Declared b d -> kind_of (d_ast d) <> None ->
      exists s, w_decl W s = Some d /\ forall s', w_decl W s' = Some d -> s' = s;
  (* [D44]: scopes and exports are exactly what declarations and imports
     put there; since w_scope is a function, two symbols under one name in
     one scope make the program have no reading at all *)
  wo_exports : forall p ns n e, Loaded b p -> (w_exp W p ns n = Some e <-> Exports b W p ns n e);
  wo_scope : forall p ns n e, Loaded b p -> (w_scope W p ns n = Some e <-> InScope b W p ns n e);
  (* an attached method is filed under its receiver, a type of its own
     file, and the table holds exactly the declared ones ([D61]) *)
  wo_attached : forall r n s, w_attached W r n = Some s <-> AttachedDecl W r n s;
  (* requirements: of a val/fn/method/context/functor, its assume nesting's
     type symbols; of a type or unit, none; of a tag or alias, the least
     relevance-closed sets ([D71]), every listed name a type of its nesting
     ([D73]).  Lists have no repeats; their order is free, but fixed per
     symbol, which fixes the order of a nominal's arguments in `TNom`. *)
  wo_reqs_nodup : forall s, NoDup (w_reqs W s);
  wo_reqs_plain : forall s k, kind_sym W s = Some k ->
      k <> DKTag -> k <> DKAlias -> k <> DKType -> k <> DKUnit ->
      forall t, In t (w_reqs W s) <-> AssumeTypes W s t;
  wo_reqs_none : forall s, kind_sym W s = Some DKType \/ kind_sym W s = Some DKUnit -> w_reqs W s = [];
  wo_reqs_closed : RelevanceClosed W (w_reqs W);
  wo_reqs_least : forall R, RelevanceClosed W R ->
      (* R agrees with the requirements fixed above, of everything but a
         tag or alias (note 31) *)
      (forall s, kind_sym W s <> Some DKTag -> kind_sym W s <> Some DKAlias -> R s = w_reqs W s) ->
      forall g, (kind_sym W g = Some DKTag \/ kind_sym W g = Some DKAlias) -> incl (w_reqs W g) (R g);
  wo_listed : forall g d ps t n, w_decl W g = Some d -> nominal_body (d_ast d) = Some (ps, t) -> In n ps ->
      exists x, resolve_sym W (d_mod d) [n] = Some x /\ AssumeTypes W g x;
  (* record fields: distinct names get distinct numbers *)
  wo_fld : forall a c, w_fld W a = w_fld W c -> a = c;
  wo_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* Library symbols the language itself refers to ([D45], [D68]): the
   symbol in scope as `n` in `<prelude directory>/<short>.moss`.
   Reexports are ordinary scope entries, including here (note 68). *)
Definition ends_with (suf s : string) : Prop := exists pre, s = String.append pre suf.

Definition LibraryModule (W : world) (short p : string) : Prop :=
  match w_library W with
  | Some dir => p = String.append dir (String.append short ".moss")
  | None => False
  end.

Definition LangSym (W : world) (short : string) (ns : nspace) (n : name) (s : nat) : Prop :=
  exists d p, w_decl W s = Some d /\ LibraryModule W short p /\
              w_scope W p ns n = Some (ESym s).

(* [D75]/m4: the platform's reference tags, `Ref` and `Array` of
   lib/wasm.moss, which only the library builds from a bare handle (note
   32). *)
Definition PlatformRefTag (W : world) (s : nat) : Prop :=
  LangSym W "wasm" NSName "Ref" s \/ LangSym W "wasm" NSName "Array" s.

(* Module p is in the selected library's directory or a subdirectory. *)
Definition InLibrary (W : world) (p : string) : Prop :=
  exists dir rest, w_library W = Some dir /\ p = String.append dir rest.

(* ======================================================================== *)
(* V.2 Types                                                                 *)
(* ======================================================================== *)

(* `TyOf W p tm this t τ`: the type expression t, written in module p,
   denotes τ, where `tm` gives each type symbol in force its meaning here
   ([D72]: a type symbol's meaning is read where the type is written;
   rebinding it later does not change τ) and `this` the receiver type
   inside a method declaration.  A nominal reference is total: each of its
   requirements is bound in brackets or in force ([D22] as clarified). *)
Inductive TyOf (W : world) : string -> list (nat * ty) -> option ty -> aty -> ty -> Prop :=
| TyNever : forall p tm th, TyOf W p tm th ATNever never
| TyUnit  : forall p tm th, TyOf W p tm th ATUnit TUnit
| TyThis  : forall p tm t, TyOf W p tm (Some t) ATThis t
| TyRec   : forall p tm th fs ts,
    Forall2 (fun nt ft => fst ft = w_fld W (fst nt) /\ TyOf W p tm th (snd nt) (snd ft)) fs ts ->
    TyOf W p tm th (ATRec fs) (TNom rec_head ts)
| TyUnion : forall p tm th ms ts flat,
    Forall2 (TyOf W p tm th) ms ts ->
    flat = flat_map members ts ->
    (* [D15]: members are nominal (a record's reserved head is not: note
       34), and no two share a head (note 33), so each appears once *)
    Forall (fun m => exists n xs, m = TNom n xs /\ n <> rec_head) flat -> NoDup (map head flat) ->
    TyOf W p tm th (ATUnion ms) (TUnion flat)
| TyType  : forall p tm th pa t tau,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> nlook t tm = Some tau ->
    TyOf W p tm th (ATRef pa None) tau
| TyNom   : forall p tm th pa a g k bs args,
    resolve_sym W p pa = Some g -> kind_sym W g = Some k -> (k = DKTag \/ k = DKUnit) ->
    Brackets W p tm g a bs ->
    (* the arguments: each requirement, in its fixed order, as bound or in force *)
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ tm) = Some (snd ra)) (w_reqs W g) args ->
    TyOf W p tm th (ATRef pa a) (TNom g args)
| TyAlias : forall p tm th pa a g d n body bs tau,
    resolve_sym W p pa = Some g -> w_decl W g = Some d -> d_ast d = ADAlias n body ->
    Brackets W p tm g a bs ->
    TyOf W (d_mod d) (bs ++ tm) None body tau ->
    TyOf W p tm th (ATRef pa a) tau
(* `Brackets W p tm g a bs`: the bracket application `a` on g binds each
   left side (a type symbol among g's requirements, [D71]; on a method,
   any type symbol: note 56) to its right side read here; later bindings
   override earlier ones. *)
with Brackets (W : world) : string -> list (nat * ty) -> nat -> option (list (path * spec)) -> list (nat * ty) -> Prop :=
| BrNone : forall p tm g, Brackets W p tm g None []
| BrSome : forall p tm g bs l,
    Forall2 (fun b tb =>
               exists sp sa, snd b = Spec (Some sp) None sa /\
                 resolve_sym W p (fst b) = Some (fst tb) /\ kind_sym W (fst tb) = Some DKType /\
                 (kind_sym W g <> Some DKMeth -> In (fst tb) (w_reqs W g)) /\
                 TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    Brackets W p tm g (Some bs) (rev l).

(* ======================================================================== *)
(* V.3 Contexts in force, and their formation with merging                  *)
(* ======================================================================== *)

(* A context in force at a point of a body.  Every table is keyed by
   identity ([D62]): a val, fn or functor by its symbol, a method by its
   receiver's canonical *type* and its symbol; a lookup takes the latest
   entry.  `c_sub` is the merging substitution ([D43]) on generations;
   types in the other tables are canonical (c_sub applied).  `c_apps`
   records, for every provision, the instantiation it was made at ([D62],
   [D71] coherence).  `c_funs` records, for each abstract functor in
   force, what its fixed inputs X(F) denoted where it was provided ([D84]
   rules 2 and 5). *)
Inductive lmut := LLet | LVar | LCaptured.

Record cx := Cx {
  c_ty     : list (nat * ty);
  c_sub    : subst;
  c_vals   : list (nat * ty);
  c_fns    : list (nat * fsig);
  c_mths   : list ((ty * nat) * fsig);
  c_funs   : list (nat * list (nat * ty));
  c_apps   : list (item * inst);
  c_locals : list (name * (ty * lmut))
}.

Fixpoint mlook (r : ty) (m : nat) (l : list ((ty * nat) * fsig)) : option fsig :=
  match l with
  | [] => None
  | ((r', m'), g) :: rest => if ty_eqb r r' && Nat.eqb m m' then Some g else mlook r m rest
  end.

Definition canon (C : cx) (t : ty) : ty := app_subst (c_sub C) t.

(* The item a provision is filed under, and its runtime key ([D62]): the
   elaborated items carry θ = [] at this milestone (DRAFT NOTE 3). *)
Definition mkey (r : ty) (m : nat) : item := IMth r m [].

(* Note 28: a method provision is filed (c_mths, c_apps) under its
   receiver and method, mkey; its run-time key carries θ, the
   instantiation it was made at (its known entries, canonical). *)
Definition known (a : inst) : subst :=
  flat_map (fun q => match snd q with Some t => [(fst q, t)] | None => [] end) a.
Definition theta_of (C : cx) (a : inst) : subst := map (fun q => (fst q, canon C (snd q))) (known a).
Definition rkey (C : cx) (r : ty) (m : nat) : item :=
  IMth r m (match ilook (mkey r m) (c_apps C) with Some a => theta_of C a | None => [] end).
Definition rkey_of (C : cx) (k : item) : item :=
  match k with IMth r m _ => rkey C r m | _ => k end.

(* A provision has one identity; method type arguments are read separately. *)
Definition provision_id (it : item) : item :=
  match it with IMth r m _ => IMth r m [] | _ => it end.

Definition provides (C : cx) (i : item) : Prop :=
  match i with
  | IVal v => nlook v (c_vals C) <> None
  | IFn f => nlook f (c_fns C) <> None
  | IMth r m _ => mlook r m (c_mths C) <> None
  | IFun u => nlook u (c_funs C) <> None
  | ITy t => nlook t (c_ty C) <> None
  end.

(* A raw context entry, as an item of a telescope introduces it. *)
Inductive entry :=
| NTy  (t : nat)
| NVal (v : nat) (t : ty) (a : inst)
| NFn  (f : nat) (g : fsig) (a : inst)
| NMth (r : ty) (m : nat) (g : fsig) (a : inst)
| NFun (u : nat) (fx : subst).

(* `SigOf W s tm this g`: the declared signature of fn/method s read with
   the type meanings tm (and receiver type `this`). *)
Definition SigOf (W : world) (s : nat) (tm : list (nat * ty)) (this : option ty) (g : fsig) : Prop :=
  exists d rcv n ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    Forall2 (fun pt t => TyOf W (d_mod d) tm this (snd pt) t) ps (sg_params g) /\
    match rt with Some r => TyOf W (d_mod d) tm this r (sg_ret g) | None => sg_ret g = TUnit end.

(* A requirement t of fn or method s that its signature (a parameter's or
   the result's type) mentions (note 51). *)
Definition SigMentions (W : world) (s : nat) (t : nat) : Prop :=
  exists d rcv n ps rt body, w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    ((exists pt, In pt ps /\ FreeIn W (w_reqs W) (d_mod d) (snd pt) t) \/
     (exists r, rt = Some r /\ FreeIn W (w_reqs W) (d_mod d) r t)).

(* The instantiation a provision of s is made at: each requirement's meaning
   (for a method, exactly the requirements its signature mentions, in
   their fixed order: note 51). *)
Definition AppOf (W : world) (s : nat) (tm : list (nat * ty)) (a : inst) : Prop :=
  exists rs, incl rs (w_reqs W s) /\ a = map (fun r => (r, nlook r tm)) rs /\
    (kind_sym W s <> Some DKMeth -> rs = w_reqs W s) /\
    (kind_sym W s = Some DKMeth -> exists ms, rs = map snd (filter fst (combine ms (w_reqs W s))) /\
       Forall2 (fun m r => m = true <-> SigMentions W s r) ms (w_reqs W s)).

(* The receiver type of a method item `R.m`: R's meaning if R is a type
   symbol, else R as a nominal at its requirements' meanings. *)
Definition RecvTy (W : world) (r : nat) (tm : list (nat * ty)) (t : ty) : Prop :=
  match nlook r tm with
  | Some t' => t = t'
  | None =>
      match w_decl W r with
      | Some d =>
          match d_ast d with
          | ADAlias _ body => TyOf W (d_mod d) tm None body t
          | _ => exists args, t = TNom r args /\
                   Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
          end
      | None => exists args, t = TNom r args /\
                  Forall2 (fun x xa => fst xa = x /\ nlook x tm = Some (snd xa)) (w_reqs W r) args
      end
  end.

(* The method a name `.n` on receiver symbol r means in module p's items:
   the attached one, else the detached one in scope ([D61]). *)
Definition MethodNamed (W : world) (p : string) (r : nat) (n : name) (m : nat) : Prop :=
  Attached W r n m \/
  ((forall m', ~ Attached W r n m') /\ w_scope W p NSDetached n = Some (ESym m)).

(* The fixed inputs of a functor ([D84] rule 1), here because assuming an
   abstract functor asks them in force (ItemEntries, note 70). *)

Definition functor_parts (d : adecl) : option (option (list name) * list spec * list spec * option (list (list (spec * option aexpr)))) :=
  match d with ADFunctor _ ps a r b => Some (ps, a, r, b) | _ => None end.

(* The type outputs of F: the type symbols its result lists (through contexts). *)
Definition FunOutputs (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ SpecTypes W (d_mod d) sp t.

(* M(F) ([D84] rule 1, with DECISIONS.md's correction): the type symbols the
   result items mention — a val's or fn's requirements (its dependencies,
   for a fn its whole telescope), a method's receiver and the requirements
   its signature mentions, less what brackets bind, plus the brackets'
   right sides; closed through relevance (w_reqs); less F's outputs. *)
Inductive ResultMentions (W : world) : nat -> string -> spec -> nat -> Prop :=
| RMValFn : forall self p pa a s t,
    resolve_sym W p pa = Some s -> (kind_sym W s = Some DKVal \/ kind_sym W s = Some DKFn) ->
    In t (w_reqs W s) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMReceiver : forall self p pa dn a r t,
    resolve_sym W p pa = Some r ->
    (t = r /\ kind_sym W r = Some DKType \/
     kind_sym W r <> Some DKType /\ In t (w_reqs W r)) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMMeth : forall self p pa dn a r m t,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    In t (w_reqs W m) -> SigMentions W m t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMBracket : forall self p pa d a bp sp dr sa t,
    resolve_sym W p pa <> None ->
    In (bp, Spec (Some sp) dr sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W (w_reqs W) p (ATRef sp sa) t ->
    ResultMentions W self p (Spec (Some pa) d a) t
| RMCtx : forall self p pa a c dc n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some dc -> d_ast dc = ADCtx n its ->
    In sp its -> ResultMentions W self (d_mod dc) sp t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t
| RMFunctor : forall self p pa a u du n names args res sp t,
    resolve_sym W p pa = Some u -> w_decl W u = Some du ->
    d_ast du = ADFunctor n (Some names) args res None -> u <> self ->
    In sp res -> ResultMentions W u (d_mod du) sp t ->
    ~ FunOutputs W u t ->
    ~ (exists name, In name names /\ resolve_sym W (d_mod du) [name] = Some t) ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) None a) t.

Definition Mentions (W : world) (u : nat) (t : nat) : Prop :=
  exists d ps a r b sp, w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    In sp r /\ ResultMentions W u (d_mod d) sp t /\ ~ FunOutputs W u t.

(* P(F): the brackets; an unbracketed abstract functor has P(F) = M(F). *)
Definition Poly (W : world) (u : nat) (t : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    match ps with
    | None => Mentions W u t
    | Some names => exists x, In x names /\ resolve_sym W (d_mod d) [x] = Some t
    end.

(* X(F) = M(F) ∖ P(F), the fixed inputs ([D84] rule 1). *)
Definition Fixed (W : world) (u : nat) (t : nat) : Prop := Mentions W u t /\ ~ Poly W u t.

(* A functor provision retains its fixed inputs as read where provided. *)
Definition FixedReadings (W : world) (u : nat) (tm : list (nat * ty)) (fx : subst) : Prop :=
  forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x tm = Some tx.
Definition fixed_subst (sigma : subst) (fx : subst) : subst :=
  map (fun q => (fst q, app_subst sigma (snd q))) fx.
Definition fixed_app (fx : subst) : inst :=
  map (fun q => (fst q, Some (snd q))) fx.

(* `ItemEntries W p ob tm sp es tm'`: the context item `sp`, written in
   module p, under the enclosing items' bracket bindings `ob`, introduces
   entries `es`; the type meanings in force grow from tm to tm' (each
   assumed type is its own generation, TAbs t).  An item is read at
   `ob ++ tm`: a bracket binds only while the item it is on is read, and a
   type item is put in force as its own generation even where a bracket
   binds it (note 71).  [D20]: a type the item mentions must already be in
   force or bound (TyOf fails otherwise). *)
Inductive ItemEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| IEType : forall p ob tm pa t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NTy t]
                (match nlook t tm with Some _ => tm | None => (t, TAbs t) :: tm end)
| IENothing : forall p ob tm pa s k,
    resolve_sym W p pa = Some s -> kind_sym W s = Some k ->
    (k = DKUnit \/ k = DKTag \/ k = DKAlias \/
     (k = DKVal /\ exists d n t i, w_decl W s = Some d /\ d_ast d = ADVal n t (Some i))) ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [] tm
| IEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    Brackets W p (ob ++ tm) s a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| IEFn : forall p ob tm a pa s g bs ap,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    Brackets W p (ob ++ tm) s a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| IEFun : forall p ob tm pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d -> d_ast d = ADFunctor n ps args res None ->
    (* [D84] rule 2, [D20]: F is at an instantiation of X(F), so assuming
       it asks X(F) in force (note 70) *)
    (forall x, Fixed W u x -> nlook x tm <> None) ->
    FixedReadings W u tm fx ->
    ItemEntries W p ob tm (Spec (Some pa) None None) [NFun u fx] tm
| IEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    Brackets W p (ob ++ tm) m a bs ->
    RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    ItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
| IECtx : forall p ob tm a pa c d n its bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    Brackets W p (ob ++ tm) c a bs ->
    (* [D61]: the items of a context are read in the module that wrote them *)
    ItemsEntries W (d_mod d) (bs ++ ob) tm its es tm' ->
    ItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with ItemsEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> list spec -> list entry -> list (nat * ty) -> Prop :=
| IsNil : forall p ob tm, ItemsEntries W p ob tm [] [] tm
| IsCons : forall p ob tm sp r es1 tm1 es2 tm2,
    ItemEntries W p ob tm sp es1 tm1 -> ItemsEntries W p ob tm1 r es2 tm2 ->
    ItemsEntries W p ob tm (sp :: r) (es1 ++ es2) tm2.

(* An assume item `p` or `R.m` is the context item `p` or `R.m` unbracketed. *)
Definition assume_spec (it : path * option name) : spec := Spec (Some (fst it)) (snd it) None.

(* The key an entry is filed under, read under σ. *)
Definition entry_key (s : subst) (e : entry) : option item :=
  match e with
  | NTy _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey (app_subst s r) m)
  | NFun u _ => Some (IFun u)
  end.

Definition entry_sig (s : subst) (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] (app_subst s t))
  | NFn _ g _ | NMth _ _ g _ => Some (sig_subst s g)
  | _ => None
  end.

(* [D43] Merging.  σ *unifies* the entries if any two entries filed under
   one key (read under σ) have one signature under σ.  The merge of a
   telescope is a most general such σ that is idempotent (no cyclic merge:
   a finding of the mechanization, [D43] calculus note), mapping only
   generations of the telescope.  A merge identifying two distinct
   concrete heads has no unifier, so the telescope does not form. *)
Definition Unifies (s : subst) (es : list entry) : Prop :=
  forall e1 e2 k, In e1 es -> In e2 es -> entry_key s e1 = Some k -> entry_key s e2 = Some k ->
    entry_sig s e1 = entry_sig s e2.

Definition MostGeneralMerge (s : subst) (es : list entry) : Prop :=
  Unifies s es /\
  (forall t, app_subst s (app_subst s t) = app_subst s t) /\
  (forall s', Unifies s' es -> (forall t, app_subst s' (app_subst s t) = app_subst s' t)).

(* The context a merged telescope makes: its tables are the entries read
   under σ.  Instantiations keep None for a requirement not in force where
   the item was written, completed from the context (the elaborator's
   "freeze"). *)
Definition entries_cx (s : subst) (tm : list (nat * ty)) (es : list entry) : cx :=
  let fz (a : inst) : inst :=
    map (fun q => (fst q, option_map (app_subst s)
                            (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a in
  Cx (map (fun q => (fst q, app_subst s (snd q))) tm) s
     (flat_map (fun e => match e with NVal v t _ => [(v, app_subst s t)] | _ => [] end) es)
     (flat_map (fun e => match e with NFn f g _ => [(f, sig_subst s g)] | _ => [] end) es)
     (flat_map (fun e => match e with NMth r m g _ => [((app_subst s r, m), sig_subst s g)] | _ => [] end) es)
     (flat_map (fun e => match e with NFun u fx => [(u, fixed_subst s fx)] | _ => [] end) es)
     (flat_map (fun e => match e, entry_key s e with
                         | NVal _ _ a, Some k | NFn _ _ a, Some k | NMth _ _ _ a, Some k => [(k, fz a)]
                         | NFun u fx, Some k => [(k, fixed_app (fixed_subst s fx))]
                         | _, _ => [] end) es)
     [].

(* The needs of a telescope: its non-type keys, as a set (order is free). *)
Definition Needs (s : subst) (es : list entry) (ns : list item) : Prop :=
  NoDup ns /\ forall k, In k ns <-> exists e, In e es /\ entry_key s e = Some k.

(* [D84] (8), [D37] (note 54): `main` runs in the platform's root context.
   Beside what it assumes, any function in scope as `main` has lib/wasm.moss's
   `Gc`: each abstract functor `Gc` lists that is not already in force,
   after the types it is fixed in (X(F), [D84] rule 1), each in force as
   its own generation unless it is already. *)
Definition IsMainFn (W : world) (s : nat) : Prop :=
  exists d n ps rt body p, w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).

Definition add_tys (xs : list nat) (tm : list (nat * ty)) : list (nat * ty) :=
  fold_left (fun tm x => match nlook x tm with Some _ => tm | None => (x, TAbs x) :: tm end) xs tm.

(* `RootItems W q es0 tm its es tm'`: Gc's items `its` (written in module
   q), after the entries es0 and with meanings tm, add the entries es. *)
Inductive RootItems (W : world) (q : string) : list entry -> list (nat * ty) -> list spec -> list entry -> list (nat * ty) -> Prop :=
| RINil : forall es0 tm, RootItems W q es0 tm [] [] tm
| RIFun : forall es0 tm p a rest u du n ps args res xs fx es tm',
    resolve_sym W q p = Some u -> w_decl W u = Some du -> d_ast du = ADFunctor n ps args res None ->
    ~ (exists fx0, In (NFun u fx0) es0) -> NoDup xs -> (forall x, In x xs <-> Fixed W u x) ->
    FixedReadings W u (add_tys xs tm) fx ->
    RootItems W q (es0 ++ map NTy xs ++ [NFun u fx]) (add_tys xs tm) rest es tm' ->
    RootItems W q es0 tm (Spec (Some p) None a :: rest) (map NTy xs ++ [NFun u fx] ++ es) tm'
| RISkip : forall es0 tm sp rest es tm',
    match sp with
    | Spec (Some p) None _ => forall u, resolve_sym W q p = Some u -> kind_sym W u = Some DKFunctor -> exists fx, In (NFun u fx) es0
    | _ => True
    end ->
    RootItems W q es0 tm rest es tm' -> RootItems W q es0 tm (sp :: rest) es tm'.

Inductive RootCtx (W : world) (s : nat) (es0 : list entry) (tm : list (nat * ty)) : list entry -> list (nat * ty) -> Prop :=
| RootNone :
    (~ IsMainFn W s \/ forall gc d n its, LangSym W "wasm" NSName "Gc" gc -> w_decl W gc = Some d -> d_ast d <> ADCtx n its) ->
    RootCtx W s es0 tm [] tm
| RootGc : forall gc d n its es tm',
    IsMainFn W s -> LangSym W "wasm" NSName "Gc" gc -> w_decl W gc = Some d -> d_ast d = ADCtx n its ->
    RootItems W (d_mod d) es0 tm its es tm' -> RootCtx W s es0 tm es tm'.

(* `CtxOf W s C ns`: the context the assume nesting of symbol s forms (and,
   for `main`, the root context: note 54), and its needs (what a caller
   must provide, [D20]). *)
Definition CtxOf (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ C = entries_cx σ tmr (es ++ er) /\ Needs σ (es ++ er) ns.

(* ======================================================================== *)
(* V.4 Calls: needs, satisfaction maps and coherence                        *)
(* ======================================================================== *)

(* A need of the callee, read at the call site: a method need's receiver
   is re-read with the caller's type meanings ([D21]: an unapplied
   reference shares the symbols in force). *)
Definition at_site (C : cx) (k : item) : item :=
  match k with
  | IMth r m th => IMth (canon C (app_subst (map (fun q => (fst q, snd q)) (c_ty C)) r)) m th
  | _ => k
  end.

(* θ of a direct call of s from context C (note 27): each of s's
   requirements, in their fixed order (w_reqs), read at the site — its
   meaning in C, or itself where C gives it none — canonically. *)
Definition theta_at (W : world) (C : cx) (s : nat) : subst :=
  map (fun r => (r, canon C (app_subst (c_ty C) (TAbs r)))) (w_reqs W s).

(* [D20]/[D62]/[D71]: a call of callee s (assume context Cs, needs ns)
   from context C is answered by satisfaction map sm: sm sends each need
   (exactly once) to the caller's provision of it read here, which must be
   in force, at the instantiation the callee needs it at (coherence); and
   the callee's merges ([D43]) must hold here. *)
Definition Satisfies (C : cx) (Cs : cx) (ns : list item) (sm : smap) : Prop :=
  (* each need, by its run-time key in the callee, to the caller's
     provision of it read here, by its run-time key here (note 28) *)
  sm = map (fun k => (rkey_of Cs k, rkey_of C (at_site C k))) ns /\
  (forall k, In k ns -> provides C (at_site C k)) /\
  (* coherence: where both say what a requirement means, they agree *)
  (forall k a, In k ns -> ilook k (c_apps Cs) = Some a ->
     forall t want, In (t, Some want) a ->
       match ilook (at_site C k) (c_apps C) with
       | Some have => In (t, Some (canon C (app_subst (c_ty C) want))) have
       | None => True
       end) /\
  (* merges: every equation of the callee's σ holds of the caller's meanings *)
  (forall g t, In (g, t) (c_sub Cs) ->
     (forall x, In x (g :: gens t) -> nlook x (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) t)).

(* Arguments checked against parameter types ([D17] fits), elaborated in order. *)

(* ======================================================================== *)
(* V.5 Functors ([D55], [D78], [D84] with DECISIONS.md)                       *)
(* ======================================================================== *)

(* F's own terms: the context its argument telescope forms, plus its
   implicit mentioned inputs (symbols the explicit inputs do not list). *)
Definition FunCtx (W : world) (u : nat) (C : cx) (ns : list item) : Prop :=
  exists d ps a r b es tm tm' σ implicit,
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    ItemsEntries W (d_mod d) [] [] a es tm /\
    (* implicit inputs: only an abstract functor has them *)
    (forall t, In t implicit <-> (b = None /\ Mentions W u t /\ nlook t tm = None)) /\
    tm' = map (fun t => (t, TAbs t)) implicit ++ tm /\
    MostGeneralMerge σ es /\ C = entries_cx σ tm' es /\ Needs σ es ns.

(* F's result, provided in context C with each output type t read as
   `out t` (a fresh generation at an application): the context grows by
   the result's items, each at the instantiation it is made at, and `ks`
   are its non-type keys. *)
(* Output brackets are bindings at the point an output is installed.  Unlike
   an input telescope application, the output reader permits a binding that
   the target does not mention.  The right-hand types still use [TyOf]. *)
Inductive OutputBrackets (W : world) (p : string) (tm : list (nat * ty)) :
    option (list (path * spec)) -> list (nat * ty) -> Prop :=
| OBrNone : OutputBrackets W p tm None []
| OBrSome : forall bs l,
    Forall2 (fun b tb =>
      exists sp sa, snd b = Spec (Some sp) None sa /\
        resolve_sym W p (fst b) = Some (fst tb) /\
        kind_sym W (fst tb) = Some DKType /\
        TyOf W p tm None (ATRef sp sa) (snd tb)) bs l ->
    OutputBrackets W p tm (Some bs) (rev l).

(* Output installation follows the result list in order.  Type outputs have
   already received their generations; the item walk leaves their meanings
   alone.  A defined function or value is already available and adds no slot. *)
Inductive OutputItemEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> spec -> list entry -> list (nat * ty) -> Prop :=
| OIEType : forall p ob tm pa a bs t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NTy t] tm
| OIENothing : forall p ob tm pa a bs s k,
    resolve_sym W p pa = Some s -> kind_sym W s = Some k ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (k = DKUnit \/ k = DKTag \/ k = DKAlias \/
      (k = DKVal /\ exists d n t i, w_decl W s = Some d /\ d_ast d = ADVal n t (Some i)) \/
      (k = DKFn /\ exists d r n ps rt b, w_decl W s = Some d /\
         d_ast d = ADFn r n ps rt (Some b))) ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [] tm
| OIEVal : forall p ob tm a pa s d n t ty0 bs ap,
    resolve_sym W p pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    TyOf W (d_mod d) (bs ++ ob ++ tm) None t ty0 ->
    AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NVal s ty0 ap] tm
| OIEFn : forall p ob tm a pa s g bs ap d rcv n ps rt,
    resolve_sym W p pa = Some s -> kind_sym W s = Some DKFn ->
    w_decl W s = Some d -> d_ast d = ADFn rcv n ps rt None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    SigOf W s (bs ++ ob ++ tm) None g -> AppOf W s (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFn s g ap] tm
| OIEFun : forall p ob tm a bs pa u d n ps args res fx,
    resolve_sym W p pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps args res None ->
    OutputBrackets W p (ob ++ tm) a bs ->
    (forall x, Fixed W u x -> nlook x (bs ++ ob ++ tm) <> None) ->
    FixedReadings W u (bs ++ ob ++ tm) fx ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) [NFun u fx] tm
| OIEMeth : forall p ob tm a pa dn r m rt g bs ap,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    OutputBrackets W p (ob ++ tm) a bs -> RecvTy W r (bs ++ ob ++ tm) rt ->
    SigOf W m (bs ++ ob ++ tm) (Some rt) g -> AppOf W m (bs ++ ob ++ tm) ap ->
    OutputItemEntries W p ob tm (Spec (Some pa) (Some dn) a) [NMth rt m g ap] tm
| OIECtx : forall p ob tm a pa c d n its bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemsEntries W (d_mod d) (bs ++ ob) tm its es tm' ->
    OutputItemEntries W p ob tm (Spec (Some pa) None a) es tm'
with OutputItemsEntries (W : world) : string -> list (nat * ty) ->
    list (nat * ty) -> list spec -> list entry -> list (nat * ty) -> Prop :=
| OIsNil : forall p ob tm, OutputItemsEntries W p ob tm [] [] tm
| OIsCons : forall p ob tm sp rest es1 tm1 es2 tm2,
    OutputItemEntries W p ob tm sp es1 tm1 ->
    OutputItemsEntries W p ob tm1 rest es2 tm2 ->
    OutputItemsEntries W p ob tm (sp :: rest) (es1 ++ es2) tm2.

(* Result slots follow the written output occurrences.  Repeated output keys
   remain repeated slots, as in both evaluators; input [Needs] stays set-valued. *)
Definition ResultKeys (s : subst) (es : list entry) (ks : list item) : Prop :=
  ks = flat_map (fun e => match entry_key s e with Some k => [k] | None => [] end) es.

(* Replace the output roles before normalizing through the caller's merges.
   Output provisions and their recorded applications use these completed
   meanings, as [provided_at] freezes the corresponding source readings. *)
Definition output_type_map (C : cx) (outs : list nat) (out : nat -> ty) : list (nat * ty) :=
  map (fun q => (fst q, canon C (snd q))) (map (fun t => (t, out t)) outs ++ c_ty C).

Definition output_cx (C : cx) (tm : list (nat * ty)) (es : list entry) : cx :=
  Cx (map (fun q => (fst q, canon C (snd q))) tm) (c_sub C)
            (flat_map (fun e => match e with NVal v t _ => [(v, canon C t)] | _ => [] end) (rev es) ++ c_vals C)
            (flat_map (fun e => match e with NFn f g _ => [(f, sig_subst (c_sub C) g)] | _ => [] end) (rev es) ++ c_fns C)
            (flat_map (fun e => match e with NMth r m g _ => [((canon C r, m), sig_subst (c_sub C) g)] | _ => [] end) (rev es) ++ c_mths C)
            (flat_map (fun e => match e with NFun u' fx => [(u', fixed_subst (c_sub C) fx)] | _ => [] end) (rev es) ++ c_funs C)
            (flat_map (fun e => match e, entry_key (c_sub C) e with
                                | NVal _ _ a0, Some k | NFn _ _ a0, Some k | NMth _ _ _ a0, Some k => [(k, a0)]
                                | NFun u fx, Some k => [(k, fixed_app (fixed_subst (c_sub C) fx))]
                                | _, _ => [] end) (rev es) ++ c_apps C)
            (c_locals C).

Definition ProvideResult (W : world) (u : nat) (C : cx) (out : nat -> ty) (C' : cx) (ks : list item) : Prop :=
  exists d ps a r b outs es tm',
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    (forall t, In t outs <-> FunOutputs W u t) /\
    OutputItemsEntries W (d_mod d) [] (output_type_map C outs out) r es tm' /\
    ResultKeys (c_sub C) es ks /\
    C' = output_cx C tm' es.

(* [D84] rule 7: a family is an abstract functor whose result is method
   items only and holds no type, each item's key determining every P(F)
   symbol it mentions; its bindings bind no vals and apply no functor but
   a family (DECISIONS.md). *)
Inductive FamilyResult (W : world) : string -> spec -> Prop :=
| FRMethod : forall mi pa dn br,
    FamilyResult W mi (Spec (Some pa) (Some dn) br)
| FRContext : forall mi pa br u d n its,
    resolve_sym W mi pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADCtx n its ->
    Forall (FamilyResult W (d_mod d)) its ->
    FamilyResult W mi (Spec (Some pa) None br).

Definition FamilyShape (W : world) (u : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    Forall (FamilyResult W (d_mod d)) r.

Definition FamilyBracketBound (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists b, In b (match a with Some bs => bs | None => [] end) /\
    resolve_sym W p (fst b) = Some t.
Definition FamilyBracketFree (W : world) (p : string)
    (a : option (list (path * spec))) (t : nat) : Prop :=
  exists bp pa dn bs, In (bp, Spec (Some pa) dn bs)
    (match a with Some brackets => brackets | None => [] end) /\
    FreeIn W (w_reqs W) p (ATRef pa bs) t.

(* Every polymorphic signature/bracket dependency not replaced by the leaf's
   brackets is determined by its receiver key, as the actual D84 classifier
   checks.  A method-only abstract functor need not be a family. *)
Inductive FamilyKeyed (W : world) (u : nat) : string -> spec -> Prop :=
| FKMethod : forall p pa dn a r m,
    resolve_sym W p pa = Some r -> MethodNamed W p r dn m ->
    (forall t, Poly W u t ->
       (SigMentions W m t \/ FamilyBracketFree W p a t) ->
       ~ FamilyBracketBound W p a t ->
       FreeIn W (w_reqs W) p (ATRef pa None) t) ->
    FamilyKeyed W u p (Spec (Some pa) (Some dn) a)
| FKContext : forall p pa a c d n its,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    Forall (FamilyKeyed W u (d_mod d)) its ->
    FamilyKeyed W u p (Spec (Some pa) None a).

Definition FamilyKeys (W : world) (u : nat) : Prop :=
  forall d n ps a r, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r None ->
    Forall (FamilyKeyed W u (d_mod d)) r.
Definition Family (W : world) (u : nat) : Prop :=
  FamilyShape W u /\ FamilyKeys W u.

(* ======================================================================== *)
(* V.6 Bodies: expressions, blocks, binds                                    *)
(* ======================================================================== *)

(* The frame of a body: its module, return type, receiver type (a method),
   whether a `break` has a loop, the numbering of local names (injective;
   parameters, after `this`, are numbered from 0 in order: the runtime
   passes arguments so), and the generations `bind F;` made in it so far
   (a value of their types may not leave the block that made them, [D78]). *)
Record frame := Frame {
  fr_mod : string; fr_ret : ty; fr_this : option ty; fr_loop : bool;
  fr_var : list (name * nat); fr_fresh : list nat;
  fr_applying : list nat;   (* functors being applied by name: [D55] no cycle *)
  fr_pins : list nat        (* in a body bound to F: M(F), which it may not rebind ([D84] 3d) *)
}.

(* a local's number, for a name the numbering covers ([Named]) *)
Definition var_of (F : frame) (n : name) : nat :=
  match slook n (fr_var F) with Some k => k | None => 0 end.
Definition Named (F : frame) (n : name) : Prop := slook n (fr_var F) <> None.
(* the numbering is injective, and numbers the parameters 0, 1, … after `this` *)
Definition Numbering (F : frame) (this : option ty) (params : list name) : Prop :=
  NoDup (map fst (fr_var F)) /\ NoDup (map snd (fr_var F)) /\
  (forall i pn, nth_error params i = Some pn ->
     slook pn (fr_var F) = Some (match this with Some _ => S i | None => i end)).

Definition with_loop (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) true (fr_var F) (fr_fresh F) (fr_applying F) (fr_pins F).
Definition with_mod (F : frame) (p : string) (u : nat) : frame :=
  Frame p (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (u :: fr_applying F) (fr_pins F).
Definition with_fresh (F : frame) (gs : list nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F ++ gs) (fr_applying F) (fr_pins F).

Definition add_local (C : cx) (n : name) (t : ty) (mu : lmut) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) ((n, (t, mu)) :: c_locals C).
Definition set_ty (C : cx) (t : nat) (tau : ty) : cx :=
  Cx ((t, tau) :: c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition provide (C : cx) (k : item) (a : inst) (C1 : cx) : cx :=
  Cx (c_ty C1) (c_sub C1) (c_vals C1) (c_fns C1) (c_mths C1) (c_funs C1) ((k, a) :: c_apps C1) (c_locals C1).
Definition set_val (C : cx) (v : nat) (t : ty) : cx :=
  Cx (c_ty C) (c_sub C) ((v, t) :: c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_fn (C : cx) (f : nat) (g : fsig) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) ((f, g) :: c_fns C) (c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_mth (C : cx) (r : ty) (m : nat) (g : fsig) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (((r, m), g) :: c_mths C) (c_funs C) (c_apps C) (c_locals C).
Definition set_fun (C : cx) (u : nat) (fx : list (nat * ty)) : cx :=
  Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) ((u, fx) :: c_funs C) ((IFun u, fixed_app fx) :: c_apps C) (c_locals C).

Definition body_of (P : xprogram) (f : nat) : option xexpr := nlook f (xo_fns P).
Definition funbody_of (P : xprogram) (h : nat) : option (tele * list xbind) := nlook h (xo_funs P).

(* A symbol elaboration makes (a lambda's provider, a forwarder, a wrapper,
   a captured local's val, a generation, a linked functor body) is not a
   source symbol. *)
Definition Fresh (W : world) (s : nat) : Prop := w_decl W s = None /\ s <> 0.

(* The meaning of a decimal literal ([D68]): digits, an optional fraction
   and an optional exponent, as numerator / denominator. *)
Fixpoint digits_val (l : list ascii) (acc : nat) : nat :=
  match l with [] => acc | c :: r => digits_val r (10 * acc + (nat_of_ascii c - 48)) end.

Fixpoint split_at (c : ascii) (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | x :: r => if Ascii.eqb x c then (rev acc, Some r) else split_at c r (x :: acc)
  end.

Definition lit_value (text : string) : nat * nat :=
  let '(mant, ex) := split_at "e"%char (list_ascii_of_string text) [] in
  let '(whole, frac) := split_at "."%char mant [] in
  let fr := match frac with Some f => f | None => [] end in
  let n := digits_val (whole ++ fr) 0 in
  let '(eneg, emag) := match ex with
                       | Some ("-"%char :: d) => (true, digits_val d 0)
                       | Some ("+"%char :: d) => (false, digits_val d 0)
                       | Some d => (false, digits_val d 0)
                       | None => (false, 0) end in
  if eneg then (n, 10 ^ (emag + length fr))
  else if Nat.leb (length fr) emag then (n * 10 ^ (emag - length fr), 1)
  else (n, 10 ^ (length fr - emag)).

Definition is_float_lit (text : string) : bool :=
  existsb (fun c => Ascii.eqb c "."%char || Ascii.eqb c "e"%char) (list_ascii_of_string text).

(* [D84] rule 7 / DECISIONS.md: at most one family in force produces a key.
   Here θ is the matching substitution over P(F), not the full emitted
   application argument list; fixed caller meanings cannot be overridden. *)
Definition FixedAgreement (W : world) (C : cx) (u : nat) : Prop :=
  forall x, Fixed W u x ->
    match ilook (IFun u) (c_apps C), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => canon C was = canon C now
        | _ => True
        end
    | _, _ => True
    end.

Definition FamilyProduces (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  exists fx th C' ks, nlook u (c_funs C) = Some fx /\ Family W u /\
    Forall (fun q => Poly W u (fst q)) th /\
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) th C) (fun t => TAbs t) C' ks /\
    In k ks.

Definition OnlyFamily (W : world) (C : cx) (u : nat) (k : item) : Prop :=
  forall u', u' <> u -> ~ FamilyProduces W C u' k.

(* A literal's binary arithmetic call uses a provision already in force,
   or a unique family applied in a scope around that call ([D84] rule 7).
   Its one parameter and result are exactly the literal's own type. *)
Inductive LitOp (W : world) (C : cx) (t : ty) (m : nat) (ox oy : xexpr) : xexpr -> Prop :=
| LitOpIn :
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitOp W C t m ox oy (XMeth ox m (rkey C t m) [oy])
| LitOpFamily : forall u fx matching th sm Cu nsu C' ks,
    mlook t m (c_mths C) = None -> nlook u (c_funs C) = Some fx -> Family W u ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (forall a, Poly W u a -> nlook a (c_ty Cu) <> None -> exists tt, In (a, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) (fun a => TAbs a) C' ks ->
    In (mkey t m) ks -> OnlyFamily W C u (mkey t m) ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    mlook t m (c_mths C') = Some (FSig [t] t) ->
    LitOp W C t m ox oy (XApply u th sm (XMeth ox m (rkey C' t m) [oy])).

(* [D68] A literal is arithmetic over fixed library symbols: lib/int.moss's
   `zero` and `one` (as provided here), num.moss's `.add`/`.mul`/`.div`
   provided at their type, and lib/float.moss's `float`.  `LitTerm` says a
   term denotes the rational n/d in exact arithmetic; the literal's
   elaboration is *some* such term at the literal's type.  (The draft does
   not fix the term's shape; DRAFT NOTE 6.) *)
Inductive LitTerm (W : world) (C : cx) (t : ty) : xexpr -> nat -> nat -> Prop :=
| LitZero : forall z, LangSym W "int" NSName "zero" z -> nlook z (c_vals C) = Some t ->
    LitTerm W C t (XVl z) 0 1
| LitOne : forall o, LangSym W "int" NSName "one" o -> nlook o (c_vals C) = Some t ->
    LitTerm W C t (XVl o) 1 1
| LitAdd : forall m x y a b c d, LangSym W "num" NSDetached "add" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * d + c * b) (b * d)
| LitMul : forall m x y a b c d, LangSym W "num" NSDetached "mul" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * c) (b * d)
| LitDiv : forall m x y a b c d, LangSym W "num" NSDetached "div" m ->
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitTerm W C t x a b -> LitTerm W C t y c d -> c <> 0 ->
    LitTerm W C t (XMeth x m (rkey C t m) [y]) (a * d) (b * c)
| LitFloat : forall f ti tp x a b, LangSym W "float" NSName "float" f ->
    nlook f (c_fns C) = Some (FSig [tp] t) -> fits ti tp -> LitTerm W C ti x a b ->
    LitTerm W C t (XCallA f [x]) a b
| LitAddOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "add" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t result (a * d + c * b) (b * d)
| LitMulOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "mul" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d ->
    LitTerm W C t result (a * c) (b * d)
| LitDivOp : forall m x y result a b c d,
    LangSym W "num" NSDetached "div" m -> LitOp W C t m x y result ->
    LitTerm W C t x a b -> LitTerm W C t y c d -> c <> 0 ->
    LitTerm W C t result (a * d) (b * c).

(* Operators ([D68], [D77] rule 2): fixed to num.moss's detached methods;
   the `for.` operators of [D85]'s `for` sugar (Parse.for_desugar) to
   access.moss's (note 60).  The library module's short name, and the
   method's name in it. *)
Definition operator_method (op : string) (unary : bool) : option (string * string) :=
  if unary then
    match op with
    | "-" => Some ("num", "neg") | "!" => Some ("num", "not")
    | "for.begin" => Some ("access", "begin") | _ => None
    end
  else
    match op with
    | "*" => Some ("num", "mul") | "/" => Some ("num", "div") | "%" => Some ("num", "rem")
    | "+" => Some ("num", "add") | "-" => Some ("num", "sub") | "<<" => Some ("num", "shl")
    | ">>" => Some ("num", "shr") | "==" => Some ("num", "eq") | "!=" => Some ("num", "ne")
    | "<" => Some ("num", "lt") | ">" => Some ("num", "gt") | "<=" => Some ("num", "le")
    | ">=" => Some ("num", "ge") | "&" => Some ("num", "and") | "^" => Some ("num", "xor")
    | "|" => Some ("num", "or")
    | "for.more" => Some ("access", "more") | "for.get" => Some ("access", "get")
    | "for.after" => Some ("access", "after")
    | _ => None
    end.

(* the payload type of a tag at its arguments *)
Inductive PayloadOf (W : world) : nat -> list (nat * ty) -> ty -> Prop :=
| PayTag : forall s xs d n ps t pay,
    w_decl W s = Some d -> d_ast d = ADTag n ps t -> TyOf W (d_mod d) xs None t pay ->
    PayloadOf W s xs pay.

(* The fields of a record pattern `P{…}` against the payload's fields
   `ftys`, in the order written: `x` binds the field x to a local x, `x =
   y` to a local y, `x = _` binds nothing (note 35). *)
Inductive FieldPats (W : world) (F : frame) (ftys : list (nat * ty))
    : list (name * option apat) -> list (name * ty) -> list (fld * var) -> Prop :=
| FPNil : FieldPats W F ftys [] [] []
| FPPun : forall n t r bs xs,
    nlook (w_fld W n) ftys = Some t -> Named F n -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, None) :: r) ((n, t) :: bs) ((w_fld W n, var_of F n) :: xs)
| FPWild : forall n t r bs xs,
    nlook (w_fld W n) ftys = Some t -> FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some APWild) :: r) bs xs
| FPBind : forall n y t r bs xs,
    nlook (w_fld W n) ftys = Some t -> resolve W (fr_mod F) [y] = None -> Named F y ->
    FieldPats W F ftys r bs xs ->
    FieldPats W F ftys ((n, Some (APPath [y])) :: r) ((y, t) :: bs) ((w_fld W n, var_of F y) :: xs).

(* The pattern judgment: a source pattern, in frame F, against scrutinee
   type `st` compiles to an xpat, binding locals; `hs` are the heads it
   covers and `catch` whether it matches everything.  A head names the one
   member of st with that head ([D15], note 33). *)
Inductive PatOf (W : world) (F : frame) (st : ty) : apat -> xpat -> list (name * ty) -> list nat -> bool -> Prop :=
| PWild : PatOf W F st APWild (XPAny None) [] [] true
| PBinder : forall n,
    resolve W (fr_mod F) [n] = None -> Named F n ->
    PatOf W F st (APPath [n]) (XPAny (Some (var_of F n))) [(n, st)] [] true
| PUnit : forall pa s k xs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some k -> (k = DKUnit \/ k = DKTag) ->
    In (TNom s xs) (members st) ->
    PatOf W F st (APPath pa) (XPHead s None []) [] [s] false
(* `S(y)`: y is the payload, at the member's arguments; a struct tag's is a
   bare record type (note 35) *)
| PTagBind : forall pa s xs y pay,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
    PayloadOf W s xs pay ->
    resolve W (fr_mod F) [y] = None -> Named F y ->
    PatOf W F st (APTag pa (APPath [y])) (XPHead s (Some (var_of F y)) []) [(y, pay)] [s] false
(* `S{x, x = y, x = _}`: S's payload is a record, its fields bound by name *)
| PRecord : forall pa s xs fs ftys bs fxs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
    PayloadOf W s xs (TNom rec_head ftys) ->
    FieldPats W F ftys fs bs fxs ->
    PatOf W F st (APRec (Some pa) fs) (XPHead s None fxs) bs [s] false.

(* `let` binds an immutable local, `var` an assignable one. *)
Inductive LetStmt : astmt -> name -> option aty -> aexpr -> lmut -> Prop :=
| LSLet : forall n a y, LetStmt (ASLet n a y) n a y LLet
| LSVar : forall n a y, LetStmt (ASVar n a y) n a y LVar.

(* Whether a `break` in an expression, block or statement leaves the
   innermost loop around it: one not inside a nested `while` or `loop`
   (note 42). *)
Inductive Breaks : aexpr -> Prop :=
| BkBreak : Breaks AEBreak
| BkCall : forall p a args y, In y args -> Breaks y -> Breaks (AECall p a args)
| BkField : forall o n, Breaks o -> Breaks (AEField o n)
| BkMethRecv : forall o p args, Breaks o -> Breaks (AEMeth o p args)
| BkMethArg : forall o p args y, In y args -> Breaks y -> Breaks (AEMeth o p args)
| BkRec : forall p a fs n y, In (n, Some y) fs -> Breaks y -> Breaks (AERec p a fs)
| BkIfC : forall c b e, Breaks c -> Breaks (AEIf c b e)
| BkIfB : forall c b e, BlockBreaks b -> Breaks (AEIf c b e)
| BkIfE : forall c b y, Breaks y -> Breaks (AEIf c b (Some y))
| BkMatchS : forall s arms, Breaks s -> Breaks (AEMatch s arms)
| BkMatchA : forall s arms pt y, In (pt, y) arms -> Breaks y -> Breaks (AEMatch s arms)
| BkReturn : forall y, Breaks y -> Breaks (AEReturn (Some y))
| BkBlock : forall b, BlockBreaks b -> Breaks (AEBlock b)
| BkUnary : forall op y, Breaks y -> Breaks (AEUnary op y)
| BkBinL : forall op l r, Breaks l -> Breaks (AEBinary op l r)
| BkBinR : forall op l r, Breaks r -> Breaks (AEBinary op l r)
| BkLam : forall ps r y, Breaks y -> Breaks (AELam ps r y)
with BlockBreaks : ablock -> Prop :=
| BBStmt : forall ss tl s, In s ss -> StmtBreaks s -> BlockBreaks (ABlock ss tl)
| BBTail : forall ss y, Breaks y -> BlockBreaks (ABlock ss (Some y))
with StmtBreaks : astmt -> Prop :=
| SBLet : forall n t y, Breaks y -> StmtBreaks (ASLet n t y)
| SBVar : forall n t y, Breaks y -> StmtBreaks (ASVar n t y)
| SBAssign : forall n y, Breaks y -> StmtBreaks (ASAssign n y)
| SBExpr : forall y, Breaks y -> StmtBreaks (ASExpr y)
| SBBind : forall its sp y, In (sp, Some y) its -> Breaks y -> StmtBreaks (ASBind its).

(* The type a block has as a branch of an `if` or a `match` arm: its
   tail's, and `()` without a tail, even if it cannot fall through
   (note 42). *)
Definition branch_ty (b : ablock) (t : ty) : ty :=
  match b with ABlock _ None => TUnit | ABlock _ (Some _) => t end.

(* Auxiliary judgments (not mutually recursive with the main ones). *)

Inductive Join : ty -> ty -> option ty -> ty -> Prop :=
| JoinExp : forall a b t, Join a b (Some t) t
| JoinLeftNever : forall b, Join never b None b
| JoinSame : forall a b, (b = never \/ a = b) -> a <> never -> Join a b None a.

(* D78 compares raw signature leaves, recursively through contexts. Types
   remain leaves; units, tags, aliases, and concrete values/functions do
   not require a provision. Brackets affect later interpretations, not
   this preliminary leaf-identity check. *)
Definition result_already_provided (d : adecl) : bool :=
  match d with
  | ADUnit _ | ADTag _ _ _ | ADAlias _ _ => true
  | ADVal _ _ (Some _) | ADFn RPlain _ _ _ (Some _) => true
  | _ => false
  end.
Inductive ResultSpecLeaf (W : world) : string -> spec -> (nat * option name) -> Prop :=
| RSLMethod : forall p pa dn br s,
    resolve_sym W p pa = Some s ->
    ResultSpecLeaf W p (Spec (Some pa) (Some dn) br) (s,Some dn)
| RSLPlain : forall p pa br s d,
    resolve_sym W p pa = Some s -> w_decl W s = Some d ->
    (forall n its, d_ast d <> ADCtx n its) ->
    result_already_provided (d_ast d) = false ->
    ResultSpecLeaf W p (Spec (Some pa) None br) (s,None)
| RSLContext : forall p pa br c d n its sp k,
    resolve_sym W p pa = Some c -> w_decl W c = Some d ->
    d_ast d = ADCtx n its -> In sp its ->
    ResultSpecLeaf W (d_mod d) sp k ->
    ResultSpecLeaf W p (Spec (Some pa) None br) k.
Inductive ResultLeaf (W : world) : nat -> (nat * option name) -> Prop :=
| RL : forall u d n ps a r b sp k,
    w_decl W u = Some d -> d_ast d = ADFunctor n ps a r b ->
    In sp r -> ResultSpecLeaf W (d_mod d) sp k -> ResultLeaf W u k.

(* [D78]/[D84] 3e: G's result is F's, leaf by leaf. *)
Definition SameResult (W : world) (u g : nat) : Prop :=
  forall k, ResultLeaf W u k <-> ResultLeaf W g k.

(* a method bind, filed also under each key it answers as another spelling
   (pairs: the body's key, the key as declared) *)
Definition refile (pairs : list (item * item)) (b : xbind) : list xbind :=
  match b with
  | XBMth t m th f sm =>
      b :: flat_map (fun p => match p with
                              | (IMth t' m' _, IMth t0 _ th0) =>
                                  if ty_eqb t t' && Nat.eqb m m' && negb (item_eqb (IMth t0 m th0) (IMth t m th))
                                  then [XBMth t0 m th0 f sm] else []
                              | _ => [] end) pairs
  | _ => [b]
  end.

Inductive BindsOf : xexpr -> list xbind -> Prop :=
| BOEnd : BindsOf XUnit []
| BOTy : forall t tau x r, BindsOf x r -> BindsOf (XBindTy t tau x) (XBTy t tau :: r)
| BOVal : forall v e x r, BindsOf x r -> BindsOf (XBindVal v e x) (XBVal v e :: r)
| BOFn : forall f g sm x r, BindsOf x r -> BindsOf (XBindFn f g sm x) (XBFn f g sm :: r)
| BOMth : forall t m th f sm x r, BindsOf x r -> BindsOf (XBindMth t m th f sm x) (XBMth t m th f sm :: r)
| BOFun : forall u h sm smc x r, BindsOf x r -> BindsOf (XBindFun u h sm smc x) (XBFun u h sm smc :: r)
| BOFunA : forall u h sm x r, BindsOf x r -> BindsOf (XBindFunA u h sm x) (XBFunA u h sm :: r)
| BOApply : forall u th sm x r, BindsOf x r -> BindsOf (XApply u th sm x) (XBApply u th sm :: r).

Inductive Mentions_name : aexpr -> name -> Prop :=
| MNPath : forall n, Mentions_name (AEPath [n] None) n
| MNSub : forall e e' n, Sub_expr e' e -> Mentions_name e' n -> Mentions_name e n

with Sub_expr : aexpr -> aexpr -> Prop :=
| SubCallArg : forall p a args y, In y args -> Sub_expr y (AECall p a args)
| SubField : forall o n, Sub_expr o (AEField o n)
| SubMethRecv : forall o p args, Sub_expr o (AEMeth o p args)
| SubMethArg : forall o p args y, In y args -> Sub_expr y (AEMeth o p args)
| SubRec : forall p a fs n y, In (n, Some y) fs -> Sub_expr y (AERec p a fs)
| SubRecPun : forall p a fs n, In (n, None) fs -> Sub_expr (AEPath [n] None) (AERec p a fs)
| SubIfC : forall c b e, Sub_expr c (AEIf c b e)
| SubIfB : forall c ss tl e y, (exists st, In st ss /\ Stmt_sub y st) \/ tl = Some y -> Sub_expr y (AEIf c (ABlock ss tl) e)
| SubIfE : forall c b y, Sub_expr y (AEIf c b (Some y))
| SubMatchS : forall s arms, Sub_expr s (AEMatch s arms)
| SubMatchA : forall s arms pt y, In (pt, y) arms -> Sub_expr y (AEMatch s arms)
| SubReturn : forall y, Sub_expr y (AEReturn (Some y))
| SubBlockT : forall ss y, Sub_expr y (AEBlock (ABlock ss (Some y)))
| SubBlockS : forall ss tl s y, In s ss -> Stmt_sub y s -> Sub_expr y (AEBlock (ABlock ss tl))
| SubUnary : forall op y, Sub_expr y (AEUnary op y)
| SubBinL : forall op l r, Sub_expr l (AEBinary op l r)
| SubBinR : forall op l r, Sub_expr r (AEBinary op l r)
| SubLam : forall ps r y, Sub_expr y (AELam ps r y)

with Stmt_sub : aexpr -> astmt -> Prop :=
| SSLet : forall n t y, Stmt_sub y (ASLet n t y)
| SSVar : forall n t y, Stmt_sub y (ASVar n t y)
| SSAssign : forall n y, Stmt_sub y (ASAssign n y)
| SSAssignName : forall n y, Stmt_sub (AEPath [n] None) (ASAssign n y)
| SSExpr : forall y, Stmt_sub y (ASExpr y)
| SSWhileC : forall c b, Stmt_sub c (ASWhile c b)
| SSWhileB : forall c b, Stmt_sub (AEBlock b) (ASWhile c b)
| SSLoop : forall b, Stmt_sub (AEBlock b) (ASLoop b)
| SSBind : forall its sp y, In (sp, Some y) its -> Stmt_sub y (ASBind its).

(* Automatic family demand checks an initial missing method key, then applies
   its unique family in the context extended by preceding demands.  Type
   arguments are temporary; a family provides methods only. *)
Definition family_restore (C Cr : cx) : cx :=
  Cx (c_ty C) (c_sub Cr) (c_vals Cr) (c_fns Cr) (c_mths Cr)
     (c_funs Cr) (c_apps Cr) (c_locals Cr).


Inductive FamilyStep (W : world) (C0 : cx) (wanted : list item)
    : cx -> nat -> subst -> subst -> smap -> cx -> Prop :=
| FamilyStepApply : forall C ot m u fx matching th sm Cu nsu Cr ks,
    In (mkey ot m) wanted -> mlook ot m (c_mths C0) = None ->
    nlook u (c_funs C0) = Some fx -> nlook u (c_funs C) = Some fx ->
    Family W u -> OnlyFamily W C0 u (mkey ot m) ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (forall t, Poly W u t -> nlook t (c_ty Cu) <> None -> exists tt, In (t, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)
       (fun t => TAbs t) Cr ks ->
    In (mkey ot m) ks ->
    FamilyStep W C0 wanted C u matching th sm (family_restore C Cr).

(* Group identity is the inferred matching substitution.  Distinct groups
   may emit equal complete type arguments after reading the other inputs. *)
Inductive Demand (W : world) (C0 : cx) (wanted : list item)
    : list (nat * subst) -> cx -> (xexpr -> xexpr) -> cx -> Prop :=
| DemandDone : forall used C,
    Demand W C0 wanted used C (fun x => x) C
| DemandApply : forall used C u matching th sm Cm wrap Cf,
    ~ In (u, matching) used -> FamilyStep W C0 wanted C u matching th sm Cm ->
    Demand W C0 wanted ((u, matching) :: used) Cm wrap Cf ->
    Demand W C0 wanted used C (fun x => XApply u th sm (wrap x)) Cf.

(* Main judgments.
   `Ex W P F C e exp x t`: in frame F and context C, the expression e,
   checked against the expected type `exp` if any, elaborates to x of type t.
   `Blk W P F C ss tl exp x t`: a block (statements ss, optional tail).
   `Call W P F C callee args x t` etc. are its pieces. *)
Inductive Ex (W : world) (P : xprogram) : frame -> cx -> aexpr -> option ty -> xexpr -> ty -> Prop :=
| ExUnit : forall F C exp, Ex W P F C AEUnit exp XUnit TUnit
| ExThis : forall F C exp t, fr_this F = Some t -> Ex W P F C AEThis exp (XVar 0) t
| ExLocal : forall F C exp n t mu,
    slook n (c_locals C) = Some (t, mu) -> Named F n ->
    Ex W P F C (AEPath [n] None) exp (XVar (var_of F n)) t
| ExUnitTag : forall F C exp pa s,
    (forall n, pa = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKUnit ->
    Ex W P F C (AEPath pa None) exp (XTag s [] XUnit) (TNom s [])
| ExVal : forall F C exp pa s d n t0 t,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 None ->
    nlook s (c_vals C) = Some t ->
    Ex W P F C (AEPath pa None) exp (XVl s) t
(* a defined val (no assumptions yet, [D50]) is its initializer, inlined,
   elaborated in an empty context *)
| ExConst : forall F C exp pa s d n t0 i t x,
    (forall n', pa = [n'] -> slook n' (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> w_decl W s = Some d -> d_ast d = ADVal n t0 (Some i) ->
    d_assumes d = [] -> TyOf W (d_mod d) [] None t0 t ->
    (exists F0 t', Ex W P F0 (Cx [] [] [] [] [] [] [] []) i (Some t) x t' /\ fits t' t /\
                   fr_mod F0 = d_mod d) ->
    Ex W P F C (AEPath pa None) exp (XConst s x) t
(* a tag applied to its payload *)
| ExTag : forall F C exp pa a args s bs xs pay y v ty,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay ->
    args = [y] -> Ex W P F C y (Some pay) v ty -> fits ty pay ->
    Ex W P F C (AECall pa a args) exp (XTag s xs v) (TNom s xs)
(* [D20]/[D21] a defined fn: its needs answered from here (sm), its
   signature read with the meanings here *)
| ExCallD : forall F C exp pa args f Cf ns sm g xs,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns -> Satisfies C Cf ns sm ->
    SigOf W f (c_ty C) None g ->
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (XCallD f (theta_at W C f) sm xs) (sg_ret g)
(* Defined calls may first demand missing methods from unique families. *)
| ExCallDDemand : forall F C exp pa args f Cf ns sm g xs wrap C',
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns ->
    Demand W C (map (at_site C) ns) [] C wrap C' ->
    Satisfies C' Cf ns sm -> SigOf W f (c_ty C') None g ->
    Args W P F C' args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp
       (wrap (XCallD f (theta_at W C' f) sm xs)) (sg_ret g)
(* an abstract fn: whatever provides it here *)
| ExCallA : forall F C exp pa args f g xs,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (XCallA f xs) (sg_ret g)
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
| ExLit : forall F C exp text x t num den,
    (* its type: `one`'s, `zero`'s (`0` needs only `zero`: note 63), or a float's *)
    ((exists o, LangSym W "int" NSName "one" o /\ nlook o (c_vals C) = Some t) \/
     (exists z, LangSym W "int" NSName "zero" z /\ nlook z (c_vals C) = Some t) \/
     is_float_lit text = true) ->
    lit_value text = (num, den) ->
    LitTerm W C t x num den ->
    Ex W P F C (AELit text) exp x t
| ExIfThen : forall F C exp c b cx' bx bt,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    opt_fits TUnit exp -> (bt = TUnit \/ bt = never) ->
    Ex W P F C (AEIf c b None) exp (XIf cx' bx XUnit) TUnit
| ExIfElse : forall F C exp c b y cx' bx bt ex' et t,
    Cond W P F C c cx' -> BlkE W P F C b exp bx bt ->
    Branch W P F C y exp ex' et -> Join (branch_ty b bt) et exp t ->
    Ex W P F C (AEIf c b (Some y)) exp (XIf cx' bx ex') t
| ExMatch : forall F C exp s arms sx st xarms t,
    Ex W P F C s None sx st ->
    Arms W P F C (canon C st) arms exp xarms t ->
    Ex W P F C (AEMatch s arms) exp (XMatchP sx xarms) t
| ExBlock : forall F C exp b x t,
    BlkE W P F C b exp x t -> Ex W P F C (AEBlock b) exp x (match exp with Some e => e | None => t end)
| ExReturnUnit : forall F C exp,
    (fr_ret F = TUnit \/ fr_ret F = never) ->
    Ex W P F C (AEReturn None) exp (XReturn XUnit) never
| ExReturn : forall F C exp e v t',
    Ex W P F C e (Some (fr_ret F)) v t' -> fits t' (fr_ret F) ->
    Ex W P F C (AEReturn (Some e)) exp (XReturn v) never
| ExBreak : forall F C exp, fr_loop F = true -> Ex W P F C AEBreak exp XBreak never
| ExRecord : forall F C exp pa a fs s bs xs pay ftys xfs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay -> pay = TNom rec_head ftys ->
    (* every field given once, in any order; the record holds them in the payload's order *)
    NoDup (map fst fs) -> length fs = length ftys ->
    Forall2 (fun ft xf => fst xf = fst ft /\
               exists n v vt, w_fld W n = fst ft /\ In (n, v) fs /\
                 Ex W P F C (match v with Some y => y | None => AEPath [n] None end) (Some (snd ft)) (snd xf) vt /\
                 fits vt (snd ft)) ftys xfs ->
    Ex W P F C (AERec pa a fs) exp (XTag s xs (XRec xfs)) (TNom s xs)
| ExField : forall F C exp o n ox ot rec t,
    Ex W P F C o None ox ot ->
    (* a tag's payload, or a bare record (note 44) *)
    ((exists s xs, ot = TNom s xs /\ kind_sym W s = Some DKTag /\ PayloadOf W s xs rec) \/ rec = ot) ->
    (exists ftys, rec = TNom rec_head ftys /\ nlook (w_fld W n) ftys = Some t) ->
    Ex W P F C (AEField o n) exp (XField ox (w_fld W n)) t

(* the payload type of a tag at its arguments *)

with Args (W : world) (P : xprogram) : frame -> cx -> list aexpr -> list ty -> list xexpr -> Prop :=
| ArgsNil : forall F C, Args W P F C [] [] []
| ArgsCons : forall F C a r t ts x xs at',
    Ex W P F C a (Some t) x at' -> fits (canon C at') (canon C t) ->
    Args W P F C r ts xs -> Args W P F C (a :: r) (t :: ts) (x :: xs)

(* [D45] a condition is a Bool (lib/bool.moss's) *)
with Cond (W : world) (P : xprogram) : frame -> cx -> aexpr -> xexpr -> Prop :=
| CondBool : forall F C c x t b,
    LangSym W "bool" NSName "Bool" b ->
    Ex W P F C c (Some (TNom b [])) x t -> fits t (TNom b []) -> Cond W P F C c x

(* a branch of an `if` (its `else`) or a match arm: a block has its
   branch type, or the expected one (note 42) *)
with Branch (W : world) (P : xprogram) : frame -> cx -> aexpr -> option ty -> xexpr -> ty -> Prop :=
| BranchBlock : forall F C b exp x t,
    BlkE W P F C b exp x t ->
    Branch W P F C (AEBlock b) exp x (match exp with Some e => e | None => branch_ty b t end)
| BranchExpr : forall F C y exp x t,
    (forall b, y <> AEBlock b) -> Ex W P F C y exp x t -> Branch W P F C y exp x t

(* branches agree, or the expected type decides *)

with Arms (W : world) (P : xprogram) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (xpat * xexpr) -> ty -> Prop :=
| ArmsEmptyNever : forall F C exp, Arms W P F C never [] exp [] (match exp with Some e => e | None => never end)
| ArmsSome : forall F C st arms exp xarms covered catch t,
    st <> never ->
    ArmsList W P F C st arms exp xarms covered catch t ->
    (* exhaustive: a catch-all arm, or every member's head covered *)
    (catch = true \/ (members st <> [] /\ forall m h, In m (members st) -> head m = Some h -> In h covered)) ->
    Arms W P F C st arms exp xarms t

with ArmsList (W : world) (P : xprogram) : frame -> cx -> ty -> list (apat * aexpr) -> option ty -> list (xpat * xexpr) -> list nat -> bool -> ty -> Prop :=
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
with MethCall (W : world) (P : xprogram) : frame -> cx -> xexpr -> ty -> path -> list aexpr -> xexpr -> ty -> Prop :=
(* `o.mod::m(...)`: the detached method reached through its module *)
| MCQualified : forall F C ox ot pa args m x t,
    length pa > 1 -> resolve_detached W (fr_mod F) pa = Some m ->
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
| MCAttachedDefined : forall F C ox ot n args h att Ca ns sm g xs rargs,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) Ca ns sm ->
    SigOf W att (rargs ++ c_ty C) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    MethCall W P F C ox ot [n] args
             (XCallD att (theta_at W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) att) sm (ox :: xs))
             (sg_ret g)
(* Defined attached calls can demand methods after installing receiver arguments.
   Their source arguments are still checked in the original caller context. *)
| MCAttachedDefinedDemand : forall F C ox ot n args h att Ca ns sm g xs rargs wrap Cd,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    Demand W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)
      (map (at_site (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)) ns) []
      (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
    Satisfies Cd Ca ns sm -> SigOf W att (c_ty Cd) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    MethCall W P F C ox ot [n] args
      (wrap (XCallD att (theta_at W Cd att) sm (ox :: xs))) (sg_ret g)
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
with Provided (W : world) (P : xprogram) : frame -> cx -> xexpr -> ty -> nat -> list aexpr -> xexpr -> ty -> Prop :=
| PrIn : forall F C ox ot m args g xs,
    mlook ot m (c_mths C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Provided W P F C ox ot m args (XMeth ox m (rkey C ot m) xs) (sg_ret g)
(* [D84] rule 7: no provision here, but a family F in force produces the
   key at θ (read off the key: θ instantiates P(F) so that a result item's
   receiver becomes ot); the call is elaborated as if preceded by `bind F;`
   at θ, in a scope of its own: `XApply F θ sm` around the call.  No search:
   at most one family in a context produces a key (DECISIONS.md, rule 7). *)
| PrFamily : forall F C ox ot m args u fx matching th sm Cu nsu C' ks g xs,
    mlook ot m (c_mths C) = None ->
    nlook u (c_funs C) = Some fx -> Family W u ->
    Forall (fun q => Poly W u (fst q)) matching ->
    FunCtx W u Cu nsu ->
    (* θ covers the polymorphic inputs actually present in F's own context. *)
    (forall t, Poly W u t -> nlook t (c_ty Cu) <> None -> exists tt, In (t, tt) th) ->
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here,
       nlook t (c_ty (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C)) = Some here /\
       In (t, canon (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) here) th) ->
    FixedAgreement W (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) u ->
    ProvideResult W u (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) (fun t => TAbs t) C' ks ->
    In (mkey ot m) ks ->
    (* uniqueness: no other family in force produces this key *)
    OnlyFamily W C u (mkey ot m) ->
    Satisfies (fold_left (fun C q => set_ty C (fst q) (snd q)) matching C) Cu nsu sm ->
    mlook ot m (c_mths C') = Some g ->
    Args W P F C args (sg_params g) xs ->
    Provided W P F C ox ot m args (XApply u th sm (XMeth ox m (rkey C' ot m) xs)) (sg_ret g)

(* a block whose type is checked against `exp` and may not let a value of
   a generation it made escape ([D78]) *)
with BlkE (W : world) (P : xprogram) : frame -> cx -> ablock -> option ty -> xexpr -> ty -> Prop :=
| BlkECheck : forall F C ss tl exp x t made,
    Blk W P F C ss tl exp x t made ->
    (forall g, In g made -> ~ In g (gens t)) ->
    opt_fits t exp ->
    BlkE W P F C (ABlock ss tl) exp x t

(* `Blk ... x t made`: `made` are the generations its binds made *)
with Blk (W : world) (P : xprogram) : frame -> cx -> list astmt -> option aexpr -> option ty -> xexpr -> ty -> list nat -> Prop :=
| BlkTail : forall F C y exp x t, Ex W P F C y exp x t -> Blk W P F C [] (Some y) exp x t []
| BlkEmpty : forall F C exp, Blk W P F C [] None exp XUnit TUnit []
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
    Blk W P F C (st :: rest) tl exp (XLet (var_of F n) v xr) t made
| BlkAssign : forall F C n y rest tl exp tv v vt xr t made,
    slook n (c_locals C) = Some (tv, LVar) ->
    Ex W P F C y (Some tv) v vt -> fits vt tv ->
    Blk W P F C rest tl exp xr t made ->
    Named F n ->
    Blk W P F C (ASAssign n y :: rest) tl exp (XSeq (XAssign (var_of F n) v) xr) t made
| BlkExprLast : forall F C y exp v vt,
    Ex W P F C y None v vt ->
    Blk W P F C [ASExpr y] None exp (XSeq v XUnit) (if ty_eqb vt never then never else TUnit) []
| BlkExpr : forall F C y rest tl exp v vt xr t made,
    (rest <> [] \/ tl <> None) ->
    Ex W P F C y None v vt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASExpr y :: rest) tl exp (XSeq v xr) t made
| BlkWhile : forall F C c b rest tl exp cx' bx bt xr t made,
    Cond W P F C c cx' -> BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASWhile c b :: rest) tl exp (XSeq (XWhile cx' bx) xr) t made
| BlkLoop : forall F C b rest tl exp bx bt xr t made,
    (rest <> [] \/ tl <> None \/ BlockBreaks b) ->
    BlkE W P (with_loop F) C b None bx bt ->
    Blk W P F C rest tl exp xr t made ->
    Blk W P F C (ASLoop b :: rest) tl exp (XSeq (XLoop bx) xr) t made
(* a block whose last statement is a `loop` no `break` leaves cannot fall
   through (note 42) *)
| BlkLoopLast : forall F C b exp bx bt,
    BlkE W P (with_loop F) C b None bx bt -> ~ BlockBreaks b ->
    Blk W P F C [ASLoop b] None exp (XSeq (XLoop bx) XUnit) never []
(* a bind statement: its items in order, each wrapping the rest *)
| BlkBind : forall F C its rest tl exp wrap C' F' xr t made made',
    BindItems W P F C its wrap C' F' made ->
    Blk W P F' C' rest tl exp xr t made' ->
    Blk W P F C (ASBind its :: rest) tl exp (wrap xr) t (made ++ made')

(* `BindItems ... wrap C' F' made`: a run of bind items elaborates to the
   binds `wrap` puts around whatever follows them. *)
with BindItems (W : world) (P : xprogram) : frame -> cx -> list (spec * option aexpr) -> (xexpr -> xexpr) -> cx -> frame -> list nat -> Prop :=
| BINil : forall F C, BindItems W P F C [] (fun x => x) C F []
| BICons : forall F C it r w1 C1 F1 m1 w2 C2 F2 m2,
    BindItem W P F C it w1 C1 F1 m1 -> BindItems W P F1 C1 r w2 C2 F2 m2 ->
    BindItems W P F C (it :: r) (fun x => w1 (w2 x)) C2 F2 (m1 ++ m2)

with BindItem (W : world) (P : xprogram) : frame -> cx -> (spec * option aexpr) -> (xexpr -> xexpr) -> cx -> frame -> list nat -> Prop :=
(* [D72] `bind T = τ;` starts a new generation: from here T means τ (read
   here); what was typed before keeps the old meaning.  Nothing at run time.
   [D84] 3d: in a body bound to F, a type of M(F) may not be rebound. *)
| BTypeBind : forall F C pa t rp ra tau,
    resolve_sym W (fr_mod F) pa = Some t -> kind_sym W t = Some DKType ->
    ~ In t (fr_pins F) ->
    TyOf W (fr_mod F) (c_ty C) None (ATRef rp ra) tau ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath rp ra)) (XBindTy t (canon C tau))
             (set_ty C t (canon C tau)) F []
(* `bind v = e;` *)
| BValBind : forall F C pa v d n t0 i declared y x xt a,
    resolve_sym W (fr_mod F) pa = Some v -> w_decl W v = Some d -> d_ast d = ADVal n t0 i ->
    TyOf W (d_mod d) (c_ty C) None t0 declared ->
    Ex W P F C y (Some declared) x xt -> fits xt declared ->
    AppOf W v (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some y) (XBindVal v x)
             (provide C (IVal v) a (set_val C v declared)) F []
(* `bind f = g;`: g a defined fn whose signature, read here, is f's *)
| BFnBind : forall F C pa f g sg pg sm a,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    FnProvider W P F C (AEPath pg None) sg None g sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFn f g sm)
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D70] `bind f = |ps| body;`: see LambdaProvider *)
| BFnLambda : forall F C pa f sg ps rt body pid pre sm a,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    SigOf W f (c_ty C) None sg ->
    LambdaProvider W P F C ps rt body sg None pid pre sm ->
    AppOf W f (c_ty C) a ->
    BindItem W P F C (Spec (Some pa) None None, Some (AELam ps rt body)) (fun x => pre (XBindFn f pid sm x))
             (provide C (IFn f) a (set_fn C f sg)) F []
(* [D61] `bind R.m[brackets] = provider;`: the key is R read here (under
   the brackets), the provider an attached defined method of R's head, a
   defined fn (eta-expanded to take the receiver, [D54]), or a lambda *)
| BMethBind : forall F C pa dn a y r m bs rt sg target pre sm ap,
    resolve_sym W (fr_mod F) pa = Some r -> MethodNamed W (fr_mod F) r dn m ->
    Brackets W (fr_mod F) (c_ty C) m a bs ->
    TyOf W (fr_mod F) (bs ++ c_ty C) None (ATRef pa None) rt ->
    SigOf W m (bs ++ c_ty C) (Some rt) sg ->
    MethProvider W P F C y sg rt target pre sm ->
    AppOf W m (bs ++ c_ty C) ap ->
    BindItem W P F C (Spec (Some pa) (Some dn) a, Some y)
             (fun x => pre (XBindMth (canon C rt) m (theta_of C ap) target sm x))
             (provide C (mkey (canon C rt) m) ap (set_mth C (canon C rt) m sg)) F []
(* [D55] `bind F;` for F with a body: F's binds, here, read in F's module *)
| BApplyBody : forall F C pa u d n ps a r bodies wrap C' F' made,
    resolve_sym W (fr_mod F) pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADFunctor n ps a r (Some bodies) -> ~ In u (fr_applying F) ->
    BindItems W P (with_mod F (d_mod d) u) C (concat bodies) wrap C' F' made ->
    BindItem W P F C (Spec (Some pa) None None, None) wrap C'
             (Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F') (fr_applying F) (fr_pins F)) made
(* [D78]/[D84] rule 5, `bind F;` for F abstract and in force: an
   application at θ = F's inputs read here; each type output already in
   force here becomes a fresh rigid generation ([D72]); F's fixed inputs
   X(F) must denote what they did where F was provided, else a static error;
   exactly F's result is then in force, its items at the application's
   generations. *)
| BApplyAbstract : forall F C pa u fx Cu ns sm th gensof C' ks newgens,
    resolve_sym W (fr_mod F) pa = Some u -> nlook u (c_funs C) = Some fx ->
    FunCtx W u Cu ns ->
    (* inputs in force here, and θ reads them *)
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here, nlook t (c_ty C) = Some here /\
        In (t, canon C here) th) ->
    (* rule 5: fixed inputs unchanged since the provision *)
    (forall x tx, In (x, tx) fx -> exists here,
      nlook x (c_ty C) = Some here /\ canon C here = canon C tx) ->
    Satisfies C Cu ns sm ->
    (* outputs: fresh where the symbol is already in force *)
    (forall t, FunOutputs W u t ->
       (nlook t (c_ty C) = None /\ gensof t = t) \/
       (nlook t (c_ty C) <> None /\ Fresh W (gensof t) /\ In (gensof t) newgens /\ In (t, TAbs (gensof t)) th)) ->
    ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
    BindItem W P F C (Spec (Some pa) None None, None) (XApply u th sm) C' (with_fresh F newgens) newgens
(* [D78]/[D84] rule 4: `bind F = H;`, H abstract in force: H's inputs
   are among F's, and F's binding is H's *)
| BFunCopy : forall F C pa u pg h Cu nsu Ch nsh sm fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some h ->
    nlook h (c_funs C) <> None ->
    FunCtx W u Cu nsu -> FunCtx W h Ch nsh -> incl nsh nsu ->
    (* Copying requires the target fixed inputs now and preserves every
       known fixed interpretation carried by the existing source binding. *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    (forall x, Fixed W h x -> Fixed W u x) ->
    FixedAgreement W C h ->
    SameResult W u h ->
    sm = map (fun k => (rkey_of Ch k, rkey_of Cu k)) nsh ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFunA u h sm) (set_fun C u fx) F []
(* [D84] rule 3, `bind F = G;`, G with a body: a closure.  G's inputs
   that F takes are linked (supplied by each application), the rest are
   captured here (vals by value) and must be in force here; G's body is
   checked once, in its own terms, as the core functor body h (LinkBody). *)
| BFunClosure : forall F C pa u pg gg Cu nsu Cg nsg h sm smc fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some gg ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    (exists n ps a r b, ast_of W gg = Some (ADFunctor n ps a r (Some b))) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    (* 3a: X(F) in force here *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u gg ->
    LinkBody W P u gg h ->
    sm = map (fun k => (rkey_of Cg k, rkey_of Cu k)) (filter (fun k => existsb (item_eqb k) nsu) nsg) ->
    smc = map (fun k => (rkey_of Cg k, rkey_of C (at_site C k))) (filter (fun k => negb (existsb (item_eqb k) nsu)) nsg) ->
    (forall k k', In (k, k') smc -> provides C k') ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (XBindFun u h sm smc) (set_fun C u fx) F []

(* [D66] a fn provider: a defined fn (its needs answered here), or an
   abstract fn in force, through a fresh defined forwarder whose body calls
   it; its signature read here must be the bound fn's. *)
with FnProvider (W : world) (P : xprogram) : frame -> cx -> aexpr -> fsig -> option ty -> nat -> smap -> Prop :=
| FPDefined : forall F C pa g sg Cg ns sm,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists n ps rt b, ast_of W g = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W g Cg ns -> Satisfies C Cg ns sm ->
    SigOf W g (c_ty C) None (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    FnProvider W P F C (AEPath pa None) sg None g sm
| FPForward : forall F C pa g sg w n,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists nm ps rt, ast_of W g = Some (ADFn RPlain nm ps rt None)) ->
    nlook g (c_fns C) = Some sg -> n = length (sg_params sg) ->
    Fresh W w -> body_of P w = Some (XCallA g (map XVar (seq 0 n))) ->
    FnProvider W P F C (AEPath pa None) sg None w [(IFn g, IFn g)]

(* a method provider (the receiver is the provider's first argument) *)
with MethProvider (W : world) (P : xprogram) : frame -> cx -> aexpr -> fsig -> ty -> nat -> (xexpr -> xexpr) -> smap -> Prop :=
| MPAttached : forall F C rp n rg att rt sg Ca ns sm,
    resolve_sym W (fr_mod F) rp = Some rg -> Attached W rg n att ->
    (exists rn ps r b, ast_of W att = Some (ADFn (RAttached rn) n ps r (Some b))) ->
    head rt = Some rg ->
    CtxOf W att Ca ns -> Satisfies C Ca ns sm ->
    SigOf W att (c_ty C) None
      (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    MethProvider W P F C (AEField (AEPath rp None) n) sg rt att (fun x => x) sm
| MPWrapped : forall F C pa sg rt g sm w n Cg nsg,
    FnProvider W P F C (AEPath pa None) sg None g sm ->
    CtxOf W g Cg nsg -> n = length (sg_params sg) ->
    Fresh W w ->
    body_of P w = Some (XCallD g (theta_at W Cg g) (map (fun k => (rkey_of Cg k, rkey_of Cg k)) nsg) (map XVar (seq 1 n))) ->
    MethProvider W P F C (AEPath pa None) sg rt w (fun x => x) sm
| MPLambda : forall F C ps r body sg rt pid pre sm,
    LambdaProvider W P F C ps r body sg (Some rt) pid pre sm ->
    MethProvider W P F C (AELam ps r body) sg rt pid pre sm

(* [D70] an anonymous function at a bind: its parameters are the bound
   signature's (annotations, if any, must agree); each local of the
   enclosing body it mentions is captured by value — a fresh val bound here
   to the local's value; the function is a fresh defined fn assuming
   everything in force here (with the captured vals), its body read with
   the captured names as those vals (`let x = v;` in front); its needs are
   answered by the same items here. *)
with LambdaProvider (W : world) (P : xprogram) : frame -> cx -> list (name * option aty) -> option aty -> aexpr -> fsig -> option ty -> nat -> (xexpr -> xexpr) -> smap -> Prop :=
| LP : forall F C ps rt body sg this pid caps capvals Cl Fl lbody ltype keys sm,
    length ps = length (sg_params sg) -> NoDup (map fst ps) ->
    Forall2 (fun pq t => match snd pq with
                         | Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C t
                         | None => True end) ps (sg_params sg) ->
    match rt with Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C (sg_ret sg)
                 | None => True end ->
    (* captured: the enclosing locals it mentions, less its parameters *)
    (forall n, In n caps <-> (Mentions_name body n /\ slook n (c_locals C) <> None /\ ~ In n (map fst ps) /\ fr_applying F = [])) ->
    NoDup caps -> length capvals = length caps -> Forall (Fresh W) capvals -> NoDup capvals ->
    Fresh W pid ->
    (* the provider's own context: everything here, the captured vals in
       force (as LCaptured locals: not assignable), no other locals *)
    Cl = fold_left (fun C' q => let '(n, v) := q in
                      match slook n (c_locals C) with
                      | Some (t, _) => add_local (provide C' (IVal v) [] (set_val C' v t)) n t LCaptured
                      | None => C' end)
                   (combine caps capvals)
                   (Cx (c_ty C) (c_sub C) (c_vals C) (c_fns C) (c_mths C) (c_funs C) (c_apps C) []) ->
    (* its frame: numbered afresh, parameters (after `this`) from 0 *)
    fr_mod Fl = fr_mod F -> fr_ret Fl = sg_ret sg -> fr_this Fl = this -> fr_loop Fl = false ->
    Numbering Fl this (map fst ps) -> Forall (Named Fl) caps -> Forall (Named F) caps ->
    BlkE W P Fl (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet)
                           (combine (map fst ps) (sg_params sg)) Cl)
         (ABlock [] (Some body)) (Some (sg_ret sg)) lbody ltype ->
    fits ltype (sg_ret sg) ->
    body_of P pid = Some (fold_right (fun q x => XLet (var_of Fl (fst q)) (XVl (snd q)) x) lbody
                                    (combine caps capvals)) ->
    (* its needs: every key in force here, each answered by itself *)
    (forall k, In k keys <-> (provides Cl k /\
       (match k with ITy _ => False | _ => True end) /\ k = provision_id k)) ->
    NoDup keys -> sm = map (fun k => (rkey_of Cl k, rkey_of C k)) keys ->
    LambdaProvider W P F C ps rt body sg this pid
      (fun x => fold_right (fun q x => XBindVal (snd q) (XVar (var_of F (fst q))) x) x (combine caps capvals))
      sm

(* [D78]/[D84] 3e: G's result is F's, item by item: the same leaves
   (keys, as written, through contexts). *)

(* [D84] 3b–3e: G's binds checked once, in G's own terms — G's context
   formed from its own inputs, each output of F as itself — producing the
   core functor body h: its result telescope (F's output types, then F's
   result items), and its binds.  3b: no captured item mentions a type
   that applications supply; 3d: pins may not rebind M(F); 3e: every item
   of F's result is made by the binds, at the types F promises. *)
with LinkBody (W : world) (P : xprogram) : nat -> nat -> nat -> Prop :=
| LB : forall u gg h dg n ps a r bodies Cg nsg Fg wrap Cb Fb made outs Cr ks binds0 binds,
    w_decl W gg = Some dg -> d_ast dg = ADFunctor n ps a r (Some bodies) ->
    FunCtx W gg Cg nsg ->
    (* 3b *)
    (forall k t, In k nsg -> ~ (exists Cu nsu, FunCtx W u Cu nsu /\ In k nsu) ->
       match k with
       | IVal v => forall vt, nlook v (c_vals Cg) = Some vt -> In t (gens vt) -> nlook t (c_ty Cg) = None
       | _ => True end) ->
    fr_mod Fg = d_mod dg -> fr_pins Fg = [] \/ (forall x, In x (fr_pins Fg) <-> Mentions W u x) ->
    BindItems W P Fg Cg (concat bodies) wrap Cb Fb made ->
    (forall t, In t outs <-> FunOutputs W u t) ->
    ProvideResult W u Cg (fun t => TAbs t) Cr ks ->
    (* 3e: each result key is provided by the body *)
    (forall k, In k ks -> provides Cb k) ->
    (* h's body, as a run of binds, is what `wrap` puts around nothing *)
    BindsOf (wrap XUnit) binds0 ->
    (* a result method item is filed under its key as F declares it too
       (receiver and θ in F's terms, note 28) *)
    binds = flat_map (refile (map (fun k => (at_site Cb k, rkey_of Cr k)) ks)) binds0 ->
    Fresh W h ->
    funbody_of P h = Some (map ITy outs ++ map (rkey_of Cr) ks, binds) ->
    LinkBody W P u gg h.

(* ======================================================================== *)
(* V.7 Programs                                                              *)
(* ======================================================================== *)

(* A defined fn or method f: its body, under the context its assumes form
   (and, for an attached method, the receiver as `this` at its
   requirements' generations), with the parameters numbered from 0 (after
   `this`), elaborates to `body_of P f`. *)
Definition FnElab (W : world) (P : xprogram) (f : nat) : Prop :=
  forall d rcv n ps rt b,
    w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  exists C ns this g F x t,
    CtxOf W f C ns /\
    (match rcv with
     | RAttached _ => exists r args, Attached W r n f /\ this = Some (TNom r args) /\
                        Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args
     | _ => this = None end) /\
    SigOf W f (c_ty C) this g /\
    fr_mod F = d_mod d /\ fr_ret F = sg_ret g /\ fr_this F = this /\ fr_loop F = false /\
    fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F this (map fst ps) /\
    BlkE W P F (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine (map fst ps) (sg_params g)) C)
         b (Some (sg_ret g)) x t /\
    body_of P f = Some x.

(* [D79]/platform: main's initial environment.  Each need of main must be
   something the platform provides: lib/wasm.moss's and lib/wasip1.moss's
   fns are primitives (`prim_of_name`, Part IV), wasistd's `i32_zero`/
   `i32_one` the I32 constants, and an abstract functor of those modules a
   platform binding (DRAFT NOTE 9). *)
Definition platform_mod (W : world) (p : string) : Prop :=
  LibraryModule W "wasm" p \/ LibraryModule W "wasip1" p \/ LibraryModule W "wasistd" p.

Inductive PlatformEntry (W : world) (P : xprogram) : item -> xdentry -> Prop :=
| PEZero : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_zero") -> PlatformEntry W P (IVal v) (XDVal (XVI32 0%uint63))
| PEOne : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_one") -> PlatformEntry W P (IVal v) (XDVal (XVI32 1%uint63))
| PEPrim : forall f d n ps rt body pr, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = Some pr ->
    PlatformEntry W P (IFn f) (XDPrim pr)
| PEUnsupported : forall f d n ps rt body i, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = None -> nth_error (xo_prims P) i = Some n ->
    PlatformEntry W P (IFn f) (XDPrim (PUnsupported i))
| PEFun : forall u d n ps a r h, w_decl W u = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFunctor n ps a r None -> funbody_of P h <> None ->
    PlatformEntry W P (IFun u) (XDFun h [] []).

(* The runtime type prefix is a representation of the source context,
   separate from the values and code supplied by the platform. Its six
   machine heads are fresh and selected from the actual library scope. *)
Definition initial_machine_names : list name :=
 ["I32";"I64";"F32";"F64";"StructRef";"ArrayRef"].
Definition initial_machine_subst(W:world)(base:nat):subst:=
 flat_map(fun q=>match w_library W with
 |None=>[]
 |Some dir=>match w_scope W(String.append dir "wasm.moss")NSName(snd q)with
   |Some(ESym symbol)=>[(symbol,TNom(base+fst q)[])]
   |_=>[]end end)(combine(seq 0 6)initial_machine_names).
Definition InitialTypes(W:world)(C:cx)(types:subst):Prop:=
 exists base,(forall slot,slot<6->Fresh W(base+slot))/\
 forall key,nlook key types=option_map(app_subst(initial_machine_subst W base))(nlook key(c_ty C)).

(* The relation.  [D16]: every defined fn is elaborated, reached or not. *)
Definition Elaborates (b : bundle) (P : xprogram) : Prop :=
  exists W, WorldOf b W /\
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
       FnElab W P f) /\
    (* main: the entry's `main`, a defined fn assuming only what the
       platform provides *)
    w_scope W (b_entry b) NSName "main" = Some (ESym (xo_main P)) /\
    (exists d n ps rt body, w_decl W (xo_main P) = Some d /\ d_ast d = ADFn RPlain n ps rt (Some body)) /\
    (exists C ns types platform, CtxOf W (xo_main P) C ns /\ InitialTypes W C types /\
       xo_env P = xinst [] types ++ platform /\ map fst platform = map (rkey_of C) ns /\
       Forall (fun ie => PlatformEntry W P (fst ie) (snd ie)) platform) /\
    (* Bool's symbols, when lib/bool.moss is loaded ([D45]) *)
    (LangSym W "bool" NSName "Bool" (xo_bool P) /\ LangSym W "bool" NSName "True" (xo_true P) /\
     LangSym W "bool" NSName "False" (xo_false P)
     \/ ((forall s, ~ LangSym W "bool" NSName "Bool" s) /\ xo_bool P = 0 /\ xo_true P = 0 /\ xo_false P = 0)).

End Elaboration.

(* DRAFT NOTES — where the draft departs from the elaborator (Elab.v), and
   the spec questions writing it raised.  Numbered for reference.

   1. (permissive) Layout ([D59]) and the platform reference checks are not
      in the draft: the elaborator also rejects a type with no layout and a
      `Ref`/`Array` built from a bare handle.  Completeness (relation ⇒
      elaborator accepts) cannot hold until they are.
   2. (permissive) [D79] memory instructions under a rebound `Memory`:
      the elaborator reports "unsupported"; the draft says nothing.
      m4: they are `Addr`'s methods and the family `Memories` (D79 as
      amended); the elaborator reports "unsupported" where `Memories` is
      applied in a program that binds `Memory`, since MossX has one memory.
   3. (decided: see note 27) θ is part of the elaboration contract.
   4. (spec gap) Order.  Requirement lists, needs and satisfaction maps
      are sets here; the elaborator's order is its walk's.  Observable only
      through `TNom` argument order, which the draft fixes per symbol
      (w_reqs) without saying which.  Is the canonical order part of the
      language (it is part of the runtime key, [D62])?
   5. (spec gap) [D43] most general merge.  The draft takes σ most general
      among *self-consistent* unifiers (keys compared under the unifier
      itself), which is what "union the atom graphs, unify per key" means
      when method keys contain merged types.  The elaborator iterates its
      walk at most 8 times (and a 9th must add nothing, note 52); a
      telescope needing more rounds is rejected by it and accepted here.
   6. (deliberately loose) [D68] literals: any term over zero/one/add/mul
      (/div, float) denoting the literal's exact value is allowed; the
      elaborator's is a Horner chain with digits as sums of `one`.  If the
      designer wants the shape fixed (it is observable through overflow
      in a provided `.add`), the rule must say which.
   7. (m4: implemented) [D84] rule 7 (PrFamily) and rule 1's fixed inputs
      (Fixed, rule 5's check in BApplyAbstract) are in the relation, and
      the elaborator now does both (Elab.family_member/apply_family at a
      call's miss; functor_fixed, fixed_ok in apply_abstract).  Rule 7's
      θ is "read off the key": the
      draft states it as *any* θ under which the family's result contains
      the key, plus uniqueness of the family — DECISIONS.md's "no two items
      share an instance of their key" is what makes θ unique, and is not
      yet a condition of `Family`.
   8. (spec gap) [D70] capture is by *mention*: a local named in the body
      (anywhere, including as a field-punning record entry) is captured,
      even if shadowed inside the lambda by a `let`.  The elaborator does
      the same (free_names_e is syntactic); the designer should confirm.
   9. (abstracted) The platform's functors (`Heap`, `Mutable`, `Arrays`)
      are bound to synthesized bodies of primitive binds (Elab.platform_fun);
      the draft says only that some body exists.
  10. (not covered) `const_val`'s cache, forwarder/wrapper sharing (one per
      provider) and lowering order are unobservable and absent.
  11. (spec gap) [D77] rule 4 needs "the context provides the detached
      name at the receiver"; the draft reads this as a provision at the
      receiver's canonical type exactly ([D62]), as the elaborator does —
      a provision at another instantiation of the same head does not win.
  12. (resolved) [D61] `find_method_decl` reads an attached method in
      the receiver's file only, as Attached does. The redundant lookup
      in the current module has been removed together with note 57's
      detached-method fallback.

   CHANGES THE PROOF WORK FORCED (RelFacts.v, RelExamples.v, RelNegative.v,
   RelFamily.v), recorded for the designer:
  13. Attached methods are a table of the world (w_attached, with
      wo_attached tying it to the declarations), not an existential over
      all symbols: otherwise "the attached method of Box named get" could
      not be identified without enumerating every symbol (family example).
  14. Local numbering is a finite injective list (Named/Numbering), not an
      injective function on all strings: the elaborator numbers only the
      names a body uses, in order of first use.
  15. A block ending in `e;` has the type () (or |) *whatever* is expected
      (BlkExprLast took exp = None only, so every fn whose body ends in a
      statement was unrelatable: found on app.moss).
  16. ProvideResult reads the type outputs from the declaration
      (FunOutputs), not from the entries being produced (a circularity).
  17. Rule 7's "one family per key" is the named premise OnlyFamily.
  18. Positivity: `if` with/without else, `return` with/without value,
      and `let`/`var` (LetStmt) are separate rules; SameResult is a
      definition.
   Elaborator finding (cosmetic): its rule-7 miss message prints the key
   as "`.Box.get`" (Elab.provided_call strips only a leading dot).

   m4 (5ea4e2e's language), where the elaborator now goes beyond the draft:
  19. (not in the draft) [D84] rule 7 for a callee's needs (lower.demand_
      needs): at a call of a defined fn or attached method, each method
      need nothing in force provides and a family in force produces is met
      by `XApply F θ sm` around the call (one per family and θ).  The draft
      has PrFamily for method calls only; CallDirect/Satisfies would need
      the same premise as PrFamily per need.
  20. (not in the draft) [D84] (8), [D37]: `main` runs in the platform's
      root context: CtxOf for `main` must add lib/wasm.moss's `Gc` (its
      functors, with the types they are fixed in) beside its assumes
      (Elab.add_root_context).  ItemsEntries resolves context items by
      path in the declaring module, and `Gc` need not be in scope there, so
      the draft needs an entry form by symbol.
  21. (not in the draft) `Family` is only "method items only"; the
      elaborator also checks rule 7's conditions: keys determine P(F)
      (Elab.is_family), a family's binding binds no val and applies no
      functor but a family, no direct bind of a member in force, no two
      families producing a key, no two items of one at a key instance.
  22. (not in the draft) [D84] 3d pins (a body's bind of x ∈ X(F) denotes
      what x does at the bind; a pin mentioning a per-application type is
      refused), rule 4's X(H) ⊆ X(F), and rule 2's "assuming F needs X(F)
      in force" are checked by the elaborator (bind_functor), not stated.
  23. (permissive, MossX) The platform's `Memories` members are the memory
      primitives, which MossX.mem_args lets take an `Addr` receiver (and
      source); and a `Ref`/`Array` rebuilt from a bare handle inside the
      library (GcArrays) is an unchecked cast (see note 1).
  24. (m4, implementation optimization; the relation is unchanged) Under
      PrFamily, `XApply F θ sm e` composes the family's whole result at θ at
      every application, though e reads one member: for the platform's
      `Memories` (27 members) at every memory access, about two thirds of
      the compiler's interpreted time.  The elaborator applies families
      exactly as drafted; the oracle then runs FastPath.fast_program of its
      output.  For each platform functor of primitives in main's root
      context that no bind, copy or satisfaction map moves (a syntactic
      check), that pass files it as XDFunSym h and turns each
      `XApply F θ [] e` into `XApplyBr F [(h, bs', ps)] e`, where ps is the
      pairs of inst_smap θ Dr whose requirement e may look up and bs' the
      primitives they read.  PROVED (FastPath.fast_run, bound as
      Certified.fast_path_agrees): for every program, argv, rd and fuel, the
      transformed program's run equals the original's unless the original
      reports XFuel; with CloX.engine_agrees, Certified.fast_path_engine_agrees
      covers the default pipeline.

   CHANGES THE SOUNDNESS PROOF FORCED (Track A, WorldSound.v):
  25. (relation wrong; adjusted to the elaborator) [D44] a `use` of a plain
      name that its target exports both as a symbol and as a module alias:
      ExUse gave the importer both, the elaborator (Elab.link_uses) only
      the symbol, as path resolution (`resolve`) reads the first segment.
      ExUse's alias branch now requires that t export no symbol of that
      name (`w_exp W t NSName (u_name u) = None`).  Without it no reading
      of such a bundle by the elaborator is a `WorldOf`.
  26. (elaborator support fixed, not the relation) NMap.nset walked a fixed
      64 levels, so a key of 2^64 or more was silently not stored; no
      theorem about the loader's symbol table could hold for every bundle.
      It now walks S k levels (enough for every key; identical to 64 levels
      on every key an OCaml int can hold, so the extracted oracle is
      unchanged).
  27. (decided by the designer, Track A) θ is part of the elaboration
      contract, so that elaborated programs are typable (Track B found
      elaborated_typed false for polymorphic programs without it):
      - a tag `S(e)`/`S{…}` at type TNom S xs is XTag S xs …: θ is the
        type's own arguments (ExTag, ExRecord; a unit has none);
      - a direct call of f (a defined fn, a defined attached method at
        its receiver's instantiation, a wrapper's call of its provider) is
        XCallD f (theta_at W C f) …: each of f's requirements, in w_reqs
        order, read at the site (its meaning in C, else itself),
        canonical (ExCallD, MCAttachedDefined with the receiver's
        arguments set, MPWrapped in the provider's own context);
      - `bind T = τ;` is XBindTy T τ (canonical) in a body and XBTy T τ in
        a functor body's binds (BTypeBind, BindsOf's BOTy); it was
        omitted before.
      Checked GC casts now read these annotations. Runtime type bindings
      and captured type arguments identify the generated allocation type;
      incompatible raw-handle rewrapping traps, matching native ref.cast.
      Tag annotations are canonicalized before emission, including merged
      arguments nested inside nominal applications.
  28. (decided by the designer, Track A) θ in method keys: a method
      provision is still filed under its receiver and method (mkey, in
      c_mths and c_apps: at most one per key, [D62]), but its run-time key
      carries θ, the instantiation it was made at (rkey C r m: c_apps's
      entry, its known entries, canonical).  Every elaborated key is the
      run-time key read in the context that holds or looks it up:
      XMeth's (PrIn, PrFamily after the application, LitTerm), XBindMth's
      (BMethBind: θ = the provision's AppOf), and each side of every
      satisfaction map (Satisfies: callee's needs in the callee's
      context, provisions in the caller's; the wrapper's, lambda's and
      functor binds' maps likewise), main's environment, and a linked
      functor body's result telescope (in F's own terms).  A linked body's
      method binds are filed under F's spelling of the key as well
      (`refile`; the elaborator filed a renamed receiver so already, and
      now does for θ: a body that pins a fixed input, e.g. `bind Index =
      Int;`, makes its key's θ differ from F's).  Part VI's choice_of now
      compares provisions by identity (provision_id: the key without θ),
      since θ is where a provision was made, not which one it is.
      This changes MossX's run-time keys: the oracle's stdout, stderr and
      exit status were checked identical before and after on the same
      programs as note 27's, the compiler compiling tests/wasi/tags.moss
      included, and cargo test passes with it.
  29. (elaborator, for the proof; no change in behaviour) solve_requirements
      writes a tag's or alias's requirements through set_nominal_reqs,
      which writes only if the symbol is a tag or alias and drops repeats.
      Its group holds only tags and aliases and its lists have no repeats,
      so nothing changes (oracle outputs checked identical, as in note 27);
      the guard makes the requirement cache's invariant (ElabFrame.ReqOK)
      provable step by step.
  30. (discrepancy, resolved by the designer: the elaborator now follows
      the relation, which is unchanged) Depth of contexts.
      AssumeTypes/SpecTypes (V.1) reach a context's type symbols through
      contexts nested to any depth, and allow cycles (context A naming
      context B naming A: the inductive simply has finite derivations).
      The elaborator walked contexts 64 deep (assume_type_syms ->
      walk_specs 64) and failed ("context nests too deeply") beyond that,
      including on any cycle; the self-hosted compiler (lower.nest_types)
      stopped silently 17 deep.  That cost (i) completeness, and (ii)
      WorldOf's requirement lists for symbols the elaborator never asks
      about (TODO_reqs_reading (c)) could not be had by running its own
      `reqs` on them.  The options were to bound the relation, to forbid
      cyclic contexts, or to record that completeness held only below 64.
      Decided: neither implementation bounds the depth.  Both walk an
      assume nesting depth first with the set of contexts entered so far,
      entering each context once, until nothing new appears; a cycle
      requires the union of its contexts' types.  Elab.walk_specs (fuel
      one more than the symbols, which only a bug could use up: each
      call enters a context not entered before) gives the types in the
      order the old walk did on every nesting it accepted, so requirement
      lists, and with them TNom argument order, are unchanged there
      (oracle outputs checked identical on tests/wasi, tests/errors and
      examples).  WorldSound.assume_type_syms_exact is re-proved for the
      new walk: what it computes is exactly AssumeTypes, at any depth and
      through cycles.  The same walk reads a functor's result types
      (V.5 FunOutputs: Elab.result_types, which stopped at 17, and
      lower.result_type_syms, silently at 17).
      Making a function's context (V.3 ItemEntries) walks the same
      nesting, with brackets, and there a cycle has no finite derivation:
      a body that assumes a cycle of contexts has no reading.  The
      elaborator bounded that walk at 64 and the compiler at 16.  Its fuel
      is now twice `walk_fuel`, which only a cycle uses up (Elab reports
      "contexts nest in a cycle"; the compiler stops at a context already
      being walked and reports it, e_deep), so it has no depth bound
      either and rejects exactly the cycles. The designer subsequently
      required rejecting cycles at declaration, including unused contexts:
      check_contexts now validates the declaration graph before lowering.
      The remaining D84 walks use a bound derived from the declaration
      graph's size rather than the old 16/17-level limits. Tests:
      tests/wasi/context_deep.moss (70 deep), context_shared.moss and
      tests/errors/context_cycle{,_unused,_nominal}.moss.
  31. (relation wrong, adjusted to the elaborator) Relevance (V.1).
      FreeIn's FBracket read a bracket's right side even when the head of
      `R[…]` resolves to nothing; the elaborator (free_type_syms) reads
      brackets only under a head that resolves to a symbol.  FBracket now
      asks that.  And wo_reqs_least quantified over every relevance-closed
      R, which constrains only tags and aliases, so R could make a val
      mentioned in a nominal's body (FNom) require nothing while the
      elaborator reads its fixed requirements: least is now over the R
      that agree with w_reqs on everything but tags and aliases.
  32. [D75]/[D81]: reconstructing Ref/Array from an opaque handle is now
      modeled by the native checked cast, inside or outside the selected
      library. The old directory restriction in ExTag and the oracle was
      an unsupported-case boundary for missing runtime casts, and is
      removed. Source tag typing still checks the declared payload; the
      runtime checks the allocation kind and grounded held type.


   CHANGES DECIDED BY THE DESIGNER (spec fixes):
  33. (relation and both implementations unsound; decided: rejected)
      [D15] two union members with one head, as in `A[X=I32] | A[X=I64]`
      for `tag A[X](X)`.  A match arm names a member by its head alone
      (`A(y)`), so such a union gives the arm no one payload type: the
      elaborator typed it at the last such member (its table of heads,
      Elab.synth_match, keeps the last member set under a head), the
      self-hosted compiler at the first, and the program then ran stuck
      under the oracle, or compiled to an invalid module or a wrong
      value.  TyUnion now asks that no two members
      share a head (`NoDup (map head flat)`, which implies the old
      `NoDup flat`); the elaborator reports "union members `A[X=I32]` and
      `A[X=I64]` share a head (D15)" and the compiler e_union, named by
      the head.  Only a written union is checked: a union type arises from
      nothing else (Join without an expected type asks for equal types),
      and substitution keeps heads.  PatOf's tag rules (note 35) find
      their member by head, which is now unique.  Tests:
      tests/errors/union_shared_head*.moss.
  34. (relation permissive; decided: rejected) [D15] a union member that
      is a bare record type, as in `type U = | {a: I32} | Red;`.  TyUnion
      counted `TNom rec_head` (V.0: a record's reserved head) as nominal;
      the elaborator rejected it ("not nominal (D15)") and the self-hosted
      compiler accepted it.  TyUnion now asks `n <> rec_head` of each
      member, and the compiler reports e_union, named by nothing.  Test:
      tests/errors/union_record_member.moss.
  35. (relation incomplete; adjusted to the elaborator) Patterns.  PatOf
      had no rule for a tag pattern with a payload binder (`S(y)`) or a
      record pattern (`S{x, x = y, x = _}`), so no match with one was
      related.  PTagBind binds y to S's payload at the member's
      arguments (PayloadOf): for a struct tag that is the bare record
      type `TNom rec_head ftys`, which a parameter or another tag of the
      same payload can take (tests/wasi/record_payload_*.moss).  PRecord
      binds fields by name through FieldPats, in the order written.  As
      the elaborator (compile_pattern) does: a sub-pattern of `S(…)` is
      a binder only, a field's is a binder or `_`, and `{…}` with no tag
      head is rejected, so neither has a rule.  Two more changes make
      PatOf match it: PatOf takes the frame, and a binder (PBinder,
      PTagBind, FieldPats) is the local `var_of F y`, where PBinder had
      `XPAny None` (the elaborator writes the local's number, so no
      match with a binder arm was related); and PUnit also covers a
      bare `S` for a tag with a payload, which the elaborator accepts.
      Not stated, as layouts are nowhere in the relation: the
      elaborator's layout check (layout_ok) of a bound payload.
  40. (elaborator unsound; adjusted to the relation) [D73] Listed
      parameters of a tag nothing uses.  WorldOf's wo_listed asks of every
      tag that each parameter it lists is a type its assume nesting
      requires.  The elaborator checked that only in solve_requirements,
      that is, only of the tags elaboration reaches, so it accepted a
      program with an unused `tag Bad[T](I32)` whose nesting does not
      require T, which has no reading.  It now checks every tag of the
      loader's world first (Elab.check_listed, the same listed_params).
      Oracle outputs are identical on all 243 programs and on the compiler
      compiling tests/wasi/tags.moss; the self-hosted compiler (src/) is
      unchanged and still accepts such a program.
  41. (resolved by the designer: imported main is allowed) The entry's
      scope identifies main, regardless of its declaration's spelling.
      Elaborates now permits `import "./lib.moss" use start as main;`
      where lib declares `fn start`. IsMainFn and both implementations
      give the root context to a function in scope as main in any loaded
      module, so its body is checked consistently wherever called (D16).
      Test: tests/wasi/imported_main.moss, whose imported main binds Heap.
  42. (relation incomplete and wrong; adjusted to the elaborator, which
      changes only where noted) Divergence in blocks.  The elaborator
      gives a block two types: the one its value has (`|` when its last
      statement cannot fall through: an expression of type `|`, or a
      `loop` no `break` leaves), and its type as a branch (its tail's, `()`
      without a tail), which `if` and `match` join.  The relation had only
      the first, and not for a loop:
      (a) `fn stop(): Never { loop {} }` (tests/wasi/branches_diverge.moss)
          had no reading: Blk typed a block ending in a statement `loop`
          `()`.  New rule BlkLoopLast: a block whose last statement is a
          `loop` no `break` leaves (BlockBreaks, defined structurally) is
          `|`; BlkLoop now asks that something follow or that the loop
          break, as BlkExpr does.
      (b) a branch block without a tail that cannot fall through is `()`
          to the elaborator (synth_if, synth_match use block_type) and was
          `|` to the relation, so `if c { return; } else { return; }` was
          `()` there and `|` here, and a later `match` on it (a catch-all
          arm, accepted) had no reading (ArmsSome asks st <> never).  New
          judgment Branch (an `else` and a match arm: a block has
          branch_ty, `()` without a tail, or the expected type), used by
          ExIfElse (the then-block joins at branch_ty too) and ArmsOne/
          ArmsMore; ArmsOne's type is the arm's, as the elaborator's
          one-arm match is (it joined with the expected type only from
          the second arm on).
      (c) elaborator: has_break_e/has_break_b walked 10000 deep, so a
          `break` nested deeper made its loop count as never falling
          through (and the program ran into what followed).  They are now
          structural (has_break_s for statements), BlockBreaks's mirror.
      (d) elaborator: a union type all of whose members are `|` (`N | N`
          for `type N = |;`) was YUnion [], which is_never does not see,
          while TyUnion makes it TUnion [] = never; elab_type now gives
          YNever for a union of no members.
      Oracle stdout, stderr and exit status are identical on all 243
      programs and on the compiler compiling tests/wasi/tags.moss.
  43. (elaborator unsound; adjusted to the relation) Two parameters of a
      fn with one name, `fn pick(x: I32, x: I32): I32 { x }`.  Numbering
      (V.6) numbers parameter i as i (after `this`) by name, so no
      numbering exists and such a fn has no reading; the elaborator
      accepted it (`x` read the first).  lower_body now rejects it ("is
      declared twice"), as lambda_provider already did for an anonymous
      function's parameters.  Oracle outputs identical as in note 42; the
      self-hosted compiler (src/) is unchanged.
  44. (relation incomplete; adjusted to the elaborator) A field of a bare
      record, `fn get(r: { a: I32, b: I32 }): I32 { r.b }`
      (tests/wasi/record_payload_param.moss, note 35).  A record's type is
      TNom rec_head …, which ExField's `match ot with TNom s xs => …`
      read as a tag's application and asked kind_sym W rec_head = tag,
      which never holds, so the program had no reading.  ExField now asks
      that the receiver be a tag's application whose payload is the
      record, or the record itself.
  45. (elaborator unsound; adjusted to the relation) (a) A value path
      with brackets, `let r = Red[I32 = I32];` for a unit `Red`: no rule
      reads a value path's brackets (ExUnitTag, ExVal, ExConst ask
      `AEPath pa None`), but synth_path ignored them and accepted.  It now
      rejects them ("only a type or a tag constructor takes brackets").
      (b) The escape check of a block ([D78], BlkE: no generation `bind
      F;` made may occur in the block's type) read the type's generations
      with ety_gens, which stopped 5000 deep; it is now structural, as
      Part V's gens is.  Oracle outputs identical on all 243 programs and
      on the compiler compiling tests/wasi/tags.moss.

   CHANGES OF TRACK A, PART 2 (calls, contexts, types; notes 50-59):
  50. (elaborator unsound; adjusted to the relation) A type symbol with
      empty brackets, `x: T[]` for `type T;`: TyType reads only
      `ATRef pa None`, so `T[]` has no reading, but elab_type read it as
      `T` (check_relevant and check_totality pass an empty application).
      elab_type now rejects brackets on a type symbol ("type `T` takes no
      brackets"; nonempty ones are still rejected first by check_relevant,
      with the message they had).  Test: tests/errors/type_brackets.moss;
      the self-hosted compiler still accepts it (listed as its bug in
      errors.rs and agreement.rs).  Also for the proof, with no change in
      behaviour: a union member whose head is 0 is rejected (0 is no
      symbol; TyUnion asks n <> rec_head).  Oracle outputs: see note 53.
      PROVED (BodyTy.pd_ty_holds): elab_type, app_subst and symbol_type
      against TyOf and Brackets, discharging BodySound's pd_ty.
  51. (relation loose; tightened to the elaborator, asked by Track B)
      AppOf let a method provision's instantiation range over any subset
      of its requirements.  It is now exactly those its signature mentions
      (SigMentions: FreeIn of a parameter's or the result's type, against
      w_reqs), in w_reqs order, as Elab.provision_app records it
      (signature_type_syms filters the requirement list by free_type_syms
      of the signature).  The relation only; the oracle is unchanged.
  52. (elaborator unsound; adjusted to the relation) [D43] merged_walk
      re-ran the assume walk with the merges found so far until a round
      added none, but after its 8th run it returned without checking, so a
      telescope whose merges take more rounds (each round finds that
      B(k-1) and C(k-1) are one, which makes `B(k-1).w` and `C(k-1).w` one
      key, and so B(k) and C(k) one type, in the next) was used half
      merged: its σ does not unify its entries, so it is no CtxOf and the
      body has no reading, yet a body that did not need the missing merges
      was accepted.  merged_walk now checks every round, the last too
      (eight walks and a ninth that must add nothing; a telescope that
      settles in eight is accepted as before), and rejects one that has
      not settled ("merging the context does not settle in 8 rounds";
      draft note 5 said so already).  Test:
      tests/errors/merge_unsettled.moss (eight levels); the same with
      seven is accepted as before.  The self-hosted compiler
      (lower.merged_walk) still accepts it (listed as its bug).
  53. (elaborator unsound; adjusted to the relation) [D43] unify had no
      occurs check: merging `A.gimme[Foo=X]` with `A.gimme[Foo=Box[T=X]]`
      set X to Box[T=X], a cyclic merge, which no finite type unifies and
      MostGeneralMerge (idempotent σ) excludes; a function in that context
      that never read X was accepted.  unify now rejects merging a
      generation with a type that contains it.  Test:
      tests/errors/merge_cyclic.moss; the self-hosted compiler still
      accepts it (listed as its bug).  For notes 50, 52 and 53 together,
      oracle stdout, stderr and exit status are identical on all 243
      programs of tests/wasi, tests/errors and examples and on the compiler
      compiling tests/wasi/tags.moss.
  54. (relation incomplete; adjusted to the elaborator, which is
      unchanged) [D84] (8), note 20: the elaborator forms `main`'s context
      with the platform's root context (Elab.add_root_context: lib/wasm.moss's
      `Gc`, each of its abstract functors not already in force after the
      types it is fixed in), but CtxOf had only the assumes.  So a `main`
      that assumes `Wasm, Wasi, Branch` and calls a fn assuming `Gc` (or
      binds `Heap`) had no reading, though the oracle accepts and runs it
      (exit status 0): tests/wasi/gc_slots.moss, whose `main` assumes
      `Wasm, Wasi, Branch` and calls `demo`, which assumes `Gc`; likewise
      functor_alloc, gc_ast, gc_bodiless and gc_resolution.  CtxOf now adds RootCtx's entries
      for a module's own `fn main` (IsMainFn), as the elaborator does,
      by symbol (Gc's items are read in lib/wasm.moss, where they are in
      scope); the merge, the context and the needs are over both.  The
      relation only at that stage. Note 41 now also counts an imported
      main in IsMainFn and in both implementations.
  55. (elaborator fragile; made total for the proof, no change on any
      program tried) [D43] Elab.canon followed the merges with a fuel of
      5000 that counted the type's depth as well as the merge steps, and
      returned what it had reached when the fuel ran out: a deep enough
      type, or a long enough chain of merges, came back unresolved, and a
      key or a fits read off it would not be the relation's (σ is a
      resolution).  The merges unify makes are triangular (an edge g ↦ t
      maps a generation not merged yet to a canonical type without g, by
      the occurs check of note 53), so following them from any type takes
      at most as many steps along a path as there are edges: canon's fuel
      now counts only those steps, and is the number of edges plus one,
      which never runs out (Merge.canon_go_fuel).  unify also compares a
      nominal's argument keys before zipping its arguments (one head has
      one argument list, its requirements in order; `combine` dropped a
      longer list's tail).  PROVED (Merge.v): canon is the resolution of
      the edges and is idempotent; σ = suf uf, the edges resolved, is an
      idempotent substitution with tr (canon t) = σ (tr t); whatever
      respects the edges is unchanged by σ first (resp_mg, most
      generality); unify keeps the edges triangular, adds only edges every
      unifier of its two sides respects, and when it adds none its two
      sides already resolve alike (unify_spec).
  56. (elaborator unsound; adjusted to the relation) [D71] A bracket on a
      context, val or fn item binds a type symbol among the item's
      requirements (Brackets asks it), as on a type or a bind, but
      add_spec never checked it: `context D = Int, C[T = Int, U = Int];`
      for a `C` that requires only `T` was accepted (exit status 0), and
      no CtxOf reads it.  add_spec now checks (check_relevant, the
      message elab_type's has).  Test: tests/errors/context_brackets.moss;
      the self-hosted compiler still accepts it (listed as its bug).
      (relation incomplete; adjusted to the elaborator) A method item's
      brackets: the designer's [D43] example tests/wasi/merge.moss (and
      merge_order, merge_functor, functor_merge_bracketed, ...) writes
      `A.gimme[Foo=B]` for `fn .gimme(): Foo;`, whose requirements are none
      (it is declared under no assume), so Brackets, asking Foo among them,
      gave those programs no CtxOf, though the oracle runs them (merge.moss
      prints `qbq`).  The elaborator binds any type symbol on a method item
      (and rejects it on a bind: check_relevant, as before); Brackets now
      asks the left side among the requirements only for a target that is
      not a method.  Oracle stdout, stderr and exit status for notes 55 and
      56 are identical to the previous oracle's on all 243 programs of
      tests/wasi, tests/errors and examples and on the compiler compiling
      tests/wasi/tags.moss, but for the new error test.
  57. (elaborator unsound; adjusted to the relation) [D71] Brackets on a
      context item that takes none: ItemEntries reads a type, a tag, a
      unit, an alias, a defined val and a functor item only unbracketed
      (IEType, IENothing, IEFun), but add_spec elaborated the brackets and
      add_symbol_item ignored them, so `context K = T[];` and
      `context L = Int, Box[T = Int];` were accepted (exit status 0) with no
      CtxOf.  add_spec now rejects them ("`T` takes no brackets in a
      context").  Test: tests/errors/context_item_brackets.moss; the
      self-hosted compiler still accepts it (listed as its bug).  Oracle
      stdout, stderr and exit status are identical to the previous
      oracle's on all programs and on the compiler compiling
      tests/wasi/tags.moss, but for the new error test.
      RESOLVED (designer decision): find_method_decl no longer falls back
      to a detached method in the receiver's home module. Previously, for a method
      item `A.m` of a context, falls back to a detached `.m` of A's own
      module when none is in scope here; MethodNamed (IEMeth) does not, so
      `context K = Int, A, A.gimme;` with `.gimme` detached in A's module
      and not imported is accepted (exit status 0) with no CtxOf, while a
      call `a.gimme()` in a body is rejected ("not a detached method in
      scope"). Both implementations now reject such a context, as
      MethodNamed requires. Tests: context_detached_unimported.moss and
      context_detached_imported.moss.
  58. (elaborator fragile; made robust for the proof, no change on any
      program) fn_info caches a function's info under its symbol, but
      computed it from the gsym record it was handed, so the cache was
      only as good as every caller's record: an entry under a symbol
      could have been made from another record with that number.  It
      now reads the symbol table's entry for the number (sym) before it
      walks.  PROVED (InfoCache.v): every entry of the cache is fn_info's
      merged walk of the table's entry for its key, or (lambda_provider)
      an entry for a symbol the elaborator made, and every step keeps
      this, so fn_info forms its function's context (CtxOf with the
      enclosing brackets apart, CtxMerge.CtxOfIE) whether its answer was
      cached or not, for a function neither a forwarder nor `main`.
      Oracle stdout, stderr and exit status are identical to the previous
      oracle's on all programs of tests/wasi, tests/errors and examples
      and on the compiler compiling tests/wasi/tags.moss.
  59. (discrepancy, reported, not changed: the relation reads more than the
      elaborator accepts) [D43] Merged types in a body.  CtxOf reads a
      body's types canonically (σ applied: c_ty, c_vals, a parameter's
      type), so a type symbol merged with `|` is `|` there and two merged
      type symbols are one type.  The elaborator tests divergence
      (check_stmt's `ASExpr` flag is_never, synth_if's and `return`'s
      is_unit_or_never, a match on `|`) and joins branches (join: is_never,
      ety_eqb) on its types as they are, not canonically (canon e).  So
      with `type N = |;`, `context C1 = A, B, A.gimme[Foo=B];` and
      `context C2 = A, A.gimme[Foo=N];`, the fn
      `assume Wasm, C1, C2 { fn f(x: B): I32 { x; } }` is rejected ("block
      yields `()`, expected `I32`", exit status 64) though BlkExprLast reads
      its body as `|`; and, B and C merged (as in tests/wasi/merge.moss),
      `fn pick(k: Bool, b: B, c: C): B { let r = if k { b } else { c }; r }`
      is rejected ("branches disagree: `B` vs `C`") though Join reads it.
      Every test the elaborator passes implies the canonical one, so no
      accepted program lacks a reading on this account; but where they
      differ the elaborator's statement-level divergence (`x;` is `()` to
      it, `|` to the relation) is not the relation's, which BodySound's
      StmtConcl equates.  Lifting CxRel's en_uf = [] therefore needs the
      elaborator to test canonically (is_never (canon e t), join over
      canon e): it newly accepts only such programs.  Left for that lift,
      as it changes synth/check_stmt, which the bodies track (proof-a2) is
      changing too.

   CHANGES OF TRACK A, PART 3 (fields, match, records, operators, tags,
   literals, defined vals; notes 60-68):
  60. (relation incomplete; adjusted to the elaborator) [D85] `for x in
      xs` is sugar (Parse.for_desugar) over four operators, `for.begin`,
      `for.more`, `for.get` and `for.after`, which the elaborator reads as
      access.moss's detached `.begin`/`.more`/`.get`/`.after`.  Part V's
      operator_method knew only num.moss's, so no program with a `for`
      loop had a reading, among them tests/wasi/lists.moss, iterate.moss,
      newton.moss and five more, which the oracle runs.  operator_method
      now gives the library module with the method's name, as the
      elaborator's does (the same table, BodyOps.operator_rel), and
      ExUnary/ExBinary read LangSym at that module.
  61. (elaborator, for the proof; no change in behaviour) A pattern names a
      member of the scrutinee's type by its head, and compile_pattern's
      table of heads also holds an abstract type's (a generation's or a
      `type`'s number).  A tag never shares its number with a type, but
      nothing local says so; compile_pattern now asks that the member it
      finds under a tag's or unit's head be that tag's application
      (YNom), so PUnit/PTagBind/PRecord's `In (TNom s xs) (members st)`
      follows (BodyData.pat_step).  Union members are tag and unit
      applications (BodySound.ety_wf, which the proof of elab_type
      against TyOf now owes), so the exhaustiveness check reads heads as
      ArmsSome does.
  62. (elaborator unsound; adjusted to the relation) A record literal of a
      payload that names a field twice, `tag P { a: I32, a: I32 };` and
      `P { a = one }`: ExRecord asks for as many fields as the payload
      has (`length fs = length ftys`, every field given once), the
      elaborator only that each payload field's name be given, and it
      ran the program.  synth_record now rejects it ("names a field
      twice, so no record literal builds it").  Such a type is still
      readable (TyRec), and `P(r)`, patterns and field reads of it are
      unchanged.  Test: tests/errors/record_field_twice.moss (the oracle
      ran it and exited 1; the self-hosted compiler still accepts it).
  63. (relation incomplete; adjusted to the elaborator) The literal `0`
      needs only lib/int.moss's `zero`: the elaborator types it at
      zero's type, but ExLit asked for `one` in force at the literal's
      type (or a float literal), so `assume Int, zero { fn f(): Int { 0 }
      }` was accepted (the oracle ran it) with no reading.  ExLit now also
      takes zero's type.
  64. (resolved by the designer: literal arithmetic follows families)
      [D68, D84 rule 7] LitOp and LitTerm now allow the same unique-family
      provision as method calls, including its type bindings, requirements,
      produced keys and resulting method signature. Direct arithmetic
      constructors remain valid. Both elaborators check each literal
      arithmetic method takes and returns the literal's own type; arbitrary
      selected library modules need not have a hardcoded standard signature.
      Tests: tests/wasi/literal_family.moss and the literal_signature fixture.
  65. (elaborator unsound on bundles the parser does not write; adjusted
      to the relation) [D68] lit_value and is_float_lit read a literal's
      text in the parser's normal form (Parse.lit_norm): digits, then
      optionally `.` and digits, then optionally `e`, `-` and digits.  The
      elaborator also split an exponent at `E`, and read any character as
      a digit (code - 48), so a bundle with `(Y "1E3")` (a Float at 1000;
      lit_value: not a float literal) or `(Y "/")` (one; lit_value: 0) had
      no reading.  synth_literal now rejects a text not in that form ("is
      not a numeric literal (D68)").  The oracle parses source, whose
      literals are always in that form, so it cannot be given one; outputs
      identical on all 245 programs.
  66. (elaborator, for the proof; no change in behaviour) const_val's
      cache write now only adds an entry: an initializer that reaches its
      own val recurses until the fuel runs out, so no successful run
      overwrote one.  The cache so only grows (ElabFrame's fx_consts), and
      BodySound's Later reads every entry as its val's ExConst premises
      (lt_consts, ConstFact; BodyConst.v proves pd_const).  That the last
      state of a run holds only such entries is an obligation of the
      whole run (each entry is made by const_val's fresh case, which
      BodyConst.const_fresh relates, from an earlier state), proved in
      ElabConsts.v (elab_consts: every step keeps the cache valid).
  67. (elaborator, for the proof; no change in behaviour) lang_call (a
      literal's `.add`/`.mul`/`.div`, note 64) called provided_call also
      when no provision was in force and no family produced the key, for
      provided_call to search for a family again, on a later state (fresh
      numbers), and report the missing provision.  lang_call now reports
      it itself (Elab.no_provision, the same message), so a literal's
      arithmetic is provided_call's direct case only (BodyLit.v).
  68. (resolved by the designer: library identity is the prelude's
      directory) A user module named `lib/wasm.moss` is not the selected
      library. The world now carries that directory (w_library), tied to
      b_prelude by WorldOf. LangSym selects a symbol from the scope of the
      exact sibling module `<directory>/<short>.moss`, as lang_symbol does;
      a reexport is valid, and a declaration need not live in that module.
      This scope alignment also removes the old, false generic Hlang
      assumption: int.moss itself reexports num.moss's detached `.eq`.
      LibraryFacts proves the identity/scope facts from the loader.
      PlatformRefTag follows the selected wasm scope (including deliberate
      reexports), as is_platform_sym does; InLibrary uses the same directory.
      Native primitive/operator lookup uses the same exact directory, not
      a last-basename match. Disabling automatic prelude imports retains
      the default `lib/` directory from the working directory; it does not
      disable library identity. Tests: tests/wasi/library_identity.moss,
      with a user's own lib/wasm.moss, in both native prelude modes.

   CHANGES OF TRACK B (the typing bridge; notes 70-71):
  70. (relation permissive; adjusted to the elaborator, no change in what
      is accepted) [D84] rule 2 / [D20]: assuming an abstract functor F
      asks its fixed inputs X(F) in force, as the elaborator has checked
      since m4 (add_symbol_item: "assuming `F` needs `T` in force: `F` is
      fixed in them").  IEFun did not ask it, so the relation related
      programs the elaborator rejects, and Core's OkFun (X(φ) available
      where φ is listed) could not be read off a CtxOf (Track B).  IEFun
      now asks every x with Fixed W u x to have a meaning in tm; Fixed
      and what it is built from move ahead of ItemEntries.
  71. (relation wrong; adjusted to the elaborator, no change in what an
      implementation accepts) Brackets on a context item, `C[T=U]`.  The
      elaborator (add_spec, add_symbol_item; provide_spec likewise) reads
      the items of C under the bracket substitution, which it passes down
      and drops when C is done, and puts a type item `T` of C in force as
      its own generation whether or not a bracket binds it (the env, not
      the substitution, holds the types in force).  IECtx instead walked
      C's items at `bs ++ tm` and returned the walk's tm', so the bindings
      outlived C: in `context Outer = T, U, Inner[T=U];` (with `Inner`
      under `assume T`) T meant U for the rest of the walk and in the
      context made, where the elaborator has T as its own generation; and
      IEType did not put a bracket-bound T in force.  So Part V gave such
      a context meanings the elaborator does not (typing `fn f(t: T): T`
      at U -> U, and relating `fn g(t: T): U { t }`, which the elaborator
      rejects), and keys that are no requirement (tests/wasi/generic.moss:
      T and L in `exercise`'s context).  ItemEntries/ItemsEntries now take
      the enclosing brackets `ob` apart from the types in force `tm`: an
      item is read at `ob ++ tm` (its own brackets at `bs ++ ob ++ tm`),
      IECtx walks C's items under `bs ++ ob` with tm threaded alone, and
      IEType consults and extends tm only.  CtxOf, FunCtx and
      ProvideResult start with no brackets.  The elaborator is unchanged;
      the relation's worked examples (RelSupport's tactic) bracket no
      context item, so their proofs stand.  Track B: what makes
      BRIDGE-A2's "each requirement means its own generation, and only
      requirements have meanings" true of Part V.
*)

(* ######################################################################## *)
(* Part VI. The obligations                                                  *)
(* ######################################################################## *)

(* An elaborated program as the runtime sees it: its bodies looked up by
   symbol (the first entry wins), with the command line `argv` and the
   host file system `rd` (both inputs, like the program). *)
Definition program_of (p : xprogram) (argv : list (list int)) (rd : list int -> option (list int)) : xprog :=
  mkXProg (fun f => Elaboration.nlook f (xo_fns p)) (fun u => Elaboration.nlook u (xo_funs p))
          (xo_bool p) (xo_true p) (xo_false p) argv rd (xo_cast p)
    (fun h => match Elaboration.nlook h (xo_inputs p) with Some ts => ts | None => [] end)
    (fun h => match Elaboration.nlook h (xo_exports p) with Some sm => sm | None => [] end)
    (xo_immutable p)
    (fun h => match Elaboration.nlook h (xo_fixed_inst p) with Some th => th | None => [] end)
    (fun h => match Elaboration.nlook h (xo_fixed_keys p) with Some keys => keys | None => [] end).

(* D1 as a property of the relation: which callee a method call means is
   determined by the context and the key — the case analysis of
   MethCall/Provided has at most one applicable case, and each reads a
   function (a scope, the attached table, the context's table at the key).
   A family application's θ is not part of the choice (DRAFT NOTE 7). *)
Inductive choice := CDirect (f : nat) | CProv (i : item) | CFamily (u : nat) (i : item).

(* The provision chosen is its identity, the key without θ: θ is where the
   provision was made (Part V, note 28), and for a family application the
   application's own θ (DRAFT NOTE 7). *)

Definition provision_id (it : item) : item := Elaboration.provision_id it.

Fixpoint choice_of (x : xexpr) : option choice :=
  match x with
  | XCallD f _ _ _ => Some (CDirect f)
  | XMeth _ _ it _ => Some (CProv (provision_id it))
  | XApply u _ _ (XMeth _ _ it _) => Some (CFamily u (provision_id it))
  | XApply _ _ _ rest => choice_of rest
  | _ => None
  end.

(* What Certified.v binds today.  Read the comment on each. *)
(* The front end's contract, in Ender's ProblemGrammar / parser_sound /
   parser_complete pattern: `Syntax.Derives` (Part I.2, a DRAFT for the
   designer's review) is the declarative grammar of Moss's surface syntax
   over a file's bytes, relating it to the surface AST of Part I, and the
   parser must be sound and complete for it.  FrontEnd.v and
   FrontEndComplete.v prove both for Parse.parse_moss, and Certified.v
   binds them (`FrontEndCertified <: FRONT_END`). *)
Module Type FRONT_END.
  Parameter parse : string -> Input.res Input.afile.
  Axiom parser_sound : forall s f, parse s = Input.Good f -> Syntax.Derives (list_byte_of_string s) f.
  Axiom parser_complete : forall s f, Syntax.Derives (list_byte_of_string s) f -> parse s = Input.Good f.
End FRONT_END.

(* The loader's contract: a bundle loaded from a file system `read` is
   what Loader.Bundled says (Part I.3): every module a file whose bytes
   derive its AST, each once, closed under imports and the prelude.  It
   is soundness only: a loader that fails on every input meets it (that
   one which reads and derives every file succeeds is not stated). *)
Module Type LOADER.
  Parameter load : (string -> option string) -> string -> string -> string -> Input.res Input.bundle.
  Axiom load_sound : forall read root prelude entry b,
    load read root prelude entry = Input.Good b -> Loader.Bundled read root prelude entry b.
End LOADER.

Module Type PROVED_NOW.
  (* The elaborator: a total Gallina function from the tokens of a bundle
     to a program or an error (so total and deterministic by construction),
     reading the tokens with Part I's reader. *)
  Parameter elaborate : Input.bundle -> Input.res xprogram.
  Parameter elaborate_sx : Input.sx -> Input.res xprogram.
  Parameter elaborate_sx_reads : forall x,
    elaborate_sx x = match Input.read_bundle x with
                     | Input.Good b => elaborate b
                     | Input.Bad m => Input.Bad m
                     end.

  (* Core Moss's dynamics: big-step evaluation is deterministic, and the
     fueled interpreter is adequate for it (both directions, on Ok). *)
  Parameter eval_deterministic : forall Sg d g e w1 w2,
    eval Sg d g e w1 -> eval Sg d g e w2 -> w1 = w2.
  Parameter interp_sound : forall fuel Sg d g e w,
    interp fuel Sg d g e = Ok w -> eval Sg d g e w.
  Parameter interp_complete : forall Sg d g e w,
    eval Sg d g e w -> exists fuel, interp fuel Sg d g e = Ok w.

  (* Specialization: substituting a grounding into an elaborated term's
     type annotations does not change what it evaluates to.  (The stronger
     theorems of Specialization.v — specialized code is monomorphically
     well-typed — are about the typing judgments, which are proof devices
     and stay out of this file.) *)
  Parameter spec_eval : forall Sg d ge g e w,
    eval Sg d ge (spec g e) w <-> eval Sg d ge e w.

  (* MossX, over the primitive semantics the implementation supplies. *)
  Declare Module Prims : PRIM_SEMANTICS.
  Module X := XSem Prims.

  (* On embedded core programs MossX computes exactly what Core Moss
     does, for every fuel (Ok, Err, OutOfFuel), touching neither the
     locals nor the machine. *)
  Parameter xinterp_conservative : forall fuel Sg d g e st,
    res_agree (interp fuel Sg d g e)
              (X.xr_out (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)))
    /\ X.xr_g (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)) = emb_venv g
    /\ X.xr_st (X.xinterp fuel (emb_prog Sg) (emb_denv d) (emb_venv g) st (emb_expr e)) = st.

  (* The primitives' law. *)
  Parameter prim_law : forall P p ps r ws st,
    prim_sig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
    match fst (Prims.run_prim P p ws st) with
    | XOk w => xvty P w r
    | XTrap _ | XExit _ => True
    | _ => False
    end.

  (* Track B: the same law at the memory primitives' method signatures. *)
  Parameter prim_mlaw : forall P p ps r ws st,
    prim_msig p = Some (ps, r) -> Forall2 (xvty P) ws ps ->
    match fst (Prims.run_prim P p ws st) with
    | XOk w => xvty P w r
    | XTrap _ | XExit _ => True
    | _ => False
    end.

  (* Track B: a primitive the model does not implement stops the program
     (XUns), whatever its arguments. *)
  Parameter prim_ulaw : forall P c ws st,
    fst (Prims.run_prim P (PUnsupported c) ws st) = XUns c.

  (* Core safety, through a typing predicate the implementation chooses.
     `Typed` is a proof device: an auditor need not read its definition.
     What makes these two obligations worth anything is (a) the pending
     `elaborated_typed` of COMPLETE, which ties `Typed` to elaboration, and
     (b) `typed_inhabited`, which rules out `Typed := False`: a program with
     functors and an existential result is Typed.  By themselves they say
     nothing about any Moss program. *)
  Parameter Typed : gsig -> fsym -> Prop.
  Parameter typed_safe : forall Sg main body fuel,
    Typed Sg main -> g_fn Sg main = Some ([], mkSig [] TUnit, Some body) ->
    interp fuel Sg [] [] body <> Err.
  Parameter typed_inhabited : exists Sg main u,
    Typed Sg main /\ g_fun Sg u <> None.

  (* D1, search-free resolution, as a property of the elaboration relation:
     two elaborations of one method call in one context choose one callee. *)
  Parameter method_choice_unique : forall b W P F C ox ot pa args x1 t1 x2 t2,
    Elaboration.WorldOf b W ->
    Elaboration.MethCall W P F C ox ot pa args x1 t1 ->
    Elaboration.MethCall W P F C ox ot pa args x2 t2 ->
    choice_of x1 = choice_of x2.
End PROVED_NOW.

(* The whole contract.  Certified.v does NOT bind this yet; the obligations
   below PROVED_NOW are pending (see Certified.v for the list). *)
Module Type COMPLETE.
  Include PROVED_NOW.

  (* The elaborator is sound and complete for the (DRAFT) relation. *)
  Parameter elaborate_sound : forall b p,
    elaborate b = Input.Good p -> Elaboration.Elaborates b p.
  Parameter elaborate_complete : forall b,
    (exists p, Elaboration.Elaborates b p) -> exists p, elaborate b = Input.Good p.

  (* Accepted programs never go wrong: whatever the command line, the file
     system, the initial machine and the fuel, running an accepted
     program's main never gets stuck (XErr).  It may trap, exit, run out of
     fuel, or hit an unmodelled primitive (XUns). *)
  Parameter accepted_never_err : forall b p,
    elaborate b = Input.Good p ->
    forall fuel argv rd st0 c,
      X.xr_out (X.run_main fuel (program_of p argv rd) (xo_main p) (xo_env p) st0) <> XErr c.

  (* The bridge the proof of accepted_never_err is expected to go through
     on the core fragment: an elaborated program whose bodies are all core
     terms is Typed. *)
  Parameter elaborated_typed : forall b p,
    elaborate b = Input.Good p ->
    (forall f x, In (f, x) (xo_fns p) -> exists e, x = emb_expr e) ->
    exists Sg, Typed Sg (xo_main p) /\
      forall f x, Elaboration.nlook f (xo_fns p) = Some x ->
        exists D S e, g_fn Sg f = Some (D, S, Some e) /\ emb_expr e = x.
End COMPLETE.

(* Approved continuation alignment: float literal conversion uses the same
   checked argument subtyping as an ordinary call. LitFloat records the
   selected parameter type and the actual fits premise; it does not assume
   every custom library declares an exactly equal Int parameter. *)
