(* Audit.v — source-to-program and program-to-world semantics.
   The public build/run interface and its five obligations appear at the end.
   All generic definitions of Builds and Runs are in this file:
     I. Source syntax and module loading.
     II. Elaborated programs and runtime structures.
     III. Reference execution.
     IV. Base primitive signatures (in AuditBase.v).
     V. Declarative elaboration.
   Only Rocq's standard library is imported. *)
From Stdlib Require Import PrimInt63 ZArith.
From Stdlib Require Import String Ascii List Bool Arith.
From Stdlib Require Strings.Byte.
Import ListNotations.

(* ######################################################################## *)
(* Part I. The input                                                         *)
(* ######################################################################## *)

(* The surface AST and the parsed bundle consumed by elaboration. *)
Module Input.
Local Open Scope string_scope.

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

(* A type equation `τ = τ` ([D90]): both sides are type references (a
   path and its brackets), and the order of the sides does not matter. *)
(* An assume item: `p` or `R.m`, or an equation. *)
Inductive aitem : Type :=
| AIHead : path * option name -> aitem
| AIEq   : aty -> aty -> aitem.

(* A context item: a spec, or an equation. *)
Inductive citem : Type :=
| CISpec : spec -> citem
| CIEq   : aty -> aty -> citem.

Inductive adecl : Type :=
| ADAssume  : list aitem -> list adecl -> adecl
| ADType    : name -> adecl
| ADAlias   : name -> aty -> adecl
| ADTag     : name -> list name -> aty -> adecl
| ADUnit    : name -> adecl
| ADVal     : name -> aty -> option aexpr -> adecl
| ADFn      : fnrecv -> name -> list (name * aty) -> option aty -> option ablock -> adecl
| ADCtx     : name -> list citem -> adecl
(* brackets P(F) (D84 rule 1; None: unbracketed, the 61b7b76 syntax), args,
   result, and the body: None for an abstract functor (D78) *)
| ADFunctor : name -> option (list name) -> list spec -> list spec
              -> option (list (list (spec * option aexpr))) -> adecl.

(* [D90], rule B: the references an equation's side writes, each as an
   unbracketed item: its head, and the right sides of its brackets (not
   their left sides, which a bracket binds).  The type symbols among them
   are the equation's own requirements (SpecTypes); what a nominal it
   names requires must already be in force, as for any context item
   ([D20]). *)
Fixpoint spec_heads (s : spec) : list spec :=
  match s with
  | Spec (Some pa) None a =>
      Spec (Some pa) None None ::
      match a with
      | Some bs => (fix go (l : list (path * spec)) : list spec :=
                      match l with [] => [] | b :: r => (spec_heads (snd b) ++ go r)%list end) bs
      | None => []
      end
  | _ => []
  end.
Definition ty_heads (t : aty) : list spec :=
  match t with ATRef pa a => spec_heads (Spec (Some pa) None a) | _ => [] end.

(* What a context or assume item names, as specs: an equation names its
   sides' references. *)
Definition citem_specs (it : citem) : list spec :=
  match it with CISpec s => [s] | CIEq l r => (ty_heads l ++ ty_heads r)%list end.
Definition aitem_specs (it : aitem) : list spec :=
  match it with
  | AIHead (pa, None) => [Spec (Some pa) None None]
  | AIHead (_, Some _) => []
  | AIEq l r => (ty_heads l ++ ty_heads r)%list
  end.

(* A context's spec items, without its equations. *)
Definition ctx_specs (its : list citem) : list spec :=
  flat_map (fun it => match it with CISpec s => [s] | CIEq _ _ => [] end) its.

(* An import's `use` item: (dotted, name), and its alias if any. *)
Record ause := AUse { u_dot : bool; u_name : name; u_alias : option (bool * name) }.

Record aimport := AImport {
  i_path  : string;
  i_alias : option name;
  i_glob  : bool;
  i_uses  : list ause
}.

Record afile := AFile { f_imports : list aimport; f_decls : list adecl }.

Record amodule := AModule {
  am_path : string; am_prelude : bool; am_targets : list string; am_file : afile
}.

Record bundle := Bundle { b_entry : string; b_prelude : string; b_mods : list amodule }.

End Input.


(* ######################################################################## *)
(* Part I.2  The concrete syntax: `Derives`, the front end's specification   *)
(* ######################################################################## *)

(* DRAFT, for the designer's review.  `Derives bytes f` says that the source
   file `bytes` has the surface AST `f` (above).  It is the specification
   used by Builds. FrontEnd.v and FrontEndComplete.v prove the executable
   lexer and parser sound and complete for it.

   It is in two layers:

   1. Lexical structure (`Lexes`): the bytes are a sequence of whitespace,
      comments and tokens, each token the longest that fits (`Lexeme`).
   2. A grammar over tokens, one relation per nonterminal, each named after
      the nonterminal of the thesis proposal's grammar figure (proposal.typ,
      <grammar>) that it refines, with that figure's letter: File F, Import
      m, Use u, Path p, Head h, Spec s, Type τ, Declaration d,
      Function f, Block b, Statement i, Bind w, Binding β, Expression e,
      Binary ⊕, Unary ⊖, Conditional c, Arm a, Pattern π.

   HOW TO READ A RULE.  A grammar relation `X ts v rest` says: the token
   list `ts` begins with an X whose AST is `v`, and `rest` is what follows
   it (so `ts` = the X's tokens ++ `rest`).  A rule's premises read left to
   right as the production does: `Tk LET ts0 ts1` is the token `let`,
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

(* A token's kind, named as lex.py's `Kind` is: a name, a literal, a
   keyword (`KEYWORDS`), a symbol (`one_char`, `two_char`), or the end. *)
Inductive token_kind :=
| NAME | STRING | INTEGER | DECIMAL
| AS | ASSUME | FUNCTOR | BIND | BREAK | CONTEXT | ELSE | FN | FOR | IF | IMPORT
| LET | LOOP | MATCH | RETURN | TAG | THIS_TYPE | THIS | TYPE | USE | VAL | VAR
| WHILE | IN | UNDERSCORE
| EXCLAM | PERCENT | AMPERSAND | LPAREN | RPAREN | STAR | PLUS | COMMA | HYPHEN
| DOT | SLASH | COLON | SEMI | LESS | EQUAL | GREATER | LBRACKET | RBRACKET
| CARET | LBRACE | PIPE | RBRACE
| EXCLAM_EQUAL | COLON_COLON | EQUAL_GREATER | LESS_LESS | LESS_EQUAL
| EQUAL_EQUAL | GREATER_EQUAL | GREATER_GREATER | HYPHEN_GREATER
| EOF.

(* Kinds are compared by their position in the list above. *)
Definition token_kind_index (k : token_kind) : nat :=
  match k with
  | NAME => 0 | STRING => 1 | INTEGER => 2 | DECIMAL => 3
  | AS => 4 | ASSUME => 5 | FUNCTOR => 6 | BIND => 7 | BREAK => 8 | CONTEXT => 9
  | ELSE => 10 | FN => 11 | FOR => 12 | IF => 13 | IMPORT => 14 | LET => 15
  | LOOP => 16 | MATCH => 17 | RETURN => 18 | TAG => 19 | THIS_TYPE => 20
  | THIS => 21 | TYPE => 22 | USE => 23 | VAL => 24 | VAR => 25 | WHILE => 26
  | IN => 27 | UNDERSCORE => 28
  | EXCLAM => 29 | PERCENT => 30 | AMPERSAND => 31 | LPAREN => 32 | RPAREN => 33
  | STAR => 34 | PLUS => 35 | COMMA => 36 | HYPHEN => 37 | DOT => 38 | SLASH => 39
  | COLON => 40 | SEMI => 41 | LESS => 42 | EQUAL => 43 | GREATER => 44
  | LBRACKET => 45 | RBRACKET => 46 | CARET => 47 | LBRACE => 48 | PIPE => 49
  | RBRACE => 50
  | EXCLAM_EQUAL => 51 | COLON_COLON => 52 | EQUAL_GREATER => 53 | LESS_LESS => 54
  | LESS_EQUAL => 55 | EQUAL_EQUAL => 56 | GREATER_EQUAL => 57
  | GREATER_GREATER => 58 | HYPHEN_GREATER => 59
  | EOF => 60
  end.
Definition token_kind_eqb (a b : token_kind) : bool :=
  Nat.eqb (token_kind_index a) (token_kind_index b).

(* A token: its kind, its text, and its byte offset (only error messages
   use it). *)
Record token := Tok { tk : token_kind; tx : string; toff : nat }.

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
Definition KEYWORDS : list (string * token_kind) :=
  [("as", AS); ("assume", ASSUME); ("functor", FUNCTOR); ("bind", BIND);
   ("break", BREAK); ("context", CONTEXT); ("else", ELSE); ("fn", FN);
   ("for", FOR); ("if", IF); ("import", IMPORT); ("let", LET);
   ("loop", LOOP); ("match", MATCH); ("return", RETURN); ("tag", TAG);
   ("This", THIS_TYPE); ("this", THIS); ("type", TYPE); ("use", USE);
   ("val", VAL); ("var", VAR); ("while", WHILE); ("in", IN);
   (* (f) `_` alone is a word of its own, the wildcard pattern, and not a
      name (`_x` and `x_` are names) *)
   ("_", UNDERSCORE)].

Fixpoint assoc {B : Type} (k : string) (l : list (string * B)) : option B :=
  match l with [] => None | (a, b) :: r => if String.eqb a k then Some b else assoc k r end.

(* The symbols: one character, and two (the longer always wins). *)
Definition one_char (b : byte) : option token_kind :=
  match b with
  | "!"%byte => Some EXCLAM | "%"%byte => Some PERCENT | "&"%byte => Some AMPERSAND
  | "("%byte => Some LPAREN | ")"%byte => Some RPAREN | "*"%byte => Some STAR
  | "+"%byte => Some PLUS | ","%byte => Some COMMA | "-"%byte => Some HYPHEN
  | "."%byte => Some DOT | "/"%byte => Some SLASH | ":"%byte => Some COLON
  | ";"%byte => Some SEMI | "<"%byte => Some LESS | "="%byte => Some EQUAL
  | ">"%byte => Some GREATER | "["%byte => Some LBRACKET | "]"%byte => Some RBRACKET
  | "^"%byte => Some CARET | "{"%byte => Some LBRACE | "|"%byte => Some PIPE
  | "}"%byte => Some RBRACE
  | _ => None
  end.

Definition two_char (a b : byte) : option token_kind :=
  match a, b with
  | "!"%byte, "="%byte => Some EXCLAM_EQUAL | ":"%byte, ":"%byte => Some COLON_COLON
  | "="%byte, ">"%byte => Some EQUAL_GREATER | "<"%byte, "<"%byte => Some LESS_LESS
  | "<"%byte, "="%byte => Some LESS_EQUAL | "="%byte, "="%byte => Some EQUAL_EQUAL
  | ">"%byte, "="%byte => Some GREATER_EQUAL | ">"%byte, ">"%byte => Some GREATER_GREATER
  | "-"%byte, ">"%byte => Some HYPHEN_GREATER
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
Inductive Lexeme : list byte -> token_kind -> list byte -> Prop :=
| LxSym2 : forall a b k rest, two_char a b = Some k -> Lexeme [a; b] k rest
| LxSym1 : forall a k rest, one_char a = Some k ->
    (forall b r, rest = b :: r -> two_char a b = None) -> Lexeme [a] k rest
(* a name or keyword (`in` is a keyword, of `for`) *)
| LxWord : forall c ns rest,
    is_name_start c = true -> forallb is_name_cont ns = true -> starts is_name_cont rest = false ->
    Lexeme (c :: ns) (match assoc (string_of_list_byte (c :: ns)) KEYWORDS with
                      | Some k => k | None => NAME end) rest
| LxString : forall body rest, StrBody body ->
    Lexeme (""""%byte :: body ++ [""""%byte]) STRING rest
(* digits, then the optional fraction and exponent; a numeric literal may
   not run into a letter, digit or `_`, and a digit run followed by `.` and
   a digit takes the fraction *)
| LxNumber : forall d ds frac ex rest,
    forallb is_digit (d :: ds) = true -> Fraction frac -> Exponent ex ->
    starts is_name_cont rest = false ->
    (frac = [] -> ex = [] -> forall c r, rest = "."%byte :: c :: r -> is_digit c = false) ->
    Lexeme (d :: ds ++ frac ++ ex)
           (match frac, ex with [], [] => INTEGER | _, _ => DECIMAL end) rest.

(* `Lexes i cs ts`: the bytes `cs`, starting at offset `i`, are the tokens
   `ts`: whitespace and comments (`#` to the end of the line) between
   tokens, and the token EOF at the end. *)
Inductive Lexes : nat -> list byte -> list token -> Prop :=
| LexEnd : forall i, Lexes i [] [Tok EOF "" i]
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
Definition eof_tok : token := Tok EOF "" 0.
Definition peek (ts : list token) : token := match ts with t :: _ => t | [] => eof_tok end.
Fixpoint peekn (n : nat) (ts : list token) : token :=
  match ts with
  | [] => eof_tok
  | [t] => t
  | t :: r => match n with 0 => t | S m => peekn m r end
  end.
Definition kind (ts : list token) : token_kind := tk (peek ts).
Definition kind_at (n : nat) (ts : list token) : token_kind := tk (peekn n ts).
Definition advance (ts : list token) : list token :=
  match ts with [_] => ts | _ :: r => r | [] => [] end.

(* `Tk k ts ts'`: a token of kind k, then ts'.  `TkText k ts x ts'`: the
   same, whose text is x; `TkName` for a name. *)
Definition Tk (k : token_kind) (ts ts' : list token) : Prop := kind ts = k /\ ts' = advance ts.
Definition TkText (k : token_kind) (ts : list token) (x : string) (ts' : list token) : Prop :=
  kind ts = k /\ tx (peek ts) = x /\ ts' = advance ts.
Definition TkName := TkText NAME.
(* An optional token. *)
Definition OptTk (k : token_kind) (ts ts' : list token) : Prop :=
  Tk k ts ts' \/ (kind ts <> k /\ ts' = ts).

(* List[X]: X, separated by `,`, with an optional trailing comma, up to
   (not including) one of the `closers`. *)
Inductive CommaList {A : Type} (X : list token -> A -> list token -> Prop) (closers : list token_kind)
  : list token -> list A -> list token -> Prop :=
| CLNil : forall ts, In (kind ts) closers -> CommaList X closers ts [] ts
| CLLast : forall ts x ts1, ~ In (kind ts) closers -> X ts x ts1 -> kind ts1 <> COMMA ->
    CommaList X closers ts [x] ts1
| CLCons : forall ts x ts1 ts2 xs ts3, ~ In (kind ts) closers -> X ts x ts1 -> Tk COMMA ts1 ts2 ->
    CommaList X closers ts2 xs ts3 -> CommaList X closers ts (x :: xs) ts3.

(* ===================== Names: Path p, Head h, Spec s ====================== *)

(* p ::= x | p::x *)
Inductive Path : list token -> path -> list token -> Prop :=
| PathOne : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> COLON_COLON -> Path ts [x] ts1
| PathMore : forall ts x ts1 ts2 p ts3, TkName ts x ts1 -> Tk COLON_COLON ts1 ts2 ->
    Path ts2 p ts3 -> Path ts (x :: p) ts3.

(* h ::= p | p.x | .x *)
Inductive Head : list token -> option path * option name -> list token -> Prop :=
| HeadPath : forall ts p ts1, Path ts p ts1 -> kind ts1 <> DOT -> Head ts (Some p, None) ts1
| HeadMeth : forall ts p ts1 ts2 x ts3, Path ts p ts1 -> Tk DOT ts1 ts2 -> TkName ts2 x ts3 ->
    Head ts (Some p, Some x) ts3
| HeadDot : forall ts ts1 x ts2, Tk DOT ts ts1 -> TkName ts1 x ts2 -> Head ts (None, Some x) ts2.

(* s ::= h | h[p = s, ...]   (the brackets are `App`, optional) *)
Inductive Spec_ : list token -> spec -> list token -> Prop :=
| SpecG : forall ts p d ts1 a ts2, Head ts (p, d) ts1 -> App ts1 a ts2 -> Spec_ ts (Input.Spec p d a) ts2
with App : list token -> sapp -> list token -> Prop :=
| AppNone : forall ts, kind ts <> LBRACKET -> App ts None ts
| AppSome : forall ts ts1 bs ts2 ts3, Tk LBRACKET ts ts1 ->
    CommaList AppBinding [RBRACKET] ts1 bs ts2 -> Tk RBRACKET ts2 ts3 -> App ts (Some bs) ts3
with AppBinding : list token -> path * spec -> list token -> Prop :=
| AppBindingG : forall ts p ts1 ts2 s ts3, Path ts p ts1 -> Tk EQUAL ts1 ts2 -> Spec_ ts2 s ts3 ->
    AppBinding ts (p, s) ts3.

(* ===================== Types τ ============================================ *)

(* τ ::= r | This | () | (τ) | {x: τ, ...} | τ | τ | `|`
   Refined: a type is a bare `|` (the uninhabited type), or one or more
   atoms separated by `|`, optionally preceded by `|`; several atoms are a
   union, one is itself. *)
Inductive Type_ : list token -> aty -> list token -> Prop :=
| TyNever : forall ts ts1, Tk PIPE ts ts1 ->
    ~ In (kind ts1) [NAME; THIS_TYPE; LPAREN; LBRACE] -> Type_ ts ATNever ts1
| TyMembers : forall ts ts1 ms ts2, OptTk PIPE ts ts1 -> Members ts1 ms ts2 ->
    Type_ ts (match ms with [m] => m | _ => ATUnion ms end) ts2
with Members : list token -> list aty -> list token -> Prop :=
| MemOne : forall ts t ts1, TypeAtom ts t ts1 -> kind ts1 <> PIPE -> Members ts [t] ts1
| MemMore : forall ts t ts1 ts2 ms ts3, TypeAtom ts t ts1 -> Tk PIPE ts1 ts2 ->
    Members ts2 ms ts3 -> Members ts (t :: ms) ts3
with TypeAtom : list token -> aty -> list token -> Prop :=
| TARef : forall ts p ts1 a ts2, Path ts p ts1 -> App ts1 a ts2 -> TypeAtom ts (ATRef p a) ts2
| TAThis : forall ts ts1, Tk THIS_TYPE ts ts1 -> TypeAtom ts ATThis ts1
| TAUnit : forall ts ts1 ts2, Tk LPAREN ts ts1 -> Tk RPAREN ts1 ts2 -> TypeAtom ts ATUnit ts2
| TAParen : forall ts ts1 t ts2 ts3, Tk LPAREN ts ts1 -> Type_ ts1 t ts2 -> Tk RPAREN ts2 ts3 ->
    TypeAtom ts t ts3
| TARecord : forall ts t ts1, RecordType ts t ts1 -> TypeAtom ts t ts1
with RecordType : list token -> aty -> list token -> Prop :=
| RecordTypeG : forall ts ts1 fs ts2 ts3, Tk LBRACE ts ts1 ->
    CommaList TypeField [RBRACE] ts1 fs ts2 -> Tk RBRACE ts2 ts3 -> RecordType ts (ATRec fs) ts3
with TypeField : list token -> name * aty -> list token -> Prop :=
| TypeFieldG : forall ts x ts1 ts2 t ts3, TkName ts x ts1 -> Tk COLON ts1 ts2 -> Type_ ts2 t ts3 ->
    TypeField ts (x, t) ts3.

(* (`:` τ)? *)
Inductive OptType : list token -> option aty -> list token -> Prop :=
| OptTypeNone : forall ts, kind ts <> COLON -> OptType ts None ts
| OptTypeSome : forall ts ts1 t ts2, Tk COLON ts ts1 -> Type_ ts1 t ts2 -> OptType ts (Some t) ts2.

(* ===================== Patterns π ========================================= *)

(* π ::= _ | p | p(π) | {x = π, ...} | p{x = π, ...}   (`= π` optional);
   `_` is its own token, not a name (f) *)
Inductive Pattern : list token -> apat -> list token -> Prop :=
| PatWild : forall ts ts1, Tk UNDERSCORE ts ts1 -> Pattern ts APWild ts1
(* a path not followed by `(` or `{` *)
| PatPath : forall ts p ts1, Path ts p ts1 -> ~ In (kind ts1) [LBRACE; LPAREN] ->
    Pattern ts (APPath p) ts1
| PatTag : forall ts p ts1 ts2 q ts3 ts4, Path ts p ts1 -> Tk LPAREN ts1 ts2 ->
    Pattern ts2 q ts3 -> Tk RPAREN ts3 ts4 -> Pattern ts (APTag p q) ts4
| PatRecord : forall ts p ts1 fs ts2, Path ts p ts1 -> RecordPattern ts1 fs ts2 ->
    Pattern ts (APRec (Some p) fs) ts2
| PatAnon : forall ts fs ts1, RecordPattern ts fs ts1 -> Pattern ts (APRec None fs) ts1
with RecordPattern : list token -> list (name * option apat) -> list token -> Prop :=
| RecordPatternG : forall ts ts1 fs ts2 ts3, Tk LBRACE ts ts1 ->
    CommaList PatField [RBRACE] ts1 fs ts2 -> Tk RBRACE ts2 ts3 -> RecordPattern ts fs ts3
with PatField : list token -> name * option apat -> list token -> Prop :=
| PatFieldShort : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> EQUAL -> PatField ts (x, None) ts1
| PatFieldLong : forall ts x ts1 ts2 q ts3, TkName ts x ts1 -> Tk EQUAL ts1 ts2 ->
    Pattern ts2 q ts3 -> PatField ts (x, Some q) ts3.

(* ===================== Operators: Binary ⊕, Unary ⊖ ======================= *)

(* ⊕: the token kinds of the binary operators, and the operator each is. *)
Definition BINARY : list (token_kind * string) :=
  [(STAR, "*"); (SLASH, "/"); (PERCENT, "%"); (PLUS, "+"); (HYPHEN, "-");
   (LESS_LESS, "<<"); (GREATER_GREATER, ">>"); (EQUAL_EQUAL, "==");
   (EXCLAM_EQUAL, "!="); (LESS, "<"); (GREATER, ">"); (LESS_EQUAL, "<=");
   (GREATER_EQUAL, ">="); (AMPERSAND, "&"); (CARET, "^"); (PIPE, "|")].
(* D68's classes *)
Definition FRACTION := ["/"; "%"].
Definition ADDITIVE := ["+"; "-"].
Definition SHIFT := ["<<"; ">>"].
Definition COMPARISON := ["=="; "!="; "<"; ">"; "<="; ">="].
Definition BITWISE := ["&"; "^"; "|"].
Definition mem (s : string) (l : list string) : bool := existsb (String.eqb s) l.
(* the binary operator next, if any *)
Fixpoint kind_assoc (k : token_kind) (l : list (token_kind * string)) : option string :=
  match l with [] => None | (a, b) :: r => if token_kind_eqb a k then Some b else kind_assoc k r end.
Definition binop (ts : list token) : option string := kind_assoc (kind ts) BINARY.
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

(* D89: a bracket application on a function call, `f[A = T](e1, ..., en)`,
   is the call run under those binds, its arguments evaluated first, in
   the caller's terms: `{ let %arg0 = e1; ...; bind A = T; f(%arg0, ...) }`.
   `%` starts no name a program can write.  Elaboration reads the
   expansion (Elaboration.ExCallBr); a tag's application is not expanded. *)
Definition call_arg_name (i : nat) : name := "%arg" ++ show_nat i.
Definition call_block (p : path) (a : list (path * spec)) (args : list aexpr) : aexpr :=
  let names := map call_arg_name (seq 0 (List.length args)) in
  AEBlock (ABlock (List.app (map (fun na => ASLet (fst na) None (snd na)) (combine names args))
                   [ASBind (map (fun b => (Spec (Some (fst b)) None None,
                                           Some (match snd b with
                                                 | Spec (Some rp) _ ra => AEPath rp ra
                                                 | _ => AEUnit
                                                 end))) a)])
                  (Some (AECall p None (map (fun n => AEPath [n] None) names)))).

(* (e) D70: at `(`, a lambda is next: `()` or `(x)` followed by `=>` or
   `:`, or `(x` followed by `:` or `,`. *)
Definition at_lambda (ts : list token) : bool :=
  let arrow k := token_kind_eqb k EQUAL_GREATER || token_kind_eqb k COLON in
  if token_kind_eqb (kind_at 1 ts) RPAREN then arrow (kind_at 2 ts)
  else if token_kind_eqb (kind_at 1 ts) NAME then
    if token_kind_eqb (kind_at 2 ts) COLON || token_kind_eqb (kind_at 2 ts) COMMA then true
    else token_kind_eqb (kind_at 2 ts) RPAREN && arrow (kind_at 3 ts)
  else false.

(* D68: `-` before a literal with a method call or field (`-1.abs()`) is
   refused: `-` binds looser than `.`, and nobody means `-(1.abs())`. *)
Definition neg_literal_dot (ts : list token) : Prop :=
  In (kind ts) [INTEGER; DECIMAL] /\ kind_at 1 ts = DOT.

(* A statement `x = e;` is next. *)
Definition assign_ahead (ts : list token) : Prop := kind ts = NAME /\ kind_at 1 ts = EQUAL.

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
| EMatch : forall nr ts ts1 s ts2 ts3 arms ts4 ts5, Tk MATCH ts ts1 -> Expr true ts1 s ts2 ->
    Tk LBRACE ts2 ts3 -> Arms ts3 arms ts4 -> Tk RBRACE ts4 ts5 -> Expr nr ts (AEMatch s arms) ts5
| EReturnNone : forall nr ts ts1, Tk RETURN ts ts1 -> In (kind ts1) [SEMI; COMMA; RBRACE; RPAREN] ->
    Expr nr ts (AEReturn None) ts1
| EReturn : forall nr ts ts1 e ts2, Tk RETURN ts ts1 -> ~ In (kind ts1) [SEMI; COMMA; RBRACE; RPAREN] ->
    Expr nr ts1 e ts2 -> Expr nr ts (AEReturn (Some e)) ts2
| EBreak : forall nr ts ts1, Tk BREAK ts ts1 -> Expr nr ts AEBreak ts1
| EOperators : forall nr ts e ts1, Operators nr ts e ts1 -> Expr nr ts e ts1

(* c ::= if e b (else b | else c)? *)
with Cond : list token -> aexpr -> list token -> Prop :=
| CondIf : forall ts ts1 c ts2 t ts3, Tk IF ts ts1 -> Expr true ts1 c ts2 -> Block ts2 t ts3 ->
    kind ts3 <> ELSE -> Cond ts (AEIf c t None) ts3
| CondElseIf : forall ts ts1 c ts2 t ts3 ts4 e ts5, Tk IF ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 t ts3 -> Tk ELSE ts3 ts4 -> Cond ts4 e ts5 -> Cond ts (AEIf c t (Some e)) ts5
| CondElse : forall ts ts1 c ts2 t ts3 ts4 b ts5, Tk IF ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 t ts3 -> Tk ELSE ts3 ts4 -> Block ts4 b ts5 -> Cond ts (AEIf c t (Some (AEBlock b))) ts5

(* a ::= π => e, | π => b ,?   — and the last arm's `,` is optional in
   both forms; an arm body starting with `{` is the block form (b). *)
with Arms : list token -> list (apat * aexpr) -> list token -> Prop :=
| ArmsEnd : forall ts, kind ts = RBRACE -> Arms ts [] ts
| ArmsCons : forall ts p ts1 ts2 body ts3 arms ts4, Pattern ts p ts1 -> Tk EQUAL_GREATER ts1 ts2 ->
    ArmBody ts2 body ts3 -> Arms ts3 arms ts4 -> Arms ts ((p, body) :: arms) ts4
with ArmBody : list token -> aexpr -> list token -> Prop :=
| ArmBlock : forall ts b ts1 ts2, Block ts b ts1 -> OptTk COMMA ts1 ts2 -> ArmBody ts (AEBlock b) ts2
| ArmExpr : forall ts e ts1 ts2, kind ts <> LBRACE -> Expr false ts e ts1 -> Tk COMMA ts1 ts2 ->
    ArmBody ts e ts2
| ArmExprLast : forall ts e ts1, kind ts <> LBRACE -> Expr false ts e ts1 -> kind ts1 = RBRACE ->
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
| UnaryNeg : forall nr ts ts1 u ts2, Tk HYPHEN ts ts1 -> ~ neg_literal_dot ts1 -> Unary nr ts1 u ts2 ->
    Unary nr ts (AEUnary "-" u) ts2
| UnaryNot : forall nr ts ts1 u ts2, Tk EXCLAM ts ts1 -> Unary nr ts1 u ts2 ->
    Unary nr ts (AEUnary "!" u) ts2
| UnaryPostfix : forall nr ts p ts1 e ts2, Primary nr ts p ts1 -> Suffixes p ts1 e ts2 -> Unary nr ts e ts2

(* e.p(e, ...) | e.x: suffixes, as many as follow *)
with Suffixes : aexpr -> list token -> aexpr -> list token -> Prop :=
| SufEnd : forall e ts, kind ts <> DOT -> Suffixes e ts e ts
| SufMethod : forall e ts ts1 p ts2 ts3 args ts4 ts5 e' ts6, Tk DOT ts ts1 -> Path ts1 p ts2 ->
    Tk LPAREN ts2 ts3 -> CommaList (Expr false) [RPAREN] ts3 args ts4 -> Tk RPAREN ts4 ts5 ->
    Suffixes (AEMeth e p args) ts5 e' ts6 -> Suffixes e ts e' ts6
| SufField : forall e ts ts1 x ts2 e' ts3, Tk DOT ts ts1 -> Path ts1 [x] ts2 -> kind ts2 <> LPAREN ->
    Suffixes (AEField e x) ts2 e' ts3 -> Suffixes e ts e' ts3

(* (e) | () | this | b | int | dec | r | r(e, ...) | r{x = e, ...}, and
   the lambda (x: τ, ...): τ => e (D70), which the figure lists under β *)
with Primary : bool -> list token -> aexpr -> list token -> Prop :=
| PrimLambda : forall nr ts e ts1, Lambda ts e ts1 -> Primary nr ts e ts1
| PrimUnit : forall nr ts ts1 ts2, Tk LPAREN ts ts1 -> at_lambda ts = false -> Tk RPAREN ts1 ts2 ->
    Primary nr ts AEUnit ts2
| PrimParen : forall nr ts ts1 e ts2 ts3, Tk LPAREN ts ts1 -> at_lambda ts = false ->
    Expr false ts1 e ts2 -> Tk RPAREN ts2 ts3 -> Primary nr ts e ts3
| PrimThis : forall nr ts ts1, Tk THIS ts ts1 -> Primary nr ts AEThis ts1
| PrimLit : forall nr ts, In (kind ts) [INTEGER; DECIMAL] ->
    Primary nr ts (AELit (lit_norm (tx (peek ts)))) (advance ts)
| PrimString : forall nr ts, kind ts = STRING ->
    string_utf8_valid (string_value (tx (peek ts))) = true ->
    Primary nr ts (string_desugar (string_value (tx (peek ts)))) (advance ts)
| PrimBlock : forall ts b ts1, Block ts b ts1 -> Primary false ts (AEBlock b) ts1
| PrimRef : forall nr ts p ts1 a ts2, Path ts p ts1 -> App ts1 a ts2 -> kind ts2 <> LPAREN ->
    (nr = true \/ kind ts2 <> LBRACE) -> Primary nr ts (AEPath p a) ts2
| PrimCall : forall nr ts p ts1 a ts2 ts3 args ts4 ts5, Path ts p ts1 -> App ts1 a ts2 ->
    Tk LPAREN ts2 ts3 -> CommaList (Expr false) [RPAREN] ts3 args ts4 -> Tk RPAREN ts4 ts5 ->
    Primary nr ts (AECall p a args) ts5
| PrimRecord : forall ts p ts1 a ts2 ts3 fs ts4 ts5, Path ts p ts1 -> App ts1 a ts2 ->
    Tk LBRACE ts2 ts3 -> CommaList RecField [RBRACE] ts3 fs ts4 -> Tk RBRACE ts4 ts5 ->
    Primary false ts (AERec p a fs) ts5
with RecField : list token -> name * option aexpr -> list token -> Prop :=
| RecFieldShort : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> EQUAL -> RecField ts (x, None) ts1
| RecFieldLong : forall ts x ts1 ts2 e ts3, TkName ts x ts1 -> Tk EQUAL ts1 ts2 -> Expr false ts2 e ts3 ->
    RecField ts (x, Some e) ts3
with Lambda : list token -> aexpr -> list token -> Prop :=
| LambdaG : forall ts ts1 ps ts2 ts3 ret ts4 ts5 body ts6, Tk LPAREN ts ts1 ->
    CommaList LamParam [RPAREN] ts1 ps ts2 -> Tk RPAREN ts2 ts3 -> OptType ts3 ret ts4 ->
    Tk EQUAL_GREATER ts4 ts5 -> Expr false ts5 body ts6 -> Lambda ts (AELam ps ret body) ts6
with LamParam : list token -> name * option aty -> list token -> Prop :=
| LamParamG : forall ts x ts1 t ts2, TkName ts x ts1 -> OptType ts1 t ts2 -> LamParam ts (x, t) ts2

(* b ::= { i ... e? } *)
with Block : list token -> ablock -> list token -> Prop :=
| BlockG : forall ts ts1 ss tl ts2 ts3, Tk LBRACE ts ts1 -> Stmts ts1 ss tl ts2 -> Tk RBRACE ts2 ts3 ->
    Block ts (ABlock ss tl) ts3

(* i ...  e?: the statements of a block, and its trailing expression.
   i ::= let x (: τ)? = e; | var x (: τ)? = e; | x = e; | w
       | while e b | loop b | for x in e b | e; | e
   An expression statement without `;` must START with `if`, `match` or
   `{` and not be followed by `;` or `}` (then it is the trailing
   expression).  The expression rules are tried only where no keyword
   statement and no `x = ...` is next (b). *)
with Stmts : list token -> list astmt -> option aexpr -> list token -> Prop :=
| StEnd : forall ts, kind ts = RBRACE -> Stmts ts [] None ts
| StLet : forall ts ts1 x ts2 t ts3 ts4 e ts5 ts6 ss tl ts7, Tk LET ts ts1 -> TkName ts1 x ts2 ->
    OptType ts2 t ts3 -> Tk EQUAL ts3 ts4 -> Expr false ts4 e ts5 -> Tk SEMI ts5 ts6 ->
    Stmts ts6 ss tl ts7 -> Stmts ts (ASLet x t e :: ss) tl ts7
| StVar : forall ts ts1 x ts2 t ts3 ts4 e ts5 ts6 ss tl ts7, Tk VAR ts ts1 -> TkName ts1 x ts2 ->
    OptType ts2 t ts3 -> Tk EQUAL ts3 ts4 -> Expr false ts4 e ts5 -> Tk SEMI ts5 ts6 ->
    Stmts ts6 ss tl ts7 -> Stmts ts (ASVar x t e :: ss) tl ts7
| StBind : forall ts w ts1 ss tl ts2, Bind ts w ts1 -> Stmts ts1 ss tl ts2 -> Stmts ts (ASBind w :: ss) tl ts2
| StWhile : forall ts ts1 c ts2 b ts3 ss tl ts4, Tk WHILE ts ts1 -> Expr true ts1 c ts2 ->
    Block ts2 b ts3 -> Stmts ts3 ss tl ts4 -> Stmts ts (ASWhile c b :: ss) tl ts4
| StLoop : forall ts ts1 b ts2 ss tl ts3, Tk LOOP ts ts1 -> Block ts1 b ts2 -> Stmts ts2 ss tl ts3 ->
    Stmts ts (ASLoop b :: ss) tl ts3
| StFor : forall ts ts1 x ts2 ts3 coll ts4 body ts5 ss tl ts6, Tk FOR ts ts1 -> TkName ts1 x ts2 ->
    Tk IN ts2 ts3 -> Expr true ts3 coll ts4 -> Block ts4 body ts5 -> Stmts ts5 ss tl ts6 ->
    Stmts ts (for_desugar x coll body :: ss) tl ts6
| StAssign : forall ts x ts1 ts2 e ts3 ts4 ss tl ts5, TkName ts x ts1 -> Tk EQUAL ts1 ts2 ->
    Expr false ts2 e ts3 -> Tk SEMI ts3 ts4 -> Stmts ts4 ss tl ts5 -> Stmts ts (ASAssign x e :: ss) tl ts5
| StExpr : forall ts e ts1 ts2 ss tl ts3, ~ assign_ahead ts -> Expr false ts e ts1 -> Tk SEMI ts1 ts2 ->
    Stmts ts2 ss tl ts3 -> Stmts ts (ASExpr e :: ss) tl ts3
| StBraced : forall ts e ts1 ss tl ts2, ~ assign_ahead ts -> In (kind ts) [IF; MATCH; LBRACE] ->
    Expr false ts e ts1 -> ~ In (kind ts1) [SEMI; RBRACE] -> Stmts ts1 ss tl ts2 ->
    Stmts ts (ASExpr e :: ss) tl ts2
| StTail : forall ts e ts1, ~ assign_ahead ts -> Expr false ts e ts1 -> kind ts1 = RBRACE ->
    Stmts ts [] (Some e) ts1

(* w ::= bind β, ...;    β ::= s | s = e *)
with Bind : list token -> list (spec * option aexpr) -> list token -> Prop :=
| BindG : forall ts ts1 bs ts2 ts3, Tk BIND ts ts1 -> CommaList Binding [SEMI] ts1 bs ts2 ->
    Tk SEMI ts2 ts3 -> Bind ts bs ts3
with Binding : list token -> spec * option aexpr -> list token -> Prop :=
| BindingApply : forall ts s ts1, Spec_ ts s ts1 -> kind ts1 <> EQUAL -> Binding ts (s, None) ts1
| BindingBind : forall ts s ts1 ts2 e ts3, Spec_ ts s ts1 -> Tk EQUAL ts1 ts2 -> Expr false ts2 e ts3 ->
    Binding ts (s, Some e) ts3.

(* ===================== Declarations d ===================================== *)

(* [x, ...], as a tag's or functor's brackets *)
Definition NameList (ts : list token) (xs : list name) (rest : list token) : Prop :=
  exists ts1 ts2, Tk LBRACKET ts ts1 /\ CommaList TkName [RBRACKET] ts1 xs ts2 /\ Tk RBRACKET ts2 rest.

(* f ::= x | x.x | .x *)
Inductive Function_ : list token -> fnrecv * name -> list token -> Prop :=
| FnPlain : forall ts x ts1, TkName ts x ts1 -> kind ts1 <> DOT -> Function_ ts (RPlain, x) ts1
| FnAttached : forall ts r ts1 ts2 x ts3, TkName ts r ts1 -> Tk DOT ts1 ts2 -> TkName ts2 x ts3 ->
    Function_ ts (RAttached r, x) ts3
| FnDetached : forall ts ts1 x ts2, Tk DOT ts ts1 -> TkName ts1 x ts2 -> Function_ ts (RDetached, x) ts2.

Inductive Param : list token -> name * aty -> list token -> Prop :=
| ParamG : forall ts x ts1 ts2 t ts3, TkName ts x ts1 -> Tk COLON ts1 ts2 -> Type_ ts2 t ts3 ->
    Param ts (x, t) ts3.

(* A type equation's side ([D90]): a type reference `p` or `p[p = s, ...]`. *)
Definition TypeRef (ts : list token) (t : aty) (rest : list token) : Prop :=
  exists p a ts1, Path ts p ts1 /\ App ts1 a rest /\ t = ATRef p a.

(* An equation `r = r` ([D90]).  Its `=` follows a whole reference, so it is
   outside any brackets: in `f[T=A] = B` the first `=` is the bracket's. *)
Definition Equation (ts : list token) (lr : aty * aty) (rest : list token) : Prop :=
  exists ts1 ts2, TypeRef ts (fst lr) ts1 /\ Tk EQUAL ts1 ts2 /\ TypeRef ts2 (snd lr) rest.

(* an assume's item: h, but not `.x` (and not followed by brackets or `=`,
   which make it an equation's side); or an equation *)
Inductive AssumeItem : list token -> aitem -> list token -> Prop :=
| AssumeHead : forall ts it rest, Head ts (Some (fst it), snd it) rest ->
    kind rest <> EQUAL -> kind rest <> LBRACKET -> AssumeItem ts (AIHead it) rest
| AssumeEq : forall ts l r rest, Equation ts (l, r) rest -> AssumeItem ts (AIEq l r) rest.

(* a context's item: s; or an equation *)
Inductive CtxItem : list token -> citem -> list token -> Prop :=
| CtxSpec : forall ts s rest, Spec_ ts s rest -> kind rest <> EQUAL -> CtxItem ts (CISpec s) rest
| CtxEq : forall ts l r rest, Equation ts (l, r) rest -> CtxItem ts (CIEq l r) rest.

(* `[x, ...]` after `tag x`, possibly absent or empty *)
Definition TagParams (ts : list token) (xs : list name) (rest : list token) : Prop :=
  (kind ts <> LBRACKET /\ xs = [] /\ rest = ts) \/ NameList ts xs rest.

(* d ::= assume a, ... { d ... } | type x; | type x = τ; | val x: τ (= e)?;
       | tag x; | tag x([x, ...])? (τ); | tag x([x, ...])? {x: τ, ...};
       | fn f(x: τ, ...) (: τ)? ; | fn f(x: τ, ...) (: τ)? b
       | context x = c, ...; | functor x([x, ...])?: s, ... -> s, ...;
       | functor x: s, ... -> s, ... { w ... }
   where an assume item a ::= h | r = r and a context item c ::= s | r = r,
   r a type reference (an equation, [D90]) *)
Inductive Decl : list token -> adecl -> list token -> Prop :=
| DAssume : forall ts ts1 its ts2 ts3 ds ts4 ts5, Tk ASSUME ts ts1 ->
    CommaList AssumeItem [LBRACE] ts1 its ts2 -> Tk LBRACE ts2 ts3 -> Decls ts3 ds ts4 ->
    Tk RBRACE ts4 ts5 -> Decl ts (ADAssume its ds) ts5
| DType : forall ts ts1 x ts2 ts3, Tk TYPE ts ts1 -> TkName ts1 x ts2 -> Tk SEMI ts2 ts3 ->
    Decl ts (ADType x) ts3
| DAlias : forall ts ts1 x ts2 ts3 t ts4 ts5, Tk TYPE ts ts1 -> TkName ts1 x ts2 -> Tk EQUAL ts2 ts3 ->
    Type_ ts3 t ts4 -> Tk SEMI ts4 ts5 -> Decl ts (ADAlias x t) ts5
(* `tag x;`, without brackets: `tag x[];` is refused (D73) *)
| DUnit : forall ts ts1 x ts2 ts3, Tk TAG ts ts1 -> TkName ts1 x ts2 -> kind ts2 <> LBRACKET ->
    Tk SEMI ts2 ts3 -> Decl ts (ADUnit x) ts3
| DTagRecord : forall ts ts1 x ts2 ps ts3 t ts4 ts5, Tk TAG ts ts1 -> TkName ts1 x ts2 ->
    TagParams ts2 ps ts3 -> RecordType ts3 t ts4 -> Tk SEMI ts4 ts5 -> Decl ts (ADTag x ps t) ts5
(* a record payload is written with braces, not `({...})` (D83) *)
| DTag : forall ts ts1 x ts2 ps ts3 ts4 t ts5 ts6 ts7, Tk TAG ts ts1 -> TkName ts1 x ts2 ->
    TagParams ts2 ps ts3 -> Tk LPAREN ts3 ts4 -> Type_ ts4 t ts5 -> (forall fs, t <> ATRec fs) ->
    Tk RPAREN ts5 ts6 -> Tk SEMI ts6 ts7 -> Decl ts (ADTag x ps t) ts7
| DVal : forall ts ts1 x ts2 ts3 t ts4 ts5, Tk VAL ts ts1 -> TkName ts1 x ts2 -> Tk COLON ts2 ts3 ->
    Type_ ts3 t ts4 -> Tk SEMI ts4 ts5 -> Decl ts (ADVal x t None) ts5
| DValDefined : forall ts ts1 x ts2 ts3 t ts4 ts5 e ts6 ts7, Tk VAL ts ts1 -> TkName ts1 x ts2 ->
    Tk COLON ts2 ts3 -> Type_ ts3 t ts4 -> Tk EQUAL ts4 ts5 -> Expr false ts5 e ts6 ->
    Tk SEMI ts6 ts7 -> Decl ts (ADVal x t (Some e)) ts7
| DFnAbstract : forall ts ts1 rx ts2 ts3 ps ts4 ts5 ret ts6 ts7, Tk FN ts ts1 -> Function_ ts1 rx ts2 ->
    Tk LPAREN ts2 ts3 -> CommaList Param [RPAREN] ts3 ps ts4 -> Tk RPAREN ts4 ts5 ->
    OptType ts5 ret ts6 -> Tk SEMI ts6 ts7 -> Decl ts (ADFn (fst rx) (snd rx) ps ret None) ts7
| DFn : forall ts ts1 rx ts2 ts3 ps ts4 ts5 ret ts6 b ts7, Tk FN ts ts1 -> Function_ ts1 rx ts2 ->
    Tk LPAREN ts2 ts3 -> CommaList Param [RPAREN] ts3 ps ts4 -> Tk RPAREN ts4 ts5 ->
    OptType ts5 ret ts6 -> Block ts6 b ts7 -> Decl ts (ADFn (fst rx) (snd rx) ps ret (Some b)) ts7
| DContext : forall ts ts1 x ts2 ts3 ss ts4 ts5, Tk CONTEXT ts ts1 -> TkName ts1 x ts2 ->
    Tk EQUAL ts2 ts3 -> CommaList CtxItem [SEMI] ts3 ss ts4 -> Tk SEMI ts4 ts5 -> Decl ts (ADCtx x ss) ts5
(* an abstract functor, its brackets optional (D78, D84) *)
| DFunctorAbstract : forall ts ts1 x ts2 ps ts3 ts4 args ts5 ts6 res ts7 ts8, Tk FUNCTOR ts ts1 ->
    TkName ts1 x ts2 ->
    ((kind ts2 <> LBRACKET /\ ps = None /\ ts3 = ts2) \/ exists l, ps = Some l /\ NameList ts2 l ts3) ->
    Tk COLON ts3 ts4 -> CommaList Spec_ [HYPHEN_GREATER] ts4 args ts5 -> Tk HYPHEN_GREATER ts5 ts6 ->
    CommaList Spec_ [LBRACE; SEMI] ts6 res ts7 -> Tk SEMI ts7 ts8 ->
    Decl ts (ADFunctor x ps args res None) ts8
(* a functor with a body has no brackets (D84) *)
| DFunctor : forall ts ts1 x ts2 ts3 args ts4 ts5 res ts6 ts7 ws ts8 ts9, Tk FUNCTOR ts ts1 ->
    TkName ts1 x ts2 -> kind ts2 <> LBRACKET -> Tk COLON ts2 ts3 ->
    CommaList Spec_ [HYPHEN_GREATER] ts3 args ts4 -> Tk HYPHEN_GREATER ts4 ts5 ->
    CommaList Spec_ [LBRACE; SEMI] ts5 res ts6 -> Tk LBRACE ts6 ts7 -> Binds ts7 ws ts8 ->
    Tk RBRACE ts8 ts9 -> Decl ts (ADFunctor x None args res (Some ws)) ts9
with Decls : list token -> list adecl -> list token -> Prop :=
| DeclsEnd : forall ts, kind ts = RBRACE -> Decls ts [] ts
| DeclsCons : forall ts d ts1 ds ts2, Decl ts d ts1 -> Decls ts1 ds ts2 -> Decls ts (d :: ds) ts2
with Binds : list token -> list (list (spec * option aexpr)) -> list token -> Prop :=
| BindsEnd : forall ts, kind ts = RBRACE -> Binds ts [] ts
| BindsCons : forall ts w ts1 ws ts2, Bind ts w ts1 -> Binds ts1 ws ts2 -> Binds ts (w :: ws) ts2.

(* ===================== Imports m, Uses u, the File F ====================== *)

(* u ::= x | x as x | .x | .x as .x   (a rename keeps the dottedness) *)
Definition UseName (ts : list token) (u : bool * name) (rest : list token) : Prop :=
  exists ts1, ((fst u = true /\ Tk DOT ts ts1) \/ (fst u = false /\ kind ts <> DOT /\ ts1 = ts)) /\
              TkName ts1 (snd u) rest.
Inductive Use : list token -> ause -> list token -> Prop :=
| UsePlain : forall ts d x ts1, UseName ts (d, x) ts1 -> kind ts1 <> AS -> Use ts (AUse d x None) ts1
| UseRenamed : forall ts d x ts1 ts2 y ts3, UseName ts (d, x) ts1 -> Tk AS ts1 ts2 ->
    UseName ts2 (d, y) ts3 -> Use ts (AUse d x (Some (d, y))) ts3.

(* m ::= import str (as x)? (use * | use u, ...)?; *)
Inductive Import_ : list token -> aimport -> list token -> Prop :=
| ImportG : forall ts ts1 s ts2 alias ts3 glob uses ts4 ts5, Tk IMPORT ts ts1 ->
    TkText STRING ts1 s ts2 ->
    ((kind ts2 <> AS /\ alias = None /\ ts3 = ts2) \/
     exists ts' a, alias = Some a /\ Tk AS ts2 ts' /\ TkName ts' a ts3) ->
    ((kind ts3 <> USE /\ glob = false /\ uses = [] /\ ts4 = ts3) \/
     (exists ts', Tk USE ts3 ts' /\ Tk STAR ts' ts4 /\ glob = true /\ uses = []) \/
     (exists ts', Tk USE ts3 ts' /\ kind ts' <> STAR /\ glob = false /\
                  CommaList Use [SEMI] ts' uses ts4)) ->
    Tk SEMI ts4 ts5 -> Import_ ts (AImport (string_value s) alias glob uses) ts5.

Inductive Imports : list token -> list aimport -> list token -> Prop :=
| ImportsEnd : forall ts, kind ts <> IMPORT -> Imports ts [] ts
| ImportsCons : forall ts m ts1 ms ts2, Import_ ts m ts1 -> Imports ts1 ms ts2 -> Imports ts (m :: ms) ts2.

Inductive DeclsToEnd : list token -> list adecl -> list token -> Prop :=
| EndOfFile : forall ts, kind ts = EOF -> DeclsToEnd ts [] ts
| DeclThen : forall ts d ts1 ds ts2, Decl ts d ts1 -> DeclsToEnd ts1 ds ts2 -> DeclsToEnd ts (d :: ds) ts2.

(* F ::= m ... d ... (then the end of the file) *)
Definition File (ts : list token) (f : afile) : Prop :=
  exists ts1 ts2, Imports ts (f_imports f) ts1 /\ DeclsToEnd ts1 (f_decls f) ts2.

(* The specification: the bytes of a source file have the AST f. *)
Definition Derives (bytes : list byte) (f : afile) : Prop :=
  exists ts, Lexes 0 (source_bytes bytes) ts /\ File ts f.

End Syntax.

(* ######################################################################## *)
(* Part I.3  The module loader                                               *)
(* ######################################################################## *)

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
   module holds exactly f, whether it wants the prelude,
   and its imports resolved (D64); and every module it imports, and the
   prelude if it wants it, is in the bundle too. *)
Definition Bundled (read : string -> option string) (root prelude_in entry_in : string) (b : bundle) : Prop :=
  b_entry b = normalize root None entry_in /\ b_prelude b = normalize root None prelude_in /\
  NoDup (map am_path (b_mods b)) /\ In (b_entry b) (map am_path (b_mods b)) /\
  forall m, In m (b_mods b) ->
    exists text f, read (am_path m) = Some text /\ Syntax.Derives (list_byte_of_string text) f /\
      am_file m = f /\
      am_prelude m = wants_prelude (b_prelude b) (am_path m) /\
      am_targets m = map (fun i => resolve root (b_prelude b) (am_path m) (i_path i)) (f_imports f) /\
      (forall t, In t (am_targets m) -> In t (map am_path (b_mods b))) /\
      (am_prelude m = true -> In (b_prelude b) (map am_path (b_mods b))).
End Loader.

(* The public model has its own namespace. Implementation and proof helpers
   may import the shared surface syntax without importing this model's names. *)
Module Semantics.

(* ######################################################################## *)
(* Part II. Elaborated programs                                              *)
(* ######################################################################## *)

Set Implicit Arguments.

(* Shared symbols, types and provision keys. *)


(* ===================== Symbols =========================================== *)

Definition tsym := nat.  (* abstract type symbols *)
Definition nsym := nat.  (* nominal tag names *)
Definition vsym := nat.  (* val symbols *)
Definition fsym := nat.  (* fn symbols *)
Definition msym := nat.  (* method symbols *)
Definition var  := nat.  (* term variables *)
Definition usym := nat.  (* functor symbols [D55]/[D78] *)

(* ===================== Types and substitutions =========================== *)

(* Machine representations and structural records are not nominal declarations.
   Type arguments are finite rows over the declaration's parameter symbols. *)
Inductive machine_class := MScalar (sort : nat) | MReference (kind : nat).

Inductive ty : Type :=
| TMachine : tsym -> ty
| TRecord : list (string * ty) -> ty
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
  | TMachine m => TMachine m
  | TRecord fs => TRecord ((fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (f, t) :: rest => (f, app_subst s t) :: go rest end) fs)
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
  | TMachine a, TMachine b => Nat.eqb a b
  | TRecord fs, TRecord gs =>
      (fix go (fs gs : list (string * ty)) :=
        match fs, gs with
        | [], [] => true
        | (f,t)::fs, (g,u)::gs => String.eqb f g && ty_eqb t u && go fs gs
        | _, _ => false end) fs gs
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

(* ===================== Items and telescopes ============================== *)

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

(* Satisfaction maps: for each requirement of a callee/provider, the in-force
   item that satisfies it (recorded by elaboration, used by the dynamics). *)
Definition smap := list (item * item).

(* The reference language: typed annotations are retained through execution. *)

Local Open Scope nat_scope.

(* ===================== Values ============================================ *)

Definition fld := string.  (* source field names *)

Definition address := nat.

Inductive value : Type :=
| VUnit : value
| VTag : nsym -> value -> value
| VRecord : list (fld * value) -> value
| VScalar : nat -> Z -> value
| VRef : address -> option (nat * ty) -> value.

(* An operation's platform name and operands. *)
Record op := Op { op_name : string; op_syms : list nat }.

(* ===================== Expressions and bindings ========================== *)

Inductive pattern : Type :=
| PAny  : option var -> pattern                          (* _ or a binder *)
| PHead : nsym -> option var -> list (fld * var) -> pattern.

(* Code determines the generations and source aliases a closure uses.
   Capture reads these expressions once, rather than copying ambient types. *)
Record type_capture := TypeCapture {
  capture_generations : subst;
  capture_aliases : subst
}.

(* Expressions include calls, provision binding, records and control flow.
   Satisfaction maps identify the provisions passed to a call. Each binding
   has exactly one syntax form, used in expressions and in functor bodies. *)
Inductive expr : Type :=
| EVar                 : var -> expr
| EUnit                : expr
| EVal                 : vsym -> expr
| ETag                 : nsym -> subst -> expr -> expr
| ECall                : fsym -> subst -> smap -> list expr -> expr
| EInvoke              : fsym -> list expr -> expr
| EMethod              : expr -> msym -> item -> list expr -> expr
| ELet                 : var -> expr -> expr -> expr
| EBind                : binding -> expr -> expr
| ESeq                 : expr -> expr -> expr
| EAssign              : var -> expr -> expr
| EIf                  : expr -> expr -> expr -> expr
| EWhile               : expr -> expr -> expr
| ELoop                : expr -> expr
| EReturn              : expr -> expr
| EBreak               : expr
| ERecord              : list (fld * expr) -> expr
| EField               : expr -> fld -> expr
| EMatch               : expr -> list (pattern * expr) -> expr
with binding : Type :=
| BType                : tsym -> ty -> binding
| BValue               : vsym -> expr -> binding
| BFunction            : fsym -> fsym -> type_capture -> smap -> binding
| BMethod              : ty -> msym -> subst -> fsym -> type_capture -> smap -> binding
| BFunctor             : usym -> usym -> type_capture -> smap -> smap -> binding
(* `bind F = H;`: the closure of H, its needs read through the smap and
   each position of F's declared result read at the given position of H's *)
| BCopyFunctor         : usym -> usym -> smap -> list nat -> binding
(* an application: θ, the output destinations, the needs, and the caller's
   key for each position of F's declared result *)
| BApply               : usym -> subst -> list (tsym * tsym) -> smap -> list item -> binding
| BPrimitive           : item -> op -> binding.

(* Named builders are notation for the single binding form. *)
Definition apply_expression u arguments inputs results body := EBind (BApply u arguments [] inputs results) body.
Definition apply_output_expression u arguments outputs inputs results body :=
  EBind (BApply u arguments outputs inputs results) body.
Definition bind_type_expression t tau e := EBind (BType t tau) e.
Definition bind_value_expression v value e := EBind (BValue v value) e.
Definition bind_function_expression f body types captures e := EBind (BFunction f body types captures) e.
Definition bind_method_expression receiver method application body types captures e :=
  EBind (BMethod receiver method application body types captures) e.
Definition bind_functor_expression u body types linked captured e := EBind (BFunctor u body types linked captured) e.
Definition copy_functor_expression u provider inputs positions e := EBind (BCopyFunctor u provider inputs positions) e.
Definition bind_primitive_expression key operation e := EBind (BPrimitive key operation) e.

(* ===================== Runtime structures ================================ *)

(* Generation meanings are stable within an activation. Source aliases track
   rebinding and final type outputs separately. A closure captures all three
   namespaces; grounding never reads the mutable source aliases. *)
Inductive provision_value : Type :=
| ValueProvision : value -> provision_value
| FunctionClosure : fsym -> environment -> provision_value
(* code, needs adaptation, captures, result positions. A copy composes its
   step into both at once (compose_adaptation, compose_positions), so a closure
   carries one of each however many copies made it. None is the root's
   own: a platform family's needs as they are, a root's sources in order. *)
| FunctorClosure : usym -> option smap -> environment -> option (list (option nat)) -> provision_value
| PrimitiveClosure : op -> option ty -> provision_value
with environment : Type :=
| EnvironmentWithAliases : subst -> subst -> list (item * provision_value) -> environment.
Definition Environment (types : subst) (provisions : list (item * provision_value)) :=
  EnvironmentWithAliases types [] provisions.
Definition type_meanings (d : environment) := match d with EnvironmentWithAliases ts _ _ => ts end.
Definition type_aliases (d : environment) := match d with EnvironmentWithAliases _ aliases _ => aliases end.
Definition provision_values (d : environment) := match d with EnvironmentWithAliases _ _ ps => ps end.
Definition empty_environment := Environment [] [].
Definition locals := list (var * value).
Definition ground (d : environment) (tau : ty) : ty := app_subst (type_meanings d) tau.
Definition append_environment (a b : environment) :=
  EnvironmentWithAliases (type_meanings a ++ type_meanings b)
    (type_aliases a ++ type_aliases b) (provision_values a ++ provision_values b).
Definition install_provision (key : item) (entry : provision_value) (d : environment) :=
  EnvironmentWithAliases (type_meanings d) (type_aliases d) ((key,entry) :: provision_values d).
Definition instantiate (d : environment) (th : subst) : environment :=
  let meanings := map (fun q => (fst q, ground d (snd q))) th in
  EnvironmentWithAliases meanings meanings [].
Definition typed_capture (d : environment) (types : type_capture) (dc : environment) : environment :=
  EnvironmentWithAliases
    (map (fun q => (fst q, ground d (snd q))) (capture_generations types))
    (map (fun q => (fst q, ground d (snd q))) (capture_aliases types))
    (provision_values dc).
Definition capture_environment (d dc : environment) : environment :=
  EnvironmentWithAliases (type_meanings dc ++ type_meanings d)
    (type_aliases dc ++ type_aliases d) (provision_values dc).
Definition primitive_closure (d : environment) (i : item) (p : op) : provision_value :=
  PrimitiveClosure p (match i with
    | IMth tau _ _ => Some (ground d tau)
    | _ => None end).
Fixpoint lookup_entry (i : item) (entries : list (item * provision_value)) : option provision_value :=
  match entries with
  | [] => None
  | (key,value)::rest => if item_eqb i key then Some value else lookup_entry i rest
  end.
Definition lookup_provision (i : item) (d : environment) := lookup_entry i (provision_values d).

Fixpoint lookup_local (x : var) (g : locals) : option value :=
  match g with
  | [] => None
  | (x', w) :: r => if Nat.eqb x x' then Some w else lookup_local x r
  end.

Fixpoint assign_local (x : var) (w : value) (g : locals) : option locals :=
  match g with
  | [] => None
  | (x', w') :: r =>
      if Nat.eqb x x' then Some ((x', w) :: r)
      else match assign_local x w r with
           | Some r' => Some ((x', w') :: r')
           | None => None
           end
  end.

Fixpoint select_provisions (d : environment) (sm : smap) : option environment :=
  match sm with
  | [] => Some empty_environment
  | (req, s) :: r =>
      match lookup_provision s d, select_provisions d r with
      | Some en, Some d' => Some (install_provision req en d')
      | _, _ => None
      end
  end.

(* A closure's needs adaptation, applied to what the application selected. *)
Definition adapt_provisions (d : environment) (a : option smap) : option environment :=
  match a with
  | None => Some d
  | Some sm => select_provisions d sm
  end.
(* The source a satisfaction map selects for a requirement: its first
   entry's, as select_provisions installs them. *)
Fixpoint smap_source (req : item) (sm : smap) : option item :=
  match sm with
  | [] => None
  | (r, s) :: rest => if item_eqb req r then Some s else smap_source req rest
  end.
(* `bind F = H;` composes its smap sm (H's needs, read from F's) under H's
   adaptation a: each requirement a reads from H's key s reads from sm's
   source for s. Selecting through the composition is selecting through sm
   and then a (EagerCopy.adapt_compose). A key sm lacks is kept, which only
   matters where the two-step selection is stuck. *)
Definition compose_adaptation (a : option smap) (sm : smap) : option smap :=
  Some (match a with
    | None => sm
    | Some c => map (fun q => (fst q, match smap_source (snd q) sm with Some s => s | None => snd q end)) c
    end).

Definition parameter_values (ws : list value) : locals := combine (seq 0 (List.length ws)) ws.

(* Program bodies and their declared formal interfaces. *)
Record cast_spec : Type := CastSpec {
  cast_kind : nat;
  cast_held : tsym;
  (* Ref and Array are ordinary wrappers when their respective raw
     handle payload is rebound to a different representation. *)
  cast_handle : option (tsym * ty)
}.
(* A semantic program contains: the bodies of every
   defined fn (and of the fns elaboration synthesizes: forwarders, lambda
   providers, wrappers), the core functor bodies `bind F = G;` made, Bool's
   symbols, `main`, main's initial runtime environment (the platform's
   provisions for what main assumes), and the names of primitives the
   model does not implement, in diagnostic index order. *)
Record functor_code := FunctorCode {
  fc_inputs : list tsym;
  (* the inputs each application supplies (the rest are captured) *)
  fc_linked : list tsym;
  (* the body's type table (G's canonical c_ty): an input it maps to a
     type reads as that type over the inputs' meanings, so the body's
     environment absorbs G's merges; an input it omits reads as itself *)
  fc_table : list (tsym * ty);
  (* the result's type outputs (the body's roles); an application reads
     each one's final meaning *)
  fc_outputs : tele;
  fc_bindings : list binding;
  (* where the body left each item of the declared result, position by
     position (the declared result's keys, ResultKeys): the body's own key
     for it. An application installs the item at position i under the
     caller's key for position i, so no key of the body's is read in the
     caller's terms (tests/regressions/functor_input_output_generations.moss,
     tests/wasi/copy_extra_key.moss, tests/wasi/linked_copy_bracket.moss) *)
  fc_sources : list item
}.
Record program : Type := Program {
  program_functions : list (nat * expr);
  program_functors : list (usym * functor_code);
  program_bool : nat; program_true : nat; program_false : nat;
  program_main : nat;
  program_environment : environment;
  program_unsupported : list string;
  program_casts : list (nsym * cast_spec);
  program_machines : list (tsym * machine_class)
}.

(* Execution view: symbol lookups, runtime inputs and representation data.
   program_of constructs this view from program and the host inputs. *)
Record program_view : Type := ProgramView {
  view_function    : fsym -> option expr;
  view_functor   : usym -> option functor_code;
  view_bool  : nsym;
  view_true  : nsym;
  view_false : nsym;
  view_arguments  : list (list int);
  (* the host filesystem, an input like argv: a path's bytes, if readable *)
  view_read  : list int -> option (list int);
  (* checked reference tags and their header requirements *)
  view_casts : list (nsym * cast_spec);
  view_machines : list (tsym * machine_class)
}.

Fixpoint lookup_cast (n : nsym) (cs : list (nsym * cast_spec)) : option cast_spec :=
  match cs with
  | [] => None
  | (n', c) :: r => if Nat.eqb n n' then Some c else lookup_cast n r
  end.
Definition bind_type (d : environment) (t : tsym) (tau : ty) : environment :=
  EnvironmentWithAliases (type_meanings d) ((t, ground d tau) :: type_aliases d) (provision_values d).
Definition call_environment (d : environment) (th : subst) (dc : environment) : environment :=
  (* A new activation receives its formals and lexical capture, rather than
     unrelated ambient caller generations which may reuse its local names. *)
  append_environment (instantiate d th) dc.

Definition output_meaning (d : environment) (source : tsym) : option ty :=
  match lookup_subst source (type_aliases d) with
  | Some meaning => Some meaning
  | None => lookup_subst source (type_meanings d)
  end.
(* Rebase exactly the provider's formal roles. Linked inputs (fc_linked)
   are read from θ here; the rest, including an input that only a copy of
   the functor takes, use their captured source meanings. Unrelated
   captured generations cannot occupy this activation's local output names.
   An input the body's table (fc_table) maps to a type then reads as that
   type over those meanings: a merged-away input reads as its
   representative, so the body's merges hold by construction. Elaborated
   code reads types through that table, so it never sees the difference. *)
Definition application_environment (P : program_view) (d : environment) (h : usym) (th : subst) (dc : environment) : environment :=
  let linked := match view_functor P h with Some code => fc_linked code | None => [] end in
  let inputs := match view_functor P h with Some code => fc_inputs code | None => [] end in
  let table := match view_functor P h with Some code => fc_table code | None => [] end in
  let base source :=
    match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning dc source with
      | Some meaning => meaning | None => TAbs source end
    end in
  let bases := map (fun source => (source, base source)) inputs in
  let meanings := map (fun source => (source,
    match lookup_subst source table with
    | Some t => app_subst bases t
    | None => base source
    end)) inputs in
  EnvironmentWithAliases meanings meanings (provision_values dc).

(* A closure's result positions: position i reads the root's source at
   the i-th entry (None, the root's own sources in order). Each position of
   a copy names a position of its source's declared result; a stray one
   (None) is unreachable for a checked program (CopyInterpretation), and no
   provision is keyed by a type item, so it makes the selection stuck. *)
Definition closure_sources (p : option (list (option nat))) (sources : list item) : list item :=
  match p with
  | None => sources
  | Some q => map (fun o => match o with Some i => nth i sources (ITy 0) | None => ITy 0 end) q
  end.
(* `bind F = H;` composes its positions rm (position j of F's result is
   position rm_j of H's) under H's positions p. Reading the root's sources
   through the composition is reading them through p and then rm
   (EagerCopy.sources_compose), exactly. *)
Definition compose_positions (p : option (list (option nat))) (rm : list nat) : option (list (option nat)) :=
  Some (match p with
    | None => map Some rm
    | Some q => map (fun j => nth j q None) rm
    end).
(* An application's selection: the caller's key for each position, read
   from that position's source. A later position wins, as the caller's
   static context reads a repeated key at its last setter (output_cx's
   `rev es`), so the run-time provision under a repeated caller key is the
   one its static type describes, whatever the sources of the earlier
   positions (two of F's keys may meet only at the caller: A.m and B.m
   with A and B read as one type). *)
Definition result_selection (rs : list item) (p : option (list (option nat))) (sources : list item) : smap :=
  rev (combine rs (closure_sources p sources)).
(* Read every output from one completed body snapshot before installing any
   caller destination. An unchanged formal may be an output too. *)
Fixpoint read_result_types (d : environment) (Dr : tele) (outputs : list (tsym * tsym))
    : option (subst * subst) :=
  match outputs with
  | [] => Some ([], [])
  | (source, destination) :: rest =>
      if existsb (item_eqb (ITy source)) Dr then
        match output_meaning d source, read_result_types d Dr rest with
        | Some meaning, Some (writes, aliases) =>
            Some ((destination, meaning) :: writes, (source, meaning) :: aliases)
        | _, _ => None end
      else None
  end.
Fixpoint fresh_result_types (writes occupied : subst) : bool :=
  match writes with
  | [] => true
  | (destination, meaning) :: rest =>
      match lookup_subst destination occupied with
      | Some _ => false
      | None => fresh_result_types rest ((destination, meaning) :: occupied)
      end
  end.
(* The writes shadow any meaning the caller has for a destination, as the
   oracle's (dr ++ d) does. Audit's static rules keep every destination out
   of the caller's context and frame types (FreshFor, the output clauses),
   so nothing typed in scope is re-grounded; an occupied destination can
   still occur at run time (an inlined defined val's kept output, a
   lambda's capture of a declared symbol it mentions), so a freshness check
   here would make accepted programs Stuck (scratch/orch/results2-AUDIT.md).
   [fresh_result_types] stays as the check's statement. *)
Definition result_environment (outputs : list (tsym * tsym))
    (d3 : environment) (Dr : tele) (dr : environment) : option environment :=
  match read_result_types d3 Dr outputs with
  | Some (writes, aliases) => Some (EnvironmentWithAliases writes aliases (provision_values dr))
  | None => None
  end.

(* Outcomes.  Normal/Stuck/OutOfFuel are the core's Ok/Err/OutOfFuel; Returning and Breaking
   are control transfers; Trap is a defined failure,
   Exit a WASI proc_exit, UnsupportedOperation a primitive this model does not implement.
   The nat in Stuck/Trap/UnsupportedOperation is a diagnostic code only. *)
Inductive signal : Type :=
| Normal               : value -> signal
| Returning            : value -> signal
| Breaking             : signal
| Stuck                : nat -> signal
| Trap                 : nat -> signal
| Exit                 : int -> signal
| UnsupportedOperation : nat -> signal
| OutOfFuel            : signal.

(* ######################################################################## *)
(* Part III. Dynamic semantics                                               *)
(* ######################################################################## *)

(* Rule A: an application files each item of F's declared result under
   the caller's key for it (the key its scope refers to it by), read from
   the body where the root left it (closure_sources, result_selection).
   Exactly F's declared result is installed, as `bind F;` keeps exactly
   F's result (tests/wasi/copy_extra_key.moss), and no key of the body's is
   read in the caller's terms (tests/wasi/linked_copy_bracket.moss). *)

(* Reference execution is parameterized by primitive operations and their
   state. Public.interface_laws specifies their typed and heap behavior.
   The callback receives the closure's optional site and implements its
   platform's calling convention. A concrete implementation must prove that
   its representation refines this interface; the existing ExecutionModel
   proofs do not supply that bridge. *)
Module Type PRIMITIVES.
  Parameter mstate : Type.
  Parameter run_prim : program_view -> op -> option ty -> list value -> mstate -> signal * mstate.
End PRIMITIVES.

Module Interpreter (Import Prims : PRIMITIVES).
Record evaluation : Type := Evaluation { evaluation_signal : signal; evaluation_locals : locals; evaluation_state : mstate }.

(* ===================== Reference execution =============================== *)

(* Diagnostic codes for Stuck (stuck states) *)
Definition e_var := 1.      Definition e_val := 2.      Definition e_compose := 3.
Definition e_nobody := 4.   Definition e_break := 5.    Definition e_entry := 6.
Definition e_match := 7.    Definition e_chain := 8.    Definition e_assign := 9.
Definition e_bool := 10.    Definition e_field := 11.   Definition e_branch := 12.
Definition e_fun := 13.     Definition e_pat := 14.

Definition ret_call (g : locals) (r : evaluation) : evaluation :=
  match r with
  | Evaluation o _ st => Evaluation (match o with Returning w => Normal w | Breaking => Stuck e_break | o => o end) g st
  end.

Definition pop1 (r : evaluation) : evaluation :=
  match r with Evaluation o g st => Evaluation o (tl g) st end.

Definition popn (n : nat) (r : evaluation) : evaluation :=
  match r with Evaluation o g st => Evaluation o (skipn n g) st end.

Definition with_g (g : locals) (r : evaluation) : evaluation :=
  match r with Evaluation o _ st => Evaluation o g st end.

(* A checked source wrapper is the native ref.cast_null operation. The
   metadata is the allocation's runtime type header, carried by an opaque
   handle; extracting/repacking a handle never changes that header. *)
Definition check_tag (P : program_view) (d : environment) (n : nsym) (th : subst) (w : value) : signal :=
  match lookup_cast n (view_casts P) with
  | None => Normal (VTag n w)
  | Some spec =>
      let enabled := match cast_handle spec with
                     | None => true
                     | Some (h, expected) => ty_eqb (ground d (match lookup_subst h th with Some t => t | None => TAbs h end)) expected
                     end in
      if enabled then
        match lookup_subst (cast_held spec) th, w with
        | Some tau, VRef _ (Some (k', tau')) =>
            if Nat.eqb (cast_kind spec) k' && ty_eqb (ground d tau) tau'
            then Normal (VTag n w) else Trap 37
        | _, _ => Trap 37
        end
      else Normal (VTag n w)
  end.
(* Transparent call entry for the interpreter-agreement proofs. *)
Definition run_prim_at := run_prim.

Definition truthy (P : program_view) (w : value) : option bool :=
  match w with
  | VTag b (VTag tag VUnit) =>
      if Nat.eqb b (view_bool P) then
        if Nat.eqb tag (view_true P) then Some true
        else if Nat.eqb tag (view_false P) then Some false else None
      else None
  | _ => None
  end.

Fixpoint assoc_fld (f : fld) (fs : list (fld * value)) : option value :=
  match fs with
  | [] => None
  | (f', w) :: r => if String.eqb f f' then Some w else assoc_fld f r
  end.

Definition get_field (f : fld) (w : value) : option value :=
  match w with
  | VRecord fs => assoc_fld f fs
  | VTag _ (VRecord fs) => assoc_fld f fs
  | _ => None
  end.

Inductive pm : Type := PMatch (bs : locals) | PNo | PBad.

Fixpoint bind_fields (fs : list (fld * var)) (r : list (fld * value)) : option locals :=
  match fs with
  | [] => Some []
  | (f, x) :: rest =>
      match assoc_fld f r, bind_fields rest r with
      | Some w, Some bs => Some ((x, w) :: bs)
      | _, _ => None
      end
  end.

Definition match_pat (p : pattern) (w : value) : pm :=
  match p with
  | PAny None => PMatch []
  | PAny (Some x) => PMatch [(x, w)]
  | PHead n ob fs =>
      match w with
      | VTag n' pay =>
          if Nat.eqb n n' then
            let b1 := match ob with Some x => [(x, pay)] | None => [] end in
            match fs with
            | [] => PMatch b1
            | _ => match pay with
                   | VRecord r => match bind_fields fs r with
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

Fixpoint select_arm (arms : list (pattern * expr)) (w : value)
  : option (locals * expr) :=
  match arms with
  | [] => None
  | (p, eb) :: r =>
      match match_pat p w with
      | PMatch bs => Some (bs, eb)
      | PNo => select_arm r w
      | PBad => None
      end
  end.

Fixpoint eval (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (e : expr) {struct fuel} : evaluation :=
  match fuel with
  | 0 => Evaluation OutOfFuel g st
  | S k =>
      match e with
      | EVar x => match lookup_local x g with
                  | Some w => Evaluation (Normal w) g st
                  | None => Evaluation (Stuck e_var) g st
                  end
      | EUnit => Evaluation (Normal VUnit) g st
      | EVal v => match lookup_provision (IVal v) d with
                 | Some (ValueProvision w) => Evaluation (Normal w) g st
                 | _ => Evaluation (Stuck e_val) g st
                 end
      | ETag n th e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 => Evaluation (check_tag P d n th w) g1 st1
          | r => r
          end
      | ECall f th sm args =>
          match view_function P f with
          | Some body =>
              match eval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  match select_provisions d sm with
                  | Some d' => ret_call g1 (eval k P (call_environment d th d') (parameter_values ws) st1 body)
                  | None => Evaluation (Stuck e_compose) g1 st1
                  end
              | inr r => r
              end
          | None => Evaluation (Stuck e_nobody) g st
          end
      | EInvoke f args =>
          match lookup_provision (IFn f) d with
          | Some (FunctionClosure f' df) =>
              match view_function P f' with
              | Some body =>
                  match eval_arguments k P d g st args with
                  | inl (ws, g1, st1) => ret_call g1 (eval k P df (parameter_values ws) st1 body)
                  | inr r => r
                  end
              | None => Evaluation (Stuck e_nobody) g st
              end
          | Some (PrimitiveClosure p held) =>
              match eval_arguments k P d g st args with
              | inl (ws, g1, st1) =>
                  let (o, st2) := run_prim_at P p held ws st1 in Evaluation o g1 st2
              | inr r => r
              end
          | _ => Evaluation (Stuck e_entry) g st
          end
      | EMethod e0 m prov args =>
          match lookup_provision prov d with
          | Some (FunctionClosure f' d') =>
              match view_function P f' with
              | Some body =>
                  match eval k P d g st e0 with
                  | Evaluation (Normal w0) g0 st0 =>
                      match eval_arguments k P d g0 st0 args with
                      | inl (ws, g1, st1) =>
                          ret_call g1 (eval k P d' (parameter_values (w0 :: ws)) st1 body)
                      | inr r => r
                      end
                  | r => r
                  end
              | None => Evaluation (Stuck e_nobody) g st
              end
          | Some (PrimitiveClosure p held) =>
              match eval k P d g st e0 with
              | Evaluation (Normal w0) g0 st0 =>
                  match eval_arguments k P d g0 st0 args with
                  | inl (ws, g1, st1) =>
                      let (o, st2) := run_prim_at P p held (w0 :: ws) st1 in Evaluation o g1 st2
                  | inr r => r
                  end
              | r => r
              end
          | _ => Evaluation (Stuck e_entry) g st
          end
      | ELet x e1 e2 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w1) g1 st1 => pop1 (eval k P d ((x, w1) :: g1) st1 e2)
          | r => r
          end
      | EBind binding e1 =>
          match eval_bindings k P d g st [binding] with
          | inl (d1, g1, st1) => eval k P d1 g1 st1 e1
          | inr result => result
          end
            | ESeq e1 e2 =>
          match eval k P d g st e1 with
          | Evaluation (Normal _) g1 st1 => eval k P d g1 st1 e2
          | r => r
          end
      | EAssign x e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match assign_local x w g1 with
              | Some g2 => Evaluation (Normal VUnit) g2 st1
              | None => Evaluation (Stuck e_assign) g1 st1
              end
          | r => r
          end
      | EIf c e1 e2 =>
          match eval k P d g st c with
          | Evaluation (Normal w) g1 st1 =>
              match truthy P w with
              | Some true => eval k P d g1 st1 e1
              | Some false => eval k P d g1 st1 e2
              | None => Evaluation (Stuck e_bool) g1 st1
              end
          | r => r
          end
      | EWhile c b =>
          match eval k P d g st c with
          | Evaluation (Normal w) g1 st1 =>
              match truthy P w with
              | Some true =>
                  match eval k P d g1 st1 b with
                  | Evaluation (Normal _) g2 st2 => eval k P d g2 st2 (EWhile c b)
                  | Evaluation Breaking g2 st2 => Evaluation (Normal VUnit) g2 st2
                  | r => r
                  end
              | Some false => Evaluation (Normal VUnit) g1 st1
              | None => Evaluation (Stuck e_bool) g1 st1
              end
          | r => r
          end
      | ELoop b =>
          match eval k P d g st b with
          | Evaluation (Normal _) g2 st2 => eval k P d g2 st2 (ELoop b)
          | Evaluation Breaking g2 st2 => Evaluation (Normal VUnit) g2 st2
          | r => r
          end
      | EReturn e1 =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 => Evaluation (Returning w) g1 st1
          | r => r
          end
      | EBreak => Evaluation Breaking g st
      | ERecord fs =>
          match eval_arguments k P d g st (map snd fs) with
          | inl (ws, g1, st1) => Evaluation (Normal (VRecord (combine (map fst fs) ws))) g1 st1
          | inr r => r
          end
      | EField e1 f =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match get_field f w with
              | Some v => Evaluation (Normal v) g1 st1
              | None => Evaluation (Stuck e_field) g1 st1
              end
          | r => r
          end
      | EMatch e0 arms =>
          match eval k P d g st e0 with
          | Evaluation (Normal w) g1 st1 =>
              match select_arm arms w with
              | Some (bs, eb) =>
                  popn (List.length bs) (eval k P d (bs ++ g1) st1 eb)
              | None => Evaluation (Stuck e_pat) g1 st1
              end
          | r => r
          end
      end
  end
with eval_arguments (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (l : list expr) {struct fuel} : (list value * locals * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel g st)
  | S k =>
      match l with
      | [] => inl ([], g, st)
      | e1 :: r =>
          match eval k P d g st e1 with
          | Evaluation (Normal w) g1 st1 =>
              match eval_arguments k P d g1 st1 r with
              | inl (ws, g2, st2) => inl (w :: ws, g2, st2)
              | inr rr => inr rr
              end
          | rr => inr rr
          end
      end
  end
with eval_bindings (fuel : nat) (P : program_view) (d : environment) (g : locals) (st : mstate)
    (bs : list binding) {struct fuel} : (environment * locals * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel g st)
  | S k =>
      match bs with
      | [] => inl (d, g, st)
      | BType t tau :: r => eval_bindings k P (bind_type d t tau) g st r
      | BValue v e1 :: r =>
          match eval k P d g st e1 with
          | Evaluation (Normal w1) g1 st1 => eval_bindings k P (install_provision (IVal v) (ValueProvision w1) d) g1 st1 r
          | rr => inr rr
          end
      | BFunction f f' types sm :: r =>
          match select_provisions d sm with
          | Some dc => eval_bindings k P (install_provision (IFn f) (FunctionClosure f' (typed_capture d types dc)) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BMethod tauk m th f' types sm :: r =>
          match select_provisions d sm with
          | Some dc => eval_bindings k P (install_provision (IMth tauk m th) (FunctionClosure f' (typed_capture d types dc)) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BFunctor u h types sm smc :: r =>
          match select_provisions d smc with
          | Some dc => eval_bindings k P (install_provision (IFun u) (FunctorClosure h (Some sm) (typed_capture d types dc) None) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BCopyFunctor u u' sm rm :: r =>
          match lookup_provision (IFun u') d with
          | Some (FunctorClosure h a dc p) =>
              eval_bindings k P (install_provision (IFun u) (FunctorClosure h (compose_adaptation a sm) dc (compose_positions p rm)) d) g st r
          | _ => inr (Evaluation (Stuck e_fun) g st)
          end
      | BApply u th outputs sm rs :: r =>
          match apply_functor k P d u th outputs sm rs st with
          | inl (dr, st1) => eval_bindings k P (append_environment dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | BPrimitive i p :: r => eval_bindings k P (install_provision i (primitive_closure d i p) d) g st r
      end
  end
with apply_functor (fuel : nat) (P : program_view) (d : environment) (u : usym) (th : subst)
    (outputs : list (tsym * tsym)) (sm : smap) (rs : list item) (st : mstate) {struct fuel}
    : (environment * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel [] st)
  | S k =>
      match lookup_provision (IFun u) d with
      | Some (FunctorClosure h a dc p) =>
          match view_functor P h with
          | Some code =>
              let Dr := fc_outputs code in
              let bs := fc_bindings code in
              match select_provisions d sm with
              | Some d1 =>
                  match adapt_provisions d1 a with
                  | Some d2 =>
                      match eval_bindings k P (application_environment P d h th (append_environment d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match select_provisions d3 (result_selection rs p (fc_sources code)) with
                          | Some dr => match result_environment outputs d3 Dr dr with
                              | Some result => inl (result, st3)
                              | None => inr (Evaluation (Stuck e_compose) [] st3) end
                          | None => inr (Evaluation (Stuck e_compose) [] st3)
                          end
                      | inr rr => inr rr
                      end
                  | None => inr (Evaluation (Stuck e_chain) [] st)
                  end
              | None => inr (Evaluation (Stuck e_compose) [] st)
              end
          | None => inr (Evaluation (Stuck e_nobody) [] st)
          end
      | _ => inr (Evaluation (Stuck e_fun) [] st)
      end
  end.

(* Running `main`: its body in the initial environment (the platform's
   provisions for main's needs) and the initial machine. *)
Definition run_main (fuel : nat) (P : program_view) (main : fsym) (d0 : environment)
    (st0 : mstate) : evaluation :=
  match view_function P main with
  | Some body => ret_call [] (eval fuel P d0 [] st0 body)
  | None => Evaluation (Stuck e_nobody) [] st0
  end.
End Interpreter.

(* ######################################################################## *)
(* Part V. The declarative elaboration relation — DRAFT                      *)
(* ######################################################################## *)

(* EaMain.v implements the executable elaborator for these judgments.
   AuditBuildExample.v checks an inhabited public build/run derivation.
   Elaborator soundness and completeness are obligations of PublicFor.CORRECT.

   WHAT IT SAYS.  `Elaborates b p` relates a bundle `b` (Part I) to an
   elaborated program `p` (Part II): `p` is *an* elaboration of `b` if
   there is a static reading `W` of the bundle (which declaration each
   symbol number is, what each name in each module denotes, each symbol's
   requirements) satisfying the conditions of V.1, and every defined
   function's body in `p` is related to its source by the typed judgment
   of V.4–V.6 under the context its assumes form (V.3).

   WHAT MAKES IT DECLARATIVE.  It fixes no algorithm, no traversal order,
   no fuel, and no numbering. Symbol numbers, local-variable numbers,
   fresh generations and synthesized symbols are all
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
   encodes. Historical differences, counterexamples and open design
   questions are recorded in docs/design/audit-notes.md.
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
  | TUnit | TMachine _ => []
  | TRecord fs => (fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (_,t)::rest => gens t ++ go rest end) fs
  | TAbs g => [g]
  | TNom _ xs => (fix go (l : list (tsym * ty)) := match l with [] => [] | p :: r => gens (snd p) ++ go r end) xs
  | TUnion ms => (fix go (l : list ty) := match l with [] => [] | m :: r => gens m ++ go r end) ms
  end.

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

Definition explicit_imports (m : amodule) : list (string * aimport) :=
  combine (am_targets m) (f_imports (am_file m)).

Definition imports_of (b : bundle) (m : amodule) : list (string * aimport) :=
  (if am_prelude m then [(b_prelude b, AImport (b_prelude b) None true [])] else [])
  ++ combine (am_targets m) (f_imports (am_file m)).

Inductive Loaded (b : bundle) : string -> Prop :=
| LoadEntry : Loaded b (b_entry b)
| LoadImport : forall p m t imp,
    Loaded b p -> module_of b p = Some m -> In (t, imp) (imports_of b m) -> Loaded b t.

(* A declaration together with where it is: its module and the assume
   items enclosing it, outermost first ([D20]: requirements nest by union). *)
Record decl := Decl { d_mod : string; d_ast : adecl; d_assumes : list aitem }.

Inductive OccursIn : list adecl -> list aitem -> adecl -> Prop :=
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

(* A provision's signature and its captured interpretation occupy one row.
   Identity is the value/function/functor symbol or receiver-and-method pair;
   type arguments belong to the provision, not to a parallel cache. *)
Inductive provision :=
| ProvidedVal (value_type : ty) (interpretation : inst)
| ProvidedFn (signature : fsig) (interpretation : inst)
| ProvidedMethod (signature : fsig) (interpretation : inst)
| ProvidedFunctor (fixed : subst).
(* A merge records the members of each class and, when the class has a
   structured meaning, a type read from the telescope. *)
Record mclass := MClass {
  mc_members : list tsym;
  mc_shape : option ty
}.
Definition merge := list mclass.

Record cx := Cx {
  c_ty : subst;
  c_sub : subst;
  c_merge : merge;
  c_provisions : list (item * provision);
  c_locals : list (name * (ty * lmut))
}.
(* The static world: a reading of the bundle.  It is a witness: the
   relation asks only that one exist satisfying `WorldOf`. *)
Record world := World {
  w_decl  : nat -> option decl;                      (* symbol -> declaration *)
  w_scope : string -> nspace -> name -> option ent;  (* module, namespace, name -> denotation *)
  w_exp   : string -> nspace -> name -> option ent;  (* what a module exports *)
  w_attached : nat -> name -> option nat;            (* receiver symbol, name -> attached method *)
  w_reqs  : nat -> list nat;                         (* a symbol's requirements (type symbols) *)
  w_library : option string;                        (* prelude's directory prefix *)
  (* The context and needs each fn, method or functor symbol forms.
     Merge classes describe type equality ([D43]), and the stored section
     supplies its spelling during migration. Every rule reading a symbol's
     context uses this one, so call sites, providers and bodies agree on
     canonical types and provision keys. *)
  w_context : nat -> option (cx * list item)
}.

Definition kind_sym (W : world) (s : nat) : option dkind :=
  match w_decl W s with Some d => kind_of (d_ast d) | None => None end.

Definition ast_of (W : world) (s : nat) : option adecl := option_map d_ast (w_decl W s).
Definition mod_of (W : world) (s : nat) : string :=
  match w_decl W s with Some d => d_mod d | None => "" end.

(* [D44]/[D61]/[D64]: what a module exports, and what its scope holds.
   A module exports its own declarations and what it imported by `use`
   (with the use's local name), not what a glob import brought in.  Its
   ordinary scope holds what it exports and everything its explicit glob
   imports export. The implicit prelude fills only unbound names. *)
Inductive Exports (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ExOwn : forall p s d ns n,
    w_decl W s = Some d -> d_mod d = p -> filed_name (d_ast d) = Some (ns, n) ->
    Exports b W p ns n (ESym s)
| ExUse : forall p m t imp u e ns,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = false ->
    In u (i_uses imp) ->
    ns = (if u_dot u then NSDetached else NSName) ->
    (* a plain name is the symbol t exports under it, else the module alias
       (as path resolution reads it): note 25 *)
    (Exports b W t ns (u_name u) e \/
     (u_dot u = false /\ w_exp W t NSName (u_name u) = None /\ Exports b W t NSAlias (u_name u) e)) ->
    Exports b W p (match e with EMod _ => NSAlias | _ => ns end)
            (match u_alias u with Some (_, a) => a | None => u_name u end) e
| ExAlias : forall p m t imp a,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_alias imp = Some a ->
    Exports b W p NSAlias a (EMod t).

Inductive OrdinaryScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScExported : forall p ns n e, Exports b W p ns n e -> OrdinaryScope b W p ns n e
| ScGlob : forall p m t imp ns n e,
    module_of b p = Some m -> In (t, imp) (explicit_imports m) -> i_glob imp = true ->
    Exports b W t ns n e -> OrdinaryScope b W p ns n e.

(* Shadowing points from this module toward the implicit outer scope:
   a local declaration or explicit import hides the matching prelude binding.
   The prelude is visible only when this module has no ordinary binding in
   that namespace. Other names and other modules retain their defaults.
   Ordinary bindings have equal priority: their conflicts are errors under
   WorldOf, not a choice made by import order. Prelude defaults are not exports. *)
Inductive InScope (b : bundle) (W : world) : string -> nspace -> name -> ent -> Prop :=
| ScPrimary : forall p ns n e,
    OrdinaryScope b W p ns n e -> InScope b W p ns n e
| ScPrelude : forall p m ns n e,
    module_of b p = Some m -> am_prelude m = true ->
    Exports b W (b_prelude b) ns n e ->
    (forall e', ~ OrdinaryScope b W p ns n e') -> InScope b W p ns n e.

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

(* [D20]/[D21]: the type symbols an assume nesting requires, through the
   contexts it names, with those its equations write ([D90]).  For a val,
   fn, method, context or functor these are its requirements. *)
Inductive SpecTypes (W : world) : string -> spec -> nat -> Prop :=
| STType : forall p pa a t,
    resolve_sym W p pa = Some t -> kind_sym W t = Some DKType -> SpecTypes W p (Spec (Some pa) None a) t
| STCtx : forall p pa a c d n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    In sp (flat_map citem_specs its) -> SpecTypes W (d_mod d) sp t -> SpecTypes W p (Spec (Some pa) None a) t.

Definition AssumeTypes (W : world) (s : nat) (t : nat) : Prop :=
  exists d sp, w_decl W s = Some d /\ In sp (flat_map aitem_specs (d_assumes d)) /\
               SpecTypes W (d_mod d) sp t.

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
     exactly one symbol. Structural records have no nominal identity *)
  wo_decl_sound : forall s d, w_decl W s = Some d -> Declared b d /\ kind_of (d_ast d) <> None;
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
  wo_library : w_library W = Loader.library_dir (b_prelude b)
}.

(* Library symbols the language itself refers to ([D45], [D68]): the
   symbol in scope as `n` in `<prelude directory>/<short>.moss`.
   Reexports are ordinary scope entries, including here (note 68). *)
Definition LibraryModule (W : world) (short p : string) : Prop :=
  match w_library W with
  | Some dir => p = String.append dir (String.append short ".moss")
  | None => False
  end.

Definition LangSym (W : world) (short : string) (ns : nspace) (n : name) (s : nat) : Prop :=
  exists d p, w_decl W s = Some d /\ LibraryModule W short p /\
              w_scope W p ns n = Some (ESym s).

(* The language items: the library items the language's own rules need
   (criteria section 8), each by its module's short name, namespace and
   name.  The rules outside Platform name library items only through this
   table, `operator_method` (the operators, [D68], [D85]) and the string
   literal's desugaring (Syntax.string_desugar, resolved in scope).
   - Bool, True, False ([D45]): a condition's type, and its two values.
   - zero, one, and num.moss's add, mul and div ([D68]): a numeric
     literal's arithmetic; float.moss's float: a fractional literal's.
   - Gc (note 54): main's root context, the context main assumes
     implicitly. *)
Inductive lang_item := LIBool | LITrue | LIFalse | LIZero | LIOne | LIAdd | LIMul | LIDiv | LIFloat
  | LIRoot.
Definition lang_item_key (i : lang_item) : string * nspace * name :=
  match i with
  | LIBool => ("bool", NSName, "Bool") | LITrue => ("bool", NSName, "True")
  | LIFalse => ("bool", NSName, "False")
  | LIZero => ("int", NSName, "zero") | LIOne => ("int", NSName, "one")
  | LIAdd => ("num", NSDetached, "add") | LIMul => ("num", NSDetached, "mul")
  | LIDiv => ("num", NSDetached, "div")
  | LIFloat => ("float", NSName, "float")
  | LIRoot => ("wasm", NSName, "Gc")
  end.
Definition LangItem (W : world) (i : lang_item) (s : nat) : Prop :=
  let '(short, ns, n) := lang_item_key i in LangSym W short ns n s.

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
    NoDup (map fst fs) ->
    Forall2 (fun nt ft => fst ft = fst nt /\ TyOf W p tm th (snd nt) (snd ft)) fs ts ->
    TyOf W p tm th (ATRec fs) (TRecord ts)
| TyUnion : forall p tm th ms ts flat,
    Forall2 (TyOf W p tm th) ms ts ->
    flat = flat_map members ts ->
    (* Union members have nominal identities, with one payload type per head. *)
    Forall (fun m => exists n xs, m = TNom n xs) flat -> NoDup (map head flat) ->
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

(* The context types lmut, provision and cx are declared before the world
   (V.1), which records each symbol's context. *)
Definition c_vals (C : cx) : list (nat * ty) :=
  flat_map (fun q => match q with
    | (IVal v, ProvidedVal t _) => [(v,t)] | _ => [] end) (c_provisions C).
Definition c_fns (C : cx) : list (nat * fsig) :=
  flat_map (fun q => match q with
    | (IFn f, ProvidedFn g _) => [(f,g)] | _ => [] end) (c_provisions C).
Definition c_mths (C : cx) : list ((ty * nat) * fsig) :=
  flat_map (fun q => match q with
    | (IMth r m _, ProvidedMethod g _) => [((r,m),g)] | _ => [] end) (c_provisions C).
Definition c_funs (C : cx) : list (nat * subst) :=
  flat_map (fun q => match q with
    | (IFun u, ProvidedFunctor fx) => [(u,fx)] | _ => [] end) (c_provisions C).
Definition c_apps (C : cx) : list (item * inst) :=
  map (fun q => (fst q, match snd q with
    | ProvidedVal _ a | ProvidedFn _ a | ProvidedMethod _ a => a
    | ProvidedFunctor fx => map (fun p => (fst p, Some (snd p))) fx end)) (c_provisions C).

Fixpoint mlook (r : ty) (m : nat) (l : list ((ty * nat) * fsig)) : option fsig :=
  match l with
  | [] => None
  | ((r', m'), g) :: rest => if ty_eqb r r' && Nat.eqb m m' then Some g else mlook r m rest
  end.

Definition canon (C : cx) (t : ty) : ty := app_subst (c_sub C) t.
(* A key, and a provision, read through C's merge.  Two contexts choose
   their merges' representatives independently, so a need one context
   spells `X.m` another may spell `Y.m`; read through F's merge, both are
   F's one need (criteria.md section 2, coherence). *)
Definition canon_item (C : cx) (k : item) : item := app_subst_item (c_sub C) k.

(* A provision identity omits the captured instantiation. Runtime references
   retain that annotation so a callee's requirements can name its own interface. *)
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

(* The least congruence generated by the class equations. The template
   rule permits substitution under every type constructor. *)
Inductive MergeEq (M : merge) : ty -> ty -> Prop :=
| MERefl : forall t, MergeEq M t t
| MESym : forall t u, MergeEq M t u -> MergeEq M u t
| METrans : forall t u v, MergeEq M t u -> MergeEq M u v -> MergeEq M t v
| MEClass : forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) ->
    MergeEq M (TAbs x) (TAbs y)
| MEShape : forall c x t, In c M -> In x (mc_members c) -> mc_shape c = Some t ->
    MergeEq M (TAbs x) t
| MECong : forall t s r,
    (forall x, In x (gens t) -> MergeEq M (app_subst s (TAbs x)) (app_subst r (TAbs x))) ->
    MergeEq M (app_subst s t) (app_subst r t).

Definition Solved (M : merge) : Prop :=
  NoDup (flat_map mc_members M) /\
  (forall c t, In c M -> mc_shape c = Some t -> forall x, t <> TAbs x) /\
  exists rank : tsym -> nat,
    (forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) -> rank x = rank y) /\
    (forall c t x y, In c M -> mc_shape c = Some t ->
       In x (mc_members c) -> In y (gens t) -> rank y < rank x).

Definition merge_symbols (M : merge) : list tsym :=
  flat_map (fun c => mc_members c ++
    match mc_shape c with Some t => gens t | None => [] end) M.

(* Expand the structured classes simultaneously. Along any dependency
   path, a solved merge visits each class at most once. *)
Fixpoint shape_table (M : merge) (fuel : nat) : subst :=
  match fuel with
  | 0 => []
  | S n =>
      let rest := shape_table M n in
      flat_map (fun c => match mc_shape c with
        | Some t => map (fun x => (x, app_subst rest t)) (mc_members c)
        | None => []
        end) M
  end.

Definition expand (M : merge) (t : ty) : ty :=
  app_subst (shape_table M (length M)) t.

Definition same_classb (M : merge) (x y : tsym) : bool :=
  Nat.eqb x y ||
  existsb (fun c => existsb (Nat.eqb x) (mc_members c) &&
                     existsb (Nat.eqb y) (mc_members c)) M.

Fixpoint ty_eqb_with (eq_abs : tsym -> tsym -> bool) (a b : ty) {struct a} : bool :=
  match a, b with
  | TMachine a, TMachine b => Nat.eqb a b
  | TRecord fs, TRecord gs =>
      (fix go (fs gs : list (string * ty)) :=
        match fs, gs with
        | [], [] => true
        | (f,t)::fs, (g,u)::gs => String.eqb f g && ty_eqb_with eq_abs t u && go fs gs
        | _, _ => false end) fs gs
  | TUnit, TUnit => true
  | TAbs t1, TAbs t2 => eq_abs t1 t2
  | TNom n1 th1, TNom n2 th2 =>
      Nat.eqb n1 n2 &&
      ((fix go (l1 l2 : list (tsym * ty)) : bool :=
          match l1, l2 with
          | [], [] => true
          | p1 :: r1, p2 :: r2 =>
              Nat.eqb (fst p1) (fst p2) && ty_eqb_with eq_abs (snd p1) (snd p2) && go r1 r2
          | _, _ => false
          end) th1 th2)
  | TUnion m1, TUnion m2 =>
      (fix go (l1 l2 : list ty) : bool :=
         match l1, l2 with
         | [], [] => true
         | t1 :: r1, t2 :: r2 => ty_eqb_with eq_abs t1 t2 && go r1 r2
         | _, _ => false
         end) m1 m2
  | _, _ => false
  end.

Definition merge_eqb (M : merge) (t u : ty) : bool :=
  ty_eqb_with (same_classb M) (expand M t) (expand M u).

(* Keep the outer constructor and its labels, with children erased. *)
Definition type_outer (t : ty) : ty :=
  match t with
  | TMachine m => TMachine m
  | TRecord fs => TRecord (map (fun p => (fst p, TUnit)) fs)
  | TUnit => TUnit
  | TAbs _ => TAbs 0
  | TNom n args => TNom n (map (fun p => (fst p, TUnit)) args)
  | TUnion ms => TUnion (map (fun _ => TUnit) ms)
  end.

Definition type_children (t : ty) : list ty :=
  match t with
  | TRecord fs => map snd fs
  | TNom _ args => map snd args
  | TUnion ms => ms
  | _ => []
  end.

Definition HeadM (M : merge) (t : ty) : Prop := head (expand M t) <> None.

Definition FitsM (M : merge) (t expected : ty) : Prop :=
  MergeEq M t expected \/ MergeEq M t never \/
  exists ms, expand M expected = TUnion ms /\
    forall x, In x (members (expand M t)) -> exists y, In y ms /\ MergeEq M x y.

Inductive JoinM (M : merge) : ty -> ty -> option ty -> ty -> Prop :=
| JoinMExp : forall a b t, JoinM M a b (Some t) t
| JoinMLeftNever : forall a b, MergeEq M a never -> JoinM M a b None b
| JoinMSame : forall a b, (MergeEq M b never \/ MergeEq M a b) ->
    ~ MergeEq M a never -> JoinM M a b None a.

Definition KeyEq (M : merge) (k k' : item) : Prop :=
  match k, k' with
  | IMth t m _, IMth u n _ => m = n /\ MergeEq M t u
  | _, _ => provision_id k = provision_id k'
  end.

Definition item_eqbM (M : merge) (k k' : item) : bool :=
  match k, k' with
  | IMth t m _, IMth u n _ => Nat.eqb m n && merge_eqb M t u
  | _, _ => item_eqb (provision_id k) (provision_id k')
  end.

(* Return the stored key along with its provision. *)
Fixpoint ilookM {A : Type} (M : merge) (k : item)
    (l : list (item * A)) : option (item * A) :=
  match l with
  | [] => None
  | (k', a) :: rest => if item_eqbM M k k' then Some (k', a) else ilookM M k rest
  end.

Fixpoint mlookM {A : Type} (M : merge) (r : ty) (m : msym)
    (l : list ((ty * msym) * A)) : option ((ty * msym) * A) :=
  match l with
  | [] => None
  | ((r', m'), a) :: rest =>
      if Nat.eqb m m' && merge_eqb M r r' then Some ((r', m'), a)
      else mlookM M r m rest
  end.

Definition SigEq (M : merge) (g g' : fsig) : Prop :=
  Forall2 (MergeEq M) (sg_params g) (sg_params g') /\ MergeEq M (sg_ret g) (sg_ret g').

Definition InstEq (M : merge) (a a' : subst) : Prop :=
  Forall2 (fun q r => fst q = fst r /\ MergeEq M (snd q) (snd r)) a a'.

Definition read_inst (a : inst) : subst :=
  map (fun q => (fst q, match snd q with Some t => t | None => TAbs (fst q) end)) a.

Definition ProvEq (M : merge) (p p' : provision) : Prop :=
  match p, p' with
  | ProvidedVal t a, ProvidedVal u b => MergeEq M t u /\ InstEq M (read_inst a) (read_inst b)
  | ProvidedFn g a, ProvidedFn h b | ProvidedMethod g a, ProvidedMethod h b =>
      SigEq M g h /\ InstEq M (read_inst a) (read_inst b)
  | ProvidedFunctor a, ProvidedFunctor b => InstEq M a b
  | _, _ => False
  end.

(* A reading satisfies each equation in its source merge at its target
   merge. A section chooses one spelling for each class during migration. *)
Definition Respects (M : merge) (s : subst) (N : merge) : Prop :=
  (forall c x y, In c M -> In x (mc_members c) -> In y (mc_members c) ->
     MergeEq N (app_subst s (TAbs x)) (app_subst s (TAbs y))) /\
  (forall c x t, In c M -> In x (mc_members c) -> mc_shape c = Some t ->
     MergeEq N (app_subst s (TAbs x)) (app_subst s t)).

Definition SectionOf (M : merge) (s : subst) : Prop :=
  (forall x, MergeEq M (TAbs x) (app_subst s (TAbs x))) /\
  (forall t u, MergeEq M t u -> app_subst s t = app_subst s u).

Definition provides (C : cx) (i : item) : Prop :=
  match i with
  | IVal v => nlook v (c_vals C) <> None
  | IFn f => nlook f (c_fns C) <> None
  | IMth r m _ => mlook r m (c_mths C) <> None
  | IFun u => nlook u (c_funs C) <> None
  | ITy t => nlook t (c_ty C) <> None
  end.

Definition providesM (M : merge) (C : cx) (i : item) : Prop :=
  match i with
  | IMth r m _ => mlookM M r m (c_mths C) <> None
  | _ => provides C i
  end.

Definition provision_inst (p : provision) : inst :=
  match p with
  | ProvidedVal _ a | ProvidedFn _ a | ProvidedMethod _ a => a
  | ProvidedFunctor fx => map (fun q => (fst q, Some (snd q))) fx end.
Definition provision_subst (sigma : subst) (p : provision) : provision :=
  let inst_sub a := map (fun q => (fst q, option_map (app_subst sigma) (snd q))) a in
  match p with
  | ProvidedVal t a => ProvidedVal (app_subst sigma t) (inst_sub a)
  | ProvidedFn g a => ProvidedFn (sig_subst sigma g) (inst_sub a)
  | ProvidedMethod g a => ProvidedMethod (sig_subst sigma g) (inst_sub a)
  | ProvidedFunctor fx => ProvidedFunctor (map (fun q => (fst q, app_subst sigma (snd q))) fx)
  end.
Definition canon_provision (C : cx) (p : provision) : provision := provision_subst (c_sub C) p.
Definition provision_signature (p : provision) : option fsig :=
  match p with
  | ProvidedVal t _ => Some (FSig [] t)
  | ProvidedFn g _ | ProvidedMethod g _ => Some g
  | ProvidedFunctor _ => None end.
Definition provision_refines (promised actual : provision) : Prop :=
  (match promised, actual with
   | ProvidedVal _ _, ProvidedVal _ _ | ProvidedFn _ _, ProvidedFn _ _
   | ProvidedMethod _ _, ProvidedMethod _ _ | ProvidedFunctor _, ProvidedFunctor _ => True
   | _, _ => False end) /\
  provision_signature promised = provision_signature actual /\
  forall t value, In (t, Some value) (provision_inst promised) ->
    In (t, Some value) (provision_inst actual).
Definition provision_gens (p : provision) : list nat :=
  (match provision_signature p with
   | Some g => flat_map gens (sg_ret g :: sg_params g) | None => [] end) ++
  flat_map (fun q => match snd q with Some t => gens t | None => [] end) (provision_inst p).

(* A raw context entry, as an item of a telescope introduces it. *)
Inductive entry :=
| NTy  (t : nat)
| NVal (v : nat) (t : ty) (a : inst)
| NFn  (f : nat) (g : fsig) (a : inst)
| NMth (r : ty) (m : nat) (g : fsig) (a : inst)
| NFun (u : nat) (fx : subst)
(* an equation item's two sides, read ([D90]): no key, only a merge *)
| NEq  (l r : ty).

(* `SigOf W s tm this g`: the declared signature of fn/method s read with
   the type meanings tm (and receiver type `this`). *)
Definition SigOf (W : world) (s : nat) (tm : list (nat * ty)) (this : option ty) (g : fsig) : Prop :=
  exists d rcv n ps rt body,
    w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    Forall2 (fun pt t => TyOf W (d_mod d) tm this (snd pt) t) ps (sg_params g) /\
    match rt with Some r => TyOf W (d_mod d) tm this r (sg_ret g) | None => sg_ret g = TUnit end.

(* A type symbol t that the signature of fn or method s (a parameter's or
   the result's type) mentions (note 51). *)
Definition SigMentions (W : world) (s : nat) (t : nat) : Prop :=
  exists d rcv n ps rt body, w_decl W s = Some d /\ d_ast d = ADFn rcv n ps rt body /\
    ((exists pt, In pt ps /\ FreeIn W (w_reqs W) (d_mod d) (snd pt) t) \/
     (exists r, rt = Some r /\ FreeIn W (w_reqs W) (d_mod d) r t)).

(* The instantiation a provision of s is made at: each requirement's meaning
   (for a method, exactly the requirements its signature mentions, in
   their fixed order: note 51). *)
Inductive Select {A : Type} (P : A -> Prop) : list A -> list A -> Prop :=
| SelectNil : Select P [] []
| SelectKeep : forall x xs selected, P x -> Select P xs selected ->
    Select P (x :: xs) (x :: selected)
| SelectDrop : forall x xs selected, ~ P x -> Select P xs selected ->
    Select P (x :: xs) selected.
(* [D20], sigclose: a method's signature is closed over its requirements,
   which include every type symbol it mentions, so θ records each. The
   mentioned symbols outside its assume nesting (w_reqs) follow the
   nesting's, ascending: a list fixed by its members. *)
Fixpoint Ascending (l : list nat) : Prop :=
  match l with
  | x :: ((y :: _) as r) => x < y /\ Ascending r
  | _ => True
  end.
Definition SigOutside (W : world) (s : nat) (extra : list nat) : Prop :=
  Ascending extra /\ forall t, In t extra <-> SigMentions W s t /\ ~ In t (w_reqs W s).
Definition AppOf (W : world) (s : nat) (tm : subst) (a : inst) : Prop :=
  exists requirements,
    a = map (fun r => (r, nlook r tm)) requirements /\
    ((kind_sym W s = Some DKMeth /\ exists selected extra,
        Select (SigMentions W s) (w_reqs W s) selected /\ SigOutside W s extra /\
        requirements = selected ++ extra) \/
     (kind_sym W s <> Some DKMeth /\ requirements = w_reqs W s)).

(* The receiver type of a method item `R.m`: R's meaning if R is a type
   symbol, else R as a nominal at its requirements' meanings.  A type
   symbol not in force has no meaning: it is not read as a nominal `TNom R
   []` ([D20]: a type an item mentions must already be in force; the
   oracle rejects `assume T.m, T`, tests/errors/assume_method_before_type.moss). *)
Definition RecvTy (W : world) (r : nat) (tm : list (nat * ty)) (t : ty) : Prop :=
  match nlook r tm with
  | Some t' => t = t'
  | None =>
      match w_decl W r with
      | Some d =>
          match d_ast d with
          | ADAlias _ body => TyOf W (d_mod d) tm None body t
          | _ => kind_sym W r <> Some DKType /\ exists args, t = TNom r args /\
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
   for a fn its whole telescope), a method's receiver, and every type
   symbol a fn's or method's signature mentions (sigclose, #12b), less
   what brackets bind, plus the brackets' right sides; closed through
   relevance (w_reqs); less F's outputs. *)
Inductive ResultMentions (W : world) : nat -> string -> spec -> nat -> Prop :=
| RMValFn : forall self p pa a s t,
    resolve_sym W p pa = Some s -> (kind_sym W s = Some DKVal \/ kind_sym W s = Some DKFn) ->
    (In t (w_reqs W s) \/ kind_sym W s = Some DKFn /\ SigMentions W s t) ->
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
    SigMentions W m t ->
    ~ (exists b, In b (match a with Some bs => bs | None => [] end) /\ resolve_sym W p (fst b) = Some t) ->
    ResultMentions W self p (Spec (Some pa) (Some dn) a) t
| RMBracket : forall self p pa d a bp sp dr sa t,
    resolve_sym W p pa <> None ->
    In (bp, Spec (Some sp) dr sa) (match a with Some bs => bs | None => [] end) ->
    FreeIn W (w_reqs W) p (ATRef sp sa) t ->
    ResultMentions W self p (Spec (Some pa) d a) t
| RMCtx : forall self p pa a c dc n its sp t,
    resolve_sym W p pa = Some c -> w_decl W c = Some dc -> d_ast dc = ADCtx n its ->
    In (CISpec sp) its -> ResultMentions W self (d_mod dc) sp t ->
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

(* Each of xs in force as its own generation, unless it is already. *)
Definition add_tys (xs : list nat) (tm : list (nat * ty)) : list (nat * ty) :=
  fold_left (fun tm x => match nlook x tm with Some _ => tm | None => (x, TAbs x) :: tm end) xs tm.

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
with ItemsEntries (W : world) : string -> list (nat * ty) -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| IsNil : forall p ob tm, ItemsEntries W p ob tm [] [] tm
| IsCons : forall p ob tm sp r es1 tm1 es2 tm2,
    ItemEntries W p ob tm sp es1 tm1 -> ItemsEntries W p ob tm1 r es2 tm2 ->
    ItemsEntries W p ob tm (CISpec sp :: r) (es1 ++ es2) tm2
(* [D90]: an equation `l = r` names the references its sides write
   (`ty_heads`), each read as an unbracketed item: a type symbol among them
   is put in force as its own generation unless it is already (rule B: it
   is a requirement, as if assumed).  It then merges its two sides, read
   here: a merge constraint only, with no key and so no need.  The sides'
   order does not matter (`PublicEquation`). *)
| IsEq : forall p ob tm l r es1 tm1 tl tr rest es2 tm2,
    ItemsEntries W p ob tm (map CISpec (ty_heads l ++ ty_heads r)) es1 tm1 ->
    TyOf W p (ob ++ tm1) None l tl -> TyOf W p (ob ++ tm1) None r tr ->
    ItemsEntries W p ob tm1 rest es2 tm2 ->
    ItemsEntries W p ob tm (CIEq l r :: rest) (es1 ++ NEq tl tr :: es2) tm2.

(* An assume item `p` or `R.m` is the context item `p` or `R.m` unbracketed;
   an equation is itself. *)
Definition assume_spec (it : aitem) : citem :=
  match it with
  | AIHead h => CISpec (Spec (Some (fst h)) (snd h) None)
  | AIEq l r => CIEq l r
  end.

(* The key an entry is filed under, read under σ. *)
Definition entry_key (s : subst) (e : entry) : option item :=
  match e with
  | NTy _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey (app_subst s r) m)
  | NFun u _ => Some (IFun u)
  | NEq _ _ => None
  end.

Definition entry_sig (s : subst) (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] (app_subst s t))
  | NFn _ g _ | NMth _ _ g _ => Some (sig_subst s g)
  | _ => None
  end.

(* An entry's instantiation, read under σ.  A requirement not in force
   where the item was read (None) is read as itself: the context's freeze
   completes it from the type table, where each assumed type is its own
   generation. *)
Definition inst_read (s : subst) (a : inst) : list (nat * ty) :=
  map (fun q => (fst q, app_subst s (match snd q with Some t => t | None => TAbs (fst q) end))) a.
Definition entry_inst (s : subst) (e : entry) : option (list (nat * ty)) :=
  match e with
  | NVal _ _ a | NFn _ _ a | NMth _ _ _ a => Some (inst_read s a)
  | _ => None
  end.

(* [D43] Merging.  σ *unifies* the entries if any two entries filed under
   one key (read under σ) have one signature and one instantiation under
   σ: merging two entries of one item also unifies their instantiations,
   so the merged key's instantiation does not depend on which entry comes
   first (criteria section 2, coherence; Δ4).  The merge of a
   telescope is a most general such σ that is idempotent (no cyclic merge:
   a finding of the mechanization, [D43] calculus note), mapping only
   generations of the telescope, each to one value (as the elaborator's
   union-find does; a shadowed second value would never be read, but
   would still sit in the context's substitution).  A merge identifying two distinct
   concrete heads has no unifier, so the telescope does not form. *)
Definition Unifies (s : subst) (es : list entry) : Prop :=
  (forall e1 e2 k, In e1 es -> In e2 es -> entry_key s e1 = Some k -> entry_key s e2 = Some k ->
    entry_sig s e1 = entry_sig s e2 /\ entry_inst s e1 = entry_inst s e2) /\
  (* [D90]: an equation's sides are one type under σ *)
  (forall l r, In (NEq l r) es -> app_subst s l = app_subst s r).

Definition inst_gens (a : inst) : list nat :=
  flat_map (fun q => match snd q with Some t => gens t | None => [] end) a.
Definition sig_gens (g : fsig) : list nat := flat_map gens (sg_ret g :: sg_params g).
Definition entry_gens (e : entry) : list nat :=
  match e with
  | NTy t => [t]
  | NVal _ t a => gens t ++ inst_gens a
  | NFn _ g a => sig_gens g ++ inst_gens a
  | NMth r _ g a => gens r ++ sig_gens g ++ inst_gens a
  | NFun _ fx => flat_map (fun q => gens (snd q)) fx
  | NEq l r => gens l ++ gens r
  end.
Definition MostGeneralMerge (s : subst) (es : list entry) : Prop :=
  Unifies s es /\
  (forall t, app_subst s (app_subst s t) = app_subst s t) /\
  (forall s', Unifies s' es -> (forall t, app_subst s' (app_subst s t) = app_subst s' t)) /\
  (* σ maps only generations of the telescope, to types over them *)
  (forall g t, In (g, t) s -> In g (flat_map entry_gens es) /\ incl (gens t) (flat_map entry_gens es)) /\
  (* σ is a function: no generation has a second, shadowed value *)
  (forall g t, In (g, t) s -> lookup_subst g s = Some t).

Definition raw_entry_key (e : entry) : option item :=
  match e with
  | NTy _ | NEq _ _ => None
  | NVal v _ _ => Some (IVal v)
  | NFn f _ _ => Some (IFn f)
  | NMth r m _ _ => Some (mkey r m)
  | NFun u _ => Some (IFun u)
  end.

Definition raw_entry_sig (e : entry) : option fsig :=
  match e with
  | NVal _ t _ => Some (FSig [] t)
  | NFn _ g _ | NMth _ _ g _ => Some g
  | _ => None
  end.

Definition raw_entry_inst (e : entry) : option subst :=
  match e with
  | NVal _ _ a | NFn _ _ a | NMth _ _ _ a => Some (read_inst a)
  | _ => None
  end.

Definition entry_types (e : entry) : list ty :=
  match e with
  | NTy x => [TAbs x]
  | NVal _ t a => t :: map snd (read_inst a)
  | NFn _ g a => sg_ret g :: sg_params g ++ map snd (read_inst a)
  | NMth r _ g a => r :: sg_ret g :: sg_params g ++ map snd (read_inst a)
  | NFun _ fx => map snd fx
  | NEq l r => [l; r]
  end.

Inductive TypeSubterm (u : ty) : ty -> Prop :=
| SubtermSelf : TypeSubterm u u
| SubtermRecord : forall fs f t, In (f, t) fs -> TypeSubterm u t -> TypeSubterm u (TRecord fs)
| SubtermNominal : forall n args x t, In (x, t) args -> TypeSubterm u t -> TypeSubterm u (TNom n args)
| SubtermUnion : forall ts t, In t ts -> TypeSubterm u t -> TypeSubterm u (TUnion ts).

Definition ShapesRead (es : list entry) (M : merge) : Prop :=
  forall c t, In c M -> mc_shape c = Some t ->
    exists e u, In e es /\ In u (entry_types e) /\ TypeSubterm t u.

Definition OptionEq {A : Type} (R : A -> A -> Prop) (a b : option A) : Prop :=
  match a, b with
  | Some x, Some y => R x y
  | None, None => True
  | _, _ => False
  end.

Definition UnifiesMerge (M : merge) (es : list entry) : Prop :=
  (forall e e' k k', In e es -> In e' es ->
     raw_entry_key e = Some k -> raw_entry_key e' = Some k' -> KeyEq M k k' ->
     OptionEq (SigEq M) (raw_entry_sig e) (raw_entry_sig e') /\
     OptionEq (InstEq M) (raw_entry_inst e) (raw_entry_inst e')) /\
  (forall l r, In (NEq l r) es -> MergeEq M l r).

Definition MergeOf (es : list entry) (M : merge) : Prop :=
  Solved M /\ ShapesRead es M /\ incl (merge_symbols M) (flat_map entry_gens es) /\
  UnifiesMerge M es /\
  forall s, Unifies s es -> forall t u, MergeEq M t u -> app_subst s t = app_subst s u.

(* The context a merged telescope makes: its tables are the entries read
   under σ.  Instantiations keep None for a requirement not in force where
   the item was written, completed from the context (the elaborator's
   "freeze"). *)
Definition entry_provision (s : subst) (freeze : inst -> inst) (e : entry)
    : list (item * provision) :=
  match e with
  | NTy _ => []
  | NVal v t a => [(IVal v, ProvidedVal (app_subst s t) (freeze a))]
  | NFn f g a => [(IFn f, ProvidedFn (sig_subst s g) (freeze a))]
  | NMth r m g a => [(mkey (app_subst s r) m, ProvidedMethod (sig_subst s g) (freeze a))]
  | NFun u fx => [(IFun u, ProvidedFunctor (fixed_subst s fx))]
  | NEq _ _ => []
  end.
Definition entries_cx (s : subst) (M : merge) (tm : subst) (es : list entry) : cx :=
  let freeze a := map (fun q => (fst q, option_map (app_subst s)
    (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a in
  Cx (map (fun q => (fst q, app_subst s (snd q))) tm) s M
     (flat_map (entry_provision s freeze) es) [].

(* The needs of a telescope: its non-type keys, as a set (order is free). *)
Definition Needs (s : subst) (es : list entry) (ns : list item) : Prop :=
  NoDup ns /\ forall k, In k ns <-> exists e, In e es /\ entry_key s e = Some k.

(* [D84] (8), [D37] (note 54): `main` runs in the platform's root context.
   Beside what it assumes, any function in scope as `main` has the root
   context, the language item LIRoot (lib/wasm.moss's `Gc`): each abstract
   functor it lists that is not already in force,
   after the types it is fixed in (X(F), [D84] rule 1), each in force as
   its own generation unless it is already. *)
Definition IsMainFn (W : world) (s : nat) : Prop :=
  exists d n ps rt body p, w_decl W s = Some d /\ d_ast d = ADFn RPlain n ps rt body /\
    w_scope W p NSName "main" = Some (ESym s).


(* `RootItems W q es0 tm its es tm'`: Gc's items `its` (written in module
   q), after the entries es0 and with meanings tm, add the entries es. *)
Inductive RootItems (W : world) (q : string) : list entry -> list (nat * ty) -> list citem -> list entry -> list (nat * ty) -> Prop :=
| RINil : forall es0 tm, RootItems W q es0 tm [] [] tm
| RIFun : forall es0 tm p a rest u du n ps args res xs fx es tm',
    resolve_sym W q p = Some u -> w_decl W u = Some du -> d_ast du = ADFunctor n ps args res None ->
    ~ (exists fx0, In (NFun u fx0) es0) -> NoDup xs -> (forall x, In x xs <-> Fixed W u x) ->
    FixedReadings W u (add_tys xs tm) fx ->
    RootItems W q (es0 ++ map NTy xs ++ [NFun u fx]) (add_tys xs tm) rest es tm' ->
    RootItems W q es0 tm (CISpec (Spec (Some p) None a) :: rest) (map NTy xs ++ [NFun u fx] ++ es) tm'
| RISkip : forall es0 tm sp rest es tm',
    match sp with
    | CISpec (Spec (Some p) None _) => forall u, resolve_sym W q p = Some u -> kind_sym W u = Some DKFunctor -> exists fx, In (NFun u fx) es0
    | _ => True
    end ->
    RootItems W q es0 tm rest es tm' -> RootItems W q es0 tm (sp :: rest) es tm'.

Inductive RootCtx (W : world) (s : nat) (es0 : list entry) (tm : list (nat * ty)) : list entry -> list (nat * ty) -> Prop :=
| RootNone :
    (~ IsMainFn W s \/ forall gc d n its, LangItem W LIRoot gc -> w_decl W gc = Some d -> d_ast d <> ADCtx n its) ->
    RootCtx W s es0 tm [] tm
| RootGc : forall gc d n its es tm',
    IsMainFn W s -> LangItem W LIRoot gc -> w_decl W gc = Some d -> d_ast d = ADCtx n its ->
    RootItems W (d_mod d) es0 tm its es tm' -> RootCtx W s es0 tm es tm'.

(* `CtxOf W s C ns`: the context the assume nesting of symbol s forms (and,
   for `main`, the root context: note 54), and its needs (what a caller
   must provide, [D20]). *)
Definition CtxOf (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ M,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ MergeOf (es ++ er) M /\ SectionOf M σ /\
    C = entries_cx σ M tmr (es ++ er) /\ Needs σ (es ++ er) ns /\
    w_context W s = Some (C, ns).

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
(* A symbol's instantiation in its own context: each type in force there
   as it reads there (a wrapper calling the symbol it wraps). *)
Definition theta_ctx (C : cx) : subst :=
  map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty C).

(* The instantiation of a call of s (assume context Cs) from context C: a
   meaning for each of s's requirements, in their fixed order. Where C
   gives a requirement a meaning, θ reads it here (theta_at). Every merge
   of s's context ([D43]) must hold of θ, including for requirements C
   leaves unbound: merged types are one type, so a merged class bound
   here gives its unbound members that meaning too, and a caller cannot
   bind two members of one class differently.
   θ is principal (criteria, principality): every use of s also asks its
   requirements in force here (`RequirementsInForce`, rule B), so each
   requirement is bound here, or merged in Cs with a member bound here, or
   merged with a structured type whose symbols are bound here, and these
   clauses determine θ up to the merges here. *)
Definition CallTheta (W : world) (C Cs : cx) (s : nat) (th : subst) : Prop :=
  map fst th = map fst (c_ty Cs) /\
  (forall r, In r (map fst (c_ty Cs)) -> nlook r (c_ty C) <> None ->
     lookup_subst r th = Some (canon C (app_subst (c_ty C) (TAbs r)))) /\
  Respects (c_merge Cs) th (c_merge C).

(* Rule B: a requirement may be supplied through any member of its merge
   class. For a structured meaning, the same rule applies to each abstract
   generation remaining after the structured classes have been expanded. *)
Definition InForceClass (C : cx) (M : merge) (r : nat) : Prop :=
  exists y, MergeEq M (TAbs y) (TAbs r) /\ nlook y (c_ty C) <> None.

Definition InForceThrough (C Cs : cx) (r : nat) : Prop :=
  InForceClass C (c_merge Cs) r \/
  let t := expand (c_merge Cs) (TAbs r) in
  (forall z, t <> TAbs z) /\ forall y, In y (gens t) -> InForceClass C (c_merge Cs) y.
Definition RequirementsInForce (C Cs : cx) : Prop :=
  forall r, In r (map fst (c_ty Cs)) -> InForceThrough C Cs r.

(* A signature in Cs's terms, read here at instantiation th. *)
(* A need of the callee, read here at instantiation th. *)
Definition at_th (C : cx) (th : subst) (k : item) : item :=
  match k with
  | IMth r m a => IMth (canon C (app_subst th r)) m a
  | _ => k
  end.
Definition sig_at (C : cx) (th : subst) (g : fsig) : fsig :=
  FSig (map (fun t => canon C (app_subst th t)) (sg_params g)) (canon C (app_subst th (sg_ret g))).
Definition canon_sig (C : cx) (g : fsig) : fsig :=
  FSig (map (canon C) (sg_params g)) (canon C (sg_ret g)).

(* [D20]/[D62]/[D71]: a call of callee s (assume context Cs, needs ns)
   from context C at instantiation th needs the caller's provision of
   each requirement. The provision must be in force at the required
   instantiation, with the type or signature the callee's context gives
   it read at th. The callee's merges ([D43]) must hold here. A
   provision's type can mention types beyond its own requirements. *)
Definition NeedsSatisfied (C Cs : cx) (th : subst) (ns : list item) : Prop :=
  (forall k, In k ns -> provides C (at_th C th k)) /\
  (* coherence: where both say what a requirement means, they agree *)
  (forall k a, In k ns -> ilook k (c_apps Cs) = Some a ->
     forall t want, In (t, Some want) a ->
       match ilook (at_th C th k) (c_apps C) with
       | Some have => In (t, Some (canon C (app_subst th want))) have
       | None => True
       end) /\
  (* types: the provision selected here has the callee's view of it *)
  (forall k p, In k ns -> ilook k (c_provisions Cs) = Some p ->
     exists p', ilook (at_th C th k) (c_provisions C) = Some p' /\
       option_map (canon_sig C) (provision_signature p') = option_map (sig_at C th) (provision_signature p)) /\
  (* merges: every merge class of the callee whose members, and the
     classes of the symbols of its structure, are all in force here holds
     of the caller's meanings.  Stated by class (criteria section 2,
     coherence): the classes do not depend on the callee's choice of
     representatives (PublicOrientation.mgm_classes_iff), while the
     equations of its σ do.  A class only partly in force here is not
     checked under any representative (a closure's class with a linked
     member: BFunClosure; before, an equation of σ was skipped only when it
     mentioned a symbol not in force, so such a class was checked under a
     representative that is captured and not under one that is linked).
     At a call, CallTheta's merge clause already holds of every class. *)
  (forall g, (forall y, (exists x, (x = g \/ In x (gens (canon Cs (TAbs g)))) /\
                          canon Cs (TAbs y) = canon Cs (TAbs x)) -> nlook y (c_ty C) <> None) ->
     canon C (app_subst (c_ty C) (TAbs g)) = canon C (app_subst (c_ty C) (canon Cs (TAbs g)))).

(* Translation uses the same provision readings to pair each callee
   key with the caller's run-time key. *)
Definition satisfaction_map (C Cs : cx) (th : subst) (ns : list item) : smap :=
  map (fun k => (rkey_of Cs k, rkey_of C (at_th C th k))) ns.

Definition translate_call (C Cs : cx) (f : fsym) (th : subst)
    (needs : list item) (args : list expr) : expr :=
  ECall f th (satisfaction_map C Cs th needs) args.

Definition Satisfies (C Cs : cx) (th : subst) (ns : list item) (sm : smap) : Prop :=
  sm = satisfaction_map C Cs th ns /\ NeedsSatisfied C Cs th ns.

(* Arguments checked against parameter types ([D17] fits), elaborated in order. *)

(* ======================================================================== *)
(* V.5 Functors ([D55], [D78], [D84] with DECISIONS.md)                       *)
(* ======================================================================== *)

(* F's own terms: the context its argument telescope forms, plus its
   implicit mentioned inputs (symbols the explicit inputs do not list). *)
Definition FunCtx (W : world) (u : nat) (C : cx) (ns : list item) : Prop :=
  exists d ps a r b es tm tm' σ M implicit,
    w_decl W u = Some d /\ functor_parts (d_ast d) = Some (ps, a, r, b) /\
    ItemsEntries W (d_mod d) [] [] (map CISpec a) es tm /\
    (* implicit inputs: only an abstract functor has them *)
    (forall t, In t implicit <-> (b = None /\ Mentions W u t /\ nlook t tm = None)) /\
    tm' = map (fun t => (t, TAbs t)) implicit ++ tm /\
    MostGeneralMerge σ es /\ MergeOf es M /\ SectionOf M σ /\
    C = entries_cx σ M tm' es /\ Needs σ es ns /\
    w_context W u = Some (C, ns).

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
(* a result's context holds no equation: an equation is a requirement
   ([D90]), and a result provides *)
| OIECtx : forall p ob tm a pa c d n its sps bs es tm',
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    OutputBrackets W p (ob ++ tm) a bs ->
    OutputItemsEntries W (d_mod d) (bs ++ ob) tm sps es tm' ->
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

Definition output_cx (C : cx) (tm : subst) (es : list entry) : cx :=
  Cx (map (fun q => (fst q, canon C (snd q))) tm) (c_sub C) (c_merge C)
    (flat_map (entry_provision (c_sub C) (fun a => a)) (rev es) ++ c_provisions C)
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
| FRContext : forall mi pa br u d n its sps,
    resolve_sym W mi pa = Some u -> w_decl W u = Some d ->
    d_ast d = ADCtx n its -> its = map CISpec sps ->
    Forall (FamilyResult W (d_mod d)) sps ->
    FamilyResult W mi (Spec (Some pa) None br).

(* A family's listed polymorphic inputs are type symbols ([D84] rule 1; the
   oracle's functor_params rejects any other, tests/errors/family_poly_nontype.moss). *)
Definition FamilyShape (W : world) (u : nat) : Prop :=
  exists d n ps a r, w_decl W u = Some d /\ d_ast d = ADFunctor n ps a r None /\
    Forall (FamilyResult W (d_mod d)) r /\
    (forall names x t, ps = Some names -> In x names -> resolve_sym W (d_mod d) [x] = Some t ->
       kind_sym W t = Some DKType).

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
| FKContext : forall p pa a c d n its sps,
    resolve_sym W p pa = Some c -> w_decl W c = Some d -> d_ast d = ADCtx n its ->
    its = map CISpec sps ->
    Forall (FamilyKeyed W u (d_mod d)) sps ->
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
  fr_mod : string; fr_ret : option ty; fr_this : option ty; fr_loop : bool;
  fr_var : list (name * nat); fr_fresh : list nat;
  fr_applying : list nat;   (* functors being applied by name: [D55] no cycle *)
  fr_pins : list nat        (* in a body bound to F: P(F) ∩ M(F), which it may not rebind ([D84] 3d) *)
}.

(* a local's number, for a name the numbering covers ([Named]) *)
Definition var_of (F : frame) (n : name) : nat :=
  match slook n (fr_var F) with Some k => k | None => 0 end.
Definition Named (F : frame) (n : name) : Prop := slook n (fr_var F) <> None.
(* the numbering is injective, and numbers the parameters 0, 1, … after `this` *)
Definition Numbering (F : frame) (this : option ty) (params : list name) : Prop :=
  NoDup (map fst (fr_var F)) /\ NoDup (map snd (fr_var F)) /\
  (* `this` is a keyword, never a numbered name: with a receiver, slot 0
     is the receiver alone, so no local can shadow it. *)
  (match this with Some _ => forall n, slook n (fr_var F) <> Some 0
                   | None => True end) /\
  (forall i pn, nth_error params i = Some pn ->
     slook pn (fr_var F) = Some (match this with Some _ => S i | None => i end)).

Definition with_loop (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) true (fr_var F) (fr_fresh F) (fr_applying F) (fr_pins F).
Definition with_mod (F : frame) (p : string) (u : nat) : frame :=
  Frame p (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (u :: fr_applying F) (fr_pins F).
Definition with_fresh (F : frame) (gs : list nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F ++ gs) (fr_applying F) (fr_pins F).
(* A nested block's frame: [D84] 3d's pins constrain only a linked body's
   own binds, as a lambda body's frame is unconstrained. *)
Definition without_pins (F : frame) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F) (fr_applying F) [].

Definition add_local (C : cx) (n : name) (t : ty) (mu : lmut) : cx :=
  Cx (c_ty C) (c_sub C) (c_merge C) (c_provisions C) ((n, (t, mu)) :: c_locals C).
Definition set_ty (C : cx) (t : nat) (tau : ty) : cx :=
  Cx ((t, tau) :: c_ty C) (c_sub C) (c_merge C) (c_provisions C) (c_locals C).
Definition set_provision (C : cx) (k : item) (p : provision) : cx :=
  Cx (c_ty C) (c_sub C) (c_merge C) ((k,p) :: filter (fun q => negb (item_eqb k (fst q))) (c_provisions C)) (c_locals C).
Definition provide (C : cx) (k : item) (a : inst) (C1 : cx) : cx :=
  match ilook k (c_provisions C1) with
  | Some (ProvidedVal t _) => set_provision C1 k (ProvidedVal t a)
  | Some (ProvidedFn g _) => set_provision C1 k (ProvidedFn g a)
  | Some (ProvidedMethod g _) => set_provision C1 k (ProvidedMethod g a)
  | _ => C1
  end.
Definition set_val (C : cx) (v : nat) (t : ty) : cx :=
  set_provision C (IVal v) (ProvidedVal t []).
Definition set_fn (C : cx) (f : nat) (g : fsig) : cx :=
  set_provision C (IFn f) (ProvidedFn g []).
Definition set_mth (C : cx) (r : ty) (m : nat) (g : fsig) : cx :=
  set_provision C (mkey r m) (ProvidedMethod g []).
Definition set_fun (C : cx) (u : nat) (fx : subst) : cx :=
  set_provision C (IFun u) (ProvidedFunctor fx).

Definition body_of (P : program) (f : nat) : option expr := nlook f (program_functions P).
Definition funbody_of (P : program) (h : nat) : option functor_code := nlook h (program_functors P).

(* A symbol elaboration makes (a lambda's provider, a forwarder, a wrapper,
   a captured local's val, a generation, a linked functor body) is not a
   source symbol. *)
Definition Fresh (W : world) (s : nat) : Prop := w_decl W s = None.

(* A generation is fresh for this scope when it is absent from the source
   world and from every type currently reachable in the static environment,
   including its merges: a merged-away key keeps its meaning (that of its
   representative) wherever the context is captured. *)
Definition context_gens (C : cx) : list nat :=
  flat_map (fun q => fst q :: gens (snd q)) (c_sub C) ++
  flat_map (fun q => gens (snd q) ++ gens (canon C (snd q))) (c_ty C) ++
  flat_map (fun q => provision_gens (snd q) ++
    provision_gens (provision_subst (c_sub C) (snd q))) (c_provisions C) ++
  flat_map (fun q => gens (fst (snd q)) ++ gens (canon C (fst (snd q)))) (c_locals C).
(* The generations a method key's receiver mentions (the fresh-output
   clause of BApplyAbstract, #10, names them). *)
Definition key_receiver_gens (C : cx) : list nat :=
  flat_map (fun q => match fst q with IMth r _ _ => gens r | _ => [] end) (c_provisions C).

Definition context_type_support (C : cx) : list nat :=
  map fst (c_ty C) ++ context_gens C ++ key_receiver_gens C.
(* A lambda or closure keeps the meanings of the generations its context
   mentions.  A method key's receiver mentions only those: rule B puts a
   callee's requirements in force at every use, so a family demand keys
   its methods at types in force (PublicFormation.KeysScoped). *)
Definition lexical_capture (C : cx) : type_capture :=
  TypeCapture
    (map (fun generation => (generation, TAbs generation)) (context_gens C))
    (map (fun q => (fst q, canon C (snd q))) (c_ty C)).
(* An allocation scope reserves identities from the surrounding namespace
   and its live support. Its allocation history grows by whole batches. *)
Record allocation_scope (identity : Type) := AllocationScope {
  scope_permits : identity -> Prop;
  scope_allocated : list identity;
  scope_support : list identity
}.

Definition AllocationFresh {identity} (before : allocation_scope identity)
    (batch : list identity) : Prop :=
  NoDup batch /\ Forall (fun g => scope_permits before g /\
    ~ In g (scope_allocated before ++ scope_support before)) batch.

Definition extend_allocation_scope {identity} (before : allocation_scope identity)
    (batch : list identity) : allocation_scope identity :=
  AllocationScope (scope_permits before) (scope_allocated before ++ batch) (scope_support before).

Definition Allocates {identity} (before : allocation_scope identity)
    (batch : list identity) (after : allocation_scope identity) : Prop :=
  AllocationFresh before batch /\ after = extend_allocation_scope before batch.

(* The existing frame and context supply the initial scope. The history is
   also the frame's record of generations that may not escape its block. *)
Definition body_allocation_scope (W : world) (F : frame) (C : cx) : allocation_scope nat :=
  AllocationScope (Fresh W) (fr_fresh F)
    (context_gens C ++
     (match fr_ret F with Some t => gens t | None => [] end) ++
     match fr_this F with Some t => gens t | None => [] end).

Definition FreshFor (W : world) (F : frame) (C : cx) (gs : list nat) : Prop :=
  AllocationFresh (body_allocation_scope W F C) gs.

Definition frame_after_allocation (F : frame) (after : allocation_scope nat) : frame :=
  Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F)
    (scope_allocated after) (fr_applying F) (fr_pins F).

(* The meaning of a decimal literal ([D68]): digits, an optional fraction
   and an optional exponent, as numerator / denominator. *)
Fixpoint digits_val (l : list ascii) (acc : nat) : nat :=
  match l with [] => acc | c :: r => digits_val r (10 * acc + (nat_of_ascii c - 48)) end.

Fixpoint split_at (c : ascii) (l : list ascii) (acc : list ascii) : list ascii * option (list ascii) :=
  match l with
  | [] => (rev acc, None)
  | x :: r => if Ascii.eqb x c then (rev acc, Some r) else split_at c r (x :: acc)
  end.

(* Literal syntax determines an arithmetic tree, including evaluation order.
   Algebraically equal trees are not interchangeable: contextual operations
   may have effects, overflow, trap, or round differently. *)
Inductive literal_tree :=
| LZero | LOne
| LAdd (x y : literal_tree) | LMul (x y : literal_tree)
| LDiv (x y : literal_tree) | LFloat (x : literal_tree).
Definition small_nat (n : nat) : literal_tree :=
  match n with 0 => LZero | S k => Nat.iter k (fun x => LAdd x LOne) LOne end.
Fixpoint strip_digit_zeros (ds : list ascii) : list ascii :=
  match ds with "0"%char :: rest => strip_digit_zeros rest | _ => ds end.
Definition digit_tree (d : ascii) := small_nat (nat_of_ascii d - 48).
Definition integer_tree (ds : list ascii) : literal_tree :=
  match strip_digit_zeros ds with
  | [] => LZero
  | first :: rest => fold_left (fun acc d =>
      let product := LMul acc (small_nat 10) in
      if Ascii.eqb d "0"%char then product else LAdd product (digit_tree d))
      rest (digit_tree first)
  end.
Definition literal_tree_of (text : string) : literal_tree :=
  let '(mant, ex) := split_at "e"%char (list_ascii_of_string text) [] in
  let '(whole, frac) := split_at "."%char mant [] in
  match ex, frac with
  | None, None => integer_tree whole
  | _, _ =>
      let fraction := match frac with Some ds => ds | None => [] end in
      let '(negative, magnitude) := match ex with
        | Some ("-"%char :: ds) => (true, digits_val ds 0)
        | Some ds => (false, digits_val ds 0)
        | None => (false, 0) end in
      let '(divide, power) := if negative then (true, magnitude + length fraction)
        else if Nat.leb (length fraction) magnitude then (false, magnitude - length fraction)
        else (true, length fraction - magnitude) in
      let ten := LFloat (small_nat 10) in
      let value := match strip_digit_zeros (whole ++ fraction) with
        | [] => LFloat LZero
        | first :: rest => fold_left (fun acc d =>
            let product := LMul acc ten in
            if Ascii.eqb d "0"%char then product
            else LAdd product (LFloat (digit_tree d))) rest (LFloat (digit_tree first)) end in
      match power with
      | 0 => value
      | S k => let scale := Nat.iter k (fun acc => LMul acc ten) ten in
          if divide then LDiv value scale else LMul value scale
      end
  end.
Definition NumericText (text : string) : Prop :=
  exists raw kind, Syntax.Lexeme (String.list_byte_of_string raw) kind [] /\
    (kind = Syntax.INTEGER \/ kind = Syntax.DECIMAL) /\ Syntax.lit_norm raw = text.

(* [D84] rule 7 / DECISIONS.md: at most one family in force produces a key.
   Here θ is the matching substitution over P(F), not the full emitted
   application argument list; fixed caller meanings cannot be overridden. *)
Definition FixedAgreement (W : world) (C : cx) (u : nat) : Prop :=
  forall x, Fixed W u x ->
    match ilook (IFun u) (c_apps C), nlook x (c_ty C) with
    | Some recorded, Some now =>
        match nlook x recorded with
        | Some (Some was) => MergeEq (c_merge C) was now
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

Definition family_context (C : cx) (matching : subst) : cx :=
  fold_left (fun C q => set_ty C (fst q) (snd q)) matching C.
Definition FamilyApplication (W : world) (C : cx) (u : usym)
    (matching arguments : subst) (inputs : smap) (result : cx) (keys : list item) : Prop :=
  exists fixed formal needs,
    nlook u (c_funs C) = Some fixed /\ Family W u /\
    Forall (fun q => Poly W u (fst q)) matching /\
    FunCtx W u formal needs /\
    arguments = map (fun q => (fst q,
      canon (family_context C matching)
        (app_subst (c_ty (family_context C matching)) (TAbs (fst q))))) (c_ty formal) /\
    (forall t, In t (map fst (c_ty formal)) -> nlook t (c_ty (family_context C matching)) <> None) /\
    FixedAgreement W (family_context C matching) u /\
    Satisfies (family_context C matching) formal arguments needs inputs /\
    ProvideResult W u (family_context C matching) (fun t => TAbs t) result keys /\
    NoDup (map provision_id keys).

(* Rule B for a family demand (criteria sections 2 and 3): the matching
   read off the demanded key is in force here, each generation it
   mentions one that a type in force here means.  A key whose receiver
   mentions a symbol out of force (a callee requirement left unbound,
   before rule B) is not demanded. *)
Definition MatchingInForce (C : cx) (matching : subst) : Prop :=
  forall q y, In q matching -> In y (gens (canon C (snd q))) ->
    exists x u, In (x, u) (c_ty C) /\ In y (gens (canon C u)).

(* [D84] rule 7: θ is read off the demanded key.  The result is formed once
   with each P(F) symbol standing for itself (the oracle's markers), and the
   demanded method's formal receiver there is matched against the key's
   receiver: θ's domain is exactly the P(F) symbols that receiver mentions,
   and each value is the part of the key at that symbol.  The member is
   unique: every item of the family with method m whose receiver matches
   the key gives the same matching (up to the merges here), else which one
   is meant is ambiguous and the demand fails (D84 rule 7; the oracle's
   `family_member`, tests/errors/family_ambiguous_member.moss). *)
Definition FamilyReadOff (W : world) (C : cx) (u : usym) (ot : ty) (m : nat)
    (matching : subst) : Prop :=
  exists marks formal_result formal_keys rf a,
    (forall t, Poly W u t <-> In t marks) /\
    ProvideResult W u (family_context C (map (fun t => (t, TAbs t)) marks))
      (fun t => TAbs t) formal_result formal_keys /\
    In (IMth rf m a) formal_keys /\
    (forall t, In t (map fst matching) <-> In t marks /\ In t (gens rf)) /\
    NoDup (map fst matching) /\ canon C (app_subst matching rf) = ot /\
    (forall rf' a' matching', In (IMth rf' m a') formal_keys ->
       (forall t, In t (map fst matching') <-> In t marks /\ In t (gens rf')) ->
       canon C (app_subst matching' rf') = ot ->
       forall t, option_map (canon C) (lookup_subst t matching') =
                 option_map (canon C) (lookup_subst t matching)).

(* A literal's binary arithmetic call uses a provision already in force,
   or a unique family applied in a scope around that call ([D84] rule 7).
   Its one parameter and result are exactly the literal's own type. *)
Inductive LitOp (W : world) (C : cx) (t : ty) (m : nat) (ox oy : expr) : expr -> Prop :=
| LitOpIn :
    mlook t m (c_mths C) = Some (FSig [t] t) ->
    LitOp W C t m ox oy (EMethod ox m (rkey C t m) [oy])
| LitOpFamily : forall u matching th sm C' ks,
    mlook t m (c_mths C) = None ->
    FamilyApplication W C u matching th sm C' ks -> FamilyReadOff W C u t m matching ->
    In (mkey t m) ks -> OnlyFamily W C u (mkey t m) ->
    mlook t m (c_mths C') = Some (FSig [t] t) -> MatchingInForce C matching ->
    LitOp W C t m ox oy (apply_expression u th sm (map (rkey_of C') ks) (EMethod ox m (rkey C' t m) [oy])).

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
| PUnit : forall pa s k xs,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some k -> (k = DKUnit \/ k = DKTag) ->
    In (TNom s xs) (members st) ->
    PatOf W F st (APPath pa) (PHead s None []) [] [s] false
(* `S(y)`: y is the payload, at the member's arguments; a struct tag's is a
   bare record type (note 35) *)
| PTagBind : forall pa s xs y pay,
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag -> In (TNom s xs) (members st) ->
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

(* `let` binds an immutable local, `var` an assignable one. *)
Inductive LetStmt : astmt -> name -> option aty -> aexpr -> lmut -> Prop :=
| LSLet : forall n a y, LetStmt (ASLet n a y) n a y LLet
| LSVar : forall n a y, LetStmt (ASVar n a y) n a y LVar.

(* Whether a `break` in an expression, block or statement leaves the
   innermost loop around it: one not inside a nested `while` or `loop`
   (note 42). *)
Inductive Breaks (W : world) : string -> aexpr -> Prop :=
| BkBreak : forall md,Breaks W md AEBreak
| BkCall : forall md p a args y, In y args -> Breaks W md y -> Breaks W md (AECall p a args)
| BkField : forall md o n, Breaks W md o -> Breaks W md (AEField o n)
| BkMethRecv : forall md o p args, Breaks W md o -> Breaks W md (AEMeth o p args)
| BkMethArg : forall md o p args y, In y args -> Breaks W md y -> Breaks W md (AEMeth o p args)
| BkRec : forall md p a fs n y, In (n, Some y) fs -> Breaks W md y -> Breaks W md (AERec p a fs)
| BkIfC : forall md c b e, Breaks W md c -> Breaks W md (AEIf c b e)
| BkIfB : forall md c b e, BlockBreaks W md b -> Breaks W md (AEIf c b e)
| BkIfE : forall md c b y, Breaks W md y -> Breaks W md (AEIf c b (Some y))
| BkMatchS : forall md s arms, Breaks W md s -> Breaks W md (AEMatch s arms)
| BkMatchA : forall md s arms pt y, In (pt, y) arms -> Breaks W md y -> Breaks W md (AEMatch s arms)
| BkReturn : forall md y, Breaks W md y -> Breaks W md (AEReturn (Some y))
| BkBlock : forall md b, BlockBreaks W md b -> Breaks W md (AEBlock b)
| BkUnary : forall md op y, Breaks W md y -> Breaks W md (AEUnary op y)
| BkBinL : forall md op l r, Breaks W md l -> Breaks W md (AEBinary op l r)
| BkBinR : forall md op l r, Breaks W md r -> Breaks W md (AEBinary op l r)
with BlockBreaks (W : world) : string -> ablock -> Prop :=
| BBStmt : forall md ss tl s, In s ss -> StmtBreaks W md s -> BlockBreaks W md (ABlock ss tl)
| BBTail : forall md ss y, Breaks W md y -> BlockBreaks W md (ABlock ss (Some y))
with StmtBreaks (W : world) : string -> astmt -> Prop :=
| SBLet : forall md n t y, Breaks W md y -> StmtBreaks W md (ASLet n t y)
| SBVar : forall md n t y, Breaks W md y -> StmtBreaks W md (ASVar n t y)
| SBAssign : forall md n y, Breaks W md y -> StmtBreaks W md (ASAssign n y)
| SBExpr : forall md y, Breaks W md y -> StmtBreaks W md (ASExpr y)
| SBBind : forall md its it, In it its -> ItemBreaks W md it -> StmtBreaks W md (ASBind its)
(* a `while` condition is evaluated outside the loop it controls *)
| SBWhile : forall md c b, Breaks W md c -> StmtBreaks W md (ASWhile c b)
(* a bind item breaks when its expression does, or, for `bind F;` with F
   a functor with a body, when an item of F's body does: the body's binds
   run in place, inside the loop around the `bind` (read in F's module) *)
with ItemBreaks (W : world) : string -> spec * option aexpr -> Prop :=
| IBExpr : forall md sp y, Breaks W md y -> ItemBreaks W md (sp, Some y)
| IBApply : forall md pa u d n ps a r bodies it,
    resolve_sym W md pa = Some u -> w_decl W u = Some d -> d_ast d = ADFunctor n ps a r (Some bodies) ->
    In it (concat bodies) -> ItemBreaks W (d_mod d) it ->
    ItemBreaks W md (Spec (Some pa) None None, None).

(* The type a block has as a branch of an `if` or a `match` arm: its
   tail's, and `()` without a tail, even if it cannot fall through
   (note 42). *)
Definition branch_ty (b : ablock) (t : ty) : ty :=
  match b with ABlock _ None => TUnit | ABlock _ (Some _) => t end.

(* Auxiliary judgments (not mutually recursive with the main ones). *)

(* A forwarder's identity is determined by the abstract input it invokes.
   Typing checks that input in the context. Translation assigns the
   forwarder's function body its program-table number. *)
Record forwarder_handle := ForwarderOf { forwarder_source : nat }.

Definition TypesForwarder (W : world) (F : frame) (C : cx)
    (pa : path) (sg : fsig) (handle : forwarder_handle) : Prop :=
  let g := forwarder_source handle in
  resolve_sym W (fr_mod F) pa = Some g /\ kind_sym W g = Some DKFn /\
  (exists nm ps rt, ast_of W g = Some (ADFn RPlain nm ps rt None)) /\
  nlook g (c_fns C) = Some sg.

Definition forwarder_body (handle : forwarder_handle) (sg : fsig) : expr :=
  EInvoke (forwarder_source handle) (map EVar (seq 0 (length (sg_params sg)))).

Definition TranslatesForwarder (W : world) (P : program) (handle : forwarder_handle)
    (sg : fsig) (number : nat) : Prop :=
  Fresh W number /\ body_of P number = Some (forwarder_body handle sg).

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
    d_ast d = ADCtx n its -> In sp (flat_map citem_specs its) ->
    ResultSpecLeaf W (d_mod d) sp k ->
    ResultSpecLeaf W p (Spec (Some pa) None br) k.
Inductive ResultLeaf (W : world) : nat -> (nat * option name) -> Prop :=
| RL : forall u d n ps a r b sp k,
    w_decl W u = Some d -> d_ast d = ADFunctor n ps a r b ->
    In sp r -> ResultSpecLeaf W (d_mod d) sp k -> ResultLeaf W u k.

(* `CollidesOnlyAs ks ks0`: two positions of a result that `ks` files
   under one provision id are one item in the reading `ks0` too (the same
   result read elsewhere, position by position).  A collision `ks0` lacks
   would leave which item is installed to a choice ([D84] rule 7's
   ambiguity, for a non-family functor; criteria section 2). *)
Definition CollidesOnlyAs (ks ks0 : list item) : Prop :=
  forall i j k k', nth_error ks i = Some k -> nth_error ks j = Some k' ->
    provision_id k = provision_id k' ->
    exists k0 k0', nth_error ks0 i = Some k0 /\ nth_error ks0 j = Some k0' /\
      provision_id k0 = provision_id k0'.

(* [D78]/[D84] 3e: G's result is F's, leaf by leaf. *)
Definition SameResult (W : world) (u g : nat) : Prop :=
  forall k, ResultLeaf W u k <-> ResultLeaf W g k.

(* [D78] (the oracle's copy_output_compatible): `bind F = H;` is
   checked at every application of F at once. F's fixed inputs read as
   sigma says (what they mean where F is bound, which every application
   keeps, BApplyAbstract's rule 5), and every other generation of F's
   input readings (one sigma does not read: F's non-fixed inputs, which
   each application supplies afresh) reads as its own fresh symbol cg y,
   as the oracle replaces each unsolved generation by a fresh `@copy`
   symbol. copy_subst is that reading. H's terms are read in F's through
   F's merge (H's inputs are among F's), then at copy_subst, and there:
   - H's inputs read as F's: each of H's merges holds. A merge of H that
     F lacks is accepted when F's fixed readings make it hold, as they do
     at every application (tests/wasi/copy_site_merge.moss); one a later application
     could break is not (copy-poly-merge.moss, tests/errors/copy_merge.moss).
   - H's needs read as F's. A candidate must give the same key,
     signature and instantiation at the copy site. If several candidates
     agree there, the unique one already equal in the declared merges
     supplies the provision. An unresolved choice would be observable
     when an application has separate provisions for the target keys.
   - H's result gives each key of F's result the provision F's result
     gives it (each key's last setter), both read with the same fresh
     output roles. H's result is read in its own context Ch, where its
     merges hold. rm records, for each position i of F's result, the
     position j of H's whose item F's i-th is: H's key there is F's key
     at copy_subst, H's provision there agrees with F's, and every
     position of H's with that key at copy_subst has one key in H's own
     terms, of which j is the last (the oracle's `find ... (rev gkeys)`).
     So two result items distinct in H's own terms are never one key of
     F's, where choosing either would be a choice (coherence, criteria
     section 2; tests/errors/copy_result_collide.moss), and repeated
     positions of F read one source. An application of F installs
     exactly F's result, position by position under the caller's keys
     (Rule A, result_selection), so a key H has and F lacks is never
     installed (tests/wasi/copy_extra_key.moss).
   sigma's readings are the copy site's and may name a symbol that is also
   one of F's inputs, which must not be identified with it
   (tests/errors/copy_bracket_nonfixed.moss, tests/errors/copy_fixed_reads_poly.moss;
   scratch/orch/copyfresh-AUDIT.md). Fresh means fresh for sigma's image
   too, as the oracle's @copy symbols are: a copy site can read a fixed
   input as a fresh application generation, and a cg y or output role
   equal to it would identify it with F's input or output again
   (tests/errors/copy_fresh_collide.moss, scratch/orch/realizepf-AUDIT.md). *)
Definition copy_subst (sigma : subst) (ys : list nat) (cg : nat -> nat) : subst :=
  sigma ++ map (fun y => (y, TAbs (cg y))) ys.

(* Fixed readings of an atomic class use one slot of the current
   section. A class with different recorded readings stays symbolic. *)
Definition copy_fixed_slot (C : cx) (x : nat) : nat :=
  match canon C (TAbs x) with TAbs y => y | _ => x end.

Definition copy_fixed_readings (C : cx) (sigma : subst) : subst :=
  let readings := map (fun q => (copy_fixed_slot C (fst q), snd q)) sigma in
  filter (fun q =>
    forallb (fun r => negb (Nat.eqb (fst q) (fst r)) || ty_eqb (snd q) (snd r)) readings)
    readings.

Definition copy_allocation_scope (W : world) (sigma : subst) : allocation_scope nat :=
  AllocationScope (Fresh W) [] (flat_map (fun q => gens (snd q)) sigma).

Definition copy_need_key (C : cx) (s : subst) (k : item) : item :=
  app_subst_item s (canon_item C (provision_id k)).

Definition merge_equations (M : merge) : list entry :=
  flat_map (fun c =>
    match mc_members c with
    | [] => []
    | x :: rest =>
        let members := map (fun y => NEq (TAbs x) (TAbs y)) rest in
        match mc_shape c with
        | Some t => NEq (TAbs x) t :: members
        | None => members
        end
    end) M.

Definition CopyNeedAt (C : cx) (s : subst) (k : item) (p : provision) (k' : item) : Prop :=
  copy_need_key C s k = copy_need_key C s k' /\
  exists p', ilook (provision_id k') (c_provisions C) = Some p' /\
    provision_subst s (canon_provision C p) = provision_subst s (canon_provision C p').

Definition CopyNeedChoice (Cu Ch : cx) (s : subst) (needs : list item)
    (k : item) (p : provision) (target : item) : Prop :=
  In target needs /\ CopyNeedAt Cu s k p target /\
  ((forall other, In other needs -> CopyNeedAt Cu s k p other -> other = target) \/
   exists M, MergeOf (merge_equations (c_merge Cu) ++ merge_equations (c_merge Ch)) M /\
     KeyEq M k target /\
     (forall other, In other needs -> CopyNeedAt Cu s k p other -> KeyEq M k other -> other = target)).

Definition CopyInterpretation (W : world) (sigma : subst) (u h : nat) (Cu : cx) (nsu nsh : list item) (Ch : cx)
    (rm : list nat) (target : item -> item) : Prop :=
  let fixed := copy_fixed_readings Cu sigma in
  exists outs gen ys cg Cr ks Cr' ks' scope_after,
    (forall t, In t outs <-> FunOutputs W u t) /\
    Allocates (copy_allocation_scope W sigma) (map gen outs ++ map cg ys) scope_after /\
    (forall y, In y ys <-> (exists x tx, nlook x (c_ty Cu) = Some tx /\ In y (gens tx)) /\
                          lookup_subst y fixed = None) /\
    (* inputs *)
    (forall x tx, nlook x (c_ty Ch) = Some tx ->
       app_subst (copy_subst fixed ys cg) (canon Cu (TAbs x)) = app_subst (copy_subst fixed ys cg) (canon Cu tx)) /\
    (* needs *)
    (forall k, In k nsh -> exists p,
       ilook (provision_id k) (c_provisions Ch) = Some p /\
       CopyNeedChoice Cu Ch (copy_subst fixed ys cg) nsu k p (target k)) /\
    (* the result *)
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks /\
    ProvideResult W h Ch (fun t => TAbs (gen t)) Cr' ks' /\
    length rm = length ks /\
    (forall i k, nth_error ks i = Some k -> exists j k' p p',
       nth_error rm i = Some j /\ nth_error ks' j = Some k' /\
       app_subst_item (copy_subst fixed ys cg) (canon_item Cu (provision_id k')) =
         app_subst_item (copy_subst fixed ys cg) (provision_id k) /\
       (forall j' k'', nth_error ks' j' = Some k'' ->
          app_subst_item (copy_subst fixed ys cg) (canon_item Cu (provision_id k'')) =
            app_subst_item (copy_subst fixed ys cg) (provision_id k) ->
          provision_id k'' = provision_id k' /\ j' <= j) /\
       ilook (provision_id k) (c_provisions Cr) = Some p /\
       ilook (provision_id k') (c_provisions Cr') = Some p' /\
       provision_subst (copy_subst fixed ys cg) p = provision_subst (copy_subst fixed ys cg) (canon_provision Cu p')).

(* A promised item is compared with the body's at the body's meanings of
   the output roles (each role at what the site's table makes of it,
   canonically), as the oracle's promised_result_type does: a body that
   binds `T = Bool` provides `mk(): T` as `mk(): Bool`. *)
Definition role_reads (site : cx) (outs : list nat) (gen : nat -> nat) : subst :=
  map (fun t => (gen t, canon site (app_subst (c_ty site) (TAbs (gen t))))) outs.
(* Q6: a fixed input of F that the body pins (binds, whether or not G takes
   it) is compared at the pin, canonically: a body that pins `Tok = Char`
   provides `tok(): Tok` as `tok(): Char` (tests/wasi/pin_fixed_agree.moss).
   Every application reads Tok as the bind site did (rule 5), and the bind
   site's reading is the pin's (BFunClosure), so the two agree. *)
Definition pin_reads (body : cx) (pins : subst) : subst :=
  map (fun q => (fst q, canon body (snd q))) pins.
(* Where a linked body's result key is read (LinkBody): an output role
   (gen t) at the body's final table (a body that binds T = Bool provides
   T.m as Bool.m), every symbol at G's own readings. An input's meaning at
   an application is the caller's, and a rebind in the body writes only its
   alias, so a key's receiver may not follow it (an unpinned merge
   representative, tests/errors/link_merge_rep_rebind.moss; the oracle's
   check is by written leaf, scratch/orch/linknewpf-AUDIT.md). That holds
   for an input of F that is also an output: its input occurrences (a
   merge representative's) read at G's table, only its role at the body's
   (tests/errors/link_output_input_rep.moss, scratch/orch/realize3-AUDIT.md).
   The roles stay in the keys and promised types, so the site's table
   alone reads them.  A pinned fixed input (Q6, LinkBody's pins) reads at
   its pin, the body's meaning of it: every application reads it as the
   bind site did, which BFunClosure checks is the pin, so the body's key
   is the caller's (lib/wasistd.moss's GcArrays binds `Array.get` at
   `ArrayHandle = ArrayRef`; tests/wasi/pin_fixed_key.moss, AUDIT-CHANGES
   #73). *)
Definition link_site (Cg Cb : cx) (pins : subst) (outs : list nat) (gen : nat -> nat) : cx :=
  Cx (map (fun t => (gen t, match nlook t (c_ty Cb) with Some v => v | None => TAbs t end)) outs ++ pins ++ c_ty Cg)
     (c_sub Cb) (c_merge Cb) (c_provisions Cb) (c_locals Cb).
(* Each promised key, read at the body's site (at_site re-reads its
   receiver), is provided there, at the promised provision with `reads`
   substituted (the roles' meanings, role_reads). *)
Definition ResultRealizes (body promised : cx) (reads : subst) (keys : list item) : Prop :=
  forall k, In k keys -> exists required actual,
    ilook (provision_id k) (c_provisions promised) = Some required /\
    ilook (provision_id (at_site body k)) (c_provisions body) = Some actual /\
    provision_refines
      (provision_subst (c_sub body) (provision_subst reads required))
      (provision_subst (c_sub body) actual).
Definition FamilyBindings (W : world) (bindings : list binding) : Prop :=
  Forall (fun binding => match binding with
    | BValue _ _ => False
    | BApply family _ _ _ _ => Family W family
    | _ => True end) bindings.

Inductive BindsOf : expr -> list binding -> Prop :=
| BOEnd : BindsOf EUnit []
| BOBind : forall binding body rest,
    BindsOf body rest -> BindsOf (EBind binding body) (binding :: rest).

(* Capture is lexical: an inner binder hides an outer name only after its
   initializer. Assigning an outer name is a mention as well as reading it. *)
Section LexicalNames.
Variable global_name : name -> bool.
Fixpoint pattern_names (pattern : apat) : list name :=
  match pattern with
  | APWild => []
  | APPath [n] => if global_name n then [] else [n]
  | APPath _ => []
  | APTag _ payload => pattern_names payload
  | APRec _ fields => flat_map (fun q => match snd q with
      | None => [fst q] | Some p => pattern_names p end) fields
  end.
Fixpoint free_names (e : aexpr) : list name :=
  match e with
  | AEUnit | AEThis | AEBreak | AELit _ => []
  | AEPath [n] None => [n]
  | AEPath _ _ => []
  | AECall _ _ args => flat_map free_names args
  | AEField receiver _ | AEUnary _ receiver => free_names receiver
  | AEMeth receiver _ args => free_names receiver ++ flat_map free_names args
  | AERec _ _ fields => flat_map (fun q => match snd q with
      | Some value => free_names value | None => [fst q] end) fields
  | AEIf condition body otherwise => free_names condition ++ free_block_names body ++
      match otherwise with Some e => free_names e | None => [] end
  | AEMatch scrutinee arms => free_names scrutinee ++
      flat_map (fun arm => fold_right (remove String.string_dec) (free_names (snd arm))
        (pattern_names (fst arm))) arms
  | AEReturn value => match value with Some e => free_names e | None => [] end
  | AEBlock body => free_block_names body
  | AEBinary _ lhs rhs => free_names lhs ++ free_names rhs
  | AELam params _ body => fold_right (fun p names => remove String.string_dec (fst p) names)
      (free_names body) params
  end
with free_block_names (body : ablock) : list name :=
  match body with ABlock statements tail =>
    let rest := match tail with Some e => free_names e | None => [] end in
    (fix walk (ss : list astmt) : list name :=
      match ss with
      | [] => rest
      | ASLet n _ value :: more | ASVar n _ value :: more =>
          free_names value ++ remove String.string_dec n (walk more)
      | statement :: more => free_statement_names statement ++ walk more
      end) statements
  end
with free_statement_names (statement : astmt) : list name :=
  match statement with
  | ASLet _ _ value | ASVar _ _ value | ASExpr value => free_names value
  | ASAssign n value => n :: free_names value
  | ASBind bindings => flat_map (fun q => match snd q with
      | Some e => free_names e | None => [] end) bindings
  | ASWhile condition body => free_names condition ++ free_block_names body
  | ASLoop body => free_block_names body
  end.
End LexicalNames.
Definition Mentions_name (W : world) (F : frame) (body : aexpr) (n : name) : Prop :=
  In n (free_names (fun n => match resolve W (fr_mod F) [n] with
    | Some _ => true | None => false end) body).

(* Automatic family demand checks an initial missing method key, then applies
   its unique family in the context extended by preceding demands.  Type
   arguments are temporary; a family provides methods only. *)
Definition family_restore (C Cr : cx) : cx :=
  Cx (c_ty C) (c_sub Cr) (c_merge Cr) (c_provisions Cr) (c_locals Cr).

Inductive FamilyStep (W : world) (C0 : cx) (wanted : list item)
    : cx -> nat -> subst -> subst -> smap -> list item -> cx -> Prop :=
| FamilyStepApply : forall C ot m u matching th sm Cr ks,
    In (mkey ot m) wanted -> mlook ot m (c_mths C0) = None ->
    OnlyFamily W C0 u (mkey ot m) ->
    FamilyApplication W C u matching th sm Cr ks -> FamilyReadOff W C u ot m matching ->
    In (mkey ot m) ks -> MatchingInForce C matching ->
    FamilyStep W C0 wanted C u matching th sm (map (rkey_of Cr) ks) (family_restore C Cr).

(* Group identity is the inferred matching substitution.  Distinct groups
   may emit equal complete type arguments after reading the other inputs. *)
Inductive Demand (W : world) (C0 : cx) (wanted : list item)
    : list (nat * subst) -> cx -> (expr -> expr) -> cx -> Prop :=
| DemandDone : forall used C,
    Demand W C0 wanted used C (fun x => x) C
| DemandApply : forall used C u matching th sm rs Cm wrap Cf,
    ~ In (u, matching) used -> FamilyStep W C0 wanted C u matching th sm rs Cm ->
    Demand W C0 wanted ((u, matching) :: used) Cm wrap Cf ->
    Demand W C0 wanted used C (fun x => apply_expression u th sm rs (wrap x)) Cf.

(* D89: the bracket application on a call of the fn f: each binding names
   a type symbol among f's requirements and binds it to a type by name. *)
Definition CallBrackets (W : world) (p : string) (f : nat) (bs : list (path * spec)) : Prop :=
  Forall (fun b => (exists x, resolve_sym W p (fst b) = Some x /\ kind_sym W x = Some DKType /\
                              In x (w_reqs W f)) /\
                   exists rp ra, snd b = Spec (Some rp) None ra) bs.

(* [D78]/[D84] rule 5, `bind F;` for F abstract and in force.
   Typing resolves the functor and determines the meanings at its use,
   the new identities and the provisions available afterward. *)
Record application := Application {
  app_functor : nat;
  app_input : cx;
  app_needs : list item;
  app_arguments : subst;
  app_outputs : list nat;
  app_identity : nat -> nat;
  app_allocated : list nat;
  app_result : cx;
  app_provisions : list item
}.

(* A fresh generation preserves an existing meaning of the same source
   name. A provision key can mention a type even when it has no binding
   (tests/errors/lambda_declared_output.moss). LinkBody separately checks
   that a body does not make an input of the functor to which it is bound
   (tests/errors/link_kept_output_pin.moss). *)
Definition OutputIdentityAt (W : world) (F : frame) (C : cx)
    (a : application) : Prop :=
  forall t, FunOutputs W (app_functor a) t ->
    (nlook t (c_ty C) = None /\ app_identity a t = t /\
      ~ In t ((match fr_ret F with Some r => gens r | None => [] end) ++
              (match fr_this F with Some r => gens r | None => [] end)) /\
      ~ In t (context_gens C ++ key_receiver_gens C)) \/
    ((nlook t (c_ty C) <> None \/ In t (context_gens C ++ key_receiver_gens C)) /\
      In (app_identity a t) (app_allocated a)).

Definition TypesApplication (W : world) (F : frame) (C : cx)
    (pa : path) (a : application) : Prop :=
  let u := app_functor a in
  let Cu := app_input a in
  let th := app_arguments a in
  let gensof := app_identity a in
  resolve_sym W (fr_mod F) pa = Some u /\
  (* Fixed inputs retain their meanings from the provision site. *)
  (exists fixed, nlook u (c_funs C) = Some fixed /\
    forall x tx, In (x, tx) fixed -> exists here,
      nlook x (c_ty C) = Some here /\ canon C here = canon C tx) /\
  FunCtx W u Cu (app_needs a) /\
  (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here, nlook t (c_ty C) = Some here) /\
  th = map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cu) /\
  NeedsSatisfied C Cu th (app_needs a) /\
  FreshFor W F C (app_allocated a) /\
  (forall t t', FunOutputs W u t -> FunOutputs W u t' -> gensof t = gensof t' -> t = t') /\
  OutputIdentityAt W F C a /\
  NoDup (app_outputs a) /\ (forall t, In t (app_outputs a) <-> FunOutputs W u t) /\
  ProvideResult W u C (fun t => TAbs (gensof t)) (app_result a) (app_provisions a) /\
  (* Distinct result items must stay distinct at this application.
     The declared result may repeat a key (tests/wasi/result_repeat_in_f.moss). *)
  (exists Cf ksf, ProvideResult W u Cu (fun t => TAbs t) Cf ksf /\
    CollidesOnlyAs (app_provisions a) ksf).

(* Encode the checked application as a run-time binding. *)
Definition translate_application (C : cx) (a : application) : expr -> expr :=
  let th := app_arguments a in
  apply_output_expression (app_functor a) th
    (map (fun t => (t, app_identity a t)) (app_outputs a))
    (satisfaction_map C (app_input a) th (app_needs a))
    (map (rkey_of (app_result a)) (app_provisions a)).

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
| ExUnitTag : forall F C exp pa s,
    (forall n, pa = [n] -> slook n (c_locals C) = None) ->
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKUnit ->
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
    resolve_sym W (fr_mod F) pa = Some s -> kind_sym W s = Some DKTag ->
    Brackets W (fr_mod F) (c_ty C) s a bs ->
    Forall2 (fun r ra => fst ra = r /\ nlook r (bs ++ c_ty C) = Some (snd ra)) (w_reqs W s) xs ->
    PayloadOf W s xs pay ->
    args = [y] -> Ex W P F C y (Some pay) v ty -> fits ty pay ->
    Ex W P F C (AECall pa a args) exp (ETag s xs v) (TNom s xs)
(* [D20]/[D21] a defined fn: its needs answered from here (sm), its
   signature read with the meanings here *)
| ExCallD : forall F C exp pa args f Cf ns th g xs wrap C',
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns ->
    (* family demands read f's needs through θ (rule B's companion) *)
    Demand W C (map (at_th C th) ns) [] C wrap C' ->
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
       (wrap (translate_call C' Cf f th ns xs)) (sg_ret g)
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
    NoDup (map fst fs) -> length fs = length ftys ->
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
| MCAttachedDefined : forall F C ox ot n args h att Ca ns th g xs rargs wrap Cd,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    Demand W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)
      (map (at_th (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) th) ns) []
      (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
    NeedsSatisfied Cd Ca th ns -> CallTheta W Cd Ca att th -> SigOf W att (c_ty Cd) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    RequirementsInForce (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) Ca ->
    MethCall W P F C ox ot [n] args
      (wrap (translate_call Cd Ca att th ns (ox :: xs))) (sg_ret g)
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
| PrFamily : forall F C ox ot m args u matching th sm C' ks g xs,
    mlook ot m (c_mths C) = None ->
    FamilyApplication W C u matching th sm C' ks -> FamilyReadOff W C u ot m matching ->
    In (mkey ot m) ks -> OnlyFamily W C u (mkey ot m) ->
    mlook ot m (c_mths C') = Some g -> Args W P F C args (sg_params g) xs ->
    MatchingInForce C matching ->
    Provided W P F C ox ot m args (apply_expression u th sm (map (rkey_of C') ks) (EMethod ox m (rkey C' ot m) xs)) (sg_ret g)

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
    BindItems W P (with_mod F (d_mod d) u) C (concat bodies) wrap C' F' made ->
    BindItem W P F C (Spec (Some pa) None None, None) wrap C'
             (Frame (fr_mod F) (fr_ret F) (fr_this F) (fr_loop F) (fr_var F) (fr_fresh F') (fr_applying F) (fr_pins F)) made
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
| BFunClosure : forall F C pa u pg gg Cu nsu Cg nsg h pins sm smc fx,
    resolve_sym W (fr_mod F) pa = Some u -> resolve_sym W (fr_mod F) pg = Some gg ->
    (exists n ps a r, ast_of W u = Some (ADFunctor n ps a r None)) ->
    (* F's and G's contexts, as the world records them (LinkBody states
       that they are their FunCtx) *)
    w_context W u = Some (Cu, nsu) -> w_context W gg = Some (Cg, nsg) ->
    (* 3a: X(F) in force here *)
    (forall x, Fixed W u x -> nlook x (c_ty C) <> None) ->
    SameResult W u gg ->
    LinkBody W P u gg h pins ->
    (* the linked needs: those of G that are F's, read through F's merge *)
    sm = map (fun k => (rkey_of Cg k, rkey_of Cu (canon_item Cu k))) (filter (fun k => existsb (item_eqb (canon_item Cu k)) nsu) nsg) ->
    (* 3b: the captured needs are answered here as a call would answer
       them, at G's inputs read here: provided, coherent, at G's view of
       their types, and G's merges holding of the meanings here *)
    Satisfies C Cg (map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg))
      (filter (fun k => negb (existsb (item_eqb (canon_item Cu k)) nsu)) nsg) smc ->
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
    CtxOf W g Cg nsg -> n = length (sg_params sg) ->
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
    length ps = length (sg_params sg) -> NoDup (map fst ps) ->
    Forall2 (fun pq t => match snd pq with
                         | Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ MergeEq (c_merge C) t' t
                         | None => True end) ps (sg_params sg) ->
    match rt with Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ MergeEq (c_merge C) t' (sg_ret sg)
                 | None => True end ->
    (* captured: the enclosing locals it mentions, less its parameters *)
    (forall n, In n caps <-> (Mentions_name W F body n /\ slook n (c_locals C) <> None /\ ~ In n (map fst ps) /\ fr_applying F = [])) ->
    NoDup caps -> length capvals = length caps -> Forall (Fresh W) capvals -> NoDup capvals ->
    (* new symbols: no val the context provides is reused, so the
       prefix that binds them leaves every provision in force as it was *)
    Forall (fun v => ilook (IVal v) (c_provisions C) = None) capvals ->
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
    body_of P pid = Some (fold_right (fun q x => ELet (var_of Fl (fst q)) (EVal (snd q)) x) lbody
                                    (combine caps capvals)) ->
    (* its needs: every key in force here, each answered by itself *)
    (forall k, In k keys <-> (provides Cl k /\
       (match k with ITy _ => False | _ => True end) /\ k = provision_id k)) ->
    NoDup keys -> sm = map (fun k => (rkey_of Cl k, rkey_of C k)) keys ->
    LambdaProvider W P F C ps rt body sg this pid
      (fun x => fold_right (fun q x => bind_value_expression (snd q) (EVar (var_of F (fst q))) x) x (combine caps capvals))
      sm

(* [D78]/[D84] 3e: G's result is F's, item by item: the same leaves
   (keys, as written, through contexts). *)

(* [D84] 3b–3e: G's binds checked once, in G's own terms — G's context
   formed from its own inputs, each output of F as itself — producing the
   core functor body h: its result telescope (F's output types, then F's
   result items), and its binds.  3b: no captured item mentions a type
   that applications supply; 3d: pins may not rebind P(F) ∩ M(F) (Q6); 3e: every item
   of F's result is made by the binds, at the types F promises. *)
with LinkBody (W : world) (P : program) : nat -> nat -> nat -> subst -> Prop :=
| LB : forall u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0 pins,
    w_decl W gg = Some dg -> d_ast dg = ADFunctor n ps a r (Some bodies) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    (* 3b: a captured item (value, fn, method or functor) may mention fixed
       or privately captured types, but no target input type supplied afresh
       by each application: not in its type or signature, its recorded
       instantiation, or a method's receiver. *)
    (forall k p t, In k nsg -> ~ In (canon_item Cu k) nsu -> ilook (provision_id k) (c_provisions Cg) = Some p ->
       In t (provision_gens p ++ match k with IMth r _ _ => gens r | _ => [] end) ->
       nlook t (c_ty Cu) <> None -> Fixed W u t) ->
    (* G reads each need F links as F does, and its readings of F's inputs
       hold in F's context (the oracle: linked input interpretation, D84).
       A need of G is linked when it is one of F's read through F's merge
       (F's congruence decides "same need", whatever representatives the
       two contexts chose: examples/sameneed-*.moss).  A class of G that
       met two of F's need classes would be ambiguous; the next clause
       rules it out (`PublicSameNeed.link_need_unambiguous`).
       An input of F reads in G as a type over F's inputs: G's merges may
       not send a linked input to a type only G has, which the closure
       captures at the bind while each application supplies the input
       afresh (the oracle's 3b sees the first unmerged reading;
       tests/errors/link_merge_captured.moss, scratch/orch/cap3b-AUDIT.md) *)
    (forall k, In k nsg -> In (canon_item Cu k) nsu ->
       option_map (canon_provision Cu) (ilook (provision_id k) (c_provisions Cg)) =
       ilook (provision_id (canon_item Cu k)) (c_provisions Cu)) ->
    (forall x tx, nlook x (c_ty Cg) = Some tx -> nlook x (c_ty Cu) <> None ->
       (forall y, In y (gens tx) -> nlook y (c_ty Cu) <> None) /\ canon Cu (TAbs x) = canon Cu tx) ->
    fr_mod Fg = d_mod dg -> fr_ret Fg = None ->
    fr_this Fg = None -> fr_loop Fg = false ->
    (* its locals are numbered as a body's are *)
    Numbering Fg None [] ->
    (* 3d, Q6: the protected inputs Π are the polymorphic types F's result
       mentions, P(F) ∩ M(F); a fixed input may be pinned (bound by the
       body), whether or not G takes it, and BFunClosure checks that the
       pin agrees with the bind site (AUDIT-CHANGES #95) *)
    (forall x, In x (fr_pins Fg) <-> Mentions W u x /\ Poly W u x) ->
    BindItems W P Fg Cg (concat bodies) wrap Cb Fb made ->
    (* no generation the body makes is an input of F: each application
       supplies F's inputs, and a body write would replace one under a
       key the caller files at its own reading (a kept output at an
       unmentioned merge representative, tests/errors/link_kept_output_rep.moss;
       scratch/orch/realize2-AUDIT.md) *)
    (forall t, In t made -> nlook t (c_ty Cu) = None) ->
    (forall t, In t outs <-> FunOutputs W u t) ->
    (forall t, In t outs -> exists meaning, nlook t (c_ty Cb) = Some meaning) ->
    NoDup (map gen outs) ->
    (forall t, In t outs -> Fresh W (gen t)) ->
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks ->
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
    Fresh W h ->
    (* each item of F's declared result, position by position, is read
       where the body bound it: a method under the body's runtime key for
       its receiver read at link_site (the application installs it under
       the caller's key for that position) *)
    funbody_of P h = Some (FunctorCode (map fst (c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (c_ty Cu))) (map fst (c_ty Cg))) (c_ty Cg)
      (map ITy outs) binds0
      (map (fun k => rkey_of (link_site Cg Cb pins outs gen) (at_site (link_site Cg Cb pins outs gen) k)) ks)) ->
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

(* ======================================================================== *)
(* V.7 Programs                                                              *)
(* ======================================================================== *)

(* A defined fn or method f: its body, under the context its assumes form
   (and, for an attached method, the receiver as `this` at its
   requirements' generations), with the parameters numbered from 0 (after
   `this`), elaborates to `body_of P f`. *)
Definition FnElab (W : world) (P : program) (f : nat) : Prop :=
  forall d rcv n ps rt b,
    w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some b) ->
  exists C ns this g F x t,
    CtxOf W f C ns /\
    (match rcv with
     (* Q5: an attached method's receiver is nominal *)
     | RAttached _ => exists r args, Attached W r n f /\ (kind_sym W r = Some DKTag \/ kind_sym W r = Some DKUnit) /\
                        this = Some (TNom r args) /\
                        Forall2 (fun x xa => fst xa = x /\ nlook x (c_ty C) = Some (snd xa)) (w_reqs W r) args
     | _ => this = None end) /\
    SigOf W f (c_ty C) this g /\
    fr_mod F = d_mod d /\ fr_ret F = Some (sg_ret g) /\ fr_this F = this /\ fr_loop F = false /\
    fr_fresh F = [] /\ fr_applying F = [] /\ fr_pins F = [] /\
    Numbering F this (map fst ps) /\
    BlkE W P F (fold_left (fun C' q => add_local C' (fst q) (snd q) LLet) (combine (map fst ps) (sg_params g)) C)
         b (Some (sg_ret g)) x t /\
    body_of P f = Some x.

(* The runtime type prefix reads the source context with each machine
   type represented by its declaration's symbol. *)
Definition machine_subst (machines : list (tsym * machine_class)) : subst :=
  map (fun q => (fst q, TMachine (fst q))) machines.
Definition InitialTypes (machines : list (tsym * machine_class)) (C : cx) (types : subst) : Prop :=
  types = map (fun q => (fst q, app_subst (machine_subst machines) (snd q))) (c_ty C).

(* [D16]/[D30] no expanding cycle (Kennedy and Pierce's expansive
   cycles, decided generically, before monomorphization).  A specializer
   makes one copy of each code node per instantiation it reaches, and one
   WasmGC type per nominal instance; both sets must be finite.

   Instantiations.  A code node is a body of P: a fn (a defined fn, a
   lambda, a forwarder, a wrapper, a method provider) or a core functor
   body.  A vertex (n, x) is n's formal type symbol x.  An edge
   (n, u) -> (m, x) says that some way n reaches m gives x a meaning built
   from u's; it is strict when u occurs under a type constructor (a
   nominal, a record or a union), so m's instantiation can be larger:
   - a direct call `ECall m θ` (plain calls, bracket applications, and
     attached methods at the receiver's instantiation, [D74]): x ↦ θ x;
   - a closure made by `bind f = g`, `bind R.m = g`, a lambda, or
     `bind F = G`: the closure's code at the types it captures
     (generations and aliases);
   - an application `bind F;` and a family member demanded on demand
     ([D84] rule 7), both `BApply θ`: every core functor body, at each of
     its linked inputs θ supplies (which body is bound to F is not known
     statically);
   Types in force at a site are already read in the site's θ and
   captures (CallTheta, lexical_capture), so a `bind T = τ` in force
   counts through them.  Within a node, a symbol is read as itself, a
   functor body's input through its table, and an output an application
   writes as possibly containing, strictly, everything its θ mentions:
   the abstract functor's body is not inspected (the owner's choice (a);
   it rejects scratch/expand/mono/c3c_control.moss, a functor whose output
   is constant, which native compiles).
   The check: a level for every vertex that no edge raises and every
   strict edge lowers.  Such a level exists iff no cycle of the (finite)
   graph has a strict edge, which is decidable: the level of a vertex can
   be taken as the most strict edges on a path from it, at most the
   number of vertices, computed on the graph's condensation.  It bounds
   the specializer: an instantiation reached from main's at vertex v has
   depth at most main's plus level v times the deepest type P writes. *)
Inductive code_node : Type := CodeFn (f : nat) | CodeFunctor (h : nat).

(* The type symbols of a type, each with whether it occurs under a
   constructor. *)
Fixpoint ty_occurrences (tau : ty) : list (tsym * bool) :=
  let under := map (fun q : tsym * bool => (fst q, true)) in
  match tau with
  | TUnit | TMachine _ => []
  | TAbs t => [(t, false)]
  | TRecord fs => under ((fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (_, t) :: r => ty_occurrences t ++ go r end) fs)
  | TNom _ args => under ((fix go (l : list (tsym * ty)) :=
      match l with [] => [] | (_, t) :: r => ty_occurrences t ++ go r end) args)
  | TUnion ms => under ((fix go (l : list ty) :=
      match l with [] => [] | t :: r => ty_occurrences t ++ go r end) ms)
  end.

(* The nominal instances a type mentions, nested ones included. *)
Fixpoint ty_nominals (tau : ty) : list (nsym * subst) :=
  match tau with
  | TUnit | TMachine _ | TAbs _ => []
  | TRecord fs => (fix go (fs : list (string * ty)) :=
      match fs with [] => [] | (_, t) :: r => ty_nominals t ++ go r end) fs
  | TNom n args => (n, args) :: (fix go (l : list (tsym * ty)) :=
      match l with [] => [] | (_, t) :: r => ty_nominals t ++ go r end) args
  | TUnion ms => (fix go (l : list ty) :=
      match l with [] => [] | t :: r => ty_nominals t ++ go r end) ms
  end.

(* What a node's code does to instantiations: it enters a node at a
   substitution, or applies an abstract functor at θ with outputs. *)
Inductive code_site : Type :=
| SiteEnter (n : code_node) (th : subst)
| SiteApply (th : subst) (outputs : list (tsym * tsym)).

Fixpoint expr_sites (e : expr) : list code_site :=
  let many := fix go (l : list expr) := match l with [] => [] | x :: r => expr_sites x ++ go r end in
  match e with
  | EVar _ | EUnit | EVal _ | EBreak => []
  | ETag _ _ e1 | EAssign _ e1 | ELoop e1 | EReturn e1 | EField e1 _ => expr_sites e1
  | ECall f th _ args => SiteEnter (CodeFn f) th :: many args
  | EInvoke _ args => many args
  | EMethod e0 _ _ args => expr_sites e0 ++ many args
  | ELet _ e1 e2 | ESeq e1 e2 | EWhile e1 e2 => expr_sites e1 ++ expr_sites e2
  | EBind b e1 => binding_sites b ++ expr_sites e1
  | EIf c e1 e2 => expr_sites c ++ expr_sites e1 ++ expr_sites e2
  | ERecord fs => (fix go (l : list (fld * expr)) :=
      match l with [] => [] | (_, x) :: r => expr_sites x ++ go r end) fs
  | EMatch e0 arms => expr_sites e0 ++ (fix go (l : list (pattern * expr)) :=
      match l with [] => [] | (_, x) :: r => expr_sites x ++ go r end) arms
  end
with binding_sites (b : binding) : list code_site :=
  match b with
  | BType _ _ | BCopyFunctor _ _ _ _ | BPrimitive _ _ => []
  | BValue _ e => expr_sites e
  | BFunction _ g types _ | BMethod _ _ _ g types _ =>
      [SiteEnter (CodeFn g) (capture_generations types ++ capture_aliases types)]
  | BFunctor _ h types _ _ =>
      [SiteEnter (CodeFunctor h) (capture_generations types ++ capture_aliases types)]
  | BApply _ th outputs _ _ => [SiteApply th outputs]
  end.

Definition node_sites (P : program) (n : code_node) : list code_site :=
  match n with
  | CodeFn f => match body_of P f with Some e => expr_sites e | None => [] end
  | CodeFunctor h => match funbody_of P h with
      | Some code => flat_map binding_sites (fc_bindings code) | None => [] end
  end.
Definition node_table (P : program) (n : code_node) : subst :=
  match n with
  | CodeFunctor h => match funbody_of P h with Some code => fc_table code | None => [] end
  | CodeFn _ => []
  end.

(* `NodeReads P n t u s`: in n's code, the type symbol t contains n's
   formal u (strictly if s). *)
Inductive NodeReads (P : program) (n : code_node) : tsym -> tsym -> bool -> Prop :=
| ReadsSelf : forall t, NodeReads P n t t false
| ReadsTable : forall x tau v s u s',
    In (x, tau) (node_table P n) -> In (v, s) (ty_occurrences tau) -> NodeReads P n v u s' ->
    NodeReads P n x u (s || s')
| ReadsOutput : forall th outputs source t x tau v s u s',
    In (SiteApply th outputs) (node_sites P n) -> In (source, t) outputs ->
    In (x, tau) th -> In (v, s) (ty_occurrences tau) -> NodeReads P n v u s' ->
    NodeReads P n t u true.

(* The nodes a site enters, each at the substitution of its formals. *)
Definition site_targets (P : program) (site : code_site) : list (code_node * subst) :=
  match site with
  | SiteEnter n th => [(n, th)]
  | SiteApply th _ => map (fun q => (CodeFunctor (fst q),
      filter (fun p => existsb (Nat.eqb (fst p)) (fc_linked (snd q))) th)) (program_functors P)
  end.

Definition InstantiationEdge (P : program) (a b : code_node * tsym) (strict : bool) : Prop :=
  exists site th tau v s s',
    In site (node_sites P (fst a)) /\ In (fst b, th) (site_targets P site) /\
    In (snd b, tau) th /\ In (v, s) (ty_occurrences tau) /\
    NodeReads P (fst a) v (snd a) s' /\ strict = s || s'.

(* The context a node's code is elaborated in: a fn's own (the world's
   record), a lambda provider's (the enclosing context, whose merges it
   keeps), a linked functor body's (G's). *)
Inductive NodeCx (W : world) (P : program) : code_node -> cx -> Prop :=
| NCFn : forall f C ns, w_context W f = Some (C, ns) -> NodeCx W P (CodeFn f) C
| NCLambda : forall F C ps rt body sg this pid pre sm,
    LambdaProvider W P F C ps rt body sg this pid pre sm -> NodeCx W P (CodeFn pid) C
| NCBody : forall u gg h pins Cg nsg, LinkBody W P u gg h pins -> w_context W gg = Some (Cg, nsg) ->
    NodeCx W P (CodeFunctor h) Cg.

(* Merges are edges too (criteria section 2, coherence).  Within a node,
   a type symbol t is the type its context's merge reads it as, `canon C
   (TAbs t)`: an edge from each symbol that reading mentions to t, strict
   under a constructor, and, where t reads as a symbol z, an edge back from
   t to z.  So the members of a merge class share a level, and a class
   merged with a structure lies strictly above what the structure
   mentions.  Which member a context spells a class by is the merge's
   choice of representative (MostGeneralMerge); the classes do not depend
   on it (PublicOrientation.mgm_classes_iff), and two orientations read a
   structure alike up to renaming within classes (mgm_reorient), so these
   constraints do not either.  Without them a cycle could enter a class at
   one member and leave it at another (or at the structure it is merged
   with), and whether it is seen would depend on the spelling. *)
Definition InstantiationsBounded (W : world) (P : program) : Prop :=
  exists level : code_node * tsym -> nat,
    (forall a b strict,
       InstantiationEdge P a b strict -> level b + (if strict then 1 else 0) <= level a) /\
    (forall n C t v s, NodeCx W P n C -> In (v, s) (ty_occurrences (canon C (TAbs t))) ->
       level (n, t) + (if s then 1 else 0) <= level (n, v)) /\
    (forall n C t z, NodeCx W P n C -> canon C (TAbs t) = TAbs z -> level (n, z) <= level (n, t)).

(* Types.  A nominal's payload, read at its own requirements, mentions
   nominal instances; an edge (g, x) -> (g', y) says g' is mentioned with
   y's argument built from x, strictly under a constructor.  Every
   instance mentioned counts, behind a `Ref` or an `Array` too: a WasmGC
   type names the types of its fields, so `Cons[T]` holding a
   `Ref[Boxed=Nest[T=Box]]` needs `Cons[T=Box[T]]`, and so on
   (scratch/expand/mono/c8c_type_only.moss).  [D58]'s finite layout (no
   tag containing itself, with no heap between) is a different graph,
   `LayoutReaches` below: it follows a tag's payload at its arguments, so
   it does not look behind a `Ref`, whose payload does not mention them. *)
Definition TypeEdge (W : world) (a b : nsym * tsym) (strict : bool) : Prop :=
  exists pay args tau,
    PayloadOf W (fst a) (map (fun t => (t, TAbs t)) (w_reqs W (fst a))) pay /\
    In (fst b, args) (ty_nominals pay) /\ In (snd b, tau) args /\
    In (snd a, strict) (ty_occurrences tau).

Definition TypesBounded (W : world) : Prop :=
  exists level : nsym * tsym -> nat, forall a b strict,
    TypeEdge W a b strict -> level b + (if strict then 1 else 0) <= level a.

Definition NoExpandingCycle (W : world) (P : program) : Prop :=
  TypesBounded W /\ InstantiationsBounded W P.

(* [D58]/[D59] finite layouts.  A value is a run of scalars, and there is
   no heap between a type and what it contains, so a type containing itself
   has no finite layout.  `LayoutReaches W τ n`: computing τ's layout
   computes the nominal n's: a tag's layout is its payload's, at its
   arguments; a record's is its fields' in sequence; a union's is its
   members' beside a discriminating word.  Everything else is one scalar or
   none, a leaf: a machine type, as the platform declares it (machine
   reference types included), and an abstract type.  So there is no case
   for any library's names: `Ref[Boxed=X]` is a tag whose payload is a
   machine reference type, so its layout is that one scalar, whatever X is,
   and `tag Cell { head: I32, tail: Nil | Ref[Boxed=Cell] }` has a layout
   (AUDIT-CHANGES #90).
   The check is on definitions: a tag whose payload, read at its own
   requirements, reaches the tag is rejected, used or not.  An abstract
   type is a leaf because no meaning given to it can make a layout
   infinite: a bind's right-hand side is read at the old meanings ([D72]),
   so every ground type is a finite term, and unfolding one only reaches
   payloads of tags, none of which reaches itself. *)
Inductive LayoutReaches (W : world) : ty -> nsym -> Prop :=
| LayoutTag : forall n args, LayoutReaches W (TNom n args) n
| LayoutPayload : forall n args pay m,
    PayloadOf W n args pay -> LayoutReaches W pay m -> LayoutReaches W (TNom n args) m
| LayoutField : forall fs f t m,
    In (f, t) fs -> LayoutReaches W t m -> LayoutReaches W (TRecord fs) m
| LayoutMember : forall ms t m,
    In t ms -> LayoutReaches W t m -> LayoutReaches W (TUnion ms) m.

Definition FiniteLayouts (W : world) : Prop :=
  forall n pay, PayloadOf W n (map (fun t => (t, TAbs t)) (w_reqs W n)) pay ->
    ~ LayoutReaches W pay n.

(* [D20]: an assumed provision brings its requirements and its needs, not
   just a signature.  `AssumeCtx W s C ns`: the context the assume nesting
   of symbol s forms, and its needs (CtxOf without the world's record,
   since a functor's record is its inputs' context, FunCtx). *)
Definition AssumeCtx (W : world) (s : nat) (C : cx) (ns : list item) : Prop :=
  exists d es tm er tmr σ M,
    w_decl W s = Some d /\
    ItemsEntries W (d_mod d) [] [] (map assume_spec (d_assumes d)) es tm /\
    RootCtx W s es tm er tmr /\
    MostGeneralMerge σ (es ++ er) /\ MergeOf (es ++ er) M /\ SectionOf M σ /\
    C = entries_cx σ M tmr (es ++ er) /\ Needs σ (es ++ er) ns.

(* The symbol a need names: a val, fn or method (a functor's fixed inputs
   are asked in force where it is assumed, IEFun). *)
Definition need_item (k : item) : option nat :=
  match k with IVal v => Some v | IFn f => Some f | IMth _ m _ => Some m | _ => None end.

(* An atomic class needed by an assumption is a leaf of an expanded
   requirement. It needs a fresh reading when no member is in force. *)
Definition MissingAtomicClass (C Cs : cx) (r : nat) : Prop :=
  (exists x z, In x (map fst (c_ty Cs)) /\
    In z (gens (expand (c_merge Cs) (TAbs x))) /\
    MergeEq (c_merge Cs) (TAbs r) (TAbs z)) /\
  ~ InForceClass C (c_merge Cs) r.

Definition AssumptionAllocation {identity} (C Cs : cx) (before : allocation_scope identity)
    (reading : nat -> option identity) : Prop :=
  exists batch after,
    Allocates before batch after /\
    (forall r, MissingAtomicClass C Cs r <-> exists g, reading r = Some g) /\
    (forall g, In g batch <-> exists r, reading r = Some g) /\
    (forall r r' g g', reading r = Some g -> reading r' = Some g' ->
      (g = g' <-> MergeEq (c_merge Cs) (TAbs r) (TAbs r'))).

Definition assumption_scope (W : world) (C Cs : cx) : allocation_scope nat :=
  AllocationScope (Fresh W) [] (context_type_support C ++ context_type_support Cs).

Definition AssumptionReadings (W : world) (C Cs : cx) (th : subst) : Prop :=
  exists atom,
    AssumptionAllocation C Cs (assumption_scope W C Cs) atom /\
    forall r a, In r (map fst (c_ty Cs)) -> atom r = Some a ->
      lookup_subst r th = Some (TAbs a).

(* `AssumedComplete W C ns`: in the formed context C, each assumed item
   (a need) has its requirements in force, bound by a bracket on it or
   assumed anywhere in the list (the instantiation it is recorded at,
   completed from the whole list, so list order is immaterial), and its
   own needs are provided here, read as a call of it at that
   instantiation would read them (CallTheta, Satisfies).  This is the
   oracle's check_assumes and check_assumed_requirements. *)
Definition AssumedComplete (W : world) (C : cx) (ns : list item) : Prop :=
  forall k s, In k ns -> need_item k = Some s ->
    exists a, ilook k (c_apps C) = Some a /\ (forall t o, In (t, o) a -> o <> None) /\
      let Cv := fold_left (fun C q => set_ty C (fst q) (snd q)) (known a) C in
      exists Cs nss th sm, CtxOf W s Cs nss /\ CallTheta W Cv Cs s th /\ Satisfies Cv Cs th nss sm /\
        AssumptionReadings W Cv Cs th.

(* Every symbol's assume nesting forms, with its assumed items complete.
   The oracle checks each val, fn, method, context and functor symbol's;
   a type's, unit's, tag's or alias's nesting is checked too (criteria
   section 7), so a nesting that holds only those is not exempt. *)
Definition AssumesComplete (W : world) : Prop :=
  forall s k, kind_sym W s = Some k ->
    exists C ns, AssumeCtx W s C ns /\ AssumedComplete W C ns.

(* Criteria section 7: every declaration is checked as written, whether
   or not anything uses it ([D16]'s rule for defined fns, for every kind
   of declaration).  As written means read where it is written, before
   anything is bound: each type symbol stands for itself.  A use reads the
   declaration at its own meanings, checked there as before. *)
Definition as_written (L : list nat) : list (nat * ty) := map (fun t => (t, TAbs t)) L.

(* A defined val's initializer elaborates as ExConst reads it at a use: no
   assumptions, its type read in an empty context, in an empty frame. *)
Definition ConstElab (W : world) (P : program) (d : decl) (t0 : aty) (i : aexpr) : Prop :=
  d_assumes d = [] /\ exists t, TyOf W (d_mod d) [] None t0 t /\
    exists F0 x t', Ex W P F0 (Cx [] [] [] [] []) i (Some t) x t' /\ fits t' t /\
      fr_mod F0 = d_mod d /\ fr_ret F0 = None /\ fr_this F0 = None /\ fr_loop F0 = false /\
      fr_fresh F0 = [] /\ fr_applying F0 = [] /\ fr_pins F0 = [] /\ Numbering F0 None [].

(* [D55] a functor with a body binds everything its result promises: each
   leaf of its declared result (ResultLeaf) is bound by an item of the
   body, by written name, as the oracle's functor_total and native check
   it: `bind p = e` and `bind R.m = e` bind their own leaf, and `bind G;`
   binds G's declared result.  A promise is not kept by an item that
   merely reads as it here (`bind Scalar = Int` does not bind
   `Scalar.add`, even where `Int.add` is in force).  For a direct
   application this is a check only: the caller sees the body's binds
   (BApplyBody), not the promise. *)
Inductive BodyBinds (W : world) (p : string) : spec * option aexpr -> nat * option name -> Prop :=
| BBItem : forall pa dn a y s,
    resolve_sym W p pa = Some s -> BodyBinds W p (Spec (Some pa) dn a, Some y) (s, dn)
| BBApply : forall pa dn a g k,
    resolve_sym W p pa = Some g -> kind_sym W g = Some DKFunctor -> ResultLeaf W g k ->
    BodyBinds W p (Spec (Some pa) dn a, None) k.

Definition FunctorTotal (W : world) (u : nat) : Prop :=
  forall d n ps a r bodies k, w_decl W u = Some d -> d_ast d = ADFunctor n ps a r (Some bodies) ->
    ResultLeaf W u k -> exists it, In it (concat bodies) /\ BodyBinds W (d_mod d) it k.

(* `DeclForms W P s`: s's declaration forms as written.
   - A context's items form (ItemsEntries), so a context that nests in a
     cycle, or names an item not in scope, is an error even when nothing
     assumes it (the oracle's context_acyclic; native expands every
     context).
   - A tag's payload and an alias's body denote a type (TyOf), so an
     unbound name, an alias cycle, a repeated record field, or a union
     member that is not nominal or shares a head is an error, used or not
     ([D15]).
   - A val's type, and a fn's or method's signature, denote types; a
     defined val's initializer elaborates (ConstElab).
   - A functor's argument telescope forms (FunCtx) and its result forms
     over it, each other type symbol standing for itself (a body may bind
     a type its result mentions, as lib/wasistd.moss's WasiFloats binds
     Int), and, if it has a body, the body binds its whole result
     (FunctorTotal), applied or not. *)
Definition DeclForms (W : world) (P : program) (s : nat) : Prop :=
  forall d, w_decl W s = Some d ->
    match d_ast d with
    | ADCtx _ its => exists L es tm, ItemsEntries W (d_mod d) [] (as_written L) its es tm
    | ADTag _ _ t | ADAlias _ t => exists L tau, TyOf W (d_mod d) (as_written L) None t tau
    | ADVal _ t i => (exists L tau, TyOf W (d_mod d) (as_written L) None t tau) /\
                     (forall e, i = Some e -> ConstElab W P d t e)
    | ADFn RPlain _ _ _ _ => exists L g, SigOf W s (as_written L) None g
    | ADFn _ _ _ _ _ => exists L this g, SigOf W s (as_written L) (Some this) g
    | ADFunctor _ _ _ _ _ => exists C ns L C' ks, FunCtx W s C ns /\
        ProvideResult W s (Cx (c_ty C ++ as_written L) (c_sub C) (c_merge C) (c_provisions C) (c_locals C))
          (fun t => TAbs t) C' ks /\ FunctorTotal W s
    | _ => True
    end.

Definition DeclsForm (W : world) (P : program) : Prop := forall s, DeclForms W P s.

(* The language items are what the language's rules take them to be
   ([D45]): Bool is a tag with no requirements whose payload is the union of
   its two values, True and False, distinct units. *)
Definition LangItemsFormed (W : world) : Prop :=
  forall symbol, LangItem W LIBool symbol ->
    exists truth falsity,
      LangItem W LITrue truth /\ LangItem W LIFalse falsity /\
      kind_sym W symbol = Some DKTag /\ kind_sym W truth = Some DKUnit /\
      kind_sym W falsity = Some DKUnit /\ truth <> falsity /\ w_reqs W symbol = [] /\
      (PayloadOf W symbol [] (TUnion [TNom truth []; TNom falsity []]) \/
       PayloadOf W symbol [] (TUnion [TNom falsity []; TNom truth []])).

(* The relation.  [D16]: every defined fn is elaborated, reached or not. *)
Definition ElaboratesIn (b : bundle) (W : world) (P : program) : Prop :=
  WorldOf b W /\
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
       FnElab W P f) /\
    (* main: the entry's `main`, a defined fn assuming only what the
       platform provides *)
    w_scope W (b_entry b) NSName "main" = Some (ESym (program_main P)) /\
    (exists d n rt body, w_decl W (program_main P) = Some d /\ d_ast d = ADFn RPlain n [] rt (Some body)) /\
    (* main's environment provides exactly main's needs; what each one is,
       the platform's provision, is checked by Linking using Provides. *)
    (exists C ns types platform, CtxOf W (program_main P) C ns /\ InitialTypes (program_machines P) C types /\
       program_environment P = Environment types platform /\ map fst platform = map (rkey_of C) ns) /\
    (* Bool's symbols, when lib/bool.moss is loaded ([D45]) *)
    (LangItem W LIBool (program_bool P) /\ LangItem W LITrue (program_true P) /\
     LangItem W LIFalse (program_false P)
     \/ ((forall s, ~ LangItem W LIBool s) /\ program_bool P = 0 /\ program_true P = 0 /\ program_false P = 0)) /\
    (* the language items are formed: Bool's layout *)
    LangItemsFormed W /\
    (* [D16]/[D30]: finitely many instantiations *)
    NoExpandingCycle W P /\
    (* [D20]: assumed items bring their requirements and needs *)
    AssumesComplete W /\
    (* criteria section 7: every declaration forms as written *)
    DeclsForm W P /\
    (* [D58]/[D59]: every tag has a finite layout *)
    FiniteLayouts W.

Definition Elaborates (b : bundle) (P : program) : Prop :=
  exists W, ElaboratesIn b W P.

End Elaboration.


(* The platform's source signatures. These checks relate library declarations
   to the machine operations supplied at linking, using the same world as
   elaboration. A matching name alone is insufficient. *)
(* The language's linking rule consumes only this static platform contract.
   A platform supplies the table interpretation, entry provisions and ABI. *)
Module Type STATIC_PLATFORM.
  Parameter Tables : Elaboration.world -> list (tsym * machine_class) -> list (nsym * cast_spec) -> Prop.
  Parameter Provides : Elaboration.world -> program -> Elaboration.cx -> item -> provision_value -> Prop.
  Parameter EntryABI : Elaboration.world -> program -> Prop.
End STATIC_PLATFORM.

Module Linking (Pf : STATIC_PLATFORM).
Import Elaboration.
Definition Linked (W : world) (P : program) : Prop :=
  Pf.EntryABI W P /\ Pf.Tables W (program_machines P) (program_casts P) /\
  exists context needs types entries,
    CtxOf W (program_main P) context needs /\ InitialTypes (program_machines P) context types /\
    program_environment P = Environment types entries /\
    Forall (fun entry => Pf.Provides W P context (fst entry) (snd entry)) entries.
Definition Ready (P : program) : Prop :=
  exists bundle world, ElaboratesIn bundle world P /\ Linked world P.
End Linking.

(* The operations installed anywhere in a program, including captured
   environments and platform-family bodies. Extra interface operations are
   harmless; an interface must supply at least these requirements. *)
Fixpoint expression_primitives (expression : expr) : list op :=
  match expression with
  | EVar _ | EUnit | EVal _ | EBreak => []
  | ETag _ _ e | EAssign _ e | ELoop e | EReturn e | EField e _ => expression_primitives e
  | EBind binding e => binding_primitives binding ++ expression_primitives e
  | ECall _ _ _ args | EInvoke _ args => flat_map expression_primitives args
  | EMethod receiver _ _ args => expression_primitives receiver ++ flat_map expression_primitives args
  | ELet _ a b | ESeq a b | EWhile a b => expression_primitives a ++ expression_primitives b
  | EIf a b c => expression_primitives a ++ expression_primitives b ++ expression_primitives c
  | ERecord fields => flat_map (fun field => expression_primitives (snd field)) fields
  | EMatch e arms => expression_primitives e ++ flat_map (fun arm => expression_primitives (snd arm)) arms
  end
with binding_primitives (binding : binding) : list op :=
  match binding with
  | BValue _ e => expression_primitives e
  | BPrimitive _ operation => [operation]
  | _ => []
  end.
Fixpoint entry_primitives (entry : provision_value) : list op :=
  match entry with
  | PrimitiveClosure operation _ => [operation]
  | FunctionClosure _ (EnvironmentWithAliases _ _ captured) | FunctorClosure _ _ (EnvironmentWithAliases _ _ captured) _ =>
      flat_map (fun entry => entry_primitives (snd entry)) captured
  | _ => []
  end.
Definition required_primitives (program : program) : list op :=
  flat_map (fun fn => expression_primitives (snd fn)) (program_functions program) ++
  flat_map (fun fn => flat_map binding_primitives (fc_bindings (snd fn))) (program_functors program) ++
  flat_map (fun entry => entry_primitives (snd entry)) (provision_values (program_environment program)).

Definition program_of (p : program) (argv : list (list int))
    (rd : list int -> option (list int)) : program_view :=
  ProgramView (fun f => Elaboration.nlook f (program_functions p))
    (fun u => Elaboration.nlook u (program_functors p))
    (program_bool p) (program_true p) (program_false p) argv rd (program_casts p) (program_machines p).

(* ######################################################################## *)
(* Public source-to-world contract                                           *)
(* ######################################################################## *)

(* Rocq strings store bytes. source contents must pass the UTF-8 check
   below; paths use normalized absolute names in the modeled filesystem. *)
Definition canonical_path (name : string) : bool :=
  Loader.is_abs name &&
  String.eqb (Loader.normalize "/" None name) name.

Record file_path := AbsolutePath {
  path_text : string;
  path_canonical : canonical_path path_text = true
}.
(* A build receives an explicit finite snapshot, not an oracle for an
   unbounded filesystem. Distinct entries have distinct canonical paths. *)
Record source := Source {
  source_files : list (file_path * string);
  source_unique : NoDup (map (fun file => path_text (fst file)) source_files)
}.
(* program is the concrete code representation. Admissible below states
   its source-level typechecking condition; arbitrary code records may
   still be malformed. *)

Definition path_of_string (name : string) : option file_path :=
  match canonical_path name as b return canonical_path name = b -> option file_path with
  | true => fun Valid => Some (AbsolutePath name Valid)
  | false => fun _ => None
  end eq_refl.

Definition source_lookup (snapshot : source) (name : string) : option string :=
  option_map snd (find (fun file => String.eqb name (path_text (fst file)))
    (source_files snapshot)).

Definition source_read (snapshot : source) (name : string) : option string :=
  match source_lookup snapshot name with
  | Some text => if Syntax.string_utf8_valid text then Some text else None
  | None => None
  end.

(* The language uses the same static platform at build and run time.
   The platform supplies its raw-host contract and adapts primitive calls. *)
Module Type PLATFORM.
  Include STATIC_PLATFORM.
  Parameter hty : Type.
  Parameter call : forall S : Type,
    (program_view -> op -> list value -> S -> signal * S) ->
    program_view -> op -> option ty -> list value -> S -> signal * S.
  Parameter laws : forall S : Type, program_view -> (op -> bool) ->
    (program_view -> op -> list value -> S -> signal * S) -> (hty -> S -> Prop) -> Prop.
End PLATFORM.

(* A host chooses the primitive-managed state and raw operations.
   Public also keeps arguments and the read-only file snapshot in the world. *)
Module Type HOST_FOR (Pf : PLATFORM).
  Parameter state : Type.
  Parameter primitive : program_view -> op -> list value -> state -> signal * state.
  Parameter heap_ok : Pf.hty -> state -> Prop.
End HOST_FOR.

Module PublicFor (Pf : PLATFORM) (Host : HOST_FOR Pf).
  Module Link := Linking Pf.

  Record state := World {
    arguments : list (list int);
    read_file : list int -> option (list int);
    external : Host.state
  }.

  Record interface := HostInterface {
    supports : op -> bool;
    primitive : program_view -> op -> list value -> Host.state -> signal * Host.state;
    heap_ok : Pf.hty -> Host.state -> Prop
  }.

  (* The default host makes every operation available; the platform laws
     describe the behavior of its raw operations. *)
  Definition default_interface : interface :=
    HostInterface (fun _ => true) Host.primitive Host.heap_ok.

  Definition runtime (program : program) (initial : state) : program_view :=
    program_of program (arguments initial) (read_file initial).

  Definition interface_laws (P : program_view) (interface : interface) : Prop :=
    @Pf.laws Host.state P (supports interface) (primitive interface) (heap_ok interface).

  Definition Compatible (program : program) (interface : interface) : Prop :=
    Link.Ready program /\
    (forall operation, In operation (required_primitives program) -> supports interface operation = true) /\
    forall argv read, interface_laws (program_of program argv read) interface.

  (* Traps, exits and unsupported primitives are explicit outcomes. Wrong
     represents an internal error, including a missing interface operation. *)
  Inductive outcome :=
  | Returned : value -> outcome
  | Exited : int -> outcome
  | Trapped : nat -> outcome
  | Unsupported : nat -> outcome
  | Wrong : nat -> outcome.

  Record execution := Result {
    execution_outcome : outcome;
    final_state : state
  }.

  (* None means this finite observation has not produced an outcome. *)
  Definition partial (A : Type) := nat -> option A.
  Definition e_missing_primitive := 15.

  Module ReferenceHost <: PRIMITIVES.
    Definition mstate := (interface * state)%type.
    Definition raw_primitive P p values (machine : mstate) :=
      let '(interface, state) := machine in
      if supports interface p then
        let '(out, next) := primitive interface P p values (external state) in
        (out, (interface, World (arguments state) (read_file state) next))
      else (Stuck e_missing_primitive, machine).
    Definition run_prim := @Pf.call mstate raw_primitive.
  End ReferenceHost.
  Module Reference := Interpreter ReferenceHost.

  Definition observe (result : Reference.evaluation) : option execution :=
    let state := snd (Reference.evaluation_state result) in
    match Reference.evaluation_signal result with
    | Normal value | Returning value => Some (Result (Returned value) state)
    | Exit code => Some (Result (Exited code) state)
    | Trap code => Some (Result (Trapped code) state)
    | UnsupportedOperation code => Some (Result (Unsupported code) state)
    | Stuck code => Some (Result (Wrong code) state)
    | Breaking => Some (Result (Wrong Reference.e_break) state)
    | OutOfFuel => None
    end.

  (* The build target is this platform. source declarations and primitive
     bindings are checked against one shared static reading of the bundle.
     The prelude is fixed configuration and is read through source too. *)
  Definition Builds (prelude : file_path) (source : source) (entry : file_path)
      (program : program) : Prop :=
    exists bundle world,
      Loader.Bundled (source_read source) "/" (path_text prelude) (path_text entry) bundle /\
      Elaboration.ElaboratesIn bundle world program /\ Link.Linked world program.

  Definition Admissible (prelude : file_path) (program : program) : Prop :=
    exists source entry, Builds prelude source entry program.

  Definition Runs (program : program) (interface : interface)
      (initial : state) (result : execution) : Prop :=
    exists fuel,
      observe (Reference.run_main fuel (runtime program initial)
        (program_main program) (program_environment program) (interface, initial)) = Some result.

  (* The five public obligations for an implementation. Completeness covers builds
     for this platform and permits the implementation's own fresh symbols.
     Compatibility with the default interface follows from building;
     other compatible interfaces are covered by the same run/safety claims.
     Safety concerns Builds and Runs themselves; executable safety follows
     by combining it with build_sound and run_sound. *)
  Module Type CORRECT.
    Parameter prelude : file_path.
    Parameter build : source -> file_path -> option program.
    Parameter run : program -> interface -> state -> partial execution.

    Axiom build_sound : forall source entry program,
      build source entry = Some program ->
      Builds prelude source entry program /\ Compatible program default_interface.

    Axiom build_complete : forall source entry,
      (exists program, Builds prelude source entry program) ->
      exists program, build source entry = Some program.

    Axiom run_sound : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall fuel result,
        run program interface initial fuel = Some result ->
        Runs program interface initial result.

    Axiom run_complete : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall result, Runs program interface initial result ->
        exists fuel, run program interface initial fuel = Some result.

    Axiom safety : forall program interface initial,
      Admissible prelude program ->
      Compatible program interface ->
      forall code final,
        ~ Runs program interface initial (Result (Wrong code) final).
  End CORRECT.
End PublicFor.

End Semantics.

(* Public entry points; detailed definitions live in the model's namespace. *)
Definition file_path := Semantics.file_path.
Definition source := Semantics.source.
Definition Source := Semantics.Source.
Definition source_files := Semantics.source_files.
Definition source_lookup := Semantics.source_lookup.
Definition program := Semantics.program.
Definition path_text := Semantics.path_text.
Definition path_canonical := Semantics.path_canonical.
Definition canonical_path := Semantics.canonical_path.
Definition path_of_string := Semantics.path_of_string.
Definition source_read := Semantics.source_read.
Definition AbsolutePath := Semantics.AbsolutePath.
