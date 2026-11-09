(* Audit.v — source-to-program and program-to-world semantics.
   The public build/run interface and its five obligations appear at the end.
   All definitions of the Builds and Runs relations are in this file:
     I. Source syntax and module loading.
     II. Elaborated programs and runtime structures.
     III. Reference execution.
     IV. Primitive signatures.
     V. Declarative elaboration.
   Only Rocq's standard library is imported. *)
From Stdlib Require Import PrimInt63 Uint63 PrimFloat ZArith.
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

   It is in two layers, as docs/reference/syntax.md is:

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

(* ===================== Names: Path p, Head h, Spec s ====================== *)

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
Inductive machine := MI32 | MI64 | MF32 | MF64 | MStructRef | MArrayRef.
Definition machine_eqb (a b : machine) : bool :=
  match a, b with
  | MI32, MI32 | MI64, MI64 | MF32, MF32 | MF64, MF64
  | MStructRef, MStructRef | MArrayRef, MArrayRef => true
  | _, _ => false
  end.

Inductive ty : Type :=
| TMachine : machine -> ty
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
  | TMachine a, TMachine b => machine_eqb a b
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
| VUnit                : value
| VTag                 : nsym -> value -> value
| VI32                 : int -> value                (* invariant kept by every op: < 2^32 *)
| VF64                 : float -> value
| VRecord              : list (fld * value) -> value
| VF32                 : float -> value              (* a binary64 holding a binary32 value *)
| VI64                 : Z -> value                  (* invariant kept by every op: [0, 2^64) *)
| VHandle              : address -> value                (* host-facing opaque heap identity *)
| VCheckedHandle       : bool -> ty -> address -> value. (* allocation kind and ground held type *)

(* ===================== Primitives ======================================== *)

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
   program with UnsupportedOperation, which the oracle reports as "unsupported" *)
| PUnsupported : nat -> prim.

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
| BFunctor             : usym -> usym -> smap -> smap -> binding
| BCopyFunctor         : usym -> usym -> smap -> binding
| BApply               : usym -> subst -> list (tsym * tsym) -> smap -> binding
| BPrimitive           : item -> prim -> binding.

(* Named builders are notation for the single binding form. *)
Definition apply_expression u arguments inputs body := EBind (BApply u arguments [] inputs) body.
Definition apply_output_expression u arguments outputs inputs body :=
  EBind (BApply u arguments outputs inputs) body.
Definition bind_type_expression t tau e := EBind (BType t tau) e.
Definition bind_value_expression v value e := EBind (BValue v value) e.
Definition bind_function_expression f body types captures e := EBind (BFunction f body types captures) e.
Definition bind_method_expression receiver method application body types captures e :=
  EBind (BMethod receiver method application body types captures) e.
Definition bind_functor_expression u body linked captured e := EBind (BFunctor u body linked captured) e.
Definition copy_functor_expression u provider inputs e := EBind (BCopyFunctor u provider inputs) e.
Definition bind_primitive_expression key operation e := EBind (BPrimitive key operation) e.

(* ===================== Runtime structures ================================ *)

(* Generation meanings are stable within an activation. Source aliases track
   rebinding and final type outputs separately. A closure captures all three
   namespaces; grounding never reads the mutable source aliases. *)
Inductive provision_value : Type :=
| ValueProvision : value -> provision_value
| FunctionClosure : fsym -> environment -> provision_value
| FunctorClosure : usym -> list smap -> environment -> provision_value
| PrimitiveClosure : prim -> option ty -> provision_value
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
Definition primitive_closure (d : environment) (i : item) (p : prim) : provision_value :=
  PrimitiveClosure p (match p, i with
    | PBox _, IMth tau _ _ | PArrNew _, IMth tau _ _ => Some (ground d tau)
    | _, _ => None end).
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

Fixpoint adapt_provisions (d : environment) (sms : list smap) : option environment :=
  match sms with
  | [] => Some d
  | sm :: r =>
      match adapt_provisions d r with
      | Some d' => select_provisions d' sm
      | None => None
      end
  end.

Definition parameter_values (ws : list value) : locals := combine (seq 0 (List.length ws)) ws.

(* Program bodies and their declared formal interfaces. *)
Record cast_spec : Type := CastSpec {
  cast_array : bool;
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
   model does not implement (PUnsupported i is the i-th). *)
Record functor_code := FunctorCode {
  fc_inputs : list tsym;
  (* the inputs each application supplies (the rest are captured) *)
  fc_linked : list tsym;
  fc_outputs : tele;
  fc_bindings : list binding;
  (* where the body left each result item: a result key (as the functor
     declares it) paired with the body's own key for it; an item not
     listed is read under its own key *)
  fc_sources : list (item * item)
}.
Record program : Type := Program {
  program_functions : list (nat * expr);
  program_functors : list (usym * functor_code);
  program_bool : nat; program_true : nat; program_false : nat;
  program_main : nat;
  program_environment : environment;
  program_unsupported : list string;
  program_casts : list (nsym * cast_spec)
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
  (* selected Ref/Array tag -> (array?, held-type parameter) *)
  view_casts : list (nsym * cast_spec)
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
   captured generations cannot occupy this activation's local output names. *)
Definition application_environment (P : program_view) (d : environment) (h : usym) (th : subst) (dc : environment) : environment :=
  let linked := match view_functor P h with Some code => fc_linked code | None => [] end in
  let meanings := map (fun source => (source,
    match (if existsb (Nat.eqb source) linked then lookup_subst source th else None) with
    | Some meaning => ground d meaning
    | None => match output_meaning dc source with
      | Some meaning => meaning | None => TAbs source end
    end)) (match view_functor P h with Some code => fc_inputs code | None => [] end) in
  EnvironmentWithAliases meanings meanings (provision_values dc).

(* Formal input interpretation and output destination roles are distinct.
   Result provisions use the output roles in preference to input roles. *)
Definition result_substitution (inputs : subst) (outputs : list (tsym * tsym)) : subst :=
  map (fun q => (fst q, TAbs (snd q))) outputs ++ inputs.
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
Definition result_environment (caller : environment) (outputs : list (tsym * tsym))
    (d3 : environment) (Dr : tele) (dr : environment) : option environment :=
  match read_result_types d3 Dr outputs with
  | Some (writes, aliases) =>
      if fresh_result_types writes (type_meanings caller) then
        Some (EnvironmentWithAliases writes aliases (provision_values dr))
      else None
  | None => None
  end.

(* Outcomes.  Normal/Stuck/OutOfFuel are the core's Ok/Err/OutOfFuel; Returning and Breaking
   are control transfers; Trap is a Wasm trap (well-defined failure),
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

(* Rule A: an application at θ files each result entry under the result
   item read at θ — the key its scope refers to it by. *)
Definition inst_smap (th : subst) (D : tele) : smap :=
  map (fun i => (app_subst_item th i, i)) (filter (fun i => negb (is_ty_item i)) D).
(* A result item is read from the body under the key its source names
   (fc_sources), by default its own: a linked body binds a result method
   under its own receiver, not under F's spelling of it. *)
Fixpoint source_key (sources : list (item * item)) (i : item) : item :=
  match sources with
  | [] => i
  | (k, s) :: rest => if item_eqb i k then s else source_key rest i
  end.
Definition result_smap (th : subst) (D : tele) (sources : list (item * item)) : smap :=
  map (fun q => (fst q, source_key sources (snd q))) (inst_smap th D).

(* Reference execution is parameterized by primitive operations and their
   state. Public.interface_laws specifies their typed and heap behavior.
   A concrete implementation must prove that its representation refines this
   interface; the existing ExecutionModel proofs do not supply that bridge. *)
Module Type PRIMITIVES.
  Parameter mstate : Type.
  Parameter run_prim : program_view -> prim -> list value -> mstate -> signal * mstate.
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
        | Some tau, VCheckedHandle k' tau' _ =>
            if Bool.eqb (cast_array spec) k' && ty_eqb (ground d tau) tau'
            then Normal (VTag n w) else Trap 37
        | _, _ => Trap 37
        end
      else Normal (VTag n w)
  end.
Definition raw_receiver (w : value) : value :=
  match w with
  | VTag n (VCheckedHandle _ _ a) => VTag n (VHandle a)
  | _ => w
  end.
Definition primitive_arguments (p : prim) (ws : list value) : list value :=
  match p, ws with
  | PArrCopy, [r; i; s; j; count] => [raw_receiver r; i; raw_receiver s; j; count]
  | PDeref, r :: args | PRefSet, r :: args | PArrLen, r :: args
  | PArrGet, r :: args | PArrSet, r :: args | PArrFill, r :: args => raw_receiver r :: args
  | _, _ => ws
  end.
Definition allocation_result (p : prim) (tau : ty) (o : signal) : signal :=
  match p, o with
  | PBox _, Normal (VTag n (VHandle a)) => Normal (VTag n (VCheckedHandle false tau a))
  | PArrNew _, Normal (VTag n (VHandle a)) => Normal (VTag n (VCheckedHandle true tau a))
  | _, _ => o
  end.
Definition run_prim_at (P : program_view) (p : prim) (ot : option ty)
    (ws : list value) (st : mstate) : signal * mstate :=
  let '(o, st') := run_prim P p (primitive_arguments p ws) st in
  (match ot with Some tau => allocation_result p tau o | None => o end, st').

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
      | BFunctor u h sm smc :: r =>
          match select_provisions d smc with
          | Some dc => eval_bindings k P (install_provision (IFun u) (FunctorClosure h [sm] (capture_environment d dc)) d) g st r
          | None => inr (Evaluation (Stuck e_compose) g st)
          end
      | BCopyFunctor u u' sm :: r =>
          match lookup_provision (IFun u') d with
          | Some (FunctorClosure h sms dc) =>
              eval_bindings k P (install_provision (IFun u) (FunctorClosure h (sms ++ [sm]) dc) d) g st r
          | _ => inr (Evaluation (Stuck e_fun) g st)
          end
      | BApply u th outputs sm :: r =>
          match apply_functor k P d u th outputs sm st with
          | inl (dr, st1) => eval_bindings k P (append_environment dr d) g st1 r
          | inr rr => inr (with_g g rr)
          end
      | BPrimitive i p :: r => eval_bindings k P (install_provision i (primitive_closure d i p) d) g st r
      end
  end
with apply_functor (fuel : nat) (P : program_view) (d : environment) (u : usym) (th : subst)
    (outputs : list (tsym * tsym)) (sm : smap) (st : mstate) {struct fuel} : (environment * mstate) + evaluation :=
  match fuel with
  | 0 => inr (Evaluation OutOfFuel [] st)
  | S k =>
      match lookup_provision (IFun u) d with
      | Some (FunctorClosure h sms dc) =>
          match view_functor P h with
          | Some code =>
              let Dr := fc_outputs code in
              let bs := fc_bindings code in
              match select_provisions d sm with
              | Some d1 =>
                  match adapt_provisions d1 sms with
                  | Some d2 =>
                      match eval_bindings k P (application_environment P d h th (append_environment d2 dc)) [] st bs with
                      | inl (d3, _, st3) =>
                          match select_provisions d3 (result_smap (result_substitution th outputs) Dr (fc_sources code)) with
                          | Some dr => match result_environment d outputs d3 Dr dr with
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
(* Part IV. The primitives' signatures                                       *)
(* ######################################################################## *)

Inductive shape : Type := ShapeI32 | ShapeI64 | ShapeF32 | ShapeF64 | ShapeUnit | ShapeBool
(* Track B: an address as `Memories`' members receive it (an `Addr`, a tag
   over an I32), and any tag (the memory a member is a method of) *)
| ShapeAddress | ShapeTag
(* no value: the result of an operation declared to return `|`, so the
   result law (`primitive_result`) forbids it to return normally *)
| ShapeNever.

Definition value_shape (P : program_view) (w : value) (t : shape) : Prop :=
  match t, w with
  | ShapeI32, VI32 _ | ShapeI64, VI64 _ | ShapeF32, VF32 _ | ShapeF64, VF64 _ => True
  | ShapeUnit, VUnit => True
  | ShapeBool, VTag b (VTag t' VUnit) => b = view_bool P /\ (t' = view_true P \/ t' = view_false P)
  | ShapeAddress, VTag _ (VI32 _) => True
  | ShapeTag, VTag _ _ => True
  | ShapeNever, _ => False
  | _, _ => False
  end.

(* The signatures of lib/wasm.moss, lib/wasip1.moss and wasistd's i32_bool
   (Moss's I32 / I64 / F32 / F64 / () / Bool).  The GC primitives and the
   unmodelled ones have none here: their results depend on the heap. *)
Definition prim_sig (p : prim) : option (list shape * shape) :=
  match p with
  | PI32Bin _ | PI32Cmp _ => Some ([ShapeI32; ShapeI32], ShapeI32)
  | PI32Un _ => Some ([ShapeI32], ShapeI32)
  | PF64Bin _ => Some ([ShapeF64; ShapeF64], ShapeF64)
  | PF64Cmp _ => Some ([ShapeF64; ShapeF64], ShapeI32)
  | PF64Un _ => Some ([ShapeF64], ShapeF64)
  | PConv (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([ShapeF64], ShapeI32)
  | PConv (CConvS | CConvU) => Some ([ShapeI32], ShapeF64)
  | PLoad _ => Some ([ShapeI32], ShapeI32)
  | PStore _ => Some ([ShapeI32; ShapeI32], ShapeUnit)
  | PF64Load => Some ([ShapeI32], ShapeF64)
  | PF64Store => Some ([ShapeI32; ShapeF64], ShapeUnit)
  | PMemSize => Some ([], ShapeI32)
  | PMemGrow => Some ([ShapeI32], ShapeI32)
  | PMemCopy | PMemFill => Some ([ShapeI32; ShapeI32; ShapeI32], ShapeUnit)
  | PUnreachable => Some ([], ShapeNever)
  | PFdWrite => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32], ShapeI32)
  | PProcExit => Some ([ShapeI32], ShapeUnit)
  | PArgsSizesGet | PArgsGet => Some ([ShapeI32; ShapeI32], ShapeI32)
  | PPathOpen => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32; ShapeI32; ShapeI64; ShapeI64; ShapeI32; ShapeI32], ShapeI32)
  | PFdRead => Some ([ShapeI32; ShapeI32; ShapeI32; ShapeI32], ShapeI32)
  | PFdClose => Some ([ShapeI32], ShapeI32)
  | PI32Bool => Some ([ShapeI32], ShapeBool)
  | PF32Bin _ => Some ([ShapeF32; ShapeF32], ShapeF32)
  | PF32Cmp _ => Some ([ShapeF32; ShapeF32], ShapeI32)
  | PF32Un _ => Some ([ShapeF32], ShapeF32)
  | PConv32 (CTruncS | CTruncU | CTruncSatS | CTruncSatU) => Some ([ShapeF32], ShapeI32)
  | PConv32 (CConvS | CConvU) => Some ([ShapeI32], ShapeF32)
  | PF32Load => Some ([ShapeI32], ShapeF32)
  | PF32Store => Some ([ShapeI32; ShapeF32], ShapeUnit)
  | PDemote => Some ([ShapeF64], ShapeF32)
  | PPromote => Some ([ShapeF32], ShapeF64)
  | PI32ReinterpF32 => Some ([ShapeF32], ShapeI32)
  | PF32ReinterpI32 => Some ([ShapeI32], ShapeF32)
  | PI64Bin _ => Some ([ShapeI64; ShapeI64], ShapeI64)
  | PI64Cmp _ => Some ([ShapeI64; ShapeI64], ShapeI32)
  | PI64Un U64Eqz => Some ([ShapeI64], ShapeI32)
  | PI64Un _ => Some ([ShapeI64], ShapeI64)
  | PWrap => Some ([ShapeI64], ShapeI32)
  | PExtendS | PExtendU => Some ([ShapeI32], ShapeI64)
  | PI64Load _ => Some ([ShapeI32], ShapeI64)
  | PI64Store _ => Some ([ShapeI32; ShapeI64], ShapeUnit)
  | PConv64 (C64Trunc f32 _ _) => Some ([if f32 then ShapeF32 else ShapeF64], ShapeI64)
  | PConv64 (C64Conv to32 _) => Some ([ShapeI64], if to32 then ShapeF32 else ShapeF64)
  | PI64ReinterpF64 => Some ([ShapeF64], ShapeI64)
  | PF64ReinterpI64 => Some ([ShapeI64], ShapeF64)
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy | PUnsupported _ => None
  end.

(* Track B.  The memory primitives as the platform provides them, as
   methods of `Memories` (D79): an address
   argument is an `Addr`, and memory.size and memory.grow receive the
   memory they are a method of. The host must implement these source
   calling conventions as well as the plain primitive signatures. *)
Definition prim_msig (p : prim) : option (list shape * shape) :=
  match p with
  | PLoad _ => Some ([ShapeAddress], ShapeI32)
  | PStore _ => Some ([ShapeAddress; ShapeI32], ShapeUnit)
  | PF64Load => Some ([ShapeAddress], ShapeF64)
  | PF64Store => Some ([ShapeAddress; ShapeF64], ShapeUnit)
  | PF32Load => Some ([ShapeAddress], ShapeF32)
  | PF32Store => Some ([ShapeAddress; ShapeF32], ShapeUnit)
  | PI64Load _ => Some ([ShapeAddress], ShapeI64)
  | PI64Store _ => Some ([ShapeAddress; ShapeI64], ShapeUnit)
  | PMemCopy => Some ([ShapeAddress; ShapeAddress; ShapeI32], ShapeUnit)
  | PMemFill => Some ([ShapeAddress; ShapeI32; ShapeI32], ShapeUnit)
  | PMemSize => Some ([ShapeTag], ShapeI32)
  | PMemGrow => Some ([ShapeTag; ShapeI32], ShapeI32)
  | _ => None
  end.

(* Which library fn is which primitive: the elaborator gives `main`'s need of
   a lib/wasm.moss or lib/wasip1.moss fn named n the primitive
   `prim_of_name n`.  An auditor checks this table
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
   not yet proved to refine this model. ComponentContracts.v and the
   existing source proofs concern ExecutionModel. AuditRepresentation.v names
   the required representation/refinement boundary. AuditBuildExample.v checks
   an inhabited public build/run derivation; it is not a soundness proof.

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

Lemma explicit_in_imports : forall b m t imp,
  In (t, imp) (explicit_imports m) -> In (t, imp) (imports_of b m).
Proof.
  intros b m t imp H. unfold imports_of, explicit_imports in *.
  apply in_or_app. right. exact H.
Qed.

Lemma prelude_in_imports : forall b m, am_prelude m = true ->
  In (b_prelude b, AImport (b_prelude b) None true []) (imports_of b m).
Proof. intros b m H. unfold imports_of. rewrite H. left. reflexivity. Qed.

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
Record cx := Cx {
  c_ty : subst;
  c_sub : subst;
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
  (* The context and needs each fn, method or functor symbol forms. Merging
     is symmetric but its substitution picks a representative ([D43]); every
     rule reading a symbol's context uses this one, so call sites, providers
     and bodies agree on canonical types and provision keys. *)
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

(* Every visible binding still originates in an export, either here or
   in a dependency. Clients that only need declaration provenance need
   not depend on the priority rules used to select that binding. *)
Lemma scope_exports : forall b W p ns n e, InScope b W p ns n e ->
  Exports b W p ns n e \/ exists m t imp, module_of b p = Some m /\
    In (t, imp) (imports_of b m) /\ Exports b W t ns n e.
Proof.
  intros b W p ns n e H. destruct H as [p ns n e Hordinary | p m ns n e Hm Hpre Hexp Hnone].
  - destruct Hordinary as [p ns n e Hexp | p m t imp ns n e Hm Hin Hg Hexp].
    + left. exact Hexp.
    + right. exists m, t, imp. split; [exact Hm|].
      split; [apply explicit_in_imports; exact Hin|exact Hexp].
  - right. exists m, (b_prelude b), (AImport (b_prelude b) None true []).
    split; [exact Hm|]. split; [apply prelude_in_imports; exact Hpre|exact Hexp].
Qed.

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

Definition provides (C : cx) (i : item) : Prop :=
  match i with
  | IVal v => nlook v (c_vals C) <> None
  | IFn f => nlook f (c_fns C) <> None
  | IMth r m _ => mlook r m (c_mths C) <> None
  | IFun u => nlook u (c_funs C) <> None
  | ITy t => nlook t (c_ty C) <> None
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
Inductive Select {A : Type} (P : A -> Prop) : list A -> list A -> Prop :=
| SelectNil : Select P [] []
| SelectKeep : forall x xs selected, P x -> Select P xs selected ->
    Select P (x :: xs) (x :: selected)
| SelectDrop : forall x xs selected, ~ P x -> Select P xs selected ->
    Select P (x :: xs) selected.
Definition AppOf (W : world) (s : nat) (tm : subst) (a : inst) : Prop :=
  exists requirements,
    a = map (fun r => (r, nlook r tm)) requirements /\
    ((kind_sym W s = Some DKMeth /\ Select (SigMentions W s) (w_reqs W s) requirements) \/
     (kind_sym W s <> Some DKMeth /\ requirements = w_reqs W s)).

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
   generations of the telescope, each to one value (as the elaborator's
   union-find does; a shadowed second value would never be read, but
   would still sit in the context's substitution).  A merge identifying two distinct
   concrete heads has no unifier, so the telescope does not form. *)
Definition Unifies (s : subst) (es : list entry) : Prop :=
  forall e1 e2 k, In e1 es -> In e2 es -> entry_key s e1 = Some k -> entry_key s e2 = Some k ->
    entry_sig s e1 = entry_sig s e2.

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
  end.
Definition MostGeneralMerge (s : subst) (es : list entry) : Prop :=
  Unifies s es /\
  (forall t, app_subst s (app_subst s t) = app_subst s t) /\
  (forall s', Unifies s' es -> (forall t, app_subst s' (app_subst s t) = app_subst s' t)) /\
  (* σ maps only generations of the telescope, to types over them *)
  (forall g t, In (g, t) s -> In g (flat_map entry_gens es) /\ incl (gens t) (flat_map entry_gens es)) /\
  (* σ is a function: no generation has a second, shadowed value *)
  (forall g t, In (g, t) s -> lookup_subst g s = Some t).

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
  end.
Definition entries_cx (s : subst) (tm : subst) (es : list entry) : cx :=
  let freeze a := map (fun q => (fst q, option_map (app_subst s)
    (match snd q with Some t => Some t | None => nlook (fst q) tm end))) a in
  Cx (map (fun q => (fst q, app_subst s (snd q))) tm) s
     (flat_map (entry_provision s freeze) es) [].

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
    MostGeneralMerge σ (es ++ er) /\ C = entries_cx σ tmr (es ++ er) /\ Needs σ (es ++ er) ns /\
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
   bind two members of one class differently. *)
Definition CallTheta (W : world) (C Cs : cx) (s : nat) (th : subst) : Prop :=
  map fst th = map fst (c_ty Cs) /\
  (forall r, In r (map fst (c_ty Cs)) -> nlook r (c_ty C) <> None ->
     lookup_subst r th = Some (canon C (app_subst (c_ty C) (TAbs r)))) /\
  (forall g t, In (g, t) (c_sub Cs) -> canon C (app_subst th (TAbs g)) = canon C (app_subst th t)).

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
   from context C at instantiation th is answered by satisfaction map sm:
   sm sends each need (exactly once) to the caller's provision of it read
   here, which must be in force, at the instantiation the callee needs it
   at (coherence), with the type or signature the callee's context gives
   it read at th; and the callee's merges ([D43]) must hold here. A
   provision's type can mention types that are not its requirements, so
   recorded requirement meanings alone do not determine it. *)
Definition Satisfies (C : cx) (Cs : cx) (th : subst) (ns : list item) (sm : smap) : Prop :=
  (* each need, by its run-time key in the callee, to the caller's
     provision of it read here, by its run-time key here (note 28) *)
  sm = map (fun k => (rkey_of Cs k, rkey_of C (at_th C th k))) ns /\
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
    MostGeneralMerge σ es /\ C = entries_cx σ tm' es /\ Needs σ es ns /\
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

Definition output_cx (C : cx) (tm : subst) (es : list entry) : cx :=
  Cx (map (fun q => (fst q, canon C (snd q))) tm) (c_sub C)
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
  fr_mod : string; fr_ret : option ty; fr_this : option ty; fr_loop : bool;
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

Definition add_local (C : cx) (n : name) (t : ty) (mu : lmut) : cx :=
  Cx (c_ty C) (c_sub C) (c_provisions C) ((n, (t, mu)) :: c_locals C).
Definition set_ty (C : cx) (t : nat) (tau : ty) : cx :=
  Cx ((t, tau) :: c_ty C) (c_sub C) (c_provisions C) (c_locals C).
Definition set_provision (C : cx) (k : item) (p : provision) : cx :=
  Cx (c_ty C) (c_sub C) ((k,p) :: filter (fun q => negb (item_eqb k (fst q))) (c_provisions C)) (c_locals C).
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
Definition lexical_capture (C : cx) : type_capture :=
  TypeCapture
    (map (fun generation => (generation, TAbs generation)) (context_gens C))
    (map (fun q => (fst q, canon C (snd q))) (c_ty C)).
Definition FreshFor (W : world) (F : frame) (C : cx) (gs : list nat) : Prop :=
  NoDup gs /\ Forall (fun g => Fresh W g /\
    ~ In g (fr_fresh F ++ context_gens C ++
       (match fr_ret F with Some t => gens t | None => [] end) ++
       match fr_this F with Some t => gens t | None => [] end)) gs.

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
    (kind = "INTEGER" \/ kind = "DECIMAL") /\ Syntax.lit_norm raw = text.

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

(* [D84] rule 7: θ is read off the demanded key.  The result is formed once
   with each P(F) symbol standing for itself (the oracle's markers), and the
   demanded method's formal receiver there is matched against the key's
   receiver: θ's domain is exactly the P(F) symbols that receiver mentions,
   and each value is the part of the key at that symbol. *)
Definition FamilyReadOff (W : world) (C : cx) (u : usym) (ot : ty) (m : nat)
    (matching : subst) : Prop :=
  exists marks formal_result formal_keys rf a,
    (forall t, Poly W u t <-> In t marks) /\
    ProvideResult W u (family_context C (map (fun t => (t, TAbs t)) marks))
      (fun t => TAbs t) formal_result formal_keys /\
    In (IMth rf m a) formal_keys /\
    (forall t, In t (map fst matching) <-> In t marks /\ In t (gens rf)) /\
    NoDup (map fst matching) /\ canon C (app_subst matching rf) = ot.

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
    mlook t m (c_mths C') = Some (FSig [t] t) ->
    LitOp W C t m ox oy (apply_expression u th sm (EMethod ox m (rkey C' t m) [oy])).

(* Interpret exactly the prescribed tree through contextual arithmetic.
   A family may supply an operation, using the same rule as an explicit call. *)
Inductive Literal (W : world) (C : cx) : literal_tree -> expr -> ty -> Prop :=
| LiteralZero : forall z t,
    LangSym W "int" NSName "zero" z -> nlook z (c_vals C) = Some t ->
    Literal W C LZero (EVal z) t
| LiteralOne : forall o t,
    LangSym W "int" NSName "one" o -> nlook o (c_vals C) = Some t ->
    Literal W C LOne (EVal o) t
| LiteralAdd : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangSym W "num" NSDetached "add" m -> LitOp W C t m x y result ->
    Literal W C (LAdd a b) result t
| LiteralMul : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangSym W "num" NSDetached "mul" m -> LitOp W C t m x y result ->
    Literal W C (LMul a b) result t
| LiteralDiv : forall a b x y result t m,
    Literal W C a x t -> Literal W C b y t ->
    LangSym W "num" NSDetached "div" m -> LitOp W C t m x y result ->
    Literal W C (LDiv a b) result t
| LiteralFloat : forall a x ti tp t f,
    Literal W C a x ti -> LangSym W "float" NSName "float" f ->
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
   last field but read from its first (bootstrap/SAFETY-SITES.md). *)
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

(* [D78] (the oracle's copy_outputs_match): read at F's inputs with the
   same fresh output roles, H's result gives each key of F's result the
   provision F's result gives it (each key's last setter), F's fixed
   inputs read as sigma says (as they are in force where F is bound). *)
Definition CopyOutputs (W : world) (sigma : subst) (u h : nat) (Cu : cx) : Prop :=
  exists outs gen Cr ks Cr' ks',
    (forall t, In t outs <-> FunOutputs W u t) /\ NoDup (map gen outs) /\
    (forall t, In t outs -> Fresh W (gen t)) /\
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks /\
    ProvideResult W h Cu (fun t => TAbs (gen t)) Cr' ks' /\
    (forall k, In k ks -> exists p p',
       ilook (provision_id k) (c_provisions Cr) = Some p /\
       ilook (provision_id k) (c_provisions Cr') = Some p' /\
       provision_subst sigma p = provision_subst sigma p').

(* Restore fresh output generations to their declared symbols when filing
   the reusable code body. Input meanings remain those of the target. *)
Definition output_renaming (outs : list nat) (gen : nat -> nat) (k : item) : item :=
  app_subst_item (map (fun t => (gen t, TAbs t)) outs) k.

Definition ResultRealizes (body promised : cx) (rename : subst) (keys : list item) : Prop :=
  forall k, In k keys -> exists required actual,
    ilook (provision_id k) (c_provisions promised) = Some required /\
    ilook (provision_id (at_site body (app_subst_item rename k))) (c_provisions body) = Some actual /\
    provision_refines
      (provision_subst (c_sub body) (provision_subst rename required))
      (provision_subst (c_sub body) actual).
Definition FamilyBindings (W : world) (bindings : list binding) : Prop :=
  Forall (fun binding => match binding with
    | BValue _ _ => False
    | BApply family _ _ _ => Family W family
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
  Cx (c_ty C) (c_sub Cr) (c_provisions Cr) (c_locals Cr).

Inductive FamilyStep (W : world) (C0 : cx) (wanted : list item)
    : cx -> nat -> subst -> subst -> smap -> cx -> Prop :=
| FamilyStepApply : forall C ot m u matching th sm Cr ks,
    In (mkey ot m) wanted -> mlook ot m (c_mths C0) = None ->
    OnlyFamily W C0 u (mkey ot m) ->
    FamilyApplication W C u matching th sm Cr ks -> FamilyReadOff W C u ot m matching ->
    In (mkey ot m) ks ->
    FamilyStep W C0 wanted C u matching th sm (family_restore C Cr).

(* Group identity is the inferred matching substitution.  Distinct groups
   may emit equal complete type arguments after reading the other inputs. *)
Inductive Demand (W : world) (C0 : cx) (wanted : list item)
    : list (nat * subst) -> cx -> (expr -> expr) -> cx -> Prop :=
| DemandDone : forall used C,
    Demand W C0 wanted used C (fun x => x) C
| DemandApply : forall used C u matching th sm Cm wrap Cf,
    ~ In (u, matching) used -> FamilyStep W C0 wanted C u matching th sm Cm ->
    Demand W C0 wanted ((u, matching) :: used) Cm wrap Cf ->
    Demand W C0 wanted used C (fun x => apply_expression u th sm (wrap x)) Cf.

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
    (exists F0 t', Ex W P F0 (Cx [] [] [] []) i (Some t) x t' /\ fits t' t /\
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
| ExCallD : forall F C exp pa args f Cf ns sm th g xs wrap C',
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt b, ast_of W f = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W f Cf ns ->
    Demand W C (map (at_site C) ns) [] C wrap C' ->
    Satisfies C' Cf th ns sm -> CallTheta W C' Cf f th -> SigOf W f (c_ty C') None g ->
    Args W P F C' args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp
       (wrap (ECall f th sm xs)) (sg_ret g)
(* an abstract fn: whatever provides it here *)
| ExCallA : forall F C exp pa args f g xs,
    resolve_sym W (fr_mod F) pa = Some f -> kind_sym W f = Some DKFn ->
    (exists n ps rt, ast_of W f = Some (ADFn RPlain n ps rt None)) ->
    nlook f (c_fns C) = Some g ->
    Args W P F C args (sg_params g) xs ->
    Ex W P F C (AECall pa None args) exp (EInvoke f xs) (sg_ret g)
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
    LangSym W "bool" NSName "Bool" b ->
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
| MCAttachedDefined : forall F C ox ot n args h att Ca ns sm th g xs rargs wrap Cd,
    head ot = Some h -> Attached W h n att ->
    ~ (exists m, w_scope W (fr_mod F) NSDetached n = Some (ESym m) /\ mlook ot m (c_mths C) <> None) ->
    (exists rn ps rt b, ast_of W att = Some (ADFn (RAttached rn) n ps rt (Some b))) ->
    rargs = (match ot with TNom _ xs => xs | _ => [] end) ->
    CtxOf W att Ca ns ->
    Demand W (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)
      (map (at_site (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C)) ns) []
      (fold_left (fun C q => set_ty C (fst q) (snd q)) rargs C) wrap Cd ->
    Satisfies Cd Ca th ns sm -> CallTheta W Cd Ca att th -> SigOf W att (c_ty Cd) (Some ot) g ->
    Args W P F C args (sg_params g) xs ->
    MethCall W P F C ox ot [n] args
      (wrap (ECall att th sm (ox :: xs))) (sg_ret g)
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
    Provided W P F C ox ot m args (apply_expression u th sm (EMethod ox m (rkey C' ot m) xs)) (sg_ret g)

(* a block whose type is checked against `exp` and may not let a value of
   a generation it made escape ([D78]) *)
with BlkE (W : world) (P : program) : frame -> cx -> ablock -> option ty -> expr -> ty -> Prop :=
| BlkECheck : forall F C ss tl exp x t made,
    Blk W P F C ss tl exp x t made ->
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
(* [D78]/[D84] rule 5, `bind F;` for F abstract and in force: an
   application at θ = F's inputs read here; each type output already in
   force here becomes a fresh rigid generation ([D72]); F's fixed inputs
   X(F) must denote what they did where F was provided, else a static error;
   exactly F's result is then in force, its items at the application's
   generations. *)
| BApplyAbstract : forall F C pa u fx Cu ns sm th gensof C' ks newgens outs,
    resolve_sym W (fr_mod F) pa = Some u -> nlook u (c_funs C) = Some fx ->
    FunCtx W u Cu ns ->
    (* inputs in force here, and θ reads them *)
    (forall t tt, nlook t (c_ty Cu) = Some tt -> exists here, nlook t (c_ty C) = Some here) ->
    th = map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cu) ->
    (* rule 5: fixed inputs unchanged since the provision *)
    (forall x tx, In (x, tx) fx -> exists here,
      nlook x (c_ty C) = Some here /\ canon C here = canon C tx) ->
    Satisfies C Cu th ns sm ->
    FreshFor W F C newgens ->
    (* Repeated output roles share a generation; distinct roles do not. *)
    (forall a b, FunOutputs W u a -> FunOutputs W u b -> gensof a = gensof b -> a = b) ->
    (* outputs: fresh where the symbol is already in force; one not in
       force is installed as itself, and the frame's return and receiver
       types (read where every symbol they mention is in force) do not
       mention it *)
    (forall t, FunOutputs W u t ->
       (nlook t (c_ty C) = None /\ gensof t = t /\
          ~ In t ((match fr_ret F with Some r => gens r | None => [] end) ++
                  (match fr_this F with Some r => gens r | None => [] end))) \/
       (nlook t (c_ty C) <> None /\ Fresh W (gensof t) /\ In (gensof t) newgens)) ->
    NoDup outs -> (forall t, In t outs <-> FunOutputs W u t) ->
    ProvideResult W u C (fun t => TAbs (gensof t)) C' ks ->
    (* [D78]: every output destination, fresh or not, is made here: a
       value of its type may not leave the block *)
    BindItem W P F C (Spec (Some pa) None None, None)
      (apply_output_expression u th (map (fun t => (t, gensof t)) outs) sm)
      C' (with_fresh F newgens) (map gensof outs)
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
    (* H reads its needs and inputs as F does (the oracle: input
       interpretation, D78) *)
    (forall k, In k nsh -> ilook (provision_id k) (c_provisions Ch) = ilook (provision_id k) (c_provisions Cu)) ->
    (forall x tx, nlook x (c_ty Ch) = Some tx -> canon Cu (TAbs x) = canon Cu tx) ->
    sm = map (fun k => (rkey_of Ch k, rkey_of Cu k)) nsh ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    (* and H's result reads as F's (the oracle: output interpretation, D78) *)
    CopyOutputs W (map (fun q => (fst q, canon C (snd q))) fx) u h Cu ->
    (* H's inputs, implicit ones too, are among F's (the oracle's input
       interpretation check fails on one F lacks) *)
    (forall x tx, nlook x (c_ty Ch) = Some tx -> nlook x (c_ty Cu) <> None) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (copy_functor_expression u h sm) (set_fun C u fx) F []
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
    (* 3b: the captured needs are answered here as a call would answer
       them, at G's inputs read here: provided, coherent, at G's view of
       their types, and G's merges holding of the meanings here *)
    Satisfies C Cg (map (fun q => (fst q, canon C (app_subst (c_ty C) (TAbs (fst q))))) (c_ty Cg))
      (filter (fun k => negb (existsb (item_eqb k) nsu)) nsg) smc ->
    (forall x tx, In (x, tx) fx <-> Fixed W u x /\ nlook x (c_ty C) = Some tx) ->
    BindItem W P F C (Spec (Some pa) None None, Some (AEPath pg None)) (bind_functor_expression u h sm smc) (set_fun C u fx) F []

(* [D66] a fn provider: a defined fn (its needs answered here), or an
   abstract fn in force, through a fresh defined forwarder whose body calls
   it; its signature read here must be the bound fn's. *)
with FnProvider (W : world) (P : program) : frame -> cx -> aexpr -> fsig -> option ty -> nat -> type_capture -> smap -> Prop :=
| FPDefined : forall F C pa g sg Cg ns sm th,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists n ps rt b, ast_of W g = Some (ADFn RPlain n ps rt (Some b))) ->
    CtxOf W g Cg ns -> Satisfies C Cg th ns sm -> CallTheta W C Cg g th ->
    SigOf W g (c_ty C) None (FSig (map (canon C) (sg_params sg)) (canon C (sg_ret sg))) ->
    FnProvider W P F C (AEPath pa None) sg None g (TypeCapture th th) sm
| FPForward : forall F C pa g sg w n,
    resolve_sym W (fr_mod F) pa = Some g -> kind_sym W g = Some DKFn ->
    (exists nm ps rt, ast_of W g = Some (ADFn RPlain nm ps rt None)) ->
    nlook g (c_fns C) = Some sg -> n = length (sg_params sg) ->
    Fresh W w -> body_of P w = Some (EInvoke g (map EVar (seq 0 n))) ->
    FnProvider W P F C (AEPath pa None) sg None w (TypeCapture [] []) [(IFn g, IFn g)]

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
                         | Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C t
                         | None => True end) ps (sg_params sg) ->
    match rt with Some a => exists t', TyOf W (fr_mod F) (c_ty C) this a t' /\ canon C t' = canon C (sg_ret sg)
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
                   (Cx (c_ty C) (c_sub C) (c_provisions C) []) ->
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
   that applications supply; 3d: pins may not rebind M(F); 3e: every item
   of F's result is made by the binds, at the types F promises. *)
with LinkBody (W : world) (P : program) : nat -> nat -> nat -> Prop :=
| LB : forall u gg h dg n ps a r bodies Cu nsu Cg nsg Fg wrap Cb Fb made outs gen Cr ks binds0,
    w_decl W gg = Some dg -> d_ast dg = ADFunctor n ps a r (Some bodies) ->
    FunCtx W u Cu nsu -> FunCtx W gg Cg nsg ->
    (* 3b: a captured item (value, fn, method or functor) may mention fixed
       or privately captured types, but no target input type supplied afresh
       by each application: not in its type or signature, its recorded
       instantiation, or a method's receiver. *)
    (forall k p t, In k nsg -> ~ In k nsu -> ilook (provision_id k) (c_provisions Cg) = Some p ->
       In t (provision_gens p ++ match k with IMth r _ _ => gens r | _ => [] end) ->
       nlook t (c_ty Cu) <> None -> Fixed W u t) ->
    (* G reads each need F links as F does, and its readings of F's inputs
       hold in F's context (the oracle: linked input interpretation, D84) *)
    (forall k, In k nsg -> In k nsu ->
       ilook (provision_id k) (c_provisions Cg) = ilook (provision_id k) (c_provisions Cu)) ->
    (forall x tx, nlook x (c_ty Cg) = Some tx -> nlook x (c_ty Cu) <> None ->
       (forall y, In y (gens tx) -> nlook y (c_ty Cu) <> None) -> canon Cu (TAbs x) = canon Cu tx) ->
    fr_mod Fg = d_mod dg -> fr_ret Fg = None ->
    fr_this Fg = None -> fr_loop Fg = false ->
    (* its locals are numbered as a body's are *)
    Numbering Fg None [] ->
    (forall x, In x (fr_pins Fg) <-> Mentions W u x) ->
    BindItems W P Fg Cg (concat bodies) wrap Cb Fb made ->
    (forall t, In t outs <-> FunOutputs W u t) ->
    (forall t, In t outs -> exists meaning, nlook t (c_ty Cb) = Some meaning) ->
    NoDup (map gen outs) ->
    (forall t, In t outs -> Fresh W (gen t)) ->
    ProvideResult W u Cu (fun t => TAbs (gen t)) Cr ks ->
    (* Every output has the promised signature and known type meanings. *)
    ResultRealizes Cb Cr (map (fun t => (gen t, TAbs t)) outs) ks ->
    (* h's body, as a run of binds, is what `wrap` puts around nothing *)
    BindsOf (wrap EUnit) binds0 ->
    (Family W u -> FamilyBindings W binds0) ->
    Fresh W h ->
    (* a result item, keyed as F declares it (receiver and θ in F's terms,
       note 28), is read where the body bound it: a method under the
       body's runtime key for its receiver read in the body *)
    funbody_of P h = Some (FunctorCode (map fst (c_ty Cg))
      (filter (fun t => existsb (Nat.eqb t) (map fst (c_ty Cu))) (map fst (c_ty Cg)))
      (map ITy outs ++ map (fun k => output_renaming outs gen (rkey_of Cr k)) ks) binds0
      (map (fun k => (output_renaming outs gen (rkey_of Cr k),
                      rkey_of Cb (at_site Cb (output_renaming outs gen k)))) ks)) ->
    LinkBody W P u gg h.

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

(* [D79]/platform: main's initial environment.  Each need of main must be
   something the platform provides: lib/wasm.moss's and lib/wasip1.moss's
   fns are primitives (`prim_of_name`, Part IV), wasistd's `i32_zero`/
   `i32_one` the I32 constants, and an abstract functor of those modules a
   platform binding checked by Platform.Family. *)
Definition platform_mod (W : world) (p : string) : Prop :=
  LibraryModule W "wasm" p \/ LibraryModule W "wasip1" p \/ LibraryModule W "wasistd" p.

Inductive PlatformEntry (W : world) (P : program) : item -> provision_value -> Prop :=
| PEZero : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_zero") -> PlatformEntry W P (IVal v) (ValueProvision (VI32 0%uint63))
| PEOne : forall v d, w_decl W v = Some d -> platform_mod W (d_mod d) ->
    filed_name (d_ast d) = Some (NSName, "i32_one") -> PlatformEntry W P (IVal v) (ValueProvision (VI32 1%uint63))
| PEPrim : forall f d n ps rt body pr, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = Some pr ->
    PlatformEntry W P (IFn f) (PrimitiveClosure pr None)
| PEUnsupported : forall f d n ps rt body i, w_decl W f = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFn RPlain n ps rt body -> prim_of_name n = None -> nth_error (program_unsupported P) i = Some n ->
    PlatformEntry W P (IFn f) (PrimitiveClosure (PUnsupported i) None)
| PEFun : forall u d n ps a r h, w_decl W u = Some d -> platform_mod W (d_mod d) ->
    d_ast d = ADFunctor n ps a r None -> funbody_of P h <> None ->
    PlatformEntry W P (IFun u) (FunctorClosure h [] empty_environment).

(* The runtime type prefix is a representation of the source context,
   separate from the values and code supplied by the platform. Its machine
   representations are selected from the actual library scope. *)
Definition initial_machines : list (name * machine) :=
  [("I32", MI32); ("I64", MI64); ("F32", MF32); ("F64", MF64);
   ("StructRef", MStructRef); ("ArrayRef", MArrayRef)].
Definition initial_machine_subst (W : world) : subst :=
  flat_map (fun q => match w_library W with
    | None => []
    | Some dir => match w_scope W (dir ++ "wasm.moss") NSName (fst q) with
      | Some (ESym symbol) => [(symbol, TMachine (snd q))]
      | _ => [] end end) initial_machines.
Definition InitialTypes (W : world) (C : cx) (types : subst) : Prop :=
  types = map (fun q => (fst q, app_subst (initial_machine_subst W) (snd q))) (c_ty C).

(* The relation.  [D16]: every defined fn is elaborated, reached or not. *)
Definition ElaboratesIn (b : bundle) (W : world) (P : program) : Prop :=
  WorldOf b W /\
    (forall f d rcv n ps rt body, w_decl W f = Some d -> d_ast d = ADFn rcv n ps rt (Some body) ->
       FnElab W P f) /\
    (* main: the entry's `main`, a defined fn assuming only what the
       platform provides *)
    w_scope W (b_entry b) NSName "main" = Some (ESym (program_main P)) /\
    (exists d n rt body, w_decl W (program_main P) = Some d /\ d_ast d = ADFn RPlain n [] rt (Some body)) /\
    (exists C ns types platform, CtxOf W (program_main P) C ns /\ InitialTypes W C types /\
       program_environment P = Environment types platform /\ map fst platform = map (rkey_of C) ns /\
       Forall (fun ie => PlatformEntry W P (fst ie) (snd ie)) platform) /\
    (* Bool's symbols, when lib/bool.moss is loaded ([D45]) *)
    (LangSym W "bool" NSName "Bool" (program_bool P) /\ LangSym W "bool" NSName "True" (program_true P) /\
     LangSym W "bool" NSName "False" (program_false P)
     \/ ((forall s, ~ LangSym W "bool" NSName "Bool" s) /\ program_bool P = 0 /\ program_true P = 0 /\ program_false P = 0)).

Definition Elaborates (b : bundle) (P : program) : Prop :=
  exists W, ElaboratesIn b W P.

End Elaboration.


(* The platform's source signatures. These checks relate library declarations
   to the machine operations supplied at linking, using the same world as
   elaboration. A matching name alone is insufficient. *)
Module Platform.
Import Input Elaboration.
Local Open Scope string_scope.
Local Open Scope list_scope.

Definition MachineType (W : world) (name : string) (t : ty) : Prop :=
  exists symbol, LangSym W "wasm" NSName name symbol /\
    kind_sym W symbol = Some DKType /\ t = TAbs symbol.
Definition NominalType (W : world) (library name : string) (t : ty) : Prop :=
  exists symbol args, LangSym W library NSName name symbol /\ t = TNom symbol args.

Definition ValueType (W : world) (t : ty) (shape : shape) : Prop :=
  match shape with
  | ShapeI32 => MachineType W "I32" t
  | ShapeI64 => MachineType W "I64" t
  | ShapeF32 => MachineType W "F32" t
  | ShapeF64 => MachineType W "F64" t
  | ShapeUnit => t = TUnit
  | ShapeBool => exists symbol, LangSym W "bool" NSName "Bool" symbol /\ t = TNom symbol []
  | ShapeAddress => NominalType W "wasm" "Addr" t
  | ShapeTag => exists symbol args, t = TNom symbol args
  | ShapeNever => t = never
  end.
Definition Signature (W : world) (params : list ty) (result : ty)
    (signature : option (list shape * shape)) : Prop :=
  exists inputs output, signature = Some (inputs, output) /\
    Forall2 (ValueType W) params inputs /\ ValueType W result output.

Definition ReferenceType (W : world) (array : bool) (t held : ty) : Prop :=
  exists symbol parameter args,
    LangSym W "wasm" NSName (if array then "Array" else "Ref") symbol /\
    LangSym W "wasm" NSName (if array then "Arrayed" else "Boxed") parameter /\
    t = TNom symbol args /\ nlook parameter args = Some held.

Definition GCSignature (W : world) (operation : prim) (params : list ty)
    (result : ty) : Prop :=
  match operation, params with
  | PBox symbol, [held] => LangSym W "wasm" NSName "Ref" symbol /\ ReferenceType W false result held
  | PDeref, [reference] => ReferenceType W false reference result
  | PRefSet, [reference; held] => ReferenceType W false reference held /\ result = TUnit
  | PArrNew symbol, [held; count] => LangSym W "wasm" NSName "Array" symbol /\
      MachineType W "I32" count /\ ReferenceType W true result held
  | PArrLen, [array] => MachineType W "I32" result /\ exists held, ReferenceType W true array held
  | PArrGet, [array; index] => MachineType W "I32" index /\ ReferenceType W true array result
  | PArrSet, [array; index; held] => MachineType W "I32" index /\
      ReferenceType W true array held /\ result = TUnit
  | PArrFill, [array; index; held; count] => MachineType W "I32" index /\
      MachineType W "I32" count /\ ReferenceType W true array held /\ result = TUnit
  | PArrCopy, [dst; di; src; si; count] => MachineType W "I32" di /\
      MachineType W "I32" si /\ MachineType W "I32" count /\ result = TUnit /\
      exists held, ReferenceType W true dst held /\ ReferenceType W true src held
  | _, _ => False
  end.

Definition PrimitiveSignature (W : world) (operation : prim)
    (params : list ty) (result : ty) : Prop :=
  match operation with
  (* An unsupported operation never returns a value, so imposes no result type. *)
  | PUnsupported _ => True
  | _ => Signature W params result (prim_sig operation) \/
      Signature W params result (prim_msig operation) \/ GCSignature W operation params result
  end.

Definition AtKey (W : world) (C : cx) (key : item) (operation : prim) : Prop :=
  match key with
  | IFn f => exists signature, nlook f (c_fns C) = Some signature /\
      PrimitiveSignature W operation (sg_params signature) (sg_ret signature)
  | IMth receiver method _ => exists signature,
      mlook receiver method (c_mths C) = Some signature /\
      PrimitiveSignature W operation (receiver :: sg_params signature) (sg_ret signature)
  | _ => False
  end.

(* The platform families provide GC methods or methods of Addr. *)
Definition MethodPrimitive (W : world) (key : item) (operation : prim) : Prop :=
  exists receiver method theta d form name params result body,
    key = IMth receiver method theta /\ w_decl W method = Some d /\
    d_ast d = ADFn form name params result body /\
    match form with
    | RDetached =>
        (name = "boxed" /\ exists symbol, LangSym W "wasm" NSName "Ref" symbol /\ operation = PBox symbol) \/
        (name = "arrayed" /\ exists symbol, LangSym W "wasm" NSName "Array" symbol /\ operation = PArrNew symbol)
    | RAttached path => exists symbol, resolve_sym W (d_mod d) [path] = Some symbol /\
        ((LangSym W "wasm" NSName "Ref" symbol /\
            ((name = "deref" /\ operation = PDeref) \/ (name = "set" /\ operation = PRefSet))) \/
         (LangSym W "wasm" NSName "Array" symbol /\
            ((name = "length" /\ operation = PArrLen) \/ (name = "get" /\ operation = PArrGet) \/
             (name = "set" /\ operation = PArrSet) \/ (name = "fill" /\ operation = PArrFill) \/
             (name = "copy" /\ operation = PArrCopy))) \/
         (LangSym W "wasm" NSName "Addr" symbol /\ prim_of_name name = Some operation))
    | RPlain => False
    end.

(* The machine types a platform item's signature mentions are fixed
   inputs of its functor, in its context: the platform's provision exists
   only at the machine types, so it may not be polymorphic in one
   (lib/wasm.moss: Heap, Mutable, Arrays and Memories bracket only Boxed,
   Arrayed and Memory). *)
Definition MachinesFixed (W : world) (source : usym) (inputs outputs : cx) (key : item) : Prop :=
  match key with
  | IMth receiver method _ => forall signature, mlook receiver method (c_mths outputs) = Some signature ->
      forall name machine symbol, In (name, machine) initial_machines -> LangSym W "wasm" NSName name symbol ->
      In symbol (flat_map gens (sg_ret signature :: receiver :: sg_params signature)) ->
      Fixed W source symbol /\ In symbol (map fst (c_ty inputs))
  | _ => True
  end.

Definition Family (W : world) (P : program) (source code : usym) : Prop :=
  exists inputs needs outputs keys bindings,
    FunCtx W source inputs needs /\
    (* the platform provides method items only (Elab.v platform_fun): no
       type output, which its code could not return *)
    (forall t, ~ FunOutputs W source t) /\
    ProvideResult W source inputs (fun t => TAbs t) outputs keys /\
    (* each item is installed and returned under its run-time key, the
       instantiation it is made at (rkey_of), as a linked body's are *)
    funbody_of P code = Some (FunctorCode (map fst (c_ty inputs)) (map fst (c_ty inputs))
      (map (rkey_of outputs) keys) bindings []) /\
    Forall2 (fun key binding => exists operation,
      binding = BPrimitive (rkey_of outputs key) operation /\ MethodPrimitive W key operation /\
      AtKey W outputs key operation /\ MachinesFixed W source inputs outputs key) keys bindings.

(* The handle tags carry a raw machine handle (Ref a StructRef, Array an
   ArrayRef, Addr an I32), which the primitives read as that machine value.
   A platform fn's signature must read that requirement as itself: a
   context item can bracket it (`i64_store[I32 = Addr[I32 = I64]]`), which
   would type a handle whose payload is not the raw machine type. *)
Definition raw_handles : list (string * string) :=
  [("Ref", "StructRef"); ("Array", "ArrayRef"); ("Addr", "I32")].
Definition RawHandleRead (W : world) (t : ty) : Prop :=
  forall name handle symbol args, In (name, handle) raw_handles ->
    LangSym W "wasm" NSName name symbol -> t = TNom symbol args ->
    forall raw, LangSym W "wasm" NSName handle raw -> nlook raw args = Some (TAbs raw).
Definition HandlesRead (W : world) (C : cx) (key : item) : Prop :=
  match key with
  | IFn f => forall signature, nlook f (c_fns C) = Some signature ->
      Forall (RawHandleRead W) (sg_ret signature :: sg_params signature)
  | _ => True
  end.

Definition Entry (W : world) (P : program) (C : cx)
    (key : item) (value : provision_value) : Prop :=
  match key, value with
  | IVal value, ValueProvision (VI32 _) => exists t,
      nlook value (c_vals C) = Some t /\ MachineType W "I32" t
  | IFn _, PrimitiveClosure operation None => AtKey W C key operation /\ HandlesRead W C key
  | IFun source, FunctorClosure code [] (EnvironmentWithAliases [] [] []) => Family W P source code
  | _, _ => False
  end.

(* These source types have fixed runtime representations. Check their
   declarations as well as the signatures that mention them. *)
Definition abstract_names : list string :=
  ["I32"; "I64"; "F32"; "F64"; "StructRef"; "ArrayRef"; "Memory"; "Boxed"; "Arrayed"].
Definition TagLayout (W : world) (name parameter handle : string) : Prop :=
  forall symbol, LangSym W "wasm" NSName name symbol ->
  exists param raw rest,
    LangSym W "wasm" NSName parameter param /\
    LangSym W "wasm" NSName handle raw /\ kind_sym W symbol = Some DKTag /\
    w_reqs W symbol = param :: rest /\
    PayloadOf W symbol (map (fun t => (t, TAbs t)) (w_reqs W symbol)) (TAbs raw).
Definition Layouts (W : world) : Prop :=
  (forall name symbol, In name abstract_names -> LangSym W "wasm" NSName name symbol ->
    kind_sym W symbol = Some DKType) /\
  (forall left right symbol, In left abstract_names -> In right abstract_names ->
    LangSym W "wasm" NSName left symbol -> LangSym W "wasm" NSName right symbol -> left = right) /\
  TagLayout W "Ref" "Boxed" "StructRef" /\
  TagLayout W "Array" "Arrayed" "ArrayRef" /\
  TagLayout W "Addr" "Memory" "I32" /\
  (forall symbol, LangSym W "bool" NSName "Bool" symbol ->
    exists truth falsity,
      LangSym W "bool" NSName "True" truth /\ LangSym W "bool" NSName "False" falsity /\
      kind_sym W symbol = Some DKTag /\ kind_sym W truth = Some DKUnit /\
      kind_sym W falsity = Some DKUnit /\ truth <> falsity /\ w_reqs W symbol = [] /\
      (PayloadOf W symbol [] (TUnion [TNom truth []; TNom falsity []]) \/
       PayloadOf W symbol [] (TUnion [TNom falsity []; TNom truth []]))).

Definition CastEntry (W : world) (array : bool) (entry : option (nsym * cast_spec)) : Prop :=
  let tag := if array then "Array" else "Ref" in
  let held := if array then "Arrayed" else "Boxed" in
  let raw := if array then "ArrayRef" else "StructRef" in
  (exists n t h, LangSym W "wasm" NSName tag n /\
    LangSym W "wasm" NSName held t /\ LangSym W "wasm" NSName raw h /\
    entry = Some (n, CastSpec array t
      (Some (h, TMachine (if array then MArrayRef else MStructRef))))) \/
  ((forall n, ~ LangSym W "wasm" NSName tag n) /\ entry = None).
Definition Representations (W : world) (P : program) : Prop :=
  exists reference array, CastEntry W false reference /\ CastEntry W true array /\
    program_casts P = (match reference with Some x => [x] | None => [] end) ++
                (match array with Some x => [x] | None => [] end).

(* The native entry ABI carries no argument or result lanes. Nominal units
   and unions carry discriminators; ordinary tags carry their payload lanes. *)
Inductive EntryResult (W : world) : ty -> Prop :=
| EntryUnit : EntryResult W TUnit
| EntryNever : EntryResult W never
| EntryRecord : forall fields,
    Forall (fun field => EntryResult W (snd field)) fields ->
    EntryResult W (TRecord fields)
| EntryTag : forall n arguments payload,
    PayloadOf W n arguments payload -> EntryResult W payload ->
    EntryResult W (TNom n arguments).
Definition EntryPoint (W : world) (P : program) : Prop :=
  exists context needs signature,
    CtxOf W (program_main P) context needs /\
    SigOf W (program_main P) (c_ty context) None signature /\
    sg_params signature = [] /\ EntryResult W (sg_ret signature).

Definition Linked (W : world) (P : program) : Prop :=
  EntryPoint W P /\ Layouts W /\ Representations W P /\
  exists context needs types entries,
    CtxOf W (program_main P) context needs /\ InitialTypes W context types /\
    program_environment P = Environment types entries /\
    Forall (fun entry => Entry W P context (fst entry) (snd entry)) entries.

Definition Ready (P : program) : Prop :=
  exists bundle world, ElaboratesIn bundle world P /\ Linked world P.
End Platform.

(* The operations installed anywhere in a program, including captured
   environments and platform-family bodies. Extra interface operations are
   harmless; an interface must supply at least these requirements. *)
Fixpoint expression_primitives (expression : expr) : list prim :=
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
with binding_primitives (binding : binding) : list prim :=
  match binding with
  | BValue _ e => expression_primitives e
  | BPrimitive _ operation => [operation]
  | _ => []
  end.
Fixpoint entry_primitives (entry : provision_value) : list prim :=
  match entry with
  | PrimitiveClosure operation _ => [operation]
  | FunctionClosure _ (EnvironmentWithAliases _ _ captured) | FunctorClosure _ _ (EnvironmentWithAliases _ _ captured) =>
      flat_map (fun entry => entry_primitives (snd entry)) captured
  | _ => []
  end.
Definition required_primitives (program : program) : list prim :=
  flat_map (fun fn => expression_primitives (snd fn)) (program_functions program) ++
  flat_map (fun fn => flat_map binding_primitives (fc_bindings (snd fn))) (program_functors program) ++
  flat_map (fun entry => entry_primitives (snd entry)) (provision_values (program_environment program)).

Definition program_of (p : program) (argv : list (list int))
    (rd : list int -> option (list int)) : program_view :=
  ProgramView (fun f => Elaboration.nlook f (program_functions p))
    (fun u => Elaboration.nlook u (program_functors p))
    (program_bool p) (program_true p) (program_false p) argv rd (program_casts p).

(* Addresses are semantic identities. A concrete heap supplies its own
   representation and proves that allocation, reading and writing refine these
   laws; no bit ordering or trie structure is part of the language. *)
Module Heap.
Local Unset Implicit Arguments.
(* A store typing: an address's kind (true: an array) and what its
   contents (a box's value, an array's elements) satisfy. Addresses are
   allocation identities, independent of the concrete heap representation. *)
Definition hty : Type := address -> option (bool * (value -> Prop)).

(* Ψ' extends Ψ: every address Ψ types, Ψ' types the same way. *)
Definition hext (Psi Psi' : hty) : Prop :=
  forall a k Q, Psi a = Some (k, Q) -> Psi' a = Some (k, Q).

(* w is a reference (a tag over a heap address) of kind k whose contents
   satisfy Q. *)
Definition href (Psi : hty) (k : bool) (w : value) (Q : value -> Prop) : Prop :=
  exists n a, w = VTag n (VHandle a) /\ Psi a = Some (k, Q).

(* What a GC primitive needs of its arguments, for contents satisfying Q. *)
Definition gc_pre (Psi : hty) (p : prim) (ws : list value) (Q : value -> Prop) : Prop :=
  match p, ws with
  | PBox _, [v] => Q v
  | PDeref, [r] => href Psi false r Q
  | PRefSet, [r; v] => href Psi false r Q /\ Q v
  | PArrNew _, [v; VI32 _] => Q v
  | PArrLen, [r] => href Psi true r Q
  | PArrGet, [r; VI32 _] => href Psi true r Q
  | PArrSet, [r; VI32 _; v] => href Psi true r Q /\ Q v
  | PArrFill, [r; VI32 _; v; VI32 _] => href Psi true r Q /\ Q v
  | PArrCopy, [r; VI32 _; r2; VI32 _; VI32 _] =>
      href Psi true r Q /\ exists Q2, href Psi true r2 Q2 /\ forall x, Q2 x -> Q x
  | _, _ => False
  end.

(* What it gives back, when it gives a value w, with Ψ' its store typing after. *)
Definition gc_post (Psi Psi' : hty) (p : prim) (Q : value -> Prop) (w : value) : Prop :=
  hext Psi Psi' /\
  match p with
  | PBox rf => exists a, w = VTag rf (VHandle a) /\ Psi a = None /\ Psi' a = Some (false, Q)
  | PArrNew ar => exists a, w = VTag ar (VHandle a) /\ Psi a = None /\ Psi' a = Some (true, Q)
  | PDeref | PArrGet => Q w
  | PArrLen => exists n, w = VI32 n
  | _ => w = VUnit
  end.

Definition is_gc (p : prim) : bool :=
  match p with
  | PBox _ | PDeref | PRefSet | PArrNew _ | PArrLen | PArrGet | PArrSet
  | PArrFill | PArrCopy => true
  | _ => false
  end.


End Heap.

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

Definition path_of_string (name : string) : option file_path.
Proof.
  destruct (canonical_path name) eqn:Valid.
  - exact (Some (AbsolutePath name Valid)).
  - exact None.
Defined.

Definition source_lookup (snapshot : source) (name : string) : option string :=
  option_map snd (find (fun file => String.eqb name (path_text (fst file)))
    (source_files snapshot)).

Definition source_read (snapshot : source) (name : string) : option string :=
  match source_lookup snapshot name with
  | Some text => if Syntax.string_utf8_valid text then Some text else None
  | None => None
  end.

(* The concrete host chooses the primitive-managed state and operations.
   Public keeps arguments and the read-only file snapshot in the world too,
   so the default interface is one fixed value for every invocation. *)
Module Type HOST.
  Parameter state : Type.
  Parameter primitive : program_view -> prim -> list value -> state -> signal * state.
  Parameter heap_ok : Heap.hty -> state -> Prop.
End HOST.

Module Public (Host : HOST).
  Record state := World {
    arguments : list (list int);
    read_file : list int -> option (list int);
    external : Host.state
  }.

  Record interface := HostInterface {
    supports : prim -> bool;
    primitive : program_view -> prim -> list value -> Host.state -> signal * Host.state;
    heap_ok : Heap.hty -> Host.state -> Prop
  }.

  (* Unsupported operations have a defined stopping outcome in the existing
     primitive model; providing them does not promise a returning result. *)
  Definition default_interface : interface :=
    HostInterface (fun _ => true) Host.primitive Host.heap_ok.

  Definition runtime (program : program) (initial : state) : program_view :=
    program_of program (arguments initial) (read_file initial).

  Definition primitive_result P result (out : signal) : Prop :=
    match out with
    | Normal value => value_shape P value result
    | Trap _ | Exit _ => True
    | _ => False
    end.

  (* Laws of supplied operations, under the platform's audited signatures.
     The source-side agreement with these signatures is Platform.Linked. *)
  Definition interface_laws (P : program_view) (interface : interface) : Prop :=
    let step := primitive interface P in
    let valid_heap := heap_ok interface in
    (forall p params result values state,
      supports interface p = true -> prim_sig p = Some (params, result) ->
      Forall2 (value_shape P) values params ->
      primitive_result P result (fst (step p values state))) /\
    (forall p params result values state,
      supports interface p = true -> prim_msig p = Some (params, result) ->
      Forall2 (value_shape P) values params ->
      primitive_result P result (fst (step p values state))) /\
    (forall code values state, supports interface (PUnsupported code) = true ->
      fst (step (PUnsupported code) values state) = UnsupportedOperation code) /\
    (forall typing weaker state,
      valid_heap typing state ->
      (forall address kind predicate,
        weaker address = Some (kind, predicate) ->
        exists original, typing address = Some (kind, original) /\
          forall value, original value -> predicate value) ->
      valid_heap weaker state) /\
    (forall state, valid_heap (fun _ => None) state) /\
    (forall p values state typing,
      supports interface p = true ->
      Heap.is_gc p = false -> valid_heap typing state ->
      valid_heap typing (snd (step p values state))) /\
    (forall p values state typing predicate,
      supports interface p = true ->
      valid_heap typing state -> Heap.gc_pre typing p values predicate ->
      match step p values state with
      | (Normal value, next) =>
          exists extended, valid_heap extended next /\
            Heap.gc_post typing extended p predicate value
      | (Trap _, next) => valid_heap typing next
      | _ => False
      end).

  Definition Compatible (program : program) (interface : interface) : Prop :=
    Platform.Ready program /\
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
    Definition run_prim P p values (machine : mstate) :=
      let '(interface, state) := machine in
      if supports interface p then
        let '(out, next) := primitive interface P p values (external state) in
        (out, (interface, World (arguments state) (read_file state) next))
      else (Stuck e_missing_primitive, machine).
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
      Elaboration.ElaboratesIn bundle world program /\ Platform.Linked world program.

  Definition Admissible (prelude : file_path) (program : program) : Prop :=
    exists source entry, Builds prelude source entry program.

  Definition Runs (program : program) (interface : interface)
      (initial : state) (result : execution) : Prop :=
    exists fuel,
      observe (Reference.run_main fuel (runtime program initial)
        (program_main program) (program_environment program) (interface, initial)) = Some result.

  (* The five public obligations remain pending. Completeness covers builds
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
End Public.

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
Module Type HOST := Semantics.HOST.
Module Public := Semantics.Public.
