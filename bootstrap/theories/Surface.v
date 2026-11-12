(* The source AST, lexical specification and desugarings shared by the
   Menhir grammar and the rest of the language specification. *)
From Stdlib Require Import String List Bool Arith.
From Stdlib Require Strings.Byte.
Import ListNotations.

(* The surface AST and the parsed bundle consumed by elaboration. *)
Module Input.
Local Open Scope string_scope.

(* ===================== The surface AST =========================== *)

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

(* An annotation can occur on a whole binding or on one of its parts. *)
Inductive binding_form : Type :=
| BindName : name -> option aty -> binding_form
| BindProduct : bool -> list (name * binding_form) -> option aty -> binding_form
| BindTag : path -> option nat -> binding_form -> option aty -> binding_form.

Inductive apat : Type :=
| APWild
| APPath : path -> apat
(* None is a match pattern. Some arity opens a single tag at a binding,
   with None arity for a named record payload. *)
| APTag  : path -> apat -> option (option nat) -> apat
| APRec  : option path -> list (name * option apat) -> apat.

Inductive aexpr : Type :=
| AEUnit | AEThis
| AEPath   : path -> sapp -> aexpr
| AECall   : path -> sapp -> list aexpr -> aexpr
| AEField  : aexpr -> name -> bool -> aexpr  (* true requires a bare product *)
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
| AEBareRecord : list (name * option aexpr) -> aexpr
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
| ADTag     : name -> list name -> option nat -> aty -> adecl
| ADBareTag : name -> adecl
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


Module Syntax.
Import Stdlib.Strings.Byte Input.
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

(* The default source position for an error after the input. *)
Definition eof_tok : token := Tok EOF "" 0.

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
(* Tuple positions use reserved labels, which source fields cannot spell.
   A nonempty tuple reuses the structural product rules and layout. *)
Definition tuple_field_name (i : nat) : name := "%" ++ show_nat i.

Definition tuple_fields {A} (items : list A) : list (name * A) :=
  combine (map tuple_field_name (seq 0 (List.length items))) items.

Definition tuple_type (items : list aty) : aty :=
  match items with [] => ATUnit | _ => ATRec (tuple_fields items) end.

Definition tuple_expr (items : list aexpr) : aexpr :=
  match items with [] => AEUnit | _ => AEBareRecord (tuple_fields (map Some items)) end.

(* A tag stores a singleton's element directly. Its declaration retains the
   arity, so a single tuple element is distinct from multiple elements. *)
Definition positional_payload_type (items : list aty) : aty :=
  match items with [t] => t | _ => tuple_type items end.

Definition tag_call_payload (arity : option nat) (args : list aexpr) : option aexpr :=
  match arity with
  | None => match args with [e] => Some e | _ => None end
  | Some n =>
      if Nat.eqb n (List.length args) then
        Some (match args with [e] => e | _ => tuple_expr args end)
      else None
  end.

Definition positional_pattern (p : path) (items : list apat) : apat :=
  match items with
  | [] => APPath p
  | [q] => APTag p q None
  | _ => APRec (Some p) (tuple_fields (map Some items))
  end.

(* D68's classes *)
Definition FRACTION := ["/"; "%"].
Definition ADDITIVE := ["+"; "-"].
Definition SHIFT := ["<<"; ">>"].
Definition COMPARISON := ["=="; "!="; "<"; ">"; "<="; ">="].
Definition BITWISE := ["&"; "^"; "|"].
Definition mem (s : string) (l : list string) : bool := existsb (String.eqb s) l.
(* ===================== Literal text and desugaring ============================= *)

(* A literal's AST text: an exponent as its value (`1E+03` is `1e3`,
   `2e-0` is `2e0`); (c). *)
Fixpoint digits_value (ds : list byte) (acc : nat) : nat :=
  match ds with
  | d :: r => digits_value r (acc * 10 + (Byte.to_nat d - 48))
  | [] => acc
  end.
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

(* Bindings translate to ordinary local bindings and field reads. Generated
   names are outside the source name alphabet. *)
Fixpoint binding_type (parameter : bool) (form : binding_form) : option aty :=
  match form with
  | BindName _ ann | BindProduct _ _ (Some _ as ann) | BindTag _ _ _ (Some _ as ann) => ann
  | BindTag tag _ _ None => if parameter then Some (ATRef tag None) else None
  | BindProduct positional fields None =>
      let fix read_fields fs :=
        match fs with
        | [] => Some []
        | (field, sub) :: rest =>
            match binding_type parameter sub, read_fields rest with
            | Some t, Some types => Some ((field, t) :: types)
            | _, _ => None
            end
        end in
      match positional, fields with
      | true, [] => Some ATUnit
      | _, _ => option_map ATRec (read_fields fields)
      end
  end.

Fixpoint unique_names (xs : list name) : bool :=
  match xs with
  | [] => true
  | x :: rest => negb (existsb (String.eqb x) rest) && unique_names rest
  end.

Fixpoint binding_names (form : binding_form) : list name :=
  match form with
  | BindName x _ => [x]
  | BindTag _ _ sub _ => binding_names sub
  | BindProduct _ fields _ =>
      (fix names fs :=
         match fs with [] => [] | (_, sub) :: rest => List.app (binding_names sub) (names rest) end) fields
  end.

Fixpoint binding_fields_valid (form : binding_form) : bool :=
  match form with
  | BindName _ _ => true
  | BindTag _ _ sub _ => binding_fields_valid sub
  | BindProduct _ fields _ =>
      unique_names (map fst fields) &&
      (fix valid fs :=
         match fs with [] => true | (_, sub) :: rest => binding_fields_valid sub && valid rest end) fields
  end.

Definition binding_valid (form : binding_form) : bool :=
  binding_fields_valid form && unique_names (binding_names form).

Definition parameter_bindings_valid (forms : list binding_form) : bool :=
  if existsb (fun form => match form with BindName _ _ => false | _ => true end) forms then
    forallb binding_fields_valid forms && unique_names (flat_map binding_names forms)
  else true.

(* An abstract signature has no body in which to check a projection
   against a second annotation. Use one source of type information along
   each path of its binding form. *)
Definition binding_annotation (form : binding_form) : option aty :=
  match form with BindName _ ann | BindProduct _ _ ann | BindTag _ _ _ ann => ann end.

Fixpoint abstract_binding_valid (covered : bool) (form : binding_form) : bool :=
  let ann := binding_annotation form in
  let annotated := match ann with Some _ => true | None => false end in
  negb (covered && annotated) &&
  match form with
  | BindName _ _ => true
  | BindTag _ _ _ _ => false
  | BindProduct _ fields _ =>
      (fix valid fs :=
         match fs with
         | [] => true
         | (_, sub) :: rest => abstract_binding_valid (covered || annotated) sub && valid rest
         end) fields
  end.

Definition parameter_name (i : nat) (form : binding_form) : name :=
  match form with
  | BindName x _ => x
  | _ => "%parameter" ++ show_nat i
  end.

(* A field with its own annotation agrees exactly with the containing
   product's field type. Synthesize the singleton record before checking it
   so the check cannot inject its field into a larger union. *)
Definition checked_field (ann : option aty) (field : aexpr) : aexpr :=
  match ann with
  | None => field
  | Some t =>
      let key := "%field" in
      let actual := AEBlock (ABlock [ASLet key None (AEBareRecord [(key, Some field)])] (Some (ref key))) in
      AEBlock (ABlock [ASLet key (Some (ATRec [(key, t)])) actual] (Some (AEField (ref key) key true)))
  end.

(* A positional pattern accounts for the entire tuple. Assigning a tuple
   rebuilt from the named positions checks both its arity and its kind. *)
Definition check_product_shape (positional : bool) (root : name) (fields : list (name * binding_form)) : list astmt :=
  let check := "%shape" in
  let rebuilt :=
    match positional, fields with
    | true, [] => AEUnit
    | _, _ => AEBareRecord (map (fun '(field, _) => (field, Some (AEField (ref root) field true))) fields)
    end in
  [ASVar check None (ref root); ASAssign check rebuilt].

Definition bind_component (mutable : bool) (slot : name) (form : binding_form) (value : aexpr) : astmt :=
  let ann := binding_annotation form in
  let value := checked_field ann value in
  match form with
  | BindName x _ => (if mutable then ASVar else ASLet) x ann value
  | _ => ASLet slot ann value
  end.

Fixpoint bind_components (mutable : bool) (root : name) (form : binding_form) : list astmt :=
  match form with
  | BindName _ _ => []
  | BindTag tag arity payload _ =>
      let slot := root ++ "%payload" in
      let binder := "%opened" in
      let value := AEMatch (ref root) [(APTag tag (APPath [binder]) (Some arity), ref binder)] in
      bind_component mutable slot payload value ::
      match payload with BindProduct false [] _ => [] | _ => bind_components mutable slot payload end
  | BindProduct positional fields _ =>
      let fix bind_fields fs :=
        match fs with
        | [] => []
        | (field, sub) :: rest =>
            let child := root ++ "%" ++ field in
            let value := AEField (ref root) field true in
            bind_component mutable child sub value :: List.app (bind_components mutable child sub) (bind_fields rest)
        end in
      let check := if positional || match fields with [] => true | _ => false end
                   then check_product_shape positional root fields else [] in
      List.app check (bind_fields fields)
  end.

Definition destructure (mutable : bool) (form : binding_form) (value : aexpr) : list astmt :=
  match form with
  | BindName x ann => [(if mutable then ASVar else ASLet) x ann value]
  | _ =>
      let root := "%binding" in
      ASLet root (binding_type false form) value :: bind_components mutable root form
  end.

Fixpoint parameter_signature (i : nat) (forms : list binding_form) : option (list (name * aty)) :=
  match forms with
  | [] => Some []
  | form :: rest =>
      match binding_type true form, parameter_signature (S i) rest with
      | Some t, Some ps => Some ((parameter_name i form, t) :: ps)
      | _, _ => None
      end
  end.

Fixpoint lambda_signature (i : nat) (forms : list binding_form) : list (name * option aty) :=
  match forms with
  | [] => []
  | form :: rest => (parameter_name i form, binding_type false form) :: lambda_signature (S i) rest
  end.

Fixpoint parameter_bindings (i : nat) (forms : list binding_form) : list astmt :=
  match forms with
  | [] => []
  | form :: rest => List.app (bind_components false (parameter_name i form) form) (parameter_bindings (S i) rest)
  end.

Definition bind_fn_parameters (forms : list binding_form) (body : ablock) : ablock :=
  let 'ABlock ss tail := body in ABlock (List.app (parameter_bindings 0 forms) ss) tail.

Definition bind_lambda_parameters (forms : list binding_form) (body : aexpr) : aexpr :=
  match parameter_bindings 0 forms with [] => body | ss => AEBlock (ABlock ss (Some body)) end.

Definition tuple_binding (forms : list binding_form) (ann : option aty) : binding_form :=
  BindProduct true (tuple_fields forms) ann.

Definition annotate_binding (form : binding_form) (ann : option aty) : option binding_form :=
  match ann with
  | None => Some form
  | Some _ =>
      match form with
      | BindName x None => Some (BindName x ann)
      | BindProduct positional fields None => Some (BindProduct positional fields ann)
      | BindTag tag arity payload None => Some (BindTag tag arity payload ann)
      | _ => None
      end
  end.

Definition positional_binding (tag : path) (forms : list binding_form) (ann : option aty) : binding_form :=
  let payload := match forms with [one] => one | _ => tuple_binding forms None end in
  BindTag tag (Some (List.length forms)) payload ann.

End Syntax.
