(* ======================================================================== *)
(* Parse.v — Moss's front end in Gallina (m4): lexer, parser, the dump, and *)
(* the bundle loader.  It replaces the Python parser (bootstrap/mossc/      *)
(* lex.py, parse.py, sexpr.py) and py/bundle.py in the oracle's pipeline:   *)
(*                                                                           *)
(*   source text --lex--> tokens --parse--> Audit.Input.afile --dump--> sx  *)
(*                                                                           *)
(* `dump_file` yields exactly the s-expression that sexpr.py writes for the *)
(* same file (and `print_sx` its text, byte for byte; see README, Part 1),  *)
(* so the elaborator (`Elab.elaborate_sx`) is fed what it was fed before.   *)
(* `load_bundle` is py/bundle.py: it walks the imports in the same order,   *)
(* resolves them as collect.Loader does (D64), and reads files through an   *)
(* oracle `read`, the only effect, which the OCaml driver supplies.         *)
(*                                                                           *)
(* Everything is total: the lexer and parser run on fuel computed from the  *)
(* input's length, and running out is reported (it never happens on the    *)
(* corpus).  Error messages are lex.py's and parse.py's, with their class   *)
(* (`LexError`/`ParseError`) and `line:col` as `error_message` prints them. *)
(* ======================================================================== *)

From Stdlib Require Import String Ascii List Bool Arith.
From Stdlib Require Import Strings.Byte.
From NM Require Import Audit DumpRoundTrip.
Import ListNotations.
Import Audit.Input Audit.Syntax Audit.Dump Audit.Loader.
Local Open Scope string_scope.

(* ######################################################################## *)
(* Lexer (lex.py)                                                            *)
(* ######################################################################## *)

(* Tokens, character classes, the keyword and symbol tables, and the token
   readers `peek`/`kind`/`advance` are Audit.Syntax's: the specification
   is stated in their terms. *)

(* Python's repr of a one-character string (ASCII; a byte >= 0x80 is shown
   as \xNN, where Python would show the decoded character). *)
Definition repr_char (b : byte) : string :=
  match b with
  | "'"%byte => """'"""
  | "\"%byte => "'\\'"
  | x09 => "'\t'" | x0a => "'\n'" | x0d => "'\r'"
  | _ => let n := Byte.to_nat b in
         if Nat.leb 32 n && Nat.leb n 126 then "'" ++ string_of_list_byte [b] ++ "'"
         else "'\x" ++ hexdig (n / 16) ++ hexdig (n mod 16) ++ "'"
  end.

(* A byte >= 0xC2 that starts a UTF-8 sequence is shown with its sequence,
   as Python shows the decoded (printable) character. *)
Definition repr_char_at (c : byte) (r : list byte) : string :=
  let n := Byte.to_nat c in
  let cont := fun b => let m := Byte.to_nat b in Nat.leb 128 m && Nat.ltb m 192 in
  let take k := firstn k (fst (span cont r)) in
  let k := if Nat.leb 240 n then 3 else if Nat.leb 224 n then 2 else if Nat.leb 194 n then 1 else 0 in
  if Nat.ltb 0 k && Nat.eqb (length (take k)) k then "'" ++ string_of_list_byte (c :: take k) ++ "'"
  else repr_char c.

Inductive lres := LOk (ts : list token) | LErr (msg : string) (off : nat).

Definition MAX_EXPONENT_DIGITS := 3.  (* MAX_EXPONENT = 999 *)


(* A string literal's body, from just after the opening quote at [i]:
   the bytes up to the closing quote, and what follows. *)
Fixpoint lex_string (i j : nat) (cs : list byte) (acc : list byte)
  : (list byte * list byte) + (string * nat) :=
  match cs with
  | [] => inr ("unclosed string", i)
  | x0a :: _ => inr ("unclosed string", i)
  | """"%byte :: r => inl (rev' acc, r)
  | "\"%byte :: d :: r => if is_escape d then lex_string i (S (S j)) r (d :: "\"%byte :: acc)
                     else inr ("invalid escape sequence", j)
  | "\"%byte :: [] => inr ("invalid escape sequence", j)
  | c :: r => lex_string i (S j) r (c :: acc)
  end.

(* A numeric literal starting with digit [c] at offset [i] (D68). *)
Definition lex_number (c : byte) (r : list byte) (i : nat)
  : (token * list byte * nat) + (string * nat) :=
  let (ds, r1) := span is_digit r in
  let int_text := c :: ds in
  let '(kind, frac, r2) :=
    match r1 with
    | "."%byte :: d :: r' =>
        if is_digit d then let (ds2, r'') := span is_digit r' in ("DECIMAL", "."%byte :: d :: ds2, r'')
        else ("INTEGER", [], r1)
    | _ => ("INTEGER", [], r1)
    end in
  let j := i + length int_text + length frac in        (* offset of r2 *)
  let body := app int_text frac in
  let exp :=
    match r2 with
    | e :: s :: r3 =>
        if is_e e && is_sign s then
          match r3 with
          | d :: _ => if is_digit d then inl (Some ([e; s], r3)) else inr ("an exponent needs at least one digit (D68)", j + 2)
          | [] => inr ("an exponent needs at least one digit (D68)", j + 2)
          end
        else if is_e e && is_digit s then inl (Some ([e], s :: r3))
        else inl None
    | _ => inl None
    end in
  match exp with
  | inr err => inr err
  | inl None =>
      match r2 with
      | d :: _ => if is_name_cont d then inr ("a numeric literal has no suffix; separate it from the name (D68)", j)
                  else inl (Tok kind (string_of_list_byte body) i, r2, j)
      | [] => inl (Tok kind (string_of_list_byte body) i, r2, j)
      end
  | inl (Some (pre, r3)) =>
      let start := j + length pre in
      let (eds, r4) := span is_digit r3 in
      if Nat.ltb MAX_EXPONENT_DIGITS (length (strip_zeros eds)) then inr ("an exponent is at most 999 (D68)", start)
      else
        let k := start + length eds in
        let text := app body (app pre eds) in
        match r4 with
        | d :: _ => if is_name_cont d then inr ("a numeric literal has no suffix; separate it from the name (D68)", k)
                    else inl (Tok "DECIMAL" (string_of_list_byte text) i, r4, k)
        | [] => inl (Tok "DECIMAL" (string_of_list_byte text) i, r4, k)
        end
  end.

(* One token's worth of input: what is left, where it starts, and the
   tokens so far; or an error.  It never ends the list: only `lex_go` adds
   EOF, at the end of the input (lex_ends_eof). *)
Definition lex_char (c : byte) (r : list byte) (i : nat) (acc : list token)
  : (list byte * nat * list token) + (string * nat) :=
  if is_ws c then inl (r, S i, acc)
  else if Byte.eqb c "#"%byte then
    let (com, rest) := span (fun b => negb (Byte.eqb b x0a)) r in
    inl (rest, S i + length com, acc)
  else
  match (match r with d :: r' => match two_char c d with Some k => Some (k, d, r') | None => None end
                      | [] => None end) with
  | Some (k, d, r') => inl (r', S (S i), Tok k (string_of_list_byte [c; d]) i :: acc)
  | None =>
  match one_char c with
  | Some k => inl (r, S i, Tok k (string_of_list_byte [c]) i :: acc)
  | None =>
    if is_digit c then
      match lex_number c r i with
      | inl (t, r', j) => inl (r', j, t :: acc)
      | inr (m, o) => inr (m, o)
      end
    else if is_name_start c then
      let (ns, r') := span is_name_cont r in
      let text := string_of_list_byte (c :: ns) in
      let k := match assoc text KEYWORDS with Some k => k | None => "NAME" end in
      inl (r', S i + length ns, Tok k text i :: acc)
    else if Byte.eqb c """"%byte then
      match lex_string i (S i) r [] with
      | inl (body, r') =>
          inl (r', S i + length body + 1, Tok "STRING" (string_of_list_byte (c :: app body [c])) i :: acc)
      | inr (m, o) => inr (m, o)
      end
    else inr ("unexpected character " ++ repr_char_at c r, i)
  end end.

Fixpoint lex_go (fuel : nat) (cs : list byte) (i : nat) (acc : list token) : lres :=
  match fuel with
  | 0 => LErr "lexer out of fuel" i
  | S fuel =>
      match cs with
      | [] => LOk (rev' (Tok "EOF" "" i :: acc))
      | c :: r =>
          match lex_char c r i acc with
          | inl (r', i', acc') => lex_go fuel r' i' acc'
          | inr (m, o) => LErr m o
          end
      end
  end.

Definition lex (cs : list byte) : lres := lex_go (S (length cs)) cs 0 [].


(* lex.position: 1-based line and column of a byte offset. *)
Fixpoint line_col (cs : list byte) (off line col : nat) : nat * nat :=
  match off, cs with
  | 0, _ | _, [] => (line, col)
  | S o, c :: r => if Byte.eqb c x0a then line_col r o (S line) 1 else line_col r o line (S col)
  end.


(* ######################################################################## *)
(* Parser (parse.py), producing Audit.Input's surface AST                    *)
(* ######################################################################## *)

Inductive pr (A : Type) : Type :=
| POk : A -> list token -> pr A
| PErr : string -> nat -> pr A.
Arguments POk {A}.
Arguments PErr {A}.

Definition pbind {A B} (r : pr A) (k : A -> list token -> pr B) : pr B :=
  match r with POk a ts => k a ts | PErr m o => PErr m o end.

Notation "'let@' x '&' ts ':=' r 'in' k" := (pbind r (fun x ts => k))
  (at level 200, x pattern, ts name, r at level 100, k at level 200).

Definition at_ (k : string) (ts : list token) : bool := String.eqb (kind ts) k.
Definition at_any (ks : list string) (ts : list token) : bool := existsb (String.eqb (kind ts)) ks.
Definition err {A} (msg : string) (ts : list token) : pr A := PErr msg (toff (peek ts)).
Definition fuel_err {A} (ts : list token) : pr A := err "parser out of fuel" ts.
Definition expect (k : string) (ts : list token) : pr token :=
  if at_ k ts then POk (peek ts) (advance ts)
  else err ("expected " ++ k ++ ", found " ++ kind ts) ts.
Definition expect_name (ts : list token) : pr string :=
  let@ t & ts := expect "NAME" ts in POk (tx t) ts.
Definition eat (k : string) (ts : list token) : bool * list token :=
  if at_ k ts then (true, advance ts) else (false, ts).

(* List[X]: comma-separated, optional trailing comma (Parser.comma_list) *)
Fixpoint comma_list {A} (n : nat) (item : list token -> pr A) (closers : list string)
  (ts : list token) : pr (list A) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_any closers ts then POk [] ts else
      let@ x & ts := item ts in
      if at_ "COMMA" ts then
        let@ xs & ts := comma_list n item closers (advance ts) in POk (x :: xs) ts
      else POk [x] ts
  end.

(* ===================== Names and applications ============================= *)

Fixpoint path_rest (n : nat) (acc : list string) (ts : list token) : pr path :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "COLON_COLON" ts then
        let@ x & ts := expect_name (advance ts) in path_rest n (x :: acc) ts
      else POk (rev' acc) ts
  end.

Definition parse_path (n : nat) (ts : list token) : pr path :=
  let@ x & ts := expect_name ts in path_rest n [x] ts.

Fixpoint parse_spec (n : nat) (ts : list token) : pr spec :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ (p, d) & ts :=
        (if at_ "DOT" ts then
           let@ d & ts := expect_name (advance ts) in POk (None, Some d) ts
         else
           let@ p & ts := parse_path n ts in
           if at_ "DOT" ts then
             let@ d & ts := expect_name (advance ts) in POk (Some p, Some d) ts
           else POk (Some p, None) ts) in
      let@ a & ts := parse_app_opt n ts in
      POk (Spec p d a) ts
  end
with parse_app_opt (n : nat) (ts : list token) : pr sapp :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "LBRACKET" ts then
        let@ _ & ts := expect "LBRACKET" ts in
        let@ bs & ts := comma_list n (parse_binding n) ["RBRACKET"] ts in
        let@ _ & ts := expect "RBRACKET" ts in
        POk (Some bs) ts
      else POk None ts
  end
with parse_binding (n : nat) (ts : list token) : pr (path * spec) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ p & ts := parse_path n ts in
      let@ _ & ts := expect "EQUAL" ts in
      let@ s & ts := parse_spec n ts in
      POk (p, s) ts
  end.

Definition parse_assume_item (n : nat) (ts : list token) : pr (path * option name) :=
  let@ p & ts := parse_path n ts in
  if at_ "DOT" ts then let@ d & ts := expect_name (advance ts) in POk (p, Some d) ts
  else POk (p, None) ts.

(* ===================== Types ============================================== *)

Definition at_type_atom (ts : list token) : bool :=
  at_any ["NAME"; "THIS_TYPE"; "LPAREN"; "LBRACE"] ts.

Fixpoint parse_type (n : nat) (ts : list token) : pr aty :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let (piped, ts) := eat "PIPE" ts in
      if piped && negb (at_type_atom ts) then POk ATNever ts else
      let@ m & ts := parse_type_atom n ts in
      let@ ms & ts := union_rest n ts in
      match ms with [] => POk m ts | _ => POk (ATUnion (m :: ms)) ts end
  end
with union_rest (n : nat) (ts : list token) : pr (list aty) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "PIPE" ts then
        let@ m & ts := parse_type_atom n (advance ts) in
        let@ ms & ts := union_rest n ts in POk (m :: ms) ts
      else POk [] ts
  end
with parse_type_atom (n : nat) (ts : list token) : pr aty :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "NAME" ts then
        let@ p & ts := parse_path n ts in
        let@ a & ts := parse_app_opt n ts in POk (ATRef p a) ts
      else if at_ "THIS_TYPE" ts then POk ATThis (advance ts)
      else if at_ "LPAREN" ts then
        let ts := advance ts in
        if at_ "RPAREN" ts then POk ATUnit (advance ts) else
        let@ t & ts := parse_type n ts in
        if at_ "COMMA" ts then err "there are no tuple types (D63); use a record" ts else
        let@ _ & ts := expect "RPAREN" ts in POk t ts
      else if at_ "LBRACE" ts then parse_record_type n ts
      else err ("expected a type, found " ++ kind ts) ts
  end
with parse_record_type (n : nat) (ts : list token) : pr aty :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "LBRACE" ts in
      let@ fs & ts := comma_list n (type_field n) ["RBRACE"] ts in
      let@ _ & ts := expect "RBRACE" ts in
      POk (ATRec fs) ts
  end
with type_field (n : nat) (ts : list token) : pr (name * aty) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ x & ts := expect_name ts in
      let@ _ & ts := expect "COLON" ts in
      let@ t & ts := parse_type n ts in POk (x, t) ts
  end.

Definition parse_opt_type (n : nat) (ts : list token) : pr (option aty) :=
  let (b, ts) := eat "COLON" ts in
  if b then let@ t & ts := parse_type n ts in POk (Some t) ts else POk None ts.

(* ===================== Patterns =========================================== *)

Fixpoint parse_pattern (n : nat) (ts : list token) : pr apat :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "LBRACE" ts then parse_record_pattern n None ts
      else if at_ "UNDERSCORE" ts then POk APWild (advance ts)
      else if at_ "NAME" ts then
        let@ p & ts := parse_path n ts in
        if at_ "LBRACE" ts then parse_record_pattern n (Some p) ts
        else if at_ "LPAREN" ts then
          let@ sub & ts := parse_pattern n (advance ts) in
          let@ _ & ts := expect "RPAREN" ts in POk (APTag p sub) ts
        else POk (APPath p) ts
      else err ("expected a pattern, found " ++ kind ts) ts
  end
with parse_record_pattern (n : nat) (p : option path) (ts : list token) : pr apat :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "LBRACE" ts in
      let@ fs & ts := comma_list n (pat_field n) ["RBRACE"] ts in
      let@ _ & ts := expect "RBRACE" ts in
      POk (APRec p fs) ts
  end
with pat_field (n : nat) (ts : list token) : pr (name * option apat) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ x & ts := expect_name ts in
      if at_ "EQUAL" ts then
        let@ q & ts := parse_pattern n (advance ts) in POk (x, Some q) ts
      else POk (x, None) ts
  end.

(* ===================== Operators (D68) ==================================== *)

Definition unary_op (k : string) : option string :=
  if String.eqb k "HYPHEN" then Some "-" else if String.eqb k "EXCLAM" then Some "!" else None.

(* Parser.mixed: two operators that may not meet *)
Definition mixed {A} (before : option string) (ts : list token) : pr A :=
  let after := match binop ts with Some a => a | None => "" end in
  let m := match before with
           | Some b =>
               if mem b COMPARISON && mem after COMPARISON then "comparisons do not chain"
               else if mem b SHIFT && mem after SHIFT then "shifts do not chain"
               else if String.eqb b after then "`" ++ after ++ "` does not chain here"
               else "`" ++ b ++ "` and `" ++ after ++ "` are not ordered"
           | None => "`None` and `" ++ after ++ "` are not ordered"
           end in
  err (m ++ "; add parentheses (D68)") ts.

(* ===================== Expressions, statements, blocks ==================== *)

(* A literal's text is normalized by Audit.Syntax.lit_norm, and `for` is
   desugared by Audit.Syntax.for_desugar (D85). *)

Fixpoint parse_expr (n : nat) (nr : bool) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "IF" ts then parse_if n ts
      else if at_ "MATCH" ts then
        let@ s & ts := parse_expr n true (advance ts) in
        let@ _ & ts := expect "LBRACE" ts in
        let@ arms & ts := parse_arms n ts in
        let@ _ & ts := expect "RBRACE" ts in
        POk (AEMatch s arms) ts
      else if at_ "RETURN" ts then
        let ts := advance ts in
        if at_any ["SEMI"; "COMMA"; "RBRACE"; "RPAREN"] ts then POk (AEReturn None) ts
        else let@ e & ts := parse_expr n nr ts in POk (AEReturn (Some e)) ts
      else if at_ "BREAK" ts then POk AEBreak (advance ts)
      else parse_operators n nr ts
  end
with parse_if (n : nat) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "IF" ts in
      let@ c & ts := parse_expr n true ts in
      let@ t & ts := parse_block n ts in
      let (el, ts) := eat "ELSE" ts in
      if el then
        if at_ "IF" ts then let@ e & ts := parse_if n ts in POk (AEIf c t (Some e)) ts
        else let@ b & ts := parse_block n ts in POk (AEIf c t (Some (AEBlock b))) ts
      else POk (AEIf c t None) ts
  end
with parse_arms (n : nat) (ts : list token) : pr (list (apat * aexpr)) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "RBRACE" ts then POk [] ts else
      let@ p & ts := parse_pattern n ts in
      let@ _ & ts := expect "EQUAL_GREATER" ts in
      let@ body & ts :=
        (if at_ "LBRACE" ts then
           let@ b & ts := parse_block n ts in POk (AEBlock b) (snd (eat "COMMA" ts))
         else
           let@ e & ts := parse_expr n false ts in
           if at_ "RBRACE" ts then POk e ts
           else let@ _ & ts := expect "COMMA" ts in POk e ts) in
      let@ rest & ts := parse_arms n ts in
      POk ((p, body) :: rest) ts
  end
with parse_operators (n : nat) (nr : bool) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ first & ts := parse_unary n nr ts in
      match binop ts with
      | None => POk first ts
      | Some op =>
          if mem op BITWISE then
            let@ e & ts := bit_loop n nr op first ts in
            match binop ts with Some _ => mixed (Some op) ts | None => POk e ts end
          else
            let@ (lhs, last) & ts := parse_operand n nr first ts in
            match binop ts with
            | None => POk lhs ts
            | Some o2 =>
                if mem o2 COMPARISON then
                  let@ u & ts := parse_unary n nr (advance ts) in
                  let@ (rhs, last2) & ts := parse_operand n nr u ts in
                  match binop ts with
                  | Some _ => mixed (match last2 with None => Some o2 | l => l end) ts
                  | None => POk (AEBinary o2 lhs rhs) ts
                  end
                else mixed last ts
            end
      end
  end
with bit_loop (n : nat) (nr : bool) (op : string) (e : aexpr) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      match binop ts with
      | Some o => if String.eqb o op then
                    let@ u & ts := parse_unary n nr (advance ts) in
                    bit_loop n nr op (AEBinary op e u) ts
                  else POk e ts
      | None => POk e ts
      end
  end
(* a comparison's operand: a shift, or an arithmetic expression; with the
   last operator it ended on *)
with parse_operand (n : nat) (nr : bool) (first : aexpr) (ts : list token)
  : pr (aexpr * option string) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if binop_in SHIFT ts then
        let op := match binop ts with Some o => o | None => "" end in
        let@ u & ts := parse_unary n nr (advance ts) in POk (AEBinary op first u, Some op) ts
      else
        let@ (e, last) & ts := parse_term n nr first ts in
        let@ (e, last) & ts := add_loop n nr e last ts in
        match last with
        | Some _ => if binop_in SHIFT ts then mixed last ts else POk (e, last) ts
        | None => POk (e, last) ts
        end
  end
with add_loop (n : nat) (nr : bool) (e : aexpr) (last : option string) (ts : list token)
  : pr (aexpr * option string) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      match binop ts with
      | Some op =>
          if mem op ADDITIVE then
            let@ u & ts := parse_unary n nr (advance ts) in
            let@ (rhs, _) & ts := parse_term n nr u ts in
            add_loop n nr (AEBinary op e rhs) (Some op) ts
          else POk (e, last) ts
      | None => POk (e, last) ts
      end
  end
with parse_term (n : nat) (nr : bool) (first : aexpr) (ts : list token)
  : pr (aexpr * option string) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ (e, last) & ts := mul_loop n nr first None ts in
      match binop ts with
      | Some op =>
          if mem op FRACTION then
            let@ u & ts := parse_unary n nr (advance ts) in
            if binop_in ("*" :: FRACTION) ts then mixed (Some op) ts
            else POk (AEBinary op e u, Some op) ts
          else POk (e, last) ts
      | None => POk (e, last) ts
      end
  end
with mul_loop (n : nat) (nr : bool) (e : aexpr) (last : option string) (ts : list token)
  : pr (aexpr * option string) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if binop_in ["*"] ts then
        let@ u & ts := parse_unary n nr (advance ts) in
        mul_loop n nr (AEBinary "*" e u) (Some "*") ts
      else POk (e, last) ts
  end
with parse_unary (n : nat) (nr : bool) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      match unary_op (kind ts) with
      | None => parse_postfix n nr ts
      | Some op =>
          let ts := advance ts in
          if String.eqb op "-" && at_any ["INTEGER"; "DECIMAL"] ts && String.eqb (kind_at 1 ts) "DOT" then
            let text := tx (peek ts) in
            let nm := if String.eqb (kind_at 2 ts) "NAME" then tx (peekn 2 ts) else "m" in
            err ("`-` before a literal with a method call: write `(-" ++ text ++ ")." ++ nm
                 ++ "(...)` or `-(" ++ text ++ "." ++ nm ++ "(...))` (D68)") ts
          else let@ u & ts := parse_unary n nr ts in POk (AEUnary op u) ts
      end
  end
with parse_postfix (n : nat) (nr : bool) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ e & ts := parse_primary n nr ts in postfix_loop n e ts
  end
with postfix_loop (n : nat) (e : aexpr) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "DOT" ts then
        let@ p & ts := parse_path n (advance ts) in
        if at_ "LPAREN" ts then
          let@ args & ts := comma_list n (parse_expr n false) ["RPAREN"] (advance ts) in
          let@ _ & ts := expect "RPAREN" ts in
          postfix_loop n (AEMeth e p args) ts
        else match p with
             | [f] => postfix_loop n (AEField e f) ts
             | _ => err "field access takes a single name" ts
             end
      else POk e ts
  end
with parse_lambda (n : nat) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "LPAREN" ts in
      let@ ps & ts := comma_list n (fun ts => let@ x & ts := expect_name ts in
                                              let@ t & ts := parse_opt_type n ts in
                                              POk (x, t) ts) ["RPAREN"] ts in
      let@ _ & ts := expect "RPAREN" ts in
      let@ ret & ts := parse_opt_type n ts in
      let@ _ & ts := expect "EQUAL_GREATER" ts in
      let@ body & ts := parse_expr n false ts in
      POk (AELam ps ret body) ts
  end
with parse_primary (n : nat) (nr : bool) (ts : list token) : pr aexpr :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "LPAREN" ts && at_lambda ts then parse_lambda n ts
      else if at_ "LPAREN" ts then
        let ts := advance ts in
        if at_ "RPAREN" ts then POk AEUnit (advance ts) else
        let@ e & ts := parse_expr n false ts in
        let@ _ & ts := expect "RPAREN" ts in POk e ts
      else if at_ "THIS" ts then POk AEThis (advance ts)
      else if at_any ["INTEGER"; "DECIMAL"] ts then POk (AELit (lit_norm (tx (peek ts)))) (advance ts)
      else if at_ "STRING" ts then
        let value := string_value (tx (peek ts)) in
        if string_utf8_valid value then POk (string_desugar value) (advance ts)
        else err "invalid UTF-8 in string literal" ts
      else if at_ "LBRACE" ts && negb nr then
        let@ b & ts := parse_block n ts in POk (AEBlock b) ts
      else if at_ "NAME" ts then
        let@ p & ts := parse_path n ts in
        let@ a & ts := parse_app_opt n ts in
        if at_ "LPAREN" ts then
          let@ args & ts := comma_list n (parse_expr n false) ["RPAREN"] (advance ts) in
          let@ _ & ts := expect "RPAREN" ts in
          POk (AECall p a args) ts
        else if at_ "LBRACE" ts && negb nr then
          let@ fs & ts := comma_list n (fun ts => let@ x & ts := expect_name ts in
                                                  if at_ "EQUAL" ts then
                                                    let@ v & ts := parse_expr n false (advance ts) in
                                                    POk (x, Some v) ts
                                                  else POk (x, None) ts) ["RBRACE"] (advance ts) in
          let@ _ & ts := expect "RBRACE" ts in
          POk (AERec p a fs) ts
        else POk (AEPath p a) ts
      else err ("expected an expression, found " ++ kind ts) ts
  end
with parse_block (n : nat) (ts : list token) : pr ablock :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "LBRACE" ts in
      let@ (ss, tl) & ts := block_loop n ts in
      let@ _ & ts := expect "RBRACE" ts in
      POk (ABlock ss tl) ts
  end
with block_loop (n : nat) (ts : list token) : pr (list astmt * option aexpr) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let more (s : astmt) (ts : list token) :=
        let@ (ss, tl) & ts := block_loop n ts in POk (s :: ss, tl) ts in
      if at_ "RBRACE" ts then POk ([], None) ts
      else if at_ "LET" ts || at_ "VAR" ts then
        let is_let := at_ "LET" ts in
        let@ x & ts := expect_name (advance ts) in
        let@ t & ts := parse_opt_type n ts in
        let@ _ & ts := expect "EQUAL" ts in
        let@ e & ts := parse_expr n false ts in
        let@ _ & ts := expect "SEMI" ts in
        more ((if is_let then ASLet else ASVar) x t e) ts
      else if at_ "BIND" ts then
        let@ b & ts := parse_bind n ts in more (ASBind b) ts
      else if at_ "WHILE" ts then
        let@ c & ts := parse_expr n true (advance ts) in
        let@ b & ts := parse_block n ts in more (ASWhile c b) ts
      else if at_ "LOOP" ts then
        let@ b & ts := parse_block n (advance ts) in more (ASLoop b) ts
      else if at_ "FOR" ts then
        let ts := advance ts in
        let@ x & ts := expect_name ts in
        let@ _ & ts := expect "IN" ts in
        let@ coll & ts := parse_expr n true ts in
        let@ body & ts := parse_block n ts in
        more (for_desugar x coll body) ts
      else if at_ "NAME" ts && String.eqb (kind_at 1 ts) "EQUAL" then
        let x := tx (peek ts) in
        let@ e & ts := parse_expr n false (advance (advance ts)) in
        let@ _ & ts := expect "SEMI" ts in
        more (ASAssign x e) ts
      else
        let braced := at_any ["IF"; "MATCH"; "LBRACE"] ts in
        let@ e & ts := parse_expr n false ts in
        if at_ "SEMI" ts then more (ASExpr e) (advance ts)
        else if at_ "RBRACE" ts then POk ([], Some e) ts
        else if braced then more (ASExpr e) ts
        else err ("expected SEMI or RBRACE, found " ++ kind ts) ts
  end
with parse_bind (n : nat) (ts : list token) : pr (list (spec * option aexpr)) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      let@ _ & ts := expect "BIND" ts in
      let@ its & ts := comma_list n (fun ts => let@ s & ts := parse_spec n ts in
                                               if at_ "EQUAL" ts then
                                                 let@ e & ts := parse_expr n false (advance ts) in
                                                 POk (s, Some e) ts
                                               else POk (s, None) ts) ["SEMI"] ts in
      let@ _ & ts := expect "SEMI" ts in
      POk its ts
  end.

(* ===================== Declarations and files ============================= *)

Definition name_list (n : nat) (ts : list token) : pr (list name) :=
  let@ _ & ts := expect "LBRACKET" ts in
  let@ ps & ts := comma_list n expect_name ["RBRACKET"] ts in
  let@ _ & ts := expect "RBRACKET" ts in
  POk ps ts.

Fixpoint binds_loop (n : nat) (ts : list token) : pr (list (list (spec * option aexpr))) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "RBRACE" ts then POk [] ts else
      let@ b & ts := parse_bind n ts in
      let@ bs & ts := binds_loop n ts in POk (b :: bs) ts
  end.

Fixpoint parse_decl (n : nat) (ts : list token) : pr adecl :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "ASSUME" ts then
        let@ its & ts := comma_list n (parse_assume_item n) ["LBRACE"] (advance ts) in
        let@ _ & ts := expect "LBRACE" ts in
        let@ ds & ts := decls_until n ts in
        let@ _ & ts := expect "RBRACE" ts in
        POk (ADAssume its ds) ts
      else if at_ "TYPE" ts then
        let@ x & ts := expect_name (advance ts) in
        if at_ "SEMI" ts then POk (ADType x) (advance ts)
        else if at_ "EQUAL" ts then
          let@ t & ts := parse_type n (advance ts) in
          let@ _ & ts := expect "SEMI" ts in POk (ADAlias x t) ts
        else err ("`type " ++ x ++ "` is abstract (`;`) or an alias (`=`); declare a nominal type with `tag` (D83)") ts
      else if at_ "TAG" ts then
        let@ x & ts := expect_name (advance ts) in
        let bracketed := at_ "LBRACKET" ts in
        let@ ps & ts := (if bracketed then name_list n ts else POk [] ts) in
        if at_ "SEMI" ts then
          (if bracketed then err ("a tag with listed parameters has a payload; a unit tag is `tag " ++ x ++ ";` (D73)") ts
           else POk (ADUnit x) (advance ts)) else
        let@ t & ts :=
          (if at_ "LBRACE" ts then parse_record_type n ts
           else
             let@ _ & ts := expect "LPAREN" ts in
             let at0 := ts in
             let@ t & ts := parse_type n ts in
             match t with
             | ATRec _ => err ("a record payload is written `tag " ++ x ++ " { ... };` (D83)") at0
             | _ =>
                 if at_ "COMMA" ts then err "there are no tuple types (D63); use a record" ts else
                 let@ _ & ts := expect "RPAREN" ts in POk t ts
             end) in
        let@ _ & ts := expect "SEMI" ts in
        POk (ADTag x ps t) ts
      else if at_ "VAL" ts then
        let@ x & ts := expect_name (advance ts) in
        let@ _ & ts := expect "COLON" ts in
        let@ t & ts := parse_type n ts in
        let@ i & ts := (if at_ "EQUAL" ts then
                          let@ e & ts := parse_expr n false (advance ts) in POk (Some e) ts
                        else POk None ts) in
        let@ _ & ts := expect "SEMI" ts in
        POk (ADVal x t i) ts
      else if at_ "FN" ts then
        let ts := advance ts in
        let@ (rcv, x) & ts :=
          (if at_ "DOT" ts then let@ x & ts := expect_name (advance ts) in POk (RDetached, x) ts
           else
             let@ x & ts := expect_name ts in
             if at_ "DOT" ts then let@ y & ts := expect_name (advance ts) in POk (RAttached x, y) ts
             else POk (RPlain, x) ts) in
        let@ _ & ts := expect "LPAREN" ts in
        let@ ps & ts := comma_list n (fun ts => let@ p & ts := expect_name ts in
                                                let@ _ & ts := expect "COLON" ts in
                                                let@ t & ts := parse_type n ts in
                                                POk (p, t) ts) ["RPAREN"] ts in
        let@ _ & ts := expect "RPAREN" ts in
        let@ ret & ts := parse_opt_type n ts in
        if at_ "SEMI" ts then POk (ADFn rcv x ps ret None) (advance ts)
        else let@ b & ts := parse_block n ts in POk (ADFn rcv x ps ret (Some b)) ts
      else if at_ "CONTEXT" ts then
        let@ x & ts := expect_name (advance ts) in
        let@ _ & ts := expect "EQUAL" ts in
        let@ its & ts := comma_list n (parse_spec n) ["SEMI"] ts in
        let@ _ & ts := expect "SEMI" ts in
        POk (ADCtx x its) ts
      else if at_ "FUNCTOR" ts then
        let@ x & ts := expect_name (advance ts) in
        let@ ps & ts := (if at_ "LBRACKET" ts then
                           let@ l & ts := name_list n ts in POk (Some l) ts
                         else POk None ts) in
        let@ _ & ts := expect "COLON" ts in
        let@ args & ts := comma_list n (parse_spec n) ["HYPHEN_GREATER"] ts in
        let@ _ & ts := expect "HYPHEN_GREATER" ts in
        let@ res & ts := comma_list n (parse_spec n) ["LBRACE"; "SEMI"] ts in
        if at_ "SEMI" ts then POk (ADFunctor x ps args res None) (advance ts) else
        match ps with
        | Some _ => err "only an abstract functor lists polymorphic inputs (D84)" ts
        | None =>
            let@ _ & ts := expect "LBRACE" ts in
            let@ bs & ts := binds_loop n ts in
            let@ _ & ts := expect "RBRACE" ts in
            POk (ADFunctor x None args res (Some bs)) ts
        end
      else err ("expected a declaration, found " ++ kind ts) ts
  end
with decls_until (n : nat) (ts : list token) : pr (list adecl) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "RBRACE" ts then POk [] ts else
      let@ d & ts := parse_decl n ts in
      let@ ds & ts := decls_until n ts in POk (d :: ds) ts
  end.

Definition use_name (ts : list token) : pr (bool * name) :=
  let (d, ts) := eat "DOT" ts in
  let@ x & ts := expect_name ts in POk (d, x) ts.

Definition use_item (ts : list token) : pr ause :=
  let@ (d, x) & ts := use_name ts in
  if at_ "AS" ts then
    let@ (d', y) & ts := use_name (advance ts) in
    if Bool.eqb d d' then POk (AUse d x (Some (d', y))) ts
    else err "a use rename must keep the name's dottedness" ts
  else POk (AUse d x None) ts.

Definition parse_import (n : nat) (ts : list token) : pr aimport :=
  let@ _ & ts := expect "IMPORT" ts in
  let@ s & ts := expect "STRING" ts in
  let@ alias & ts := (if at_ "AS" ts then let@ a & ts := expect_name (advance ts) in POk (Some a) ts
                      else POk None ts) in
  let@ (glob, uses) & ts :=
    (if at_ "USE" ts then
       let ts := advance ts in
       if at_ "STAR" ts then POk (true, []) (advance ts)
       else let@ us & ts := comma_list n use_item ["SEMI"] ts in POk (false, us) ts
     else POk (false, []) ts) in
  let@ _ & ts := expect "SEMI" ts in
  POk (AImport (string_value (tx s)) alias glob uses) ts.

Fixpoint imports_loop (n : nat) (ts : list token) : pr (list aimport) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "IMPORT" ts then
        let@ i & ts := parse_import n ts in
        let@ is & ts := imports_loop n ts in POk (i :: is) ts
      else POk [] ts
  end.

Fixpoint decls_to_eof (n : nat) (ts : list token) : pr (list adecl) :=
  match n with
  | 0 => fuel_err ts
  | S n =>
      if at_ "EOF" ts then POk [] ts else
      let@ d & ts := parse_decl n ts in
      let@ ds & ts := decls_to_eof n ts in POk (d :: ds) ts
  end.

Definition parse_tokens (ts : list token) : pr afile :=
  (* Fuel bounds the depth of the parser's recursion, which is at most a
     constant times the number of tokens: every loop iteration consumes a
     token, and at most about a dozen calls nest without consuming one. *)
  let n := 32 * (length ts + 1) in
  let@ is & ts := imports_loop n ts in
  let@ ds & ts := decls_to_eof n ts in
  POk (AFile is ds) ts.

(* parse.parse with error_message's `line:col: ` and the error's class *)
Definition parse_moss (src : string) : res afile :=
  let cs := newlines (list_byte_of_string src) in
  let where_ o := let (l, c) := line_col cs o 1 1 in show_nat l ++ ":" ++ show_nat c ++ ": " in
  match lex cs with
  | LErr m o => Bad (where_ o ++ "LexError: " ++ m)
  | LOk ts =>
      match parse_tokens ts with
      | POk f _ => Good f
      | PErr m o => Bad (where_ o ++ "ParseError: " ++ m)
      end
  end.

(* The dump (sexpr.py's format, AST -> sx) is Audit.Dump's: the
   specification of the loader (Audit.Loader.Bundled) is stated with it. *)

(* ===================== Text ================================================ *)

(* sexpr.py's text: `(` `)` around a node, `[` `]` around a list, one space
   between; atoms bare.  Built as a list of pieces, concatenated once. *)
Fixpoint pr_sx (x : sx) (acc : list string) {struct x} : list string :=
  match x with
  | SA a => a :: acc
  | SL b xs =>
      (if b then "[" else "(") ::
      (fix go (xs : list sx) (acc : list string) {struct xs} : list string :=
         match xs with
         | [] => acc
         | [y] => pr_sx y acc
         | y :: r => pr_sx y (" " :: go r acc)
         end) xs ((if b then "]" else ")") :: acc)
  end.

Definition print_sx (x : sx) : string := String.concat "" (pr_sx x []).

(* ######################################################################## *)
(* Bundles (py/bundle.py; collect.Loader's normalize, resolve, wants_prelude) *)
(* ######################################################################## *)

(* normalize, resolve and wants_prelude are Audit.Loader's. *)

Record lmod := LMod { lm_path : string; lm_wants : bool; lm_targets : list string; lm_dump : sx }.

(* bundle.py's walk: a stack, the entry first; after a module, its imports
   (first import on top), and then the prelude on top if it wants it. *)
Fixpoint walk (fuel : nat) (read : string -> option string) (root prelude : string)
  (todo seen : list string) (acc : list lmod) : res (list lmod) :=
  match fuel with
  | 0 => Bad "bundle: too many modules"
  | S fuel =>
  match todo with
  | [] => Good (rev' acc)
  | p :: todo =>
      if existsb (String.eqb p) seen then walk fuel read root prelude todo seen acc else
      match read p with
      | None => Bad ("bundle: " ++ p ++ ": OSError: cannot read")
      | Some text =>
          match parse_moss text with
          | Bad m => Bad ("bundle: " ++ p ++ ": " ++ m)
          | Good f =>
              if DumpRoundTrip.wf_names f then
              let w := wants_prelude prelude p in
              let ts := map (fun i => resolve root prelude p (i_path i)) (f_imports f) in
              let todo := app ts todo in
              let todo := if w then prelude :: todo else todo in
              walk fuel read root prelude todo (p :: seen) (LMod p w ts (dump_file f) :: acc)
              else Bad ("bundle: " ++ p ++ ": names cannot be represented in the AST dump")
          end
      end
  end end.

Definition bundle_sx (entry prelude : string) (ms : list lmod) : sx :=
  SL false (SA "bundle" :: SA entry :: SA prelude ::
            map (fun m => SL false [SA "module"; SA (lm_path m); SA (if lm_wants m then "1" else "0");
                                    SL false (map SA (lm_targets m)); lm_dump m]) ms).

(* the bundle, as Elab.elaborate_sx reads it, for an entry file *)
Definition load_bundle (read : string -> option string) (root prelude_in entry_in : string)
  : res (string * string * list lmod) :=
  let prelude := normalize root None prelude_in in
  let entry := normalize root None entry_in in
  let* ms := walk 100000 read root prelude [entry] [] [] in
  Good (entry, prelude, ms).

(* The bundle the elaborator reads, as Part VI's LOADER states it: the
   loaded modules' dumps, read back (Elab.elaborate_sx reads them so). *)
Definition load (read : string -> option string) (root prelude_in entry_in : string) : res bundle :=
  match load_bundle read root prelude_in entry_in with
  | Good (e, p, ms) => read_bundle (bundle_sx e p ms)
  | Bad m => Bad m
  end.

(* bundle.py's text, byte for byte (quote() escapes backslashes and double quotes) *)
Fixpoint quote_bytes (cs : list byte) : list byte :=
  match cs with
  | [] => []
  | c :: r => app (if Byte.eqb c "\"%byte || Byte.eqb c """"%byte then ["\"%byte; c] else [c]) (quote_bytes r)
  end.
Definition quote (s : string) : string :=
  """" ++ string_of_list_byte (quote_bytes (list_byte_of_string s)) ++ """".

Definition nl : string := String.String "010"%char "".

Definition print_bundle (entry prelude : string) (ms : list lmod) : string :=
  let head := "(bundle " ++ quote entry ++ " " ++ quote prelude in
  let mods := flat_map (fun m => [nl; "(module " ++ quote (lm_path m) ++ " " ++
                                  (if lm_wants m then "1" else "0") ++ " (" ++
                                  String.concat " " (map quote (lm_targets m)) ++ ")" ++ nl;
                                  print_sx (lm_dump m); ")"]) ms in
  String.concat "" (head :: app mods [")" ++ nl]).

(* ######################################################################## *)
(* What is proved (m4)                                                       *)
(* ######################################################################## *)

(* Totality and termination are Gallina's: every function above is total.
   Determinism, stated: the front end is a function of the source text. *)
Theorem parse_moss_deterministic : forall s f1 f2,
  parse_moss s = Good f1 -> parse_moss s = Good f2 -> f1 = f2.
Proof. intros s f1 f2 H1 H2. rewrite H1 in H2. injection H2. auto. Qed.

(* The lexer's output always ends with the EOF token, which the parser's
   `peekn`/`advance` rely on (Parser.peek's clamping). *)
Lemma lex_go_eof : forall fuel cs i acc ts,
  lex_go fuel cs i acc = LOk ts -> exists pre o, ts = app pre [Tok "EOF" "" o].
Proof.
  induction fuel as [|fuel IH]; intros cs i acc ts H; simpl in H; [discriminate|].
  destruct cs as [|c r].
  - injection H as <-. exists (rev' acc), i. unfold rev'. rewrite <- !rev_alt. simpl. reflexivity.
  - destruct (lex_char c r i acc) as [[[r' i'] acc'] | [m o]]; [eauto | discriminate].
Qed.

Theorem lex_ends_eof : forall cs ts,
  lex cs = LOk ts -> exists pre o, ts = app pre [Tok "EOF" "" o].
Proof. intros cs ts H. exact (lex_go_eof _ _ _ _ _ H). Qed.

(* Paths: the dump writes a path joined by `::`, the reader splits it; for
   names without `:` (which every NAME token is), that is the identity. *)
Definition no_colon (n : string) : Prop := ~ In ":"%char (list_ascii_of_string n).

Lemma split_on_colons_app : forall l acc rest,
  ~ In ":"%char l ->
  split_on_colons (app l rest) acc = split_on_colons rest (app (rev l) acc).
Proof.
  induction l as [|c l IH]; intros acc rest Hn; simpl; [reflexivity|].
  assert (c <> ":"%char) by (intro; apply Hn; left; auto).
  assert (~ In ":"%char l) by (intro; apply Hn; right; auto).
  destruct c as [[] [] [] [] [] [] [] []]; try (rewrite IH by auto; simpl; rewrite <- app_assoc; reflexivity);
    exfalso; apply H; reflexivity.
Qed.

Lemma split_on_colons_none : forall l acc,
  ~ In ":"%char l -> split_on_colons l acc = [app (rev acc) l].
Proof.
  intros l acc Hn. rewrite <- (app_nil_r l) at 1. rewrite split_on_colons_app by auto.
  simpl. rewrite rev_app_distr, rev_involutive. reflexivity.
Qed.

Lemma list_ascii_append : forall a b,
  list_ascii_of_string (a ++ b) = app (list_ascii_of_string a) (list_ascii_of_string b).
Proof. induction a as [|c a IH]; intro b; simpl; [reflexivity|]. rewrite IH. reflexivity. Qed.

Theorem split_join : forall p, p <> [] -> Forall no_colon p -> split_path (join_path p) = p.
Proof.
  unfold split_path, join_path.
  intros p Hne Hok.
  enough (H : forall q, Forall no_colon q -> forall x, no_colon x ->
            split_on_colons (list_ascii_of_string (String.concat "::" (x :: q))) [] =
            map list_ascii_of_string (x :: q)).
  { destruct p as [|x p]; [congruence|]. inversion Hok as [|? ? Hx Hp]; subst.
    rewrite (H p Hp x Hx). simpl. rewrite string_of_list_ascii_of_string.
    f_equal. rewrite map_map. rewrite <- (map_id p) at 2. apply map_ext.
    intro. apply string_of_list_ascii_of_string. }
  clear p Hne Hok. intro q. induction q as [|y q IH]; intros Hp x Hx.
  - simpl. rewrite split_on_colons_none by exact Hx. reflexivity.
  - inversion Hp; subst. change (String.concat "::" (x :: y :: q)) with (x ++ "::" ++ String.concat "::" (y :: q)).
    rewrite list_ascii_append. rewrite split_on_colons_app by exact Hx.
    remember (String.concat "::" (y :: q)) as rest eqn:Er. simpl.
    rewrite Er, IH by auto. rewrite app_nil_r, rev_involutive. reflexivity.
Qed.
