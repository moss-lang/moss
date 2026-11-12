(* The reference lexer, Menhir parser entry point, AST dump and legacy
   bundle loader. Source offsets survive raw-string expansion. *)

From Stdlib Require Import String Ascii List Bool Arith.
From Stdlib Require Import Strings.Byte.
From NM Require MossGrammar MossActions.
From NM Require Export Sx.
From NM Require Import ExecutionModel SourceNames.
Import ListNotations.
Import ExecutionModel.Input ExecutionModel.Syntax Sx.Dump ExecutionModel.Loader.
Local Open Scope string_scope.

(* ######################################################################## *)
(* Lexer (lex.py)                                                            *)
(* ######################################################################## *)

(* The lexer uses the token kinds and character classes in the lexical
   specification. *)

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
        if is_digit d then let (ds2, r'') := span is_digit r' in (DECIMAL, "."%byte :: d :: ds2, r'')
        else (INTEGER, [], r1)
    | _ => (INTEGER, [], r1)
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
                    else inl (Tok DECIMAL (string_of_list_byte text) i, r4, k)
        | [] => inl (Tok DECIMAL (string_of_list_byte text) i, r4, k)
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
      let k := match assoc text KEYWORDS with Some k => k | None => NAME end in
      inl (r', S i + length ns, Tok k text i :: acc)
    else if Byte.eqb c """"%byte then
      match lex_string i (S i) r [] with
      | inl (body, r') =>
          inl (r', S i + length body + 1, Tok STRING (string_of_list_byte (c :: app body [c])) i :: acc)
      | inr (m, o) => inr (m, o)
      end
    else inr ("unexpected character " ++ repr_char_at c r, i)
  end end.

Fixpoint lex_go (fuel : nat) (cs : list byte) (i : nat) (acc : list token) : lres :=
  match fuel with
  | 0 => LErr "lexer out of fuel" i
  | S fuel =>
      match cs with
      | [] => LOk (rev' (Tok EOF "" i :: acc))
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
(* Menhir parser entry point                                                 *)
(* ######################################################################## *)

Definition kind_name (k : token_kind) : string :=
  match k with
  | NAME => "NAME" | STRING => "STRING" | INTEGER => "INTEGER"
  | DECIMAL => "DECIMAL" | AS => "AS" | ASSUME => "ASSUME"
  | FUNCTOR => "FUNCTOR" | BIND => "BIND" | BREAK => "BREAK"
  | CONTEXT => "CONTEXT" | ELSE => "ELSE" | FN => "FN" | FOR => "FOR"
  | IF => "IF" | IMPORT => "IMPORT" | LET => "LET" | LOOP => "LOOP"
  | MATCH => "MATCH" | RETURN => "RETURN" | TAG => "TAG"
  | THIS_TYPE => "THIS_TYPE" | THIS => "THIS" | TYPE => "TYPE" | USE => "USE"
  | VAL => "VAL" | VAR => "VAR" | WHILE => "WHILE" | IN => "IN"
  | UNDERSCORE => "UNDERSCORE" | EXCLAM => "EXCLAM" | PERCENT => "PERCENT"
  | AMPERSAND => "AMPERSAND" | LPAREN => "LPAREN" | RPAREN => "RPAREN"
  | STAR => "STAR" | PLUS => "PLUS" | COMMA => "COMMA" | HYPHEN => "HYPHEN"
  | DOT => "DOT" | SLASH => "SLASH" | COLON => "COLON" | SEMI => "SEMI"
  | LESS => "LESS" | EQUAL => "EQUAL" | GREATER => "GREATER"
  | LBRACKET => "LBRACKET" | RBRACKET => "RBRACKET" | CARET => "CARET"
  | LBRACE => "LBRACE" | PIPE => "PIPE" | RBRACE => "RBRACE"
  | EXCLAM_EQUAL => "EXCLAM_EQUAL" | COLON_COLON => "COLON_COLON"
  | EQUAL_GREATER => "EQUAL_GREATER" | LESS_LESS => "LESS_LESS"
  | LESS_EQUAL => "LESS_EQUAL" | EQUAL_EQUAL => "EQUAL_EQUAL"
  | GREATER_EQUAL => "GREATER_EQUAL" | GREATER_GREATER => "GREATER_GREATER"
  | HYPHEN_GREATER => "HYPHEN_GREATER" | EOF => "EOF"
  end.
(* Menhir's bound is logarithmic in the number of automaton steps.
   GrammarBound proves that this gives enough steps for every file tree. *)
Definition parse_log_fuel (ts : list token) : nat :=
  8 + Nat.log2_up (S (List.length ts)).

Import MossGrammar.MenhirLibParser.Inter.

CoFixpoint padding : buffer := Buf_cons (MossGrammar.PADDING tt) padding.

Definition parse_tokens (ts : list token) : MossActions.checked afile :=
  let input := app_buf (map MossGrammar.of_token ts) padding in
  match MossGrammar.file (parse_log_fuel ts) input with
  | Parsed_pr result rest =>
      let t := buf_head rest in
      if MossGrammar.is_padding t then result
      else MossActions.Invalid "unexpected tokens after the end of the file"
             (toff (MossGrammar.source_token t))
  | Fail_pr_full _ t =>
      let t := MossGrammar.source_token t in
      MossActions.Invalid ("unexpected " ++ kind_name (tk t)) (toff t)
  | Timeout_pr => MossActions.Invalid "parser out of fuel" 0
  end.

(* Report lexer and parser errors at their original source offsets. *)
Definition parse_moss (src : string) : res afile :=
  let original := newlines (list_byte_of_string src) in
  let expanded := raw_source original in
  let cs := map fst expanded in
  let where_ o := let original_off := snd (nth o expanded (x00, length original)) in
    let (l, c) := line_col original original_off 1 1 in show_nat l ++ ":" ++ show_nat c ++ ": " in
  match lex cs with
  | LErr m o => Bad (where_ o ++ "LexError: " ++ m)
  | LOk ts =>
      match parse_tokens ts with
      | MossActions.Checked f => Good f
      | MossActions.Invalid m o => Bad (where_ o ++ "ParseError: " ++ m)
      end
  end.

(* The loader uses the S-expression codec in Sx.Dump. *)

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

(* normalize, resolve and wants_prelude are ExecutionModel.Loader's. *)

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
              if SourceNames.wf_names f then
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

(* The lexer marks the end of the source with an explicit EOF token. *)
Lemma lex_go_eof : forall fuel cs i acc ts,
  lex_go fuel cs i acc = LOk ts -> exists pre o, ts = app pre [Tok EOF "" o].
Proof.
  induction fuel as [|fuel IH]; intros cs i acc ts H; simpl in H; [discriminate|].
  destruct cs as [|c r].
  - injection H as <-. exists (rev' acc), i. unfold rev'. rewrite <- !rev_alt. simpl. reflexivity.
  - destruct (lex_char c r i acc) as [[[r' i'] acc'] | [m o]]; [eauto | discriminate].
Qed.

Theorem lex_ends_eof : forall cs ts,
  lex cs = LOk ts -> exists pre o, ts = app pre [Tok EOF "" o].
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
