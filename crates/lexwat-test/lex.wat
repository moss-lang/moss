;; lex.wat — a streaming tokenizer emulating crates/moss-core/src/lex.rs
;;
;; It imports `next_byte` and exports `next_token`.
;;
;;   (func $next_byte (result i32))
;;       Returns the next byte of the source as 0..=255, or a negative value
;;       (e.g. -1) once the source is exhausted. Each byte is returned exactly
;;       once, in order (a pure stream — no seeking).
;;
;;   (func $next_token (result i32))
;;       Returns the id of the next token, advancing an internal cursor. Token
;;       ids match the declaration order of the `Token` enum in lex.rs (Eof = 0,
;;       Exclam = 1, ... Str = 52). The trailing `Eof` (0) is emitted exactly
;;       like `lex()` pushes it; calling again past the end keeps returning 0.
;;       A negative result (-1) signals an invalid token (LexError::InvalidToken),
;;       at which point lexing stops, mirroring `lex()` returning `Err`.
;;
;; Because `next_byte` is a forward-only stream but logos performs maximal munch
;; with backtracking (e.g. `123u3` lexes as `123u` then `3`), every byte read is
;; buffered into linear memory. The cursor `$pos` indexes into that buffer, so
;; re-reading / backing up is free.
;;
;; UTF-8: source is assumed UTF-8. String contents and char literals handle
;; multi-byte scalars exactly like logos (string bytes >= 0x80 pass through; a
;; char literal `'.'` consumes one whole scalar). The `\w` (identifier) and `\s`
;; (whitespace) classes use their ASCII definitions — moss identifiers and
;; whitespace are ASCII, so this matches logos for all real moss source.

(module
  (import "env" "next_byte" (func $next_byte (result i32)))

  (memory (export "memory") 256)

  ;; Keyword table at offset 0: a sequence of entries, each
  ;;   [len:u8][id:u8][bytes...]
  ;; terminated by a zero length byte. Ids are the Token enum discriminants.
  (data (i32.const 0)
    "\02\1eas"        ;; As       = 30
    "\06\1fassume"    ;; Assume   = 31
    "\04\20bind"      ;; Bind     = 32
    "\07\21context"   ;; Context  = 33
    "\04\22else"      ;; Else     = 34
    "\02\23fn"        ;; Fn       = 35
    "\02\24if"        ;; If       = 36
    "\06\25import"    ;; Import   = 37
    "\03\26let"       ;; Let      = 38
    "\04\27type"      ;; Type     = 39
    "\03\28use"       ;; Use      = 40
    "\03\29val"       ;; Val      = 41
    "\03\2avar"       ;; Var      = 42
    "\05\2bwhile"     ;; While    = 43
    "\00")

  ;; Input buffer base. Keyword table occupies [0, ~83), so 1024 is clear.
  (global $BUF i32 (i32.const 1024))

  (global $pos (mut i32) (i32.const 0))      ;; cursor: start of next token
  (global $buf_len (mut i32) (i32.const 0))  ;; bytes pulled from next_byte
  (global $eof (mut i32) (i32.const 0))      ;; 1 once next_byte hit end

  ;; Reset all state so one instance can lex a fresh source (used by benchmarks).
  (func $reset (export "reset")
    (global.set $pos (i32.const 0))
    (global.set $buf_len (i32.const 0))
    (global.set $eof (i32.const 0)))

  ;; Byte at index $i (0..=255), or -1 if $i is at/after end of source.
  (func $get (param $i i32) (result i32)
    (local $b i32)
    (block $filled
      (loop $fill
        ;; stop filling once at eof or the byte at $i is already buffered
        (if (i32.or (global.get $eof) (i32.gt_u (global.get $buf_len) (local.get $i)))
          (then (br $filled)))
        (local.set $b (call $next_byte))
        (if (i32.lt_s (local.get $b) (i32.const 0))
          (then (global.set $eof (i32.const 1)) (br $filled)))
        (i32.store8 (i32.add (global.get $BUF) (global.get $buf_len)) (local.get $b))
        (global.set $buf_len (i32.add (global.get $buf_len) (i32.const 1)))
        (br $fill)))
    (if (result i32) (i32.gt_u (global.get $buf_len) (local.get $i))
      (then (i32.load8_u (i32.add (global.get $BUF) (local.get $i))))
      (else (i32.const -1))))

  (func $is_digit (param $c i32) (result i32)
    (i32.lt_u (i32.sub (local.get $c) (i32.const 48)) (i32.const 10)))

  ;; A-Z, a-z, or underscore
  (func $is_alpha (param $c i32) (result i32)
    (i32.or
      (i32.or
        (i32.lt_u (i32.sub (local.get $c) (i32.const 65)) (i32.const 26))
        (i32.lt_u (i32.sub (local.get $c) (i32.const 97)) (i32.const 26)))
      (i32.eq (local.get $c) (i32.const 95))))

  ;; \w  ==  [0-9A-Za-z_]
  (func $is_word (param $c i32) (result i32)
    (i32.or (call $is_digit (local.get $c)) (call $is_alpha (local.get $c))))

  ;; \s  ==  space, tab, LF, CR, VT, FF
  (func $is_ws (param $c i32) (result i32)
    (i32.or
      (i32.or
        (i32.or (i32.eq (local.get $c) (i32.const 32)) (i32.eq (local.get $c) (i32.const 9)))
        (i32.or (i32.eq (local.get $c) (i32.const 10)) (i32.eq (local.get $c) (i32.const 13))))
      (i32.or (i32.eq (local.get $c) (i32.const 11)) (i32.eq (local.get $c) (i32.const 12)))))

  ;; Skip `(#[^\n]*|\s)+`: runs of whitespace and `#`-to-end-of-line comments.
  (func $skip_trivia
    (local $c i32) (local $d i32)
    (block $done
      (loop $again
        (local.set $c (call $get (global.get $pos)))
        (if (call $is_ws (local.get $c))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (br $again)))
        (if (i32.eq (local.get $c) (i32.const 35))   ;; '#'
          (then
            (global.set $pos (i32.add (global.get $pos) (i32.const 1)))
            (block $cdone
              (loop $cloop
                (local.set $d (call $get (global.get $pos)))
                ;; comment body is [^\n]* — stop at LF or eof (LF is eaten as ws)
                (if (i32.or (i32.lt_s (local.get $d) (i32.const 0)) (i32.eq (local.get $d) (i32.const 10)))
                  (then (br $cdone)))
                (global.set $pos (i32.add (global.get $pos) (i32.const 1)))
                (br $cloop)))
            (br $again)))
        (br $done))))

  ;; Compare the lexeme buffer[$start .. $start+$len] against the keyword table.
  ;; Returns the keyword id, or Name (44) if it is not a keyword.
  (func $kw_lookup (param $start i32) (param $len i32) (result i32)
    (local $p i32) (local $klen i32) (local $kid i32) (local $j i32) (local $ok i32)
    (local.set $p (i32.const 0))
    (block $done
      (loop $next
        (local.set $klen (i32.load8_u (local.get $p)))
        (if (i32.eqz (local.get $klen)) (then (br $done)))   ;; terminator
        (local.set $kid (i32.load8_u (i32.add (local.get $p) (i32.const 1))))
        (if (i32.eq (local.get $klen) (local.get $len))
          (then
            (local.set $j (i32.const 0))
            (local.set $ok (i32.const 1))
            (block $cmpdone
              (loop $cmp
                (if (i32.ge_u (local.get $j) (local.get $len)) (then (br $cmpdone)))
                (if (i32.ne
                      (i32.load8_u (i32.add (i32.add (local.get $p) (i32.const 2)) (local.get $j)))
                      (i32.load8_u (i32.add (global.get $BUF) (i32.add (local.get $start) (local.get $j)))))
                  (then (local.set $ok (i32.const 0)) (br $cmpdone)))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $cmp)))
            (if (local.get $ok) (then (return (local.get $kid))))))
        (local.set $p (i32.add (local.get $p) (i32.add (i32.const 2) (local.get $klen))))
        (br $next)))
    (i32.const 44))   ;; Name

  ;; Lex a number whose digits start at $pos + $signed (1 if a leading '-' was
  ;; already seen, else 0). Updates $pos and returns the token id.
  ;; Suffixes: unsigned position may take u/u32/u64/i32/i64; signed position
  ;; (leading '-') only i32/i64 (the u-forms have no `-?` in lex.rs).
  (func $lex_number (param $signed i32) (result i32)
    (local $i i32) (local $s0 i32) (local $s1 i32) (local $s2 i32) (local $token i32)
    (local.set $i (i32.add (global.get $pos) (local.get $signed)))
    (block $ddone
      (loop $dloop
        (if (call $is_digit (call $get (local.get $i)))
          (then (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $dloop)))
        (br $ddone)))
    (local.set $token (i32.const 50))   ;; Int
    (local.set $s0 (call $get (local.get $i)))
    (local.set $s1 (call $get (i32.add (local.get $i) (i32.const 1))))
    (local.set $s2 (call $get (i32.add (local.get $i) (i32.const 2))))
    (if (i32.eqz (local.get $signed))
      (then
        (if (i32.eq (local.get $s0) (i32.const 117))   ;; 'u'
          (then
            (if (i32.and (i32.eq (local.get $s1) (i32.const 51)) (i32.eq (local.get $s2) (i32.const 50)))
              (then (local.set $token (i32.const 45)) (local.set $i (i32.add (local.get $i) (i32.const 3))))   ;; u32 -> Uint32
              (else
                (if (i32.and (i32.eq (local.get $s1) (i32.const 54)) (i32.eq (local.get $s2) (i32.const 52)))
                  (then (local.set $token (i32.const 47)) (local.set $i (i32.add (local.get $i) (i32.const 3))))   ;; u64 -> Uint64
                  (else (local.set $token (i32.const 49)) (local.set $i (i32.add (local.get $i) (i32.const 1))))))))   ;; u -> Uint
          (else
            (if (i32.eq (local.get $s0) (i32.const 105))   ;; 'i'
              (then
                (if (i32.and (i32.eq (local.get $s1) (i32.const 51)) (i32.eq (local.get $s2) (i32.const 50)))
                  (then (local.set $token (i32.const 46)) (local.set $i (i32.add (local.get $i) (i32.const 3))))   ;; i32 -> Int32
                  (else
                    (if (i32.and (i32.eq (local.get $s1) (i32.const 54)) (i32.eq (local.get $s2) (i32.const 52)))
                      (then (local.set $token (i32.const 48)) (local.set $i (i32.add (local.get $i) (i32.const 3))))))))))))   ;; i64 -> Int64
      (else
        (if (i32.eq (local.get $s0) (i32.const 105))   ;; 'i'
          (then
            (if (i32.and (i32.eq (local.get $s1) (i32.const 51)) (i32.eq (local.get $s2) (i32.const 50)))
              (then (local.set $token (i32.const 46)) (local.set $i (i32.add (local.get $i) (i32.const 3))))
              (else
                (if (i32.and (i32.eq (local.get $s1) (i32.const 54)) (i32.eq (local.get $s2) (i32.const 52)))
                  (then (local.set $token (i32.const 48)) (local.set $i (i32.add (local.get $i) (i32.const 3)))))))))))
    (global.set $pos (local.get $i))
    (local.get $token))

  (func $next_token (export "next_token") (result i32)
    (local $c i32) (local $c1 i32) (local $i i32) (local $d i32) (local $e i32) (local $n i32)

    (call $skip_trivia)
    (local.set $c (call $get (global.get $pos)))
    (if (i32.lt_s (local.get $c) (i32.const 0))
      (then (return (i32.const 0))))   ;; Eof
    (local.set $c1 (call $get (i32.add (global.get $pos) (i32.const 1))))

    ;; identifier / keyword: [A-Z_a-z]\w*
    (if (call $is_alpha (local.get $c))
      (then
        (local.set $i (global.get $pos))
        (block $wdone
          (loop $wloop
            (if (call $is_word (call $get (local.get $i)))
              (then (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $wloop)))
            (br $wdone)))
        (local.set $c (call $kw_lookup (global.get $pos) (i32.sub (local.get $i) (global.get $pos))))
        (global.set $pos (local.get $i))
        (return (local.get $c))))

    ;; number (unsigned position)
    (if (call $is_digit (local.get $c))
      (then (return (call $lex_number (i32.const 0)))))

    ;; '-' : signed number if a digit follows, else Hyphen
    (if (i32.eq (local.get $c) (i32.const 45))
      (then
        (if (call $is_digit (local.get $c1))
          (then (return (call $lex_number (i32.const 1)))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1)))
        (return (i32.const 9))))   ;; Hyphen

    ;; '!' / '!='
    (if (i32.eq (local.get $c) (i32.const 33))
      (then
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 23))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 1))))

    ;; ':' / '::'
    (if (i32.eq (local.get $c) (i32.const 58))
      (then
        (if (i32.eq (local.get $c1) (i32.const 58))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 24))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 12))))

    ;; '<' / '<<' / '<='
    (if (i32.eq (local.get $c) (i32.const 60))
      (then
        (if (i32.eq (local.get $c1) (i32.const 60))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 25))))
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 26))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 14))))

    ;; '=' / '=='
    (if (i32.eq (local.get $c) (i32.const 61))
      (then
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 27))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 15))))

    ;; '>' / '>>' / '>='
    (if (i32.eq (local.get $c) (i32.const 62))
      (then
        (if (i32.eq (local.get $c1) (i32.const 62))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 29))))
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 28))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 16))))

    ;; single-character punctuation
    (if (i32.eq (local.get $c) (i32.const 37)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 2))))    ;; %
    (if (i32.eq (local.get $c) (i32.const 38)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 3))))    ;; &
    (if (i32.eq (local.get $c) (i32.const 40)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 4))))    ;; (
    (if (i32.eq (local.get $c) (i32.const 41)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 5))))    ;; )
    (if (i32.eq (local.get $c) (i32.const 42)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 6))))    ;; *
    (if (i32.eq (local.get $c) (i32.const 43)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 7))))    ;; +
    (if (i32.eq (local.get $c) (i32.const 44)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 8))))    ;; ,
    (if (i32.eq (local.get $c) (i32.const 46)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 10))))   ;; .
    (if (i32.eq (local.get $c) (i32.const 47)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 11))))   ;; /
    (if (i32.eq (local.get $c) (i32.const 59)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 13))))   ;; ;
    (if (i32.eq (local.get $c) (i32.const 91)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 17))))   ;; [
    (if (i32.eq (local.get $c) (i32.const 93)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 18))))   ;; ]
    (if (i32.eq (local.get $c) (i32.const 94)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 19))))   ;; ^
    (if (i32.eq (local.get $c) (i32.const 123)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 20))))  ;; {
    (if (i32.eq (local.get $c) (i32.const 124)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 21))))  ;; |
    (if (i32.eq (local.get $c) (i32.const 125)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 22))))  ;; }

    ;; char literal: '.'  — a single Unicode scalar (any char but '\n') in quotes.
    ;; logos matches on `char`s, so the inner `.` may be a multi-byte UTF-8 scalar
    ;; (e.g. 'é', '😀'). Use the lead byte to size it, then require a closing quote.
    (if (i32.eq (local.get $c) (i32.const 39))
      (then
        (if (i32.lt_s (local.get $c1) (i32.const 0)) (then (return (i32.const -1))))   ;; EOF after quote
        (if (i32.lt_u (local.get $c1) (i32.const 0x80))
          (then
            (if (i32.eq (local.get $c1) (i32.const 10)) (then (return (i32.const -1))))   ;; '\n' excluded
            (local.set $n (i32.const 1)))
          (else
            (if (i32.lt_u (local.get $c1) (i32.const 0xe0)) (then (local.set $n (i32.const 2)))
              (else (if (i32.lt_u (local.get $c1) (i32.const 0xf0)) (then (local.set $n (i32.const 3)))
                (else (local.set $n (i32.const 4))))))))
        (if (i32.eq (call $get (i32.add (global.get $pos) (i32.add (i32.const 1) (local.get $n)))) (i32.const 39))
          (then
            (global.set $pos (i32.add (global.get $pos) (i32.add (i32.const 2) (local.get $n))))
            (return (i32.const 51))))
        (return (i32.const -1))))

    ;; string literal: "([^"\\\n]|\\["\\nrt])*"
    (if (i32.eq (local.get $c) (i32.const 34))
      (then
        (local.set $i (i32.add (global.get $pos) (i32.const 1)))
        (block $sdone
          (loop $sloop
            (local.set $d (call $get (local.get $i)))
            (if (i32.lt_s (local.get $d) (i32.const 0)) (then (return (i32.const -1))))   ;; unterminated
            (if (i32.eq (local.get $d) (i32.const 34))
              (then (global.set $pos (i32.add (local.get $i) (i32.const 1))) (return (i32.const 52))))   ;; closing quote
            (if (i32.eq (local.get $d) (i32.const 92))   ;; backslash escape
              (then
                (local.set $e (call $get (i32.add (local.get $i) (i32.const 1))))
                (if (i32.or
                      (i32.or (i32.eq (local.get $e) (i32.const 34)) (i32.eq (local.get $e) (i32.const 92)))
                      (i32.or (i32.eq (local.get $e) (i32.const 110))
                              (i32.or (i32.eq (local.get $e) (i32.const 114)) (i32.eq (local.get $e) (i32.const 116)))))
                  (then (local.set $i (i32.add (local.get $i) (i32.const 2))) (br $sloop))
                  (else (return (i32.const -1))))))
            (if (i32.eq (local.get $d) (i32.const 10)) (then (return (i32.const -1))))   ;; raw newline not allowed
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $sloop)))))

    ;; anything else is an invalid token
    (i32.const -1)))
