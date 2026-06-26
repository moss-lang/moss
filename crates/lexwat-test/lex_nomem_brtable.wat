;; lex_nomem_brtable.wat — combines both optimizations: no `memory` (pushback in
;; globals + i64 keyword matching, as in lex_nomem.wat) AND `br_table` dispatch
;; (as in lex_brtable.wat, but indexing the byte directly so no class table is
;; needed in memory). Same observable behavior as lex.wat.

(module
  (import "env" "next_byte" (func $next_byte (result i32)))

  ;; FIFO of pushed-back bytes (read before pulling from next_byte). Max depth 3.
  (global $pb0 (mut i32) (i32.const 0))
  (global $pb1 (mut i32) (i32.const 0))
  (global $pb2 (mut i32) (i32.const 0))
  (global $pbn (mut i32) (i32.const 0))

  (func $reset (export "reset") (global.set $pbn (i32.const 0)))

  ;; Next byte of the stream (draining pushback first), or -1 at end.
  (func $read (result i32)
    (local $b i32)
    (if (result i32) (global.get $pbn)
      (then
        (local.set $b (global.get $pb0))
        (global.set $pb0 (global.get $pb1))
        (global.set $pb1 (global.get $pb2))
        (global.set $pbn (i32.sub (global.get $pbn) (i32.const 1)))
        (local.get $b))
      (else (call $next_byte))))

  ;; Push a byte back to be re-read later (in original order). Ignores EOF (-1).
  (func $pushback (param $b i32)
    (if (i32.lt_s (local.get $b) (i32.const 0)) (then (return)))
    (if (i32.eqz (global.get $pbn))
      (then (global.set $pb0 (local.get $b)))
      (else (if (i32.eq (global.get $pbn) (i32.const 1))
        (then (global.set $pb1 (local.get $b)))
        (else (global.set $pb2 (local.get $b))))))
    (global.set $pbn (i32.add (global.get $pbn) (i32.const 1))))

  (func $is_digit (param $c i32) (result i32)
    (i32.lt_u (i32.sub (local.get $c) (i32.const 48)) (i32.const 10)))
  (func $is_alpha (param $c i32) (result i32)
    (i32.or
      (i32.or
        (i32.lt_u (i32.sub (local.get $c) (i32.const 65)) (i32.const 26))
        (i32.lt_u (i32.sub (local.get $c) (i32.const 97)) (i32.const 26)))
      (i32.eq (local.get $c) (i32.const 95))))
  (func $is_word (param $c i32) (result i32)
    (i32.or (call $is_digit (local.get $c)) (call $is_alpha (local.get $c))))
  (func $is_ws (param $c i32) (result i32)
    (i32.or
      (i32.or
        (i32.or (i32.eq (local.get $c) (i32.const 32)) (i32.eq (local.get $c) (i32.const 9)))
        (i32.or (i32.eq (local.get $c) (i32.const 10)) (i32.eq (local.get $c) (i32.const 13))))
      (i32.or (i32.eq (local.get $c) (i32.const 11)) (i32.eq (local.get $c) (i32.const 12)))))

  ;; Skip `(#[^\n]*|\s)+`, returning the first significant byte (consumed) or -1.
  (func $skip (result i32)
    (local $c i32) (local $d i32)
    (loop $again
      (local.set $c (call $read))
      (if (call $is_ws (local.get $c)) (then (br $again)))
      (if (i32.eq (local.get $c) (i32.const 35))   ;; '#'
        (then
          (block $cdone
            (loop $cloop
              (local.set $d (call $read))
              (if (i32.or (i32.lt_s (local.get $d) (i32.const 0)) (i32.eq (local.get $d) (i32.const 10)))
                (then (br $cdone)))
              (br $cloop)))
          (br $again))))
    (local.get $c))

  ;; Match the packed identifier `acc` (big-endian bytes, length <= 7) against
  ;; the keyword set; return the keyword id or Name (44).
  (func $kw_match (param $acc i64) (result i32)
    (if (i64.eq (local.get $acc) (i64.const 0x6173))         (then (return (i32.const 30)))) ;; as
    (if (i64.eq (local.get $acc) (i64.const 0x617373756d65)) (then (return (i32.const 31)))) ;; assume
    (if (i64.eq (local.get $acc) (i64.const 0x62696e64))     (then (return (i32.const 32)))) ;; bind
    (if (i64.eq (local.get $acc) (i64.const 0x636f6e74657874))(then (return (i32.const 33)))) ;; context
    (if (i64.eq (local.get $acc) (i64.const 0x656c7365))     (then (return (i32.const 34)))) ;; else
    (if (i64.eq (local.get $acc) (i64.const 0x666e))         (then (return (i32.const 35)))) ;; fn
    (if (i64.eq (local.get $acc) (i64.const 0x6966))         (then (return (i32.const 36)))) ;; if
    (if (i64.eq (local.get $acc) (i64.const 0x696d706f7274)) (then (return (i32.const 37)))) ;; import
    (if (i64.eq (local.get $acc) (i64.const 0x6c6574))       (then (return (i32.const 38)))) ;; let
    (if (i64.eq (local.get $acc) (i64.const 0x74797065))     (then (return (i32.const 39)))) ;; type
    (if (i64.eq (local.get $acc) (i64.const 0x757365))       (then (return (i32.const 40)))) ;; use
    (if (i64.eq (local.get $acc) (i64.const 0x76616c))       (then (return (i32.const 41)))) ;; val
    (if (i64.eq (local.get $acc) (i64.const 0x766172))       (then (return (i32.const 42)))) ;; var
    (if (i64.eq (local.get $acc) (i64.const 0x7768696c65))   (then (return (i32.const 43)))) ;; while
    (i32.const 44))

  ;; Lex a number; the first digit has already been consumed. $signed is 1 when a
  ;; leading '-' was consumed (then only i32/i64 suffixes apply, never u-forms).
  (func $number (param $signed i32) (result i32)
    (local $d i32) (local $e i32) (local $f i32)
    (loop $dloop
      (local.set $d (call $read))
      (if (call $is_digit (local.get $d)) (then (br $dloop))))   ;; $d = first non-digit
    ;; unsigned position may take a 'u' suffix
    (if (i32.and (i32.eqz (local.get $signed)) (i32.eq (local.get $d) (i32.const 117)))   ;; 'u'
      (then
        (local.set $e (call $read))
        (if (i32.eq (local.get $e) (i32.const 51))    ;; '3'
          (then
            (local.set $f (call $read))
            (if (i32.eq (local.get $f) (i32.const 50)) (then (return (i32.const 45))))   ;; u32 -> Uint32
            (call $pushback (local.get $e)) (call $pushback (local.get $f))
            (return (i32.const 49)))   ;; Uint (keeps the 'u')
          (else
            (if (i32.eq (local.get $e) (i32.const 54))   ;; '6'
              (then
                (local.set $f (call $read))
                (if (i32.eq (local.get $f) (i32.const 52)) (then (return (i32.const 47))))   ;; u64 -> Uint64
                (call $pushback (local.get $e)) (call $pushback (local.get $f))
                (return (i32.const 49)))
              (else
                (call $pushback (local.get $e))
                (return (i32.const 49))))))))   ;; just 'u' -> Uint
    ;; either position may take an 'i' suffix (else the bare digits are Int)
    (if (i32.eq (local.get $d) (i32.const 105))   ;; 'i'
      (then
        (local.set $e (call $read))
        (if (i32.eq (local.get $e) (i32.const 51))
          (then
            (local.set $f (call $read))
            (if (i32.eq (local.get $f) (i32.const 50)) (then (return (i32.const 46))))   ;; i32 -> Int32
            (call $pushback (local.get $d)) (call $pushback (local.get $e)) (call $pushback (local.get $f))
            (return (i32.const 50)))   ;; Int (the 'i...' is separate)
          (else
            (if (i32.eq (local.get $e) (i32.const 54))
              (then
                (local.set $f (call $read))
                (if (i32.eq (local.get $f) (i32.const 52)) (then (return (i32.const 48))))   ;; i64 -> Int64
                (call $pushback (local.get $d)) (call $pushback (local.get $e)) (call $pushback (local.get $f))
                (return (i32.const 50)))
              (else
                (call $pushback (local.get $d)) (call $pushback (local.get $e))
                (return (i32.const 50))))))))
    ;; no suffix
    (call $pushback (local.get $d))
    (i32.const 50))   ;; Int

  ;; Dispatch via a 128-wide `br_table` on the byte itself — no class table, so
  ;; no `memory` (combines the lex_brtable.wat and lex_nomem.wat ideas).
  (func $next_token (export "next_token") (result i32)
    (local $c i32) (local $d i32) (local $e i32) (local $g i32) (local $n i32)
    (local $acc i64) (local $cnt i32) (local $long i32)
    (local.set $c (call $skip))
    (if (i32.lt_s (local.get $c) (i32.const 0)) (then (return (i32.const 0))))
    (block $invalid (block $bang (block $colon (block $less (block $eq (block $gt (block $minus (block $char (block $str (block $alpha (block $digit (block $s_pct (block $s_amp (block $s_lpar (block $s_rpar (block $s_star (block $s_plus (block $s_comma (block $s_dot (block $s_slash (block $s_semi (block $s_lbrack (block $s_rbrack (block $s_caret (block $s_lbrace (block $s_pipe (block $s_rbrace 
      (br_table $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $invalid $bang $str $invalid $invalid $s_pct $s_amp $char $s_lpar $s_rpar $s_star $s_plus $s_comma $minus $s_dot $s_slash $digit $digit $digit $digit $digit $digit $digit $digit $digit $digit $colon $s_semi $less $eq $gt $invalid $invalid $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $s_lbrack $invalid $s_rbrack $s_caret $alpha $invalid $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $alpha $s_lbrace $s_pipe $s_rbrace $invalid $invalid $invalid (local.get $c))
    )  ;; -> $s_rbrace
      (return (i32.const 22))
    )  ;; -> $s_pipe
      (return (i32.const 21))
    )  ;; -> $s_lbrace
      (return (i32.const 20))
    )  ;; -> $s_caret
      (return (i32.const 19))
    )  ;; -> $s_rbrack
      (return (i32.const 18))
    )  ;; -> $s_lbrack
      (return (i32.const 17))
    )  ;; -> $s_semi
      (return (i32.const 13))
    )  ;; -> $s_slash
      (return (i32.const 11))
    )  ;; -> $s_dot
      (return (i32.const 10))
    )  ;; -> $s_comma
      (return (i32.const 8))
    )  ;; -> $s_plus
      (return (i32.const 7))
    )  ;; -> $s_star
      (return (i32.const 6))
    )  ;; -> $s_rpar
      (return (i32.const 5))
    )  ;; -> $s_lpar
      (return (i32.const 4))
    )  ;; -> $s_amp
      (return (i32.const 3))
    )  ;; -> $s_pct
      (return (i32.const 2))
    )  ;; -> $digit
      (return (call $number (i32.const 0)))
    )  ;; -> $alpha
      (local.set $acc (i64.extend_i32_u (local.get $c)))
      (local.set $cnt (i32.const 1)) (local.set $long (i32.const 0))
      (block $wdone
        (loop $wloop
          (local.set $d (call $read))
          (if (call $is_word (local.get $d))
            (then
              (local.set $cnt (i32.add (local.get $cnt) (i32.const 1)))
              (if (i32.le_u (local.get $cnt) (i32.const 7))
                (then (local.set $acc (i64.or (i64.shl (local.get $acc) (i64.const 8)) (i64.extend_i32_u (local.get $d)))))
                (else (local.set $long (i32.const 1))))
              (br $wloop))
            (else (br $wdone)))))
      (call $pushback (local.get $d))
      (if (local.get $long) (then (return (i32.const 44))))
      (return (call $kw_match (local.get $acc)))
    )  ;; -> $str
      (loop $sloop
        (local.set $d (call $read))
        (if (i32.lt_s (local.get $d) (i32.const 0)) (then (return (i32.const -1))))
        (if (i32.eq (local.get $d) (i32.const 34)) (then (return (i32.const 52))))
        (if (i32.eq (local.get $d) (i32.const 92))
          (then
            (local.set $e (call $read))
            (if (i32.or
                  (i32.or (i32.eq (local.get $e) (i32.const 34)) (i32.eq (local.get $e) (i32.const 92)))
                  (i32.or (i32.eq (local.get $e) (i32.const 110))
                          (i32.or (i32.eq (local.get $e) (i32.const 114)) (i32.eq (local.get $e) (i32.const 116)))))
              (then (br $sloop))
              (else (return (i32.const -1))))))
        (if (i32.eq (local.get $d) (i32.const 10)) (then (return (i32.const -1))))
        (br $sloop))
      (unreachable)
    )  ;; -> $char
      (local.set $d (call $read))
      (if (i32.lt_s (local.get $d) (i32.const 0)) (then (return (i32.const -1))))
      (if (i32.lt_u (local.get $d) (i32.const 0x80))
        (then
          (if (i32.eq (local.get $d) (i32.const 10)) (then (return (i32.const -1))))
          (local.set $n (i32.const 1)))
        (else
          (if (i32.lt_u (local.get $d) (i32.const 0xe0)) (then (local.set $n (i32.const 2)))
            (else (if (i32.lt_u (local.get $d) (i32.const 0xf0)) (then (local.set $n (i32.const 3)))
              (else (local.set $n (i32.const 4))))))))
      (block $kdone
        (loop $kloop
          (if (i32.le_s (local.get $n) (i32.const 1)) (then (br $kdone)))
          (drop (call $read))
          (local.set $n (i32.sub (local.get $n) (i32.const 1)))
          (br $kloop)))
      (if (i32.eq (call $read) (i32.const 39)) (then (return (i32.const 51))))
      (return (i32.const -1))
    )  ;; -> $minus
      (local.set $g (call $read))
      (if (call $is_digit (local.get $g)) (then (return (call $number (i32.const 1)))))
      (call $pushback (local.get $g)) (return (i32.const 9))
    )  ;; -> $gt
      (local.set $d (call $read))
      (if (i32.eq (local.get $d) (i32.const 62)) (then (return (i32.const 29))))
      (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 28))))
      (call $pushback (local.get $d)) (return (i32.const 16))
    )  ;; -> $eq
      (local.set $d (call $read))
      (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 27))))
      (call $pushback (local.get $d)) (return (i32.const 15))
    )  ;; -> $less
      (local.set $d (call $read))
      (if (i32.eq (local.get $d) (i32.const 60)) (then (return (i32.const 25))))
      (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 26))))
      (call $pushback (local.get $d)) (return (i32.const 14))
    )  ;; -> $colon
      (local.set $d (call $read))
      (if (i32.eq (local.get $d) (i32.const 58)) (then (return (i32.const 24))))
      (call $pushback (local.get $d)) (return (i32.const 12))
    )  ;; -> $bang
      (local.set $d (call $read))
      (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 23))))
      (call $pushback (local.get $d)) (return (i32.const 1))
    )  ;; -> $invalid
      (return (i32.const -1))
    (i32.const -1))
)
