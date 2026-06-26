;; lex_nomem.wat — same observable behavior as lex.wat, but with NO linear
;; memory at all. It is a pure state machine over the `next_byte` stream.
;;
;; Two tricks remove the need for memory:
;;
;;   * Bounded pushback. logos backtracks, but never far: after a token's last
;;     accepting position the scanner reads at most 3 more bytes before deciding
;;     (the `\d+ i 3 2` / `i 6 4` suffixes). So a 3-slot FIFO held in globals is
;;     enough to "un-read" the over-scanned bytes for the next call.
;;
;;   * Keyword matching without storing the lexeme. Identifiers of length <= 7
;;     are packed big-endian into an i64 as they are scanned and compared against
;;     precomputed keyword constants; anything longer cannot be a keyword.
;;
;; This confirms the `memory` in lex.wat is a convenience (a re-readable buffer
;; and a keyword/lookup table), not a requirement.

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

  (func $next_token (export "next_token") (result i32)
    (local $c i32) (local $d i32) (local $e i32) (local $g i32) (local $n i32) (local $k i32)
    (local $acc i64) (local $cnt i32) (local $long i32)

    (local.set $c (call $skip))
    (if (i32.lt_s (local.get $c) (i32.const 0)) (then (return (i32.const 0))))   ;; Eof

    ;; identifier / keyword
    (if (call $is_alpha (local.get $c))
      (then
        (local.set $acc (i64.extend_i32_u (local.get $c)))
        (local.set $cnt (i32.const 1))
        (local.set $long (i32.const 0))
        (block $wdone
          (loop $wloop
            (local.set $d (call $read))
            (if (call $is_word (local.get $d))
              (then
                (local.set $cnt (i32.add (local.get $cnt) (i32.const 1)))
                (if (i32.le_u (local.get $cnt) (i32.const 7))
                  (then (local.set $acc
                          (i64.or (i64.shl (local.get $acc) (i64.const 8)) (i64.extend_i32_u (local.get $d)))))
                  (else (local.set $long (i32.const 1))))
                (br $wloop))
              (else (br $wdone)))))
        (call $pushback (local.get $d))   ;; the byte that ended the identifier
        (if (local.get $long) (then (return (i32.const 44))))   ;; too long to be a keyword
        (return (call $kw_match (local.get $acc)))))

    ;; number (unsigned position)
    (if (call $is_digit (local.get $c))
      (then (return (call $number (i32.const 0)))))

    ;; '-' : signed number if a digit follows, else Hyphen
    (if (i32.eq (local.get $c) (i32.const 45))
      (then
        (local.set $g (call $read))
        (if (call $is_digit (local.get $g))
          (then (return (call $number (i32.const 1)))))
        (call $pushback (local.get $g))
        (return (i32.const 9))))   ;; Hyphen

    ;; '!' / '!='
    (if (i32.eq (local.get $c) (i32.const 33))
      (then
        (local.set $d (call $read))
        (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 23))))
        (call $pushback (local.get $d)) (return (i32.const 1))))

    ;; ':' / '::'
    (if (i32.eq (local.get $c) (i32.const 58))
      (then
        (local.set $d (call $read))
        (if (i32.eq (local.get $d) (i32.const 58)) (then (return (i32.const 24))))
        (call $pushback (local.get $d)) (return (i32.const 12))))

    ;; '<' / '<<' / '<='
    (if (i32.eq (local.get $c) (i32.const 60))
      (then
        (local.set $d (call $read))
        (if (i32.eq (local.get $d) (i32.const 60)) (then (return (i32.const 25))))
        (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 26))))
        (call $pushback (local.get $d)) (return (i32.const 14))))

    ;; '=' / '=='
    (if (i32.eq (local.get $c) (i32.const 61))
      (then
        (local.set $d (call $read))
        (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 27))))
        (call $pushback (local.get $d)) (return (i32.const 15))))

    ;; '>' / '>>' / '>='
    (if (i32.eq (local.get $c) (i32.const 62))
      (then
        (local.set $d (call $read))
        (if (i32.eq (local.get $d) (i32.const 62)) (then (return (i32.const 29))))
        (if (i32.eq (local.get $d) (i32.const 61)) (then (return (i32.const 28))))
        (call $pushback (local.get $d)) (return (i32.const 16))))

    ;; single-character punctuation
    (if (i32.eq (local.get $c) (i32.const 37)) (then (return (i32.const 2))))    ;; %
    (if (i32.eq (local.get $c) (i32.const 38)) (then (return (i32.const 3))))    ;; &
    (if (i32.eq (local.get $c) (i32.const 40)) (then (return (i32.const 4))))    ;; (
    (if (i32.eq (local.get $c) (i32.const 41)) (then (return (i32.const 5))))    ;; )
    (if (i32.eq (local.get $c) (i32.const 42)) (then (return (i32.const 6))))    ;; *
    (if (i32.eq (local.get $c) (i32.const 43)) (then (return (i32.const 7))))    ;; +
    (if (i32.eq (local.get $c) (i32.const 44)) (then (return (i32.const 8))))    ;; ,
    (if (i32.eq (local.get $c) (i32.const 46)) (then (return (i32.const 10))))   ;; .
    (if (i32.eq (local.get $c) (i32.const 47)) (then (return (i32.const 11))))   ;; /
    (if (i32.eq (local.get $c) (i32.const 59)) (then (return (i32.const 13))))   ;; ;
    (if (i32.eq (local.get $c) (i32.const 91)) (then (return (i32.const 17))))   ;; [
    (if (i32.eq (local.get $c) (i32.const 93)) (then (return (i32.const 18))))   ;; ]
    (if (i32.eq (local.get $c) (i32.const 94)) (then (return (i32.const 19))))   ;; ^
    (if (i32.eq (local.get $c) (i32.const 123)) (then (return (i32.const 20))))  ;; {
    (if (i32.eq (local.get $c) (i32.const 124)) (then (return (i32.const 21))))  ;; |
    (if (i32.eq (local.get $c) (i32.const 125)) (then (return (i32.const 22))))  ;; }

    ;; char literal '.' (one UTF-8 scalar, not '\n')
    (if (i32.eq (local.get $c) (i32.const 39))
      (then
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
        ;; consume the remaining n-1 continuation bytes
        (block $kdone
          (loop $kloop
            (if (i32.le_s (local.get $n) (i32.const 1)) (then (br $kdone)))
            (drop (call $read))
            (local.set $n (i32.sub (local.get $n) (i32.const 1)))
            (br $kloop)))
        (if (i32.eq (call $read) (i32.const 39))   ;; closing quote
          (then (return (i32.const 51))))
        (return (i32.const -1))))

    ;; string literal
    (if (i32.eq (local.get $c) (i32.const 34))
      (then
        (block $sdone
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
            (br $sloop)))))

    (i32.const -1)))
