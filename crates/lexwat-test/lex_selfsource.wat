;; lex_selfsource.wat — lex.wat with the per-byte boundary crossing removed.
;;
;; Instead of importing `next_byte` (a host call per byte), the source is placed
;; in this module's own linear memory by the host *once* (a single bulk copy),
;; and `$get` reads it directly. This is exactly the module you get by merging a
;; "driver" that owns the source and exports `next_byte` into the lexer (see
;; driver.wat + `wasm-merge`): the byte fetch becomes an in-module memory load
;; with no FFI. Dispatch is the same if-chain as lex.wat, so the only difference
;; being measured is the byte-source mechanism.
;;
;; Host protocol: write the source bytes at offset 1024, call `set_len(n)`, then
;; call `next_token` repeatedly.

(module
  (memory (export "memory") 256)

  (data (i32.const 0)
    "\02\1eas" "\06\1fassume" "\04\20bind" "\07\21context" "\04\22else"
    "\02\23fn" "\02\24if" "\06\25import" "\03\26let" "\04\27type"
    "\03\28use" "\03\29val" "\03\2avar" "\05\2bwhile" "\00")

  (global $SRC i32 (i32.const 1024))
  (global $pos (mut i32) (i32.const 0))
  (global $len (mut i32) (i32.const 0))

  (func (export "set_len") (param $n i32)
    (global.set $len (local.get $n))
    (global.set $pos (i32.const 0)))
  (func $reset (export "reset") (global.set $pos (i32.const 0)))

  ;; Byte at index $i, or -1 past the end — a plain memory load, no host call.
  (func $get (param $i i32) (result i32)
    (if (result i32) (i32.lt_u (local.get $i) (global.get $len))
      (then (i32.load8_u (i32.add (global.get $SRC) (local.get $i))))
      (else (i32.const -1))))

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

  (func $skip_trivia
    (local $c i32) (local $d i32)
    (block $done
      (loop $again
        (local.set $c (call $get (global.get $pos)))
        (if (call $is_ws (local.get $c))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (br $again)))
        (if (i32.eq (local.get $c) (i32.const 35))
          (then
            (global.set $pos (i32.add (global.get $pos) (i32.const 1)))
            (block $cdone
              (loop $cloop
                (local.set $d (call $get (global.get $pos)))
                (if (i32.or (i32.lt_s (local.get $d) (i32.const 0)) (i32.eq (local.get $d) (i32.const 10)))
                  (then (br $cdone)))
                (global.set $pos (i32.add (global.get $pos) (i32.const 1)))
                (br $cloop)))
            (br $again)))
        (br $done))))

  (func $kw_lookup (param $start i32) (param $len i32) (result i32)
    (local $p i32) (local $klen i32) (local $kid i32) (local $j i32) (local $ok i32)
    (local.set $p (i32.const 0))
    (block $done
      (loop $next
        (local.set $klen (i32.load8_u (local.get $p)))
        (if (i32.eqz (local.get $klen)) (then (br $done)))
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
                      (i32.load8_u (i32.add (global.get $SRC) (i32.add (local.get $start) (local.get $j)))))
                  (then (local.set $ok (i32.const 0)) (br $cmpdone)))
                (local.set $j (i32.add (local.get $j) (i32.const 1)))
                (br $cmp)))
            (if (local.get $ok) (then (return (local.get $kid))))))
        (local.set $p (i32.add (local.get $p) (i32.add (i32.const 2) (local.get $klen))))
        (br $next)))
    (i32.const 44))

  (func $lex_number (param $signed i32) (result i32)
    (local $i i32) (local $s0 i32) (local $s1 i32) (local $s2 i32) (local $token i32)
    (local.set $i (i32.add (global.get $pos) (local.get $signed)))
    (block $ddone
      (loop $dloop
        (if (call $is_digit (call $get (local.get $i)))
          (then (local.set $i (i32.add (local.get $i) (i32.const 1))) (br $dloop)))
        (br $ddone)))
    (local.set $token (i32.const 50))
    (local.set $s0 (call $get (local.get $i)))
    (local.set $s1 (call $get (i32.add (local.get $i) (i32.const 1))))
    (local.set $s2 (call $get (i32.add (local.get $i) (i32.const 2))))
    (if (i32.eqz (local.get $signed))
      (then
        (if (i32.eq (local.get $s0) (i32.const 117))
          (then
            (if (i32.and (i32.eq (local.get $s1) (i32.const 51)) (i32.eq (local.get $s2) (i32.const 50)))
              (then (local.set $token (i32.const 45)) (local.set $i (i32.add (local.get $i) (i32.const 3))))
              (else
                (if (i32.and (i32.eq (local.get $s1) (i32.const 54)) (i32.eq (local.get $s2) (i32.const 52)))
                  (then (local.set $token (i32.const 47)) (local.set $i (i32.add (local.get $i) (i32.const 3))))
                  (else (local.set $token (i32.const 49)) (local.set $i (i32.add (local.get $i) (i32.const 1))))))))
          (else
            (if (i32.eq (local.get $s0) (i32.const 105))
              (then
                (if (i32.and (i32.eq (local.get $s1) (i32.const 51)) (i32.eq (local.get $s2) (i32.const 50)))
                  (then (local.set $token (i32.const 46)) (local.set $i (i32.add (local.get $i) (i32.const 3))))
                  (else
                    (if (i32.and (i32.eq (local.get $s1) (i32.const 54)) (i32.eq (local.get $s2) (i32.const 52)))
                      (then (local.set $token (i32.const 48)) (local.set $i (i32.add (local.get $i) (i32.const 3))))))))))))
      (else
        (if (i32.eq (local.get $s0) (i32.const 105))
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
    (if (i32.lt_s (local.get $c) (i32.const 0)) (then (return (i32.const 0))))
    (local.set $c1 (call $get (i32.add (global.get $pos) (i32.const 1))))
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
    (if (call $is_digit (local.get $c))
      (then (return (call $lex_number (i32.const 0)))))
    (if (i32.eq (local.get $c) (i32.const 45))
      (then
        (if (call $is_digit (local.get $c1)) (then (return (call $lex_number (i32.const 1)))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 9))))
    (if (i32.eq (local.get $c) (i32.const 33))
      (then
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 23))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 1))))
    (if (i32.eq (local.get $c) (i32.const 58))
      (then
        (if (i32.eq (local.get $c1) (i32.const 58))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 24))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 12))))
    (if (i32.eq (local.get $c) (i32.const 60))
      (then
        (if (i32.eq (local.get $c1) (i32.const 60))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 25))))
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 26))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 14))))
    (if (i32.eq (local.get $c) (i32.const 61))
      (then
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 27))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 15))))
    (if (i32.eq (local.get $c) (i32.const 62))
      (then
        (if (i32.eq (local.get $c1) (i32.const 62))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 29))))
        (if (i32.eq (local.get $c1) (i32.const 61))
          (then (global.set $pos (i32.add (global.get $pos) (i32.const 2))) (return (i32.const 28))))
        (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 16))))
    (if (i32.eq (local.get $c) (i32.const 37)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 2))))
    (if (i32.eq (local.get $c) (i32.const 38)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 3))))
    (if (i32.eq (local.get $c) (i32.const 40)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 4))))
    (if (i32.eq (local.get $c) (i32.const 41)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 5))))
    (if (i32.eq (local.get $c) (i32.const 42)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 6))))
    (if (i32.eq (local.get $c) (i32.const 43)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 7))))
    (if (i32.eq (local.get $c) (i32.const 44)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 8))))
    (if (i32.eq (local.get $c) (i32.const 46)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 10))))
    (if (i32.eq (local.get $c) (i32.const 47)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 11))))
    (if (i32.eq (local.get $c) (i32.const 59)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 13))))
    (if (i32.eq (local.get $c) (i32.const 91)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 17))))
    (if (i32.eq (local.get $c) (i32.const 93)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 18))))
    (if (i32.eq (local.get $c) (i32.const 94)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 19))))
    (if (i32.eq (local.get $c) (i32.const 123)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 20))))
    (if (i32.eq (local.get $c) (i32.const 124)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 21))))
    (if (i32.eq (local.get $c) (i32.const 125)) (then (global.set $pos (i32.add (global.get $pos) (i32.const 1))) (return (i32.const 22))))
    (if (i32.eq (local.get $c) (i32.const 39))
      (then
        (if (i32.lt_s (local.get $c1) (i32.const 0)) (then (return (i32.const -1))))
        (if (i32.lt_u (local.get $c1) (i32.const 0x80))
          (then
            (if (i32.eq (local.get $c1) (i32.const 10)) (then (return (i32.const -1))))
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
    (if (i32.eq (local.get $c) (i32.const 34))
      (then
        (local.set $i (i32.add (global.get $pos) (i32.const 1)))
        (block $sdone
          (loop $sloop
            (local.set $d (call $get (local.get $i)))
            (if (i32.lt_s (local.get $d) (i32.const 0)) (then (return (i32.const -1))))
            (if (i32.eq (local.get $d) (i32.const 34))
              (then (global.set $pos (i32.add (local.get $i) (i32.const 1))) (return (i32.const 52))))
            (if (i32.eq (local.get $d) (i32.const 92))
              (then
                (local.set $e (call $get (i32.add (local.get $i) (i32.const 1))))
                (if (i32.or
                      (i32.or (i32.eq (local.get $e) (i32.const 34)) (i32.eq (local.get $e) (i32.const 92)))
                      (i32.or (i32.eq (local.get $e) (i32.const 110))
                              (i32.or (i32.eq (local.get $e) (i32.const 114)) (i32.eq (local.get $e) (i32.const 116)))))
                  (then (local.set $i (i32.add (local.get $i) (i32.const 2))) (br $sloop))
                  (else (return (i32.const -1))))))
            (if (i32.eq (local.get $d) (i32.const 10)) (then (return (i32.const -1))))
            (local.set $i (i32.add (local.get $i) (i32.const 1)))
            (br $sloop)))))
    (i32.const -1)))
