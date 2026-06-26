;; driver2.wat — drives the whole lex in one call. It *imports* the lexer's
;; next_token and *exports* next_byte (so, merged with the lexer, both directions
;; are internal wasm calls). It has two memories: $bytes (input) and $toks
;; (output token ids, one i32 each). `run()` loops next_token until the stream
;; ends, writing every id into $toks, and returns the count.
;;
;;   wasm-merge lex_nomem.wat lexer  driver2.wat env  -o merged2.wasm \
;;       --enable-multimemory
;;
;; Host: write source into $bytes, call reset + set_len(n), call run(), then read
;; `count` i32s out of $toks. One boundary crossing for the whole token stream.

(module
  (import "lexer" "next_token" (func $next_token (result i32)))

  (memory $bytes (export "bytes") 256)
  (memory $toks (export "toks") 256)

  (global $cur (mut i32) (i32.const 0))
  (global $len (mut i32) (i32.const 0))

  (func (export "set_len") (param $n i32)
    (global.set $len (local.get $n))
    (global.set $cur (i32.const 0)))

  ;; next_byte: read the next byte of $bytes, or -1 at end.
  (func (export "next_byte") (result i32)
    (local $c i32)
    (if (result i32) (i32.lt_u (global.get $cur) (global.get $len))
      (then
        (local.set $c (i32.load8_u $bytes (global.get $cur)))
        (global.set $cur (i32.add (global.get $cur) (i32.const 1)))
        (local.get $c))
      (else (i32.const -1))))

  ;; run the entire lex, storing each token id into $toks; return the count.
  (func (export "run") (result i32)
    (local $n i32) (local $id i32)
    (loop $l
      (local.set $id (call $next_token))
      (i32.store $toks (i32.shl (local.get $n) (i32.const 2)) (local.get $id))
      (local.set $n (i32.add (local.get $n) (i32.const 1)))
      (br_if $l (i32.gt_s (local.get $id) (i32.const 0))))
    (local.get $n)))
