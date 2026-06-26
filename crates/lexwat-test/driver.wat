;; driver.wat — a tiny module that owns the source bytes and *exports* next_byte.
;;
;; Merge it into the lexer (which *imports* next_byte) and the lexer's per-byte
;; host call becomes an ordinary in-module wasm call:
;;
;;   wasm-merge lex_nomem.wat lexer  driver.wat env  -o merged.wasm
;;
;; (the lexer imports "env"."next_byte", so the driver is given the merge-name
;; "env" and wasm-merge wires the import to this export). The host writes the
;; source at offset 0 of the merged module's memory, calls set_len(n), then calls
;; the lexer's next_token — with zero per-byte boundary crossings.

(module
  (memory (export "memory") 256)
  (global $cur (mut i32) (i32.const 0))
  (global $len (mut i32) (i32.const 0))

  (func (export "set_len") (param $n i32)
    (global.set $len (local.get $n))
    (global.set $cur (i32.const 0)))

  (func (export "next_byte") (result i32)
    (local $c i32)
    (if (result i32) (i32.lt_u (global.get $cur) (global.get $len))
      (then
        (local.set $c (i32.load8_u (global.get $cur)))
        (global.set $cur (i32.add (global.get $cur) (i32.const 1)))
        (local.get $c))
      (else (i32.const -1)))))
