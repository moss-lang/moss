;; A stand-in for the Moss compiler, for the flake's `cli` check: run as
;; `MOSS_COMPILER`, it ignores its arguments and writes a module that
;; every engine refuses, because its one function type has 1001
;; parameters, one more than the limit wasmparser (so Wasmtime) imposes.
;; The specification sets no such limit, and Binaryen reads and optimizes
;; the module (the function is exported so that it keeps it), so only
;; validating catches it.
(module
  (import "wasi_snapshot_preview1" "fd_write"
    (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (memory (export "memory") 1)
  ;; The one iovec: the module's 1034 bytes, from address 64.
  (data (i32.const 0) "\40\00\00\00\0a\04\00\00")
  ;; Header, type section of 1006 bytes, one `func` type, 1001 params...
  (data (i32.const 64) "\00asm\01\00\00\00\01\ee\07\01\60\e9\07")
  ;; ...(1001 bytes of i32, filled in below), no results; one function of
  ;; that type, exported so that Binaryen keeps it, whose body is empty.
  (data (i32.const 1080) "\00\03\02\01\00\07\05\01\01f\00\00\0a\04\01\02\00\0b")
  (func (export "_start")
    (memory.fill (i32.const 79) (i32.const 0x7f) (i32.const 1001))
    (drop (call $fd_write (i32.const 1) (i32.const 0) (i32.const 1) (i32.const 16)))))
