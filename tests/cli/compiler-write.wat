;; A stand-in compiler which tries to create "created" in its WASI preopen.
(module
  (import "wasi_snapshot_preview1" "path_open"
    (func $open (param i32 i32 i32 i32 i32 i64 i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "proc_exit" (func $exit (param i32)))
  (memory (export "memory") 1)
  (data (i32.const 0) "created")
  (func (export "_start")
    (call $exit
      (call $open
        (i32.const 3) (i32.const 0) (i32.const 0) (i32.const 7)
        (i32.const 1) (i64.const 64) (i64.const 0) (i32.const 0) (i32.const 1024)))))
