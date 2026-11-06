(module
 (type $Bytes (array (mut i8)))
 (global $literal (ref $Bytes)
  (array.new_fixed $Bytes 2 (i32.const 104) (i32.const 105)))
 (global $choice (mut i32) (i32.const 0))
 (global $observed (mut i32) (i32.const 0))
 (func $read (param $tag i32) (param $literal (ref null $Bytes))
   (param $dynamic (ref null $Bytes)) (param $index i32) (result i32)
  (array.get_u $Bytes
   (select (result (ref null $Bytes)) (local.get $literal) (local.get $dynamic)
    (i32.eq (local.get $tag) (i32.const 7)))
   (local.get $index)))
 (func (export "_start")
  (local $q (tuple i32 (ref null $Bytes) (ref null $Bytes)))
  ;; before
  (local.set $q
   (if (result i32 (ref null $Bytes) (ref null $Bytes)) (global.get $choice)
    (then (tuple.make 3 (i32.const 7) (global.get $literal) (ref.null none)))
    (else (tuple.make 3 (i32.const 9) (ref.null none)
      (array.new $Bytes (i32.const 9) (i32.const 2))))))
  ;; observe
  (if (i32.ne
   (call $read (tuple.extract 3 0 (local.get $q))
    (tuple.extract 3 1 (local.get $q)) (tuple.extract 3 2 (local.get $q)) (i32.const 0))
   (i32.const 9)) (then unreachable))
 ))
