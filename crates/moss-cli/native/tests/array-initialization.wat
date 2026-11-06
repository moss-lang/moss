(module
 (type $Literal (array (mut i8)))
 (global $runtime (mut i32) (i32.const 104))
 (global $index (mut i32) (i32.const 0))
 (func (export "set_runtime") (param $value i32) (param $at i32)
  (global.set $runtime (local.get $value))
  (global.set $index (local.get $at)))
 (func $literal (result (ref $Literal))
  (array.new_fixed $Literal 1 (global.get $runtime)))
 (func $zero (result (ref $Literal))
  (array.new_fixed $Literal 1 (i32.const 0)))
 (func $copy (param $source (ref null $Literal)) (result (ref $Literal))
  (local $dest (ref $Literal))
  (local $i i32)
  ;; This fill becomes array.new_fixed before initialization analysis. It must
  ;; remain a fresh dynamic object, even though its initial bytes equal $zero.
  (local.set $dest (array.new_default $Literal (i32.const 1)))
  (local.set $i (i32.const 0))
  (loop $copy
   (if (i32.lt_u (local.get $i) (i32.const 1))
    (then
     (array.set $Literal (local.get $dest) (local.get $i)
      (array.get_u $Literal (local.get $source) (local.get $i)))
     (local.set $i (i32.add (local.get $i) (i32.const 1)))
     (br $copy))))
  (local.get $dest))
 (func (export "_start")
  (if (i32.ne
   (array.get_u $Literal (call $copy (call $literal)) (i32.const 0))
   (i32.const 104)) (then unreachable))
  (if (i32.ne
   (array.get_u $Literal (call $zero) (global.get $index))
   (i32.const 0)) (then unreachable))
 ))
