(module
 (rec (type $B (array (mut i32))) (type $A (array (mut i32))))
 (func (export "_start")
  (local $source (ref $B)) (local $dest (ref $A))
  (local $index i32) (local $length i32) (local $observed i32)
  (local.set $source (array.new_fixed $B 2 (i32.const 104) (i32.const 105)))
  (local.set $dest
   (array.new_default $A (local.tee $length (array.len (local.get $source)))))
  ;; before-copy
  (loop $copy
   (if (i32.lt_u (local.get $index) (local.get $length))
    (then
     (array.set $A (local.get $dest) (local.get $index)
      (array.get $B (local.get $source) (local.get $index)))
     (local.set $index (i32.add (local.get $index) (i32.const 1)))
     (br $copy))))
  (if (i32.ne (local.get $index) (i32.const 2)) (then unreachable))
  (if (i32.ne (local.get $length) (i32.const 2)) (then unreachable))
  (if (i32.ne (array.get $A (local.get $dest) (i32.const 0)) (i32.const 104)) (then unreachable))
  (if (i32.ne (array.get $A (local.get $dest) (i32.const 1)) (i32.const 105)) (then unreachable))
  ;; checks
 ))
