(module
 (rec (type $Src (array (mut i8))) (type $Dst (array (mut i8))))
 (global $flag (mut i32) (i32.const 1))
 (func (export "_start")
  (local $source (ref null $Src)) (local $dest (ref null $Dst))
  (local $i i32) (local $bound i32)
  ;; setup
  (local.set $source (array.new_fixed $Src 1 (i32.const 104)))
  (local.set $dest (array.new_default $Dst (i32.const 1)))
  (local.set $i (i32.const 0))
  ;; before
  (loop $copy
   (if (i32.lt_u (local.get $i)
     ;; bound
     (i32.const 1))
    (then
     (array.set $Dst (local.get $dest) (local.get $i)
      (array.get_u $Src (local.get $source) (local.get $i)))
     (local.set $i (i32.add (local.get $i) (i32.const 1)))
     (br $copy))))
  ;; checks
 ))
