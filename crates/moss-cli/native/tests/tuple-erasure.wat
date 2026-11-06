(module
 (type $A (array (mut i8)))
 (func $make (param $choose i32) (result anyref (ref $A))
  (if (result anyref (ref $A)) (local.get $choose)
   (then (tuple.make 2
     (array.new_fixed $A 1 (i32.const 65))
     (array.new_fixed $A 1 (i32.const 66))))
   (else (tuple.make 2
     (ref.null any)
     (array.new_fixed $A 1 (i32.const 66))))))
 (func (export "_start")
  (local $p (tuple anyref (ref $A)))
  (local $q (tuple anyref (ref $A)))
  (local.set $p (call $make (i32.const 1)))
  (local.set $q (call $make (i32.const 1)))
  (if (ref.eq
    (ref.cast eqref (tuple.extract 2 0 (local.get $p)))
    (ref.cast eqref (tuple.extract 2 0 (local.get $q))))
   (then unreachable))))
