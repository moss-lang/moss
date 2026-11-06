(module
 (rec
  (type $Literal (array (mut i8)))
  (type $Mutable (array (mut i8))))
 (func (export "literal") (param $index i32) (result i32)
  (array.get_u $Literal
   (array.new_fixed $Literal 3 (i32.const 104) (i32.const 105) (i32.const 33))
   (local.get $index)))
 (func (export "mutable") (param $index i32) (param $value i32) (result i32)
  (local $a (ref $Mutable))
  (local.set $a (array.new $Mutable (i32.const 9) (i32.const 3)))
  (array.set $Mutable (local.get $a) (local.get $index) (local.get $value))
  (array.get_u $Mutable (local.get $a)
   (i32.rem_u (i32.add (local.get $index) (i32.const 1)) (i32.const 3)))))
