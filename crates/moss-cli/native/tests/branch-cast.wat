(module
 (type $A (array (mut i8)))
 (func $literal (result (ref $A)) (array.new_fixed $A 1 (i32.const 65)))
 (func (export "readUnknown") (param $opaque anyref) (result i32)
  (array.get_u $A
   (block $yes (result (ref $A))
    (drop (br_on_cast $yes anyref (ref $A) (local.get $opaque)))
    (unreachable))
   (i32.const 0)))
 (func (export "_start") (drop (call $literal))))
