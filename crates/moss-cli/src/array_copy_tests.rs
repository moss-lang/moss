use super::*;

const COPY: &str = include_str!("../native/tests/array-copy.wat");

fn copy_agrees(source: &str, success: bool) -> Vec<String> {
    let mut texts = Vec::new();
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(source);
        assert_eq!(execute(&module.bytes()), success);
        module.lower_with(storage);
        assert_eq!(execute(&module.bytes()), success, "{storage:?}");
        texts.push(module.text());
    }
    texts
}

#[test]
fn constant_copy_preserves_constructor_tee_and_final_counter_and_drops_builder() {
    for text in copy_agrees(COPY, true) {
        assert!(!text.contains("(loop "), "{text}");
        assert!(!text.contains("array.set"), "{text}");
        assert!(
            !text.contains("array.new_fixed"),
            "dead builder allocation remains: {text}"
        );
        assert!(!text.contains("array.new_default"), "{text}");
    }
}

#[test]
fn copied_snapshot_survives_later_builder_mutation() {
    let source = COPY.replace(";; checks", r#"
      (array.set $B (local.get $source) (i32.const 0) (i32.const 33))
      (if (i32.ne (array.get $B (local.get $source) (i32.const 0)) (i32.const 33)) (then unreachable))
      (if (i32.ne (array.get $A (local.get $dest) (i32.const 0)) (i32.const 104)) (then unreachable))
    "#);
    for text in copy_agrees(&source, true) {
        assert!(!text.contains("(loop "), "{text}");
        assert!(text.contains("array.set"), "builder mutation must remain");
    }
}

#[test]
fn observed_destination_and_effectful_or_counter_dependent_bounds_keep_loop() {
    let cases = [
        COPY.replace(
            ";; before-copy",
            "(local.set $observed (array.get $A (local.get $dest) (i32.const 0)))",
        )
        .replace(";; checks", "(if (local.get $observed) (then unreachable))"),
        COPY.replace(
            "(local.get $length))\n    (then",
            "(local.tee $observed (local.get $length)))\n    (then",
        )
        .replace(
            ";; checks",
            "(if (i32.ne (local.get $observed) (i32.const 2)) (then unreachable))",
        ),
    ];
    for source in cases {
        for text in copy_agrees(&source, true) {
            assert!(text.contains("(loop "), "{text}");
        }
    }
    let source = COPY.replace(
        "(local.get $length))\n    (then",
        "(i32.add (local.get $index) (i32.const 1)))\n    (then",
    );
    for text in copy_agrees(&source, false) {
        assert!(text.contains("(loop "), "{text}");
    }
}

#[test]
fn copying_signed_packed_elements_preserves_sign_extension() {
    let source = COPY
        .replace("$B (array (mut i32))", "$B (array (mut i8))")
        .replace("array.get $B", "array.get_s $B")
        .replace("(i32.const 104)", "(i32.const -1)")
        .replace("(i32.const 105)", "(i32.const -128)");
    for text in copy_agrees(&source, true) {
        assert!(!text.contains("(loop "), "{text}");
    }
}

fn writer_copy() -> String {
    let helper = r#"
 (func $write (param $array (ref $B)) (param $at i32) (param $pick i32)
   (param $left i32) (param $right i32) (param $last i32) (result (ref $B))
  (array.set $B (local.get $array) (local.get $at)
   (select (local.get $left) (local.get $right) (local.get $pick)))
  (array.set $B (local.get $array) (i32.add (local.get $at) (i32.const 1))
   (local.get $last))
  (local.get $array))
"#;
    COPY.replace(" (func (export", &format!("{helper} (func (export"))
        .replace(
            "(array.new_fixed $B 2 (i32.const 104) (i32.const 105))",
            "(call $write (array.new_default $B (i32.const 2)) (i32.const 0) (i32.const 1) (i32.const 104) (i32.const 33) (i32.const 105))",
        )
}

#[test]
fn bounded_array_writers_with_constant_selectors_expose_initialization() {
    for text in copy_agrees(&writer_copy(), true) {
        assert!(!text.contains("array.set"), "{text}");
        assert!(!text.contains("(loop "), "{text}");
    }
}

#[test]
fn inlined_writer_operands_keep_effects_and_out_of_bounds_traps() {
    let source = writer_copy()
        .replace(" (func $write", " (global $effects (mut i32) (i32.const 0)) (func $write")
        .replace("(i32.const 0) (i32.const 1) (i32.const 104)", "(i32.const 0) (block (result i32) (global.set $effects (i32.const 7)) (i32.const 1)) (i32.const 104)")
        .replace(";; checks", "(if (i32.ne (global.get $effects) (i32.const 7)) (then unreachable))");
    copy_agrees(&source, true);
    let trapped = writer_copy().replace(
        "(i32.const 0) (i32.const 1) (i32.const 104)",
        "(i32.const 1) (i32.const 1) (i32.const 104)",
    );
    copy_agrees(&trapped, false);
}

#[test]
fn copied_snapshot_keeps_its_value_after_inlined_builder_writes() {
    let source = writer_copy().replace(";; checks", r#"
      (drop (call $write (local.get $source) (i32.const 0) (i32.const 1)
        (i32.const 33) (i32.const 33) (i32.const 33)))
      (if (i32.ne (array.get $B (local.get $source) (i32.const 0)) (i32.const 33)) (then unreachable))
      (if (i32.ne (array.get $A (local.get $dest) (i32.const 0)) (i32.const 104)) (then unreachable))
    "#);
    copy_agrees(&source, true);
}
