use super::*;
use crate::wasmtime::{self, Engine, Module, Wasi};
use std::ffi::{CStr, CString};

const ARRAYS: &str = include_str!("../native/tests/arrays.wat");

struct Parsed(BinaryenModuleRef);
impl Parsed {
    fn new(text: &str) -> Self {
        let source = CString::new(text).unwrap();
        let module =
            Self(unsafe { BinaryenModuleParseWithFeatures(source.as_ptr(), features().unwrap()) });
        assert!(!module.0.is_null());
        assert!(unsafe { BinaryenModuleValidate(module.0) });
        module
    }
    fn lower(&self) -> usize {
        self.lower_with(StaticBytes::Memory)
    }
    fn lower_with(&self, storage: StaticBytes) -> usize {
        let count = unsafe {
            match storage {
                StaticBytes::Memory => moss_pool_static_bytes(self.0),
                StaticBytes::Globals => moss_pool_static_byte_globals(self.0),
            }
        };
        assert_ne!(count, usize::MAX);
        assert!(unsafe { BinaryenModuleValidate(self.0) });
        count
    }
    fn text(&self) -> String {
        unsafe {
            let ptr = BinaryenModuleAllocateAndWriteText(self.0);
            let text = CStr::from_ptr(ptr).to_str().unwrap().to_owned();
            free(ptr.cast());
            text
        }
    }
    fn bytes(&self) -> Vec<u8> {
        unsafe {
            let result = BinaryenModuleAllocateAndWrite(self.0, ptr::null());
            let bytes =
                slice::from_raw_parts(result.binary.cast::<u8>(), result.binary_bytes).to_vec();
            free(result.binary);
            if !result.source_map.is_null() {
                free(result.source_map.cast());
            }
            bytes
        }
    }
}
impl Drop for Parsed {
    fn drop(&mut self) {
        unsafe { BinaryenModuleDispose(self.0) }
    }
}

fn execute(bytes: &[u8]) -> bool {
    let engine = Engine::new().unwrap();
    let module = Module::new(&engine, bytes).unwrap();
    let wasi = Wasi::new(&["test".to_owned()]).unwrap();
    wasmtime::run(&engine, &module, wasi).is_ok()
}

fn agrees(text: &str, success: bool) -> String {
    let module = Parsed::new(text);
    assert_eq!(execute(&module.bytes()), success, "source behavior");
    assert!(module.lower() > 0);
    let output = module.text();
    assert_eq!(execute(&module.bytes()), success, "lowered behavior");
    output
}

// Literal pooling must not lose its type isolation during preparation. Count
// types used by reads, not declarations: merged types can leave old declarations
// behind as members of the original recursive group.
#[test]
fn optimizer_keeps_unrelated_literal_and_mutable_array_types_distinct() {
    let source = Parsed::new(include_str!("../native/tests/distinct-arrays.wat"));
    let raw = source.bytes();
    for level in ["0", "1", "2", "3", "4", "s", "z"] {
        let mut optimized = optimize_wasm(raw.clone(), parse_opt(level).unwrap()).unwrap();
        let module = Parsed(unsafe {
            BinaryenModuleReadWithFeatures(
                optimized.as_mut_ptr().cast(),
                optimized.len(),
                features().unwrap(),
            )
        });
        assert!(!module.0.is_null());
        assert!(unsafe { BinaryenModuleValidate(module.0) });
        let text = module.text();
        let read_types: std::collections::BTreeSet<_> = text
            .lines()
            .filter_map(|line| {
                line.trim()
                    .strip_prefix("(array.get_u ")
                    .and_then(|rest| rest.split_whitespace().next())
            })
            .collect();
        if ["0", "1"].contains(&level) {
            assert_eq!(read_types.len(), 2, "-O{level}: {text}");
        } else {
            assert!(
                text.contains("ref.i31") && text.contains("(data "),
                "-O{level}: literal pooling was blocked by mutable arrays: {text}"
            );
            assert!(
                !read_types.is_empty(),
                "-O{level}: mutable reads disappeared: {text}"
            );
        }
    }
}

#[test]
fn arrays_normalize_pool_and_preserve_mutable_and_dynamic_backing() {
    let output = agrees(ARRAYS, true);
    assert!(output.contains("hi"));
    assert!(output.contains("ref.i31"));
    let module = Parsed::new(ARRAYS);
    assert!(module.lower() > 0);
    unsafe {
        let name = CString::new("literal").unwrap();
        let f = BinaryenGetFunction(module.0, name.as_ptr());
        // The constructor may keep a local assignment, but no array allocation.
        assert!(!module
            .text()
            .split("(func $literal")
            .nth(1)
            .unwrap()
            .split("(func ")
            .next()
            .unwrap()
            .contains("array.new"));
        assert!(!f.is_null());
    }
}

#[test]
fn array_reads_preserve_null_and_out_of_bounds_traps() {
    for check in [
        "(drop (array.get_u $Literal (call $literal) (i32.const 2)))",
        "(drop (array.get_u $Literal (call $literal) (i32.const -1)))",
        "(drop (array.get_u $Literal (ref.null none) (i32.const 0)))",
        "(drop (array.len (ref.null none)))",
    ] {
        agrees(&ARRAYS.replace(";; checks", check), false);
    }
}

fn both(source: &str, success: bool, pooled: bool) {
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(source);
        assert_eq!(execute(&module.bytes()), success, "source");
        assert_eq!(module.lower_with(storage) > 0, pooled, "{storage:?}");
        assert_eq!(execute(&module.bytes()), success, "lowered {storage:?}");
    }
}

#[test]
fn observers_escapes_and_post_initialization_mutation_prevent_pooling() {
    for extra in [
        r#"(export "exposed" (func $literal))"#,
        "(func $identity (result i32) (ref.eq (call $literal) (call $literal)))",
        "(func $test (param anyref) (result i32) (ref.test (ref $Literal) (local.get 0)))",
        "(func $erase (result anyref) (call $literal))",
        "(func $erase (local $p anyref) (local.set $p (call $literal)))",
        "(func $cast (param anyref) (result (ref $Literal)) (ref.cast (ref $Literal) (local.get 0)))",
        "(func $write (param (ref $Literal)) (array.set $Literal (local.get 0) (i32.const 0) (i32.const 88)))",
        "(elem declare func $literal) (func $ptr (drop (ref.func $literal)))",
    ] {
        let source = ARRAYS.replace(";; extras", extra);
        // These modules have no host imports and still execute unchanged.
        both(&source, true, false);
    }
    let imported = ARRAYS.replacen(
        " (memory",
        r#" (import "host" "observe" (func (param (ref null $Literal)))) (memory"#,
        1,
    );
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        assert_eq!(Parsed::new(&imported).lower_with(storage), 0);
    }
}

#[test]
fn partially_observed_or_effectful_initialization_is_not_pooled() {
    let store = "  (array.set $Literal (local.get $a) (i32.const 1) (i32.const 105))";
    let observed = ARRAYS.replace(
        store,
        &format!("(drop (array.get_u $Literal (local.get $a) (i32.const 0))){store}"),
    );
    both(&observed, true, false);
    let effect = "(block (result i32) (global.set $effects (i32.add (global.get $effects) (i32.const 1))) (i32.const 105))";
    let source = ARRAYS
        .replace(";; extras", "(global $effects (mut i32) (i32.const 0))")
        .replacen("(i32.const 105)", effect, 1)
        .replace(
            ";; checks",
            "(if (i32.ne (global.get $effects) (i32.const 3)) (then unreachable))",
        );
    both(&source, true, false);
    let out_of_bounds = ARRAYS.replacen(
        "(array.set $Literal (local.get $a) (i32.const 1)",
        "(array.set $Literal (local.get $a) (i32.const 2)",
        1,
    );
    both(&out_of_bounds, false, false);
}

#[test]
fn tee_and_default_initialization_normalize_and_packed_values_truncate() {
    let source = ARRAYS.replace(
        "(local.set $a (array.new $Literal (i32.const 0) (i32.const 2)))\n  (array.set $Literal (local.get $a)",
        "(array.set $Literal (local.tee $a (array.new_default $Literal (i32.const 2)))",
    ).replacen("(i32.const 104)", "(i32.const 360)", 1);
    both(&source, true, true);
}

#[test]
fn globals_initialize_before_the_original_start_and_need_no_linear_memory() {
    let source = ARRAYS.replace(";; extras", r#"
      (global $started (mut i32) (i32.const 0))
      (func $oldstart
        (if (i32.ne (array.get_u $Literal (call $literal) (i32.const 0)) (i32.const 104)) (then unreachable))
        (global.set $started (i32.const 1)))
      (start $oldstart)
    "#).replace(";; checks", "(if (i32.ne (global.get $started) (i32.const 1)) (then unreachable))");
    let module = Parsed::new(&source);
    assert!(module.lower_with(StaticBytes::Globals) > 0);
    let text = module.text();
    assert!(text.contains("array.new_data"));
    assert!(!text.contains("moss.constant.bytes"));
    assert!(execute(&module.bytes()));
    let no_memory = r#"(module
      (type $A (array i8))
      (func $literal (result (ref $A)) (array.new_fixed $A 2 (i32.const 104) (i32.const 105)))
      (func (export "_start") (if (i32.ne (array.get_u $A (call $literal) (i32.const 1)) (i32.const 105)) (then unreachable))))"#;
    let module = Parsed::new(no_memory);
    assert_eq!(module.lower_with(StaticBytes::Globals), 1);
    assert!(!module.text().contains("(memory"));
    assert!(execute(&module.bytes()));
}

#[test]
fn empty_arrays_deduplicate_without_memory_or_data() {
    let source = r#"(module
      (type $A (array (mut i8)))
      (func $empty (result (ref $A)) (array.new_fixed $A 0))
      (func (export "_start") (if (array.len (call $empty)) (then unreachable))))"#;
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(source);
        assert_eq!(module.lower_with(storage), 1);
        assert!(execute(&module.bytes()));
        let text = module.text();
        assert!(!text.contains("(memory"));
        assert!(!text.contains("(data"));
        assert!(!text.contains("(start"));
    }
}

#[test]
fn unpacked_bytes_and_dynamic_wider_values_keep_their_values() {
    let source = ARRAYS
        .replace("(mut i8)", "(mut i32)")
        .replace("array.get_u", "array.get")
        .replace("array.get_s", "array.get")
        .replace("(i32.const -1)", "(i32.const 255)");
    both(&source, true, true);
    both(
        &source.replace("(i32.const 255)", "(i32.const 1000)"),
        true,
        true,
    );
}

#[test]
fn tuple_component_erasure_cannot_hide_identity_observers() {
    both(
        include_str!("../native/tests/tuple-erasure.wat"),
        true,
        false,
    );
}

#[test]
fn target_feature_and_memory_limits_decline_before_mutation() {
    let module = Parsed::new(ARRAYS);
    unsafe {
        BinaryenModuleSetFeatures(
            module.0,
            features().unwrap() & !BinaryenFeatureMultiMemory(),
        );
    }
    assert_eq!(module.lower(), 0);
    assert!(module.lower_with(StaticBytes::Globals) > 0);
    assert!(execute(&module.bytes()));
    let module = Parsed::new(ARRAYS);
    unsafe {
        BinaryenModuleSetFeatures(module.0, features().unwrap() & !BinaryenFeatureBulkMemory());
    }
    assert_eq!(module.lower_with(StaticBytes::Globals), 0);
    let full = ARRAYS.replace(";; extras", &"(memory 1)".repeat(99));
    assert_eq!(Parsed::new(&full).lower(), 0);
}

#[test]
fn signed_static_reads_and_duplicate_sites_preserve_packed_contents() {
    let source = ARRAYS.replace("(i32.const 105)", "(i32.const 255)")
      .replace(";; checks", "(if (i32.ne (array.get_s $Literal (call $literal) (i32.const 1)) (i32.const -1)) (then unreachable))");
    both(&source, true, true);
    let begin = ARRAYS.find(" (func $literal ").unwrap();
    let end = ARRAYS.find(" (func $dynamic ").unwrap();
    let duplicate = ARRAYS[begin..end].replace("$literal", "$duplicate");
    let source = ARRAYS
        .replace(";; extras", &duplicate)
        .replace(";; checks", "(drop (array.len (call $duplicate)))");
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(&source);
        assert_eq!(module.lower_with(storage), 2);
        assert_eq!(module.text().matches("hi").count(), 1);
        assert!(execute(&module.bytes()));
    }
}

#[test]
fn dispatch_factoring_keeps_unchosen_null_references_unobserved() {
    let source = r#"(module
      (type $A (array i8))
      (func $read (param i32) (param (ref null $A)) (param (ref null $A)) (param i32) (result i32)
        (if (result i32) (local.get 0)
          (then (array.get_s $A (local.get 1) (local.get 3)))
          (else (array.get_s $A (local.get 2) (local.get 3)))))
      (func $length (param i32) (param (ref null $A)) (param (ref null $A)) (result i32)
        (if (result i32) (local.get 0) (then (array.len (local.get 1))) (else (array.len (local.get 2)))))
      (func (export "_start")
        (if (i32.ne (call $read (i32.const 1) (array.new_fixed $A 1 (i32.const 98)) (ref.null none) (i32.const 0)) (i32.const 98)) (then unreachable))
        (if (i32.ne (call $read (i32.const 0) (ref.null none) (array.new_fixed $A 1 (i32.const 255)) (i32.const 0)) (i32.const -1)) (then unreachable))
        (if (i32.ne (call $length (i32.const 0) (ref.null none) (array.new_fixed $A 1 (i32.const 255))) (i32.const 1)) (then unreachable))))"#;
    let module = Parsed::new(source);
    assert!(execute(&module.bytes()));
    assert_eq!(unsafe { moss_simplify_array_dispatch(module.0) }, 2);
    assert!(unsafe { BinaryenModuleValidate(module.0) });
    let text = module.text();
    assert_eq!(text.matches("(array.get_s").count(), 1);
    assert_eq!(text.matches("(array.len").count(), 1);
    assert!(execute(&module.bytes()));
}

#[test]
fn gc_globals_support_private_typed_containers_without_exposing_literal_identity() {
    let source = include_str!("../native/tests/containers.wat");
    let module = Parsed::new(source);
    assert_eq!(
        module.lower(),
        0,
        "memory backing needs container representation rewriting"
    );
    assert!(execute(&module.bytes()));
    let module = Parsed::new(source);
    assert_eq!(module.lower_with(StaticBytes::Globals), 1);
    assert!(execute(&module.bytes()));
    for extra in [
        "(func $erase (param (ref $Row)) (result anyref) (local.get 0))",
        "(func $write (param (ref $Row)) (array.set $A (array.get $Row (local.get 0) (i32.const 0)) (i32.const 0) (i32.const 88)))",
        r#"(func (export "escape") (param (ref $Row)))"#,
    ] {
      let altered = source.replacen(" (func $literal", &format!("{extra} (func $literal"), 1);
      both(&altered, true, false);
    }
}

#[test]
fn unused_container_type_declarations_do_not_disable_pooling() {
    let source = ARRAYS.replacen(
        "(type $Mutable (array (mut i8))))",
        "(type $Mutable (array (mut i8))) (type $Unused (struct (field (ref null $Literal)))))",
        1,
    );
    both(&source, true, true);
}

#[test]
fn branch_cast_metadata_cannot_inject_untracked_gc_arrays() {
    both(include_str!("../native/tests/branch-cast.wat"), true, false);
}

#[test]
fn all_static_empty_length_helpers_still_trap_on_null() {
    let source = r#"(module
      (type $A (array i8))
      (func $empty (result (ref $A)) (array.new_fixed $A 0))
      (func $length (param (ref null $A)) (result i32) (array.len (local.get 0)))
      (func (export "_start") (drop (call $empty)) (drop (call $length (ref.null $A)))))"#;
    both(source, false, true);
}

#[test]
fn type_merging_requires_a_closed_reference_boundary() {
    let module = Parsed::new(ARRAYS);
    assert!(unsafe { moss_static_array_types_closed(module.0) });
    let source = ARRAYS.replace(
        ";; extras",
        r#"
      (type $Other (struct (field i32)))
      (func (export "foreign") (param anyref) (result i32) (ref.test (ref $Other) (local.get 0)))
    "#,
    );
    let module = Parsed::new(&source);
    assert!(!unsafe { moss_static_array_types_closed(module.0) });
    // Unrelated foreign-reference APIs do not disable pooling itself.
    assert_eq!(module.lower_with(StaticBytes::Globals), 1);
    assert!(execute(&module.bytes()));
}

const UNION_CALLS: &str = include_str!("../native/tests/array-union-calls.wat");
const UNION_LOCAL: &str = include_str!("../native/tests/array-union-local.wat");

fn coalesce(text: &str, success: bool) -> (usize, String) {
    let module = Parsed::new(text);
    assert_eq!(execute(&module.bytes()), success, "source union behavior");
    let count = unsafe { moss_coalesce_array_unions(module.0) };
    assert_ne!(count, usize::MAX);
    assert!(unsafe { BinaryenModuleValidate(module.0) });
    assert_eq!(
        execute(&module.bytes()),
        success,
        "contracted union behavior"
    );
    (count, module.text())
}

#[test]
fn private_union_parameters_results_and_locals_become_one_array_reference() {
    let (count, text) = coalesce(UNION_CALLS, true);
    assert!(count >= 3);
    assert!(!text.contains("tuple."), "{text}");
    assert!(!text.contains("(select"), "{text}");
    let (count, text) = coalesce(UNION_LOCAL, true);
    assert!(count >= 2);
    assert!(!text.contains("tuple."), "{text}");
    assert!(!text.contains("(select"), "{text}");
}

#[test]
fn single_variant_private_results_preserve_tag_null_and_effectful_consumers() {
    let source = r#"(module
      (type $Bytes (array (mut i8)))
      (global $effects (mut i32) (i32.const 0))
      (func $produce (result i32 (ref null $Bytes) (ref null $Bytes))
        (global.set $effects (i32.add (global.get $effects) (i32.const 1)))
        (tuple.make 3 (i32.const 9) (ref.null none)
          (array.new_fixed $Bytes 1 (i32.const 104))))
      (func (export "_start")
        (local $q (tuple i32 (ref null $Bytes) (ref null $Bytes)))
        (local.set $q (call $produce))
        (if (i32.ne (tuple.extract 3 0 (local.get $q)) (i32.const 9)) (then unreachable))
        (if (i32.eqz (ref.is_null (tuple.extract 3 1 (local.get $q)))) (then unreachable))
        (if (i32.ne
          (array.get_u $Bytes (tuple.extract 3 2 (local.get $q))
            (block (result i32)
              (global.set $effects (i32.add (global.get $effects) (i32.const 1)))
              (i32.const 0))) (i32.const 104)) (then unreachable))
        (if (i32.ne (global.get $effects) (i32.const 2)) (then unreachable))))"#;
    let (count, text) = coalesce(source, true);
    assert!(count > 0);
    let producer = text
        .split("(func $produce")
        .nth(1)
        .unwrap()
        .split("(func ")
        .next()
        .unwrap();
    assert!(!producer.contains("tuple.make"), "{text}");
    assert!(producer.contains("(result (ref $Bytes))"), "{text}");
}

#[test]
fn union_tag_and_inactive_lane_observers_prevent_representation_contraction() {
    for observe in [
        "(global.set $observed (tuple.extract 3 0 (local.get $q))) (if (i32.ne (global.get $observed) (i32.const 9)) (then unreachable))",
        "(if (i32.eqz (ref.is_null (tuple.extract 3 1 (local.get $q)))) (then unreachable))",
    ] {
        let source = UNION_LOCAL.replace(";; observe", observe);
        let (_, text) = coalesce(&source, true);
        assert!(text.contains("tuple.make"), "the observable tuple was contracted: {text}");
    }
}

#[test]
fn captures_before_tuple_initialization_keep_their_original_null_value() {
    let source = UNION_LOCAL
        .replace(
            ";; before",
            "(local $old (ref null $Bytes)) (local.set $old (tuple.extract 3 1 (local.get $q)))",
        )
        .replace("(global.get $choice)", "(i32.const 1)")
        .replace(
            "(tuple.extract 3 1 (local.get $q)) (tuple.extract 3 2 (local.get $q))",
            "(local.get $old) (tuple.extract 3 2 (local.get $q))",
        );
    let (_, text) = coalesce(&source, false);
    assert!(text.contains("tuple.make"));
}

#[test]
fn exported_and_indirect_union_signatures_are_not_changed() {
    for extra in [
        "(export \"raw\" (func $read))",
        "(elem declare func $read) (func $pointer (drop (ref.func $read)))",
    ] {
        let source = UNION_LOCAL.replace(
            " (func (export \"_start\")",
            &format!(" {extra} (func (export \"_start\")"),
        );
        let (count, _) = coalesce(&source, true);
        assert_eq!(count, 0);
    }
}

#[test]
fn nonnull_inactive_values_with_effects_are_not_removed() {
    let source = UNION_LOCAL
        .replace("(ref.null none)\n      (array.new $Bytes", "(array.new $Bytes (block (result i32) (global.set $observed (i32.const 1)) (i32.const 5)) (i32.const 2))\n      (array.new $Bytes")
        .replace(";; observe", "(if (i32.ne (global.get $observed) (i32.const 1)) (then unreachable))");
    let (_, text) = coalesce(&source, true);
    assert!(text.contains("tuple.make"));
}

#[test]
fn runtime_initialization_keeps_fresh_backing_but_does_not_block_static_pooling() {
    let source = include_str!("../native/tests/array-initialization.wat");
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(source);
        assert!(execute(&module.bytes()));
        assert!(module.lower_with(storage) > 0);
        assert!(execute(&module.bytes()));
        let text = module.text();
        assert!(
            text.contains("array.set"),
            "the dynamic copy was removed: {text}"
        );
        assert!(
            text.contains("(data"),
            "the fixed sites did not pool: {text}"
        );
    }
}

const COPY_SAFETY: &str = include_str!("../native/tests/array-copy-safety.wat");
fn copy_preserves(source: &str, success: bool) {
    for storage in [StaticBytes::Memory, StaticBytes::Globals] {
        let module = Parsed::new(source);
        assert_eq!(execute(&module.bytes()), success, "source copy");
        module.lower_with(storage);
        assert_eq!(
            execute(&module.bytes()),
            success,
            "normalized copy: {storage:?}"
        );
    }
}

#[test]
fn copy_normalization_requires_an_invariant_effect_free_bound() {
    let counter_bound = COPY_SAFETY.replace(
        ";; bound\n     (i32.const 1)",
        "(i32.add (local.get $i) (i32.const 1))",
    );
    copy_preserves(&counter_bound, false);
    let bound_effect = COPY_SAFETY
        .replace(
            ";; bound\n     (i32.const 1)",
            "(local.tee $bound (i32.const 1))",
        )
        .replace(
            ";; checks",
            "(if (i32.ne (local.get $bound) (i32.const 1)) (then unreachable))",
        );
    copy_preserves(&bound_effect, true);
}

#[test]
fn unknown_operator_fallback_does_not_evaluate_local_tees_twice() {
    let source = COPY_SAFETY
        .replace("(local.set $i (i32.const 0))", "(local.set $i (i32.const -2))")
        .replace(";; before", "(drop (i32.div_s (local.tee $i (i32.add (local.get $i) (i32.const 1))) (block (result i32)")
        .replace(";; checks", "(i32.const 1)))) (if (i32.ne (array.get_u $Dst (local.get $dest) (i32.const 0)) (i32.const 0)) (then unreachable))");
    copy_preserves(&source, true);
}

#[test]
fn skipped_constructors_cannot_supply_copy_facts_after_control_transfer() {
    for branch in [
        "(block $outer (block $inner (br_table $outer (i32.const 0))",
        "(block $outer (block $inner (drop (br_on_null $outer (ref.null any)))",
        "(block $outer (block $moss.copy.unknown-exit (br_if $outer (global.get $flag))",
    ] {
        let source = COPY_SAFETY
            .replace(";; setup", branch)
            .replace(";; before", "))");
        copy_preserves(&source, false);
    }
}

#[test]
fn oversized_constructors_invalidate_facts_from_effectful_operands() {
    let values = " (i32.const 0)".repeat(10000);
    let setup = format!("(drop (array.new_fixed $Src 10001 (block (result i32) (local.set $i (i32.const -1)) (i32.const 0)){values}))");
    let source = COPY_SAFETY.replace(";; before", &setup)
        .replace(";; checks", "(if (i32.ne (array.get_u $Dst (local.get $dest) (i32.const 0)) (i32.const 0)) (then unreachable))");
    copy_preserves(&source, true);
}

#[test]
fn copy_normalization_preserves_observation_signed_values_and_builder_reuse() {
    let observed = COPY_SAFETY
        .replace(";; before", "(local.set $bound (array.get_u $Dst (local.get $dest) (i32.const 0)))")
        .replace(";; checks", "(if (i32.ne (local.get $bound) (i32.const 0)) (then unreachable)) (if (i32.ne (array.get_u $Dst (local.get $dest) (i32.const 0)) (i32.const 104)) (then unreachable))");
    copy_preserves(&observed, true);
    let reused = COPY_SAFETY.replace(";; checks", "(array.set $Src (local.get $source) (i32.const 0) (i32.const 105)) (if (i32.ne (array.get_u $Dst (local.get $dest) (i32.const 0)) (i32.const 104)) (then unreachable))");
    copy_preserves(&reused, true);
    let signed = COPY_SAFETY
        .replace("(type $Dst (array (mut i8)))", "(type $Dst (array (mut i32)))")
        .replace("(i32.const 104)", "(i32.const 255)")
        .replace("array.get_u $Src", "array.get_s $Src")
        .replace(";; checks", "(if (i32.ne (array.get $Dst (local.get $dest) (i32.const 0)) (i32.const -1)) (then unreachable))");
    copy_preserves(&signed, true);
}

extern "C" {
    fn BinaryenModuleSetFeatures(module: BinaryenModuleRef, features: BinaryenFeatures);
    fn BinaryenModuleParseWithFeatures(
        text: *const c_char,
        features: BinaryenFeatures,
    ) -> BinaryenModuleRef;
    fn BinaryenModuleAllocateAndWriteText(module: BinaryenModuleRef) -> *mut c_char;
    fn BinaryenGetFunction(module: BinaryenModuleRef, name: *const c_char) -> *mut c_void;
}

#[path = "array_copy_tests.rs"]
mod array_copy_tests;
