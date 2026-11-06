//! The generated table files are what their generators write. If one
//! drifts from its generator the compiler still builds, and quietly
//! compiles the wrong instruction, so the files are checked, not just
//! produced.

use moss_tests::gensrc::{byte_class, bytes, chars, generated, wasm_ops};
use moss_tests::read;

#[test]
fn files_match_their_generators() {
    for (rel, text) in generated() {
        assert!(
            read(rel) == text,
            "regenerate {rel}: cargo run -p moss-tests --bin gensrc"
        );
    }
}

#[test]
fn chars_values_are_single_chars() {
    let all = chars();
    let mut names: Vec<&str> = all.iter().map(|(n, _)| n.as_str()).collect();
    names.sort();
    names.dedup();
    assert_eq!(names.len(), all.len(), "a char constant is named twice");
    let mut values: Vec<char> = all.iter().map(|(_, c)| *c).collect();
    values.sort();
    values.dedup();
    assert_eq!(values.len(), all.len(), "two char constants are one character");
}

#[test]
fn byte_constants_cover_every_value_once() {
    let all = bytes();
    assert_eq!(all.len(), 256);
    let signature = read("lib/byte.moss");
    let provider = read("lib/wasibyte.moss");
    for (index, (name, value)) in all.iter().enumerate() {
        assert_eq!(usize::from(*value), index);
        assert_eq!(*name, format!("x{index:02x}"));
        assert!(signature.contains(&format!("  val {name}: {};", byte_class(*value))));
        assert!(provider.contains(&format!("  bind byte::{name} = byte_{name}();")));
    }
    assert_eq!(signature.lines().filter(|line| line.trim_start().starts_with("val x")).count(), 256);
    assert!(!provider.contains("wasistd.moss"));
    assert!(!provider.contains("int.moss"));
}

/// Every function and val of the `Wasm` context is in the table, so the
/// self-hosted back end never meets one it cannot select.
#[test]
fn every_wasm_item_has_an_instruction() {
    let source = read("lib/wasm.moss");
    let mut declared = std::collections::BTreeSet::new();
    for line in source.lines().map(str::trim) {
        if let Some(rest) = line.strip_prefix("fn ") {
            if let Some((name, _)) = rest.split_once('(') {
                declared.insert(name.trim().to_string());
            }
        } else if let Some(rest) = line.strip_prefix("val ") {
            if let Some((name, _)) = rest.split_once(':') {
                declared.insert(name.trim().to_string());
            }
        }
    }
    assert!(!declared.is_empty());
    let ops: std::collections::BTreeSet<String> = wasm_ops().into_iter().map(|op| op.name).collect();
    // D75's two methods are instructions too, but which ones depends on
    // the layout of what a reference holds, so the back end selects them by
    // hand, where `bind Heap;` provides them, rather than from the table;
    // and so are D81's, which `Mutable` and `Arrays` provide. D79's are
    // `Addr`'s methods, which `Memories` provides: each is the instruction
    // of its name, at the receiver's memory.
    let missing: std::collections::BTreeSet<&str> = declared
        .iter()
        .filter(|d| !ops.contains(*d))
        .filter(|d| !d.strip_prefix("Addr.").is_some_and(|op| ops.contains(op)))
        .map(String::as_str)
        .collect();
    let expected: std::collections::BTreeSet<&str> = [
        ".boxed", "Ref.deref", "Ref.set", ".arrayed", "Array.length", "Array.get", "Array.set",
        "Array.fill", "Array.copy",
    ]
    .into();
    assert_eq!(missing, expected);
}
