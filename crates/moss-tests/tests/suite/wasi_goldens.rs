//! The `tests/wasi` programs that have a golden, `tests/wasi/stdout/NAME.txt`
//! (the semantics probes that were `bootstrap/examples/`), print it: compiled
//! by the self-hosted compiler and run, and run by the oracle. Both must
//! exit 0, except for the programs listed below. Each golden is the
//! oracle's output (`MOSS_BLESS=1` rewrites them from it).
//!
//! Whether the two front ends accept each program at all is checked by
//! `agreement`; a program the self-hosted compiler rejects is listed in
//! `NATIVE_REJECTS` here and in `agreement::GAPS` there.

use moss_tests::{oracle, read, repo, wasmtime, wasm_file, TRAP};

/// Programs whose run ends in a trap, on both: a checked cast fails.
const TRAPS: &[(&str, &str)] = &[
    ("copy_capture", "G keeps the X it captured (I32), so the cast to Ref[Boxed=F64] fails (D84 6)"),
    ("saved_generic_rebinding", "the box made at U keeps U's meaning, so the cast to Ref[Boxed=F64] fails"),
    ("saved_type_rebinding", "the box made at I32 keeps I32, so the cast to Ref[Boxed=Saved] fails"),
];

/// Programs the self-hosted compiler rejects, so only the oracle runs them
/// (`agreement::GAPS` lists each).
const NATIVE_REJECTS: &[(&str, &str)] = &[
    ("const_kept_output", "AUDIT-CHANGES #20: the self-hosted compiler rejects every use of a defined val"),
    ("sameneed_copy", "AUDIT-CHANGES #51: the self-hosted compiler rejects the copy"),
    ("saved_type_rebinding", "the self-hosted compiler rejects `main`"),
];

/// Programs the self-hosted compiler runs to another output: (name, what
/// it prints, why).
const NATIVE_PRINTS: &[(&str, &str, &str)] = &[
    ("sameneed_meaning", "1\n", "AUDIT-CHANGES #51: the self-hosted compiler captures X.m; Audit links it"),
    ("sameneed_meaning_agree", "1\n", "AUDIT-CHANGES #51: as sameneed_meaning"),
];

fn listed<'a>(list: &[(&'a str, &'a str)], name: &str) -> bool {
    list.iter().any(|(n, _)| *n == name)
}

fn names() -> Vec<String> {
    let mut names: Vec<String> = std::fs::read_dir(repo().join("tests/wasi/stdout"))
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|x| x == "txt"))
        .map(|p| p.file_stem().unwrap().to_str().unwrap().to_string())
        .collect();
    names.sort();
    names
}

#[test]
fn the_probes_print_their_goldens() {
    let names = names();
    let code = |name: &str| if listed(TRAPS, name) { TRAP } else { 0 };
    let results = super::parallel::map(&names, |name| {
        let entry = format!("tests/wasi/{name}.moss");
        let native = (!listed(NATIVE_REJECTS, name)).then(|| {
            let compiled = super::errors::compile_raw(&entry);
            assert!(compiled.is_module(), "{entry} did not compile: {}", compiled.text());
            wasmtime(&wasm_file(&compiled.stdout), None, &[])
        });
        (native, oracle(&entry, &[]))
    });
    let mut failures = Vec::new();
    for (name, (native, reference)) in names.iter().zip(results) {
        let golden_path = format!("tests/wasi/stdout/{name}.txt");
        if std::env::var_os("MOSS_BLESS").is_some() && reference.code == code(name) {
            std::fs::write(repo().join(&golden_path), &reference.stdout).unwrap();
        }
        let golden = read(&golden_path);
        if reference.code != code(name) || reference.text() != golden {
            failures.push(format!("the oracle on {name}: exit {}, {:?}: {}", reference.code, reference.text(), reference.stderr.trim()));
        }
        if let Some(native) = native {
            let expected = NATIVE_PRINTS.iter().find(|(n, ..)| n == name).map_or(golden.as_str(), |(_, p, _)| *p);
            if native.code != code(name) || native.text() != expected {
                failures.push(format!("the self-hosted compiler on {name}: exit {}, {:?}", native.code, native.text()));
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

/// Every golden has its program, and every listed name has its golden.
#[test]
fn every_golden_belongs_to_a_program() {
    let names = names();
    for name in &names {
        assert!(repo().join(format!("tests/wasi/{name}.moss")).is_file(), "tests/wasi/stdout/{name}.txt has no program");
    }
    let lists = TRAPS.iter().chain(NATIVE_REJECTS).map(|(n, _)| *n).chain(NATIVE_PRINTS.iter().map(|(n, ..)| *n));
    for name in lists {
        assert!(names.iter().any(|n| n == name), "{name} is listed but has no golden");
    }
}
