//! Every program in `tests/errors` is refused, with golden diagnostics from
//! both front ends:
//!
//! - `tests/errors/compiler/NAME.txt`: the self-hosted compiler's report
//!   (one line per diagnostic, a letter and the names involved, D48), which
//!   is what `moss` prints;
//! - `tests/errors/oracle/NAME.txt`: the oracle's message, with the
//!   repository's path taken out, and a line and column for lexing and
//!   parsing errors.
//!
//! `MOSS_BLESS=1 cargo test -p moss-tests errors` rewrites the goldens.

use moss_tests::{oracle, read, repo, ORACLE_FRONT_END, ORACLE_REJECTED, ORACLE_UNSUPPORTED};

/// Programs the self-hosted compiler accepts and the oracle rejects: known
/// bugs of the self-hosted compiler, or (where the reason says so) a
/// recorded gap in the semantics. Each still compiles to a module, which
/// this suite checks, so a fix shows up here; then remove it from this
/// list and bless its golden.
pub const COMPILER_ACCEPTS: &[(&str, &str)] = &[
    (
        "tests/errors/bracket_method_receiver.moss",
        "AUDIT-CHANGES #5 (E26): the self-hosted compiler enters a bound \
         attached method at the bracket's receiver, and prints a wrong `1`",
    ),
    (
        "tests/errors/closure_partial_unbound_xyz.moss",
        "AUDIT-CHANGES #54, #97 and #130: the chosen private representative \
         is directly bound (tests/wasi/closure_partial_unbound_zyx.moss is accepted)",
    ),
    (
        "tests/errors/duplicate_field_binder.moss",
        "AUDIT-CHANGES #1 (E21): the self-hosted compiler lets a pattern \
         bind a name twice",
    ),
    (
        "tests/errors/expand_control_functor_const.moss",
        "AUDIT-CHANGES #48: NoExpandingCycle rejects it by choice (a); the \
         self-hosted compiler compiles it and prints 5",
    ),
    (
        "tests/errors/family_poly_nontype.moss",
        "AUDIT-CHANGES #9 (E34): a family's listed polymorphic inputs are \
         types; the self-hosted compiler does not check it",
    ),
    (
        "tests/errors/sameneed_ambiguous.moss",
        "AUDIT-CHANGES #11 and #51: the self-hosted compiler prints 7",
    ),
    (
        "tests/errors/sameneed_ambiguous_other.moss",
        "AUDIT-CHANGES #11 and #51: the self-hosted compiler prints 7",
    ),
    (
        "tests/errors/while_condition_break.moss",
        "AUDIT-CHANGES #2 (E22): the self-hosted compiler does not count a \
         break in a while condition, and its module loops forever",
    ),
    (
        "tests/errors/attached_on_abstract.moss",
        "Q5: an attached method needs a nominal receiver; the self-hosted \
         compiler does not check it",
    ),
    (
        "tests/errors/arg_count_mismatch.moss",
        "the self-hosted compiler does not check the argument count of a \
         call to `putchar`, and writes a module that does not validate",
    ),
    (
        "tests/errors/context_brackets.moss",
        "the self-hosted compiler lets a bracket on a context item bind a \
         type symbol that is none of the item's requirements (Audit.v note 56)",
    ),
    (
        "tests/errors/context_item_brackets.moss",
        "the self-hosted compiler ignores brackets on a type item of a \
         context (Audit.v note 57)",
    ),
    (
        "tests/errors/merge_cyclic.moss",
        "the self-hosted compiler merges a generation with a type that \
         contains it (no occurs check; Audit.v note 53)",
    ),
    (
        "tests/errors/record_field_twice.moss",
        "the self-hosted compiler does not check that a record literal \
         gives a payload's fields once each (Audit.v note 62)",
    ),
    (
        "tests/errors/type_brackets.moss",
        "the self-hosted compiler reads `T[]` for a type symbol as `T` \
         (Audit.v note 50)",
    ),
];

/// Programs the self-hosted compiler crashes on: it has no expanding-cycle
/// check (D16/D30), so it recurses until Wasm's call stack is exhausted (or
/// runs out of memory). The oracle rejects each (AUDIT-CHANGES #48, #52;
/// native follow-up E38). A fix shows up here as a report or a module.
pub const COMPILER_CRASHES: &[(&str, &str)] = &[
    ("tests/errors/expand_attached.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_call_brackets.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_family.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_fn_bind.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_functor_double.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_functor_forge.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_functor_output.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_lambda.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_merged_xy.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_merged_yx.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_polyrec.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_type_direct.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_type_ref.moss", "E38: no expanding-cycle check"),
    ("tests/errors/expand_type_ref_rec.moss", "E38: no expanding-cycle check"),
];

/// Programs the oracle accepts, for a recorded reason, and the self-hosted
/// compiler rejects. They have no oracle golden.
pub const ORACLE_ACCEPTS: &[(&str, &str)] = &[(
    "tests/errors/functor_output_input_changed.moss",
    "AUDIT-CHANGES #14: Audit.v reads the application's result at the \
     body's table (the consolidation reading), so no stale key arises",
)];

/// Programs the oracle does not model, so it cannot reject them either.
pub const ORACLE_UNSUPPORTED_ERRORS: &[(&str, &str)] = &[(
    "tests/errors/beyond_std.moss",
    "`main` assumes an item no platform provides; the oracle reports the \
     construct unsupported rather than judging it",
)];

fn bless() -> bool {
    std::env::var_os("MOSS_BLESS").is_some()
}

fn check_golden(path: &str, actual: &str) {
    if bless() {
        std::fs::write(repo().join(path), actual).unwrap();
    } else {
        assert_eq!(read(path), actual, "{path} (bless with MOSS_BLESS=1)");
    }
}

/// The self-hosted compiler on `entry`, without requiring it to finish:
/// a crash is a nonzero exit.
pub fn compile_raw(entry: &str) -> moss_tests::Out {
    moss_tests::wasmtime(moss_tests::compiler(), Some("lib/prelude.moss"), &[entry])
}

#[test]
fn goldens() {
    let root = format!("{}/", repo().display());
    let programs = moss_tests::moss_files("tests/errors");
    let results = super::parallel::map(&programs, |entry| (compile_raw(entry), oracle(entry, &[])));
    for (entry, (compiled, checked)) in programs.iter().zip(results) {
        let entry = entry.as_str();
        let name = entry.trim_start_matches("tests/errors/").trim_end_matches(".moss").to_string();
        if COMPILER_CRASHES.iter().any(|(e, _)| *e == entry) {
            assert_ne!(compiled.code, 0, "{entry} is listed as crashing the compiler, but it finished");
        } else {
            assert_eq!(compiled.code, 0, "the compiler failed on {entry}: {}", compiled.stderr);
            if COMPILER_ACCEPTS.iter().any(|(e, _)| *e == entry) {
                assert!(compiled.is_module(), "{entry} is listed as wrongly accepted, but is refused now");
            } else {
                assert!(!compiled.is_module(), "{entry} compiled");
                check_golden(&format!("tests/errors/compiler/{name}.txt"), &compiled.text());
            }
        }
        if ORACLE_ACCEPTS.iter().any(|(e, _)| *e == entry) {
            assert_eq!(checked.code, 0, "{entry} is listed as accepted by the oracle: {}", checked.stderr);
            continue;
        }
        if ORACLE_UNSUPPORTED_ERRORS.iter().any(|(e, _)| *e == entry) {
            assert_eq!(checked.code, ORACLE_UNSUPPORTED, "{entry}: {}", checked.stderr);
        } else {
            assert!(
                checked.code == ORACLE_REJECTED || checked.code == ORACLE_FRONT_END,
                "the oracle should reject {entry}: {} {}",
                checked.code,
                checked.stderr
            );
        }
        check_golden(&format!("tests/errors/oracle/{name}.txt"), &checked.stderr.replace(&root, ""));
    }
}

/// No golden outlives its program.
#[test]
fn every_golden_belongs_to_a_program() {
    let programs: std::collections::BTreeSet<String> = moss_tests::moss_files("tests/errors")
        .iter()
        .map(|e| e.trim_start_matches("tests/errors/").trim_end_matches(".moss").to_string())
        .collect();
    for dir in ["compiler", "oracle"] {
        for file in std::fs::read_dir(repo().join("tests/errors").join(dir)).unwrap() {
            let path = file.unwrap().path();
            let stem = path.file_stem().unwrap().to_str().unwrap().to_string();
            assert!(programs.contains(&stem), "{} has no program", path.display());
            let entry = format!("tests/errors/{stem}.moss");
            assert!(
                dir != "oracle" || !ORACLE_ACCEPTS.iter().any(|(e, _)| *e == entry),
                "{} belongs to a program the oracle accepts",
                path.display()
            );
        }
    }
}
