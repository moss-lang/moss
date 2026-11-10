//! Every program in `tests/errors` is refused, with golden diagnostics from
//! both front ends:
//!
//! - `tests/errors/compiler/NAME.txt`: the self-hosted compiler's report
//!   (one line per diagnostic, a letter and the names involved, D48), which
//!   is what `moss` prints;
//! - `tests/errors/oracle/NAME.txt`: the oracle's message, with the
//!   repository's path taken out. Its messages are the Python bootstrap's,
//!   which the old goldens held, with a line and column for lexing and
//!   parsing errors.
//!
//! `MOSS_BLESS=1 cargo test -p moss-tests errors` rewrites the goldens.

use moss_tests::{compile, oracle, read, repo, ORACLE_FRONT_END, ORACLE_REJECTED, ORACLE_UNSUPPORTED};

/// Programs the self-hosted compiler wrongly accepts: known bugs of the
/// self-hosted compiler, which the oracle rejects. Each still compiles to
/// a module, which this suite checks, so a fix shows up here; then remove
/// it from this list and bless its golden.
pub const COMPILER_ACCEPTS: &[(&str, &str)] = &[
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
        "tests/errors/merge_unsettled.moss",
        "the self-hosted compiler stops merging a context after 7 rounds \
         and uses what it has, settled or not (Audit.v note 52)",
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

/// Programs the oracle wrongly accepts, by criteria section 7: an unused
/// declaration that does not form, which it checks only where something
/// uses it (agreement.rs lists the same gap). Each has no oracle golden.
pub const ORACLE_ACCEPTS: &[&str] = &[
    "tests/errors/decl_alias_cycle.moss",
    "tests/errors/decl_const_bad.moss",
    "tests/errors/decl_const_in_assume.moss",
    "tests/errors/decl_const_mistyped.moss",
    "tests/errors/decl_ctx_bracket.moss",
    "tests/errors/decl_ctx_unresolved.moss",
    "tests/errors/decl_fn_sig.moss",
    "tests/errors/decl_functor_arg.moss",
    "tests/errors/decl_functor_res.moss",
    "tests/errors/decl_meth_sig.moss",
    "tests/errors/decl_nest_incomplete.moss",
    "tests/errors/decl_nest_type.moss",
    "tests/errors/decl_tag_unresolved.moss",
    "tests/errors/decl_val_type.moss",
    "tests/errors/functor_partial_unused.moss",
];

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

#[test]
fn goldens() {
    let root = format!("{}/", repo().display());
    for entry in moss_tests::moss_files("tests/errors") {
        let name = entry.trim_start_matches("tests/errors/").trim_end_matches(".moss").to_string();
        let compiled = compile(&entry, "lib/prelude.moss");
        if COMPILER_ACCEPTS.iter().any(|(e, _)| *e == entry) {
            assert!(compiled.is_module(), "{entry} is listed as wrongly accepted, but is refused now");
        } else {
            assert!(!compiled.is_module(), "{entry} compiled");
            check_golden(&format!("tests/errors/compiler/{name}.txt"), &compiled.text());
        }
        let checked = oracle(&entry, &[]);
        if ORACLE_ACCEPTS.contains(&entry.as_str()) {
            assert_eq!(checked.code, 0, "{entry} is listed as wrongly accepted by the oracle: {}", checked.stderr);
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
        }
    }
}
