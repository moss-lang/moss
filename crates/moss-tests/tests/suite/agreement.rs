//! The two front ends accept and reject the same programs.
//!
//! The Rocq front end (the oracle's lexer, parser and elaborator, the
//! front end of the bootstrap) and the self-hosted compiler are run on
//! every program of `tests/wasi`, `tests/errors`, `tests/regressions` and
//! `examples`. Each
//! either accepts a program (writes a module; elaborates it) or rejects it
//! (writes a report; refuses it), and the two must say the same, except
//! for the programs listed in `GAPS` below, each with the verdicts it gets
//! and why. A listed program must get exactly the listed verdicts, so a
//! gap that closes shows up here too.
//!
//! The oracle elaborates only (`--elab-only`): what this checks is
//! acceptance. What accepted programs *do* is checked program by program
//! elsewhere in the suite, against goldens and against the oracle's run.

use moss_tests::{compile, moss_files, oracle_command};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Verdict {
    Accept,
    Reject,
    /// The oracle reports a construct it does not model.
    Unsupported,
}
use Verdict::*;

/// Known gaps: (program, the self-hosted compiler's verdict, the oracle's,
/// why). Keep sorted by program.
/// A known self-hosted compiler bug: it does not check the argument count
/// of a call to `putchar`, and writes a module that does not validate.
const SELF_HOSTED_BUG_ARITY: &str = "self-hosted compiler bug: a call to `putchar` with no argument is accepted";
/// A known self-hosted compiler bug (Q5): an attached method needs a
/// nominal receiver, which the self-hosted compiler does not check. The
/// Python bootstrap and the oracle reject it.
const SELF_HOSTED_BUG_Q5: &str = "self-hosted compiler bug: an attached method on an abstract type is accepted (Q5)";
/// A self-hosted compiler bug: it reads `T[]` for a type symbol as `T`;
/// the oracle rejects it (Audit.v note 50).
const SELF_HOSTED_BUG_TYPE_BRACKETS: &str = "self-hosted compiler bug: empty brackets on a type symbol are accepted";
/// A self-hosted compiler bug: a bracket on a context item may bind a type
/// symbol that is none of the item's requirements; the oracle rejects it
/// (Audit.v note 56).
const SELF_HOSTED_BUG_CONTEXT_BRACKETS: &str = "self-hosted compiler bug: a context item's bracket binds a non-requirement";
/// A self-hosted compiler bug: it ignores brackets on a context item that
/// takes none (a type, here); the oracle rejects them (Audit.v note 57).
const SELF_HOSTED_BUG_ITEM_BRACKETS: &str = "self-hosted compiler bug: brackets on a type item of a context are ignored";
/// A self-hosted compiler bug: it merges a generation with a type that
/// contains it; the oracle rejects it (Audit.v note 53).
const SELF_HOSTED_BUG_MERGE_CYCLE: &str = "self-hosted compiler bug: a cyclic merge is accepted";
/// A record literal of a payload that names a field twice: the oracle (and
/// Part V's ExRecord) asks for every field once, the self-hosted compiler
/// does not check it (Audit.v note 62).
const SELF_HOSTED_BUG_FIELD_TWICE: &str = "self-hosted compiler bug: a record literal of a payload that names a field twice is accepted";
/// AUDIT-CHANGES #14: Audit.v accepts the program, by the consolidation
/// reading of `link_site`; the self-hosted compiler rejects it (D84 3d).
const AUDIT_ACCEPTS_14: &str = "Audit.v accepts what the self-hosted compiler rejects (#14)";
/// `main` assumes an item no platform provides. The oracle runs `main` in
/// the platform's root context and reports anything beyond it unsupported
/// rather than judging it.
const BEYOND: &str = "oracle gap: `main` assumes more than the platform provides";
/// D79: the host (MossX) has one linear memory, so the oracle reports a
/// program that binds `Memory` unsupported rather than run its memories on
/// one address space.
const MEMORIES: &str = "reference gap: more than one memory (D79)";
/// A library module that other tests import, with no `main`: the
/// self-hosted compiler reports the missing `main`, and the oracle, which
/// only runs whole programs, reports it unsupported.
const LIBRARY: &str = "a library module with no `main`";

/// Known gaps: (program, the self-hosted compiler's verdict, the oracle's,
/// why). Sorted by program.
const GAPS: &[(&str, Verdict, Verdict, &str)] = &[
    ("tests/errors/arg_count_mismatch.moss", Accept, Reject, SELF_HOSTED_BUG_ARITY),
    ("tests/errors/attached_on_abstract.moss", Accept, Reject, SELF_HOSTED_BUG_Q5),
    ("tests/errors/beyond_std.moss", Reject, Unsupported, BEYOND),
    ("tests/errors/context_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_CONTEXT_BRACKETS),
    ("tests/errors/context_item_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_ITEM_BRACKETS),
    ("tests/errors/functor_output_input_changed.moss", Reject, Accept, AUDIT_ACCEPTS_14),
    ("tests/errors/merge_cyclic.moss", Accept, Reject, SELF_HOSTED_BUG_MERGE_CYCLE),
    ("tests/errors/record_field_twice.moss", Accept, Reject, SELF_HOSTED_BUG_FIELD_TWICE),
    ("tests/errors/type_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_TYPE_BRACKETS),
    ("tests/wasi/acrosslib.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/app.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/arena.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/arrays_in_scratch.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/bridge.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/doubledouble.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/functor_capture_memory.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/gc_array_memory.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/memories.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/memory_any_type.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/memory_copy.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/memory_reset.moss", Accept, Unsupported, MEMORIES),
    ("tests/wasi/saturating.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/shadowing.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/signature.moss", Reject, Unsupported, LIBRARY),
];

/// What the self-hosted compiler says.
fn compiler(entry: &str) -> Verdict {
    if compile(entry, "lib/prelude.moss").is_module() {
        Accept
    } else {
        Reject
    }
}

/// What the oracle's front end says.
fn oracle(entry: &str) -> (Verdict, String) {
    let out = oracle_command(&["--elab-only", entry]);
    let verdict = match out.code {
        0 => Accept,
        moss_tests::ORACLE_FRONT_END | moss_tests::ORACLE_REJECTED => Reject,
        moss_tests::ORACLE_UNSUPPORTED => Unsupported,
        code => panic!("the oracle exited with {code} on {entry}: {}", out.stderr),
    };
    (verdict, out.stderr)
}

#[test]
fn the_front_ends_accept_and_reject_alike() {
    let mut programs = moss_files("tests/wasi");
    programs.extend(moss_files("tests/errors"));
    programs.extend(moss_files("tests/regressions"));
    programs.extend(moss_files("examples"));
    let mut failures = Vec::new();
    let mut counts = std::collections::BTreeMap::new();
    let verdicts = super::parallel::map(&programs, |entry| (compiler(entry), oracle(entry)));
    assert_eq!(verdicts.len(), programs.len());
    for (entry, (mine, (theirs, message))) in programs.iter().zip(verdicts) {
        *counts.entry((mine, theirs)).or_insert(0) += 1;
        match GAPS.iter().find(|(e, ..)| e == entry) {
            Some((_, m, t, why)) => {
                if (mine, theirs) != (*m, *t) {
                    failures.push(format!(
                        "{entry}: listed as {m:?}/{t:?} ({why}), but is {mine:?}/{theirs:?} now: {}",
                        message.trim()
                    ));
                }
            }
            None => {
                if mine != theirs {
                    failures.push(format!(
                        "{entry}: the self-hosted compiler says {mine:?}, the oracle {theirs:?}: {}",
                        message.trim()
                    ));
                }
            }
        }
    }
    for (entry, ..) in GAPS {
        assert!(programs.iter().any(|p| p == entry), "{entry} is listed but gone");
    }
    eprintln!("agreement over {} programs: {counts:?}", programs.len());
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
