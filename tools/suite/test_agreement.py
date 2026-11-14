import sys
from collections import Counter

from suite import support as moss
from suite import test_errors
from suite.support import (
    moss_files,
    oracle_command,
    parallel_map,
)

# The two front ends accept and reject the same programs.
#
# The Rocq front end (the oracle's lexer, parser and elaborator, the
# front end of the bootstrap) and the self-hosted compiler are run on
# every program of `tests/wasi`, `tests/errors`, `tests/regressions` and
# `examples`. Each
# either accepts a program (writes a module; elaborates it) or rejects it
# (writes a report; refuses it), and the two must say the same, except
# for the programs listed in `GAPS` below, each with the verdicts it gets
# and why. A listed program must get exactly the listed verdicts, so a
# gap that closes shows up here too.
#
# The oracle elaborates only (`--elab-only`): what this checks is
# acceptance. What accepted programs *do* is checked program by program
# elsewhere in the suite, against goldens and against the oracle's run.
Accept, Reject, Unsupported, Crash = "accept", "reject", "unsupported", "crash"
# Known gaps: (program, the self-hosted compiler's verdict, the oracle's,
# why). Keep sorted by program.
# A known self-hosted compiler bug: it does not check the argument count
# of a call to `putchar`, and writes a module that does not validate.

SELF_HOSTED_BUG_ARITY = "self-hosted compiler bug: a call to `putchar` with no argument is accepted"

# A known self-hosted compiler bug (Q5): an attached method needs a
# nominal receiver, which the self-hosted compiler does not check. The
# Python bootstrap and the oracle reject it.

SELF_HOSTED_BUG_Q5 = (
    "self-hosted compiler bug: an attached method on an abstract type is accepted (Q5)"
)

# A self-hosted compiler bug: it reads `T[]` for a type symbol as `T`;
# the oracle rejects it (Audit.v note 50).

SELF_HOSTED_BUG_TYPE_BRACKETS = (
    "self-hosted compiler bug: empty brackets on a type symbol are accepted"
)

# A self-hosted compiler bug: a bracket on a context item may bind a type
# symbol that is none of the item's requirements; the oracle rejects it
# (Audit.v note 56).

SELF_HOSTED_BUG_CONTEXT_BRACKETS = (
    "self-hosted compiler bug: a context item's bracket binds a non-requirement"
)

# A self-hosted compiler bug: it ignores brackets on a context item that
# takes none (a type, here); the oracle rejects them (Audit.v note 57).

SELF_HOSTED_BUG_ITEM_BRACKETS = (
    "self-hosted compiler bug: brackets on a type item of a context are ignored"
)

# A self-hosted compiler bug: it merges a generation with a type that
# contains it; the oracle rejects it (Audit.v note 53).

SELF_HOSTED_BUG_MERGE_CYCLE = "self-hosted compiler bug: a cyclic merge is accepted"

# AUDIT-CHANGES #14: Audit.v accepts the program, by the consolidation
# reading of `link_site`; the self-hosted compiler rejects it (D84 3d).

AUDIT_ACCEPTS_14 = "Audit.v accepts what the self-hosted compiler rejects (#14)"

# `main` assumes an item no platform provides. The oracle runs `main` in
# the platform's root context and reports anything beyond it unsupported
# rather than judging it.

BEYOND = "oracle gap: `main` assumes more than the platform provides"

# A library module that other tests import, with no `main`: the
# self-hosted compiler reports the missing `main`, and the oracle, which
# only runs whole programs, reports it unsupported.

LIBRARY = "a library module with no `main`"

# A self-hosted compiler bug or gap recorded with the program it was
# found by (the programs that were `bootstrap/examples/`): the reason is
# in `errors::COMPILER_ACCEPTS`, `errors::COMPILER_CRASHES` or
# `wasi_goldens::NATIVE_REJECTS`, and in AUDIT-CHANGES.

RECORDED = "recorded with the program (see errors.rs or wasi_goldens.rs, and AUDIT-CHANGES)"

# E38: the self-hosted compiler has no expanding-cycle check and crashes.

NO_EXPANDING_CHECK = "self-hosted compiler bug: no expanding-cycle check (E38)"

# Known gaps: (program, the self-hosted compiler's verdict, the oracle's,
# why). Sorted by program.

GAPS = [
    ("tests/errors/arg_count_mismatch.moss", Accept, Reject, SELF_HOSTED_BUG_ARITY),
    ("tests/errors/attached_on_abstract.moss", Accept, Reject, SELF_HOSTED_BUG_Q5),
    ("tests/errors/beyond_std.moss", Reject, Unsupported, BEYOND),
    ("tests/errors/bracket_method_receiver.moss", Accept, Reject, RECORDED),
    ("tests/errors/closure_partial_unbound_xyz.moss", Accept, Reject, RECORDED),
    ("tests/errors/context_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_CONTEXT_BRACKETS),
    ("tests/errors/context_item_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_ITEM_BRACKETS),
    ("tests/errors/duplicate_field_binder.moss", Accept, Reject, RECORDED),
    ("tests/errors/expand_attached.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_call_brackets.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_control_functor_const.moss", Accept, Reject, RECORDED),
    ("tests/errors/expand_family.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_fn_bind.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_functor_double.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_functor_forge.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_functor_output.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_lambda.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_merged_xy.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_merged_yx.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_polyrec.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_type_direct.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_type_ref.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/expand_type_ref_rec.moss", Crash, Reject, NO_EXPANDING_CHECK),
    ("tests/errors/family_poly_nontype.moss", Accept, Reject, RECORDED),
    ("tests/errors/functor_output_input_changed.moss", Reject, Accept, AUDIT_ACCEPTS_14),
    ("tests/errors/merge_cyclic.moss", Accept, Reject, SELF_HOSTED_BUG_MERGE_CYCLE),
    ("tests/errors/sameneed_ambiguous.moss", Accept, Reject, RECORDED),
    ("tests/errors/sameneed_ambiguous_other.moss", Accept, Reject, RECORDED),
    ("tests/errors/type_brackets.moss", Accept, Reject, SELF_HOSTED_BUG_TYPE_BRACKETS),
    ("tests/errors/while_condition_break.moss", Accept, Reject, RECORDED),
    ("tests/wasi/acrosslib.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/app.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/bridge.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/const_kept_output.moss", Reject, Accept, RECORDED),
    ("tests/wasi/copy_site_distinct_needs.moss", Reject, Accept, RECORDED),
    ("tests/wasi/copy_site_distinct_needs_flip.moss", Reject, Accept, RECORDED),
    ("tests/wasi/doubledouble.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/sameneed_copy.moss", Reject, Accept, RECORDED),
    ("tests/wasi/saturating.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/saved_type_rebinding.moss", Reject, Accept, RECORDED),
    ("tests/wasi/shadowing.moss", Reject, Unsupported, LIBRARY),
    ("tests/wasi/signature.moss", Reject, Unsupported, LIBRARY),
]

# What the self-hosted compiler says.


def compiler(entry):
    out = test_errors.compile_raw(entry)
    if out.code != 0:
        return Crash
    elif out.is_module():
        return Accept
    else:
        return Reject


# What the oracle's front end says.


def oracle(entry):
    out = oracle_command(["--elab-only", entry])
    match out.code:
        case 0:
            verdict = Accept
        case moss.ORACLE_FRONT_END | moss.ORACLE_REJECTED:
            verdict = Reject
        case moss.ORACLE_UNSUPPORTED:
            verdict = Unsupported
        case code:
            raise AssertionError(f"the oracle exited with {code} on {entry}: {out.stderr}")
    return (verdict, out.stderr)


def test_the_front_ends_accept_and_reject_alike():
    programs = [
        entry
        for directory in ("tests/wasi", "tests/errors", "tests/regressions", "examples")
        for entry in moss_files(directory)
    ]
    gaps = {entry: (mine, theirs, reason) for entry, mine, theirs, reason in GAPS}
    assert gaps.keys() <= set(programs), f"missing programs: {gaps.keys() - set(programs)}"
    failures = []
    counts = Counter()
    verdicts = parallel_map(programs, lambda entry: (compiler(entry), oracle(entry)))
    for entry, (mine, (theirs, message)) in zip(programs, verdicts, strict=True):
        counts[mine, theirs] += 1
        if entry in gaps:
            expected_mine, expected_theirs, reason = gaps[entry]
            if (mine, theirs) != (expected_mine, expected_theirs):
                failures.append(
                    f"{entry}: listed as {expected_mine}/{expected_theirs} ({reason}), "
                    f"but is {mine}/{theirs} now: {message.strip()}"
                )
        elif mine != theirs:
            failures.append(
                f"{entry}: the compiler says {mine}, the oracle {theirs}: {message.strip()}"
            )
    print(f"agreement over {len(programs)} programs: {dict(counts)}", file=sys.stderr)
    assert not failures, "\n".join(failures)
