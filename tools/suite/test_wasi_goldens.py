import os

from suite.support import (
    TRAP,
    oracle,
    parallel_map,
    read,
    repo,
    wasm_file,
    wasmtime,
)

# The `tests/wasi` programs that have a golden, `tests/wasi/stdout/NAME.txt`
# (the semantics probes that were `bootstrap/examples/`), print it: compiled
# by the self-hosted compiler and run, and run by the oracle. Both must
# exit 0, except for the programs listed below. Each golden is the
# oracle's output (`MOSS_BLESS=1` rewrites them from it).
#
# Whether the two front ends accept each program at all is checked by
# `agreement`; a program the self-hosted compiler rejects is listed in
# `NATIVE_REJECTS` here and in `agreement::GAPS` there.
# Programs whose run ends in a trap, on both: a checked cast fails.

TRAPS = [
    (
        "copy_capture",
        "G keeps the X it captured (I32), so the cast to Ref[Boxed=F64] fails (D84 6)",
    ),
    (
        "saved_generic_rebinding",
        "the box made at U keeps U's meaning, so the cast to Ref[Boxed=F64] fails",
    ),
    (
        "saved_type_rebinding",
        "the box made at I32 keeps I32, so the cast to Ref[Boxed=Saved] fails",
    ),
]

# Programs the self-hosted compiler rejects, so only the oracle runs them
# (`agreement::GAPS` lists each).

NATIVE_REJECTS = [
    (
        "const_kept_output",
        "AUDIT-CHANGES #20: the self-hosted compiler rejects every use of a defined val",
    ),
    (
        "copy_site_distinct_needs",
        "AUDIT-CHANGES #119: the self-hosted compiler cannot resolve m in out",
    ),
    ("copy_site_distinct_needs_flip", "AUDIT-CHANGES #119: as copy_site_distinct_needs"),
    ("sameneed_copy", "AUDIT-CHANGES #51: the self-hosted compiler rejects the copy"),
    ("saved_type_rebinding", "the self-hosted compiler rejects `main`"),
]

# Programs the self-hosted compiler runs to another output: (name, what
# it prints, why).

NATIVE_PRINTS = [
    (
        "sameneed_meaning",
        "1\n",
        "AUDIT-CHANGES #51: the self-hosted compiler captures X.m; Audit links it",
    ),
    ("sameneed_meaning_agree", "1\n", "AUDIT-CHANGES #51: as sameneed_meaning"),
]


def golden_names():
    return sorted(path.stem for path in (repo() / "tests/wasi/stdout").glob("*.txt"))


def test_the_probes_print_their_goldens():
    from suite.test_errors import compile_raw

    traps, rejections = dict(TRAPS), dict(NATIVE_REJECTS)
    native_output = {name: text for name, text, _ in NATIVE_PRINTS}
    names = golden_names()

    def check(name):
        entry = f"tests/wasi/{name}.moss"
        native = None
        if name not in rejections:
            compiled = compile_raw(entry)
            assert compiled.is_module(), f"{entry} did not compile: {compiled.text()}"
            native = wasmtime(wasm_file(compiled.stdout), None, [])
        return native, oracle(entry, [])

    failures = []
    for name, (native, reference) in zip(names, parallel_map(names, check), strict=True):
        code = TRAP if name in traps else 0
        golden_path = f"tests/wasi/stdout/{name}.txt"
        if os.getenv("MOSS_BLESS") is not None and reference.code == code:
            (repo() / golden_path).write_bytes(reference.stdout)
        golden = read(golden_path)
        if reference.code != code or reference.text() != golden:
            failures.append(
                f"the oracle on {name}: exit {reference.code}, {reference.text()!r}: {reference.stderr.strip()}"
            )
        if native is not None and (
            native.code != code or native.text() != native_output.get(name, golden)
        ):
            failures.append(f"the compiler on {name}: exit {native.code}, {native.text()!r}")
    assert not failures, "\n".join(failures)


def test_every_golden_belongs_to_a_program():
    names = golden_names()
    for name in names:
        assert (repo() / f"tests/wasi/{name}.moss").is_file(), f"{name}.txt has no program"
    for name, *_ in (*TRAPS, *NATIVE_REJECTS, *NATIVE_PRINTS):
        assert name in names, f"{name} is listed but has no golden"
