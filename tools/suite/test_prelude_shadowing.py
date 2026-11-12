from suite.support import (
    ORACLE_REJECTED,
    compile_source,
    oracle,
    run_module,
    sources,
)

# The implicit prelude supplies defaults; ordinary imports stay strict.


def accepts(files, expected):
    dir = sources(files)
    entry = f"{dir}/main.moss"
    compiled = compile_source(entry, "lib/prelude.moss")
    assert compiled.is_module(), f"{entry}: {compiled.text()}"
    assert run_module(compiled.stdout, []) == expected
    checked = oracle(entry, [])
    assert checked.code == 0, f"{entry}: {checked.stderr}"
    assert checked.text() == expected


def rejects(files, code):
    dir = sources(files)
    entry = f"{dir}/main.moss"
    compiled = compile_source(entry, "lib/prelude.moss")
    assert not (compiled.is_module()), f"{entry} unexpectedly compiled"
    assert code in compiled.text(), f"{entry}: {compiled.text()}"
    checked = oracle(entry, [])
    assert checked.code == ORACLE_REJECTED, f"{entry}: {checked.stderr}"


SCALAR = "type Base;\nassume Base { fn identity(x: Base): Base { x } }\n"


def test_local_declarations_shadow_prelude_names():
    accepts(
        [
            (
                "main.moss",
                """type Base;
assume Base { fn identity(x: Base): Base { x } }
fn main() {}
""",
            )
        ],
        "",
    )


def test_explicit_names_shadow_the_prelude_and_can_be_reimported():
    accepts(
        [
            ("scalar.moss", SCALAR),
            (
                "main.moss",
                """import "./scalar.moss" use Base;
import "./scalar.moss" use Base;
assume Base { fn identity(x: Base): Base { x } }
fn main() {}
""",
            ),
        ],
        "",
    )


def test_explicit_globs_shadow_prelude_defaults():
    accepts(
        [
            ("scalar.moss", SCALAR),
            (
                "main.moss",
                """import "./scalar.moss" use *;
assume Base { fn local(x: Base): Base { identity(x) } }
fn main() {}
""",
            ),
        ],
        "",
    )


def test_local_detached_methods_shadow_prelude_methods():
    accepts(
        [
            (
                "main.moss",
                """type Item;
assume Item {
  fn .read(): Item;
  assume Item.read { fn read_it(x: Item): Item { x.read() } }
}
fn main() {}
""",
            )
        ],
        "",
    )


def test_explicit_module_aliases_shadow_prelude_aliases():
    accepts(
        [
            ("ops.moss", "fn go() {}\n"),
            ("main.moss", 'import "./ops.moss" as char;\nfn main() { char::go(); }\n'),
        ],
        "",
    )


def test_module_aliases_from_explicit_globs_shadow_the_prelude():
    accepts(
        [
            ("ops.moss", "fn go() {}\n"),
            ("aliases.moss", 'import "./ops.moss" as char;\n'),
            ("main.moss", 'import "./aliases.moss" use *;\nfn main() { char::go(); }\n'),
        ],
        "",
    )


def test_unshadowed_prelude_defaults_remain_available():
    accepts(
        [
            (
                "main.moss",
                """type Local;
assume ReferenceBase { fn main() { bind StdBase; print("defaults"); putchar(char::newline); } }
""",
            )
        ],
        "defaults\n",
    )


def test_shadowing_does_not_change_bindings_in_other_modules():
    accepts(
        [
            (
                "helper.moss",
                """assume ReferenceBase { fn report() { bind StdBase; print("prelude"); putchar(char::newline); } }
""",
            ),
            (
                "main.moss",
                """import "./helper.moss" use report;
assume ReferenceBase {
  fn print() { bind StdBase; println("local"); }
  fn main() { bind StdBase; print(); report(); print(); }
}
""",
            ),
        ],
        "local\nprelude\nlocal\n",
    )


def test_conflicting_explicit_imports_still_fail_in_either_order():
    for first, second in [("a", "b"), ("b", "a")]:
        for names in ["Base", "*"]:
            main = f"""import "./{first}.moss" use {names};
import "./{second}.moss" use {names};
fn main() {{}}
"""
            rejects(
                [("a.moss", "type Base;\n"), ("b.moss", "type Base;\n"), ("main.moss", main)], "!E "
            )


def test_explicit_imports_conflict_with_local_declarations():
    rejects(
        [
            ("scalar.moss", SCALAR),
            ("main.moss", 'import "./scalar.moss" use Base;\ntype Base;\nfn main() {}\n'),
        ],
        "!E ",
    )


def test_an_explicit_prelude_glob_is_an_ordinary_import():
    rejects([("main.moss", 'import "prelude.moss" use *;\ntype Base;\nfn main() {}\n')], "!E ")


def test_implicit_prelude_names_are_not_reexported():
    rejects(
        [
            ("helper.moss", "fn exported() {}\n"),
            ("main.moss", 'import "./helper.moss" use Base;\nfn main() {}\n'),
        ],
        "!C ",
    )


def test_explicit_named_imports_can_reexport_prelude_symbols():
    accepts(
        [
            ("helper.moss", 'import "base.moss" use ReferenceBase;\n'),
            (
                "main.moss",
                """import "./helper.moss" use ReferenceBase;
assume ReferenceBase { fn main() {} }
""",
            ),
        ],
        "",
    )


def test_a_named_import_promotes_a_glob_binding_to_an_export():
    accepts(
        [
            ("helper.moss", 'import "base.moss" use *;\nimport "base.moss" use ReferenceBase;\n'),
            (
                "main.moss",
                """import "./helper.moss" use ReferenceBase;
assume ReferenceBase { fn main() {} }
""",
            ),
        ],
        "",
    )
