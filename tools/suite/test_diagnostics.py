from suite.support import (
    compiler,
    sources,
    std_main,
    wasmtime,
)

# CLI diagnostics retain source ranges independently of their rendering.


def report(entry):
    out = wasmtime(compiler(), "lib/prelude.moss", [entry, "--diagnostics=human"])
    assert out.code == 0, f"{out.stderr}"
    assert not (out.is_module()), f"{entry} unexpectedly compiled"
    return out.text()


def test_missing_names_are_underlined_at_each_use():
    text = std_main("assume Std {\n  fn main() {\n    missing();\n    missing();\n  }\n}")
    dir = sources([("main.moss", text)])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.count("this name is not in scope: `missing`") == 2, f"{got}"
    assert f"{entry}:5:5" in got, f"{got}"
    assert f"{entry}:6:5" in got, f"{got}"
    assert "5 |     missing();\n  |     ^^^^^^^^^" in got, f"{got}"


def test_columns_handle_utf8_tabs_and_crlf():
    text = std_main('assume Std {\n  fn main() {\n\tlet text = "λ"; missing();\n  }\n}').replace(
        "\n", "\r\n"
    )
    dir = sources([("main.moss", text)])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert f"{entry}:5:21" in got, f"{got}"
    assert (
        """5 |     let text = "λ"; missing();
  |                     ^^^^^^^^^"""
        in got
    ), f"{got}"
    assert not ("\r" in got), f"{got!r}"


def test_duplicate_declarations_point_to_the_second_definition():
    dir = sources([("main.moss", "type Twice;\ntype Twice;\n")])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.startswith("error: duplicate definition: `Twice`\n"), f"{got}"
    assert f"{entry}:2:1" in got, f"{got}"
    assert "2 | type Twice;\n  | ^^^^^^^^^^^" in got, f"{got}"
    assert f"note: previous definition is here\n  --> {entry}:1:1" in got, f"{got}"


def test_syntax_errors_in_multiple_modules_keep_their_own_locations():
    dir = sources(
        [
            ("main.moss", 'import "./broken.moss" as broken;\ntype Missing =\n'),
            ("broken.moss", "type Broken =\n"),
        ]
    )
    got = report(f"{dir}/main.moss")
    assert got.count("invalid syntax") == 2, f"{got}"
    assert f"{dir}/main.moss:3:1" in got, f"{got}"
    assert f"{dir}/broken.moss:2:1" in got, f"{got}"
    assert "3 | \n  | ^\n" in got, f"{got}"


def test_unreadable_imports_report_the_missing_path_without_a_fake_excerpt():
    dir = sources([("main.moss", 'import "./absent.moss" as absent;\n')])
    got = report(f"{dir}/main.moss")
    assert "cannot read this source file" in got, f"{got}"
    assert f"--> {dir}/absent.moss\n" in got, f"{got}"
    assert not (" | " in got), f"{got}"


def test_backend_errors_have_messages_and_expression_locations():
    text = std_main("assume Std {\n  fn main() {\n    break;\n  }\n}")
    dir = sources([("main.moss", text)])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.startswith("error: break is only allowed inside a loop"), f"{got}"
    assert f"{entry}:5:5" in got, f"{got}"
    assert "5 |     break;\n  |     ^^^^^" in got, f"{got}"


def test_missing_semicolons_name_the_expected_token():
    text = std_main("""assume Std {
  fn main() {
    let n = 1
    print("hello");
  }
}""")
    dir = sources([("main.moss", text)])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.startswith("error: invalid syntax: expected `;`"), f"{got}"
    assert f"{entry}:6:5" in got, f"{got}"


def test_missing_main_has_a_path_without_an_invented_source_range():
    dir = sources([("main.moss", "type Thing;\n")])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.startswith("error: no valid main function was found"), f"{got}"
    assert f"--> {entry}\n" in got, f"{got}"
    assert not (" | " in got), f"{got}"


def test_invalid_main_signatures_report_the_declaration():
    dir = sources([("main.moss", "fn main(value: ()) {}\n")])
    entry = f"{dir}/main.moss"
    got = report(entry)
    assert got.startswith("error: no valid main function was found"), f"{got}"
    assert f"{entry}:1:1" in got, f"{got}"
    assert "1 | fn main(value: ()) {}\n  | ^" in got, f"{got}"


def test_missing_functor_arrows_name_the_expected_token():
    dir = sources([("main.moss", "functor F: Int Int;\n")])
    got = report(f"{dir}/main.moss")
    assert got.startswith("error: invalid syntax: expected `->`"), f"{got}"


def test_semantic_errors_found_in_codegen_keep_their_code_domain():
    # Semantic e_incoherent and back-end c_unknown_op both have code 17.
    # A stale functor binding must never be described as an unknown Wasm op.
    got = report("tests/wasi/functor_fixed_stale.moss")
    assert (
        "error: the available binding was provided at a different type instantiation: `Heap`" in got
    ), f"{got}"
    assert "tests/wasi/functor_fixed_stale.moss:15:5" in got, f"{got}"
    assert not ("unknown WebAssembly instruction" in got), f"{got}"
