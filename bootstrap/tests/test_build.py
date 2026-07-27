import io
import shutil
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from mossc import build as build_mod
from mossc import collect
from mossc.lower import Lower

REPO = Path(__file__).resolve().parents[2]
PRELUDE = str(REPO / "lib/prelude.moss")


def wasmtime() -> str:
    """wasmtime is a hard requirement for these tests — the Nix dev shell
    and the flake's `bootstrap` check both provide it. No graceful skip:
    a missing runtime is an environment bug, not a reason to pass."""
    path = shutil.which("wasmtime")
    if path is None:
        raise AssertionError(
            "wasmtime not found on PATH; enter the Nix dev shell "
            "(or run `nix flake check`), which provides it"
        )
    return path


def compile_wasm(files, entry="main.moss"):
    def read(path):
        if path in files:
            return files[path]
        p = Path(path)
        if not p.is_absolute():
            p = REPO / path
        return p.read_text(encoding="utf-8")

    program = collect.load(entry, read=read, prelude=PRELUDE)
    lower = Lower(program)
    lower.run()
    main_sym = program.entry.names["main"]
    return build_mod.build(program, lower, main_sym)


def run_wasm(wasm: bytes) -> str:
    with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
        f.write(wasm)
        path = f.name
    result = subprocess.run(
        [wasmtime(), path], capture_output=True, text=True, timeout=120
    )
    if result.returncode != 0:
        raise AssertionError(f"wasmtime failed: {result.stderr}")
    return result.stdout


class TestWasmBackend(unittest.TestCase):
    """The compiled module must behave exactly like the interpreter: every
    runnable example's Wasm output matches its golden stdout."""

    EXAMPLES = ["hello", "true", "reassign", "params", "context", "rebind", "exit"]

    def test_examples_match_goldens(self):
        for name in self.EXAMPLES:
            with self.subTest(example=name):
                source = (REPO / f"examples/{name}.moss").read_text(encoding="utf-8")
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                wasm = compile_wasm({"main.moss": source})
                self.assertEqual(run_wasm(wasm), golden)

    def test_arithmetic_and_loops(self):
        source = (
            "assume Std {\n"
            "  fn triple(n: Int): Int { n.add(n).add(n) }\n"
            "  fn main() {\n"
            "    var i = zero;\n"
            "    while i.lt(triple(one)) {\n"
            "      putchar(char::w);\n"
            "      i = i.add(one);\n"
            "    }\n"
            "    if true.not() { putchar(char::n) } else { putchar(char::y) }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        self.assertEqual(run_wasm(wasm), "wwwy\n")

    def test_boxed_values_and_match(self):
        """Slice 2: units/unions with match, tags with payload binders, and
        records with field access and destructuring — all boxed in linear
        memory, all matching the interpreter's behavior exactly."""
        cases = {
            "colors": (
                "assume Std {\n"
                "  unit Red;\n"
                "  unit Green;\n"
                "  unit Blue;\n"
                "  type Color = | Red | Green | Blue;\n"
                "  fn code(c: Color): Char {\n"
                "    match c { Red => char::r, Green => char::g, Blue => char::b, }\n"
                "  }\n"
                "  fn main() { putchar(code(Green)); putchar(code(Red)); putchar(char::newline); }\n"
                "}\n",
                "gr\n",
            ),
            "wrap": (
                "assume Std {\n"
                "  type Wrapped Char;\n"
                "  unit Missing;\n"
                "  type Maybe = | Missing | Wrapped;\n"
                "  fn show(m: Maybe) {\n"
                "    match m { Wrapped c => putchar(c), Missing => putchar(char::question), }\n"
                "    putchar(char::newline);\n"
                "  }\n"
                "  fn main() { show(Wrapped (char::w)); show(Missing); }\n"
                "}\n",
                "w\n?\n",
            ),
            "point": (
                "assume Std {\n"
                "  type Point { x: Char, y: Char };\n"
                "  fn Point.show() {\n"
                "    putchar(char::lparen); putchar(this.x); putchar(char::comma);\n"
                "    putchar(this.y); putchar(char::rparen); putchar(char::newline);\n"
                "  }\n"
                "  fn flip(p: Point): Point {\n"
                "    match p { Point { x, y } => Point { x = y, y = x }, }\n"
                "  }\n"
                "  fn main() {\n"
                "    let p = Point { x = char::a, y = char::b };\n"
                "    p.show();\n"
                "    flip(p).show();\n"
                "  }\n"
                "}\n",
                "(a,b)\n(b,a)\n",
            ),
        }
        for name_, (source, expected) in cases.items():
            with self.subTest(case=name_):
                wasm = compile_wasm({"main.moss": source})
                self.assertEqual(run_wasm(wasm), expected)

    def test_out_of_slice_reports_itself(self):
        source = "assume Std {\n  fn main() { let p = pwd.join(first_arg()); }\n}\n"
        with self.assertRaises(build_mod.NotCompilable):
            compile_wasm({"main.moss": source})

    def test_intlist_in_wasm(self):
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    let xs = int_list();\n"
            "    var i = zero;\n"
            "    let n = one.shl(one.add(one).add(one).add(one));\n"  # 16 > cap 8
            "    while i.lt(n) { xs.push(i); i = i.add(one); }\n"
            "    if xs.length().eq(n) { putchar(char::n) }\n"
            "    if xs.get(zero).eq(zero) { putchar(char::z) }\n"
            "    xs.set(zero, one);\n"
            "    if xs.get(zero).eq(one) { putchar(char::s) }\n"
            "    if xs.get(n.sub(one)).eq(n.sub(one)) { putchar(char::e) }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        self.assertEqual(run_wasm(wasm), "nzse\n")

    def test_self_hosted_parser_compiles_to_wasm(self):
        """The whole self-hosted front end — lexer, arena parser, interner,
        duplicate detection — as one Wasm module, byte-identical to the
        interpreter."""
        driver = (
            'import "./src/intern.moss" as intern;\n'
            'import "./src/lex.moss" as lexer;\n'
            'import "./src/parse.moss" as parser;\n'
            'import "./src/tree.moss" as tree;\n'
            "assume Std {\n"
            "  fn main() {\n"
            "    bind lexer::src=first_arg();\n"
            "    bind lexer::at=cell_int();\n"
            "    bind lexer::mark=cell_int();\n"
            "    bind tree::kinds=int_list();\n"
            "    bind tree::starts=int_list();\n"
            "    bind tree::lens=int_list();\n"
            "    bind tree::kids=int_list();\n"
            "    bind tree::name_starts=int_list();\n"
            "    bind tree::name_lens=int_list();\n"
            "    bind tree::name_ids=int_list();\n"
            "    bind tree::ref_ids=int_list();\n"
            "    bind tree::ref_starts=int_list();\n"
            "    bind tree::ref_lens=int_list();\n"
            "    bind intern::istarts=int_list();\n"
            "    bind intern::ilens=int_list();\n"
            "    parser::run();\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"letters.moss": driver}, entry="letters.moss")
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        cases = [
            (
                "unit A; type A; assume B { fn f(); fn f() {} val g: B; }",
                "uA;tA;a(ff;ff;vg;)\nA!f!\nB?\n",
            ),
            (
                (REPO / "lib/bool.moss").read_text(encoding="utf-8"),
                "uFalse;uTrue;tBool;vfalse;vtrue;\n\n\n",
            ),
        ]
        for source, expected in cases:
            result = subprocess.run(
                [wasmtime(), path, source], capture_output=True, text=True, timeout=300
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, expected)

    def test_self_hosted_lexer_compiles_to_wasm(self):
        """The capstone: src/lex.moss, driven by a first_arg driver, compiled
        to a WASI module — tokenizing its own source with exactly the
        bootstrap lexer's count."""
        driver = (
            'import "./src/lex.moss" as lexer;\n'
            'import "./src/token.moss" use Eof;\n'
            "assume Std {\n"
            "  fn main() {\n"
            "    bind lexer::src=first_arg();\n"
            "    bind lexer::at=cell_int();\n"
            "    bind lexer::mark=cell_int();\n"
            "    loop { match lexer::lex() { Eof => break, _ => putchar(char::dot), } }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"dots.moss": driver}, entry="dots.moss")
        from mossc.lex import lex as bootstrap_lex

        source = (REPO / "src/lex.moss").read_text(encoding="utf-8")
        expected = len(bootstrap_lex(source)) - 1
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path, source], capture_output=True, text=True, timeout=300
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "." * expected + "\n")
