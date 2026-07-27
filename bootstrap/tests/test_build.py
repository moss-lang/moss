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


def find_wasmtime():
    direct = shutil.which("wasmtime")
    if direct:
        return direct
    if shutil.which("nix"):
        try:
            out = subprocess.run(
                ["nix", "build", "nixpkgs#wasmtime", "--no-link", "--print-out-paths"],
                capture_output=True,
                text=True,
                timeout=600,
                check=True,
            ).stdout.strip()
            candidate = Path(out) / "bin" / "wasmtime"
            if candidate.exists():
                return str(candidate)
        except (subprocess.SubprocessError, OSError):
            return None
    return None


WASMTIME = find_wasmtime()


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
        [WASMTIME, path], capture_output=True, text=True, timeout=120
    )
    if result.returncode != 0:
        raise AssertionError(f"wasmtime failed: {result.stderr}")
    return result.stdout


@unittest.skipUnless(WASMTIME, "wasmtime not available")
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
        source = "assume Std {\n  fn main() { let s = first_arg(); }\n}\n"
        with self.assertRaises(build_mod.NotCompilable):
            compile_wasm({"main.moss": source})
