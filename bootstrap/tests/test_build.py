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
        # The loader hands back canonical absolute paths; the in-memory
        # files are named relative to the repo.
        rel = str(Path(path).relative_to(REPO)) if Path(path).is_relative_to(REPO) else path
        if rel in files:
            return files[rel]
        return Path(path).read_text(encoding="utf-8")

    program = collect.load(entry, read=read, prelude=PRELUDE, root=REPO)
    lower = Lower(program)
    lower.run()
    main_sym = program.entry.names["main"]
    return build_mod.build(program, lower, main_sym)


def run_in_repo(wasm: bytes, args: list[str]) -> str:
    """Run with the repo preopened, which is what makes `Path` resolve:
    WASI paths are relative to a preopened directory, so `pwd` is empty."""
    with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
        f.write(wasm)
        path = f.name
    result = subprocess.run(
        [wasmtime(), "--dir", ".", path, *args],
        capture_output=True,
        text=True,
        timeout=300,
        cwd=REPO,
    )
    if result.returncode != 0:
        raise AssertionError(f"wasmtime failed: {result.stderr}")
    return result.stdout


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

    def test_raw_wasi_program(self):
        """D52: a program can assume the primitive context directly, with no
        `Std` and no prelude bridge — just Wasm instructions and WASI
        imports. Writes "A\\n" through fd_write, then exits 3."""
        wasm = compile_wasm({}, entry="tests/wasi/raw.moss")
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.stdout, "A\n")
        self.assertEqual(result.returncode, 3)

    def test_wasi_imports_are_only_what_is_called(self):
        """The import section is no longer a fixed three: `proc_exit` shows
        up only in the module that calls it, and every function index moves
        to accommodate it."""
        raw = compile_wasm({}, entry="tests/wasi/raw.moss")
        std = compile_wasm(
            {"main.moss": "assume Std {\n  fn main() { putchar(char::a); }\n}\n"}
        )
        self.assertIn(b"proc_exit", raw)
        self.assertNotIn(b"proc_exit", std)
        for module in (raw, std):
            # The shims' imports are always there, whatever else is.
            for field in (b"fd_write", b"args_sizes_get", b"args_get"):
                self.assertIn(field, module)

    def test_i64_reports_itself_as_out_of_slice(self):
        source = (
            'import "./lib/wasm.moss" as w use Wasm, I64;\n'
            "assume Wasm {\n"
            "  fn main() { let n = w::i64_extend_i32_u(w::i32_one); }\n"
            "}\n"
        )
        with self.assertRaises(build_mod.NotCompilable):
            compile_wasm({"main.moss": source})

    def test_fn_binds_compile_to_specialisations(self):
        """A bound fn is a direct call to a specialisation of the callee,
        not an indirect one (D2). Two providers of the same key produce two
        specialisations of everything below them, and a provider's captured
        vals are threaded down to wherever the call actually happens — here
        two frames below the bind."""
        files = {
            "runner.moss": "fn task();\n"
            "assume task {\n"
            "  fn go() { twice(); }\n"
            "  fn twice() { task(); task(); }\n"
            "}\n",
            "main.moss": 'import "./runner.moss" as runner;\n'
            "assume Std {\n"
            "  val c: Char;\n"
            "  assume c { fn emit() { putchar(c); } }\n"
            "  fn shout() { putchar(char::exclam); }\n"
            "  fn main() {\n"
            "    bind c=char::k;\n"
            "    bind runner::task=emit;\n"
            "    runner::go();\n"
            "    bind runner::task=shout;\n"
            "    runner::go();\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n",
        }
        self.assertEqual(run_wasm(compile_wasm(files)), "kk!!\n")

    def test_method_provision_compiles(self):
        """`bind N.plus = Box.plus` is a fn bind too, and the call through
        it resolves to Box.plus directly."""
        source = (
            "type N;\n"
            "fn .plus(rhs: This): This;\n"
            "val n0: N;\n"
            "context NOps = N, n0, N.plus;\n"
            "assume Std {\n"
            "  type Box Char;\n"
            "  fn Box.plus(rhs: Box): Box {\n"
            "    match this { Box a => match rhs { Box b => Box (b) } }\n"
            "  }\n"
            "  fn putbox(b: Box) { match b { Box c => putchar(c) } }\n"
            "  assume NOps { fn use_it(): N { n0.plus(n0) } }\n"
            "  fn main() {\n"
            "    bind N = Box;\n"
            "    bind n0 = Box (char::k);\n"
            "    bind N.plus = Box.plus;\n"
            "    putbox(use_it());\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(run_wasm(compile_wasm({"main.moss": source})), "k\n")

    def test_bridge_from_wasi_to_a_signature(self):
        """The shape D52 is aiming at, in miniature: user code written
        against a signature alone, a module implementing that signature
        over the primitive context, and `main` applying it with binds."""
        wasm = compile_wasm({}, entry="tests/wasi/bridged.moss")
        self.assertEqual(run_wasm(wasm), "!!")

    def test_char_code_in_wasm(self):
        """Chars are already i32 codepoints here, so `.code()` is an identity —
        but it must still agree with the interpreter."""
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    var i = zero;\n"
            "    while i.lt(char::c.code().sub(char::a.code())) {\n"
            "      putchar(char::dot);\n"
            "      i = i.add(one);\n"
            "    }\n"
            "    if char::A.code().lt(char::a.code()) { putchar(char::u) }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        self.assertEqual(run_wasm(wasm), "..u\n")

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

    def test_string_slice_in_wasm(self):
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    let s = first_arg();\n"
            "    let two = one.add(one);\n"
            "    print(s.slice(one, two));\n"
            "    print(s.slice(zero, s.length()));\n"
            "    let t = s.slice(two, two);\n"
            "    if t.length().eq(two) { putchar(char::y) } else { putchar(char::n) }\n"
            "    putchar(t.get(zero));\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path, "abcdef"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "bcabcdefyc\n")

    def test_strings_and_strlist_in_wasm(self):
        """concat and StrList, which the self-hosted module loader needs to
        hold and build file paths."""
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    let s = first_arg();\n"
            "    let xs = str_list();\n"
            "    xs.push(s.concat(s));\n"
            "    xs.push(s.slice(zero, one).concat(s.slice(one, one)));\n"
            "    var i = zero;\n"
            "    while i.lt(xs.length()) {\n"
            "      print(xs.get(i));\n"
            "      putchar(char::comma);\n"
            "      i = i.add(one);\n"
            "    }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path, "abc"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "abcabc,ab,\n")

    def test_interner_arena_in_wasm(self):
        """Same driver as the interpreter's interner test, same output."""
        from tests.test_run import ARENA_DRIVER

        wasm = compile_wasm({"main.moss": ARENA_DRIVER})
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path, "abcd"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "abcdyd\n")

    def test_allocation_grows_memory(self):
        """The module starts at two pages and nothing else grows it, so a
        program that allocates past 128K used to write off the end."""
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    let xs = int_list();\n"
            "    var i = zero;\n"
            "    let n = one.shl(one.shl(one.add(one).add(one).add(one)));\n"
            "    while i.lt(n) { xs.push(i); i = i.add(one); }\n"
            "    if xs.length().eq(n) { putchar(char::y) } else { putchar(char::n) }\n"
            "    if xs.get(n.sub(one)).eq(n.sub(one)) { putchar(char::z) }\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(run_wasm(compile_wasm({"main.moss": source})), "yz\n")

    def test_std_implemented_in_moss_over_wasi(self):
        """The retirement path, first bricks: `print` and `first_arg` are
        Moss functions over Wasi, with a bump allocator also written in
        Moss, installed by a functor. The code that calls them assumes
        `Console` and knows nothing of either."""
        wasm = compile_wasm({}, entry="tests/wasi/console.moss")
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(wasm)
            path = f.name
        result = subprocess.run(
            [wasmtime(), path, "hi"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "hihi")

    def test_representation_follows_the_static_type(self):
        """D58: a value of a nominal type is its payload, even when that
        type is a member of a union elsewhere in the program. The tag is
        attached at the injection, not at the construction."""
        source = (
            "assume Std {\n"
            "  type A Char;\n"
            "  unit Other;\n"
            "  type C = | Other | A;\n"
            "  fn bare(a: A): Char { match a { A c => c } }\n"
            "  fn tagged(c: C): Char { match c { A ch => ch, Other => char::n, } }\n"
            "  fn main() {\n"
            "    let a = A (char::y);\n"
            "    putchar(bare(a));\n"
            "    putchar(tagged(a));\n"
            "    putchar(tagged(Other));\n"
            "  }\n"
            "}\n"
        )
        self.assertEqual(run_wasm(compile_wasm({"main.moss": source})), "yyn")

    def test_nominal_wrapper_does_not_allocate(self):
        """The same, watched from underneath: constructing a wrapper leaves
        the heap pointer where it was."""
        wasm = compile_wasm({}, entry="tests/wasi/noalloc.moss")
        self.assertEqual(run_wasm(wasm), "ky\n")

    def test_numerics_over_wasi(self):
        """`Int`, `zero`, `one` and the arithmetic methods provided over
        Wasi rather than natively, driving a loop written in ordinary Std
        terms. Int goes through a nominal wrapper because only an attached
        method can see its receiver (D54), so every Int here is boxed."""
        wasm = compile_wasm({}, entry="tests/wasi/numbers.moss")
        self.assertEqual(run_wasm(wasm), "AAA=")

    def test_path_read_in_wasm(self):
        """`Path` was the last thing outside the slice: `pwd` is the empty
        path, `join` concatenates, and `read` is path_open plus fd_read
        against the preopen."""
        source = (
            "assume Std {\n"
            "  fn main() {\n"
            "    let text = pwd.join(first_arg()).read();\n"
            "    print(text);\n"
            "  }\n"
            "}\n"
        )
        wasm = compile_wasm({"main.moss": source})
        expected = (REPO / "lib/bool.moss").read_text(encoding="utf-8")
        self.assertEqual(run_in_repo(wasm, ["lib/bool.moss"]), expected)

    def test_self_hosted_cli_compiles_to_wasm(self):
        """src/main.moss, which reads its input file from disk, as a WASI
        module — matching the interpreter exactly."""
        wasm = compile_wasm({}, entry="src/main.moss")
        self.assertEqual(
            run_in_repo(wasm, ["lib/bool.moss"]),
            "uFalse;uTrue;tBool;vfalse;vtrue;\n\n\n",
        )

    def test_self_hosted_collect_compiles_to_wasm(self):
        """The whole multi-file front end as one module: it loads the
        prelude and the compiler's own sources off disk — eighteen modules
        — and explains every name in them."""
        from tests.test_run import COLLECT_DRIVER

        wasm = compile_wasm({"main.moss": COLLECT_DRIVER})
        out = run_in_repo(wasm, ["lib/prelude.moss", "src/main.moss"])
        lines = out.strip().split("\n")
        self.assertGreater(len(lines), 15)
        for line in lines:
            self.assertTrue(line.endswith(":"), f"unresolved names in {line}")

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
            "    bind tree::name_ids=int_list();\n"
            "    bind tree::ref_ids=int_list();\n"
            "    bind tree::imp_texts=str_list();\n"
            "    bind tree::imp_starts=int_list();\n"
            "    bind tree::imp_lens=int_list();\n"
            "    bind tree::imp_names=int_list();\n"
            "    bind tree::imp_stars=int_list();\n"
            "    bind intern::ichars=int_list();\n"
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
