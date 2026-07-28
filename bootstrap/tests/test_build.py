import io
import itertools
import shutil
import subprocess
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from mossc import build as build_mod
from mossc import collect
from mossc.lower import Lower

from .test_run import runnable_examples

REPO = Path(__file__).resolve().parents[2]
PRELUDE = str(REPO / "lib/prelude.moss")

# One per-run directory holds every module these tests write, and dies with
# the process — `delete=False` temporaries were left behind, a
# compiler-sized file per test.
_WASM_DIR = tempfile.TemporaryDirectory(prefix="moss-test-")
_wasm_names = itertools.count()


def wasm_file(module: bytes) -> str:
    """A module as a path wasmtime can run."""
    path = Path(_WASM_DIR.name) / f"{next(_wasm_names)}.wasm"
    path.write_bytes(module)
    return str(path)


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


def wasm_opt(module: bytes) -> bytes:
    """The same program, twenty times faster to run.

    A module this back end emits is straight-line and unoptimized, and
    running the *compiler* as one costs about two and a half minutes per
    generation; `wasm-opt -O3` costs a second and takes that to seven. It
    is a semantics-preserving rewrite, so a module it produces answers
    exactly as the original does — asserted by
    `TestSelfHostedFixpoint.test_optimizing_the_compiler_does_not_change_it`,
    which is what lets the fixpoint be checked on optimized generations.

    A hard requirement, like wasmtime: binaryen is in the dev shell and in
    the flake's `bootstrap` check."""
    path = shutil.which("wasm-opt")
    if path is None:
        raise AssertionError(
            "wasm-opt not found on PATH; enter the Nix dev shell "
            "(or run `nix flake check`), which provides binaryen"
        )
    src = wasm_file(module)
    dst = src + ".opt.wasm"
    subprocess.run(
        [path, "-all", "-O3", "-o", dst, src], check=True, timeout=600
    )
    return Path(dst).read_bytes()


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
    path = wasm_file(wasm)
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
    path = wasm_file(wasm)
    result = subprocess.run(
        [wasmtime(), path], capture_output=True, text=True, timeout=120
    )
    if result.returncode != 0:
        raise AssertionError(f"wasmtime failed: {result.stderr}")
    return result.stdout


class TestWasmBackend(unittest.TestCase):
    """The compiled module must behave exactly like the interpreter: every
    runnable example's Wasm output matches its golden stdout."""

    def test_examples_match_goldens(self):
        for name in runnable_examples():
            with self.subTest(example=name):
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                wasm = compile_wasm({}, entry=f"examples/{name}.moss")
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
        path = wasm_file(wasm)
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

    def test_i64_is_a_scalar_type(self):
        """D59: a value is a sequence of Wasm scalars, and not every scalar
        is an i32. `1 << 32` has an empty low half, which is only true if
        the shift really happened in 64 bits — and the intermediate lives
        in an i64 local."""
        wasm = compile_wasm({}, entry="tests/wasi/int64.moss")
        self.assertEqual(run_wasm(wasm), "yy\n")

    def test_raw_wasi_program(self):
        """D52: a program can assume the primitive context directly, with no
        `Std` and no prelude bridge — just Wasm instructions and WASI
        imports. Writes "A\\n" through fd_write, then exits 3."""
        wasm = compile_wasm({}, entry="tests/wasi/raw.moss")
        path = wasm_file(wasm)
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
        path = wasm_file(wasm)
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
        path = wasm_file(wasm)
        result = subprocess.run(
            [wasmtime(), path, "abc"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "abcabc,ab,\n")

    def test_interner_arena_in_wasm(self):
        """Same driver as the interpreter's interner test, same output."""
        from tests.test_run import ARENA_DRIVER

        wasm = compile_wasm({"main.moss": ARENA_DRIVER})
        path = wasm_file(wasm)
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
        path = wasm_file(wasm)
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

    def test_records_and_unions_do_not_allocate(self):
        """D59: the compiler has no heap. Records are one scalar per field
        and an injected union is a discriminant beside its payload, so a
        loop building both leaves the heap pointer where it was."""
        wasm = compile_wasm({}, entry="tests/wasi/noheap.moss")
        self.assertEqual(run_wasm(wasm), "bnbnbny\n")

    def test_nominal_wrapper_does_not_allocate(self):
        """The same, watched from underneath: constructing a wrapper leaves
        the heap pointer where it was."""
        wasm = compile_wasm({}, entry="tests/wasi/noalloc.moss")
        self.assertEqual(run_wasm(wasm), "ky\n")

    def test_the_whole_std_over_wasi(self):
        """The milestone: code written against the ordinary `Std`
        signature, with every one of its names provided in Moss over the
        primitive context. `main` assumes `Wasm, Wasi, Branch` and applies
        one functor (D52, D55); nothing native is underneath."""
        wasm = compile_wasm({}, entry="tests/wasi/full.moss")
        source = (REPO / "lib/bool.moss").read_text(encoding="utf-8")
        self.assertEqual(
            run_in_repo(wasm, ["lib/bool.moss"]), source[:-1] + " yy\n"
        )

    def test_containers_over_wasi(self):
        """CellInt and IntList in Moss, over the library's allocator. The
        list is pushed past its initial capacity, so the grow-by-copy runs
        — the first real data structure rather than a wrapper over
        instructions."""
        wasm = compile_wasm({}, entry="tests/wasi/containers.moss")
        self.assertEqual(run_wasm(wasm), "yyyy\n")

    def test_path_over_wasi(self):
        """D57 cleared by D59: `Path.read` is Moss now — path_open with
        genuine i64 rights masks, then fd_read onto the top of the heap
        the library itself manages."""
        wasm = compile_wasm({}, entry="tests/wasi/files.moss")
        expected = (REPO / "lib/bool.moss").read_text(encoding="utf-8")
        self.assertEqual(run_in_repo(wasm, ["lib/bool.moss"]), expected)

    def test_strings_and_chars_over_wasi(self):
        """String's methods and the char constants, in Moss over Wasi. The
        wrapper that carries the methods costs nothing now (D58), and the
        constants are ors of powers of two, since there are no literals."""
        wasm = compile_wasm({}, entry="tests/wasi/strings.moss")
        path = wasm_file(wasm)
        result = subprocess.run(
            [wasmtime(), path, "ab"], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "ababby\n")

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

    def test_self_hosted_compiler_compiles_to_wasm(self):
        """The self-hosted compiler as a WASI module: src/main.moss built
        by the bootstrap, then run on a program of its own — and the
        module it writes is the one the interpreter wrote, byte for
        byte, and it runs."""
        from tests.test_run import run_bytes

        wasm = compile_wasm({}, entry="src/main.moss")
        path = wasm_file(wasm)
        result = subprocess.run(
            [wasmtime(), "--dir", ".", path, "", "tests/wasi/prim.moss"],
            capture_output=True,
            timeout=600,
            cwd=REPO,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        emitted = result.stdout
        self.assertEqual(emitted[:8], b"\0asm\x01\0\0\0")

        # The same input through the bootstrap's own back end, for the
        # behaviour rather than the bytes: two compilers, one program.
        inner = wasm_file(emitted)
        mine = subprocess.run(
            [wasmtime(), inner], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(mine.returncode, 0, mine.stderr)
        self.assertEqual(mine.stdout, "ABKDKJGG\n")
        theirs = compile_wasm({}, entry="tests/wasi/prim.moss")
        self.assertEqual(run_wasm(theirs), mine.stdout)

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
        path = wasm_file(wasm)
        result = subprocess.run(
            [wasmtime(), path, source], capture_output=True, text=True, timeout=300
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "." * expected + "\n")


class TestSelfHostedCompilesTheExamples(unittest.TestCase):
    """The milestone: every runnable example, compiled by the compiler
    written in Moss, produces its golden output.

    These are ordinary Moss over `Std` — and `Std` is Moss too, provided
    over the primitive context by one functor (D52, D55). So this
    exercises the whole of it: contextual vals as parameters, fn and
    method binds, a functor's worth of them applied at once, nominal
    wrappers that cost nothing, and one specialization per environment.

    Run as Wasm rather than interpreted, because interpreting the
    compiler takes about a hundred seconds per example and running it
    takes fifty milliseconds."""

    def compiler(self):
        wasm = wasm_opt(compile_wasm({}, entry="src/main.moss"))
        return wasm_file(wasm)

    def test_examples_match_their_goldens(self):
        compiler = self.compiler()
        for name in runnable_examples():
            with self.subTest(example=name):
                built = subprocess.run(
                    [wasmtime(), "--dir", ".", compiler,
                     "lib/prelude.moss", f"examples/{name}.moss"],
                    capture_output=True,
                    timeout=600,
                    cwd=REPO,
                )
                self.assertEqual(built.returncode, 0, built.stderr)
                self.assertEqual(
                    built.stdout[:8], b"\0asm\x01\0\0\0",
                    f"not a module: {built.stdout[:200]!r}",
                )
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                self.assertEqual(run_wasm(built.stdout), golden)


class TestSelfHostedFixpoint(unittest.TestCase):
    """The milestone that ends the list in docs/implementation/selfhosting.md:
    the compiler compiles itself, and the result is a fixpoint.

    `S0 = B(S)` is the compiler as the bootstrap builds it; `S1 = S0(S)` is
    the compiler as *it* builds itself; `S2 = S1(S)` is one more turn of the
    crank. `S1 == S2` byte for byte says the compiler is a fixpoint of
    itself: whatever S0 did differently is gone, and S1 reproduces its own
    input exactly. `S0 != S1` is expected and asserted, because two
    different compilers emit different code for one source — if those two
    were ever equal, this test would be comparing something to itself.

    Both compilers are deterministic, which is what the byte comparison
    depends on.

    Each generation *runs* through `wasm-opt -O3`, which takes a round from
    two and a half minutes to seven seconds: this back end emits
    straight-line unoptimized code, and almost all of the cost is the
    constant factor of `Std` written in Moss. What is compared is still the
    raw output of each generation, never the optimized one — raw equality
    implies optimized equality and not the other way round, so comparing
    optimized modules could hide a difference the optimizer happens to
    erase. The optimizer is only ever the thing that runs a compiler, never
    the thing that produces a module under test."""

    def compile_self(self, compiler: bytes) -> bytes:
        """One generation: run this compiler on the compiler's own source."""
        path = wasm_file(wasm_opt(compiler))
        result = subprocess.run(
            [wasmtime(), "--dir", ".", path,
             "lib/prelude.moss", "src/main.moss"],
            capture_output=True,
            timeout=1800,
            cwd=REPO,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            result.stdout[:8], b"\0asm\x01\0\0\0",
            # A diagnostic is a letter and a name (D48), so the first lines
            # of a failure are the report, not a module.
            f"not a module: {result.stdout[:400]!r}",
        )
        return result.stdout

    def test_the_compiler_is_a_fixpoint_of_itself(self):
        s0 = compile_wasm({}, entry="src/main.moss")
        s1 = self.compile_self(s0)
        s2 = self.compile_self(s1)
        self.assertEqual(s1, s2, "S1 != S2: the compiler is not a fixpoint")
        self.assertNotEqual(
            s0, s1, "S0 == S1, so this test compared a module with itself"
        )

    def test_optimizing_the_compiler_does_not_change_it(self):
        """What the speedup above rests on: the optimized compiler and the
        compiler as emitted produce the same module for the same input. Run
        on an example rather than on `src/`, so it costs one slow generation
        instead of two."""
        s0 = compile_wasm({}, entry="src/main.moss")
        args = ["lib/prelude.moss", "examples/context.moss"]

        def compile_with(module: bytes) -> bytes:
            path = wasm_file(module)
            result = subprocess.run(
                [wasmtime(), "--dir", ".", path, *args],
                capture_output=True,
                timeout=1800,
                cwd=REPO,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            return result.stdout

        emitted = compile_with(s0)
        self.assertEqual(emitted[:8], b"\0asm\x01\0\0\0", emitted[:400])
        self.assertEqual(compile_with(wasm_opt(s0)), emitted)


class TestSelfHostedEmitter(unittest.TestCase):
    """The self-hosted encoder, compiled: the emitter runs as Wasm and the
    module it writes is byte-for-byte the one the interpreter wrote."""

    def test_emitted_module_matches_the_interpreters_and_runs(self):
        from .test_run import run_bytes

        wasm = compile_wasm({}, entry="tests/wasi/emit.moss")
        path = wasm_file(wasm)
        result = subprocess.run(
            [wasmtime(), path], capture_output=True, timeout=300
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        emitted = result.stdout
        self.assertEqual(emitted, run_bytes({}, entry="tests/wasi/emit.moss"))

        inner = wasm_file(emitted)
        result = subprocess.run(
            [wasmtime(), inner], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "Hello, world!\n")
