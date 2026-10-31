import os
import unittest
from pathlib import Path

from mossc import collect, interp
from mossc.lower import Lower, LowerError

REPO = Path(__file__).resolve().parents[2]
PRELUDE = str(REPO / "lib/prelude.moss")


def run_bytes(files, entry="main.moss", args=None):
    """Run a program given as {path: source}; lib/ comes from disk.

    Program output is captured as bytes — `interp.emit` is the sink, not
    `sys.stdout`, because a program may write binary (the self-hosted
    emitter writes a Wasm module) and a text capture would not survive it.
    """

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
    out = bytearray()
    original = interp.emit
    interp.emit = out.extend
    try:
        interp.run_main(program, lower, args)
    finally:
        interp.emit = original
    return bytes(out)


def run(files, entry="main.moss", args=None):
    return run_bytes(files, entry=entry, args=args).decode("utf-8")


_DRIVERS: dict = {}

# A directory that compiled drivers are shared through, between processes
# and, if it outlives them, between runs. Unset — as under a plain
# `python3 -m unittest` — each process compiles its own; tests/parallel.py
# sets it so that its workers compile each driver once among them.
DRIVER_CACHE_ENV = "MOSS_TEST_DRIVER_CACHE"


def _driver_key(driver: str) -> str:
    """What a compiled driver depends on: its text, the Moss it imports
    (all of it lives in src/ and lib/), and the bootstrap compiling it."""
    import hashlib

    digest = hashlib.sha256(driver.encode("utf-8"))
    roots = [REPO / "src", REPO / "lib", REPO / "bootstrap/mossc"]
    for path in sorted(p for root in roots for p in root.rglob("*") if p.is_file()):
        if "__pycache__" in path.parts:
            continue
        digest.update(b"\0" + str(path.relative_to(REPO)).encode("utf-8") + b"\0")
        digest.update(path.read_bytes())
    return digest.hexdigest()


def compile_driver(driver: str) -> str:
    """The path of `driver` compiled to Wasm: once per process, and once
    per cache directory when there is one."""
    path = _DRIVERS.get(driver)
    if path is not None:
        return path
    from .test_build import compile_wasm, wasm_file

    cache = os.environ.get(DRIVER_CACHE_ENV)
    if not cache:
        path = wasm_file(compile_wasm({"main.moss": driver}))
    else:
        target = Path(cache).resolve() / f"{_driver_key(driver)}.wasm"
        # Whoever takes the lock first compiles; the rest wait for it and
        # find the module there. Written to a private name and renamed, so
        # nobody ever sees half a module.
        with open(target.with_suffix(".lock"), "w") as lock:
            _lock(lock)
            if not target.exists():
                partial = target.with_suffix(f".{os.getpid()}.partial")
                partial.write_bytes(compile_wasm({"main.moss": driver}))
                os.replace(partial, target)
        path = str(target)
    _DRIVERS[driver] = path
    return path


def _lock(file):
    """Hold an exclusive lock on `file` until it is closed."""
    try:
        import fcntl
    except ImportError:  # Windows: no flock; at worst two compile one driver.
        return
    fcntl.flock(file, fcntl.LOCK_EX)


def run_driver(driver, args, expect_code=0, argv0=None, preopen="."):
    """Run a self-hosted driver as Wasm rather than interpreting it.

    A driver is ordinary Moss over `Std`, so the bootstrap can compile it
    to a WASI module — once, and cached here, since these drivers pull in
    all of `src/`. Interpreting them instead is what made this suite take
    minutes: `interp` is a Python tree-walker over the core IR, so it runs
    the self-hosted compiler some three orders of magnitude slower than
    the same code as Wasm. The program under test is identical either way.
    """
    import subprocess

    from .test_build import wasmtime

    path = compile_driver(driver)
    command = [wasmtime()]
    if argv0 is not None:
        command += ["--argv0", argv0]
    result = subprocess.run(
        [*command, "--dir", preopen, path, *args],
        capture_output=True,
        timeout=900,
        cwd=REPO,
    )
    if result.returncode != expect_code:
        raise AssertionError(f"wasmtime failed: {result.stderr.decode()}")
    return result.stdout


def main_body(body, decls=""):
    return {"main.moss": f"assume Std {{\n{decls}\n  fn main() {{ {body} }}\n}}"}


class TestHello(unittest.TestCase):
    def test_hello_world(self):
        self.assertEqual(
            run({}, entry="examples/hello.moss"), "Hello, world!\n"
        )


def runnable_examples():
    """Every example except `escape`, which is still written in the old
    language (it needs string literals) and is excluded until D48 is
    decided. Globbed rather than listed, so a new example cannot be
    forgotten and a deleted one cannot leave its golden behind."""
    return sorted(
        p.stem for p in (REPO / "examples").glob("*.moss") if p.stem != "escape"
    )


class TestExamples(unittest.TestCase):
    """Every rewritten example matches its golden stdout."""

    def test_goldens(self):
        for name in runnable_examples():
            with self.subTest(example=name):
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                # Loaded in place: an example's imports are relative to it.
                self.assertEqual(run({}, entry=f"examples/{name}.moss"), golden)

    def test_every_golden_belongs_to_an_example(self):
        # The other direction of the glob above: `instances.txt` once
        # outlived its example by a month because nothing looked.
        goldens = {p.stem for p in (REPO / "tests/examples/stdout").glob("*.txt")}
        examples = {p.stem for p in (REPO / "examples").glob("*.moss")}
        self.assertEqual(goldens, examples)


class TestErrors(unittest.TestCase):
    def test_not_in_scope(self):
        # hello.md: a name nobody declared is a *scope* error.
        with self.assertRaises(LowerError) as ctx:
            run(main_body("foo();"))
        self.assertIn("not in scope", ctx.exception.message)

    def test_not_in_context(self):
        # hello.md: a declared but unassumed function is a *context* error.
        files = {
            "main.moss": "fn example();\nfn main() { example(); }",
        }
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("not available in the context", ctx.exception.message)

    def test_assuming_more_than_std(self):
        # hello.md's example: main may assume only a subset of Std (D39).
        files = {
            "main.moss": "fn example();\nassume example { fn main() { example(); } }",
        }
        with self.assertRaises(interp.LinkError) as ctx:
            run(files)
        self.assertIn("not part of the `Std` context", str(ctx.exception))


class TestContext(unittest.TestCase):
    def test_needs_pass_through(self):
        out = run(
            main_body(
                "twice();",
                decls="  fn twice() { putchar(char::a); putchar(char::a); }",
            )
        )
        self.assertEqual(out, "aa")

    def test_bind_val_lexical(self):
        files = main_body(
            "bind c=char::x; emit(); bind c=char::y; emit();",
            decls="  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
        )
        self.assertEqual(run(files), "xy")

    def test_bind_scoped_to_block(self):
        # A bind inside an if-block must not leak out of it.
        files = main_body(
            "bind c=char::x; if true { bind c=char::y; emit(); } emit();",
            decls="  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
        )
        self.assertEqual(run(files), "yx")

    def test_bare_block_scopes_binds_and_lets(self):
        # D65: a bare block scopes exactly as an if-block does. Its `let`
        # shadows the outer one at another width — a record over a char —
        # and the outer one is back afterwards.
        files = main_body(
            "bind c=char::x; let p = char::p; "
            "{ bind c=char::y; let p = Pair { a = char::q, b = char::r }; "
            "emit(); putchar(p.b); } "
            "emit(); putchar(p); "
            "putchar({ bind c=char::z; let t = got(); t });",
            decls="  val c: Char;\n  type Pair { a: Char, b: Char };\n"
            "  assume c { fn emit() { putchar(c); } fn got(): Char { c } }",
        )
        self.assertEqual(run(files), "yrxpz")

    def test_fn_bind_to_contextual_fn_captures(self):
        # D66: `bind f = g;` with `g` abstract captures `g` as provided at
        # the bind: neither a rebind of `g` after it nor one deeper in the
        # call tree reaches `f`. The signature mentions an abstract type.
        files = main_body(
            "bind T = Char; bind c = char::a; bind g = emit; bind f = g; "
            "bind c = char::b; f(char::x); bind g = other; f(char::y); "
            "g(char::z); deeper(char::w);",
            decls="  type T;\n  val c: Char;\n"
            "  assume T { fn g(x: T); fn f(x: T); }\n"
            "  assume c { fn emit(x: Char) { putchar(c); putchar(x); } }\n"
            "  fn other(x: Char) { putchar(char::hyphen); }\n"
            "  assume T, f, g { fn deeper(x: T) { bind g = loud; f(x); } }\n"
            "  assume T, g { fn loud(x: T) { g(x); g(x); } }",
        )
        self.assertEqual(run(files), "axay-aw")

    def test_fn_bind_to_unavailable_contextual_fn(self):
        files = main_body(
            "bind f = g;",
            decls="  fn g(x: Char);\n  fn f(x: Char);",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("`g` is not available in the context here", ctx.exception.message)

    def test_fn_bind_to_contextual_fn_checks_the_signature(self):
        files = main_body(
            "bind f = putchar;",
            decls="  fn f(x: Int);",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("fn bind signature mismatch", ctx.exception.message)

    def test_unbound_val_is_context_error(self):
        files = main_body(
            "emit();",
            decls="  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("not available in the context", ctx.exception.message)

    def test_bind_fn_across_modules(self):
        files = {
            "lexer.moss": "type In;\nassume In {\n  fn next(): In;\n"
            "  assume next { fn go(): In { next() } }\n}",
            "main.moss": 'import "./lexer.moss" as lexer;\n'
            "assume Std {\n"
            "  fn mine(): Char { char::z }\n"
            "  fn main() {\n"
            "    bind lexer::In=Char;\n"
            "    bind lexer::next=mine;\n"
            "    putchar(lexer::go());\n"
            "  }\n}",
        }
        self.assertEqual(run(files), "z")

    def test_fn_bind_without_type_bind_is_error(self):
        # D27: bind lexer::next=mine does not infer bind lexer::In=Char.
        files = {
            "lexer.moss": "type In;\nassume In { fn next(): In; }",
            "main.moss": 'import "./lexer.moss" as lexer;\n'
            "assume Std {\n"
            "  fn mine(): Char { char::z }\n"
            "  fn main() { bind lexer::next=mine; }\n}",
        }
        with self.assertRaises(LowerError):
            run(files)

    def test_fn_bind_captures_context(self):
        # The provider's own needs are captured at the bind site, so the
        # callee can run it without assuming them (the cli.moss pattern).
        files = {
            "runner.moss": "fn task;"
            if False
            else "fn task();\nassume task { fn go() { task(); } }",
            "main.moss": 'import "./runner.moss" as runner;\n'
            "assume Std {\n"
            "  val c: Char;\n"
            "  assume c { fn emit() { putchar(c); } }\n"
            "  fn main() {\n"
            "    bind c=char::k;\n"
            "    bind runner::task=emit;\n"
            "    runner::go();\n"
            "  }\n}",
        }
        self.assertEqual(run(files), "k")

    def test_record_payloads_and_binder_scope(self):
        """The interpreter agrees with both back ends on a union of records
        of different widths, and on a pattern binder being scoped to its
        arm: the outer `x` is back after the match."""
        files = {
            "main.moss": "assume Std {\n"
            "  type Lit Char;\n"
            "  type Both { l: Char, r: Char };\n"
            "  type Node = | Lit | Both;\n"
            "  fn first(n: Node): Char { match n { Lit c => c, Both { l, r = _ } => l, } }\n"
            "  fn main() {\n"
            "    putchar(first(Both { r = char::b, l = char::a }));\n"
            "    putchar(first(Lit (char::c)));\n"
            "    let x = char::d;\n"
            "    match Lit (char::e) { Lit x => putchar(x), _ => (), }\n"
            "    putchar(x);\n"
            "    putchar(char::newline);\n"
            "  }\n"
            "}\n",
        }
        self.assertEqual(run(files), "aced\n")

    def test_a_bind_shadows_a_bind_whose_type_was_rebound(self):
        """D25: a later bind of the same symbol in an inner block shadows.
        `WasiFloats32` inside a scope with an f64 `Float` rebinds `Float`
        and then its ops, whose entries in force were checked against the
        old `Float`; they are replaced, not merged with (D43 is for the
        items of contexts being assumed). Both back ends run this too."""
        self.assertEqual(run({}, entry="tests/wasi/shadow.moss"), SHADOW_OUT)

    def test_fn_bind_capture_survives_rebinds(self):
        self.assertEqual(run(CAPTURE_REBOUND), "hhhh\n")


# What tests/wasi/records.moss prints, one property per line.
RECORDS_OUT = "ab\nba\nbaq\ncdez\ncd\ngf\nho\n"

# What tests/wasi/nested.moss prints.
NESTED_OUT = "bca\nfda\nghi\n"

# What tests/wasi/float.moss prints, one property per line; the comments
# there say why each is what it is.
FLOAT_OUT = "".join(
    f"{line}\n"
    for line in (
        1414213562, 1414213504, 1414213562, 333333343, 333333333, "0100",
        0, 2147483647, -2147483648, 0, 2147483647, -3,
        2, 4, -3, -2, -2, -1, 1, 2,
        1065353216, 1, 1073741824, 1568669697, -1,
        1, 333333344, 0, 2,
    )
)

# What tests/wasi/shadow.moss prints: f64, f32, f64, f32, f64.
SHADOW_OUT = "333333333\n333333344\n333333333\n333333344\n333333333\n"

# What tests/wasi/literals.moss prints: the same report at f64, then f32.
LITERALS_OUT = (
    "0 7 10 100 1000 1234567 2147483647 -42 3 0 \n"
    "3 -2 1 0 250 1 16777215 \n"
    "3141593 333333333 ny\n"
    "314159265 333333333 \n"
    "0 7 10 100 1000 1234567 2147483647 -42 3 0 \n"
    "3 -2 1 0 250 1 16777215 \n"
    "3141593 333333344 yn\n"
    # A third to twenty-one places is not f32's nearest third: its digits
    # were summed in f32 and rounded on the way (D68).
    "314159264 333333248 \n"
)

# What tests/wasi/lists.moss prints: an IntList beside a FloatList at f64,
# then the same at f32. A thousand 0.1s sum to 100 at f64 and to
# 99.999048 at f32; a third differs in the eighth digit.
LISTS_OUT = (
    "1000 1000 100000000 332833500 \n"
    "333333333 1 1 -7 251001 \n"
    "1000 1000 99999048 332833500 \n"
    "333333344 1 1 -7 251001 \n"
)

# What tests/wasi/overflow.moss prints before it traps: an oversized
# literal under wrapping i32 arithmetic, then one that fits, under it and
# under arithmetic that traps on overflow.
OVERFLOW_OUT = "1\n2147483647\n2147483647\n"

# D75: what tests/wasi/gc_tree.moss prints — the thesis's expression, a
# full tree of 2^10 ones and its depth, and the expression read through a
# contextual val, doubled — and what tests/wasi/gc_ast.moss prints, once
# over a pool of indices and once over the GC heap.
GC_TREE_OUT = "-10\n1024\n11\n-20\n"
GC_AST_OUT = "-10\n0\n4096\n" * 2
# ...and what tests/wasi/gc_two.moss prints, with `Heap` applied at two
# types in one scope.
GC_TWO_OUT = "3\n12\n7\n1\n"

# D78: what tests/wasi/functor_fast.moss prints — √2 and √3 scaled by a
# billion, at f64 and then at f32.
FUNCTOR_FAST_OUT = "1414213562\n1732050808\n1414213504\n1732050944\n"

# D78: what tests/wasi/functor_trace.moss prints — the notes of a traced
# subtree and the whole value, under each of two tracers.
FUNCTOR_TRACE_OUT = "2,7,4,3,3,6,\n7\n[2][7][4][3][3][6]\n7\n"


def wasm_text(module: bytes) -> str:
    """A module as Binaryen prints it, to look for instructions."""
    import shutil
    import subprocess

    from .test_build import wasm_features, wasm_file

    features = [f"--enable-{feature}" for feature in wasm_features()]
    return subprocess.run(
        [shutil.which("wasm-dis"), *features, wasm_file(module)],
        capture_output=True, text=True, check=True, timeout=120,
    ).stdout


def starts_with_rec_group(module: bytes) -> bool:
    """Whether the type section's first entry is a `rec` group (0x4E)."""
    def leb(at):
        value = shift = 0
        while True:
            byte = module[at]
            at += 1
            value |= (byte & 0x7F) << shift
            shift += 7
            if not byte & 0x80:
                return value, at

    at = 8
    while at < len(module):
        sid = module[at]
        size, body = leb(at + 1)
        if sid == 1:
            _, first = leb(body)
            return module[first] == 0x4E
        at = body + size
    return False

# What tests/wasi/ops.moss prints: D68's operators on `Int` and `Bool`.
OPS_OUT = (
    "19 -7 3 1 -7 2 23 35 21 5 1 -1 \n"
    "ynynnyny y\n"
    "2 15 13 1024 128 -5 \n"
    "nnynyyyyn\n"
)

# What tests/wasi/dual.moss prints, times a million: f(2), f'(2) and
# f''(2) for f(x) = x^3 - 2x - 5; Newton's iterates from 2 toward its
# root, 2.0945514815...; and d/dx (x * d/dy (x + y)) at 1.
DUAL_OUT = (
    "-1000000\n10000000\n12000000\n"
    "2100000\n2094568\n2094551\n2094551\n"
    "1000000\n"
)

# What tests/wasi/newton.moss prints: the same report at f64, then at f32.
NEWTON_OUT = (
    "1500000000\n1416666667\n1414213562\n1414213562\n0\n333333333\n"
    "-100000000\n16777217\n24-2-2\n2147483647\nnynnyy0\n"
    "1500000000\n1416666752\n1414213504\n1414213504\n0\n333333344\n"
    "-100000000\n16777216\n24-2-2\n2147483647\nnynnyy0\n"
)


# D26: a bound provider's needs are captured at the bind site, so a frame
# between the bind and the call that rebinds them changes nothing —
# directly (`relay`), a frame further down (`deep`), or through a provider
# that itself captured a fn bind (`wrap`, whose `task` is still `emit` with
# `c=h`). Even in the bind's own scope, a later rebind of `c` leaves the
# earlier `bind task=emit` alone. Every line prints `h`; any other letter
# names the rebind that leaked through.
CAPTURE_REBOUND = {
    "main.moss": "assume Std {\n"
    "  val c: Char;\n"
    "  fn task();\n"
    "  fn outer();\n"
    "  assume c { fn emit() { putchar(c); } }\n"
    "  fn shout() { putchar(char::exclam); }\n"
    "  assume task {\n"
    "    fn relay() { bind c=char::i; task(); }\n"
    "    fn deep() { bind c=char::j; below(); }\n"
    "    fn below() { task(); }\n"
    "    fn wrap() { task(); }\n"
    "  }\n"
    "  assume outer { fn later() { bind c=char::l; bind task=shout; outer(); } }\n"
    "  fn main() {\n"
    "    bind c=char::h;\n"
    "    bind task=emit;\n"
    "    relay();\n"
    "    deep();\n"
    "    bind outer=wrap;\n"
    "    bind c=char::k;\n"
    "    later();\n"
    "    task();\n"
    "    putchar(char::newline);\n"
    "  }\n"
    "}\n",
}


class TestMethods(unittest.TestCase):
    def test_attached_defined(self):
        files = main_body(
            "let w = Wrap (char::q); putchar(w.get());",
            decls="  type Wrap Char;\n"
            "  fn Wrap.get(): Char { match this { Wrap c => c } }",
        )
        self.assertEqual(run(files), "q")

    def test_detached_provided(self):
        files = main_body(
            "bind A=Char;\n"
            "    bind a=char::m;\n"
            "    bind A.gimme=giveq;\n"
            "    putchar(go());",
            decls="  type A;\n"
            "  fn .gimme(): Char;\n"
            "  val a: A;\n"
            "  context Ctx = A, a, A.gimme;\n"
            "  assume Ctx { fn go(): Char { a.gimme() } }\n"
            "  fn giveq(): Char { char::q }",
        )
        self.assertEqual(run(files), "q")

    def test_attached_declared_beside_its_receiver_needs_no_import(self):
        """The other half of D61: an attached method matches *by name* in
        the receiver's own module and is never imported — including one that
        is only declared there and provided from the context, which the
        detached rule (import it, or `mod::m`) does not cover."""
        files = main_body(
            "bind Wrap.spell=giveq;\n"
            "    let w = Wrap (char::z);\n"
            "    putchar(w.spell());",
            decls="  type Wrap Char;\n"
            "  fn Wrap.spell(): Char;\n"
            "  fn giveq(): Char { char::q }\n"
            "  context Spelling = Wrap.spell;\n",
        )
        self.assertEqual(run(files), "q")

    def test_attached_on_abstract_rejected(self):
        # Q5: attached methods require nominal receivers.
        files = main_body(
            "()",
            decls="  type A;\n  fn A.m(): Char { char::q }",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("nominal receiver", ctx.exception.message)


class TestMatch(unittest.TestCase):
    def test_bool_and_if(self):
        files = main_body(
            "if yes() { putchar(char::y) } else { putchar(char::n) }",
            decls="  fn yes(): Bool { true }",
        )
        self.assertEqual(run(files), "y")

    def test_bool_branch_and_not(self):
        files = main_body(
            "putchar(pick(false)); putchar(pick(true)); putchar(pick(true.not()));",
            decls="  fn pick(b: Bool): Char { if b { char::t } else { char::f } }",
        )
        self.assertEqual(run(files), "ftf")

    def test_non_exhaustive(self):
        files = main_body(
            "()",
            decls="  unit A;\n  unit B;\n  type AB = | A | B;\n"
            "  fn pick(x: AB): Char { match x { A => char::t, } }",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("not exhaustive", ctx.exception.message)

    def test_tag_and_record(self):
        files = main_body(
            "putchar(first(Pair { x = char::a, y = char::b }));",
            decls="  type Pair { x: Char, y: Char };\n"
            "  fn first(p: Pair): Char { match p { Pair { x } => x } }",
        )
        self.assertEqual(run(files), "a")

    def test_union_injection(self):
        files = main_body(
            "putchar(show(go(true))); putchar(show(go(false)));",
            decls="  unit Stop;\n"
            "  type Step Char;\n"
            "  type Out = | Stop | Step;\n"
            "  fn go(b: Bool): Out { if b { Step (char::s) } else { Stop } }\n"
            "  fn show(o: Out): Char { match o { Step c => c, Stop => char::dot, } }",
        )
        self.assertEqual(run(files), "s.")

    def test_while_loop_break(self):
        files = main_body(
            "var again = true;\n"
            "    while again { putchar(char::w); again = false; }\n"
            "    loop { putchar(char::l); break; }",
        )
        self.assertEqual(run(files), "wl")

    def test_recursion(self):
        files = main_body(
            "putchar(rec(true));",
            decls="  fn rec(b: Bool): Char { if b { rec(false) } else { char::r } }",
        )
        self.assertEqual(run(files), "r")


class TestStd(unittest.TestCase):
    def test_int_methods_and_while(self):
        files = main_body(
            "var i: Int = zero;\n"
            "    let stop = one.add(one).add(one);\n"
            "    while i.lt(stop) { putchar(char::i); i = i.add(one); }"
        )
        self.assertEqual(run(files), "iii")

    def test_char_eq(self):
        files = main_body(
            "if char::a.eq(char::a) { putchar(char::y) }\n"
            "    if char::a.eq(char::b) { putchar(char::n) }"
        )
        self.assertEqual(run(files), "y")

    def test_char_code(self):
        """`Char.code` is what lets the self-hosted interner store names as
        codepoints instead of spans into a single source buffer."""
        files = main_body(
            "let n = char::c.code().sub(char::a.code());\n"
            "    var i = zero;\n"
            "    while i.lt(n) { putchar(char::dot); i = i.add(one); }\n"
            "    if char::A.code().lt(char::a.code()) { putchar(char::u) }"
        )
        self.assertEqual(run(files), "..u")

    def test_cell(self):
        files = main_body(
            "let c = cell_int();\n"
            "    c.write(one);\n"
            "    if c.read().eq(one) { putchar(char::c) }"
        )
        self.assertEqual(run(files), "c")

    def test_string_arg(self):
        files = main_body(
            "let s = first_arg();\n"
            "    print(s);\n"
            "    putchar(s.get(zero));\n"
            "    if s.length().eq(one.add(one)) { putchar(char::exclam) }"
        )
        self.assertEqual(run(files, args=["ok"]), "oko!")

    def test_string_slice(self):
        files = main_body(
            "let s = first_arg();\n"
            "    let two = one.add(one);\n"
            "    print(s.slice(one, two));\n"
            "    print(s.slice(zero, s.length()));\n"
            "    let t = s.slice(two, two);\n"
            "    if t.length().eq(two) { putchar(char::y) } else { putchar(char::n) }\n"
            "    putchar(t.get(zero));"
        )
        self.assertEqual(run(files, args=["abcdef"]), "bcabcdefyc")

    def test_string_concat(self):
        files = main_body(
            "let s = first_arg();\n"
            "    print(s.concat(s));\n"
            "    putchar(char::comma);\n"
            "    print(s.slice(zero, one).concat(s.slice(one, one)));\n"
            "    putchar(char::comma);\n"
            "    print(s.concat(s.slice(zero, zero)));"
        )
        self.assertEqual(run(files, args=["abc"]), "abcabc,ab,abc")

    def test_str_list(self):
        """StrList is IntList's shape at String elements — a distinct
        nominal type, since a native list cannot be generic (D51)."""
        files = main_body(
            "let xs = str_list();\n"
            "    let s = first_arg();\n"
            "    xs.push(s.slice(zero, one));\n"
            "    xs.push(s);\n"
            "    var i = zero;\n"
            "    while i.lt(xs.length()) { print(xs.get(i)); putchar(char::comma); "
            "i = i.add(one); }"
        )
        self.assertEqual(run(files, args=["abc"]), "a,abc,")

    def test_string_slice_out_of_range_panics(self):
        files = main_body("let s = first_arg().slice(zero, one.add(one));")
        with self.assertRaises(interp.MossPanic):
            run(files, args=["x"])

    def test_division_by_zero_panics(self):
        files = main_body("let x = one.div(zero);")
        with self.assertRaises(interp.MossPanic):
            run(files)


class TestStdOverWasi(unittest.TestCase):
    def test_the_interpreter_runs_the_moss_std(self):
        """The retirement, from the other side: the interpreter provides
        only the primitive context — a linear memory, the instructions over
        it, and the WASI calls — and the whole of `Std` above that is the
        same Moss the backend compiles."""
        import os

        cwd = os.getcwd()
        os.chdir(REPO)
        try:
            out = run({}, entry="tests/wasi/full.moss", args=["lib/bool.moss"])
        finally:
            os.chdir(cwd)
        source = (REPO / "lib/bool.moss").read_text(encoding="utf-8")
        self.assertEqual(out, source[:-1] + " yy\n")


class TestOperators(unittest.TestCase):
    """D68: an operator is a call of one fixed method of lib/num.moss,
    provided through the context like any method."""

    def test_over_the_moss_std(self):
        self.assertEqual(run({}, entry="tests/wasi/ops.moss"), OPS_OUT)

    def test_over_the_native_std(self):
        from .test_build import compile_wasm, run_wasm

        files = main_body(
            "let two = one + one; let six = two * (two + one);"
            "putchar((six * six / two - six % (two + two) + char::A.code()).char());"
            "if !(six < two) & (six >= six) & !false { putchar(char::y); }"
            "if (two << two) - (six >> one) == two + two + one { putchar(char::y); }"
            "if (-two < zero) ^ true { putchar(char::n); } else { putchar(char::y); }"
        )
        # 36 / 2 - 6 % 4 = 16, so `Q`.
        self.assertEqual(run(files), "Qyyy")
        self.assertEqual(run_wasm(compile_wasm(files)), "Qyyy")

    def test_an_operator_the_context_lacks_is_the_method_call_error(self):
        messages = []
        for body in ("c + c", "c.add(c)"):
            with self.assertRaises(LowerError) as caught:
                run(main_body(f"let c = char::a; putchar({body});"))
            messages.append(caught.exception.message)
        self.assertEqual(messages[0], messages[1])
        self.assertIn("no method `.add` is available for `Char`", messages[0])

    def test_the_symbol_is_fixed_not_looked_up_by_name(self):
        """`+` is num.moss's `.add` even where no `.add` is in scope: a
        file beside the library gets no prelude, and this one imports no
        detached `.add` — yet its `+` is provided by `Std` as usual."""
        files = {
            "lib/plus.moss": (
                'import "./std.moss" use Std, putchar;\n'
                'import "./int.moss" use one;\n'
                'import "./char.moss" as char use .char, .code;\n'
                "assume Std {\n"
                "  fn main() {\n"
                "    let two = one + one;\n"
                "    putchar((two + char::A.code()).char());\n"
                "  }\n"
                "}\n"
            ),
        }
        self.assertEqual(run(files, entry="lib/plus.moss"), "C")
        with self.assertRaises(LowerError) as caught:
            run(
                {"lib/plus.moss": files["lib/plus.moss"].replace("one + one", "one.add(one)")},
                entry="lib/plus.moss",
            )
        self.assertIn("`.add` is not a detached method in scope", caught.exception.message)


class TestLiterals(unittest.TestCase):
    """D68: a literal is a use of fixed library symbols — `zero`, `one`,
    `.add`, `.mul` for an integer, and `float` and `.div` too for a
    decimal — so it means whatever `Int` and `Float` are bound to."""

    def test_over_the_moss_std(self):
        self.assertEqual(run({}, entry="tests/wasi/literals.moss"), LITERALS_OUT)

    def test_over_the_native_std(self):
        from .test_build import compile_wasm, run_wasm

        files = main_body(
            "putchar((65 + 1.add(1) * 3 - 0).char());"
            "putchar((2.5 * 4.0).round().add(char::A.code()).char());"
            "if 0.1 + 0.2 != 0.3 { putchar(char::y); }"
            "putchar((-(-66)).char());"
        )
        self.assertEqual(run(files), "GKyB")
        self.assertEqual(run_wasm(compile_wasm(files)), "GKyB")

    def test_a_literal_needs_its_symbols_in_context(self):
        for body, name in (("let n = 0;", "zero"), ("let n = 12;", "one")):
            with self.subTest(body=body):
                with self.assertRaises(LowerError) as caught:
                    run({"main.moss": f"fn main() {{ {body} }}"})
                self.assertIn(f"means `{name}`, which is not available", caught.exception.message)
        with self.assertRaises(LowerError) as caught:
            run({"main.moss": "assume Int, zero, one, Int.add, Int.mul {\n"
                 "  fn f() { let x = 1.5; }\n"
                 "}\n"
                 "assume Std { fn main() { } }"})
        self.assertIn("means `float`, which is not available", caught.exception.message)

    def test_an_oversized_literal_does_what_the_arithmetic_does(self):
        """tests/wasi/overflow.moss: 2^32 + 1 wraps to 1 under `WasiStd`'s
        i32 `Int`, and traps — at run time, when it is evaluated — under a
        test-local `Int` whose `.add` and `.mul` trap on overflow."""
        out = bytearray()
        original = interp.emit
        interp.emit = out.extend
        try:
            program = collect.load(
                "tests/wasi/overflow.moss", prelude=PRELUDE, root=REPO
            )
            lower = Lower(program)
            lower.run()
            with self.assertRaises(interp.MossPanic):
                interp.run_main(program, lower, None)
        finally:
            interp.emit = original
        self.assertEqual(bytes(out).decode(), OVERFLOW_OUT)

    def test_a_literal_follows_the_binding_of_int(self):
        """`7` is seven `one`s added up, not a constant, so a scope that
        binds `one` to 2 has `7` mean 14 (D25: to the end of the block)."""
        files = {
            "main.moss": (
                "assume Std {\n"
                "  fn main() {\n"
                "    bind one = 2;\n"
                "    putchar((char::A.code() + 7 - 7 - 7).char());\n"
                "  }\n"
                "}\n"
            )
        }
        # Inside the block `one` is 2, so `7` is 14: 65 + 14 - 14 - 14.
        self.assertEqual(run(files), "3")


class TestLists(unittest.TestCase):
    """D69: `float_list()` makes a packed list of whatever `Float` is
    bound to — under `WasiStd` an f64, in a block binding `WasiFloats32`
    an f32 — through one implementation over a per-type layout."""

    def test_over_the_moss_std(self):
        self.assertEqual(run({}, entry="tests/wasi/lists.moss"), LISTS_OUT)


class TestRepresentation(unittest.TestCase):
    def test_injection_is_where_the_tag_appears(self):
        """D58, on the interpreter: same program, same answer. Its own
        representation tags everything, which is exactly why this has to
        agree with the compiled one."""
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
        self.assertEqual(run({"main.moss": source}), "yyn")


class TestFunctors(unittest.TestCase):
    """D55: a functor maps one structure to another. `bind F;` installs its
    binds here; its totality against the declared result signature is
    checked once, at the functor."""

    SOURCE = (
        "type Text;\n"
        "fn emit(t: Text);\n"
        "val greeting: Text;\n"
        "context MiniStd = Text, emit, greeting;\n"
        "assume Std {\n"
        "  fn shout(c: Char) { putchar(c); putchar(char::exclam); }\n"
        "  functor CharStd: Std -> MiniStd {\n"
        "    bind Text = Char;\n"
        "    bind emit = shout;\n"
        "%s"
        "  }\n"
        "  assume MiniStd { fn app() { emit(greeting); emit(greeting); } }\n"
        "  fn main() { bind CharStd; app(); }\n"
        "}\n"
    )

    def test_application_installs_the_binds(self):
        source = self.SOURCE % "    bind greeting = char::h;\n"
        self.assertEqual(run({"main.moss": source}), "h!h!")

    def test_incomplete_functor_reports_at_the_functor(self):
        with self.assertRaises(LowerError) as ctx:
            run({"main.moss": self.SOURCE % ""})
        self.assertIn("does not bind `greeting`", ctx.exception.message)

    def test_self_application_is_rejected(self):
        source = (
            "context Empty = ;\n"
            "assume Std {\n"
            "  functor Loopy: Std -> Empty { bind Loopy; }\n"
            "  fn main() { bind Loopy; }\n"
            "}\n"
        )
        with self.assertRaises(LowerError) as ctx:
            run({"main.moss": source})
        self.assertIn("applies itself", ctx.exception.message)

    def test_bind_of_a_non_functor_is_an_error(self):
        source = "assume Std {\n  val c: Char;\n  fn main() { bind c; }\n}\n"
        with self.assertRaises(LowerError) as ctx:
            run({"main.moss": source})
        self.assertIn("is not a functor", ctx.exception.message)


class TestGenerics(unittest.TestCase):
    def test_iscell_idiom(self):
        """The shared-T functor idiom from src/cell.moss, end to end: a
        totally-applied context (IsCell[T=Int, Cell=MyCell]) provides
        detached methods keyed at the *resolved* receiver type, satisfied by
        binds whose providers are attached methods (which can see `this`),
        with a D44 rename: the fixture's `.fetch`/`.stash` are called here
        under names of this module's choosing, which is the rule a detached
        method obeys — it does not match on the name it was declared under
        (D61)."""
        files = {
            "main.moss": (
                'import "./tests/fixtures/inner.moss" use T;\n'
                'import "./tests/fixtures/cell.moss" use Cell, IsCell, .fetch as .cread, .stash as .cwrite;\n'
                "\n"
                "assume Std {\n"
                "  type MyCell CellInt;\n"
                "\n"
                "  fn MyCell.get(): Int {\n"
                "    match this { MyCell c => c.read() }\n"
                "  }\n"
                "\n"
                "  fn MyCell.put(x: Int) {\n"
                "    match this { MyCell c => c.write(x), }\n"
                "  }\n"
                "\n"
                "  context CellOps = IsCell[T=Int, Cell=MyCell];\n"
                "\n"
                "  assume CellOps {\n"
                "    fn bump(c: MyCell) {\n"
                "      c.cwrite(c.cread().add(one));\n"
                "    }\n"
                "  }\n"
                "\n"
                "  fn main() {\n"
                "    bind T=Int;\n"
                "    bind MyCell.cread=MyCell.get;\n"
                "    bind MyCell.cwrite=MyCell.put;\n"
                "    let c = MyCell (cell_int());\n"
                "    c.put(one);\n"
                "    bump(c);\n"
                "    if c.get().eq(one.add(one)) { putchar(char::y) }\n"
                "  }\n"
                "}\n"
            )
        }
        self.assertEqual(run(files), "y")


DOTS_DRIVER = (
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


class TestSelfHostedLexer(unittest.TestCase):
    """A driver over src/lex.moss prints one dot per token. The reference
    for token counts is the bootstrap lexer itself."""

    def lex_dots(self, source):
        return run({"dots.moss": DOTS_DRIVER}, entry="dots.moss", args=[source])

    def bootstrap_count(self, source):
        from mossc.lex import lex

        return len(lex(source)) - 1  # minus EOF

    def test_symbols(self):
        self.assertEqual(self.lex_dots("{ } ( ) ; |"), "." * 6 + "\n")

    def test_two_char_symbols(self):
        source = "== = => :: : <= << < >= >> > !="
        self.assertEqual(self.lex_dots(source), "." * 12 + "\n")

    def test_names_and_strings(self):
        source = 'import "a\\"b" as foo_2;'
        self.assertEqual(self.lex_dots(source), "." * 5 + "\n")

    def test_comments_and_whitespace(self):
        self.assertEqual(self.lex_dots("# a comment\n;\t;\n  ; # eof"), "." * 3 + "\n")

    def test_keyword_kinds(self):
        """The keyword trie distinguishes keywords from names sharing their
        prefixes/case — no string literals involved (D48)."""
        files = {
            "kinds.moss": (
                'import "./src/lex.moss" as lexer;\n'
                'import "./src/token.moss" use Eof, Name, Assume, As, ThisType,'
                " ThisValue, Loop, Str;\n"
                "\n"
                "assume Std {\n"
                "  fn main() {\n"
                "    bind lexer::src=first_arg();\n"
                "    bind lexer::at=cell_int();\n"
                "    bind lexer::mark=cell_int();\n"
    "    bind lexer::mark=cell_int();\n"
                "    loop {\n"
                "      match lexer::lex() {\n"
                "        Eof => break,\n"
                "        Assume => putchar(char::A),\n"
                "        As => putchar(char::a),\n"
                "        ThisType => putchar(char::T),\n"
                "        ThisValue => putchar(char::t),\n"
                "        Loop => putchar(char::l),\n"
                "        Name => putchar(char::n),\n"
                "        Str => putchar(char::s),\n"
                "        _ => putchar(char::dot),\n"
                "      }\n"
                "    }\n"
                "    putchar(char::newline);\n"
                "  }\n"
                "}\n"
            )
        }
        source = 'assume assumes as As this This loops loop "x" ;'
        self.assertEqual(run(files, entry="kinds.moss", args=[source]), "AnantTnls.\n")

    def test_literal_kinds(self):
        """D68's literals: `I` an integer, `D` a decimal, `b` the bad token
        the bootstrap lexer would raise an error for — a suffix. Length is
        no error: a literal is only arithmetic."""
        files = {
            "kinds.moss": (
                'import "./src/lex.moss" as lexer;\n'
                'import "./src/token.moss" use Eof, Name, Dot, Integer, Decimal, Bad;\n'
                "\n"
                "assume Std {\n"
                "  fn main() {\n"
                "    bind lexer::src=first_arg();\n"
                "    bind lexer::at=cell_int();\n"
                "    bind lexer::mark=cell_int();\n"
                "    loop {\n"
                "      match lexer::lex() {\n"
                "        Eof => break,\n"
                "        Integer => putchar(char::I),\n"
                "        Decimal => putchar(char::D),\n"
                "        Bad => putchar(char::b),\n"
                "        Name => putchar(char::n),\n"
                "        Dot => putchar(char::dot),\n"
                "        _ => putchar(char::underscore),\n"
                "      }\n"
                "    }\n"
                "    putchar(char::newline);\n"
                "  }\n"
                "}\n"
            )
        }
        cases = [
            ("x2 123 0 007 3.14 0.5", "nIIIDD"),
            ("1.add(2)", "I.n_I_"),
            ("1.x 1. 2.5.abs", "I.nI.D.n"),
            ("2x", "bn"),
            ("1.5f", "bn"),
            ("000000000000000000001.5 0.00000000000000000000001", "DD"),
            ("3.14159265358979323846264338327950288", "D"),
            ("123456789012345678901234567890", "I"),
        ]
        for source, expected in cases:
            with self.subTest(source=source):
                self.assertEqual(run(files, entry="kinds.moss", args=[source]), expected + "\n")

    def test_agrees_with_bootstrap_on_real_files(self):
        for rel in ["lib/bool.moss", "lib/num.moss", "src/lex.moss", "src/cli.moss"]:
            with self.subTest(file=rel):
                text = (REPO / rel).read_text(encoding="utf-8")
                expected = self.bootstrap_count(text)
                self.assertEqual(self.lex_dots(text), "." * expected + "\n")


if __name__ == "__main__":
    unittest.main()


class TestMerging(unittest.TestCase):
    """D43 consistent merging: duplicate keys unify their bindings instead
    of erroring, and only a genuinely unsatisfiable merge is rejected."""

    DECLS = (
        "  type Foo;\n"
        "  type A;\n"
        "  type B;\n"
        "  type C;\n"
        "  fn .gimme(): Foo;\n"
        "  context Ctx1 = A, B, A.gimme[Foo=B];\n"
        "  context Ctx2 = A, C, A.gimme[Foo=C];\n"
        "  context Ctx3 = Ctx1, Ctx2;\n"
    )

    def test_designer_example_merges(self):
        # Inside `assume Ctx3`, B and C are one atom: a.gimme() (declared to
        # yield B by Ctx1's item) is accepted where C is required.
        files = main_body(
            "bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;\n"
            "    bind A.gimme=giveq;\n"
            "    putchar(go(char::m));",
            decls=self.DECLS
            + "  assume Ctx3 {\n"
            "    fn go(a: A): C { a.gimme() }\n"
            "  }\n"
            "  fn giveq(): Char { char::q }",
        )
        self.assertEqual(run(files), "q")

    def test_unsatisfiable_merge_is_error(self):
        files = main_body(
            "()",
            decls="  type A;\n"
            "  fn .gimme(): Foo;\n"
            "  type Foo;\n"
            "  unit U1;\n"
            "  unit U2;\n"
            "  context Ctx1 = A, A.gimme[Foo=U1];\n"
            "  context Ctx2 = A, A.gimme[Foo=U2];\n"
            "  context Ctx3 = Ctx1, Ctx2;\n"
            "  assume Ctx3 { fn go() { } }",
        )
        with self.assertRaises(LowerError) as ctx:
            run(files)
        self.assertIn("cannot merge", ctx.exception.message)

    def test_binding_one_merged_symbol_binds_both(self):
        # After Ctx3 merges B with C, code under it can pass a B where a C
        # is expected and vice versa.
        files = main_body(
            "bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;\n"
            "    bind A.gimme=giveq;\n"
            "    roundtrip();",
            decls=self.DECLS
            + "  assume Ctx3 {\n"
            "    val b: B;\n"
            "    assume b {\n"
            "      fn takes_c(x: C) { }\n"
            "      fn roundtrip() { takes_c(b); }\n"
            "    }\n"
            "  }\n"
            "  fn giveq(): Char { char::q }",
        )
        files["main.moss"] = files["main.moss"].replace(
            "    roundtrip();", "    bind b=char::b;\n    roundtrip();"
        )
        self.assertEqual(run(files), "")


class TestIntList(unittest.TestCase):
    def test_push_get_set_length(self):
        files = main_body(
            "let xs = int_list();\n"
            "    xs.push(one);\n"
            "    xs.push(one.add(one));\n"
            "    if xs.length().eq(one.add(one)) { putchar(char::n) }\n"
            "    if xs.get(one).eq(one.add(one)) { putchar(char::g) }\n"
            "    xs.set(zero, zero);\n"
            "    if xs.get(zero).eq(zero) { putchar(char::s) }"
        )
        self.assertEqual(run(files), "ngs")

    def test_out_of_range_panics(self):
        files = main_body("let xs = int_list(); xs.get(zero);")
        with self.assertRaises(interp.MossPanic):
            run(files)


class TestMultiInstantiation(unittest.TestCase):
    def test_two_iscell_instantiations_coexist(self):
        """The designer's point about IsList: with detached methods and a
        unique receiver type per instantiation (NameList-style), one scope
        holds IsCell at T=Int *and* T=Char — the keys are (CA, read) and
        (CB, read), distinct atoms, so D51's collision never happens.

        Interpreted, so it holds the *front end* to the rule and no back
        end to anything. tests/wasi/generic.moss is the compiled version,
        run through both back ends — which is the coverage this test's
        absence of hid: the self-hosted compiler reported `e_method` for
        the whole idiom and nothing noticed."""
        files = {
            "main.moss": (
                'import "./tests/fixtures/inner.moss" use T;\n'
                'import "./tests/fixtures/cell.moss" use Cell, IsCell, .fetch as .cread, .stash as .cwrite;\n'
                "\n"
                "assume Std {\n"
                "  type CA CellInt;\n"
                "  type CB Char;\n"
                "\n"
                "  fn CA.rd(): Int { match this { CA c => c.read() } }\n"
                "  fn CA.wr(x: Int) { match this { CA c => c.write(x), } }\n"
                "  fn CB.rd(): Char { match this { CB c => c } }\n"
                "  fn CB.wr(x: Char) { }\n"
                "\n"
                "  context Both = IsCell[T=Int, Cell=CA], IsCell[T=Char, Cell=CB];\n"
                "\n"
                "  assume Both {\n"
                "    fn go(a: CA, b: CB): Char {\n"
                "      a.cwrite(a.cread().add(one));\n"
                "      b.cread()\n"
                "    }\n"
                "  }\n"
                "\n"
                "  fn main() {\n"
                "    bind T=Int;\n"
                "    bind CA.cread=CA.rd;\n"
                "    bind CA.cwrite=CA.wr;\n"
                "    bind T=Char;\n"
                "    bind CB.cread=CB.rd;\n"
                "    bind CB.cwrite=CB.wr;\n"
                "    let a = CA (cell_int());\n"
                "    let b = CB (char::z);\n"
                "    putchar(go(a, b));\n"
                "  }\n"
                "}\n"
            )
        }
        self.assertEqual(run(files), "z")


ARENA_DRIVER = (
    'import "./src/intern.moss" as intern;\n'
    'import "./src/lex.moss" as lexer;\n'
    "assume Std {\n"
    "  assume intern::Interner {\n"
    "    fn take(text: String, ns: Int, nl: Int): Int {\n"
    "      bind lexer::src=text;\n"
    "      intern::intern(ns, nl)\n"
    "    }\n"
    "  }\n"
    "  fn main() {\n"
    "    bind intern::ichars=int_list();\n"
    "    bind intern::istarts=int_list();\n"
    "    bind intern::ilens=int_list();\n"
    "    bind intern::ibuckets=int_list();\n"
    "    bind intern::ilink=int_list();\n"
    "    bind intern::imask=cell_int();\n"
    "    let s = first_arg();\n"
    "    let two = one.add(one);\n"
    "    let a = take(s, zero, two);\n"
    "    let b = take(s, two, two);\n"
    "    let c = take(s, zero, two);\n"
    "    intern::put(a);\n"
    "    intern::put(b);\n"
    "    if a.eq(c) { putchar(char::y) } else { putchar(char::n) }\n"
    "    if a.eq(b) { putchar(char::n) } else { putchar(char::d) }\n"
    "    putchar(char::newline);\n"
    "  }\n"
    "}\n"
)


class TestInterner(unittest.TestCase):
    def test_names_outlive_their_source(self):
        """The point of the codepoint arena: `take` is the only thing that
        binds `lexer::src`, so by the time `main` prints the names back the
        source they were read from is out of scope entirely. Ids are stable
        (same text, same id) and distinct texts differ."""
        self.assertEqual(
            run({"main.moss": ARENA_DRIVER}, args=["abcd"]), "abcdyd\n"
        )


class TestSelfHostedEmitter(unittest.TestCase):
    """The back end's encoder, in Moss: tests/wasi/emit.moss writes a WASI
    module to standard output, and that module says hello."""

    def test_emits_a_module_that_runs(self):
        import shutil
        import subprocess

        from .test_build import wasm_file

        module = run_bytes({}, entry="tests/wasi/emit.moss")
        self.assertEqual(module[:8], b"\0asm\x01\0\0\0")
        path = wasm_file(module)
        wasmtime = shutil.which("wasmtime")
        self.assertIsNotNone(wasmtime, "wasmtime is not on PATH")
        result = subprocess.run(
            [wasmtime, path], capture_output=True, text=True, timeout=120
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "Hello, world!\n")


PARSE_DRIVER = (
    'import "./src/ast.moss" as ast;\n'
    'import "./src/dump.moss" as dump;\n'
    'import "./src/intern.moss" as intern;\n'
    'import "./src/lex.moss" as lexer;\n'
    'import "./src/syntax.moss" as syntax;\n'
    "assume Std {\n"
    "  fn main() {\n"
    "    bind ast::nkind=int_list();\n"
    "    bind ast::na=int_list();\n"
    "    bind ast::nb=int_list();\n"
    "    bind ast::nc=int_list();\n"
    "    bind ast::nd=int_list();\n"
    "    bind ast::kids=int_list();\n"
    "    bind ast::pnames=int_list();\n"
    "    bind ast::pstart=int_list();\n"
    "    bind ast::plen=int_list();\n"
    "    bind ast::itexts=str_list();\n"
    "    bind intern::ichars=int_list();\n"
    "    bind intern::istarts=int_list();\n"
    "    bind intern::ilens=int_list();\n"
    "    bind intern::ibuckets=int_list();\n"
    "    bind intern::ilink=int_list();\n"
    "    bind intern::imask=cell_int();\n"
    "    bind lexer::src=pwd.join(first_arg()).read();\n"
    "    bind lexer::at=cell_int();\n"
    "    bind lexer::mark=cell_int();\n"
    "    bind syntax::tok=cell_int();\n"
    "    bind syntax::tstart=cell_int();\n"
    "    bind syntax::tend=cell_int();\n"
    "    bind syntax::tok2=cell_int();\n"
    "    bind syntax::t2start=cell_int();\n"
    "    bind syntax::t2end=cell_int();\n"
    "    bind syntax::errs=cell_int();\n"
    "    bind syntax::errpos=cell_int();\n"
    "    go();\n"
    "  }\n"
    "  assume ast::Ast, syntax::Cursor, intern::Interner,\n"
    "    lexer::src, lexer::at, lexer::mark {\n"
    "    fn go() {\n"
    "      let roots = int_list();\n"
    "      syntax::parse_file(roots);\n"
    "      if syntax::errs.read().gt(zero) { putchar(char::question); }\n"
    "      dump::dump_file(roots);\n"
    "    }\n"
    "  }\n"
    "}\n"
)

# Literal snippets (D68) for the parser agreement test, beside the
# operator tables in tests/test_parse.py: (source, refused).
LITERAL_SNIPPETS = [
    ("1.add(2)", False),
    ("3.14 * r * r", False),
    ("-1 * 2", False),
    ("(-1).abs()", False),
    ("-(1.abs())", False),
    ("-1.abs()", True),
    ("-2.5.sqrt()", True),
    ("- 1.x", True),
    ("x - 1.abs()", False),
    ("1 << n + 1", True),
    ("0.5 + 007", False),
    ("f(10, 0.25)", False),
]

# What tests/wasi/lambda.moss prints: each letter is named beside the
# line that prints it.
LAMBDA_OUT = "ABxCypqmnBwgdD4\n"

# Anonymous-function snippets (D70) for the same test: (source, refused).
LAMBDA_SNIPPETS = [
    ("bind f = (x) => x;", False),
    ("bind f = () => g();", False),
    ("bind f = (x: Int, y): Char => { h(x); y };", False),
    ("bind f = (x,) => x, g = (y) => y;", False),
    ("bind T.m = (n: This) => this.add(n);", False),
    ("bind f = (): () => {};", False),
    ("bind f = (x) => (x).g();", False),
    ("let v = (x) => x;", False),
    ("f((x) => x)", False),
    ("(x).g()", False),
    ("(x)", False),
    ("()", False),
    ("(x + y) * z", False),
    ("(x, y)", True),
    ("(x: Int)", True),
    ("bind f = (x) =>;", True),
    ("bind f = (x): => x;", True),
    ("bind f = (1) => x;", True),
]

# Everything that is written in the language — every live .moss file except
# `examples/escape.moss`, which needs string literals (D48) and parses under
# neither parser.
def corpus():
    for folder, pattern in (
        ("src", "*.moss"),
        ("lib", "*.moss"),
        ("examples", "*.moss"),
        ("tests/wasi", "*.moss"),
    ):
        for path in sorted((REPO / folder).glob(pattern)):
            rel = str(path.relative_to(REPO))
            if rel == "examples/escape.moss":
                continue
            yield rel


class TestSelfHostedSyntax(unittest.TestCase):
    """src/syntax.moss against bootstrap/mossc/parse.py, over the whole
    corpus: both write the tree in the compact form of mossc/sexpr.py, and
    the two must agree character for character. This is the front end's
    real check — an approximate parser can produce a plausible letter
    dump, but it cannot produce the bootstrap's tree."""

    def test_agrees_with_the_bootstrap_parser(self):
        from mossc.parse import parse
        from mossc.sexpr import dump

        for rel in corpus():
            with self.subTest(file=rel):
                expected = dump(parse((REPO / rel).read_text(encoding="utf-8")))
                actual = run_driver(PARSE_DRIVER, [rel]).decode("utf-8")
                self.assertEqual(actual, expected + "\n")

    def test_operators_agree_with_the_bootstrap_parser(self):
        """D68's precedence table, snippet by snippet: the same tree where
        the bootstrap groups, and a syntax error where it refuses. The
        self-hosted parser reports an error as a count, so a refusal is
        the `?` its driver prints."""
        from .test_parse import OPERATOR_ERRORS, OPERATOR_GROUPINGS

        cases = [(source, False) for source, _ in OPERATOR_GROUPINGS]
        cases += [(source, True) for source, _ in OPERATOR_ERRORS]
        cases += LITERAL_SNIPPETS
        self.agree_on_snippets(cases)

    def test_lambdas_agree_with_the_bootstrap_parser(self):
        """D70: an anonymous function is told from `()` and from a
        parenthesized expression by what follows its parenthesis. The
        same trees, snippet by snippet — where it is legal and where it
        is not, which is for lowering to say — and the same refusals."""
        self.agree_on_snippets(LAMBDA_SNIPPETS)

    def agree_on_snippets(self, cases):
        import tempfile

        from mossc.parse import ParseError, parse
        from mossc.sexpr import dump

        with tempfile.TemporaryDirectory(prefix="moss-ops-") as folder:
            for i, (source, refused) in enumerate(cases):
                with self.subTest(source=source):
                    text = f"fn f() {{ {source} }}\n"
                    (Path(folder) / f"{i}.moss").write_text(text, encoding="utf-8")
                    actual = run_driver(
                        PARSE_DRIVER, [f"{i}.moss"], preopen=f"{folder}::."
                    ).decode("utf-8")
                    if refused:
                        with self.assertRaises(ParseError):
                            parse(text)
                        self.assertTrue(actual.startswith("?"), actual)
                    else:
                        self.assertEqual(actual, dump(parse(text)) + "\n")


SCOPE_DRIVER = (
    "import \"./src/ast.moss\" as ast;\n"
    "import \"./src/boot.moss\" use Boot, Compiler;\n"
    "import \"./src/bytes.moss\" use *;\n"
    "import \"./src/intern.moss\" as intern;\n"
    "import \"./src/prog.moss\" as prog;\n"
    "\n"
    "assume Std {\n"
    "  fn main() {\n"
    "    bind Boot;\n"
    "    go();\n"
    "  }\n"
    "\n"
    "  assume Compiler {\n"
    "    fn tab() { putchar(char::tab); }\n"
    "\n"
    "    fn go() {\n"
    "      let entry = prog::load_program(arg_at(one), arg_at(one.add(one)));\n"
    "      var e = zero;\n"
    "      while e.lt(prog::error_count()) {\n"
    "        putchar(char::exclam);\n"
    "        put_int(prog::err_code.get(e));\n"
    "        putchar(char::space);\n"
    "        print(prog::module_path(prog::err_mod.get(e)));\n"
    "        if ast::is_some(prog::err_name.get(e)) {\n"
    "          putchar(char::space);\n"
    "          intern::put(prog::err_name.get(e));\n"
    "        }\n"
    "        putchar(char::newline);\n"
    "        e = e.add(one);\n"
    "      }\n"
    "      var i = zero;\n"
    "      while i.lt(prog::ent_mod.length()) {\n"
    "        print(prog::module_path(prog::ent_mod.get(i)));\n"
    "        tab();\n"
    "        put_int(prog::ent_ns.get(i));\n"
    "        tab();\n"
    "        intern::put(prog::ent_name.get(i));\n"
    "        tab();\n"
    "        let to = prog::ent_to.get(i);\n"
    "        if prog::ent_ns.get(i).eq(prog::ns_alias()) {\n"
    "          print(prog::module_path(to));\n"
    "          tab();\n"
    "          putchar(char::star);\n"
    "        } else {\n"
    "          print(prog::module_path(prog::sym_mod.get(to)));\n"
    "          tab();\n"
    "          intern::put(prog::sym_name.get(to));\n"
    "        }\n"
    "        putchar(char::newline);\n"
    "        i = i.add(one);\n"
    "      }\n"
    "      var a = zero;\n"
    "      while a.lt(prog::att_mod.length()) {\n"
    "        print(prog::module_path(prog::att_mod.get(a)));\n"
    "        tab();\n"
    "        putchar(char::at);\n"
    "        tab();\n"
    "        intern::put(prog::sym_name.get(prog::att_recv.get(a)));\n"
    "        putchar(char::dot);\n"
    "        intern::put(prog::att_name.get(a));\n"
    "        tab();\n"
    "        print(prog::module_path(prog::sym_mod.get(prog::att_sym.get(a))));\n"
    "        tab();\n"
    "        intern::put(prog::att_name.get(a));\n"
    "        putchar(char::newline);\n"
    "        a = a.add(one);\n"
    "      }\n"
    "    }\n"
    "  }\n"
    "}\n"
)


class TestSelfHostedCollectScopes(unittest.TestCase):
    """src/prog.moss against bootstrap/mossc/collect.py: the module graph,
    the symbols every declaration gets, and the scope each module ends up
    with — names, detached methods, aliases, and attached methods keyed by
    receiver. Compared as a set of (module, namespace, name, target)
    rows, over the compiler's own sources: 35 modules, 3061 rows."""

    def rows_from_bootstrap(self, entry):
        import os

        from mossc import ast, collect

        program = collect.load(entry, prelude=PRELUDE, root=REPO)

        def rel(path):
            return os.path.relpath(path, REPO)

        def sname(sym):
            if isinstance(sym.decl, ast.Fndef):
                return sym.decl.name.name
            return sym.name

        rows = []
        for m in program.modules.values():
            for name, sym in m.names.items():
                rows.append((rel(m.path), "0", name, rel(sym.module.path), sname(sym)))
            for name, sym in m.detached.items():
                rows.append((rel(m.path), "1", name, rel(sym.module.path), sname(sym)))
            for name, sub in m.aliases.items():
                rows.append((rel(m.path), "2", name, rel(sub.path), "*"))
            for (_, mn), sym in m.attached.items():
                rows.append(
                    (rel(m.path), "@", f"{sym.receiver.name}.{mn}",
                     rel(sym.module.path), mn)
                )
        return sorted(rows)

    def rows_from_moss(self, entry):
        out = run_driver(SCOPE_DRIVER, ["lib/prelude.moss", entry]).decode("utf-8")
        rows = []
        for line in out.strip().split("\n"):
            self.assertFalse(line.startswith("!"), f"collect error: {line}")
            rows.append(tuple(line.split("\t")))
        return sorted(rows)

    def test_scopes_match_the_bootstrap(self):
        for entry in ("src/main.moss", "tests/wasi/full.moss"):
            with self.subTest(entry=entry):
                self.assertEqual(
                    self.rows_from_moss(entry), self.rows_from_bootstrap(entry)
                )


COMPILER_DRIVER = (
    'import "./src/cli.moss" as cli;\n'
    "assume Std {\n"
    "  fn main() { cli::cli(); }\n"
    "}\n"
)


class TestSelfHostedBackEnd(unittest.TestCase):
    """The self-hosted compiler, end to end: src/ reads a Moss program off
    disk, resolves it, and writes a WASI module that runs.

    The language it covers is the primitive context of D52 — `Wasm`
    instructions, `Wasi` imports, `Bool` for `if` to eliminate, plain
    functions, `let`/`var`, `if`/`else`, `while`, `loop`/`break` and
    `return`. That is what tests/wasi/raw.moss and tests/wasi/prim.moss
    are written in, and the bootstrap compiles them too, so both
    compilers can be held to the same program's behaviour.
    """

    def compile_with_moss(self, entry, prelude=""):
        return run_driver(COMPILER_DRIVER, [entry], argv0=prelude)

    def wasmtime_run(self, module, expect_code=0):
        import shutil
        import subprocess

        from .test_build import wasm_file

        path = wasm_file(module)
        result = subprocess.run(
            [shutil.which("wasmtime"), path],
            capture_output=True,
            text=True,
            timeout=120,
        )
        self.assertEqual(result.returncode, expect_code, result.stderr)
        return result.stdout

    def test_raw_wasi_program(self):
        module = self.compile_with_moss("tests/wasi/raw.moss")
        self.assertEqual(module[:8], b"\0asm\x01\0\0\0")
        # proc_exit(3), exactly as under the bootstrap's back end.
        self.assertEqual(self.wasmtime_run(module, expect_code=3), "A\n")

    def test_control_flow_and_i64(self):
        module = self.compile_with_moss("tests/wasi/prim.moss")
        self.assertEqual(self.wasmtime_run(module), "ABKDKJGG\n")

    def test_calls_across_modules(self):
        """A function is compiled in the module that declared it: its
        names resolve there, not where it was called from."""
        module = self.compile_with_moss("tests/wasi/across.moss")
        self.assertEqual(self.wasmtime_run(module), "AC\n")

    def test_tags_units_and_match(self):
        """D58 and D59's value model: a nominal tag over a scalar is that
        scalar, a union of units is one scalar of discriminant, and
        `match` is the only thing that has to tell them apart. Nothing
        here allocates."""
        module = self.compile_with_moss("tests/wasi/tags.moss")
        self.assertEqual(self.wasmtime_run(module), "EBACEAB\n")

    def test_contextual_vals(self):
        """D2: a val is the only context that exists at runtime, so a
        function that assumes one is implicitly parameterized by it and
        that parameterization is a real Wasm parameter. Covers a need
        threaded through a frame that does not use it, a `bind` shadowed
        inside a block, and one rebound at the same level."""
        module = self.compile_with_moss("tests/wasi/ctx.moss")
        self.assertEqual(self.wasmtime_run(module), "CEECI\n")

    def test_functor_and_the_static_context(self):
        """The whole of what a `bind` can do: a type, a val, a plain
        function and a detached method at a receiver, all provided at once
        by applying a functor (D55). Code written against the signature
        alone still compiles to a direct call (D2), which means the callee
        is compiled once per environment that reaches it, and the keys are
        compared after canonicalization through the bindings in force
        (D43) — the callee knows its receiver by a symbol the caller has
        since bound."""
        module = self.compile_with_moss("tests/wasi/functor.moss")
        self.assertEqual(self.wasmtime_run(module), "ABD\n")

    def test_a_union_wider_than_one_scalar(self):
        """The rest of D59's value model. `Read | Eof` is a discriminant
        beside room for the payload, which needs three things at once: a
        member injecting where it enters the union, a `match` that reads
        the discriminant and then the payload, and a function returning
        two scalars — a multi-result type, which is what the bootstrap's
        own back end already emits. This is the shape src/lex.moss's
        `peek()` returns."""
        module = self.compile_with_moss("tests/wasi/wide.moss")
        self.assertEqual(self.wasmtime_run(module), "ABCD\n")

    def test_a_bulk_write(self):
        """`put_bytes`, compiled by the compiler written in Moss — which is
        also how it writes its own output, so this is the one primitive
        whose cost the fixpoint test pays directly."""
        module = self.compile_with_moss("tests/wasi/bulk.moss")
        self.assertEqual(self.wasmtime_run(module), "hi!\n")

    def test_a_generic_container(self):
        """A receiver an application binds. A context item may key a
        detached method on an *abstract* type — `context IsList = L.get;` —
        which `IsList[T=Int, L=IntArr]` then fixes, and the provision has
        to land at `IntArr` rather than at `L` or no call finds it. That is
        D51's generic container, and the same mechanism the library's own
        `String.get[Elem=Char]` uses with the receiver written concretely,
        so this is the case where the brackets actually have to be read.

        Two instantiations coexist here, which is the point: the keys are
        (IntArr, get) and (CharArr, get), distinct atoms, so nothing
        merges."""
        module = self.compile_with_moss("tests/wasi/generic.moss")
        self.assertEqual(self.wasmtime_run(module), "hiDCC\n")

    def test_a_fn_bind_captures_at_the_bind_site(self):
        """D26: a bound provider runs with the vals in force where it was
        bound. Its captured values travel under keys of their own, so a
        rebind between the bind and the call — in the calling frame, a
        frame above it, or inside a provider that captured a bind of its
        own — cannot reach it, through a plain fn or a detached method."""
        module = self.compile_with_moss("tests/wasi/capture.moss")
        self.assertEqual(self.wasmtime_run(module), "CCCCE\n")

    def test_a_bare_block_scopes_its_binds_and_lets(self):
        """D65: a bare block is an expression. The val and fn binds made
        inside it are gone at its end, a `let` inside it shadows only
        until then while an assignment to an outer `var` stays, and its
        value is its tail — as a `let` initializer and as an argument."""
        module = self.compile_with_moss("tests/wasi/block.moss")
        self.assertEqual(self.wasmtime_run(module), "EDCBGAIEE\n")

    def test_a_fn_bound_to_a_contextual_fn(self):
        """D66: `bind f = g;` with `g` abstract is a bind of a forwarder
        to `g`, so it captures `g` as provided at the bind — at a type a
        bind fixed, inside a functor written against the signature alone,
        and for a primitive. Rebinding `g` afterwards, in the same frame
        or deeper in the call tree, does not reach `f`."""
        module = self.compile_with_moss("tests/wasi/forward.moss")
        self.assertEqual(self.wasmtime_run(module), "CCEDEC\n")

    def test_a_plain_fn_provides_a_method(self):
        """D54: a plain function may provide a method, with the method's
        declared parameters — the receiver excluded — so it cannot see
        what the method was called on, though the call still evaluates
        the receiver. An attached provider takes it as `this`. The
        self-hosted compiler used to pass the receiver to a plain
        provider anyway (invalid Wasm here; a plain fn written to take it
        was accepted), and the bootstrap's Wasm back end refused the call;
        all three now run it alike."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/plain_provider.moss"
        self.assertEqual(run_bytes({}, entry=entry), b"abrcd\n")
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "abrcd\n")
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(out), "abrcd\n")

    def test_a_plain_fn_provider_cannot_take_or_see_the_receiver(self):
        """D54, the other half: a plain provider written to take the
        receiver as a first parameter is a signature mismatch (the
        bootstrap's message; `e_signature`, `S`, named by the method, in
        the self-hosted compiler, which used to accept it), and `this` in
        one is an error (`e_this`, `T`)."""
        from .test_build import compile_wasm

        cases = {
            "plain_provider_receiver": (
                "method bind signature mismatch: provider `second` has "
                "(Box[Char=Char]) -> Char, need () -> Char; a plain function "
                "providing a method takes the method's parameters, which "
                "exclude the receiver (D54)",
                "!S {} m\n",
            ),
            "plain_provider_this": (
                "`this` is only legal inside method bodies",
                "!T {}\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertEqual(out.decode("utf-8"), report.format(entry))

    def test_anonymous_functions_at_binds(self):
        """D70: `(params) => body` as a fn or method bind's right side —
        unannotated and annotated, capturing a `let` and a `var` by value
        (an assignment after the bind is not seen), capturing context
        items as they are at the bind (a later rebind of a fn or a val is
        not seen), in a functor's body, as a method provider seeing the
        receiver as `this` through a wrapper and with none at all, with
        a block body, one inside another, and one yielding a float (whose
        body is a block yielding an f64, which the bootstrap's Wasm back end
        typed as an i32 for any bare block before). The interpreter, the
        bootstrap's Wasm back end and the self-hosted compiler agree."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/lambda.moss"
        self.assertEqual(run_bytes({}, entry=entry).decode("utf-8"), LAMBDA_OUT)
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), LAMBDA_OUT)
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(out), LAMBDA_OUT)

    def test_anonymous_function_errors(self):
        """D70's errors in both compilers: a parameter count or an
        annotation that is not the bound declaration's (`e_signature`,
        `S`, named by the bound fn), an assignment to a captured local
        and a lambda anywhere but a bind (`e_lambda`, `U`), `this` in one
        bound to a fn (`e_this`, `T`), and a lambda calling the fn it is
        bound to, which means what it meant at the bind — here nothing
        (`c_unbound`, `?Q`)."""
        from .test_build import compile_wasm

        cases = {
            "lambda_arity": (
                "the anonymous function bound to `step` takes 2 parameter(s), "
                "but `step` takes 1 (D70)",
                "!S {} step\n",
            ),
            "lambda_annotation": (
                "parameter `c` of the anonymous function bound to `step` is "
                "annotated `Int`, but `step` declares `Char` there (D70)",
                "!S {} step\n",
            ),
            "lambda_assign": (
                "`seen` is a local captured by an anonymous function, which "
                "captures it by value and may not assign it (D70)",
                "!U {} seen\n",
            ),
            "lambda_outside": (
                "an anonymous function may only be the right side of a fn or "
                "method bind (D70)",
                "!U {}\n",
            ),
            "lambda_this": (
                "`this` is only legal inside method bodies; an anonymous "
                "function bound to a fn has no receiver",
                "!T {}\n",
            ),
            "lambda_recursive": (
                "`step` is not available in the context here",
                "?Q {} step step\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertTrue(
                    out.decode("utf-8").startswith(report.format(entry)), out[:200]
                )

    def test_float_scalars(self):
        """D67: f32 and f64 are scalars like i32 and i64 — in locals,
        parameters, results, record fields and memory, through the whole
        of `Wasm`'s float half, f32 rounding to single precision."""
        module = self.compile_with_moss("tests/wasi/float.moss")
        self.assertEqual(self.wasmtime_run(module), FLOAT_OUT)

    def test_operators(self):
        """D68's operators, compiled as the calls of `num.moss`'s methods
        they are, at `Int` and at `Bool`."""
        module = self.compile_with_moss("tests/wasi/ops.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), OPS_OUT)

    def test_literals(self):
        """D68's literals, at f64 and — in a block that binds
        `WasiFloats32` — at f32, from one `report` function."""
        module = self.compile_with_moss("tests/wasi/literals.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), LITERALS_OUT)

    def test_float_lists(self):
        """D69's packed lists: an `IntList` and a `FloatList` side by side,
        at f64 and — in a block binding `WasiFloats32` — at f32, from one
        `report` function and one list implementation."""
        module = self.compile_with_moss("tests/wasi/lists.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), LISTS_OUT)

    def test_an_oversized_literal_does_what_the_arithmetic_does(self):
        """tests/wasi/overflow.moss in both compilers: the wrapped literal,
        the checked one that fits, and then a trap (wasmtime's exit status
        for one is 134) where the checked one does not."""
        import subprocess

        from .test_build import compile_wasm, wasm_file, wasmtime

        result = subprocess.run(
            [wasmtime(), wasm_file(compile_wasm({}, entry="tests/wasi/overflow.moss"))],
            capture_output=True, text=True, timeout=120,
        )
        self.assertEqual((result.returncode, result.stdout), (134, OVERFLOW_OUT))
        self.assertIn("unreachable", result.stderr)
        module = self.compile_with_moss("tests/wasi/overflow.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module, expect_code=134), OVERFLOW_OUT)

    def test_an_operator_the_context_lacks_is_the_method_call_error(self):
        """D68: `c + c` where nothing provides `Char.add` is reported
        exactly as `c.add(c)` is — `e_method` naming `add` — by both
        compilers."""
        from .test_build import compile_wasm

        with self.assertRaises(LowerError):
            compile_wasm({}, entry="tests/wasi/unprovided.moss")
        report = self.compile_with_moss(
            "tests/wasi/unprovided.moss", prelude="lib/prelude.moss"
        ).decode("utf-8")
        self.assertIn("?K tests/wasi/unprovided.moss by_operator add\n", report)
        self.assertIn("?K tests/wasi/unprovided.moss by_method add\n", report)

    def test_float(self):
        """D67's `Float`, written against `Std` and run under two of them:
        `WasiStd`, which provides it at f64, and one assembled the same
        way with `WasiFloats32`. One `report` function, two compiled
        specializations, two precisions."""
        module = self.compile_with_moss("tests/wasi/newton.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), NEWTON_OUT)

    def test_matches_the_bootstrap_on_behaviour(self):
        """Two compilers, one program: the bytes differ — the bootstrap
        emits shims this back end has no need for — but what the modules
        do is the same."""
        from .test_build import compile_wasm, run_wasm

        cases = (
            ("tests/wasi/prim.moss", "ABKDKJGG\n"),
            ("tests/wasi/across.moss", "AC\n"),
            ("tests/wasi/tags.moss", "EBACEAB\n"),
            ("tests/wasi/ctx.moss", "CEECI\n"),
            ("tests/wasi/functor.moss", "ABD\n"),
            ("tests/wasi/wide.moss", "ABCD\n"),
            ("tests/wasi/generic.moss", "hiDCC\n"),
            ("tests/wasi/bulk.moss", "hi!\n"),
            ("tests/wasi/capture.moss", "CCCCE\n"),
            ("tests/wasi/block.moss", "EDCBGAIEE\n"),
            ("tests/wasi/forward.moss", "CCEDEC\n"),
            ("tests/wasi/float.moss", FLOAT_OUT),
            ("tests/wasi/plain_provider.moss", "abrcd\n", "lib/prelude.moss"),
            ("tests/wasi/lambda.moss", LAMBDA_OUT, "lib/prelude.moss"),
            ("tests/wasi/phantom.moss", "42\nz\n42 z\n", "lib/prelude.moss"),
            ("tests/wasi/packed.moss", "3000000000\n", "lib/prelude.moss"),
            ("tests/wasi/concrete_list.moss", "2\n", "lib/prelude.moss"),
            # An anonymous function reaching an `IntList` method in a
            # function generic over `Std`: the context's provision, not
            # `Packed`'s attached method (D70, D77).
            ("tests/wasi/lambda_list.moss", "2\n7\n", "lib/prelude.moss"),
            ("tests/wasi/resolution.moss", "91122\n", "lib/prelude.moss"),
            ("tests/wasi/gc_tree.moss", GC_TREE_OUT, "lib/prelude.moss"),
            ("tests/wasi/gc_ast.moss", GC_AST_OUT, "lib/prelude.moss"),
            ("tests/wasi/gc_two.moss", GC_TWO_OUT, "lib/prelude.moss"),
            # D75 under D77: `Ref`'s attached `.deref` unless a detached
            # `.deref` in scope is provided at the receiver.
            ("tests/wasi/gc_resolution.moss", "42\n7\n42\n", "lib/prelude.moss"),
            # Written against `Std`, so it needs the prelude in scope.
            ("tests/wasi/records.moss", RECORDS_OUT, "lib/prelude.moss"),
            ("tests/wasi/nested.moss", NESTED_OUT, "lib/prelude.moss"),
            ("tests/wasi/newton.moss", NEWTON_OUT, "lib/prelude.moss"),
            ("tests/wasi/shadow.moss", SHADOW_OUT, "lib/prelude.moss"),
            ("tests/wasi/ops.moss", OPS_OUT, "lib/prelude.moss"),
            ("tests/wasi/literals.moss", LITERALS_OUT, "lib/prelude.moss"),
            ("tests/wasi/lists.moss", LISTS_OUT, "lib/prelude.moss"),
        )
        for entry, expected, *prelude in cases:
            with self.subTest(entry=entry):
                mine = self.wasmtime_run(self.compile_with_moss(entry, *prelude))
                self.assertEqual(mine, expected)
                self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), expected)

    def test_gc_references(self):
        """D75: a recursive tree through `Ref[Boxed=Expr]`, and the
        thesis's interpreter written against an abstract `ExprRef`, run
        over an index pool and then over the GC heap through the
        knot-tied `Node`, and an `Expr` and a `Stmt` boxed side by side
        in one scope, give the same output interpreted and in both
        compilers. Each compiled module has its struct types as a `rec`
        group, builds with `struct.new`, and — since no program forges a
        reference — contains no `ref.cast`."""
        from .test_build import compile_wasm, run_wasm

        for entry, expected in (
            ("tests/wasi/gc_tree.moss", GC_TREE_OUT),
            ("tests/wasi/gc_ast.moss", GC_AST_OUT),
            ("tests/wasi/gc_two.moss", GC_TWO_OUT),
        ):
            with self.subTest(entry=entry):
                self.assertEqual(run({}, entry=entry), expected)
                for module in (
                    self.compile_with_moss(entry, prelude="lib/prelude.moss"),
                    compile_wasm({}, entry=entry),
                ):
                    self.assertTrue(starts_with_rec_group(module))
                    text = wasm_text(module)
                    self.assertIn("struct.new", text)
                    self.assertIn("struct.get", text)
                    self.assertNotIn("ref.cast", text)
                    self.assertEqual(run_wasm(module), expected)

    def test_a_forged_reference_traps_where_it_is_forged(self):
        """D75: rebuilding a `Ref` from the bare `StructRef` it was taken
        apart into is the one cast. At the type the struct was made at it
        succeeds; at another it traps at the construction, in the
        interpreter and both compilers, before anything is read."""
        import subprocess

        from mossc.interp import MossPanic

        from .test_build import compile_wasm, wasm_file, wasmtime

        entry = "tests/wasi/gc_forge.moss"
        with self.assertRaises(MossPanic) as caught:
            run({}, entry=entry)
        self.assertIn("cast failure", str(caught.exception))
        for module in (
            self.compile_with_moss(entry, prelude="lib/prelude.moss"),
            compile_wasm({}, entry=entry),
        ):
            self.assertEqual(wasm_text(module).count("ref.cast"), 2)
            result = subprocess.run(
                [wasmtime(), wasm_file(module)], capture_output=True, text=True, timeout=120
            )
            self.assertEqual((result.returncode, result.stdout), (134, "3\n4\n"))
            self.assertIn("cast failure", result.stderr)

    def test_abstract_functors(self):
        """D78: a functor with no body is an abstract functor, anywhere —
        a requirement that a scope provides by binding a functor to it,
        and that `bind F;` applies through whatever it is bound to. The
        interpreter with `Trace` bound to two tracers in one program and
        applied inside a generic function; `Fast`, whose result holds the
        type `Float`, bound to f64 and to f32 floats for one kernel; a
        library applying an abstract allocator whose result holds the
        handle type, for two types, bound to a GC-heap allocator in one
        block and to a pool of indices in another, each with fewer inputs
        than the allocator takes; an abstract functor producing a val,
        evaluated at each application; and D75's `gc_bodiless`,
        whose bodiless functor outside lib/wasm.moss is now legal and
        bound to `Heap`. Interpreted and in both compilers."""
        from .test_build import compile_wasm, run_wasm

        for entry, expected in (
            ("tests/wasi/functor_trace.moss", FUNCTOR_TRACE_OUT),
            ("tests/wasi/functor_fast.moss", FUNCTOR_FAST_OUT),
            ("tests/wasi/functor_alloc.moss", "21\n4\n14\n" * 2),
            # Two applications whose bindings make one concrete handle
            # type: two generations, so two `.load`s (the self-hosted
            # compiler used to refuse it rather than conflate them).
            ("tests/wasi/functor_same_handle.moss", "21\n4\n14\n"),
            ("tests/wasi/functor_val.moss", "10\n20\n30\n3\n5\n6\n0\n"),
            ("tests/wasi/gc_bodiless.moss", "42\n"),
        ):
            with self.subTest(entry=entry):
                self.assertEqual(run({}, entry=entry), expected)
                for module in (
                    self.compile_with_moss(entry, prelude="lib/prelude.moss"),
                    compile_wasm({}, entry=entry),
                ):
                    self.assertEqual(run_wasm(module), expected)

    def test_abstract_functor_binds_are_checked(self):
        """D78: `bind F = G;` needs `G`'s inputs among `F`'s and `G`'s
        result exactly `F`'s, checked where the bind is written (more
        inputs, a missing output, an extra output); `G`'s binds may use
        only `G`'s inputs, whatever an application has; `bind F;` with
        nothing bound to `F` is the ordinary error for a requirement not
        in context; a functor bound to one whose result holds a type must
        provide that type too; each item of the result must be made at the
        type the application promises; and no value of a type `bind F;`
        made may leave its block. Rejected in both compilers."""
        from .test_build import compile_wasm

        cases = {
            "functor_inputs": (
                "`Counting` cannot be bound to `Trace`: it needs `counter`, "
                "which `Trace` does not take",
                "!X {} counter\n",
            ),
            "functor_missing": (
                "`Half` cannot be bound to `Trace`: it does not provide "
                "`flush`, which `Trace` promises",
                "!X {} flush\n",
            ),
            "functor_extra": (
                "`Chatty` cannot be bound to `Trace`: it also provides "
                "`flush`, which `Trace` does not",
                "!X {} flush\n",
            ),
            # A bound functor relies only on its own argument signature,
            # though every application here has more.
            "functor_body": (
                "functor `Tally`, bound to an abstract functor, may rely only "
                "on its own argument signature (D78): calling `count` needs "
                "`counter`",
                "!P {} counter\n",
            ),
            "functor_unbound": (
                "`Trace` is not available in the context here",
                "?Q {} demo Trace\n",
            ),
            # A type in the result is provided like any other item.
            "functor_type": (
                "`Twice` cannot be bound to `Pick`: it does not provide "
                "`Scalar`, which `Pick` promises",
                "!X {} Scalar\n",
            ),
            # A bound functor making a result item at another type than
            # the application promises (its own `Int`, inside).
            "functor_promise": (
                "`WasiFloats`, bound to `Fast`, provides `float` at another "
                "type than `Fast` promises here (D78)",
                "?X {} kernel float\n",
            ),
            # A value of a type `bind F;` made leaving its block, where the
            # type made differs from the one outside.
            "functor_escape": (
                "this block yields `Float@Fast`, the type `bind Fast;` made "
                "in it: a value of that type cannot leave the block (D78)",
                "?W {} leak Float\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertTrue(
                    out.decode("utf-8").startswith(report.format(entry)), out[:200]
                )

    def test_generations_that_specialize_alike_are_told_apart(self):
        """D72 and D78: two generations of a type that specialize to one
        type are still two types, since both compilers check a function
        as written, against its generic types: `x`, a `T` from before
        `bind T = Int;`, used as a `T` after it where `T` was `Int` all
        along; and a value of the `Float` `bind Fast;` made leaving its
        block where that `Float` is the f64 the outer one is too. The
        self-hosted compiler used to check types as specialized, and
        compiled both. Rejected in both compilers."""
        from .test_build import compile_wasm

        cases = {
            "generation_mix": (
                "`y` is declared `Int` but initialized with `T`",
                "?S {} mix\n",
            ),
            "functor_escape_same": (
                "this block yields `Float@Fast`, the type `bind Fast;` made "
                "in it: a value of that type cannot leave the block (D78)",
                "?W {} leak Float\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertTrue(
                    out.decode("utf-8").startswith(report.format(entry)), out[:200]
                )

    def test_a_union_slot_holds_one_valtype(self):
        """D75 meets D59: a union's slot has its first member's valtype,
        so `Leaf Int | Node { l: Ref, r: Ref }` would put a reference
        where an i32 goes. Both compilers refuse it, naming the member,
        rather than emit a module that does not validate."""
        from mossc.build import NotCompilable

        from .test_build import compile_wasm

        entry = "tests/wasi/gc_slots.moss"
        with self.assertRaises(NotCompilable) as caught:
            compile_wasm({}, entry=entry)
        self.assertIn("`Node` puts a reference where", str(caught.exception))
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(out.decode("utf-8"), f"?P {entry} demo Node\n")

    def test_records(self):
        """The rest of D59's value model: a record is its fields' runs,
        one after another, so records nest, sit inside unions of different
        widths, travel as parameters, returns and contextual vals, and
        destructure by name — renamed, wildcarded, or whole."""
        module = self.compile_with_moss("tests/wasi/records.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), RECORDS_OUT)

    def test_only_reachable_vals_are_passed(self):
        """Inside `assume Std`, every function needs every `Std` val, and
        each provider bound to one of its fn needs brings its own along.
        Passing them all ran `bar` here past Wasm's thousand-parameter
        limit; both back ends pass only the vals a function can reach."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/levels.moss"
        module = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), "1\n3\n")
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "1\n3\n")

    def test_rebinding_on_the_recursive_path_terminates(self):
        """A recursive function that rebinds one of its own fn needs on its
        own recursive path. Keyed by everything in scope, each
        specialization captured the provision it shadowed and demanded
        another, without end; keyed by what is reached, there are two.
        Its `Float` result also has to be learned, not assumed an i32."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/rebind_recursive.moss"
        module = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), "2\n1\n0\n11\n")
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "2\n1\n0\n11\n")

    def test_phantoms_and_receivers_are_checked(self):
        """D73's listed parameters must be types the declaration requires,
        and D74's attached method is at its receiver's instantiation: a
        `Box[T=Int]`'s `.get()` is an `Int` wherever `T` has been rebound,
        and a `Cell[Item=Int]` read where the `Char` codec is in force
        needs its codec at `Int`. Both used to read an `Int` as a `Char`."""
        from .test_build import compile_wasm

        cases = {
            "phantom_unlisted": (
                "`Cell` lists `Other`, which is not a type its assume nesting "
                "requires (D73)",
                "!V {} Other\n",
            ),
            "receiver_rebound": (
                "argument `c` of `putchar` is `Int`, expected `Char`",
                "?S {} demo\n",
            ),
            "phantom_misread": (
                "calling `Cell.first` needs `encode` at [Item=Int], but the one "
                "in the context here was provided at [Item=Char]",
                "?S {} demo\n",
            ),
            # D75: a reference's `.deref()` is at what it holds.
            "gc_misread": (
                "argument `c` of `putchar` is `Int`, expected `Char`",
                "?S {} demo\n",
            ),
            # D75: boxing is a context item, which only the platform's
            # `Heap` provides; code that has not applied it cannot box.
            "gc_unheaped": (
                "no method `.boxed` is available for `Int` in the context here",
                "?K {} demo boxed\n",
            ),
            # ...and applying `Heap` needs its argument signature here.
            "gc_needs_boxed": (
                "applying `Heap` needs `Boxed`, which is not available in "
                "the context here",
                "?Q {} demo Boxed\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertTrue(
                    out.decode("utf-8").startswith(report.format(entry)), out[:200]
                )

    def test_multiple_memories(self):
        """D79: a program names a second memory as a unit and binds
        `Memory` to it in a block, where the library's lists live in it.
        A list in each memory, summed; one memory's allocator reset,
        which reuses its own space and leaves the other's data alone; and
        text made in `Scratch` copied into the main memory to be printed,
        and back. Interpreted and in both compilers, each module has two
        memories, exports the first, and puts the second's index on the
        instructions specialized at it."""
        from .test_build import compile_wasm, run_wasm

        for entry, expected in (
            ("tests/wasi/memories.moss", "5050\n51225\n56275\n"),
            ("tests/wasi/memory_reset.moss", "51225\n55\n1024\n1\n5050\n"),
            ("tests/wasi/memory_copy.moss", "hi\nih\n"),
        ):
            with self.subTest(entry=entry):
                self.assertEqual(run({}, entry=entry), expected)
                for module in (
                    self.compile_with_moss(entry, prelude="lib/prelude.moss"),
                    compile_wasm({}, entry=entry),
                ):
                    text = wasm_text(module)
                    self.assertIn("(memory $0 ", text)
                    self.assertIn("(memory $1 ", text)
                    self.assertNotIn("(memory $2 ", text)
                    self.assertIn('(export "memory" (memory $0))', text)
                    self.assertIn("i32.load $1", text)
                    self.assertIn("i32.store $1", text)
                    self.assertEqual(run_wasm(module), expected)

    def test_one_memory_is_one_memory(self):
        """D79 changes nothing for a program that binds no `Memory`: the
        module has one memory, as before."""
        from .test_build import compile_wasm

        entry = "tests/wasi/lists.moss"
        for module in (
            self.compile_with_moss(entry, prelude="lib/prelude.moss"),
            compile_wasm({}, entry=entry),
        ):
            text = wasm_text(module)
            self.assertIn("(memory $0 ", text)
            self.assertNotIn("(memory $1 ", text)
            self.assertNotIn("i32.load $1", text)

    def test_a_handle_is_typed_by_its_memory(self):
        """D79: a list or a string lists the memory it is in, so one made
        in `Scratch` is not the main memory's, though both are an address:
        passing a `Scratch` list where `main`'s `IntList` is expected, and
        printing a `Scratch` string as it is (WASI reads only the main
        memory), are type errors in both compilers; and calling WASI where
        `Memory` is rebound is incoherent."""
        from .test_build import compile_wasm

        cases = {
            "memory_mismatch": (
                "argument `xs` of `sum` is `Packed[Item=Num[I32=I32], "
                "Memory=Scratch, I32=I32]`, expected `Packed[Item=Num[I32=I32], "
                "Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            "memory_print": (
                "argument `s` of `print` is `Str[Memory=Scratch, I32=I32]`, "
                "expected `Str[Memory=Memory, I32=I32]`",
                "?S {} main\n",
            ),
            # WASI preview 1 reads only the first memory, so its functions
            # are provided at the root's `Memory` alone: a WASI-calling
            # functor applied where `Memory` is rebound is incoherent,
            # where it used to write into `Scratch` and print `AA`.
            "memory_wasi": (
                "calling `wasi_putchar` needs `args_get` at [Memory=Scratch]",
                "!R {} args_get\n",
            ),
        }
        for name, (message, report) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertTrue(
                    out.decode("utf-8").startswith(report.format(entry)), out[:200]
                )

    def test_a_type_bind_starts_a_generation(self):
        """D72: what was typed before a bind of `Int` keeps the `Int` it
        had, in a function generic over it, while a value made after the
        bind adds with the new one's `+`."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/generations.moss"
        module = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), "-2\n40\n")
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "-2\n40\n")

    def test_dual_numbers_by_rebinding_float(self):
        """D72: `bind Float = Dual` over the `Float` in force, inside
        functions generic over `Float`, differentiates code written
        against it, and twice gives a second derivative. Nested
        differentiation is correct (1) because the outer variable has to
        be lifted into the inner generation; added as it is, it is a type
        error, not perturbation confusion (2). The bootstrap's front end
        accepts the one and rejects the other; its back end compiles each
        function once, at one scalar per abstract type, so it cannot run
        a `Float` two wide."""
        from .test_build import compile_wasm

        module = self.compile_with_moss("tests/wasi/dual.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), DUAL_OUT)
        program = collect.load("tests/wasi/dual.moss", prelude=PRELUDE, root=REPO)
        Lower(program).run()

        entry = "tests/wasi/perturbation.moss"
        with self.assertRaises(LowerError) as caught:
            compile_wasm({}, entry=entry)
        self.assertIn(
            "argument `rhs` of `.add` is `Dual[Base=Dual[Base=Float]]`, "
            "expected `Dual[Base=Float]`",
            str(caught.exception),
        )
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        self.assertTrue(out.decode("utf-8").startswith(f"?S {entry} nested\n"), out[:200])

    def test_temporaries_nested_anywhere(self):
        """A local comes from emission itself, so a construct that needs a
        temporary works however deeply it is nested: a `match` in a call's
        arguments, as a method's receiver, in a record field or an
        unannotated `let`, and a field of a record that is not in a local.
        A separate walk used to create locals ahead of emission and never
        looked inside a call's arguments."""
        module = self.compile_with_moss("tests/wasi/nested.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), NESTED_OUT)

    def test_a_type_that_contains_itself_is_reported(self):
        """No heap means no finite layout for a type that contains itself,
        so it is `e_shape` (`N`), naming the type, rather than a compiler
        that never stops laying it out."""
        out = self.compile_with_moss("tests/wasi/cyclic.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        report = out.decode("utf-8")
        self.assertTrue(report.startswith("!N "), report[:200])
        self.assertIn("Cell", report)

    def test_a_module_that_will_not_open_is_reported(self):
        """A path the host refuses -- absent here, but equally one that
        lies outside the directory it opened -- is `e_read` (`O`). The
        read itself cannot report it: `Path.read` has no `String` to
        return and traps, which is why `parse_module` asks
        `Path.readable` first. Before that, a mistyped import came out of
        the driver as a Wasm backtrace."""
        out = self.compile_with_moss("tests/wasi/absent.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        report = out.decode("utf-8")
        self.assertTrue(report.startswith("!O "), report[:200])
        self.assertIn("tests/wasi/nosuch.moss", report)

    def test_a_misfitting_type_is_reported(self):
        """The D17 check, wired into the back end: a value of one nominal
        type where another is declared, both one scalar wide, so nothing
        but `types::fits` would notice. The bootstrap rejects it in
        lowering; the self-hosted compiler reports `c_bad_type` (`?S`)
        and writes nothing."""
        from .test_build import compile_wasm

        with self.assertRaises(LowerError):
            compile_wasm({}, entry="tests/wasi/misfit.moss")
        out = self.compile_with_moss("tests/wasi/misfit.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        self.assertTrue(out.decode("utf-8").startswith("?S "), out[:200])

    UNCALLED = {
        # entry: (the bootstrap's message, the self-hosted report, prelude)
        "tests/wasi/uncalled.moss": (
            "argument `a` of `f` is `B[I32=I32]`, expected `A[I32=I32]`",
            "?S tests/wasi/uncalled.moss unused\n",
            "",
        ),
        "tests/wasi/uncalled_generic.moss": (
            "block yields `T`, expected `I32`",
            "?S tests/wasi/uncalled_generic.moss unused\n",
            "",
        ),
        # An anonymous function a reached bind makes, never called.
        "tests/wasi/uncalled_lambda.moss": (
            "block yields `Int`, expected `Char`",
            "?S tests/wasi/uncalled_lambda.moss step\n",
            "lib/prelude.moss",
        ),
        # A functor bound to an abstract one that nothing applies (D78).
        "tests/wasi/functor_unapplied.moss": (
            "functor `Tally`, bound to an abstract functor, may rely only "
            "on its own argument signature (D78): calling `count` needs "
            "`counter`",
            "!P tests/wasi/functor_unapplied.moss counter\n",
            "lib/prelude.moss",
        ),
    }

    def test_an_uncalled_function_is_checked_by_the_bootstrap(self):
        """D16: no post-monomorphization checks, so a function is checked
        whether or not anything calls it — a concrete one, a generic one,
        of which no specialization exists, an anonymous one, and the
        binds of a functor bound to an abstract one that nothing applies.
        The bootstrap lowers every function once, against its generic
        types."""
        from .test_build import compile_wasm

        for entry, (message, _, _) in self.UNCALLED.items():
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))

    def test_an_uncalled_function_is_checked_by_the_self_hosted_compiler(self):
        """The same programs. The self-hosted compiler types a body as it
        emits it, so after the passes that emit what the program reaches,
        a check pass walks everything they did not, generically, and
        reports what it finds, naming the frame."""
        for entry, (_, report, prelude) in self.UNCALLED.items():
            with self.subTest(entry=entry):
                out = self.compile_with_moss(entry, prelude=prelude)
                self.assertEqual(out.decode("utf-8", "replace"), report)

    MATCH_ARMS = {
        # entry: (the bootstrap's message, the self-hosted report)
        "tests/wasi/match_unreached.moss": (
            "match arm yields `B[I32=I32]`, expected `I32`",
            "?S tests/wasi/match_unreached.moss unused\n",
        ),
        "tests/wasi/match_generic.moss": (
            "match arm yields `B[I32=I32]`, expected `I32`",
            "?S tests/wasi/match_generic.moss pick\n",
        ),
        "tests/wasi/match_generic_union.moss": (
            "match arm yields `B[I32=I32]`, expected `I32`",
            "?S tests/wasi/match_generic_union.moss pick\n",
        ),
        "tests/wasi/match_tag_abstract.moss": (
            "`Box` is not a member of `T`",
            "?S tests/wasi/match_tag_abstract.moss pick Box\n",
        ),
    }

    def test_every_match_arm_is_checked_by_the_bootstrap(self):
        """D16 and D34: every arm of a `match` is checked against the
        scrutinee's type as written, whichever arms a specialization would
        run — on a scrutinee of abstract type `T`, whose one head is `T`
        itself, so that a binder or `_` covers it and anything after one
        is never run, in a function nothing calls, in one reached where
        `T` has one head and where it is a union; and a tag pattern on it
        names no member of `T`."""
        from .test_build import compile_wasm

        for entry, (message, _) in self.MATCH_ARMS.items():
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(message, str(caught.exception))

    def test_every_match_arm_is_checked_by_the_self_hosted_compiler(self):
        """The same programs. The self-hosted compiler takes a match's
        shape from the specialization — one head runs the first arm, and
        a union stops at the first arm that matches anything — and
        checks the arms it does not run all the same, emitting them
        nowhere."""
        for entry, (_, report) in self.MATCH_ARMS.items():
            with self.subTest(entry=entry):
                out = self.compile_with_moss(entry)
                self.assertEqual(out.decode("utf-8", "replace"), report)

    def test_an_uncalled_abstract_application_is_checked_by_the_bootstrap(self):
        """D16 and D78: `bind F;` in a function nothing calls is checked
        against every functor `F` is bound to anywhere — each must make
        each item of `F`'s result at the type the application promises
        (functor_promise.moss, with `kernel` unreached)."""
        from .test_build import compile_wasm

        entry = "tests/wasi/functor_promise_unreached.moss"
        with self.assertRaises(LowerError) as caught:
            compile_wasm({}, entry=entry)
        self.assertIn(
            "`WasiFloats`, bound to `Fast`, provides `float` at another "
            "type than `Fast` promises here (D78)",
            str(caught.exception),
        )

    @unittest.expectedFailure
    def test_an_uncalled_abstract_application_is_checked_by_the_self_hosted_compiler(self):
        """The same program. The check pass runs the binds of each functor
        `F` can be bound to at the application, as a reached application
        runs the one it is bound to, and compares what they make with
        what the application promises."""
        entry = "tests/wasi/functor_promise_unreached.moss"
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(out.decode("utf-8", "replace"), f"?X {entry} kernel float\n")

    def test_a_callee_requires_what_it_assumes_and_does_not_use(self):
        """D20 and D16: a call needs everything the callee's declaration
        assumes, read off the declaration, not off what its body happens
        to use: `quiet` assumes `X.gimme` and never calls it, and calling
        it with no `X.gimme` in context is rejected by both compilers. The
        self-hosted compiler used to find a need missing only when
        emitting a use of it."""
        from .test_build import compile_wasm

        entry = "tests/wasi/unused_need.moss"
        with self.assertRaises(LowerError) as caught:
            compile_wasm({}, entry=entry)
        self.assertIn(
            "calling `quiet` needs `X[Char=Char].gimme`, which is not "
            "available in the context here",
            str(caught.exception),
        )
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(out.decode("utf-8", "replace"), f"!P {entry} gimme\n")

    def test_a_provision_answers_only_its_instantiation(self):
        """Coherence: a callee's val, fn or method requirement is met only
        by a provision made at the instantiation the callee means — a
        method item carries its application, and a val or fn its ambient
        one (CoreMoss.v deviation 8). Each of these was accepted by the
        bootstrap, which matched the requirement by its key alone, and
        read a `Char` as an `Int` or a `Point`. Both compilers now reject
        each at the call, in the front end: the bootstrap with the message
        below, the self-hosted compiler with `e_incoherent` (`R`), named by
        the item. (It used to report `c_bad_type`, `?S`, from the back
        end's `types::fits` on a specialization.)"""
        from .test_build import compile_wasm

        cases = {
            "incoherent_method": (
                "use_it", "`X[Char=Char].gimme` at [Foo=Int]", "[Foo=Char]", "gimme",
            ),
            "incoherent_bind_application": (
                "use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m",
            ),
            "incoherent_rebound": ("use_it", "`W[Char=Char].m` at [T=Int]", "[T=Char]", "m"),
            "incoherent_core_shape": (
                "use_it", "`W[Char=Char].m` at [T=Point[Char=Char]]", "[T=Char]", "m",
            ),
            "incoherent_val_fn": ("get_seed", "`seed` at [T=Int]", "[T=Char]", "seed"),
            "incoherent_fn": ("get_made", "`make` at [T=Int]", "[T=Char]", "make"),
        }
        for name, (callee, needs, made, item) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    f"calling `{callee}` needs {needs}, but the one in the "
                    f"context here was provided at {made}",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                report = out.decode("utf-8")
                self.assertTrue(report.startswith(f"!R {entry} {item}\n"), report[:200])

    def test_a_provision_from_before_a_rebind_is_refused(self):
        """A val assumed at the ambient `T` does not serve a call made
        after `bind T = Int` in the same body. Both compilers reject it.
        The self-hosted compiler used to accept it and read a `Char` as an
        `Int`: the rebinding is the same width, so its back end's
        `types::fits` on the specialization saw nothing wrong."""
        from .test_build import compile_wasm

        entry = "tests/wasi/incoherent_shadowed.moss"
        with self.assertRaises(LowerError) as caught:
            compile_wasm({}, entry=entry)
        self.assertIn(
            "needs `seed` at [T=Int], but the one in the context here was "
            "provided at [T as assumed, before a later `bind T`]",
            str(caught.exception),
        )
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(out.decode("utf-8"), f"!R {entry} seed\n")

    def test_an_assumed_provision_is_at_the_whole_ambient_instantiation(self):
        """`assume seed, T` — the type after the item — still puts `seed`
        at the function's `T`, so two functions of that block call each
        other. The bootstrap's first coherence check read `T` as not in
        force for `seed`, and refused the call."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/coherent_assume_order.moss"
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "c\n")
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(out), "c\n")

    def test_reproviding_after_a_rebind_is_coherent(self):
        """The control: provide again at the new `T`, and both compilers
        accept and run it."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/coherent_rebound.moss"
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "ab\n")
        self.assertEqual(self.wasmtime_run(self.compile_with_moss(entry, prelude="lib/prelude.moss")), "ab\n")

    def test_an_assume_needs_the_types_its_item_mentions(self):
        """D20: assuming `W.m`, or `seed` and `make`, whose signatures
        mention `T`, is legal only where `T` is assumed or bound. The
        bootstrap says the type is not available; the self-hosted compiler
        reports `e_absent` (`P`), naming the type."""
        from .test_build import compile_wasm

        for name in ("assume_without_type", "assume_without_type_val_fn"):
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    "type `T` is not available in the context here",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                report = out.decode("utf-8")
                self.assertTrue(report.startswith(f"!P {entry} T\n"), report[:200])

    def test_relevance_is_a_least_fixpoint(self):
        """D71: `type A { b: B, t: T }; type B { a: A };` — both are
        parameterized by `T`, whichever is computed first, so `get_b(): B`
        mentions `T` and assuming it without `T` is D20's error. Each
        compiler once answered by whichever declaration it reached first,
        and accepted the A-first program."""
        from .test_build import compile_wasm

        for order in ("a_first", "b_first"):
            entry = f"tests/wasi/relevance_{order}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    "type `T` (required by `B`) is not available in the context here",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                report = out.decode("utf-8")
                self.assertTrue(report.startswith(f"!P {entry} T\n"), report[:200])

    def test_applying_an_irrelevant_parameter(self):
        """D71: brackets bind only a nominal's relevant parameters. A unit
        has none (`Stop[Int=Char]`), and `W[T=Char]` binds a symbol `W`'s
        payload does not mention. The bootstrap names the application;
        the self-hosted compiler reports `e_irrelevant` (`Q`), named by the
        symbol bound. The bootstrap used to accept both, ignoring the
        binding, and the self-hosted compiler the second."""
        from .test_build import compile_wasm

        cases = {
            "irrelevant_unit": ("Stop", "Int", "none"),
            "irrelevant_tag": ("W", "T", "`Char`"),
        }
        for name, (nominal, bound, params) in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    f"application of `{nominal}` binds `{bound}`, which is not one of "
                    f"its parameters (the type symbols its payload mentions: {params}; D71)",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertEqual(out.decode("utf-8"), f"!Q {entry} {bound}\n")

    def test_applying_a_relevant_parameter_found_by_fixpoint(self):
        """`B[T=Int]` is legal: `T` reaches `B` through `A`, and is
        relevant to `B` even when `A` is computed first. Both compilers
        accept it (and the self-hosted compiler now reads the brackets on
        a type bind's right-hand side rather than dropping them)."""
        from .test_build import compile_wasm, run_wasm

        entry = "tests/wasi/relevance_applied.moss"
        self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), "y\n")
        out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(out), "y\n")

    def test_a_merge_makes_two_types_one(self):
        """D43, the designer's example: a context with `A.gimme[Foo=B]` and
        `A.gimme[Foo=C]` (and a val at both) merges the two bindings, so
        `B` and `C` are one type inside it and may be used
        interchangeably. The self-hosted compiler used to keep the first
        of the two assumptions, and found `C` where `B` was expected.
        Merging does not depend on the order of the assume list
        (`merge_order`, which the bootstrap used to reject), and is done
        in a functor's argument signature too (`merge_functor`)."""
        from .test_build import compile_wasm, run_wasm

        for name, expected in (
            ("merge", "qbq\n"),
            ("merge_order", "k\n"),
            ("merge_functor", "tt\n"),
        ):
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), expected)
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertEqual(self.wasmtime_run(out), expected)

    def test_an_inconsistent_merge(self):
        """D43: a merge that would identify two distinct concrete types —
        `A.gimme[Foo=U1]` with `A.gimme[Foo=U2]` — is an error where the
        context is assumed, though nothing calls the function assuming
        it, or in a functor's argument signature. The self-hosted compiler
        reports `e_merge` (`Y`), named by the item whose bindings would not
        merge; it used to keep the first and accept the program."""
        from .test_build import compile_wasm

        for name in ("merge_inconsistent", "merge_functor_inconsistent"):
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    "cannot merge `U1` with `U2` for `.gimme` (D43)",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertEqual(out.decode("utf-8"), f"!Y {entry} gimme\n")

    def test_a_caller_binds_merged_types_alike(self):
        """D43: a merged context requires the merged types to be one, so a
        function assuming one may be called only where they are: not with
        `B` and `C` bound to `Char` and `Int` (`merge_caller`, and
        `merge_methods_caller`, where the merge also makes `B.m` and `C.m`
        one key), and not from a function generic over `B` and `C` apart,
        though every specialization of it binds both to `Char`
        (`merge_caller_generic`, D80). The self-hosted compiler reports
        `e_merge` (`Y`), named by the merged type."""
        from .test_build import compile_wasm

        cases = {
            "merge_caller": "`Char` and `Int`",
            "merge_methods_caller": "`Char` and `Int`",
            "merge_caller_generic": "`B` and `C`",
        }
        for name, here in cases.items():
            entry = f"tests/wasi/{name}.moss"
            with self.subTest(entry=entry):
                with self.assertRaises(LowerError) as caught:
                    compile_wasm({}, entry=entry)
                self.assertIn(
                    "needs `B` and `C` to be one type, which its context merged "
                    f"(D43), but here they are {here}",
                    str(caught.exception),
                )
                out = self.compile_with_moss(entry, prelude="lib/prelude.moss")
                self.assertNotEqual(out[:4], b"\0asm")
                self.assertEqual(out.decode("utf-8"), f"!Y {entry} B\n")

    def test_relevance_does_not_depend_on_order(self):
        """The bootstrap's `type_requirements` on the same declarations,
        asked in both orders."""
        for order in ("AB", "BA"):
            with self.subTest(order=order):
                program = collect.load(
                    "tests/wasi/relevance_a_first.moss", prelude=PRELUDE, root=REPO
                )
                lower = Lower(program)
                got = {
                    name: [t.name for t in lower.type_requirements(program.entry.names[name])]
                    for name in order
                }
                self.assertEqual(got, {"A": ["T"], "B": ["T"]})

    def test_applying_a_type(self):
        """D18: a nominal type's identity is its declaration and its static
        bindings, so `Box[Elem=I32]` substitutes into the payload. Both
        compilers run tests/wasi/applied.moss."""
        from .test_build import compile_wasm, run_wasm

        self.assertEqual(run_wasm(compile_wasm({}, entry="tests/wasi/applied.moss")), "A\n")
        module = self.compile_with_moss("tests/wasi/applied.moss")
        self.assertEqual(self.wasmtime_run(module), "A\n")

    def test_a_record_at_a_wider_instantiation(self):
        """A record whose fields are typed by an abstract `T` is laid out
        per instantiation, so a method on it compiles at `T = Char` and at
        `T = Point` alike."""
        module = self.compile_with_moss("tests/wasi/widened.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), "ba\nefd\n")

    def test_provisions_are_keyed_by_instantiation(self):
        """D62: a provision serves only the instantiation it was made at,
        so two of one method at two instantiations coexist, each compiled
        for its own type — and a receiver at a third finds none."""
        module = self.compile_with_moss("tests/wasi/provisions.moss", prelude="lib/prelude.moss")
        self.assertEqual(self.wasmtime_run(module), "ac\n")
        out = self.compile_with_moss("tests/wasi/mismatch.moss", prelude="lib/prelude.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        report = out.decode("utf-8")
        self.assertTrue(report.startswith("?K "), report[:200])
        self.assertIn("peek", report)


NEEDS_DRIVER = (
    "import \"./src/ast.moss\" as ast;\n"
    "import \"./src/boot.moss\" use Boot, Compiler;\n"
    "import \"./src/bytes.moss\" use *;\n"
    "import \"./src/intern.moss\" as intern;\n"
    "import \"./src/lower.moss\" as lower;\n"
    "import \"./src/prog.moss\" as prog;\n"
    "import \"./src/types.moss\" as types;\n"
    "\n"
    "assume Std {\n"
    "  fn main() {\n"
    "    bind Boot;\n"
    "    go();\n"
    "  }\n"
    "\n"
    "  assume Compiler {\n"
    "    fn tab() { putchar(char::tab); }\n"
    "\n"
    "    fn go() {\n"
    "      let entry = prog::load_program(arg_at(one), arg_at(one.add(one)));\n"
    "      var e = zero;\n"
    "      while e.lt(prog::error_count()) {\n"
    "        putchar(char::exclam);\n"
    "        put_int(prog::err_code.get(e));\n"
    "        putchar(char::space);\n"
    "        print(prog::module_path(prog::err_mod.get(e)));\n"
    "        if ast::is_some(prog::err_name.get(e)) {\n"
    "          putchar(char::space);\n"
    "          intern::put(prog::err_name.get(e));\n"
    "        }\n"
    "        putchar(char::newline);\n"
    "        e = e.add(one);\n"
    "      }\n"
    "      lower::lower_all();\n"
    "      var f = zero;\n"
    "      while f.lt(prog::sym_count()) {\n"
    "        if lower::is_lowered(f) {\n"
    "          print(prog::module_path(prog::sym_mod.get(f)));\n"
    "          tab();\n"
    "          if ast::is_some(prog::sym_recv.get(f)) {\n"
    "            intern::put(prog::sym_name.get(prog::sym_recv.get(f)));\n"
    "            putchar(char::dot);\n"
    "          }\n"
    "          intern::put(prog::sym_name.get(f));\n"
    "          var i = zero;\n"
    "          while i.lt(lower::needs_count(f)) {\n"
    "            tab();\n"
    "            let r = lower::need_recv_at(f, i);\n"
    "            if ast::is_some(r) {\n"
    "              intern::put(prog::sym_name.get(types::head_of(r)));\n"
    "              putchar(char::dot);\n"
    "            }\n"
    "            intern::put(prog::sym_name.get(lower::need_sym_at(f, i)));\n"
    "            i = i.add(one);\n"
    "          }\n"
    "          putchar(char::newline);\n"
    "        }\n"
    "        f = f.add(one);\n"
    "      }\n"
    "    }\n"
    "  }\n"
    "}\n"
)


class TestSelfHostedNeeds(unittest.TestCase):
    """src/lower.moss against the requirement lists the bootstrap's
    lowering computes. A declaration nested in `assume` blocks is
    implicitly parameterized by everything they name (D26), so this is
    where contexts stop being syntax: the list, in order, is a
    function's calling convention.

    tests/wasi/full.moss is the demanding case — it reaches all of `Std`
    provided in Moss over the primitive context, so the eight functors,
    the attached methods behind every operator, and contexts nested
    several deep all have to flatten the same way."""

    def bootstrap_rows(self, entry):
        import os

        from mossc import ast, collect
        from mossc.lower import Lower

        program = collect.load(entry, prelude=PRELUDE, root=REPO)
        lower = Lower(program)
        lower.run()

        from mossc.types import head

        def key(k):
            if isinstance(k, tuple):
                # A key's receiver is a type (D62); the rows name its head.
                return f"{head(k[0]).name}.{k[1].decl.name.name}"
            if isinstance(k.decl, ast.Fndef):
                return k.decl.name.name
            return k.name

        # Deduplicated by symbol identity, not by table: a defined
        # function that other modules `use *` appears in each of their
        # name tables, and it is still one function.
        rows, seen = [], set()
        for module in program.modules.values():
            for table in (module.names, module.attached):
                for sym in table.values():
                    if id(sym) not in lower.fns or id(sym) in seen:
                        continue
                    seen.add(id(sym))
                    name = sym.decl.name.name
                    if sym.receiver is not None:
                        name = f"{sym.receiver.name}.{name}"
                    rows.append(
                        [os.path.relpath(sym.module.path, REPO), name]
                        + [key(k) for k in lower.fns[id(sym)].needs]
                    )
        return sorted(rows)

    def moss_rows(self, entry):
        out = run_driver(NEEDS_DRIVER, ["lib/prelude.moss", entry]).decode("utf-8")
        rows = []
        for line in out.strip().split("\n"):
            self.assertFalse(line.startswith("!"), f"lowering error: {line}")
            rows.append(line.split("\t"))
        return sorted(rows)

    def test_needs_match_the_bootstrap(self):
        for entry in ("tests/wasi/full.moss", "src/main.moss"):
            with self.subTest(entry=entry):
                mine, theirs = self.moss_rows(entry), self.bootstrap_rows(entry)
                self.assertEqual(len(mine), len(theirs))
                self.assertEqual(mine, theirs)


# Every driver `run_driver` runs, so tests/parallel.py can start compiling
# them all before any test asks. One missing here costs only that wait.
DRIVERS = (PARSE_DRIVER, SCOPE_DRIVER, COMPILER_DRIVER, NEEDS_DRIVER)
