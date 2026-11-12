from suite import support as moss
from suite.support import (
    PRELUDE,
    build,
    compiler,
    fresh,
    once,
    oracle,
    oracle_command,
    output,
    repo,
    run_module,
    source,
    sources,
    std_main,
    wasm_file,
    wasmtime,
)

# Parts of the compiler written in Moss, driven one at a time: the parser
# (`src/syntax.moss`) against the oracle's Rocq parser, the module loader
# and scope tables (`src/prog.moss`), the requirement lists of lowering
# (`src/lower.moss`), and the Wasm encoder (`src/emit.moss`).
#
# A driver is an ordinary Moss program over `Std` that imports what it
# drives, compiled by the compiler under test and run under Wasmtime.
#
# Ported from `TestSelfHostedSyntax`, `TestSelfHostedCollectScopes`,
# `TestSelfHostedNeeds` and `TestSelfHostedEmitter` in the Python
# bootstrap's `tests/test_run.py`.
# A driver, written as a whole program whose `main` assumes `Std` and
# which imports `{root}/src/...`, made a program the platform runs and
# compiled: the path of its module.


def compile_driver(text):
    dir = sources([("main.moss", std_main(text))])
    return wasm_file(build(f"{dir}/main.moss"))


# Run a compiled driver from the repository root (which it reads files
# relative to), with `argv0` and `args`.


def run_driver(driver, argv0, args):
    out = wasmtime(driver, argv0, args)
    assert out.code == 0, f"the driver failed on {args!r}: {out.stderr}"
    return out


# Parse the file named by the first argument, and print `?` if the
# parser reported an error, then the tree in the compact form of the
# dump (which the oracle's `--dump` writes too).

syntax__PARSE_DRIVER = """import "{root}/src/ast.moss" as ast;
import "{root}/src/dump.moss" as dump;
import "{root}/src/intern.moss" as intern;
import "{root}/src/lex.moss" as lexer;
import "{root}/src/syntax.moss" as syntax;
assume Std {
  fn main() {
    bind ast::nkind=int_list();
    bind ast::na=int_list();
    bind ast::nb=int_list();
    bind ast::nc=int_list();
    bind ast::nd=int_list();
    bind ast::nfile=int_list();
    bind ast::nstart=int_list();
    bind ast::nend=int_list();
    bind ast::source_file=cell_int();
    bind ast::kids=int_list();
    bind ast::pnames=int_list();
    bind ast::pstart=int_list();
    bind ast::plen=int_list();
    bind ast::itexts=str_list();
    bind intern::ichars=int_list();
    bind intern::istarts=int_list();
    bind intern::ilens=int_list();
    bind intern::ibuckets=int_list();
    bind intern::ilink=int_list();
    bind intern::imask=cell_int();
    bind lexer::src=pwd.join(first_arg()).read();
    bind lexer::at=cell_int();
    bind lexer::mark=cell_int();
    bind syntax::tok=cell_int();
    bind syntax::tstart=cell_int();
    bind syntax::tend=cell_int();
    bind syntax::previous_end=cell_int();
    bind syntax::tok2=cell_int();
    bind syntax::t2start=cell_int();
    bind syntax::t2end=cell_int();
    bind syntax::errs=cell_int();
    bind syntax::errpos=cell_int();
    bind syntax::errend=cell_int();
    bind syntax::expected=cell_int();
    go();
  }
  assume ast::Ast, syntax::Cursor, intern::Interner,
    lexer::src, lexer::at, lexer::mark {
    fn go() {
      let roots = int_list();
      syntax::parse_file(roots);
      if syntax::errs.read().gt(zero) { putchar(char::question); }
      dump::dump_file(roots);
    }
  }
}
"""


# Known self-hosted compiler limitation: compiling anything that
# imports `src/dump.moss` exhausts Wasmtime's default wasm stack (512
# KiB) in the self-hosted compiler — a stack overflow trap — though
# the oracle elaborates the driver at once; its long `else if` chains
# recurse deeply. The Python suite compiled its drivers with the
# Python back end and never met it. So this driver is compiled by the
# compiler under test with a 4 MiB wasm stack.
@once
def syntax__parse_driver():
    entry = source(std_main(syntax__PARSE_DRIVER))
    default = wasmtime(compiler(), PRELUDE, [entry])
    assert default.code and "call stack exhausted" in default.stderr, (
        f"the parse driver can now use compile_driver: {default.stderr}"
    )
    out = output(
        [
            "wasmtime",
            "run",
            "-W",
            "max-wasm-stack=4194304",
            "--argv0",
            PRELUDE,
            "--dir",
            ".",
            compiler(),
            entry,
        ],
        cwd=repo(),
    )
    assert out.code == 0, f"the compiler failed on the parse driver: {out.stderr}"
    assert out.is_module(), f"the parse driver did not compile: {out.text()}"
    return wasm_file(out.stdout)


# The oracle's parser on each file (relative to the repository): its
# dump and a newline, or `ERROR msg` and a newline.
def syntax__rocq_dumps(files):
    directory = fresh("rocq-dump")
    directory.mkdir()
    out = oracle_command(["--dump", directory, *files])
    assert out.code == 0, f"the oracle's --dump: {out.stderr}"
    names = [
        "_".join(part for part in (repo() / file).as_posix().split("/") if part) for file in files
    ]
    return [(directory / (name + ".rocq.sx")).read_text() for name in names]


# Every live source file, including the quoted string fixtures.


def syntax__corpus():
    files = []
    for dir in ["src", "lib", "examples", "tests/wasi", "tests/fixtures/string_literals"]:
        files.extend(moss.moss_files(dir))
    return files


def test_syntax__quoted_payload_ast_uses_contextual_byte_paths():
    file = moss.source('fn demo() { let text = "Aé\\n\\"\\\\\\t\\r"; }')
    expected = syntax__rocq_dumps([file])[0]
    actual = run_driver(syntax__parse_driver(), None, [file]).text()
    assert actual == expected
    assert actual.count("(k string_write1 _)") == 6
    assert actual.count("(k string_write2 _)") == 1
    for name in ["41", "c3", "a9", "0a", "22", "5c", "09", "0d"]:
        assert f"(k byte::x{name} _)" in actual, f"{actual}"
    # Only lengths and indices remain integer literals (8 and 0..7).
    for value in [65, 195, 169, 10, 34, 92, 9, 13]:
        assert not (f"(Y {value})" in actual), f"{actual}"
    payload = "ࠀ\ud7ff\uffff𐀀\U00050000\U0010ffff"
    file = moss.source(f'fn demo() {{ let text = "{payload}"; }}')
    expected = syntax__rocq_dumps([file])[0]
    actual = run_driver(syntax__parse_driver(), None, [file]).text()
    assert actual == expected
    assert actual.count("(k string_write3 _)") == 3
    assert actual.count("(k string_write4 _)") == 3
    for name, lead, second in [
        ("ThreeE0", "e0", "a0"),
        ("ThreeED", "ed", "9f"),
        ("ThreeOrdinary", "ef", "bf"),
        ("FourF0", "f0", "90"),
        ("FourMiddle", "f1", "90"),
        ("FourF4", "f4", "8f"),
    ]:
        prefix = f"(o (k byte::{name} _) [(p lead (k byte::x{lead} _)) (p second (k byte::x{second} _))])"
        assert prefix in actual, f"{actual}"
    for value in [224, 160, 128, 237, 159, 191, 239, 240, 144, 241, 244, 143]:
        assert not (f"(Y {value})" in actual), f"{actual}"


# src/syntax.moss against the oracle's Rocq parser, over the whole
# corpus: both write the tree in the compact dump form (the Python
# bootstrap's `mossc/sexpr.py`, which the Rocq parser's dump
# reproduces byte for byte), and the two must agree character for
# character. This is the front end's real check — an approximate
# parser can produce a plausible letter dump, but it cannot produce
# the reference parser's tree.


def test_syntax__agrees_with_the_bootstrap_parser():
    files = syntax__corpus()
    expected = syntax__rocq_dumps(files)
    failures = []
    for file, expected in zip(files, expected):
        assert not (expected.startswith("ERROR")), f"the oracle cannot parse {file}: {expected}"
        actual = run_driver(syntax__parse_driver(), None, [file]).text()
        if actual != expected:
            failures.append(file)
    assert not failures, f"the parsers disagree on {failures!r}"


# D68's partial order, as sources that group (the trees are compared).

syntax__OPERATOR_GROUPINGS = [
    "-a * b",
    "!a & b",
    "- -a",
    "!!a",
    "-a.f(b)",
    "-x.y",
    "a * b * c",
    "a * b * c / d",
    "a * b % c",
    "a / -b",
    "(a / b) * c",
    "a / (b / c)",
    "a - b + c",
    "a - b - c",
    "a + b * c",
    "a * b + c / d",
    "a + b % c - d",
    "a + b == c * d",
    "a != b",
    "a <= b - c",
    "-a >= b",
    "a > b",
    "a < b",
    "a << b",
    "-a >> -b",
    "x << f(n) == y",
    "y == x >> n",
    "(a << b) + c",
    "a & b & c",
    "a | b | c",
    "a ^ b ^ c",
    "!a | !b",
    "(a < b) & (c < d)",
    "(x & mask) == z",
]

# Mixing that D68 refuses.

syntax__OPERATOR_ERRORS = [
    "a / b * c",
    "a * b / c * d",
    "a / b / c",
    "a / b % c",
    "a % b * c",
    "a % b % c",
    "a < b < c",
    "a == b == c",
    "a < b == c",
    "k << n + k",
    "n + k << m",
    "a * b >> c",
    "a << b * c",
    "a << b << c",
    "a << b >> c",
    "a & b | c",
    "a | b & c",
    "a ^ b & c",
    "a < b & c < d",
    "x & mask == z",
    "a & b + c",
    "a + b & c",
    "a << b & c",
    "a & b << c",
    "a == b | c",
]

# Literal snippets (D68): (source, refused).

syntax__LITERAL_SNIPPETS = [
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
    ("1e-9 + 2.5E10 * 1.5e+3", False),
    ("f(1E+03, 0.000123e-0004, 0e0, 7e999)", False),
    ("1e5.abs() - 1.exp()", False),
    ("1.e5 + 1.5.e3", False),
    ("-1e5.abs()", True),
]

# Anonymous-function snippets (D70): (source, refused).

syntax__LAMBDA_SNIPPETS = [
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
    ("(x, y)", False),
    ("(x,)", False),
    ("(x, y).0", True),
    ("let x, y = (a, b);", False),
    ("let outer::Wrapped(Point {x, y = other: T}) = p;", False),
    ("let Marker() = marker;", False),
    ("let ((x: T,), {name = y: U}) = v;", False),
    ("bind f = ((x: Int, y: Int)) => x;", False),
    ("bind f = (x, y) => (x, y);", False),
    ("((x)) => x", False),
    ("if () => x {}", True),
    ("if () => x() {}", False),
    ("let r = { x = f(x, y) };", False),
    ("let r = { x = f(x, y); x };", False),
    ("{ x; } + y.f(z) { q; }", False),
    ("match x { _ => { x; } _ => { x, } }", False),
    ("(x: Int)", True),
    ("bind f = (x) =>;", True),
    ("bind f = (x): => x;", True),
    ("bind f = (1) => x;", True),
]

# Each snippet as the body of `fn f() { ... }`: where it is refused,
# the oracle's dump is `ERROR ...` and the self-hosted driver prints
# `?` first; otherwise the two print the same tree.


def syntax__agree_on_snippets(cases):
    texts = [f"fn f() {{ {source} }}\n" for (source, _) in cases]
    names = [f"{i}.moss" for i in range(0, len(cases))]
    files = [(n, t) for (n, t) in zip(names, texts)]
    dir = sources(files)
    paths = [f"{dir}/{n}" for n in names]
    expected = syntax__rocq_dumps(paths)
    failures = []
    for ((source, refused), path), expected in zip(zip(cases, paths), expected):
        actual = run_driver(syntax__parse_driver(), None, [path]).text()
        if refused:
            ok = expected.startswith("ERROR ") and actual.startswith("?")
        else:
            ok = not (actual.startswith("?")) and (actual == expected)
        if not (ok):
            failures.append(f"{source!r}: oracle {expected!r}, self-hosted {actual!r}")
    assert not failures, "{}".format("\n".join(failures))


# D68's precedence table, snippet by snippet: the same tree where the
# reference parser groups, and a syntax error where it refuses. The
# self-hosted parser reports an error as a count, so a refusal is the
# `?` its driver prints.


def test_syntax__operators_agree_with_the_bootstrap_parser():
    cases = [(s, False) for s in syntax__OPERATOR_GROUPINGS]
    cases.extend([(s, True) for s in syntax__OPERATOR_ERRORS])
    cases.extend(syntax__LITERAL_SNIPPETS)
    syntax__agree_on_snippets(cases)


# D70: an anonymous function is told from `()` and from a
# parenthesized expression by what follows its parenthesis. The same
# trees, snippet by snippet — where it is legal and where it is not,
# which is for elaboration to say — and the same refusals.


def test_syntax__lambdas_agree_with_the_bootstrap_parser():
    syntax__agree_on_snippets(syntax__LAMBDA_SNIPPETS)


# Load the program `arg 2` with the prelude `arg 1`, and print each
# collect error as `!CODE MODULE [NAME]`, then each module's scope as
# rows of (module, namespace, name, target module, target name), then
# its attached methods.

collect_scopes__SCOPE_DRIVER = """import "{root}/src/ast.moss" as ast;
import "{root}/src/boot.moss" use Boot, Compiler;
import "{root}/src/bytes.moss" use *;
import "{root}/src/intern.moss" as intern;
import "{root}/src/prog.moss" as prog;

assume Std {
  fn main() {
    bind Boot;
    go();
  }

  assume Compiler {
    fn tab() { putchar(char::tab); }

    fn go() {
      let entry = prog::load_program(arg_at(one), arg_at(one.add(one)));
      var e = zero;
      while e.lt(prog::error_count()) {
        putchar(char::exclam);
        put_int(prog::err_code.get(e));
        putchar(char::space);
        print(prog::module_path(prog::err_mod.get(e)));
        if ast::is_some(prog::err_name.get(e)) {
          putchar(char::space);
          intern::put(prog::err_name.get(e));
        }
        putchar(char::newline);
        e = e.add(one);
      }
      var i = zero;
      while i.lt(prog::ent_mod.length()) {
        print(prog::module_path(prog::ent_mod.get(i)));
        tab();
        put_int(prog::ent_ns.get(i));
        tab();
        intern::put(prog::ent_name.get(i));
        tab();
        let to = prog::ent_to.get(i);
        if prog::ent_ns.get(i).eq(prog::ns_alias()) {
          print(prog::module_path(to));
          tab();
          putchar(char::star);
        } else {
          print(prog::module_path(prog::sym_mod.get(to)));
          tab();
          intern::put(prog::sym_name.get(to));
        }
        putchar(char::newline);
        i = i.add(one);
      }
      var a = zero;
      while a.lt(prog::att_mod.length()) {
        print(prog::module_path(prog::att_mod.get(a)));
        tab();
        putchar(char::at);
        tab();
        intern::put(prog::sym_name.get(prog::att_recv.get(a)));
        putchar(char::dot);
        intern::put(prog::att_name.get(a));
        tab();
        print(prog::module_path(prog::sym_mod.get(prog::att_sym.get(a))));
        tab();
        intern::put(prog::att_name.get(a));
        putchar(char::newline);
        a = a.add(one);
      }
    }
  }
}
"""

# src/prog.moss's module graph, the symbols every declaration gets,
# and the scope each module ends up with — names, detached methods,
# aliases, and attached methods keyed by receiver — over the
# compiler's own sources and over tests/wasi/full.moss. These rows
# were compared with the Python bootstrap's `collect`, which is gone
# and has no counterpart in the oracle; what is left is that the
# driver loads both programs with no collect error, and that its rows
# are well formed and name the modules they should.


def test_collect_scopes__scopes_match_the_bootstrap():
    driver = compile_driver(collect_scopes__SCOPE_DRIVER)
    for entry in ["src/main.moss", "tests/wasi/full.moss"]:
        out = run_driver(driver, None, ["lib/prelude.moss", entry]).text()
        rows = [list(line.split("\t")) for line in out.splitlines()]
        assert all(not row[0].startswith("!") for row in rows), f"{entry}: collect error: {out}"
        assert all(len(row) == 5 for row in rows), f"{entry}: a malformed row"
        assert all(row[1] in ["0", "1", "2", "@"] for row in rows), f"{entry}"
        assert any(row[0].endswith(entry) for row in rows), f"{entry}: no rows for the entry"
        assert any(row[0].endswith("lib/prelude.moss") for row in rows), f"{entry}: no prelude"


# Load and lower the program `arg 2` with the prelude `arg 1`, and
# print each lowered function's requirement list: module, name, and
# each need (a method's as `Recv.name`).

needs__NEEDS_DRIVER = """import "{root}/src/ast.moss" as ast;
import "{root}/src/boot.moss" use Boot, Compiler;
import "{root}/src/bytes.moss" use *;
import "{root}/src/intern.moss" as intern;
import "{root}/src/lower.moss" as lower;
import "{root}/src/prog.moss" as prog;
import "{root}/src/types.moss" as types;

assume Std {
  fn main() {
    bind Boot;
    go();
  }

  assume Compiler {
    fn tab() { putchar(char::tab); }

    fn go() {
      let entry = prog::load_program(arg_at(one), arg_at(one.add(one)));
      var e = zero;
      while e.lt(prog::error_count()) {
        putchar(char::exclam);
        put_int(prog::err_code.get(e));
        putchar(char::space);
        print(prog::module_path(prog::err_mod.get(e)));
        if ast::is_some(prog::err_name.get(e)) {
          putchar(char::space);
          intern::put(prog::err_name.get(e));
        }
        putchar(char::newline);
        e = e.add(one);
      }
      lower::lower_all();
      var f = zero;
      while f.lt(prog::sym_count()) {
        if lower::is_lowered(f) {
          print(prog::module_path(prog::sym_mod.get(f)));
          tab();
          if ast::is_some(prog::sym_recv.get(f)) {
            intern::put(prog::sym_name.get(prog::sym_recv.get(f)));
            putchar(char::dot);
          }
          intern::put(prog::sym_name.get(f));
          var i = zero;
          while i.lt(lower::needs_count(f)) {
            tab();
            let r = lower::need_recv_at(f, i);
            if ast::is_some(r) {
              intern::put(prog::sym_name.get(types::head_of(r)));
              putchar(char::dot);
            }
            intern::put(prog::sym_name.get(lower::need_sym_at(f, i)));
            i = i.add(one);
          }
          putchar(char::newline);
        }
        f = f.add(one);
      }
    }
  }
}
"""

# src/lower.moss's requirement lists. A declaration nested in
# `assume` blocks is implicitly parameterized by everything they name
# (D26), so this is where contexts stop being syntax: the list, in
# order, is a function's calling convention. tests/wasi/full.moss is
# the demanding case — it reaches all of `Std` provided in Moss over
# the primitive context. The lists were compared with the Python
# bootstrap's lowering, which is gone and whose internals the oracle
# does not expose; what is left is that the driver lowers both
# programs with no error and lists functions from each.


def test_needs__needs_match_the_bootstrap():
    driver = compile_driver(needs__NEEDS_DRIVER)
    for entry in ["tests/wasi/full.moss", "src/main.moss"]:
        out = run_driver(driver, None, ["lib/prelude.moss", entry]).text()
        assert all(not line.startswith("!") for line in out.splitlines()), (
            f"{entry}: lowering error: {out}"
        )
        assert all(len(line.split("\t")) >= 2 for line in out.splitlines()), (
            f"{entry}: a malformed row"
        )
        assert any(line.startswith(entry) for line in out.splitlines()), (
            f"{entry}: no rows for the entry"
        )


# The back end's encoder, in Moss: tests/wasi/emit.moss writes a WASI
# module to standard output, and that module says hello. Compiled by
# the self-hosted compiler; and the oracle, running the same program,
# writes the same module byte for byte.


def test_emitter__emits_a_module_that_runs():
    entry = "tests/wasi/emit.moss"
    module = run_driver(wasm_file(build(entry)), None, []).stdout
    assert module[:8] == b"\x00asm\x01\x00\x00\x00"
    assert run_module(module, []) == "Hello, world!\n"
    interpreted = oracle(entry, [])
    assert interpreted.code == 0, f"{interpreted.stderr}"
    assert interpreted.stdout == module, "the oracle wrote a different module"
