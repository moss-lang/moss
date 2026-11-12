//! Parts of the compiler written in Moss, driven one at a time: the parser
//! (`src/syntax.moss`) against the oracle's Rocq parser, the module loader
//! and scope tables (`src/prog.moss`), the requirement lists of lowering
//! (`src/lower.moss`), and the Wasm encoder (`src/emit.moss`).
//!
//! A driver is an ordinary Moss program over `Std` that imports what it
//! drives, compiled by the compiler under test and run under Wasmtime.
//!
//! Ported from `TestSelfHostedSyntax`, `TestSelfHostedCollectScopes`,
//! `TestSelfHostedNeeds` and `TestSelfHostedEmitter` in the Python
//! bootstrap's `tests/test_run.py`.

use std::fs;
use std::path::Path;
use std::sync::OnceLock;

use moss_tests::{
    build, oracle, oracle_command, repo, run_module, scratch, sources, std_main, wasm_file,
    wasmtime, Out,
};

/// A driver, written as a whole program whose `main` assumes `Std` and
/// which imports `{root}/src/...`, made a program the platform runs and
/// compiled: the path of its module.
fn compile_driver(text: &str) -> std::path::PathBuf {
    let dir = sources(&[("main.moss", &std_main(text))]);
    wasm_file(&build(&format!("{dir}/main.moss")))
}

/// Run a compiled driver from the repository root (which it reads files
/// relative to), with `argv0` and `args`.
fn run_driver(driver: &Path, argv0: Option<&str>, args: &[&str]) -> Out {
    let out = wasmtime(driver, argv0, args);
    assert_eq!(out.code, 0, "the driver failed on {args:?}: {}", out.stderr);
    out
}

mod syntax {
    use super::*;

    /// Parse the file named by the first argument, and print `?` if the
    /// parser reported an error, then the tree in the compact form of the
    /// dump (which the oracle's `--dump` writes too).

    const PARSE_DRIVER: &str = "\
import \"{root}/src/ast.moss\" as ast;
import \"{root}/src/dump.moss\" as dump;
import \"{root}/src/intern.moss\" as intern;
import \"{root}/src/lex.moss\" as lexer;
import \"{root}/src/syntax.moss\" as syntax;
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
";

    /// Known self-hosted compiler limitation: compiling anything that
    /// imports `src/dump.moss` exhausts Wasmtime's default wasm stack (512
    /// KiB) in the self-hosted compiler — a stack overflow trap — though
    /// the oracle elaborates the driver at once; its long `else if` chains
    /// recurse deeply. The Python suite compiled its drivers with the
    /// Python back end and never met it. So this driver is compiled by the
    /// compiler under test with a 4 MiB wasm stack.
    fn parse_driver() -> &'static Path {
        static DRIVER: OnceLock<std::path::PathBuf> = OnceLock::new();
        DRIVER.get_or_init(|| {
            let dir = sources(&[("main.moss", &std_main(PARSE_DRIVER))]);
            let entry = format!("{dir}/main.moss");
            let default = moss_tests::wasmtime(moss_tests::compiler(), Some("lib/prelude.moss"), &[&entry]);
            assert!(
                default.code != 0 && default.stderr.contains("call stack exhausted"),
                "the self-hosted compiler no longer overflows its stack on the parse driver; \
                 compile it with `compile_driver` again: {}",
                default.stderr
            );
            let out = moss_tests::output(
                std::process::Command::new("wasmtime")
                    .args(["run", "-W", "max-wasm-stack=4194304", "--argv0", "lib/prelude.moss", "--dir", "."])
                    .arg(moss_tests::compiler())
                    .arg(&entry)
                    .current_dir(repo()),
            );
            assert_eq!(out.code, 0, "the compiler failed on the parse driver: {}", out.stderr);
            assert!(out.is_module(), "the parse driver did not compile: {}", out.text());
            wasm_file(&out.stdout)
        })
    }

    /// The oracle's parser on each file (relative to the repository): its
    /// dump and a newline, or `ERROR msg` and a newline.
    fn rocq_dumps(files: &[String]) -> Vec<String> {
        let dir = scratch().join(format!("dump-{}", files.len()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let dir_arg = dir.to_str().unwrap().to_string();
        let mut args = vec!["--dump", dir_arg.as_str()];
        args.extend(files.iter().map(String::as_str));
        let out = oracle_command(&args);
        assert_eq!(out.code, 0, "the oracle's --dump: {}", out.stderr);
        files
            .iter()
            .map(|file| {
                // The oracle's `mangle`: the absolute path, its `/`s `_`s.
                let path = repo().join(file);
                let mangled: Vec<&str> =
                    path.to_str().unwrap().split('/').filter(|s| !s.is_empty()).collect();
                let name = format!("{}.rocq.sx", mangled.join("_"));
                fs::read_to_string(dir.join(&name)).unwrap_or_else(|e| panic!("{name}: {e}"))
            })
            .collect()
    }

    /// Every live source file, including the quoted string fixtures.
    fn corpus() -> Vec<String> {
        let mut files = Vec::new();
        for dir in ["src", "lib", "examples", "tests/wasi", "tests/fixtures/string_literals"] {
            files.extend(moss_tests::moss_files(dir));
        }
        files
    }

    #[test]
    fn quoted_payload_ast_uses_contextual_byte_paths() {
        let file = moss_tests::source(r#"fn demo() { let text = "Aé\n\"\\\t\r"; }"#);
        let expected = rocq_dumps(&[file.clone()]).remove(0);
        let actual = run_driver(parse_driver(), None, &[&file]).text();
        assert_eq!(actual, expected);
        assert_eq!(actual.matches("(k string_write1 _)").count(), 6);
        assert_eq!(actual.matches("(k string_write2 _)").count(), 1);
        for name in ["41", "c3", "a9", "0a", "22", "5c", "09", "0d"] {
            assert!(actual.contains(&format!("(k byte::x{name} _)")), "{actual}");
        }
        // Only lengths and indices remain integer literals (8 and 0..7).
        for value in [65, 195, 169, 10, 34, 92, 9, 13] {
            assert!(!actual.contains(&format!("(Y {value})")), "{actual}");
        }

        let payload = "\u{800}\u{d7ff}\u{ffff}\u{10000}\u{50000}\u{10ffff}";
        let file = moss_tests::source(&format!("fn demo() {{ let text = \"{payload}\"; }}"));
        let expected = rocq_dumps(&[file.clone()]).remove(0);
        let actual = run_driver(parse_driver(), None, &[&file]).text();
        assert_eq!(actual, expected);
        assert_eq!(actual.matches("(k string_write3 _)").count(), 3);
        assert_eq!(actual.matches("(k string_write4 _)").count(), 3);
        for (name, lead, second) in [
            ("ThreeE0", "e0", "a0"), ("ThreeED", "ed", "9f"),
            ("ThreeOrdinary", "ef", "bf"), ("FourF0", "f0", "90"),
            ("FourMiddle", "f1", "90"), ("FourF4", "f4", "8f"),
        ] {
            let prefix = format!("(o (k byte::{name} _) [(p lead (k byte::x{lead} _)) (p second (k byte::x{second} _))])");
            assert!(actual.contains(&prefix), "{actual}");
        }
        for value in [224, 160, 128, 237, 159, 191, 239, 240, 144, 241, 244, 143] {
            assert!(!actual.contains(&format!("(Y {value})")), "{actual}");
        }
    }

    /// src/syntax.moss against the oracle's Rocq parser, over the whole
    /// corpus: both write the tree in the compact dump form (the Python
    /// bootstrap's `mossc/sexpr.py`, which the Rocq parser's dump
    /// reproduces byte for byte), and the two must agree character for
    /// character. This is the front end's real check — an approximate
    /// parser can produce a plausible letter dump, but it cannot produce
    /// the reference parser's tree.
    #[test]
    fn agrees_with_the_bootstrap_parser() {
        let files = corpus();
        let expected = rocq_dumps(&files);
        let mut failures = Vec::new();
        for (file, expected) in files.iter().zip(&expected) {
            assert!(!expected.starts_with("ERROR"), "the oracle cannot parse {file}: {expected}");
            let actual = run_driver(parse_driver(), None, &[file]).text();
            if actual != *expected {
                failures.push(file.clone());
            }
        }
        assert!(failures.is_empty(), "the parsers disagree on {failures:?}");
    }

    /// D68's partial order, as sources that group (the trees are compared).
    const OPERATOR_GROUPINGS: &[&str] = &[
        // Unary operators bind tightest, method calls and fields tighter.
        "-a * b",
        "!a & b",
        "- -a",
        "!!a",
        "-a.f(b)",
        "-x.y",
        // `*` chains; `/` and `%` take a product on the left, a unary on
        // the right.
        "a * b * c",
        "a * b * c / d",
        "a * b % c",
        "a / -b",
        "(a / b) * c",
        "a / (b / c)",
        // Additive: left-associative, looser than multiplicative.
        "a - b + c",
        "a - b - c",
        "a + b * c",
        "a * b + c / d",
        "a + b % c - d",
        // Comparison: looser than additive, either side.
        "a + b == c * d",
        "a != b",
        "a <= b - c",
        "-a >= b",
        "a > b",
        "a < b",
        // Shifts take unaries, and may be a comparison's operand.
        "a << b",
        "-a >> -b",
        "x << f(n) == y",
        "y == x >> n",
        "(a << b) + c",
        // Bitwise: each chains with itself, and nothing else.
        "a & b & c",
        "a | b | c",
        "a ^ b ^ c",
        "!a | !b",
        "(a < b) & (c < d)",
        "(x & mask) == z",
    ];

    /// Mixing that D68 refuses.
    const OPERATOR_ERRORS: &[&str] = &[
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
    ];

    /// Literal snippets (D68): (source, refused).
    const LITERAL_SNIPPETS: &[(&str, bool)] = &[
        ("1.add(2)", false),
        ("3.14 * r * r", false),
        ("-1 * 2", false),
        ("(-1).abs()", false),
        ("-(1.abs())", false),
        ("-1.abs()", true),
        ("-2.5.sqrt()", true),
        ("- 1.x", true),
        ("x - 1.abs()", false),
        ("1 << n + 1", true),
        ("0.5 + 007", false),
        ("f(10, 0.25)", false),
        // Exponents: the dump writes the exponent's value, `1E+03` as `1e3`.
        ("1e-9 + 2.5E10 * 1.5e+3", false),
        ("f(1E+03, 0.000123e-0004, 0e0, 7e999)", false),
        ("1e5.abs() - 1.exp()", false),
        ("1.e5 + 1.5.e3", false),
        ("-1e5.abs()", true),
    ];

    /// Anonymous-function snippets (D70): (source, refused).
    const LAMBDA_SNIPPETS: &[(&str, bool)] = &[
        ("bind f = (x) => x;", false),
        ("bind f = () => g();", false),
        ("bind f = (x: Int, y): Char => { h(x); y };", false),
        ("bind f = (x,) => x, g = (y) => y;", false),
        ("bind T.m = (n: This) => this.add(n);", false),
        ("bind f = (): () => {};", false),
        ("bind f = (x) => (x).g();", false),
        ("let v = (x) => x;", false),
        ("f((x) => x)", false),
        ("(x).g()", false),
        ("(x)", false),
        ("()", false),
        ("(x + y) * z", false),
        ("(x, y)", false),
        ("(x,)", false),
        ("(x, y).0", true),
        ("let x, y = (a, b);", false),
        ("let outer::Wrapped(Point {x, y = other: T}) = p;", false),
        ("let Marker() = marker;", false),
        ("let ((x: T,), {name = y: U}) = v;", false),
        ("bind f = ((x: Int, y: Int)) => x;", false),
        ("bind f = (x, y) => (x, y);", false),
        ("(x: Int)", true),
        ("bind f = (x) =>;", true),
        ("bind f = (x): => x;", true),
        ("bind f = (1) => x;", true),
    ];

    /// Each snippet as the body of `fn f() { ... }`: where it is refused,
    /// the oracle's dump is `ERROR ...` and the self-hosted driver prints
    /// `?` first; otherwise the two print the same tree.
    fn agree_on_snippets(cases: &[(&str, bool)]) {
        let texts: Vec<String> = cases.iter().map(|(source, _)| format!("fn f() {{ {source} }}\n")).collect();
        let names: Vec<String> = (0..cases.len()).map(|i| format!("{i}.moss")).collect();
        let files: Vec<(&str, &str)> =
            names.iter().zip(&texts).map(|(n, t)| (n.as_str(), t.as_str())).collect();
        let dir = sources(&files);
        let paths: Vec<String> = names.iter().map(|n| format!("{dir}/{n}")).collect();
        let expected = rocq_dumps(&paths);
        let mut failures = Vec::new();
        for (((source, refused), path), expected) in cases.iter().zip(&paths).zip(&expected) {
            let actual = run_driver(parse_driver(), None, &[path]).text();
            let ok = if *refused {
                expected.starts_with("ERROR ") && actual.starts_with('?')
            } else {
                !actual.starts_with('?') && actual == *expected
            };
            if !ok {
                failures.push(format!("{source:?}: oracle {expected:?}, self-hosted {actual:?}"));
            }
        }
        assert!(failures.is_empty(), "{}", failures.join("\n"));
    }

    /// D68's precedence table, snippet by snippet: the same tree where the
    /// reference parser groups, and a syntax error where it refuses. The
    /// self-hosted parser reports an error as a count, so a refusal is the
    /// `?` its driver prints.
    #[test]
    fn operators_agree_with_the_bootstrap_parser() {
        let mut cases: Vec<(&str, bool)> = OPERATOR_GROUPINGS.iter().map(|s| (*s, false)).collect();
        cases.extend(OPERATOR_ERRORS.iter().map(|s| (*s, true)));
        cases.extend_from_slice(LITERAL_SNIPPETS);
        agree_on_snippets(&cases);
    }

    /// D70: an anonymous function is told from `()` and from a
    /// parenthesized expression by what follows its parenthesis. The same
    /// trees, snippet by snippet — where it is legal and where it is not,
    /// which is for elaboration to say — and the same refusals.
    #[test]
    fn lambdas_agree_with_the_bootstrap_parser() {
        agree_on_snippets(LAMBDA_SNIPPETS);
    }
}

mod collect_scopes {
    use super::*;

    /// Load the program `arg 2` with the prelude `arg 1`, and print each
    /// collect error as `!CODE MODULE [NAME]`, then each module's scope as
    /// rows of (module, namespace, name, target module, target name), then
    /// its attached methods.
    const SCOPE_DRIVER: &str = "\
import \"{root}/src/ast.moss\" as ast;
import \"{root}/src/boot.moss\" use Boot, Compiler;
import \"{root}/src/bytes.moss\" use *;
import \"{root}/src/intern.moss\" as intern;
import \"{root}/src/prog.moss\" as prog;

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
";

    /// src/prog.moss's module graph, the symbols every declaration gets,
    /// and the scope each module ends up with — names, detached methods,
    /// aliases, and attached methods keyed by receiver — over the
    /// compiler's own sources and over tests/wasi/full.moss. These rows
    /// were compared with the Python bootstrap's `collect`, which is gone
    /// and has no counterpart in the oracle; what is left is that the
    /// driver loads both programs with no collect error, and that its rows
    /// are well formed and name the modules they should.
    #[test]
    fn scopes_match_the_bootstrap() {
        let driver = compile_driver(SCOPE_DRIVER);
        for entry in ["src/main.moss", "tests/wasi/full.moss"] {
            let out = run_driver(&driver, None, &["lib/prelude.moss", entry]).text();
            let rows: Vec<Vec<&str>> = out.lines().map(|line| line.split('\t').collect()).collect();
            assert!(rows.iter().all(|row| !row[0].starts_with('!')), "{entry}: collect error: {out}");
            assert!(rows.iter().all(|row| row.len() == 5), "{entry}: a malformed row");
            assert!(rows.iter().all(|row| ["0", "1", "2", "@"].contains(&row[1])), "{entry}");
            assert!(rows.iter().any(|row| row[0].ends_with(entry)), "{entry}: no rows for the entry");
            assert!(rows.iter().any(|row| row[0].ends_with("lib/prelude.moss")), "{entry}: no prelude");
        }
    }
}

mod needs {
    use super::*;

    /// Load and lower the program `arg 2` with the prelude `arg 1`, and
    /// print each lowered function's requirement list: module, name, and
    /// each need (a method's as `Recv.name`).
    const NEEDS_DRIVER: &str = "\
import \"{root}/src/ast.moss\" as ast;
import \"{root}/src/boot.moss\" use Boot, Compiler;
import \"{root}/src/bytes.moss\" use *;
import \"{root}/src/intern.moss\" as intern;
import \"{root}/src/lower.moss\" as lower;
import \"{root}/src/prog.moss\" as prog;
import \"{root}/src/types.moss\" as types;

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
";

    /// src/lower.moss's requirement lists. A declaration nested in
    /// `assume` blocks is implicitly parameterized by everything they name
    /// (D26), so this is where contexts stop being syntax: the list, in
    /// order, is a function's calling convention. tests/wasi/full.moss is
    /// the demanding case — it reaches all of `Std` provided in Moss over
    /// the primitive context. The lists were compared with the Python
    /// bootstrap's lowering, which is gone and whose internals the oracle
    /// does not expose; what is left is that the driver lowers both
    /// programs with no error and lists functions from each.
    #[test]
    fn needs_match_the_bootstrap() {
        let driver = compile_driver(NEEDS_DRIVER);
        for entry in ["tests/wasi/full.moss", "src/main.moss"] {
            let out = run_driver(&driver, None, &["lib/prelude.moss", entry]).text();
            assert!(out.lines().all(|line| !line.starts_with('!')), "{entry}: lowering error: {out}");
            assert!(out.lines().all(|line| line.split('\t').count() >= 2), "{entry}: a malformed row");
            assert!(out.lines().any(|line| line.starts_with(entry)), "{entry}: no rows for the entry");
        }
    }
}

mod emitter {
    use super::*;

    /// The back end's encoder, in Moss: tests/wasi/emit.moss writes a WASI
    /// module to standard output, and that module says hello. Compiled by
    /// the self-hosted compiler; and the oracle, running the same program,
    /// writes the same module byte for byte.
    #[test]
    fn emits_a_module_that_runs() {
        let entry = "tests/wasi/emit.moss";
        let module = run_driver(&wasm_file(&build(entry)), None, &[]).stdout;
        assert_eq!(&module[..8], b"\0asm\x01\0\0\0");
        assert_eq!(run_module(&module, &[]), "Hello, world!\n");
        let interpreted = oracle(entry, &[]);
        assert_eq!(interpreted.code, 0, "{}", interpreted.stderr);
        assert!(interpreted.stdout == module, "the oracle wrote a different module");
    }
}
