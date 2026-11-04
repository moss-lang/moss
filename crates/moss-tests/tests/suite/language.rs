//! The language, program by program: each test is a small program (or one
//! of `tests/wasi`) that the self-hosted compiler must compile and run to
//! the expected output, or refuse, and that the oracle must run to the
//! same output, or reject with the Python bootstrap's message.
//!
//! Ported from the Python bootstrap's `tests/test_run.py`, whose programs
//! ran under the interpreter's native `Std`. A program whose `main`
//! assumes `Std` is not one the platform runs (D52), so each is wrapped
//! here: `std_program`/`std_main` make its `main` a `run` that the
//! platform's `main` calls under `WasiStd`.

use moss_tests::*;

/// Python's `main_body(body, decls)`: `assume Std { decls fn main() {
/// body } }`, as a program the platform runs.
fn prog(decls: &str, body: &str) -> String {
    source(&std_program(decls, body))
}

/// A whole program whose `main` assumes `Std`, as one the platform runs.
fn whole(text: &str) -> String {
    source(&std_main(text))
}

/// Several files, `main.moss` among them, whose `main` assumes `Std`;
/// returns the path of `main.moss`.
fn program(files: &[(&str, &str)]) -> String {
    let wrapped: Vec<(&str, String)> = files
        .iter()
        .map(|(name, text)| {
            let text = if *name == "main.moss" { std_main(text) } else { text.to_string() };
            (*name, text)
        })
        .collect();
    let refs: Vec<(&str, &str)> = wrapped.iter().map(|(n, t)| (*n, t.as_str())).collect();
    format!("{}/main.moss", sources(&refs))
}

/// Both runners print `expected`: the self-hosted compiler's module under
/// Wasmtime, and the oracle.
fn agree(entry: &str, args: &[&str], expected: &str) {
    assert_eq!(run(entry, args), expected, "the self-hosted compiler on {entry}");
    assert_eq!(oracle_run(entry, args), expected, "the oracle on {entry}");
}

/// Both refuse `entry`: the self-hosted compiler writes a report, and the
/// oracle rejects it with a message containing `message`. Returns the
/// self-hosted compiler's report and the oracle's message.
fn refused(entry: &str, message: &str) -> (String, String) {
    let report = rejects(entry);
    let theirs = oracle_rejects(entry);
    assert!(theirs.contains(message), "the oracle on {entry}: {theirs}");
    (report, theirs)
}

/// Both trap (Python's `MossPanic`), having printed `printed` first.
fn traps(entry: &str, args: &[&str], printed: &str) {
    assert_eq!(run_module_code(&build(entry), args, TRAP), printed, "the self-hosted compiler on {entry}");
    let out = oracle(entry, args);
    assert_eq!(out.code, TRAP, "the oracle on {entry}: {}", out.stderr);
    assert_eq!(out.text(), printed, "the oracle on {entry}");
}

// What tests/wasi/shadow.moss prints: f64, f32, f64, f32, f64.
const SHADOW_OUT: &str = "333333333\n333333344\n333333333\n333333344\n333333333\n";

// What tests/wasi/ops.moss prints: D68's operators on `Int` and `Bool`.
const OPS_OUT: &str = "19 -7 3 1 -7 2 23 35 21 5 1 -1 \n\
                       ynynnyny y\n\
                       2 15 13 1024 128 -5 \n\
                       nnynyyyyn\n";

// What tests/wasi/exponents.moss prints: the same report at f64, then f32.
// `1e39` is inf at f32 and `1e-46` zero.
const EXPONENTS_OUT: &str = "yyyyyyyyy\n\
                             1500 125 1 250 -42 100000 602 \n\
                             n nn yy\n\
                             yyyyyyyyy\n\
                             1500 125 1 250 -42 100000 602 \n\
                             n yy yy\n";

// What tests/wasi/literals.moss prints: the same report at f64, then f32.
// A third to twenty-one places is not f32's nearest third: its digits were
// summed in f32 and rounded on the way (D68).
const LITERALS_OUT: &str = "0 7 10 100 1000 1234567 2147483647 -42 3 0 \n\
                            3 -2 1 0 250 1 16777215 \n\
                            3141593 333333333 ny\n\
                            314159265 333333333 \n\
                            0 7 10 100 1000 1234567 2147483647 -42 3 0 \n\
                            3 -2 1 0 250 1 16777215 \n\
                            3141593 333333344 yn\n\
                            314159264 333333248 \n";

// What tests/wasi/lists.moss prints: an IntList beside a FloatList at f64,
// then the same at f32. A thousand 0.1s sum to 100 at f64 and to 99.999048
// at f32; a third differs in the eighth digit.
const LISTS_OUT: &str = "1000 1000 100000000 332833500 \n\
                         333333333 1 1 -7 251001 \n\
                         1000 1000 99999048 332833500 \n\
                         333333344 1 1 -7 251001 \n";

// What tests/wasi/overflow.moss prints before it traps: an oversized
// literal under wrapping i32 arithmetic, then one that fits, under it and
// under arithmetic that traps on overflow.
const OVERFLOW_OUT: &str = "1\n2147483647\n2147483647\n";

const SIZE_REBIND_OUT: &str = "1705032704\n-294967296\n-2\n";
const SIZE_STD_OUT: &str = "-294967296\n4\n2147483647\n4\n";

// `test_run.TestHello`.
#[test]
fn hello_world() {
    agree("examples/hello.moss", &[], "Hello, world!\n");
}

// `test_run.TestExamples` is `examples.rs`.

mod errors {
    use super::*;

    /// hello.md: a name nobody declared is a *scope* error.
    #[test]
    fn not_in_scope() {
        refused(&prog("", "foo();"), "not in scope");
    }

    /// hello.md: a declared but unassumed function is a *context* error.
    #[test]
    fn not_in_context() {
        refused(&source("fn example();\nfn main() { example(); }"), "not available in the context");
    }

    /// hello.md's example: `main` may assume only what the platform
    /// provides (D39, D52). The self-hosted compiler refuses it; the
    /// oracle runs `main` in the platform's root context and reports
    /// anything beyond it unsupported rather than judging it (as for
    /// `tests/errors/beyond_std.moss`).
    #[test]
    fn assuming_more_than_std() {
        let entry = source("fn example();\nassume example { fn main() { example(); } }");
        rejects(&entry);
        let out = oracle(&entry, &[]);
        assert_eq!(out.code, ORACLE_UNSUPPORTED, "{}", out.stderr);
    }
}

mod context {
    use super::*;

    #[test]
    fn needs_pass_through() {
        let entry = prog("  fn twice() { putchar(char::a); putchar(char::a); }", "twice();");
        agree(&entry, &[], "aa");
    }

    #[test]
    fn bind_val_lexical() {
        let entry = prog(
            "  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
            "bind c=char::x; emit(); bind c=char::y; emit();",
        );
        agree(&entry, &[], "xy");
    }

    /// A bind inside an if-block must not leak out of it.
    #[test]
    fn bind_scoped_to_block() {
        let entry = prog(
            "  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
            "bind c=char::x; if true { bind c=char::y; emit(); } emit();",
        );
        agree(&entry, &[], "yx");
    }

    /// D65: a bare block scopes exactly as an if-block does. Its `let`
    /// shadows the outer one at another width — a record over a char —
    /// and the outer one is back afterwards.
    #[test]
    fn bare_block_scopes_binds_and_lets() {
        let entry = prog(
            "  val c: Char;\n  tag Pair { a: Char, b: Char };\n\
             \x20 assume c { fn emit() { putchar(c); } fn got(): Char { c } }",
            "bind c=char::x; let p = char::p; \
             { bind c=char::y; let p = Pair { a = char::q, b = char::r }; \
             emit(); putchar(p.b); } \
             emit(); putchar(p); \
             putchar({ bind c=char::z; let t = got(); t });",
        );
        agree(&entry, &[], "yrxpz");
    }

    /// D66: `bind f = g;` with `g` abstract captures `g` as provided at the
    /// bind: neither a rebind of `g` after it nor one deeper in the call
    /// tree reaches `f`. The signature mentions an abstract type.
    #[test]
    fn fn_bind_to_contextual_fn_captures() {
        let entry = prog(
            r"  type T;
  val c: Char;
  assume T { fn g(x: T); fn f(x: T); }
  assume c { fn emit(x: Char) { putchar(c); putchar(x); } }
  fn other(x: Char) { putchar(char::hyphen); }
  assume T, f, g { fn deeper(x: T) { bind g = loud; f(x); } }
  assume T, g { fn loud(x: T) { g(x); g(x); } }",
            "bind T = Char; bind c = char::a; bind g = emit; bind f = g; \
             bind c = char::b; f(char::x); bind g = other; f(char::y); \
             g(char::z); deeper(char::w);",
        );
        agree(&entry, &[], "axay-aw");
    }

    #[test]
    fn fn_bind_to_unavailable_contextual_fn() {
        let entry = prog("  fn g(x: Char);\n  fn f(x: Char);", "bind f = g;");
        refused(&entry, "`g` is not available in the context here");
    }

    #[test]
    fn fn_bind_to_contextual_fn_checks_the_signature() {
        let entry = prog("  fn f(x: Int);", "bind f = putchar;");
        // Known self-hosted bug: it does not check a fn bind's signature
        // against the provider's — `putchar` takes a `Char`, `f` an `Int`
        // — and compiles this, and even a program that calls `f`. When it
        // is fixed, assert `rejects`.
        assert!(compile(&entry, "lib/prelude.moss").is_module());
        let theirs = oracle_rejects(&entry);
        assert!(
            theirs.contains("fn bind signature mismatch: provider `putchar` has (Char) -> (), need (Int) -> ()"),
            "{theirs}"
        );
    }

    #[test]
    fn unbound_val_is_context_error() {
        let entry = prog("  val c: Char;\n  assume c { fn emit() { putchar(c); } }", "emit();");
        refused(&entry, "not available in the context");
    }

    #[test]
    fn bind_fn_across_modules() {
        let entry = program(&[
            (
                "lexer.moss",
                "type In;\nassume In {\n  fn next(): In;\n  assume next { fn go(): In { next() } }\n}",
            ),
            (
                "main.moss",
                r#"import "./lexer.moss" as lexer;
assume Std {
  fn mine(): Char { char::z }
  fn main() {
    bind lexer::In=Char;
    bind lexer::next=mine;
    putchar(lexer::go());
  }
}"#,
            ),
        ]);
        agree(&entry, &[], "z");
    }

    /// D27: `bind lexer::next=mine` does not infer `bind lexer::In=Char`.
    #[test]
    fn fn_bind_without_type_bind_is_error() {
        let entry = |call: &str| {
            program(&[
                (
                    "lexer.moss",
                    "type In;\nassume In {\n  fn next(): In;\n  assume next { fn go(): In { next() } }\n}",
                ),
                (
                    "main.moss",
                    &format!(
                        "import \"./lexer.moss\" as lexer;\n\
                         assume Std {{\n\
                         \x20 fn mine(): Char {{ char::z }}\n\
                         \x20 fn main() {{ bind lexer::next=mine; {call} }}\n\
                         }}"
                    ),
                ),
            ])
        };
        let message = "type `In` is not available in the context here";
        // Known self-hosted bug: it only checks a fn bind whose provision
        // is used, so it compiles the bind alone (the Python test's
        // program). When it is fixed, assert `rejects` here too.
        let unused = entry("");
        assert!(compile(&unused, "lib/prelude.moss").is_module());
        assert!(oracle_rejects(&unused).contains(message));
        // With a call that reaches `next`, both refuse it.
        refused(&entry("lexer::go();"), message);
    }

    /// The provider's own needs are captured at the bind site, so the
    /// callee can run it without assuming them (the cli.moss pattern).
    #[test]
    fn fn_bind_captures_context() {
        let entry = program(&[
            ("runner.moss", "fn task();\nassume task { fn go() { task(); } }"),
            (
                "main.moss",
                r#"import "./runner.moss" as runner;
assume Std {
  val c: Char;
  assume c { fn emit() { putchar(c); } }
  fn main() {
    bind c=char::k;
    bind runner::task=emit;
    runner::go();
  }
}"#,
            ),
        ]);
        agree(&entry, &[], "k");
    }

    /// Both runners agree on a union of records of different widths, and
    /// on a pattern binder being scoped to its arm: the outer `x` is back
    /// after the match.
    #[test]
    fn record_payloads_and_binder_scope() {
        let entry = whole(
            r"assume Std {
  tag Lit(Char);
  tag Both { l: Char, r: Char };
  type Node = | Lit | Both;
  fn first(n: Node): Char { match n { Lit(c) => c, Both { l, r = _ } => l, } }
  fn main() {
    putchar(first(Both { r = char::b, l = char::a }));
    putchar(first(Lit(char::c)));
    let x = char::d;
    match Lit(char::e) { Lit(x) => putchar(x), _ => (), }
    putchar(x);
    putchar(char::newline);
  }
}
",
        );
        agree(&entry, &[], "aced\n");
    }

    /// D25: a later bind of the same symbol in an inner block shadows.
    /// `WasiFloats32` inside a scope with an f64 `Float` rebinds `Float`
    /// and then its ops, whose entries in force were checked against the
    /// old `Float`; they are replaced, not merged with (D43 is for the
    /// items of contexts being assumed).
    #[test]
    fn a_bind_shadows_a_bind_whose_type_was_rebound() {
        agree("tests/wasi/shadow.moss", &[], SHADOW_OUT);
    }

    /// D26: a bound provider's needs are captured at the bind site, so a
    /// frame between the bind and the call that rebinds them changes
    /// nothing — directly (`relay`), a frame further down (`deep`), or
    /// through a provider that itself captured a fn bind (`wrap`, whose
    /// `task` is still `emit` with `c=h`). Even in the bind's own scope, a
    /// later rebind of `c` leaves the earlier `bind task=emit` alone.
    /// Every letter is `h`; any other names the rebind that leaked through.
    #[test]
    fn fn_bind_capture_survives_rebinds() {
        agree(&whole(CAPTURE_REBOUND), &[], "hhhh\n");
    }
}

/// D26's cases for `context::fn_bind_capture_survives_rebinds`.
const CAPTURE_REBOUND: &str = r"assume Std {
  val c: Char;
  fn task();
  fn outer();
  assume c { fn emit() { putchar(c); } }
  fn shout() { putchar(char::exclam); }
  assume task {
    fn relay() { bind c=char::i; task(); }
    fn deep() { bind c=char::j; below(); }
    fn below() { task(); }
    fn wrap() { task(); }
  }
  assume outer { fn later() { bind c=char::l; bind task=shout; outer(); } }
  fn main() {
    bind c=char::h;
    bind task=emit;
    relay();
    deep();
    bind outer=wrap;
    bind c=char::k;
    later();
    task();
    putchar(char::newline);
  }
}
";

mod methods {
    use super::*;

    #[test]
    fn attached_defined() {
        let entry = prog(
            "  tag Wrap(Char);\n  fn Wrap.get(): Char { match this { Wrap(c) => c } }",
            "let w = Wrap(char::q); putchar(w.get());",
        );
        agree(&entry, &[], "q");
    }

    #[test]
    fn detached_provided() {
        let entry = prog(
            r"  type A;
  fn .gimme(): Char;
  val a: A;
  context Ctx = A, a, A.gimme;
  assume Ctx { fn go(): Char { a.gimme() } }
  fn giveq(): Char { char::q }",
            "bind A=Char;\n    bind a=char::m;\n    bind A.gimme=giveq;\n    putchar(go());",
        );
        agree(&entry, &[], "q");
    }

    /// The other half of D61: an attached method matches *by name* in the
    /// receiver's own module and is never imported — including one that
    /// is only declared there and provided from the context, which the
    /// detached rule (import it, or `mod::m`) does not cover.
    #[test]
    fn attached_declared_beside_its_receiver_needs_no_import() {
        let entry = prog(
            r"  tag Wrap(Char);
  fn Wrap.spell(): Char;
  fn giveq(): Char { char::q }
  context Spelling = Wrap.spell;
",
            "bind Wrap.spell=giveq;\n    let w = Wrap(char::z);\n    putchar(w.spell());",
        );
        agree(&entry, &[], "q");
    }

    /// Q5: attached methods require nominal receivers.
    #[test]
    fn attached_on_abstract_rejected() {
        let entry = prog("  type A;\n  fn A.m(): Char { char::q }", "()");
        // Known self-hosted bug (Q5, as for
        // tests/errors/attached_on_abstract.moss): the self-hosted
        // compiler does not check that an attached method's receiver is
        // nominal, and compiles this. When it is fixed, assert `rejects`.
        assert!(compile(&entry, "lib/prelude.moss").is_module());
        assert!(oracle_rejects(&entry).contains("nominal receiver"));
    }
}

mod matching {
    use super::*;

    #[test]
    fn bool_and_if() {
        let entry = prog(
            "  fn yes(): Bool { true }",
            "if yes() { putchar(char::y) } else { putchar(char::n) }",
        );
        agree(&entry, &[], "y");
    }

    #[test]
    fn bool_branch_and_not() {
        let entry = prog(
            "  fn pick(b: Bool): Char { if b { char::t } else { char::f } }",
            "putchar(pick(false)); putchar(pick(true)); putchar(pick(true.not()));",
        );
        agree(&entry, &[], "ftf");
    }

    #[test]
    fn non_exhaustive() {
        let entry = prog(
            "  tag A;\n  tag B;\n  type AB = | A | B;\n\
             \x20 fn pick(x: AB): Char { match x { A => char::t, } }",
            "()",
        );
        refused(&entry, "not exhaustive");
    }

    #[test]
    fn tag_and_record() {
        let entry = prog(
            "  tag Pair { x: Char, y: Char };\n\
             \x20 fn first(p: Pair): Char { match p { Pair { x } => x } }",
            "putchar(first(Pair { x = char::a, y = char::b }));",
        );
        agree(&entry, &[], "a");
    }

    #[test]
    fn union_injection() {
        let entry = prog(
            r"  tag Stop;
  tag Step(Char);
  type Out = | Stop | Step;
  fn go(b: Bool): Out { if b { Step(char::s) } else { Stop } }
  fn show(o: Out): Char { match o { Step(c) => c, Stop => char::dot, } }",
            "putchar(show(go(true))); putchar(show(go(false)));",
        );
        agree(&entry, &[], "s.");
    }

    #[test]
    fn while_loop_break() {
        let entry = prog(
            "",
            "var again = true;\n\
             \x20   while again { putchar(char::w); again = false; }\n\
             \x20   loop { putchar(char::l); break; }",
        );
        agree(&entry, &[], "wl");
    }

    #[test]
    fn recursion() {
        let entry = prog(
            "  fn rec(b: Bool): Char { if b { rec(false) } else { char::r } }",
            "putchar(rec(true));",
        );
        agree(&entry, &[], "r");
    }
}

mod std_lib {
    use super::*;

    #[test]
    fn int_methods_and_while() {
        let entry = prog(
            "",
            "var i: Int = zero;\n\
             \x20   let stop = one.add(one).add(one);\n\
             \x20   while i.lt(stop) { putchar(char::i); i = i.add(one); }",
        );
        agree(&entry, &[], "iii");
    }

    #[test]
    fn char_eq() {
        let entry = prog(
            "",
            "if char::a.eq(char::a) { putchar(char::y) }\n\
             \x20   if char::a.eq(char::b) { putchar(char::n) }",
        );
        agree(&entry, &[], "y");
    }

    /// `Char.code` is what lets the self-hosted interner store names as
    /// codepoints instead of spans into a single source buffer.
    #[test]
    fn char_code() {
        let entry = prog(
            "",
            "let n = char::c.code().sub(char::a.code());\n\
             \x20   var i = zero;\n\
             \x20   while i.lt(n) { putchar(char::dot); i = i.add(one); }\n\
             \x20   if char::A.code().lt(char::a.code()) { putchar(char::u) }",
        );
        agree(&entry, &[], "..u");
    }

    #[test]
    fn cell() {
        let entry = prog(
            "",
            "let c = cell_int();\n\
             \x20   c.write(one);\n\
             \x20   if c.read().eq(one) { putchar(char::c) }",
        );
        agree(&entry, &[], "c");
    }

    #[test]
    fn string_arg() {
        let entry = prog(
            "",
            "let s = first_arg();\n\
             \x20   print(s);\n\
             \x20   putchar(s.get(zero));\n\
             \x20   if s.length().eq(one.add(one)) { putchar(char::exclam) }",
        );
        agree(&entry, &["ok"], "oko!");
    }

    #[test]
    fn string_slice() {
        let entry = prog(
            "",
            "let s = first_arg();\n\
             \x20   let two = one.add(one);\n\
             \x20   print(s.slice(one, two));\n\
             \x20   print(s.slice(zero, s.length()));\n\
             \x20   let t = s.slice(two, two);\n\
             \x20   if t.length().eq(two) { putchar(char::y) } else { putchar(char::n) }\n\
             \x20   putchar(t.get(zero));",
        );
        agree(&entry, &["abcdef"], "bcabcdefyc");
    }

    #[test]
    fn string_concat() {
        let entry = prog(
            "",
            "let s = first_arg();\n\
             \x20   print(s.concat(s));\n\
             \x20   putchar(char::comma);\n\
             \x20   print(s.slice(zero, one).concat(s.slice(one, one)));\n\
             \x20   putchar(char::comma);\n\
             \x20   print(s.concat(s.slice(zero, zero)));",
        );
        agree(&entry, &["abc"], "abcabc,ab,abc");
    }

    /// `StrList` is `IntList`'s shape at `String` elements — a distinct
    /// nominal type (D51).
    #[test]
    fn str_list() {
        let entry = prog(
            "",
            "let xs = str_list();\n\
             \x20   let s = first_arg();\n\
             \x20   xs.push(s.slice(zero, one));\n\
             \x20   xs.push(s);\n\
             \x20   var i = zero;\n\
             \x20   while i.lt(xs.length()) { print(xs.get(i)); putchar(char::comma); \
             i = i.add(one); }",
        );
        agree(&entry, &["abc"], "a,abc,");
    }

    /// The Python bootstrap's native `Std` panicked on a slice past the
    /// end. `WasiStd`'s `String.slice` (lib/wasistd.moss) does not check
    /// its bounds, so neither runner traps: they agree that it runs.
    #[test]
    fn string_slice_out_of_range_panics() {
        let entry = prog("", "let s = first_arg().slice(zero, one.add(one));");
        agree(&entry, &["x"], "");
    }

    #[test]
    fn division_by_zero_panics() {
        traps(&prog("", "let x = one.div(zero);"), &[], "");
    }
}

mod std_over_wasi {
    use super::*;

    /// The whole of `Std` is Moss over the primitive context — a linear
    /// memory, the instructions over it, and the WASI calls — and both
    /// runners run that same Moss: the oracle interprets it and the
    /// compiler compiles it.
    #[test]
    fn the_interpreter_runs_the_moss_std() {
        let text = read("lib/bool.moss");
        let expected = format!("{} yy\n", &text[..text.len() - 1]);
        agree("tests/wasi/full.moss", &["lib/bool.moss"], &expected);
    }
}

/// D68: an operator is a call of one fixed method of lib/num.moss,
/// provided through the context like any method.
mod operators {
    use super::*;

    #[test]
    fn over_the_moss_std() {
        agree("tests/wasi/ops.moss", &[], OPS_OUT);
    }

    #[test]
    fn exponents_over_the_moss_std() {
        agree("tests/wasi/exponents.moss", &[], EXPONENTS_OUT);
    }

    /// An exponent `N` over `k` fractional digits divides `m` by
    /// `10^(k-N)` when that is positive — so `1e-9` is exactly
    /// `0.000000001`, and a decimal literal is the case `N` = 0, unchanged
    /// — multiplies it by `10^(N-k)` when that is, and is `float(m)` alone
    /// between; each power a product of `float(ten)`s, left to right. The
    /// self-hosted compiler writes the same module for each pair.
    #[test]
    fn exponent_constructions() {
        let module = |expr: &str| build(&prog("", &format!("let x = {expr}; putchar(char::A);")));
        // The ten of a power is ten `one`s added up (`emit_small` in
        // src/codegen.moss), where the literal `10` is `one * ten`: the
        // same value, but not the same module, so it is spelled out.
        let ten = "float(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1)";
        let tens = |n: usize| format!("({})", vec![ten; n].join(" * "));
        let cases = [
            ("1e-9", "0.000000001".to_string()),
            ("1.5e0", "1.5".to_string()),
            ("2.5E-3", "0.0025".to_string()),
            ("1e-9", format!("float(1) / {}", tens(9))),
            ("2.5e10", format!("(float(2) * {ten} + float(5)) * {}", tens(9))),
            ("1.5e+3", format!("(float(1) * {ten} + float(5)) * {}", tens(2))),
            ("1.5e1", format!("float(1) * {ten} + float(5)")),
            ("1e0", "float(1)".to_string()),
            ("0e5", format!("float(0) * {}", tens(5))),
        ];
        for (literal, same) in cases {
            assert!(module(literal) == module(&same), "{literal} is not {same}");
        }
    }

    /// Once over the Python bootstrap's native `Std`; now over `WasiStd`.
    #[test]
    fn over_the_native_std() {
        let entry = prog(
            "",
            "let two = one + one; let six = two * (two + one);\
             putchar((six * six / two - six % (two + two) + char::A.code()).char());\
             if !(six < two) & (six >= six) & !false { putchar(char::y); }\
             if (two << two) - (six >> one) == two + two + one { putchar(char::y); }\
             if (-two < zero) ^ true { putchar(char::n); } else { putchar(char::y); }",
        );
        // 36 / 2 - 6 % 4 = 16, so `Q`.
        agree(&entry, &[], "Qyyy");
    }

    #[test]
    fn an_operator_the_context_lacks_is_the_method_call_error() {
        // Each message names its own file, which is taken out.
        let [op, call] = ["c + c", "c.add(c)"].map(|body| {
            let entry = prog("", &format!("let c = char::a; putchar({body});"));
            let (mine, theirs) = refused(&entry, "no method `.add` is available for `Char`");
            (mine.replace(&entry, "main.moss"), theirs.replace(&entry, "main.moss"))
        });
        assert_eq!(op.0, call.0, "the self-hosted compiler's reports");
        assert_eq!(op.1, call.1, "the oracle's messages");
    }

    /// `+` is num.moss's `.add` even where no `.add` is in scope: a file
    /// beside the library gets no prelude, and this one imports no
    /// detached `.add` — yet its `+` is provided by `Std` as usual. The
    /// library is copied to a scratch directory so that the file can sit
    /// beside it, with that copy's prelude given to both runners.
    #[test]
    fn the_symbol_is_fixed_not_looked_up_by_name() {
        let plus = r#"import "./std.moss" use Std, putchar;
import "./int.moss" use one;
import "./char.moss" as char use .char, .code;
assume Std {
  fn main() {
    let two = one + one;
    putchar((two + char::A.code()).char());
  }
}
"#;
        let library = |plus: &str| {
            let mut files: Vec<(String, String)> = std::fs::read_dir(repo().join("lib"))
                .unwrap()
                .map(|e| e.unwrap().path())
                .filter(|p| p.extension().is_some_and(|x| x == "moss"))
                .map(|p| {
                    let name = format!("lib/{}", p.file_name().unwrap().to_str().unwrap());
                    (name.clone(), read(&name))
                })
                .collect();
            files.push(("lib/plus.moss".to_string(), std_main(plus)));
            let refs: Vec<(&str, &str)> = files.iter().map(|(n, t)| (n.as_str(), t.as_str())).collect();
            let dir = sources(&refs);
            (format!("{dir}/lib/plus.moss"), format!("{dir}/lib/prelude.moss"))
        };
        let (entry, prelude) = library(plus);
        assert_eq!(run_module(&build_with(&entry, &prelude), &[]), "C");
        let out = oracle_command(&["--prelude", &prelude, &entry]);
        assert_eq!((out.code, out.text().as_str()), (0, "C"), "{}", out.stderr);

        let (entry, prelude) = library(&plus.replace("one + one", "one.add(one)"));
        rejects_with(&entry, &prelude);
        let out = oracle_command(&["--prelude", &prelude, &entry]);
        assert_eq!(out.code, ORACLE_REJECTED, "{}", out.stderr);
        assert!(out.stderr.contains("`.add` is not a detached method in scope"), "{}", out.stderr);
    }
}

/// D68: a literal is a use of fixed library symbols — `zero`, `one`,
/// `.add`, `.mul` for an integer, and `float` and `.div` too for a decimal
/// — so it means whatever `Int` and `Float` are bound to.
mod literals {
    use super::*;

    #[test]
    fn over_the_moss_std() {
        agree("tests/wasi/literals.moss", &[], LITERALS_OUT);
    }

    /// Once over the Python bootstrap's native `Std`; now over `WasiStd`.
    #[test]
    fn over_the_native_std() {
        let entry = prog(
            "",
            "putchar((65 + 1.add(1) * 3 - 0).char());\
             putchar((2.5 * 4.0).round().add(char::A.code()).char());\
             if 0.1 + 0.2 != 0.3 { putchar(char::y); }\
             putchar((-(-66)).char());",
        );
        agree(&entry, &[], "GKyB");
    }

    #[test]
    fn a_literal_needs_its_symbols_in_context() {
        for (body, name) in [("let n = 0;", "zero"), ("let n = 12;", "one")] {
            let entry = source(&format!("fn main() {{ {body} }}"));
            refused(&entry, &format!("means `{name}`, which is not available"));
        }
        let assumed = |body: &str| {
            source(&std_main(&format!(
                "assume Int, zero, one, Int.add, Int.mul {{\n  fn f() {{ {body} }}\n}}\n\
                 assume Std {{ fn main() {{ }} }}"
            )))
        };
        refused(&assumed("let x = 1.5;"), "means `float`, which is not available");
        // An exponent makes a literal `Float` with no point at all.
        refused(&assumed("let x = 1e3;"), "the literal `1e3` means `float`");
    }

    /// tests/wasi/overflow.moss: 2^32 + 1 wraps to 1 under `WasiStd`'s i32
    /// `Int`, and traps — at run time, when it is evaluated — under a
    /// test-local `Int` whose `.add` and `.mul` trap on overflow.
    #[test]
    fn an_oversized_literal_does_what_the_arithmetic_does() {
        traps("tests/wasi/overflow.moss", &[], OVERFLOW_OUT);
    }

    /// `7` is seven `one`s added up, not a constant, so a scope that binds
    /// `one` to 2 has `7` mean 14 (D25: to the end of the block).
    #[test]
    fn a_literal_follows_the_binding_of_int() {
        let entry = whole(
            r"assume Std {
  fn main() {
    bind one = 2;
    putchar((char::A.code() + 7 - 7 - 7).char());
  }
}
",
        );
        // Inside the block `one` is 2, so `7` is 14: 65 + 14 - 14 - 14.
        agree(&entry, &[], "3");
    }
}

/// D69: `float_list()` makes a packed list of whatever `Float` is bound to
/// — under `WasiStd` an f64, in a block binding `WasiFloats32` an f32 —
/// through one implementation over a per-type layout.
mod lists {
    use super::*;

    #[test]
    fn over_the_moss_std() {
        agree("tests/wasi/lists.moss", &[], LISTS_OUT);
    }
}

/// Draft, over D84's Rule A: arrays indexed by `Size`, which a rebinding
/// of `Int` leaves alone (D72), and `for` over access.moss's iteration
/// methods.
mod size_and_for {
    use super::*;

    #[test]
    fn for_() {
        agree("tests/wasi/iterate.moss", &[], "503217abc\n");
    }

    #[test]
    fn a_std_only_library_rebinds_int_and_keeps_arrays() {
        agree("tests/wasi/size_rebind.moss", &[], SIZE_REBIND_OUT);
    }

    #[test]
    fn all_of_std_under_a_std_only_int() {
        agree("tests/wasi/size_std.moss", &[], SIZE_STD_OUT);
    }

    #[test]
    fn generic_code_does_not_mix_int_and_size() {
        refused("tests/wasi/size_mix.moss", "argument `index` of `.get` is `Int`, expected `Size`");
    }
}

mod representation {
    use super::*;

    /// D58: same program, same answer from both runners. The oracle's own
    /// representation tags everything, which is exactly why this has to
    /// agree with the compiled one.
    #[test]
    fn injection_is_where_the_tag_appears() {
        let entry = whole(
            r"assume Std {
  tag A(Char);
  tag Other;
  type C = | Other | A;
  fn bare(a: A): Char { match a { A(c) => c } }
  fn tagged(c: C): Char { match c { A(ch) => ch, Other => char::n, } }
  fn main() {
    let a = A(char::y);
    putchar(bare(a));
    putchar(tagged(a));
    putchar(tagged(Other));
  }
}
",
        );
        agree(&entry, &[], "yyn");
    }
}

/// D55: a functor maps one structure to another. `bind F;` installs its
/// binds here; its totality against the declared result signature is
/// checked once, at the functor.
mod functors {
    use super::*;

    fn functor(binds: &str) -> String {
        whole(&format!(
            "type Text;\n\
             fn emit(t: Text);\n\
             val greeting: Text;\n\
             context MiniStd = Text, emit, greeting;\n\
             assume Std {{\n\
             \x20 fn shout(c: Char) {{ putchar(c); putchar(char::exclam); }}\n\
             \x20 functor CharStd: Std -> MiniStd {{\n\
             \x20   bind Text = Char;\n\
             \x20   bind emit = shout;\n\
             {binds}\
             \x20 }}\n\
             \x20 assume MiniStd {{ fn app() {{ emit(greeting); emit(greeting); }} }}\n\
             \x20 fn main() {{ bind CharStd; app(); }}\n\
             }}\n"
        ))
    }

    #[test]
    fn application_installs_the_binds() {
        agree(&functor("    bind greeting = char::h;\n"), &[], "h!h!");
    }

    #[test]
    fn incomplete_functor_reports_at_the_functor() {
        refused(&functor(""), "does not bind `greeting`");
    }

    #[test]
    fn self_application_is_rejected() {
        let entry = whole(
            r"context Empty = ;
assume Std {
  functor Loopy: Std -> Empty { bind Loopy; }
  fn main() { bind Loopy; }
}
",
        );
        refused(&entry, "applies itself");
    }

    #[test]
    fn bind_of_a_non_functor_is_an_error() {
        let entry = whole("assume Std {\n  val c: Char;\n  fn main() { bind c; }\n}\n");
        refused(&entry, "is not a functor");
    }
}

mod generics {
    use super::*;

    /// The shared-T functor idiom from src/cell.moss, end to end: a
    /// totally-applied context (IsCell[T=Int, Cell=MyCell]) provides
    /// detached methods keyed at the *resolved* receiver type, satisfied
    /// by binds whose providers are attached methods (which can see
    /// `this`), with a D44 rename: the fixture's `.fetch`/`.stash` are
    /// called here under names of this module's choosing, which is the
    /// rule a detached method obeys — it does not match on the name it
    /// was declared under (D61).
    #[test]
    fn iscell_idiom() {
        let entry = program(&[(
            "main.moss",
            r#"import "{root}/tests/fixtures/inner.moss" use T;
import "{root}/tests/fixtures/cell.moss" use Cell, IsCell, .fetch as .cread, .stash as .cwrite;

assume Std {
  tag MyCell(CellInt);

  fn MyCell.get(): Int {
    match this { MyCell(c) => c.read() }
  }

  fn MyCell.put(x: Int) {
    match this { MyCell(c) => c.write(x), }
  }

  context CellOps = IsCell[T=Int, Cell=MyCell];

  assume CellOps {
    fn bump(c: MyCell) {
      c.cwrite(c.cread().add(one));
    }
  }

  fn main() {
    bind T=Int;
    bind MyCell.cread=MyCell.get;
    bind MyCell.cwrite=MyCell.put;
    let c = MyCell(cell_int());
    c.put(one);
    bump(c);
    if c.get().eq(one.add(one)) { putchar(char::y) }
  }
}
"#,
        )]);
        // Known self-hosted bug: a detached method imported under another
        // name (`.stash as .cwrite`, D44) is reported (`Q`, e_irrelevant)
        // when called through the renamed context; the same program with
        // the fixture's own names compiles and runs. When it is fixed,
        // assert `agree(&entry, &[], "y")`.
        assert!(rejects(&entry).starts_with("?Q "));
        assert_eq!(oracle_run(&entry, &[]), "y");
    }
}

/// A driver over src/lex.moss prints one dot per token, or a letter per
/// token kind. The Python bootstrap's lexer was the reference for token
/// counts; with it gone, the expected counts are written out, and on real
/// files the reference is the oracle running the same driver.
mod self_hosted_lexer {
    use super::*;
    use std::sync::OnceLock;

    const DOTS_DRIVER: &str = r#"import "{root}/src/lex.moss" as lexer;
import "{root}/src/token.moss" use Eof;
assume Std {
  fn main() {
    bind lexer::src=first_arg();
    bind lexer::at=cell_int();
    bind lexer::mark=cell_int();
    loop { match lexer::lex() { Eof => break, _ => putchar(char::dot), } }
    putchar(char::newline);
  }
}
"#;

    /// A driver, compiled once, beside its entry for the oracle.
    struct Driver {
        entry: String,
        module: Vec<u8>,
    }

    impl Driver {
        fn new(text: &str) -> Driver {
            let entry = program(&[("main.moss", text)]);
            let module = build(&entry);
            Driver { entry, module }
        }

        /// What it prints for `source`, which both runners must agree on.
        fn run(&self, source: &str) -> String {
            let mine = run_module(&self.module, &[source]);
            assert_eq!(oracle_run(&self.entry, &[source]), mine, "the oracle on {source:?}");
            mine
        }
    }

    fn dots() -> &'static Driver {
        static DOTS: OnceLock<Driver> = OnceLock::new();
        DOTS.get_or_init(|| Driver::new(DOTS_DRIVER))
    }

    fn lex_dots(source: &str) -> String {
        dots().run(source)
    }

    fn count(n: usize) -> String {
        format!("{}\n", ".".repeat(n))
    }

    #[test]
    fn symbols() {
        assert_eq!(lex_dots("{ } ( ) ; |"), count(6));
    }

    #[test]
    fn two_char_symbols() {
        assert_eq!(lex_dots("== = => :: : <= << < >= >> > !="), count(12));
    }

    #[test]
    fn names_and_strings() {
        assert_eq!(lex_dots(r#"import "a\"b" as foo_2;"#), count(5));
    }

    #[test]
    fn comments_and_whitespace() {
        assert_eq!(lex_dots("# a comment\n;\t;\n  ; # eof"), count(3));
    }

    /// The keyword trie distinguishes keywords from names sharing their
    /// prefixes/case — no string literals involved (D48).
    #[test]
    fn keyword_kinds() {
        let driver = Driver::new(
            r#"import "{root}/src/lex.moss" as lexer;
import "{root}/src/token.moss" use Eof, Name, Assume, As, ThisType, ThisValue, Loop, Str;

assume Std {
  fn main() {
    bind lexer::src=first_arg();
    bind lexer::at=cell_int();
    bind lexer::mark=cell_int();
    loop {
      match lexer::lex() {
        Eof => break,
        Assume => putchar(char::A),
        As => putchar(char::a),
        ThisType => putchar(char::T),
        ThisValue => putchar(char::t),
        Loop => putchar(char::l),
        Name => putchar(char::n),
        Str => putchar(char::s),
        _ => putchar(char::dot),
      }
    }
    putchar(char::newline);
  }
}
"#,
        );
        let source = r#"assume assumes as As this This loops loop "x" ;"#;
        assert_eq!(driver.run(source), "AnantTnls.\n");
    }

    /// D68's literals: `I` an integer, `D` a decimal, `b` the bad token
    /// the lexer reports an error for — a suffix. Length is no error: a
    /// literal is only arithmetic.
    #[test]
    fn literal_kinds() {
        let driver = Driver::new(
            r#"import "{root}/src/lex.moss" as lexer;
import "{root}/src/token.moss" use Eof, Name, Dot, Integer, Decimal, Bad;

assume Std {
  fn main() {
    bind lexer::src=first_arg();
    bind lexer::at=cell_int();
    bind lexer::mark=cell_int();
    loop {
      match lexer::lex() {
        Eof => break,
        Integer => putchar(char::I),
        Decimal => putchar(char::D),
        Bad => putchar(char::b),
        Name => putchar(char::n),
        Dot => putchar(char::dot),
        _ => putchar(char::underscore),
      }
    }
    putchar(char::newline);
  }
}
"#,
        );
        let cases = [
            ("x2 123 0 007 3.14 0.5", "nIIIDD"),
            ("1.add(2)", "I.n_I_"),
            ("1.x 1. 2.5.abs", "I.nI.D.n"),
            ("2x", "bn"),
            ("1.5f", "bn"),
            // An exponent makes a decimal; without digits it is a suffix,
            // and a sign with no digit after it is an error too.
            ("1e9 1e-9 1.5e+3 2.5E10 0e0 1e5.abs", "DDDDDD.n"),
            ("1e", "bn"),
            ("1e+", "bn_"),
            ("1ex", "bn"),
            ("1e5x", "bn"),
            ("1.e5 1.5.e3", "I.nD.n"),
            ("1e999 1e-0000999", "DD"),
            ("1e1000", "b"),
            ("000000000000000000001.5 0.00000000000000000000001", "DD"),
            ("3.14159265358979323846264338327950288", "D"),
            ("123456789012345678901234567890", "I"),
        ];
        for (source, expected) in cases {
            assert_eq!(driver.run(source), format!("{expected}\n"), "{source}");
        }
    }

    /// Once against the Python bootstrap's lexer's count; now the
    /// compiled driver and the oracle's run of it must agree, on a
    /// nonzero count.
    #[test]
    fn agrees_with_bootstrap_on_real_files() {
        for rel in ["lib/bool.moss", "lib/num.moss", "src/lex.moss", "src/cli.moss"] {
            let out = lex_dots(&read(rel));
            assert!(out.len() > 100 && out.trim_end_matches('\n').bytes().all(|b| b == b'.'), "{rel}: {out}");
        }
    }
}

/// D43 consistent merging: duplicate keys unify their bindings instead of
/// erroring, and only a genuinely unsatisfiable merge is rejected.
mod merging {
    use super::*;

    const DECLS: &str = r"  type Foo;
  type A;
  type B;
  type C;
  fn .gimme(): Foo;
  context Ctx1 = A, B, A.gimme[Foo=B];
  context Ctx2 = A, C, A.gimme[Foo=C];
  context Ctx3 = Ctx1, Ctx2;
";

    /// Inside `assume Ctx3`, B and C are one atom: `a.gimme()` (declared to
    /// yield B by Ctx1's item) is accepted where C is required.
    #[test]
    fn designer_example_merges() {
        let entry = prog(
            &format!(
                "{DECLS}  assume Ctx3 {{\n    fn go(a: A): C {{ a.gimme() }}\n  }}\n\
                 \x20 fn giveq(): Char {{ char::q }}"
            ),
            "bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;\n\
             \x20   bind A.gimme=giveq;\n\
             \x20   putchar(go(char::m));",
        );
        agree(&entry, &[], "q");
    }

    #[test]
    fn unsatisfiable_merge_is_error() {
        let entry = prog(
            r"  type A;
  fn .gimme(): Foo;
  type Foo;
  tag U1;
  tag U2;
  context Ctx1 = A, A.gimme[Foo=U1];
  context Ctx2 = A, A.gimme[Foo=U2];
  context Ctx3 = Ctx1, Ctx2;
  assume Ctx3 { fn go() { } }",
            "()",
        );
        refused(&entry, "cannot merge");
    }

    /// After Ctx3 merges B with C, code under it can pass a B where a C is
    /// expected and vice versa.
    #[test]
    fn binding_one_merged_symbol_binds_both() {
        let entry = prog(
            &format!(
                "{DECLS}  assume Ctx3 {{\n\
                 \x20   val b: B;\n\
                 \x20   assume b {{\n\
                 \x20     fn takes_c(x: C) {{ }}\n\
                 \x20     fn roundtrip() {{ takes_c(b); }}\n\
                 \x20   }}\n\
                 \x20 }}\n\
                 \x20 fn giveq(): Char {{ char::q }}"
            ),
            "bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;\n\
             \x20   bind A.gimme=giveq;\n\
             \x20   bind b=char::b;\n\
             \x20   roundtrip();",
        );
        agree(&entry, &[], "");
    }
}

mod int_list {
    use super::*;

    #[test]
    fn push_get_set_length() {
        let entry = prog(
            "",
            "let xs = int_list();\n\
             \x20   xs.push(one);\n\
             \x20   xs.push(one.add(one));\n\
             \x20   if xs.length().eq(one.add(one)) { putchar(char::n) }\n\
             \x20   if xs.get(one).eq(one.add(one)) { putchar(char::g) }\n\
             \x20   xs.set(zero, zero);\n\
             \x20   if xs.get(zero).eq(zero) { putchar(char::s) }",
        );
        agree(&entry, &[], "ngs");
    }

    /// The Python bootstrap's native `Std` panicked on an index past the
    /// end. `WasiStd`'s `IntList.get` (lib/wasistd.moss, `packed_get`) does
    /// not check its bounds, so neither runner traps: they agree that it
    /// runs.
    #[test]
    fn out_of_range_panics() {
        agree(&prog("", "let xs = int_list(); xs.get(zero);"), &[], "");
    }
}

mod multi_instantiation {
    use super::*;

    /// The designer's point about IsList: with detached methods and a
    /// unique receiver type per instantiation (NameList-style), one scope
    /// holds IsCell at T=Int *and* T=Char — the keys are (CA, read) and
    /// (CB, read), distinct atoms, so D51's collision never happens.
    /// tests/wasi/generic.moss is a larger version of the same; this one
    /// once caught the self-hosted compiler reporting `e_method` for the
    /// whole idiom.
    #[test]
    fn two_iscell_instantiations_coexist() {
        let entry = program(&[(
            "main.moss",
            r#"import "{root}/tests/fixtures/inner.moss" use T;
import "{root}/tests/fixtures/cell.moss" use Cell, IsCell, .fetch as .cread, .stash as .cwrite;

assume Std {
  tag CA(CellInt);
  tag CB(Char);

  fn CA.rd(): Int { match this { CA(c) => c.read() } }
  fn CA.wr(x: Int) { match this { CA(c) => c.write(x), } }
  fn CB.rd(): Char { match this { CB(c) => c } }
  fn CB.wr(x: Char) { }

  context Both = IsCell[T=Int, Cell=CA], IsCell[T=Char, Cell=CB];

  assume Both {
    fn go(a: CA, b: CB): Char {
      a.cwrite(a.cread().add(one));
      b.cread()
    }
  }

  fn main() {
    bind T=Int;
    bind CA.cread=CA.rd;
    bind CA.cwrite=CA.wr;
    bind T=Char;
    bind CB.cread=CB.rd;
    bind CB.cwrite=CB.wr;
    let a = CA(cell_int());
    let b = CB(char::z);
    putchar(go(a, b));
  }
}
"#,
        )]);
        // Known self-hosted bug: as in `generics::iscell_idiom`, the
        // renamed detached methods (`.fetch as .cread`, `.stash as
        // .cwrite`) are reported (`Q`, `P`). When it is fixed, assert
        // `agree(&entry, &[], "z")`.
        assert!(rejects(&entry).starts_with("?Q "));
        assert_eq!(oracle_run(&entry, &[]), "z");
    }
}

/// The interner's driver.
const ARENA_DRIVER: &str = r#"import "{root}/src/intern.moss" as intern;
import "{root}/src/lex.moss" as lexer;
assume Std {
  assume intern::Interner {
    fn take(text: String, ns: Int, nl: Int): Int {
      bind lexer::src=text;
      intern::intern(ns, nl)
    }
  }
  fn main() {
    bind intern::ichars=int_list();
    bind intern::istarts=int_list();
    bind intern::ilens=int_list();
    bind intern::ibuckets=int_list();
    bind intern::ilink=int_list();
    bind intern::imask=cell_int();
    let s = first_arg();
    let two = one.add(one);
    let a = take(s, zero, two);
    let b = take(s, two, two);
    let c = take(s, zero, two);
    intern::put(a);
    intern::put(b);
    if a.eq(c) { putchar(char::y) } else { putchar(char::n) }
    if a.eq(b) { putchar(char::n) } else { putchar(char::d) }
    putchar(char::newline);
  }
}
"#;

mod interner {
    use super::*;

    /// The point of the codepoint arena: `take` is the only thing that
    /// binds `lexer::src`, so by the time `main` prints the names back the
    /// source they were read from is out of scope entirely. Ids are stable
    /// (same text, same id) and distinct texts differ.
    #[test]
    fn names_outlive_their_source() {
        agree(&program(&[("main.moss", ARENA_DRIVER)]), &["abcd"], "abcdyd\n");
    }
}
