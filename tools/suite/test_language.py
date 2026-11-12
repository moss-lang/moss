from suite.support import (
    ORACLE_REJECTED,
    ORACLE_UNSUPPORTED,
    TRAP,
    build,
    build_with,
    compile_source,
    moss_files,
    once,
    oracle,
    oracle_command,
    oracle_rejects,
    oracle_run,
    read,
    rejects,
    rejects_with,
    run,
    run_module,
    run_module_code,
    source,
    sources,
    std_main,
    std_program,
)

# The language, program by program: each test is a small program (or one
# of `tests/wasi`) that the self-hosted compiler must compile and run to
# the expected output, or refuse, and that the oracle must run to the
# same output, or reject with the Python bootstrap's message.
#
# Ported from the Python bootstrap's `tests/test_run.py`, whose programs
# ran under the interpreter's native `Std`. A program whose `main`
# assumes `Std` is not one the platform runs (D52), so each is wrapped
# here: `std_program`/`std_main` make its `main` a `run` that the
# platform's `main` calls under `WasiStd`.
# Python's `main_body(body, decls)`: `assume Std { decls fn main() {
# body } }`, as a program the platform runs.


def prog(decls, body):
    return source(std_program(decls, body))


# A whole program whose `main` assumes `Std`, as one the platform runs.


def whole(text):
    return source(std_main(text))


# Several files, `main.moss` among them, whose `main` assumes `Std`;
# returns the path of `main.moss`.
def program(files):
    return (
        sources([(name, std_main(text) if name == "main.moss" else text) for name, text in files])
        + "/main.moss"
    )


# Both runners print `expected`: the self-hosted compiler's module under
# Wasmtime, and the oracle.


def agree(entry, args, expected):
    assert run(entry, args) == expected, f"the self-hosted compiler on {entry}"
    assert oracle_run(entry, args) == expected, f"the oracle on {entry}"


# Both refuse `entry`: the self-hosted compiler writes a report, and the
# oracle rejects it with a message containing `message`. Returns the
# self-hosted compiler's report and the oracle's message.


def refused(entry, message):
    report = rejects(entry)
    theirs = oracle_rejects(entry)
    assert message in theirs, f"the oracle on {entry}: {theirs}"
    return (report, theirs)


# Both trap (Python's `MossPanic`), having printed `printed` first.


def traps(entry, args, printed):
    assert run_module_code(build(entry), args, TRAP) == printed, (
        f"the self-hosted compiler on {entry}"
    )
    out = oracle(entry, args)
    assert out.code == TRAP, f"the oracle on {entry}: {out.stderr}"
    assert out.text() == printed, f"the oracle on {entry}"


# What tests/wasi/shadow.moss prints: f64, f32, f64, f32, f64.

SHADOW_OUT = "333333333\n333333344\n333333333\n333333344\n333333333\n"

# What tests/wasi/ops.moss prints: D68's operators on `Int` and `Bool`.

OPS_OUT = """19 -7 3 1 -7 2 23 35 21 5 1 -1\x20
ynynnyny y
2 15 13 1024 128 -5\x20
nnynyyyyn
"""

# What tests/wasi/exponents.moss prints: the same report at f64, then f32.
# `1e39` is inf at f32 and `1e-46` zero.

EXPONENTS_OUT = """yyyyyyyyy
1500 125 1 250 -42 100000 602\x20
n nn yy
yyyyyyyyy
1500 125 1 250 -42 100000 602\x20
n yy yy
"""

# What tests/wasi/literals.moss prints: the same report at f64, then f32.
# A third to twenty-one places is not f32's nearest third: its digits were
# summed in f32 and rounded on the way (D68).

LITERALS_OUT = """0 7 10 100 1000 1234567 2147483647 -42 3 0\x20
3 -2 1 0 250 1 16777215\x20
3141593 333333333 ny
314159265 333333333\x20
0 7 10 100 1000 1234567 2147483647 -42 3 0\x20
3 -2 1 0 250 1 16777215\x20
3141593 333333344 yn
314159264 333333248\x20
"""

# What tests/wasi/lists.moss prints: an IntList beside a FloatList at f64,
# then the same at f32. A thousand 0.1s sum to 100 at f64 and to 99.999048
# at f32; a third differs in the eighth digit.

LISTS_OUT = """1000 1000 100000000 332833500\x20
333333333 1 1 -7 251001\x20
1000 1000 99999048 332833500\x20
333333344 1 1 -7 251001\x20
"""

# What tests/wasi/overflow.moss prints before it traps: an oversized
# literal under wrapping i32 arithmetic, then one that fits, under it and
# under arithmetic that traps on overflow.

OVERFLOW_OUT = "1\n2147483647\n2147483647\n"

SIZE_REBIND_OUT = "1705032704\n-294967296\n-2\n"

SIZE_STD_OUT = "-294967296\n4\n2147483647\n4\n"

# `test_run.TestHello`.


def test_hello_world():
    agree("examples/hello.moss", [], "Hello, world!\n")


# `test_run.TestExamples` is `examples.rs`.
# A name nobody declared is a *scope* error.


def test_errors__not_in_scope():
    refused(prog("", "foo();"), "not in scope")


# A declared but unassumed function is a *context* error.


def test_errors__not_in_context():
    refused(source("fn example();\nfn main() { example(); }"), "not available in the context")


# `main` may assume only what the platform
# provides (D39, D52). The self-hosted compiler refuses it; the
# oracle runs `main` in the platform's root context and reports
# anything beyond it unsupported rather than judging it (as for
# `tests/errors/beyond_std.moss`).


def test_errors__assuming_more_than_std():
    entry = source("fn example();\nassume example { fn main() { example(); } }")
    rejects(entry)
    out = oracle(entry, [])
    assert out.code == ORACLE_UNSUPPORTED, f"{out.stderr}"


def test_context__needs_pass_through():
    entry = prog("  fn twice() { putchar(char::a); putchar(char::a); }", "twice();")
    agree(entry, [], "aa")


def test_context__bind_val_lexical():
    entry = prog(
        "  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
        "bind c=char::x; emit(); bind c=char::y; emit();",
    )
    agree(entry, [], "xy")


# A bind inside an if-block must not leak out of it.


def test_context__bind_scoped_to_block():
    entry = prog(
        "  val c: Char;\n  assume c { fn emit() { putchar(c); } }",
        "bind c=char::x; if true { bind c=char::y; emit(); } emit();",
    )
    agree(entry, [], "yx")


# D65: a bare block scopes exactly as an if-block does. Its `let`
# shadows the outer one at another width — a record over a char —
# and the outer one is back afterwards.


def test_context__bare_block_scopes_binds_and_lets():
    entry = prog(
        """  val c: Char;
  tag Pair { a: Char, b: Char };
  assume c { fn emit() { putchar(c); } fn got(): Char { c } }""",
        "bind c=char::x; let p = char::p; { bind c=char::y; let p = Pair { a = char::q, b = char::r }; emit(); putchar(p.b); } emit(); putchar(p); putchar({ bind c=char::z; let t = got(); t });",
    )
    agree(entry, [], "yrxpz")


# D66: `bind f = g;` with `g` abstract captures `g` as provided at the
# bind: neither a rebind of `g` after it nor one deeper in the call
# tree reaches `f`. The signature mentions an abstract type.


def test_context__fn_bind_to_contextual_fn_captures():
    entry = prog(
        """  type T;
  val c: Char;
  assume T { fn g(x: T); fn f(x: T); }
  assume c { fn emit(x: Char) { putchar(c); putchar(x); } }
  fn other(x: Char) { putchar(char::hyphen); }
  assume T, f, g { fn deeper(x: T) { bind g = loud; f(x); } }
  assume T, g { fn loud(x: T) { g(x); g(x); } }""",
        "bind T = Char; bind c = char::a; bind g = emit; bind f = g; bind c = char::b; f(char::x); bind g = other; f(char::y); g(char::z); deeper(char::w);",
    )
    agree(entry, [], "axay-aw")


def test_context__fn_bind_to_unavailable_contextual_fn():
    entry = prog("  fn g(x: Char);\n  fn f(x: Char);", "bind f = g;")
    refused(entry, "`g` is not available in the context here")


def test_context__fn_bind_to_contextual_fn_checks_the_signature():
    entry = prog("  fn f(x: Int);", "bind f = putchar;")
    # Known self-hosted bug: it does not check a fn bind's signature
    # against the provider's — `putchar` takes a `Char`, `f` an `Int`
    # — and compiles this, and even a program that calls `f`. When it
    # is fixed, assert `rejects`.
    assert compile_source(entry, "lib/prelude.moss").is_module()
    theirs = oracle_rejects(entry)
    assert (
        "fn bind signature mismatch: provider `putchar` has (Char) -> (), need (Int) -> ()"
        in theirs
    ), f"{theirs}"


def test_context__unbound_val_is_context_error():
    entry = prog("  val c: Char;\n  assume c { fn emit() { putchar(c); } }", "emit();")
    refused(entry, "not available in the context")


def test_context__bind_fn_across_modules():
    entry = program(
        [
            (
                "lexer.moss",
                """type In;
assume In {
  fn next(): In;
  assume next { fn go(): In { next() } }
}""",
            ),
            (
                "main.moss",
                """import "./lexer.moss" as lexer;
assume Std {
  fn mine(): Char { char::z }
  fn main() {
    bind lexer::In=Char;
    bind lexer::next=mine;
    putchar(lexer::go());
  }
}""",
            ),
        ]
    )
    agree(entry, [], "z")


# D27: `bind lexer::next=mine` does not infer `bind lexer::In=Char`.


def test_context__fn_bind_without_type_bind_is_error():
    def entry(call):
        return program(
            [
                (
                    "lexer.moss",
                    """type In;
assume In {
  fn next(): In;
  assume next { fn go(): In { next() } }
}""",
                ),
                (
                    "main.moss",
                    f"""import "./lexer.moss" as lexer;
assume Std {{
  fn mine(): Char {{ char::z }}
  fn main() {{ bind lexer::next=mine; {call} }}
}}""",
                ),
            ]
        )

    message = "type `In` is not available in the context here"
    # Known self-hosted bug: it only checks a fn bind whose provision
    # is used, so it compiles the bind alone (the Python test's
    # program). When it is fixed, assert `rejects` here too.
    unused = entry("")
    assert compile_source(unused, "lib/prelude.moss").is_module()
    assert message in oracle_rejects(unused)
    # With a call that reaches `next`, both refuse it.
    refused(entry("lexer::go();"), message)


# The provider's own needs are captured at the bind site, so the
# callee can run it without assuming them (the cli.moss pattern).


def test_context__fn_bind_captures_context():
    entry = program(
        [
            ("runner.moss", "fn task();\nassume task { fn go() { task(); } }"),
            (
                "main.moss",
                """import "./runner.moss" as runner;
assume Std {
  val c: Char;
  assume c { fn emit() { putchar(c); } }
  fn main() {
    bind c=char::k;
    bind runner::task=emit;
    runner::go();
  }
}""",
            ),
        ]
    )
    agree(entry, [], "k")


# Both runners agree on a union of records of different widths, and
# on a pattern binder being scoped to its arm: the outer `x` is back
# after the match.


def test_context__record_payloads_and_binder_scope():
    entry = whole("""assume Std {
  tag Lit(Char);
  tag Both { l: Char, r: Char };
  type Node = Lit | Both;
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
""")
    agree(entry, [], "aced\n")


# D25: a later bind of the same symbol in an inner block shadows.
# `WasiFloats32` inside a scope with an f64 `Float` rebinds `Float`
# and then its ops, whose entries in force were checked against the
# old `Float`; they are replaced, not merged with (D43 is for the
# items of contexts being assumed).


def test_context__a_bind_shadows_a_bind_whose_type_was_rebound():
    agree("tests/wasi/shadow.moss", [], SHADOW_OUT)


# D26: a bound provider's needs are captured at the bind site, so a
# frame between the bind and the call that rebinds them changes
# nothing — directly (`relay`), a frame further down (`deep`), or
# through a provider that itself captured a fn bind (`wrap`, whose
# `task` is still `emit` with `c=h`). Even in the bind's own scope, a
# later rebind of `c` leaves the earlier `bind task=emit` alone.
# Every letter is `h`; any other names the rebind that leaked through.


def test_context__fn_bind_capture_survives_rebinds():
    agree(whole(CAPTURE_REBOUND), [], "hhhh\n")


# D26's cases for `context::fn_bind_capture_survives_rebinds`.

CAPTURE_REBOUND = """assume Std {
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
"""


def test_methods__attached_defined():
    entry = prog(
        """  tag Wrap(Char);
  fn Wrap.get(): Char { match this { Wrap(c) => c } }""",
        "let w = Wrap(char::q); putchar(w.get());",
    )
    agree(entry, [], "q")


def test_methods__detached_provided():
    entry = prog(
        """  type A;
  fn .gimme(): Char;
  val a: A;
  context Ctx = A, a, A.gimme;
  assume Ctx { fn go(): Char { a.gimme() } }
  fn giveq(): Char { char::q }""",
        """bind A=Char;
    bind a=char::m;
    bind A.gimme=giveq;
    putchar(go());""",
    )
    agree(entry, [], "q")


# The other half of D61: an attached method matches *by name* in the
# receiver's own module and is never imported — including one that
# is only declared there and provided from the context, which the
# detached rule (import it, or `mod::m`) does not cover.


def test_methods__attached_declared_beside_its_receiver_needs_no_import():
    entry = prog(
        """  tag Wrap(Char);
  fn Wrap.spell(): Char;
  fn giveq(): Char { char::q }
  context Spelling = Wrap.spell;
""",
        """bind Wrap.spell=giveq;
    let w = Wrap(char::z);
    putchar(w.spell());""",
    )
    agree(entry, [], "q")


# Q5: attached methods require nominal receivers.


def test_methods__attached_on_abstract_rejected():
    entry = prog("  type A;\n  fn A.m(): Char { char::q }", "()")
    # Known self-hosted bug (Q5, as for
    # tests/errors/attached_on_abstract.moss): the self-hosted
    # compiler does not check that an attached method's receiver is
    # nominal, and compiles this. When it is fixed, assert `rejects`.
    assert compile_source(entry, "lib/prelude.moss").is_module()
    assert "nominal receiver" in oracle_rejects(entry)


def test_matching__bool_and_if():
    entry = prog(
        "  fn yes(): Bool { true }", "if yes() { putchar(char::y) } else { putchar(char::n) }"
    )
    agree(entry, [], "y")


def test_matching__bool_branch_and_not():
    entry = prog(
        "  fn pick(b: Bool): Char { if b { char::t } else { char::f } }",
        "putchar(pick(false)); putchar(pick(true)); putchar(pick(true.not()));",
    )
    agree(entry, [], "ftf")


def test_matching__non_exhaustive():
    entry = prog(
        """  tag A;
  tag B;
  type AB = A | B;
  fn pick(x: AB): Char { match x { A => char::t, } }""",
        "()",
    )
    refused(entry, "not exhaustive")


def test_matching__tag_and_record():
    entry = prog(
        """  tag Pair { x: Char, y: Char };
  fn first(p: Pair): Char { match p { Pair { x } => x } }""",
        "putchar(first(Pair { x = char::a, y = char::b }));",
    )
    agree(entry, [], "a")


def test_matching__union_injection():
    entry = prog(
        """  tag Stop;
  tag Step(Char);
  type Out = Stop | Step;
  fn go(b: Bool): Out { if b { Step(char::s) } else { Stop } }
  fn show(o: Out): Char { match o { Step(c) => c, Stop => char::dot, } }""",
        "putchar(show(go(true))); putchar(show(go(false)));",
    )
    agree(entry, [], "s.")


def test_matching__while_loop_break():
    entry = prog(
        "",
        """var again = true;
    while again { putchar(char::w); again = false; }
    loop { putchar(char::l); break; }""",
    )
    agree(entry, [], "wl")


def test_matching__recursion():
    entry = prog(
        "  fn rec(b: Bool): Char { if b { rec(false) } else { char::r } }", "putchar(rec(true));"
    )
    agree(entry, [], "r")


def test_std_lib__int_methods_and_while():
    entry = prog(
        "",
        """var i: Int = zero;
    let stop = one.add(one).add(one);
    while i.lt(stop) { putchar(char::i); i = i.add(one); }""",
    )
    agree(entry, [], "iii")


def test_std_lib__char_eq():
    entry = prog(
        "",
        """if char::a.eq(char::a) { putchar(char::y) }
    if char::a.eq(char::b) { putchar(char::n) }""",
    )
    agree(entry, [], "y")


# `Char.code` is what lets the self-hosted interner store names as
# codepoints instead of spans into a single source buffer.


def test_std_lib__char_code():
    entry = prog(
        "",
        """let n = char::c.code().sub(char::a.code());
    var i = zero;
    while i.lt(n) { putchar(char::dot); i = i.add(one); }
    if char::A.code().lt(char::a.code()) { putchar(char::u) }""",
    )
    agree(entry, [], "..u")


def test_std_lib__cell():
    entry = prog(
        "",
        """let c = cell_int();
    c.write(one);
    if c.read().eq(one) { putchar(char::c) }""",
    )
    agree(entry, [], "c")


def test_std_lib__string_arg():
    entry = prog(
        "",
        """let s = first_arg();
    print(s);
    putchar(s.get(zero));
    if s.length().eq(one.add(one)) { putchar(char::exclam) }""",
    )
    agree(entry, ["ok"], "oko!")


def test_std_lib__string_slice():
    entry = prog(
        "",
        """let s = first_arg();
    let two = one.add(one);
    print(s.slice(one, two));
    print(s.slice(zero, s.length()));
    let t = s.slice(two, two);
    if t.length().eq(two) { putchar(char::y) } else { putchar(char::n) }
    putchar(t.get(zero));""",
    )
    agree(entry, ["abcdef"], "bcabcdefyc")


def test_std_lib__string_concat():
    entry = prog(
        "",
        """let s = first_arg();
    print(s.concat(s));
    putchar(char::comma);
    print(s.slice(zero, one).concat(s.slice(one, one)));
    putchar(char::comma);
    print(s.concat(s.slice(zero, zero)));""",
    )
    agree(entry, ["abc"], "abcabc,ab,abc")


# `StrList` is `IntList`'s shape at `String` elements — a distinct
# nominal type (D51).


def test_std_lib__str_list():
    entry = prog(
        "",
        """let xs = str_list();
    let s = first_arg();
    xs.push(s.slice(zero, one));
    xs.push(s);
    var i = zero;
    while i.lt(xs.length()) { print(xs.get(i)); putchar(char::comma); i = i.add(one); }""",
    )
    agree(entry, ["abc"], "a,abc,")


# The GC string provider checks slice bounds in both runners.


def test_std_lib__string_slice_out_of_range_panics():
    entry = prog("", "let s = first_arg().slice(zero, one.add(one));")
    traps(entry, ["x"], "")


def test_std_lib__division_by_zero_panics():
    traps(prog("", "let x = one.div(zero);"), [], "")


# The whole of `Std` is Moss over the primitive context — a linear
# memory, the instructions over it, and the WASI calls — and both
# runners run that same Moss: the oracle interprets it and the
# compiler compiles it.


def test_std_over_wasi__the_interpreter_runs_the_moss_std():
    text = read("lib/bool.moss")
    expected = f"{text[: (len(text) - 1)]} yy\n"
    agree("tests/wasi/full.moss", ["lib/bool.moss"], expected)


# D68: an operator is a call of one fixed method of lib/num.moss,
# provided through the context like any method.


def test_operators__over_the_moss_std():
    agree("tests/wasi/ops.moss", [], OPS_OUT)


def test_operators__exponents_over_the_moss_std():
    agree("tests/wasi/exponents.moss", [], EXPONENTS_OUT)


# An exponent `N` over `k` fractional digits divides `m` by
# `10^(k-N)` when that is positive — so `1e-9` is exactly
# `0.000000001`, and a decimal literal is the case `N` = 0, unchanged
# — multiplies it by `10^(N-k)` when that is, and is `float(m)` alone
# between; each power a product of `float(ten)`s, left to right. The
# self-hosted compiler writes the same module for each pair.


def test_operators__exponent_constructions():
    def module(expr):
        return build(prog("", f"let x = {expr}; putchar(char::A);"))

    # The ten of a power is ten `one`s added up (`emit_small` in
    # src/codegen.moss), where the literal `10` is `one * ten`: the
    # same value, but not the same module, so it is spelled out.
    ten = "float(1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1)"

    def tens(n):
        return "(" + " * ".join([ten] * n) + ")"

    cases = [
        ("1e-9", "0.000000001"),
        ("1.5e0", "1.5"),
        ("2.5E-3", "0.0025"),
        ("1e-9", f"float(1) / {tens(9)}"),
        ("2.5e10", f"(float(2) * {ten} + float(5)) * {tens(9)}"),
        ("1.5e+3", f"(float(1) * {ten} + float(5)) * {tens(2)}"),
        ("1.5e1", f"float(1) * {ten} + float(5)"),
        ("1e0", "float(1)"),
        ("0e5", f"float(0) * {tens(5)}"),
    ]
    for literal, same in cases:
        assert module(literal) == module(same), f"{literal} is not {same}"


# Once over the Python bootstrap's native `Std`; now over `WasiStd`.


def test_operators__over_the_native_std():
    entry = prog(
        "",
        "let two = one + one; let six = two * (two + one);putchar((six * six / two - six % (two + two) + char::A.code()).char());if !(six < two) & (six >= six) & !false { putchar(char::y); }if (two << two) - (six >> one) == two + two + one { putchar(char::y); }if (-two < zero) ^ true { putchar(char::n); } else { putchar(char::y); }",
    )
    # 36 / 2 - 6 % 4 = 16, so `Q`.
    agree(entry, [], "Qyyy")


def test_operators__an_operator_the_context_lacks_is_the_method_call_error():
    def reports(body):
        entry = prog("", f"let c = char::a; putchar({body});")
        mine, theirs = refused(entry, "no method `.add` is available for `Char`")
        return mine.replace(entry, "main.moss"), theirs.replace(entry, "main.moss")

    op, call = reports("c + c"), reports("c.add(c)")
    assert op[0] == call[0], "the self-hosted compiler's reports"
    assert op[1] == call[1], "the oracle's messages"


# `+` is num.moss's `.add` even where no `.add` is in scope: a file
# beside the library gets no prelude, and this one imports no
# detached `.add` — yet its `+` is provided by `Std` as usual. The
# library is copied to a scratch directory so that the file can sit
# beside it, with that copy's prelude given to both runners.
def test_operators__the_symbol_is_fixed_not_looked_up_by_name():
    plus = """import "./std.moss" use Std, putchar;
import "./int.moss" use one;
import "./char.moss" as char use .char, .code;
assume Std {
  fn main() {
    let two = one + one;
    putchar((two + char::A.code()).char());
  }
}
"""

    def library(text):
        files = [(name, read(name)) for name in moss_files("lib")]
        files.append(("lib/plus.moss", std_main(text)))
        directory = sources(files)
        return f"{directory}/lib/plus.moss", f"{directory}/lib/prelude.moss"

    entry, prelude = library(plus)
    assert run_module(build_with(entry, prelude), []) == "C"
    out = oracle_command(["--prelude", prelude, entry])
    assert (out.code, out.text()) == (0, "C"), out.stderr

    entry, prelude = library(plus.replace("one + one", "one.add(one)"))
    rejects_with(entry, prelude)
    out = oracle_command(["--prelude", prelude, entry])
    assert out.code == ORACLE_REJECTED, out.stderr
    assert "`.add` is not a detached method in scope" in out.stderr, out.stderr


# D68: a literal is a use of fixed library symbols — `zero`, `one`,
# `.add`, `.mul` for an integer, and `float` and `.div` too for a decimal
# — so it means whatever `Int` and `Float` are bound to.


def test_literals__over_the_moss_std():
    agree("tests/wasi/literals.moss", [], LITERALS_OUT)


# Once over the Python bootstrap's native `Std`; now over `WasiStd`.


def test_literals__over_the_native_std():
    entry = prog(
        "",
        "putchar((65 + 1.add(1) * 3 - 0).char());putchar((2.5 * 4.0).round().add(char::A.code()).char());if 0.1 + 0.2 != 0.3 { putchar(char::y); }putchar((-(-66)).char());",
    )
    agree(entry, [], "GKyB")


def test_literals__a_literal_needs_its_symbols_in_context():
    for body, name in [("let n = 0;", "zero"), ("let n = 12;", "one")]:
        entry = source(f"fn main() {{ {body} }}")
        refused(entry, f"means `{name}`, which is not available")

    def assumed(body):
        return source(
            std_main(f"""assume Int, zero, one, Int.add, Int.mul {{
  fn f() {{ {body} }}
}}
assume Std {{ fn main() {{ }} }}""")
        )

    refused(assumed("let x = 1.5;"), "means `float`, which is not available")
    # An exponent makes a literal `Float` with no point at all.
    refused(assumed("let x = 1e3;"), "the literal `1e3` means `float`")


# tests/wasi/overflow.moss: 2^32 + 1 wraps to 1 under `WasiStd`'s i32
# `Int`, and traps — at run time, when it is evaluated — under a
# test-local `Int` whose `.add` and `.mul` trap on overflow.


def test_literals__an_oversized_literal_does_what_the_arithmetic_does():
    traps("tests/wasi/overflow.moss", [], OVERFLOW_OUT)


# `7` is seven `one`s added up, not a constant, so a scope that binds
# `one` to 2 has `7` mean 14 (D25: to the end of the block).


def test_literals__a_literal_follows_the_binding_of_int():
    entry = whole("""assume Std {
  fn main() {
    bind one = 2;
    putchar((char::A.code() + 7 - 7 - 7).char());
  }
}
""")
    # Inside the block `one` is 2, so `7` is 14: 65 + 14 - 14 - 14.
    agree(entry, [], "3")


# D69: `float_list()` makes a packed list of whatever `Float` is bound to
# — under `WasiStd` an f64, in a block binding `WasiFloats32` an f32 —
# through one implementation over a per-type layout.


def test_lists__over_the_moss_std():
    agree("tests/wasi/lists.moss", [], LISTS_OUT)


# Draft, over D84's Rule A: arrays indexed by `Size`, which a rebinding
# of `Int` leaves alone (D72), and `for` over access.moss's iteration
# methods.


def test_size_and_for__for_():
    agree("tests/wasi/iterate.moss", [], "503217abc\n")


def test_size_and_for__a_std_only_library_rebinds_int_and_keeps_arrays():
    agree("tests/wasi/size_rebind.moss", [], SIZE_REBIND_OUT)


def test_size_and_for__all_of_std_under_a_std_only_int():
    agree("tests/wasi/size_std.moss", [], SIZE_STD_OUT)


def test_size_and_for__generic_code_does_not_mix_int_and_size():
    refused("tests/wasi/size_mix.moss", "argument `index` of `.get` is `Int`, expected `Size`")


# D58: same program, same answer from both runners. The oracle's own
# representation tags everything, which is exactly why this has to
# agree with the compiled one.


def test_representation__injection_is_where_the_tag_appears():
    entry = whole("""assume Std {
  tag A(Char);
  tag Other;
  type C = Other | A;
  fn bare(a: A): Char { match a { A(c) => c } }
  fn tagged(c: C): Char { match c { A(ch) => ch, Other => char::n, } }
  fn main() {
    let a = A(char::y);
    putchar(bare(a));
    putchar(tagged(a));
    putchar(tagged(Other));
  }
}
""")
    agree(entry, [], "yyn")


# D55: a functor maps one structure to another. `bind F;` installs its
# binds here; its totality against the declared result signature is
# checked once, at the functor.


def functors__functor(binds):
    return whole(f"""type Text;
fn emit(t: Text);
val greeting: Text;
context MiniStd = Text, emit, greeting;
assume Std {{
  fn shout(c: Char) {{ putchar(c); putchar(char::exclam); }}
  functor CharStd: Std -> MiniStd {{
    bind Text = Char;
    bind emit = shout;
{binds}  }}
  assume MiniStd {{ fn app() {{ emit(greeting); emit(greeting); }} }}
  fn main() {{ bind CharStd; app(); }}
}}
""")


def test_functors__application_installs_the_binds():
    agree(functors__functor("    bind greeting = char::h;\n"), [], "h!h!")


def test_functors__incomplete_functor_reports_at_the_functor():
    refused(functors__functor(""), "does not bind `greeting`")


def test_functors__self_application_is_rejected():
    entry = whole("""context Empty = ;
assume Std {
  functor Loopy: Std -> Empty { bind Loopy; }
  fn main() { bind Loopy; }
}
""")
    refused(entry, "applies itself")


def test_functors__bind_of_a_non_functor_is_an_error():
    entry = whole("assume Std {\n  val c: Char;\n  fn main() { bind c; }\n}\n")
    refused(entry, "is not a functor")


# The shared-T functor idiom from src/cell.moss, end to end: a
# totally-applied context (IsCell[T=Int, Cell=MyCell]) provides
# detached methods keyed at the *resolved* receiver type, satisfied
# by binds whose providers are attached methods (which can see
# `this`), with a D44 rename: the fixture's `.fetch`/`.stash` are
# called here under names of this module's choosing, which is the
# rule a detached method obeys — it does not match on the name it
# was declared under (D61).


def test_generics__iscell_idiom():
    entry = program(
        [
            (
                "main.moss",
                """import "{root}/tests/fixtures/inner.moss" use T;
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
""",
            )
        ]
    )
    # Known self-hosted bug: a detached method imported under another
    # name (`.stash as .cwrite`, D44) is reported (`Q`, e_irrelevant)
    # when called through the renamed context; the same program with
    # the fixture's own names compiles and runs. When it is fixed,
    # assert `agree(&entry, &[], "y")`.
    assert rejects(entry).startswith("?Q ")
    assert oracle_run(entry, []) == "y"


# A driver over src/lex.moss prints one dot per token, or a letter per
# token kind. The Python bootstrap's lexer was the reference for token
# counts; with it gone, the expected counts are written out, and on real
# files the reference is the oracle running the same driver.

self_hosted_lexer__DOTS_DRIVER = """import "{root}/src/lex.moss" as lexer;
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
"""


# A driver, compiled once, beside its entry for the oracle.
class LexerDriver:
    def __init__(self, text):
        self.entry = program([("main.moss", text)])
        self.module = build(self.entry)

    def run(self, source):
        mine = run_module(self.module, [source])
        assert oracle_run(self.entry, [source]) == mine, f"the oracle on {source!r}"
        return mine


@once
def self_hosted_lexer__dots():
    return LexerDriver(self_hosted_lexer__DOTS_DRIVER)


def self_hosted_lexer__lex_dots(source):
    return self_hosted_lexer__dots().run(source)


def self_hosted_lexer__count(n):
    return "{}\n".format(("." * n))


def test_self_hosted_lexer__symbols():
    assert self_hosted_lexer__lex_dots("{ } ( ) ; |") == self_hosted_lexer__count(6)


def test_self_hosted_lexer__two_char_symbols():
    assert self_hosted_lexer__lex_dots(
        "== = => :: : <= << < >= >> > !="
    ) == self_hosted_lexer__count(12)


def test_self_hosted_lexer__names_and_strings():
    assert self_hosted_lexer__lex_dots('import "a\\"b" as foo_2;') == self_hosted_lexer__count(5)


def test_self_hosted_lexer__comments_and_whitespace():
    assert self_hosted_lexer__lex_dots("# a comment\n;\t;\n  ; # eof") == self_hosted_lexer__count(
        3
    )


# The keyword trie distinguishes keywords from names sharing their
# prefixes/case — no string literals involved (D48).


def test_self_hosted_lexer__keyword_kinds():
    driver = LexerDriver("""import "{root}/src/lex.moss" as lexer;
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
""")
    source = 'assume assumes as As this This loops loop "x" ;'
    assert driver.run(source) == "AnantTnls.\n"


# D68's literals: `I` an integer, `D` a decimal, `b` the bad token
# the lexer reports an error for — a suffix. Length is no error: a
# literal is only arithmetic.


def test_self_hosted_lexer__literal_kinds():
    driver = LexerDriver("""import "{root}/src/lex.moss" as lexer;
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
""")
    cases = [
        ("x2 123 0 007 3.14 0.5", "nIIIDD"),
        ("1.add(2)", "I.n_I_"),
        ("1.x 1. 2.5.abs", "I.nI.D.n"),
        ("2x", "bn"),
        ("1.5f", "bn"),
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
    ]
    for source, expected in cases:
        assert driver.run(source) == f"{expected}\n", f"{source}"


# Once against the Python bootstrap's lexer's count; now the
# compiled driver and the oracle's run of it must agree, on a
# nonzero count.


def test_self_hosted_lexer__agrees_with_bootstrap_on_real_files():
    for rel in ["lib/bool.moss", "lib/num.moss", "src/lex.moss", "src/cli.moss"]:
        out = self_hosted_lexer__lex_dots(read(rel))
        assert (len(out) > 100) and set(out.rstrip("\n")) == {"."}, f"{rel}: {out}"


# D43 consistent merging: duplicate keys unify their bindings instead of
# erroring, and only a genuinely unsatisfiable merge is rejected.


def test_merging__merging_cannot_bind_a_type_outside_the_telescope():
    def with_context(items):
        return source(f"""type T;
type A;
assume T {{ fn f(); }}
context K = {items};
assume K {{ fn g() {{}} }}
fn main() {{}}
""")

    for items in ["A, f[T=A], f", "A, f, f[T=A]"]:
        entry = with_context(items)
        out = oracle_command(["--elab-only", entry])
        assert out.code == ORACLE_REJECTED, f"{out.stderr}"
        assert "a type that is not in force (D43)" in out.stderr, f"{out.stderr}"
    entry = with_context("T, A, f[T=A], f")
    out = oracle_command(["--elab-only", entry])
    assert out.code == 0, f"{out.stderr}"


merging__DECLS = """  type Foo;
  type A;
  type B;
  type C;
  fn .gimme(): Foo;
  context Ctx1 = A, B, A.gimme[Foo=B];
  context Ctx2 = A, C, A.gimme[Foo=C];
  context Ctx3 = Ctx1, Ctx2;
"""

# Inside `assume Ctx3`, B and C are one atom: `a.gimme()` (declared to
# yield B by Ctx1's item) is accepted where C is required.


def test_merging__designer_example_merges():
    entry = prog(
        f"""{merging__DECLS}  assume Ctx3 {{
    fn go(a: A): C {{ a.gimme() }}
  }}
  fn giveq(): Char {{ char::q }}""",
        """bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;
    bind A.gimme=giveq;
    putchar(go(char::m));""",
    )
    agree(entry, [], "q")


def test_merging__unsatisfiable_merge_is_error():
    entry = prog(
        """  type A;
  fn .gimme(): Foo;
  type Foo;
  tag U1;
  tag U2;
  context Ctx1 = A, A.gimme[Foo=U1];
  context Ctx2 = A, A.gimme[Foo=U2];
  context Ctx3 = Ctx1, Ctx2;
  assume Ctx3 { fn go() { } }""",
        "()",
    )
    refused(entry, "cannot merge")


# After Ctx3 merges B with C, code under it can pass a B where a C is
# expected and vice versa.


def test_merging__binding_one_merged_symbol_binds_both():
    entry = prog(
        f"""{merging__DECLS}  assume Ctx3 {{
    val b: B;
    assume b {{
      fn takes_c(x: C) {{ }}
      fn roundtrip() {{ takes_c(b); }}
    }}
  }}
  fn giveq(): Char {{ char::q }}""",
        """bind Foo=Char; bind A=Char; bind B=Char; bind C=Char;
    bind A.gimme=giveq;
    bind b=char::b;
    roundtrip();""",
    )
    agree(entry, [], "")


def test_int_list__push_get_set_length():
    entry = prog(
        "",
        """let xs = int_list();
    xs.push(one);
    xs.push(one.add(one));
    if xs.length().eq(one.add(one)) { putchar(char::n) }
    if xs.get(one).eq(one.add(one)) { putchar(char::g) }
    xs.set(zero, zero);
    if xs.get(zero).eq(zero) { putchar(char::s) }""",
    )
    agree(entry, [], "ngs")


# The Python bootstrap's native `Std` panicked on an index past the
# end. `WasiStd`'s `IntList.get` (lib/wasistd.moss, `packed_get`) does
# not check its bounds, so neither runner traps: they agree that it
# runs.


def test_int_list__out_of_range_panics():
    agree(prog("", "let xs = int_list(); xs.get(zero);"), [], "")


# The designer's point about IsList: with detached methods and a
# unique receiver type per instantiation (NameList-style), one scope
# holds IsCell at T=Int *and* T=Char — the keys are (CA, read) and
# (CB, read), distinct atoms, so D51's collision never happens.
# tests/wasi/generic.moss is a larger version of the same; this one
# once caught the self-hosted compiler reporting `e_method` for the
# whole idiom.


def test_multi_instantiation__two_iscell_instantiations_coexist():
    entry = program(
        [
            (
                "main.moss",
                """import "{root}/tests/fixtures/inner.moss" use T;
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
""",
            )
        ]
    )
    # Known self-hosted bug: as in `generics::iscell_idiom`, the
    # renamed detached methods (`.fetch as .cread`, `.stash as
    # .cwrite`) are reported (`Q`, `P`). When it is fixed, assert
    # `agree(&entry, &[], "z")`.
    assert rejects(entry).startswith("?Q ")
    assert oracle_run(entry, []) == "z"


# The interner's driver.

ARENA_DRIVER = """import "{root}/src/intern.moss" as intern;
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
"""

# The point of the codepoint arena: `take` is the only thing that
# binds `lexer::src`, so by the time `main` prints the names back the
# source they were read from is out of scope entirely. Ids are stable
# (same text, same id) and distinct texts differ.


def test_interner__names_outlive_their_source():
    agree(program([("main.moss", ARENA_DRIVER)]), ["abcd"], "abcdyd\n")
