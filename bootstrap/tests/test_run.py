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


def main_body(body, decls=""):
    return {"main.moss": f"assume Std {{\n{decls}\n  fn main() {{ {body} }}\n}}"}


class TestHello(unittest.TestCase):
    def test_hello_world(self):
        self.assertEqual(
            run({}, entry="examples/hello.moss"), "Hello, world!\n"
        )


class TestExamples(unittest.TestCase):
    """Every rewritten example matches its golden stdout. `escape` is still
    written in the old language (it needs string literals) and is excluded
    until D48 is decided."""

    EXAMPLES = ["hello", "true", "reassign", "params", "context", "rebind", "exit"]

    def test_goldens(self):
        for name in self.EXAMPLES:
            with self.subTest(example=name):
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                # Loaded in place: an example's imports are relative to it.
                self.assertEqual(run({}, entry=f"examples/{name}.moss"), golden)


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
        with a D44 rename keeping src's .read distinct from Std's."""
        files = {
            "main.moss": (
                'import "./tests/fixtures/inner.moss" use T;\n'
                'import "./tests/fixtures/cell.moss" use Cell, IsCell, .read as .cread, .write as .cwrite;\n'
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
        (CB, read), distinct atoms, so D51's collision never happens."""
        files = {
            "main.moss": (
                'import "./tests/fixtures/inner.moss" use T;\n'
                'import "./tests/fixtures/cell.moss" use Cell, IsCell, .read as .cread, .write as .cwrite;\n'
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
        import tempfile

        module = run_bytes({}, entry="tests/wasi/emit.moss")
        self.assertEqual(module[:8], b"\0asm\x01\0\0\0")
        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(module)
            path = f.name
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

# Everything that is written in the MVP language. `src/lower.moss` is
# previous-iteration code (§12 errata) and `examples/escape.moss` needs
# string literals, so neither parses under either parser.
def corpus():
    for folder, pattern in (
        ("src", "*.moss"),
        ("lib", "*.moss"),
        ("examples", "*.moss"),
        ("tests/wasi", "*.moss"),
    ):
        for path in sorted((REPO / folder).glob(pattern)):
            rel = str(path.relative_to(REPO))
            if rel in ("src/lower.moss", "examples/escape.moss"):
                continue
            yield rel


class TestSelfHostedSyntax(unittest.TestCase):
    """src/syntax.moss against bootstrap/mossc/parse.py, over the whole
    corpus: both write the tree in the compact form of mossc/sexpr.py, and
    the two must agree character for character. This is the front end's
    real check — an approximate parser can produce a plausible letter
    dump, but it cannot produce the bootstrap's tree."""

    def test_agrees_with_the_bootstrap_parser(self):
        import os

        from mossc.parse import parse
        from mossc.sexpr import dump

        cwd = os.getcwd()
        os.chdir(REPO)
        try:
            for rel in corpus():
                with self.subTest(file=rel):
                    expected = dump(parse((REPO / rel).read_text(encoding="utf-8")))
                    actual = run({"main.moss": PARSE_DRIVER}, args=[rel])
                    self.assertEqual(actual, expected + "\n")
        finally:
            os.chdir(cwd)


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
    rows, over the compiler's own sources: 23 modules, 1101 rows."""

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
        import os

        cwd = os.getcwd()
        os.chdir(REPO)
        try:
            out = run(
                {"main.moss": SCOPE_DRIVER}, args=["lib/prelude.moss", entry]
            )
        finally:
            os.chdir(cwd)
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
        import os

        cwd = os.getcwd()
        os.chdir(REPO)
        try:
            return run_bytes({"main.moss": COMPILER_DRIVER}, args=[prelude, entry])
        finally:
            os.chdir(cwd)

    def wasmtime_run(self, module, expect_code=0):
        import shutil
        import subprocess
        import tempfile

        with tempfile.NamedTemporaryFile(suffix=".wasm", delete=False) as f:
            f.write(module)
            path = f.name
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

    def test_matches_the_bootstrap_on_behaviour(self):
        """Two compilers, one program: the bytes differ — the bootstrap
        emits shims this back end has no need for — but what the modules
        do is the same."""
        from .test_build import compile_wasm, run_wasm

        for entry, expected in (("tests/wasi/prim.moss", "ABKDKJGG\n"),):
            with self.subTest(entry=entry):
                mine = self.wasmtime_run(self.compile_with_moss(entry))
                self.assertEqual(mine, expected)
                self.assertEqual(run_wasm(compile_wasm({}, entry=entry)), expected)

    def test_a_program_beyond_the_slice_is_reported(self):
        """`Std` is a library over the primitive context, and providing it
        means functors, methods and tags — none of which this back end
        compiles yet. It says so rather than emitting something wrong."""
        out = self.compile_with_moss("examples/hello.moss", prelude="lib/prelude.moss")
        self.assertNotEqual(out[:4], b"\0asm")
        self.assertTrue(out.decode("utf-8").startswith("?"))
