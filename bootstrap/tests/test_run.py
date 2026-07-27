import io
import unittest
from contextlib import redirect_stdout
from pathlib import Path

from mossc import collect, interp
from mossc.lower import Lower, LowerError

REPO = Path(__file__).resolve().parents[2]
PRELUDE = str(REPO / "lib/prelude.moss")


def run(files, entry="main.moss", args=None):
    """Run a program given as {path: source}; lib/ comes from disk."""

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
    out = io.StringIO()
    with redirect_stdout(out):
        interp.run_main(program, lower, args)
    return out.getvalue()


def main_body(body, decls=""):
    return {"main.moss": f"assume Std {{\n{decls}\n  fn main() {{ {body} }}\n}}"}


class TestHello(unittest.TestCase):
    def test_hello_world(self):
        source = (REPO / "examples/hello.moss").read_text(encoding="utf-8")
        self.assertEqual(run({"main.moss": source}), "Hello, world!\n")


class TestExamples(unittest.TestCase):
    """Every rewritten example matches its golden stdout. `escape` is still
    written in the old language (it needs string literals) and is excluded
    until D48 is decided."""

    EXAMPLES = ["hello", "true", "reassign", "params", "context", "rebind", "exit"]

    def test_goldens(self):
        for name in self.EXAMPLES:
            with self.subTest(example=name):
                source = (REPO / f"examples/{name}.moss").read_text(encoding="utf-8")
                golden = (REPO / f"tests/examples/stdout/{name}.txt").read_text(
                    encoding="utf-8"
                )
                self.assertEqual(run({"main.moss": source}), golden)


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

    def test_string_slice_out_of_range_panics(self):
        files = main_body("let s = first_arg().slice(zero, one.add(one));")
        with self.assertRaises(interp.MossPanic):
            run(files, args=["x"])

    def test_division_by_zero_panics(self):
        files = main_body("let x = one.div(zero);")
        with self.assertRaises(interp.MossPanic):
            run(files)


class TestGenerics(unittest.TestCase):
    def test_iscell_idiom(self):
        """The shared-T functor idiom from src/cell.moss, end to end: a
        totally-applied context (IsCell[T=Int, Cell=MyCell]) provides
        detached methods keyed at the *resolved* receiver type, satisfied by
        binds whose providers are attached methods (which can see `this`),
        with a D44 rename keeping src's .read distinct from Std's."""
        files = {
            "main.moss": (
                'import "./src/inner.moss" use T;\n'
                'import "./src/cell.moss" use Cell, IsCell, .read as .cread, .write as .cwrite;\n'
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
                'import "./src/inner.moss" use T;\n'
                'import "./src/cell.moss" use Cell, IsCell, .read as .cread, .write as .cwrite;\n'
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


class TestSelfHostedParser(unittest.TestCase):
    """src/main.moss drives src/parse.moss: one letter per declaration
    (i=import, a=assume, t=type, u=unit, v=val, c=context, f=fn, x=junk)."""

    def parse_letters(self, source):
        import tempfile

        with tempfile.NamedTemporaryFile("w", suffix=".moss", delete=False) as f:
            f.write(source)
            target = f.name
        return run({}, entry=str(REPO / "src/main.moss"), args=[target])

    def test_shapes(self):
        source = (
            'import "./x.moss" use A;\n'
            "unit U;\n"
            "type T { a: A };\n"
            "assume A {\n"
            "  val v: A;\n"
            "  fn f(): A;\n"
            "  fn g() { if b { c(); } loop { break; } }\n"
            "  assume v { fn h(); }\n"
            "}\n"
            "context C = A;\n"
        )
        self.assertEqual(
            self.parse_letters(source), "i./x.moss;uU;tT;a(vv;ff;fg;a(fh;))cC;\n\n\n"
        )

    def test_junk_marked(self):
        self.assertEqual(self.parse_letters("; unit U;"), "?;uU;\n\n\n")

    def test_real_files_have_no_junk(self):
        for rel in [
            "lib/bool.moss",
            "lib/std.moss",
            "src/lex.moss",
            "src/parse.moss",
            "src/tree.moss",
        ]:
            with self.subTest(file=rel):
                text = (REPO / rel).read_text(encoding="utf-8")
                out = self.parse_letters(text)
                # Only the tree line: `?` also marks unresolved references,
                # and real files reference prelude names the self-hosted
                # resolver cannot see until it loads modules.
                self.assertNotIn("?", out.split("\n")[0])
                self.assertGreater(len(out.strip()), 0)

    def test_parses_itself(self):
        text = (REPO / "src/parse.moss").read_text(encoding="utf-8")
        self.assertEqual(
            self.parse_letters(text),
            "i./lex.moss;i./token.moss;i./intern.moss;i./tree.moss;"
            "a(a(fskip_braces;frefscan_to_semi;fskip_to_semi;"
            "frefscan_fn;fnamed;fdecls;fput_name;fhas_name;fdump;fdups;"
            "fdeclared;fcontains;fresolve;frun;))\n\n\n",
        )

    def test_names_read_back_from_the_arena(self):
        text = (REPO / "lib/bool.moss").read_text(encoding="utf-8")
        self.assertEqual(
            self.parse_letters(text), "uFalse;uTrue;tBool;vfalse;vtrue;\n\n\n"
        )

    def test_duplicate_declarations_reported(self):
        """Collect's first check, self-hosted: two declarations sharing a
        name in one scope, found via interned name ids in the arena."""
        source = (
            "unit A;\n"
            "type A;\n"
            "assume B {\n"
            "  fn f();\n"
            "  fn f() {}\n"
            "  val g: B;\n"
            "}\n"
        )
        # The third line is resolution: B is referenced but never declared.
        self.assertEqual(
            self.parse_letters(source), "uA;tA;a(ff;ff;vg;)\nA!f!\nB?\n"
        )

    def test_unresolved_references_reported(self):
        """Collect's second check, self-hosted: references that name no
        declaration print as name+? — the "not in scope" error."""
        source = (
            "type A;\n"
            "val v: A;\n"
            "val w: B;\n"
            "assume A {\n"
            "  fn f(x: A): Missing;\n"
            "}\n"
        )
        self.assertEqual(
            self.parse_letters(source), "tA;vv;vw;a(ff;)\n\nB?Missing?\n"
        )

    def test_method_names(self):
        source = "assume A { fn T.m(); fn .d(); fn plain(); }"
        self.assertEqual(self.parse_letters(source), "a(fm;fd;fplain;)\n\n\n")
