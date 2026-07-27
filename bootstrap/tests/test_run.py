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
        return collect.default_read(path)

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

    def test_golden_stdout(self):
        golden = (REPO / "tests/examples/stdout/hello.txt").read_text(encoding="utf-8")
        source = (REPO / "examples/hello.moss").read_text(encoding="utf-8")
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
            "bind c=char::x; if True { bind c=char::y; emit(); } emit();",
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
            decls="  fn yes(): Bool { True }",
        )
        self.assertEqual(run(files), "y")

    def test_match_units(self):
        files = main_body(
            "putchar(pick(False)); putchar(pick(True));",
            decls="  fn pick(b: Bool): Char { match b { True => char::t, False => char::f, } }",
        )
        self.assertEqual(run(files), "ft")

    def test_non_exhaustive(self):
        files = main_body(
            "()",
            decls="  fn pick(b: Bool): Char { match b { True => char::t, } }",
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
            "putchar(show(go(True))); putchar(show(go(False)));",
            decls="  unit Stop;\n"
            "  type Step Char;\n"
            "  type Out = | Stop | Step;\n"
            "  fn go(b: Bool): Out { match b { True => Step (char::s), False => Stop, } }\n"
            "  fn show(o: Out): Char { match o { Step c => c, Stop => char::dot, } }",
        )
        self.assertEqual(run(files), "s.")

    def test_while_loop_break(self):
        files = main_body(
            "var again: Bool = True;\n"
            "    while again { putchar(char::w); again = False; }\n"
            "    loop { putchar(char::l); break; }",
        )
        self.assertEqual(run(files), "wl")

    def test_recursion(self):
        files = main_body(
            "putchar(rec(True));",
            decls="  fn rec(b: Bool): Char { match b { True => rec(False), False => char::r, } }",
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

    def test_division_by_zero_panics(self):
        files = main_body("let x = one.div(zero);")
        with self.assertRaises(interp.MossPanic):
            run(files)


class TestSelfHostedLexer(unittest.TestCase):
    """src/main.moss drives src/lex.moss: one dot per token. The reference
    for token counts is the bootstrap lexer itself (keywords lex as Name in
    the self-hosted lexer, but the counts must agree)."""

    def lex_dots(self, source):
        import tempfile

        with tempfile.NamedTemporaryFile("w", suffix=".moss", delete=False) as f:
            f.write(source)
            target = f.name
        return run({}, entry=str(REPO / "src/main.moss"), args=[target])

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

    def test_agrees_with_bootstrap_on_real_files(self):
        for rel in ["lib/bool.moss", "lib/num.moss", "src/lex.moss", "src/cli.moss"]:
            with self.subTest(file=rel):
                text = (REPO / rel).read_text(encoding="utf-8")
                expected = self.bootstrap_count(text)
                self.assertEqual(self.lex_dots(text), "." * expected + "\n")


if __name__ == "__main__":
    unittest.main()
