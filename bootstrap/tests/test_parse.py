import unittest
from pathlib import Path

from mossc import ast
from mossc.parse import ParseError, parse

REPO = Path(__file__).resolve().parents[2]


class TestCorpus(unittest.TestCase):
    def test_corpus_parses(self):
        # The shared glob, not a hand-kept list: a list of "files already
        # in the language" fell two files behind the language.
        from .test_run import corpus

        for rel in corpus():
            with self.subTest(file=rel):
                source = (REPO / rel).read_text(encoding="utf-8")
                parse(source)

    def test_token_moss_shape(self):
        file = parse((REPO / "src/token.moss").read_text(encoding="utf-8"))
        self.assertEqual(file.imports, [])
        self.assertTrue(all(isinstance(d, ast.Unitdef) for d in file.decls))
        self.assertEqual(file.decls[0], ast.Unitdef("Eof"))

    def test_main_moss_shape(self):
        file = parse((REPO / "src/main.moss").read_text(encoding="utf-8"))
        self.assertEqual(file.imports[0].path, "./cli.moss")
        (assume,) = file.decls
        # D52: `main` receives the primitive context and installs `Std`.
        self.assertEqual(
            assume.items,
            [
                ast.AssumeItem(["Wasm"], None),
                ast.AssumeItem(["Wasi"], None),
                ast.AssumeItem(["Branch"], None),
            ],
        )
        (fn,) = assume.decls
        self.assertEqual(fn.name, ast.FnName(None, False, "main"))
        self.assertEqual(
            fn.body.stmts[-1],
            ast.ExprStmt(ast.Call(ast.PathExpr(["cli", "cli"], None), [])),
        )


class TestDeclarations(unittest.TestCase):
    def test_type_forms(self):
        file = parse("type A; type B = C; type D E; type F = | G | H; type N = |;")
        self.assertEqual(
            file.decls,
            [
                ast.Tydef("A"),
                ast.Aliasdef("B", ast.TyRef(["C"], None)),
                ast.Tagdef("D", ast.TyRef(["E"], None)),
                ast.Aliasdef("F", ast.TyUnion([ast.TyRef(["G"], None), ast.TyRef(["H"], None)])),
                ast.Aliasdef("N", ast.TyNever()),
            ],
        )

    def test_record_tag_and_val(self):
        file = parse("type P { x: A, y: (B, C) };\nval v: P;")
        tag, val = file.decls
        self.assertEqual(
            tag,
            ast.Tagdef(
                "P",
                ast.TyRecord(
                    [("x", ast.TyRef(["A"], None)), ("y", ast.TyTuple([ast.TyRef(["B"], None), ast.TyRef(["C"], None)]))]
                ),
            ),
        )
        self.assertEqual(val, ast.Valdef("v", ast.TyRef(["P"], None)))

    def test_fn_names(self):
        file = parse("fn f(); fn T.m(x: A): B; fn .d(): This;")
        plain, attached, detached = file.decls
        self.assertEqual(plain.name, ast.FnName(None, False, "f"))
        self.assertIsNone(plain.ret)
        self.assertEqual(attached.name, ast.FnName("T", False, "m"))
        self.assertEqual(attached.params, [ast.Param("x", ast.TyRef(["A"], None))])
        self.assertEqual(detached.name, ast.FnName(None, True, "d"))
        self.assertEqual(detached.ret, ast.TyThis())

    def test_context_with_apps(self):
        file = parse("context C = A, B.m[Foo=D], .n;")
        (ctx,) = file.decls
        self.assertEqual(
            ctx.items,
            [
                ast.Spec(["A"], None, None),
                ast.Spec(["B"], "m", [ast.Binding(["Foo"], ast.Spec(["D"], None, None))]),
                ast.Spec(None, "n", None),
            ],
        )

    def test_import_forms(self):
        file = parse(
            'import "./a.moss" as a;\n'
            'import "./b.moss" use *;\n'
            'import "./c.moss" use x, .m as .m1;\n'
        )
        a, b, c = file.imports
        self.assertEqual((a.alias, a.glob, a.uses), ("a", False, []))
        self.assertTrue(b.glob)
        self.assertEqual(
            c.uses,
            [
                ast.UseItem(ast.UseName(False, "x"), None),
                ast.UseItem(ast.UseName(True, "m"), ast.UseName(True, "m1")),
            ],
        )

    def test_use_rename_must_keep_dot(self):
        with self.assertRaises(ParseError):
            parse('import "./a.moss" use .m as m1;')


class TestExpressions(unittest.TestCase):
    def body(self, source):
        (fn,) = parse(f"fn f() {{ {source} }}").decls
        return fn.body

    def test_statements(self):
        block = self.body("let x = f(); var y = x; y = g(); h();")
        self.assertEqual(
            [type(s) for s in block.stmts], [ast.Let, ast.Var, ast.Assign, ast.ExprStmt]
        )
        self.assertIsNone(block.tail)

    def test_tail_expression(self):
        block = self.body("f(); x")
        self.assertEqual(block.tail, ast.PathExpr(["x"], None))

    def test_bind(self):
        block = self.body("bind lexer::next_byte=next_byte; bind parser::TokenId=Int;")
        first, second = block.stmts
        self.assertEqual(first, ast.Bind([(ast.Spec(["lexer", "next_byte"], None, None), ast.PathExpr(["next_byte"], None))]))
        self.assertEqual(second, ast.Bind([(ast.Spec(["parser", "TokenId"], None, None), ast.PathExpr(["Int"], None))]))

    def test_method_field_and_qualified_call(self):
        block = self.body("x.f; x.m(y); x.b::m(y);")
        field, method, qualified = [s.expr for s in block.stmts]
        self.assertEqual(field, ast.Field(ast.PathExpr(["x"], None), "f"))
        self.assertEqual(method, ast.MethodCall(ast.PathExpr(["x"], None), ["m"], [ast.PathExpr(["y"], None)]))
        self.assertEqual(qualified.path, ["b", "m"])

    def test_tag_and_record_construction(self):
        block = self.body("Some[T=TokenId] (x); Import { from, name = n };")
        tag, record = [s.expr for s in block.stmts]
        self.assertEqual(tag.callee, ast.PathExpr(["Some"], [ast.Binding(["T"], ast.Spec(["TokenId"], None, None))]))
        self.assertEqual(record.fields, [("from", None), ("name", ast.PathExpr(["n"], None))])

    def test_if_else_chain(self):
        block = self.body("if a { b() } else if c { d() } else { e() }")
        self.assertIsInstance(block.tail, ast.If)
        self.assertIsInstance(block.tail.els, ast.If)
        self.assertIsInstance(block.tail.els.els, ast.Block)

    def test_if_as_statement_without_semi(self):
        block = self.body("if a { b(); } f();")
        self.assertEqual([type(s) for s in block.stmts], [ast.ExprStmt, ast.ExprStmt])

    def test_loops(self):
        block = self.body("loop { break; } while a { b(); }")
        self.assertEqual([type(s) for s in block.stmts], [ast.Loop, ast.While])

    def test_match(self):
        block = self.body(
            "match peek() { Eof => return, As => { next(); } Some token => f(token), _ => (), }"
        )
        match = block.tail
        self.assertEqual(len(match.arms), 4)
        self.assertEqual(match.arms[0].pattern, ast.PatPath(["Eof"]))
        self.assertEqual(match.arms[0].body, ast.Return(None))
        self.assertIsInstance(match.arms[1].body, ast.Block)
        self.assertEqual(match.arms[2].pattern, ast.PatTag(["Some"], ast.PatPath(["token"])))
        self.assertEqual(match.arms[3].pattern, ast.PatWild())
        self.assertEqual(match.arms[3].body, ast.UnitExpr())

    def test_empty_match(self):
        block = self.body("match panic() {}")
        self.assertEqual(block.tail.arms, [])

    def test_record_patterns(self):
        block = self.body("match x { p::Symbol { name = token } => a, p::Fn { sig } => b, }")
        arms = block.tail.arms
        self.assertEqual(arms[0].pattern, ast.PatRecord(["p", "Symbol"], [("name", ast.PatPath(["token"]))]))
        self.assertEqual(arms[1].pattern, ast.PatRecord(["p", "Fn"], [("sig", None)]))

    def test_no_record_in_scrutinee(self):
        with self.assertRaises(ParseError):
            self.body("match x { y } { _ => a, }")

    def test_this(self):
        (assume,) = parse("assume C { fn C.gimme(): C { this } }").decls
        (fn,) = assume.decls
        self.assertEqual(fn.body.tail, ast.ThisExpr())

    def test_missing_semi(self):
        with self.assertRaises(ParseError):
            self.body("f() g();")


if __name__ == "__main__":
    unittest.main()
