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
        file = parse("type P { x: A, y: (B), z: () };\nval v: P;")
        tag, val = file.decls
        self.assertEqual(
            tag,
            ast.Tagdef(
                "P",
                ast.TyRecord(
                    [("x", ast.TyRef(["A"], None)), ("y", ast.TyRef(["B"], None)), ("z", ast.TyUnit())]
                ),
            ),
        )
        self.assertEqual(val, ast.Valdef("v", ast.TyRef(["P"], None)))

    def test_no_tuple_types(self):
        # D63: records are the only product type; `(T)` is grouping.
        with self.assertRaises(ParseError) as caught:
            parse("type P (A, B);")
        self.assertIn("no tuple types", str(caught.exception))

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

    def test_anonymous_function(self):
        """D70: `(params) => body`, each parameter's type and the result
        type optional; told from `()` and a parenthesized expression by
        what follows the parenthesis."""
        block = self.body(
            "bind f = (x) => x, g = (): Char => {}, T.m = (a: Int, b,) => this;"
        )
        (bind,) = block.stmts
        (_, f), (_, g), (_, m) = bind.items
        self.assertEqual(
            f, ast.Lambda([ast.Param("x", None)], None, ast.PathExpr(["x"], None))
        )
        self.assertEqual(g, ast.Lambda([], ast.TyRef(["Char"], None), ast.Block([], None)))
        self.assertEqual(
            m,
            ast.Lambda(
                [ast.Param("a", ast.TyRef(["Int"], None)), ast.Param("b", None)],
                None,
                ast.ThisExpr(),
            ),
        )
        block = self.body("(x).f(); (); (x + y)")
        self.assertIsInstance(block.stmts[0].expr, ast.MethodCall)
        self.assertEqual(block.stmts[1].expr, ast.UnitExpr())
        self.assertIsInstance(block.tail, ast.Binary)

    def test_anonymous_function_body_extends_to_the_end(self):
        """The body is a whole expression: `(x) => x + 1` is one lambda,
        and a block body is a bare block (D65)."""
        (bind,) = self.body("bind f = (x) => x + 1;").stmts
        self.assertIsInstance(bind.items[0][1].body, ast.Binary)

    def test_anonymous_function_needs_its_arrow(self):
        for source in ("(x, y)", "(x: Int)", "bind f = (x): => x;"):
            with self.subTest(source=source):
                with self.assertRaises(ParseError):
                    self.body(source)

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

    def test_bare_block(self):
        # D65: a block is an expression; as a statement it needs no `;`.
        block = self.body("{ bind X; foo(); } let y = { let z = f(); z }; { g() }")
        stmt, let = block.stmts
        bind = ast.Bind([(ast.Spec(["X"], None, None), None)])
        call = ast.Call(ast.PathExpr(["foo"], None), [])
        self.assertEqual(stmt, ast.ExprStmt(ast.Block([bind, ast.ExprStmt(call)], None)))
        self.assertEqual(
            let.expr,
            ast.Block(
                [ast.Let("z", None, ast.Call(ast.PathExpr(["f"], None), []))],
                ast.PathExpr(["z"], None),
            ),
        )
        self.assertEqual(
            block.tail, ast.Block([], ast.Call(ast.PathExpr(["g"], None), []))
        )

    def test_bare_block_as_operand(self):
        block = self.body("f({ x }); { a }.m(); {}")
        arg = block.stmts[0].expr.args[0]
        self.assertEqual(arg, ast.Block([], ast.PathExpr(["x"], None)))
        method = block.stmts[1].expr
        self.assertEqual(method.obj, ast.Block([], ast.PathExpr(["a"], None)))
        self.assertEqual(block.tail, ast.Block([], None))

    def test_no_bare_block_in_condition(self):
        # As with a record, a brace where a condition or scrutinee begins
        # opens the body, so a bare block there must be parenthesized.
        for source in ("if { a } { b() }", "while { a } { b(); }", "match { a } { _ => b }"):
            with self.subTest(source=source), self.assertRaises(ParseError):
                self.body(source)
        block = self.body("if ({ a }) { b() }")
        self.assertEqual(block.tail.cond, ast.Block([], ast.PathExpr(["a"], None)))

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


def grouping(expr) -> str:
    """An expression with every operator parenthesized, so a test can say
    how the parser grouped it."""
    if isinstance(expr, ast.Binary):
        return f"({grouping(expr.lhs)} {expr.op} {grouping(expr.rhs)})"
    if isinstance(expr, ast.Unary):
        return f"({expr.op}{grouping(expr.operand)})"
    if isinstance(expr, ast.PathExpr):
        return "::".join(expr.path)
    if isinstance(expr, ast.Literal):
        return expr.text
    if isinstance(expr, ast.MethodCall):
        args = ", ".join(grouping(a) for a in expr.args)
        return f"{grouping(expr.obj)}.{'::'.join(expr.path)}({args})"
    if isinstance(expr, ast.Field):
        return f"{grouping(expr.obj)}.{expr.name}"
    if isinstance(expr, ast.Call):
        args = ", ".join(grouping(a) for a in expr.args)
        return f"{grouping(expr.callee)}({args})"
    raise AssertionError(f"no grouping for {expr!r}")


# D68's partial order, as (source, how it groups). tests/test_run.py holds
# the self-hosted parser to the same table.
OPERATOR_GROUPINGS = [
    # Unary operators bind tightest, method calls and fields tighter.
    ("-a * b", "((-a) * b)"),
    ("!a & b", "((!a) & b)"),
    ("- -a", "(-(-a))"),
    ("!!a", "(!(!a))"),
    ("-a.f(b)", "(-a.f(b))"),
    ("-x.y", "(-x.y)"),
    # `*` chains; `/` and `%` take a product on the left, a unary on the right.
    ("a * b * c", "((a * b) * c)"),
    ("a * b * c / d", "(((a * b) * c) / d)"),
    ("a * b % c", "((a * b) % c)"),
    ("a / -b", "(a / (-b))"),
    ("(a / b) * c", "((a / b) * c)"),
    ("a / (b / c)", "(a / (b / c))"),
    # Additive: left-associative, looser than multiplicative.
    ("a - b + c", "((a - b) + c)"),
    ("a - b - c", "((a - b) - c)"),
    ("a + b * c", "(a + (b * c))"),
    ("a * b + c / d", "((a * b) + (c / d))"),
    ("a + b % c - d", "((a + (b % c)) - d)"),
    # Comparison: looser than additive, either side.
    ("a + b == c * d", "((a + b) == (c * d))"),
    ("a != b", "(a != b)"),
    ("a <= b - c", "(a <= (b - c))"),
    ("-a >= b", "((-a) >= b)"),
    ("a > b", "(a > b)"),
    ("a < b", "(a < b)"),
    # Shifts take unaries, and may be a comparison's operand.
    ("a << b", "(a << b)"),
    ("-a >> -b", "((-a) >> (-b))"),
    ("x << f(n) == y", "((x << f(n)) == y)"),
    ("y == x >> n", "(y == (x >> n))"),
    ("(a << b) + c", "((a << b) + c)"),
    # Bitwise: each chains with itself, and nothing else.
    ("a & b & c", "((a & b) & c)"),
    ("a | b | c", "((a | b) | c)"),
    ("a ^ b ^ c", "((a ^ b) ^ c)"),
    ("!a | !b", "((!a) | (!b))"),
    ("(a < b) & (c < d)", "((a < b) & (c < d))"),
    ("(x & mask) == z", "((x & mask) == z)"),
]

# Mixing that D68 refuses, with what the message says.
OPERATOR_ERRORS = [
    ("a / b * c", "`/` and `*` are not ordered"),
    ("a * b / c * d", "`/` and `*` are not ordered"),
    ("a / b / c", "`/` does not chain"),
    ("a / b % c", "`/` and `%` are not ordered"),
    ("a % b * c", "`%` and `*` are not ordered"),
    ("a % b % c", "`%` does not chain"),
    ("a < b < c", "comparisons do not chain"),
    ("a == b == c", "comparisons do not chain"),
    ("a < b == c", "comparisons do not chain"),
    ("k << n + k", "`<<` and `+` are not ordered"),
    ("n + k << m", "`+` and `<<` are not ordered"),
    ("a * b >> c", "`*` and `>>` are not ordered"),
    ("a << b * c", "`<<` and `*` are not ordered"),
    ("a << b << c", "shifts do not chain"),
    ("a << b >> c", "shifts do not chain"),
    ("a & b | c", "`&` and `|` are not ordered"),
    ("a | b & c", "`|` and `&` are not ordered"),
    ("a ^ b & c", "`^` and `&` are not ordered"),
    ("a < b & c < d", "`<` and `&` are not ordered"),
    ("x & mask == z", "`&` and `==` are not ordered"),
    ("a & b + c", "`&` and `+` are not ordered"),
    ("a + b & c", "`+` and `&` are not ordered"),
    ("a << b & c", "`<<` and `&` are not ordered"),
    ("a & b << c", "`&` and `<<` are not ordered"),
    ("a == b | c", "`==` and `|` are not ordered"),
]


class TestOperators(unittest.TestCase):
    """D68: precedence is a partial order, so some mixtures group and the
    rest are syntax errors that say which two operators met."""

    def expr(self, source):
        (fn,) = parse(f"fn f() {{ {source} }}").decls
        return fn.body.tail

    def test_groupings(self):
        for source, expected in OPERATOR_GROUPINGS:
            with self.subTest(source=source):
                self.assertEqual(grouping(self.expr(source)), expected)

    def test_unordered_mixtures_are_errors(self):
        for source, message in OPERATOR_ERRORS:
            with self.subTest(source=source):
                with self.assertRaises(ParseError) as caught:
                    self.expr(source)
                self.assertIn(message, caught.exception.message)
                self.assertIn("add parentheses (D68)", caught.exception.message)

    def test_the_error_points_at_the_second_operator(self):
        with self.assertRaises(ParseError) as caught:
            self.expr("a / b * c")
        self.assertEqual(caught.exception.token.text, "*")

    def test_nodes(self):
        self.assertEqual(
            self.expr("-a + b"),
            ast.Binary(
                "+", ast.Unary("-", ast.PathExpr(["a"], None)), ast.PathExpr(["b"], None)
            ),
        )

    def test_operators_in_conditions_and_arguments(self):
        (fn,) = parse("fn f() { while i < n { g(a + b, -c); } if !done { h() } }").decls
        loop, cond = fn.body.stmts[0], fn.body.tail
        self.assertEqual(grouping(loop.cond), "(i < n)")
        self.assertEqual(
            [grouping(a) for a in loop.body.stmts[0].expr.args], ["(a + b)", "(-c)"]
        )
        self.assertEqual(grouping(cond.cond), "(!done)")

    def test_no_record_after_an_operator_in_a_condition(self):
        # The brace after `b` opens the body, as it would after `b` alone.
        (fn,) = parse("fn f() { if a < b { c } }").decls
        self.assertEqual(grouping(fn.body.tail.cond), "(a < b)")

    def test_return_and_let_take_operators(self):
        block = self.body_of("let x = a * b; return x + y;")
        self.assertEqual(grouping(block.stmts[0].expr), "(a * b)")
        self.assertEqual(grouping(block.stmts[1].expr.expr), "(x + y)")

    def test_literals(self):
        self.assertEqual(self.expr("3.14"), ast.Literal("3.14"))
        self.assertEqual(grouping(self.expr("1.add(2)")), "1.add(2)")
        self.assertEqual(grouping(self.expr("-1 * 2")), "((-1) * 2)")
        self.assertEqual(grouping(self.expr("(-1).abs()")), "(-1).abs()")
        self.assertEqual(grouping(self.expr("-(1.abs())")), "(-1.abs())")
        self.assertEqual(grouping(self.expr("x - 1.abs()")), "(x - 1.abs())")
        self.assertEqual(grouping(self.expr("2.5.sqrt()")), "2.5.sqrt()")

    def test_minus_before_a_literal_with_a_method_call(self):
        """`-1.abs()` would be `-(1.abs())`, so D68 refuses it."""
        for source in ("-1.abs()", "-2.5.sqrt()", "- 1.x", "- -1.abs()"):
            with self.subTest(source=source):
                with self.assertRaises(ParseError) as caught:
                    self.expr(source)
                self.assertIn("`-` before a literal with a method call", caught.exception.message)

    def test_literal_errors_from_the_draft(self):
        with self.assertRaises(ParseError) as caught:
            self.expr("1 << n + 1")
        self.assertIn("`<<` and `+` are not ordered", caught.exception.message)

    def body_of(self, source):
        (fn,) = parse(f"fn f() {{ {source} }}").decls
        return fn.body


if __name__ == "__main__":
    unittest.main()
