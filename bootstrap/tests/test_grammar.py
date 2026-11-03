"""The VS Code grammar against the lexer.

vscode/syntaxes/moss.tmLanguage.json is a second description of the token
set, kept by hand, and nothing else notices when it falls behind the
lexer: a keyword it does not know is highlighted as a variable, and an
operator it splits in two is highlighted as two. So this test reads the
grammar with Python's `re`, tokenizes each token of mossc/lex.py on its
own the way a TextMate engine does — at each position the pattern whose
match starts earliest wins, ties going to the one listed first — and
checks what it is called.
"""

import json
import re
import unittest
from pathlib import Path

from mossc.lex import KEYWORDS, ONE_CHAR, TWO_CHAR
from mossc.parse import BINARY, UNARY

REPO = Path(__file__).resolve().parents[2]
GRAMMAR = REPO / "vscode/syntaxes/moss.tmLanguage.json"


def load():
    return json.loads(GRAMMAR.read_text(encoding="utf-8"))


def flatten(grammar, patterns, seen=()):
    """The grammar's top-level patterns, with each `include` expanded in
    place: the order a TextMate engine tries them in."""
    out = []
    for pattern in patterns:
        if "include" in pattern:
            key = pattern["include"].lstrip("#")
            if key in seen:
                continue
            out += flatten(grammar, grammar["repository"][key]["patterns"], (*seen, key))
        elif "patterns" in pattern and "begin" not in pattern:
            out += flatten(grammar, pattern["patterns"], seen)
        else:
            out.append(pattern)
    return out


def scope_of(pattern) -> str:
    if "name" in pattern:
        return pattern["name"]
    captures = pattern.get("captures") or pattern.get("beginCaptures") or {}
    return captures.get("1", captures.get("0", {})).get("name", "")


def tokenize(text: str) -> list:
    """(text, scope) for each match the grammar's top level makes in
    `text`, one line with no nesting — which is all a token alone needs."""
    grammar = load()
    patterns = [
        (re.compile(p.get("match", p.get("begin"))), scope_of(p), p.get("captures") if "match" in p else None)
        for p in flatten(grammar, grammar["patterns"])
    ]
    out = []
    pos = 0
    while pos < len(text):
        best = None
        for regex, scope, captures in patterns:
            m = regex.search(text, pos)
            if m is not None and m.end() > m.start():
                if best is None or m.start() < best[0].start():
                    best = (m, scope, captures)
        if best is None:
            break
        m, scope, captures = best
        if text[pos : m.start()].strip():
            out.append((text[pos : m.start()], None))
        if captures:
            # Each capture is a token of its own, as an engine scopes it;
            # what lies between them is only whitespace.
            at = m.start()
            for group in sorted(captures, key=int):
                if m.group(int(group)) is None:
                    continue
                start, end = m.span(int(group))
                assert not text[at:start].strip(), (text, m.group(0))
                out.append((text[start:end], captures[group]["name"]))
                at = end
            assert not text[at : m.end()].strip(), (text, m.group(0))
        else:
            out.append((m.group(0), scope))
        pos = m.end()
    if text[pos:].strip():
        out.append((text[pos:], None))
    return out


class TestGrammarAgreesWithTheLexer(unittest.TestCase):
    def assert_one_token(self, text, prefixes):
        tokens = tokenize(text)
        self.assertEqual(len(tokens), 1, f"{text!r} splits into {tokens}")
        (matched, scope) = tokens[0]
        self.assertEqual(matched, text)
        self.assertTrue(
            scope and scope.startswith(prefixes),
            f"{text!r} is scoped {scope!r}, wanted one of {prefixes}",
        )
        return scope

    def test_every_keyword(self):
        for word in KEYWORDS:
            with self.subTest(keyword=word):
                self.assert_one_token(
                    word,
                    ("keyword.", "storage.type", "variable.language"),
                )

    def test_every_symbol(self):
        for symbol in [*ONE_CHAR, *TWO_CHAR]:
            with self.subTest(symbol=symbol):
                self.assert_one_token(symbol, ("keyword.operator.", "punctuation."))

    def test_operators_are_no_longer_reserved(self):
        """D68 gave the reserved operator tokens a meaning, so each is
        scoped by what it does — `|` neutrally, since it is also the union
        type's bar."""
        expected = {
            "*": "arithmetic", "/": "arithmetic", "%": "arithmetic",
            "+": "arithmetic", "-": "arithmetic",
            "==": "comparison", "!=": "comparison", "<": "comparison",
            ">": "comparison", "<=": "comparison", ">=": "comparison",
            "<<": "bitwise.shift", ">>": "bitwise.shift",
            "&": "bitwise", "^": "bitwise", "!": "logical",
        }
        operators = set(BINARY.values()) | set(UNARY.values())
        self.assertEqual(operators, set(expected) | {"|"})
        for op in sorted(operators):
            with self.subTest(op=op):
                scope = self.assert_one_token(op, ("keyword.operator",))
                self.assertNotIn("reserved", scope)
                if op == "|":
                    self.assertEqual(scope, "keyword.operator.moss")
                else:
                    self.assertEqual(scope, f"keyword.operator.{expected[op]}.moss")

    def test_numeric_literals(self):
        self.assertEqual(self.assert_one_token("123", ("constant.numeric",)),
                         "constant.numeric.integer.moss")
        self.assertEqual(self.assert_one_token("3.14", ("constant.numeric",)),
                         "constant.numeric.decimal.moss")
        # `1.add(2)` is a method call on `1`: the `.` is an accessor.
        self.assertEqual(
            tokenize("1.add(2)"),
            [
                ("1", "constant.numeric.integer.moss"),
                (".", "punctuation.accessor.moss"),
                ("add", "entity.name.function.moss"),
                ("(", "punctuation.section.group.moss"),
                ("2", "constant.numeric.integer.moss"),
                (")", "punctuation.section.group.moss"),
            ],
        )
        self.assertEqual(tokenize("x2")[0], ("x2", "variable.other.moss"))

    def test_exponents(self):
        """D68: an exponent makes a decimal literal, point or no point."""
        for literal in ("1e9", "1e-9", "1E3", "1.5e+3", "2.5E10", "6.02E23"):
            with self.subTest(literal=literal):
                self.assertEqual(
                    self.assert_one_token(literal, ("constant.numeric",)),
                    "constant.numeric.decimal.moss",
                )
        # `1.e5` is `1`, an accessor and a name, as the lexer has it.
        self.assertEqual(
            tokenize("1.e5")[:2],
            [("1", "constant.numeric.integer.moss"), (".", "punctuation.accessor.moss")],
        )
        self.assertEqual(
            tokenize("1e5.abs()")[:2],
            [("1e5", "constant.numeric.decimal.moss"), (".", "punctuation.accessor.moss")],
        )

    def test_the_lexer_and_the_grammar_split_alike(self):
        """Every token the lexer makes of a line, the grammar makes too."""
        from mossc.lex import lex

        line = "if a << 2 >= -b.c & !d { x = 3.14 * y - 1e-9; } else { 1.add(2.5E+3) }"
        lexed = [t.text for t in lex(line)[:-1]]
        self.assertEqual([text for text, _ in tokenize(line)], lexed)


class TestMethodsAndVals(unittest.TestCase):
    """A method is highlighted as a function wherever the syntax says it is
    one, not only where it is called."""

    def scopes(self, line):
        return [(text, scope) for text, scope in tokenize(line) if text.strip(".,;:=[]()")]

    def scope(self, line, name):
        found = [scope for text, scope in self.scopes(line) if text == name]
        self.assertEqual(len(found), 1, f"{name!r} in {line!r}: {self.scopes(line)}")
        return found[0]

    def test_a_method_named_through_its_type(self):
        for line, name in [
            ("context Arith = Value, Value.add, Value.sub;", "add"),
            ("bind Bool.not = Bool.flip;", "not"),
            ("bind Bool.not = Bool.flip;", "flip"),
            ("functor Heap: Wasm, Boxed -> Boxed.boxed, Ref.deref;", "deref"),
            ("assume Std, T, W.m {", "m"),
            ("Value.add(x)", "add"),
        ]:
            with self.subTest(line=line, name=name):
                self.assertEqual(self.scope(line, name), "entity.name.function.moss")

    def test_a_detached_method(self):
        for line, name in [
            ('import "num.moss" use .eq, .ne;', "eq"),
            ('import "num.moss" use .eq, .ne;', "ne"),
            ("context Ast = ExprRef, .data, add;", "data"),
            ("bind A.m[T=B] = .n;", "n"),
            ("    .read,", "read"),
        ]:
            with self.subTest(line=line, name=name):
                self.assertEqual(self.scope(line, name), "entity.name.function.moss")

    def test_a_field_is_not_a_method(self):
        self.assertEqual(self.scope("let y = x.f;", "f"), "variable.other.moss")
        self.assertEqual(self.scope("let y = x.f(1);", "f"), "entity.name.function.moss")
        self.assertEqual(self.scope("let y = 2.5;", "2.5"), "constant.numeric.decimal.moss")

    def test_a_val_is_declared_as_a_constant(self):
        for line, name in [("val A: Char;", "A"), ("val true: Bool = Bool (True);", "true")]:
            with self.subTest(line=line):
                self.assertEqual(self.scope(line, name), "entity.name.constant.moss")


if __name__ == "__main__":
    unittest.main()
