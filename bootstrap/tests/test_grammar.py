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
        (re.compile(p.get("match", p.get("begin"))), scope_of(p))
        for p in flatten(grammar, grammar["patterns"])
    ]
    out = []
    pos = 0
    while pos < len(text):
        best = None
        for regex, scope in patterns:
            m = regex.search(text, pos)
            if m is not None and m.end() > m.start():
                if best is None or m.start() < best[0].start():
                    best = (m, scope)
        if best is None:
            break
        m, scope = best
        if text[pos : m.start()].strip():
            out.append((text[pos : m.start()], None))
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

    def test_the_lexer_and_the_grammar_split_alike(self):
        """Every token the lexer makes of a line, the grammar makes too."""
        from mossc.lex import lex

        line = "if a << 2 >= -b.c & !d { x = 3.14 * y; } else { 1.add(2) }"
        lexed = [t.text for t in lex(line)[:-1]]
        self.assertEqual([text for text, _ in tokenize(line)], lexed)


if __name__ == "__main__":
    unittest.main()
