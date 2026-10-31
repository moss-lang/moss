import unittest

from mossc.lex import Kind, LexError, lex, position


def kinds(source):
    return [t.kind for t in lex(source)]


class TestLex(unittest.TestCase):
    def test_empty(self):
        self.assertEqual(kinds(""), [Kind.EOF])

    def test_symbols_maximal_munch(self):
        self.assertEqual(
            kinds("== = => :: : <= <<"),
            [
                Kind.EQUAL_EQUAL,
                Kind.EQUAL,
                Kind.EQUAL_GREATER,
                Kind.COLON_COLON,
                Kind.COLON,
                Kind.LESS_EQUAL,
                Kind.LESS_LESS,
                Kind.EOF,
            ],
        )

    def test_adjacent_symbols(self):
        # `===` is `==` then `=`; `=>>` is `=>` then `>`.
        self.assertEqual(kinds("==="), [Kind.EQUAL_EQUAL, Kind.EQUAL, Kind.EOF])
        self.assertEqual(kinds("=>>"), [Kind.EQUAL_GREATER, Kind.GREATER, Kind.EOF])

    def test_keywords_and_names(self):
        tokens = lex("fn foo(this: This) _bar2")
        self.assertEqual(
            [t.kind for t in tokens],
            [
                Kind.FN,
                Kind.NAME,
                Kind.LPAREN,
                Kind.THIS,
                Kind.COLON,
                Kind.THIS_TYPE,
                Kind.RPAREN,
                Kind.NAME,
                Kind.EOF,
            ],
        )
        self.assertEqual(tokens[1].text, "foo")
        self.assertEqual(tokens[7].text, "_bar2")

    def test_keyword_prefix_is_a_name(self):
        self.assertEqual(kinds("iffy formal thistle"), [Kind.NAME] * 3 + [Kind.EOF])

    def test_comments_and_shebang(self):
        source = "#!/usr/bin/env moss\nunit X; # trailing\n# whole line\n"
        self.assertEqual(kinds(source), [Kind.UNIT, Kind.NAME, Kind.SEMI, Kind.EOF])

    def test_string(self):
        token = lex('import "a\\n\\"b\\\\";')[1]
        self.assertEqual(token.kind, Kind.STRING)
        self.assertEqual(token.string_value(), 'a\n"b\\')

    def test_unclosed_string(self):
        with self.assertRaises(LexError):
            lex('import "abc;')
        with self.assertRaises(LexError):
            lex('import "abc\n";')

    def test_bad_escape(self):
        with self.assertRaises(LexError):
            lex('import "a\\q";')

    def test_unexpected_character(self):
        with self.assertRaises(LexError) as ctx:
            lex("unit X;\n@")
        self.assertEqual(position("unit X;\n@", ctx.exception.offset), (2, 1))

    def test_numeric_literals(self):
        # D68: a digit starts a literal, and still continues a name.
        tokens = lex("x2 123 0 007 3.14 0.5")
        self.assertEqual(
            [(t.kind, t.text) for t in tokens[:-1]],
            [
                (Kind.NAME, "x2"),
                (Kind.INTEGER, "123"),
                (Kind.INTEGER, "0"),
                (Kind.INTEGER, "007"),
                (Kind.DECIMAL, "3.14"),
                (Kind.DECIMAL, "0.5"),
            ],
        )

    def test_a_dot_not_followed_by_a_digit_is_not_a_point(self):
        # `1.add(2)` calls a method on `1`.
        self.assertEqual(
            kinds("1.add(2)"),
            [Kind.INTEGER, Kind.DOT, Kind.NAME, Kind.LPAREN, Kind.INTEGER, Kind.RPAREN, Kind.EOF],
        )
        self.assertEqual(kinds("1.x"), [Kind.INTEGER, Kind.DOT, Kind.NAME, Kind.EOF])
        self.assertEqual(kinds("1._"), [Kind.INTEGER, Kind.DOT, Kind.NAME, Kind.EOF])
        self.assertEqual(kinds("1."), [Kind.INTEGER, Kind.DOT, Kind.EOF])
        self.assertEqual(kinds("1.;"), [Kind.INTEGER, Kind.DOT, Kind.SEMI, Kind.EOF])
        # ...and after a decimal literal, the next `.` is a method call.
        self.assertEqual(
            kinds("2.5.abs()"),
            [Kind.DECIMAL, Kind.DOT, Kind.NAME, Kind.LPAREN, Kind.RPAREN, Kind.EOF],
        )

    def test_no_suffixes(self):
        for source in ("2x", "2_", "1.5f", "0x10", "1e5"):
            with self.subTest(source=source), self.assertRaises(LexError) as ctx:
                lex(source)
            self.assertIn("no suffix", ctx.exception.message)

    def test_no_bounds(self):
        """D68: a literal is only arithmetic over the context's symbols, so
        nothing about its size is the lexer's business — not even digits
        past what an f64 can hold, nor an integer past any `Int`."""
        for source in (
            "3.14159265358979323846264338327950288",
            "0.00000000000000000000000001",
            "123456789012345678901234567890",
        ):
            with self.subTest(source=source):
                self.assertEqual(len(lex(source)), 2)

    def test_positions(self):
        source = "unit X;\nunit Y;\n"
        tokens = lex(source)
        self.assertEqual(position(source, tokens[3].offset), (2, 1))
        self.assertEqual(position(source, tokens[4].offset), (2, 6))


if __name__ == "__main__":
    unittest.main()
