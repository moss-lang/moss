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

    def test_no_numeric_literals(self):
        # Digits can continue a name but cannot start a token.
        self.assertEqual(kinds("x2"), [Kind.NAME, Kind.EOF])
        with self.assertRaises(LexError):
            lex("2x")

    def test_positions(self):
        source = "unit X;\nunit Y;\n"
        tokens = lex(source)
        self.assertEqual(position(source, tokens[3].offset), (2, 1))
        self.assertEqual(position(source, tokens[4].offset), (2, 6))


if __name__ == "__main__":
    unittest.main()
