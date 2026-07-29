"""Lexer for the MVP token set of docs/reference/syntax.md."""

from dataclasses import dataclass
from enum import Enum, auto


class Kind(Enum):
    # One-character symbols.
    EXCLAM = auto()
    PERCENT = auto()
    AMPERSAND = auto()
    LPAREN = auto()
    RPAREN = auto()
    STAR = auto()
    PLUS = auto()
    COMMA = auto()
    HYPHEN = auto()
    HYPHEN_GREATER = auto()
    DOT = auto()
    SLASH = auto()
    COLON = auto()
    SEMI = auto()
    LESS = auto()
    EQUAL = auto()
    GREATER = auto()
    LBRACKET = auto()
    RBRACKET = auto()
    CARET = auto()
    LBRACE = auto()
    PIPE = auto()
    RBRACE = auto()
    # Two-character symbols.
    EXCLAM_EQUAL = auto()
    COLON_COLON = auto()
    EQUAL_GREATER = auto()
    LESS_LESS = auto()
    LESS_EQUAL = auto()
    EQUAL_EQUAL = auto()
    GREATER_EQUAL = auto()
    GREATER_GREATER = auto()
    # Keywords.
    AS = auto()
    ASSUME = auto()
    BIND = auto()
    BREAK = auto()
    CONTEXT = auto()
    ELSE = auto()
    FN = auto()
    FOR = auto()
    FUNCTOR = auto()
    IF = auto()
    IMPORT = auto()
    LET = auto()
    LOOP = auto()
    MATCH = auto()
    RETURN = auto()
    THIS_TYPE = auto()  # This
    THIS = auto()  # this
    TYPE = auto()
    UNIT = auto()
    USE = auto()
    VAL = auto()
    VAR = auto()
    WHILE = auto()
    # Everything else.
    NAME = auto()
    STRING = auto()
    EOF = auto()


KEYWORDS = {
    "as": Kind.AS,
    "assume": Kind.ASSUME,
    "functor": Kind.FUNCTOR,
    "bind": Kind.BIND,
    "break": Kind.BREAK,
    "context": Kind.CONTEXT,
    "else": Kind.ELSE,
    "fn": Kind.FN,
    "for": Kind.FOR,
    "if": Kind.IF,
    "import": Kind.IMPORT,
    "let": Kind.LET,
    "loop": Kind.LOOP,
    "match": Kind.MATCH,
    "return": Kind.RETURN,
    "This": Kind.THIS_TYPE,
    "this": Kind.THIS,
    "type": Kind.TYPE,
    "unit": Kind.UNIT,
    "use": Kind.USE,
    "val": Kind.VAL,
    "var": Kind.VAR,
    "while": Kind.WHILE,
}

ONE_CHAR = {
    "!": Kind.EXCLAM,
    "%": Kind.PERCENT,
    "&": Kind.AMPERSAND,
    "(": Kind.LPAREN,
    ")": Kind.RPAREN,
    "*": Kind.STAR,
    "+": Kind.PLUS,
    ",": Kind.COMMA,
    "-": Kind.HYPHEN,
    ".": Kind.DOT,
    "/": Kind.SLASH,
    ":": Kind.COLON,
    ";": Kind.SEMI,
    "<": Kind.LESS,
    "=": Kind.EQUAL,
    ">": Kind.GREATER,
    "[": Kind.LBRACKET,
    "]": Kind.RBRACKET,
    "^": Kind.CARET,
    "{": Kind.LBRACE,
    "|": Kind.PIPE,
    "}": Kind.RBRACE,
}

TWO_CHAR = {
    "!=": Kind.EXCLAM_EQUAL,
    "::": Kind.COLON_COLON,
    "=>": Kind.EQUAL_GREATER,
    "<<": Kind.LESS_LESS,
    "<=": Kind.LESS_EQUAL,
    "==": Kind.EQUAL_EQUAL,
    ">=": Kind.GREATER_EQUAL,
    ">>": Kind.GREATER_GREATER,
    "->": Kind.HYPHEN_GREATER,
}

ESCAPES = {'"': '"', "\\": "\\", "n": "\n", "r": "\r", "t": "\t"}


@dataclass(frozen=True)
class Token:
    kind: Kind
    text: str
    offset: int

    @property
    def end(self) -> int:
        return self.offset + len(self.text)

    def string_value(self) -> str:
        assert self.kind == Kind.STRING
        out = []
        i = 1
        while i < len(self.text) - 1:
            c = self.text[i]
            if c == "\\":
                out.append(ESCAPES[self.text[i + 1]])
                i += 2
            else:
                out.append(c)
                i += 1
        return "".join(out)


def position(source: str, offset: int) -> tuple[int, int]:
    """1-based (line, column) of an offset."""
    line = source.count("\n", 0, offset) + 1
    start = source.rfind("\n", 0, offset) + 1
    return line, offset - start + 1


class LexError(Exception):
    def __init__(self, message: str, offset: int):
        super().__init__(message)
        self.message = message
        self.offset = offset


def is_name_start(c: str) -> bool:
    return c.isalpha() or c == "_"


def is_name_continue(c: str) -> bool:
    return c.isalnum() or c == "_"


def lex(source: str) -> list[Token]:
    tokens = []
    i = 0
    n = len(source)
    while i < n:
        c = source[i]
        if c in " \t\r\n":
            i += 1
        elif c == "#":
            end = source.find("\n", i)
            i = n if end == -1 else end
        elif source[i : i + 2] in TWO_CHAR:
            tokens.append(Token(TWO_CHAR[source[i : i + 2]], source[i : i + 2], i))
            i += 2
        elif c in ONE_CHAR:
            tokens.append(Token(ONE_CHAR[c], c, i))
            i += 1
        elif is_name_start(c):
            j = i + 1
            while j < n and is_name_continue(source[j]):
                j += 1
            text = source[i:j]
            tokens.append(Token(KEYWORDS.get(text, Kind.NAME), text, i))
            i = j
        elif c == '"':
            j = i + 1
            while True:
                if j >= n or source[j] == "\n":
                    raise LexError("unclosed string", i)
                if source[j] == '"':
                    break
                if source[j] == "\\":
                    if j + 1 >= n or source[j + 1] not in ESCAPES:
                        raise LexError("invalid escape sequence", j)
                    j += 2
                else:
                    j += 1
            tokens.append(Token(Kind.STRING, source[i : j + 1], i))
            i = j + 1
        else:
            raise LexError(f"unexpected character {c!r}", i)
    tokens.append(Token(Kind.EOF, "", n))
    return tokens
