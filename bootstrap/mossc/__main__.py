"""CLI: `python -m mossc lex FILE` or `python -m mossc parse FILE`."""

import sys

from . import ast
from .lex import LexError, lex, position
from .parse import ParseError, error_message, parse


def main() -> int:
    if len(sys.argv) != 3 or sys.argv[1] not in ("lex", "parse"):
        print("usage: python -m mossc {lex|parse} FILE", file=sys.stderr)
        return 2
    command, path = sys.argv[1], sys.argv[2]
    with open(path, encoding="utf-8") as f:
        source = f.read()
    try:
        if command == "lex":
            for token in lex(source):
                line, col = position(source, token.offset)
                print(f"{line}:{col}\t{token.kind.name}\t{token.text}")
        else:
            print(ast.dump(parse(source)))
    except LexError as e:
        line, col = position(source, e.offset)
        print(f"{path}:{line}:{col}: {e.message}", file=sys.stderr)
        return 1
    except ParseError as e:
        print(f"{path}:{error_message(source, e)}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
