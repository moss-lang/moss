"""CLI: `python -m mossc {lex|parse|run} FILE`."""

import sys
from pathlib import Path

from . import ast, collect, interp
from .lex import LexError, lex, position
from .lower import Lower, LowerError
from .parse import ParseError, error_message, parse

PRELUDE = str(Path(__file__).resolve().parents[2] / "lib/prelude.moss")


def run(path: str) -> int:
    program = collect.load(path, prelude=PRELUDE)
    lower = Lower(program)
    lower.run()
    interp.run_main(program, lower)
    return 0


def main() -> int:
    if len(sys.argv) != 3 or sys.argv[1] not in ("lex", "parse", "run"):
        print("usage: python -m mossc {lex|parse|run} FILE", file=sys.stderr)
        return 2
    command, path = sys.argv[1], sys.argv[2]
    try:
        if command == "run":
            return run(path)
        with open(path, encoding="utf-8") as f:
            source = f.read()
        if command == "lex":
            for token in lex(source):
                line, col = position(source, token.offset)
                print(f"{line}:{col}\t{token.kind.name}\t{token.text}")
        else:
            print(ast.dump(parse(source)))
    except LexError as e:
        with open(path, encoding="utf-8") as f:
            source = f.read()
        line, col = position(source, e.offset)
        print(f"{path}:{line}:{col}: {e.message}", file=sys.stderr)
        return 1
    except ParseError as e:
        print(f"{path}: {e.message}", file=sys.stderr)
        return 1
    except (collect.CollectError, LowerError, interp.LinkError, interp.MossPanic) as e:
        print(e, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
