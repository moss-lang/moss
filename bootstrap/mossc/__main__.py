"""CLI: `python -m mossc {lex|parse|run} FILE`."""

import sys
from pathlib import Path

from . import ast, collect, interp
from .lex import LexError, lex, position
from .lower import Lower, LowerError
from .parse import ParseError, error_message, parse

_PRELUDE_ABS = Path(__file__).resolve().parents[2] / "lib/prelude.moss"
try:
    # Relative to the working directory when possible, so that the prelude
    # and a user file's own `../lib/...` import name the same module: the
    # loader keys modules by path, and two spellings would load twice.
    PRELUDE = str(_PRELUDE_ABS.relative_to(Path.cwd()))
except ValueError:
    PRELUDE = str(_PRELUDE_ABS)


def run(path: str, args: list) -> int:
    program = collect.load(path, prelude=PRELUDE)
    lower = Lower(program)
    lower.run()
    interp.run_main(program, lower, args)
    return 0


def build_cmd(path: str) -> int:
    from . import build as build_mod
    from .collect import SymKind

    program = collect.load(path, prelude=PRELUDE)
    lower = Lower(program)
    lower.run()
    main_sym = program.entry.names.get("main")
    if main_sym is None or main_sym.kind != SymKind.FN or main_sym.decl.body is None:
        print(f"{path}: no `main` function", file=sys.stderr)
        return 1
    sys.stdout.buffer.write(build_mod.build(program, lower, main_sym))
    return 0


def main() -> int:
    if len(sys.argv) < 3 or sys.argv[1] not in ("lex", "parse", "run", "build"):
        print("usage: python -m mossc {lex|parse|run|build} FILE", file=sys.stderr)
        return 2
    command, path = sys.argv[1], sys.argv[2]
    try:
        if command == "run":
            return run(path, sys.argv[3:])
        if command == "build":
            return build_cmd(path)
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
