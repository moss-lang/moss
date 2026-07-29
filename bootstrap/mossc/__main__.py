"""Bootstrap compiler protocol: `python -m mossc FILE` writes Wasm."""

import os
import sys
from pathlib import Path

from . import collect
from .lex import LexError
from .lower import Lower, LowerError
from .parse import ParseError

PRELUDE = str(
    Path(os.environ.get("MOSS_LIB", Path(__file__).resolve().parents[2] / "lib"))
    / "prelude.moss"
)


def compile_path(path: str) -> int:
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
    if len(sys.argv) != 2:
        print("usage: python -m mossc FILE", file=sys.stderr)
        return 2
    path = sys.argv[1]
    try:
        return compile_path(path)
    except LexError as e:
        with open(path, encoding="utf-8") as f:
            source = f.read()
        from .lex import position

        line, col = position(source, e.offset)
        print(f"{path}:{line}:{col}: {e.message}", file=sys.stderr)
        return 1
    except ParseError as e:
        print(f"{path}: {e.message}", file=sys.stderr)
        return 1
    except (collect.CollectError, LowerError) as e:
        print(e, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
