import unittest
from pathlib import Path

from mossc import collect, interp
from mossc.lex import LexError, position
from mossc.lower import Lower, LowerError
from mossc.parse import ParseError

REPO = Path(__file__).resolve().parents[2]
CASES = sorted(p.name for p in (REPO / "tests/errors").glob("*.moss"))


def diagnose(rel: str) -> str:
    """The one-line diagnostic the pipeline produces for a bad program,
    with repo-relative paths so the goldens are stable."""

    def read(path):
        return (REPO / path).read_text(encoding="utf-8")

    try:
        program = collect.load(rel, read=read, prelude="lib/prelude.moss")
        lower = Lower(program)
        lower.run()
        interp.run_main(program, lower, [])
    except LexError as e:
        line, col = position(read(rel), e.offset)
        return f"{rel}:{line}:{col}: {e.message}"
    except ParseError as e:
        line, col = position(read(rel), e.token.offset)
        return f"{rel}:{line}:{col}: {e.message}"
    except (collect.CollectError, LowerError, interp.LinkError) as e:
        return str(e)
    raise AssertionError(f"{rel} unexpectedly succeeded")


class TestErrorGoldens(unittest.TestCase):
    def test_goldens(self):
        for name in CASES:
            with self.subTest(case=name):
                got = diagnose(f"tests/errors/{name}")
                golden = REPO / "tests/errors/stderr" / (Path(name).stem + ".txt")
                self.assertEqual(got + "\n", golden.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()


def regenerate():
    """python3 -c 'from tests.test_errors import regenerate; regenerate()'"""
    for name in CASES:
        golden = REPO / "tests/errors/stderr" / (Path(name).stem + ".txt")
        golden.write_text(diagnose(f"tests/errors/{name}") + "\n", encoding="utf-8")
        print(golden)
