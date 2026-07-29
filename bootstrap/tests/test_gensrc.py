import unittest
from pathlib import Path

from mossc import build, gensrc

REPO = Path(__file__).resolve().parents[2]


class TestGeneratedSources(unittest.TestCase):
    """The three table files under src/ are generated. If one drifts from
    its generator the compiler still builds, and quietly compiles the
    wrong instruction — so the files are checked, not just produced."""

    def test_files_match_their_generators(self):
        for rel, generate in gensrc.GENERATED.items():
            with self.subTest(file=rel):
                on_disk = (REPO / rel).read_text(encoding="utf-8")
                self.assertEqual(
                    on_disk,
                    generate(),
                    f"regenerate {rel}: python3 -m mossc.gensrc",
                )

    def test_opcodes_agree_with_the_python_back_end(self):
        """Both back ends select the same instruction for the same `Wasm`
        item. The self-hosted table carries an opcode, an encoding and an
        alignment where build.py carries the assembled bytes, so the
        comparison reassembles them."""
        for name, (code, kind, align, _res) in gensrc.wasm_ops().items():
            if kind in (gensrc.CONST0, gensrc.CONST1):
                continue  # values, not instructions; build.py has no entry
            with self.subTest(op=name):
                expected = build.WASM_OPS[name]
                if kind == gensrc.SIMPLE:
                    actual = bytes([code])
                elif kind == gensrc.MEMARG:
                    actual = bytes([code]) + build.uleb(align) + build.uleb(0)
                elif kind == gensrc.MEMZERO:
                    actual = bytes([code, 0])
                elif kind == gensrc.MEMCOPY:
                    actual = bytes([code, 10, 0, 0])
                else:
                    actual = bytes([code, 11, 0])
                self.assertEqual(actual, expected)

    def test_every_wasm_item_has_an_instruction(self):
        """Every function and val of the `Wasm` context is in the table,
        so the self-hosted back end never meets one it cannot select."""
        source = (REPO / "lib/wasm.moss").read_text(encoding="utf-8")
        declared = set()
        for line in source.splitlines():
            line = line.strip()
            if line.startswith("fn ") and "(" in line:
                declared.add(line[3:].split("(")[0].strip())
            elif line.startswith("val ") and ":" in line:
                declared.add(line[4:].split(":")[0].strip())
        self.assertTrue(declared)
        self.assertEqual(declared - set(gensrc.wasm_ops()), set())


if __name__ == "__main__":
    unittest.main()
