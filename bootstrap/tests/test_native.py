import unittest
from pathlib import Path

from mossc.collect import load
from mossc.native import CHARS, gen_char_moss, gen_wasichar_moss

REPO = Path(__file__).resolve().parents[2]


class TestNativeTables(unittest.TestCase):
    def test_char_moss_is_generated_from_chars(self):
        generated = gen_char_moss()
        on_disk = (REPO / "lib/char.moss").read_text(encoding="utf-8")
        self.assertEqual(on_disk, generated, "regenerate lib/char.moss from mossc.native")

    def test_wasichar_moss_is_generated_from_chars(self):
        generated = gen_wasichar_moss()
        on_disk = (REPO / "lib/wasichar.moss").read_text(encoding="utf-8")
        self.assertEqual(
            on_disk, generated, "regenerate lib/wasichar.moss from mossc.native"
        )

    def test_chars_values_are_single_chars(self):
        for name, value in CHARS.items():
            self.assertEqual(len(value), 1, name)

    def test_prelude_graph_loads(self):
        program = load(str(REPO / "lib/prelude.moss"))
        exports = program.entry.export_names
        self.assertEqual(
            sorted(exports),
            [
                "Bool",
                "CellInt",
                "Char",
                "False",
                "Int",
                "IntList",
                "Path",
                "Std",
                "StrList",
                "String",
                "True",
                "arg_at",
                "arg_count",
                "cell_int",
                "false",
                "first_arg",
                "int_list",
                "one",
                "print",
                "put_bytes",
                "putchar",
                "pwd",
                "str_list",
                "true",
                "zero",
            ],
        )
        self.assertIn("char", program.entry.export_aliases)
        char = program.entry.export_aliases["char"]
        for name in CHARS:
            self.assertIn(name, char.export_names)


if __name__ == "__main__":
    unittest.main()
