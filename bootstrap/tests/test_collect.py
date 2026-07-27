import unittest
from pathlib import Path

from mossc.collect import CollectError, SymKind, load, resolve_detached, resolve_path

REPO = Path(__file__).resolve().parents[2]


ROOT = "/moss"  # the in-memory tree's root; paths arrive canonicalized


def fake(files):
    """A read function over an in-memory file tree."""

    def read(path):
        import os

        key = os.path.relpath(path, ROOT)
        try:
            return files[key]
        except KeyError:
            raise FileNotFoundError(path)

    return read


class TestLoading(unittest.TestCase):
    def test_real_prelude_graph(self):
        program = load(str(REPO / "tests/fixtures/prelude.moss"))
        self.assertEqual(len(program.modules), 4)  # prelude, cell, inner, option
        self.assertEqual(program.order[-1], program.entry)
        exports = program.entry.export_names
        for name in ["Cell", "IsCell", "T", "None", "Option", "Some"]:
            self.assertIn(name, exports)

    def test_diamond_is_shared(self):
        program = load(str(REPO / "tests/fixtures/prelude.moss"))
        cell = program.modules[str(REPO / "tests/fixtures/cell.moss")]
        option = program.modules[str(REPO / "tests/fixtures/option.moss")]
        t_via_cell = resolve_path(cell, ["T"])
        t_via_option = resolve_path(option, ["T"])
        self.assertIs(t_via_cell, t_via_option)

    def test_cycle_error(self):
        files = {
            "a.moss": 'import "./b.moss";',
            "b.moss": 'import "./a.moss";',
        }
        with self.assertRaises(CollectError) as ctx:
            load("a.moss", read=fake(files), root=ROOT)
        self.assertIn("cycle", ctx.exception.message)

    def test_relative_paths(self):
        files = {
            "x/a.moss": 'import "../y/b.moss" use B;',
            "y/b.moss": "type B;",
        }
        program = load("x/a.moss", read=fake(files), root=ROOT)
        self.assertIn(ROOT + "/y/b.moss", program.modules)


class TestScopes(unittest.TestCase):
    def test_kinds(self):
        files = {
            "m.moss": "type A; unit U; type L = A; type G A; val v: A;"
            "fn f(); context C = A;"
            "assume A { fn A.m(): A; fn .d(): This; }"
        }
        m = load("m.moss", read=fake(files), root=ROOT).entry
        kinds = {name: s.kind for name, s in m.names.items()}
        self.assertEqual(
            kinds,
            {
                "A": SymKind.TYPE,
                "U": SymKind.UNIT,
                "L": SymKind.ALIAS,
                "G": SymKind.TAG,
                "v": SymKind.VAL,
                "f": SymKind.FN,
                "C": SymKind.CONTEXT,
            },
        )
        self.assertIn("d", m.detached)
        a = m.names["A"]
        self.assertIn((id(a), "m"), m.attached)
        self.assertIs(m.attached[(id(a), "m")].receiver, a)

    def test_assume_nesting_recorded(self):
        files = {"m.moss": "type A; type B; assume A { assume B { fn f(); } }"}
        m = load("m.moss", read=fake(files), root=ROOT).entry
        assumes = m.names["f"].assumes
        self.assertEqual([item.path for item in assumes], [["A"], ["B"]])

    def test_duplicate_definition(self):
        with self.assertRaises(CollectError):
            load("m.moss", read=fake({"m.moss": "type A; unit A;"}), root=ROOT)

    def test_detached_body_rejected(self):
        with self.assertRaises(CollectError):
            load("m.moss", read=fake({"m.moss": "fn .d() { () }"}), root=ROOT)

    def test_unknown_receiver(self):
        with self.assertRaises(CollectError):
            load("m.moss", read=fake({"m.moss": "fn A.m();"}), root=ROOT)

    def test_receiver_forward_reference(self):
        m = load(
            "m.moss",
            read=fake({"m.moss": "assume A { fn A.m(); } type A;"}),
            root=ROOT,
        ).entry
        self.assertIn((id(m.names["A"]), "m"), m.attached)


class TestImports(unittest.TestCase):
    def test_use_and_alias_and_glob(self):
        files = {
            "a.moss": "type A; fn .m();",
            "b.moss": 'import "./a.moss" as a use A, .m as .m1;',
            "c.moss": 'import "./b.moss" use *;',
        }
        program = load("c.moss", read=fake(files), root=ROOT)
        b = program.modules[ROOT + "/b.moss"]
        c = program.entry
        self.assertIn("A", b.names)
        self.assertIn("m1", b.detached)
        self.assertIs(b.detached["m1"], program.modules[ROOT + "/a.moss"].detached["m"])
        # Explicit uses and aliases re-export; c gets them via glob.
        self.assertIn("A", c.names)
        self.assertIn("m1", c.detached)
        self.assertIn("a", c.aliases)

    def test_glob_does_not_reexport(self):
        files = {
            "a.moss": "type A;",
            "b.moss": 'import "./a.moss" use *;',
            "c.moss": 'import "./b.moss" use *;',
        }
        c = load("c.moss", read=fake(files), root=ROOT).entry
        self.assertNotIn("A", c.names)

    def test_collision_is_error(self):
        files = {
            "a.moss": "fn .m();",
            "b.moss": "fn .m();",
            "c.moss": 'import "./a.moss" use .m;\nimport "./b.moss" use .m;',
        }
        with self.assertRaises(CollectError) as ctx:
            load("c.moss", read=fake(files), root=ROOT)
        self.assertIn("two different symbols", ctx.exception.message)

    def test_same_symbol_twice_is_fine(self):
        files = {
            "a.moss": "type A;",
            "b.moss": 'import "./a.moss" use A;',
            "c.moss": 'import "./a.moss" use A;\nimport "./b.moss" use A;',
        }
        c = load("c.moss", read=fake(files), root=ROOT).entry
        self.assertIn("A", c.names)

    def test_rename_avoids_collision(self):
        files = {
            "a.moss": "fn .m();",
            "b.moss": "fn .m();",
            "e.moss": 'import "./a.moss" use .m as .m1;\nimport "./b.moss" use .m as .m2;',
        }
        e = load("e.moss", read=fake(files), root=ROOT).entry
        self.assertIn("m1", e.detached)
        self.assertIn("m2", e.detached)

    def test_missing_export(self):
        files = {"a.moss": "type A;", "b.moss": 'import "./a.moss" use B;'}
        with self.assertRaises(CollectError):
            load("b.moss", read=fake(files), root=ROOT)

    def test_qualified_detached(self):
        files = {
            "a.moss": "fn .m();",
            "d.moss": 'import "./a.moss" as a;',
        }
        d = load("d.moss", read=fake(files), root=ROOT).entry
        symbol = resolve_detached(d, ["a", "m"])
        self.assertIsNotNone(symbol)
        self.assertEqual(symbol.name, ".m")


class TestPrelude(unittest.TestCase):
    FILES = {
        "lib/prelude.moss": 'import "./std.moss" use Std, putchar;',
        "lib/std.moss": "type Char; assume Char { fn putchar(c: Char); } context Std = Char, putchar;",
        "hello.moss": "assume Std { fn main() { } }",
    }

    def test_auto_import(self):
        program = load("hello.moss", read=fake(self.FILES), prelude="lib/prelude.moss", root=ROOT)
        self.assertIn("Std", program.entry.names)
        self.assertIn("putchar", program.entry.names)

    def test_lib_files_skip_auto_import(self):
        program = load("hello.moss", read=fake(self.FILES), prelude="lib/prelude.moss", root=ROOT)
        std = program.modules[ROOT + "/lib/std.moss"]
        self.assertNotIn("Std", std.aliases)
        self.assertEqual(len(std.tree.imports), 0)


if __name__ == "__main__":
    unittest.main()
