"""Exercise proof checking and dependency invalidation with the real Rocq compiler."""

import os
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[2]


@unittest.skipUnless(shutil.which("rocq"), "run inside nix develop .#bootstrap")
class QuickCheckTests(unittest.TestCase):
    def setUp(self):
        scratch = ROOT / "scratch"
        scratch.mkdir(exist_ok=True)
        self.temporary = tempfile.TemporaryDirectory(prefix="quick-check-test-", dir=scratch)
        self.addCleanup(self.temporary.cleanup)
        self.project = pathlib.Path(self.temporary.name)
        bootstrap = self.project / "bootstrap"
        self.sources = bootstrap / "theories"
        self.sources.mkdir(parents=True)
        self.script = bootstrap / "quick-check.py"
        shutil.copyfile(ROOT / "bootstrap" / "quick-check.py", self.script)
        self.write("Dependency", "Definition answer : nat := 0.\n")
        self.write("Target", "From NM Require Import Dependency.\n"
                   "Lemma answer_correct : answer = 0. Proof. reflexivity. Qed.\n")

    def write(self, module, source):
        (self.sources / f"{module}.v").write_text(source)

    def run_check(self, success=True, environment=None):
        result = subprocess.run(
            [sys.executable, str(self.script), "Target"],
            capture_output=True,
            text=True,
            env=environment,
        )
        output = result.stdout + result.stderr
        if success:
            self.assertEqual(result.returncode, 0, output)
        else:
            self.assertNotEqual(result.returncode, 0, output)
        return output

    def test_valid_proof_and_warm_cache(self):
        self.assertIn("vok: Target (", self.run_check())
        self.assertIn("vok: Target (unchanged)", self.run_check())

    def test_invalid_proof_is_rejected_repeatedly(self):
        self.run_check()
        self.write("Target", "Lemma wrong : False. Proof. exact I. Qed.\n")
        first_failure = self.run_check(success=False)
        self.assertIn("vos: Target", first_failure)
        self.assertIn("Target failed", first_failure)
        self.assertIn("Target failed", self.run_check(success=False))

    def test_dependency_change_invalidates_checked_target(self):
        self.run_check()
        self.write("Dependency", "Definition answer : nat := 1.\n")
        self.assertIn("Target failed", self.run_check(success=False))
        self.write("Dependency", "Definition answer : nat := 0.\n")
        self.assertNotIn("vok: Target (unchanged)", self.run_check())

    def test_deleted_dependency_cannot_be_loaded_from_cache(self):
        self.run_check()
        (self.sources / "Dependency.v").unlink()
        self.run_check(success=False)

    def test_library_search_change_invalidates_cache(self):
        self.run_check()
        extra_libraries = self.project / "extra-libraries"
        extra_libraries.mkdir()
        environment = dict(os.environ, ROCQPATH=str(extra_libraries))
        output = self.run_check(environment=environment)
        self.assertIn("vos: Dependency", output)
        self.assertNotIn("vok: Target (unchanged)", output)

    def test_dependency_discovery_errors_show_diagnostics(self):
        self.write("Target", 'Require Import "unterminated.\n')
        output = self.run_check(success=False)
        self.assertIn("Error", output)
        self.assertIn("Syntax error", output)
        self.assertNotIn("vos: Target", output)

    def test_mutable_external_library_invalidates_cache(self):
        libraries = self.project / "external-libraries"
        external = libraries / "Vendor" / "External.v"
        external.parent.mkdir(parents=True)
        self.write("Target", "From Vendor Require Import External.\n"
                   "Lemma answer_correct : external_answer = 0. Proof. reflexivity. Qed.\n")

        for variable in ("ROCQPATH", "COQPATH"):
            for mode in ("vo", "vos"):
                with self.subTest(variable=variable, mode=mode):
                    environment = dict(os.environ, **{variable: str(libraries)})
                    for other in ("ROCQPATH", "COQPATH"):
                        if other != variable:
                            environment.pop(other, None)

                    def compile_external(answer):
                        external.write_text(f"Definition external_answer : nat := {answer}.\n")
                        for suffix in (".vo", ".vos", ".vok"):
                            external.with_suffix(suffix).unlink(missing_ok=True)
                        command = ["rocq", "c", "-q", "-no-glob", "-Q", str(libraries), ""]
                        if mode == "vos":
                            command.append("-vos")
                        result = subprocess.run(
                            command + [str(external)], capture_output=True, text=True,
                            env=environment,
                        )
                        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                        marker = external.with_suffix(".vos")
                        self.assertTrue(marker.exists())
                        if mode == "vo":
                            self.assertEqual(marker.stat().st_size, 0)
                        else:
                            self.assertGreater(marker.stat().st_size, 0)

                    compile_external(0)
                    self.run_check(environment=environment)
                    self.assertIn(
                        "vok: Target (unchanged)", self.run_check(environment=environment)
                    )
                    compile_external(1)
                    output = self.run_check(success=False, environment=environment)
                    self.assertIn("Target failed", output)
                    self.assertNotIn("vok: Target (unchanged)", output)
                    compile_external(0)
                    self.assertNotIn(
                        "vok: Target (unchanged)", self.run_check(environment=environment)
                    )

    def test_loaded_file_change_invalidates_checked_target(self):
        loaded = self.project / "loaded.v"
        loaded.write_text("Definition loaded_answer : nat := 0.\n")
        self.write("Target", f'Load "{loaded.as_posix()}".\n'
                   "Lemma answer_correct : loaded_answer = 0. Proof. reflexivity. Qed.\n")
        self.run_check()
        self.assertIn("vok: Target (unchanged)", self.run_check())
        loaded.write_text("Definition loaded_answer : nat := 1.\n")
        output = self.run_check(success=False)
        self.assertIn("Target failed", output)
        self.assertNotIn("vok: Target (unchanged)", output)
        loaded.write_text("Definition loaded_answer : nat := 0.\n")
        self.assertNotIn("vok: Target (unchanged)", self.run_check())


if __name__ == "__main__":
    unittest.main()
