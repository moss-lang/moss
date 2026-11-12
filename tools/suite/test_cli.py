"""The native launcher, exercised through its installed command line."""

import os
import shutil
import subprocess
from pathlib import Path

from build import dev_launcher
from suite.support import compiler, fresh, once, output, repo, std_main


@once
def launcher():
    if path := os.getenv("MOSS_LAUNCHER"):
        return Path(path)
    return dev_launcher(repo() / "target")


def invoke(*args, cwd, overrides=None, needs_compiler=True):
    environment = os.environ.copy()
    environment.pop("MOSS_ROOT", None)
    environment.pop("MOSS_COMPILER", None)
    if not os.getenv("MOSS_LAUNCHER"):
        environment["MOSS_ROOT"] = str(repo())
        if needs_compiler:
            environment["MOSS_COMPILER"] = str(compiler())
    environment.update(overrides or {})
    return output([launcher(), *args], cwd=cwd, env=environment)


def directory_with_hello():
    directory = fresh("cli")
    directory.mkdir()
    shutil.copyfile(repo() / "examples/hello.moss", directory / "hello.moss")
    return directory


def test_help_and_version():
    cwd = directory_with_hello()
    for args in (("--help",), ("run", "-h"), ("build", "--help")):
        out = invoke(*args, cwd=cwd, needs_compiler=False)
        assert out.code == 0 and "Usage:" in out.text(), out
    revision = os.getenv("MOSS_REV")
    if not revision:
        revision = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=repo(), text=True
        ).strip()
    for flag in ("--version", "-V"):
        out = invoke(flag, cwd=cwd, needs_compiler=False)
        assert (out.code, out.text().strip()) == (
            0,
            f"moss https://github.com/moss-lang/moss/tree/{revision}",
        )


def test_compile_run_and_optimize():
    cwd = directory_with_hello()
    for args in (("-O3", "hello.moss"), ("run", "-Os", "hello.moss"), ("run", "hello.moss")):
        out = invoke(*args, cwd=cwd)
        assert (out.code, out.text()) == (0, "Hello, world!\n"), out
    for level in ("-Oz", "-O0"):
        out = invoke("build", "hello.moss", level, "-o", "hello.wasm", cwd=cwd)
        assert out.code == 0, out
        subprocess.run(["wasm-tools", "validate", cwd / "hello.wasm"], check=True)
    out = invoke("-O", "hello.moss", cwd=cwd, needs_compiler=False)
    assert out.code == 2, out


def test_program_arguments_are_moss_arguments():
    cwd = directory_with_hello()
    (cwd / "args.moss").write_text(
        std_main("""assume Std {
  fn main() {
    var i = 1;
    while i < arg_count() {
      print(arg_at(i));
      putchar(char::newline);
      i = i + 1;
    }
  }
}
"""),
        encoding="utf-8",
    )
    out = invoke("run", "args.moss", "--", "-O3", "--", "é space", cwd=cwd)
    assert (out.code, out.text()) == (0, "-O3\n--\né space\n"), out


def test_the_installed_prelude_belongs_to_the_compiler():
    cwd = directory_with_hello()
    (cwd / "other/lib").mkdir(parents=True)
    (cwd / "other/src").mkdir()
    (cwd / "other/lib/prelude.moss").write_text("this is not Moss\n")
    shutil.copyfile(cwd / "hello.moss", cwd / "other/src/hello.moss")
    out = invoke("other/src/hello.moss", cwd=cwd)
    assert (out.code, out.text()) == (0, "Hello, world!\n"), out


def test_human_diagnostics_are_printed_with_host_paths():
    cwd = directory_with_hello()
    (cwd / "invalid.moss").write_text("fn main() {\n  missing();\n  missing();\n}\n")
    out = invoke("build", "invalid.moss", "-o", "invalid.wasm", cwd=cwd)
    assert out.code == 1 and not (cwd / "invalid.wasm").exists(), out
    headline = "error: this name is not in scope: `missing`"
    lines = out.stderr.splitlines()
    assert lines[0] == headline and lines.count(headline) == 2, out
    assert "  --> invalid.moss:2:3" in lines and "  --> invalid.moss:3:3" in lines, out
    assert not any(line.startswith(("moss:", "error[")) for line in lines), out


def stand_in(name, cwd):
    path = cwd / (name + ".wasm")
    subprocess.run(
        ["wasm-tools", "parse", repo() / f"tests/cli/{name}.wat", "-o", path], check=True
    )
    return {"MOSS_COMPILER": str(path)}


def test_an_invalid_compiler_module_is_never_written_or_run():
    cwd = directory_with_hello()
    overrides = stand_in("invalid-output", cwd)
    for flags in ((), ("-O0",), ("-O3",)):
        out = invoke(
            *flags,
            "build",
            "hello.moss",
            "-o",
            "out.wasm",
            cwd=cwd,
            overrides=overrides,
            needs_compiler=False,
        )
        assert out.code == 1 and not (cwd / "out.wasm").exists(), out
        assert "function params size is out of bounds" in out.stderr, out
        assert "This is a compiler bug" in out.stderr, out
    out = invoke("hello.moss", cwd=cwd, overrides=overrides, needs_compiler=False)
    assert out.code == 1 and "This is a compiler bug" in out.stderr, out


def test_the_compiler_gets_a_read_only_preopen():
    cwd = directory_with_hello()
    overrides = stand_in("compiler-write", cwd) | {"MOSS_ROOT": str(cwd)}
    out = invoke("hello.moss", cwd=cwd, overrides=overrides, needs_compiler=False)
    assert out.code == 1 and "compiler exited with status" in out.stderr, out
    assert not (cwd / "created").exists(), out
