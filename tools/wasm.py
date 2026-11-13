"""WebAssembly tool commands and the features Moss targets."""

import subprocess
from functools import cache
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
PRELUDE = "lib/prelude.moss"
COMPILER = "src/main.moss"


@cache
def feature_rows():
    return tuple(
        line.split()
        for line in (ROOT / "wasm-features.txt").read_text().splitlines()
        if line.strip() and not line.lstrip().startswith("#")
    )


def binaryen_flags():
    return ["-mvp", *("--enable-" + row[0] for row in feature_rows())]


def validator_flags():
    return "--features=" + ",".join(["-all", "mvp", *(row[1] for row in feature_rows())])


def run(*args, cwd=None, output=None):
    if output is None:
        subprocess.run(args, cwd=cwd, check=True)
    else:
        with output.open("wb") as stream:
            subprocess.run(args, cwd=cwd, stdout=stream, check=True)


def validate(module):
    run("wasm-tools", "validate", validator_flags(), module)


def optimize(source, output):
    validate(source)
    run("wasm-opt", *binaryen_flags(), "-O3", source, "-o", output)


def self_compile(source, compiler, output):
    run(
        "wasmtime",
        "run",
        "--argv0",
        PRELUDE,
        "--dir",
        ".",
        compiler,
        COMPILER,
        cwd=source,
        output=output,
    )
    with output.open("rb") as module:
        prefix = module.read(1024)
    if not prefix.startswith(b"\0asm\x01\0\0\0"):
        raise ValueError(
            "the seed compiler did not emit Wasm: " + prefix.decode(errors="replace").strip()
        )


def oracle_compile_args(oracle, options=()):
    return [oracle, "--time", *options, "--pages", "2", "--argv0", PRELUDE, COMPILER, COMPILER]


def with_maximum_stack(command):
    # macOS rejects stack-limit changes from Python's custom-stack executable.
    # Set the soft limit in Bash so the interpreter inherits it at exec.
    return ["bash", "-c", 'ulimit -S -s "$(ulimit -H -s)" && exec -- "$@"', "moss-stack", *command]
