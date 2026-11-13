"""Bootstrap and check the self-hosted Moss compiler."""

import argparse
import os
import subprocess
import sys
import tempfile
import time
from contextlib import contextmanager
from pathlib import Path

from wasm import (
    COMPILER,
    optimize,
    oracle_compile_args,
    run,
    self_compile,
    with_maximum_stack,
)


@contextmanager
def phases(operation):
    started = time.monotonic()

    def phase(message):
        print(
            f"{operation}: {message} (elapsed {time.monotonic() - started:.1f}s)",
            file=sys.stderr,
            flush=True,
        )

    yield phase
    phase("complete")


def bootstrap(source, oracle, output, details):
    details.mkdir()
    first = details / "s1.wasm"
    second = details / "s2.wasm"
    with phases("bootstrap") as phase:
        phase("interpreting the compiler to produce S1")
        run(*with_maximum_stack(oracle_compile_args(oracle)), cwd=source, output=first)
        phase("validating and optimizing S1")
        optimize(first, output)
        phase("running S1 to produce S2")
        self_compile(source, output, second)
        phase("checking the compiler fixpoint")
        if first.read_bytes() != second.read_bytes():
            raise ValueError("the compiler is not a fixpoint: S1 != S2")


def seeded(source, launcher, output):
    with tempfile.TemporaryDirectory(prefix="moss-seed-", dir=Path.cwd()) as directory:
        module = Path(directory) / "compiler.wasm"
        subprocess.run(
            [launcher, "build", source / COMPILER, "-o", module],
            env=os.environ | {"MOSS_ROOT": str(source)},
            check=True,
        )
        optimize(module, output)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    audited = commands.add_parser("bootstrap")
    seed = commands.add_parser("seeded")
    for command in (audited, seed):
        command.add_argument("source", type=Path)
        command.add_argument("output", type=Path)
    audited.add_argument("oracle", type=Path)
    audited.add_argument("details", type=Path)
    seed.add_argument("launcher", type=Path)
    args = parser.parse_args()
    if args.command == "bootstrap":
        bootstrap(args.source, args.oracle, args.output.resolve(), args.details.resolve())
    else:
        seeded(args.source, args.launcher, args.output.resolve())


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        sys.exit(f"moss bootstrap: {error}")
