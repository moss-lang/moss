"""Measure the interpreted compiler self-build."""

import argparse
import hashlib
import os
import resource
import subprocess
import sys
import time
from datetime import datetime
from pathlib import Path

from wasm import ROOT, oracle_compile_args, with_maximum_stack


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, usage="%(prog)s OUTDIR [REFERENCE-S1.wasm] [-- DRIVER-OPTION ...]"
    )
    parser.add_argument("directory", type=Path)
    parser.add_argument("reference", nargs="?", type=Path)
    arguments = sys.argv[1:]
    extra = []
    if "--" in arguments:
        separator = arguments.index("--")
        arguments, extra = arguments[:separator], arguments[separator + 1 :]
    args = parser.parse_args(arguments)
    out = args.directory
    out.mkdir(parents=True, exist_ok=True)
    driver = Path(os.getenv("DRIVER", ROOT / "bootstrap/_build/default/run/driver.exe")).resolve()
    revision = (
        subprocess.run(
            ["git", "rev-parse", "--short", "HEAD"], cwd=ROOT, capture_output=True, text=True
        ).stdout.strip()
        or "?"
    )
    command = with_maximum_stack(oracle_compile_args(driver, extra))

    with (out / "info.txt").open("w", encoding="utf-8") as info:

        def report(message):
            print(message, file=info, flush=True)
            print(message, flush=True)

        report(
            f"self-compile: {driver} {' '.join(extra)} (commit {revision}), "
            f"started {datetime.now().astimezone().isoformat(timespec='seconds')}"
        )
        start = time.monotonic()
        with (out / "s1.wasm").open("wb") as wasm, (out / "err.txt").open("wb") as errors:
            status = subprocess.call(command, cwd=ROOT, stdout=wasm, stderr=errors)
        if status < 0:
            status = 128 - status
        peak = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss
        if sys.platform == "darwin":
            peak //= 1024
        timing = (
            f"Elapsed (wall clock) time (seconds): {time.monotonic() - start:.2f}\n"
            f"Maximum resident set size (kbytes): {peak}"
        )
        (out / "time.txt").write_text(timing + "\n", encoding="utf-8")
        report(
            f"exit {status}, finished {datetime.now().astimezone().isoformat(timespec='seconds')}"
        )
        report(timing)
        for line in (out / "err.txt").read_text(errors="replace").splitlines()[-3:]:
            print(line)
        if args.reference and status == 0:
            actual = (out / "s1.wasm").read_bytes()
            expected = args.reference.read_bytes()
            if actual != expected:
                report(f"DIFFERENT from {args.reference}: sizes {len(expected)} and {len(actual)}")
                return 1
            report(
                f"IDENTICAL to {args.reference} (sha256 {hashlib.sha256(actual).hexdigest()[:16]})"
            )
    return status


if __name__ == "__main__":
    sys.exit(main())
