#!/usr/bin/env python3
"""Run Moss's tests. A filter matches the module or test name."""

import argparse
import importlib
import sys
import time
import traceback
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

from suite.support import worker_count


def discover():
    for path in sorted(Path(__file__).with_name("suite").glob("test_*.py")):
        module = importlib.import_module("suite." + path.stem)
        for name, function in vars(module).items():
            if name.startswith("test_") and callable(function):
                yield f"{path.stem[5:]}.{name[5:].replace('__', '.')}", function


def run(function):
    try:
        function()
    except Exception:
        return traceback.format_exc()
    return None


def main():
    if not __debug__:
        sys.exit("Moss tests need Python assertions enabled")
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("filter", nargs="*", help="test names or substrings")
    parser.add_argument("--list", action="store_true", help="list selected tests")
    args = parser.parse_args()
    filters = [pattern.replace("::", ".") for pattern in args.filter]
    tests = {
        name: function
        for name, function in discover()
        if not filters or any(pattern in name for pattern in filters)
    }
    if not tests:
        parser.error("no tests match")
    if args.list:
        print("\n".join(sorted(tests)))
        return 0

    failed = {}
    started = time.monotonic()
    with ThreadPoolExecutor(max_workers=worker_count(len(tests))) as pool:
        pending = {pool.submit(run, function): name for name, function in tests.items()}
        for result in as_completed(pending):
            name = pending[result]
            if error := result.result():
                failed[name] = error
                print(f"FAIL {name}", flush=True)
            else:
                print(f"ok   {name}", flush=True)
    for name, error in sorted(failed.items()):
        print(f"\n{name}\n{error}", file=sys.stderr)
    elapsed = time.monotonic() - started
    print(f"{len(tests) - len(failed)} passed, {len(failed)} failed in {elapsed:.2f}s")
    return int(bool(failed))


if __name__ == "__main__":
    sys.exit(main())
