"""The bootstrap's tests, across every core: `python3 -m tests.parallel`.

The same tests as `python3 -m unittest` — discovered the same way, and
the count is checked — but each test method is a unit of its own, handed
to whichever worker process is free. A serial run is bound by one core
while most of its time is spent waiting on wasmtime or in the bootstrap
compiling all of src/, which is work that parallelizes perfectly.

Workers are started with `spawn` on every platform, so nothing here leans
on state inherited across a fork: each worker discovers the tests itself
and runs the ones it is handed by id. The drivers `run_driver` compiles
are shared through a directory (see `DRIVER_CACHE_ENV` in test_run.py)
that this process makes and the workers inherit through the environment,
and compiling them is the first work handed out.

Output follows unittest's: a dot per test, then each failure and error
with its traceback, then a count — plus the slowest units, which bound
the wall time no matter how many cores there are.
"""

import argparse
import multiprocessing
import os
import sys
import tempfile
import time
import unittest
from concurrent.futures import FIRST_COMPLETED, ProcessPoolExecutor, wait

from .test_run import DRIVER_CACHE_ENV

# Tests known to take far longer than the rest, started first so that the
# longest of them is not left to run alone at the end. Only an ordering:
# a test missing from here, or named here and gone, changes no outcome.
SLOW_FIRST = (
    "tests.test_build.TestSelfHostedFixpoint.",
    "tests.test_run.TestSelfHostedBackEnd.test_matches_the_bootstrap_on_behaviour",
    "tests.test_build.TestSelfHostedCompilesTheExamples.",
    "tests.test_build.TestWasmBackend.test_self_hosted_compiler_compiles_to_wasm",
    "tests.test_run.TestSelfHostedNeeds.",
    "tests.test_run.TestSelfHostedBackEnd.",
    "tests.test_run.TestSelfHosted",
)


def flatten(suite):
    for test in suite:
        if isinstance(test, unittest.TestSuite):
            yield from flatten(test)
        else:
            yield test


def discover():
    """Exactly what `python3 -m unittest` runs from bootstrap/: discovery
    from `.` with the default pattern. A module that fails to import comes
    back as a test that errors, here as there."""
    return list(flatten(unittest.TestLoader().discover(".")))


_TESTS = None


def _test(test_id):
    global _TESTS
    if _TESTS is None:
        _TESTS = {}
        for test in discover():
            _TESTS.setdefault(test.id(), []).append(test)
    return _TESTS[test_id]


def run_unit(test_id):
    """Run one test in this worker; return what the parent prints, as
    plain data. A suite of one still runs class and module fixtures."""
    result = unittest.TestResult()
    start = time.perf_counter()
    unittest.TestSuite(_test(test_id))(result)
    elapsed = time.perf_counter() - start
    return {
        "id": test_id,
        "time": elapsed,
        "run": result.testsRun,
        "errors": [(str(t), tb) for t, tb in result.errors],
        "failures": [(str(t), tb) for t, tb in result.failures],
        "skipped": [(str(t), why) for t, why in result.skipped],
        "expected": len(result.expectedFailures),
        "unexpected": [str(t) for t in result.unexpectedSuccesses],
    }


def compile_driver(driver):
    from .test_run import compile_driver

    start = time.perf_counter()
    compile_driver(driver)
    return time.perf_counter() - start


def priority(test_id):
    for rank, prefix in enumerate(SLOW_FIRST):
        if test_id.startswith(prefix):
            return rank
    return len(SLOW_FIRST)


def jobs_default():
    count = getattr(os, "process_cpu_count", os.cpu_count)()
    return max(1, count or 1)


def main(argv=None):
    parser = argparse.ArgumentParser(prog="python3 -m tests.parallel")
    parser.add_argument(
        "-j", "--jobs", type=int, default=0,
        help="worker processes (default, or 0: one per CPU)",
    )
    parser.add_argument(
        "--slowest", type=int, default=10, help="how many slow units to list"
    )
    args = parser.parse_args(argv)
    jobs = args.jobs or jobs_default()

    tests = discover()
    ids = [test.id() for test in tests]
    order = sorted(dict.fromkeys(ids), key=priority)  # stable within a rank

    from .test_run import DRIVERS

    started = time.perf_counter()
    with tempfile.TemporaryDirectory(prefix="moss-drivers-") as scratch:
        # An outer cache, if one is set, is kept and reused; otherwise
        # this run's lives and dies with it.
        os.environ.setdefault(DRIVER_CACHE_ENV, scratch)
        context = multiprocessing.get_context("spawn")
        with ProcessPoolExecutor(jobs, mp_context=context) as pool:
            pending = {pool.submit(compile_driver, d): None for d in DRIVERS}
            pending.update({pool.submit(run_unit, i): i for i in order})
            results, driver_times = [], []
            while pending:
                done, _ = wait(pending, return_when=FIRST_COMPLETED)
                for future in done:
                    unit = pending.pop(future)
                    outcome = future.result()
                    if unit is None:
                        driver_times.append(outcome)
                        continue
                    results.append(outcome)
                    sys.stderr.write(mark(outcome))
                    sys.stderr.flush()
    wall = time.perf_counter() - started
    return report(results, driver_times, len(tests), wall, jobs, args.slowest)


def mark(outcome):
    if outcome["errors"]:
        return "E"
    if outcome["failures"]:
        return "F"
    if outcome["unexpected"]:
        return "u"
    if outcome["skipped"]:
        return "s"
    if outcome["expected"]:
        return "x"
    return "."


def report(results, driver_times, discovered, wall, jobs, slowest):
    out = sys.stderr
    out.write("\n")
    # Discovery order, as unittest would print them.
    rank = {test_id: n for n, test_id in enumerate(r["id"] for r in results)}
    results.sort(key=lambda r: rank[r["id"]])
    for kind, key in (("ERROR", "errors"), ("FAIL", "failures")):
        for result in results:
            for name, tb in result[key]:
                out.write("=" * 70 + "\n")
                out.write(f"{kind}: {name}\n")
                out.write("-" * 70 + "\n")
                out.write(tb + "\n")
    for result in results:
        for name in result["unexpected"]:
            out.write(f"UNEXPECTED SUCCESS: {name}\n")

    run = sum(r["run"] for r in results)
    errors = sum(len(r["errors"]) for r in results)
    failures = sum(len(r["failures"]) for r in results)
    skipped = sum(len(r["skipped"]) for r in results)
    expected = sum(r["expected"] for r in results)
    unexpected = sum(len(r["unexpected"]) for r in results)

    out.write("\nSlowest units:\n")
    for result in sorted(results, key=lambda r: -r["time"])[:slowest]:
        out.write(f"  {result['time']:7.2f}s  {result['id']}\n")
    if driver_times:
        out.write(
            f"  ({len(driver_times)} drivers compiled in "
            f"{max(driver_times):.2f}s at most)\n"
        )
    busy = sum(r["time"] for r in results) + sum(driver_times)
    out.write("-" * 70 + "\n")
    out.write(
        f"Ran {run} tests in {wall:.3f}s on {jobs} workers "
        f"({busy:.1f}s of work)\n\n"
    )

    ok = errors == failures == unexpected == 0
    if run != discovered:
        out.write(f"MISMATCH: discovered {discovered} tests but ran {run}\n")
        ok = False
    details = [
        f"{label}={n}"
        for label, n in (
            ("failures", failures),
            ("errors", errors),
            ("skipped", skipped),
            ("expected failures", expected),
            ("unexpected successes", unexpected),
        )
        if n
    ]
    out.write(("OK" if ok else "FAILED") + (f" ({', '.join(details)})" if details else "") + "\n")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
