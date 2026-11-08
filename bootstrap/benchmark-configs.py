#!/usr/bin/env python3
"""Measure capped bootstrap configurations against one immutable input snapshot.

Run inside `nix develop`, passing the extracted oracle and a durable output
directory outside the Nix store. Each configuration has a one-hour wall limit.
"""

import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import time


ROOT = Path(__file__).resolve().parent.parent
CONFIGS = {
    "default": {},
    "closure-only": {"MOSS_FASTPATH": "0", "MOSS_PRUNE": "0"},
    "prune-only": {"MOSS_FASTPATH": "0"},
    "fastpath-only": {"MOSS_PRUNE": "0"},
}
FEATURE_NAMES = {
    "mutable-globals": ["mutable-global"],
    "nontrapping-float-to-int": ["saturating-float-to-int"],
    "sign-ext": ["sign-extension"],
    "multivalue": ["multi-value"],
    "bulk-memory": ["bulk-memory"],
    "reference-types": ["reference-types"],
    "simd": ["simd"],
    "gc": ["gc", "function-references"],
    "multimemory": ["multi-memory"],
}


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def measure(name, settings, output, snapshot, oracle, timeout):
    directory = output / name
    directory.mkdir()
    started = time.monotonic()
    deadline = started + timeout
    phases = []
    env = dict(os.environ)
    # Compare explicit configurations, regardless of the invoking shell.
    for variable in ("MOSS_FASTPATH", "MOSS_PRUNE", "MOSS_ENGINE"):
        env.pop(variable, None)
    env.update(settings, MOSS_ENGINE="lazy", MOSS_REF_TIME="1")
    save(directory / "configuration.json", dict(settings, MOSS_ENGINE="lazy"))

    def run(phase, command, stdout_path=None):
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            raise TimeoutError("configuration exceeded wall-clock limit")
        elapsed = time.monotonic() - started
        print(f"{name}: {phase} (elapsed {elapsed:.1f}s)", flush=True)
        with (directory / (phase + ".stderr.log")).open("wb") as stderr:
            outpath = stdout_path or directory / (phase + ".stdout.log")
            with outpath.open("wb") as stdout:
                process = subprocess.Popen(command, cwd=snapshot, env=env,
                                           stdout=stdout, stderr=stderr,
                                           start_new_session=True)
                save(directory / "running.json", {"phase": phase, "pid": process.pid})
                try:
                    returncode = process.wait(timeout=remaining)
                except BaseException:
                    try:
                        os.killpg(process.pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
                    process.wait()
                    raise
                if returncode:
                    raise subprocess.CalledProcessError(returncode, command)
        phases.append({"phase": phase, "elapsed_seconds": time.monotonic() - started})

    try:
        run("interpret", [str(oracle), "--pages", "2", "--argv0", "lib/prelude.moss",
                          "src/main.moss", "src/main.moss"], directory / "s1.wasm")
        features = [line for line in (snapshot / "wasm-features.txt").read_text().splitlines()
                    if line and not line.startswith("#")]
        validate_features = ["-all", "mvp"] + [name for f in features for name in FEATURE_NAMES[f]]
        run("validate", ["wasm-tools", "validate", "--features=" + ",".join(validate_features),
                         str(directory / "s1.wasm")])
        run("optimize", ["wasm-opt", "-mvp", *["--enable-" + f for f in features], "-O3",
                         str(directory / "s1.wasm"), "-o", str(directory / "s1-opt.wasm")])
        run("self-compile", ["wasmtime", "run", "--argv0", "lib/prelude.moss", "--dir",
                             str(snapshot) + "::.", str(directory / "s1-opt.wasm"), "src/main.moss"],
            directory / "s2.wasm")
        if (directory / "s1.wasm").read_bytes() != (directory / "s2.wasm").read_bytes():
            raise RuntimeError("S1 and S2 differ")
        result = {"status": "passed", "compiler_sha256": digest(directory / "s1.wasm")}
    except Exception as error:
        result = {"status": "failed", "error": repr(error)}
    result.update(elapsed_seconds=time.monotonic() - started, phases=phases)
    save(directory / "result.json", result)
    (directory / "running.json").unlink(missing_ok=True)
    print(f"{name}: {result['status']} ({result['elapsed_seconds']:.1f}s)", flush=True)
    return name, result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("oracle", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--config", action="append", choices=CONFIGS)
    parser.add_argument("--timeout", type=int, default=3600)
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args()
    if not 1 <= args.timeout <= 3600 or args.jobs < 1:
        parser.error("timeout must be 1..3600 seconds and jobs must be positive")
    configs = list(dict.fromkeys(args.config or CONFIGS))
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=False)
    snapshot = output / "input"
    snapshot.mkdir()
    for directory in ("src", "lib"):
        shutil.copytree(ROOT / directory, snapshot / directory)
    shutil.copy2(ROOT / "wasm-features.txt", snapshot)
    oracle = output / "oracle"
    shutil.copy2(args.oracle.resolve(), oracle)
    inputs = {str(path.relative_to(snapshot)): digest(path)
              for path in sorted(snapshot.rglob("*")) if path.is_file()}
    save(output / "inputs.json", {
        "commit": subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip(),
        "oracle_sha256": digest(oracle), "source_sha256": inputs,
        "configurations": configs, "jobs": min(args.jobs, len(configs)),
        "timeout_seconds_per_configuration": args.timeout,
        "started_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
    })
    with ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = [executor.submit(measure, name, CONFIGS[name], output, snapshot, oracle, args.timeout)
                   for name in configs]
        results = dict(future.result() for future in futures)
    unchanged = all(digest(snapshot / path) == sha for path, sha in inputs.items())
    hashes = {result["compiler_sha256"] for result in results.values() if result["status"] == "passed"}
    save(output / "results.json", {"configurations": results, "snapshot_unchanged": unchanged,
                                   "passing_compilers_agree": len(hashes) <= 1})
    if not unchanged or len(hashes) > 1:
        raise SystemExit("snapshot changed or passing compiler outputs disagree")


if __name__ == "__main__":
    main()
