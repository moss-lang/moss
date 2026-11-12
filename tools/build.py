#!/usr/bin/env python3
"""Build the Moss compiler and run a launcher against a source checkout."""

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

from wasm import ROOT, feature_rows, optimize, run, self_compile, validate


def compiler_digest():
    digest = hashlib.sha256()
    for path in ("tools/wasm.py", "wasm-features.txt"):
        digest.update((ROOT / path).read_bytes())
    for tool in ("wasmtime", "wasm-opt", "wasm-tools"):
        digest.update(os.fsencode(shutil.which(tool) or tool))
        digest.update(b"\0")
    for name in ("lib", "src"):
        for path in sorted((ROOT / name).rglob("*.moss")):
            digest.update(os.fsencode(path.relative_to(ROOT)))
            digest.update(b"\0")
            digest.update(path.read_bytes())
            digest.update(b"\0")
    return digest.hexdigest()


def dev_compiler(target):
    import fcntl

    with (target / "compiler.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        output = target / "moss.wasm"
        stamp = target / "moss.inputs"
        digest = compiler_digest()
        if output.is_file() and stamp.is_file() and stamp.read_text().strip() == digest:
            return output

        configured_seed = os.getenv("MOSS_SEED")
        if configured_seed:
            seed = Path(configured_seed).resolve()
        else:
            seed = output if output.is_file() else ROOT / "result-compiler"
        if not seed.is_file():
            raise ValueError(
                f"compiler seed missing at {seed}. Set MOSS_SEED to a previously built "
                "mossc.wasm or run nix build .#compiler -o result-compiler."
            )

        print("Building the self-hosted Moss compiler...", file=sys.stderr)
        with tempfile.TemporaryDirectory(prefix="compiler-", dir=target) as directory:
            directory = Path(directory)
            raw, first, second = (directory / name for name in ("raw.wasm", "s1.wasm", "s2.wasm"))
            validate(seed)
            self_compile(ROOT, seed, raw)
            optimize(raw, first)
            self_compile(ROOT, first, raw)
            optimize(raw, second)
            # A failed rebuild or interrupted rename leaves a usable seed, but
            # can never make that seed look current.
            stamp.unlink(missing_ok=True)
            second.replace(output)
            pending = directory / "inputs"
            pending.write_text(digest + "\n")
            pending.replace(stamp)
        return output


def launcher_config():
    return (
        os.getenv("CXX", "c++"),
        os.environ["MOSS_BINARYEN"],
        os.environ["MOSS_WASMTIME_INCLUDE"],
        os.environ["MOSS_WASMTIME_LIB"],
    )


def build_launcher(output, revision, platform, config):
    cxx, binaryen, include, lib = config
    output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="launcher-", dir=output.parent) as directory:
        directory = Path(directory)
        features = ["MVP", *(name for row in feature_rows() for name in row[2].split(","))]
        expression = " | ".join(f"BinaryenFeature{name}()" for name in features)
        (directory / "config.h").write_text(
            f"#define MOSS_REV {json.dumps(revision)}\n#define MOSS_BINARYEN_FEATURES ({expression})\n"
        )
        options = ["-ffunction-sections", "-fdata-sections"]
        libraries = []
        if platform == "linux-static":
            options.extend(["-static", "-Wl,--gc-sections"])
            libraries = ["-pthread", "-ldl", "-lm"]
        elif platform == "darwin-static":
            options.append("-Wl,-dead_strip")
            libraries = ["-lzstd", "-liconv"]
        elif platform == "windows":
            options.extend(
                [
                    "-static",
                    "-municode",
                    "-Wl,--gc-sections",
                    "-DWASM_API_EXTERN=",
                    "-DWASI_API_EXTERN=",
                ]
            )
            libraries = [
                "-lws2_32",
                "-ladvapi32",
                "-luserenv",
                "-lntdll",
                "-lshell32",
                "-lole32",
                "-lbcrypt",
                "-lzstd",
            ]
        elif sys.platform == "darwin":
            options.extend(["-Wl,-dead_strip", f"-Wl,-rpath,{binaryen}/lib", f"-Wl,-rpath,{lib}"])
        else:
            options.append("-Wl,--gc-sections")
        result = directory / output.name
        run(
            cxx,
            "-std=c++20",
            "-O2",
            *options,
            "-I" + binaryen + "/include",
            "-I" + include,
            "-I" + str(directory),
            ROOT / "launcher/main.cc",
            "-L" + binaryen + "/lib",
            "-L" + lib,
            "-lbinaryen",
            "-lwasmtime",
            *libraries,
            "-o",
            result,
        )
        result.replace(output)


def dev_launcher(target):
    import fcntl

    target.mkdir(exist_ok=True)
    launcher = target / "moss"
    with (target / "launcher.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        config = launcher_config()
        revision = subprocess.check_output(
            ["git", "rev-parse", "HEAD"], cwd=ROOT, text=True
        ).strip()
        inputs = [ROOT / "launcher/main.cc", ROOT / "wasm-features.txt", Path(__file__)]
        digest = hashlib.sha256(repr((revision, config, shutil.which(config[0]))).encode())
        for path in inputs:
            digest.update(path.read_bytes())
        stamp = target / "launcher.inputs"
        current = digest.hexdigest()
        if not launcher.is_file() or not stamp.is_file() or stamp.read_text() != current:
            build_launcher(launcher, revision, "native", config)
            stamp.write_text(current)
    return launcher


def dev(argv):
    target = ROOT / "target"
    launcher = dev_launcher(target)
    environment = os.environ | {"MOSS_ROOT": str(ROOT)}
    if not environment.get("MOSS_COMPILER") and not launcher_info(argv):
        environment["MOSS_COMPILER"] = str(dev_compiler(target))
    os.execve(launcher, [launcher, *argv], environment)


def launcher_info(argv):
    """Help and version should be available before a checkout has a compiler."""
    return (
        not argv
        or argv[0] in {"-h", "--help", "help", "-V", "--version"}
        or (
            len(argv) == 2
            and argv[0] in {"build", "run"}
            and argv[1] in {"-h", "--help", "-V", "--version"}
        )
    )


def main():
    if sys.argv[1:2] == ["dev"]:
        dev(sys.argv[2:])
        return
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output", type=Path)
    parser.add_argument("revision")
    parser.add_argument("platform", choices=("native", "linux-static", "darwin-static", "windows"))
    args = parser.parse_args()
    build_launcher(args.output.resolve(), args.revision, args.platform, launcher_config())


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        sys.exit(f"moss launcher build: {error}")
