#!/usr/bin/env python3
"""Check selected proofs with statement-only dependencies in an isolated cache."""

import argparse
import fcntl
import hashlib
import json
import os
import pathlib
import shlex
import shutil
import subprocess
import sys
import time


BOOTSTRAP = pathlib.Path(__file__).resolve().parent
SOURCES = BOOTSTRAP / "theories"
CACHE = BOOTSTRAP.parent / "scratch" / "rocq-quick"
FLAGS = ["-q", "-no-glob", "-w", "-abstract-large-number,-non-full-mutual,-deprecated"]


def digest(value):
    return hashlib.sha256(value).hexdigest()


def library_inputs():
    inputs = []
    for variable in ("COQPATH", "ROCQPATH"):
        for entry in os.environ.get(variable, "").split(os.pathsep):
            if not entry:
                continue
            path = pathlib.Path(entry).resolve()
            if path.is_relative_to("/nix/store"):
                inputs.append([str(path)])
            else:
                # Either variable can affect discovery, including matching mappings.
                artifacts = [
                    [str(file), digest(file.read_bytes())]
                    for file in sorted(path.rglob("*"))
                    if file.is_file() and file.suffix in (".vo", ".vos", ".v", ".cmxs", ".cma")
                ]
                inputs.append([str(path), artifacts])
    return inputs


def dependencies(directory, modules):
    search_paths = []
    for variable in ("COQPATH", "ROCQPATH"):
        for path in os.environ.get(variable, "").split(os.pathsep):
            if path:
                search_paths.extend(["-R", path, ""])
    result = subprocess.run(
        ["rocq", "dep", "-boot", "-vos", *search_paths, "-Q", ".", "NM"]
        + [f"{name}.v" for name in sorted(modules)],
        cwd=directory,
        check=True,
        capture_output=True,
        text=True,
    )
    if "[module-not-found," in result.stderr:
        raise RuntimeError(result.stderr)
    graph = {}
    extra_inputs = {}
    for line in result.stdout.replace("\\\n", " ").splitlines():
        targets, _, prerequisites = line.partition(":")
        if not targets.strip():
            continue
        target = shlex.split(targets)[0]
        if target.endswith(".vos"):
            module = pathlib.Path(target).stem
            graph[module] = []
            extra_inputs[module] = []
            for prerequisite in shlex.split(prerequisites):
                path = pathlib.Path(prerequisite)
                if path.suffix == ".vos" and path.parent == pathlib.Path(".") and path.stem in modules:
                    graph[module].append(path.stem)
                elif path.suffix in (".vos", ".vo", ".v") and path != pathlib.Path(f"{module}.v"):
                    resolved = (directory / path).resolve()
                    if path.suffix == ".vos":
                        # Empty .vos sentinels use the full .vo; hash both when present.
                        candidates = [resolved, resolved.with_suffix(".vo")]
                    else:
                        candidates = [resolved]
                    artifacts = [candidate for candidate in candidates if candidate.exists()]
                    if not artifacts:
                        raise RuntimeError(f"Missing dependency {path}")
                    extra_inputs[module].extend(artifacts)
    return graph, extra_inputs


def dependency_order(graph, targets):
    order = []
    visited = set()
    visiting = set()

    def visit(module):
        if module in visiting:
            raise RuntimeError(f"Cyclic theory imports involving {module}")
        if module in visited:
            return
        if module not in graph:
            raise RuntimeError(f"Missing dependency {module}")
        visiting.add(module)
        for dependency in graph[module]:
            visit(dependency)
        visiting.remove(module)
        visited.add(module)
        order.append(module)

    for target in targets:
        visit(target)
    return order


def compile_module(module, mode, directory):
    log = directory / f"{module}.{mode}.log"
    started = time.perf_counter()
    with log.open("w") as output:
        result = subprocess.run(
            ["rocq", "c", *FLAGS, f"-{mode}", "-Q", ".", "NM",
             "-o", f"{module}.{mode}", f"{module}.v"],
            cwd=directory,
            stdout=output,
            stderr=subprocess.STDOUT,
        )
    if result.returncode:
        (directory / f"{module}.{mode}").unlink(missing_ok=True)
        print(log.read_text(), file=sys.stderr)
        raise RuntimeError(f"{module} failed; compiler output is in {log}")
    print(f"{mode}: {module} ({time.perf_counter() - started:.2f}s)", flush=True)


def check(targets):
    directory = CACHE / "theories"
    directory.mkdir(parents=True, exist_ok=True)
    snapshots = {source.stem: source.read_bytes() for source in sorted(SOURCES.glob("*.v"))}
    for source in directory.glob("*.v"):
        if source.stem not in snapshots:
            for suffix in (".v", ".vos", ".vok"):
                source.with_suffix(suffix).unlink(missing_ok=True)
    for name, contents in snapshots.items():
        mirrored = directory / f"{name}.v"
        if not mirrored.exists() or mirrored.read_bytes() != contents:
            mirrored.write_bytes(contents)
    graph, extra_inputs = dependencies(directory, snapshots)
    for target in targets:
        if target not in graph:
            raise RuntimeError(f"Unknown theory {target!r}; choose a module from {SOURCES}")
    order = dependency_order(graph, targets)
    compiler = subprocess.check_output(["rocq", "--version"])
    library = subprocess.check_output(["rocq", "c", "-where"])
    config = subprocess.check_output(["rocq", "c", "-config"])
    environment = {name: os.environ.get(name) for name in (
        "ROCQPATH", "COQPATH", "ROCQLIB", "COQLIB", "ROCQRUNTIMELIB",
        "OCAMLPATH", "CAML_LD_LIBRARY_PATH", "LD_LIBRARY_PATH", "DYLD_LIBRARY_PATH",
    )}
    library_snapshot = library_inputs()
    toolchain = digest(compiler + library + pathlib.Path(__file__).read_bytes()
                       + str(pathlib.Path(shutil.which("rocq")).resolve()).encode()
                       + config + json.dumps(environment, sort_keys=True).encode()
                       + json.dumps(library_snapshot).encode()
                       + json.dumps(FLAGS).encode())
    manifest_path = CACHE / "manifest.json"
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else {}
    keys = {}
    observed_inputs = {}
    fingerprints = {}
    manifest = {name: entry for name, entry in manifest.items() if name in snapshots}

    def save():
        temporary = manifest_path.with_suffix(".new")
        temporary.write_text(json.dumps(manifest, indent=2) + "\n")
        temporary.replace(manifest_path)

    for name in order:
        signatures = []
        for path in extra_inputs[name]:
            immutable = path.is_relative_to("/nix/store")
            if path not in fingerprints:
                # Nix store paths identify immutable library contents.
                fingerprints[path] = str(path) if immutable else digest(path.read_bytes())
            fingerprint = fingerprints[path]
            signatures.append([str(path), fingerprint])
            original = SOURCES / path.name if path.parent == directory else path
            if not immutable:
                observed_inputs[original] = fingerprint
        keys[name] = digest(json.dumps([
            toolchain, name, digest(snapshots[name]), [keys[dep] for dep in graph[name]], signatures
        ]).encode())
        entry = manifest.setdefault(name, {})
        if (entry.get("vos") == keys[name]
                and (directory / f"{name}.vos").exists()
                and (directory / f"{name}.v").exists()):
            continue
        (directory / f"{name}.vos").unlink(missing_ok=True)
        (directory / f"{name}.vok").unlink(missing_ok=True)
        entry.clear()
        save()
        compile_module(name, "vos", directory)
        entry["vos"] = keys[name]
        save()

    for name in targets:
        entry = manifest[name]
        if entry.get("vok") == keys[name] and (directory / f"{name}.vok").exists():
            print(f"vok: {name} (unchanged)", flush=True)
            continue
        (directory / f"{name}.vok").unlink(missing_ok=True)
        entry.pop("vok", None)
        save()
        compile_module(name, "vok", directory)
        entry["vok"] = keys[name]
        save()

    for name in order:
        if (SOURCES / f"{name}.v").read_bytes() != snapshots[name]:
            raise RuntimeError(f"{name}.v changed during checking; run the command again")
    for path, fingerprint in observed_inputs.items():
        if not path.exists() or digest(path.read_bytes()) != fingerprint:
            raise RuntimeError(f"{path} changed during checking; run the command again")
    if library_inputs() != library_snapshot:
        raise RuntimeError("External libraries changed during checking; run the command again")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("modules", nargs="+", help="Theory module names, for example Audit FrontEnd")
    args = parser.parse_args()
    if shutil.which("rocq") is None:
        parser.error("rocq is unavailable; run inside nix develop .#bootstrap")
    targets = [pathlib.Path(module).stem for module in args.modules]
    CACHE.mkdir(parents=True, exist_ok=True)
    with (CACHE / "lock").open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            parser.error("another quick check is using this worktree's cache")
        print("Development check: target proofs are checked against statement-only dependencies.", flush=True)
        print("Run bootstrap/check.sh for full proof and assumption verification.", flush=True)
        started = time.perf_counter()
        try:
            check(targets)
        except subprocess.CalledProcessError as error:
            diagnostic = error.stderr or error.output or ""
            if isinstance(diagnostic, bytes):
                diagnostic = diagnostic.decode(errors="replace")
            parser.exit(1, f"{diagnostic}\n{error}\n")
        except RuntimeError as error:
            parser.exit(1, f"{error}\n")
        print(f"Development check passed ({time.perf_counter() - started:.2f}s).", flush=True)


if __name__ == "__main__":
    main()
