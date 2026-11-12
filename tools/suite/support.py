"""Run the compiler, Wasm tools and Rocq oracle against a source checkout."""

import os
import subprocess
import sys
from concurrent.futures import Future, ThreadPoolExecutor
from dataclasses import dataclass
from functools import cache, wraps
from itertools import count
from pathlib import Path
from tempfile import TemporaryDirectory
from threading import Lock

from wasm import COMPILER, PRELUDE, binaryen_flags, maximize_stack, validator_flags

REFERENCE = "moss-ref"
ORACLE_FRONT_END = 63
ORACLE_REJECTED = 64
ORACLE_UNSUPPORTED = 65
TRAP = 134
WASI_IMPORTS = 'import "base.moss" use ReferenceBase;\nimport "wasi.moss" use StdBase;\n'
WASI_MAIN = "assume ReferenceBase {\n  fn main() {\n    bind StdBase;\n    run();\n  }\n}\n"


def once(function):
    """Share a result, including a setup failure, across concurrent tests."""
    lock = Lock()
    pending = None

    @wraps(function)
    def get():
        nonlocal pending
        with lock:
            if pending is None:
                pending = Future()
                try:
                    pending.set_result(function())
                except Exception as error:
                    pending.set_exception(error)
        return pending.result()

    return get


@once
def repo():
    root = Path(__file__).resolve().parents[2]
    print(f"Moss tests sources: {root}", file=sys.stderr)
    return root


@once
def scratch_directory():
    target = repo() / "target/moss-tests"
    target.mkdir(parents=True, exist_ok=True)
    # Keep test programs in the checkout for Wasmtime's directory preopen.
    directory = TemporaryDirectory(prefix="suite-", dir=target)
    return directory


names = count()


def scratch():
    return Path(scratch_directory().name)


def fresh(stem):
    return scratch() / f"{next(names)}-{stem}"


def rel(path):
    return path.relative_to(repo()).as_posix()


@dataclass
class Out:
    code: int
    stdout: bytes
    stderr: str

    def text(self):
        return self.stdout.decode("utf-8", errors="replace")

    def is_module(self):
        return self.stdout.startswith(b"\0asm\x01\0\0\0")


def output(args, *, cwd=None, env=None, inherit_stderr=False):
    out = subprocess.run(
        args,
        cwd=cwd,
        env=env,
        stdin=subprocess.DEVNULL,
        stdout=subprocess.PIPE,
        stderr=None if inherit_stderr else subprocess.PIPE,
    )
    code = out.returncode if out.returncode >= 0 else -1
    return Out(code, out.stdout, (out.stderr or b"").decode("utf-8", errors="replace"))


def validate(path):
    out = output(["wasm-tools", "validate", validator_flags(), path])
    if out.code:
        raise AssertionError(f"the compiler wrote an invalid module: {out.stderr.strip()}")


@cache
def wasm_file(module):
    path = fresh("module.wasm")
    path.write_bytes(module)
    if module.startswith(b"\0asm"):
        validate(path)
    return path


def wasm_opt(module):
    source = wasm_file(module)
    path = fresh("opt.wasm")
    out = output(["wasm-opt", *binaryen_flags(), "-O3", source, "-o", path])
    assert out.code == 0, f"wasm-opt failed: {out.stderr}"
    return path.read_bytes()


def wasm_text(module):
    out = output(["wasm-dis", *binaryen_flags()[1:], wasm_file(module)])
    assert out.code == 0, f"wasm-dis failed: {out.stderr}"
    return out.stdout.decode()


def starts_with_rec_group(module):
    def leb(at):
        value = 0
        for shift in count(0, 7):
            byte = module[at]
            at += 1
            value |= (byte & 0x7F) << shift
            if not byte & 0x80:
                return value, at

    at = 8
    while at < len(module):
        section = module[at]
        size, body = leb(at + 1)
        if section == 1:
            _, first = leb(body)
            return module[first] == 0x4E
        at = body + size
    return False


def wasmtime(module, argv0, args):
    override = [] if argv0 is None else ["--argv0", argv0]
    return output(["wasmtime", "run", *override, "--dir", ".", module, *args], cwd=repo())


def run_module_code(module, args, code):
    out = wasmtime(wasm_file(module), None, args)
    assert out.code == code, f"wasmtime: {out.stderr}"
    return out.text()


def run_module(module, args):
    return run_module_code(module, args, 0)


@once
def seed():
    path = Path(os.environ.get("MOSS_SEED", repo() / "result-compiler")).resolve()
    assert path.is_file(), (
        f"no seed compiler at {path}. Set MOSS_SEED to a previously built mossc.wasm "
        "or run nix build .#compiler -o result-compiler."
    )
    print(f"Moss tests seed: {path}", file=sys.stderr)
    return path


def bootstrap_details():
    if path := os.getenv("MOSS_BOOTSTRAP_DETAILS"):
        return Path(path)


def compile_self(module):
    out = wasmtime(module, PRELUDE, [COMPILER])
    assert out.code == 0, out.stderr
    assert out.is_module(), f"not a module: {out.text()[:400]!r}"
    return out.stdout


@once
def compiler_raw():
    if details := bootstrap_details():
        return (details / "s1.wasm").read_bytes()
    validate(seed())
    return compile_self(seed())


@once
def compiler():
    if bootstrap_details():
        return seed()
    return wasm_file(wasm_opt(compiler_raw()))


def compile_source(entry, prelude):
    out = wasmtime(compiler(), prelude, [entry])
    assert out.code == 0, f"the compiler failed on {entry}: {out.stderr}"
    return out


def build_with(entry, prelude):
    out = compile_source(entry, prelude)
    assert out.is_module(), f"{entry} did not compile: {out.text()[:400]}"
    wasm_file(out.stdout)
    return out.stdout


def build(entry):
    return build_with(entry, PRELUDE)


def run(entry, args):
    return run_module(build(entry), args)


def rejects_with(entry, prelude):
    out = compile_source(entry, prelude)
    assert not out.is_module(), f"{entry} compiled, and should have been rejected"
    return out.text()


def rejects(entry):
    return rejects_with(entry, PRELUDE)


@once
def oracle_path():
    if path := os.getenv("MOSS_ORACLE"):
        path = Path(path)
    else:
        result = output(
            [
                "nix",
                "build",
                "--no-link",
                "--print-out-paths",
                "--print-build-logs",
                f"{repo()}#{REFERENCE}",
            ],
            inherit_stderr=True,
        )
        assert result.code == 0, f"building the current Rocq oracle failed: {result.code}"
        path = Path(result.text().strip()) / "bin" / REFERENCE
    path = path.resolve()
    assert path.is_file(), f"no Rocq oracle at {path}"
    print(f"Moss tests oracle: {path}", file=sys.stderr)
    return path


@once
def oracle_env():
    maximize_stack()
    environment = os.environ.copy()
    environment.pop("MOSS_LIB", None)
    return environment


def oracle_command(args):
    return output([oracle_path(), *args], cwd=repo(), env=oracle_env())


def oracle(entry, args):
    return oracle_command([entry, *args])


def oracle_run(entry, args):
    out = oracle(entry, args)
    assert out.code == 0, f"the oracle on {entry}: {out.stderr}"
    return out.text()


def oracle_rejects(entry):
    out = oracle(entry, [])
    assert out.code in (ORACLE_REJECTED, ORACLE_FRONT_END), (
        f"the oracle should reject {entry}, but exited with {out.code}: {out.stderr}"
    )
    return out.stderr


def sources(files):
    directory = fresh("src")
    root = os.path.relpath(repo(), directory)
    for name, text in files:
        path = directory / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text.replace("{root}", root), encoding="utf-8")
    return rel(directory)


def source(text):
    return sources([("main.moss", text)]) + "/main.moss"


def std_program_with(imports, declarations, body):
    return f"{WASI_IMPORTS}{imports}assume Std {{\n{declarations}\n  fn run() {{ {body} }}\n}}\n{WASI_MAIN}"


def std_program(declarations, body):
    return std_program_with("", declarations, body)


def std_main(text):
    assert "fn main()" in text, f"no main in {text}"
    imports, lines = [], []
    for line in text.splitlines():
        (imports if line.startswith("import ") else lines).append(line)
    imports = "".join(line + "\n" for line in imports)
    body = "\n".join(lines).replace("fn main()", "fn run()")
    return f"{WASI_IMPORTS}{imports}{body}\n{WASI_MAIN}"


def runnable_examples():
    return sorted(path.stem for path in (repo() / "examples").glob("*.moss"))


def moss_files(directory):
    return sorted(rel(path) for path in (repo() / directory).glob("*.moss"))


def read(path):
    return (repo() / path).read_bytes().decode("utf-8")


def worker_count(limit):
    cpus = getattr(os, "process_cpu_count", os.cpu_count)() or 1
    return max(1, min(cpus, limit, int(os.environ.get("MOSS_TEST_JOBS", cpus))))


def parallel_map(items, check, workers=None):
    workers = worker_count(8) if workers is None else workers
    if workers <= 1 or len(items) <= 1:
        return [check(item) for item in items]
    with ThreadPoolExecutor(max_workers=workers) as pool:
        return list(pool.map(check, items))
