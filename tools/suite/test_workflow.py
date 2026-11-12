"""A test process resolves and builds the oracle for its own checkout."""

import os
import shutil
import subprocess
import sys

from suite.support import fresh, repo


def test_oracle_build_tracks_the_runtime_checkout():
    root = fresh("oracle workflow")
    support = root / "tools/suite"
    support.mkdir(parents=True)
    shutil.copyfile(repo() / "tools/wasm.py", support.parent / "wasm.py")
    for name in ("__init__.py", "support.py"):
        shutil.copyfile(repo() / "tools/suite" / name, support / name)
    tools = root / "bin"
    tools.mkdir()
    calls = root / "calls"
    current = root / "current-oracle"
    (current / "bin").mkdir(parents=True)
    (current / "bin/moss-ref").touch()
    previous = root / "result-moss-ref"
    (previous / "bin").mkdir(parents=True)
    (previous / "bin/moss-ref").touch()
    explicit = root / "explicit-moss-ref"
    explicit.touch()
    nix = tools / "nix"
    nix.write_text(
        f"#!{sys.executable}\n"
        "import os, sys\n"
        "from pathlib import Path\n"
        f"with Path({str(calls)!r}).open('a') as log:\n"
        "    print(sys.argv[-1], file=log)\n"
        "if os.getenv('MOSS_WORKFLOW_FAIL'):\n"
        "    sys.exit(1)\n"
        f"print({str(current)!r})\n"
    )
    nix.chmod(0o755)
    child = """
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import sys
from suite.support import oracle_path, repo

assert repo() == Path(sys.argv[1])
with ThreadPoolExecutor(max_workers=8) as pool:
    paths = list(pool.map(lambda _: oracle_path(), range(8)))
assert paths == [Path(sys.argv[2])] * 8
"""
    environment = os.environ | {"PATH": str(tools), "PYTHONPATH": str(root / "tools")}
    environment.pop("MOSS_ORACLE", None)
    cmd = [sys.executable, "-c", child, str(root)]

    def run(expected, env):
        return subprocess.run([*cmd, expected], cwd=repo(), env=env, capture_output=True, text=True)

    result = run(current / "bin/moss-ref", environment)
    assert result.returncode == 0, result.stderr
    assert calls.read_text().splitlines() == [f"{root}#moss-ref"]
    result = run(current / "bin/moss-ref", environment | {"MOSS_WORKFLOW_FAIL": "1"})
    assert result.returncode != 0
    assert "building the current Rocq oracle failed" in result.stderr
    assert calls.read_text().splitlines() == [f"{root}#moss-ref"] * 2
    result = run(explicit, environment | {"MOSS_ORACLE": str(explicit), "MOSS_WORKFLOW_FAIL": "1"})
    assert result.returncode == 0, result.stderr
    assert calls.read_text().splitlines() == [f"{root}#moss-ref"] * 2
