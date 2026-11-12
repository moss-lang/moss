"""Build the Rocq theories and check the assumptions of the bootstrap."""

import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent
CLOSED = "Closed under the global context"
PRIMITIVE_FILES = (
    "Numbers/Cyclic/Int63/PrimInt63.v",
    "Floats/PrimFloat.v",
    "Array/PrimArray.v",
)
TRUSTED_DEFINITIONS = (
    "Audit.v",
    "AuditBase.v",
    "Surface.v",
    "MossActions.v",
    "MossGrammar.vy",
)


def check_assumptions(build_dir):
    for name in ("Certificate.log", "Closed.log"):
        shutil.copyfile(build_dir / "default/assumptions" / name, build_dir / name)

    corelib = Path(subprocess.check_output(["rocq", "c", "-where"], text=True).strip())
    primitives = {
        name
        for path in PRIMITIVE_FILES
        for name in re.findall(
            r"^Primitive ([A-Za-z0-9_]+)", (corelib / "theories" / path).read_text(), re.MULTILINE
        )
    }
    # Assumption names head unindented lines. Types can continue on indented lines.
    unexpected = []
    for line in (build_dir / "Certificate.log").read_text().splitlines():
        if line in ("Axioms:", CLOSED) or line.startswith(" "):
            continue
        name = line.partition(" :")[0].rsplit(".", 1)[-1]
        if name not in primitives:
            unexpected.append(line)
    if unexpected:
        raise ValueError(
            "the certificate assumes more than the kernel's primitives:\n" + "\n".join(unexpected)
        )

    open_theorems = [
        line for line in (build_dir / "Closed.log").read_text().splitlines() if line != CLOSED
    ]
    if open_theorems:
        raise ValueError(
            "a theorem of assumptions/Closed.v is not closed:\n" + "\n".join(open_theorems)
        )


def outside_module_types(lines):
    in_module_type = False
    for number, line in lines:
        if line.lstrip().startswith("Module Type "):
            in_module_type = True
        elif not in_module_type:
            yield number, line
        elif line.lstrip().startswith("End "):
            in_module_type = False


def reject_matching(paths, pattern, message, *, allow_module_types=False):
    matches = []
    for path in paths:
        lines = enumerate(path.read_text().splitlines(), 1)
        if allow_module_types:
            lines = outside_module_types(lines)
        for number, line in lines:
            if re.search(pattern, line):
                matches.append(f"{path}:{number}:{line}")
    if matches:
        raise ValueError(message + "\n" + "\n".join(matches))


def main(targets):
    os.chdir(ROOT)
    build_dir = Path(os.getenv("DUNE_BUILD_DIR") or "_build")
    profile = os.getenv("DUNE_PROFILE") or "dev"
    jobs = os.getenv("DUNE_JOBS")
    started = time.monotonic()
    print(f"check.py: compiling all Rocq theories ({profile} profile)", file=sys.stderr)
    # Dune schedules each assumption audit as soon as its theory dependencies are built.
    subprocess.run(
        [
            "dune",
            "build",
            "--build-dir",
            build_dir,
            "--profile",
            profile,
            *(["-j", jobs] if jobs else []),
            "@theories/all",
            "assumptions/Certificate.log",
            "assumptions/Closed.log",
            *targets,
        ],
        check=True,
    )
    audit_started = time.monotonic()
    print(
        f"check.py: Rocq theory build finished in {audit_started - started:.0f}s, "
        "including the assumption audits",
        file=sys.stderr,
    )
    print("check.py: inspecting certificate assumptions and closed theorems", file=sys.stderr)
    check_assumptions(build_dir)

    theories = sorted(Path("theories").glob("*.v"))
    reject_matching(
        [*theories, *sorted(Path("run").glob("*.v"))], r"\b(Admitted|admit)\.", "Admitted"
    )
    reject_matching(
        [Path("theories") / name for name in TRUSTED_DEFINITIONS],
        r"\b(Proof|Qed|Defined|Admitted)\.",
        "trusted definitions contain a proof; use AuditFacts.v or BaseFacts.v for lemmas",
    )
    reject_matching(
        theories,
        r"^\s*(Parameter|Axiom)\b",
        "a Parameter or Axiom outside a module type",
        allow_module_types=True,
    )
    print("check.py: every file compiles, and the certificate assumes only the kernel's primitives")
    finished = time.monotonic()
    print(
        f"check.py: assumption inspection finished in {finished - audit_started:.0f}s; "
        f"complete Rocq check in {finished - started:.0f}s",
        file=sys.stderr,
    )


if __name__ == "__main__":
    try:
        main(sys.argv[1:])
    except subprocess.CalledProcessError as error:
        sys.exit(error.returncode)
    except (OSError, ValueError) as error:
        sys.exit(f"check.py: {error}")
