#!/usr/bin/env bash
# The bootstrap step (flake.nix, portableCompiler: "interpreting the
# compiler to produce S1"), run by hand: src/main.moss, interpreted,
# compiles src/main.moss.  Writes OUT/s1.wasm, OUT/err.txt (the driver's
# stderr, with --time) and OUT/time.txt (GNU time -v: wall time, peak
# RSS).  With a REFERENCE s1.wasm, compares the two.
#
# Usage, from the repository root:
#   bootstrap/run/self-compile.sh OUT [REFERENCE] [-- DRIVER_OPTION...]
# DRIVER (default bootstrap/_build/default/run/driver.exe) selects the
# executable.  Expect hours (README.md).
set -euo pipefail
out=$1; shift
ref=""
if [ $# -gt 0 ] && [ "$1" != "--" ]; then ref=$(realpath "$1"); shift; fi
[ $# -gt 0 ] && [ "$1" = "--" ] && shift
root=$(cd "$(dirname "$0")/../.." && pwd)
driver=$(realpath "${DRIVER:-$root/bootstrap/_build/default/run/driver.exe}")
mkdir -p "$out"; out=$(realpath "$out")
ulimit -s unlimited 2>/dev/null || ulimit -s "$(ulimit -H -s)"
cd "$root"
echo "self-compile: $driver $* (commit $(git rev-parse --short HEAD 2>/dev/null || echo ?)), started $(date -Is)" | tee "$out/info.txt"
set +e
/usr/bin/time -v -o "$out/time.txt" "$driver" --time "$@" --pages 2 --argv0 lib/prelude.moss src/main.moss src/main.moss \
  > "$out/s1.wasm" 2> "$out/err.txt"
code=$?
set -e
echo "exit $code, finished $(date -Is)" | tee -a "$out/info.txt"
grep -E "Elapsed \(wall|Maximum resident" "$out/time.txt" | tee -a "$out/info.txt"
tail -n 3 "$out/err.txt"
if [ -n "$ref" ]; then
  if cmp "$ref" "$out/s1.wasm"; then
    echo "IDENTICAL to $ref (sha256 $(sha256sum < "$out/s1.wasm" | cut -c1-16))" | tee -a "$out/info.txt"
  else
    echo "DIFFERENT from $ref: sizes $(stat -c %s "$ref") (reference) and $(stat -c %s "$out/s1.wasm")" | tee -a "$out/info.txt"
    exit 1
  fi
fi
exit $code
