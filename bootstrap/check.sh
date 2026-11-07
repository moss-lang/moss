#!/usr/bin/env bash
# Compile every Rocq file and audit the certificate (`nix flake check` runs
# this as the `bootstrap-rocq` check). Needs what
# `nix develop .#bootstrap` provides.
#
# - assumptions/Certificate.v: what Certified.v binds, and the extracted
#   roots, assume only the kernel's `Primitive` declarations (Rocq's
#   primitive integers, floats and arrays).
# - assumptions/Closed.v: the theorems the design notes cite as closed are
#   closed under the global context.
# - Nothing is Admitted, and Parameters and Axioms are declared only inside
#   module types (Audit.v's obligations, never assumptions).
set -euo pipefail
cd "$(dirname "$0")"
fail() { echo "check.sh: $*" >&2; exit 1; }

echo "check.sh: compiling all Rocq theories (${DUNE_PROFILE:-dev} profile)" >&2
dune_jobs=()
if [ -n "${DUNE_JOBS:-}" ]; then dune_jobs=(-j "$DUNE_JOBS"); fi
build_dir="${DUNE_BUILD_DIR:-_build}"
dune build --build-dir "$build_dir" --profile "${DUNE_PROFILE:-dev}" "${dune_jobs[@]}" @theories/all
echo "check.sh: auditing certificate assumptions and closed theorems" >&2
# These audits inspect independent roots and write separate outputs.
# Wait for both, and propagate a failure from either compiler process.
audit_pids=()
for f in Certificate Closed; do
  rocq c -q -no-glob -Q "$build_dir/default/theories" NM -o "$build_dir/$f.vo" assumptions/$f.v > "$build_dir/$f.log" &
  audit_pids+=("$!")
done
audit_failed=false
for pid in "${audit_pids[@]}"; do
  if ! wait "$pid"; then audit_failed=true; fi
done
if "$audit_failed"; then fail "an assumption audit failed to compile"; fi

corelib=$(rocq c -where)/theories
prims=$(sed -n 's/^Primitive \([A-Za-z0-9_]*\).*/\1/p' "$corelib"/Numbers/Cyclic/Int63/PrimInt63.v \
  "$corelib"/Floats/PrimFloat.v "$corelib"/Array/PrimArray.v)
# Each assumption is a line `Name : type`, its type continuing on indented lines.
other=$(grep -v -e '^Axioms:$' -e '^Closed under the global context$' -e '^ ' "$build_dir/Certificate.log" \
  | sed 's/ :.*//; s/.*\.//' | grep -vxF "$prims" || true)
[ -z "$other" ] || fail "the certificate assumes more than the kernel's primitives:" $other

open=$(grep -vx 'Closed under the global context' "$build_dir/Closed.log" || true)
[ -z "$open" ] || fail "a theorem of assumptions/Closed.v is not closed: $open"

if grep -nE '\b(Admitted|admit)\.' theories/*.v ocaml/*.v; then fail "Admitted"; fi
if sed '/^Module Type /,/^End /d' theories/*.v | grep -E '^\s*(Parameter|Axiom)\b'; then
  fail "a Parameter or Axiom outside a module type"
fi
echo "check.sh: every file compiles, and the certificate assumes only the kernel's primitives"
