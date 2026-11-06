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
dune build --profile "${DUNE_PROFILE:-dev}" "${dune_jobs[@]}" @theories/all
echo "check.sh: auditing certificate assumptions and closed theorems" >&2
for f in Certificate Closed; do
  rocq c -q -no-glob -Q _build/default/theories NM -o _build/$f.vo assumptions/$f.v > _build/$f.log
done

corelib=$(rocq c -where)/theories
prims=$(sed -n 's/^Primitive \([A-Za-z0-9_]*\).*/\1/p' "$corelib"/Numbers/Cyclic/Int63/PrimInt63.v \
  "$corelib"/Floats/PrimFloat.v "$corelib"/Array/PrimArray.v)
# Each assumption is a line `Name : type`, its type continuing on indented lines.
other=$(grep -v -e '^Axioms:$' -e '^Closed under the global context$' -e '^ ' _build/Certificate.log \
  | sed 's/ :.*//; s/.*\.//' | grep -vxF "$prims" || true)
[ -z "$other" ] || fail "the certificate assumes more than the kernel's primitives:" $other

open=$(grep -vx 'Closed under the global context' _build/Closed.log || true)
[ -z "$open" ] || fail "a theorem of assumptions/Closed.v is not closed: $open"

if grep -nE '\b(Admitted|admit)\.' theories/*.v ocaml/*.v; then fail "Admitted"; fi
if sed '/^Module Type /,/^End /d' theories/*.v | grep -E '^\s*(Parameter|Axiom)\b'; then
  fail "a Parameter or Axiom outside a module type"
fi
echo "check.sh: every file compiles, and the certificate assumes only the kernel's primitives"
