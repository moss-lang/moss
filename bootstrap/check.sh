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
#   module types (public and component obligations, never assumptions).
# - theories/Audit.v, the trusted statement, holds no proofs: its helper
#   lemmas live in theories/AuditFacts.v.
set -euo pipefail
cd "$(dirname "$0")"
fail() { echo "check.sh: $*" >&2; exit 1; }

rocq_check_started=$SECONDS
echo "check.sh: compiling all Rocq theories (${DUNE_PROFILE:-dev} profile)" >&2
dune_jobs=()
if [ -n "${DUNE_JOBS:-}" ]; then dune_jobs=(-j "$DUNE_JOBS"); fi
build_dir="${DUNE_BUILD_DIR:-_build}"
# The audits are dune rules (assumptions/dune), so each starts as soon as
# the theory files it requires are built, overlapping the rest of the build.
dune build --build-dir "$build_dir" --profile "${DUNE_PROFILE:-dev}" "${dune_jobs[@]}" @theories/all \
  assumptions/Certificate.log assumptions/Closed.log
echo "check.sh: Rocq theory build finished in $((SECONDS - rocq_check_started))s, including the assumption audits" >&2
rocq_audit_started=$SECONDS
echo "check.sh: inspecting certificate assumptions and closed theorems" >&2
for f in Certificate Closed; do cp -f "$build_dir/default/assumptions/$f.log" "$build_dir/$f.log"; done

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
if grep -nE '\b(Proof|Qed|Defined|Admitted)\.' theories/Audit.v; then
  fail "theories/Audit.v contains a proof; put lemmas in theories/AuditFacts.v and write definitions as terms"
fi
if sed '/^[[:space:]]*Module Type /,/^[[:space:]]*End /d' theories/*.v | grep -E '^\s*(Parameter|Axiom)\b'; then
  fail "a Parameter or Axiom outside a module type"
fi
echo "check.sh: every file compiles, and the certificate assumes only the kernel's primitives"

echo "check.sh: assumption inspection finished in $((SECONDS - rocq_audit_started))s; complete Rocq check in $((SECONDS - rocq_check_started))s" >&2
