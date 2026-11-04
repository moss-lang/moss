#!/usr/bin/env bash
# Compile every Rocq file and audit what it proves (`nix flake check` runs
# this as the `bootstrap-rocq` check; about 5 minutes and 6 GB).
#
# - Every top-level Theorem or Corollary of a file has a `Print Assumptions`
#   line in that file, and the audited names are exactly those theorems
#   (Certified.v, which only binds Audit.v's obligations, is exempt).
# - The calculus files (CoreMoss, Narrowing, KeyOnly, Specialization, D84)
#   must report every audited theorem "Closed under the global context".
# - The files that use Rocq's primitive integers, floats and arrays (Audit,
#   MossX, the elaborator, the front end, ...) may list assumptions, but
#   each must be one of the kernel's `Primitive` declarations.
# - Audit.v may declare Parameters and Axioms only inside module types: they
#   are the pending obligations (README.md), never assumptions.
#
# Needs `rocq` with the Rocq standard library on ROCQPATH, as in
# `nix develop .#bootstrap`. Works in _build/check.
set -euo pipefail
cd "$(dirname "$0")"
rm -rf _build/check && mkdir -p _build/check
cp theories/*.v _build/check/
cd _build/check
CORELIB=$(rocq c -where)/theories
PRIMS=$(cat $CORELIB/Numbers/Cyclic/Int63/PrimInt63.v $CORELIB/Floats/PrimFloat.v $CORELIB/Array/PrimArray.v \
        | grep -oE '^Primitive [A-Za-z0-9_]+' | cut -d' ' -f2 | sort -u)
FILES="Audit RelFacts CoreMoss Narrowing KeyOnly Specialization D84 MossX FastX NMap Sx Elab Parse Prune
       ElabSound Certified RelSupport BxBridged BxMisfit BxNonex BxFamily RelExamples RelNegative RelFamily"
# Every file must be in the list: a file left out would be neither compiled
# nor audited.
diff <(printf '%s\n' $FILES | sort) <(ls *.v | sed 's/\.v$//' | sort) \
  || { echo "check.sh: FILES is not every .v file" >&2; exit 1; }
for f in $FILES; do
  rocq c -q -w -abstract-large-number,-non-full-mutual,-deprecated -Q . NM $f.v > $f.log 2>&1 || { cat $f.log; exit 1; }
  a=$(grep -c '^Print Assumptions' $f.v || true)
  case $f in
    CoreMoss|Narrowing|KeyOnly|Specialization|D84)
      c=$(grep -c 'Closed under the global context' $f.log || true)
      [ "$a" = "$c" ] && [ "$(grep -vc 'Closed under the global context' $f.log)" = 0 ] \
        || { cat $f.log; echo "AUDIT FAIL $f: $c of $a theorems closed under the global context"; exit 1; }
      tag=closed;;
    *)
      c=$(grep -c '^Axioms:\|Closed under the global context' $f.log || true)
      bad=$(grep -v '^Axioms:\|Closed under the global context' $f.log | grep -oE '^[A-Za-z0-9_.]+' \
            | sed 's/.*\.//' | sort -u | comm -23 - <(echo "$PRIMS") || true)
      [ "$a" = "$c" ] && [ -z "$bad" ] || { cat $f.log; echo "AUDIT FAIL $f: $bad"; exit 1; }
      tag="audited (kernel primitives only)";;
  esac
  if [ $f != Certified ]; then
    diff <(grep -oE '^(Theorem|Corollary) [A-Za-z0-9_]+' $f.v | cut -d' ' -f2 | sort) \
         <(grep -oE '^Print Assumptions [A-Za-z0-9_.]+' $f.v | cut -d' ' -f3 | sed -e 's/\.$//' -e 's/.*\.//' | sort) \
      || { echo "AUDIT FAIL $f: the audited names are not exactly its Theorems and Corollaries"; exit 1; }
  fi
  if [ $f = Audit ]; then
    out=$(awk '/^Module Type /{t=1} /^End [A-Z_]+\./{t=0} /^ *(Parameter|Axiom)\b/ && !t {print NR": "$0}' $f.v)
    [ -z "$out" ] || { echo "AUDIT FAIL Audit: Parameter outside a module type: $out"; exit 1; }
    tag="$tag; Parameters only in module types"
  elif grep -nE '^\s*(Admitted|admit|Axiom|Parameter)\b' $f.v >/dev/null || grep -nE '\b(Admitted|admit)\.' $f.v >/dev/null; then
    echo "AUDIT FAIL $f mentions Admitted/admit/Axiom/Parameter:"; grep -nE '\b(Admitted|admit|Axiom|Parameter)\b' $f.v | head; exit 1
  fi
  echo "$f: $c/$a $tag"
done
echo "check.sh: all $(echo $FILES | wc -w) files compiled and audited"
