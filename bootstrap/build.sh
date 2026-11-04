#!/usr/bin/env bash
# Build the oracle, `_build/moss_m2`: compile the Rocq sources extraction
# needs (no audit; ./check.sh does that), extract the front end, the
# elaborator and the interpreter to OCaml, and compile them with the driver.
#
# Needs `coqc` (Rocq 9.1) on PATH with the Rocq standard library on
# ROCQPATH, and an OCaml 4.14 `ocamlopt`, which should be flambda: only
# flambda honours the `[@inlined]` in Extract2.v's `nat_case`, and without
# it the interpreter keeps every frame's incoming machine state alive, which
# takes the self-compile from 3 GB to more than 17 (README.md, Memory).
# `nix develop .#bootstrap` provides all three; `nix build .#oracle` runs
# this script.
set -euo pipefail
cd "$(dirname "$0")"
OCAMLOPT=${OCAMLOPT:-ocamlopt}
$OCAMLOPT -config | grep -q '^flambda: true' \
  || echo "WARNING: $OCAMLOPT is not flambda: the oracle will retain old memory versions (README.md, Memory)" >&2
mkdir -p _build/theories
cp theories/*.v _build/theories/
cd _build/theories
# Once a file is recompiled, every later one is too (they depend on it).
prev=; dirty=
for f in Audit CoreMoss MossX NMap Sx Elab FastX Prune Parse; do
  stale=; for p in $prev; do [ $p.vo -nt $f.vo ] && stale=1; done; prev="$prev $f"
  if [ -n "$dirty$stale" ] || ! [ -f $f.vo ] || ! [ $f.vo -nt $f.v ]; then
    dirty=1
    coqc -q -Q . NM $f.v > $f.build.log 2>&1 || { cat $f.build.log; echo "build: $f.v does not compile" >&2; exit 1; }
  fi
done
rm -rf extracted && mkdir extracted && coqc -q -Q . NM Extract2.v
cd ..
rm -rf ml && mkdir ml && cd ml
cp ../../ocaml/kernel/* ../theories/extracted/* ../../ocaml/moss_m2.ml .
FILES=$(ocamldep -sort *.mli *.ml)
$OCAMLOPT ${OCAMLFLAGS:-} -w -a -O3 -inline 200 -unsafe -I +unix unix.cmxa $FILES -o ../moss_m2 2>/dev/null \
  || $OCAMLOPT ${OCAMLFLAGS:-} -w -a -inline 200 -unsafe -I +unix unix.cmxa $FILES -o ../moss_m2
echo "built $(cd .. && pwd)/moss_m2"
