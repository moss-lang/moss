#!/usr/bin/env bash
# realize2: compare the oracle (A, Elab.v as in safety) with a variant (B,
# scratch/realize2/variant-Elab.patch: check_link_inputs rejects an input of
# F that the linked body's final table has, reading as itself, and G's own
# table lacks: a kept output at an input of F, as Audit's LinkBody made
# clause rejects).
# Run from the repo root:
#   find tests examples bootstrap/examples scratch/copyfresh scratch/realizepf scratch/realize2 -name '*.moss' -not -path '*/varsrc/*' | sort | xargs -P 8 -n 1 scratch/orch/realize2-cmp.sh
f=$1
A=$PWD/scratch/realize2/base/moss_m2.exe
B=$PWD/scratch/realize2/var/moss_m2.exe
flag=--elab-only; [ "${MODE:-}" = run ] && flag=
ra=$(timeout 600 $A $flag "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B $flag "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
