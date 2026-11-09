#!/usr/bin/env bash
# realize3: compare the oracle (A, Elab.v as in safety, whose link check
# reads an output role at the body's final table and an input symbol as its
# generation, as Audit's new link_site does) with a variant (B,
# scratch/realize3/varsrc: body_key also reads an input occurrence of an
# output symbol at the body's final table, as Audit's old link_site did).
# Run from the repo root:
#   find tests examples bootstrap/examples scratch/copyfresh scratch/realizepf scratch/realize2 scratch/realize3 -name '*.moss' -not -path '*/varsrc/*' | sort | xargs -P 8 -n 1 scratch/orch/realize3-cmp.sh
f=$1
A=$PWD/scratch/realize3/base/moss_m2.exe
B=$PWD/scratch/realize3/var/moss_m2.exe
flag=--elab-only; [ "${MODE:-}" = run ] && flag=
ra=$(timeout 600 $A $flag "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B $flag "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
