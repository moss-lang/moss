#!/usr/bin/env bash
# Compare elaboration verdicts of the oracle (A) and the chainfwd variant (B,
# kept outputs unmentioned) on one file. Run from the repo root:
#   find tests examples bootstrap/examples -name '*.moss' | sort | xargs -P 8 -n 1 scratch/orch/chainfwd-cmp.sh
f=$1
A=$PWD/bootstrap/_build/default/ocaml/moss_m2.exe
B=$PWD/scratch/chainfwd/variant/bootstrap/_build/default/ocaml/moss_m2.exe
ra=$(timeout 600 $A --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(timeout 600 $B --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
