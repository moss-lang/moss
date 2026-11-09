#!/usr/bin/env bash
# Compare elaboration verdicts of the oracle (A, safety's Elab.v, built in
# moss-ob-family4, identical Elab.v/ocaml) and the cap3b variant (B: link_body
# also requires each input of F that G has to read in G as a type over F's
# inputs, Audit LinkBody). Run from the repo root:
#   find tests examples bootstrap/examples -name '*.moss' | sort | xargs -P 8 -n 1 scratch/orch/cap3b-cmp.sh
f=$1
A=$HOME/github/moss-lang/moss-ob-family4/bootstrap/_build/default/ocaml/moss_m2.exe
B=$PWD/scratch/cap3b/variant/bootstrap/_build/default/ocaml/moss_m2.exe
ra=$(timeout 600 $A --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(timeout 600 $B --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
