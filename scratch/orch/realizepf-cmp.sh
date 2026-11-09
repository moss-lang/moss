#!/usr/bin/env bash
# realizepf: compare the oracle (A, this branch's Elab.v: each unsolved
# generation of F reads as a fully fresh @copy symbol, which Audit's
# CopyOutputs now requires: cg y and the output roles avoid sigma's image)
# with a variant (B) in which every unsolved generation reads as the first
# generation of the solved readings' image that is not one of F's input
# generations, when there is one (the collision the old Audit rule allowed;
# B is cruder, since it also collides with declared site symbols and maps
# several unsolved generations to one). Build B from a copy of bootstrap/
# with that edit to copy_output_compatible's `None` branch.
# Run from the repo root:
#   find tests examples bootstrap/examples scratch/copyfresh scratch/realizepf -name '*.moss' | sort | xargs -P 8 -n 1 scratch/orch/realizepf-cmp.sh
f=$1
A=$PWD/scratch/realizepf/base/moss_m2.exe
B=$PWD/scratch/realizepf/var/moss_m2.exe
flag=--elab-only; [ "${MODE:-}" = run ] && flag=
ra=$(timeout 600 $A $flag "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B $flag "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
