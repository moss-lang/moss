#!/usr/bin/env bash
# copyfresh: compare the oracle (A, this branch's Elab.v, whose
# copy_output_compatible sends every unsolved generation of F to a fresh
# @copy symbol: what Audit's CopyOutputs now does) with a variant (B)
# whose unsolved generations map to themselves (`| None => ret (x,YAbs x)`:
# the old Audit rule, sigma = F's fixed readings only). A file where they
# differ is one the change affects. Build both with
#   cd bootstrap && dune build --build-dir _build_cf ocaml/moss_m2.exe
# (B after that one-line edit) into scratch/copyfresh/{base,var}/.
# Run from the repo root:
#   find tests examples bootstrap/examples scratch/copyfresh -name '*.moss' | sort | xargs -P 8 -n 1 scratch/orch/copyfresh-cmp.sh
# MODE=run compares full runs (stdout, stderr, exit status) instead.
f=$1
A=$PWD/scratch/copyfresh/base/moss_m2.exe
B=$PWD/scratch/copyfresh/var/moss_m2.exe
flag=--elab-only; [ "${MODE:-}" = run ] && flag=
ra=$(timeout 600 $A $flag "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B $flag "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
