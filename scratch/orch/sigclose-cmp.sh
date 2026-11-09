#!/usr/bin/env bash
# Compare elaboration verdicts of the oracle before (A) and after (B) the
# sigclose widening (a method's θ records every type symbol its signature
# mentions). Run from the repo root, after building both into
# scratch/sigclose/{base,var}/moss_m2.exe:
#   find tests examples bootstrap/examples -name '*.moss' | sort | xargs -P 8 -n 1 scratch/orch/sigclose-cmp.sh
# MODE=run compares full runs (stdout, stderr, exit status) instead.
f=$1
A=$PWD/scratch/sigclose/base/moss_m2.exe
B=$PWD/scratch/sigclose/var/moss_m2.exe
flag=--elab-only; [ "${MODE:-}" = run ] && flag=
ra=$(timeout 600 $A $flag "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B $flag "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
