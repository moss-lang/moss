#!/usr/bin/env bash
# linknewpf: compare the oracle (A) with a variant (B) whose D84 3d pins
# leave out the gens of F's result keys' receivers (Elab.v `mf` without its
# first flat_map). A file where they differ has a linked body that rebinds
# a symbol of a result key's receiver (read in F's merged terms): the only
# programs where Audit's link_site reading differs from at_site Cb.
# Run from the repo root:
#   find tests examples bootstrap/examples scratch/hunt scratch/linknewpf -name '*.moss' | sort | xargs -P 8 -n 1 scratch/linknewpf/cmp.sh
f=$1
A=$PWD/scratch/linknewpf/base/moss_m2.exe
B=$PWD/scratch/linknewpf/var/moss_m2.exe
ra=$(timeout 600 $A --elab-only "$f" 2>&1; echo "exit=$?")
rb=$(timeout 600 $B --elab-only "$f" 2>&1; echo "exit=$?")
ra=$(echo "$ra" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(echo "$rb" | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
