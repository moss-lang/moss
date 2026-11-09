#!/usr/bin/env bash
cd ~/github/moss-lang/moss-ob-results
A=$PWD/bootstrap/_build/default/ocaml/moss_m2.exe
B=$PWD/scratch/variant/moss_m2.exe
cp $A scratch/variant/base.exe
A=$PWD/scratch/variant/base.exe
out=scratch/cmp.out; : > $out; : > scratch/cmp.progress
find tests examples bootstrap/examples -name '*.moss' | sort | xargs -P 6 -I{} bash -c '
  f={}
  ra=$(timeout 300 '"$A"' --elab-only "$f" 2>&1 | head -c 400 | tr "\n" " " | sed "s/elaborated in [0-9.]*s/elaborated/")
  rb=$(timeout 300 '"$B"' --elab-only "$f" 2>&1 | head -c 400 | tr "\n" " " | sed "s/elaborated in [0-9.]*s/elaborated/")
  if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]" >> scratch/cmp.out; fi
  echo "done $f" >> scratch/cmp.progress'
echo FINISHED >> $out
