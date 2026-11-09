#!/usr/bin/env bash
# callerkeys2: compare the oracle (A) with a variant (B) whose
# copy_output_check matches each key k of F's result only to a key h of
# H's result equal to k (the same provision id at F's inputs), and then
# also asks the existing agreement after the copy substitution:
#   find (fun h -> key_eqb k h) (rev gkeys)  ...
#   (&&) (key_eqb mapped (copy_key generic gv h)) (copy_slot_eq generic fv gv k h)
# instead of find (fun h -> key_eqb mapped (copy_key generic gv h)).
# B is the extracted Elab.ml of ob-family4's oracle with that edit,
# built in scratch/ck/variant (dune, as scratch/r4/variant in ob-results4).
# Run: cat corpus | xargs -P 8 -n 1 scratch/orch/callerkeys2-cmp.sh
f=$1
A=$HOME/github/moss-lang/moss-ob-family4/bootstrap/_build/default/ocaml/moss_m2.exe
B=$HOME/github/moss-lang/moss-ob-callerkeys2/scratch/ck/variant/_build/default/moss_m2.exe
ra=$(timeout 600 $A --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
rb=$(timeout 600 $B --elab-only "$f" 2>&1 | head -c 400 | tr '\n' ' ' | sed 's/elaborated in [0-9.]*s/elaborated/')
if [ "$ra" != "$rb" ]; then echo "DIFF $f :: A=[$ra] :: B=[$rb]"; else echo "SAME $f"; fi
