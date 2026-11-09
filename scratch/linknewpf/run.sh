#!/bin/bash
# usage: run.sh file.moss -- oracle (this worktree) and native (moss-safety)
f=$(realpath "$1"); cd ~/github/moss-lang/moss-ob-linknewpf
echo "--- oracle elab"; timeout 120 bootstrap/_build/default/ocaml/moss_m2.exe --elab-only "$f" 2>&1 | tail -3; echo "exit ${PIPESTATUS[0]}"
echo "--- oracle run"; timeout 120 bootstrap/_build/default/ocaml/moss_m2.exe "$f" 2>&1 | tail -3; echo "exit ${PIPESTATUS[0]}"
cd ~/github/moss-lang/moss-safety
echo "--- native"; timeout 300 ./target/release/moss run "$f" 2>&1 | tail -3; echo "exit ${PIPESTATUS[0]}"
