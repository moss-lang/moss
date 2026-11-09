#!/bin/bash
f=$(realpath "$1"); cd ~/github/moss-lang/moss-safety
echo "--- oracle"; timeout 120 ~/github/moss-lang/moss-ob-sigclose/scratch/sigclose/var/moss_m2.exe "$f" 2>&1 | tail -5; echo "oracle exit ${PIPESTATUS[0]}"
echo "--- native"; MOSS_SEED=/nix/store/1h1710x0wvzmnhj5hna8ax9bpzxyvq02-mossc.wasm timeout 300 ~/github/moss-lang/moss-ob-family/target/release/moss run "$f" 2>&1 | tail -5; echo "native exit ${PIPESTATUS[0]}"
