# Falsification hunt: shared brief

Goal: find out, cheaply and before anyone tries to prove them, whether the
remaining named obligations of `safety_obligations`
(bootstrap/theories/PublicEntry.v, at `safety` 73234e7 in
~/github/moss-lang/moss-safety) are TRUE. Overnight, roughly one in two
obligations turned out false when someone tried to prove it; each was
found with a small hand-written .moss program. Your job is to be that
program-writer up front.

Read-only on Rocq: do NOT edit any .v file, do not commit to any branch.
Write only under ~/github/moss-lang/moss-safety/scratch/hunt/<yourname>/.

Tools:
- Oracle (unchanged overnight): ~/github/moss-lang/moss-ob-family4/bootstrap/_build/default/ocaml/moss_m2.exe FILE
  (add --elab-only to only elaborate). Run from ~/github/moss-lang/moss-safety.
- Native: MOSS_SEED=/nix/store/1h1710x0wvzmnhj5hna8ax9bpzxyvq02-mossc.wasm ~/github/moss-lang/moss-ob-family/target/release/moss run FILE
  (native rejects every use of a defined `val`; says nothing about const code).
- Audit has no executable interpreter: decide what Audit's elaboration
  (A.ElaboratesIn / the public relation) and semantics do by reading
  bootstrap/theories/Audit.v. The oracle is NOT Audit: Audit now has ~a
  dozen changes the oracle lacks (see SAFETY-SITES.md "Observations").
- Context: bootstrap/SAFETY-SITES.md (Observations, hypotheses table),
  scratch/orch/*-STATUS.md and *-AUDIT.md, bootstrap/examples/*.moss
  (many are previous counterexamples: reuse their shapes).

Known and NOT to re-report: ChainForwardCopyKeys is false
(examples/linked-copy-bracket.moss); a pending "caller-keyed install"
Audit change (design: "Round 2" in scratch/orch/roles-STATUS.md) will
make ChainKeysOk/ChainForwardCopyKeys true by construction. When you
find a failure, say whether it would survive that change.

Method, per obligation:
1. Read its exact statement; restate in one plain sentence what it claims.
2. List the ways it could fail (each quantifier, each "every"/"exactly",
   each place a context, copy, merge, family, bracket, output, unbound
   declared symbol, const, lambda, or nested body is involved).
3. For each, try to build an accepted program (by Audit's elaboration)
   that violates it. Run candidates on oracle and native. Write each
   tried program to your dir with a header comment: what it targets,
   oracle result, native result, Audit verdict + reasoning.
4. Verdict per obligation: FALSE (with counterexample), LIKELY TRUE
   (with the argument and what you tried), or UNCLEAR (what blocks you).

Time box: about 2 hours. Report file: scratch/hunt/<yourname>/REPORT.md.
Final message: verdict per obligation in a few lines each.
