# worldform: Audit.v changes

## 1. MostGeneralMerge: σ is a function

What: `Elaboration.MostGeneralMerge s es` gains a fifth conjunct
`forall g t, In (g, t) s -> lookup_subst g s = Some t`.

Why: `PublicFormation.MergeFormed` (every pair of a merge of formed entries
has a formed value) was false for every world, by the definition alone. Take
`es = [NTy g]` and `s = [(g, TAbs g); (g, TUnion [TAbs g])]`. Unifies holds
vacuously (no keyed entries). `s` acts as the identity, so it is idempotent
and most general. Its pairs map g to types over the telescope's generations
`[g]`. But the shadowed pair's value `TUnion [TAbs g]` is not formed (a union
member must be nominal). `entries_cx σ` keeps σ as the context's `c_sub`, and
`FormedCx` (cf_sub) asks every pair of it to be ok, so the junk is not
harmless to the proof, though no lookup ever reads it.

Test: not a program-level issue (a definition-level witness, above).

Evidence it rejects nothing: σ is existential in CtxOf/FunCtx, and the
elaborator's σ is `suf uf = map (fun p => (fst p, ...)) uf` over its
union-find, whose lookups agree with its pairs. Deduplicating any σ keeps
`app_subst σ` unchanged, so every other conjunct still holds.

Alternatives considered: weakening MergeFormed to `lookup_subst g s = Some t`.
That does not work, because cf_sub quantifies over `In`. Changing FormedCx
instead would touch every formation proof.

Proofs fixed: PublicCallRealize (2 destructs), PublicFormation (2 destructs),
AuditBuildExample (main_context).

## 2. Platform.Entry: a platform fn reads its raw handles as themselves

What: `Platform.Entry`'s `IFn` case is now
`AtKey W C key operation /\ HandlesRead W C key`. `HandlesRead` asks each type
in the fn's signature in main's context that is a handle tag (wasm `Ref`,
`Array` or `Addr`) to have its raw-handle requirement (`StructRef`,
`ArrayRef`, `I32`) read as `TAbs` of itself (`RawHandleRead`).

Why: PublicEntry needed `HandleFormed` for every fn in main's context, so
that the platform primitive is a typed call (PublicPrimitiveSafety). It is
false for a world Audit accepts. A context item can bracket a platform fn's
requirement:
[handle-rebind.moss](../../bootstrap/examples/handle-rebind.moss) has
`context Store = ..., i64_store[I32 = Addr[I32 = I64]]` and `assume Store`
on main. main's context then files i64_store at
`(Addr[Memory=Memory, I32=I64], I64) -> ()`. That is a Platform signature
for PI64Store (prim_msig `[ShapeAddress; ShapeI64]`), so Linked held, while
the Addr's raw handle reads as I64.

Implementations:
- Oracle (moss_m2 --elab-only): rejects it, with "calling `i64_store` needs
  `Memories` at another instantiation than the one in the context here was
  provided at (D62)". This is check_assumed_requirements (D20), which Audit
  does not mirror.
- Native (moss-safety `moss run`): accepts it (rc 0). main never calls
  i64_store. With a call (`i64_store(Addr[I32 = I64](x), x)`), native rejects
  the program ("incompatible types"), since a defined fn is called directly at
  its declared signature.

With the shipped libraries, the only plain platform fns whose primitive
signature admits a handle-tag type are the defined memory fns (`i32_load`
etc., through prim_msig). Each requires `Memories`, which is fixed at I32, so
the oracle's coherence check rejects every rebinding of I32 there. The new
premise therefore rejects nothing the oracle accepts with the shipped
libraries. It does reject the degenerate unused-assume program above, which
native accepts.

Alternatives considered:
- Mirror the oracle's D20 assumed-requirement coherence in Audit. It is
  larger, and native does not check it either.
- Type only the primitives main can actually invoke. A defined platform fn's
  provision is never invoked (calls to it are ECall), but the slice's
  environment invariant types every closure. That would need a restructuring
  of S.Realizes.
- Put the condition in `AtKey`. AtKey is also used for family methods, which
  are not affected.

Proofs fixed: PublicEntry.entry_realizes (`proj1 En`).
