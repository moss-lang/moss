# Audit design notes

Historical review notes moved from [Audit.v](../../bootstrap/theories/Audit.v).
Its definitions and theorem contracts specify the current formal model. These
notes preserve known gaps, counterexamples, and earlier design decisions.

DRAFT NOTES — where the draft departs from the elaborator (Elab.v), and
   the spec questions writing it raised.  Numbered for reference.

   1. (permissive) Layout ([D59]) and the platform reference checks are not
      in the draft: the elaborator also rejects a type with no layout and a
      `Ref`/`Array` built from a bare handle.  Completeness (relation ⇒
      elaborator accepts) cannot hold until they are.
   2. (permissive) [D79] memory instructions under a rebound `Memory`:
      the elaborator reports "unsupported"; the draft says nothing.
      m4: they are `Addr`'s methods and the family `Memories` (D79 as
      amended); the elaborator reports "unsupported" where `Memories` is
      applied in a program that binds `Memory`, since MossX has one memory.
   3. (decided: see note 27) θ is part of the elaboration contract.
   4. (spec gap) Order.  Requirement lists, needs and satisfaction maps
      are sets here; the elaborator's order is its walk's.  Observable only
      through `TNom` argument order, which the draft fixes per symbol
      (w_reqs) without saying which.  Is the canonical order part of the
      language (it is part of the runtime key, [D62])?
   5. (spec gap) [D43] most general merge.  The draft takes σ most general
      among *self-consistent* unifiers (keys compared under the unifier
      itself), which is what "union the atom graphs, unify per key" means
      when method keys contain merged types.  The elaborator iterates its
      walk at most 8 times (and a 9th must add nothing, note 52); a
      telescope needing more rounds is rejected by it and accepted here.
   6. (deliberately loose) [D68] literals: any term over zero/one/add/mul
      (/div, float) denoting the literal's exact value is allowed; the
      elaborator's is a Horner chain with digits as sums of `one`.  If the
      designer wants the shape fixed (it is observable through overflow
      in a provided `.add`), the rule must say which.
   7. (m4: implemented) [D84] rule 7 (PrFamily) and rule 1's fixed inputs
      (Fixed, rule 5's check in BApplyAbstract) are in the relation, and
      the elaborator now does both (Elab.family_member/apply_family at a
      call's miss; functor_fixed, fixed_ok in apply_abstract).  Rule 7's
      θ is "read off the key": the
      draft states it as *any* θ under which the family's result contains
      the key, plus uniqueness of the family — DECISIONS.md's "no two items
      share an instance of their key" is what makes θ unique, and is not
      yet a condition of `Family`.
   8. (spec gap) [D70] capture is by *mention*: a local named in the body
      (anywhere, including as a field-punning record entry) is captured,
      even if shadowed inside the lambda by a `let`.  The elaborator does
      the same (free_names_e is syntactic); the designer should confirm.
   9. (abstracted) The platform's functors (`Heap`, `Mutable`, `Arrays`)
      are bound to synthesized bodies of primitive binds (Elab.platform_fun);
      the draft says only that some body exists.
  10. (not covered) `const_val`'s cache, forwarder/wrapper sharing (one per
      provider) and lowering order are unobservable and absent.
  11. (spec gap) [D77] rule 4 needs "the context provides the detached
      name at the receiver"; the draft reads this as a provision at the
      receiver's canonical type exactly ([D62]), as the elaborator does —
      a provision at another instantiation of the same head does not win.
  12. (resolved) [D61] `find_method_decl` reads an attached method in
      the receiver's file only, as Attached does. The redundant lookup
      in the current module has been removed together with note 57's
      detached-method fallback.

   CHANGES THE PROOF WORK FORCED (RelFacts.v), recorded for the designer:
  13. Attached methods are a table of the world (w_attached, with
      wo_attached tying it to the declarations), not an existential over
      all symbols: otherwise "the attached method of Box named get" could
      not be identified without enumerating every symbol (family example).
  14. Local numbering is a finite injective list (Named/Numbering), not an
      injective function on all strings: the elaborator numbers only the
      names a body uses, in order of first use.
  15. A block ending in `e;` has the type () (or |) *whatever* is expected
      (BlkExprLast took exp = None only, so every fn whose body ends in a
      statement was unrelatable: found on app.moss).
  16. ProvideResult reads the type outputs from the declaration
      (FunOutputs), not from the entries being produced (a circularity).
  17. Rule 7's "one family per key" is the named premise OnlyFamily.
  18. Positivity: `if` with/without else, `return` with/without value,
      and `let`/`var` (LetStmt) are separate rules; SameResult is a
      definition.
   Elaborator finding (cosmetic): its rule-7 miss message prints the key
   as "`.Box.get`" (Elab.provided_call strips only a leading dot).

   m4 (5ea4e2e's language), where the elaborator now goes beyond the draft:
  19. (not in the draft) [D84] rule 7 for a callee's needs (lower.demand_
      needs): at a call of a defined fn or attached method, each method
      need nothing in force provides and a family in force produces is met
      by `XApply F θ sm` around the call (one per family and θ).  The draft
      has PrFamily for method calls only; CallDirect/Satisfies would need
      the same premise as PrFamily per need.
  20. (not in the draft) [D84] (8), [D37]: `main` runs in the platform's
      root context: CtxOf for `main` must add lib/wasm.moss's `Gc` (its
      functors, with the types they are fixed in) beside its assumes
      (Elab.add_root_context).  ItemsEntries resolves context items by
      path in the declaring module, and `Gc` need not be in scope there, so
      the draft needs an entry form by symbol.
  21. (not in the draft) `Family` is only "method items only"; the
      elaborator also checks rule 7's conditions: keys determine P(F)
      (Elab.is_family), a family's binding binds no val and applies no
      functor but a family, no direct bind of a member in force, no two
      families producing a key, no two items of one at a key instance.
  22. (not in the draft) [D84] 3d pins (a body's bind of x ∈ X(F) denotes
      what x does at the bind; a pin mentioning a per-application type is
      refused), rule 4's X(H) ⊆ X(F), and rule 2's "assuming F needs X(F)
      in force" are checked by the elaborator (bind_functor), not stated.
  23. (permissive, MossX) The platform's `Memories` members are the memory
      primitives, which MossX.mem_args lets take an `Addr` receiver (and
      source); and a `Ref`/`Array` rebuilt from a bare handle inside the
      library (GcArrays) is an unchecked cast (see note 1).
  24. (m4, implementation optimization; the relation is unchanged) Under
      PrFamily, `XApply F θ sm e` composes the family's whole result at θ at
      every application, though e reads one member: for the platform's
      `Memories` (27 members) at every memory access, about two thirds of
      the compiler's interpreted time.  The elaborator applies families
      exactly as drafted; the oracle then runs FastPath.fast_program of its
      output.  For each platform functor of primitives in main's root
      context that no bind, copy or satisfaction map moves (a syntactic
      check), that pass files it as XDFunSym h and turns each
      `XApply F θ [] e` into `XApplyBr F [(h, bs', ps)] e`, where ps is the
      pairs of inst_smap θ Dr whose requirement e may look up and bs' the
      primitives they read. PROVED (FastPath.fast_run): for every program,
      argv, rd and fuel, the
      transformed program's run equals the original's unless the original
      reports XFuel; with CachedCloFacts.engine_agrees, OracleCertified.interpreter_correct
      covers the default pipeline.

   CHANGES THE SOUNDNESS PROOF FORCED (Track A, WorldSound.v):
  25. (relation wrong; adjusted to the elaborator) [D44] a `use` of a plain
      name that its target exports both as a symbol and as a module alias:
      ExUse gave the importer both, the elaborator (Elab.link_uses) only
      the symbol, as path resolution (`resolve`) reads the first segment.
      ExUse's alias branch now requires that t export no symbol of that
      name (`w_exp W t NSName (u_name u) = None`).  Without it no reading
      of such a bundle by the elaborator is a `WorldOf`.
  26. (elaborator support fixed, not the relation) NMap.nset walked a fixed
      64 levels, so a key of 2^64 or more was silently not stored; no
      theorem about the loader's symbol table could hold for every bundle.
      It now walks S k levels (enough for every key; identical to 64 levels
      on every key an OCaml int can hold, so the extracted oracle is
      unchanged).
  27. (decided by the designer, Track A) θ is part of the elaboration
      contract, so that elaborated programs are typable (Track B found
      elaborated_typed false for polymorphic programs without it):
      - a tag `S(e)`/`S{…}` at type TNom S xs is XTag S xs …: θ is the
        type's own arguments (ExTag, ExRecord; a unit has none);
      - a direct call of f (a defined fn, a defined attached method at
        its receiver's instantiation, a wrapper's call of its provider) is
        XCallD f (theta_at W C f) …: each of f's requirements, in w_reqs
        order, read at the site (its meaning in C, else itself),
        canonical (ExCallD, MCAttachedDefined with the receiver's
        arguments set, MPWrapped in the provider's own context);
      - `bind T = τ;` is XBindTy T τ (canonical) in a body and XBTy T τ in
        a functor body's binds (BTypeBind, BindsOf's BOTy); it was
        omitted before.
      Checked GC casts now read these annotations. Runtime type bindings
      and captured type arguments identify the generated allocation type;
      incompatible raw-handle rewrapping traps, matching native ref.cast.
      Tag annotations are canonicalized before emission, including merged
      arguments nested inside nominal applications.
  28. (decided by the designer, Track A) θ in method keys: a method
      provision is still filed under its receiver and method (mkey, in
      c_mths and c_apps: at most one per key, [D62]), but its run-time key
      carries θ, the instantiation it was made at (rkey C r m: c_apps's
      entry, its known entries, canonical).  Every elaborated key is the
      run-time key read in the context that holds or looks it up:
      XMeth's (PrIn, PrFamily after the application, LitTerm), XBindMth's
      (BMethBind: θ = the provision's AppOf), and each side of every
      satisfaction map (Satisfies: callee's needs in the callee's
      context, provisions in the caller's; the wrapper's, lambda's and
      functor binds' maps likewise), main's environment, and a linked
      functor body's result telescope (in F's own terms).  A linked body's
      method binds are filed under F's spelling of the key as well
      (`refile`; the elaborator filed a renamed receiver so already, and
      now does for θ: a body that pins a fixed input, e.g. `bind Index =
      Int;`, makes its key's θ differ from F's).  Part VI's choice_of now
      compares provisions by identity (provision_id: the key without θ),
      since θ is where a provision was made, not which one it is.
      This changes MossX's run-time keys: the oracle's stdout, stderr and
      exit status were checked identical before and after on the same
      programs as note 27's, the compiler compiling tests/wasi/tags.moss
      included, and cargo test passes with it.
  29. (elaborator, for the proof; no change in behaviour) solve_requirements
      writes a tag's or alias's requirements through set_nominal_reqs,
      which writes only if the symbol is a tag or alias and drops repeats.
      Its group holds only tags and aliases and its lists have no repeats,
      so nothing changes (oracle outputs checked identical, as in note 27);
      the guard makes the requirement cache's invariant (ElabFrame.ReqOK)
      provable step by step.
  30. (discrepancy, resolved by the designer: the elaborator now follows
      the relation, which is unchanged) Depth of contexts.
      AssumeTypes/SpecTypes (V.1) reach a context's type symbols through
      contexts nested to any depth, and allow cycles (context A naming
      context B naming A: the inductive simply has finite derivations).
      The elaborator walked contexts 64 deep (assume_type_syms ->
      walk_specs 64) and failed ("context nests too deeply") beyond that,
      including on any cycle; the self-hosted compiler (lower.nest_types)
      stopped silently 17 deep.  That cost (i) completeness, and (ii)
      WorldOf's requirement lists for symbols the elaborator never asks
      about (TODO_reqs_reading (c)) could not be had by running its own
      `reqs` on them.  The options were to bound the relation, to forbid
      cyclic contexts, or to record that completeness held only below 64.
      Decided: neither implementation bounds the depth.  Both walk an
      assume nesting depth first with the set of contexts entered so far,
      entering each context once, until nothing new appears; a cycle
      requires the union of its contexts' types.  Elab.walk_specs (fuel
      one more than the symbols, which only a bug could use up: each
      call enters a context not entered before) gives the types in the
      order the old walk did on every nesting it accepted, so requirement
      lists, and with them TNom argument order, are unchanged there
      (oracle outputs checked identical on tests/wasi, tests/errors and
      examples).  WorldSound.assume_type_syms_exact is re-proved for the
      new walk: what it computes is exactly AssumeTypes, at any depth and
      through cycles.  The same walk reads a functor's result types
      (V.5 FunOutputs: Elab.result_types, which stopped at 17, and
      lower.result_type_syms, silently at 17).
      Making a function's context (V.3 ItemEntries) walks the same
      nesting, with brackets, and there a cycle has no finite derivation:
      a body that assumes a cycle of contexts has no reading.  The
      elaborator bounded that walk at 64 and the compiler at 16.  Its fuel
      is now twice `walk_fuel`, which only a cycle uses up (Elab reports
      "contexts nest in a cycle"; the compiler stops at a context already
      being walked and reports it, e_deep), so it has no depth bound
      either and rejects exactly the cycles. The designer subsequently
      required rejecting cycles at declaration, including unused contexts:
      check_contexts now validates the declaration graph before lowering.
      The remaining D84 walks use a bound derived from the declaration
      graph's size rather than the old 16/17-level limits. Tests:
      tests/wasi/context_deep.moss (70 deep), context_shared.moss and
      tests/errors/context_cycle{,_unused,_nominal}.moss.
  31. (relation wrong, adjusted to the elaborator) Relevance (V.1).
      FreeIn's FBracket read a bracket's right side even when the head of
      `R[…]` resolves to nothing; the elaborator (free_type_syms) reads
      brackets only under a head that resolves to a symbol.  FBracket now
      asks that.  And wo_reqs_least quantified over every relevance-closed
      R, which constrains only tags and aliases, so R could make a val
      mentioned in a nominal's body (FNom) require nothing while the
      elaborator reads its fixed requirements: least is now over the R
      that agree with w_reqs on everything but tags and aliases.
  32. [D75]/[D81]: reconstructing Ref/Array from an opaque handle is now
      modeled by the native checked cast, inside or outside the selected
      library. The old directory restriction in ExTag and the oracle was
      an unsupported-case boundary for missing runtime casts, and is
      removed. Source tag typing still checks the declared payload; the
      runtime checks the allocation kind and grounded held type.


   CHANGES DECIDED BY THE DESIGNER (spec fixes):
  33. (relation and both implementations unsound; decided: rejected)
      [D15] two union members with one head, as in `A[X=I32] | A[X=I64]`
      for `tag A[X](X)`.  A match arm names a member by its head alone
      (`A(y)`), so such a union gives the arm no one payload type: the
      elaborator typed it at the last such member (its table of heads,
      Elab.synth_match, keeps the last member set under a head), the
      self-hosted compiler at the first, and the program then ran stuck
      under the oracle, or compiled to an invalid module or a wrong
      value.  TyUnion now asks that no two members
      share a head (`NoDup (map head flat)`, which implies the old
      `NoDup flat`); the elaborator reports "union members `A[X=I32]` and
      `A[X=I64]` share a head (D15)" and the compiler e_union, named by
      the head.  Only a written union is checked: a union type arises from
      nothing else (Join without an expected type asks for equal types),
      and substitution keeps heads.  PatOf's tag rules (note 35) find
      their member by head, which is now unique.  Tests:
      tests/errors/union_shared_head*.moss.
  34. (relation permissive; decided: rejected) [D15] a union member that
      is a bare record type, as in `type U = {a: I32} | Red;`.  TyUnion
      counted `TNom rec_head` (V.0: a record's reserved head) as nominal;
      the elaborator rejected it ("not nominal (D15)") and the self-hosted
      compiler accepted it.  TyUnion now asks `n <> rec_head` of each
      member, and the compiler reports e_union, named by nothing.  Test:
      tests/errors/union_record_member.moss.
  35. (relation incomplete; adjusted to the elaborator) Patterns.  PatOf
      had no rule for a tag pattern with a payload binder (`S(y)`) or a
      record pattern (`S{x, x = y, x = _}`), so no match with one was
      related.  PTagBind binds y to S's payload at the member's
      arguments (PayloadOf): for a struct tag that is the bare record
      type `TNom rec_head ftys`, which a parameter or another tag of the
      same payload can take (tests/wasi/record_payload_*.moss).  PRecord
      binds fields by name through FieldPats, in the order written.  As
      the elaborator (compile_pattern) does: a sub-pattern of `S(…)` is
      a binder only, a field's is a binder or `_`, and `{…}` with no tag
      head is rejected, so neither has a rule.  Two more changes make
      PatOf match it: PatOf takes the frame, and a binder (PBinder,
      PTagBind, FieldPats) is the local `var_of F y`, where PBinder had
      `XPAny None` (the elaborator writes the local's number, so no
      match with a binder arm was related); and PUnit also covers a
      bare `S` for a tag with a payload, which the elaborator accepts.
      Not stated, as layouts are nowhere in the relation: the
      elaborator's layout check (layout_ok) of a bound payload.
  40. (elaborator unsound; adjusted to the relation) [D73] Listed
      parameters of a tag nothing uses.  WorldOf's wo_listed asks of every
      tag that each parameter it lists is a type its assume nesting
      requires.  The elaborator checked that only in solve_requirements,
      that is, only of the tags elaboration reaches, so it accepted a
      program with an unused `tag Bad[T](I32)` whose nesting does not
      require T, which has no reading.  It now checks every tag of the
      loader's world first (Elab.check_listed, the same listed_params).
      Oracle outputs are identical on all 243 programs and on the compiler
      compiling tests/wasi/tags.moss; the self-hosted compiler (src/) is
      unchanged and still accepts such a program.
  41. (resolved by the designer: imported main is allowed) The entry's
      scope identifies main, regardless of its declaration's spelling.
      Elaborates now permits `import "./lib.moss" use start as main;`
      where lib declares `fn start`. IsMainFn and both implementations
      give the root context to a function in scope as main in any loaded
      module, so its body is checked consistently wherever called (D16).
      Test: tests/wasi/imported_main.moss, whose imported main binds Heap.
  42. (relation incomplete and wrong; adjusted to the elaborator, which
      changes only where noted) Divergence in blocks.  The elaborator
      gives a block two types: the one its value has (`|` when its last
      statement cannot fall through: an expression of type `|`, or a
      `loop` no `break` leaves), and its type as a branch (its tail's, `()`
      without a tail), which `if` and `match` join.  The relation had only
      the first, and not for a loop:
      (a) `fn stop(): Never { loop {} }` (tests/wasi/branches_diverge.moss)
          had no reading: Blk typed a block ending in a statement `loop`
          `()`.  New rule BlkLoopLast: a block whose last statement is a
          `loop` no `break` leaves (BlockBreaks, defined structurally) is
          `|`; BlkLoop now asks that something follow or that the loop
          break, as BlkExpr does.
      (b) a branch block without a tail that cannot fall through is `()`
          to the elaborator (synth_if, synth_match use block_type) and was
          `|` to the relation, so `if c { return; } else { return; }` was
          `()` there and `|` here, and a later `match` on it (a catch-all
          arm, accepted) had no reading (ArmsSome asks st <> never).  New
          judgment Branch (an `else` and a match arm: a block has
          branch_ty, `()` without a tail, or the expected type), used by
          ExIfElse (the then-block joins at branch_ty too) and ArmsOne/
          ArmsMore; ArmsOne's type is the arm's, as the elaborator's
          one-arm match is (it joined with the expected type only from
          the second arm on).
      (c) elaborator: has_break_e/has_break_b walked 10000 deep, so a
          `break` nested deeper made its loop count as never falling
          through (and the program ran into what followed).  They are now
          structural (has_break_s for statements), BlockBreaks's mirror.
      (d) elaborator: a union type all of whose members are `|` (`N | N`
          for `type N = |;`) was YUnion [], which is_never does not see,
          while TyUnion makes it TUnion [] = never; elab_type now gives
          YNever for a union of no members.
      Oracle stdout, stderr and exit status are identical on all 243
      programs and on the compiler compiling tests/wasi/tags.moss.
  43. (elaborator unsound; adjusted to the relation) Two parameters of a
      fn with one name, `fn pick(x: I32, x: I32): I32 { x }`.  Numbering
      (V.6) numbers parameter i as i (after `this`) by name, so no
      numbering exists and such a fn has no reading; the elaborator
      accepted it (`x` read the first).  lower_body now rejects it ("is
      declared twice"), as lambda_provider already did for an anonymous
      function's parameters.  Oracle outputs identical as in note 42; the
      self-hosted compiler (src/) is unchanged.
  44. (relation incomplete; adjusted to the elaborator) A field of a bare
      record, `fn get(r: { a: I32, b: I32 }): I32 { r.b }`
      (tests/wasi/record_payload_param.moss, note 35).  A record's type is
      TNom rec_head …, which ExField's `match ot with TNom s xs => …`
      read as a tag's application and asked kind_sym W rec_head = tag,
      which never holds, so the program had no reading.  ExField now asks
      that the receiver be a tag's application whose payload is the
      record, or the record itself.
  45. (elaborator unsound; adjusted to the relation) (a) A value path
      with brackets, `let r = Red[I32 = I32];` for a unit `Red`: no rule
      reads a value path's brackets (ExUnitTag, ExVal, ExConst ask
      `AEPath pa None`), but synth_path ignored them and accepted.  It now
      rejects them ("only a type or a tag constructor takes brackets").
      (b) The escape check of a block ([D78], BlkE: no generation `bind
      F;` made may occur in the block's type) read the type's generations
      with ety_gens, which stopped 5000 deep; it is now structural, as
      Part V's gens is.  Oracle outputs identical on all 243 programs and
      on the compiler compiling tests/wasi/tags.moss.

   CHANGES OF TRACK A, PART 2 (calls, contexts, types; notes 50-59):
  50. (elaborator unsound; adjusted to the relation) A type symbol with
      empty brackets, `x: T[]` for `type T;`: TyType reads only
      `ATRef pa None`, so `T[]` has no reading, but elab_type read it as
      `T` (check_relevant and check_totality pass an empty application).
      elab_type now rejects brackets on a type symbol ("type `T` takes no
      brackets"; nonempty ones are still rejected first by check_relevant,
      with the message they had).  Test: tests/errors/type_brackets.moss;
      the self-hosted compiler still accepts it (listed as its bug in
      errors.rs and agreement.rs).  Also for the proof, with no change in
      behaviour: a union member whose head is 0 is rejected (0 is no
      symbol; TyUnion asks n <> rec_head).  Oracle outputs: see note 53.
      PROVED (BodyTy.pd_ty_holds): elab_type, app_subst and symbol_type
      against TyOf and Brackets, discharging BodySound's pd_ty.
  51. (relation loose; tightened to the elaborator, asked by Track B)
      AppOf let a method provision's instantiation range over any subset
      of its requirements.  It is now exactly those its signature mentions
      (SigMentions: FreeIn of a parameter's or the result's type, against
      w_reqs), in w_reqs order, as Elab.provision_app records it
      (signature_type_syms filters the requirement list by free_type_syms
      of the signature).  The relation only; the oracle is unchanged.
  52. (elaborator unsound; adjusted to the relation) [D43] merged_walk
      re-ran the assume walk with the merges found so far until a round
      added none, but after its 8th run it returned without checking, so a
      telescope whose merges take more rounds (each round finds that
      B(k-1) and C(k-1) are one, which makes `B(k-1).w` and `C(k-1).w` one
      key, and so B(k) and C(k) one type, in the next) was used half
      merged: its σ does not unify its entries, so it is no CtxOf and the
      body has no reading, yet a body that did not need the missing merges
      was accepted.  merged_walk now checks every round, the last too
      (eight walks and a ninth that must add nothing; a telescope that
      settles in eight is accepted as before), and rejects one that has
      not settled ("merging the context does not settle in 8 rounds";
      draft note 5 said so already).  Test:
      tests/errors/merge_unsettled.moss (eight levels); the same with
      seven is accepted as before.  The self-hosted compiler
      (lower.merged_walk) still accepts it (listed as its bug).
  53. (elaborator unsound; adjusted to the relation) [D43] unify had no
      occurs check: merging `A.gimme[Foo=X]` with `A.gimme[Foo=Box[T=X]]`
      set X to Box[T=X], a cyclic merge, which no finite type unifies and
      MostGeneralMerge (idempotent σ) excludes; a function in that context
      that never read X was accepted.  unify now rejects merging a
      generation with a type that contains it.  Test:
      tests/errors/merge_cyclic.moss; the self-hosted compiler still
      accepts it (listed as its bug).  For notes 50, 52 and 53 together,
      oracle stdout, stderr and exit status are identical on all 243
      programs of tests/wasi, tests/errors and examples and on the compiler
      compiling tests/wasi/tags.moss.
  54. (relation incomplete; adjusted to the elaborator, which is
      unchanged) [D84] (8), note 20: the elaborator forms `main`'s context
      with the platform's root context (Elab.add_root_context: lib/wasm.moss's
      `Gc`, each of its abstract functors not already in force after the
      types it is fixed in), but CtxOf had only the assumes.  So a `main`
      that assumes `Wasm, Wasi, Branch` and calls a fn assuming `Gc` (or
      binds `Heap`) had no reading, though the oracle accepts and runs it
      (exit status 0): tests/wasi/gc_slots.moss, whose `main` assumes
      `Wasm, Wasi, Branch` and calls `demo`, which assumes `Gc`; likewise
      functor_alloc, gc_ast, gc_bodiless and gc_resolution.  CtxOf now adds RootCtx's entries
      for a module's own `fn main` (IsMainFn), as the elaborator does,
      by symbol (Gc's items are read in lib/wasm.moss, where they are in
      scope); the merge, the context and the needs are over both.  The
      relation only at that stage. Note 41 now also counts an imported
      main in IsMainFn and in both implementations.
  55. (elaborator fragile; made total for the proof, no change on any
      program tried) [D43] Elab.canon followed the merges with a fuel of
      5000 that counted the type's depth as well as the merge steps, and
      returned what it had reached when the fuel ran out: a deep enough
      type, or a long enough chain of merges, came back unresolved, and a
      key or a fits read off it would not be the relation's (σ is a
      resolution).  The merges unify makes are triangular (an edge g ↦ t
      maps a generation not merged yet to a canonical type without g, by
      the occurs check of note 53), so following them from any type takes
      at most as many steps along a path as there are edges: canon's fuel
      now counts only those steps, and is the number of edges plus one,
      which never runs out (Merge.canon_go_fuel).  unify also compares a
      nominal's argument keys before zipping its arguments (one head has
      one argument list, its requirements in order; `combine` dropped a
      longer list's tail).  PROVED (Merge.v): canon is the resolution of
      the edges and is idempotent; σ = suf uf, the edges resolved, is an
      idempotent substitution with tr (canon t) = σ (tr t); whatever
      respects the edges is unchanged by σ first (resp_mg, most
      generality); unify keeps the edges triangular, adds only edges every
      unifier of its two sides respects, and when it adds none its two
      sides already resolve alike (unify_spec).
  56. (elaborator unsound; adjusted to the relation) [D71] A bracket on a
      context, val or fn item binds a type symbol among the item's
      requirements (Brackets asks it), as on a type or a bind, but
      add_spec never checked it: `context D = Int, C[T = Int, U = Int];`
      for a `C` that requires only `T` was accepted (exit status 0), and
      no CtxOf reads it.  add_spec now checks (check_relevant, the
      message elab_type's has).  Test: tests/errors/context_brackets.moss;
      the self-hosted compiler still accepts it (listed as its bug).
      (relation incomplete; adjusted to the elaborator) A method item's
      brackets: the designer's [D43] example tests/wasi/merge.moss (and
      merge_order, merge_functor, functor_merge_bracketed, ...) writes
      `A.gimme[Foo=B]` for `fn .gimme(): Foo;`, whose requirements are none
      (it is declared under no assume), so Brackets, asking Foo among them,
      gave those programs no CtxOf, though the oracle runs them (merge.moss
      prints `qbq`).  The elaborator binds any type symbol on a method item
      (and rejects it on a bind: check_relevant, as before); Brackets now
      asks the left side among the requirements only for a target that is
      not a method.  Oracle stdout, stderr and exit status for notes 55 and
      56 are identical to the previous oracle's on all 243 programs of
      tests/wasi, tests/errors and examples and on the compiler compiling
      tests/wasi/tags.moss, but for the new error test.
  57. (elaborator unsound; adjusted to the relation) [D71] Brackets on a
      context item that takes none: ItemEntries reads a type, a tag, a
      unit, an alias, a defined val and a functor item only unbracketed
      (IEType, IENothing, IEFun), but add_spec elaborated the brackets and
      add_symbol_item ignored them, so `context K = T[];` and
      `context L = Int, Box[T = Int];` were accepted (exit status 0) with no
      CtxOf.  add_spec now rejects them ("`T` takes no brackets in a
      context").  Test: tests/errors/context_item_brackets.moss; the
      self-hosted compiler still accepts it (listed as its bug).  Oracle
      stdout, stderr and exit status are identical to the previous
      oracle's on all programs and on the compiler compiling
      tests/wasi/tags.moss, but for the new error test.
      RESOLVED (designer decision): find_method_decl no longer falls back
      to a detached method in the receiver's home module. Previously, for a method
      item `A.m` of a context, falls back to a detached `.m` of A's own
      module when none is in scope here; MethodNamed (IEMeth) does not, so
      `context K = Int, A, A.gimme;` with `.gimme` detached in A's module
      and not imported is accepted (exit status 0) with no CtxOf, while a
      call `a.gimme()` in a body is rejected ("not a detached method in
      scope"). Both implementations now reject such a context, as
      MethodNamed requires. Tests: context_detached_unimported.moss and
      context_detached_imported.moss.
  58. (elaborator fragile; made robust for the proof, no change on any
      program) fn_info caches a function's info under its symbol, but
      computed it from the gsym record it was handed, so the cache was
      only as good as every caller's record: an entry under a symbol
      could have been made from another record with that number.  It
      now reads the symbol table's entry for the number (sym) before it
      walks.  PROVED (InfoCache.v): every entry of the cache is fn_info's
      merged walk of the table's entry for its key, or (lambda_provider)
      an entry for a symbol the elaborator made, and every step keeps
      this, so fn_info forms its function's context (CtxOf with the
      enclosing brackets apart, CtxMerge.CtxOfIE) whether its answer was
      cached or not, for a function neither a forwarder nor `main`.
      Oracle stdout, stderr and exit status are identical to the previous
      oracle's on all programs of tests/wasi, tests/errors and examples
      and on the compiler compiling tests/wasi/tags.moss.
  59. (discrepancy, reported, not changed: the relation reads more than the
      elaborator accepts) [D43] Merged types in a body.  CtxOf reads a
      body's types canonically (σ applied: c_ty, c_vals, a parameter's
      type), so a type symbol merged with `|` is `|` there and two merged
      type symbols are one type.  The elaborator tests divergence
      (check_stmt's `ASExpr` flag is_never, synth_if's and `return`'s
      is_unit_or_never, a match on `|`) and joins branches (join: is_never,
      ety_eqb) on its types as they are, not canonically (canon e).  So
      with `type N = |;`, `context C1 = A, B, A.gimme[Foo=B];` and
      `context C2 = A, A.gimme[Foo=N];`, the fn
      `assume Wasm, C1, C2 { fn f(x: B): I32 { x; } }` is rejected ("block
      yields `()`, expected `I32`", exit status 64) though BlkExprLast reads
      its body as `|`; and, B and C merged (as in tests/wasi/merge.moss),
      `fn pick(k: Bool, b: B, c: C): B { let r = if k { b } else { c }; r }`
      is rejected ("branches disagree: `B` vs `C`") though Join reads it.
      Every test the elaborator passes implies the canonical one, so no
      accepted program lacks a reading on this account; but where they
      differ the elaborator's statement-level divergence (`x;` is `()` to
      it, `|` to the relation) is not the relation's, which BodySound's
      StmtConcl equates.  Lifting CxRel's en_uf = [] therefore needs the
      elaborator to test canonically (is_never (canon e t), join over
      canon e): it newly accepts only such programs.  Left for that lift,
      as it changes synth/check_stmt, which the bodies track (proof-a2) is
      changing too.

   CHANGES OF TRACK A, PART 3 (fields, match, records, operators, tags,
   literals, defined vals; notes 60-68):
  60. (relation incomplete; adjusted to the elaborator) [D85] `for x in
      xs` is sugar (Parse.for_desugar) over four operators, `for.begin`,
      `for.more`, `for.get` and `for.after`, which the elaborator reads as
      access.moss's detached `.begin`/`.more`/`.get`/`.after`.  Part V's
      operator_method knew only num.moss's, so no program with a `for`
      loop had a reading, among them tests/wasi/lists.moss, iterate.moss,
      newton.moss and five more, which the oracle runs.  operator_method
      now gives the library module with the method's name, as the
      elaborator's does (the same table, BodyOps.operator_rel), and
      ExUnary/ExBinary read LangSym at that module.
  61. (elaborator, for the proof; no change in behaviour) A pattern names a
      member of the scrutinee's type by its head, and compile_pattern's
      table of heads also holds an abstract type's (a generation's or a
      `type`'s number).  A tag never shares its number with a type, but
      nothing local says so; compile_pattern now asks that the member it
      finds under a tag's or unit's head be that tag's application
      (YNom), so PUnit/PTagBind/PRecord's `In (TNom s xs) (members st)`
      follows (BodyData.pat_step).  Union members are tag and unit
      applications (BodySound.ety_wf, which the proof of elab_type
      against TyOf now owes), so the exhaustiveness check reads heads as
      ArmsSome does.
  62. (elaborator unsound; adjusted to the relation) A record literal of a
      payload that names a field twice, `tag P { a: I32, a: I32 };` and
      `P { a = one }`: ExRecord asks for as many fields as the payload
      has (`length fs = length ftys`, every field given once), the
      elaborator only that each payload field's name be given, and it
      ran the program.  synth_record now rejects it ("names a field
      twice, so no record literal builds it").  Such a type is still
      readable (TyRec), and `P(r)`, patterns and field reads of it are
      unchanged.  Test: tests/errors/record_field_twice.moss (the oracle
      ran it and exited 1; the self-hosted compiler still accepts it).
  63. (relation incomplete; adjusted to the elaborator) The literal `0`
      needs only lib/int.moss's `zero`: the elaborator types it at
      zero's type, but ExLit asked for `one` in force at the literal's
      type (or a float literal), so `assume Int, zero { fn f(): Int { 0 }
      }` was accepted (the oracle ran it) with no reading.  ExLit now also
      takes zero's type.
  64. (resolved by the designer: literal arithmetic follows families)
      [D68, D84 rule 7] LitOp and LitTerm now allow the same unique-family
      provision as method calls, including its type bindings, requirements,
      produced keys and resulting method signature. Direct arithmetic
      constructors remain valid. Both elaborators check each literal
      arithmetic method takes and returns the literal's own type; arbitrary
      selected library modules need not have a hardcoded standard signature.
      Tests: tests/wasi/literal_family.moss and the literal_signature fixture.
  65. (elaborator unsound on bundles the parser does not write; adjusted
      to the relation) [D68] lit_value and is_float_lit read a literal's
      text in the parser's normal form (Parse.lit_norm): digits, then
      optionally `.` and digits, then optionally `e`, `-` and digits.  The
      elaborator also split an exponent at `E`, and read any character as
      a digit (code - 48), so a bundle with `(Y "1E3")` (a Float at 1000;
      lit_value: not a float literal) or `(Y "/")` (one; lit_value: 0) had
      no reading.  synth_literal now rejects a text not in that form ("is
      not a numeric literal (D68)").  The oracle parses source, whose
      literals are always in that form, so it cannot be given one; outputs
      identical on all 245 programs.
  66. (elaborator, for the proof; no change in behaviour) const_val's
      cache write now only adds an entry: an initializer that reaches its
      own val recurses until the fuel runs out, so no successful run
      overwrote one.  The cache so only grows (ElabFrame's fx_consts), and
      BodySound's Later reads every entry as its val's ExConst premises
      (lt_consts, ConstFact; BodyConst.v proves pd_const).  That the last
      state of a run holds only such entries is an obligation of the
      whole run (each entry is made by const_val's fresh case, which
      BodyConst.const_fresh relates, from an earlier state).
  67. (elaborator, for the proof; no change in behaviour) lang_call (a
      literal's `.add`/`.mul`/`.div`, note 64) called provided_call also
      when no provision was in force and no family produced the key, for
      provided_call to search for a family again, on a later state (fresh
      numbers), and report the missing provision.  lang_call now reports
      it itself (Elab.no_provision, the same message), so a literal's
      arithmetic is provided_call's direct case only (BodyLit.v).
  68. (resolved by the designer: library identity is the prelude's
      directory) A user module named `lib/wasm.moss` is not the selected
      library. The world now carries that directory (w_library), tied to
      b_prelude by WorldOf. LangSym selects a symbol from the scope of the
      exact sibling module `<directory>/<short>.moss`, as lang_symbol does;
      a reexport is valid, and a declaration need not live in that module.
      This scope alignment also removes the old, false generic Hlang
      assumption: int.moss itself reexports num.moss's detached `.eq`.
      LibraryFacts proves the identity/scope facts from the loader.
      PlatformRefTag follows the selected wasm scope (including deliberate
      reexports), as is_platform_sym does; InLibrary uses the same directory.
      Native primitive/operator lookup uses the same exact directory, not
      a last-basename match. Disabling automatic prelude imports retains
      the default `lib/` directory from the working directory; it does not
      disable library identity. Tests: tests/wasi/library_identity.moss,
      with a user's own lib/wasm.moss, in both native prelude modes.

   CHANGES OF TRACK B (the typing bridge; notes 70-71):
  70. (relation permissive; adjusted to the elaborator, no change in what
      is accepted) [D84] rule 2 / [D20]: assuming an abstract functor F
      asks its fixed inputs X(F) in force, as the elaborator has checked
      since m4 (add_symbol_item: "assuming `F` needs `T` in force: `F` is
      fixed in them").  IEFun did not ask it, so the relation related
      programs the elaborator rejects, and Core's OkFun (X(φ) available
      where φ is listed) could not be read off a CtxOf (Track B).  IEFun
      now asks every x with Fixed W u x to have a meaning in tm; Fixed
      and what it is built from move ahead of ItemEntries.
  71. (relation wrong; adjusted to the elaborator, no change in what an
      implementation accepts) Brackets on a context item, `C[T=U]`.  The
      elaborator (add_spec, add_symbol_item; provide_spec likewise) reads
      the items of C under the bracket substitution, which it passes down
      and drops when C is done, and puts a type item `T` of C in force as
      its own generation whether or not a bracket binds it (the env, not
      the substitution, holds the types in force).  IECtx instead walked
      C's items at `bs ++ tm` and returned the walk's tm', so the bindings
      outlived C: in `context Outer = T, U, Inner[T=U];` (with `Inner`
      under `assume T`) T meant U for the rest of the walk and in the
      context made, where the elaborator has T as its own generation; and
      IEType did not put a bracket-bound T in force.  So Part V gave such
      a context meanings the elaborator does not (typing `fn f(t: T): T`
      at U -> U, and relating `fn g(t: T): U { t }`, which the elaborator
      rejects), and keys that are no requirement (tests/wasi/generic.moss:
      T and L in `exercise`'s context).  ItemEntries/ItemsEntries now take
      the enclosing brackets `ob` apart from the types in force `tm`: an
      item is read at `ob ++ tm` (its own brackets at `bs ++ ob ++ tm`),
      IECtx walks C's items under `bs ++ ob` with tm threaded alone, and
      IEType consults and extends tm only.  CtxOf, FunCtx and
      ProvideResult start with no brackets.  The elaborator is unchanged.
      Track B: what makes
      BRIDGE-A2's "each requirement means its own generation, and only
      requirements have meanings" true of Part V.
