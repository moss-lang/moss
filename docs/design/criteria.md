# Moss semantics: design criteria

The D-notes in [`semantics.md`](semantics.md) record individual decisions,
and they sometimes contradict each other as the design moves. This document
lists the properties those decisions are judged against. When a rule has
several reasonable forms, prefer the one that keeps all of these true. When
a rule would break one of them, that is a reason to change the rule, not to
weaken the criterion.

Each criterion names the theorem that states it, or says that it is not yet
stated. Theorem names refer to `bootstrap/theories/`.

## 1. Safety

An accepted program never goes wrong: no run of an admissible program ends
in `Wrong`.

- Stated as `Audit.Public.CORRECT.safety` in [`Audit.v`](../../bootstrap/theories/Audit.v).
- Proved as `public_safety` (`PublicEntry.v`), which `assumptions/Certificate.v`
  checks has exactly the type of `CORRECT.safety` and assumes only the
  kernel's primitives.
- The core calculus has its own version, `CoreMoss.safety`.

## 2. Coherence

Every derivation of a judgment gives the same result. Where the rules leave
a choice open, such as a merge's representative, an order, a fresh symbol's
name, or which family item a key matches, the choice must not change whether
a program is accepted or how it runs.

- This is the standard criterion for elaboration semantics, in the sense of
  Reynolds' coherence and of coherent type-class resolution.
- An open choice that changes acceptance or behaviour is a bug in the rules.
  The fix is to make the meaning independent of the choice. Fixing the choice
  by convention (for example, by source order) is not a fix.
- When two derivations would give different results and neither is more
  general, the program is ambiguous and is rejected. A family key that
  matches two items with different matchings is an example.
- Not yet stated as a theorem for `Audit.v`. `CoreMoss.method_resolution_functional`
  and `D84.on_demand_theta_determined` are instances in the calculus.

## 3. Principality

When a rule solves for something, it takes the most general solution and
never a guessed instance.

- Example: a use of an item (a call, a provider bind, a family demand)
  needs each of the item's type requirements in force: bound or assumed
  where it is used, or in force through the item's merge class (a member
  in force, or a structured type whose symbols are in force). So the
  instantiation is determined by the use: each requirement is read as its
  meaning there, or through its class, never as some other type that
  merely makes a later check succeed, and never as an unbound symbol
  (rule B, [`AUDIT-CHANGES.md`](../../bootstrap/AUDIT-CHANGES.md) #60).
- Example: context merges are most-general unifiers (`MostGeneralMerge`).
- Principality usually also secures coherence, since a most general
  solution is unique up to renaming.
- Not yet stated as a theorem for `Audit.v`.

## 4. Specialization commutes

Running generic code at an instantiation means the same as running the code
specialized to that instantiation. This is what lets the compilers
monomorphize every program (D16, D80).

It has three parts:

1. **Phase separation.** Run-time behaviour depends on types only through
   static instantiations. Types are not erased: heap headers and cast checks
   read ground types at run time. But every type computed at run time is
   determined by the program point and its static instantiation path.
2. **Finitely many instantiations.** Each accepted program reaches finitely
   many (code, ground instantiation) pairs, up to renaming of the fresh
   generations that functor applications create. This needs a check that
   rejects expanding cycles, in code and in type definitions (D30's planned
   resolution, Kennedy and Pierce's expansive cycles). Without it the
   property is false: `Specialization.polyrec_infinite`. `Audit.v` states
   the check as `NoExpandingCycle`, part of `ElaboratesIn`; neither
   compiler implements it yet.
3. **A behaviour-preserving specialization.** A translation removes every
   run-time type computation and preserves behaviour. Together with safety,
   specialized programs are safe too.

- The calculus has `CoreMoss.phase_separation` and the D16 theorem in
  `Specialization.v`. None of the three parts is yet stated for `Audit.v`.

## 5. Stability under rebinding

Rebinding a type or provision after something has captured it does not
change what the capture means. A functor's fixed inputs keep the meaning
they had where it was bound, and a linked body reads its inputs as the
caller supplied them, whatever the body rebinds.

- The calculus has `CoreMoss.fixed_inputs_never_stale`,
  `CoreMoss.capture_at_bind` and `CoreMoss.capture_through_copy`.
- In `Audit.v`, this is what the D84 rule 3b checks, the fixed-input
  agreement, and `link_site`'s readings enforce. It is not yet stated as one
  theorem.

## 6. Fidelity to the implementations

The semantics rejects nothing that both implementations accept and run
correctly, and it accepts nothing that both reject.

- This is a working criterion, not a theorem. It is checked by running
  example programs on the oracle and on native, and by comparing an oracle
  variant over the test corpus.
- Where the implementations disagree with each other, the first five
  criteria decide. Section 7 decides even where they agree.
- A rule that breaks this criterion only to make a proof easier is a debt.
  It should be listed with its alternative, as the kept-output clause was in
  [`bootstrap/AUDIT-CHANGES.md`](../../bootstrap/AUDIT-CHANGES.md) (#10) until
  it was replaced.

## 7. Every declaration is checked

Every declaration is checked as written, whether or not anything uses it.
A declaration that does not form makes the program wrong, even when no
body reads it.

- This is [D16]'s rule for defined functions, which must all elaborate,
  reached or not, extended to every kind of declaration: a context's items,
  a tag's payload, an alias's body, a val's type and initializer, a fn's or
  method's signature, a functor's arguments, result and body, and the
  assume nesting each one is written in.
- *As written* means read where it is written, before anything is bound:
  each type symbol stands for itself. A use reads the declaration at its
  own meanings, and that reading is checked at the use, as before.
- So whether a declaration is well formed does not depend on whether
  something happens to use it, and a library's errors do not wait for the
  first program that calls into the wrong part of it.
- Where both implementations accept a declaration that does not form,
  this criterion overrides fidelity (section 6): the implementations are
  the ones to change.
- In `Audit.v`, `DeclsForm` and `AssumesComplete` (every symbol's assume
  nesting) are conditions of `ElaboratesIn`. This is a check, not a
  theorem.

## Using the criteria

- **Choosing among rule forms:** prefer the form that keeps every criterion
  true. Then prefer the simpler one.
- **Judging a fix found by a proof:** a check that only makes a proof go
  through is suspect unless it follows from one of these criteria or mirrors
  the implementations.
- **Justifying existing rules:** cite the criterion a rule serves. For
  example, the caller-keyed result install and `link_site`'s readings serve
  coherence and stability, and the expanding-cycle check serves
  specialization.
