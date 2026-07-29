# Moss semantics: feedback

This is human-written feedback about the first version of `semantics.md` in this directory.

## 1. Design thesis

### Scope _can_ depend on types

Quoting from the other doc:

> Scope resolution never depends on types or on what has been bound.

Unfortunately this is not quite true. Methods complicate this story, because they allow type to become part of the key for stuff in the context. But of course, the way I now want methods to work is currently not actually demonstrated anywhere in the codebase.

The wrinkle is that there are actually two different kinds of methods: _attached_ methods and _detached_ methods. (In this way, this is actually somewhat similar to the prototype I built in Spring 2026.) So resolving symbols needs to know types so it can know when a method call refers to an attached method vs a detached Here's an example:

```
type Foo;

assume Foo {
  fn .gimme(): Foo;
}

type A;
type B;
type C;

assume C {
  fn C.gimme(): C {
    this
  }
}

context Ctx =
  A,
  B,
  C,

  A.gimme[Foo=B],
  B.gimme[Foo=C],
;

assume Ctx {
  fn example(a: A): C {
    let b = a.gimme();
    let c = b.gimme();
    c.gimme()
  }
}
```

In the above example, there's a _detached_ method `.gimme` (which `Ctx` asserts to exist for `A` returning `B`, and also for `B` returning `C`), and also an _attached_ method `C.gimme`. Two of the method calls in the `example` function refer to the detached method, and one refers to the attached method. Type information is needed for this.

I _hope_ this doesn't make things too messy. My intuition is that, since we're pretty much just doing type inference in a forward direction and not any crazy global constraint solving or anything like that, it should be fine. We'll need to interleave symbol resolution with type inference for this one reason; hopefully not too bad?

## 2. Lexical structure

D5 and D7 sound good.

## 3. Modules, imports, and scope

D9 and D10 sound good. Let's forbid import cycles, at least for now.

## 4. Declarations

D12 and D14 sound good.

### `unit` is... fine

I really don't like the fact that `unit` breaks the pattern that otherwise exists for both `type` and `fn`. It's the best I can think of for now, though. If you have any alternative suggestions, please let me know.

### Detached methods

As discussed above, in addition to `fn T.m(x: Ty): Ty;` / `{...}` we also need `fn .m(x: Ty): Ty;` / `{...}`.

### \[D13\] Nominal sum types currently not used consistently

You're correct, this is an inconsistency in the codebase as it currently stands: _either_ the usage sites would need to explicitly wrap using the `Option` symbol, _or_ the `Option` type definition would need to change to an alias:

```
type Option = None | Some;
```

## 5. Types

D17 and D18 sound good.

### \[D16\] Sum types can't contain abstract types

Yeah I was writing this a bunch in the code so far because it's very convenient, but I think for the MVP we should disallow this and only allow sum types to contain nominal types. I _think_ it shouldn't be too bad to just go through and systematically add one wrapper layer to all the instances that currently exist; it'll just be a bit of boilerplate, which we can look at and decide later.

One of a sum type's variants _can_ be an abstract type _only if_ the context it's being defined in _already constraints_ that abstract type to a specific nominal type.

Note that in general, any post-monomorphization checks like the pairwise-distinct proposal are off the table. All semantic checking must happen before monomorphization.

## 6. Contexts, `assume`, and implicit parameterization

D23 sounds good.

### \[D20\] Prerequisites for `assume`

D20 sounds good: a piece of context can only be assumed once there's enough stuff already assumed for its declaration to even make sense.

### \[D22\]/\[D24\] No partial application

No, partial application cannot be allowed; this is the tarpit that the most recent prototype fell into this spring. You wrote this, though, which made me think I may be misremembering:

> (Used implicitly all over `src/`; stating it explicitly.)

If `src/` is indeed using partial application all over, then that's a big issue. Please provide to me specific examples, so I can see if I'm just misremembering or if I just haven't yet made the semantics clear enough to you.

I think similarly we need to rule out D24. The issue is again that it requires too much search and cleverness in inference. If any of this is unclear or raises other issues, let's discuss it.

## 7. `bind`

D25, D26, D27, and D28 sound good.

### \[D30\] `bind`-returning functions are functors

This is one of the least elegant parts of the design I ended up with for the prototype I built this spring. It was clear that there needed to be _some_ way to map from an instance of one context shape to an instance of a different context shape, but it was unclear how to express that as a language feature. Obviously the language already has functions, so why not make it so that a function can return a context?

With the new framing of contexts as being similar to ML-style modules, the perspective is clear: this is a functor. Functors and functions are different things. Yes, OCaml does support modules as values and thus the line gets a bit blurry, but setting that aside, functors are just a different language construct from functions. So perhaps Moss should do the same thing, and just have them be different even at the syntactic level.

The thing that I'm unsure about here is, what about `val`s? It seems like it'd be pretty useful to be able to say "take some stuff in my current context and also a couple of these values, and thread those around and give me back a context that uses those values for something". And I think this is part of why I ended up just having a `fn` be able to return a `bind` earlier. But honestly, I haven't quite thought this through well enough yet.

### \[30\] Monomorphization recursion limit

Yeah I guess this would _probably_ end up being an exception to the previous rule I stated about how no semantic checks should happen after monomorphization. I think there's precedent for this in prior work: for instance, in Rust, you can use generics to construct a function that would need a type `T` to be an infinitely-nested `Box<Box<...>>` or `Vec<Vec<...>>` or something, and if I remember correctly, rust-analyzer doesn't say anything but if you try to `cargo run` it then it fails. So this seems similar.

## 8. Expressions, statements, and patterns

D32 and D34 sound good.

### \[D31\]/\[D35\] No `for` loops for now

Overall the proposed syntax for statements looks good. I think let's just have either `while` or `loop` or maybe both of those, but let's leave out `for` for now, because if we had `for` then we'd have to come up with semantics for iterators or something like that, and I think we should punt that til a bit later.

### \[D33\] No operators for now

I'm pretty sure a lot of the existing code I've written use operators, but let's leave them out of the MVP. I think your instinct about method calls is exactly right, and that's probably exactly what we'll end up doing after the MVP. Or maybe it'll be such a small change that we decide it's worth the extra syntactic niceness to have it in the MVP.

In any case, if I remember correctly, `lib/ops.moss` is actually already up to date with the semantics I want. I don't think it'd be _strictly_ syntactic sugar, though, because we don't want the operators to refer to just any method in scope named `.add` or `.ge` or whatever: we want them to _specifically_ be symbols that refer to the declarations in `lib/ops.moss`.

## 9. Methods

### Q1

Yes, let's just use `This`.

### Q2

Similarly we now have `this`.

### Q3

Hmm, good point, I hadn't thought about this. I guess there's not really any point to allowing a detached method to be defined? So I guess let's not allow it. Unless you can think of a situation where it _would_ be useful?

### Q4

Yes, this is one of the things that make detached methods so useful for writing programs: once you've resolved the type of the object the method is being called on, and you've resolved the method itself, then everything from the square brackets where that method appeared in the context get shoved into the context for interpreting the rest of the method signature. So in this case, we don't have `Foo` in the context, but `Ctx` says that once we know we're calling `.gimme` on an `A`, we can use `Foo=B` to interpret the `Foo` that appears in the signature of `.gimme`. This is how we thread the needle to support ergonomics while not having crazy search-y synthesis stuff from the context.

### Q5

I'm pretty sure this is just an error/inconsistency in the existing code under `src/`: I think we need to only allow attached methods to be on nominal types, not abstract ones.

### Q6

The answer to this one is a bit subtle. In short, unlike in previous prototypes, definitions of contexts in the new semantics operate via _consistent merging_.

```
context Ctx1 = A, B, A.gimme[Foo=B];
context Ctx2 = A, C, A.gimme[Foo=C];
context Ctx3 = Ctx1, Ctx2;
```

A give context can only have _one_ binding for `A.gimme`. So is `Ctx3` just inconsistent? Well, no; it is quite possible for it to be satisfied consistently. All that needs to happen is for `B` and `C` to be bound to the same type. So the compiler sees it something like this:

```
context Ctx3 = A, B, C=B
```

I think there's a representational question here about symmetry: should we really have one of `B`/`C` be "primary" and then represent the other as being bound to it? I actually think probably not. Perhaps a better way would just be to have a context be two parts:

1. A set of atoms.
2. A definition of a DAG from those atoms.

So in this case, both `Ctx1` and `Ctx2` would have three atoms (one for `A`, one for `B` or `C`, and one for the `A.gimme`). And `Ctx3` would _also_ have three atoms. The only differences between `Ctx1`/`Ctx2`/`Ctx3` would be what they say about which _symbols_ relate to which atoms, if that makes sense.

For some academic literature on the specific idea of consistent merging (probably not super relevant to the broader ideas here about symbols and contexts), you can take a look at the paper ["Making a Type Difference: Subtraction on Intersection Types as Generalized Record Operations"](https://doi.org/10.1145/3571224) which my advisor sent me earlier this year, as well as some of the papers that one cites.

### Q7

I think we can let record fields and method names collide, disambiguating using the same approach Rust does.

## 10. Execution model and entry point

D39 sounds good.

### \[D38\] Bootstrap shortcut could be fine

The bootstrap shortcut you've described isn't how I'd do things, but honestly, that may just be a skill issue on my part. Obviously we have to get there for what I'd consider the MVP, but if you think it'd be easier to postpone it til we've done some other stuff first, that seems like a fine plan.

## 11. Bootstrap compiler pipeline (Python)

D40 sounds good.
