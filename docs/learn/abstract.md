# Abstract types

The [Context](context.md) lesson left abstract values and functions bound;
this one does the same for *types* — and answers a question that's been
hanging since the first lesson: what, exactly, is `Char`?

## A type someone else chooses

A `type` declaration with no definition is an abstract type: a name in
scope, a meaning to be provided.

```moss
type T;

assume Std, T {
  fn keep(x: T): T {
    x
  }
}

assume Std {
  fn main() {
    bind T=Char;
    putchar(keep(char::k));
    putchar(char::newline);
  }
}
```

This prints `k`. `keep` is written against a type it knows nothing about —
it can only pass `T` values along, never invent or inspect them. `main`
chooses what `T` is with `bind T=Char;`, exactly the way it bound values in
the earlier lesson, except that binding a *type* happens entirely at compile
time.

If this smells like generics, that's because it is. Moss has no type
parameter lists; a function is generic when its requirements include a type
symbol, and instantiation is a `bind` (or an application like `keep[T=Char]`
inside a context, which we'll meet below).

## Types built on abstract types

Everything from the [Types](types.md) lesson can mention an abstract type:

```moss
type T;

assume Std, T {
  type Pair { first: T, second: T };

  fn swap(p: Pair): Pair {
    match p {
      Pair { first, second } => Pair { first = second, second = first },
    }
  }
}

assume Std {
  fn main() {
    bind T=Char;
    let p = Pair { first = char::a, second = char::b };
    putchar(swap(p).first);
    putchar(char::newline);
  }
}
```

This prints `b`. `Pair` is a generic pair type and `swap` a generic
function, written with no bracketed type parameters anywhere — the shared
symbol `T` ties them together. Two different regions that bind `T`
differently get two different `Pair` types; a region that binds `T=Char`
once gets one `Pair`, coherently, everywhere it looks.

## Detached methods

Attached methods (`fn Point.show()`) name their receiver up front. A
*detached* method declares only the shape, leaving the receiver to be
supplied per type:

```moss
assume Std {
  fn .loud(): Char;

  type Horn Char;

  fn Horn.blast(): Char {
    match this { Horn c => c }
  }

  fn main() {
    bind Horn.loud=Horn.blast;
    let h = Horn (char::t);
    putchar(h.loud());
    putchar(char::newline);
  }
}
```

This prints `t`. The declaration `fn .loud(): Char;` creates the method
symbol; `bind Horn.loud=Horn.blast;` provides it *at the type `Horn`*, using
an attached method as the implementation (the only kind of provider that can
see the receiver as `this`). Other types could get their own `.loud` in the
same scope — the provisions don't collide, because a detached method is
keyed by the pair (receiver type, method).

That pair-keying is the answer to how the standard library gives many types
the same operations: `.eq` is declared *once*, detached, in the library, and
`Std` provides it at `Int`, at `Char`, and so on — `Int.eq` and `Char.eq`
are different keys carrying different implementations. In a detached
signature you can also write `This` for "the receiver's type", which is how
`.add(rhs: This): This` says that adding an `Int` to an `Int` gives an
`Int`.

## So what is `Char`?

Abstract. `lib/char.moss` says `type Char;` and nothing more; `Std` includes
it, along with a hundred abstract `val`s like `char::a` and detached-method
provisions like `Char.eq`. When your `main` runs, the runtime provides
implementations for all of it — today from the interpreter, eventually from
generated WebAssembly. Nothing in the language knows what a character *is*;
your whole program is written against a context somebody satisfies at the
last moment. That is the design, all the way down: `main` is just a function
with requirements, and running a program is one big `bind`.
