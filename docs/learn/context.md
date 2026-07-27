# Context

In [Hello, world!](hello.md) we saw the difference between *scope* (knowing
what a name refers to) and *context* (having the thing that name refers to),
and we used a context someone else defined: `Std`. In this lesson we'll make
our own context requirements and satisfy them ourselves. Every program here
is runnable; you can copy any of them into a file and `moss` it.

## An abstract value

Here's the whole idea of Moss in eight lines ([`examples/context.moss`](/examples/context.moss)):

```moss
assume Std {
  val c: Char;

  assume c {
    fn print_it() {
      putchar(c);
      putchar(char::newline);
    }

    fn wait_for_it() {
      print_it();
    }
  }

  fn main() {
    bind c=char::h;
    wait_for_it();
  }
}
```

Running this prints `h`. Piece by piece:

- `val c: Char;` declares an *abstract value*: a name with a type but no
  definition. Nobody has said what `c` is — only that, wherever it's
  available, it's a `Char`. (Compare `putchar` in the previous lesson: also
  declared without a definition, just for a function instead of a value.)
- `assume c { ... }` wraps declarations that *require* `c`. Inside the
  braces, `c` can be used as an ordinary value. In exchange, everything
  declared inside carries the requirement outward: you can only call
  `print_it` from a place that has `c`.
- Note that `wait_for_it` never mentions `c` itself — but it calls
  `print_it`, which needs `c`, so `wait_for_it` must sit inside the
  `assume c` block too. Requirements are part of a function's interface.
- `bind c=char::h;` is how a requirement gets satisfied: from this statement
  to the end of the enclosing block, `c` *is* `char::h`. The call to
  `wait_for_it()` on the next line is inside that region, so it's allowed,
  and the `c` it (transitively) uses is `h`.

If you delete the `bind` line, the program stops compiling: `main` calls
`wait_for_it`, which needs `c`, and `main` neither assumes nor binds it. The
error says `c` is *not available in the context* — the same kind of error as
removing `assume Std` in the previous lesson, because it's the same
mechanism. `Std` is just a bundle of names like `c`.

## Rebinding

A `bind` lasts until the end of its block, and an inner `bind` shadows an
outer one. This program ([`examples/rebind.moss`](/examples/rebind.moss))
prints `h`, then `i`, then `h` again:

```moss
assume Std {
  val c: Char;

  fn hi() {
    bind c=char::i;
    print_it();
  }

  assume c {
    fn print_it() {
      putchar(c);
      putchar(char::newline);
    }

    fn hello_hi() {
      print_it();
      hi();
      print_it();
    }
  }

  fn main() {
    bind c=char::h;
    hello_hi();
  }
}
```

Two things are worth staring at:

- `hi` is *not* inside the `assume c` block. It doesn't need to be: it binds
  `c` itself before calling `print_it`, so it satisfies the requirement
  locally. A function's requirements can be met either by its own assumes or
  by binds in its body.
- The third line of output is `h`, not `i`. `hi`'s bind of `c` ended with
  `hi`'s body; back in `hello_hi`, the `c` in force is still the one `main`
  bound. Binds are *scoped*, not global mutation.

## Abstract functions

Values aren't special; you can leave a *function* abstract and bind it later:

```moss
assume Std {
  fn greet();

  assume greet {
    fn twice() {
      greet();
      greet();
    }
  }

  fn wave() {
    putchar(char::o);
    putchar(char::slash);
    putchar(char::newline);
  }

  fn main() {
    bind greet=wave;
    twice();
  }
}
```

This prints `o/` twice. `twice` has no idea what `greet` does — it only knows
the signature. `main` decides.

One subtlety that makes this powerful: when you bind a function, the
function you provide brings *its own context with it*, captured at the bind
site. If `wave` had needed some `val` of its own, `main` would have had to
have that val available when it wrote `bind greet=wave;` — and `twice`
could still call `greet` without ever knowing about it. Requirements never
leak through a bind.

## Naming a bundle: `context`

Once you have a few requirements that travel together, name them:

```moss
assume Std {
  val first: Char;
  val second: Char;

  context Greeting = first, second;

  assume Greeting {
    fn greet() {
      putchar(first);
      putchar(second);
      putchar(char::newline);
    }
  }

  fn main() {
    bind first=char::h;
    bind second=char::i;
    greet();
  }
}
```

A `context` declaration is nothing but a named list of requirements —
`assume Greeting` means exactly `assume first, second`. `Std` itself is
declared this way in the standard library: a long list of the types, values,
and functions you've been using. There is no magic in it; you could write
your own.

---

Next up: types can be abstract too — the `Char` you've been assuming out of
`Std` is one — and methods, which let one context provide the same
operation for many different types. Those lessons still need the language to
sit still for a moment first.
