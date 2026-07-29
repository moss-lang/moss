# Types

So far every value we've touched came from `Std`. In this lesson we make our
own types. Every program here runs; try them.

## Units and unions

The simplest type you can declare has exactly one value, which shares its
name:

```moss
assume Std {
  unit Red;
  unit Green;
  unit Blue;

  type Color = | Red | Green | Blue;

  fn code(c: Color): Char {
    match c {
      Red => char::r,
      Green => char::g,
      Blue => char::b,
    }
  }

  fn main() {
    putchar(code(Green));
    putchar(code(Red));
    putchar(char::newline);
  }
}
```

This prints `gr`. A `unit` declaration gives you a type and its single value
in one stroke, and a union type like `Color` says a value is one of several
such alternatives. Two things the compiler enforces for you:

- Passing `Green` where a `Color` is expected just works: a value of a
  member type *injects* into any union that lists it.
- The `match` must be exhaustive. Delete the `Blue` arm and the program
  stops compiling with an error naming the missing case. (A `_` arm accepts
  everything, if that's what you mean.)

## Tags

A `type` declaration whose name is followed by another type wraps a payload
— we call it a *tag*:

```moss
assume Std {
  type Wrapped Char;

  unit Missing;

  type Maybe = | Missing | Wrapped;

  fn show(m: Maybe) {
    match m {
      Wrapped c => putchar(c),
      Missing => putchar(char::question),
    }
    putchar(char::newline);
  }

  fn main() {
    show(Wrapped (char::w));
    show(Missing);
  }
}
```

This prints `w`, then `?`. `Wrapped (char::w)` constructs a tag value;
matching with `Wrapped c` takes it apart, binding the payload. Tags are
*nominal*: a `Wrapped` is not a `Char`, even though it carries one — you must
put the value in and take it out explicitly. That's what lets `Maybe`
discriminate its members at runtime.

You've already been using a type built this way. `Bool` is declared in the
standard library as a tag over a union of two units, and `true` and `false`
are just values defined once and for all:

```moss
unit False;
unit True;

type Bool | False | True;

val false: Bool = Bool (False);
val true: Bool = Bool (True);
```

## Records

A tag's payload can be a record, which gets you named fields:

```moss
assume Std {
  type Point { x: Char, y: Char };

  fn Point.show() {
    putchar(char::lparen);
    putchar(this.x);
    putchar(char::comma);
    putchar(this.y);
    putchar(char::rparen);
    putchar(char::newline);
  }

  fn flip(p: Point): Point {
    match p {
      Point { x, y } => Point { x = y, y = x },
    }
  }

  fn main() {
    let p = Point { x = char::a, y = char::b };
    p.show();
    flip(p).show();
  }
}
```

This prints `(a,b)` then `(b,a)`. Three constructions worth noting:

- `Point { x = char::a, y = char::b }` builds the record; every field must
  be given. In `flip`, `Point { x = y, y = x }` builds one with the fields
  swapped.
- `Point { x, y }` in a pattern destructures, binding each field to a local
  of the same name (you can rename with `x = somewhere_else`).
- Field access reads through the tag: `this.x` works directly on a `Point`.

## Methods

`flip` is an ordinary function, but `show` is something new: a *method*
attached to `Point`. Inside its body, `this` is the receiver — the `p` in
`p.show()`. A method declared this way, `fn Point.show()`, is called an
*attached* method, and it must attach to a specific nominal type like a tag
or a unit.

There is a second kind — *detached* methods, declared without a receiver
(`fn .show();`) and provided per type through the context machinery from the
previous lesson. They're how the standard library gives you `.add` on `Int`
and `.eq` on `Char` from one declaration. That story deserves its own
lesson, once you've seen abstract types — which is also where `Char` itself,
a type you've assumed from `Std` all along without anyone defining it, gets
explained.
