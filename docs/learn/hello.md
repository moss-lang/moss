# Hello, world!

Now that you've installed Moss, you're ready to run your first Moss program! Create a file called `hello.moss` with these contents:

```moss
assume Std {
  fn main() {
    putchar(char::H);
    putchar(char::e);
    putchar(char::l);
    putchar(char::l);
    putchar(char::o);
    putchar(char::comma);
    putchar(char::space);
    putchar(char::w);
    putchar(char::o);
    putchar(char::r);
    putchar(char::l);
    putchar(char::d);
    putchar(char::exclam);
    putchar(char::newline);
  }
}
```

Now run that file like this:

```sh
moss hello.moss
```

You should see this output:

```
Hello, world!
```

## Breaking it down

Let's go through this "Hello, world!" program piece by piece.

### `assume`

We'll start with the first line:

```moss
assume Std {
```

An `assume` block takes some context (`Std`, in this case), and causes everything within the following curly braces to assume that that context is available. OK, but what does that mean exactly? Well, try removing this `assume` line (and the corresponding closing curly brace) from the file and re-running it. You should see multiple errors, pointing at `putchar` saying that function is not available in the context, and pointing at the `char::*` values saying they are not available in the context.

If you've used other programming languages, thse errors may seem like they're saying that `putchar` is not in _scope_. But this is actually not the case, as we'll discuss shortly. To see this, try using a function name that is _actually_ not in scope:

```moss
fn main() {
  foo();
}
```

If you try to run this, you'll get a different error saying that there is no function named `foo` in scope. The short explanation for now is, scoping is about knowing _what_ a name refers to, whereas context is about _having_ the thing that name refers to.

### `Std`

The specific context we're using in this program is called `Std`, and it provides most of the things you'd expect from a normal programming language: primitive types, functions to manipulate those types, input/output, etc. If you're writing a complex or specialized program then you may very well decide to not use `Std` for some or all of your codebase, but for many use cases, the typical pattern is to `assume Std` at the top of each file, and any other context is layered on top of `Std`, as we'll see in later examples.

Where does `Std` come from? It's defined in the [_prelude_](/lib/prelude.moss), which provides a set of symbols that are automatically imported in every file of Moss source code (other than those that make up the standard library itself). This relates back to our earlier discussion on the difference between scope and context: the prelude brings `Std` into _scope_, so you can refer to it by name, but that doesn't mean you _have_ access to everything in `Std` automatically. That requires an `assume` block.

### `fn`

The `fn` keyword declares a function! This is pretty standard. The parentheses `()` are used to declare function parameters, which in this case isn't very interesting since `main` takes no parameters. Then the curly braces denote the body of the function. Since there's no colon `:` between the (empty) parameter list and the opening curly brace `{`, this function implicitly returns the "unit type", which you could write explicitly if you want:

```moss
fn main(): () {
  println("Hello, world!");
}
```

This is just a fancy way of saying it returns nothing: or more precisely, there's only one possible thing it could return, so the returned value holds no information.

### `main`

The name `main` is special. When you run a Moss program via the `moss` command in your terminal, it must contain a function named `main` that takes no parameters and returns the unit type. This is the function that will actually get run. The `main` function can call other functions, of course, but if there's any function that `main` doesn't call (either directly or indirectly), then that function simply doesn't get run at all.

The `main` function actually has a special relationship with the `Std` context. Although `main` takes no _parameters_ per se, our program _does_ assume that it has access to everything in the `Std` context, as we've discussed before. That is, not only can we refer to things like `putchar` by name, we can actually use them. Where do these come from? Well, when Moss runs a program, it automatically provides the `Std` context to the `main` function.

But wait, in that case, why did we get errors when we removed `assume Std` from our program? That's because the language still requires that every function declares the context it needs by being inside of an appropriate `assume` block. So if our program doesn't say anything to indicate that `main` assumes `Std`, it can't use it. Moss will still try to provide `Std` to `main` when it runs it, but if `main` doesn't declare that it needs `Std`, it just gets dropped and thrown away.

So, perhaps it'd be more accurate to say that `main` can assume any _subset_ of the `Std` context. If it assumes less, that's fine; the parts it doesn't assume will simply be thrown away. But if it assumes _more_, that's an error. For instance, as a sneak peek at how we'll later be able to declare our own custom contexts, here's an example that would throw an error due to `main` assuming too much:

```moss
fn example();

assume example {
  fn main() {
    example();
  }
}
```

If you try to run this, you'll get an error saying that `example` is not part of the `Std` context, so `main` is not allowed to assume that `example` is available in its context.

### `putchar`

The `putchar` function is pretty straightforward, it just prints a character to stdout. It's part of the prelude, so we can refer to it by name.

### `char::*`

Moss is still experimental, so the set of features is still quite limited. In particular, there are currently no string literals, or literals of any kind. As a result, this simple "Hello, world!" program needs to awkwardly list every single character in the string instead of just having one string literal.

The double colon `::` indicates the access of a symbol inside of a namespace. The prelude provides a `char` symbol which points to a module containing many different character values. So, `char::e` accesses the symbol `e` from that `char` module. All these characters are provided by the `Std` context.

---

And that's it! Don't worry if anything from the above explanations didn't make complete sense; we'll explore all these topics in more detail soon.
