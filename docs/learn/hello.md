# Hello, world!

Now that you've installed Moss, you're ready to run your first Moss program! Create a file called `hello.moss` with these contents:

```moss
assume Std;

fn main() {
  println("Hello, world!");
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
assume Std;
```

An `assume` statement takes some context (`Std`, in this case), and causes everything in the same file to automatically assume that that context is available. OK, but what does that mean exactly? Well, try removing this `assume` line from the file and re-running it. You should see a couple errors, one pointing at `"Hello, world!"` saying there is no `String` type available in the context, and the other pointing at `println` saying that function is not available in the context.

If you've used other programming languages, the latter error may seem like it's saying that `println` is not in _scope_. But this is actually not the case, as we'll discuss shortly. To see this, try using a function name that is _actually_ not in scope:

```moss
fn main() {
  foo();
}
```

If you try to run this, you'll get a different error saying that there is no function named `foo` in scope. The short explanation for now is, scoping is about knowing _what_ a name refers to, whereas context is about _having_ the thing that name refers to.

OK, so, do we need to have an `assume` statement at the top of every program? Actually no! Really, `assume` is just a convenience; it's always possible to rewrite your program without it. Here's what that would look like in our case:

```moss
fn main[Std]() {
  println("Hello, world!");
}
```

The square brackets are used to list the context that a function assumes. We didn't see them earlier because they're optional if the function doesn't assume anything beyond the file-wide context already declared by any `assume` statement above. But you can still write them if you want:

```moss
assume Std;

fn main[]() {
  println("Hello, world!");
}
```

### `Std`

The specific context we're using in this program is called `Std`, and it provides all the things you'd expect from a normal programming language: primitive types, functions to manipulate those types, input/output, etc. If you're writing a complex or specialized program then you may very well decide to not use `Std` for some or all of your codebase, but for many use cases, the typical pattern is to `assume Std` at the top of each file, and any other context is layered on top of `Std`, as we'll see in later examples.

Where does `Std` come from? It's defined in the [_prelude_](/lib/prelude.moss), which provides a set of symbols that are automatically imported in every file of Moss source code (other than those that make up the standard library itself). This relates back to our earlier discussion on the difference between scope and context: the prelude brings `Std` into _scope_, so you can refer to it by name, but that doesn't mean you _have_ access to everything in `Std` automatically. That requires either an `assume` or listing the context in square brackets.

### `fn`

The `fn` keyword declares a function! This is pretty standard. We've already discussed the optional square brackets for declaring the context of the function. The parentheses `()` are used to declare function parameters, which in this case isn't very interesting since `main` takes no parameters. Then the curly braces denote the body of the function. Since there's no colon `:` between the (empty) parameter list and the opening curly brace `{`, this function implicitly returns the "unit type", which you could write explicitly if you want:

```moss
fn main(): () {
  println("Hello, world!");
}
```

This is just a fancy way of saying it returns nothing: or more precisely, there's only one possible thing it could return, so the returned value holds no information.

### `main`

The name `main` is special. When you run a Moss program via the `moss` command in your terminal, it must contain a function named `main` that takes no parameters and returns the unit type. This is the function that will actually get run. The `main` function can call other functions, of course, but if there's any function that `main` doesn't call (either directly or indirectly), then that function simply doesn't get run at all.

The `main` function actually has a special relationship with the `Std` context. Although `main` takes no _parameters_ per se, our program _does_ assume that it has access to everything in the `Std` context, as we've discussed before. That is, not only can we refer to things like `println` by name, we can actually use them. Where do these come from? Well, when Moss runs a program, it automatically provides the `Std` context to the `main` function.

But wait, in that case, why did we get errors when we removed `assume Std` from our program? That's because, although Moss does provide tools like `assume` to make it more convenient to declare what context we assume, the language still requires that every function _does_ declare the context it needs in some way, whether via `assume` or via explicit square brackets or something else. So if our program doesn't say anything to indicate that `main` assumes `Std`, it can't use it. Moss will still try to provide `Std` to `main` when it runs it, but if `main` doesn't declare that it needs `Std`, it just gets dropped and thrown away.

So, perhaps it'd be more accurate to say that `main` can assume any _subset_ of the `Std` context. If it assumes less, that's fine; the parts it doesn't assume will simply be thrown away. But if it assumes _more_, that's an error. For instance, as a sneak peek at how we'll later be able to declare our own custom contexts, here's an example that would throw an error due to `main` assuming too much:

```moss
fn example();

fn main[example]() {
  example();
}
```

If you try to run this, you'll get an error saying that `example` is not part of the `Std` context, so `main` is not allowed to assume that `example` is available in its context.

### `println`

The `println` function is pretty straightforward, it just prints a string followed by a newline. If you want to print the same string without a starting a new line at the end, you can just use `print`:

```moss
assume Std;

fn main() {
  print("Hello, world!");
}
```

The `println` function is part of the prelude too, so we can refer to it by name. It's defined by calling `print` twice (once with our string and once with the newline character), and `print` is part of the `Std` context, which is why writing `assume Std` then allows us to call `println` from `main`.

### `"Hello, world!"`

Surprisingly, this is actually the most "magical" part of the whole program. Remember how earlier we saw that, without `Std`, we get an error not just on the call to `println`, but on the `"Hello, world!"` string literal itself? This is because writing `"Hello, world!"` _creates_ a `String` with those thirteen characters as its contents. So, there needs to be somewhere to put those contents. Where do we put them? Well, of course, we put them in memory. So our program assumes that it's running on a computer that has memory, and it uses that memory to store the thirteen characters of the string `"Hello, world!"`.

The key word here is "assumes": this is part of our context! Not only does `Std` include functions like `print`, it also includes your computer's memory and operations to access that memory. So without `Std`, there's nowhere to put the bytes of a string literal, hence the error.

---

And that's it! Don't worry if anything from the above explanations didn't make complete sense; we'll explore all these topics in more detail soon.
