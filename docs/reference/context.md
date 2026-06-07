# Context

Every top-level declaration includes **Needs** which declare the context that declaration assumes to be available. This can be thought of as a form of "semantic scoping," which is distinct from lexical scoping: just because a symbol can be resolved statically does not mean that the referent of that symbol is available dynamically.

Similar to lexical scoping, at each program point there is a stack of contexts. When something needs to be satisfied from context, it uses the highest context in the stack which can satisfy that need.

One big difference is that, while an individual scope in the stack can only map a symbol to at most one thing, a context may have multiple entries that simultaneously match a given need. To deal with this, every context has a partial order defined on its entries. So there are three possibilities:

- Nothing matches. Resolution proceeds to the next lower context in the stack.
- The subset of matches contains a unique upper bound modulo the partial order. That is the resolution.
- The subset of matches does not contain an upper bound. Resolution fails due to ambiguity.

## **List**\[**Need**\]

A **List**\[**Need**\] in the program syntax semantically becomes an individual context in the stack. We define this via iteration: an empty list corresponds to the empty context, and to turn a nonempty **List**\[**Need**\] into a context, we take the context from the list of all but the last **Need**, and then fold in that last **Need**. Thus, the semantics of a **List**\[**Need**\] are order-dependent.

To fold in a **Need**, first the right-hand-side **Entry** is resolved. If it is a **Literal**, this is trivial. So the general case is that the right-hand side is a **Spec**.

## **Spec**

## **List**\[**Binding**\]
