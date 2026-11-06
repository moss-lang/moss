// Included inside static_bytes.cpp's anonymous namespace. After GC type
// merging, factor identical reads out of union branches. This removes duplicate
// read paths; it does not by itself coalesce (tag, literalRef, dynamicRef) into
// a single-reference String representation.
bool dispatchLeaf(Expression *e) { return e->is<LocalGet>() || e->is<Const>(); }
bool dispatchCondition(Expression *e) {
  if (dispatchLeaf(e))
    return true;
  if (auto *unary = e->dynCast<Unary>())
    return unary->op == EqZInt32 && dispatchLeaf(unary->value);
  if (auto *binary = e->dynCast<Binary>())
    return (binary->op == EqInt32 || binary->op == NeInt32) &&
           dispatchLeaf(binary->left) && dispatchLeaf(binary->right);
  return false;
}
bool dispatchRef(Expression *e) {
  return e->is<LocalGet>() || e->is<RefNull>();
}
bool sameDispatchIndex(Expression *a, Expression *b) {
  if (auto *left = a->dynCast<LocalGet>()) {
    auto *right = b->dynCast<LocalGet>();
    return right && left->index == right->index && left->type == right->type;
  }
  if (auto *left = a->dynCast<Const>()) {
    auto *right = b->dynCast<Const>();
    return right && left->value == right->value;
  }
  return false;
}
struct ArrayDispatch : PostWalker<ArrayDispatch> {
  Builder b;
  size_t changed = 0;
  ArrayDispatch(Module &m) : b(m) {}
  void visitIf(If *e) {
    if (!e->ifFalse || !dispatchCondition(e->condition))
      return;
    Expression *left = nullptr, *right = nullptr;
    auto *a = e->ifTrue->dynCast<ArrayGet>();
    auto *c = e->ifFalse->dynCast<ArrayGet>();
    if (a && c) {
      if (a->isAtomic() || c->isAtomic() || a->signed_ != c->signed_ ||
          a->type != c->type || !sameDispatchIndex(a->index, c->index))
        return;
      left = a->ref;
      right = c->ref;
    } else {
      auto *x = e->ifTrue->dynCast<ArrayLen>();
      auto *y = e->ifFalse->dynCast<ArrayLen>();
      if (!x || !y)
        return;
      left = x->ref;
      right = y->ref;
    }
    if (!dispatchRef(left) || !dispatchRef(right) || left->type != right->type)
      return;
    // select evaluates both refs before its condition. All moved expressions
    // are effect-free and nontrapping, and the common index is also a pure
    // leaf. The actual read/null/bounds check still occurs exactly once.
    auto *ref = b.makeSelect(e->condition, left, right);
    replaceCurrent(
        a ? static_cast<Expression *>(b.makeArrayGet(
                ref, a->index, MemoryOrder::Unordered, a->type, a->signed_))
          : static_cast<Expression *>(b.makeArrayLen(ref)));
    ++changed;
  }
};
size_t simplifyArrayDispatch(Module &m) {
  ArrayDispatch fold(m);
  for (auto &f : m.functions) {
    if (f->imported())
      continue;
    auto before = fold.changed;
    fold.walk(f->body);
    if (fold.changed != before)
      f->effects.reset();
  }
  if (fold.changed)
    m.indirectCallEffects.clear();
  return fold.changed;
}
