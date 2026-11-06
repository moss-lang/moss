// Recognize a construction-only mutation phase. The destination is freshly
// allocated into a local, followed immediately by a pure loop. The local is
// never reassigned during that phase; earlier/later assignments are harmless.
// It cannot escape before or during that loop. Subsequent mutations still fail
// the ordinary type-wide use check. This proof is useful for runtime builders:
// their backing remains GC allocated while constant constructions of the same
// array type may be pooled.
struct ArrayInitializations {
  std::unordered_set<Expression *> origins, writes;
  static bool pure(Expression *e, Index destination,
                   std::unordered_set<Expression *> &stores, size_t &budget) {
    if (!budget)
      return false;
    --budget;
    if (auto *get = e->dynCast<LocalGet>())
      return get->index != destination && get->type == Type::i32;
    if (auto *set = e->dynCast<LocalSet>())
      return set->index != destination && set->value->type == Type::i32 &&
             pure(set->value, destination, stores, budget);
    if (auto *set = e->dynCast<ArraySet>()) {
      auto *ref = set->ref->dynCast<LocalGet>();
      if (!ref || ref->index != destination || set->isAtomic() ||
          !pure(set->index, destination, stores, budget) ||
          !pure(set->value, destination, stores, budget))
        return false;
      stores.insert(set);
      return true;
    }
    if (auto *get = e->dynCast<ArrayGet>()) {
      auto *ref = get->ref->dynCast<LocalGet>();
      return ref && ref->index != destination && !get->isAtomic() &&
             get->type == Type::i32 &&
             pure(get->index, destination, stores, budget);
    }
    if (auto *len = e->dynCast<ArrayLen>()) {
      auto *ref = len->ref->dynCast<LocalGet>();
      return ref && ref->index != destination;
    }
    if (auto *br = e->dynCast<Break>(); br && br->value)
      return false;
    if (!(e->is<Block>() || e->is<Loop>() || e->is<If>() || e->is<Break>() ||
          e->is<Const>() || e->is<Unary>() || e->is<Binary>() || e->is<Nop>() ||
          e->is<Drop>()))
      return false;
    for (auto *child : ChildIterator(e))
      if (!pure(child, destination, stores, budget))
        return false;
    return true;
  }
  struct Blocks : PostWalker<Blocks> {
    ArrayInitializations &result;
    explicit Blocks(ArrayInitializations &r) : result(r) {}
    void visitBlock(Block *block) {
      for (size_t i = 0; i + 1 < block->list.size(); ++i) {
        auto *set = block->list[i]->dynCast<LocalSet>();
        if (!set || set->isTee())
          continue;
        auto *allocation = set->value;
        if (!(allocation->is<ArrayNew>() || allocation->is<ArrayNewFixed>()) ||
            !allocation->type.isRef() ||
            !byteArray(allocation->type.getHeapType()))
          continue;
        // Intervening scalar initialization may not even read the fresh ref.
        size_t next = i + 1;
        for (; next < block->list.size(); ++next) {
          auto *scalar = block->list[next]->dynCast<LocalSet>();
          if (!scalar || scalar->value->type != Type::i32)
            break;
          std::unordered_set<Expression *> none;
          size_t budget = 256;
          if (!pure(scalar, set->index, none, budget) || !none.empty())
            break;
        }
        if (next >= block->list.size() || !block->list[next]->is<Loop>())
          continue;
        std::unordered_set<Expression *> stores;
        size_t budget = 4096;
        if (pure(block->list[next], set->index, stores, budget) &&
            !stores.empty()) {
          result.origins.insert(allocation);
          result.writes.insert(stores.begin(), stores.end());
        }
      }
    }
  };
  explicit ArrayInitializations(Module &module) {
    for (auto &f : module.functions) {
      if (f->imported())
        continue;
      Blocks(*this).walk(f->body);
    }
  }
};
