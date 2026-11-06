// Recognize bounded GC construction/copy helpers and constant copy loops. This
// knows no source names or literal protocol. Unsupported effects/control flow
// invalidate facts; normalization never relies on a provider annotation.
struct CopyHelperShape
    : PostWalker<CopyHelperShape, UnifiedExpressionVisitor<CopyHelperShape>> {
  size_t count = 0;
  bool safe = true, fresh = false, read = false, write = false, loop = false;
  void visitExpression(Expression *e) {
    if (++count > 256)
      safe = false;
    fresh |= e->is<ArrayNew>() || e->is<ArrayNewFixed>();
    read |= e->is<ArrayGet>();
    write |= e->is<ArraySet>();
    loop |= e->is<Loop>();
    if (e->is<Const>() || e->is<LocalGet>() || e->is<LocalSet>() ||
        e->is<Drop>() || e->is<Nop>() || e->is<Block>() || e->is<If>() ||
        e->is<Loop>() || e->is<Break>() || e->is<Return>() || e->is<Unary>() ||
        e->is<Binary>() || e->is<ArrayNew>() || e->is<ArrayNewFixed>() ||
        e->is<ArrayGet>() || e->is<ArraySet>() || e->is<ArrayLen>() ||
        e->is<TupleMake>() || e->is<TupleExtract>() || e->is<Select>() ||
        e->is<RefNull>())
      return;
    safe = false;
  }
};
size_t inlineArrayCopyHelpers(Module &m) {
  std::unordered_map<Name, std::optional<uint8_t>> hints;
  for (auto &f : m.functions) {
    if (f->imported() ||
        f->funcAnnotations.toolchainInline == CodeAnnotation::NeverInline)
      continue;
    bool resultArray = false, resultSafe = true;
    for (auto part : f->getResults()) {
      if (part == Type::i32)
        continue;
      if (!part.isRef()) {
        resultSafe = false;
        break;
      }
      auto type = part.getHeapType();
      if (type == HeapType::none)
        continue;
      if (!byteArray(type)) {
        resultSafe = false;
        break;
      }
      resultArray = true;
    }
    if (!resultSafe || !resultArray)
      continue;
    CopyHelperShape shape;
    shape.walk(f->body);
    // Leaf writers expose initialization stores even when constant selector
    // arguments select among several record/union payload lanes. Inlining
    // preserves all operand effects and traps; eligibility is proved later.
    bool writer = shape.write && !shape.fresh && !shape.read && !shape.loop;
    bool copy = shape.fresh && shape.read && shape.write && shape.loop;
    if (shape.safe && (writer || copy)) {
      hints.emplace(f->name, f->funcAnnotations.toolchainInline);
      f->funcAnnotations.toolchainInline = CodeAnnotation::AlwaysInline;
    }
  }
  if (hints.empty())
    return 0;
  PassRunner runner(&m);
  runner.options.optimizeLevel = 2;
  runner.options.inlining.allowFunctionsWithLoops = true;
  runner.add("inlining-optimizing");
  runner.add("simplify-locals");
  runner.add("optimize-instructions");
  runner.add("precompute");
  runner.add("vacuum");
  runner.run();
  for (auto [name, hint] : hints)
    if (auto *f = m.getFunctionOrNull(name))
      f->funcAnnotations.toolchainInline = hint;
  return hints.size();
}

struct ConstantArrayCopies {
  struct Array {
    HeapType type;
    std::vector<int32_t> values;
    Expression **origin;
    std::vector<std::pair<Index, int32_t>> effects;
    bool valid = true, observed = false, modified = false;
  };
  struct Value {
    std::optional<int32_t> number;
    std::shared_ptr<Array> array;
    Value() = default;
    Value(int32_t n) : number(n) {}
    Value(std::shared_ptr<Array> a) : array(std::move(a)) {}
  };
  struct State {
    std::vector<Value> locals;
    Name exit;
    bool terminal = false;
    State(Function &f) : locals(f.getNumLocals()) {
      for (Index i = f.getNumParams(); i < f.getNumLocals(); ++i)
        if (f.getLocalType(i) == Type::i32)
          locals[i] = Value(0);
    }
    void invalidateArrays() {
      for (auto &v : locals)
        if (v.array) {
          v.array->valid = false;
          v.array.reset();
        }
    }
    void clear() {
      for (auto &v : locals) {
        if (v.array)
          v.array->valid = false;
        v = Value();
      }
    }
  };
  Module &m;
  Builder b;
  Function *owner;
  size_t remaining = 100000, changes = 0;
  ConstantArrayCopies(Module &m, Function *f) : m(m), b(m), owner(f) {}
  int32_t stored(HeapType type, int32_t n) {
    return type.getArray().element.packedType == Field::i8
               ? int32_t(uint32_t(n) & 255)
               : n;
  }
  std::optional<int32_t>
  scalar(Expression *e, State &s,
         std::vector<std::pair<Index, int32_t>> *effects = nullptr) {
    if (auto n = integer(e))
      return n;
    if (auto *get = e->dynCast<LocalGet>())
      return s.locals[get->index].number;
    if (auto *set = e->dynCast<LocalSet>(); set && set->isTee()) {
      // Ordinary scalar probing must not execute effects speculatively before
      // the fallback walker visits children. Constructor captures reify them.
      if (!effects)
        return {};
      auto value = scalar(set->value, s, effects);
      if (!value)
        return {};
      s.locals[set->index] = Value(*value);
      if (effects)
        effects->emplace_back(set->index, *value);
      return value;
    }
    if (auto *len = e->dynCast<ArrayLen>()) {
      if (auto *get = len->ref->dynCast<LocalGet>()) {
        auto a = s.locals[get->index].array;
        if (a && a->valid)
          return int32_t(a->values.size());
      }
    }
    if (auto *op = e->dynCast<Binary>()) {
      auto a = scalar(op->left, s, effects), c = scalar(op->right, s, effects);
      if (!a || !c)
        return {};
      auto x = uint32_t(*a), y = uint32_t(*c);
      switch (op->op) {
      case AddInt32:
        return int32_t(x + y);
      case SubInt32:
        return int32_t(x - y);
      case AndInt32:
        return int32_t(x & y);
      case OrInt32:
        return int32_t(x | y);
      case XorInt32:
        return int32_t(x ^ y);
      case LtUInt32:
        return x < y;
      case GeUInt32:
        return x >= y;
      case EqInt32:
        return x == y;
      case NeInt32:
        return x != y;
      default:
        return {};
      }
    }
    if (auto *op = e->dynCast<Unary>(); op && op->op == EqZInt32) {
      auto n = scalar(op->value, s, effects);
      if (n)
        return !*n;
    }
    return {};
  }
  std::shared_ptr<Array> localArray(Expression *e, State &s) {
    if (auto *get = e->dynCast<LocalGet>())
      return s.locals[get->index].array;
    return {};
  }
  bool copyLoop(Expression *&e, State &s) {
    auto *loop = e->dynCast<Loop>();
    if (!loop)
      return false;
    auto *branch = loop->body->dynCast<If>();
    if (!branch || branch->ifFalse)
      return false;
    auto *condition = branch->condition->dynCast<Binary>();
    if (!condition || condition->op != LtUInt32)
      return false;
    auto *counter = condition->left->dynCast<LocalGet>();
    if (auto *boundGet = condition->right->dynCast<LocalGet>();
        counter && boundGet && boundGet->index == counter->index)
      return false;
    auto *boundExpr = condition->right;
    bool pureBound =
        integer(boundExpr).has_value() || boundExpr->is<LocalGet>();
    if (auto *len = boundExpr->dynCast<ArrayLen>())
      pureBound = len->ref->is<LocalGet>();
    if (!pureBound)
      return false;
    auto bound = scalar(boundExpr, s);
    if (!counter || !bound || *bound < 0 || size_t(*bound) > MaxFixedElements ||
        s.locals[counter->index].number != 0)
      return false;
    auto *body = branch->ifTrue->dynCast<Block>();
    if (!body)
      return false;
    std::vector<Expression *> ops;
    for (auto *op : body->list)
      if (!op->is<Nop>())
        ops.push_back(op);
    if (ops.size() != 3)
      return false;
    auto *set = ops[0]->dynCast<ArraySet>();
    auto *increment = ops[1]->dynCast<LocalSet>();
    auto *back = ops[2]->dynCast<Break>();
    if (!set || set->isAtomic() || !increment || increment->isTee() ||
        increment->index != counter->index || !back ||
        back->name != loop->name || back->condition || back->value)
      return false;
    auto *read = set->value->dynCast<ArrayGet>();
    auto *add = increment->value->dynCast<Binary>();
    if (!read || read->isAtomic() || !add || add->op != AddInt32 ||
        integer(add->right) != 1)
      return false;
    auto same = [&](Expression *x) {
      auto *g = x->dynCast<LocalGet>();
      return g && g->index == counter->index;
    };
    if (!same(set->index) || !same(read->index) || !same(add->left))
      return false;
    auto src = localArray(read->ref, s), dest = localArray(set->ref, s);
    if (!src || !dest || !src->valid || !dest->valid || src == dest ||
        dest->modified || dest->observed ||
        src->values.size() != size_t(*bound) ||
        dest->values.size() != size_t(*bound) || size_t(*bound) > remaining)
      return false;
    remaining -= size_t(*bound);
    auto values = src->values;
    if (read->signed_ && src->type.getArray().element.packedType == Field::i8)
      for (auto &value : values)
        value = int8_t(uint8_t(value));
    for (auto &value : values)
      value = stored(dest->type, value);
    std::vector<Expression *> replacement;
    for (auto [local, value] : dest->effects)
      replacement.push_back(b.makeLocalSet(local, b.makeConst(value)));
    replacement.push_back(fixed(b, dest->type, values));
    *dest->origin =
        replacement.size() == 1 ? replacement[0] : b.makeBlock(replacement);
    dest->values = std::move(values);
    e = b.makeLocalSet(counter->index, b.makeConst(*bound));
    s.locals[counter->index] = Value(*bound);
    ++changes;
    return true;
  }
  Value scan(Expression *&e, State &s) {
    if (!remaining) {
      s.clear();
      s.terminal = true;
      return {};
    }
    --remaining;
    if (auto *block = e->dynCast<Block>()) {
      Value result;
      for (auto *&item : block->list) {
        result = scan(item, s);
        if (s.terminal || s.exit) {
          if (!s.terminal && s.exit == block->name) {
            s.exit = Name();
            s.clear();
          }
          return {};
        }
      }
      return result;
    }
    if (auto *get = e->dynCast<LocalGet>())
      return s.locals[get->index];
    if (auto n = integer(e))
      return Value(*n);
    if (auto *set = e->dynCast<LocalSet>()) {
      auto value = scan(set->value, s);
      s.locals[set->index] = value;
      return set->isTee() ? value : Value();
    }
    if (auto *node = e->dynCast<ArrayNewFixed>();
        node && e->type.isRef() && byteArray(e->type.getHeapType())) {
      if (node->values.size() > MaxFixedElements ||
          node->values.size() > remaining) {
        s.clear();
        s.terminal = true;
        return {};
      }
      std::vector<int32_t> values;
      for (auto *op : node->values) {
        auto n = integer(op);
        if (!n) {
          s.invalidateArrays();
          for (auto *&operand : node->values) {
            scan(operand, s);
            if (s.terminal || s.exit)
              break;
          }
          return {};
        }
        values.push_back(stored(e->type.getHeapType(), *n));
      }
      remaining -= values.size();
      return Value(std::make_shared<Array>(
          Array{e->type.getHeapType(), std::move(values), &e, {}}));
    }
    if (auto *node = e->dynCast<ArrayNew>();
        node && e->type.isRef() && byteArray(e->type.getHeapType())) {
      auto init = node->init ? integer(node->init) : std::optional<int32_t>(0);
      std::vector<std::pair<Index, int32_t>> effects;
      auto before = s.locals;
      auto n = scalar(node->size, s, &effects);
      if (!init || !n || *n < 0 || size_t(*n) > MaxFixedElements ||
          size_t(*n) > remaining) {
        s.locals = std::move(before);
        s.invalidateArrays();
        if (node->init)
          scan(node->init, s);
        if (!s.terminal && !s.exit)
          scan(node->size, s);
        return {};
      }
      remaining -= size_t(*n);
      return Value(std::make_shared<Array>(
          Array{e->type.getHeapType(),
                std::vector<int32_t>(*n, stored(e->type.getHeapType(), *init)),
                &e, std::move(effects)}));
    }
    if (auto *len = e->dynCast<ArrayLen>()) {
      auto a = scan(len->ref, s).array;
      return a && a->valid ? Value(int32_t(a->values.size())) : Value();
    }
    if (auto *get = e->dynCast<ArrayGet>()) {
      auto a = scan(get->ref, s).array;
      auto at = scan(get->index, s).number;
      if (a && a->valid) {
        a->observed = true;
        if (at && uint32_t(*at) < a->values.size()) {
          auto n = a->values[uint32_t(*at)];
          if (get->signed_ &&
              a->type.getArray().element.packedType == Field::i8)
            n = int8_t(uint8_t(n));
          return Value(n);
        }
      }
      return {};
    }
    if (auto *set = e->dynCast<ArraySet>()) {
      auto a = scan(set->ref, s).array;
      auto at = scan(set->index, s).number;
      auto n = scan(set->value, s).number;
      if (a && a->valid && at && n && uint32_t(*at) < a->values.size() &&
          !set->isAtomic()) {
        a->values[uint32_t(*at)] = stored(a->type, *n);
        a->modified = true;
      } else {
        if (a)
          a->valid = false;
        else
          s.clear();
      }
      return {};
    }
    if (e->is<Loop>()) {
      if (copyLoop(e, s))
        return {};
      s.clear();
      State inner(*owner);
      inner.clear();
      scan(e->cast<Loop>()->body, inner);
      s.clear();
      if (inner.terminal || (inner.exit && inner.exit != e->cast<Loop>()->name))
        s.terminal = true;
      return {};
    }
    if (auto *branch = e->dynCast<If>()) {
      auto condition = scan(branch->condition, s).number;
      if (condition)
        return *condition
                   ? scan(branch->ifTrue, s)
                   : (branch->ifFalse ? scan(branch->ifFalse, s) : Value());
      s.clear();
      State yes(*owner);
      yes.clear();
      scan(branch->ifTrue, yes);
      State no(*owner);
      no.clear();
      if (branch->ifFalse)
        scan(branch->ifFalse, no);
      s.clear();
      if (yes.terminal || no.terminal || yes.exit || no.exit)
        s.terminal = true;
      return {};
    }
    if (auto *br = e->dynCast<Break>()) {
      if (br->value)
        scan(br->value, s);
      auto take = br->condition ? scan(br->condition, s).number
                                : std::optional<int32_t>(1);
      if (!take || *take) {
        s.clear();
        if (take)
          s.exit = br->name;
        else
          s.terminal = true;
      }
      return {};
    }
    if (auto *drop = e->dynCast<Drop>()) {
      scan(drop->value, s);
      return {};
    }
    if (e->is<Nop>())
      return {};
    if (auto *ret = e->dynCast<Return>()) {
      if (ret->value)
        scan(ret->value, s);
      s.clear();
      s.terminal = true;
      return {};
    }
    if (e->is<Unreachable>() || e->is<BrOn>() || e->is<Switch>() ||
        Properties::isControlFlowStructure(e)) {
      s.clear();
      s.terminal = true;
      return {};
    }
    if (auto n = scalar(e, s))
      return Value(*n);
    for (auto *&child : ChildIterator(e)) {
      scan(child, s);
      if (s.terminal || s.exit)
        break;
    }
    // Calls and memory effects cannot modify the caller's scalar locals.
    // Their child expressions have already been visited in execution order.
    if (e->is<Call>() || e->is<CallIndirect>() || e->is<CallRef>() ||
        e->is<Store>() || e->is<Load>() || e->is<GlobalGet>() ||
        e->is<GlobalSet>() || e->is<MemorySize>() || e->is<MemoryGrow>())
      s.invalidateArrays();
    else
      s.clear();
    return {};
  }
  size_t run() {
    State state(*owner);
    scan(owner->body, state);
    return changes;
  }
};
size_t normalizeArrayCopies(Module &m) {
  inlineArrayCopyHelpers(m);
  // Inlining exposes constant builder initialization beside the copy loop.
  NormalizeArrayBlocks blocks(m);
  NormalizeArrayFills fills(m);
  for (auto &f : m.functions)
    if (!f->imported()) {
      blocks.walk(f->body);
      fills.walk(f->body);
    }
  size_t count = 0;
  for (auto &f : m.functions)
    if (!f->imported())
      count += ConstantArrayCopies(m, f.get()).run();
  if (count) {
    auto cleanup = [&]() {
      PassRunner runner(&m);
      runner.options.optimizeLevel = 2;
      runner.add("simplify-locals");
      runner.add("vacuum");
      runner.add("remove-unused-module-elements");
      runner.run();
    };
    cleanup();
    // Removing a snapshot copy can expose the builder's initialization as
    // contiguous stores. Normalize again so ordinary DCE can discard the
    // now-unused fixed allocation and its constant operands.
    for (auto &f : m.functions)
      if (!f->imported()) {
        blocks.remaining = 1024 * 1024;
        blocks.walk(f->body);
        fills.walk(f->body);
      }
    cleanup();
  }
  return count;
}
