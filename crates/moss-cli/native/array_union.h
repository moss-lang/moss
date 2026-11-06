// Contract private split reference unions after their backing array types have
// merged. No source names or literal annotations are trusted. Function inputs
// are contracted only when every use observes the same selected reference;
// local tuples additionally require checked one-hot producers and SSA flows.
std::optional<int32_t> unionInteger(Expression *e) {
  if (auto *c = e->dynCast<Const>(); c && c->value.type == Type::i32)
    return c->value.geti32();
  return {};
}
struct UnionPredicate {
  LocalGet *tag = nullptr;
  int32_t key = 0;
  bool equal = false;
  bool test(int32_t value) const { return (value == key) == equal; }
};
std::optional<UnionPredicate> unionPredicate(Expression *e) {
  if (auto *get = e->dynCast<LocalGet>(); get && get->type == Type::i32)
    return UnionPredicate{get, 0, false};
  if (auto *unary = e->dynCast<Unary>(); unary && unary->op == EqZInt32) {
    if (auto *get = unary->value->dynCast<LocalGet>())
      return UnionPredicate{get, 0, true};
  }
  auto *binary = e->dynCast<Binary>();
  if (!binary || (binary->op != EqInt32 && binary->op != NeInt32))
    return {};
  auto *get = binary->left->dynCast<LocalGet>();
  auto key = unionInteger(binary->right);
  if (!get || !key) {
    get = binary->right->dynCast<LocalGet>();
    key = unionInteger(binary->left);
  }
  if (!get || !key)
    return {};
  return UnionPredicate{get, *key, binary->op == EqInt32};
}
struct UnionSelect {
  Select *node;
  UnionPredicate predicate;
  LocalGet *left;
  LocalGet *right;
};
std::optional<UnionSelect> unionSelect(Expression *e) {
  auto *node = e->dynCast<Select>();
  if (!node)
    return {};
  auto condition = unionPredicate(node->condition);
  auto *left = node->ifTrue->dynCast<LocalGet>();
  auto *right = node->ifFalse->dynCast<LocalGet>();
  if (!condition || !left || !right || !left->type.isRef() ||
      left->type != right->type || !left->type.getHeapType().isArray())
    return {};
  return UnionSelect{node, *condition, left, right};
}
struct UnionNodes
    : PostWalker<UnionNodes, UnifiedExpressionVisitor<UnionNodes>> {
  std::vector<Expression *> nodes;
  std::unordered_map<Expression *, Expression *> parent;
  std::unordered_map<Index, std::vector<LocalSet *>> sets;
  std::unordered_map<Index, std::vector<LocalGet *>> gets;
  void visitExpression(Expression *e) {
    nodes.push_back(e);
    for (auto *child : ChildIterator(e))
      parent[child] = e;
    if (auto *set = e->dynCast<LocalSet>())
      sets[set->index].push_back(set);
    if (auto *get = e->dynCast<LocalGet>())
      gets[get->index].push_back(get);
  }
};
struct ReplaceUnionSelects : PostWalker<ReplaceUnionSelects> {
  const std::unordered_set<Expression *> &selected;
  Builder b;
  Index index;
  Type type;
  ReplaceUnionSelects(Module &m, const std::unordered_set<Expression *> &s,
                      Index i, Type t)
      : selected(s), b(m), index(i), type(t) {}
  void visitSelect(Select *e) {
    if (selected.contains(e))
      replaceCurrent(b.makeLocalGet(index, type));
  }
};
struct UnionParamRemap : PostWalker<UnionParamRemap> {
  Index first;
  UnionParamRemap(Index i) : first(i) {}
  Index remap(Index i) {
    if (i <= first)
      return i;
    if (i <= first + 2)
      return first;
    return i - 2;
  }
  void visitLocalGet(LocalGet *e) { e->index = remap(e->index); }
  void visitLocalSet(LocalSet *e) { e->index = remap(e->index); }
};
struct UnionCallInputs : PostWalker<UnionCallInputs> {
  Module &module;
  Function *owner;
  Name target;
  Index first;
  UnionPredicate predicate;
  bool leftAtTrue;
  Type refType;
  Builder b;
  UnionCallInputs(Module &m, Function *f, Name name, Index i, UnionPredicate p,
                  bool left, Type type)
      : module(m), owner(f), target(name), first(i), predicate(p),
        leftAtTrue(left), refType(type), b(m) {}
  void visitCall(Call *call) {
    if (call->target != target)
      return;
    // Evaluate all original operands once, in order, before selecting. The
    // discarded tag and inactive reference can themselves have effects.
    std::vector<Expression *> sequence, args;
    std::vector<Index> saved;
    for (auto *arg : call->operands) {
      auto index = owner->getNumLocals();
      owner->vars.push_back(arg->type);
      saved.push_back(index);
      sequence.push_back(b.makeLocalSet(index, arg));
    }
    auto get = [&](Index i) {
      return b.makeLocalGet(saved[i], owner->getLocalType(saved[i]));
    };
    for (Index i = 0; i < saved.size(); ++i) {
      if (i == first) {
        auto *condition = b.makeBinary(predicate.equal ? EqInt32 : NeInt32,
                                       get(first), b.makeConst(predicate.key));
        args.push_back(b.makeSelect(condition,
                                    get(first + (leftAtTrue ? 1 : 2)),
                                    get(first + (leftAtTrue ? 2 : 1))));
      } else if (i < first || i > first + 2) {
        args.push_back(get(i));
      }
    }
    call->operands.set(args);
    sequence.push_back(call);
    replaceCurrent(b.makeBlock(sequence, call->type));
  }
};
size_t contractArrayUnionParameters(Module &m) {
  std::unordered_set<Name> exposed;
  for (auto &ex : m.exports)
    if (ex->kind == ExternalKind::Function)
      exposed.insert(*ex->getInternalName());
  struct FunctionRefs : PostWalker<FunctionRefs> {
    std::unordered_set<Name> &names;
    FunctionRefs(std::unordered_set<Name> &n) : names(n) {}
    void visitRefFunc(RefFunc *e) { names.insert(e->func); }
  } refs(exposed);
  refs.walkModule(&m);
  size_t changed = 0;
  for (auto &f : m.functions) {
    if (f->imported() || exposed.contains(f->name))
      continue;
    for (Index first = 0; first + 2 < f->getNumParams(); ++first) {
      auto params = f->getParams();
      auto refType = params[first + 1];
      if (params[first] != Type::i32 || !refType.isRef() ||
          !refType.getHeapType().isArray() || params[first + 2] != refType)
        continue;
      UnionNodes nodes;
      nodes.walk(f->body);
      if (!nodes.sets[first].empty() || !nodes.sets[first + 1].empty() ||
          !nodes.sets[first + 2].empty())
        continue;
      std::unordered_set<Expression *> selected, allowed;
      std::optional<UnionSelect> model;
      bool safe = true;
      for (auto *e : nodes.nodes) {
        auto match = unionSelect(e);
        if (!match || match->predicate.tag->index != first)
          continue;
        bool left =
            match->left->index == first + 1 && match->right->index == first + 2;
        bool right =
            match->left->index == first + 2 && match->right->index == first + 1;
        if (!left && !right)
          continue;
        if (model) {
          bool modelLeft = model->left->index == first + 1;
          safe &= model->predicate.key == match->predicate.key &&
                  (model->predicate.equal == modelLeft) ==
                      (match->predicate.equal == left);
        } else {
          model = match;
        }
        selected.insert(e);
        allowed.insert(match->predicate.tag);
        allowed.insert(match->left);
        allowed.insert(match->right);
      }
      for (Index i = first; i <= first + 2; ++i)
        for (auto *get : nodes.gets[i])
          safe &= allowed.contains(get);
      if (!safe || selected.empty())
        continue;
      // Bounds/unsupported operands are checked for every caller before any
      // mutation. A null function reference or an exported ABI already
      // declined.
      for (auto &caller : m.functions) {
        if (caller->imported())
          continue;
        UnionNodes uses;
        uses.walk(caller->body);
        size_t added = 0;
        for (auto *e : uses.nodes) {
          if (auto *call = e->dynCast<Call>();
              call && call->target == f->name) {
            added += call->operands.size();
            for (auto *arg : call->operands)
              safe &= arg->type.isSingle() && arg->type != Type::unreachable;
          }
        }
        safe &= caller->getNumLocals() + added <= 50000;
      }
      if (!safe)
        continue;
      bool left = model->left->index == first + 1;
      auto predicate = model->predicate;
      ReplaceUnionSelects(m, selected, first + 1, refType).walk(f->body);
      UnionParamRemap(first).walk(f->body);
      std::vector<Type> newParams;
      for (Index i = 0; i < params.size(); ++i) {
        if (i == first)
          newParams.push_back(refType);
        else if (i < first || i > first + 2)
          newParams.push_back(params[i]);
      }
      f->setParams(Type(newParams));
      f->localNames.clear();
      f->localIndices.clear();
      for (auto &caller : m.functions)
        if (!caller->imported())
          UnionCallInputs(m, caller.get(), f->name, first, predicate, left,
                          refType)
              .walk(caller->body);
      ++changed;
      // Revisit the same position: another adjacent union may follow it.
      if (first)
        --first;
    }
  }
  return changed;
}

std::optional<HeapType> arrayUnionTuple(Type type) {
  if (!type.isTuple() || type.size() != 3 || type[0] != Type::i32)
    return {};
  std::optional<HeapType> array;
  for (Index i : {1u, 2u}) {
    if (!type[i].isRef())
      return {};
    auto heap = type[i].getHeapType();
    if (heap.isArray()) {
      if (array && *array != heap)
        return {};
      array = heap;
    } else if (heap != HeapType::none) {
      return {};
    }
  }
  return array;
}
struct ArrayUnionReturn {
  Function *function;
  HeapType array;
  std::unordered_map<Expression *, unsigned> active;
  std::unordered_set<Expression *> producers;
  std::map<int32_t, unsigned> cases;
  bool nonNull = true;
  bool producer(Expression *e) {
    if (e->type == Type::unreachable)
      return true;
    auto shape = arrayUnionTuple(e->type);
    if (!shape || *shape != array)
      return false;
    if (auto *tuple = e->dynCast<TupleMake>()) {
      auto tag = unionInteger(tuple->operands[0]);
      if (!tag)
        return false;
      unsigned lane = tuple->operands[2]->is<RefNull>()   ? 1
                      : tuple->operands[1]->is<RefNull>() ? 2
                                                          : 0;
      if (!lane)
        return false;
      auto [it, added] = cases.emplace(*tag, lane);
      if (!added && it->second != lane)
        return false;
      active[e] = lane;
      nonNull &= tuple->operands[lane]->type.isNonNullable();
    } else if (auto *node = e->dynCast<If>()) {
      if (!node->ifFalse || !producer(node->ifTrue) || !producer(node->ifFalse))
        return false;
    } else if (auto *node = e->dynCast<Block>()) {
      if (node->name || node->list.empty() || !producer(node->list.back()))
        return false;
    } else
      return false;
    producers.insert(e);
    return true;
  }
};
using ArrayUnionReturns = std::unordered_map<Name, ArrayUnionReturn>;
struct LocalArrayUnion {
  Module &module;
  Function &function;
  UnionNodes &nodes;
  LocalGraph &graph;
  LocalSet *root;
  const ArrayUnionReturns *returns;
  std::unordered_set<Call *> calls;
  HeapType array;
  Type reference;
  bool safe = true;
  std::unordered_set<Index> bundles, scalars;
  std::unordered_set<Expression *> bundleValues, producers, selected;
  std::unordered_set<Expression *> conditions, selectorParts;
  std::unordered_map<Expression *, unsigned> projections, components;
  std::unordered_map<Expression *, unsigned> active;
  std::unordered_map<int32_t, unsigned> cases;
  bool nonNull = true;
  std::unordered_set<Expression *> busy;
  LocalArrayUnion(Module &m, Function &f, UnionNodes &n, LocalGraph &g,
                  LocalSet *r, HeapType a,
                  const ArrayUnionReturns *models = nullptr)
      : module(m), function(f), nodes(n), graph(g), root(r), returns(models),
        array(a), reference(Type(a, Nullable)) {}
  bool unique(LocalGet *get) {
    auto &defs = nodes.sets[get->index];
    auto &reaching = graph.getSets(get);
    return defs.size() == 1 && reaching.size() == 1 &&
           *reaching.begin() == defs[0];
  }
  bool bundle(Expression *e) {
    if (auto *get = e->dynCast<LocalGet>()) {
      if (!bundles.contains(get->index) || !unique(get))
        return false;
    } else if (auto *set = e->dynCast<LocalSet>()) {
      if (!set->isTee() || !bundles.contains(set->index))
        return false;
    } else if (auto *block = e->dynCast<Block>()) {
      if (block->name || block->list.empty() || !bundle(block->list.back()))
        return false;
    } else {
      return false;
    }
    bundleValues.insert(e);
    return true;
  }
  bool producer(Expression *e) {
    if (e->type == Type::unreachable)
      return true;
    auto shape = arrayUnionTuple(e->type);
    if (!shape || *shape != array)
      return false;
    if (auto *tuple = e->dynCast<TupleMake>()) {
      if (tuple->operands.size() != 3)
        return false;
      auto tag = unionInteger(tuple->operands[0]);
      if (!tag)
        return false;
      unsigned lane = 0;
      if (tuple->operands[2]->is<RefNull>())
        lane = 1;
      else if (tuple->operands[1]->is<RefNull>())
        lane = 2;
      if (!lane)
        return false;
      auto [it, inserted] = cases.emplace(*tag, lane);
      if (!inserted && it->second != lane)
        return false;
      active[e] = lane;
      nonNull &= tuple->operands[lane]->type.isNonNullable();
    } else if (auto *call = e->dynCast<Call>();
               call && returns && returns->contains(call->target)) {
      auto &model = returns->at(call->target);
      if (model.array != array)
        return false;
      for (auto [tag, lane] : model.cases) {
        auto [it, added] = cases.emplace(tag, lane);
        if (!added && it->second != lane)
          return false;
      }
      nonNull &= model.nonNull;
      calls.insert(call);
    } else if (auto *iff = e->dynCast<If>()) {
      if (!iff->ifFalse || !producer(iff->ifTrue) || !producer(iff->ifFalse))
        return false;
    } else if (auto *block = e->dynCast<Block>()) {
      if (block->name || block->list.empty() || !producer(block->list.back()))
        return false;
    } else {
      return false;
    }
    producers.insert(e);
    return true;
  }
  std::optional<unsigned> component(Expression *e) {
    if (auto it = components.find(e); it != components.end())
      return it->second;
    if (!busy.insert(e).second)
      return {};
    std::optional<unsigned> result;
    if (auto *extract = e->dynCast<TupleExtract>()) {
      if (extract->index < 3 && bundle(extract->tuple)) {
        result = extract->index;
        projections[e] = extract->index;
      }
    } else if (auto *get = e->dynCast<LocalGet>()) {
      if (unique(get) && !bundles.contains(get->index)) {
        result = component(nodes.sets[get->index][0]->value);
        if (result)
          scalars.insert(get->index);
      }
    } else if (auto *set = e->dynCast<LocalSet>()) {
      if (set->isTee() && nodes.sets[set->index].size() == 1 &&
          !bundles.contains(set->index)) {
        result = component(set->value);
        if (result)
          scalars.insert(set->index);
      }
    } else if (auto *block = e->dynCast<Block>()) {
      if (!block->name && !block->list.empty())
        result = component(block->list.back());
    }
    busy.erase(e);
    if (result)
      components[e] = *result;
    return result;
  }
  bool selectorShape(Expression *e, std::unordered_set<Expression *> &parts) {
    if (auto *get = e->dynCast<LocalGet>()) {
      auto lane = component(get);
      if (!lane || (*lane != 1 && *lane != 2))
        return false;
      parts.insert(e);
      return true;
    }
    if (e->is<Unreachable>()) {
      parts.insert(e);
      return true;
    }
    Expression *condition = nullptr, *left = nullptr, *right = nullptr;
    if (auto *node = e->dynCast<Select>()) {
      condition = node->condition;
      left = node->ifTrue;
      right = node->ifFalse;
    } else if (auto *node = e->dynCast<If>(); node && node->ifFalse) {
      condition = node->condition;
      left = node->ifTrue;
      right = node->ifFalse;
    } else
      return false;
    auto predicate = unionPredicate(condition);
    if (!predicate)
      return false;
    auto tag = component(predicate->tag);
    if (!tag || *tag != 0 || !selectorShape(left, parts) ||
        !selectorShape(right, parts))
      return false;
    parts.insert(e);
    parts.insert(condition);
    return true;
  }
  std::optional<unsigned> selectorLane(Expression *e, int32_t value) {
    if (auto *get = e->dynCast<LocalGet>())
      return component(get);
    Expression *condition = nullptr, *left = nullptr, *right = nullptr;
    if (auto *node = e->dynCast<Select>()) {
      condition = node->condition;
      left = node->ifTrue;
      right = node->ifFalse;
    } else if (auto *node = e->dynCast<If>(); node && node->ifFalse) {
      condition = node->condition;
      left = node->ifTrue;
      right = node->ifFalse;
    } else
      return {};
    auto predicate = unionPredicate(condition);
    return selectorLane(predicate->test(value) ? left : right, value);
  }
  bool check() {
    if (nodes.sets[root->index].size() != 1 ||
        root->index < function.getNumParams() || !producer(root->value) ||
        cases.empty())
      return false;
    reference = Type(array, nonNull ? NonNullable : Nullable);
    bundles.insert(root->index);
    bool added;
    do {
      added = false;
      for (auto &[index, defs] : nodes.sets) {
        if (bundles.contains(index) || defs.size() != 1 ||
            index < function.getNumParams())
          continue;
        auto shape = arrayUnionTuple(function.getLocalType(index));
        if (shape && *shape == array && bundle(defs[0]->value))
          added |= bundles.insert(index).second;
      }
    } while (added);
    for (auto *e : nodes.nodes)
      component(e);
    for (auto &[index, defs] : nodes.sets) {
      if (bundles.contains(index))
        continue;
      for (auto *set : defs) {
        if (component(set->value)) {
          if (defs.size() != 1 || index < function.getNumParams())
            return false;
          scalars.insert(index);
        }
      }
    }
    for (auto index : bundles)
      for (auto *get : nodes.gets[index])
        if (!bundle(get))
          return false;
    for (auto index : scalars)
      for (auto *get : nodes.gets[index])
        if (!component(get))
          return false;
    for (auto *e : nodes.nodes) {
      if (!e->type.isRef() || e->type.getHeapType() != array)
        continue;
      std::unordered_set<Expression *> parts;
      if (!selectorShape(e, parts))
        continue;
      bool agrees = true;
      for (auto [value, lane] : cases) {
        auto chosen = selectorLane(e, value);
        agrees &= chosen && *chosen == lane;
      }
      if (agrees) {
        selected.insert(e);
        selectorParts.insert(parts.begin(), parts.end());
      }
    }
    if (selected.empty())
      return false;
    auto parent = [&](Expression *e) -> Expression * {
      auto it = nodes.parent.find(e);
      return it == nodes.parent.end() ? nullptr : it->second;
    };
    for (auto *e : bundleValues) {
      auto *p = parent(e);
      bool allowed = p && p->is<Drop>();
      if (auto *set = p ? p->dynCast<LocalSet>() : nullptr)
        allowed |= bundles.contains(set->index) && set->value == e;
      if (auto *block = p ? p->dynCast<Block>() : nullptr)
        allowed |= bundleValues.contains(p) && block->list.back() == e;
      if (auto *extract = p ? p->dynCast<TupleExtract>() : nullptr)
        allowed |= projections.contains(extract) && extract->tuple == e;
      if (!allowed)
        return false;
    }
    for (auto [e, lane] : components) {
      if (selected.contains(e) || cases.size() == 1)
        continue;
      auto *p = parent(e);
      bool allowed = p && (p->is<Drop>() || selectorParts.contains(p) ||
                           conditions.contains(p));
      if (auto *set = p ? p->dynCast<LocalSet>() : nullptr)
        allowed |= scalars.contains(set->index) && set->value == e;
      if (auto *block = p ? p->dynCast<Block>() : nullptr)
        allowed |= components.contains(p) && block->list.back() == e;
      if (!allowed)
        return false;
    }
    return true;
  }
};
struct RewriteLocalArrayUnion
    : PostWalker<RewriteLocalArrayUnion,
                 UnifiedExpressionVisitor<RewriteLocalArrayUnion>> {
  LocalArrayUnion &proof;
  Builder b;
  RewriteLocalArrayUnion(Module &m, LocalArrayUnion &p) : proof(p), b(m) {}
  void visitExpression(Expression *e) {
    if (proof.selected.contains(e)) {
      replaceCurrent(b.makeLocalGet(proof.root->index, proof.reference));
      return;
    }
    if (auto it = proof.active.find(e); it != proof.active.end()) {
      replaceCurrent(e->cast<TupleMake>()->operands[it->second]);
      return;
    }
    if (auto it = proof.projections.find(e); it != proof.projections.end()) {
      auto *value = e->cast<TupleExtract>()->tuple;
      if (it->second == 0) {
        int32_t tag = proof.cases.size() == 1 ? proof.cases.begin()->first : 0;
        replaceCurrent(
            b.makeBlock({b.makeDrop(value), b.makeConst(tag)}, Type::i32));
      } else if (proof.cases.size() == 1 &&
                 it->second != proof.cases.begin()->second) {
        replaceCurrent(b.makeBlock(
            {b.makeDrop(value), b.makeRefNull(e->type.getHeapType())},
            e->type));
      } else {
        replaceCurrent(value);
      }
      return;
    }
    if (proof.producers.contains(e) || proof.bundleValues.contains(e))
      e->type = proof.reference;
    else if (auto it = proof.components.find(e);
             it != proof.components.end() && it->second != 0 &&
             !(proof.cases.size() == 1 &&
               it->second != proof.cases.begin()->second))
      e->type = proof.reference;
    if (auto *set = e->dynCast<LocalSet>();
        set && (proof.bundles.contains(set->index) ||
                proof.scalars.contains(set->index)))
      set->finalize();
  }
};
size_t contractLocalArrayUnions(Module &m) {
  size_t changed = 0;
  for (auto &f : m.functions) {
    if (f->imported())
      continue;
    // Rebuild reaching definitions after each successful rewrite.
    while (true) {
      UnionNodes nodes;
      nodes.walk(f->body);
      LocalGraph graph(f.get(), &m);
      bool found = false;
      for (auto *e : nodes.nodes) {
        auto *set = e->dynCast<LocalSet>();
        if (!set || set->index < f->getNumParams())
          continue;
        auto array = arrayUnionTuple(f->getLocalType(set->index));
        if (!array)
          continue;
        LocalArrayUnion proof(m, *f, nodes, graph, set, *array);
        if (!proof.check())
          continue;
        for (auto index : proof.bundles)
          f->vars[index - f->getNumParams()] = proof.reference;
        for (auto index : proof.scalars) {
          auto lane = proof.component(nodes.sets[index][0]->value);
          if (lane && *lane != 0 &&
              !(proof.cases.size() == 1 &&
                *lane != proof.cases.begin()->second))
            f->vars[index - f->getNumParams()] = proof.reference;
        }
        RewriteLocalArrayUnion(m, proof).walk(f->body);
        f->effects.reset();
        ReFinalize().walkFunctionInModule(f.get(), &m);
        ++changed;
        found = true;
        break;
      }
      if (!found)
        break;
    }
  }
  return changed;
}
struct RewriteArrayUnionReturn
    : PostWalker<RewriteArrayUnionReturn,
                 UnifiedExpressionVisitor<RewriteArrayUnionReturn>> {
  ArrayUnionReturn &proof;
  Type reference;
  RewriteArrayUnionReturn(ArrayUnionReturn &p)
      : proof(p), reference(Type(p.array, p.nonNull ? NonNullable : Nullable)) {
  }
  void visitExpression(Expression *e) {
    if (auto it = proof.active.find(e); it != proof.active.end()) {
      replaceCurrent(e->cast<TupleMake>()->operands[it->second]);
      return;
    }
    if (proof.producers.contains(e))
      e->type = reference;
  }
};
// A single-variant result can be narrowed without restricting its consumers:
// rebuild the exact old tuple at each call. Ordinary cleanup can then fold its
// constant tag/null components, even through effectful argument/result blocks.
struct ReconstructArrayUnionCalls : PostWalker<ReconstructArrayUnionCalls> {
  Function &owner;
  Name target;
  Type oldResults, reference;
  int32_t tag;
  unsigned active;
  Builder b;
  ReconstructArrayUnionCalls(Module &m, Function &f, ArrayUnionReturn &model,
                             Type old)
      : owner(f), target(model.function->name), oldResults(old),
        reference(Type(model.array, model.nonNull ? NonNullable : Nullable)),
        tag(model.cases.begin()->first), active(model.cases.begin()->second),
        b(m) {}
  void visitCall(Call *call) {
    if (call->target != target)
      return;
    auto local = owner.getNumLocals();
    owner.vars.push_back(reference);
    call->type = reference;
    std::vector<Expression *> values{b.makeConst(tag)};
    for (unsigned lane : {1u, 2u}) {
      values.push_back(lane == active ? static_cast<Expression *>(
                                            b.makeLocalGet(local, reference))
                                      : static_cast<Expression *>(b.makeRefNull(
                                            oldResults[lane].getHeapType())));
    }
    replaceCurrent(b.makeBlock(
        {b.makeLocalSet(local, call), b.makeTupleMake(values)}, oldResults));
  }
};
size_t contractArrayUnionReturns(Module &m) {
  std::unordered_set<Name> exposed;
  for (auto &ex : m.exports)
    if (ex->kind == ExternalKind::Function)
      exposed.insert(*ex->getInternalName());
  struct Refs : PostWalker<Refs> {
    std::unordered_set<Name> &names;
    Refs(std::unordered_set<Name> &n) : names(n) {}
    void visitRefFunc(RefFunc *e) { names.insert(e->func); }
  } refs(exposed);
  refs.walkModule(&m);
  ArrayUnionReturns models;
  for (auto &f : m.functions) {
    if (f->imported() || exposed.contains(f->name))
      continue;
    auto array = arrayUnionTuple(f->getResults());
    if (!array)
      continue;
    ArrayUnionReturn proof{f.get(), *array, {}, {}, {}};
    bool safe = proof.producer(f->body);
    UnionNodes nodes;
    nodes.walk(f->body);
    for (auto *e : nodes.nodes) {
      if (auto *ret = e->dynCast<Return>())
        safe &= ret->value && proof.producer(ret->value);
      if (auto *call = e->dynCast<Call>())
        safe &= !call->isReturn;
      if (auto *call = e->dynCast<CallIndirect>())
        safe &= !call->isReturn;
      if (auto *call = e->dynCast<CallRef>())
        safe &= !call->isReturn;
    }
    if (safe && !proof.cases.empty())
      models.emplace(f->name, std::move(proof));
  }
  if (models.empty())
    return 0;
  size_t reconstructed = 0;
  std::vector<Name> single;
  for (auto &[name, model] : models)
    if (model.cases.size() == 1)
      single.push_back(name);
  for (auto name : single) {
    auto &model = models.at(name);
    bool safe = true;
    for (auto &caller : m.functions) {
      if (caller->imported())
        continue;
      UnionNodes uses;
      uses.walk(caller->body);
      size_t calls = 0;
      for (auto *e : uses.nodes)
        if (auto *call = e->dynCast<Call>(); call && call->target == name) {
          safe &= !call->isReturn && call->type != Type::unreachable;
          ++calls;
        }
      safe &= caller->getNumLocals() + calls <= 50000;
    }
    if (!safe) {
      models.erase(name);
      continue;
    }
    auto oldResults = model.function->getResults();
    RewriteArrayUnionReturn(model).walk(model.function->body);
    model.function->setResults(
        Type(model.array, model.nonNull ? NonNullable : Nullable));
    for (auto &caller : m.functions)
      if (!caller->imported())
        ReconstructArrayUnionCalls(m, *caller, model, oldResults)
            .walk(caller->body);
    models.erase(name);
    ++reconstructed;
  }
  if (models.empty())
    return reconstructed;
  struct Plan {
    std::unique_ptr<UnionNodes> nodes;
    std::unique_ptr<LocalGraph> graph;
    std::unique_ptr<LocalArrayUnion> proof;
  };
  std::vector<Plan> plans;
  std::unordered_set<Call *> covered;
  while (!models.empty()) {
    plans.clear();
    covered.clear();
    for (auto &f : m.functions) {
      if (f->imported())
        continue;
      auto nodes = std::make_unique<UnionNodes>();
      nodes->walk(f->body);
      auto graph = std::make_unique<LocalGraph>(f.get(), &m);
      // Each accepted proof must keep its own stable parent/definition graph.
      for (auto *e : nodes->nodes) {
        auto *set = e->dynCast<LocalSet>();
        if (!set || set->index < f->getNumParams())
          continue;
        auto array = arrayUnionTuple(f->getLocalType(set->index));
        if (!array)
          continue;
        auto pnodes = std::make_unique<UnionNodes>();
        pnodes->walk(f->body);
        auto pgraph = std::make_unique<LocalGraph>(f.get(), &m);
        auto proof = std::make_unique<LocalArrayUnion>(m, *f, *pnodes, *pgraph,
                                                       set, *array, &models);
        if (!proof->check() || proof->calls.empty())
          continue;
        // Overlapping roots (one producer inside another) are conservatively
        // left for a later round after cleanup.
        bool overlap = false;
        for (auto &plan : plans)
          if (&plan.proof->function == f.get()) {
            for (auto index : proof->bundles)
              overlap |= plan.proof->bundles.contains(index);
            for (auto index : proof->scalars)
              overlap |= plan.proof->scalars.contains(index);
          }
        if (overlap)
          continue;
        covered.insert(proof->calls.begin(), proof->calls.end());
        plans.push_back(
            {std::move(pnodes), std::move(pgraph), std::move(proof)});
      }
    }
    std::unordered_set<Name> remove;
    std::unordered_set<Name> used;
    for (auto &f : m.functions)
      if (!f->imported()) {
        UnionNodes nodes;
        nodes.walk(f->body);
        for (auto *e : nodes.nodes)
          if (auto *call = e->dynCast<Call>();
              call && models.contains(call->target)) {
            used.insert(call->target);
            if (!covered.contains(call))
              remove.insert(call->target);
          }
      }
    for (auto &[name, _] : models)
      if (!used.contains(name))
        remove.insert(name);
    if (remove.empty())
      break;
    for (auto name : remove)
      models.erase(name);
  }
  if (models.empty())
    return reconstructed;
  for (auto &[_, proof] : models) {
    RewriteArrayUnionReturn(proof).walk(proof.function->body);
    proof.function->setResults(
        Type(proof.array, proof.nonNull ? NonNullable : Nullable));
  }
  for (auto &plan : plans) {
    auto &proof = *plan.proof;
    for (auto index : proof.bundles)
      proof.function.vars[index - proof.function.getNumParams()] =
          proof.reference;
    for (auto index : proof.scalars) {
      auto lane = proof.component(plan.nodes->sets[index][0]->value);
      if (lane && *lane != 0 &&
          !(proof.cases.size() == 1 && *lane != proof.cases.begin()->second))
        proof.function.vars[index - proof.function.getNumParams()] =
            proof.reference;
    }
    RewriteLocalArrayUnion(m, proof).walk(proof.function.body);
  }
  return reconstructed + models.size();
}

size_t coalesceArrayUnions(Module &m) {
  auto params = contractArrayUnionParameters(m);
  for (auto &f : m.functions)
    if (!f->imported())
      ReFinalize().walkFunctionInModule(f.get(), &m);
  auto locals = contractLocalArrayUnions(m);
  auto returns = contractArrayUnionReturns(m);
  if (params || locals || returns) {
    for (auto &f : m.functions)
      f->effects.reset();
    m.indirectCallEffects.clear();
  }
  for (auto &f : m.functions)
    if (!f->imported())
      ReFinalize().walkFunctionInModule(f.get(), &m);
  return params + locals + returns;
}
