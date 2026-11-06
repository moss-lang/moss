// Constant initialized arrays -> pooled static backing.
// Every reference use is checked before representation changes; identity,
// reflection, post-initialization mutation and host ABI escapes decline.
#include "ir/iteration.h"
#include "ir/local-graph.h"
#include "ir/module-utils.h"
#include "ir/utils.h"
#include "pass.h"
#include "wasm-builder.h"
#include "wasm-traversal.h"
#include "wasm.h"
#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>

namespace {
using namespace wasm;
constexpr uint32_t Empty = 0x7fffffff;
constexpr size_t MaxPoolBytes = 16 * 1024 * 1024;

bool has(Type type, HeapType candidate) {
  for (auto part : type) {
    if (part.isRef() && part.getHeapType() == candidate)
      return true;
  }
  return false;
}

bool related(Type type, HeapType candidate,
             std::unordered_set<HeapType> &seen) {
  for (auto part : type) {
    if (!part.isRef())
      continue;
    auto heap = part.getHeapType();
    if (heap == candidate)
      return true;
    if (heap.isBasic() || !seen.insert(heap).second)
      continue;
    for (auto child : heap.getTypeChildren()) {
      if (related(child, candidate, seen))
        return true;
    }
  }
  return false;
}
bool related(Type type, HeapType candidate) {
  std::unordered_set<HeapType> seen;
  return related(type, candidate, seen);
}
bool flow(Type value, Type destination, HeapType candidate) {
  if (!has(value, candidate))
    return true;
  if (value.size() != destination.size())
    return false;
  for (Index i = 0; i < value.size(); ++i) {
    if (has(value[i], candidate) && !has(destination[i], candidate))
      return false;
  }
  return true;
}
std::optional<int32_t> integer(Expression *expr) {
  if (auto *c = expr->dynCast<Const>(); c && c->value.type == Type::i32)
    return c->value.geti32();
  return {};
}

bool byteArray(HeapType type) {
  if (!type.isArray() || type.isOpen() || type.isShared() ||
      type.getDeclaredSuperType())
    return false;
  auto field = type.getArray().element;
  return field.type == Type::i32 && (field.packedType == Field::i8 ||
                                     field.packedType == Field::NotPacked);
}
#include "array_init.h"

struct Uses : PostWalker<Uses, UnifiedExpressionVisitor<Uses>> {
  Module &module;
  HeapType candidate;
  Function *owner = nullptr;
  bool containers;
  const ArrayInitializations &initialization;
  bool safe = true;
  std::unordered_map<Name, Type> labels;
  Uses(Module &m, HeapType c, const ArrayInitializations &i, bool g = false)
      : module(m), candidate(c), containers(g), initialization(i) {}
  bool tracked(Type type) const {
    return containers ? related(type, candidate) : has(type, candidate);
  }
  bool flows(Type value, Type destination) const {
    if (!containers)
      return flow(value, destination, candidate);
    if (!tracked(value))
      return true;
    if (value.size() != destination.size())
      return false;
    for (Index i = 0; i < value.size(); ++i) {
      if (tracked(value[i]) &&
          (!destination[i].isRef() || !value[i].isRef() ||
           value[i].getHeapType() != destination[i].getHeapType()))
        return false;
    }
    return true;
  }
  std::optional<Type> element(Expression *ref) const {
    if (!ref->type.isRef() || !ref->type.getHeapType().isArray())
      return {};
    return ref->type.getHeapType().getArray().element.type;
  }
  std::optional<Type> field(Expression *ref, Index index) const {
    if (!ref->type.isRef() || !ref->type.getHeapType().isStruct())
      return {};
    return ref->type.getHeapType().getStruct().fields[index].type;
  }
  void visitExpression(Expression *e) {
    bool child = false;
    for (auto *c : ChildIterator(e))
      child |= tracked(c->type);
    if (!containers && related(e->type, candidate) && !has(e->type, candidate))
      safe = false;
    if (containers) {
      // Function references are not aggregate containers. Their related
      // signatures must not provide an indirect route out of the use graph.
      for (auto part : e->type)
        if (tracked(part) &&
            (!part.isRef() ||
             (!part.getHeapType().isArray() && !part.getHeapType().isStruct())))
          safe = false;
    }
    if (auto *cast = e->dynCast<RefCast>();
        cast && related(cast->type, candidate))
      safe = false;
    if (auto *test = e->dynCast<RefTest>();
        test && related(test->castType, candidate))
      safe = false;
    if (auto *branch = e->dynCast<BrOn>();
        branch && related(branch->castType, candidate))
      safe = false;
    if (auto *ref = e->dynCast<RefFunc>()) {
      auto *target = module.getFunction(ref->func);
      if (tracked(target->getParams()) || tracked(target->getResults()))
        safe = false;
    }
    if (!child && !tracked(e->type))
      return;
    if (!owner) {
      safe = false;
      return;
    }
    if (auto *allocation = e->dynCast<ArrayNew>();
        allocation && has(e->type, candidate)) {
      // Runtime constructions remain ordinary GC arrays. Only a separately
      // proven private initialization phase may write this array type.
      return;
    }
    if (auto *store = e->dynCast<ArraySet>();
        store && has(store->ref->type, candidate)) {
      safe &= !store->isAtomic() && initialization.writes.contains(store);
      return;
    }
    if (containers) {
      if (auto *node = e->dynCast<StructNew>()) {
        if (!e->type.isRef()) {
          safe = false;
          return;
        }
        safe &= !node->desc;
        auto &fields = node->type.getHeapType().getStruct().fields;
        for (Index i = 0; i < node->operands.size(); ++i)
          safe &= flows(node->operands[i]->type, fields[i].type);
        return;
      }
      if (auto *get = e->dynCast<StructGet>()) {
        auto type = field(get->ref, get->index);
        safe &= type && !get->isAtomic() && flows(*type, e->type);
        return;
      }
      if (auto *set = e->dynCast<StructSet>()) {
        auto type = field(set->ref, set->index);
        safe &= type && !set->isAtomic() && flows(set->value->type, *type);
        return;
      }
      if (auto *node = e->dynCast<ArrayNew>()) {
        if (!e->type.isRef()) {
          safe = false;
          return;
        }
        safe &= !has(e->type, candidate);
        auto type = e->type.getHeapType().getArray().element.type;
        if (node->init)
          safe &= flows(node->init->type, type);
        return;
      }
      if (auto *set = e->dynCast<ArraySet>()) {
        auto type = element(set->ref);
        safe &= type && !has(set->ref->type, candidate) && !set->isAtomic() &&
                flows(set->value->type, *type);
        return;
      }
      if (auto *fill = e->dynCast<ArrayFill>()) {
        auto type = element(fill->ref);
        safe &= type && !has(fill->ref->type, candidate) &&
                flows(fill->value->type, *type);
        return;
      }
      if (auto *copy = e->dynCast<ArrayCopy>()) {
        auto dest = element(copy->destRef), src = element(copy->srcRef);
        safe &= dest && src && !has(copy->destRef->type, candidate) &&
                flows(*src, *dest);
        return;
      }
      if (auto *eq = e->dynCast<RefEq>()) {
        // Container identity is unchanged: only its leaf byte arrays are
        // pooled.
        safe &=
            !has(eq->left->type, candidate) && !has(eq->right->type, candidate);
        return;
      }
    }
    if (auto *node = e->dynCast<ArrayNewFixed>()) {
      if (!e->type.isRef()) {
        safe = false;
        return;
      }
      safe &= tracked(e->type);
      auto type = e->type.getHeapType().getArray().element.type;
      for (auto *value : node->values)
        safe &= flows(value->type, type);
      return;
    }
    if (auto *get = e->dynCast<ArrayGet>()) {
      auto type = element(get->ref);
      safe &= type && tracked(get->ref->type) && !get->isAtomic() &&
              flows(*type, e->type);
      return;
    }
    if (auto *len = e->dynCast<ArrayLen>()) {
      safe &= tracked(len->ref->type);
      return;
    }
    if (auto *set = e->dynCast<LocalSet>()) {
      safe &= flows(set->value->type, owner->getLocalType(set->index));
      return;
    }
    if (auto *call = e->dynCast<Call>()) {
      auto *target = module.getFunction(call->target);
      auto params = target->getParams();
      for (Index i = 0; i < call->operands.size(); ++i)
        safe &= flows(call->operands[i]->type, params[i]);
      if (call->isReturn)
        safe &= flows(target->getResults(), owner->getResults());
      return;
    }
    if (auto *ret = e->dynCast<Return>()) {
      safe &= !ret->value || flows(ret->value->type, owner->getResults());
      return;
    }
    if (auto *br = e->dynCast<Break>()) {
      if (br->value && tracked(br->value->type)) {
        auto it = labels.find(br->name);
        safe &= it != labels.end() && flows(br->value->type, it->second);
      }
      return;
    }
    if (auto *block = e->dynCast<Block>()) {
      if (!block->list.empty())
        safe &= flows(block->list.back()->type, e->type);
      return;
    }
    if (auto *branch = e->dynCast<If>()) {
      safe &= flows(branch->ifTrue->type, e->type);
      if (branch->ifFalse)
        safe &= flows(branch->ifFalse->type, e->type);
      return;
    }
    if (auto *loop = e->dynCast<Loop>()) {
      safe &= flows(loop->body->type, e->type);
      return;
    }
    if (auto *select = e->dynCast<Select>()) {
      safe &= flows(select->ifTrue->type, e->type) &&
              flows(select->ifFalse->type, e->type);
      return;
    }
    if (auto *tuple = e->dynCast<TupleMake>()) {
      if (e->type == Type::unreachable) {
        safe = false;
        return;
      }
      for (Index i = 0; i < tuple->operands.size(); ++i)
        safe &= flows(tuple->operands[i]->type, e->type[i]);
      return;
    }
    if (e->is<Drop>() || e->is<LocalGet>() || e->is<RefNull>() ||
        e->is<TupleExtract>())
      return;
    if (e->is<RefIsNull>())
      return;
    if (auto *as = e->dynCast<RefAs>(); as && as->op == RefAsNonNull)
      return;
    safe = false;
  }
  struct Labels : PostWalker<Labels> {
    std::unordered_map<Name, Type> &labels;
    Labels(std::unordered_map<Name, Type> &l) : labels(l) {}
    void visitBlock(Block *b) {
      if (b->name)
        labels[b->name] = b->type;
    }
  };
  bool check() {
    for (auto &global : module.globals)
      if (related(global->type, candidate))
        return false;
    for (auto &table : module.tables)
      if (related(table->type, candidate))
        return false;
    for (auto &tag : module.tags)
      if (related(Type(tag->type, Nullable), candidate))
        return false;
    for (auto &ex : module.exports) {
      if (ex->kind == ExternalKind::Function) {
        auto *f = module.getFunction(*ex->getInternalName());
        if (related(f->getParams(), candidate) ||
            related(f->getResults(), candidate))
          return false;
      }
    }
    for (auto &f : module.functions) {
      if (f->imported()) {
        if (related(f->getParams(), candidate) ||
            related(f->getResults(), candidate))
          return false;
        continue;
      }
      owner = f.get();
      if (!flows(owner->body->type, owner->getResults()))
        return false;
      labels.clear();
      Labels(labels).walk(owner->body);
      walk(owner->body);
    }
    owner = nullptr;
    walkModuleCode(&module);
    return safe;
  }
};

struct Pool {
  std::vector<char> data;
  std::map<std::vector<char>, uint32_t> offsets;
  std::unordered_map<Expression *, uint32_t> sites;
};
Type widen(Type type, HeapType candidate) {
  if (type.isRef() && type.getHeapType() == candidate)
    return Type(HeapType::eq, type.getNullability());
  if (type.isTuple()) {
    auto tuple = type.getTuple();
    for (auto &part : tuple)
      part = widen(part, candidate);
    return Type(tuple);
  }
  return type;
}
Name memoryName(Module &m) {
  std::string name = "moss.constant.bytes";
  while (m.getMemoryOrNull(name) || m.getDataSegmentOrNull(name))
    name += ".";
  return Name(name);
}
// Bounded, local initialization normalization. No reads, aliases, calls, or
// control flow are crossed. Only constant values/indices move, and every
// removed store is proved in bounds. A trapping/unrecognized store stays put.
constexpr size_t MaxFixedElements = 10000;
std::optional<std::vector<int32_t>> initialValues(ArrayNew *node) {
  if (!node->type.isRef() || !byteArray(node->type.getHeapType()))
    return {};
  auto size = integer(node->size);
  auto init = node->init ? integer(node->init) : std::optional<int32_t>(0);
  if (!size || !init || *size < 0 || size_t(*size) > MaxFixedElements)
    return {};
  return std::vector<int32_t>(size_t(*size), *init);
}
ArrayNewFixed *fixed(Builder &b, HeapType type,
                     const std::vector<int32_t> &values) {
  std::vector<Expression *> operands;
  operands.reserve(values.size());
  for (auto value : values)
    operands.push_back(b.makeConst(value));
  return b.makeArrayNewFixed(type, operands);
}
struct NormalizeArrayBlocks : PostWalker<NormalizeArrayBlocks> {
  Builder b;
  size_t remaining = 1024 * 1024;
  NormalizeArrayBlocks(Module &m) : b(m) {}
  void visitBlock(Block *block) {
    auto &list = block->list;
    for (size_t i = 0; i < list.size(); ++i) {
      LocalSet *local = list[i]->dynCast<LocalSet>();
      bool firstStore = false;
      if (!local) {
        if (auto *store = list[i]->dynCast<ArraySet>();
            store && !store->isAtomic()) {
          local = store->ref->dynCast<LocalSet>();
          firstStore = local && local->isTee();
        }
      }
      if (!local || (!firstStore && local->isTee()))
        continue;
      auto *allocation = local->value->dynCast<ArrayNew>();
      if (!allocation)
        continue;
      auto size = integer(allocation->size);
      if (!size || *size < 0 || size_t(*size) > remaining)
        continue;
      remaining -= size_t(*size);
      auto values = initialValues(allocation);
      if (!values)
        continue;
      size_t j = i + (firstStore ? 0 : 1);
      for (; j < list.size() && remaining; ++j) {
        --remaining;
        if (list[j]->is<Nop>())
          continue;
        auto *store = list[j]->dynCast<ArraySet>();
        if (!store || store->isAtomic())
          break;
        bool same = false;
        if (firstStore && j == i)
          same = store->ref == local;
        else if (auto *get = store->ref->dynCast<LocalGet>())
          same = get->index == local->index;
        auto index = integer(store->index), value = integer(store->value);
        if (!same || !index || !value || *index < 0 ||
            size_t(*index) >= values->size())
          break;
        (*values)[*index] = *value;
      }
      if (firstStore && j == i)
        continue;
      // Do not expand large fill allocations on types that may later fail
      // pooling. Full store runs have more encoded work than the fixed
      // constructor, even if some stores overwrite the same element.
      size_t stores = 0;
      for (size_t k = i + (firstStore ? 0 : 1); k < j; ++k)
        stores += list[k]->is<ArraySet>();
      if (stores < values->size())
        continue;
      auto *replacement = fixed(b, allocation->type.getHeapType(), *values);
      if (firstStore)
        list[i] = b.makeLocalSet(local->index, replacement);
      else
        local->value = replacement;
      for (size_t k = i + 1; k < j; ++k)
        list[k] = b.makeNop();
      if (j > i)
        i = j - 1;
    }
  }
};
struct NormalizeArrayFills : PostWalker<NormalizeArrayFills> {
  Builder b;
  NormalizeArrayFills(Module &m) : b(m) {}
  void visitArrayNew(ArrayNew *node) {
    auto size = integer(node->size);
    if (!size || *size < 0 || *size > 1)
      return;
    if (auto values = initialValues(node); values && values->size() <= 1)
      replaceCurrent(fixed(b, node->type.getHeapType(), *values));
  }
};
#include "array_copy.h"

struct ArrayConstants : PostWalker<ArrayConstants> {
  HeapType candidate;
  Pool pool;
  bool overflow = false;
  std::unordered_set<Expression *> gets, lengths, writes;
  const ArrayInitializations &initialization;
  size_t constructions = 0;
  ArrayConstants(HeapType c, const ArrayInitializations &i)
      : candidate(c), initialization(i) {}
  void visitArrayGet(ArrayGet *e) {
    if (has(e->ref->type, candidate))
      gets.insert(e);
  }
  void visitArrayLen(ArrayLen *e) {
    if (has(e->ref->type, candidate))
      lengths.insert(e);
  }
  void visitArrayNew(ArrayNew *e) {
    if (has(e->type, candidate))
      ++constructions;
  }
  void visitArraySet(ArraySet *e) {
    if (has(e->ref->type, candidate) && initialization.writes.contains(e))
      writes.insert(e);
  }
  void visitArrayNewFixed(ArrayNewFixed *e) {
    if (!has(e->type, candidate))
      return;
    ++constructions;
    if (initialization.origins.contains(e))
      return;
    if (e->values.size() > MaxPoolBytes) {
      overflow = true;
      return;
    }
    std::vector<char> bytes;
    auto packed = candidate.getArray().element.packedType == Field::i8;
    for (auto *value : e->values) {
      auto n = integer(value);
      if (!n || (!packed && (*n < 0 || *n > 255)))
        return;
      bytes.push_back(char(uint32_t(*n) & 255));
    }
    if (bytes.empty()) {
      pool.sites[e] = Empty;
      return;
    }
    auto it = pool.offsets.find(bytes);
    if (it == pool.offsets.end()) {
      auto &data = pool.data;
      if (data.size() + bytes.size() + 7 > MaxPoolBytes) {
        overflow = true;
        return;
      }
      while (data.size() % 4)
        data.push_back(0);
      uint32_t offset = data.size(), length = bytes.size();
      for (unsigned i = 0; i < 4; ++i)
        data.push_back(char(length >> (i * 8)));
      data.insert(data.end(), bytes.begin(), bytes.end());
      it = pool.offsets.emplace(std::move(bytes), offset).first;
    }
    pool.sites[e] = it->second;
  }
};
Name helperName(Module &m, std::string name) {
  while (m.getFunctionOrNull(name))
    name += ".";
  return Name(name);
}
struct RewriteArrays
    : PostWalker<RewriteArrays, UnifiedExpressionVisitor<RewriteArrays>> {
  HeapType candidate;
  ArrayConstants &analysis;
  Builder b;
  Name getUnsigned, getSigned, length;
  RewriteArrays(Module &m, HeapType c, ArrayConstants &a, Name u, Name s,
                Name l)
      : candidate(c), analysis(a), b(m), getUnsigned(u), getSigned(s),
        length(l) {}
  void visitExpression(Expression *e) {
    if (auto it = analysis.pool.sites.find(e);
        it != analysis.pool.sites.end()) {
      replaceCurrent(b.makeRefI31(b.makeConst(int32_t(it->second))));
      return;
    }
    if (analysis.writes.contains(e)) {
      auto *store = e->cast<ArraySet>();
      store->ref = b.makeRefCast(store->ref, Type(candidate, NonNullable));
      store->finalize();
      return;
    }
    if (analysis.gets.contains(e)) {
      auto *get = e->cast<ArrayGet>();
      replaceCurrent(b.makeCall(get->signed_ ? getSigned : getUnsigned,
                                {get->ref, get->index}, Type::i32));
      return;
    }
    if (analysis.lengths.contains(e)) {
      replaceCurrent(b.makeCall(length, {e->cast<ArrayLen>()->ref}, Type::i32));
      return;
    }
    // Nonconstant fixed constructions keep ordinary dynamic array backing.
    if (!e->is<ArrayNewFixed>() && !e->is<ArrayNew>())
      e->type = widen(e->type, candidate);
  }
};
#include "array_dispatch.h"
#include "array_globals.h"
#include "array_union.h"

size_t lowerArray(Module &m, HeapType candidate,
                  const ArrayInitializations &init, bool globals) {
  if (!Uses(m, candidate, init, globals).check())
    return 0;
  ArrayConstants analysis(candidate, init);
  for (auto &f : m.functions)
    if (!f->imported())
      analysis.walk(f->body);
  auto &pool = analysis.pool;
  if (analysis.overflow || pool.sites.empty())
    return 0;
  if (globals)
    return lowerArrayGlobals(m, candidate, analysis);
  if (m.functions.size() > 999997 ||
      (!pool.data.empty() &&
       (m.memories.size() >= 100 || m.dataSegments.size() >= 100000)))
    return 0;
  auto memory = memoryName(m);
  auto unsignedName = helperName(m, "moss.constant.array.get");
  // Reserve distinct names before adding helpers.
  auto signedName = helperName(m, "moss.constant.array.get.signed");
  auto lengthName = helperName(m, "moss.constant.array.length");
  for (auto &f : m.functions) {
    f->setParams(widen(f->getParams(), candidate));
    f->setResults(widen(f->getResults(), candidate));
    for (auto &var : f->vars)
      var = widen(var, candidate);
  }
  RewriteArrays rewrite(m, candidate, analysis, unsignedName, signedName,
                        lengthName);
  for (auto &f : m.functions)
    if (!f->imported())
      rewrite.walk(f->body);
  Builder b(m);
  bool nonempty = !pool.data.empty();
  bool dynamic = analysis.constructions > pool.sites.size();
  if (nonempty) {
    auto mem = std::make_unique<Memory>();
    mem->name = memory;
    mem->initial = mem->max = (pool.data.size() + 65535) / 65536;
    m.addMemory(std::move(mem));
    auto data = std::make_unique<DataSegment>();
    data->name = memory;
    data->memory = memory;
    data->offset = b.makeConst(0);
    data->data = std::move(pool.data);
    m.addDataSegment(std::move(data));
  }
  auto handle = [&] { return b.makeLocalGet(0, Type(HeapType::eq, Nullable)); };
  auto index = [&] { return b.makeLocalGet(1, Type::i32); };
  auto offset = [&] { return b.makeLocalGet(2, Type::i32); };
  auto len = [&] { return b.makeLocalGet(3, Type::i32); };
  auto immediate = [&] {
    return b.makeI31Get(
        b.makeRefCast(handle(), Type(HeapType::i31, NonNullable)), false);
  };
  for (bool sign : {false, true}) {
    auto f = std::make_unique<Function>();
    f->name = sign ? signedName : unsignedName;
    f->setParams(Type({Type(HeapType::eq, Nullable), Type::i32}));
    f->setResults(Type::i32);
    f->vars = {Type::i32, Type::i32};
    Expression *fast = b.makeUnreachable();
    if (nonempty)
      fast = b.makeBlock(
          {b.makeLocalSet(2, immediate()),
           b.makeIf(
               b.makeBinary(EqInt32, offset(), b.makeConst(int32_t(Empty))),
               b.makeUnreachable()),
           b.makeLocalSet(
               3, b.makeLoad(4, false, 0, 4, offset(), Type::i32, memory)),
           b.makeIf(b.makeBinary(GeUInt32, index(), len()),
                    b.makeUnreachable()),
           b.makeLoad(
               1, sign && candidate.getArray().element.packedType == Field::i8,
               4, 1, b.makeBinary(AddInt32, offset(), index()), Type::i32,
               memory)},
          Type::i32);
    f->body = b.makeIf(
        b.makeRefTest(handle(), Type(HeapType::i31, NonNullable)), fast,
        b.makeArrayGet(b.makeRefCast(handle(), Type(candidate, NonNullable)),
                       index(), MemoryOrder::Unordered, Type::i32,
                       sign && candidate.getArray().element.packedType ==
                                   Field::i8));
    if (!dynamic)
      f->body = fast;
    m.addFunction(std::move(f));
  }
  auto f = std::make_unique<Function>();
  f->name = lengthName;
  f->setParams(Type(HeapType::eq, Nullable));
  f->setResults(Type::i32);
  f->vars = {Type::i32};
  Expression *fast = b.makeConst(0);
  if (nonempty)
    fast = b.makeBlock(
        {b.makeLocalSet(1, immediate()),
         b.makeIf(b.makeBinary(EqInt32, index(), b.makeConst(int32_t(Empty))),
                  b.makeConst(0),
                  b.makeLoad(4, false, 0, 4, index(), Type::i32, memory))},
        Type::i32);
  f->body = b.makeIf(
      b.makeRefTest(handle(), Type(HeapType::i31, NonNullable)), fast,
      b.makeArrayLen(b.makeRefCast(handle(), Type(candidate, NonNullable))));
  if (!dynamic) {
    if (!nonempty)
      fast = b.makeBlock({b.makeLocalSet(1, immediate()), fast}, Type::i32);
    f->body = fast;
  }
  m.addFunction(std::move(f));
  return pool.sites.size();
}

} // namespace
static size_t poolBytes(wasm::Module *module, bool globals) noexcept {
  try {
    using namespace wasm;
    if (!module->features.hasGC() ||
        (!globals && !module->features.hasMultiMemory()))
      return 0;
    NormalizeArrayBlocks blocks(*module);
    NormalizeArrayFills fills(*module);
    for (auto &f : module->functions)
      if (!f->imported()) {
        blocks.remaining = 1024 * 1024;
        blocks.walk(f->body);
        fills.walk(f->body);
      }
    normalizeArrayCopies(*module);
    ArrayInitializations initialization(*module);
    auto types = ModuleUtils::collectHeapTypes(*module);
    size_t sites = 0;
    for (auto type : types)
      if (byteArray(type))
        sites += lowerArray(*module, type, initialization, globals);
    for (auto &f : module->functions) {
      f->effects.reset();
      if (!f->imported())
        ReFinalize().walkFunctionInModule(f.get(), module);
    }
    module->indirectCallEffects.clear();
    return sites;
  } catch (...) {
    // Never unwind C++ exceptions across Rust. A partially changed module is
    // discarded by the caller, not silently treated as an unoptimized module.
    return size_t(-1);
  }
}

extern "C" size_t moss_pool_static_bytes(wasm::Module *module) noexcept {
  return poolBytes(module, false);
}
extern "C" size_t moss_pool_static_byte_globals(wasm::Module *module) noexcept {
  return poolBytes(module, true);
}

extern "C" size_t moss_simplify_array_dispatch(wasm::Module *module) noexcept {
  try {
    return simplifyArrayDispatch(*module);
  } catch (...) {
    return size_t(-1);
  }
}

// Closed-world type merging may rebuild private recursive groups. Only enable
// it when the module has no reference-valued host boundary: otherwise foreign
// modules can fabricate structurally matching "private" GC types and pass them
// through an abstract reference ABI. Pooling itself does not need this gate.
extern "C" bool moss_static_array_types_closed(wasm::Module *module) noexcept {
  try {
    using namespace wasm;
    auto refs = [](Type type) {
      for (auto part : type)
        if (part.isRef())
          return true;
      return false;
    };
    for (auto &f : module->functions)
      if (f->imported() && (refs(f->getParams()) || refs(f->getResults())))
        return false;
    for (auto &g : module->globals)
      if (g->imported() && refs(g->type))
        return false;
    for (auto &t : module->tables)
      if (t->imported())
        return false;
    for (auto &t : module->tags)
      if (t->imported() && refs(t->type.getSignature().params))
        return false;
    for (auto &ex : module->exports) {
      switch (ex->kind) {
      case ExternalKind::Function: {
        auto *f = module->getFunction(*ex->getInternalName());
        if (refs(f->getParams()) || refs(f->getResults()))
          return false;
        break;
      }
      case ExternalKind::Global:
        if (refs(module->getGlobal(*ex->getInternalName())->type))
          return false;
        break;
      case ExternalKind::Table:
        return false;
      case ExternalKind::Tag:
        if (refs(module->getTag(*ex->getInternalName())
                     ->type.getSignature()
                     .params))
          return false;
        break;
      default:
        break;
      }
    }
    return true;
  } catch (...) {
    return false;
  }
}

extern "C" size_t moss_coalesce_array_unions(wasm::Module *module) noexcept {
  try {
    return coalesceArrayUnions(*module);
  } catch (...) {
    return size_t(-1);
  }
}
