// Alternative target for the same checked array construction/use graph.
// Included inside static_bytes.cpp's anonymous namespace, after ArrayConstants.
//
// Passive data is copied into one GC array per distinct payload at module
// startup. Literal evaluation only reads a private global; array operations and
// dynamic constructions keep their original GC representation. No linear
// memory is created. The resource cost moves to instantiation, not each use.
struct RewriteGlobalArrays : PostWalker<RewriteGlobalArrays> {
  ArrayConstants &analysis;
  const std::unordered_map<uint32_t, Name> &globals;
  HeapType candidate;
  Builder b;

  RewriteGlobalArrays(Module &m, HeapType c, ArrayConstants &a,
                      const std::unordered_map<uint32_t, Name> &g)
      : analysis(a), globals(g), candidate(c), b(m) {}

  void visitArrayNewFixed(ArrayNewFixed *node) {
    auto it = analysis.pool.sites.find(node);
    if (it == analysis.pool.sites.end())
      return;
    auto name = globals.at(it->second);
    if (it->second == Empty) {
      replaceCurrent(
          b.makeGlobalGet(name, Type(candidate, NonNullable, Exact)));
    } else {
      // Nonempty globals are populated by the generated module start before
      // the original start or any externally callable function can use them.
      replaceCurrent(
          b.makeRefAs(RefAsNonNull,
                      b.makeGlobalGet(name, Type(candidate, Nullable, Exact))));
    }
  }
};

Name globalArrayName(Module &m, size_t &next) {
  // Keep name generation linear in the number of pool entries, including
  // modules containing preexisting names in this private-name family.
  std::string name;
  do {
    name = "moss.constant.array." + std::to_string(next++);
  } while (m.getGlobalOrNull(name));
  return Name(name);
}
Name globalArrayDataName(Module &m) {
  size_t next = m.dataSegments.size();
  std::string name;
  do {
    name = "moss.constant.array.data." + std::to_string(next++);
  } while (m.getDataSegmentOrNull(name));
  return Name(name);
}
Name globalArrayStartName(Module &m) {
  size_t next = m.functions.size();
  std::string name;
  do {
    name = "moss.constant.array.initialize." + std::to_string(next++);
  } while (m.getFunctionOrNull(name));
  return Name(name);
}

size_t lowerArrayGlobals(Module &m, HeapType candidate,
                         ArrayConstants &analysis) {
  auto &pool = analysis.pool;
  if (!pool.offsets.empty() && !m.features.hasBulkMemory())
    return 0;
  if (pool.sites.empty())
    return 0;
  bool empty = false;
  for (auto [_, offset] : pool.sites)
    empty |= offset == Empty;
  size_t count = pool.offsets.size() + size_t(empty);
  // Bound generated startup code and respect the target validator's module
  // limits. All resource checks precede mutation. The source-byte pool budget
  // also applies to expanded i32 data, so it is not silently quadrupled.
  if (count > 100000 || m.globals.size() + count > 1000000)
    return 0;
  if (!pool.offsets.empty() &&
      (m.dataSegments.size() >= 100000 || m.functions.size() >= 1000000))
    return 0;

  struct Entry {
    uint32_t sourceOffset;
    uint32_t dataOffset;
    uint32_t length;
  };
  std::vector<Entry> entries;
  std::vector<char> bytes;
  bool packed = candidate.getArray().element.packedType == Field::i8;
  size_t stride = packed ? 1 : 4;
  std::vector<std::pair<const std::vector<char> *, uint32_t>> payloads;
  for (const auto &[payload, offset] : pool.offsets)
    payloads.emplace_back(&payload, offset);
  std::sort(payloads.begin(), payloads.end(),
            [](const auto &a, const auto &b) { return a.second < b.second; });
  for (const auto &[payloadPointer, offset] : payloads) {
    const auto &payload = *payloadPointer;
    if (payload.size() > (MaxPoolBytes - bytes.size()) / stride)
      return 0;
    entries.push_back(
        {offset, uint32_t(bytes.size()), uint32_t(payload.size())});
    if (packed) {
      bytes.insert(bytes.end(), payload.begin(), payload.end());
    } else {
      // array.new_data decodes the destination storage type. The memory target
      // compresses i32 byte values to one byte, but GC i32 elements require
      // four little-endian bytes each in this segment.
      for (char value : payload) {
        bytes.push_back(char(uint8_t(value)));
        bytes.insert(bytes.end(), 3, 0);
      }
    }
  }

  Builder b(m);
  std::unordered_map<uint32_t, Name> globals;
  size_t nextGlobal = m.globals.size();
  if (empty) {
    auto name = globalArrayName(m, nextGlobal);
    m.addGlobal(Builder::makeGlobal(
        name, Type(candidate, NonNullable, Exact),
        b.makeArrayNewFixed(candidate, std::vector<Expression *>{}),
        Builder::Immutable));
    globals.emplace(Empty, name);
  }
  std::vector<Expression *> initialize;
  if (!entries.empty()) {
    auto dataName = globalArrayDataName(m);
    auto segment = std::make_unique<DataSegment>();
    segment->name = dataName;
    // An absent memory name makes the segment passive. Active segments are
    // dropped before the start function executes and cannot serve these reads.
    segment->data = std::move(bytes);
    m.addDataSegment(std::move(segment));
    for (auto entry : entries) {
      auto name = globalArrayName(m, nextGlobal);
      m.addGlobal(Builder::makeGlobal(name, Type(candidate, Nullable, Exact),
                                      b.makeRefNull(candidate),
                                      Builder::Mutable));
      globals.emplace(entry.sourceOffset, name);
      initialize.push_back(b.makeGlobalSet(
          name, b.makeArrayNewData(candidate, dataName,
                                   b.makeConst(int32_t(entry.dataOffset)),
                                   b.makeConst(int32_t(entry.length)))));
    }
    // This newly introduced segment is private, so no source instruction can
    // observe its lifetime. Release it once all global backing arrays exist.
    if (m.features.hasBulkMemory())
      initialize.push_back(b.makeDataDrop(dataName));
    auto previousStart = m.start;
    if (previousStart)
      initialize.push_back(
          b.makeCall(previousStart, std::vector<Expression *>{}, Type::none));
    auto start = std::make_unique<Function>();
    start->name = globalArrayStartName(m);
    start->setParams(Type::none);
    start->setResults(Type::none);
    start->body = b.makeBlock(initialize);
    m.start = start->name;
    m.addFunction(std::move(start));
  }

  RewriteGlobalArrays rewrite(m, candidate, analysis, globals);
  for (auto &f : m.functions)
    if (!f->imported())
      rewrite.walk(f->body);
  return pool.sites.size();
}
