#include "config.h"

#include <binaryen-c.h>
#include <wasmtime.h>

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <iterator>
#include <memory>
#include <mutex>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#include <windows.h>
#elif defined(__APPLE__)
#include <mach-o/dyld.h>
#include <unistd.h>
#else
#include <unistd.h>
#endif

namespace {

namespace fs = std::filesystem;
using namespace std::string_view_literals;

template <auto destroy>
struct Delete {
  void operator()(auto *value) const { destroy(value); }
};

template <class T, auto destroy>
using Owned = std::unique_ptr<T, Delete<destroy>>;

using Engine = Owned<wasm_engine_t, wasm_engine_delete>;
using Module = Owned<wasmtime_module_t, wasmtime_module_delete>;
using Wasi = Owned<wasi_config_t, wasi_config_delete>;

std::string utf8(const fs::path &path) {
  auto bytes = path.generic_u8string();
  return {bytes.begin(), bytes.end()};
}

fs::path path_from_utf8(std::string_view value) {
  return fs::path(std::u8string(value.begin(), value.end()));
}

std::optional<fs::path> env_path(const char *name) {
#ifdef _WIN32
  auto wide = fs::path(name).wstring();
  if (auto *value = _wgetenv(wide.c_str()))
    return fs::path(value);
#else
  if (auto *value = std::getenv(name))
    return fs::path(value);
#endif
  return {};
}

fs::path executable() {
#ifdef _WIN32
  std::wstring name(32768, L'\0');
  auto size = GetModuleFileNameW(nullptr, name.data(), name.size());
  if (!size || size == name.size())
    throw std::runtime_error("could not locate the Moss executable");
  name.resize(size);
  return fs::canonical(name);
#elif defined(__APPLE__)
  std::uint32_t size = 0;
  _NSGetExecutablePath(nullptr, &size);
  std::string name(size, '\0');
  if (_NSGetExecutablePath(name.data(), &size) != 0)
    throw std::runtime_error("could not locate the Moss executable");
  return fs::canonical(name.c_str());
#else
  return fs::canonical("/proc/self/exe");
#endif
}

void check(wasmtime_error_t *raw) {
  if (!raw)
    return;
  Owned<wasmtime_error_t, wasmtime_error_delete> error(raw);
  wasm_byte_vec_t message;
  wasmtime_error_message(error.get(), &message);
  const auto bytes = std::string(message.data, message.size);
  wasm_byte_vec_delete(&message);
  throw std::runtime_error(bytes);
}

void check_trap(wasm_trap_t *raw) {
  if (!raw)
    return;
  Owned<wasm_trap_t, wasm_trap_delete> trap(raw);
  wasm_byte_vec_t message;
  wasm_trap_message(trap.get(), &message);
  const auto bytes = std::string(message.data, message.size);
  wasm_byte_vec_delete(&message);
  throw std::runtime_error(bytes);
}

std::string read(const fs::path &path) {
  std::ifstream stream(path, std::ios::binary);
  if (!stream)
    throw std::runtime_error("could not read " + utf8(path));
  stream.exceptions(std::ios::badbit);
  return {std::istreambuf_iterator<char>(stream), {}};
}

Module compile_module(const Engine &engine, std::string_view wasm) {
  wasmtime_module_t *module = nullptr;
  check(wasmtime_module_new(engine.get(),
                            reinterpret_cast<const uint8_t *>(wasm.data()),
                            wasm.size(), &module));
  return Module(module);
}

Module load_compiler(const Engine &engine, const fs::path &prefix) {
  if (auto path = env_path("MOSS_COMPILER"))
    return compile_module(engine, read(*path));
  auto path = prefix / "libexec/moss/compiler.cwasm";
  wasmtime_module_t *module = nullptr;
  // The package builds this file with the same Wasmtime and target.
#ifdef _WIN32
  // Some Windows filesystems do not support Wasmtime's executable file mapping.
  const auto bytes = read(path);
  check(wasmtime_module_deserialize(engine.get(),
                                    reinterpret_cast<const uint8_t *>(bytes.data()),
                                    bytes.size(), &module));
#else
  check(wasmtime_module_deserialize_file(engine.get(), utf8(path).c_str(), &module));
#endif
  return Module(module);
}

Wasi wasi_config(const fs::path &directory, bool writable,
                 const std::vector<std::string> &args) {
  Wasi config(wasi_config_new());
  if (!config)
    throw std::runtime_error("could not create a WASI configuration");
  std::vector<const char *> argv;
  for (const auto &arg : args)
    argv.push_back(arg.c_str());
  if (!wasi_config_set_argv(config.get(), argv.size(), argv.data()))
    throw std::runtime_error("could not set the WASI arguments");
  if (!wasi_config_preopen_dir(config.get(), utf8(directory).c_str(), ".", writable))
    throw std::runtime_error("could not open directory: " + utf8(directory));
  wasi_config_inherit_stderr(config.get());
  return config;
}

struct Capture {
  std::string bytes;
  std::mutex mutex;

  static ptrdiff_t write(void *data, const unsigned char *bytes, size_t size) noexcept {
    auto &capture = *static_cast<Capture *>(data);
    try {
      std::lock_guard lock(capture.mutex);
      capture.bytes.append(reinterpret_cast<const char *>(bytes), size);
      return size;
    } catch (...) {
      return -1;
    }
  }
};

int run(const Engine &engine, const Module &module, Wasi config) {
  Owned<wasmtime_linker_t, wasmtime_linker_delete> linker(wasmtime_linker_new(engine.get()));
  Owned<wasmtime_store_t, wasmtime_store_delete> store(
      wasmtime_store_new(engine.get(), nullptr, nullptr));
  auto *context = wasmtime_store_context(store.get());
  check(wasmtime_linker_define_wasi(linker.get()));
  check(wasmtime_context_set_wasi(context, config.release()));

  wasmtime_instance_t instance;
  wasm_trap_t *trap = nullptr;
  check(wasmtime_linker_instantiate(linker.get(), context, module.get(), &instance, &trap));
  check_trap(trap);

  wasmtime_extern_t entry;
  if (!wasmtime_instance_export_get(context, &instance, "_start", 6, &entry))
    throw std::runtime_error("module does not export `_start`");
  if (entry.kind != WASMTIME_EXTERN_FUNC) {
    wasmtime_extern_delete(&entry);
    throw std::runtime_error("`_start` is not a function");
  }
  const auto start = entry.of.func;
  wasmtime_extern_delete(&entry);
  auto *error = wasmtime_func_call(context, &start, nullptr, 0, nullptr, 0, &trap);
  int status;
  if (error && wasmtime_error_exit_status(error, &status)) {
    wasmtime_error_delete(error);
    return status;
  }
  check(error);
  check_trap(trap);
  return 0;
}

constexpr auto compiler_bug = "\nThis is a compiler bug, not a problem with your program; "
                              "please report it at https://github.com/moss-lang/moss/issues "
                              "with the program that triggered it."sv;

void validate(const Engine &engine, std::string_view bytes, bool optimized = false) {
  try {
    check(wasmtime_module_validate(engine.get(),
                                   reinterpret_cast<const uint8_t *>(bytes.data()), bytes.size()));
  } catch (const std::runtime_error &error) {
    if (optimized)
      throw std::runtime_error(std::string("Binaryen's optimizer turned a valid WebAssembly "
                                           "module into an invalid one: ") + error.what() +
                               "\nPlease report the program and optimization level at "
                               "https://github.com/moss-lang/moss/issues. Building without -O avoids it.");
    throw std::runtime_error(std::string("the compiler wrote an invalid WebAssembly module: ") +
                             error.what() + std::string(compiler_bug));
  }
}

struct Optimization {
  int level;
  int shrink;
};

std::string optimize(const Engine &engine, std::string bytes, Optimization optimization) {
  validate(engine, bytes);
  auto *raw = BinaryenModuleReadWithFeatures(bytes.data(), bytes.size(), MOSS_BINARYEN_FEATURES);
  if (!raw)
    throw std::runtime_error("Binaryen could not read the compiler output");
  Owned<wasm::Module, BinaryenModuleDispose> module(raw);
  BinaryenSetOptimizeLevel(optimization.level);
  BinaryenSetShrinkLevel(optimization.shrink);
  BinaryenSetDebugInfo(false);
  BinaryenModuleOptimize(module.get());
  const auto result = BinaryenModuleAllocateAndWrite(module.get(), nullptr);
  Owned<void, std::free> binary(result.binary);
  Owned<char, std::free> source_map(result.sourceMap);
  if (!binary || !result.binaryBytes)
    throw std::runtime_error("Binaryen failed to emit the optimized module");
  return {static_cast<const char *>(binary.get()), result.binaryBytes};
}

std::optional<fs::path> beneath(const fs::path &path, const fs::path &parent) {
  auto current = path.begin();
  for (const auto &component : parent) {
    if (current == path.end() || *current++ != component)
      return {};
  }
  fs::path relative;
  for (; current != path.end(); ++current)
    relative /= *current;
  return relative;
}

fs::path common_directory(fs::path root, const fs::path &cwd, const fs::path &entry) {
  while (!beneath(cwd, root) || !beneath(entry, root)) {
    auto parent = root.parent_path();
    if (parent == root || parent.empty())
      throw std::runtime_error("compiler inputs do not share a filesystem root");
    root = parent;
  }
  return root;
}

std::string render_diagnostics(std::string_view report, const fs::path &root,
                               const fs::path &cwd) {
  auto prefix = utf8(root);
  if (!prefix.ends_with('/'))
    prefix += '/';
  auto working = utf8(cwd);
  if (!working.ends_with('/'))
    working += '/';
  std::string result;
  while (!report.empty()) {
    const auto newline = report.find('\n');
    auto line = report.substr(0, newline);
    if (line.starts_with("  --> ")) {
      const auto location = prefix + std::string(line.substr(6));
      auto path = std::string_view(location);
      if (path.starts_with(working)) {
        const auto relative = path.substr(working.size());
        if (relative != ".." && !relative.starts_with("../") &&
            relative.find("/../") == relative.npos)
          path = relative;
      }
      result += "  --> ";
      result += path;
    } else {
      result += line;
    }
    if (newline == report.npos)
      break;
    result += '\n';
    report.remove_prefix(newline + 1);
  }
  while (!result.empty() && (result.back() == '\n' || result.back() == '\r'))
    result.pop_back();
  return result;
}

std::string run_compiler(const Engine &engine, const Module &compiler,
                         const fs::path &library, const fs::path &cwd, const fs::path &entry) {
  const auto root = common_directory(library, cwd, entry);
  Capture output;
  auto config = wasi_config(root, false,
                            {utf8(*beneath(library / "lib/prelude.moss", root)),
                             utf8(*beneath(entry, root)), "--diagnostics=human"});
  wasi_config_set_stdout_custom(config.get(), Capture::write, &output, nullptr);
  if (const auto status = run(engine, compiler, std::move(config)); status != 0)
    throw std::runtime_error("compiler exited with status " + std::to_string(status));
  if (output.bytes.starts_with("\0asm\x01\0\0\0"sv))
    return std::move(output.bytes);
  if (output.bytes.starts_with("error: "))
    throw std::runtime_error(render_diagnostics(output.bytes, root, cwd));
  throw std::runtime_error("compiler did not write a WebAssembly module" +
                           (output.bytes.empty() ? "" : ":\n" + output.bytes));
}

int run_program(const Engine &engine, std::string_view bytes, const fs::path &cwd,
                const std::vector<std::string> &arguments) {
  Module program;
  try {
    program = compile_module(engine, bytes);
  } catch (const std::runtime_error &error) {
    throw std::runtime_error(std::string("Wasmtime refused the compiled module: ") +
                             error.what() + std::string(compiler_bug));
  }
  auto config = wasi_config(cwd, true, arguments);
  wasi_config_inherit_stdin(config.get());
  wasi_config_inherit_stdout(config.get());
  return run(engine, program, std::move(config));
}

enum class Command { run, build };

constexpr auto help = R"(The Moss compiler and program runner

Usage:
  moss [OPTIONS] FILE [ARGS]...
  moss [OPTIONS] run FILE [ARGS]...
  moss [OPTIONS] build FILE [-o OUTPUT]

Options:
  -O LEVEL              Binaryen optimization level (0, 1, 2, 3, 4, s, or z)
  -o, --output OUTPUT   Write the built module to a file
  -h, --help            Print help
  -V, --version         Print version
)"sv;

struct UsageError : std::runtime_error {
  using std::runtime_error::runtime_error;
};

struct Arguments {
  Command command = Command::run;
  std::optional<Optimization> optimization;
  std::string entry;
  std::optional<fs::path> output;
  std::vector<std::string> program_args;
};

Optimization optimization_level(std::string_view level) {
  if (level.size() == 1 && level[0] >= '0' && level[0] <= '4')
    return {level[0] - '0', 0};
  if (level == "s")
    return {2, 1};
  if (level == "z")
    return {2, 2};
  throw UsageError("invalid optimization level `" + std::string(level) +
                   "`; expected 0, 1, 2, 3, 4, s, or z");
}

std::optional<Arguments> parse(const std::vector<std::string> &argv) {
  if (argv.empty()) {
    std::cerr << help;
    throw UsageError("missing entry file");
  }
  Arguments result;
  bool explicit_command = false;
  bool options = true;
  bool first_program_arg = true;
  for (size_t index = 0; index < argv.size(); ++index) {
    const std::string_view arg = argv[index];
    if (result.command == Command::run && !result.entry.empty()) {
      if (arg != "--" || !first_program_arg || !options)
        result.program_args.emplace_back(arg);
      first_program_arg = false;
    } else if (options && (arg == "-h" || arg == "--help" || arg == "help")) {
      std::cout << help;
      return {};
    } else if (options && (arg == "-V" || arg == "--version")) {
      std::cout << "moss https://github.com/moss-lang/moss/tree/" MOSS_REV "\n";
      return {};
    } else if (options && arg == "--") {
      options = false;
    } else if (options && arg.starts_with("-O")) {
      auto level = arg.substr(2);
      if (level.empty() && index + 1 < argv.size())
        level = argv[++index];
      if (level.starts_with('='))
        level.remove_prefix(1);
      if (result.optimization)
        throw UsageError("-O can only be given once");
      result.optimization = optimization_level(level);
    } else if (options && result.command == Command::build &&
               (arg.starts_with("-o") || arg == "--output" || arg.starts_with("--output="))) {
      if (result.output)
        throw UsageError("--output can only be given once");
      const bool short_option = arg.starts_with("-o");
      auto value = short_option ? arg.substr(2) : arg.substr(8 + (arg.size() > 8));
      if (short_option && value.starts_with('='))
        value.remove_prefix(1);
      if (value.empty() && index + 1 < argv.size())
        value = argv[++index];
      if (value.empty() || value.starts_with('-'))
        throw UsageError("--output requires a file");
      result.output = path_from_utf8(value);
    } else if (options && arg.starts_with('-')) {
      throw UsageError("unexpected argument `" + std::string(arg) + "`");
    } else if (options && !explicit_command && result.entry.empty() &&
               (arg == "build" || arg == "run")) {
      result.command = arg == "build" ? Command::build : Command::run;
      explicit_command = true;
    } else if (result.entry.empty()) {
      result.entry = arg;
      result.program_args.emplace_back(arg);
    } else {
      throw UsageError("unexpected argument `" + std::string(arg) + "`");
    }
  }
  if (result.entry.empty())
    throw UsageError("missing entry file");
  return result;
}

void write_module(const std::optional<fs::path> &output, std::string_view bytes) {
  if (output) {
    std::ofstream stream(*output, std::ios::binary);
    if (!stream.write(bytes.data(), bytes.size()) || !stream.flush())
      throw std::runtime_error("could not write " + utf8(*output));
    return;
  }
#ifdef _WIN32
  if (_isatty(_fileno(stdout)))
#else
  if (isatty(fileno(stdout)))
#endif
    throw std::runtime_error("refusing to write a WebAssembly binary to a terminal; use -o");
  if (!std::cout.write(bytes.data(), bytes.size()) || !std::cout.flush())
    throw std::runtime_error("could not write WebAssembly to stdout");
}

int launcher(const std::vector<std::string> &argv) {
#ifdef _WIN32
  _setmode(_fileno(stdin), _O_BINARY);
  _setmode(_fileno(stdout), _O_BINARY);
#endif
  try {
    const auto args = parse(argv);
    if (!args)
      return 0;
    const auto cwd = fs::current_path();
    const auto prefix = executable().parent_path().parent_path();
    const auto library = fs::absolute(env_path("MOSS_ROOT").value_or(prefix / "share/moss"));
    const auto entry = fs::absolute(path_from_utf8(args->entry));
    Engine engine(wasm_engine_new());
    if (!engine)
      throw std::runtime_error("could not create a Wasmtime engine");
    auto bytes = run_compiler(engine, load_compiler(engine, prefix), library, cwd, entry);
    if (args->optimization)
      bytes = optimize(engine, std::move(bytes), *args->optimization);
    if (args->command == Command::run)
      return run_program(engine, bytes, cwd, args->program_args);
    validate(engine, bytes, args->optimization.has_value());
    write_module(args->output, bytes);
    return 0;
  } catch (const UsageError &error) {
    std::cerr << "error: " << error.what() << "\n\nFor more information, try '--help'.\n";
    return 2;
  } catch (const std::exception &error) {
    std::cerr << error.what() << '\n';
    return 1;
  }
}

} // namespace

#ifdef _WIN32
int wmain(int argc, wchar_t **argv) {
  std::vector<std::string> args;
  for (int i = 1; i < argc; ++i) {
    const auto size = WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, nullptr, 0, nullptr, nullptr);
    if (!size)
      return 2;
    std::string value(size, '\0');
    WideCharToMultiByte(CP_UTF8, 0, argv[i], -1, value.data(), size, nullptr, nullptr);
    value.pop_back();
    args.push_back(std::move(value));
  }
  return launcher(args);
}
#else
int main(int argc, char **argv) {
  return launcher({argv + 1, argv + argc});
}
#endif
