use std::{
    collections::HashMap,
    env,
    ffi::OsStr,
    fs, io,
    path::{self, Path, PathBuf},
};

use line_index::{LineIndex, TextSize};

use crate::{
    intern::StrId,
    lex::{ByteIndex, lex, string},
    lower::{IR, ModuleId, Named, Names, SigdefId, TydefId, ValdefId, lower},
    parse::{ParseError, parse},
};

/// Directory to standard library directory.
///
/// Should always be `None` for distribution. Usually set to `Some("lib")` for local development.
const DIR: Option<&str> = option_env!("MOSS_LIB");

#[derive(Clone, Copy, Debug)]
pub struct Types {
    pub uint32: TydefId,
    pub int32: TydefId,
    pub uint64: TydefId,
    pub int64: TydefId,
    pub uint: TydefId,
    pub int: TydefId,
    pub char: TydefId,
    pub string: TydefId,
}

/// The contextual digit values and radix used to desugar numeric literals.
#[derive(Clone, Copy, Debug)]
pub struct Numerals {
    pub number: TydefId,
    pub digit0: ValdefId,
    pub digit1: ValdefId,
    pub digit2: ValdefId,
    pub digit3: ValdefId,
    pub digit4: ValdefId,
    pub digit5: ValdefId,
    pub digit6: ValdefId,
    pub digit7: ValdefId,
    pub digit8: ValdefId,
    pub digit9: ValdefId,
    pub radix: ValdefId,
}

/// The contextual operations used to desugar string and character literals.
#[derive(Clone, Copy, Debug)]
pub struct Builders {
    pub char_from_codepoint: SigdefId,
    pub string_builder: SigdefId,
    pub set_char: SigdefId,
    pub build: SigdefId,
}

/// The arithmetic operations (and their output types) used to combine numeric digits.
#[derive(Clone, Copy, Debug)]
pub struct Arith {
    pub add: SigdefId,
    pub mul: SigdefId,
    pub add_out: TydefId,
    pub mul_out: TydefId,
    pub lhs: TydefId,
    pub rhs: TydefId,
}

#[derive(Clone, Copy, Debug)]
pub struct Base {
    pub types: Types,
    /// The digit/radix values, or [`None`] before the prelude module that declares them
    /// (`std.moss`) has been lowered. The precompile fills this in as soon as `std.moss` is
    /// lowered, so it is always [`Some`] by the time any module containing a numeric literal is
    /// lowered, and the literal desugar resolves the digits contextually through it.
    pub numerals: Option<Numerals>,
    pub builders: Builders,
    pub arith: Arith,
}

#[derive(Clone, Copy, Debug)]
pub struct Lib {
    pub char: ModuleId,
    pub int: ModuleId,
    pub literal: ModuleId,
    pub prelude: ModuleId,
    pub string: ModuleId,
    pub types: ModuleId,
    pub wasm: ModuleId,
    pub wasip1: ModuleId,
    pub wasi: ModuleId,
}

struct Precompile {
    ir: IR,
    names: Names,
    preprelude: ModuleId,
    dir: PathBuf,
    modules: HashMap<StrId, ModuleId>,
    /// The base context threaded into each module as it is lowered. [`None`] until the types,
    /// builders, and arithmetic it references have themselves been lowered; its [`Base::numerals`]
    /// field stays [`None`] only until `std.moss` (which declares the digit/radix values) is
    /// lowered, after which it is filled in for every later module.
    base: Option<Base>,
}

impl Precompile {
    fn error(&self, path: &Path, source: &str, byte: ByteIndex, message: &str) {
        let lines = LineIndex::new(source);
        let line_col = lines.line_col(TextSize::new(byte.raw()));
        let line = line_col.line + 1;
        let col = line_col.col + 1;
        panic!("{}:{line}:{col} {message}", path.display())
    }

    fn lib(&mut self, path: StrId) -> io::Result<ModuleId> {
        if let Some(&module) = self.modules.get(&path) {
            return Ok(module);
        }
        let absolute = path::absolute(self.dir.join(&self.ir.strings[path]))?;
        let source = fs::read_to_string(&absolute)?;
        let (tokens, starts) = lex(&source).unwrap();
        let tree = parse(&tokens)
            .map_err(|err| match err {
                ParseError::Expected { id, tokens: _ } => {
                    self.error(&absolute, &source, starts[id], &err.message())
                }
            })
            .unwrap();
        let imports = tree
            .imports
            .iter()
            .map(|import| {
                let s = string(&source, &starts, import.from);
                let id = self.ir.strings.make_id(&s);
                self.lib(id)
            })
            .collect::<io::Result<Vec<_>>>()?;
        let module = lower(
            &source,
            &starts,
            &tree,
            &mut self.ir,
            &mut self.names,
            self.base,
            self.preprelude,
            &imports,
        )
        .map_err(|err| {
            let (tokens, message) = err.describe(&source, &starts, &tree, &self.ir, &self.names);
            let start = match tokens {
                Some(range) => starts[range.first],
                None => ByteIndex::new(0),
            };
            self.error(&absolute, &source, start, &message)
        })
        .unwrap();
        self.modules.insert(path, module);
        // `std.moss` declares the digit/radix values that numeric literals desugar to. As soon as
        // it has been lowered, fill in `base.numerals` (with stable valdef ids) so that every later
        // module's literals resolve their digits contextually through `Base` rather than failing.
        if self.ir.strings.get_id("./std.moss") == Some(path) {
            self.fill_numerals(module);
        }
        Ok(module)
    }

    /// Populate `base.numerals` from the digit/radix values declared in the (now-lowered) `std`
    /// module.
    fn fill_numerals(&mut self, std: ModuleId) {
        let base = self
            .base
            .as_mut()
            .expect("base must be built before std.moss is lowered");
        base.numerals = Some(Numerals {
            number: tydef(&self.ir, &self.names, std, "Number"),
            digit0: valdef(&self.ir, &self.names, std, "digit0"),
            digit1: valdef(&self.ir, &self.names, std, "digit1"),
            digit2: valdef(&self.ir, &self.names, std, "digit2"),
            digit3: valdef(&self.ir, &self.names, std, "digit3"),
            digit4: valdef(&self.ir, &self.names, std, "digit4"),
            digit5: valdef(&self.ir, &self.names, std, "digit5"),
            digit6: valdef(&self.ir, &self.names, std, "digit6"),
            digit7: valdef(&self.ir, &self.names, std, "digit7"),
            digit8: valdef(&self.ir, &self.names, std, "digit8"),
            digit9: valdef(&self.ir, &self.names, std, "digit9"),
            radix: valdef(&self.ir, &self.names, std, "radix"),
        });
    }

    fn prelude(mut self) -> io::Result<(IR, Names, Base, Lib)> {
        let path_literal = self.ir.strings.make_id("./literal.moss");
        let path_int = self.ir.strings.make_id("./int.moss");
        let path_char = self.ir.strings.make_id("./char.moss");
        let path_string = self.ir.strings.make_id("./string.moss");
        let path_ops = self.ir.strings.make_id("./ops.moss");
        let literal = self.lib(path_literal)?;
        let int = self.lib(path_int)?;
        let char = self.lib(path_char)?;
        let string = self.lib(path_string)?;
        let ops = self.lib(path_ops)?;
        let base_types = Types {
            uint32: tydef(&self.ir, &self.names, int, "Uint32"),
            int32: tydef(&self.ir, &self.names, int, "Int32"),
            uint64: tydef(&self.ir, &self.names, int, "Uint64"),
            int64: tydef(&self.ir, &self.names, int, "Int64"),
            uint: tydef(&self.ir, &self.names, int, "Uint"),
            int: tydef(&self.ir, &self.names, int, "Int"),
            char: tydef(&self.ir, &self.names, char, "Char"),
            string: tydef(&self.ir, &self.names, string, "String"),
        };
        let base_builders = Builders {
            char_from_codepoint: sigdef(&self.ir, &self.names, char, "char_from_codepoint"),
            string_builder: sigdef(&self.ir, &self.names, string, "string_builder"),
            set_char: sigdef(&self.ir, &self.names, string, "set_char"),
            build: sigdef(&self.ir, &self.names, string, "build"),
        };
        let base_arith = Arith {
            add: sigdef(&self.ir, &self.names, ops, "add"),
            mul: sigdef(&self.ir, &self.names, ops, "mul"),
            add_out: tydef(&self.ir, &self.names, ops, "AddOut"),
            mul_out: tydef(&self.ir, &self.names, ops, "MulOut"),
            lhs: tydef(&self.ir, &self.names, ops, "Lhs"),
            rhs: tydef(&self.ir, &self.names, ops, "Rhs"),
        };
        // The digit/radix values live in `std.moss`, which is lowered as part of the prelude below.
        // `numerals` therefore starts out `None`, but `lib` fills it in as soon as `std.moss` has
        // been lowered (before any later module's literals are desugared).
        self.base = Some(Base {
            types: base_types,
            numerals: None,
            builders: base_builders,
            arith: base_arith,
        });
        let path_prelude = self.ir.strings.make_id("./prelude.moss");
        let prelude = self.lib(path_prelude)?;
        let base = self
            .base
            .expect("base.numerals must be filled in once std.moss has been lowered");
        debug_assert!(base.numerals.is_some(), "std.moss should have been lowered");
        let types = self.modules[&self.ir.strings.get_id("./types.moss").unwrap()];
        let wasm = self.modules[&self.ir.strings.get_id("./wasm.moss").unwrap()];
        let wasip1 = self.modules[&self.ir.strings.get_id("./wasip1.moss").unwrap()];
        let wasi = self.modules[&self.ir.strings.get_id("./wasi.moss").unwrap()];
        let lib = Lib {
            char,
            int,
            literal,
            prelude,
            string,
            types,
            wasm,
            wasip1,
            wasi,
        };
        Ok((self.ir, self.names, base, lib))
    }
}

fn tydef(ir: &IR, names: &Names, module: ModuleId, name: &str) -> TydefId {
    let id = ir.strings.get_id(name).unwrap();
    match names.names.get(&(module, id)) {
        Some(&Named::Tydef(def)) => def,
        _ => panic!("missing base type `{name}`"),
    }
}

fn sigdef(ir: &IR, names: &Names, module: ModuleId, name: &str) -> SigdefId {
    let id = ir.strings.get_id(name).unwrap();
    match names.names.get(&(module, id)) {
        Some(&Named::Sigdef(def)) => def,
        _ => panic!("missing base function `{name}`"),
    }
}

fn valdef(ir: &IR, names: &Names, module: ModuleId, name: &str) -> ValdefId {
    let id = ir.strings.get_id(name).unwrap();
    match names.names.get(&(module, id)) {
        Some(&Named::Valdef(def)) => def,
        _ => panic!("missing base value `{name}`"),
    }
}

fn get_lib_dir() -> Option<PathBuf> {
    // We must use `std::fs::canonicalize` here instead of `std::path::absolute` to account for
    // cases where the path to the compiler executable contains symlinks, as happens in the Nix
    // build for the VS Code extension, for instance.
    let exe = fs::canonicalize(env::args().next()?).ok()?;
    let bin = exe.parent()?;
    if bin.file_name() != Some(OsStr::new("bin")) {
        return None;
    }
    Some(bin.parent()?.join("lib"))
}

pub fn prelude() -> (IR, Names, Base, Lib) {
    let dir = match DIR {
        Some(dir) => fs::canonicalize(dir).ok(),
        None => get_lib_dir(),
    }
    .unwrap();
    let mut ir = IR::default();
    let names = Names::default();
    let preprelude = ir.modules.push(());
    let modules = HashMap::new();
    Precompile {
        ir,
        names,
        preprelude,
        dir,
        modules,
        base: None,
    }
    .prelude()
    .unwrap()
}
