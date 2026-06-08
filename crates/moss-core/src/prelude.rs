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
    /// The digit/radix values, or [`None`] while the prelude module that declares them is itself
    /// being lowered (in which case the literal desugar resolves them by name from the current
    /// module instead).
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
}

impl Precompile {
    fn error(&self, path: &Path, source: &str, byte: ByteIndex, message: &str) {
        let lines = LineIndex::new(source);
        let line_col = lines.line_col(TextSize::new(byte.raw()));
        let line = line_col.line + 1;
        let col = line_col.col + 1;
        panic!("{}:{line}:{col} {message}", path.display())
    }

    fn lib(&mut self, path: StrId, base: Option<Base>) -> io::Result<ModuleId> {
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
                self.lib(id, base)
            })
            .collect::<io::Result<Vec<_>>>()?;
        let module = lower(
            &source,
            &starts,
            &tree,
            &mut self.ir,
            &mut self.names,
            base,
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
        Ok(module)
    }

    fn tydef(&self, module: ModuleId, name: &str) -> TydefId {
        let id = self.ir.strings.get_id(name).unwrap();
        match self.names.names.get(&(module, id)) {
            Some(&Named::Tydef(def)) => def,
            _ => panic!("missing base type `{name}`"),
        }
    }

    fn sigdef(&self, module: ModuleId, name: &str) -> SigdefId {
        let id = self.ir.strings.get_id(name).unwrap();
        match self.names.names.get(&(module, id)) {
            Some(&Named::Sigdef(def)) => def,
            _ => panic!("missing base function `{name}`"),
        }
    }

    fn valdef(&self, module: ModuleId, name: &str) -> ValdefId {
        let id = self.ir.strings.get_id(name).unwrap();
        match self.names.names.get(&(module, id)) {
            Some(&Named::Valdef(def)) => def,
            _ => panic!("missing base value `{name}`"),
        }
    }

    fn prelude(mut self) -> io::Result<(IR, Names, Base, Lib)> {
        let path_literal = self.ir.strings.make_id("./literal.moss");
        let path_int = self.ir.strings.make_id("./int.moss");
        let path_char = self.ir.strings.make_id("./char.moss");
        let path_string = self.ir.strings.make_id("./string.moss");
        let path_ops = self.ir.strings.make_id("./ops.moss");
        let literal = self.lib(path_literal, None)?;
        let int = self.lib(path_int, None)?;
        let char = self.lib(path_char, None)?;
        let string = self.lib(path_string, None)?;
        let ops = self.lib(path_ops, None)?;
        let base_types = Types {
            uint32: self.tydef(int, "Uint32"),
            int32: self.tydef(int, "Int32"),
            uint64: self.tydef(int, "Uint64"),
            int64: self.tydef(int, "Int64"),
            uint: self.tydef(int, "Uint"),
            int: self.tydef(int, "Int"),
            char: self.tydef(char, "Char"),
            string: self.tydef(string, "String"),
        };
        let base_builders = Builders {
            char_from_codepoint: self.sigdef(char, "char_from_codepoint"),
            string_builder: self.sigdef(string, "string_builder"),
            set_char: self.sigdef(string, "set_char"),
            build: self.sigdef(string, "build"),
        };
        let base_arith = Arith {
            add: self.sigdef(ops, "add"),
            mul: self.sigdef(ops, "mul"),
            add_out: self.tydef(ops, "AddOut"),
            mul_out: self.tydef(ops, "MulOut"),
            lhs: self.tydef(ops, "Lhs"),
            rhs: self.tydef(ops, "Rhs"),
        };
        // The digit/radix values live in `std.moss`, which is lowered as part of the prelude below
        // and so is not yet available here. During that lowering the literal desugar resolves them
        // by name from the current module; afterwards we fill them in for downstream compilation.
        let base = Base {
            types: base_types,
            numerals: None,
            builders: base_builders,
            arith: base_arith,
        };
        let path_prelude = self.ir.strings.make_id("./prelude.moss");
        let prelude = self.lib(path_prelude, Some(base))?;
        let std = self.modules[&self.ir.strings.get_id("./std.moss").unwrap()];
        let base = Base {
            numerals: Some(Numerals {
                number: self.tydef(std, "Number"),
                digit0: self.valdef(std, "digit0"),
                digit1: self.valdef(std, "digit1"),
                digit2: self.valdef(std, "digit2"),
                digit3: self.valdef(std, "digit3"),
                digit4: self.valdef(std, "digit4"),
                digit5: self.valdef(std, "digit5"),
                digit6: self.valdef(std, "digit6"),
                digit7: self.valdef(std, "digit7"),
                digit8: self.valdef(std, "digit8"),
                digit9: self.valdef(std, "digit9"),
                radix: self.valdef(std, "radix"),
            }),
            ..base
        };
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
    }
    .prelude()
    .unwrap()
}
