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
    lower::{FndefId, IR, ModuleId, Named, Names, TydefId, ValdefId, lower},
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

#[derive(Clone, Copy, Debug)]
pub struct LitTypes {
    pub uint31: TydefId,
    pub uint32: TydefId,
    pub int32: TydefId,
    pub uint63: TydefId,
    pub uint64: TydefId,
    pub int64: TydefId,
    pub uint: TydefId,
    pub int: TydefId,
    pub char: TydefId,
    pub string: TydefId,
}

#[derive(Clone, Copy, Debug)]
pub struct LitVals {
    pub uint31: ValdefId,
    pub uint32: ValdefId,
    pub int32: ValdefId,
    pub uint63: ValdefId,
    pub uint64: ValdefId,
    pub int64: ValdefId,
    pub uint: ValdefId,
    pub int: ValdefId,
    pub char: ValdefId,
    pub string: ValdefId,
}

#[derive(Clone, Copy, Debug)]
pub struct Lits {
    pub uint31_realize_uint32: FndefId,
    pub uint32_realize_uint32: FndefId,
    pub uint31_realize_int32: FndefId,
    pub int32_realize_int32: FndefId,
    pub uint31_realize_uint64: FndefId,
    pub uint32_realize_uint64: FndefId,
    pub uint63_realize_uint64: FndefId,
    pub uint64_realize_uint64: FndefId,
    pub uint31_realize_int64: FndefId,
    pub uint32_realize_int64: FndefId,
    pub int32_realize_int64: FndefId,
    pub uint63_realize_int64: FndefId,
    pub int64_realize_int64: FndefId,
    pub uint31_realize_uint: FndefId,
    pub uint32_realize_uint: FndefId,
    pub uint63_realize_uint: FndefId,
    pub uint64_realize_uint: FndefId,
    pub uint_realize_uint: FndefId,
    pub uint31_realize_int: FndefId,
    pub uint32_realize_int: FndefId,
    pub int32_realize_int: FndefId,
    pub uint63_realize_int: FndefId,
    pub uint64_realize_int: FndefId,
    pub int64_realize_int: FndefId,
    pub uint_realize_int: FndefId,
    pub int_realize_int: FndefId,
    pub char_realize: FndefId,
    pub string_realize: FndefId,
}

#[derive(Clone, Copy, Debug)]
pub struct Base {
    pub types: Types,
    pub lit_types: LitTypes,
    pub lit_vals: LitVals,
    pub lits: Lits,
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

    fn fndef(&self, module: ModuleId, name: &str) -> FndefId {
        let id = self.ir.strings.get_id(name).unwrap();
        match self.names.names.get(&(module, id)) {
            Some(&Named::Fndef(def)) => def,
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
        let literal = self.lib(path_literal, None)?;
        let int = self.lib(path_int, None)?;
        let char = self.lib(path_char, None)?;
        let string = self.lib(path_string, None)?;
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
        let base_lit_types = LitTypes {
            uint31: self.tydef(literal, "LiteralUint31"),
            uint32: self.tydef(literal, "LiteralUint32"),
            int32: self.tydef(literal, "LiteralInt32"),
            uint63: self.tydef(literal, "LiteralUint63"),
            uint64: self.tydef(literal, "LiteralUint64"),
            int64: self.tydef(literal, "LiteralInt64"),
            uint: self.tydef(literal, "LiteralUint"),
            int: self.tydef(literal, "LiteralInt"),
            char: self.tydef(literal, "LiteralChar"),
            string: self.tydef(literal, "LiteralString"),
        };
        let base_lit_vals = LitVals {
            uint31: self.valdef(literal, "literal_uint31"),
            uint32: self.valdef(literal, "literal_uint32"),
            int32: self.valdef(literal, "literal_int32"),
            uint63: self.valdef(literal, "literal_uint63"),
            uint64: self.valdef(literal, "literal_uint64"),
            int64: self.valdef(literal, "literal_int64"),
            uint: self.valdef(literal, "literal_uint"),
            int: self.valdef(literal, "literal_int"),
            char: self.valdef(literal, "literal_char"),
            string: self.valdef(literal, "literal_string"),
        };
        let base_lits = Lits {
            uint31_realize_uint32: self.fndef(literal, "uint31_realize_uint32"),
            uint32_realize_uint32: self.fndef(literal, "uint32_realize_uint32"),
            uint31_realize_int32: self.fndef(literal, "uint31_realize_int32"),
            int32_realize_int32: self.fndef(literal, "int32_realize_int32"),
            uint31_realize_uint64: self.fndef(literal, "uint31_realize_uint64"),
            uint32_realize_uint64: self.fndef(literal, "uint32_realize_uint64"),
            uint63_realize_uint64: self.fndef(literal, "uint63_realize_uint64"),
            uint64_realize_uint64: self.fndef(literal, "uint64_realize_uint64"),
            uint31_realize_int64: self.fndef(literal, "uint31_realize_int64"),
            uint32_realize_int64: self.fndef(literal, "uint32_realize_int64"),
            int32_realize_int64: self.fndef(literal, "int32_realize_int64"),
            uint63_realize_int64: self.fndef(literal, "uint63_realize_int64"),
            int64_realize_int64: self.fndef(literal, "int64_realize_int64"),
            uint31_realize_uint: self.fndef(literal, "uint31_realize_uint"),
            uint32_realize_uint: self.fndef(literal, "uint32_realize_uint"),
            uint63_realize_uint: self.fndef(literal, "uint63_realize_uint"),
            uint64_realize_uint: self.fndef(literal, "uint64_realize_uint"),
            uint_realize_uint: self.fndef(literal, "uint_realize_uint"),
            uint31_realize_int: self.fndef(literal, "uint31_realize_int"),
            uint32_realize_int: self.fndef(literal, "uint32_realize_int"),
            int32_realize_int: self.fndef(literal, "int32_realize_int"),
            uint63_realize_int: self.fndef(literal, "uint63_realize_int"),
            uint64_realize_int: self.fndef(literal, "uint64_realize_int"),
            int64_realize_int: self.fndef(literal, "int64_realize_int"),
            uint_realize_int: self.fndef(literal, "uint_realize_int"),
            int_realize_int: self.fndef(literal, "int_realize_int"),
            char_realize: self.fndef(literal, "char_realize"),
            string_realize: self.fndef(literal, "string_realize"),
        };
        let base = Base {
            types: base_types,
            lit_types: base_lit_types,
            lit_vals: base_lit_vals,
            lits: base_lits,
        };
        let path_prelude = self.ir.strings.make_id("./prelude.moss");
        let prelude = self.lib(path_prelude, Some(base))?;
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
