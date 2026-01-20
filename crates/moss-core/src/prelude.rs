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
    lower::{FndefId, IR, ModuleId, Named, Names, lower},
    parse::{ParseError, parse},
};

/// Directory to standard library directory.
///
/// Should always be `None` for distribution. Usually set to `Some("lib")` for local development.
const DIR: Option<&str> = option_env!("MOSS_LIB");

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
    pub int_realize_int: FndefId,
    pub char_realize: FndefId,
    pub string_realize: FndefId,
}

#[derive(Clone, Copy, Debug)]
pub struct Lib {
    pub literal: ModuleId,
    pub prelude: ModuleId,
    pub types: ModuleId,
    pub wasm: ModuleId,
    pub wasip1: ModuleId,
    pub wasi: ModuleId,
    pub lits: Lits,
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

    fn literal_fndef(&self, lib_literal: ModuleId, name: &str) -> FndefId {
        let id = self.ir.strings.get_id(name).unwrap();
        match self.names.names.get(&(lib_literal, id)) {
            Some(&Named::Fndef(fndef)) => fndef,
            _ => panic!("missing literal function `{name}`"),
        }
    }

    fn prelude(mut self) -> io::Result<(IR, Names, Lib)> {
        let path = self.ir.strings.make_id("./prelude.moss");
        let prelude = self.lib(path)?;
        let literal = self.modules[&self.ir.strings.get_id("./literal.moss").unwrap()];
        let types = self.modules[&self.ir.strings.get_id("./types.moss").unwrap()];
        let wasm = self.modules[&self.ir.strings.get_id("./wasm.moss").unwrap()];
        let wasip1 = self.modules[&self.ir.strings.get_id("./wasip1.moss").unwrap()];
        let wasi = self.modules[&self.ir.strings.get_id("./wasi.moss").unwrap()];
        let lits = Lits {
            uint31_realize_uint32: self.literal_fndef(literal, "uint31_realize_uint32"),
            uint32_realize_uint32: self.literal_fndef(literal, "uint32_realize_uint32"),
            uint31_realize_int32: self.literal_fndef(literal, "uint31_realize_int32"),
            int32_realize_int32: self.literal_fndef(literal, "int32_realize_int32"),
            uint31_realize_uint64: self.literal_fndef(literal, "uint31_realize_uint64"),
            uint32_realize_uint64: self.literal_fndef(literal, "uint32_realize_uint64"),
            uint63_realize_uint64: self.literal_fndef(literal, "uint63_realize_uint64"),
            uint64_realize_uint64: self.literal_fndef(literal, "uint64_realize_uint64"),
            uint31_realize_int64: self.literal_fndef(literal, "uint31_realize_int64"),
            uint32_realize_int64: self.literal_fndef(literal, "uint32_realize_int64"),
            int32_realize_int64: self.literal_fndef(literal, "int32_realize_int64"),
            uint63_realize_int64: self.literal_fndef(literal, "uint63_realize_int64"),
            int64_realize_int64: self.literal_fndef(literal, "int64_realize_int64"),
            uint31_realize_uint: self.literal_fndef(literal, "uint31_realize_uint"),
            uint32_realize_uint: self.literal_fndef(literal, "uint32_realize_uint"),
            uint63_realize_uint: self.literal_fndef(literal, "uint63_realize_uint"),
            uint64_realize_uint: self.literal_fndef(literal, "uint64_realize_uint"),
            uint_realize_uint: self.literal_fndef(literal, "uint_realize_uint"),
            uint31_realize_int: self.literal_fndef(literal, "uint31_realize_int"),
            uint32_realize_int: self.literal_fndef(literal, "uint32_realize_int"),
            int32_realize_int: self.literal_fndef(literal, "int32_realize_int"),
            uint63_realize_int: self.literal_fndef(literal, "uint63_realize_int"),
            uint64_realize_int: self.literal_fndef(literal, "uint64_realize_int"),
            int64_realize_int: self.literal_fndef(literal, "int64_realize_int"),
            int_realize_int: self.literal_fndef(literal, "int_realize_int"),
            char_realize: self.literal_fndef(literal, "char_realize"),
            string_realize: self.literal_fndef(literal, "string_realize"),
        };
        let lib = Lib {
            literal,
            prelude,
            types,
            wasm,
            wasip1,
            wasi,
            lits,
        };
        Ok((self.ir, self.names, lib))
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

pub fn prelude() -> (IR, Names, Lib) {
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
