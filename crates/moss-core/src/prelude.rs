use std::{
    env,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
};

use line_index::{LineIndex, TextSize};

use crate::{
    lex::{ByteIndex, lex},
    lower::{IR, ModuleId, Names, lower},
    parse::{ParseError, parse},
};

/// Directory to standard library directory.
///
/// Should always be `None` for distribution. Usually set to `Some("lib")` for local development.
const DIR: Option<&str> = option_env!("MOSS_LIB");

pub struct Lib {
    pub bool: ModuleId,
    pub wasm: ModuleId,
    pub wasip1: ModuleId,
    pub wasi: ModuleId,
    pub prelude: ModuleId,
}

struct Precompile {
    ir: IR,
    names: Names,
    preprelude: ModuleId,
}

impl Precompile {
    fn error(&self, source: &str, byte: ByteIndex, message: &str) {
        let lines = LineIndex::new(source);
        let line_col = lines.line_col(TextSize::new(byte.raw()));
        let line = line_col.line + 1;
        let col = line_col.col + 1;
        panic!("stdlib file, line {line}, column {col}: {message}")
    }

    fn lib(&mut self, imports: &[ModuleId], source: &str) -> ModuleId {
        let (tokens, starts) = lex(source).unwrap();
        let tree = parse(&tokens)
            .map_err(|err| match err {
                ParseError::Expected { id, tokens: _ } => {
                    self.error(source, starts[id], &err.message())
                }
            })
            .unwrap();
        lower(
            source,
            &starts,
            &tree,
            &mut self.ir,
            &mut self.names,
            self.preprelude,
            imports,
        )
        .map_err(|err| {
            let (tokens, message) = err.describe(source, &starts, &tree, &self.ir, &self.names);
            let start = match tokens {
                Some(range) => starts[range.first],
                None => ByteIndex::new(0),
            };
            self.error(source, start, &message)
        })
        .unwrap()
    }

    fn prelude(mut self, dir: &Path) -> io::Result<(IR, Names, Lib)> {
        let bool = self.lib(&[], &fs::read_to_string(dir.join("bool.moss"))?);
        let wasm = self.lib(&[], &fs::read_to_string(dir.join("wasm.moss"))?);
        let wasip1 = self.lib(&[wasm], &fs::read_to_string(dir.join("wasip1.moss"))?);
        let wasi = self.lib(
            &[bool, wasip1, wasm],
            &fs::read_to_string(dir.join("wasi.moss"))?,
        );
        let prelude = self.lib(
            &[bool, wasi, wasm],
            &fs::read_to_string(dir.join("prelude.moss"))?,
        );
        let lib = Lib {
            bool,
            wasm,
            wasip1,
            wasi,
            prelude,
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
    Precompile {
        ir,
        names,
        preprelude,
    }
    .prelude(&dir)
    .unwrap()
}
