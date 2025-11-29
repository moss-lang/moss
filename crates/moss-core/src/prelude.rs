use line_index::{LineIndex, TextSize};

use crate::{
    lex::{ByteIndex, lex},
    lower::{IR, ModuleId, Names, Tydef, Type, lower},
    parse::{ParseError, parse},
};

pub struct Lib {
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

    fn prelude(mut self) -> (IR, Names, Lib) {
        let wasm = self.lib(&[], include_str!("../../../lib/wasm.moss"));
        let wasip1 = self.lib(&[wasm], include_str!("../../../lib/wasip1.moss"));
        let wasi = self.lib(&[wasip1, wasm], include_str!("../../../lib/wasi.moss"));
        let prelude = self.lib(&[wasi], include_str!("../../../lib/prelude.moss"));
        let lib = Lib {
            wasm,
            wasip1,
            wasi,
            prelude,
        };
        (self.ir, self.names, lib)
    }
}

pub fn prelude() -> (IR, Names, Lib) {
    let mut ir = IR::default();
    let mut names = Names::default();
    let preprelude = ir.modules.push(());
    {
        let name = ir.strings.make_id("StringLit");
        let ty = ir.ty(Type::String);
        let tydef = ir.tydefs.push(Tydef { def: Some(ty) });
        names.tydefs.insert((preprelude, name), tydef);
    }
    {
        let name = ir.strings.make_id("Bool");
        let ty = ir.ty(Type::Bool);
        let tydef = ir.tydefs.push(Tydef { def: Some(ty) });
        names.tydefs.insert((preprelude, name), tydef);
    }
    {
        let name = ir.strings.make_id("RawInt32");
        let ty = ir.ty(Type::Int32);
        let tydef = ir.tydefs.push(Tydef { def: Some(ty) });
        names.tydefs.insert((preprelude, name), tydef);
    }
    {
        let name = ir.strings.make_id("RawInt64");
        let ty = ir.ty(Type::Int64);
        let tydef = ir.tydefs.push(Tydef { def: Some(ty) });
        names.tydefs.insert((preprelude, name), tydef);
    }
    Precompile {
        ir,
        names,
        preprelude,
    }
    .prelude()
}
