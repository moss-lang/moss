use crate::{
    lex::lex,
    lower::{IR, ModuleId, Names, lower},
    parse::parse,
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
}

impl Precompile {
    fn lib(&mut self, imports: &[ModuleId], source: &str) -> ModuleId {
        let (tokens, starts) = lex(source).unwrap();
        lower(
            source,
            &starts,
            &parse(&tokens).unwrap(),
            &mut self.ir,
            &mut self.names,
            None,
            imports,
        )
        .unwrap()
    }

    fn prelude(mut self) -> (IR, Names, Lib) {
        let wasm = self.lib(&[], include_str!("../../../lib/wasm.con"));
        let wasip1 = self.lib(&[wasm], include_str!("../../../lib/wasip1.con"));
        let wasi = self.lib(&[wasip1, wasm], include_str!("../../../lib/wasi.con"));
        let prelude = self.lib(&[wasi], include_str!("../../../lib/prelude.con"));
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
    Precompile {
        ir: IR::default(),
        names: Names::default(),
    }
    .prelude()
}
