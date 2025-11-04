use index_vec::IndexSlice;

use crate::{
    lex::lex,
    lower::{IR, ModuleId, Names, lower},
    parse::parse,
};

struct Lib {
    ir: IR,
    names: Names,
}

impl Lib {
    fn lib(&mut self, imports: &[ModuleId], source: &str) -> ModuleId {
        let (tokens, starts) = lex(source).unwrap();
        lower(
            source,
            &starts,
            &parse(&tokens).unwrap(),
            &mut self.ir,
            &mut self.names,
            None,
            IndexSlice::new(imports),
        )
        .unwrap()
    }

    fn prelude(mut self) -> (IR, Names, ModuleId) {
        let wasm = self.lib(&[], include_str!("../../../lib/wasm.con"));
        let wasip1 = self.lib(&[wasm], include_str!("../../../lib/wasip1.con"));
        let wasi = self.lib(&[wasip1, wasm], include_str!("../../../lib/wasi.con"));
        let prelude = self.lib(&[wasi], include_str!("../../../lib/prelude.con"));
        (self.ir, self.names, prelude)
    }
}

pub fn prelude() -> (IR, Names, ModuleId) {
    Lib {
        ir: IR::default(),
        names: Names::default(),
    }
    .prelude()
}
