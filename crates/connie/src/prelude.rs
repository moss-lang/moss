use index_vec::IndexSlice;

use crate::{
    lex::lex,
    lower::{IR, Paths, lower},
    parse::parse,
};

struct Lib {
    ir: IR,
}

impl Lib {
    fn lib(&mut self, imports: &[&Paths], source: &str) -> Paths {
        let (tokens, starts) = lex(source).unwrap();
        let tree = parse(&tokens).unwrap();
        let indexed = IndexSlice::new(imports);
        lower(source, &starts, &tree, &mut self.ir, None, indexed).unwrap()
    }

    fn prelude(mut self) -> (IR, Paths) {
        let wasm = self.lib(&[], include_str!("../../../lib/wasm.con"));
        let wasip1 = self.lib(&[&wasm], include_str!("../../../lib/wasip1.con"));
        let wasi = self.lib(&[&wasip1, &wasm], include_str!("../../../lib/wasi.con"));
        let prelude = self.lib(&[&wasi], include_str!("../../../lib/prelude.con"));
        (self.ir, prelude)
    }
}

pub fn prelude() -> (IR, Paths) {
    Lib { ir: IR::default() }.prelude()
}
