use crate::{
    lex::lex,
    lower::{IR, ModuleId, Names, Tydef, Type, lower},
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
    preprelude: ModuleId,
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
            self.preprelude,
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
    Precompile {
        ir,
        names,
        preprelude,
    }
    .prelude()
}
