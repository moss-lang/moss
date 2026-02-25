use std::collections::HashMap;

use index_vec::{IndexVec, index_vec};

use crate::{
    intern::StrId,
    lower::{Body, IR, Instr, InstrId, ModuleId, Named, Names, Tydef},
};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
    source: HashMap<Named, (ModuleId, StrId)>,
    printed: IndexVec<InstrId, bool>,
}

impl<'a> Dump<'a> {
    fn named_raw(&self, named: Named, name: StrId) -> Option<Body> {
        match named {
            Named::Module(_) => todo!(),
            Named::Tydef(def) => {
                print!("type #{} \"{}\"", def.index(), &self.ir.strings[name]);
                let Tydef(body) = self.ir.tydefs[def];
                Some(body)
            }
            Named::Tagdef(def) => todo!(),
            Named::Aliasdef(def) => todo!(),
            Named::Sigdef(def) => todo!(),
            Named::Fndef(def) => todo!(),
            Named::Valdef(def) => todo!(),
            Named::Ctxdef(def) => todo!(),
        }
    }

    fn named(&self, named: Named) -> Option<Body> {
        let (_, name) = self.source[&named];
        self.named_raw(named, name)
    }

    fn instr(&mut self, instr: InstrId) {
        print!("    %{} = ", instr.index());
        if self.printed[instr] {
            println!("duplicate");
            return;
        }

        self.printed[instr] = true;
        match self.ir.instrs[instr] {
            Instr::Lambda => print!("lambda {{"),
            Instr::EndLambda { result } => todo!(),
            Instr::Apply { lambda, args } => todo!(),
            Instr::Stack { items } => {
                print!("stack");

                let mut first = true;
                for item in items {
                    if !first {
                        print!(",");
                    }
                    first = false;

                    print!(" %{}", self.ir.items[item].index());
                }
            }
            Instr::NeedTydef { def, param } => todo!(),
            Instr::NeedSigdef { def, param } => todo!(),
            Instr::NeedValdef { def, param } => todo!(),
            Instr::NeedCtxdef { def, param } => todo!(),
            Instr::Tagdef { def } => todo!(),
            Instr::Aliasdef { def } => todo!(),
            Instr::Tuple { elems } => todo!(),
            Instr::Record { fields } => todo!(),
            Instr::Context => todo!(),
            Instr::Fndef { def } => todo!(),
            Instr::Get { ctx, slot } => todo!(),
            Instr::Lit { val } => todo!(),
            Instr::Bind { args, bind } => todo!(),
            Instr::BindTydef { def, bind } => todo!(),
            Instr::BindSigdef { def, bind } => todo!(),
            Instr::BindValdef { def, bind } => todo!(),
            Instr::BindCtxdef { def, bind } => todo!(),
            Instr::Sig { param, result } => todo!(),
            Instr::Set { lhs, rhs } => todo!(),
            Instr::If { ty, cond } => todo!(),
            Instr::Else { result } => todo!(),
            Instr::EndIf { result } => todo!(),
            Instr::Loop => todo!(),
            Instr::EndLoop => todo!(),
            Instr::Br { depth } => todo!(),
            Instr::Expr { ty, expr } => todo!(),
        }
        println!();
    }

    fn body(&mut self, body: Body) {
        for instr in body.body {
            self.instr(instr);
        }
        println!("    return %{}", body.result().index());
        println!("  }}");
    }

    fn program(&mut self) {
        self.source = HashMap::<Named, (ModuleId, StrId)>::new();
        let mut by_module = HashMap::<ModuleId, Vec<Named>>::new();
        for (&(module, name), &named) in &self.names.names {
            self.source.entry(named).or_insert((module, name));
            by_module.entry(module).or_default().push(named);
        }

        let mut first = true;
        for (module, _) in self.ir.modules.iter_enumerated() {
            if !first {
                println!();
            }
            first = false;

            print!("module #{} {{", module.index());

            for &named in by_module.get(&module).into_iter().flatten() {
                let (original, name) = self.source[&named];
                if module != original {
                    continue;
                }

                println!();
                print!("  ");
                let contents = self.named_raw(named, name);
                println!(" {{");
                if let Some(body) = contents {
                    self.body(body);
                }
            }

            println!("}}");
        }

        let mut prev = None;
        for (instr, _) in self.ir.instrs.iter_enumerated() {
            if self.printed[instr] {
                continue;
            }

            match prev {
                None => {
                    if !first {
                        println!();
                    }
                    first = false;

                    println!("unfinished {{");
                    println!("  {{");
                }
                Some(_) => todo!(),
            }
            self.instr(instr);
            prev = Some(instr);
        }
        if prev.is_some() {
            println!("  }}");
            println!("}}");
        }
    }
}

pub fn dump(ir: &IR, names: &Names) {
    Dump {
        ir,
        names,
        source: HashMap::new(),
        printed: index_vec![false; ir.instrs.len()],
    }
    .program();
}
