use std::collections::HashMap;

use index_vec::{IndexVec, index_vec};

use crate::{
    intern::StrId,
    lower::{
        Aliasdef, Body, Ctxdef, Expr, IR, Instr, InstrId, InstrList, ModuleId, Named, Names,
        Sigdef, Tagdef, Tydef, Valdef,
    },
};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
    source: HashMap<Named, (ModuleId, StrId)>,
    printed: IndexVec<InstrId, bool>,
    indent: usize,
}

impl<'a> Dump<'a> {
    fn named_raw(&self, named: Named, name: Option<StrId>) -> Option<Body> {
        let body = match named {
            Named::Module(id) => {
                print!("module #{}", id.index());
                None
            }
            Named::Tydef(def) => {
                print!("type #{}", def.index());
                let Tydef(body) = self.ir.tydefs[def];
                Some(body)
            }
            Named::Tagdef(def) => {
                print!("tag #{}", def.index());
                let Tagdef(body) = self.ir.tagdefs[def];
                Some(body)
            }
            Named::Aliasdef(def) => {
                print!("alias #{}", def.index());
                let Aliasdef(body) = self.ir.aliasdefs[def];
                Some(body)
            }
            Named::Sigdef(def) => {
                print!("sig #{}", def.index());
                let Sigdef(body) = self.ir.sigdefs[def];
                Some(body)
            }
            Named::Fndef(def) => {
                print!("fn #{}", def.index());
                let Sigdef(body) = self.ir.fndefs[def];
                Some(body)
            }
            Named::Valdef(def) => {
                print!("val #{}", def.index());
                let Valdef(body) = self.ir.valdefs[def];
                Some(body)
            }
            Named::Ctxdef(def) => {
                print!("context #{}", def.index());
                let Ctxdef(body) = self.ir.ctxdefs[def];
                Some(body)
            }
        };
        if let Some(string) = name {
            print!(" \"{}\"", &self.ir.strings[string]);
        }
        body
    }

    fn named(&self, named: Named) -> Option<Body> {
        match self.source.get(&named) {
            Some(&(_, name)) => self.named_raw(named, Some(name)),
            None => self.named_raw(named, None),
        }
    }

    fn items(&self, items: InstrList) {
        let mut first = true;
        for item in items {
            if !first {
                print!(",");
            }
            first = false;

            print!(" %{}", self.ir.items[item].index());
        }
    }

    fn expr(&self, expr: Expr) {
        match expr {
            Expr::Param { ty } => {
                print!("param typed %{}", ty.index());
            }
            Expr::Copy { value } => {
                print!("copy %{}", value.index());
            }
            Expr::Nominal { ty, inner } => {
                print!("nominal %{} from %{}", ty.index(), inner.index());
            }
            Expr::Tuple { elems } => {
                print!("tuple");
                self.items(elems);
            }
            Expr::Record { fields } => {
                print!("record");
                for &(name, value) in fields.get(&self.ir.records) {
                    print!(" {}=%{}", &self.ir.strings[name], value.index());
                }
            }
            Expr::Elem { tuple, index } => {
                print!("element {} from %{}", index.index(), tuple.index());
            }
            Expr::Field { record, index } => {
                print!("field {} from %{}", index.index(), record.index());
            }
            Expr::Val { val } => {
                print!("value %{}", val.index());
            }
            Expr::Call { func, arg } => {
                print!("call %{} with %{}", func.index(), arg.index());
            }
        }
    }

    fn instr(&mut self, instr: InstrId) {
        let instruction = self.ir.instrs[instr];
        print!("    ");
        if let Instr::EndLambda {
            start: _,
            result: _,
        } = instruction
        {
            self.indent -= 1;
        }
        for _ in 0..self.indent {
            print!("  ")
        }
        print!("%{}", instr.index());
        if let Instr::Expr { ty, expr: _ } = instruction {
            print!(": %{}", ty.index());
        }
        print!(" = ");
        if self.printed[instr] {
            println!("duplicate");
            return;
        }

        self.printed[instr] = true;
        match instruction {
            Instr::Lambda => {
                print!("lambda");
                self.indent += 1;
            }
            Instr::EndLambda { start, result } => {
                print!(
                    "end lambda %{} returning %{}",
                    start.index(),
                    result.index(),
                );
            }
            Instr::Apply { lambda, args } => {
                print!("apply %{} to", lambda.index());
                self.items(args);
            }
            Instr::Stack { items } => {
                print!("stack");
                self.items(items);
            }
            Instr::NeedTydef { def, param } => {
                print!("need ");
                self.named(Named::Tydef(def));
                print!(" via %{}", param.index());
            }
            Instr::NeedSigdef { def, param } => {
                print!("need ");
                self.named(Named::Sigdef(def));
                print!(" via %{}", param.index());
            }
            Instr::NeedValdef { def, param } => {
                print!("need ");
                self.named(Named::Valdef(def));
                print!(" via %{}", param.index());
            }
            Instr::NeedCtxdef { def, param } => {
                print!("need ");
                self.named(Named::Ctxdef(def));
                print!(" via %{}", param.index());
            }
            Instr::Tagdef { def } => {
                print!("tag ");
                self.named(Named::Tagdef(def));
            }
            Instr::Aliasdef { def } => {
                print!("alias ");
                self.named(Named::Aliasdef(def));
            }
            Instr::Tuple { elems } => {
                print!("tuple");
                self.items(elems);
            }
            Instr::Record { fields } => {
                print!("record");
                for &(name, value) in fields.get(&self.ir.records) {
                    print!(" {}=%{}", &self.ir.strings[name], value.index());
                }
            }
            Instr::Context => {
                print!("context");
            }
            Instr::Fndef { def } => {
                print!("fn ");
                self.named(Named::Fndef(def));
            }
            Instr::Get { ctx, slot } => {
                print!("get slot {} from %{}", slot.index(), ctx.index());
            }
            Instr::Lit { val } => {
                print!("lit {:?}", val);
            }
            Instr::Bind { args, bind } => {
                print!("bind");
                self.items(args);
                print!(" to %{}", bind.index());
            }
            Instr::BindTydef { def, bind } => {
                print!("bind ");
                self.named(Named::Tydef(def));
                print!(" with %{}", bind.index());
            }
            Instr::BindSigdef { def, bind } => {
                print!("bind ");
                self.named(Named::Sigdef(def));
                print!(" with %{}", bind.index());
            }
            Instr::BindValdef { def, bind } => {
                print!("bind ");
                self.named(Named::Valdef(def));
                print!(" with %{}", bind.index());
            }
            Instr::BindCtxdef { def, bind } => {
                print!("bind ");
                self.named(Named::Ctxdef(def));
                print!(" with %{}", bind.index());
            }
            Instr::Sig { param, result } => {
                print!("sig from %{} to %{}", param.index(), result.index());
            }
            Instr::Set { lhs, rhs } => {
                print!("set %{} to %{}", lhs.index(), rhs.index());
            }
            Instr::If { ty, cond } => {
                print!("if %{} producing %{}", cond.index(), ty.index());
            }
            Instr::Else { result } => {
                print!("else with %{}", result.index());
            }
            Instr::EndIf { result } => {
                print!("end if with %{}", result.index());
            }
            Instr::Loop => {
                print!("loop");
            }
            Instr::EndLoop => {
                print!("end loop");
            }
            Instr::Br { depth } => {
                print!("br {}", depth.0);
            }
            Instr::Expr { ty: _, expr } => {
                self.expr(expr);
            }
        }
        println!();
    }

    fn body(&mut self, body: Body) {
        self.indent = 0;
        for instr in body.body {
            self.instr(instr);
        }
        println!("    return %{}", body.result().index());
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

            self.named(Named::Module(module));
            print!(" {{");

            for &named in by_module.get(&module).into_iter().flatten() {
                let (original, name) = self.source[&named];
                if module != original {
                    continue;
                }

                println!();
                print!("  ");
                let contents = self.named_raw(named, Some(name));
                match contents {
                    None => println!(";"),
                    Some(body) => {
                        println!(" {{");
                        self.body(body);
                        println!("  }}");
                    }
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
                    self.indent = 0;
                }
                Some(before) => {
                    if before + 1 != instr {
                        println!("  }}");
                        println!();
                        println!("  {{");
                        self.indent = 0;
                    }
                }
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
        indent: 0,
    }
    .program();
}
