use std::collections::HashMap;

use crate::{
    intern::StrId,
    lower::{IR, Instr, ModuleId, Named, Names, Tydef},
};

pub fn dump(ir: &IR, names: &Names) {
    let mut defined_in = HashMap::<Named, (ModuleId, StrId)>::new();
    let mut by_module = HashMap::<ModuleId, Vec<Named>>::new();
    for (&(module, name), &named) in &names.names {
        defined_in.entry(named).or_insert((module, name));
        by_module.entry(module).or_default().push(named);
    }

    let mut first = true;
    for (module, _) in ir.modules.iter_enumerated() {
        if !first {
            println!();
        }
        first = false;

        println!("module #{} {{", module.index());

        let mut first = true;
        for &named in by_module.get(&module).into_iter().flatten() {
            let (original, name) = defined_in[&named];
            if module != original {
                continue;
            }

            if !first {
                println!();
            }
            first = false;

            match named {
                Named::Module(_) => todo!(),
                Named::Tydef(def) => {
                    println!("  type #{} \"{}\" {{", def.index(), &ir.strings[name]);
                    let Tydef(body) = ir.tydefs[def];
                    for instr in body.body {
                        print!("    %{} = ", instr.index());
                        match ir.instrs[instr] {
                            Instr::Lambda => todo!(),
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

                                    print!(" %{}", ir.items[item].index());
                                }

                                println!();
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
                    }
                    println!("    return %{}", body.result().index());
                    println!("  }}");
                }
                Named::Tagdef(def) => todo!(),
                Named::Aliasdef(def) => todo!(),
                Named::Sigdef(def) => todo!(),
                Named::Fndef(def) => todo!(),
                Named::Valdef(def) => todo!(),
                Named::Ctxdef(def) => todo!(),
            }
        }

        println!("}}");
    }

    if !first {
        println!();
    }
    first = false;
    println!("unfinished {{");
    println!("}}");

    let _ = first;
}
