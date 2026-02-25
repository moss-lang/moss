use std::collections::HashMap;

use crate::{
    intern::StrId,
    lower::{IR, ModuleId, Named, Names, Tydef},
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

        println!("module {} {{", module.index());

        let mut first = true;
        for &named in by_module.get(&module).into_iter().flatten() {
            if !first {
                println!();
            }
            first = false;

            let (original, name) = defined_in[&named];
            if module == original {
                match named {
                    Named::Module(_) => todo!(),
                    Named::Tydef(def) => {
                        print!("  type {} \"{}\"", def.index(), &ir.strings[name]);
                        let Tydef(body) = ir.tydefs[def];
                        println!(" {{");
                        for instr in body.body {
                            println!("    {:?}", ir.instrs[instr]);
                        }
                        println!("  }}");
                    }
                    Named::Tagdef(def) => todo!(),
                    Named::Aliasdef(def) => todo!(),
                    Named::Sigdef(def) => todo!(),
                    Named::Fndef(def) => todo!(),
                    Named::Valdef(def) => todo!(),
                    Named::Ctxdef(def) => todo!(),
                }
            } else {
                match named {
                    Named::Module(_) => todo!(),
                    Named::Tydef(def) => todo!(),
                    Named::Tagdef(def) => todo!(),
                    Named::Aliasdef(def) => todo!(),
                    Named::Sigdef(def) => todo!(),
                    Named::Fndef(def) => todo!(),
                    Named::Valdef(def) => todo!(),
                    Named::Ctxdef(def) => todo!(),
                }
            }
        }

        println!("}}");
    }
}
