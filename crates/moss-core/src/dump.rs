use crate::lower::{Aliasdef, Ctxdef, IR, Names, Sigdef, Tagdef, Tydef, Valdef};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
}

impl<'a> Dump<'a> {
    fn program(&mut self) {
        for (i, node) in self.ir.nodes.iter().enumerate() {
            println!("%{i} = {node:?}");
        }
        for &named in &self.names.order {
            match named {
                crate::lower::Named::Module(_) => todo!(),
                crate::lower::Named::Tydef(def) => {
                    println!();
                    let Tydef(node) = self.ir.tydefs[def];
                    println!("type #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Tagdef(def) => {
                    println!();
                    let Tagdef(node) = self.ir.tagdefs[def];
                    println!("tag #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Aliasdef(def) => {
                    println!();
                    let Aliasdef(node) = self.ir.aliasdefs[def];
                    println!("alias #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Sigdef(def) => {
                    println!();
                    let Sigdef(node) = self.ir.sigdefs[def];
                    println!("sig #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Fndef(def) => {
                    println!();
                    let Sigdef(node) = self.ir.fndefs[def];
                    println!("fn #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Valdef(def) => {
                    println!();
                    let Valdef(node) = self.ir.valdefs[def];
                    println!("val #{} = %{}", def.index(), node.index());
                }
                crate::lower::Named::Ctxdef(def) => {
                    println!();
                    let Ctxdef(node) = self.ir.ctxdefs[def];
                    println!("context #{} = %{}", def.index(), node.index());
                }
            }
        }
    }
}

pub fn dump(ir: &IR, names: &Names) {
    Dump { ir, names }.program();
}
