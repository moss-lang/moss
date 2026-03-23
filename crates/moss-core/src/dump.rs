use crate::lower::{IR, Names};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
}

impl<'a> Dump<'a> {
    fn program(&mut self) {
        for (i, node) in self.ir.nodes.iter().enumerate() {
            println!("%{i} = {node:?}");
        }
    }
}

pub fn dump(ir: &IR, names: &Names) {
    Dump { ir, names }.program();
}
