use std::collections::HashMap;

use crate::{
    intern::StrId,
    lower::{
        Aliasdef, Ctxdef, IR, Named, Names, Node, NodeList, Sigdef, Tagdef, Tydef, Valdef,
    },
};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
    named: HashMap<Named, (bool, StrId)>,
}

impl<'a> Dump<'a> {
    fn named_raw(&self, named: Named, name: Option<(bool, StrId)>) {
        match named {
            Named::Module(id) => {
                print!("module #{}", id.index());
            }
            Named::Tydef(def) => {
                print!("type #{}", def.index());
            }
            Named::Tagdef(def) => {
                print!("tag #{}", def.index());
            }
            Named::Aliasdef(def) => {
                print!("alias #{}", def.index());
            }
            Named::Sigdef(def) => {
                print!("sig #{}", def.index());
            }
            Named::Fndef(def) => {
                print!("fn #{}", def.index());
            }
            Named::Valdef(def) => {
                print!("val #{}", def.index());
            }
            Named::Ctxdef(def) => {
                print!("context #{}", def.index());
            }
        }
        if let Some((dot, string)) = name {
            let raw = &self.ir.strings[string];
            if dot {
                print!(" \".{raw}\"");
            } else {
                print!(" \"{raw}\"");
            }
        }
    }

    fn named(&self, named: Named) {
        self.named_raw(named, self.named.get(&named).copied());
    }

    fn list(&self, list: NodeList) {
        let mut first = true;
        for &id in &self.ir.lists[list] {
            if !first {
                print!(", ");
            }
            first = false;
            print!("%{}", id.index());
        }
    }

    fn node(&self, node: Node) {
        match node {
            Node::Nothing => print!("nothing"),
            Node::Lambda {
                level,
                needs,
                result,
            } => {
                print!("lambda({level}) need [");
                self.list(needs);
                print!("] return %{}", result.index());
            }
            Node::Apply { lambda, args } => {
                print!("apply %{} to [", lambda.index());
                self.list(args);
                print!("]");
            }
            Node::List { items } => {
                print!("[");
                self.list(items);
                print!("]");
            }
            Node::Need { level, def, param } => {
                print!("need({level}) ");
                self.named(def.into());
                print!(" via %{}", param.index());
            }
            Node::Tagdef { def } => {
                self.named(Named::Tagdef(def));
            }
            Node::Aliasdef { def } => {
                self.named(Named::Aliasdef(def));
            }
            Node::Tuple { elems } => {
                print!("tuple type (");
                self.list(elems);
                print!(")");
            }
            Node::Context => {
                print!("context type");
            }
            Node::Fndef { def } => {
                self.named(Named::Fndef(def));
            }
            Node::Get { ctx, slot } => {
                print!("slot {} of %{}", slot.index(), ctx.index());
            }
            Node::Dyn { slot } => {
                print!("dynamic slot {}", slot.index());
            }
            Node::Bind { args, bind } => {
                print!("bind for [");
                self.list(args);
                print!("] using %{}", bind.index());
            }
            Node::BindDef { def, bind } => {
                print!("bind ");
                self.named(def.into());
                print!(" via %{}", bind.index());
            }
            Node::Sig { param, result } => {
                let from = param.index();
                let to = result.index();
                print!("function signature %{from} -> %{to}");
            }
        }
    }

    fn program(&mut self) {
        for (&(_, name), &named) in &self.names.names {
            self.named.entry(named).or_insert((false, name));
        }

        for (&(_, name), &named) in &self.names.detached {
            self.named.entry(named.into()).or_insert((true, name));
        }

        for &named in &self.names.order {
            self.named(named);
            let def = match named {
                Named::Module(_) => None,
                Named::Tydef(def) => {
                    let Tydef(node) = self.ir.tydefs[def];
                    Some(node)
                }
                Named::Tagdef(def) => {
                    let Tagdef(node) = self.ir.tagdefs[def];
                    Some(node)
                }
                Named::Aliasdef(def) => {
                    let Aliasdef(node) = self.ir.aliasdefs[def];
                    Some(node)
                }
                Named::Sigdef(def) => {
                    let Sigdef(node) = self.ir.sigdefs[def];
                    Some(node)
                }
                Named::Fndef(def) => {
                    let Sigdef(node) = self.ir.fndefs[def];
                    Some(node)
                }
                Named::Valdef(def) => {
                    let Valdef(node) = self.ir.valdefs[def];
                    Some(node)
                }
                Named::Ctxdef(def) => {
                    let Ctxdef(node) = self.ir.ctxdefs[def];
                    Some(node)
                }
            };
            if let Some(node) = def {
                print!(" = %{}", node.index());
            }
            println!();
        }

        for (i, &node) in self.ir.nodes.iter().enumerate() {
            print!("%{i} = ");
            self.node(node);
            println!();
        }
    }
}

pub fn dump(ir: &IR, names: &Names) {
    Dump {
        ir,
        names,
        named: HashMap::new(),
    }
    .program();
}
