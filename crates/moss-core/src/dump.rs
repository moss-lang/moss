use std::collections::HashMap;

use crate::{
    intern::StrId,
    lower::{
        Aliasdef, Ctxdef, IR, Named, Names, Node, NodeList, Sigdef, Tagdef, Tydef, Val, Valdef,
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
            Node::NeedTydef { level, def, param } => {
                print!("need({level}) ");
                self.named(Named::Tydef(def));
                print!(" via %{}", param.index());
            }
            Node::NeedSigdef { level, def, param } => {
                print!("need({level}) ");
                self.named(Named::Sigdef(def));
                print!(" via %{}", param.index());
            }
            Node::NeedValdef { level, def, param } => {
                print!("need({level}) ");
                self.named(Named::Valdef(def));
                print!(" via %{}", param.index());
            }
            Node::NeedCtxdef { level, def, param } => {
                print!("need({level}) ");
                self.named(Named::Ctxdef(def));
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
            Node::Lit { val } => match val {
                Val::Uint31(n) => print!("uint31 {n}"),
                Val::Uint32(n) => print!("uint32 {n}"),
                Val::Int32(n) => print!("int32 {n}"),
                Val::Uint63(n) => print!("uint63 {n}"),
                Val::Uint64(n) => print!("uint64 {n}"),
                Val::Int64(n) => print!("int64 {n}"),
                Val::Uint(s) => print!("uint {}", &self.ir.strings[s]),
                Val::Int(s) => print!("int {}", &self.ir.strings[s]),
                Val::Char(c) => print!("c {c:?}"),
                Val::String(s) => print!("string {s:?}"),
            },
            Node::Bind { args, bind } => {
                print!("bind for [");
                self.list(args);
                print!("] using %{}", bind.index());
            }
            Node::BindTydef { def, bind } => {
                print!("bind ");
                self.named(Named::Tydef(def));
                print!(" via %{}", bind.index());
            }
            Node::BindSigdef { def, bind } => {
                print!("bind ");
                self.named(Named::Sigdef(def));
                print!(" via %{}", bind.index());
            }
            Node::BindValdef { def, bind } => {
                print!("bind ");
                self.named(Named::Valdef(def));
                print!(" via %{}", bind.index());
            }
            Node::BindCtxdef { def, bind } => {
                print!("bind ");
                self.named(Named::Ctxdef(def));
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
