use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    fmt::Write,
};

use crate::{
    intern::StrId,
    lower::{
        Aliasdef, Ctxdef, IR, Named, Names, Node, NodeId, NodeList, Sigdef, Tagdef, Tydef, Val,
        Valdef,
    },
};

struct Dump<'a> {
    ir: &'a IR,
    names: &'a Names,
    named: HashMap<Named, (bool, StrId)>,
}

impl<'a> Dump<'a> {
    fn prepare_names(&mut self) {
        for (&(_, name), &named) in &self.names.names {
            self.named.entry(named).or_insert((false, name));
        }

        for (&(_, name), &named) in &self.names.detached {
            self.named.entry(named.into()).or_insert((true, name));
        }
    }

    fn named_raw(&self, out: &mut impl Write, named: Named, name: Option<(bool, StrId)>) {
        match named {
            Named::Module(id) => {
                let _ = write!(out, "module #{}", id.index());
            }
            Named::Tydef(def) => {
                let _ = write!(out, "type #{}", def.index());
            }
            Named::Tagdef(def) => {
                let _ = write!(out, "tag #{}", def.index());
            }
            Named::Aliasdef(def) => {
                let _ = write!(out, "alias #{}", def.index());
            }
            Named::Sigdef(def) => {
                let _ = write!(out, "sig #{}", def.index());
            }
            Named::Fndef(def) => {
                let _ = write!(out, "fn #{}", def.index());
            }
            Named::Valdef(def) => {
                let _ = write!(out, "val #{}", def.index());
            }
            Named::Ctxdef(def) => {
                let _ = write!(out, "context #{}", def.index());
            }
        }
        if let Some((dot, string)) = name {
            let raw = &self.ir.strings[string];
            if dot {
                let _ = write!(out, " \".{raw}\"");
            } else {
                let _ = write!(out, " \"{raw}\"");
            }
        }
    }

    fn named_text(&self, named: Named) -> String {
        let mut out = String::new();
        self.named_raw(&mut out, named, self.named.get(&named).copied());
        out
    }

    fn list_text(&self, list: NodeList) -> String {
        let mut out = String::new();
        let mut first = true;
        for &id in &self.ir.lists[list] {
            if !first {
                out.push_str(", ");
            }
            first = false;
            let _ = write!(out, "%{}", id.index());
        }
        out
    }

    fn node_text(&self, node: Node) -> String {
        let mut out = String::new();
        match node {
            Node::Nothing => out.push_str("nothing"),
            Node::Lambda {
                level,
                needs,
                result,
            } => {
                let _ = write!(
                    out,
                    "lambda({level}) need [{}] return %{}",
                    self.list_text(needs),
                    result.index()
                );
            }
            Node::Apply { lambda, args } => {
                let _ = write!(out, "apply %{} to [{}]", lambda.index(), self.list_text(args));
            }
            Node::List { items } => {
                let _ = write!(out, "[{}]", self.list_text(items));
            }
            Node::NeedTydef { level, def, param } => {
                let _ = write!(
                    out,
                    "need({level}) {} via %{}",
                    self.named_text(Named::Tydef(def)),
                    param.index()
                );
            }
            Node::NeedSigdef { level, def, param } => {
                let _ = write!(
                    out,
                    "need({level}) {} via %{}",
                    self.named_text(Named::Sigdef(def)),
                    param.index()
                );
            }
            Node::NeedValdef { level, def, param } => {
                let _ = write!(
                    out,
                    "need({level}) {} via %{}",
                    self.named_text(Named::Valdef(def)),
                    param.index()
                );
            }
            Node::NeedCtxdef { level, def, param } => {
                let _ = write!(
                    out,
                    "need({level}) {} via %{}",
                    self.named_text(Named::Ctxdef(def)),
                    param.index()
                );
            }
            Node::Tagdef { def } => out.push_str(&self.named_text(Named::Tagdef(def))),
            Node::Aliasdef { def } => out.push_str(&self.named_text(Named::Aliasdef(def))),
            Node::Tuple { elems } => {
                let _ = write!(out, "tuple type ({})", self.list_text(elems));
            }
            Node::Context => out.push_str("context type"),
            Node::Fndef { def } => out.push_str(&self.named_text(Named::Fndef(def))),
            Node::Get { ctx, slot } => {
                let _ = write!(out, "slot {} of %{}", slot.index(), ctx.index());
            }
            Node::Lit { val } => match val {
                Val::Uint31(n) => {
                    let _ = write!(out, "uint31 {n}");
                }
                Val::Uint32(n) => {
                    let _ = write!(out, "uint32 {n}");
                }
                Val::Int32(n) => {
                    let _ = write!(out, "int32 {n}");
                }
                Val::Uint63(n) => {
                    let _ = write!(out, "uint63 {n}");
                }
                Val::Uint64(n) => {
                    let _ = write!(out, "uint64 {n}");
                }
                Val::Int64(n) => {
                    let _ = write!(out, "int64 {n}");
                }
                Val::Uint(s) => {
                    let _ = write!(out, "uint {}", &self.ir.strings[s]);
                }
                Val::Int(s) => {
                    let _ = write!(out, "int {}", &self.ir.strings[s]);
                }
                Val::Char(c) => {
                    let _ = write!(out, "c {c:?}");
                }
                Val::String(s) => {
                    let _ = write!(out, "string {s:?}");
                }
            },
            Node::Bind { args, bind } => {
                let _ = write!(
                    out,
                    "bind for [{}] using %{}",
                    self.list_text(args),
                    bind.index()
                );
            }
            Node::BindTydef { def, bind } => {
                let _ = write!(
                    out,
                    "bind {} via %{}",
                    self.named_text(Named::Tydef(def)),
                    bind.index()
                );
            }
            Node::BindSigdef { def, bind } => {
                let _ = write!(
                    out,
                    "bind {} via %{}",
                    self.named_text(Named::Sigdef(def)),
                    bind.index()
                );
            }
            Node::BindValdef { def, bind } => {
                let _ = write!(
                    out,
                    "bind {} via %{}",
                    self.named_text(Named::Valdef(def)),
                    bind.index()
                );
            }
            Node::BindCtxdef { def, bind } => {
                let _ = write!(
                    out,
                    "bind {} via %{}",
                    self.named_text(Named::Ctxdef(def)),
                    bind.index()
                );
            }
            Node::Sig { param, result } => {
                let _ = write!(
                    out,
                    "function signature %{} -> %{}",
                    param.index(),
                    result.index()
                );
            }
        }
        out
    }

    fn node_defs(&self, node: NodeId) -> Vec<String> {
        let mut defs = Vec::new();
        for &named in &self.names.order {
            let def = match named {
                Named::Module(_) => None,
                Named::Tydef(def) => {
                    let Tydef(id) = self.ir.tydefs[def];
                    Some(id)
                }
                Named::Tagdef(def) => {
                    let Tagdef(id) = self.ir.tagdefs[def];
                    Some(id)
                }
                Named::Aliasdef(def) => {
                    let Aliasdef(id) = self.ir.aliasdefs[def];
                    Some(id)
                }
                Named::Sigdef(def) => {
                    let Sigdef(id) = self.ir.sigdefs[def];
                    Some(id)
                }
                Named::Fndef(def) => {
                    let Sigdef(id) = self.ir.fndefs[def];
                    Some(id)
                }
                Named::Valdef(def) => {
                    let Valdef(id) = self.ir.valdefs[def];
                    Some(id)
                }
                Named::Ctxdef(def) => {
                    let Ctxdef(id) = self.ir.ctxdefs[def];
                    Some(id)
                }
            };
            if def == Some(node) {
                defs.push(self.named_text(named));
            }
        }
        defs
    }

    fn node_edges(&self, node: Node) -> Vec<(String, NodeId)> {
        fn indexed(prefix: &str, list: &[NodeId]) -> Vec<(String, NodeId)> {
            list.iter()
                .enumerate()
                .map(|(i, &id)| (format!("{prefix}[{i}]"), id))
                .collect()
        }

        match node {
            Node::Nothing | Node::Tagdef { .. } | Node::Aliasdef { .. } | Node::Context
            | Node::Fndef { .. } | Node::Lit { .. } => Vec::new(),
            Node::Lambda { needs, result, .. } => {
                let mut edges = indexed("need", &self.ir.lists[needs]);
                edges.push(("result".into(), result));
                edges
            }
            Node::Apply { lambda, args } => {
                let mut edges = vec![("lambda".into(), lambda)];
                edges.extend(indexed("arg", &self.ir.lists[args]));
                edges
            }
            Node::List { items } => indexed("item", &self.ir.lists[items]),
            Node::NeedTydef { param, .. }
            | Node::NeedSigdef { param, .. }
            | Node::NeedValdef { param, .. }
            | Node::NeedCtxdef { param, .. } => vec![("param".into(), param)],
            Node::Tuple { elems } => indexed("elem", &self.ir.lists[elems]),
            Node::Get { ctx, .. } => vec![("ctx".into(), ctx)],
            Node::Bind { args, bind } => {
                let mut edges = indexed("arg", &self.ir.lists[args]);
                edges.push(("bind".into(), bind));
                edges
            }
            Node::BindTydef { bind, .. }
            | Node::BindSigdef { bind, .. }
            | Node::BindValdef { bind, .. }
            | Node::BindCtxdef { bind, .. } => vec![("bind".into(), bind)],
            Node::Sig { param, result } => vec![("param".into(), param), ("result".into(), result)],
        }
    }

    fn program(&mut self) {
        self.prepare_names();

        for &named in &self.names.order {
            print!("{}", self.named_text(named));
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
            println!("%{i} = {}", self.node_text(node));
        }
    }
}

fn html(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(ch),
        }
    }
    out
}

fn dot_string(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\l"),
            _ => out.push(ch),
        }
    }
    out
}

fn node_shape(node: Node) -> &'static str {
    match node {
        Node::Nothing => "plaintext",
        Node::Lambda { .. } => "ellipse",
        Node::Apply { .. } => "box",
        Node::List { .. } => "folder",
        Node::NeedTydef { .. }
        | Node::NeedSigdef { .. }
        | Node::NeedValdef { .. }
        | Node::NeedCtxdef { .. } => "box",
        Node::Tagdef { .. } | Node::Aliasdef { .. } | Node::Fndef { .. } => "note",
        Node::Tuple { .. } => "box3d",
        Node::Context => "component",
        Node::Get { .. } => "box",
        Node::Lit { .. } => "note",
        Node::Bind { .. }
        | Node::BindTydef { .. }
        | Node::BindSigdef { .. }
        | Node::BindValdef { .. }
        | Node::BindCtxdef { .. } => "box",
        Node::Sig { .. } => "diamond",
    }
}

fn node_at(ir: &IR, id: NodeId) -> Node {
    ir.nodes[id.index()]
}

fn materialized_lines(
    dump: &Dump<'_>,
    _ir: &IR,
    node: NodeId,
    root_names: &HashMap<NodeId, Vec<&str>>,
) -> Vec<String> {
    let mut lines = vec![format!("%{}", node.index())];
    lines.extend(dump.node_defs(node));
    if let Some(names) = root_names.get(&node) {
        lines.push(format!("roots: {}", names.join(", ")));
    }
    lines
}

fn indent(level: usize) -> String {
    "  ".repeat(level)
}

fn push_block(lines: &mut Vec<String>, block: Vec<String>) {
    lines.extend(block);
}

fn push_item_block(lines: &mut Vec<String>, mut block: Vec<String>) {
    if let Some(last) = block.last_mut() {
        last.push(',');
    }
    lines.extend(block);
}

fn append_to_last(lines: &mut Vec<String>, suffix: &str) {
    if let Some(last) = lines.last_mut() {
        last.push_str(suffix);
    }
}

fn render_ref_pretty(
    dump: &Dump<'_>,
    ir: &IR,
    node: NodeId,
    indent_level: usize,
    materialized: &BTreeSet<NodeId>,
    stack: &mut HashSet<NodeId>,
) -> Vec<String> {
    if materialized.contains(&node) {
        return vec![format!("{}%{}", indent(indent_level), node.index())];
    }

    let inner = render_node_pretty(dump, ir, node, indent_level + 1, materialized, stack);
    if inner.len() == 1 {
        return vec![format!(
            "{}({})",
            indent(indent_level),
            inner[0].trim_start()
        )];
    }

    let mut out = inner;
    out[0] = format!("{}({}", indent(indent_level), out[0].trim_start());
    append_to_last(&mut out, ")");
    out
}

fn render_node_pretty(
    dump: &Dump<'_>,
    ir: &IR,
    node: NodeId,
    indent_level: usize,
    materialized: &BTreeSet<NodeId>,
    stack: &mut HashSet<NodeId>,
) -> Vec<String> {
    if !stack.insert(node) {
        return vec![format!("{}(cycle to %{})", indent(indent_level), node.index())];
    }

    let rendered = match node_at(ir, node) {
        Node::Nothing => vec![format!("{}nothing", indent(indent_level))],
        Node::Lambda {
            level,
            needs,
            result,
        } => {
            let mut lines = vec![format!("{}lambda({level})", indent(indent_level))];
            if ir.lists[needs].is_empty() {
                lines.push(format!("{}need []", indent(indent_level + 1)));
            } else {
                lines.push(format!("{}need [", indent(indent_level + 1)));
                for &need in &ir.lists[needs] {
                    push_item_block(
                        &mut lines,
                        render_ref_pretty(dump, ir, need, indent_level + 2, materialized, stack),
                    );
                }
                lines.push(format!("{}]", indent(indent_level + 1)));
            }
            lines.push(format!("{}return", indent(indent_level + 1)));
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, result, indent_level + 2, materialized, stack),
            );
            lines
        }
        Node::Apply { lambda, args } => {
            let mut lines = vec![format!("{}apply", indent(indent_level))];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, lambda, indent_level + 1, materialized, stack),
            );
            if ir.lists[args].is_empty() {
                lines.push(format!("{}to []", indent(indent_level)));
            } else {
                lines.push(format!("{}to [", indent(indent_level)));
                for &arg in &ir.lists[args] {
                    push_item_block(
                        &mut lines,
                        render_ref_pretty(dump, ir, arg, indent_level + 1, materialized, stack),
                    );
                }
                lines.push(format!("{}]", indent(indent_level)));
            }
            lines
        }
        Node::List { items } => {
            if ir.lists[items].is_empty() {
                vec![format!("{}[]", indent(indent_level))]
            } else {
                let mut lines = vec![format!("{}[", indent(indent_level))];
                for &item in &ir.lists[items] {
                    push_item_block(
                        &mut lines,
                        render_ref_pretty(dump, ir, item, indent_level + 1, materialized, stack),
                    );
                }
                lines.push(format!("{}]", indent(indent_level)));
                lines
            }
        }
        Node::NeedTydef { level, def, param } => {
            let mut lines = vec![format!(
                "{}need({level}) {} via",
                indent(indent_level),
                dump.named_text(Named::Tydef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, param, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::NeedSigdef { level, def, param } => {
            let mut lines = vec![format!(
                "{}need({level}) {} via",
                indent(indent_level),
                dump.named_text(Named::Sigdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, param, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::NeedValdef { level, def, param } => {
            let mut lines = vec![format!(
                "{}need({level}) {} via",
                indent(indent_level),
                dump.named_text(Named::Valdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, param, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::NeedCtxdef { level, def, param } => {
            let mut lines = vec![format!(
                "{}need({level}) {} via",
                indent(indent_level),
                dump.named_text(Named::Ctxdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, param, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::Tagdef { def } => vec![format!(
            "{}{}",
            indent(indent_level),
            dump.named_text(Named::Tagdef(def))
        )],
        Node::Aliasdef { def } => vec![format!(
            "{}{}",
            indent(indent_level),
            dump.named_text(Named::Aliasdef(def))
        )],
        Node::Tuple { elems } => {
            if ir.lists[elems].is_empty() {
                vec![format!("{}tuple type ()", indent(indent_level))]
            } else {
                let mut lines = vec![format!("{}tuple type (", indent(indent_level))];
                for &elem in &ir.lists[elems] {
                    push_item_block(
                        &mut lines,
                        render_ref_pretty(dump, ir, elem, indent_level + 1, materialized, stack),
                    );
                }
                lines.push(format!("{})", indent(indent_level)));
                lines
            }
        }
        Node::Context => vec![format!("{}context type", indent(indent_level))],
        Node::Fndef { def } => vec![format!(
            "{}{}",
            indent(indent_level),
            dump.named_text(Named::Fndef(def))
        )],
        Node::Get { ctx, slot } => {
            let mut lines = vec![format!("{}slot {} of", indent(indent_level), slot.index())];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, ctx, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::Lit { val } => vec![format!(
            "{}{}",
            indent(indent_level),
            dump.node_text(Node::Lit { val })
        )],
        Node::Bind { args, bind } => {
            let mut lines = if ir.lists[args].is_empty() {
                vec![format!("{}bind for [] using", indent(indent_level))]
            } else {
                vec![format!("{}bind for [", indent(indent_level))]
            };
            for &arg in &ir.lists[args] {
                push_item_block(
                    &mut lines,
                    render_ref_pretty(dump, ir, arg, indent_level + 1, materialized, stack),
                );
            }
            if !ir.lists[args].is_empty() {
                lines.push(format!("{}] using", indent(indent_level)));
            }
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, bind, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::BindTydef { def, bind } => {
            let mut lines = vec![format!(
                "{}bind {} via",
                indent(indent_level),
                dump.named_text(Named::Tydef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, bind, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::BindSigdef { def, bind } => {
            let mut lines = vec![format!(
                "{}bind {} via",
                indent(indent_level),
                dump.named_text(Named::Sigdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, bind, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::BindValdef { def, bind } => {
            let mut lines = vec![format!(
                "{}bind {} via",
                indent(indent_level),
                dump.named_text(Named::Valdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, bind, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::BindCtxdef { def, bind } => {
            let mut lines = vec![format!(
                "{}bind {} via",
                indent(indent_level),
                dump.named_text(Named::Ctxdef(def))
            )];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, bind, indent_level + 1, materialized, stack),
            );
            lines
        }
        Node::Sig { param, result } => {
            let mut lines = vec![format!("{}function signature", indent(indent_level))];
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, param, indent_level + 1, materialized, stack),
            );
            lines.push(format!("{}->", indent(indent_level)));
            push_block(
                &mut lines,
                render_ref_pretty(dump, ir, result, indent_level + 1, materialized, stack),
            );
            lines
        }
    };
    stack.remove(&node);
    rendered
}

fn materialized_edges(
    dump: &Dump<'_>,
    ir: &IR,
    node: NodeId,
    materialized: &BTreeSet<NodeId>,
) -> Vec<NodeId> {
    fn collect(
        dump: &Dump<'_>,
        ir: &IR,
        node: NodeId,
        materialized: &BTreeSet<NodeId>,
        seen: &mut HashSet<NodeId>,
        out: &mut BTreeSet<NodeId>,
    ) {
        if !seen.insert(node) {
            return;
        }
        for (_, next) in dump.node_edges(node_at(ir, node)) {
            if materialized.contains(&next) {
                out.insert(next);
            } else {
                collect(dump, ir, next, materialized, seen, out);
            }
        }
        seen.remove(&node);
    }

    let mut out = BTreeSet::new();
    collect(
        dump,
        ir,
        node,
        materialized,
        &mut HashSet::new(),
        &mut out,
    );
    out.into_iter().collect()
}

fn materialized_label(
    dump: &Dump<'_>,
    ir: &IR,
    node: NodeId,
    materialized: &BTreeSet<NodeId>,
    root_names: &HashMap<NodeId, Vec<&str>>,
) -> String {
    let mut lines = materialized_lines(dump, ir, node, root_names);
    let mut stack = HashSet::new();
    lines.extend(render_node_pretty(
        dump,
        ir,
        node,
        0,
        materialized,
        &mut stack,
    ));
    lines.join("\n")
}

pub fn dump(ir: &IR, names: &Names) {
    Dump {
        ir,
        names,
        named: HashMap::new(),
    }
    .program();
}

struct FocusDump<'a> {
    dump: Dump<'a>,
    root_names: HashMap<NodeId, Vec<&'a str>>,
    materialized: BTreeSet<NodeId>,
}

fn build_focus_dump<'a>(ir: &'a IR, names: &'a Names, roots: &'a [(String, NodeId)]) -> FocusDump<'a> {
    let mut dump = Dump {
        ir,
        names,
        named: HashMap::new(),
    };
    dump.prepare_names();

    let mut root_names: HashMap<NodeId, Vec<&str>> = HashMap::new();
    let mut queue = VecDeque::new();
    for (name, node) in roots {
        root_names.entry(*node).or_default().push(name);
        queue.push_back(*node);
    }

    let mut keep = BTreeSet::new();
    while let Some(node) = queue.pop_front() {
        if !keep.insert(node) {
            continue;
        }
        for (_, next) in dump.node_edges(node_at(ir, node)) {
            queue.push_back(next);
        }
    }

    let mut incoming = HashMap::<NodeId, usize>::new();
    for &node in &keep {
        for (_, next) in dump.node_edges(node_at(ir, node)) {
            if keep.contains(&next) {
                *incoming.entry(next).or_default() += 1;
            }
        }
    }

    let mut materialized = BTreeSet::new();
    for &node in &keep {
        if root_names.contains_key(&node) || incoming.get(&node).copied().unwrap_or(0) != 1 {
            materialized.insert(node);
        }
    }

    FocusDump {
        dump,
        root_names,
        materialized,
    }
}

pub fn dump_focus_text(ir: &IR, names: &Names, roots: &[(String, NodeId)]) -> Vec<(NodeId, String)> {
    let focus = build_focus_dump(ir, names, roots);
    let mut out = Vec::new();

    for &node in &focus.materialized {
        let lines = materialized_lines(&focus.dump, ir, node, &focus.root_names);
        let mut stack = HashSet::new();
        let mut lines = lines;
        lines.extend(render_node_pretty(
            &focus.dump,
            ir,
            node,
            0,
            &focus.materialized,
            &mut stack,
        ));
        out.push((node, lines.join("\n") + "\n"));
    }

    out
}

pub fn dump_dot_focus(ir: &IR, names: &Names, roots: &[(String, NodeId)]) -> String {
    let focus = build_focus_dump(ir, names, roots);

    let mut out = String::new();
    out.push_str("digraph IR {\n");
    out.push_str("  graph [rankdir=LR, splines=true, overlap=false, fontname=\"Menlo\"];\n");
    out.push_str(
        "  node [shape=box, style=\"rounded,filled\", fillcolor=\"#f8f8f8\", color=\"#444444\", fontname=\"Menlo\", fontsize=10];\n",
    );
    out.push_str("  edge [color=\"#666666\"];\n");

    for node in &focus.materialized {
        let node = *node;
        let label = materialized_label(&focus.dump, ir, node, &focus.materialized, &focus.root_names);
        let fill = match focus.root_names.get(&node) {
            Some(names) if names.iter().any(|name| *name == "substituted") => "#d9ead3",
            Some(names) if names.iter().any(|name| *name == "target") => "#cfe2f3",
            Some(names) if names.iter().any(|name| *name == "param") => "#f4cccc",
            Some(_) => "#fff2cc",
            None => "#f8f8f8",
        };
        let _ = writeln!(
            out,
            "  n{} [shape={}, fillcolor=\"{}\", label=\"{}\\l\"];",
            node.index(),
            node_shape(node_at(ir, node)),
            fill,
            dot_string(&label)
        );
    }

    for node in &focus.materialized {
        let node = *node;
        for next in materialized_edges(&focus.dump, ir, node, &focus.materialized) {
            let _ = writeln!(out, "  n{} -> n{};", node.index(), next.index());
        }
    }

    out.push_str("}\n");
    out
}
