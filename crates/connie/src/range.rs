use crate::{
    lex::TokenId,
    parse::{Expr, ExprId, Path, Tree},
};

pub struct Inclusive {
    pub first: TokenId,
    pub last: TokenId,
}

struct Ranger<'a> {
    tree: &'a Tree,
}

impl Ranger<'_> {
    fn path(&self, path: Path) -> Inclusive {
        let first = path.name;
        let last = match path.names.last() {
            None => path.name,
            Some(name) => self.tree.names[name],
        };
        Inclusive { first, last }
    }

    fn expr(&self, id: ExprId) -> Inclusive {
        match self.tree.exprs[id] {
            Expr::Path(path) => self.path(path),
            Expr::String(token) => Inclusive {
                first: token,
                last: token,
            },
            Expr::Call(callee, args) => {
                let Inclusive { first, last } = self.expr(callee);
                let last = match args.last() {
                    None => last + 2,
                    Some(arg) => self.expr(arg).last + 1,
                };
                Inclusive { first, last }
            }
        }
    }
}

pub fn expr_range(tree: &Tree, id: ExprId) -> Inclusive {
    Ranger { tree }.expr(id)
}
