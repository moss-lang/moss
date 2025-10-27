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
            Expr::Int(token) | Expr::String(token) => Inclusive {
                first: token,
                last: token,
            },
            Expr::Field(object, field) => Inclusive {
                first: self.expr(object).first,
                last: field,
            },
            Expr::Method(object, method, args) => Inclusive {
                first: self.expr(object).first,
                last: match args.last() {
                    None => method + 2,
                    Some(arg) => self.expr(arg).last + 1,
                },
            },
            Expr::Call(callee, args) => {
                let Inclusive { first, last } = self.expr(callee);
                let last = match args.last() {
                    None => last + 2,
                    Some(arg) => self.expr(arg).last + 1,
                };
                Inclusive { first, last }
            }
            Expr::Binary(lhs, _, rhs) => Inclusive {
                first: self.expr(lhs).first,
                last: self.expr(rhs).last,
            },
        }
    }
}

pub fn expr_range(tree: &Tree, id: ExprId) -> Inclusive {
    Ranger { tree }.expr(id)
}
