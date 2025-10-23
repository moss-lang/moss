use crate::{
    lex::TokenId,
    parse::{Expr, ExprId, Path, Tree},
    util::IdRange,
};

fn range(start: TokenId, end: TokenId) -> IdRange<TokenId> {
    IdRange { start, end }
}

struct Ranger<'a> {
    tree: &'a Tree,
}

impl Ranger<'_> {
    fn path(&self, path: Path) -> IdRange<TokenId> {
        range(
            path.name,
            if path.names.is_empty() {
                path.name
            } else {
                self.tree.names[path.names.end - 1]
            },
        )
    }

    fn expr(&self, id: ExprId) -> IdRange<TokenId> {
        match self.tree.exprs[id] {
            Expr::Path(path) => self.path(path),
            Expr::String(token) => range(token, token),
            Expr::Call(callee, args) => {
                let start = self.expr(callee).start;
                let end = if args.is_empty() {
                    start + 3
                } else {
                    self.expr(args.end).end
                };
                range(start, end)
            }
        }
    }
}

pub fn expr_range(tree: &Tree, id: ExprId) -> IdRange<TokenId> {
    Ranger { tree }.expr(id)
}
