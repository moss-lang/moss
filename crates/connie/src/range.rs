use crate::{
    lex::TokenId,
    parse::{Block, Expr, ExprId, Path, Stmt, StmtId, Tree},
};

pub struct Inclusive {
    pub first: TokenId,
    pub last: TokenId,
}

fn single(token: TokenId) -> Inclusive {
    Inclusive {
        first: token,
        last: token,
    }
}

struct Ranger<'a> {
    tree: &'a Tree,
}

impl Ranger<'_> {
    fn path(&self, path: Path) -> Inclusive {
        let first = match path.prefix.first() {
            Some(name) => self.tree.names[name],
            None => path.last,
        };
        let last = path.last;
        Inclusive { first, last }
    }

    fn expr(&self, id: ExprId) -> Inclusive {
        match self.tree.exprs[id] {
            Expr::This(token) => single(token),
            Expr::Path(path) => self.path(path),
            Expr::Int(token) => single(token),
            Expr::String(token) => single(token),
            Expr::Struct(path, fields) => {
                let first = self.path(path).first;
                let last = match fields.last() {
                    None => first + 2,
                    Some(field) => self.expr(self.tree.fields[field].val).last + 1,
                };
                Inclusive { first, last }
            }
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
            Expr::Call(callee, _, args) => {
                let Inclusive { first, last } = self.path(callee);
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
            Expr::If(cond, yes, no) => {
                let range = self.expr(cond);
                let first = range.first - 1;
                let last = if let Some(block) = no
                    && let Some(range) = self.block(block)
                {
                    range.last
                } else if let Some(range) = self.block(yes) {
                    range.last
                } else {
                    range.last + 2
                };
                Inclusive { first, last }
            }
        }
    }

    fn stmt(&self, id: StmtId) -> Inclusive {
        match self.tree.stmts[id] {
            Stmt::Let(name, rhs) => {
                let first = name;
                let last = self.expr(rhs).last + 1;
                Inclusive { first, last }
            }
            Stmt::Var(name, rhs) => {
                let first = name;
                let last = self.expr(rhs).last + 1;
                Inclusive { first, last }
            }
            Stmt::Assign(lhs, rhs) => {
                let first = self.expr(lhs).first;
                let last = self.expr(rhs).last + 1;
                Inclusive { first, last }
            }
            Stmt::While(cond, block) => match self.block(block) {
                Some(range) => Inclusive {
                    first: self.expr(cond).first,
                    last: range.last,
                },
                None => {
                    let range = self.expr(cond);
                    Inclusive {
                        first: range.first,
                        last: range.last + 2,
                    }
                }
            },
            Stmt::Expr(expr) => {
                let range = self.expr(expr);
                Inclusive {
                    first: range.first,
                    last: range.last + 1,
                }
            }
        }
    }

    fn block(&self, block: Block) -> Option<Inclusive> {
        match (block.stmts.first(), block.stmts.last(), block.expr) {
            (Some(_), None, _) | (None, Some(_), _) => unreachable!(),
            (None, None, None) => None,
            (Some(stmt), Some(_), Some(expr)) => Some(Inclusive {
                first: self.stmt(stmt).first - 1,
                last: self.expr(expr).last + 1,
            }),
            (Some(first), Some(last), None) if first == last => {
                let range = self.stmt(first);
                Some(Inclusive {
                    first: range.first - 1,
                    last: range.last + 1,
                })
            }
            (Some(first), Some(last), None) => Some(Inclusive {
                first: self.stmt(first).first - 1,
                last: self.stmt(last).last + 1,
            }),
            (None, None, Some(expr)) => {
                let range = self.expr(expr);
                Some(Inclusive {
                    first: range.first - 1,
                    last: range.last - 1,
                })
            }
        }
    }
}

pub fn path_range(tree: &Tree, path: Path) -> Inclusive {
    Ranger { tree }.path(path)
}

pub fn expr_range(tree: &Tree, id: ExprId) -> Inclusive {
    Ranger { tree }.expr(id)
}
