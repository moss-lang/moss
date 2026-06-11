use crate::{
    lex::TokenId,
    parse::{
        Bind, BindId, Binding, BindingId, Block, Entry, Expr, ExprId, Path, Spec, Stmt, StmtId,
        Tree,
    },
};

pub struct Inclusive {
    pub first: TokenId,
    pub last: TokenId,
}

pub fn single(token: TokenId) -> Inclusive {
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

    fn spec(&self, spec: Spec) -> Inclusive {
        let Spec { dot, path, binds } = spec;
        let Inclusive { first, last } = self.path(path);
        Inclusive {
            first: if dot { first - 1 } else { first },
            last: match binds.last() {
                None => last,
                Some(bind) => self.bind(bind).last + 1, // TODO: Handle optional trailing comma.
            },
        }
    }

    fn entry(&self, entry: Entry) -> Inclusive {
        match entry {
            Entry::Lit(token) => single(token),
            Entry::Ref(spec) => self.spec(spec),
        }
    }

    fn bind(&self, id: BindId) -> Inclusive {
        let Bind { key, val } = self.tree.binds[id];
        let Inclusive { first, last } = self.spec(key);
        Inclusive {
            first,
            last: match val {
                None => last,
                Some(entry) => self.entry(entry).last,
            },
        }
    }

    fn binding(&self, id: BindingId) -> Inclusive {
        match self.tree.bindings[id] {
            Binding::Single(bind) => self.bind(bind),
            Binding::Composite(expr) => self.expr(expr),
        }
    }

    fn expr(&self, id: ExprId) -> Inclusive {
        match self.tree.exprs[id] {
            Expr::Lit(token) => single(token),
            Expr::Path(path) => self.path(path),
            Expr::Tag(path, inner) => Inclusive {
                first: self.path(path).first,
                last: self.expr(inner).last,
            },
            Expr::Record(lbrace, _, rbrace) => Inclusive {
                first: lbrace,
                last: rbrace,
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
            Expr::Call(callee, _, args) => {
                let Inclusive { first, last } = self.path(callee);
                let last = match args.last() {
                    None => last + 2,
                    Some(arg) => self.expr(arg).last + 1,
                };
                Inclusive { first, last }
            }
            Expr::Unary(_, inner) => {
                let Inclusive { first, last } = self.expr(inner);
                Inclusive {
                    first: first - 1,
                    last,
                }
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
            Expr::Bind(start, bindings) => {
                let first = start;
                Inclusive {
                    first,
                    last: match bindings.last() {
                        None => first + 1,
                        Some(binding) => self.binding(binding).last,
                    },
                }
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

pub fn expr_range(tree: &Tree, id: ExprId) -> Inclusive {
    Ranger { tree }.expr(id)
}
