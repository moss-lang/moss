use std::mem::{replace, take};

use index_vec::{IndexVec, define_index_type};
use indexmap::IndexMap;
use logos::Logos;

use crate::{
    intern::{StrId, Strings},
    lex::{Token, TokenId, TokenStarts},
    parse::{self, Ctx, Expr, ExprId, Region, RegionId, Stmt, StmtId, Tree, Val},
    util::IdRange,
};

define_index_type! {
    pub struct PathId = u32;
}

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct VarId = u32;
}

define_index_type! {
    pub struct InstrId = u32;
}

define_index_type! {
    pub struct FuncId = u32;
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Unit,
    String,
}

#[derive(Clone, Copy, Debug)]
pub struct Var {
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub enum Instr {
    String(StrId),
    Get(VarId),
    Provide(VarId, InstrId, IdRange<InstrId>),
    Println(InstrId),
}

#[derive(Clone, Copy, Debug)]
pub struct Func {
    pub param: TypeId,
    pub result: TypeId,
    pub body: IdRange<InstrId>,
}

#[derive(Debug, Default)]
pub struct IR {
    pub strings: Strings,
    pub types: IndexVec<TypeId, Type>,
    pub vars: IndexVec<VarId, Var>,
    pub vals: IndexVec<InstrId, TypeId>,
    pub instrs: IndexVec<InstrId, Instr>,
    pub funcs: IndexVec<FuncId, Func>,
    pub main: Option<FuncId>,
}

type Path = Option<(PathId, StrId)>;

#[derive(Clone, Copy, Debug)]
enum Named {
    Scope,
    Type(TypeId),
    Var(VarId),
    Val(InstrId),
    Func(FuncId),
}

impl Named {
    fn ty(self) -> TypeId {
        match self {
            Self::Type(id) => id,
            _ => panic!(),
        }
    }

    fn var(self) -> VarId {
        match self {
            Self::Var(id) => id,
            _ => panic!(),
        }
    }

    fn val(self) -> InstrId {
        match self {
            Self::Val(id) => id,
            _ => panic!(),
        }
    }

    fn func(self) -> FuncId {
        match self {
            Self::Func(id) => id,
            _ => panic!(),
        }
    }
}

pub enum LowerError {}

type LowerResult<T> = Result<T, LowerError>;

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: IR,
    paths: IndexMap<Path, Named>,
    path: PathId,
    funcs: IndexVec<FuncId, (PathId, parse::FuncId)>,
    instrs: Vec<Instr>,
}

impl<'a> Lower<'a> {
    fn slice(&self, token: TokenId) -> &'a str {
        let start = self.starts[token].index();
        let (_, range) = Token::lexer(&self.source[start..])
            .spanned()
            .next()
            .unwrap();
        &self.source[(range.start + start)..(range.end + start)]
    }

    fn string(&mut self, token: TokenId) -> StrId {
        self.ir.strings.make(self.slice(token))
    }

    fn get_path_of(&self, token: TokenId) -> PathId {
        let mut parent = self.path;
        let string = self.ir.strings.get(self.slice(token)).unwrap();
        loop {
            match self.paths.get_index_of(&Some((parent, string))) {
                Some(i) => return PathId::from_usize(i),
                None => {
                    let (parts, _) = self.paths.get_index(parent.index()).unwrap();
                    let (grandparent, _) = parts.unwrap();
                    parent = grandparent;
                }
            }
        }
    }

    fn get_path(&mut self, path: parse::Path) -> Named {
        let mut id = self.get_path_of(path.name);
        let Named::Scope = self.paths[id.index()] else {
            panic!();
        };
        for name in path.names {
            let string = self.string(self.tree.names[name]);
            id = PathId::from_usize(self.paths.get_index_of(&Some((id, string))).unwrap());
        }
        self.paths[id.index()]
    }

    fn get(&self, token: TokenId) -> Named {
        self.paths[self.get_path_of(token).index()]
    }

    fn set_name(&mut self, name: StrId, named: Named) -> PathId {
        let (i, _) = self.paths.insert_full(Some((self.path, name)), named);
        PathId::from_usize(i)
    }

    fn set_token(&mut self, token: TokenId, named: Named) -> PathId {
        let name = self.string(token);
        self.set_name(name, named)
    }

    fn set_scope(&mut self, token: TokenId) -> PathId {
        self.set_token(token, Named::Scope)
    }

    fn set_var(&mut self, token: TokenId, id: VarId) -> PathId {
        self.set_token(token, Named::Var(id))
    }

    fn set_func(&mut self, token: TokenId, id: FuncId) -> PathId {
        self.set_token(token, Named::Func(id))
    }

    fn expr(&mut self, expr: ExprId) -> InstrId {
        match self.tree.exprs[expr] {
            Expr::Path(path) => match self.get_path(path) {
                Named::Scope => panic!(),
                Named::Var(var) => todo!(),
                Named::Type(_) => panic!(),
                Named::Val(val) => todo!(),
                Named::Func(func) => todo!(),
            },
            Expr::String(token) => todo!(),
            Expr::Call(callee, args) => todo!(),
        }
    }

    fn region(&mut self, region: RegionId) {
        let Region {
            ctxs,
            needs,
            funcs,
            regions,
        } = self.tree.regions[region];
        for ctx in ctxs {
            let Ctx { name, vals } = self.tree.ctxs[ctx];
            let parent = self.path;
            self.path = self.set_scope(name);
            for val in vals {
                let Val { name, ty } = self.tree.vals[val];
                let ty = match self.tree.types[ty] {
                    parse::Type::Name(name) => self.get(name).ty(),
                };
                let id = self.ir.vars.push(Var { ty });
                self.set_var(name, id);
            }
            self.path = parent;
        }
        for need in needs {
            let _ = need; // TODO
        }
        for func in funcs {
            let id = self.funcs.push((self.path, func));
            self.set_func(self.tree.funcs[func].name, id);
        }
        for child in regions {
            self.region(child);
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) {
        for stmt in stmts {
            match self.tree.stmts[stmt] {
                Stmt::Provide(path, expr) => {
                    let var = self.get_path(path).var();
                    let val = self.expr(expr);
                    let outer = take(&mut self.instrs);
                    self.stmts(IdRange {
                        start: StmtId::from_raw(stmt.raw() + 1),
                        ..stmts
                    });
                    let inner = IdRange::new(&mut self.ir.instrs, replace(&mut self.instrs, outer));
                    self.instrs.push(Instr::Provide(var, val, inner));
                    break;
                }
                Stmt::Expr(expr) => {
                    self.expr(expr);
                }
            }
        }
    }

    fn func(&mut self, func: parse::FuncId) -> Func {
        let body = self.tree.funcs[func].body;
        let param = self.ir.types.push(Type::Unit);
        let result = self.ir.types.push(Type::Unit);
        self.stmts(body.stmts);
        assert!(body.expr.is_none()); // TODO
        Func {
            param,
            result,
            body: IdRange::new(&mut self.ir.instrs, take(&mut self.instrs)),
        }
    }

    fn program(mut self) -> LowerResult<IR> {
        {
            let name = self.ir.strings.make("String");
            let ty = self.ir.types.push(Type::String);
            self.set_name(name, Named::Type(ty));
        }
        self.region(self.tree.root);
        for (scope, func) in take(&mut self.funcs) {
            self.path = scope;
            self.func(func);
        }
        Ok(self.ir)
    }
}

pub fn lower(source: &str, starts: &TokenStarts, tree: &Tree) -> LowerResult<IR> {
    let mut paths = IndexMap::new();
    let (i, _) = paths.insert_full(None, Named::Scope);
    Lower {
        source,
        starts,
        tree,
        ir: IR::default(),
        paths,
        path: PathId::from_usize(i),
        funcs: IndexVec::new(),
        instrs: Vec::new(),
    }
    .program()
}
