use std::mem::take;

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::{IndexMap, IndexSet};

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex},
    parse::{self, Binop, Ctx, Expr, ExprId, Region, RegionId, Stmt, StmtId, Tree, Val},
    range::{Inclusive, expr_range},
    tuples::{TupleRange, Tuples},
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
    pub struct FuncId = u32;
}

define_index_type! {
    pub struct InstrId = u32;
}

define_index_type! {
    pub struct RefId = u32;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Bool,
    Int,
    String,
    Tuple(TupleRange),
    List(TypeId),
}

impl Type {
    pub fn tuple(self) -> TupleRange {
        match self {
            Type::Tuple(range) => range,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Var {
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Depth(pub u32);

#[derive(Clone, Copy, Debug)]
pub struct Index(pub u32);

#[derive(Clone, Copy, Debug)]
pub enum IntArith {
    Add,
    Sub,
}

#[derive(Clone, Copy, Debug)]
pub enum IntComp {
    Less,
}

#[derive(Clone, Copy, Debug)]
pub enum Instr {
    Int(i32),
    String(StrId),
    Tuple(IdRange<RefId>),
    Elem(InstrId, Index),
    IntArith(InstrId, IntArith, InstrId),
    IntComp(InstrId, IntComp, InstrId),
    Len(InstrId),
    Index(InstrId, InstrId),
    Set(InstrId, InstrId),
    Param,
    Get(VarId),
    Call(FuncId, InstrId),
    Bind(VarId, InstrId),
    If(InstrId),
    Loop,
    Br(Depth),
    End,
    Return(InstrId),
    Args,
    Println(InstrId),
}

#[derive(Clone, Copy, Debug)]
pub struct Func {
    pub param: TypeId,
    pub result: TypeId,
}

#[derive(Debug, Default)]
pub struct IR {
    pub strings: Strings,
    pub types: IndexSet<Type>,
    pub tuples: Tuples<TypeId>,
    pub vars: IndexVec<VarId, Var>,
    pub funcs: IndexVec<FuncId, Func>,
    pub main: Option<FuncId>,
    pub vals: IndexVec<InstrId, TypeId>,
    pub instrs: IndexVec<InstrId, Instr>,
    pub refs: IndexVec<RefId, InstrId>,
    pub bodies: IndexVec<FuncId, InstrId>,
}

type Path = Option<(PathId, StrId)>;

#[derive(Clone, Copy, Debug)]
enum Named {
    Scope,
    Type(TypeId),
    Var(VarId),
    Func(FuncId),
    Val(InstrId),
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

    fn func(self) -> FuncId {
        match self {
            Self::Func(id) => id,
            _ => panic!(),
        }
    }

    fn val(self) -> InstrId {
        match self {
            Self::Val(id) => id,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LowerError {
    NoMain,
    ArgCount(parse::ExprId),
}

impl LowerError {
    pub fn describe(self, _: &str, _: &TokenStarts, tree: &Tree) -> (Option<Inclusive>, String) {
        match self {
            LowerError::NoMain => (None, "no `main` function".to_owned()),
            LowerError::ArgCount(expr) => (
                Some(expr_range(tree, expr)),
                "wrong number of arguments".to_owned(),
            ),
        }
    }
}

type LowerResult<T> = Result<T, LowerError>;

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: IR,
    paths: IndexMap<Path, Named>,
    path: PathId,
    funcs: Vec<(PathId, parse::FuncId, FuncId)>,
}

impl<'a> Lower<'a> {
    fn slice(&self, token: TokenId) -> &'a str {
        &self.source[relex(self.source, self.starts, token)]
    }

    fn name(&mut self, token: TokenId) -> StrId {
        self.ir.strings.make_id(self.slice(token))
    }

    fn string(&mut self, token: TokenId) -> StrId {
        let quoted = self.slice(token);
        self.ir.strings.make_id(&quoted[1..quoted.len() - 1])
    }

    fn ty(&mut self, ty: Type) -> TypeId {
        let (i, _) = self.ir.types.insert_full(ty);
        TypeId::from_usize(i)
    }

    fn ty_tuple(&mut self, elems: &[TypeId]) -> TypeId {
        let tuple = self.ir.tuples.make(elems);
        self.ty(Type::Tuple(tuple))
    }

    fn ty_unit(&mut self) -> TypeId {
        self.ty_tuple(&[])
    }

    fn get_path_of(&self, token: TokenId) -> PathId {
        let mut parent = self.path;
        let string = self.ir.strings.get_id(self.slice(token)).unwrap();
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
        for name in path.names {
            let Named::Scope = self.paths[id.index()] else {
                panic!();
            };
            let string = self.name(self.tree.names[name]);
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
        let name = self.name(token);
        self.set_name(name, named)
    }

    fn set_scope(&mut self, token: TokenId) -> PathId {
        self.set_token(token, Named::Scope)
    }

    fn set_var(&mut self, token: TokenId, id: VarId) -> PathId {
        self.set_token(token, Named::Var(id))
    }

    fn set_val(&mut self, token: TokenId, id: InstrId) -> PathId {
        self.set_token(token, Named::Val(id))
    }

    fn parse_ty(&mut self, ty: parse::TypeId) -> TypeId {
        match self.tree.types[ty] {
            parse::Type::Name(name) => self.get(name).ty(),
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
                let ty = self.parse_ty(ty);
                let id = self.ir.vars.push(Var { ty });
                self.set_var(name, id);
            }
            self.path = parent;
        }
        for need in needs {
            let _ = need; // TODO
        }
        for func in funcs {
            let parse::Func {
                name,
                params,
                body: _,
            } = self.tree.funcs[func];
            let types = params
                .into_iter()
                .map(|param| self.parse_ty(self.tree.params[param].ty))
                .collect::<Vec<TypeId>>();
            let param = self.ty_tuple(&types);
            let result = self.ty_unit();
            let id = self.ir.funcs.push(Func { param, result });
            self.funcs.push((self.path, func, id));
            let name = self.name(name);
            self.set_name(name, Named::Func(id));
            if &self.ir.strings[name] == "main" {
                self.ir.main = Some(id);
            }
        }
        for child in regions {
            self.region(child);
        }
    }

    fn instr(&mut self, ty: TypeId, instr: Instr) -> InstrId {
        let id_val = self.ir.vals.push(ty);
        let id_instr = self.ir.instrs.push(instr);
        assert_eq!(id_val, id_instr);
        id_val
    }

    fn instr_tuple(&mut self, ty: TypeId, vals: &[InstrId]) -> InstrId {
        let start = self.ir.refs.len_idx();
        self.ir.refs.extend_from_slice(IndexSlice::new(vals));
        let end = self.ir.refs.len_idx();
        self.instr(ty, Instr::Tuple(IdRange { start, end }))
    }

    fn ret(&mut self, val: InstrId) -> InstrId {
        let ty = self.ty_unit(); // This is a bit meaningless for a `Return` instruction.
        self.instr(ty, Instr::Return(val))
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<InstrId> {
        match self.tree.exprs[expr] {
            Expr::Path(path) => match self.get_path(path) {
                Named::Scope => panic!(),
                Named::Type(_) => panic!(),
                Named::Var(var) => {
                    let ty = self.ir.vars[var].ty;
                    Ok(self.instr(ty, Instr::Get(var)))
                }
                Named::Func(_) => panic!(),
                Named::Val(val) => Ok(val),
            },
            Expr::Int(token) => {
                let ty = self.ty(Type::Int);
                let n = self.slice(token).parse().unwrap();
                Ok(self.instr(ty, Instr::Int(n)))
            }
            Expr::String(token) => {
                let ty = self.ty(Type::String);
                let string = self.string(token);
                Ok(self.instr(ty, Instr::String(string)))
            }
            Expr::Field(_, _) => todo!(),
            Expr::Method(object, method, args) => {
                let val = self.expr(object)?;
                match (self.ir.types[self.ir.vals[val].index()], self.slice(method)) {
                    (Type::List(_), "len") => {
                        assert!(args.is_empty());
                        let ty = self.ty(Type::Int);
                        Ok(self.instr(ty, Instr::Len(val)))
                    }
                    (Type::List(inner), "get") => {
                        assert_eq!(args.len(), 1);
                        let index = self.expr(args.first().unwrap())?;
                        Ok(self.instr(inner, Instr::Index(val, index)))
                    }
                    _ => panic!(),
                }
            }
            Expr::Call(callee, args) => match self.tree.exprs[callee] {
                Expr::Path(path) => {
                    let func = self.get_path(path).func();
                    let Func { param, result, .. } = self.ir.funcs[func];
                    let params = &self.ir.tuples[self.ir.types[param.index()].tuple()];
                    if args.len() != params.len() {
                        return Err(LowerError::ArgCount(expr));
                    }
                    let vals = args
                        .into_iter()
                        .map(|arg| self.expr(arg))
                        .collect::<LowerResult<Vec<InstrId>>>()?;
                    let arg = self.instr_tuple(param, &vals);
                    Ok(self.instr(result, Instr::Call(func, arg)))
                }
                _ => panic!(),
            },
            Expr::Binary(l, op, r) => {
                let a = self.expr(l)?;
                let b = self.expr(r)?;
                Ok(match op {
                    Binop::Add => {
                        let ty = self.ty(Type::Int);
                        self.instr(ty, Instr::IntArith(a, IntArith::Add, b))
                    }
                    Binop::Sub => {
                        let ty = self.ty(Type::Int);
                        self.instr(ty, Instr::IntArith(a, IntArith::Sub, b))
                    }
                    Binop::Less => {
                        let ty = self.ty(Type::Bool);
                        self.instr(ty, Instr::IntComp(a, IntComp::Less, b))
                    }
                })
            }
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) -> LowerResult<()> {
        for stmt in stmts {
            match self.tree.stmts[stmt] {
                Stmt::Let(name, rhs) => {
                    let val = self.expr(rhs)?;
                    self.set_val(name, val);
                }
                Stmt::Var(name, rhs) => {
                    let val = self.expr(rhs)?;
                    self.set_val(name, val);
                }
                Stmt::Assign(lhs, rhs) => match self.tree.exprs[lhs] {
                    Expr::Path(path) => {
                        assert!(path.names.is_empty());
                        let before = self.get(path.name).val();
                        let after = self.expr(rhs)?;
                        let unit = self.ty_unit();
                        self.instr(unit, Instr::Set(before, after));
                    }
                    _ => panic!(),
                },
                Stmt::Provide(path, expr) => {
                    let ty = self.ty_unit();
                    let var = self.get_path(path).var();
                    let val = self.expr(expr)?;
                    self.instr(ty, Instr::Bind(var, val));
                    self.stmts(IdRange {
                        start: StmtId::from_raw(stmt.raw() + 1),
                        ..stmts
                    })?;
                    let unit = self.ty_unit();
                    self.instr(unit, Instr::End);
                    break;
                }
                Stmt::While(cond, body) => {
                    assert!(body.expr.is_none());
                    let unit = self.ty_unit();
                    self.instr(unit, Instr::Loop);
                    let val = self.expr(cond)?;
                    self.instr(unit, Instr::If(val));
                    self.stmts(body.stmts)?;
                    self.instr(unit, Instr::Br(Depth(1)));
                    self.instr(unit, Instr::End);
                    self.instr(unit, Instr::End);
                }
                Stmt::Expr(expr) => {
                    self.expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn body(&mut self, id: FuncId, func: parse::FuncId) -> LowerResult<InstrId> {
        let start = self.ir.instrs.len_idx();
        let parse::Func {
            name: _,
            params,
            body,
        } = self.tree.funcs[func];
        let tuple_ty = self.ir.funcs[id].param;
        let tuple_val = self.instr(tuple_ty, Instr::Param);
        for (index, (param, tuple_loc)) in params
            .into_iter()
            .zip(self.ir.types[tuple_ty.index()].tuple())
            .enumerate()
        {
            let ty = self.ir.tuples[tuple_loc];
            let val = self.instr(ty, Instr::Elem(tuple_val, Index(index.try_into().unwrap())));
            let name = self.tree.params[param].name;
            self.set_val(name, val);
        }
        self.stmts(body.stmts)?;
        let ret = match body.expr {
            Some(expr) => self.expr(expr)?,
            None => {
                let ty = self.ty_unit();
                self.instr_tuple(ty, &[])
            }
        };
        self.ret(ret);
        Ok(start)
    }

    fn program(&mut self) -> LowerResult<()> {
        let string = {
            let name = self.ir.strings.make_id("String");
            let ty = self.ty(Type::String);
            self.set_name(name, Named::Type(ty));
            ty
        };
        {
            let name = self.ir.strings.make_id("args");
            let param = self.ty_unit();
            let result = self.ty(Type::List(string));
            let start = self.ir.instrs.len_idx();
            let ret = self.instr(result, Instr::Args);
            self.ret(ret);
            let id_type = self.ir.funcs.push(Func { param, result });
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_type, id_body);
            self.set_name(name, Named::Func(id_type));
        }
        {
            let name = self.ir.strings.make_id("println");
            let param = self.ty_tuple(&[string]);
            let result = self.ty_unit();
            let start = self.ir.instrs.len_idx();
            let val = self.instr(param, Instr::Param);
            let ret = self.instr(result, Instr::Println(val));
            self.ret(ret);
            let id_type = self.ir.funcs.push(Func { param, result });
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_type, id_body);
            self.set_name(name, Named::Func(id_type));
        }
        self.region(self.tree.root);
        if self.ir.main.is_none() {
            return Err(LowerError::NoMain);
        }
        for (scope, func, id_type) in take(&mut self.funcs) {
            self.path = scope;
            let start = self.body(id_type, func)?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_type, id_body);
        }
        Ok(())
    }
}

pub fn lower(source: &str, starts: &TokenStarts, tree: &Tree) -> LowerResult<IR> {
    let mut paths = IndexMap::new();
    let (i, _) = paths.insert_full(None, Named::Scope);
    let mut lower = Lower {
        source,
        starts,
        tree,
        ir: IR::default(),
        paths,
        path: PathId::from_usize(i),
        funcs: Vec::new(),
    };
    lower.program().map(|()| lower.ir)
}
