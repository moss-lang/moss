use std::{collections::HashMap, mem::take, ops::AddAssign};

use index_vec::{Idx, IndexSlice, IndexVec, define_index_type};
use indexmap::IndexSet;

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex},
    parse::{self, ExprId, ImportId, Tree},
    range::{Inclusive, expr_range},
    tuples::{TupleRange, Tuples},
    util::IdRange,
};

define_index_type! {
    pub struct ModuleId = u32;
}

define_index_type! {
    pub struct TypeId = u32;
}

define_index_type! {
    pub struct ElemId = u32;
}

define_index_type! {
    pub struct TydefId = u32;
}

define_index_type! {
    pub struct FndefId = u32;
}

define_index_type! {
    pub struct ValdefId = u32;
}

define_index_type! {
    pub struct CtxdefId = u32;
}

define_index_type! {
    pub struct StructdefId = u32;
}

define_index_type! {
    pub struct NeedTypeId = u32;
}

define_index_type! {
    pub struct NeedFuncId = u32;
}

define_index_type! {
    pub struct NeedValId = u32;
}

define_index_type! {
    pub struct NeedCtxId = u32;
}

define_index_type! {
    pub struct FieldId = u32;
}

define_index_type! {
    pub struct LocalId = u32;
}

define_index_type! {
    pub struct RefId = u32;
}

define_index_type! {
    pub struct PathId = u32;
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    /// Static constant string, usually a literal. Runtime-varying strings are not a primitive type.
    String,

    Bool,

    Int32,

    /// All empty tuples are identically the unit type.
    Tuple(TupleRange),

    Tydef(TydefId),

    Structdef(StructdefId),
}

impl Type {
    pub fn tuple(self) -> TupleRange {
        match self {
            Type::Tuple(range) => range,
            _ => panic!(),
        }
    }

    pub fn tydef(self) -> TydefId {
        match self {
            Type::Tydef(id) => id,
            _ => panic!(),
        }
    }

    pub fn structdef(self) -> StructdefId {
        match self {
            Type::Structdef(id) => id,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Tydef {
    def: Option<TypeId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Fndef {
    pub needs: Needs,
    pub param: TypeId,
    pub result: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Valdef {
    pub needs: Needs,
    pub ty: TypeId,
}

#[derive(Clone, Copy, Debug)]
pub struct Needs {
    pub types: IdRange<NeedTypeId>,
    pub funcs: IdRange<NeedFuncId>,
    pub vals: IdRange<NeedFuncId>,
    pub ctxs: IdRange<NeedCtxId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub needs: Needs,
}

#[derive(Clone, Copy, Debug)]
pub struct Structdef {
    pub fields: IdRange<TypeId>,
}

#[derive(Clone, Copy, Debug)]
pub enum NeedKind {
    Default,
    Static,
}

#[derive(Clone, Copy, Debug)]
pub struct Need<T> {
    pub kind: NeedKind,
    pub id: T,
}

#[derive(Clone, Copy, Debug)]
pub struct Depth(pub u32);

#[derive(Clone, Copy, Debug)]
pub enum Int32Arith {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone, Copy, Debug)]
pub enum Int32Comp {
    Neq,
    Less,
}

/// When executed, each instruction implicitly defines a mutable local variable.
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    /// Bind a contextual type until the next [`Instr::End`].
    ///
    /// Type: unit.
    BindType(TydefId, TydefId),

    /// Bind a contextual function until the next [`Instr::End`].
    ///
    /// Type: unit.
    BindFunc(FndefId, FndefId),

    /// Bind a contextual value until the next [`Instr::End`].
    ///
    /// Type: unit.
    BindVal(ValdefId, LocalId),

    /// Get a contextual value.
    ///
    /// Type: that of the given value.
    Val(ValdefId),

    /// Get the object this method is being called on.
    ///
    /// Type: this method's object type.
    This,

    /// Get the value of the parameter to this function.
    ///
    /// Type: this function's parameter type.
    Param,

    /// Set the left-hand local to the value of the right-hand local.
    ///
    /// Type: unit.
    Set(LocalId, LocalId),

    /// A 32-bit integer constant.
    ///
    /// Type: [`Type::Int32`].
    Int32(i32),

    /// A string constant.
    ///
    /// Type: [`Type::String`].
    String(StrId),

    /// Construct a tuple.
    ///
    /// Type: [`Type::Tuple`] with the given element types.
    Tuple(IdRange<RefId>),

    /// Get an element of a tuple.
    ///
    /// Type: the element type.
    Elem(LocalId, ElemId),

    /// Get a field of a struct.
    ///
    /// Type: the field type.
    Field(LocalId, FieldId),

    /// Compute integer arithmetic.
    ///
    /// Type: [`Type::Int32`].
    Int32Arith(LocalId, Int32Arith, LocalId),

    /// Compare two integers.
    ///
    /// Type: [`Type::Bool`].
    Int32Comp(LocalId, Int32Comp, LocalId),

    /// Call a function.
    ///
    /// Type: the function's result type.
    Call(FndefId, LocalId),

    /// End the current block, clearing any bindings made inside the block.
    ///
    /// Type: unit.
    End,

    /// Start a block.
    ///
    /// Type: unit.
    Block,

    /// Start a block only if the given condition is true.
    ///
    /// Type: unit.
    If(LocalId),

    /// Start a block only if the preceding [`Instr::If`] condition was false.
    ///
    /// Type: unit.
    Else,

    /// Start a loop block.
    ///
    /// Type: unit.
    Loop,

    /// Branch to the end of the block with the given depth, or the beginning if it's a loop block.
    ///
    /// Type: unit.
    Br(Depth),

    /// Return from this function.
    ///
    /// Type: unit.
    Return(LocalId),
}

#[derive(Debug, Default)]
pub struct IR {
    pub modules: IndexVec<ModuleId, ()>,
    pub strings: Strings,
    pub types: IndexSet<Type>,
    pub tuples: Tuples<TypeId>,
    pub tydefs: IndexVec<TydefId, Tydef>,
    pub fndefs: IndexVec<FndefId, Fndef>,
    pub valdefs: IndexVec<ValdefId, Valdef>,
    pub ctxdefs: IndexVec<CtxdefId, Ctxdef>,
    pub structdefs: IndexVec<StructdefId, Structdef>,
    pub need_types: IndexVec<NeedTypeId, Need<TypeId>>,
    pub need_funcs: IndexVec<NeedFuncId, Need<FndefId>>,
    pub need_vals: IndexVec<NeedValId, Need<ValdefId>>,
    pub need_ctxs: IndexVec<NeedCtxId, Need<CtxdefId>>,
    pub locals: IndexVec<LocalId, TypeId>,
    pub instrs: IndexVec<LocalId, Instr>,
    pub refs: IndexVec<RefId, LocalId>,
    pub bodies: IndexVec<FndefId, LocalId>,
}

#[derive(Clone, Copy, Debug)]
pub enum LowerError {
    Undefined(TokenId),
    ArgCount(ExprId),
}

impl LowerError {
    pub fn describe(self, _: &str, _: &TokenStarts, tree: &Tree) -> (Option<Inclusive>, String) {
        match self {
            LowerError::Undefined(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "undefined".to_owned(),
            ),
            LowerError::ArgCount(expr) => (
                Some(expr_range(tree, expr)),
                "wrong number of arguments".to_owned(),
            ),
        }
    }
}

type LowerResult<T> = Result<T, LowerError>;

#[derive(Debug, Default)]
pub struct Names {
    pub tydefs: HashMap<(ModuleId, StrId), TydefId>,
    pub fndefs: HashMap<(ModuleId, StrId), FndefId>,
    pub valdefs: HashMap<(ModuleId, StrId), ValdefId>,
    pub ctxdefs: HashMap<(ModuleId, StrId), CtxdefId>,
    pub structdefs: HashMap<(ModuleId, StrId), StructdefId>,
    pub fields: HashMap<(ModuleId, StructdefId, StrId), FieldId>,
    pub methods: HashMap<(ModuleId, StructdefId, StrId), FndefId>,
}

struct Lower<'a> {
    source: &'a str,
    starts: &'a TokenStarts,
    tree: &'a Tree,
    ir: &'a mut IR,
    names: &'a mut Names,
    module: ModuleId,
    prelude: Option<ModuleId>,
    imports: &'a IndexSlice<ImportId, [ModuleId]>,
    tydefs: IndexVec<parse::TydefId, TydefId>,
    fndefs: IndexVec<parse::FndefId, FndefId>,
    valdefs: IndexVec<parse::ValdefId, ValdefId>,
    ctxdefs: IndexVec<parse::CtxdefId, CtxdefId>,
    structdefs: IndexVec<parse::StructdefId, StructdefId>,
    funcs: Vec<(PathId, parse::FndefId, FndefId)>,
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

    fn get_path_of(&self, token: TokenId) -> LowerResult<PathId> {
        let mut parent = self.path;
        let Some(string) = self.ir.strings.get_id(self.slice(token)) else {
            return Err(LowerError::Undefined(token));
        };
        loop {
            match self.paths.get_index_of(&Some((parent, string))) {
                Some(i) => return Ok(PathId::from_usize(i)),
                None => {
                    let (parts, _) = self.paths.get_index(parent.index()).unwrap();
                    let (grandparent, _) = parts.unwrap();
                    parent = grandparent;
                }
            }
        }
    }

    fn get_path(&mut self, path: parse::Path) -> LowerResult<Named> {
        let mut id = self.get_path_of(path.name)?;
        for name in path.names {
            let Named::Scope = self.paths[id.index()] else {
                panic!();
            };
            let string = self.name(self.tree.names[name]);
            id = PathId::from_usize(self.paths.get_index_of(&Some((id, string))).unwrap());
        }
        Ok(self.paths[id.index()])
    }

    fn get(&self, token: TokenId) -> LowerResult<Named> {
        Ok(self.paths[self.get_path_of(token)?.index()])
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

    fn parse_ty(&mut self, ty: parse::TypeId) -> LowerResult<TypeId> {
        match self.tree.types[ty] {
            parse::Type::Name(name) => Ok(self.get(name)?.ty()),
        }
    }

    fn imports(&mut self) -> LowerResult<()> {
        for (id, import) in self.tree.imports.iter_enumerated() {
            if let Some(name) = import.name {
                self.set_token(name, Named::Import(id));
            }
            let paths = self.imports[id];
            let root = PathId::new(paths.get_index_of(&None).unwrap());
            for name in import.using {
                let string = self.string(self.tree.names[name]);
                let named = paths[&Some((root, string))];
                match named {
                    Named::Module | Named::Import(_) => panic!(),
                    Named::Ty(_)
                    | Named::Fndef(_)
                    | Named::Valdef(_)
                    | Named::Ctxdef(_)
                    | Named::Structdef(_)
                    | Named::Field(_)
                    | Named::Local(_) => {
                        self.set_name(string, named);
                    }
                }
            }
        }
        Ok(())
    }

    fn prealloc_impl<I: Idx, J: Idx + AddAssign<usize>, T: Copy + parse::Named, U>(
        &mut self,
        old: &IndexVec<I, T>,
        new: &IndexVec<J, U>,
        named: impl Fn(J) -> Named,
    ) -> IndexVec<I, J> {
        let mut next = new.next_idx();
        old.iter()
            .map(|item| {
                let id = next;
                next += 1;
                self.set_token(item.name(), named(id));
                id
            })
            .collect()
    }

    fn prealloc<I: Idx, J: Idx + AddAssign<usize> + Into<Named>, T: Copy + parse::Named, U>(
        &mut self,
        old: &IndexVec<I, T>,
        new: &IndexVec<J, U>,
    ) -> IndexVec<I, J> {
        self.prealloc_impl(old, new, J::into)
    }

    fn names(&mut self) {
        self.tydefs = self.prealloc_impl(&self.tree.tydefs, &self.ir.tydefs, |id| {
            Named::Ty(self.ty(Type::Tydef(id)))
        });
        self.fndefs = self.prealloc(&self.tree.fndefs, &self.ir.fndefs);
        self.valdefs = self.prealloc(&self.tree.valdefs, &self.ir.valdefs);
        self.ctxdefs = self.prealloc(&self.tree.ctxdefs, &self.ir.ctxdefs);
        self.structdefs = self.prealloc(&self.tree.structdefs, &self.ir.structdefs);
    }

    fn decls(&mut self) -> LowerResult<()> {
        for (id, &parse::Tydef { name: _, def }) in self.tree.tydefs.iter_enumerated() {
            let tydef = Tydef {
                def: match def {
                    Some(ty) => Some(self.parse_ty(ty)?),
                    None => None,
                },
            };
            assert_eq!(self.ir.tydefs.push(tydef), self.tydefs[id]);
        }
        for (
            id,
            parse::Fndef {
                ty,
                name,
                needs,
                params,
                result,
                def: _,
            },
        ) in self.tree.fndefs.iter_enumerated()
        {
            let types = params
                .into_iter()
                .map(|param| self.parse_ty(self.tree.params[param].ty))
                .collect::<LowerResult<Vec<TypeId>>>()?;
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
        for (id, valdef) in self.tree.valdefs.iter_enumerated() {
            todo!();
        }
        for Ctxdef { name, needs } in self.tree.ctxdefs {
            let parent = self.path;
            self.path = self.set_scope(name);
            for val in vals {
                let Val { name, ty } = self.tree.vals[val];
                let ty = self.parse_ty(ty)?;
                let id = self.ir.vars.push(Var { ty });
                self.set_var(name, id);
            }
            self.path = parent;
        }
        for (id, structdef) in self.tree.structdefs.iter_enumerated() {
            todo!();
        }
        Ok(())
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
            Expr::Path(path) => match self.get_path(path)? {
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
                    let func = self.get_path(path)?.func();
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
                        let before = self.get(path.name)?.val();
                        let after = self.expr(rhs)?;
                        let unit = self.ty_unit();
                        self.instr(unit, Instr::Set(before, after));
                    }
                    _ => panic!(),
                },
                Stmt::Provide(path, expr) => {
                    let ty = self.ty_unit();
                    let var = self.get_path(path)?.var();
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

    fn defs(&mut self) -> LowerResult<()> {
        for (scope, func, id_type) in take(&mut self.funcs) {
            self.path = scope;
            let start = self.body(id_type, func)?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_type, id_body);
        }
        Ok(())
    }

    fn program(&mut self) -> LowerResult<()> {
        self.imports()?;
        self.names();
        self.decls()?;
        self.defs()?;
        Ok(())
    }
}

pub fn lower(
    source: &str,
    starts: &TokenStarts,
    tree: &Tree,
    ir: &mut IR,
    prelude: Option<ModuleId>,
    imports: &IndexSlice<ImportId, [ModuleId]>,
) -> LowerResult<(ModuleId, Names)> {
    let mut names = Names::default();
    let module = ir.modules.push(());
    let mut lower = Lower {
        source,
        starts,
        tree,
        ir,
        names: &mut names,
        module,
        prelude,
        imports,
        tydefs: IndexVec::new(),
        fndefs: IndexVec::new(),
        valdefs: IndexVec::new(),
        ctxdefs: IndexVec::new(),
        structdefs: IndexVec::new(),
        funcs: Vec::new(),
    };
    lower.program()?;
    Ok((module, names))
}
