use std::{collections::HashMap, mem::take};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::IndexSet;

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex},
    parse::{self, Expr, ExprId, NeedId, Param, Path, Stmt, StmtId, Tree},
    range::{Inclusive, expr_range, path_range},
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
    pub struct NeedTyId = u32;
}

define_index_type! {
    pub struct NeedFnId = u32;
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
    pub def: Option<TypeId>,
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
    pub tys: IdRange<NeedTyId>,
    pub fns: IdRange<NeedFnId>,
    pub vals: IdRange<NeedValId>,
    pub ctxs: IdRange<NeedCtxId>,
}

#[derive(Clone, Copy, Debug)]
pub struct Ctxdef {
    pub needs: Needs,
}

#[derive(Clone, Copy, Debug)]
pub struct Structdef {
    pub fields: TupleRange,
}

#[derive(Clone, Copy, Debug)]
pub enum NeedKind {
    /// A set of needs that all have their individual kinds.
    Set,

    /// A need that must be satisfied statically.
    Static,

    /// A need that should be satisfied dynamically.
    Dynamic,
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
    BindTy(TydefId, TypeId),

    /// Bind a contextual function until the next [`Instr::End`].
    ///
    /// Type: unit.
    BindFn(FndefId, FndefId),

    /// Bind a contextual value until the next [`Instr::End`].
    ///
    /// Type: unit.
    BindVal(ValdefId, LocalId),

    /// Get a contextual value.
    ///
    /// Type: that of the given value.
    Val(ValdefId),

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
    pub fields: Tuples<(StrId, TypeId)>,
    pub need_tys: IndexVec<NeedTyId, Need<TypeId>>,
    pub need_fns: IndexVec<NeedFnId, Need<FndefId>>,
    pub need_vals: IndexVec<NeedValId, Need<ValdefId>>,
    pub need_ctxs: IndexVec<NeedCtxId, Need<CtxdefId>>,
    pub locals: IndexVec<LocalId, TypeId>,
    pub instrs: IndexVec<LocalId, Instr>,
    pub refs: IndexVec<RefId, LocalId>,
    pub bodies: IndexVec<FndefId, Option<LocalId>>,
}

impl IR {
    pub fn ty(&mut self, ty: Type) -> TypeId {
        let (i, _) = self.types.insert_full(ty);
        TypeId::from_usize(i)
    }
}

type ModuleNames<T> = HashMap<(ModuleId, StrId), T>;

fn get_name<T: Copy>(
    prelude: ModuleId,
    current: ModuleId,
    names: &ModuleNames<T>,
    key: (ModuleId, StrId),
) -> Option<T> {
    let (module, name) = key;
    if let Some(&id) = names.get(&(module, name)) {
        Some(id)
    } else if module == current
        && let Some(&id) = names.get(&(prelude, name))
    {
        Some(id)
    } else {
        None
    }
}

#[derive(Debug, Default)]
pub struct Names {
    pub modules: ModuleNames<ModuleId>,
    pub tydefs: ModuleNames<TydefId>,
    pub fndefs: ModuleNames<FndefId>,
    pub valdefs: ModuleNames<ValdefId>,
    pub ctxdefs: ModuleNames<CtxdefId>,
    pub structdefs: ModuleNames<StructdefId>,
    pub methods: HashMap<(ModuleId, StructdefId, StrId), FndefId>,
}

#[derive(Clone, Copy, Debug)]
pub enum LowerError {
    Undefined(TokenId),
    Ambiguous(TokenId),
    NeedStaticCtx(Path),
    ThisNotMethod(ExprId),
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
            LowerError::Ambiguous(token) => (
                Some(Inclusive {
                    first: token,
                    last: token,
                }),
                "ambiguous".to_owned(),
            ),
            LowerError::NeedStaticCtx(path) => (
                Some(path_range(tree, path)),
                "use `static` on individual items, not on `context` dependencies".to_owned(),
            ),
            LowerError::ThisNotMethod(expr) => (
                Some(expr_range(tree, expr)),
                "cannot use `this` in a function that is not a method".to_owned(),
            ),
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
    ir: &'a mut IR,
    names: &'a mut Names,
    module: ModuleId,
    prelude: ModuleId,
    imports: &'a [ModuleId],
    tydefs: IndexVec<parse::TydefId, TydefId>,
    fndefs: IndexVec<parse::FndefId, (Option<StructdefId>, FndefId)>,
    valdefs: IndexVec<parse::ValdefId, ValdefId>,
    ctxdefs: IndexVec<parse::CtxdefId, CtxdefId>,
    structdefs: IndexVec<parse::StructdefId, StructdefId>,
    funcs: Vec<(parse::FndefId, Option<StructdefId>, FndefId)>,
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
        self.ir.ty(ty)
    }

    fn ty_tuple(&mut self, elems: &[TypeId]) -> TypeId {
        let tuple = self.ir.tuples.make(elems);
        self.ty(Type::Tuple(tuple))
    }

    fn ty_unit(&mut self) -> TypeId {
        self.ty_tuple(&[])
    }

    fn path(&mut self, path: Path) -> LowerResult<(ModuleId, StrId)> {
        let mut module = self.module;
        for name in path.prefix {
            let token = self.tree.names[name];
            let string = self.name(token);
            let Some(&next) = self.names.modules.get(&(module, string)) else {
                return Err(LowerError::Undefined(token));
            };
            module = next;
        }
        let name = self.name(path.last);
        Ok((module, name))
    }

    fn tydef(&mut self, key: (ModuleId, StrId)) -> Option<TydefId> {
        get_name(self.prelude, self.module, &self.names.tydefs, key)
    }

    fn fndef(&mut self, key: (ModuleId, StrId)) -> Option<FndefId> {
        get_name(self.prelude, self.module, &self.names.fndefs, key)
    }

    fn valdef(&mut self, key: (ModuleId, StrId)) -> Option<ValdefId> {
        get_name(self.prelude, self.module, &self.names.valdefs, key)
    }

    fn ctxdef(&mut self, key: (ModuleId, StrId)) -> Option<CtxdefId> {
        get_name(self.prelude, self.module, &self.names.ctxdefs, key)
    }

    fn structdef(&mut self, key: (ModuleId, StrId)) -> Option<StructdefId> {
        get_name(self.prelude, self.module, &self.names.structdefs, key)
    }

    fn imports(&mut self) -> LowerResult<()> {
        for (import, &module) in self.tree.imports.iter().zip(self.imports) {
            if let Some(token) = import.name {
                let name = self.name(token);
                self.names.modules.insert((self.module, name), module);
            }
            for using in import.using {
                let token = self.tree.names[using];
                let name = self.name(token);
                let mut found = false;
                if let Some(&module) = self.names.modules.get(&(module, name)) {
                    found = true;
                    self.names.modules.insert((self.module, name), module);
                }
                if let Some(&tydef) = self.names.tydefs.get(&(module, name)) {
                    found = true;
                    self.names.tydefs.insert((self.module, name), tydef);
                }
                if let Some(&fndef) = self.names.fndefs.get(&(module, name)) {
                    found = true;
                    self.names.fndefs.insert((self.module, name), fndef);
                }
                if let Some(&valdef) = self.names.valdefs.get(&(module, name)) {
                    found = true;
                    self.names.valdefs.insert((self.module, name), valdef);
                }
                if let Some(&ctxdef) = self.names.ctxdefs.get(&(module, name)) {
                    found = true;
                    self.names.ctxdefs.insert((self.module, name), ctxdef);
                }
                if let Some(&structdef) = self.names.structdefs.get(&(module, name)) {
                    found = true;
                    self.names.structdefs.insert((self.module, name), structdef);
                }
                if !found {
                    return Err(LowerError::Undefined(token));
                }
            }
        }
        Ok(())
    }

    fn names(&mut self) -> LowerResult<()> {
        // We must process struct names before function names so that we can attach methods.
        let mut next_struct = self.ir.structdefs.next_idx();
        let mut next_fn = self.ir.fndefs.next_idx();
        self.structdefs = (self.tree.structdefs.iter())
            .map(|item| {
                let id = next_struct;
                next_struct += 1;
                let name = self.name(item.name);
                self.names.structdefs.insert((self.module, name), id);
                id
            })
            .collect();
        self.fndefs = (self.tree.fndefs.iter())
            .map(|item| {
                let id = next_fn;
                next_fn += 1;
                let name = self.name(item.name);
                let structdef = match item.ty {
                    None => {
                        self.names.fndefs.insert((self.module, name), id);
                        None
                    }
                    Some(token) => {
                        let name = self.name(token);
                        let Some(&structdef) = self.names.structdefs.get(&(self.module, name))
                        else {
                            return Err(LowerError::Undefined(token));
                        };
                        self.names
                            .methods
                            .insert((self.module, structdef, name), id);
                        Some(structdef)
                    }
                };
                Ok((structdef, id))
            })
            .collect::<LowerResult<_>>()?;
        // Other kinds of things can happen in arbitrary order.
        let mut next_ty = self.ir.tydefs.next_idx();
        let mut next_val = self.ir.valdefs.next_idx();
        let mut next_ctx = self.ir.ctxdefs.next_idx();
        self.tydefs = (self.tree.tydefs.iter())
            .map(|item| {
                let id = next_ty;
                next_ty += 1;
                let name = self.name(item.name);
                self.names.tydefs.insert((self.module, name), id);
                id
            })
            .collect();
        self.valdefs = (self.tree.valdefs.iter())
            .map(|item| {
                let id = next_val;
                next_val += 1;
                let name = self.name(item.name);
                self.names.valdefs.insert((self.module, name), id);
                id
            })
            .collect();
        self.ctxdefs = (self.tree.ctxdefs.iter())
            .map(|item| {
                let id = next_ctx;
                next_ctx += 1;
                let name = self.name(item.name);
                self.names.ctxdefs.insert((self.module, name), id);
                id
            })
            .collect();
        Ok(())
    }

    fn needs(&mut self, needs: IdRange<NeedId>) -> LowerResult<Needs> {
        let mut tys = Vec::new();
        let mut fns = Vec::new();
        let mut vals = Vec::new();
        let mut ctxs = Vec::new();
        for need in needs {
            let parse::Need { kind, path } = self.tree.needs[need];
            let key = self.path(path)?;
            match (
                self.tydef(key),
                self.fndef(key),
                self.valdef(key),
                self.ctxdef(key),
                self.structdef(key),
            ) {
                (Some(tydef), None, None, None, None) => {
                    let kind = match kind {
                        parse::NeedKind::Default | parse::NeedKind::Static => NeedKind::Static,
                    };
                    let id = self.ty(Type::Tydef(tydef));
                    tys.push(Need { kind, id });
                }
                (None, Some(id), None, None, None) => {
                    let kind = match kind {
                        parse::NeedKind::Default => NeedKind::Dynamic,
                        parse::NeedKind::Static => NeedKind::Static,
                    };
                    fns.push(Need { kind, id });
                }
                (None, None, Some(id), None, None) => {
                    let kind = match kind {
                        parse::NeedKind::Default => NeedKind::Dynamic,
                        parse::NeedKind::Static => NeedKind::Static,
                    };
                    vals.push(Need { kind, id });
                }
                (None, None, None, Some(id), None) => {
                    let kind = match kind {
                        parse::NeedKind::Default => NeedKind::Dynamic,
                        parse::NeedKind::Static => return Err(LowerError::NeedStaticCtx(path)),
                    };
                    ctxs.push(Need { kind, id });
                }
                (None, None, None, None, Some(structdef)) => {
                    let kind = match kind {
                        parse::NeedKind::Default | parse::NeedKind::Static => NeedKind::Static,
                    };
                    let id = self.ty(Type::Structdef(structdef));
                    tys.push(Need { kind, id });
                }
                (None, None, None, None, None) => return Err(LowerError::Undefined(path.last)),
                _ => return Err(LowerError::Ambiguous(path.last)),
            }
        }
        Ok(Needs {
            tys: IdRange::new(&mut self.ir.need_tys, tys),
            fns: IdRange::new(&mut self.ir.need_fns, fns),
            vals: IdRange::new(&mut self.ir.need_vals, vals),
            ctxs: IdRange::new(&mut self.ir.need_ctxs, ctxs),
        })
    }

    fn parse_ty(&mut self, ty: parse::TypeId) -> LowerResult<TypeId> {
        match self.tree.types[ty] {
            parse::Type::Path(path) => {
                let key = self.path(path)?;
                match (self.tydef(key), self.structdef(key)) {
                    (Some(tydef), None) => Ok(self.ty(Type::Tydef(tydef))),
                    (None, Some(structdef)) => Ok(self.ty(Type::Structdef(structdef))),
                    (None, None) => Err(LowerError::Undefined(path.last)),
                    (Some(_), Some(_)) => Err(LowerError::Ambiguous(path.last)),
                }
            }
        }
    }

    fn decls(&mut self) -> LowerResult<()> {
        for (id, &parse::Tydef { name: _, def }) in self.tree.tydefs.iter_enumerated() {
            let id = self.tydefs[id];
            let tydef = Tydef {
                def: match def {
                    Some(ty) => Some(self.parse_ty(ty)?),
                    None => None,
                },
            };
            assert_eq!(self.ir.tydefs.push(tydef), id);
        }
        for (
            parse_id,
            &parse::Fndef {
                ty: _,
                name: _,
                needs,
                params,
                result,
                def: _,
            },
        ) in self.tree.fndefs.iter_enumerated()
        {
            let (structdef, id) = self.fndefs[parse_id];
            self.funcs.push((parse_id, structdef, id));
            let mut types = Vec::new();
            if let Some(structdef) = structdef {
                types.push(self.ty(Type::Structdef(structdef)));
            }
            for param in params {
                types.push(self.parse_ty(self.tree.params[param].ty)?);
            }
            let fndef = Fndef {
                needs: self.needs(needs)?,
                param: self.ty_tuple(&types),
                result: match result {
                    Some(ty) => self.parse_ty(ty)?,
                    None => self.ty_unit(),
                },
            };
            assert_eq!(self.ir.fndefs.push(fndef), id);
        }
        for (id, &parse::Valdef { name: _, needs, ty }) in self.tree.valdefs.iter_enumerated() {
            let id = self.valdefs[id];
            let valdef = Valdef {
                needs: self.needs(needs)?,
                ty: self.parse_ty(ty)?,
            };
            assert_eq!(self.ir.valdefs.push(valdef), id);
        }
        for (id, &parse::Ctxdef { name: _, needs }) in self.tree.ctxdefs.iter_enumerated() {
            let id = self.ctxdefs[id];
            let ctxdef = Ctxdef {
                needs: self.needs(needs)?,
            };
            assert_eq!(self.ir.ctxdefs.push(ctxdef), id);
        }
        for (id, parse::Structdef { name: _, fields }) in self.tree.structdefs.iter_enumerated() {
            let id = self.structdefs[id];
            let pairs = fields
                .into_iter()
                .map(|param| {
                    let Param { name, ty } = self.tree.params[param];
                    Ok((self.name(name), self.parse_ty(ty)?))
                })
                .collect::<LowerResult<Vec<_>>>()?;
            let structdef = Structdef {
                fields: self.ir.fields.make(&pairs),
            };
            assert_eq!(self.ir.structdefs.push(structdef), id);
        }
        Ok(())
    }

    fn bodies(&mut self) -> LowerResult<()> {
        for (fndef, structdef, id_type) in take(&mut self.funcs) {
            let start = Body {
                x: self,
                tree: fndef,
                ir: id_type,
                ty: structdef,
                locals: HashMap::new(),
            }
            .body()?;
            let id_body = self.ir.bodies.push(start);
            assert_eq!(id_type, id_body);
        }
        Ok(())
    }

    fn program(&mut self) -> LowerResult<()> {
        self.imports()?;
        self.names()?;
        self.decls()?;
        self.bodies()?;
        Ok(())
    }
}

struct Body<'a, 'b> {
    x: &'b mut Lower<'a>,
    tree: parse::FndefId,
    ir: FndefId,
    ty: Option<StructdefId>,
    locals: HashMap<StrId, LocalId>,
}

impl Body<'_, '_> {
    fn get(&mut self, token: TokenId) -> LowerResult<LocalId> {
        let name = self.x.name(token);
        match self.locals.get(&name) {
            Some(&local) => Ok(local),
            None => Err(LowerError::Undefined(token)),
        }
    }

    fn set(&mut self, token: TokenId, local: LocalId) {
        let name = self.x.name(token);
        self.locals.insert(name, local);
    }

    fn instr(&mut self, ty: TypeId, instr: Instr) -> LocalId {
        let id_local = self.x.ir.locals.push(ty);
        let id_instr = self.x.ir.instrs.push(instr);
        assert_eq!(id_local, id_instr);
        id_local
    }

    fn instr_tuple(&mut self, ty: TypeId, locals: &[LocalId]) -> LocalId {
        let start = self.x.ir.refs.len_idx();
        self.x.ir.refs.extend_from_slice(IndexSlice::new(locals));
        let end = self.x.ir.refs.len_idx();
        self.instr(ty, Instr::Tuple(IdRange { start, end }))
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<LocalId> {
        match self.x.tree.exprs[expr] {
            Expr::This(_) => todo!(),
            Expr::Path(path) => {
                let (module, name) = self.x.path(path)?;
                if path.prefix.is_empty()
                    && let Some(&local) = self.locals.get(&name)
                {
                    return Ok(local);
                }
                let Some(val) = self.x.valdef((module, name)) else {
                    return Err(LowerError::Undefined(path.last));
                };
                let ty = self.x.ir.valdefs[val].ty;
                Ok(self.instr(ty, Instr::Val(val)))
            }
            Expr::Int(token) => {
                let ty = self.x.ty(Type::Int32);
                let n = self.x.slice(token).parse().unwrap();
                Ok(self.instr(ty, Instr::Int32(n)))
            }
            Expr::String(token) => {
                let ty = self.x.ty(Type::String);
                let string = self.x.string(token);
                Ok(self.instr(ty, Instr::String(string)))
            }
            Expr::Struct(_, _) => todo!(),
            Expr::Field(object, field) => {
                let obj = self.expr(object)?;
                let name = self.x.name(field);
                let fields = &self.x.ir.fields[self.x.ir.structdefs
                    [self.x.ir.types[self.x.ir.locals[obj].index()].structdef()]
                .fields];
                // TODO: Make this not be linear time.
                let id = FieldId::from_usize(
                    fields.iter().position(|&(field, _)| field == name).unwrap(),
                );
                let (_, ty) = fields[id.index()];
                Ok(self.instr(ty, Instr::Field(obj, id)))
            }
            Expr::Method(object, method, args) => {
                let obj = self.expr(object)?;
                let ty = self.x.ir.locals[obj];
                let structdef = self.x.ir.types[ty.index()].structdef();
                let name = self.x.name(method);
                let Some(&fndef) = self.x.names.methods.get(&(self.x.module, structdef, name))
                else {
                    return Err(LowerError::Undefined(method));
                };
                let Fndef {
                    needs: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() != params.len() {
                    return Err(LowerError::ArgCount(expr));
                }
                let mut locals = vec![obj];
                for arg in args {
                    locals.push(self.expr(arg)?);
                }
                let arg = self.instr_tuple(param, &locals);
                Ok(self.instr(result, Instr::Call(fndef, arg)))
            }
            Expr::Call(callee, binds, args) => {
                if !binds.is_empty() {
                    todo!()
                }
                let key = self.x.path(callee)?;
                let Some(fndef) = self.x.fndef(key) else {
                    return Err(LowerError::Undefined(callee.last));
                };
                let Fndef {
                    needs: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() != params.len() {
                    return Err(LowerError::ArgCount(expr));
                }
                let locals = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<LocalId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                Ok(self.instr(result, Instr::Call(fndef, arg)))
            }
            Expr::Binary(l, op, r) => {
                let a = self.expr(l)?;
                let b = self.expr(r)?;
                match (
                    self.x.ir.types[self.x.ir.locals[a].index()],
                    self.x.ir.types[self.x.ir.locals[b].index()],
                ) {
                    (Type::Int32, Type::Int32) => match op {
                        parse::Binop::Add => {
                            let ty = self.x.ty(Type::Int32);
                            Ok(self.instr(ty, Instr::Int32Arith(a, Int32Arith::Add, b)))
                        }
                        parse::Binop::Sub => {
                            let ty = self.x.ty(Type::Int32);
                            Ok(self.instr(ty, Instr::Int32Arith(a, Int32Arith::Sub, b)))
                        }
                        parse::Binop::Mul => {
                            let ty = self.x.ty(Type::Int32);
                            Ok(self.instr(ty, Instr::Int32Arith(a, Int32Arith::Mul, b)))
                        }
                        parse::Binop::Div => {
                            let ty = self.x.ty(Type::Int32);
                            Ok(self.instr(ty, Instr::Int32Arith(a, Int32Arith::Div, b)))
                        }
                        parse::Binop::Neq => {
                            let ty = self.x.ty(Type::Bool);
                            Ok(self.instr(ty, Instr::Int32Comp(a, Int32Comp::Neq, b)))
                        }
                        parse::Binop::Less => {
                            let ty = self.x.ty(Type::Bool);
                            Ok(self.instr(ty, Instr::Int32Comp(a, Int32Comp::Less, b)))
                        }
                    },
                    _ => todo!(),
                }
            }
            Expr::If(_, _, _) => todo!(),
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) -> LowerResult<()> {
        for stmt in stmts {
            match self.x.tree.stmts[stmt] {
                Stmt::Let(name, rhs) => {
                    let local = self.expr(rhs)?;
                    self.set(name, local);
                }
                Stmt::Var(name, rhs) => {
                    let local = self.expr(rhs)?;
                    self.set(name, local);
                }
                Stmt::Assign(lhs, rhs) => match self.x.tree.exprs[lhs] {
                    Expr::Path(path) => {
                        assert!(path.prefix.is_empty());
                        let before = self.get(path.last)?;
                        let after = self.expr(rhs)?;
                        let unit = self.x.ty_unit();
                        self.instr(unit, Instr::Set(before, after));
                    }
                    _ => panic!(),
                },
                Stmt::While(cond, body) => {
                    assert!(body.expr.is_none());
                    let unit = self.x.ty_unit();
                    self.instr(unit, Instr::Loop);
                    let local = self.expr(cond)?;
                    self.instr(unit, Instr::If(local));
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

    fn body(&mut self) -> LowerResult<Option<LocalId>> {
        let parse::Fndef {
            ty: _,
            name: _,
            needs: _,
            params,
            result: _,
            def,
        } = self.x.tree.fndefs[self.tree];
        let Some(body) = def else { return Ok(None) };
        let start = self.x.ir.instrs.len_idx();
        let Fndef {
            needs: _,
            param: tuple_ty,
            result: ret_ty,
        } = self.x.ir.fndefs[self.ir];
        let tuple_local = self.instr(tuple_ty, Instr::Param);
        for (index, (param, tuple_loc)) in params
            .into_iter()
            .zip(self.x.ir.types[tuple_ty.index()].tuple())
            .enumerate()
        {
            let ty = self.x.ir.tuples[tuple_loc];
            let local = self.instr(ty, Instr::Elem(tuple_local, ElemId::new(index)));
            let name = self.x.tree.params[param].name;
            self.set(name, local);
        }
        self.stmts(body.stmts)?;
        let ret = match body.expr {
            Some(expr) => self.expr(expr)?,
            None => {
                let ty = self.x.ty_unit();
                self.instr_tuple(ty, &[])
            }
        };
        self.instr(ret_ty, Instr::Return(ret));
        Ok(Some(start))
    }
}

pub fn lower(
    source: &str,
    starts: &TokenStarts,
    tree: &Tree,
    ir: &mut IR,
    names: &mut Names,
    prelude: ModuleId,
    imports: &[ModuleId],
) -> LowerResult<ModuleId> {
    assert_eq!(tree.imports.len(), imports.len());
    let module = ir.modules.push(());
    let mut lower = Lower {
        source,
        starts,
        tree,
        ir,
        names,
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
    Ok(module)
}
