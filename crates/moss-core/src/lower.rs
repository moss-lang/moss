use std::{collections::HashMap, fmt, mem::take};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::IndexSet;

use crate::{
    intern::{StrId, Strings},
    lex::{TokenId, TokenStarts, relex},
    parse::{self, Bind, Block, Expr, ExprId, Field, NeedId, Param, Path, Stmt, StmtId, Tree},
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

    Int64,

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
    pub def: Needs,
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
    Rem,
}

#[derive(Clone, Copy, Debug)]
pub enum Int32Comp {
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

/// When executed, each instruction implicitly defines a mutable local variable.
#[derive(Clone, Copy, Debug)]
pub enum Instr {
    /// Start a block in which everything is computed at compile time.
    ///
    /// Type: unit.
    Static,

    /// End the current [`Instr::Static`] block.
    ///
    /// Type: unit.
    EndStatic,

    /// Bind a contextual value until the next [`Instr::EndBind`].
    ///
    /// Type: unit.
    BindVal(ValdefId, LocalId),

    /// End the most recent binding.
    ///
    /// Type: unit.
    EndBind,

    /// Get a contextual value.
    ///
    /// Type: that of the given value.
    Val(ValdefId),

    /// Get the value of the parameter to this function.
    ///
    /// Type: this function's parameter type.
    Param,

    /// Copy the value of another local into a fresh local.
    ///
    /// Type: same as the given local.
    Copy(LocalId),

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

    /// Construct a struct.
    ///
    /// Type: the given struct type.
    Struct(StructdefId, IdRange<RefId>),

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

    /// Start a block only if the given condition is true.
    ///
    /// Type: unit.
    If(LocalId, TypeId),

    /// Start a block only if the preceding [`Instr::If`] condition was false.
    ///
    /// Type: same as the specified local from the true branch.
    Else(LocalId),

    /// End the current [`Instr::If`] or [`Instr::Else`] block.
    ///
    /// Type: same as the specified local from the false branch.
    EndIf(LocalId),

    /// Start a loop block.
    ///
    /// Type: unit.
    Loop,

    /// End the current [`Instr::Loop`] block.
    ///
    /// Type: unit.
    EndLoop,

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
    pub need_tys: IndexVec<NeedTyId, Need<TydefId>>,
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

fn get_name_method(
    prelude: ModuleId,
    current: ModuleId,
    names: &HashMap<(ModuleId, StructdefId, StrId), FndefId>,
    key: (ModuleId, StructdefId, StrId),
) -> Option<FndefId> {
    let (module, ty, name) = key;
    if let Some(&id) = names.get(&(module, ty, name)) {
        Some(id)
    } else if module == current
        && let Some(&id) = names.get(&(prelude, ty, name))
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

struct ErrorCtx<'a> {
    tree: &'a Tree,
    ir: &'a IR,
}

impl ErrorCtx<'_> {
    fn path(&self, id: Path) -> Inclusive {
        path_range(self.tree, id)
    }

    fn expr(&self, id: ExprId) -> Inclusive {
        expr_range(self.tree, id)
    }

    fn ty(&self, id: TypeId) -> FatType<'_, '_> {
        FatType { ctx: self, id }
    }
}

struct FatType<'a, 'b> {
    ctx: &'b ErrorCtx<'a>,
    id: TypeId,
}

impl fmt::Display for FatType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.ctx.ir.types[self.id.index()] {
            Type::String => write!(f, "`StringLit`"),
            Type::Bool => write!(f, "`Bool`"),
            Type::Int32 => write!(f, "`RawInt32`"),
            Type::Int64 => write!(f, "`RawInt64`"),
            Type::Tuple(_) => write!(f, "a tuple"),
            Type::Tydef(tydef) => write!(f, "`type` index {tydef:?}"),
            Type::Structdef(structdef) => write!(f, "`struct` index {structdef:?}"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum LowerError {
    Undefined(TokenId),
    Ambiguous(TokenId),
    NeedStaticCtx(Path),
    NeedStructdef(Path),
    ExpectedType(TypeId, ExprId, TypeId),
    ThisNotMethod(ExprId),
    Overflow(ExprId),
    MissingField(ExprId, StructdefId, FieldId),
    ArgCount(ExprId),
}

impl LowerError {
    pub fn describe(
        self,
        _: &str,
        _: &TokenStarts,
        tree: &Tree,
        ir: &IR,
        _: &Names,
    ) -> (Option<Inclusive>, String) {
        let ctx = ErrorCtx { tree, ir };
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
                Some(ctx.path(path)),
                "use `static` on individual items, not on `context` dependencies".to_owned(),
            ),
            LowerError::NeedStructdef(path) => (
                Some(ctx.path(path)),
                "cannot have a concrete `struct` as a dependency".to_owned(),
            ),
            LowerError::ExpectedType(expected, expr, actual) => (
                Some(ctx.expr(expr)),
                format!("expected {}, got {}", ctx.ty(expected), ctx.ty(actual)),
            ),
            LowerError::ThisNotMethod(expr) => (
                Some(ctx.expr(expr)),
                "cannot use `this` in a function that is not a method".to_owned(),
            ),
            LowerError::Overflow(expr) => (Some(ctx.expr(expr)), "integer too large".to_owned()),
            LowerError::MissingField(expr, structdef, field) => {
                let (name, _) = ctx.ir.fields[ctx.ir.structdefs[structdef].fields][field.index()];
                (
                    Some(ctx.expr(expr)),
                    format!("missing field `{}`", &ctx.ir.strings[name]),
                )
            }
            LowerError::ArgCount(expr) => {
                (Some(ctx.expr(expr)), "wrong number of arguments".to_owned())
            }
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
        let mut escaped = String::new();
        let quoted = self.slice(token);
        let mut chars = quoted[1..quoted.len() - 1].chars();
        while let Some(c) = chars.next() {
            escaped.push(match c {
                '\\' => match chars.next() {
                    Some('"') => '"',
                    Some('\\') => '\\',
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    _ => unreachable!(),
                },
                _ => c,
            });
        }
        self.ir.strings.make_id(&escaped)
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

    /// Recursively dereferences `ty` through `type` aliases.
    ///
    /// Very shallow: only works if `ty` itself is an alias, not if it contains aliases.
    fn resolve(&self, mut ty: TypeId) -> TypeId {
        while let Type::Tydef(tydef) = self.ir.types[ty.index()]
            && let Some(id) = self.ir.tydefs[tydef].def
        {
            ty = id;
        }
        ty
    }

    fn path(&mut self, path: Path) -> LowerResult<(ModuleId, StrId)> {
        let mut module = self.module;
        for name in path.prefix {
            let token = self.tree.names[name];
            let string = self.name(token);
            let Some(next) = get_name(
                self.prelude,
                self.module,
                &self.names.modules,
                (module, string),
            ) else {
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

    fn method(&mut self, key: (ModuleId, StructdefId, StrId)) -> Option<FndefId> {
        get_name_method(self.prelude, self.module, &self.names.methods, key)
    }

    fn imports(&mut self) -> LowerResult<()> {
        for (import, &module) in self.tree.imports.iter().zip(self.imports) {
            if let Some(token) = import.name {
                let name = self.name(token);
                self.names.modules.insert((self.module, name), module);
            }
            for using in import.names {
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
            for using in import.methods {
                let tokens = self.tree.methods[using];
                let name = self.name(tokens.ty);
                let Some(&ty) = self.names.structdefs.get(&(module, name)) else {
                    return Err(LowerError::Undefined(tokens.ty));
                };
                let name = self.name(tokens.name);
                let Some(&method) = self.names.methods.get(&(module, ty, name)) else {
                    return Err(LowerError::Undefined(tokens.name));
                };
                self.names.methods.insert((self.module, ty, name), method);
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
                let fn_name = self.name(item.name);
                let structdef = match item.ty {
                    None => {
                        self.names.fndefs.insert((self.module, fn_name), id);
                        None
                    }
                    Some(token) => {
                        let struct_name = self.name(token);
                        let Some(&structdef) =
                            self.names.structdefs.get(&(self.module, struct_name))
                        else {
                            return Err(LowerError::Undefined(token));
                        };
                        self.names
                            .methods
                            .insert((self.module, structdef, fn_name), id);
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
                (Some(id), None, None, None, None) => {
                    let kind = match kind {
                        parse::NeedKind::Default | parse::NeedKind::Static => NeedKind::Static,
                    };
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
                (None, None, None, None, Some(_)) => return Err(LowerError::NeedStructdef(path)),
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
            // For methods, we just make the object the first element of the type for the tuple of
            // parameters, since that way codegen doesn't need to think about the difference between
            // functions and methods at all.
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
        for (id, &parse::Ctxdef { name: _, def }) in self.tree.ctxdefs.iter_enumerated() {
            let id = self.ctxdefs[id];
            let ctxdef = Ctxdef {
                def: self.needs(def)?,
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
                this: None, // Gets set later if `ty` is `Some`.
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
    this: Option<LocalId>,
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

    fn instr_struct(&mut self, structdef: StructdefId, locals: &[LocalId]) -> LocalId {
        let ty = self.x.ir.ty(Type::Structdef(structdef));
        let start = self.x.ir.refs.len_idx();
        self.x.ir.refs.extend_from_slice(IndexSlice::new(locals));
        let end = self.x.ir.refs.len_idx();
        self.instr(ty, Instr::Struct(structdef, IdRange { start, end }))
    }

    fn expr(&mut self, expr: ExprId) -> LowerResult<LocalId> {
        match self.x.tree.exprs[expr] {
            Expr::This(_) => match self.this {
                None => Err(LowerError::ThisNotMethod(expr)),
                Some(local) => Ok(local),
            },
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
                let Ok(n) = self.x.slice(token).parse() else {
                    return Err(LowerError::Overflow(expr));
                };
                Ok(self.instr(ty, Instr::Int32(n)))
            }
            Expr::String(token) => {
                let ty = self.x.ty(Type::String);
                let string = self.x.string(token);
                Ok(self.instr(ty, Instr::String(string)))
            }
            Expr::Struct(path, fields) => {
                let key = self.x.path(path)?;
                let Some(structdef) = self.x.structdef(key) else {
                    return Err(LowerError::Undefined(path.last));
                };
                let map = fields
                    .into_iter()
                    .map(|field| {
                        let Field { name, val } = self.x.tree.fields[field];
                        Ok((self.x.name(name), self.expr(val)?))
                    })
                    .collect::<LowerResult<HashMap<StrId, LocalId>>>()?;
                let locals = self.x.ir.fields[self.x.ir.structdefs[structdef].fields]
                    .iter()
                    .enumerate()
                    .map(|(i, (name, _))| match map.get(name) {
                        Some(&local) => Ok(local),
                        None => Err(LowerError::MissingField(expr, structdef, FieldId::new(i))),
                    })
                    .collect::<LowerResult<Vec<LocalId>>>()?;
                Ok(self.instr_struct(structdef, &locals))
            }
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
                let Some(fndef) = self.x.method((self.x.module, structdef, name)) else {
                    return Err(LowerError::Undefined(method));
                };
                let Fndef {
                    needs: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() + 1 != params.len() {
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
                let key = self.x.path(callee)?;
                let Some(fndef) = self.x.fndef(key) else {
                    return Err(LowerError::Undefined(callee.last));
                };
                let Fndef {
                    needs: _,
                    param,
                    result,
                } = self.x.ir.fndefs[fndef];
                // TODO: Handle dynamic bindings instead of just assuming `NeedKind::Static`.
                let unit = self.x.ty_unit();
                self.instr(unit, Instr::Static);
                let bindings = binds
                    .get(&self.x.tree.binds)
                    .iter()
                    .map(|&Bind { path, val }| {
                        let key = self.x.path(path)?;
                        let Some(valdef) = self.x.valdef(key) else {
                            return Err(LowerError::Undefined(path.last));
                        };
                        let local = self.expr(val)?;
                        Ok((valdef, local))
                    })
                    .collect::<LowerResult<Vec<_>>>()?;
                self.instr(unit, Instr::EndStatic);
                for (valdef, local) in bindings {
                    self.instr(unit, Instr::BindVal(valdef, local));
                }
                let params = &self.x.ir.tuples[self.x.ir.types[param.index()].tuple()];
                if args.len() != params.len() {
                    return Err(LowerError::ArgCount(expr));
                }
                let locals = args
                    .into_iter()
                    .map(|arg| self.expr(arg))
                    .collect::<LowerResult<Vec<LocalId>>>()?;
                let arg = self.instr_tuple(param, &locals);
                let call = self.instr(result, Instr::Call(fndef, arg));
                for _ in binds {
                    self.instr(unit, Instr::EndBind);
                }
                Ok(call)
            }
            Expr::Binary(l, op, r) => {
                let local_l = self.expr(l)?;
                let local_r = self.expr(r)?;
                let int = self.x.ty(Type::Int32);
                let ty_l = self.x.ir.locals[local_l];
                let ty_r = self.x.ir.locals[local_r];
                match (
                    self.x.ir.types[self.x.resolve(ty_l).index()],
                    self.x.ir.types[self.x.resolve(ty_r).index()],
                ) {
                    (Type::Int32, Type::Int32) => {
                        match op {
                            parse::Binop::Add => Ok(self
                                .instr(int, Instr::Int32Arith(local_l, Int32Arith::Add, local_r))),
                            parse::Binop::Sub => Ok(self
                                .instr(int, Instr::Int32Arith(local_l, Int32Arith::Sub, local_r))),
                            parse::Binop::Mul => Ok(self
                                .instr(int, Instr::Int32Arith(local_l, Int32Arith::Mul, local_r))),
                            parse::Binop::Div => Ok(self
                                .instr(int, Instr::Int32Arith(local_l, Int32Arith::Div, local_r))),
                            parse::Binop::Rem => Ok(self
                                .instr(int, Instr::Int32Arith(local_l, Int32Arith::Rem, local_r))),
                            parse::Binop::Eq => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self
                                    .instr(bool, Instr::Int32Comp(local_l, Int32Comp::Eq, local_r)))
                            }
                            parse::Binop::Neq => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self.instr(
                                    bool,
                                    Instr::Int32Comp(local_l, Int32Comp::Neq, local_r),
                                ))
                            }
                            parse::Binop::Lt => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self
                                    .instr(bool, Instr::Int32Comp(local_l, Int32Comp::Lt, local_r)))
                            }
                            parse::Binop::Gt => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self
                                    .instr(bool, Instr::Int32Comp(local_l, Int32Comp::Gt, local_r)))
                            }
                            parse::Binop::Leq => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self.instr(
                                    bool,
                                    Instr::Int32Comp(local_l, Int32Comp::Leq, local_r),
                                ))
                            }
                            parse::Binop::Geq => {
                                let bool = self.x.ty(Type::Bool);
                                Ok(self.instr(
                                    bool,
                                    Instr::Int32Comp(local_l, Int32Comp::Geq, local_r),
                                ))
                            }
                        }
                    }
                    (Type::Int32, _) => Err(LowerError::ExpectedType(int, r, ty_r)),
                    _ => Err(LowerError::ExpectedType(int, l, ty_l)),
                }
            }
            Expr::If(cond, yes, no) => {
                let unit = self.x.ty_unit();
                let cond = self.expr(cond)?;
                // We don't know the type yet.
                let start = self.instr(unit, Instr::If(cond, unit));
                let mut local = self.block(yes)?;
                // Rewrite the original instruction now that we know the type.
                let ty = self.x.ir.locals[local];
                self.x.ir.instrs[start] = Instr::If(cond, ty);
                if let Some(block) = no {
                    self.instr(ty, Instr::Else(local));
                    local = self.block(block)?;
                }
                Ok(self.instr(ty, Instr::EndIf(local)))
            }
        }
    }

    fn stmts(&mut self, stmts: IdRange<StmtId>) -> LowerResult<()> {
        for stmt in stmts {
            match self.x.tree.stmts[stmt] {
                Stmt::Let(name, rhs) => {
                    let local = self.expr(rhs)?;
                    let ty = self.x.ir.locals[local];
                    let copy = self.instr(ty, Instr::Copy(local));
                    self.set(name, copy);
                }
                Stmt::Var(name, rhs) => {
                    let local = self.expr(rhs)?;
                    let ty = self.x.ir.locals[local];
                    let copy = self.instr(ty, Instr::Copy(local));
                    self.set(name, copy);
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
                    self.instr(unit, Instr::If(local, unit));
                    self.stmts(body.stmts)?;
                    let fake = self.instr(unit, Instr::Br(Depth(1)));
                    self.instr(unit, Instr::EndIf(fake));
                    self.instr(unit, Instr::EndLoop);
                }
                Stmt::Expr(expr) => {
                    self.expr(expr)?;
                }
            }
        }
        Ok(())
    }

    fn block(&mut self, block: Block) -> LowerResult<LocalId> {
        self.stmts(block.stmts)?;
        match block.expr {
            Some(expr) => Ok(self.expr(expr)?),
            None => {
                let ty = self.x.ty_unit();
                Ok(self.instr_tuple(ty, &[]))
            }
        }
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
        let mut types = self.x.ir.types[tuple_ty.index()].tuple().into_iter();
        let mut index = 0;
        if let Some(structdef) = self.ty {
            // Because methods just have their object as the first parameter, we need to skip past
            // it so that the rest of the tuple aligns properly with the original parsed parameters.
            types.next();
            let ty = self.x.ty(Type::Structdef(structdef));
            self.this = Some(self.instr(ty, Instr::Elem(tuple_local, ElemId::new(index))));
            index += 1;
        }
        for (param, tuple_loc) in params.into_iter().zip(types) {
            let ty = self.x.ir.tuples[tuple_loc];
            let local = self.instr(ty, Instr::Elem(tuple_local, ElemId::new(index)));
            let name = self.x.tree.params[param].name;
            self.set(name, local);
            index += 1;
        }
        let ret = self.block(body)?;
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
