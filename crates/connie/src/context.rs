use std::{
    hash::{Hash, Hasher},
    ops::{AddAssign, BitXorAssign, Index, SubAssign},
};

use index_vec::define_index_type;
use indexmap::IndexSet;

use crate::{
    lower::{FndefId, IR, TydefId, TypeId, ValdefId},
    tuples::{TupleLoc, TupleRange, Tuples},
    util::default_hash,
};

define_index_type! {
    pub struct ContextId = u32;
}

define_index_type! {
    pub struct TyId = u32;
}

define_index_type! {
    pub struct BuiltinId = u32;
}

define_index_type! {
    pub struct FnId = u32;
}

define_index_type! {
    pub struct ValId = u32;
}

#[derive(Clone, Copy, Debug, Default, Eq, Hash, PartialEq)]
struct UnorderedHash(u64);

impl AddAssign<u64> for UnorderedHash {
    fn add_assign(&mut self, rhs: u64) {
        self.0.bitxor_assign(rhs);
    }
}

impl SubAssign<u64> for UnorderedHash {
    fn sub_assign(&mut self, rhs: u64) {
        self.0.bitxor_assign(rhs);
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Context {
    tydefs: im_rc::HashMap<TydefId, TyId>,
    fndefs: im_rc::HashMap<FndefId, FnId>,
    valdefs: im_rc::HashMap<ValdefId, ValId>,
    hash_tydefs: UnorderedHash,
    hash_fndefs: UnorderedHash,
    hash_valdefs: UnorderedHash,
}

impl Hash for Context {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_tydefs.hash(state);
        self.hash_fndefs.hash(state);
        self.hash_valdefs.hash(state);
    }
}

impl Context {
    pub fn set_ty(&mut self, tydef: TydefId, t: TyId) {
        if let Some(prev) = self.tydefs.insert(tydef, t) {
            self.hash_tydefs -= default_hash(&(tydef, prev));
        }
        self.hash_tydefs += default_hash(&(tydef, t));
    }

    pub fn set_fn(&mut self, fndef: FndefId, f: FnId) {
        if let Some(prev) = self.fndefs.insert(fndef, f) {
            self.hash_fndefs -= default_hash(&(fndef, prev));
        }
        self.hash_fndefs += default_hash(&(fndef, f));
    }

    pub fn set_val(&mut self, valdef: ValdefId, v: ValId) {
        if let Some(prev) = self.valdefs.insert(valdef, v) {
            self.hash_valdefs -= default_hash(&(valdef, prev));
        }
        self.hash_valdefs += default_hash(&(valdef, v));
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Ty {
    String,
    Bool,
    Int32,
    Tuple(TupleRange),
}

impl Ty {
    pub fn tuple(self) -> TupleRange {
        match self {
            Ty::Tuple(range) => range,
            _ => panic!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Fn {
    Builtin(BuiltinId),
    Fndef(ContextId, FndefId),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Val {
    Int32(i32),
    Dynamic(ValdefId, TyId),
}

#[derive(Debug)]
pub struct Cache<'a> {
    ir: &'a IR,
    contexts: IndexSet<Context>,
    tuples: Tuples<TyId>,
    tys: IndexSet<Ty>,
    fns: IndexSet<Fn>,
    vals: IndexSet<Val>,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        let mut contexts = IndexSet::new();
        contexts.insert(Context::default());
        Self {
            ir,
            contexts,
            tuples: Tuples::new(),
            tys: IndexSet::new(),
            fns: IndexSet::new(),
            vals: IndexSet::new(),
        }
    }

    pub fn empty(&self) -> ContextId {
        ContextId::new(0)
    }

    pub fn make_ctx(&mut self, context: Context) -> ContextId {
        let (i, _) = self.contexts.insert_full(context);
        ContextId::from_usize(i)
    }

    pub fn next_fn(&self) -> FnId {
        FnId::from_usize(self.fns.len())
    }

    fn make_ty(&mut self, t: Ty) -> TyId {
        let (i, _) = self.tys.insert_full(t);
        TyId::from_usize(i)
    }

    fn make_fn(&mut self, f: Fn) -> FnId {
        let (i, _) = self.fns.insert_full(f);
        FnId::from_usize(i)
    }

    fn make_val(&mut self, v: Val) -> ValId {
        let (i, _) = self.vals.insert_full(v);
        ValId::from_usize(i)
    }

    pub fn ty_string(&mut self) -> TyId {
        self.make_ty(Ty::String)
    }

    pub fn ty_bool(&mut self) -> TyId {
        self.make_ty(Ty::Bool)
    }

    pub fn ty_int32(&mut self) -> TyId {
        self.make_ty(Ty::Int32)
    }

    pub fn ty_tuple(&mut self, elems: &[TyId]) -> TyId {
        let tuple = self.tuples.make(elems);
        self.make_ty(Ty::Tuple(tuple))
    }

    pub fn ty_unit(&mut self) -> TyId {
        self.ty_tuple(&[])
    }

    pub fn fn_builtin(&mut self, builtin: BuiltinId) -> FnId {
        self.make_fn(Fn::Builtin(builtin))
    }

    pub fn fn_fndef(&mut self, ctx: ContextId, fndef: FndefId) -> FnId {
        self.make_fn(Fn::Fndef(ctx, fndef))
    }

    pub fn val_dynamic(&mut self, valdef: ValdefId, ty: TyId) -> ValId {
        self.make_val(Val::Dynamic(valdef, ty))
    }

    pub fn ty(&mut self, _ctx: ContextId, _ty: TypeId) -> TyId {
        todo!()
    }

    pub fn fndef(&mut self, _ctx: ContextId, _fndef: FndefId) -> FnId {
        todo!()
    }

    pub fn valdef(&mut self, _ctx: ContextId, _valdef: ValdefId) -> ValId {
        todo!()
    }
}

impl<'a> Index<ContextId> for Cache<'a> {
    type Output = Context;

    fn index(&self, id: ContextId) -> &Context {
        &self.contexts[id.index()]
    }
}

impl<'a> Index<TupleLoc> for Cache<'a> {
    type Output = TyId;

    fn index(&self, loc: TupleLoc) -> &TyId {
        &self.tuples[loc]
    }
}

impl<'a> Index<TupleRange> for Cache<'a> {
    type Output = [TyId];

    fn index(&self, range: TupleRange) -> &[TyId] {
        &self.tuples[range]
    }
}

impl<'a> Index<TyId> for Cache<'a> {
    type Output = Ty;

    fn index(&self, t: TyId) -> &Ty {
        &self.tys[t.index()]
    }
}

impl<'a> Index<FnId> for Cache<'a> {
    type Output = Fn;

    fn index(&self, f: FnId) -> &Fn {
        &self.fns[f.index()]
    }
}

impl<'a> Index<ValId> for Cache<'a> {
    type Output = Val;

    fn index(&self, v: ValId) -> &Val {
        &self.vals[v.index()]
    }
}
