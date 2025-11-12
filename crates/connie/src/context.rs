use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
    ops::{AddAssign, BitXorAssign, Index, SubAssign},
};

use index_vec::define_index_type;
use indexmap::IndexSet;

use crate::{
    lower::{FndefId, IR, StructdefId, TydefId, Type, TypeId, ValdefId},
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
    Structdef(StructdefId, TupleRange),
}

impl Ty {
    pub fn tuple(self) -> TupleRange {
        match self {
            Ty::Tuple(elems) => elems,
            _ => panic!(),
        }
    }

    pub fn structdef(self) -> TupleRange {
        match self {
            Ty::Structdef(_, fields) => fields,
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
    tydefs: Vec<u8>,
    tuples: Tuples<TyId>,
    tys: IndexSet<Ty>,
    fns: IndexSet<Fn>,
    vals: IndexSet<Val>,
    types: HashMap<(ContextId, TypeId), TyId>,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        let mut contexts = IndexSet::new();
        contexts.insert(Context::default());
        let mut tydefs = Vec::new();
        assert!(ir.tydefs.len() <= 8); // TODO: Handle larger bitsets.
        for &ty in &ir.types {
            let bitset = match ty {
                Type::String | Type::Bool | Type::Int32 => 0,
                Type::Tuple(elems) => {
                    let mut bitset = 0;
                    for elem in &ir.tuples[elems] {
                        bitset |= tydefs[elem.index()];
                    }
                    bitset
                }
                Type::Tydef(tydef) => match ir.tydefs[tydef].def {
                    Some(_) => 0,
                    None => 1 << tydef.index(),
                },
                Type::Structdef(structdef) => {
                    let mut bitset = 0;
                    for (_, field) in &ir.fields[ir.structdefs[structdef].fields] {
                        bitset |= tydefs[field.index()];
                    }
                    bitset
                }
            };
            tydefs.push(bitset);
        }
        Self {
            ir,
            contexts,
            tydefs,
            tuples: Tuples::new(),
            tys: IndexSet::new(),
            fns: IndexSet::new(),
            vals: IndexSet::new(),
            types: HashMap::new(),
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

    pub fn ty_structdef(&mut self, structdef: StructdefId, elems: &[TyId]) -> TyId {
        let tuple = self.tuples.make(elems);
        self.make_ty(Ty::Structdef(structdef, tuple))
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

    fn ty_prune(&mut self, ctx: ContextId, ty: TypeId) -> ContextId {
        let mut subcontext = Context::default();
        let context = &self[ctx];
        let mut bitset = self.tydefs[ty.index()];
        while let i = bitset.trailing_zeros()
            && i < self.ir.tydefs.len_idx().raw()
        {
            let tydef = TydefId::from_raw(i);
            subcontext.set_ty(tydef, context.tydefs[&tydef]);
            bitset &= !(1 << i);
        }
        self.make_ctx(subcontext)
    }

    fn ty_pruned(&mut self, ctx: ContextId, ty: TypeId) -> TyId {
        if let Some(&t) = self.types.get(&(ctx, ty)) {
            return t;
        }
        // TODO: Make this not susceptible to stack overflows.
        let t = match self.ir.types[ty.index()] {
            Type::String => self.ty_string(),
            Type::Bool => self.ty_bool(),
            Type::Int32 => self.ty_int32(),
            Type::Tuple(elems) => {
                let types = self.ir.tuples[elems]
                    .iter()
                    .map(|&elem| self.ty_pruned(ctx, elem))
                    .collect::<Vec<TyId>>();
                self.ty_tuple(&types)
            }
            Type::Tydef(tydef) => match self.ir.tydefs[tydef].def {
                Some(def) => self.ty_pruned(ctx, def),
                None => self.contexts[ctx.index()].tydefs[&tydef],
            },
            Type::Structdef(structdef) => {
                let types = self.ir.fields[self.ir.structdefs[structdef].fields]
                    .iter()
                    .map(|&(_, field)| self.ty_pruned(ctx, field))
                    .collect::<Vec<TyId>>();
                self.ty_structdef(structdef, &types)
            }
        };
        self.types.insert((ctx, ty), t);
        t
    }

    pub fn ty(&mut self, ctx: ContextId, ty: TypeId) -> TyId {
        if let Some(&t) = self.types.get(&(ctx, ty)) {
            return t;
        }
        let ctx2 = self.ty_prune(ctx, ty);
        let t = self.ty_pruned(ctx2, ty);
        self.types.insert((ctx, ty), t);
        t
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
