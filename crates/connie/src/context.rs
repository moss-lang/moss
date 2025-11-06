use index_vec::define_index_type;

use crate::lower::{FndefId, IR, TydefId, TypeId, ValdefId};

define_index_type! {
    pub struct ContextId = u32;
}

define_index_type! {
    pub struct TyId = u32;
}

define_index_type! {
    pub struct FnId = u32;
}

define_index_type! {
    pub struct ValId = u32;
}

pub enum Constant {
    Int32(i32),
}

pub struct Cache<'a> {
    ir: &'a IR,
}

impl<'a> Cache<'a> {
    pub fn new(ir: &'a IR) -> Self {
        Self { ir }
    }

    pub fn empty(&self) -> ContextId {
        todo!()
    }

    pub fn bind_ty(&mut self, _ctx: ContextId, _def: TydefId, _bind: TypeId) -> ContextId {
        todo!()
    }

    pub fn bind_fn(&mut self, _ctx: ContextId, _def: FndefId, _bind: FndefId) -> ContextId {
        todo!()
    }

    pub fn bind_val(
        &mut self,
        _ctx: ContextId,
        _def: ValdefId,
        _bind: Option<Constant>,
    ) -> ContextId {
        todo!()
    }

    pub fn bound_ty(&self, _ctx: ContextId, _def: TydefId) -> TypeId {
        todo!()
    }

    pub fn bound_fn(&self, _ctx: ContextId, _def: FndefId) -> FndefId {
        todo!()
    }

    pub fn bound_val(&self, _ctx: ContextId, _def: ValdefId) -> Option<Constant> {
        todo!()
    }

    pub fn index_ty(&mut self, _ctx: ContextId, _ty: TypeId) -> TyId {
        todo!()
    }

    pub fn index_fn(&mut self, _ctx: ContextId, _fndef: FndefId) -> FnId {
        todo!()
    }

    pub fn index_val(&mut self, _ctx: ContextId, _valdef: ValdefId) -> ValId {
        todo!()
    }
}
