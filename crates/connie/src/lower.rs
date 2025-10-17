use index_vec::{IndexVec, define_index_type};

use crate::{
    lex::{TokenId, TokenStarts},
    parse::Tree,
};

define_index_type! {
    pub struct PathId = u32;
}

pub type Path = Option<(PathId, TokenId)>;

#[derive(Debug)]
pub struct IR {
    paths: IndexVec<PathId, Path>,
}

pub enum LowerError {}

type LowerResult<T> = Result<T, LowerError>;

pub fn lower(_: &str, _: &TokenStarts, _: &Tree) -> LowerResult<IR> {
    Ok(IR {
        paths: IndexVec::new(),
    })
}
