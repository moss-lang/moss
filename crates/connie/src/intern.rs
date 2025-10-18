use std::{
    hash::{DefaultHasher, Hash, Hasher},
    ops::Index,
};

use index_vec::define_index_type;
use indexmap::{
    IndexMap,
    map::{RawEntryApiV1, raw_entry_v1::RawEntryMut},
};

define_index_type! {
    pub struct StrId = u32;
}

define_index_type! {
    struct StrLoc = u32;
}

#[derive(Debug, Default)]
pub struct Strings {
    data: String,
    names: IndexMap<(StrLoc, StrLoc), ()>,
}

impl Strings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn make(&mut self, name: &str) -> StrId {
        let mut state = DefaultHasher::new();
        name.hash(&mut state);
        let hash = state.finish();
        let entry = self
            .names
            .raw_entry_mut_v1()
            .from_hash(hash, |&(i, j)| name == &self.data[i.index()..j.index()]);
        let id = StrId::new(entry.index());
        if let RawEntryMut::Vacant(vacant) = entry {
            let i = StrLoc::from_usize(self.data.len());
            self.data.push_str(name);
            let j = StrLoc::from_usize(self.data.len());
            vacant.insert_hashed_nocheck(hash, (i, j), ());
        }
        id
    }
}

impl Index<StrId> for Strings {
    type Output = str;

    fn index(&self, id: StrId) -> &str {
        let (&(i, j), _) = self.names.get_index(id.index()).unwrap();
        &self.data[i.index()..j.index()]
    }
}

#[cfg(test)]
mod tests {
    use crate::intern::Strings;

    #[test]
    fn test_same() {
        let mut names = Strings::new();
        let a = names.make("foo");
        let b = names.make("foo");
        assert_eq!(a, b);
    }

    #[test]
    fn test_different() {
        let mut names = Strings::new();
        let a = names.make("foo");
        let b = names.make("bar");
        assert_ne!(a, b);
    }
}
