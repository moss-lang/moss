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
    strings: IndexMap<(StrLoc, StrLoc), ()>,
}

impl Strings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn make(&mut self, string: &str) -> StrId {
        let mut state = DefaultHasher::new();
        string.hash(&mut state);
        let hash = state.finish();
        let entry = self
            .strings
            .raw_entry_mut_v1()
            .from_hash(hash, |&(i, j)| string == &self.data[i.index()..j.index()]);
        let id = StrId::new(entry.index());
        if let RawEntryMut::Vacant(vacant) = entry {
            let i = StrLoc::from_usize(self.data.len());
            self.data.push_str(string);
            let j = StrLoc::from_usize(self.data.len());
            vacant.insert_hashed_nocheck(hash, (i, j), ());
        }
        id
    }

    pub fn get(&self, string: &str) -> Option<StrId> {
        let mut state = DefaultHasher::new();
        string.hash(&mut state);
        let hash = state.finish();
        self.strings
            .raw_entry_v1()
            .index_from_hash(hash, |&(i, j)| string == &self.data[i.index()..j.index()])
            .map(StrId::from_usize)
    }
}

impl Index<StrId> for Strings {
    type Output = str;

    fn index(&self, id: StrId) -> &str {
        let (&(i, j), _) = self.strings.get_index(id.index()).unwrap();
        &self.data[i.index()..j.index()]
    }
}

#[cfg(test)]
mod tests {
    use crate::intern::Strings;

    #[test]
    fn test_same() {
        let mut strings = Strings::new();
        let a = strings.make("foo");
        let b = strings.make("foo");
        assert_eq!(a, b);
    }

    #[test]
    fn test_different() {
        let mut strings = Strings::new();
        let a = strings.make("foo");
        let b = strings.make("bar");
        assert_ne!(a, b);
    }

    #[test]
    fn test_missing() {
        let strings = Strings::new();
        let a = strings.get("foo");
        assert_eq!(a, None);
    }

    #[test]
    fn test_present() {
        let mut strings = Strings::new();
        let a = strings.make("foo");
        let b = strings.get("foo");
        assert_eq!(Some(a), b);
    }
}
