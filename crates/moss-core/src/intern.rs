use std::ops::Index;

use index_vec::define_index_type;
use indexmap::{
    IndexMap,
    map::{RawEntryApiV1, raw_entry_v1::RawEntryMut},
};

use crate::util::{IdRange, default_hash};

define_index_type! {
    pub struct StrId = u32;
}

define_index_type! {
    pub struct StrLoc = u32;
}

pub type StrRange = IdRange<StrLoc>;

#[derive(Debug, Default)]
pub struct Strings {
    data: String,
    strings: IndexMap<StrRange, ()>,
}

impl Strings {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn id_of(&self, range: StrRange) -> StrId {
        StrId::from_usize(self.strings.get_index_of(&range).unwrap())
    }

    pub fn range_of(&self, id: StrId) -> StrRange {
        let (&range, _) = self.strings.get_index(id.index()).unwrap();
        range
    }

    pub fn ranges(&self) -> impl Iterator<Item = StrRange> {
        self.strings.keys().copied()
    }

    fn get_full(&self, string: &str) -> Option<(StrId, StrRange)> {
        let hash = default_hash(&string);
        self.strings
            .raw_entry_v1()
            .from_hash_full(hash, |&StrRange { start, end }| {
                string == &self.data[start.index()..end.index()]
            })
            .map(|(index, &range, ())| (StrId::from_usize(index), range))
    }

    pub fn get_id(&self, string: &str) -> Option<StrId> {
        self.get_full(string).map(|(id, _)| id)
    }

    pub fn get(&self, string: &str) -> Option<StrRange> {
        self.get_full(string).map(|(_, range)| range)
    }

    fn make_full(&mut self, string: &str) -> (StrId, StrRange) {
        let hash = default_hash(&string);
        match self
            .strings
            .raw_entry_mut_v1()
            .from_hash(hash, |&StrRange { start, end }| {
                string == &self.data[start.index()..end.index()]
            }) {
            RawEntryMut::Occupied(entry) => (StrId::new(entry.index()), *entry.key()),
            RawEntryMut::Vacant(entry) => {
                let id = StrId::new(entry.index());
                let start = StrLoc::from_usize(self.data.len());
                self.data.push_str(string);
                let end = StrLoc::from_usize(self.data.len());
                let range = StrRange { start, end };
                entry.insert_hashed_nocheck(hash, range, ());
                (id, range)
            }
        }
    }

    pub fn make_id(&mut self, string: &str) -> StrId {
        let (id, _) = self.make_full(string);
        id
    }

    pub fn make(&mut self, string: &str) -> StrRange {
        let (_, range) = self.make_full(string);
        range
    }
}

impl Index<StrRange> for Strings {
    type Output = str;

    fn index(&self, StrRange { start, end }: StrRange) -> &str {
        &self.data[start.index()..end.index()]
    }
}

impl Index<StrId> for Strings {
    type Output = str;

    fn index(&self, id: StrId) -> &str {
        &self[self.range_of(id)]
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
