use std::{
    hash::{DefaultHasher, Hash, Hasher},
    ops::Index,
};

use index_vec::{IndexSlice, IndexVec, define_index_type};
use indexmap::{
    IndexMap,
    map::{RawEntryApiV1, raw_entry_v1::RawEntryMut},
};

use crate::util::IdRange;

define_index_type! {
    pub struct TupleId = u32;
}

define_index_type! {
    pub struct TupleLoc = u32;
}

pub type TupleRange = IdRange<TupleLoc>;

#[derive(Debug)]
pub struct Tuples<T> {
    data: IndexVec<TupleLoc, T>,
    tuples: IndexMap<TupleRange, ()>,
}

impl<T> Default for Tuples<T> {
    fn default() -> Self {
        Self {
            data: IndexVec::new(),
            tuples: IndexMap::new(),
        }
    }
}

impl<T> Tuples<T> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn count(&self) -> usize {
        self.data.len()
    }

    pub fn id_of(&self, range: TupleRange) -> TupleId {
        TupleId::from_usize(self.tuples.get_index_of(&range).unwrap())
    }

    pub fn range_of(&self, id: TupleId) -> TupleRange {
        let (&range, _) = self.tuples.get_index(id.index()).unwrap();
        range
    }

    pub fn ranges(&self) -> impl Iterator<Item = TupleRange> {
        self.tuples.keys().copied()
    }
}

impl<T: Eq + Hash> Tuples<T> {
    fn get_full(&self, tuple: &[T]) -> Option<(TupleId, TupleRange)> {
        let mut state = DefaultHasher::new();
        tuple.hash(&mut state);
        let hash = state.finish();
        self.tuples
            .raw_entry_v1()
            .from_hash_full(hash, |&TupleRange { start, end }| {
                tuple == self.data[start..end].as_raw_slice()
            })
            .map(|(index, &range, ())| (TupleId::from_usize(index), range))
    }

    pub fn get_id(&self, tuple: &[T]) -> Option<TupleId> {
        self.get_full(tuple).map(|(id, _)| id)
    }

    pub fn get(&self, tuple: &[T]) -> Option<TupleRange> {
        self.get_full(tuple).map(|(_, range)| range)
    }
}

impl<T: Clone + Eq + Hash> Tuples<T> {
    fn make_full(&mut self, tuple: &[T]) -> (TupleId, TupleRange) {
        let mut state = DefaultHasher::new();
        tuple.hash(&mut state);
        let hash = state.finish();
        match self
            .tuples
            .raw_entry_mut_v1()
            .from_hash(hash, |&TupleRange { start, end }| {
                tuple == self.data[start..end].as_raw_slice()
            }) {
            RawEntryMut::Occupied(entry) => (TupleId::new(entry.index()), *entry.key()),
            RawEntryMut::Vacant(entry) => {
                let id = TupleId::new(entry.index());
                let start = self.data.len_idx();
                self.data.extend_from_slice(IndexSlice::new(tuple));
                let end = self.data.len_idx();
                let range = TupleRange { start, end };
                entry.insert_hashed_nocheck(hash, range, ());
                (id, range)
            }
        }
    }

    pub fn make_id(&mut self, tuple: &[T]) -> TupleId {
        let (id, _) = self.make_full(tuple);
        id
    }

    pub fn make(&mut self, tuple: &[T]) -> TupleRange {
        let (_, range) = self.make_full(tuple);
        range
    }
}

impl<T> Index<TupleLoc> for Tuples<T> {
    type Output = T;

    fn index(&self, id: TupleLoc) -> &T {
        &self.data[id]
    }
}

impl<T> Index<TupleRange> for Tuples<T> {
    type Output = [T];

    fn index(&self, TupleRange { start, end }: TupleRange) -> &[T] {
        self.data[start..end].as_raw_slice()
    }
}

impl<T> Index<TupleId> for Tuples<T> {
    type Output = [T];

    fn index(&self, id: TupleId) -> &[T] {
        &self[self.range_of(id)]
    }
}

#[cfg(test)]
mod tests {
    use crate::tuples::Tuples;

    #[test]
    fn test_same() {
        let mut tuples = Tuples::new();
        let a = tuples.make(&['x', 'y']);
        let b = tuples.make(&['x', 'y']);
        assert_eq!(a, b);
    }

    #[test]
    fn test_different() {
        let mut tuples = Tuples::new();
        let a = tuples.make(&['x', 'y']);
        let b = tuples.make(&['z', 'w']);
        assert_ne!(a, b);
    }

    #[test]
    fn test_missing() {
        let tuples = Tuples::new();
        let a = tuples.get(&['x', 'y']);
        assert_eq!(a, None);
    }

    #[test]
    fn test_present() {
        let mut tuples = Tuples::new();
        let a = tuples.make(&['x', 'y']);
        let b = tuples.get(&['x', 'y']);
        assert_eq!(Some(a), b);
    }
}
