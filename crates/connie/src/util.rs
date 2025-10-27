use std::hash::{Hash, Hasher};

use index_vec::{Idx, IndexSlice, IndexVec};

#[derive(Clone, Copy, Debug)]
pub struct IdRange<I> {
    pub start: I,
    pub end: I,
}

impl<I: Idx> IdRange<I> {
    pub fn new<T>(v: &mut IndexVec<I, T>, xs: Vec<T>) -> Self {
        let start = v.next_idx();
        v.append(&mut IndexVec::from_vec(xs));
        let end = v.next_idx();
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.end.index() - self.start.index()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn get<'a, T>(&self, xs: &'a IndexSlice<I, [T]>) -> &'a [T] {
        xs[self.start..self.end].as_raw_slice()
    }

    pub fn first(&self) -> Option<I> {
        if self.is_empty() {
            None
        } else {
            Some(self.start)
        }
    }

    pub fn last(&self) -> Option<I> {
        if self.is_empty() {
            None
        } else {
            Some(I::from_usize(self.end.index() - 1))
        }
    }
}

impl<I: Ord> PartialEq for IdRange<I> {
    fn eq(&self, other: &Self) -> bool {
        // All empty ranges are considered equal.
        match (self.start < self.end, other.start < other.end) {
            (false, false) => true,
            (true, false) | (false, true) => false,
            (true, true) => self.start == other.start && self.end == other.end,
        }
    }
}

impl<I: Ord> Eq for IdRange<I> {}

impl<I: Idx> Hash for IdRange<I> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let IdRange { mut start, mut end } = *self;
        // All empty ranges are considered equal.
        if start >= end {
            start = I::from_usize(0);
            end = I::from_usize(0);
        }
        start.hash(state);
        end.hash(state);
    }
}

impl<I: Idx> IntoIterator for IdRange<I> {
    type Item = I;

    type IntoIter = IdRangeIter<I>;

    fn into_iter(self) -> Self::IntoIter {
        IdRangeIter(self)
    }
}

pub struct IdRangeIter<I>(IdRange<I>);

impl<I: Idx> Iterator for IdRangeIter<I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        let IdRange { start, end } = self.0;
        if start < end {
            self.0.start = I::from_usize(start.index() + 1);
            Some(start)
        } else {
            None
        }
    }
}

impl<I: Idx> DoubleEndedIterator for IdRangeIter<I> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let IdRange { start, end } = self.0;
        if start < end {
            self.0.end = I::from_usize(end.index() - 1);
            Some(self.0.end)
        } else {
            None
        }
    }
}
