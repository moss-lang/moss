use std::hash::{BuildHasherDefault, Hash, Hasher};

use index_vec::{Idx, IndexSlice, IndexVec};

/// A fast, non-cryptographic hasher (the `FxHash` algorithm used by `rustc` and Firefox).
///
/// The IR is full of small integer keys (node ids, levels, string ids) that get hashed
/// constantly during lowering; the standard-library `SipHash` is far stronger than we need for
/// internal hash-consing, so we use this instead.
#[derive(Default)]
pub struct FxHasher {
    hash: usize,
}

// Constant from `rustc-hash`: an odd 64-bit multiplier with a good bit-mixing pattern.
const FX_SEED: usize = 0x51_7c_c1_b7_27_22_0a_95;

impl FxHasher {
    #[inline]
    fn add(&mut self, i: usize) {
        self.hash = (self.hash.rotate_left(5) ^ i).wrapping_mul(FX_SEED);
    }
}

impl Hasher for FxHasher {
    #[inline]
    fn write(&mut self, mut bytes: &[u8]) {
        while bytes.len() >= 8 {
            self.add(usize::from_ne_bytes(bytes[..8].try_into().unwrap()));
            bytes = &bytes[8..];
        }
        if bytes.len() >= 4 {
            self.add(u32::from_ne_bytes(bytes[..4].try_into().unwrap()) as usize);
            bytes = &bytes[4..];
        }
        if bytes.len() >= 2 {
            self.add(u16::from_ne_bytes(bytes[..2].try_into().unwrap()) as usize);
            bytes = &bytes[2..];
        }
        if let Some(&b) = bytes.first() {
            self.add(b as usize);
        }
    }

    #[inline]
    fn write_u8(&mut self, i: u8) {
        self.add(i as usize);
    }

    #[inline]
    fn write_u16(&mut self, i: u16) {
        self.add(i as usize);
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.add(i as usize);
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.add(i as usize);
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.add(i);
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.hash as u64
    }
}

/// A [`std::hash::BuildHasher`] for [`FxHasher`], for use with `HashMap`/`IndexSet` etc.
pub type FxBuildHasher = BuildHasherDefault<FxHasher>;

pub fn default_hash(x: &impl Hash) -> u64 {
    let mut state = FxHasher::default();
    x.hash(&mut state);
    state.finish()
}

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
        self.end.index().saturating_sub(self.start.index())
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
