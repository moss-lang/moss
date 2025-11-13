use std::{marker::PhantomData, ops::BitOrAssign};

use index_vec::Idx;

type Chunk = u128;

const CHUNK_BITS: usize = Chunk::BITS as usize;

fn chunks_per_set(len: impl Idx) -> usize {
    len.index().div_ceil(CHUNK_BITS)
}

#[derive(Clone, Copy)]
pub struct Subset<'a, I> {
    len: I,
    bits: &'a [Chunk],
}

impl<'a, I: Idx> IntoIterator for Subset<'a, I> {
    type Item = I;

    type IntoIter = SubsetIter<'a, I>;

    fn into_iter(self) -> Self::IntoIter {
        let (&chunk, rest) = self.bits.split_first().unwrap_or((&0, &[]));
        SubsetIter {
            _i: PhantomData,
            offset: 0,
            chunk,
            rest,
        }
    }
}

pub struct SubsetIter<'a, I> {
    _i: PhantomData<I>,
    offset: usize,
    chunk: Chunk,
    rest: &'a [Chunk],
}

impl<'a, I: Idx> Iterator for SubsetIter<'a, I> {
    type Item = I;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let bit = self.chunk.trailing_zeros();
            if bit < Chunk::BITS {
                self.chunk &= !(1 << bit);
                return Some(I::from_usize(self.offset + bit as usize));
            }
            let (&chunk, rest) = self.rest.split_first()?;
            self.offset += CHUNK_BITS;
            self.chunk = chunk;
            self.rest = rest;
        }
    }
}

pub struct SubsetMut<'a, I> {
    len: I,
    bits: &'a mut [Chunk],
}

impl<'a, I: Idx> SubsetMut<'a, I> {
    pub fn include(&mut self, i: I) {
        assert!(i < self.len);
        let index = i.index();
        self.bits[index / CHUNK_BITS] |= 1 << (index % CHUNK_BITS);
    }
}

impl<'a, I: Idx> BitOrAssign<Subset<'a, I>> for SubsetMut<'a, I> {
    fn bitor_assign(&mut self, rhs: Subset<'a, I>) {
        assert_eq!(self.len, rhs.len);
        for (left, &right) in self.bits.iter_mut().zip(rhs.bits) {
            *left |= right;
        }
    }
}

pub struct SubsetsView<'a, I, J> {
    _j: PhantomData<J>,
    len: I,
    left: &'a [Chunk],
    gap: usize,
    right: &'a [Chunk],
}

impl<'a, I: Idx, J> SubsetsView<'a, I, J> {
    fn chunks_per_set(&self) -> usize {
        chunks_per_set(self.len)
    }
}

impl<'a, I: Idx, J: Idx> SubsetsView<'a, I, J> {
    pub fn get(&self, j: J) -> Subset<'a, I> {
        let stride = self.chunks_per_set();
        let start = j.index() * stride;
        Subset {
            len: self.len,
            bits: match start.checked_sub(self.left.len() + self.gap) {
                None => &self.left[start..start + stride],
                Some(delta) => &self.right[delta..delta + stride],
            },
        }
    }
}

#[derive(Debug)]
pub struct Subsets<I, J> {
    _j: PhantomData<J>,
    len: I,
    bits: Vec<Chunk>,
}

impl<I, J> Subsets<I, J> {
    pub fn new(len: I) -> Self {
        Self {
            _j: PhantomData,
            len,
            bits: Vec::new(),
        }
    }
}

impl<I: Idx, J> Subsets<I, J> {
    fn view(&'_ self) -> SubsetsView<'_, I, J> {
        SubsetsView {
            _j: PhantomData,
            len: self.len,
            left: &self.bits,
            gap: 0,
            right: &[],
        }
    }

    fn chunks_per_set(&self) -> usize {
        self.view().chunks_per_set()
    }

    pub fn push(&'_ mut self) -> (SubsetsView<'_, I, J>, SubsetMut<'_, I>) {
        let split = self.bits.len();
        for _ in 0..self.chunks_per_set() {
            self.bits.push(0);
        }
        let len = self.len;
        let (left, bits) = self.bits.split_at_mut(split);
        let view = SubsetsView {
            _j: PhantomData,
            len,
            left,
            gap: 0,
            right: &[],
        };
        (view, SubsetMut { len, bits })
    }
}

impl<I: Idx, J: Idx> Subsets<I, J> {
    pub fn get(&'_ self, j: J) -> Subset<'_, I> {
        self.view().get(j)
    }

    pub fn get_mut(&'_ mut self, j: J) -> (SubsetsView<'_, I, J>, SubsetMut<'_, I>) {
        let gap = self.chunks_per_set();
        let len = self.len;
        let (left, rest) = self.bits.split_at_mut(j.index() * gap);
        let (bits, right) = rest.split_at_mut(gap);
        let view = SubsetsView {
            _j: PhantomData,
            len,
            left,
            gap,
            right,
        };
        (view, SubsetMut { len, bits })
    }
}

#[cfg(test)]
mod tests {
    use index_vec::define_index_type;

    use crate::subsets::Subsets;

    define_index_type! {
        pub struct A = u32;
    }

    define_index_type! {
        pub struct B = u32;
    }

    #[test]
    fn test_thousand() {
        let mut subsets = Subsets::<A, B>::new(A::new(1000));
        let (_, mut set) = subsets.push();
        set.include(A::new(800));
        set.include(A::new(400));
        set.include(A::new(500));
        let elems: Vec<A> = subsets.get(B::new(0)).into_iter().collect();
        assert_eq!(elems, vec![A::new(400), A::new(500), A::new(800)]);
    }
}
