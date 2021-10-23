use fixedbitset::FixedBitSet;
use fixedbitset::IndexRange;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::marker::PhantomData;

pub trait Wrapper: From<Self::Wrapped> {
    type Wrapped;
    fn dewrap(&self) -> &Self::Wrapped;
    fn dewrap_into(self) -> Self::Wrapped
    where
        Self: Sized;
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WrappedBitSet<W: Wrapper<Wrapped = usize>> {
    bitset: FixedBitSet,
    wrapper_type: PhantomData<W>,
}

impl<W> fmt::Display for WrappedBitSet<W>
where
    W: Wrapper<Wrapped = usize>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.bitset.fmt(f)
    }
}

impl<W: Wrapper<Wrapped = usize>> WrappedBitSet<W> {
    pub fn new() -> Self {
        Self {
            bitset: FixedBitSet::new(),
            wrapper_type: PhantomData,
        }
    }

    pub fn with_capacity(bits: usize) -> Self {
        Self {
            bitset: FixedBitSet::with_capacity(bits),
            wrapper_type: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.bitset.len()
    }

    pub fn contains(&self, bit: W) -> bool {
        self.bitset.contains(bit.dewrap_into())
    }

    #[inline]
    pub fn set_range<T: IndexRange>(&mut self, range: T, enabled: bool) {
        self.bitset.set_range(range, enabled);
    }

    #[inline]
    pub fn insert_range<T: IndexRange>(&mut self, range: T) {
        self.bitset.insert_range(range);
    }

    #[inline]
    pub fn toggle_range<T: IndexRange>(&mut self, range: T) {
        self.bitset.toggle_range(range);
    }

    #[inline]
    pub fn as_slice(&self) -> &[u32] {
        self.bitset.as_slice()
    }

    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [u32] {
        self.bitset.as_mut_slice()
    }

    #[inline]
    pub fn insert(&mut self, bit: W) {
        self.bitset.insert(bit.dewrap_into());
    }

    #[inline]
    pub fn put(&mut self, bit: W) -> bool {
        self.bitset.put(bit.dewrap_into())
    }

    #[inline]
    pub fn toggle(&mut self, bit: W) {
        self.bitset.toggle(bit.dewrap_into());
    }

    #[inline]
    pub fn set(&mut self, bit: W, enabled: bool) {
        self.bitset.set(bit.dewrap_into(), enabled);
    }
}
