/// `newtype!` provides a unified implementation of the newtype pattern by reducing the boilerplate.
/// It currently supports:
///  - vec
///  - id
///  - set
///  - slice
#[macro_export]
macro_rules! newtype {
    // == Slice Newtype ==
    // Default indexer is assuming crate::newtype::Wrapper trait, and ownership of the index (see vec newtype).
    (@slice $(#[$($meta: meta),*])* $visibility: vis $name: ident ($interior_type: ty) [$indexer: ty] of $vec_type: ty) => {
	newtype!{@slice $(#[$($meta),*])* $visibility $name ($interior_type) [$indexer :getter |x: $indexer| crate::newtype::Wrapper::dewrap_into(x)] of $vec_type}
    };
    // Slice newtype implementation
    (@slice $(#[$($meta: meta),*])* $visibility: vis $name: ident ($interior_type: ty) [$indexer: ty :getter $getter: expr] of $vec_type: ty) => {
	$(#[$($meta),*])*
	#[derive(Debug)]
	#[repr(transparent)]
	$visibility struct $name([$interior_type]);

	impl $name {
	    $visibility fn len(&self) -> usize {
		self.0.len()
	    }
	}

	impl std::convert::AsRef<$name> for $vec_type {
	    fn as_ref(&self) -> &$name {
		unsafe { &*(self.0.as_ref() as *const [_] as *const $name) }
	    }
	}

	impl std::ops::Deref for $vec_type {
	    type Target = $name;
	    fn deref(&self) -> &Self::Target {
		self.as_ref()
	    }
	}

	impl std::ops::Index<$indexer> for $name  {
	    type Output = $interior_type;

	    fn index(&self, index: $indexer) -> &Self::Output {
		&self.0[$getter(index)]
	    }
	}
    };
        // == HashMap Newtype ==
    // Vector newtype implementation
    (@map
     $(#[$($meta:meta),*])*
     $visibility:vis $name:ident ($value:ty) [$key:ty]
     $(
	 impl {
	     $($method: item)*
	 }
     )?) => {
	$(#[$($meta),*])*
        #[derive(Debug, Default, PartialEq, Eq)]
	#[allow(missing_docs)]
        $visibility struct $name(hashbrown::HashMap<$key, $value>);
	impl $name {
	    #![allow(unused, missing_docs)]
	    $visibility fn new() -> Self {
		Self(hashbrown::HashMap::new())
	    }

	    $visibility fn len(&self) -> usize {
		self.0.len()
	    }

	    $visibility fn len_as(&self) -> $key {
		self.0.len().into()
	    }

	    $visibility fn iter(&self) -> hashbrown::hash_map::Iter<'_, $key, $value> {
		self.0.iter()
	    }

	    $visibility fn iter_mut(&mut self) -> hashbrown::hash_map::IterMut<'_, $key, $value> {
		self.0.iter_mut()
	    }

	    $visibility fn is_empty(&self) -> bool {
		self.0.is_empty()
	    }

	    $visibility fn insert(&mut self, key: $key, value: $value) {
		self.0.insert(key, value);
	    }

	    $visibility fn get(&self, key: &$key) -> Option<&$value> {
		self.0.get(key)
	    }
	}

	$(
	    impl $name {
		$($method)*
	    }
	)?

        impl From<hashbrown::HashMap<$key, $value>> for $name {
            fn from(hm: HashMap<$key, $value>) -> Self {
                Self(hm)
            }
        }

        impl std::ops::Index<$key> for $name {
            type Output = $value;

            fn index(&self, index: $key) -> &Self::Output {
                &self.0[&index]
            }
        }

	// For test build only, allow directly the wrapped type to index.
	// Not sure if this might be useful, sooner or later...
	// #[cfg(test)]
	// impl std::ops::Index<<$indexer as crate::newtype::Wrapper>::Wrapped> for $name {
	//     type Output = $interior_type;

	//     fn index(&self, index: <$indexer as crate::newtype::Wrapper>::Wrapped) -> &Self::Output {
	// 	&self.0[index]
	//     }
	// }
    };
    // == BitSet Newtype ==
    // Default indexer is assuming crate::newtype::Wrapper trait, and ownership of the index (see vec newtype).
    (@set $(#[$($meta: meta),*])* $visibility: vis $name: ident [$indexer: ty]) => {
	newtype!{@set $(#[$($meta),*])* $visibility $name [$indexer :getter |x: $indexer| crate::newtype::Wrapper::dewrap_into(x)]}
    };
    // BitSet newtype implementation
    (@set $(#[$($meta: meta),*])* $visibility: vis $name: ident [$indexer: ty :getter $getter: expr]) => {
	$(#[$($meta),*])*
	#[derive(Debug, serde::Serialize, serde::Deserialize)]
	#[allow(missing_docs)]
	$visibility struct $name(fixedbitset::FixedBitSet);

	impl std::fmt::Display for $name {
	    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.0.fmt(f)
	    }
	}

	impl $name {
	    #![allow(unused, missing_docs)]
	    $visibility const fn new() -> Self {
		Self(fixedbitset::FixedBitSet::new())
	    }

	    $visibility fn from_vec(size: $indexer, vec: Vec<$indexer>) -> Self {
		let mut set = fixedbitset::FixedBitSet::with_capacity($getter(size));
		for i in vec {
		    set.insert($getter(i));
		}
		Self(set)
	    }

	    $visibility fn with_capacity(size: $indexer) -> Self {
		Self(fixedbitset::FixedBitSet::with_capacity($getter(size)))
	    }

	    $visibility fn with_raw_capacity(size: usize) -> Self {
		Self(fixedbitset::FixedBitSet::with_capacity(size))
	    }

	    #[inline]
	    $visibility fn len(&self) -> usize {
		self.0.len()
	    }

	    #[inline]
	    $visibility fn is_empty(&self) -> bool {
		self.0.len() == 0
	    }

	    #[inline]
	    $visibility fn len_as(&self) -> $indexer {
		self.0.len().into()
	    }

	    #[inline]
	    $visibility fn contains(&self, bit: $indexer) -> bool {
		self.0.contains($getter(bit))
	    }

	    #[inline]
	    $visibility fn set_range(&mut self, std::ops::Range { start, end }: std::ops::Range<$indexer>, enabled: bool) {
		self.0.set_range($getter(start)..$getter(end), enabled)
	    }

	    #[inline]
	    $visibility fn insert_range(&mut self, range: std::ops::Range<$indexer>) {
		self.set_range(range, true)
	    }

	    #[inline]
	    $visibility fn toggle_range(&mut self, std::ops::Range { start, end }: std::ops::Range<$indexer>) {
		self.0.toggle_range($getter(start)..$getter(end))
	    }

	    #[inline]
	    $visibility fn as_slice(&self) -> &[u32] {
		self.0.as_slice()
	    }

	    #[inline]
	    $visibility fn as_mut_slice(&mut self) -> &mut [u32] {
		self.0.as_mut_slice()
	    }

	    #[inline]
	    $visibility fn insert(&mut self, bit: $indexer) {
		self.0.insert($getter(bit))
	    }

	    #[inline]
	    $visibility fn put(&mut self, bit: $indexer) -> bool {
		self.0.put($getter(bit))
	    }

	    #[inline]
	    $visibility fn toggle(&mut self, bit: $indexer) {
		self.0.toggle($getter(bit))
	    }

	    #[inline]
	    $visibility fn set(&mut self, bit: $indexer, enabled: bool) {
		self.0.set($getter(bit), enabled)
	    }
	}
    };
    // == Identifier Newtype ==
    // Default inderior_type is usize
    (@id $(#[$($meta: meta),*])* $visibility: vis $name: ident $(impl { $($method: item)* })?) => {
	newtype!{@id $(#[$($meta),*])* #[derive(Copy, PartialEq, Eq, Hash)] $visibility $name (usize) $(impl { $($method)* })?}
    };
    // Identifier newtype implementation
    (@id $(#[$($meta: meta),*])* $visibility: vis $name: ident ($interior_type: ty) $(impl { $($method: item)* })?) => {
	$(#[$($meta),*])*
	#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
	#[allow(missing_docs)]
	$visibility struct $name(pub $interior_type);

	$(
	    impl $name {
		$($method)*
	    }
	)?

	impl From<$interior_type> for $name {
	    fn from(x: $interior_type) -> Self {
		Self(x)
	    }
	}

	impl crate::newtype::Wrapper for $name {
	    type Wrapped = $interior_type;
	    fn dewrap(&self) -> &Self::Wrapped {
		&self.0
	    }
	    fn dewrap_into(self) -> Self::Wrapped {
		self.0
	    }
	}

	impl std::fmt::Display for $name {
	    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		write!(f, "{}", self.0)
	    }
	}
    };
    // == Vector Newtype ==
    // Default indexer is assuming crate::newtype::Wrapper trait, and ownership of the index (otherwise, go for `crate::wrapper::Wrapped::dewrap`).
    (@vec $(#[$($meta: meta),*])* $visibility: vis $name: ident ($interior_type: ty) [$indexer: ty] $($rest:tt)*) => {
	newtype!{@vec $(#[$($meta),*])* $visibility $name ($interior_type) [$indexer :getter |x: $indexer| crate::newtype::Wrapper::dewrap_into(x)] $($rest)*}
    };
    // Vector newtype implementation
    (@vec
     $(#[$($meta: meta),*])*
     $visibility: vis $name: ident ($interior_type: ty) [$indexer: ty :getter $getter: expr]
     $(
	 impl {
	     $($method: item)*
	 }
     )?) => {
	$(#[$($meta),*])*
        #[derive(Debug, Default)]
	#[allow(missing_docs)]
        $visibility struct $name(Vec<$interior_type>);
	impl $name {
	    #![allow(unused, missing_docs)]
	    $visibility fn new() -> Self {
		Self(Vec::new())
	    }

	    $visibility fn len(&self) -> usize {
		self.0.len()
	    }

	    $visibility fn len_as(&self) -> $indexer {
		self.0.len().into()
	    }

	    $visibility fn iter(&self) -> std::slice::Iter<'_, $interior_type> {
		self.0.iter()
	    }

	    $visibility fn iter_mut(&mut self) -> std::slice::IterMut<'_, $interior_type> {
		self.0.iter_mut()
	    }

	    $visibility fn is_empty(&self) -> bool {
		self.0.is_empty()
	    }

	    $visibility fn push(&mut self, element: $interior_type) {
		self.0.push(element)
	    }
	}

	$(
	    impl $name {
		$($method)*
	    }
	)?

        impl From<Vec<$interior_type>> for $name {
            fn from(v: Vec<$interior_type>) -> Self {
                Self(v)
            }
        }

	impl Extend<$interior_type> for $name {
	    fn extend<I: IntoIterator<Item=$interior_type>>(&mut self, iter: I) {
		self.0.extend(iter);
	    }
	}

        impl std::ops::Index<$indexer> for $name {
            type Output = $interior_type;

            fn index(&self, index: $indexer) -> &Self::Output {
                &self.0[$getter(index)]
            }
        }

	impl std::ops::IndexMut<$indexer> for $name {
	    fn index_mut(&mut self, index: $indexer) -> &mut Self::Output {
		&mut self.0[$getter(index)]
	    }
	}

	// For test build only, allow directly the wrapped type to index.
	// Not sure if this might be useful, sooner or later...
	// #[cfg(test)]
	// impl std::ops::Index<<$indexer as crate::newtype::Wrapper>::Wrapped> for $name {
	//     type Output = $interior_type;

	//     fn index(&self, index: <$indexer as crate::newtype::Wrapper>::Wrapped) -> &Self::Output {
	// 	&self.0[index]
	//     }
	// }
    };

    // == Newtype Interface ==
    ($(#[$($meta: meta),*])* $visibility: vis $newtype: ident $name: ident $($rest:tt)*) => {
	newtype!{@$newtype $(#[$($meta),*])* $visibility $name $($rest)*}
    };
}

/// `nvec!` stands for `newtype vec!`. It allows a similar usage of the `vec!`
/// macro, but for newtype vectors. The syntax is the same, but with the target
/// newtype vector beforehand.
#[macro_export]
macro_rules! nvec {
    ($nvec:tt $c:expr ; $nb:expr) => {
        $nvec::from(vec![$c; crate::newtype::Wrapper::dewrap_into($nb)])
    };
}

pub trait Wrapper: From<Self::Wrapped> {
    type Wrapped;
    fn dewrap(&self) -> &Self::Wrapped;
    fn dewrap_into(self) -> Self::Wrapped
    where
        Self: Sized;
}
