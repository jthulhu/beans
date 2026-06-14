//! # Unicode module
//!
//! This module takes care of generating `u8` based automata for decoding utf-8 strings.  This
//! permits recognizing utf-8 strings with a byte automaton.  It's important, because:
//! - We want to handle unicode.  ASCII only lexers are outdated.
//! - We want automata that process the input stream byte per byte, not character per character.
//!   This way, the automaton can still have a fully table driven processing, while having a
//!   reasonable memory footprint.
//! Because adding a unicode class to an automaton can be somewhat expensive, this module offers
//! a cache driven API.  You can limit the amount of entries in the cache.

use std::{borrow::Borrow, collections::HashMap, iter::once, ops::RangeInclusive};

use fixedbitset::FixedBitSet;

use crate::{
    nfa::{NDState, NdStateId, TokenId},
    syntax::{Class, ClassElement},
};

const ONE_BYTE_MAX_BITS: u8 = 7;
const ONE_BYTE_MIN: char = char::from_u32(0).unwrap();
const ONE_BYTE_MAX: char = char::from_u32((1 << ONE_BYTE_MAX_BITS) - 1).unwrap();

const TWO_BYTES_MAX_BITS: u8 = 5 + 6;
const TWO_BYTES_MIN: char = char::from_u32(ONE_BYTE_MAX as u32 + 1).unwrap();
const TWO_BYTES_MAX: char = char::from_u32((1 << TWO_BYTES_MAX_BITS) - 1).unwrap();

const THREE_BYTES_MAX_BITS: u8 = 4 + 6 + 6;
const THREE_BYTES_MIN: char = char::from_u32(TWO_BYTES_MAX as u32 + 1).unwrap();
const THREE_BYTES_MAX: char = char::from_u32((1 << THREE_BYTES_MAX_BITS) - 1).unwrap();

const FOUR_BYTES_MIN: char = char::from_u32(THREE_BYTES_MAX as u32 + 1).unwrap();
const FOUR_BYTES_MAX: char = char::MAX;

const _: () = assert!(ONE_BYTE_MIN.len_utf8() == 1);
const _: () = assert!(ONE_BYTE_MAX.len_utf8() == 1);
const _: () = assert!(TWO_BYTES_MIN.len_utf8() == 2);
const _: () = assert!(TWO_BYTES_MAX.len_utf8() == 2);
const _: () = assert!(THREE_BYTES_MIN.len_utf8() == 3);
const _: () = assert!(THREE_BYTES_MAX.len_utf8() == 3);
const _: () = assert!(FOUR_BYTES_MIN.len_utf8() == 4);
const _: () = assert!(FOUR_BYTES_MAX.len_utf8() == 4);

/// Intersect two ranges.
fn intersect<T: Ord>(left: RangeInclusive<T>, right: RangeInclusive<T>) -> RangeInclusive<T> {
    let (left_start, left_end) = left.into_inner();
    let (right_start, right_end) = right.into_inner();
    left_start.max(right_start)..=left_end.min(right_end)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PreStateId(u32);

impl PreStateId {
    const SINK: Self = Self(NdStateId::SINK.into_inner());
    const END: Self = Self(0);

    #[inline]
    pub fn new(pos: usize) -> Self {
        debug_assert!(pos <= u32::MAX as usize);
        Self(pos as u32)
    }

    #[inline]
    pub fn offset(self, offset: usize) -> NdStateId {
        debug_assert!(offset <= u32::MAX as usize);
        if self == Self::SINK {
            NdStateId::SINK
        } else {
            NdStateId::new(self.0 as usize + offset)
        }
    }
}

/// Find the successor of `chr`, that is, the smallest character greater than `chr`.  This is
/// *not* the same as casting `chr` to `u32`, taking the successor, and then casting back,
/// because the `char` type has holes in it.
fn char_succ(chr: char) -> Option<char> {
    let mut chrs = chr..=char::MAX;
    debug_assert_eq!(chrs.next().unwrap(), chr);
    chrs.next()
}

/// Find the predecessor of `chr`, that is, the greatest character less than `chr`.  This is
/// *not* the same as casting `chr` to `u32`, taking the predecessor, and then casting back,
/// because the `char` type has holes in it.
fn char_pred(chr: char) -> Option<char> {
    (char::MIN..chr).next_back()
}

/// Represents a set of unicode code points.
///
/// Unicode classes are stable by negation, and by union.  For instance, if we want to build the
/// unicode class that corresponds to `[^\n]`, we can do so by
/// ```rust
/// # use dlsl_regex::UnicodeClass;
/// # fn main() {
/// UnicodeClass::new_char('\n').negate()
/// # ;
/// # }
/// ```
///
/// Internally, unicode classes are normalized, so that you can check whether or not two classes
/// contain exactly the same code points by checking for equality.
/// ```rust
/// # use dlsl_regex::UnicodeClass;
/// # fn main() {
/// let ab = UnicodeClass::new_range('a'..='b');
/// let a = UnicodeClass::new_char('a');
/// let b = UnicodeClass::new_char('b');
/// assert_eq!(ab, a.union(&b));
/// assert_eq!(a.negate().negate(), a);
/// # }
/// ```
#[derive(Debug, Hash, Default, PartialEq, Eq, Clone)]
pub struct UnicodeClass {
    /// Sorted, disjoint, non-mergeable ranges of characters.
    ranges: Vec<RangeInclusive<char>>,
}

impl UnicodeClass {
    #[inline]
    pub fn new_char(chr: char) -> Self {
        Self {
            ranges: vec![chr..=chr],
        }
    }

    #[inline]
    pub fn all() -> Self {
        Self {
            ranges: vec![char::MIN..=char::MAX],
        }
    }

    #[inline]
    pub fn empty() -> Self {
        Self { ranges: Vec::new() }
    }

    #[inline]
    pub fn new_range(range: RangeInclusive<char>) -> Self {
        Self::new_ranges(once(range))
    }

    pub fn new_ranges(ranges: impl Iterator<Item = impl Borrow<RangeInclusive<char>>>) -> Self {
        let mut old_ranges: Vec<_> = ranges
            .map(|x| x.borrow().clone())
            .filter(|range| !range.is_empty())
            .collect();
        old_ranges.sort_unstable_by_key(|range| *range.start());
        let mut ranges: Vec<RangeInclusive<char>> = Vec::new();
        for range in old_ranges {
            // The test for mergeability has to be implemented this way, because the `char` type
            // may have holes.  Ranges are aware of that, and hence they are a reliable way to
            // detect mergeability.
            if let Some(last) = ranges.last_mut()
                && (*last.start()..=*range.end()).count() <= last.count() + range.clone().count()
            {
                *last = *last.start()..=*range.end();
            } else {
                ranges.push(range);
            }
        }

        Self { ranges }
    }

    pub fn negate(&self) -> Self {
        let mut ranges_iter = self.ranges.iter();
        let mut ranges = Vec::new();
        let mut start = if let Some(first) = ranges_iter.next() {
            if let Some(high) = char_pred(*first.start()) {
                ranges.push(char::MIN..=high);
            }
            if let Some(nxt) = char_succ(*first.end()) {
                nxt
            } else {
                return Self { ranges };
            }
        } else {
            return Self {
                ranges: vec![char::MIN..=char::MAX],
            };
        };

        for range in ranges_iter {
            ranges.push(start..=char_pred(*range.start()).unwrap());
            if let Some(end) = char_succ(*range.end()) {
                start = end;
            } else {
                return Self { ranges };
            }
        }
        ranges.push(start..=char::MAX);
        println!("ranges: {ranges:?}");
        Self { ranges }
    }

    pub fn union(&self, other: &UnicodeClass) -> UnicodeClass {
        Self::new_ranges(self.ranges.iter().chain(other.ranges.iter()))
    }

    pub fn is_empty(&self) -> bool {
        self.ranges.is_empty()
    }
}

impl From<Class> for UnicodeClass {
    fn from(value: Class) -> Self {
        let res = UnicodeClass::new_ranges(value.elements.into_iter().flat_map(|element| match element {
            ClassElement::Char(chr) => UnicodeClass::new_char(chr),
            ClassElement::Range { start, end } => UnicodeClass::new_range(start..=end),
            ClassElement::Word => todo!(),
            ClassElement::NoWord => todo!(),
            ClassElement::Space => todo!(),
            ClassElement::NoSpace => todo!(),
            ClassElement::Class(class) => class.into(),
        }.ranges));
        if value.negate { res.negate() } else { res }
    }
}

impl Default for PreStateId {
    fn default() -> Self {
        Self::SINK
    }
}

pub struct RegexCache {
    limit: Option<usize>,
    cache: HashMap<UnicodeClass, ComputedRange>,
}

impl RegexCache {
    pub fn new() -> Self {
        Self {
            limit: None,
            cache: HashMap::new(),
        }
    }

    pub fn new_with_limit(limit: usize) -> Self {
        Self {
            limit: Some(limit),
            cache: HashMap::new(),
        }
    }

    pub fn build(
        &mut self,
        class: UnicodeClass,
        states: &mut Vec<NDState>,
        start: NdStateId,
        token: TokenId,
    ) -> NdStateId {
        let result = self
            .cache
            .entry(class.clone())
            .or_insert_with(|| ComputedRange::new(&class.ranges))
            .execute(states, start.into_inner() as usize, token);
        if let Some(lim) = self.limit
            && self.cache.len() > lim
        {
            self.cache.clear();
        }
        result
    }

    pub fn clear(&mut self) {
        self.cache.clear();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct PreNDState {
    transitions: [PreStateId; 256],
}

impl Default for PreNDState {
    fn default() -> Self {
        Self {
            transitions: [PreStateId::default(); _],
        }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
struct ComputedRange {
    states: Vec<PreNDState>,
    start: Box<PreNDState>,
}

impl ComputedRange {
    fn new_char(chr: char) -> Self {
        let mut start = Box::new(PreNDState::default());
        let mut states = vec![PreNDState::default(); chr.len_utf8()];
        let mut buffer = [0; 4];
        let buffer = chr.encode_utf8(&mut buffer).as_bytes();
        start.transitions[buffer[0] as usize] = PreStateId::new(states.len() - 1);
        for (i, byte) in buffer.iter().copied().enumerate().skip(1) {
            states[i].transitions[byte as usize] = PreStateId::new(i - 1);
        }
        Self { states, start }
    }

    fn new(ranges: &[RangeInclusive<char>]) -> Self {
        // Fast path: if we're adding just a single character, there is a much faster way to do
        // it.
        if ranges.len() == 1 && ranges[0].clone().count() == 1 {
            return Self::new_char(ranges[0].clone().next().unwrap());
        }
        let mut cr = ComputedRange::default();

        cr.states.push(PreNDState::default());

        let mut blop: HashMap<[u8; 3], FixedBitSet> = HashMap::new();

        // 4 bytes characters
        for chr in ranges
            .iter()
            .cloned()
            .flat_map(|range| intersect(range, FOUR_BYTES_MIN..=FOUR_BYTES_MAX))
        {
            let mut buffer = [0; 4];
            chr.encode_utf8(&mut buffer);
            let [key @ .., transition] = buffer;
            blop.entry(key)
                .or_insert(FixedBitSet::with_capacity(256))
                .insert(transition as _);
        }

        let mut node_3: HashMap<FixedBitSet, PreStateId> = HashMap::new();
        for value in blop.values() {
            node_3.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                for i in value.ones() {
                    state.transitions[i] = PreStateId::END;
                }
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_4_3: HashMap<[u8; 3], PreStateId> = blop
            .into_iter()
            .map(|(key, value)| (key, node_3[&value]))
            .collect();

        let mut blop: HashMap<[u8; 2], Box<[PreStateId; 256]>> = HashMap::new();

        for prefix @ [key @ .., transition] in map_4_3.keys().copied() {
            blop.entry(key)
                .or_insert_with(|| Box::new([PreStateId::default(); _]))[transition as usize] =
                map_4_3[&prefix];
        }

        let mut node_2: HashMap<Box<[PreStateId; 256]>, PreStateId> = HashMap::new();
        for value in blop.values() {
            node_2.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                state.transitions.clone_from(value);
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_4_2: HashMap<[u8; 2], PreStateId> = blop
            .into_iter()
            .map(|(key, value)| (key, node_2[&value]))
            .collect();

        let mut blop: HashMap<u8, Box<[PreStateId; 256]>> = HashMap::new();

        for prefix @ [key, transition] in map_4_2.keys().copied() {
            blop.entry(key)
                .or_insert_with(|| Box::new([PreStateId::default(); _]))[transition as usize] =
                map_4_2[&prefix];
        }

        let mut node_1: HashMap<Box<[PreStateId; 256]>, PreStateId> = HashMap::new();
        for value in blop.values() {
            node_1.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                state.transitions.clone_from(value);
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_4_1: HashMap<u8, PreStateId> = blop
            .into_iter()
            .map(|(key, value)| (key, node_1[&value]))
            .collect();

        for (key, value) in map_4_1.into_iter() {
            debug_assert!(cr.start.transitions[key as usize] == PreStateId::SINK);
            cr.start.transitions[key as usize] = value;
        }

        // 3 bytes characters
        let mut blop: HashMap<[u8; 2], FixedBitSet> = HashMap::new();

        for chr in ranges
            .iter()
            .cloned()
            .flat_map(|range| intersect(range, THREE_BYTES_MIN..=THREE_BYTES_MAX))
        {
            let mut buffer = [0; 3];
            chr.encode_utf8(&mut buffer);
            let [key @ .., transition] = buffer;
            blop.entry(key)
                .or_insert(FixedBitSet::with_capacity(256))
                .insert(transition as usize);
        }

        for value in blop.values() {
            node_3.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                for i in value.ones() {
                    state.transitions[i] = PreStateId::END;
                }
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_3_3: HashMap<[u8; 2], PreStateId> = blop
            .into_iter()
            .map(|(key, value)| (key, node_3[&value]))
            .collect();

        let mut blop: HashMap<u8, Box<[PreStateId; 256]>> = HashMap::new();

        for prefix @ [key, transition] in map_3_3.keys().copied() {
            blop.entry(key)
                .or_insert_with(|| Box::new([PreStateId::default(); _]))[transition as usize] =
                map_3_3[&prefix];
        }

        for value in blop.values() {
            node_2.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                state.transitions.clone_from(value);
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_3_2: HashMap<u8, PreStateId> = blop
            .into_iter()
            .map(|(key, value)| (key, node_2[&value]))
            .collect();

        for (key, value) in map_3_2.into_iter() {
            debug_assert!(cr.start.transitions[key as usize] == PreStateId::SINK);
            cr.start.transitions[key as usize] = value;
        }

        // 2 bytes character
        let mut blop: HashMap<u8, FixedBitSet> = HashMap::new();

        for chr in ranges
            .iter()
            .cloned()
            .flat_map(|range| intersect(range, TWO_BYTES_MIN..=TWO_BYTES_MAX))
        {
            let mut buffer = [0; 2];
            chr.encode_utf8(&mut buffer);
            let [key, transition] = buffer;
            blop.entry(key)
                .or_insert(FixedBitSet::with_capacity(256))
                .insert(transition as usize);
        }

        for value in blop.values() {
            node_3.entry(value.clone()).or_insert_with(|| {
                let id = cr.states.len();
                let mut state = PreNDState::default();
                for i in value.ones() {
                    state.transitions[i] = PreStateId::END;
                }
                cr.states.push(state);
                PreStateId::new(id)
            });
        }

        let map_2_3: HashMap<_, _> = blop
            .into_iter()
            .map(|(key, value)| (key, node_3[&value]))
            .collect();
        for (key, value) in map_2_3.into_iter() {
            debug_assert!(cr.start.transitions[key as usize] == PreStateId::SINK);
            cr.start.transitions[key as usize] = value;
        }

        // 1 byte character
        for byte in ranges
            .iter()
            .cloned()
            .flat_map(|range| intersect(range, ONE_BYTE_MIN..=ONE_BYTE_MAX))
        {
            debug_assert!(byte <= u8::MAX as _);
            debug_assert!(cr.start.transitions[byte as usize] == PreStateId::SINK);
            cr.start.transitions[byte as u8 as usize] = PreStateId::END;
        }

        cr
    }

    /// Add the automaton that recognizes the computed range, to the states, starting from
    /// `start`, and returning the ending state.
    fn execute(&self, states: &mut Vec<NDState>, start: usize, token: TokenId) -> NdStateId {
        let offset = states.len();

        for i in 0..256 {
            states[start].transitions[i] = self.start.transitions[i].offset(offset);
        }

        states.extend(self.states.iter().map(|state| NDState {
            transitions: state.transitions.map(|id| id.offset(offset)),
            empty: Vec::new(),
            token: Some(token),
            accepting: false,
        }));

        NdStateId::new(offset)
    }
}

#[cfg(test)]
mod tests {
    use super::{ComputedRange, UnicodeClass};
    use crate::{NDState, NdStateId};

    #[test]
    fn build_all() {
        let cr = ComputedRange::new(&[char::MIN..=char::MAX]);
        let mut states = vec![NDState::new(NdStateId::new(0))];
        cr.execute(&mut states, 0, NdStateId::new(0));
        assert_eq!(states.len(), 9);
    }

    #[test]
    fn build_single() {
        for c in char::MIN..=char::MAX {
            let cr = ComputedRange::new(&[c..=c]);
            assert_eq!(cr, ComputedRange::new_char(c));
            let mut states = vec![NDState::new(NdStateId::new(0))];
            let end = cr.execute(&mut states, 0, NdStateId(0));
            assert_eq!(end, NdStateId::new(1));
            assert_eq!(states.len(), 1 + c.len_utf8());
            assert!(states[0].empty.is_empty());
        }
    }

    #[test]
    fn negate_char() {
        let a = UnicodeClass::new_char('a');
        let na = a.negate();
        println!("{na:?}");
        assert_eq!(na.negate(), a);
    }

    #[test]
    fn negate_all_empty() {
        assert_eq!(UnicodeClass::all().negate(), UnicodeClass::empty());
        assert_eq!(UnicodeClass::empty().negate(), UnicodeClass::all());
    }

    #[test]
    fn union_skip() {
        assert_eq!(
            UnicodeClass::new_char('\u{D7ff}').union(&UnicodeClass::new_char('\u{E000}')),
            UnicodeClass::new_range('\u{D7ff}'..='\u{E000}')
        );
    }
}
