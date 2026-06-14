use std::{
    borrow::Borrow,
    collections::HashMap,
    fmt::{self, Debug, Display},
    hash::Hash,
    ops::Index,
};

use fixedbitset::FixedBitSet;
use qp_trie::Trie;

use crate::{
    RegexCache,
    hir::{GroupError, GroupOffset, Hir, HirError},
    lir::{Lir, LirSyntax},
    syntax::{Ast, SyntaxError},
};

#[derive(Debug)]
pub struct BuildError;

impl From<SyntaxError> for BuildError {
    fn from(_value: SyntaxError) -> Self {
        Self
    }
}
impl From<GroupError> for BuildError {
    fn from(_: GroupError) -> Self {
        Self
    }
}
impl From<HirError> for BuildError {
    fn from(_: HirError) -> Self {
        Self
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NdStateId(u32);

impl Default for NdStateId {
    fn default() -> Self {
        Self::SINK
    }
}

impl Debug for NdStateId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::SINK {
            write!(f, "⊥")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl NdStateId {
    pub const SINK: Self = Self(0);

    #[inline]
    pub fn is_sink(self) -> bool {
        self == Self::SINK
    }

    #[inline]
    pub fn new(pos: usize) -> Self {
        debug_assert!(pos <= u32::MAX as usize);
        Self(pos as u32)
    }

    #[inline]
    pub const fn into_inner(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct NdStateIdBytes([u8; 4]);

impl From<NdStateId> for NdStateIdBytes {
    fn from(value: NdStateId) -> Self {
        Self(value.0.to_ne_bytes())
    }
}
impl From<NdStateIdBytes> for NdStateId {
    fn from(value: NdStateIdBytes) -> Self {
        Self(u32::from_ne_bytes(value.0))
    }
}
impl Borrow<[u8]> for NdStateIdBytes {
    fn borrow(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Debug)]
pub struct NDState {
    pub transitions: [NdStateId; 256],
    pub empty: Vec<NdStateId>,
    pub token: Option<TokenId>,
    pub accepting: bool,
}

impl NDState {
    #[inline]
    pub fn new(token: TokenId) -> Self {
        Self {
            transitions: [NdStateId::SINK; _],
            empty: Vec::new(),
            token: Some(token),
            accepting: false,
        }
    }

    #[inline]
    pub fn sink() -> Self {
        Self {
            transitions: [NdStateId::SINK; _],
            empty: Vec::new(),
            token: None,
            accepting: false,
        }
    }
}

impl Index<u8> for NDState {
    type Output = NdStateId;

    fn index(&self, index: u8) -> &Self::Output {
        &self.transitions[index as usize]
    }
}

impl Display for NDState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "State:")?;
        let mut last: Option<(NdStateId, _)> = None;
        for (i, tra) in self.transitions.iter().copied().enumerate() {
            if let Some((lst, strt)) = last {
                if lst != tra {
                    if !lst.is_sink() {
                        writeln!(f, " -[{strt:x}-{:x}]-> {}", i - 1, lst.0)?;
                    }
                    last = Some((tra, i));
                }
            } else {
                last = Some((tra, i));
            }
        }
        if let Some((last, start)) = last
            && !last.is_sink()
        {
            writeln!(f, " -[{start:x}-ff]-> {}", last.0)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct NdStateSet {
    set: Trie<NdStateIdBytes, ()>,
}

impl Hash for NdStateSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.set.count().hash(state);
        for i in self.set.keys().copied() {
            i.hash(state);
        }
    }
}
impl FromIterator<NdStateId> for NdStateSet {
    fn from_iter<T: IntoIterator<Item = NdStateId>>(iter: T) -> Self {
        Self {
            set: iter.into_iter().map(|id| (id.into(), ())).collect(),
        }
    }
}

impl NdStateSet {
    /// Insert `state` into the set.  Returns whether `state` was already present in the set.
    pub fn insert(&mut self, state: NdStateId) -> bool {
        self.set.insert(state.into(), ()).is_some()
    }

    /// Check whether the set contains `state`.
    pub fn contains(&self, state: NdStateId) -> bool {
        self.set.contains_key::<NdStateIdBytes>(&state.into())
    }

    pub fn iter(&self) -> impl Iterator<Item = NdStateId> {
        self.set.keys().map(|key| NdStateId::from(*key))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TokenId(u16);

impl TokenId {
    pub fn new(pos: usize) -> Self {
        debug_assert!(pos <= u16::MAX as _);
        Self(pos as _)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Regular {
        regex: String,
        captures_info: Vec<GroupOffset>,
    },
    Synthetic,
}

#[derive(Debug, Clone)]
pub struct TokenSet {
    set: FixedBitSet,
}

impl TokenSet {
    pub fn is_empty(&self) -> bool {
        self.set.is_clear()
    }

    pub fn intersect(&mut self, other: &TokenSet) {
        self.set.intersect_with(&other.set);
    }

    pub fn iter(&self) -> impl Iterator<Item = TokenId> {
        self.set.ones().map(|x| TokenId(x as _))
    }

    pub fn contains(&self, id: TokenId) -> bool {
        self.set.contains(id.0 as usize)
    }
}

impl FromIterator<TokenId> for TokenSet {
    fn from_iter<T: IntoIterator<Item = TokenId>>(iter: T) -> Self {
        Self {
            set: iter.into_iter().map(|id| id.0 as usize).collect(),
        }
    }
}

/// Nondeterministif Finite Automaton.
pub struct Nfa {
    states: Vec<NDState>,
    token_by_regex: HashMap<String, TokenId>,
    tokens: Vec<Token>,
    start: FixedBitSet,
}

impl Nfa {
    /// Create a new `NfaBuilder`.
    pub fn new() -> Self {
        Self {
            states: vec![NDState::sink()],
            token_by_regex: HashMap::new(),
            tokens: Vec::new(),
            start: FixedBitSet::new(),
        }
    }

    /// Allocate a synthetic token.  A synthetic token is not associated to a regular expression
    /// that it matches, and, as such, are never produced by the regex engine.  Synthetic tokens
    /// are useful if the lexer has an extra layer between the regex engine and the stream
    /// interface.  For instance, a lexer hook could transform indentation information into
    /// synthetic parenthesis.
    pub fn allocate_token(&mut self) -> TokenId {
        let id = TokenId::new(self.tokens.len());
        self.tokens.push(Token::Synthetic);
        id
    }

    #[inline]
    pub fn start(&self) -> NdStateSet {
        self.epsilon_closure(self.start.ones().map(NdStateId::new))
    }

    #[inline]
    pub fn tokens(&self, states: impl IntoIterator<Item = impl Borrow<NdStateId>>) -> TokenSet {
        states
            .into_iter()
            .filter_map(|state| self[*state.borrow()].token)
            .collect()
    }

    pub fn accepting(
        &self,
        states: impl IntoIterator<Item = impl Borrow<NdStateId>>,
    ) -> TokenSet {
        states
            .into_iter()
            .filter_map(|state| {
                if self[*state.borrow()].accepting {
                    self[*state.borrow()].token
                } else {
                    None
                }
            })
            .collect()
    }

    #[inline]
    pub fn transition(
        &self,
        byte: u8,
        states: impl IntoIterator<Item = impl Borrow<NdStateId>>,
    ) -> NdStateSet {
        self.epsilon_closure(states.into_iter().map(|state| self[*state.borrow()][byte]))
    }

    /// Add a token corresponding a regular expression `regex`.  This can fail, if the regular
    /// expression is not valid.
    pub fn add_token(
        &mut self,
        regex: &str,
        regex_cache: &mut RegexCache,
    ) -> Result<TokenId, BuildError> {
        if let Some(id) = self.token_by_regex.get(regex) {
            return Ok(*id);
        }
        let start = self.states.len();
        self.start.grow_and_insert(start);
        let id = TokenId::new(self.tokens.len());
        self.token_by_regex.insert(regex.to_string(), id);
        self.states.push(NDState::new(id));
        let ast = Ast::new(regex)?;
        let hir = Hir::try_from(ast)?;
        let lir = Lir::try_from(hir)?;
        self.tokens.push(Token::Regular {
            regex: regex.to_string(),
            captures_info: lir.groups,
        });
        let end = self.build_automaton(start, *lir.syntax, id, regex_cache);
        self.states[end].accepting = true;
        Ok(id)
    }

    /// Build the automaton generated by `lir`, starting from the `start` state.  It tags every
    /// node created this way with `token`, end outputs the index of the ending state.
    fn build_automaton(
        &mut self,
        start: usize,
        lir: LirSyntax,
        token: TokenId,
        regex_cache: &mut RegexCache,
    ) -> usize {
        match lir {
            LirSyntax::Union(items) => {
                let end = self.states.len();
                self.states.push(NDState::new(token));
                for item in items {
                    let local_start = self.states.len();
                    self.states[start].empty.push(NdStateId::new(local_start));
                    self.states.push(NDState::new(token));
                    let local_end = self.build_automaton(local_start, item, token, regex_cache);
                    self.states[local_end].empty.push(NdStateId::new(end));
                }
                end
            }
            LirSyntax::Cons(items) => {
                let mut end = start;
                for item in items {
                    end = self.build_automaton(end, item, token, regex_cache);
                }
                end
            }
            LirSyntax::Star(lir) => {
                let end = self.build_automaton(start, *lir, token, regex_cache);
                self.states[start].empty.push(NdStateId::new(end));
                self.states[end].empty.push(NdStateId::new(start));
                end
            }
            LirSyntax::Option(lir) => {
                let end = self.build_automaton(start, *lir, token, regex_cache);
                self.states[start].empty.push(NdStateId::new(end));
                end
            }
            LirSyntax::Class(class) => regex_cache
                .build(class, &mut self.states, NdStateId::new(start), token)
                .into_inner() as usize,
        }
    }

    fn epsilon_closure(
        &self,
        states: impl IntoIterator<Item = impl Borrow<NdStateId>>,
    ) -> NdStateSet {
        let mut result = NdStateSet::default();
        let mut boundary = states
            .into_iter()
            .map(|state| *state.borrow())
            .collect::<Vec<_>>();
        while let Some(state) = boundary.pop() {
            if result.insert(state) {
                continue;
            }
            boundary.extend(self[state].empty.iter());
        }
        result
    }

    pub fn longest_match(
        &self,
        string: &[u8],
        mut tokens: TokenSet,
    ) -> Option<(usize, TokenSet)> {
        let mut state = self.start();
        let mut accepting = self.accepting(state.iter());
        accepting.intersect(&tokens);
        let mut result = if accepting.is_empty() {
            None
        } else {
            Some((0, accepting))
        };
        for (i, byte) in string.iter().copied().enumerate() {
            state = self.transition(byte, state.iter());
            tokens.intersect(&self.tokens(state.iter()));
            let mut accepting = self.accepting(state.iter());
            accepting.intersect(&tokens);
            if !accepting.is_empty() {
                result = Some((i + 1, accepting))
            }
            if tokens.is_empty() {
                break;
            }
        }
        result
    }
}

impl Index<TokenId> for Nfa {
    type Output = Token;

    fn index(&self, index: TokenId) -> &Self::Output {
        &self.tokens[index.0 as usize]
    }
}

impl Index<NdStateId> for Nfa {
    type Output = NDState;

    fn index(&self, state: NdStateId) -> &Self::Output {
        &self.states[state.into_inner() as usize]
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::{GroupOffset, Offset};

    use super::{Nfa, Token};

    #[test]
    fn build_automaton() {
        let mut nfa = Nfa::new();
        let source_regex = "(?abd)|(?efg)";
        let first_token = nfa.add_token(source_regex).unwrap();
        assert_eq!(first_token.0, 0);
        let Token::Regular {
            ref regex,
            ref captures_info,
        } = nfa[first_token]
        else {
            panic!()
        };
        assert_eq!(regex, source_regex);
        assert_eq!(captures_info.len(), 1);
        assert_eq!(
            captures_info[0],
            GroupOffset {
                start: Offset::BothEnds {
                    start_offset: 0,
                    end_offset: 3,
                },
                end: Offset::BothEnds {
                    start_offset: 3,
                    end_offset: 0,
                }
            }
        );
    }
}
