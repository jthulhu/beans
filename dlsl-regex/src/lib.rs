use std::fmt::{self, Debug, Display};

mod unicode;

pub use unicode::{UnicodeClass, RegexCache};

enum Regex {
    Union(Vec<Regex>),
    Concat(Vec<Regex>),
    Star(Box<Regex>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct StateId(u32);

impl Default for StateId {
    fn default() -> Self {
        Self::SINK
    }
}

impl Debug for StateId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == Self::SINK {
            write!(f, "⊥")
        } else {
            write!(f, "{}", self.0)
        }
    }
}

impl StateId {
    pub const SINK: Self = Self(u32::MAX);
    
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

#[derive(Debug, Default, Clone, Copy)]
pub struct TokenId(u16);

impl TokenId {
    pub fn new(id: u16) -> Self {
        Self(id)
    }
}

#[derive(Debug)]
pub struct NDState {
    pub transitions: [StateId; 256],
    pub empty: Vec<StateId>,
    pub token: TokenId,
}

impl NDState {
    pub fn new(token: TokenId) -> Self {
        Self {
            transitions: [StateId::SINK; _],
            empty: Vec::new(),
            token,
        }
    }
}

impl Display for NDState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "State:")?;
        let mut last: Option<(StateId, _)> = None;
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
        if let Some((last, start)) = last && !last.is_sink() {
            writeln!(f, " -[{start:x}-ff]-> {}", last.0)?;
        }
        Ok(())
    }
}
