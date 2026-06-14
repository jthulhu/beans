use std::ops::Add;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Add for Span {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}
