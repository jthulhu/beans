//! # Higher Intermediate Representation
//!
//! This module provides a higher intermediate representation for regular expressions.  Regular
//! expressions are still represented as syntax tree, but with variadic spread.  Some syntactic
//! noise is removed.
//!
//! `Hir` has smart constructors that help turning a binary syntax tree into a variadic syntax
//! tree.  
//! They also report syntactically valid but semantically wrong regex.  An instance of such regex
//! is `a??`, which is semantically equivalent to `a?`, or `()*`, which is the same as `()`.
//! This kind of regular expressions usually breaks backtracking algorithms.  Although this
//! engine would have no problem handling this kind of regular expressions, given the popularity
//! of backtracking engines, people would probably expect these buggy regex to be reported as
//! such.
//! The smart constructors also simplify regular expressions.

use std::{
    mem::take,
    ops::{Add, BitAnd},
};

use crate::{
    UnicodeClass,
    span::Span,
    syntax::{Ast, Class, NodeKind},
};

/// Temporary standalone for group errors.
pub struct GroupError;

pub enum ErrorKind {
    NullableNullable { span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GroupOffset {
    pub start: Offset,
    pub end: Offset,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Offset {
    FromStart {
        offset: usize,
    },
    FromEnd {
        offset: usize,
    },
    BothEnds {
        start_offset: usize,
        end_offset: usize,
    },
}

impl BitAnd for Offset {
    type Output = Option<Self>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (
                Offset::FromStart {
                    offset: left_offset,
                },
                Offset::FromStart {
                    offset: right_offset,
                },
            ) if left_offset == right_offset => Some(Offset::FromStart {
                offset: left_offset,
            }),
            (Offset::FromStart { offset }, Offset::BothEnds { start_offset, .. })
            | (Offset::BothEnds { start_offset, .. }, Offset::FromStart { offset })
                if start_offset == offset =>
            {
                Some(Offset::FromStart { offset })
            }
            (
                Offset::FromEnd {
                    offset: left_offset,
                },
                Offset::FromEnd {
                    offset: right_offset,
                },
            ) if left_offset == right_offset => Some(Offset::FromEnd {
                offset: left_offset,
            }),
            (Offset::FromEnd { offset }, Offset::BothEnds { end_offset, .. })
            | (Offset::BothEnds { end_offset, .. }, Offset::FromEnd { offset })
                if offset == end_offset =>
            {
                Some(Offset::FromEnd { offset })
            }
            (
                Offset::BothEnds {
                    start_offset: left_start,
                    end_offset: left_end,
                },
                Offset::BothEnds {
                    start_offset: right_start,
                    end_offset: right_end,
                },
            ) => {
                if left_start == right_start && left_end == right_end {
                    Some(Offset::BothEnds {
                        start_offset: left_start,
                        end_offset: left_end,
                    })
                } else if left_start == right_start {
                    Some(Offset::FromStart { offset: left_start })
                } else if left_end == right_end {
                    Some(Offset::FromEnd { offset: left_end })
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

pub struct HirError {
    kind: Box<ErrorKind>,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Union(Vec<Hir>),
    Cons(Vec<Hir>),
    Capture(Hir),
    Star(Hir),
    Option(Hir),
    Class(UnicodeClass),
}

#[derive(Debug, Clone, Copy)]
pub enum Size {
    Bottom,
    Size(usize),
}

impl Add for Size {
    type Output = Size;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Size::Bottom, _) | (_, Size::Bottom) => Size::Bottom,
            (Size::Size(left), Size::Size(right)) => Size::Size(left + right),
        }
    }
}

impl BitAnd for Size {
    type Output = Option<Size>;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Size::Bottom, Size::Bottom) => Some(Self::Bottom),
            (Size::Bottom, Size::Size(size)) | (Size::Size(size), Size::Bottom) => {
                Some(Self::Size(size))
            }
            (Size::Size(left_size), Size::Size(right_size)) if left_size == right_size => {
                Some(Size::Size(left_size))
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Hir {
    pub kind: Box<Kind>,
    pub span: Span,
    /// The size of matched strings by the regular expression.  Returns `None` if there is not a
    /// unique such length.  For instance, `foo|bar` has a size of 3, while `a?`, `ab*` and
    /// `ab|c` have no static size.
    ///
    /// This is useful to compute whether group captures can be statically determined.  This will
    /// not work perfectly on some edge cases.  In particular, if you do `[]|a`, then the only
    /// matched string is `a`, but its reported size will be `None`.  This is not much of an
    /// issue as `[]` is a useless regex for a lexer.
    ///
    /// **Note**: the size is expressed in unicode characters, not bytes, to avoid character
    /// classes from having non-unique size.
    pub size: Option<Size>,
}

impl Hir {
    pub fn empty(span: Span) -> Self {
        Self {
            kind: Box::new(Kind::Union(Vec::new())),
            span,
            size: Some(Size::Bottom),
        }
    }

    pub fn epsilon(span: Span) -> Self {
        Self {
            kind: Box::new(Kind::Cons(Vec::new())),
            span,
            size: Some(Size::Size(0)),
        }
    }

    pub fn cons(mut self, other: Hir, span: Span) -> Self {
        if (self.is_empty() || other.is_empty()) && !self.has_capture() && other.has_capture() {
            Self::empty(span)
        } else {
            let size = if let Some(left_size) = self.size
                && let Some(right_size) = other.size
            {
                Some(left_size + right_size)
            } else {
                None
            };
            let cons = if let Kind::Cons(ref mut cons) = *self.kind {
                cons
            } else {
                &mut vec![self]
            };
            if let Kind::Cons(other_cons) = *other.kind {
                cons.extend(other_cons);
            } else {
                cons.push(other);
            }
            Self {
                kind: Box::new(Kind::Cons(take(cons))),
                span,
                size,
            }
        }
    }

    pub fn union(mut self, other: Hir, span: Span) -> Self {
        if self.is_empty() && other.is_empty() && !self.has_capture() && !other.has_capture() {
            Self::empty(span)
        } else {
            let size = if let Some(left_size) = self.size
                && let Some(right_size) = other.size
            {
                left_size & right_size
            } else {
                None
            };
            let union = if let Kind::Union(ref mut union) = *self.kind {
                union
            } else {
                &mut vec![self]
            };
            if let Kind::Union(other_union) = *other.kind {
                union.extend(other_union);
            } else {
                union.push(other);
            }
            Self {
                kind: Box::new(Kind::Union(take(union))),
                span,
                size,
            }
        }
    }

    pub fn capture(self, span: Span) -> Self {
        if self.is_empty() {
            // report error
            todo!()
        }
        Self {
            size: self.size,
            kind: Box::new(Kind::Capture(self)),
            span,
        }
    }

    pub fn star(self, span: Span) -> Self {
        if self.nullable() {
            // throw error
        }
        if self.is_empty() {
            Self::epsilon(span)
        } else {
            Self {
                kind: Box::new(Kind::Star(self)),
                span,
                size: None,
            }
        }
    }

    pub fn plus(self, span: Span) -> Self {
        if self.is_empty() {
            Self::empty(span)
        } else {
            self.clone().cons(self.star(span), span)
        }
    }

    pub fn option(self, span: Span) -> Self {
        if self.nullable() {
            // throw error
        }
        if self.is_empty() {
            Self::epsilon(span)
        } else {
            Self {
                kind: Box::new(Kind::Option(self)),
                span,
                size: None,
            }
        }
    }

    pub fn class(class: Class, span: Span) -> Self {
        let cls = UnicodeClass::from(class);
        if cls.is_empty() {
            Self::empty(span)
        } else {
            Self {
                kind: Box::new(Kind::Class(cls)),
                span,
                size: Some(Size::Size(1)),
            }
        }
    }

    pub fn has_capture(&self) -> bool {
        match &*self.kind {
            Kind::Union(hirs) | Kind::Cons(hirs) => hirs.iter().any(|hir| hir.has_capture()),
            Kind::Capture(_) => true,
            Kind::Star(hir) | Kind::Option(hir) => hir.has_capture(),
            Kind::Class(_) => false,
        }
    }

    /// Check whether the regular expression is nullable, that is, whether it matches the empty
    /// string.
    pub fn nullable(&self) -> bool {
        match *self.kind {
            Kind::Union(ref v) => v.iter().any(|ir| ir.nullable()),
            Kind::Cons(ref v) => v.iter().all(|ir| ir.nullable()),
            Kind::Capture(ref ir) => ir.nullable(),
            Kind::Class(_) => false,
            Kind::Star(_) | Kind::Option(_) => true,
        }
    }

    /// Check whether no string matches the regular expression.
    pub fn is_empty(&self) -> bool {
        match &*self.kind {
            Kind::Union(hirs) => hirs.iter().all(|ir| ir.is_empty()),
            Kind::Cons(hirs) => hirs.iter().any(|ir| ir.is_empty()),
            Kind::Capture(hir) => hir.is_empty(),
            Kind::Star(_) | Kind::Option(_) => false,
            Kind::Class(class) => class.is_empty(),
        }
    }

    /// Find the offsets of capture groups.  Returns an error is there is a group whose position
    /// cannot be deduced statically for all matched strings.
    pub fn groups(&self) -> Result<Vec<GroupOffset>, GroupError> {
        let mut offsets = self.groups_backwards()?;
        offsets.reverse();
        Ok(offsets)
    }

    fn groups_backwards(&self) -> Result<Vec<GroupOffset>, GroupError> {
        match &*self.kind {
            Kind::Union(hirs) => hirs
                .iter()
                .map(|ir| ir.groups_backwards())
                .reduce(|left, right| {
                    let left = left?;
                    let right = right?;
                    if left.len() != right.len() {
                        Err(GroupError)
                    } else {
                        left.into_iter()
                            .zip(right.into_iter())
                            .map(|(lft, rght)| {
                                Some(GroupOffset {
                                    start: (lft.start & rght.start)?,
                                    end: (lft.end & rght.end)?,
                                })
                            })
                            .collect::<Option<_>>()
                            .ok_or(GroupError)
                    }
                })
                .ok_or(GroupError)?,
            Kind::Cons(hirs) => {
                let mut size_before = vec![Some(Size::Size(0))];
                for hir in hirs.iter() {
                    size_before.push(
                        size_before
                            .last()
                            .unwrap()
                            .and_then(|size| Some(size + hir.size?)),
                    );
                }
                let mut size_after = vec![Some(Size::Size(0))];
                for hir in hirs.iter().rev() {
                    size_after.push(
                        size_after
                            .last()
                            .unwrap()
                            .and_then(|size| Some(size + hir.size?)),
                    );
                }
                size_after.pop();
                size_after.reverse();

                let mut result = Vec::new();
                for (i, hir) in hirs.iter().enumerate().rev() {
                    let size_bfr = size_before[i];
                    let size_aftr = size_after[i];

                    for group in hir.groups_backwards()? {
                        result.push(GroupOffset {
                            start: match group.start {
                                Offset::BothEnds {
                                    start_offset,
                                    end_offset,
                                } if let Some(Size::Size(before)) = size_bfr
                                    && let Some(Size::Size(after)) = size_aftr =>
                                {
                                    Offset::BothEnds {
                                        start_offset: start_offset + before,
                                        end_offset: end_offset + after,
                                    }
                                }
                                Offset::FromStart { offset }
                                | Offset::BothEnds {
                                    start_offset: offset,
                                    ..
                                } if let Some(Size::Size(before)) = size_bfr => {
                                    Offset::FromStart {
                                        offset: before + offset,
                                    }
                                }
                                Offset::FromEnd { offset }
                                | Offset::BothEnds {
                                    end_offset: offset, ..
                                } if let Some(Size::Size(after)) = size_aftr => {
                                    Offset::FromEnd {
                                        offset: offset + after,
                                    }
                                }
                                _ => return Err(GroupError),
                            },
                            end: match group.end {
                                Offset::BothEnds {
                                    start_offset,
                                    end_offset,
                                } if let Some(Size::Size(before)) = size_bfr
                                    && let Some(Size::Size(after)) = size_aftr =>
                                {
                                    Offset::BothEnds {
                                        start_offset: start_offset + before,
                                        end_offset: end_offset + after,
                                    }
                                }
                                Offset::FromStart { offset }
                                | Offset::BothEnds {
                                    start_offset: offset,
                                    ..
                                } if let Some(Size::Size(before)) = size_bfr => {
                                    Offset::FromStart {
                                        offset: offset + before,
                                    }
                                }
                                Offset::FromEnd { offset }
                                | Offset::BothEnds {
                                    end_offset: offset, ..
                                } if let Some(Size::Size(after)) = size_aftr => {
                                    Offset::FromEnd {
                                        offset: offset + after,
                                    }
                                }
                                _ => return Err(GroupError),
                            },
                        });
                    }
                }
                Ok(result)
            }
            Kind::Capture(hir) => {
                let mut groups = hir.groups_backwards()?;
                if let Some(size) = hir.size {
                    groups.push(GroupOffset {
                        start: if let Size::Size(size) = size {
                            Offset::BothEnds {
                                start_offset: 0,
                                end_offset: size,
                            }
                        } else {
                            Offset::FromStart { offset: 0 }
                        },
                        end: if let Size::Size(size) = size {
                            Offset::BothEnds {
                                start_offset: size,
                                end_offset: 0,
                            }
                        } else {
                            Offset::FromEnd { offset: 0 }
                        },
                    });
                }
                Ok(groups)
            }
            Kind::Star(hir) | Kind::Option(hir) => {
                if hir.groups_backwards()?.is_empty() {
                    Ok(Vec::new())
                } else {
                    Err(GroupError)
                }
            }
            Kind::Class(_) => Ok(Vec::new()),
        }
    }
}

impl TryFrom<Ast> for Hir {
    type Error = HirError;

    fn try_from(value: Ast) -> Result<Self, Self::Error> {
        match *value.kind {
            NodeKind::Union { left, right } => {
                Ok(Self::union(left.try_into()?, right.try_into()?, value.span))
            }
            NodeKind::Empty => Ok(Self::empty(value.span)),
            NodeKind::Epsilon => Ok(Self::epsilon(value.span)),
            NodeKind::Cons { left, right } => {
                Ok(Self::cons(left.try_into()?, right.try_into()?, value.span))
            }
            NodeKind::Class(class) => Ok(Self::class(class, value.span)),
            NodeKind::Group(ast) => ast.try_into(),
            NodeKind::Capture(ast) => Ok(Self::capture(ast.try_into()?, value.span)),
            NodeKind::Star(ast) => Ok(Self::star(ast.try_into()?, value.span)),
            NodeKind::Plus(ast) => Ok(Self::plus(ast.try_into()?, value.span)),
            NodeKind::Optional(ast) => Ok(Self::option(ast.try_into()?, value.span)),
        }
    }
}
