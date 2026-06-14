use crate::{
    UnicodeClass,
    hir::{GroupError, GroupOffset, Hir, Kind},
};

#[derive(Debug)]
pub enum LirSyntax {
    Union(Vec<LirSyntax>),
    Cons(Vec<LirSyntax>),
    Star(Box<LirSyntax>),
    Option(Box<LirSyntax>),
    Class(UnicodeClass),
}

impl From<Hir> for LirSyntax {
    fn from(value: Hir) -> Self {
        match *value.kind {
            Kind::Capture(hir) => hir.into(),
            Kind::Union(hirs) => Self::union(hirs.into_iter().map(Into::into)),
            Kind::Cons(hirs) => Self::cons(hirs.into_iter().map(Into::into)),
            Kind::Star(hir) => Self::star(hir.into()),
            Kind::Option(hir) => Self::option(hir.into()),
            Kind::Class(class) => Self::class(class),
        }
    }
}

impl LirSyntax {
    #[inline]
    pub fn cons(conses: impl Iterator<Item = Self>) -> Self {
        let mut conses = conses
            .flat_map(|lir| match lir {
                Self::Cons(elms) => elms,
                lir => vec![lir],
            })
            .collect::<Vec<_>>();
        if conses.len() == 1 {
            conses.pop().unwrap()
        } else {
            Self::Cons(conses)
        }
    }

    #[inline]
    pub fn union(unions: impl Iterator<Item = Self>) -> Self {
        let mut unions = unions
            .flat_map(|lir| match lir {
                Self::Union(elms) => elms,
                lir => vec![lir],
            })
            .collect::<Vec<_>>();
        if unions.len() == 1 {
            unions.pop().unwrap()
        } else {
            Self::Union(unions)
        }
    }

    #[inline]
    pub fn star(lir: Self) -> Self {
        Self::Star(Box::new(lir))
    }

    #[inline]
    pub fn option(lir: Self) -> Self {
        Self::Option(Box::new(lir))
    }

    #[inline]
    pub fn class(class: UnicodeClass) -> Self {
        Self::Class(class)
    }
}

#[derive(Debug)]
pub struct Lir {
    pub syntax: Box<LirSyntax>,
    pub groups: Vec<GroupOffset>,
}

impl TryFrom<Hir> for Lir {
    type Error = GroupError;

    fn try_from(hir: Hir) -> Result<Self, Self::Error> {
        Ok(Self {
            groups: hir.groups()?,
            syntax: Box::new(hir.into()),
        })
    }
}
