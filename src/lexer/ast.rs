use crate::{error::Result, parser::AST, span::Span, typed::*};
use std::rc::Rc;

#[derive(Debug)]
pub(crate) struct Ast {
    pub terminals: Vec<Terminal>,
    pub span: Span,
}

impl Tree for Ast {
    fn read(ast: crate::parser::AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            terminals: get!(node => terminals).to_tree::<Spanned<_>>()?.inner,
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug)]
pub(crate) struct Terminal {
    pub ignore: Spanned<bool>,
    pub keyword: Spanned<bool>,
    pub unwanted: Spanned<bool>,
    pub name: Spanned<Rc<str>>,
    pub regex: Spanned<Rc<str>>,
    pub comment: Option<Spanned<Rc<str>>>,
    pub span: Span,
}

impl Tree for Terminal {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self {
            ignore: get!(node => ignore).to_tree()?,
            keyword: get!(node => keyword).to_tree()?,
            unwanted: get!(node => unwanted).to_tree()?,
            comment: get!(node => comment)
                .to_tree::<Spanned<Option<Comment>>>()?
                .transpose()
                .map(|x| x.map(|y| y.0).merge()),
            name: spanned_value!(node => name),
            regex: spanned_value!(node => value),
            span: span!(node),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

struct Comment(Spanned<Rc<str>>);

impl Tree for Comment {
    fn read(ast: AST) -> Result<Self> {
        let mut node = node!(ast);
        Ok(Self(spanned_value!(node => value)))
    }

    fn span(&self) -> &Span {
        &self.0.span
    }
}
