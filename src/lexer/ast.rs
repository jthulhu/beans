use std::rc::Rc;
use crate::{parser::AST, typed::*};

#[derive(Debug)]
pub(crate) struct Ast {
    pub terminals: Vec<Terminal>,
}

impl Tree for Ast {
    fn read(ast: crate::parser::AST) -> Self {
        let mut node = node!(ast);
        Self {
            terminals: Vec::read(get!(node => terminals)),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Terminal {
    pub ignore: bool,
    pub keyword: bool,
    pub unwanted: bool,
    pub name: Rc<str>,
    pub regex: Rc<str>,
    pub comment: Option<Rc<str>>,
}

impl Tree for Terminal {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self {
            ignore: Option::<()>::read(get!(node => ignore)).is_some(),
            keyword: Option::<()>::read(get!(node => keyword)).is_some(),
            unwanted: Option::<()>::read(get!(node => unwanted)).is_some(),
            comment: Option::read(get!(node => comment)).map(|x: Comment| x.0),
            name: value!(node => name),
            regex: value!(node => value),
        }
    }
}

struct Comment(Rc<str>);

impl Tree for Comment {
    fn read(ast: AST) -> Self {
        let mut node = node!(ast);
        Self(value!(node => value))
    }
}
