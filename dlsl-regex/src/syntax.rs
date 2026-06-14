use std::{error::Error, fmt::Display, mem, str::FromStr};

use crate::span::Span;

#[derive(Debug)]
pub enum SyntaxError {
    EscapedChar { chr: char, span: Span },
    EscapedEndOfRegex { span: Span },
    UnclosedGroup { span: Span },
    UnclosedClass { span: Span },
    UnmatchedClosingParen { span: Span },
}

impl Error for SyntaxError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }

    fn cause(&self) -> Option<&dyn Error> {
        self.source()
    }
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::EscapedChar { chr, .. } => {
                write!(f, "do not escape the character `{chr}`")
            }
            SyntaxError::EscapedEndOfRegex { .. } => {
                write!(f, "missing escaped character")
            }
            SyntaxError::UnclosedGroup { .. } => write!(f, "missing closing parenthesis"),
            SyntaxError::UnclosedClass { .. } => write!(f, "missing closing bracket"),
            SyntaxError::UnmatchedClosingParen { .. } => {
                write!(f, "unmatched closing parenthesis")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ClassElement {
    Range { start: char, end: char },
    Word,
    NoWord,
    Space,
    NoSpace,
    Class(Class),
    Char(char),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Class {
    pub negate: bool,
    pub elements: Vec<ClassElement>,
}

impl Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        if self.negate {
            write!(f, "^")?;
        }
        for element in self.elements.iter() {
            match element {
                ClassElement::Range { start, end } => write!(f, "{start}-{end}")?,
                ClassElement::Word => write!(f, "\\w")?,
                ClassElement::NoWord => write!(f, "\\W")?,
                ClassElement::Space => write!(f, "\\s")?,
                ClassElement::NoSpace => write!(f, "\\S")?,
                ClassElement::Class(class) => write!(f, "{class}")?,
                ClassElement::Char(c @ ('-' | '[' | ']' | '^' | '\\')) => write!(f, "\\{c}")?,
                ClassElement::Char('\n') => write!(f, "\\n")?,
                ClassElement::Char(chr) => write!(f, "{chr}")?,
            }
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub enum NodeKind {
    Union {
        left: Ast,
        right: Ast,
    },
    #[default]
    Empty,
    Epsilon,
    Cons {
        left: Ast,
        right: Ast,
    },
    Class(Class),
    Group(Ast),
    Capture(Ast),
    Star(Ast),
    Plus(Ast),
    Optional(Ast),
}

impl Display for NodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Union { left, right } => write!(f, "({left}|{right})"),
            Self::Empty => write!(f, "Ø"),
            Self::Epsilon => write!(f, "ε"),
            Self::Cons { left, right } => write!(f, "({left}·{right})"),
            Self::Class(class) => write!(f, "{class}"),
            Self::Capture(regex) => write!(f, "{{{regex}}}"),
            Self::Star(regex) => write!(f, "({regex})*"),
            Self::Plus(regex) => write!(f, "({regex})+"),
            Self::Optional(regex) => write!(f, "({regex})?"),
            Self::Group(regex) => write!(f, "({regex})"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Ast {
    pub kind: Box<NodeKind>,
    pub span: Span,
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Ast {
    pub fn new(input: &str) -> Result<Self, SyntaxError> {
        let ast = Self::parse_group::<false>(&mut input.char_indices(), 0)?;
        Ok(ast)
    }

    fn cons(left: Ast, right: Ast) -> Self {
        Self {
            span: Span {
                start: left.span.start,
                end: right.span.end,
            },
            kind: Box::new(NodeKind::Cons { left, right }),
        }
    }

    fn union(left: Ast, right: Ast) -> Self {
        Self {
            span: Span {
                start: left.span.start,
                end: right.span.end,
            },
            kind: Box::new(NodeKind::Union { left, right }),
        }
    }

    fn epsilon(pos: usize) -> Self {
        Self {
            kind: Box::new(NodeKind::Epsilon),
            span: Span {
                start: pos,
                end: pos,
            },
        }
    }

    fn parse_class(
        input: &mut impl Iterator<Item = (usize, char)>,
        start: usize,
    ) -> Result<(Class, usize), SyntaxError> {
        let mut first = true;
        let mut negate = false;
        let mut elements = Vec::new();
        let mut last_pos = start;
        let end = loop {
            last_pos = match input.next() {
                Some((pos, '^')) if first => {
                    negate = true;
                    pos + '^'.len_utf8()
                }
                Some((pos, ']')) => {
                    break pos + ']'.len_utf8();
                }
                Some((pos, '[')) => {
                    let (class, end_pos) = Self::parse_class(input, pos)?;
                    elements.push(ClassElement::Class(class));
                    end_pos
                }
                Some((pos, '\\')) => match input.next() {
                    Some((mpos, c @ ('^' | '\\' | '[' | ']' | '-'))) => {
                        elements.push(ClassElement::Char(c));
                        mpos + c.len_utf8()
                    }
                    Some((mpos, c @ 'w')) => {
                        elements.push(ClassElement::Word);
                        mpos + c.len_utf8()
                    }
                    Some((mpos, c @ 'W')) => {
                        elements.push(ClassElement::NoWord);
                        mpos + c.len_utf8()
                    }
                    Some((mpos, c @ 's')) => {
                        elements.push(ClassElement::Space);
                        mpos + c.len_utf8()
                    }
                    Some((mpos, c @ 'S')) => {
                        elements.push(ClassElement::NoSpace);
                        mpos + c.len_utf8()
                    }
                    Some((mpos, c)) => {
                        return Err(SyntaxError::EscapedChar {
                            chr: c,
                            span: Span {
                                start: pos,
                                end: mpos + c.len_utf8(),
                            },
                        });
                    }
                    None => {
                        return Err(SyntaxError::EscapedEndOfRegex {
                            span: Span {
                                start: pos,
                                end: pos + '\\'.len_utf8(),
                            },
                        });
                    }
                },
                Some((pos, '-'))
                    if let Some(last) = elements.last_mut()
                        && let ClassElement::Char(start_chr) = *last =>
                {
                    match input.next() {
                        Some((mpos, end_chr)) => {
                            *last = ClassElement::Range {
                                start: start_chr,
                                end: end_chr,
                            };
                            mpos + end_chr.len_utf8()
                        }
                        None => {
                            return Err(SyntaxError::UnclosedClass {
                                span: Span {
                                    start,
                                    end: pos + '-'.len_utf8(),
                                },
                            });
                        }
                    }
                }
                Some((pos, c)) => {
                    elements.push(ClassElement::Char(c));
                    pos + c.len_utf8()
                }
                None => {
                    return Err(SyntaxError::UnclosedClass {
                        span: Span {
                            start,
                            end: last_pos,
                        },
                    });
                }
            };
            first = false;
        };

        Ok((Class { negate, elements }, end))
    }

    fn parse_group<const END_WITH_PAREN: bool>(
        input: &mut impl Iterator<Item = (usize, char)>,
        start: usize,
    ) -> Result<Self, SyntaxError> {
        let mut result = Ast {
            kind: Box::new(NodeKind::Empty),
            span: Span { start, end: start },
        };
        let mut last = Ast {
            kind: Box::new(NodeKind::Epsilon),
            span: Span { start, end: start },
        };

        let mut capture = false;

        let end = loop {
            match input.next() {
                Some((_, '?')) if last.span.end == start => capture = true,
                Some((pos, c @ ')')) => {
                    if END_WITH_PAREN {
                        break pos + c.len_utf8();
                    } else {
                        return Err(SyntaxError::UnmatchedClosingParen {
                            span: Span {
                                start: pos,
                                end: pos + c.len_utf8(),
                            },
                        });
                    }
                }
                Some((pos, c @ ('+' | '*' | '?'))) => {
                    let node = if let NodeKind::Cons { ref mut right, .. } = *last.kind {
                        right
                    } else {
                        &mut last
                    };
                    let span = Span {
                        start: node.span.start,
                        end: pos + c.len_utf8(),
                    };
                    match c {
                        '+' => {
                            *node = Ast {
                                span,
                                kind: Box::new(NodeKind::Plus(mem::take(node))),
                            };
                        }
                        '?' => {
                            *node = Ast {
                                span,
                                kind: Box::new(NodeKind::Optional(mem::take(node))),
                            }
                        }
                        '*' => {
                            *node = Ast {
                                span,
                                kind: Box::new(NodeKind::Star(mem::take(node))),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
                Some((pos, '(')) => {
                    let ast = Self::parse_group::<true>(input, pos)?;
                    last = Ast::cons(last, ast);
                }
                Some((pos, c @ '|')) => {
                    result = Ast::union(result, last);
                    last = Ast::epsilon(pos + c.len_utf8());
                }
                Some((pos, '\\')) => match input.next() {
                    Some((posm, c @ 'w')) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::Word],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, c @ 'W')) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::NoWord],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, c @ 's')) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::Space],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, c @ 'S')) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::NoSpace],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, c @ 'n')) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::Char('\n')],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, c @ ('[' | ']' | '(' | ')' | '\\' | '+' | '*' | '?' | '.'))) => {
                        last = Ast::cons(
                            last,
                            Ast {
                                kind: Box::new(NodeKind::Class(Class {
                                    negate: false,
                                    elements: vec![ClassElement::Char(c)],
                                })),
                                span: Span {
                                    start: pos,
                                    end: posm + c.len_utf8(),
                                },
                            },
                        )
                    }
                    Some((posm, chr)) => {
                        return Err(SyntaxError::EscapedChar {
                            chr,
                            span: Span {
                                start: pos,
                                end: posm + chr.len_utf8(),
                            },
                        });
                    }
                    None => {
                        return Err(SyntaxError::EscapedEndOfRegex {
                            span: Span {
                                start: pos,
                                end: pos + '\\'.len_utf8(),
                            },
                        });
                    }
                },
                Some((pos, '[')) => {
                    let (class, end_pos) = Self::parse_class(input, pos)?;
                    last = Ast::cons(
                        last,
                        Ast {
                            kind: Box::new(NodeKind::Class(class)),
                            span: Span {
                                start: pos,
                                end: end_pos,
                            },
                        },
                    )
                }
                Some((pos, chr)) => {
                    last = Ast::cons(
                        last,
                        Ast {
                            kind: Box::new(NodeKind::Class(Class {
                                negate: false,
                                elements: vec![ClassElement::Char(chr)],
                            })),
                            span: Span {
                                start: pos,
                                end: pos + chr.len_utf8(),
                            },
                        },
                    );
                }
                None => {
                    if END_WITH_PAREN {
                        return Err(SyntaxError::UnclosedGroup {
                            span: Span {
                                start,
                                end: last.span.end,
                            },
                        });
                    } else {
                        break last.span.end;
                    }
                }
            }
        };

        result = Ast::union(result, last);
        result = Ast {
            span: Span {
                start: start,
                end: end,
            },
            kind: Box::new(if capture {
                NodeKind::Capture(result)
            } else {
                NodeKind::Group(result)
            }),
        };
        Ok(result)
    }
}

impl FromStr for Ast {
    type Err = SyntaxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::new(s)
    }
}

#[cfg(test)]
mod tests {
    #![allow(unused)]

    use crate::{span::Span, syntax::NodeKind};

    use super::{Ast, Class};

    enum MockAst {
        Union {
            left: Box<MockAst>,
            right: Box<MockAst>,
        },
        Empty,
        Epsilon,
        Cons {
            left: Box<MockAst>,
            right: Box<MockAst>,
        },
        Class(Class),
        Group(Box<MockAst>),
        Capture(Box<MockAst>),
        Star(Box<MockAst>),
        Plus(Box<MockAst>),
        Optional(Box<MockAst>),
    }

    impl MockAst {
        fn union<const N: usize>(nodes: [MockAst; N]) -> Self {
            let mut result = Self::Empty;
            if nodes.is_empty() {
                return Self::Union {
                    left: Box::new(result),
                    right: Box::new(Self::Epsilon),
                };
            }
            for node in nodes {
                result = Self::Union {
                    left: Box::new(result),
                    right: Box::new(node),
                }
            }
            result
        }

        fn cons<const N: usize>(nodes: [MockAst; N]) -> Self {
            let mut result = Self::Epsilon;
            for node in nodes {
                result = Self::Cons {
                    left: Box::new(result),
                    right: Box::new(node),
                }
            }
            result
        }
    }

    macro_rules! mock_ast {
        ($({$($node:tt)*})|*) => {
            MockAst::union([$(mock_ast!(@cons $($node)*)),*])
        };
        (@cons $({$($node:tt)*}),*) => {
            MockAst::cons([$($mock_ast!(@elem $($node)*)),*])
        };
        (@elem (?$($node:tt)*)) => {
            MockAst::Capture(Box::new(mock_ast!($($node)*)))
        };
        (@elem ($($node:tt)*)) => {
            MockAst::Group(Box::new(mock_ast!($($node)*)))
        };
        (@elem $($node:tt)*?) => {

        };
        (@single ($node:tt)) => {};
        (@class_element [^$($class:tt)*]) => {
            ClassElement::Class(Class {
                negate: true,
                elements: mock_ast!(@class $($class)*),
            })
        };
        (@class_element [$($class:tt)*]) => {
            ClassElement::Class(Class {
                negate: false,
                elements: mock_ast!(@class $($class)*),
            })
        };
        (@class_element r"\w") => {
            ClassElement::Word
        };
        (@class_element r"\W") => {
            ClassElement::NoWord
        };
        (@class_element r"\s") => {
            ClassElement::Space
        };
        (@class_element r"\S") => {
            ClassElement::NoSpace
        };
        (@class_element $start:tt - $end:tt) => {
            ClassElement::Range {
                start: $start,
                end: $end,
            }
        };
        (@class_element $chr:tt) => {
            ClassElement::Char($chr)
        };
        (@class $(($($elem:tt)*))|*) => {
            vec![$(mock_ast!(@class_element $($elem)*))*]
        };
        ([^$($class:tt)*]) => {
            MockAst::Class(Class {
                negate: true,
                elements: mock_ast!(@class $($class)*),
            })
        };
        ([$($class:tt)*]) => {
            MockAst::Class(Class {
                negate: false,
                elements: mock_ast!(@class $($class)*),
            })
        };
    }

    impl PartialEq<Ast> for MockAst {
        fn eq(&self, other: &Ast) -> bool {
            match (self, &*other.kind) {
                (
                    MockAst::Cons {
                        left: mock_left,
                        right: mock_right,
                    },
                    NodeKind::Cons { left, right },
                )
                | (
                    MockAst::Union {
                        left: mock_left,
                        right: mock_right,
                    },
                    NodeKind::Union { left, right },
                ) => **mock_left == *left && **mock_right == *right,
                (MockAst::Empty, NodeKind::Empty) | (MockAst::Epsilon, NodeKind::Epsilon) => {
                    true
                }
                (MockAst::Class(mock_class), NodeKind::Class(class)) => mock_class == class,
                (MockAst::Group(mock_ast), NodeKind::Group(ast))
                | (MockAst::Capture(mock_ast), NodeKind::Capture(ast))
                | (MockAst::Star(mock_ast), NodeKind::Star(ast))
                | (MockAst::Plus(mock_ast), NodeKind::Plus(ast))
                | (MockAst::Optional(mock_ast), NodeKind::Optional(ast)) => **mock_ast == *ast,
                _ => false,
            }
        }
    }

    #[test]
    fn empty_regex() {
        let regex = "".parse::<Ast>().unwrap();
        assert_eq!(regex.span, Span { start: 0, end: 0 });
        let NodeKind::Group(regex) = *regex.kind else {
            panic!()
        };
        let NodeKind::Union { left, right } = *regex.kind else {
            panic!("expected `NodeKind::Union`, found {regex:?}");
        };
        assert_eq!(*left.kind, NodeKind::Empty);
        assert_eq!(*right.kind, NodeKind::Epsilon);
    }

    #[test]
    fn regex_union() {
        let regex = "ab|cd".parse::<Ast>().unwrap();
        assert_eq!(regex.span, Span { start: 0, end: 5 });
        let NodeKind::Union { left, right } = *regex.kind else {
            panic!("expected `NodeKind::Union`, found {regex:?}");
        };
        assert_eq!(right.span, Span { start: 3, end: 5 });
        let NodeKind::Union { left, right } = *left.kind else {
            panic!("expected `NodeKind::Union`, found {right:?}");
        };
        assert_eq!(*left.kind, NodeKind::Empty);
        assert_eq!(right.span, Span { start: 0, end: 2 });
    }

    #[test]
    fn regex_group() {
        let input = "(ab|c(d))|(e)f";
        let regex = input.parse::<Ast>().unwrap();
        println!("{regex}");
        assert_eq!(
            regex.span,
            Span {
                start: 0,
                end: input.len()
            }
        );
        let NodeKind::Union { left, right } = *regex.kind else {
            panic!("expected `NodeKind::Union`, found {regex:?}");
        };
        assert_eq!(right.span, Span { start: 10, end: 14 });
        assert_eq!(left.span, Span { start: 0, end: 9 });
        let NodeKind::Union { left, right } = *left.kind else {
            panic!("expected `NodeKind::Union`, found {left:?}");
        };
        assert_eq!(left.span, Span { start: 0, end: 0 });
        assert_eq!(*left.kind, NodeKind::Empty);
        assert_eq!(right.span, Span { start: 0, end: 9 });
    }
}
